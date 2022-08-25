(require hyrule * :readers *)
(import logging)
(import itertools [chain])
(import re)
(import jedhy.api [API])
(import pygls.lsp.methods [COMPLETION
                           HOVER
                           TEXT_DOCUMENT_DID_CHANGE
                           TEXT_DOCUMENT_DID_CLOSE
                           TEXT_DOCUMENT_DID_OPEN])
(import pygls.lsp.types [CompletionItem
                         CompletionList
                         CompletionOptions
                         CompletionParams
                         Hover
                         MarkupContent
                         MarkupKind])
(import pygls.server [LanguageServer])

(import .jedhy [Jedhy])

(setv logger (logging.getLogger "hyls.server"))

(defn cursor-line [ls uri ln]
  (let [doc (ls.workspace.get_document uri)
        content doc.source
        lines (content.split "\n")]
    (get lines ln)))

(defn cursor-word [ls uri ln cn]
  (let [line (cursor-line ls uri ln)]
    (for [m (re.finditer r"[\.\?\-\w]+" line)]
      (when (and (<= (m.start) cn) (<= cn (m.end)))
        (return (cut line (m.start) cn))))))

(defn cursor-word-all [ls uri ln cn]
  (let [line (cursor-line ls uri ln)]
    (for [m (re.finditer r"[\.\?\-\w]+" line)]
      (when (and (<= (m.start) cn) (<= cn (m.end)))
        (return (cut line (m.start) (m.end)))))))

(defclass Server []
  (defn __init__ [self]
    (setv self.server (LanguageServer))
    (setv self.jedhy (Jedhy (API :logger logger) :logger logger))
    (setv self.imports [])

    (defn
      [(self.server.feature
         COMPLETION
         (CompletionOptions :trigger_characters ["."]))]
      completions [params]
      (let [word (cursor-word self.server
                              params.text_document.uri
                              params.position.line
                              params.position.character)]
        (setv complist (CompletionList
                         :is_incomplete False
                         :items []))
        (when (not (isinstance word (type None)))
          (for [candidate (self.jedhy.complete word)]
            (complist.add_item (CompletionItem :label candidate))))
        complist))

    (defn [(self.server.feature HOVER)] hover [params]
      (let [word (cursor-word-all self.server
                                  params.text_document.uri
                                  params.position.line
                                  params.position.character)]
        (when (not (isinstance word (type None)))
          (let [docs (self.jedhy.docs word)]
            (when (!= docs "")
              (Hover
                :contents (MarkupContent
                            :kind MarkupKind.PlainText
                            :value docs)))))))

    (defn [(self.server.feature TEXT_DOCUMENT_DID_OPEN)] did-open [params]
      (setv self.imports [])
      (self.find-and-eval-imports self.server params.text_document.uri)
      (self.jedhy.refresh-ns self.imports))

    (defn [(self.server.feature TEXT_DOCUMENT_DID_CLOSE)] did-close [params]
      (setv self.imports []))

    (defn [(self.server.feature TEXT_DOCUMENT_DID_CHANGE)] did-change [params]
      None))

  (defn find-and-eval-imports [self ls uri]
    (let [doc (ls.workspace.get_document uri)
          iters (chain
                  (re.finditer r"\(\s*(import|require)\s+([\w\.]+|[\w\.\s\*\?:\[\]]+)\)"
                               doc.source)
                  (re.finditer r"\(\s*(defn)\s+([\w\.]+|[\w\.\s\*\?:\[\]]+)\)"
                               doc.source))]
      (for [m iters]
        (logger.info (+ "try to evaluate: " (m.group)))
        ;; FIXME: HyEvalError("module 'hy' has no attribute 'hyx_XampersandXreader'")
        ;; when eval (require hyrule * :readers *).
        ;; see also: https://github.com/hylang/hy/issues/2291
        (try
          (-> (m.group)
              (hy.read)
              (hy.eval))
          (except [e BaseException]
                  (logger.info (+ "cannot evaluate: " (repr e))))
          (else
            (self.imports.append (m.group)))))))
  (defn start [self]
    (self.server.start_io)))
