;;; kiro-test.el --- Test harness for Kiro modes

(require 'ert)
(require 'kiro-meta-mode)
(require 'kiro-task-mode)
(require 'kiro-tenfold)

;;; RDF Tests
(ert-deftest kiro-test-rdf-escape ()
  (should (string= (kiro-rdf-escape "hello") "hello"))
  (should (string= (kiro-rdf-escape "say \"hi\"") "say \\\"hi\\\"")))

(ert-deftest kiro-test-rdf-triple ()
  (should (string-match-p "<s> <p> \"o\"" (kiro-rdf-triple "s" "p" "o"))))

;;; Emoji Tests
(ert-deftest kiro-test-emoji-mapping ()
  (should (string= (kiro-emoji 2) "⚡"))
  (should (string= (kiro-emoji 71) "👑"))
  (should (string= (kiro-emoji 999) "🔢")))

;;; URL Tests
(ert-deftest kiro-test-url-generation ()
  (should (string= (kiro-make-url "zone" "71")
                   "https://kiro.zone/zone/71"))
  (should (string-match-p "\\?key=val"
                          (kiro-make-url "task" "1" '(("key" . "val"))))))

;;; Buffer Tests
(ert-deftest kiro-test-buffer-names ()
  (should (string= (kiro-get-buffer-name 2) "kiro-binary"))
  (should (string= (kiro-get-buffer-name 71) "kiro-omega"))
  (should (string= (kiro-get-buffer-name "jocko") "kiro-jocko")))

;;; 10-fold Tests
(ert-deftest kiro-test-tenfold-classify ()
  (with-temp-buffer
    (insert "binary true false 0 1")
    (should (= (kiro-tenfold-classify (current-buffer)) 1)))
  (with-temp-buffer
    (insert "rdf triple time")
    (should (= (kiro-tenfold-classify (current-buffer)) 3))))

(ert-deftest kiro-test-tenfold-emoji ()
  (should (string= (cdr (assoc 0 kiro-tenfold-emoji)) "∅"))
  (should (string= (cdr (assoc 9 kiro-tenfold-emoji)) "🌀")))

(provide 'kiro-test)
;;; kiro-test.el ends here
