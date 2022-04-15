;;; test-root.el --- Test suite for root.el          -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Jay Morgan

;; Author: Jay Morgan <jay@morganwastaken.com>

(require 'root-mode)

(ert-deftest root-test-push-new ()
  "Tests the functionality of push-new in root.el"
  (should (equal (push-new "a" (list "b" "c")) (list "a" "b" "c"))))

(ert-deftest root-test-pluck-item ()
  "Tests the functionality of plucking an item from a list as defined in root.el"
  (should (equal (pluck-item 'a '((a . b))) 'b))
  (should (equal (pluck-item 'a '((b . c))) nil))
  (should (equal (pluck-item "a" '(("a" . 3))) 3))
  (should (equal (pluck-item :a '((:a . b))) 'b))
  (should (equal (pluck-item 'a '((a . b) (a . c))) 'b)))

(ert-deftest root-test-make-earmuff ()
  "Tests that a string can be given earmuffs, i.e. name -> *name*"
  (should (equal (make-earmuff "name") "*name*"))
  (should (equal (make-earmuff "*name*") "*name*"))
  (should (equal (make-earmuff "*name") "**name*"))
  (should (equal (make-earmuff "") ""))
  (should (equal (make-earmuff "a") "*a*")))

(ert-deftest root-test-make-no-earmuff ()
  "Tests that earmuffs can be removed from strings"
  (should (equal (make-no-earmuff "*name*") "name"))
  (should (equal (make-no-earmuff "name") "name"))
  (should (equal (make-no-earmuff "") ""))
  (should (equal (make-no-earmuff "a") "a")))
