;;; test-root.el --- Test suite for root.el          -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Jay Morgan

;; Author: Jay Morgan <jay@morganwastaken.com>

(require 'root-mode)

;; helper functions
(defmacro with-root-repl (&rest body)
  `(progn
     (run-root)
     ,@body
     (let ((kill-buffer-query-functions nil))
       (kill-buffer root-buffer-name))))

;; example test files
(defconst test-file-1
  "#include <cstdio>
int test() {
    printf(\"This is a test\");
    return 0;
}")

(defconst test-file-2
  "#include <string>
#include <cstdio>

template<typename T>
T add(T a, T b) {
    return a + b;
}

/* 
 * this is a function
 * @param : no param
 * @return : int
 */
int test() {
    // run the test
    return add(1, 2);
}")

(defconst test-file-3
  "#include <cstdio>

struct MyFairClass {
    MyFairClass(const char* name) : name {name} {};
    void print_my_name() {
        printf(\"My Fair %s\\n\", this->name);
    }
    private:
    const char* name;
};

void test() {
    MyFairClass m{\"Lady\"};
    m.print_my_name();
}")

;;; begin tests

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


(defmacro do-test-file (test-file expected)
  `(with-root-repl
    (root-eval-string ,test-file)
    (root-eval-string "test()")
    (sleep-for 0.5)
    (let ((result (root--get-last-output)))
      (should (equal result ,expected)))))

(ert-deftest root-test-root-file-1 ()
  "Tests that test-file-1 can be sent to the REPL and the correct result is returned"
  (do-test-file test-file-1 "This is a test(int) 0\n"))

(ert-deftest root-test-root-file-2 ()
  "Tests that test-file-2 can be send to the ROOT REPL and the correct result is returned."
  (do-test-file test-file-2 "(int) 3\n"))

(ert-deftest root-test-root-file-3 ()
  "Tests that test-file-3 can be sent to the REPL and the correct result is returned."
  (do-test-file test-file-3 "My Fair Lady\n"))
