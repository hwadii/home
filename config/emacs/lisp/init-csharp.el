;;; init-csharp.el --- -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(defun reapply-csharp-ts-mode-font-lock-settings ()
  "Fixes csharp-ts-mode font lock with latest version of parser"
  (interactive)
  (setq csharp-ts-mode--keywords
        '("this" "add" "alias" "as" "base" "break" "case" "catch" "checked" "class" "continue"
          "default" "delegate" "do" "else" "enum" "event" "explicit" "extern" "finally"
          "for" "foreach" "global" "goto" "if" "implicit" "interface" "is" "lock"
          "namespace" "notnull" "operator" "params" "return" "remove" "sizeof"
          "stackalloc" "static" "struct" "switch" "throw" "try" "typeof" "unchecked"
          "using" "while" "new" "await" "in" "yield" "get" "set" "when" "out" "ref" "from"
          "where" "select" "record" "init" "with" "let"))

  (let ((ops '("--" "-" "-=" "&" "&=" "&&" "+" "++" "+=" "<" "<=" "<<" "<<=" "="
               "==" "!" "!=" "=>" ">" ">=" ">>" ">>=" ">>>" ">>>=" "|" "|=" "||"
               "?" "??" "??=" "^" "^=" "~" "*" "*=" "/" "/=" "%" "%=" ":")))
    (setq csharp-ts-mode--font-lock-settings
          (treesit-font-lock-rules
           :language 'c-sharp
           :feature 'bracket
           '((["(" ")" "[" "]" "{" "}" (interpolation_brace)]) @font-lock-bracket-face)

           :language 'c-sharp
           :feature 'delimiter
           `((["," ":" ";"]) @font-lock-delimiter-face
             ([,@ops]) @font-lock-operator-face
             )

           :language 'c-sharp
           :override t
           :feature 'comment
           '((comment) @font-lock-comment-face)

           :language 'c-sharp
           :override t
           :feature 'keyword
           `([,@csharp-ts-mode--keywords] @font-lock-keyword-face
             (modifier) @font-lock-keyword-face
             (implicit_type) @font-lock-keyword-face)

           :language 'c-sharp
           :override t
           :feature 'property
           `((attribute name: (identifier) @font-lock-property-use-face))

           :language 'c-sharp
           :override t
           :feature 'literal
           `((integer_literal) @font-lock-number-face
             (real_literal) @font-lock-number-face
             (null_literal) @font-lock-constant-face
             (boolean_literal) @font-lock-constant-face)

           :language 'c-sharp
           :override t
           :feature 'string
           `([(character_literal)
              (string_literal)
              (raw_string_literal)
              (verbatim_string_literal)
              ;; (interpolated_string_expression)
              (string_content)
              (interpolation_start)
              (interpolation_quote)] @font-lock-string-face)

           :language 'c-sharp
           :override t
           :feature 'escape-sequence
           '((escape_sequence) @font-lock-escape-face)

           :language 'c-sharp
           :feature 'type
           :override t
           '((generic_name (identifier) @font-lock-type-face)
             (type_parameter (identifier) @font-lock-type-face)
             (parameter type: (identifier) @font-lock-type-face)
             (type_argument_list (identifier) @font-lock-type-face)
             (as_expression right: (identifier) @font-lock-type-face)
             (is_expression right: (identifier) @font-lock-type-face)
             (_ type: (identifier) @font-lock-type-face)
             (predefined_type) @font-lock-builtin-face
             )

           :language 'c-sharp
           :feature 'definition
           :override t
           '((interface_declaration name: (identifier) @font-lock-type-face)
             (class_declaration name: (identifier) @font-lock-type-face)
             (enum_declaration name: (identifier) @font-lock-type-face)
             (struct_declaration (identifier) @font-lock-type-face)
             (record_declaration (identifier) @font-lock-type-face)
             (namespace_declaration name: (identifier) @font-lock-type-face)
             (constructor_declaration name: (identifier) @font-lock-constructor-face)
             (destructor_declaration name: (identifier) @font-lock-constructor-face)
             (base_list (identifier) @font-lock-type-face)
             (enum_member_declaration (identifier) @font-lock-variable-name-face)
             (parameter name: (identifier) @font-lock-variable-name-face)
             (implicit_parameter) @font-lock-variable-name-face
             )

           :language 'c-sharp
           :feature 'function
           '((method_declaration name: (identifier) @font-lock-function-name-face)
             (local_function_statement name: (identifier) @font-lock-function-name-face)
             (invocation_expression
              function: (member_access_expression
                         name: (identifier) @font-lock-function-call-face))
             (invocation_expression
              function: (identifier) @font-lock-function-call-face)
             (invocation_expression
              function: (member_access_expression
                         name: (generic_name (identifier) @font-lock-function-call-face)))
             (invocation_expression
              function: (generic_name (identifier) @font-lock-function-call-face)))

           :language 'c-sharp
           :feature 'expression
           '((identifier) @font-lock-variable-use-face)

           :language 'c-sharp
           :feature 'directives
           :override t
           '((preproc_if
              "#if" @font-lock-preprocessor-face)
             (preproc_if
              "#endif" @font-lock-preprocessor-face)
             (preproc_elif
              "#elif" @font-lock-preprocessor-face)
             (preproc_else
              "#else" @font-lock-preprocessor-face)
             ;; (preproc_endif) @font-lock-preprocessor-face
             (preproc_define
              "#define" @font-lock-preprocessor-face
              (preproc_arg) @font-lock-constant-face)
             (preproc_undef
              "#undef" @font-lock-preprocessor-face
              (preproc_arg) @font-lock-constant-face)

             (preproc_nullable) @font-lock-preprocessor-face
             (preproc_pragma) @font-lock-preprocessor-face
             (preproc_region
              "#region" @font-lock-preprocessor-face
              (preproc_arg) @font-lock-comment-face)
             (preproc_endregion) @font-lock-preprocessor-face)))))

(provide 'init-csharp)
;;; init-csharp.el ends here
