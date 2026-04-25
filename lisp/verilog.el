;;; verilog.el --- Verilog settings & file header  -*- lexical-binding: t -*-

;; Indent
(setq verilog-indent-level               4
      verilog-indent-level-module        0
      verilog-indent-level-declaration   0
      verilog-case-indent                4
      verilog-cexp-indent                4
      verilog-indent-lists               nil
      verilog-indent-level-behavioral    4
      verilog-indent-level-directive     4
      verilog-auto-indent-on-newline     nil
      verilog-tab-always-indent          t
      verilog-indent-begin-after-if      nil
      verilog-auto-newline               nil
      verilog-auto-endcomments           t
      verilog-auto-reset-widths          t
      verilog-assignment-delay           "#1 "
      verilog-auto-lineup                nil)

;; File header insert function
(defun my/insert-verilog-file-header ()
  "Insert a Verilog file header with module template."
  (interactive)
  (let* ((cur-file       (read-from-minibuffer
                          "File name? "
                          (file-name-nondirectory (buffer-file-name))))
         (cur-date       (format-time-string "%Y-%m-%d"))
         (cur-author     "Tao Yuxin")
         (cur-email      "ytaoai@connect.ust.hk")
         (cur-description (read-from-minibuffer "Description? ")))
    (insert (format "//****************************************************************\\\n"))
    (insert (format "// Copyright (C) %s %s, All right reserved.\n"
                    (format-time-string "%Y") cur-author))
    (insert (format "// File        : %s \n" cur-file))
    (insert (format "// Author      : %s \n" cur-author))
    (insert (format "// E-mail      : %s \n" cur-email))
    (insert (format "// date        : %s \n" cur-date))
    (insert (format "// Description : %s \n" cur-description))
    (insert (format "//****************************************************************/\n"))
    (insert "\n")
    (insert "// synopsys translate_off\n")
    (insert "`timescale 1ns/1ps\n")
    (insert "// synopsys translate_on\n")
    (insert "\n")
    (insert (format "module %s\n" (file-name-base cur-file)))
    (insert "#(\n")
    (insert "    parameter TDLY = 1\n")
    (insert ")\n")
    (insert "(\n")
    (insert "    input wire clk,\n")
    (insert "    input wire rst_n,\n")
    (insert "    input wire i_dat,\n")
    (insert "    output wire o_dat\n")
    (insert ");\n")
    (insert "\n\n\n\n")
    (insert "endmodule\n")))

(provide 'verilog)
;;; verilog.el ends here
