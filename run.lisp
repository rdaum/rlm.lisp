(asdf:load-system "rlm")

(setf rlm/client:*model* "openrouter/hunter-alpha")
(setf rlm:*verbose* t)

(let* ((context "The following is a list of employees and their departments:
- Alice: Engineering, joined 2019, salary $120,000
- Bob: Marketing, joined 2020, salary $95,000
- Charlie: Engineering, joined 2018, salary $135,000
- Diana: Sales, joined 2021, salary $88,000
- Eve: Engineering, joined 2022, salary $110,000
- Frank: Marketing, joined 2019, salary $102,000
- Grace: Sales, joined 2020, salary $91,000
- Hank: Engineering, joined 2017, salary $145,000")
       (question "What is the average salary in the Engineering department?")
       (result (rlm:query context question :max-iterations 10)))
  (format t "~%~%========================================~%")
  (format t "Answer: ~A~%" (rlm:query-result-answer result))
  (format t "Iterations: ~A~%" (rlm:query-result-iterations result))
  (format t "Token usage: ~S~%" (rlm:query-result-token-usage result)))
