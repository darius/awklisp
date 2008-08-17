; Simple Eliza program -- see Norvig, _Paradigms of AI Programming_.
; Uses: startup lists

(define rule-pattern car)
(define rule-answers cdr)

(define rules
  '(((hello)
     (How do you do -- please state your problem))
    ((I want)
     (What would it mean if you got -R-)
     (Why do you want -R-)
     (Suppose you got -R- soon))
    ((if)
     (Do you really think its likely that -R-)
     (Do you wish that -R-)
     (What do you think about -R-)
     (Really-- if -R-))
    ((I was)
     (Were you really?)
     (Perhaps I already knew you were -R-)
     (Why do you tell me you were -R- now?))
    ((I am)
     (In what way are you -R-)
     (Do you want to be -R-))
    ((because)
     (Is that the real reason?)
     (What other reasons might there be?)
     (Does that reason seem to explain anything else?))
    ((I feel)
     (Do you often feel -R-))
    ((I felt)
     (What other feelings do you have?))
    ((yes)
     (You seem quite positive)
     (You are sure)
     (I understand))
    ((no)
     (Why not?)
     (You are being a bit negative)
     (Are you saying no just to be negative?))
    ((someone)
     (Can you be more specific?))
    ((everyone)
     (Surely not everyone)
     (Can you think of anyone in particular?)
     (Who for example?)
     (You are thinking of a special person))
    ((perhaps)
     (You do not seem quite certain))
    ((are)
     (Did you think they might not be -R-)
     (Possibly they are -R-))
    (()
     (Very interesting)
     (I am not sure I understand you fully)
     (What does that suggest to you?)
     (Please continue)
     (Go on)
     (Do you feel strongly about discussing such things?))))

(define eliza
  (lambda ()
    (say '(Hello-- please state your problem))
    (let ((sentence '*))
      (while (begin 
               (write '>) 
               (not (eq? the-eof-object (set! sentence (read)))))
        (say (eliza-answer sentence))))))

(define eliza-answer
  (lambda (sentence)
    (some
      (lambda (rule)
        (let ((env (match (rule-pattern rule) sentence)))
          (and env
               (flatten
                 (sublis (switch-roles env)
                         (random-element (rule-answers rule)))))))
      rules)))

(define switch-roles 
  (lambda (words) 
    (sublis '(('I . 'you) ('you . 'I) ('me . 'you) ('am . 'are))
            words)))

; Matching

; If  sublist  is a sublist of  lst , then return an a-list with -R- bound to 
; the part of  lst  after  sublist .  Else return nil.
(define match
  (lambda (sublist lst)
    (cond
      ((matches-head? sublist lst)
       (list (cons '-R- (after-head sublist lst))))
      ((null? lst) nil)
      (else (match sublist (cdr lst))))))

(define matches-head?
  (lambda (alleged-head lst)
    (cond
      ((null? lst) (null? alleged-head))
      ((null? alleged-head) t)
      (else
        (and (eq? (car alleged-head) (car lst))
             (matches-head? (cdr alleged-head) (cdr lst)))))))          

(define after-head
  (lambda (head lst)
    (if (null? head)
        lst
        (after-head (cdr head) (cdr lst)))))

; Help functions

(define some
  (lambda (test? lst)
    (let ((result nil))
      (while (and lst (not (set! result (test? (car lst)))))
        (set! lst (cdr lst)))
      result)))

(define flatten
  (lambda (lst)
    (cond
      ((null? lst) '())
      ((null? (car lst)) (flatten (cdr lst)))
      ((atom? (car lst))
       (cons (car lst) (flatten (cdr lst))))
      (else (append (flatten (car lst))
                    (flatten (cdr lst)))))))

(define say
  (lambda (sentence)
    (for-each write sentence)
    (newline)))

(define random-element
  (lambda (lst)
    (list-ref lst (random (length lst)))))
