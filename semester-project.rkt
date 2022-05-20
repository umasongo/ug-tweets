#lang racket
;;; data-science to process the text, and plot to visualize the results
(require data-science)
(require csv-reading)
(require plot)
(require math)

;;Read csv tweeter dataset
(define file-path "tweeter-dataset.csv")

(define (read-csv file-path #:->number? [->number? #f] #:header? [header? #t])
  (let ((csv-reader (make-csv-reader-maker
                     '((separator-chars #\|)
                       (comment-chars #\#))))) 
    (with-input-from-file file-path
      (lambda ()
	(let* ((tmp (csv->list (csv-reader (current-input-port)))))
	  (if ->number?
	      ;; try to convert everything to numbers rather than
	      ;; strings. This should be made smarter, converting only
	      ;; those columns which are actually numbers
	      (if header?
		  (cons (car tmp) (map (lambda (x) (map string->number x)) (cdr tmp)))
		  (map (lambda (x) (map string->number x)) tmp))
	      ;; Else, leave everything as strings
	      tmp))))))



;;; -------------------------------------------------------------------------
;;; No. 1, 2, 3, 4 - denotes the new abstraction i developed or extended 
;;; on top of exiting
;;; -------------------------------------------------------------------------

;;; 1.
;;; Return the raw tweets based on the get request below
;;; this is also where we build our query using lists of pair (key/value)
(define raw_tweets (read-csv file-path))
;;raw_tweets

;;; 2.
;;; Remove just the tweet text, source, and timestamp from each tweet hash.
;;;Finally, remove retweets.
(define data
  (let ([tmp (map (λ (x) (list (list-ref x 0))) raw_tweets)])
    (filter (λ (x) (not (string-prefix? (first x) "RT"))) tmp)))

;;; 3.
;;; Remove empty strings and flatten tweets into a single list of words
(define flat_data (flatten data))

;;; 4.
;;; Combine all tweets into a single string
(define all_tweet-text (apply string-append flat_data))


;;; ---------------------------------------------------------------
;;; Using abstractions from the data-science-master package
;;; ---------------------------------------------------------------

;;; We capture the text from the above symbol (tweet-text), removing capitalization, 
;;; punctuation, and then extra spaces
(define tweet-text (string-normalize-spaces
		   (remove-punctuation
		    (string-downcase all_tweet-text) #:websafe? #t)))

;;; To begin our sentiment analysis, we extract each unique word
;;; and the number of times it occurred in the document
(define words (document->tokens tweet-text #:sort? #t))

;;; Using the nrc lexicon, we can label each (non stop-word) with an
;;; emotional label.
(define sentiment (list->sentiment words #:lexicon 'nrc))

;;; sentiment, created above, consists of a list of triplets of the pattern
;;; (token sentiment freq) for each token in the document. Many words will have 
;;; the same sentiment label, so we aggregrate (by summing) across such tokens.
(aggregate sum ($ sentiment 'sentiment) ($ sentiment 'freq))

;;; Now, we can visualize this result as a barplot (discrete-histogram)
(let ([counts (aggregate sum ($ sentiment 'sentiment) ($ sentiment 'freq))])
  (parameterize ((plot-width 800))
    (plot (list
	   (tick-grid)
	   (discrete-histogram
	    (sort counts (λ (x y) (> (second x) (second y))))
	    #:color "MediumSlateBlue"
	    #:line-color "MediumSlateBlue"))
	  #:x-label "Emotions"
	  #:y-label "Frequency")))

(define sentiment_bing (list->sentiment words #:lexicon 'bing))
(parameterize ([plot-height 200])
  (plot (discrete-histogram
	 (aggregate sum ($ sentiment_bing 'sentiment) ($ sentiment_bing 'freq))
	 #:y-min 0
	 #:y-max 100000
	 #:invert? #t
	 #:color "MediumOrchid"
	 #:line-color "MediumOrchid")
	#:x-label "Frequency"
	#:y-label "Sentiments"))