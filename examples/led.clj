
(pin-mode 13 :output)

(forever
 (digital-write 13 :high)
 (sleep 500)
 (digital-write 13 :low)
 (sleep 500))
