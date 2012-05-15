
(native-declare #<
                #include "opencv/cv.h"
                #include "opencv/highgui.h"
                >#)

(defn wait-key [i] "__result = var((char)cvWaitKey(NUMBER(i)->intValue()));")

(defn video-capture [i]
  #<
  cv::VideoCapture *cap = new cv::VideoCapture(NUMBER(i)->intValue());
  if (cap->isOpened())
   __result = var(new Pointer(cap));
  >#)

(defn named-window [n] "cv::namedWindow(toCppString(n),1);")

(defn query-frame [c]
  #<
  cv::VideoCapture *cap = static_cast<cv::VideoCapture*>(POINTER(c)->ptr);
  cap->grab();
  cv::Mat *image = new cv::Mat;
  cap->retrieve(*image, 0);
  __result = var(new Pointer(image));
  >#)

(defn show-image [f img]
  #<
  cv::Mat *i = static_cast<cv::Mat*>(POINTER(img)->ptr);
  imshow(toCppString(f), *i);
  >#)

(def cam (video-capture 0))

(named-window "cam")

(while (not= (wait-key 1) \q)
  (let [f (query-frame cam)]
    (show-image "cam" f)))
