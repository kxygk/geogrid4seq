(ns geogrid4seq
  "Grayscale images as geographic grids and vice versa"
  (:use
   geogrid
   geoprim))

(defrecord
    seqgrid
    [^long
     width-pix
     ^long
     height-pix
     ^double
     eas-res
     ^double
     sou-res
     norwes-point
     data-array]
  grid
  (dimension-pix
    [_]
    [width-pix
     height-pix])
  (eassou-res
    [_]
    [eas-res
     sou-res])
  (corner
    [_]
    norwes-point)
  (data
    [_]
    data-array)
  (subregion
    [given-grid
     region]
  (let [{:keys [crop-region-pixel-offsets
                overruns]} (adjusted-crop-region-to-grid
                             region
                             given-grid)
        {:keys [^long
                start-x
                ^long
                ended-x
                ^long
                start-y
                ^long
                ended-y]}  crop-region-pixel-offsets
        crop-width         (inc
                             (-
                               ended-x
                               start-x))
        crop-height        (inc
                             (-
                               ended-y
                               start-y))
        copy-if-in-region  (fn 
                             [^long
                              row
                              ^long
                              col]
                             (if (and
                                   (>=
                                     row
                                     0)
                                   (<
                                     row
                                     height-pix)
                                   (>=
                                     col
                                     0)
                                   (<
                                     col
                                     width-pix))
                               ;; copy pixel over
                               (aget
                                 data-array
                                 (+
                                   col
                                   (*
                                     row
                                     width-pix)))
                               ;; `0.0` otherwise 
                               0.0))
        cropped-data       (mapv
                             (fn
                               [row]
                               (mapv
                                 (fn
                                   [col]
                                   (copy-if-in-region
                                     row
                                     col))
                                 (range
                                   start-x
                                   (inc
                                     ended-x))))
                             (range
                               start-y
                               (inc
                                 ended-y)))
        [^double
         grid-corner-eas
         ^double
         grid-corner-sou]  (as-eassou
                             norwes-point)
        cropped-corner     (point-eassou
                             (+
                               grid-corner-eas ;;double
                               (*
                                 start-x
                                 eas-res))
                             (+
                               grid-corner-sou
                               (*
                                 start-y
                                 sou-res)))]
    (->seqgrid
      crop-width
      crop-height
      eas-res
      sou-res
      cropped-corner
      (->>
        cropped-data
        flatten
        (into-array
          Double/TYPE))))))

(defn
  build-grid
  [[width-pix
    height-pix
    eas-res
    sou-res
    norwes-point]
   data-seq]
  (let[data-array (into-array
                    Double/TYPE ;; Hopefully this always works...
                    data-seq)]
    (->seqgrid
      width-pix
      height-pix
      eas-res
      sou-res
      norwes-point
      data-array))
#_([old-geogrid
    data-seq]
   ;; make a new geogrid based on a previous one
   ;; just use new data
   (let [[width-pix
          height-pix] (-> old-geogrid
                          dimension-pix)
         [eas-res
          sou-res]    (-> old-geogrid
                          eassou-res)
         norwes-point (-> old-geogrid
                          corner)]
     (build-grid width-pix
                 height-pix
                 eas-res
                 sou-res
                 norwes-point
                 data-seq))))
#_
(:data-array
(subregion
  (geogrid4seq/build-grid
    10
    10
    1.0
    1.0
    (geoprim/point-eassou
      5
      5)
    (range
      0
      100))
  (region
    (geoprim/point-eassou
      2.5
      2.5)
    (geoprim/point-eassou
      7.5
      7.5))))
      
  
(defn
  convert-to
  [given-grid]
  (let [[width
         height]      (dimension-pix
                        given-grid)
        [eas-res
         sou-res]     (eassou-res
                        given-grid)
        norwes-corner (corner
                        given-grid)
        data-seq      (data
                        given-grid)]
    (->seqgrid
      width
      height
      eas-res
      sou-res
      norwes-corner
      (into-array
        Double/TYPE ;; Hopefully this always works...
        data-seq))))

(defn
  convert-to-normalized
  "Assuming data on the range:
  0-MAX
  makes data on the:
  0-1 range"
  [grid]
  (let [data-max (->> grid
                      data
                      (apply max))]
    (build-grid (params grid)
                (normalized-data grid
                                 0
                                 data-max))))
