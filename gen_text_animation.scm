;;; A GIMP plugin to help with lyric-video creation
;;; Released under the terms of GPLv3.
;;; Dependencies:
;;; * AnimatedPathStrokeSuite by Fence Post: https://www.deviantart.com/fence-post/art/Animated-Path-Stroking-Script-152654834
;;; * Wrapper around the script: https://github.com/peanutbutterandcrackers/gimp-scripts/blob/master/script-fu-animate-path-strokes.scm

(define (script-fu-generate-text-animation intext intext_font intext_fontsize intext_fontcolor animation_type animation_framecount)
  (gimp-context-push)
  (gimp-context-set-foreground intext_fontcolor)
  (let* (
	 (imagewidth 10)
	 (imageheight 10)
	 (image (car (gimp-image-new
		      imagewidth imageheight RGB)))
	 (img image)
	 (textlayer (car (gimp-text-fontname image -1 0 0 intext 0 TRUE
					     intext_fontsize PIXELS intext_font)))
	 (animationbase)
	 (textpath (car (gimp-vectors-new-from-text-layer image textlayer)))
	 )

    (set! imagewidth (car (gimp-drawable-width textlayer)))
    (set! imageheight (car (gimp-drawable-height textlayer))) 
    (gimp-image-resize image imagewidth imageheight 0 0)

    (gimp-image-insert-vectors image textpath 0 -1)
    
    (set! animationbase (car (gimp-layer-new image imagewidth imageheight RGBA-IMAGE "Base Animation Layer" 100 LAYER-MODE-NORMAL)))
    (gimp-image-insert-layer image animationbase 0 -1)

    ;; animations go in here
    '(let* (
	   (simultaneous_animation 0)
	   (stroke_by_stroke_animation 1)
	   (animation_mode (if (equal? animation_type simultaneous_animation) simultaneous_animation stroke_by_stroke_animation))
	   (paint_path_but_dont_add_path_segments 1)
	   (true 1)
	   (false 0)
	   (solid_color 0)
	   (gradient_method "FG to BG (RGB)")
	   (gradient_length 0.0)
	   (paint_method_constant 0)
	   (paint_method_incremental 1)
	   (fade_out_last_segment false)
	   )
       (script-fu-animate-path-strokes image textpath animation_mode
				      animation_framecount paint_path_but_dont_add_path_segments true
				      1 2 solid_color intext_fontcolor '(255 255 255) gradient_method gradient_length
				      paint_method_constant fade_out_last_segment 0.0 0 0)
      )

    (let* (
	   (true 1)
	   (false 0)
	   (simultaneous_animation 0)
	   (stroke_by_stroke_animation 1)
	   (animation_mode (if (equal? animation_type simultaneous_animation) simultaneous_animation stroke_by_stroke_animation))
	   (paint_path_but_dont_add_segments 1)
	   (solid_color 0)
	   (gradient_method "FG to BG (RGB)")
	   (gradient_length 0.0)
	   (paintmethod_constant 0)
	   (paintmethod_incremental 1)
	   (paintbrush_fadeout 0.0)
	   (fadeout_last_segment false)
	   (pressure_exposure 0.0)
	   (brushtype_hard 0)
	   (brushtype_soft 1)
	   (dodge_burn_method_shadows 0)
	   )
      (script-fu-animate-path-strokes img textpath animation_mode animation_framecount paint_path_but_dont_add_segments
				      true 1 2 solid_color intext_fontcolor '(255 255 255) gradient_method gradient_length
				      paintmethod_constant paintbrush_fadeout fadeout_last_segment pressure_exposure brushtype_hard
				      dodge_burn_method_shadows)
      )

    (gimp-image-raise-item-to-top image textlayer)
    
    (gimp-display-new image)
    (gimp-context-pop)
    (gimp-image-clean-all image)
    )
  )

(script-fu-register
 "script-fu-generate-text-animation"
 "Text Animation Generator"
 "Creates a text animation from the input text with the given parameters"
 "Prafulla Giri"
 "Copyright 2019, Prafulla Giri (GPLv3+)"
 "December 24, 2019"
 ""
 SF-TEXT "Text" "Enter text here."
 SF-FONT "Font" "Sans"
 SF-VALUE "Font size" "12"
 SF-COLOR "Font color" '(0 0 0)
 SF-OPTION "Animation Type" '("All Strokes Simultaneously"
			      "Stroke by Stroke")
 SF-VALUE "No. of Animation Frames" "30"
 )

(script-fu-menu-register "script-fu-generate-text-animation"
			 "<Image>/_Animation")
