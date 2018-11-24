; This script will stroke-animate all strokes of a path
; Basically calls fp-script-fu-path-animate for every single stroke of the given path
; has two options
; 1. animate all strokes simultaneously
; 2. animate one-stroke at a time
; All the other options are passed to fp-script-fu-path-animate
; fp-script-fu-path-animate courtesy of Art Wade (https://www.deviantart.com/fence-post/art/Animated-Path-Stroking-Script-152654834)

(define (script-fu-animate-path-strokes	image
				vectors		; the path being worked on
		                animation-mode ; the mode of animation
				frames		; the number of frames to paint the path or establish the path segments
				option		; options on whether to paint the paths or add them to the image
				handling		; paint the remaining frames above a path when painting stops? Usually yes.
				kf1			; path point to start with if keyframe option is chosen
				kf2			; path point to end with if keyframe option is chosen
				colorMethod		; solid color or a gradient
				foregroundColor	; what foreground color to use in painting
				backgroundColor	; background color when a FG-BG gradient is chosen or set to black for the eraser tool
				gradient		; the gradient to paint with
				gradLength		; length of the gradient used before it repeats
				paintMethod		; incremental or constant
				fadeOut		; conversion factor for making the fade option increase based on segment length
				fadeLast		; whether or not the last segment will fade out
				pressure		; for air/smudge - also used for dodge/burn exposure
				hardness		; eraser brush (hard or soft) or the whether dodge or burn is used
				dodgeMode		; whether shadows, midtones, or highlights are affected by dodge/burn				
				)

        (gimp-context-push)
        (gimp-image-undo-group-start image)

	(let* (
        	(number-of-strokes (car (gimp-vectors-get-strokes vectors))) ; number of strokes in the given path
        	(stroke-id 1) ; the stroke  to animate, starts from 1
        	(base-layer (car (gimp-image-get-active-layer image))) ; the layer to go back to, if required
	      )
        
       		(while (<= stroke-id number-of-strokes) 

			(fp-script-fu-anim-path-stroke image vectors stroke-id frames option handling kf1 kf2 colorMethod foregroundColor backgroundColor gradient gradLength paintMethod fadeOut fadeLast pressure hardness dodgeMode)
            
			(if (equal? animation-mode 0)
               		 	; animation modes: 0 - All strokes simultaneously, 1 - one stroke at a time
				; If animation-mode is 0, set the base layer as the active layer so that all strokes will be animated 'simultaneously'
                		(gimp-image-set-active-layer image base-layer)

			    	; if the animation-mode is one path at a time, create a copy of the active layer at the end of each call to fp-script-fu-anim-path-stroke
			    	; and set it as the active layer so that there won't be an overlapping animation frame for two consecutive strokes
				(gimp-image-insert-layer image
					(car (gimp-layer-copy (car (gimp-image-get-active-layer image)) TRUE)) ; copy of the current active layer
					0 ; the parent layer/layer-group - None
					0 ; the position - the top of the layer stack
				)
            		)

                	(set! stroke-id (+ stroke-id 1))
        	)
	)

        (gimp-image-undo-group-end image)
        (gimp-context-pop)

)



(script-fu-register "script-fu-animate-path-strokes"
  "<Vectors>/Animate Path Strokes..."
  "Animate all strokes of a given path by repeatedly calling fp-script-fu-anim-path-stroke"
  "Prafulla Giri"
  "Prafulla Giri"
  "November 24, 2018"
  "RGB* GRAYSCALE*"
  SF-IMAGE       	"Image" 0
  SF-VECTORS	"Path" 0
  SF-OPTION "Animation Mode" '("All Strokes Simultaneously" "One Stroke At A Time")
  SF-ADJUSTMENT   "Number of frames (layers) to paint each stroke or number of segments to split each strokes into" '(10 2 2000 1 1 0 1)
  SF-OPTION		"Paint and path options" '("Paint paths - Add path segments to image"
								"Paint paths - Don't add path segments to image"
								"Don't paint paths - Add path segments to image"
								"Paint paths that have been previously segmented \n- One path per layer, no new path segments are created"
								"Paint by keyframes (choose points) - Add path segments to image"
								"Paint by keyframes (choose points) - Don't add path segments to image")
  SF-TOGGLE		"Add a copy of the final painted path segment to each layer above it in layer stack (Continues to top of layer stack)" TRUE 
  SF-ADJUSTMENT	"Start point for keyframe (Needs to be less than end point)"	'(1 1 2000 1 1 0 1)  
  SF-ADJUSTMENT	"End point for keyframe (Needs to be greater than start point)"	'(2 2 2000 1 1 0 1)
  SF-OPTION		"Color method"  '("Solid Color"
                                   "Gradient")
  SF-COLOR      	"Foreground color"          '(0 0 0)
  SF-COLOR		"Background color (Used only when one of the FG-BG gradients chosen)" '(255 255 255)
  SF-GRADIENT	"Gradient"       "FG to BG (RGB)"
  SF-ADJUSTMENT	"Gradient length" '(0.0 0.0 5000.0 1 1 1 1)
  SF-OPTION		"Paint method (Paintbrush/Eraser)"          	'("Constant"
  									"Incremental")
  SF-ADJUSTMENT	"Fade out (Paintbrush only)" '(0.0 0.0 99.9 1 1 1 1)
  SF-TOGGLE		"Fade out last segment" FALSE 
  SF-ADJUSTMENT	"Airbrush/Smudge pressure or Dodge/Burn exposure" '(0.0 0.0 100.0 1 1 1 1)
  SF-OPTION		"Eraser hardness or Dodge/Burn option" 	'("Hard Brush or Dodge"
								"Soft Brush or Burn")
  SF-OPTION		"Dodge/Burn method"          	'("Shadows"
  									"Midtones"
									"Highlights")
)
