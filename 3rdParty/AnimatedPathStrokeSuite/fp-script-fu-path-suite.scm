; GIMP - The GNU Image Manipulation Program
; Copyright (C) 1995 Spencer Kimball and Peter Mattis
;
; This program is free software; you can redistribute it and/or modify
; it under the terms of the GNU General Public License as published by
; the Free Software Foundation; either version 2 of the License, or
; (at your option) any later version.
;
; This program is distributed in the hope that it will be useful,
; but WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
; GNU General Public License for more details.
;
; You should have received a copy of the GNU General Public License
; along with this program; if not, write to the Free Software
; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; This script was created as a gift to two individuals who have inspired me through their commitment to GIMP, 
;;;; their willingness to share knowledge, and dedication to artists all around the world.
;;;;
;;;; First and foremost is ClayOgre.  Like me, ClayOgre, whom I haven't seen online in awhile had a fascination
;;;; with animations and paths.  When I first began scripting, he suggested that I try and figure out how to create
;;;; scripts combining animations and paths.  I laughed!  No way it would ever happen.  Well, here we are today
;;;; with my very first animated path script.  Thanks, CO, for your inspiration.
;;;;
;;;; Secondly, and certainly not least, is to saulgoode.  Saulgoode is a silent giant in the GIMP community.  You
;;;; never see much artwork produced by him, but what he bestows on the GIMP community through his tutorials,
;;;; scripts, understanding of programming and computers, and the inner-workings of GIMP is definitely "artwork!"
;;;; Without his sharing of knowledge, I would never have attempted scripting.
;;;;
;;;; My hats off to both of you for your inspiration!  Thanks for all you do and I hope this script meets your
;;;; satisfaction.
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; This script takes a single path or a series of paths and paints the paths on individual frames creating
;;;; an animated hand-drawn effect.
;;;;
;;;; The script can be found under the Paths Dialog (Right-Click on the desired path) Animated Path Stroking...
;;;;
;;;; Several options are included:
;;;;
;;;; Painting path segments and adding the segments to the image.
;;;; Painting path segments and not adding the segments to the image.
;;;; Not painting path segments, but adding the segments to the image.
;;;; Painting previously created segments - one per layer with no new segments added to the image.
;;;; Keyframing (Painting by selecting a starting an ending point)and adding the segments to the image.
;;;; Keyframing and not adding the segments to the image.
;;;;
;;;; The user chooses the path to work with, the number of frames to paint on or number of segments to 
;;;; split the path into, which layer to work on, the interval that points are collected. (Default is every 2
;;;; pixels, but it can be 1-5 pixels or the calculated segment length based on the path length / the number
;;;; of frames.) The user can paint with a solid color or gradient, limit the painting to a selected area, and
;;;; set several brush options that can only be added to scripts through the script dialog.
;;;; Brush opacity/blend mode are currently modified in the brush options dialog.
;;;;
;;;; In addition to this script, I've written some supplemental scripts to help users with the main script.
;;;; These scripts can found in the path dialog window and include the following functions:
;;;;
;;;; Path Details... Provides information about the chosen path.
;;;; Path Direction... Shows the user which way the path draws.
;;;; Path Reverse Direction... Creates a copy of the chosen path, but reversing the direction it's drawn.
;;;; Show Point... Identifies a chosen point within the path.
;;;; Stroke Delete... Deletes a chosen stroke from a path.
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




(define (fp-script-fu-anim-path-stroke	image	 
				vectors		; the path being worked on
				stroke-id		; which path stroke to work with
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



; set up some variables.

(let* (
		    	
		(dist 0)				; used to track the distance along the path being evaluated
		(pathLength 0)			; a calculation of the path length
		(numPoints 0)			; used to set the number of points in an array
		(points 0)				; used as an array for holding points from the paths
		(newPoints 0)			; used as an array for holding points from the paths
		(x 0)					; x value of a given path coordinate
		(y 0)					; y value of a given path coordinate
		(x1 0)				; x value of a given path coordinate in the keyframing option
		(y1 0)				; y value of a given path coordinate in the keyframing option
		(x2 0)				; x value of a given path coordinate in the keyframing option
		(y2 0)				; y value of a given path coordinate in the keyframing option
		(newPath 0)				; for creating a temporary path
		(segmentLength 0)			; the calculated segment length (pathLength / frames)
		(activeLayer 0)			; the active layer
		(counter 1)				; used to keep track of the status of various tests
		(origSegmentLength 0)		; a "multiplier" for the segment length
		(precision 1)			; how precise the script should be when determine points on the path
		(pathName 0)			; used for setting a new path name
		(origPathName 0)			; used to hold the original path name
		(pos 0)				; used to identify the position in an array
		(pos2 0)				; used to identify the position in an array for keyframing
		(arrayLength 0)			; establishes the length of the given array
		(origSelect 0)			; used to turn on/off existing selection
		(layerList 0)			; tracks the layer ID's in the image
		(stackPos 0)			; tracks the position in the layer stack of a given layer
		(tempLayer 0)			; a temporary copy of a given layer
		(pathList 0)			; tracks the path ID's in the image
		(pathNums 0)			; tracks the number of paths in the image
		(pathPos 0)				; tracks the position in the layer stack of a given layer
		(paintTool 0)			; used for determining the paint method being used (paintbrush, airbrush, etc.)
		(closed 0)				; used for determining if the given path is closed
		(kfLastPt 0)			; used for establishing the last point in an array for keyframing
		(distPct 0)				; used to convert the distInc to a decimal for estimating array size
		(distInc 1)				; how often points are collected
		(layer 0)				; the active layer in the image
		(fadeOut (/ fadeOut 100))	; converting fadeOut to a percentage 
		(fade 0)				; used to fade out the brush during the drawing
		
)	

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; If the path exists, but it is empty, an error is given.  If the user has chosen a stroke within the path that
;;;; does not exist, an error is given.  If Option 3 is chosen (paint existing paths) and only 1 path exists, an 
;;;; error is given.  For options 4 and 5 (keyframing), the beginning point must be less than the ending point and
;;;; the points must exist.  Otherwise, errors are given.  A test calculation is set up using the path length + 
;;;; 200 to determine an approximate array size based on the distInc.(the choice of 200 is arbitrary to allow for
;;;; collection of points at segment lengths that are not whole numbers) If the arrayLength is > 25000, an error
;;;; is generated.  I added this test based on a response from saulgoode, exceeding 25,000 elements in arrays may
;;;; result in a slowdown.  I don't want the script to cause system crashes.
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(if (= (car (gimp-vectors-get-strokes vectors)) 0)
(gimp-message "There are no points in the selected path.")
(begin

(if (> stroke-id (car (gimp-vectors-get-strokes vectors)))
(gimp-message "That stroke does not exist in the selected path.")
(begin

(if (and (= option 3) (< (car (gimp-image-get-vectors image)) 2))
(gimp-message "You must have more than one path for this to work.")
(begin

(if (and (or (= option 4)(= option 5))(> kf1 (/ (cadr (gimp-vectors-stroke-get-points vectors stroke-id)) 6)))
(gimp-message "The starting keyframe point is greater than the number of points in the image.")
(begin

(if (and (or (= option 4)(= option 5))(> kf2 (/ (cadr (gimp-vectors-stroke-get-points vectors stroke-id)) 6)))
(gimp-message "The ending keyframe point is greater than the number of points in the image.")
(begin


(if (or (and (or (= option 4) (= option 5))(> kf1 kf2))
(and (or (= option 4) (= option 5))(= kf1 kf2)))

(gimp-message "The starting keyframe must be less than the ending keyframe.")
(begin

(set! pathLength (+ 200 (car (gimp-vectors-stroke-get-length vectors stroke-id precision))))
(set! distPct (/ 1 distInc))
(set! arrayLength (inexact->exact (* (* (ceiling pathLength) distPct) 6)))

(if (> arrayLength 25000)
(gimp-message "Your path appears to be too long and could cause GIMP to crash.  Reduce the length of your path 
(i.e. split into smaller segments) to allow the script to work properly.")
(begin

(set! pathLength (car (gimp-vectors-stroke-get-length vectors stroke-id precision)))
(set! segmentLength (/ pathLength frames))
(if (< segmentLength 1)
(gimp-message "You have too many frames for the given path length.  Select a smaller number of frames.")
(begin




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; Set up the script so that it can be undone with a single click and that all user settings are
;;;; reset once the script is finished running.
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(gimp-context-push)
(gimp-image-undo-group-start image)


; set the active layer, some temporary layers, the stack position of the active layer, and the list of layers
; in the image

(set! activeLayer (car (gimp-image-get-active-layer image)))
(gimp-layer-add-alpha activeLayer)
(set! tempLayer (car (gimp-layer-copy activeLayer image)))
(set! stackPos (car (gimp-image-get-layer-position image activeLayer)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; Sets handling to FALSE if the stackPos = 0.  This is used as a "key" to "unlock" a test later.  Might not
;;;; be the most efficient way, but it worked, so I went with it!  Also, the layer ID numbers within the image
;;;; are collected to be used later.
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(if (= stackPos 0)
(set! handling FALSE)
)

(set! layerList (cadr (gimp-image-get-layers image)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; Determine the current paint tool (method) set by the user and then define a function for painting 
;;;; depending on which tool is chosen.  Several of the methods cannot be scripted, so if they are selected,
;;;; the script automatically applies the paintbrush method.  I wish the ink tool could be added to scripts.
;;;; It would be great for this script!) Also, if a gradient is chosen to be painted, it is set, otherwise
;;;; the gradient length is set to zero.  If this didn't take place, the gradient length value would be passed
;;;; into the script and used even if a solid color was chosen.
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(if (= colorMethod 1)
	(gimp-context-set-gradient gradient)
	(set! gradLength 0)
)


(gimp-context-set-foreground foregroundColor)
(gimp-context-set-background backgroundColor)

(set! paintTool (car (gimp-context-get-paint-method)))


(if (string=? "gimp-pencil" paintTool)
	(define (user-paint-tool activeLayer)(gimp-pencil activeLayer numPoints points))
  )

(if (string=? "gimp-eraser" paintTool)
	(begin
	(gimp-context-set-background '(0 0 0))
	(define (user-paint-tool activeLayer)(gimp-eraser activeLayer numPoints points hardness paintMethod))
	)
)

(if (string=? "gimp-airbrush" paintTool)
	(begin
	(define (user-paint-tool activeLayer)(gimp-airbrush activeLayer pressure numPoints points))
	)
)

(if (string=? "gimp-smudge" paintTool)
	(define (user-paint-tool activeLayer)(gimp-smudge activeLayer pressure numPoints points))
)

(if (or (string=? "gimp-ink" paintTool)(string=? "gimp-paintbrush" paintTool)
	(string=? "gimp-clone" paintTool) (string=? "gimp-heal" paintTool)
	(string=? "gimp-perspective-clone" paintTool) (string=? "gimp-convolve" paintTool)
	)
	(define (user-paint-tool activeLayer)(gimp-paintbrush activeLayer fade numPoints points paintMethod gradLength))
)

(if (string=? "gimp-dodge-burn" paintTool)
	(define (user-paint-tool activeLayer)(gimp-dodgeburn activeLayer pressure hardness dodgeMode numPoints points))
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; Test to see if the user wants to create new path segments instead of using previously created ones (option 3).  
;;;; If so, the script then checks to see if a keyframing option is to be used.  If keyframing is desired, 
;;;; the 2 points chosen are used to create a temporary path using the original path as a template.  Once
;;;; the temporary path is created, it is passed into the main routine, where points are then calculated 
;;;; along the path at the distance interval specified by the user.  The default distance interval has been
;;;; set at 2, however, the user can set it between 1 & 5 or using the calculated segment length based on the 
;;;; number of frames.  2 seems to collect a good number of points along the path without missing any details
;;;; and will generally allow the use of longer paths without slowing down GIMP with an excessive array size.
;;;; Higher distance increments will collect less points, but the path details may be lost.  If the distance
;;;; increment happens to be greater than the calculated segment length, it is set = to the segment length.
;;;; The array size is the (path length + 200) * 6.  6 points are required for each path point (2 incoming and 2 
;;;; outgoing control handles, and 2 anchor points).  I added 200 to the path length arbitrarily to account  
;;;; for some extra points along the way.  Based on a response from saulgoode, exceeding 25,000 elements in
;;;; arrays may result in a slowdown.  Because of this, I've set up the script to provide an error if it appears 
;;;; the array length is greater than 25,000. The user then can split up the long path into smaller chunks
;;;; and proceed.
;;;;
;;;; Essentially, the process here is fairly straightforward, but I'm not sure that my approach is the most
;;;; efficient from a programming standpoint(I'm still a beginner!)  The first thing we do is take the active
;;;; path, establish a path length, segment length (path length / number of frames), a duplicate of the segment
;;;; length (origSegmentLength) which is used as a multiplier, get the path name and whether or not it is closed.
;;;;
;;;; Next, an array to hold the path points as they are collected.  (My original design did not use an array
;;;; storing the points, but used the gimp-vectors-bezier-stroke-new-moveto and gimp-vectors-bezier-stroke-lineto
;;;; procedures.  There was a flaw with this design for this particular use: points are always added to the 
;;;; beginning of the array rather than appending it.  With each added point, the beginning of the path segment
;;;; was always changing and when stroked with a non-animated brush at a spacing other than 1, it always resulted
;;;; in an animated look.  While this might be acceptable for some cases, in most it was not, IMO.)
;;;;
;;;; The script then begins collecting points at the selected interval and adding them to the array.  Points
;;;; are collected as long as the distance at which the points are being collected is less than the path length
;;;; as well as that the counter is less than or equal to the number of frames.  (Originally, I didn't create 
;;;; path to describe the entire original path as the path was already there and could easily be painted.  This
;;;; was fine as long as the brush spacing was set to 1.  However, if the brush spacing were wider, the last
;;;; painted segment looked different than the previous.  The only way I could work around that was to go ahead
;;;; and create an exact copy of the original with points along the entire distance.)
;;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(if (<> option 3)

(begin ; option <> 3

(set! origPathName (car (gimp-vectors-get-name vectors)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; If option is = 4 or 5 (keyframing), proceed with this section. Create a temporary path using the 
;;;; start/end points chosen and then pass the path into the main routine where segments/painting occurs.  If
;;;; option 4 is chosen, the path segments are added to the image.  If option 5, the segments are not added.
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(if (or (= option 4) (= option 5))

(begin  ; option = 4 or option =5
(set! points (caddr (gimp-vectors-stroke-get-points vectors stroke-id)))
(set! pos (* (- kf1 1) 6))
(set! kfLastPt (+ (* (- kf2 1) 6) 5))
(set! arrayLength (* (+ (- kf2 kf1) 1) 6))
(set! newPoints (cons-array arrayLength 'double))

(while (<= counter (/ arrayLength 6))
(set! x (aref points pos))
(set! y (aref points (+ pos 1)))
(set! x1 (aref points (+ pos 2)))
(set! y1 (aref points (+ pos 3)))
(set! x2 (aref points (+ pos 4)))
(set! y2 (aref points (+ pos 5)))

(aset newPoints pos2 x)
(aset newPoints (+ pos2 1) y)
(aset newPoints (+ pos2 2) x1)
(aset newPoints (+ pos2 3) y1)
(aset newPoints (+ pos2 4) x2)
(aset newPoints (+ pos2 5) y2)


(set! pos (+ pos 6))
(set! pos2 (+ pos2 6))
(set! counter (+ counter 1))
); closure for while counter <= (/ arrayLength 6))

(set! vectors (car (gimp-vectors-new image origPathName)))
(gimp-vectors-stroke-new-from-points vectors 0 arrayLength newPoints FALSE)

(if (= option 4) ; add paths
(begin ; for option = 4 add paths
(set! pathName (string-append origPathName " Keyframes Points " (number->string kf1) " - " (number->string kf2)))
(gimp-image-add-vectors image vectors -1)
(gimp-vectors-set-name vectors pathName)

); closure for begin option = 4
); closure for if option = 4

(set! pos 0)
(set! counter 1)
(set! stroke-id 1)

); closure for begin option = 4 or option = 5
); closure for if option = 4 or option = 5

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; If option 4 or 5 were used above, the path generated from that routine is passed into this section for 
;;;; further processing.  If option 4 or 5 were not used, this section proceeds using the active path.  The
;;;; first section defines some variables, sets up the path array, and gets the first point at distance 0.
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(set! origPathName (car (gimp-vectors-get-name vectors)))
(set! pathLength (car (gimp-vectors-stroke-get-length vectors stroke-id precision)))
(set! segmentLength (/ pathLength frames))
(set! origSegmentLength segmentLength)

(set! closed (cadddr (gimp-vectors-stroke-get-points vectors stroke-id)))


(set! arrayLength (inexact->exact (* (+ (ceiling pathLength) 200) 6)))
(set! points (cons-array arrayLength 'double))
(set! x (car (gimp-vectors-stroke-get-point-at-dist vectors stroke-id dist precision)))
(set! y (cadr (gimp-vectors-stroke-get-point-at-dist vectors stroke-id dist precision)))

(aset points pos x)
(aset points (+ pos 1) y)
(aset points (+ pos 2) x)
(aset points (+ pos 3) y)
(aset points (+ pos 4) x)
(aset points (+ pos 5) y)


(set! dist (+ distInc))

(set! pos 6)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; A loop is then created for collecting the remaining points.  As long as the distance is less than the 
;;;; path length and the counter is less than or equal to the frames, path points are collected.  One the 
;;;; collection starts, there are 2 main routines: collection of points up to the segment length and then 
;;;; handling the segments.  The handling of the segments routine is the most complex and addressed first by
;;;; the script.  If the distance tracker equals the truncated (whole number) segment length, the script needs
;;;; to collect 1 or 2 final points for the path to be created.  If the distance is less than the segment length,
;;;; the script captures the point at that distance, then a final point is captured at the segment length.  A
;;;; If the distance is not less than the segment length, a point is only collected at the segment length. A
;;;; path is then created and painted and/or added to the image.   The other test for determining how close 
;;;; the distance is to the segment length is by checking to see if the distance + distInc amount >= segment
;;;; I set this up to account for the option for different distance settings (1-5). 
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(while (and (< dist pathLength) (<= counter frames))

	(if  (= dist (trunc segmentLength))
		
		(begin ; (= dist (trunc segmentLength))
	
			(if (< dist segmentLength)

				(begin ; (< dist segmentLength)
			
		(set! x (car (gimp-vectors-stroke-get-point-at-dist vectors stroke-id dist precision)))
		(set! y (cadr (gimp-vectors-stroke-get-point-at-dist vectors stroke-id dist precision)))
		
		
		(aset points pos x)
		(aset points (+ pos 1) y)
		(aset points (+ pos 2) x)
		(aset points (+ pos 3) y)
		(aset points (+ pos 4) x)
		(aset points (+ pos 5) y)

		(set! pos (+ pos 6))
		(set! numPoints (+ numPoints 6))

				); closure for begin (< dist segmentLength)

			); closure for if (< dist segmentLength)


		(set! x (car (gimp-vectors-stroke-get-point-at-dist vectors stroke-id segmentLength 1)))
		(set! y (cadr (gimp-vectors-stroke-get-point-at-dist vectors stroke-id segmentLength 1)))
		
		(aset points pos x)
		(aset points (+ pos 1) y)
		(aset points (+ pos 2) x)
		(aset points (+ pos 3) y)
		(aset points (+ pos 4) x)
		(aset points (+ pos 5) y)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; The new path is created.  If the path represents a copy of the entire path (the last frame), a check is done
;;;; to see if the original path is closed.  If so, the new path is closed too.  
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


		(set! newPath (car (gimp-vectors-new image origPathName)))

		(if (and (= frames 0)(= closed TRUE))
			(gimp-vectors-stroke-new-from-points newPath 0 numPoints points TRUE)
			(gimp-vectors-stroke-new-from-points newPath 0 numPoints points FALSE)
		)
		
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; A test is now run to see if the user wants the new path added to the image.  If so, it's assigned a name 
;;;; based on the original path name and added.  The name assigned depends on whether it's the first one or 
;;;; anything after. 
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


			(if (or (= option 0)(= option 2) (= option 4)) 

				(begin ; (or (= option 0)(= option 2) (= option 4)) 
					(if (= counter 1)
						(begin
						(set! pathName (string-append origPathName " \n- Original, Unmodified Path"))
						(gimp-vectors-set-name vectors pathName)
						); closure for (or (= option 0)(= option 2) (= option 4)) 
					); closure for if (or (= option 0)(= option 2) (= option 4)) 

				(set! pathName (string-append origPathName " \nSegment " (number->string counter)))
				(gimp-vectors-set-name newPath pathName)
				(gimp-progress-set-text (string-append "Adding path segment to image."))
				(gimp-image-add-vectors image newPath -1)
				); closure for begin (or (= option 0)(= option 2) (= option 4)) 
			); closure for if (or (= option 0)(= option 2) (= option 4)) 




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  
;;;; If the user wants (only option 2 does not paint) the path painted, it is done using the brush settings/colors
;;;; chosen.  Some brush features can't be manipulated with a script.  Others are required to established within
;;;; a script if they are to be used.  This is inconvenient, but that's the way it is.  I wanted to keep the 
;;;; script interface as simple as possible and to allow space for future changes as desired.  So, rather than
;;;; adding too many brush features to the code, I only added those that were necessary.  The others are required
;;;; to be set by the user in the appropriate tool dialog.
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	

(if (<> option 2)
	(begin

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  
;;;; Here the script checks to see where the current layer is within the stack.  If it is at the top (layer "0"),
;;;; one routine is run.  If it is somewhere in stack below the top, another routine is run. As stated in a
;;;; previous comment, if the stackPos is intially 0, handling is set to false.  The handling test can be used as
;;;; a "key" to "unlock" the test later, as I describe below.
;;;;
;;;; If the stack position is 0, the first routine begins.  The script always paints the active layer.  If the 
;;;; brush being painted with always used normal mode and opacity of 100, there would probably be no problem, 
;;;; with painting the active layer and then making a copy of that, adding it to the top, and painting it...
;;;; continuing until the number of frames is met.  However, I discovered that in order to account for various
;;;; opacities/brush modes, I needed to make a temporary copy of the active layer and make copies of it as 
;;;; necessary.  Once the temp copy is made, the active layer is painted.
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(if (and (= stackPos 0) (= handling FALSE))

(begin

(set! tempLayer (car (gimp-layer-copy activeLayer TRUE)))
(gimp-progress-set-text (string-append "Painting active layer."))
(set! fade (* fadeOut (* 2.5 segmentLength)))
(user-paint-tool activeLayer)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  
;;;; Next, a test is run to see if we are finished painting the last frame.  If not, we need to add the temp layer
;;;; to the image and set it as the active layer.  Counters are adjusted (further down the script) and the script
;;;; goes back to collecting points, if required, or painting/copying layers, as specified by the user. 
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	(if (< counter frames)
		(begin
		(gimp-progress-set-text (string-append "Adding temporary layer to image."))
		(gimp-image-add-layer image tempLayer -1)
		(set! activeLayer (car (gimp-image-get-active-layer image)))		
		); closure for begin counter < frames
	); closure for if counter < frames

); closure for begin stackPos = 0 and handling = False

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  
;;;; If the stackPos <> 0 and handling <> False, the script needs to set the active layer (determined by the stack
;;;; position variable).  Since there are layers above it, the script, at this point doesn't need to make any 
;;;; copies; it paints the active layer.
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(begin  ; if stackPos <> 0 and handling <> False

(set! activeLayer (car (gimp-image-set-active-layer image (aref layerList stackPos))))
(set! activeLayer (car (gimp-image-get-active-layer image)))
(gimp-progress-set-text (string-append "Painting active layer."))
(if (= fadeLast FALSE)
	(set! fadeOut 0.0)
)
(set! fade (* fadeOut (* 2.5 segmentLength)))
(user-paint-tool activeLayer)
(set! stackPos (- stackPos 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; Next, a test is run to see if the number of frames has been met and if the handling is true.  Recall that
;;;; when the script was run, if the user wanted handling to be true, it's only made false if the script starts
;;;; at the top of the layer.  If there are no more frames and the user wants handling (paints every remaining
;;;; layer above the current one with the full path, the next routine handles that.  If the user doesn't want
;;;; handling, the remaining layers are left unpainted, which will result in that portion of the animation abruptly
;;;; ending at that point should additional paths be painted later above it.  While this may be the desired 
;;;; effect, in most situations, it probably isn't.  
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(if (and (= counter frames) (= handling TRUE))

	(begin

		(while (>= stackPos 0)
		(set! activeLayer (car (gimp-image-set-active-layer image (aref layerList stackPos))))
		(set! activeLayer (car (gimp-image-get-active-layer image)))
		(gimp-progress-set-text (string-append "Painting active layer."))
		(if (= fadeLast FALSE)
			(set! fadeOut 0.0)
		)
		(set! fade (* fadeOut (* 2.5 segmentLength)))
		(user-paint-tool activeLayer)
		(set! stackPos (- stackPos 1))
		); closure for while stackPos >= 0
	
	) ; closure for begin counter = frames and handling = TRUE

); closure for if counter = frames and handling = TRUE

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; If we started below the top of the stack and the test to see if counter = frames and handling = TRUE was not
;;;; met, the script need to know what to do next.  Because the stackPos variable was reduced above, it's now likely
;;;; that it = 0.  If this is the case and the counter doesn't yet equal the frames, we still have some points to
;;;; collect, paths to create (and add, if required), and paint.  Because the routine will take us back to the 
;;;; points collection routine, when it comes time to paint, our stackPos now equals 0 and we would like to use
;;;; "(if (and (= stackPos 0) (= handling FALSE))" routine again.  Currently, our handling equals True.  As
;;;; stated, we set the "key" (handling) to False, which allows us to use that routine again.
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	(if (= stackPos 0)
		(begin
		(set! handling FALSE)
		(set! activeLayer (car (gimp-image-set-active-layer image (aref layerList stackPos))))
		(set! activeLayer (car (gimp-image-get-active-layer image)))
		)
	)

); closure for begin stackPos = 0 and handling = FALSE

); closure for if stackPos = 0 and handling = FALSE


); closure for begin option <> 2
); closure for if option <> 2


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  
;;;; Now that a segment has been painted and added to the image as requested,  Counters are adjusted to run the  
;;;; process again.
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



		(set! segmentLength (+ segmentLength origSegmentLength))
		(set! counter (+ counter 1))
		(set! dist (+ dist distInc))
		(set! pos (+ pos 6))		
		(set! numPoints (+ numPoints 6))		
		);begin
		
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  
;;;; If the points collected for the new path have not, so far, exceeded the segment length, no path can be
;;;; created and the above painting/added path to the image could not take place.  Therefore, the next point is
;;;; collected and counters adjusted to run the process again.
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


		(begin
		(gimp-progress-set-text (string-append "Calculating points along the path for segment " 
			(number->string counter)))
		(gimp-progress-update (/ counter frames))
		(set! x (car (gimp-vectors-stroke-get-point-at-dist vectors stroke-id dist precision)))
		(set! y (cadr (gimp-vectors-stroke-get-point-at-dist vectors stroke-id dist precision)))

		(aset points pos x)
		(aset points (+ pos 1) y)
		(aset points (+ pos 2) x)
		(aset points (+ pos 3) y)
		(aset points (+ pos 4) x)
		(aset points (+ pos 5) y)

		(set! pos (+ pos 6))
		(set! numPoints (+ numPoints 6))
		(set! dist (+ dist distInc))

		);begin				

	); closure for "(if  (= dist (trunc segmentLength))"

); closure for "(while (and (< dist pathLength) (<= counter frames))"

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  
;;;; Option 2 (creating and painting of new path segments) is complete.
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



); closure begin option <> 3

); closure if option <> 3


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;	
;;;; Section for stroking existing path segments.  No new path segments are created.  Same layer handling options
;;;; are used: can paint the remaining layers above it to the top of the stack.  This generally follows the same
;;;; concepts as described above; however, an added routine is provided to handle all of the paths in the stack.
;;;; Similar to the stackPos method, a pathPos determines the position of the path in the stack.
;;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(if (= option 3)

(begin

(set! pathList (cadr (gimp-image-get-vectors image)))
(set! pathNums (car (gimp-image-get-vectors image)))
(set! pathPos (car (gimp-image-get-vectors-position image vectors)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;	
;;;; If the number of frames selected by the user exceeds the number of paths remaining in the stack (active path
;;;; + all paths above it), the number of frames is set to = the minimum value of the number of paths or the
;;;; pathPos + 1.  Let's say there are 5 paths, the active path in the stack is at the bottom of the path stack
;;;; (pathPos 4)and the user chooses 10 frames.  There aren't enough paths (5) to paint 10 frames, so since
;;;; pathNums = 5 and pathPos (4) + 1 = 5 the minimum of these two values is both 5, and the number of frames is 
;;;; set to 5.  If the number of frames is 10, there are 5 paths, and the stackPos is one up from the bottom (3),
;;;; the minimum of pathNums (5) and pathPos (3) +1 = 4.  In that case, number of frames would be set to 4.
;;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(if (> frames (+ pathPos 1))
(set! frames (min pathNums (+ pathPos 1)))
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;	
;;;; As long as the number of counter is <= frames, run the routine.  The first is test the stackPos (layer) and if
;;;; it's 0 and handling is false, the number of points in the path and the point ID's are obtained.  Since the 
;;;; active layer is at the top, we need additional copies of the layer to work with.  A temp copy is made and 
;;;; the active layer is painted.
;;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(while (<= counter frames)

(if (and (= stackPos 0) (= handling FALSE))

(begin     ; (and (= stackPos 0) (= handling FALSE))

(set! numPoints (car (gimp-vectors-stroke-interpolate vectors stroke-id precision)))
(set! points (cadr (gimp-vectors-stroke-interpolate vectors stroke-id precision)))
(set! activeLayer (car (gimp-image-get-active-layer image)))
(set! tempLayer (car (gimp-layer-copy activeLayer TRUE)))
(gimp-progress-set-text (string-append "Painting active layer."))
(gimp-progress-update (/ counter frames))
(set! fade (* fadeOut (* 2.5 segmentLength)))
(user-paint-tool activeLayer)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;	
;;;; If the counter < frames add the temp copy, make it active, and reduce the pathPos by 1.  Go to the portion
;;;; of the routine where the counter is adjusted, the next path is made active, and the main routine is run again.
;;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	
	(if (< counter frames)
		(begin   ; if counter < frames
		(gimp-image-add-layer image tempLayer -1)
		(set! activeLayer (car (gimp-image-get-active-layer image)))	
		(set! pathPos (- pathPos 1))	
		); closure for begin counter < frames
	); closure if counter < frames

); closure for begin (and (= stackPos 0) (= handling FALSE))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;	
;;;; If the stackPos <> 0 and handling <> False, get the points/path ID's, set the active layer, and paint it.  
;;;; reduce the stackPos/pathPos by 1. 
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(begin
(set! numPoints (car (gimp-vectors-stroke-interpolate vectors stroke-id precision)))
(set! points (cadr (gimp-vectors-stroke-interpolate vectors stroke-id precision)))
(set! activeLayer (car (gimp-image-set-active-layer image (aref layerList stackPos))))
(set! activeLayer (car (gimp-image-get-active-layer image)))
(gimp-progress-set-text (string-append "Painting active layer."))
(if (= fadeLast FALSE)
	(set! fadeOut 0.0)
)
(set! fade (* fadeOut (* 2.5 segmentLength)))
(user-paint-tool activeLayer)
(set! stackPos (- stackPos 1))
(set! pathPos (- pathPos 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;	
;;;; Test to see if counter = frames and handling = True.  If so, proceed.  As long as the stackPos (layer) is >=0,
;;;; paint the final path on each remaining layer in the stack.
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(if (and (= counter frames) (= handling TRUE))

	(begin

		(while (>= stackPos 0)
		(set! activeLayer (car (gimp-image-set-active-layer image (aref layerList stackPos))))
		(set! activeLayer (car (gimp-image-get-active-layer image)))
		(gimp-progress-set-text (string-append "Painting active layer."))
		(if (= fadeLast FALSE)
			(set! fadeOut 0.0)
		)
		(set! fade (* fadeOut (* 2.5 segmentLength)))
		(user-paint-tool activeLayer)
		(set! stackPos (- stackPos 1))
		); closure for while
	
	) ; closure for begin

); closure for if counter = frames and handling = TRUE

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;	
;;;; If the stackPos is = 0, set handling to False, set the new active layer.
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


			(if (= stackPos 0)
				(begin
				(set! handling FALSE)
				(set! activeLayer (car (gimp-image-set-active-layer image (aref layerList stackPos))))
				(set! activeLayer (car (gimp-image-get-active-layer image)))
				)
			)

); closure for begin stackPos = 0 and handling = FALSE

); closure for if stackPos = 0 and handling = FALSE

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;	
;;;; Set the next active path and adjust the counter by 1.  Return to main routine and run again as required.
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(set! vectors (car (gimp-image-set-active-vectors image (aref pathList pathPos))))
(set! vectors (car (gimp-image-get-active-vectors image)))
(set! counter (+ counter 1))


); closure for while counter <= frames

); closure begin option = 3
); closure for option = 3


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; Allow for the script actions to be undone with a single click and reset all user settings that
;;;; existed prior to running the script.  Also added code to reset paint method.  For some reason, it always
;;;; goes back to paintbrush once the script finishes.  I want it to stay at what it was originally.  The user
;;;; needs to be the one to change it.
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(gimp-context-set-paint-method paintTool)
(gimp-image-undo-group-end image)
(gimp-context-pop)
(gimp-displays-flush)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; Create closures for various tests used at beginning of the script and end the script
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

); closure for begin "Too many frames for path length"
); closure for if "Too many frames for path length"

); closure for begin "Reduce the length of your path"
); closure for if "Reduce the length of your path" 

); closure for begin "starting keyframe greater than number of points" Option 4 or 5
); closure for if "starting keyframe greater than number of points" Option 4 or 5

); closure for begin "ending keyframe greater than number of points" Option 4 or 5
); closure for if "ending keyframe greater than number of points" Option 4 or 5

); closure for begin "keyframe 1 needs to be less than keyframe 2" Option 4 or 5
); closure for if "keyframe 1 needs to be less than keyframe 2" Option 4 or 5

); closure for begin "need more than one path" Option 3
); closure for if "need more than one path" Option 3

); closure for begin "stroke does not exist"
); closure for if the test "stroke does not exist"


); closure for begin "points exist in the path"
); closure for if the test "points exist in the path"


  );  closure for the "let*" block
); closure for the script

(script-fu-register "fp-script-fu-anim-path-stroke"
  "<Vectors>/Animated Path Stroking..."
  "Create an animated paint effect using paths"
  "Art Wade"
  "Art Wade"
  "January 17, 2010"
  "RGB* GRAYSCALE*"
  SF-IMAGE       	"Image" 0
  SF-VECTORS	"Path" 0
  SF-ADJUSTMENT	"Which stroke to paint or split"	'(1 1 1000 1 1 0 1)
  SF-ADJUSTMENT   "Number of frames (layers) to paint the path or number of segments to split the path into" '(10 2 2000 1 1 0 1)
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



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; Supplemental path scripts to make life a little easier!
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; This script shows the user what direction the path or a given stroke within a path is heading.  A
;;;; layer is added to the image and a line is painted with the beginning point being green and the 
;;;; ending point being red.  A message pops up indicating whether the path is open or closed.  If no 
;;;; points exist within the path or a stroke does not exist, an error message is given.  Once you know
;;;; the direction, you can reverse it with the Reverse Path Direction script.
;;;;
;;;; The script can be found under the paths dialog (Right click on the desired path and choose
;;;; "Path Stroke Direction..."
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define (fp-script-fu-vector-direction	image		 
				vectors
				stroke-id			
					)

(let* (
		    	
		(layer 0)
		(pathLength 0)
		(x 0)
		(y 0)
		(precision 1)
		(newLayer 0)
		(closed 0)
		(points 0)
		(numCoords 0)
	)

; Test to see if an active path is chosen.  If not, an error message is given.

(if (= (car (gimp-vectors-get-strokes vectors)) 0)
(gimp-message "There are no points in the selected path.")
(begin

(if (> stroke-id (car (gimp-vectors-get-strokes vectors)))
(gimp-message "That stroke does not exist in the selected path.")
(begin

(gimp-context-push)
(gimp-image-undo-group-start image)

(set! layer (car (gimp-image-get-active-layer image)))
(set! newLayer (car (gimp-layer-copy layer TRUE)))
(gimp-image-add-layer image newLayer -1)
(gimp-layer-resize-to-image-size newLayer)
(gimp-drawable-fill newLayer WHITE-FILL)

(set! vectors (car (gimp-image-get-active-vectors image)))
(set! pathLength (car (gimp-vectors-stroke-get-length vectors stroke-id precision)))
(set! numCoords (car (gimp-vectors-stroke-interpolate vectors stroke-id precision)))
(set! points (cadr (gimp-vectors-stroke-interpolate vectors stroke-id precision)))

(set! closed (cadddr (gimp-vectors-stroke-get-points vectors stroke-id)))

  (gimp-context-set-foreground '(0 255 0))
  (gimp-context-set-background '(255 0 0))
  (gimp-context-set-gradient "FG to BG (HSV clockwise hue)")
  (gimp-brush-new "Path Stroke")
  (gimp-brush-set-radius "Path Stroke" 5)
  (gimp-brush-set-hardness "Path Stroke" 0.5)
  (gimp-brush-set-spacing "Path Stroke" 1)
  (gimp-context-set-opacity 100)
  (gimp-context-set-brush "Path Stroke")
  (gimp-paintbrush newLayer 0 numCoords points 0 pathLength)
  (gimp-brush-delete "Path Stroke")
 
(if (= closed TRUE)
(gimp-message (string-append "Path is closed."))
(gimp-message (string-append "Path is not closed."))
)


(gimp-image-undo-group-end image)
(gimp-context-pop)
(gimp-displays-flush)

); closure for begin "stroke does not exist"
); closure for if the test "stroke does not exist"


); closure for begin "points exist in the path"
); closure for if the test "points exist in the path"
)
)

(script-fu-register "fp-script-fu-vector-direction"
  "<Vectors>/Path Direction..."
  "Show the direction which the path is going. Green is start, Red is end. If painted with outer black
\nline, path is closed."
  "Art Wade"
  "Art Wade"
  "January 15, 2010"
  "RGB*"
  SF-IMAGE       	"Image" 0
  SF-VECTORS    	"Path" 0
  SF-ADJUSTMENT	"Which stroke to find its direction"	'(1 1 1000 1 1 0 1)
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; This script lets the user shows the user identify a point on a given path/stroke.  Just select the 
;;;; desire point to locate and the stroke and the script will paint a dot with on the given point.
;;;; If no points exist within the path or a stroke does not exist, an error message is given.
;;;;
;;;; The script can be found under the paths dialog (Right click on the desired path and choose
;;;; "Show Point..."
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define (fp-script-fu-show-point-num	image		 
				vectors
				point
				stroke-id
)

(let* (
		    	
		(layer 0)
		(x 0)
		(y 0)
		(newLayer 0)
		(points 0)
		(newPoints (cons-array 2 'double))
		(coordPos (+ (* 6 (- point 1)) 2))
		(numCoords 0)
		(precision 1)
	)

; Test to see if an active path is chosen.  If not, an error message is given.

(if (= (car (gimp-vectors-get-strokes vectors)) 0)
(gimp-message "There are no points in the selected path.")
(begin

(if (> stroke-id (car (gimp-vectors-get-strokes vectors)))
(gimp-message "That stroke does not exist in the selected path.")

(begin

(set! numCoords (cadr (gimp-vectors-stroke-get-points vectors stroke-id)))

(if (<= point (/ numCoords 6))

(begin

(gimp-context-push)
(gimp-image-undo-group-start image)
(set! layer (car (gimp-image-get-active-layer image)))
(set! newLayer (car (gimp-layer-copy layer TRUE)))
(gimp-image-add-layer image newLayer -1)
(gimp-layer-resize-to-image-size newLayer)
(gimp-drawable-fill newLayer WHITE-FILL)
(set! points (caddr (gimp-vectors-stroke-get-points vectors stroke-id)))


(set! x (aref points coordPos))
(set! y (aref points (+ coordPos 1)))
(aset newPoints 0 x)
(aset newPoints 1 y)


(gimp-context-set-foreground '(255 0 0))
(gimp-context-set-brush "Circle (17)")
(gimp-paintbrush-default newLayer 2 newPoints)


(gimp-image-undo-group-end image)
(gimp-context-pop)
(gimp-displays-flush)

); goes with begin

(begin

(gimp-message (string-append "There are only " (number->string (/ numCoords 6)) " points in the path. Choose a point within this range."))

); goes with begin
); goes with if

); closure for begin "stroke does not exist"
); closure for if the test "stroke does not exist"


); closure for begin "points exist in the path"
); closure for if the test "points exist in the path"



); goes with let
); goes with define

(script-fu-register "fp-script-fu-show-point-num"
  "<Vectors>/Show Point..."
  "Show chosen point number."
  "Art Wade"
  "Art Wade"
  "January 15, 2010"
  "*"
  SF-IMAGE       "Image" 0
  SF-VECTORS    	"Path" 0
  SF-ADJUSTMENT	"Show Point" '(1 1 10000 1 1 0 1)
  SF-ADJUSTMENT	"Which stroke"	'(1 1 1000 1 1 0 1)


)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; This script lets the user reverse the direction of a path or given stroke within a path.
;;;; If no points exist within the path or a stroke does not exist, an error message is given.
;;;;
;;;; The script can be found under the paths dialog (Right click on the desired path and choose
;;;; "Reverse Path Direction..."
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (fp-script-fu-reverse-vector	image		 
				vectors
				stroke-id)

(let* (	(x1 0)
		(y1 0)
		(x2 0)
		(y2 0)
		(x3 0)
		(y3 0)
		(precision 1)
		(counter 0)
		(numCoords 0)
		(points 0)
		(newPoints 0)
		(newArrayPos 0)
		(newVector 0)
		(pointCounter 0)
		(newVectorName 0)
		(closed 0)
	)


(if (= (car (gimp-vectors-get-strokes vectors)) 0)
(gimp-message "There are no points in the selected path.")
(begin

(if (> stroke-id (car (gimp-vectors-get-strokes vectors)))
(gimp-message "That stroke does not exist in the selected path.")

(begin

(gimp-context-push)
(gimp-image-undo-group-start image)

(set! closed (cadddr (gimp-vectors-stroke-get-points vectors stroke-id)))

(set! points (caddr (gimp-vectors-stroke-get-points vectors stroke-id)))
(set! numCoords (cadr (gimp-vectors-stroke-get-points vectors stroke-id)))
(set! newPoints  (cons-array numCoords 'double))
(set! counter numCoords)

(if (= closed TRUE)
(begin

(set! x1 (aref points pointCounter))
(set! y1 (aref points (+ pointCounter 1)))
(set! x2 (aref points (+ pointCounter 2)))
(set! y2 (aref points (+ pointCounter 3)))
(set! x3 (aref points (+ pointCounter 4)))
(set! y3 (aref points (+ pointCounter 5)))

(aset newPoints newArrayPos x1)
(aset newPoints (+ newArrayPos 1) y1)
(aset newPoints (+ newArrayPos 2) x2)
(aset newPoints (+ newArrayPos 3) y2)
(aset newPoints (+ newArrayPos 4) x3)
(aset newPoints (+ newArrayPos 5) y3)

(set! pointCounter (- numCoords 6))
(set! newArrayPos (+ newArrayPos 6))

(while (> counter 6)

(set! x1 (aref points pointCounter))
(set! y1 (aref points (+ pointCounter 1)))
(set! x2 (aref points (+ pointCounter 2)))
(set! y2 (aref points (+ pointCounter 3)))
(set! x3 (aref points (+ pointCounter 4)))
(set! y3 (aref points (+ pointCounter 5)))

(aset newPoints newArrayPos x1)
(aset newPoints (+ newArrayPos 1) y1)
(aset newPoints (+ newArrayPos 2) x2)
(aset newPoints (+ newArrayPos 3) y2)
(aset newPoints (+ newArrayPos 4) x3)
(aset newPoints (+ newArrayPos 5) y3)


(set! counter (- counter 6))
(set! newArrayPos (+ newArrayPos 6))
(set! pointCounter (- pointCounter 6))

); goes with while

); goes with begin

(begin

(set! pointCounter (- numCoords 6))

(while (> counter 5)

(set! x1 (aref points pointCounter))
(set! y1 (aref points (+ pointCounter 1)))
(set! x2 (aref points (+ pointCounter 2)))
(set! y2 (aref points (+ pointCounter 3)))
(set! x3 (aref points (+ pointCounter 4)))
(set! y3 (aref points (+ pointCounter 5)))

(aset newPoints newArrayPos x1)
(aset newPoints (+ newArrayPos 1) y1)
(aset newPoints (+ newArrayPos 2) x2)
(aset newPoints (+ newArrayPos 3) y2)
(aset newPoints (+ newArrayPos 4) x3)
(aset newPoints (+ newArrayPos 5) y3)


(set! counter (- counter 6))
(set! newArrayPos (+ newArrayPos 6))
(set! pointCounter (- pointCounter 6))

); goes with while

); goes with begin

); goes with if


(set! newVector (car (gimp-vectors-new image "New Vector")))
(set! newVectorName (car (gimp-vectors-get-name vectors)))
(gimp-vectors-set-name newVector (string-append "Reversed copy of " newVectorName))
(set! closed (cadddr (gimp-vectors-stroke-get-points vectors stroke-id)))

(if (= closed TRUE)
(gimp-vectors-stroke-new-from-points newVector 0 numCoords newPoints TRUE)
(gimp-vectors-stroke-new-from-points newVector 0 numCoords newPoints FALSE)
)
(gimp-image-add-vectors image newVector -1)
(set! vectors (car (gimp-image-get-active-vectors image)))

(gimp-image-undo-group-end image)
(gimp-context-pop)
(gimp-displays-flush)

); closure for begin "stroke does not exist"
); closure for if the test "stroke does not exist"


); closure for begin "points exist in the path"
); closure for if the test "points exist in the path"


)
)

(script-fu-register "fp-script-fu-reverse-vector"
  "<Vectors>/Reverse Path Direction..."
  "Reverse the path direction."
  "Art Wade"
  "Art Wade"
  "January 15, 2010"
  "*"
  SF-IMAGE       	"Image" 0
  SF-VECTORS    	"Path" 0
  SF-ADJUSTMENT	"Which stroke to reverse"	'(1 1 1000 1 1 0 1)
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; This script identifies some information about the given path or stroke within the path.
;;;; Will give the path length (rounded), the number of points, the number of strokes, and the closed
;;;; status.
;;;;
;;;; If no points exist within the path or a stroke does not exist, an error message is given.
;;;;
;;;; The script can be found under the paths dialog (Right click on the desired path and choose
;;;; "Path Details..."
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(define (fp-script-fu-vector-details	image		 
				vectors
				stroke-id)

(let* (	(precision 1)
		(numCoords 0)
		(points 0)
		(closed 0)
		(pathLength 0)
		(distPct 0)
		(arrayLength 0)
		(pathLength2 0)
		(arrayTest 0)
		(distInc 1)
	)

(if (= (car (gimp-vectors-get-strokes vectors)) 0)
(gimp-message "There are no points in the selected path.")
(begin

(if (> stroke-id (car (gimp-vectors-get-strokes vectors)))
(gimp-message "That stroke does not exist in the selected path.")

(begin


(set! pathLength2 (+ 200 (car (gimp-vectors-stroke-get-length vectors stroke-id precision))))
(set! distPct (/ 1 distInc))
(set! arrayLength (inexact->exact (* (* (ceiling pathLength2) distPct) 6)))


(gimp-context-push)
(gimp-image-undo-group-start image)
(set! pathLength (car (gimp-vectors-stroke-get-length vectors stroke-id precision)))
(set! closed (cadddr (gimp-vectors-stroke-get-points vectors stroke-id)))
(set! numCoords (cadr (gimp-vectors-stroke-get-points vectors stroke-id)))

(if (= closed TRUE)
(set! closed "closed")
(set! closed "not closed.")
)

(if (> arrayLength 25000)
(set! arrayTest "Reduce the length of your path (i.e. split path into smaller segments) to avoid a script error.")
(set! arrayTest "The script should run fine with this path length.")
)

(gimp-image-undo-group-end image)
(gimp-context-pop)
(gimp-displays-flush)



(gimp-message (string-append "Path Length: " (number->string (round pathLength)) "\nNumber of Points: " 
(number->string (/ numCoords 6)) "\nPath is " closed "\n"arrayTest))

); closure for begin "stroke does not exist"
); closure for if the test "stroke does not exist"


); closure for begin "points exist in the path"
); closure for if the test "points exist in the path"

)
)

(script-fu-register "fp-script-fu-vector-details"
  "<Vectors>/Path Details..."
  "Provides path length, number of points, and closed status of the chosen path."
  "Art Wade"
  "Art Wade"
  "January 15, 2010"
  "*"
  SF-IMAGE       	"Image" 0
  SF-VECTORS    	"Path" 0
  SF-ADJUSTMENT	"Which stroke to get information about"	'(1 1 1000 1 1 0 1)
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; This script allows you to delete a stroke within the given path.
;;;;
;;;; If no points exist within the path or a stroke does not exist, an error message is given.
;;;;
;;;; The script can be found under the paths dialog (Right click on the desired path and choose
;;;; "Stroke Delete..."
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(define (fp-script-fu-vector-stroke-delete	image		 
				vectors
				stroke-id)

(let* (	(precision 1)
	)

(if (= (car (gimp-vectors-get-strokes vectors)) 0)
(gimp-message "There are no points in the selected path.")
(begin

(if (> stroke-id (car (gimp-vectors-get-strokes vectors)))
(gimp-message "That stroke does not exist in the selected path.")

(begin

(if (= (car (gimp-vectors-get-strokes vectors)) 1)
(gimp-message "There is only one stroke within the path and it can't be deleted with the script.")
(begin

(gimp-context-push)
(gimp-image-undo-group-start image)
(gimp-vectors-remove-stroke vectors stroke-id)
(gimp-image-undo-group-end image)
(gimp-context-pop)
(gimp-displays-flush)

); closure for begin "only one stroke"
); closure for if "only one stroke"

); closure for begin "stroke does not exist"
); closure for if the test "stroke does not exist"

); closure for begin "points exist in the path"
); closure for if the test "points exist in the path"

)
)

(script-fu-register "fp-script-fu-vector-stroke-delete"
  "<Vectors>/Stroke Delete..."
  "Deletes a given stroke within the chosen path."
  "Art Wade"
  "Art Wade"
  "January 15, 2010"
  "*"
  SF-IMAGE       	"Image" 0
  SF-VECTORS    	"Path" 0
  SF-ADJUSTMENT	"Which stroke to delete"	'(1 1 1000 1 1 0 1)
)
