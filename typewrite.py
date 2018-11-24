#!/usr/bin/env python

from gimpfu import *

def create_text_layer(drawable, text_color, text_string, font, font_size, draw_on=None):
	# foreground color is used to fill the foreground layer
	if tuple(pdb.gimp_context_get_foreground()) != tuple((i*255 for i in text_color)):
		gimp.set_foreground(text_color)
	text_layer = pdb.gimp_text_fontname(drawable, draw_on, 0, 0, text_string, 10, True, font_size, PIXELS, font)
	return text_layer

def create_background_layer(drawable, bg_color, make_transparent=False):
	# Background color is used to fill the background layer
	if tuple(pdb.gimp_context_get_background()) != tuple((i*255 for i in bg_color)):
		gimp.set_background(bg_color)
	background_layer = pdb.gimp_layer_new(drawable, drawable.width, drawable.height, RGBA_IMAGE, "backdrop", 100.0, LAYER_MODE_NORMAL)
	pdb.gimp_image_insert_layer(drawable, background_layer, None, pdb.gimp_image_get_layers(drawable)[0]) # backdrop always goes at the bottom
	if not make_transparent:
		pdb.gimp_drawable_fill(background_layer, FILL_BACKGROUND)
	return background_layer

def typewrite(initstr, font, font_size, transparent_background, fg_color, bg_color):
	gimp.progress_init("Typewriting. Please wait...")

	img = gimp.Image(100, 100, RGB)

	text_layer = create_text_layer(img, fg_color, initstr, font, font_size) 
	img.resize(text_layer.width, text_layer.height, 0, 0)
	backdrop = create_background_layer(img, bg_color, make_transparent=transparent_background)
	merged_layer = pdb.gimp_image_merge_down(img, text_layer, CLIP_TO_IMAGE)

	for i in range(len(initstr) - 1, 0, -1):
		backdrop = create_background_layer(img, bg_color, make_transparent=transparent_background)
		str_slice = initstr[:i].decode('utf-8', 'ignore').encode('utf-8', 'ignore')
		text_layer = create_text_layer(img, fg_color, str_slice, font, font_size, draw_on=backdrop)
		pdb.gimp_floating_sel_anchor(text_layer)
		#pdb.gimp_progress_update(float(abs(i-len(initstr)))/len(initstr)) # delaying the whole process? o.O
		pdb.gimp_progress_pulse()

	create_background_layer(img, bg_color, make_transparent=transparent_background)

	gimp.Display(img)

register(
	"python_fu_typewrite",
	"Automated Typing Animation",
	"Create an image sequence that animates typewriter typing",
	"Prafulla Giri", "Prafulla Giri", "2018",
	"Typewrite...",
	"",
	[
		(PF_TEXT, "initstr", "Text", "GNU\nImage\nManipulation\nProgram"),
		(PF_FONT, "font", "Font-Face", "Sans"),
		(PF_SPINNER, "font_size", "Font Size", 50, (1, 3000, 1)),
		(PF_BOOL, "transparent_background", "Transparent Background?", True),
		(PF_COLOR, "fg_color", "Foreground Color", (1.0, 1.0, 1.0)),
		(PF_COLOR, "bg_color", "Background Color", (0.0, 0.0, 0.0))
	],
	[],
	typewrite,
	menu="<Image>/Filters/Custom"
)

main()
