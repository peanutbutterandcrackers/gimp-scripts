#!/usr/bin/env python

import string, random
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

def dup_layer(layer, x):
	"""Duplicate a layer x amount of times, at the top of the layer stack"""
	pass
	
# Two modes of flash-up:
# generate random gibberish dup text
# take a really long gibberish string, and with frame, display a slice of it that is equal to the main text's
# length (minux the whitespace chars), with the whitespaces fixed back up, and at the very end of the string
# have the original text

def gen_dup_text(init_text):
	"""Takes a text, and returns a duplicate text with same-lettered text. Whitespaces entact."""
	init_makeup = [ line.split(' ') for line in init_text.split('\n') ]
	dup = [ [] for line in range(len(init_makeup)) ] # will be populated later

	# start duplicating text: line-by-line
	current_line = 1
	for line in init_makeup:
		for word in line:
			#dup[current_line - 1].append(''.join(random.choice(string.lowercase) for i in range(len(word))))
			generated_word = ''
			for char in word:
				if char in string.lowercase:
					generated_word += random.choice(string.lowercase)
				elif char in string.uppercase:
					generated_word += random.choice(string.uppercase)
				elif char in string.digits:
					generated_word += random.choice(string.digits)
				elif char in string.punctuation:
					generated_word += random.choice(string.punctuation)
				else:
					generated_word += random.choice(string.letters)
			dup[current_line - 1].append(generated_word)
					
		current_line += 1
	
	# convert the [ [list], [of, lines] ] dup to string
	for i in range(len(dup)):
		dup[i] = ' '.join(dup[i])
	
	return '\n'.join(dup)
	

def flashup(initstr, font, font_size, transparent_background, fg_color, bg_color, total_flashup_frames):
	gimp.progress_init("Typewriting. Please wait...")

	img = gimp.Image(100, 100, RGB)

	text_layer = create_text_layer(img, fg_color, initstr, font, font_size) 
	img.resize(text_layer.width, text_layer.height, 0, 0)
	backdrop = create_background_layer(img, bg_color, make_transparent=transparent_background)
	merged_layer = pdb.gimp_image_merge_down(img, text_layer, CLIP_TO_IMAGE)

	for i in range(total_flashup_frames):
		backdrop = create_background_layer(img, bg_color, make_transparent=transparent_background)
		str_slice = gen_dup_text(initstr) # var name kept as-is from previous script
		text_layer = create_text_layer(img, fg_color, str_slice, font, font_size, draw_on=backdrop)
		if text_layer.width > img.width:
			img.resize(text_layer.width, text_layer.height, 0, 0)
			pdb.gimp_layer_resize_to_image_size(backdrop)
		pdb.gimp_floating_sel_anchor(text_layer)
		pdb.gimp_progress_pulse()

	create_background_layer(img, bg_color, make_transparent=transparent_background)

	gimp.Display(img)

register(
	"python_fu_flashup",
	"Automated Typing Animation",
	"Create an image sequence that animates flashup typing",
	"Prafulla Giri", "Prafulla Giri", "2018",
	"Flashup...",
	"",
	[
		(PF_TEXT, "initstr", "Text", "GNU\nImage\nManipulation\nProgram"),
		(PF_FONT, "font", "Font-Face", "Mono"),
		(PF_SPINNER, "font_size", "Font Size", 50, (1, 3000, 1)),
		(PF_BOOL, "transparent_background", "Transparent Background?", True),
		(PF_COLOR, "fg_color", "Foreground Color", (1.0, 1.0, 1.0)),
		(PF_COLOR, "bg_color", "Background Color", (0.0, 0.0, 0.0)),
		(PF_INT, "total_flashup_frames", "Number of Flashed Frames", 30)
	],
	[],
	flashup,
	menu="<Image>/Filters/Custom"
)

main()
