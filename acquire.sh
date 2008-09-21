#!/bin/bash


################################################################################
# A shell script to collect colour data from a webcam over time
################################################################################


################################################################################
# Configuration
################################################################################

GRABBED_IMAGE="shot.jpg" # Set by camgrab, don't change

GRAB_COUNT=25 # Number of times to grab, first is immediate
GRAB_SLEEP=$((60*60)) # Delay between each grab, in seconds

PALETTE_DEPTH=5 # The maximum number of colours to a palette. Less may be found.

OUTPUT_FILE="data-`date +%s`.sexps"


################################################################################
# Functions
################################################################################

# Grab the image currently visible to the webcam
# Camgrab grabs a smaller image but has better colour

function grab_frame
{
    camgrab
}


function create_output_file
{
    echo -n "" > ${OUTPUT_FILE}
}


# Get the average colour for the entire image

function colour_average
{
    grab_frame
    # Format as a list of lists, all on one line
    # This makes it easier to read in for processing
    # Even though each list consists of a single element
    echo -n "(" >> ${OUTPUT_FILE}
    convert "${GRABBED_IMAGE}" -scale 1x1\! -format '%[pixel:u]' info:- \
	| perl -ple 's/rgb\(([^%]+)%,([^%]+)%,([^%]+)%\)/\(\1 \2 \3)/' \
	>> ${OUTPUT_FILE}
    
    echo -n ")" >> ${OUTPUT_FILE}
}


# Get the palette for the image
# There may be fewer colours if e.g. the image is all white or black
# This is actually what we want for the project

function colour_palette
{
    grab_frame
    # Format as a list of lists, all on one line
    # This makes it easier to read in for processing
    echo -n "(" >> ${OUTPUT_FILE}
    convert "${GRABBED_IMAGE}" -format %c -colors 5 histogram:info:- \
	| perl -pl0e 's/.*rgb\(([^%]+)%,([^%]+)%,([^%]+)%\)/\(\1 \2 \3)/' \
	>> ${OUTPUT_FILE}
    echo ")" >> ${OUTPUT_FILE}
}


################################################################################
# Main flow of control
################################################################################

echo Creating output file.
create_output_file
echo Created.
echo Looping, grabbing frames.
# Then do in a loop with a sleep
for ((i = 0; i < ${GRAB_COUNT}; i += 1)); do
    echo Grabbing frame.
    #colour_average
    colour_palette
    echo `date`, grabbing next frame in ${GRAB_SLEEP} seconds.
    sleep ${GRAB_SLEEP}
done
echo Done.