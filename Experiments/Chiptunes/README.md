This outputs 8kHz, 8bit pcm data.
For Ubuntu and most other modern Linux:

main | padsp tee | aplay

For older Unix you might be able to pipe into /dev/audio

Windows & Mac users will need to get creative, like
writing to a file and sticking on a WAV header.

