# W. Zenk-Möltgen, 2017-03-01
# argument is a zip file with text files in it with tweed ids (one per line)
# output is a text file with number of found tweets per county

# uses oauth 1 (user authentication) instead of oauth 2 (application authentication) - quicker
# uses zip file as argument instead of single text file

import sys
import time
import os
from twython import Twython
import json
import zipfile
from math import ceil



# set these variables according to your directory structure
parentpath='/home/mysa/phd/pollution/sentiment/data/gesis/rehydrate'
directory = '../data/gesis/state_id_2015-07/'
logfilepath_info=parentpath+'/rehydrateScript_log.txt'

#delete old log
open(logfilepath_info, 'w')
#write header to log
today = time.strftime('%Y-%m-%d %H:%M:%S')
message = "Logging of rehydrateScript started " + today + "\n"
sys.stdout.write(" " + message)
open(logfilepath_info, 'a').write(message)
message = "file, ratio, found, total \n" 
sys.stdout.write(" " + message)
open(logfilepath_info, 'a').write(message)

def get_filepaths(directory):
    """
    This function will generate the file names in a directory 
    tree by walking the tree either top-down or bottom-up. For each 
    directory in the tree rooted at directory top (including top itself), 
    it yields a 3-tuple (dirpath, dirnames, filenames).
    """
    file_paths = []  # List which will store all of the full filepaths.

    # Walk the tree.
    for root, directories, files in os.walk(directory):
        for filename in files:
            # Join the two strings in order to form the full filepath.
            filepath = os.path.join(root, filename)
            file_paths.append(filepath)  # Add it to the list.

    return file_paths  # Self-explanatory.

def getTweets(zipfilepath):
	# uncomment this to show the path 
	#print(zipfilepath)	
	
	# set input and output file
	parentpath='/home/mysa/phd/pollution/sentiment/data/gesis'
	targetlist=zipfilepath  #sys.argv[1] #text file listing feeds to query, one per line. full path ok.
	today = time.strftime("%Y%m%d")
	logfilepath_info=parentpath+'/rehydrateScript_log.txt'

	#if you want to delete the logfile first, uncomment this 
	#if os.path.exists(logfilepath_info): 
		#os.remove(logfilepath_info)


	#authenticate: enter your keys registered with Twitter
	APP_KEY='Kv2FRFAeuPwOMx3jDb5ISDTOc' #25 alphanumeric characters
	APP_SECRET='5U3OLqfoDPqvVMcPZaUJwR6Uhlm5sI1ks7nlAkMmEjjGc2HPqY'
	ACCESS_TOKEN='1673976033748418560-CCZoNi42mm2zbu4R0fbXSTkpSoQIk2'
	OAUTH_TOKEN='1673976033748418560-CCZoNi42mm2zbu4R0fbXSTkpSoQIk2'
	OAUTH_TOKEN_SECRET='j7lXDO5XhFOOKJxz4rf2rkeIvMLONxzC2Oi08YAcRPMeA'

	try:
	#if True:
		#twitter=Twython(APP_KEY,access_token=ACCESS_TOKEN) 					#Application auth
		twitter = Twython(APP_KEY, APP_SECRET, OAUTH_TOKEN, OAUTH_TOKEN_SECRET) #User auth

		i=0
		with zipfile.ZipFile(targetlist) as z:
			if zipfilepath.endswith('.zip'):
			#if zipfilepath == './county_id_2014-06/county_id_2014-06-01.zip' and zipfilepath.endswith('.zip'):
			#	open(logfilepath_info, 'a').write(zipfilepath + '\n')

				for filename in z.namelist():
					if not os.path.isdir(filename):
						# if you want to read the file, uncomment this
						#with z.open(filename) as f:
							#for line in f:
								#print line

						if filename.endswith('.txt'):
						#if zipfilepath != './county_id_2014-06/county_id_2014-06-20.zip' and filename.endswith('_ID.txt'):
							#print(filename)
							i+=1
							if False: 	# i >= 2: 
								break	# for testing, early exit possible
							else:
								#read tweet ids from text file per line 
								handles = [line.rstrip() for line in z.open(filename)] 

								sys.stdout.write('\r')

								statuses=[] #initialize data object
								hl=len(handles)
								cycles=ceil(hl/100)
								for i in range(0, cycles): ## iterate through all tweets 
								    h=handles[0:100]
								    del handles[0:100]
								    incremental = twitter.lookup_status(id=h) # each call gets 100 tweets 
								    statuses.extend(incremental)
								    drawProgressBar(i/cycles, 40)
								    # 1 second rest between api calls. API allows 900 calls/15 min. (user auth)
								    time.sleep(1) # 1 second for user auth
								    #time.sleep(4) # 4 seconds for app auth  (300/15min.)

								drawProgressBar(1, 40)

								for i, val in enumerate(statuses):
									myid=statuses[i]['id_str']
									mytext=statuses[i]['text'].replace('"', r'&quot;')
									#if you want to output the tweet text: create a text of some tweet attributes, uncomment this
									myline=str(i) + ', ' + json.dumps(myid)[0:40] + ', "' + mytext + '"\n'
									f.write(myline) 
									(json.dumps(mytext, indent=5))
									drawProgressBar(i/100, 40)

								drawProgressBar(1, 40)
								
								# this is the result
								message = "" + filename + ", " + str(i/hl) + ", " + str(i) + ", " + str(hl) + " \n"
								sys.stdout.write(" " + message)

								#append to log
								open(logfilepath_info, 'a').write(message)





	#except ValueError: # catch this exception
	except: # catch *all* exceptions
	#if False:
		e = sys.exc_info()[0]
		ed = sys.exc_info()[1]
		#append error to log
		open(logfilepath_info, 'a').write("Error: %s \n" % e)
		open(logfilepath_info, 'a').write("ErrorDescription: %s \n" % ed)

def drawProgressBar(percent, barLen = 20):
    # draw a progress bar 
    sys.stdout.write("\r")
    progress = ""
    for i in range(barLen):
        if i < int(barLen * percent):
            progress += "="
        else:
            progress += " "
    sys.stdout.write("[ %s ] %.2f%%" % (progress, percent * 100))
    sys.stdout.flush()

# Run the above function and store its results in a variable.   
full_file_paths = get_filepaths(directory)

# use the path and loop through all zip-files in it 
#for filename in full_file_paths:
#    if filename.endswith(".zip"): 
#        print(os.path.join(directory, filename))
        
#        getTweets(filename)
        # 1 second rest between api calls. API allows 900 calls/15 min.
        # time.sleep(1)
#        continue
#    else:
#        continue

getTweets('/home/mysa/phd/pollution/sentiment/data/gesis/state_id_2015-07/state_id_2015-07-05.zip')

today = time.strftime('%Y-%m-%d %H:%M:%S')
message = "Logging of rehydrateScript finished " + today + "\n"
sys.stdout.write(" " + message)
open(logfilepath_info, 'a').write(message)



