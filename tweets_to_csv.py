import csv
import re


def parse_line(raw_line):
    # remove json in the middle before splitting
    if '\t' in raw_line:  # if line does not contain tab, it means there was a tweet text with \n.
        raw_line = re.sub(r'{[^}]*}', '', raw_line)
    
    fields = raw_line.split('\t')

    if len(fields) != 17:
        return None
    
    text = fields[0].replace('|', ',')  # replace | just in case because of delimiter in csv
    tweet_id = fields[3]
    username = fields[7].replace('|', ',')
    geo_availability = fields[9]
    coordinates = fields[10] + ' ' + fields[11]
    timestamp = fields[16]
    #print(tweet_id, username, timestamp, text, geo_availability, coordinates)
    #print('\n')
    return tweet_id, username, timestamp, text, geo_availability, coordinates


def parse_file(input_file, output_file):
    with open(input_file, 'r') as input:
        data = input.read()
        lines = data.split('\t1\n')  # if line ends with number 1, it's the end of the tweet (some tweets have newlines in them)

    with open(output_file, 'w') as output:
        csv_writer = csv.writer(output, delimiter='|', escapechar='\\')
        header = ('id', 'username', 'timestamp', 'text', 'geo_available', 'coordinates')
        csv_writer.writerow(header)
        
        for line in lines:
            processed = parse_line(line)
            if processed:
                csv_writer.writerow(processed)


def main():
    parse_file('../data/twitter/pfeffer_2015-06-2015-07_2023-09-11_1957/2015-07/2015-07-31.txt', '../data/tweets/all/31_07-2015.csv')


if __name__ == '__main__':
    main()
