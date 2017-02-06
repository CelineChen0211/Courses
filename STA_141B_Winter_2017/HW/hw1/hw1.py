#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Tue Jan 17 10:44:15 2017

@author: CJ
"""

def read_birthdays(file_path):
    """Read the contents of the birthdays file into a string.
    
    Arguments:
        file_path (string): The path to the birthdays file.
        
    Returns:
        string: The contents of the birthdays file.
    """
    with open(file_path) as file:
        return file.read()
        
text = read_birthdays('/Users/CJ/Dropbox/Course Winter 2017/STA 141B/HW/hw1/birthdays.txt')

#==============================================================================
# Extract the number of births in the United States for each day in 1978
#==============================================================================

# generate a list with each line of the text (empty lines removed)
text_lines = [elem for elem in text.split('\n') if elem != '']
# check the starting and ending points of the list where birthday information exists
text_lines = text_lines[4:369]

# generate a list with date and counts
date_num = [elm.split('\t') for elm in text_lines]

            
def get_birth_info(list): 
    # define a function to extract the date and counts in the following format
    #   [month, day, year, count] and every element should be integer
    
    ans = [int(s) for s in list[0].split('/')]
    ans.append(int(list[1]))
    return ans

result = [get_birth_info(elem) for elem in date_num]

