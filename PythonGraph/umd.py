# Ryan Forsyth
# 04/13/2015

import json
import re
from urllib.request import urlopen
from urllib.error import HTTPError
from graph import Graph

def pre_reqs_raw(course):
    """
    Get the raw pre-requisite data
    
    Argument:
    
    course --
    String
    The course dept and number i.e. CMSC330
    
    Return:
    String
    The prerequisites
    """
    
    link = "http://api.umd.io/v0/courses/" + course
    json_data = urlopen(link).read().decode('utf-8')
    data = json.loads(json_data)
    return data['relationships']['prereqs']
    

def pre_reqs(course):
    """
    Get the cleaned up pre-requisite data
    
    Argument:
    
    course --
    String
    The course dept and number i.e. CMSC330
    
    Return:
    List of Strings
    The list of courses that are pre-requisites for this course
    """
    
    try:
        raw_string = pre_reqs_raw(course)
        raw = raw_string[0]
        words = re.split('\s', raw)          # Split on white space
        requirements = [w for w in words if re.match('[A-Z]{4}[0-9]{3}', w)]
        length = len(requirements)
        if length > 0:
            for i in range(0, length):
                requirements[i] = requirements[i][:7]
        return requirements      
    except IndexError:
        return ["No Course"]
   
    
def pre_reqs_tree(course, g = None, visited=[]):
    """
    Create a pre-requisite tree for this course.
    
    Arguments:
    
    course --
    String
    The course dept and number i.e. CMSC330
    
    g --
    Graph
    The graph
    
    visited --
    List of Strings
    The list of visited courses
    
    Return:
    The graph that represents the pre-requisite tree
    """
    
    if g == None:
        g = Graph([[0]], [course])
        
    requirements = pre_reqs(course)
    
    for requirement in requirements:
        if requirement != "No Course":
            if requirement not in g.vertex_names:
                g.insert(requirement) # Add a new vertex
                
            g.insert(course, [requirement])     # Make an edge
            
            if requirement not in visited:
                visited.append(requirement)
                pre_reqs_tree(requirement, g, visited)  # Recursively draw the tree for each
    
    return g

def pre_reqs_graph(department):
    """
    Creates a pre-requisite graph of for all 400 level courses in a department
    
    Argument:
    
    department --
    The department being considered
    
    Return:
    The graph of pre-requisite courses
    """
    
    g = Graph()
    for i in range(0, 99):
        course = department
        if i in range(0, 9):
            course += "40"
        else:
            course += "4"
        course += str(i)
            
        try:
            g = pre_reqs_tree(course, g)
        except HTTPError:
            # Do nothing, go back to top of loop
            pass
    return g
    
