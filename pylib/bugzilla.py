import os
from ConfigParser import RawConfigParser
from Pymacs import lisp
from bugz.bugzilla import Bugz

def get_bug(bug_id):
    root = bugzObj.get(bug_id).getroot()
    bugelem = root.find('bug')
    return [[lisp[attr], bugelem.find(attr).text]
             for attr in ('bug_id', 'bug_status', 'short_desc', 'priority', 'bug_severity', 'component')]

def _load_config():
    parser = RawConfigParser()
    parser.read(os.path.expanduser('~/.bugzilla'))
    return dict(parser.items('main'))

config = _load_config()
bugzObj = Bugz(config['url'], config['user'], config['password'])

