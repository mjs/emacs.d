import os
from ConfigParser import RawConfigParser
from Pymacs import lisp
from bugz.bugzilla import Bugz

def create_bug(product, component, title, description, assigned_to, priority, severity):
    result = bugzObj.post(product, component, title, description,
                          assigned_to=assigned_to,
                          priority=priority,
                          severity=severity)
    if result == 0:
        raise IOError('bug creation failed')
    return result
    

def get_bug(bug_id):
    root = bugzObj.get(bug_id).getroot()
    bugelem = root.find('bug')
    return [[lisp[attr], bugelem.find(attr).text]
             for attr in ('bug_id', 'bug_status', 'short_desc', 'priority', 'bug_severity', 'component')]


def close_bug(bug_id, status='RESOLVED', resolution='FIXED'):
    bugzObj.modify(bug_id, status=status, resolution=resolution)


def _load_config():
    parser = RawConfigParser()
    parser.read(os.path.expanduser('~/.bugzilla'))
    return dict(parser.items('main'))

#needs to not initialise if config file is missing
#config = _load_config()
#bugzObj = Bugz(config['url'], config['user'], config['password'])

