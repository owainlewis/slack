## Python script to generate boilerplate

METHODS = [ "api.test"
          , "auth.test"
          , "channels.archive"
          , "channels.create"
          , "channels.history"
          , "channels.info"
          , "channels.invite"
          , "channels.join"
          , "channels.kick"
          , "channels.leave"
          , "channels.list"
          , "channels.mark"
          , "channels.rename"
          , "channels.setPurpose"
          , "channels.setTopic"
          , "channels.unarchive"
          , "chat.delete"
          , "chat.postMessage"
          , "chat.update"
          , "emoji.list"
          , "files.delete"
          , "files.info"
          , "files.list"
          , "files.upload"
          , "groups.archive"
          , "groups.close"
          , "groups.create"
          , "groups.createChild"
          , "groups.history"
          , "groups.invite"
          , "groups.kick"
          , "groups.leave"
          , "groups.list"
          , "groups.mark"
          , "groups.open"
          , "groups.rename"
          , "groups.setPurpose"
          , "groups.setTopic"
          , "groups.unarchive"
          , "im.close"
          , "im.history"
          , "im.list"
          , "im.mark"
          , "im.open"
          , "oauth.access"
          , "rtm.start"
          , "search.all"
          , "search.files"
          , "search.messages"
          , "stars.list"
          , "team.accessLogs"
          , "user.getPresence"
          , "user.info"
          , "user.list"
          , "user.setActive"
          , "users.setPresence"]


TEMPLATE = """%s :: Token -> RequestParams -> IO SlackResponse
%s token params = request token "%s" params
"""

def uppercase_first_char(s):
    """ Uppercase the first letter of a string """
    return ''.join(s[0].upper() + s[1:])

def haskell_name(m):
    parts = m.split('.')
    fst = parts[0]
    rst = [uppercase_first_char(x) for x in parts[1:]]
    return ''.join([fst] + rst)

def main():
    for m in METHODS:
        hn = haskell_name(m)
        print(TEMPLATE % (hn, hn, m))

if __name__ == '__main__':
    main()
