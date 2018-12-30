Autonomic is a tool for automating the administration of your organisation. The formalism allows setting policies that define the operating logic of your organisation, including mechanisms for the change of these policies.

This repo is set up with the set of policies for beginning a game of [Nomic](https://github.com/autonomic#why-is-nomic). The interface is implemented as a [Slack](www.slack.com) application, that you interact with by the use of [slash commands](www.slack.com/api/slach_commands) from a Slack workspace shared by your organisations members.

contents
-

1. [Usage demo](www.github.com/rskew/autonomic#usage-demo)
2. [How to set up](www.github.com/rskew/autonomic#how-to-set-up)
3. [Why is Nomic](www.github.com/rskew/autonomic#why-is-nomic)
4. [Prolog is cool](www.github.com/rskew/autonomic#prolog-is-cool)
5. [Ideas for extensions](www.github.com/rskew/autonomic#ideas-for-extensions)


Usage Demo
--

+pics of slash commands and their responses in the slack application+



How to set up
-

You can then add this application to your Slack workspace through the following steps:

- In the 'workspace configuration' view, add an application and call it 'autonomic', or watever you want its posts to be labelled as.
- Grab the auth token and save in a file called `auth.token` in the repo's top level directory.
- Run the application on your server. You can do this with the command `swipl autonomic.pl -p 80833`, once you have [SWI Prolog](www.swiprolog.com) (an open source Prolog) installed. If you want to quickly test it out, [ngrok](www.ngrok.com) allows you to avoid the hassle of setting up a webserver and exposing ports.
- Test it out! Type one of autonomic's slash commands in any channel of your Slack workspace.


Why is Nomic
-

Nomic is a game(?) where the rules of the game dictate what you can do in your turn, and your turn involves changing the rules of the game.

Because of its self-modifying nature, a game of Nomic can develop into anything. You could add a rule that change the way people can behave while playing, you could add rules that envelop other games into Nomic, and you can go full Jumanji and rule that the game expand to include other parts of life.

Here are some links about Nomic:
- http://legacy.earlham.edu/~peters/nomic.htm

The [initial set of rules](www.github.com/rskew/autonomic/rules/initial_rules.pl) involve the basics for a running system. Take the time to read them, and you'll see that it's quite a blank slate.



Prolog is cool
-

Prolog is chosen as the implementation language.

- It's a lot of fun to write Prolog code :)
- Prolog is a made for rule-based reasoning systems. It's more natural to express self-referential rule-based systems in Prolog. It's also evocative of ways to extend the system to mediate more user behaviour.



Ideas for extensions
-

Full ontological machine-interpretable rules
  - RDF representation of rules
  - proposals require a structured format with declarations for any new ontological concepts
  - partial automatic parsing
    - prompt the user for resolution of ambiguous or unknown concepts
  - certain game behaviour can be checked against the rules automatically
    - e.g. market transactions in a pun/meme economy
  - rule base can be used to help solve problems
    - rules can encode procedures e.g. which vendors to use to source memes.
