# Line Length

You should format source code so that no line is longer than 80 characters.

Old text terminals were standardised on 80 columns which they in turn inherited from even older punch card technology. While modern computer screens support vastly more than this, there are a couple of considerations to keep in mind that motivate us to continue supporting an 80 character limit:

* Displaying code in web pages, paste-bins, gist services, etc., is much cleaner and easier to read when the character width is limited to 80 characters.
* Most modern text editors allow for multiple panes, allowing several files to be open side-by-side, supporting the easy editing and referencing of multiple files simultaneously; limiting these files to 80 characters in width facilitates this type of workflow.
* Code that has to be examined under emergency circumstances (such as via a terminal attached to a crash cart in a data centre, or in an emergency shell session without a graphical window manager) is much easier to read quickly when character width is limited to 80.
* Lastly, such a convention encourages good naming discipline!
