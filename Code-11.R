
#file-->new file-->R markdown-->template-->flex
#To see this output we need to "Knit" the file by either clicking on the 
#Knit icon near the top of our file or pressing Ctrl+Shift+K.

#We could have a one-page dashboard but that would mean cramming a lot of 
#information in a small space. Flexdashboard gives us an option for having 
#multiple pages on our dashboard by demarcating sections in the code using the === header

#Now within a page, the layout orientation is based on rows or columns (or both). 
#A new row or column can be added using the ---header or ##.

#In R markdown working code needs to be encapsulated by the delimiters ```r{} and ``` 
#also called the code chunk delimiters. The way R markdown works is it will embed the 
#results at the end of the code chunk onto the HTML dashboard output.

#ValueBox: Sometimes you want to include one or more simple values within a dashboard. 
#You can use the valueBox function to display single values along with a title and 
#optional icon.
#You can specify icons from three different icon sets:
#Font Awesome
#Ionicons
#Bootstrap Glyphicons
#When referring to an icon you should use it's full name including the icon set 
#prefix (e.g. "fa-github", "ion-social-twitter", "glyphicon-time", etc.).

#Linking
#If a valueBox displays a value for which additional detail is available on another 
#dashboard page you can enable navigation to the other page via the href parameter.

#Gauges: Gauges display values on a meter within a specified range.
