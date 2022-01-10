# How to do a Crowdfunding Roundup

This process is about as automated as it can be, I think. I've skinned the cat various ways over the years and settled on this approach. Perhaps you prefer a slightly different approach, perhaps you are more clever than I and can think of ways to automate further. But the fundamental hurdle to streamlining automation is that people are messy and so Kickstarter/Kicktraq data are messy. The Tabletop Games category contains most - but not all - items you'd want to consider for a post and most - but not all - items you'd want to consider are in the Tabletop Games category. Automating retrieval of project data is relatively easy. The part that needs human intervention is either sorting out bs if you do a bulk import, or choosing projects if you do a selective import.

Anyway, here's my full process. It takes ~2-3 hours once a week, depending on how many games get released on Kickstarter in that time.

## Step 1 - Retrieve and store game info from Kicktraq
First I pull in the latest data from Kicktraq using the `processKicktraq()` function and supplying my private key, storying this in a variable. Let's pretend this is called `gameData`.

Then I use the `addToAirtable(gameData, atApiKey)` to view projects one day at a time and choosing which to add to my AirTable database. By default it looks at everything added yesterday, but by including an additional parameter (`today() - 2` for example) data from any date can be mined. (You could have it just pull everything in automatically and then trim the fat out of whatever database later. I did it this way so I could make it part of a daily routine to add new stuff to the database.)

One other quirk: you have to add games in batches of no more than 10 at a time because of the way AirTable's API works. This is only ever a problem on Tuesdays which has become the Unofficial Publish Your Kickstarter Project day in the board game world. Other than that, there's seldom more than 10 projects of interest on any given day.

### Step 1.5 - Add games from other services (optional)
The biggest reason why I've been reluctant to make adding games from other services - IndieGogo, GameFound, etc - a regular thing is that they suppress the start date of projects. (Kickstarter does, too, now, but via Kicktraq I can tell.) Knowing the start and end dates of a project is very important for making sure things end up on the right lists during the right week. GameFound isn't so bad as it's dedicated to board game campaigns and there generally isn't more than 1 or 2 new ones a week. But, I have yet to automate the process of retrieving and updating that data.

## Step 2 - Add details manually
There are certain details I've found difficult enough to automate that I just do it manually. This is what takes the most time and time investment definitely scales with the quantity of new projects. I tend to just put on some music and power through it.

I look at every project that's been added recently and I manually open each Kickstarter page to add the following:

 - Tags, such as `#bling`, `#newedition`, `#lolwut`, and `#take2` - The only consistent way I've found to discover this info is quickly skimming the project page and the creator page.
 - Price - I wish there was an easy, consistent way to tell what the lowest reward level is that gets you something of value but this just takes skimming and deciding. Average pledge gets picked up automatically in the data mining step, though, so that's something.
 - Number of players - If there's a piece of info that's consistently difficult to find, it's this. Well-run campaigns from professionals tend to state it clearly, but sometimes even professional-looking projects will make you really hunt for it or forget to include it at all.
 - BoardGameGeek link - These don't exist for every game, but I check the Kickstarter geeklist for the appropriate period ([this](https://boardgamegeek.com/geeklist/293749/2022-kickstarter-and-other-crowdfunded-boardgame-p) is the latest at this time of writing) and copy-paste in the links. (This is also sometimes useful for double-checking if I've missed anything and helping to fill in other missing info)
 
## Step 3 - Update database
I pull in the latest data once again with `processKicktraq()` and then use `updateAirtable()` to make sure everything in the database is as up-to-date as can be
 
## Step 4 - Create the post text
Luckily, this is completely automated. Run the command to create the file and it just works.

## Step 5 - Prep the title
This is something that could definitely be automated, but it takes maybe 5 minutes so I never have. I do a quick count of the number of projects in each of the lists and pick a notable project from each to be included in the post title.

## Step 6 - Post
That's it, done!
