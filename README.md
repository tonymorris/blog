# thegapchessclub.org.au

### Making website changes

* Edit files ending in `.html` or `.md`
* These files can be found in:
  * the base directory
  * the `people` directory
  * the `snippets` directory
  * the `templates` directory
  * the `posts` directory
* Blog posts should go in the `posts` directory.
* To upload any other file to the website, put it in the `share` directory.
  The file will then be available at `https://thegapchessclub.org.au/share/<your-file>`
* After making changes, about 6 minutes later the changes should be live on the website.

### Build and run locally

* `stack install --only-dependencies`
* `stack setup`
* `stack build`
* `stack exec site watch`
