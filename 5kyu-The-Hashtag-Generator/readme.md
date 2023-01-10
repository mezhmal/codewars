https://www.codewars.com/kata/52449b062fb80683ec000024/

_5 kyu_

# The Hashtag Generator

The marketing team is spending way too much time typing in hashtags.
Let's help them with our own Hashtag Generator!

Here's the deal:

- It must start with a hashtag (#).
- All words must have their first letter capitalized.
- If the final result is longer than 140 chars it must return false (actually empty string for Scala tests).
- If the input or the result is an empty string it must return false (actually empty string for Scala tests).

Examples

```
" Hello there thanks for trying my Kata"  =>  "#HelloThereThanksForTryingMyKata"
"    Hello     World   "                  =>  "#HelloWorld"
""                                        =>  false  (actually "" for Scala tests)
```
