# JSON comments for automation

Documentation: [JSON comments for automation](https://github.com/gbif/backbone-feedback-automation/blob/master/JSON-comments-for-automation.md)

### Introduction

In this document we describe our **backbone feedback automation** system. This is an ongoing effort to describe taxonomic issues in a structured way that can be machine checked. We now describe requests for correction to the [Catalogue of Life](https://www.gbif.org/dataset/7ddf754f-d193-4cc9-b351-99906754a03b) using assertions written a `JSON` comment.

> Note, that just because we have written a `JSON` description of the issue, doesn't mean we described it in the right way or that the original comment is correct. The structured `JSON` comments merely exist to describe the issue in a clear, machine-readable way.

Currently, we using this system to describe issues in two repositories:
1. https://github.com/CatalogueOfLife/data/issues
2. https://github.com/gbif/backbone-feedback/issues

The code that actually does the checking and label generation is found here:
https://github.com/gbif/backbone-feedback-automation

A GitHub actions script now periodically scans the issues to see if the changes have been applied to the COL. The issue will then get one of the following labels:

* **autocheck - issue open on COL** : The issue is assessed to be open based on the JSON comment
* **autocheck - issue closed on COL** : The issue is assessed to be closed based on the JSON comment
* **autocheck - issue unclear on COL** : There is some error. Usually this means there is a bug in the autocheck script or the name doesn't exist.

### JSON comment working options

Names are always compared using the full **scientific authorship**, so when constructing your `JSON` assertions, always use the canonical name and authorship.

#### Name Change

The name should be changed or there is some misspelling. The issue is considered closed if the `currentName` is either not found in the COL or is properly synonymized with the `proposedName`.

```json
// json for auto-checking
{
"currentName": "",
"proposedName": ""
}
```
[why is this here?](https://github.com/gbif/backbone-feedback-automation/blob/master/JSON-comments-for-automation.md)

#### bad name

Name should not exist at all. Should only be used if the name should not be a synonym or combination. For example, some non-sense name or character encoding issue could be considered a "bad name". Issue is considered closed if it does not appear in the COL.

```json
// json for auto-checking
{
"badName": ""
}
```
[why is this here?](https://github.com/gbif/backbone-feedback-automation/blob/master/JSON-comments-for-automation.md)

#### missing name

Name should exist. Issue is considered closed if the name appears in the COL.

```json
// json for auto-checking
{
"missingName": ""
}
```
[why is this here?](https://github.com/gbif/backbone-feedback-automation/blob/master/JSON-comments-for-automation.md)

#### rank change

`name` here refers to the focal name which has the wrong rank. This one can be difficult because the focal name can different depending on the rank, so this might need to modified in the future.

```json
// json for auto-checking
{
"name": "",
"wrongRank": "",
"rightRank": null
}
```
[why is this here?](https://github.com/gbif/backbone-feedback-automation/blob/master/JSON-comments-for-automation.md)

#### wrong group

The `name` is in the wrong higher taxonomic group. If the name doesn't appear in the COL, it will get the label **autocheck - issue unclear on COL**. Issue is considered closed if `name` either **does not** appear in `wrongGroup` or has `rightGroup` in its higher classification.

```json
// json for auto-checking
{
"name": "",
"wrongGroup": "",
"rightGroup": null
}
```
[why is this here?](https://github.com/gbif/backbone-feedback-automation/blob/master/JSON-comments-for-automation.md)

### synonym issues

The structure below is used synonymy issues. `wrongParent` is the accepted name that the `name` should **not** be tied to. `rightParent` is accepted name that the `name` should be linked to. Similarly, `rightStatus` and `wrongStatus` describes the status of the `name`.

COL uses several variations for **status**: "provisionally accepted", "accepted order", "accepted genus", "synonym species", "ambiguous synonym" ... But the auto-check script ignores this nuance and rather normalizes these values to "synonym" and "accepted", so as to not over-burden the `JSON` comment annotator. It is ok to use `"rightStatus": "accepted order"` and other similar variations, but these (for now) are treated equivalently by the auto-checking script.

```json
// json for auto-checking
{
"name": "",
"wrongParent": null,
"rightParent" : null,
"wrongStatus": "SYNONYM",
"rightStatus": "ACCEPTED"
}
```
[why is this here?](https://github.com/gbif/backbone-feedback-automation/blob/master/JSON-comments-for-automation.md)

### basionym issues

```json
// json for auto-checking
{
"name": "",
"wrongBasionym": "",
"rightBasionym": ""
}
```
[why is this here?](https://github.com/gbif/backbone-feedback-automation/blob/master/JSON-comments-for-automation.md)

#### ARRAY

If multiple proposed changes are found in one issue, one can use a `JSON` array.

```json
// json for auto-checking
[
{
"currentName": "Paradohertya triphasia Bethune-Baker, 1904",
"proposedName": "Paradohertya trifascia Bethune-Baker, 1904"
},
{
"name": "Alphocoris spec Linnavuori, 1984",
"wrongGroup": "Plumbaginaceae",
"rightGroup": null
}
]
```
[why is this here?](https://github.com/gbif/backbone-feedback-automation/blob/master/JSON-comments-for-automation.md)

### Tips for creating good JSON comments

* When information is not known, use `null`. For example, if the user didn't supply the `rightGroup` or `rightParent` or `rightStatus`, just fill in `null` for those values.
* When getting "unclear" label on an issue because of a missing name, it is usually a good a idea to also add a `missingName` assertion as well. Then the issue will resolve to "open", and it is more explicit about what actually needs to be fixed.
* Better to over-describe an issue than to under-describe, so if in doubt add more assertions to the `JSON ARRAY`.
* Most taxonomic issues can be described using one of or a combination of patterns listed above, but occasionally there can be complicated issues that cannot be described.
* Use the full scientific name with authorship.
* Put the examples, in your GitHub saved replies to quickly generate the correct template.
* Always include the "Why is this here?" link, so that other users know why that comment exists.
