# Use Cases

## Patching

See [MXPF](https://github.com/matortheeternal/mxpf) for a general idea of how I want to support patching for xedit-lib.  Below is a step-by-step process with javascript code snippets to demonstrate how patching would be done.

### Step 1
Specify what files you're patching.

```javascript
  fileNames = GetUserFileSelection();
```

### Step 2 [Optional]
Specify a destination file for patched records, or create a new one.

```javascript
  patchFile = FindOrCreateFileByName('MyPatch.esp');
```

### Step 3
Load the records you want to patch and any other needed records into arrays.

```javascript
  records = LoadRecords(fileNames, 'ARMO');
```
  
### Step 4 [Optional]
Filter records. 

```javascript
  filteredRecords = records.filter(function(record) {
    return GetIntValue(record, 'Some\\Value') > 1;
  });
```
  
### Step 5 [Optional]
Copy records to patch file.

```javascript
  CopyRecordsToFile(records, patchFile);
```
  
### Step 6 [Optional]
Create supplementary records.

```javascript
  recordData.forEach(function(recordData) {
    rec = NewRecord(patchFile, 'GLOB');
    ApplyJsonToRecord(rec, recordData);
  });
```
  
### Step 7 
Patch the records.

```javascript
  patchRecords.forEach(function(patchRecord) {
    SetValue(patchRecord, 'Path\\To\\Something', 'SomeValue');
  });
```
      
## Automation
See [Automation Tools](http://www.nexusmods.com/skyrim/mods/49373/) for an idea of what sort of tools I'd like to build/promote building with xedit-lib.

- Select records and set values on them
- Set values on records that meet a condition
- Set values relative to other values
- Output values to multiple formats (JSON,XML,CSV,YAML,TXT)
  
## Checking for and fixing errors

- Allow users to specify what plugins they want to fix errors for
- Find all errors in the plugins, storing the elements the errors occur on
  in an array.
- Iterate through the array, displaying the errors and possible resolution 
  options in an easy-to-use GUI.
- Allow for custom resolution.
  
## Building new mods
In order to do this we'll need to build a fully-functional GUI application equivalent to or better than TES5Edit's GUI.