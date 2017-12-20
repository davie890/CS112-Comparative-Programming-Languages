#!/afs/cats.ucsc.edu/courses/cmps112-wm/usr/smalltalk/bin/gst -f

"
Davie Truong
dtruong8@ucsc.edu
1525861
"

"Need to read data from the cmd line to determine what operation to do"
"hzip.st -dtcu inputfile [outputfile]"

nl := Character nl.     "Abreviates the newline character"

optionD := false. "Debug information is printed"
optionT := false. "Compression algorithm is activated, decoding tree is printed to standard out"
                  "Output file name may not be specified"
optionC := false. "The input file is compressed"
                  "Written to output file if specified or standard out if not"
optionU := false. "The input file, assumed to be compressed, gets uncompressed"
                  "Written to output file if specified or standard out if not"
outputfilename := stdout.   "Have the outputfile be defaulted to stdout unless later updated"

FileStream extend [
   nl := Character nl.
   catFile [
      |count|
      count := 0.
      [self atEnd not] whileTrue: [
         count := count + 1.
         count asString size to: 5 do: [:skip| stdout << ' '].
         stdout << count << ': ' << self nextLine << nl.
      ]
   ]
]

Character extend [
      isPrint [
            ^ (Character space <= self) & (self <= $~)
      ]
      visible [
            self isPrint ifTrue: [^ self asString]
                         ifFalse: [^ self asInteger printStringRadix: 8]
      ]
]

Object subclass: ZeroArray [
   |theArray|

   ZeroArray class >> new [
      self shouldNotImplement.
   ]

   ZeroArray class >> new: size [
      |result|
      result := super new.
      result init: size.
      ^ result
   ]

   init: size [
      theArray := Array new: size.
   ]

   size [
      ^ theArray size.
   ]

   at: index [
      ^ theArray at: index + 1.
   ]

   at: index put: value [
      ^ theArray at: index + 1 put: value.
   ]

   incr: index [
      (self at: index) isNil
         ifTrue: [ self at: index put: 0].
      self at: index put: (self at: index) + 1.
   ]

   keysAndValuesDo: aBlock [
      (0 to: (self size) - 1) do: [:index |
         aBlock value: index value: (self at: index).
      ]
   ]
]

Object subclass: Leaf [
   |char count isleafNode|
   char [ ^ char ]
   count [ ^ count ]
   isleafNode [^ isleafNode]      "Used to check if it is a leaf node"
   
   Leaf class >> new [
      self shouldNotImplement
   ]

   Leaf class >> new: aChar count: aCount [
      |result|
      result := super new.
      result setChar: aChar andCount: aCount.      
      ^result
   ]

   setChar: aChar andCount: aCount [
      char := aChar.
      count := aCount.
      isleafNode := true.
   ]

   <= other [
      ^ (count < other count)
      | ((count = other count) & (char <= other char))
   ]

   printBase: aStream [
      ^ aStream << self class << '(' << char << ',' << count
   ]

   printOn: aStream [
      (self printBase: aStream) << ')'.
   ]

   inorder: visitor prefix: string [
      visitor value: char value: string.
   ]
   
   "For post order traversal"
   postorder: visitor [
      visitor  value: char value: isleafNode.
   ]
]

Leaf subclass: Tree [
   |left right|

   Tree class >> new: aChar count: aCount [
      self shouldNotImplement
   ]

   Tree class >> new: aChar count: aCount left: aLeft right: aRight [
      |result|
      result := super new: aChar count: aCount.
      result setLeft: aLeft andRight: aRight.
      ^ result
   ]

   setLeft: aLeft andRight: aRight [
      left := aLeft.
      right := aRight.
      isleafNode := false.  "Not a leaf node"
   ]

   left  [ ^ left]
   right [ ^ right]

   printOn: aStream [
      (self printBase: aStream) << ',' << left << ',' << right << ')'.
   ]

   inorder: visitor prefix: string [
      left inorder: visitor prefix: string, '0'.
      right inorder: visitor prefix: string, '1'.
   ]
   
   "Post Order traversal"
   postorder: visitor[      
      left postorder: visitor .
      right postorder: visitor .  
      visitor value: char value:isleafNode.
   ]
]

Object subclass: OutBits [
   |bitIndex currentByte myStream|
   OutBits class >> new [
      self shouldNotImplement.
   ]
   OutBits class >> new: fileStream [
      |result|
      result := super new.
      result init: fileStream.
      ^result
   ]
   clearByte [
      bitIndex := 8.
      currentByte := 0.
   ]
   init: fileStream [
      myStream := fileStream.
      self clearByte.
   ]
   flushByte [
      bitIndex = 8 ifFalse: [
         myStream nextPutByte: currentByte.
         self clearByte.
      ]
   ]
   writeBit: bit [
      currentByte := currentByte bitAt: bitIndex put: bit.
      bitIndex := bitIndex - 1.
      bitIndex = 0 ifTrue: [self flushByte].
   ]
]

Object subclass: BitStream [
   |bitIndex byte myStream|
   BitStream class >> new: fileStream [
      |result|
      result := super new.
      result init: fileStream.
      ^result
   ]
   init: fileStream [
      myStream := fileStream.
      bitIndex := 1.
   ]
   nextBit [
      bitIndex = 1 ifTrue: [
         byte := myStream next.
         bitIndex := 9.
      ].
      bitIndex := bitIndex - 1.
      ^byte value bitAt: bitIndex
   ]
   atEnd [
      ^bitIndex = 1 and: [myStream atEnd]
   ]
]

"Variables"
|inputfile outputfile readfile inputfilename outputfilename|


(Smalltalk arguments size > 1) & (Smalltalk arguments size < 4) & (((Smalltalk getArgv: 1) at: 1) = $-)
ifTrue: 
[   "Check to see if there the user inputted at least 2 of the 3 parameters. Must contain Options and Inputfile name"
   (1 to: Smalltalk arguments size)                      "Goes through the parameters"
      do: [:index |
         element := (Smalltalk arguments at: index).        "Assigns element to the current parameter"
         (element includes: $-)                             "Check to see if the parameter is our options setting"
         ifTrue: 
         [  
            oneOptionFlag := false.                      "Check to see if t,c, or u is enabled"
            (element includes: $d) ifTrue: [ optionD := true. ].
            (element includes: $t) ifTrue: [ optionT := true. oneOptionFlag:= true. ].
            (element includes: $c) ifTrue: [ optionC := true. oneOptionFlag:= true. ].
            (element includes: $u) ifTrue: [ optionU := true. oneOptionFlag:= true. ].

            (oneOptionFlag = false)
            ifTrue:
            [
               stderr << 'Error: Exactly one of the options -t, -c, or -u is required.' << nl.
               ObjectMemory quit: 1
            ].
         ]
         ifFalse:                                        "We are at the inputfile parameter"
         [                                               
            (inputfilename = nil) 
            ifTrue: [inputfilename := element.]                 "Assign the inputfilename if it was empty"
            ifFalse: [outputfilename := element.]               "Assigns the outputfilename if the inputfilename was assigned and the parameter exists"
         ]
   ].
]
ifFalse: [
   stderr << 'Must be in the form: hzip.st -dtcu inputfile [outputfile]' << nl.
   stderr << 'At the very least, must contain one Options flag [t, c, or u] and inputfile' << nl.
   ObjectMemory quit: 1
].

"Openning the files"
inputfile := FileStream open: inputfilename
                        mode: FileStream read.
readfile := FileStream open: inputfilename
                        mode: FileStream read.
(outputfilename = stdout)
ifTrue: [ outputfile := nil. ]
ifFalse: [ outputfile := FileStream open: outputfilename 
                                    mode: FileStream write. ].

"OPTION D ENABLED"
"Print the debug statement if option is enabled"
optionD ifTrue: 
[
      stdout << 'Debug: Enabled Option(s): ' << (Smalltalk getArgv: 1) 
      <<  ', Input File: ' << inputfilename << ', Output File: ' << outputfilename << nl.
].

"Creates an array to store the frequency of each character"
frequencyTable := ZeroArray new: 257.
frequencyTable at: 256 put: 1.

"Used to store leaf nodes of character and count"
priorityQueue := SortedCollection new.

optionU ifFalse:
[
      encodingTable := ZeroArray new: 257.

      "Reads inputfile and creates a frequency table, counting the number of times"
      "each character input appears on input"
      [inputfile atEnd not] 
      whileTrue:
            [
                  |char|
                  char := inputfile next asInteger.
                  frequencyTable incr: char.
            ].
      
      "Iterates over the frequency table for each non-zero element, create a leaf node"
      "Inserts the leaf node into a priority queue, with the character and count"
      frequencyTable keysAndValuesDo: 
      [:key :value |
            (value notNil and: [value > 0]) ifTrue:
            [
                  priorityQueue add: (Leaf new: key count: value).
            ]
      ].

      "Repeatedly remove two smallest elements from the queue, creating a new tree which is"
      "then entered into the priority queue. The smaller tree/leaf becomes the left child"
      "The process stops when there is only one tree left and the priority queue is empty"
      [priorityQueue size > 1] whileTrue: 
      [
            |leftNode RightNode treeChar totalCount|
            leftNode := priorityQueue removeFirst.
            rightNode := priorityQueue removeFirst.
            treeChar := leftNode char.
            totalCount := leftNode count + rightNode count.
            treeTemp := Tree new: treeChar count: totalCount left: leftNode right: rightNode.
            priorityQueue add: treeTemp.
      ].

      "For each non-zero character in the frequency table, construct an encoding string"
      "Using depth-first traversal. Encoding string is a sequence of bits indicitating the path"
      "from the root to a leaf"
      tree := priorityQueue removeFirst.
      encodingTable := ZeroArray new: 257.
      tree inorder: 
      [:char :string |
            encodingTable at: char put: string.
      ] prefix: ''.

      "Perform a post-order traversl of the decoding tree, writing out one bit at a time in big"
      "endian format. For each leaf write out a 0 bit, followed by 8 bits of the corresponding byte"
      "Write out the bits in the order that is highest bit first."
      "If the byte is 0, write out bit 8, which will be a 0 for a byte value of 0, and 1 for byte value of 256"
      binaryRep :=''.
      tree postorder: [:char :isleafNode|
            isleafNode ifFalse: [binaryRep := binaryRep, '1'.]
                       ifTrue:
                        [
                              bitTemp := ''.
                              (char = 256) 
                              ifTrue: [bitTemp := '000000001']
                              ifFalse: 
                              [
                                    (char = 0) 
                                    ifTrue: [bitTemp := '000000000']
                                    ifFalse:
                                    [
                                          8 to: 1 by: -1 do:[:i|
                                                bitTemp := bitTemp, ((char bitAt: i) printString).
                                          ]
                                    ].
                              ].
                        binaryRep := (binaryRep, '0', bitTemp).
                        ].
      ]
].

"OPTION T ENABLED"
"Write out the encoding table sorted by character. The first column is a sigle cheracter,"
"if printable, or an integer if not. The second column is the frequency for that character"
"The third column is a sequence of 0 and 1 cheacters indicating the encoding"
optionT ifTrue:
[
      encodingTable keysAndValuesDo:
      [:key :value|
            (value notNil and: [value > 0]) 
            ifTrue:
            [
                  stdout << key asCharacter visible << '     ' << (frequencyTable at: key)
                  << '    ' << value << nl.
            ].
      ].
].

"OPTION C ENABLED"
"Input file is compressed and written to output file if specified or stdout otherwise"
optionC ifTrue:
[
      (outputfile = nil)
      ifTrue:
      [
           [readfile atEnd not] whileTrue:
           [
                 |char|
                 char := readfile next asInteger.
                 code := (encodingTable at: char).
                 stdout << code.
           ] .
           stdout << (encodingTable at: 256) << nl.
      ]
      ifFalse:
      [
            outputBits := OutBits new: outputfile.
            1 to: (binaryRep size) do: 
            [:index|
                  bit := ((binaryRep at: index) asInteger) - 48.
                  outputBits writeBit: bit.
            ].
            outputBits writeBit: 1.
            [readfile atEnd not] whileTrue:
            [
                  |char|
                  char := readfile next asInteger.
                  code := (encodingTable at: char).
                  1 to: (code size) do:
                  [:index|
                        bit := ((code at: index) asInteger) - 48.
                        outputBits writeBit: bit.
                  ].
            ].
            endOfFile := (encodingTable at: 256).
            1 to: (endOfFile size) do:
            [:index|
                  bit := ((endOfFile at: index) asInteger) - 48.
                  outputBits writeBit: bit.
            ].
            outputBits flushByte.
      ].
].

"OPTION U ENABLED"
"The file is uncompressed"
optionU ifTrue:
[
      |stack endOfFile|

      (outputfile = nil) ifTrue: [ outputfile := stdout. ].

      stack := OrderedCollection new.
      filebits := BitStream new: inputfile.
      endOfFile := false.
      treeReconstructedFlag := false.
      treeFinal := ''.
      node := ''.

      "Reconstruct the Huffman decoding tree"
      "Reading bits"
      [(filebits atEnd not) & (endOfFile not)] whileTrue:
      [
            charBit := 0.
            bit := filebits nextBit.

            treeReconstructedFlag 
            ifFalse:
            [
                  (bit = 0) ifTrue:       "Read the next 8 bits and reconstruct the byte"
                  [
                        8 to: 1 by: -1 do:
                        [:index|
                              bitTemp := filebits nextBit.
                              charBit := charBit bitAt: index put: bitTemp.
                        ].
                        
                        "The value is 0, read one more bit"
                        (charBit = 0) ifTrue:
                        [
                              bitTemp := filebits nextBit.
                              (bitTemp = 1) ifTrue: [ charBit := charBit bitAt: 9 put: 1. ].
                        ].
                        stack add: (Leaf new: charBit count: 0).
                  ].

                  "Pop the 1-subtree from the stack, then pop the 0-subtree from the stack"
                  "Create a new tree with both of those children and push the new tree back"
                  "on the stack"
                  (bit = 1) ifTrue:
                  [
                        (stack size > 1) 
                        ifTrue:
                        [
                              rightNode := stack removeLast.
                              leftNode := stack removeLast.
                              treeTemp := (Tree new: leftNode char count: 0 left: leftNode right: rightNode).
                              stack add: treeTemp.
                        ]
                        ifFalse:
                        [
                              treeFinal := stack removeLast.
                              node := treeFinal.
                              treeReconstructedFlag := true.
                        ].
                  ].
            ]

            "Loop over the rest of the input file to reconstruct the original file"
            "Initialize a pointer to the root of the decoding tree."
            ifTrue:
            [
                  (node isleafNode) 
                  ifTrue:
                  [
                        ((node char) = 256) 
                        ifTrue: [ endOfFile := true ]
                        ifFalse:
                        [
                              outputfile nextPut: (Character value: (node char)).
                              node := treeFinal.
                        ].
                  ]
                  ifFalse:
                  [
                        (bit = 0)
                        ifTrue:
                        [
                              node := node left.
                              (node isleafNode) ifTrue:
                              [
                                    (endOfFile not) ifTrue:
                                    [
                                          ((node char) = 256) 
                                          ifTrue: [ endOfFile := true ]
                                          ifFalse: 
                                          [
                                                outputfile nextPut: (Character value: (node char)).
                                                node := treeFinal.
                                          ].
                                    ]     
                              ]
                        ]
                        ifFalse:
                        [
                              node := node right.
                              (node isleafNode) 
                              ifTrue:
                              [
                                    ((node char) = 256)
                                    ifTrue: [ endOfFile := true ]
                                    ifFalse:
                                    [
                                          outputfile nextPut: (Character value: (node char)).
                                          node := treeFinal.
                                    ]
                              ]
                        ].
                  ].
            ].
      ].
].

inputfile close.
(outputfile ~= nil) ifTrue: [ outputfile close. ].
ObjectMemory quit: 0