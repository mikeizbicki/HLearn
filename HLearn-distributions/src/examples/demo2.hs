dataset :: [(Int,[(Int,DataItem)])]
dataset = 
    [(0, [(0,Continuous 6.0),(1,Continuous 180),(2,Continuous 12)])
    ,(0, [(0,Continuous 5.92),(1,Continuous 190),(2,Continuous 11)])
    ,(0, [(0,Continuous 5.58),(1,Continuous 170),(2,Continuous 12)])
    ,(0, [(0,Continuous 5.92),(1,Continuous 165),(2,Continuous 10)])
    ,(1, [(0,Continuous 5.5),(1, Continuous 130),(2,Continuous 7)])
    ,(1, [(0,Continuous 5.5),(1,Continuous 150),(2,Continuous 8)])
    ,(1, [(0,Continuous 5.42),(1,Continuous 130),(2,Continuous 7)])
    ,(1, [(0,Continuous 5.75),(1,Continuous 150),(2,Continuous 9)])
    ]
    
model=train' modelparams dataset
    
desc :: DataDesc Int
desc = DataDesc
    { numLabels = 2
    , labelL = [0,1]
    , numAttr = 20
    }
