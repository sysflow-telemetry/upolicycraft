# MRM Usage

To demonstrate the Microservice-Aware Reference Monitor (MRM), run the following command. Models can be found in the `resources/models` folder.

    ./driver/sfprocessor -log trace -config=./driver/pipelines/echo-pipeline.json resources/traces/echo-benign/ | grep "Security Alert"
    ./driver/sfprocessor -log trace -config=./driver/pipelines/echo-pipeline.json resources/traces/echo-exploit.sf | grep "Security Alert"
