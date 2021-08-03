# MIDS Usage

Set up a symlink to this folder from the `sf-processor/plugins` folder.

To demonstrate the Microservice-Aware Reference Monitor (MRM), run the following command from the driver directory in the `sf-processor` repository.

    ./sfprocessor -log trace -config=../plugins/mrm/resources/pipelines/echo.json ../plugins/mrm/resources/traces/echo/benign/ | grep "Security Alert"
    ./sfprocessor -log trace -config=../plugins/mrm/resources/pipelines/echo.json ../plugins/mrm/resources/traces/echo/malicious/ | grep "Security Alert"

Models can be found in the `resources/models` folder.
