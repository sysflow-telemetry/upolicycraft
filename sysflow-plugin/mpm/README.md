# Microservice-Aware Policy Monitor (MPM)

The microservice-aware policy monitor (MPM) is a component of a microservice
intrusion detection system (microIDS or uIDS) that uses effect graphs produced
by uPolicyCraft to detect policy violations in microservices.

Set up a symlink to this folder from the `sf-processor/plugins` folder.

To demonstrate the Microservice-Aware Policy Monitor (MPM), run the following
command from the driver directory in the `sf-processor` repository.

    ./sfprocessor -log trace -config=../plugins/mpm/resources/pipelines/echo.json ../plugins/mpm/resources/traces/echo/benign/ | grep "Security Alert"
    ./sfprocessor -log trace -config=../plugins/mpm/resources/pipelines/echo.json ../plugins/mpm/resources/traces/echo/malicious/ | grep "Security Alert"

Models can be found in the `resources/models` folder.

A real "deployment" of a distributed effect graph can be accomplished by
composing a pipeline consisting of the effect graphs within a microservice
architecture. Next, you deploy sf-processor to listen on a UNIX domain socket
to monitor for container telemetry  produced by the SysFlow Agent. Policy
violations are then immediately detected (e.g., within a millisecond) as
sfprocessor checks telemetry records against effect graphs in real-time.
