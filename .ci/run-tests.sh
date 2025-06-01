#!/bin/sh

CONTAINER_IP=`podman container inspect --format '{{ (index .NetworkSettings.Networks "ci_default").IPAddress }}' ci_rlgl-server_1`

podman logs --follow ci_rlgl-server_1 &
LOG_PID=$!

if [ -z "${CONTAINER_IP}" ]; then
	  echo "ERROR: Container has no IP address"
	  # podman container inspect ci_rlgl-server_1
    CONTAINER_IP=127.0.0.1
fi

CONTAINER_IP=127.0.0.1

echo "Container IP: $CONTAINER_IP"

if ! podman container exists ci_rlgl-server_1; then
	  echo "ERROR: Container doesn't exist"
	  exit 1
fi

CONTAINER_STATUS=$(podman container inspect --format '{{.State.Status}}' ci_rlgl-server_1)
echo "Container status: $CONTAINER_STATUS"

if [ "$CONTAINER_STATUS" != "running" ]; then
	  echo "ERROR: Container is not running"
	  podman logs ci_rlgl-server_1
	  exit 1
fi

echo "Waiting for service to be ready..."
WAIT_COUNT=0
if timeout -s TERM 120 bash -c \
	         'while [[ "$(curl -s -o /dev/null -L -w ''%{http_code}'' ${0})" != "200" ]]; do
		   echo "Attempt $((++WAIT_COUNT)): Waiting for ${0}"
	     sleep 5
	   done' http://$CONTAINER_IP:8080/healthz ; then

	  echo "Service is ready!"
	  sleep 5  # Extra safety margin

	  # Setup test environment in` container
	  podman exec -t -e IP="$CONTAINER_IP" ci_rlgl-server_1 \
	         sh -c 'echo -n "http://$CONTAINER_IP:8080" > /tmp/server-uri'

	  SERVER_URI=$(podman exec -t ci_rlgl-server_1 cat /tmp/server-uri)
    echo "Server URI: $SERVER_URI"

	  # Run tests
	  ./.ci/test.sh "$CONTAINER_IP:8080" || TEST_FAILED=true

	  # Always show logs
	  kill $LOG_PID 2>/dev/null || true
	  podman logs ci_rlgl-server_1
	  if [ "$TEST_FAILED" = "true" ]; then
	      exit 1
	  fi
else
    echo "ERROR: Service failed to start within timeout"
	  kill $LOG_PID 2>/dev/null || true
	  podman logs ci_rlgl-server_1
	  exit 1
fi
