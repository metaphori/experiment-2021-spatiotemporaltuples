TASK_NAME=$1
TASK_IMAGE=$2
TASK_CMD=$3
TASK_WD=$4
#TASK_WD={$4:-'/'}
#SLEEP_TIME={$5:-'7d'}
SLEEP_TIME=$5

kubectl apply -f - <<EOF
apiVersion: v1
kind: Pod
metadata:
  name: $TASK_NAME
spec:
  restartPolicy: Never
  containers:
  - image: $TASK_IMAGE
    name: $TASK_NAME-container
    imagePullPolicy: Always
    command:
    - sh
    - '-c'
    - 'cd $TASK_WD ; $TASK_CMD; sleep $SLEEP_TIME'
EOF
