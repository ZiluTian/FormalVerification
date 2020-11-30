import subprocess
import argparse

parser = argparse.ArgumentParser(description='Run benchmark')
parser.add_argument('mode', type=str, choices=['recursive', 'iterative'],
                    help='an integer for the accumulator')
args = parser.parse_args()
if args.mode == 'recursive':
    classname = 'zamsat.Benchmark'
else:
    classname = 'zamsat.BenchmarkIterative'

subprocess.run(['sbt', 'package'])

test_cnt = 10
names=["uf20-91", "uf50-218", "uuf50-218", "uf75-325", "uf100-430", "uuf100-430", "uf125-538", "uf200-860", "uf250-1065"]

for name in names:
	print(f"Running {name}")
	pref = name.split('-')[0]
	files = [name + '/' + pref + '-0' + str(i) + '.cnf' for i in range(1, test_cnt + 1)]
#	print('\n'.join(files))
	res = subprocess.run(['scala', '-cp', 'target/scala-2.13/zamsat_2.13-0.1.0-SNAPSHOT.jar', classname],
	 input='\n'.join(files), text=True, capture_output=True, timeout=300)
	print(res.stdout)
