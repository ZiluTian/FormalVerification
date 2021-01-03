import subprocess
import argparse
import time

parser = argparse.ArgumentParser(description='Run benchmark')
parser.add_argument('mode', type=str, choices=['recursive', 'iterative', 'sat4j', 'cafesat'],
                    help='an integer for the accumulator')
args = parser.parse_args()

test_cnt = 10
names=["uf20-91", "uf50-218", "uuf50-218", "uf75-325", "uf100-430", "uuf100-430", "uf125-538", "uf200-860", "uf250-1065"]

if args.mode == 'sat4j':
    for name in names:
        print(f"Running {name}")
        pref = name.split('-')[0]
        files = ['benchmarks/' + name + '/' + pref + '-0' + str(i) + '.cnf' for i in range(1, test_cnt + 1)]
        total_time = 0
        for file in files:
            # need to remove trailing trash in files
            with open("tmp.cnf", 'w') as tmpfile:
                subprocess.run(['head', '-n', '-3', file], stdout=tmpfile)
            res = subprocess.run(['java', '-jar', '../sat4j-sat.jar', "tmp.cnf"],
             text=True, capture_output=True, timeout=300)
#             print(res.stdout)
            total_time += float(res.stdout.split("\n")[-2].split()[-1])
        print(f"Mean time:{total_time / len(files)}")
elif args.mode == 'cafesat':
    for name in names:
        print(f"Running {name}")
        pref = name.split('-')[0]
        files = ['benchmarks/' + name + '/' + pref + '-0' + str(i) + '.cnf' for i in range(1, test_cnt + 1)]
        total_time = 0
        for file in files:
            # need to remove trailing trash in files
            res = subprocess.run(["../cafesat/target/cafesat", "--dimacs", file, "--time"],
             text=True, capture_output=True, timeout=30)
            total_time += float(res.stdout.split()[1])
        print(f"Mean time:{total_time / len(files)}")
else:
    if args.mode == 'recursive':
        classname = 'zamsat.Benchmark'
    else:
        classname = 'zamsat.BenchmarkIterative'

    subprocess.run(['sbt', 'package'])

    for name in names:
        print(f"Running {name}")
        pref = name.split('-')[0]
        files = [name + '/' + pref + '-0' + str(i) + '.cnf' for i in range(1, test_cnt + 1)]
    #	print('\n'.join(files))
        res = subprocess.run(['scala', '-cp', 'target/scala-2.13/zamsat_2.13-0.1.0-SNAPSHOT.jar', classname],
         input='\n'.join(files), text=True, capture_output=True, timeout=300)
        print(res.stdout)
