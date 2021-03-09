CMD=cabal new-repl test:prose-test --repl-options=-fno-break-on-exception --repl-options=-fno-break-on-error

develop:
	ghcid -c '${CMD}' --test ':main --rerun'

develop-all:
	ghcid -c '${CMD}' --test ':main'
