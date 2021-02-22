develop:
	ghcid -c 'stack ghci prose:lib prose:test:prose-test --ghci-options -fobject-code' --test ':main --rerun'

develop-all:
	ghcid -c 'stack ghci prose:lib prose:test:prose-test --ghci-options -fobject-code' --test ':main'
