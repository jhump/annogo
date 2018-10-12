.PHONY: default
default: deps checkgofmt vet predeclared staticcheck unused ineffassign test

.PHONY: deps
deps:
	go get -d -v -t ./...

.PHONY: updatedeps
updatedeps:
	go get -d -v -t -u -f ./...

.PHONY: install
install:
	go install ./...

.PHONY: checkgofmt
checkgofmt:
	@echo gofmt -s -l .
	@if [ -n "$$(go version | awk '{ print $$3 }' | grep -v devel)" ]; then \
		output="$$(gofmt -s -l .)" ; \
		if [ -n "$$output"  ]; then \
		    echo "$$output"; \
			echo "Run gofmt on the above files!"; \
			exit 1; \
		fi; \
	fi

.PHONY: vet
vet:
	go vet ./...

# goyacc generates assignments where LHS is never used, so we need to run
# staticheck in a way that ignores the errors in that generated code
.PHONY: staticcheck
staticcheck:
	@echo staticcheck --ignore $$(go list ./... | grep parser)/annotations.y.go:* ./...
	@go get honnef.co/go/tools/cmd/staticcheck
	@staticcheck --ignore $$(go list ./... | grep parser)/annotations.y.go:* ./...

.PHONY: unused
unused:
	@echo unused --ignore $$(go list ./... | grep parser)/annotations.y.go:* ./...
	@go get honnef.co/go/tools/cmd/unused
	@unused --ignore $$(go list ./... | grep parser)/annotations.y.go:* ./...

# same remarks as for staticcheck: we ignore errors in generated proto.y.go
.PHONY: ineffassign
ineffassign:
	@go get github.com/gordonklaus/ineffassign
	@echo ineffassign . --ignore parser/annotations.y.go
	@ineffassign -n $$(find . -type d | grep -v 'parser')
	@output="$$(ineffassign ./parser | grep -v 'annotationsDollar' || true)" ; \
	if [ -n "$$output"  ]; then \
	    echo "$$output"; \
	    exit 1; \
	fi

.PHONY: predeclared
predeclared:
	@go get github.com/nishanths/predeclared
	predeclared .

# Intentionally omitted from CI, but target here for ad-hoc reports.
.PHONY: golint
golint:
	@go get golang.org/x/lint/golint
	golint -min_confidence 0.9 -set_exit_status ./...

# Intentionally omitted from CI, but target here for ad-hoc reports.
.PHONY: errcheck
errcheck:
	@go get github.com/kisielk/errcheck
	errcheck ./...

.PHONY: test
test:
	go test -cover -race ./...

.PHONY: generate
generate:
	@go get golang.org/x/tools/cmd/goyacc
	@go install ./cmd/aptgo
	go generate ./...

.PHONY: testcover
testcover:
	@echo go test -race -covermode=atomic ./...
	@echo "mode: atomic" > coverage.out
	@for dir in $$(go list ./...); do \
		go test -race -coverprofile profile.out -covermode=atomic $$dir ; \
		if [ -f profile.out ]; then \
			tail -n +2 profile.out >> coverage.out && rm profile.out ; \
		fi \
	done

