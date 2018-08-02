package processor

import "sync"

var (
	registryLock      sync.Mutex
	registeredPlugins []Processor
)

// RegisterProcessor registers the given annotation processor.
func RegisterProcessor(p Processor) {
	registryLock.Lock()
	defer registryLock.Unlock()
	registeredPlugins = append(registeredPlugins, p)
}

// AllRegisteredProcessors returns the list of all registered processors.
func AllRegisteredProcessors() []Processor {
	registryLock.Lock()
	defer registryLock.Unlock()
	procs := make([]Processor, len(registeredPlugins))
	copy(procs, registeredPlugins)
	return procs
}
