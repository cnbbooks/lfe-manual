PROJ = slate
VERSION = 2.7.0
BASE_DOCKER_VERSION = ruby2.6
DOCKER_ORG = lfex
DOCKER_TAG_PREFIX = $(DOCKER_ORG)/$(PROJ)
FULL_VERSION = $(VERSION)-$(BASE_DOCKER_VERSION)
DOCKER_TAG = $(DOCKER_TAG_PREFIX):$(FULL_VERSION)
DOCKER_LATEST = $(DOCKER_TAG_PREFIX):latest
CONTAINER_NAME = lfe-rebar3-docs

image:
	docker build . -t $(DOCKER_TAG)
	docker tag $(DOCKER_TAG) $(DOCKER_LATEST)

publish:
	docker push $(DOCKER_TAG)
	docker push $(DOCKER_LATEST)

run:
	docker run -d --rm --name $(CONTAINER_NAME) \
		-p 4567:4567 \
		-v `pwd`/../../docs:/srv/slate/build \
		-v `pwd`/source:/srv/slate/source $(DOCKER_TAG)

stop:
	docker stop $(CONTAINER_NAME)

regen:
	docker exec -it $(CONTAINER_NAME) /bin/bash -c "bundle exec middleman build"
