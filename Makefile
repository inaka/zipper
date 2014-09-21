PROJECT = zipper

DEPS = sync

dep_sync = git https://github.com/rustyio/sync.git master

include erlang.mk

ERLC_OPTS += +warn_unused_vars +warn_export_all +warn_shadow_vars +warn_unused_import +warn_unused_function
ERLC_OPTS += +warn_bif_clash +warn_unused_record +warn_deprecated_function +warn_obsolete_guard +strict_validation
ERLC_OPTS += +warn_export_vars +warn_exported_vars +warn_missing_spec +warn_untyped_record +debug_info

# Commont Test Config

CT_SUITES = zipper zipper_default
CT_OPTS = -cover test/zipper.coverspec

shell: app
	erl -pa ebin -pa deps/*/ebin -s sync

tests-shell: app build-ct-suites
	erl -pa ebin -pa deps/*/ebin -pa test -s sync
