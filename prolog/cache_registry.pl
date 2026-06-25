% ============================================================================
% CACHE REGISTRY — single invalidation surface for in-session memo caches
% ============================================================================
% Problem this solves (docs/technical/swipl_load_path_and_probe_gotchas.md §7):
% several modules memoize per-constraint results in module-private dynamic
% predicates. An in-session overlay/retract probe that changes the facts those
% memos were computed from silently reads PRE-overlay state unless every cache
% is cleared — and a stale-cache "no change" is byte-identical to a real null
% result. Before this module, each probe had to know every cache by name.
%
% Usage:
%   ?- cache_registry:clear_all_caches.
%
% Contributing a cache (in the caching module):
%   :- multifile cache_registry:clear_hook/0.
%   cache_registry:clear_hook :- my_module_clear_predicate.
%
% Registered (2026-06-04): boltzmann_compliance (cached_classification/3,
% cached_coupling/2), covering_analysis (cached_grid_sig/2),
% grothendieck_cohomology (cached_obstruction/3 + run info), drl_fpn (fpn_*
% caches, all contexts), context_profile_mining (trajectory_cached/3),
% arakelov_height (arakelov_threshold_cache global — nb_delete, because a
% sentinel VALUE would be read back as a real threshold).
%
% Deliberately NOT registered: maxent_* state (maxent_classifier.pl) — that is
% corpus-FITTED MODEL state re-established only by its own fit runner, not a
% recompute-on-demand memo; clearing it mid-session would strand readers on an
% empty model. If your probe perturbs inputs to the MaxEnt fit, re-run the fit
% explicitly.
%
% probe_harness:with_overlay/2 calls clear_all_caches/0 automatically (before
% the goal and after restore). Direct probes should call it after any manual
% retract/assertz of corpus facts.
% ============================================================================

:- module(cache_registry, [
    clear_all_caches/0
]).

:- multifile clear_hook/0.

%% clear_all_caches
%  Runs every registered clear_hook/0 clause. Each hook is exception-guarded
%  so one failing hook cannot mask the others (a partial clear that LOOKS
%  complete is the exact defect this module exists to prevent).
clear_all_caches :-
    forall(clause(clear_hook, Body),
           (   catch(call(Body), Error,
                     print_message(warning, cache_registry_hook_failed(Error)))
           ->  true
           ;   print_message(warning, cache_registry_hook_failed(goal_failed(Body)))
           )).
