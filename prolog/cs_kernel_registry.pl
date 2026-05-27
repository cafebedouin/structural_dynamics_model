% ============================================================================
% CS KERNEL REGISTRY
% ============================================================================
% Cross-reading analytics for contested kernels.
%
% A contested kernel is a structural arrangement that multiple constraint stories
% read differently. cs_kernel_id/2 links each reading to its kernel atom.
%
% Exports:
%   cs_readings_for_kernel/2   — cs_readings_for_kernel(+K, -UID-C Pairs)
%   cs_kernel_coverage/2       — cs_kernel_coverage(+K, -N)
%   cs_kernel_divergence/4     — cs_kernel_divergence(+K, -Ctx, -UID1-C1, -UID2-C2)
%
% cs_kernel_divergence/4 is the CS-layer analogue of perspectival_incoherence:
% same kernel, different readings, different DR-type at the same observer context.
% First-class diagnostic — not hedged. Uses classify_at_time/4 (canonical
% post-2026-05-17 sigmoid pipeline: χ = ε × f(d) × σ(S)).
% DR/CS invariant: classify_at_time calls use C (name-keyed); DR is instance-blind.
% Two instances sharing C will receive the same DR type — by design.
% ============================================================================

:- module(cs_kernel_registry, [
    cs_readings_for_kernel/2,
    cs_kernel_coverage/2,
    cs_kernel_divergence/4
]).

:- use_module(narrative_ontology).
:- use_module(drl_composition).
:- use_module(constraint_indexing).

% Declare cs_kernel_id/2 multifile in narrative_ontology so testsets can extend it.
:- multifile narrative_ontology:cs_kernel_id/2.

%% cs_readings_for_kernel(+K, -Pairs)
%  Pairs = sorted list of UID-C pairs for readings that declare cs_kernel_id(C, K).
%  UID is the story_uid surrogate; C is the reading name. Multiple instances of C
%  (re-runs of the same reading) produce distinct UID-C pairs — by design.
cs_readings_for_kernel(K, Pairs) :-
    findall(UID-C, (narrative_ontology:cs_story_uid(C, UID),
                    narrative_ontology:cs_kernel_id(C, K)), Pairs0),
    sort(Pairs0, Pairs).

%% cs_kernel_coverage(+K, -N)
%  N = number of distinct reading instances registered for kernel K.
cs_kernel_coverage(K, N) :-
    cs_readings_for_kernel(K, Pairs),
    length(Pairs, N).

%% cs_kernel_divergence(+K, -Ctx, -UID1-C1, -UID2-C2)
%  Fires when two reading instances of kernel K classify differently at the same
%  observer context Ctx (a context/4 tuple from site_contexts_product/1).
%  UID1 @< UID2 prevents symmetric duplicates and correctly distinguishes instances
%  sharing a name (different re-runs). DR classify_at_time calls remain C-keyed
%  (DR is instance-blind by design: two instances sharing C see the same DR type).
%  Time fixed at 0 (baseline comparison across readings).
cs_kernel_divergence(K, Ctx, UID1-C1, UID2-C2) :-
    cs_readings_for_kernel(K, Pairs),
    member(UID1-C1, Pairs), member(UID2-C2, Pairs), UID1 @< UID2,
    constraint_indexing:site_contexts_product(AllContexts),
    member(Ctx, AllContexts),
    once(drl_composition:classify_at_time(C1, 0, Ctx, Type1)),
    once(drl_composition:classify_at_time(C2, 0, Ctx, Type2)),
    Type1 \= Type2.
