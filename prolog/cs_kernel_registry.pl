% ============================================================================
% CS KERNEL REGISTRY
% ============================================================================
% Cross-reading analytics for contested kernels.
%
% A contested kernel is a structural arrangement that multiple constraint stories
% read differently. cs_kernel_id/2 links each reading to its kernel atom.
%
% Exports:
%   cs_readings_for_kernel/2   — cs_readings_for_kernel(+K, -ConstraintList)
%   cs_kernel_coverage/2       — cs_kernel_coverage(+K, -N)
%   cs_kernel_divergence/4     — cs_kernel_divergence(+K, -Ctx, -C1, -C2)
%
% cs_kernel_divergence/4 is the CS-layer analogue of perspectival_incoherence:
% same kernel, different readings, different DR-type at the same observer context.
% First-class diagnostic — not hedged. Uses classify_at_time/4 (canonical
% post-2026-05-17 sigmoid pipeline: χ = ε × f(d) × σ(S)).
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

%% cs_readings_for_kernel(+K, -Cs)
%  Cs = sorted list of constraint IDs that declare cs_kernel_id(C, K).
cs_readings_for_kernel(K, Cs) :-
    findall(C, narrative_ontology:cs_kernel_id(C, K), Cs0),
    sort(Cs0, Cs).

%% cs_kernel_coverage(+K, -N)
%  N = number of distinct readings registered for kernel K.
cs_kernel_coverage(K, N) :-
    cs_readings_for_kernel(K, Cs),
    length(Cs, N).

%% cs_kernel_divergence(+K, -Ctx, -C1, -C2)
%  Fires when two readings of kernel K classify differently at the same observer
%  context Ctx (a context/4 tuple from site_contexts_product/1). C1 @< C2 prevents
%  symmetric duplicates. Time fixed at 0 (baseline comparison across readings).
cs_kernel_divergence(K, Ctx, C1, C2) :-
    cs_readings_for_kernel(K, Cs),
    member(C1, Cs), member(C2, Cs), C1 @< C2,
    constraint_indexing:site_contexts_product(AllContexts),
    member(Ctx, AllContexts),
    once(drl_composition:classify_at_time(C1, 0, Ctx, Type1)),
    once(drl_composition:classify_at_time(C2, 0, Ctx, Type2)),
    Type1 \= Type2.
