% ============================================================================
% persistence_export.pl — Extended classification export with H1 and W1 data
%
% Emits per-constraint classification, cohomological obstruction (H1), and
% Wasserstein transport profile (W1) for persistence barcode computation.
%
% Output format (one set of lines per constraint):
%   CLASSIFY:<ConstraintID>:<PowerAtom>:<Type>   (4 lines per constraint)
%   H1:<ConstraintID>:<H1Value>                  (1 line per constraint)
%   W1:<ConstraintID>:<W12>:<W23>:<W34>          (1 line per constraint)
%
% Usage (standalone):
%   swipl -g "[stack], persistence_export:export_classifications_with_topology, halt"
%
% Usage (from overlay — called by python/persistence_sweep.py):
%   :- [stack].
%   :- persistence_export:export_classifications_with_topology, halt.
% ============================================================================

:- module(persistence_export, [
    export_classifications_with_topology/0
]).

:- use_module(corpus_loader).
:- use_module(logical_fingerprint).
:- use_module(drl_core).
:- use_module(grothendieck_cohomology).
:- use_module(measurement_layer).
:- use_module(maxent_classifier).

%% export_classifications_with_topology/0
%  Exports CLASSIFY + H1 + W1 lines for all corpus constraints.
%  Sorted output for stable diffing across parameter perturbations.
%
%  Prerequisites: corpus must be loadable, MaxEnt must be runnable.
%  Side effects: populates MaxEnt distributions and cohomology cache.

export_classifications_with_topology :-
    corpus_loader:ensure_corpus_loaded,
    % Compute MaxEnt distributions at all 4 canonical contexts
    measurement_layer:wasserstein_contexts(WCtxs),
    catch(maxent_classifier:maxent_multi_run(WCtxs, _), _, true),
    % Clear and recompute cohomology cache
    grothendieck_cohomology:cohomology_cleanup,
    % Collect all constraint IDs
    findall(C, logical_fingerprint:known_constraint(C), RawCs),
    sort(RawCs, Constraints),
    % Emit classifications (same format as bifurcation_export)
    forall(
        (   member(C, Constraints),
            drl_core:standard_context(Ctx),
            Ctx = context(agent_power(Power), _, _, _),
            drl_core:dr_type(C, Ctx, Type)
        ),
        format("CLASSIFY:~w:~w:~w~n", [C, Power, Type])
    ),
    % Emit H1 values
    forall(
        member(C, Constraints),
        emit_h1(C)
    ),
    % Emit W1 transport profiles
    forall(
        member(C, Constraints),
        emit_w1(C)
    ).

%% emit_h1(+C)
%  Emit H1 line for constraint C. Silently skips on failure.
emit_h1(C) :-
    (   catch(grothendieck_cohomology:cohomological_obstruction(C, _H0, H1), _, fail)
    ->  format("H1:~w:~w~n", [C, H1])
    ;   true
    ).

%% emit_w1(+C)
%  Emit W1 transport profile for constraint C. Silently skips on failure.
emit_w1(C) :-
    (   catch(measurement_layer:wasserstein_transport_profile(C, Profile), _, fail)
    ->  Profile = transport_profile(
            edge(u1_u2, W12),
            edge(u2_u3, W23),
            edge(u3_u4, W34)
        ),
        format("W1:~w:~6f:~6f:~6f~n", [C, W12, W23, W34])
    ;   true
    ).
