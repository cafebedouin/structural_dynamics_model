% ============================================================================
% probe_oq35_field_counterfactual.pl
%
% OQ-35 rows 2-3: is accessibility_collapse / resistance load-bearing or cosmetic?
%
% Settles load-bearing-vs-cosmetic with a counterfactual WITNESS, not a code-read.
% The diff variable is the FULL per-constraint observation tuple, NOT dr_type alone:
% post-OQ-138 the signatures these fields feed (false_summit_mountain, false_ci_rope,
% constructed_high_extraction) were converted RECLASSIFY->ROUTE — they revert dr_type
% to the metric type and carry their effect in verdict_join.{verdict,alerts,signature_grade}.
% Diffing dr_type alone would show a false 0-diff on exactly the path this OQ centers.
%
% observation(C) = obs(DrType, Signatures, Verdict, Alerts, SigGrade)
%   DrType     — drl_core:dr_type/2 (default context)
%   Signatures — sorted SET of signature_detection:constraint_signature/2 solutions
%   Verdict    — verdict_join headline (green|yellow|red)
%   Alerts     — sorted verdict_join Alerts list
%   SigGrade   — verdict_join signature_grade (correction|commentary|none)
%
% Five observation passes (all per-process; with_retracted/2 snapshots, restores,
% and runs cache_registry:clear_all_caches/0 — clears the Boltzmann memo hazard):
%   baseline      — no retraction
%   treatment     — retract accessibility_collapse + resistance metrics
%   pc_victim     — POSITIVE CONTROL: retract constraint_victim/2 (flips FSM/FCR
%                   severity moderate->informational -> grade + alerts change)
%   pc_claim      — POSITIVE CONTROL: retract constraint_claim(_,mountain) (flips
%                   constructed_high_extraction severe->informational)
%   null          — NULL CONTROL: with_retracted([], ...) (retracts nothing, clears
%                   caches) — must be byte-identical to baseline (observable stability)
%
% Plus PRESENCE counts: constraint_metric(_,accessibility_collapse,_) and resistance.
%
% Usage (one swipl process per corpus; overlay corpus_path with asserta BEFORE load):
%   swipl -g "asserta(config:param(corpus_path,'DIR')), [stack],
%             corpus_loader:load_all_testsets,
%             [probe_oq35_field_counterfactual],
%             run_oq35_probe('OUTDIR'), halt" -t "halt(1)"
% ============================================================================

:- use_module(probe_harness).
:- use_module(cache_registry).
:- use_module(corpus_loader).
:- use_module(drl_core).
:- use_module(signature_detection).
:- use_module(diagnostic_summary).
:- use_module(narrative_ontology).
:- use_module(library(lists)).

%% observe(+C, -Obs)
%  The full per-constraint observation tuple. Every component is captured under
%  catch/fail so a constraint that throws is recorded as `error`, never silently
%  dropped (absence must not read as success).
observe(C, obs(DrType, Sigs, Verdict, AlertsS, Grade)) :-
    (   catch(drl_core:dr_type(C, T0), _, fail) -> DrType = T0 ; DrType = error ),
    findall(S, catch(signature_detection:constraint_signature(C, S), _, fail), Ss0),
    sort(Ss0, Sigs),
    (   catch(( diagnostic_summary:diagnostic_summary(C, Summary),
                diagnostic_summary:verdict_join(C, Summary,
                    verdict_join(V0, _Base, _Cap, Alerts0, _Grid, _Meas, G0)) ),
              _, fail)
    ->  Verdict = V0, sort(Alerts0, AlertsS), Grade = G0
    ;   Verdict = error, AlertsS = [], Grade = error
    ).

%% all_obs(-Pairs)  Pairs = [C-Obs, ...] over the authoritative denominator,
%  key-sorted for a stable line order independent of corpus_constraint/1 order.
all_obs(Pairs) :-
    findall(C-Obs,
            ( corpus_loader:corpus_constraint(C), observe(C, Obs) ),
            Pairs0),
    keysort(Pairs0, Pairs).

%% write_obs(+File, +Pairs)  one `Id<TAB>writeq(Obs)` line per constraint.
write_obs(File, Pairs) :-
    setup_call_cleanup(
        open(File, write, S),
        forall(member(C-Obs, Pairs),
               ( write(S, C), write(S, '\t'),
                 writeq(S, Obs), nl(S) )),
        close(S)).

%% presence_count(+Metric, -N)
presence_count(Metric, N) :-
    findall(x, narrative_ontology:constraint_metric(_, Metric, _), L),
    length(L, N).

%% run_oq35_probe(+OutDir)
%  Writes <OutDir>/{baseline,treatment,pc_victim,pc_claim,null}.txt and prints a
%  SUMMARY block (denominator + presence counts) to stdout for the driver to scrape.
run_oq35_probe(OutDir) :-
    findall(C, corpus_loader:corpus_constraint(C), Cs), length(Cs, N),
    presence_count(accessibility_collapse, NAcc),
    presence_count(resistance, NRes),

    % --- baseline (clear caches first so it is comparable to with_retracted passes)
    cache_registry:clear_all_caches,
    all_obs(Base),
    atomic_list_concat([OutDir, '/baseline.txt'], BF),  write_obs(BF, Base),

    % --- treatment: retract the two target metrics
    probe_harness:with_retracted(
        [ narrative_ontology:constraint_metric(_, accessibility_collapse, _),
          narrative_ontology:constraint_metric(_, resistance, _) ],
        all_obs(Treat)),
    atomic_list_concat([OutDir, '/treatment.txt'], TF), write_obs(TF, Treat),

    % --- positive control A: retract constraint_victim/2 (flips FSM/FCR severity)
    probe_harness:with_retracted(
        [ narrative_ontology:constraint_victim(_, _) ],
        all_obs(PCV)),
    atomic_list_concat([OutDir, '/pc_victim.txt'], PVF), write_obs(PVF, PCV),

    % --- positive control B: retract mountain claims (flips constructed_high severity)
    probe_harness:with_retracted(
        [ narrative_ontology:constraint_claim(_, mountain) ],
        all_obs(PCC)),
    atomic_list_concat([OutDir, '/pc_claim.txt'], PCF), write_obs(PCF, PCC),

    % --- null control: retract nothing (clears caches), must equal baseline
    probe_harness:with_retracted([], all_obs(Null)),
    atomic_list_concat([OutDir, '/null.txt'], NF), write_obs(NF, Null),

    format("OQ35_SUMMARY denominator=~w presence_accessibility_collapse=~w presence_resistance=~w~n",
           [N, NAcc, NRes]).
