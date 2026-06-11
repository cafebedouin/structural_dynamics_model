% ============================================================================
% Histogram gate (OQ-98 Step 3, between Commits 1 and 2)
% ============================================================================
% verdict_join/3 exists after Commit 1 but nothing serializes it yet.
% This script pastes the corpus-wide base_verdict -> joined_verdict transition
% histogram (with counts by alert type) so the moderate-vs-severe severity
% ruling settles on evidence BEFORE any output ships (operator flag 1).
%
% Run from prolog/ on the run_pipeline loader chain:
%   swipl -l stack.pl -l covering_analysis.pl -l maxent_classifier.pl \
%         -l dirac_classification.pl -l diagnostic_summary.pl \
%         -l post_synthesis.pl -l json_report.pl \
%         -l ../audits/2026-06-11_oq98_verdict_join/histogram_gate.pl \
%         -g "hist_main, halt." -t "halt(1)"
% ============================================================================

:- use_module(library(lists)).
:- use_module(library(pairs)).

hist_main :-
    setup_call_cleanup(
        open('../audits/2026-06-11_oq98_verdict_join/histogram_witness.txt', write, W),
        hist_run(W),
        close(W)).

hist_run(W) :-
    corpus_loader:load_all_testsets,
    % Replicate json_report precompute (json_report.pl:69-91)
    constraint_indexing:default_context(MaxEntCtx),
    measurement_layer:wasserstein_contexts(WCtxs),
    catch(maxent_classifier:maxent_multi_run(WCtxs, _), _, true),
    catch(maxent_classifier:maxent_indexed_run(MaxEntCtx, _), _, true),
    (   config:param(fpn_enabled, 1)
    ->  catch(drl_fpn:fpn_run(MaxEntCtx, _), _, true)
    ;   true
    ),
    catch(grothendieck_cohomology:corpus_cohomology(_), _, true),
    json_report:load_abductive_data,
    format(W, '=== OQ-98 histogram gate: base -> joined transitions (2026-06-11) ===~n', []),
    findall(C, corpus_loader:corpus_constraint(C), CsRaw), sort(CsRaw, Cs),
    length(Cs, NC),
    format(W, 'corpus: ~w constraints~n~n', [NC]),
    % once/1 on both goals: json_report consumes exactly the FIRST solution
    % (its ->/; hoist); without it diagnostic_summary's nondeterminism
    % inflated the histogram to 50 rows over 48 constraints (first run).
    findall(row(C, Base, Joined, Cap, AlertTypes, SigGrade),
            ( member(C, Cs),
              once(catch(diagnostic_summary:diagnostic_summary(C, S), _, fail)),
              once(diagnostic_summary:verdict_join(C, S,
                  verdict_join(Joined, Base, Cap, Alerts, _, _, SigGrade))),
              findall(T-Sev, member(alert(T, Sev, _), Alerts), AlertTypes)
            ),
            Rows),
    length(Rows, NRows),
    format(W, 'rows computed: ~w / ~w (missing = diagnostic_summary failed)~n~n', [NRows, NC]),
    % per-constraint rows
    forall(member(row(C, B, J, Cap, ATs, SG), Rows),
           format(W, '~w: ~w -> ~w (cap ~w; sig_grade ~w; alerts ~w)~n',
                  [C, B, J, Cap, SG, ATs])),
    % transition histogram
    findall(B-J, member(row(_, B, J, _, _, _), Rows), Trans),
    msort(Trans, TransSorted),
    clumped_count(TransSorted, TransHist),
    format(W, '~n--- transition histogram (base -> joined: count) ---~n', []),
    forall(member((B-J)-N, TransHist),
           format(W, '  ~w -> ~w: ~w~n', [B, J, N])),
    % alert-type histogram
    findall(T-Sev, ( member(row(_, _, _, _, ATs, _), Rows),
                     member(T-Sev, ATs) ), AllAlerts),
    msort(AllAlerts, AllSorted),
    clumped_count(AllSorted, AlertHist),
    format(W, '~n--- alert-type histogram (type-severity: count) ---~n', []),
    forall(member((T-Sev)-N, AlertHist),
           format(W, '  ~w [~w]: ~w~n', [T, Sev, N])),
    % signature grade histogram
    findall(SG, member(row(_, _, _, _, _, SG), Rows), SGs),
    msort(SGs, SGSorted),
    clumped_count(SGSorted, SGHist),
    format(W, '~n--- signature grade histogram ---~n', []),
    forall(member(SG-N, SGHist),
           format(W, '  ~w: ~w~n', [SG, N])),
    % cap histogram
    findall(Cap, member(row(_, _, _, Cap, _, _), Rows), Caps),
    msort(Caps, CapsSorted),
    clumped_count(CapsSorted, CapHist),
    format(W, '~n--- cap_applied histogram ---~n', []),
    forall(member(Cap-N, CapHist),
           format(W, '  ~w: ~w~n', [Cap, N])).

%% clumped_count(+SortedList, -KeyCountPairs)
clumped_count([], []).
clumped_count([H|T], [H-N|Rest]) :-
    count_prefix(H, T, 1, N, Remainder),
    clumped_count(Remainder, Rest).

count_prefix(_, [], N, N, []).
count_prefix(H, [H|T], Acc, N, Rest) :-
    !, Acc1 is Acc + 1,
    count_prefix(H, T, Acc1, N, Rest).
count_prefix(_, L, N, N, L).
