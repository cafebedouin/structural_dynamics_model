% probe_oq197_controls.pl — OQ-197 acceptance controls (positive + paired negative).
% Loaded after [stack] + a corpus overlay. Prints, for the loaded corpus, the
% gap_status distribution under BOTH sources plus the substrate-reproduced counts,
% so the positive assertion and its negative control sit in one output.

:- module(probe_oq197_controls, [oq197_summary/1]).
:- use_module(library(lists)).

status_kind(gap(_,_,_),          gap)                         :- !.
status_kind(no_gap,              no_gap)                       :- !.
status_kind(undetermined(R),     undet(R))                     :- !.

% count of constraints whose gap_status under Source has the given kind template
count_kind(Source, KindTmpl) :-
    aggregate_all(count,
        ( corpus_loader:corpus_constraint(C),
          report_generator:gap_status(C, Source, S),
          status_kind(S, KindTmpl) ),
        N),
    format("      ~w: ~w~n", [KindTmpl, N]).

dist(Source) :-
    format("    source=~w:~n", [Source]),
    count_kind(Source, gap),
    count_kind(Source, no_gap),
    count_kind(Source, undet(no_seats)),
    count_kind(Source, undet(single_seat)),
    count_kind(Source, undet(single_power_position)).

% independently reproduce "cross-seat-varying": >=2 distinct non-unknown CANONICAL
% dr_type across the seats (the population the 944 claim names).
canonical_varying(C) :-
    findall(T, report_generator:seat_type_reading(C, canonical, reading(_,_,T,_)), Ts),
    sort(Ts, Distinct),
    Distinct = [_, _|_].

oq197_summary(Label) :-
    aggregate_all(count, corpus_loader:corpus_constraint(_), NC),
    aggregate_all(count, narrative_ontology:constraint_stakeholder(_,_,_,_,_,_,_), NStake),
    aggregate_all(count, (corpus_loader:corpus_constraint(C), canonical_varying(C)), NVary),
    format("~n==== ~w ====~n", [Label]),
    format("  corpus_constraints=~w  stakeholder_facts=~w  canonical_cross_seat_varying=~w~n",
           [NC, NStake, NVary]),
    format("  gap_status distribution:~n"),
    dist(stakeholder),
    dist(canonical),
    % positive (i)/(ii): of the canonical-varying set, how many read undetermined vs
    % gap vs no_gap UNDER THE LIVE (a) source (never silent-0 is: none silently absent —
    % every one lands in exactly one bucket, shown below).
    aggregate_all(count, (corpus_loader:corpus_constraint(C), canonical_varying(C),
                          report_generator:gap_status(C, stakeholder, undetermined(_))), VaryUndetA),
    aggregate_all(count, (corpus_loader:corpus_constraint(C), canonical_varying(C),
                          report_generator:gap_status(C, stakeholder, gap(_,_,_))), VaryGapA),
    aggregate_all(count, (corpus_loader:corpus_constraint(C), canonical_varying(C),
                          report_generator:gap_status(C, stakeholder, no_gap)), VaryNoGapA),
    format("  canonical-varying under source(a): undetermined=~w gap=~w no_gap=~w  (sum must = ~w)~n",
           [VaryUndetA, VaryGapA, VaryNoGapA, NVary]),
    % case (ii) population from substrate: constraints with >=1 authored stakeholder fact
    % whose source-(a) gap_status is undetermined = "stakeholders present but insufficient".
    aggregate_all(count,
        ( corpus_loader:corpus_constraint(C),
          once(narrative_ontology:constraint_stakeholder(C,_,_,_,_,_,_)),
          report_generator:gap_status(C, stakeholder, undetermined(_)) ),
        PresentInsuff),
    aggregate_all(count,
        ( corpus_loader:corpus_constraint(C),
          once(narrative_ontology:constraint_stakeholder(C,_,_,_,_,_,_)),
          report_generator:gap_status(C, stakeholder, gap(_,_,_)) ),
        PresentGap),
    aggregate_all(count,
        ( corpus_loader:corpus_constraint(C),
          once(narrative_ontology:constraint_stakeholder(C,_,_,_,_,_,_)),
          report_generator:gap_status(C, stakeholder, no_gap) ),
        PresentNoGap),
    format("  case(ii) stakeholders-PRESENT under source(a): undetermined=~w (present-but-insufficient) | gap=~w | no_gap=~w~n",
           [PresentInsuff, PresentGap, PresentNoGap]).
