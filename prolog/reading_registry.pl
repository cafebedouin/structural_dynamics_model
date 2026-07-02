% ============================================================================
% READING REGISTRY (OQ-137) — the aggregatable-reading totality registry
% ============================================================================
% One queryable fact per "aggregatable reading": a verdict/reading predicate an
% aggregate (census, report table, sweep) could consume as a measurement over a
% corpus-enumerable key. The OQ-121 typed-absence convention
% (docs/design/design_discipline.md §5) binds exactly these predicates: on its
% declared domain the reading must return an EXPLICIT token (out-of-domain /
% absence / measured), never fail silently — a bare failure collapses
% didn't-apply / measured-clear / didn't-look into one missing token at every
% read site (Build Discipline Pattern 6 at the source).
%
% The registry is the machine-checkable half of that convention:
% tests/test_reading_totality.pl walks every total_on_domain entry and proves
% exactly-one-solution over the entry's declared domain (totality AND
% determinism — a nondeterministic reading over-counts in an aggregate exactly
% the way a partial one under-counts).
%
%   aggregatable_reading(Module:Pred/Arity, DomainSpec, Class)
%     DomainSpec ∈ {corpus_constraint, seat}   (extend reading_domain_key/2)
%     Class      ∈ {total_on_domain, partial_by_design(Reason)}
%   The KEY args are the FIRST len(DomainSpec-key) arguments of the predicate;
%   remaining arguments are outputs.
%
%   partial_by_design(Reason): genuinely relational predicates whose off-domain
%   silence is correct (e.g. a per-seat lookup keyed on an existing seat).
%   Recorded so "not total" is a DECLARED class, not an unexamined absence; the
%   totality suite does not run them. A predicate that fails ON its own declared
%   domain is neither class — it is a defect: fix it to a typed token (§5),
%   then register it total_on_domain.
%
% ANTI-FORK BRIDGE (Pattern 2): census_source_backing/2 maps every
% commentary_census source to the registry entry backing its commentary_cell/3
% hook. test_reading_totality.pl proves forall commentary_source(S) a backing
% entry exists and is registered total_on_domain — so a census source can never
% be absent from the totality registry (the census's Σ==n_corpus invariant
% presumes exactly the property this registry tests).
%
% LOADING: not loaded by stack.pl (mirrors commentary_census.pl) — load
% alongside: swipl -l stack.pl -l reading_registry.pl ... . Module-qualified
% calls into corpus_loader/narrative_ontology bind at call time, so this file
% loads cleanly on its own; the predicates must be resident when the domain
% enumeration runs (they are, under [stack]).
%
% REGISTRATION IS OPT-IN (named residual risk, OQ-137 close): a new reading
% predicate that never registers escapes the totality suite entirely. When you
% ADD an aggregatable reading, add its entry here in the same change.
% ============================================================================

:- module(reading_registry, [
    aggregatable_reading/3,
    census_source_backing/2,
    reading_domain_key/2
]).

:- use_module(corpus_loader).

% Extension points are multifile so a future module can register its own
% readings without editing this file.
:- multifile aggregatable_reading/3.
:- multifile census_source_backing/2.

% ----------------------------------------------------------------------------
% DOMAIN ENUMERATION  reading_domain_key(+DomainSpec, -KeyArgs)
%   One solution per domain member; KeyArgs is the list of key arguments in
%   predicate-argument order. Corpus membership is corpus_constraint/1 — the
%   authoritative denominator (never constraint_metric unions).
% ----------------------------------------------------------------------------
reading_domain_key(corpus_constraint, [C]) :-
    corpus_loader:corpus_constraint(C).
reading_domain_key(seat, [C, Name]) :-
    corpus_loader:corpus_constraint(C),
    narrative_ontology:constraint_stakeholder(C, Name, _, _, _, _, _).

% ----------------------------------------------------------------------------
% REGISTRY — seeded with the proven-total family (OQ-121 hand-fixes + the two
% never-fail templates). Exactly-one over the live corpus witnessed 2026-07-02
% (N=119 constraints, 661 seats, 0 violations on every entry below).
% ----------------------------------------------------------------------------
aggregatable_reading(stakeholder_seats:q6_crosscheck/3,        corpus_constraint, total_on_domain).
aggregatable_reading(stakeholder_seats:extraction_state/2,     corpus_constraint, total_on_domain).
aggregatable_reading(stakeholder_seats:consensus_provenance/2, corpus_constraint, total_on_domain).
aggregatable_reading(stakeholder_seats:seat_perceived_vs_real/4, seat,            total_on_domain).
aggregatable_reading(signature_detection:constraint_signature/2, corpus_constraint, total_on_domain).

% Partial-by-design (relational; off-domain silence is the domain, not a
% defect). The doc-named case from OQ-137's scope discriminator:
aggregatable_reading(stakeholder_seats:in_contention/3, corpus_constraint,
    partial_by_design('relation between seats, not a per-constraint verdict; holds only where a beneficiary-side/payer pair shares a power atom')).

% ----------------------------------------------------------------------------
% ANTI-FORK BRIDGE  census_source_backing(?Source, ?Entry)
%   Every commentary_census:commentary_source/1 must appear here, and its Entry
%   must be a total_on_domain registry entry (enforced by
%   tests/test_reading_totality.pl:census_sources_all_registered).
% ----------------------------------------------------------------------------
census_source_backing(q6,                 stakeholder_seats:q6_crosscheck/3).
census_source_backing(extraction_reading, stakeholder_seats:extraction_state/2).
census_source_backing(consensus,          stakeholder_seats:consensus_provenance/2).
