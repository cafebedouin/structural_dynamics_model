% ============================================================================
% CONSTRAINT STORY: statute_of_anne_ip_foundation__entangled_event_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_statute_of_anne_ip_foundation__entangled_event_reading, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

% --- Constraint Identity Rule (DP-001: ε-Invariance) ---
% Each constraint story must have a single, stable base extractiveness (ε).
% If changing the observable used to evaluate this constraint would change ε,
% you are looking at two distinct constraints. Write separate .pl files for
% each, link them with affects_constraint/2, and document the relationship
% in both files' narrative context sections.
%
% The context tuple is CLOSED at arity 4: (P, T, E, S).
% Do not add measurement_basis, beneficiary/victim, or any other arguments.
% Linter Rule 23 enforces context/4.
%
% See: epsilon_invariance_principle.md

% --- Namespace Hooks (Required for loading) ---
:- multifile
    domain_priors:base_extractiveness/2,
    domain_priors:suppression_score/2,
    domain_priors:theater_ratio/2,
    domain_priors:requires_active_enforcement/1,
    narrative_ontology:has_sunset_clause/1,
    narrative_ontology:interval/3,
    narrative_ontology:measurement/5,
    narrative_ontology:constraint_metric/3,
    narrative_ontology:constraint_beneficiary/2,
    narrative_ontology:constraint_victim/2,
    narrative_ontology:constraint_claim/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_axiom_grounding/3,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:cs_created_at/2,
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: statute_of_anne_ip_foundation__entangled_event_reading
 *   human_readable: Statute of Anne (1710) as Entangled Conceptual-Institutional Event
 *   domain: legal_history/intellectual_property/institutional_economics
 *
 * SUMMARY:
 *   This story instantiates the 'entangled event' reading of the Statute of
 *   Anne kernel: that the 1710 act's conceptual innovation (a term-limited,
 *   statutory literary right, as opposed to a perpetual common-law claim) and
 *   its institutional reallocation (moving practical control from the
 *   Stationers' Company toward a nominal author-vesting that publishers
 *   immediately re-captured by assignment) happened in the same clauses, at
 *   the same moment, and cannot be separated into a 'first the concept, then
 *   the reallocation' or 'first the reallocation, then the concept followed'
 *   sequence. The sibling readings (conceptual_emergence_reading,
 *   institutional_reallocation_reading) each isolate one dimension as
 *   primary; this reading holds that isolating either dimension misdescribes
 *   the historical fact, because the statutory text performs both moves in a
 *   single legislative act whose beneficiary structure is irreducibly
 *   ambiguous (authors nominal, publishers practical) and whose casualty is
 *   not a person but the possibility of a clean theoretical account — courts
 *   spent sixty-four years (to Donaldson v Becket, 1774) trying and failing
 *   to retroactively separate the two dimensions the statute had fused.
 *
 * KEY AGENTS:
 *   - authors_nominal: nominal rights-holder, low practical power, immediate assignor
 *   - booksellers_and_publishers: organized actor who operationalized both the new concept and the new right for commercial ends
 *   - unlicensed_printers_and_provincial_booksellers: bear penalties under a still-theoretically-unsettled regime
 *   - the_reading_public: delayed beneficiary of the term-limit concept, near-term payer of uncertain pricing
 *   - kings_bench_and_chancery_judges: institutional actor forced to retroactively decide which reading of the fused event would govern
 *   - legal_historians: analytical observer documenting the persistence of the three-way reading dispute itself as evidence
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(statute_of_anne_ip_foundation__entangled_event_reading, 0.55).
domain_priors:suppression_score(statute_of_anne_ip_foundation__entangled_event_reading, 0.42).
domain_priors:theater_ratio(statute_of_anne_ip_foundation__entangled_event_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(statute_of_anne_ip_foundation__entangled_event_reading, extractiveness, 0.55).
narrative_ontology:constraint_metric(statute_of_anne_ip_foundation__entangled_event_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(statute_of_anne_ip_foundation__entangled_event_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(statute_of_anne_ip_foundation__entangled_event_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(statute_of_anne_ip_foundation__entangled_event_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(statute_of_anne_ip_foundation__entangled_event_reading, tangled_rope).
narrative_ontology:human_readable(statute_of_anne_ip_foundation__entangled_event_reading, "Statute of Anne (1710) as Entangled Conceptual-Institutional Event").
narrative_ontology:topic_domain(statute_of_anne_ip_foundation__entangled_event_reading, "legal_history/intellectual_property/institutional_economics").

domain_priors:requires_active_enforcement(statute_of_anne_ip_foundation__entangled_event_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(statute_of_anne_ip_foundation__entangled_event_reading, '4e018081-f2bb-4f04-b2af-101014fbc0a9').
narrative_ontology:cs_kernel_codification('4e018081-f2bb-4f04-b2af-101014fbc0a9', formalized).
narrative_ontology:cs_authority_grounding('4e018081-f2bb-4f04-b2af-101014fbc0a9', lineage).
narrative_ontology:cs_interpretation_layer_present('4e018081-f2bb-4f04-b2af-101014fbc0a9').
narrative_ontology:cs_reading_relation('4e018081-f2bb-4f04-b2af-101014fbc0a9', statute_of_anne_ip_foundation__conceptual_emergence_reading, coexists_with).
narrative_ontology:cs_reading_relation('4e018081-f2bb-4f04-b2af-101014fbc0a9', statute_of_anne_ip_foundation__institutional_reallocation_reading, coexists_with).
narrative_ontology:cs_axiom('4e018081-f2bb-4f04-b2af-101014fbc0a9', foundational, conceptual_and_institutional_change_are_co_constitutive).
narrative_ontology:cs_axiom_status(conceptual_and_institutional_change_are_co_constitutive, holdable).
narrative_ontology:cs_axiom_grounding('4e018081-f2bb-4f04-b2af-101014fbc0a9', conceptual_and_institutional_change_are_co_constitutive, conventional).
narrative_ontology:cs_axiom('4e018081-f2bb-4f04-b2af-101014fbc0a9', secondary, beneficiary_ambiguity_is_a_structural_feature_not_a_gap_to_resolve).
narrative_ontology:cs_axiom_status(beneficiary_ambiguity_is_a_structural_feature_not_a_gap_to_resolve, holdable).
narrative_ontology:cs_axiom_grounding('4e018081-f2bb-4f04-b2af-101014fbc0a9', beneficiary_ambiguity_is_a_structural_feature_not_a_gap_to_resolve, conventional).
narrative_ontology:cs_reference_frame('4e018081-f2bb-4f04-b2af-101014fbc0a9', guild_succession_crisis_resolution).
narrative_ontology:cs_drift_state('4e018081-f2bb-4f04-b2af-101014fbc0a9', post_donaldson_v_becket_1774, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('4e018081-f2bb-4f04-b2af-101014fbc0a9', '').
narrative_ontology:cs_kernel_id(statute_of_anne_ip_foundation__entangled_event_reading, statute_of_anne_ip_foundation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(statute_of_anne_ip_foundation__entangled_event_reading, authors_nominal).
narrative_ontology:constraint_beneficiary(statute_of_anne_ip_foundation__entangled_event_reading, booksellers_and_publishers).
narrative_ontology:constraint_victim(statute_of_anne_ip_foundation__entangled_event_reading, conceptual_clarity_of_the_public_domain).
narrative_ontology:constraint_victim(statute_of_anne_ip_foundation__entangled_event_reading, unlicensed_printers_and_provincial_booksellers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(statute_of_anne_ip_foundation__entangled_event_reading, the_reading_public).
narrative_ontology:constraint_victim(statute_of_anne_ip_foundation__entangled_event_reading, the_reading_public).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Named in the statute's preamble as the vested rights-holder and encouraged as the object of the new 'encouragement of learning.' In practice, most authors lacked the capital, distribution networks, or bargaining leverage to exploit the fourteen-year term themselves and routinely assigned the right immediately to a bookseller for a lump sum, so the nominal grant rarely translated into durable control or income.
narrative_ontology:constraint_stakeholder(statute_of_anne_ip_foundation__entangled_event_reading, authors_nominal, beneficiary,
    powerless, biographical, constrained, national).

% Former Stationers' Company members who lost their perpetual common-law claim to copy but immediately reconstituted commercial control by acquiring the new statutory term from authors via assignment. They lobbied for the statute's passage, administered its registration requirements, and later litigated (Millar v Taylor, Donaldson v Becket) to try to preserve perpetual rights under the new institutional form — the entity that actually operationalized both the new concept and the new right.
narrative_ontology:constraint_stakeholder(statute_of_anne_ip_foundation__entangled_event_reading, booksellers_and_publishers, beneficiary,
    organized, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(statute_of_anne_ip_foundation__entangled_event_reading, booksellers_and_publishers, agenda_setter).

% Printers outside London and outside the pre-existing Stationers' monopoly, previously operating in gray-market or reprint trades, now faced a newly codified, statutorily enforceable exclusive right that criminalized reprinting even where no coherent conceptual account of what was being protected — expression, labor, or market position — yet existed to guide them. They bore penalties under a regime whose theoretical basis was still being worked out in courtrooms for the next sixty years.
narrative_ontology:constraint_stakeholder(statute_of_anne_ip_foundation__entangled_event_reading, unlicensed_printers_and_provincial_booksellers, payer,
    powerless, biographical, trapped, regional).

% Gained an eventual, if delayed, guarantee that works would enter the public domain after a fixed term — a benefit of the new limited-duration concept. In the near term, however, paid whatever price booksellers set under a legal regime that could not yet say clearly whether the right was a policy tool bounded in time or a perpetual property revived by another name, leaving prices and access uncertain for decades.
narrative_ontology:constraint_stakeholder(statute_of_anne_ip_foundation__entangled_event_reading, the_reading_public, payer,
    moderate, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(statute_of_anne_ip_foundation__entangled_event_reading, the_reading_public, beneficiary).

% Spent the six decades following 1710 adjudicating what the statute actually was — property, privilege, or something new — because the text itself did not resolve whether it codified a preexisting natural right or created an entirely novel, term-limited entitlement. Their rulings (culminating in Donaldson v Becket, 1774) did not discover a pre-existing answer; they retroactively fixed which reading of the single 1710 event would govern going forward.
narrative_ontology:constraint_stakeholder(statute_of_anne_ip_foundation__entangled_event_reading, kings_bench_and_chancery_judges, agenda_setter,
    institutional, generational, analytical, national).

% Study the statute after the fact and disagree about whether it should be read as conceptual innovation, institutional reallocation, or an irreducibly fused event. This story takes the fused-event position: that the disagreement itself is evidence the two dimensions were never separable in the historical record, not merely under-analyzed.
narrative_ontology:constraint_stakeholder(statute_of_anne_ip_foundation__entangled_event_reading, legal_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(statute_of_anne_ip_foundation__entangled_event_reading, booksellers_and_publishers).
narrative_ontology:fixing_cost_class(statute_of_anne_ip_foundation__entangled_event_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provided a single, publicly registrable mechanism (Stationers' Hall entry, fixed term, statutory penalty) that let authors, booksellers, and the public all reference one settled procedure for who could print what and for how long — replacing an opaque, perpetual, guild-internal claim with a legible, dated, external one.
% TRANSFER_FUNCTION: Moves control over reprinting from the Stationers' Company's perpetual common-law claim to a new statutory entitlement nominally vested in authors but almost immediately reassignable to publishers; moves interpretive certainty away from everyone, since the statute did not itself specify which theory of the right (natural property vs. limited policy grant) governed, leaving that question to be fought out later in courts.
% ABSENT_VOICES: Provincial and unlicensed printers who had operated in the interstices of the old Stationers' monopoly had no seat in drafting the statute and are barely visible in the parliamentary record; their objection — that a newly codified exclusive right was being imposed on a trade practice whose legal status had previously been ambiguous enough to work with — is reconstructed mainly from later prosecution records, not from contemporaneous testimony.
% DISAPPEARANCE_RATIONALE: Remove the 1710 event and there is no single anchor point from which to date either 'copyright as a concept' or 'authors/publishers as rights-holders' — the entire subsequent case law (Millar v Taylor, Donaldson v Becket, and the statutory tradition through to modern copyright acts) loses its founding reference. Because this reading holds the conceptual and institutional dimensions as one act, removing it does not just delay a right's assignment (institutional reading) or delay a concept's invention (conceptual reading) — it removes the single fact both later disputes needed to argue about.
% FOUNDING_PROBLEM: Booksellers' perpetual, guild-enforced monopoly over reprinting had become commercially unstable and increasingly contested by provincial trade and expiring licensing acts; Parliament needed a replacement mechanism that could simultaneously supply a new legal category (a term-limited right) and a new allocation of who held it (nominally authors), because no existing legal vocabulary distinguished the two — the statute had to do both jobs in the same clause or do neither.
% FOUNDING_PROBLEM_CORROBORATION: Legal historians outside the publishing trade (e.g., scholarship on the Donaldson v Becket aftermath and the Stationers' Company records held independently of any publisher's interest) corroborate that the guild-monopoly crisis which prompted the 1710 act was fully resolved by the 1774 House of Lords ruling; no publisher-side or author-side advocacy group attests that the original crisis persists today. The persistence of statutory copyright as an institution is now justified by entirely different, later-generated rationales (incentive theory, moral rights) rather than the original guild-succession problem.
narrative_ontology:disappearance_verdict(statute_of_anne_ip_foundation__entangled_event_reading, world_rearranges).
narrative_ontology:founding_problem_status(statute_of_anne_ip_foundation__entangled_event_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(statute_of_anne_ip_foundation__entangled_event_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(statute_of_anne_ip_foundation__entangled_event_reading, 'none', 1).
narrative_ontology:epsilon_provenance(statute_of_anne_ip_foundation__entangled_event_reading, 0.55, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(statute_of_anne_ip_foundation__entangled_event_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(statute_of_anne_ip_foundation__entangled_event_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(statute_of_anne_ip_foundation__entangled_event_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is authored at a moderate 0.55 (rather than the low end a pure conceptual-emergence reading might carry, or the high end a pure institutional-capture reading might carry) because this reading holds that SOME of what looks like extraction is actually the unavoidable cost of instantiating a genuinely new legal category for the first time — you cannot test-drive a term-limited right without someone holding it and someone else being excluded by it. Suppression (0.42) and theater_ratio (0.38) trend upward through the interval as courts layer interpretive machinery (assignments, registration enforcement, litigation over the right's true nature) onto a statute whose founding ambiguity was never resolved by the legislature itself, only by decades of case law improvising an answer.
 *
 * DIRECTIONALITY LOGIC:
 *   Authors are declared nominal beneficiaries because the text names them, but their exit options are constrained (assignment was near-universal and economically necessary) — the derivation should not push them to the full-beneficiary end despite the nominal declaration. Booksellers/publishers carry the practical beneficiary position with arbitrage-grade exit (they could and did restructure around whichever legal outcome prevailed). Unlicensed printers are trapped payers: the statute criminalized what had previously been a gray-market practice, with no transition period matched to the conceptual uncertainty they were penalized under. The reading public sits dual-role because the term-limit concept eventually benefits them (public domain entry) while near-term pricing extracts from them.
 *
 * MANDATROPHY ANALYSIS:
 *   The entangled-event reading resists two opposite mislabeling errors: it does not let the coordination story (Parliament solving a real monopoly-succession problem) launder the extraction (publishers recapturing practical control via assignment), and it does not let the extraction story (publishers as sole beneficiaries) erase the genuine conceptual innovation (a term-limited right did not exist before and had to be invented in the same breath as it was allocated). Classifying as tangled_rope rather than pure snare or pure rope preserves both: coordination function (a legible, registrable, dated procedure replacing an opaque perpetual guild claim) and asymmetric extraction (unlicensed printers pay under a still-undertheorized regime) coexist in the same statutory act, requiring active enforcement (statutory penalties, Stationers' Hall registration, and sixty years of litigation) to hold.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    separability_of_conceptual_and_institutional_dimensions,
    'Is the inseparability of the conceptual (new term-limited right category) and institutional (author/publisher reallocation) dimensions of the 1710 statute a genuine historical fact, or an artifact of this reading choosing not to prioritize one dimension as causally or logically prior to the other?',
    'Close textual and procedural history of the statute''s drafting: if surviving parliamentary drafts, committee records, or petitions show the conceptual category (term limits, public-domain reversion) being settled analytically prior to and independent of the question of who would hold the right, that would support the conceptual_emergence_reading''s separability claim over this reading''s fusion claim; if the drafting record shows both questions being negotiated simultaneously in the same clauses with no independent settlement of either, that supports this reading.',
    'If the dimensions are shown to be separable after all, this constraint''s claimed_type and beneficiary/victim structure would need to be redistributed across the two sibling readings rather than held jointly, and the ''victim is conceptual clarity'' framing would lose its evidentiary basis.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(separability_of_conceptual_and_institutional_dimensions, conceptual, 'Whether the fused-event reading is a defensible historical claim or a framing choice that avoids adjudicating primacy between the sibling readings.').

omega_variable(
    beneficiary_ambiguity_resolution_over_time,
    'Does the ambiguity between authors (nominal beneficiary) and publishers (practical beneficiary) resolve cleanly in favor of publishers once assignment practices are examined at scale, or does a meaningful minority of authors retain and exploit the statutory right themselves?',
    'Archival analysis of Stationers'' Hall registration records and known assignment contracts from 1710-1774 to determine what fraction of registered works were assigned immediately versus retained by the named author.',
    'If assignment was near-universal, the authors_nominal beneficiary declaration is closer to a legal fiction and the effective beneficiary structure collapses toward publishers alone, which would push this reading''s metrics and possibly its classification closer to the institutional_reallocation_reading''s snare-leaning profile.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_ambiguity_resolution_over_time, empirical, 'Whether the nominal author-benefit is real in a meaningful fraction of cases or almost entirely superseded by publisher assignment in practice.').

omega_variable(
    conceptual_clarity_as_a_coherent_victim,
    'Can ''conceptual clarity'' meaningfully be named as a victim of a legal event, or is this a category error that smuggles in a normative judgment (that clarity was owed and was denied) dressed as a structural fact?',
    'Compare against other foundational legal instruments that also left their theoretical basis contested for decades (e.g., early patent statutes, early contract-doctrine codifications) to see whether sustained multi-decade judicial contestation over a statute''s theoretical basis is a common and unremarkable feature of legal innovation, or unusually severe in this case.',
    'If contestation of this duration and intensity is normal for foundational statutes generally, naming ''conceptual clarity'' as a victim overstates the distinctiveness of the Statute of Anne''s fusion and may indicate the entangled_event_reading is over-dramatizing an ordinary feature of common-law development.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(conceptual_clarity_as_a_coherent_victim, conceptual, 'Whether naming conceptual clarity as a victim group is a defensible structural claim or an artifact of this reading''s framing.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(statute_of_anne_ip_foundation__entangled_event_reading, 1710, 1774).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(stat_tr_t1710, statute_of_anne_ip_foundation__entangled_event_reading, theater_ratio, 1710, 0.2).
narrative_ontology:measurement(stat_tr_t1720, statute_of_anne_ip_foundation__entangled_event_reading, theater_ratio, 1720, 0.25).
narrative_ontology:measurement(stat_tr_t1735, statute_of_anne_ip_foundation__entangled_event_reading, theater_ratio, 1735, 0.32).
narrative_ontology:measurement(stat_tr_t1750, statute_of_anne_ip_foundation__entangled_event_reading, theater_ratio, 1750, 0.36).
narrative_ontology:measurement(stat_tr_t1765, statute_of_anne_ip_foundation__entangled_event_reading, theater_ratio, 1765, 0.4).
narrative_ontology:measurement(stat_tr_t1774, statute_of_anne_ip_foundation__entangled_event_reading, theater_ratio, 1774, 0.38).

% Extraction over time
narrative_ontology:measurement(stat_be_t1710, statute_of_anne_ip_foundation__entangled_event_reading, base_extractiveness, 1710, 0.35).
narrative_ontology:measurement(stat_be_t1720, statute_of_anne_ip_foundation__entangled_event_reading, base_extractiveness, 1720, 0.42).
narrative_ontology:measurement(stat_be_t1735, statute_of_anne_ip_foundation__entangled_event_reading, base_extractiveness, 1735, 0.5).
narrative_ontology:measurement(stat_be_t1750, statute_of_anne_ip_foundation__entangled_event_reading, base_extractiveness, 1750, 0.53).
narrative_ontology:measurement(stat_be_t1765, statute_of_anne_ip_foundation__entangled_event_reading, base_extractiveness, 1765, 0.58).
narrative_ontology:measurement(stat_be_t1774, statute_of_anne_ip_foundation__entangled_event_reading, base_extractiveness, 1774, 0.55).

% Suppression requirement over time
narrative_ontology:measurement(stat_su_t1710, statute_of_anne_ip_foundation__entangled_event_reading, suppression_requirement, 1710, 0.3).
narrative_ontology:measurement(stat_su_t1720, statute_of_anne_ip_foundation__entangled_event_reading, suppression_requirement, 1720, 0.33).
narrative_ontology:measurement(stat_su_t1735, statute_of_anne_ip_foundation__entangled_event_reading, suppression_requirement, 1735, 0.38).
narrative_ontology:measurement(stat_su_t1750, statute_of_anne_ip_foundation__entangled_event_reading, suppression_requirement, 1750, 0.4).
narrative_ontology:measurement(stat_su_t1765, statute_of_anne_ip_foundation__entangled_event_reading, suppression_requirement, 1765, 0.45).
narrative_ontology:measurement(stat_su_t1774, statute_of_anne_ip_foundation__entangled_event_reading, suppression_requirement, 1774, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(statute_of_anne_ip_foundation__entangled_event_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(statute_of_anne_ip_foundation__entangled_event_reading, 0.12).
narrative_ontology:affects_constraint(statute_of_anne_ip_foundation__entangled_event_reading, conceptual_emergence_reading).
narrative_ontology:affects_constraint(statute_of_anne_ip_foundation__entangled_event_reading, institutional_reallocation_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the statute_of_anne_ip_foundation kernel. conceptual_emergence_reading treats the new legal category (term-limited right displacing perpetual claim) as the primary event with institutional reallocation as downstream consequence; institutional_reallocation_reading treats the transfer of practical control from the Stationers' Company to authors/publishers as primary with the conceptual apparatus as its packaging or justification. This entangled_event_reading declines to assign either dimension causal or logical priority, holding both as co-occurring facts of the same 1710 act, and treats the sixty-four-year judicial struggle to disentangle them (1710-1774) as itself evidence for the fusion claim. All three stories share the historical referent (the Statute of Anne and its aftermath) but author materially different ε, beneficiary/victim structures, and claimed types, consistent with the ε-invariance principle: they are linked via network edges, not merged into one story.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
