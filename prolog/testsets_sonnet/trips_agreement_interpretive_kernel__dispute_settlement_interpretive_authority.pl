% ============================================================================
% CONSTRAINT STORY: trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, []).

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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority
 *   human_readable: WTO Dispute Settlement Interpretive Authority Over TRIPS Text
 *   domain: international_trade_law/public_health_policy/intellectual_property
 *
 * SUMMARY:
 *   This story isolates the meta-level constraint within the TRIPS kernel
 *   contest: not what the text says about compulsory licensing or patent
 *   scope (those are the sibling readings), but who holds binding authority
 *   to say what the text means when parties disagree, and what enforces that
 *   authority. Dispute panels were built as a neutral multilateral forum;
 *   over three decades their accumulated jurisprudence has functioned as a
 *   one-way ratchet narrowing public-health flexibilities in practice even
 *   where the underlying text (per the flexibility reading) supports them,
 *   while the 2019 U.S. blockade of Appellate Body appointments has pushed
 *   enforcement back toward the bilateral power asymmetry the mechanism was
 *   built to replace.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, 0.68).
domain_priors:suppression_score(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, 0.71).
domain_priors:theater_ratio(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, 0.44).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, extractiveness, 0.68).
narrative_ontology:constraint_metric(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, theater_ratio, 0.44).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, tangled_rope).
narrative_ontology:human_readable(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, "WTO Dispute Settlement Interpretive Authority Over TRIPS Text").
narrative_ontology:topic_domain(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, "international_trade_law/public_health_policy/intellectual_property").

domain_priors:requires_active_enforcement(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, '3223b394-ad6f-474b-9690-ad415d5e7e53').
narrative_ontology:cs_kernel_codification('3223b394-ad6f-474b-9690-ad415d5e7e53', formalized).
narrative_ontology:cs_authority_grounding('3223b394-ad6f-474b-9690-ad415d5e7e53', lineage).
narrative_ontology:cs_interpretation_layer_present('3223b394-ad6f-474b-9690-ad415d5e7e53').
narrative_ontology:cs_reading_relation('3223b394-ad6f-474b-9690-ad415d5e7e53', trips_agreement_interpretive_kernel__strong_exclusivity_reading, influences).
narrative_ontology:cs_reading_relation('3223b394-ad6f-474b-9690-ad415d5e7e53', trips_agreement_interpretive_kernel__public_health_flexibility_reading, influences).
narrative_ontology:cs_axiom('3223b394-ad6f-474b-9690-ad415d5e7e53', foundational, binding_multilateral_adjudication_supersedes_unilateral_action).
narrative_ontology:cs_axiom_status(binding_multilateral_adjudication_supersedes_unilateral_action, holdable).
narrative_ontology:cs_axiom_grounding('3223b394-ad6f-474b-9690-ad415d5e7e53', binding_multilateral_adjudication_supersedes_unilateral_action, conventional).
narrative_ontology:cs_axiom('3223b394-ad6f-474b-9690-ad415d5e7e53', foundational, accumulated_panel_precedent_constitutes_authoritative_treaty_meaning).
narrative_ontology:cs_axiom_status(accumulated_panel_precedent_constitutes_authoritative_treaty_meaning, holdable).
narrative_ontology:cs_axiom_grounding('3223b394-ad6f-474b-9690-ad415d5e7e53', accumulated_panel_precedent_constitutes_authoritative_treaty_meaning, instrumental).
narrative_ontology:cs_reference_frame('3223b394-ad6f-474b-9690-ad415d5e7e53', uruguay_round_rules_based_multilateralism).
narrative_ontology:cs_drift_state('3223b394-ad6f-474b-9690-ad415d5e7e53', post_appellate_body_collapse, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('3223b394-ad6f-474b-9690-ad415d5e7e53', '').
narrative_ontology:cs_kernel_id(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, trips_agreement_interpretive_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, originator_pharmaceutical_exporters).
narrative_ontology:constraint_beneficiary(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, developed_state_trade_negotiators).
narrative_ontology:constraint_victim(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, generic_manufacturing_states).
narrative_ontology:constraint_victim(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, low_income_patient_populations).
narrative_ontology:constraint_vindicates(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, multilateral_rules_based_trade_order).
narrative_ontology:constraint_vindicates(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, single_undertaking_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Research-based pharmaceutical firms and their home-state trade delegations rely on panel rulings to police compulsory licensing and parallel imports abroad. They file complaints, submit amicus briefs, and lobby for retaliation authorization when a member state's TRIPS flexibility use is read as exceeding narrow exceptions. Their exit option is arbitrage: they can shift manufacturing and litigation strategy across jurisdictions while the interpretive machinery itself remains stable and available to them.
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, originator_pharmaceutical_exporters, beneficiary,
    organized, generational, arbitrage, global).

% Control panel composition proposals, select which disputes reach adjudication, and have historically blocked Appellate Body appointments to freeze unfavorable jurisprudence in place. They administer the retaliation-authorization process and can escalate bilaterally when the multilateral forum stalls, giving them a fallback the constraint's other parties lack.
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, developed_state_trade_negotiators, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, developed_state_trade_negotiators, beneficiary).

% States with generic pharmaceutical production capacity (e.g. India, Brazil) must defend compulsory licensing decisions against dispute threats and the chilling effect of anticipated retaliation. Losing or settling a dispute forces domestic legal amendment under threat of trade sanctions on unrelated export sectors. Their exit is constrained: withdrawal from WTO membership would cost far more than compliance, so they absorb the interpretive ruling even when it narrows a flexibility they believe the text grants.
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, generic_manufacturing_states, payer,
    moderate, biographical, constrained, national).

% Patients dependent on generic medicines bear the downstream cost when a panel ruling narrows compulsory licensing or parallel import practice, delaying access to affordable treatment. They have no standing before the panel, no representation in the dispute, and no capacity to exit the health system or the jurisdiction whose law is being adjudicated.
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, low_income_patient_populations, payer,
    powerless, biographical, trapped, national).

% Convenes panels, adopts or rejects panel and (formerly) Appellate Body reports, and authorizes retaliation. Since the U.S. blocked Appellate Body appointments starting 2019, appeals into a legal void are effectively terminal, so panel rulings increasingly stand unreviewed or member states resort to ad hoc arbitration (Article 25) or bilateral settlement outside the multilateral track — a structural drift the constraint itself catalyzed.
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, wto_dispute_settlement_body, agenda_setter,
    institutional, generational, analytical, global).

% NGOs and public-health coalitions who would argue for reading TRIPS flexibilities broadly are not parties to state-to-state disputes and can only submit unsolicited amicus briefs, which panels are not obligated to consider. They watch interpretive precedent accumulate against the flexibility reading without a formal seat.
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, civil_society_health_advocates, excluded,
    organized, generational, trapped, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single, binding forum to resolve disputed readings of ambiguous TRIPS text so that 164 member states are not each unilaterally interpreting and retaliating over intellectual property obligations — a genuine alternative to unconstrained trade war over IP disagreements.
% TRANSFER_FUNCTION: Moves interpretive authority over ambiguous treaty language away from the negotiating states that drafted the ambiguity and toward panel jurisprudence, and moves the practical cost of that jurisprudence's accumulated bias toward generic-manufacturing states and the patients who depend on them, while insulating panel-favored exporting interests from having to renegotiate the text itself.
% ABSENT_VOICES: Patients and public-health NGOs have no standing before panels; only member states are parties. Civil society amicus submissions are formally permitted but not binding on panel reasoning, so the population most affected by interpretive drift toward the exclusivity reading is structurally absent from the room where it is decided.
% DISAPPEARANCE_RATIONALE: If binding panel interpretive authority vanished, TRIPS text would revert to unresolved textual ambiguity adjudicated only through diplomatic pressure and unilateral retaliation (as under GATT's weaker predecessor system) — states would renegotiate flexibilities bilaterally, litigation strategy by originator firms would lose its multilateral leverage point, and generic-manufacturing states would face fewer binding constraints on compulsory licensing absent a forum to challenge them.
% FOUNDING_PROBLEM: The 1994 Uruguay Round needed a credible, rules-based mechanism to enforce the newly harmonized TRIPS obligations, replacing the toothless GATT dispute process and preventing powerful states from imposing unilateral Section 301-style trade sanctions over IP disagreements without multilateral review.
% FOUNDING_PROBLEM_CORROBORATION: Trade law scholars and the WTO Secretariat attest the original problem (unchecked unilateral retaliation) was substantially solved through the 1995-2019 period. Independent assessments from UNCTAD, the WHO, and academic trade-law literature outside the beneficiary states corroborate that since the Appellate Body's collapse, bilateral power substitution has partially reconstituted the exact pre-1995 problem the mechanism was built to end — the founding problem is dead in form but reviving in substance, a status the originator-exporter states have not acknowledged.
narrative_ontology:disappearance_verdict(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, world_rearranges).
narrative_ontology:founding_problem_status(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, 'none', 1).
narrative_ontology:epsilon_provenance(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, 0.68, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises across the interval (0.38 to 0.68) as panel jurisprudence accumulates precedent favoring the exclusivity reading in contested cases (Canada-Pharmaceutical Patents, the Doha Declaration's contested implementation), narrowing the practical scope of flexibilities that generic-manufacturing states rely on. Theater ratio climbs sharply post-2019 (0.28 to 0.44) because with the Appellate Body non-functional, panel proceedings and their formal multilateral framing increasingly mask the reality that outcomes are settled through bilateral pressure, unilateral retaliation threats, and negotiated non-appeal rather than binding multilateral review. Suppression tracks the same trajectory: the credible threat of authorized trade retaliation is what makes panel rulings binding in practice, and that threat has intensified even as the forum's legitimacy has eroded.
 *
 * DIRECTIONALITY LOGIC:
 *   Originator exporters and developed-state negotiators sit near the beneficiary end: they set panel agendas, have arbitrage-grade exit (can pursue bilateral leverage when multilateral forums stall), and collect the interpretive stability that protects existing jurisprudence. Generic-manufacturing states and low-income patient populations sit near the target end: they bear the compliance cost of adverse rulings and have constrained-to-trapped exit, since withdrawal from the WTO trading system is not a real option and patients have no standing at all.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification depends on distinguishing the genuine coordination function (a single binding forum beats unconstrained unilateral trade retaliation over IP disputes) from the asymmetric extraction that has grown inside it (jurisprudential drift toward exclusivity, enforced through the same retaliation mechanism the forum was meant to discipline). Calling this a pure snare would erase the real value multilateral adjudication has provided in preventing worse unilateral outcomes; calling it a pure rope would erase the documented pattern of interpretive capture and the post-2019 reversion to bilateral power substitution. Tangled rope holds both facts without collapsing either.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    interpretive_authority_vs_bilateral_substitution,
    'Does binding panel interpretive authority over TRIPS still exist in practice after the Appellate Body''s 2019 collapse, or has it been substantively replaced by bilateral power bargaining that merely retains the procedural form of WTO dispute settlement?',
    'Track the proportion of TRIPS disputes since 2019 resolved through completed panel-and-Appellate-Body review versus those settled via Article 25 arbitration, bilateral negotiation, or unappealed panel reports left in legal limbo ("appeals into the void").',
    'If bilateral substitution dominates, this constraint has already drifted from tangled_rope toward piton (the multilateral form persists theatrically while the actual enforcement mechanism has shifted elsewhere) or toward a reconstituted snare wielded by whichever state has greater unilateral retaliation capacity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(interpretive_authority_vs_bilateral_substitution, empirical, 'Whether multilateral interpretive authority is functionally intact or has been substantively replaced by bilateral leverage since 2019.').

omega_variable(
    precedent_ratchet_directionality,
    'Is the documented drift of panel jurisprudence toward the exclusivity reading a product of the legal merits of disputed cases, or a structural artifact of which states have the resources to litigate and appeal repeatedly?',
    'Compare case outcomes controlling for the litigating parties'' legal resources and repeat-player status against a baseline of textually equivalent disputes brought by resource-poor states.',
    'If resource asymmetry drives outcomes independent of legal merit, the interpretive authority reading is closer to snare (extraction masked as neutral adjudication); if outcomes track textual merit regardless of resources, the tangled_rope coordination function is doing more real work than the extraction critique credits.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(precedent_ratchet_directionality, conceptual, 'Whether jurisprudential drift reflects legal merit or litigant resource asymmetry.').

omega_variable(
    kernel_framing_under_determination,
    'Is the meta-level ''who interprets'' constraint genuinely separable from the substantive ''what does the text mean'' constraints, or does authoring them as three separate stories understate how tightly interpretive authority and substantive outcome are coupled in a common-law-like precedent system?',
    'Assess whether a change in panel composition or forum authority (this reading) predicts a change in substantive rulings (the sibling readings) independent of any change in the treaty text itself.',
    'If interpretive-authority changes reliably predict substantive outcome shifts, the three-way decomposition remains structurally sound (ε-invariance preserved) but the reading_relations should weight ''influences'' toward both siblings more heavily than a purely orthogonal meta-constraint would suggest.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_framing_under_determination, conceptual, 'Whether the interpretive-authority reading is truly orthogonal to the two substantive readings or exerts strong causal influence over which substantive reading prevails in practice.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, 1995, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(trip_tr_t1995, trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, theater_ratio, 1995, 0.12).
narrative_ontology:measurement(trip_tr_t2001, trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, theater_ratio, 2001, 0.15).
narrative_ontology:measurement(trip_tr_t2005, trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, theater_ratio, 2005, 0.2).
narrative_ontology:measurement(trip_tr_t2013, trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, theater_ratio, 2013, 0.28).
narrative_ontology:measurement(trip_tr_t2019, trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, theater_ratio, 2019, 0.38).
narrative_ontology:measurement(trip_tr_t2024, trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, theater_ratio, 2024, 0.44).

% Extraction over time
narrative_ontology:measurement(trip_be_t1995, trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, base_extractiveness, 1995, 0.38).
narrative_ontology:measurement(trip_be_t2001, trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, base_extractiveness, 2001, 0.42).
narrative_ontology:measurement(trip_be_t2005, trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, base_extractiveness, 2005, 0.5).
narrative_ontology:measurement(trip_be_t2013, trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, base_extractiveness, 2013, 0.58).
narrative_ontology:measurement(trip_be_t2019, trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, base_extractiveness, 2019, 0.63).
narrative_ontology:measurement(trip_be_t2024, trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, base_extractiveness, 2024, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(trip_su_t1995, trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, suppression_requirement, 1995, 0.45).
narrative_ontology:measurement(trip_su_t2001, trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, suppression_requirement, 2001, 0.52).
narrative_ontology:measurement(trip_su_t2005, trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, suppression_requirement, 2005, 0.58).
narrative_ontology:measurement(trip_su_t2013, trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, suppression_requirement, 2013, 0.64).
narrative_ontology:measurement(trip_su_t2019, trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, suppression_requirement, 2019, 0.7).
narrative_ontology:measurement(trip_su_t2024, trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, suppression_requirement, 2024, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, 0.1).
narrative_ontology:affects_constraint(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, trips_agreement_interpretive_kernel__strong_exclusivity_reading).
narrative_ontology:affects_constraint(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, trips_agreement_interpretive_kernel__public_health_flexibility_reading).

% DUAL FORMULATION NOTE:
% This story is the meta-level (interpretive authority / enforcement) member of a three-story TRIPS kernel family. The two substantive siblings — strong_exclusivity_reading and public_health_flexibility_reading — each claim a different reading of what the treaty text means; this story claims a reading of who gets to authoritatively settle that disagreement and how the settlement is enforced. ε differs sharply by design: this story's extraction concerns capture of the adjudicative process itself, not the substantive scope of patent flexibilities. All three stories are linked; degradation in this story's purity (e.g. bilateral substitution replacing multilateral adjudication) is expected to propagate pressure toward whichever substantive reading the currently-dominant enforcement actor favors.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
