% ============================================================================
% CONSTRAINT STORY: ihl_distinction_proportionality__outcomes_based_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ihl_distinction_proportionality__outcomes_based_reading, []).

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
 *   constraint_id: ihl_distinction_proportionality__outcomes_based_reading
 *   human_readable: IHL Outcomes-Based Compliance for Autonomous Weapons
 *   domain: international_humanitarian_law/military_ethics/technology_governance
 *
 * SUMMARY:
 *   This constraint story models the outcomes-based reading of the IHL
 *   distinction and proportionality kernel: the claim that autonomous weapon
 *   systems (LAWS) comply with International Humanitarian Law if their
 *   technical performance meets or exceeds human operator benchmarks. The
 *   reading is presented as technology-neutralâlaw governs effects, not
 *   meansâand is contested by human-agency and categorical-prohibition
 *   readings. It is authored as a Tangled Rope because it carries a genuine
 *   coordination function (legal clarity for cross-border military
 *   operations, treaty continuity without renegotiation) while simultaneously
 *   extracting interpretive authority from humanitarian law custodians and
 *   transferring lethal risk to civilian populations.
 *
 * KEY AGENTS:
 *   - state_militaries (agenda_setter/beneficiary, institutional/constrained): advance the reading to unlock LAWS deployment
 *   - defense_contractors (beneficiary, powerful/mobile): gain legal markets for autonomous systems
 *   - humanitarian_law_custodians (payer, institutional/constrained): lose interpretive authority to technical metrics
 *   - civilian_populations (payer, powerless/trapped): bear algorithmic targeting risk without representation in threshold-setting
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ihl_distinction_proportionality__outcomes_based_reading, 0.56).
domain_priors:suppression_score(ihl_distinction_proportionality__outcomes_based_reading, 0.48).
domain_priors:theater_ratio(ihl_distinction_proportionality__outcomes_based_reading, 0.32).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ihl_distinction_proportionality__outcomes_based_reading, extractiveness, 0.56).
narrative_ontology:constraint_metric(ihl_distinction_proportionality__outcomes_based_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(ihl_distinction_proportionality__outcomes_based_reading, theater_ratio, 0.32).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ihl_distinction_proportionality__outcomes_based_reading, accessibility_collapse, 0.44).
narrative_ontology:constraint_metric(ihl_distinction_proportionality__outcomes_based_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ihl_distinction_proportionality__outcomes_based_reading, tangled_rope).
narrative_ontology:human_readable(ihl_distinction_proportionality__outcomes_based_reading, "IHL Outcomes-Based Compliance for Autonomous Weapons").
narrative_ontology:topic_domain(ihl_distinction_proportionality__outcomes_based_reading, "international_humanitarian_law/military_ethics/technology_governance").

domain_priors:requires_active_enforcement(ihl_distinction_proportionality__outcomes_based_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ihl_distinction_proportionality__outcomes_based_reading, '5ce4f043-e630-4aad-b5cf-538b7da44012').
narrative_ontology:cs_kernel_codification('5ce4f043-e630-4aad-b5cf-538b7da44012', formalized).
narrative_ontology:cs_authority_grounding('5ce4f043-e630-4aad-b5cf-538b7da44012', lineage).
narrative_ontology:cs_interpretation_layer_present('5ce4f043-e630-4aad-b5cf-538b7da44012').
narrative_ontology:cs_reading_relation('5ce4f043-e630-4aad-b5cf-538b7da44012', ihl_distinction_proportionality__human_agency_reading, forecloses).
narrative_ontology:cs_reading_relation('5ce4f043-e630-4aad-b5cf-538b7da44012', ihl_distinction_proportionality__categorical_prohibition_reading, forecloses).
narrative_ontology:cs_axiom('5ce4f043-e630-4aad-b5cf-538b7da44012', foundational, technology_neutrality_of_ihl).
narrative_ontology:cs_axiom_status(technology_neutrality_of_ihl, holdable).
narrative_ontology:cs_axiom_grounding('5ce4f043-e630-4aad-b5cf-538b7da44012', technology_neutrality_of_ihl, conventional).
narrative_ontology:cs_axiom('5ce4f043-e630-4aad-b5cf-538b7da44012', foundational, performance_equivalence_satisfies_obligation).
narrative_ontology:cs_axiom_status(performance_equivalence_satisfies_obligation, holdable).
narrative_ontology:cs_axiom_grounding('5ce4f043-e630-4aad-b5cf-538b7da44012', performance_equivalence_satisfies_obligation, empirically_contingent).
narrative_ontology:cs_reference_frame('5ce4f043-e630-4aad-b5cf-538b7da44012', technology_neutral_outcome_standard).
narrative_ontology:cs_drift_state('5ce4f043-e630-4aad-b5cf-538b7da44012', contemporary_laws_debate, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('5ce4f043-e630-4aad-b5cf-538b7da44012', '').
narrative_ontology:cs_kernel_id(ihl_distinction_proportionality__outcomes_based_reading, ihl_distinction_proportionality).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ihl_distinction_proportionality__outcomes_based_reading, state_militaries).
narrative_ontology:constraint_beneficiary(ihl_distinction_proportionality__outcomes_based_reading, defense_contractors).
narrative_ontology:constraint_victim(ihl_distinction_proportionality__outcomes_based_reading, humanitarian_law_custodians).
narrative_ontology:constraint_victim(ihl_distinction_proportionality__outcomes_based_reading, civilian_populations).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(ihl_distinction_proportionality__outcomes_based_reading, military_operators).
narrative_ontology:constraint_vindicates(ihl_distinction_proportionality__outcomes_based_reading, technology_neutrality_principle).
narrative_ontology:constraint_vindicates(ihl_distinction_proportionality__outcomes_based_reading, performance_based_compliance).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Advance an interpretive framework that treats IHL distinction and proportionality as satisfied by measurable system performance, enabling lawful deployment of lethal autonomous weapons. They set compliance thresholds through national policy and multilateral negotiation, gaining operational latitude while maintaining legal legitimacy.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__outcomes_based_reading, state_militaries, agenda_setter,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(ihl_distinction_proportionality__outcomes_based_reading, state_militaries, beneficiary).

% Design, market, and sell autonomous weapon systems whose legality depends on passing technical distinction and proportionality benchmarks. The outcomes-based reading converts their engineering outputs into legal licenses, opening procurement channels that human-agency readings would close.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__outcomes_based_reading, defense_contractors, beneficiary,
    powerful, biographical, mobile, global).

% Receive rules of engagement that authorize lethal action by autonomous systems once technical validation is complete, reducing decision latency and cognitive burden in high-tempo operations. Their tactical choices are bounded by command-directed system authorization.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__outcomes_based_reading, military_operators, beneficiary,
    organized, immediate, constrained, national).

% ICRC, academic experts, and treaty bodies whose interpretive authority over the moral content of distinction and proportionality is displaced by technical performance metrics. Their objections are reframed as technologically illiterate or politically motivated within the outcomes-based discourse.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__outcomes_based_reading, humanitarian_law_custodians, payer,
    institutional, civilizational, constrained, global).

% Inhabit conflict zones where lethal targeting decisions are made by algorithmic systems validated against statistical performance benchmarks rather than individual human moral judgment. They cannot opt out of the risk allocation imposed by metric-driven targeting errors.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__outcomes_based_reading, civilian_populations, payer,
    powerless, immediate, trapped, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a common, measurable legal standard for lawful autonomous weapon use across diverse national military systems, allowing states to field LAWS without renegotiating IHL treaties.
% TRANSFER_FUNCTION: Transfers interpretive authority over lethal force lawfulness from humanitarian legal experts and human operators to technical verification regimes and system performance benchmarks; transfers downside risk from military decision-makers to civilian populations in target areas.
% ABSENT_VOICES: Civilian populations in conflict zones are not represented in threshold-setting for distinction and proportionality metrics; individual soldiers who would exercise moral judgment but are overridden by system authorization are structurally excluded from the legal conversation.
% DISAPPEARANCE_RATIONALE: If this reading disappeared, states deploying LAWS would lose their primary legal justification and would need to either return to human-operator benchmarks, accept categorical prohibitions, or halt autonomous programs; defense contractors would face collapsed legal markets; IHL discourse would recentre on human agency rather than technical optimization.
% FOUNDING_PROBLEM: How to maintain IHL compliance when weapon systems can target and apply force without real-time human deliberation.
% FOUNDING_PROBLEM_CORROBORATION: State militaries and defense contractors attest the problem requires an outcomes-based solution to preserve military effectiveness. Humanitarian law custodians and human rights organizations attest the problem is better solved by preserving human agency or prohibiting LAWS outright; no neutral party outside the benefiting coalition corroborates that outcomes-based compliance is the necessary or correct evolution of IHL.
narrative_ontology:disappearance_verdict(ihl_distinction_proportionality__outcomes_based_reading, world_rearranges).
narrative_ontology:founding_problem_status(ihl_distinction_proportionality__outcomes_based_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ihl_distinction_proportionality__outcomes_based_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(ihl_distinction_proportionality__outcomes_based_reading, 'none', 1).
narrative_ontology:epsilon_provenance(ihl_distinction_proportionality__outcomes_based_reading, 0.56, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ihl_distinction_proportionality__outcomes_based_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(ihl_distinction_proportionality__outcomes_based_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(ihl_distinction_proportionality__outcomes_based_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.56) is moderate: the reading genuinely coordinates state military behavior around a shared standard, but the standard is built to permit a weapons class that generates procurement revenue and operational advantage. Suppression (0.48) reflects the interpretive work required to frame human-agency requirements as technologically biased; it is not raw violence but structural suppression of alternative legal meanings. Theater ratio (0.32) captures the growing performative dimension of technical compliance assessmentâmetrics that are gameable and audited by parties with financial stakes. Accessibility collapse (0.44) is moderate: the technology-neutral framing makes human-agency readings appear retrograde, but they remain intellectually available. Resistance (0.62) is substantial, driven by ICRC, civil society, and the Group of Governmental Experts.
 *
 * PERSPECTIVAL GAP:
 *   The state military seat experiences this constraint as genuine coordination: a legal bridge that prevents IHL from becoming an obstacle to necessary military modernization. The humanitarian law custodian seat experiences it as extraction: their professional expertise in the moral content of distinction and proportionality is treated as sentimentality, while their institutional role in treaty interpretation is bypassed by technical committees. The civilian population seat experiences pure risk transfer. These divergences are structurally derived from the beneficiary/victim declarations and exit modulations.
 *
 * DIRECTIONALITY LOGIC:
 *   State militaries and defense contractors are structural beneficiaries: the reading subsidizes their operational and commercial objectives (low d, damped effective extraction). Humanitarian law custodians and civilian populations are structural targets: the reading extracts interpretive authority from the former and transfers lethal risk to the latter (high d, amplified effective extraction). The directionality asymmetry is reinforced by scopeâglobal spatial scope makes verification of technical metrics difficult and amplifies effective extraction for trapped civilian populations.
 *
 * MANDATROPHY ANALYSIS:
 *   The coordination function is genuine: without some interpretive standard, LAWS deployment would occur in a legal vacuum or require impossible treaty renegotiation. However, the reading prevents mislabeling as pure coordination because (1) it was advanced by the parties with the most to gain from LAWS legality, (2) the compliance thresholds are not independently set by neutral parties, and (3) the human-agency reading offers an alternative coordination path (human-machine teaming) that the outcomes-based reading suppresses. The Tangled Rope classification captures this hybridity rather than allowing the coordination story to mask the extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    technical_metric_validity,
    'Can distinction and proportionality be validly operationalized as technical performance metrics, or do these metrics necessarily omit the contextual moral judgment that IHL traditionally requires?',
    'Comparative empirical analysis of civilian casualty rates and targeting error types between human-operated and autonomous systems in controlled or reconstructed scenarios, paired with legal review of whether metric capture matches treaty intent.',
    'If metrics are shown to omit legally relevant moral context, the outcomes-based reading collapses toward a Snare (coordination story as cover for extraction); if validated, the Tangled Rope classification tightens toward genuine coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(technical_metric_validity, empirical, 'Whether technical performance proxies validly capture IHL obligations.').

omega_variable(
    technology_neutrality_as_construct,
    'Is the technology-neutral framing (law governs outcomes, not means) a genuine axiom of IHL treaty interpretation, or a constructed narrative that advantages militaries with advanced technical measurement capacity?',
    'Historical treaty interpretation analysis examining whether IHL instruments have ever treated the character of the weapon or decision-agent as irrelevant to legality, versus whether this principle is novel and contested.',
    'If technology-neutrality is a recent construct, the reading''s authority grounding shifts from lineage to extraction, reclassifying the constraint''s authority structure and raising its effective extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(technology_neutrality_as_construct, conceptual, 'Whether technology-neutrality is inherited or constructed.').

omega_variable(
    kernel_reading_foreclosure,
    'Does the outcomes-based reading logically foreclose the human-agency and categorical-prohibition readings within a single legal framework, or can they coexist as regulatory tiers?',
    'Jurisprudential analysis of whether national or international legal instruments can simultaneously hold that machines alone may satisfy IHL and that human moral judgment is irreducibly required.',
    'If genuine foreclosure holds, the kernel readings are mutually exclusive legal standards and the outcomes-based reading''s adoption structurally eliminates its siblings; if coexistence is possible, the relation should be coexists_with and the constraint''s suppression score should be revised downward.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_foreclosure, conceptual, 'Logical relationship between sibling kernel readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ihl_distinction_proportionality__outcomes_based_reading, 0, 12).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ihl_outcomes_tr_t0, ihl_distinction_proportionality__outcomes_based_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(ihl_outcomes_tr_t3, ihl_distinction_proportionality__outcomes_based_reading, theater_ratio, 3, 0.2).
narrative_ontology:measurement(ihl_outcomes_tr_t6, ihl_distinction_proportionality__outcomes_based_reading, theater_ratio, 6, 0.24).
narrative_ontology:measurement(ihl_outcomes_tr_t9, ihl_distinction_proportionality__outcomes_based_reading, theater_ratio, 9, 0.28).
narrative_ontology:measurement(ihl_outcomes_tr_t12, ihl_distinction_proportionality__outcomes_based_reading, theater_ratio, 12, 0.32).

% Extraction over time
narrative_ontology:measurement(ihl_outcomes_be_t0, ihl_distinction_proportionality__outcomes_based_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(ihl_outcomes_be_t3, ihl_distinction_proportionality__outcomes_based_reading, base_extractiveness, 3, 0.44).
narrative_ontology:measurement(ihl_outcomes_be_t6, ihl_distinction_proportionality__outcomes_based_reading, base_extractiveness, 6, 0.48).
narrative_ontology:measurement(ihl_outcomes_be_t9, ihl_distinction_proportionality__outcomes_based_reading, base_extractiveness, 9, 0.52).
narrative_ontology:measurement(ihl_outcomes_be_t12, ihl_distinction_proportionality__outcomes_based_reading, base_extractiveness, 12, 0.56).

% Suppression requirement over time
narrative_ontology:measurement(ihl_outcomes_su_t0, ihl_distinction_proportionality__outcomes_based_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(ihl_outcomes_su_t3, ihl_distinction_proportionality__outcomes_based_reading, suppression_requirement, 3, 0.32).
narrative_ontology:measurement(ihl_outcomes_su_t6, ihl_distinction_proportionality__outcomes_based_reading, suppression_requirement, 6, 0.38).
narrative_ontology:measurement(ihl_outcomes_su_t9, ihl_distinction_proportionality__outcomes_based_reading, suppression_requirement, 9, 0.43).
narrative_ontology:measurement(ihl_outcomes_su_t12, ihl_distinction_proportionality__outcomes_based_reading, suppression_requirement, 12, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ihl_distinction_proportionality__outcomes_based_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(ihl_distinction_proportionality__outcomes_based_reading, human_agency_reading).
narrative_ontology:affects_constraint(ihl_distinction_proportionality__outcomes_based_reading, categorical_prohibition_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the ihl_distinction_proportionality kernel, decomposed from the colloquial label 'IHL distinction and proportionality requirements' which conflates outcome-based, human-agency, and categorical-prohibition readings. Each reading carries a distinct epsilon and stakeholder structure.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
