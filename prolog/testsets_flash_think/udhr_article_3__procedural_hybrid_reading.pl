% ============================================================================
% CONSTRAINT STORY: udhr_article_3__procedural_hybrid_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_udhr_article_3__procedural_hybrid_reading, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: udhr_article_3__procedural_hybrid_reading
 *   human_readable: UDHR Article 3: Procedural Hybrid Reading (Due Process, Torture Prohibition)
 *   domain: constitutional_law/human_rights/political_philosophy
 *
 * SUMMARY:
 *   This constraint represents a 'procedural hybrid' reading of Article 3 of
 *   the Universal Declaration of Human Rights (UDHR), which guarantees
 *   fundamental due process protections such as habeas corpus and the
 *   prohibition of torture, without explicitly resolving the broader
 *   substantive debate between negative liberty (freedom from state
 *   interference) and positive entitlement (state provision of welfare). It
 *   functions as a foundational international legal standard, limiting state
 *   arbitrary power and providing a baseline for individual protection. The
 *   metrics reflect its moderate extractiveness on state power and the
 *   ongoing, though often resisted, enforcement of these principles.
 *
 * KEY AGENTS:
 *   - individuals_seeking_due_process: Primary beneficiary (powerless/trapped) — receives protection
 *   - states_seeking_unfettered_power: Primary payer (institutional/constrained) — bears limits on arbitrary action
 *   - international_legal_bodies: Agenda setter (institutional/analytical) — interprets and enforces
 *   - national_judiciaries: Agenda setter (institutional/constrained) — applies domestic law derived from Article 3
 *   - human_rights_advocates: Beneficiary (organized/mobile) — uses Article 3 as a tool for advocacy
 *   - authoritarian_regimes: Excluded (institutional/identity_locked) — rejects or violates the constraint
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(udhr_article_3__procedural_hybrid_reading, 0.45).
domain_priors:suppression_score(udhr_article_3__procedural_hybrid_reading, 0.55).
domain_priors:theater_ratio(udhr_article_3__procedural_hybrid_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(udhr_article_3__procedural_hybrid_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(udhr_article_3__procedural_hybrid_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(udhr_article_3__procedural_hybrid_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(udhr_article_3__procedural_hybrid_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(udhr_article_3__procedural_hybrid_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(udhr_article_3__procedural_hybrid_reading, rope).
narrative_ontology:human_readable(udhr_article_3__procedural_hybrid_reading, "UDHR Article 3: Procedural Hybrid Reading (Due Process, Torture Prohibition)").
narrative_ontology:topic_domain(udhr_article_3__procedural_hybrid_reading, "constitutional_law/human_rights/political_philosophy").

domain_priors:requires_active_enforcement(udhr_article_3__procedural_hybrid_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(udhr_article_3__procedural_hybrid_reading, '46d2519b-a191-47b6-9379-d609f8f83f67').
narrative_ontology:cs_kernel_codification('46d2519b-a191-47b6-9379-d609f8f83f67', fixed_text).
narrative_ontology:cs_authority_grounding('46d2519b-a191-47b6-9379-d609f8f83f67', lineage).
narrative_ontology:cs_interpretation_layer_present('46d2519b-a191-47b6-9379-d609f8f83f67').
narrative_ontology:cs_reading_relation('46d2519b-a191-47b6-9379-d609f8f83f67', udhr_article_3__negative_liberty_reading, coexists_with).
narrative_ontology:cs_reading_relation('46d2519b-a191-47b6-9379-d609f8f83f67', udhr_article_3__positive_entitlement_reading, coexists_with).
narrative_ontology:cs_axiom('46d2519b-a191-47b6-9379-d609f8f83f67', foundational, procedural_justice_is_foundational).
narrative_ontology:cs_axiom_status(procedural_justice_is_foundational, holdable).
narrative_ontology:cs_axiom_grounding('46d2519b-a191-47b6-9379-d609f8f83f67', procedural_justice_is_foundational, deontological).
narrative_ontology:cs_axiom('46d2519b-a191-47b6-9379-d609f8f83f67', foundational, torture_is_categorically_prohibited).
narrative_ontology:cs_axiom_status(torture_is_categorically_prohibited, holdable).
narrative_ontology:cs_axiom_grounding('46d2519b-a191-47b6-9379-d609f8f83f67', torture_is_categorically_prohibited, deontological).
narrative_ontology:cs_reference_frame('46d2519b-a191-47b6-9379-d609f8f83f67', post_wwii_consensus_on_dignity).
narrative_ontology:cs_drift_state('46d2519b-a191-47b6-9379-d609f8f83f67', contemporary_human_rights_era, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('46d2519b-a191-47b6-9379-d609f8f83f67', '').
narrative_ontology:cs_kernel_id(udhr_article_3__procedural_hybrid_reading, udhr_article_3).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(udhr_article_3__procedural_hybrid_reading, individuals_seeking_due_process).
narrative_ontology:constraint_beneficiary(udhr_article_3__procedural_hybrid_reading, human_rights_advocates).
narrative_ontology:constraint_beneficiary(udhr_article_3__procedural_hybrid_reading, international_legal_bodies).
narrative_ontology:constraint_victim(udhr_article_3__procedural_hybrid_reading, states_seeking_unfettered_power).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Directly benefits from protections against arbitrary detention, torture, and other forms of cruel, inhuman, or degrading treatment. Their ability to exit abusive situations is often non-existent, making these procedural guarantees their primary defense.
narrative_ontology:constraint_stakeholder(udhr_article_3__procedural_hybrid_reading, individuals_seeking_due_process, beneficiary,
    powerless, immediate, trapped, universal).

% Bears the cost of limitations on their sovereign power, particularly the inability to arbitrarily detain, torture, or deny fair process. While they can resist, outright rejection carries significant international reputational and legal costs.
narrative_ontology:constraint_stakeholder(udhr_article_3__procedural_hybrid_reading, states_seeking_unfettered_power, payer,
    institutional, generational, constrained, global).

% Utilizes Article 3 as a foundational legal and moral standard in their work to monitor, report on, and challenge human rights abuses. The clarity of procedural guarantees provides a strong basis for their advocacy.
narrative_ontology:constraint_stakeholder(udhr_article_3__procedural_hybrid_reading, human_rights_advocates, beneficiary,
    organized, biographical, mobile, global).

% Interprets, monitors, and enforces Article 3 through conventions, tribunals, and reporting mechanisms. They are responsible for developing jurisprudence and holding states accountable, thereby administering the constraint.
narrative_ontology:constraint_stakeholder(udhr_article_3__procedural_hybrid_reading, international_legal_bodies, agenda_setter,
    institutional, generational, analytical, global).

% Applies Article 3 principles (often incorporated into national law) in domestic courts, reviewing executive actions for due process violations and prohibiting torture. Their independence is crucial for the constraint's effectiveness.
narrative_ontology:constraint_stakeholder(udhr_article_3__procedural_hybrid_reading, national_judiciaries, agenda_setter,
    institutional, generational, constrained, national).

% Often reject or systematically violate the procedural guarantees of Article 3, viewing them as an impediment to state control or national security. They are excluded from the legitimate discourse of international human rights law but continue to operate outside its norms.
narrative_ontology:constraint_stakeholder(udhr_article_3__procedural_hybrid_reading, authoritarian_regimes, excluded,
    institutional, biographical, identity_locked, regional).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a universal baseline for state conduct regarding life, liberty, and security of person, preventing arbitrary state action and fostering international legal coherence around minimum procedural justice standards.
% TRANSFER_FUNCTION: Transfers limits on arbitrary state power (e.g., detention without charge, torture) from states to individuals, ensuring a minimum standard of procedural justice and human dignity.
% ABSENT_VOICES: Authoritarian regimes and those who prioritize state security above individual procedural rights are structurally excluded from the consensus-building process of international human rights law; they would argue for greater state discretion and fewer external checks.
% DISAPPEARANCE_RATIONALE: If Article 3's procedural guarantees vanished overnight, states would have fewer checks on arbitrary power, leading to a significant increase in arbitrary detention, torture, and other human rights abuses, fundamentally reorganizing the relationship between states and individuals globally.
% FOUNDING_PROBLEM: Widespread arbitrary detention, torture, and summary executions by states, particularly in the aftermath of WWII, necessitating universal minimum standards for state conduct to protect fundamental human dignity.
% FOUNDING_PROBLEM_CORROBORATION: International human rights organizations (e.g., Amnesty International, Human Rights Watch), UN special rapporteurs, and national legal bodies consistently document ongoing violations and the necessity of these protections, corroborating that the founding problem remains a live global challenge.
narrative_ontology:disappearance_verdict(udhr_article_3__procedural_hybrid_reading, world_rearranges).
narrative_ontology:founding_problem_status(udhr_article_3__procedural_hybrid_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(udhr_article_3__procedural_hybrid_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(udhr_article_3__procedural_hybrid_reading, 'none', 1).
narrative_ontology:epsilon_provenance(udhr_article_3__procedural_hybrid_reading, 0.45, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(udhr_article_3__procedural_hybrid_reading_tests).
:- end_tests(udhr_article_3__procedural_hybrid_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness is moderate because while it limits state power, it doesn't impose extensive positive obligations. Suppression is moderate as states are actively constrained from arbitrary actions, but the constraint itself isn't suppressing alternatives to its own existence. Theater ratio is low, indicating that the procedural guarantees are generally taken seriously by signatory states, even if violations occur. Resistance is moderate, reflecting ongoing challenges from states, particularly in contexts of national security or political instability. The temporal measurements show a slight increase in extractiveness and suppression over time, reflecting the strengthening of international human rights law and its enforcement mechanisms, followed by a slight dip, possibly due to periods of increased state resistance.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of individuals and human rights advocates, Article 3 is a vital, if imperfect, shield against state abuse. From the perspective of states seeking unfettered power, it is an external imposition that limits their sovereignty. The engine's computation of per-seat classification will reflect this divergence, showing the constraint as a Rope for beneficiaries and a more extractive Tangled Rope or Snare for states that resist its limitations.
 *
 * DIRECTIONALITY LOGIC:
 *   Individuals are full beneficiaries, as the constraint directly protects their fundamental rights. States seeking unfettered power are targets, as the constraint limits their arbitrary actions. International legal bodies and national judiciaries act as agenda setters, enforcing the constraint. Human rights advocates benefit from having a clear standard to uphold. Authoritarian regimes are excluded, as their very nature often conflicts with these procedural guarantees.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    udhr_article_3_reading_identity,
    'Is this constraint a genuine ''procedural hybrid'' reading of UDHR Article 3, or is it primarily a ''negative liberty'' reading with procedural emphasis?',
    'Analysis of judicial interpretations and state practice: if interpretations consistently avoid mandating positive entitlements even when procedural guarantees are met, it leans more towards negative liberty.',
    'If primarily a negative liberty reading, the extractiveness on states might be slightly lower, as it demands less in terms of positive action, and the classification might lean more towards a pure Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(udhr_article_3_reading_identity, conceptual, 'Ambiguity in the precise scope of the ''procedural hybrid'' reading versus a ''negative liberty'' reading.').

omega_variable(
    enforcement_vs_rhetoric_gap,
    'To what extent do states'' rhetorical commitments to Article 3 procedural guarantees align with their actual enforcement practices?',
    'Empirical data on state compliance, judicial independence, and documented instances of arbitrary detention or torture, particularly in ''emergency'' contexts.',
    'If the gap is wide, the effective extractiveness on states is lower than stated, and the theater_ratio is higher, potentially shifting the classification towards a Piton or a more extractive Snare for individuals.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(enforcement_vs_rhetoric_gap, empirical, 'Gap between declared commitment and actual enforcement of procedural guarantees.').

omega_variable(
    substantive_entitlement_pressure,
    'Does the existence of procedural guarantees implicitly create pressure for states to provide substantive entitlements, even if not explicitly mandated by this reading?',
    'Longitudinal studies of legal development and social policy in states that strongly uphold Article 3 procedural rights: do they tend to expand welfare provisions over time?',
    'If such pressure is significant, this reading, while not explicitly mandating entitlements, indirectly ''influences'' the ''positive entitlement'' reading, potentially increasing its long-term impact on state resources.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(substantive_entitlement_pressure, empirical, 'Indirect pressure for substantive entitlements from procedural guarantees.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(udhr_article_3__procedural_hybrid_reading, 1948, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(udhr_tr_t1948, udhr_article_3__procedural_hybrid_reading, theater_ratio, 1948, 0.1).
narrative_ontology:measurement(udhr_tr_t1968, udhr_article_3__procedural_hybrid_reading, theater_ratio, 1968, 0.15).
narrative_ontology:measurement(udhr_tr_t1988, udhr_article_3__procedural_hybrid_reading, theater_ratio, 1988, 0.18).
narrative_ontology:measurement(udhr_tr_t2008, udhr_article_3__procedural_hybrid_reading, theater_ratio, 2008, 0.22).
narrative_ontology:measurement(udhr_tr_t2024, udhr_article_3__procedural_hybrid_reading, theater_ratio, 2024, 0.2).

% Extraction over time
narrative_ontology:measurement(udhr_be_t1948, udhr_article_3__procedural_hybrid_reading, base_extractiveness, 1948, 0.35).
narrative_ontology:measurement(udhr_be_t1968, udhr_article_3__procedural_hybrid_reading, base_extractiveness, 1968, 0.4).
narrative_ontology:measurement(udhr_be_t1988, udhr_article_3__procedural_hybrid_reading, base_extractiveness, 1988, 0.43).
narrative_ontology:measurement(udhr_be_t2008, udhr_article_3__procedural_hybrid_reading, base_extractiveness, 2008, 0.46).
narrative_ontology:measurement(udhr_be_t2024, udhr_article_3__procedural_hybrid_reading, base_extractiveness, 2024, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(udhr_su_t1948, udhr_article_3__procedural_hybrid_reading, suppression_requirement, 1948, 0.45).
narrative_ontology:measurement(udhr_su_t1968, udhr_article_3__procedural_hybrid_reading, suppression_requirement, 1968, 0.5).
narrative_ontology:measurement(udhr_su_t1988, udhr_article_3__procedural_hybrid_reading, suppression_requirement, 1988, 0.53).
narrative_ontology:measurement(udhr_su_t2008, udhr_article_3__procedural_hybrid_reading, suppression_requirement, 2008, 0.58).
narrative_ontology:measurement(udhr_su_t2024, udhr_article_3__procedural_hybrid_reading, suppression_requirement, 2024, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(udhr_article_3__procedural_hybrid_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(udhr_article_3__procedural_hybrid_reading, udhr_article_3__negative_liberty_reading).
narrative_ontology:affects_constraint(udhr_article_3__procedural_hybrid_reading, udhr_article_3__positive_entitlement_reading).
narrative_ontology:affects_constraint(udhr_article_3__procedural_hybrid_reading, iccpr_article_9_due_process).
narrative_ontology:affects_constraint(udhr_article_3__procedural_hybrid_reading, cat_torture_prohibition).

% DUAL FORMULATION NOTE:
% This constraint is one of three distinct readings of UDHR Article 3, each with different ε values and structural implications. This 'procedural hybrid' reading focuses on due process and torture prohibition, coexisting with both the 'negative liberty' and 'positive entitlement' readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
