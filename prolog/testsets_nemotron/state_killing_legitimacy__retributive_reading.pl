% ============================================================================
% CONSTRAINT STORY: state_killing_legitimacy__retributive_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_state_killing_legitimacy__retributive_reading, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: state_killing_legitimacy__retributive_reading
 *   human_readable: Retributive Legitimacy of State Killing (Lex Talionis Reading)
 *   domain: criminal_justice/political_philosophy/legal_theory
 *
 * SUMMARY:
 *   This constraint story captures the retributive reading of state killing
 *   legitimacy: the claim that a murderer forfeits their right to life
 *   through proportional desert (lex talionis). The constraint operates as a
 *   snare from the analytical seat — it extracts the offender's life under a
 *   legitimacy claim that the offender morally deserves it. The coordination
 *   function (limiting state violence to proportional desert) is real but
 *   thin; the extraction (the offender's life) is total and irreversible. The
 *   beneficiary structure includes an abstract 'moral_order' (the vindicated
 *   proposition given actor status) and victim families whose identity is
 *   locked to the retributive frame. The agenda setter (state execution
 *   apparatus) maintains the constraint with high suppression (no exit for
 *   the condemned) and low theater (the machinery is brutally functional).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(state_killing_legitimacy__retributive_reading, 0.82).
domain_priors:suppression_score(state_killing_legitimacy__retributive_reading, 0.9).
domain_priors:theater_ratio(state_killing_legitimacy__retributive_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(state_killing_legitimacy__retributive_reading, extractiveness, 0.82).
narrative_ontology:constraint_metric(state_killing_legitimacy__retributive_reading, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(state_killing_legitimacy__retributive_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(state_killing_legitimacy__retributive_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(state_killing_legitimacy__retributive_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(state_killing_legitimacy__retributive_reading, snare).
narrative_ontology:human_readable(state_killing_legitimacy__retributive_reading, "Retributive Legitimacy of State Killing (Lex Talionis Reading)").
narrative_ontology:topic_domain(state_killing_legitimacy__retributive_reading, "criminal_justice/political_philosophy/legal_theory").

domain_priors:requires_active_enforcement(state_killing_legitimacy__retributive_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(state_killing_legitimacy__retributive_reading, '97cd9044-535a-4e9d-820e-292c235e59e3').
narrative_ontology:cs_kernel_codification('97cd9044-535a-4e9d-820e-292c235e59e3', formalized).
narrative_ontology:cs_authority_grounding('97cd9044-535a-4e9d-820e-292c235e59e3', lineage).
narrative_ontology:cs_interpretation_layer_present('97cd9044-535a-4e9d-820e-292c235e59e3').
narrative_ontology:cs_reading_relation('97cd9044-535a-4e9d-820e-292c235e59e3', state_killing_legitimacy__deterrence_reading, coexists_with).
narrative_ontology:cs_reading_relation('97cd9044-535a-4e9d-820e-292c235e59e3', state_killing_legitimacy__abolition_reading, forecloses).
narrative_ontology:cs_axiom('97cd9044-535a-4e9d-820e-292c235e59e3', foundational, murderer_forfeits_life_right_by_desert).
narrative_ontology:cs_axiom_status(murderer_forfeits_life_right_by_desert, holdable).
narrative_ontology:cs_axiom_grounding('97cd9044-535a-4e9d-820e-292c235e59e3', murderer_forfeits_life_right_by_desert, deontological).
narrative_ontology:cs_axiom('97cd9044-535a-4e9d-820e-292c235e59e3', secondary, proportional_punishment_restores_moral_order).
narrative_ontology:cs_axiom_status(proportional_punishment_restores_moral_order, holdable).
narrative_ontology:cs_axiom_grounding('97cd9044-535a-4e9d-820e-292c235e59e3', proportional_punishment_restores_moral_order, deontological).
narrative_ontology:cs_reference_frame('97cd9044-535a-4e9d-820e-292c235e59e3', classical_retributive_justice).
narrative_ontology:cs_drift_state('97cd9044-535a-4e9d-820e-292c235e59e3', contemporary_empirical_challenge_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('97cd9044-535a-4e9d-820e-292c235e59e3', '').
narrative_ontology:cs_kernel_id(state_killing_legitimacy__retributive_reading, state_killing_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(state_killing_legitimacy__retributive_reading, moral_order).
narrative_ontology:constraint_beneficiary(state_killing_legitimacy__retributive_reading, victim_families_retributive).
narrative_ontology:constraint_victim(state_killing_legitimacy__retributive_reading, condemned_offenders).
narrative_ontology:constraint_vindicates(state_killing_legitimacy__retributive_reading, lex_talionis_principle).
narrative_ontology:constraint_vindicates(state_killing_legitimacy__retributive_reading, proportional_desert_doctrine).
narrative_ontology:constraint_vindicates(state_killing_legitimacy__retributive_reading, moral_order_restoration_through_punishment).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Individuals convicted of murder who face execution under this reading. They bear the ultimate cost — their lives — as the constraint's direct target. No exit exists once convicted and sentenced; the state's coercive apparatus ensures compliance. Their moral desert status under this reading is the justification for their total extraction.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__retributive_reading, condemned_offenders, payer,
    powerless, immediate, trapped, national).

% The abstract normative structure that the retributive reading claims is restored or maintained through proportional punishment. It does not act or collect rents; it is the vindicated proposition given actor-like status in the constraint's own logic. The constraint's legitimacy derives from its service to this order.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__retributive_reading, moral_order, beneficiary,
    institutional, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(state_killing_legitimacy__retributive_reading, moral_order).

% Families of murder victims who experience the execution as morally required closure or justice. Their identity and moral framework are fused with the retributive logic; they cannot conceive of justice without proportional desert. They benefit psychologically and morally from the constraint's operation, but their position is not one of material extraction.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__retributive_reading, victim_families_retributive, beneficiary,
    moderate, biographical, identity_locked, local).

% The legal and correctional institutions that administer capital punishment: legislatures that authorize it, courts that impose it, prisons that carry it out. They set the agenda for when and how the constraint operates. They have institutional exit options (moratoria, commutation powers, abolition votes) but exercise them rarely; the constraint's persistence serves institutional legitimacy and deterrence signaling.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__retributive_reading, state_execution_apparatus, agenda_setter,
    institutional, generational, arbitrage, national).

% Organizations, activists, and legal advocates who reject state killing categorically. They are structurally excluded from the constraint's internal logic — the retributive reading treats their objection as morally irrelevant because the offender's desert settles the matter. They operate outside the constraint, seeking to dismantle it through legal challenge, public opinion, and international pressure.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__retributive_reading, abolitionist_civil_society, excluded,
    organized, generational, mobile, global).

% Scholars of criminal law, political philosophy, and ethics who analyze the constraint's structure without being subject to it. They map the conceptual architecture of desert-based legitimacy, track its empirical operation, and compare it across sibling readings. Their exit is analytical — they can adopt any reading's frame.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__retributive_reading, legal_philosophy_observers, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a principled, non-arbitrary limit on state violence by tying the ultimate penalty to a specific moral trigger (murder) and a proportionality principle (life for life). This coordinates social expectations about the boundaries of legitimate punishment and prevents escalation to excessive or discriminatory sanctions.
% TRANSFER_FUNCTION: Moves the offender's life from the offender to the moral order (as restoration) and to victim families (as vindication), mediated by the state's execution apparatus. The transfer is not material but normative: the constraint asserts that the offender's forfeiture *is* the transfer, and the state's role is to enact it.
% ABSENT_VOICES: The condemned offender's own voice after sentencing is structurally silenced — the constraint's logic treats their subsequent preferences as morally void. Future generations who might inherit a different moral consensus about desert are also absent; the constraint locks in a civilizational-time commitment on their behalf.
% DISAPPEARANCE_RATIONALE: If the retributive legitimacy constraint vanished overnight, the legal architecture of capital punishment would lose its primary moral justification in jurisdictions that retain it. Abolitionist momentum would accelerate; moratoria would become abolition; the moral vocabulary of 'desert' would shift to deterrence or incapacitation rationales — or the practice would collapse entirely. The world of punishment would rearrange.
% FOUNDING_PROBLEM: The pre-modern problem of unbounded vengeance and arbitrary sovereign killing: without a proportionality principle, blood feuds and sovereign caprice made punishment unpredictable and often grossly disproportionate. Lex talionis was built to cap vengeance at equivalence — an eye for an eye, no more.
% FOUNDING_PROBLEM_CORROBORATION: Legal historians (e.g., Foucault, Whitman) corroborate that proportionality principles emerged to limit sovereign arbitrariness and private vengeance. However, the same scholars note that modern state killing has reconstituted new forms of arbitrariness (racial disparity, geographic lottery, error rates) that the founding principle does not constrain. The retributive reading's beneficiaries (victim families, moral order advocates) assert the problem remains live; abolitionists and procedural critics attest it is dead in practice.
narrative_ontology:disappearance_verdict(state_killing_legitimacy__retributive_reading, world_rearranges).
narrative_ontology:founding_problem_status(state_killing_legitimacy__retributive_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(state_killing_legitimacy__retributive_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_nemotron+rescue1', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(state_killing_legitimacy__retributive_reading, 'none', 1).
narrative_ontology:epsilon_provenance(state_killing_legitimacy__retributive_reading, 0.82, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(state_killing_legitimacy__retributive_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(state_killing_legitimacy__retributive_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(state_killing_legitimacy__retributive_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.82) because the constraint takes everything from the offender — their life — and the transfer is justified by a desert judgment that the offender cannot contest. Suppression is near-total (0.9) because the state's coercive apparatus allows no exit once the sentence is final; the condemned are trapped by definition. Theater is low (0.15) because the execution machinery is not performative — it is the constraint's real function. Accessibility collapse is moderate (0.65) because alternatives (life without parole, restorative justice) remain conceptually available but are politically and morally excluded by the desert logic. Resistance is high (0.75) because abolitionist movements, legal challenges, and international norms actively contest the constraint.
 *
 * DIRECTIONALITY LOGIC:
 *   The condemned offender is the full target (d ≈ 1.0): powerless, trapped, immediate time horizon. The state execution apparatus is the agenda setter with institutional power and arbitrage-grade exit (it could abolish but chooses not to). Victim families are beneficiaries with identity-locked exit — their moral identity fuses with the constraint, making exit unthinkable. The moral_order is an abstract beneficiary with no exit (it is the constraint's own legitimating fiction). Abolitionists are excluded (mobile, organized) — they would object but are kept outside the constraint's logic. Observers are analytical.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (limiting unbounded vengeance) is historically real but contested as live. Modern state killing exhibits the very arbitrariness the principle was built to prevent (racial disparity, error, geographic lottery). The constraint persists not because it solves the founding problem but because it serves as a legitimacy anchor for state power and a symbolic satisfier for retributive intuition. This is mandatrophy: the mandate (proportional desert) has outlived its function (limiting arbitrariness) but the constraint remains.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is this constraint a distinct reading of the state_killing_legitimacy kernel, or does it collapse into the deterrence_reading in practice?',
    'Examine whether jurisdictions retaining capital punishment justify it primarily through desert language (retributive) or deterrence language (consequentialist) in statutes, judicial opinions, and public discourse. If the same actors deploy both rationales interchangeably, the readings may not be structurally distinct.',
    'If the readings collapse, the kernel has fewer than three live readings; the retributive_reading''s distinct ε and beneficiary structure would be an analytical artifact. The constraint family would need re-specification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Whether the retributive reading is structurally distinct from deterrence in institutional practice').

omega_variable(
    moral_order_as_beneficiary_ontology,
    'Does ''moral_order'' as a beneficiary represent a real structural position, or is it a personification of the constraint''s own self-justification?',
    'Trace whether any human or institutional actor collects rents, status, or power *because* the moral order is treated as a beneficiary. If the benefit accrues only to the state apparatus or victim families, ''moral_order'' is a vindicated proposition misclassified as a beneficiary.',
    'If moral_order is not a real beneficiary, the constraint''s coordination function (beneficiary presence) is weaker than authored, potentially shifting classification toward snare (pure extraction) rather than tangled_rope. The False Summit logic would not apply since claimed_type is already snare.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(moral_order_as_beneficiary_ontology, conceptual, 'Ontological status of abstract normative structures as beneficiaries').

omega_variable(
    desert_based_extraction_measurement,
    'Can extractiveness be measured independently of the desert judgment that constitutes the constraint''s legitimacy?',
    'Compare ε across the three sibling readings using the same standing arrangement (state killing of murderers) as referent. If ε differs only because the desert judgment changes the *valuation* of the extraction (deserved vs. undeserved), then ε is reading-indexed in a way that violates ε-invariance unless each reading is a separate constraint (which they are, per kernel rules).',
    'Confirms that the kernel-reading decomposition correctly isolates ε-invariant constraints. If ε cannot be stabilized per reading, the decomposition fails.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(desert_based_extraction_measurement, empirical, 'Whether ε stabilizes per reading when referent is fixed').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(state_killing_legitimacy__retributive_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(skl_ret_tr_t0, state_killing_legitimacy__retributive_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(skl_ret_tr_t25, state_killing_legitimacy__retributive_reading, theater_ratio, 25, 0.12).
narrative_ontology:measurement(skl_ret_tr_t50, state_killing_legitimacy__retributive_reading, theater_ratio, 50, 0.13).
narrative_ontology:measurement(skl_ret_tr_t75, state_killing_legitimacy__retributive_reading, theater_ratio, 75, 0.14).
narrative_ontology:measurement(skl_ret_tr_t100, state_killing_legitimacy__retributive_reading, theater_ratio, 100, 0.15).

% Extraction over time
narrative_ontology:measurement(skl_ret_be_t0, state_killing_legitimacy__retributive_reading, base_extractiveness, 0, 0.75).
narrative_ontology:measurement(skl_ret_be_t25, state_killing_legitimacy__retributive_reading, base_extractiveness, 25, 0.78).
narrative_ontology:measurement(skl_ret_be_t50, state_killing_legitimacy__retributive_reading, base_extractiveness, 50, 0.8).
narrative_ontology:measurement(skl_ret_be_t75, state_killing_legitimacy__retributive_reading, base_extractiveness, 75, 0.81).
narrative_ontology:measurement(skl_ret_be_t100, state_killing_legitimacy__retributive_reading, base_extractiveness, 100, 0.82).

% Suppression requirement over time
narrative_ontology:measurement(skl_ret_su_t0, state_killing_legitimacy__retributive_reading, suppression_requirement, 0, 0.85).
narrative_ontology:measurement(skl_ret_su_t25, state_killing_legitimacy__retributive_reading, suppression_requirement, 25, 0.87).
narrative_ontology:measurement(skl_ret_su_t50, state_killing_legitimacy__retributive_reading, suppression_requirement, 50, 0.88).
narrative_ontology:measurement(skl_ret_su_t75, state_killing_legitimacy__retributive_reading, suppression_requirement, 75, 0.89).
narrative_ontology:measurement(skl_ret_su_t100, state_killing_legitimacy__retributive_reading, suppression_requirement, 100, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(state_killing_legitimacy__retributive_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(state_killing_legitimacy__retributive_reading, state_killing_legitimacy__deterrence_reading).
narrative_ontology:affects_constraint(state_killing_legitimacy__retributive_reading, state_killing_legitimacy__abolition_reading).

% DUAL FORMULATION NOTE:
% This constraint is one member of the state_killing_legitimacy kernel family. The three readings decompose the natural-language concept 'death penalty legitimacy' into structurally distinct constraints with different ε, beneficiary/victim structures, and coordination functions. They are linked via affects_constraints. The retributive_reading claims the offender forfeits life-right through desert; deterrence_reading claims execution prevents future murders; abolition_reading claims state killing is categorically illegitimate. Each has its own ε and classification.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(state_killing_legitimacy__retributive_reading, institutional, 0.15).
constraint_indexing:directionality_override(state_killing_legitimacy__retributive_reading, powerless, 1.0).
constraint_indexing:directionality_override(state_killing_legitimacy__retributive_reading, moderate, 0.2).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
