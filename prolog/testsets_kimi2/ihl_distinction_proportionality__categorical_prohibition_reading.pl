% ============================================================================
% CONSTRAINT STORY: ihl_distinction_proportionality__categorical_prohibition_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ihl_distinction_proportionality__categorical_prohibition_reading, []).

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
    narrative_ontology:suppression_profile/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
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
 *   constraint_id: ihl_distinction_proportionality__categorical_prohibition_reading
 *   human_readable: Martens Clause Categorical Prohibition of Lethal Autonomous Weapons
 *   domain: international_law/military_ethics/technology_governance
 *
 * SUMMARY:
 *   This constraint instantiates the categorical prohibition reading of the
 *   Martens Clause kernel: the claim that principles of humanity and public
 *   conscience render lethal autonomous weapons unlawful per se, irrespective
 *   of technical performance in distinction or proportionality. The
 *   constraint operates as a tangled rope because it coordinates the
 *   international community around a genuine humanitarian standard while
 *   asymmetrically extracting strategic options from militarily advanced
 *   states. Anti-militarist civil society and states lacking LAWS capability
 *   are the structural beneficiaries; advanced military states and their
 *   defense industries are the structural targets. The reading forecloses any
 *   outcomes-based justification because performance cannot legitimate
 *   machine-decided killing, and it influences the human-agency reading by
 *   rendering its regulatory nuances moot.
 *
 * KEY AGENTS:
 *   - anti_militarist_civil_society: Primary beneficiary (organized/mobile) â gains moral-political legitimacy from the prohibition without bearing compliance costs
 *   - states_without_laws_capability: Secondary beneficiary (institutional/constrained) â avoids arms-race disadvantage; subsidized by the constraint
 *   - advanced_military_states: Primary target (institutional/constrained) â bears the strategic cost of forgoing autonomous weapons
 *   - defense_autonomy_industry: Secondary target (powerful/constrained) â faces market exclusion if the technology class is banned
 *   - international_humanitarian_law_community: Analytical observer (institutional/analytical) â interprets the kernel without direct cost or benefit
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ihl_distinction_proportionality__categorical_prohibition_reading, 0.88).
domain_priors:suppression_score(ihl_distinction_proportionality__categorical_prohibition_reading, 0.72).
domain_priors:theater_ratio(ihl_distinction_proportionality__categorical_prohibition_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ihl_distinction_proportionality__categorical_prohibition_reading, extractiveness, 0.88).
narrative_ontology:constraint_metric(ihl_distinction_proportionality__categorical_prohibition_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(ihl_distinction_proportionality__categorical_prohibition_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ihl_distinction_proportionality__categorical_prohibition_reading, accessibility_collapse, 0.82).
narrative_ontology:constraint_metric(ihl_distinction_proportionality__categorical_prohibition_reading, resistance, 0.78).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ihl_distinction_proportionality__categorical_prohibition_reading, tangled_rope).
narrative_ontology:human_readable(ihl_distinction_proportionality__categorical_prohibition_reading, "Martens Clause Categorical Prohibition of Lethal Autonomous Weapons").
narrative_ontology:topic_domain(ihl_distinction_proportionality__categorical_prohibition_reading, "international_law/military_ethics/technology_governance").

domain_priors:requires_active_enforcement(ihl_distinction_proportionality__categorical_prohibition_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ihl_distinction_proportionality__categorical_prohibition_reading, 'cf592eab-22a6-4a48-b80f-a5d8e34ed933').
narrative_ontology:cs_kernel_codification('cf592eab-22a6-4a48-b80f-a5d8e34ed933', fixed_text).
narrative_ontology:cs_authority_grounding('cf592eab-22a6-4a48-b80f-a5d8e34ed933', lineage).
narrative_ontology:cs_interpretation_layer_present('cf592eab-22a6-4a48-b80f-a5d8e34ed933').
narrative_ontology:cs_reading_relation('cf592eab-22a6-4a48-b80f-a5d8e34ed933', ihl_distinction_proportionality__human_agency_reading, influences).
narrative_ontology:cs_reading_relation('cf592eab-22a6-4a48-b80f-a5d8e34ed933', ihl_distinction_proportionality__outcomes_based_reading, forecloses).
narrative_ontology:cs_axiom('cf592eab-22a6-4a48-b80f-a5d8e34ed933', foundational, machine_decided_killing_violates_dignity_per_se).
narrative_ontology:cs_axiom_status(machine_decided_killing_violates_dignity_per_se, holdable).
narrative_ontology:cs_axiom_grounding('cf592eab-22a6-4a48-b80f-a5d8e34ed933', machine_decided_killing_violates_dignity_per_se, deontological).
narrative_ontology:cs_axiom('cf592eab-22a6-4a48-b80f-a5d8e34ed933', foundational, martens_clause_categorical_prohibition_laws).
narrative_ontology:cs_axiom_status(martens_clause_categorical_prohibition_laws, holdable).
narrative_ontology:cs_axiom_grounding('cf592eab-22a6-4a48-b80f-a5d8e34ed933', martens_clause_categorical_prohibition_laws, conventional).
narrative_ontology:cs_reference_frame('cf592eab-22a6-4a48-b80f-a5d8e34ed933', martens_clause_humanitarian_minimum).
narrative_ontology:cs_drift_state('cf592eab-22a6-4a48-b80f-a5d8e34ed933', contemporary_laws_proliferation, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('cf592eab-22a6-4a48-b80f-a5d8e34ed933', '').
narrative_ontology:cs_kernel_id(ihl_distinction_proportionality__categorical_prohibition_reading, ihl_distinction_proportionality).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ihl_distinction_proportionality__categorical_prohibition_reading, anti_militarist_civil_society).
narrative_ontology:constraint_beneficiary(ihl_distinction_proportionality__categorical_prohibition_reading, states_without_laws_capability).
narrative_ontology:constraint_victim(ihl_distinction_proportionality__categorical_prohibition_reading, advanced_military_states).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(ihl_distinction_proportionality__categorical_prohibition_reading, defense_autonomy_industry).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Campaigns through international networks to establish that machine-decided killing is unlawful under the Martens Clause regardless of targeting accuracy. They collect petition signatures, publish legal analyses, and lobby treaty bodies. If the categorical reading fails, they retain other disarmament targets.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__categorical_prohibition_reading, anti_militarist_civil_society, beneficiary,
    organized, generational, mobile, global).

% Do not possess the industrial base to build lethal autonomous weapons and would face strategic vulnerability if adversaries deployed them. They advance the categorical reading in diplomatic forums while investing in conventional defense. They are bound by the same treaties but sacrifice no existing programs.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__categorical_prohibition_reading, states_without_laws_capability, beneficiary,
    institutional, generational, constrained, global).

% Have invested in autonomous targeting systems and view machine speed as a strategic advantage. The categorical reading forces them to abandon or covertly continue programs under legal risk. Their maneuvering room is limited by the structure of customary international law and the diplomatic cost of openly rejecting humanitarian treaties.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__categorical_prohibition_reading, advanced_military_states, payer,
    institutional, generational, constrained, global).

% Engineering firms and AI contractors developing sensor-to-shooter loops for defense ministries. They face cancelled contracts and diverted R&D investment if the categorical prohibition enters binding treaty form. Their testimony is rarely admitted in the humanitarian forums where the prohibition is negotiated.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__categorical_prohibition_reading, defense_autonomy_industry, payer,
    powerful, biographical, constrained, global).

% Scholars and jurists who draft legal opinions on whether the 1899 Martens Clause extends to algorithmic decision-making. They debate text, intent, and state practice without personal financial stake in whether LAWS are deployed or banned.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__categorical_prohibition_reading, international_humanitarian_law_community, observer,
    institutional, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(ihl_distinction_proportionality__categorical_prohibition_reading, diffuse).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Prevents an arms race in lethal autonomous weapons and establishes a humanitarian floor by preserving human moral agency over life-and-death decisions, coordinating the international community around a shared legal standard derived from the Martens Clause.
% TRANSFER_FUNCTION: Transfers strategic military advantage away from states with advanced autonomous programs toward states without such capability; transfers political legitimacy and institutional resources to humanitarian actors who assert the primacy of human dignity over technological performance.
% ABSENT_VOICES: Defense technology firms and military operators with direct experience of autonomous system capabilities are largely excluded from the humanitarian legal forums; their operational perspective is treated as irrelevant to the dignity-based prohibition.
% DISAPPEARANCE_RATIONALE: If the categorical prohibition vanished, advanced military states would accelerate LAWS acquisition and deployment, a new arms race would destabilize strategic balances, and the post-World War II humanitarian legal architecture would lose a contested but load-bearing pillar.
% FOUNDING_PROBLEM: The prospect of warfare in which machines make lethal decisions without human moral judgment, eroding the principle of humanity and severing the link between political choice and military violence that the laws of war were designed to regulate.
% FOUNDING_PROBLEM_CORROBORATION: The International Committee of the Red Cross attests that removing human judgment from lethal decision-making poses a distinct humanitarian problem, from a seat independent of state military interest. Major military powers and their allied security scholars dispute both the uniqueness of the problem and the categorical remedy.
narrative_ontology:disappearance_verdict(ihl_distinction_proportionality__categorical_prohibition_reading, world_rearranges).
narrative_ontology:founding_problem_status(ihl_distinction_proportionality__categorical_prohibition_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ihl_distinction_proportionality__categorical_prohibition_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(ihl_distinction_proportionality__categorical_prohibition_reading, 'none', 1).
narrative_ontology:epsilon_provenance(ihl_distinction_proportionality__categorical_prohibition_reading, 0.88, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ihl_distinction_proportionality__categorical_prohibition_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(ihl_distinction_proportionality__categorical_prohibition_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(ihl_distinction_proportionality__categorical_prohibition_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.88) because the constraint bans an entire technology class regardless of operational merit, imposing substantial strategic costs on advanced actors. Suppression (0.72) reflects the legal and diplomatic machinery required to hold the prohibition against state resistance. Accessibility collapse is high (0.82) because once the categorical reading is accepted, no lawful design alternative existsâthe exit is not better engineering but abandoning the technology. Resistance (0.78) is high because major military powers actively contest the reading in CCW forums and national policy. Theater ratio is low (0.25) because the humanitarian coordination function is genuine and not merely performative, though diplomatic theater surrounds the CCW process.
 *
 * PERSPECTIVAL GAP:
 *   From the beneficiary seat (non-capable states, humanitarian advocates), the constraint is a necessary coordination mechanism that prevents technological escalation and protects human dignity. From the payer seat (advanced military states), the same constraint appears as a constructed legal barrier that freezes military-technological competition at a moment when their adversaries might gain ground. The engine computes this divergence from the structural asymmetry in exit options and cost-bearing, not from the claim.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (anti-militarist civil society, states without LAWS) experience low directionality because the constraint subsidizes their strategic position or moral agenda without imposing compliance costs. The payer (advanced military states) experiences high directionality because the constraint specifically targets their technological advantage for elimination. Civil society has mobile exit (can shift advocacy); advanced states have constrained exit (customary law is sticky and reputational costs of violation are high).
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as tangled rope prevents mislabeling the constraint as a pure snare (which would ignore the genuine humanitarian coordination function and the reality of beneficiary states that sincerely hold the norm) or as a pure rope (which would ignore the asymmetric extraction from advanced military actors). The mandate is liveâthe founding problem of machine-decided killing remains contested but unresolvedâso mandatrophy is not declared resolved.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_position,
    'Is the Martens Clause properly read as a categorical prohibition of autonomous weapons, or as a flexible humanitarian minimum that adapts to technological change?',
    'Comparative legal analysis of state practice and opinio juris; ICJ advisory opinion or authoritative treaty interpretation.',
    'If the Martens Clause is a flexible minimum, this constraint dissolves into a different reading (human_agency or outcomes_based); if it is categorical, the sibling readings are foreclosed in any unified legal framework.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_position, conceptual, 'Whether this reading is the correct interpretation of the Martens Clause kernel').

omega_variable(
    martens_clause_natural_law_status,
    'Does the Martens Clause express a pre-legal humanitarian limit inherent to armed conflict, or is it a constructed legal instrument that asymmetrically benefits non-weaponizing states?',
    'Historical genealogy of the Martens Clause coupled with empirical analysis of which state categories advance and resist the categorical reading.',
    'If pre-legal and inherent, directionality is uniform (all states equally bound by natural law); if constructed, the constraint is a tangled rope benefiting states that face no compliance cost.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(martens_clause_natural_law_status, conceptual, 'Whether the Martens Clause is a natural law limit or a constructed norm').

omega_variable(
    state_compliance_gap,
    'To what extent are advanced military states already operating autonomous systems that violate the categorical prohibition, creating a compliance gap?',
    'Intelligence and open-source monitoring of military autonomous systems with lethal capabilities; whistleblower disclosures.',
    'Wide compliance gap would indicate the constraint''s suppression is lower than measured and the prohibition is largely theater; narrow gap would support the constraint''s effectiveness.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(state_compliance_gap, empirical, 'Whether states are covertly violating the categorical prohibition').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ihl_distinction_proportionality__categorical_prohibition_reading, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ihl__tr_t0, ihl_distinction_proportionality__categorical_prohibition_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(ihl__tr_t3, ihl_distinction_proportionality__categorical_prohibition_reading, theater_ratio, 3, 0.18).
narrative_ontology:measurement(ihl__tr_t6, ihl_distinction_proportionality__categorical_prohibition_reading, theater_ratio, 6, 0.2).
narrative_ontology:measurement(ihl__tr_t9, ihl_distinction_proportionality__categorical_prohibition_reading, theater_ratio, 9, 0.22).
narrative_ontology:measurement(ihl__tr_t12, ihl_distinction_proportionality__categorical_prohibition_reading, theater_ratio, 12, 0.24).
narrative_ontology:measurement(ihl__tr_t15, ihl_distinction_proportionality__categorical_prohibition_reading, theater_ratio, 15, 0.25).

% Extraction over time
narrative_ontology:measurement(ihl__be_t0, ihl_distinction_proportionality__categorical_prohibition_reading, base_extractiveness, 0, 0.68).
narrative_ontology:measurement(ihl__be_t3, ihl_distinction_proportionality__categorical_prohibition_reading, base_extractiveness, 3, 0.72).
narrative_ontology:measurement(ihl__be_t6, ihl_distinction_proportionality__categorical_prohibition_reading, base_extractiveness, 6, 0.76).
narrative_ontology:measurement(ihl__be_t9, ihl_distinction_proportionality__categorical_prohibition_reading, base_extractiveness, 9, 0.8).
narrative_ontology:measurement(ihl__be_t12, ihl_distinction_proportionality__categorical_prohibition_reading, base_extractiveness, 12, 0.84).
narrative_ontology:measurement(ihl__be_t15, ihl_distinction_proportionality__categorical_prohibition_reading, base_extractiveness, 15, 0.88).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(ihl_distinction_proportionality__categorical_prohibition_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ihl_distinction_proportionality__categorical_prohibition_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(ihl_distinction_proportionality__categorical_prohibition_reading, ihl_distinction_proportionality__human_agency_reading).
narrative_ontology:affects_constraint(ihl_distinction_proportionality__categorical_prohibition_reading, ihl_distinction_proportionality__outcomes_based_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the ihl_distinction_proportionality kernel. The kernel decomposes into at least three structurally distinct constraints because the natural-language label 'IHL distinction and proportionality as applied to autonomous weapons' conflates claims with different epsilon values, beneficiary structures, and logical scope.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
