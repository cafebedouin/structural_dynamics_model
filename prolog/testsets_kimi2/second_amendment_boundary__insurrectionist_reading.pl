% ============================================================================
% CONSTRAINT STORY: second_amendment_boundary__insurrectionist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_second_amendment_boundary__insurrectionist_reading, []).

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
 *   constraint_id: second_amendment_boundary__insurrectionist_reading
 *   human_readable: Second Amendment Insurrectionist Reading (Armed Resistance Capacity)
 *   domain: constitutional_law/political_theory/firearms_policy
 *
 * SUMMARY:
 *   This constraint instantiates the insurrectionist reading of the Second
 *   Amendment kernel: the claim that the right to keep and bear arms exists
 *   to preserve individual and collective capacity for armed resistance
 *   against tyrannical government, with individual possession treated as
 *   instrumental to potential overthrow. The reading is contested within
 *   constitutional law, producing a structurally asymmetric arrangement where
 *   armed citizens and the firearms industry benefit from legal protection of
 *   militarized possession, while the state security apparatus loses its
 *   monopoly on legitimate force and civilians bear elevated ambient risk of
 *   violence. It is authored as a tangled rope because it carries a genuine
 *   coordination story (republican safeguard/deterrent against tyranny)
 *   alongside substantial asymmetric extraction.
 *
 * KEY AGENTS:
 *   - armed_citizens_resistance: Primary beneficiary (organized/mobile) â claims deterrent legitimacy and organizes political identity around armed sovereignty
 *   - firearms_industry: Secondary beneficiary (powerful/arbitrage) â captures revenue from expanded military-grade civilian markets
 *   - state_security_apparatus: Primary payer (institutional/trapped) â loses regulatory capacity and faces operational threat from militarized populace
 *   - civilians_caught_in_conflict: Secondary payer (powerless/trapped) â bears ambient mortality risk from widespread militarized weapons
 *   - federal_judiciary_insurrectionist: Agenda setter (institutional/analytical) â administers the constraint through jurisprudential enforcement
 *   - constitutional_historians: Analytical observer (analytical/analytical) â evaluates historical validity without stake in the constraint's persistence
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(second_amendment_boundary__insurrectionist_reading, 0.78).
domain_priors:suppression_score(second_amendment_boundary__insurrectionist_reading, 0.82).
domain_priors:theater_ratio(second_amendment_boundary__insurrectionist_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(second_amendment_boundary__insurrectionist_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(second_amendment_boundary__insurrectionist_reading, suppression_requirement, 0.82).
narrative_ontology:constraint_metric(second_amendment_boundary__insurrectionist_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(second_amendment_boundary__insurrectionist_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(second_amendment_boundary__insurrectionist_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(second_amendment_boundary__insurrectionist_reading, tangled_rope).
narrative_ontology:human_readable(second_amendment_boundary__insurrectionist_reading, "Second Amendment Insurrectionist Reading (Armed Resistance Capacity)").
narrative_ontology:topic_domain(second_amendment_boundary__insurrectionist_reading, "constitutional_law/political_theory/firearms_policy").

domain_priors:requires_active_enforcement(second_amendment_boundary__insurrectionist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(second_amendment_boundary__insurrectionist_reading, '06980ff2-d08c-4229-8c90-7e1d51c6aa78').
narrative_ontology:cs_kernel_codification('06980ff2-d08c-4229-8c90-7e1d51c6aa78', fixed_text).
narrative_ontology:cs_authority_grounding('06980ff2-d08c-4229-8c90-7e1d51c6aa78', lineage).
narrative_ontology:cs_interpretation_layer_present('06980ff2-d08c-4229-8c90-7e1d51c6aa78').
narrative_ontology:cs_reading_relation('06980ff2-d08c-4229-8c90-7e1d51c6aa78', second_amendment_boundary__individual_right_reading, influences).
narrative_ontology:cs_reading_relation('06980ff2-d08c-4229-8c90-7e1d51c6aa78', second_amendment_boundary__militia_conditioned_reading, forecloses).
narrative_ontology:cs_axiom('06980ff2-d08c-4229-8c90-7e1d51c6aa78', foundational, right_to_revolutionary_force).
narrative_ontology:cs_axiom_status(right_to_revolutionary_force, holdable).
narrative_ontology:cs_axiom_grounding('06980ff2-d08c-4229-8c90-7e1d51c6aa78', right_to_revolutionary_force, deontological).
narrative_ontology:cs_axiom('06980ff2-d08c-4229-8c90-7e1d51c6aa78', foundational, individual_arms_effective_against_tyranny).
narrative_ontology:cs_axiom_status(individual_arms_effective_against_tyranny, holdable).
narrative_ontology:cs_axiom_grounding('06980ff2-d08c-4229-8c90-7e1d51c6aa78', individual_arms_effective_against_tyranny, empirically_contingent).
narrative_ontology:cs_reference_frame('06980ff2-d08c-4229-8c90-7e1d51c6aa78', founding_republican_safeguard).
narrative_ontology:cs_drift_state('06980ff2-d08c-4229-8c90-7e1d51c6aa78', contemporary_security_state, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('06980ff2-d08c-4229-8c90-7e1d51c6aa78', '').
narrative_ontology:cs_kernel_id(second_amendment_boundary__insurrectionist_reading, second_amendment_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(second_amendment_boundary__insurrectionist_reading, armed_citizens_resistance).
narrative_ontology:constraint_beneficiary(second_amendment_boundary__insurrectionist_reading, firearms_industry).
narrative_ontology:constraint_victim(second_amendment_boundary__insurrectionist_reading, state_security_apparatus).
narrative_ontology:constraint_victim(second_amendment_boundary__insurrectionist_reading, civilians_caught_in_conflict).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Assert a constitutional right to possess military-grade arms as an individual safeguard against tyrannical government. Benefit from judicial invalidation of disarmament and licensing laws. Their political identity is organized around the deterrent legitimacy of an armed populace.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__insurrectionist_reading, armed_citizens_resistance, beneficiary,
    organized, biographical, mobile, national).

% Profits from expanded legal markets for militarized weapons protected under the insurrectionist reading. Lobbying and marketing amplify the framing of armed citizenship, directly benefiting from the constraint's suppression of regulatory alternatives.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__insurrectionist_reading, firearms_industry, beneficiary,
    powerful, biographical, arbitrage, national).

% Federal and state law enforcement and military whose capacity to monopolize legitimate armed force is judicially constrained. They bear the operational risk of confronting militarized civilians and the strategic cost of planning for domestic insurrectionary scenarios.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__insurrectionist_reading, state_security_apparatus, payer,
    institutional, generational, trapped, national).

% General public, including unarmed individuals, who face elevated ambient risk of mass violence and the potential collateral damage of armed insurrection. They cannot readily exit the national jurisdiction or the legal framework that enables widespread militarized possession.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__insurrectionist_reading, civilians_caught_in_conflict, payer,
    powerless, immediate, trapped, national).

% Judicial actors who interpret the Second Amendment to protect individual possession of military-grade arms against legislative restriction. They administer the constraint by striking down gun laws and establishing the insurrectionist reading as binding precedent.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__insurrectionist_reading, federal_judiciary_insurrectionist, agenda_setter,
    institutional, generational, analytical, national).

% Academic historians and legal scholars evaluating the historical validity of the insurrectionist reading against founding-era sources and comparative republican theory. They do not collect from or pay into the constraint.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__insurrectionist_reading, constitutional_historians, observer,
    analytical, civilizational, analytical, national).

narrative_ontology:fixing_cost_class(second_amendment_boundary__insurrectionist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Preserves a distributed capacity for armed popular resistance against centralized tyranny, theoretically coordinating a deterrent check on government overreach by ensuring the people retain the means of revolution.
% TRANSFER_FUNCTION: Transfers the monopoly on legitimate armed force away from the state security apparatus toward the armed citizenry; transfers risk and mortality from the state onto civilians and state agents who face militarized opposition.
% ABSENT_VOICES: Gun control legislators and unarmed urban populations who would prefer comprehensive disarmament are structurally excluded from jurisprudential influence; their policy preferences are treated as tyranny-adjacent precursors rather than legitimate governance.
% DISAPPEARANCE_RATIONALE: If the insurrectionist reading vanished overnight, legislatures would re-enact assault weapon bans and comprehensive licensing regimes; the firearms industry's military-grade civilian market would contract sharply; state security operations would recalibrate threat models away from domestic insurrectionary confrontation.
% FOUNDING_PROBLEM: Founding-era fear of standing armies and centralized military power subjugating the populace; the desire to ensure the people retained the practical means to overthrow a tyrannical government.
% FOUNDING_PROBLEM_CORROBORATION: Insurrectionist historians and some originalist jurists attest the founding problem is live and justifies the reading. Mainstream historians and comparative constitutional scholars attest the problem is anachronistic in the context of the modern security state; corroboration from outside the armed-citizen beneficiary set is split along methodological and ideological lines.
narrative_ontology:disappearance_verdict(second_amendment_boundary__insurrectionist_reading, world_rearranges).
narrative_ontology:founding_problem_status(second_amendment_boundary__insurrectionist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(second_amendment_boundary__insurrectionist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(second_amendment_boundary__insurrectionist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(second_amendment_boundary__insurrectionist_reading, 0.78, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(second_amendment_boundary__insurrectionist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(second_amendment_boundary__insurrectionist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(second_amendment_boundary__insurrectionist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.78) is high because the constraint systematically transfers security costs from the state and general public to the beneficiaries of unrestricted possession. Suppression (0.82) is higher because the reading's persistence requires active judicial suppression of legislative disarmament efforts. Theater ratio (0.45) reflects significant performative maintenance of founding-era revolutionary rhetoric alongside genuine legal effect. Accessibility collapse (0.60) indicates that disarmament alternatives are substantially foreclosed within US constitutional jurisprudence but remain visible in comparative law. Resistance (0.75) is high due to sustained opposition from gun control advocates and security professionals. The measurement series share a single time grid to prevent misalignment artifacts.
 *
 * PERSPECTIVAL GAP:
 *   The armed citizen seat experiences the constraint as a safeguard of liberty and deterrent against state overreach. The state security seat experiences the same constraint as a forced degradation of operational capacity and an invitation to insurrectionary violence. The civilian payer seat experiences it as an unchosen elevation of ambient risk. The engine computes these divergent seat classifications from the structural asymmetry in exit options (mobile/arbitrage for beneficiaries, trapped for payers) and beneficiary/victim declarations.
 *
 * DIRECTIONALITY LOGIC:
 *   Armed citizens and the firearms industry are declared beneficiaries, deriving low directionality (subsidized by the constraint's protection of possession). The state security apparatus and civilians caught in conflict are declared victims/payers, deriving high directionality (targeted by the constraint's extraction of security and safety). The federal judiciary sits as agenda setter with analytical exit. Directionality is structurally derived from these declarations without override.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled rope classification prevents mislabeling the constraint as pure coordination (rope) by requiring declared victims and active enforcement, which captures the asymmetric security extraction. It prevents mislabeling as pure extraction (snare) by acknowledging the genuine republican safeguard coordination story that motivates the beneficiaries. If the empirical premise of armed resistance against modern states were falsified, the coordination component would atrophy and the constraint would drift toward piton or snare depending on whether enforcement became purely theatrical or actively rent-seeking.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_scope_ambiguity,
    'Does the Second Amendment kernel admit logically distinct readings with mutually exclusive beneficiary sets, or is the insurrectionist reading the only coherent interpretation of the text?',
    'Comparative doctrinal analysis of which readings are actually held by distinct interpretive communities, plus historical evidence of ratifier intent regarding individual versus collective resistance capacity.',
    'If multiple coherent readings exist, the kernel is genuinely contested and the insurrectionist reading''s high extraction is reading-specific rather than textually necessitated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_scope_ambiguity, conceptual, 'Whether the kernel text admits multiple structurally distinct readings.').

omega_variable(
    insurrectionist_empirical_premise,
    'Is individual possession of small arms empirically sufficient to overthrow a modern surveillance and military state, falsifying the insurrectionist reading''s instrumental premise?',
    'Historical case studies of insurgencies against modern states and analysis of force asymmetries; if the premise is falsified, the coordination story collapses into theater.',
    'A falsified empirical premise would reclassify the constraint''s coordination component as largely performative, raising theater_ratio and potentially shifting the computed type toward snare or piton.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(insurrectionist_empirical_premise, empirical, 'Empirical validity of armed resistance premise against modern states.').

omega_variable(
    civilian_risk_structural_contingent,
    'Is the elevated civilian violence risk under this reading a structural feature of militarized distribution or contingent on criminal misuse?',
    'Cross-jurisdictional epidemiological comparison of violence rates under varying regulatory regimes, controlling for socioeconomic variables.',
    'If structural, the victim set is larger and extraction from civilians is intrinsic; if contingent, the extraction is mediated through other constraints and the victim set may be narrower.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(civilian_risk_structural_contingent, empirical, 'Whether civilian risk is structurally intrinsic or contingent.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(second_amendment_boundary__insurrectionist_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(seco_tr_t0, second_amendment_boundary__insurrectionist_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(seco_tr_t8, second_amendment_boundary__insurrectionist_reading, theater_ratio, 8, 0.3).
narrative_ontology:measurement(seco_tr_t16, second_amendment_boundary__insurrectionist_reading, theater_ratio, 16, 0.35).
narrative_ontology:measurement(seco_tr_t24, second_amendment_boundary__insurrectionist_reading, theater_ratio, 24, 0.4).
narrative_ontology:measurement(seco_tr_t32, second_amendment_boundary__insurrectionist_reading, theater_ratio, 32, 0.43).
narrative_ontology:measurement(seco_tr_t40, second_amendment_boundary__insurrectionist_reading, theater_ratio, 40, 0.45).

% Extraction over time
narrative_ontology:measurement(seco_be_t0, second_amendment_boundary__insurrectionist_reading, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(seco_be_t8, second_amendment_boundary__insurrectionist_reading, base_extractiveness, 8, 0.5).
narrative_ontology:measurement(seco_be_t16, second_amendment_boundary__insurrectionist_reading, base_extractiveness, 16, 0.6).
narrative_ontology:measurement(seco_be_t24, second_amendment_boundary__insurrectionist_reading, base_extractiveness, 24, 0.68).
narrative_ontology:measurement(seco_be_t32, second_amendment_boundary__insurrectionist_reading, base_extractiveness, 32, 0.74).
narrative_ontology:measurement(seco_be_t40, second_amendment_boundary__insurrectionist_reading, base_extractiveness, 40, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(seco_su_t0, second_amendment_boundary__insurrectionist_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(seco_su_t8, second_amendment_boundary__insurrectionist_reading, suppression_requirement, 8, 0.6).
narrative_ontology:measurement(seco_su_t16, second_amendment_boundary__insurrectionist_reading, suppression_requirement, 16, 0.7).
narrative_ontology:measurement(seco_su_t24, second_amendment_boundary__insurrectionist_reading, suppression_requirement, 24, 0.76).
narrative_ontology:measurement(seco_su_t32, second_amendment_boundary__insurrectionist_reading, suppression_requirement, 32, 0.8).
narrative_ontology:measurement(seco_su_t40, second_amendment_boundary__insurrectionist_reading, suppression_requirement, 40, 0.82).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(second_amendment_boundary__insurrectionist_reading, identity_coordination).
narrative_ontology:affects_constraint(second_amendment_boundary__insurrectionist_reading, individual_right_reading).
narrative_ontology:affects_constraint(second_amendment_boundary__insurrectionist_reading, militia_conditioned_reading).

% DUAL FORMULATION NOTE:
% This constraint is the insurrectionist reading of the Second Amendment kernel, decomposed from the natural-language 'Second Amendment' which conflates individual-right, insurrectionist, and militia-conditioned structurally distinct claims. Each reading carries a different epsilon, beneficiary structure, and classification.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
