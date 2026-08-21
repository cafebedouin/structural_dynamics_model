% ============================================================================
% CONSTRAINT STORY: second_amendment_boundary__insurrectionist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: second_amendment_boundary__insurrectionist_reading
 *   human_readable: Second Amendment: Insurrectionist Reading (Right to Overthrow Tyranny)
 *   domain: constitutional_law/political_theory/firearms_policy
 *
 * SUMMARY:
 *   This constraint represents the 'insurrectionist reading' of the Second
 *   Amendment, which posits that the right to bear arms exists primarily to
 *   preserve the capacity for armed resistance against a tyrannical
 *   government, with individual possession being instrumental to potential
 *   overthrow. This reading extends protection to military-grade arms and
 *   views state disarmament efforts as precursors to tyranny. While
 *   proponents frame it as a 'Rope' (coordination against tyranny), the
 *   structural consequences, including identifiable victims (state security,
 *   civilians in conflict), lead to its classification as a 'Snare' from an
 *   objective analytical perspective. The claim/metric gap is deliberate: the
 *   constraint is CLAIMED as a Rope by its adherents, but the authored
 *   metrics describe a substantially extractive and suppressive operation
 *   from the perspective of the state and society.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(second_amendment_boundary__insurrectionist_reading, 0.85).
domain_priors:suppression_score(second_amendment_boundary__insurrectionist_reading, 0.9).
domain_priors:theater_ratio(second_amendment_boundary__insurrectionist_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(second_amendment_boundary__insurrectionist_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(second_amendment_boundary__insurrectionist_reading, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(second_amendment_boundary__insurrectionist_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(second_amendment_boundary__insurrectionist_reading, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(second_amendment_boundary__insurrectionist_reading, resistance, 0.8).

% --- Constraint claim ---
narrative_ontology:constraint_claim(second_amendment_boundary__insurrectionist_reading, snare).
narrative_ontology:human_readable(second_amendment_boundary__insurrectionist_reading, "Second Amendment: Insurrectionist Reading (Right to Overthrow Tyranny)").
narrative_ontology:topic_domain(second_amendment_boundary__insurrectionist_reading, "constitutional_law/political_theory/firearms_policy").

domain_priors:requires_active_enforcement(second_amendment_boundary__insurrectionist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(second_amendment_boundary__insurrectionist_reading, 'cf7d5195-4901-4785-a3a0-e854a126f1c0').
narrative_ontology:cs_kernel_codification('cf7d5195-4901-4785-a3a0-e854a126f1c0', fixed_text).
narrative_ontology:cs_authority_grounding('cf7d5195-4901-4785-a3a0-e854a126f1c0', lineage).
narrative_ontology:cs_interpretation_layer_present('cf7d5195-4901-4785-a3a0-e854a126f1c0').
narrative_ontology:cs_reading_relation('cf7d5195-4901-4785-a3a0-e854a126f1c0', second_amendment_boundary__individual_right_reading, coexists_with).
narrative_ontology:cs_reading_relation('cf7d5195-4901-4785-a3a0-e854a126f1c0', second_amendment_boundary__militia_conditioned_reading, forecloses).
narrative_ontology:cs_axiom('cf7d5195-4901-4785-a3a0-e854a126f1c0', foundational, individual_arms_for_tyranny_resistance).
narrative_ontology:cs_axiom_status(individual_arms_for_tyranny_resistance, holdable).
narrative_ontology:cs_axiom_grounding('cf7d5195-4901-4785-a3a0-e854a126f1c0', individual_arms_for_tyranny_resistance, deontological).
narrative_ontology:cs_axiom('cf7d5195-4901-4785-a3a0-e854a126f1c0', secondary, state_disarmament_is_tyranny_precursor).
narrative_ontology:cs_axiom_status(state_disarmament_is_tyranny_precursor, holdable).
narrative_ontology:cs_axiom_grounding('cf7d5195-4901-4785-a3a0-e854a126f1c0', state_disarmament_is_tyranny_precursor, empirically_contingent).
narrative_ontology:cs_reference_frame('cf7d5195-4901-4785-a3a0-e854a126f1c0', founding_era_republican_virtue).
narrative_ontology:cs_drift_state('cf7d5195-4901-4785-a3a0-e854a126f1c0', contemporary_political_polarization, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('cf7d5195-4901-4785-a3a0-e854a126f1c0', '2024-07-30T12:00:00Z').
narrative_ontology:cs_kernel_id(second_amendment_boundary__insurrectionist_reading, second_amendment_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(second_amendment_boundary__insurrectionist_reading, armed_citizens_claiming_deterrent_legitimacy).
narrative_ontology:constraint_victim(second_amendment_boundary__insurrectionist_reading, state_security_apparatus).
narrative_ontology:constraint_victim(second_amendment_boundary__insurrectionist_reading, civilians_in_conflict_zones).
narrative_ontology:constraint_vindicates(second_amendment_boundary__insurrectionist_reading, right_to_revolution_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Adherents to this reading believe their armed status deters government overreach and preserves liberty. They see themselves as the ultimate check on tyranny, justifying possession of military-grade arms. Their identity is often fused with this role, making exit from this ideological stance unthinkable.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__insurrectionist_reading, armed_citizens_claiming_deterrent_legitimacy, beneficiary,
    organized, generational, identity_locked, national).

% The police, military, and intelligence agencies are the direct targets of any actualized 'resistance.' They bear the costs of maintaining order, countering domestic threats, and potentially engaging in armed conflict with citizens. Their operational capacity is constrained by the perceived threat and the legal/political landscape this reading creates.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__insurrectionist_reading, state_security_apparatus, payer,
    institutional, immediate, constrained, national).

% In any scenario of armed resistance or civil conflict, ordinary civilians are the primary victims, facing violence, displacement, and disruption of essential services. They have no meaningful exit from the consequences of such conflict.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__insurrectionist_reading, civilians_in_conflict_zones, payer,
    powerless, immediate, trapped, local).

% As the representatives of the state, they are the ultimate target of the 'resistance' envisioned by this reading. They are responsible for maintaining public order and would implement disarmament policies, which this reading interprets as tyrannical precursors. Their legitimacy is constantly challenged by this interpretation.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__insurrectionist_reading, government_officials, agenda_setter,
    institutional, biographical, constrained, national).

% Academics and legal experts who analyze the historical, textual, and jurisprudential foundations of the Second Amendment. They observe the practical and theoretical implications of this reading without directly participating in its enforcement or resistance.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__insurrectionist_reading, constitutional_scholars, observer,
    analytical, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(second_amendment_boundary__insurrectionist_reading, armed_citizens_claiming_deterrent_legitimacy).
narrative_ontology:fixing_cost_class(second_amendment_boundary__insurrectionist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: This reading coordinates the potential for collective armed resistance among citizens, framing individual arms possession as a deterrent against perceived governmental tyranny and a means for its overthrow.
% TRANSFER_FUNCTION: It transfers the burden of potential armed conflict to the state and civilians, while transferring a sense of legitimacy and power to armed citizens who see themselves as guardians of liberty. It also transfers the cost of maintaining a heavily armed populace (e.g., through gun violence) to society at large.
% ABSENT_VOICES: Those who prioritize public safety, social cohesion, and democratic political processes over the capacity for armed insurrection are often marginalized or dismissed by adherents of this reading. Victims of gun violence and proponents of stricter firearms regulation are structurally excluded from the core conversation of this reading.
% DISAPPEARANCE_RATIONALE: If this reading vanished overnight, the legal and political landscape surrounding firearms in the United States would fundamentally shift. Debates over gun control would be reframed, the perceived legitimacy of armed militias would erode, and the state's capacity to regulate arms would be significantly enhanced, leading to a reorganization of power dynamics.
% FOUNDING_PROBLEM: The founding problem was the historical fear of tyrannical government, drawing from colonial experiences with British rule, and the perceived need for a popular check on centralized power to secure a free state.
% FOUNDING_PROBLEM_CORROBORATION: Historians and political theorists attest to the historical context of the founding problem. However, its contemporary status is highly contested: proponents of this reading argue the threat of tyranny is ever-present, while mainstream legal scholars and government officials argue that modern democratic states are fundamentally different from 18th-century monarchies, rendering the 'insurrectionist' justification obsolete. Independent analyses often highlight the self-serving nature of claims from the beneficiary group.
narrative_ontology:disappearance_verdict(second_amendment_boundary__insurrectionist_reading, world_rearranges).
narrative_ontology:founding_problem_status(second_amendment_boundary__insurrectionist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(second_amendment_boundary__insurrectionist_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(second_amendment_boundary__insurrectionist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(second_amendment_boundary__insurrectionist_reading, 0.85, 'gemini-2.5-flash', 'none', direct).

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
 *   Extractiveness is high (0.85) because this reading imposes significant costs on the state (e.g., inability to control arms, potential for civil unrest) and society (e.g., gun violence, fear). Suppression is very high (0.90) because the state must actively suppress any actualized insurrectionist activity, and this reading itself suppresses alternative political processes for its adherents. Theater ratio is low (0.10) as the threat of armed resistance, while often rhetorical, is taken seriously by state actors, and the costs are real. Accessibility collapse is high (0.75) as adherents may view political alternatives as insufficient or compromised, leading to a collapse of other options. Resistance is high (0.80) as the state actively resists the practical implications of this reading through legal challenges and enforcement actions.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of 'armed_citizens_claiming_deterrent_legitimacy,' this reading is a vital 'Rope' that coordinates defense against tyranny. From the perspective of 'state_security_apparatus' and 'civilians_in_conflict_zones,' it operates as a 'Snare,' extracting peace and safety, and requiring constant, costly suppression. The engine computes this divergence from the structural data; the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   The 'armed_citizens_claiming_deterrent_legitimacy' are the primary beneficiaries, gaining perceived power and legitimacy. The 'state_security_apparatus' and 'civilians_in_conflict_zones' are the clear victims, bearing the direct costs and risks of potential conflict. 'Government_officials' are agenda-setters who bear the burden of governing under this interpretation. Constitutional scholars are analytical observers.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    definition_of_tyranny,
    'What constitutes ''tyrannical government'' sufficient to justify armed resistance, and who adjudicates this definition?',
    'Establishment of clear, universally accepted criteria for tyranny, or a legitimate, non-partisan adjudicative body whose rulings are respected by all parties.',
    'If ''tyranny'' is subjectively defined by the beneficiary group, the constraint''s justification is self-serving, amplifying its extractive nature. If objectively defined, it could theoretically function as a legitimate check, reducing extraction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(definition_of_tyranny, conceptual, 'Ambiguity in the trigger condition for armed resistance.').

omega_variable(
    efficacy_of_armed_resistance,
    'Does individual possession of military-grade arms actually deter or successfully overthrow modern tyrannical governments, or does it primarily lead to civil unrest and civilian casualties?',
    'Empirical study of historical and contemporary instances of armed citizen resistance against state power, assessing outcomes, costs, and effectiveness.',
    'If empirically ineffective, the ''deterrent'' or ''overthrow'' function is theatrical, increasing the constraint''s theater_ratio and exposing its pure extractive nature (costs without function). If effective, it would support the ''Rope'' framing.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(efficacy_of_armed_resistance, empirical, 'Empirical question of whether armed citizenry actually deters tyranny.').

omega_variable(
    militia_clause_interpretation,
    'To what extent does the ''well regulated Militia'' clause condition or limit the ''right to keep and bear Arms'' in this reading?',
    'Further jurisprudential rulings or constitutional amendments that explicitly clarify the relationship between the prefatory and operative clauses.',
    'If the militia clause is found to impose significant conditions, it would undermine the individual, unconditioned right central to the insurrectionist reading, potentially reclassifying it towards a ''militia_conditioned_reading'' and reducing its perceived legitimacy and extractiveness.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(militia_clause_interpretation, conceptual, 'Ambiguity of the ''well regulated Militia'' clause''s role.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(second_amendment_boundary__insurrectionist_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(seco_tr_t0, second_amendment_boundary__insurrectionist_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(seco_tr_t6, second_amendment_boundary__insurrectionist_reading, theater_ratio, 6, 0.13).
narrative_ontology:measurement(seco_tr_t12, second_amendment_boundary__insurrectionist_reading, theater_ratio, 12, 0.12).
narrative_ontology:measurement(seco_tr_t18, second_amendment_boundary__insurrectionist_reading, theater_ratio, 18, 0.11).
narrative_ontology:measurement(seco_tr_t24, second_amendment_boundary__insurrectionist_reading, theater_ratio, 24, 0.1).
narrative_ontology:measurement(seco_tr_t30, second_amendment_boundary__insurrectionist_reading, theater_ratio, 30, 0.1).

% Extraction over time
narrative_ontology:measurement(seco_be_t0, second_amendment_boundary__insurrectionist_reading, base_extractiveness, 0, 0.75).
narrative_ontology:measurement(seco_be_t6, second_amendment_boundary__insurrectionist_reading, base_extractiveness, 6, 0.78).
narrative_ontology:measurement(seco_be_t12, second_amendment_boundary__insurrectionist_reading, base_extractiveness, 12, 0.81).
narrative_ontology:measurement(seco_be_t18, second_amendment_boundary__insurrectionist_reading, base_extractiveness, 18, 0.83).
narrative_ontology:measurement(seco_be_t24, second_amendment_boundary__insurrectionist_reading, base_extractiveness, 24, 0.84).
narrative_ontology:measurement(seco_be_t30, second_amendment_boundary__insurrectionist_reading, base_extractiveness, 30, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(seco_su_t0, second_amendment_boundary__insurrectionist_reading, suppression_requirement, 0, 0.8).
narrative_ontology:measurement(seco_su_t6, second_amendment_boundary__insurrectionist_reading, suppression_requirement, 6, 0.83).
narrative_ontology:measurement(seco_su_t12, second_amendment_boundary__insurrectionist_reading, suppression_requirement, 12, 0.86).
narrative_ontology:measurement(seco_su_t18, second_amendment_boundary__insurrectionist_reading, suppression_requirement, 18, 0.88).
narrative_ontology:measurement(seco_su_t24, second_amendment_boundary__insurrectionist_reading, suppression_requirement, 24, 0.89).
narrative_ontology:measurement(seco_su_t30, second_amendment_boundary__insurrectionist_reading, suppression_requirement, 30, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(second_amendment_boundary__insurrectionist_reading, identity_coordination).
narrative_ontology:affects_constraint(second_amendment_boundary__insurrectionist_reading, second_amendment_boundary__individual_right_reading).
narrative_ontology:affects_constraint(second_amendment_boundary__insurrectionist_reading, second_amendment_boundary__militia_conditioned_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three distinct readings of the 'second_amendment_boundary' kernel. Each reading instantiates a different constraint with unique structural properties and ε values.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
