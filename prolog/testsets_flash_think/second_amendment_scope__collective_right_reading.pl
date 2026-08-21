% ============================================================================
% CONSTRAINT STORY: second_amendment_scope__collective_right_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_second_amendment_scope__collective_right_reading, []).

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
 *   constraint_id: second_amendment_scope__collective_right_reading
 *   human_readable: Second Amendment: Collective Right Reading (State Militia Authority)
 *   domain: constitutional_law/political_theory/rights_jurisprudence
 *
 * SUMMARY:
 *   This constraint story instantiates the 'collective right' reading of the
 *   Second Amendment, which interprets the amendment as protecting the right
 *   of states to maintain militias, rather than an individual's right to own
 *   firearms for private purposes. Under this reading, state governments are
 *   the primary beneficiaries, wielding broad authority to regulate firearms
 *   to ensure public safety and the effectiveness of their militias.
 *   Individual firearms owners, particularly those not affiliated with a
 *   militia, are subject to these regulations and are considered 'payers' in
 *   this framework. The metrics reflect a relatively low extractiveness and
 *   theater, as the constraint is seen as a legitimate exercise of state
 *   power for coordination, but with moderate suppression due to the
 *   regulatory implications for individuals.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(second_amendment_scope__collective_right_reading, 0.15).
domain_priors:suppression_score(second_amendment_scope__collective_right_reading, 0.4).
domain_priors:theater_ratio(second_amendment_scope__collective_right_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(second_amendment_scope__collective_right_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(second_amendment_scope__collective_right_reading, suppression_requirement, 0.4).
narrative_ontology:constraint_metric(second_amendment_scope__collective_right_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(second_amendment_scope__collective_right_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(second_amendment_scope__collective_right_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(second_amendment_scope__collective_right_reading, rope).
narrative_ontology:human_readable(second_amendment_scope__collective_right_reading, "Second Amendment: Collective Right Reading (State Militia Authority)").
narrative_ontology:topic_domain(second_amendment_scope__collective_right_reading, "constitutional_law/political_theory/rights_jurisprudence").

domain_priors:requires_active_enforcement(second_amendment_scope__collective_right_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(second_amendment_scope__collective_right_reading, '3317ed86-fcad-4b46-be46-630fff2bd46f').
narrative_ontology:cs_kernel_codification('3317ed86-fcad-4b46-be46-630fff2bd46f', fixed_text).
narrative_ontology:cs_authority_grounding('3317ed86-fcad-4b46-be46-630fff2bd46f', lineage).
narrative_ontology:cs_interpretation_layer_present('3317ed86-fcad-4b46-be46-630fff2bd46f').
narrative_ontology:cs_reading_relation('3317ed86-fcad-4b46-be46-630fff2bd46f', second_amendment_scope__individual_right_reading, forecloses).
narrative_ontology:cs_reading_relation('3317ed86-fcad-4b46-be46-630fff2bd46f', second_amendment_scope__civic_right_reading, forecloses).
narrative_ontology:cs_axiom('3317ed86-fcad-4b46-be46-630fff2bd46f', foundational, militia_clause_primary).
narrative_ontology:cs_axiom_status(militia_clause_primary, holdable).
narrative_ontology:cs_axiom_grounding('3317ed86-fcad-4b46-be46-630fff2bd46f', militia_clause_primary, conventional).
narrative_ontology:cs_axiom('3317ed86-fcad-4b46-be46-630fff2bd46f', foundational, state_sovereignty_over_arms).
narrative_ontology:cs_axiom_status(state_sovereignty_over_arms, holdable).
narrative_ontology:cs_axiom_grounding('3317ed86-fcad-4b46-be46-630fff2bd46f', state_sovereignty_over_arms, deontological).
narrative_ontology:cs_reference_frame('3317ed86-fcad-4b46-be46-630fff2bd46f', original_collective_intent).
narrative_ontology:cs_drift_state('3317ed86-fcad-4b46-be46-630fff2bd46f', contemporary_judicial_interpretations, gap(repudiation_pressure, severe, true)).
narrative_ontology:cs_created_at('3317ed86-fcad-4b46-be46-630fff2bd46f', '').
narrative_ontology:cs_kernel_id(second_amendment_scope__collective_right_reading, second_amendment_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(second_amendment_scope__collective_right_reading, state_governments).
narrative_ontology:constraint_beneficiary(second_amendment_scope__collective_right_reading, organized_militias).
narrative_ontology:constraint_victim(second_amendment_scope__collective_right_reading, individual_firearms_owners).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Under this reading, states retain the authority to organize, arm, and discipline their militias, and to regulate civilian possession of firearms to ensure public safety and the effectiveness of the militia. They benefit from broad regulatory power.
narrative_ontology:constraint_stakeholder(second_amendment_scope__collective_right_reading, state_governments, agenda_setter,
    institutional, generational, constrained, national).

% These entities, often state-controlled National Guards, are the direct beneficiaries of the Second Amendment's protection under this reading, as it ensures their existence and the state's power to maintain them. They rely on state support and regulation.
narrative_ontology:constraint_stakeholder(second_amendment_scope__collective_right_reading, organized_militias, beneficiary,
    organized, biographical, constrained, regional).

% Individuals who own firearms for personal use, unconnected to militia service, are subject to state regulation under this reading. Their ability to own certain types of arms or carry them in certain ways is contingent on state law, not an inherent right. They bear the cost of restricted access.
narrative_ontology:constraint_stakeholder(second_amendment_scope__collective_right_reading, individual_firearms_owners, payer,
    powerless, biographical, constrained, national).

% The federal courts, particularly the Supreme Court, are the ultimate interpreters of the Second Amendment. Historically, this reading was dominant in their jurisprudence, but recent decisions have shifted away from it. Their interpretation shapes the constraint's application.
narrative_ontology:constraint_stakeholder(second_amendment_scope__collective_right_reading, federal_judiciary, agenda_setter,
    institutional, civilizational, analytical, universal).

% These groups actively campaign for an individual right to bear arms, directly opposing the collective right reading. Under this reading, their core claim is denied, and they are excluded from the constitutional framework that would grant such a right. They are a source of resistance.
narrative_ontology:constraint_stakeholder(second_amendment_scope__collective_right_reading, gun_rights_advocates, excluded,
    organized, biographical, constrained, national).

% Academics who analyze the historical context, text, and evolving jurisprudence of the Second Amendment. They observe and critique the various readings, including the collective right interpretation, without directly benefiting or paying.
narrative_ontology:constraint_stakeholder(second_amendment_scope__collective_right_reading, constitutional_scholars, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To coordinate state power in maintaining a well-regulated militia for public safety and defense, ensuring a disciplined force available to the state.
% TRANSFER_FUNCTION: Transfers the primary authority and responsibility for firearms regulation from individuals to state governments, in service of the militia's function.
% ABSENT_VOICES: Individual gun owners and gun rights advocacy groups, who would argue for an inherent individual right to bear arms, independent of militia service, and would object to broad state regulatory authority.
% DISAPPEARANCE_RATIONALE: If this reading vanished, the legal landscape for firearms regulation would be fundamentally altered, likely shifting decisively towards an individual rights interpretation. This would lead to significant changes in state and federal firearms laws, potentially impacting public safety, state defense capabilities, and the balance of power between individuals and the state regarding arms.
% FOUNDING_PROBLEM: To ensure that states retained the capacity to maintain effective militias for defense against foreign invasion and domestic insurrection, preventing both federal disarmament of state forces and uncontrolled private arms that could threaten public order.
% FOUNDING_PROBLEM_CORROBORATION: Historical texts from the founding era, early legal commentaries (e.g., St. George Tucker), and some modern constitutional historians corroborate that the original intent was primarily collective, focusing on the militia. However, this is heavily contested by individual rights advocates and some legal scholars, particularly after recent Supreme Court decisions.
narrative_ontology:disappearance_verdict(second_amendment_scope__collective_right_reading, world_rearranges).
narrative_ontology:founding_problem_status(second_amendment_scope__collective_right_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(second_amendment_scope__collective_right_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(second_amendment_scope__collective_right_reading, 'none', 1).
narrative_ontology:epsilon_provenance(second_amendment_scope__collective_right_reading, 0.15, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(second_amendment_scope__collective_right_reading_tests).
:- end_tests(second_amendment_scope__collective_right_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness is low (0.15) because this reading frames the amendment as a structural feature of federalism and state power, not primarily as a mechanism for extracting resources from individuals. Suppression is moderate (0.40) because while it grants states significant regulatory power, it doesn't completely prohibit private ownership, only subjects it to state control. Theater ratio is low (0.10) as the interpretation is a direct, functional reading of the text's militia clause. Accessibility collapse is moderate (0.40) because it limits individual claims but doesn't eliminate all forms of gun ownership. Resistance is high (0.60) because this reading is heavily contested by individual rights advocates, especially in the modern era.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of state governments and organized militias, this reading provides essential coordination for public safety and defense, with minimal extraction. From the perspective of individual firearms owners, it represents a significant limitation on their perceived rights, imposing costs through regulation and potential prohibition. The federal judiciary's historical adherence to this reading, and its recent departure, highlights the contested nature of this perspectival gap.
 *
 * DIRECTIONALITY LOGIC:
 *   State governments and organized militias are the clear beneficiaries (low d) as this reading affirms their authority and existence. Individual firearms owners are the targets (high d) as their rights are subordinated to state power and regulation. The federal judiciary acts as an agenda-setter, interpreting the scope of this right, while gun rights advocates are excluded voices whose core claims are denied by this interpretation.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identification,
    'Is this constraint accurately identified as the ''collective_right_reading'' of the ''second_amendment_scope'' kernel?',
    'Comparison with historical legal scholarship and judicial opinions explicitly articulating this interpretation.',
    'If misidentified, the entire analysis of its relationship to sibling readings and its drift state would be compromised.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identification, conceptual, 'Confirms the specific reading being analyzed.').

omega_variable(
    founding_intent_ambiguity,
    'Was the original intent of the Second Amendment primarily collective (militia-focused) or individual (private arms ownership)?',
    'Further historical and textual analysis of founding-era documents, debates, and state constitutional provisions, seeking consensus among constitutional historians.',
    'If resolved as primarily individual, the ''collective_right_reading'' would be reclassified as a ''snare'' or ''tangled_rope'' that misrepresents original intent to justify state control; if resolved as primarily collective, it reinforces the ''rope'' classification.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(founding_intent_ambiguity, empirical, 'Ambiguity regarding the original intent of the Second Amendment.').

omega_variable(
    structural_delta_from_individual_right,
    'How does this ''collective_right_reading'' structurally differ from the ''individual_right_reading''?',
    'Direct comparison of beneficiary/victim sets, scope of rights, and regulatory authority implied by each reading.',
    'The ''collective_right_reading'' forecloses the ''individual_right_reading'' by denying the premise of an individual right unconnected to militia service. If this foreclosure is not structurally robust, the relationship between readings would need re-evaluation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(structural_delta_from_individual_right, conceptual, 'Structural differences between collective and individual right readings.').

omega_variable(
    structural_delta_from_civic_right,
    'How does this ''collective_right_reading'' structurally differ from the ''civic_right_reading''?',
    'Direct comparison of the emphasis on state authority versus individual participation, and the scope of individual rights granted (or denied) by each reading.',
    'The ''collective_right_reading'' forecloses the ''civic_right_reading'' by denying any individual right, even one conditioned on militia service, emphasizing state control over individual participation. If this foreclosure is not structurally robust, the relationship between readings would need re-evaluation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(structural_delta_from_civic_right, conceptual, 'Structural differences between collective and civic right readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(second_amendment_scope__collective_right_reading, 1791, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(seco_tr_t1791, second_amendment_scope__collective_right_reading, theater_ratio, 1791, 0.05).
narrative_ontology:measurement(seco_tr_t1850, second_amendment_scope__collective_right_reading, theater_ratio, 1850, 0.07).
narrative_ontology:measurement(seco_tr_t1900, second_amendment_scope__collective_right_reading, theater_ratio, 1900, 0.08).
narrative_ontology:measurement(seco_tr_t1950, second_amendment_scope__collective_right_reading, theater_ratio, 1950, 0.09).
narrative_ontology:measurement(seco_tr_t2000, second_amendment_scope__collective_right_reading, theater_ratio, 2000, 0.1).
narrative_ontology:measurement(seco_tr_t2024, second_amendment_scope__collective_right_reading, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(seco_be_t1791, second_amendment_scope__collective_right_reading, base_extractiveness, 1791, 0.1).
narrative_ontology:measurement(seco_be_t1850, second_amendment_scope__collective_right_reading, base_extractiveness, 1850, 0.12).
narrative_ontology:measurement(seco_be_t1900, second_amendment_scope__collective_right_reading, base_extractiveness, 1900, 0.13).
narrative_ontology:measurement(seco_be_t1950, second_amendment_scope__collective_right_reading, base_extractiveness, 1950, 0.14).
narrative_ontology:measurement(seco_be_t2000, second_amendment_scope__collective_right_reading, base_extractiveness, 2000, 0.15).
narrative_ontology:measurement(seco_be_t2024, second_amendment_scope__collective_right_reading, base_extractiveness, 2024, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(seco_su_t1791, second_amendment_scope__collective_right_reading, suppression_requirement, 1791, 0.2).
narrative_ontology:measurement(seco_su_t1850, second_amendment_scope__collective_right_reading, suppression_requirement, 1850, 0.25).
narrative_ontology:measurement(seco_su_t1900, second_amendment_scope__collective_right_reading, suppression_requirement, 1900, 0.3).
narrative_ontology:measurement(seco_su_t1950, second_amendment_scope__collective_right_reading, suppression_requirement, 1950, 0.35).
narrative_ontology:measurement(seco_su_t2000, second_amendment_scope__collective_right_reading, suppression_requirement, 2000, 0.4).
narrative_ontology:measurement(seco_su_t2024, second_amendment_scope__collective_right_reading, suppression_requirement, 2024, 0.4).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(second_amendment_scope__collective_right_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(second_amendment_scope__collective_right_reading, firearms_regulation_state_level).
narrative_ontology:affects_constraint(second_amendment_scope__collective_right_reading, federal_firearms_legislation).
narrative_ontology:affects_constraint(second_amendment_scope__collective_right_reading, second_amendment_scope__individual_right_reading).
narrative_ontology:affects_constraint(second_amendment_scope__collective_right_reading, second_amendment_scope__civic_right_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three distinct readings of the 'second_amendment_scope' kernel, each with different structural properties and classifications. They are linked to show their contested relationship within the same constitutional domain.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
