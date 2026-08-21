% ============================================================================
% CONSTRAINT STORY: second_amendment_arms_right__collective_right_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_second_amendment_arms_right__collective_right_reading, []).

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
 *   constraint_id: second_amendment_arms_right__collective_right_reading
 *   human_readable: Second Amendment: Collective Right Reading (State Militia Authority)
 *   domain: Constitutional Law / Political Philosophy / Legal Interpretation
 *
 * SUMMARY:
 *   This constraint represents the 'collective right' reading of the Second
 *   Amendment, which interprets the right to keep and bear arms as primarily
 *   protecting the authority of state governments to maintain well-regulated
 *   militias, rather than an individual's right to own firearms for any
 *   purpose outside of militia service. Under this reading, individual gun
 *   ownership is subject to broad state and federal regulation. The claimed
 *   type is 'rope' because it coordinates state power for collective
 *   security, with minimal extraction from the states themselves.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(second_amendment_arms_right__collective_right_reading, 0.15).
domain_priors:suppression_score(second_amendment_arms_right__collective_right_reading, 0.25).
domain_priors:theater_ratio(second_amendment_arms_right__collective_right_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(second_amendment_arms_right__collective_right_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(second_amendment_arms_right__collective_right_reading, suppression_requirement, 0.25).
narrative_ontology:constraint_metric(second_amendment_arms_right__collective_right_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(second_amendment_arms_right__collective_right_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(second_amendment_arms_right__collective_right_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(second_amendment_arms_right__collective_right_reading, rope).
narrative_ontology:human_readable(second_amendment_arms_right__collective_right_reading, "Second Amendment: Collective Right Reading (State Militia Authority)").
narrative_ontology:topic_domain(second_amendment_arms_right__collective_right_reading, "Constitutional Law / Political Philosophy / Legal Interpretation").

domain_priors:requires_active_enforcement(second_amendment_arms_right__collective_right_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(second_amendment_arms_right__collective_right_reading, '29ff5285-4e05-49dc-9152-3a04056e682c').
narrative_ontology:cs_kernel_codification('29ff5285-4e05-49dc-9152-3a04056e682c', fixed_text).
narrative_ontology:cs_authority_grounding('29ff5285-4e05-49dc-9152-3a04056e682c', lineage).
narrative_ontology:cs_interpretation_layer_present('29ff5285-4e05-49dc-9152-3a04056e682c').
narrative_ontology:cs_reading_relation('29ff5285-4e05-49dc-9152-3a04056e682c', second_amendment_arms_right__individual_right_reading, forecloses).
narrative_ontology:cs_reading_relation('29ff5285-4e05-49dc-9152-3a04056e682c', second_amendment_arms_right__civic_republican_reading, coexists_with).
narrative_ontology:cs_axiom('29ff5285-4e05-49dc-9152-3a04056e682c', foundational, state_militia_paramount).
narrative_ontology:cs_axiom_status(state_militia_paramount, holdable).
narrative_ontology:cs_axiom_grounding('29ff5285-4e05-49dc-9152-3a04056e682c', state_militia_paramount, conventional).
narrative_ontology:cs_axiom('29ff5285-4e05-49dc-9152-3a04056e682c', foundational, individual_arms_subordinate_to_state_power).
narrative_ontology:cs_axiom_status(individual_arms_subordinate_to_state_power, holdable).
narrative_ontology:cs_axiom_grounding('29ff5285-4e05-49dc-9152-3a04056e682c', individual_arms_subordinate_to_state_power, conventional).
narrative_ontology:cs_reference_frame('29ff5285-4e05-49dc-9152-3a04056e682c', original_ratification_intent).
narrative_ontology:cs_drift_state('29ff5285-4e05-49dc-9152-3a04056e682c', contemporary_judicial_era, gap(axiom_overriding, substantial, true)).
narrative_ontology:cs_created_at('29ff5285-4e05-49dc-9152-3a04056e682c', '').
narrative_ontology:cs_kernel_id(second_amendment_arms_right__collective_right_reading, second_amendment_arms_right).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(second_amendment_arms_right__collective_right_reading, state_governments).
narrative_ontology:constraint_beneficiary(second_amendment_arms_right__collective_right_reading, federal_government).
narrative_ontology:constraint_beneficiary(second_amendment_arms_right__collective_right_reading, gun_control_advocates).
narrative_ontology:constraint_victim(second_amendment_arms_right__collective_right_reading, individual_gun_owners).
narrative_ontology:constraint_vindicates(second_amendment_arms_right__collective_right_reading, state_sovereignty_doctrine).
narrative_ontology:constraint_vindicates(second_amendment_arms_right__collective_right_reading, public_safety_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Under this reading, state governments possess the authority to organize and maintain militias, and to regulate the private ownership of arms as necessary for public safety and the effective functioning of these militias. They benefit from clear regulatory power.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__collective_right_reading, state_governments, agenda_setter,
    institutional, generational, mobile, national).

% The federal government benefits from a clear understanding that the Second Amendment does not impede its ability to regulate firearms, particularly outside the context of state-organized militias, thus supporting its broader regulatory powers.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__collective_right_reading, federal_government, beneficiary,
    institutional, generational, mobile, national).

% Individual citizens who wish to own firearms outside of an organized militia context find their right to do so subject to plenary state and federal regulation. Their ability to acquire and possess certain types of arms is curtailed, and they bear the costs of compliance with various laws.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__collective_right_reading, individual_gun_owners, payer,
    powerless, biographical, constrained, national).

% Advocates for an expansive individual right to bear arms are structurally excluded from the core logic of this reading, which prioritizes collective state power. Their arguments for unfettered individual ownership are not recognized as central to the Second Amendment's purpose.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__collective_right_reading, gun_rights_advocates, excluded,
    organized, biographical, constrained, national).

% Advocates for stricter gun control measures benefit from this reading, as it provides a strong constitutional basis for state and federal governments to enact broad regulations on firearms, aligning with their policy goals for public safety.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__collective_right_reading, gun_control_advocates, beneficiary,
    organized, biographical, mobile, national).

% Scholars who analyze constitutional text and history from an academic perspective, often debating the original intent and evolving interpretations of the Second Amendment. They observe the legal and political contestation without directly participating as a party to the constraint.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__collective_right_reading, constitutional_scholars, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To coordinate the authority of state governments to maintain well-regulated militias and enact arms regulations for public safety, preventing the proliferation of private armies and ensuring collective security.
% TRANSFER_FUNCTION: Transfers primary regulatory authority over private arms from individuals to state and federal governments, enabling the latter to implement public safety measures and ensure the effectiveness of state militias.
% ABSENT_VOICES: Individual rights advocates and those who believe in an inherent, pre-political right to self-defense outside state control are largely absent from the foundational premises of this reading. They would argue that this interpretation unduly subordinates individual liberty to state power.
% DISAPPEARANCE_RATIONALE: If this collective right reading vanished, the legal landscape for gun control would be fundamentally altered. State and federal governments would lose a significant basis for regulating firearms, likely leading to a substantial expansion of individual gun rights and a forced rearrangement of public safety frameworks.
% FOUNDING_PROBLEM: The Second Amendment was established to ensure the security of free states by allowing them to maintain well-regulated militias, addressing concerns about standing armies and potential federal overreach regarding state defense capabilities, while also preventing private individuals from forming unregulated armed groups.
% FOUNDING_PROBLEM_CORROBORATION: Historical legal scholars and some contemporary constitutional law experts corroborate that the primary concern at the time of the Second Amendment's drafting was state security and militia organization, not an unfettered individual right. This is supported by early state constitutional provisions and historical context from outside the direct beneficiaries of this reading.
narrative_ontology:disappearance_verdict(second_amendment_arms_right__collective_right_reading, world_rearranges).
narrative_ontology:founding_problem_status(second_amendment_arms_right__collective_right_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(second_amendment_arms_right__collective_right_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(second_amendment_arms_right__collective_right_reading, 'none', 1).
narrative_ontology:epsilon_provenance(second_amendment_arms_right__collective_right_reading, 0.15, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(second_amendment_arms_right__collective_right_reading_tests).
:- end_tests(second_amendment_arms_right__collective_right_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.15) because, from the perspective of this reading, the regulation of individual arms is a legitimate exercise of state power for collective security, not an undue extraction. Suppression (0.25) is also relatively low for the states, as their authority is affirmed, but it does represent a suppression of individual actions. Theater ratio is very low (0.10) as the core function of enabling state militias and public safety regulation is genuine. Accessibility collapse (0.70) is high for individuals seeking unregulated arms ownership, as this reading collapses those alternatives. Resistance (0.60) is significant due to ongoing challenges from individual rights advocates.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of state governments, this reading is a clear 'rope' that coordinates their authority for public safety. From the perspective of individual gun owners, however, it functions as a 'snare' or 'tangled rope' that extracts their liberty and imposes costs through regulation. The engine's per-seat classification will capture this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   State and federal governments are beneficiaries, gaining clear regulatory authority over firearms. Gun control advocates also benefit from the legal space this reading provides for their policy goals. Individual gun owners are the primary payers, as their right to bear arms is subordinated to state power and subject to regulation. Gun rights advocates are excluded, as their core arguments are not recognized by this interpretation.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    historical_intent_ambiguity,
    'Was the original historical intent of the Second Amendment exclusively collective (state militia-focused), or did it also encompass an individual right to bear arms?',
    'Further historical and linguistic analysis of founding-era documents, state constitutional provisions, and contemporary legal commentaries, seeking consensus among non-partisan historians.',
    'If a significant individual component is found to be part of the original intent, this reading''s claim to historical fidelity would be weakened, potentially shifting its classification towards a ''snare'' for individual owners, or requiring a more nuanced ''tangled_rope'' classification acknowledging both aspects.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(historical_intent_ambiguity, empirical, 'Ambiguity regarding the precise balance of collective vs. individual rights in the Second Amendment''s original intent.').

omega_variable(
    judicial_precedent_impact,
    'How does the Supreme Court''s ''individual right'' jurisprudence (e.g., Heller, McDonald) structurally impact the viability and persistence of the collective right reading in contemporary legal discourse?',
    'Analysis of subsequent lower court rulings, legislative actions, and scholarly reception to determine the extent to which the collective right reading continues to be applied or cited as authoritative.',
    'If the individual right jurisprudence has effectively superseded the collective right reading in practice, this constraint''s persistence would be primarily due to institutional inertia or theatrical maintenance, pushing it towards a ''piton'' or ''snare'' (if still actively enforced for extraction) rather than a ''rope''.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(judicial_precedent_impact, conceptual, 'The effect of modern individual-right judicial precedents on the collective right interpretation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(second_amendment_arms_right__collective_right_reading, 1791, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(seco_tr_t1791, second_amendment_arms_right__collective_right_reading, theater_ratio, 1791, 0.08).
narrative_ontology:measurement(seco_tr_t1850, second_amendment_arms_right__collective_right_reading, theater_ratio, 1850, 0.09).
narrative_ontology:measurement(seco_tr_t1900, second_amendment_arms_right__collective_right_reading, theater_ratio, 1900, 0.09).
narrative_ontology:measurement(seco_tr_t1950, second_amendment_arms_right__collective_right_reading, theater_ratio, 1950, 0.1).
narrative_ontology:measurement(seco_tr_t2000, second_amendment_arms_right__collective_right_reading, theater_ratio, 2000, 0.1).
narrative_ontology:measurement(seco_tr_t2024, second_amendment_arms_right__collective_right_reading, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(seco_be_t1791, second_amendment_arms_right__collective_right_reading, base_extractiveness, 1791, 0.1).
narrative_ontology:measurement(seco_be_t1850, second_amendment_arms_right__collective_right_reading, base_extractiveness, 1850, 0.12).
narrative_ontology:measurement(seco_be_t1900, second_amendment_arms_right__collective_right_reading, base_extractiveness, 1900, 0.13).
narrative_ontology:measurement(seco_be_t1950, second_amendment_arms_right__collective_right_reading, base_extractiveness, 1950, 0.14).
narrative_ontology:measurement(seco_be_t2000, second_amendment_arms_right__collective_right_reading, base_extractiveness, 2000, 0.15).
narrative_ontology:measurement(seco_be_t2024, second_amendment_arms_right__collective_right_reading, base_extractiveness, 2024, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(seco_su_t1791, second_amendment_arms_right__collective_right_reading, suppression_requirement, 1791, 0.2).
narrative_ontology:measurement(seco_su_t1850, second_amendment_arms_right__collective_right_reading, suppression_requirement, 1850, 0.22).
narrative_ontology:measurement(seco_su_t1900, second_amendment_arms_right__collective_right_reading, suppression_requirement, 1900, 0.23).
narrative_ontology:measurement(seco_su_t1950, second_amendment_arms_right__collective_right_reading, suppression_requirement, 1950, 0.24).
narrative_ontology:measurement(seco_su_t2000, second_amendment_arms_right__collective_right_reading, suppression_requirement, 2000, 0.25).
narrative_ontology:measurement(seco_su_t2024, second_amendment_arms_right__collective_right_reading, suppression_requirement, 2024, 0.25).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(second_amendment_arms_right__collective_right_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(second_amendment_arms_right__collective_right_reading, second_amendment_arms_right__individual_right_reading).
narrative_ontology:affects_constraint(second_amendment_arms_right__collective_right_reading, second_amendment_arms_right__civic_republican_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three distinct readings of the Second Amendment, each with different structural properties and classifications. They are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
