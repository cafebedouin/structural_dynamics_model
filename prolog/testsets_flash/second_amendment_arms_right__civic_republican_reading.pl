% ============================================================================
% CONSTRAINT STORY: second_amendment_arms_right__civic_republican_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_second_amendment_arms_right__civic_republican_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: second_amendment_arms_right__civic_republican_reading
 *   human_readable: Second Amendment Arms Right (Civic Republican Reading)
 *   domain: constitutional_law/political_philosophy/legal_interpretation
 *
 * SUMMARY:
 *   This constraint represents the civic republican reading of the Second
 *   Amendment, which interprets the right to keep and bear arms as
 *   intrinsically linked to the duty of citizens to participate in a
 *   well-regulated militia for the security of a free state. It is neither a
 *   purely individual right (as in the individual_right_reading) nor solely a
 *   state prerogative (as in the collective_right_reading). This reading
 *   emphasizes the collective good of self-governance through an armed
 *   citizenry, implying both rights and responsibilities for citizen-militia
 *   members, and allowing for moderate regulation that supports civic
 *   participation rather than suppressing it.
 *
 * KEY AGENTS:
 *   - citizen_militia_members: Primary beneficiary (right + duty) / moderate exit
 *   - republican_polity: Primary beneficiary (security) / institutional exit
 *   - state_legislatures: Agenda setter (regulation) / institutional exit
 *   - unqualified_citizens: Payer (bears exclusion) / constrained exit
 *   - overreaching_state_power: Victim (constrained) / institutional exit
 *   - individual_rights_advocates: Excluded (framing) / organized exit
 *   - collective_rights_advocates: Excluded (framing) / organized exit
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(second_amendment_arms_right__civic_republican_reading, 0.35).
domain_priors:suppression_score(second_amendment_arms_right__civic_republican_reading, 0.2).
domain_priors:theater_ratio(second_amendment_arms_right__civic_republican_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(second_amendment_arms_right__civic_republican_reading, extractiveness, 0.35).
narrative_ontology:constraint_metric(second_amendment_arms_right__civic_republican_reading, suppression_requirement, 0.2).
narrative_ontology:constraint_metric(second_amendment_arms_right__civic_republican_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(second_amendment_arms_right__civic_republican_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(second_amendment_arms_right__civic_republican_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(second_amendment_arms_right__civic_republican_reading, rope).
narrative_ontology:human_readable(second_amendment_arms_right__civic_republican_reading, "Second Amendment Arms Right (Civic Republican Reading)").
narrative_ontology:topic_domain(second_amendment_arms_right__civic_republican_reading, "constitutional_law/political_philosophy/legal_interpretation").

domain_priors:requires_active_enforcement(second_amendment_arms_right__civic_republican_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(second_amendment_arms_right__civic_republican_reading, '00468e6e-257d-42d0-8fd5-31530509d5d3').
narrative_ontology:cs_kernel_codification('00468e6e-257d-42d0-8fd5-31530509d5d3', fixed_text).
narrative_ontology:cs_authority_grounding('00468e6e-257d-42d0-8fd5-31530509d5d3', lineage).
narrative_ontology:cs_interpretation_layer_present('00468e6e-257d-42d0-8fd5-31530509d5d3').
narrative_ontology:cs_reading_relation('00468e6e-257d-42d0-8fd5-31530509d5d3', second_amendment_arms_right__individual_right_reading, coexists_with).
narrative_ontology:cs_reading_relation('00468e6e-257d-42d0-8fd5-31530509d5d3', second_amendment_arms_right__collective_right_reading, coexists_with).
narrative_ontology:cs_axiom('00468e6e-257d-42d0-8fd5-31530509d5d3', foundational, armed_citizenry_for_republican_self_governance).
narrative_ontology:cs_axiom_status(armed_citizenry_for_republican_self_governance, holdable).
narrative_ontology:cs_axiom_grounding('00468e6e-257d-42d0-8fd5-31530509d5d3', armed_citizenry_for_republican_self_governance, deontological).
narrative_ontology:cs_axiom('00468e6e-257d-42d0-8fd5-31530509d5d3', foundational, well_regulated_militia_is_civic_duty).
narrative_ontology:cs_axiom_status(well_regulated_militia_is_civic_duty, holdable).
narrative_ontology:cs_axiom_grounding('00468e6e-257d-42d0-8fd5-31530509d5d3', well_regulated_militia_is_civic_duty, conventional).
narrative_ontology:cs_reference_frame('00468e6e-257d-42d0-8fd5-31530509d5d3', founding_era_civic_republicanism).
narrative_ontology:cs_drift_state('00468e6e-257d-42d0-8fd5-31530509d5d3', contemporary_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('00468e6e-257d-42d0-8fd5-31530509d5d3', '').
narrative_ontology:cs_kernel_id(second_amendment_arms_right__civic_republican_reading, second_amendment_arms_right).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(second_amendment_arms_right__civic_republican_reading, citizen_militia_members).
narrative_ontology:constraint_beneficiary(second_amendment_arms_right__civic_republican_reading, republican_polity).
narrative_ontology:constraint_victim(second_amendment_arms_right__civic_republican_reading, unqualified_citizens).
narrative_ontology:constraint_victim(second_amendment_arms_right__civic_republican_reading, overreaching_state_power).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(second_amendment_arms_right__civic_republican_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(second_amendment_arms_right__civic_republican_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(second_amendment_arms_right__civic_republican_reading_tests).
:- end_tests(second_amendment_arms_right__civic_republican_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.35) as there are costs associated with training and qualification for armed citizenship, but these are framed as duties for the common good. Suppression is low (0.20) because the constraint aims to enable, not suppress, armed citizenship, albeit within a 'well-regulated' framework. Theater ratio is low (0.10) as the civic republican ideal is actively pursued through policy and discourse, not merely performed. Accessibility collapse is moderate (0.60) because while the right is broadly accessible, it is not absolute and requires adherence to civic duties. Resistance is moderate (0.40) due to ongoing debates about the precise scope of 'well-regulated' and the balance between individual liberty and collective security.
 *
 * PERSPECTIVAL GAP:
 *   Citizen-militia members experience this as a beneficial framework that grants rights while imposing reasonable duties. Unqualified citizens, however, experience it as a barrier to arms ownership. State legislatures, while empowered to regulate, are also constrained by the underlying civic republican principle, preventing arbitrary disarmament. The engine's per-seat classification will reflect these divergences.
 *
 * DIRECTIONALITY LOGIC:
 *   Citizen-militia members are beneficiaries (d near 0.0) as they gain the right to bear arms and participate in self-governance, even with associated duties. The republican polity is also a beneficiary (d near 0.0) as it gains security and popular sovereignty. Unqualified citizens are payers (d near 1.0) as they are excluded from armed citizenship due to lack of qualification. Overreaching state power is a victim (d near 1.0) as its ability to disarm the populace is constrained. State legislatures are agenda-setters, balancing regulation with the civic right.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading prevents mislabeling by explicitly linking the right to a civic duty and a collective purpose. If the 'well-regulated' aspect atrophied or the civic duty became purely performative, it would drift towards an individual_right_reading (more extractive for the state, less for individuals) or a piton (if the civic purpose became entirely theatrical). The current framing, with its moderate extractiveness and low suppression, suggests a functional, albeit contested, constraint.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identification,
    'Is this constraint a genuine civic republican reading of the Second Amendment, or is it a hybrid of individual and collective rights framings?',
    'Analysis of judicial opinions and legislative history that explicitly articulate the ''armed citizenship for self-governance'' principle, distinct from purely individual or state-centered interpretations.',
    'If a hybrid, the classification would shift to reflect the dominant underlying framing (e.g., more individual-right-like if individual liberty is prioritized over civic duty, or more collective-right-like if state control is emphasized).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_identification, conceptual, 'Ambiguity in the precise boundaries of the civic republican reading.').

omega_variable(
    regulatory_scope_ambiguity,
    'What specific types of arms regulations are permissible under this civic republican reading, and how do they balance public safety with the right/duty of armed citizenship?',
    'Further judicial clarification or legislative action that defines the scope of ''well-regulated'' in the context of civic republicanism, particularly regarding training, storage, and types of arms.',
    'If regulations are found to be overly restrictive, the constraint''s suppression and extractiveness would increase for citizen-militia members; if too permissive, the ''well-regulated'' aspect would atrophy, potentially shifting the constraint towards a more individualistic interpretation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_scope_ambiguity, empirical, 'Uncertainty regarding the practical limits of state regulatory authority.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(second_amendment_arms_right__civic_republican_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(seco_tr_t0, second_amendment_arms_right__civic_republican_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(seco_tr_t10, second_amendment_arms_right__civic_republican_reading, theater_ratio, 10, 0.1).
narrative_ontology:measurement(seco_tr_t20, second_amendment_arms_right__civic_republican_reading, theater_ratio, 20, 0.1).

% Extraction over time
narrative_ontology:measurement(seco_be_t0, second_amendment_arms_right__civic_republican_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(seco_be_t10, second_amendment_arms_right__civic_republican_reading, base_extractiveness, 10, 0.32).
narrative_ontology:measurement(seco_be_t20, second_amendment_arms_right__civic_republican_reading, base_extractiveness, 20, 0.35).

% Suppression requirement over time
narrative_ontology:measurement(seco_su_t0, second_amendment_arms_right__civic_republican_reading, suppression_requirement, 0, 0.15).
narrative_ontology:measurement(seco_su_t10, second_amendment_arms_right__civic_republican_reading, suppression_requirement, 10, 0.18).
narrative_ontology:measurement(seco_su_t20, second_amendment_arms_right__civic_republican_reading, suppression_requirement, 20, 0.2).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(second_amendment_arms_right__civic_republican_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(second_amendment_arms_right__civic_republican_reading, 0.08).
narrative_ontology:affects_constraint(second_amendment_arms_right__civic_republican_reading, second_amendment_arms_right__individual_right_reading).
narrative_ontology:affects_constraint(second_amendment_arms_right__civic_republican_reading, second_amendment_arms_right__collective_right_reading).
narrative_ontology:affects_constraint(second_amendment_arms_right__civic_republican_reading, state_militia_funding).
narrative_ontology:affects_constraint(second_amendment_arms_right__civic_republican_reading, firearms_training_standards).

% DUAL FORMULATION NOTE:
% This constraint is one of three distinct readings of the Second Amendment arms right kernel. Each reading has a unique structural profile and is modeled as a separate constraint, linked here for network analysis.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
