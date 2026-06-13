% ============================================================================
% CONSTRAINT STORY: church_turing_thesis__mathematical_definition_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_church_turing_thesis__mathematical_definition_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: church_turing_thesis__mathematical_definition_reading
 *   human_readable: Church-Turing Thesis (Mathematical Definition Reading)
 *   domain: philosophy_of_mathematics/computation
 *
 * SUMMARY:
 *   This constraint represents the 'mathematical definition' reading of the
 *   Church-Turing Thesis, which posits that the thesis is a stipulative
 *   definition of 'effective computability' and thus true by convention, not
 *   subject to empirical test. It serves as a foundational coordination
 *   mechanism for formal systems. This reading is distinct from
 *   interpretations that view the thesis as an empirical claim about physical
 *   reality or an epistemological boundary.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(church_turing_thesis__mathematical_definition_reading, 0.02).
domain_priors:suppression_score(church_turing_thesis__mathematical_definition_reading, 0.05).
domain_priors:theater_ratio(church_turing_thesis__mathematical_definition_reading, 0.01).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(church_turing_thesis__mathematical_definition_reading, extractiveness, 0.02).
narrative_ontology:constraint_metric(church_turing_thesis__mathematical_definition_reading, suppression_requirement, 0.05).
narrative_ontology:constraint_metric(church_turing_thesis__mathematical_definition_reading, theater_ratio, 0.01).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(church_turing_thesis__mathematical_definition_reading, accessibility_collapse, 0.95).
narrative_ontology:constraint_metric(church_turing_thesis__mathematical_definition_reading, resistance, 0.01).

% --- Constraint claim ---
narrative_ontology:constraint_claim(church_turing_thesis__mathematical_definition_reading, rope).
narrative_ontology:human_readable(church_turing_thesis__mathematical_definition_reading, "Church-Turing Thesis (Mathematical Definition Reading)").
narrative_ontology:topic_domain(church_turing_thesis__mathematical_definition_reading, "philosophy_of_mathematics/computation").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(church_turing_thesis__mathematical_definition_reading, 'f7303597-ec93-48d7-86b3-99e1c6163344').
narrative_ontology:cs_kernel_codification('f7303597-ec93-48d7-86b3-99e1c6163344', formalized).
narrative_ontology:cs_authority_grounding('f7303597-ec93-48d7-86b3-99e1c6163344', expertise).
narrative_ontology:cs_interpretation_layer_present('f7303597-ec93-48d7-86b3-99e1c6163344').
narrative_ontology:cs_reading_relation('f7303597-ec93-48d7-86b3-99e1c6163344', church_turing_thesis__physical_claim_reading, coexists_with).
narrative_ontology:cs_reading_relation('f7303597-ec93-48d7-86b3-99e1c6163344', church_turing_thesis__epistemological_boundary_reading, coexists_with).
narrative_ontology:cs_axiom('f7303597-ec93-48d7-86b3-99e1c6163344', foundational, computability_is_formally_definable).
narrative_ontology:cs_axiom_status(computability_is_formally_definable, holdable).
narrative_ontology:cs_axiom_grounding('f7303597-ec93-48d7-86b3-99e1c6163344', computability_is_formally_definable, conventional).
narrative_ontology:cs_axiom('f7303597-ec93-48d7-86b3-99e1c6163344', foundational, definitions_are_not_empirical).
narrative_ontology:cs_axiom_status(definitions_are_not_empirical, holdable).
narrative_ontology:cs_axiom_grounding('f7303597-ec93-48d7-86b3-99e1c6163344', definitions_are_not_empirical, deontological).
narrative_ontology:cs_reference_frame('f7303597-ec93-48d7-86b3-99e1c6163344', formal_mathematical_definition).
narrative_ontology:cs_drift_state('f7303597-ec93-48d7-86b3-99e1c6163344', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('f7303597-ec93-48d7-86b3-99e1c6163344', '').
narrative_ontology:cs_kernel_id(church_turing_thesis__mathematical_definition_reading, church_turing_thesis).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(church_turing_thesis__mathematical_definition_reading, mathematicians).
narrative_ontology:constraint_beneficiary(church_turing_thesis__mathematical_definition_reading, computer_scientists).
narrative_ontology:constraint_beneficiary(church_turing_thesis__mathematical_definition_reading, logicians).
narrative_ontology:constraint_vindicates(church_turing_thesis__mathematical_definition_reading, formal_systems_clarity).
narrative_ontology:constraint_vindicates(church_turing_thesis__mathematical_definition_reading, computability_theory_coherence).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefit from a clear, universally accepted definition of computability that allows for rigorous proofs and consistent theoretical development. They use the thesis as a foundational axiom in their work.
narrative_ontology:constraint_stakeholder(church_turing_thesis__mathematical_definition_reading, mathematicians, beneficiary,
    institutional, generational, mobile, global).

% Utilize the thesis as the bedrock for understanding the limits of what algorithms can achieve. It provides a stable conceptual framework for designing and analyzing computational systems.
narrative_ontology:constraint_stakeholder(church_turing_thesis__mathematical_definition_reading, computer_scientists, beneficiary,
    institutional, generational, mobile, global).

% Rely on the thesis for the formal development of computability theory and the study of decidability. It provides a precise target for their formal systems.
narrative_ontology:constraint_stakeholder(church_turing_thesis__mathematical_definition_reading, logicians, beneficiary,
    institutional, generational, mobile, global).

% Analyze the conceptual implications and different interpretations of the Church-Turing Thesis, including its status as a definition versus an empirical claim. They are not bound by its operational use but study its foundations.
narrative_ontology:constraint_stakeholder(church_turing_thesis__mathematical_definition_reading, philosophers_of_computation, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a shared, unambiguous definition of 'effective computability' for mathematicians, computer scientists, and logicians, enabling consistent discourse and rigorous proof within formal systems.
% TRANSFER_FUNCTION: Transfers conceptual clarity and definitional stability to the fields of computability theory and theoretical computer science, from the collective agreement of the mathematical community.
% ABSENT_VOICES: No voices are truly 'absent' from this reading, as it is a definitional convention. Those who dispute its status as a definition (e.g., claiming it's empirical) are engaging with different readings of the same kernel, not being excluded from this one.
% DISAPPEARANCE_RATIONALE: If the Church-Turing Thesis as a mathematical definition vanished, the foundational coherence of computability theory would collapse. There would be no agreed-upon formalization for 'effectively computable function,' leading to widespread ambiguity and hindering theoretical progress across mathematics and computer science.
% FOUNDING_PROBLEM: The problem of providing a precise, formal definition for the intuitive notion of 'effective computability' or 'algorithm' in the early 20th century, to enable rigorous mathematical study.
% FOUNDING_PROBLEM_CORROBORATION: The problem of formalizing intuitive concepts remains live in mathematics. The continued utility and universal acceptance of the Church-Turing Thesis as a definition by the vast majority of mathematicians and computer scientists, who are outside the 'beneficiary' set in the sense of collecting rents, corroborates its status. Its foundational role is attested in countless textbooks and research papers.
narrative_ontology:disappearance_verdict(church_turing_thesis__mathematical_definition_reading, world_rearranges).
narrative_ontology:founding_problem_status(church_turing_thesis__mathematical_definition_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(church_turing_thesis__mathematical_definition_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(church_turing_thesis__mathematical_definition_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(church_turing_thesis__mathematical_definition_reading_tests).
:- end_tests(church_turing_thesis__mathematical_definition_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is very low (0.02) because this reading of the thesis primarily provides conceptual clarity and a shared foundation, with negligible costs imposed on participants. Suppression is minimal (0.05) as adherence is driven by the utility of a consistent definition rather than coercion; alternatives are not suppressed but simply fall outside the agreed-upon definition. Theater ratio is negligible (0.01) as its function is purely definitional and directly serves its stated purpose. Accessibility collapse is high (0.95) because once the definition is accepted, there are no 'alternative' definitions of effective computability that can be used within the same formal framework without causing confusion. Resistance is very low (0.01) because the mathematical community largely accepts this definitional role.
 *
 * PERSPECTIVAL GAP:
 *   For this reading, there is little perspectival gap among those who accept it as a definition; all experience it as a beneficial coordination. The primary 'gap' exists between this reading and other interpretations of the Church-Turing Thesis (e.g., as an empirical claim), which are modeled as separate constraints.
 *
 * DIRECTIONALITY LOGIC:
 *   Mathematicians, computer scientists, and logicians are all beneficiaries (d near 0.0) as they gain a stable, unambiguous foundation for their work. There are no identifiable victims, as a definition cannot 'extract' from or 'suppress' agents in the same way a policy or physical law might. Philosophers of computation act as observers, analyzing its status without being directly governed by its operational use.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    status_as_definition_vs_claim,
    'Is the Church-Turing Thesis fundamentally a mathematical definition, an empirical claim about physical reality, or an epistemological boundary?',
    'Conceptual analysis of its role in proofs and scientific practice, and philosophical debate on the nature of mathematical objects and physical computation. No single empirical test can resolve this philosophical ambiguity.',
    'If reclassified as an empirical claim, its extractiveness and suppression might increase (e.g., if it limits physical computing beyond current understanding); if as an epistemological boundary, its accessibility collapse might be higher. As a definition, it remains a Rope.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(status_as_definition_vs_claim, conceptual, 'Ambiguity regarding the fundamental status of the Church-Turing Thesis.').

omega_variable(
    definitional_scope_ambiguity,
    'Does the ''mathematical definition'' reading implicitly foreclose or merely coexist with the ''physical claim'' reading, or do they address different domains?',
    'Further philosophical analysis of the relationship between mathematical formalisms and physical instantiations of computation. Can a definition about formal systems constrain claims about physical systems?',
    'If this reading forecloses the physical claim, it implies a stronger conceptual boundary. If they merely coexist, it suggests the thesis operates on different levels of analysis without direct contradiction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(definitional_scope_ambiguity, conceptual, 'Relationship between the mathematical definition and physical interpretations.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(church_turing_thesis__mathematical_definition_reading, 1936, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(chur_tr_t1936, church_turing_thesis__mathematical_definition_reading, theater_ratio, 1936, 0.01).
narrative_ontology:measurement(chur_tr_t1950, church_turing_thesis__mathematical_definition_reading, theater_ratio, 1950, 0.01).
narrative_ontology:measurement(chur_tr_t1975, church_turing_thesis__mathematical_definition_reading, theater_ratio, 1975, 0.01).
narrative_ontology:measurement(chur_tr_t2000, church_turing_thesis__mathematical_definition_reading, theater_ratio, 2000, 0.01).
narrative_ontology:measurement(chur_tr_t2024, church_turing_thesis__mathematical_definition_reading, theater_ratio, 2024, 0.01).

% Extraction over time
narrative_ontology:measurement(chur_be_t1936, church_turing_thesis__mathematical_definition_reading, base_extractiveness, 1936, 0.02).
narrative_ontology:measurement(chur_be_t1950, church_turing_thesis__mathematical_definition_reading, base_extractiveness, 1950, 0.02).
narrative_ontology:measurement(chur_be_t1975, church_turing_thesis__mathematical_definition_reading, base_extractiveness, 1975, 0.02).
narrative_ontology:measurement(chur_be_t2000, church_turing_thesis__mathematical_definition_reading, base_extractiveness, 2000, 0.02).
narrative_ontology:measurement(chur_be_t2024, church_turing_thesis__mathematical_definition_reading, base_extractiveness, 2024, 0.02).

% Suppression requirement over time
narrative_ontology:measurement(chur_su_t1936, church_turing_thesis__mathematical_definition_reading, suppression_requirement, 1936, 0.05).
narrative_ontology:measurement(chur_su_t1950, church_turing_thesis__mathematical_definition_reading, suppression_requirement, 1950, 0.05).
narrative_ontology:measurement(chur_su_t1975, church_turing_thesis__mathematical_definition_reading, suppression_requirement, 1975, 0.05).
narrative_ontology:measurement(chur_su_t2000, church_turing_thesis__mathematical_definition_reading, suppression_requirement, 2000, 0.05).
narrative_ontology:measurement(chur_su_t2024, church_turing_thesis__mathematical_definition_reading, suppression_requirement, 2024, 0.05).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(church_turing_thesis__mathematical_definition_reading, information_standard).
narrative_ontology:affects_constraint(church_turing_thesis__mathematical_definition_reading, church_turing_thesis__physical_claim_reading).
narrative_ontology:affects_constraint(church_turing_thesis__mathematical_definition_reading, church_turing_thesis__epistemological_boundary_reading).

% DUAL FORMULATION NOTE:
% The Church-Turing Thesis is a kernel with multiple readings. This file models the 'mathematical definition' reading, which is distinct from the 'physical claim' and 'epistemological boundary' readings due to differing epsilon values and structural properties. All three are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
