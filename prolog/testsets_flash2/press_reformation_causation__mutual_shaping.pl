% ============================================================================
% CONSTRAINT STORY: press_reformation_causation__mutual_shaping
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_press_reformation_causation__mutual_shaping, []).

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
 *   constraint_id: press_reformation_causation__mutual_shaping
 *   human_readable: Printing Press and Reformation: Mutual Shaping
 *   domain: history_of_technology/religious_history/media_studies
 *
 * SUMMARY:
 *   This constraint story instantiates the 'mutual shaping' reading of the
 *   relationship between the printing press and the Reformation. It posits a
 *   bidirectional causal relationship where the printing press created new
 *   affordances that reformers exploited, and in turn, the reformers' use
 *   cases and demands shaped the technological development and diffusion of
 *   printing. This reading emphasizes co-evolution and rejects both pure
 *   technological determinism and pure strategic deployment. The constraint
 *   is classified as a Scaffold because the press provided a temporary,
 *   enabling structure that facilitated a historical transition, rather than
 *   a fixed, extractive, or purely coordinative arrangement.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(press_reformation_causation__mutual_shaping, 0.15).
domain_priors:suppression_score(press_reformation_causation__mutual_shaping, 0.05).
domain_priors:theater_ratio(press_reformation_causation__mutual_shaping, 0.0).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(press_reformation_causation__mutual_shaping, extractiveness, 0.15).
narrative_ontology:constraint_metric(press_reformation_causation__mutual_shaping, suppression_requirement, 0.05).
narrative_ontology:constraint_metric(press_reformation_causation__mutual_shaping, theater_ratio, 0.0).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(press_reformation_causation__mutual_shaping, accessibility_collapse, 0.1).
narrative_ontology:constraint_metric(press_reformation_causation__mutual_shaping, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(press_reformation_causation__mutual_shaping, scaffold).
narrative_ontology:human_readable(press_reformation_causation__mutual_shaping, "Printing Press and Reformation: Mutual Shaping").
narrative_ontology:topic_domain(press_reformation_causation__mutual_shaping, "history_of_technology/religious_history/media_studies").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(press_reformation_causation__mutual_shaping, 'e33b7a2d-4076-4615-9bb0-9b1b7c8e923a').
narrative_ontology:cs_kernel_codification('e33b7a2d-4076-4615-9bb0-9b1b7c8e923a', distributed).
narrative_ontology:cs_authority_grounding('e33b7a2d-4076-4615-9bb0-9b1b7c8e923a', expertise).
narrative_ontology:cs_interpretation_layer_present('e33b7a2d-4076-4615-9bb0-9b1b7c8e923a').
narrative_ontology:cs_reading_relation('e33b7a2d-4076-4615-9bb0-9b1b7c8e923a', press_reformation_causation__technological_determinism, forecloses).
narrative_ontology:cs_reading_relation('e33b7a2d-4076-4615-9bb0-9b1b7c8e923a', press_reformation_causation__strategic_deployment, coexists_with).
narrative_ontology:cs_axiom('e33b7a2d-4076-4615-9bb0-9b1b7c8e923a', foundational, technology_and_agency_co_evolve).
narrative_ontology:cs_axiom_status(technology_and_agency_co_evolve, holdable).
narrative_ontology:cs_axiom_grounding('e33b7a2d-4076-4615-9bb0-9b1b7c8e923a', technology_and_agency_co_evolve, empirically_contingent).
narrative_ontology:cs_axiom('e33b7a2d-4076-4615-9bb0-9b1b7c8e923a', foundational, affordances_shape_use_use_shapes_technology).
narrative_ontology:cs_axiom_status(affordances_shape_use_use_shapes_technology, holdable).
narrative_ontology:cs_axiom_grounding('e33b7a2d-4076-4615-9bb0-9b1b7c8e923a', affordances_shape_use_use_shapes_technology, empirically_contingent).
narrative_ontology:cs_reference_frame('e33b7a2d-4076-4615-9bb0-9b1b7c8e923a', co_evolutionary_historical_analysis).
narrative_ontology:cs_drift_state('e33b7a2d-4076-4615-9bb0-9b1b7c8e923a', contemporary_historical_scholarship, gap(stable, minor, true)).
narrative_ontology:cs_created_at('e33b7a2d-4076-4615-9bb0-9b1b7c8e923a', '').
narrative_ontology:cs_kernel_id(press_reformation_causation__mutual_shaping, press_reformation_causation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(press_reformation_causation__mutual_shaping, reformation_reformers).
narrative_ontology:constraint_beneficiary(press_reformation_causation__mutual_shaping, printing_press_industry).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(press_reformation_causation__mutual_shaping, catholic_church).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Exploited the printing press to disseminate their ideas, translating texts into vernacular languages and reaching wider audiences. Their actions, in turn, created demand for printed materials and influenced the development of printing technology.
narrative_ontology:constraint_stakeholder(press_reformation_causation__mutual_shaping, reformation_reformers, beneficiary,
    organized, generational, mobile, continental).

% Benefited from the increased demand for printed materials generated by the Reformation. Printers adapted their technologies and business models to meet the needs of reformers, leading to innovations in production and distribution.
narrative_ontology:constraint_stakeholder(press_reformation_causation__mutual_shaping, printing_press_industry, beneficiary,
    organized, biographical, mobile, regional).

% Initially struggled to adapt to the rapid dissemination of dissenting ideas facilitated by the press. While eventually adopting printing for its own purposes, it faced a significant challenge to its authority and control over information.
narrative_ontology:constraint_stakeholder(press_reformation_causation__mutual_shaping, catholic_church, payer,
    institutional, civilizational, constrained, global).

% Remained largely unaffected by the direct consumption of printed materials, though they were influenced by the broader cultural and religious shifts driven by the Reformation and the press. Their access to information was mediated by literate individuals.
narrative_ontology:constraint_stakeholder(press_reformation_causation__mutual_shaping, illiterate_population, excluded,
    powerless, biographical, trapped, local).

% Analyze the complex interplay between technological development and social change, seeking to understand the reciprocal causal links between the printing press and the Reformation.
narrative_ontology:constraint_stakeholder(press_reformation_causation__mutual_shaping, historians_of_technology, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Facilitated the co-evolution of a new communication technology and a major religious movement, enabling both to adapt and reinforce each other's development.
% TRANSFER_FUNCTION: Transferred new possibilities for communication and organization from the printing press to reformers, and transferred demand and developmental direction from reformers back to the printing industry.
% ABSENT_VOICES: The voices of those who saw technology as a neutral tool or as a sole determinant of historical change are absent from this 'mutual shaping' reading, as it emphasizes interaction and co-creation.
% DISAPPEARANCE_RATIONALE: If the mutual shaping dynamic had not occurred, the Reformation's spread would have been fundamentally different, and the development trajectory of printing technology would have been altered, leading to a vastly different historical outcome.
% FOUNDING_PROBLEM: The problem of understanding complex historical causality, specifically the relationship between technological innovation and social/religious movements.
% FOUNDING_PROBLEM_CORROBORATION: Historians of technology and media studies scholars corroborate the complexity of this causal relationship, citing extensive archival research and theoretical frameworks that move beyond simplistic deterministic models. This corroboration comes from outside the immediate beneficiaries of the historical process itself.
narrative_ontology:disappearance_verdict(press_reformation_causation__mutual_shaping, world_rearranges).
narrative_ontology:founding_problem_status(press_reformation_causation__mutual_shaping, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(press_reformation_causation__mutual_shaping, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(press_reformation_causation__mutual_shaping, 'none', 1).
narrative_ontology:epsilon_provenance(press_reformation_causation__mutual_shaping, 0.15, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(press_reformation_causation__mutual_shaping_tests).
:- end_tests(press_reformation_causation__mutual_shaping_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.15) because the relationship was primarily one of mutual enablement and co-creation, not one party extracting from another through the constraint itself. Suppression is very low (0.05) as the constraint describes an emergent historical dynamic, not an actively enforced rule. Theater ratio is zero as there's no performative maintenance. Accessibility collapse is low (0.1) because the 'constraint' here is a historical dynamic, not a barrier to alternatives. Resistance is low (0.05) as it describes a co-evolutionary process, not a contested structure.
 *
 * PERSPECTIVAL GAP:
 *   This reading highlights the dynamic interplay, contrasting with views that might see the press as a neutral tool (strategic deployment) or an unstoppable force (technological determinism). The 'mutual shaping' perspective emphasizes the agency of both technology and human actors in a co-evolutionary loop.
 *
 * DIRECTIONALITY LOGIC:
 *   Both Reformation reformers and the printing press industry are beneficiaries, as they mutually reinforced each other's growth and development. The Catholic Church, while eventually adapting, initially bore costs in terms of challenged authority and control, making it a payer. The illiterate population was largely outside the direct causal loop of this specific mutual shaping, hence 'excluded'. Historians are observers.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is not subject to mandatrophy in the typical sense, as it describes a historical causal relationship rather than an ongoing institutional arrangement. Its 'function' is to accurately model a complex historical process. The scaffold classification reflects its role as a transitional, enabling structure in a specific historical period, not a structure with a mandate that could atrophy.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    causal_directionality_ambiguity,
    'What was the precise weighting and timing of causal influence between the printing press and the Reformation?',
    'Further detailed historical and quantitative analysis, potentially using counterfactual modeling or event-history analysis, to disentangle specific causal pathways and their relative strengths.',
    'A stronger weighting towards one side (e.g., the press''s inherent properties) might push the classification closer to a ''mountain'' (technological determinism), while a stronger weighting towards reformers'' agency might lean towards ''rope'' (strategic deployment). This reading asserts a balanced, mutual influence.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(causal_directionality_ambiguity, empirical, 'Ambiguity in the precise balance of bidirectional causality.').

omega_variable(
    kernel_reading_distinction,
    'Is this ''mutual shaping'' reading sufficiently distinct from ''strategic deployment'' or ''technological determinism'' to warrant a separate constraint, or are the differences merely matters of emphasis?',
    'Conceptual analysis of the core axioms and their implications for historical methodology. If the axioms lead to fundamentally different explanatory frameworks, separate constraints are warranted.',
    'If the distinctions are deemed superficial, this constraint might be merged with a sibling reading, losing the nuance of co-evolutionary dynamics. If distinct, it reinforces the value of multi-perspectival historical analysis.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_distinction, conceptual, 'Distinction between kernel readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(press_reformation_causation__mutual_shaping, 1450, 1650).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pres_tr_t1450, press_reformation_causation__mutual_shaping, theater_ratio, 1450, 0.0).
narrative_ontology:measurement(pres_tr_t1500, press_reformation_causation__mutual_shaping, theater_ratio, 1500, 0.0).
narrative_ontology:measurement(pres_tr_t1550, press_reformation_causation__mutual_shaping, theater_ratio, 1550, 0.0).
narrative_ontology:measurement(pres_tr_t1600, press_reformation_causation__mutual_shaping, theater_ratio, 1600, 0.0).
narrative_ontology:measurement(pres_tr_t1650, press_reformation_causation__mutual_shaping, theater_ratio, 1650, 0.0).

% Extraction over time
narrative_ontology:measurement(pres_be_t1450, press_reformation_causation__mutual_shaping, base_extractiveness, 1450, 0.05).
narrative_ontology:measurement(pres_be_t1500, press_reformation_causation__mutual_shaping, base_extractiveness, 1500, 0.1).
narrative_ontology:measurement(pres_be_t1550, press_reformation_causation__mutual_shaping, base_extractiveness, 1550, 0.15).
narrative_ontology:measurement(pres_be_t1600, press_reformation_causation__mutual_shaping, base_extractiveness, 1600, 0.15).
narrative_ontology:measurement(pres_be_t1650, press_reformation_causation__mutual_shaping, base_extractiveness, 1650, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(pres_su_t1450, press_reformation_causation__mutual_shaping, suppression_requirement, 1450, 0.05).
narrative_ontology:measurement(pres_su_t1500, press_reformation_causation__mutual_shaping, suppression_requirement, 1500, 0.05).
narrative_ontology:measurement(pres_su_t1550, press_reformation_causation__mutual_shaping, suppression_requirement, 1550, 0.05).
narrative_ontology:measurement(pres_su_t1600, press_reformation_causation__mutual_shaping, suppression_requirement, 1600, 0.05).
narrative_ontology:measurement(pres_su_t1650, press_reformation_causation__mutual_shaping, suppression_requirement, 1650, 0.05).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(press_reformation_causation__mutual_shaping, information_standard).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings (mutual_shaping, strategic_deployment, technological_determinism) of the 'press_reformation_causation' kernel. Each reading offers a distinct causal explanation for the historical relationship.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
