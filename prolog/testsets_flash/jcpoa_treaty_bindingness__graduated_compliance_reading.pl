% ============================================================================
% CONSTRAINT STORY: jcpoa_treaty_bindingness__graduated_compliance_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jcpoa_treaty_bindingness__graduated_compliance_reading, []).

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
 *   constraint_id: jcpoa_treaty_bindingness__graduated_compliance_reading
 *   human_readable: JCPOA: Graduated Compliance Reading
 *   domain: international_law/nuclear_non_proliferation/treaty_compliance
 *
 * SUMMARY:
 *   This constraint represents the JCPOA as a scaled reciprocal commitment,
 *   where enforcement is graduated and tied to proportional compliance
 *   assessment. This reading emphasizes de-escalation and the flexibility to
 *   adjust sanctions relief in response to Iranian actions, rather than a
 *   rigid, all-or-nothing approach. It is one reading of the broader
 *   'jcpoa_treaty_bindingness' kernel, distinct from interpretations
 *   emphasizing strict multilateral bindingness or unilateral transactional
 *   provisionality.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jcpoa_treaty_bindingness__graduated_compliance_reading, 0.45).
domain_priors:suppression_score(jcpoa_treaty_bindingness__graduated_compliance_reading, 0.6).
domain_priors:theater_ratio(jcpoa_treaty_bindingness__graduated_compliance_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jcpoa_treaty_bindingness__graduated_compliance_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(jcpoa_treaty_bindingness__graduated_compliance_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(jcpoa_treaty_bindingness__graduated_compliance_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jcpoa_treaty_bindingness__graduated_compliance_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(jcpoa_treaty_bindingness__graduated_compliance_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jcpoa_treaty_bindingness__graduated_compliance_reading, tangled_rope).
narrative_ontology:human_readable(jcpoa_treaty_bindingness__graduated_compliance_reading, "JCPOA: Graduated Compliance Reading").
narrative_ontology:topic_domain(jcpoa_treaty_bindingness__graduated_compliance_reading, "international_law/nuclear_non_proliferation/treaty_compliance").

domain_priors:requires_active_enforcement(jcpoa_treaty_bindingness__graduated_compliance_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jcpoa_treaty_bindingness__graduated_compliance_reading, '51b3553c-ee08-4982-8ff6-be61e729d830').
narrative_ontology:cs_kernel_codification('51b3553c-ee08-4982-8ff6-be61e729d830', formalized).
narrative_ontology:cs_authority_grounding('51b3553c-ee08-4982-8ff6-be61e729d830', lineage).
narrative_ontology:cs_interpretation_layer_present('51b3553c-ee08-4982-8ff6-be61e729d830').
narrative_ontology:cs_reading_relation('51b3553c-ee08-4982-8ff6-be61e729d830', jcpoa_treaty_bindingness__binding_multilateral_reading, coexists_with).
narrative_ontology:cs_reading_relation('51b3553c-ee08-4982-8ff6-be61e729d830', jcpoa_treaty_bindingness__transactional_provisional_reading, coexists_with).
narrative_ontology:cs_axiom('51b3553c-ee08-4982-8ff6-be61e729d830', foundational, proportional_response_principle).
narrative_ontology:cs_axiom_status(proportional_response_principle, holdable).
narrative_ontology:cs_axiom_grounding('51b3553c-ee08-4982-8ff6-be61e729d830', proportional_response_principle, conventional).
narrative_ontology:cs_axiom('51b3553c-ee08-4982-8ff6-be61e729d830', foundational, de_escalation_priority).
narrative_ontology:cs_axiom_status(de_escalation_priority, holdable).
narrative_ontology:cs_axiom_grounding('51b3553c-ee08-4982-8ff6-be61e729d830', de_escalation_priority, instrumental).
narrative_ontology:cs_reference_frame('51b3553c-ee08-4982-8ff6-be61e729d830', adaptive_diplomatic_framework).
narrative_ontology:cs_drift_state('51b3553c-ee08-4982-8ff6-be61e729d830', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('51b3553c-ee08-4982-8ff6-be61e729d830', '').
narrative_ontology:cs_kernel_id(jcpoa_treaty_bindingness__graduated_compliance_reading, jcpoa_treaty_bindingness).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jcpoa_treaty_bindingness__graduated_compliance_reading, pragmatic_diplomacy_advocates).
narrative_ontology:constraint_beneficiary(jcpoa_treaty_bindingness__graduated_compliance_reading, economic_actors_seeking_partial_engagement).
narrative_ontology:constraint_victim(jcpoa_treaty_bindingness__graduated_compliance_reading, iranian_hardliners).
narrative_ontology:constraint_victim(jcpoa_treaty_bindingness__graduated_compliance_reading, us_sanctions_hawks).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jcpoa_treaty_bindingness__graduated_compliance_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(jcpoa_treaty_bindingness__graduated_compliance_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jcpoa_treaty_bindingness__graduated_compliance_reading_tests).
:- end_tests(jcpoa_treaty_bindingness__graduated_compliance_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.45) is moderate, reflecting the reciprocal nature of the agreement where both sides make concessions. Suppression (0.6) is present due to the threat of sanctions snapback, but it is graduated, allowing for partial non-compliance without immediate collapse. The theater ratio (0.2) is low, as the compliance mechanisms are largely functional, though political posturing can introduce some performativity. The measurements reflect periods of increased tension (e.g., 2019 after US withdrawal) and subsequent de-escalation efforts.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of pragmatic diplomacy advocates, this reading of the JCPOA is a successful, flexible tool for managing proliferation. From the perspective of Iranian hardliners or US sanctions hawks, it is either an unacceptable compromise or an insufficient deterrent, respectively. The engine will compute these divergent classifications based on their declared structural positions and exit options.
 *
 * DIRECTIONALITY LOGIC:
 *   Iran is a primary target (payer) due to the limitations on its nuclear program. The P5+1 states are agenda-setters, collectively enforcing the constraint. Pragmatic diplomacy advocates and economic actors are beneficiaries, gaining from de-escalation and partial market access. Hardliners and hawks on both sides are victims, as the agreement constrains their preferred maximalist policies.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading prevents mislabeling the JCPOA as a pure snare by emphasizing its genuine coordination function (non-proliferation) and the reciprocal nature of commitments. It avoids treating the agreement as a mere 'piton' by acknowledging the active, albeit graduated, enforcement and the live founding problem of nuclear proliferation. The graduated enforcement mechanism is designed to prevent mandatrophy by allowing for adaptive responses rather than rigid adherence to an outdated mandate.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    graduated_enforcement_effectiveness,
    'Is the graduated enforcement mechanism sufficiently robust to deter significant non-compliance, or does it invite incremental violations?',
    'Empirical analysis of Iran''s compliance trajectory under graduated sanctions, compared to counterfactuals of ''all-or-nothing'' enforcement.',
    'If graduated enforcement proves ineffective, the constraint''s effective extractiveness for Iran might be lower than intended, and its coordination function weakened, potentially shifting its classification towards a more performative or even failed type from the perspective of non-proliferation advocates.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(graduated_enforcement_effectiveness, empirical, 'Effectiveness of proportional compliance assessment in deterring violations.').

omega_variable(
    kernel_reading_distinction,
    'Is this ''graduated compliance'' reading of the JCPOA sufficiently distinct from the ''binding multilateral'' or ''transactional provisional'' readings, or do they represent overlapping policy preferences rather than structurally different constraints?',
    'Analysis of policy outcomes and diplomatic statements: if different readings consistently lead to distinct enforcement actions, dispute resolution approaches, and economic engagement patterns, the structural distinction is confirmed.',
    'If the readings are not structurally distinct, the framework might be over-decomposing the kernel, suggesting a single, more complex constraint with internal tensions rather than multiple, separate ones. This would require re-evaluating the ε-invariance principle for this specific kernel.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_distinction, conceptual, 'Distinction between different interpretations of JCPOA bindingness.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jcpoa_treaty_bindingness__graduated_compliance_reading, 2015, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jcpo_tr_t2015, jcpoa_treaty_bindingness__graduated_compliance_reading, theater_ratio, 2015, 0.1).
narrative_ontology:measurement(jcpo_tr_t2017, jcpoa_treaty_bindingness__graduated_compliance_reading, theater_ratio, 2017, 0.15).
narrative_ontology:measurement(jcpo_tr_t2019, jcpoa_treaty_bindingness__graduated_compliance_reading, theater_ratio, 2019, 0.3).
narrative_ontology:measurement(jcpo_tr_t2021, jcpoa_treaty_bindingness__graduated_compliance_reading, theater_ratio, 2021, 0.25).
narrative_ontology:measurement(jcpo_tr_t2023, jcpoa_treaty_bindingness__graduated_compliance_reading, theater_ratio, 2023, 0.2).
narrative_ontology:measurement(jcpo_tr_t2024, jcpoa_treaty_bindingness__graduated_compliance_reading, theater_ratio, 2024, 0.2).

% Extraction over time
narrative_ontology:measurement(jcpo_be_t2015, jcpoa_treaty_bindingness__graduated_compliance_reading, base_extractiveness, 2015, 0.5).
narrative_ontology:measurement(jcpo_be_t2017, jcpoa_treaty_bindingness__graduated_compliance_reading, base_extractiveness, 2017, 0.45).
narrative_ontology:measurement(jcpo_be_t2019, jcpoa_treaty_bindingness__graduated_compliance_reading, base_extractiveness, 2019, 0.6).
narrative_ontology:measurement(jcpo_be_t2021, jcpoa_treaty_bindingness__graduated_compliance_reading, base_extractiveness, 2021, 0.55).
narrative_ontology:measurement(jcpo_be_t2023, jcpoa_treaty_bindingness__graduated_compliance_reading, base_extractiveness, 2023, 0.45).
narrative_ontology:measurement(jcpo_be_t2024, jcpoa_treaty_bindingness__graduated_compliance_reading, base_extractiveness, 2024, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(jcpo_su_t2015, jcpoa_treaty_bindingness__graduated_compliance_reading, suppression_requirement, 2015, 0.7).
narrative_ontology:measurement(jcpo_su_t2017, jcpoa_treaty_bindingness__graduated_compliance_reading, suppression_requirement, 2017, 0.65).
narrative_ontology:measurement(jcpo_su_t2019, jcpoa_treaty_bindingness__graduated_compliance_reading, suppression_requirement, 2019, 0.8).
narrative_ontology:measurement(jcpo_su_t2021, jcpoa_treaty_bindingness__graduated_compliance_reading, suppression_requirement, 2021, 0.7).
narrative_ontology:measurement(jcpo_su_t2023, jcpoa_treaty_bindingness__graduated_compliance_reading, suppression_requirement, 2023, 0.6).
narrative_ontology:measurement(jcpo_su_t2024, jcpoa_treaty_bindingness__graduated_compliance_reading, suppression_requirement, 2024, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jcpoa_treaty_bindingness__graduated_compliance_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(jcpoa_treaty_bindingness__graduated_compliance_reading, iran_nuclear_program_limitations).
narrative_ontology:affects_constraint(jcpoa_treaty_bindingness__graduated_compliance_reading, international_sanctions_regime).

% DUAL FORMULATION NOTE:
% This constraint is one of three distinct readings of the 'jcpoa_treaty_bindingness' kernel. The 'binding_multilateral_reading' emphasizes consensus-based modification, while the 'transactional_provisional_reading' allows for unilateral voiding. Each reading defines a structurally different constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
