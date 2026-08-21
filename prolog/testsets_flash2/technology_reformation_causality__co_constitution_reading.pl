% ============================================================================
% CONSTRAINT STORY: technology_reformation_causality__co_constitution_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_technology_reformation_causality__co_constitution_reading, []).

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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
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
 *   constraint_id: technology_reformation_causality__co_constitution_reading
 *   human_readable: Co-Constitutive Causality of Printing Press and Reformation
 *   domain: history_of_technology/religious_history/media_studies
 *
 * SUMMARY:
 *   This constraint models the co-constitutive relationship between the
 *   printing press and the Protestant Reformation. It argues that technology
 *   (the press) enabled, but did not solely determine, the Reformation's
 *   trajectory. Instead, social actors (reformers) actively shaped what the
 *   press produced, leading to a bidirectional causality. This reading
 *   challenges both purely deterministic and purely voluntarist accounts of
 *   historical change. The constraint is classified as a Rope because the
 *   interaction itself created a coordination mechanism for social and
 *   technological forces, with relatively low extraction inherent to the
 *   co-evolutionary process.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(technology_reformation_causality__co_constitution_reading, 0.25).
domain_priors:suppression_score(technology_reformation_causality__co_constitution_reading, 0.15).
domain_priors:theater_ratio(technology_reformation_causality__co_constitution_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(technology_reformation_causality__co_constitution_reading, extractiveness, 0.25).
narrative_ontology:constraint_metric(technology_reformation_causality__co_constitution_reading, suppression_requirement, 0.15).
narrative_ontology:constraint_metric(technology_reformation_causality__co_constitution_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(technology_reformation_causality__co_constitution_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(technology_reformation_causality__co_constitution_reading, resistance, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(technology_reformation_causality__co_constitution_reading, rope).
narrative_ontology:human_readable(technology_reformation_causality__co_constitution_reading, "Co-Constitutive Causality of Printing Press and Reformation").
narrative_ontology:topic_domain(technology_reformation_causality__co_constitution_reading, "history_of_technology/religious_history/media_studies").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(technology_reformation_causality__co_constitution_reading, 'ab02e095-70c3-4d69-b6f3-8ee9dd91f213').
narrative_ontology:cs_kernel_codification('ab02e095-70c3-4d69-b6f3-8ee9dd91f213', distributed).
narrative_ontology:cs_authority_grounding('ab02e095-70c3-4d69-b6f3-8ee9dd91f213', diffuse_epistemic).
narrative_ontology:cs_reading_relation('ab02e095-70c3-4d69-b6f3-8ee9dd91f213', technology_reformation_causality__technological_determinism_reading, coexists_with).
narrative_ontology:cs_reading_relation('ab02e095-70c3-4d69-b6f3-8ee9dd91f213', technology_reformation_causality__beneficiary_agency_reading, coexists_with).
narrative_ontology:cs_axiom('ab02e095-70c3-4d69-b6f3-8ee9dd91f213', foundational, mutual_shaping_of_technology_and_society).
narrative_ontology:cs_axiom_status(mutual_shaping_of_technology_and_society, holdable).
narrative_ontology:cs_axiom_grounding('ab02e095-70c3-4d69-b6f3-8ee9dd91f213', mutual_shaping_of_technology_and_society, empirically_contingent).
narrative_ontology:cs_axiom('ab02e095-70c3-4d69-b6f3-8ee9dd91f213', foundational, technology_as_enabling_not_determining).
narrative_ontology:cs_axiom_status(technology_as_enabling_not_determining, holdable).
narrative_ontology:cs_axiom_grounding('ab02e095-70c3-4d69-b6f3-8ee9dd91f213', technology_as_enabling_not_determining, empirically_contingent).
narrative_ontology:cs_reference_frame('ab02e095-70c3-4d69-b6f3-8ee9dd91f213', complex_adaptive_systems_view).
narrative_ontology:cs_drift_state('ab02e095-70c3-4d69-b6f3-8ee9dd91f213', contemporary_historical_scholarship, gap(stable, minor, true)).
narrative_ontology:cs_created_at('ab02e095-70c3-4d69-b6f3-8ee9dd91f213', '2024-07-30T12:00:00Z').
narrative_ontology:cs_kernel_id(technology_reformation_causality__co_constitution_reading, technology_reformation_causality).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(technology_reformation_causality__co_constitution_reading, protestant_reformers).
narrative_ontology:constraint_beneficiary(technology_reformation_causality__co_constitution_reading, printing_press_operators).
narrative_ontology:constraint_beneficiary(technology_reformation_causality__co_constitution_reading, vernacular_readers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(technology_reformation_causality__co_constitution_reading, catholic_church_hierarchy).
narrative_ontology:constraint_vindicates(technology_reformation_causality__co_constitution_reading, social_construction_of_technology).
narrative_ontology:constraint_vindicates(technology_reformation_causality__co_constitution_reading, actor_network_theory).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Utilized the printing press to disseminate theological arguments and vernacular Bibles, shaping its content and distribution networks. Benefited from the press's capacity to amplify their message, but also invested in its development and use.
narrative_ontology:constraint_stakeholder(technology_reformation_causality__co_constitution_reading, protestant_reformers, beneficiary,
    organized, generational, mobile, continental).

% Provided the technical means for mass production of texts. Benefited economically from the demand generated by reformers, but also influenced the types of texts produced and their reach through their technical capabilities and business decisions.
narrative_ontology:constraint_stakeholder(technology_reformation_causality__co_constitution_reading, printing_press_operators, beneficiary,
    moderate, biographical, constrained, regional).

% Gained unprecedented access to religious texts in their own languages, fostering new forms of literacy and religious engagement. Their demand for such texts, in turn, fueled the printing industry and the Reformation's spread.
narrative_ontology:constraint_stakeholder(technology_reformation_causality__co_constitution_reading, vernacular_readers, beneficiary,
    powerless, biographical, constrained, local).

% Experienced a loss of control over religious discourse and a challenge to its interpretive authority due to the widespread dissemination of texts it could not fully censor. Its attempts to suppress the press were largely ineffective against the co-evolving system.
narrative_ontology:constraint_stakeholder(technology_reformation_causality__co_constitution_reading, catholic_church_hierarchy, payer,
    institutional, civilizational, constrained, global).

% Analyze the historical interaction, often overemphasizing the press's autonomous causal power. This reading challenges their unidirectional view by highlighting the mutual shaping between technology and society.
narrative_ontology:constraint_stakeholder(technology_reformation_causality__co_constitution_reading, technological_determinists, observer,
    analytical, generational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Enabled the co-evolution of a new media ecosystem where technological capabilities (printing) and social demands (religious reform) mutually reinforced each other, coordinating the production and consumption of mass-produced texts.
% TRANSFER_FUNCTION: Facilitated the transfer of religious authority and interpretive power from the Catholic Church hierarchy to individual readers and Protestant reformers, mediated by the printed word.
% ABSENT_VOICES: Those who held a purely deterministic view of technology's impact, or a purely voluntarist view of human agency, would find their perspectives challenged by the nuanced, bidirectional causality described here.
% DISAPPEARANCE_RATIONALE: If the co-constitutive dynamic vanished, the historical narrative of the Reformation would fundamentally change, requiring a re-evaluation of how major social transformations occur in relation to technological change. The idea of technology as a neutral tool or an autonomous force would become dominant, obscuring the complex interplay.
% FOUNDING_PROBLEM: The problem of understanding how major historical transformations, like the Reformation, arise from the complex interplay between technological innovation and social agency, avoiding reductionist explanations.
% FOUNDING_PROBLEM_CORROBORATION: Historians of technology, media theorists, and social scientists outside of purely deterministic or voluntarist camps corroborate the ongoing challenge of accurately modeling co-constitutive causality in historical analysis.
narrative_ontology:disappearance_verdict(technology_reformation_causality__co_constitution_reading, world_rearranges).
narrative_ontology:founding_problem_status(technology_reformation_causality__co_constitution_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(technology_reformation_causality__co_constitution_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(technology_reformation_causality__co_constitution_reading, 'none', 1).
narrative_ontology:epsilon_provenance(technology_reformation_causality__co_constitution_reading, 0.25, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(technology_reformation_causality__co_constitution_reading_tests).
:- end_tests(technology_reformation_causality__co_constitution_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.25) because the constraint describes a mutually beneficial co-evolutionary process rather than a zero-sum extraction. The 'extraction' here is primarily the cost of navigating and adapting to a rapidly changing media landscape, borne by the Catholic Church hierarchy which struggled to maintain its monopoly on information. Suppression is also low (0.15) as the system's persistence relied on the emergent properties of the interaction, not active coercion. Theater ratio is negligible (0.05) as the process was genuinely functional. Accessibility collapse is moderate (0.7) because while the press opened new avenues, it also created new barriers for those unable to adapt. Resistance is low (0.1) because the co-evolution was a powerful, self-reinforcing dynamic.
 *
 * PERSPECTIVAL GAP:
 *   This reading emphasizes the interaction term, where neither technology nor society is fully determinant. This contrasts sharply with deterministic views (where technology drives change) and purely agency-focused views (where technology is a neutral tool). The engine's classification of this as a Rope reflects the coordination inherent in the co-evolution, which would be missed by more reductionist perspectives.
 *
 * DIRECTIONALITY LOGIC:
 *   Protestant reformers, printing press operators, and vernacular readers are beneficiaries, as they actively shaped and benefited from the emergent media ecosystem. The Catholic Church hierarchy is a payer, bearing the costs of losing its information monopoly and struggling to adapt. Technological determinists are observers, whose analytical framework is challenged by this co-constitutive view.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    causal_primacy_ambiguity,
    'What was the precise weighting of technological enablement versus social shaping in specific historical instances?',
    'Detailed micro-historical case studies that trace specific innovations and their social uptake, quantifying the relative influence of each factor.',
    'A stronger weighting towards technology would shift the constraint closer to a ''snare'' for those unable to adapt, while a stronger weighting towards social agency might reduce its perceived ''naturalness'' as a Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(causal_primacy_ambiguity, empirical, 'Ambiguity in the precise balance of technological vs. social causality.').

omega_variable(
    framing_of_causality,
    'Is the concept of ''co-constitution'' itself a sufficiently robust causal framework, or does it obscure underlying deterministic or agentic forces?',
    'Philosophical and theoretical work on causality in complex systems, assessing the explanatory power and predictive utility of co-constitutive models compared to alternative frameworks.',
    'If co-constitution is deemed too vague, the constraint might decompose into separate, more reductionist constraints (e.g., a ''technological enablement'' Rope and a ''reformer agency'' Rope), each with different classifications.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(framing_of_causality, conceptual, 'Conceptual debate over the robustness of co-constitutive causality.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(technology_reformation_causality__co_constitution_reading, 1450, 1650).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Extraction over time
narrative_ontology:measurement(tech_be_t1450, technology_reformation_causality__co_constitution_reading, base_extractiveness, 1450, 0.1).
narrative_ontology:measurement(tech_be_t1500, technology_reformation_causality__co_constitution_reading, base_extractiveness, 1500, 0.15).
narrative_ontology:measurement(tech_be_t1550, technology_reformation_causality__co_constitution_reading, base_extractiveness, 1550, 0.2).
narrative_ontology:measurement(tech_be_t1600, technology_reformation_causality__co_constitution_reading, base_extractiveness, 1600, 0.23).
narrative_ontology:measurement(tech_be_t1650, technology_reformation_causality__co_constitution_reading, base_extractiveness, 1650, 0.25).

% Suppression requirement over time
narrative_ontology:measurement(tech_su_t1450, technology_reformation_causality__co_constitution_reading, suppression_requirement, 1450, 0.05).
narrative_ontology:measurement(tech_su_t1500, technology_reformation_causality__co_constitution_reading, suppression_requirement, 1500, 0.1).
narrative_ontology:measurement(tech_su_t1550, technology_reformation_causality__co_constitution_reading, suppression_requirement, 1550, 0.13).
narrative_ontology:measurement(tech_su_t1600, technology_reformation_causality__co_constitution_reading, suppression_requirement, 1600, 0.14).
narrative_ontology:measurement(tech_su_t1650, technology_reformation_causality__co_constitution_reading, suppression_requirement, 1650, 0.15).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(technology_reformation_causality__co_constitution_reading, information_standard).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'technology_reformation_causality' kernel, focusing on bidirectional causality. It is linked to 'technological_determinism_reading' and 'beneficiary_agency_reading' as sibling interpretations.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
