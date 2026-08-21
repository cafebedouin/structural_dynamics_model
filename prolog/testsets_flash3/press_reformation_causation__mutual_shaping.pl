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
 *   This constraint models the 'mutual shaping' reading of the relationship
 *   between the printing press and the Reformation. It posits a bidirectional
 *   causal link: the printing press provided new affordances that reformers
 *   actively exploited, and in turn, the reformers' specific uses and demands
 *   shaped the technological and industrial development of printing. This
 *   reading positions the press as a 'scaffold' – an enabling structure that
 *   facilitated a historical transition, rather than a deterministic force or
 *   a neutral tool. The constraint's low extractiveness and suppression
 *   reflect its nature as an analytical framework for understanding
 *   historical processes, not an active extractive mechanism.
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
narrative_ontology:constraint_metric(press_reformation_causation__mutual_shaping, resistance, 0.0).

% --- Constraint claim ---
narrative_ontology:constraint_claim(press_reformation_causation__mutual_shaping, scaffold).
narrative_ontology:human_readable(press_reformation_causation__mutual_shaping, "Printing Press and Reformation: Mutual Shaping").
narrative_ontology:topic_domain(press_reformation_causation__mutual_shaping, "history_of_technology/religious_history/media_studies").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(press_reformation_causation__mutual_shaping, 'a508d277-4b2d-4a91-a504-8d7a05cf077b').
narrative_ontology:cs_kernel_codification('a508d277-4b2d-4a91-a504-8d7a05cf077b', distributed).
narrative_ontology:cs_authority_grounding('a508d277-4b2d-4a91-a504-8d7a05cf077b', expertise).
narrative_ontology:cs_interpretation_layer_present('a508d277-4b2d-4a91-a504-8d7a05cf077b').
narrative_ontology:cs_reading_relation('a508d277-4b2d-4a91-a504-8d7a05cf077b', press_reformation_causation__technological_determinism, coexists_with).
narrative_ontology:cs_reading_relation('a508d277-4b2d-4a91-a504-8d7a05cf077b', press_reformation_causation__strategic_deployment, coexists_with).
narrative_ontology:cs_axiom('a508d277-4b2d-4a91-a504-8d7a05cf077b', foundational, technology_and_agency_co_evolve).
narrative_ontology:cs_axiom_status(technology_and_agency_co_evolve, holdable).
narrative_ontology:cs_axiom_grounding('a508d277-4b2d-4a91-a504-8d7a05cf077b', technology_and_agency_co_evolve, empirically_contingent).
narrative_ontology:cs_axiom('a508d277-4b2d-4a91-a504-8d7a05cf077b', secondary, affordances_are_exploited_and_reinforced).
narrative_ontology:cs_axiom_status(affordances_are_exploited_and_reinforced, holdable).
narrative_ontology:cs_axiom_grounding('a508d277-4b2d-4a91-a504-8d7a05cf077b', affordances_are_exploited_and_reinforced, empirically_contingent).
narrative_ontology:cs_reference_frame('a508d277-4b2d-4a91-a504-8d7a05cf077b', complex_adaptive_systems_view).
narrative_ontology:cs_drift_state('a508d277-4b2d-4a91-a504-8d7a05cf077b', contemporary_historical_scholarship, gap(stable, minor, true)).
narrative_ontology:cs_created_at('a508d277-4b2d-4a91-a504-8d7a05cf077b', '').
narrative_ontology:cs_kernel_id(press_reformation_causation__mutual_shaping, press_reformation_causation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(press_reformation_causation__mutual_shaping, reformation_reformers).
narrative_ontology:constraint_beneficiary(press_reformation_causation__mutual_shaping, printing_press_industry).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(press_reformation_causation__mutual_shaping, literate_public).
narrative_ontology:constraint_victim(press_reformation_causation__mutual_shaping, catholic_church).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Exploited the printing press to disseminate their ideas, gaining unprecedented reach and speed. Their agency in using the press for specific purposes (e.g., vernacular Bibles, pamphlets) directly influenced the press's development and market.
narrative_ontology:constraint_stakeholder(press_reformation_causation__mutual_shaping, reformation_reformers, beneficiary,
    organized, generational, mobile, continental).

% Benefited from the demand created by reformers for printed materials, leading to innovation in printing techniques, distribution networks, and business models. The industry's development was shaped by the content and volume demanded by religious movements.
narrative_ontology:constraint_stakeholder(press_reformation_causation__mutual_shaping, printing_press_industry, beneficiary,
    organized, biographical, mobile, regional).

% Initially struggled to adapt to the rapid dissemination of dissenting ideas, facing challenges to its authority and control over information. Its attempts at censorship and counter-propaganda were reactive and often less effective than the reformers' proactive use of the press.
narrative_ontology:constraint_stakeholder(press_reformation_causation__mutual_shaping, catholic_church, payer,
    institutional, civilizational, constrained, global).

% Gained unprecedented access to diverse texts, including religious tracts, vernacular Bibles, and political pamphlets. This access fostered literacy and critical thinking, contributing to broader social and intellectual changes.
narrative_ontology:constraint_stakeholder(press_reformation_causation__mutual_shaping, literate_public, beneficiary,
    moderate, biographical, mobile, local).

% Analyze the complex interplay between technological development and social change, seeking to understand the bidirectional causal links between the printing press and the Reformation without resorting to deterministic or purely instrumental explanations.
narrative_ontology:constraint_stakeholder(press_reformation_causation__mutual_shaping, historians_of_technology, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provided a scalable, relatively rapid means for the dissemination of complex ideas and texts across geographical and social boundaries, enabling a shared intellectual and religious discourse that was previously impossible.
% TRANSFER_FUNCTION: Facilitated the transfer of information, religious doctrine, and political ideas from authors and publishers to a mass audience, and simultaneously transferred demand and innovation feedback from users (reformers) back to the printing industry.
% ABSENT_VOICES: Those who held a purely deterministic view of technology (e.g., that the press *alone* caused the Reformation) or a purely instrumental view (e.g., that the press was a neutral tool with no inherent shaping power) are absent from this 'mutual shaping' framing, as their perspectives simplify the complex causality.
% DISAPPEARANCE_RATIONALE: If the mutual shaping dynamic vanished, the historical understanding of both the printing press's development and the Reformation's trajectory would fundamentally change. The co-evolutionary narrative would be replaced by simpler, less accurate, unidirectional causal models.
% FOUNDING_PROBLEM: The problem of understanding complex historical causality, specifically the relationship between technological innovation and major social/religious movements, avoiding reductionist explanations.
% FOUNDING_PROBLEM_CORROBORATION: Historians of technology and media studies scholars widely corroborate the need for nuanced, co-evolutionary models, citing extensive research that demonstrates reciprocal influence rather than simple cause-and-effect. This perspective is supported by analysis of primary sources from both the printing industry and religious movements of the era.
narrative_ontology:disappearance_verdict(press_reformation_causation__mutual_shaping, world_rearranges).
narrative_ontology:founding_problem_status(press_reformation_causation__mutual_shaping, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(press_reformation_causation__mutual_shaping, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
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
 *   The low extractiveness (0.15) reflects that this is an analytical framework, not a material constraint extracting resources. The 'scaffold' classification is chosen because the press acted as a temporary, enabling structure for a historical transition (the Reformation), which then became integrated into the broader social and technological landscape. It was not a permanent, fixed 'mountain' nor a purely extractive 'snare'. The absence of active enforcement (false) and sunset clause (false) reflects its historical nature; the 'scaffold' here refers to its *function* in the historical process, not a policy with a literal sunset. The low suppression (0.05) and resistance (0.0) indicate that this analytical framework itself meets little opposition, though the historical events it describes were highly contested.
 *
 * PERSPECTIVAL GAP:
 *   This 'mutual shaping' reading stands in contrast to more reductionist views. A technological determinist would see the press as the primary cause, while a purely instrumentalist view would see it as a neutral tool. This constraint's classification as a 'scaffold' (an enabling structure) captures the dynamic interplay, where the press's affordances were exploited and reinforced by human agency, leading to a co-evolutionary outcome. The engine's classification of this analytical framework as a scaffold reflects its role in enabling a historical transition.
 *
 * DIRECTIONALITY LOGIC:
 *   Both Reformation reformers and the printing press industry are beneficiaries, as their co-evolutionary relationship mutually advanced their respective goals. The Catholic Church is a 'payer' in the sense that it bore the costs of adapting to this new dynamic, losing some of its control over information. The literate public also benefited from increased access to information. Historians of technology are observers, analyzing the dynamic.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    causal_primacy_ambiguity,
    'To what extent did the inherent properties of the printing press (e.g., reproducibility, speed) *pre-exist* and *enable* the Reformation, versus how much were these properties *amplified* or *directed* by the specific demands and innovations of the reformers?',
    'Detailed counterfactual historical analysis, comparing the actual trajectory with scenarios where either the technology or the agency was significantly altered.',
    'A stronger pre-existing enablement would lean towards a more ''mountain-like'' aspect of the press''s influence, while stronger amplification by agency would emphasize the ''rope-like'' coordination and adaptation aspects. This would shift the balance within the ''scaffold'' classification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(causal_primacy_ambiguity, empirical, 'Ambiguity in the precise balance of technological affordance vs. human agency in shaping historical outcomes.').

omega_variable(
    framing_underdetermination,
    'Is the ''mutual shaping'' framework the most appropriate lens, or does it obscure aspects better captured by ''technological determinism'' or ''strategic deployment''?',
    'Comparative historical analysis of different regions and time periods, testing which framework best explains the observed outcomes given varying initial conditions and agent intentions.',
    'If a different framework proves more explanatory in certain contexts, it would suggest that the ''mutual shaping'' reading is not universally applicable, potentially leading to a decomposition of the kernel into context-dependent constraints.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(framing_underdetermination, conceptual, 'Whether the ''mutual shaping'' framework is the optimal conceptual lens for this historical dynamic.').


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
narrative_ontology:measurement(pres_be_t1450, press_reformation_causation__mutual_shaping, base_extractiveness, 1450, 0.1).
narrative_ontology:measurement(pres_be_t1500, press_reformation_causation__mutual_shaping, base_extractiveness, 1500, 0.12).
narrative_ontology:measurement(pres_be_t1550, press_reformation_causation__mutual_shaping, base_extractiveness, 1550, 0.15).
narrative_ontology:measurement(pres_be_t1600, press_reformation_causation__mutual_shaping, base_extractiveness, 1600, 0.14).
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
% This constraint is one reading ('mutual_shaping') of the 'press_reformation_causation' kernel, which also includes 'technological_determinism' and 'strategic_deployment' readings. Each reading represents a distinct analytical framework for understanding the historical relationship.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
