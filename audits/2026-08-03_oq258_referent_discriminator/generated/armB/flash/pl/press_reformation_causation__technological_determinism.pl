% ============================================================================
% CONSTRAINT STORY: press_reformation_causation__technological_determinism
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_press_reformation_causation__technological_determinism, []).

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
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: press_reformation_causation__technological_determinism
 *   human_readable: Printing Press as Deterministic Cause of Reformation
 *   domain: history_of_technology/religious_history/media_studies
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(press_reformation_causation__technological_determinism, 0.05).
domain_priors:suppression_score(press_reformation_causation__technological_determinism, 0.02).
domain_priors:theater_ratio(press_reformation_causation__technological_determinism, 0.01).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(press_reformation_causation__technological_determinism, extractiveness, 0.05).
narrative_ontology:constraint_metric(press_reformation_causation__technological_determinism, suppression_requirement, 0.02).
narrative_ontology:constraint_metric(press_reformation_causation__technological_determinism, theater_ratio, 0.01).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(press_reformation_causation__technological_determinism, accessibility_collapse, 0.95).
narrative_ontology:constraint_metric(press_reformation_causation__technological_determinism, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(press_reformation_causation__technological_determinism, mountain).
narrative_ontology:human_readable(press_reformation_causation__technological_determinism, "Printing Press as Deterministic Cause of Reformation").
narrative_ontology:topic_domain(press_reformation_causation__technological_determinism, "history_of_technology/religious_history/media_studies").

domain_priors:emerges_naturally(press_reformation_causation__technological_determinism).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(press_reformation_causation__technological_determinism, 'c8d52139-dddc-4efe-a71e-d49bb895f85f').
narrative_ontology:cs_kernel_codification('c8d52139-dddc-4efe-a71e-d49bb895f85f', implicit).
narrative_ontology:cs_authority_grounding('c8d52139-dddc-4efe-a71e-d49bb895f85f', diffuse_epistemic).
narrative_ontology:cs_reading_relation('c8d52139-dddc-4efe-a71e-d49bb895f85f', press_reformation_causation__strategic_deployment, forecloses).
narrative_ontology:cs_reading_relation('c8d52139-dddc-4efe-a71e-d49bb895f85f', press_reformation_causation__mutual_shaping, forecloses).
narrative_ontology:cs_axiom('c8d52139-dddc-4efe-a71e-d49bb895f85f', foundational, technology_determines_social_change).
narrative_ontology:cs_axiom_status(technology_determines_social_change, holdable).
narrative_ontology:cs_axiom_grounding('c8d52139-dddc-4efe-a71e-d49bb895f85f', technology_determines_social_change, empirically_contingent).
narrative_ontology:cs_axiom('c8d52139-dddc-4efe-a71e-d49bb895f85f', secondary, censorship_rendered_impossible_by_press).
narrative_ontology:cs_axiom_status(censorship_rendered_impossible_by_press, holdable).
narrative_ontology:cs_axiom_grounding('c8d52139-dddc-4efe-a71e-d49bb895f85f', censorship_rendered_impossible_by_press, empirically_contingent).
narrative_ontology:cs_reference_frame('c8d52139-dddc-4efe-a71e-d49bb895f85f', technology_as_prime_mover).
narrative_ontology:cs_drift_state('c8d52139-dddc-4efe-a71e-d49bb895f85f', contemporary_historical_scholarship, gap(axiom_overriding, substantial, true)).
narrative_ontology:cs_created_at('c8d52139-dddc-4efe-a71e-d49bb895f85f', '').
narrative_ontology:cs_kernel_id(press_reformation_causation__technological_determinism, press_reformation_causation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(press_reformation_causation__technological_determinism, protestant_reformers).
narrative_ontology:constraint_beneficiary(press_reformation_causation__technological_determinism, vernacular_readers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(press_reformation_causation__technological_determinism, catholic_church).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The technology itself, acting as an autonomous force that inherently drives social change. It dictates the possibilities and impossibilities of information dissemination, making prior forms of control obsolete.
narrative_ontology:constraint_stakeholder(press_reformation_causation__technological_determinism, printing_press_technology, agenda_setter,
    institutional, civilizational, analytical, universal).

% Benefited from the printing press's inherent capacity to rapidly disseminate their ideas and vernacular translations of scripture, bypassing traditional gatekeepers. They were empowered by the technology's exogenous force.
narrative_ontology:constraint_stakeholder(press_reformation_causation__technological_determinism, protestant_reformers, beneficiary,
    organized, generational, mobile, continental).

% Its traditional authority structure, based on controlled information flow and Latin scripture, was fundamentally undermined by the printing press. Its attempts at censorship were rendered futile by the technology's unstoppable spread.
narrative_ontology:constraint_stakeholder(press_reformation_causation__technological_determinism, catholic_church, payer,
    institutional, civilizational, trapped, global).

% Gained unprecedented access to religious texts in their native languages, fostering individual interpretation and reducing reliance on clerical intermediaries. This was an inevitable outcome of the press's operation.
narrative_ontology:constraint_stakeholder(press_reformation_causation__technological_determinism, vernacular_readers, beneficiary,
    moderate, biographical, mobile, regional).

% Analyze the causal relationship between technological innovation and social change, often seeking to identify deterministic forces. This reading aligns with a view of technology as a primary driver.
narrative_ontology:constraint_stakeholder(press_reformation_causation__technological_determinism, historians_of_technology, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The printing press inherently coordinated the rapid, widespread, and decentralized dissemination of information, enabling a new form of public discourse that bypassed traditional bottlenecks.
% TRANSFER_FUNCTION: Transferred the power of information control from centralized authorities (like the Church) to the technology itself, which then diffused it broadly to a mass readership.
% ABSENT_VOICES: Scholars emphasizing human agency, strategic choices, and the co-evolution of technology and society would object, arguing that the press was a tool whose impact depended on how it was used, rather than an autonomous cause.
% DISAPPEARANCE_RATIONALE: If the deterministic causal link vanished, the narrative of the Reformation would fundamentally change, requiring a re-evaluation of the roles of human agency, political context, and religious doctrine in driving historical events, rather than attributing them to an inevitable technological force.
% FOUNDING_PROBLEM: The problem of explaining the rapid and widespread success of the Reformation, particularly its ability to challenge entrenched religious authority.
% FOUNDING_PROBLEM_CORROBORATION: Proponents of technological determinism in historical analysis attest to the problem's live status, viewing it as a prime example of technology's transformative power. Critics, however, contest this, arguing for more nuanced, multi-causal explanations.
narrative_ontology:disappearance_verdict(press_reformation_causation__technological_determinism, world_rearranges).
narrative_ontology:founding_problem_status(press_reformation_causation__technological_determinism, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(press_reformation_causation__technological_determinism, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'f1436bd4937f864097dabaad92b27bd9b6eec212', '2026-08-03',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(press_reformation_causation__technological_determinism, 'none', 1).
narrative_ontology:epsilon_provenance(press_reformation_causation__technological_determinism, 0.05, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(press_reformation_causation__technological_determinism_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(press_reformation_causation__technological_determinism, ExtMetricName, E),
    domain_priors:suppression_score(press_reformation_causation__technological_determinism, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(press_reformation_causation__technological_determinism),
    narrative_ontology:constraint_metric(press_reformation_causation__technological_determinism, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(press_reformation_causation__technological_determinism, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(press_reformation_causation__technological_determinism_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */


/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    technological_agency_ambiguity,
    'Is the printing press an autonomous causal agent (as this reading suggests), or a neutral tool whose impact is mediated by human agency and strategic deployment?',
    'Comparative historical analysis of other technological introductions where human choices demonstrably shaped outcomes, or counterfactual analysis of the Reformation without strategic deployment by reformers.',
    'If human agency is primary, the constraint shifts from a ''mountain'' (exogenous force) to a ''rope'' or ''tangled_rope'' (coordination/extraction through human action), with different beneficiaries and victims.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(technological_agency_ambiguity, conceptual, 'Ambiguity regarding the printing press''s causal autonomy vs. its role as a tool.').

omega_variable(
    causal_determinism_vs_contingency,
    'To what extent was the Reformation an inevitable outcome of the printing press, versus a contingent historical event shaped by multiple interacting factors (theological, political, social)?',
    'Detailed historical studies that trace the specific pathways of influence, identifying points of contingency where different outcomes were possible, or comparative studies of regions where the press was present but the Reformation did not occur.',
    'If contingency is high, the ''mountain'' classification for the press''s causal power is weakened, potentially reclassifying it as a ''rope'' or ''scaffold'' that enabled, rather than determined, the Reformation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(causal_determinism_vs_contingency, empirical, 'The degree of causal determinism attributed to the printing press.').

omega_variable(
    kernel_reading_identification,
    'This constraint is one reading of the ''press_reformation_causation'' kernel. This specific reading is ''technological_determinism''. What would change if a sibling reading were adopted?',
    'Adopting the ''strategic_deployment'' reading would shift the focus to reformers'' agency and the press as a tool, changing the beneficiary structure and potentially the constraint type to a ''rope'' or ''tangled_rope''. Adopting ''mutual_shaping'' would emphasize co-evolution, making the constraint more dynamic and less deterministic.',
    'The entire structural analysis, including claimed type, beneficiaries, victims, and metrics, would change significantly, reflecting a different understanding of causality and agency.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identification, conceptual, 'This constraint is the ''technological_determinism'' reading of the ''press_reformation_causation'' kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(press_reformation_causation__technological_determinism, 1450, 1550).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pres_tr_t1450, press_reformation_causation__technological_determinism, theater_ratio, 1450, 0.01).
narrative_ontology:measurement(pres_tr_t1475, press_reformation_causation__technological_determinism, theater_ratio, 1475, 0.01).
narrative_ontology:measurement(pres_tr_t1500, press_reformation_causation__technological_determinism, theater_ratio, 1500, 0.01).
narrative_ontology:measurement(pres_tr_t1525, press_reformation_causation__technological_determinism, theater_ratio, 1525, 0.01).
narrative_ontology:measurement(pres_tr_t1550, press_reformation_causation__technological_determinism, theater_ratio, 1550, 0.01).

% Extraction over time
narrative_ontology:measurement(pres_be_t1450, press_reformation_causation__technological_determinism, base_extractiveness, 1450, 0.01).
narrative_ontology:measurement(pres_be_t1475, press_reformation_causation__technological_determinism, base_extractiveness, 1475, 0.02).
narrative_ontology:measurement(pres_be_t1500, press_reformation_causation__technological_determinism, base_extractiveness, 1500, 0.03).
narrative_ontology:measurement(pres_be_t1525, press_reformation_causation__technological_determinism, base_extractiveness, 1525, 0.04).
narrative_ontology:measurement(pres_be_t1550, press_reformation_causation__technological_determinism, base_extractiveness, 1550, 0.05).

% Suppression requirement over time
narrative_ontology:measurement(pres_su_t1450, press_reformation_causation__technological_determinism, suppression_requirement, 1450, 0.01).
narrative_ontology:measurement(pres_su_t1475, press_reformation_causation__technological_determinism, suppression_requirement, 1475, 0.01).
narrative_ontology:measurement(pres_su_t1500, press_reformation_causation__technological_determinism, suppression_requirement, 1500, 0.02).
narrative_ontology:measurement(pres_su_t1525, press_reformation_causation__technological_determinism, suppression_requirement, 1525, 0.02).
narrative_ontology:measurement(pres_su_t1550, press_reformation_causation__technological_determinism, suppression_requirement, 1550, 0.02).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(press_reformation_causation__technological_determinism, information_standard).
narrative_ontology:affects_constraint(press_reformation_causation__technological_determinism, press_reformation_causation__strategic_deployment).
narrative_ontology:affects_constraint(press_reformation_causation__technological_determinism, press_reformation_causation__mutual_shaping).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'printing press caused the Reformation' kernel. This 'technological_determinism' reading posits the press as an autonomous causal force. The 'strategic_deployment' reading emphasizes human agency, and the 'mutual_shaping' reading focuses on co-evolution. Each is a distinct constraint with different structural properties.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
