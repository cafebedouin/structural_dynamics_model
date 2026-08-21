% ============================================================================
% CONSTRAINT STORY: press_reformation_causality__technological_determinism
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_press_reformation_causality__technological_determinism, []).

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
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: press_reformation_causality__technological_determinism
 *   human_readable: Printing Press as Autonomous Determinant of Reformation
 *   domain: history_of_technology/religious_history/media_studies
 *
 * SUMMARY:
 *   This constraint story instantiates the 'technological_determinism'
 *   reading of the 'press_reformation_causality' kernel. It posits the
 *   printing press as an autonomous enabling technology whose inherent
 *   properties made the spread of vernacular scripture and the success of the
 *   Reformation inevitable. Human agency in this narrative is largely
 *   reactive to the technological imperative. The constraint is the perceived
 *   inevitability of the historical outcome, not a mutable social structure.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(press_reformation_causality__technological_determinism, 0.05).
domain_priors:suppression_score(press_reformation_causality__technological_determinism, 0.05).
domain_priors:theater_ratio(press_reformation_causality__technological_determinism, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(press_reformation_causality__technological_determinism, extractiveness, 0.05).
narrative_ontology:constraint_metric(press_reformation_causality__technological_determinism, suppression_requirement, 0.05).
narrative_ontology:constraint_metric(press_reformation_causality__technological_determinism, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(press_reformation_causality__technological_determinism, accessibility_collapse, 0.9).
narrative_ontology:constraint_metric(press_reformation_causality__technological_determinism, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(press_reformation_causality__technological_determinism, mountain).
narrative_ontology:human_readable(press_reformation_causality__technological_determinism, "Printing Press as Autonomous Determinant of Reformation").
narrative_ontology:topic_domain(press_reformation_causality__technological_determinism, "history_of_technology/religious_history/media_studies").

domain_priors:emerges_naturally(press_reformation_causality__technological_determinism).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(press_reformation_causality__technological_determinism, 'df035675-03ca-4fd9-b45e-3969b5d04b9e').
narrative_ontology:cs_kernel_codification('df035675-03ca-4fd9-b45e-3969b5d04b9e', implicit).
narrative_ontology:cs_authority_grounding('df035675-03ca-4fd9-b45e-3969b5d04b9e', self_enforcing).
narrative_ontology:cs_reading_relation('df035675-03ca-4fd9-b45e-3969b5d04b9e', press_reformation_causality__strategic_deployment, forecloses).
narrative_ontology:cs_reading_relation('df035675-03ca-4fd9-b45e-3969b5d04b9e', press_reformation_causality__co_constitution, forecloses).
narrative_ontology:cs_axiom('df035675-03ca-4fd9-b45e-3969b5d04b9e', foundational, technology_as_autonomous_force).
narrative_ontology:cs_axiom_status(technology_as_autonomous_force, holdable).
narrative_ontology:cs_axiom_grounding('df035675-03ca-4fd9-b45e-3969b5d04b9e', technology_as_autonomous_force, deontological).
narrative_ontology:cs_axiom('df035675-03ca-4fd9-b45e-3969b5d04b9e', foundational, social_change_as_technologically_determined).
narrative_ontology:cs_axiom_status(social_change_as_technologically_determined, holdable).
narrative_ontology:cs_axiom_grounding('df035675-03ca-4fd9-b45e-3969b5d04b9e', social_change_as_technologically_determined, empirically_contingent).
narrative_ontology:cs_reference_frame('df035675-03ca-4fd9-b45e-3969b5d04b9e', technological_autonomy_paradigm).
narrative_ontology:cs_drift_state('df035675-03ca-4fd9-b45e-3969b5d04b9e', contemporary_media_studies_critique, gap(stable, minor, false)).
narrative_ontology:cs_created_at('df035675-03ca-4fd9-b45e-3969b5d04b9e', '').
narrative_ontology:cs_kernel_id(press_reformation_causality__technological_determinism, press_reformation_causality).

% --- Structural relationships ---
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(press_reformation_causality__technological_determinism, vernacular_readers).
narrative_ontology:constraint_beneficiary(press_reformation_causality__technological_determinism, reformation_leaders).
narrative_ontology:constraint_victim(press_reformation_causality__technological_determinism, catholic_church).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The printing press itself, viewed as an autonomous force whose inherent capabilities (mass production, standardization) inevitably led to the spread of vernacular scripture and the success of the Reformation. It sets the 'agenda' by its mere existence and capabilities.
narrative_ontology:constraint_stakeholder(press_reformation_causality__technological_determinism, printing_press_technology, agenda_setter,
    institutional, generational, trapped, continental).
narrative_ontology:stakeholder_non_agent(press_reformation_causality__technological_determinism, printing_press_technology).

% Individuals who gained access to scripture in their native languages due to the press. Their increased literacy and direct engagement with religious texts are seen as an inevitable outcome of the technology's spread, rather than a choice.
narrative_ontology:constraint_stakeholder(press_reformation_causality__technological_determinism, vernacular_readers, beneficiary,
    powerless, biographical, constrained, local).

% Figures like Martin Luther, whose ideas and writings were rapidly disseminated by the press. Their success is attributed to the press's inherent power to spread information, making their role more reactive to an inevitable technological force.
narrative_ontology:constraint_stakeholder(press_reformation_causality__technological_determinism, reformation_leaders, beneficiary,
    moderate, biographical, constrained, regional).

% The established religious authority whose monopoly on scripture interpretation and dissemination was eroded by the printing press. Their efforts to suppress vernacular texts and maintain control are seen as ultimately futile against the technology's inevitable impact.
narrative_ontology:constraint_stakeholder(press_reformation_causality__technological_determinism, catholic_church, payer,
    institutional, generational, constrained, continental).

% Scholars who interpret historical events, particularly the Reformation, through the lens of technological determinism, emphasizing the printing press as the primary, autonomous driver of social and religious change.
narrative_ontology:constraint_stakeholder(press_reformation_causality__technological_determinism, historians_technological_determinism, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The printing press, by its nature, coordinated the widespread, standardized dissemination of information, enabling a shared textual basis for religious and intellectual discourse across diverse populations.
% TRANSFER_FUNCTION: Transferred religious authority and interpretive power from the centralized Catholic Church to individual readers and local communities, driven by the inevitable spread of vernacular texts.
% ABSENT_VOICES: The voices of individual printers and reformers, who actively made strategic choices about what to print and how to distribute it, are largely absent or downplayed in this deterministic narrative, as their agency is subsumed by the technology's 'inevitable' impact.
% DISAPPEARANCE_RATIONALE: If the deterministic influence of the printing press were to vanish, the historical fact of the Reformation's success and the spread of vernacular scripture would remain unchanged, as this reading posits its inevitability. The 'constraint' is the historical outcome, not a mutable structure.
% FOUNDING_PROBLEM: The problem of understanding how a single technological innovation could lead to such profound and widespread social and religious upheaval, seeking a clear, unidirectional causal explanation.
% FOUNDING_PROBLEM_CORROBORATION: Proponents of technological determinism within media studies and history continue to attest to the live status of this problem, arguing for the press's autonomous role. Critics from co-constitution and strategic deployment perspectives contest this, citing evidence of human agency and feedback loops.
narrative_ontology:disappearance_verdict(press_reformation_causality__technological_determinism, world_unchanged).
narrative_ontology:founding_problem_status(press_reformation_causality__technological_determinism, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(press_reformation_causality__technological_determinism, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(press_reformation_causality__technological_determinism, 'none', 1).
narrative_ontology:epsilon_provenance(press_reformation_causality__technological_determinism, 0.05, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(press_reformation_causality__technological_determinism_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(press_reformation_causality__technological_determinism, ExtMetricName, E),
    domain_priors:suppression_score(press_reformation_causality__technological_determinism, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(press_reformation_causality__technological_determinism),
    narrative_ontology:constraint_metric(press_reformation_causality__technological_determinism, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(press_reformation_causality__technological_determinism, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(press_reformation_causality__technological_determinism_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The metrics reflect the deterministic view: extractiveness, suppression, and theater are all very low (0.05) because the press is seen as a neutral, unstoppable force, not actively extracting or suppressing, nor engaging in performance. Accessibility collapse is very high (0.90) because the effects are considered unavoidable, collapsing alternatives to its influence. Resistance is low (0.05) because resistance against an inevitable force is futile. The claimed type is 'mountain' as it's treated as an irreducible, natural-law-like force in history.
 *
 * PERSPECTIVAL GAP:
 *   This reading presents the press's influence as a fixed, inevitable force, obscuring the agency of printers, reformers, and political actors. Other readings (strategic_deployment, co_constitution) would emphasize human choices and feedback loops, leading to very different classifications and stakeholder dynamics.
 *
 * DIRECTIONALITY LOGIC:
 *   The 'printing_press_technology' itself is framed as the agenda-setter, an autonomous force. 'Vernacular_readers' and 'reformation_leaders' are beneficiaries of its inevitable impact. The 'catholic_church' is a payer, bearing the cost of its authority's erosion. 'Historians_technological_determinism' are observers, analyzing this deterministic causality.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is this constraint a genuine ''mountain'' of technological inevitability, or is it a specific ''reading'' of a more complex historical kernel?',
    'Comparative historical analysis across different regions and time periods, examining variations in the press''s impact based on local social, political, and religious contexts, rather than universal technological properties.',
    'If confirmed as a reading, the classification would shift from ''mountain'' to a type that accounts for human agency and social contingency (e.g., ''rope'' or ''tangled_rope'' for the ''co_constitution'' reading, or ''snare'' for specific ''strategic_deployment'' instances).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'This constraint is the ''technological_determinism'' reading of the ''press_reformation_causality'' kernel, which treats the press''s influence as an autonomous, inevitable force.').

omega_variable(
    agency_vs_autonomy_ambiguity,
    'To what extent was the spread of vernacular scripture and the Reformation''s success an autonomous outcome of printing technology, versus the result of strategic choices and active deployment by human agents?',
    'Detailed historical case studies focusing on the decisions of printers, patrons, and reformers regarding content, language, and distribution, and the responses of authorities, to quantify the role of human agency versus technological affordances.',
    'If human agency is found to be dominant, the constraint''s extractiveness and suppression metrics would likely increase, and its classification would shift from ''mountain'' to a type reflecting active coordination or extraction (e.g., ''tangled_rope'' or ''snare'').',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(agency_vs_autonomy_ambiguity, empirical, 'Ambiguity between technological autonomy and human strategic agency in driving historical outcomes.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(press_reformation_causality__technological_determinism, 1450, 1650).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pres_tr_t1450, press_reformation_causality__technological_determinism, theater_ratio, 1450, 0.05).
narrative_ontology:measurement(pres_tr_t1500, press_reformation_causality__technological_determinism, theater_ratio, 1500, 0.05).
narrative_ontology:measurement(pres_tr_t1550, press_reformation_causality__technological_determinism, theater_ratio, 1550, 0.05).
narrative_ontology:measurement(pres_tr_t1600, press_reformation_causality__technological_determinism, theater_ratio, 1600, 0.05).
narrative_ontology:measurement(pres_tr_t1650, press_reformation_causality__technological_determinism, theater_ratio, 1650, 0.05).

% Extraction over time
narrative_ontology:measurement(pres_be_t1450, press_reformation_causality__technological_determinism, base_extractiveness, 1450, 0.05).
narrative_ontology:measurement(pres_be_t1500, press_reformation_causality__technological_determinism, base_extractiveness, 1500, 0.05).
narrative_ontology:measurement(pres_be_t1550, press_reformation_causality__technological_determinism, base_extractiveness, 1550, 0.05).
narrative_ontology:measurement(pres_be_t1600, press_reformation_causality__technological_determinism, base_extractiveness, 1600, 0.05).
narrative_ontology:measurement(pres_be_t1650, press_reformation_causality__technological_determinism, base_extractiveness, 1650, 0.05).

% Suppression requirement over time
narrative_ontology:measurement(pres_su_t1450, press_reformation_causality__technological_determinism, suppression_requirement, 1450, 0.05).
narrative_ontology:measurement(pres_su_t1500, press_reformation_causality__technological_determinism, suppression_requirement, 1500, 0.05).
narrative_ontology:measurement(pres_su_t1550, press_reformation_causality__technological_determinism, suppression_requirement, 1550, 0.05).
narrative_ontology:measurement(pres_su_t1600, press_reformation_causality__technological_determinism, suppression_requirement, 1600, 0.05).
narrative_ontology:measurement(pres_su_t1650, press_reformation_causality__technological_determinism, suppression_requirement, 1650, 0.05).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(press_reformation_causality__technological_determinism, information_standard).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
