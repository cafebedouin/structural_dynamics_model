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
 *   human_readable: Printing Press as Deterministic Force in Reformation
 *   domain: history_of_technology/religious_history/media_studies
 *
 * SUMMARY:
 *   This constraint story instantiates the 'technological determinism'
 *   reading of the 'press_reformation_causality' kernel. It posits the
 *   printing press as an autonomous, mountain-like force whose inherent
 *   properties (speed, reproducibility, cost-reduction) made the spread of
 *   vernacular scripture and the success of the Reformation inevitable. Human
 *   agency is largely seen as reactive to this technological imperative, and
 *   the beneficiary structure is obscured by the focus on the technology's
 *   inherent causal power.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(press_reformation_causality__technological_determinism, 0.05).
domain_priors:suppression_score(press_reformation_causality__technological_determinism, 0.95).
domain_priors:theater_ratio(press_reformation_causality__technological_determinism, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(press_reformation_causality__technological_determinism, extractiveness, 0.05).
narrative_ontology:constraint_metric(press_reformation_causality__technological_determinism, suppression_requirement, 0.95).
narrative_ontology:constraint_metric(press_reformation_causality__technological_determinism, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(press_reformation_causality__technological_determinism, accessibility_collapse, 0.95).
narrative_ontology:constraint_metric(press_reformation_causality__technological_determinism, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(press_reformation_causality__technological_determinism, mountain).
narrative_ontology:human_readable(press_reformation_causality__technological_determinism, "Printing Press as Deterministic Force in Reformation").
narrative_ontology:topic_domain(press_reformation_causality__technological_determinism, "history_of_technology/religious_history/media_studies").

domain_priors:emerges_naturally(press_reformation_causality__technological_determinism).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(press_reformation_causality__technological_determinism, 'f90a632a-b6e6-4eb6-a798-38614ee75b95').
narrative_ontology:cs_kernel_codification('f90a632a-b6e6-4eb6-a798-38614ee75b95', implicit).
narrative_ontology:cs_authority_grounding('f90a632a-b6e6-4eb6-a798-38614ee75b95', self_enforcing).
narrative_ontology:cs_reading_relation('f90a632a-b6e6-4eb6-a798-38614ee75b95', press_reformation_causality__co_constitution, forecloses).
narrative_ontology:cs_reading_relation('f90a632a-b6e6-4eb6-a798-38614ee75b95', press_reformation_causality__strategic_deployment, forecloses).
narrative_ontology:cs_axiom('f90a632a-b6e6-4eb6-a798-38614ee75b95', foundational, technological_imperative_is_primary).
narrative_ontology:cs_axiom_status(technological_imperative_is_primary, holdable).
narrative_ontology:cs_axiom_grounding('f90a632a-b6e6-4eb6-a798-38614ee75b95', technological_imperative_is_primary, empirically_contingent).
narrative_ontology:cs_axiom('f90a632a-b6e6-4eb6-a798-38614ee75b95', foundational, human_agency_is_reactive).
narrative_ontology:cs_axiom_status(human_agency_is_reactive, holdable).
narrative_ontology:cs_axiom_grounding('f90a632a-b6e6-4eb6-a798-38614ee75b95', human_agency_is_reactive, empirically_contingent).
narrative_ontology:cs_reference_frame('f90a632a-b6e6-4eb6-a798-38614ee75b95', technological_inevitability).
narrative_ontology:cs_drift_state('f90a632a-b6e6-4eb6-a798-38614ee75b95', contemporary_historical_analysis, gap(stable, minor, true)).
narrative_ontology:cs_created_at('f90a632a-b6e6-4eb6-a798-38614ee75b95', '').
narrative_ontology:cs_kernel_id(press_reformation_causality__technological_determinism, press_reformation_causality).

% --- Structural relationships ---
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(press_reformation_causality__technological_determinism, reformation_movement).
narrative_ontology:constraint_beneficiary(press_reformation_causality__technological_determinism, vernacular_readers).
narrative_ontology:constraint_victim(press_reformation_causality__technological_determinism, catholic_church).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The autonomous technological force whose inherent properties (speed, reproducibility, cost-reduction) inevitably drove the spread of vernacular scripture and the success of the Reformation. It is not an agent in the human sense, but the primary causal driver in this reading.
narrative_ontology:constraint_stakeholder(press_reformation_causality__technological_determinism, printing_press_technology, agenda_setter,
    institutional, civilizational, trapped, universal).
narrative_ontology:stakeholder_non_agent(press_reformation_causality__technological_determinism, printing_press_technology).

% Benefited from the inevitable spread of its ideas and texts, which the printing press made unstoppable. Its success is seen as a direct, unavoidable consequence of the technology's existence and properties.
narrative_ontology:constraint_stakeholder(press_reformation_causality__technological_determinism, reformation_movement, beneficiary,
    organized, generational, mobile, continental).

% Gained unprecedented access to religious texts in their own languages, leading to increased literacy and direct engagement with scripture, all driven by the press's inherent capabilities.
narrative_ontology:constraint_stakeholder(press_reformation_causality__technological_determinism, vernacular_readers, beneficiary,
    moderate, biographical, mobile, local).

% Suffered a loss of control over religious discourse and interpretation, as the printing press autonomously undermined its monopoly on information. Its resistance was ultimately futile against the technological imperative.
narrative_ontology:constraint_stakeholder(press_reformation_causality__technological_determinism, catholic_church, payer,
    institutional, generational, constrained, global).

% Analyze the historical impact of the printing press, often emphasizing its autonomous and deterministic role in shaping societal outcomes like the Reformation.
narrative_ontology:constraint_stakeholder(press_reformation_causality__technological_determinism, historians_of_technology, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The printing press inherently coordinated the rapid, widespread dissemination of information, enabling a shared textual basis for religious and social movements across vast distances.
% TRANSFER_FUNCTION: Transferred the power of information dissemination from centralized, elite institutions (like the Church) to a more distributed, technologically-driven system, making knowledge more accessible to the masses.
% ABSENT_VOICES: The voices of human agents who strategically adapted, resisted, or co-opted the technology are absent from this deterministic account; they would argue for the role of human choice and social context.
% DISAPPEARANCE_RATIONALE: If the deterministic causal power of the printing press vanished overnight, the historical narrative of the Reformation would be fundamentally rewritten. The spread of vernacular scripture would no longer be seen as inevitable, and the success of the Reformation would be attributed to other, non-technological factors, profoundly altering our understanding of history.
% FOUNDING_PROBLEM: The problem of slow, expensive, and centrally controlled information dissemination, which limited access to knowledge and hindered rapid social and religious change.
% FOUNDING_PROBLEM_CORROBORATION: Historians and media theorists adhering to technological determinism attest that the printing press effectively 'solved' the problem of information scarcity and control, making its impact inevitable. The Catholic Church's historical records, while not corroborating inevitability, document the rapid and uncontrollable spread of dissenting texts.
narrative_ontology:disappearance_verdict(press_reformation_causality__technological_determinism, world_rearranges).
narrative_ontology:founding_problem_status(press_reformation_causality__technological_determinism, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(press_reformation_causality__technological_determinism, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
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
 *   The low extractiveness and theater_ratio reflect the view of technology as a neutral, functional force, not actively extracting rents or performing theatrically. High suppression, accessibility_collapse, and low resistance reflect the inevitability of its impact: alternatives to its influence collapsed, and resistance was largely futile against its autonomous spread. The claimed type 'mountain' directly reflects the prompt's instruction to classify the technology as such, emphasizing its unchangeable, fixed nature in this reading.
 *
 * PERSPECTIVAL GAP:
 *   Other readings, such as 'strategic_deployment' or 'co_constitution,' would emphasize human agency, strategic choices by reformers and printers, and the mutual shaping of technology and society. This deterministic reading, however, foregrounds the technology's autonomous causal power, leading to a fundamentally different classification where the technology itself is a 'mountain' and human actors are downstream responders.
 *
 * DIRECTIONALITY LOGIC:
 *   From this deterministic perspective, the printing press itself is the primary 'agenda-setter' or causal force. The Reformation movement and vernacular readers are 'beneficiaries' in the sense that they were the inevitable recipients of the technology's transformative power. The Catholic Church is a 'payer' as it bore the costs of losing its information monopoly, unable to resist the technological tide. The 'beneficiary structure obscured' aspect means that while effects are clear, the 'beneficiary' is less an active agent collecting rents and more a passive recipient of an inevitable outcome.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is this constraint a valid, self-consistent reading of the ''press_reformation_causality'' kernel?',
    'Conceptual analysis of the internal consistency of the technological determinism argument within historical and media studies discourse.',
    'If inconsistent, this reading would be reclassified as a conceptual error or a non-viable framing, leading to its removal from the kernel''s valid readings.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Confirms this constraint as the ''technological_determinism'' reading of the ''press_reformation_causality'' kernel.').

omega_variable(
    technology_as_mountain_ambiguity,
    'Is the printing press truly an ''autonomous enabling technology'' with mountain-like inevitability, or is its impact mediated by human agency and social context?',
    'Comparative historical analysis of print cultures in different societies (e.g., East Asia vs. Europe) and periods, examining variations in impact despite similar technological capabilities.',
    'If human agency and context are found to be primary mediators, the constraint would be reclassified from ''mountain'' to a ''rope'' or ''tangled_rope'' (e.g., ''strategic_deployment'' reading), with higher extractiveness and lower suppression, reflecting human choice and contestation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(technology_as_mountain_ambiguity, empirical, 'Ambiguity of technology''s autonomous causal power vs. human mediation.').

omega_variable(
    human_agency_obscured_ambiguity,
    'Does this deterministic reading obscure the active choices and strategic actions of individuals and groups in shaping the Reformation, thereby misrepresenting their roles?',
    'Analysis of primary historical sources (e.g., letters, manifestos, legal documents) detailing the intentional efforts of reformers, printers, and authorities to use or control print media.',
    'If significant, intentional human agency is revealed, the ''beneficiary'' and ''victim'' roles would become more active and less passive, potentially shifting the constraint''s classification towards a ''tangled_rope'' or ''snare'' (e.g., ''strategic_deployment'' reading) where human actors actively extract or are extracted from.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(human_agency_obscured_ambiguity, empirical, 'Whether human agency is genuinely absent or merely obscured by the deterministic framing.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(press_reformation_causality__technological_determinism, 1450, 1650).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pres_tr_t1450, press_reformation_causality__technological_determinism, theater_ratio, 1450, 0.01).
narrative_ontology:measurement(pres_tr_t1500, press_reformation_causality__technological_determinism, theater_ratio, 1500, 0.02).
narrative_ontology:measurement(pres_tr_t1550, press_reformation_causality__technological_determinism, theater_ratio, 1550, 0.03).
narrative_ontology:measurement(pres_tr_t1600, press_reformation_causality__technological_determinism, theater_ratio, 1600, 0.04).
narrative_ontology:measurement(pres_tr_t1650, press_reformation_causality__technological_determinism, theater_ratio, 1650, 0.05).

% Extraction over time
narrative_ontology:measurement(pres_be_t1450, press_reformation_causality__technological_determinism, base_extractiveness, 1450, 0.01).
narrative_ontology:measurement(pres_be_t1500, press_reformation_causality__technological_determinism, base_extractiveness, 1500, 0.02).
narrative_ontology:measurement(pres_be_t1550, press_reformation_causality__technological_determinism, base_extractiveness, 1550, 0.03).
narrative_ontology:measurement(pres_be_t1600, press_reformation_causality__technological_determinism, base_extractiveness, 1600, 0.04).
narrative_ontology:measurement(pres_be_t1650, press_reformation_causality__technological_determinism, base_extractiveness, 1650, 0.05).

% Suppression requirement over time
narrative_ontology:measurement(pres_su_t1450, press_reformation_causality__technological_determinism, suppression_requirement, 1450, 0.8).
narrative_ontology:measurement(pres_su_t1500, press_reformation_causality__technological_determinism, suppression_requirement, 1500, 0.85).
narrative_ontology:measurement(pres_su_t1550, press_reformation_causality__technological_determinism, suppression_requirement, 1550, 0.9).
narrative_ontology:measurement(pres_su_t1600, press_reformation_causality__technological_determinism, suppression_requirement, 1600, 0.93).
narrative_ontology:measurement(pres_su_t1650, press_reformation_causality__technological_determinism, suppression_requirement, 1650, 0.95).


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
