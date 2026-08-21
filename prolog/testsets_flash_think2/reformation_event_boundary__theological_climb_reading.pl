% ============================================================================
% CONSTRAINT STORY: reformation_event_boundary__theological_climb_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_reformation_event_boundary__theological_climb_reading, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: reformation_event_boundary__theological_climb_reading
 *   human_readable: Reformation as Theological Breakthrough (Theological Climb Reading)
 *   domain: historical_epistemology/religious_history/commitment_system_analysis
 *
 * SUMMARY:
 *   This constraint story instantiates the 'theological_climb_reading' of the
 *   'reformation_event_boundary' kernel. It frames the Reformation as
 *   primarily a theological innovation, driven by Luther's rediscovery of
 *   justification by faith alone, which is presented as a genuine doctrinal
 *   breakthrough. This truth, once revealed, 'required' institutional
 *   separation from the Catholic Church, which is seen as having deviated
 *   from core Christian doctrine. The constraint itself is the theological
 *   truth, which is presented as a liberating and unchangeable 'Mountain'
 *   from this reading's perspective.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(reformation_event_boundary__theological_climb_reading, 0.1).
domain_priors:suppression_score(reformation_event_boundary__theological_climb_reading, 0.1).
domain_priors:theater_ratio(reformation_event_boundary__theological_climb_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(reformation_event_boundary__theological_climb_reading, extractiveness, 0.1).
narrative_ontology:constraint_metric(reformation_event_boundary__theological_climb_reading, suppression_requirement, 0.1).
narrative_ontology:constraint_metric(reformation_event_boundary__theological_climb_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(reformation_event_boundary__theological_climb_reading, accessibility_collapse, 0.9).
narrative_ontology:constraint_metric(reformation_event_boundary__theological_climb_reading, resistance, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(reformation_event_boundary__theological_climb_reading, mountain).
narrative_ontology:human_readable(reformation_event_boundary__theological_climb_reading, "Reformation as Theological Breakthrough (Theological Climb Reading)").
narrative_ontology:topic_domain(reformation_event_boundary__theological_climb_reading, "historical_epistemology/religious_history/commitment_system_analysis").

domain_priors:emerges_naturally(reformation_event_boundary__theological_climb_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(reformation_event_boundary__theological_climb_reading, 'f64fe02e-f42f-4448-9c9f-f1421492a1f2').
narrative_ontology:cs_kernel_codification('f64fe02e-f42f-4448-9c9f-f1421492a1f2', fixed_text).
narrative_ontology:cs_authority_grounding('f64fe02e-f42f-4448-9c9f-f1421492a1f2', lineage).
narrative_ontology:cs_interpretation_layer_present('f64fe02e-f42f-4448-9c9f-f1421492a1f2').
narrative_ontology:cs_reading_relation('f64fe02e-f42f-4448-9c9f-f1421492a1f2', reformation_event_boundary__political_swap_reading, forecloses).
narrative_ontology:cs_reading_relation('f64fe02e-f42f-4448-9c9f-f1421492a1f2', reformation_event_boundary__composite_overdetermination_reading, coexists_with).
narrative_ontology:cs_axiom('f64fe02e-f42f-4448-9c9f-f1421492a1f2', foundational, justification_by_faith_alone_is_divine_truth).
narrative_ontology:cs_axiom_status(justification_by_faith_alone_is_divine_truth, holdable).
narrative_ontology:cs_axiom_grounding('f64fe02e-f42f-4448-9c9f-f1421492a1f2', justification_by_faith_alone_is_divine_truth, theological).
narrative_ontology:cs_axiom('f64fe02e-f42f-4448-9c9f-f1421492a1f2', foundational, scripture_is_sole_infallible_authority).
narrative_ontology:cs_axiom_status(scripture_is_sole_infallible_authority, holdable).
narrative_ontology:cs_axiom_grounding('f64fe02e-f42f-4448-9c9f-f1421492a1f2', scripture_is_sole_infallible_authority, theological).
narrative_ontology:cs_reference_frame('f64fe02e-f42f-4448-9c9f-f1421492a1f2', lutheran_theological_paradigm).
narrative_ontology:cs_drift_state('f64fe02e-f42f-4448-9c9f-f1421492a1f2', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('f64fe02e-f42f-4448-9c9f-f1421492a1f2', '').
narrative_ontology:cs_kernel_id(reformation_event_boundary__theological_climb_reading, reformation_event_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(reformation_event_boundary__theological_climb_reading, believers_in_justification_by_faith).
narrative_ontology:constraint_beneficiary(reformation_event_boundary__theological_climb_reading, protestant_reformers).
narrative_ontology:constraint_victim(reformation_event_boundary__theological_climb_reading, catholic_church_hierarchy).
narrative_ontology:constraint_victim(reformation_event_boundary__theological_climb_reading, theologians_of_the_old_guard).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(reformation_event_boundary__theological_climb_reading, secular_rulers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The primary proponents and articulators of the new theological understanding. They actively challenged existing doctrines and institutional structures based on this 'rediscovered' truth, leading to institutional separation.
narrative_ontology:constraint_stakeholder(reformation_event_boundary__theological_climb_reading, protestant_reformers, agenda_setter,
    powerful, biographical, mobile, regional).

% Individuals who embraced the doctrine of justification by faith alone, finding spiritual liberation from perceived burdens of works-righteousness and ecclesiastical mediation. They benefit from the clarity and directness of this theological truth.
narrative_ontology:constraint_stakeholder(reformation_event_boundary__theological_climb_reading, believers_in_justification_by_faith, beneficiary,
    powerless, biographical, mobile, local).

% The established ecclesiastical authority whose theological claims and institutional power were directly challenged and undermined by the new doctrine. They bore the cost of schism, loss of members, and erosion of spiritual authority.
narrative_ontology:constraint_stakeholder(reformation_event_boundary__theological_climb_reading, catholic_church_hierarchy, payer,
    institutional, generational, constrained, global).

% Scholars and teachers committed to scholastic theology and traditional Catholic doctrine. Their intellectual work and professional identity were challenged by the reformers, forcing them to defend established positions or face marginalization.
narrative_ontology:constraint_stakeholder(reformation_event_boundary__theological_climb_reading, theologians_of_the_old_guard, payer,
    moderate, biographical, identity_locked, global).

% Initially observed the theological disputes, but quickly recognized opportunities to assert greater autonomy from papal authority and potentially seize church lands. From this reading's perspective, their actions were secondary to the theological imperative.
narrative_ontology:constraint_stakeholder(reformation_event_boundary__theological_climb_reading, secular_rulers, observer,
    powerful, biographical, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(reformation_event_boundary__theological_climb_reading, secular_rulers, beneficiary).

% Those who continued to believe in the necessity of good works and sacraments for salvation, or who were deeply invested in the traditional Catholic framework. Their theological perspective was actively rejected by the reformers.
narrative_ontology:constraint_stakeholder(reformation_event_boundary__theological_climb_reading, adherents_of_works_righteousness, excluded,
    powerless, biographical, identity_locked, local).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Unifies believers around a core theological truth (justification by faith alone), providing a clear and direct path to salvation and a basis for new ecclesiastical structures.
% TRANSFER_FUNCTION: Transfers spiritual authority from institutional intermediaries and sacramental systems to individual faith and the direct interpretation of Scripture; transfers legitimacy from the Catholic hierarchy to the new Protestant churches.
% ABSENT_VOICES: Those who would argue for the necessity of works or sacraments for salvation, or the sole interpretive authority of the Pope, are excluded from the new theological framework's discourse, their views deemed erroneous.
% DISAPPEARANCE_RATIONALE: If the doctrine of justification by faith alone vanished, the entire theological and institutional structure of Protestantism would collapse, and the historical narrative of the Reformation as a 'climb' (theological breakthrough) would be fundamentally altered, requiring a complete re-evaluation of its origins and impact.
% FOUNDING_PROBLEM: The perceived corruption and theological error of the late medieval Catholic Church, particularly regarding the means of salvation (works-righteousness, indulgences) and the locus of spiritual authority.
% FOUNDING_PROBLEM_CORROBORATION: Protestant theologians and historians, as well as independent scholars of religious thought who acknowledge the genuine theological innovations of the period, corroborate that the theological problems addressed by Luther remain central to Protestant identity and continue to be debated in interfaith dialogue.
narrative_ontology:disappearance_verdict(reformation_event_boundary__theological_climb_reading, world_rearranges).
narrative_ontology:founding_problem_status(reformation_event_boundary__theological_climb_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(reformation_event_boundary__theological_climb_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(reformation_event_boundary__theological_climb_reading, 'none', 1).
narrative_ontology:epsilon_provenance(reformation_event_boundary__theological_climb_reading, 0.1, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(reformation_event_boundary__theological_climb_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(reformation_event_boundary__theological_climb_reading, ExtMetricName, E),
    domain_priors:suppression_score(reformation_event_boundary__theological_climb_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(reformation_event_boundary__theological_climb_reading),
    narrative_ontology:constraint_metric(reformation_event_boundary__theological_climb_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(reformation_event_boundary__theological_climb_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(reformation_event_boundary__theological_climb_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The metrics reflect the internal logic of the 'theological climb' reading: the rediscovered truth of justification by faith alone is inherently non-extractive (0.1) and non-suppressive (0.1) for those who accept it, as it liberates from false doctrine. Its 'naturalness' as divine truth leads to high accessibility collapse (0.9) for alternatives and low resistance (0.1) to the truth itself. The low theater ratio (0.05) reflects the genuine nature of the theological claim. The presence of beneficiaries (believers, reformers) and victims (Catholic hierarchy whose authority is undermined) on a claimed Mountain triggers the False Summit Mechanism, which is appropriate for a claim of 'divine truth' that also benefits specific human actors.
 *
 * PERSPECTIVAL GAP:
 *   The core perspectival gap lies between this reading, which sees a genuine theological 'climb' and liberation, and other readings that emphasize political opportunism or institutional collapse. From the perspective of the Catholic Church hierarchy, the same events would be experienced as an attack on legitimate authority and a destructive schism, not a 'climb'. The engine's FSM detection will highlight this divergence from the claimed 'Mountain' status.
 *
 * DIRECTIONALITY LOGIC:
 *   From this reading's perspective, 'believers_in_justification_by_faith' and 'protestant_reformers' are clear beneficiaries, liberated by the truth. The 'catholic_church_hierarchy' and 'theologians_of_the_old_guard' are victims, as their theological and institutional foundations are challenged and undermined by this 'breakthrough'. Secular rulers are initially observers but become secondary beneficiaries by leveraging the theological schism for political gain. The theological truth itself is the constraint, and its 'discovery' reconfigures power and benefit.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading inherently resists mandatrophy by asserting the timeless and divine nature of the rediscovered truth. The 'mandate' is the eternal truth of God's word, which cannot atrophy. Any perceived 'atrophy' would be attributed to human error or corruption, not the truth itself. The FSM detection, however, challenges this by noting the beneficiaries and victims, suggesting a potential 'false summit' where a claimed natural law serves specific interests.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    theological_truth_vs_social_construct,
    'Is ''justification by faith alone'' a genuine, divinely revealed theological truth (a Mountain), or a powerful social/ideological construct that benefited specific actors (a Snare/Tangled Rope)?',
    'Philosophical and theological analysis of epistemic claims, historical sociology of knowledge, and comparative study of religious movements. No definitive empirical resolution is possible for a theological claim, but its social function can be analyzed.',
    'If primarily a social construct, the constraint would reclassify from Mountain to a more extractive type (e.g., Tangled Rope or Snare), reflecting the benefits accrued by reformers and the costs imposed on the old order.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(theological_truth_vs_social_construct, conceptual, 'Ambiguity between theological truth and social construction.').

omega_variable(
    reading_contest_political_swap,
    'How much did the ''theological breakthrough'' genuinely drive institutional separation, versus serving as a rationalization for secular rulers to seize church assets (as argued by the political_swap_reading)?',
    'Detailed historical analysis of primary sources, focusing on the motivations of secular rulers and the timing of their interventions relative to theological developments. Counterfactual history exploring scenarios without Luther''s theological claims.',
    'If the political motivations were primary, this reading''s claim of theological primacy would be undermined, potentially shifting the overall classification of the Reformation event towards a more extractive, politically driven type.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_contest_political_swap, empirical, 'Contest between theological and political primacy in the Reformation.').

omega_variable(
    reading_contest_composite_overdetermination,
    'To what extent can the Reformation be reduced to a primary theological cause, given the simultaneous and irreducible influence of political, economic, and social factors (as argued by the composite_overdetermination_reading)?',
    'Interdisciplinary historical scholarship integrating theological, political, economic, and social history. Methodological debate on causal reductionism versus emergent complexity in historical events.',
    'If the composite view is adopted, this reading''s emphasis on a single primary cause would be seen as an oversimplification, leading to a more nuanced, multi-constraint analysis of the Reformation event.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_contest_composite_overdetermination, conceptual, 'Contest between single-cause and overdetermined composite views of the Reformation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(reformation_event_boundary__theological_climb_reading, 1517, 1555).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(refo_tr_t1517, reformation_event_boundary__theological_climb_reading, theater_ratio, 1517, 0.05).
narrative_ontology:measurement(refo_tr_t1525, reformation_event_boundary__theological_climb_reading, theater_ratio, 1525, 0.05).
narrative_ontology:measurement(refo_tr_t1535, reformation_event_boundary__theological_climb_reading, theater_ratio, 1535, 0.05).
narrative_ontology:measurement(refo_tr_t1545, reformation_event_boundary__theological_climb_reading, theater_ratio, 1545, 0.05).
narrative_ontology:measurement(refo_tr_t1555, reformation_event_boundary__theological_climb_reading, theater_ratio, 1555, 0.05).

% Extraction over time
narrative_ontology:measurement(refo_be_t1517, reformation_event_boundary__theological_climb_reading, base_extractiveness, 1517, 0.1).
narrative_ontology:measurement(refo_be_t1525, reformation_event_boundary__theological_climb_reading, base_extractiveness, 1525, 0.1).
narrative_ontology:measurement(refo_be_t1535, reformation_event_boundary__theological_climb_reading, base_extractiveness, 1535, 0.1).
narrative_ontology:measurement(refo_be_t1545, reformation_event_boundary__theological_climb_reading, base_extractiveness, 1545, 0.1).
narrative_ontology:measurement(refo_be_t1555, reformation_event_boundary__theological_climb_reading, base_extractiveness, 1555, 0.1).

% Suppression requirement over time
narrative_ontology:measurement(refo_su_t1517, reformation_event_boundary__theological_climb_reading, suppression_requirement, 1517, 0.1).
narrative_ontology:measurement(refo_su_t1525, reformation_event_boundary__theological_climb_reading, suppression_requirement, 1525, 0.1).
narrative_ontology:measurement(refo_su_t1535, reformation_event_boundary__theological_climb_reading, suppression_requirement, 1535, 0.1).
narrative_ontology:measurement(refo_su_t1545, reformation_event_boundary__theological_climb_reading, suppression_requirement, 1545, 0.1).
narrative_ontology:measurement(refo_su_t1555, reformation_event_boundary__theological_climb_reading, suppression_requirement, 1555, 0.1).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
