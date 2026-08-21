% ============================================================================
% CONSTRAINT STORY: reformation_composite__technological_mediation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_reformation_composite__technological_mediation_reading, []).

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
 *   constraint_id: reformation_composite__technological_mediation_reading
 *   human_readable: Reformation as Technologically Mediated Mass Movement (Technological Mediation Reading)
 *   domain: historical_epistemology/religious_history/political_economy
 *
 * SUMMARY:
 *   This constraint models the Reformation as a fundamentally technological
 *   event, where the printing press acts as a 'mountain' — an unchangeable
 *   physical reality that transforms local theological dissent into a
 *   continental mass movement. The core claim is that the technology itself,
 *   by enabling rapid and widespread information diffusion, was the primary
 *   driver, making the scale and speed of the Reformation inevitable once the
 *   technology was adopted. This reading emphasizes publication rates,
 *   literacy, and the physical constraints of information flow as key
 *   observables.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(reformation_composite__technological_mediation_reading, 0.05).
domain_priors:suppression_score(reformation_composite__technological_mediation_reading, 0.1).
domain_priors:theater_ratio(reformation_composite__technological_mediation_reading, 0.0).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(reformation_composite__technological_mediation_reading, extractiveness, 0.05).
narrative_ontology:constraint_metric(reformation_composite__technological_mediation_reading, suppression_requirement, 0.1).
narrative_ontology:constraint_metric(reformation_composite__technological_mediation_reading, theater_ratio, 0.0).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(reformation_composite__technological_mediation_reading, accessibility_collapse, 0.9).
narrative_ontology:constraint_metric(reformation_composite__technological_mediation_reading, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(reformation_composite__technological_mediation_reading, mountain).
narrative_ontology:human_readable(reformation_composite__technological_mediation_reading, "Reformation as Technologically Mediated Mass Movement (Technological Mediation Reading)").
narrative_ontology:topic_domain(reformation_composite__technological_mediation_reading, "historical_epistemology/religious_history/political_economy").

domain_priors:emerges_naturally(reformation_composite__technological_mediation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(reformation_composite__technological_mediation_reading, '16247bc9-cc64-4448-ba83-b02b631369c5').
narrative_ontology:cs_kernel_codification('16247bc9-cc64-4448-ba83-b02b631369c5', implicit).
narrative_ontology:cs_authority_grounding('16247bc9-cc64-4448-ba83-b02b631369c5', diffuse_epistemic).
narrative_ontology:cs_reading_relation('16247bc9-cc64-4448-ba83-b02b631369c5', reformation_composite__theological_fragmentation_reading, influences).
narrative_ontology:cs_reading_relation('16247bc9-cc64-4448-ba83-b02b631369c5', reformation_composite__political_realignment_reading, influences).
narrative_ontology:cs_axiom('16247bc9-cc64-4448-ba83-b02b631369c5', foundational, information_diffusion_drives_social_change).
narrative_ontology:cs_axiom_status(information_diffusion_drives_social_change, holdable).
narrative_ontology:cs_axiom_grounding('16247bc9-cc64-4448-ba83-b02b631369c5', information_diffusion_drives_social_change, empirically_contingent).
narrative_ontology:cs_axiom('16247bc9-cc64-4448-ba83-b02b631369c5', foundational, technological_affordances_shape_historical_outcomes).
narrative_ontology:cs_axiom_status(technological_affordances_shape_historical_outcomes, holdable).
narrative_ontology:cs_axiom_grounding('16247bc9-cc64-4448-ba83-b02b631369c5', technological_affordances_shape_historical_outcomes, empirically_contingent).
narrative_ontology:cs_reference_frame('16247bc9-cc64-4448-ba83-b02b631369c5', pre_print_information_regime).
narrative_ontology:cs_drift_state('16247bc9-cc64-4448-ba83-b02b631369c5', post_gutenberg_era, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('16247bc9-cc64-4448-ba83-b02b631369c5', '').
narrative_ontology:cs_kernel_id(reformation_composite__technological_mediation_reading, reformation_composite).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(reformation_composite__technological_mediation_reading, protestant_reformers).
narrative_ontology:constraint_beneficiary(reformation_composite__technological_mediation_reading, literate_populace).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(reformation_composite__technological_mediation_reading, emerging_nation_states).
narrative_ontology:constraint_victim(reformation_composite__technological_mediation_reading, catholic_church_hierarchy).
narrative_ontology:constraint_vindicates(reformation_composite__technological_mediation_reading, technological_determinism_hypothesis).
narrative_ontology:constraint_vindicates(reformation_composite__technological_mediation_reading, information_diffusion_theory).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The physical technology itself, enabling rapid, cheap, and widespread dissemination of texts. It sets the 'agenda' by making mass communication possible, fundamentally altering the information landscape.
narrative_ontology:constraint_stakeholder(reformation_composite__technological_mediation_reading, printing_press_technology, agenda_setter,
    institutional, generational, analytical, continental).
narrative_ontology:stakeholder_non_agent(reformation_composite__technological_mediation_reading, printing_press_technology).

% Benefited immensely from the printing press, which allowed their theological arguments and critiques to reach a vast audience quickly, bypassing traditional gatekeepers and fostering a sense of shared movement across disparate regions.
narrative_ontology:constraint_stakeholder(reformation_composite__technological_mediation_reading, protestant_reformers, beneficiary,
    organized, biographical, mobile, continental).

% Paid the cost of losing control over information dissemination. The printing press undermined their monopoly on scripture interpretation and theological discourse, leading to widespread dissent and fragmentation of their authority.
narrative_ontology:constraint_stakeholder(reformation_composite__technological_mediation_reading, catholic_church_hierarchy, payer,
    institutional, generational, constrained, continental).

% Benefited from unprecedented access to religious texts, pamphlets, and theological debates, fostering individual interpretation and engagement with religious ideas, leading to increased literacy and intellectual ferment.
narrative_ontology:constraint_stakeholder(reformation_composite__technological_mediation_reading, literate_populace, beneficiary,
    moderate, biographical, mobile, local).

% Benefited indirectly by the printing press enabling the spread of ideas that challenged papal authority, providing a tool for consolidating their own power and asserting religious autonomy within their territories.
narrative_ontology:constraint_stakeholder(reformation_composite__technological_mediation_reading, emerging_nation_states, beneficiary,
    institutional, generational, arbitrage, continental).

% Analyze the causal role of technology in historical transformations, using the Reformation as a case study for information diffusion and its societal impacts. Their 'exit' is to adopt alternative explanatory frameworks.
narrative_ontology:constraint_stakeholder(reformation_composite__technological_mediation_reading, historical_epistemologists, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The printing press coordinated the rapid, widespread, and standardized dissemination of theological and political texts, enabling a shared intellectual and religious discourse across geographical boundaries that was previously impossible.
% TRANSFER_FUNCTION: Transferred information, ideas, and arguments from individual authors to a mass audience, bypassing traditional gatekeepers and accelerating the pace of intellectual and religious change.
% ABSENT_VOICES: Illiterate populations, whose access to the new information was still mediated, and those who lacked the means to print their own counter-arguments, were effectively excluded from direct participation in the new information economy.
% DISAPPEARANCE_RATIONALE: If the printing press had not emerged, the Reformation would have remained a series of localized theological disputes, lacking the technological infrastructure to become a continental mass movement. The entire course of European religious, political, and intellectual history would have been fundamentally different.
% FOUNDING_PROBLEM: The problem of slow, expensive, and limited information dissemination, which constrained the spread of new ideas and maintained centralized control over knowledge.
% FOUNDING_PROBLEM_CORROBORATION: Historians of technology and communication universally corroborate that the problem of information dissemination was fundamentally transformed by the printing press, rendering the pre-print problem 'dead' in its original form. The Catholic Church hierarchy, however, might contest this, arguing that the 'problem' was not dissemination but heresy, which the press exacerbated.
narrative_ontology:disappearance_verdict(reformation_composite__technological_mediation_reading, world_rearranges).
narrative_ontology:founding_problem_status(reformation_composite__technological_mediation_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(reformation_composite__technological_mediation_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(reformation_composite__technological_mediation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(reformation_composite__technological_mediation_reading, 0.05, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(reformation_composite__technological_mediation_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(reformation_composite__technological_mediation_reading, ExtMetricName, E),
    domain_priors:suppression_score(reformation_composite__technological_mediation_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(reformation_composite__technological_mediation_reading),
    narrative_ontology:constraint_metric(reformation_composite__technological_mediation_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(reformation_composite__technological_mediation_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(reformation_composite__technological_mediation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The printing press, as a physical technology, has negligible extractiveness (0.05) and suppression (0.1) in itself; it simply enables new forms of communication. Its 'mountain' classification reflects its unchangeable nature and profound impact on the information landscape. The slight increase in extractiveness and suppression over time reflects the increasing efforts by authorities (like the Catholic Church) to control or suppress the output of the press, which ultimately proved futile against the underlying technological reality. Theater ratio is zero as the press is a functional technology, not performative.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the Protestant reformers, the printing press was a providential tool for spreading truth, a pure enabler. From the perspective of the Catholic Church, it was a destructive force that facilitated heresy and fragmentation. This reading, however, frames the press as a neutral, 'mountain-like' force whose structural properties (speed, volume, cost) dictated the historical outcome, independent of the intentions or desires of the human actors.
 *
 * DIRECTIONALITY LOGIC:
 *   The printing press itself is a neutral technology, but its operation creates beneficiaries (Protestant reformers, literate populace, emerging nation-states) and a 'payer' (Catholic Church hierarchy) who bears the cost of its disruptive effects. The technology subsidizes the reformers by amplifying their message, while it extracts from the Church by undermining its information monopoly. The 'agenda-setter' is the technology itself, as it dictates the new possibilities for information flow.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    technological_determinism_vs_agency,
    'To what extent was the Reformation''s trajectory truly determined by the printing press, versus being shaped by human agency (theological choices, political decisions)?',
    'Comparative historical analysis of other regions/periods with similar technological shifts but different social/political outcomes, or counterfactual history exploring alternative choices by key actors.',
    'If human agency is found to be more decisive, the ''mountain'' classification of the printing press''s influence might be re-evaluated as a ''rope'' or ''tangled_rope'' that enabled, but did not determine, the outcome. If technological determinism holds, the mountain classification is reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(technological_determinism_vs_agency, conceptual, 'Ambiguity between technological determinism and human agency in historical causation.').

omega_variable(
    natural_law_vs_constructed_impact,
    'Is the printing press''s impact on the Reformation a ''natural law'' consequence of its physical properties, or a ''constructed'' outcome mediated by social and political choices about its use?',
    'Analysis of how different societies adopted and regulated printing technology; if its effects varied widely based on social context, its ''naturalness'' as a mountain is challenged.',
    'If the impact is primarily constructed, the constraint might be reclassified as a ''rope'' (if beneficial coordination) or ''tangled_rope'' (if also extractive), reflecting the human choices embedded in its operation and regulation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_impact, empirical, 'Whether the printing press''s historical impact is a natural consequence or socially constructed.').

omega_variable(
    reading_identity_and_structural_delta,
    'This constraint is one reading of the ''reformation_composite'' kernel. What specific structural elements would change if a sibling reading (e.g., ''theological_fragmentation_reading'' or ''political_realignment_reading'') were adopted as the primary frame?',
    'Explicit comparison of observable metrics (e.g., for ''theological_fragmentation_reading'', the primary observables would be doctrinal disputes and confessional boundaries; for ''political_realignment_reading'', it would be state-building and diplomatic shifts).',
    'Adopting a sibling reading would shift the primary causal mechanism, leading to different beneficiaries/victims, different core metrics (e.g., extractiveness of theological conformity, suppression of political dissent), and thus a different constraint classification for the ''Reformation'' itself.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_identity_and_structural_delta, conceptual, 'Impact of alternative kernel readings on constraint structure and classification.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(reformation_composite__technological_mediation_reading, 1450, 1650).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(refo_tr_t1450, reformation_composite__technological_mediation_reading, theater_ratio, 1450, 0.0).
narrative_ontology:measurement(refo_tr_t1500, reformation_composite__technological_mediation_reading, theater_ratio, 1500, 0.0).
narrative_ontology:measurement(refo_tr_t1550, reformation_composite__technological_mediation_reading, theater_ratio, 1550, 0.0).
narrative_ontology:measurement(refo_tr_t1600, reformation_composite__technological_mediation_reading, theater_ratio, 1600, 0.0).
narrative_ontology:measurement(refo_tr_t1650, reformation_composite__technological_mediation_reading, theater_ratio, 1650, 0.0).

% Extraction over time
narrative_ontology:measurement(refo_be_t1450, reformation_composite__technological_mediation_reading, base_extractiveness, 1450, 0.01).
narrative_ontology:measurement(refo_be_t1500, reformation_composite__technological_mediation_reading, base_extractiveness, 1500, 0.02).
narrative_ontology:measurement(refo_be_t1550, reformation_composite__technological_mediation_reading, base_extractiveness, 1550, 0.03).
narrative_ontology:measurement(refo_be_t1600, reformation_composite__technological_mediation_reading, base_extractiveness, 1600, 0.04).
narrative_ontology:measurement(refo_be_t1650, reformation_composite__technological_mediation_reading, base_extractiveness, 1650, 0.05).

% Suppression requirement over time
narrative_ontology:measurement(refo_su_t1450, reformation_composite__technological_mediation_reading, suppression_requirement, 1450, 0.05).
narrative_ontology:measurement(refo_su_t1500, reformation_composite__technological_mediation_reading, suppression_requirement, 1500, 0.07).
narrative_ontology:measurement(refo_su_t1550, reformation_composite__technological_mediation_reading, suppression_requirement, 1550, 0.08).
narrative_ontology:measurement(refo_su_t1600, reformation_composite__technological_mediation_reading, suppression_requirement, 1600, 0.09).
narrative_ontology:measurement(refo_su_t1650, reformation_composite__technological_mediation_reading, suppression_requirement, 1650, 0.1).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(reformation_composite__technological_mediation_reading, information_standard).
narrative_ontology:affects_constraint(reformation_composite__technological_mediation_reading, reformation_composite__theological_fragmentation_reading).
narrative_ontology:affects_constraint(reformation_composite__technological_mediation_reading, reformation_composite__political_realignment_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'reformation_composite' kernel. This 'technological_mediation_reading' emphasizes the printing press as the primary driver, influencing the theological and political outcomes described in the sibling readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
