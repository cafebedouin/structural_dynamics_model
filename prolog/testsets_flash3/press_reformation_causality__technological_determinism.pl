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
    narrative_ontology:constraint_vindicates/2,
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
 *   This constraint story models the 'technological determinism' reading of
 *   the printing press's role in the Reformation. In this view, the printing
 *   press is an autonomous, enabling technology whose inherent capabilities
 *   (mass production, rapid dissemination of vernacular scripture) made the
 *   success of the Reformation inevitable, regardless of human agency or
 *   strategic choices. It is classified as a Mountain due to its perceived
 *   unchangeable, fixed nature as a technological force. Human actors are
 *   seen as downstream responders to this technological imperative.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(press_reformation_causality__technological_determinism, 0.05).
domain_priors:suppression_score(press_reformation_causality__technological_determinism, 0.02).
domain_priors:theater_ratio(press_reformation_causality__technological_determinism, 0.01).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(press_reformation_causality__technological_determinism, extractiveness, 0.05).
narrative_ontology:constraint_metric(press_reformation_causality__technological_determinism, suppression_requirement, 0.02).
narrative_ontology:constraint_metric(press_reformation_causality__technological_determinism, theater_ratio, 0.01).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(press_reformation_causality__technological_determinism, accessibility_collapse, 0.9).
narrative_ontology:constraint_metric(press_reformation_causality__technological_determinism, resistance, 0.01).

% --- Constraint claim ---
narrative_ontology:constraint_claim(press_reformation_causality__technological_determinism, mountain).
narrative_ontology:human_readable(press_reformation_causality__technological_determinism, "Printing Press as Deterministic Force in Reformation").
narrative_ontology:topic_domain(press_reformation_causality__technological_determinism, "history_of_technology/religious_history/media_studies").

domain_priors:emerges_naturally(press_reformation_causality__technological_determinism).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(press_reformation_causality__technological_determinism, '2ddf8ae2-952b-4d5b-9e4d-58a1983530ce').
narrative_ontology:cs_kernel_codification('2ddf8ae2-952b-4d5b-9e4d-58a1983530ce', implicit).
narrative_ontology:cs_authority_grounding('2ddf8ae2-952b-4d5b-9e4d-58a1983530ce', diffuse_epistemic).
narrative_ontology:cs_reading_relation('2ddf8ae2-952b-4d5b-9e4d-58a1983530ce', press_reformation_causality__strategic_deployment, forecloses).
narrative_ontology:cs_reading_relation('2ddf8ae2-952b-4d5b-9e4d-58a1983530ce', press_reformation_causality__co_constitution, forecloses).
narrative_ontology:cs_axiom('2ddf8ae2-952b-4d5b-9e4d-58a1983530ce', foundational, technology_as_primary_driver).
narrative_ontology:cs_axiom_status(technology_as_primary_driver, holdable).
narrative_ontology:cs_axiom_grounding('2ddf8ae2-952b-4d5b-9e4d-58a1983530ce', technology_as_primary_driver, empirically_contingent).
narrative_ontology:cs_axiom('2ddf8ae2-952b-4d5b-9e4d-58a1983530ce', foundational, historical_outcomes_are_inevitable).
narrative_ontology:cs_axiom_status(historical_outcomes_are_inevitable, holdable).
narrative_ontology:cs_axiom_grounding('2ddf8ae2-952b-4d5b-9e4d-58a1983530ce', historical_outcomes_are_inevitable, empirically_contingent).
narrative_ontology:cs_reference_frame('2ddf8ae2-952b-4d5b-9e4d-58a1983530ce', autonomous_technological_unfolding).
narrative_ontology:cs_drift_state('2ddf8ae2-952b-4d5b-9e4d-58a1983530ce', contemporary_historiography, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('2ddf8ae2-952b-4d5b-9e4d-58a1983530ce', '').
narrative_ontology:cs_kernel_id(press_reformation_causality__technological_determinism, press_reformation_causality).

% --- Structural relationships ---
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(press_reformation_causality__technological_determinism, vernacular_scripture_readers).
narrative_ontology:constraint_victim(press_reformation_causality__technological_determinism, catholic_church).
narrative_ontology:constraint_vindicates(press_reformation_causality__technological_determinism, technological_determinism_theory).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The printing press itself, viewed as an autonomous force whose inherent capabilities (mass production, rapid dissemination) dictated historical outcomes, particularly the spread of vernacular scripture and the inevitability of the Reformation.
narrative_ontology:constraint_stakeholder(press_reformation_causality__technological_determinism, printing_press_technology, agenda_setter,
    institutional, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(press_reformation_causality__technological_determinism, printing_press_technology).

% Individuals who gained direct access to religious texts in their native languages, enabling personal interpretation and fostering religious dissent, a direct consequence of the press's inherent capabilities.
narrative_ontology:constraint_stakeholder(press_reformation_causality__technological_determinism, vernacular_scripture_readers, beneficiary,
    powerless, biographical, constrained, regional).

% The historical movement itself, whose success is attributed directly and primarily to the printing press's ability to disseminate its ideas and challenge established religious authority, making its triumph inevitable.
narrative_ontology:constraint_stakeholder(press_reformation_causality__technological_determinism, reformation_movement, beneficiary,
    institutional, generational, analytical, continental).
narrative_ontology:stakeholder_non_agent(press_reformation_causality__technological_determinism, reformation_movement).

% The established religious authority whose control over information and interpretation was fundamentally undermined by the printing press, leading to an inevitable loss of power and influence as vernacular scripture spread uncontrollably.
narrative_ontology:constraint_stakeholder(press_reformation_causality__technological_determinism, catholic_church, payer,
    institutional, civilizational, constrained, global).

% Historians and scholars who analyze the period and interpret the role of technology, often adopting a deterministic view that emphasizes the press's inherent power to shape events.
narrative_ontology:constraint_stakeholder(press_reformation_causality__technological_determinism, historical_observers, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The printing press inherently coordinated the rapid, widespread dissemination of information, enabling a shared textual basis for religious and intellectual movements across diverse populations without requiring active human coordination beyond its initial invention.
% TRANSFER_FUNCTION: Transferred the means of knowledge production and dissemination from centralized, elite institutions (like the Church and monastic scriptoria) to a more decentralized, accessible, and ultimately uncontrollable network of printers and readers.
% ABSENT_VOICES: Scholars emphasizing human agency, strategic choices, and socio-political contexts would object, arguing that the press was a tool whose impact depended on how it was used, not an autonomous force. Their voices are often marginalized in purely deterministic accounts.
% DISAPPEARANCE_RATIONALE: If the printing press had never been invented, the spread of vernacular scripture would have been severely limited, and the Reformation, if it occurred at all, would have taken a vastly different, likely much slower and less widespread, trajectory. The entire intellectual and religious landscape of early modern Europe would be fundamentally altered.
% FOUNDING_PROBLEM: The problem of slow, expensive, and error-prone manual reproduction of texts, which limited access to knowledge and centralized control over information.
% FOUNDING_PROBLEM_CORROBORATION: The problem of slow manual reproduction is universally acknowledged as solved by the printing press. However, the deterministic claim that the press *alone* made the Reformation inevitable is contested by most contemporary historians, who emphasize a confluence of factors. No corroboration from outside the technological determinism framework exists for the inevitability claim.
narrative_ontology:disappearance_verdict(press_reformation_causality__technological_determinism, world_rearranges).
narrative_ontology:founding_problem_status(press_reformation_causality__technological_determinism, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(press_reformation_causality__technological_determinism, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
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
 *   The metrics reflect the deterministic view: extractiveness, suppression, and theater ratio are near zero because the press is seen as a neutral, natural force, not an extractive or coercive mechanism. Accessibility collapse is high (0.9) because the press fundamentally altered the landscape of information access, making alternatives to print-based dissemination largely irrelevant. Resistance is low (0.01) because, from this perspective, the technological force was too powerful to be effectively resisted. The claimed type is 'mountain' because the core premise is that the technology's impact was an irreducible, natural-law-like limit on historical outcomes.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the printing press itself (as an 'agent' in this deterministic reading), its operation is a natural unfolding of its capabilities, with no 'extraction' or 'suppression' in a human sense. From the perspective of the Catholic Church, it was a force that extracted control and imposed costs, but this reading frames that as an inevitable consequence of the technology, not a human-imposed constraint. The deterministic reading obscures the beneficiary structure by framing the outcomes as natural rather than chosen.
 *
 * DIRECTIONALITY LOGIC:
 *   The 'printing_press_technology' is framed as an 'agenda_setter' (though non-agent) because its inherent properties dictate the course of events. 'Vernacular_scripture_readers' and the 'reformation_movement' are beneficiaries, as they directly gain from the press's capabilities. The 'catholic_church' is a payer, bearing the costs of its disrupted authority. However, the deterministic framing minimizes the 'agency' of these actors, making their directionality a response to the technology's inherent force.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification prevents mislabeling the press's impact as a human-constructed 'snare' or 'tangled_rope' by emphasizing the deterministic claim. If the deterministic claim were true, the constraint would indeed be a Mountain. The analysis highlights that the 'mandate' (the inherent technological imperative) is seen as perpetually 'live' within this reading, making questions of mandatrophy irrelevant, as the technology's function is seen as immutable and self-justifying.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    technological_autonomy_vs_agency,
    'Is the printing press truly an autonomous force, or was its impact mediated and shaped by human agency, strategic choices, and socio-political contexts?',
    'Comparative historical analysis of print cultures in different regions and periods, examining variations in impact despite similar technological availability, or counterfactual historical modeling.',
    'If human agency is found to be primary, the constraint would reclassify from Mountain to a more constructed type (e.g., Rope or Tangled Rope), with higher extractiveness and suppression reflecting the choices made by actors.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(technological_autonomy_vs_agency, conceptual, 'Ambiguity regarding the degree of technological autonomy versus human agency in shaping historical outcomes.').

omega_variable(
    beneficiary_obscurity_vs_capture,
    'Does the deterministic framing genuinely reflect a lack of concentrated beneficiaries, or does it obscure how specific actors (e.g., early printers, certain reformers) strategically captured and leveraged the press''s capabilities for their own gain?',
    'Detailed economic and social history of early print capitalism and Reformation-era patronage, identifying specific individuals or groups who disproportionately profited or gained power from print dissemination.',
    'If concentrated beneficiaries are identified, the constraint would shift from a pure Mountain (with no beneficiaries) to a False Summit Mountain or even a Snare/Tangled Rope, as the ''natural'' outcome would be revealed as serving specific interests.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_obscurity_vs_capture, empirical, 'Uncertainty about whether the absence of declared beneficiaries is accurate or a consequence of the deterministic framing.').

omega_variable(
    natural_law_vs_historical_contingency,
    'Is the ''inevitability'' of the Reformation due to the printing press a natural law-like outcome, or a contingent historical development that could have unfolded differently?',
    'Analysis of historical counterfactuals and the role of non-technological factors (e.g., political fragmentation, theological disputes, individual leadership) in the Reformation''s success.',
    'If the outcome is found to be contingent, the ''emerges_naturally'' property would be false, and the constraint would reclassify away from Mountain, likely to a constructed type like Rope or Tangled Rope, reflecting human choices and their consequences.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(natural_law_vs_historical_contingency, conceptual, 'Ambiguity between a deterministic, natural-law interpretation and a contingent, historically specific one.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(press_reformation_causality__technological_determinism, 1450, 1650).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pres_tr_t1450, press_reformation_causality__technological_determinism, theater_ratio, 1450, 0.01).
narrative_ontology:measurement(pres_tr_t1490, press_reformation_causality__technological_determinism, theater_ratio, 1490, 0.01).
narrative_ontology:measurement(pres_tr_t1530, press_reformation_causality__technological_determinism, theater_ratio, 1530, 0.01).
narrative_ontology:measurement(pres_tr_t1570, press_reformation_causality__technological_determinism, theater_ratio, 1570, 0.01).
narrative_ontology:measurement(pres_tr_t1610, press_reformation_causality__technological_determinism, theater_ratio, 1610, 0.01).
narrative_ontology:measurement(pres_tr_t1650, press_reformation_causality__technological_determinism, theater_ratio, 1650, 0.01).

% Extraction over time
narrative_ontology:measurement(pres_be_t1450, press_reformation_causality__technological_determinism, base_extractiveness, 1450, 0.05).
narrative_ontology:measurement(pres_be_t1490, press_reformation_causality__technological_determinism, base_extractiveness, 1490, 0.05).
narrative_ontology:measurement(pres_be_t1530, press_reformation_causality__technological_determinism, base_extractiveness, 1530, 0.05).
narrative_ontology:measurement(pres_be_t1570, press_reformation_causality__technological_determinism, base_extractiveness, 1570, 0.05).
narrative_ontology:measurement(pres_be_t1610, press_reformation_causality__technological_determinism, base_extractiveness, 1610, 0.05).
narrative_ontology:measurement(pres_be_t1650, press_reformation_causality__technological_determinism, base_extractiveness, 1650, 0.05).

% Suppression requirement over time
narrative_ontology:measurement(pres_su_t1450, press_reformation_causality__technological_determinism, suppression_requirement, 1450, 0.02).
narrative_ontology:measurement(pres_su_t1490, press_reformation_causality__technological_determinism, suppression_requirement, 1490, 0.02).
narrative_ontology:measurement(pres_su_t1530, press_reformation_causality__technological_determinism, suppression_requirement, 1530, 0.02).
narrative_ontology:measurement(pres_su_t1570, press_reformation_causality__technological_determinism, suppression_requirement, 1570, 0.02).
narrative_ontology:measurement(pres_su_t1610, press_reformation_causality__technological_determinism, suppression_requirement, 1610, 0.02).
narrative_ontology:measurement(pres_su_t1650, press_reformation_causality__technological_determinism, suppression_requirement, 1650, 0.02).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
