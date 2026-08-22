% ============================================================================
% CONSTRAINT STORY: press_reformation_causation__technological_determinism
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
    narrative_ontology:constraint_vindicates/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
 *   constraint_id: press_reformation_causation__technological_determinism
 *   human_readable: Technological Determinism: Printing Press as Irreversible Cause of the Reformation
 *   domain: history/religion/technology
 *
 * SUMMARY:
 *   The technological determinism reading of the press-Reformation kernel
 *   treats Gutenberg's invention as an irresistible natural force that made
 *   ecclesiastical censorship impossible and vernacular scripture inevitable.
 *   In this reading, the printing press is not a tool strategically wielded
 *   by reformers, nor a medium shaped by reciprocal social forces, but an
 *   upstream mountain of material capacity that determined religious and
 *   political outcomes. The Catholic Church's institutional monopoly on
 *   interpretation is the primary target, while reformers, printers, and lay
 *   readers are downstream beneficiaries of an exogenous technical shock. The
 *   constraint is authored as a mountain because this reading claims
 *   natural-law status for the press's impact, but the presence of
 *   identifiable beneficiaries triggers FSM evaluation.
 *
 * KEY AGENTS:
 *   - Protestant reformers: Downstream beneficiaries (organized/mobile) â exploit the press's exogenous capacity
 *   - Catholic Church: Primary target (institutional/constrained) â loses informational monopoly, resistance futile
 *   - Lay readers: Secondary beneficiaries (moderate/constrained) â gain vernacular access but cannot opt out
 *   - Printers and publishers: Economic beneficiaries (moderate/arbitrage) â profit from demand, mobile across jurisdictions
 *   - Secular rulers: Incidental beneficiaries (powerful/mobile) â gain leverage from Church weakness
 *   - Historian observer: Analytical seat â assesses causal weight of technology
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(press_reformation_causation__technological_determinism, 0.68).
domain_priors:suppression_score(press_reformation_causation__technological_determinism, 0.25).
domain_priors:theater_ratio(press_reformation_causation__technological_determinism, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(press_reformation_causation__technological_determinism, extractiveness, 0.68).
narrative_ontology:constraint_metric(press_reformation_causation__technological_determinism, suppression_requirement, 0.25).
narrative_ontology:constraint_metric(press_reformation_causation__technological_determinism, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(press_reformation_causation__technological_determinism, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(press_reformation_causation__technological_determinism, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(press_reformation_causation__technological_determinism, mountain).
narrative_ontology:human_readable(press_reformation_causation__technological_determinism, "Technological Determinism: Printing Press as Irreversible Cause of the Reformation").
narrative_ontology:topic_domain(press_reformation_causation__technological_determinism, "history/religion/technology").

domain_priors:emerges_naturally(press_reformation_causation__technological_determinism).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(press_reformation_causation__technological_determinism, '0618400f-3ded-4476-9a1b-8b0ab91db9a8').
narrative_ontology:cs_kernel_codification('0618400f-3ded-4476-9a1b-8b0ab91db9a8', distributed).
narrative_ontology:cs_authority_grounding('0618400f-3ded-4476-9a1b-8b0ab91db9a8', expertise).
narrative_ontology:cs_interpretation_layer_present('0618400f-3ded-4476-9a1b-8b0ab91db9a8').
narrative_ontology:cs_reading_relation('0618400f-3ded-4476-9a1b-8b0ab91db9a8', press_reformation_causation__strategic_deployment, influences).
narrative_ontology:cs_reading_relation('0618400f-3ded-4476-9a1b-8b0ab91db9a8', press_reformation_causation__mutual_shaping, influences).
narrative_ontology:cs_axiom('0618400f-3ded-4476-9a1b-8b0ab91db9a8', foundational, technology_as_autonomous_cause).
narrative_ontology:cs_axiom_status(technology_as_autonomous_cause, holdable).
narrative_ontology:cs_axiom_grounding('0618400f-3ded-4476-9a1b-8b0ab91db9a8', technology_as_autonomous_cause, empirically_contingent).
narrative_ontology:cs_axiom('0618400f-3ded-4476-9a1b-8b0ab91db9a8', foundational, human_agency_subordinate_to_media).
narrative_ontology:cs_axiom_status(human_agency_subordinate_to_media, holdable).
narrative_ontology:cs_axiom_grounding('0618400f-3ded-4476-9a1b-8b0ab91db9a8', human_agency_subordinate_to_media, empirically_contingent).
narrative_ontology:cs_reference_frame('0618400f-3ded-4476-9a1b-8b0ab91db9a8', technological_imperative).
narrative_ontology:cs_drift_state('0618400f-3ded-4476-9a1b-8b0ab91db9a8', post_revisionist_era, gap(axiom_overriding, substantial, true)).
narrative_ontology:cs_created_at('0618400f-3ded-4476-9a1b-8b0ab91db9a8', '').
narrative_ontology:cs_kernel_id(press_reformation_causation__technological_determinism, press_reformation_causation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(press_reformation_causation__technological_determinism, protestant_reformers).
narrative_ontology:constraint_beneficiary(press_reformation_causation__technological_determinism, lay_readers).
narrative_ontology:constraint_beneficiary(press_reformation_causation__technological_determinism, printers_and_publishers).
narrative_ontology:constraint_beneficiary(press_reformation_causation__technological_determinism, secular_rulers).
narrative_ontology:constraint_victim(press_reformation_causation__technological_determinism, catholic_church).
narrative_ontology:constraint_vindicates(press_reformation_causation__technological_determinism, technological_imperative_hypothesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Exploited the press's capacity for rapid, wide distribution of vernacular polemic and scripture. Their message scaled beyond what manuscript or oral transmission could achieve. They did not create the press but rode its exogenous capacity, unable to opt out of the new information ecology once deployed.
narrative_ontology:constraint_stakeholder(press_reformation_causation__technological_determinism, protestant_reformers, beneficiary,
    organized, biographical, mobile, continental).

% Held a monopoly on scriptural interpretation and sacramental mediation. The press eroded its capacity to control theological discourse across linguistic boundaries. Attempted censorship through indices, licensing, and territorial restrictions, but could not suppress the technical diffusion of movable type or the demand for vernacular text.
narrative_ontology:constraint_stakeholder(press_reformation_causation__technological_determinism, catholic_church, payer,
    institutional, generational, constrained, continental).

% Gained direct access to vernacular scripture and polemical literature outside parish and priestly mediation. Their literacy and interpretive autonomy expanded, though they could not opt out of the transformation of the information environment once the press entered their region.
narrative_ontology:constraint_stakeholder(press_reformation_causation__technological_determinism, lay_readers, beneficiary,
    moderate, biographical, constrained, regional).

% Profited from explosive demand for religious and controversial print. Moved between cities and principalities to evade local censorship, scaling output to meet demand created by confessional conflict. Their economic interest aligned with maximum circulation and minimum restriction.
narrative_ontology:constraint_stakeholder(press_reformation_causation__technological_determinism, printers_and_publishers, beneficiary,
    moderate, biographical, arbitrage, regional).

% Gained leverage over ecclesiastical authority as their territories became sites of confessional competition. Could play reform movements against papal claims to jurisdictional supremacy, extracting political autonomy from the Church's weakened informational monopoly.
narrative_ontology:constraint_stakeholder(press_reformation_causation__technological_determinism, secular_rulers, beneficiary,
    powerful, generational, mobile, continental).

% Analyzes the causal weight of printing technology in the Reformation. Some adopt the technological determinism framework; others dispute it in favor of strategic or co-evolutionary readings. Their own career and paradigm incentives may align with epochal, technology-driven narratives.
narrative_ontology:constraint_stakeholder(press_reformation_causation__technological_determinism, historian_observer, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(press_reformation_causation__technological_determinism, diffuse).
narrative_ontology:fixing_cost_class(press_reformation_causation__technological_determinism, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: None â the arrangement does not coordinate agents but subordinates human agency to an exogenous technical capacity. The press operates as a deterministic cause, not a coordinating mechanism.
% TRANSFER_FUNCTION: Moves interpretive and institutional authority from the centralized Catholic Church to decentralized reform movements, lay readerships, and secular territorial rulers; moves causal explanatory power from human agency to material technology.
% ABSENT_VOICES: Illiterate populations, oral culture practitioners, women excluded from print discourse, Eastern Orthodox communities outside the Latin print sphere, and manuscript-based religious movements that rejected print. Their absence from the 'inevitable' narrative is structural â the reading constructs progress around literacy and print.
% DISAPPEARANCE_RATIONALE: If the press's deterministic causation vanished (i.e., if technology did not determine religious outcomes), the entire explanatory framework for the Reformation's rapid diffusion would collapse. Historiography would need to relocate causation in human strategy, social structure, or contingency; the Catholic Church's authority structure would remain intact in the counterfactual; and the beneficiaries would lose their exogenous advantage.
% FOUNDING_PROBLEM: The historiographical need to explain why the Reformation succeeded in achieving mass, rapid, cross-border diffusion where earlier medieval heresies had been localized and suppressed.
% FOUNDING_PROBLEM_CORROBORATION: Social historians and cultural historians attest that explaining the Reformation's scale remains a live problem, but they contest whether technology is the solution. Book historians corroborate the importance of print volume and distribution, but from outside the technological determinism framework they attribute equal or greater weight to translator networks, patronage, and urban political climates.
narrative_ontology:disappearance_verdict(press_reformation_causation__technological_determinism, world_rearranges).
narrative_ontology:founding_problem_status(press_reformation_causation__technological_determinism, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(press_reformation_causation__technological_determinism, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(press_reformation_causation__technological_determinism, 'none', 1).
narrative_ontology:epsilon_provenance(press_reformation_causation__technological_determinism, 0.68, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(press_reformation_causation__technological_determinism_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(press_reformation_causation__technological_determinism, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

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

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.68) because the constraint strips the Church of interpretive monopoly and strips all human actors of causal agency, transferring historical efficacy to technology. Suppression is low (0.25) because the constraint persists by technical necessity, not active enforcement; the Church's enforcement attempts are futile resistance to a mountain. Theater ratio is low-moderate (0.20) because the determinism narrative is substantive historiography with some performative maintenance in textbook transmission. Accessibility collapse is very high (0.92) because once movable type and vernacular printing exist, the alternative (complete Church control of information) becomes structurally unreachable. Resistance is substantial (0.60) because the Church actively resisted through indices, licensing, and censorship â but the reading frames this resistance as futile, producing the divergence between claimed mountain and measured resistance that the engine is designed to detect. The measurement grid tracks the accumulation of extractive impact from the press's invention through the confessional wars.
 *
 * PERSPECTIVAL GAP:
 *   The Catholic Church seat experiences high effective extraction (loss of monopoly, doctrinal control, and territorial authority) and high directionality toward the target pole. Protestant reformers experience low directionality (beneficiary) despite having limited exit options â they cannot un-invent the press, but they are positioned to harvest its capacity. The historian observer seat sees the full structure: a claimed mountain that nonetheless registers concentrated extraction on one institutional party and concentrated benefit on others, which is the false-summit signature.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (reformers, lay readers, printers, secular rulers) sit at low d because the constraint subsidizes their capacity and authority. The Catholic Church sits at high d because the constraint extracts its institutional control over interpretation. Printers have arbitrage-grade exit (they can relocate to permissive jurisdictions), further damping their d. The Church is constrained (cannot exit the European information environment). Lay readers are constrained (cannot opt out of the new information ecology). Reformers are mobile (can choose whether and how to use the press, but benefit from its existence regardless).
 *
 * MANDATROPHY ANALYSIS:
 *   The classification prevents mislabeling the constraint as pure extraction (snare) by noting that no human agenda-setter enforces the press's causation â it operates without institutional administration. However, it also prevents mislabeling as pure coordination (rope) because the constraint asymmetrically destroys one institutional actor's capacity while benefiting others. The FSM pathway captures the critical ambiguity: a mountain with beneficiaries is either a false natural law (constructed to benefit identifiable agents) or a genuine natural law that happens to advantage some parties. The omega variables route this ambiguity for empirical resolution.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_social_construction,
    'Is the printing press''s causal impact on religious revolution a natural consequence of its technical properties, or a socially constructed outcome dependent on specific political, economic, and cultural conditions?',
    'Comparative historical analysis of non-European print cultures (e.g., Ottoman Empire, East Asia) where the press did not produce equivalent religious rupture.',
    'If non-European cases show different outcomes, the mountain claim is falsified and should recompute as snare or tangled_rope (constructed extraction from Church authority, not natural law).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_social_construction, empirical, 'Whether press causation is natural law or constructed contingency').

omega_variable(
    beneficiary_identification_ambiguity,
    'Do the reformers genuinely benefit as free agents, or does the technological determinism framework itself extract agency from all human actors, including reformers, by making them mere conduits of technical necessity?',
    'Examine primary sources for whether reformers claim autonomous agency or attribute success to providence and technology.',
    'If reformers are themselves stripped of agency, the beneficiary/payer structure collapses and directionality shifts toward universal target status (all human agents pay the cost of determinism).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(beneficiary_identification_ambiguity, conceptual, 'Whether beneficiaries retain agency under determinism').

omega_variable(
    resistance_futility_vs_enforcement_decay,
    'Does the Church''s futile resistance indicate a genuine mountain (resistance is meaningless against natural law), or an enforcement decay where the Church lacked the organizational capacity to suppress print?',
    'Archival analysis of censorship enforcement capacity relative to print volume and territorial diffusion.',
    'If enforcement was structurally impossible at any feasible cost, mountain classification is supported; if enforcement was possible but politically costly, the constraint is a rope or tangled_rope with high exit costs.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(resistance_futility_vs_enforcement_decay, empirical, 'Whether Church futility signals mountain or failed enforcement').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(press_reformation_causation__technological_determinism, 0, 150).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pres_tr_t0, press_reformation_causation__technological_determinism, theater_ratio, 0, 0.05).
narrative_ontology:measurement(pres_tr_t25, press_reformation_causation__technological_determinism, theater_ratio, 25, 0.08).
narrative_ontology:measurement(pres_tr_t50, press_reformation_causation__technological_determinism, theater_ratio, 50, 0.12).
narrative_ontology:measurement(pres_tr_t75, press_reformation_causation__technological_determinism, theater_ratio, 75, 0.15).
narrative_ontology:measurement(pres_tr_t100, press_reformation_causation__technological_determinism, theater_ratio, 100, 0.18).
narrative_ontology:measurement(pres_tr_t125, press_reformation_causation__technological_determinism, theater_ratio, 125, 0.19).
narrative_ontology:measurement(pres_tr_t150, press_reformation_causation__technological_determinism, theater_ratio, 150, 0.2).

% Extraction over time
narrative_ontology:measurement(pres_be_t0, press_reformation_causation__technological_determinism, base_extractiveness, 0, 0.2).
narrative_ontology:measurement(pres_be_t25, press_reformation_causation__technological_determinism, base_extractiveness, 25, 0.4).
narrative_ontology:measurement(pres_be_t50, press_reformation_causation__technological_determinism, base_extractiveness, 50, 0.6).
narrative_ontology:measurement(pres_be_t75, press_reformation_causation__technological_determinism, base_extractiveness, 75, 0.68).
narrative_ontology:measurement(pres_be_t100, press_reformation_causation__technological_determinism, base_extractiveness, 100, 0.7).
narrative_ontology:measurement(pres_be_t125, press_reformation_causation__technological_determinism, base_extractiveness, 125, 0.71).
narrative_ontology:measurement(pres_be_t150, press_reformation_causation__technological_determinism, base_extractiveness, 150, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(pres_su_t0, press_reformation_causation__technological_determinism, suppression_requirement, 0, 0.1).
narrative_ontology:measurement(pres_su_t25, press_reformation_causation__technological_determinism, suppression_requirement, 25, 0.3).
narrative_ontology:measurement(pres_su_t50, press_reformation_causation__technological_determinism, suppression_requirement, 50, 0.55).
narrative_ontology:measurement(pres_su_t75, press_reformation_causation__technological_determinism, suppression_requirement, 75, 0.7).
narrative_ontology:measurement(pres_su_t100, press_reformation_causation__technological_determinism, suppression_requirement, 100, 0.65).
narrative_ontology:measurement(pres_su_t125, press_reformation_causation__technological_determinism, suppression_requirement, 125, 0.6).
narrative_ontology:measurement(pres_su_t150, press_reformation_causation__technological_determinism, suppression_requirement, 150, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
