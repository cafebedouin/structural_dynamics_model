% ============================================================================
% CONSTRAINT STORY: competence_retention_exercise__catastrophe_as_necessary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_competence_retention_exercise__catastrophe_as_necessary, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
 *   constraint_id: competence_retention_exercise__catastrophe_as_necessary
 *   human_readable: Catastrophe-as-Necessary Reading of Competence Retention
 *   domain: safety_engineering/organizational_learning
 *
 * SUMMARY:
 *   This story instantiates one reading of a contested kernel about how
 *   organizations retain genuine safety competence over time. The
 *   catastrophe-as-necessary reading holds that only lived-through disaster
 *   provides the visceral stakes and organizational learning required for
 *   real competence; simulation, however sophisticated, is rehearsal that
 *   cannot substitute for the real thing. This reading has genuine
 *   coordination value — it names a real phenomenon (competence decay during
 *   long incident-free stretches, the 'safest-looking moment is often the
 *   most dangerous' pattern documented across high-hazard industries) — but
 *   it also structurally elevates catastrophe-experienced actors
 *   (investigators, veteran operators, reform coalitions) at the expense of
 *   frontline staff, junior workers, simulation vendors, and exposed
 *   communities. It treats a periodic catastrophic 'reset' as functionally
 *   necessary, which is where the extraction lives: the doctrine's
 *   beneficiaries gain standing and resources specifically because disasters
 *   happen, and the doctrine offers no legitimate route to competence
 *   recognition short of surviving one.
 *
 * KEY AGENTS:
 *   - incident_investigation_bodies: institutional beneficiary whose mandate and funding expand after catastrophes
 *   - veteran_operators_with_catastrophe_experience: moderate-power beneficiaries whose status depends on catastrophe experience being irreplaceable
 *   - frontline_operators_during_incident_free_periods: powerless payers whose competence is structurally delegitimized absent a lived disaster
 *   - communities_exposed_to_high_hazard_facilities: powerless payers who absorb the human cost of the doctrine's implied 'necessary reset'
 *   - safety_science_researchers: analytical observers whose comparative evidence complicates the catastrophe-necessity premise
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(competence_retention_exercise__catastrophe_as_necessary, 0.58).
domain_priors:suppression_score(competence_retention_exercise__catastrophe_as_necessary, 0.42).
domain_priors:theater_ratio(competence_retention_exercise__catastrophe_as_necessary, 0.31).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(competence_retention_exercise__catastrophe_as_necessary, extractiveness, 0.58).
narrative_ontology:constraint_metric(competence_retention_exercise__catastrophe_as_necessary, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(competence_retention_exercise__catastrophe_as_necessary, theater_ratio, 0.31).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(competence_retention_exercise__catastrophe_as_necessary, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(competence_retention_exercise__catastrophe_as_necessary, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(competence_retention_exercise__catastrophe_as_necessary, tangled_rope).
narrative_ontology:human_readable(competence_retention_exercise__catastrophe_as_necessary, "Catastrophe-as-Necessary Reading of Competence Retention").
narrative_ontology:topic_domain(competence_retention_exercise__catastrophe_as_necessary, "safety_engineering/organizational_learning").

domain_priors:requires_active_enforcement(competence_retention_exercise__catastrophe_as_necessary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(competence_retention_exercise__catastrophe_as_necessary, '3c34ebe8-001d-48ef-97a6-e94447057d8e').
narrative_ontology:cs_kernel_codification('3c34ebe8-001d-48ef-97a6-e94447057d8e', distributed).
narrative_ontology:cs_authority_grounding('3c34ebe8-001d-48ef-97a6-e94447057d8e', practice).
narrative_ontology:cs_interpretation_layer_present('3c34ebe8-001d-48ef-97a6-e94447057d8e').
narrative_ontology:cs_reading_relation('3c34ebe8-001d-48ef-97a6-e94447057d8e', competence_retention_exercise__simulation_as_sufficient, forecloses).
narrative_ontology:cs_reading_relation('3c34ebe8-001d-48ef-97a6-e94447057d8e', competence_retention_exercise__near_miss_as_bridge, influences).
narrative_ontology:cs_axiom('3c34ebe8-001d-48ef-97a6-e94447057d8e', foundational, catastrophe_is_epistemically_irreplaceable).
narrative_ontology:cs_axiom_status(catastrophe_is_epistemically_irreplaceable, holdable).
narrative_ontology:cs_axiom_grounding('3c34ebe8-001d-48ef-97a6-e94447057d8e', catastrophe_is_epistemically_irreplaceable, empirically_contingent).
narrative_ontology:cs_axiom('3c34ebe8-001d-48ef-97a6-e94447057d8e', secondary, visceral_stakes_are_necessary_for_genuine_learning).
narrative_ontology:cs_axiom_status(visceral_stakes_are_necessary_for_genuine_learning, holdable).
narrative_ontology:cs_axiom_grounding('3c34ebe8-001d-48ef-97a6-e94447057d8e', visceral_stakes_are_necessary_for_genuine_learning, instrumental).
narrative_ontology:cs_reference_frame('3c34ebe8-001d-48ef-97a6-e94447057d8e', post_disaster_investigative_authority_tradition).
narrative_ontology:cs_drift_state('3c34ebe8-001d-48ef-97a6-e94447057d8e', contemporary_high_reliability_organization_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('3c34ebe8-001d-48ef-97a6-e94447057d8e', '').
narrative_ontology:cs_kernel_id(competence_retention_exercise__catastrophe_as_necessary, competence_retention_exercise).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(competence_retention_exercise__catastrophe_as_necessary, incident_investigation_bodies).
narrative_ontology:constraint_beneficiary(competence_retention_exercise__catastrophe_as_necessary, post_disaster_reform_coalitions).
narrative_ontology:constraint_beneficiary(competence_retention_exercise__catastrophe_as_necessary, veteran_operators_with_catastrophe_experience).
narrative_ontology:constraint_victim(competence_retention_exercise__catastrophe_as_necessary, frontline_operators_during_incident_free_periods).
narrative_ontology:constraint_victim(competence_retention_exercise__catastrophe_as_necessary, communities_exposed_to_high_hazard_facilities).
narrative_ontology:constraint_victim(competence_retention_exercise__catastrophe_as_necessary, junior_staff_denied_credible_alternative_pathways).
narrative_ontology:constraint_vindicates(competence_retention_exercise__catastrophe_as_necessary, organizational_amnesia_thesis).
narrative_ontology:constraint_vindicates(competence_retention_exercise__catastrophe_as_necessary, safety_normalization_of_deviance_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Investigate catastrophes after they occur and issue the authoritative findings that reshape regulation and training doctrine. Their institutional relevance, funding, and mandate expand specifically in the aftermath of disasters; incident-free years reduce their visibility and budget leverage. They administer the doctrine that treats catastrophe as the primary teacher.
narrative_ontology:constraint_stakeholder(competence_retention_exercise__catastrophe_as_necessary, incident_investigation_bodies, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(competence_retention_exercise__catastrophe_as_necessary, incident_investigation_bodies, beneficiary).

% Advocacy groups, unions, and reform-minded managers who gain standing, funding, and policy leverage in the wake of catastrophic failures. They cite the catastrophe-as-teacher narrative to justify sweeping changes that would otherwise face institutional resistance during calm periods.
narrative_ontology:constraint_stakeholder(competence_retention_exercise__catastrophe_as_necessary, post_disaster_reform_coalitions, beneficiary,
    organized, generational, mobile, national).

% Operators who lived through a real catastrophic event hold elevated status and are treated as uniquely credentialed sources of 'real' competence. Their authority within the organization depends on the belief that their catastrophe-derived knowledge cannot be replicated by simulation, which insulates them from being displaced by simulation-trained peers.
narrative_ontology:constraint_stakeholder(competence_retention_exercise__catastrophe_as_necessary, veteran_operators_with_catastrophe_experience, beneficiary,
    moderate, biographical, constrained, local).

% Work the plant, control room, or unit during the long stretches between disasters. Under this reading their competence is treated as inherently degrading and untrustworthy no matter how much they train, because only a real catastrophe counts as genuine exercise. They bear the psychological burden of being told their skills are unverifiable and carry the operational risk when the eventual 'necessary reset' event actually occurs.
narrative_ontology:constraint_stakeholder(competence_retention_exercise__catastrophe_as_necessary, frontline_operators_during_incident_free_periods, payer,
    powerless, immediate, trapped, local).

% Live near refineries, plants, or transit corridors governed by these organizations. If the doctrine is correct that only real catastrophes reset competence, then this reading structurally accepts a periodic catastrophic event as the mechanism of organizational learning — meaning these communities are the ones who absorb the human and environmental cost of that 'necessary' reset.
narrative_ontology:constraint_stakeholder(competence_retention_exercise__catastrophe_as_necessary, communities_exposed_to_high_hazard_facilities, payer,
    powerless, biographical, trapped, regional).

% New hires and less senior staff who have never experienced a real catastrophe are told, implicitly or explicitly, that their simulation-based training cannot make them genuinely competent. They have no path to the credibility conferred by catastrophe experience short of a disaster occurring on their watch, which structurally blocks legitimate advancement and undermines their standing in safety-critical decisions.
narrative_ontology:constraint_stakeholder(competence_retention_exercise__catastrophe_as_necessary, junior_staff_denied_credible_alternative_pathways, payer,
    powerless, biographical, constrained, local).

% Build and sell high-fidelity simulators and training programs. Under this reading their product is structurally devalued as 'mere rehearsal,' regardless of fidelity improvements. They are not consulted in the doctrine's formation and have no voice in a debate that determines whether their investment case is credible.
narrative_ontology:constraint_stakeholder(competence_retention_exercise__catastrophe_as_necessary, simulation_and_training_vendors, excluded,
    organized, biographical, constrained, national).

% Study organizational learning empirically across industries, comparing incident-free high-reliability organizations against ones that experienced disasters. They can evaluate whether the catastrophe-as-necessary claim holds up against comparative evidence, but their findings are cited selectively by whichever coalition benefits from a given reading.
narrative_ontology:constraint_stakeholder(competence_retention_exercise__catastrophe_as_necessary, safety_science_researchers, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(competence_retention_exercise__catastrophe_as_necessary, diffuse).
narrative_ontology:fixing_cost_class(competence_retention_exercise__catastrophe_as_necessary, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The reading coordinates organizational attention and resource allocation around a genuine hazard: competence decay during long incident-free periods is real, and the doctrine mobilizes vigilance, funding, and reform energy that might otherwise atrophy under complacency.
% TRANSFER_FUNCTION: Moves institutional credibility, funding, and authority toward catastrophe-experienced actors (investigators, veteran operators, post-disaster reformers) and away from simulation-trained staff and vendors; moves risk and cost of the 'necessary reset' onto frontline workers and exposed communities who bear the disaster when it eventually occurs.
% ABSENT_VOICES: Simulation and training vendors, and safety researchers studying incident-free high-reliability organizations (aviation, nuclear carrier operations) that maintain competence for decades without catastrophic resets, are structurally excluded from shaping the doctrine — their evidence would complicate the claim that only real disasters teach.
% DISAPPEARANCE_RATIONALE: Incident investigation bodies and post-disaster coalitions would argue the world becomes more dangerous if this reading vanished, because vigilance would erode without the visceral stakes narrative. Frontline operators, communities, and simulation vendors would argue the world improves, because resources currently withheld pending catastrophic proof would flow instead into rigorous simulation-based competence programs, and junior staff would gain legitimate advancement paths.
% FOUNDING_PROBLEM: The doctrine was built to explain a real and repeatedly observed phenomenon: organizations that go too long without an incident become complacent, drift into normalized deviance, and suffer worse failures than more recently chastened peers — the felt need was to explain why 'nothing bad has happened' is not the same as 'we are safe.'
% FOUNDING_PROBLEM_CORROBORATION: Safety science researchers outside the benefiting coalitions corroborate the underlying decay phenomenon (drift into failure, normalization of deviance) but do not corroborate the specific claim that ONLY actual catastrophe can reset it — comparative studies of aircraft carrier flight-deck operations and nuclear submarine crews suggest sustained incident-free high reliability is achievable through disciplined simulation and near-miss analysis, which undercuts the catastrophe-necessity premise even as it supports the decay premise.
narrative_ontology:disappearance_verdict(competence_retention_exercise__catastrophe_as_necessary, contested).
narrative_ontology:founding_problem_status(competence_retention_exercise__catastrophe_as_necessary, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(competence_retention_exercise__catastrophe_as_necessary, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(competence_retention_exercise__catastrophe_as_necessary, 'none', 1).
narrative_ontology:epsilon_provenance(competence_retention_exercise__catastrophe_as_necessary, 0.58, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(competence_retention_exercise__catastrophe_as_necessary_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(competence_retention_exercise__catastrophe_as_necessary, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(competence_retention_exercise__catastrophe_as_necessary_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at a moderate-high and rising 0.58 because the coordination function (naming real competence decay) is genuine but is bundled with an extraction mechanism: institutional and reputational capital flows disproportionately to those positioned to benefit from catastrophe having occurred, while frontline and junior actors bear a structural devaluation of their non-catastrophe-derived competence with no remedy available to them. Suppression is moderate (0.42) — this is not a coercively enforced doctrine in the way an OS platform commission is, but it is actively reinforced through training budgets, promotion criteria, and post-incident narrative construction that a rising suppression_requirement trajectory reflects as institutional memory ossifies around catastrophe-derived authority. Theater ratio rises modestly (0.18→0.31) reflecting a growing share of 'lessons learned' activity becoming ceremonial invocation of past disasters rather than genuinely updating practice.
 *
 * PERSPECTIVAL GAP:
 *   Incident investigation bodies and post-disaster coalitions experience this constraint as a genuine, hard-won coordination mechanism that keeps organizations honest. Frontline operators and exposed communities experience the same structure as an arrangement that discounts their day-to-day vigilance and treats periodic catastrophe as an acceptable, even necessary, cost of institutional learning. The engine should compute divergent seat types from this same structural data — agenda-setter and beneficiary seats trending toward coordination-favorable readings, payer seats trending toward extraction-favorable readings.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (investigation bodies, reform coalitions, veteran operators) are declared because they gain standing, funding, or unassailable authority specifically through the catastrophe-necessity narrative — this pushes their derived directionality toward the beneficiary end. Victims (frontline operators, exposed communities, junior staff) bear the costs: delegitimized competence, blocked advancement, and ultimately exposure to the disaster the doctrine treats as periodically necessary — this pushes their derived directionality toward the target end. Simulation vendors are excluded rather than victimized outright; their devaluation is collateral to the doctrine's authority structure rather than its direct extraction target.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — real, measurable competence decay during incident-free periods — remains partially live, which prevents this from being classified as a pure zombie mandate. But the specific mechanism this reading insists on (catastrophe as the ONLY valid reset) is contested by outside corroboration (comparative high-reliability-organization research), which is exactly the divergence the mandatrophy framework is built to surface: a real founding problem persisting alongside a narrower, self-serving prescription about how it must be solved.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    catastrophe_necessity_vs_decay_reality,
    'Is the observed competence decay during incident-free periods best remedied only by actual catastrophe, or can rigorously designed simulation and near-miss analysis achieve equivalent remediation — making the catastrophe-necessity claim a cover story for status allocation rather than a genuine epistemic requirement?',
    'Comparative longitudinal study of high-reliability organizations (naval aviation carrier operations, nuclear submarine crews, some air traffic control systems) that sustain low-incident safety records for decades via simulation and near-miss protocols, versus organizations whose competence is explicitly catastrophe-anchored — measuring actual safety outcomes, not narrative self-report.',
    'If simulation-sustained high-reliability organizations show equivalent or superior long-run safety outcomes, this reading''s core premise is empirically falsified and the constraint would reclassify toward pure extraction (status allocation to catastrophe-experienced actors, no genuine coordination remainder). If catastrophe-anchored organizations consistently outperform, the coordination function is more strongly vindicated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(catastrophe_necessity_vs_decay_reality, empirical, 'Whether catastrophe is genuinely necessary for competence retention or whether this is a status-allocation narrative riding on a real but separately-remediable decay phenomenon.').

omega_variable(
    kernel_reading_selection_pressure,
    'Why does an organization or field select the catastrophe_as_necessary reading over near_miss_as_bridge or simulation_as_sufficient, when all three readings can explain the same underlying decay phenomenon?',
    'Trace which actors within a given organization campaign for which reading, and whether reading selection correlates with which actors hold catastrophe-derived authority versus simulation/near-miss expertise — a self-interested selection pattern would indicate the reading functions partly as an authority-preservation mechanism.',
    'If reading selection tracks incumbent authority structures rather than independent safety evidence, this substantially strengthens the tangled_rope classification (coordination function real, but reading choice itself is captured by beneficiary interests) over a pure Rope reading of the same underlying decay problem.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_selection_pressure, conceptual, 'Whether the choice among sibling kernel readings is evidence-driven or authority-preservation-driven.').

omega_variable(
    necessary_reset_moral_cost,
    'Does treating catastrophe as a ''necessary system reset'' create a moral hazard where organizations under this doctrine become less motivated to prevent catastrophic events, since such events are framed as ultimately functional for organizational learning?',
    'Examine whether organizations explicitly or implicitly operating under this doctrine show measurably different investment in catastrophe-prevention (versus catastrophe-response and post-catastrophe learning infrastructure) compared to organizations operating under the simulation_as_sufficient or near_miss_as_bridge readings.',
    'If catastrophe-as-necessary organizations underinvest in prevention relative to response/learning infrastructure, this reading''s coordination story is significantly undercut — it would function as a rationalization for underinvestment in prevention, shifting cost onto exposed communities and frontline workers.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(necessary_reset_moral_cost, empirical, 'Whether the doctrine creates a moral hazard reducing prevention investment relative to response investment.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(competence_retention_exercise__catastrophe_as_necessary, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comp_tr_t0, competence_retention_exercise__catastrophe_as_necessary, theater_ratio, 0, 0.18).
narrative_ontology:measurement(comp_tr_t8, competence_retention_exercise__catastrophe_as_necessary, theater_ratio, 8, 0.21).
narrative_ontology:measurement(comp_tr_t16, competence_retention_exercise__catastrophe_as_necessary, theater_ratio, 16, 0.24).
narrative_ontology:measurement(comp_tr_t24, competence_retention_exercise__catastrophe_as_necessary, theater_ratio, 24, 0.27).
narrative_ontology:measurement(comp_tr_t32, competence_retention_exercise__catastrophe_as_necessary, theater_ratio, 32, 0.29).
narrative_ontology:measurement(comp_tr_t40, competence_retention_exercise__catastrophe_as_necessary, theater_ratio, 40, 0.31).

% Extraction over time
narrative_ontology:measurement(comp_be_t0, competence_retention_exercise__catastrophe_as_necessary, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(comp_be_t8, competence_retention_exercise__catastrophe_as_necessary, base_extractiveness, 8, 0.46).
narrative_ontology:measurement(comp_be_t16, competence_retention_exercise__catastrophe_as_necessary, base_extractiveness, 16, 0.5).
narrative_ontology:measurement(comp_be_t24, competence_retention_exercise__catastrophe_as_necessary, base_extractiveness, 24, 0.53).
narrative_ontology:measurement(comp_be_t32, competence_retention_exercise__catastrophe_as_necessary, base_extractiveness, 32, 0.56).
narrative_ontology:measurement(comp_be_t40, competence_retention_exercise__catastrophe_as_necessary, base_extractiveness, 40, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(comp_su_t0, competence_retention_exercise__catastrophe_as_necessary, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(comp_su_t8, competence_retention_exercise__catastrophe_as_necessary, suppression_requirement, 8, 0.33).
narrative_ontology:measurement(comp_su_t16, competence_retention_exercise__catastrophe_as_necessary, suppression_requirement, 16, 0.36).
narrative_ontology:measurement(comp_su_t24, competence_retention_exercise__catastrophe_as_necessary, suppression_requirement, 24, 0.38).
narrative_ontology:measurement(comp_su_t32, competence_retention_exercise__catastrophe_as_necessary, suppression_requirement, 32, 0.4).
narrative_ontology:measurement(comp_su_t40, competence_retention_exercise__catastrophe_as_necessary, suppression_requirement, 40, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(competence_retention_exercise__catastrophe_as_necessary, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(competence_retention_exercise__catastrophe_as_necessary, 0.12).
narrative_ontology:affects_constraint(competence_retention_exercise__catastrophe_as_necessary, simulation_as_sufficient).
narrative_ontology:affects_constraint(competence_retention_exercise__catastrophe_as_necessary, near_miss_as_bridge).

% DUAL FORMULATION NOTE:
% This constraint is one of three linked readings of the competence_retention_exercise kernel. simulation_as_sufficient holds the opposite structural position (high-fidelity simulation is equivalent to real events), directly contesting this reading's core premise. near_miss_as_bridge occupies a middle position (near-misses suffice without full catastrophe). All three share the underlying decay phenomenon but diverge in beneficiary/victim structure: this reading concentrates authority in catastrophe-experienced actors and investigation bodies; simulation_as_sufficient would concentrate authority in simulation vendors and technical training staff; near_miss_as_bridge would concentrate authority in incident-reporting and near-miss-analysis functions. Each reading's ε is authored independently per the ε-invariance principle — they are not measurements of the same constraint from different angles but structurally distinct constraints sharing a contested kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
