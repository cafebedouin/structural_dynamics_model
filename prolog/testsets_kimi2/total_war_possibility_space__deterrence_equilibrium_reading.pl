% ============================================================================
% CONSTRAINT STORY: total_war_possibility_space__deterrence_equilibrium_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_total_war_possibility_space__deterrence_equilibrium_reading, []).

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
 *   constraint_id: total_war_possibility_space__deterrence_equilibrium_reading
 *   human_readable: Nuclear Deterrence Equilibrium (Total War Reachability Reading)
 *   domain: international_relations/strategic_studies
 *
 * SUMMARY:
 *   This constraint story instantiates the deterrence_equilibrium_reading of
 *   the total_war_possibility_space kernel. It treats the standing
 *   arrangement as one in which total war remains strategically
 *   reachableâactively planned for, doctrinally theorized, and materially
 *   preparedâbut is deterred by the certainty of mutual devastation. The
 *   constraint is not the absence of war but the active equilibrium of terror
 *   that prevents it, sustained by continuous investment in warfighting
 *   capability. This reading is contested by siblings: the
 *   space_contraction_reading holds that nuclear weapons have rendered total
 *   war unthinkable, while the nuclear_taboo_reading locates prohibition in
 *   normative evolution. The metrics and claim are independently authored:
 *   the constraint is claimed as tangled_rope because it combines genuine
 *   coordination (war prevention) with asymmetric extraction (resource
 *   diversion, risk externalization) and requires active institutional
 *   enforcement.
 *
 * KEY AGENTS:
 *   - Nuclear weapon states: Agenda-setter (institutional/civilizational/constrained exit) â administer the equilibrium
 *   - Defense industrial base: Beneficiary (powerful/generational/constrained) â captures the resource flow
 *   - Strategic community: Beneficiary (organized/generational/identity_locked) â legitimates the doctrine
 *   - Tax-bearing public: Payer (powerless/biographical/trapped) â funds the system without voice
 *   - Frontline host populations: Payer (powerless/biographical/trapped) â bear localized existential risk
 *   - Non-nuclear weapon states: Excluded (moderate/generational/trapped) â affected but excluded from design
 *   - Independent analysts: Observer (analytical/generational/analytical) â external evaluative seat
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(total_war_possibility_space__deterrence_equilibrium_reading, 0.72).
domain_priors:suppression_score(total_war_possibility_space__deterrence_equilibrium_reading, 0.71).
domain_priors:theater_ratio(total_war_possibility_space__deterrence_equilibrium_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(total_war_possibility_space__deterrence_equilibrium_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(total_war_possibility_space__deterrence_equilibrium_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(total_war_possibility_space__deterrence_equilibrium_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(total_war_possibility_space__deterrence_equilibrium_reading, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(total_war_possibility_space__deterrence_equilibrium_reading, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(total_war_possibility_space__deterrence_equilibrium_reading, tangled_rope).
narrative_ontology:human_readable(total_war_possibility_space__deterrence_equilibrium_reading, "Nuclear Deterrence Equilibrium (Total War Reachability Reading)").
narrative_ontology:topic_domain(total_war_possibility_space__deterrence_equilibrium_reading, "international_relations/strategic_studies").

domain_priors:requires_active_enforcement(total_war_possibility_space__deterrence_equilibrium_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(total_war_possibility_space__deterrence_equilibrium_reading, '753982cf-7fe2-44b9-9c5a-31b29ec53587').
narrative_ontology:cs_kernel_codification('753982cf-7fe2-44b9-9c5a-31b29ec53587', formalized).
narrative_ontology:cs_authority_grounding('753982cf-7fe2-44b9-9c5a-31b29ec53587', expertise).
narrative_ontology:cs_interpretation_layer_present('753982cf-7fe2-44b9-9c5a-31b29ec53587').
narrative_ontology:cs_reading_relation('753982cf-7fe2-44b9-9c5a-31b29ec53587', total_war_possibility_space__space_contraction_reading, forecloses).
narrative_ontology:cs_reading_relation('753982cf-7fe2-44b9-9c5a-31b29ec53587', total_war_possibility_space__nuclear_taboo_reading, coexists_with).
narrative_ontology:cs_axiom('753982cf-7fe2-44b9-9c5a-31b29ec53587', foundational, credible_deterrence_requires_continuous_counterforce_capability).
narrative_ontology:cs_axiom_status(credible_deterrence_requires_continuous_counterforce_capability, holdable).
narrative_ontology:cs_axiom_grounding('753982cf-7fe2-44b9-9c5a-31b29ec53587', credible_deterrence_requires_continuous_counterforce_capability, empirically_contingent).
narrative_ontology:cs_axiom('753982cf-7fe2-44b9-9c5a-31b29ec53587', foundational, mutual_assured_destruction_is_strategically_stable).
narrative_ontology:cs_axiom_status(mutual_assured_destruction_is_strategically_stable, holdable).
narrative_ontology:cs_axiom_grounding('753982cf-7fe2-44b9-9c5a-31b29ec53587', mutual_assured_destruction_is_strategically_stable, empirically_contingent).
narrative_ontology:cs_reference_frame('753982cf-7fe2-44b9-9c5a-31b29ec53587', stable_mutual_vulnerability).
narrative_ontology:cs_drift_state('753982cf-7fe2-44b9-9c5a-31b29ec53587', post_cold_war_multi_polarity, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('753982cf-7fe2-44b9-9c5a-31b29ec53587', '').
narrative_ontology:cs_kernel_id(total_war_possibility_space__deterrence_equilibrium_reading, total_war_possibility_space).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(total_war_possibility_space__deterrence_equilibrium_reading, nuclear_weapon_states).
narrative_ontology:constraint_beneficiary(total_war_possibility_space__deterrence_equilibrium_reading, defense_industrial_base).
narrative_ontology:constraint_beneficiary(total_war_possibility_space__deterrence_equilibrium_reading, strategic_community).
narrative_ontology:constraint_victim(total_war_possibility_space__deterrence_equilibrium_reading, tax_bearing_public).
narrative_ontology:constraint_victim(total_war_possibility_space__deterrence_equilibrium_reading, frontline_host_populations).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintain and modernize nuclear arsenals, set deterrence doctrine, and negotiate arms control from a position of assured destruction capability. Their strategic planning assumes total war remains reachable and must be deterred through continuous capability investment. Exit is constrained by security-dilemma dynamics and alliance commitments.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__deterrence_equilibrium_reading, nuclear_weapon_states, agenda_setter,
    institutional, civilizational, constrained, global).

% Designs, manufactures, and maintains nuclear delivery systems and command infrastructure. Receives sustained, lucrative contracts tied to modernization cycles justified by deterrence requirements. Workforce and capital are specialized to the sector, making exit costly.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__deterrence_equilibrium_reading, defense_industrial_base, beneficiary,
    powerful, generational, constrained, global).

% Develops and legitimizes deterrence doctrine, escalation theory, and warfighting concepts within think tanks, universities, and military academies. Career advancement and intellectual reputation depend on the continued centrality of nuclear strategy in security studies.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__deterrence_equilibrium_reading, strategic_community, beneficiary,
    organized, generational, identity_locked, global).

% Funds nuclear arsenals through taxation without meaningful input into doctrine or deployment decisions. Bears the opportunity cost of diverted public spending and the existential risk of potential nuclear exchange. No individual exit from the deterrent system is available.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__deterrence_equilibrium_reading, tax_bearing_public, payer,
    powerless, biographical, trapped, national).

% Live in proximity to nuclear weapons bases and command sites, bearing elevated environmental and targeting risks without consent or compensation. Their communities are structurally tied to base economies but are also first-strike targets. No political mechanism reliably prioritizes their safety over strategic posture.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__deterrence_equilibrium_reading, frontline_host_populations, payer,
    powerless, biographical, trapped, regional).

% Excluded from nuclear deterrence decision-making but subject to downstream effects including potential nuclear winter, environmental contamination, and alliance pressure to support or host infrastructure. Their security preferences are underweighted in strategic discourse.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__deterrence_equilibrium_reading, non_nuclear_weapon_states, excluded,
    moderate, generational, trapped, global).

% Evaluate the empirical and normative claims of deterrence theory from outside the strategic community. They document costs, risks, and alternative security architectures but lack institutional power to alter doctrine.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__deterrence_equilibrium_reading, independent_analysts, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Prevents total war between nuclear-armed adversaries by making the anticipated costs of strategic exchange exceed any conceivable political benefit, thereby coordinating mutual restraint through the shared threat of mutual annihilation.
% TRANSFER_FUNCTION: Moves financial and human resources from general publics and frontline communities to nuclear weapons states, their defense industrial bases, and strategic research institutions in the form of sustained military spending and basing access, to maintain the continuous readiness and modernization required for credible deterrence.
% ABSENT_VOICES: Anti-nuclear and disarmament movements, frontline host populations, non-nuclear weapon states, and victims of nuclear testing are systematically underweighted in strategic planning; their exclusion is structural to a deterrence discourse that frames security exclusively through warfighting capability.
% DISAPPEARANCE_RATIONALE: If the deterrence equilibrium vanished, great-power military spending would shift dramatically away from strategic nuclear forces, alliance architectures built on extended deterrence would fracture or transform, and the institutional core of strategic studies would lose its organizing premise; the international security order would reorganize around fundamentally different threat assessments.
% FOUNDING_PROBLEM: How to prevent catastrophic great-power total war in an era of nuclear weapons capable of civilization-scale destruction.
% FOUNDING_PROBLEM_CORROBORATION: Cold War historians and former defense officials from nuclear weapon states attest to the founding problem's historical urgency; however, independent security scholars and humanitarian initiatives (e.g., ICAN, TPNW) from outside the beneficiary set argue the problem's character has shifted toward risk management while the institutional response remains locked in its original form, suggesting functional drift.
narrative_ontology:disappearance_verdict(total_war_possibility_space__deterrence_equilibrium_reading, world_rearranges).
narrative_ontology:founding_problem_status(total_war_possibility_space__deterrence_equilibrium_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(total_war_possibility_space__deterrence_equilibrium_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(total_war_possibility_space__deterrence_equilibrium_reading, 'none', 1).
narrative_ontology:epsilon_provenance(total_war_possibility_space__deterrence_equilibrium_reading, 0.72, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(total_war_possibility_space__deterrence_equilibrium_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(total_war_possibility_space__deterrence_equilibrium_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(total_war_possibility_space__deterrence_equilibrium_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.72) because the constraint diverts trillions in public resources to specialized beneficiaries and externalizes catastrophic risk onto excluded populations. Suppression (0.71) reflects the active marginalization of disarmament alternatives and the structural suppression of non-nuclear voices. Theater ratio (0.45) captures the growing performative dimension of modernization: much current investment exceeds the technical requirements of minimal deterrence and serves signaling and institutional interests. Accessibility collapse (0.78) is high because, once the deterrence frame is accepted, unilateral exit appears strategically irrational, collapsing the imaginative and policy space for abolition. Resistance (0.35) is moderate: anti-nuclear movements persist but are politically marginalized and lack leverage over nuclear command authority. The measurement series show extraction rising from the early nuclear era through the Cold War, moderating temporarily in the post-Cold War decade, and resurging with great-power competition and modernization programs, while theater ratio steadily accumulates.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter and beneficiary seats (nuclear states, defense industry, strategic community) experience the constraint as essential coordination that prevents civilizational catastrophe; the computed type from their structural position should emphasize coordination. The payer seats (taxpayers, host populations) experience it as extractive risk-imposition and resource diversion; from their position the engine should compute higher effective extraction. The excluded seats (non-nuclear states) experience a combination of externalized risk and voicelessness. This divergence is the signal the corpus is designed to capture.
 *
 * DIRECTIONALITY LOGIC:
 *   Nuclear weapon states sit at low directionality as agenda-setters who define the rules and benefit from great-power status. The defense industrial base and strategic community are structural beneficiaries (low d) whose continued existence depends on the constraint. Tax-bearing public and frontline populations are full targets (high d): they pay costs and bear risks without controlling the arrangement. Non-nuclear states are also high-d targets, though their exclusion means they are not even inside the coordination bargain. Independent analysts occupy the analytical position with neutral d.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problemâpreventing civilization-scale warâwas historically live. The constraint avoids mandatrophy mislabeling because the coordination function (war prevention) remains partially genuine: nuclear arsenals have indeed prevented great-power total war. However, the reading predicts continuous doctrinal and investment activity that exceeds pure stability requirements. This excess is the extraction signature that distinguishes tangled_rope from rope: the same structure that coordinates also enriches and empowers specific constituencies. Were the coordination function to atrophy entirely while investment persisted, the constraint would degrade toward snare or piton.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    deterrence_extraction_or_necessity,
    'Does the current scale and doctrinal complexity of nuclear modernization reflect the objective technical requirements of credible deterrence, or does it represent extractive surplus captured by the defense industrial base and strategic community?',
    'Comparative analysis of minimum deterrence postures versus actual force structures and spending; regulatory or legislative audit of cost-to-capability ratios.',
    'If surplus is shown, the constraint''s extractiveness is higher than its coordination justification supports, strengthening the tangled_rope classification; if not, the constraint moves toward rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(deterrence_extraction_or_necessity, empirical, 'Whether nuclear modernization tracks deterrence necessity or institutional rent-seeking').

omega_variable(
    kernel_reading_foreclosure,
    'Does the continuous theorization of counterforce targeting and escalation ladders under the deterrence equilibrium reading logically foreclose the space_contraction_reading''s claim that total war has been removed from the strategically thinkable?',
    'Documentary analysis of strategic doctrine to determine whether war plans treat total war as a live planning scenario or as a formally foreclosed option.',
    'If doctrine retains active total-war planning, the deterrence_equilibrium reading is vindicated and the space_contraction reading is logically foreclosed within a unified strategic framework; if not, the readings may coexist as domain-specific framings.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_foreclosure, conceptual, 'Logical relationship between deterrence equilibrium and space contraction readings').

omega_variable(
    stability_instability_paradox,
    'Does the deterrence equilibrium prevent total war, or does it merely displace conflict into lower-intensity proxy wars and accidental-risk domains?',
    'Historical counterfactual analysis and crisis-outcome data (e.g., Cuban Missile Crisis, Cold War proxy conflicts, near-miss incidents).',
    'If deterrence is shown to prevent total war but increase proxy and accident risk, the coordination benefit is partial and the constraint''s net extractiveness rises; if it prevents war altogether, the coordination function is stronger.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(stability_instability_paradox, empirical, 'Empirical status of deterrence success versus instability displacement').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(total_war_possibility_space__deterrence_equilibrium_reading, 0, 80).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tota_tr_t0, total_war_possibility_space__deterrence_equilibrium_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(tota_tr_t20, total_war_possibility_space__deterrence_equilibrium_reading, theater_ratio, 20, 0.3).
narrative_ontology:measurement(tota_tr_t40, total_war_possibility_space__deterrence_equilibrium_reading, theater_ratio, 40, 0.35).
narrative_ontology:measurement(tota_tr_t60, total_war_possibility_space__deterrence_equilibrium_reading, theater_ratio, 60, 0.4).
narrative_ontology:measurement(tota_tr_t80, total_war_possibility_space__deterrence_equilibrium_reading, theater_ratio, 80, 0.45).

% Extraction over time
narrative_ontology:measurement(tota_be_t0, total_war_possibility_space__deterrence_equilibrium_reading, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(tota_be_t20, total_war_possibility_space__deterrence_equilibrium_reading, base_extractiveness, 20, 0.7).
narrative_ontology:measurement(tota_be_t40, total_war_possibility_space__deterrence_equilibrium_reading, base_extractiveness, 40, 0.68).
narrative_ontology:measurement(tota_be_t60, total_war_possibility_space__deterrence_equilibrium_reading, base_extractiveness, 60, 0.65).
narrative_ontology:measurement(tota_be_t80, total_war_possibility_space__deterrence_equilibrium_reading, base_extractiveness, 80, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(tota_su_t0, total_war_possibility_space__deterrence_equilibrium_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(tota_su_t20, total_war_possibility_space__deterrence_equilibrium_reading, suppression_requirement, 20, 0.6).
narrative_ontology:measurement(tota_su_t40, total_war_possibility_space__deterrence_equilibrium_reading, suppression_requirement, 40, 0.65).
narrative_ontology:measurement(tota_su_t60, total_war_possibility_space__deterrence_equilibrium_reading, suppression_requirement, 60, 0.68).
narrative_ontology:measurement(tota_su_t80, total_war_possibility_space__deterrence_equilibrium_reading, suppression_requirement, 80, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
