% ============================================================================
% CONSTRAINT STORY: catastrophe_proxy_sufficiency__simulation_fidelity_threshold
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_catastrophe_proxy_sufficiency__simulation_fidelity_threshold, []).

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
    narrative_ontology:suppression_profile/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: catastrophe_proxy_sufficiency__simulation_fidelity_threshold
 *   human_readable: Simulation Fidelity Threshold for Catastrophe-Competence Retention
 *   domain: safety_engineering/organizational_learning
 *
 * SUMMARY:
 *   High-reliability organizations (nuclear plants, airlines, navies) rely on
 *   simulation to build and certify competence for catastrophic scenarios
 *   they cannot ethically or safely induce in real life. The 'simulation
 *   fidelity threshold' reading holds that this is a genuinely solvable
 *   coordination problem: as simulation technology (motion platforms,
 *   physiological stress induction, adversarial scenario generation, VR
 *   immersion) advances, it crosses a threshold at which the simulated
 *   stressor becomes functionally equivalent to the real catastrophe for
 *   competence-retention purposes. Under this reading, sufficiency is not a
 *   fixed philosophical impossibility (as the catastrophe_necessity_reading
 *   holds) nor an already-achieved universal fact (as the
 *   simulation_as_proxy_catastrophe_reading holds) — it is a moving technical
 *   target that specific simulator generations either clear or do not, and
 *   organizations that invest sufficiently get real competence retention
 *   while those that under-invest do not. This is the reading that best
 *   explains why organizations differentiate: some simulator programs
 *   demonstrably work (post-incident review finds trained response matched
 *   threshold-crossing training) and others demonstrably do not.
 *
 * KEY AGENTS:
 *   - simulation_technology_vendors: primary beneficiary — sells the threshold-crossing hardware and scenario design
 *   - high_reliability_organizations: agenda-setter and beneficiary — decides investment level, bears risk of under-investment
 *   - certifying_regulators: beneficiary — gains a defensible, technical certification basis
 *   - frontline_operators_undertrained: payer — bears consequences if threshold was not actually crossed
 *   - organizations_using_legacy_simulators: payer — chases a moving technical target with constrained resources
 *   - post_incident_investigators: analytical observer — the only party who can retrospectively locate where the threshold actually sat
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, 0.42).
domain_priors:suppression_score(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, 0.28).
domain_priors:theater_ratio(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, extractiveness, 0.42).
narrative_ontology:constraint_metric(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, suppression_requirement, 0.28).
narrative_ontology:constraint_metric(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, rope).
narrative_ontology:human_readable(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, "Simulation Fidelity Threshold for Catastrophe-Competence Retention").
narrative_ontology:topic_domain(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, "safety_engineering/organizational_learning").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, 'e9d28ad8-3431-4b8e-8b25-2c1ef64ca872').
narrative_ontology:cs_kernel_codification('e9d28ad8-3431-4b8e-8b25-2c1ef64ca872', distributed).
narrative_ontology:cs_authority_grounding('e9d28ad8-3431-4b8e-8b25-2c1ef64ca872', expertise).
narrative_ontology:cs_interpretation_layer_present('e9d28ad8-3431-4b8e-8b25-2c1ef64ca872').
narrative_ontology:cs_reading_relation('e9d28ad8-3431-4b8e-8b25-2c1ef64ca872', catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, influences).
narrative_ontology:cs_reading_relation('e9d28ad8-3431-4b8e-8b25-2c1ef64ca872', catastrophe_proxy_sufficiency__catastrophe_necessity_reading, coexists_with).
narrative_ontology:cs_reading_relation('e9d28ad8-3431-4b8e-8b25-2c1ef64ca872', catastrophe_proxy_sufficiency__hybrid_degradation_reading, coexists_with).
narrative_ontology:cs_axiom('e9d28ad8-3431-4b8e-8b25-2c1ef64ca872', foundational, sufficiency_is_technology_gated_not_categorical).
narrative_ontology:cs_axiom_status(sufficiency_is_technology_gated_not_categorical, holdable).
narrative_ontology:cs_axiom_grounding('e9d28ad8-3431-4b8e-8b25-2c1ef64ca872', sufficiency_is_technology_gated_not_categorical, empirically_contingent).
narrative_ontology:cs_axiom('e9d28ad8-3431-4b8e-8b25-2c1ef64ca872', secondary, threshold_crossing_is_binary_once_achieved).
narrative_ontology:cs_axiom_status(threshold_crossing_is_binary_once_achieved, holdable).
narrative_ontology:cs_axiom_grounding('e9d28ad8-3431-4b8e-8b25-2c1ef64ca872', threshold_crossing_is_binary_once_achieved, empirically_contingent).
narrative_ontology:cs_reference_frame('e9d28ad8-3431-4b8e-8b25-2c1ef64ca872', technology_gated_sufficiency_standard).
narrative_ontology:cs_drift_state('e9d28ad8-3431-4b8e-8b25-2c1ef64ca872', contemporary_high_fidelity_simulation_era, gap(practice_drift, minor, true)).
narrative_ontology:cs_created_at('e9d28ad8-3431-4b8e-8b25-2c1ef64ca872', '').
narrative_ontology:cs_kernel_id(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, catastrophe_proxy_sufficiency).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, simulation_technology_vendors).
narrative_ontology:constraint_beneficiary(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, high_reliability_organizations).
narrative_ontology:constraint_beneficiary(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, certifying_regulators).
narrative_ontology:constraint_victim(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, frontline_operators_undertrained).
narrative_ontology:constraint_victim(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, organizations_using_legacy_simulators).
narrative_ontology:constraint_vindicates(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, competence_is_technology_dependent_not_categorical).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Design and sell high-fidelity simulators (full-motion flight decks, nuclear control-room replicas, VR-based crisis simulators) whose value proposition depends on the claim that crossing a measurable fidelity threshold substitutes for real catastrophic experience. Revenue scales with organizations' willingness to invest in ever-higher-fidelity hardware and scenario design. Benefits directly from the threshold framing because it converts an open-ended competence question into a purchasable technical upgrade.
narrative_ontology:constraint_stakeholder(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, simulation_technology_vendors, beneficiary,
    organized, biographical, arbitrage, global).

% Nuclear plant operators, airlines, aircraft carriers, and similar organizations set internal training standards and decide how much to invest in simulator fidelity. They benefit from a coordination mechanism that lets them certify competence without waiting for or causing a real disaster, but they bear the ongoing cost of chasing the fidelity threshold as technology and threat models evolve.
narrative_ontology:constraint_stakeholder(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, high_reliability_organizations, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, high_reliability_organizations, beneficiary).

% Aviation, nuclear, and maritime regulators write certification requirements that reference simulator fidelity standards. They benefit from a defensible, auditable proxy for competence that does not require inducing real catastrophes, and their institutional legitimacy rests on the threshold being real and measurable rather than a matter of institutional judgment.
narrative_ontology:constraint_stakeholder(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, certifying_regulators, beneficiary,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, certifying_regulators, agenda_setter).

% Pilots, control-room operators, and emergency responders trained on simulators that have not actually crossed the fidelity threshold (because their organization could not afford the newest hardware, or because the threshold for their specific catastrophe type has not been technically achieved) bear the risk of a false sense of readiness. They cannot verify from the inside whether their training crossed the threshold or merely approximated it; their exit option in a live crisis is nonexistent.
narrative_ontology:constraint_stakeholder(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, frontline_operators_undertrained, payer,
    moderate, immediate, trapped, local).

% Smaller or resource-constrained operators (regional airlines, smaller utilities) run simulator fleets one or two technology generations behind the frontier. Under the threshold framing, their training investment may sit below the sufficiency line without their knowledge, since the threshold itself is only visible in retrospect or through incident analysis. They pay the ongoing capital cost of chasing a moving technical target.
narrative_ontology:constraint_stakeholder(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, organizations_using_legacy_simulators, payer,
    moderate, biographical, constrained, national).

% Accident investigation boards (NTSB, IAEA review panels) retrospectively assess whether simulator training was sufficient given what a real event actually demanded. They are the primary evidentiary source for locating where the fidelity threshold sits for a given catastrophe class, but their findings arrive only after a failure has already occurred.
narrative_ontology:constraint_stakeholder(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, post_incident_investigators, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the genuine problem that organizations cannot ethically or practically induce real catastrophes to train personnel, by establishing that a sufficiently high-fidelity simulated stressor can substitute for the genuine article — allowing competence to be built, certified, and maintained without waiting for or causing disaster.
% TRANSFER_FUNCTION: Moves capital from organizations and regulators toward simulation technology vendors, in exchange for training capacity; moves risk from organizations (which avoid inducing real catastrophes) onto frontline operators and the public who bear the consequences if the purchased fidelity level turns out to be below the true sufficiency threshold for their catastrophe class.
% ABSENT_VOICES: Future incident victims and the operators who will face a real catastrophe under threshold-uncertain training are structurally absent from the fidelity-adequacy conversation — they cannot testify to whether the threshold was actually crossed until after an event has already tested it, by which point the finding arrives too late for them.
% DISAPPEARANCE_RATIONALE: If the fidelity-threshold framework disappeared, organizations would lose the technical/legal cover for claiming trained competence without real catastrophic experience; regulators would need an alternative certification basis (return to raw hours, apprenticeship lineage, or explicit acknowledgment of irreducible uncertainty); simulation vendors would lose the specific value proposition of threshold-crossing hardware, though simulation itself would likely continue in a less totalizing role.
% FOUNDING_PROBLEM: Organizations operating catastrophic-risk systems (nuclear, aviation, naval) needed a way to build and verify operator competence for events too costly, dangerous, or rare to practice on directly.
% FOUNDING_PROBLEM_CORROBORATION: Independent accident investigation boards (e.g., post-Three Mile Island NRC reviews, NTSB simulator-adequacy findings after specific crashes) corroborate that the underlying training gap is real and persistent — but the same investigators have also found, in specific cases, that the fidelity threshold claimed by the operating organization and its simulator vendor was not in fact crossed, meaning the threshold framing's technical sufficiency claim is independently contested even though the founding problem itself is not in dispute.
narrative_ontology:disappearance_verdict(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, world_rearranges).
narrative_ontology:founding_problem_status(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, 'none', 1).
narrative_ontology:epsilon_provenance(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(catastrophe_proxy_sufficiency__simulation_fidelity_threshold_tests).
:- end_tests(catastrophe_proxy_sufficiency__simulation_fidelity_threshold_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.42) rather than high: this reading describes a genuine, technically-grounded coordination mechanism, not a pure rent-extraction scheme — but it is not zero, because the threshold's technology-dependence creates an ongoing capital-extraction relationship between vendors/well-resourced organizations and resource-constrained organizations/frontline operators who cannot independently verify whether their specific training program cleared the threshold. Suppression is comparatively low (0.28) because there is no active coercive suppression of alternatives — organizations can and do choose different training philosophies — but resistance (0.4) reflects real professional debate (pilot unions, control-room operator associations) about whether specific simulator programs have actually crossed sufficiency. Theater ratio is low-moderate and rises slowly (0.12→0.22) reflecting the honest risk that fidelity certification becomes partly performative (checking boxes on a certification standard) even where genuine technical improvement continues underneath.
 *
 * DIRECTIONALITY LOGIC:
 *   Simulation vendors sit closest to the beneficiary end: they profit from the technology-dependence framing regardless of whether any given organization's training actually clears the threshold, since sales are driven by the perception of a moving target requiring continuous upgrade. High-reliability organizations and regulators are secondary beneficiaries — they get a workable, auditable certification story — but they also bear real cost pressure to keep pace. Frontline operators and resource-constrained organizations sit closer to the target end: they cannot verify threshold-crossing from the inside and bear the downside risk if the claimed threshold was not actually met, a risk that only becomes visible after a real incident.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (competence-building for events too dangerous to practice on directly) remains fully live — this is not a case of an arrangement outliving its function. What differentiates this reading from mandatrophy is precisely the technology-dependent sufficiency claim: because the threshold is not fixed, the constraint's function does not decay into pure theater as long as the underlying technology continues to genuinely improve and organizations continue to genuinely invest. The risk case (theater ratio creeping upward) is where certification becomes decoupled from actual fidelity improvement — a certifying body signs off on 'crossing the threshold' as a compliance ritual rather than a technically verified fact. That drift is tracked but has not yet dominated in the authored metrics.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    threshold_measurability,
    'Is the fidelity threshold at which simulation becomes catastrophe-equivalent for competence retention actually measurable in advance, or only inferable retrospectively from incident outcomes?',
    'Systematic comparison of pre-incident simulator fidelity specifications against post-incident investigation findings on whether trained response matched real-event demands, across a large sample of catastrophic events and near-misses.',
    'If the threshold is only inferable retrospectively, the sufficiency claim cannot function as prospective certification, which would push this reading toward the hybrid_degradation_reading or even the catastrophe_necessity_reading — since a threshold no one can verify in advance is functionally equivalent to an unverifiable proxy.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(threshold_measurability, empirical, 'Whether fidelity sufficiency can be certified ex ante or only diagnosed ex post.').

omega_variable(
    kernel_reading_disagreement_locus,
    'Where exactly does this reading''s claim diverge from the sibling readings — is it the existence of a threshold at all, or only its technology-dependence?',
    'This is a conceptual/genealogical question rather than an empirical one: it depends on how strictly ''threshold'' is defined relative to the sibling claims (categorical sufficiency, categorical insufficiency, and generational degradation) and is not resolvable by additional data alone.',
    'If the sibling readings are read charitably as also technology-sensitive at the margins, the distinctiveness of this reading narrows to the specific claim that sufficiency is binary once the threshold is crossed (rather than continuously improving), which is the load-bearing structural claim this story authors.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_disagreement_locus, conceptual, 'Locating the precise structural disagreement between this reading and its siblings within the catastrophe_proxy_sufficiency kernel.').

omega_variable(
    vendor_incentive_to_declare_threshold_crossed,
    'Do simulation technology vendors and organizations that purchased their equipment have an incentive to declare the fidelity threshold crossed before it actually has been, given that certification and sales both depend on the claim?',
    'Audit of independent (non-vendor-affiliated) technical assessments of specific simulator programs against vendor and purchasing-organization public claims of threshold-crossing sufficiency.',
    'If such an incentive is confirmed and acted upon, the extractiveness score authored here (0.42) is likely understated, and the constraint would drift toward tangled_rope as genuine coordination (competence building) mixes with asymmetric extraction (frontline operators bearing risk from prematurely declared sufficiency).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(vendor_incentive_to_declare_threshold_crossed, empirical, 'Whether declared threshold-crossing tracks actual technical sufficiency or vendor/organizational incentive.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cata_tr_t0, catastrophe_proxy_sufficiency__simulation_fidelity_threshold, theater_ratio, 0, 0.12).
narrative_ontology:measurement(cata_tr_t8, catastrophe_proxy_sufficiency__simulation_fidelity_threshold, theater_ratio, 8, 0.14).
narrative_ontology:measurement(cata_tr_t16, catastrophe_proxy_sufficiency__simulation_fidelity_threshold, theater_ratio, 16, 0.17).
narrative_ontology:measurement(cata_tr_t24, catastrophe_proxy_sufficiency__simulation_fidelity_threshold, theater_ratio, 24, 0.19).
narrative_ontology:measurement(cata_tr_t32, catastrophe_proxy_sufficiency__simulation_fidelity_threshold, theater_ratio, 32, 0.21).
narrative_ontology:measurement(cata_tr_t40, catastrophe_proxy_sufficiency__simulation_fidelity_threshold, theater_ratio, 40, 0.22).

% Extraction over time
narrative_ontology:measurement(cata_be_t0, catastrophe_proxy_sufficiency__simulation_fidelity_threshold, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(cata_be_t8, catastrophe_proxy_sufficiency__simulation_fidelity_threshold, base_extractiveness, 8, 0.33).
narrative_ontology:measurement(cata_be_t16, catastrophe_proxy_sufficiency__simulation_fidelity_threshold, base_extractiveness, 16, 0.36).
narrative_ontology:measurement(cata_be_t24, catastrophe_proxy_sufficiency__simulation_fidelity_threshold, base_extractiveness, 24, 0.39).
narrative_ontology:measurement(cata_be_t32, catastrophe_proxy_sufficiency__simulation_fidelity_threshold, base_extractiveness, 32, 0.41).
narrative_ontology:measurement(cata_be_t40, catastrophe_proxy_sufficiency__simulation_fidelity_threshold, base_extractiveness, 40, 0.42).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, resource_allocation).
narrative_ontology:boltzmann_floor_override(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, 0.12).
narrative_ontology:affects_constraint(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, simulation_as_proxy_catastrophe_reading).
narrative_ontology:affects_constraint(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, catastrophe_necessity_reading).
narrative_ontology:affects_constraint(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, hybrid_degradation_reading).

% DUAL FORMULATION NOTE:
% This story is one of four readings of the catastrophe_proxy_sufficiency kernel. simulation_as_proxy_catastrophe_reading claims current simulation is already categorically sufficient (a stronger, more Rope-like claim with less extraction); catastrophe_necessity_reading claims simulation is categorically insufficient regardless of technology (implying the entire training apparatus is theater — a Piton-leaning reading); hybrid_degradation_reading locates the failure in generational tacit-knowledge decay rather than a technical threshold (a slower-acting, harder-to-detect Tangled-Rope-leaning reading). This reading (simulation_fidelity_threshold) occupies the middle ground: sufficiency is real and achievable but conditional on technology investment, producing a genuine but bounded coordination function with a real, technology-gated extractive edge for under-resourced organizations and frontline operators who cannot verify threshold-crossing directly.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
