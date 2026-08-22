% ============================================================================
% CONSTRAINT STORY: unclos_maritime_sovereignty__hybrid_effective_control_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_unclos_maritime_sovereignty__hybrid_effective_control_reading, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
 *   constraint_id: unclos_maritime_sovereignty__hybrid_effective_control_reading
 *   human_readable: UNCLOS Hybrid Effective Control Maritime Sovereignty Reading
 *   domain: international_law/maritime_governance/geopolitical_strategy
 *
 * SUMMARY:
 *   This constraint instantiates a contested reading of the UNCLOS maritime
 *   sovereignty kernel that assigns graduated legal status to maritime
 *   features: naturally formed land features generate full territorial seas
 *   and exclusive economic zones, while artificial installations begin with a
 *   500-meter safety zone under Article 60 but may mature into territorial
 *   claims if the constructing state maintains prolonged effective control
 *   and other claimants fail to challenge the occupation. The reading sits
 *   between strict geographic formalism and expansive immediate-occupation
 *   doctrines. It functions as coordination by supplying a rule-like
 *   framework for maritime expansion that avoids immediate military
 *   confrontation, but it operates as extraction by systematically favoring
 *   states with artificial island construction capacity and sustained naval
 *   power projection over militarily weaker coastal claimants. The
 *   arrangement is actively enforced through maintenance of physical
 *   installations, patrols, administrative declarations, and diplomatic
 *   suppression of challenges.
 *
 * KEY AGENTS:
 *   - constructing_coastal_states: Primary beneficiary (institutional/global) â captures maritime space through construction and control.
 *   - weaker_maritime_claimants: Primary payer (moderate/regional) â loses access to waters around matured artificial claims.
 *   - international_tribunals: Analytical observer (institutional/global) â interprets UNCLOS, often contradicting hybrid reading.
 *   - affected_fishing_communities: Excluded (powerless/local) â livelihoods impacted but absent from state-centric legal framework.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(unclos_maritime_sovereignty__hybrid_effective_control_reading, 0.55).
domain_priors:suppression_score(unclos_maritime_sovereignty__hybrid_effective_control_reading, 0.6).
domain_priors:theater_ratio(unclos_maritime_sovereignty__hybrid_effective_control_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(unclos_maritime_sovereignty__hybrid_effective_control_reading, extractiveness, 0.55).
narrative_ontology:constraint_metric(unclos_maritime_sovereignty__hybrid_effective_control_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(unclos_maritime_sovereignty__hybrid_effective_control_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(unclos_maritime_sovereignty__hybrid_effective_control_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(unclos_maritime_sovereignty__hybrid_effective_control_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(unclos_maritime_sovereignty__hybrid_effective_control_reading, tangled_rope).
narrative_ontology:human_readable(unclos_maritime_sovereignty__hybrid_effective_control_reading, "UNCLOS Hybrid Effective Control Maritime Sovereignty Reading").
narrative_ontology:topic_domain(unclos_maritime_sovereignty__hybrid_effective_control_reading, "international_law/maritime_governance/geopolitical_strategy").

domain_priors:requires_active_enforcement(unclos_maritime_sovereignty__hybrid_effective_control_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(unclos_maritime_sovereignty__hybrid_effective_control_reading, 'b99ac230-c5d4-4aeb-8462-920b67189e4f').
narrative_ontology:cs_kernel_codification('b99ac230-c5d4-4aeb-8462-920b67189e4f', formalized).
narrative_ontology:cs_authority_grounding('b99ac230-c5d4-4aeb-8462-920b67189e4f', distributed).
narrative_ontology:cs_reading_relation('b99ac230-c5d4-4aeb-8462-920b67189e4f', unclos_maritime_sovereignty__strict_geographic_reading, coexists_with).
narrative_ontology:cs_reading_relation('b99ac230-c5d4-4aeb-8462-920b67189e4f', unclos_maritime_sovereignty__expansive_construction_reading, coexists_with).
narrative_ontology:cs_axiom('b99ac230-c5d4-4aeb-8462-920b67189e4f', foundational, feature_origin_gradation).
narrative_ontology:cs_axiom_status(feature_origin_gradation, holdable).
narrative_ontology:cs_axiom_grounding('b99ac230-c5d4-4aeb-8462-920b67189e4f', feature_origin_gradation, conventional).
narrative_ontology:cs_axiom('b99ac230-c5d4-4aeb-8462-920b67189e4f', foundational, control_ripening_doctrine).
narrative_ontology:cs_axiom_status(control_ripening_doctrine, holdable).
narrative_ontology:cs_axiom_grounding('b99ac230-c5d4-4aeb-8462-920b67189e4f', control_ripening_doctrine, instrumental).
narrative_ontology:cs_reference_frame('b99ac230-c5d4-4aeb-8462-920b67189e4f', graduated_maritime_order).
narrative_ontology:cs_drift_state('b99ac230-c5d4-4aeb-8462-920b67189e4f', post_south_china_sea_arbitration, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('b99ac230-c5d4-4aeb-8462-920b67189e4f', '').
narrative_ontology:cs_kernel_id(unclos_maritime_sovereignty__hybrid_effective_control_reading, unclos_maritime_sovereignty).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(unclos_maritime_sovereignty__hybrid_effective_control_reading, constructing_coastal_states).
narrative_ontology:constraint_victim(unclos_maritime_sovereignty__hybrid_effective_control_reading, weaker_maritime_claimants).
narrative_ontology:constraint_vindicates(unclos_maritime_sovereignty__hybrid_effective_control_reading, effective_control_doctrine).
narrative_ontology:constraint_vindicates(unclos_maritime_sovereignty__hybrid_effective_control_reading, graduated_maritime_entitlement).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% States with the engineering capacity and naval power to construct artificial islands on low-tide elevations or submerged features, maintain permanent garrisons and administrative apparatus, and defend these installations against external challenge. They exploit a legal pathway that allows limited initial status under UNCLOS Article 60 to ripen into full territorial claims over time, thereby expanding exclusive resource jurisdiction without immediate military conquest.
narrative_ontology:constraint_stakeholder(unclos_maritime_sovereignty__hybrid_effective_control_reading, constructing_coastal_states, beneficiary,
    institutional, generational, mobile, global).

% Coastal states with historic or geographic claims to maritime features in contested regions, lacking the capital and power-projection capacity to establish or maintain artificial installations. They bear the cost of having surrounding waters and seabed resources gradually enclosed by competitor states' matured artificial-feature claims, with practical recourse limited to protracted, expensive international litigation that major constructing states may ignore.
narrative_ontology:constraint_stakeholder(unclos_maritime_sovereignty__hybrid_effective_control_reading, weaker_maritime_claimants, payer,
    moderate, generational, constrained, regional).

% Arbitral bodies and the International Court of Justice tasked with interpreting UNCLOS provisions on the regime of islands and artificial installations. They have issued rulingsânotably the 2016 South China Sea Arbitrationâthat reject the maturation doctrine for artificial features, creating direct tension with the state practice that advances the hybrid effective-control pathway.
narrative_ontology:constraint_stakeholder(unclos_maritime_sovereignty__hybrid_effective_control_reading, international_tribunals, observer,
    institutional, civilizational, analytical, global).

% Artisanal fishers and indigenous coastal communities whose traditional fishing grounds and livelihoods overlap with contested maritime features. They are structurally excluded from the state-centric legal framework that parcels maritime space among sovereign claimants and treats their economic dependence on open access as secondary to territorial sovereignty.
narrative_ontology:constraint_stakeholder(unclos_maritime_sovereignty__hybrid_effective_control_reading, affected_fishing_communities, excluded,
    powerless, biographical, trapped, local).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(unclos_maritime_sovereignty__hybrid_effective_control_reading, constructing_coastal_states).
narrative_ontology:fixing_cost_class(unclos_maritime_sovereignty__hybrid_effective_control_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a graduated legal framework for allocating sovereignty over maritime features, differentiating entitlements by natural versus artificial origin and by duration of effective control, in order to reduce the risk of immediate armed conflict over ambiguous or newly constructed features.
% TRANSFER_FUNCTION: Moves exclusive maritime resource rightsâfisheries, hydrocarbon access, and strategic waterwaysâfrom non-constructing claimant states to states that build artificial installations and maintain prolonged uncontested control over them.
% ABSENT_VOICES: Artisanal fishing communities, indigenous coastal peoples, and non-claimant littoral states whose economic security depends on open maritime access are excluded from the state-centric framework; they would contest the enclosure of traditionally open waters but lack standing in the sovereign-claimant discourse.
% DISAPPEARANCE_RATIONALE: If the hybrid effective-control framework vanished, artificial features would revert to generating no territorial entitlements beyond 500-meter safety zones; construction-intensive state expansion would lose its primary legal pathway to maritime enclosure; and the default would revert to strict geographic or pure power-based occupation without the legitimizing intermediate stage.
% FOUNDING_PROBLEM: Post-World War II technological advances enabled offshore claims far beyond historic limits, while decolonization produced a surge of new coastal states seeking resource jurisdiction; the hybrid reading attempted to balance geographic fidelity with pragmatic recognition of state practice and effective administration.
% FOUNDING_PROBLEM_CORROBORATION: UNCLOS III negotiating histories record contestation over Article 121 and artificial features, corroborating that no consensus resolved the issue. The 2016 South China Sea Arbitration and subsequent academic legal analysis from outside the constructing-state bloc corroborate that the hybrid reading remains contested and is not the authoritative interpretation; constructing states cite their own practice as corroboration, but no independent external seat validates the hybrid reading as the intended solution.
narrative_ontology:disappearance_verdict(unclos_maritime_sovereignty__hybrid_effective_control_reading, world_rearranges).
narrative_ontology:founding_problem_status(unclos_maritime_sovereignty__hybrid_effective_control_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(unclos_maritime_sovereignty__hybrid_effective_control_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(unclos_maritime_sovereignty__hybrid_effective_control_reading, 'none', 1).
narrative_ontology:epsilon_provenance(unclos_maritime_sovereignty__hybrid_effective_control_reading, 0.55, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(unclos_maritime_sovereignty__hybrid_effective_control_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(unclos_maritime_sovereignty__hybrid_effective_control_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(unclos_maritime_sovereignty__hybrid_effective_control_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.55 because the hybrid reading creates a reproducible mechanism by which resource-rich maritime space is transferred from non-constructing to constructing states under color of law. Suppression at 0.60 reflects the combination of physical enforcement (naval patrols, installation maintenance) and legal-diplomatic suppression of challenges through cost and deterrence. Theater ratio at 0.40 captures the performative dimension: much artificial island construction includes symbolic infrastructure (airstrips, monuments, administrative plaques) designed to signal sovereignty to international audiences rather than serve local economic needs. Accessibility collapse at 0.50 indicates that while legal alternatives (arbitration) exist, they are slow, costly, and often ignored by powerful constructing states. Resistance at 0.55 reflects active but largely ineffective legal and diplomatic pushback by weaker claimants and occasional tribunal rulings.
 *
 * PERSPECTIVAL GAP:
 *   From the constructing state's seat, the hybrid reading is necessary pragmatic coordination: rigid geographic formalism would freeze resource claims in an unjust colonial-era pattern, and effective control should matter. From the weaker claimant's seat, the same rule is extraction dressed as law: the 'maturation' period is simply the time required for the powerful state to consolidate a fait accompli. The engine computes this divergence from the structural asymmetry in power, exit options, and the beneficiary-victim mapping.
 *
 * DIRECTIONALITY LOGIC:
 *   Constructing coastal states are structural beneficiaries: they collect expanded EEZ and territorial sea equivalents through a low-cost construction pathway, giving them directionality near the beneficiary end. Weaker maritime claimants are structural payers: they bear the loss of traditional fishing grounds, hydrocarbon prospects, and strategic waterways without commensurate gain, giving them directionality near the target end. International tribunals sit at analytical scope with neutral directionality. Fishing communities are excluded entirely, lacking standing in the state-centric framework.
 *
 * MANDATROPHY ANALYSIS:
 *   The hybrid reading risks mandatrophy if its original purpose was to provide a temporary equitable adjustment during decolonization and technological transition. Today, it functions as a permanent subsidy to construction-intensive state power. The founding problemâhow to allocate maritime space fairly as technology enabled offshore claimsâhas not been solved by this reading; instead, the reading has become a vehicle for asymmetric enclosure. Its persistence depends on active enforcement (naval presence, diplomatic suppression) rather than participant preference among weaker claimants, distinguishing it from pure coordination.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_artificial_boundary_stability,
    'Is the natural versus artificial feature distinction in maritime law a stable ontological category, or does advancing geoengineering technology collapse it into a constructed administrative label?',
    'Comparative analysis of state practice and tribunal rulings as geoengineering capacity spreads beyond the current major-power constructors; if small states begin altering features cheaply, the category boundary destabilizes.',
    'If the boundary collapses, the hybrid reading''s coordination rationale evaporates and the constraint reveals itself as a pure power-based extraction mechanism; if stable, the graduated framework retains structural legitimacy.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_artificial_boundary_stability, conceptual, 'Stability of the natural-artificial distinction under technological change').

omega_variable(
    effective_control_legitimacy,
    'Does prolonged effective control over an artificial feature generate customary international law entitlement, or is it merely the retroactive legalization of faits accomplis achieved through military asymmetry?',
    'Longitudinal study of international tribunal recognition: if hybrid-matured claims are progressively recognized or acquiesced to by third states, a customary norm is forming; if tribunals consistently reject them, the legitimacy remains contested.',
    'If recognized, the hybrid reading hardens into enforceable international law; if rejected, it remains an extractive arrangement dependent on raw power rather than legal authority.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(effective_control_legitimacy, empirical, 'Whether effective control over artificial features generates customary legal entitlement').

omega_variable(
    absence_of_challenge_structural,
    'Is the absence of challenge by weaker claimants evidence of genuine legal acquiescence, or a structural artifact of military and economic asymmetry that produces no valid consent?',
    'Diplomatic history review combined with power-asymmetry metrics: if weaker claimants file formal protests but lack capacity to enforce them, the absence is structural, not consensual.',
    'If structural, the hybrid reading''s maturation mechanism rests on coercion rather than tacit agreement, raising suppression and extraction metrics; if consensual, the reading reflects a more benign coordination function.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(absence_of_challenge_structural, empirical, 'Structural versus consensual basis for absent challenge').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(unclos_maritime_sovereignty__hybrid_effective_control_reading, 0, 42).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(uncl_tr_t0, unclos_maritime_sovereignty__hybrid_effective_control_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(uncl_tr_t14, unclos_maritime_sovereignty__hybrid_effective_control_reading, theater_ratio, 14, 0.2).
narrative_ontology:measurement(uncl_tr_t21, unclos_maritime_sovereignty__hybrid_effective_control_reading, theater_ratio, 21, 0.28).
narrative_ontology:measurement(uncl_tr_t28, unclos_maritime_sovereignty__hybrid_effective_control_reading, theater_ratio, 28, 0.35).
narrative_ontology:measurement(uncl_tr_t35, unclos_maritime_sovereignty__hybrid_effective_control_reading, theater_ratio, 35, 0.42).
narrative_ontology:measurement(uncl_tr_t42, unclos_maritime_sovereignty__hybrid_effective_control_reading, theater_ratio, 42, 0.4).

% Extraction over time
narrative_ontology:measurement(uncl_be_t0, unclos_maritime_sovereignty__hybrid_effective_control_reading, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(uncl_be_t14, unclos_maritime_sovereignty__hybrid_effective_control_reading, base_extractiveness, 14, 0.32).
narrative_ontology:measurement(uncl_be_t21, unclos_maritime_sovereignty__hybrid_effective_control_reading, base_extractiveness, 21, 0.38).
narrative_ontology:measurement(uncl_be_t28, unclos_maritime_sovereignty__hybrid_effective_control_reading, base_extractiveness, 28, 0.46).
narrative_ontology:measurement(uncl_be_t35, unclos_maritime_sovereignty__hybrid_effective_control_reading, base_extractiveness, 35, 0.52).
narrative_ontology:measurement(uncl_be_t42, unclos_maritime_sovereignty__hybrid_effective_control_reading, base_extractiveness, 42, 0.55).

% Suppression requirement over time
narrative_ontology:measurement(uncl_su_t0, unclos_maritime_sovereignty__hybrid_effective_control_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(uncl_su_t14, unclos_maritime_sovereignty__hybrid_effective_control_reading, suppression_requirement, 14, 0.35).
narrative_ontology:measurement(uncl_su_t21, unclos_maritime_sovereignty__hybrid_effective_control_reading, suppression_requirement, 21, 0.42).
narrative_ontology:measurement(uncl_su_t28, unclos_maritime_sovereignty__hybrid_effective_control_reading, suppression_requirement, 28, 0.52).
narrative_ontology:measurement(uncl_su_t35, unclos_maritime_sovereignty__hybrid_effective_control_reading, suppression_requirement, 35, 0.6).
narrative_ontology:measurement(uncl_su_t42, unclos_maritime_sovereignty__hybrid_effective_control_reading, suppression_requirement, 42, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(unclos_maritime_sovereignty__hybrid_effective_control_reading, unclos_maritime_sovereignty__strict_geographic_reading).
narrative_ontology:affects_constraint(unclos_maritime_sovereignty__hybrid_effective_control_reading, unclos_maritime_sovereignty__expansive_construction_reading).

% DUAL FORMULATION NOTE:
% This constraint is one member of the unclos_maritime_sovereignty constraint family. The hybrid_effective_control_reading decomposes from the colloquial label 'UNCLOS maritime sovereignty' because its epsilon, beneficiary structure, and legal mechanics differ structurally from the strict geographic and expansive construction readings. Each reading has distinct stakeholders, distinct metrics, and distinct classification trajectories.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
