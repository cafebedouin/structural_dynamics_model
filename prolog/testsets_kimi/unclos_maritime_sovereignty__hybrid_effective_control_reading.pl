% ============================================================================
% CONSTRAINT STORY: unclos_maritime_sovereignty__hybrid_effective_control_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
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
 *   constraint_id: unclos_maritime_sovereignty__hybrid_effective_control_reading
 *   human_readable: UNCLOS Maritime Sovereignty â Hybrid Effective Control Reading
 *   domain: international_law/maritime_governance/geopolitical_strategy
 *
 * SUMMARY:
 *   This constraint instantiates the hybrid effective control reading of the
 *   UNCLOS maritime sovereignty kernel. Under this reading, naturally formed
 *   features generate full territorial sea and EEZ entitlement, while
 *   artificial features generate only a 500-meter safety zoneâbut may
 *   mature into broader territorial claims through prolonged effective
 *   control absent challenge. The reading creates a graduated sovereignty
 *   regime that benefits states with the engineering and naval capacity to
 *   construct and defend artificial installations, while militarily weaker
 *   claimants bear the costs of eroded access and compressed maritime
 *   boundaries. The arrangement coordinates safety and order around
 *   installations but simultaneously extracts maritime space from the commons
 *   and rival claimants through a time-dependent maturation mechanism.
 *
 * KEY AGENTS:
 *   - states_with_construction_capacity: Primary agenda-setter and beneficiary (institutional/global) â builds, patrols, and administers artificial features; captures expanded maritime entitlement.
 *   - militarily_weaker_claimants: Primary payer (organized/regional) â contests claims but lacks construction/enforcement capacity; bears access costs.
 *   - international_tribunals: Analytical observer (institutional/global) â interprets UNCLOS but lacks enforcement leverage.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(unclos_maritime_sovereignty__hybrid_effective_control_reading, 0.62).
domain_priors:suppression_score(unclos_maritime_sovereignty__hybrid_effective_control_reading, 0.58).
domain_priors:theater_ratio(unclos_maritime_sovereignty__hybrid_effective_control_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(unclos_maritime_sovereignty__hybrid_effective_control_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(unclos_maritime_sovereignty__hybrid_effective_control_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(unclos_maritime_sovereignty__hybrid_effective_control_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(unclos_maritime_sovereignty__hybrid_effective_control_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(unclos_maritime_sovereignty__hybrid_effective_control_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(unclos_maritime_sovereignty__hybrid_effective_control_reading, tangled_rope).
narrative_ontology:human_readable(unclos_maritime_sovereignty__hybrid_effective_control_reading, "UNCLOS Maritime Sovereignty â Hybrid Effective Control Reading").
narrative_ontology:topic_domain(unclos_maritime_sovereignty__hybrid_effective_control_reading, "international_law/maritime_governance/geopolitical_strategy").

domain_priors:requires_active_enforcement(unclos_maritime_sovereignty__hybrid_effective_control_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(unclos_maritime_sovereignty__hybrid_effective_control_reading, '962b5a4d-5ab2-4bb5-aa57-594d6cd1b7b4').
narrative_ontology:cs_kernel_codification('962b5a4d-5ab2-4bb5-aa57-594d6cd1b7b4', formalized).
narrative_ontology:cs_authority_grounding('962b5a4d-5ab2-4bb5-aa57-594d6cd1b7b4', lineage).
narrative_ontology:cs_interpretation_layer_present('962b5a4d-5ab2-4bb5-aa57-594d6cd1b7b4').
narrative_ontology:cs_reading_relation('962b5a4d-5ab2-4bb5-aa57-594d6cd1b7b4', unclos_maritime_sovereignty__strict_geographic_reading, forecloses).
narrative_ontology:cs_reading_relation('962b5a4d-5ab2-4bb5-aa57-594d6cd1b7b4', unclos_maritime_sovereignty__expansive_construction_reading, coexists_with).
narrative_ontology:cs_axiom('962b5a4d-5ab2-4bb5-aa57-594d6cd1b7b4', foundational, graduated_sovereignty_by_feature_type).
narrative_ontology:cs_axiom_status(graduated_sovereignty_by_feature_type, holdable).
narrative_ontology:cs_axiom_grounding('962b5a4d-5ab2-4bb5-aa57-594d6cd1b7b4', graduated_sovereignty_by_feature_type, conventional).
narrative_ontology:cs_axiom('962b5a4d-5ab2-4bb5-aa57-594d6cd1b7b4', foundational, effective_control_matures_title).
narrative_ontology:cs_axiom_status(effective_control_matures_title, holdable).
narrative_ontology:cs_axiom_grounding('962b5a4d-5ab2-4bb5-aa57-594d6cd1b7b4', effective_control_matures_title, instrumental).
narrative_ontology:cs_reference_frame('962b5a4d-5ab2-4bb5-aa57-594d6cd1b7b4', unclos_graduated_entitlement_framework).
narrative_ontology:cs_drift_state('962b5a4d-5ab2-4bb5-aa57-594d6cd1b7b4', post_south_china_sea_arbitration_2016, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('962b5a4d-5ab2-4bb5-aa57-594d6cd1b7b4', '').
narrative_ontology:cs_kernel_id(unclos_maritime_sovereignty__hybrid_effective_control_reading, unclos_maritime_sovereignty).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(unclos_maritime_sovereignty__hybrid_effective_control_reading, states_with_construction_capacity).
narrative_ontology:constraint_victim(unclos_maritime_sovereignty__hybrid_effective_control_reading, militarily_weaker_claimants).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Construct artificial installations on submerged features and low-tide elevations; patrol surrounding waters; administer 500-meter safety zones; assert that prolonged effective control matures limited zones into full territorial sea and EEZ entitlements. They collect strategic maritime positioning, resource access, and expanded sovereign jurisdiction.
narrative_ontology:constraint_stakeholder(unclos_maritime_sovereignty__hybrid_effective_control_reading, states_with_construction_capacity, agenda_setter,
    institutional, generational, mobile, global).
narrative_ontology:stakeholder_secondary_role(unclos_maritime_sovereignty__hybrid_effective_control_reading, states_with_construction_capacity, beneficiary).

% Contest the maritime claims of construction-capable states but lack the naval and engineering capacity to build competing artificial features or sustain prolonged enforcement. They bear the cost of eroded maritime access, lost fisheries, and compressed EEZ boundaries, relying on slow international legal challenges that lack coercive enforcement.
narrative_ontology:constraint_stakeholder(unclos_maritime_sovereignty__hybrid_effective_control_reading, militarily_weaker_claimants, payer,
    organized, generational, constrained, regional).

% Interpret UNCLOS provisions concerning artificial installations and maritime entitlement; issue binding and advisory rulings that attempt to delimit the legal effect of artificial features, but possess no independent enforcement capacity against non-compliant major powers.
narrative_ontology:constraint_stakeholder(unclos_maritime_sovereignty__hybrid_effective_control_reading, international_tribunals, observer,
    institutional, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Regulates artificial installations in maritime zones by establishing safety perimeters, preventing collisions, and providing a doctrinal framework that attempts to balance economic use with territorial stability.
% TRANSFER_FUNCTION: Moves maritime spatial entitlement from the international commons and weaker claimant zones to construction-capable states over time, beginning with a 500-meter safety zone and potentially maturing into full territorial sea and EEZ through prolonged effective control.
% ABSENT_VOICES: Small island developing states without naval power, indigenous maritime communities, and non-state ocean users are largely excluded from the effective-control narrative despite being most affected by the territorialization of former commons.
% DISAPPEARANCE_RATIONALE: If the hybrid effective-control framework vanished, construction-capable states would lose the doctrinal pathway to convert artificial installations into sovereign maritime zones; rival claimants would contest the 500-meter safety zones and any matured claims; regional maritime order would revert to stricter geographic or purely negotiated delimitation.
% FOUNDING_PROBLEM: How to regulate the growing number of artificial installations in maritime zones without either forbidding them entirely (hindering economic activity) or allowing instant territorialization (encouraging aggressive unilateralism).
% FOUNDING_PROBLEM_CORROBORATION: The Permanent Court of Arbitration (Philippines v. China, 2016) and independent academic maritime law scholars attest that the hybrid framework has been instrumentalized for territorial expansion; no state entirely outside the regional dispute corroborates the hybrid reading as a neutral coordination mechanism.
narrative_ontology:disappearance_verdict(unclos_maritime_sovereignty__hybrid_effective_control_reading, world_rearranges).
narrative_ontology:founding_problem_status(unclos_maritime_sovereignty__hybrid_effective_control_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(unclos_maritime_sovereignty__hybrid_effective_control_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(unclos_maritime_sovereignty__hybrid_effective_control_reading, 'none', 1).
narrative_ontology:epsilon_provenance(unclos_maritime_sovereignty__hybrid_effective_control_reading, 0.62, 'kimi-k2.6', 'none', direct).

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
 *   Extractiveness (0.62) is intermediate because the constraint combines a real coordination function (500m safety zones preventing collision around installations) with a substantial transfer mechanism (maturation into territorial claims). Suppression (0.58) reflects the active naval patrols and administrative exclusion required to maintain effective control, without which the maturation pathway collapses. Theater ratio (0.40) captures the increasing performative dimension of patrols and ceremonies that assert sovereignty beyond the safety function. Accessibility collapse (0.45) is moderate: weaker claimants retain legal arbitration as an alternative, but its effectiveness is limited by enforcement gaps. Resistance (0.55) reflects persistent diplomatic and legal opposition from weaker claimants and adverse tribunal rulings.
 *
 * PERSPECTIVAL GAP:
 *   The construction-capable state seat experiences the constraint as a legitimate safety and administrative regime it maintains; the weaker claimant seat experiences the same patrols and zoning as coercive territorial compression. The tribunal seat sees a doctrinal framework drifting toward extraction. The engine computes this divergence from the structural dataâno single authored type resolves the dispute.
 *
 * DIRECTIONALITY LOGIC:
 *   states_with_construction_capacity are declared beneficiaries and agenda-setters: they collect sovereign jurisdiction and strategic positioning, giving them a beneficiary directionality (low d). militarily_weaker_claimants are declared victims/payers: they lose maritime access and jurisdictional space, giving them target directionality (high d). international_tribunals are observers with analytical exit, placing them at the neutral/analytical pole.
 *
 * MANDATROPHY ANALYSIS:
 *   Classifying this as a tangled rope prevents two errors: (1) reading the safety-zone coordination as a pure rope, which would ignore the asymmetric extraction of maturing territorial claims; and (2) reading the territorial expansion as a pure snare, which would ignore the genuine collision-prevention and installation-protection function. The R5 genealogy reveals a contested founding problemâregulating artificial installations without encouraging annexationâsuggesting the arrangement has not fully atrophied into a piton, though the rising theater ratio indicates some performative drift.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    feature_type_boundary_stability,
    'Does prolonged effective control erode the categorical distinction between natural and artificial features in maritime entitlement, rendering the hybrid graduation mechanism unstable?',
    'Comparative case law analysis tracking tribunal treatment of artificial features over time, and state practice documentation showing whether safety zones have consistently expanded into territorial claims.',
    'If the boundary collapses, the hybrid reading converges toward the expansive construction reading; if it holds, the reading remains structurally distinct as a tangled rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(feature_type_boundary_stability, conceptual, 'Stability of the natural/artificial feature distinction under prolonged control').

omega_variable(
    maturation_threshold_indeterminacy,
    'What duration and intensity of effective control is sufficient to mature a 500m safety zone into a territorial sea, and is that threshold adjudicable?',
    'Empirical survey of state claims and tribunal rulings to identify any emergent customary threshold; absence of such threshold would indicate the maturation clause is a standardless extraction pathway.',
    'A determinate threshold would constrain extraction and stabilize the coordination function; indeterminacy confirms the maturation mechanism as a discretionary expansion tool.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(maturation_threshold_indeterminacy, empirical, 'Whether the maturation timeline is legally determinate').

omega_variable(
    tribunal_enforcement_gap,
    'Is the constraint''s persistence explained by legal legitimacy or by the inability of international tribunals to enforce rulings against construction-capable states?',
    'Track compliance rates with adverse tribunal rulings on maritime entitlement and correlate with power asymmetry between the tribunal''s sponsor coalition and the non-compliant state.',
    'If persistence is primarily coercive power rather than legal authority, the constraint''s coordination story is cover for a snare; if legal authority retains independent force despite enforcement gaps, the tangled rope classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(tribunal_enforcement_gap, conceptual, 'Legitimacy versus power in constraint persistence').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(unclos_maritime_sovereignty__hybrid_effective_control_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(uncl_tr_t0, unclos_maritime_sovereignty__hybrid_effective_control_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(uncl_tr_t10, unclos_maritime_sovereignty__hybrid_effective_control_reading, theater_ratio, 10, 0.25).
narrative_ontology:measurement(uncl_tr_t20, unclos_maritime_sovereignty__hybrid_effective_control_reading, theater_ratio, 20, 0.3).
narrative_ontology:measurement(uncl_tr_t30, unclos_maritime_sovereignty__hybrid_effective_control_reading, theater_ratio, 30, 0.35).
narrative_ontology:measurement(uncl_tr_t40, unclos_maritime_sovereignty__hybrid_effective_control_reading, theater_ratio, 40, 0.4).

% Extraction over time
narrative_ontology:measurement(uncl_be_t0, unclos_maritime_sovereignty__hybrid_effective_control_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(uncl_be_t10, unclos_maritime_sovereignty__hybrid_effective_control_reading, base_extractiveness, 10, 0.52).
narrative_ontology:measurement(uncl_be_t20, unclos_maritime_sovereignty__hybrid_effective_control_reading, base_extractiveness, 20, 0.58).
narrative_ontology:measurement(uncl_be_t30, unclos_maritime_sovereignty__hybrid_effective_control_reading, base_extractiveness, 30, 0.6).
narrative_ontology:measurement(uncl_be_t40, unclos_maritime_sovereignty__hybrid_effective_control_reading, base_extractiveness, 40, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(uncl_su_t0, unclos_maritime_sovereignty__hybrid_effective_control_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(uncl_su_t10, unclos_maritime_sovereignty__hybrid_effective_control_reading, suppression_requirement, 10, 0.45).
narrative_ontology:measurement(uncl_su_t20, unclos_maritime_sovereignty__hybrid_effective_control_reading, suppression_requirement, 20, 0.5).
narrative_ontology:measurement(uncl_su_t30, unclos_maritime_sovereignty__hybrid_effective_control_reading, suppression_requirement, 30, 0.54).
narrative_ontology:measurement(uncl_su_t40, unclos_maritime_sovereignty__hybrid_effective_control_reading, suppression_requirement, 40, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(unclos_maritime_sovereignty__hybrid_effective_control_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(unclos_maritime_sovereignty__hybrid_effective_control_reading, strict_geographic_reading).
narrative_ontology:affects_constraint(unclos_maritime_sovereignty__hybrid_effective_control_reading, expansive_construction_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the unclos_maritime_sovereignty kernel. The strict_geographic_reading treats the kernel as a natural-law boundary (low Îµ). The expansive_construction_reading treats it as a permissive regime for instant territorialization (high Îµ). This hybrid reading instantiates an intermediate Îµ by combining feature-type graduation with a time-based maturation mechanism.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
