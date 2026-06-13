% ============================================================================
% CONSTRAINT STORY: neutron_star_bombardment_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_neutron_star_bombardment_reading, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: neutron_star_bombardment_reading
 *   human_readable: Neutron Star Companion Bombardment Actinide Production
 *   domain: astrophysics/stellar_spectroscopy/nuclear_physics
 *
 * SUMMARY:
 *   This constraint instantiates one reading of the contested actinide
 *   replenishment kernel: that observed actinide abundances in certain
 *   stellar atmospheres result from bombardment by high-energy particle winds
 *   from neutron star companions. The mechanism requires a binary system
 *   where the neutron star's relativistic wind delivers neutrons and
 *   energetic particles to the stellar atmosphere, driving spallation
 *   reactions and neutron capture that produce heavy actinides. This reading
 *   makes specific structural predictions: radial velocity variations
 *   indicating binary motion, X-ray emission from the neutron star, and
 *   particle wind signatures correlated with actinide abundance. Sibling
 *   readings propose alternative mechanisms (superheavy element decay,
 *   artifact disposal) that do not require binary companions and make
 *   different observational predictions. The constraint is claimed as
 *   mountain (a physical mechanism that operates when the structural
 *   conditions exist) but declares beneficiaries because research programs
 *   studying neutron star binaries gain explanatory scope if this mechanism
 *   is validated, triggering FSM evaluation.
 *
 * KEY AGENTS:
 *   - binary_system_theorists: organized/mobile — benefit from expanded explanatory scope for neutron star companions
 *   - high_energy_astrophysics_programs: institutional/mobile — benefit from additional diagnostic for compact objects
 *   - superheavy_element_researchers: organized/mobile — excluded from this reading's framework; advocate alternative mechanism
 *   - observational_spectroscopists: organized/analytical — measure the observables that discriminate between readings
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(neutron_star_bombardment_reading, 0.12).
domain_priors:suppression_score(neutron_star_bombardment_reading, 0.08).
domain_priors:theater_ratio(neutron_star_bombardment_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(neutron_star_bombardment_reading, extractiveness, 0.12).
narrative_ontology:constraint_metric(neutron_star_bombardment_reading, suppression_requirement, 0.08).
narrative_ontology:constraint_metric(neutron_star_bombardment_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(neutron_star_bombardment_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(neutron_star_bombardment_reading, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(neutron_star_bombardment_reading, mountain).
narrative_ontology:human_readable(neutron_star_bombardment_reading, "Neutron Star Companion Bombardment Actinide Production").
narrative_ontology:topic_domain(neutron_star_bombardment_reading, "astrophysics/stellar_spectroscopy/nuclear_physics").

domain_priors:emerges_naturally(neutron_star_bombardment_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(neutron_star_bombardment_reading, 'c4469b82-e0ab-4eae-bd46-8474df2a8b72').
narrative_ontology:cs_kernel_codification('c4469b82-e0ab-4eae-bd46-8474df2a8b72', distributed).
narrative_ontology:cs_authority_grounding('c4469b82-e0ab-4eae-bd46-8474df2a8b72', expertise).
narrative_ontology:cs_interpretation_layer_present('c4469b82-e0ab-4eae-bd46-8474df2a8b72').
narrative_ontology:cs_reading_relation('c4469b82-e0ab-4eae-bd46-8474df2a8b72', neutron_star_bombardment_reading__superheavy_decay_reading, influences).
narrative_ontology:cs_reading_relation('c4469b82-e0ab-4eae-bd46-8474df2a8b72', neutron_star_bombardment_reading__artifact_disposal_reading, coexists_with).
narrative_ontology:cs_axiom('c4469b82-e0ab-4eae-bd46-8474df2a8b72', foundational, binary_companion_necessity).
narrative_ontology:cs_axiom_status(binary_companion_necessity, holdable).
narrative_ontology:cs_axiom_grounding('c4469b82-e0ab-4eae-bd46-8474df2a8b72', binary_companion_necessity, empirically_contingent).
narrative_ontology:cs_axiom('c4469b82-e0ab-4eae-bd46-8474df2a8b72', secondary, high_energy_wind_sufficiency).
narrative_ontology:cs_axiom_status(high_energy_wind_sufficiency, holdable).
narrative_ontology:cs_axiom_grounding('c4469b82-e0ab-4eae-bd46-8474df2a8b72', high_energy_wind_sufficiency, empirically_contingent).
narrative_ontology:cs_reference_frame('c4469b82-e0ab-4eae-bd46-8474df2a8b72', standard_nucleosynthesis_framework).
narrative_ontology:cs_drift_state('c4469b82-e0ab-4eae-bd46-8474df2a8b72', post_actinide_detection_era, gap(axiom_overriding, substantial, true)).
narrative_ontology:cs_created_at('c4469b82-e0ab-4eae-bd46-8474df2a8b72', '').
narrative_ontology:cs_kernel_id(neutron_star_bombardment_reading, actinide_replenishment_mechanism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(neutron_star_bombardment_reading, binary_system_theorists).
narrative_ontology:constraint_beneficiary(neutron_star_bombardment_reading, high_energy_astrophysics_programs).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Research programs focused on neutron star binary systems gain explanatory scope if this mechanism operates: the actinide signature becomes a diagnostic for neutron star companions even when the companion is not directly detected. Their models predict correlated observables (radial velocity variation, X-ray emission, particle wind signatures) that can be tested.
narrative_ontology:constraint_stakeholder(neutron_star_bombardment_reading, binary_system_theorists, beneficiary,
    organized, biographical, mobile, global).

% Observational programs studying high-energy phenomena in stellar systems benefit from an additional observable consequence of neutron star winds. If bombardment produces detectable actinide signatures, it expands the diagnostic toolkit for characterizing compact object companions and validates investment in high-resolution spectroscopy.
narrative_ontology:constraint_stakeholder(neutron_star_bombardment_reading, high_energy_astrophysics_programs, beneficiary,
    institutional, generational, mobile, global).

% Research programs investigating superheavy element decay chains as the actinide source are structurally excluded from this reading's framework. They would argue that the bombardment mechanism requires fine-tuned binary parameters and that decay from primordial superheavy elements is a simpler explanation not requiring companion detection.
narrative_ontology:constraint_stakeholder(neutron_star_bombardment_reading, superheavy_element_researchers, excluded,
    organized, biographical, mobile, global).

% Measure actinide abundances, radial velocities, and search for correlated X-ray emission. Their observations constrain which mechanism operates: bombardment predicts time-variable signatures and binary motion; other mechanisms do not. They can falsify this reading by demonstrating actinide-rich stars lack binary companions or particle wind signatures.
narrative_ontology:constraint_stakeholder(neutron_star_bombardment_reading, observational_spectroscopists, observer,
    organized, biographical, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: None — this is a proposed physical mechanism, not a coordination arrangement. The constraint describes a natural process by which neutron star winds transmute stellar atmospheric material.
% TRANSFER_FUNCTION: High-energy particles and neutrons transfer from the neutron star wind to the stellar atmosphere, where nuclear reactions produce actinides. No economic or social transfer occurs.
% ABSENT_VOICES: Proponents of alternative actinide production mechanisms (superheavy decay, artifact disposal) are excluded from this reading's framework by construction. They would argue the bombardment scenario requires improbable binary configurations and that simpler mechanisms explain the observations.
% DISAPPEARANCE_RATIONALE: If this mechanism were shown not to operate, the actinide observations would remain and would require explanation by alternative mechanisms. The physical universe does not rearrange itself around human theories; only the explanatory framework changes.
% FOUNDING_PROBLEM: Observed actinide abundances in certain stellar atmospheres exceed predictions from standard nucleosynthesis and require an external production mechanism.
% FOUNDING_PROBLEM_CORROBORATION: The actinide abundance anomaly is documented in peer-reviewed spectroscopic observations independent of any particular explanation. Multiple research groups across different institutions have confirmed the measurements. The problem's existence is not contested; only the mechanism is.
narrative_ontology:disappearance_verdict(neutron_star_bombardment_reading, world_unchanged).
narrative_ontology:founding_problem_status(neutron_star_bombardment_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(neutron_star_bombardment_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'unspecified', 'agent/example_platform_commission.json',
    'claude-sonnet-4-5-20250929', 'unspecified').
narrative_ontology:story_seed(neutron_star_bombardment_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(neutron_star_bombardment_reading_tests).

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(neutron_star_bombardment_reading, ExtMetricName, E),
    domain_priors:suppression_score(neutron_star_bombardment_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(neutron_star_bombardment_reading),
    narrative_ontology:constraint_metric(neutron_star_bombardment_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(neutron_star_bombardment_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(neutron_star_bombardment_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.12) because the mechanism, if it operates, is a physical process with minimal institutional overhead — the extraction reflects the modest career investment in defending this particular reading against alternatives. Suppression is very low (0.08) because alternative explanations remain viable and no institutional machinery prevents their investigation. Theater ratio is low (0.15) because most research activity is directed at genuine observational tests rather than defending the framework. Accessibility collapse is moderately high (0.72) because IF a star has a neutron star companion with the right parameters, bombardment follows from known nuclear physics; but the IF is a substantial empirical gate. Resistance is moderate (0.35) because competing research programs advocate different mechanisms and the observational evidence does not yet decisively favor one reading. The temporal trajectory shows declining extraction and theater as observational constraints accumulate and the mechanism either gains empirical support or is ruled out by null results.
 *
 * PERSPECTIVAL GAP:
 *   From the beneficiary seats (binary theorists, high-energy programs), this constraint appears as a genuine physical mechanism that expands the diagnostic toolkit for neutron star companions — a mountain that validates their research direction. From the excluded seat (superheavy researchers), the same structure appears as an unnecessarily complex explanation that requires fine-tuned binary parameters when simpler mechanisms exist. From the analytical seat (spectroscopists), it is one testable hypothesis among several, distinguished by its specific observational predictions. The engine computes these divergences from the structural data; the claimed type (mountain) reflects the physical-mechanism framing, while the metrics capture the modest institutional extraction from career investment in this particular reading.
 *
 * DIRECTIONALITY LOGIC:
 *   Binary system theorists and high-energy astrophysics programs are beneficiaries (d near 0.2-0.3): their research programs gain scope if this mechanism operates, but they can pivot to other phenomena if it does not. Superheavy element researchers are excluded rather than victimized — their alternative reading is structurally incompatible with this one, but no suppression prevents them from pursuing it. Observational spectroscopists are analytical (d = 0.5): they measure the discriminating observables without commitment to any particular mechanism.
 *
 * MANDATROPHY ANALYSIS:
 *   This is not a mandatrophy case. The constraint describes a proposed physical mechanism, not a coordination arrangement whose function has been superseded. The founding problem (actinide abundance anomaly) remains live, and this reading is one of several competing explanations. If observational evidence rules out bombardment, the constraint dissolves rather than persisting as a zombie institution.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    binary_fraction_constraint,
    'What fraction of actinide-rich stars are in binary systems with neutron star companions?',
    'Systematic radial velocity surveys of actinide-rich stars to detect binary motion, combined with X-ray observations to confirm neutron star companions.',
    'If most actinide-rich stars lack neutron star companions, the bombardment mechanism cannot be the dominant source and extraction from this reading increases (it becomes a special case rather than a general explanation). If most are in appropriate binaries, the mechanism gains support and extraction decreases.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(binary_fraction_constraint, empirical, 'Whether the required binary configuration is common enough to explain the observed actinide population.').

omega_variable(
    production_rate_sufficiency,
    'Can neutron star wind bombardment produce actinides at the observed abundances given realistic binary parameters and neutron star spin-down luminosities?',
    'Detailed nuclear reaction network calculations incorporating measured neutron capture cross-sections and spallation yields, constrained by observed neutron star wind properties.',
    'If calculated production rates fall short of observed abundances by orders of magnitude, the mechanism is ruled out regardless of binary fraction. If rates match, the mechanism remains viable and extraction from defending it decreases.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(production_rate_sufficiency, empirical, 'Whether the proposed nuclear physics can quantitatively account for the observations.').

omega_variable(
    mechanism_exclusivity,
    'Are the bombardment, superheavy decay, and artifact disposal mechanisms mutually exclusive, or could multiple mechanisms operate in different stellar populations?',
    'Conceptual analysis of whether the mechanisms make incompatible predictions for the same stars, or whether they could operate in different contexts (e.g., bombardment in binaries, decay in isolated stars, artifacts in specific spatial regions).',
    'If mechanisms are mutually exclusive, confirming one reading forecloses the others and increases extraction from the losing readings. If they can coexist, multiple readings can be simultaneously correct for different populations, reducing extraction from all readings.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mechanism_exclusivity, conceptual, 'Whether validating one reading necessarily invalidates the siblings, or whether the kernel decomposes into context-dependent sub-kernels.').

omega_variable(
    natural_vs_constructed_ambiguity,
    'Is this constraint a genuine physical mechanism (mountain) or a constructed theoretical framework that benefits specific research programs (false summit)?',
    'FSM evaluation: if the mechanism operates, it is a physical process independent of who studies it; if observational tests consistently fail to find the predicted signatures while the framework persists due to institutional investment, it is a false summit.',
    'Determines whether the declared beneficiaries (binary theorists, high-energy programs) are incidental to a natural process or are structurally positioned to extract career value from defending a framework that does not correspond to physical reality.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_vs_constructed_ambiguity, empirical, 'Whether the constraint describes a real physical mechanism or a theoretical construct sustained by research program investment.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(neutron_star_bombardment_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(neut_tr_t0, neutron_star_bombardment_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement_basis(neut_tr_t0, observed).
narrative_ontology:measurement(neut_tr_t10, neutron_star_bombardment_reading, theater_ratio, 10, 0.2).
narrative_ontology:measurement_basis(neut_tr_t10, observed).
narrative_ontology:measurement(neut_tr_t20, neutron_star_bombardment_reading, theater_ratio, 20, 0.18).
narrative_ontology:measurement_basis(neut_tr_t20, observed).
narrative_ontology:measurement(neut_tr_t30, neutron_star_bombardment_reading, theater_ratio, 30, 0.17).
narrative_ontology:measurement_basis(neut_tr_t30, observed).
narrative_ontology:measurement(neut_tr_t40, neutron_star_bombardment_reading, theater_ratio, 40, 0.16).
narrative_ontology:measurement_basis(neut_tr_t40, observed).
narrative_ontology:measurement(neut_tr_t50, neutron_star_bombardment_reading, theater_ratio, 50, 0.15).
narrative_ontology:measurement_basis(neut_tr_t50, observed).

% Extraction over time
narrative_ontology:measurement(neut_be_t0, neutron_star_bombardment_reading, base_extractiveness, 0, 0.18).
narrative_ontology:measurement_basis(neut_be_t0, observed).
narrative_ontology:measurement(neut_be_t10, neutron_star_bombardment_reading, base_extractiveness, 10, 0.16).
narrative_ontology:measurement_basis(neut_be_t10, observed).
narrative_ontology:measurement(neut_be_t20, neutron_star_bombardment_reading, base_extractiveness, 20, 0.14).
narrative_ontology:measurement_basis(neut_be_t20, observed).
narrative_ontology:measurement(neut_be_t30, neutron_star_bombardment_reading, base_extractiveness, 30, 0.13).
narrative_ontology:measurement_basis(neut_be_t30, observed).
narrative_ontology:measurement(neut_be_t40, neutron_star_bombardment_reading, base_extractiveness, 40, 0.12).
narrative_ontology:measurement_basis(neut_be_t40, observed).
narrative_ontology:measurement(neut_be_t50, neutron_star_bombardment_reading, base_extractiveness, 50, 0.12).
narrative_ontology:measurement_basis(neut_be_t50, observed).

% Suppression requirement over time
narrative_ontology:measurement(neut_su_t0, neutron_star_bombardment_reading, suppression_requirement, 0, 0.12).
narrative_ontology:measurement_basis(neut_su_t0, observed).
narrative_ontology:measurement(neut_su_t10, neutron_star_bombardment_reading, suppression_requirement, 10, 0.11).
narrative_ontology:measurement_basis(neut_su_t10, observed).
narrative_ontology:measurement(neut_su_t20, neutron_star_bombardment_reading, suppression_requirement, 20, 0.1).
narrative_ontology:measurement_basis(neut_su_t20, observed).
narrative_ontology:measurement(neut_su_t30, neutron_star_bombardment_reading, suppression_requirement, 30, 0.09).
narrative_ontology:measurement_basis(neut_su_t30, observed).
narrative_ontology:measurement(neut_su_t40, neutron_star_bombardment_reading, suppression_requirement, 40, 0.08).
narrative_ontology:measurement_basis(neut_su_t40, observed).
narrative_ontology:measurement(neut_su_t50, neutron_star_bombardment_reading, suppression_requirement, 50, 0.08).
narrative_ontology:measurement_basis(neut_su_t50, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(neutron_star_bombardment_reading, information_standard).
narrative_ontology:affects_constraint(neutron_star_bombardment_reading, superheavy_decay_reading).
narrative_ontology:affects_constraint(neutron_star_bombardment_reading, artifact_disposal_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the actinide_replenishment_mechanism kernel. The kernel decomposes into three structural readings with different observational predictions: neutron_star_bombardment_reading (this file) requires binary systems and predicts correlated high-energy signatures; superheavy_decay_reading requires primordial superheavy elements and predicts no binary motion; artifact_disposal_reading requires technological actors and predicts spatial clustering. The readings are linked via network.affects_constraints because confirming one mechanism in a stellar population constrains where the others must operate. All three readings must be evaluated against the same observational data (actinide abundances, radial velocities, X-ray emission, spatial distribution) to determine which mechanism dominates in which contexts.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
