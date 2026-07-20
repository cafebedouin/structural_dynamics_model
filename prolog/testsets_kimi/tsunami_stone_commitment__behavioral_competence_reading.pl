% ============================================================================
% CONSTRAINT STORY: tsunami_stone_commitment__behavioral_competence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_tsunami_stone_commitment__behavioral_competence_reading, []).

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
    narrative_ontology:suppression_profile/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
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
 *   constraint_id: tsunami_stone_commitment__behavioral_competence_reading
 *   human_readable: Tsunami Stone Inscription: Live Behavioral Competence Reading
 *   domain: disaster_anthropology/commitment_system/institutional_memory
 *
 * SUMMARY:
 *   This constraint instantiates the behavioral_competence_reading of the
 *   tsunami_stone_commitment kernel: the claim that a tsunami warning stone
 *   inscription retained live behavioral force through active
 *   intergenerational norm enforcement rather than decaying into symbolic
 *   heritage. In coastal communities marked by such stones, elders transmit
 *   an ancestral prohibition against building below the stone's elevation,
 *   and compliance is maintained through identity-bound social obligation
 *   rather than through state enforcement or material incentive. The sibling
 *   commemorative_husk_reading asserts the opposite empirical claim â that
 *   compliance is coincidental or weakly enforced and the stone is now a
 *   commemorative artifact. The catastrophe_validation_axis treats the 2011
 *   tsunami as a binary empirical test of the stone's efficacy. This story
 *   authors the behavioral reading as a piton: the founding warning function
 *   has been superseded by modern seismic monitoring and broadcast systems,
 *   yet the inscription persists through inertial, identity-locked
 *   intergenerational transmission with no concentrated beneficiary capturing
 *   rents from its operation.
 *
 * KEY AGENTS:
 *   - Intergenerational lineage keepers (organized/identity_locked): agenda_setters who maintain oral tradition and mild social vigilance around the stone, without extracting material benefit.
 *   - Coastal community residents (moderate/identity_locked): payers who bear the diffuse cost of restricted coastal land use and comply through fused communal identity.
 *   - Modern disaster agencies (institutional/analytical): observers whose operational infrastructure has superseded the stone's original function.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(tsunami_stone_commitment__behavioral_competence_reading, 0.12).
domain_priors:suppression_score(tsunami_stone_commitment__behavioral_competence_reading, 0.22).
domain_priors:theater_ratio(tsunami_stone_commitment__behavioral_competence_reading, 0.72).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(tsunami_stone_commitment__behavioral_competence_reading, extractiveness, 0.12).
narrative_ontology:constraint_metric(tsunami_stone_commitment__behavioral_competence_reading, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(tsunami_stone_commitment__behavioral_competence_reading, theater_ratio, 0.72).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(tsunami_stone_commitment__behavioral_competence_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(tsunami_stone_commitment__behavioral_competence_reading, resistance, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(tsunami_stone_commitment__behavioral_competence_reading, piton).
narrative_ontology:human_readable(tsunami_stone_commitment__behavioral_competence_reading, "Tsunami Stone Inscription: Live Behavioral Competence Reading").
narrative_ontology:topic_domain(tsunami_stone_commitment__behavioral_competence_reading, "disaster_anthropology/commitment_system/institutional_memory").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(tsunami_stone_commitment__behavioral_competence_reading, '880edd61-68d9-476f-867a-3d6a86a28a29').
narrative_ontology:cs_kernel_codification('880edd61-68d9-476f-867a-3d6a86a28a29', fixed_text).
narrative_ontology:cs_authority_grounding('880edd61-68d9-476f-867a-3d6a86a28a29', lineage).
narrative_ontology:cs_interpretation_layer_present('880edd61-68d9-476f-867a-3d6a86a28a29').
narrative_ontology:cs_reading_relation('880edd61-68d9-476f-867a-3d6a86a28a29', tsunami_stone_commitment__commemorative_husk_reading, forecloses).
narrative_ontology:cs_reading_relation('880edd61-68d9-476f-867a-3d6a86a28a29', tsunami_stone_commitment__catastrophe_validation_axis, coexists_with).
narrative_ontology:cs_axiom('880edd61-68d9-476f-867a-3d6a86a28a29', foundational, ancestral_inscription_is_live_prescription).
narrative_ontology:cs_axiom_status(ancestral_inscription_is_live_prescription, holdable).
narrative_ontology:cs_axiom_grounding('880edd61-68d9-476f-867a-3d6a86a28a29', ancestral_inscription_is_live_prescription, conventional).
narrative_ontology:cs_axiom('880edd61-68d9-476f-867a-3d6a86a28a29', foundational, intergenerational_survival_competence).
narrative_ontology:cs_axiom_status(intergenerational_survival_competence, holdable).
narrative_ontology:cs_axiom_grounding('880edd61-68d9-476f-867a-3d6a86a28a29', intergenerational_survival_competence, instrumental).
narrative_ontology:cs_reference_frame('880edd61-68d9-476f-867a-3d6a86a28a29', live_prescriptive_competence).
narrative_ontology:cs_drift_state('880edd61-68d9-476f-867a-3d6a86a28a29', post_modern_warning_systems_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('880edd61-68d9-476f-867a-3d6a86a28a29', '').
narrative_ontology:cs_kernel_id(tsunami_stone_commitment__behavioral_competence_reading, tsunami_stone_commitment).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(tsunami_stone_commitment__behavioral_competence_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(tsunami_stone_commitment__behavioral_competence_reading, 'none', 1).
narrative_ontology:epsilon_provenance(tsunami_stone_commitment__behavioral_competence_reading, 0.12, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(tsunami_stone_commitment__behavioral_competence_reading_tests).
:- end_tests(tsunami_stone_commitment__behavioral_competence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is very low (0.12) because no agent captures a concentrated rent; the constraint extracts only diffuse compliance costs from residents. Suppression is low-moderate (0.22) because enforcement relies on identity-bound social obligation rather than on violent coercion or structural barriers. Theater ratio is high (0.72) because the constraint's persistence is increasingly performative: modern warning systems handle the original protective function, while intergenerational transmission maintains the norm as identity theater. Resistance is very low (0.15) because the identity-lock makes exit invisible and opposition unthinkable within the community frame. The metrics are authored independently of the claimed piton type; the engine measures the divergence.
 *
 * PERSPECTIVAL GAP:
 *   Lineage keepers experience the constraint as live cultural duty and ancestral trust; the engine from their seat may compute a rope-like coordination reading because they see genuine intergenerational value. Residents experience it as an identity-locked restriction on economic opportunity; from their seat the engine computes a low-extraction piton because the costs are diffuse and exit is fused to self-concept. Modern agencies see only a superseded cultural artifact; their analytical seat computes near-zero extraction. The divergence is structural, not erroneous.
 *
 * DIRECTIONALITY LOGIC:
 *   No beneficiaries are declared, consistent with the 'no extractive beneficiary structure' authoring instruction. The lineage keepers are agenda_setters but do not materially benefit; their authority is identity-bound. Residents are payers with identity_locked exit, which structurally orients them toward the target side of directionality. Because base extractiveness is near the floor, effective extraction remains negligible despite the residents' positional orientation. The absence of a beneficiary is the structural signal that differentiates this piton from a snare.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem â tsunami mortality in the absence of centralized warning infrastructure â is dead. Modern seismic networks and broadcast evacuation orders solve it more reliably than a stone marker. The constraint persists not because its original function is irreplaceable, but because intergenerational identity maintenance has become self-sustaining. This is mandatrophy resolved: the mandate outlived its problem. Classifying it as piton rather than rope prevents the error of attributing current persistence to current coordination need; classifying it as piton rather than snare prevents the error of inventing a beneficiary who does not exist.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    behavioral_vs_symbolic_status,
    'Does the stone inscription currently govern behavior through active norm enforcement, or has it decayed to a commemorative symbol whose compliance is incidental?',
    'Ethnographic observation of community decision-making around coastal development, evacuation drills, and intergenerational transmission practices.',
    'If symbolic, reclassification toward commemorative_husk_reading or piton with higher theater_ratio; if behavioral, support for this reading''s claim of live competence.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(behavioral_vs_symbolic_status, empirical, 'Core kernel contest between live behavioral force and symbolic decay').

omega_variable(
    functional_redundancy_modern_systems,
    'Is the stone''s persistence driven by non-redundant protective function or by inertial identity maintenance now that modern warning systems exist?',
    'Comparative analysis of tsunami mortality and land-use outcomes between stone-observant communities and demographically matched non-observant communities with equivalent modern infrastructure.',
    'If redundant with modern systems, confirms piton classification; if it provides non-redundant protection, shifts classification toward rope or scaffold.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(functional_redundancy_modern_systems, empirical, 'Whether the constraint persists by inertia or by irreplaceable function').

omega_variable(
    suppression_mechanism_intergenerational,
    'Is norm compliance maintained by structural social sanctions or by internalized identity fusion with ancestral tradition?',
    'Post-exit trajectory study: observe whether young people who leave the community continue to heed the stone''s prescription or abandon it once structural sanctions are removed.',
    'If internalized, effective suppression exceeds the structural measure; if purely structural, constraint weakens with mobility.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_intergenerational, empirical, 'Structural versus internalized suppression in intergenerational norm enforcement').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(tsunami_stone_commitment__behavioral_competence_reading, 0, 120).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tsun_tr_t0, tsunami_stone_commitment__behavioral_competence_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(tsun_tr_t30, tsunami_stone_commitment__behavioral_competence_reading, theater_ratio, 30, 0.38).
narrative_ontology:measurement(tsun_tr_t60, tsunami_stone_commitment__behavioral_competence_reading, theater_ratio, 60, 0.52).
narrative_ontology:measurement(tsun_tr_t90, tsunami_stone_commitment__behavioral_competence_reading, theater_ratio, 90, 0.64).
narrative_ontology:measurement(tsun_tr_t120, tsunami_stone_commitment__behavioral_competence_reading, theater_ratio, 120, 0.72).

% Extraction over time
narrative_ontology:measurement(tsun_be_t0, tsunami_stone_commitment__behavioral_competence_reading, base_extractiveness, 0, 0.08).
narrative_ontology:measurement(tsun_be_t30, tsunami_stone_commitment__behavioral_competence_reading, base_extractiveness, 30, 0.09).
narrative_ontology:measurement(tsun_be_t60, tsunami_stone_commitment__behavioral_competence_reading, base_extractiveness, 60, 0.1).
narrative_ontology:measurement(tsun_be_t90, tsunami_stone_commitment__behavioral_competence_reading, base_extractiveness, 90, 0.11).
narrative_ontology:measurement(tsun_be_t120, tsunami_stone_commitment__behavioral_competence_reading, base_extractiveness, 120, 0.12).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(tsunami_stone_commitment__behavioral_competence_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(tsunami_stone_commitment__behavioral_competence_reading, identity_coordination).
narrative_ontology:affects_constraint(tsunami_stone_commitment__behavioral_competence_reading, commemorative_husk_reading).
narrative_ontology:affects_constraint(tsunami_stone_commitment__behavioral_competence_reading, catastrophe_validation_axis).

% DUAL FORMULATION NOTE:
% The tsunami_stone_commitment kernel decomposes into three structurally distinct readings: behavioral_competence_reading (live norm enforcement, low Îµ), commemorative_husk_reading (symbolic decay, high theater), and catastrophe_validation_axis (empirical test frame, epistemic). Each reading instantiates a different constraint with its own Îµ, stakeholders, and classification. This reading asserts active behavioral competence; the husk reading asserts coincidental compliance; the validation axis treats the 2011 tsunami as decisive evidence.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
