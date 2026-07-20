% ============================================================================
% CONSTRAINT STORY: tsunami_stone_commitment__catastrophe_validation_axis
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_tsunami_stone_commitment__catastrophe_validation_axis, []).

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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: tsunami_stone_commitment__catastrophe_validation_axis
 *   human_readable: 2011 Tsunami as Catastrophe Validation Axis
 *   domain: disaster_anthropology/commitment_systems
 *
 * SUMMARY:
 *   The 2011 TÅhoku tsunami inundated coastal regions of Japan, destroying
 *   communities below historical marker stones while sparing areas above
 *   them. This event is interpreted along the catastrophe_validation_axis as
 *   a decisive empirical test that provided binary validation for
 *   intergenerational stone-inscription commitments. The constraint is the
 *   physical inundation pattern itself, operating as a mountain-type
 *   adjudication device that resolves interpretive ambiguity between
 *   competing readings of the stone commitment kernel. As a physical event,
 *   it extracts from no one and requires no enforcement; its structural role
 *   is to serve as an irreducible empirical checkpoint for the commitment
 *   system's validity.
 *
 * KEY AGENTS:
 *   - Coastal communities above and below stone markers: Experienced differential survival outcomes based on topographic position relative to inscribed boundaries.
 *   - Disaster anthropologists and geomorphologists: Analytical observers who interpret the survival pattern as validation of institutional memory.
 *   - Intergenerational transmission networks: Lineage-based maintenance systems for stone legibility and placement.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(tsunami_stone_commitment__catastrophe_validation_axis, 0.02).
domain_priors:suppression_score(tsunami_stone_commitment__catastrophe_validation_axis, 0.01).
domain_priors:theater_ratio(tsunami_stone_commitment__catastrophe_validation_axis, 0.01).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(tsunami_stone_commitment__catastrophe_validation_axis, extractiveness, 0.02).
narrative_ontology:constraint_metric(tsunami_stone_commitment__catastrophe_validation_axis, suppression_requirement, 0.01).
narrative_ontology:constraint_metric(tsunami_stone_commitment__catastrophe_validation_axis, theater_ratio, 0.01).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(tsunami_stone_commitment__catastrophe_validation_axis, accessibility_collapse, 0.94).
narrative_ontology:constraint_metric(tsunami_stone_commitment__catastrophe_validation_axis, resistance, 0.01).

% --- Constraint claim ---
narrative_ontology:constraint_claim(tsunami_stone_commitment__catastrophe_validation_axis, mountain).
narrative_ontology:human_readable(tsunami_stone_commitment__catastrophe_validation_axis, "2011 Tsunami as Catastrophe Validation Axis").
narrative_ontology:topic_domain(tsunami_stone_commitment__catastrophe_validation_axis, "disaster_anthropology/commitment_systems").

domain_priors:emerges_naturally(tsunami_stone_commitment__catastrophe_validation_axis).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(tsunami_stone_commitment__catastrophe_validation_axis, '7b7aacf7-de50-4390-a374-960239f0db2c').
narrative_ontology:cs_kernel_codification('7b7aacf7-de50-4390-a374-960239f0db2c', fixed_text).
narrative_ontology:cs_authority_grounding('7b7aacf7-de50-4390-a374-960239f0db2c', lineage).
narrative_ontology:cs_interpretation_layer_present('7b7aacf7-de50-4390-a374-960239f0db2c').
narrative_ontology:cs_reading_relation('7b7aacf7-de50-4390-a374-960239f0db2c', tsunami_stone_commitment__behavioral_competence_reading, influences).
narrative_ontology:cs_reading_relation('7b7aacf7-de50-4390-a374-960239f0db2c', tsunami_stone_commitment__commemorative_husk_reading, influences).
narrative_ontology:cs_axiom('7b7aacf7-de50-4390-a374-960239f0db2c', foundational, catastrophic_adjudication_principle).
narrative_ontology:cs_axiom_status(catastrophic_adjudication_principle, holdable).
narrative_ontology:cs_axiom_grounding('7b7aacf7-de50-4390-a374-960239f0db2c', catastrophic_adjudication_principle, empirically_contingent).
narrative_ontology:cs_axiom('7b7aacf7-de50-4390-a374-960239f0db2c', foundational, binary_empirical_validation).
narrative_ontology:cs_axiom_status(binary_empirical_validation, holdable).
narrative_ontology:cs_axiom_grounding('7b7aacf7-de50-4390-a374-960239f0db2c', binary_empirical_validation, empirically_contingent).
narrative_ontology:cs_reference_frame('7b7aacf7-de50-4390-a374-960239f0db2c', intergenerational_warning_validity).
narrative_ontology:cs_drift_state('7b7aacf7-de50-4390-a374-960239f0db2c', post_2011_tsunami, gap(stable, minor, true)).
narrative_ontology:cs_created_at('7b7aacf7-de50-4390-a374-960239f0db2c', '').
narrative_ontology:cs_kernel_id(tsunami_stone_commitment__catastrophe_validation_axis, tsunami_stone_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_vindicates(tsunami_stone_commitment__catastrophe_validation_axis, long_term_warning_efficacy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: No coordination function; this constraint is a physical event whose inundation pattern provides binary empirical evidence for the validity of intergenerational territorial commitments inscribed in stone markers. It operates as a natural adjudication device rather than a social coordination mechanism.
% TRANSFER_FUNCTION: No transfer; the constraint does not move resources, status, or labor between agents. Its structural role is epistemic and adjudicative: it validates or falsifies interpretive claims about the stone commitment kernel.
% ABSENT_VOICES: Communities that had removed or relocated stone markers prior to 2011 and suffered complete devastation are absent from the post-hoc validation narrative; their experience would complicate the binary success/failure framing. Geologists who emphasize stochastic recurrence intervals rather than deterministic ancestral knowledge are marginalized in commemorative accounts.
% DISAPPEARANCE_RATIONALE: Without the 2011 tsunami's decisive empirical validation, the stone inscription commitment kernel would lack a natural adjudicator between the behavioral competence and commemorative husk readings. The hermeneutic equilibrium would remain contested, institutional memory would lack catastrophic anchoring, and disaster anthropology would lose its canonical empirical test case for intergenerational commitment validation.
% FOUNDING_PROBLEM: The problem of validating intergenerational territorial commitments across centuries when behavioral enforcement decays, institutional memory erodes, and written records may be lost or ignored.
% FOUNDING_PROBLEM_CORROBORATION: Disaster anthropologists and geomorphologists attest independently to the recurrent problem of warning decay across generations; paleotsunami researchers confirm the geological risk that the stones encoded. The corroboration comes from analytical and scientific seats outside any single protected community.
narrative_ontology:disappearance_verdict(tsunami_stone_commitment__catastrophe_validation_axis, world_rearranges).
narrative_ontology:founding_problem_status(tsunami_stone_commitment__catastrophe_validation_axis, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(tsunami_stone_commitment__catastrophe_validation_axis, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(tsunami_stone_commitment__catastrophe_validation_axis, 'none', 1).
narrative_ontology:epsilon_provenance(tsunami_stone_commitment__catastrophe_validation_axis, 0.02, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(tsunami_stone_commitment__catastrophe_validation_axis_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(tsunami_stone_commitment__catastrophe_validation_axis, ExtMetricName, E),
    domain_priors:suppression_score(tsunami_stone_commitment__catastrophe_validation_axis, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(tsunami_stone_commitment__catastrophe_validation_axis),
    narrative_ontology:constraint_metric(tsunami_stone_commitment__catastrophe_validation_axis, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(tsunami_stone_commitment__catastrophe_validation_axis, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(tsunami_stone_commitment__catastrophe_validation_axis_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint is authored as a mountain because the 2011 tsunami inundation pattern is an irreducible physical event. Its Îµ is near-zero because natural catastrophes do not extract; they operate indiscriminately. Suppression is near-zero because no agency enforces the tsunami. Accessibility collapse is very high (~0.94) because the physical evidence of destruction and survival patterns leaves little interpretive ambiguity once mapped. Resistance is near-zero because physical events meet no social resistance. The theater ratio is negligible: there is no performative maintenance of a tsunami.
 *
 * PERSPECTIVAL GAP:
 *   There is minimal perspectival gap because the constraint is a physical event. Analytical observers and affected communities see the same inundation pattern. The only divergence is interpretive: some communities may read the event as ancestral wisdom validated, while others read it as stochastic geography. But the constraint itself â the physical inundation boundary â is observer-invariant.
 *
 * DIRECTIONALITY LOGIC:
 *   No directionality derivation is needed: the constraint declares no beneficiaries or victims. The physical tsunami does not subsidize or extract from specific agents in its role as validation mechanism. Communities above the marker lines experienced preservation; those below experienced destruction. But this is not a directional extraction relationship â it is a uniform physical exposure differentiated only by topographic position relative to the stone markers.
 *
 * MANDATROPHY ANALYSIS:
 *   The mountain classification prevents mislabeling the tsunami validation axis as a social construct or extractive institution. While the framing of the event as 'validation' is a human interpretation, the underlying constraint (the inundation pattern) is physical. Mandatrophy does not apply because there is no mandate that has outlived its function; the constraint is a singular historical event whose structural force is episodic, not institutional.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    causal_attribution_to_stones,
    'Does the 2011 survival pattern validate the specific stone inscriptions as causal protectors, or does it merely correlate with elevation and geography that would have protected regardless of the stones?',
    'Cross-reference inundation maps with stone locations against pure elevation models; compare survival rates in communities with and without stones at equivalent elevations.',
    'If the stones added no protective information beyond topography, the constraint is a spurious correlation rather than a structurally sound validation mechanism; if they added information, the validation axis holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(causal_attribution_to_stones, empirical, 'Whether the tsunami validation effect is causal or correlational.').

omega_variable(
    binary_test_vs_graduated_risk,
    'Does a single catastrophic event provide sufficient binary validation, or does it obscure graduated risk profiles that the stone system may misrepresent?',
    'Paleotsunami studies and recurrence interval analysis to determine whether the 2011 event was representative or an outlier relative to the stones'' implied warnings.',
    'If the 2011 event was an outlier, the binary validation axis overstates the empirical support for the stone commitments; if representative, the validation holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(binary_test_vs_graduated_risk, empirical, 'Whether single-event validation generalizes to the commitment system''s empirical basis.').

omega_variable(
    kernel_reading_contestation,
    'Does the 2011 tsunami validate the stone commitment kernel itself, or does it merely validate the original inscription while leaving the intergenerational transmission mechanism contested?',
    'Comparative analysis of communities with and without maintained stone traditions at equivalent elevations; if unmaintained stones also predicted survival, the validation applies to the original inscription alone.',
    'If validation is limited to the original inscription, the catastrophe_validation_axis reading supports the commemorative_husk reading''s claim that ongoing behavioral enforcement was weak; if maintained stones outperformed unmaintained ones, the behavioral_competence reading is strengthened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Whether the validation applies to the kernel or only the original inscription.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(tsunami_stone_commitment__catastrophe_validation_axis, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(tsunami_stone_commitment__catastrophe_validation_axis, tsunami_stone_commitment__behavioral_competence_reading).
narrative_ontology:affects_constraint(tsunami_stone_commitment__catastrophe_validation_axis, tsunami_stone_commitment__commemorative_husk_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the tsunami_stone_commitment kernel, decomposed per the Îµ-invariance principle. The 2011 tsunami event operates as a physical mountain-type constraint that adjudicates between the behavioral competence and commemorative husk readings by providing decisive empirical validation. Each reading carries a distinct Îµ and structural profile.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
