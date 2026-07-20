% ============================================================================
% CONSTRAINT STORY: aneyoshi_stone_directive__behavioral_competence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_aneyoshi_stone_directive__behavioral_competence_reading, []).

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
    constraint_indexing:constraint_classification/3,
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
 *   constraint_id: aneyoshi_stone_directive__behavioral_competence_reading
 *   human_readable: Aneyoshi Stone Directive â Behavioral Competence Reading
 *   domain: disaster anthropology / institutional memory / land-use governance
 *
 * SUMMARY:
 *   In 1933, a tsunami destroyed the lower settlement of Aneyoshi, Japan.
 *   Survivors erected an inscribed stone warning: 'Do not build below this
 *   point.' For 78 years, the village retained the directive as a binding
 *   land-use constraint despite the absence of external scientific
 *   validation, institutional enforcement, or modern governance integration.
 *   The 2011 Tohoku tsunami inundated the coast precisely to the stone's
 *   line, sparing the village. This reading treats the stone not as a
 *   commemorative relic but as a behaviorally competent constraint that
 *   successfully aligned human settlement with an underlying physical
 *   geography mountain â the tsunami inundation boundary â without
 *   beneficiaries, victims, or active enforcement. It is one reading of a
 *   contested kernel; the sibling reading treats the stone as a commemorative
 *   husk that lost behavioral force during the inter-catastrophe period.
 *
 * KEY AGENTS:
 *   - Aneyoshi villagers: Continuous compliers (moderate power / constrained exit by geography and tradition) â they structured settlement around the stone's boundary without external enforcement; no transfer or extraction flows to or from them.
 *   - Modern municipal planners: Excluded institutional actors (institutional power / analytical exit) â represented an alternative governance frame that could have overridden the stone but did not penetrate the village's practice during the interval.
 *   - 2011 Tohoku tsunami: Natural empirical validator â the physical event that tested the boundary; not an agent but the referent of the mountain constraint.
 *   - Disaster anthropologists: Analytical observers (institutional / analytical exit) â post-2011 investigators who document and interpret the constraint's persistence.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(aneyoshi_stone_directive__behavioral_competence_reading, 0.06).
domain_priors:suppression_score(aneyoshi_stone_directive__behavioral_competence_reading, 0.04).
domain_priors:theater_ratio(aneyoshi_stone_directive__behavioral_competence_reading, 0.06).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(aneyoshi_stone_directive__behavioral_competence_reading, extractiveness, 0.06).
narrative_ontology:constraint_metric(aneyoshi_stone_directive__behavioral_competence_reading, suppression_requirement, 0.04).
narrative_ontology:constraint_metric(aneyoshi_stone_directive__behavioral_competence_reading, theater_ratio, 0.06).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(aneyoshi_stone_directive__behavioral_competence_reading, accessibility_collapse, 0.93).
narrative_ontology:constraint_metric(aneyoshi_stone_directive__behavioral_competence_reading, resistance, 0.02).

% --- Constraint claim ---
narrative_ontology:constraint_claim(aneyoshi_stone_directive__behavioral_competence_reading, mountain).
narrative_ontology:human_readable(aneyoshi_stone_directive__behavioral_competence_reading, "Aneyoshi Stone Directive â Behavioral Competence Reading").
narrative_ontology:topic_domain(aneyoshi_stone_directive__behavioral_competence_reading, "disaster anthropology / institutional memory / land-use governance").

domain_priors:emerges_naturally(aneyoshi_stone_directive__behavioral_competence_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(aneyoshi_stone_directive__behavioral_competence_reading, '34be5156-dcad-44ac-920f-95ef71addba9').
narrative_ontology:cs_kernel_codification('34be5156-dcad-44ac-920f-95ef71addba9', fixed_text).
narrative_ontology:cs_authority_grounding('34be5156-dcad-44ac-920f-95ef71addba9', practice).
narrative_ontology:cs_reading_relation('34be5156-dcad-44ac-920f-95ef71addba9', aneyoshi_stone_directive__commemorative_husk_reading, forecloses).
narrative_ontology:cs_axiom('34be5156-dcad-44ac-920f-95ef71addba9', foundational, unvalidated_prescription_maintains_binding_force).
narrative_ontology:cs_axiom_status(unvalidated_prescription_maintains_binding_force, holdable).
narrative_ontology:cs_axiom_grounding('34be5156-dcad-44ac-920f-95ef71addba9', unvalidated_prescription_maintains_binding_force, empirically_contingent).
narrative_ontology:cs_axiom('34be5156-dcad-44ac-920f-95ef71addba9', foundational, physical_geography_overrides_institutional_validation).
narrative_ontology:cs_axiom_status(physical_geography_overrides_institutional_validation, holdable).
narrative_ontology:cs_axiom_grounding('34be5156-dcad-44ac-920f-95ef71addba9', physical_geography_overrides_institutional_validation, empirically_contingent).
narrative_ontology:cs_reference_frame('34be5156-dcad-44ac-920f-95ef71addba9', tsunami_inundation_boundary_as_law).
narrative_ontology:cs_drift_state('34be5156-dcad-44ac-920f-95ef71addba9', inter_catastrophe_modernization_period, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('34be5156-dcad-44ac-920f-95ef71addba9', '').
narrative_ontology:cs_kernel_id(aneyoshi_stone_directive__behavioral_competence_reading, aneyoshi_stone_directive).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Aligns settlement location with a tsunami-safe elevation without requiring continuous scientific monitoring or centralized enforcement, effectively coordinating the community's spatial distribution with a persistent natural hazard.
% TRANSFER_FUNCTION: No transfer of value between agents; the constraint operates as a risk-avoidance alignment that prevents development in the lowland zone but extracts no resources from compliers.
% ABSENT_VOICES: Modern real-estate developers and municipal politicians who might prefer lower-elevation development for short-term economic gain were structurally absent from the village's traditional decision space; their exclusion was geographic and institutional rather than a suppression strategy by a benefiting party.
% DISAPPEARANCE_RATIONALE: If the inscribed stone vanished but the physical tsunami risk remained, the inundation boundary would persist as a mountain constraint; the world would not rearrange because the hazard is independent of the marker. Only if the underlying geography changed would the world rearrange.
% FOUNDING_PROBLEM: Protecting the community from catastrophic tsunami inundation by demarcating a permanent, inter-generational safe-elevation boundary for settlement.
% FOUNDING_PROBLEM_CORROBORATION: Geological paleotsunami records and the 2011 Tohoku tsunami event corroborate the founding problem from outside the village's oral tradition; no beneficiary group manufactures the hazard narrative, and the empirical validation came from the physical event itself.
narrative_ontology:disappearance_verdict(aneyoshi_stone_directive__behavioral_competence_reading, world_unchanged).
narrative_ontology:founding_problem_status(aneyoshi_stone_directive__behavioral_competence_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(aneyoshi_stone_directive__behavioral_competence_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(aneyoshi_stone_directive__behavioral_competence_reading, 'none', 1).
narrative_ontology:epsilon_provenance(aneyoshi_stone_directive__behavioral_competence_reading, 0.06, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(aneyoshi_stone_directive__behavioral_competence_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(aneyoshi_stone_directive__behavioral_competence_reading, ExtMetricName, E),
    domain_priors:suppression_score(aneyoshi_stone_directive__behavioral_competence_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(aneyoshi_stone_directive__behavioral_competence_reading),
    narrative_ontology:constraint_metric(aneyoshi_stone_directive__behavioral_competence_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(aneyoshi_stone_directive__behavioral_competence_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(aneyoshi_stone_directive__behavioral_competence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is negligible (0.06) because the constraint channels behavior away from a natural hazard without transferring resources to any party. Suppression is near-zero (0.04) because persistence requires no coercion; the constraint is self-enforcing through the empirical reality of tsunami inundation. Theater ratio is low (0.06) because the stone requires no performative maintenance to function â its force derives from alignment with physical geography, not ritual. Accessibility collapse is very high (0.93) because once the inundation boundary is understood, building below it is not a viable alternative. Resistance is near-zero (0.02) because physical geography meets no human resistance. The metrics are authored independently of the mountain claim: the claim asserts a natural law, while the metrics describe the constraint's actual operation. Their alignment is descriptive, not tuned.
 *
 * PERSPECTIVAL GAP:
 *   There is minimal seat divergence because the constraint is a mountain: all agents who interact with it â villagers, observers, later municipal actors â experience it as a physical limit rather than an extractive arrangement. The villager seat and the analytical observer seat both compute toward mountain; there is no beneficiary seat extracting at their expense and no victim seat bearing asymmetric costs.
 *
 * DIRECTIONALITY LOGIC:
 *   No beneficiaries or victims are declared. The constraint operates symmetrically on any agent who would inhabit the zone: it does not subsidize any agent, nor does it extract from any. Directionality for all applicable seats sits near symmetric (d â 0.5), and with negligible base extractiveness Îµ, effective extraction Ï remains negligible across all seats. No directionality override is necessary.
 *
 * MANDATROPHY ANALYSIS:
 *   Mandatrophy is not declared because the constraint's mandate â aligning settlement with tsunami-safe elevation â has not outlived its function. The 2011 tsunami validated that the founding problem remains live. Classifying this as a mountain prevents mislabeling it as a piton (which would require theatrical maintenance of an atrophied function) or a snare (which would require identifiable victims and active coercion). The low theater ratio and negligible extractiveness support the structural claim that the constraint persists by alignment with physical reality, not by institutional inertia.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    behavioral_competence_vs_memorial_husk,
    'Did the stone directive maintain continuous behavioral force over land-use decisions, or was its binding nature retrospectively constructed after the 2011 tsunami?',
    'Archaeological survey of building foundations and municipal land-use records from 1933â2011 to verify whether construction actually respected the stone''s elevation boundary.',
    'If inter-catastrophe construction respected the boundary, this reading is corroborated as a mountain-aligned constraint; if not, the commemorative husk reading is supported and this constraint reclassifies toward piton or snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(behavioral_competence_vs_memorial_husk, empirical, 'Empirical ambiguity about continuous behavioral competence versus retrospective narrative construction.').

omega_variable(
    physical_boundary_accuracy,
    'Does the stone''s inscribed elevation boundary accurately track the maximum credible tsunami inundation horizon, or is the alignment coincidental?',
    'Paleotsunami deposit mapping and hydrodynamic modeling to validate the stone''s line against independent geological estimates of recurrence-interval inundation.',
    'If the boundary is physically accurate, the mountain classification is reinforced; if arbitrary, the constraint is a conventional rule misclassified as natural law.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(physical_boundary_accuracy, empirical, 'Whether the stone''s boundary is a genuine physical limit or a constructed convention.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(aneyoshi_stone_directive__behavioral_competence_reading, 0, 78).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(aney_tr_t0, aneyoshi_stone_directive__behavioral_competence_reading, theater_ratio, 0, 0.06).
narrative_ontology:measurement(aney_tr_t15, aneyoshi_stone_directive__behavioral_competence_reading, theater_ratio, 15, 0.06).
narrative_ontology:measurement(aney_tr_t30, aneyoshi_stone_directive__behavioral_competence_reading, theater_ratio, 30, 0.06).
narrative_ontology:measurement(aney_tr_t45, aneyoshi_stone_directive__behavioral_competence_reading, theater_ratio, 45, 0.06).
narrative_ontology:measurement(aney_tr_t60, aneyoshi_stone_directive__behavioral_competence_reading, theater_ratio, 60, 0.06).
narrative_ontology:measurement(aney_tr_t78, aneyoshi_stone_directive__behavioral_competence_reading, theater_ratio, 78, 0.06).

% Extraction over time
narrative_ontology:measurement(aney_be_t0, aneyoshi_stone_directive__behavioral_competence_reading, base_extractiveness, 0, 0.06).
narrative_ontology:measurement(aney_be_t15, aneyoshi_stone_directive__behavioral_competence_reading, base_extractiveness, 15, 0.06).
narrative_ontology:measurement(aney_be_t30, aneyoshi_stone_directive__behavioral_competence_reading, base_extractiveness, 30, 0.06).
narrative_ontology:measurement(aney_be_t45, aneyoshi_stone_directive__behavioral_competence_reading, base_extractiveness, 45, 0.06).
narrative_ontology:measurement(aney_be_t60, aneyoshi_stone_directive__behavioral_competence_reading, base_extractiveness, 60, 0.06).
narrative_ontology:measurement(aney_be_t78, aneyoshi_stone_directive__behavioral_competence_reading, base_extractiveness, 78, 0.06).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(aneyoshi_stone_directive__behavioral_competence_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(aneyoshi_stone_directive__behavioral_competence_reading, aneyoshi_stone_directive__commemorative_husk_reading).

% DUAL FORMULATION NOTE:
% The natural-language label 'Aneyoshi stone directive' conflates two structurally distinct constraints: this reading, which asserts continuous behavioral competence aligned with physical geography (negligible Îµ, mountain), and the sibling commemorative_husk_reading, which treats the stone as a post-hoc memorial artifact that lost behavioral force (likely higher Îµ, piton or snare). They are linked as a constraint family because they compete to explain the same historical object.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
