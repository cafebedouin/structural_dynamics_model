% ============================================================================
% CONSTRAINT STORY: aneyoshi_stone_directive__behavioral_competence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
    narrative_ontology:suppression_profile/2,
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
 *   constraint_id: aneyoshi_stone_directive__behavioral_competence_reading
 *   human_readable: Aneyoshi Stone Directive â Behavioral Competence Reading
 *   domain: disaster_anthropology/institutional_memory/land_use_governance
 *
 * SUMMARY:
 *   The Aneyoshi stone is a tsunami warning inscription erected after the
 *   1896 Sanriku earthquake and retained through the 1933 and 2011 tsunamis.
 *   It marks a boundary above which homes should be built. This reading
 *   treats the stone not as a memorial relic but as a continuously binding
 *   land-use constraint that retained behavioral competence for 78 years
 *   without scientific validation. The constraint encodes a geophysical risk
 *   boundary; compliance is survival-positive and extracts from no one.
 *
 * KEY AGENTS:
 *   - Aneyoshi village community: Practices land-use restraint aligned with the stone inscription; neither extracts from nor is extracted by the constraint. Their compliance is survival-positive and opportunity-cost-bearing (foregone lowland development), yielding near-symmetric directionality.
 *   - Tsunami inundation geophysics: The non-agent physical boundary that the stone encodes; the actual source of constraint.
 *   - External disaster-management authorities: Analytical observers who could validate or invalidate the boundary but have not been the source of the constraint's persistence.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(aneyoshi_stone_directive__behavioral_competence_reading, 0.05).
domain_priors:suppression_score(aneyoshi_stone_directive__behavioral_competence_reading, 0.05).
domain_priors:theater_ratio(aneyoshi_stone_directive__behavioral_competence_reading, 0.08).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(aneyoshi_stone_directive__behavioral_competence_reading, extractiveness, 0.05).
narrative_ontology:constraint_metric(aneyoshi_stone_directive__behavioral_competence_reading, suppression_requirement, 0.05).
narrative_ontology:constraint_metric(aneyoshi_stone_directive__behavioral_competence_reading, theater_ratio, 0.08).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(aneyoshi_stone_directive__behavioral_competence_reading, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(aneyoshi_stone_directive__behavioral_competence_reading, resistance, 0.03).

% --- Constraint claim ---
narrative_ontology:constraint_claim(aneyoshi_stone_directive__behavioral_competence_reading, mountain).
narrative_ontology:human_readable(aneyoshi_stone_directive__behavioral_competence_reading, "Aneyoshi Stone Directive â Behavioral Competence Reading").
narrative_ontology:topic_domain(aneyoshi_stone_directive__behavioral_competence_reading, "disaster_anthropology/institutional_memory/land_use_governance").

domain_priors:emerges_naturally(aneyoshi_stone_directive__behavioral_competence_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(aneyoshi_stone_directive__behavioral_competence_reading, '92f66921-80e4-439c-b43a-1905bf12db04').
narrative_ontology:cs_kernel_codification('92f66921-80e4-439c-b43a-1905bf12db04', fixed_text).
narrative_ontology:cs_authority_grounding('92f66921-80e4-439c-b43a-1905bf12db04', lineage).
narrative_ontology:cs_interpretation_layer_present('92f66921-80e4-439c-b43a-1905bf12db04').
narrative_ontology:cs_reading_relation('92f66921-80e4-439c-b43a-1905bf12db04', aneyoshi_stone_directive__commemorative_husk_reading, forecloses).
narrative_ontology:cs_axiom('92f66921-80e4-439c-b43a-1905bf12db04', foundational, retained_behavioral_competence).
narrative_ontology:cs_axiom_status(retained_behavioral_competence, holdable).
narrative_ontology:cs_axiom_grounding('92f66921-80e4-439c-b43a-1905bf12db04', retained_behavioral_competence, empirically_contingent).
narrative_ontology:cs_axiom('92f66921-80e4-439c-b43a-1905bf12db04', foundational, binding_land_use_directive).
narrative_ontology:cs_axiom_status(binding_land_use_directive, holdable).
narrative_ontology:cs_axiom_grounding('92f66921-80e4-439c-b43a-1905bf12db04', binding_land_use_directive, conventional).
narrative_ontology:cs_reference_frame('92f66921-80e4-439c-b43a-1905bf12db04', active_directive_frame).
narrative_ontology:cs_drift_state('92f66921-80e4-439c-b43a-1905bf12db04', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('92f66921-80e4-439c-b43a-1905bf12db04', '').
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
% COORDINATION_FUNCTION: Transmits a geophysical risk boundary (tsunami inundation limit) across generations so that settlement patterns remain aligned with collective survival requirements.
% TRANSFER_FUNCTION: No asymmetric transfer; the constraint forecloses lowland development but the foregone land use is not captured by any party. The transfer is from individual short-term building convenience to collective survival probability, with no intermediate collector.
% ABSENT_VOICES: Modern urban planners and civil engineers who would demand hydrodynamic validation before recognizing the boundary; their absence is structuralâthe directive persisted through oral tradition without scientific corroboration.
% DISAPPEARANCE_RATIONALE: If the stone inscription vanished, the tsunami inundation boundary would remain a physical fact; modern instruments could rediscover it, and the village's survival would still depend on heeding the underlying geophysics. No social rearrangement is required because the constraint is not a social construct.
% FOUNDING_PROBLEM: How to prevent tsunami mortality in a coastal village with recurring inundation risk and no external early-warning or defensive infrastructure.
% FOUNDING_PROBLEM_CORROBORATION: The 2011 TÅhoku tsunami provided catastrophic corroboration from outside the village: settlements below similar historical markers were destroyed, while Aneyoshi's compliance with the stone boundary preserved lives. Geophysical and disaster-management literature external to the village attests the persistent risk.
narrative_ontology:disappearance_verdict(aneyoshi_stone_directive__behavioral_competence_reading, world_unchanged).
narrative_ontology:founding_problem_status(aneyoshi_stone_directive__behavioral_competence_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(aneyoshi_stone_directive__behavioral_competence_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(aneyoshi_stone_directive__behavioral_competence_reading, 'none', 1).
narrative_ontology:epsilon_provenance(aneyoshi_stone_directive__behavioral_competence_reading, 0.05, 'kimi-k2.6', 'none', direct).

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
 *   Extractiveness is near-zero (0.05) because the constraint merely forecloses building in a zone that is physically dangerous; there is no rent or asymmetric transfer. Suppression is minimal (0.05) because compliance aligns with self-preservation and requires no coercion. Accessibility collapse is high (0.92) because once the tsunami risk is understood, lowland settlement ceases to be a viable alternative. Resistance is negligible (0.03) since the constraint does not advance any party's interest against another's. Theater ratio is low (0.08) because the stone's function is to transmit a physical warning, not to sustain a performance of authority.
 *
 * PERSPECTIVAL GAP:
 *   The village seat experiences the directive as self-evident, lived tradition; the external scientific or planning seat may perceive it as unvalidated folklore. However, because the constraint is physically grounded, this perspectival difference does not alter the metric profile: effective extraction remains negligible from every seat.
 *
 * DIRECTIONALITY LOGIC:
 *   No beneficiaries or victims are declared. The village population sits near symmetric: they bear the opportunity cost of restricted building land and simultaneously receive the survival benefit of elevated settlement. Because base extractiveness is near-zero, effective extraction chi is negligible regardless of seat. The constraint's force originates in the geophysical boundary, not in any agent's enforcement.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problemârecurring tsunami mortalityâremains live, as demonstrated by the 2011 TÅhoku event. There is no mandate obsolescence. The constraint cannot be a piton because its functional content remains fully operative; it cannot be a snare because there is no capturing agent. The low theater ratio confirms the absence of performative maintenance.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'Does the Aneyoshi stone directive retain genuine behavioral competence over land-use decisions, or has it become a commemorative husk with no operative force?',
    'Ethnographic observation of contemporary land-use boundary decisions in Aneyoshi: do villagers cite the stone as an active reason for building location, or only as historical memory? Geomorphological validation of the tsunami run-up boundary against modern survey data.',
    'If behavioral competence is confirmed, this reading (mountain, negligible extraction) stands. If the directive is honored only rhetorically or post-hoc, the constraint collapses toward a piton or commemorative artifact with high theater ratio and no functional coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest, empirical, 'Empirical contest between behavioral competence and commemorative husk readings of the stone.').

omega_variable(
    geomorphological_stability,
    'Has the coastal geomorphology of Aneyoshi remained stable enough over 78 years that the stone''s physical boundary still accurately encodes tsunami risk?',
    'Modern hydrodynamic and bathymetric survey comparing 1896/1933 tsunami inundation against present-day terrain and sea-level rise.',
    'If the boundary is no longer physically accurate, the mountain claim is weakened even if behavioral competence persists; the constraint would shift toward a traditional practice potentially misaligned with natural law.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(geomorphological_stability, empirical, 'Whether the stone''s physical boundary remains geomorphologically valid.').

omega_variable(
    institutional_memory_transmission,
    'Is the 78-year retention of the directive evidence of a functioning oral institutional memory, or of a constraint so aligned with physical geography that it requires no institutional maintenance?',
    'Comparative ethnography of villages with similar tsunami stones but different behavioral outcomes; analysis of intergenerational transmission mechanisms in Aneyoshi.',
    'If retention requires active institutional maintenance, the constraint has a coordination component even at low extraction. If retention is passive due to physical obviousness, the mountain classification is strengthened.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(institutional_memory_transmission, conceptual, 'Nature of the institutional memory sustaining the directive.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(aneyoshi_stone_directive__behavioral_competence_reading, 0, 78).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(aney_tr_t0, aneyoshi_stone_directive__behavioral_competence_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement(aney_tr_t15, aneyoshi_stone_directive__behavioral_competence_reading, theater_ratio, 15, 0.08).
narrative_ontology:measurement(aney_tr_t30, aneyoshi_stone_directive__behavioral_competence_reading, theater_ratio, 30, 0.07).
narrative_ontology:measurement(aney_tr_t45, aneyoshi_stone_directive__behavioral_competence_reading, theater_ratio, 45, 0.07).
narrative_ontology:measurement(aney_tr_t60, aneyoshi_stone_directive__behavioral_competence_reading, theater_ratio, 60, 0.08).
narrative_ontology:measurement(aney_tr_t78, aneyoshi_stone_directive__behavioral_competence_reading, theater_ratio, 78, 0.08).

% Extraction over time
narrative_ontology:measurement(aney_be_t0, aneyoshi_stone_directive__behavioral_competence_reading, base_extractiveness, 0, 0.05).
narrative_ontology:measurement(aney_be_t15, aneyoshi_stone_directive__behavioral_competence_reading, base_extractiveness, 15, 0.05).
narrative_ontology:measurement(aney_be_t30, aneyoshi_stone_directive__behavioral_competence_reading, base_extractiveness, 30, 0.05).
narrative_ontology:measurement(aney_be_t45, aneyoshi_stone_directive__behavioral_competence_reading, base_extractiveness, 45, 0.05).
narrative_ontology:measurement(aney_be_t60, aneyoshi_stone_directive__behavioral_competence_reading, base_extractiveness, 60, 0.05).
narrative_ontology:measurement(aney_be_t78, aneyoshi_stone_directive__behavioral_competence_reading, base_extractiveness, 78, 0.05).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(aneyoshi_stone_directive__behavioral_competence_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
