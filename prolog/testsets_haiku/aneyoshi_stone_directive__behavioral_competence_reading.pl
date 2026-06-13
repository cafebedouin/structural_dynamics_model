% ============================================================================
% CONSTRAINT STORY: aneyoshi_stone_directive__behavioral_competence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:suppression_profile/2,
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
 *   constraint_id: aneyoshi_stone_directive__behavioral_competence_reading
 *   human_readable: Aneyoshi Stone Directive: Behavioral Competence Reading
 *   domain: disaster_anthropology/institutional_memory/land_governance
 *
 * SUMMARY:
 *   In 1270, a stone was inscribed in Aneyoshi village (Japan, Iwate
 *   Prefecture) with a directive marking the maximum tsunami inundation
 *   boundary observed in the village's history. For 78 years across the
 *   modern period (spanning two inter-catastrophe intervals and multiple
 *   administrative turnovers), the stone's position has remained a binding
 *   constraint on land-use planning: building codes, evacuation zones, and
 *   official inundation maps treat the stone's elevation as the edge of the
 *   habitability zone. The stone has never been re-measured or re-validated
 *   by geomorphological experts since its inscription. This constraint story
 *   instantiates the BEHAVIORAL COMPETENCE READING: the stone directive
 *   persists because it marks a real geographical boundary that residents
 *   continue to observe and that authorities continue to treat as validated.
 *   The claim is that the constraint IS physical geography, not a cultural
 *   artifact riding on physical geography as a metaphor. The alternative
 *   reading (commemorative_husk_reading) would argue the stone has become a
 *   symbolic marker whose original function has been forgotten, and
 *   persistence is now purely ceremonial inertia. These two readings have
 *   opposite structural implications: the behavioral-competence reading
 *   yields a mountain (very low ε, no beneficiary); the commemorative-husk
 *   reading yields a piton (higher theater, inertial maintenance by
 *   institutional agenda-setters).
 *
 * KEY AGENTS:
 *   - aneyoshi_village_residents: embodied observers of water behavior and settlement patterns; treat the stone as a landmark that corresponds to observed inundation risk without necessarily understanding its original directive or its 78-year unvalidated status
 *   - disaster_response_authorities: institutional agenda-setters who administer evacuation zones and building codes; cite the stone in official planning documents without re-validating its position
 *   - geomorphological_researchers: the excluded technical authority whose expertise would resolve whether the stone's position is still accurate; their absence from the constraint's validation loop is the structural condition that allows unvalidated persistence
 *   - analytical_observer: sits outside the system and observes the constraint as an instance of institutional memory that has survived without re-validation — the measurement object
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(aneyoshi_stone_directive__behavioral_competence_reading, 0.08).
domain_priors:suppression_score(aneyoshi_stone_directive__behavioral_competence_reading, 0.12).
domain_priors:theater_ratio(aneyoshi_stone_directive__behavioral_competence_reading, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(aneyoshi_stone_directive__behavioral_competence_reading, extractiveness, 0.08).
narrative_ontology:constraint_metric(aneyoshi_stone_directive__behavioral_competence_reading, suppression_requirement, 0.12).
narrative_ontology:constraint_metric(aneyoshi_stone_directive__behavioral_competence_reading, theater_ratio, 0.65).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(aneyoshi_stone_directive__behavioral_competence_reading, accessibility_collapse, 0.88).
narrative_ontology:constraint_metric(aneyoshi_stone_directive__behavioral_competence_reading, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(aneyoshi_stone_directive__behavioral_competence_reading, mountain).
narrative_ontology:human_readable(aneyoshi_stone_directive__behavioral_competence_reading, "Aneyoshi Stone Directive: Behavioral Competence Reading").
narrative_ontology:topic_domain(aneyoshi_stone_directive__behavioral_competence_reading, "disaster_anthropology/institutional_memory/land_governance").

domain_priors:emerges_naturally(aneyoshi_stone_directive__behavioral_competence_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(aneyoshi_stone_directive__behavioral_competence_reading, '08425365-b1e1-447d-8875-adc11717f3ac').
narrative_ontology:cs_kernel_codification('08425365-b1e1-447d-8875-adc11717f3ac', fixed_text).
narrative_ontology:cs_authority_grounding('08425365-b1e1-447d-8875-adc11717f3ac', practice).
narrative_ontology:cs_interpretation_layer_present('08425365-b1e1-447d-8875-adc11717f3ac').
narrative_ontology:cs_reading_relation('08425365-b1e1-447d-8875-adc11717f3ac', aneyoshi_stone_directive__commemorative_husk_reading, coexists_with).
narrative_ontology:cs_axiom('08425365-b1e1-447d-8875-adc11717f3ac', foundational, stone_marks_validated_inundation_boundary).
narrative_ontology:cs_axiom_status(stone_marks_validated_inundation_boundary, holdable).
narrative_ontology:cs_axiom_grounding('08425365-b1e1-447d-8875-adc11717f3ac', stone_marks_validated_inundation_boundary, empirically_contingent).
narrative_ontology:cs_axiom('08425365-b1e1-447d-8875-adc11717f3ac', foundational, behavioral_force_derives_from_physical_geography).
narrative_ontology:cs_axiom_status(behavioral_force_derives_from_physical_geography, holdable).
narrative_ontology:cs_axiom_grounding('08425365-b1e1-447d-8875-adc11717f3ac', behavioral_force_derives_from_physical_geography, deontological).
narrative_ontology:cs_reference_frame('08425365-b1e1-447d-8875-adc11717f3ac', validated_geographical_marker).
narrative_ontology:cs_drift_state('08425365-b1e1-447d-8875-adc11717f3ac', contemporary_unvalidated_period, gap(codification_collapse, substantial, false)).
narrative_ontology:cs_created_at('08425365-b1e1-447d-8875-adc11717f3ac', '').
narrative_ontology:cs_kernel_id(aneyoshi_stone_directive__behavioral_competence_reading, aneyoshi_stone_directive).

% --- Structural relationships ---
narrative_ontology:constraint_vindicates(aneyoshi_stone_directive__behavioral_competence_reading, institutional_memory_persistence_doctrine).
narrative_ontology:constraint_vindicates(aneyoshi_stone_directive__behavioral_competence_reading, geographical_determinism_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Inhabit the coastal basin where the stone directive marks the tsunami inundation boundary. For 78 years across multiple inter-catastrophe periods, they have observed the stone's position and adjusted settlement patterns accordingly, without explicit knowledge of the directive's original function or explicit codification of its meaning. They treat the stone as a landmark that corresponds to observed tidal and wave behavior.
narrative_ontology:constraint_stakeholder(aneyoshi_stone_directive__behavioral_competence_reading, aneyoshi_village_residents, observer,
    powerless, generational, constrained, local).

% Administer evacuation zones and building codes. They reference the stone directive in official inundation maps and planning documents, treating it as a validated empirical constraint on settlement. They have not independently re-measured the stone's position or re-validated the original inundation claim since the directive was inscribed.
narrative_ontology:constraint_stakeholder(aneyoshi_stone_directive__behavioral_competence_reading, disaster_response_authorities, agenda_setter,
    institutional, biographical, mobile, national).

% Possess the expertise to measure shore migration, tsunami run-up, and sediment deposition that would validate or refute the stone's position as a binding inundation boundary. They are rarely consulted in land-use decisions; their expertise is excluded from the constraint's maintenance loop.
narrative_ontology:constraint_stakeholder(aneyoshi_stone_directive__behavioral_competence_reading, geomorphological_researchers, excluded,
    powerful, biographical, mobile, national).

% Views the stone directive as an instance of institutional constraint persistence: a rule that has survived 78 years and multiple catastrophe cycles without formal re-validation, maintaining behavioral force despite gaps in the chain of explicit knowledge transfer.
narrative_ontology:constraint_stakeholder(aneyoshi_stone_directive__behavioral_competence_reading, analytical_observer, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The stone marks a geographical threshold — the observed maximum inundation boundary from documented tsunami events. The constraint coordinates settlement patterns by establishing a shared, persistent, spatially-fixed reference point that residents and authorities treat as the edge of the habitability zone.
% TRANSFER_FUNCTION: No resource transfer occurs. The constraint does not move goods or enforce payment. It allocates space: settlement is tacitly restricted above the stone's elevation, and the restriction persists without active enforcement or extraction.
% ABSENT_VOICES: Geomorphological experts are structurally excluded from the constraint's validation loop. They would object that 78 years without re-measurement is indefensible for a physical boundary claim, but they are not consulted in land-use planning. Local residents' understanding of the stone's function drifts over time; younger generations may not know the directive's original tsunami-inundation rationale.
% DISAPPEARANCE_RATIONALE: If the stone directive disappeared or were invalidated tomorrow, the geographical facts — tsunami run-up, shore migration, sediment deposition — would persist. Residents would still observe water behavior and adjust settlement based on direct observation rather than the stone's position. The world rearranges only if the directive constrains behavior beyond what residents would choose from physical observation alone. This reading argues the stone's behavioral force IS physical geography, not a constructed rule riding on the stone as a symbol.
% FOUNDING_PROBLEM: Tsunamis generated by offshore earthquakes inundate the coastal basin unpredictably. The directive was inscribed on the stone to mark the maximum observed inundation boundary so that future inhabitants would know where settlement was safe from inundation.
% FOUNDING_PROBLEM_CORROBORATION: Geomorphological research attests that the basin remains at risk of tsunami inundation at similar or potentially greater magnitude. The founding problem (future residents need a persistent physical marker to identify the inundation zone) remains live; however, the stone's position as a *validated* marker of that zone is disputed. Seismic and coastal-change data from researchers outside the benefiting-parties set support this reading.
narrative_ontology:disappearance_verdict(aneyoshi_stone_directive__behavioral_competence_reading, world_unchanged).
narrative_ontology:founding_problem_status(aneyoshi_stone_directive__behavioral_competence_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(aneyoshi_stone_directive__behavioral_competence_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(aneyoshi_stone_directive__behavioral_competence_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(aneyoshi_stone_directive__behavioral_competence_reading_tests).

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
 *   Extractiveness is very low (0.08 throughout) because the constraint does not move resources or enforce payment — it allocates space based on an observed geographical threshold. There is no party that benefits from compliance the way an extractive constraint benefits a beneficiary. Suppression is very low (0.12) because residents are not coerced to comply; they observe tsunami behavior and naturally avoid the inundation zone. Accessibility collapse is very high (0.88) because once the inundation boundary is understood, alternatives to the settlement restriction collapse — you cannot choose to inhabit a tsunami-inundated zone. Resistance is very low (0.05) because the constraint does not meet resistance; nobody disputes that tsunamis are dangerous and that the inundation zone should be avoided. The theater_ratio is the diagnostic metric for this reading: it rises from 0.3 (early period, when knowledge of the directive's purpose was fresher) to 0.65 (late period, after 78 years of institutional citation without validation or re-measurement). The rise in theater_ratio indicates increasing divergence between the functional claim (the stone marks a validated geographical boundary) and the symbolic claim (the stone is cited in official documents regardless of whether its position has been re-validated). A flat extractiveness series and a rising theater_ratio, together with low suppression and very high accessibility collapse, are consistent with a mountain whose behavioral force derives from the geography it marks, not from institutional enforcement. This reading treats the rising theater_ratio as the sign of a constraint that is losing the knowledge infrastructure that would sustain it as validated geography — moving toward the commemorative husk. The theater rise is NOT the primary evidence the reading rests on; it is diagnostic uncertainty that the omega variables address.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of village residents, the stone is a landmark that corresponds to observed water behavior — they experience it as a geographical fact. From the perspective of disaster response authorities, the stone is an officially-binding constraint that they cite in planning documents — they experience it as an institutional fact. From the perspective of geomorphological researchers (excluded from the constraint's operation), the stone is an unvalidated claim whose accuracy cannot be assessed without re-measurement — they would experience it as a knowledge gap. The analytical observer sees all three perspectives and measures the divergence between them (via the rising theater_ratio) as the constraint's vulnerability to reclassification. The constraint does not compute differently from different seats in the sense that snares or tangled ropes do; rather, different seats have different epistemic access to the constraint's validity, and the rising theater_ratio indicates that access is diverging.
 *
 * DIRECTIONALITY LOGIC:
 *   There are no beneficiaries in this reading. The stone directive does not move goods or enforce asymmetric extraction. The village residents avoid the inundation zone because they observe water behavior; the authorities cite the stone because it is a persistent institutional reference point. Neither party benefits from the directive's existence in the way a snare's beneficiary benefits from the snare. The constraint's behavioral force derives from the geography it marks, not from any party's interest in maintaining a rule. This absence of beneficiaries is the marker that distinguishes this reading from the commemorative_husk_reading, which would identify the authorities as beneficiaries (they maintain the constraint's citation even without re-validation, which confers authority legitimacy and reduces the cost of re-measurement).
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint shows a specific mandatrophy signature: the founding problem (future residents need a persistent marker to identify the tsunami inundation zone) was live when the stone was inscribed, and it remains live (the basin is still at risk, residents still need the boundary marked). The constraint has not resolved its mandate; rather, the constraint's persistence has become decoupled from active maintenance of its validity. An institution (the disaster response authorities) continues to cite the directive without re-validating it — that is the mandatrophy state. The directive's original purpose has not been obsolesced; rather, institutional citation has replaced validation as the mechanism sustaining the constraint. The classification as mountain (under the behavioral competence reading) depends on the claim that the stone's behavioral force derives from the geography it marks, not from institutional inertia. If that claim is correct, the theater_ratio rise is diagnostic of a loss of validation infrastructure, not of mandatrophy per se (mandatrophy would require the founding problem to be dead, not just the validation chain to be broken). The omega variables address the ambiguity.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    stone_position_validation_gap,
    'Is the stone''s position an accurate record of the actual maximum tsunami inundation boundary, or has coastal geomorphology shifted (shore migration, sediment deposition, subsidence) such that the stone''s elevation no longer marks a meaningful physical threshold?',
    'Geomorphological survey comparing the stone''s position to contemporary topographic data, sediment cores, and tide-gauge records from the inter-catastrophe period. Re-measurement of tsunami run-up zones via modern bathymetric and shore-survey methods.',
    'If the stone''s position is validated, the constraint is a natural law (mountain classification holds); if invalidated, the constraint is a commemorative artifact maintaining behavioral force through institutional inertia (reclassifies toward piton). If partially shifted (e.g., coastal accretion below the stone), the directive''s correspondence to actual inundation risk degrades and the constraint becomes a false summit candidate.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(stone_position_validation_gap, empirical, 'Whether 78 years of coastal change has decoupled the stone from the physical geography it was inscribed to mark.').

omega_variable(
    behavioral_competence_vs_commemorative_husk,
    'Does the stone directive maintain behavioral force because residents and authorities treat it as a validated geographical boundary, or does it persist because it is a commemorative artifact whose original function has been forgotten and replaced by symbolic reverence?',
    'Ethnographic study of how residents and authorities explain the stone''s purpose. Interview the agenda-setters (disaster response authorities) asking explicitly: have you re-validated the stone''s position? Have you consulted geomorphological experts? Do you treat it as a binding physical constraint or as a culturally-inherited landmark? Trace knowledge transfer across generations of residents.',
    'If behavioral force derives from residents'' understanding of the stone as a validated physical boundary (behavioral competence reading), then the constraint is a mountain: persistence follows from the geography it marks. If behavioral force derives from commemoration and institutional inertia without understanding or re-validation (commemorative husk reading), then the constraint is a piton: persistence is theater. This reading asserts the former; the sibling reading asserts the latter.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(behavioral_competence_vs_commemorative_husk, conceptual, 'Whether the constraint''s behavioral persistence depends on physical geography or on commemorative practice divorced from validation.').

omega_variable(
    knowledge_transfer_chain_integrity,
    'Has the knowledge that the stone marks a tsunami inundation boundary been continuously transmitted through 78 years and multiple generational turnovers, or has the chain broken and the directive''s rationale been lost to residents while formal authority documents retain a hollow citation?',
    'Linguistic and anthropological trace: interviews with residents of different generations asking what the stone means and why it is treated as a boundary. Document matching (comparing original stone inscription to contemporary official maps and planning documents) to identify whether the rationale has been explicitly preserved or only the physical artifact.',
    'If the knowledge chain is intact, residents and authorities understand why the stone matters (behavioral competence reading holds); if broken, the directive has become a commemorative husk maintained by institutional citation without understanding. The measurement series shows theater_ratio rising from 0.3 to 0.65, suggesting increasing divergence between functional (understanding-based) and symbolic (citation-based) maintenance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(knowledge_transfer_chain_integrity, empirical, 'Whether the knowledge that makes the stone a directive (rather than just an artifact) has been preserved across 78 years.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(aneyoshi_stone_directive__behavioral_competence_reading, 0, 78).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(aney_tr_t0, aneyoshi_stone_directive__behavioral_competence_reading, theater_ratio, 0, 0.3).
narrative_ontology:measurement_basis(aney_tr_t0, observed).
narrative_ontology:measurement(aney_tr_t13, aneyoshi_stone_directive__behavioral_competence_reading, theater_ratio, 13, 0.4).
narrative_ontology:measurement_basis(aney_tr_t13, observed).
narrative_ontology:measurement(aney_tr_t26, aneyoshi_stone_directive__behavioral_competence_reading, theater_ratio, 26, 0.5).
narrative_ontology:measurement_basis(aney_tr_t26, observed).
narrative_ontology:measurement(aney_tr_t39, aneyoshi_stone_directive__behavioral_competence_reading, theater_ratio, 39, 0.58).
narrative_ontology:measurement_basis(aney_tr_t39, observed).
narrative_ontology:measurement(aney_tr_t52, aneyoshi_stone_directive__behavioral_competence_reading, theater_ratio, 52, 0.62).
narrative_ontology:measurement_basis(aney_tr_t52, observed).
narrative_ontology:measurement(aney_tr_t65, aneyoshi_stone_directive__behavioral_competence_reading, theater_ratio, 65, 0.65).
narrative_ontology:measurement_basis(aney_tr_t65, observed).
narrative_ontology:measurement(aney_tr_t78, aneyoshi_stone_directive__behavioral_competence_reading, theater_ratio, 78, 0.65).
narrative_ontology:measurement_basis(aney_tr_t78, observed).

% Extraction over time
narrative_ontology:measurement(aney_be_t0, aneyoshi_stone_directive__behavioral_competence_reading, base_extractiveness, 0, 0.08).
narrative_ontology:measurement_basis(aney_be_t0, observed).
narrative_ontology:measurement(aney_be_t13, aneyoshi_stone_directive__behavioral_competence_reading, base_extractiveness, 13, 0.08).
narrative_ontology:measurement_basis(aney_be_t13, observed).
narrative_ontology:measurement(aney_be_t26, aneyoshi_stone_directive__behavioral_competence_reading, base_extractiveness, 26, 0.08).
narrative_ontology:measurement_basis(aney_be_t26, observed).
narrative_ontology:measurement(aney_be_t39, aneyoshi_stone_directive__behavioral_competence_reading, base_extractiveness, 39, 0.08).
narrative_ontology:measurement_basis(aney_be_t39, observed).
narrative_ontology:measurement(aney_be_t52, aneyoshi_stone_directive__behavioral_competence_reading, base_extractiveness, 52, 0.08).
narrative_ontology:measurement_basis(aney_be_t52, observed).
narrative_ontology:measurement(aney_be_t65, aneyoshi_stone_directive__behavioral_competence_reading, base_extractiveness, 65, 0.08).
narrative_ontology:measurement_basis(aney_be_t65, observed).
narrative_ontology:measurement(aney_be_t78, aneyoshi_stone_directive__behavioral_competence_reading, base_extractiveness, 78, 0.08).
narrative_ontology:measurement_basis(aney_be_t78, observed).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(aneyoshi_stone_directive__behavioral_competence_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(aneyoshi_stone_directive__behavioral_competence_reading, information_standard).
narrative_ontology:affects_constraint(aneyoshi_stone_directive__behavioral_competence_reading, aneyoshi_stone_directive__commemorative_husk_reading).

% DUAL FORMULATION NOTE:
% The aneyoshi_stone_directive kernel has been decomposed into two constraint stories: behavioral_competence_reading (this file) and commemorative_husk_reading. The decomposition follows from the ε-invariance principle: measuring the constraint via 'the stone marks a validated geographical boundary' yields very low ε (mountain); measuring it via 'the stone is an institutional citation without re-validation' yields higher ε and higher theater_ratio (piton). These are structurally distinct constraints with different beneficiary structures, different behavioral mechanisms, and different stability conditions. The two readings coexist in public discourse but neither logically forecloses the other within a single framework — the contest is about which reading captures the constraint's actual function. Both stories link to each other via network.affects_constraints to enable constraint-family analysis and contamination propagation.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
