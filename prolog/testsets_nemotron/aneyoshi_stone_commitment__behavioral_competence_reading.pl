% ============================================================================
% CONSTRAINT STORY: aneyoshi_stone_commitment__behavioral_competence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_aneyoshi_stone_commitment__behavioral_competence_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: aneyoshi_stone_commitment__behavioral_competence_reading
 *   human_readable: Aneyoshi Stone as Live Land-Use Rule (Behavioral Competence Reading)
 *   domain: disaster_anthropology/commitment_systems/temporal_institutional_analysis
 *
 * SUMMARY:
 *   The Aneyoshi stone (erected 1933 after the 1896 and 1933 Sanriku
 *   tsunamis) bears the inscription: 'High dwellings are the peace and
 *   harmony of our descendants. Remember the calamity of the great tsunamis.
 *   Do not build any homes below this point.' Under the behavioral competence
 *   reading, this stone functioned as an active land-use regulation across 78
 *   years — household building decisions were constrained by the stone's
 *   directive, and the 2011 Tohoku tsunami survival of Aneyoshi households
 *   (zero fatalities, homes above the stone intact) is causally linked to
 *   this compliance. The constraint is claimed as mountain: the tsunami
 *   physics and the stone's geographic placement create a natural-law
 *   boundary that would persist regardless of human enforcement.
 *   Beneficiaries are declared (aneyoshi_households, aneyoshi_descendants,
 *   towada_village_administration) because identifiable agents benefit from
 *   the constraint's operation — this is an FSM candidate requiring omega
 *   documentation.
 *
 * KEY AGENTS:
 *   - aneyoshi_households: Primary beneficiary (powerless/identity_locked) — survival secured by compliance
 *   - aneyoshi_descendants: Primary beneficiary (powerless/identity_locked) — inherited protection through intergenerational transmission
 *   - towada_village_administration: Secondary beneficiary (organized/constrained) — administrative legitimacy from disaster resilience showcase
 *   - stone_directive: Analytical observer — the constraint itself as regulatory mechanism
 *   - commemorative_husk_reading_proponents: Excluded — would argue the stone is purely symbolic
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(aneyoshi_stone_commitment__behavioral_competence_reading, 0.03).
domain_priors:suppression_score(aneyoshi_stone_commitment__behavioral_competence_reading, 0.12).
domain_priors:theater_ratio(aneyoshi_stone_commitment__behavioral_competence_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(aneyoshi_stone_commitment__behavioral_competence_reading, extractiveness, 0.03).
narrative_ontology:constraint_metric(aneyoshi_stone_commitment__behavioral_competence_reading, suppression_requirement, 0.12).
narrative_ontology:constraint_metric(aneyoshi_stone_commitment__behavioral_competence_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(aneyoshi_stone_commitment__behavioral_competence_reading, accessibility_collapse, 0.88).
narrative_ontology:constraint_metric(aneyoshi_stone_commitment__behavioral_competence_reading, resistance, 0.04).

% --- Constraint claim ---
narrative_ontology:constraint_claim(aneyoshi_stone_commitment__behavioral_competence_reading, mountain).
narrative_ontology:human_readable(aneyoshi_stone_commitment__behavioral_competence_reading, "Aneyoshi Stone as Live Land-Use Rule (Behavioral Competence Reading)").
narrative_ontology:topic_domain(aneyoshi_stone_commitment__behavioral_competence_reading, "disaster_anthropology/commitment_systems/temporal_institutional_analysis").

domain_priors:emerges_naturally(aneyoshi_stone_commitment__behavioral_competence_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(aneyoshi_stone_commitment__behavioral_competence_reading, '0d7d31aa-7ff3-4b60-9f18-2ea6293e5222').
narrative_ontology:cs_kernel_codification('0d7d31aa-7ff3-4b60-9f18-2ea6293e5222', fixed_text).
narrative_ontology:cs_authority_grounding('0d7d31aa-7ff3-4b60-9f18-2ea6293e5222', lineage).
narrative_ontology:cs_interpretation_layer_present('0d7d31aa-7ff3-4b60-9f18-2ea6293e5222').
narrative_ontology:cs_reading_relation('0d7d31aa-7ff3-4b60-9f18-2ea6293e5222', aneyoshi_stone_commitment__commemorative_husk_reading, forecloses).
narrative_ontology:cs_axiom('0d7d31aa-7ff3-4b60-9f18-2ea6293e5222', foundational, intergenerational_disaster_memory_operational).
narrative_ontology:cs_axiom_status(intergenerational_disaster_memory_operational, holdable).
narrative_ontology:cs_axiom_grounding('0d7d31aa-7ff3-4b60-9f18-2ea6293e5222', intergenerational_disaster_memory_operational, empirically_contingent).
narrative_ontology:cs_axiom('0d7d31aa-7ff3-4b60-9f18-2ea6293e5222', foundational, stone_directive_causally_effective).
narrative_ontology:cs_axiom_status(stone_directive_causally_effective, holdable).
narrative_ontology:cs_axiom_grounding('0d7d31aa-7ff3-4b60-9f18-2ea6293e5222', stone_directive_causally_effective, empirically_contingent).
narrative_ontology:cs_reference_frame('0d7d31aa-7ff3-4b60-9f18-2ea6293e5222', stone_erection_1933).
narrative_ontology:cs_drift_state('0d7d31aa-7ff3-4b60-9f18-2ea6293e5222', post_2011_tsunami_validation, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('0d7d31aa-7ff3-4b60-9f18-2ea6293e5222', '').
narrative_ontology:cs_kernel_id(aneyoshi_stone_commitment__behavioral_competence_reading, aneyoshi_stone_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(aneyoshi_stone_commitment__behavioral_competence_reading, aneyoshi_households).
narrative_ontology:constraint_beneficiary(aneyoshi_stone_commitment__behavioral_competence_reading, aneyoshi_descendants).
narrative_ontology:constraint_beneficiary(aneyoshi_stone_commitment__behavioral_competence_reading, towada_village_administration).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Households in Aneyoshi hamlet whose building decisions were constrained by the stone's directive — they built and maintained homes above the stone line across generations. Their survival in the 2011 tsunami (zero fatalities) is causally attributed to this compliance. Exit from the constraint would mean building below the line, which is physically suicidal given tsunami physics; their identity is fused with the stone's directive through intergenerational transmission and place attachment.
narrative_ontology:constraint_stakeholder(aneyoshi_stone_commitment__behavioral_competence_reading, aneyoshi_households, beneficiary,
    powerless, biographical, identity_locked, local).

% Descendants of the original Aneyoshi households who inherited the stone's directive through oral transmission, household instruction, and community practice. They benefit from the accumulated survival capital of 78 years of compliance. Their exit options are identity-locked: rejecting the stone's directive would mean rejecting the ancestral wisdom that preserved their lineage, a form of relational and ideological identity fusion with the constraint.
narrative_ontology:constraint_stakeholder(aneyoshi_stone_commitment__behavioral_competence_reading, aneyoshi_descendants, beneficiary,
    powerless, generational, identity_locked, local).

% The municipal administration that oversees Aneyoshi hamlet. They benefit administratively from the hamlet's disaster resilience showcase status (zero fatalities in 2011). They play an agenda-setter role by maintaining the stone as a designated cultural property and incorporating its directive into local disaster planning. Their exit is constrained by bureaucratic accountability and the political cost of abandoning a proven survival mechanism.
narrative_ontology:constraint_stakeholder(aneyoshi_stone_commitment__behavioral_competence_reading, towada_village_administration, beneficiary,
    organized, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(aneyoshi_stone_commitment__behavioral_competence_reading, towada_village_administration, agenda_setter).

% The stone's inscribed directive itself, treated as an analytical object — the regulatory mechanism that marks the tsunami inundation boundary. It neither collects nor pays; it structures the constraint's operational logic. As a non-agent entity, it is excluded from beneficiary/victim derivation and directionality computation.
narrative_ontology:constraint_stakeholder(aneyoshi_stone_commitment__behavioral_competence_reading, stone_directive, observer,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(aneyoshi_stone_commitment__behavioral_competence_reading, stone_directive).

% Scholars and analysts who argue the Aneyoshi stone functions only as a memorial artifact — that compliance was coincidental or driven by other factors (land availability, economic conditions) and the stone's directive had no causal force on building decisions. They are structurally excluded from this reading's framework because the behavioral competence reading's core premise (active behavioral constraint) directly contradicts their position. They would object to the mountain classification and the causal survival attribution.
narrative_ontology:constraint_stakeholder(aneyoshi_stone_commitment__behavioral_competence_reading, commemorative_husk_reading_proponents, excluded,
    organized, biographical, mobile, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates intergenerational land-use decisions to keep human settlement above the tsunami inundation line — solves the collective action problem of maintaining disaster memory and behavioral compliance across generations where individual memory fades and economic pressure pushes toward riskier low-lying land.
% TRANSFER_FUNCTION: Moves zero material resources; transfers survival probability from the counterfactual (building below the stone line) to the actual (building above). The 'cost' is the opportunity cost of not using lower land, which is negligible compared to the survival benefit. No extraction occurs — the constraint is purely coordinative.
% ABSENT_VOICES: The commemorative husk reading proponents (scholars arguing the stone is purely symbolic) are absent from this reading's framework. They would argue that the 2011 survival was luck or attributable to other factors (evacuation culture, topography), not the stone's directive. They are located in academic disaster studies and historical sociology communities.
% DISAPPEARANCE_RATIONALE: If the stone and its directive vanished overnight (or were never erected), Aneyoshi households would likely have built on lower, more economically convenient land over the 78-year interval. The 2011 tsunami would then have caused fatalities — the world rearranges because the constraint's behavioral guidance was causally effective. The physical tsunami risk remains, but the behavioral mechanism to avoid it would be degraded.
% FOUNDING_PROBLEM: The 1896 and 1933 Sanriku tsunamis devastated Aneyoshi hamlet; survivors erected the stone to transmit the lesson 'do not build below this line' to future generations who would not remember the disasters directly.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem (tsunami recurrence risk) is corroborated by geological science (recurring megathrust earthquakes on the Japan Trench, ~500-1000 year recurrence) and the 2011 Tohoku tsunami itself — both independent of the benefiting parties. The stone's directive remains the primary behavioral mechanism addressing this live problem in Aneyoshi.
narrative_ontology:disappearance_verdict(aneyoshi_stone_commitment__behavioral_competence_reading, world_rearranges).
narrative_ontology:founding_problem_status(aneyoshi_stone_commitment__behavioral_competence_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(aneyoshi_stone_commitment__behavioral_competence_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(aneyoshi_stone_commitment__behavioral_competence_reading, 'none', 1).
narrative_ontology:epsilon_provenance(aneyoshi_stone_commitment__behavioral_competence_reading, 0.03, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(aneyoshi_stone_commitment__behavioral_competence_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(aneyoshi_stone_commitment__behavioral_competence_reading, ExtMetricName, E),
    domain_priors:suppression_score(aneyoshi_stone_commitment__behavioral_competence_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(aneyoshi_stone_commitment__behavioral_competence_reading),
    narrative_ontology:constraint_metric(aneyoshi_stone_commitment__behavioral_competence_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(aneyoshi_stone_commitment__behavioral_competence_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(aneyoshi_stone_commitment__behavioral_competence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is near-zero (0.03) because the constraint imposes negligible cost on those it governs — building above the stone line is the only viable option given tsunami physics; the stone merely marks what geography already dictates. Suppression is low (0.12) because alternatives (building below) are physically suicidal, not socially prohibited — the constraint does not need active enforcement. Theater ratio is minimal (0.05) because the stone's function is entirely instrumental (marking the survival boundary) with no performative layer. Accessibility collapse is very high (0.88) because the physical reality of tsunami inundation makes alternatives genuinely inaccessible — this is not social closure but geographic necessity. Resistance is near-zero (0.04) because no party resists a constraint that saves their lives; the 2011 outcome validates universal acceptance. The slight rise in suppression_requirement over time reflects demographic aging and out-migration pressures, not enforcement intensification.
 *
 * PERSPECTIVAL GAP:
 *   The mountain classification means all seats should compute as mountain — but the FSM mechanism tests whether declared beneficiaries on a mountain indicate a false summit. If the stone's directive is socially constructed rather than physically necessary, the constraint would reclassify as tangled_rope (coordination + extraction from those who might prefer to build lower for economic reasons). The engine will compute this divergence; the authored claim (mountain) and metrics (mountain-profile) are independent.
 *
 * DIRECTIONALITY LOGIC:
 *   All declared beneficiaries (aneyoshi_households, aneyoshi_descendants, towada_village_administration) are structural beneficiaries — the constraint subsidizes their survival and continuity. No victims exist because the constraint extracts nothing; compliance is costless relative to the alternative (death). Directionality for all beneficiary seats is near d=0.0 (full beneficiary). The analytical observer seat (stone_directive) sees the full structure at d=0.5 (symmetric). The excluded commemorative_husk_reading_proponents would experience this constraint as a misclassification if their reading were true, but under this reading they have no structural position.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (tsunami survival) remains live — tsunami risk has not diminished. The constraint's mandate has not atrophied; its function is as vital in 2011 as in 1933. Mandatrophy is not resolved and not applicable. The constraint persists because its founding problem persists, not due to inertia.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is this constraint the behavioral competence reading of the aneyoshi_stone_commitment kernel, and how does it structurally differ from the commemorative husk reading?',
    'Compare the land-use compliance rates, survival outcomes, and institutional transmission mechanisms across the two readings; the behavioral competence reading predicts sustained compliance and causal survival linkage, the commemorative husk reading predicts symbolic observance without behavioral constraint.',
    'If the commemorative husk reading were empirically validated, this constraint''s low extraction and mountain classification would be invalidated for the post-1960 interval — the stone would be reclassified as a piton or scaffold with high theater_ratio. The readings foreclose each other within a single analytical framework.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'This constraint instantiates the behavioral_competence_reading of the aneyoshi_stone_commitment kernel; the sibling commemorative_husk_reading is a distinct constraint with divergent structural properties.').

omega_variable(
    stone_directive_natural_law_ambiguity,
    'Does the stone''s directive function as a genuine natural-law constraint (geographic/tsunami physics) or a constructed social commitment that happens to align with physical reality?',
    'Analyze whether compliance is motivated by belief in the stone''s authority vs. rational assessment of tsunami risk; test via counterfactual — if the stone were removed but tsunami risk remained, would compliance persist?',
    'If compliance depends on the stone''s symbolic authority, the constraint is a constructed commitment (tangled_rope or rope) not a mountain; if compliance tracks physical risk independently, the mountain classification holds. FSM candidate: mountain with declared beneficiaries requires this omega.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(stone_directive_natural_law_ambiguity, conceptual, 'Natural-law vs. constructed commitment ambiguity for a mountain with beneficiaries — FSM trigger documentation.').

omega_variable(
    transmission_mechanism_stability,
    'Is the intergenerational transmission of the stone''s directive structurally stable, or does it depend on contingent social reproduction that could fail?',
    'Track household-level transmission fidelity across the 1960-2011 interval; measure correlation between explicit instruction events and subsequent building decisions; assess vulnerability to demographic collapse or out-migration.',
    'If transmission is contingent, the constraint''s mountain classification holds only conditionally — a demographic shock could convert it to a snare (if compliance is enforced) or piton (if it becomes performative). The 2011 outcome validates the reading but does not prove structural necessity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(transmission_mechanism_stability, empirical, 'Stability of the behavioral transmission mechanism across the 78-year interval.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(aneyoshi_stone_commitment__behavioral_competence_reading, 1933, 2011).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(aneyoshi_stone_commitment__behavioral_competence_reading_tr_t1933, aneyoshi_stone_commitment__behavioral_competence_reading, theater_ratio, 1933, 0.02).
narrative_ontology:measurement(aneyoshi_stone_commitment__behavioral_competence_reading_tr_t1945, aneyoshi_stone_commitment__behavioral_competence_reading, theater_ratio, 1945, 0.03).
narrative_ontology:measurement(aneyoshi_stone_commitment__behavioral_competence_reading_tr_t1960, aneyoshi_stone_commitment__behavioral_competence_reading, theater_ratio, 1960, 0.04).
narrative_ontology:measurement(aneyoshi_stone_commitment__behavioral_competence_reading_tr_t1975, aneyoshi_stone_commitment__behavioral_competence_reading, theater_ratio, 1975, 0.05).
narrative_ontology:measurement(aneyoshi_stone_commitment__behavioral_competence_reading_tr_t1990, aneyoshi_stone_commitment__behavioral_competence_reading, theater_ratio, 1990, 0.05).
narrative_ontology:measurement(aneyoshi_stone_commitment__behavioral_competence_reading_tr_t2011, aneyoshi_stone_commitment__behavioral_competence_reading, theater_ratio, 2011, 0.05).

% Extraction over time
narrative_ontology:measurement(aneyoshi_stone_commitment__behavioral_competence_reading_be_t1933, aneyoshi_stone_commitment__behavioral_competence_reading, base_extractiveness, 1933, 0.02).
narrative_ontology:measurement(aneyoshi_stone_commitment__behavioral_competence_reading_be_t1945, aneyoshi_stone_commitment__behavioral_competence_reading, base_extractiveness, 1945, 0.02).
narrative_ontology:measurement(aneyoshi_stone_commitment__behavioral_competence_reading_be_t1960, aneyoshi_stone_commitment__behavioral_competence_reading, base_extractiveness, 1960, 0.03).
narrative_ontology:measurement(aneyoshi_stone_commitment__behavioral_competence_reading_be_t1975, aneyoshi_stone_commitment__behavioral_competence_reading, base_extractiveness, 1975, 0.03).
narrative_ontology:measurement(aneyoshi_stone_commitment__behavioral_competence_reading_be_t1990, aneyoshi_stone_commitment__behavioral_competence_reading, base_extractiveness, 1990, 0.03).
narrative_ontology:measurement(aneyoshi_stone_commitment__behavioral_competence_reading_be_t2011, aneyoshi_stone_commitment__behavioral_competence_reading, base_extractiveness, 2011, 0.03).

% Suppression requirement over time
narrative_ontology:measurement(aneyoshi_stone_commitment__behavioral_competence_reading_su_t1933, aneyoshi_stone_commitment__behavioral_competence_reading, suppression_requirement, 1933, 0.05).
narrative_ontology:measurement(aneyoshi_stone_commitment__behavioral_competence_reading_su_t1945, aneyoshi_stone_commitment__behavioral_competence_reading, suppression_requirement, 1945, 0.08).
narrative_ontology:measurement(aneyoshi_stone_commitment__behavioral_competence_reading_su_t1960, aneyoshi_stone_commitment__behavioral_competence_reading, suppression_requirement, 1960, 0.1).
narrative_ontology:measurement(aneyoshi_stone_commitment__behavioral_competence_reading_su_t1975, aneyoshi_stone_commitment__behavioral_competence_reading, suppression_requirement, 1975, 0.11).
narrative_ontology:measurement(aneyoshi_stone_commitment__behavioral_competence_reading_su_t1990, aneyoshi_stone_commitment__behavioral_competence_reading, suppression_requirement, 1990, 0.12).
narrative_ontology:measurement(aneyoshi_stone_commitment__behavioral_competence_reading_su_t2011, aneyoshi_stone_commitment__behavioral_competence_reading, suppression_requirement, 2011, 0.12).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(aneyoshi_stone_commitment__behavioral_competence_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(aneyoshi_stone_commitment__behavioral_competence_reading, 0.08).
narrative_ontology:affects_constraint(aneyoshi_stone_commitment__behavioral_competence_reading, aneyoshi_stone_commitment__commemorative_husk_reading).

% DUAL FORMULATION NOTE:
% The aneyoshi_stone_commitment kernel decomposes into two constraint stories: this behavioral_competence_reading (mountain, very low epsilon, causal survival linkage) and the commemorative_husk_reading (piton or scaffold, high theater_ratio, symbolic observance only). They are linked via affects_constraints. The decomposition follows the ε-invariance principle: the same physical stone yields two structurally distinct constraints depending on whether its directive retains behavioral force.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
