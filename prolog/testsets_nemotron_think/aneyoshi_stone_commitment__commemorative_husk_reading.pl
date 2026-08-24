% ============================================================================
% CONSTRAINT STORY: aneyoshi_stone_commitment__commemorative_husk_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_aneyoshi_stone_commitment__commemorative_husk_reading, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: aneyoshi_stone_commitment__commemorative_husk_reading
 *   human_readable: Aneyoshi Stone as Commemorative Husk
 *   domain: disaster_anthropology/commitment_systems/temporal_institutional_analysis
 *
 * SUMMARY:
 *   The Aneyoshi stone (1896) bears the inscription: 'High dwellings are the
 *   peace and harmony of our descendants. Remember the calamity of the great
 *   tsunamis. Do not build any homes below this point.' In the
 *   commemorative_husk_reading, this directive has decayed into a memorial
 *   observance: annual ceremonies honor the stone, tourism thrives on its
 *   narrative, but land-use decisions — including post-2011 reconstruction —
 *   proceed independently of its warning line. The stone functions as a
 *   museum piece; its behavioral authority is zero. This reading instantiates
 *   high extractiveness (epsilon) because the constraint occupies the
 *   institutional slot for 'tsunami risk governance' while delivering no
 *   behavioral constraint, extracting commemorative labor and symbolic
 *   capital without protective return.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(aneyoshi_stone_commitment__commemorative_husk_reading, 0.68).
domain_priors:suppression_score(aneyoshi_stone_commitment__commemorative_husk_reading, 0.15).
domain_priors:theater_ratio(aneyoshi_stone_commitment__commemorative_husk_reading, 0.78).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(aneyoshi_stone_commitment__commemorative_husk_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(aneyoshi_stone_commitment__commemorative_husk_reading, suppression_requirement, 0.15).
narrative_ontology:constraint_metric(aneyoshi_stone_commitment__commemorative_husk_reading, theater_ratio, 0.78).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(aneyoshi_stone_commitment__commemorative_husk_reading, accessibility_collapse, 0.22).
narrative_ontology:constraint_metric(aneyoshi_stone_commitment__commemorative_husk_reading, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(aneyoshi_stone_commitment__commemorative_husk_reading, piton).
narrative_ontology:human_readable(aneyoshi_stone_commitment__commemorative_husk_reading, "Aneyoshi Stone as Commemorative Husk").
narrative_ontology:topic_domain(aneyoshi_stone_commitment__commemorative_husk_reading, "disaster_anthropology/commitment_systems/temporal_institutional_analysis").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(aneyoshi_stone_commitment__commemorative_husk_reading, '13e37690-3e5a-4746-846a-a973537dbf83').
narrative_ontology:cs_kernel_codification('13e37690-3e5a-4746-846a-a973537dbf83', fixed_text).
narrative_ontology:cs_authority_grounding('13e37690-3e5a-4746-846a-a973537dbf83', lineage).
narrative_ontology:cs_interpretation_layer_present('13e37690-3e5a-4746-846a-a973537dbf83').
narrative_ontology:cs_reading_relation('13e37690-3e5a-4746-846a-a973537dbf83', aneyoshi_stone_commitment__behavioral_competence_reading, coexists_with).
narrative_ontology:cs_axiom('13e37690-3e5a-4746-846a-a973537dbf83', foundational, memorial_observance_sufficient).
narrative_ontology:cs_axiom_status(memorial_observance_sufficient, holdable).
narrative_ontology:cs_axiom_grounding('13e37690-3e5a-4746-846a-a973537dbf83', memorial_observance_sufficient, conventional).
narrative_ontology:cs_axiom('13e37690-3e5a-4746-846a-a973537dbf83', secondary, behavioral_compliance_not_required).
narrative_ontology:cs_axiom_status(behavioral_compliance_not_required, holdable).
narrative_ontology:cs_axiom_grounding('13e37690-3e5a-4746-846a-a973537dbf83', behavioral_compliance_not_required, conventional).
narrative_ontology:cs_reference_frame('13e37690-3e5a-4746-846a-a973537dbf83', ancestral_behavioral_mandate).
narrative_ontology:cs_drift_state('13e37690-3e5a-4746-846a-a973537dbf83', post_2011_tsunami, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('13e37690-3e5a-4746-846a-a973537dbf83', '').
narrative_ontology:cs_kernel_id(aneyoshi_stone_commitment__commemorative_husk_reading, aneyoshi_stone_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(aneyoshi_stone_commitment__commemorative_husk_reading, collective_memory_community).
narrative_ontology:constraint_beneficiary(aneyoshi_stone_commitment__commemorative_husk_reading, local_tourism_operators).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(aneyoshi_stone_commitment__commemorative_husk_reading, local_residents).
narrative_ontology:constraint_victim(aneyoshi_stone_commitment__commemorative_husk_reading, local_residents).
narrative_ontology:constraint_vindicates(aneyoshi_stone_commitment__commemorative_husk_reading, memorial_observance_sufficient_for_ancestral_honor).
narrative_ontology:constraint_vindicates(aneyoshi_stone_commitment__commemorative_husk_reading, symbolic_continuity_substitutes_for_behavioral_compliance).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintain the stone, organize annual memorial ceremonies, and curate its narrative for visitors. They administer the commemorative practice but have no authority over land-use decisions. Their role depends on the stone remaining a living symbol; they would lose institutional purpose if it were treated as a mere artifact.
narrative_ontology:constraint_stakeholder(aneyoshi_stone_commitment__commemorative_husk_reading, memorial_keepers, agenda_setter,
    organized, generational, constrained, local).

% Participate in memorial observances and bear the opportunity cost of maintaining a symbolic commitment that does not constrain building choices. Some residents live below the stone's warning line without social sanction. They gain community identity and tourism adjacency benefits, but the stone's behavioral void means tsunami risk is managed through modern warning systems, not the ancestral directive.
narrative_ontology:constraint_stakeholder(aneyoshi_stone_commitment__commemorative_husk_reading, local_residents, payer,
    moderate, biographical, mobile, local).
narrative_ontology:stakeholder_secondary_role(aneyoshi_stone_commitment__commemorative_husk_reading, local_residents, beneficiary).

% Profit from the stone as a disaster-tourism destination. They promote the commemorative narrative ('the stone that saved Aneyoshi') without emphasizing that no current residents follow its directive. Their revenue depends on the stone's symbolic potency, not its operational force.
narrative_ontology:constraint_stakeholder(aneyoshi_stone_commitment__commemorative_husk_reading, local_tourism_operators, beneficiary,
    moderate, biographical, mobile, regional).

% Study the stone as a case of commitment decay and memorial substitution. They document the gap between the inscription's behavioral command and actual land-use patterns, and track how the commemorative reading displaces the behavioral reading in public discourse.
narrative_ontology:constraint_stakeholder(aneyoshi_stone_commitment__commemorative_husk_reading, disaster_researchers, observer,
    analytical, civilizational, analytical, global).

% The 1896 survivors who erected the stone with the explicit intent of restricting settlement to safe elevations. They are structurally excluded from the current conversation; their behavioral intent has been replaced by a memorial reading they did not authorize and would not recognize as compliance.
narrative_ontology:constraint_stakeholder(aneyoshi_stone_commitment__commemorative_husk_reading, ancestral_erectors, excluded,
    powerless, generational, trapped, local).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates collective memory and community identity around the shared disaster experience of 1896 and 2011, providing a focal point for annual observance and intergenerational narrative transmission.
% TRANSFER_FUNCTION: Moves reverence and commemorative labor from living behavioral practice (building only above the line) to static symbolic observance (ceremonies, tourism, narrative curation). Moves tourism revenue to local operators. Moves disaster-risk management from the stone's directive to modern technological systems (early warning, evacuation planning).
% ABSENT_VOICES: The ancestral erectors who intended a behavioral land-use constraint, not a memorial. Future generations who may face tsunami risk without the stone's operational guidance. The behavioral_competence_reading proponents (local elders, some disaster historians) who argue the stone's directive was followed in 2011 and remains valid — they are present in discourse but their reading is not instantiated in current land-use practice.
% DISAPPEARANCE_RATIONALE: If the stone vanished overnight, land-use patterns would not shift — residents already build below the line without sanction. The annual ceremony would cease, tourism would decline, and the community would lose a symbolic anchor, but no behavioral constraint would be lifted because none is currently operative. The world rearranges only symbolically.
% FOUNDING_PROBLEM: Prevent tsunami deaths by restricting permanent settlement to elevations above the stone's position, based on the 1896 Meiji tsunami inundation line.
% FOUNDING_PROBLEM_CORROBORATION: Geological surveys confirm tsunami risk persists at the same elevations. Modern disaster management (JMA early warning, evacuation routes, seawalls) is the corroborated current solution, attested by municipal government and national agencies — not by the stone's directive. The behavioral_competence_reading proponents contest 'dead' status, citing 2011 survival as evidence the constraint remained live; however, post-2011 reconstruction below the stone line and municipal zoning maps corroborate the 'dead' assessment.
narrative_ontology:disappearance_verdict(aneyoshi_stone_commitment__commemorative_husk_reading, world_unchanged).
narrative_ontology:founding_problem_status(aneyoshi_stone_commitment__commemorative_husk_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(aneyoshi_stone_commitment__commemorative_husk_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(aneyoshi_stone_commitment__commemorative_husk_reading, 'none', 1).
narrative_ontology:epsilon_provenance(aneyoshi_stone_commitment__commemorative_husk_reading, 0.68, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(aneyoshi_stone_commitment__commemorative_husk_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(aneyoshi_stone_commitment__commemorative_husk_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(aneyoshi_stone_commitment__commemorative_husk_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68) is high because the stone claims the authority of a life-saving rule while functioning as a symbol — the gap between claim and operation is the extraction. Theater_ratio (0.78) is very high: the annual ceremony, guide narratives, and municipal signage perform compliance while actual building ignores the line. Suppression (0.15) is low: no social or legal sanction for building below the stone. Accessibility_collapse (0.22) is low: the alternative (building below) is fully accessible and practiced. Resistance (0.08) is near zero: no constituency resists the stone because it imposes no cost. The measurements trace the 125-year decay from behavioral constraint (low theater, moderate suppression) to commemorative husk (high theater, near-zero suppression).
 *
 * PERSPECTIVAL GAP:
 *   From the memorial keeper seat, the stone is a living Rope coordinating memory and identity. From the resident seat, it is a Piton — a degraded constraint they navigate around. From the researcher seat, it is a case study in commitment decay. The engine computes this divergence; the authored claim (piton) reflects the structural reality that the constraint's primary function (behavioral land-use restriction) has atrophied while the form persists theatrically.
 *
 * DIRECTIONALITY LOGIC:
 *   Memorial keepers are agenda_setters: they curate the narrative but cannot enforce the directive (d ~0.3 — they benefit symbolically but lack power). Local residents are payers (commemorative labor, opportunity cost of maintaining a non-functional constraint) and secondary beneficiaries (identity, tourism adjacency) — d ~0.55. Tourism operators are beneficiaries (revenue from symbolic potency) — d ~0.2. Disaster researchers are analytical observers (d=0.0). Ancestral erectors are excluded: their intent is structurally overridden; they would be full targets (d=1.0) if present. The engine computes per-seat types from these structural positions.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (tsunami mortality prevention) is dead as a behavioral mandate — modern systems solve it differently. The stone persists as a Piton because no party benefits enough to maintain it as a behavioral rule (residents want building freedom) and no party is hurt enough to fix it (modern warning systems provide protection). The commemorative reading is the mandatrophy resolution: the constraint rebrands its own obsolescence as fulfilled duty.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    residual_behavioral_influence,
    'Does the stone''s symbolic presence exert any subtle normative influence on land-use decisions (e.g., marginal hesitation, social pressure), or is the behavioral void total?',
    'Micro-ethnographic study of building permit discussions, real-estate marketing language, and resident interviews in Aneyoshi and comparable villages without memorial stones.',
    'If residual influence exists, extractiveness is lower (some coordination function persists) and the piton classification weakens toward tangled_rope. If void is total, the high-epsilon piton classification is confirmed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(residual_behavioral_influence, empirical, 'Whether the commemorative husk retains any behavioral shadow.').

omega_variable(
    memorial_suffices_as_cover,
    'Is the ''memorial observance suffices'' axiom a genuine cultural resolution of the ancestral commitment, or a cover story that displaces the uncomfortable fact of behavioral abandonment?',
    'Discourse analysis of municipal records, ceremony speeches, and school curricula across generations; comparison with other Japanese tsunami stones that retained behavioral force.',
    'If cover story, the constraint is a snare (extraction via displacement of accountability). If genuine resolution, the piton classification holds — the community has collectively redefined the commitment''s terms.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(memorial_suffices_as_cover, conceptual, 'Whether the commemorative reading is an honest redefinition or a displacement maneuver.').

omega_variable(
    twenty_eleven_survival_attribution,
    'Was Aneyoshi''s 2011 survival due to the stone''s behavioral constraint (behavioral_competence_reading) or to luck/topography/modern systems (commemorative_husk_reading)?',
    'Forensic reconstruction of 2011 inundation levels relative to the stone line, building stock analysis, and survivor testimony correlation.',
    'If behavioral_competence_reading is empirically correct, this reading''s high epsilon is falsified and the kernel resolves to a single constraint (mountain or rope). If commemorative_husk_reading is correct, the kernel remains contested and the sibling reading is a false summit.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(twenty_eleven_survival_attribution, empirical, 'The empirical core of the kernel contest — which reading matches the 2011 facts.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(aneyoshi_stone_commitment__commemorative_husk_reading, 0, 125).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(aneyoshi_husk_tr_t0, aneyoshi_stone_commitment__commemorative_husk_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement_basis(aneyoshi_husk_tr_t0, observed).
narrative_ontology:measurement(aneyoshi_husk_tr_t30, aneyoshi_stone_commitment__commemorative_husk_reading, theater_ratio, 30, 0.15).
narrative_ontology:measurement_basis(aneyoshi_husk_tr_t30, observed).
narrative_ontology:measurement(aneyoshi_husk_tr_t60, aneyoshi_stone_commitment__commemorative_husk_reading, theater_ratio, 60, 0.35).
narrative_ontology:measurement_basis(aneyoshi_husk_tr_t60, observed).
narrative_ontology:measurement(aneyoshi_husk_tr_t90, aneyoshi_stone_commitment__commemorative_husk_reading, theater_ratio, 90, 0.58).
narrative_ontology:measurement_basis(aneyoshi_husk_tr_t90, observed).
narrative_ontology:measurement(aneyoshi_husk_tr_t115, aneyoshi_stone_commitment__commemorative_husk_reading, theater_ratio, 115, 0.72).
narrative_ontology:measurement_basis(aneyoshi_husk_tr_t115, observed).
narrative_ontology:measurement(aneyoshi_husk_tr_t125, aneyoshi_stone_commitment__commemorative_husk_reading, theater_ratio, 125, 0.78).
narrative_ontology:measurement_basis(aneyoshi_husk_tr_t125, observed).

% Extraction over time
narrative_ontology:measurement(aneyoshi_husk_be_t0, aneyoshi_stone_commitment__commemorative_husk_reading, base_extractiveness, 0, 0.12).
narrative_ontology:measurement_basis(aneyoshi_husk_be_t0, observed).
narrative_ontology:measurement(aneyoshi_husk_be_t30, aneyoshi_stone_commitment__commemorative_husk_reading, base_extractiveness, 30, 0.18).
narrative_ontology:measurement_basis(aneyoshi_husk_be_t30, observed).
narrative_ontology:measurement(aneyoshi_husk_be_t60, aneyoshi_stone_commitment__commemorative_husk_reading, base_extractiveness, 60, 0.32).
narrative_ontology:measurement_basis(aneyoshi_husk_be_t60, observed).
narrative_ontology:measurement(aneyoshi_husk_be_t90, aneyoshi_stone_commitment__commemorative_husk_reading, base_extractiveness, 90, 0.51).
narrative_ontology:measurement_basis(aneyoshi_husk_be_t90, observed).
narrative_ontology:measurement(aneyoshi_husk_be_t115, aneyoshi_stone_commitment__commemorative_husk_reading, base_extractiveness, 115, 0.63).
narrative_ontology:measurement_basis(aneyoshi_husk_be_t115, observed).
narrative_ontology:measurement(aneyoshi_husk_be_t125, aneyoshi_stone_commitment__commemorative_husk_reading, base_extractiveness, 125, 0.68).
narrative_ontology:measurement_basis(aneyoshi_husk_be_t125, observed).

% Suppression requirement over time
narrative_ontology:measurement(aneyoshi_husk_su_t0, aneyoshi_stone_commitment__commemorative_husk_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement_basis(aneyoshi_husk_su_t0, observed).
narrative_ontology:measurement(aneyoshi_husk_su_t30, aneyoshi_stone_commitment__commemorative_husk_reading, suppression_requirement, 30, 0.38).
narrative_ontology:measurement_basis(aneyoshi_husk_su_t30, observed).
narrative_ontology:measurement(aneyoshi_husk_su_t60, aneyoshi_stone_commitment__commemorative_husk_reading, suppression_requirement, 60, 0.28).
narrative_ontology:measurement_basis(aneyoshi_husk_su_t60, observed).
narrative_ontology:measurement(aneyoshi_husk_su_t90, aneyoshi_stone_commitment__commemorative_husk_reading, suppression_requirement, 90, 0.22).
narrative_ontology:measurement_basis(aneyoshi_husk_su_t90, observed).
narrative_ontology:measurement(aneyoshi_husk_su_t115, aneyoshi_stone_commitment__commemorative_husk_reading, suppression_requirement, 115, 0.16).
narrative_ontology:measurement_basis(aneyoshi_husk_su_t115, observed).
narrative_ontology:measurement(aneyoshi_husk_su_t125, aneyoshi_stone_commitment__commemorative_husk_reading, suppression_requirement, 125, 0.15).
narrative_ontology:measurement_basis(aneyoshi_husk_su_t125, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(aneyoshi_stone_commitment__commemorative_husk_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(aneyoshi_stone_commitment__commemorative_husk_reading, 0.08).
narrative_ontology:affects_constraint(aneyoshi_stone_commitment__commemorative_husk_reading, aneyoshi_stone_commitment__behavioral_competence_reading).

% DUAL FORMULATION NOTE:
% Kernel aneyoshi_stone_commitment decomposes into two readings with divergent epsilon: behavioral_competence_reading (low epsilon, Mountain/Rope) claims the stone retained operational land-use authority; commemorative_husk_reading (high epsilon, Piton) claims it decayed to symbolic observance. The decomposition follows the epsilon-invariance principle: the same physical stone instantiates two structurally distinct constraints depending on whether its directive governs building decisions.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(aneyoshi_stone_commitment__commemorative_husk_reading, moderate, 0.55).
constraint_indexing:directionality_override(aneyoshi_stone_commitment__commemorative_husk_reading, organized, 0.3).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
