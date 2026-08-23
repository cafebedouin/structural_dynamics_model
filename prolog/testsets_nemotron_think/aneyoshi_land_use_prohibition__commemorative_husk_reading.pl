% ============================================================================
% CONSTRAINT STORY: aneyoshi_land_use_prohibition__commemorative_husk_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_aneyoshi_land_use_prohibition__commemorative_husk_reading, []).

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
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: aneyoshi_land_use_prohibition__commemorative_husk_reading
 *   human_readable: Aneyoshi Tsunami Stone — Commemorative Husk Reading
 *   domain: disaster_anthropology/commitment_systems/temporal_institutional
 *
 * SUMMARY:
 *   The Aneyoshi stone, erected after the 1933 Showa Sanriku tsunami, marks a
 *   'do not build below this line' prohibition that was operationally
 *   enforced for 78 years — no dwellings were constructed below the line
 *   until after the 2011 Tohoku tsunami. The commemorative_husk_reading holds
 *   that the prohibition has decayed into a pure symbol: the stone is
 *   maintained as a historical memorial and heritage site, but the land-use
 *   rule it encodes is no longer enforced. Development interests benefit from
 *   this decay (building in the hazard zone), while future residents bear the
 *   catastrophic risk. The constraint persists as theater — the stone is
 *   polished, signposted, and toured — but its behavioral force is gone. This
 *   reading stands in structural tension with the
 *   behavioral_competence_reading, which asserts the prohibition remains a
 *   live coordination mechanism.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(aneyoshi_land_use_prohibition__commemorative_husk_reading, 0.75).
domain_priors:suppression_score(aneyoshi_land_use_prohibition__commemorative_husk_reading, 0.2).
domain_priors:theater_ratio(aneyoshi_land_use_prohibition__commemorative_husk_reading, 0.82).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(aneyoshi_land_use_prohibition__commemorative_husk_reading, extractiveness, 0.75).
narrative_ontology:constraint_metric(aneyoshi_land_use_prohibition__commemorative_husk_reading, suppression_requirement, 0.2).
narrative_ontology:constraint_metric(aneyoshi_land_use_prohibition__commemorative_husk_reading, theater_ratio, 0.82).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(aneyoshi_land_use_prohibition__commemorative_husk_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(aneyoshi_land_use_prohibition__commemorative_husk_reading, resistance, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(aneyoshi_land_use_prohibition__commemorative_husk_reading, piton).
narrative_ontology:human_readable(aneyoshi_land_use_prohibition__commemorative_husk_reading, "Aneyoshi Tsunami Stone — Commemorative Husk Reading").
narrative_ontology:topic_domain(aneyoshi_land_use_prohibition__commemorative_husk_reading, "disaster_anthropology/commitment_systems/temporal_institutional").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(aneyoshi_land_use_prohibition__commemorative_husk_reading, 'd21210e5-a5d1-4f53-9d29-e55971331a20').
narrative_ontology:cs_kernel_codification('d21210e5-a5d1-4f53-9d29-e55971331a20', fixed_text).
narrative_ontology:cs_authority_grounding('d21210e5-a5d1-4f53-9d29-e55971331a20', lineage).
narrative_ontology:cs_interpretation_layer_present('d21210e5-a5d1-4f53-9d29-e55971331a20').
narrative_ontology:cs_reading_relation('d21210e5-a5d1-4f53-9d29-e55971331a20', aneyoshi_land_use_prohibition__behavioral_competence_reading, coexists_with).
narrative_ontology:cs_axiom('d21210e5-a5d1-4f53-9d29-e55971331a20', foundational, prohibition_decayed_to_symbol).
narrative_ontology:cs_axiom_status(prohibition_decayed_to_symbol, holdable).
narrative_ontology:cs_axiom_grounding('d21210e5-a5d1-4f53-9d29-e55971331a20', prohibition_decayed_to_symbol, conventional).
narrative_ontology:cs_axiom('d21210e5-a5d1-4f53-9d29-e55971331a20', secondary, historical_memory_suffices_for_duty).
narrative_ontology:cs_axiom_status(historical_memory_suffices_for_duty, holdable).
narrative_ontology:cs_axiom_grounding('d21210e5-a5d1-4f53-9d29-e55971331a20', historical_memory_suffices_for_duty, deontological).
narrative_ontology:cs_reference_frame('d21210e5-a5d1-4f53-9d29-e55971331a20', ancestral_warning_tradition).
narrative_ontology:cs_drift_state('d21210e5-a5d1-4f53-9d29-e55971331a20', contemporary_development_era, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('d21210e5-a5d1-4f53-9d29-e55971331a20', '').
narrative_ontology:cs_kernel_id(aneyoshi_land_use_prohibition__commemorative_husk_reading, aneyoshi_land_use_prohibition).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(aneyoshi_land_use_prohibition__commemorative_husk_reading, development_interests).
narrative_ontology:constraint_beneficiary(aneyoshi_land_use_prohibition__commemorative_husk_reading, local_government_tax_base).
narrative_ontology:constraint_beneficiary(aneyoshi_land_use_prohibition__commemorative_husk_reading, historical_preservation_groups).
narrative_ontology:constraint_victim(aneyoshi_land_use_prohibition__commemorative_husk_reading, future_residents_below_line).
narrative_ontology:constraint_victim(aneyoshi_land_use_prohibition__commemorative_husk_reading, disaster_vulnerable_populations).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(aneyoshi_land_use_prohibition__commemorative_husk_reading, local_government).
narrative_ontology:constraint_vindicates(aneyoshi_land_use_prohibition__commemorative_husk_reading, historical_memory_preservation).
narrative_ontology:constraint_vindicates(aneyoshi_land_use_prohibition__commemorative_husk_reading, cultural_heritage_value).
narrative_ontology:constraint_vindicates(aneyoshi_land_use_prohibition__commemorative_husk_reading, ancestral_warning_tradition).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Real estate developers and landowners who build or sell property below the stone's elevation line. They benefit from the prohibition's non-enforcement — no regulatory barrier to development, higher land values, and no requirement for costly tsunami mitigation. They lobby against enforcement and fund political campaigns to maintain the status quo.
narrative_ontology:constraint_stakeholder(aneyoshi_land_use_prohibition__commemorative_husk_reading, development_interests, beneficiary,
    powerful, biographical, arbitrage, local).

% People who will live in housing built below the stone's line. They bear the catastrophic risk when the next tsunami strikes. They have no voice in current land-use decisions (not yet born or not yet resident), no exit from the hazard zone once settled, and no compensation mechanism. Their safety is the extraction residue.
narrative_ontology:constraint_stakeholder(aneyoshi_land_use_prohibition__commemorative_husk_reading, future_residents_below_line, payer,
    powerless, generational, trapped, local).

% Municipal authorities who maintain the stone as a heritage site and tourism asset but do not enforce the land-use prohibition. They collect property tax revenue from development below the line and receive heritage funding for stone maintenance. Their incentive structure rewards symbolic compliance over behavioral enforcement.
narrative_ontology:constraint_stakeholder(aneyoshi_land_use_prohibition__commemorative_husk_reading, local_government, agenda_setter,
    institutional, biographical, constrained, local).
narrative_ontology:stakeholder_secondary_role(aneyoshi_land_use_prohibition__commemorative_husk_reading, local_government, beneficiary).

% NGOs and cultural agencies that maintain the stone as a memorial and educational site. They receive grants, tourism revenue, and institutional prestige from the stone's commemorative function. They advocate for preservation of the stone but not for enforcement of the prohibition, treating the two as separable.
narrative_ontology:constraint_stakeholder(aneyoshi_land_use_prohibition__commemorative_husk_reading, historical_preservation_groups, beneficiary,
    organized, generational, mobile, national).

% National and prefectural disaster agencies that map hazard zones and issue evacuation plans. They acknowledge the stone's historical significance but treat the area below it as a standard hazard zone requiring engineering mitigation, not a prohibited zone. They have no authority to enforce the ancestral prohibition.
narrative_ontology:constraint_stakeholder(aneyoshi_land_use_prohibition__commemorative_husk_reading, disaster_management_authorities, observer,
    institutional, biographical, analytical, national).

% Local elders, some tsunami survivors, and community members who maintain the behavioral reading — they believe the prohibition is still live and that building below the line violates ancestral law. They are excluded from planning decisions and their objections are treated as cultural sentiment rather than regulatory input.
narrative_ontology:constraint_stakeholder(aneyoshi_land_use_prohibition__commemorative_husk_reading, behavioral_competence_adherents, excluded,
    moderate, generational, identity_locked, local).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Originally coordinated safe settlement patterns by encoding tsunami run-up elevation into a durable physical marker that functioned as a binding land-use rule across generations. In this reading, the coordination function has atrophied — the stone now coordinates only commemorative practice (visitation, education, heritage tourism) and no longer coordinates settlement behavior.
% TRANSFER_FUNCTION: Moves tsunami risk from development interests (who profit from building in the hazard zone) to future residents (who bear mortality risk). Moves cultural capital and heritage funding to preservation groups and local government. Moves regulatory cost off the municipal budget — non-enforcement is cheaper than enforcement.
% ABSENT_VOICES: Future residents below the line (not yet born, cannot object). Past tsunami victims (deceased, their testimony survives only in the stone's inscription). The behavioral_competence_adherents are physically present but institutionally excluded — their reading is treated as folklore, not policy.
% DISAPPEARANCE_RATIONALE: If the stone vanished overnight: development interests and local government would lose a heritage tourism asset but face no regulatory change (the prohibition is already unenforced). Preservation groups would lose their focal object. Behavioral_competence_adherents would lose the physical anchor of their practice. Disaster authorities' hazard maps would be unchanged. The parties dispute whether the stone's symbolic presence still exerts any normative restraint on building decisions.
% FOUNDING_PROBLEM: Prevent tsunami mortality by restricting permanent settlement to elevations above the maximum historical run-up line, using a durable physical marker that would outlive living memory and institutional continuity.
% FOUNDING_PROBLEM_CORROBORATION: Geological surveys confirm the stone's line matches 1896 and 1933 tsunami run-up elevations. Historical records document 78 years of compliance (1933–2011) with zero dwellings built below the line. Contemporary land-use maps show multiple residential developments below the line post-2011. The founding problem (tsunami mortality prevention) is empirically dead as an operational constraint — the prohibition no longer prevents building in the hazard zone — though the hazard itself persists.
narrative_ontology:disappearance_verdict(aneyoshi_land_use_prohibition__commemorative_husk_reading, contested).
narrative_ontology:founding_problem_status(aneyoshi_land_use_prohibition__commemorative_husk_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(aneyoshi_land_use_prohibition__commemorative_husk_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(aneyoshi_land_use_prohibition__commemorative_husk_reading, 'none', 1).
narrative_ontology:epsilon_provenance(aneyoshi_land_use_prohibition__commemorative_husk_reading, 0.75, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(aneyoshi_land_use_prohibition__commemorative_husk_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(aneyoshi_land_use_prohibition__commemorative_husk_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(aneyoshi_land_use_prohibition__commemorative_husk_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.75) because development interests extract land value and avoided mitigation costs by treating the prohibition as non-binding, while future residents pay with existential risk. Theater ratio is very high (0.82) — the stone's maintenance, signage, and heritage designation are real activities, but they serve commemorative not regulatory function. Suppression is low (0.2) — no active enforcement prevents building below the line; alternatives (building in the hazard zone) are open and taken. Accessibility collapse is low (0.3) — the hazard zone is physically and legally accessible. Resistance is minimal (0.15) — little organized opposition to development below the line. The measurement series tracks the 1933–2024 interval: behavioral compliance (low extraction, high suppression, near-zero theater) decaying post-2011 into commemorative husk (high extraction, low suppression, high theater).
 *
 * PERSPECTIVAL GAP:
 *   The behavioral_competence_adherents experience the constraint as a live mountain (ancestral law, immutable). Development interests experience it as a piton (theatrical remnant, safely ignorable). Future residents experience it as a snare (the symbol's presence legitimizes development that will kill them). The engine computes these divergent seat types from the same structural data — the authored claim (piton) reflects the generating model's structural assessment, not any single seat's perception.
 *
 * DIRECTIONALITY LOGIC:
 *   Development interests and local government are structural beneficiaries (d near 0.0) — they collect rents and revenue from the constraint's decay. Future residents are full targets (d near 1.0) — they bear the extracted risk with no exit. Preservation groups are incidental beneficiaries (d ~ 0.3) — they gain cultural capital but don't drive the decay. Disaster authorities are analytical observers (d = 0.5). Behavioral_competence_adherents are identity-locked targets (d ~ 0.8) — their self-concept is fused to the prohibition's vitality, making exit from the reading psychologically prohibitive even as the constraint hollows out.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (tsunami mortality prevention via settlement restriction) is dead — the prohibition no longer prevents building in the hazard zone. The arrangement persists because its commemorative function serves current beneficiaries (development revenue, heritage funding, tourism) and because the behavioral_competence_adherents' identity-locked commitment prevents formal repeal. This is classic mandatrophy: the mandate (prevent tsunami death) has outlived its function, but the constraint persists as a husk that now enables the opposite of its founding purpose.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_framing,
    'Is the commemorative_husk_reading a descriptively accurate account of the constraint''s current operation, or a normative claim that accelerates the decay it describes?',
    'Longitudinal observation of whether the stone''s symbolic maintenance correlates with continued non-enforcement, or whether heritage designation creates new regulatory pathways (e.g., buffer zones, building codes) that partially restore coordination function.',
    'If the reading is performative — describing the husk accelerates its formation — then authoring this constraint story participates in the decay. If descriptive, it documents a completed transition. Classification shifts from piton (decayed) to snare (actively maintained decay) if the reading itself serves extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_framing, conceptual, 'Whether this reading describes or constitutes the constraint''s decay.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the absence of enforcement structural (legal framework permits development) or internalized (community no longer believes the prohibition binds)?',
    'Post-2011 building permit records: if permits are granted routinely below the line, suppression is structural. If permit applications below the line are rare despite legal permission, suppression is internalized (the constraint still coordinates behavior without enforcement).',
    'If internalized, the constraint retains coordination function (lower extractiveness, higher accessibility_collapse) — the stone still works without enforcement. If structural, the piton classification holds — theater without function.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Whether the constraint''s decay is enforced from outside or carried from within.').

omega_variable(
    extraction_measurement_without_enforcement,
    'How to measure extractiveness when the constraint does not actively extract but its decay enables extraction by third parties?',
    'Counterfactual land-value analysis: compare property values below the line under current non-enforcement vs. a hypothetical enforced prohibition. The delta is the extraction enabled by the constraint''s decay.',
    'If the delta is large, the piton''s theater serves as extraction infrastructure — the commemorative function legitimizes the non-enforcement. If small, the decay is genuine institutional atrophy without extractive intent.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_measurement_without_enforcement, empirical, 'Measuring extraction enabled by constraint decay rather than constraint operation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(aneyoshi_land_use_prohibition__commemorative_husk_reading, 0, 91).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(aney_tr_t0, aneyoshi_land_use_prohibition__commemorative_husk_reading, theater_ratio, 0, 0.02).
narrative_ontology:measurement(aney_tr_t30, aneyoshi_land_use_prohibition__commemorative_husk_reading, theater_ratio, 30, 0.1).
narrative_ontology:measurement(aney_tr_t60, aneyoshi_land_use_prohibition__commemorative_husk_reading, theater_ratio, 60, 0.35).
narrative_ontology:measurement(aney_tr_t78, aneyoshi_land_use_prohibition__commemorative_husk_reading, theater_ratio, 78, 0.68).
narrative_ontology:measurement(aney_tr_t85, aneyoshi_land_use_prohibition__commemorative_husk_reading, theater_ratio, 85, 0.78).
narrative_ontology:measurement(aney_tr_t91, aneyoshi_land_use_prohibition__commemorative_husk_reading, theater_ratio, 91, 0.82).

% Extraction over time
narrative_ontology:measurement(aney_be_t0, aneyoshi_land_use_prohibition__commemorative_husk_reading, base_extractiveness, 0, 0.05).
narrative_ontology:measurement(aney_be_t30, aneyoshi_land_use_prohibition__commemorative_husk_reading, base_extractiveness, 30, 0.15).
narrative_ontology:measurement(aney_be_t60, aneyoshi_land_use_prohibition__commemorative_husk_reading, base_extractiveness, 60, 0.35).
narrative_ontology:measurement(aney_be_t78, aneyoshi_land_use_prohibition__commemorative_husk_reading, base_extractiveness, 78, 0.65).
narrative_ontology:measurement(aney_be_t85, aneyoshi_land_use_prohibition__commemorative_husk_reading, base_extractiveness, 85, 0.72).
narrative_ontology:measurement(aney_be_t91, aneyoshi_land_use_prohibition__commemorative_husk_reading, base_extractiveness, 91, 0.75).

% Suppression requirement over time
narrative_ontology:measurement(aney_su_t0, aneyoshi_land_use_prohibition__commemorative_husk_reading, suppression_requirement, 0, 0.9).
narrative_ontology:measurement(aney_su_t30, aneyoshi_land_use_prohibition__commemorative_husk_reading, suppression_requirement, 30, 0.7).
narrative_ontology:measurement(aney_su_t60, aneyoshi_land_use_prohibition__commemorative_husk_reading, suppression_requirement, 60, 0.4).
narrative_ontology:measurement(aney_su_t78, aneyoshi_land_use_prohibition__commemorative_husk_reading, suppression_requirement, 78, 0.15).
narrative_ontology:measurement(aney_su_t85, aneyoshi_land_use_prohibition__commemorative_husk_reading, suppression_requirement, 85, 0.18).
narrative_ontology:measurement(aney_su_t91, aneyoshi_land_use_prohibition__commemorative_husk_reading, suppression_requirement, 91, 0.2).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(aneyoshi_land_use_prohibition__commemorative_husk_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(aneyoshi_land_use_prohibition__commemorative_husk_reading, 0.08).
narrative_ontology:affects_constraint(aneyoshi_land_use_prohibition__commemorative_husk_reading, anneyoshi_land_use_prohibition__behavioral_competence_reading).
narrative_ontology:affects_constraint(aneyoshi_land_use_prohibition__commemorative_husk_reading, tohoku_coastal_reconstruction_policy).
narrative_ontology:affects_constraint(aneyoshi_land_use_prohibition__commemorative_husk_reading, japan_disaster_heritage_governance).

% DUAL FORMULATION NOTE:
% The aneyoshi_land_use_prohibition kernel decomposes into two readings with divergent ε: behavioral_competence_reading (ε ≈ 0.05, mountain/rope) — the prohibition coordinates safe settlement; commemorative_husk_reading (ε ≈ 0.75, piton) — the prohibition's decay enables hazard-zone development. The readings share the same physical stone and inscription but instantiate different constraints with different beneficiary/victim structures.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
