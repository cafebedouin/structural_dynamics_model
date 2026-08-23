% ============================================================================
% CONSTRAINT STORY: stone_land_use_rule__behavioral_competence
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_stone_land_use_rule__behavioral_competence, []).

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
 *   constraint_id: stone_land_use_rule__behavioral_competence
 *   human_readable: Stone Marker as Live Land-Use Prohibition (Behavioral Competence Reading)
 *   domain: disaster_anthropology/institutional_memory/land_use_governance
 *
 * SUMMARY:
 *   A stone marker erected after a 1947 landslide functions as a live
 *   land-use prohibition in an Andean village. The constraint is not the
 *   stone itself but the daily spatial practice it anchors: residents route
 *   paths, site homes, and teach children to respect the marker's implicit
 *   boundary. Compliance has persisted for 78 years without formal
 *   enforcement, written law, or state presence. The 'behavioral competence'
 *   reading treats this as a genuine coordination institution — the community
 *   solves a collective action problem (avoiding the hazard zone) through a
 *   practice that is self-enforcing because every participant's survival
 *   depends on everyone's compliance. Economic costs are real (steep hill
 *   climb to safe building ground) but accepted as the price of survival.
 *   This reading stands in structural tension with the 'commemorative husk'
 *   reading, which sees the stone as a symbolic memorial whose behavioral
 *   force has decayed.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(stone_land_use_rule__behavioral_competence, 0.15).
domain_priors:suppression_score(stone_land_use_rule__behavioral_competence, 0.12).
domain_priors:theater_ratio(stone_land_use_rule__behavioral_competence, 0.08).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(stone_land_use_rule__behavioral_competence, extractiveness, 0.15).
narrative_ontology:constraint_metric(stone_land_use_rule__behavioral_competence, suppression_requirement, 0.12).
narrative_ontology:constraint_metric(stone_land_use_rule__behavioral_competence, theater_ratio, 0.08).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(stone_land_use_rule__behavioral_competence, accessibility_collapse, 0.38).
narrative_ontology:constraint_metric(stone_land_use_rule__behavioral_competence, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(stone_land_use_rule__behavioral_competence, rope).
narrative_ontology:human_readable(stone_land_use_rule__behavioral_competence, "Stone Marker as Live Land-Use Prohibition (Behavioral Competence Reading)").
narrative_ontology:topic_domain(stone_land_use_rule__behavioral_competence, "disaster_anthropology/institutional_memory/land_use_governance").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(stone_land_use_rule__behavioral_competence, '9d8e1ce0-ef1d-4628-b96d-83f8ae9e4c39').
narrative_ontology:cs_kernel_codification('9d8e1ce0-ef1d-4628-b96d-83f8ae9e4c39', implicit).
narrative_ontology:cs_authority_grounding('9d8e1ce0-ef1d-4628-b96d-83f8ae9e4c39', practice).
narrative_ontology:cs_interpretation_layer_present('9d8e1ce0-ef1d-4628-b96d-83f8ae9e4c39').
narrative_ontology:cs_reading_relation('9d8e1ce0-ef1d-4628-b96d-83f8ae9e4c39', stone_land_use_rule__commemorative_husk, coexists_with).
narrative_ontology:cs_axiom('9d8e1ce0-ef1d-4628-b96d-83f8ae9e4c39', foundational, stone_anchors_living_practice).
narrative_ontology:cs_axiom_status(stone_anchors_living_practice, holdable).
narrative_ontology:cs_axiom_grounding('9d8e1ce0-ef1d-4628-b96d-83f8ae9e4c39', stone_anchors_living_practice, conventional).
narrative_ontology:cs_axiom('9d8e1ce0-ef1d-4628-b96d-83f8ae9e4c39', secondary, daily_compliance_requires_no_state_enforcement).
narrative_ontology:cs_axiom_status(daily_compliance_requires_no_state_enforcement, holdable).
narrative_ontology:cs_axiom_grounding('9d8e1ce0-ef1d-4628-b96d-83f8ae9e4c39', daily_compliance_requires_no_state_enforcement, empirically_contingent).
narrative_ontology:cs_reference_frame('9d8e1ce0-ef1d-4628-b96d-83f8ae9e4c39', stone_as_behavioral_anchor).
narrative_ontology:cs_drift_state('9d8e1ce0-ef1d-4628-b96d-83f8ae9e4c39', contemporary_tourism_pressure, gap(practice_drift, minor, true)).
narrative_ontology:cs_created_at('9d8e1ce0-ef1d-4628-b96d-83f8ae9e4c39', '').
narrative_ontology:cs_kernel_id(stone_land_use_rule__behavioral_competence, stone_land_use_rule).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(stone_land_use_rule__behavioral_competence, local_community).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(stone_land_use_rule__behavioral_competence, local_residents).
narrative_ontology:constraint_beneficiary(stone_land_use_rule__behavioral_competence, landowning_families).
narrative_ontology:constraint_victim(stone_land_use_rule__behavioral_competence, landowning_families).
narrative_ontology:constraint_vindicates(stone_land_use_rule__behavioral_competence, communal_spatial_practice_sustains_risk_reduction).
narrative_ontology:constraint_vindicates(stone_land_use_rule__behavioral_competence, informal_institution_outperforms_formal_zoning_in_hazard_zones).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Residents of the village comply with the stone's prohibition by routing daily paths around the hazard zone and forgoing reconstruction on ancestral plots below the marker. They accept the steep hill climb to safe building ground as the price of collective survival. The practice is taught to children through daily accompaniment, not formal instruction.
narrative_ontology:constraint_stakeholder(stone_land_use_rule__behavioral_competence, local_residents, beneficiary,
    organized, generational, constrained, local).

% Families holding title to land below the stone bear the direct economic cost: they cannot rebuild or cultivate the most accessible terraces. They accept this cost because the alternative — rebuilding in the landslide path — killed their grandparents. Their compliance is voluntary but structurally constrained by the community's shared memory.
narrative_ontology:constraint_stakeholder(stone_land_use_rule__behavioral_competence, landowning_families, beneficiary,
    moderate, biographical, constrained, local).
narrative_ontology:stakeholder_secondary_role(stone_land_use_rule__behavioral_competence, landowning_families, payer).

% Elders and designated story-holders maintain the oral protocol: they lead the annual walk to the stone, recount the disaster narrative, and correct deviations in routing practice. Their authority derives from lived witness and communal recognition, not formal appointment. Exit would mean abandoning their identity as keepers of the village's survival.
narrative_ontology:constraint_stakeholder(stone_land_use_rule__behavioral_competence, memory_keepers, agenda_setter,
    moderate, generational, identity_locked, local).

% Town officials recognize the stone's de facto authority but have never codified it into zoning law. They route infrastructure permits around the prohibited zone informally. Their position is analytically supportive but institutionally hands-off — formalizing the rule would risk breaking the practice's autonomy.
narrative_ontology:constraint_stakeholder(stone_land_use_rule__behavioral_competence, municipal_authorities, observer,
    institutional, biographical, analytical, regional).

% Tourism and hydropower interests have proposed projects on the prohibited land. They are structurally excluded: the community refuses engagement, the stone's authority is non-negotiable, and municipal authorities decline to override the practice. Their capital and legal resources find no purchase against a constraint that operates through daily spatial habit.
narrative_ontology:constraint_stakeholder(stone_land_use_rule__behavioral_competence, outside_developers, excluded,
    powerful, immediate, trapped, national).

% Researchers document the stone as a rare case of disaster memory translating into sustained behavioral compliance without state enforcement. They see the full structural picture: the stone coordinates risk avoidance, the practice enforces it, and the community's identity is fused to the constraint.
narrative_ontology:constraint_stakeholder(stone_land_use_rule__behavioral_competence, anthropological_observers, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Prevents rebuilding and daily habitation in a known landslide runout zone by anchoring communal spatial practice to a physical marker that requires no literacy, bureaucracy, or state capacity to operate.
% TRANSFER_FUNCTION: Moves development rights and economic value from individual landowners (who forgo use of accessible terraces below the stone) to the collective (which gains continued survival in the hazard zone). The transfer is accepted, not coerced — the cost is the steep hill climb to safe ground.
% ABSENT_VOICES: Outside developers and tourism operators who would monetize the prohibited land; future generations not yet socialized into the practice; displaced families from the original disaster who might have returned if the stone did not exist. The excluded developers are structurally present in the constraint's operation — their exclusion is what the practice maintains.
% DISAPPEARANCE_RATIONALE: If the stone and its daily practice vanished overnight, rebuilding on the accessible terraces would begin within a year. Formal zoning is absent; the only barrier is the practice itself. The next major rainfall event would then kill residents who had returned to the runout zone.
% FOUNDING_PROBLEM: After the 1947 landslide killed 83 villagers, the community needed to prevent rebuilding in the runout zone but had no formal zoning capacity, no literacy to maintain written records, and no state presence to enforce regulation.
% FOUNDING_PROBLEM_CORROBORATION: Geological surveys confirm the hazard zone remains active; oral histories from three generations of elders attest the founding disaster and the stone's erection; municipal records show no formal zoning was ever enacted for the area; anthropological literature (Oliver-Smith 1996, Gaillard 2008) cites this case as a canonical example of informal institution sustaining risk reduction.
narrative_ontology:disappearance_verdict(stone_land_use_rule__behavioral_competence, world_rearranges).
narrative_ontology:founding_problem_status(stone_land_use_rule__behavioral_competence, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(stone_land_use_rule__behavioral_competence, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(stone_land_use_rule__behavioral_competence, 'none', 1).
narrative_ontology:epsilon_provenance(stone_land_use_rule__behavioral_competence, 0.15, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(stone_land_use_rule__behavioral_competence_tests).
:- end_tests(stone_land_use_rule__behavioral_competence_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.15) because the constraint extracts no rents — the cost borne by landowning families is the coordination cost of survival, not a transfer to a beneficiary class. Suppression is low (0.12) because no active enforcement machinery exists; compliance is maintained through daily practice and identity fusion. Theater ratio is minimal (0.08) — the annual walk to the stone is functional memory transmission, not performative ritual. Accessibility collapse is moderate (0.38): formal alternatives (zoning, relocation) exist in principle but are structurally unavailable. Resistance is near-zero (0.08) because the constraint's beneficiaries and payers are the same people — the community constrains itself.
 *
 * PERSPECTIVAL GAP:
 *   From the memory_keeper seat, the constraint is a sacred trust — identity-locked, non-negotiable. From the landowning_family seat, it is a costly but accepted necessity — they could defect individually but don't. From the developer seat, it is an illegitimate barrier — but their exclusion is structural, not extractive. The engineer computes these as different types: rope for the community, snare for the developer (if they ever gained standing), mountain for the observer. The divergence is the measurement.
 *
 * DIRECTIONALITY LOGIC:
 *   The local_community and landowning_families are both beneficiaries (survival) and payers (hill climb) — their directionality is near-symmetric (d ≈ 0.5). Memory_keepers are identity-locked agenda_setters (d ≈ 0.3, subsidized by status). Municipal_authorities are analytical observers (d ≈ 0.0). Outside_developers are trapped excluded parties (d ≈ 1.0 but they bear no extraction because they never gain access). The engine will compute per-seat types from these structural positions.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (preventing rebuilding in the runout zone) remains live — the hazard has not diminished. The constraint has not atrophied into a piton because its coordination function is exercised daily, not annually. The theater ratio's slight rise (0.05→0.08) reflects tourism pressure making the annual walk more visible, but the daily routing practice remains purely functional. No mandatrophy: the mandate matches the function.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    committer_frame_behavioral_competence,
    'This constraint is the behavioral_competence reading of the stone_land_use_rule kernel. The sibling commemorative_husk reading claims the stone''s behavioral force has decayed. Where is the structural disagreement located?',
    'Longitudinal observation of compliance rates under tourism pressure; measurement of whether daily routing practice persists when observers are absent; comparison of land-use patterns against the stone''s implicit boundary vs. formal zoning (where it exists).',
    'If the sibling reading is structurally accurate, this constraint''s claimed_type (rope) is false — it would be a piton (degraded coordination maintained theatrically). The epsilon would rise from 0.15 to >0.4 as maintenance costs become extractive overhead without coordination benefit.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(committer_frame_behavioral_competence, empirical, 'Commitment-system framing: behavioral_competence vs commemorative_husk readings of the same stone marker.').

omega_variable(
    voluntary_vs_implicit_coercion,
    'Is compliance truly voluntary (accepted coordination cost) or maintained by implicit social coercion (ostracism, shame, identity threat)?',
    'Ethnographic study of deviation events: what happens when a family attempts to rebuild below the stone? Is the response communal discussion (coordination) or social sanction (coercion)?',
    'If implicit coercion is significant, suppression is understated and the constraint may be tangled_rope (coordination + asymmetric extraction via social pressure) rather than pure rope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(voluntary_vs_implicit_coercion, conceptual, 'Whether the low suppression score masks internalized coercion mechanisms.').

omega_variable(
    tourism_pressure_drift,
    'Will increasing tourism and land-value pressure cause the practice to fracture, converting the constraint from rope to piton or snare?',
    'Track land-use change proposals, developer offers, and youth outmigration over the next decade. Measure whether the annual walk becomes performative (tourist-facing) while daily routing decays.',
    'If tourism fractures the practice, the constraint''s type transitions: rope → piton (if practice becomes theatrical) or rope → snare (if developers capture municipal authority and impose extraction). The current low epsilon is historically contingent.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(tourism_pressure_drift, empirical, 'Whether external market pressure will degrade the constraint''s coordination purity.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(stone_land_use_rule__behavioral_competence, 0, 78).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(stone_land_use_rule__behavioral_competence_tr_t0, stone_land_use_rule__behavioral_competence, theater_ratio, 0, 0.05).
narrative_ontology:measurement(stone_land_use_rule__behavioral_competence_tr_t15, stone_land_use_rule__behavioral_competence, theater_ratio, 15, 0.06).
narrative_ontology:measurement(stone_land_use_rule__behavioral_competence_tr_t30, stone_land_use_rule__behavioral_competence, theater_ratio, 30, 0.07).
narrative_ontology:measurement(stone_land_use_rule__behavioral_competence_tr_t45, stone_land_use_rule__behavioral_competence, theater_ratio, 45, 0.07).
narrative_ontology:measurement(stone_land_use_rule__behavioral_competence_tr_t60, stone_land_use_rule__behavioral_competence, theater_ratio, 60, 0.08).
narrative_ontology:measurement(stone_land_use_rule__behavioral_competence_tr_t78, stone_land_use_rule__behavioral_competence, theater_ratio, 78, 0.08).

% Extraction over time
narrative_ontology:measurement(stone_land_use_rule__behavioral_competence_be_t0, stone_land_use_rule__behavioral_competence, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(stone_land_use_rule__behavioral_competence_be_t15, stone_land_use_rule__behavioral_competence, base_extractiveness, 15, 0.16).
narrative_ontology:measurement(stone_land_use_rule__behavioral_competence_be_t30, stone_land_use_rule__behavioral_competence, base_extractiveness, 30, 0.14).
narrative_ontology:measurement(stone_land_use_rule__behavioral_competence_be_t45, stone_land_use_rule__behavioral_competence, base_extractiveness, 45, 0.15).
narrative_ontology:measurement(stone_land_use_rule__behavioral_competence_be_t60, stone_land_use_rule__behavioral_competence, base_extractiveness, 60, 0.15).
narrative_ontology:measurement(stone_land_use_rule__behavioral_competence_be_t78, stone_land_use_rule__behavioral_competence, base_extractiveness, 78, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(stone_land_use_rule__behavioral_competence_su_t0, stone_land_use_rule__behavioral_competence, suppression_requirement, 0, 0.1).
narrative_ontology:measurement(stone_land_use_rule__behavioral_competence_su_t15, stone_land_use_rule__behavioral_competence, suppression_requirement, 15, 0.11).
narrative_ontology:measurement(stone_land_use_rule__behavioral_competence_su_t30, stone_land_use_rule__behavioral_competence, suppression_requirement, 30, 0.11).
narrative_ontology:measurement(stone_land_use_rule__behavioral_competence_su_t45, stone_land_use_rule__behavioral_competence, suppression_requirement, 45, 0.12).
narrative_ontology:measurement(stone_land_use_rule__behavioral_competence_su_t60, stone_land_use_rule__behavioral_competence, suppression_requirement, 60, 0.12).
narrative_ontology:measurement(stone_land_use_rule__behavioral_competence_su_t78, stone_land_use_rule__behavioral_competence, suppression_requirement, 78, 0.12).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(stone_land_use_rule__behavioral_competence, identity_coordination).
narrative_ontology:boltzmann_floor_override(stone_land_use_rule__behavioral_competence, 0.08).
narrative_ontology:affects_constraint(stone_land_use_rule__behavioral_competence, stone_land_use_rule__commemorative_husk).

% DUAL FORMULATION NOTE:
% This constraint and its sibling commemorative_husk form a constraint family decomposing the 'stone_land_use_rule' kernel. The behavioral_competence reading instantiates the kernel as a live coordination institution (rope, epsilon=0.15). The commemorative_husk reading instantiates it as a degraded memorial (piton, epsilon>0.4). They share the same physical referent (the stone) but disagree on whether the spatial practice it anchors is functional or performative. The network edge reflects that the behavioral_competence reading's persistence creates the conditions the commemorative_husk reading describes as decay.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
