% ============================================================================
% CONSTRAINT STORY: stone_land_use_rule__commemorative_husk
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_stone_land_use_rule__commemorative_husk, []).

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
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
 *   constraint_id: stone_land_use_rule__commemorative_husk
 *   human_readable: Stone Land-Use Rule (Commemorative Reading)
 *   domain: institutional_memory/land_governance/disaster_anthropology
 *
 * SUMMARY:
 *   A stone marker commemorates a historical disaster that claimed lives in a
 *   vulnerable waterfront zone. The stone was originally placed as a warning
 *   against risky settlement patterns in that zone. Over decades, the stone's
 *   function has degraded from a behavioral constraint (influencing land-use
 *   decisions) into a pure memorial artifact (anchoring collective memory and
 *   ritual). Waterfront development has accelerated around the stone's
 *   location; zoning decisions are made independently of the marker's
 *   presence. The community performs annual commemorations while the
 *   constraint's original protective function has vanished. This story
 *   instantiates the commemorative-husk reading of the stone_land_use_rule
 *   kernel — the reading in which the stone is no longer a live land-use
 *   constraint but rather a symbolic artifact whose function is remembered,
 *   not enforced.
 *
 * KEY AGENTS:
 *   - Stone custodians: maintain the memorial, organize annual rites
 *   - Waterfront developers: build adjacent to and around the site; benefit from symbolic-only constraint
 *   - Municipal administration: host the stone, make zoning decisions independent of its warning
 *   - Community descendants: provide affective labor for commemoration; identity-locked to the disaster narrative
 *   - Institutional historians: document the decay of behavioral constraint into symbolic gesture
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(stone_land_use_rule__commemorative_husk, 0.82).
domain_priors:suppression_score(stone_land_use_rule__commemorative_husk, 0.15).
domain_priors:theater_ratio(stone_land_use_rule__commemorative_husk, 0.91).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(stone_land_use_rule__commemorative_husk, extractiveness, 0.82).
narrative_ontology:constraint_metric(stone_land_use_rule__commemorative_husk, suppression_requirement, 0.15).
narrative_ontology:constraint_metric(stone_land_use_rule__commemorative_husk, theater_ratio, 0.91).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(stone_land_use_rule__commemorative_husk, accessibility_collapse, 0.22).
narrative_ontology:constraint_metric(stone_land_use_rule__commemorative_husk, resistance, 0.73).

% --- Constraint claim ---
narrative_ontology:constraint_claim(stone_land_use_rule__commemorative_husk, piton).
narrative_ontology:human_readable(stone_land_use_rule__commemorative_husk, "Stone Land-Use Rule (Commemorative Reading)").
narrative_ontology:topic_domain(stone_land_use_rule__commemorative_husk, "institutional_memory/land_governance/disaster_anthropology").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(stone_land_use_rule__commemorative_husk, '6f9f6c8c-3c4f-4d86-a20a-5918be99625a').
narrative_ontology:cs_kernel_codification('6f9f6c8c-3c4f-4d86-a20a-5918be99625a', fixed_text).
narrative_ontology:cs_authority_grounding('6f9f6c8c-3c4f-4d86-a20a-5918be99625a', extraction).
narrative_ontology:cs_interpretation_layer_present('6f9f6c8c-3c4f-4d86-a20a-5918be99625a').
narrative_ontology:cs_reading_relation('6f9f6c8c-3c4f-4d86-a20a-5918be99625a', stone_land_use_rule__behavioral_competence, coexists_with).
narrative_ontology:cs_axiom('6f9f6c8c-3c4f-4d86-a20a-5918be99625a', foundational, memorial_primacy).
narrative_ontology:cs_axiom_status(memorial_primacy, holdable).
narrative_ontology:cs_axiom_grounding('6f9f6c8c-3c4f-4d86-a20a-5918be99625a', memorial_primacy, conventional).
narrative_ontology:cs_axiom('6f9f6c8c-3c4f-4d86-a20a-5918be99625a', foundational, commemoration_over_regulation).
narrative_ontology:cs_axiom_status(commemoration_over_regulation, holdable).
narrative_ontology:cs_axiom_grounding('6f9f6c8c-3c4f-4d86-a20a-5918be99625a', commemoration_over_regulation, instrumental).
narrative_ontology:cs_reference_frame('6f9f6c8c-3c4f-4d86-a20a-5918be99625a', stone_as_memorial_artifact).
narrative_ontology:cs_drift_state('6f9f6c8c-3c4f-4d86-a20a-5918be99625a', contemporary_development_era, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('6f9f6c8c-3c4f-4d86-a20a-5918be99625a', '').
narrative_ontology:cs_kernel_id(stone_land_use_rule__commemorative_husk, stone_land_use_rule).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(stone_land_use_rule__commemorative_husk, waterfront_developers).
narrative_ontology:constraint_beneficiary(stone_land_use_rule__commemorative_husk, municipal_administration).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(stone_land_use_rule__commemorative_husk, community_descendants).
narrative_ontology:constraint_vindicates(stone_land_use_rule__commemorative_husk, historical_tragedy_acknowledged).
narrative_ontology:constraint_vindicates(stone_land_use_rule__commemorative_husk, community_resilience_narrative).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintain the memorial stone, organize annual commemorations, interpret its meaning for school groups and visitors. They treat the stone as a sacred object whose function is remembrance, not land regulation. Their power derives from custodial legitimacy, not enforcement machinery — they cannot prevent construction and do not attempt to.
narrative_ontology:constraint_stakeholder(stone_land_use_rule__commemorative_husk, stone_custodians, agenda_setter,
    organized, generational, constrained, local).

% Develop land adjacent to and around the memorial stone site. They observe the stone symbolically but face no material constraint on building decisions — the stone's presence generates public sentiment (which they manage through heritage framing) but no binding land-use rule. They benefit from the stone's decay into pure commemoration: it marks the site as historically significant without preventing development, which increases land value and generates goodwill from being 'respectful' of heritage.
narrative_ontology:constraint_stakeholder(stone_land_use_rule__commemorative_husk, waterfront_developers, beneficiary,
    institutional, biographical, mobile, local).

% Hosts the memorial stone on public land, enables commemorative events, and makes zoning decisions independent of the stone's original warning function. The stone extracts from the community through performance: annual maintenance, ceremony staging, and narrative management create institutional work while the constraint's actual regulatory function has atrophied. The administration benefits by appearing to honor the past while exploiting waterfront value.
narrative_ontology:constraint_stakeholder(stone_land_use_rule__commemorative_husk, municipal_administration, beneficiary,
    institutional, generational, mobile, local).
narrative_ontology:stakeholder_secondary_role(stone_land_use_rule__commemorative_husk, municipal_administration, agenda_setter).

% Descendants of those who died in the original disaster and members of the affected community. They provide the affective labor that sustains the stone's meaning — they show up for commemorations, teach the story to children, and maintain the collective memory. They cannot exit this identity (the disaster is part of their family and community history). They bear the cost of theater: the performative obligations that keep the stone symbolically alive while its actual protective function (preventing risky waterfront development) has vanished.
narrative_ontology:constraint_stakeholder(stone_land_use_rule__commemorative_husk, community_descendants, payer,
    organized, generational, identity_locked, local).
narrative_ontology:stakeholder_secondary_role(stone_land_use_rule__commemorative_husk, community_descendants, observer).

% Document the stone's original function as a warning against hazardous waterfront settlement and its transformation into a pure commemorative artifact. They observe the decay of behavioral constraint into symbolic gesture and the structural conditions that enabled the decay.
narrative_ontology:constraint_stakeholder(stone_land_use_rule__commemorative_husk, institutional_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(stone_land_use_rule__commemorative_husk, municipal_administration).
narrative_ontology:fixing_cost_class(stone_land_use_rule__commemorative_husk, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates community memory and collective identity through annual ritual and educational transmission. The stone serves as a focal point for remembering a shared tragedy and maintaining narratives of resilience and historical consciousness.
% TRANSFER_FUNCTION: Transfers affective and performative labor from community descendants and custodians to the municipal administration and, indirectly, to waterfront developers. The descendants bear the emotional cost of keeping the memory alive; the administration appropriates that labor to legitimize development decisions; developers benefit from the stone's decay into pure symbolism, which removes the land-use constraint while retaining public sentiment.
% ABSENT_VOICES: Those who would advocate for enforcing the stone's original land-use prohibition (restricting waterfront development) are structurally absent from commemoration spaces. Disaster-risk specialists, indigenous land-management practitioners, and critical heritage scholars who might challenge the symbolic-only reading are not invited to shape the interpretive frame.
% DISAPPEARANCE_RATIONALE: If the stone vanished, the community would lose a material anchor for collective memory and identity maintenance — the affective practice that sustains cohesion around the disaster narrative would fragment. However, land-use decisions would remain unchanged (they already ignore the stone's original function), and development pressure would, if anything, intensify without even the symbolic constraint.
% FOUNDING_PROBLEM: A major disaster caused loss of life and property, and survivors placed a stone to mark the hazard and warn future generations against risky waterfront settlement patterns.
% FOUNDING_PROBLEM_CORROBORATION: Institutional historians and disaster anthropologists document that the founding problem (preventing recurrence of waterfront settlement in hazardous zones) is no longer addressed by the stone. Contemporary zoning decisions and development permits are made independently of the stone's location. Municipal administration documents show the stone is classified as a heritage/cultural artifact, not a land-use constraint. Descendant community members privately acknowledge the stone no longer protects the waterfront, though public commemoration rituals frame it as 'honoring those we lost' rather than 'preventing future losses'.
narrative_ontology:disappearance_verdict(stone_land_use_rule__commemorative_husk, world_rearranges).
narrative_ontology:founding_problem_status(stone_land_use_rule__commemorative_husk, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(stone_land_use_rule__commemorative_husk, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(stone_land_use_rule__commemorative_husk, 'none', 1).
narrative_ontology:epsilon_provenance(stone_land_use_rule__commemorative_husk, 0.82, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(stone_land_use_rule__commemorative_husk_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(stone_land_use_rule__commemorative_husk, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(stone_land_use_rule__commemorative_husk_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The commemorative-husk reading is characterized by high extractiveness (0.82 at interval end) because the constraint no longer regulates the behavior it was meant to regulate (waterfront development) but instead extracts performative and affective labor from the community to maintain its symbolic status. Theater is extremely high (0.91) because nearly all remaining activity around the stone is performative — ceremonies, educational narratives, municipal framing as heritage — while the actual land-use function has collapsed. Suppression is low (0.15) because this reading involves no active coercive enforcement; the constraint persists through inertia and affective attachment, not through suppression of alternatives. Accessibility collapse is very low (0.22) because alternatives (building near the stone, ignoring its warning) are not collapsed — they are freely chosen by developers and administrators. Resistance is high (0.73) from the descendant community, which mourns the loss of the stone's protective function but cannot reorganize land-use governance unilaterally. The measurement trajectory shows theater rising sharply (0.68 → 0.91) as the stone becomes more purely commemorative, while suppression stays flat and low (0.11 → 0.15). Extractiveness rises steadily (0.61 → 0.82) as more affective labor is required to maintain the stone's meaning against the lived reality of development.
 *
 * PERSPECTIVAL GAP:
 *   From the administrator and developer seats, this is a successful integration of heritage into development planning: the stone is honored, commemorations are funded, the community's identity is maintained, and development proceeds smoothly. From the descendant community's seat, the same arrangement is a slow extraction of meaning: the stone no longer does what it was meant to do (prevent waterfront hazard), yet the community must continue performing the ritual that justifies keeping it. From the historian's seat, this is visible as institutional inertia — the constraint persists not because it solves a current problem, but because the actors who benefit from its decay (developers, administration) have no incentive to change it, and the actors who suffer from its decay (descendants) lack the power to enforce its original function.
 *
 * DIRECTIONALITY LOGIC:
 *   Waterfront developers and municipal administration are structural beneficiaries (d near 0.0–0.3): they benefit from the stone's decay into pure symbolism, which removes the land-use constraint while retaining public sentiment that legitimizes development. They have mobile or strategic exit options (they can build elsewhere, or reshape the narrative frame). Community descendants are targets (d near 0.8–1.0): they are identity-locked to the disaster narrative and unable to exit the affective obligation to maintain the stone's meaning. Stone custodians sit between (d near 0.5–0.6): they benefit from custodial legitimacy but bear the labor cost of keeping the stone meaningful against institutional drift. The measurement trajectory (rising extractiveness, rising theater) tracks the systematic shift in directionality: as the stone becomes more performative and less protective, the descendant community bears more cost relative to the benefit they receive, pushing their d upward.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (preventing risky waterfront settlement) is dead: contemporary building decisions are made independently of the stone's location and meaning. Yet the constraint persists, transformed into a commemorative artifact. The stone is a textbook piton: it extracts affective and performative labor from the community (high extractiveness, high theater), but no single beneficiary captures enough to maintain active enforcement (low suppression, decentralized benefit to administration/developers). The original mandate (land-use protection) has outlived its function; the residual constraint is maintained through inertia and the descendant community's identity-locked commitment to the commemoration ritual. The theater ratio trajectory (rising from 0.68 to 0.91 over 40 years) documents the piton's maturation: the stone becomes increasingly a performance object and decreasingly a functional constraint.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    affective_labor_boundary,
    'Is the measured extraction (theater, affective labor cost) experienced by community descendants as a burden they would abandon if they could, or as an identity-constituting practice they choose to maintain?',
    'Post-exit ethnography: follow descendants who relocate away from the commemoration community; observe whether they continue commemorative practice, and how they frame it. Conduct interviews about perceived cost vs. identity fusion.',
    'If experienced as burden: the constraint is snare-like (extraction masked by identity-lock). If chosen as identity practice: the constraint is more rope-like (coordination function outweighs extraction cost). The measurement remains identical (high theater, high extractiveness); the reading shifts the normative evaluation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(affective_labor_boundary, empirical, 'Whether affective labor extraction is coercive or identity-constituting.').

omega_variable(
    behavioral_competence_possibility,
    'Is the behavioral-competence reading (stone as live land-use constraint) still possible, or has the decay into commemoration foreclosed it?',
    'Scenario: if a new disaster occurred and descendants demanded that the stone''s original function be restored (land-use restriction enforced), could the municipal administration comply, or has institutional/legal drift made the original function impossible to reinstantiate?',
    'If the behavioral reading is still possible, the two readings coexist (different actors hold different framings). If foreclosed by institutional drift (zoning law has changed, development rights are vested, etc.), the commemorative reading has become the only live option, and the kernel has collapsed into a single reading.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(behavioral_competence_possibility, conceptual, 'Whether the behavioral-competence reading is still live or has been structurally foreclosed.').

omega_variable(
    suppression_mechanism_latency,
    'The low suppression score (0.15) reflects the absence of active enforcement machinery. But is descendants'' compliance with the commemorative frame (performing annually, maintaining the narrative) maintained through internalized identity-lock, or through external pressure that is harder to measure than formal enforcement?',
    'Ethnographic observation during a commemorative event that is disrupted (say, by development of an adjacent site, or by a descendant who refuses to participate): what happens? Is the frame reasserted through peer pressure, institutional authority, or has descendants'' internalized identification with the stone become self-enforcing?',
    'If internalized, the effective suppression is higher than the structural measure suggests — descendants carry the suppression with them. If external, the low suppression accurately captures a brittle constraint that persists only through affective attachment and would fracture if that attachment weakened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_latency, empirical, 'Whether measured suppression is structural or internalized through identity-lock.').

omega_variable(
    reading_sibling_coexistence,
    'Can the behavioral-competence reading and the commemorative-husk reading coexist within the same institutional framework, or are they mutually exclusive instantiations of the kernel?',
    'Examine whether any stakeholder genuinely holds both readings simultaneously (e.g., an administrator who treats the stone as heritage AND enforces it as a land-use constraint), or whether holding one reading entails rejecting the other.',
    'If coexistent: the two readings are in genuine contest, held by different parties; neither forecloses the other. If mutually exclusive: the behavioral-competence reading has been foreclosed by the institutional dominance of the commemorative frame, and only one reading remains live.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_sibling_coexistence, conceptual, 'Logical relationship between the two kernel readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(stone_land_use_rule__commemorative_husk, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ston_tr_t0, stone_land_use_rule__commemorative_husk, theater_ratio, 0, 0.68).
narrative_ontology:measurement_basis(ston_tr_t0, observed).
narrative_ontology:measurement(ston_tr_t5, stone_land_use_rule__commemorative_husk, theater_ratio, 5, 0.74).
narrative_ontology:measurement_basis(ston_tr_t5, observed).
narrative_ontology:measurement(ston_tr_t10, stone_land_use_rule__commemorative_husk, theater_ratio, 10, 0.79).
narrative_ontology:measurement_basis(ston_tr_t10, observed).
narrative_ontology:measurement(ston_tr_t15, stone_land_use_rule__commemorative_husk, theater_ratio, 15, 0.84).
narrative_ontology:measurement_basis(ston_tr_t15, observed).
narrative_ontology:measurement(ston_tr_t20, stone_land_use_rule__commemorative_husk, theater_ratio, 20, 0.87).
narrative_ontology:measurement_basis(ston_tr_t20, observed).
narrative_ontology:measurement(ston_tr_t25, stone_land_use_rule__commemorative_husk, theater_ratio, 25, 0.89).
narrative_ontology:measurement_basis(ston_tr_t25, observed).
narrative_ontology:measurement(ston_tr_t30, stone_land_use_rule__commemorative_husk, theater_ratio, 30, 0.9).
narrative_ontology:measurement_basis(ston_tr_t30, observed).
narrative_ontology:measurement(ston_tr_t40, stone_land_use_rule__commemorative_husk, theater_ratio, 40, 0.91).
narrative_ontology:measurement_basis(ston_tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(ston_be_t0, stone_land_use_rule__commemorative_husk, base_extractiveness, 0, 0.61).
narrative_ontology:measurement_basis(ston_be_t0, observed).
narrative_ontology:measurement(ston_be_t5, stone_land_use_rule__commemorative_husk, base_extractiveness, 5, 0.64).
narrative_ontology:measurement_basis(ston_be_t5, observed).
narrative_ontology:measurement(ston_be_t10, stone_land_use_rule__commemorative_husk, base_extractiveness, 10, 0.68).
narrative_ontology:measurement_basis(ston_be_t10, observed).
narrative_ontology:measurement(ston_be_t15, stone_land_use_rule__commemorative_husk, base_extractiveness, 15, 0.72).
narrative_ontology:measurement_basis(ston_be_t15, observed).
narrative_ontology:measurement(ston_be_t20, stone_land_use_rule__commemorative_husk, base_extractiveness, 20, 0.76).
narrative_ontology:measurement_basis(ston_be_t20, observed).
narrative_ontology:measurement(ston_be_t25, stone_land_use_rule__commemorative_husk, base_extractiveness, 25, 0.79).
narrative_ontology:measurement_basis(ston_be_t25, observed).
narrative_ontology:measurement(ston_be_t30, stone_land_use_rule__commemorative_husk, base_extractiveness, 30, 0.81).
narrative_ontology:measurement_basis(ston_be_t30, observed).
narrative_ontology:measurement(ston_be_t40, stone_land_use_rule__commemorative_husk, base_extractiveness, 40, 0.82).
narrative_ontology:measurement_basis(ston_be_t40, observed).

% Suppression requirement over time
narrative_ontology:measurement(ston_su_t0, stone_land_use_rule__commemorative_husk, suppression_requirement, 0, 0.11).
narrative_ontology:measurement_basis(ston_su_t0, observed).
narrative_ontology:measurement(ston_su_t5, stone_land_use_rule__commemorative_husk, suppression_requirement, 5, 0.12).
narrative_ontology:measurement_basis(ston_su_t5, observed).
narrative_ontology:measurement(ston_su_t10, stone_land_use_rule__commemorative_husk, suppression_requirement, 10, 0.13).
narrative_ontology:measurement_basis(ston_su_t10, observed).
narrative_ontology:measurement(ston_su_t15, stone_land_use_rule__commemorative_husk, suppression_requirement, 15, 0.14).
narrative_ontology:measurement_basis(ston_su_t15, observed).
narrative_ontology:measurement(ston_su_t20, stone_land_use_rule__commemorative_husk, suppression_requirement, 20, 0.14).
narrative_ontology:measurement_basis(ston_su_t20, observed).
narrative_ontology:measurement(ston_su_t25, stone_land_use_rule__commemorative_husk, suppression_requirement, 25, 0.15).
narrative_ontology:measurement_basis(ston_su_t25, observed).
narrative_ontology:measurement(ston_su_t30, stone_land_use_rule__commemorative_husk, suppression_requirement, 30, 0.15).
narrative_ontology:measurement_basis(ston_su_t30, observed).
narrative_ontology:measurement(ston_su_t40, stone_land_use_rule__commemorative_husk, suppression_requirement, 40, 0.15).
narrative_ontology:measurement_basis(ston_su_t40, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(stone_land_use_rule__commemorative_husk, identity_coordination).
narrative_ontology:boltzmann_floor_override(stone_land_use_rule__commemorative_husk, 0.12).
narrative_ontology:affects_constraint(stone_land_use_rule__commemorative_husk, stone_land_use_rule__behavioral_competence).

% DUAL FORMULATION NOTE:
% The stone_land_use_rule kernel has been decomposed into two constraint stories: behavioral_competence (stone as live land-use constraint, enforced through daily spatial practice) and commemorative_husk (stone as memorial artifact, function decayed to pure commemoration). These are structurally distinct constraints with different ε values, different beneficiary/victim structures, and different persistence mechanisms. The behavioral reading shows low extractiveness (constraint protects future generations from hazard), active resistance from beneficiaries (developers want to build), and moderate suppression (enforcement through social coordination). The commemorative reading shows high extractiveness (affective labor extraction from descendants), low resistance from beneficiaries (administration/developers favor it), and low suppression (persistence through inertia and identity-lock). The two readings coexist as live positions held by different parties within the ongoing dispute about what the stone should do. Neither reading forecloses the other logically, though institutional drift has made the behavioral reading harder to instantiate. Both readings share the same kernel (the persistent commitment: 'This stone marks a hazardous waterfront zone') but diverge on whether the stone functions as a behavioral constraint or as a memorial artifact.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(stone_land_use_rule__commemorative_husk, organized, 0.78).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
