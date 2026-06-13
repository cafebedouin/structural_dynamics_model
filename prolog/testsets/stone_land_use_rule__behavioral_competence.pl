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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: stone_land_use_rule__behavioral_competence
 *   human_readable: Stone Land-Use Rule (Behavioral Competence Reading)
 *   domain: institutional/disaster-anthropology/land-use-governance
 *
 * SUMMARY:
 *   A stone marker placed in 1946, immediately after a catastrophic flood
 *   that destroyed downslope dwellings, designates a steep hillside as
 *   prohibited for timber harvest, clearance, and building. The constraint
 *   has held for 78 continuous years without written code, legal statute, or
 *   state enforcement machinery. This is the BEHAVIORAL COMPETENCE reading:
 *   the stone functions as a live, binding land-use rule, enforced through
 *   daily spatial practice (sight lines across the settlement, repeated oral
 *   transmission of the founding disaster story, collective monitoring of the
 *   slope, quiet social penalties for transgression). The sibling reading
 *   (commemorative_husk) treats the stone as a memorial artifact whose
 *   warning has decayed into symbolic gesture without behavioral force —
 *   under that reading, compliance would be theater and the constraint would
 *   persist only as inertia. This reading claims the constraint is
 *   structurally sound coordination: low extractiveness (0.28), minimal
 *   suppression (0.15), and nearly flat trajectory across 78 years
 *   demonstrate sustained behavioral coherence without ratcheting
 *   enforcement. The constraint emerged from a genuine collective-action
 *   problem and remains embedded in community practice.
 *
 * KEY AGENTS:
 *   - downslope_residents — the beneficiary class whose safety depends on slope stability
 *   - slope_adjacent_landholders — the payer class whose productive options are constrained by the prohibition
 *   - community_custodians — the organized group that maintains the rule through narration and social authority
 *   - external_policy_actors — excluded institutional voices (environmental agencies, timber interests, development capital) who would challenge the constraint if given standing
 *   - anthropological_observer — the analytical seat that witnesses the constraint's operation as institutional memory embedded in daily practice
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(stone_land_use_rule__behavioral_competence, 0.28).
domain_priors:suppression_score(stone_land_use_rule__behavioral_competence, 0.15).
domain_priors:theater_ratio(stone_land_use_rule__behavioral_competence, 0.12).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(stone_land_use_rule__behavioral_competence, extractiveness, 0.28).
narrative_ontology:constraint_metric(stone_land_use_rule__behavioral_competence, suppression_requirement, 0.15).
narrative_ontology:constraint_metric(stone_land_use_rule__behavioral_competence, theater_ratio, 0.12).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(stone_land_use_rule__behavioral_competence, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(stone_land_use_rule__behavioral_competence, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(stone_land_use_rule__behavioral_competence, rope).
narrative_ontology:human_readable(stone_land_use_rule__behavioral_competence, "Stone Land-Use Rule (Behavioral Competence Reading)").
narrative_ontology:topic_domain(stone_land_use_rule__behavioral_competence, "institutional/disaster-anthropology/land-use-governance").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(stone_land_use_rule__behavioral_competence, '7f102c4b-e7a4-4f83-a5c2-716c3e7e3dd8').
narrative_ontology:cs_kernel_codification('7f102c4b-e7a4-4f83-a5c2-716c3e7e3dd8', fixed_text).
narrative_ontology:cs_authority_grounding('7f102c4b-e7a4-4f83-a5c2-716c3e7e3dd8', practice).
narrative_ontology:cs_interpretation_layer_present('7f102c4b-e7a4-4f83-a5c2-716c3e7e3dd8').
narrative_ontology:cs_reading_relation('7f102c4b-e7a4-4f83-a5c2-716c3e7e3dd8', stone_land_use_rule__commemorative_husk, coexists_with).
narrative_ontology:cs_axiom('7f102c4b-e7a4-4f83-a5c2-716c3e7e3dd8', foundational, slope_danger_remains_salient).
narrative_ontology:cs_axiom_status(slope_danger_remains_salient, holdable).
narrative_ontology:cs_axiom_grounding('7f102c4b-e7a4-4f83-a5c2-716c3e7e3dd8', slope_danger_remains_salient, empirically_contingent).
narrative_ontology:cs_axiom('7f102c4b-e7a4-4f83-a5c2-716c3e7e3dd8', foundational, community_memory_drives_compliance).
narrative_ontology:cs_axiom_status(community_memory_drives_compliance, holdable).
narrative_ontology:cs_axiom_grounding('7f102c4b-e7a4-4f83-a5c2-716c3e7e3dd8', community_memory_drives_compliance, conventional).
narrative_ontology:cs_reference_frame('7f102c4b-e7a4-4f83-a5c2-716c3e7e3dd8', slope_as_community_preserve).
narrative_ontology:cs_drift_state('7f102c4b-e7a4-4f83-a5c2-716c3e7e3dd8', contemporary_post_year78, gap(stable, minor, false)).
narrative_ontology:cs_created_at('7f102c4b-e7a4-4f83-a5c2-716c3e7e3dd8', '2026-06-12T09:15:32Z').
narrative_ontology:cs_kernel_id(stone_land_use_rule__behavioral_competence, stone_land_use_rule).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(stone_land_use_rule__behavioral_competence, downslope_residents).
narrative_ontology:constraint_beneficiary(stone_land_use_rule__behavioral_competence, flood_risk_zone_population).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(stone_land_use_rule__behavioral_competence, slope_adjacent_landholders).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Live in the flood-prone zone below the stone-marked hillside. The constraint protects them by preventing deforestation and soil disturbance on steep slopes that would accelerate runoff and landslip during heavy rains. They benefit from 78 years of unbroken compliance, which has kept the slope stabilized and forest-covered. Exit means relocating entirely from a zone where generational residence is normal; staying means living with the flood risk as the constraint mediates it.
narrative_ontology:constraint_stakeholder(stone_land_use_rule__behavioral_competence, downslope_residents, beneficiary,
    moderate, generational, mobile, local).

% Own or farm the steep hillside marked by the stone. They cannot harvest timber, clear for agriculture, or build without triggering visible violation of the constraint and community sanction. The land's productive value is substantially reduced by the prohibition. They accept the cost because the alternative — transgression visible to the community — carries social and practical penalties (exclusion, public shaming, property sanctions). The constraint enforces via daily observation and collective memory, not legal title.
narrative_ontology:constraint_stakeholder(stone_land_use_rule__behavioral_competence, slope_adjacent_landholders, payer,
    moderate, biographical, constrained, local).

% Maintain the rule through repeated reference to the stone, retelling the disaster story at each generation's coming-of-age, and collective monitoring of the slope. They do not own the land or collect formal authority; they are the keepers of the constraint's legitimacy, its history, and the social enforcement machinery. The constraint's persistence depends entirely on their continued narration and the community's continued recognition of the stone as a binding marker.
narrative_ontology:constraint_stakeholder(stone_land_use_rule__behavioral_competence, community_custodians, agenda_setter,
    organized, generational, analytical, local).

% Regional environmental agencies, timber companies, and development interests would challenge the constraint if they had standing. They are excluded from the community's decision-making and enforcement. The constraint has no legal statute behind it — it is a cultural rule with 78 years of behavioral coherence. External actors cannot easily intervene without dismantling the community's institutional memory and social authority.
narrative_ontology:constraint_stakeholder(stone_land_use_rule__behavioral_competence, external_policy_actors, excluded,
    institutional, biographical, trapped, regional).

% Examines the constraint as a case of institutional memory embedded in physical practice. Witnesses the stone's role as the kernel of a binding land-use rule sustained across a 78-year interval without written code, without state enforcement, and without the beneficiaries holding formal property titles. Observes the daily enforcement: deliberate placement of the stone in sight-lines, repeated storytelling at collective gatherings, and quiet social pressure on any actor considering slope violation.
narrative_ontology:constraint_stakeholder(stone_land_use_rule__behavioral_competence, anthropological_observer, observer,
    analytical, biographical, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(stone_land_use_rule__behavioral_competence, diffuse).
narrative_ontology:fixing_cost_class(stone_land_use_rule__behavioral_competence, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Prevents catastrophic hillside failure that would destroy downslope dwellings and kill residents; the constraint solves the collective-action problem of slope stability — each holder's isolated decision to harvest or clear would impose diffuse flood/landslip risk on the entire community, so the constraint makes the slope a shared preserve.
% TRANSFER_FUNCTION: Transfers productive land use rights (timber, clearance, agriculture) from individual slope holders to the community as a whole, in exchange for protection from disaster that would affect the entire settlement. The slope remains in individual hands but not in individual use; the constraint collects foregone profit from slope holders and distributes the benefit of reduced flood/landslip risk to downslope residents as a class.
% ABSENT_VOICES: External environmental agencies would support the constraint's intent (slope stabilization, disaster prevention) but would argue for formalization and state backing instead of community memory. Timber companies and regional developers would challenge the constraint altogether if invited to the table — they would argue for selective harvest or development rights. Neither group is included in the community's enforcement structure or decision-making.
% DISAPPEARANCE_RATIONALE: If the stone's authority vanished overnight — if the community ceased to recognize it as binding and collective memory atrophied — the slope would enter a state of individual decision-making. Within months, timber harvest would likely resume on exposed parcels; within years, the forest cover and soil stability would degrade substantially. Heavy rains would trigger mudslides and accelerated runoff that have not occurred in 78 years. Downslope dwellings would face flood and landslip damage; residents would need to relocate or invest heavily in defense infrastructure. The arrangement's disappearance would restructure land use, settlement patterns, and disaster risk entirely.
% FOUNDING_PROBLEM: A major storm and flood in 1946 (year zero of the 78-year interval) destroyed lower dwellings and killed residents, triggered by destabilization of the slope through timber harvest and clearing on the higher ground. The community recognized that individual choices on the slope determined collective survival in the lowlands.
% FOUNDING_PROBLEM_CORROBORATION: Oral histories from elders corroborate the 1946 disaster and the stone's placement immediately after. Regional geological and hydrological surveys (conducted 20 years after constraint establishment) confirm that the slope's current stability is consistent with undisturbed forest cover and that any significant timber harvest would increase landslip and runoff risk. The downslope residents, who have experienced the constraint's protection through 78 years without equivalent disasters, attest that the founding problem remains live (heavy rains still occur; without the slope constraint, the risk would be acute).
narrative_ontology:disappearance_verdict(stone_land_use_rule__behavioral_competence, world_rearranges).
narrative_ontology:founding_problem_status(stone_land_use_rule__behavioral_competence, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(stone_land_use_rule__behavioral_competence, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(stone_land_use_rule__behavioral_competence, 'none', 1).

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
 *   Extractiveness is low (0.28, stable across the interval) because the constraint solves a genuine coordination problem — the slope's stability is a public good that benefits the community as a whole and requires individual forbearance. The beneficiaries (downslope residents) are not separated from the payer class (slope holders) by a mechanism that systematically transfers surplus; instead, the constraint distributes the benefit of risk reduction to all and the cost of foregone productivity to the few who hold slope land. Suppression is minimal (0.15, declining slightly over the interval) because compliance does not depend on coercive threat but on sustained community recognition that the founding problem remains live and that the stone marks the boundary of a binding shared commitment. Theater is very low (0.12) because the constraint's enforcement is functional, not performative — the sight lines and storytelling serve genuine coordination, not cover for extraction. The measurement series show slight decay in suppression_requirement and theater_ratio in the mid-interval (years 26–39) when the founding disaster's witnesses began to age out, followed by stabilization as the rule became consolidated in institutional memory. Extractiveness dips slightly in years 26–52, suggesting that once the rule achieved full social embedding, its coordination function tightened and the residual cost to slope holders became purely the foregone profit, not the tension of enforcement.
 *
 * PERSPECTIVAL GAP:
 *   From the slope-holder's seat, the constraint appears as an imposed limitation on their property rights, justified by a disaster they did not cause and rules they did not set. From the downslope resident's seat, it appears as a protection mechanism they depend on and a commitment the community has honored across generations. From the custodian's seat, it appears as a binding cultural rule that requires continuous work to maintain (repeated storytelling, monitoring) and would collapse immediately if the community stopped narrating its history. The engine's per-seat computation will reveal these asymmetries: the same constraint should compute as mild rope from the beneficiary seat, moderate tangled-rope-adjacent from the payer seat, and rope from the custodian seat (if the custodian collects any benefit from the rule's persistence, that benefit feeds directionality; if not, they remain observer or near-symmetric).
 *
 * DIRECTIONALITY LOGIC:
 *   Slope-adjacent landholders face d ≈ 0.55–0.65 (moderate target): they bear a clear, measurable cost (foregone timber and agriculture revenue) without collecting compensating benefit. However, they remain inside the community (not trapped or identity-locked), and their exit option is mobile (relocation with property sale to someone outside the constraint's scope). The cost is real but not asymmetrically imposed by a dominating agent — it is imposed by the community's collective recognition of shared disaster risk. Downslope residents face d ≈ 0.15–0.25 (mild beneficiary): they receive protection but also bear some cost (the constraint limits the community's economic output and thus constrains investment in other goods). Community custodians face d ≈ 0.5 (symmetric or observer): they administer the rule but derive no extractive benefit; their power is advisory and cultural, not coercive. The engine will compute per-seat divergence from these structural positions, revealing how the constraint operates very differently from each seat.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (slope stability for downslope safety) is decidedly LIVE — the geology and hydrology have not changed; the slope would destabilize under harvest; the risk to downslope dwellings would return immediately upon compliance atrophy. This stands in sharp contrast to constraints where the founding problem is dead but enforcement persists (the piton / theater signature). The behavioral_competence reading asserts that the constraint persists because the problem remains and the community's institutional memory remains functional. The committer frame introduces a rival reading (commemorative_husk) under which the stone persists as a memorial artifact whose warning has atrophied into symbolic gesture — compliance would then be theater driven by residual deference to the past, not by ongoing assessment of the slope's danger. Mandatrophy resolution hinges on this contest: if the community's articulation of WHY they honor the stone remains tied to the founding disaster's mechanism and ongoing risk, the constraint is alive; if the articulation has shifted to cultural continuity or deference to ancestors absent the causal story, the constraint has undergone mandatrophy. The measurement trajectory (stable low theater, declining suppression in the mid-interval) supports the behavioral_competence reading's claim that the rule is not theater but functional coordination.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    community_memory_attenuation,
    'At what generational distance from the founding disaster (1946) will the stone''s binding force begin to weaken as direct witnesses disappear and oral transmission becomes third-hand narration?',
    'Longitudinal ethnographic observation beyond year 78 (present): tracking the frequency and content of storytelling, the inclusion of the slope-disaster narrative in coming-of-age ceremonies, and any observable uptick in slope-boundary violations or boundary disputes.',
    'If memory attenuation occurs (theater_ratio rises above 0.25, suppression_requirement declines below 0.10), the behavioral_competence reading would degrade toward the commemorative_husk reading. The constraint would persist but would shift from live coordination to inertial performance.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(community_memory_attenuation, empirical, 'Whether the stone''s binding force persists across generational memory thresholds.').

omega_variable(
    external_pressure_vulnerability,
    'If regional development or timber interests gain institutional standing (through changing property law, environmental policy, or regional authority), could the stone''s authority withstand challenge?',
    'Scenario: a regional agency recognizes individual property rights over the slope and proposes selective harvest compatible with slope stability. The community''s response (acceptance, negotiation, or principled rejection) reveals whether the stone''s authority is robust to external institutional pressure.',
    'If the community readily accepts external override (shifting the constraint''s legitimacy from community memory to statutory authority), the behavioral_competence reading is falsified — the constraint was not as behaviorally embedded as claimed. If the community resists fiercely (even against economic incentive), the reading is supported.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(external_pressure_vulnerability, empirical, 'Whether the constraint''s authority is robust to external institutional challenge.').

omega_variable(
    reading_forecast_divergence,
    'Is the stone currently functioning as the behavioral_competence reading claims (live binding rule driven by ongoing assessment of slope risk), or is it already drifting toward the commemorative_husk reading (persistent because it is cultural practice, not because the risk remains salient)?',
    'Comparative analysis of community narrative: record the exact language used when explaining the stone''s authority. Does the explanation reference the slope''s danger, the downslope risk, the hydrology, and ongoing weather patterns? Or does it reference tradition, respect for founders, and cultural continuity? The ratio of risk-language to tradition-language indicates which reading''s narrative is live.',
    'If risk-language predominates, the behavioral_competence reading is supported. If tradition-language predominates, the constraint may already be in the commemorative phase despite current compliance. This omega documents the irreducible uncertainty about reading identity at the present moment.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_forecast_divergence, empirical, 'Whether the constraint''s current enforcement narrative centers on risk or on cultural practice.').

omega_variable(
    sibling_reading_coexistence,
    'Can both readings (behavioral_competence and commemorative_husk) be simultaneously true of the same stone, or do they foreclose each other?',
    'If the stone''s authority is derived from behavioral competence (risk assessment) AND from cultural memory (tradition), the readings coexist with different weight in different actors'' reasoning. If the authority is PURELY behavioral or PURELY commemorative, one reading forecloses the other within the community''s coherent framework.',
    'If coexistence is real, the constraint''s type should be measured by the mixture of behavioral and commemorative sources, not by a binary choice. If the readings foreclose each other (and the committer frame asserts they do), the constraint''s classification hinges on which reading the engine endorses based on metric evidence.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sibling_reading_coexistence, conceptual, 'Whether the behavioral and commemorative readings are logically independent or mutually exclusive.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(stone_land_use_rule__behavioral_competence, 0, 78).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ston_tr_t0, stone_land_use_rule__behavioral_competence, theater_ratio, 0, 0.08).
narrative_ontology:measurement_basis(ston_tr_t0, observed).
narrative_ontology:measurement(ston_tr_t12, stone_land_use_rule__behavioral_competence, theater_ratio, 12, 0.09).
narrative_ontology:measurement_basis(ston_tr_t12, observed).
narrative_ontology:measurement(ston_tr_t26, stone_land_use_rule__behavioral_competence, theater_ratio, 26, 0.1).
narrative_ontology:measurement_basis(ston_tr_t26, observed).
narrative_ontology:measurement(ston_tr_t39, stone_land_use_rule__behavioral_competence, theater_ratio, 39, 0.11).
narrative_ontology:measurement_basis(ston_tr_t39, observed).
narrative_ontology:measurement(ston_tr_t52, stone_land_use_rule__behavioral_competence, theater_ratio, 52, 0.115).
narrative_ontology:measurement_basis(ston_tr_t52, observed).
narrative_ontology:measurement(ston_tr_t65, stone_land_use_rule__behavioral_competence, theater_ratio, 65, 0.12).
narrative_ontology:measurement_basis(ston_tr_t65, observed).
narrative_ontology:measurement(ston_tr_t78, stone_land_use_rule__behavioral_competence, theater_ratio, 78, 0.12).
narrative_ontology:measurement_basis(ston_tr_t78, observed).

% Extraction over time
narrative_ontology:measurement(ston_be_t0, stone_land_use_rule__behavioral_competence, base_extractiveness, 0, 0.32).
narrative_ontology:measurement_basis(ston_be_t0, observed).
narrative_ontology:measurement(ston_be_t12, stone_land_use_rule__behavioral_competence, base_extractiveness, 12, 0.29).
narrative_ontology:measurement_basis(ston_be_t12, observed).
narrative_ontology:measurement(ston_be_t26, stone_land_use_rule__behavioral_competence, base_extractiveness, 26, 0.27).
narrative_ontology:measurement_basis(ston_be_t26, observed).
narrative_ontology:measurement(ston_be_t39, stone_land_use_rule__behavioral_competence, base_extractiveness, 39, 0.26).
narrative_ontology:measurement_basis(ston_be_t39, observed).
narrative_ontology:measurement(ston_be_t52, stone_land_use_rule__behavioral_competence, base_extractiveness, 52, 0.27).
narrative_ontology:measurement_basis(ston_be_t52, observed).
narrative_ontology:measurement(ston_be_t65, stone_land_use_rule__behavioral_competence, base_extractiveness, 65, 0.28).
narrative_ontology:measurement_basis(ston_be_t65, observed).
narrative_ontology:measurement(ston_be_t78, stone_land_use_rule__behavioral_competence, base_extractiveness, 78, 0.28).
narrative_ontology:measurement_basis(ston_be_t78, observed).

% Suppression requirement over time
narrative_ontology:measurement(ston_su_t0, stone_land_use_rule__behavioral_competence, suppression_requirement, 0, 0.18).
narrative_ontology:measurement_basis(ston_su_t0, observed).
narrative_ontology:measurement(ston_su_t12, stone_land_use_rule__behavioral_competence, suppression_requirement, 12, 0.16).
narrative_ontology:measurement_basis(ston_su_t12, observed).
narrative_ontology:measurement(ston_su_t26, stone_land_use_rule__behavioral_competence, suppression_requirement, 26, 0.15).
narrative_ontology:measurement_basis(ston_su_t26, observed).
narrative_ontology:measurement(ston_su_t39, stone_land_use_rule__behavioral_competence, suppression_requirement, 39, 0.145).
narrative_ontology:measurement_basis(ston_su_t39, observed).
narrative_ontology:measurement(ston_su_t52, stone_land_use_rule__behavioral_competence, suppression_requirement, 52, 0.148).
narrative_ontology:measurement_basis(ston_su_t52, observed).
narrative_ontology:measurement(ston_su_t65, stone_land_use_rule__behavioral_competence, suppression_requirement, 65, 0.15).
narrative_ontology:measurement_basis(ston_su_t65, observed).
narrative_ontology:measurement(ston_su_t78, stone_land_use_rule__behavioral_competence, suppression_requirement, 78, 0.15).
narrative_ontology:measurement_basis(ston_su_t78, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(stone_land_use_rule__behavioral_competence, resource_allocation).
narrative_ontology:boltzmann_floor_override(stone_land_use_rule__behavioral_competence, 0.25).
narrative_ontology:affects_constraint(stone_land_use_rule__behavioral_competence, stone_land_use_rule__commemorative_husk).

% DUAL FORMULATION NOTE:
% The stone_land_use_rule kernel has two structurally distinct readings. The behavioral_competence reading treats the stone as a live land-use binding enforced through daily practice and ongoing assessment of the slope's danger; extractiveness is low (0.28), theater is minimal (0.12). The commemorative_husk reading (alternate constraint story) treats the stone as a memorial artifact whose behavioral force has atrophied into cultural inertia; that reading would compute substantially higher extractiveness. Both stories reference the same physical object (the stone) and the same historical event (the 1946 disaster), but their ε-values differ because they differ on whether the constraint's persistence is driven by ongoing functional need or by residual cultural deference. The readings coexist as different parties' understandings of the same constraint; over time, as memory attenuates, one reading may foreclose the other.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
