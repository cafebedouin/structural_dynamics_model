% ============================================================================
% CONSTRAINT STORY: stone_land_use_rule__commemorative_husk
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
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
 *   human_readable: Stone Marker as Commemorative Husk (Decayed Land-Use Warning)
 *   domain: disaster_anthropology/institutional_memory/land_use_governance
 *
 * SUMMARY:
 *   A stone marker originally placed to warn against settlement in a hazard
 *   zone (flood plain, seismic zone, or subsidence area) has been reframed as
 *   a cultural/memorial artifact. Its function has transformed from
 *   behavioral constraint (do not build here) to symbolic gesture (remember
 *   what happened here). Planning authorities issue development permits for
 *   the marked zone; the stone is protected as heritage while the land is
 *   treated as safe for construction. The underlying hazard persists
 *   (confirmed by scientific assessment), but the constraint's degradation
 *   means no active mechanism prevents development. The extractiveness score
 *   reflects the benefit to developers and the cost to future inhabitants;
 *   the theater ratio (0.88) reflects that almost all enforcement activity is
 *   now interpretive/ceremonial rather than land-use-restrictive.
 *
 * KEY AGENTS:
 *   - Waterfront developers: benefit from constraint degradation; permits proceed unimpeded.
 *   - Monument custodians: maintain memorial frame; interpret stone as cultural artifact.
 *   - Planning authorities: issue building permits in marked zone; bear future liability.
 *   - Future inhabitants: occupy structures in hazard zone; powerless to exit; bear consequence-risk.
 *   - Original warning-bearers: deceased/displaced; voices excluded from current frame.
 *   - Hazard scientists: monitor the underlying geophysical condition; observations marginalized by memorial narrative.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(stone_land_use_rule__commemorative_husk, 0.82).
domain_priors:suppression_score(stone_land_use_rule__commemorative_husk, 0.15).
domain_priors:theater_ratio(stone_land_use_rule__commemorative_husk, 0.88).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(stone_land_use_rule__commemorative_husk, extractiveness, 0.82).
narrative_ontology:constraint_metric(stone_land_use_rule__commemorative_husk, suppression_requirement, 0.15).
narrative_ontology:constraint_metric(stone_land_use_rule__commemorative_husk, theater_ratio, 0.88).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(stone_land_use_rule__commemorative_husk, accessibility_collapse, 0.22).
narrative_ontology:constraint_metric(stone_land_use_rule__commemorative_husk, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(stone_land_use_rule__commemorative_husk, piton).
narrative_ontology:human_readable(stone_land_use_rule__commemorative_husk, "Stone Marker as Commemorative Husk (Decayed Land-Use Warning)").
narrative_ontology:topic_domain(stone_land_use_rule__commemorative_husk, "disaster_anthropology/institutional_memory/land_use_governance").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(stone_land_use_rule__commemorative_husk, 'bc495d61-ad31-44ed-958e-1f5db5c10f9c').
narrative_ontology:cs_kernel_codification('bc495d61-ad31-44ed-958e-1f5db5c10f9c', fixed_text).
narrative_ontology:cs_authority_grounding('bc495d61-ad31-44ed-958e-1f5db5c10f9c', extraction).
narrative_ontology:cs_interpretation_layer_present('bc495d61-ad31-44ed-958e-1f5db5c10f9c').
narrative_ontology:cs_reading_relation('bc495d61-ad31-44ed-958e-1f5db5c10f9c', stone_land_use_rule__behavioral_competence, coexists_with).
narrative_ontology:cs_axiom('bc495d61-ad31-44ed-958e-1f5db5c10f9c', foundational, stone_is_cultural_memorial_artifact).
narrative_ontology:cs_axiom_status(stone_is_cultural_memorial_artifact, holdable).
narrative_ontology:cs_axiom_grounding('bc495d61-ad31-44ed-958e-1f5db5c10f9c', stone_is_cultural_memorial_artifact, conventional).
narrative_ontology:cs_axiom('bc495d61-ad31-44ed-958e-1f5db5c10f9c', secondary, hazard_avoidance_superseded_by_modern_infrastructure).
narrative_ontology:cs_axiom_status(hazard_avoidance_superseded_by_modern_infrastructure, holdable).
narrative_ontology:cs_axiom_grounding('bc495d61-ad31-44ed-958e-1f5db5c10f9c', hazard_avoidance_superseded_by_modern_infrastructure, empirically_contingent).
narrative_ontology:cs_reference_frame('bc495d61-ad31-44ed-958e-1f5db5c10f9c', hazard_avoidance_prohibition).
narrative_ontology:cs_drift_state('bc495d61-ad31-44ed-958e-1f5db5c10f9c', contemporary_memorial_curation_era, gap(codification_collapse, severe, false)).
narrative_ontology:cs_created_at('bc495d61-ad31-44ed-958e-1f5db5c10f9c', '').
narrative_ontology:cs_kernel_id(stone_land_use_rule__commemorative_husk, stone_land_use_rule).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(stone_land_use_rule__commemorative_husk, waterfront_developers).
narrative_ontology:constraint_victim(stone_land_use_rule__commemorative_husk, future_inhabitants).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(stone_land_use_rule__commemorative_husk, planning_authorities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Building near or on the marked hazard zone proceeds unimpeded. The stone's presence is treated as historical artifact, not behavioral constraint. They benefit from the constraint's degradation: the warning has lost its performative force, clearing the land for development that real enforcement would prohibit. The memorial status protects the stone from removal while the land remains developable.
narrative_ontology:constraint_stakeholder(stone_land_use_rule__commemorative_husk, waterfront_developers, beneficiary,
    institutional, biographical, arbitrage, regional).

% Maintain, interpret, and defend the stone's preservation. They have created a narrative frame around it as memorial, cultural artifact, historical marker—not as an active constraint on behavior. They conduct ceremonies, interpret its history, and argue for its protection on grounds of cultural value and remembrance. The interpretive layer absorbs the original warning function into heritage rhetoric.
narrative_ontology:constraint_stakeholder(stone_land_use_rule__commemorative_husk, monument_custodians, agenda_setter,
    organized, generational, constrained, regional).

% Make zoning and development decisions. They officially acknowledge the stone's historical significance and protect its physical location. However, they treat the marked zone as safe for development, issuing building permits without behavioral restrictions tied to the stone's original warning. They bear the future liability burden if the hazard the stone originally marked materializes.
narrative_ontology:constraint_stakeholder(stone_land_use_rule__commemorative_husk, planning_authorities, agenda_setter,
    institutional, generational, mobile, regional).
narrative_ontology:stakeholder_secondary_role(stone_land_use_rule__commemorative_husk, planning_authorities, payer).

% Will occupy or depend on structures built in the marked zone. They cannot exit the geography; the stone's warning is historical decoration they may never encounter or understand. If the underlying hazard (flood, collapse, tsunami, subsidence) materializes, they bear the consequences while the constraint's transformation into theater means no active defense mechanism exists.
narrative_ontology:constraint_stakeholder(stone_land_use_rule__commemorative_husk, future_inhabitants, payer,
    powerless, generational, trapped, regional).

% The community or individuals who originally placed or heeded the stone's warning are dead or displaced. They would object to the constraint's degradation, but their voices are inaccessible. The memorial frame substitutes remembrance-of-the-people for adherence-to-the-warning.
narrative_ontology:constraint_stakeholder(stone_land_use_rule__commemorative_husk, original_warning_bearers, excluded,
    powerless, civilizational, trapped, regional).

% Track the underlying geophysical or hydrological condition the stone originally warned about. They may produce reports confirming the hazard persists; these reports circulate separately from the stone's interpretive frame and rarely drive planning decisions when the memorial narrative dominates.
narrative_ontology:constraint_stakeholder(stone_land_use_rule__commemorative_husk, hazard_monitoring_scientists, observer,
    institutional, generational, analytical, regional).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(stone_land_use_rule__commemorative_husk, waterfront_developers).
narrative_ontology:fixing_cost_class(stone_land_use_rule__commemorative_husk, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: None remaining in operational terms. The constraint originally coordinated collective spatial practice around a shared hazard: 'do not build here.' That coordination function has atrophied. The current function is cultural/memorial: remembrance of a past event and the people whose warning created the marker.
% TRANSFER_FUNCTION: Moves development rights to waterfront property owners, away from spatial caution, enabled by the stone's reframing as historical artifact rather than behavioral constraint. The underlying transfer is hazard-risk-exposure toward powerless future inhabitants and away from developers and planning authorities (who retain decision power but outsource consequence-bearing).
% ABSENT_VOICES: Original warning bearers (deceased or displaced) who established the stone's behavioral meaning; future inhabitants who will occupy the zone and bear the hazard risk; any organized hazard-response community that might argue for the constraint's reactivation. These parties are structurally excluded from decisions because the stone is now curated as memory, not policed as rule.
% DISAPPEARANCE_RATIONALE: If the stone and its memorial significance disappeared overnight, planning would already be underway for the waterfront zone (it effectively is); the only change would be loss of the historical narrative frame and possibly a small heritage tourism revenue stream. The underlying land-use pattern would be unaffected because the stone has no behavioral force. Conversely, if the stone's warning function were reactivated—building restrictions imposed to match the original hazard—the world would rearrange dramatically: development permits would be rescinded, property values would collapse, and planning would shift away from the marked zone.
% FOUNDING_PROBLEM: A disaster (flood, landslide, tsunami, or subsidence event) killed or displaced people in this location. Survivors or witnesses marked the site with a stone to warn subsequent generations: do not settle or build here; this place is dangerous.
% FOUNDING_PROBLEM_CORROBORATION: Geological surveys and hazard assessments by scientists outside the memorial custodian community confirm the underlying hazard persists (e.g., the flood plain is still active, the seismic zone is still active). However, planning authorities cite the monument's 'historical' status rather than these assessments when making development decisions. The corroboration is contested: scientists attest the hazard is live; planners attest the founding problem has been 'solved' by modern infrastructure (flood walls, building codes) that render the stone's original warning obsolete. The memorial frame uses this contested corroboration to justify treating the stone as artifact rather than constraint.
narrative_ontology:disappearance_verdict(stone_land_use_rule__commemorative_husk, world_rearranges).
narrative_ontology:founding_problem_status(stone_land_use_rule__commemorative_husk, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(stone_land_use_rule__commemorative_husk, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
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
 *   Extractiveness rises from 0.45 to 0.82 because the constraint's behavioral force decays while development intensity in the zone increases. At t=0 the stone still carries some cultural weight as warning; by t=40 it is purely memorial, and waterfront development has proceeded unimpeded. Suppression falls from 0.35 to 0.08 because there is nothing left to suppress—the constraint has been negotiated out of existence through the memorial frame. Theater rises from 0.55 to 0.88 because what remains is ceremony and heritage interpretation with zero behavioral effect. Accessibility collapse is low (0.22) because alternatives to building in the zone technically exist (inland sites are available); the constraint's weakness means developers do not need to collapse alternatives—they can build where they want. Resistance is high (0.71) because scientists and hazard-response communities mount real objections, but these are channeled into academic publications and planning-board comments that the memorial frame brackets as 'historical interest' rather than operational constraint. The measurement series on a shared time grid shows the one-direction drift: the constraint atrophies into theater and extraction.
 *
 * PERSPECTIVAL GAP:
 *   The constraint manifests differently to each seat: developers see obsolescence-of-warning (the stone's job is done; modern codes handle safety); custodians see preservation-imperative (the stone is culturally vital; its meaning is remembrance, not prohibition); planners see dual-duty-negotiation (history and development both matter; the memorial satisfies the history requirement while zoning permits development); scientists see hazard-persistence (the geological condition has not changed; the stone's original warning is still valid). Future inhabitants see nothing—they are not in the conversation. The engine identifies this as structural powerlessness (d → 1.0) coupled with identity-lockedness (they cannot exit the geography).
 *
 * DIRECTIONALITY LOGIC:
 *   Waterfront developers benefit from the constraint's degradation—their d-value approaches 0.0 (full beneficiary). Monument custodians and planning authorities sit near symmetric (d ≈ 0.5): they nominally coordinate remembrance and heritage preservation, but they also benefit from the land's developability and the memorial frame's resolution of the constraint-versus-development tension. Future inhabitants are the true targets (d ≈ 1.0): they bear hazard-zone exposure without having any say in its acceptance and without the cognitive/behavioral frame (the working stone) that would have signaled danger. The constraint's transformation into theater means directionality has inverted: what was once a shared warning (symmetric) is now extraction toward powerless future actors.
 *
 * MANDATROPHY ANALYSIS:
 *   This is a piton: the founding problem (hazard in this location) is dead in the framing but live in the underlying condition. The original constraint (spatial prohibition) was built to solve it. The constraint persists not because any party benefits enough to maintain it and no party is hurt enough to fix it—but rather because the constraint has been reclassified out of existence. No one is hurt by 'maintaining' a memorial stone; the hurt comes from land-use decisions the stone no longer constrains. Mandatrophy is resolved: the constraint's mandate (prevent building in hazard zone) is no longer its function (remember-the-warning). The theater ratio's rise (0.55 → 0.88) confirms the diagnosis: the remaining activity is ceremonial maintenance with zero behavioral force.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    hazard_persistence_vs_infrastructure_mitigation,
    'Does the underlying geophysical hazard (flood, subsidence, seismic activity) persist at the original magnitude, or has modern infrastructure (dikes, building codes, monitoring) effectively mitigated it below the threshold the original warning responded to?',
    'Comparative hazard assessment: geophysical surveys of the marked zone against modern engineering safety thresholds. If infrastructure has dropped the residual risk below historical event magnitude, the stone''s warning is empirically obsolete; if residual risk remains above historical event magnitude, the warning is live but ignored.',
    'If hazard persists unmitigated: the constraint''s transformation from behavioral to theatrical is unambiguous extraction; if mitigated: the constraint''s degradation is partial justification. The reading does not change—the stone is still memorial—but the cost class of ignoring it shifts.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(hazard_persistence_vs_infrastructure_mitigation, empirical, 'Whether the underlying hazard that motivated the stone''s placement persists at actionable magnitude.').

omega_variable(
    memorial_versus_behavioral_decomposition,
    'Is it structurally possible for the stone to function simultaneously as cultural memorial AND behavioral constraint, or does the memorial frame necessarily occlude the behavioral warning once a community begins heritage curation?',
    'Ethnographic and historical analysis of communities that maintain active spatial taboos around marked sites while also treating the marks as cultural artifacts. If parallel function is observed elsewhere, it is theoretically possible; if memorial frame universally displaces behavioral function, the constraint is a one-way transformation.',
    'If simultaneous function is possible: the reading is contingent on planning-authority choice to bracket the behavioral function, and could be reversed by political decision. If memorial-absorbs-behavioral is structural: the reading reflects an irreversible transition and the constraint is truly a piton (no party can resurrect the warning by changing frame alone).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(memorial_versus_behavioral_decomposition, conceptual, 'Whether memorial significance and behavioral constraint are structurally separable or mutually exclusive in institutional practice.').

omega_variable(
    future_inhabitant_knowledge_collapse,
    'Will future inhabitants occupy the marked zone with knowledge that a warning stone exists there, or will the stone''s location, meaning, and historical context be lost to them?',
    'Post-occupancy ethnography and cognitive science: do residents of buildings constructed in marked zones know why the stone is there and what it warned about? Does that knowledge influence their behavior or sense of safety?',
    'If knowledge persists: future inhabitants carry informational hazard-awareness even if the behavioral constraint is gone (partial protection). If knowledge is lost: they occupy hazard zones with zero signal of danger (maximum extraction and powerlessness). This does not change the reading—the stone is still memorial—but it determines whether extraction is pure (knowledge loss) or partial (knowledge persists).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(future_inhabitant_knowledge_collapse, empirical, 'Whether the stone''s historical meaning survives transmission to future occupants of the marked zone.').

omega_variable(
    kernel_reading_distinction_hazard_inference,
    'What is the minimal observable evidence that would distinguish the ''commemorative_husk'' reading (this one: zero behavioral force) from the ''behavioral_competence'' reading (sibling: the stone is a live constraint)?',
    'Examine actual land-use decisions: are buildings being constructed in the marked zone (strong evidence for commemorative_husk)? Are land parcels remaining undeveloped and owners citing the stone as reason (evidence for behavioral_competence)? Behavioral evidence is the disambiguator because the readings split on WHETHER the constraint has behavioral force, not on WHAT the stone is.',
    'This omega names the kernel contest itself: the two readings are observationally distinct if you look at spatial practice. If development proceeds, this reading is corroborated; if it does not, the sibling is corroborated. The readings coexist_with each other because different jurisdictions will show different behavior.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_distinction_hazard_inference, empirical, 'The kernel contest between memorial artifact and live constraint is resolved by observing whether the stone constrains land use or merely marks historical site.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(stone_land_use_rule__commemorative_husk, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ston_tr_t0, stone_land_use_rule__commemorative_husk, theater_ratio, 0, 0.55).
narrative_ontology:measurement(ston_tr_t5, stone_land_use_rule__commemorative_husk, theater_ratio, 5, 0.62).
narrative_ontology:measurement(ston_tr_t10, stone_land_use_rule__commemorative_husk, theater_ratio, 10, 0.68).
narrative_ontology:measurement(ston_tr_t15, stone_land_use_rule__commemorative_husk, theater_ratio, 15, 0.73).
narrative_ontology:measurement(ston_tr_t20, stone_land_use_rule__commemorative_husk, theater_ratio, 20, 0.78).
narrative_ontology:measurement(ston_tr_t25, stone_land_use_rule__commemorative_husk, theater_ratio, 25, 0.82).
narrative_ontology:measurement(ston_tr_t30, stone_land_use_rule__commemorative_husk, theater_ratio, 30, 0.85).
narrative_ontology:measurement(ston_tr_t40, stone_land_use_rule__commemorative_husk, theater_ratio, 40, 0.88).

% Extraction over time
narrative_ontology:measurement(ston_be_t0, stone_land_use_rule__commemorative_husk, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(ston_be_t5, stone_land_use_rule__commemorative_husk, base_extractiveness, 5, 0.52).
narrative_ontology:measurement(ston_be_t10, stone_land_use_rule__commemorative_husk, base_extractiveness, 10, 0.61).
narrative_ontology:measurement(ston_be_t15, stone_land_use_rule__commemorative_husk, base_extractiveness, 15, 0.68).
narrative_ontology:measurement(ston_be_t20, stone_land_use_rule__commemorative_husk, base_extractiveness, 20, 0.74).
narrative_ontology:measurement(ston_be_t25, stone_land_use_rule__commemorative_husk, base_extractiveness, 25, 0.78).
narrative_ontology:measurement(ston_be_t30, stone_land_use_rule__commemorative_husk, base_extractiveness, 30, 0.81).
narrative_ontology:measurement(ston_be_t40, stone_land_use_rule__commemorative_husk, base_extractiveness, 40, 0.82).

% Suppression requirement over time
narrative_ontology:measurement(ston_su_t0, stone_land_use_rule__commemorative_husk, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(ston_su_t5, stone_land_use_rule__commemorative_husk, suppression_requirement, 5, 0.28).
narrative_ontology:measurement(ston_su_t10, stone_land_use_rule__commemorative_husk, suppression_requirement, 10, 0.22).
narrative_ontology:measurement(ston_su_t15, stone_land_use_rule__commemorative_husk, suppression_requirement, 15, 0.18).
narrative_ontology:measurement(ston_su_t20, stone_land_use_rule__commemorative_husk, suppression_requirement, 20, 0.15).
narrative_ontology:measurement(ston_su_t25, stone_land_use_rule__commemorative_husk, suppression_requirement, 25, 0.12).
narrative_ontology:measurement(ston_su_t30, stone_land_use_rule__commemorative_husk, suppression_requirement, 30, 0.1).
narrative_ontology:measurement(ston_su_t40, stone_land_use_rule__commemorative_husk, suppression_requirement, 40, 0.08).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(stone_land_use_rule__commemorative_husk, identity_coordination).
narrative_ontology:boltzmann_floor_override(stone_land_use_rule__commemorative_husk, 0.12).
narrative_ontology:affects_constraint(stone_land_use_rule__commemorative_husk, stone_land_use_rule__behavioral_competence).

% DUAL FORMULATION NOTE:
% This constraint and stone_land_use_rule__behavioral_competence form a kernel family. The two readings split on whether the stone functions as a behavioral land-use constraint (behavioral_competence) or has decayed into a cultural memorial without operative force (commemorative_husk). Both readings can be simultaneously true in different jurisdictions or for different parties. They are not competing versions of the same fact—they are different functional instantiations of the same artifact, distinguished by whether the underlying hazard is actively avoided or passively remembered. This reading (commemorative_husk) establishes zero behavioral constraint and high extractiveness (developers benefit from the constraint's degradation). The sibling reading establishes active spatial taboo and coordination around hazard avoidance. The kernel is the stone itself; the readings differ on what the stone DOES in contemporary practice.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
