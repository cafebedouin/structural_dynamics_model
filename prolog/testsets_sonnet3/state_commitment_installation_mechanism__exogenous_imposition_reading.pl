% ============================================================================
% CONSTRAINT STORY: state_commitment_installation_mechanism__exogenous_imposition_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_state_commitment_installation_mechanism__exogenous_imposition_reading, []).

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
 *   constraint_id: state_commitment_installation_mechanism__exogenous_imposition_reading
 *   human_readable: Top-Down Decree Installation of New State Commitments (Exogenous Imposition Reading)
 *   domain: historical_sociology/state_formation/cultural_authority
 *
 * SUMMARY:
 *   This story instantiates the exogenous-imposition reading of the
 *   state-commitment-installation kernel: a central authority holding a
 *   self-declared transformation mandate installs a new legal, religious,
 *   linguistic, or tenurial commitment by decree, without prior demonstration
 *   or grassroots advocacy at the institutional fringe. Legitimacy is
 *   asserted top-down and enforced through administrative and coercive
 *   apparatus; resistance concentrates at the base among customary
 *   authorities and peripheral communities whose standing the decree voids.
 *   This is a distinct constraint from the endogenous_climb_reading (where
 *   legitimacy accrues bottom-up through demonstrated local superiority, no
 *   coercive installation apparatus) and the hybrid_cascade_reading
 *   (apex-installed but requiring subsequent fringe validation to stabilize —
 *   a two-phase process this reading denies is necessary). Each reading has
 *   its own ε: exogenous imposition runs high extraction and high suppression
 *   by construction (that is the reading's structural claim), while a genuine
 *   endogenous-climb constraint would show low suppression and voluntary
 *   uptake. These are not the same constraint measured differently — they are
 *   three constraints sharing a kernel.
 *
 * KEY AGENTS:
 *   - central_state_apparatus: agenda_setter/beneficiary (institutional/arbitrage) — issues the decree, enforces it, consolidates jurisdiction
 *   - modernizing_elite_faction: beneficiary (powerful/mobile) — supplies the ideological rationale, staffs the new institutions
 *   - local_customary_authorities: payer (moderate/trapped) — lose adjudicative function without transition
 *   - peripheral_communities: payer (powerless/trapped) — bear coerced compliance costs
 *   - displaced_practitioners_of_prior_norm: payer (powerless/trapped) — lose livelihood/status in a single administrative act
 *   - reform_era_historians: observer (analytical) — reconstructs the installation-versus-legitimation record from archives
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(state_commitment_installation_mechanism__exogenous_imposition_reading, 0.68).
domain_priors:suppression_score(state_commitment_installation_mechanism__exogenous_imposition_reading, 0.79).
domain_priors:theater_ratio(state_commitment_installation_mechanism__exogenous_imposition_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(state_commitment_installation_mechanism__exogenous_imposition_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(state_commitment_installation_mechanism__exogenous_imposition_reading, suppression_requirement, 0.79).
narrative_ontology:constraint_metric(state_commitment_installation_mechanism__exogenous_imposition_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(state_commitment_installation_mechanism__exogenous_imposition_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(state_commitment_installation_mechanism__exogenous_imposition_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(state_commitment_installation_mechanism__exogenous_imposition_reading, tangled_rope).
narrative_ontology:human_readable(state_commitment_installation_mechanism__exogenous_imposition_reading, "Top-Down Decree Installation of New State Commitments (Exogenous Imposition Reading)").
narrative_ontology:topic_domain(state_commitment_installation_mechanism__exogenous_imposition_reading, "historical_sociology/state_formation/cultural_authority").

domain_priors:requires_active_enforcement(state_commitment_installation_mechanism__exogenous_imposition_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(state_commitment_installation_mechanism__exogenous_imposition_reading, 'cc8236ce-50b8-4fb5-8588-817f70ce4e1f').
narrative_ontology:cs_kernel_codification('cc8236ce-50b8-4fb5-8588-817f70ce4e1f', formalized).
narrative_ontology:cs_authority_grounding('cc8236ce-50b8-4fb5-8588-817f70ce4e1f', extraction).
narrative_ontology:cs_interpretation_layer_present('cc8236ce-50b8-4fb5-8588-817f70ce4e1f').
narrative_ontology:cs_reading_relation('cc8236ce-50b8-4fb5-8588-817f70ce4e1f', state_commitment_installation_mechanism__endogenous_climb_reading, forecloses).
narrative_ontology:cs_reading_relation('cc8236ce-50b8-4fb5-8588-817f70ce4e1f', state_commitment_installation_mechanism__hybrid_cascade_reading, influences).
narrative_ontology:cs_axiom('cc8236ce-50b8-4fb5-8588-817f70ce4e1f', foundational, mandate_holder_authority_sufficient_for_legitimacy).
narrative_ontology:cs_axiom_status(mandate_holder_authority_sufficient_for_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('cc8236ce-50b8-4fb5-8588-817f70ce4e1f', mandate_holder_authority_sufficient_for_legitimacy, conventional).
narrative_ontology:cs_axiom('cc8236ce-50b8-4fb5-8588-817f70ce4e1f', foundational, grassroots_demonstration_not_required_for_stable_adoption).
narrative_ontology:cs_axiom_status(grassroots_demonstration_not_required_for_stable_adoption, holdable).
narrative_ontology:cs_axiom_grounding('cc8236ce-50b8-4fb5-8588-817f70ce4e1f', grassroots_demonstration_not_required_for_stable_adoption, empirically_contingent).
narrative_ontology:cs_reference_frame('cc8236ce-50b8-4fb5-8588-817f70ce4e1f', mandate_holder_transformation_authority).
narrative_ontology:cs_drift_state('cc8236ce-50b8-4fb5-8588-817f70ce4e1f', post_installation_enforcement_plateau, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('cc8236ce-50b8-4fb5-8588-817f70ce4e1f', '').
narrative_ontology:cs_kernel_id(state_commitment_installation_mechanism__exogenous_imposition_reading, state_commitment_installation_mechanism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(state_commitment_installation_mechanism__exogenous_imposition_reading, central_state_apparatus).
narrative_ontology:constraint_beneficiary(state_commitment_installation_mechanism__exogenous_imposition_reading, modernizing_elite_faction).
narrative_ontology:constraint_victim(state_commitment_installation_mechanism__exogenous_imposition_reading, local_customary_authorities).
narrative_ontology:constraint_victim(state_commitment_installation_mechanism__exogenous_imposition_reading, peripheral_communities).
narrative_ontology:constraint_victim(state_commitment_installation_mechanism__exogenous_imposition_reading, displaced_practitioners_of_prior_norm).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Holds a self-declared transformation mandate (post-revolutionary, post-independence, or reform-era) and issues decree, statute, or administrative order installing the new commitment (a legal code, a religious reform, a language standard, a land tenure regime) without prior demonstration at the periphery. Deploys administrative, educational, and coercive apparatus to enforce compliance and frames resistance as backwardness requiring correction. Collects legitimacy, consolidated jurisdiction, and often direct revenue or resource control from the new arrangement.
narrative_ontology:constraint_stakeholder(state_commitment_installation_mechanism__exogenous_imposition_reading, central_state_apparatus, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(state_commitment_installation_mechanism__exogenous_imposition_reading, central_state_apparatus, beneficiary).

% Urban, educated, or cosmopolitan faction whose social capital is denominated in the new commitment's terms (new legal literacy, new religious orthodoxy, new administrative language). Advocates for the decree from inside the state apparatus or adjacent institutions, gains preferential access to the positions the new order creates, and can relocate or code-switch if local backlash grows severe.
narrative_ontology:constraint_stakeholder(state_commitment_installation_mechanism__exogenous_imposition_reading, modernizing_elite_faction, beneficiary,
    powerful, biographical, mobile, national).

% Village elders, customary judges, local clergy, or hereditary officeholders whose authority derived from the prior commitment. The decree strips their adjudicative or ritual function overnight without transitional accommodation; they cannot relocate their authority elsewhere because it was place-bound and relationship-bound. Some are absorbed into the new apparatus at reduced status; most are simply superseded.
narrative_ontology:constraint_stakeholder(state_commitment_installation_mechanism__exogenous_imposition_reading, local_customary_authorities, payer,
    moderate, biographical, trapped, regional).

% Populations at geographic or social distance from the capital who receive the new commitment as an unfamiliar imposition — new legal categories, new taxation logic, new marriage or inheritance rules — enforced by administrators, police, or troops with no local mandate. Compliance is coerced through fines, land reallocation, or physical enforcement; genuine exit means flight, which is costly and rare.
narrative_ontology:constraint_stakeholder(state_commitment_installation_mechanism__exogenous_imposition_reading, peripheral_communities, payer,
    powerless, biographical, trapped, regional).

% Individuals whose livelihood, status, or identity was constituted by the superseded arrangement — scribes trained in the old legal script, clergy of the disestablished order, holders of customary land titles now voided. The decree renders their accumulated skill and standing worthless in a single administrative act, with no phased transition and no compensation mechanism built into the installation.
narrative_ontology:constraint_stakeholder(state_commitment_installation_mechanism__exogenous_imposition_reading, displaced_practitioners_of_prior_norm, payer,
    powerless, biographical, trapped, local).

% Examine the archival record of decree, enforcement correspondence, and periphery petitions/rebellions to reconstruct whether the new commitment actually took root through installation alone or required subsequent local re-legitimation the official record suppresses.
narrative_ontology:constraint_stakeholder(state_commitment_installation_mechanism__exogenous_imposition_reading, reform_era_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(state_commitment_installation_mechanism__exogenous_imposition_reading, central_state_apparatus).
narrative_ontology:fixing_cost_class(state_commitment_installation_mechanism__exogenous_imposition_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Rapid, uniform replacement of a fragmented or contested prior arrangement (customary law, plural religious authority, decentralized land tenure) with a single legible standard the state can administer, tax, and project power through — solving a genuine coordination problem for the center's own governance capacity.
% TRANSFER_FUNCTION: Moves adjudicative authority, ritual/legal legitimacy, and often land or tax revenue from customary and peripheral authorities to the central state apparatus and the elite faction whose credentials match the new order; moves compliance costs and status losses onto peripheral communities and displaced practitioners.
% ABSENT_VOICES: Peripheral communities and customary authorities are not consulted before the decree; petitions and revolts after the fact are the only record of their objection, and are frequently classified in official archives as disorder to be suppressed rather than as a legitimate counter-claim.
% DISAPPEARANCE_RATIONALE: If the installation mechanism were withdrawn (decree rescinded, enforcement apparatus stood down), customary authorities would reassert prior adjudicative function, displaced practitioners would resume prior roles where communities remembered them, and the state would lose the uniform administrative surface the decree created — governance would fragment back toward the pre-decree plural arrangement.
% FOUNDING_PROBLEM: The center faced (or claimed to face) an urgent transformation problem — national unification, modernization, religious reform, post-conflict consolidation — that fragmented customary authority was seen as unable to solve fast enough, and the state's mandate-holders installed a new commitment by decree to resolve it in one administrative move.
% FOUNDING_PROBLEM_CORROBORATION: The central state apparatus and modernizing elite faction attest the founding problem was real and remains substantially live (ongoing modernization or unification imperative). Independent historians working from periphery petition archives and post-decree rebellion records attest the problem was real at the center but the installation mechanism outran demonstrated local need — the decree's speed and coercive enforcement pattern is corroborated by administrative correspondence describing suppression of customary courts rather than negotiated transition, a source outside both benefiting factions.
narrative_ontology:disappearance_verdict(state_commitment_installation_mechanism__exogenous_imposition_reading, world_rearranges).
narrative_ontology:founding_problem_status(state_commitment_installation_mechanism__exogenous_imposition_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(state_commitment_installation_mechanism__exogenous_imposition_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(state_commitment_installation_mechanism__exogenous_imposition_reading, 'none', 1).
narrative_ontology:epsilon_provenance(state_commitment_installation_mechanism__exogenous_imposition_reading, 0.68, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(state_commitment_installation_mechanism__exogenous_imposition_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(state_commitment_installation_mechanism__exogenous_imposition_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(state_commitment_installation_mechanism__exogenous_imposition_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68 at interval end) reflects the transfer of adjudicative authority, land, and status from customary/peripheral actors to the center and its elite faction — the decree's speed prevents the kind of negotiated buy-in that would lower measured extraction. Suppression (0.79, peaking near 0.82 at the enforcement crest before settling) is high because this reading's defining structural claim IS that legitimacy is installed rather than earned — the apparatus must actively suppress the customary alternative rather than out-compete it. Theater ratio rises over the interval (0.20 to 0.42) as the initial coercive installation phase gives way to increasing performative compliance rituals (ceremonial oaths to the new code, staged public adoption events) that substitute for genuine local uptake — a Goodhart-style drift where visible compliance replaces the substantive transformation the mandate claimed to deliver.
 *
 * PERSPECTIVAL GAP:
 *   From the central state apparatus's seat, the decree is coordination — it solves a real governance-fragmentation problem the center actually faced. From the local customary authority's seat, the identical decree is extraction — a functioning (if imperfect) local order was overridden without negotiation. The engine computes both seats from the same structural data; the divergence is exactly what the tangled_rope classification is for: genuine coordination function (national administrative legibility) coexisting with asymmetric extraction (customary authority stripped without compensation) held together by active enforcement.
 *
 * DIRECTIONALITY LOGIC:
 *   The central state apparatus and modernizing elite faction sit near the full-beneficiary end: they hold arbitrage/mobile exit and directly collect the consolidated jurisdiction, revenue, or status the new commitment creates. Local customary authorities, peripheral communities, and displaced practitioners sit near the full-target end: trapped exit, place-bound identity investment in the prior arrangement, and no voice in the decree's design. This is the reading's core structural asymmetry — the state benefits from an arrangement it built and enforces, with no grassroots demonstration phase to offset the extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (fragmented governance impeding a stated transformation goal) may remain partly live at the center while having already been resolved or superseded at the periphery through informal re-accommodation — the classic mandatrophy pattern where the state's justification outlives the local reality it was meant to address. The contested founding_problem_status captures this: the state can correctly claim the transformation imperative persists nationally even as, locally, communities have long since either absorbed or quietly reverted around the decree, making continued enforcement partly performative (rising theater_ratio) rather than functionally necessary.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    installation_sufficiency_vs_hybrid_stabilization,
    'Does top-down decree installation alone sustain the new commitment''s legitimacy over the long run, or does the arrangement in fact require a subsequent fringe-validation phase to stabilize, making this reading empirically indistinguishable from the hybrid_cascade_reading after enough time passes?',
    'Longitudinal archival tracing of enforcement intensity and compliance drift: if suppression_requirement can be reduced without loss of adoption once local re-legitimation occurs, the case is better described by hybrid_cascade; if suppression must remain high indefinitely to sustain compliance, exogenous_imposition holds as the accurate reading.',
    'If hybrid stabilization is empirically required, this story''s claimed_type and its high sustained suppression trajectory would need reclassification toward the hybrid reading rather than standing as a distinct constraint — the two readings would collapse into one with a two-phase structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(installation_sufficiency_vs_hybrid_stabilization, empirical, 'Whether decree-only installation is durable or covertly requires fringe validation, which would blur the boundary with the hybrid_cascade sibling reading.').

omega_variable(
    genuine_transformation_mandate_vs_constructed_pretext,
    'Is the central state apparatus''s transformation mandate a genuine response to a real coordination failure in the prior fragmented order, or is the mandate itself a constructed pretext that manufactures the urgency needed to justify decree-based imposition and bypass negotiated transition?',
    'Comparative analysis of pre-decree governance performance metrics (dispute resolution rates, revenue collection, cross-regional coordination failures) against the state''s own crisis narrative; corroboration from non-state contemporaneous observers (foreign diplomats, independent chroniclers, missionary or trader accounts) about whether the prior arrangement was actually failing at the scale claimed.',
    'If the mandate is substantially manufactured, the coordination-function half of the tangled_rope classification weakens toward snare (extraction dressed as necessary transformation); if the mandate reflects genuine prior dysfunction, the tangled_rope classification (genuine coordination function plus asymmetric extraction) is well-supported.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(genuine_transformation_mandate_vs_constructed_pretext, conceptual, 'Whether the state''s founding-problem narrative reflects genuine prior fragmentation or is constructed to license coercive installation.').

omega_variable(
    kernel_framing_underdetermination,
    'Given that all three kernel readings (endogenous_climb, exogenous_imposition, hybrid_cascade) could in principle describe the same historical episode depending on which archival record is privileged (official decree record vs. periphery petition record vs. later reconciliation record), is the choice of this story''s reading determined by the episode''s actual structure or by which archive the analyst foregrounds?',
    'Triangulate the decree''s enforcement correspondence (does it describe suppression of prior customary function, consistent with exogenous_imposition) against petition/rebellion records (do they show resistance-then-defeat, consistent with exogenous_imposition, or resistance-then-negotiated-accommodation, consistent with hybrid_cascade) and against any evidence of prior institutional-fringe advocacy predating the decree (which would support endogenous_climb instead).',
    'If enforcement correspondence and petition records show sustained coercion with no negotiated accommodation, exogenous_imposition is the well-grounded reading for this episode; if accommodation records dominate, the episode more properly belongs to the hybrid_cascade story instead, and this story should be understood as covering a narrower or different historical case.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_framing_underdetermination, conceptual, 'Whether the choice among the three kernel readings is settled by the episode''s own structure or is an artifact of which archival record is privileged in analysis.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(state_commitment_installation_mechanism__exogenous_imposition_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(stat_tr_t0, state_commitment_installation_mechanism__exogenous_imposition_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement_basis(stat_tr_t0, observed).
narrative_ontology:measurement(stat_tr_t8, state_commitment_installation_mechanism__exogenous_imposition_reading, theater_ratio, 8, 0.28).
narrative_ontology:measurement_basis(stat_tr_t8, observed).
narrative_ontology:measurement(stat_tr_t16, state_commitment_installation_mechanism__exogenous_imposition_reading, theater_ratio, 16, 0.33).
narrative_ontology:measurement_basis(stat_tr_t16, observed).
narrative_ontology:measurement(stat_tr_t24, state_commitment_installation_mechanism__exogenous_imposition_reading, theater_ratio, 24, 0.37).
narrative_ontology:measurement_basis(stat_tr_t24, observed).
narrative_ontology:measurement(stat_tr_t32, state_commitment_installation_mechanism__exogenous_imposition_reading, theater_ratio, 32, 0.4).
narrative_ontology:measurement_basis(stat_tr_t32, observed).
narrative_ontology:measurement(stat_tr_t40, state_commitment_installation_mechanism__exogenous_imposition_reading, theater_ratio, 40, 0.42).
narrative_ontology:measurement_basis(stat_tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(stat_be_t0, state_commitment_installation_mechanism__exogenous_imposition_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement_basis(stat_be_t0, observed).
narrative_ontology:measurement(stat_be_t8, state_commitment_installation_mechanism__exogenous_imposition_reading, base_extractiveness, 8, 0.6).
narrative_ontology:measurement_basis(stat_be_t8, observed).
narrative_ontology:measurement(stat_be_t16, state_commitment_installation_mechanism__exogenous_imposition_reading, base_extractiveness, 16, 0.63).
narrative_ontology:measurement_basis(stat_be_t16, observed).
narrative_ontology:measurement(stat_be_t24, state_commitment_installation_mechanism__exogenous_imposition_reading, base_extractiveness, 24, 0.65).
narrative_ontology:measurement_basis(stat_be_t24, observed).
narrative_ontology:measurement(stat_be_t32, state_commitment_installation_mechanism__exogenous_imposition_reading, base_extractiveness, 32, 0.67).
narrative_ontology:measurement_basis(stat_be_t32, observed).
narrative_ontology:measurement(stat_be_t40, state_commitment_installation_mechanism__exogenous_imposition_reading, base_extractiveness, 40, 0.68).
narrative_ontology:measurement_basis(stat_be_t40, observed).

% Suppression requirement over time
narrative_ontology:measurement(stat_su_t0, state_commitment_installation_mechanism__exogenous_imposition_reading, suppression_requirement, 0, 0.7).
narrative_ontology:measurement_basis(stat_su_t0, observed).
narrative_ontology:measurement(stat_su_t8, state_commitment_installation_mechanism__exogenous_imposition_reading, suppression_requirement, 8, 0.78).
narrative_ontology:measurement_basis(stat_su_t8, observed).
narrative_ontology:measurement(stat_su_t16, state_commitment_installation_mechanism__exogenous_imposition_reading, suppression_requirement, 16, 0.82).
narrative_ontology:measurement_basis(stat_su_t16, observed).
narrative_ontology:measurement(stat_su_t24, state_commitment_installation_mechanism__exogenous_imposition_reading, suppression_requirement, 24, 0.8).
narrative_ontology:measurement_basis(stat_su_t24, observed).
narrative_ontology:measurement(stat_su_t32, state_commitment_installation_mechanism__exogenous_imposition_reading, suppression_requirement, 32, 0.79).
narrative_ontology:measurement_basis(stat_su_t32, observed).
narrative_ontology:measurement(stat_su_t40, state_commitment_installation_mechanism__exogenous_imposition_reading, suppression_requirement, 40, 0.79).
narrative_ontology:measurement_basis(stat_su_t40, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(state_commitment_installation_mechanism__exogenous_imposition_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(state_commitment_installation_mechanism__exogenous_imposition_reading, 0.12).
narrative_ontology:affects_constraint(state_commitment_installation_mechanism__exogenous_imposition_reading, endogenous_climb_reading).
narrative_ontology:affects_constraint(state_commitment_installation_mechanism__exogenous_imposition_reading, hybrid_cascade_reading).

% DUAL FORMULATION NOTE:
% This story is one of three constraints decomposed from the natural-language concept 'how new commitments gain legitimacy' (the state_commitment_installation_mechanism kernel). The exogenous_imposition_reading (this file) authors high extraction and high, sustained suppression because its core structural claim is that legitimacy is asserted and enforced from the apex without grassroots demonstration. The endogenous_climb_reading authors low suppression and voluntary uptake (legitimacy earned bottom-up). The hybrid_cascade_reading authors apex-first installation like this reading but adds a required fringe-stabilization phase this reading's axioms explicitly deny is structurally necessary. All three share the kernel but diverge in beneficiary structure, enforcement requirement, and resistance pattern — they are linked here rather than merged because their epsilon values and stakeholder structures are genuinely distinct.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
