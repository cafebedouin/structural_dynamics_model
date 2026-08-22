% ============================================================================
% CONSTRAINT STORY: legitimacy_of_imposed_practice__exogenous_override_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_legitimacy_of_imposed_practice__exogenous_override_reading, []).

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
 *   constraint_id: legitimacy_of_imposed_practice__exogenous_override_reading
 *   human_readable: State Decree Authority Displaces Prior Practice (Exogenous Override Reading)
 *   domain: political_history/state_formation
 *
 * SUMMARY:
 *   This constraint instantiates the exogenous override reading of the
 *   contested kernel 'legitimacy of imposed practice.' The reading asserts
 *   that state legal authority is sufficient to displace prior practice
 *   through decree alone, regardless of whether rural populations internalize
 *   the new norm — compliance follows from mandate and enforcement, not from
 *   bottom-up adoption. The state claims its authority to declare the new
 *   practice legitimate is intrinsic to its legal power; rural populations
 *   experience the decree as external imposition; and a gap persists between
 *   observable compliance (public performance) and internalized practice
 *   (what people actually do when enforcement is absent). This story
 *   generates one ε for the standing arrangement the reading describes:
 *   state-imposed practice displacement via decree, assessed from the
 *   reading's own structural premises.
 *
 * KEY AGENTS:
 *   - state_modernization_apparatus (institutional, agenda-setter): issues and enforces the decree; claims legal authority suffices to displace practice
 *   - rural_populations (powerless, payer): bear compliance costs; experience decree as external imposition; largely fail to internalize
 *   - traditional_practice_communities (moderate, identity-locked payer/beneficiary): custodians of prior practice; resist through ritual concealment and selective compliance
 *   - urban_modernizing_elite (powerful, beneficiary): already adopted the new practice; benefit from decree legitimizing their preferences
 *   - enforcement_apparatus (organized, agenda-setter/payer): police compliance; face pressure to show results; enforcement becomes theatricalized
 *   - rival_legitimacy_sources (moderate, excluded): religious and customary authorities structurally excluded from authority to govern practice change
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(legitimacy_of_imposed_practice__exogenous_override_reading, 0.68).
domain_priors:suppression_score(legitimacy_of_imposed_practice__exogenous_override_reading, 0.79).
domain_priors:theater_ratio(legitimacy_of_imposed_practice__exogenous_override_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(legitimacy_of_imposed_practice__exogenous_override_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(legitimacy_of_imposed_practice__exogenous_override_reading, suppression_requirement, 0.79).
narrative_ontology:constraint_metric(legitimacy_of_imposed_practice__exogenous_override_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(legitimacy_of_imposed_practice__exogenous_override_reading, accessibility_collapse, 0.52).
narrative_ontology:constraint_metric(legitimacy_of_imposed_practice__exogenous_override_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(legitimacy_of_imposed_practice__exogenous_override_reading, tangled_rope).
narrative_ontology:human_readable(legitimacy_of_imposed_practice__exogenous_override_reading, "State Decree Authority Displaces Prior Practice (Exogenous Override Reading)").
narrative_ontology:topic_domain(legitimacy_of_imposed_practice__exogenous_override_reading, "political_history/state_formation").

domain_priors:requires_active_enforcement(legitimacy_of_imposed_practice__exogenous_override_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(legitimacy_of_imposed_practice__exogenous_override_reading, '074a6d91-26be-423d-b9df-dcb4eba321dc').
narrative_ontology:cs_kernel_codification('074a6d91-26be-423d-b9df-dcb4eba321dc', formalized).
narrative_ontology:cs_authority_grounding('074a6d91-26be-423d-b9df-dcb4eba321dc', extraction).
narrative_ontology:cs_interpretation_layer_present('074a6d91-26be-423d-b9df-dcb4eba321dc').
narrative_ontology:cs_reading_relation('074a6d91-26be-423d-b9df-dcb4eba321dc', legitimacy_of_imposed_practice__endogenous_climb_reading, forecloses).
narrative_ontology:cs_reading_relation('074a6d91-26be-423d-b9df-dcb4eba321dc', legitimacy_of_imposed_practice__hybrid_scaffolding_reading, influences).
narrative_ontology:cs_axiom('074a6d91-26be-423d-b9df-dcb4eba321dc', foundational, legal_decree_displaces_practice_without_internalization).
narrative_ontology:cs_axiom_status(legal_decree_displaces_practice_without_internalization, holdable).
narrative_ontology:cs_axiom_grounding('074a6d91-26be-423d-b9df-dcb4eba321dc', legal_decree_displaces_practice_without_internalization, empirically_contingent).
narrative_ontology:cs_axiom('074a6d91-26be-423d-b9df-dcb4eba321dc', foundational, state_authority_is_intrinsically_self_legitimizing).
narrative_ontology:cs_axiom_status(state_authority_is_intrinsically_self_legitimizing, holdable).
narrative_ontology:cs_axiom_grounding('074a6d91-26be-423d-b9df-dcb4eba321dc', state_authority_is_intrinsically_self_legitimizing, conventional).
narrative_ontology:cs_reference_frame('074a6d91-26be-423d-b9df-dcb4eba321dc', state_unilateral_authority_to_reshape_practice).
narrative_ontology:cs_drift_state('074a6d91-26be-423d-b9df-dcb4eba321dc', contemporary_post_enforcement_normalization, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('074a6d91-26be-423d-b9df-dcb4eba321dc', '').
narrative_ontology:cs_kernel_id(legitimacy_of_imposed_practice__exogenous_override_reading, legitimacy_of_imposed_practice).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(legitimacy_of_imposed_practice__exogenous_override_reading, state_modernization_apparatus).
narrative_ontology:constraint_victim(legitimacy_of_imposed_practice__exogenous_override_reading, rural_populations).
narrative_ontology:constraint_victim(legitimacy_of_imposed_practice__exogenous_override_reading, traditional_practice_communities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(legitimacy_of_imposed_practice__exogenous_override_reading, traditional_practice_communities).
narrative_ontology:constraint_beneficiary(legitimacy_of_imposed_practice__exogenous_override_reading, urban_modernizing_elite).
narrative_ontology:constraint_victim(legitimacy_of_imposed_practice__exogenous_override_reading, enforcement_apparatus).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Issues and enforces the legal decree mandating the new practice (calendar reform, dress codes, language requirements, or similar). Justifies the decree as modernization, national unity, or hygiene/efficiency. Deploys administrative capacity to audit compliance and punish non-compliance. Claims legal authority suffices to displace prior practice through sheer mandate force, independent of whether rural populations internalize the new norm.
narrative_ontology:constraint_stakeholder(legitimacy_of_imposed_practice__exogenous_override_reading, state_modernization_apparatus, agenda_setter,
    institutional, generational, arbitrage, national).

% Bear the immediate compliance costs: retraining, material investment (new clothes, calendars), social friction within communities that resist the shift. They were not consulted on the decree and experience it as external imposition. Exit is not available — they cannot leave the jurisdiction or maintain the old practice openly without penalties. They internalize the new practice only under duress, and many sustain the old practice in private or in ritual contexts where enforcement is weaker.
narrative_ontology:constraint_stakeholder(legitimacy_of_imposed_practice__exogenous_override_reading, rural_populations, payer,
    powerless, biographical, trapped, local).

% Organized custodians of prior practice — religious authorities, village elders, craft guilds, or cultural specialists who derive authority and identity from transmitting the old ways. They experience the decree as delegitimizing their authority and threatening their social role. They resist through ritual concealment, selective compliance, or reinterpreting the new practice to embed old meanings. They are not victims of the constraint alone — they also benefit from maintaining community cohesion through continuity — but the decree forces them to either abandon identity or face punishment.
narrative_ontology:constraint_stakeholder(legitimacy_of_imposed_practice__exogenous_override_reading, traditional_practice_communities, payer,
    moderate, generational, identity_locked, local).
narrative_ontology:stakeholder_secondary_role(legitimacy_of_imposed_practice__exogenous_override_reading, traditional_practice_communities, beneficiary).

% Aligned with the state's modernization agenda for reasons of efficiency, cosmopolitanism, or material advantage. They adopt the new practice readily and often enforce it socially within their networks. They experience the decree as legitimizing their already-preferred way of living and marginalizing traditional practice as backwards. They have exit options (migration, career flexibility) that shield them from the compliance costs borne by rural populations.
narrative_ontology:constraint_stakeholder(legitimacy_of_imposed_practice__exogenous_override_reading, urban_modernizing_elite, beneficiary,
    powerful, biographical, mobile, national).

% Police, inspectors, judges, and administrators who carry out the decree's enforcement. They invest considerable labor in detecting non-compliance, auditing practice, and punishing violations. Over time, enforcement becomes routinized and can itself become performative — the visible exercise of state power matters as much as actual compliance. They face pressure to show results and may fabricate or exaggerate compliance levels to justify continued enforcement budgets.
narrative_ontology:constraint_stakeholder(legitimacy_of_imposed_practice__exogenous_override_reading, enforcement_apparatus, agenda_setter,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(legitimacy_of_imposed_practice__exogenous_override_reading, enforcement_apparatus, payer).

% Religious authorities, customary law keepers, and cultural authorities whose legitimacy derives from sources other than the state (divine revelation, ancestral mandate, community consensus). The decree structurally excludes them from the authority to govern practice change. They would argue that legitimate practice displacement requires their buy-in or at least accommodation of hybrid forms, but the decree treats their authority as superseded. Their exclusion is what the enforcement machinery exists to maintain.
narrative_ontology:constraint_stakeholder(legitimacy_of_imposed_practice__exogenous_override_reading, rival_legitimacy_sources, excluded,
    moderate, generational, identity_locked, local).

% Historians, anthropologists, and political theorists who observe the constraint from outside any participating seat. They analyze whether the decree actually achieved displacement (calendar adoption vs. continued lunar reckoning privately; dress codes donned in public but removed at home) and measure the gap between compliance theater and internalized practice.
narrative_ontology:constraint_stakeholder(legitimacy_of_imposed_practice__exogenous_override_reading, comparative_observers, observer,
    analytical, generational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(legitimacy_of_imposed_practice__exogenous_override_reading, state_modernization_apparatus).
narrative_ontology:fixing_cost_class(legitimacy_of_imposed_practice__exogenous_override_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: State-level synchronization and identity: a single legal standard (calendar, dress, language) enables administrative coordination across diverse regions and signals national unity. The decree solves the coordination problem of aligning dispersed practices without negotiating with every local authority.
% TRANSFER_FUNCTION: Moves legitimacy from traditional practice sources (religion, custom, community consensus) to state-declared legal authority. Transfers the cost of adjustment (retraining, material investment, social friction) from the urban modernizing elite (who were already adopting the new practice) to rural populations (who bear compliance costs involuntarily). Transfers the burden of legitimacy-maintenance from the state (which simply decrees) to enforcement apparatus (which must police the gap between decree and actual practice).
% ABSENT_VOICES: Rival legitimacy sources (religious authorities, customary law keepers) are structurally excluded from the conversation about whether the decree is legitimate or necessary. They would argue that imposed practice change without internalization creates only theatrical compliance, not real displacement. Rural populations themselves are not consulted — the decree is handed down, not negotiated.
% DISAPPEARANCE_RATIONALE: If the decree vanished and enforcement ceased, most rural populations would revert to or continue practicing the old ways in full — not because they secretly preferred them the whole time, but because many never internalized the new practice beyond the surface level required to avoid punishment. The state would lose a tool for imposing its modernization agenda without local consent. Urban modernizers would continue the new practice (they already adopted it voluntarily); rural communities would reassert traditional practice where enforcement weakness permitted.
% FOUNDING_PROBLEM: Territorial unification and administrative coherence: a newly unified or modernizing nation-state sought to eliminate regional practice variation that complicated governance, taxation, communication, or military organization.
% FOUNDING_PROBLEM_CORROBORATION: State modernization apparatus attests the founding problem is live and ongoing — coordination across practice diversity remains a governance challenge. Historians and anthropologists from outside the state apparatus attest that the founding problem WAS real at the moment of decree but has since been substantially solved by natural economic and communication integration; the decree's PERSISTENCE is driven by state identity-consolidation and enforcement bureaucracy expansion, not ongoing problem-solving. Rural populations attest they never felt the founding problem as their problem — it was the state's problem with their diversity, not a coordination failure they faced.
narrative_ontology:disappearance_verdict(legitimacy_of_imposed_practice__exogenous_override_reading, world_rearranges).
narrative_ontology:founding_problem_status(legitimacy_of_imposed_practice__exogenous_override_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(legitimacy_of_imposed_practice__exogenous_override_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(legitimacy_of_imposed_practice__exogenous_override_reading, 'none', 1).
narrative_ontology:epsilon_provenance(legitimacy_of_imposed_practice__exogenous_override_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(legitimacy_of_imposed_practice__exogenous_override_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(legitimacy_of_imposed_practice__exogenous_override_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(legitimacy_of_imposed_practice__exogenous_override_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.68 terminal) because the decree transfers legitimacy and compliance burden to rural populations without their consent and without compensating them for adjustment costs. The constraint is tangled — it coordinates state identity and administration (real coordination function) while simultaneously extracting from those who bear adjustment costs without benefiting from the coordination itself. Suppression is high (0.79 terminal) because compliance depends on coercive enforcement, not on participants' voluntary preference. The measurement series show extraction and suppression rising steeply in the first 16 time points (the enforcement infrastructure is built and hardened) then plateauing (enforcement becomes routinized; resistance persists but stabilizes). Theater rises throughout: early enforcement targets actual practice displacement; later enforcement becomes performance of state authority, with diminishing returns on actual behavioral change — enforcement activity persists primarily to maintain bureaucratic legitimacy, not to achieve further displacement. One shared grid (every metric at every time point) ensures the measurements align: all three series trace the same 6-point temporal arc.
 *
 * PERSPECTIVAL GAP:
 *   The state modernization apparatus and rural populations compute radically different types from identical structural data. The apparatus computes the constraint as rope-like (coordination + consent of the powerful) or even mountain-like (decree is natural fact of political reality). Rural populations compute it as snare (extraction, identity-lock, enforcement without exit). The engine's per-seat computation surfaces this divergence: the agenda-setter sees coordination and legitimate authority; the payer sees coercion and extraction. This divergence is NOT an error — it is the measurement the framework exists to take. The exogenous override reading builds this divergence into its premise: it asserts the state's legal authority as sufficient, which is exactly what the state apparatus believes but rural seats reject.
 *
 * DIRECTIONALITY LOGIC:
 *   The state modernization apparatus sits near the beneficiary end (d ≈ 0.15): it declares authority and enforces, collecting legitimacy gains (the decree establishes state power to reshape society). Rural populations sit near the target end (d ≈ 0.85): they bear compliance costs involuntarily, lack exit options, and have not internalized the new practice — their situation is extractive imposition. Traditional practice communities are identity-locked targets (d ≈ 0.80): their exit would require abandoning identity, so they are trapped. Urban modernizers sit near the beneficiary end (d ≈ 0.20): they already adopted and benefit from decree legitimation. Enforcement apparatus sits near symmetric (d ≈ 0.55): they benefit from budget expansion and bureaucratic growth, but pay in labor and are constrained by pressure to show compliance results. Rival legitimacy sources are excluded rather than positioned: their d is not computed because they are structurally prevented from participating. The perspectival gap is large: from the state's position, the exogenous override reading is true (legal authority suffices); from rural seats, it is manifestly false (people are performing compliance without internalization).
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint carries potential mandatrophy: the founding problem (territorial unification, administrative coherence) may be substantially solved by time 25–35 (economic integration, communication technology, generations of bilinguals/bicultural participation solve much of the coordination problem without the decree), yet the decree persists and enforcement expands (theater rising to 0.41 by endpoint). The rising theater ratio is the diagnostic signal: enforcement activity increasingly serves to maintain bureaucratic legitimacy and state identity-consolidation rather than to solve the original coordination problem. This is classic mandatrophy — a constraint outlives its founding problem. The exogenous override reading, however, reframes mandatrophy: if the founding problem is truly 'state authority to reshape society' (rather than 'practical coordination'), then the founding problem is live as long as state legitimacy requires visible power to transform practice. Under this reframing, mandatrophy is averted: the state's mandate is perpetual authority maintenance, not temporal problem-solving. This reframing is exactly what the exogenous override reading licenses — it lets the constraint call its own survival 'success.' A competing reading would reject this reframing and diagnose mandatrophy straightforwardly.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    suppression_internalization_boundary,
    'To what extent has the rural population internalized the new practice (experiencing it as legitimate) versus merely performing compliance under duress (experiencing it as external imposition)?',
    'Post-enforcement natural experiment: when state enforcement weakens or is removed, do populations continue the new practice voluntarily or revert? Multi-generational tracking of private vs. public practice divergence.',
    'If suppression is largely structural (external penalties), the constraint depends on active enforcement and would weaken or reverse if enforcement ceased — supporting the exogenous override reading''s claim that decree suffices (with enforcement cost). If suppression is internalized (people have come to believe the new practice is legitimate), the constraint has achieved deeper displacement — supporting the endogenous climb reading''s claim that only internalization matters.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_internalization_boundary, empirical, 'Boundary between structural suppression (external penalties) and internalized suppression (belief in legitimacy)').

omega_variable(
    founding_problem_temporal_status,
    'Does the founding problem (territorial administrative coherence via practice uniformity) remain live, or has it been substantially solved by economic integration and communication technology, leaving the decree as mandatrophic persistence?',
    'Counterfactual administrative performance analysis: if the decree were removed but economic/communication integration remained, would state administrative function degrade materially? Comparative analysis with jurisdictions that never imposed the decree.',
    'If the founding problem is dead (solved by other means), the constraint has passed mandatrophy threshold and the exogenous override reading''s legitimacy claim (state authority sustains itself) becomes the entire justification — the constraint is now pure extraction. If the founding problem is live, the tangled rope framing holds: the decree coordinates state administration while extracting from rural populations.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(founding_problem_temporal_status, empirical, 'Whether state-decree practice uniformity remains functionally necessary for governance').

omega_variable(
    reading_foreclosure_structure,
    'Does the exogenous override reading''s core claim (legal decree suffices regardless of internalization) logically foreclose the endogenous climb reading (internalization is necessary), or do they coexist as live positions held by different parties?',
    'Logical analysis: if the endogenous climb reading holds that internalization is necessary, and the exogenous override reading holds that mandate is sufficient, can both be true in the same framework? No — one is a logical negation of the other. But do they hold in different frameworks (state authority framework vs. practice legitimacy framework)? Yes — they can coexist as different commitments to different authority sources.',
    'If the readings foreclose each other (one is true, one is false), the framework must arbitrate which source of authority — legal decree or community internalization — actually determines legitimate practice change. If the readings coexist (different parties'' different frameworks), the framework preserves both as live, and the constraint itself is the site of the contest between them. This schema decision routes to the reading_relations declaration: ''forecloses'' vs. ''coexists_with''.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_foreclosure_structure, conceptual, 'Logical and framework relationship between exogenous override and endogenous climb readings').

omega_variable(
    state_authority_legitimacy_source,
    'What grounds the state''s authority to declare and enforce practice displacement: legal positivism (state''s own declared authority is self-legitimizing), natural law (state authority derives from some external source), or pragmatic efficacy (state authority is legitimate if it works)?',
    'Philosophical analysis of the regime''s own legitimacy claims in founding documents, judicial opinions, and administrative justifications. Comparison across regimes with different legitimacy frameworks.',
    'Different legitimacy sources produce different assessments of the exogenous override reading: if positivism (state authority is intrinsic), the reading is sound. If natural law (authority must derive from external source), the reading is incomplete — decree alone is insufficient. If efficacy-based (authority is justified by results), the reading fails where the decree fails to achieve actual displacement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(state_authority_legitimacy_source, conceptual, 'The epistemic and normative source of state authority to impose practice displacement').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(legitimacy_of_imposed_practice__exogenous_override_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(legi_tr_t0, legitimacy_of_imposed_practice__exogenous_override_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(legi_tr_t8, legitimacy_of_imposed_practice__exogenous_override_reading, theater_ratio, 8, 0.28).
narrative_ontology:measurement(legi_tr_t16, legitimacy_of_imposed_practice__exogenous_override_reading, theater_ratio, 16, 0.32).
narrative_ontology:measurement(legi_tr_t25, legitimacy_of_imposed_practice__exogenous_override_reading, theater_ratio, 25, 0.37).
narrative_ontology:measurement(legi_tr_t35, legitimacy_of_imposed_practice__exogenous_override_reading, theater_ratio, 35, 0.4).
narrative_ontology:measurement(legi_tr_t50, legitimacy_of_imposed_practice__exogenous_override_reading, theater_ratio, 50, 0.41).

% Extraction over time
narrative_ontology:measurement(legi_be_t0, legitimacy_of_imposed_practice__exogenous_override_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(legi_be_t8, legitimacy_of_imposed_practice__exogenous_override_reading, base_extractiveness, 8, 0.51).
narrative_ontology:measurement(legi_be_t16, legitimacy_of_imposed_practice__exogenous_override_reading, base_extractiveness, 16, 0.58).
narrative_ontology:measurement(legi_be_t25, legitimacy_of_imposed_practice__exogenous_override_reading, base_extractiveness, 25, 0.65).
narrative_ontology:measurement(legi_be_t35, legitimacy_of_imposed_practice__exogenous_override_reading, base_extractiveness, 35, 0.67).
narrative_ontology:measurement(legi_be_t50, legitimacy_of_imposed_practice__exogenous_override_reading, base_extractiveness, 50, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(legi_su_t0, legitimacy_of_imposed_practice__exogenous_override_reading, suppression_requirement, 0, 0.68).
narrative_ontology:measurement(legi_su_t8, legitimacy_of_imposed_practice__exogenous_override_reading, suppression_requirement, 8, 0.71).
narrative_ontology:measurement(legi_su_t16, legitimacy_of_imposed_practice__exogenous_override_reading, suppression_requirement, 16, 0.75).
narrative_ontology:measurement(legi_su_t25, legitimacy_of_imposed_practice__exogenous_override_reading, suppression_requirement, 25, 0.78).
narrative_ontology:measurement(legi_su_t35, legitimacy_of_imposed_practice__exogenous_override_reading, suppression_requirement, 35, 0.79).
narrative_ontology:measurement(legi_su_t50, legitimacy_of_imposed_practice__exogenous_override_reading, suppression_requirement, 50, 0.79).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(legitimacy_of_imposed_practice__exogenous_override_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(legitimacy_of_imposed_practice__exogenous_override_reading, 0.12).
narrative_ontology:affects_constraint(legitimacy_of_imposed_practice__exogenous_override_reading, legitimacy_of_imposed_practice__endogenous_climb_reading).
narrative_ontology:affects_constraint(legitimacy_of_imposed_practice__exogenous_override_reading, legitimacy_of_imposed_practice__hybrid_scaffolding_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the contested kernel 'legitimacy of imposed practice.' All three readings share the same referent (state-imposed practice displacement) but disagree on what makes it legitimate and persistent. ε (extractiveness) is reading-indexed: the exogenous override reading assesses the standing decree arrangement from the reading's own lights (legal mandate is sufficient, suppression is structural cost) and authors ε=0.68 for that reading. The endogenous climb reading would author lower ε (assessing the same arrangement as failing to achieve real displacement without internalization). The hybrid scaffolding reading would author moderate ε (assessing the arrangement as partially successful where messaging scaffolds imposition). The three stories are linked by network.affects_constraints in both directions: each sibling influences the others because a shift in one reading's empirical status affects the credibility of the others' framing.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(legitimacy_of_imposed_practice__exogenous_override_reading, moderate, 0.8).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
