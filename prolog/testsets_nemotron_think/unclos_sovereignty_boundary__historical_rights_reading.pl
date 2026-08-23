% ============================================================================
% CONSTRAINT STORY: unclos_sovereignty_boundary__historical_rights_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_unclos_sovereignty_boundary__historical_rights_reading, []).

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
 *   constraint_id: unclos_sovereignty_boundary__historical_rights_reading
 *   human_readable: Historical Rights Override of UNCLOS EEZ Provisions
 *   domain: international_law/maritime_governance/geopolitical_strategy
 *
 * SUMMARY:
 *   This constraint story models the 'historical rights reading' of the
 *   UNCLOS sovereignty boundary kernel — the claim that pre-UNCLOS historical
 *   usage and occupation generate sovereign rights that override the treaty's
 *   200nm EEZ provisions. This is the structural position advanced by
 *   expansive claimant states (most prominently China in the South China Sea,
 *   but also Vietnam, Philippines, and others in varying forms). The
 *   constraint operates as a tangled rope: it performs a genuine coordination
 *   function by providing a recognized vocabulary for managing overlapping
 *   claims that the treaty text left ambiguous, while simultaneously
 *   extracting exclusive resource rights and jurisdictional control from
 *   EEZ-holding coastal states and imposing navigation restrictions on global
 *   shipping. The coordination function is real — without some framework for
 *   historical claims, every overlap would be pure force — but the extraction
 *   is asymmetric and escalating.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(unclos_sovereignty_boundary__historical_rights_reading, 0.68).
domain_priors:suppression_score(unclos_sovereignty_boundary__historical_rights_reading, 0.62).
domain_priors:theater_ratio(unclos_sovereignty_boundary__historical_rights_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(unclos_sovereignty_boundary__historical_rights_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(unclos_sovereignty_boundary__historical_rights_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(unclos_sovereignty_boundary__historical_rights_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(unclos_sovereignty_boundary__historical_rights_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(unclos_sovereignty_boundary__historical_rights_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(unclos_sovereignty_boundary__historical_rights_reading, tangled_rope).
narrative_ontology:human_readable(unclos_sovereignty_boundary__historical_rights_reading, "Historical Rights Override of UNCLOS EEZ Provisions").
narrative_ontology:topic_domain(unclos_sovereignty_boundary__historical_rights_reading, "international_law/maritime_governance/geopolitical_strategy").

domain_priors:requires_active_enforcement(unclos_sovereignty_boundary__historical_rights_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(unclos_sovereignty_boundary__historical_rights_reading, '427dc7b1-0387-4814-b34e-98679856e4bb').
narrative_ontology:cs_kernel_codification('427dc7b1-0387-4814-b34e-98679856e4bb', formalized).
narrative_ontology:cs_authority_grounding('427dc7b1-0387-4814-b34e-98679856e4bb', lineage).
narrative_ontology:cs_interpretation_layer_present('427dc7b1-0387-4814-b34e-98679856e4bb').
narrative_ontology:cs_reading_relation('427dc7b1-0387-4814-b34e-98679856e4bb', unclos_sovereignty_boundary__strict_eez_reading, forecloses).
narrative_ontology:cs_reading_relation('427dc7b1-0387-4814-b34e-98679856e4bb', unclos_sovereignty_boundary__non_ratifier_enforcement_reading, coexists_with).
narrative_ontology:cs_axiom('427dc7b1-0387-4814-b34e-98679856e4bb', foundational, historical_usage_generates_sovereign_title).
narrative_ontology:cs_axiom_status(historical_usage_generates_sovereign_title, holdable).
narrative_ontology:cs_axiom_grounding('427dc7b1-0387-4814-b34e-98679856e4bb', historical_usage_generates_sovereign_title, conventional).
narrative_ontology:cs_axiom('427dc7b1-0387-4814-b34e-98679856e4bb', foundational, historical_rights_override_treaty_eez_provisions).
narrative_ontology:cs_axiom_status(historical_rights_override_treaty_eez_provisions, holdable).
narrative_ontology:cs_axiom_grounding('427dc7b1-0387-4814-b34e-98679856e4bb', historical_rights_override_treaty_eez_provisions, conventional).
narrative_ontology:cs_axiom('427dc7b1-0387-4814-b34e-98679856e4bb', secondary, intertemporal_law_preserves_preexisting_rights).
narrative_ontology:cs_axiom_status(intertemporal_law_preserves_preexisting_rights, holdable).
narrative_ontology:cs_axiom_grounding('427dc7b1-0387-4814-b34e-98679856e4bb', intertemporal_law_preserves_preexisting_rights, conventional).
narrative_ontology:cs_reference_frame('427dc7b1-0387-4814-b34e-98679856e4bb', pre_unclos_customary_maritime_order).
narrative_ontology:cs_drift_state('427dc7b1-0387-4814-b34e-98679856e4bb', post_south_china_sea_arbitration, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('427dc7b1-0387-4814-b34e-98679856e4bb', '').
narrative_ontology:cs_kernel_id(unclos_sovereignty_boundary__historical_rights_reading, unclos_sovereignty_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(unclos_sovereignty_boundary__historical_rights_reading, expansive_claimant_states).
narrative_ontology:constraint_beneficiary(unclos_sovereignty_boundary__historical_rights_reading, historical_narrative_institutions).
narrative_ontology:constraint_victim(unclos_sovereignty_boundary__historical_rights_reading, eez_holding_coastal_states).
narrative_ontology:constraint_victim(unclos_sovereignty_boundary__historical_rights_reading, navigational_actor_states).
narrative_ontology:constraint_victim(unclos_sovereignty_boundary__historical_rights_reading, small_island_developing_states).
narrative_ontology:constraint_vindicates(unclos_sovereignty_boundary__historical_rights_reading, historical_usage_generates_sovereign_title).
narrative_ontology:constraint_vindicates(unclos_sovereignty_boundary__historical_rights_reading, treaty_law_does_not_extinguish_preexisting_rights).
narrative_ontology:constraint_vindicates(unclos_sovereignty_boundary__historical_rights_reading, intertemporal_law_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Assert sovereignty over maritime areas exceeding UNCLOS EEZ limits based on historical usage, occupation, and administration. Deploy coast guard, maritime militia, and administrative apparatus to enforce claims. Benefit from extended resource rights (fisheries, hydrocarbons), strategic depth, and nationalist legitimacy. Can shift between legal forums (ITLOS, bilateral talks, unilateral action) to advance claims.
narrative_ontology:constraint_stakeholder(unclos_sovereignty_boundary__historical_rights_reading, expansive_claimant_states, agenda_setter,
    institutional, generational, arbitrage, regional).
narrative_ontology:stakeholder_secondary_role(unclos_sovereignty_boundary__historical_rights_reading, expansive_claimant_states, beneficiary).

% Hold UNCLOS-based EEZ entitlements that overlap with expansive historical claims. Lose exclusive control over resources and jurisdiction in overlapped areas. Must invest in maritime domain awareness, coast guard capacity, and legal defense. Exit options limited: can litigate (slow, enforcement uncertain), build alliances (costly), or accommodate (loss of sovereignty).
narrative_ontology:constraint_stakeholder(unclos_sovereignty_boundary__historical_rights_reading, eez_holding_coastal_states, payer,
    institutional, generational, constrained, regional).

% Commercial shipping and naval powers requiring stable, predictable maritime passage. Face increased constraint from historical claims that restrict navigation rights (e.g., prior notification, innocent passage limitations in claimed 'historic waters'). Can reroute at cost, but global trade chokepoints limit true exit. Bear compliance costs and risk of confrontation.
narrative_ontology:constraint_stakeholder(unclos_sovereignty_boundary__historical_rights_reading, navigational_actor_states, payer,
    powerful, biographical, mobile, global).

% Depend entirely on UNCLOS EEZ for economic survival (tuna fisheries, seabed minerals). Historical claims from larger states can subsume their entire maritime entitlement. Lack naval capacity, legal resources, and diplomatic leverage to resist. Structurally excluded from great-power negotiations over 'historic rights' frameworks.
narrative_ontology:constraint_stakeholder(unclos_sovereignty_boundary__historical_rights_reading, small_island_developing_states, payer,
    powerless, generational, trapped, regional).
narrative_ontology:stakeholder_secondary_role(unclos_sovereignty_boundary__historical_rights_reading, small_island_developing_states, excluded).

% ITLOS, ICJ, and arbitral tribunals adjudicating disputes between historical rights claims and UNCLOS EEZ entitlements. Their jurisprudence (e.g., South China Sea Arbitration, Chagos) shapes the constraint's enforcement trajectory. Do not collect rents but their interpretive authority determines whether the constraint hardens or yields.
narrative_ontology:constraint_stakeholder(unclos_sovereignty_boundary__historical_rights_reading, international_tribunals, observer,
    institutional, generational, analytical, universal).

% Academic institutes, think tanks, and state-backed research bodies producing historical evidence (maps, voyages, administrative records) to substantiate expansive claims. Receive state funding and prestige; their professional identity is fused to the validity of the historical rights framework. Exit would require repudiating their life's work and institutional mission.
narrative_ontology:constraint_stakeholder(unclos_sovereignty_boundary__historical_rights_reading, historical_narrative_institutions, beneficiary,
    organized, civilizational, identity_locked, regional).

% Traditional fishing communities with pre-state usage patterns that neither UNCLOS nor state-centric historical rights frameworks adequately capture. Their customary practices are cited by claimant states as evidence but they have no standing in inter-state proceedings. Would object to both state-centric frameworks if given voice.
narrative_ontology:constraint_stakeholder(unclos_sovereignty_boundary__historical_rights_reading, indigenous_coastal_communities, excluded,
    powerless, biographical, identity_locked, local).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Manages overlapping maritime sovereignty claims where pre-UNCLOS historical practice and the UNCLOS EEZ framework generate irreducible conflict; provides a vocabulary for states to articulate claims without immediate resort to force.
% TRANSFER_FUNCTION: Moves exclusive resource rights (fisheries, hydrocarbons, seabed minerals) and jurisdictional control (enforcement, regulation, military access) from EEZ-holding coastal states to expansive claimant states, justified by historical usage narratives that predate the treaty regime.
% ABSENT_VOICES: Indigenous and traditional fishing communities whose usage patterns are instrumentalized as evidence but who have no standing in dispute resolution; small island developing states whose entire EEZ entitlements can be subsumed; non-regional commercial shipping interests bearing compliance costs without representation.
% DISAPPEARANCE_RATIONALE: If historical rights claims vanished overnight, UNCLOS EEZ boundaries would govern exclusively — overlapped areas would revert to the EEZ-holding coastal state, navigational rights would stabilize per UNCLOS Part V, and the legal basis for current enforcement actions (coast guard patrols, administrative regulations in disputed zones) would evaporate. The South China Sea, East China Sea, and Arctic disputes would restructure around treaty-based entitlements.
% FOUNDING_PROBLEM: UNCLOS (1982) established the 200nm EEZ regime without fully resolving pre-existing historical claims, traditional fishing rights, and 'historic waters' assertions — creating a structural gap where states with pre-UNCLOS maritime practices could not reconcile their claims with the new treaty framework.
% FOUNDING_PROBLEM_CORROBORATION: Legal scholars outside claimant states (Rothwell, Oude Elferink, Klein) attest the founding problem is real — UNCLOS Article 10 (bays), Article 15 (historic titles), and Article 298(1)(a)(i) (dispute exclusion) reflect the drafters' awareness but leave the content of 'historic rights' undefined. Claimant states argue the problem remains live; non-claimant states and tribunals (South China Sea Arbitration) argue UNCLOS extinguished or strictly limited such claims.
narrative_ontology:disappearance_verdict(unclos_sovereignty_boundary__historical_rights_reading, world_rearranges).
narrative_ontology:founding_problem_status(unclos_sovereignty_boundary__historical_rights_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(unclos_sovereignty_boundary__historical_rights_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(unclos_sovereignty_boundary__historical_rights_reading, 'none', 1).
narrative_ontology:epsilon_provenance(unclos_sovereignty_boundary__historical_rights_reading, 0.68, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(unclos_sovereignty_boundary__historical_rights_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(unclos_sovereignty_boundary__historical_rights_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(unclos_sovereignty_boundary__historical_rights_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68) reflects the substantial transfer of resource rights and jurisdictional authority from EEZ-holding states to claimants. Suppression (0.62) captures the active enforcement required: coast guard patrols, maritime militia presence, administrative regulations, and the exclusion of rival claimants' activities. Theater ratio (0.38) is moderate — the historical research apparatus and legal argumentation are genuine but an increasing share of activity serves to legitimize extraction rather than resolve coordination. Accessibility collapse (0.55) is partial: alternatives (UNCLOS dispute settlement, bilateral joint development, code of conduct negotiations) exist but are structurally constrained by the claimants' refusal to accept them as exclusive. Resistance (0.71) is high: legal challenges, freedom of navigation operations, alliance-building, and domestic political pushback in victim states.
 *
 * PERSPECTIVAL GAP:
 *   From the claimant seat, this is a rope: a necessary coordination mechanism for historical justice and legal continuity. From the EEZ-holding coastal state seat, it is a snare: extraction disguised as history. From the navigational actor seat, it is a tangled rope: some coordination value (predictable claim-making) but increasing extraction (passage restrictions). The engine computes this divergence from the structural data — the claimed_type (tangled_rope) reflects this author's structural assessment, not any single seat's experience.
 *
 * DIRECTIONALITY LOGIC:
 *   Expansive claimant states are structural beneficiaries (d ~ 0.15): they set the agenda, collect the resource rents, and control the enforcement timeline. EEZ-holding coastal states are full targets (d ~ 0.85): they bear the costs of lost resources, enforcement burden, and sovereignty erosion with constrained exit. Navigational actor states are payers (d ~ 0.65): they face compliance costs and route disruptions but retain global rerouting options. Small island developing states are trapped payers (d ~ 0.9): no naval capacity, no legal leverage, total dependence on the EEZ regime. Historical narrative institutions are identity-locked beneficiaries (d ~ 0.2): their professional existence depends on the framework's validity. International tribunals are analytical observers (d = 0.5). Indigenous communities are identity-locked excluded (d ~ 0.8): their practices are cited but they have no standing.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's founding problem (UNCLOS's incomplete resolution of pre-existing claims) remains contested — not dead. The arrangement has not atrophied into a piton; enforcement is intensifying (suppression rising from 0.40 to 0.62 over 42 years). However, the coordination function is degrading: theater ratio rising from 0.15 to 0.38 indicates more performative legal/historical activity per unit of actual dispute resolution. If the founding problem were universally acknowledged as dead (UNCLOS fully supersedes), this would be a piton. The contested status keeps it in tangled_rope territory.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_ambiguity,
    'Does the ''historical rights'' reading instantiate a distinct constraint from the ''strict EEZ'' reading, or are they competing interpretations of a single constraint?',
    'Compare ε values: if historical rights claims extract substantially from EEZ-holding states while strict EEZ reading extracts near-zero, they are distinct constraints per ε-invariance. Track whether tribunals treat them as mutually exclusive legal regimes.',
    'If distinct constraints, each gets independent classification; if single constraint with observer-dependent ε, the framework''s core invariance principle is violated. Current evidence (South China Sea Arbitration treating them as incompatible) supports distinct constraints.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_ambiguity, conceptual, 'Whether kernel readings map to distinct constraints per ε-invariance').

omega_variable(
    historical_evidence_threshold,
    'What quantum and quality of historical evidence suffices to generate sovereign rights overriding UNCLOS EEZ?',
    'Tribunal jurisprudence (South China Sea, Chagos, Qatar v. Bahrain) establishing evidentiary standards; state practice convergence on threshold.',
    'Low threshold → more states qualify as beneficiaries, extraction spreads. High threshold → only few claimants benefit, constraint narrows. Current jurisprudence suggests high threshold (effective administration, not mere usage).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(historical_evidence_threshold, empirical, 'Evidentiary standard for historical rights to override treaty EEZ').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression exerted by historical rights claims primarily structural (naval/coast guard enforcement, administrative regulations) or internalized (coastal states self-censoring resource development, shipping lines pre-complying)?',
    'Post-arbitral award behavior: if coastal states resume activity in awarded areas without physical enforcement change, suppression was partly internalized. Track investment decisions in overlapped zones.',
    'If substantially internalized, effective suppression exceeds structural measure — targets carry the constraint after legal victories. Would raise χ for payer seats beyond engine''s structural computation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression in maritime disputes').

omega_variable(
    navigation_freedom_interaction,
    'How does this reading structurally interact with the non_ratifier_enforcement_reading (customary FoN enforcement)?',
    'Analyze state practice: do claimant states asserting historical rights also restrict FoN in claimed ''historic waters''? Do FoN-enforcing states treat historical rights as valid limitations?',
    'If historical rights claims systematically restrict FoN, the two readings are in structural tension (influences/forecloses). If they operate in separate domains (resource rights vs. passage), they coexist.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(navigation_freedom_interaction, conceptual, 'Structural interaction between historical rights and customary FoN enforcement').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(unclos_sovereignty_boundary__historical_rights_reading, 0, 42).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(unclos_hist_rights_tr_t0, unclos_sovereignty_boundary__historical_rights_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(unclos_hist_rights_tr_t7, unclos_sovereignty_boundary__historical_rights_reading, theater_ratio, 7, 0.2).
narrative_ontology:measurement(unclos_hist_rights_tr_t14, unclos_sovereignty_boundary__historical_rights_reading, theater_ratio, 14, 0.25).
narrative_ontology:measurement(unclos_hist_rights_tr_t21, unclos_sovereignty_boundary__historical_rights_reading, theater_ratio, 21, 0.3).
narrative_ontology:measurement(unclos_hist_rights_tr_t28, unclos_sovereignty_boundary__historical_rights_reading, theater_ratio, 28, 0.33).
narrative_ontology:measurement(unclos_hist_rights_tr_t35, unclos_sovereignty_boundary__historical_rights_reading, theater_ratio, 35, 0.36).
narrative_ontology:measurement(unclos_hist_rights_tr_t42, unclos_sovereignty_boundary__historical_rights_reading, theater_ratio, 42, 0.38).

% Extraction over time
narrative_ontology:measurement(unclos_hist_rights_be_t0, unclos_sovereignty_boundary__historical_rights_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(unclos_hist_rights_be_t7, unclos_sovereignty_boundary__historical_rights_reading, base_extractiveness, 7, 0.42).
narrative_ontology:measurement(unclos_hist_rights_be_t14, unclos_sovereignty_boundary__historical_rights_reading, base_extractiveness, 14, 0.48).
narrative_ontology:measurement(unclos_hist_rights_be_t21, unclos_sovereignty_boundary__historical_rights_reading, base_extractiveness, 21, 0.55).
narrative_ontology:measurement(unclos_hist_rights_be_t28, unclos_sovereignty_boundary__historical_rights_reading, base_extractiveness, 28, 0.61).
narrative_ontology:measurement(unclos_hist_rights_be_t35, unclos_sovereignty_boundary__historical_rights_reading, base_extractiveness, 35, 0.65).
narrative_ontology:measurement(unclos_hist_rights_be_t42, unclos_sovereignty_boundary__historical_rights_reading, base_extractiveness, 42, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(unclos_hist_rights_su_t0, unclos_sovereignty_boundary__historical_rights_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(unclos_hist_rights_su_t7, unclos_sovereignty_boundary__historical_rights_reading, suppression_requirement, 7, 0.45).
narrative_ontology:measurement(unclos_hist_rights_su_t14, unclos_sovereignty_boundary__historical_rights_reading, suppression_requirement, 14, 0.5).
narrative_ontology:measurement(unclos_hist_rights_su_t21, unclos_sovereignty_boundary__historical_rights_reading, suppression_requirement, 21, 0.54).
narrative_ontology:measurement(unclos_hist_rights_su_t28, unclos_sovereignty_boundary__historical_rights_reading, suppression_requirement, 28, 0.57).
narrative_ontology:measurement(unclos_hist_rights_su_t35, unclos_sovereignty_boundary__historical_rights_reading, suppression_requirement, 35, 0.6).
narrative_ontology:measurement(unclos_hist_rights_su_t42, unclos_sovereignty_boundary__historical_rights_reading, suppression_requirement, 42, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(unclos_sovereignty_boundary__historical_rights_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(unclos_sovereignty_boundary__historical_rights_reading, 0.12).
narrative_ontology:affects_constraint(unclos_sovereignty_boundary__historical_rights_reading, unclos_sovereignty_boundary__strict_eez_reading).
narrative_ontology:affects_constraint(unclos_sovereignty_boundary__historical_rights_reading, unclos_sovereignty_boundary__non_ratifier_enforcement_reading).
narrative_ontology:affects_constraint(unclos_sovereignty_boundary__historical_rights_reading, south_china_sea_nine_dash_line).
narrative_ontology:affects_constraint(unclos_sovereignty_boundary__historical_rights_reading, east_china_sea_senkaku_dispute).
narrative_ontology:affects_constraint(unclos_sovereignty_boundary__historical_rights_reading, arctic_continental_shelf_claims).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the unclos_sovereignty_boundary kernel. The strict_eez_reading (ε ≈ 0.05, Mountain) treats UNCLOS EEZ as exclusive. The non_ratifier_enforcement_reading (ε ≈ 0.25, Rope/Tangled Rope) treats customary FoN as independently enforceable. This historical_rights_reading (ε = 0.68, Tangled Rope) asserts pre-treaty historical rights override EEZ. Their ε values differ by an order of magnitude — they are distinct constraints linked by the kernel, not one constraint with measurement variance.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(unclos_sovereignty_boundary__historical_rights_reading, institutional, 0.15).
constraint_indexing:directionality_override(unclos_sovereignty_boundary__historical_rights_reading, institutional, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
