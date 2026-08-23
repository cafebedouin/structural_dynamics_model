% ============================================================================
% CONSTRAINT STORY: unclos_maritime_sovereignty__hybrid_effective_control_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_unclos_maritime_sovereignty__hybrid_effective_control_reading, []).

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
 *   constraint_id: unclos_maritime_sovereignty__hybrid_effective_control_reading
 *   human_readable: UNCLOS Maritime Sovereignty — Hybrid Effective Control Reading
 *   domain: international_law/maritime_governance/geopolitical_strategy
 *
 * SUMMARY:
 *   This constraint story captures the hybrid effective control reading of
 *   UNCLOS maritime sovereignty — the interpretive position that natural
 *   features generate full maritime entitlements while artificial features
 *   generate only 500m safety zones under Article 60/80, but may mature into
 *   territorial claims through prolonged effective control absent effective
 *   challenge. This reading sits between the strict geographic reading (only
 *   natural islands count) and the expansive construction reading (building
 *   creates entitlements). It is the de facto operating rule in the South
 *   China Sea and similar disputed zones: claimants build, occupy, and wait.
 *   The constraint is a tangled rope because it performs genuine coordination
 *   (preventing pure might-makes-right by requiring duration and absence of
 *   challenge) while extracting maritime space from weaker claimants who
 *   cannot match construction or sustain challenge.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(unclos_maritime_sovereignty__hybrid_effective_control_reading, 0.58).
domain_priors:suppression_score(unclos_maritime_sovereignty__hybrid_effective_control_reading, 0.72).
domain_priors:theater_ratio(unclos_maritime_sovereignty__hybrid_effective_control_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(unclos_maritime_sovereignty__hybrid_effective_control_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(unclos_maritime_sovereignty__hybrid_effective_control_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(unclos_maritime_sovereignty__hybrid_effective_control_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(unclos_maritime_sovereignty__hybrid_effective_control_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(unclos_maritime_sovereignty__hybrid_effective_control_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(unclos_maritime_sovereignty__hybrid_effective_control_reading, tangled_rope).
narrative_ontology:human_readable(unclos_maritime_sovereignty__hybrid_effective_control_reading, "UNCLOS Maritime Sovereignty — Hybrid Effective Control Reading").
narrative_ontology:topic_domain(unclos_maritime_sovereignty__hybrid_effective_control_reading, "international_law/maritime_governance/geopolitical_strategy").

domain_priors:requires_active_enforcement(unclos_maritime_sovereignty__hybrid_effective_control_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(unclos_maritime_sovereignty__hybrid_effective_control_reading, '1dd6cc13-4f67-4c12-abbd-d950d29e554d').
narrative_ontology:cs_kernel_codification('1dd6cc13-4f67-4c12-abbd-d950d29e554d', formalized).
narrative_ontology:cs_authority_grounding('1dd6cc13-4f67-4c12-abbd-d950d29e554d', lineage).
narrative_ontology:cs_interpretation_layer_present('1dd6cc13-4f67-4c12-abbd-d950d29e554d').
narrative_ontology:cs_reading_relation('1dd6cc13-4f67-4c12-abbd-d950d29e554d', unclos_maritime_sovereignty__strict_geographic_reading, coexists_with).
narrative_ontology:cs_reading_relation('1dd6cc13-4f67-4c12-abbd-d950d29e554d', unclos_maritime_sovereignty__expansive_construction_reading, influences).
narrative_ontology:cs_axiom('1dd6cc13-4f67-4c12-abbd-d950d29e554d', foundational, effective_control_matures_entitlement).
narrative_ontology:cs_axiom_status(effective_control_matures_entitlement, holdable).
narrative_ontology:cs_axiom_grounding('1dd6cc13-4f67-4c12-abbd-d950d29e554d', effective_control_matures_entitlement, conventional).
narrative_ontology:cs_axiom('1dd6cc13-4f67-4c12-abbd-d950d29e554d', foundational, feature_type_creates_entitlement_gradient).
narrative_ontology:cs_axiom_status(feature_type_creates_entitlement_gradient, holdable).
narrative_ontology:cs_axiom_grounding('1dd6cc13-4f67-4c12-abbd-d950d29e554d', feature_type_creates_entitlement_gradient, conventional).
narrative_ontology:cs_reference_frame('1dd6cc13-4f67-4c12-abbd-d950d29e554d', unclos_1994_entry_into_force).
narrative_ontology:cs_drift_state('1dd6cc13-4f67-4c12-abbd-d950d29e554d', post_2016_arbitration, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('1dd6cc13-4f67-4c12-abbd-d950d29e554d', '').
narrative_ontology:cs_kernel_id(unclos_maritime_sovereignty__hybrid_effective_control_reading, unclos_maritime_sovereignty).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(unclos_maritime_sovereignty__hybrid_effective_control_reading, construction_capable_states).
narrative_ontology:constraint_beneficiary(unclos_maritime_sovereignty__hybrid_effective_control_reading, regional_power_projectors).
narrative_ontology:constraint_victim(unclos_maritime_sovereignty__hybrid_effective_control_reading, militarily_weaker_claimants).
narrative_ontology:constraint_victim(unclos_maritime_sovereignty__hybrid_effective_control_reading, non_claimant_coastal_states).
narrative_ontology:constraint_vindicates(unclos_maritime_sovereignty__hybrid_effective_control_reading, effective_control_as_sovereignty_basis).
narrative_ontology:constraint_vindicates(unclos_maritime_sovereignty__hybrid_effective_control_reading, graduated_feature_hierarchy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% States with advanced dredging, construction, and naval capacity (e.g., China, USA, Japan, Vietnam) that can physically transform submerged features into artificial islands and sustain administrative presence. They benefit from the hybrid rule's ambiguity: it allows them to build facts on the water and convert effective control into legal claims over time. They set the agenda by acting first — construction creates the control the rule rewards.
narrative_ontology:constraint_stakeholder(unclos_maritime_sovereignty__hybrid_effective_control_reading, construction_capable_states, beneficiary,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(unclos_maritime_sovereignty__hybrid_effective_control_reading, construction_capable_states, agenda_setter).

% States with sufficient naval and coast guard capacity to patrol and enforce claims in adjacent waters (e.g., Philippines, Malaysia, Indonesia in the South China Sea context). They benefit from the 500m safety zone as a enforceable buffer and from the maturation pathway when they can sustain presence. Their exit is mobile — they can shift patrol patterns, but not the underlying geography.
narrative_ontology:constraint_stakeholder(unclos_maritime_sovereignty__hybrid_effective_control_reading, regional_power_projectors, beneficiary,
    powerful, biographical, mobile, regional).

% States with competing maritime claims but insufficient construction or enforcement capacity to match artificial island building (e.g., some Southeast Asian claimants). They bear the cost of the hybrid rule: their traditional fishing grounds and potential EEZs are eroded by others' constructed features, and they cannot reciprocate. Exit is constrained — diplomatic protest and legal arbitration are available but slow and often ignored.
narrative_ontology:constraint_stakeholder(unclos_maritime_sovereignty__hybrid_effective_control_reading, militarily_weaker_claimants, payer,
    moderate, biographical, constrained, national).

% Coastal states without active claims in the disputed area but whose navigation rights and resource access are affected by expanding safety zones and matured claims. They pay through reduced freedom of navigation, constrained fishing access, and the precedent that construction creates rights. Exit is constrained — they must navigate through or around contested zones.
narrative_ontology:constraint_stakeholder(unclos_maritime_sovereignty__hybrid_effective_control_reading, non_claimant_coastal_states, payer,
    moderate, biographical, constrained, national).

% ITLOS, ICJ, and arbitral tribunals interpreting UNCLOS Article 121 and the status of artificial features. They observe the constraint's operation, issue rulings that clarify or contest the hybrid reading, but cannot enforce compliance. Their exit is analytical — they interpret but do not inhabit the maritime space.
narrative_ontology:constraint_stakeholder(unclos_maritime_sovereignty__hybrid_effective_control_reading, international_tribunals, observer,
    institutional, generational, analytical, global).

% Global shipping companies and flag states whose vessels transit affected waters. They would object to expanded territorial claims restricting innocent passage but have no formal seat in sovereignty disputes. Their exit is mobile — they can reroute — but at significant cost and delay.
narrative_ontology:constraint_stakeholder(unclos_maritime_sovereignty__hybrid_effective_control_reading, commercial_shipping_interests, excluded,
    organized, immediate, mobile, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a graduated legal framework for maritime entitlements that distinguishes natural islands (full EEZ/territorial sea), artificial installations (500m safety zones), and a maturation pathway for sustained effective control — reducing conflict by making entitlements depend on observable feature type and duration of control rather than pure assertion.
% TRANSFER_FUNCTION: Transfers maritime space and resource rights from weaker claimants and the global commons to states with construction and enforcement capacity, through the mechanism of recognizing artificial feature maturation. The transfer is not monetary but spatial-legal: square kilometers of EEZ, seabed resources, and navigation corridors.
% ABSENT_VOICES: Small island developing states without construction capacity; indigenous fishing communities whose traditional grounds are enclosed; environmental stakeholders affected by dredging and militarization. They are structurally excluded from the sovereignty negotiation — UNCLOS state-centric framework gives them no standing.
% DISAPPEARANCE_RATIONALE: If the hybrid rule vanished, the South China Sea and similar disputes would revert to binary classification: either features are islands (full entitlement) or they are not (no entitlement). Claimants would either escalate to pure force or retreat to strict geographic readings. The current managed ambiguity — where construction buys time and control buys law — would collapse into sharper confrontation or total legalization.
% FOUNDING_PROBLEM: UNCLOS Article 121's binary island/rock distinction left a gap: it did not anticipate large-scale artificial island construction on submerged features, nor the strategic use of prolonged effective control to create de facto entitlements. The hybrid reading emerged to manage this gap without amending the Convention.
% FOUNDING_PROBLEM_CORROBORATION: Legal scholars (e.g., Rothwell, Oude Elferink) document the drafting history showing Article 121 was not designed for artificial features. Tribunal awards (Philippines v. China, 2016) reject artificial feature entitlements. State practice since 2013 shows construction-capable states treating the gap as permission. No non-beneficiary corroboration exists for the maturation pathway — it is asserted by the actors who benefit from it.
narrative_ontology:disappearance_verdict(unclos_maritime_sovereignty__hybrid_effective_control_reading, world_rearranges).
narrative_ontology:founding_problem_status(unclos_maritime_sovereignty__hybrid_effective_control_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(unclos_maritime_sovereignty__hybrid_effective_control_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_nemotron+rescue1', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(unclos_maritime_sovereignty__hybrid_effective_control_reading, 'none', 1).
narrative_ontology:epsilon_provenance(unclos_maritime_sovereignty__hybrid_effective_control_reading, 0.58, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(unclos_maritime_sovereignty__hybrid_effective_control_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(unclos_maritime_sovereignty__hybrid_effective_control_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(unclos_maritime_sovereignty__hybrid_effective_control_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58) reflects the net transfer of maritime space to construction-capable states — not total extraction because the 500m limit and maturation requirement impose real constraints on the beneficiaries. Suppression (0.72) is high because the rule's persistence depends on active enforcement: coast guard patrols, militarized outposts, and diplomatic pressure to prevent challenge. Theater ratio (0.38) is moderate: safety zone administration and environmental monitoring are real functions, but a growing share of activity (runway construction, missile deployment, tourism promotion) serves claim consolidation. The measurement series shows extraction and suppression rising together post-2012 as construction accelerated.
 *
 * PERSPECTIVAL GAP:
 *   From the construction-capable state's seat, this is a rope: a workable coordination mechanism that prevents chaos by rewarding sustained presence. From the weaker claimant's seat, it is a snare: the coordination story (maturation through peaceful control) is cover for extraction backed by force they cannot match. The engine computes this divergence from the structural data — the claimed type (tangled_rope) acknowledges both functions exist simultaneously.
 *
 * DIRECTIONALITY LOGIC:
 *   Construction-capable states are structural beneficiaries (d ~ 0.15): they collect the maritime space, write the facts on the water, and control the maturation clock. Regional power projectors are secondary beneficiaries (d ~ 0.3): they gain enforceable buffers but cannot initiate at scale. Militarily weaker claimants and non-claimant coastal states are targets (d ~ 0.75-0.85): they lose space and access, and their exit options (legal, diplomatic) are slow and often ineffective. International tribunals are analytical observers (d = 0.5): they interpret but do not collect or pay. Commercial shipping is excluded — affected but not seated.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (Article 121's silence on artificial features) remains live — UNCLOS has not been amended. But the hybrid reading's maturation pathway has outlived its coordination function: it no longer prevents conflict but structures it, and the 'absent challenge' condition is now actively engineered by the powerful. The constraint persists because no party can unilaterally replace it — construction-capable states would lose their maturation pathway; weaker claimants would lose even the 500m limit. This is mandatrophy: the original mandate (manage the gap) is dead, but the arrangement persists as a structured arena for competition.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    maturation_threshold_ambiguity,
    'What duration and quality of ''effective control absent challenge'' suffices for maturation? Is there a threshold, or is it a continuous variable assessed ex post?',
    'State practice and tribunal analysis of specific maturation claims (e.g., Ito Aba/Taiping Island, Scarborough Shoal). Codification through an Implementation Agreement or subsequent practice consensus.',
    'If maturation requires a high, clear threshold (e.g., 50 years continuous administration), the constraint is more rope-like. If it is a low or manipulable threshold, the constraint is more snare-like — the coordination function is illusory.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(maturation_threshold_ambiguity, conceptual, 'Whether the maturation pathway has a determinate standard or is inherently manipulable.').

omega_variable(
    challenge_definition_ambiguity,
    'What counts as ''challenge'' that resets or blocks the maturation clock? Diplomatic protest? Arbitration filing? Physical interference? Naval patrol?',
    'Analysis of state practice: which actions have been treated as effective challenges by claimants and tribunals. The 2016 Philippines v. China arbitration addressed this partially but not exhaustively.',
    'If only physical interference counts, construction-capable states can deter challenge and mature claims unilaterally (snare). If diplomatic/legal acts count, weaker claimants have effective tools (rope).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(challenge_definition_ambiguity, empirical, 'Whether the challenge condition is a genuine check on maturation or a performative requirement.').

omega_variable(
    kernel_reading_framing,
    'Is the hybrid reading a genuine intermediate position, or does it structurally collapse into the expansive construction reading because the maturation pathway is the only operative term?',
    'Longitudinal analysis: track whether any artificial feature has matured WITHOUT the claimant also asserting expansive construction logic. If all maturation claims are accompanied by expansive assertions, the hybrid reading is a transitional framing.',
    'If the hybrid reading is a stable intermediate position, it has independent coordination value. If it collapses into expansive construction, it is a tactical framing with no independent structural existence.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_framing, conceptual, 'Whether this reading has independent structural coherence or is a waystation to the expansive reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(unclos_maritime_sovereignty__hybrid_effective_control_reading, 1994, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(uncl_tr_t1994, unclos_maritime_sovereignty__hybrid_effective_control_reading, theater_ratio, 1994, 0.1).
narrative_ontology:measurement(uncl_tr_t2002, unclos_maritime_sovereignty__hybrid_effective_control_reading, theater_ratio, 2002, 0.15).
narrative_ontology:measurement(uncl_tr_t2012, unclos_maritime_sovereignty__hybrid_effective_control_reading, theater_ratio, 2012, 0.22).
narrative_ontology:measurement(uncl_tr_t2016, unclos_maritime_sovereignty__hybrid_effective_control_reading, theater_ratio, 2016, 0.3).
narrative_ontology:measurement(uncl_tr_t2020, unclos_maritime_sovereignty__hybrid_effective_control_reading, theater_ratio, 2020, 0.35).
narrative_ontology:measurement(uncl_tr_t2024, unclos_maritime_sovereignty__hybrid_effective_control_reading, theater_ratio, 2024, 0.38).

% Extraction over time
narrative_ontology:measurement(uncl_be_t1994, unclos_maritime_sovereignty__hybrid_effective_control_reading, base_extractiveness, 1994, 0.25).
narrative_ontology:measurement(uncl_be_t2002, unclos_maritime_sovereignty__hybrid_effective_control_reading, base_extractiveness, 2002, 0.3).
narrative_ontology:measurement(uncl_be_t2012, unclos_maritime_sovereignty__hybrid_effective_control_reading, base_extractiveness, 2012, 0.42).
narrative_ontology:measurement(uncl_be_t2016, unclos_maritime_sovereignty__hybrid_effective_control_reading, base_extractiveness, 2016, 0.51).
narrative_ontology:measurement(uncl_be_t2020, unclos_maritime_sovereignty__hybrid_effective_control_reading, base_extractiveness, 2020, 0.56).
narrative_ontology:measurement(uncl_be_t2024, unclos_maritime_sovereignty__hybrid_effective_control_reading, base_extractiveness, 2024, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(uncl_su_t1994, unclos_maritime_sovereignty__hybrid_effective_control_reading, suppression_requirement, 1994, 0.4).
narrative_ontology:measurement(uncl_su_t2002, unclos_maritime_sovereignty__hybrid_effective_control_reading, suppression_requirement, 2002, 0.45).
narrative_ontology:measurement(uncl_su_t2012, unclos_maritime_sovereignty__hybrid_effective_control_reading, suppression_requirement, 2012, 0.58).
narrative_ontology:measurement(uncl_su_t2016, unclos_maritime_sovereignty__hybrid_effective_control_reading, suppression_requirement, 2016, 0.65).
narrative_ontology:measurement(uncl_su_t2020, unclos_maritime_sovereignty__hybrid_effective_control_reading, suppression_requirement, 2020, 0.7).
narrative_ontology:measurement(uncl_su_t2024, unclos_maritime_sovereignty__hybrid_effective_control_reading, suppression_requirement, 2024, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(unclos_maritime_sovereignty__hybrid_effective_control_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(unclos_maritime_sovereignty__hybrid_effective_control_reading, 0.12).
narrative_ontology:affects_constraint(unclos_maritime_sovereignty__hybrid_effective_control_reading, unclos_maritime_sovereignty__strict_geographic_reading).
narrative_ontology:affects_constraint(unclos_maritime_sovereignty__hybrid_effective_control_reading, unclos_maritime_sovereignty__expansive_construction_reading).
narrative_ontology:affects_constraint(unclos_maritime_sovereignty__hybrid_effective_control_reading, south_china_sea_nine_dash_line_enforcement).
narrative_ontology:affects_constraint(unclos_maritime_sovereignty__hybrid_effective_control_reading, arctic_continental_shelf_claims).

% DUAL FORMULATION NOTE:
% Part of the unclos_maritime_sovereignty constraint family (kernel). This reading (hybrid_effective_control_reading) decomposes the kernel's ambiguity into a graduated sovereignty rule. The strict_geographic_reading is the upstream Mountain (low extraction, high naturalness). The expansive_construction_reading is the downstream Snare (high extraction, pure assertion). This reading is the Tangled Rope in between — it cites the strict reading's naturalness while enabling the expansive reading's extraction through the maturation pathway. ε values differ: strict ~0.15, hybrid ~0.58, expansive ~0.85.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(unclos_maritime_sovereignty__hybrid_effective_control_reading, institutional, 0.1).
constraint_indexing:directionality_override(unclos_maritime_sovereignty__hybrid_effective_control_reading, moderate, 0.8).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
