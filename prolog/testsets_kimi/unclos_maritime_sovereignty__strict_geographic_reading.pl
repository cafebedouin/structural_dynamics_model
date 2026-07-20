% ============================================================================
% CONSTRAINT STORY: unclos_maritime_sovereignty__strict_geographic_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_unclos_maritime_sovereignty__strict_geographic_reading, []).

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
 *   constraint_id: unclos_maritime_sovereignty__strict_geographic_reading
 *   human_readable: UNCLOS Strict Geographic Reading: Natural Features Only for Territorial Sea and EEZ
 *   domain: international_law/maritime_governance/geopolitical_strategy
 *
 * SUMMARY:
 *   This constraint instantiates the strict geographic reading of the UNCLOS
 *   maritime sovereignty kernel: only naturally formed land features above
 *   water at high tide generate territorial sea and EEZ, while artificial
 *   islands and installations are categorically denied such entitlement. The
 *   reading is contested by expansionist coastal states that have invested in
 *   massive artificial construction and by hybrid interpretations that would
 *   allow limited safety zones to mature into broader claims over time. Naval
 *   powers and non-claimant states benefit from the constraint because it
 *   limits sovereignty creep and preserves freedom of navigation and
 *   international commons. The constraint is a tangled rope: it coordinates a
 *   stable maritime order by preventing manufactured territorial expansion,
 *   but it asymmetrically extracts sovereignty potential from expansionist
 *   coastal states, requiring active legal and naval enforcement to hold
 *   against contrary state practice.
 *
 * KEY AGENTS:
 *   - naval_powers (agenda_setter/beneficiary): Enforce freedom of navigation and benefit from constrained rival claims
 *   - non_claimant_maritime_states (beneficiary): Gain preserved access to waters that would otherwise be enclosed
 *   - expansionist_coastal_states (payer): Bear the loss of EEZ and territorial sea claims around artificial features
 *   - international_tribunals (agenda_setter): Administer the strict reading through dispute settlement
 *   - maritime_law_scholars (observer): Provide interpretive frameworks
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(unclos_maritime_sovereignty__strict_geographic_reading, 0.65).
domain_priors:suppression_score(unclos_maritime_sovereignty__strict_geographic_reading, 0.58).
domain_priors:theater_ratio(unclos_maritime_sovereignty__strict_geographic_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(unclos_maritime_sovereignty__strict_geographic_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(unclos_maritime_sovereignty__strict_geographic_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(unclos_maritime_sovereignty__strict_geographic_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(unclos_maritime_sovereignty__strict_geographic_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(unclos_maritime_sovereignty__strict_geographic_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(unclos_maritime_sovereignty__strict_geographic_reading, tangled_rope).
narrative_ontology:human_readable(unclos_maritime_sovereignty__strict_geographic_reading, "UNCLOS Strict Geographic Reading: Natural Features Only for Territorial Sea and EEZ").
narrative_ontology:topic_domain(unclos_maritime_sovereignty__strict_geographic_reading, "international_law/maritime_governance/geopolitical_strategy").

domain_priors:requires_active_enforcement(unclos_maritime_sovereignty__strict_geographic_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(unclos_maritime_sovereignty__strict_geographic_reading, '24beb9a3-b4de-4ec8-8c22-ec54510a151b').
narrative_ontology:cs_kernel_codification('24beb9a3-b4de-4ec8-8c22-ec54510a151b', formalized).
narrative_ontology:cs_authority_grounding('24beb9a3-b4de-4ec8-8c22-ec54510a151b', lineage).
narrative_ontology:cs_interpretation_layer_present('24beb9a3-b4de-4ec8-8c22-ec54510a151b').
narrative_ontology:cs_reading_relation('24beb9a3-b4de-4ec8-8c22-ec54510a151b', unclos_maritime_sovereignty__expansive_construction_reading, forecloses).
narrative_ontology:cs_reading_relation('24beb9a3-b4de-4ec8-8c22-ec54510a151b', unclos_maritime_sovereignty__hybrid_effective_control_reading, forecloses).
narrative_ontology:cs_axiom('24beb9a3-b4de-4ec8-8c22-ec54510a151b', foundational, natural_formation_sovereignty_gate).
narrative_ontology:cs_axiom_status(natural_formation_sovereignty_gate, holdable).
narrative_ontology:cs_axiom_grounding('24beb9a3-b4de-4ec8-8c22-ec54510a151b', natural_formation_sovereignty_gate, conventional).
narrative_ontology:cs_axiom('24beb9a3-b4de-4ec8-8c22-ec54510a151b', foundational, artificial_features_permanent_exclusion).
narrative_ontology:cs_axiom_status(artificial_features_permanent_exclusion, holdable).
narrative_ontology:cs_axiom_grounding('24beb9a3-b4de-4ec8-8c22-ec54510a151b', artificial_features_permanent_exclusion, conventional).
narrative_ontology:cs_reference_frame('24beb9a3-b4de-4ec8-8c22-ec54510a151b', strict_textual_sovereignty).
narrative_ontology:cs_drift_state('24beb9a3-b4de-4ec8-8c22-ec54510a151b', post_south_china_sea_arbitration_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('24beb9a3-b4de-4ec8-8c22-ec54510a151b', '').
narrative_ontology:cs_kernel_id(unclos_maritime_sovereignty__strict_geographic_reading, unclos_maritime_sovereignty).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(unclos_maritime_sovereignty__strict_geographic_reading, naval_powers).
narrative_ontology:constraint_beneficiary(unclos_maritime_sovereignty__strict_geographic_reading, non_claimant_maritime_states).
narrative_ontology:constraint_victim(unclos_maritime_sovereignty__strict_geographic_reading, expansionist_coastal_states).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintain global naval presence and conduct freedom-of-navigation operations to challenge expansive maritime claims. They benefit from a legal regime that prevents artificial expansion of territorial seas, preserving open sea-lanes and strategic mobility. They actively assert the strict reading through military patrols, alliance coordination, and diplomatic pressure.
narrative_ontology:constraint_stakeholder(unclos_maritime_sovereignty__strict_geographic_reading, naval_powers, agenda_setter,
    powerful, generational, mobile, global).
narrative_ontology:stakeholder_secondary_role(unclos_maritime_sovereignty__strict_geographic_reading, naval_powers, beneficiary).

% Benefit from constrained sovereignty claims by expansionist neighbors, which preserves access to fisheries, shipping lanes, and seabed resources in areas that would otherwise be enclosed by artificially generated EEZs. They rely on the strict reading to prevent adjacent waters from being absorbed without reciprocal constraint.
narrative_ontology:constraint_stakeholder(unclos_maritime_sovereignty__strict_geographic_reading, non_claimant_maritime_states, beneficiary,
    moderate, generational, constrained, global).

% Have invested in large-scale artificial island construction on submerged features and low-tide elevations to support territorial and resource claims. The strict reading nullifies these claims, denying them EEZ and territorial sea generated by artificial features and restricting them to 500-meter safety zones around installations. They contest the reading through continued construction, non-appearance at tribunals, and alternative legal argumentation.
narrative_ontology:constraint_stakeholder(unclos_maritime_sovereignty__strict_geographic_reading, expansionist_coastal_states, payer,
    powerful, generational, constrained, regional).

% Adjudicate disputes over maritime entitlement under UNCLOS, interpreting Article 121 and the regime of artificial islands. Their rulings â notably the 2016 South China Sea Arbitration â reaffirm that artificial construction does not confer territorial sea or EEZ, effectively administering the strict reading against contrary state practice.
narrative_ontology:constraint_stakeholder(unclos_maritime_sovereignty__strict_geographic_reading, international_tribunals, agenda_setter,
    institutional, generational, analytical, global).

% Analyze and debate the coherence of the natural-artificial distinction in UNCLOS, the evidentiary standards for naturally formed status, and the relationship between treaty text and subsequent state practice. They provide the interpretive frameworks through which the strict reading is justified or challenged.
narrative_ontology:constraint_stakeholder(unclos_maritime_sovereignty__strict_geographic_reading, maritime_law_scholars, observer,
    analytical, generational, analytical, global).

% Would benefit from more flexible baselines or feature-status rules that account for climate-driven erosion and ecological restoration, but their interests are marginalized in the strict reading's rigid natural-artificial binary. They are rarely heard in debates dominated by major naval powers and coastal claimants.
narrative_ontology:constraint_stakeholder(unclos_maritime_sovereignty__strict_geographic_reading, small_island_developing_states, excluded,
    moderate, generational, constrained, regional).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(unclos_maritime_sovereignty__strict_geographic_reading, diffuse).
narrative_ontology:fixing_cost_class(unclos_maritime_sovereignty__strict_geographic_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Prevents unilateral territorial expansion through engineering, preserving a stable maritime order where sovereignty claims are limited to naturally occurring land features and freedom of navigation is protected against manufactured enclosure.
% TRANSFER_FUNCTION: Moves potential maritime resource and strategic entitlement from expansionist coastal states to the international commons (high seas and EEZ of others), while preserving navigational freedom and strategic space for naval powers and non-claimant states.
% ABSENT_VOICES: Coastal populations of claimant states whose livelihoods depend on fishing around artificial features are not directly represented in the legal debate; small island developing states that might benefit from flexible baselines reflecting climate adaptation are sidelined by the strict reading's rigidity; construction firms and laborers building artificial features are excluded from the diplomatic conversation entirely.
% DISAPPEARANCE_RATIONALE: If the strict geographic reading vanished, expansionist coastal states would claim full EEZ around artificial installations, shrinking international waters and high seas, triggering naval counter-assertions, redistributing fisheries and seabed mineral rights across the Indo-Pacific and other contested regions, and collapsing the natural-feature limitation that currently bounds sovereignty creep.
% FOUNDING_PROBLEM: Mid-20th century fears that technological land reclamation would allow states to manufacture territorial claims and destabilize the maritime order by eroding the distinction between land and sea, leading to a free-for-all of engineered expansion.
% FOUNDING_PROBLEM_CORROBORATION: Naval powers and non-claimant states attest the problem is live. The Permanent Court of Arbitration (2016) and independent UNCLOS scholarship corroborate from outside the beneficiary set of expansionist states that unilateral artificial expansion threatens the maritime legal order.
narrative_ontology:disappearance_verdict(unclos_maritime_sovereignty__strict_geographic_reading, world_rearranges).
narrative_ontology:founding_problem_status(unclos_maritime_sovereignty__strict_geographic_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(unclos_maritime_sovereignty__strict_geographic_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(unclos_maritime_sovereignty__strict_geographic_reading, 'none', 1).
narrative_ontology:epsilon_provenance(unclos_maritime_sovereignty__strict_geographic_reading, 0.65, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(unclos_maritime_sovereignty__strict_geographic_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(unclos_maritime_sovereignty__strict_geographic_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(unclos_maritime_sovereignty__strict_geographic_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.65) is substantial because the reading nullifies significant sovereignty claims and resource entitlements that expansionist states sought to manufacture. Suppression (0.58) reflects active legal and diplomatic suppression of expansive claims, backed by naval operations; it is not total because expansionist states continue to build and operate artificial features in defiance. Theater_ratio (0.38 at interval end) captures the increasing performative dimension of legal proceedings and diplomatic communiques that assert the reading while physical facts on the water diverge. Accessibility_collapse (0.50) is moderate: the strict reading is the treaty text, but the expansive and hybrid readings remain live alternatives advocated by powerful actors. Resistance (0.62) is significant because expansionist states actively contest the reading through construction, non-appearance at tribunals, and alternative legal arguments.
 *
 * PERSPECTIVAL GAP:
 *   From the naval-power seat, the constraint is necessary coordination that prevents a chaotic sovereignty grab and preserves global commons. From the expansionist-coastal-state seat, the same constraint is an externally imposed legal barrier that extracts their capacity to consolidate control over adjacent waters and seabed resources. The non-claimant state seat experiences moderate benefit without active enforcement burden. The engine computes this divergence from the same structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Naval powers and non-claimant states are structural beneficiaries: the constraint subsidizes their strategic and economic interests by limiting competitor enclosure. Their directionality sits near the beneficiary end (low d). Expansionist coastal states are the declared victims: the constraint directly targets their manufactured claims, placing them near the full-target end (high d). International tribunals administer the constraint but do not collect its extraction; their directionality is symmetric (d ~ 0.5). No override is needed because the structural derivation captures the relationships.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint prevents mislabeling by its clear coordination function: without a natural-feature requirement, technological powers could manufacture territorial sea and EEZ without limit, collapsing the land-sea distinction and destabilizing maritime order. This genuine coordination function distinguishes it from a pure snare. However, the distributional asymmetry â naval powers gain strategic space while expansionist coastal states lose claimed resources â prevents classification as a pure rope. The active enforcement requirement (tribunal rulings, FONOPs, diplomatic pressure) confirms the tangled rope structure: it must be held in place against powerful exit-seeking actors.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contested_status,
    'Is the strict geographic reading of UNCLOS Article 121 the sole legally valid interpretation, or does state practice around artificial island construction constitute a competing customary norm that narrows the reading''s reach?',
    'Persistent objector analysis and subsequent treaty practice survey across UNCLOS parties; if a critical mass of coastal states consistently assert EEZ around artificial features and are not effectively challenged, the strict reading may be reduced to a formalistic minority position.',
    'If state practice overrides the textual reading, this constraint''s extractiveness diminishes and its classification may shift toward rope or scaffold (a transitional norm); if textualism holds against practice, it remains a tangled_rope actively enforced against expansionist deviation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contested_status, conceptual, 'Whether strict textualism or competing customary law governs artificial island status').

omega_variable(
    artificiality_technological_boundary,
    'Does the natural-artificial distinction remain legally coherent as climate adaptation and geoengineering technologies blur the boundary between naturally formed and human-maintained features?',
    'Adjudication of cases involving ecologically restored reefs, terraformed features, or climate-resilience construction that permanently alters a feature''s elevation and composition.',
    'If the boundary collapses technologically, the strict reading''s enforcement becomes arbitrary, raising theater_ratio and potentially reclassifying toward piton as the constraint is maintained performatively against material reality.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(artificiality_technological_boundary, conceptual, 'Technological erosion of the natural-artificial legal boundary').

omega_variable(
    enforcement_mechanism_ambiguity,
    'Is the suppression of expansive claims achieved primarily through legal-institutional channels (tribunals, diplomatic pressure) or through naval power projection and the threat of kinetic escalation?',
    'Comparative case analysis of FONOP outcomes versus tribunal compliance rates; if naval presence is the actual enforcement mechanism, the constraint''s character is more coercive than juridical.',
    'If naval power is the dominant enforcement mechanism, the constraint''s suppression score understates the coercive foundation and the type may lean toward snare; if legal-institutional channels dominate, the tangled_rope classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_mechanism_ambiguity, empirical, 'Whether enforcement is juridical or military-coercive').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(unclos_maritime_sovereignty__strict_geographic_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(uncl_tr_t0, unclos_maritime_sovereignty__strict_geographic_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(uncl_tr_t6, unclos_maritime_sovereignty__strict_geographic_reading, theater_ratio, 6, 0.22).
narrative_ontology:measurement(uncl_tr_t12, unclos_maritime_sovereignty__strict_geographic_reading, theater_ratio, 12, 0.28).
narrative_ontology:measurement(uncl_tr_t18, unclos_maritime_sovereignty__strict_geographic_reading, theater_ratio, 18, 0.32).
narrative_ontology:measurement(uncl_tr_t24, unclos_maritime_sovereignty__strict_geographic_reading, theater_ratio, 24, 0.35).
narrative_ontology:measurement(uncl_tr_t30, unclos_maritime_sovereignty__strict_geographic_reading, theater_ratio, 30, 0.38).

% Extraction over time
narrative_ontology:measurement(uncl_be_t0, unclos_maritime_sovereignty__strict_geographic_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(uncl_be_t6, unclos_maritime_sovereignty__strict_geographic_reading, base_extractiveness, 6, 0.5).
narrative_ontology:measurement(uncl_be_t12, unclos_maritime_sovereignty__strict_geographic_reading, base_extractiveness, 12, 0.55).
narrative_ontology:measurement(uncl_be_t18, unclos_maritime_sovereignty__strict_geographic_reading, base_extractiveness, 18, 0.6).
narrative_ontology:measurement(uncl_be_t24, unclos_maritime_sovereignty__strict_geographic_reading, base_extractiveness, 24, 0.63).
narrative_ontology:measurement(uncl_be_t30, unclos_maritime_sovereignty__strict_geographic_reading, base_extractiveness, 30, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(uncl_su_t0, unclos_maritime_sovereignty__strict_geographic_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(uncl_su_t6, unclos_maritime_sovereignty__strict_geographic_reading, suppression_requirement, 6, 0.45).
narrative_ontology:measurement(uncl_su_t12, unclos_maritime_sovereignty__strict_geographic_reading, suppression_requirement, 12, 0.52).
narrative_ontology:measurement(uncl_su_t18, unclos_maritime_sovereignty__strict_geographic_reading, suppression_requirement, 18, 0.58).
narrative_ontology:measurement(uncl_su_t24, unclos_maritime_sovereignty__strict_geographic_reading, suppression_requirement, 24, 0.6).
narrative_ontology:measurement(uncl_su_t30, unclos_maritime_sovereignty__strict_geographic_reading, suppression_requirement, 30, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(unclos_maritime_sovereignty__strict_geographic_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(unclos_maritime_sovereignty__strict_geographic_reading, expansive_construction_reading).
narrative_ontology:affects_constraint(unclos_maritime_sovereignty__strict_geographic_reading, hybrid_effective_control_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the unclos_maritime_sovereignty kernel. The strict_geographic_reading, expansive_construction_reading, and hybrid_effective_control_reading are structurally distinct claims that share a colloquial label but have different epsilon values, failure modes, and empirical status. They are linked as a constraint family per the epsilon-invariance principle.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
