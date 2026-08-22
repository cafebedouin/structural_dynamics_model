% ============================================================================
% CONSTRAINT STORY: unclos_maritime_sovereignty__strict_geographic_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: unclos_maritime_sovereignty__strict_geographic_reading
 *   human_readable: UNCLOS Strict Geographic Definition of Islands (Strict Reading)
 *   domain: international_law/maritime_governance
 *
 * SUMMARY:
 *   UNCLOS Article 121 defines islands as naturally formed features of land
 *   surrounded by water that are above water at high tide. This constraint
 *   instantiates the strict geographic reading of that article: only
 *   naturally formed islands generate territorial sea and exclusive economic
 *   zones; artificial constructions do not alter the legal status of
 *   submerged or low-tide features. The reading is contested by states
 *   pursuing artificial island expansion strategies (primarily in the South
 *   China Sea), who advocate for effective-occupation readings that grant
 *   sovereignty through continuous administrative control. The strict reading
 *   benefits naval powers and non-claimant states; it constrains expansionist
 *   coastal states and artificial island investors. The claim/metric
 *   divergence is intentional: CLAIMED as rope (a clear, coordinating legal
 *   rule with genuine benefit) while AUTHORED METRICS describe moderately
 *   extractive operation (extraction rises over time as artificial island
 *   technology improves and contestation intensifies). The engine measures
 *   that divergence; the committer frame documents the kernel contest in
 *   omega and cs_structure.
 *
 * KEY AGENTS:
 *   - Naval powers (USA, Russia, UK) — preserve freedom of navigation; benefit from narrow sovereignty scope
 *   - Expansionist coastal states (China, Vietnam, Philippines) — pursue artificial island expansion; pay the cost of the strict reading
 *   - Non-claimant states (most of world) — benefit from predictable maritime boundaries and resource access
 *   - UNCLOS interpretive body (ICJ, ITLOS, state practice) — agenda-setter; enforces the strict geographic reading
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(unclos_maritime_sovereignty__strict_geographic_reading, 0.38).
domain_priors:suppression_score(unclos_maritime_sovereignty__strict_geographic_reading, 0.42).
domain_priors:theater_ratio(unclos_maritime_sovereignty__strict_geographic_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(unclos_maritime_sovereignty__strict_geographic_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(unclos_maritime_sovereignty__strict_geographic_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(unclos_maritime_sovereignty__strict_geographic_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(unclos_maritime_sovereignty__strict_geographic_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(unclos_maritime_sovereignty__strict_geographic_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(unclos_maritime_sovereignty__strict_geographic_reading, rope).
narrative_ontology:human_readable(unclos_maritime_sovereignty__strict_geographic_reading, "UNCLOS Strict Geographic Definition of Islands (Strict Reading)").
narrative_ontology:topic_domain(unclos_maritime_sovereignty__strict_geographic_reading, "international_law/maritime_governance").

domain_priors:requires_active_enforcement(unclos_maritime_sovereignty__strict_geographic_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(unclos_maritime_sovereignty__strict_geographic_reading, 'db341522-e726-49a3-87a3-cfa142bb74e8').
narrative_ontology:cs_kernel_codification('db341522-e726-49a3-87a3-cfa142bb74e8', fixed_text).
narrative_ontology:cs_authority_grounding('db341522-e726-49a3-87a3-cfa142bb74e8', extraction).
narrative_ontology:cs_interpretation_layer_present('db341522-e726-49a3-87a3-cfa142bb74e8').
narrative_ontology:cs_reading_relation('db341522-e726-49a3-87a3-cfa142bb74e8', unclos_maritime_sovereignty__expansive_construction_reading, forecloses).
narrative_ontology:cs_reading_relation('db341522-e726-49a3-87a3-cfa142bb74e8', unclos_maritime_sovereignty__hybrid_effective_control_reading, coexists_with).
narrative_ontology:cs_axiom('db341522-e726-49a3-87a3-cfa142bb74e8', foundational, natural_formation_criterion_determinative).
narrative_ontology:cs_axiom_status(natural_formation_criterion_determinative, holdable).
narrative_ontology:cs_axiom_grounding('db341522-e726-49a3-87a3-cfa142bb74e8', natural_formation_criterion_determinative, deontological).
narrative_ontology:cs_axiom('db341522-e726-49a3-87a3-cfa142bb74e8', foundational, artificial_construction_legal_status_immutable).
narrative_ontology:cs_axiom_status(artificial_construction_legal_status_immutable, holdable).
narrative_ontology:cs_axiom_grounding('db341522-e726-49a3-87a3-cfa142bb74e8', artificial_construction_legal_status_immutable, conventional).
narrative_ontology:cs_reference_frame('db341522-e726-49a3-87a3-cfa142bb74e8', unclos_article_121_geographic_objectivity).
narrative_ontology:cs_drift_state('db341522-e726-49a3-87a3-cfa142bb74e8', post_2015_artificial_island_acceleration, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('db341522-e726-49a3-87a3-cfa142bb74e8', '').
narrative_ontology:cs_kernel_id(unclos_maritime_sovereignty__strict_geographic_reading, unclos_maritime_sovereignty).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(unclos_maritime_sovereignty__strict_geographic_reading, naval_powers).
narrative_ontology:constraint_beneficiary(unclos_maritime_sovereignty__strict_geographic_reading, non_claimant_states).
narrative_ontology:constraint_beneficiary(unclos_maritime_sovereignty__strict_geographic_reading, international_shipping).
narrative_ontology:constraint_victim(unclos_maritime_sovereignty__strict_geographic_reading, expansionist_coastal_states).
narrative_ontology:constraint_victim(unclos_maritime_sovereignty__strict_geographic_reading, artificial_island_investors).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(unclos_maritime_sovereignty__strict_geographic_reading, small_island_nations).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Militarily dominant states (USA, Russia, UK) that benefit from restrictions on artificial island sovereignty claims. The strict geographic reading preserves freedom of navigation and prevents artificial structures from generating exclusionary maritime zones. These powers can project force across oceanic spaces if artificial islands do not generate full territorial sea claims.
narrative_ontology:constraint_stakeholder(unclos_maritime_sovereignty__strict_geographic_reading, naval_powers, beneficiary,
    powerful, generational, arbitrage, global).

% Coastal and landlocked states without artificial island construction programs benefit from a narrow definition of island sovereignty. They face no disadvantage from the rule and benefit from access to marine resources and navigation routes that would otherwise be enclosed by expansionist neighbors.
narrative_ontology:constraint_stakeholder(unclos_maritime_sovereignty__strict_geographic_reading, non_claimant_states, beneficiary,
    organized, generational, mobile, global).

% Commercial and container shipping interests benefit from the strict definition: artificial islands do not reduce navigable waters or create new territorial choke points. Routes remain open; insurance and operational costs do not increase from new artificial maritime zones.
narrative_ontology:constraint_stakeholder(unclos_maritime_sovereignty__strict_geographic_reading, international_shipping, beneficiary,
    organized, biographical, constrained, global).

% States pursuing artificial island construction to expand territorial claims and economic zones (e.g., China, Vietnam, Philippines in the South China Sea) bear the primary cost of the strict reading: their construction yields no additional sovereignty, no expanded EEZ, and no territorial sea. They must justify occupation through alternative means or abandon ambitions.
narrative_ontology:constraint_stakeholder(unclos_maritime_sovereignty__strict_geographic_reading, expansionist_coastal_states, payer,
    powerful, generational, constrained, regional).

% Public and private entities that finance artificial island construction (often as proxies for state sovereignty claims) bear the cost of construction without the legal benefit of expanded maritime jurisdiction. Their investments do not generate territorial sea or EEZ unless the underlying feature was already naturally formed above high tide.
narrative_ontology:constraint_stakeholder(unclos_maritime_sovereignty__strict_geographic_reading, artificial_island_investors, payer,
    powerful, biographical, trapped, regional).

% The collective body of UNCLOS signatories and the international maritime law interpretive community (International Court of Justice, ITLOS, state practice) that enforces and adjudicates the strict geographic reading. They set the standard, adjudicate disputes, and interpret UNCLOS Article 121.
narrative_ontology:constraint_stakeholder(unclos_maritime_sovereignty__strict_geographic_reading, unclos_coastal_state_consortium, agenda_setter,
    institutional, generational, analytical, global).

% Naturally formed island states (e.g., Mauritius, Seychelles, Pacific island nations) benefit from a strict definition that protects the sovereignty and EEZ of naturally formed islands from being outcompeted by artificial construction. Their legal standing and resource rights depend on the natural-formation criterion remaining determinative.
narrative_ontology:constraint_stakeholder(unclos_maritime_sovereignty__strict_geographic_reading, small_island_nations, beneficiary,
    moderate, generational, trapped, local).

% Would-be advocates for expansive artificial island sovereignty (some Southeast Asian and Indian Ocean states, strategic investors) are structurally absent from the UNCLOS interpretation process as full agents: they can contest through litigation and state practice, but the treaty text and international precedent constrain their voice.
narrative_ontology:constraint_stakeholder(unclos_maritime_sovereignty__strict_geographic_reading, excluded_artificial_expansion_advocates, excluded,
    powerful, generational, trapped, regional).

% Academic and professional analysts of maritime law observe and occasionally influence the interpretation through publications, expert testimony in international courts, and advisory roles to states. They do not set the rule but shape discourse around its meaning and application.
narrative_ontology:constraint_stakeholder(unclos_maritime_sovereignty__strict_geographic_reading, international_law_scholars, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(unclos_maritime_sovereignty__strict_geographic_reading, naval_powers).
narrative_ontology:fixing_cost_class(unclos_maritime_sovereignty__strict_geographic_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a clear, verifiable criterion (natural formation above high tide) for determining which geographic features generate territorial sea and exclusive economic zones, enabling predictable maritime sovereignty claims and preventing unlimited expansion via artificial construction.
% TRANSFER_FUNCTION: Transfers maritime sovereignty and resource rights to states whose territories include naturally formed islands; withholds these rights from states that construct artificial islands. Moves strategic advantage from artificial-island-capable wealthy states to naturally situated states and naval powers favoring open navigation.
% ABSENT_VOICES: States pursuing artificial island expansion strategies (China, Vietnam, Philippines) would object if fully present in UNCLOS's original drafting and ongoing interpretation. They are present in litigation but lack veto power over treaty interpretation.
% DISAPPEARANCE_RATIONALE: If the strict geographic reading disappeared, maritime boundaries in contested regions would be redrawn within months. Artificial island projects would accelerate; navigation routes would shift; economic exclusion zones would expand dramatically.
% FOUNDING_PROBLEM: Early UNCLOS negotiation needed a clear, objective test for distinguishing legitimate island-based sovereignty from speculative or artificial territorial claims.
% FOUNDING_PROBLEM_CORROBORATION: UNCLOS Article 121 drafters attest the problem was real. Contemporary legal scholars and naval powers attest the criterion remains sound. Expansionist states attest the criterion is outdated given modern artificial island technology.
narrative_ontology:disappearance_verdict(unclos_maritime_sovereignty__strict_geographic_reading, world_rearranges).
narrative_ontology:founding_problem_status(unclos_maritime_sovereignty__strict_geographic_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(unclos_maritime_sovereignty__strict_geographic_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(unclos_maritime_sovereignty__strict_geographic_reading, 'none', 1).
narrative_ontology:epsilon_provenance(unclos_maritime_sovereignty__strict_geographic_reading, 0.38, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(unclos_maritime_sovereignty__strict_geographic_reading_tests).
:- end_tests(unclos_maritime_sovereignty__strict_geographic_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate and rising (0.22 at UNCLOS entry, 0.38 by 2025) because the constraint's restrictive effect on artificial expansion increases as technology improves and strategic incentives to expand intensify. Early UNCLOS (1982) saw limited artificial island construction; by 2015–2025, artificial island projects accelerate in the South China Sea and Indian Ocean, making the strict reading's denial of sovereignty increasingly costly for expansionist states. Suppression requirement rises correspondingly (0.25 to 0.42) because maintaining the strict geographic reading requires active legal and diplomatic enforcement against state practice that constructs artificial islands and claims de facto sovereignty. Theater ratio remains moderate (0.12 to 0.28) because the constraint combines genuine coordination (clear, verifiable definition) with real enforcement (litigation, treaty interpretation, diplomatic pressure). The measurement series track one shared time grid from UNCLOS entry (1982) to present (2025), with early projections and observed post-1995 values.
 *
 * PERSPECTIVAL GAP:
 *   Naval powers and non-claimant states experience the rule as beneficial coordination: a clear criterion preventing enclosure. Expansionist coastal states experience it as constraining extraction: their investments in artificial islands yield no additional sovereignty. The UNCLOS interpretive body experiences it as a rule to enforce and interpret. These seats should compute different types: beneficiary seats see coordination; payer seats see extraction. The engine computes this divergence from the structural data. The authored claim (rope) reflects the beneficiary framing; the metrics reflect the payer experience.
 *
 * DIRECTIONALITY LOGIC:
 *   Naval powers and non-claimant states are beneficiaries (d near 0.2): the rule preserves their navigation rights and resource access without cost. International shipping collects diffuse benefits (d near 0.3). Expansionist coastal states and artificial island investors are the targets (d near 0.8–0.9): the rule directly constrains their expansion ambitions and makes investments unproductive in terms of maritime sovereignty. The UNCLOS interpretive body is the agenda-setter (d near 0.5): it enforces the rule but also derives legitimacy from maintaining the treaty's clarity. Directionality overrides are not needed; the structural data (beneficiary/victim + exit options) derives accurate d values.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (1970s: establish objective criterion to prevent unlimited artificial expansion) is CONTESTED as to status (still live or dead?). Naval powers attest it is live: artificial island technology is more advanced, more states attempt expansion, and the criterion remains necessary. Expansionist states attest it is dead: artificial islands are now normal infrastructure, their occupation is effective and sustained, and the legal barrier is obsolete. This contestation maps to the constraint's classification: if the problem is live, the rule is genuine coordination (rope, mandate intact); if dead, it is inertial extraction (piton, mandate obsolete but persisting for beneficiary states). The strict geographic reading sits between: it has lost some functional justification (artificial islands are technologically mundane, effective control is demonstrable) but retains coordination value (prevents boundless maritime enclosure). The measurement trajectory (rising extractiveness, rising theater) suggests creeping pitonization: the rule persists partly as genuine coordination and partly as defensive assertion of naval power interests.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    artificial_island_technology_maturation,
    'Has artificial island construction technology matured enough that the distinction between natural and artificial features is becoming functionally obsolete from a maritime-control perspective?',
    'Technological and geopolitical assessment: if artificial islands can sustain permanent occupation, infrastructure, and administrative control indistinguishably from natural islands, the functional premise of the strict geographic reading erodes.',
    'If matured, the strict reading becomes indefensible on coordination grounds and should reclassify toward piton (persisting for naval power interests, not collective benefit). If immature or irrelevant (natural and artificial remain distinct in maintenance, vulnerability, environmental impact), the strict reading retains coordination justification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(artificial_island_technology_maturation, empirical, 'Whether artificial island technology has eroded the functional distinction between natural and artificial features.').

omega_variable(
    effective_control_vs_legal_status_boundary,
    'Can artificial islands generate de facto maritime sovereignty through continuous effective control and administrative presence, even if legal doctrine denies formal territorial sea? That is, does the legal rule or the control-on-ground prevail?',
    'Resolution occurs through international litigation outcomes (ICJ, ITLOS rulings on whether effective control over artificial islands modifies their legal status) and state practice (whether expansionist states successfully claim and enforce maritime zones around artificial islands without legal recognition).',
    'If effective control prevails over legal doctrine, the strict geographic reading becomes a Snare: a rule enforced by naval powers and international law institutions to suppress expansionist claims despite demonstrated control. If legal doctrine holds, the rule is genuine coordination. The reading coexists with the hybrid reading precisely on this disagreement.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(effective_control_vs_legal_status_boundary, conceptual, 'Whether maritime sovereignty derives from legal status or from effective control on the ground.').

omega_variable(
    kernel_framing_under_determination,
    'Does the natural-formation criterion in UNCLOS Article 121 ground a commitment to geographic objectivity, or does it ground a commitment to preventing artificial expansion by weaker coastal states?',
    'Historical and textual analysis of UNCLOS drafting intentions; comparison of how the rule is enforced differentially across state power levels. If enforcement is stronger against weaker states'' artificial islands and weaker against major powers'' projects, the rule is better read as extraction. If applied uniformly regardless of state power, the objectivity framing is justified.',
    'If the rule is power-indexed enforcement, reclassification toward Snare is warranted (beneficiaries use legal doctrine to suppress competitors). If uniform, the rope classification holds. This is a conceptual-uncertainty omega because it hinges on which commitments the kernel is serving.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_framing_under_determination, conceptual, 'Whether the natural-formation criterion serves geographic objectivity or suppression of expansionist rivals.').

omega_variable(
    suppression_internalization_in_international_law,
    'Have expansionist coastal states internalized the suppression of artificial island claims, such that they no longer challenge the rule''s legitimacy, or do they continue to contest it through state practice and litigation?',
    'Empirical observation of state practice: continued artificial island construction, claims of maritime zones, and litigation challenging the rule''s application indicate internalization is incomplete. Cessation of construction and explicit acceptance would indicate internalization.',
    'If internalization is incomplete (high resistance, continued contestation), the suppression is structural and the rule is extraction. If states have accepted the suppression and no longer mount challenges, the constraint has shifted toward accepted coordination (lower effective suppression, lower d for target states).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_internalization_in_international_law, empirical, 'Whether suppression is structural or internalized among expansionist coastal states.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(unclos_maritime_sovereignty__strict_geographic_reading, 1982, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(uncl_tr_t1982, unclos_maritime_sovereignty__strict_geographic_reading, theater_ratio, 1982, 0.12).
narrative_ontology:measurement_basis(uncl_tr_t1982, projected).
narrative_ontology:measurement(uncl_tr_t1995, unclos_maritime_sovereignty__strict_geographic_reading, theater_ratio, 1995, 0.16).
narrative_ontology:measurement_basis(uncl_tr_t1995, observed).
narrative_ontology:measurement(uncl_tr_t2005, unclos_maritime_sovereignty__strict_geographic_reading, theater_ratio, 2005, 0.2).
narrative_ontology:measurement_basis(uncl_tr_t2005, observed).
narrative_ontology:measurement(uncl_tr_t2015, unclos_maritime_sovereignty__strict_geographic_reading, theater_ratio, 2015, 0.26).
narrative_ontology:measurement_basis(uncl_tr_t2015, observed).
narrative_ontology:measurement(uncl_tr_t2020, unclos_maritime_sovereignty__strict_geographic_reading, theater_ratio, 2020, 0.27).
narrative_ontology:measurement_basis(uncl_tr_t2020, observed).
narrative_ontology:measurement(uncl_tr_t2025, unclos_maritime_sovereignty__strict_geographic_reading, theater_ratio, 2025, 0.28).
narrative_ontology:measurement_basis(uncl_tr_t2025, observed).

% Extraction over time
narrative_ontology:measurement(uncl_be_t1982, unclos_maritime_sovereignty__strict_geographic_reading, base_extractiveness, 1982, 0.22).
narrative_ontology:measurement_basis(uncl_be_t1982, projected).
narrative_ontology:measurement(uncl_be_t1995, unclos_maritime_sovereignty__strict_geographic_reading, base_extractiveness, 1995, 0.28).
narrative_ontology:measurement_basis(uncl_be_t1995, observed).
narrative_ontology:measurement(uncl_be_t2005, unclos_maritime_sovereignty__strict_geographic_reading, base_extractiveness, 2005, 0.32).
narrative_ontology:measurement_basis(uncl_be_t2005, observed).
narrative_ontology:measurement(uncl_be_t2015, unclos_maritime_sovereignty__strict_geographic_reading, base_extractiveness, 2015, 0.36).
narrative_ontology:measurement_basis(uncl_be_t2015, observed).
narrative_ontology:measurement(uncl_be_t2020, unclos_maritime_sovereignty__strict_geographic_reading, base_extractiveness, 2020, 0.37).
narrative_ontology:measurement_basis(uncl_be_t2020, observed).
narrative_ontology:measurement(uncl_be_t2025, unclos_maritime_sovereignty__strict_geographic_reading, base_extractiveness, 2025, 0.38).
narrative_ontology:measurement_basis(uncl_be_t2025, observed).

% Suppression requirement over time
narrative_ontology:measurement(uncl_su_t1982, unclos_maritime_sovereignty__strict_geographic_reading, suppression_requirement, 1982, 0.25).
narrative_ontology:measurement_basis(uncl_su_t1982, projected).
narrative_ontology:measurement(uncl_su_t1995, unclos_maritime_sovereignty__strict_geographic_reading, suppression_requirement, 1995, 0.32).
narrative_ontology:measurement_basis(uncl_su_t1995, observed).
narrative_ontology:measurement(uncl_su_t2005, unclos_maritime_sovereignty__strict_geographic_reading, suppression_requirement, 2005, 0.38).
narrative_ontology:measurement_basis(uncl_su_t2005, observed).
narrative_ontology:measurement(uncl_su_t2015, unclos_maritime_sovereignty__strict_geographic_reading, suppression_requirement, 2015, 0.4).
narrative_ontology:measurement_basis(uncl_su_t2015, observed).
narrative_ontology:measurement(uncl_su_t2020, unclos_maritime_sovereignty__strict_geographic_reading, suppression_requirement, 2020, 0.41).
narrative_ontology:measurement_basis(uncl_su_t2020, observed).
narrative_ontology:measurement(uncl_su_t2025, unclos_maritime_sovereignty__strict_geographic_reading, suppression_requirement, 2025, 0.42).
narrative_ontology:measurement_basis(uncl_su_t2025, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(unclos_maritime_sovereignty__strict_geographic_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(unclos_maritime_sovereignty__strict_geographic_reading, 0.12).
narrative_ontology:affects_constraint(unclos_maritime_sovereignty__strict_geographic_reading, unclos_maritime_sovereignty__expansive_construction_reading).
narrative_ontology:affects_constraint(unclos_maritime_sovereignty__strict_geographic_reading, unclos_maritime_sovereignty__hybrid_effective_control_reading).
narrative_ontology:affects_constraint(unclos_maritime_sovereignty__strict_geographic_reading, south_china_sea_artificial_island_sovereignty).
narrative_ontology:affects_constraint(unclos_maritime_sovereignty__strict_geographic_reading, freedom_of_navigation_military_vessels).

% DUAL FORMULATION NOTE:
% This is one of three constraint stories decomposing the contested kernel unclos_maritime_sovereignty. The constraint family comprises strict_geographic_reading (this story), expansive_construction_reading (artificial islands generate territorial waters), and hybrid_effective_control_reading (artificial features generate limited zones that may mature into claims). Each story has a different ε (extractiveness), different beneficiary/victim structure, and different classification. They are linked via network.affects_constraints to enable the contamination propagation analysis. Each story's cs_structure documents its reading relations and axioms independently; no single framing captures all three.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(unclos_maritime_sovereignty__strict_geographic_reading, powerful, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
