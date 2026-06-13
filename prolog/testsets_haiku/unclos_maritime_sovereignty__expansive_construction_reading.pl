% ============================================================================
% CONSTRAINT STORY: unclos_maritime_sovereignty__expansive_construction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2025-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_unclos_maritime_sovereignty__expansive_construction_reading, []).

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
 *   constraint_id: unclos_maritime_sovereignty__expansive_construction_reading
 *   human_readable: Artificial Island Maritime Sovereignty via Construction (Expansive Reading)
 *   domain: international_law/maritime_governance
 *
 * SUMMARY:
 *   A state constructs artificial islands on submerged features or low-tide
 *   elevations within a contested maritime region and asserts that effective
 *   occupation and administrative control transform these features into
 *   islands generating 12nm territorial seas and 200nm exclusive economic
 *   zones. This reading claims the expanded waters belong to the constructing
 *   state by virtue of sovereign occupation; neighboring claimants and
 *   freedom-of-navigation states read the same constraint as extractive
 *   territorial expansion that violates UNCLOS's geographic criteria for
 *   island status. The constraint is claimed as tangled_rope (coordination of
 *   maritime sovereignty + asymmetric extraction from losers) and measured as
 *   substantially extractive and actively enforced, consistent with that
 *   claim. The claim and metrics remain independent authored facts; the
 *   engine decides certification.
 *
 * KEY AGENTS:
 *   - island_constructing_state: Institutional power, generational time horizon, mobile exit — agenda-setter, sets/enforces the sovereignty claim, benefits from resource access and strategic depth
 *   - neighboring_claimant_state: Institutional power, generational time horizon, constrained exit — payer, loses navigable waters and fishing grounds, cannot withdraw
 *   - freedom_of_navigation_state: Institutional power, biographical time horizon, constrained exit — payer, loses high-seas corridors, cannot walk away
 *   - distant_fishing_communities: Powerless, biographical time horizon, trapped exit — payer, face arrest and exclusion, no alternative grounds
 *   - domestic_resource_extraction_entity: Powerful, generational time horizon, arbitrage exit — beneficiary, gains exclusive resource access
 *   - international_maritime_law_community: Analytical power, generational time horizon, analytical exit — observer, interprets the kernel
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(unclos_maritime_sovereignty__expansive_construction_reading, 0.81).
domain_priors:suppression_score(unclos_maritime_sovereignty__expansive_construction_reading, 0.72).
domain_priors:theater_ratio(unclos_maritime_sovereignty__expansive_construction_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(unclos_maritime_sovereignty__expansive_construction_reading, extractiveness, 0.81).
narrative_ontology:constraint_metric(unclos_maritime_sovereignty__expansive_construction_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(unclos_maritime_sovereignty__expansive_construction_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(unclos_maritime_sovereignty__expansive_construction_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(unclos_maritime_sovereignty__expansive_construction_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(unclos_maritime_sovereignty__expansive_construction_reading, tangled_rope).
narrative_ontology:human_readable(unclos_maritime_sovereignty__expansive_construction_reading, "Artificial Island Maritime Sovereignty via Construction (Expansive Reading)").
narrative_ontology:topic_domain(unclos_maritime_sovereignty__expansive_construction_reading, "international_law/maritime_governance").

domain_priors:requires_active_enforcement(unclos_maritime_sovereignty__expansive_construction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(unclos_maritime_sovereignty__expansive_construction_reading, '7ede6183-297b-4764-a91b-b296065ef5ec').
narrative_ontology:cs_kernel_codification('7ede6183-297b-4764-a91b-b296065ef5ec', fixed_text).
narrative_ontology:cs_authority_grounding('7ede6183-297b-4764-a91b-b296065ef5ec', lineage).
narrative_ontology:cs_interpretation_layer_present('7ede6183-297b-4764-a91b-b296065ef5ec').
narrative_ontology:cs_reading_relation('7ede6183-297b-4764-a91b-b296065ef5ec', unclos_maritime_sovereignty__strict_geographic_reading, forecloses).
narrative_ontology:cs_reading_relation('7ede6183-297b-4764-a91b-b296065ef5ec', unclos_maritime_sovereignty__hybrid_effective_control_reading, coexists_with).
narrative_ontology:cs_axiom('7ede6183-297b-4764-a91b-b296065ef5ec', foundational, effective_occupation_generates_island_status).
narrative_ontology:cs_axiom_status(effective_occupation_generates_island_status, holdable).
narrative_ontology:cs_axiom_grounding('7ede6183-297b-4764-a91b-b296065ef5ec', effective_occupation_generates_island_status, conventional).
narrative_ontology:cs_axiom('7ede6183-297b-4764-a91b-b296065ef5ec', foundational, artificial_features_qualify_under_occupation_doctrine).
narrative_ontology:cs_axiom_status(artificial_features_qualify_under_occupation_doctrine, holdable).
narrative_ontology:cs_axiom_grounding('7ede6183-297b-4764-a91b-b296065ef5ec', artificial_features_qualify_under_occupation_doctrine, conventional).
narrative_ontology:cs_reference_frame('7ede6183-297b-4764-a91b-b296065ef5ec', unclos_article_121_occupation_sovereignty).
narrative_ontology:cs_drift_state('7ede6183-297b-4764-a91b-b296065ef5ec', contemporary_contested_maritime_boundaries, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('7ede6183-297b-4764-a91b-b296065ef5ec', '2025-06-12T14:32:00Z').
narrative_ontology:cs_kernel_id(unclos_maritime_sovereignty__expansive_construction_reading, unclos_maritime_sovereignty).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(unclos_maritime_sovereignty__expansive_construction_reading, island_constructing_states).
narrative_ontology:constraint_beneficiary(unclos_maritime_sovereignty__expansive_construction_reading, domestic_resource_extraction_entities).
narrative_ontology:constraint_victim(unclos_maritime_sovereignty__expansive_construction_reading, neighboring_claimant_states).
narrative_ontology:constraint_victim(unclos_maritime_sovereignty__expansive_construction_reading, freedom_of_navigation_states).
narrative_ontology:constraint_victim(unclos_maritime_sovereignty__expansive_construction_reading, distant_fishing_communities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(unclos_maritime_sovereignty__expansive_construction_reading, domestic_resource_extraction_entity).
narrative_ontology:constraint_victim(unclos_maritime_sovereignty__expansive_construction_reading, neighboring_claimant_state).
narrative_ontology:constraint_victim(unclos_maritime_sovereignty__expansive_construction_reading, freedom_of_navigation_state).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Invests in dredging, land reclamation, and construction on submerged features or low-tide elevations within claimed maritime zones. Asserts that effective occupation and administrative control (installation of lighthouses, civil administration, resource extraction) transform the feature into an island generating a 12nm territorial sea and up to 200nm EEZ. Views this as a legitimate exercise of sovereignty within waters it already claims. Justifies the practice as resource security and maritime development.
narrative_ontology:constraint_stakeholder(unclos_maritime_sovereignty__expansive_construction_reading, island_constructing_state, agenda_setter,
    institutional, generational, mobile, regional).

% Faces contraction of navigable waters and fishing zones as the constructing state expands its claimed territorial sea through artificial island construction. May hold overlapping claims to the same submerged features based on alternative maritime delimitation principles or historical occupation. Constrained exit: withdrawal from the region is not feasible; only option is prolonged diplomatic negotiation or military posturing.
narrative_ontology:constraint_stakeholder(unclos_maritime_sovereignty__expansive_construction_reading, neighboring_claimant_state, payer,
    institutional, generational, constrained, regional).

% Operates merchant and military vessels in international waters and relies on high-seas navigation rights through straits and archipelagic waters. Artificial island construction shrinks the legal high-seas corridor and enlarges areas where the constructing state can enforce domestic law on foreign vessels. Freedom-of-navigation states object to the sovereignty claim but lack enforcement power in the region; constrained to diplomatic protest or freedom-of-navigation operations (risky and escalatory).
narrative_ontology:constraint_stakeholder(unclos_maritime_sovereignty__expansive_construction_reading, freedom_of_navigation_state, payer,
    institutional, biographical, constrained, global).

% Fish in waters claimed by the island-constructing state's expanded EEZ but located far from its home ports. Lack capital or political standing to challenge the sovereignty claim. Face arrest, fines, and confiscation if vessels cross into the expanded 200nm zone. No viable alternative fishing grounds; trapped by geography and economic dependence.
narrative_ontology:constraint_stakeholder(unclos_maritime_sovereignty__expansive_construction_reading, distant_fishing_communities, payer,
    powerless, biographical, trapped, regional).

% Operates mineral extraction, oil/gas drilling, or aquaculture within the expanded EEZ generated by artificial island construction. Gains exclusive economic rights and cost-free territory from the constructing state's assertion of sovereignty. Benefits from suppressed competition and stable (though contested) property claims.
narrative_ontology:constraint_stakeholder(unclos_maritime_sovereignty__expansive_construction_reading, domestic_resource_extraction_entity, beneficiary,
    powerful, generational, arbitrage, regional).

% Consists of legal scholars, treaty bodies (UN Tribunal for the Law of the Sea, International Court of Justice), and UNCLOS signatories tasked with interpreting and adjudicating maritime boundary disputes. Observes the tension between the expansive reading (effective occupation generates sovereignty) and the strict reading (only natural features qualify).
narrative_ontology:constraint_stakeholder(unclos_maritime_sovereignty__expansive_construction_reading, international_maritime_law_community, observer,
    analytical, generational, analytical, global).

% Administers UNCLOS but lacks enforcement power to compel compliance with any single reading of the artificial-island question. Would argue that artificial features do not qualify as islands under Article 121 UNCLOS unless explicitly amended, but is structurally sidelined from the dispute resolution process.
narrative_ontology:constraint_stakeholder(unclos_maritime_sovereignty__expansive_construction_reading, unclos_convention_secretariat, excluded,
    institutional, generational, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(unclos_maritime_sovereignty__expansive_construction_reading, island_constructing_state).
narrative_ontology:fixing_cost_class(unclos_maritime_sovereignty__expansive_construction_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a legal mechanism by which states can claim maritime territory through effective occupation and administrative control of submerged or low-tide features, reducing ambiguity about who controls contested waters and providing a framework (however contested) for resource allocation.
% TRANSFER_FUNCTION: Transfers access to fishing, shipping, mineral extraction, and strategic positioning rights from the regional commons (high-seas, EEZ of neutral states, or overlapping-claim areas) to the constructing state's exclusive economic and territorial control, concentrating these rights where they were previously diffuse or shared.
% ABSENT_VOICES: Small island states that lack capital for artificial construction; landlocked states with no maritime frontier; indigenous or diaspora communities whose historical fishing grounds are submerged and now claimed under the expanded jurisdiction. They would argue that artificial construction is fabricating sovereignty rather than discovering it, but their objections are not seated at the negotiating table.
% DISAPPEARANCE_RATIONALE: If the expansive reading and its enforcement mechanisms vanished overnight, the constructing state would lose de facto control of the expanded waters within weeks (vessels from rival claimants and freedom-of-navigation states would operate there unchallenged), the state's extraction entities would lose exclusive resource access, and regional maritime boundaries would revert to the strict-reading baseline or the status quo ante the construction began.
% FOUNDING_PROBLEM: Small, vulnerable states sought a mechanism to extend their maritime jurisdiction and secure access to fish stocks, oil reserves, and strategic depth without depending on powerful neighbors' goodwill. Artificial island construction on features within claimed waters offered a sovereign act: occupation and administration that assert the state's control and preempt rival claimants.
% FOUNDING_PROBLEM_CORROBORATION: The constructing state attests the problem remains live: insecurity about regional maritime resources and vulnerability to larger neighbors. Neighboring claimant states and freedom-of-navigation powers attest the original problem (resource insecurity) has been superseded by the new problem (enforced sovereignty shrinking navigable waters and stealing disputed resources). Freedom-of-navigation states and distant fishing communities attest the practice has become extractive, not protective. Legal scholarship from outside the benefiting parties (e.g., ITLOS judges, non-claimant states) documents the shift from security mechanism to territorial expansion tool.
narrative_ontology:disappearance_verdict(unclos_maritime_sovereignty__expansive_construction_reading, world_rearranges).
narrative_ontology:founding_problem_status(unclos_maritime_sovereignty__expansive_construction_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(unclos_maritime_sovereignty__expansive_construction_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(unclos_maritime_sovereignty__expansive_construction_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(unclos_maritime_sovereignty__expansive_construction_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(unclos_maritime_sovereignty__expansive_construction_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(unclos_maritime_sovereignty__expansive_construction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.81) and rising over the interval (0.48→0.81) because the constructing state expands its claimed waters progressively, concentrating economic and strategic rents that were previously diffuse or shared. Suppression is substantial (0.72) and rising because enforcement requires coast guard operations, vessel interception, and diplomatic intimidation to prevent rival claimants and freedom-of-navigation states from exercising their preferred rights in the expanded waters. Theater ratio is moderate (0.28) and stable because the operational reality (resource extraction, military positioning, shipping restrictions) matches the stated justification (sovereign occupation and administrative control) closely enough that performative maintenance is not the dominant cost — the constraint is doing what it claims. Accessibility collapse is moderate (0.65) because alternatives (navigable corridors, shared fishing grounds, unresolved boundaries) do exist but are shrinking as construction facts on the ground accumulate; resistance is moderate (0.58) because neighboring states object loudly but lack force to eject the constructed features and freedom-of-navigation states run occasional 'transit passage' operations that cost them politically but do not alter the enforcement baseline. The measurement series trace one shared time grid; every metric is authored at every time point in the interval.
 *
 * PERSPECTIVAL GAP:
 *   The constructing state perceives the constraint as sovereignty protection; the payer states perceive it as opportunistic expansion. These are incommensurable framings rooted in geographic position and power. The agenda-setter's computation will likely yield 'rope' or 'mountain' (natural exercise of sovereign rights within claimed waters), while the payer computations will yield 'snare' or 'tangled_rope' (extraction backed by effective occupation and suppression of alternatives). The engine captures this via directionality differences: the beneficiary gets low d (low effective extraction, high subsidy effect), the target gets high d (high effective extraction, suppression amplified). This per-seat divergence is the measurement the corpus is designed to take.
 *
 * DIRECTIONALITY LOGIC:
 *   The constructing state is the beneficiary and agenda-setter: it sets the terms, controls the islands, collects the rents (d → beneficiary end, near 0.0). Neighboring claimant states are victims and payers: they lose waters and resources, constrained exit (d → target end, near 1.0). Freedom-of-navigation states are payers but have higher structural power and global reach (d → target but dampened by arbitrage capacity to route around). Distant fishing communities are trapped victims with zero power (d → full target end, 1.0). Domestic extraction entities benefit (d → beneficiary end). The directionality profile is asymmetric by design: the constraint transfers from many weak and some strong targets to one institutional beneficiary. No directionality override is needed; the structural derivation captures this.
 *
 * MANDATROPHY ANALYSIS:
 *   Mandatrophy is present: the founding problem (regional maritime insecurity, resource access guarantee) would be classified as 'live' by the constructing state and 'dead' by the neighboring claimants and freedom-of-navigation states. The constraint persists because it generates rents for the beneficiaries and costs for the payers; the foundational coordination function (stabilizing maritime boundaries) has atrophied and been replaced by coordination among the beneficiaries on how to maintain the expansion. This is a tangled_rope that has degraded toward snare: the coordination component (clarity about who controls which waters) is real but subsidiary; the extraction component (concentrating access and rents) dominates. The remedy would require either renegotiation of the maritime boundary (undoing the artificial islands' legal effect) or acceptance of the new status quo. The constraint persists because the beneficiary is powerful enough to defend it and the payers lack coalition power to overturn it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_vs_constructed_island_status,
    'Is UNCLOS Article 121''s definition of ''island'' a geographic threshold (naturally formed, above water at high tide) or a functional threshold (any feature with effective administrative control and economic viability)?',
    'ITLOS advisory opinion or contentious case adjudication explicitly parsing Article 121 against artificial features, or amendment to UNCLOS clarifying the term. The reading instantiated here assumes functional interpretation; the strict reading assumes geographic threshold.',
    'If geographic threshold prevails, this constraint reclassifies as snare (artificial island claims are null, the constraint is pure extraction masked as sovereignty). If functional threshold prevails, the constraint stands as tangled_rope (coordination of boundary-setting plus asymmetric extraction). If the threshold remains ambiguous (most likely), the constraint persists in limbo with its type oscillating between readings.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(natural_vs_constructed_island_status, conceptual, 'The contested kernel: does island status depend on geographic naturalness or functional control?').

omega_variable(
    effective_occupation_evidentiary_standard,
    'What constitutes ''effective occupation and administrative control'' sufficient to bind neighboring states and freedom-of-navigation states? Presence of lighthouses? Coast guard patrols? Civilian settlement? Extraction activity? A threshold below which claims are merely performative?',
    'Case law accumulation from ITLOS, ICJ, and regional tribunals; comparison of accepted and disputed island claims to extract the evidentiary standard.',
    'A high threshold (permanent settlement, full civilian administration) would reduce the number of valid artificial-island claims; a low threshold (lighthouse and flag) would enable the expansive reading to proliferate. This affects whether the constraint''s extractiveness grows or stabilizes.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(effective_occupation_evidentiary_standard, empirical, 'What factual conditions suffice for effective occupation claim to bind third states?').

omega_variable(
    suppression_mechanisms_as_coordination,
    'Do coast guard operations, vessel interdiction, and diplomatic intimidation necessary to enforce the expanded sovereignty claim count as suppression (payer-side cost) or as legitimate law enforcement (coordination cost of establishing and maintaining the zone)?',
    'Post-dispute adjudication: if the tribunal sides with the constructing state, the enforcement costs reframe as legitimate; if it sides with the claimants, the same costs reframe as suppression. Absent adjudication, the framing remains contested.',
    'If framed as coordination cost, the constraint''s effective suppression score lowers and the type tilts toward rope. If framed as suppression, the constraint is confirmed as tangled_rope with substantial coercive overhead. This is a reading-dependent ambiguity — the strict reading calls the operations suppression; the expansive reading calls them law enforcement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanisms_as_coordination, conceptual, 'Whether enforcement operations are suppression or coordination cost depends on which reading''s premises you accept.').

omega_variable(
    coalition_power_of_constrained_payers,
    'Can neighboring claimant states and freedom-of-navigation states form a coalition powerful enough to impose costs on the constructing state (economic sanctions, military countermeasures, diplomatic isolation) that would make the constraint unsustainable?',
    'Historical test: precedent from similar regional disputes (Spratly Islands, Crimea annexation, Golan Heights) showing whether coalitions can reverse fait accompli.',
    'If coalition power is substantial, the constraint''s long-term persistence is uncertain and its type may shift as enforcement capability changes. If coalition power is negligible, the constraint is entrenched and its extraction will remain concentrated. This affects whether the constraint matures toward snare (uncontested extraction) or remains tangled_rope (contested but enforceable).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coalition_power_of_constrained_payers, empirical, 'Do constrained payers have latent coalition power to reverse the construction and enforcement?').

omega_variable(
    kernel_reading_foreclosure_logic,
    'This reading (expansive construction) asserts that the strict geographic reading is logically foreclosed — if effective occupation generates island status, then geographic naturalness is irrelevant. But the hybrid reading coexists: it assigns different weight to artificial vs. natural features while accepting occupation doctrine. Does the expansive reading''s axiom actually foreclose the strict reading, or do they represent incommensurable framings that coexist in law even if one would logically dominate in a coherent framework?',
    'Jurisprudential analysis of whether a single legal framework can hold both readings in simultaneous validity (yes → they coexist), or whether one framework necessarily rules out the other (yes → foreclosure holds).',
    'If foreclosure is real, the strict reading is a zombie claim maintained by states that reject UNCLOS occupation doctrine; if they coexist, the kernel contains a fundamental ambiguity that no single adjudication can resolve. This affects whether the constraint''s type is stable or oscillates as different forums apply different readings.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_foreclosure_logic, conceptual, 'Does this reading logically foreclose the strict reading, or do they coexist as incommensurable framings?').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(unclos_maritime_sovereignty__expansive_construction_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(uncl_tr_t0, unclos_maritime_sovereignty__expansive_construction_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement_basis(uncl_tr_t0, observed).
narrative_ontology:measurement(uncl_tr_t4, unclos_maritime_sovereignty__expansive_construction_reading, theater_ratio, 4, 0.23).
narrative_ontology:measurement_basis(uncl_tr_t4, observed).
narrative_ontology:measurement(uncl_tr_t8, unclos_maritime_sovereignty__expansive_construction_reading, theater_ratio, 8, 0.25).
narrative_ontology:measurement_basis(uncl_tr_t8, observed).
narrative_ontology:measurement(uncl_tr_t12, unclos_maritime_sovereignty__expansive_construction_reading, theater_ratio, 12, 0.27).
narrative_ontology:measurement_basis(uncl_tr_t12, observed).
narrative_ontology:measurement(uncl_tr_t17, unclos_maritime_sovereignty__expansive_construction_reading, theater_ratio, 17, 0.29).
narrative_ontology:measurement_basis(uncl_tr_t17, observed).
narrative_ontology:measurement(uncl_tr_t25, unclos_maritime_sovereignty__expansive_construction_reading, theater_ratio, 25, 0.28).
narrative_ontology:measurement_basis(uncl_tr_t25, observed).

% Extraction over time
narrative_ontology:measurement(uncl_be_t0, unclos_maritime_sovereignty__expansive_construction_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement_basis(uncl_be_t0, observed).
narrative_ontology:measurement(uncl_be_t4, unclos_maritime_sovereignty__expansive_construction_reading, base_extractiveness, 4, 0.54).
narrative_ontology:measurement_basis(uncl_be_t4, observed).
narrative_ontology:measurement(uncl_be_t8, unclos_maritime_sovereignty__expansive_construction_reading, base_extractiveness, 8, 0.61).
narrative_ontology:measurement_basis(uncl_be_t8, observed).
narrative_ontology:measurement(uncl_be_t12, unclos_maritime_sovereignty__expansive_construction_reading, base_extractiveness, 12, 0.69).
narrative_ontology:measurement_basis(uncl_be_t12, observed).
narrative_ontology:measurement(uncl_be_t17, unclos_maritime_sovereignty__expansive_construction_reading, base_extractiveness, 17, 0.76).
narrative_ontology:measurement_basis(uncl_be_t17, observed).
narrative_ontology:measurement(uncl_be_t25, unclos_maritime_sovereignty__expansive_construction_reading, base_extractiveness, 25, 0.81).
narrative_ontology:measurement_basis(uncl_be_t25, observed).

% Suppression requirement over time
narrative_ontology:measurement(uncl_su_t0, unclos_maritime_sovereignty__expansive_construction_reading, suppression_requirement, 0, 0.48).
narrative_ontology:measurement_basis(uncl_su_t0, observed).
narrative_ontology:measurement(uncl_su_t4, unclos_maritime_sovereignty__expansive_construction_reading, suppression_requirement, 4, 0.54).
narrative_ontology:measurement_basis(uncl_su_t4, observed).
narrative_ontology:measurement(uncl_su_t8, unclos_maritime_sovereignty__expansive_construction_reading, suppression_requirement, 8, 0.6).
narrative_ontology:measurement_basis(uncl_su_t8, observed).
narrative_ontology:measurement(uncl_su_t12, unclos_maritime_sovereignty__expansive_construction_reading, suppression_requirement, 12, 0.66).
narrative_ontology:measurement_basis(uncl_su_t12, observed).
narrative_ontology:measurement(uncl_su_t17, unclos_maritime_sovereignty__expansive_construction_reading, suppression_requirement, 17, 0.7).
narrative_ontology:measurement_basis(uncl_su_t17, observed).
narrative_ontology:measurement(uncl_su_t25, unclos_maritime_sovereignty__expansive_construction_reading, suppression_requirement, 25, 0.72).
narrative_ontology:measurement_basis(uncl_su_t25, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(unclos_maritime_sovereignty__expansive_construction_reading, global_infrastructure).
narrative_ontology:boltzmann_floor_override(unclos_maritime_sovereignty__expansive_construction_reading, 0.18).
narrative_ontology:affects_constraint(unclos_maritime_sovereignty__expansive_construction_reading, unclos_maritime_sovereignty__strict_geographic_reading).
narrative_ontology:affects_constraint(unclos_maritime_sovereignty__expansive_construction_reading, unclos_maritime_sovereignty__hybrid_effective_control_reading).
narrative_ontology:affects_constraint(unclos_maritime_sovereignty__expansive_construction_reading, south_china_sea_freedom_of_navigation_operations).
narrative_ontology:affects_constraint(unclos_maritime_sovereignty__expansive_construction_reading, regional_eez_delimitation_disputes).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the contested kernel unclos_maritime_sovereignty. The strict_geographic_reading and hybrid_effective_control_reading are sibling constraints capturing alternative interpretations of the same UNCLOS text. All three are linked via network.affects_constraints because they compete to define the same maritime boundary question. The divergence in their ε values (expansive reading ε~0.81 high extractiveness; strict reading ε~0.12 minimal extractiveness; hybrid reading ε~0.42 moderate) reflects the reading-dependent framing of what 'island' means and whether artificial features qualify.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
