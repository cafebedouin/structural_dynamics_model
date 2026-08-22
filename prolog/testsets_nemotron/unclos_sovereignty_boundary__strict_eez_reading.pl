% ============================================================================
% CONSTRAINT STORY: unclos_sovereignty_boundary__strict_eez_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-04
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_unclos_sovereignty_boundary__strict_eez_reading, []).

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
 *   constraint_id: unclos_sovereignty_boundary__strict_eez_reading
 *   human_readable: UNCLOS Article 57 Strict EEZ Boundary Enforcement
 *   domain: international_law/maritime_governance/geopolitical_strategy
 *
 * SUMMARY:
 *   This constraint story captures the strict EEZ reading of the UNCLOS
 *   sovereignty boundary kernel: Article 57's 200-nautical-mile exclusive
 *   economic zone is the exclusive legal title for coastal state resource
 *   rights; no historical, customary, or equitable overlay claims are valid.
 *   The constraint operates as a tangled rope — it solves a genuine
 *   coordination problem (allocating ocean space, preserving navigation
 *   freedoms, establishing dispute settlement) while simultaneously
 *   extracting from overlapping claimants and traditional users through
 *   active enforcement of a boundary that privileges the treaty's geometric
 *   rule over lived maritime practice. The coordinate extraction is the legal
 *   exclusion of non-geometric claims; the suppression is the dispute
 *   settlement machinery and naval enforcement that make the boundary stick.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(unclos_sovereignty_boundary__strict_eez_reading, 0.62).
domain_priors:suppression_score(unclos_sovereignty_boundary__strict_eez_reading, 0.78).
domain_priors:theater_ratio(unclos_sovereignty_boundary__strict_eez_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(unclos_sovereignty_boundary__strict_eez_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(unclos_sovereignty_boundary__strict_eez_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(unclos_sovereignty_boundary__strict_eez_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(unclos_sovereignty_boundary__strict_eez_reading, accessibility_collapse, 0.71).
narrative_ontology:constraint_metric(unclos_sovereignty_boundary__strict_eez_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(unclos_sovereignty_boundary__strict_eez_reading, tangled_rope).
narrative_ontology:human_readable(unclos_sovereignty_boundary__strict_eez_reading, "UNCLOS Article 57 Strict EEZ Boundary Enforcement").
narrative_ontology:topic_domain(unclos_sovereignty_boundary__strict_eez_reading, "international_law/maritime_governance/geopolitical_strategy").

domain_priors:requires_active_enforcement(unclos_sovereignty_boundary__strict_eez_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(unclos_sovereignty_boundary__strict_eez_reading, '6fb72f86-5479-4a81-9a0b-2b20c01993f4').
narrative_ontology:cs_kernel_codification('6fb72f86-5479-4a81-9a0b-2b20c01993f4', formalized).
narrative_ontology:cs_authority_grounding('6fb72f86-5479-4a81-9a0b-2b20c01993f4', lineage).
narrative_ontology:cs_interpretation_layer_present('6fb72f86-5479-4a81-9a0b-2b20c01993f4').
narrative_ontology:cs_reading_relation('6fb72f86-5479-4a81-9a0b-2b20c01993f4', unclos_sovereignty_boundary__historical_rights_reading, forecloses).
narrative_ontology:cs_reading_relation('6fb72f86-5479-4a81-9a0b-2b20c01993f4', unclos_sovereignty_boundary__non_ratifier_enforcement_reading, coexists_with).
narrative_ontology:cs_axiom('6fb72f86-5479-4a81-9a0b-2b20c01993f4', foundational, eez_boundary_exclusivity_is_treaty_law).
narrative_ontology:cs_axiom_status(eez_boundary_exclusivity_is_treaty_law, holdable).
narrative_ontology:cs_axiom_grounding('6fb72f86-5479-4a81-9a0b-2b20c01993f4', eez_boundary_exclusivity_is_treaty_law, conventional).
narrative_ontology:cs_axiom('6fb72f86-5479-4a81-9a0b-2b20c01993f4', foundational, historical_claims_cannot_override_unclos_geometry).
narrative_ontology:cs_axiom_status(historical_claims_cannot_override_unclos_geometry, holdable).
narrative_ontology:cs_axiom_grounding('6fb72f86-5479-4a81-9a0b-2b20c01993f4', historical_claims_cannot_override_unclos_geometry, conventional).
narrative_ontology:cs_reference_frame('6fb72f86-5479-4a81-9a0b-2b20c01993f4', unclos_package_deal_1982).
narrative_ontology:cs_drift_state('6fb72f86-5479-4a81-9a0b-2b20c01993f4', post_arbitration_era_2016, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('6fb72f86-5479-4a81-9a0b-2b20c01993f4', '').
narrative_ontology:cs_kernel_id(unclos_sovereignty_boundary__strict_eez_reading, unclos_sovereignty_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(unclos_sovereignty_boundary__strict_eez_reading, coastal_states_with_clear_eez).
narrative_ontology:constraint_beneficiary(unclos_sovereignty_boundary__strict_eez_reading, flag_states_operating_in_distant_waters).
narrative_ontology:constraint_beneficiary(unclos_sovereignty_boundary__strict_eez_reading, seabed_authority_isa).
narrative_ontology:constraint_victim(unclos_sovereignty_boundary__strict_eez_reading, overlapping_claimant_states).
narrative_ontology:constraint_victim(unclos_sovereignty_boundary__strict_eez_reading, traditional_fishing_communities_in_disputed_zones).
narrative_ontology:constraint_victim(unclos_sovereignty_boundary__strict_eez_reading, non_ratifier_navies_asserting_freedom_of_navigation).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(unclos_sovereignty_boundary__strict_eez_reading, flag_states_operating_in_distant_waters).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold exclusive sovereign rights to explore, exploit, conserve, and manage all natural resources within 200nm of their baselines. They license extraction, collect royalties, and enforce environmental regulations. Their position is backed by UNCLOS treaty law and the dispute settlement system. Exit means abandoning the legal framework that secures their resource claims — they have arbitrage-grade exit into the treaty system itself.
narrative_ontology:constraint_stakeholder(unclos_sovereignty_boundary__strict_eez_reading, coastal_states_with_clear_eez, beneficiary,
    institutional, generational, arbitrage, national).

% Benefit from the legal certainty that EEZ boundaries provide for high seas freedoms (navigation, overflight, cable-laying) outside the 200nm limit. They pay through compliance costs and licensing fees when operating inside coastal state EEZs. Their fleets can relocate; they have mobile exit options across ocean basins.
narrative_ontology:constraint_stakeholder(unclos_sovereignty_boundary__strict_eez_reading, flag_states_operating_in_distant_waters, beneficiary,
    powerful, biographical, mobile, global).
narrative_ontology:stakeholder_secondary_role(unclos_sovereignty_boundary__strict_eez_reading, flag_states_operating_in_distant_waters, payer).

% Administers the Area (seabed beyond national jurisdiction) and the parallel regime for the extended continental shelf. The ISA's authority is constituted by the same UNCLOS framework that defines EEZ boundaries — it sets the rules for where national jurisdiction ends and the common heritage of mankind begins. It does not collect rents from EEZs but its mandate depends on the boundary being respected.
narrative_ontology:constraint_stakeholder(unclos_sovereignty_boundary__strict_eez_reading, seabed_authority_isa, agenda_setter,
    institutional, generational, analytical, universal).

% States whose claimed maritime zones overlap with neighbors' EEZs under UNCLOS Article 57 (e.g., China in the South China Sea, Turkey in the Eastern Mediterranean, overlapping continental shelf claims in the Arctic). They lose access to resources and strategic space when the strict 200nm rule is enforced. Their exit options are constrained: negotiation, arbitration (which they often reject), or unilateral assertion backed by force.
narrative_ontology:constraint_stakeholder(unclos_sovereignty_boundary__strict_eez_reading, overlapping_claimant_states, payer,
    powerful, generational, constrained, regional).

% Small-scale fishers whose traditional grounds straddle EEZ boundaries or lie in disputed zones. They have no effective voice in treaty negotiations or dispute settlement. When boundaries are enforced, they are excluded from historic fishing areas without compensation. Exit is trapped — they lack capital, alternative livelihoods, or political representation.
narrative_ontology:constraint_stakeholder(unclos_sovereignty_boundary__strict_eez_reading, traditional_fishing_communities_in_disputed_zones, payer,
    powerless, biographical, trapped, local).

% Naval powers (notably the United States) that treat freedom of navigation as customary international law independent of UNCLOS ratification. They view strict EEZ enforcement as a threat to operational freedom, especially regarding military surveys and transit through straits. They are excluded from the UNCLOS dispute settlement system by non-ratification but project power to maintain access. Their exit is mobile — they operate globally and choose when to challenge.
narrative_ontology:constraint_stakeholder(unclos_sovereignty_boundary__strict_eez_reading, non_ratifier_navies_asserting_freedom_of_navigation, payer,
    powerful, biographical, mobile, global).
narrative_ontology:stakeholder_secondary_role(unclos_sovereignty_boundary__strict_eez_reading, non_ratifier_navies_asserting_freedom_of_navigation, excluded).

% Analyze the coherence of the UNCLOS regime, the legitimacy of dispute settlement outcomes, and the tension between treaty law and customary law. They do not collect rents or bear enforcement costs; their product is interpretive frameworks that influence how states and tribunals understand the constraint.
narrative_ontology:constraint_stakeholder(unclos_sovereignty_boundary__strict_eez_reading, international_legal_scholars, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a globally recognized, legally stable framework for allocating ocean space and resources among states, replacing the chaotic freedom-of-the-seas regime with defined zones of national jurisdiction and preserved high seas freedoms.
% TRANSFER_FUNCTION: Moves exclusive resource rights (fisheries, hydrocarbons, minerals, genetic resources) from the global commons into the sovereign control of coastal states within 200nm, while transferring the burden of enforcement and environmental stewardship to those same states. Overlapping claimants lose access; traditional users lose customary access without compensation.
% ABSENT_VOICES: Indigenous coastal communities with pre-colonial maritime territories not recognized by the state-centric UNCLOS framework; small island developing states whose EEZs are disproportionately large relative to their capacity to enforce; non-state actors (NGOs, scientific networks) who would advocate for stronger conservation but lack standing in dispute settlement.
% DISAPPEARANCE_RATIONALE: If the strict EEZ boundary constraint vanished overnight, coastal states would lose legal title to trillions in seabed and water-column resources; overlapping claimants would escalate unilateral assertions and militarization; the ISA would lose its jurisdictional anchor for the Area; global shipping and cable routing would face legal uncertainty. The entire post-1994 ocean governance architecture would reorganize around power rather than law.
% FOUNDING_PROBLEM: The pre-UNCLOS regime failed to prevent escalating conflict over offshore resources (oil, fish, manganese nodules) and left the high seas open to unregulated exploitation. States needed a comprehensive legal framework to allocate jurisdiction, preserve navigation freedoms, and establish dispute resolution.
% FOUNDING_PROBLEM_CORROBORATION: The UNCLOS negotiating record (1973-1982) and the 1994 Implementation Agreement attest the founding problem. Coastal states and the ISA attest it remains live (new resources, climate change, BBNJ treaty). Overlapping claimants and non-ratifiers attest the problem has shifted: the constraint now serves as a tool for powerful states to lock in advantage rather than manage common resources. Independent legal scholarship (e.g., Rothwell, Stephens, Harrison) corroborates the shifted-function reading from outside the beneficiary set.
narrative_ontology:disappearance_verdict(unclos_sovereignty_boundary__strict_eez_reading, world_rearranges).
narrative_ontology:founding_problem_status(unclos_sovereignty_boundary__strict_eez_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(unclos_sovereignty_boundary__strict_eez_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(unclos_sovereignty_boundary__strict_eez_reading, 'none', 1).
narrative_ontology:epsilon_provenance(unclos_sovereignty_boundary__strict_eez_reading, 0.62, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(unclos_sovereignty_boundary__strict_eez_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(unclos_sovereignty_boundary__strict_eez_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(unclos_sovereignty_boundary__strict_eez_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness (0.62) reflects the massive transfer of resource wealth from commons to coastal states, concentrated in states with large coastlines and broad shelves. The victims (overlapping claimants, traditional fishers, non-ratifier navies) bear real costs: lost access, criminalized livelihoods, operational constraints. Suppression (0.78) is high because the constraint's persistence depends on the UNCLOS Part XV dispute settlement system, ITLOS arbitration, and the naval presence of beneficiary states — alternatives (historical rights, customary freedom of navigation) are actively suppressed as legally invalid. Theater ratio (0.22) is low-moderate: the coordination function (legal certainty, resource management, environmental protection) is real and actively performed, but a growing share of enforcement energy defends the boundary against challengers rather than managing the commons. Accessibility collapse (0.71) is high because once a state ratifies UNCLOS, the EEZ boundary becomes the only legally recognized framework — alternative sovereignty claims collapse legally even if they persist politically. Resistance (0.58) is substantial: China's nine-dash line, Turkey's Blue Homeland, US FONOPS, and the non-ratification of UNCLOS by the US are active resistance to the strict reading.
 *
 * PERSPECTIVAL GAP:
 *   From the coastal state/ISA seat, the constraint is a rope — it coordinates ocean governance with minimal coercion relative to the alternative of conflict. From the overlapping claimant seat, it is a snare — the coordination story covers the geometric expropriation of their claimed space. From the traditional fisher seat, it is a snare with no coordination benefit at all. The engine computes this divergence from the declared beneficiaries, victims, and exit options; the claimed_type (tangled_rope) represents the authoring seat's structural judgment that both functions are genuinely present.
 *
 * DIRECTIONALITY LOGIC:
 *   Coastal states with clear EEZs are full beneficiaries (d near 0.0) — they collect the resource rents, set the licensing terms, and control the enforcement agenda within their zones. Flag states are near-symmetric (d ~0.5): they gain legal certainty on the high seas but pay compliance costs inside EEZs. The ISA is an analytical/agenda-setting seat (d ~0.0 for its mandate, but it collects no rents). Overlapping claimants are full targets (d near 1.0) — the constraint's geometry directly negates their claims. Traditional fishers are trapped targets (d = 1.0, identity_locked by livelihood and place). Non-ratifier navies are mobile targets (d ~0.8) — they bear operational costs but retain power to contest. The engine will compute per-seat effective extraction from these structural positions.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (preventing resource conflict, managing the commons) remains live in principle but the strict EEZ reading has become a tool for locking in the advantage of early ratifiers and geographically favored states. The constraint now extracts from latecomers, non-ratifiers, and non-state users while its coordination function degrades under climate-driven boundary shifts (shifting stocks, rising seas) and strategic competition. Mandatrophy is unresolved: the arrangement persists because the beneficiaries control the amendment and interpretation machinery, not because the founding problem is solved.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    strict_reading_vs_customary_law_gap,
    'Does the strict EEZ reading foreclose customary international law claims that predate UNCLOS, or does customary law survive as a parallel regime that the strict reading merely influences?',
    'ICJ/ITLOS jurisprudence on the relationship between UNCLOS and customary law (e.g., Nicaragua v. USA, Chagos Marine Protected Area arbitration). Track whether tribunals treat UNCLOS as exhaustive or as coexisting with customary rights.',
    'If customary law survives, the strict reading''s suppression is overstated — overlapping claimants retain a legal pathway. If UNCLOS is exhaustive, the strict reading''s suppression is structurally complete and the constraint is closer to snare for non-parties.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(strict_reading_vs_customary_law_gap, conceptual, 'Whether the strict EEZ reading legally forecloses or merely politically marginalizes customary law alternatives.').

omega_variable(
    climate_boundary_drift,
    'As sea-level rise shifts baselines and moves the 200nm line landward, does the strict reading''s coordinate geometry become a source of new extraction (coastal states lose EEZ area) or does the regime adapt through the extended continental shelf mechanism?',
    'Monitor ILC work on sea-level rise and maritime boundaries, state practice on baseline maintenance (Art. 5/7 UNCLOS), and CLCS recommendations on continental shelf limits beyond 200nm.',
    'If baselines are ambulatory, coastal states become victims of their own geometry — extractiveness inverts. If baselines are fixed, the constraint''s coordination function degrades as physical reality diverges from legal geometry, increasing theater ratio.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(climate_boundary_drift, empirical, 'Whether the strict reading''s geometric rigidity becomes maladaptive under climate change.').

omega_variable(
    kernel_framing_alternatives,
    'Is the unclos_sovereignty_boundary kernel best framed as a single commitment to ''UNCLOS as constitution for the oceans,'' or as a bundle of semi-independent commitments (EEZ regime, deep seabed regime, navigation regime, dispute settlement) that can be read separately?',
    'Analyze whether states ratify, implement, and invoke UNCLOS as a package or selectively. Track the BBNJ treaty (Biodiversity Beyond National Jurisdiction) as a test: does it amend the kernel or operate alongside it?',
    'If the kernel is a bundle, the strict_eez_reading is only one component and its extraction/suppression profile applies only to the EEZ sub-regime. If the kernel is unitary, the strict reading''s enforcement machinery protects the entire UNCLOS package, increasing its structural weight.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_framing_alternatives, conceptual, 'Whether the UNCLOS kernel is a unitary commitment or a modular bundle — affects how sibling readings relate.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(unclos_sovereignty_boundary__strict_eez_reading, 1994, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(uncl_tr_t1994, unclos_sovereignty_boundary__strict_eez_reading, theater_ratio, 1994, 0.1).
narrative_ontology:measurement(uncl_tr_t2000, unclos_sovereignty_boundary__strict_eez_reading, theater_ratio, 2000, 0.13).
narrative_ontology:measurement(uncl_tr_t2005, unclos_sovereignty_boundary__strict_eez_reading, theater_ratio, 2005, 0.16).
narrative_ontology:measurement(uncl_tr_t2010, unclos_sovereignty_boundary__strict_eez_reading, theater_ratio, 2010, 0.18).
narrative_ontology:measurement(uncl_tr_t2015, unclos_sovereignty_boundary__strict_eez_reading, theater_ratio, 2015, 0.2).
narrative_ontology:measurement(uncl_tr_t2020, unclos_sovereignty_boundary__strict_eez_reading, theater_ratio, 2020, 0.21).
narrative_ontology:measurement(uncl_tr_t2024, unclos_sovereignty_boundary__strict_eez_reading, theater_ratio, 2024, 0.22).

% Extraction over time
narrative_ontology:measurement(uncl_be_t1994, unclos_sovereignty_boundary__strict_eez_reading, base_extractiveness, 1994, 0.35).
narrative_ontology:measurement(uncl_be_t2000, unclos_sovereignty_boundary__strict_eez_reading, base_extractiveness, 2000, 0.42).
narrative_ontology:measurement(uncl_be_t2005, unclos_sovereignty_boundary__strict_eez_reading, base_extractiveness, 2005, 0.48).
narrative_ontology:measurement(uncl_be_t2010, unclos_sovereignty_boundary__strict_eez_reading, base_extractiveness, 2010, 0.52).
narrative_ontology:measurement(uncl_be_t2015, unclos_sovereignty_boundary__strict_eez_reading, base_extractiveness, 2015, 0.57).
narrative_ontology:measurement(uncl_be_t2020, unclos_sovereignty_boundary__strict_eez_reading, base_extractiveness, 2020, 0.6).
narrative_ontology:measurement(uncl_be_t2024, unclos_sovereignty_boundary__strict_eez_reading, base_extractiveness, 2024, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(uncl_su_t1994, unclos_sovereignty_boundary__strict_eez_reading, suppression_requirement, 1994, 0.55).
narrative_ontology:measurement(uncl_su_t2000, unclos_sovereignty_boundary__strict_eez_reading, suppression_requirement, 2000, 0.62).
narrative_ontology:measurement(uncl_su_t2005, unclos_sovereignty_boundary__strict_eez_reading, suppression_requirement, 2005, 0.68).
narrative_ontology:measurement(uncl_su_t2010, unclos_sovereignty_boundary__strict_eez_reading, suppression_requirement, 2010, 0.72).
narrative_ontology:measurement(uncl_su_t2015, unclos_sovereignty_boundary__strict_eez_reading, suppression_requirement, 2015, 0.75).
narrative_ontology:measurement(uncl_su_t2020, unclos_sovereignty_boundary__strict_eez_reading, suppression_requirement, 2020, 0.77).
narrative_ontology:measurement(uncl_su_t2024, unclos_sovereignty_boundary__strict_eez_reading, suppression_requirement, 2024, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(unclos_sovereignty_boundary__strict_eez_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(unclos_sovereignty_boundary__strict_eez_reading, 0.15).
narrative_ontology:affects_constraint(unclos_sovereignty_boundary__strict_eez_reading, unclos_deep_seabed_regime).
narrative_ontology:affects_constraint(unclos_sovereignty_boundary__strict_eez_reading, unclos_navigation_regime).
narrative_ontology:affects_constraint(unclos_sovereignty_boundary__strict_eez_reading, unclos_dispute_settlement_part_xv).
narrative_ontology:affects_constraint(unclos_sovereignty_boundary__strict_eez_reading, bbnj_treaty).
narrative_ontology:affects_constraint(unclos_sovereignty_boundary__strict_eez_reading, south_china_sea_nine_dash_line).
narrative_ontology:affects_constraint(unclos_sovereignty_boundary__strict_eez_reading, arctic_continental_shelf_claims).

% DUAL FORMULATION NOTE:
% The unclos_sovereignty_boundary kernel decomposes into three constraint stories: this strict_eez_reading (tangled_rope, high extraction from overlapping claimants), historical_rights_reading (snare from the strict reading's perspective, but rope from the claimant's perspective), and non_ratifier_enforcement_reading (rope for navigation, snare for resource access). The three stories form a constraint family linked by network.affects_constraints. The strict reading's enforcement machinery structurally influences the other two by defining the legal baseline they must contest.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(unclos_sovereignty_boundary__strict_eez_reading, institutional, 0.1).
constraint_indexing:directionality_override(unclos_sovereignty_boundary__strict_eez_reading, powerless, 1.0).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
