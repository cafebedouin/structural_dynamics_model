% ============================================================================
% CONSTRAINT STORY: unclos_maritime_sovereignty__expansive_construction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
 *   constraint_id: unclos_maritime_sovereignty__expansive_construction_reading
 *   human_readable: Artificial Island Construction and Maritime Sovereignty Expansion
 *   domain: international_law/maritime_governance/geopolitical_strategy
 *
 * SUMMARY:
 *   This constraint story instantiates the expansive construction reading of
 *   the UNCLOS maritime sovereignty kernel. The reading asserts that
 *   artificial islands constructed on submerged features or low-tide
 *   elevations generate full territorial seas (12 nautical miles) and
 *   exclusive economic zones (200 nautical miles) through effective
 *   occupation and administrative control. This is one of three structurally
 *   distinct readings of the same foundational ambiguity in UNCLOS Article
 *   121: what counts as an 'island' with zone-generating capacity. The
 *   expansive reading benefits geographically constrained states and
 *   island-constructing powers; it extracts from neighboring claimants and
 *   freedom-of-navigation states. The constraint is claimed as tangled_rope
 *   because it combines genuine coordination (clarifying the ambiguity) with
 *   asymmetric extraction (benefiting certain parties through a reading that
 *   others reject). The measurement series traces the constraint's
 *   intensification from a contested practice (t=0) to a near-normalized
 *   reading (t=35), showing how theater ratio rises (performative acceptance
 *   in diplomatic forums while legal clarity remains absent) and extraction
 *   accumulates (more states employ the reading, entrenching the asymmetry).
 *
 * KEY AGENTS:
 *   - Island-constructing states: institutional power, mobile exit (can accelerate or decelerate construction), agenda-setter role
 *   - Neighboring claimant states: powerful but constrained by geographic proximity, payer role, high cost of counter-construction or legal challenge
 *   - Freedom-of-navigation coalition: institutional power, constrained by economic routing, payer role, conduct freedom-of-navigation operations at rising risk
 *   - UNCLOS authority and tribunal: analytical seat, slow-moving interpreter, weak enforcement machinery
 *   - Geographically constrained states: beneficiary role but trapped by geography, cannot exit the constraint's applicability
 *   - Small island and landlocked states: excluded, powerless, structurally kept out of the dispute that shapes the reading
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(unclos_maritime_sovereignty__expansive_construction_reading, 0.78).
domain_priors:suppression_score(unclos_maritime_sovereignty__expansive_construction_reading, 0.72).
domain_priors:theater_ratio(unclos_maritime_sovereignty__expansive_construction_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(unclos_maritime_sovereignty__expansive_construction_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(unclos_maritime_sovereignty__expansive_construction_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(unclos_maritime_sovereignty__expansive_construction_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(unclos_maritime_sovereignty__expansive_construction_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(unclos_maritime_sovereignty__expansive_construction_reading, resistance, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(unclos_maritime_sovereignty__expansive_construction_reading, tangled_rope).
narrative_ontology:human_readable(unclos_maritime_sovereignty__expansive_construction_reading, "Artificial Island Construction and Maritime Sovereignty Expansion").
narrative_ontology:topic_domain(unclos_maritime_sovereignty__expansive_construction_reading, "international_law/maritime_governance/geopolitical_strategy").

domain_priors:requires_active_enforcement(unclos_maritime_sovereignty__expansive_construction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(unclos_maritime_sovereignty__expansive_construction_reading, 'd6522514-6920-484c-81b3-dfa6800667d2').
narrative_ontology:cs_kernel_codification('d6522514-6920-484c-81b3-dfa6800667d2', fixed_text).
narrative_ontology:cs_authority_grounding('d6522514-6920-484c-81b3-dfa6800667d2', extraction).
narrative_ontology:cs_interpretation_layer_present('d6522514-6920-484c-81b3-dfa6800667d2').
narrative_ontology:cs_reading_relation('d6522514-6920-484c-81b3-dfa6800667d2', unclos_maritime_sovereignty__strict_geographic_reading, forecloses).
narrative_ontology:cs_reading_relation('d6522514-6920-484c-81b3-dfa6800667d2', unclos_maritime_sovereignty__hybrid_effective_control_reading, coexists_with).
narrative_ontology:cs_axiom('d6522514-6920-484c-81b3-dfa6800667d2', foundational, effective_occupation_generates_sovereignty).
narrative_ontology:cs_axiom_status(effective_occupation_generates_sovereignty, holdable).
narrative_ontology:cs_axiom_grounding('d6522514-6920-484c-81b3-dfa6800667d2', effective_occupation_generates_sovereignty, instrumental).
narrative_ontology:cs_axiom('d6522514-6920-484c-81b3-dfa6800667d2', secondary, artificial_features_indistinct_from_natural).
narrative_ontology:cs_axiom_status(artificial_features_indistinct_from_natural, holdable).
narrative_ontology:cs_axiom_grounding('d6522514-6920-484c-81b3-dfa6800667d2', artificial_features_indistinct_from_natural, deontological).
narrative_ontology:cs_reference_frame('d6522514-6920-484c-81b3-dfa6800667d2', effective_occupation_maritime_principle).
narrative_ontology:cs_drift_state('d6522514-6920-484c-81b3-dfa6800667d2', contemporary_post_island_construction_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('d6522514-6920-484c-81b3-dfa6800667d2', '').
narrative_ontology:cs_kernel_id(unclos_maritime_sovereignty__expansive_construction_reading, unclos_maritime_sovereignty).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(unclos_maritime_sovereignty__expansive_construction_reading, island_constructing_states).
narrative_ontology:constraint_beneficiary(unclos_maritime_sovereignty__expansive_construction_reading, geographically_constrained_states).
narrative_ontology:constraint_victim(unclos_maritime_sovereignty__expansive_construction_reading, neighboring_claimant_states).
narrative_ontology:constraint_victim(unclos_maritime_sovereignty__expansive_construction_reading, freedom_of_navigation_coalition).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Construct artificial islands on submerged features or low-tide elevations in disputed maritime zones. Claim these constructions generate full territorial seas (12nm) and exclusive economic zones (200nm) through effective occupation and administrative control. Justify the practice as exercising sovereignty over continental shelf resources, securing maritime boundaries, and establishing presence in areas of geopolitical importance. Control the pace and scale of island construction; may accelerate or decelerate projects in response to international pressure.
narrative_ontology:constraint_stakeholder(unclos_maritime_sovereignty__expansive_construction_reading, island_constructing_states, agenda_setter,
    institutional, generational, mobile, global).

% Experience contraction of claimed maritime zones as neighboring states construct artificial islands. Their pre-existing claims to continental shelf and EEZ resources are challenged or overridden by the new features' generated zones. Pursue diplomatic protest, legal challenge under UNCLOS, and competing island construction, but these options are costly (diplomatic capital, military expenditure, construction cost) and may not reverse facts on the water. Cannot exit the geographic proximity that makes them vulnerable to the constraint.
narrative_ontology:constraint_stakeholder(unclos_maritime_sovereignty__expansive_construction_reading, neighboring_claimant_states, payer,
    powerful, generational, constrained, regional).

% Includes open-ocean trading states and naval powers whose transit routes pass through expansively claimed zones. The constraint reduces their effective freedom of navigation by extending national sovereignty claims into waters they previously treated as international. They conduct freedom-of-navigation operations (challenging the claim through deliberate transit) but face increasing costs (risk of incident, diplomatic friction) without legal clarity. Cannot avoid the waters without major rerouting at significant economic cost.
narrative_ontology:constraint_stakeholder(unclos_maritime_sovereignty__expansive_construction_reading, freedom_of_navigation_coalition, payer,
    institutional, generational, constrained, global).

% Interprets the UN Convention on the Law of the Sea. UNCLOS text uses the term 'island' without defining whether artificial features qualify. The tribunal (ITLOS) and state parties have produced competing interpretations. Holds the formal authority to resolve disputes but the machinery is slow (proceedings take years) and enforcement is weak (UNCLOS depends on state compliance, not coercive mechanisms). Sits as the analytical seat that would adjudicate the reading contest.
narrative_ontology:constraint_stakeholder(unclos_maritime_sovereignty__expansive_construction_reading, unclos_authority_and_tribunal, observer,
    institutional, generational, analytical, global).

% States with limited continental shelf or EEZ natural features that benefit from the expansive construction reading's acceptance. If the reading becomes customary international law, they gain the option to construct islands and expand their own maritime zones without legal challenge. However, they bear the high construction cost and face the risk that neighboring states' counter-construction will leave them in the same relative position. They are trapped by geography; cannot opt out of the constraint's applicability.
narrative_ontology:constraint_stakeholder(unclos_maritime_sovereignty__expansive_construction_reading, geographically_constrained_states, beneficiary,
    moderate, generational, trapped, regional).

% Have no continental shelf to construct artificial islands upon, and their objections to the expansive reading are never heard at the negotiating table. They depend on international law stability and freedom of navigation through straits. The constraint threatens both by destabilizing maritime zone definitions and creating new military chokepoints. Their interests are structurally excluded from the constraint's formation.
narrative_ontology:constraint_stakeholder(unclos_maritime_sovereignty__expansive_construction_reading, small_island_and_landlocked_states, excluded,
    powerless, generational, trapped, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(unclos_maritime_sovereignty__expansive_construction_reading, island_constructing_states).
narrative_ontology:fixing_cost_class(unclos_maritime_sovereignty__expansive_construction_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Resolves the problem of defining maritime sovereignty in an age of engineered geography: without a rule for artificial islands, no state can build confidence that constructed features will generate recognized maritime zones. The expansive reading provides one coordination solution (construction = sovereignty) at the cost of asymmetric benefit.
% TRANSFER_FUNCTION: Transfers maritime resource access and navigational freedom from the open-ocean coalition and neighboring claimants to island-constructing states. Moves geopolitical positioning from the negotiating table to the engineering site: the state that builds fastest and largest captures the zone, not the state with the strongest legal claim.
% ABSENT_VOICES: Small island states and landlocked states have no construction capacity and are not represented in the bilateral disputes driving the reading's acceptance. Freedom-of-navigation operators from non-great-power states have minimal voice in UNCLOS interpretation. Indigenous Pacific island communities are excluded entirely despite bearing the consequences of changed maritime law around their territories.
% DISAPPEARANCE_RATIONALE: If the expansive construction reading disappeared and the strict geographic reading took hold instead, artificial islands would lose their maritime zone-generating status. Constructing states would be forced to cease projects (sunk cost loss) or convert them to other uses. Neighboring claimants would reassert pre-construction maritime zones. The global maritime order would stabilize around natural geography rather than engineered facts.
% FOUNDING_PROBLEM: In the late 20th and early 21st centuries, several states (especially geographically constrained Asian powers) sought to extend maritime control over disputed shelf areas, but UNCLOS did not explicitly address whether artificial constructions qualify as 'islands' for zone-generation purposes. The expansive reading emerged as a practical solution: if you build it, it generates sovereignty, settling the ambiguity in favor of effective occupation.
% FOUNDING_PROBLEM_CORROBORATION: Island-constructing states attest the problem is live and their reading is the necessary solution. The strict geographic reading is advocated by open-ocean traders and maritime law scholars who treat artificial features as inherently different from natural formations. The hybrid reading is advanced by moderate states and some tribunal opinions. No consensus corroboration exists outside the reading's own beneficiaries; ITLOS and state practice remain divided.
narrative_ontology:disappearance_verdict(unclos_maritime_sovereignty__expansive_construction_reading, world_rearranges).
narrative_ontology:founding_problem_status(unclos_maritime_sovereignty__expansive_construction_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(unclos_maritime_sovereignty__expansive_construction_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(unclos_maritime_sovereignty__expansive_construction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(unclos_maritime_sovereignty__expansive_construction_reading, 0.78, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness rises over the interval (0.48 → 0.78) because the reading progressively becomes accepted in state practice, expanding the range of states employing it and intensifying the claims overlaid on neighboring zones. Suppression requirement also rises (0.54 → 0.72) because island-constructing states must actively enforce their claims through military presence, coast guard operations, and resistance to freedom-of-navigation challenges; the constraint persists because of this enforcement, not because neighboring states accept the reading's legitimacy. Theater ratio is moderate and plateaus around 0.41 because the constraint involves real construction (not pure performance) but also significant diplomatic theater (UNCLOS meetings, soft-law declarations, strategic ambiguity about legal status). Accessibility collapse is moderate (0.68) because alternative readings remain live — the strict geographic reading and hybrid reading are still advocated and have institutional support (some ITLOS opinions). Resistance is substantial (0.64) because the freedom-of-navigation coalition actively challenges artificial-island claims through operations and scholarship; neighboring claimants pursue legal remedies and competing construction.
 *
 * PERSPECTIVAL GAP:
 *   From the island-constructing state's seat, the constraint is an efficient solution to maritime ambiguity—effective occupation has always been a standard for territorial claims, so extending it to artificial features is natural law applied to engineering. From the neighboring claimant's seat, the constraint is a coercive re-writing of the law: UNCLOS did not authorize artificial features as islands, so the practice amounts to unilateral amendment of the treaty. From the freedom-of-navigation coalition's seat, the constraint is a creeping enclosure of the high seas. The engine computes these divergent types from the structural data: beneficiary seats compute rope or lower-extraction types, payer seats compute snare or high-extraction types, the analytical seat computes the constraint as contested (multiple readings, unresolved).
 *
 * DIRECTIONALITY LOGIC:
 *   Island-constructing states benefit from the expansive reading: they gain maritime zones (12nm + 200nm EEZ) with minimal legal friction, and they control the pace of construction. Their d-value is near the beneficiary end (~0.15-0.25) because they are architectural beneficiaries who set the constraint and collect the rents. Neighboring claimants and freedom-of-navigation operators are targets: they lose access to shelf resources and navigational freedom without having chosen the constraint. Their d-values are high (~0.75-0.85) because they are structurally constrained and bear the costs. Geographically constrained states are secondary beneficiaries with trapped exit: they benefit if the reading becomes customary law (they could then construct islands themselves) but cannot opt out of geography, so their d is moderately low (~0.35-0.45). UNCLOS tribunal is the analytical seat (d = 0.5, symmetric observer position).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (UNCLOS ambiguity about artificial islands) is contested in status: island-constructing states claim it is live (artificial construction needs legal clarity), while critics claim it is dead or solved (the strict geographic reading already provides clarity—artificial features simply don't count as islands, which is the default interpretation absent amendment). The disappearance verdict is world_rearranges, which is consistent with an active constraint. No mandatrophy signal arises from this pair. However, the measurement series shows theater_ratio plateau around t=25-35, which may indicate the constraint is entering a performative maintenance phase: states conduct operations (freedom-of-navigation challenges, diplomatic statements) that suggest the reading is still contested, but the underlying extraction (de facto acceptance of the reading in state practice) is already entrenched. This is consistent with the constraint maturing from pure extraction into performance-defending-extraction, a piton-trajectory. The high suppression_requirement (0.72 at interval end) supports this: the reading persists because of active enforcement (military presence, dismissal of legal challenges), not because anyone thinks it's legitimate. A future measurement showing suppression_requirement rising above 0.8 while resistance falls below 0.4 would confirm piton status.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    artificial_vs_natural_definition,
    'Is the boundary between ''artificial'' and ''natural'' island features legally definable, or does the expansive reading collapse that boundary?',
    'ITLOS ruling or state-party agreement on specific criteria: Does an artificial feature that remains above water for decades, generates tidal cycles, and develops fauna count as ''natural''? Or does any human construction irredeemably mark it as ''artificial''?',
    'If the boundary collapses (all sufficiently durable features count as islands regardless of origin), the expansive reading becomes logically unassailable and shifts from tangled_rope toward rope. If the boundary hardens, the expansive reading becomes untenable and forecloses in favor of the strict geographic reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(artificial_vs_natural_definition, conceptual, 'Whether ''artificial'' and ''natural'' are legally stable categories or collapse under the expansive reading.').

omega_variable(
    effective_occupation_customary_status,
    'Has the expansive reading accumulated enough state practice to achieve customary international law status, or does it remain a unilateral practice?',
    'Systematic audit of state behavior over the next 10-15 years: Do neighboring states accept artificial islands as generating zones, or do they continue to protest and refuse recognition? Do ITLOS decisions cite state practice to affirm or deny the reading?',
    'If customary status is achieved, the reading becomes self-perpetuating (states follow law they themselves created through practice). If customary status is denied, it remains a contested practice vulnerable to legal challenge and potentially reversible by agreement.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(effective_occupation_customary_status, empirical, 'Whether effective occupation of artificial islands will crystallize into customary international law.').

omega_variable(
    suppression_internalization_ambiguity,
    'Is the measured suppression (0.72) structural (active military/coast guard enforcement) or internalized (neighboring states have accepted the reading''s legitimacy and no longer resist)?',
    'Post-removal test: If island-constructing states withdrew military presence and enforcement machinery, would neighboring states continue to respect the artificial-island zones, or would they immediately reassert their own claims? If they reassert, suppression is structural; if they defer, it is internalized (they have accepted the reading).',
    'If suppression is structural and removed, the constraint collapses into negotiated zones (rope or pure coordination). If internalized, the constraint persists as customary law even without enforcement machinery.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_internalization_ambiguity, empirical, 'Whether suppression requirement reflects active enforcement or internalized acceptance.').

omega_variable(
    geographic_determinism_in_readings,
    'Does the expansive reading represent a genuine legal interpretation of UNCLOS, or is it geographic determinism dressed as law—states with construction capacity adopt the expansive reading, states without construction capacity adopt the strict reading?',
    'Comparative analysis of state positions: Do landlocked and small-island states systematically support the strict geographic reading regardless of their geographic position? Or do some geographically constrained continental states support the expansive reading despite their inability to construct islands?',
    'If readings track geographic interest perfectly, the constraint is pure geopolitical capture masquerading as legal interpretation (strengthens snare classification). If readings diverge from interest (some capacity-lacking states support expansive reading, some capacity-having states support strict reading), the legal substance of the readings is separable from geopolitical interest.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(geographic_determinism_in_readings, conceptual, 'Whether the reading contest is a matter of legal interpretation or geographic determinism.').

omega_variable(
    kernel_reading_identity_in_cs_frame,
    'Is this constraint best understood as an instantiation of ONE reading of a contested UNCLOS kernel, or as a standalone constraint that happens to relate to UNCLOS?',
    'Structural test: Does analyzing this constraint require understanding the sibling readings (strict_geographic_reading, hybrid_effective_control_reading)? If yes, it is truly a kernel reading; if the constraint''s classification is independent of the sibling readings'' claims, it is a standalone constraint that coexists with competitors but is not one-of-three interpretations of a shared kernel.',
    'If kernel reading: the constraint''s legitimacy depends on the relative strength of competing readings; if one sibling reading is adopted by a consensus of states, this reading''s classification could shift sharply. If standalone: the constraint is independently classifiable and only accidentally related to UNCLOS interpretation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity_in_cs_frame, conceptual, 'Whether this constraint is structurally one reading of a kernel or a standalone constraint.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(unclos_maritime_sovereignty__expansive_construction_reading, 0, 35).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(uncl_tr_t0, unclos_maritime_sovereignty__expansive_construction_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement_basis(uncl_tr_t0, observed).
narrative_ontology:measurement(uncl_tr_t5, unclos_maritime_sovereignty__expansive_construction_reading, theater_ratio, 5, 0.26).
narrative_ontology:measurement_basis(uncl_tr_t5, observed).
narrative_ontology:measurement(uncl_tr_t10, unclos_maritime_sovereignty__expansive_construction_reading, theater_ratio, 10, 0.31).
narrative_ontology:measurement_basis(uncl_tr_t10, observed).
narrative_ontology:measurement(uncl_tr_t15, unclos_maritime_sovereignty__expansive_construction_reading, theater_ratio, 15, 0.36).
narrative_ontology:measurement_basis(uncl_tr_t15, observed).
narrative_ontology:measurement(uncl_tr_t20, unclos_maritime_sovereignty__expansive_construction_reading, theater_ratio, 20, 0.38).
narrative_ontology:measurement_basis(uncl_tr_t20, observed).
narrative_ontology:measurement(uncl_tr_t25, unclos_maritime_sovereignty__expansive_construction_reading, theater_ratio, 25, 0.4).
narrative_ontology:measurement_basis(uncl_tr_t25, observed).
narrative_ontology:measurement(uncl_tr_t30, unclos_maritime_sovereignty__expansive_construction_reading, theater_ratio, 30, 0.41).
narrative_ontology:measurement_basis(uncl_tr_t30, observed).
narrative_ontology:measurement(uncl_tr_t35, unclos_maritime_sovereignty__expansive_construction_reading, theater_ratio, 35, 0.41).
narrative_ontology:measurement_basis(uncl_tr_t35, observed).

% Extraction over time
narrative_ontology:measurement(uncl_be_t0, unclos_maritime_sovereignty__expansive_construction_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement_basis(uncl_be_t0, observed).
narrative_ontology:measurement(uncl_be_t5, unclos_maritime_sovereignty__expansive_construction_reading, base_extractiveness, 5, 0.54).
narrative_ontology:measurement_basis(uncl_be_t5, observed).
narrative_ontology:measurement(uncl_be_t10, unclos_maritime_sovereignty__expansive_construction_reading, base_extractiveness, 10, 0.62).
narrative_ontology:measurement_basis(uncl_be_t10, observed).
narrative_ontology:measurement(uncl_be_t15, unclos_maritime_sovereignty__expansive_construction_reading, base_extractiveness, 15, 0.68).
narrative_ontology:measurement_basis(uncl_be_t15, observed).
narrative_ontology:measurement(uncl_be_t20, unclos_maritime_sovereignty__expansive_construction_reading, base_extractiveness, 20, 0.73).
narrative_ontology:measurement_basis(uncl_be_t20, observed).
narrative_ontology:measurement(uncl_be_t25, unclos_maritime_sovereignty__expansive_construction_reading, base_extractiveness, 25, 0.76).
narrative_ontology:measurement_basis(uncl_be_t25, observed).
narrative_ontology:measurement(uncl_be_t30, unclos_maritime_sovereignty__expansive_construction_reading, base_extractiveness, 30, 0.77).
narrative_ontology:measurement_basis(uncl_be_t30, observed).
narrative_ontology:measurement(uncl_be_t35, unclos_maritime_sovereignty__expansive_construction_reading, base_extractiveness, 35, 0.78).
narrative_ontology:measurement_basis(uncl_be_t35, observed).

% Suppression requirement over time
narrative_ontology:measurement(uncl_su_t0, unclos_maritime_sovereignty__expansive_construction_reading, suppression_requirement, 0, 0.54).
narrative_ontology:measurement_basis(uncl_su_t0, observed).
narrative_ontology:measurement(uncl_su_t5, unclos_maritime_sovereignty__expansive_construction_reading, suppression_requirement, 5, 0.58).
narrative_ontology:measurement_basis(uncl_su_t5, observed).
narrative_ontology:measurement(uncl_su_t10, unclos_maritime_sovereignty__expansive_construction_reading, suppression_requirement, 10, 0.62).
narrative_ontology:measurement_basis(uncl_su_t10, observed).
narrative_ontology:measurement(uncl_su_t15, unclos_maritime_sovereignty__expansive_construction_reading, suppression_requirement, 15, 0.66).
narrative_ontology:measurement_basis(uncl_su_t15, observed).
narrative_ontology:measurement(uncl_su_t20, unclos_maritime_sovereignty__expansive_construction_reading, suppression_requirement, 20, 0.68).
narrative_ontology:measurement_basis(uncl_su_t20, observed).
narrative_ontology:measurement(uncl_su_t25, unclos_maritime_sovereignty__expansive_construction_reading, suppression_requirement, 25, 0.7).
narrative_ontology:measurement_basis(uncl_su_t25, observed).
narrative_ontology:measurement(uncl_su_t30, unclos_maritime_sovereignty__expansive_construction_reading, suppression_requirement, 30, 0.71).
narrative_ontology:measurement_basis(uncl_su_t30, observed).
narrative_ontology:measurement(uncl_su_t35, unclos_maritime_sovereignty__expansive_construction_reading, suppression_requirement, 35, 0.72).
narrative_ontology:measurement_basis(uncl_su_t35, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(unclos_maritime_sovereignty__expansive_construction_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(unclos_maritime_sovereignty__expansive_construction_reading, 0.12).
narrative_ontology:affects_constraint(unclos_maritime_sovereignty__expansive_construction_reading, unclos_maritime_sovereignty__strict_geographic_reading).
narrative_ontology:affects_constraint(unclos_maritime_sovereignty__expansive_construction_reading, unclos_maritime_sovereignty__hybrid_effective_control_reading).
narrative_ontology:affects_constraint(unclos_maritime_sovereignty__expansive_construction_reading, south_china_sea_island_militarization).
narrative_ontology:affects_constraint(unclos_maritime_sovereignty__expansive_construction_reading, freedom_of_navigation_operations).

% DUAL FORMULATION NOTE:
% This constraint is part of the unclos_maritime_sovereignty kernel family. It instantiates the expansive_construction_reading, which interprets UNCLOS Article 121 to permit artificial islands to generate full territorial seas and EEZs. The sibling strict_geographic_reading denies this, treating artificial features as legally inert. The hybrid_effective_control_reading permits maturation through prolonged occupation without challenge. All three are readings of the same kernel text; each produces a different constraint with different ε, beneficiaries, victims, and type. They are linked via network.affects_constraints to enable cross-reading analysis and to trace how adoption of one reading forecloses or influences the others.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(unclos_maritime_sovereignty__expansive_construction_reading, powerful, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
