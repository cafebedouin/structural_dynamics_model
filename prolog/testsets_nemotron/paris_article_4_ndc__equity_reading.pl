% ============================================================================
% CONSTRAINT STORY: paris_article_4_ndc__equity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_paris_article_4_ndc__equity_reading, []).

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
 *   constraint_id: paris_article_4_ndc__equity_reading
 *   human_readable: CBDR-Structured NDC Interpretation (Equity Reading)
 *   domain: international/climate/governance
 *
 * SUMMARY:
 *   The Paris Agreement's Article 4 establishes NDCs as the mitigation
 *   vehicle. The equity reading interprets 'common but differentiated
 *   responsibilities and respective capabilities' (CBDR-RC) as requiring
 *   structural distinctions in obligation: developed states bear binding
 *   mitigation and finance obligations; developing states retain policy space
 *   and receive support. This reading is contested — the sovereigntist
 *   reading treats NDCs as purely voluntary self-determination; the
 *   supranational reading treats them as binding ratcheting commitments
 *   toward net-zero with international accountability. The equity reading
 *   instantiates moderate base extractiveness (0.48) with asymmetric
 *   distribution: extraction flows from developed-state emitters and their
 *   carbon-intensive industries to developing-state coalitions and climate
 *   finance institutions. The constraint requires active enforcement
 *   (transparency framework, global stocktake, compliance committee) and
 *   functions as tangled rope — genuine coordination (collective mitigation
 *   architecture) fused with asymmetric extraction (differentiated obligation
 *   and finance flows).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(paris_article_4_ndc__equity_reading, 0.48).
domain_priors:suppression_score(paris_article_4_ndc__equity_reading, 0.35).
domain_priors:theater_ratio(paris_article_4_ndc__equity_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(paris_article_4_ndc__equity_reading, extractiveness, 0.48).
narrative_ontology:constraint_metric(paris_article_4_ndc__equity_reading, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(paris_article_4_ndc__equity_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(paris_article_4_ndc__equity_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(paris_article_4_ndc__equity_reading, resistance, 0.52).

% --- Constraint claim ---
narrative_ontology:constraint_claim(paris_article_4_ndc__equity_reading, tangled_rope).
narrative_ontology:human_readable(paris_article_4_ndc__equity_reading, "CBDR-Structured NDC Interpretation (Equity Reading)").
narrative_ontology:topic_domain(paris_article_4_ndc__equity_reading, "international/climate/governance").

domain_priors:requires_active_enforcement(paris_article_4_ndc__equity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(paris_article_4_ndc__equity_reading, '4d2d9c47-a20e-49bd-b275-847f5bdf68a8').
narrative_ontology:cs_kernel_codification('4d2d9c47-a20e-49bd-b275-847f5bdf68a8', formalized).
narrative_ontology:cs_authority_grounding('4d2d9c47-a20e-49bd-b275-847f5bdf68a8', lineage).
narrative_ontology:cs_interpretation_layer_present('4d2d9c47-a20e-49bd-b275-847f5bdf68a8').
narrative_ontology:cs_reading_relation('4d2d9c47-a20e-49bd-b275-847f5bdf68a8', paris_article_4_ndc__sovereigntist_reading, coexists_with).
narrative_ontology:cs_reading_relation('4d2d9c47-a20e-49bd-b275-847f5bdf68a8', paris_article_4_ndc__supranational_reading, influences).
narrative_ontology:cs_axiom('4d2d9c47-a20e-49bd-b275-847f5bdf68a8', foundational, differentiation_is_structurally_binding).
narrative_ontology:cs_axiom_status(differentiation_is_structurally_binding, holdable).
narrative_ontology:cs_axiom_grounding('4d2d9c47-a20e-49bd-b275-847f5bdf68a8', differentiation_is_structurally_binding, conventional).
narrative_ontology:cs_axiom('4d2d9c47-a20e-49bd-b275-847f5bdf68a8', foundational, historical_responsibility_grounds_obligation).
narrative_ontology:cs_axiom_status(historical_responsibility_grounds_obligation, holdable).
narrative_ontology:cs_axiom_grounding('4d2d9c47-a20e-49bd-b275-847f5bdf68a8', historical_responsibility_grounds_obligation, deontological).
narrative_ontology:cs_reference_frame('4d2d9c47-a20e-49bd-b275-847f5bdf68a8', paris_2015_cbdr_compromise).
narrative_ontology:cs_drift_state('4d2d9c47-a20e-49bd-b275-847f5bdf68a8', post_gst1_2023, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('4d2d9c47-a20e-49bd-b275-847f5bdf68a8', '').
narrative_ontology:cs_kernel_id(paris_article_4_ndc__equity_reading, paris_article_4_ndc).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(paris_article_4_ndc__equity_reading, developing_state_coalitions).
narrative_ontology:constraint_beneficiary(paris_article_4_ndc__equity_reading, climate_finance_institutions).
narrative_ontology:constraint_beneficiary(paris_article_4_ndc__equity_reading, equity_advocacy_networks).
narrative_ontology:constraint_victim(paris_article_4_ndc__equity_reading, developed_state_emitters).
narrative_ontology:constraint_victim(paris_article_4_ndc__equity_reading, carbon_intensive_industries).
narrative_ontology:constraint_vindicates(paris_article_4_ndc__equity_reading, common_but_differentiated_responsibilities_principle).
narrative_ontology:constraint_vindicates(paris_article_4_ndc__equity_reading, historical_responsibility_doctrine).
narrative_ontology:constraint_vindicates(paris_article_4_ndc__equity_reading, climate_justice_framework).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% G77+China, AOSIS, LDC Group, African Group — leverage CBDR interpretation to retain policy space for development, access climate finance, and avoid symmetric mitigation burdens. They hold procedural veto power in consensus forums but face pressure to enhance ambition. Exit means losing the equity framework that protects their development trajectory.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__equity_reading, developing_state_coalitions, beneficiary,
    organized, generational, constrained, global).

% EU, US, Japan, Canada, Australia — bear binding mitigation obligations and climate finance transfer commitments under CBDR interpretation. They set the agenda through financial leverage and technical capacity but are structurally targeted for extraction (finance, technology transfer, deeper cuts). Exit means reputational costs and loss of regime influence, but they have domestic political space to resist.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__equity_reading, developed_state_emitters, payer,
    institutional, biographical, mobile, global).
narrative_ontology:stakeholder_secondary_role(paris_article_4_ndc__equity_reading, developed_state_emitters, agenda_setter).

% GCF, GEF, multilateral development banks, bilateral climate funds — channel and administer the $100B+/yr transfer flow. Their mandate, staffing, and relevance expand with CBDR operationalization. They benefit from the arrangement's persistence but face pressure to demonstrate effectiveness. Exit options are high — they can pivot to other development finance.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__equity_reading, climate_finance_institutions, beneficiary,
    institutional, generational, arbitrage, global).

% Fossil fuel, cement, steel, aviation sectors in developed states — face regulated phase-down, carbon pricing, and border adjustment mechanisms driven by developed-state NDCs. They bear concentrated costs while the equity framework shields developing-state competitors. Exit means stranded assets or relocation to less regulated jurisdictions (constrained by CBAM-type measures).
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__equity_reading, carbon_intensive_industries, payer,
    organized, biographical, constrained, global).

% Climate justice NGOs, legal scholars, Global South think tanks — define and defend the CBDR interpretation, shape negotiating positions, litigate for enforcement. They gain institutional access and normative authority from the framework's persistence. Their exit is nearly costless — they can shift framing — but their influence depends on the reading's viability.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__equity_reading, equity_advocacy_networks, agenda_setter,
    organized, generational, mobile, global).
narrative_ontology:stakeholder_secondary_role(paris_article_4_ndc__equity_reading, equity_advocacy_networks, beneficiary).

% Frontline communities in both developed and developing states — Indigenous peoples, small island residents, agricultural workers, urban poor. They bear climate impacts regardless of NDC interpretation but have no structural voice in the equity/sovereignty/supranational contest. The CBDR reading claims to represent them but operates at state-coalition level. They cannot exit the climate impacts; they are trapped in the constraint's consequences.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__equity_reading, vulnerable_populations, excluded,
    powerless, biographical, trapped, global).

% IPCC, academic researchers, independent assessment bodies — evaluate NDC adequacy, track collective progress, assess equity claims. They see the full structural field but hold no enforcement power. Their exit is analytical frame-switching; they are not bound by the regime's political commitments.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__equity_reading, analytical_observers, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the collective action problem of global mitigation by allocating differentiated responsibilities: developed states lead on mitigation and finance; developing states contribute according to capability with support. The equity interpretation makes the allocation structurally binding rather than voluntary, enabling trust-based cooperation.
% TRANSFER_FUNCTION: Moves binding mitigation obligation and climate finance ($100B+/yr, technology transfer, capacity building) from developed states to developing states. Moves policy space and development rights from developed to developing states. Moves veto power over supranational enforcement to equity coalitions.
% ABSENT_VOICES: Vulnerable populations (frontline communities, Indigenous peoples, future generations) are structurally excluded — the reading operates at state-coalition level and claims to represent them but they have no seat. Subnational actors (cities, states, provinces) in developed countries pushing for faster action are also absent. Future generations have no voice but bear the consequences of any interpretation's adequacy.
% DISAPPEARANCE_RATIONALE: If the CBDR-structured interpretation vanished overnight, the Paris Agreement would revert to the sovereigntist reading (voluntary pledges) or collapse into fragmentation. Developed states would lose binding finance obligations; developing states would lose protected policy space; the trust architecture enabling the global stocktake would dissolve. The $100B+ finance flow, technology transfer commitments, and differentiated timelines would become purely voluntary — a fundamental rearrangement of the climate regime's political economy.
% FOUNDING_PROBLEM: The Kyoto Protocol's binary Annex I/non-Annex I division became obsolete as emissions profiles shifted, but developed states refused to accept symmetric obligations without differentiation. The 2009 Copenhagen collapse revealed the deadlock: developing states would not accept binding targets without finance and differentiation; developed states would not provide finance without developing-state commitments. CBDR-structured NDCs were the 2015 compromise — differentiation without binary annexes, binding process with nationally determined content.
% FOUNDING_PROBLEM_CORROBORATION: Developed states (EU, US negotiators) attest the founding problem is substantially solved — differentiation is operationalized, the regime exists — and the arrangement now functions as rent extraction. Developing state negotiators (G77 chairs, AOSIS leads) attest the problem is live — differentiation is eroded by supranational pressure, finance is inadequate, and the equity framework must be defended. Independent legal scholars (Rajamani, Voigt) and IPCC WGIII assessments corroborate that the structural tension between differentiation and collective adequacy remains unresolved — the founding problem persists in mutated form.
narrative_ontology:disappearance_verdict(paris_article_4_ndc__equity_reading, world_rearranges).
narrative_ontology:founding_problem_status(paris_article_4_ndc__equity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(paris_article_4_ndc__equity_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(paris_article_4_ndc__equity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(paris_article_4_ndc__equity_reading, 0.48, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(paris_article_4_ndc__equity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(paris_article_4_ndc__equity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(paris_article_4_ndc__equity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness (0.48) reflects the structural transfer: developed states bear binding costs (mitigation, finance) while developing states receive policy space and resources. The asymmetry is moderate — not pure extraction — because the coordination function is real: without differentiation, the regime collapses (Copenhagen precedent). Suppression (0.35) is modest: the constraint operates through consensus and transparency, not coercion, though the compliance committee and global stocktake create enforcement pressure. Theater ratio (0.42) is elevated: a growing share of negotiation energy performs equity rituals (differentiation language, finance pledges) while actual ambition and delivery lag. Accessibility collapse (0.58) is moderate: alternative interpretations (sovereigntist, supranational) remain live and contested. Resistance (0.52) is significant: developed states resist binding finance; developing states resist supranational creep; industries resist phase-down.
 *
 * PERSPECTIVAL GAP:
 *   From the developed-state payer seat, the constraint is extractive: binding finance and deeper cuts with insufficient reciprocity. From the developing-state beneficiary seat, it is coordination: the only framework protecting their development space and securing support. From the climate finance institution seat, it is institutional sustenance: their mandate expands with each COP. From the vulnerable-population excluded seat, it is inadequate: differentiation at state level does not protect them from impacts. The engine computes per-seat types from these structural positions — the equity reading claims tangled rope but seats experience it as snare (payers), rope (beneficiaries), or mountain (observers seeing structural necessity).
 *
 * DIRECTIONALITY LOGIC:
 *   Developed states and their carbon-intensive industries are structural targets (d near 1.0): they pay the transfers and bear the binding cuts. Developing state coalitions are structural beneficiaries (d near 0.0): they receive finance, technology, policy space. Climate finance institutions sit at d ≈ 0.15: they administer the flow and expand their mandate but bear fiduciary risk. Equity advocacy networks are near d ≈ 0.2: they gain influence but their position depends on the reading's viability. Vulnerable populations are trapped (identity_locked → high d): they bear climate impacts regardless of interpretation. Analytical observers sit at d = 0.5 (symmetric). Exit options differentiate: developed states are mobile (can resist domestically, exit Paris with reputational cost); developing states are constrained (need the regime for finance and voice); industries are constrained (CBAM limits relocation); advocates are mobile; vulnerable populations are trapped.
 *
 * MANDATROPHY ANALYSIS:
 *   The equity reading prevents mislabeling coordination as pure extraction by maintaining that differentiation is the *condition* for collective action, not its byproduct. The founding problem (Copenhagen deadlock) was real and the CBDR-structured NDC architecture solved it — but the solution has partially atrophied: finance delivery lags, the $100B goal was missed, supranational pressure erodes differentiation. Mandatrophy is unresolved: the arrangement persists because neither side can afford collapse, but the coordination-extraction balance has shifted toward extraction. The theater ratio rise (0.25→0.42) tracks this: more performance of equity, less delivery. The constraint is not yet piton — the coordination function remains load-bearing — but it drifts toward tangled rope with extraction dominance.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    equity_reading_kernel_identity,
    'Is the equity reading of paris_article_4_ndc a distinct constraint with its own stable ε, or does its classification depend on which observable (finance flows, mitigation outcomes, procedural equity) is measured?',
    'Apply the ε-invariance test: measure extraction from developed states (finance + mitigation cost) and transfer to developing states across multiple observables. If ε varies significantly by observable, decompose into separate constraints (e.g., finance_transfer_obligation, mitigation_differentiation_obligation, procedural_equity_veto).',
    'If ε is observable-dependent, this story conflates multiple constraints. Decomposition would yield separate classifications for finance transfer (snare/tangled rope), mitigation differentiation (rope/tangled rope), and procedural veto (rope). The equity reading''s claimed tangled_rope type would be an artifact of measurement averaging.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(equity_reading_kernel_identity, conceptual, 'Whether the equity reading satisfies ε-invariance or requires decomposition per DP-001.').

omega_variable(
    sovereigntist_foreclosure_boundary,
    'Does the equity reading''s core premise (differentiation is structurally binding) logically foreclose the sovereigntist reading (NDCs as purely voluntary) within any single commitment framework, or do they coexist as live positions?',
    'Analyze whether a party adopting the equity reading is logically committed to rejecting the sovereigntist premise. Test: can a single legal framework simultaneously hold that NDCs are binding differentiated obligations AND purely voluntary pledges? If no → forecloses. If yes (different parties hold each) → coexists_with.',
    'If forecloses, the readings are mutually exclusive within a framework — the kernel has a structural fault line. If coexists_with, the contest is political not logical, and both readings remain live in the regime''s pluralistic architecture.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sovereigntist_foreclosure_boundary, conceptual, 'Structural relation between equity and sovereigntist readings of the NDC kernel.').

omega_variable(
    supranational_influence_vector,
    'Does the equity reading create structural downstream pressure on the supranational reading (changes legitimacy conditions or resource availability) without foreclosing it?',
    'Trace whether the equity reading''s operationalization (differentiated timelines, finance conditionality, equity veto) alters the conditions under which the supranational reading could advance — e.g., by making uniform ratcheting politically harder, or by consuming diplomatic bandwidth that supranational enforcement would need.',
    'If influences, the equity reading shapes the supranational reading''s viability without logical exclusion — a structural pressure relation. This would appear as influences in reading_relations.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(supranational_influence_vector, empirical, 'Whether equity reading structurally pressures supranational reading downstream.').

omega_variable(
    finance_delivery_gap_as_extraction,
    'Is the persistent gap between pledged and delivered climate finance ($100B goal missed, post-2025 goal undefined) structural extraction by developed states, or a coordination failure within the equity framework?',
    'Compare finance delivery trajectories against developed states'' fiscal capacity and other international commitments. If delivery tracks capacity → coordination failure. If delivery systematically lags capacity while mitigation pressure on developing states increases → extraction.',
    'If extraction, the equity reading''s extractiveness is understated (current ε=0.48 captures only formal obligations, not delivery gaps). The constraint would be more snare-like from the developing-state seat. If coordination failure, the theater ratio (0.42) captures the performative gap honestly.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(finance_delivery_gap_as_extraction, empirical, 'Whether the finance delivery gap is extractive or coordinative in nature.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(paris_article_4_ndc__equity_reading, 2015, 2035).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(paris_art4_eq_tr_t2015, paris_article_4_ndc__equity_reading, theater_ratio, 2015, 0.25).
narrative_ontology:measurement(paris_art4_eq_tr_t2018, paris_article_4_ndc__equity_reading, theater_ratio, 2018, 0.3).
narrative_ontology:measurement(paris_art4_eq_tr_t2021, paris_article_4_ndc__equity_reading, theater_ratio, 2021, 0.35).
narrative_ontology:measurement(paris_art4_eq_tr_t2024, paris_article_4_ndc__equity_reading, theater_ratio, 2024, 0.38).
narrative_ontology:measurement(paris_art4_eq_tr_t2027, paris_article_4_ndc__equity_reading, theater_ratio, 2027, 0.4).
narrative_ontology:measurement(paris_art4_eq_tr_t2030, paris_article_4_ndc__equity_reading, theater_ratio, 2030, 0.41).
narrative_ontology:measurement(paris_art4_eq_tr_t2033, paris_article_4_ndc__equity_reading, theater_ratio, 2033, 0.42).
narrative_ontology:measurement(paris_art4_eq_tr_t2035, paris_article_4_ndc__equity_reading, theater_ratio, 2035, 0.42).

% Extraction over time
narrative_ontology:measurement(paris_art4_eq_be_t2015, paris_article_4_ndc__equity_reading, base_extractiveness, 2015, 0.32).
narrative_ontology:measurement(paris_art4_eq_be_t2018, paris_article_4_ndc__equity_reading, base_extractiveness, 2018, 0.38).
narrative_ontology:measurement(paris_art4_eq_be_t2021, paris_article_4_ndc__equity_reading, base_extractiveness, 2021, 0.42).
narrative_ontology:measurement(paris_art4_eq_be_t2024, paris_article_4_ndc__equity_reading, base_extractiveness, 2024, 0.45).
narrative_ontology:measurement(paris_art4_eq_be_t2027, paris_article_4_ndc__equity_reading, base_extractiveness, 2027, 0.47).
narrative_ontology:measurement(paris_art4_eq_be_t2030, paris_article_4_ndc__equity_reading, base_extractiveness, 2030, 0.48).
narrative_ontology:measurement(paris_art4_eq_be_t2033, paris_article_4_ndc__equity_reading, base_extractiveness, 2033, 0.5).
narrative_ontology:measurement(paris_art4_eq_be_t2035, paris_article_4_ndc__equity_reading, base_extractiveness, 2035, 0.5).

% Suppression requirement over time
narrative_ontology:measurement(paris_art4_eq_su_t2015, paris_article_4_ndc__equity_reading, suppression_requirement, 2015, 0.2).
narrative_ontology:measurement(paris_art4_eq_su_t2018, paris_article_4_ndc__equity_reading, suppression_requirement, 2018, 0.25).
narrative_ontology:measurement(paris_art4_eq_su_t2021, paris_article_4_ndc__equity_reading, suppression_requirement, 2021, 0.3).
narrative_ontology:measurement(paris_art4_eq_su_t2024, paris_article_4_ndc__equity_reading, suppression_requirement, 2024, 0.33).
narrative_ontology:measurement(paris_art4_eq_su_t2027, paris_article_4_ndc__equity_reading, suppression_requirement, 2027, 0.34).
narrative_ontology:measurement(paris_art4_eq_su_t2030, paris_article_4_ndc__equity_reading, suppression_requirement, 2030, 0.35).
narrative_ontology:measurement(paris_art4_eq_su_t2033, paris_article_4_ndc__equity_reading, suppression_requirement, 2033, 0.35).
narrative_ontology:measurement(paris_art4_eq_su_t2035, paris_article_4_ndc__equity_reading, suppression_requirement, 2035, 0.35).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(paris_article_4_ndc__equity_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(paris_article_4_ndc__equity_reading, 0.18).
narrative_ontology:affects_constraint(paris_article_4_ndc__equity_reading, paris_article_9_finance).
narrative_ontology:affects_constraint(paris_article_4_ndc__equity_reading, paris_article_13_transparency).
narrative_ontology:affects_constraint(paris_article_4_ndc__equity_reading, paris_article_14_global_stocktake).
narrative_ontology:affects_constraint(paris_article_4_ndc__equity_reading, paris_article_4_ndc__sovereigntist_reading).
narrative_ontology:affects_constraint(paris_article_4_ndc__equity_reading, paris_article_4_ndc__supranational_reading).

% DUAL FORMULATION NOTE:
% This constraint is the equity_reading of the paris_article_4_ndc kernel. It differs structurally from the sovereigntist_reading (voluntary pledges, ε≈0.15, rope) and supranational_reading (binding ratcheting, ε≈0.65, snare/tangled_rope). The three readings form a constraint family linked by network.affects_constraints. The equity reading's moderate asymmetric ε (0.48) sits between them — it coordinates through differentiation while extracting through binding finance/mitigation asymmetry.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(paris_article_4_ndc__equity_reading, institutional, 0.15).
constraint_indexing:directionality_override(paris_article_4_ndc__equity_reading, organized, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
