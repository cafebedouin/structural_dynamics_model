% ============================================================================
% CONSTRAINT STORY: bretton_woods_treaty_substrate__sovereignty_defense
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_bretton_woods_treaty_substrate__sovereignty_defense, []).

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
 *   constraint_id: bretton_woods_treaty_substrate__sovereignty_defense
 *   human_readable: Bretton Woods Treaty Substrate (Sovereignty Defense Reading)
 *   domain: international_political_economy/monetary_history/institutional_design
 *
 * SUMMARY:
 *   This reading of the Bretton Woods treaty substrate interprets the system
 *   as a constraint on external monetary discipline that nominally preserves
 *   national monetary sovereignty but in practice extracts seigniorage and
 *   policy autonomy from non-reserve-currency states for the benefit of the
 *   United States. The gold-dollar anchor becomes a snare: it forces
 *   asymmetric adjustment on deficit countries while the reserve issuer faces
 *   no symmetric constraint. The IMF enforces this asymmetry through
 *   conditionality. The constraint is a tangled rope because it genuinely
 *   coordinated post-war trade and reconstruction (the rope function) while
 *   simultaneously embedding an extractive structure that grew over time (the
 *   snare function).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(bretton_woods_treaty_substrate__sovereignty_defense, 0.72).
domain_priors:suppression_score(bretton_woods_treaty_substrate__sovereignty_defense, 0.78).
domain_priors:theater_ratio(bretton_woods_treaty_substrate__sovereignty_defense, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(bretton_woods_treaty_substrate__sovereignty_defense, extractiveness, 0.72).
narrative_ontology:constraint_metric(bretton_woods_treaty_substrate__sovereignty_defense, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(bretton_woods_treaty_substrate__sovereignty_defense, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(bretton_woods_treaty_substrate__sovereignty_defense, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(bretton_woods_treaty_substrate__sovereignty_defense, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(bretton_woods_treaty_substrate__sovereignty_defense, tangled_rope).
narrative_ontology:human_readable(bretton_woods_treaty_substrate__sovereignty_defense, "Bretton Woods Treaty Substrate (Sovereignty Defense Reading)").
narrative_ontology:topic_domain(bretton_woods_treaty_substrate__sovereignty_defense, "international_political_economy/monetary_history/institutional_design").

domain_priors:requires_active_enforcement(bretton_woods_treaty_substrate__sovereignty_defense).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(bretton_woods_treaty_substrate__sovereignty_defense, 'bdf65d3e-fff3-4bc5-a9b1-f3bd5398c12c').
narrative_ontology:cs_kernel_codification('bdf65d3e-fff3-4bc5-a9b1-f3bd5398c12c', formalized).
narrative_ontology:cs_authority_grounding('bdf65d3e-fff3-4bc5-a9b1-f3bd5398c12c', extraction).
narrative_ontology:cs_interpretation_layer_present('bdf65d3e-fff3-4bc5-a9b1-f3bd5398c12c').
narrative_ontology:cs_reading_relation('bdf65d3e-fff3-4bc5-a9b1-f3bd5398c12c', bretton_woods_treaty_substrate__keynesian_embedded_liberalism, influences).
narrative_ontology:cs_reading_relation('bdf65d3e-fff3-4bc5-a9b1-f3bd5398c12c', bretton_woods_treaty_substrate__neoliberal_convertibility, coexists_with).
narrative_ontology:cs_axiom('bdf65d3e-fff3-4bc5-a9b1-f3bd5398c12c', foundational, exorbitant_privilege_is_structural).
narrative_ontology:cs_axiom_status(exorbitant_privilege_is_structural, holdable).
narrative_ontology:cs_axiom_grounding('bdf65d3e-fff3-4bc5-a9b1-f3bd5398c12c', exorbitant_privilege_is_structural, empirically_contingent).
narrative_ontology:cs_axiom('bdf65d3e-fff3-4bc5-a9b1-f3bd5398c12c', foundational, gold_anchor_is_snare_not_stabilizer).
narrative_ontology:cs_axiom_status(gold_anchor_is_snare_not_stabilizer, holdable).
narrative_ontology:cs_axiom_grounding('bdf65d3e-fff3-4bc5-a9b1-f3bd5398c12c', gold_anchor_is_snare_not_stabilizer, empirically_contingent).
narrative_ontology:cs_reference_frame('bdf65d3e-fff3-4bc5-a9b1-f3bd5398c12c', bretton_woods_founding_compromise).
narrative_ontology:cs_drift_state('bdf65d3e-fff3-4bc5-a9b1-f3bd5398c12c', collapse_1971, gap(codification_collapse, severe, true)).
narrative_ontology:cs_created_at('bdf65d3e-fff3-4bc5-a9b1-f3bd5398c12c', '').
narrative_ontology:cs_kernel_id(bretton_woods_treaty_substrate__sovereignty_defense, bretton_woods_treaty_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(bretton_woods_treaty_substrate__sovereignty_defense, united_states).
narrative_ontology:constraint_beneficiary(bretton_woods_treaty_substrate__sovereignty_defense, international_commercial_banks).
narrative_ontology:constraint_victim(bretton_woods_treaty_substrate__sovereignty_defense, major_non_reserve_states).
narrative_ontology:constraint_victim(bretton_woods_treaty_substrate__sovereignty_defense, developing_countries).
narrative_ontology:constraint_vindicates(bretton_woods_treaty_substrate__sovereignty_defense, exorbitant_privilege_thesis).
narrative_ontology:constraint_vindicates(bretton_woods_treaty_substrate__sovereignty_defense, asymmetric_adjustment_doctrine).
narrative_ontology:constraint_vindicates(bretton_woods_treaty_substrate__sovereignty_defense, gold_anchor_as_snare).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Issues the world's primary reserve currency, sets the rules of the IMF and the gold-dollar standard, and enforces adjustment on deficit countries while avoiding adjustment itself. Collects seigniorage and structural demand for dollar assets. Can exit the constraint by closing the gold window (1971) without catastrophic cost.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__sovereignty_defense, united_states, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(bretton_woods_treaty_substrate__sovereignty_defense, united_states, beneficiary).

% Gain stable dollar-denominated lending opportunities, recycle petrodollars and Eurodollar markets, and benefit from IMF-backed debt service enforcement. Their business model depends on the dollar system's stability but they can shift currency exposure.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__sovereignty_defense, international_commercial_banks, beneficiary,
    organized, biographical, mobile, global).

% Countries like France, UK, West Germany, Japan. Must maintain dollar reserves, accept asymmetric adjustment (deflate to defend parity), and face gold conversion pressure. They have some leverage (e.g., French gold conversion) but cannot unilaterally change the system without triggering crisis.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__sovereignty_defense, major_non_reserve_states, payer,
    powerful, biographical, constrained, national).

% Face IMF conditionality that prioritizes creditor repayment over domestic welfare, have no reserve currency status, and cannot easily exit the dollar system because trade and debt are dollar-denominated. Their monetary sovereignty is most constrained.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__sovereignty_defense, developing_countries, payer,
    moderate, biographical, trapped, national).

% Enforces the adjustable peg system, polices capital controls, and imposes conditionality on borrowers. Its authority derives from the treaty text and U.S. backing. It cannot exit the constraint because it is the constraint's administrator.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__sovereignty_defense, imf, agenda_setter,
    institutional, generational, analytical, global).

% Advocate for a stricter gold standard or alternative monetary orders. Their exclusion from the operational core of Bretton Woods (which was a gold-exchange standard, not a full gold standard) makes them commentators rather than participants.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__sovereignty_defense, gold_standard_proponents, observer,
    organized, biographical, mobile, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provided a stable international monetary framework for post-war reconstruction and trade expansion by pegging currencies to the dollar and the dollar to gold, with the IMF as lender of last resort and arbiter of parity changes.
% TRANSFER_FUNCTION: Transfers real resources and policy autonomy from non-reserve-currency states (especially developing countries) to the United States via seigniorage, asymmetric adjustment burdens, and the requirement to hold dollar reserves. The gold anchor forces deficit countries to contract while the reserve issuer expands.
% ABSENT_VOICES: Colonial and post-colonial monetary authorities in Africa and Asia whose currencies were pegged to metropolitan currencies (sterling, franc) and thus doubly constrained. They were not represented at Bretton Woods and had no voice in IMF governance. Also, domestic labor movements in deficit countries forced to bear adjustment costs.
% DISAPPEARANCE_RATIONALE: If the Bretton Woods constraints vanished overnight, the dollar's reserve role would end, the U.S. would lose exorbitant privilege, non-reserve states would regain monetary policy space but face exchange rate volatility, and the IMF would lose its core enforcement mandate. The global monetary order would reorganize around floating rates or a new anchor — as it did after 1971.
% FOUNDING_PROBLEM: Post-war monetary chaos: competitive devaluations, trade collapse, and the absence of a credible international lender of last resort. The system was built to stabilize exchange rates and provide liquidity for reconstruction while preserving national policy autonomy through capital controls and adjustable pegs.
% FOUNDING_PROBLEM_CORROBORATION: Keynesian architects (e.g., Keynes, White) attested the problem was live in 1944. U.S. officials later attested the problem was solved (hence the system's success). French and developing country officials (e.g., Rueff, Prebisch) attested the problem morphed into asymmetric extraction by the 1960s. Independent economic historians (Eichengreen, Obstfeld) corroborate the shift from coordination to extraction.
narrative_ontology:disappearance_verdict(bretton_woods_treaty_substrate__sovereignty_defense, world_rearranges).
narrative_ontology:founding_problem_status(bretton_woods_treaty_substrate__sovereignty_defense, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(bretton_woods_treaty_substrate__sovereignty_defense, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(bretton_woods_treaty_substrate__sovereignty_defense, 'none', 1).
narrative_ontology:epsilon_provenance(bretton_woods_treaty_substrate__sovereignty_defense, 0.72, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(bretton_woods_treaty_substrate__sovereignty_defense_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(bretton_woods_treaty_substrate__sovereignty_defense, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(bretton_woods_treaty_substrate__sovereignty_defense_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises from 0.45 to 0.72 as the system matures: early years feature genuine coordination (Marshall Plan, European recovery), later years feature rising U.S. deficits, gold outflows, and the Triffin dilemma. Suppression increases from 0.55 to 0.78 as IMF conditionality hardens and capital controls become the only defense for non-reserve states. Theater ratio rises from 0.25 to 0.45 because the gold standard's credibility becomes increasingly performative — the U.S. cannot honor convertibility at $35/oz but maintains the pretense. Accessibility collapse at 0.58 reflects that alternatives (floating, SDRs, regional blocs) exist but are politically suppressed. Resistance at 0.55 captures French gold conversion, the Group of 77 demands, and academic critique (Triffin, Kindleberger).
 *
 * PERSPECTIVAL GAP:
 *   From the U.S. seat, the constraint is a rope (coordination with modest cost). From the developing country seat, it is a snare (pure extraction). From the major non-reserve state seat, it is a tangled rope (coordination with asymmetric extraction). The engine computes this divergence from the declared power/exit/role structure. The claimed_type (tangled_rope) reflects the author's structural judgment that the coordination function was real but degraded into extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   The United States is the structural beneficiary (d near 0.0) because it issues the reserve currency, sets the agenda, and can exit (Nixon shock). International banks are beneficiaries (d ~0.2) with mobile exit. Major non-reserve states are payers with constrained exit (d ~0.7) — they can threaten gold conversion but cannot leave the dollar system. Developing countries are trapped payers (d ~0.95) — they have no reserve currency, face IMF conditionality, and cannot exit without default. The IMF is an agenda-setter with analytical exit (it observes but cannot change the treaty without member consent).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (post-war monetary chaos) was substantially solved by the 1960s, but the constraint persisted and intensified extraction. The mandate to 'preserve monetary sovereignty' became a cover for the exorbitant privilege. The system's collapse in 1971 was a mandatrophy resolution: the constraint vanished when its extraction became unsustainable, not when its coordination function ended. The reading's axioms capture this: the gold anchor is a snare, not a stabilizer.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_constructed_extraction,
    'Is the asymmetric adjustment burden a necessary feature of any reserve currency system (natural law) or a contingent design choice of Bretton Woods that could have been symmetric (e.g., Keynes''s bancor)?',
    'Counterfactual analysis of the Keynes plan (ICU) and comparison with later symmetric systems (e.g., SDR allocation, euro). If symmetric designs were feasible, the extraction is constructed, not natural.',
    'If natural, the constraint trends toward mountain (inevitable). If constructed, it is a false summit mountain or tangled rope — the FSM signature would fire because beneficiaries (U.S.) are declared on a constraint that claims naturalness.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_extraction, conceptual, 'Whether the extraction is inherent to reserve currency systems or a contingent design flaw.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression of non-reserve states'' monetary sovereignty structural (IMF conditionality, dollar invoicing) or internalized (policy elites adopting ''sound finance'' norms that align with creditor interests)?',
    'Post-exit trajectory: after 1971, did developing countries gain policy space or did they internalize the constraint via Washington Consensus? If suppression persists after formal constraint removal, it is partially internalized.',
    'If internalized, effective suppression is higher than structural measures suggest — the constraint''s legacy extraction continues via cognitive capture.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression in the post-Bretton Woods era.').

omega_variable(
    kernel_reading_framing,
    'Does this reading (sovereignty_defense) foreclose the keynesian_embedded_liberalism reading, or do they coexist as different emphases on the same historical system?',
    'Test whether a single analytical framework can simultaneously hold: (a) capital controls protected policy space (keynesian) and (b) the gold-dollar anchor extracted sovereignty (sovereignty_defense). If yes, they coexist; if the second logically negates the first, sovereignty_defense forecloses keynesian.',
    'Determines the reading_relation declared in cs_structure. A forecloses relation would mean the kernel cannot sustain both readings in one framework.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_framing, conceptual, 'Logical relationship between sovereignty_defense and keynesian_embedded_liberalism readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(bretton_woods_treaty_substrate__sovereignty_defense, 0, 27).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bret_tr_t0, bretton_woods_treaty_substrate__sovereignty_defense, theater_ratio, 0, 0.25).
narrative_ontology:measurement(bret_tr_t5, bretton_woods_treaty_substrate__sovereignty_defense, theater_ratio, 5, 0.3).
narrative_ontology:measurement(bret_tr_t10, bretton_woods_treaty_substrate__sovereignty_defense, theater_ratio, 10, 0.35).
narrative_ontology:measurement(bret_tr_t15, bretton_woods_treaty_substrate__sovereignty_defense, theater_ratio, 15, 0.4).
narrative_ontology:measurement(bret_tr_t20, bretton_woods_treaty_substrate__sovereignty_defense, theater_ratio, 20, 0.42).
narrative_ontology:measurement(bret_tr_t25, bretton_woods_treaty_substrate__sovereignty_defense, theater_ratio, 25, 0.44).
narrative_ontology:measurement(bret_tr_t27, bretton_woods_treaty_substrate__sovereignty_defense, theater_ratio, 27, 0.45).

% Extraction over time
narrative_ontology:measurement(bret_be_t0, bretton_woods_treaty_substrate__sovereignty_defense, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(bret_be_t5, bretton_woods_treaty_substrate__sovereignty_defense, base_extractiveness, 5, 0.52).
narrative_ontology:measurement(bret_be_t10, bretton_woods_treaty_substrate__sovereignty_defense, base_extractiveness, 10, 0.58).
narrative_ontology:measurement(bret_be_t15, bretton_woods_treaty_substrate__sovereignty_defense, base_extractiveness, 15, 0.63).
narrative_ontology:measurement(bret_be_t20, bretton_woods_treaty_substrate__sovereignty_defense, base_extractiveness, 20, 0.68).
narrative_ontology:measurement(bret_be_t25, bretton_woods_treaty_substrate__sovereignty_defense, base_extractiveness, 25, 0.71).
narrative_ontology:measurement(bret_be_t27, bretton_woods_treaty_substrate__sovereignty_defense, base_extractiveness, 27, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(bret_su_t0, bretton_woods_treaty_substrate__sovereignty_defense, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(bret_su_t5, bretton_woods_treaty_substrate__sovereignty_defense, suppression_requirement, 5, 0.62).
narrative_ontology:measurement(bret_su_t10, bretton_woods_treaty_substrate__sovereignty_defense, suppression_requirement, 10, 0.68).
narrative_ontology:measurement(bret_su_t15, bretton_woods_treaty_substrate__sovereignty_defense, suppression_requirement, 15, 0.72).
narrative_ontology:measurement(bret_su_t20, bretton_woods_treaty_substrate__sovereignty_defense, suppression_requirement, 20, 0.75).
narrative_ontology:measurement(bret_su_t25, bretton_woods_treaty_substrate__sovereignty_defense, suppression_requirement, 25, 0.77).
narrative_ontology:measurement(bret_su_t27, bretton_woods_treaty_substrate__sovereignty_defense, suppression_requirement, 27, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(bretton_woods_treaty_substrate__sovereignty_defense, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(bretton_woods_treaty_substrate__sovereignty_defense, 0.12).
narrative_ontology:affects_constraint(bretton_woods_treaty_substrate__sovereignty_defense, bretton_woods_treaty_substrate__keynesian_embedded_liberalism).
narrative_ontology:affects_constraint(bretton_woods_treaty_substrate__sovereignty_defense, bretton_woods_treaty_substrate__neoliberal_convertibility).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the bretton_woods_treaty_substrate kernel. The keynesian reading emphasizes capital controls as sovereignty protection; the neoliberal reading emphasizes the system's failure to achieve convertibility; this reading emphasizes the extractive asymmetry of the gold-dollar standard. All three share the same treaty text but instantiate different constraints with different beneficiary/victim structures and epsilon values.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(bretton_woods_treaty_substrate__sovereignty_defense, institutional, 0.05).
constraint_indexing:directionality_override(bretton_woods_treaty_substrate__sovereignty_defense, powerful, 0.7).
constraint_indexing:directionality_override(bretton_woods_treaty_substrate__sovereignty_defense, moderate, 0.9).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
