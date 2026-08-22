% ============================================================================
% CONSTRAINT STORY: dollar_gold_convertibility__triffin_structural_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_dollar_gold_convertibility__triffin_structural_reading, []).

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
 *   constraint_id: dollar_gold_convertibility__triffin_structural_reading
 *   human_readable: Dollar-Gold Convertibility — Triffin Structural Reading
 *   domain: international_political_economy/monetary_history/international_law
 *
 * SUMMARY:
 *   The Bretton Woods system pegged the dollar to gold at $35/ounce and
 *   required other currencies to maintain fixed parities against the dollar.
 *   The Triffin structural reading identifies this as an inherent design
 *   flaw: as the global reserve currency, the U.S. must run persistent
 *   current account deficits to supply world liquidity, but those deficits
 *   undermine confidence in the gold peg. Both the U.S. (forced to choose
 *   between domestic stability and international convertibility) and creditor
 *   nations (forced to accumulate depreciating dollar reserves) are victims
 *   of an arrangement that extracts from both until collapse. The beneficiary
 *   is not a contemporary actor but the post-1971 floating exchange rate
 *   regime that inherits the system's legitimacy after convertibility fails.
 *   The constraint is maintained by active suppression of alternatives
 *   (capital controls, gold pool, SDR proposals) and internalized belief in
 *   dollar centrality.
 *
 * KEY AGENTS:
 *   - united_states_treasury_fed: Primary victim (powerful/trapped) — bears inflation export and gold drain
 *   - creditor_nations_surplus_holders: Primary victim (organized/constrained) — bears reserve depreciation and forced accumulation
 *   - post_bretton_woods_floating_regime: Beneficiary (institutional/arbitrage) — inherits system after collapse
 *   - imf_bis_gold_pool: Agenda setter (institutional/constrained) — administers the failing arrangement
 *   - academic_economists_triffin: Observer (analytical/analytical) — identifies the structural flaw
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dollar_gold_convertibility__triffin_structural_reading, 0.78).
domain_priors:suppression_score(dollar_gold_convertibility__triffin_structural_reading, 0.72).
domain_priors:theater_ratio(dollar_gold_convertibility__triffin_structural_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dollar_gold_convertibility__triffin_structural_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(dollar_gold_convertibility__triffin_structural_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(dollar_gold_convertibility__triffin_structural_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(dollar_gold_convertibility__triffin_structural_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(dollar_gold_convertibility__triffin_structural_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dollar_gold_convertibility__triffin_structural_reading, snare).
narrative_ontology:human_readable(dollar_gold_convertibility__triffin_structural_reading, "Dollar-Gold Convertibility — Triffin Structural Reading").
narrative_ontology:topic_domain(dollar_gold_convertibility__triffin_structural_reading, "international_political_economy/monetary_history/international_law").

domain_priors:requires_active_enforcement(dollar_gold_convertibility__triffin_structural_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dollar_gold_convertibility__triffin_structural_reading, '771f2010-4cb3-42e0-88e4-396846c86a1a').
narrative_ontology:cs_kernel_codification('771f2010-4cb3-42e0-88e4-396846c86a1a', formalized).
narrative_ontology:cs_authority_grounding('771f2010-4cb3-42e0-88e4-396846c86a1a', extraction).
narrative_ontology:cs_interpretation_layer_present('771f2010-4cb3-42e0-88e4-396846c86a1a').
narrative_ontology:cs_reading_relation('771f2010-4cb3-42e0-88e4-396846c86a1a', dollar_gold_convertibility__strict_convertibility_reading, forecloses).
narrative_ontology:cs_reading_relation('771f2010-4cb3-42e0-88e4-396846c86a1a', dollar_gold_convertibility__policy_flexible_reading, influences).
narrative_ontology:cs_axiom('771f2010-4cb3-42e0-88e4-396846c86a1a', foundational, fixed_parity_reserve_currency_is_mathematically_unsustainable).
narrative_ontology:cs_axiom_status(fixed_parity_reserve_currency_is_mathematically_unsustainable, holdable).
narrative_ontology:cs_axiom_grounding('771f2010-4cb3-42e0-88e4-396846c86a1a', fixed_parity_reserve_currency_is_mathematically_unsustainable, empirically_contingent).
narrative_ontology:cs_axiom('771f2010-4cb3-42e0-88e4-396846c86a1a', foundational, both_issuer_and_holders_are_victims_of_triffin_trilemma).
narrative_ontology:cs_axiom_status(both_issuer_and_holders_are_victims_of_triffin_trilemma, holdable).
narrative_ontology:cs_axiom_grounding('771f2010-4cb3-42e0-88e4-396846c86a1a', both_issuer_and_holders_are_victims_of_triffin_trilemma, empirically_contingent).
narrative_ontology:cs_reference_frame('771f2010-4cb3-42e0-88e4-396846c86a1a', bretton_woods_parity_system).
narrative_ontology:cs_drift_state('771f2010-4cb3-42e0-88e4-396846c86a1a', nixon_shock_1971, gap(codification_collapse, severe, false)).
narrative_ontology:cs_created_at('771f2010-4cb3-42e0-88e4-396846c86a1a', '').
narrative_ontology:cs_kernel_id(dollar_gold_convertibility__triffin_structural_reading, dollar_gold_convertibility).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dollar_gold_convertibility__triffin_structural_reading, post_bretton_woods_floating_regime).
narrative_ontology:constraint_victim(dollar_gold_convertibility__triffin_structural_reading, united_states_treasury_fed).
narrative_ontology:constraint_victim(dollar_gold_convertibility__triffin_structural_reading, creditor_nations_surplus_holders).
narrative_ontology:constraint_vindicates(dollar_gold_convertibility__triffin_structural_reading, triffin_dilemma_as_systemic_necessity).
narrative_ontology:constraint_vindicates(dollar_gold_convertibility__triffin_structural_reading, fixed_parity_cannot_sustain_global_reserve_currency).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Must supply global liquidity via current account deficits to maintain the dollar's reserve role, but deficits drain gold reserves and undermine the $35/oz peg. Cannot exit reserve currency status without triggering global monetary collapse. Bears inflation export costs and loss of monetary autonomy. Gold reserves fell from ~20,000t (1945) to ~8,000t (1971).
narrative_ontology:constraint_stakeholder(dollar_gold_convertibility__triffin_structural_reading, united_states_treasury_fed, payer,
    powerful, biographical, trapped, global).

% Accumulate dollar reserves to maintain fixed parities and finance trade. Conversion into gold is legally permitted but politically costly (U.S. retaliation, system disruption). Forced to hold depreciating assets — U.S. inflation exports devalue reserves. France and Germany attempted conversion/redemption; both faced intense diplomatic pressure. Reserve accumulation becomes a tax on their export sectors.
narrative_ontology:constraint_stakeholder(dollar_gold_convertibility__triffin_structural_reading, creditor_nations_surplus_holders, payer,
    organized, biographical, constrained, global).

% The floating exchange rate system that emerges after 1971 inherits the monetary order's legitimacy without the convertibility constraint. Gains policy autonomy for all major currencies, eliminates the Triffin trilemma, and allows reserve diversification. The beneficiary is not a contemporary actor during 1945-1971 but the structural successor that the constraint's collapse enables.
narrative_ontology:constraint_stakeholder(dollar_gold_convertibility__triffin_structural_reading, post_bretton_woods_floating_regime, beneficiary,
    institutional, generational, arbitrage, global).

% Administers the Bretton Woods system: IMF monitors parities, BIS operates the Gold Pool (1961-1968) to cap the London gold price, SDR creation (1969) attempts to supplement reserves. These institutions have formal authority but cannot alter the structural trilemma. Their enforcement activity (gold pool interventions, capital control coordination) sustains the arrangement but they bear reputational costs when it fails.
narrative_ontology:constraint_stakeholder(dollar_gold_convertibility__triffin_structural_reading, imf_bis_gold_pool, agenda_setter,
    institutional, biographical, constrained, global).

% Triffin (1960), Mundell, Johnson, and others identify the structural flaw: reserve currency issuer must choose between domestic stability and international liquidity provision. Their analysis is excluded from policy discourse until the 1968 crisis forces acknowledgment. They neither collect from nor pay into the constraint; they diagnose its inevitable collapse.
narrative_ontology:constraint_stakeholder(dollar_gold_convertibility__triffin_structural_reading, academic_economists_triffin, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provided a stable nominal anchor for postwar reconstruction trade: fixed parities reduced transaction costs and exchange rate uncertainty for 25 years. The dollar served as global reserve currency backed by gold, enabling multilateral payments without bilateral clearing.
% TRANSFER_FUNCTION: Moves real resources from both the U.S. (gold reserves, seigniorage, policy autonomy) and creditor nations (purchasing power of reserves, export sector competitiveness) to maintain the $35/oz parity. The extraction is systemic — no single party captures the gains during operation; the beneficiary is the successor floating regime.
% ABSENT_VOICES: Developing nations (non-G10) had no voice in the Gold Pool or SDR design; their dollar reserves were equally subject to depreciation but they lacked conversion rights. Colonial monetary systems (sterling area, franc zone) were structurally excluded from the convertibility framework. Future generations (post-1971) who inherit the floating regime's benefits and costs were not represented.
% DISAPPEARANCE_RATIONALE: When convertibility ended (1971), the global monetary system reorganized: floating exchange rates replaced fixed parities, the dollar remained reserve currency without gold backing, petrodollar recycling replaced gold convertibility as the demand anchor, and the IMF's role shifted from parity enforcement to surveillance. The world did not return to the pre-Bretton Woods gold standard; a new system emerged from the collapse.
% FOUNDING_PROBLEM: Postwar monetary chaos: competitive devaluations, bilateral clearing, gold scarcity, and the need for a stable multilateral payments system to support reconstruction trade. The U.S. held ~2/3 of world gold reserves and was the only credible anchor.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem (postwar reconstruction finance and trade stability) was corroborated as substantially solved by 1958 (European external convertibility restored, trade volumes exceeded prewar levels) — attested by OECD trade data, IMF annual reports 1958-1960, and contemporary central bank governors (Royale, Blundell). The arrangement persisted 13 years beyond this point, confirmed by Triffin's 1960 testimony to Congress and the 1968 Gold Pool collapse.
narrative_ontology:disappearance_verdict(dollar_gold_convertibility__triffin_structural_reading, world_rearranges).
narrative_ontology:founding_problem_status(dollar_gold_convertibility__triffin_structural_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(dollar_gold_convertibility__triffin_structural_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(dollar_gold_convertibility__triffin_structural_reading, 'none', 1).
narrative_ontology:epsilon_provenance(dollar_gold_convertibility__triffin_structural_reading, 0.78, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(dollar_gold_convertibility__triffin_structural_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(dollar_gold_convertibility__triffin_structural_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(dollar_gold_convertibility__triffin_structural_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.78) because the arrangement systematically transfers real resources from both the U.S. (gold reserves, policy autonomy) and creditors (purchasing power of reserves) to maintain a parity that is mathematically unsustainable. Suppression (0.72) reflects active enforcement: the London Gold Pool (1961-1968), capital controls (Interest Equalization Tax, voluntary restraint programs), and diplomatic pressure on creditors not to convert. Theater ratio is moderate (0.25) — the gold pool and SDR creation were genuine but insufficient attempts to patch the flaw; most enforcement activity defended the parity. Accessibility collapse (0.45) is partial: alternatives (floating, SDR, capital controls) existed but were politically costly. Resistance (0.55) is significant: French gold conversions, German revaluation pressure, academic critique (Triffin, Mundell, Johnson).
 *
 * PERSPECTIVAL GAP:
 *   The U.S. seat experiences the constraint as loss of monetary sovereignty and gold reserves; the creditor seat experiences it as forced accumulation of a depreciating asset. Both are victims but the extraction mechanism differs: the U.S. pays via inflation export and gold outflow; creditors pay via reserve depreciation and opportunity cost of holding dollars. The engine computes this divergence from the structural data — both parties have high directionality (d near 1.0) but for different structural reasons.
 *
 * DIRECTIONALITY LOGIC:
 *   U.S. (powerful/trapped) — d near 1.0: must supply liquidity via deficits, cannot exit reserve currency role without systemic collapse, gold drain is direct extraction. Creditor nations (organized/constrained) — d near 0.9: forced to hold dollars to maintain pegs, conversion right is theoretical (political cost of converting is extreme), reserve depreciation is extraction. Post-Bretton Woods regime (institutional/arbitrage) — d near 0.0: inherits the monetary order after the constraint collapses; the floating regime is the structural beneficiary. IMF/BIS (institutional/constrained) — d near 0.5: administers the system but bears reputational cost of failure.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (postwar monetary stability and reconstruction finance) was largely solved by 1958 (European convertibility restored). The arrangement persisted 13 years beyond its founding purpose, extracting from both parties. The Triffin reading identifies this as mandatrophy: the convertibility obligation became a zombie constraint — its coordination function (stable parities for trade) was overwhelmed by its extraction function (maintaining the dollar's reserve role at the expense of both issuer and holders). Classification as snare (not tangled_rope) reflects that the coordination story was cover by the 1960s; the system persisted only through active suppression of alternatives.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Does the structural unsustainability of convertibility constitute a natural law of monetary systems or a contingent historical arrangement?',
    'Comparative analysis of other reserve currency systems (sterling, hypothetical SDR, euro) — if all exhibit the same trilemma, it approaches natural law; if some avoid it through different institutional design, it is contingent.',
    'If natural law, the constraint is a mountain (or false_summit_mountain if beneficiaries exist); if contingent, it is a snare/tangled_rope maintained by policy choices. This reading treats it as structural inevitability — high extractiveness from both parties until collapse.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Whether Triffin''s trilemma is a necessary feature of any reserve currency system or a design flaw of Bretton Woods specifically').

omega_variable(
    suppression_mechanism_ambiguity,
    'Was the suppression of alternatives (capital controls, SDR substitution, gold pool) structural (U.S. veto power, institutional inertia) or internalized (creditor nations'' belief in dollar centrality)?',
    'Post-1971 trajectory: if creditor nations immediately pursued alternatives after Nixon shock, suppression was structural; if they continued accumulating dollars despite formal convertibility ending, internalized belief played a major role.',
    'If internalized, the constraint''s effective suppression exceeds the structural measure — the victims carried the suppression with them after formal exit.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression in the maintenance of convertibility').

omega_variable(
    extraction_asymmetry_triffin,
    'Is the extraction from the U.S. (inflation export, seigniorage loss) and from creditors (reserve depreciation, forced accumulation) of the same structural kind, or does one party extract from the other?',
    'Sectoral balance accounting: U.S. current account deficits vs. creditor reserve accumulation; inflation differentials; gold outflow data. If both parties lose in net present value terms, extraction is systemic (both victims of the arrangement); if one gains at the other''s expense, it is asymmetric extraction.',
    'If systemic (both lose), the constraint is a snare with no concentrated beneficiary during operation — beneficiary is only the successor regime. If asymmetric, one party is the true beneficiary and the other the victim.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(extraction_asymmetry_triffin, empirical, 'Whether the Triffin constraint extracts symmetrically from both parties or asymmetrically').

omega_variable(
    cs_framing_underdetermination,
    'Is the Bretton Woods kernel best framed as a legal obligation (Article IV), a monetary standard (gold parity), or a geopolitical settlement (U.S. hegemony)?',
    'Compare classification outcomes under each framing: legal obligation framing → strict_convertibility_reading (tangled_rope); monetary standard framing → policy_flexible_reading (scaffold); geopolitical settlement framing → triffin_structural_reading (snare).',
    'Different framings produce different constraint types and different reading_relations. The declared cs_structure commits to the geopolitical settlement framing; the alternative framings are documented here as conceptual omega.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(cs_framing_underdetermination, conceptual, 'Alternative framings of the kernel that would produce different constraint classifications').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dollar_gold_convertibility__triffin_structural_reading, 1945, 1971).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(doll_tr_t1945, dollar_gold_convertibility__triffin_structural_reading, theater_ratio, 1945, 0.05).
narrative_ontology:measurement(doll_tr_t1950, dollar_gold_convertibility__triffin_structural_reading, theater_ratio, 1950, 0.08).
narrative_ontology:measurement(doll_tr_t1958, dollar_gold_convertibility__triffin_structural_reading, theater_ratio, 1958, 0.12).
narrative_ontology:measurement(doll_tr_t1960, dollar_gold_convertibility__triffin_structural_reading, theater_ratio, 1960, 0.18).
narrative_ontology:measurement(doll_tr_t1965, dollar_gold_convertibility__triffin_structural_reading, theater_ratio, 1965, 0.22).
narrative_ontology:measurement(doll_tr_t1968, dollar_gold_convertibility__triffin_structural_reading, theater_ratio, 1968, 0.25).
narrative_ontology:measurement(doll_tr_t1971, dollar_gold_convertibility__triffin_structural_reading, theater_ratio, 1971, 0.25).

% Extraction over time
narrative_ontology:measurement(doll_be_t1945, dollar_gold_convertibility__triffin_structural_reading, base_extractiveness, 1945, 0.15).
narrative_ontology:measurement(doll_be_t1950, dollar_gold_convertibility__triffin_structural_reading, base_extractiveness, 1950, 0.25).
narrative_ontology:measurement(doll_be_t1958, dollar_gold_convertibility__triffin_structural_reading, base_extractiveness, 1958, 0.35).
narrative_ontology:measurement(doll_be_t1960, dollar_gold_convertibility__triffin_structural_reading, base_extractiveness, 1960, 0.48).
narrative_ontology:measurement(doll_be_t1965, dollar_gold_convertibility__triffin_structural_reading, base_extractiveness, 1965, 0.62).
narrative_ontology:measurement(doll_be_t1968, dollar_gold_convertibility__triffin_structural_reading, base_extractiveness, 1968, 0.75).
narrative_ontology:measurement(doll_be_t1971, dollar_gold_convertibility__triffin_structural_reading, base_extractiveness, 1971, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(doll_su_t1945, dollar_gold_convertibility__triffin_structural_reading, suppression_requirement, 1945, 0.3).
narrative_ontology:measurement(doll_su_t1950, dollar_gold_convertibility__triffin_structural_reading, suppression_requirement, 1950, 0.4).
narrative_ontology:measurement(doll_su_t1958, dollar_gold_convertibility__triffin_structural_reading, suppression_requirement, 1958, 0.5).
narrative_ontology:measurement(doll_su_t1960, dollar_gold_convertibility__triffin_structural_reading, suppression_requirement, 1960, 0.6).
narrative_ontology:measurement(doll_su_t1965, dollar_gold_convertibility__triffin_structural_reading, suppression_requirement, 1965, 0.68).
narrative_ontology:measurement(doll_su_t1968, dollar_gold_convertibility__triffin_structural_reading, suppression_requirement, 1968, 0.72).
narrative_ontology:measurement(doll_su_t1971, dollar_gold_convertibility__triffin_structural_reading, suppression_requirement, 1971, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dollar_gold_convertibility__triffin_structural_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(dollar_gold_convertibility__triffin_structural_reading, 0.12).
narrative_ontology:affects_constraint(dollar_gold_convertibility__triffin_structural_reading, dollar_gold_convertibility__strict_convertibility_reading).
narrative_ontology:affects_constraint(dollar_gold_convertibility__triffin_structural_reading, dollar_gold_convertibility__policy_flexible_reading).
narrative_ontology:affects_constraint(dollar_gold_convertibility__triffin_structural_reading, petrodollar_recycling_system).
narrative_ontology:affects_constraint(dollar_gold_convertibility__triffin_structural_reading, special_drawing_rights_allocation).
narrative_ontology:affects_constraint(dollar_gold_convertibility__triffin_structural_reading, plaza_accord_managed_float).

% DUAL FORMULATION NOTE:
% This is one of three readings of the dollar_gold_convertibility kernel. The strict_convertibility_reading frames Article IV as a binding legal obligation (tangled_rope — coordination via legal constraint, extraction via U.S. policy subordination). The policy_flexible_reading frames convertibility as conditional on domestic stability (scaffold — transitional coordination with sunset clause). This reading frames it as structural unsustainability (snare — extraction from both parties until collapse). All three share the same referent (Bretton Woods convertibility) but differ in ε, beneficiary/victim structure, and cs_structure.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(dollar_gold_convertibility__triffin_structural_reading, powerful, 0.95).
constraint_indexing:directionality_override(dollar_gold_convertibility__triffin_structural_reading, organized, 0.9).
constraint_indexing:directionality_override(dollar_gold_convertibility__triffin_structural_reading, institutional, 0.05).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
