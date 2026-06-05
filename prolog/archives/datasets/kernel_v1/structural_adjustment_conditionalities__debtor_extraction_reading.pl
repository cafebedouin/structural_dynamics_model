% ============================================================================
% CONSTRAINT STORY: structural_adjustment_conditionalities__debtor_extraction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_structural_adjustment_conditionalities__debtor_extraction_reading, []).

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
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: structural_adjustment_conditionalities__debtor_extraction_reading
 *   human_readable: Structural Adjustment Conditionalities as Debtor Extraction (Neo-Colonial Reading)
 *   domain: international_political_economy/development_finance/institutional_economics
 *
 * SUMMARY:
 *   Structural adjustment conditionalities (SACs) — the policy packages
 *   imposed by the IMF and World Bank on debtor nations as a condition of
 *   refinancing — are contested at the most fundamental level. This reading
 *   instantiates the debtor-extraction frame: conditionalities are mechanisms
 *   through which transnational capital and creditor banks extract resources
 *   from debtor-state populations by dismantling social contracts,
 *   privatizing public assets, suppressing wages, and concentrating
 *   decision-making power in creditor-controlled institutions. The reading
 *   does not claim all debt relationships are extraction — it claims that the
 *   specific architecture of SACs, as implemented from the 1980s onward,
 *   systematically distributes costs downward (to vulnerable populations,
 *   labor, public services) and benefits upward (to creditors, multinational
 *   corporations acquiring privatized assets, finance capital). The
 *   constraint exhibits high extractiveness (0.78), high suppression (0.82),
 *   and theater that has increased over time (0.45 → 0.68) as the technical
 *   language of 'structural adjustment' has thickened while the
 *   distributional consequences have remained constant. The
 *   base_extractiveness trajectory reflects the acceleration of SAC packages
 *   post-1989 (after the Cold War removed alternative financing and
 *   geopolitical leverage for debtor states) and the accumulation of
 *   extractive precedent as creditors learned which policy packages faced
 *   least resistance.
 *
 * KEY AGENTS:
 *   - Debtor state populations: primary victims (powerless/trapped) — lose public services, wage protections, subsidies, democratic control; cannot exit or negotiate
 *   - Debtor state governments: secondary victims (moderate/constrained) — face pressure to implement conditionalities; high cost of refusal but occasional successful resistance (Argentina, Malaysia, Ecuador)
 *   - Transnational capital and creditor banks: primary beneficiaries (institutional/arbitrage) — capture interest payments, privatization opportunities, asset appreciation from capital account opening; can refinance or move capital elsewhere
 *   - Multinational corporations: secondary beneficiaries (powerful/mobile) — acquire privatized public assets at below-replacement value; enjoy labor market liberalization and reduced environmental regulation; high exit options
 *   - IMF/World Bank institutional apparatus: institutional actor (institutional/arbitrage) — enforces conditionalities through control of refinancing; uses technical language (macroeconomic stability, fiscal sustainability) to legitimize distributional choices; benefits from institutional prestige and organizational inertia
 *   - Analytical observer: sees the structural pattern (analytical/analytical) — recognizes conditionalities as a contingent institutional arrangement that could be otherwise; rejects the 'natural law' framing as false summit
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(structural_adjustment_conditionalities__debtor_extraction_reading, 0.78).
domain_priors:suppression_score(structural_adjustment_conditionalities__debtor_extraction_reading, 0.82).
domain_priors:theater_ratio(structural_adjustment_conditionalities__debtor_extraction_reading, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(structural_adjustment_conditionalities__debtor_extraction_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(structural_adjustment_conditionalities__debtor_extraction_reading, suppression_requirement, 0.82).
narrative_ontology:constraint_metric(structural_adjustment_conditionalities__debtor_extraction_reading, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(structural_adjustment_conditionalities__debtor_extraction_reading, snare).
narrative_ontology:human_readable(structural_adjustment_conditionalities__debtor_extraction_reading, "Structural Adjustment Conditionalities as Debtor Extraction (Neo-Colonial Reading)").
narrative_ontology:topic_domain(structural_adjustment_conditionalities__debtor_extraction_reading, "international_political_economy/development_finance/institutional_economics").

domain_priors:requires_active_enforcement(structural_adjustment_conditionalities__debtor_extraction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(structural_adjustment_conditionalities__debtor_extraction_reading, '18ff55e7-3f70-4251-82f4-a8cec575b728').
narrative_ontology:cs_kernel_codification('18ff55e7-3f70-4251-82f4-a8cec575b728', formalized).
narrative_ontology:cs_authority_grounding('18ff55e7-3f70-4251-82f4-a8cec575b728', extraction).
narrative_ontology:cs_interpretation_layer_present('18ff55e7-3f70-4251-82f4-a8cec575b728').
narrative_ontology:cs_reading_relation('18ff55e7-3f70-4251-82f4-a8cec575b728', structural_adjustment_conditionalities__creditor_coordination_reading, coexists_with).
narrative_ontology:cs_reading_relation('18ff55e7-3f70-4251-82f4-a8cec575b728', structural_adjustment_conditionalities__hybrid_selectivity_reading, influences).
narrative_ontology:cs_axiom('18ff55e7-3f70-4251-82f4-a8cec575b728', foundational, creditor_authority_constructed_via_extraction).
narrative_ontology:cs_axiom_status(creditor_authority_constructed_via_extraction, holdable).
narrative_ontology:cs_axiom_grounding('18ff55e7-3f70-4251-82f4-a8cec575b728', creditor_authority_constructed_via_extraction, empirically_contingent).
narrative_ontology:cs_axiom('18ff55e7-3f70-4251-82f4-a8cec575b728', foundational, distributional_asymmetry_is_structural_not_incidental).
narrative_ontology:cs_axiom_status(distributional_asymmetry_is_structural_not_incidental, holdable).
narrative_ontology:cs_axiom_grounding('18ff55e7-3f70-4251-82f4-a8cec575b728', distributional_asymmetry_is_structural_not_incidental, empirically_contingent).
narrative_ontology:cs_reference_frame('18ff55e7-3f70-4251-82f4-a8cec575b728', debtor_state_sovereignty_and_social_contract).
narrative_ontology:cs_drift_state('18ff55e7-3f70-4251-82f4-a8cec575b728', contemporary_neoliberal_consolidation, gap(authority_erosion, severe, false)).
narrative_ontology:cs_created_at('18ff55e7-3f70-4251-82f4-a8cec575b728', '2026-02-26T00:00:00Z').
narrative_ontology:cs_kernel_id(structural_adjustment_conditionalities__debtor_extraction_reading, structural_adjustment_conditionalities).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(structural_adjustment_conditionalities__debtor_extraction_reading, transnational_capital).
narrative_ontology:constraint_beneficiary(structural_adjustment_conditionalities__debtor_extraction_reading, creditor_banks).
narrative_ontology:constraint_beneficiary(structural_adjustment_conditionalities__debtor_extraction_reading, multinational_corporations).
narrative_ontology:constraint_victim(structural_adjustment_conditionalities__debtor_extraction_reading, debtor_state_populations).
narrative_ontology:constraint_victim(structural_adjustment_conditionalities__debtor_extraction_reading, domestic_public_services).
narrative_ontology:constraint_victim(structural_adjustment_conditionalities__debtor_extraction_reading, labor_organizing_capacity).
narrative_ontology:constraint_victim(structural_adjustment_conditionalities__debtor_extraction_reading, national_democratic_sovereignty).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DEBTOR STATE POPULATION (SNARE) — Domestic populations experience conditionalities as coercive extraction: healthcare and education budgets cut under IMF directives, wage suppression enforced via 'labor market flexibility' clauses, public goods dismantled for privatization, subsidies on essential goods removed. Exit is impossible — the debtor state has no sovereign control over policy; populations cannot exit the nation-state. Extraction is maximal and undisguised once the coercive mechanism (debt servicing conditionality) is activated.
constraint_indexing:constraint_classification(structural_adjustment_conditionalities__debtor_extraction_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: DEBTOR STATE GOVERNMENT (SNARE) — The state's leadership faces high but not insurmountable costs for refusal (capital flight, loss of investment, refinancing crisis, loss of IMF/World Bank access for all future borrowing). Many do refuse — Argentina in 2001, Malaysia in 1997, Ecuador in 2006 defaulted or rejected conditionalities. But refusal carries immense damage: immediate fiscal crisis, currency collapse, capital withdrawal. The state is constrained, not trapped, but the constraint is severe enough that few governments refuse once debt is accumulated. Effective extraction remains snare-level because refusal triggers coordinated lender punishment.
constraint_indexing:constraint_classification(structural_adjustment_conditionalities__debtor_extraction_reading, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: CREDITOR BANK CONSORTIUM (ROPE) — Creditors experience conditionalities as a coordination mechanism: the IMF/World Bank apparatus guarantees debt repayment through enforced policy compliance and provides access to new lending. The extraction flow runs toward creditors (interest payments, debt servicing, asset seizures via privatization). Creditors have arbitrage — they can refinance, sell debt to other investors, or accept haircuts. They see the system as coordination (enforcing contracts, maintaining asset values), not as coercive extraction. The IMF/World Bank appear as neutral technical advisors, not as the enforcement arm of the extraction regime.
constraint_indexing:constraint_classification(structural_adjustment_conditionalities__debtor_extraction_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: MULTINATIONAL CAPITAL / PRIVATIZATION BENEFICIARIES (TANGLED ROPE) — Multinational corporations and foreign investors benefit enormously from conditionality-driven privatization (telecommunications, utilities, minerals, ports sold at fire-sale prices). They also bear some constraint: the privatized assets remain dependent on debtor-state political stability, and popular resistance creates operational risk. From this perspective, conditionalities provide genuine coordination (opening markets to capital, removing state capacity for worker protection or environmental regulation) plus substantial extraction (acquiring public assets at below-replacement value). Exit is high — capital can move to new investment opportunities — so extraction is not maximal (not snare-level), but the extraction mechanism is real and substantial.
constraint_indexing:constraint_classification(structural_adjustment_conditionalities__debtor_extraction_reading, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: IMF/WORLD BANK INSTITUTIONAL APPARATUS (PITON) — The IMF/World Bank frame conditionalities as technical requirements for macroeconomic stability and sound governance. The rhetoric emphasizes coordination (stabilizing currency, controlling inflation, achieving fiscal sustainability) rather than extraction. Theater is high because the 'technical' language masks the distributional consequences: inflation control via wage suppression, fiscal sustainability via public service cuts, market opening via sell-offs of state assets. The apparatus has largely internalized the belief in its own technical neutrality, despite decades of evidence that conditionality outcomes concentrate extraction toward creditors and away from debtor populations. The institutional machinery persists through inertia and self-protective narratives despite degraded legitimacy.
constraint_indexing:constraint_classification(structural_adjustment_conditionalities__debtor_extraction_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: NATURAL LAW READING / NEOCLASSICAL VIEW (MOUNTAIN) — From the neoclassical economics perspective, conditionalities are inevitable structural requirements: countries that borrow beyond their means must adjust spending or face currency collapse and capital flight. The 'conditionality' is not imposed — it is a natural consequence of intertemporal resource constraints and rational creditor behavior. Removing conditionalities would be impossible, not just undesirable — it would violate fundamental laws of macroeconomics. This perspective treats the extraction pattern as a law of nature rather than a contingent institutional arrangement. However, the base_properties and structural data (identifiable beneficiaries, measurable policy choices by IMF/World Bank, alternative debt restructuring models) reveal this as a false summit: the 'naturalness' is contingent on treating creditor interests as the measure of macroeconomic rationality.
constraint_indexing:constraint_classification(structural_adjustment_conditionalities__debtor_extraction_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(structural_adjustment_conditionalities__debtor_extraction_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(structural_adjustment_conditionalities__debtor_extraction_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(structural_adjustment_conditionalities__debtor_extraction_reading, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(structural_adjustment_conditionalities__debtor_extraction_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(structural_adjustment_conditionalities__debtor_extraction_reading, TR),
    TR >= 0.70.

:- end_tests(structural_adjustment_conditionalities__debtor_extraction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.78): Very high. The reading defines extractiveness as the asymmetric redistribution of resources from debtor populations to creditors through conditionality mechanisms. The 0.78 value reflects that SAC packages are designed to transfer resources (via debt servicing, privatization, wage suppression, currency devaluation benefits to creditors) from vulnerable populations. This is not a transient inefficiency — it is the structural outcome of the mechanism. The trajectory from 0.55 (early 1980s SACs, still contested and resisted) to 0.78 (late 2000s, normalized and expanded) reflects that creditors learned which packages faced least organized resistance and doubled down. Suppression (0.82): Very high. Conditionalities systematically reduce alternative capacity: elimination of subsidies removes escape routes for vulnerable populations; privatization removes public service employment; 'labor market flexibility' (union busting + wage controls) removes organizing capacity; exchange-rate devaluation impoverishes debtor-state workers while enriching exporters (typically foreign-owned); capital account opening enables capital flight, removing domestic investment alternatives. The suppression trajectory (0.65 → 0.82) reflects that initial SAC packages faced organized resistance (labor strikes, riots, capital flight of domestic elites); later packages incorporated mechanisms to fragment and preempt resistance. Theater ratio (0.45 → 0.68): Increasing over time. Early SACs were presented as temporary technical adjustments; by the 2000s, the rhetoric had shifted to permanent institutional reform and 'good governance,' creating more elaborate legitimating narratives while the extractive mechanism remained constant. The IMF/World Bank apparatus has become more sophisticated in framing extraction as technical necessity.
 *
 * PERSPECTIVAL GAP:
 *   This reading produces the maximum perspectival gap possible. The debtor population sees pure extraction (Snare: coercive, no negotiation, no exit). The creditor banks see coordination (Rope: mutual benefit through debt discipline and asset discipline). The multinational corporations see mixed coordination and extraction (Tangled Rope: some genuine market opening alongside asset acquisition). The IMF/World Bank institutional apparatus sees technical necessity (Piton: performative review of 'good governance' while the extraction mechanism hums beneath). The analytical observer sees a false summit — a claim that extraction is a natural law (Mountain) contradicted by the existence of alternative institutional arrangements. The perspectival divergence reveals that there is no neutral classification of SACs: the reading choice reflects a position on who bears adjustment costs and who benefits from creditor authority.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values (d) are derived from structural position: debtor populations have zero exit options (trapped → d = 0.95) and are victims of extraction (d increases toward 1.0). Creditor banks have high exit options (arbitrage → d = 0.05) and are beneficiaries (d decreases toward 0.0). The sigmoid f(d) transforms these to experienced extraction: trapped victims experience χ = 0.78 × f(0.95) × σ(national) ≈ 0.78 × 1.42 × 1.0 ≈ 1.10 (extraction exceeds ε due to victim vulnerability), while beneficiary creditors experience χ = 0.78 × f(0.05) × σ(global) ≈ 0.78 × (-0.12) × 1.2 ≈ -0.11 (negative extraction, i.e., benefit). The perspectival gap emerges from the divergence in d values: the debtor state and its population occupy positions of maximum extraction potential; the creditors occupy positions of maximum benefit; there is no perspective from which both experience the constraint symmetrically.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading resolves the mandatrophy (ε > 0.70 requires `mandatrophy_resolved: true`) by declaring explicitly that this is ONE READING of a contested kernel, not a universal claim. The debtor_extraction_reading treats SACs as a snare — high extraction, high suppression, coercive mechanism. The creditor_coordination_reading would treat SACs as a rope or tangled rope — genuine coordination around debt discipline with some distributional asymmetry. The hybrid_selectivity_reading would distinguish which conditionalities achieve real macroeconomic stabilization (coordination) versus which extract beyond what stabilization requires. The three readings are NOT all correct simultaneously for any single observer — they represent different frames. An observer can move between frames (reposition themselves) but cannot occupy all three at once. The mandatrophy is resolved by the kernel structure: SACs are a contested commitment, and classification depends on which reading's epistemic frame is adopted.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    debtor_voluntariness_ambiguity,
    'At what point does constrained acceptance of loan conditions transition from voluntary agreement to coerced compliance?',
    'Historical analysis of debtor state''s decision-making process: whether alternative borrowing sources existed, whether domestic political consent was manufactured through information control, whether IMF accessed internal government communications to pressure specific policy choices, whether conditionality packages were presented as take-it-or-leave-it ultimata versus negotiated terms.',
    'If transition point < 2% alternative borrowing options: reading implies near-total coercion (extraction mechanism is pure snare without negotiation layer). If transition point > 50% alternatives: reading implies significant debtor agency (extraction is contested, not absolute).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(debtor_voluntariness_ambiguity, empirical, 'Voluntariness threshold in debtor acceptance of conditionalities').

omega_variable(
    alternative_debt_restructuring_feasibility,
    'Could coordinated debtor default, debt restructuring, or alternative financing (China, regional development banks) have prevented the extraction pattern this reading describes?',
    'Counterfactual analysis: comparison of outcomes between countries that accepted IMF conditionalities versus those that rejected them (Argentina 2001-2003, Malaysia 1997-1998, Ecuador 2006-2008); measurement of debt sustainability under alternative restructuring scenarios; analysis of creditor coordination necessary to enforce current regime versus fragility if creditor unity breaks.',
    'If alternatives were materially viable: reading is correct — extraction regime depends on creditor coordination and debtor coordination failure. If alternatives were truly impossible: reading overstates volition — some extraction is inevitable from debt mechanics rather than choice.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(alternative_debt_restructuring_feasibility, empirical, 'Whether alternative debt structures could prevent the extraction pattern').

omega_variable(
    distributional_versus_aggregate_efficiency,
    'Do IMF conditionalities actually improve debtor-state macroeconomic outcomes, or do they distribute costs to vulnerable populations while improving creditor returns?',
    'Empirical comparison: growth rates, poverty outcomes, inequality measures, health/education indicators pre- and post-conditionality for debtor states versus matched non-debtor controls; measurement of who bears adjustment costs (labor income, public employment, subsidy removal) versus who captures benefits (asset prices, capital returns, debt service reduction).',
    'If aggregate growth improves: conditionalities may be Pareto-suboptimal but not pure extraction (tangled rope possible). If growth stalls and distribution becomes more unequal: reading is correct — extractive redistribution from debtor populations to creditors with no aggregate gain.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(distributional_versus_aggregate_efficiency, empirical, 'Whether conditionalities improve macroeconomic outcomes or only redistribute toward creditors').

omega_variable(
    creditor_belief_in_technical_necessity,
    'Do IMF/World Bank officials genuinely believe conditionalities are technical requirements for stability, or do they consciously use stability language to justify distributional goals favoring creditors?',
    'Institutional ethnography: interviews with IMF/World Bank staff on their reasoning for specific policy packages; access to internal deliberation records; analysis of whether conditionality packages match technical stability requirements or creditor preference profiles; historical tracing of how conditionality packages changed post-1980s neoliberal turn.',
    'If genuine technical belief: reading overstates intentional extraction (institutional captures itself via false ideology). If conscious use of technical language for distributional purposes: reading''s snare classification is correct — the apparatus knowingly coordinates extraction.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(creditor_belief_in_technical_necessity, empirical, 'Whether creditors believe in technical necessity of conditionalities or use it as cover').

omega_variable(
    kernel_contest_location,
    'Where exactly is the contest between the debtor_extraction_reading and its sibling readings located?',
    'Structural mapping: the extraction_reading posits that conditionalities are designed to extract from debtor populations for creditor benefit; the coordination_reading posits that conditionalities solve a real coordination problem (mutual benefit through macroeconomic stabilization); the hybrid_selectivity_reading posits that some conditionalities achieve real coordination while others extract. The contest is empirical (do conditionalities improve outcomes?), normative (who should bear adjustment costs?), and institutional (what are the IMF/World Bank actually optimizing for?).',
    'This omega documents that the kernel itself is contested — there is no neutral way to classify conditionalities without taking a position on creditor-debtor relationships. The reading declared here is the extraction-focused frame; other frames are structurally coherent but different.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_contest_location, conceptual, 'The structural location of the reading contest in the conditionalities kernel').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(structural_adjustment_conditionalities__debtor_extraction_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sac_debtor_theater_t0, structural_adjustment_conditionalities__debtor_extraction_reading, theater_ratio, 0, 0.45).
narrative_ontology:measurement(sac_debtor_theater_t10, structural_adjustment_conditionalities__debtor_extraction_reading, theater_ratio, 10, 0.58).
narrative_ontology:measurement(sac_debtor_theater_t20, structural_adjustment_conditionalities__debtor_extraction_reading, theater_ratio, 20, 0.68).

% Extraction over time
narrative_ontology:measurement(sac_debtor_extract_t0, structural_adjustment_conditionalities__debtor_extraction_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(sac_debtor_extract_t10, structural_adjustment_conditionalities__debtor_extraction_reading, base_extractiveness, 10, 0.72).
narrative_ontology:measurement(sac_debtor_extract_t20, structural_adjustment_conditionalities__debtor_extraction_reading, base_extractiveness, 20, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(sac_debtor_supp_t0, structural_adjustment_conditionalities__debtor_extraction_reading, suppression_requirement, 0, 0.65).
narrative_ontology:measurement(sac_debtor_supp_t10, structural_adjustment_conditionalities__debtor_extraction_reading, suppression_requirement, 10, 0.78).
narrative_ontology:measurement(sac_debtor_supp_t20, structural_adjustment_conditionalities__debtor_extraction_reading, suppression_requirement, 20, 0.82).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(structural_adjustment_conditionalities__debtor_extraction_reading, resource_allocation).
narrative_ontology:affects_constraint(structural_adjustment_conditionalities__debtor_extraction_reading, debt_overhang_labor_suppression).
narrative_ontology:affects_constraint(structural_adjustment_conditionalities__debtor_extraction_reading, privatization_asset_fire_sales).
narrative_ontology:affects_constraint(structural_adjustment_conditionalities__debtor_extraction_reading, capital_account_opening_currency_crisis).
narrative_ontology:affects_constraint(structural_adjustment_conditionalities__debtor_extraction_reading, structural_adjustment_conditionalities__creditor_coordination_reading).
narrative_ontology:affects_constraint(structural_adjustment_conditionalities__debtor_extraction_reading, structural_adjustment_conditionalities__hybrid_selectivity_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of a contested kernel. The sibling readings (creditor_coordination_reading, hybrid_selectivity_reading) represent alternative frames on the same institutional phenomenon. Each reading has its own constraint story with its own ε value and perspectives. The network links show this story's relationship to (1) downstream constraints affected by SAC implementation (labor suppression, privatization, currency crises), and (2) sibling readings in the same kernel family. This story is the extraction-focused frame; the sibling stories frame SACs as coordination or hybrid mechanisms.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(structural_adjustment_conditionalities__debtor_extraction_reading, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
