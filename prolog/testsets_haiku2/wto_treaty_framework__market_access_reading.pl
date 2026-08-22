% ============================================================================
% CONSTRAINT STORY: wto_treaty_framework__market_access_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_wto_treaty_framework__market_access_reading, []).

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
 *   constraint_id: wto_treaty_framework__market_access_reading
 *   human_readable: WTO Treaty Framework: Market Access Reading
 *   domain: international_trade/development_economics
 *
 * SUMMARY:
 *   The WTO treaty framework instantiates two competing readings of the same
 *   kernel commitment to multilateral trade governance. The market-access
 *   reading frames the treaty as a symmetric, universal obligation to remove
 *   tariffs and discriminatory policies, with Special and Differential (S&D)
 *   provisions treated as temporary exceptions rather than structural
 *   accommodations. Under this reading, trade liberalization is the primary
 *   purpose; non-discrimination and predictable market access are the core
 *   commitments; and developing countries' policy constraints are authored as
 *   transitional rather than permanent. This reading benefits multinational
 *   corporations and capital-exporting countries by removing the unilateral
 *   tools (infant-industry tariffs, local-content rules, technology-transfer
 *   mandates) that less-developed economies used historically to
 *   industrialize. The constraint compresses policy space for development and
 *   locks in asymmetric obligations. The developmental reading (sibling
 *   constraint, not this one) frames the treaty as an accommodation of
 *   asymmetric starting conditions, with S&D provisions as permanent
 *   structural features and policy space for development as an equal-status
 *   commitment—a fundamentally different interpretation of the same treaty
 *   text. The kernel contest sits at the intersection of trade law
 *   interpretation and development economics.
 *
 * KEY AGENTS:
 *   - multinational_corporations: Primary beneficiary; gain secure global market access without tariff walls or local-content barriers
 *   - capital_exporting_countries: Institutional beneficiary; their negotiators authored the market-access reading into the treaty; enforce it through dispute settlement
 *   - infant_industries: Primary victim; face mature global competition without the tariff protection or subsidies that enabled industrialization elsewhere
 *   - developing_country_governments: Institutional victim; relinquished policy tools (industrial policy, infant-industry protection, technology-transfer mandates); policy space is compressed
 *   - WTO Dispute Settlement Body: Agenda-setter; operationalizes the market-access reading through case law; rulings define compliance
 *   - excluded_alternative_development_models: Structurally precluded; state-directed industrialization and strategic infant-industry protection are not debated, they are authored as violations
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(wto_treaty_framework__market_access_reading, 0.78).
domain_priors:suppression_score(wto_treaty_framework__market_access_reading, 0.71).
domain_priors:theater_ratio(wto_treaty_framework__market_access_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(wto_treaty_framework__market_access_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(wto_treaty_framework__market_access_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(wto_treaty_framework__market_access_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(wto_treaty_framework__market_access_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(wto_treaty_framework__market_access_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(wto_treaty_framework__market_access_reading, tangled_rope).
narrative_ontology:human_readable(wto_treaty_framework__market_access_reading, "WTO Treaty Framework: Market Access Reading").
narrative_ontology:topic_domain(wto_treaty_framework__market_access_reading, "international_trade/development_economics").

domain_priors:requires_active_enforcement(wto_treaty_framework__market_access_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(wto_treaty_framework__market_access_reading, '3fcc49f1-81cc-4d1f-8e83-12d7b7ab0811').
narrative_ontology:cs_kernel_codification('3fcc49f1-81cc-4d1f-8e83-12d7b7ab0811', fixed_text).
narrative_ontology:cs_authority_grounding('3fcc49f1-81cc-4d1f-8e83-12d7b7ab0811', extraction).
narrative_ontology:cs_interpretation_layer_present('3fcc49f1-81cc-4d1f-8e83-12d7b7ab0811').
narrative_ontology:cs_reading_relation('3fcc49f1-81cc-4d1f-8e83-12d7b7ab0811', wto_treaty_framework__developmental_reading, coexists_with).
narrative_ontology:cs_axiom('3fcc49f1-81cc-4d1f-8e83-12d7b7ab0811', foundational, market_access_symmetry_is_primary_purpose).
narrative_ontology:cs_axiom_status(market_access_symmetry_is_primary_purpose, holdable).
narrative_ontology:cs_axiom_grounding('3fcc49f1-81cc-4d1f-8e83-12d7b7ab0811', market_access_symmetry_is_primary_purpose, instrumental).
narrative_ontology:cs_axiom('3fcc49f1-81cc-4d1f-8e83-12d7b7ab0811', foundational, sd_provisions_are_temporary_exceptions).
narrative_ontology:cs_axiom_status(sd_provisions_are_temporary_exceptions, holdable).
narrative_ontology:cs_axiom_grounding('3fcc49f1-81cc-4d1f-8e83-12d7b7ab0811', sd_provisions_are_temporary_exceptions, conventional).
narrative_ontology:cs_reference_frame('3fcc49f1-81cc-4d1f-8e83-12d7b7ab0811', symmetric_multilateral_trade_liberalization).
narrative_ontology:cs_drift_state('3fcc49f1-81cc-4d1f-8e83-12d7b7ab0811', contemporary_dispute_settlement_jurisprudence, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('3fcc49f1-81cc-4d1f-8e83-12d7b7ab0811', '').
narrative_ontology:cs_kernel_id(wto_treaty_framework__market_access_reading, wto_treaty_framework).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(wto_treaty_framework__market_access_reading, multinational_corporations).
narrative_ontology:constraint_beneficiary(wto_treaty_framework__market_access_reading, capital_exporting_countries).
narrative_ontology:constraint_beneficiary(wto_treaty_framework__market_access_reading, industrial_exporters).
narrative_ontology:constraint_victim(wto_treaty_framework__market_access_reading, infant_industries).
narrative_ontology:constraint_victim(wto_treaty_framework__market_access_reading, developing_countries_with_policy_constraints).
narrative_ontology:constraint_victim(wto_treaty_framework__market_access_reading, domestic_agricultural_producers_in_liberalizing_states).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(wto_treaty_framework__market_access_reading, developing_country_governments).
narrative_ontology:constraint_beneficiary(wto_treaty_framework__market_access_reading, domestic_agricultural_producers_in_liberalizing_states).
narrative_ontology:constraint_victim(wto_treaty_framework__market_access_reading, developing_country_governments).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Gain secure, predictable access to markets worldwide without tariff walls or discriminatory local-content rules. Can freely relocate supply chains, source components globally, and export to protected markets. The constraint removes the unilateral tools governments used to favor domestic firms or protect infant industries from foreign competition.
narrative_ontology:constraint_stakeholder(wto_treaty_framework__market_access_reading, multinational_corporations, beneficiary,
    institutional, generational, arbitrage, global).

% Authored and enforce the treaty framework. Their negotiators set the agenda around market access symmetry and non-discrimination as primary. Enforce it through dispute settlement (WTO Dispute Settlement Body) and interpret S&D provisions narrowly. Retain capacity to define what counts as 'temporary' transition. Also carry domestic constituencies harmed by liberalization, but the constraint structure binds commitments forward.
narrative_ontology:constraint_stakeholder(wto_treaty_framework__market_access_reading, capital_exporting_countries, agenda_setter,
    institutional, generational, arbitrage, global).

% Face immediate competition from established multinational producers in their home market without the tariff protection or subsidies that historically helped industries mature elsewhere. Must compete at global scale before achieving economies of scale. Their governments are contractually barred from using policy tools (infant-industry tariffs, local-content requirements, technology-transfer mandates) that enabled industrialization in now-developed countries. Exit would mean abandoning the development strategy; staying means incurring losses during maturation.
narrative_ontology:constraint_stakeholder(wto_treaty_framework__market_access_reading, infant_industries, payer,
    moderate, generational, identity_locked, national).

% Signed the treaty and are bound to it. Bear the cost of compressed policy space for industrial development and infant-industry protection. Access to export markets under non-discriminatory rules is a benefit they share with all signatories, but the asymmetry is in the tools they relinquished relative to the tools available to now-developed countries during their own industrialization. Withdrawal from the treaty is politically and economically costly (sanctions, loss of preferential trade access).
narrative_ontology:constraint_stakeholder(wto_treaty_framework__market_access_reading, developing_country_governments, payer,
    organized, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(wto_treaty_framework__market_access_reading, developing_country_governments, beneficiary).

% Face tariff elimination and reduced subsidy capacity, which exposes them to lower-cost imports from subsidized competitors and agricultural exporters. They also benefit from access to cheaper inputs and potentially expanded export markets in other liberalizing countries. The constraint locks in the tariff commitments and shapes domestic subsidy policy; exit requires treaty withdrawal, politically costly.
narrative_ontology:constraint_stakeholder(wto_treaty_framework__market_access_reading, domestic_agricultural_producers_in_liberalizing_states, payer,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(wto_treaty_framework__market_access_reading, domestic_agricultural_producers_in_liberalizing_states, beneficiary).

% Interprets whether national policies comply with the treaty's non-discrimination and market-access obligations. Applies the market-access reading (symmetric, universal, S&D temporary) through case law. Its rulings operationalize the constraint; states that lose cases face remedies (tariff retaliation, compliance orders).
narrative_ontology:constraint_stakeholder(wto_treaty_framework__market_access_reading, wto_dispute_settlement_body, agenda_setter,
    institutional, generational, analytical, global).

% Argue for infant-industry protection, strategic industrial policy, and alternative development models. These arguments are structurally precluded by the treaty's non-discrimination obligation and market-access commitment; they are not debated in the treaty forum, they are authored as violations. Their exclusion from the treaty's primary framing means their analytical frameworks do not shape the constraint's interpretation.
narrative_ontology:constraint_stakeholder(wto_treaty_framework__market_access_reading, development_economists_and_heterodox_schools, excluded,
    moderate, biographical, trapped, global).

% Represent workers and domestic firms displaced by trade liberalization. Would advocate for tariff protection, local-content rules, and policy space preservation but are structurally excluded from the treaty's negotiation and interpretation. Their adjustment costs (unemployment, deindustrialization) are real but not represented in the constraint's core framing.
narrative_ontology:constraint_stakeholder(wto_treaty_framework__market_access_reading, labor_movements_and_domestic_industry_associations, excluded,
    moderate, biographical, trapped, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(wto_treaty_framework__market_access_reading, multinational_corporations).
narrative_ontology:fixing_cost_class(wto_treaty_framework__market_access_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes predictable, transparent, uniform rules for trade across all signatories: tariff bindings, non-discrimination principles (most-favored-nation and national treatment), and dispute resolution. Reduces transaction costs of bilateral negotiation by creating a single multilateral framework.
% TRANSFER_FUNCTION: Moves market access and policy space from developing and infant-industry states to multinational corporations and capital-exporting countries. Transfers the ability to use tariffs, subsidies, local-content rules, and selective investment screening from national governments to the constraint's beneficiaries. Transfers adjustment costs (unemployment, industry displacement) to workers and domestic firms in liberalizing states.
% ABSENT_VOICES: Domestic producers in liberalizing states, labor movements, development economists advocating infant-industry protection, and representatives of alternative industrialization models are structurally excluded from the treaty's primary framing. They would argue for policy space preservation and asymmetric obligations reflecting different starting conditions but are not seated at the table where the market-access reading is defined and enforced.
% DISAPPEARANCE_RATIONALE: If the non-discrimination and market-access obligations disappeared, developing countries would immediately restore infant-industry tariffs, local-content rules, and technology-transfer mandates to accelerate industrialization; multinational corporations would face closed markets and tariff walls; and industrial policy would revert to the pre-GATT toolkit. Global supply chains would reorganize around protected blocs.
% FOUNDING_PROBLEM: Post-WWII economic chaos: protectionism and retaliatory tariff wars had contributed to the Great Depression; the treaty was designed to prevent a return to beggar-thy-neighbor trade policies and create a rules-based multilateral system with predictable, binding commitments.
% FOUNDING_PROBLEM_CORROBORATION: Developed-country governments and multinational corporations attest the founding problem remains live—without binding commitments and dispute resolution, trade wars and protectionism would return. Developing-country governments and development economists attest the founding problem was solved (tariff wars have not returned) but the constraint persists as a mechanism to lock in asymmetric policy space compression; independent scholarship documents that developing countries used industrial policy during their own catch-up (South Korea, Taiwan, China) and that the constraint now forecloses that option for others, suggesting mandate drift or deliberate asymmetric locking.
narrative_ontology:disappearance_verdict(wto_treaty_framework__market_access_reading, world_rearranges).
narrative_ontology:founding_problem_status(wto_treaty_framework__market_access_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(wto_treaty_framework__market_access_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(wto_treaty_framework__market_access_reading, 'none', 1).
narrative_ontology:epsilon_provenance(wto_treaty_framework__market_access_reading, 0.78, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(wto_treaty_framework__market_access_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(wto_treaty_framework__market_access_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(wto_treaty_framework__market_access_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.78) because the constraint transfers substantial policy space and market access from developing states to multinational corporations without compensating mechanism (S&D provisions are authored as temporary, not permanent). The measurement series (1995–2025) shows extractiveness rising incrementally as dispute settlement cases narrow S&D interpretations and as developing countries fully internalize the constraint. Suppression is substantial (0.71) because enforcement depends on active dispute settlement machinery—losing states face tariff retaliation or remedies—and on the structural elimination of exit options (withdrawal is politically/economically prohibitive). Theater ratio rises moderately (0.25→0.42 over the interval) as the Doha Development Agenda was launched and stalled: official rhetoric frames the treaty as pro-development and S&D as meaningful, but the operative mechanism (market access, non-discrimination, dispute enforcement) continues to compress policy space. The measurement grid is shared across all three metrics; every metric is authored at every examined time point (1995, 2001, 2008, 2015, 2020, 2025) so the temporal analysis is well-grounded.
 *
 * PERSPECTIVAL GAP:
 *   The perspective divergence between beneficiary and victim seats is acute. Multinational corporations and developed-country governments perceive the constraint as genuine coordination—a rules-based system that reduces transaction costs and enables predictable trade. Developing-country governments and domestic producers perceive it as enforced extraction—asymmetric obligations that compress their policy space while developed countries (now rich) used similar instruments during their own industrialization. The divergence is not empirically resolvable because it tracks a real structural asymmetry: the constraint IS coordination (it does reduce transaction costs, establish predictability, prevent tariff wars) AND extraction (it does transfer policy space and market access asymmetrically). The engine computes per-seat classification from the structural data; the divergence is the point.
 *
 * DIRECTIONALITY LOGIC:
 *   Multinational corporations sit at the full-beneficiary end (d ≈ 0.0–0.2): they pay nothing into the constraint's operation, market access flows to them, and exit costs them nothing (they can operate under any trade regime). Developing-country governments sit at the full-target end (d ≈ 0.85–0.95): they relinquished policy tools, face dispute settlement penalties, and exit is trapped (withdrawal triggers sanctions and loss of preferential access). The asymmetry in the directionality vectors is structural: developed-country negotiators authored the framework to favor their firms and lock in commitments that constrain future competitors. Capital-exporting countries sit near the beneficiary end (d ≈ 0.2): their firms benefit from the constraint, and they retain quasi-arbitrage capacity (they authored the treaty's interpretation and enforce it). Infant industries and domestic producers sit near the target end (d ≈ 0.8–0.9): they bear adjustment costs, face tariff elimination, and have no exit other than sector abandonment.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (post-WWII protectionism and tariff wars) is authored as contested status: developed countries attest it remains live; development economists and developing countries attest it is solved but the constraint persists as a mechanism to lock in asymmetric policy space compression. The disappearance verdict is world_rearranges (the constraint's removal would trigger immediate policy reversals and supply-chain reorganization), which is consistent with a constraint whose founding problem is contested but whose mandate has partially drifted—the mechanism still solves the coordination problem (preventing tariff wars) but has accrued an extraction function (locking in asymmetric policy space compression). This is textbook mandatrophy: the founding mandate (reduce protectionism) remains partially live, but the operative constraint also extracts in ways not originally anticipated. The theater ratio (0.25→0.42) captures this drift: official rhetoric invokes the founding problem and S&D provisions as meaningful, but dispute settlement and enforcement focus on market access and non-discrimination, the extraction mechanisms.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sd_permanence_vs_temporality,
    'Are S&D provisions structurally permanent accommodations of asymmetric starting conditions, or temporary transition measures toward symmetric market opening?',
    'Case law progression in WTO dispute settlement: if panels and the Appellate Body progressively narrow S&D interpretations (interpret exemptions restrictively, require sunset timelines, deny new S&D claims), the temporal reading is operationalized. If interpretations stabilize or widen exemptions as development theory advances, the permanent-accommodation reading is operationalized.',
    'If S&D are permanent-structural, the constraint''s extraction is lower and the developmental reading gains plausibility; if temporary, extraction is higher and the market-access reading is operationalized. This is the primary interpretive battleground.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sd_permanence_vs_temporality, empirical, 'Whether S&D provisions are temporary exceptions or permanent structural accommodations.').

omega_variable(
    policy_space_asymmetry,
    'Did capital-exporting countries use industrial policy (tariffs, subsidies, local-content rules, technology-transfer mandates) during their own industrialization, and is the constraint''s compression of these tools for developing countries asymmetrically applied?',
    'Historical economic analysis comparing the policy toolkit available to now-developed countries during catch-up (1800s–1950s) with the toolkit legally available to developing countries under the treaty (1995–present). If developed countries used substantially more protective instruments during industrialization than developing countries are permitted now, the asymmetry is established.',
    'If asymmetry is established, the constraint''s extraction is reframed as locking-in of competitive advantage rather than genuine symmetric obligation; if no asymmetry is found, the market-access reading''s framing as symmetric gains plausibility.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(policy_space_asymmetry, empirical, 'Whether the constraint imposes asymmetric policy constraints relative to the historical toolkit used by now-developed countries.').

omega_variable(
    infant_industry_necessity_debate,
    'Is infant-industry protection genuinely necessary for industrial catch-up in low-income countries, or can development occur through unprotected market integration and FDI inflows?',
    'Cross-national development trajectories post-1995: comparison of countries that maintained policy space (China via WTO carve-outs, India via extended transitions) vs. countries that fully liberalized. If protected-space countries achieve faster industrial catch-up, the necessity claim is supported; if outcomes are equivalent, the market-access reading''s argument that protection is unnecessary is supported.',
    'If protection is necessary, the constraint''s compression of policy space is a genuine harm to development and extraction is higher; if unnecessary, the harm is lower and the coordination benefits of market access become more prominent.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(infant_industry_necessity_debate, empirical, 'Whether infant-industry protection is functionally necessary for development or an obsolete mercantilist practice.').

omega_variable(
    reading_logical_structure,
    'Do the market-access reading and the developmental reading represent logically distinct interpretations of the same treaty text, or do they rest on incompatible axioms that cannot coexist in a single legal framework?',
    'Jurisprudential analysis: can a panel rule apply both readings to the same fact pattern (S&D as both temporary AND permanent, for instance), or does commitment to one reading logically foreclose the other? If the readings can coexist in treaty interpretation (different panels reach different conclusions based on different axioms but both are doctrinally coherent), they coexist_with; if one forecloses the other, they foreclose.',
    'If they truly foreclose, the treaty kernel is fundamentally unstable and one reading will eventually dominate through doctrinal evolution or renegotiation. If they coexist, the constraint''s type remains contested and the engine''s classification must account for reading-dependence.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_logical_structure, conceptual, 'Whether the market-access and developmental readings are logically compatible within a single interpretive framework.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(wto_treaty_framework__market_access_reading, 1995, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(wto__tr_t1995, wto_treaty_framework__market_access_reading, theater_ratio, 1995, 0.25).
narrative_ontology:measurement_basis(wto__tr_t1995, observed).
narrative_ontology:measurement(wto__tr_t2001, wto_treaty_framework__market_access_reading, theater_ratio, 2001, 0.28).
narrative_ontology:measurement_basis(wto__tr_t2001, observed).
narrative_ontology:measurement(wto__tr_t2008, wto_treaty_framework__market_access_reading, theater_ratio, 2008, 0.33).
narrative_ontology:measurement_basis(wto__tr_t2008, observed).
narrative_ontology:measurement(wto__tr_t2015, wto_treaty_framework__market_access_reading, theater_ratio, 2015, 0.38).
narrative_ontology:measurement_basis(wto__tr_t2015, observed).
narrative_ontology:measurement(wto__tr_t2020, wto_treaty_framework__market_access_reading, theater_ratio, 2020, 0.4).
narrative_ontology:measurement_basis(wto__tr_t2020, observed).
narrative_ontology:measurement(wto__tr_t2025, wto_treaty_framework__market_access_reading, theater_ratio, 2025, 0.42).
narrative_ontology:measurement_basis(wto__tr_t2025, observed).

% Extraction over time
narrative_ontology:measurement(wto__be_t1995, wto_treaty_framework__market_access_reading, base_extractiveness, 1995, 0.64).
narrative_ontology:measurement_basis(wto__be_t1995, observed).
narrative_ontology:measurement(wto__be_t2001, wto_treaty_framework__market_access_reading, base_extractiveness, 2001, 0.68).
narrative_ontology:measurement_basis(wto__be_t2001, observed).
narrative_ontology:measurement(wto__be_t2008, wto_treaty_framework__market_access_reading, base_extractiveness, 2008, 0.72).
narrative_ontology:measurement_basis(wto__be_t2008, observed).
narrative_ontology:measurement(wto__be_t2015, wto_treaty_framework__market_access_reading, base_extractiveness, 2015, 0.75).
narrative_ontology:measurement_basis(wto__be_t2015, observed).
narrative_ontology:measurement(wto__be_t2020, wto_treaty_framework__market_access_reading, base_extractiveness, 2020, 0.76).
narrative_ontology:measurement_basis(wto__be_t2020, observed).
narrative_ontology:measurement(wto__be_t2025, wto_treaty_framework__market_access_reading, base_extractiveness, 2025, 0.78).
narrative_ontology:measurement_basis(wto__be_t2025, observed).

% Suppression requirement over time
narrative_ontology:measurement(wto__su_t1995, wto_treaty_framework__market_access_reading, suppression_requirement, 1995, 0.58).
narrative_ontology:measurement_basis(wto__su_t1995, observed).
narrative_ontology:measurement(wto__su_t2001, wto_treaty_framework__market_access_reading, suppression_requirement, 2001, 0.61).
narrative_ontology:measurement_basis(wto__su_t2001, observed).
narrative_ontology:measurement(wto__su_t2008, wto_treaty_framework__market_access_reading, suppression_requirement, 2008, 0.64).
narrative_ontology:measurement_basis(wto__su_t2008, observed).
narrative_ontology:measurement(wto__su_t2015, wto_treaty_framework__market_access_reading, suppression_requirement, 2015, 0.68).
narrative_ontology:measurement_basis(wto__su_t2015, observed).
narrative_ontology:measurement(wto__su_t2020, wto_treaty_framework__market_access_reading, suppression_requirement, 2020, 0.7).
narrative_ontology:measurement_basis(wto__su_t2020, observed).
narrative_ontology:measurement(wto__su_t2025, wto_treaty_framework__market_access_reading, suppression_requirement, 2025, 0.71).
narrative_ontology:measurement_basis(wto__su_t2025, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(wto_treaty_framework__market_access_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(wto_treaty_framework__market_access_reading, 0.12).
narrative_ontology:affects_constraint(wto_treaty_framework__market_access_reading, wto_treaty_framework__developmental_reading).

% DUAL FORMULATION NOTE:
% The wto_treaty_framework kernel decomposes into two structurally distinct constraints based on how S&D provisions and policy space for development are interpreted. The market-access reading (this constraint) frames trade liberalization as primary and S&D as temporary exceptions; extractiveness is high (0.78) because policy space is compressed and asymmetrically assigned. The developmental reading frames policy space as permanent and equal-status; extractiveness is lower because S&D are structural and technology-transfer obligations are core. The same treaty text instantiates both constraints depending on which interpretive axis is in effect. These are not measurement-basis variants; they are different ε values on the same referent (the WTO treaty framework). The epsilon-invariance principle requires two separate stories.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(wto_treaty_framework__market_access_reading, moderate, 0.88).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
