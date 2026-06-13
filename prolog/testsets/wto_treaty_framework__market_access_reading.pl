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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: wto_treaty_framework__market_access_reading
 *   human_readable: WTO Market Access Reading: Symmetric Tariff Reduction and Non-Discrimination Obligation
 *   domain: international_trade/development_economics/political_economy
 *
 * SUMMARY:
 *   The WTO treaty framework contains a contested kernel: what is the
 *   treaty's primary obligation, and what is the status of policy space for
 *   development? The MARKET ACCESS READING instantiates one reading: the
 *   treaty's core commitment is symmetric tariff reduction and
 *   non-discrimination; developing countries' Special and Differential (S&D)
 *   provisions are temporary exceptions meant to sunset as transition periods
 *   expire. Under this reading, tariffs, subsidies, and local content rules
 *   are violations of the core obligation, and infant-industry protection is
 *   a temporary indulgence, not a permanent right. This reading privileges
 *   multinational market access and efficient exporters; it compresses policy
 *   space for industrial development. The sibling DEVELOPMENTAL READING
 *   (separate constraint file) reads the same text differently: policy space
 *   for development is an equal-status commitment; S&D provisions are
 *   permanent structural accommodations recognizing asymmetric starting
 *   conditions; technology transfer is core. The two readings coexist in
 *   lived practice — developed countries and multinationals read market
 *   access; developing countries and development economists read
 *   developmental space. The readings are not logically contradictory (both
 *   derive from treaty text), but they are structurally incompatible: a
 *   government cannot simultaneously treat S&D as temporary exception and
 *   permanent right.
 *
 * KEY AGENTS:
 *   - Multinational corporations: primary beneficiary of market access symmetry; operate across tariff bindings; optimize supply chains globally.
 *   - Efficient exporters in developed economies: secondary beneficiaries; already at scale; non-discrimination favors their competitive position.
 *   - Infant industries in developing economies: primary victim; cannot protect themselves beyond S&D margins; compete against entrenched global firms.
 *   - Agricultural producers in LDCs: secondary victim; face tariff reduction asymmetry (developed-country agricultural protection persists despite reading's universalism); trapped in subsistence production.
 *   - Developing-country governments: formally agenda-setters but structurally payers; ratify the treaty but face exit costs (market access loss, capital flight) that constrain their choices.
 *   - Developed-country governments: actual agenda-setters; set the reading's frame; maintain selective protections while demanding symmetry.
 *   - WTO secretariat: administrator and interpreter; benefits from dispute-settlement growth driven by narrow policy-space carve-outs.
 *   - Technology transfer claimants: excluded from core commitments; this reading privileges market mechanisms over mandatory transfer, so they have no seat in the reading's authorship.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(wto_treaty_framework__market_access_reading, 0.68).
domain_priors:suppression_score(wto_treaty_framework__market_access_reading, 0.71).
domain_priors:theater_ratio(wto_treaty_framework__market_access_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(wto_treaty_framework__market_access_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(wto_treaty_framework__market_access_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(wto_treaty_framework__market_access_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(wto_treaty_framework__market_access_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(wto_treaty_framework__market_access_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(wto_treaty_framework__market_access_reading, tangled_rope).
narrative_ontology:human_readable(wto_treaty_framework__market_access_reading, "WTO Market Access Reading: Symmetric Tariff Reduction and Non-Discrimination Obligation").
narrative_ontology:topic_domain(wto_treaty_framework__market_access_reading, "international_trade/development_economics/political_economy").

domain_priors:requires_active_enforcement(wto_treaty_framework__market_access_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(wto_treaty_framework__market_access_reading, '3d556679-e2ef-4186-aa29-5424b8fde59d').
narrative_ontology:cs_kernel_codification('3d556679-e2ef-4186-aa29-5424b8fde59d', formalized).
narrative_ontology:cs_authority_grounding('3d556679-e2ef-4186-aa29-5424b8fde59d', extraction).
narrative_ontology:cs_interpretation_layer_present('3d556679-e2ef-4186-aa29-5424b8fde59d').
narrative_ontology:cs_reading_relation('3d556679-e2ef-4186-aa29-5424b8fde59d', wto_treaty_framework__developmental_reading, coexists_with).
narrative_ontology:cs_axiom('3d556679-e2ef-4186-aa29-5424b8fde59d', foundational, trade_liberalization_symmetric_universal_obligation).
narrative_ontology:cs_axiom_status(trade_liberalization_symmetric_universal_obligation, holdable).
narrative_ontology:cs_axiom_grounding('3d556679-e2ef-4186-aa29-5424b8fde59d', trade_liberalization_symmetric_universal_obligation, conventional).
narrative_ontology:cs_axiom('3d556679-e2ef-4186-aa29-5424b8fde59d', foundational, s_and_d_provisions_temporary_exceptions).
narrative_ontology:cs_axiom_status(s_and_d_provisions_temporary_exceptions, holdable).
narrative_ontology:cs_axiom_grounding('3d556679-e2ef-4186-aa29-5424b8fde59d', s_and_d_provisions_temporary_exceptions, conventional).
narrative_ontology:cs_axiom('3d556679-e2ef-4186-aa29-5424b8fde59d', secondary, non_discrimination_primary_treaty_purpose).
narrative_ontology:cs_axiom_status(non_discrimination_primary_treaty_purpose, holdable).
narrative_ontology:cs_axiom_grounding('3d556679-e2ef-4186-aa29-5424b8fde59d', non_discrimination_primary_treaty_purpose, instrumental).
narrative_ontology:cs_reference_frame('3d556679-e2ef-4186-aa29-5424b8fde59d', symmetric_tariff_binding_framework).
narrative_ontology:cs_drift_state('3d556679-e2ef-4186-aa29-5424b8fde59d', contemporary_post_doha_impasse, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('3d556679-e2ef-4186-aa29-5424b8fde59d', '').
narrative_ontology:cs_kernel_id(wto_treaty_framework__market_access_reading, wto_treaty_framework).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(wto_treaty_framework__market_access_reading, multinational_corporations).
narrative_ontology:constraint_beneficiary(wto_treaty_framework__market_access_reading, efficient_exporters_in_developed_economies).
narrative_ontology:constraint_victim(wto_treaty_framework__market_access_reading, infant_industries_in_developing_economies).
narrative_ontology:constraint_victim(wto_treaty_framework__market_access_reading, agricultural_producers_in_least_developed_countries).
narrative_ontology:constraint_victim(wto_treaty_framework__market_access_reading, domestic_import_substitution_industries).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(wto_treaty_framework__market_access_reading, multinational_trade_law_firms).
narrative_ontology:constraint_victim(wto_treaty_framework__market_access_reading, developing_country_governments).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Access to foreign markets with reduced tariff and non-tariff barriers. Can site production in lowest-cost jurisdictions and move goods globally with minimal duties. Supply chains are optimized across sovereign borders without the friction of local content requirements or infant-industry protection. Collects rents from market access others cannot negotiate symmetrically.
narrative_ontology:constraint_stakeholder(wto_treaty_framework__market_access_reading, multinational_corporations, beneficiary,
    institutional, generational, arbitrage, global).

% Their home markets provide existing technological advantage and economies of scale. Trade rules that lock in non-discrimination favor their competitive position. Export markets open to their products at tariff rates fixed by treaty obligation, eliminating the unilateral tariff autonomy their competitors in developing economies retain (formally, until S&D sunset).
narrative_ontology:constraint_stakeholder(wto_treaty_framework__market_access_reading, efficient_exporters_in_developed_economies, beneficiary,
    powerful, generational, arbitrage, global).

% Cannot protect themselves with tariffs or local content rules beyond the margin S&D permits — and the reading frames S&D as temporary exception, not permanent right. Compete against entrenched global firms already at optimal scale. Their only exit is accepting technology transfer from multinationals as a junior partner in their supply chain, not as an independent producer.
narrative_ontology:constraint_stakeholder(wto_treaty_framework__market_access_reading, infant_industries_in_developing_economies, payer,
    moderate, biographical, constrained, regional).

% Export agricultural goods into markets still heavily protected in developed economies (agricultural exemptions persist despite the reading's universalism claim). Cannot protect their domestic markets from dumped surplus production from subsidized farms in developed countries. Tariff reduction symmetry favors the trade direction already powerful.
narrative_ontology:constraint_stakeholder(wto_treaty_framework__market_access_reading, agricultural_producers_in_least_developed_countries, payer,
    powerless, immediate, trapped, local).

% Formally agenda-setters (they ratify the treaty) but structurally constrained: exit costs (market access loss, capital flight, IMF conditionality) make non-compliance expensive. Binding tariff rates limit industrial policy tools. The reading interprets their S&D provisions as temporary, so policy space erodes as transition periods expire. They execute trade policy within a framework they did not draft and would not write the same way.
narrative_ontology:constraint_stakeholder(wto_treaty_framework__market_access_reading, developing_country_governments, payer,
    organized, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(wto_treaty_framework__market_access_reading, developing_country_governments, agenda_setter).

% Set the treaty's foundational frame: non-discrimination, tariff binding, dispute settlement. Their industries benefit most from the reading's interpretation of S&D as temporary. They maintain selective protections (agricultural subsidies, trade remedies) while demanding symmetry from others. Can invoke dispute settlement with credible enforcement (developed-country enforcement capacity); smaller countries' complaints face longer litigation and retaliation risk.
narrative_ontology:constraint_stakeholder(wto_treaty_framework__market_access_reading, developed_country_governments, agenda_setter,
    institutional, generational, arbitrage, global).

% Trade disputes enriched by the constraint's complexity and enforcement costs. Represent multinational clients in WTO and investor-state disputes. Benefit from the reading's legal rigidity (narrow S&D carve-outs, high burden of proof for policy space exceptions).
narrative_ontology:constraint_stakeholder(wto_treaty_framework__market_access_reading, multinational_trade_law_firms, beneficiary,
    powerful, generational, arbitrage, global).

% Administers the treaty and manages dispute settlement. Interprets the treaty's rules and panels apply them. The reading's frame (non-discrimination, symmetry, S&D temporality) aligns with institutional expertise and jurisdiction expansion. Litigation growth follows from narrow policy-space carve-outs.
narrative_ontology:constraint_stakeholder(wto_treaty_framework__market_access_reading, wto_secretariat, agenda_setter,
    institutional, generational, analytical, global).

% Developing countries and domestic firms seeking technology transfer as part of development rights. This reading excludes technology transfer from the treaty's core commitments (unlike the developmental reading). Their exclusion from agenda-setting is a structural feature: the market-access reading privileges market mechanisms over mandatory transfer, so they would contest the framing but have no seat in its authorship.
narrative_ontology:constraint_stakeholder(wto_treaty_framework__market_access_reading, technology_transfer_claimants, excluded,
    powerless, generational, trapped, global).

% Vulnerable to import surges from subsidized agriculture in developed countries and loss of domestic policy autonomy to protect livelihoods. Have no formal representation in trade negotiations. Would argue for permanent policy space to protect food security and rural employment, but are structurally excluded from agenda-setting.
narrative_ontology:constraint_stakeholder(wto_treaty_framework__market_access_reading, subsistence_agricultural_communities, excluded,
    powerless, immediate, trapped, local).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(wto_treaty_framework__market_access_reading, multinational_corporations).
narrative_ontology:fixing_cost_class(wto_treaty_framework__market_access_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a global rule-set for tariff bindings, non-discrimination, and market access: reduces transaction costs of bilateral negotiation by fixing commitments multilaterally. Provides dispute settlement machinery to enforce agreements. Solves the collective-action problem of unilateral tariff escalation (tariff wars).
% TRANSFER_FUNCTION: Moves market access from developing countries' protected domestic markets to multinational corporations and efficient developed-economy exporters. Transfers policy autonomy from national governments to treaty obligations and WTO panels. Transfers wealth from infant-industry workers and small farmers to multinational supply chains and large-scale exporters.
% ABSENT_VOICES: Technology transfer advocates (excluded from core commitments); subsistence agricultural communities and small farmers (structurally absent from negotiations); developing-country industrial strategists whose preferred policy tools (local content, infant-industry protection, sectoral subsidies) are out of scope; the next generation bearing the constraint's long-term effects on policy space and development capacity.
% DISAPPEARANCE_RATIONALE: If the market-access reading and its enforcement disappeared, developing countries would immediately restore tariff and subsidy tools; multinational supply chains would retrench to home-country or regional production; trade flows would re-segment by region and development level; developed-country exporters would face barriers their firms have optimized around; global market-access rents would collapse. The world's trade topology would reorganize within months.
% FOUNDING_PROBLEM: Post-WWII: reduce tariff barriers and unilateral trade wars; create rules-based trade system preventing 1930s-style protectionist collapse. Establish non-discrimination so all parties gain from opening.
% FOUNDING_PROBLEM_CORROBORATION: Developed-country trade ministries and multinational business associations attest the founding problem (tariff wars, unpredictability) is still live and the reading is the solution. Developing-country governments and development economists attest the founding problem was solved by the 1990s and the reading now persists as asymmetric extraction dressed as universal obligation. Independent development research (UNCTAD, World Bank studies on policy space, academic economics literature on infant-industry protection) documents that the founding tariff-war problem is not the constraint most developing countries face; the constraint they face is compressed policy space. The problem cited by beneficiaries and the problem the constraint actually creates are not the same.
narrative_ontology:disappearance_verdict(wto_treaty_framework__market_access_reading, world_rearranges).
narrative_ontology:founding_problem_status(wto_treaty_framework__market_access_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(wto_treaty_framework__market_access_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(wto_treaty_framework__market_access_reading, 'none', 1).

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
 *   Extractiveness runs 0.68 because the constraint extracts market access from developing countries and domestic policy autonomy from their governments, transferring both to multinational corporations and developed-country exporters. The extraction is not coercive in form (all governments formally consent by ratifying) but structural in operation: the cost of non-compliance (market exclusion, capital flight, IMF conditionality for debt relief) makes consent non-voluntary for many payers. Suppression is high (0.71) because the reading's enforcement mechanisms are costly and asymmetrically applied: developed-country trade remedies and safeguards are tolerated; developing-country infant-industry and food-security protections trigger dispute challenges. Theater is moderate-high (0.42) because the founding justification (preventing tariff wars) is no longer the live problem most developing countries face; enforcement now defends the reading itself rather than the original coordination problem. Measurements show extractiveness and suppression rising from 1995–2020 and plateauing: this reflects both the deepening integration of supply chains into the constraint and the stabilization of enforcement machinery after major disputes (Agriculture Agreement, TRIPS, DSU remedies).
 *
 * PERSPECTIVAL GAP:
 *   The developed-country-government and multinational seats compute this as rope or mild tangled rope: genuine coordination function (tariff bindings reduce uncertainty for exporters) with negotiated asymmetry that developed countries justify as temporary transition support. The developing-country-government seat and infant-industry seat compute this as tangled rope or snare: the coordination story (preventing tariff wars) is cover; the persistent function is extraction of market access and policy space. The WTO secretariat seat computes it as rope with enforcement complexities: the treaty's language is symmetric; disputes arise because parties read it differently. The engine should compute per-seat divergence from the structural data: multinationals and developed exporters have high beneficiary positions (d near 0.0); developing governments and infant industries have high victim positions (d near 1.0); the WTO secretariat has moderate position (moderate power, analytical exit, global scope — derives to d around 0.5 with a beneficiary lean from dispute-processing growth).
 *
 * DIRECTIONALITY LOGIC:
 *   Multinational corporations derive d ≈ 0.1–0.2 (institutional power, arbitrage exit, global scope, beneficiary role, no-cost compliance): the constraint subsidizes their market access; they can forum-shop across jurisdictions if one tightens. Efficient exporters in developed economies derive d ≈ 0.15–0.25 (powerful power, arbitrage exit, global scope, beneficiary role): their existing scale and home-market advantage interact with non-discrimination to lock in competitive dominance. Infant industries derive d ≈ 0.85–0.95 (moderate power, constrained exit, regional scope, victim role): they are trapped by tariff bindings and cannot protect themselves; exiting means abandoning the domestic market entirely. Agricultural producers in LDCs derive d ≈ 0.90–0.98 (powerless, trapped exit, local scope, victim role): they face dumped imports and cannot even maintain subsistence protection. Developing-country governments derive d ≈ 0.70–0.80 (organized power, constrained exit, global scope, mixed payer/agenda-setter role): they are formally agenda-setters but face exit costs that constrain choice; the 'constraint' from a developing-government seat is a binding obligation they cannot unilaterally exit, placing them in the target position. Developed-country governments derive d ≈ 0.25–0.35 (institutional power, arbitrage exit — they can defect and face trade war costs but not capital flight; global scope, agenda-setter role with beneficiary lean): they set the reading's frame and can revise it if it goes against their interest; their exit cost is trade retaliation, not market exclusion. No override needed if the base derivation captures the asymmetry; overrides may be warranted if a developed-country government's agenda-setter role overstates their power relative to domestic constituency pressure to maintain protection.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint's founding problem — preventing 1930s-style tariff escalation and establishing rules-based trade — was solved by the early 1990s. The GATT/WTO system stabilized tariff rates multilaterally and created dispute settlement. By 2000, the founding problem had substantially resolved: tariff wars were not the problem developing countries faced; compressed policy space was. The reading persists as a treaty obligation, its enforcement machinery hardened through DSU jurisprudence (narrow carve-outs for food security, infant-industry protection, public health), and its beneficiaries (multinationals, developed exporters) consolidate gains. Theater rises from 2005–2015 as developing countries invoke S&D exceptions and developed countries litigate them: the reading's frame (symmetry, temporary exceptions) requires enforcement to hold, and that enforcement increasingly looks theatrical — defending the reading itself rather than the founding coordination problem. No mandate has formally been declared obsolete (no GATT/WTO amendment process has succeeded), but mandatrophy is the structural reality: the reading persists by institutional inertia and beneficiary capture, not by problem-solving. An analyst observing that the treaty's founding justification no longer applies to its primary operation would declare the mandate dead and the constraint's persistence mandatrophic.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    s_and_d_temporality_ambiguity,
    'Are S&D provisions genuinely temporary exceptions meant to sunset, or permanent structural accommodations for asymmetric development levels?',
    'The resolution mechanism is treaty practice and negotiating history. The question can be empirically addressed via: (1) textual analysis of S&D sunset clauses (some are time-limited, some are open-ended and re-negotiated); (2) GATT/WTO committee records and negotiating notes on S&D intent; (3) panel and Appellate Body rulings interpreting S&D scope; (4) the behavior of developing countries treating S&D as permanent vs. temporary in their own policy decisions.',
    'If S&D is genuinely temporary, the reading''s temporal frame holds and extractiveness is front-loaded into a transition period, after which equilibrium is established. If S&D is genuinely permanent, the constraint''s extraction is sustained indefinitely and the reading''s claim of universalism is false — the treaty is structurally asymmetric. The computational consequence: a temporary reading reduces extractiveness over the long run (transition period ends, extraction stops); a permanent reading shows sustained extraction regardless of elapsed time.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(s_and_d_temporality_ambiguity, empirical, 'Whether S&D is temporary or permanent.').

omega_variable(
    policy_space_vs_tariff_binding_necessity,
    'Is the compression of policy space (industrial policy, food security, infant-industry protection) a necessary side effect of solving the founding problem (preventing tariff wars), or an incidental extraction enabled by the treaty''s form?',
    'Counterfactual institutional design: could a treaty system prevent tariff escalation without binding tariff rates so tightly that industrial policy becomes impossible? Comparative analysis of regional trade agreements (ASEAN, MERCOSUR, AU) that contain tariff provisions but preserve more policy space; analysis of whether tariff-war risk would recur if tariff bindings were raised or made more flexible.',
    'If policy space compression is necessary, extractiveness is the price of coordination (analogous to the cost of any market infrastructure). If incidental, the constraint carries a separable extractive component beyond what the founding problem requires. A finding of incidentality would support decomposing the constraint into two: a rope (tariff escalation prevention) and a snare (policy space appropriation).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(policy_space_vs_tariff_binding_necessity, conceptual, 'Whether policy space compression is structurally coupled to tariff-war prevention or an independent extraction.').

omega_variable(
    developed_country_agricultural_protection_inconsistency,
    'Why do developed countries maintain substantial agricultural subsidies and tariffs while demanding that developing countries remove infant-industry protection? Is this asymmetry the reading''s feature or a violation of its universalism?',
    'Systematic audit of tariff and subsidy rates by sector and country: compare effective protection rates for agriculture in developed vs. developing countries; trace WTO disputes over agricultural measures and the outcomes; analyze negotiating positions in the Doha Round on agriculture; examine the language of the Agriculture Agreement and how exceptions are crafted to accommodate developed-country protection.',
    'If the asymmetry is deliberate — the reading''s operationalization — then extractiveness is even higher than the base metrics show: the reading claims universalism but practices selectivity. If the asymmetry is inconsistency — developed countries have not lived up to their own reading — then the reading''s enforcement is selective (enforcement burden falls on developing countries), which raises suppression asymmetrically. Either way, the ambiguity reveals the reading''s structural instability: it cannot simultaneously enforce symmetry and permit the selectivity observed in practice.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(developed_country_agricultural_protection_inconsistency, empirical, 'Whether developed-country agricultural protection violates the market-access reading or is consistent with its operationalization.').

omega_variable(
    technology_transfer_exclusion_justification,
    'The market-access reading does not include technology transfer as a core commitment (unlike the developmental reading). Why is market access obligatory but technology transfer left to voluntary corporate contracts?',
    'Genealogical analysis: trace how technology transfer was debated in GATT/WTO texts and why it was excluded from binding commitments; compare with other international regimes (UN conferences on technology transfer, regional trade agreements with TT provisions). Examine whether the exclusion reflects genuine negotiating symmetry or structural imbalance (did developing countries lack leverage to secure TT commitments?).',
    'If technology transfer was excluded because both parties genuinely preferred market-based mechanisms, the reading''s shape reflects a coherent trade-off. If excluded because developed countries blocked it (imbalance of bargaining power), the reading is asymmetrically enforced: market access is mandatory; development capacity remains discretionary. The computational consequence: a power-imbalanced exclusion is an additional extraction layer — not just compression of policy space, but exclusion of capacity-building commitments.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(technology_transfer_exclusion_justification, empirical, 'Whether technology transfer exclusion reflects negotiating symmetry or power imbalance.').

omega_variable(
    contested_kernel_committer_framing,
    'This reading is one instantiation of a contested kernel (WTO treaty framework). The sibling developmental reading instantiates the same text but derives different structural consequences. Is the market-access reading the correct interpretation, or is it a reading imposed by those with power to enforce it?',
    'The question cannot be resolved empirically because both readings derive from the same treaty text and both are formally defensible. The resolution mechanism is political: whose reading becomes operative (enforced by dispute settlement, internalized by governments, taught in law schools, built into institutional practice). The question surfaces via: (1) historical analysis of negotiating intent (whose conception prevailed?); (2) institutional analysis of who enforces the reading (developed-country governments and multinationals can credibly enforce market access; developing countries cannot credibly enforce developmental space); (3) drift tracking — do the actual operations of the treaty system consistently favor one reading''s consequences?',
    'If the market-access reading prevails structurally (enforcement machinery favors it, institutional practice operationalizes it), the constraint''s persistence depends on beneficiary power, not on the reading''s intrinsic legitimacy. If the developmental reading were operationalized instead, the same treaty text would yield a different constraint entirely (policy space preserved, technology transfer obligatory, infant-industry protection permanent). The computational consequence: the reading''s stability is parasitic on the power distribution, not on the text''s meaning.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(contested_kernel_committer_framing, conceptual, 'Whether the market-access reading is the treaty''s correct interpretation or a reading imposed by beneficiary power.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(wto_treaty_framework__market_access_reading, 1995, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(wto__tr_t1995, wto_treaty_framework__market_access_reading, theater_ratio, 1995, 0.25).
narrative_ontology:measurement_basis(wto__tr_t1995, observed).
narrative_ontology:measurement(wto__tr_t2000, wto_treaty_framework__market_access_reading, theater_ratio, 2000, 0.28).
narrative_ontology:measurement_basis(wto__tr_t2000, observed).
narrative_ontology:measurement(wto__tr_t2005, wto_treaty_framework__market_access_reading, theater_ratio, 2005, 0.32).
narrative_ontology:measurement_basis(wto__tr_t2005, observed).
narrative_ontology:measurement(wto__tr_t2010, wto_treaty_framework__market_access_reading, theater_ratio, 2010, 0.37).
narrative_ontology:measurement_basis(wto__tr_t2010, observed).
narrative_ontology:measurement(wto__tr_t2015, wto_treaty_framework__market_access_reading, theater_ratio, 2015, 0.41).
narrative_ontology:measurement_basis(wto__tr_t2015, observed).
narrative_ontology:measurement(wto__tr_t2020, wto_treaty_framework__market_access_reading, theater_ratio, 2020, 0.42).
narrative_ontology:measurement_basis(wto__tr_t2020, observed).
narrative_ontology:measurement(wto__tr_t2024, wto_treaty_framework__market_access_reading, theater_ratio, 2024, 0.42).
narrative_ontology:measurement_basis(wto__tr_t2024, observed).

% Extraction over time
narrative_ontology:measurement(wto__be_t1995, wto_treaty_framework__market_access_reading, base_extractiveness, 1995, 0.52).
narrative_ontology:measurement_basis(wto__be_t1995, observed).
narrative_ontology:measurement(wto__be_t2000, wto_treaty_framework__market_access_reading, base_extractiveness, 2000, 0.57).
narrative_ontology:measurement_basis(wto__be_t2000, observed).
narrative_ontology:measurement(wto__be_t2005, wto_treaty_framework__market_access_reading, base_extractiveness, 2005, 0.61).
narrative_ontology:measurement_basis(wto__be_t2005, observed).
narrative_ontology:measurement(wto__be_t2010, wto_treaty_framework__market_access_reading, base_extractiveness, 2010, 0.65).
narrative_ontology:measurement_basis(wto__be_t2010, observed).
narrative_ontology:measurement(wto__be_t2015, wto_treaty_framework__market_access_reading, base_extractiveness, 2015, 0.67).
narrative_ontology:measurement_basis(wto__be_t2015, observed).
narrative_ontology:measurement(wto__be_t2020, wto_treaty_framework__market_access_reading, base_extractiveness, 2020, 0.68).
narrative_ontology:measurement_basis(wto__be_t2020, observed).
narrative_ontology:measurement(wto__be_t2024, wto_treaty_framework__market_access_reading, base_extractiveness, 2024, 0.68).
narrative_ontology:measurement_basis(wto__be_t2024, observed).

% Suppression requirement over time
narrative_ontology:measurement(wto__su_t1995, wto_treaty_framework__market_access_reading, suppression_requirement, 1995, 0.58).
narrative_ontology:measurement_basis(wto__su_t1995, observed).
narrative_ontology:measurement(wto__su_t2000, wto_treaty_framework__market_access_reading, suppression_requirement, 2000, 0.62).
narrative_ontology:measurement_basis(wto__su_t2000, observed).
narrative_ontology:measurement(wto__su_t2005, wto_treaty_framework__market_access_reading, suppression_requirement, 2005, 0.66).
narrative_ontology:measurement_basis(wto__su_t2005, observed).
narrative_ontology:measurement(wto__su_t2010, wto_treaty_framework__market_access_reading, suppression_requirement, 2010, 0.69).
narrative_ontology:measurement_basis(wto__su_t2010, observed).
narrative_ontology:measurement(wto__su_t2015, wto_treaty_framework__market_access_reading, suppression_requirement, 2015, 0.71).
narrative_ontology:measurement_basis(wto__su_t2015, observed).
narrative_ontology:measurement(wto__su_t2020, wto_treaty_framework__market_access_reading, suppression_requirement, 2020, 0.71).
narrative_ontology:measurement_basis(wto__su_t2020, observed).
narrative_ontology:measurement(wto__su_t2024, wto_treaty_framework__market_access_reading, suppression_requirement, 2024, 0.71).
narrative_ontology:measurement_basis(wto__su_t2024, observed).

% Leveled coercion grid (OQ-93): 32/32 authored points at t0=1995, tn=2024
narrative_ontology:measurement(wto__grid_01, wto_treaty_framework__market_access_reading, accessibility_collapse(class), 1995, 0.45).
narrative_ontology:measurement(wto__grid_02, wto_treaty_framework__market_access_reading, accessibility_collapse(class), 2024, 0.71).
narrative_ontology:measurement(wto__grid_03, wto_treaty_framework__market_access_reading, accessibility_collapse(individual), 1995, 0.42).
narrative_ontology:measurement(wto__grid_04, wto_treaty_framework__market_access_reading, accessibility_collapse(individual), 2024, 0.65).
narrative_ontology:measurement(wto__grid_05, wto_treaty_framework__market_access_reading, accessibility_collapse(organizational), 1995, 0.52).
narrative_ontology:measurement(wto__grid_06, wto_treaty_framework__market_access_reading, accessibility_collapse(organizational), 2024, 0.68).
narrative_ontology:measurement(wto__grid_07, wto_treaty_framework__market_access_reading, accessibility_collapse(structural), 1995, 0.48).
narrative_ontology:measurement(wto__grid_08, wto_treaty_framework__market_access_reading, accessibility_collapse(structural), 2024, 0.62).
narrative_ontology:measurement(wto__grid_09, wto_treaty_framework__market_access_reading, resistance(class), 1995, 0.78).
narrative_ontology:measurement(wto__grid_10, wto_treaty_framework__market_access_reading, resistance(class), 2024, 0.72).
narrative_ontology:measurement(wto__grid_11, wto_treaty_framework__market_access_reading, resistance(individual), 1995, 0.68).
narrative_ontology:measurement(wto__grid_12, wto_treaty_framework__market_access_reading, resistance(individual), 2024, 0.58).
narrative_ontology:measurement(wto__grid_13, wto_treaty_framework__market_access_reading, resistance(organizational), 1995, 0.75).
narrative_ontology:measurement(wto__grid_14, wto_treaty_framework__market_access_reading, resistance(organizational), 2024, 0.68).
narrative_ontology:measurement(wto__grid_15, wto_treaty_framework__market_access_reading, resistance(structural), 1995, 0.68).
narrative_ontology:measurement(wto__grid_16, wto_treaty_framework__market_access_reading, resistance(structural), 2024, 0.62).
narrative_ontology:measurement(wto__grid_17, wto_treaty_framework__market_access_reading, stakes_inflation(class), 1995, 0.48).
narrative_ontology:measurement(wto__grid_18, wto_treaty_framework__market_access_reading, stakes_inflation(class), 2024, 0.75).
narrative_ontology:measurement(wto__grid_19, wto_treaty_framework__market_access_reading, stakes_inflation(individual), 1995, 0.52).
narrative_ontology:measurement(wto__grid_20, wto_treaty_framework__market_access_reading, stakes_inflation(individual), 2024, 0.82).
narrative_ontology:measurement(wto__grid_21, wto_treaty_framework__market_access_reading, stakes_inflation(organizational), 1995, 0.45).
narrative_ontology:measurement(wto__grid_22, wto_treaty_framework__market_access_reading, stakes_inflation(organizational), 2024, 0.68).
narrative_ontology:measurement(wto__grid_23, wto_treaty_framework__market_access_reading, stakes_inflation(structural), 1995, 0.38).
narrative_ontology:measurement(wto__grid_24, wto_treaty_framework__market_access_reading, stakes_inflation(structural), 2024, 0.52).
narrative_ontology:measurement(wto__grid_25, wto_treaty_framework__market_access_reading, suppression(class), 1995, 0.62).
narrative_ontology:measurement(wto__grid_26, wto_treaty_framework__market_access_reading, suppression(class), 2024, 0.78).
narrative_ontology:measurement(wto__grid_27, wto_treaty_framework__market_access_reading, suppression(individual), 1995, 0.58).
narrative_ontology:measurement(wto__grid_28, wto_treaty_framework__market_access_reading, suppression(individual), 2024, 0.72).
narrative_ontology:measurement(wto__grid_29, wto_treaty_framework__market_access_reading, suppression(organizational), 1995, 0.58).
narrative_ontology:measurement(wto__grid_30, wto_treaty_framework__market_access_reading, suppression(organizational), 2024, 0.75).
narrative_ontology:measurement(wto__grid_31, wto_treaty_framework__market_access_reading, suppression(structural), 1995, 0.48).
narrative_ontology:measurement(wto__grid_32, wto_treaty_framework__market_access_reading, suppression(structural), 2024, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(wto_treaty_framework__market_access_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(wto_treaty_framework__market_access_reading, wto_treaty_framework__developmental_reading).

% DUAL FORMULATION NOTE:
% The WTO treaty framework kernel has two structurally distinct readings: (1) MARKET ACCESS READING (this file): trade liberalization as symmetric universal obligation; S&D provisions as temporary exceptions; non-discrimination as primary purpose. High extractiveness from developing countries' policy space; multinational corporations primary beneficiaries. (2) DEVELOPMENTAL READING (separate constraint): policy space for development as equal-status commitment; S&D provisions as permanent structural accommodations; technology transfer as core commitment. Different victim sets, different enforcement patterns, different beneficiary structures. Both readings derive from the same WTO text; neither is logically foreclosed by the other, but they are operationally incompatible. The two readings coexist in lived practice as competing international law interpretations. Link them via affects_constraints to enable contamination analysis and to surface the kernel contest in corpus-level analysis.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
