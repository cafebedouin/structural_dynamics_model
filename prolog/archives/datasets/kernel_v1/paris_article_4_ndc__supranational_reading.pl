% ============================================================================
% CONSTRAINT STORY: paris_article_4_ndc__supranational_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_paris_article_4_ndc__supranational_reading, []).

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
 *   constraint_id: paris_article_4_ndc__supranational_reading
 *   human_readable: NDC Binding Commitment Architecture (Supranational Reading)
 *   domain: international_climate_governance/treaty_law/political_economy
 *
 * SUMMARY:
 *   The Paris Agreement's Article 4 NDC architecture, interpreted through the
 *   supranational reading, establishes binding commitments on a ratcheting
 *   trajectory toward net-zero with international accountability mechanisms.
 *   Under this reading, states are no longer sovereign agents freely choosing
 *   climate policy — they are subjects of a supranational carbon governance
 *   system that systematically extracts from high-emission sectors and
 *   fossil-fuel-dependent economies while redistributing benefits to
 *   climate-stabilized regions and green technology exporters. The constraint
 *   combines genuine coordination function (climate stabilization is a
 *   collective good; the ratchet creates credible commitment devices) with
 *   asymmetric extraction (carbon-intensive industries face regulatory
 *   extinction; incumbent fossil-fuel incumbents face stranded assets;
 *   workers in extractive sectors face economic displacement without
 *   guaranteed transitions). The supranational reading emphasizes the binding
 *   nature of commitments (states cannot unilaterally exit without
 *   reputational and financial penalties) and the ratcheting mechanism
 *   (successive NDCs must represent increased stringency, foreclosing the
 *   option of stabilizing at a less-ambitious baseline). This reading
 *   contrasts sharply with the sovereigntist reading (which treats NDCs as
 *   aspirational targets states can modify) and the equity reading (which
 *   prioritizes historical responsibility and differentiated obligations over
 *   binding universality).
 *
 * KEY AGENTS:
 *   - Carbon-Intensive Industries: Primary victim (powerless/trapped) — face regulatory extinction and stranded assets; no viable exit
 *   - Fossil-Fuel-Dependent States: Secondary victim (moderate/constrained) — high restructuring costs; constrained by dependence on carbon revenue
 *   - Green Technology Exporters: Primary beneficiary (institutional/arbitrage) — capture demand from global decarbonization; arbitrage through licensing and manufacturing relocation
 *   - Least Developed Countries Coalition: Mixed actor (organized/constrained) — benefit from climate finance and avoided climate impacts; constrained by dependence on fossil suppliers and adaptation requirements
 *   - Article 6 Carbon Market System: Temporary mechanism (institutional/arbitrage) — creates price signals and transition pathways; designed with implicit sunset
 *   - UNFCCC International Climate Bureaucracy: Institutional maintainer (institutional/arbitrage) — maintains reporting/verification infrastructure; perverse incentives toward performative compliance
 *   - Analytical Observer: Universalist perspective (analytical/analytical) — sees binding force as emergent from physical necessity; risks naturalizing contingent institutional design
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(paris_article_4_ndc__supranational_reading, 0.58).
domain_priors:suppression_score(paris_article_4_ndc__supranational_reading, 0.62).
domain_priors:theater_ratio(paris_article_4_ndc__supranational_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(paris_article_4_ndc__supranational_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(paris_article_4_ndc__supranational_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(paris_article_4_ndc__supranational_reading, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(paris_article_4_ndc__supranational_reading, tangled_rope).
narrative_ontology:human_readable(paris_article_4_ndc__supranational_reading, "NDC Binding Commitment Architecture (Supranational Reading)").
narrative_ontology:topic_domain(paris_article_4_ndc__supranational_reading, "international_climate_governance/treaty_law/political_economy").

domain_priors:requires_active_enforcement(paris_article_4_ndc__supranational_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(paris_article_4_ndc__supranational_reading, 'bdc5b7a1-216b-4f4c-8cc8-038f1531408d').
narrative_ontology:cs_kernel_codification('bdc5b7a1-216b-4f4c-8cc8-038f1531408d', formalized).
narrative_ontology:cs_authority_grounding('bdc5b7a1-216b-4f4c-8cc8-038f1531408d', extraction).
narrative_ontology:cs_interpretation_layer_present('bdc5b7a1-216b-4f4c-8cc8-038f1531408d').
narrative_ontology:cs_reading_relation('bdc5b7a1-216b-4f4c-8cc8-038f1531408d', paris_article_4_ndc__sovereigntist_reading, forecloses).
narrative_ontology:cs_reading_relation('bdc5b7a1-216b-4f4c-8cc8-038f1531408d', paris_article_4_ndc__equity_reading, influences).
narrative_ontology:cs_axiom('bdc5b7a1-216b-4f4c-8cc8-038f1531408d', foundational, ndc_commitments_are_supranationally_binding).
narrative_ontology:cs_axiom_status(ndc_commitments_are_supranationally_binding, holdable).
narrative_ontology:cs_axiom_grounding('bdc5b7a1-216b-4f4c-8cc8-038f1531408d', ndc_commitments_are_supranationally_binding, conventional).
narrative_ontology:cs_axiom('bdc5b7a1-216b-4f4c-8cc8-038f1531408d', foundational, ratchet_mechanism_forecloses_low_ambition_equilibrium).
narrative_ontology:cs_axiom_status(ratchet_mechanism_forecloses_low_ambition_equilibrium, holdable).
narrative_ontology:cs_axiom_grounding('bdc5b7a1-216b-4f4c-8cc8-038f1531408d', ratchet_mechanism_forecloses_low_ambition_equilibrium, instrumental).
narrative_ontology:cs_axiom('bdc5b7a1-216b-4f4c-8cc8-038f1531408d', secondary, carbon_intensive_sector_extinction_is_necessary_consequence).
narrative_ontology:cs_axiom_status(carbon_intensive_sector_extinction_is_necessary_consequence, holdable).
narrative_ontology:cs_axiom_grounding('bdc5b7a1-216b-4f4c-8cc8-038f1531408d', carbon_intensive_sector_extinction_is_necessary_consequence, empirically_contingent).
narrative_ontology:cs_reference_frame('bdc5b7a1-216b-4f4c-8cc8-038f1531408d', sovereign_state_climate_cooperation_framework).
narrative_ontology:cs_drift_state('bdc5b7a1-216b-4f4c-8cc8-038f1531408d', post_2020_ndc_enhancement_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('bdc5b7a1-216b-4f4c-8cc8-038f1531408d', '').
narrative_ontology:cs_kernel_id(paris_article_4_ndc__supranational_reading, paris_article_4_ndc).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(paris_article_4_ndc__supranational_reading, global_climate_stabilization).
narrative_ontology:constraint_beneficiary(paris_article_4_ndc__supranational_reading, least_developed_countries).
narrative_ontology:constraint_beneficiary(paris_article_4_ndc__supranational_reading, green_technology_exporters).
narrative_ontology:constraint_victim(paris_article_4_ndc__supranational_reading, carbon_intensive_industries).
narrative_ontology:constraint_victim(paris_article_4_ndc__supranational_reading, fossil_fuel_economies).
narrative_ontology:constraint_victim(paris_article_4_ndc__supranational_reading, incumbent_energy_incumbents).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CARBON-INTENSIVE INDUSTRY WORKER (SNARE) — Trapped by geographic dependence on extractive industries (coal mining, oil refining) with minimal alternative employment. The NDC ratchet systematically eliminates their sector without viable transition pathways. Zero exit options; maximum experienced extraction as economic foundation dissolves.
constraint_indexing:constraint_classification(paris_article_4_ndc__supranational_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: FOSSIL FUEL ECONOMY STATE (TANGLED ROPE) — Faces coordination gains from climate action (avoided catastrophic climate impacts, international legitimacy, technology access) but bears concentrated extraction costs (revenue loss, stranded assets, economic restructuring). The ratchet mechanism forces transition via suppression of alternatives, but genuine coordination benefit exists in the stabilized climate outcome.
constraint_indexing:constraint_classification(paris_article_4_ndc__supranational_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: GREEN TECHNOLOGY EXPORTER (ROPE) — Institutional actor (solar manufacturers, wind developers, EV makers) benefits from the NDC architecture without bearing suppressive costs. Experiences the constraint as pure coordination: global carbon markets, technology transfer mandates, and climate finance create demand for their exports. Arbitrage access through technology licensing and manufacturing relocation.
constraint_indexing:constraint_classification(paris_article_4_ndc__supranational_reading, rope,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: LEAST DEVELOPED COUNTRY COALITION (TANGLED ROPE) — Organized agents (Alliance of Small Island States, LDC Group in UNFCCC) benefit from climate finance and adaptation funding, but constrained by dependence on fossil-fuel-exporting neighbors, limited institutional capacity, and vulnerability to enforcement mechanisms that target upstream emitters. Coordination function (stabilized climate) is genuine; extraction is mediated through loss-and-damage asymmetry.
constraint_indexing:constraint_classification(paris_article_4_ndc__supranational_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: ARTICLE 6 CARBON MARKET ARCHITECTURE (SCAFFOLD) — Designed as a temporary transitional mechanism with an implicit sunset. Carbon markets create price signals that drive decarbonization, but the architecture is explicitly designed to become obsolete once carbon intensity approaches zero. Low theater (markets price signals efficiently). Sunset logic: as decarbonization accelerates, market mechanisms are gradually superseded by zero-carbon normalization.
constraint_indexing:constraint_classification(paris_article_4_ndc__supranational_reading, scaffold,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: PHYSICAL CONSTRAINT VIEW (MOUNTAIN) — From a universalist/civilizational analytical stance, the NDC architecture appears as an institutional expression of biophysical necessity: atmospheric carbon concentration is a physical variable with immutable consequences (albedo feedback, heat retention, ecosystem collapse). The 'binding' and 'ratcheting' are constraints that emerge naturally from physics, not from institutional design. This perspective risks false-summit naturalization of what is actually a contingent treaty regime.
constraint_indexing:constraint_classification(paris_article_4_ndc__supranational_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 7: INTERNATIONAL CLIMATE BUREAUCRACY (PITON) — UNFCCC institutional actors, climate finance intermediaries, and climate compliance auditors maintain elaborate reporting and verification infrastructure that is substantially performative. Theater emerges because self-reporting by states creates perverse incentives (phantom reductions, accounting manipulations, creative baseline definitions). The bureaucracy persists through institutional inertia and career dependence, not because it effectively verifies compliance. Theater ratio rises as countries optimize for reporting metrics rather than emissions reductions.
constraint_indexing:constraint_classification(paris_article_4_ndc__supranational_reading, piton,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(paris_article_4_ndc__supranational_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(paris_article_4_ndc__supranational_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(paris_article_4_ndc__supranational_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(paris_article_4_ndc__supranational_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(paris_article_4_ndc__supranational_reading, TR),
    TR >= 0.70.

:- end_tests(paris_article_4_ndc__supranational_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. The supranational reading interprets Article 4 as creating systematic extraction from carbon-intensive sectors via regulatory restriction, carbon pricing, and stranded-asset liability. Unlike a pure market mechanism, the NDC ratchet is supranational — states are bound not by economic incentives but by treaty obligation. The extractiveness is not maximum because genuine coordination benefits (climate stabilization) exist and are distributed, albeit asymmetrically. The measurement trajectory (0.35 → 0.48 → 0.58) reflects that extractiveness increases as successive NDC rounds raise stringency and enforcement mechanisms (Article 15 compliance committee, Article 6 carbon markets) mature. Suppression (0.62): Moderate-high. The ratchet mechanism creates substantial suppression of alternatives: states cannot claim phantom reductions (though they attempt to); industries cannot maintain fossil fuel operations below mandated phase-out timelines; workers in coal/oil sectors face shrinking employment with nominal just-transition support. Suppression is not total because states technically retain the option to withdraw (Article 28), though withdrawal carries reputational penalties and carbon border adjustment tariffs. Theater ratio (0.48): Moderate-low. Unlike many climate mechanisms, the NDC architecture has relatively low performative content because emissions measurement is increasingly independent (satellite data, remote sensing) and accounting rules, while flexible, are becoming more stringent. Theater is not zero because creative baseline definitions and phantom efficiency improvements still occur, but the trend is toward tightening verification standards.
 *
 * PERSPECTIVAL GAP:
 *   The supranational reading produces sharp perspectival divergence because the constraint operates asymmetrically across global North/South and industrial/post-industrial boundaries. The carbon-intensive industry worker sees only extraction (Snare) — regulatory elimination with no viable transition. The fossil-fuel state sees mixed extraction and coordination (Tangled Rope) — climate benefits are real but costs are concentrated. The green technology exporter sees pure coordination (Rope) — global decarbonization creates demand with no suppressive cost. The LDC coalition sees mixed extraction and coordination (Tangled Rope) — mediated by climate finance asymmetry. The Article 6 market architect sees a temporary transition device (Scaffold) — designed to be obsoleted by zero-carbon normalization. The UNFCCC bureaucracy sees its own degraded process (Piton) — performative compliance theater that maintains institutional relevance through complexity. The universalist analytical observer risks seeing an immutable natural law (Mountain) — the binding force of atmospheric physics — but this naturalizes what is actually a contingent institutional regime. The perspectival gaps reveal that the supranational reading, while internally consistent, produces genuine winners and losers that are not compensated by justice or transition mechanisms.
 *
 * DIRECTIONALITY LOGIC:
 *   Under the supranational reading, directionality (d) is computed from the agent's structural position relative to the extraction flow. Carbon-intensive workers and fossil-fuel economies are victims with high d (they bear extraction costs). Green technology exporters are beneficiaries with low d (they capture benefits). States are intermediate — some as victims (fossil-fuel economies), some as neutral/beneficiary (post-industrial economies). The derivation chain runs: victim + trapped → d=0.95 → f(d)=1.42 (maximum experienced extraction); beneficiary + arbitrage → d=0.05 → f(d)=-0.12 (institutional subsidy effect); victim + constrained → d=0.55 → f(d)=0.75 (moderate extraction). The scope modifier σ(S) amplifies extractiveness at global scale (σ=1.2), reflecting that supranational commitments create enforcement machinery across jurisdictions. At national or regional scope, extractiveness would be dampened (σ=1.0 or σ=0.9) because exit options expand when the constraint is localized. The supranational reading's core claim is that Article 4 binding commits states to a globally-coordinated extraction system that scales extraction asymmetrically across space.
 *
 * MANDATROPHY ANALYSIS:
 *   The supranational reading's mandatrophy is resolved through the decomposition into seven structurally distinct perspectives that together constitute the constraint architecture. No single perspective captures the full structure — the mountain view naturalizes institutional design, the snare view captures only the victim experience, the rope view captures only the beneficiary experience, the tangled rope views capture intermediate agents. The constraint is Tangled Rope at the analytical/civilizational level because it simultaneously coordinates (climate stabilization) and extracts (from carbon-intensive actors) with active enforcement (Article 15 compliance, Article 6 carbon markets). The mandatrophy dissolves when each perspective is recognized as a genuine structural position rather than competing claims about 'the' type. The supranational reading's analytical claim is that the Paris NDC architecture IS a global governance structure that extracts from some and benefits others via binding commitments and ratcheting mechanisms — not a purely voluntary coordination mechanism (sovereigntist reading) and not a differentiated responsibility system (equity reading).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    binding_vs_aspirational_enforcement,
    'Are NDC commitments genuinely binding with material enforcement teeth, or aspirational targets with performative compliance mechanisms?',
    'Empirical tracking of enforcement outcomes: number of states facing sanctions/tariffs for non-compliance; correlation between stated NDC targets and actual emissions trajectories; analysis of UNFCCC enforcement mechanisms (Article 15 compliance committee lacks coercive power; Article 6 carbon markets are voluntary).',
    'If genuinely binding with enforcement: constraint is high-extraction Snare from fossil-fuel-dependent states'' perspective. If aspirational/performative: constraint downgrades to low-suppression Tangled Rope or Scaffold from same perspective.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(binding_vs_aspirational_enforcement, empirical, 'Whether NDC commitments have material enforcement mechanisms or are performative aspirations').

omega_variable(
    ratchet_mechanism_credibility,
    'Do successive NDCs represent genuinely increasing stringency, or do states game the ratchet by lowering baseline definitions and claiming phantom reductions?',
    'Comparative analysis of NDC 1 vs NDC 2 vs NDC 3 baselines; detection of baseline shifting (e.g., changing measurement methodologies to reduce apparent emissions); real-time emissions data vs self-reported reductions.',
    'If credible ratchet: the constraint operates with real teeth; extracted agents face persistent escalating costs. If gamed: the ratchet becomes Piton (performative intensity mask); theater_ratio rises substantially.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ratchet_mechanism_credibility, empirical, 'Whether successive NDC ratchets represent genuine emissions reductions or accounting manipulation').

omega_variable(
    just_transition_realism,
    'Are just-transition mechanisms (loss-and-damage finance, technology transfer, adaptation support) sufficient to prevent the snare classification from materializing for vulnerable workers and economies?',
    'Quantitative comparison: loss-and-damage fund commitments vs estimated costs of economic restructuring in fossil-fuel-dependent regions; tracking of technology transfer implementation rates; employment data in transition economies.',
    'If sufficient: the constraint remains Tangled Rope (coordination + asymmetric cost). If insufficient: the constraint solidifies as Snare for powerless agents (no viable exit despite nominal transfer mechanisms).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(just_transition_realism, empirical, 'Whether just-transition finance is proportional to restructuring costs in vulnerable economies').

omega_variable(
    supranational_authority_legitimacy,
    'On what basis do supranational climate commitments bind sovereign states? Is binding force grounded in treaty law (states agreed and can theoretically withdraw), reputational pressure (states fear isolation but retain formal exit), or coercive institutional machinery (actual enforcement against defectors)?',
    'Legal analysis of Paris Agreement termination clause (Article 28) and its use; assessment of reputational penalties and their magnitude; review of enforcement mechanisms in Article 15 and their deployment.',
    'If grounded in treaty law + state consent: binding force is conditional and consensual. If grounded in reputational pressure: binding force is psychological but not structural. If grounded in institutional coercion: binding force is supranational (EC carbon border adjustment, for example). This determines whether the constraint''s authority grounding is ''distributed'' (states consent) or ''extraction'' (institutional machinery).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(supranational_authority_legitimacy, conceptual, 'Basis of supranational authority to bind sovereign states to NDC targets').

omega_variable(
    equity_reading_compatibility,
    'Can the supranational reading (focus on binding ratcheting mechanism) coexist with the equity reading (focus on historical responsibility and differentiated obligations) in a single Paris framework, or do they represent incompatible framings?',
    'Textual analysis of Paris Agreement Article 4 and its interplay with Article 13 (transparency framework) and decisions on Common but Differentiated Responsibilities. Assessment of whether CBDR language is sufficient to reconcile binding ratchet with equity obligations.',
    'If coexistent: framework is deliberately ambiguous, allowing multiple readings. If incompatible: the framework embeds an unresolved tension that manifests as implementation failure when one reading is operationalized.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(equity_reading_compatibility, conceptual, 'Whether supranational and equity readings of the Paris NDC architecture are logically compatible').

omega_variable(
    carbon_accounting_baseline_gaming,
    'Do the flexible accounting rules in Article 13 (transparency framework) enable states to selectively game baseline definitions (e.g., frontier mining areas, industrial process efficiency claims) such that reported NDC compliance diverges substantially from real atmospheric impact?',
    'Detailed audit of NDC baselines against actual historical emissions data; detection of methodological revisions between submission cycles; comparison of self-reported reductions to independent emissions inventories (satellite data, third-party audits).',
    'If significant gaming is detected: the suppression metric should be revised downward (enforcement is porous) and theater_ratio should rise (performative accounting dominates). The constraint transitions from high-suppression to high-theater.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(carbon_accounting_baseline_gaming, empirical, 'Whether Article 13 accounting flexibility enables baseline gaming that decouples reporting from real emissions').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(paris_article_4_ndc__supranational_reading, 2015, 2030).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ndc_supra_theater_2015, paris_article_4_ndc__supranational_reading, theater_ratio, 2015, 0.52).
narrative_ontology:measurement(ndc_supra_theater_2020, paris_article_4_ndc__supranational_reading, theater_ratio, 2020, 0.5).
narrative_ontology:measurement(ndc_supra_theater_2025, paris_article_4_ndc__supranational_reading, theater_ratio, 2025, 0.48).

% Extraction over time
narrative_ontology:measurement(ndc_supra_extractiveness_2015, paris_article_4_ndc__supranational_reading, base_extractiveness, 2015, 0.35).
narrative_ontology:measurement(ndc_supra_extractiveness_2020, paris_article_4_ndc__supranational_reading, base_extractiveness, 2020, 0.48).
narrative_ontology:measurement(ndc_supra_extractiveness_2025, paris_article_4_ndc__supranational_reading, base_extractiveness, 2025, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(ndc_supra_suppression_2015, paris_article_4_ndc__supranational_reading, suppression_requirement, 2015, 0.45).
narrative_ontology:measurement(ndc_supra_suppression_2020, paris_article_4_ndc__supranational_reading, suppression_requirement, 2020, 0.55).
narrative_ontology:measurement(ndc_supra_suppression_2025, paris_article_4_ndc__supranational_reading, suppression_requirement, 2025, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(paris_article_4_ndc__supranational_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(paris_article_4_ndc__supranational_reading, paris_article_4_ndc__sovereigntist_reading).
narrative_ontology:affects_constraint(paris_article_4_ndc__supranational_reading, paris_article_4_ndc__equity_reading).
narrative_ontology:affects_constraint(paris_article_4_ndc__supranational_reading, article_6_carbon_market_architecture).
narrative_ontology:affects_constraint(paris_article_4_ndc__supranational_reading, stranded_asset_liability_regime).

% DUAL FORMULATION NOTE:
% The paris_article_4_ndc kernel decomposes into three structurally distinct constraint stories corresponding to three competing readings. The supranational reading emphasizes binding enforcement and ratcheting mechanism (ε=0.58, Tangled Rope). The sovereigntist reading emphasizes state flexibility and voluntary compliance (ε lower, tends toward Rope or Scaffold). The equity reading emphasizes historical responsibility and differentiated obligations (ε varies by North/South perspective). Each reading is a separate constraint story with its own perspectives, beneficiaries/victims, and classification. They are linked via network.affects_constraints to show the kernel structure. The supranational reading influences both siblings by establishing which accountability mechanisms are operationalized.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(paris_article_4_ndc__supranational_reading, moderate, 0.62).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
