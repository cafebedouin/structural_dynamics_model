% ============================================================================
% CONSTRAINT STORY: nafta_jurisdictional_boundary__embedded_liberalism_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_nafta_jurisdictional_boundary__embedded_liberalism_reading, []).

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
 *   constraint_id: nafta_jurisdictional_boundary__embedded_liberalism_reading
 *   human_readable: NAFTA Jurisdictional Boundary: Embedded Liberalism Reading
 *   domain: international_trade_law/political_economy/regulatory_federalism
 *
 * SUMMARY:
 *   The North American Free Trade Agreement (NAFTA), in force since 1994,
 *   creates an integrated trade and investment framework spanning the US,
 *   Canada, and Mexico. The embedded liberalism reading interprets the
 *   agreement as establishing partial jurisdictional overlap: trade rules
 *   govern market access and capital movement, but domestic governments
 *   retain defensive authority to regulate labor, environmental, and health
 *   standards provided they are applied non-discriminatorily and justified as
 *   'legitimate objectives.' This reading contrasts with two competing
 *   interpretations: a capital-supremacy reading (investor rights override
 *   domestic regulation; harmonization is mandatory) and a
 *   sovereignty-primacy reading (domestic law is supreme; the trade agreement
 *   is merely a coordination mechanism with no binding override effect). The
 *   embedded liberalism reading attempts to hold both: markets are opened,
 *   capital is protected, but regulatory space is preserved. In practice, the
 *   constraint operates as a tangled rope: genuine coordination function
 *   (predictable investment, supply-chain efficiency) coupled with asymmetric
 *   extraction (litigation costs and regulatory uncertainty concentrated on
 *   domestic labor/environmental agencies, capital and export sectors
 *   benefit). This story authorizes ε from the embedded liberalism reading's
 *   own perspective (the standing arrangement under contest), not the
 *   capital-supremacy or sovereignty-primacy alternatives.
 *
 * KEY AGENTS:
 *   - Multinational capital investors (institutional power, global scope, arbitrage exit) — beneficiaries of market access and investor-state protection
 *   - Export-oriented sectors (powerful, generational horizon, arbitrage exit) — benefit from integrated supply chains and capital mobility threat
 *   - Domestic labor standards regulators (institutional power, national scope, constrained exit) — bear the burden of uncertainty and litigation costs defending 'legitimate objectives'
 *   - Environmental protection agencies (institutional power, national scope, constrained exit) — excluded from dispute panels, face regulatory pressure
 *   - Trade dispute panels (institutional power, global scope, analytical exit) — agenda-setters interpreting 'legitimate objectives' narrowly in practice
 *   - Worker advocacy groups and environmental NGOs (organized power, excluded from formal dispute process) — articulate the gap between embedded liberalism rhetoric and capital-favorable practice
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(nafta_jurisdictional_boundary__embedded_liberalism_reading, 0.58).
domain_priors:suppression_score(nafta_jurisdictional_boundary__embedded_liberalism_reading, 0.62).
domain_priors:theater_ratio(nafta_jurisdictional_boundary__embedded_liberalism_reading, 0.44).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(nafta_jurisdictional_boundary__embedded_liberalism_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(nafta_jurisdictional_boundary__embedded_liberalism_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(nafta_jurisdictional_boundary__embedded_liberalism_reading, theater_ratio, 0.44).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(nafta_jurisdictional_boundary__embedded_liberalism_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(nafta_jurisdictional_boundary__embedded_liberalism_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(nafta_jurisdictional_boundary__embedded_liberalism_reading, tangled_rope).
narrative_ontology:human_readable(nafta_jurisdictional_boundary__embedded_liberalism_reading, "NAFTA Jurisdictional Boundary: Embedded Liberalism Reading").
narrative_ontology:topic_domain(nafta_jurisdictional_boundary__embedded_liberalism_reading, "international_trade_law/political_economy/regulatory_federalism").

domain_priors:requires_active_enforcement(nafta_jurisdictional_boundary__embedded_liberalism_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(nafta_jurisdictional_boundary__embedded_liberalism_reading, '996d3b1c-ca40-4650-992e-7aac09e63ceb').
narrative_ontology:cs_kernel_codification('996d3b1c-ca40-4650-992e-7aac09e63ceb', formalized).
narrative_ontology:cs_authority_grounding('996d3b1c-ca40-4650-992e-7aac09e63ceb', lineage).
narrative_ontology:cs_interpretation_layer_present('996d3b1c-ca40-4650-992e-7aac09e63ceb').
narrative_ontology:cs_reading_relation('996d3b1c-ca40-4650-992e-7aac09e63ceb', nafta_jurisdictional_boundary__capital_supremacy_reading, coexists_with).
narrative_ontology:cs_reading_relation('996d3b1c-ca40-4650-992e-7aac09e63ceb', nafta_jurisdictional_boundary__sovereignty_primacy_reading, coexists_with).
narrative_ontology:cs_axiom('996d3b1c-ca40-4650-992e-7aac09e63ceb', foundational, regulatory_space_preservation_within_legitimacy).
narrative_ontology:cs_axiom_status(regulatory_space_preservation_within_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('996d3b1c-ca40-4650-992e-7aac09e63ceb', regulatory_space_preservation_within_legitimacy, deontological).
narrative_ontology:cs_axiom('996d3b1c-ca40-4650-992e-7aac09e63ceb', secondary, market_access_compatible_with_standard_maintenance).
narrative_ontology:cs_axiom_status(market_access_compatible_with_standard_maintenance, holdable).
narrative_ontology:cs_axiom_grounding('996d3b1c-ca40-4650-992e-7aac09e63ceb', market_access_compatible_with_standard_maintenance, empirically_contingent).
narrative_ontology:cs_reference_frame('996d3b1c-ca40-4650-992e-7aac09e63ceb', partial_jurisdictional_overlap).
narrative_ontology:cs_drift_state('996d3b1c-ca40-4650-992e-7aac09e63ceb', contemporary_post_usmca, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('996d3b1c-ca40-4650-992e-7aac09e63ceb', '2026-06-12T14:32:00Z').
narrative_ontology:cs_kernel_id(nafta_jurisdictional_boundary__embedded_liberalism_reading, nafta_jurisdictional_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(nafta_jurisdictional_boundary__embedded_liberalism_reading, multinational_capital_investors).
narrative_ontology:constraint_beneficiary(nafta_jurisdictional_boundary__embedded_liberalism_reading, export_oriented_sectors).
narrative_ontology:constraint_victim(nafta_jurisdictional_boundary__embedded_liberalism_reading, domestic_labor_standards_regulators).
narrative_ontology:constraint_victim(nafta_jurisdictional_boundary__embedded_liberalism_reading, environmental_protection_agencies).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(nafta_jurisdictional_boundary__embedded_liberalism_reading, capital_exporting_governments).
narrative_ontology:constraint_victim(nafta_jurisdictional_boundary__embedded_liberalism_reading, capital_importing_governments).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Gain access to three large markets (US, Mexico, Canada) under unified rules with predictable dispute resolution. The constraint's 'legitimate objectives' carve-out is read narrowly by investor-state panels, and they can sue governments for regulatory changes deemed to violate market commitments. Their capital mobility means they can shift production to jurisdictions with weaker labor/environmental enforcement, creating a regulatory arbitrage advantage.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__embedded_liberalism_reading, multinational_capital_investors, beneficiary,
    institutional, generational, arbitrage, global).

% Benefit from market access and competitive advantage in integrated supply chains. They depend on keeping labor and environmental costs low and resist upward pressure on standards. They influence trade negotiation agendas and can threaten relocation to weaker-regulation jurisdictions, pressuring governments to interpret 'legitimate objectives' narrowly.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__embedded_liberalism_reading, export_oriented_sectors, beneficiary,
    powerful, generational, arbitrage, global).

% Regulate workplace safety, minimum wages, and union rights within sovereign territory. The embedded liberalism reading grants them defensive space to maintain standards, but only when justified as 'legitimate objectives' and applied non-discriminatorily. In practice, they face litigation costs and uncertainty: investor-state panels question whether labor regulations are pretexts for protectionism. They have constrained exit: they cannot unilaterally withdraw from the agreement without triggering trade retaliation.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__embedded_liberalism_reading, domestic_labor_standards_regulators, payer,
    institutional, generational, constrained, national).

% Set and enforce environmental standards (clean air, water quality, emissions). Under the embedded liberalism reading, they retain defensive authority for 'legitimate objectives,' but face the same litigation risk and burden-of-proof asymmetry as labor regulators. Environmental rules are often challenged as disguised trade barriers, even when applied equally to domestic and foreign producers. They are excluded from investor-state dispute panels and have no standing to defend their standards directly.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__embedded_liberalism_reading, environmental_protection_agencies, payer,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(nafta_jurisdictional_boundary__embedded_liberalism_reading, environmental_protection_agencies, excluded).

% Represent workers across the three countries and advocate for stronger labor standards and cross-border enforcement. They are not parties to the dispute resolution process and are excluded from trade tribunal proceedings. They argue that the 'legitimate objectives' boundary is honored in the breach: capital's arbitrage advantage is real, regulatory convergence is downward, and the constraint functions as a floor on labor cost-cutting rather than a framework for compatible standards.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__embedded_liberalism_reading, worker_advocacy_groups, excluded,
    organized, biographical, constrained, regional).

% Campaign for environmental protection and often intervene in domestic regulatory processes. They are excluded from trade dispute resolution and argue the embedded liberalism reading is honored more in rhetoric than enforcement: companies threaten relocation to weaker jurisdictions, environmental agencies back down preemptively, and the regulatory outcome is convergence-to-the-bottom despite the 'legitimate objectives' frame.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__embedded_liberalism_reading, environmental_ngos, excluded,
    organized, biographical, constrained, regional).

% Interpret the trade agreement text and adjudicate disputes between investors and governments. Under embedded liberalism, they are supposed to defer to domestic regulatory agencies on 'legitimate objectives' claims, but in practice they impose substantial evidentiary burdens and review outcomes de novo. Their interpretation transforms the theoretical constraint's structure: a 'yes, you may regulate' reading becomes a 'yes, if you survive our scrutiny' reading.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__embedded_liberalism_reading, trade_dispute_panels, agenda_setter,
    institutional, generational, analytical, global).

% The US and Canada, whose investors dominate cross-border capital flows. They co-write the trade agreement, staff the dispute panels, and have political incentive to defend their investors' interests. They also regulate labor and environment domestically, but their structural position (large capital exporters) aligns them more with investor interests than with weak-regulation jurisdictions' regulatory space needs.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__embedded_liberalism_reading, capital_exporting_governments, beneficiary,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(nafta_jurisdictional_boundary__embedded_liberalism_reading, capital_exporting_governments, agenda_setter).

% Mexico primarily: receives foreign direct investment but sees it conditional on weak labor/environmental enforcement. Under embedded liberalism, they theoretically retain regulatory space, but investor threats of relocation and litigation costs create structural pressure to keep standards low. They cannot credibly exit the agreement without losing investment flows and trade access, and cannot unilaterally impose higher standards without facing litigation or retaliation.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__embedded_liberalism_reading, capital_importing_governments, payer,
    institutional, generational, constrained, national).

% Small and medium enterprises serving domestic markets. They are not export-oriented beneficiaries, so they gain no direct benefit from market access. They face new competition from multinational imports and are excluded from the investment-arbitration framework (it applies only to investors from the three countries, protecting their capital but not their enterprises). They have no standing in trade disputes.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__embedded_liberalism_reading, domestic_industries, excluded,
    moderate, biographical, trapped, national).

% Advocate for product safety and consumer protection standards. They observe trade disputes affecting food safety, pharmaceutical approval, and labeling requirements, but are not formal parties. They argue that the embedded liberalism reading's 'legitimate objectives' language omits consumer protection and operates primarily to resolve investor-government conflicts, not consumer-citizen conflicts.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__embedded_liberalism_reading, consumer_groups, observer,
    organized, biographical, analytical, regional).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(nafta_jurisdictional_boundary__embedded_liberalism_reading, multinational_capital_investors).
narrative_ontology:fixing_cost_class(nafta_jurisdictional_boundary__embedded_liberalism_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates integrated North American markets through unified trade rules and dispute resolution, enabling predictable cross-border investment, supply chains, and commerce. Solves the collective-action problem of tariff escalation and ensures investors are not expropriated arbitrarily.
% TRANSFER_FUNCTION: Moves regulatory decision-making authority from weak-enforcement jurisdictions (Mexico) toward capital exporters' preferred standards; moves litigation costs and defensive burden onto labor and environmental agencies; redistributes profit opportunities from labor-intensive/high-environmental-cost production toward capital-intensive/integrated sectors.
% ABSENT_VOICES: Workers' organizations and environmental groups are structurally excluded from investor-state dispute panels. Domestic small/medium enterprises have no investment protection (only multinational capital does). Consumer protection advocates have no standing in trade disputes affecting food/drug safety. Their absence from the dispute mechanism is constitutive: the constraint is read as an investor-state relationship, not a stakeholder-democratic one.
% DISAPPEARANCE_RATIONALE: If the constraint vanished, North American supply chains would fragment; companies would navigate three separate regulatory regimes; cross-border investment would decline sharply; labor and environmental standards would cease converging toward capital-friendly equilibria (though the direction of change is contested: capital-exporting parties argue standards would collapse; labor/environmental advocates argue they would rise). Trade volumes would drop, but regulatory autonomy would return to domestic governments.
% FOUNDING_PROBLEM: Tariff barriers, protectionist quota systems, and regulatory uncertainty fragmented North American markets in the 1980s. Capital needed predictability for investment; exporters needed access; governments needed a multilateral rule framework to commit to openness and prevent destructive tariff wars.
% FOUNDING_PROBLEM_CORROBORATION: Capital exporters and trade officials attest the founding problem is still live: regulatory uncertainty and tariff risk persist, requiring ongoing dispute resolution. Labor and environmental advocates attest the founding problem was solved by the mid-1990s; what persists is the constraint's use to suppress regulatory experimentation beyond the original scope. Empirical economic analysis (NBER studies, congressional testimony from outside-beneficiary economists) supports the claim that tariff/quota uncertainty declined substantially in the first decade, but trade volumes and investment flows would not have collapsed absent the constraint—the constraint's modern function is regulatory harmonization and investor protection, not tariff resolution.
narrative_ontology:disappearance_verdict(nafta_jurisdictional_boundary__embedded_liberalism_reading, world_rearranges).
narrative_ontology:founding_problem_status(nafta_jurisdictional_boundary__embedded_liberalism_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(nafta_jurisdictional_boundary__embedded_liberalism_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(nafta_jurisdictional_boundary__embedded_liberalism_reading, 'none', 1).
narrative_ontology:epsilon_provenance(nafta_jurisdictional_boundary__embedded_liberalism_reading, 0.58, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(nafta_jurisdictional_boundary__embedded_liberalism_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(nafta_jurisdictional_boundary__embedded_liberalism_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(nafta_jurisdictional_boundary__embedded_liberalism_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness measures 0.58 at interval end, showing substantial but not maximal extraction: the constraint delivers genuine coordination benefits (market access, investment predictability) alongside asymmetric burden-imposition. The founding coordination problem (tariff escalation, investment uncertainty) was real and solved; the constraint now persists partly as that solved problem and partly as a mechanism for regulatory suppression. The measurement series shows slow accumulation: extractiveness rises from 0.42 (1994) to 0.58 (2026), mirroring the shift in dispute volume and investor-state panel jurisprudence—as panels interpreted 'legitimate objectives' more restrictively, the constraint's functional burden on labor/environmental regulators increased. Theater ratio rises from 0.28 to 0.44, indicating growing performativity: the 'legitimate objectives' carve-out is rhetorically maintained but increasingly honored in the breach as arbitrage pressure mounts. Suppression requirement climbs from 0.48 to 0.62, capturing the growing need for active enforcement to maintain the constraint against resistance from labor/environmental constituencies. Accessibility_collapse of 0.48 reflects that domestic alternatives (exit the agreement, strengthen labor/environmental standards unilaterally) formally exist but carry severe costs (trade retaliation, investment flight). Resistance of 0.71 is high, indicating sustained organized opposition from excluded voices. The measurements follow a single shared time grid (t=1994, 2000, 2006, 2012, 2018, 2026) for consistency.
 *
 * PERSPECTIVAL GAP:
 *   The beneficiary seats (multinational investors, export sectors, capital-exporting governments) read the constraint as successful embedded liberalism: markets are open, investment is protected, and regulatory space is preserved where legitimate. They emphasize the language of the agreement ('nothing in this chapter shall prevent a Party from adopting or enforcing measures... necessary to protect... the environment or labor standards') and cite low litigation win rates for investors challenging environmental/labor laws as evidence that the carve-out is working. The payer seats (domestic regulators) read the same constraint as regulatory capture through uncertainty: the 'legitimate objectives' test is nebulous, burden of proof is asymmetric (governments must prove non-discriminatory intent), litigation costs are prohibitive, and panel decisions are inconsistent. They observe preemptive regulatory rollback (Mexico weakening labor enforcement in anticipation of investor challenges) and note that investors rarely need to litigate because the threat of litigation suffices. The agenda-setter seat (trade panels) operates between the two readings: they declare fidelity to embedded liberalism while imposing demanding evidentiary thresholds, effectively narrowing the space they are supposed to preserve. The engine computes this divergence from power/exit/directionality: beneficiaries hold institutional power and arbitrage exit (low d, near 0.1); payers hold institutional power but constrained exit (high d, near 0.8); panels hold institutional power and analytical exit (d near 0.5, but structurally they influence the constraint more than they are constrained by it).
 *
 * DIRECTIONALITY LOGIC:
 *   Multinational capital investors: d≈0.15 (beneficiary position, arbitrage exit, can relocate production to favorable jurisdictions). Export-oriented sectors: d≈0.20 (powerful, arbitrage exit, global supply chain flexibility). Domestic labor regulators: d≈0.78 (payer position, constrained exit by treaty obligation, high vulnerability to litigation and capital flight threats). Environmental agencies: d≈0.80 (similar to labor regulators but worse—they are excluded from formal dispute standing and face additional burden of proving non-discrimination). Trade panels: d≈0.50 in structural terms but operate with amplified influence via their interpretive authority (they reset the constraint's effective operation each decision; a better frame would note that d describes a seat's extraction exposure, while institutional power describes agenda-setting capacity—panels hold agenda-setting capacity disproportionate to their extraction exposure, which is why they sit at the constraint's fulcrum). Capital-exporting governments: d≈0.25 (institutional power, arbitrage exit through regulatory arbitrage policy, beneficiary through investor representation, but also carry a reputational cost if labor/environmental standards are visibly suppressed).
 *
 * MANDATROPHY ANALYSIS:
 *   The embedded liberalism reading prevents mandatrophy conflation: the genuine founding problem (tariff escalation, investment uncertainty in fragmented North American markets) is distinct from the modern persistent function (regulatory harmonization downward, investor protection against regulatory drift). The measurement series documents the shift: early extractiveness (0.42 in 1994) reflected coordination-dominated operation; rising extractiveness (0.58 by 2026) reflects growing functional divergence. A mandatrophy reading would require the founding problem (tariff barriers) to be dead AND the constraint to persist. Here, the founding problem is contested (capital exporters say it is live; others say it is solved), and the constraint's modern function (regulatory suppression) is distinct and live. This is tangled_rope, not a piton with only mandatrophy to explain its persistence: the coordination function is real, but so is the asymmetric extraction. Theater ratio is moderate (0.44), not high enough to suggest pure inertia—the constraint is actively defended and reinterpreted by dispute panels, not merely inherited. Mandatrophy is not applicable here; the constraint persists because both functions (coordination and extraction) are live and defended.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    legitimate_objectives_boundary_definition,
    'What defines the boundary of ''legitimate objectives'' for labor and environmental regulation? Is it: (a) narrow and enumerated (capital-supremacy framing, favorable to investors); (b) broad and delegated to domestic agencies with minimal scrutiny (sovereignty-primacy framing); or (c) medium and subject to international panel review under a deferential standard (embedded liberalism framing)?',
    'Analysis of investor-state panel decisions over time: do they systematically defer to labor/environmental agencies or systematically override them? Do they apply a rational-basis test or strict scrutiny? What proportion of cases involve de novo review vs. deference? A corpus of 100+ cases provides sufficient evidence to classify the operative boundary.',
    'Narrow boundary shifts the constraint toward capital-supremacy and reclassifies extractiveness upward (ε→0.75+), theater ratio upward (→0.60+), and type toward snare. Broad boundary shifts toward sovereignty-primacy and reclassifies extractiveness downward (ε→0.40), theater ratio downward (→0.25), and type toward rope. The difference is the most material determination of embedded liberalism''s structural truth or failure.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(legitimate_objectives_boundary_definition, empirical, 'The operative scope of ''legitimate objectives'' as revealed by panel jurisprudence.').

omega_variable(
    preemptive_regulatory_rollback_mechanism,
    'To what extent does the constraint operate through litigation threat rather than litigation outcome? Do labor/environmental agencies preemptively weaken regulations to avoid investor challenges, and if so, how much of the measured suppression reflects threat-based behavior rather than actual panel override?',
    'Interviews with Mexican and Canadian regulatory officials; comparative analysis of regulatory proposals before/after high-profile litigation (e.g., the Ethyl Corp. case, the Metalclad case); testimony from capital exporting governments regarding investment promotion; survey of corporate compliance/litigation strategies.',
    'If preemptive rollback is high (>60% of suppression), the constraint''s effective extraction is higher than litigation outcomes alone suggest—threat creates behavioral suppression that is not visible in dispute statistics. This would support higher theater_ratio (→0.55+) and argue for reclassification toward snare if the threat is non-negotiable (investors will flee if standards rise). If preemptive rollback is low, the constraint''s extraction is more transparent and negotiable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(preemptive_regulatory_rollback_mechanism, empirical, 'Whether regulatory suppression is driven by litigation outcome or litigation threat.').

omega_variable(
    alternative_regulatory_regime_counterfactual,
    'If the constraint did not exist (or was revised to grant regulatory primacy to domestic governments), would labor and environmental standards in Mexico and Canada rise substantially, stay roughly constant, or reflect a complex pattern of sectoral divergence (some standards up, some down, some unstable)?',
    'Comparison to non-NAFTA jurisdictions with similar development levels; econometric analysis of pre-NAFTA and post-NAFTA regulatory trajectories; counterfactual modeling with dynamic comparative advantage; analysis of USMCA (2020) renegotiation which included stronger labor/environmental language—did standards rise where renegotiation created stronger ''legitimate objectives'' language?',
    'If standards would rise substantially (counterfactual standards 0.3 points higher across labor and environment), the constraint is operating primarily as suppression and extractiveness is correctly measured at 0.58. If standards would be roughly similar or show complex divergence, the constraint''s extraction is lower and more subtle (affecting price, not quantity), and extractiveness might lower to 0.48–0.52. This affects the mandatrophy verdict: a constraint whose removal would dramatically change outcomes is functionally active; one whose removal would change little is closer to theater/piton.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(alternative_regulatory_regime_counterfactual, conceptual, 'The magnitude of regulatory change that would occur absent the constraint.').

omega_variable(
    kernel_reading_structural_difference,
    'Is the observed gap between embedded liberalism''s stated scope (''legitimate objectives'' preserved) and its operative scope (panels narrow the boundary through jurisprudence) a flaw in the embedded liberalism reading itself, or a flaw in the panels'' interpretation of it?',
    'Textual analysis of NAFTA Chapter 11''s language and negotiation history; comparison to how other international agreements (EU law, WTO DSM, human rights treaties) interpret similar ''carve-out'' language; expert testimony from trade law scholars and dispute resolution practitioners on whether the panels applied the text faithfully or diverged from the negotiators'' intent.',
    'If the gap is a flaw in the reading (embedded liberalism is incoherent as stated), the reading is reclassifiable as a failed attempt at coordination and should be reclassified toward snare. If the gap is a flaw in interpretation (the reading is sound but the panels corrupt it), the reading remains accurately classified as tangled rope with theater ratio higher (panels are performing fidelity while enabling extraction). This affects the committer-axis verdict: did the embedded liberalism negotiators genuinely intend partial jurisdictional overlap with regulatory space preserved, or was the ''legitimate objectives'' language always a fig leaf?',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_structural_difference, conceptual, 'Whether embedded liberalism''s stated/operative gap reflects an incoherent reading or unfaithful interpretation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(nafta_jurisdictional_boundary__embedded_liberalism_reading, 1994, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(naft_tr_t1994, nafta_jurisdictional_boundary__embedded_liberalism_reading, theater_ratio, 1994, 0.28).
narrative_ontology:measurement(naft_tr_t2000, nafta_jurisdictional_boundary__embedded_liberalism_reading, theater_ratio, 2000, 0.32).
narrative_ontology:measurement(naft_tr_t2006, nafta_jurisdictional_boundary__embedded_liberalism_reading, theater_ratio, 2006, 0.37).
narrative_ontology:measurement(naft_tr_t2012, nafta_jurisdictional_boundary__embedded_liberalism_reading, theater_ratio, 2012, 0.4).
narrative_ontology:measurement(naft_tr_t2018, nafta_jurisdictional_boundary__embedded_liberalism_reading, theater_ratio, 2018, 0.42).
narrative_ontology:measurement(naft_tr_t2026, nafta_jurisdictional_boundary__embedded_liberalism_reading, theater_ratio, 2026, 0.44).

% Extraction over time
narrative_ontology:measurement(naft_be_t1994, nafta_jurisdictional_boundary__embedded_liberalism_reading, base_extractiveness, 1994, 0.42).
narrative_ontology:measurement(naft_be_t2000, nafta_jurisdictional_boundary__embedded_liberalism_reading, base_extractiveness, 2000, 0.48).
narrative_ontology:measurement(naft_be_t2006, nafta_jurisdictional_boundary__embedded_liberalism_reading, base_extractiveness, 2006, 0.52).
narrative_ontology:measurement(naft_be_t2012, nafta_jurisdictional_boundary__embedded_liberalism_reading, base_extractiveness, 2012, 0.55).
narrative_ontology:measurement(naft_be_t2018, nafta_jurisdictional_boundary__embedded_liberalism_reading, base_extractiveness, 2018, 0.57).
narrative_ontology:measurement(naft_be_t2026, nafta_jurisdictional_boundary__embedded_liberalism_reading, base_extractiveness, 2026, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(naft_su_t1994, nafta_jurisdictional_boundary__embedded_liberalism_reading, suppression_requirement, 1994, 0.48).
narrative_ontology:measurement(naft_su_t2000, nafta_jurisdictional_boundary__embedded_liberalism_reading, suppression_requirement, 2000, 0.53).
narrative_ontology:measurement(naft_su_t2006, nafta_jurisdictional_boundary__embedded_liberalism_reading, suppression_requirement, 2006, 0.57).
narrative_ontology:measurement(naft_su_t2012, nafta_jurisdictional_boundary__embedded_liberalism_reading, suppression_requirement, 2012, 0.6).
narrative_ontology:measurement(naft_su_t2018, nafta_jurisdictional_boundary__embedded_liberalism_reading, suppression_requirement, 2018, 0.61).
narrative_ontology:measurement(naft_su_t2026, nafta_jurisdictional_boundary__embedded_liberalism_reading, suppression_requirement, 2026, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(nafta_jurisdictional_boundary__embedded_liberalism_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(nafta_jurisdictional_boundary__embedded_liberalism_reading, 0.18).
narrative_ontology:affects_constraint(nafta_jurisdictional_boundary__embedded_liberalism_reading, nafta_jurisdictional_boundary__capital_supremacy_reading).
narrative_ontology:affects_constraint(nafta_jurisdictional_boundary__embedded_liberalism_reading, nafta_jurisdictional_boundary__sovereignty_primacy_reading).
narrative_ontology:affects_constraint(nafta_jurisdictional_boundary__embedded_liberalism_reading, wto_most_favored_nation_principle).
narrative_ontology:affects_constraint(nafta_jurisdictional_boundary__embedded_liberalism_reading, investor_state_dispute_settlement_mechanism).

% DUAL FORMULATION NOTE:
% This constraint is part of a three-reading kernel family for the NAFTA jurisdictional boundary dispute. The embedded liberalism reading describes the standing arrangement as preserving regulatory space within 'legitimate objectives' boundaries. The capital_supremacy_reading describes the same arrangement as investor-supreme with regulatory harmonization mandatory. The sovereignty_primacy_reading describes it as domestic-law-supreme coordination. All three share the same referent (the NAFTA Chapter 11 investment rules as applied in practice) but differ in ε assignment, structural analysis, and classification. They are linked because each reading cites the others as failures to capture the arrangement's true structure, and jurisprudential evolution in one reading affects the viability of the others. The embedded liberalism reading is positioned as the middle ground but faces pressure from both extremes: capital-supremacy actors are strengthening panel deference to investor claims; sovereignty-primacy actors are proposing treaty renegotiation to restore regulatory primacy. Decomposition follows OQ-29 (ε-invariance): each reading generates a different ε value from its own structural perspective, and they cannot be unified within a single constraint story without losing analytical precision.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(nafta_jurisdictional_boundary__embedded_liberalism_reading, institutional, 0.5).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
