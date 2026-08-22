% ============================================================================
% CONSTRAINT STORY: nafta_jurisdictional_boundary__capital_supremacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_nafta_jurisdictional_boundary__capital_supremacy_reading, []).

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
 *   constraint_id: nafta_jurisdictional_boundary__capital_supremacy_reading
 *   human_readable: Capital Supremacy Reading: Trade Agreement Jurisdictional Override
 *   domain: international_trade_law/political_economy
 *
 * SUMMARY:
 *   Under the capital_supremacy reading of the NAFTA jurisdictional boundary
 *   kernel, trade agreement text functions as supreme law, subordinating
 *   domestic regulatory authority to capital mobility rights and investor
 *   protections. This reading interprets treaty clauses as empowering
 *   multinational enterprises and institutional investors to challenge
 *   labor-protective and environmental standards as non-tariff barriers
 *   justiciable under investor-state dispute settlement (ISDS). Domestic
 *   legislators and subnational regulators lose jurisdictional authority over
 *   standards that exceed treaty-partner norms; the cost is borne by workers,
 *   environmental constituencies, and the jurisdictions that lose policy
 *   space. This constraint is NOT a mountain (it is a contestable reading of
 *   treaty text, not a fixed natural law) nor a rope (genuine coordination is
 *   buried under substantial extraction and suppression). The claim/metric
 *   gap is intentional: the constraint is CLAIMED as tangled_rope
 *   (coordinating market access while extracting from labor/environmental
 *   standards) while the authored metrics describe extractiveness growing
 *   over time (0.48 → 0.78 across the interval) and suppression hardening as
 *   the reading stabilizes institutionally (0.52 → 0.71). This divergence is
 *   the measurement the corpus exists to take: does the reading's real
 *   operation match its coordination narrative, or has extraction grown to
 *   dominate the function?
 *
 * KEY AGENTS:
 *   - multinational_capital: Institutional beneficiary (arbitrage-driven rents from regulatory harmonization downward)
 *   - institutional_investors: Institutional beneficiary (capital mobility rights legally protected)
 *   - domestic_labor_standards_beneficiaries: Moderate-power victim (constrained exit, wage/safety erosion)
 *   - environmental_regulation_constituencies: Organized victim (excluded from dispute resolution, absorb regulatory races to the bottom)
 *   - subnational_regulatory_authorities: Moderate-power victim with identity_locked exit (jurisdictional authority colonized by trade law)
 *   - trade_dispute_tribunals: Institutional agenda-setter (interpret and enforce the capital_supremacy reading; structural interest in wide reading)
 *   - treaty_signatory_governments: Nominally institutional actors but operationally captured and excluded from redefining the reading
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(nafta_jurisdictional_boundary__capital_supremacy_reading, 0.78).
domain_priors:suppression_score(nafta_jurisdictional_boundary__capital_supremacy_reading, 0.71).
domain_priors:theater_ratio(nafta_jurisdictional_boundary__capital_supremacy_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(nafta_jurisdictional_boundary__capital_supremacy_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(nafta_jurisdictional_boundary__capital_supremacy_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(nafta_jurisdictional_boundary__capital_supremacy_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(nafta_jurisdictional_boundary__capital_supremacy_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(nafta_jurisdictional_boundary__capital_supremacy_reading, resistance, 0.59).

% --- Constraint claim ---
narrative_ontology:constraint_claim(nafta_jurisdictional_boundary__capital_supremacy_reading, tangled_rope).
narrative_ontology:human_readable(nafta_jurisdictional_boundary__capital_supremacy_reading, "Capital Supremacy Reading: Trade Agreement Jurisdictional Override").
narrative_ontology:topic_domain(nafta_jurisdictional_boundary__capital_supremacy_reading, "international_trade_law/political_economy").

domain_priors:requires_active_enforcement(nafta_jurisdictional_boundary__capital_supremacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(nafta_jurisdictional_boundary__capital_supremacy_reading, '77a187fb-8802-42d0-a85e-bb4f585bad18').
narrative_ontology:cs_kernel_codification('77a187fb-8802-42d0-a85e-bb4f585bad18', formalized).
narrative_ontology:cs_authority_grounding('77a187fb-8802-42d0-a85e-bb4f585bad18', extraction).
narrative_ontology:cs_interpretation_layer_present('77a187fb-8802-42d0-a85e-bb4f585bad18').
narrative_ontology:cs_reading_relation('77a187fb-8802-42d0-a85e-bb4f585bad18', nafta_jurisdictional_boundary__embedded_liberalism_reading, influences).
narrative_ontology:cs_reading_relation('77a187fb-8802-42d0-a85e-bb4f585bad18', nafta_jurisdictional_boundary__sovereignty_primacy_reading, coexists_with).
narrative_ontology:cs_axiom('77a187fb-8802-42d0-a85e-bb4f585bad18', foundational, capital_mobility_overrides_domestic_regulation).
narrative_ontology:cs_axiom_status(capital_mobility_overrides_domestic_regulation, holdable).
narrative_ontology:cs_axiom_grounding('77a187fb-8802-42d0-a85e-bb4f585bad18', capital_mobility_overrides_domestic_regulation, empirically_contingent).
narrative_ontology:cs_axiom('77a187fb-8802-42d0-a85e-bb4f585bad18', secondary, regulatory_harmonization_follows_lowest_common_denominator).
narrative_ontology:cs_axiom_status(regulatory_harmonization_follows_lowest_common_denominator, holdable).
narrative_ontology:cs_axiom_grounding('77a187fb-8802-42d0-a85e-bb4f585bad18', regulatory_harmonization_follows_lowest_common_denominator, empirically_contingent).
narrative_ontology:cs_reference_frame('77a187fb-8802-42d0-a85e-bb4f585bad18', capital_mobility_supremacy).
narrative_ontology:cs_drift_state('77a187fb-8802-42d0-a85e-bb4f585bad18', contemporary_post_mandatrophy_diagnosis, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('77a187fb-8802-42d0-a85e-bb4f585bad18', '').
narrative_ontology:cs_kernel_id(nafta_jurisdictional_boundary__capital_supremacy_reading, nafta_jurisdictional_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(nafta_jurisdictional_boundary__capital_supremacy_reading, multinational_capital).
narrative_ontology:constraint_beneficiary(nafta_jurisdictional_boundary__capital_supremacy_reading, institutional_investors).
narrative_ontology:constraint_victim(nafta_jurisdictional_boundary__capital_supremacy_reading, domestic_labor_standards_beneficiaries).
narrative_ontology:constraint_victim(nafta_jurisdictional_boundary__capital_supremacy_reading, environmental_regulation_constituencies).
narrative_ontology:constraint_victim(nafta_jurisdictional_boundary__capital_supremacy_reading, subnational_regulatory_authorities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Operates under the capital_supremacy reading as institutional beneficiaries who can challenge domestic labor/environmental standards as non-tariff barriers. They extract rents from regulatory arbitrage: the ability to shift production to low-standard jurisdictions while exporting to high-standard markets, protected by treaty-sanctioned capital mobility rights. Their exit options are arbitrage-grade — they choose production locations based on global cost optimization and can relocate investment across treaty territories to exploit regulatory differences.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__capital_supremacy_reading, multinational_capital, beneficiary,
    institutional, civilizational, arbitrage, global).

% Hold financial instruments (equity, bonds, derivatives) whose value depends on capital mobility rights guaranteed by the capital_supremacy reading. They benefit from the reading's guarantee that capital cannot be constrained by domestic labor-protective or environmental policies. They have arbitrage-grade exit: they can shift investment across territories and asset classes to optimize returns under the reading.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__capital_supremacy_reading, institutional_investors, beneficiary,
    institutional, civilizational, arbitrage, global).

% Workers and labor unions in treaty territories who relied on domestic labor protections (wage floors, overtime rules, union organizing rights, workplace safety standards). Under the capital_supremacy reading, these standards face judicial challenge when they increase production costs relative to laxer treaty-partner jurisdictions. They bear wage suppression from regulatory competition downward and cannot exit: immigration barriers prevent relocation to higher-wage jurisdictions; unionization-busting cannot compete on cost without destroying the standards that protect them; they lack voice in trade dispute resolution.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__capital_supremacy_reading, domestic_labor_standards_beneficiaries, payer,
    moderate, biographical, constrained, national).

% Environmental organizations, indigenous communities, pollution-impacted residents in treaty territories. Domestically enacted environmental protections (carbon taxes, land-use restrictions, water-quality standards, chemical bans) are justiciable under the capital_supremacy reading as regulatory takings or discriminatory non-tariff barriers. They bear the cost of harmonization downward and are excluded from tribunal proceedings despite absorbing environmental consequences. They cannot exit the territory bearing the environmental burden.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__capital_supremacy_reading, environmental_regulation_constituencies, payer,
    organized, generational, constrained, national).

% States, provinces, municipalities that enacted labor and environmental standards through democratic processes within their territories. The capital_supremacy reading strips their jurisdictional authority: they cannot set wages, working conditions, or pollution limits exceeding treaty-partner standards without facing investor litigation. Their policy space is colonized by trade law; they lose agenda-setting authority. Exit is identity-locked: their institutional identity is defined by territorial sovereignty, yet the reading dissolves the functional substance of sovereignty in regulatory domains. They cannot cease being a state without ceasing to exist.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__capital_supremacy_reading, subnational_regulatory_authorities, payer,
    moderate, biographical, identity_locked, regional).
narrative_ontology:stakeholder_secondary_role(nafta_jurisdictional_boundary__capital_supremacy_reading, subnational_regulatory_authorities, excluded).

% ISDS panels and appellate trade bodies that interpret and enforce the capital_supremacy reading. They adjudicate whether domestic standards are compatible with capital mobility rights, operating outside domestic constitutional law and overriding domestic courts. They are the enforcement machinery of the reading itself; wider reading interpretation generates more cases and institutional power. They have structural incentive to expand the reading and sustain its supremacy over domestic regulation.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__capital_supremacy_reading, trade_dispute_tribunals, agenda_setter,
    institutional, generational, analytical, global).

% Federal governments that signed the trade agreement. Nominally parties to the arrangement, they are operationally captured: they cannot unilaterally reinterpret the treaty text (the capital_supremacy reading is stabilized as doctrine); they cannot withdraw without diplomatic and economic costs; they face investor litigation if they attempt to re-regulate. Excluded from redefining the constraint's terms once the reading is institutionalized.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__capital_supremacy_reading, treaty_signatory_governments, excluded,
    institutional, generational, constrained, global).

% Economists, legal scholars, and policy advocates who hold or advance alternative readings (embedded_liberalism, sovereignty_primacy). They argue the treaty text permits regulatory space for legitimate domestic standards. They lack formal decision authority in trade dispute resolution; their interpretive voices are excluded from the enforcement structure that consolidates the capital_supremacy reading.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__capital_supremacy_reading, alternative_reading_advocates, observer,
    moderate, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(nafta_jurisdictional_boundary__capital_supremacy_reading, multinational_capital).
narrative_ontology:fixing_cost_class(nafta_jurisdictional_boundary__capital_supremacy_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates cross-border capital mobility and predictable regulatory treatment across three treaty territories by establishing that trade agreement text is supreme law and capital mobility rights override domestic regulatory authority. This solves the collective-action problem faced by multinational firms operating across fragmented regulatory landscapes and provides institutional guarantee that capital mobility cannot be constrained by domestic labor-protective or environmental policies.
% TRANSFER_FUNCTION: Transfers regulatory authority upward from domestic legislatures and subnational regulators to international trade tribunals. Transfers rents from labor and environmental constituencies to multinational capital and institutional investors. Capital gains predictable low-regulation operating environments and confirmed mobility rights; domestic workers and environmental constituencies lose policy-making authority and absorb downward-harmonized standards. The transfer mechanism is treaty-sanctioned: domestic governments that sign commit to subordinating their regulatory authority to capital mobility protections.
% ABSENT_VOICES: Systematically excluded from the capital_supremacy reading's interpretive and dispute-resolution framework: labor unions and worker organizations (no voice in ISDS proceedings, no seat on tribunals interpreting labor standards), environmental organizations and pollution-impacted communities (excluded from tribunal access despite absorbing environmental costs), indigenous communities with land-use and environmental interests (no standing in capital mobility adjudication). These constituencies have no avenue to challenge the reading's interpretive supremacy; they can only register opposition through domestic politics, which the reading constrains.
% DISAPPEARANCE_RATIONALE: If the capital_supremacy reading and its enforcement apparatus disappeared, regulatory authority would revert to domestic legislatures; subnational authorities would regain jurisdictional autonomy; labor standards would re-tighten in high-wage jurisdictions; environmental standards would diverge by territory rather than harmonize downward. Investment patterns would shift immediately: firms would no longer be guaranteed regulatory uniformity and low-standard competitive advantage; multinational supply chains would reorganize around localized regulatory costs rather than seeking regulatory-arbitrage opportunities. The world rearranges because substantial institutional arrangements (labor markets, environmental governance, capital allocation) depend on the reading being in force.
% FOUNDING_PROBLEM: Early cross-border trade faced coordination problems: divergent regulatory standards created uncertainty for multinational firms seeking to operate across territories; lack of predictable rules for capital repatriation deterred investment; firms could not optimize production costs when labor and environmental standards differed across jurisdictions. The capital_supremacy reading was built to solve this: establish that trade agreement text is supreme law over domestic regulation so capital mobility is guaranteed and multinational firms can operate on a predictable regulatory surface (the lowest-common-denominator standard across treaty territories).
% FOUNDING_PROBLEM_CORROBORATION: Multinational capital and institutional investors attest the founding problem is LIVE and the capital_supremacy reading is necessary for efficient cross-border investment. Labor unions, environmental organizations, and subnational regulators attest the founding problem has been SOLVED in its original form (trade uncertainty is reduced) but the reading has created a NEW coordination problem: managing environmental externalities, preserving labor protections, and protecting public health from capital-driven regulatory races to the bottom. Independent economic analysis from outside the benefiting parties documents regulatory harmonization DOWNWARD (environmental standards, labor floors, tax rates declining across treaty territories after agreements interpreted under the capital_supremacy reading) — corroborating that the constraint's actual function has shifted from coordination-solving to extraction-enabling, and the founding problem's original justification no longer explains the constraint's persistence.
narrative_ontology:disappearance_verdict(nafta_jurisdictional_boundary__capital_supremacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(nafta_jurisdictional_boundary__capital_supremacy_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(nafta_jurisdictional_boundary__capital_supremacy_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(nafta_jurisdictional_boundary__capital_supremacy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(nafta_jurisdictional_boundary__capital_supremacy_reading, 0.78, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(nafta_jurisdictional_boundary__capital_supremacy_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(nafta_jurisdictional_boundary__capital_supremacy_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(nafta_jurisdictional_boundary__capital_supremacy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high and rising (0.48 → 0.78). This reflects the reading's core function: it transfers regulatory authority upward to trade tribunals and extracts downward-harmonized standards as the cost. At t=0, extractiveness is moderate because the reading is newly stabilized and alternative readings (embedded_liberalism, sovereignty_primacy) still compete for interpretive legitimacy in some jurisdictions. By t=32, extractiveness reaches plateau (0.78) as the reading becomes doctrine: trade tribunals consistently interpret capital mobility rights as superior to domestic standards; multinational firms win most ISDS cases; regulatory harmonization is downward and stable. Suppression is high (0.71 at plateau) and rising steadily (0.52 → 0.71). This is not coercive physical force but institutional suppression: domestic legislatures are legally barred from re-regulating; courts cannot overturn treaty interpretations; subnational authorities lose jurisdictional standing. Resistance from labor unions and environmental organizations is persistent (0.59) but structurally ineffective (they lack tribunal access, cannot veto treaty terms, cannot exit the territory bearing the costs). Theater is moderate and rising (0.18 → 0.42), then plateaus. The early phase shows the reading performing legitimacy work: tribunals frame investor protections as coordination mechanisms, trade officials describe harmonization as inevitable efficiency. By t=20+, theater stabilizes at 0.42: the reading is now doctrine, less need for performative legitimacy justification. The functional core (extraction + suppression) is obvious to labor and environmental constituencies, but the institutional structure no longer bothers with sophisticated theater — the reading's supremacy is established. Accessibility of alternatives collapses (0.68) as the reading institutionalizes: domestic political movements for labor re-regulation or environmental re-protection face legal barriers to implementation; jurisdictions that defect face investor suits; the reading appears as inevitable law rather than contingent political choice.
 *
 * PERSPECTIVAL GAP:
 *   The engine should compute substantial perspectival divergence. From the multinational capital seat (beneficiary, arbitrage exit, institutional power): the capital_supremacy reading solves a genuine coordination problem (predictable regulatory surfaces, capital mobility protections) with moderate overhead (some transparency requirements, treaty obligations). From the subnational regulatory authority seat (victim, identity_locked exit, moderate power): the reading is hostile colonization — loss of democratic authority, policy space stripped, unable to respond to local environmental or labor crises. From the labor standards beneficiary seat (victim, constrained exit, moderate power): the reading produces wage suppression and safety erosion through regulatory competition. The gap exists because the reading's structural asymmetry — capital gains mobility rights while workers lose jurisdictional protection — is not a measurement artifact but the constraint's core feature. Per-seat classification should diverge: multinational_capital should compute as coordinated (genuine benefit, predictable rules, positive d direction); labor_standards beneficiaries should compute as extracted (costs imposed, suppressed exit, high d direction); subnational_regulatory_authorities should compute as identity-trapped payers (functional loss of sovereignty, impossible to exit the role without identity dissolution).
 *
 * DIRECTIONALITY LOGIC:
 *   Multinational capital has d near 0.0 (full beneficiary): the reading grants them arbitrage rights, capital mobility protections, access to ISDS litigation, and the option to shift production to lower-standard jurisdictions. They benefit directly from the constraint and have high exit mobility (can relocate investment, forum-shop for favorable regulatory interpretation). Institutional investors similarly have d near 0.0: their financial instruments depend on stable capital mobility rights guaranteed by this reading. Domestic labor constituencies have d near 0.95 (full target): they bear wage suppression from regulatory competition, lack voice in trade dispute resolution, cannot exit the territory absorbing the costs, and have constrained labor-mobility options. Environmental constituencies have d near 0.90: they absorb environmental degradation from downward-harmonizing standards, are excluded from tribunal proceedings, and cannot escape the territory bearing the environmental burden. Subnational regulators have d near 0.92: their jurisdictional authority is directly constrained by this reading; they cannot re-regulate without facing investor challenges; they are identity-locked to a sovereignty role that the reading has functionally stripped. Trade tribunals have d near 0.15 (beneficiary side): they collect institutional power and fee-generating disputes from the reading; they have no incentive to narrow the reading. Treaty governments have d near 0.70 (mixed): they nominally signed the treaty but are operationally captured — they cannot unilaterally reinterpret without violating investor protections they committed to defend, yet they absorb political cost from labor and environmental constituencies harmed by the reading.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy question: does the capital_supremacy reading preserve the founding problem it was designed to solve, or has the founding problem shifted while the constraint persists? At t=0, the founding problem (uncertainty about regulatory and capital-mobility rights for multinational firms) is LIVE. The reading solves it: firms know labor standards cannot block investment, capital can move across borders, regulatory landscapes are predictable. By t=32, the founding problem is CONTESTED. Multinational capital and trade officials attest it is still live; labor and environmental constituencies attest it has been solved but the reading now creates a NEW coordination problem (managing externalities from regulatory races to the bottom, preserving health and safety floors, protecting environmental commons from capital-driven deregulation). The disappearance_verdict is world_rearranges: if the capital_supremacy reading vanished, regulatory authority would revert to domestic legislatures, investment patterns would shift, and the economic organization would reorganize around localized regulatory costs rather than uniform low-standard treatment. This suggests the founding problem has shifted from coordination (early trade uncertainty) to extraction (current capital mobility taking precedence over labor/environmental protection). The constraint persists because multinational capital benefits from it and has institutional power through trade tribunals; labor and environmental constituencies cannot fix it (lack access to treaty amendment, face costs of unilateral withdrawal). The mandatrophy signal: a reading that solves a founding problem in t=0 but creates new harms in t=32, with no decentralized mechanism to correct it, is a mandate operating past its founding justification. The reading should be flagged for review: either the founding problem has legitimately evolved and the reading should be renegotiated, or the reading is being preserved as pure extraction despite the founding problem's resolution.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    capital_supremacy_vs_legitimate_regulation_ambiguity,
    'Are labor-protective and environmental standards inherently trade-distorting non-tariff barriers, or can they coexist with trade obligations when applied non-discriminatorily?',
    'Empirical comparison: jurisdictions holding embedded_liberalism reading preserve labor/environmental standards while maintaining trade relationships; capital investment flows do not require regulatory harmonization downward to proceed. Alternatively, systematic analysis of ISDS cases shows whether tribunals consistently invalidate labor/environmental standards or distinguish legitimate regulations from protectionist disguise.',
    'If standards and trade can coexist, the capital_supremacy reading is not structurally necessary — it is a political choice to subordinate domestic authority. The constraint would reclassify from tangled_rope (mandatory coordination requiring extraction) to snare (extraction riding on a disputed claim of inevitability). If standards and trade are truly incompatible, the reading is accurate and the extraction reflects genuine cost of coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(capital_supremacy_vs_legitimate_regulation_ambiguity, empirical, 'Whether labor and environmental standards are fundamentally incompatible with trade or can coexist under alternative interpretive frameworks.').

omega_variable(
    institutional_entrenchment_of_capital_supremacy_reading,
    'Has the capital_supremacy reading become self-reinforcing institutional doctrine, or does it remain contingent on active defense against alternative interpretations?',
    'Textual analysis of tribunal decisions: do recent ISDS panels cite capital_supremacy reasoning as settled precedent, or do they engage with competing interpretations (embedded_liberalism, sovereignty_primacy)? If settled precedent dominates, the reading has moved from contested interpretation to institutional reality. Survey of treaty negotiation texts in new agreements: are capital mobility and regulatory subordination explicitly included, or are they inferred from ISDS practice interpreting older treaties?',
    'If the reading is deeply entrenched (precedent-based, self-reinforcing through tribunal hiring and interpretive tradition), it becomes harder to dislodge through alternative interpretation — it has institutionalized suppression. If the reading is still defended against competing interpretations, it remains politically contestable. Higher entrenchment suggests mandatrophy: the reading persists not because of founding-problem necessity but because institutional interests (tribunals, capital, treaty bureaucracies) benefit from its perpetuation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(institutional_entrenchment_of_capital_supremacy_reading, empirical, 'Whether the capital_supremacy reading has become institutionally entrenched doctrine or remains contingent on active interpretive defense.').

omega_variable(
    regulatory_harmonization_direction_ambiguity,
    'Does the capital_supremacy reading drive harmonization DOWNWARD (toward lowest standards) or can regulatory harmonization proceed UPWARD (toward highest standards)?',
    'Empirical analysis: track labor standards, environmental standards, and workplace safety rules across treaty territories pre-agreement and post-agreement. If harmonization is downward, the constraint is extractive from labor/environment. If harmonization is bidirectional or upward-capable, the extraction is a policy choice, not structural necessity.',
    'If only downward harmonization occurs, the capital_supremacy reading is fundamentally extractive from labor/environmental constituencies — it locks in low standards as the binding norm. If upward harmonization is possible under this reading, the extraction is contingent on how the reading is deployed, not inevitable from the reading itself. This would support a recalibration: the constraint could be reconstructed as rope (genuine coordination) if harmonization proceeded upward.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(regulatory_harmonization_direction_ambiguity, empirical, 'Whether the capital_supremacy reading structurally requires downward regulatory harmonization or permits upward harmonization as an alternative.').

omega_variable(
    subnational_identity_lock_mechanism,
    'Is the subnational_regulatory_authorities'' exit option truly identity_locked (they cannot cease being a state without identity dissolution), or can they exit through treaty withdrawal or re-negotiation?',
    'Political economy analysis: what is the actual cost of treaty withdrawal for a territory? Does withdrawal trigger economic sanctions, investor suits under ISDS, or geopolitical isolation? Can subnational regulators within a federal system re-negotiate the treaty''s domestic implementation without federal consent? Are there examples of successful re-regulation or treaty renegotiation that expanded domestic policy space?',
    'If the cost of exit is prohibitive but not identity-destroying, exit_options should be ''constrained'' rather than ''identity_locked''. If there are examples of successful re-regulation, exit_options might be ''mobile''. This affects the computed directionality for subnational regulators and their classification as victims. Higher exit costs → higher directionality toward target → higher computed extraction from their seat.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(subnational_identity_lock_mechanism, empirical, 'Whether subnational regulators are truly identity-locked to the constraint or can exit through political re-negotiation at prohibitive but not identity-destroying cost.').

omega_variable(
    kernel_decomposition_reading_contest,
    'Are the capital_supremacy, embedded_liberalism, and sovereignty_primacy readings genuinely competing interpretations of a single ambiguous kernel (the treaty text), or are they distinct constraints addressing different aspects of trade law?',
    'Textual analysis: do all three readings cite the same treaty clauses in defense of their interpretation? If yes, they are competing readings of a shared kernel. If they emphasize different clauses or different meta-rules for treaty interpretation, they may be addressing distinct constraints (e.g., capital mobility clauses vs. regulatory flexibility clauses as separate normative domains).',
    'If they are genuinely competing readings of one kernel, the constraint family structure is correct and the committer frame applies. If they are distinct constraints addressing separate aspects of trade, each story should be independent rather than reading-related. This affects how the engine should model their relationship: as competing interpretations of the same text (kernel readings) or as structurally separable constraints (network affects_constraints).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_decomposition_reading_contest, conceptual, 'Whether the three readings are interpretations of a single ambiguous treaty text or distinct constraints addressing different aspects of trade law.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(nafta_jurisdictional_boundary__capital_supremacy_reading, 0, 32).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(naft_tr_t0, nafta_jurisdictional_boundary__capital_supremacy_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement(naft_tr_t4, nafta_jurisdictional_boundary__capital_supremacy_reading, theater_ratio, 4, 0.24).
narrative_ontology:measurement(naft_tr_t8, nafta_jurisdictional_boundary__capital_supremacy_reading, theater_ratio, 8, 0.3).
narrative_ontology:measurement(naft_tr_t12, nafta_jurisdictional_boundary__capital_supremacy_reading, theater_ratio, 12, 0.36).
narrative_ontology:measurement(naft_tr_t16, nafta_jurisdictional_boundary__capital_supremacy_reading, theater_ratio, 16, 0.39).
narrative_ontology:measurement(naft_tr_t20, nafta_jurisdictional_boundary__capital_supremacy_reading, theater_ratio, 20, 0.41).
narrative_ontology:measurement(naft_tr_t24, nafta_jurisdictional_boundary__capital_supremacy_reading, theater_ratio, 24, 0.42).
narrative_ontology:measurement(naft_tr_t28, nafta_jurisdictional_boundary__capital_supremacy_reading, theater_ratio, 28, 0.42).
narrative_ontology:measurement(naft_tr_t32, nafta_jurisdictional_boundary__capital_supremacy_reading, theater_ratio, 32, 0.42).

% Extraction over time
narrative_ontology:measurement(naft_be_t0, nafta_jurisdictional_boundary__capital_supremacy_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(naft_be_t4, nafta_jurisdictional_boundary__capital_supremacy_reading, base_extractiveness, 4, 0.56).
narrative_ontology:measurement(naft_be_t8, nafta_jurisdictional_boundary__capital_supremacy_reading, base_extractiveness, 8, 0.62).
narrative_ontology:measurement(naft_be_t12, nafta_jurisdictional_boundary__capital_supremacy_reading, base_extractiveness, 12, 0.68).
narrative_ontology:measurement(naft_be_t16, nafta_jurisdictional_boundary__capital_supremacy_reading, base_extractiveness, 16, 0.72).
narrative_ontology:measurement(naft_be_t20, nafta_jurisdictional_boundary__capital_supremacy_reading, base_extractiveness, 20, 0.75).
narrative_ontology:measurement(naft_be_t24, nafta_jurisdictional_boundary__capital_supremacy_reading, base_extractiveness, 24, 0.77).
narrative_ontology:measurement(naft_be_t28, nafta_jurisdictional_boundary__capital_supremacy_reading, base_extractiveness, 28, 0.78).
narrative_ontology:measurement(naft_be_t32, nafta_jurisdictional_boundary__capital_supremacy_reading, base_extractiveness, 32, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(naft_su_t0, nafta_jurisdictional_boundary__capital_supremacy_reading, suppression_requirement, 0, 0.52).
narrative_ontology:measurement(naft_su_t4, nafta_jurisdictional_boundary__capital_supremacy_reading, suppression_requirement, 4, 0.58).
narrative_ontology:measurement(naft_su_t8, nafta_jurisdictional_boundary__capital_supremacy_reading, suppression_requirement, 8, 0.63).
narrative_ontology:measurement(naft_su_t12, nafta_jurisdictional_boundary__capital_supremacy_reading, suppression_requirement, 12, 0.67).
narrative_ontology:measurement(naft_su_t16, nafta_jurisdictional_boundary__capital_supremacy_reading, suppression_requirement, 16, 0.69).
narrative_ontology:measurement(naft_su_t20, nafta_jurisdictional_boundary__capital_supremacy_reading, suppression_requirement, 20, 0.7).
narrative_ontology:measurement(naft_su_t24, nafta_jurisdictional_boundary__capital_supremacy_reading, suppression_requirement, 24, 0.71).
narrative_ontology:measurement(naft_su_t28, nafta_jurisdictional_boundary__capital_supremacy_reading, suppression_requirement, 28, 0.71).
narrative_ontology:measurement(naft_su_t32, nafta_jurisdictional_boundary__capital_supremacy_reading, suppression_requirement, 32, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(nafta_jurisdictional_boundary__capital_supremacy_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(nafta_jurisdictional_boundary__capital_supremacy_reading, 0.18).
narrative_ontology:affects_constraint(nafta_jurisdictional_boundary__capital_supremacy_reading, nafta_jurisdictional_boundary__embedded_liberalism_reading).
narrative_ontology:affects_constraint(nafta_jurisdictional_boundary__capital_supremacy_reading, nafta_jurisdictional_boundary__sovereignty_primacy_reading).

% DUAL FORMULATION NOTE:
% The NAFTA jurisdictional boundary kernel is instantiated as three distinct constraint stories: capital_supremacy_reading (this story, treating trade agreement as supreme law), embedded_liberalism_reading (treaty as framework balancing market access with legitimate policy space), and sovereignty_primacy_reading (treaty as subordinate to sovereign domestic authority). Each reading extracts a different epsilon from the same treaty text — the readings' different beneficiary/victim structures and suppression mechanisms are not measurement artifacts but consequences of fundamentally different interpretations of ambiguous treaty language. All three stories share the same kernel (the treaty text) but instantiate it as three structurally distinct constraints. Network edges link them as reading relations (coexist_with / influences) declared in cs_structure. The constraint family represents the constitutional/jurisprudential reality: one formal text, multiple readings, multiple experienced constraints depending on which interpretation dominates in a jurisdiction.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
