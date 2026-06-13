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
 *   constraint_id: nafta_jurisdictional_boundary__embedded_liberalism_reading
 *   human_readable: NAFTA Jurisdictional Boundary (Embedded Liberalism Reading)
 *   domain: international_trade/regulatory_federalism/political_economy
 *
 * SUMMARY:
 *   The embedded liberalism reading of the NAFTA jurisdictional boundary
 *   holds that trade agreements can accommodate legitimate domestic policy
 *   space for labor and environmental protection provided regulations are
 *   non-discriminatory (applied equally to domestic and foreign producers).
 *   Under this reading, the text of NAFTA Chapter 11 (investor rights) and
 *   Articles 1102–1104 (national treatment and most-favored-nation status)
 *   permit environmental and labor standards as 'legitimate objectives' of
 *   the signatory nations, even when they restrict trade, so long as they do
 *   not discriminate by nationality. The constraint operates through the
 *   interaction of this text with the ISDS mechanism: while the text
 *   supposedly permits policy space, the structure of investor-state dispute
 *   settlement—with tribunals staffed by trade lawyers, no transparency, and
 *   enormous litigation costs—creates a strong chilling effect on domestic
 *   regulation even when it is legally defensible under embedded liberalism
 *   language. The measured extraction reflects this asymmetry: genuine
 *   coordination gain (multinational market access, tariff reduction) paired
 *   with asymmetric cost distribution (domestic regulators bear litigation
 *   risk; multinational capital benefits from the threat).
 *
 * KEY AGENTS:
 *   - multinational_capital: Institutional power, arbitrage exit, global scope — benefits from predictable market access and litigation threat against regulatory tightening
 *   - domestic_regulatory_agencies: Institutional power, constrained exit, national scope — bear litigation costs and regulatory chilling despite theoretical policy space
 *   - labor_and_environmental_advocates: Organized power, constrained exit, national scope — excluded from ISDS and diffusely harmed by chilled regulation
 *   - signatory_governments: Institutional power, trapped exit, continental scope — administer the agreement while bearing internal conflict between trade obligations and domestic law
 *   - arbitral_tribunals: Institutional power, analytical exit, global scope — interpret the 'legitimate objectives' boundary that defines actual policy space
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(nafta_jurisdictional_boundary__embedded_liberalism_reading, 0.58).
domain_priors:suppression_score(nafta_jurisdictional_boundary__embedded_liberalism_reading, 0.42).
domain_priors:theater_ratio(nafta_jurisdictional_boundary__embedded_liberalism_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(nafta_jurisdictional_boundary__embedded_liberalism_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(nafta_jurisdictional_boundary__embedded_liberalism_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(nafta_jurisdictional_boundary__embedded_liberalism_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(nafta_jurisdictional_boundary__embedded_liberalism_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(nafta_jurisdictional_boundary__embedded_liberalism_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(nafta_jurisdictional_boundary__embedded_liberalism_reading, tangled_rope).
narrative_ontology:human_readable(nafta_jurisdictional_boundary__embedded_liberalism_reading, "NAFTA Jurisdictional Boundary (Embedded Liberalism Reading)").
narrative_ontology:topic_domain(nafta_jurisdictional_boundary__embedded_liberalism_reading, "international_trade/regulatory_federalism/political_economy").

domain_priors:requires_active_enforcement(nafta_jurisdictional_boundary__embedded_liberalism_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(nafta_jurisdictional_boundary__embedded_liberalism_reading, 'd97dc6c4-43bd-4cc2-b960-ce9f67147f84').
narrative_ontology:cs_kernel_codification('d97dc6c4-43bd-4cc2-b960-ce9f67147f84', fixed_text).
narrative_ontology:cs_authority_grounding('d97dc6c4-43bd-4cc2-b960-ce9f67147f84', extraction).
narrative_ontology:cs_interpretation_layer_present('d97dc6c4-43bd-4cc2-b960-ce9f67147f84').
narrative_ontology:cs_reading_relation('d97dc6c4-43bd-4cc2-b960-ce9f67147f84', nafta_jurisdictional_boundary__capital_supremacy_reading, influences).
narrative_ontology:cs_reading_relation('d97dc6c4-43bd-4cc2-b960-ce9f67147f84', nafta_jurisdictional_boundary__sovereignty_primacy_reading, coexists_with).
narrative_ontology:cs_axiom('d97dc6c4-43bd-4cc2-b960-ce9f67147f84', foundational, regulatory_non_discrimination_permits_policy_space).
narrative_ontology:cs_axiom_status(regulatory_non_discrimination_permits_policy_space, holdable).
narrative_ontology:cs_axiom_grounding('d97dc6c4-43bd-4cc2-b960-ce9f67147f84', regulatory_non_discrimination_permits_policy_space, deontological).
narrative_ontology:cs_axiom('d97dc6c4-43bd-4cc2-b960-ce9f67147f84', foundational, capital_supremacy_rejects_embedded_space).
narrative_ontology:cs_axiom_status(capital_supremacy_rejects_embedded_space, holdable).
narrative_ontology:cs_axiom_grounding('d97dc6c4-43bd-4cc2-b960-ce9f67147f84', capital_supremacy_rejects_embedded_space, instrumental).
narrative_ontology:cs_reference_frame('d97dc6c4-43bd-4cc2-b960-ce9f67147f84', non_discriminatory_regulatory_pluralism).
narrative_ontology:cs_drift_state('d97dc6c4-43bd-4cc2-b960-ce9f67147f84', contemporary_arbitral_practice_2024, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('d97dc6c4-43bd-4cc2-b960-ce9f67147f84', '').
narrative_ontology:cs_kernel_id(nafta_jurisdictional_boundary__embedded_liberalism_reading, nafta_jurisdictional_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(nafta_jurisdictional_boundary__embedded_liberalism_reading, multinational_capital).
narrative_ontology:constraint_victim(nafta_jurisdictional_boundary__embedded_liberalism_reading, domestic_regulatory_agencies).
narrative_ontology:constraint_victim(nafta_jurisdictional_boundary__embedded_liberalism_reading, labor_and_environmental_advocates).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(nafta_jurisdictional_boundary__embedded_liberalism_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(nafta_jurisdictional_boundary__embedded_liberalism_reading, 'none', 1).

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
 *   The extractiveness series rises from 0.35 (initial estimate at ratification, representing the hope embodied in embedded liberalism language) to 0.58 (current estimate reflecting 30 years of arbitral practice narrowing policy space). The theater_ratio series rises from 0.25 to 0.48, indicating that an increasing share of compliance activity is defensive (preparing for litigation, pre-emptively watering down rules) rather than substantive (achieving stated environmental or labor objectives). The suppression_requirement series is comparatively flat (0.28 to 0.42) because the constraint does not require active coercion of the target populations—the litigation threat and chilling effect are sufficient to suppress regulatory expansion. The time-series reflects Mandatrophy drift: the founding problem (reconciling open markets with democratic regulatory space) was live in 1994; by 2024 it is effectively dead as a lived commitment of the signatory governments, even though the text remains formally unchanged. The coordination function persists (tariff reduction, market access) but the extractive overlay has accumulated without corresponding adjustment of the agreement's legitimacy claims.
 *
 * PERSPECTIVAL GAP:
 *   From the multinational capital seat (powerful institutional actor with arbitrage exit), the constraint is a genuine rope: it enables profitable market access across three nations and provides legal recourse against what they perceive as disguised protectionism. From the domestic regulatory agency seat (institutional power but trapped exit), the same constraint is experienced as a tangled rope tilted toward extraction: they have theoretical policy space but must litigate at massive cost to defend it, and arbitral rulings have narrowed that space in practice. From the labor/environmental advocate seat (organized power, constrained exit), the constraint is closer to a snare: they are excluded from the process that determines their welfare and experience only the costs (foregone protections) without access to the dispute mechanism. The engine computes these divergences from the structural data (power, exit_options, beneficiary/victim declarations); the claimed type (tangled_rope) represents the dominant structural pattern: genuine coordination overlaid with asymmetric extraction maintained by litigation threat.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality for multinational_capital derives from beneficiary declaration + institutional power + arbitrage exit: d ≈ 0.15 (substantially subsidized by the constraint). Directionality for domestic_regulatory_agencies and labor_and_environmental_advocates derives from victim declaration + institutional/organized power + constrained exit: d ≈ 0.75–0.85 (substantially targeted). The asymmetry is structural: the constraint was designed to benefit mobile capital (which can arbitrage across jurisdictions and needs predictable rules) while imposing costs on immobile labor and environmental constituencies (which cannot exit the jurisdiction and have no standing in the dispute process). The embedding of 'policy space' language was meant to soften this asymmetry but has failed because arbitral interpretation has been narrow and litigation costs remain high regardless of legal defensibility.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem—reconciling open markets with regulatory space for labor and environmental protection—was live in 1994 and remains formally stated in the agreement's preamble. However, the lived commitment to this reconciliation has atrophied significantly. Arbitral decisions increasingly narrow the 'legitimate objectives' boundary, and governments have learned that defending regulations in ISDS is costly even when the law may be on their side. The theater_ratio rising from 0.25 to 0.48 indicates that compliance activity is increasingly performative: governments make rhetorical commitments to environmental and labor protection while privately accepting that NAFTA constraints make ambitious regulation prohibitively expensive. The constraint persists because the coordination function (tariff reduction, market access) remains politically salient and because the costs of exit (economic retaliation, loss of market access) are prohibitive. But the mechanism that was supposed to keep extraction bounded—the 'legitimate objectives' defense—has become theater: it is invoked in negotiating positions and academic arguments while being hollowed out in practice by arbitral narrowing and litigation costs. This is classic Piton drift: a genuine rope (the founding coordination) is overlaid with extraction (capital advantage through litigation threat), the coordination function persists, but the explicit defense against extraction (policy space language) becomes increasingly performative.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    legitimate_objectives_boundary_collapse,
    'What constitutes a ''legitimate objective'' that permits domestic regulation despite trade impact? Has arbitral interpretation of this boundary narrowed since 1994 such that regulations now treated as ISDS-vulnerable would have been treated as policy-space-protected at ratification?',
    'Meta-analysis of ISDS awards and tribunal interpretations of Chapter 11 Annex 1104 language, comparing early cases (1997–2005) with recent cases (2015–2024), holding constant the type of regulation (environmental, labor, health). Regulatory scholars outside the arbitral process can produce corroborating analysis.',
    'If the boundary has collapsed—if standards now classified as ISDS-vulnerable would have been protected in 1994—then the founding problem (reconciling open markets with regulatory space) has been formally betrayed. This would support the mandatrophy verdict and shift the reading toward capital supremacy. If the boundary has held stable, then extraction reflects regulatory choice by governments (constrained but conscious), not treaty erosion.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(legitimate_objectives_boundary_collapse, empirical, 'Whether the ''legitimate objectives'' boundary has materially narrowed through arbitral interpretation.').

omega_variable(
    suppression_mechanism_structural_vs_cognitive,
    'Is the measured regulatory chill (suppression = 0.42) a direct result of ISDS structure and litigation threat (structural suppression), or does it reflect internalized acceptance by regulators that trade obligations legitimately constrain their authority (cognitive suppression / identity fusion)?',
    'Interviews and policy analysis of government regulatory agencies in signatory nations: do regulators view the ISDS threat as an external constraint they resent, or as a legitimate boundary they have incorporated into their decision-making? Comparison with jurisdictions outside NAFTA trade agreements (e.g., European environmental regulators not bound by ISDS, or Canadian provinces operating without ISDS constraints) would show whether suppression persists absent the threat.',
    'If suppression is purely structural (external threat), then removing the ISDS mechanism would immediately unlock regulatory expansion. If suppression is partly cognitive (internalized acceptance of trade constraints as legitimate), then regulatory expansion would be slower even with ISDS removed, because the identity fusion between ''regulators'' and ''trade-law-compliant governments'' would persist. This affects the estimated cost of constraint removal.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_vs_cognitive, conceptual, 'Whether regulatory suppression is structural (ISDS threat) or internalized (identity fusion).').

omega_variable(
    embedded_liberalism_vs_capital_supremacy_divergence,
    'Does this reading (embedded liberalism—policy space preserved if non-discriminatory) produce substantively different constraints from the capital-supremacy reading (all regulatory restrictions violate the agreement unless explicitly carved out), or do they converge in practice despite textual differences?',
    'Comparative case analysis: identify regulations that would be classified as policy-space-protected under embedded liberalism but trade-restricting under capital supremacy (e.g., non-discriminatory environmental standards that impose costs on foreign firms). Document whether ISDS outcomes differ when embedded liberalism framing is explicitly invoked by governments, versus outcomes in jurisdictions using capital-supremacy framing. If outcomes are identical despite framing difference, the readings are observationally equivalent and produce the same constraint.',
    'If the readings diverge in practice (embedded liberalism permits regulations that capital supremacy forbids), then this reading instantiates a genuinely different constraint with lower extraction. If they converge (arbitral practice rejects embedded liberalism policy-space claims as readily as capital-supremacy claims), then this reading is a false-summit mountain—it claims to permit what it does not, and should be reclassified as capital supremacy under FSM evaluation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(embedded_liberalism_vs_capital_supremacy_divergence, empirical, 'Whether the embedded liberalism reading produces observationally different constraint outcomes from capital supremacy.').

omega_variable(
    excluded_voices_absence_problem,
    'Labor and environmental advocates are excluded from ISDS standing. Does this exclusion constitute a structural feature that forces asymmetric voice (capital can litigate, labor cannot), or is it merely a procedural artifact that does not affect the substantive balance because non-discrimination language provides equal protection?',
    'Comparison of outcomes in disputes brought by capital versus outcomes in regulatory decisions defending labor/environmental standards without dispute: do governments offer stronger defenses for regulations that are never litigated because capital has already internalized the threat? Analysis of government budget allocation and staffing for trade-litigation defense versus domestic regulatory protection.',
    'If exclusion is merely procedural and non-discrimination provides equal protection, then labor and environmental advocates have no standing but need no standing—their interests are protected by the substantive text. If exclusion is structural (capital can shift litigation costs onto governments; labor cannot), then it constitutes a unidirectional suppression mechanism. This would elevate resistance to the constraint.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(excluded_voices_absence_problem, conceptual, 'Whether ISDS exclusion of non-capital voices is a structural asymmetry or a procedural artifact.').

omega_variable(
    reading_identity_uncertainty,
    'Is this constraint genuinely the embedded liberalism reading, or has it become something closer to capital supremacy through arbitral practice? The text says embedded liberalism; the outcomes increasingly resemble capital supremacy.',
    'Formal decision analysis: for each major ISDS award post-2010, classify whether the tribunal explicitly acknowledged and then narrowed the ''legitimate objectives'' boundary (embedded liberalism), or simply treated the regulation as a trade violation (capital supremacy). If >60% of recent cases show capital-supremacy outcomes despite embedded liberalism framing, reclassify the constraint.',
    'If the reading has materially shifted toward capital supremacy in practice, then this constraint is a false-summit embedded liberalism—it claims a reading it does not instantiate. The true constraint would be capital supremacy. This would require generating a separate story for what embedded liberalism WOULD look like if arbitral practice were aligned with text.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_identity_uncertainty, empirical, 'Whether this constraint reflects the embedded liberalism reading or has drifted toward capital supremacy in practice.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(nafta_jurisdictional_boundary__embedded_liberalism_reading, 1994, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(naft_tr_t1994, nafta_jurisdictional_boundary__embedded_liberalism_reading, theater_ratio, 1994, 0.25).
narrative_ontology:measurement(naft_tr_t2002, nafta_jurisdictional_boundary__embedded_liberalism_reading, theater_ratio, 2002, 0.32).
narrative_ontology:measurement(naft_tr_t2008, nafta_jurisdictional_boundary__embedded_liberalism_reading, theater_ratio, 2008, 0.38).
narrative_ontology:measurement(naft_tr_t2012, nafta_jurisdictional_boundary__embedded_liberalism_reading, theater_ratio, 2012, 0.42).
narrative_ontology:measurement(naft_tr_t2018, nafta_jurisdictional_boundary__embedded_liberalism_reading, theater_ratio, 2018, 0.45).
narrative_ontology:measurement(naft_tr_t2024, nafta_jurisdictional_boundary__embedded_liberalism_reading, theater_ratio, 2024, 0.48).

% Extraction over time
narrative_ontology:measurement(naft_be_t1994, nafta_jurisdictional_boundary__embedded_liberalism_reading, base_extractiveness, 1994, 0.35).
narrative_ontology:measurement(naft_be_t2002, nafta_jurisdictional_boundary__embedded_liberalism_reading, base_extractiveness, 2002, 0.42).
narrative_ontology:measurement(naft_be_t2008, nafta_jurisdictional_boundary__embedded_liberalism_reading, base_extractiveness, 2008, 0.48).
narrative_ontology:measurement(naft_be_t2012, nafta_jurisdictional_boundary__embedded_liberalism_reading, base_extractiveness, 2012, 0.52).
narrative_ontology:measurement(naft_be_t2018, nafta_jurisdictional_boundary__embedded_liberalism_reading, base_extractiveness, 2018, 0.56).
narrative_ontology:measurement(naft_be_t2024, nafta_jurisdictional_boundary__embedded_liberalism_reading, base_extractiveness, 2024, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(naft_su_t1994, nafta_jurisdictional_boundary__embedded_liberalism_reading, suppression_requirement, 1994, 0.28).
narrative_ontology:measurement(naft_su_t2002, nafta_jurisdictional_boundary__embedded_liberalism_reading, suppression_requirement, 2002, 0.34).
narrative_ontology:measurement(naft_su_t2008, nafta_jurisdictional_boundary__embedded_liberalism_reading, suppression_requirement, 2008, 0.38).
narrative_ontology:measurement(naft_su_t2012, nafta_jurisdictional_boundary__embedded_liberalism_reading, suppression_requirement, 2012, 0.4).
narrative_ontology:measurement(naft_su_t2018, nafta_jurisdictional_boundary__embedded_liberalism_reading, suppression_requirement, 2018, 0.41).
narrative_ontology:measurement(naft_su_t2024, nafta_jurisdictional_boundary__embedded_liberalism_reading, suppression_requirement, 2024, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(nafta_jurisdictional_boundary__embedded_liberalism_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(nafta_jurisdictional_boundary__embedded_liberalism_reading, 0.18).
narrative_ontology:affects_constraint(nafta_jurisdictional_boundary__embedded_liberalism_reading, nafta_jurisdictional_boundary__capital_supremacy_reading).
narrative_ontology:affects_constraint(nafta_jurisdictional_boundary__embedded_liberalism_reading, nafta_jurisdictional_boundary__sovereignty_primacy_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of a contested kernel. The kernel NAFTA_JURISDICTIONAL_BOUNDARY has three distinct structural instantiations depending on how the text is read: (1) Embedded Liberalism (this story): trade agreement as coordination mechanism compatible with non-discriminatory domestic regulation; produces moderate extraction through litigation-threat chilling. (2) Capital Supremacy (sibling): trade agreement as supreme law overriding domestic regulation except where explicitly carved out; produces high extraction through direct foreclosure of regulatory authority. (3) Sovereignty Primacy (sibling): trade agreement as tariff-only coordination subordinate to full state regulatory authority; produces minimal extraction. Each reading has different ε, different beneficiary/victim structure, and different classification. The three stories are linked by network.affects_constraints to indicate kernel family membership. Epsilon-invariance is preserved: each story's ε is intrinsic to its reading, not observer-relative.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(nafta_jurisdictional_boundary__embedded_liberalism_reading, organized, 0.82).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
