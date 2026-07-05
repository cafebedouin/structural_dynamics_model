% ============================================================================
% CONSTRAINT STORY: nafta_jurisdictional_boundary__embedded_liberalism_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
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
 *   constraint_id: nafta_jurisdictional_boundary__embedded_liberalism_reading
 *   human_readable: NAFTA/USMCA Jurisdictional Boundary — Embedded Liberalism Reading
 *   domain: International Trade Law / Political Economy / Regulatory Federalism
 *
 * SUMMARY:
 *   This story instantiates the embedded-liberalism reading of the
 *   NAFTA/USMCA jurisdictional boundary: the treaty text is a framework for
 *   market access that explicitly preserves 'legitimate objectives' space for
 *   environmental, labor, and health regulation, provided such regulation is
 *   applied non-discriminatorily. On this reading the boundary is a genuine,
 *   partially-successful hybrid — real coordination (predictable market
 *   access, a non-discrimination baseline that limits naked protectionism)
 *   layered with real, asymmetric cost (litigation-defense burden and
 *   anticipatory regulatory chill falling on under-resourced agencies and the
 *   communities they would have protected). This is one of three readings of
 *   the same jurisdictional kernel; the other two (capital-supremacy,
 *   sovereignty-primacy) are separate constraint files with their own ε
 *   values, per the ε-invariance principle — this file does not describe or
 *   average over them.
 *
 * KEY AGENTS:
 *   - export_oriented_manufacturers: Primary beneficiary (organized/mobile) — gains predictable market access
 *   - regulatory_agencies_with_legitimate_objectives: agenda_setter/beneficiary (institutional/constrained) — retains real but bounded authority
 *   - under_resourced_domestic_regulators: Primary payer (moderate/trapped) — absorbs litigation-defense burden
 *   - communities_facing_regulatory_chill: Primary payer (powerless/trapped) — bears the cost of standards never adopted
 *   - trade_dispute_panels: agenda_setter (institutional/analytical) — defines the boundary case by case
 *   - environmental_and_labor_advocacy_groups: excluded (organized/constrained) — no standing in disputes
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(nafta_jurisdictional_boundary__embedded_liberalism_reading, 0.46).
domain_priors:suppression_score(nafta_jurisdictional_boundary__embedded_liberalism_reading, 0.4).
domain_priors:theater_ratio(nafta_jurisdictional_boundary__embedded_liberalism_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(nafta_jurisdictional_boundary__embedded_liberalism_reading, extractiveness, 0.46).
narrative_ontology:constraint_metric(nafta_jurisdictional_boundary__embedded_liberalism_reading, suppression_requirement, 0.4).
narrative_ontology:constraint_metric(nafta_jurisdictional_boundary__embedded_liberalism_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(nafta_jurisdictional_boundary__embedded_liberalism_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(nafta_jurisdictional_boundary__embedded_liberalism_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(nafta_jurisdictional_boundary__embedded_liberalism_reading, tangled_rope).
narrative_ontology:human_readable(nafta_jurisdictional_boundary__embedded_liberalism_reading, "NAFTA/USMCA Jurisdictional Boundary — Embedded Liberalism Reading").
narrative_ontology:topic_domain(nafta_jurisdictional_boundary__embedded_liberalism_reading, "International Trade Law / Political Economy / Regulatory Federalism").

domain_priors:requires_active_enforcement(nafta_jurisdictional_boundary__embedded_liberalism_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(nafta_jurisdictional_boundary__embedded_liberalism_reading, '8c2e690c-2b44-4ed4-a066-93466be9ae6e').
narrative_ontology:cs_kernel_codification('8c2e690c-2b44-4ed4-a066-93466be9ae6e', fixed_text).
narrative_ontology:cs_authority_grounding('8c2e690c-2b44-4ed4-a066-93466be9ae6e', practice).
narrative_ontology:cs_interpretation_layer_present('8c2e690c-2b44-4ed4-a066-93466be9ae6e').
narrative_ontology:cs_reading_relation('8c2e690c-2b44-4ed4-a066-93466be9ae6e', nafta_jurisdictional_boundary__capital_supremacy_reading, coexists_with).
narrative_ontology:cs_reading_relation('8c2e690c-2b44-4ed4-a066-93466be9ae6e', nafta_jurisdictional_boundary__sovereignty_primacy_reading, coexists_with).
narrative_ontology:cs_axiom('8c2e690c-2b44-4ed4-a066-93466be9ae6e', foundational, non_discrimination_as_sufficient_boundary_condition).
narrative_ontology:cs_axiom_status(non_discrimination_as_sufficient_boundary_condition, holdable).
narrative_ontology:cs_axiom_grounding('8c2e690c-2b44-4ed4-a066-93466be9ae6e', non_discrimination_as_sufficient_boundary_condition, conventional).
narrative_ontology:cs_axiom('8c2e690c-2b44-4ed4-a066-93466be9ae6e', foundational, regulatory_authority_and_market_access_are_jointly_realizable).
narrative_ontology:cs_axiom_status(regulatory_authority_and_market_access_are_jointly_realizable, holdable).
narrative_ontology:cs_axiom_grounding('8c2e690c-2b44-4ed4-a066-93466be9ae6e', regulatory_authority_and_market_access_are_jointly_realizable, instrumental).
narrative_ontology:cs_reference_frame('8c2e690c-2b44-4ed4-a066-93466be9ae6e', gatt_embedded_liberalism_compromise).
narrative_ontology:cs_drift_state('8c2e690c-2b44-4ed4-a066-93466be9ae6e', post_usmca_renegotiation_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('8c2e690c-2b44-4ed4-a066-93466be9ae6e', '').
narrative_ontology:cs_kernel_id(nafta_jurisdictional_boundary__embedded_liberalism_reading, nafta_jurisdictional_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(nafta_jurisdictional_boundary__embedded_liberalism_reading, export_oriented_manufacturers).
narrative_ontology:constraint_beneficiary(nafta_jurisdictional_boundary__embedded_liberalism_reading, regulatory_agencies_with_legitimate_objectives).
narrative_ontology:constraint_beneficiary(nafta_jurisdictional_boundary__embedded_liberalism_reading, consumers_of_traded_goods).
narrative_ontology:constraint_victim(nafta_jurisdictional_boundary__embedded_liberalism_reading, under_resourced_domestic_regulators).
narrative_ontology:constraint_victim(nafta_jurisdictional_boundary__embedded_liberalism_reading, communities_facing_regulatory_chill).
narrative_ontology:constraint_victim(nafta_jurisdictional_boundary__embedded_liberalism_reading, small_firms_facing_arbitration_costs).
narrative_ontology:constraint_vindicates(nafta_jurisdictional_boundary__embedded_liberalism_reading, non_discrimination_principle).
narrative_ontology:constraint_vindicates(nafta_jurisdictional_boundary__embedded_liberalism_reading, legitimate_policy_space_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Gain predictable, non-discriminatory market access across the treaty area and can plan supply chains around a rules-based framework rather than case-by-case political risk. Can relocate production or restructure supply chains if a domestic regulation genuinely raises costs, giving them meaningful exit relative to purely domestic firms.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__embedded_liberalism_reading, export_oriented_manufacturers, beneficiary,
    organized, generational, mobile, continental).

% Administer environmental, health, and labor rules and must document that any trade-affecting measure is non-discriminatory and pursues a legitimate objective. Retain real regulatory authority within that boundary, but must build a defensible record for every measure that could be challenged, which shapes what they are willing to attempt.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__embedded_liberalism_reading, regulatory_agencies_with_legitimate_objectives, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(nafta_jurisdictional_boundary__embedded_liberalism_reading, regulatory_agencies_with_legitimate_objectives, beneficiary).

% Sub-national or newly-capacitated agencies lack the legal and economic staff to build the record needed to defend a measure as non-discriminatory. They absorb the compliance and litigation-defense burden that the framework assumes agencies can bear, and often decline to legislate rather than risk a costly challenge.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__embedded_liberalism_reading, under_resourced_domestic_regulators, payer,
    moderate, biographical, trapped, national).

% Live near facilities or in labor markets where a protective standard was drafted, weakened, or withdrawn because officials anticipated an investor challenge or trade complaint. Bear the health, environmental, or labor cost of the standard that was not adopted, without ever appearing as a party to any dispute.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__embedded_liberalism_reading, communities_facing_regulatory_chill, payer,
    powerless, biographical, trapped, local).

% Domestic competitors of treaty-protected firms who cannot afford investor-state or trade-panel litigation themselves, and who must compete against firms whose regulatory environment has been shaped by the threat of such litigation, without symmetric access to the same legal remedies.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__embedded_liberalism_reading, small_firms_facing_arbitration_costs, payer,
    moderate, biographical, constrained, national).

% Benefit from lower prices and wider variety from tariff-free, rules-based trade, and from the baseline of non-discriminatory regulation that prevents the most naked forms of protectionist rent extraction.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__embedded_liberalism_reading, consumers_of_traded_goods, beneficiary,
    moderate, biographical, mobile, continental).

% Adjudicate whether a challenged domestic measure is genuinely non-discriminatory and pursues a legitimate objective, or is disguised protectionism/expropriation. Their interpretive choices define, case by case, how much real policy space the 'legitimate objectives' boundary actually protects.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__embedded_liberalism_reading, trade_dispute_panels, agenda_setter,
    institutional, generational, analytical, continental).

% Have no standing to bring or join most investor-state or state-to-state disputes and are not parties to the settlements that shape regulatory chill. They would argue the 'non-discriminatory' test is applied asymmetrically against protective measures and rarely against measures favoring capital mobility.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__embedded_liberalism_reading, environmental_and_labor_advocacy_groups, excluded,
    organized, generational, constrained, continental).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared, rules-based framework so that market access across the treaty area does not depend on ad hoc political favor, while explicitly preserving a defined zone in which governments may regulate for health, safety, labor, and environmental purposes without that regulation being treated as a trade violation.
% TRANSFER_FUNCTION: Moves predictability and market access to firms able to operate across the treaty area, and moves litigation-defense burden and, at the margin, foregone protective regulation onto under-resourced agencies and the communities those agencies would have protected.
% ABSENT_VOICES: Environmental and labor advocacy groups, and the communities affected by regulatory chill, are not parties to investor-state or state-to-state dispute proceedings and have no direct voice in how panels interpret 'legitimate objective' or 'non-discriminatory' in practice.
% DISAPPEARANCE_RATIONALE: Firms and agencies dispute what would happen if the jurisdictional boundary vanished: exporters and predictability-dependent industries would say cross-border trade becomes politically unstable and regulatory arbitrage/protectionism proliferate; advocacy groups and some domestic regulators would say the disappearance of litigation exposure would simply restore standard-setting authority to domestic democratic processes with no coordination loss, since bilateral market access could be renegotiated directly.
% FOUNDING_PROBLEM: Cross-border trade needed a stable framework so that market access was not subject to unilateral tariff retaliation or ad hoc protectionism, while governments needed assurance that joining such a framework would not strip them of the ability to regulate health, safety, labor, and the environment.
% FOUNDING_PROBLEM_CORROBORATION: Trade panels and treaty negotiators attest the legitimate-objectives boundary is functioning as designed, pointing to cases where challenged measures were upheld. Independent legal scholars, several sub-national regulators, and labor/environmental NGOs outside the negotiating governments attest that the boundary is narrower in practice than in text — that anticipated litigation cost, not panel outcomes, is what actually shapes regulatory behavior, and that this chilling effect is invisible to the formal dispute record.
narrative_ontology:disappearance_verdict(nafta_jurisdictional_boundary__embedded_liberalism_reading, contested).
narrative_ontology:founding_problem_status(nafta_jurisdictional_boundary__embedded_liberalism_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(nafta_jurisdictional_boundary__embedded_liberalism_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(nafta_jurisdictional_boundary__embedded_liberalism_reading, 'none', 1).
narrative_ontology:epsilon_provenance(nafta_jurisdictional_boundary__embedded_liberalism_reading, 0.46, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(nafta_jurisdictional_boundary__embedded_liberalism_reading_tests).
:- end_tests(nafta_jurisdictional_boundary__embedded_liberalism_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is moderate (0.46 at interval end) because the coordination function is genuine and substantial — the non-discrimination baseline does prevent much naked protectionism, and agencies do retain real regulatory authority within the legitimate-objectives boundary in most documented cases. It is not low because litigation-defense costs and regulatory chill are real, non-trivial, and asymmetrically distributed toward under-resourced regulators and affected communities. Suppression (0.40) reflects the credible threat of costly dispute proceedings, which shapes agency behavior even without a formal challenge being filed — an enforcement mechanism operating mostly through anticipation rather than realized coercion. Theater ratio (0.30) is moderate: much dispute-panel jurisprudence genuinely applies the legitimate-objectives test, but a growing share of activity is defensive documentation built to survive a challenge that may never come, which is partly performative relative to the regulation's substantive purpose.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter/beneficiary seat (regulatory agencies operating comfortably within the boundary, export manufacturers), the arrangement looks like a well-functioning framework that has successfully balanced access and policy space. From the payer seats (under-resourced regulators, chilled communities, small firms), the same structure looks like extraction operating through anticipation rather than adjudication — the chilling effect never appears in the dispute record precisely because it prevents the regulation from being written in the first place, so the formal record understates the payer-seat experience.
 *
 * DIRECTIONALITY LOGIC:
 *   Export-oriented manufacturers and consumers sit near the beneficiary end: mobile firms gain predictability and can adjust if a regulation raises costs; consumers gain from non-discriminatory trade generally. Regulatory agencies with legitimate objectives are structurally mixed — they retain real authority (beneficiary of the framework's design) but are constrained by the litigation-defense burden the framework imposes (payer of its operation), which is why they carry a secondary role. Under-resourced regulators, chilled communities, and small firms facing arbitration costs sit near the target end: they bear costs generated by the same structure without commensurate voice or compensation, and their exit options (trapped/constrained) prevent them from arbitraging around the cost the way mobile capital can.
 *
 * MANDATROPHY ANALYSIS:
 *   The embedded-liberalism reading is precisely the reading that resists collapsing this constraint into either 'pure extraction dressed as trade law' or 'pure coordination with no cost.' Classifying it tangled_rope rather than rope or snare preserves the fact that both readings capture something real: the non-discrimination baseline and legitimate-objectives carve-out are functioning coordination devices in a large share of cases (this is not mandatrophy — the founding problem of stable, non-arbitrary market access remains live), while the litigation-cost asymmetry and anticipatory regulatory chill are a real, uncompensated transfer riding on that same coordination structure. Treating the whole arrangement as a mountain (settled, natural, non-negotiable) would erase the contest between readings; treating it as a pure snare would erase the genuine market-access coordination that even critics of the current balance rely on when they propose reform rather than exit.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    legitimate_objectives_boundary_stability,
    'Does the ''legitimate objectives'' / non-discrimination boundary reliably protect bona fide environmental and labor regulation in practice, or does panel jurisprudence trend toward narrowing it over time in favor of market-access claims?',
    'Longitudinal coding of dispute-panel and investor-state tribunal outcomes: track the win rate of challenged environmental/labor/health measures over the treaty''s life, and whether panels have progressively raised or lowered the evidentiary bar for ''legitimate objective'' and ''non-discriminatory'' findings.',
    'A stable or widening boundary supports classifying this reading as a genuinely durable tangled_rope with real coordination function intact; a systematically narrowing boundary would support reclassifying this reading''s trajectory toward the capital_supremacy_reading''s structure over time, i.e., convergence of readings in practice even if the text is unchanged.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legitimate_objectives_boundary_stability, empirical, 'Whether the legitimate-objectives boundary is stable, widening, or narrowing across the treaty''s dispute-resolution history.').

omega_variable(
    chilling_effect_measurability,
    'How much regulation is actually deterred or weakened by anticipated litigation exposure, given that a deterred regulation by definition never appears in any formal record?',
    'Comparative case studies of jurisdictions with and without treaty-based investor-state exposure adopting similar proposed regulations; interviews with agency drafters about legal review processes that cite litigation risk; freedom-of-information requests for internal agency risk memoranda.',
    'If the chilling effect is large and systematic, the extractiveness score understates the true transfer, since most of the cost never surfaces in litigated cases; if small, the tangled_rope classification''s victim-side weighting should be reduced toward rope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(chilling_effect_measurability, empirical, 'Whether anticipatory regulatory chill is a large, systematic, but empirically invisible component of the constraint''s extraction.').

omega_variable(
    kernel_reading_convergence_or_divergence,
    'Is the embedded-liberalism reading a stable, independent structural fact about how the treaty operates, or is it a transitional compromise that is drifting toward the capital-supremacy reading as dispute-panel jurisprudence accumulates precedent favoring market-access claims?',
    'Track whether new treaty renegotiations (e.g., USMCA relative to NAFTA) narrow, preserve, or widen the explicit textual carve-outs for labor and environmental regulation, and whether this reading''s own axioms remain ''holdable'' or become ''overridden'' within trade-law practice over subsequent treaty cycles.',
    'If drift toward capital-supremacy is confirmed, this reading''s classification would need re-evaluation at a later interval; if the boundary is durably reinforced (e.g., by explicit labor-standard mechanisms added in USMCA), this reading is strengthened relative to its siblings.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_convergence_or_divergence, conceptual, 'Whether this reading is a stable equilibrium or a way-station in a longer drift toward the capital-supremacy reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(nafta_jurisdictional_boundary__embedded_liberalism_reading, 1994, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(naft_tr_t1994, nafta_jurisdictional_boundary__embedded_liberalism_reading, theater_ratio, 1994, 0.15).
narrative_ontology:measurement(naft_tr_t2000, nafta_jurisdictional_boundary__embedded_liberalism_reading, theater_ratio, 2000, 0.18).
narrative_ontology:measurement(naft_tr_t2006, nafta_jurisdictional_boundary__embedded_liberalism_reading, theater_ratio, 2006, 0.22).
narrative_ontology:measurement(naft_tr_t2012, nafta_jurisdictional_boundary__embedded_liberalism_reading, theater_ratio, 2012, 0.25).
narrative_ontology:measurement(naft_tr_t2018, nafta_jurisdictional_boundary__embedded_liberalism_reading, theater_ratio, 2018, 0.28).
narrative_ontology:measurement(naft_tr_t2020, nafta_jurisdictional_boundary__embedded_liberalism_reading, theater_ratio, 2020, 0.29).
narrative_ontology:measurement(naft_tr_t2026, nafta_jurisdictional_boundary__embedded_liberalism_reading, theater_ratio, 2026, 0.3).

% Extraction over time
narrative_ontology:measurement(naft_be_t1994, nafta_jurisdictional_boundary__embedded_liberalism_reading, base_extractiveness, 1994, 0.3).
narrative_ontology:measurement(naft_be_t2000, nafta_jurisdictional_boundary__embedded_liberalism_reading, base_extractiveness, 2000, 0.36).
narrative_ontology:measurement(naft_be_t2006, nafta_jurisdictional_boundary__embedded_liberalism_reading, base_extractiveness, 2006, 0.4).
narrative_ontology:measurement(naft_be_t2012, nafta_jurisdictional_boundary__embedded_liberalism_reading, base_extractiveness, 2012, 0.42).
narrative_ontology:measurement(naft_be_t2018, nafta_jurisdictional_boundary__embedded_liberalism_reading, base_extractiveness, 2018, 0.44).
narrative_ontology:measurement(naft_be_t2020, nafta_jurisdictional_boundary__embedded_liberalism_reading, base_extractiveness, 2020, 0.45).
narrative_ontology:measurement(naft_be_t2026, nafta_jurisdictional_boundary__embedded_liberalism_reading, base_extractiveness, 2026, 0.46).

% Suppression requirement over time
narrative_ontology:measurement(naft_su_t1994, nafta_jurisdictional_boundary__embedded_liberalism_reading, suppression_requirement, 1994, 0.25).
narrative_ontology:measurement(naft_su_t2000, nafta_jurisdictional_boundary__embedded_liberalism_reading, suppression_requirement, 2000, 0.3).
narrative_ontology:measurement(naft_su_t2006, nafta_jurisdictional_boundary__embedded_liberalism_reading, suppression_requirement, 2006, 0.34).
narrative_ontology:measurement(naft_su_t2012, nafta_jurisdictional_boundary__embedded_liberalism_reading, suppression_requirement, 2012, 0.37).
narrative_ontology:measurement(naft_su_t2018, nafta_jurisdictional_boundary__embedded_liberalism_reading, suppression_requirement, 2018, 0.39).
narrative_ontology:measurement(naft_su_t2020, nafta_jurisdictional_boundary__embedded_liberalism_reading, suppression_requirement, 2020, 0.395).
narrative_ontology:measurement(naft_su_t2026, nafta_jurisdictional_boundary__embedded_liberalism_reading, suppression_requirement, 2026, 0.4).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(nafta_jurisdictional_boundary__embedded_liberalism_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(nafta_jurisdictional_boundary__embedded_liberalism_reading, nafta_jurisdictional_boundary__capital_supremacy_reading).
narrative_ontology:affects_constraint(nafta_jurisdictional_boundary__embedded_liberalism_reading, nafta_jurisdictional_boundary__sovereignty_primacy_reading).

% DUAL FORMULATION NOTE:
% This story is one of three readings of the nafta_jurisdictional_boundary kernel. capital_supremacy_reading treats the treaty text as supreme law that subordinates domestic regulatory standards to mandatory capital-mobility and harmonization obligations (expected higher ε, thinner policy-space boundary). sovereignty_primacy_reading treats the treaty as a coordination mechanism fully subordinate to domestic sovereign law (expected lower ε, no meaningful override of domestic labor/environmental/health authority). This file (embedded_liberalism_reading) sits structurally between them: partial jurisdictional overlap, a bounded but real legitimate-objectives defense, and moderate extraction concentrated in litigation costs and anticipatory regulatory chill rather than direct override. All three are linked via affects_constraints since litigation and negotiation activity under one reading shifts the balance of legal and political resources available to advocates of the other readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
