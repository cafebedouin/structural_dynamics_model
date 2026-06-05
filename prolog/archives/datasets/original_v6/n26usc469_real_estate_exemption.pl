% ============================================================================
% CONSTRAINT STORY: n26usc469_real_estate_exemption
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_n26usc469_real_estate_exemption, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: n26usc469_real_estate_exemption
 *   human_readable: The Real Estate Professional Exemption (Passive Activity Loss Rules)
 *   domain: economic/legal
 *
 * SUMMARY:
 *   Section 469 of the US tax code restricts deductions for passive activity
 *   losses, preventing most taxpayers from offsetting passive real estate
 *   losses against active W-2 income. However, real estate professionals
 *   meeting specific material participation tests can claim an exemption,
 *   creating a binary classification system with profound economic
 *   consequences. This constraint exhibits all six DR types depending on the
 *   observer's structural position and exit options. The same statutory rule
 *   appears as an immutable principle of tax administration (mountain), a
 *   coordination mechanism clarifying professional status (rope), a mixed
 *   hybrid with coordination and extraction (tangled rope), a temporary
 *   workaround being engineered around (scaffold), a degraded audit ritual
 *   (piton), or pure extraction (snare), depending on whether the observer
 *   can access the professional exemption and at what cost. The constraint's
 *   theater ratio (0.58) reflects that the material participation tests rely
 *   heavily on documentation and time-log compliance that may not correlate
 *   with genuine economic involvement. The extractiveness value (0.52)
 *   captures that the exemption creates a meaningful bifurcation: real estate
 *   professionals can shelter income through passive loss deductions while
 *   W-2 earners with identical economic exposure cannot.
 *
 * KEY AGENTS:
 *   - W-2 Earners with Rental Property: Primary victim (powerless/trapped) — subject to passive loss disallowance; cannot restructure employment to claim professional status without career disruption
 *   - Real Estate Professionals (Material Participants): Primary beneficiary (institutional/arbitrage) — can claim exemption and offset passive losses; have exit options through entity restructuring and professional status documentation
 *   - Limited Partners in Real Estate Funds: Secondary victim (moderate/constrained) — confined to fund structures with restricted exit options; benefit from professional management but bear passive loss disallowance
 *   - Real Estate Industry Coalition (NAR, NAREIT): Organized beneficiary (organized/constrained) — benefits from exemption framework that concentrates capital in professional hands; faces legislative closure risk
 *   - IRS Passive Activity Loss Audit System: Institutional enforcer (institutional/arbitrage) — maintains the compliance apparatus through audits and documentation review; has shifted to proportional rather than comprehensive verification
 *   - Tax Advisory Industry: Powerful architect (powerful/mobile) — engineers restructurings and workarounds; sees constraint as entrepreneurial opportunity; has high exit and arbitrage options
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing the statutory exemption as a law of taxation rather than a specific legislative choice
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(n26usc469_real_estate_exemption, 0.52).
domain_priors:suppression_score(n26usc469_real_estate_exemption, 0.65).
domain_priors:theater_ratio(n26usc469_real_estate_exemption, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(n26usc469_real_estate_exemption, extractiveness, 0.52).
narrative_ontology:constraint_metric(n26usc469_real_estate_exemption, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(n26usc469_real_estate_exemption, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(n26usc469_real_estate_exemption, tangled_rope).
narrative_ontology:human_readable(n26usc469_real_estate_exemption, "The Real Estate Professional Exemption (Passive Activity Loss Rules)").
narrative_ontology:topic_domain(n26usc469_real_estate_exemption, "economic/legal").

domain_priors:requires_active_enforcement(n26usc469_real_estate_exemption).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(n26usc469_real_estate_exemption, real_estate_professionals).
narrative_ontology:constraint_beneficiary(n26usc469_real_estate_exemption, high_income_rental_operators).
narrative_ontology:constraint_victim(n26usc469_real_estate_exemption, passive_real_estate_investors).
narrative_ontology:constraint_victim(n26usc469_real_estate_exemption, limited_partners_in_real_estate_funds).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PASSIVE INVESTOR (SNARE) — W-2 earners with real estate holdings cannot access the professional exemption without radically restructuring their work lives. Trapped by the binary: either maintain W-2 employment (and accept passive loss disallowance) or abandon career to claim real estate professional status. The constraint extracts wealth by preventing offset of material tax liability against earned income. Maximum suppression: no alternative pathway for ordinary salaried workers who own rental property.
constraint_indexing:constraint_classification(n26usc469_real_estate_exemption, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: LIMITED PARTNER IN REAL ESTATE FUND (TANGLED ROPE) — Constrained exit (cannot easily liquidate fund position; regulatory barriers to early withdrawal). Benefits from the fund structure and professional management but bears the burden of passive loss disallowance. Coordination function: funds need passive capital. Extraction: fund structures create economic outcomes where passive losses subsidize fund manager fees. Mixed structure with enforcement through the tax code.
constraint_indexing:constraint_classification(n26usc469_real_estate_exemption, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: REAL ESTATE PROFESSIONAL OPERATOR (ROPE) — Institutional actor with arbitrage options: can restructure business entity, shift to S-corp, or employ consulting arrangements to maintain professional status. Experiences the constraint as pure coordination: the 469 rules define what qualifies as professional participation, enabling operators to signal legitimate material participation to tax authorities. Low effective extraction — this agent has exit options and benefits from clarity on professional status.
constraint_indexing:constraint_classification(n26usc469_real_estate_exemption, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: REAL ESTATE INDUSTRY COALITION (TANGLED ROPE) — Organized agents (NAR, NAREIT, professional investor associations) benefit from the exemption framework (it encourages professional consolidation and capital pooling) but remain constrained by IRS enforcement and legislative change risk. Active enforcement required to maintain exemption boundaries; suppression via complexity (material participation tests, 750-hour rules). Coalition experiences both coordination (defining professional standards) and asymmetric extraction (from passive investors who subsidize professional structures).
constraint_indexing:constraint_classification(n26usc469_real_estate_exemption, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: IRS PAL AUDIT APPARATUS (PITON) — The mechanical audit process for passive activity loss claims is heavily theatrical: agents examine 750-hour logs, material participation documentation, and entity structure but cannot verify the actual economic substance of claimed professional activity. Theater ratio high because auditors have limited ability to determine genuine material participation vs. documentation theater. The audit mechanism persists through institutional inertia despite declining functional verification capacity. IRS has shifted toward proportional auditing (targeting high-value claims) rather than attempting comprehensive verification.
constraint_indexing:constraint_classification(n26usc469_real_estate_exemption, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: TAX-MOTIVATED RESTRUCTURING INDUSTRY (SCAFFOLD) — Tax advisors, CPAs, and law firms engineer around the constraint through entity restructuring, qualified business income (QBI) deductions, cost segregation studies, and opportunity zone arbitrage. These pathways create temporary workarounds with sunset risk (legislative closure). The industry sees the constraint as a coordination problem (defining professional standards) with a built-in sunset (tax law changes). Suppression is moderate because expertise is concentrated and expensive, limiting access for unsophisticated taxpayers.
constraint_indexing:constraint_classification(n26usc469_real_estate_exemption, scaffold,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, the constraint appears to reflect an immutable feature of tax administration: the need to match income type (active vs passive) to deduction source. The binary is inherent to progressive taxation of mixed-source income. However, the structural data contradicts this: the exemption is a statutory carve-out (26 USC 469(c)(7)), not a law of nature. The mountain reading is a false summit — it naturalizes a specific legislative choice as inevitable.
constraint_indexing:constraint_classification(n26usc469_real_estate_exemption, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(n26usc469_real_estate_exemption_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(n26usc469_real_estate_exemption, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(n26usc469_real_estate_exemption, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(n26usc469_real_estate_exemption, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(n26usc469_real_estate_exemption, TR),
    TR >= 0.70.

:- end_tests(n26usc469_real_estate_exemption_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The exemption creates measurable economic bifurcation. W-2 earners with real estate holdings face permanent loss disallowance (measured at ~$3.5B annually in suspended passive losses). Real estate professionals can offset losses. The extraction is not total (passive loss carryforwards survive indefinitely, enabling eventual utilization; sophisticated investors can restructure to claim professional status) but is substantial and materially increases tax liability for constrained taxpayers. The value reflects that extraction occurs through statutory mechanism but can be partially avoided through professional status restructuring. Suppression (0.65): Moderate-high. Significant barriers to accessing the exemption include: (1) the binary test (either material participation or no exemption; no intermediate status), (2) documentation burden (750-hour logs, contemporaneous records), (3) restructuring costs for unsophisticated investors, and (4) IRS enforcement preventing abuse. Suppression increased from 0.55 to 0.65 over the interval due to greater IRS scrutiny post-2017 Tax Cuts and Jobs Act. Theater ratio (0.58): Moderate. Material participation tests rely on documented hours and participation records that may not correlate with actual economic substance. IRS auditors verify documentation rather than economic reality, creating theater. However, theater is not dominant — the 750-hour rule produces measurable economic sorting (most legitimate professionals can meet it; most passive investors cannot), suggesting some functional content. Theater increased over the interval as restructuring strategies proliferated, requiring more documentation to claim exemption.
 *
 * PERSPECTIVAL GAP:
 *   The constraint exhibits maximum perspectival variance across all six types. The W-2 earner sees a permanent extraction mechanism (Snare) — passive losses suspend indefinitely, and restructuring to professional status requires career disruption they cannot afford. The real estate professional sees a coordination mechanism (Rope) — the 469 rules clarify what 'professional' means, enabling them to signal legitimate participation. The industry coalition sees mixed coordination and extraction (Tangled Rope) — the exemption concentrates capital while creating documentary obligations. The tax advisory industry sees a temporary problem being engineered around (Scaffold) — new strategies (QBI deductions, cost segregation, entity restructuring) create sunset pathways. The IRS sees its own audit apparatus as degraded (Piton) — the verification process is theater, yet persists because alternatives haven't fully replaced it. The civilizational observer risks seeing the binary as inherent to progressive taxation (Mountain) rather than as a specific statutory choice with political origins.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality derivation follows from structural relationship to the exemption. W-2 earners are victims (high d due to trapped exit) — they cannot restructure employment without career disruption, so exit_options remains trapped and d rises toward 1.0. Real estate professionals are beneficiaries (low d due to arbitrage exit) — they can restructure entity form, shift to S-corp, or employ professional documentation, so exit_options is arbitrage and d drops toward 0.0. Limited partners face constrained exit (fund restrictions prevent easy liquidation), placing them in middle position (d ≈ 0.55). The scaffold perspective (tax advisors) has mobile/powerful positioning with ready exit options (can advise around the rule) and beneficiary status (profit from restructurings), yielding low d. The piton perspective (IRS apparatus) is institutional/arbitrage with enforcement function — it sees its own verification as degraded but maintains the apparatus through inertia. The mountain perspective risks naturalizing a statutory carve-out as inevitable.
 *
 * MANDATROPHY ANALYSIS:
 *   POTENTIAL MISCLASSIFICATION: The constraint risks being mislabeled as Mountain (a natural feature of tax administration) when it is actually Tangled Rope (a statutory extraction mechanism with a coordination function). The mandatrophy resolution requires distinguishing between (1) the inherent tension in matching income type to deduction source (true structural problem) and (2) the specific statutory carve-out for real estate professionals (political choice that creates bifurcation). The former might justify a Mountain classification; the latter clearly does not. The presence of beneficiaries (real estate professionals), victims (W-2 earners), and active enforcement (IRS audits) confirms Tangled Rope rather than Mountain. The theater ratio (0.58) indicates the audit apparatus is partially performative but not dominantly theatrical. The exemption cannot be classified as Scaffold because there is no credible sunset clause — the real estate industry has successfully resisted closure attempts for decades, and the exemption shows no sign of legislated expiration. It cannot be Rope because the asymmetric extraction (passive losses disallowed for W-2 earners) is not incidental to coordination; it is central to how the constraint functions. The Tangled Rope classification is supported by the presence of both coordination (defining professional standards) and extraction (wealth bifurcation between professional and passive investors).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    material_participation_threshold,
    'What constitutes ''material participation'' in real estate operations? Is the 750-hour threshold an objective measure of genuine involvement or a compliance theater standard?',
    'Comparative analysis of claimed vs actual time allocation; correlation between audit-hour documentation and economic substance; audit adjustment rates by claimed participation level',
    'If 750-hour rule is genuine threshold: professional status is verifiable, snare classification is structural (powerless agents cannot meet threshold). If rule is theater: compliance can be manufactured through documentation, suggesting the constraint functions as extraction mechanism rather than coordination boundary.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(material_participation_threshold, empirical, 'Whether the 750-hour material participation test measures genuine involvement').

omega_variable(
    passive_loss_carryforward_utility,
    'Do passive losses suspended under 469(b) ever become usable, or do they function as permanent loss disallowance for ordinary investors?',
    'Longitudinal tracking of passive loss carryforwards: percentage that eventually offset income vs percentage that expire unused; correlation with taxpayer longevity and exit events (death, sale of property)',
    'If losses are regularly utilized: constraint is temporary friction (rope or scaffold). If losses typically expire unused: constraint is permanent wealth extraction (snare), especially for W-2 earners.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(passive_loss_carryforward_utility, empirical, 'Whether suspended passive losses become economically usable').

omega_variable(
    restructuring_cost_barrier,
    'Do the costs of entity restructuring, S-corp conversions, and professional status documentation create a practical barrier that functions as effective wealth transfer from unsophisticated to sophisticated investors?',
    'Cost analysis of restructuring: professional fees, entity setup, annual compliance burden; correlation between taxpayer sophistication and successful exemption claims',
    'If restructuring costs are prohibitive for most investors: exemption becomes a wealth-concentrating mechanism (Snare with secondary extraction from passive to professional). If costs are manageable: exemption is coordination mechanism (Rope).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(restructuring_cost_barrier, empirical, 'Whether restructuring costs create practical barriers to exemption access').

omega_variable(
    legislative_stability_of_exemption,
    'How stable is the real estate professional exemption in the legislative cycle? What is the probability it will be substantially modified or eliminated in the next 10 years?',
    'Legislative history analysis; frequency of modification attempts; current political support for preservation vs closure',
    'If exemption is politically durable: scaffold and rope perspectives are validated. If exemption faces repeated closure attempts: constraint is inherently unstable, suggesting the underlying extraction mechanism is contested.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legislative_stability_of_exemption, preference, 'Legislative durability of the real estate professional exemption').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(n26usc469_real_estate_exemption, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ral_tr_t0, n26usc469_real_estate_exemption, theater_ratio, 0, 0.42).
narrative_ontology:measurement(ral_tr_t15, n26usc469_real_estate_exemption, theater_ratio, 15, 0.52).
narrative_ontology:measurement(ral_tr_t30, n26usc469_real_estate_exemption, theater_ratio, 30, 0.58).

% Extraction over time
narrative_ontology:measurement(ral_be_t0, n26usc469_real_estate_exemption, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(ral_be_t15, n26usc469_real_estate_exemption, base_extractiveness, 15, 0.48).
narrative_ontology:measurement(ral_be_t30, n26usc469_real_estate_exemption, base_extractiveness, 30, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(n26usc469_real_estate_exemption, enforcement_mechanism).
narrative_ontology:affects_constraint(n26usc469_real_estate_exemption, qualified_business_income_deduction).
narrative_ontology:affects_constraint(n26usc469_real_estate_exemption, cost_segregation_depreciation_strategy).
narrative_ontology:affects_constraint(n26usc469_real_estate_exemption, real_estate_investment_trust_structure).

% DUAL FORMULATION NOTE:
% The real estate professional exemption is part of a constraint cluster including QBI deductions, cost segregation strategies, and REIT structures. These form a family of interconnected tax provisions that together create the modern real estate tax landscape. The exemption itself (ε=0.52) is upstream of QBI deductions and cost segregation (which use the professional status to amplify benefit). REIT structures operate in parallel with a different mechanism (pass-through entity status rather than individual professional classification). All are linked via network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(n26usc469_real_estate_exemption, organized, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
