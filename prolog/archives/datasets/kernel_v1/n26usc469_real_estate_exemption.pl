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
 *   human_readable: IRC §469 Real Estate Professional Exemption (Passive Activity Loss Rules)
 *   domain: economic/legal
 *
 * SUMMARY:
 *   The real estate professional exemption to the passive activity loss rules
 *   creates a binary classification system with profound economic
 *   consequences: real estate professionals meeting the material
 *   participation test can fully deduct passive real estate losses against
 *   active W-2 income, while passive investors and non-professional wage
 *   earners cannot. This constraint exhibits the full range of Deferential
 *   Realism types from different structural positions. For wage earners with
 *   inherited real estate, the rule operates as pure extraction (snare). For
 *   established real estate firms, it functions as a coordination mechanism
 *   enabling efficient property management (rope). For tax planning
 *   professionals, it creates demand for expertise to navigate increasingly
 *   complex statutory and regulatory pathways (tangled rope). For the
 *   statutory definition itself, material participation has become a degraded
 *   ritual — functionally incapable of distinguishing active management from
 *   passive investment, maintained through institutional inertia (piton). The
 *   constraint's theater ratio (0.65) reflects that compliance with the
 *   750-hour material participation test involves substantial documentation
 *   and audit defense costs that do not correlate well with actual property
 *   management. The rising extractiveness trajectory (0.35 → 0.58 over 40
 *   years) shows that the tax shelter design community has successfully
 *   expanded the exemption's scope through grouping elections and real
 *   property trade/business elections, increasing extraction from legitimate
 *   marginal operators.
 *
 * KEY AGENTS:
 *   - Passive Wage Earners: Primary victim (powerless/trapped) — W-2 employees with inherited or modest rental properties cannot access exemption; lose passive loss deductions indefinitely
 *   - Marginal Real Estate Operators: Secondary victim (moderate/constrained) — genuine property managers struggle with 750-hour test; face documentation burden and audit vulnerability
 *   - Established Real Estate Firms: Primary beneficiary (institutional/arbitrage) — large operators easily satisfy material participation; exemption enables efficient scaling
 *   - Tax Planning Professionals: Secondary beneficiary (organized/mobile) — CPAs, tax attorneys, real estate advisors benefit from complexity; maintain interpretive authority over statutory ambiguities
 *   - IRS Compliance Function: Institutional observer (institutional/analytical) — responsible for enforcing test; faces increasing compliance burden and audit selection difficulty as planning strategies proliferate
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing the binary active/passive distinction as inherent tax law rather than contingent statutory creation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(n26usc469_real_estate_exemption, 0.58).
domain_priors:suppression_score(n26usc469_real_estate_exemption, 0.68).
domain_priors:theater_ratio(n26usc469_real_estate_exemption, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(n26usc469_real_estate_exemption, extractiveness, 0.58).
narrative_ontology:constraint_metric(n26usc469_real_estate_exemption, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(n26usc469_real_estate_exemption, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(n26usc469_real_estate_exemption, tangled_rope).
narrative_ontology:human_readable(n26usc469_real_estate_exemption, "IRC §469 Real Estate Professional Exemption (Passive Activity Loss Rules)").
narrative_ontology:topic_domain(n26usc469_real_estate_exemption, "economic/legal").

domain_priors:requires_active_enforcement(n26usc469_real_estate_exemption).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(n26usc469_real_estate_exemption, real_estate_professionals).
narrative_ontology:constraint_beneficiary(n26usc469_real_estate_exemption, tax_shelter_designers).
narrative_ontology:constraint_victim(n26usc469_real_estate_exemption, passive_investors).
narrative_ontology:constraint_victim(n26usc469_real_estate_exemption, wage_earners).
narrative_ontology:constraint_victim(n26usc469_real_estate_exemption, tax_base_integrity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PASSIVE WAGE EARNER (SNARE) — W-2 employee with inherited or modest real estate holdings cannot access the professional exemption. Trapped by income level and occupational classification. Bears full extraction: passive losses provide no offset against salary income while real estate professionals with identical properties enjoy full deductions. No exit pathway without abandoning employment or selling appreciated assets at loss.
constraint_indexing:constraint_classification(n26usc469_real_estate_exemption, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: MARGINAL REAL ESTATE OPERATOR (TANGLED ROPE) — Genuinely participates in real estate management but struggles to meet the '750+ hours' material participation test. Constrained by documentation burden and IRS audit risk. Coordination benefit: the exemption enables legitimate real estate management that would otherwise be uneconomical. But asymmetric extraction: borderline operators face high compliance costs and selective audit vulnerability while large operators easily clear the threshold.
constraint_indexing:constraint_classification(n26usc469_real_estate_exemption, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: ESTABLISHED REAL ESTATE FIRM (ROPE) — Large institutional operators easily satisfy material participation tests. The exemption functions as pure coordination: enables efficient real estate management, reduces documentation burden relative to audit risk, scales with firm size. Net beneficiary but also provides genuine coordination service — the 750-hour test is approximately correct for distinguishing active management from passive investment.
constraint_indexing:constraint_classification(n26usc469_real_estate_exemption, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: TAX PLANNING INDUSTRY (TANGLED ROPE) — Organized professional class (CPAs, tax attorneys, real estate consultants) coordinated through professional standards and licensing. Benefits from exemption's complexity: opacity creates demand for expert advice. Coordinating function: translating statutory language into compliance pathways. Extraction: the industry's growth has made the material participation test increasingly ambiguous through 'grouping elections' (Treasury Reg. 1.469-4(d)) and real property trade/business elections (TCJA §469(c)(7)(C)), creating audit risk and documentation burden that benefits advisors. The exemption has become a terrain where organized professionals extract surplus from uncertainty they partly maintain.
constraint_indexing:constraint_classification(n26usc469_real_estate_exemption, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: STATUTORY DEFINITION (PITON) — The material participation test (750 hours / 100+ hours standard / spouse aggregation rules / grouping elections) is functionally degraded. The test was designed in 1986 to distinguish active management from passive investment. But 40 years of case law, Treasury regulations, and congressional modifications have created a complex simulacrum that neither prevents tax shelters nor reliably identifies genuine active managers. Most real estate professionals who need the exemption (small to mid-market operators) spend significant resources on compliance and audit defense. Large operators and tax shelter designers easily navigate around the test. The definition persists through institutional inertia: Congress has not substantially reformed §469 despite near-universal agreement that the complexity exceeds any policy benefit.
constraint_indexing:constraint_classification(n26usc469_real_estate_exemption, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a universal analytical perspective, some binary classification of economic actors is inherent to any tax system: the code must distinguish categories to apply different rules. The need to separate active business owners from passive investors is a structural feature of income tax design, not a contingent policy choice. The constraint is immutable law of tax administration. However, this perspective naturalizes what is actually a constructed institutional arrangement: the binary classification itself (active/passive), the specific hour threshold (750), the grouping elections, the real property trade/business carve-out, and the enforcement gap are all statutory creations that could be redesigned or eliminated.
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
 *   Extractiveness (0.58): Moderate-high. The exemption creates significant wealth transfer from passive investors and wage earners to real estate professionals. However, extractiveness is not as severe as a pure snare (>0.66) because the exemption serves a genuine coordination function for legitimate active managers — the material participation test, while degraded, is approximately correct in identifying operators who genuinely manage properties. The 0.58 value reflects that the exemption benefits both legitimate operators (coordination) and tax shelter designers (extraction) from the same statutory rule. Rising trajectory (0.35 → 0.58) shows that tax planning industry expansion has gradually captured more value through grouping elections and regulatory interpretation. Suppression (0.68): High. Multiple barriers prevent passive investors from accessing the exemption: (1) income level requirements (the exemption was originally limited to individuals with <$100k gross income and <2% entity ownership, later modified); (2) occupational classification — becoming a 'real estate professional' requires material participation in multiple properties or a spouse's material participation; (3) documentation burden — the 750-hour requirement necessitates contemporaneous records; (4) audit vulnerability — IRS audits material participation claims at elevated rates. Passive wage earners face insurmountable barriers; marginal operators face high compliance costs. Theater ratio (0.65): Moderate-high. The 750-hour material participation test is substantially performative. First, the hour threshold does not correlate well with actual property management quality or economic activity. Second, compliance involves maintaining detailed time records that do not correspond to actual workflow — property managers must artificially segregate 'material participation' hours from other business activities. Third, Treasury Regulation 1.469-5T(f) creates a 'safe harbor' for rental real estate professionals meeting an annual 100-hour threshold, which undermines the 750-hour test's operational meaning. Fourth, the 'grouping election' (Reg. 1.469-4(d)) allows real estate professionals to treat multiple properties as a single activity for purposes of the material participation test, creating ambiguity in what constitutes material participation 'in' a real property trade or business. The rituals of documentation and audit risk management consume significant resources without proportional functional verification.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates the full range of classification types from the same structural data. The wage earner classification (snare) vs. the established firm classification (rope) from the same statutory rule shows that indexical classification is not a property of the rule itself but a property of the structural position from which the rule is observed. The analytical observer's mountain view (classification is necessary) vs. the piton view (classification is degraded theater) exposes the classical oracle gap: the observer who sees the constraint as immutable law cannot see the structural choices that constitute that law.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality computation proceeds from beneficiary/victim declarations: Real estate professionals and tax planning professionals are declared beneficiaries (they capture economic surplus from the exemption's complexity and scope); passive investors, wage earners, and tax base integrity are declared victims (they bear the extraction cost). For each agent, d is derived from power level + exit options: Passive wage earners (powerless + trapped) → d ≈ 0.95 → f(d) ≈ 1.42 → high experienced extractiveness. Established firms (institutional + arbitrage) → d ≈ 0.10 → f(d) ≈ -0.01 → negative experienced extractiveness (beneficiary). Marginal operators (moderate + constrained) → d ≈ 0.65 → f(d) ≈ 1.00 → moderate experienced extractiveness. The scope modifier σ(national) = 1.0 does not dampen or amplify. The resulting perspectival gap shows that the constraint's experience is not uniform: beneficiaries perceive coordination (rope), victims perceive extraction (snare).
 *
 * MANDATROPHY ANALYSIS:
 *   The §469 real estate exemption does NOT fall into the mandatrophy trap because the tangled rope classification correctly identifies the constraint's hybrid character: it provides genuine coordination (legitimate real estate management) with asymmetric extraction (tax planning industry surplus, wage earner disadvantage). The risk of mislabeling would be: (1) classifying as rope only (missing the extraction layer), which would falsely suggest the exemption is net-beneficial coordination. (2) classifying as snare only (missing the coordination function), which would falsely suggest eliminating the exemption would improve efficiency — but many legitimate real estate businesses depend on the exemption to operate. The tangled rope classification preserves both functions: the exemption enables coordination, AND it extracts surplus from certain structural positions. The mandatrophy is resolved by accepting that both are true simultaneously, depending on the observer's position. The analytical observer must resist the temptation to see the constraint as either 'necessary tax administration' (mountain) or 'purely extractive planning strategy' (snare) — it is structurally both.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    material_participation_operational_definition,
    'Does the 750-hour threshold operationally distinguish active management from passive investment, or does it measure compliance effort and audit risk?',
    'Empirical analysis: comparison of real estate performance metrics (ROI, operational efficiency, tenant satisfaction) between §469(c)(7) certified vs. non-certified operators in comparable markets; audit rate analysis by hour threshold cohort; time-motion studies of actual material participation activities',
    'If threshold correlates with management quality: maintains rope/tangled-rope classification. If threshold measures only compliance burden: reclassify to snare/piton (the test is extractive theater, not functional screening).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(material_participation_operational_definition, empirical, 'Whether 750-hour test screens active managers or just compliance burden').

omega_variable(
    real_property_trade_business_scope_creep,
    'Has the ''real property trade or business'' election (IRC §469(c)(7)(C), added by TCJA) expanded eligibility beyond the 750-hour test''s original intent, creating a shadow exemption with lower documentation requirements?',
    'IRS data on §469(c)(7)(C) election frequency by entity type and income level; audit rate for §469(c)(7)(C) vs. §469(c)(7)(A) claims; case law on what constitutes ''real property trade or business''',
    'If election scope is wide and audit-resistant: §469 has bifurcated into a documented pathway (75O-hour) and an undocumented pathway (§469(c)(7)(C)), creating class-based extraction. If election is rarely successful: the provision is dead letter and the binary classification remains operant.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(real_property_trade_business_scope_creep, empirical, 'Whether §469(c)(7)(C) creates a shadow exemption pathway').

omega_variable(
    tax_shelter_versus_legitimate_management_distinction,
    'Can the IRS operationally distinguish material participation claims by genuine operators (who depend on the exemption to structure legitimate businesses) from material participation claims by high-income tax planners (who use the exemption to shelter non-real-estate income)?',
    'Audit outcome analysis: ratio of upheld vs. disallowed material participation claims by income level and entity type; correlation between material participation claim and stated real estate business income; longitudinal tracking of claimed hours vs. actual income generation',
    'If distinction is operationally feasible: targeted enforcement could preserve the exemption for legitimate operators while closing the tax shelter pathway, moving the constraint toward rope. If distinction is not feasible: the constraint must choose between over-broad exemption (snare for passive investors) or over-broad restriction (snare for genuine operators).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(tax_shelter_versus_legitimate_management_distinction, empirical, 'Whether IRS can distinguish shelter planning from legitimate management').

omega_variable(
    spousal_aggregation_equity,
    'Does spousal hour aggregation (allowing married couples to combine material participation hours across separate properties) create gender-based or family-structure-based extraction?',
    'Demographic analysis of §469(c)(7) exemption claims by marital status, gender composition, and property management labor division; audit vulnerability by family structure; time allocation studies in dual-career and single-career households',
    'If aggregation enables extractive structuring based on spousal income splitting: reclassify from tangled_rope to snare for disadvantaged family structures. If aggregation is operationally neutral: maintains current classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(spousal_aggregation_equity, empirical, 'Whether spousal aggregation creates family-structure-based extraction').

omega_variable(
    passive_loss_carryforward_contingency,
    'Do passive activity losses carried forward indefinitely and deductible only upon death or disposition of the entire activity constitute a genuine tax deferral (coordination) or effective permanent disallowance for most taxpayers?',
    'Longitudinal tracking of passive loss carryforwards: fraction claimed as deductions within 10 years; fraction deductible upon death (stepped-up basis advantage) vs. never claimed; actuarial analysis of claim probability by age cohort and asset type',
    'If most carryforwards are eventually claimed: passive activity loss rules are deferral only (lower extraction). If most are permanently lost or absorbed by stepped-up basis: rules are effective disallowance (higher extraction), making snare classification more appropriate for trapped agents.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(passive_loss_carryforward_contingency, empirical, 'Whether passive loss carryforwards constitute deferral or permanent disallowance').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(n26usc469_real_estate_exemption, 1986, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(re469_tr_t0, n26usc469_real_estate_exemption, theater_ratio, 0, 0.45).
narrative_ontology:measurement(re469_tr_t15, n26usc469_real_estate_exemption, theater_ratio, 15, 0.58).
narrative_ontology:measurement(re469_tr_t30, n26usc469_real_estate_exemption, theater_ratio, 30, 0.65).

% Extraction over time
narrative_ontology:measurement(re469_be_t0, n26usc469_real_estate_exemption, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(re469_be_t15, n26usc469_real_estate_exemption, base_extractiveness, 15, 0.48).
narrative_ontology:measurement(re469_be_t30, n26usc469_real_estate_exemption, base_extractiveness, 30, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(re469_su_t0, n26usc469_real_estate_exemption, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(re469_su_t15, n26usc469_real_estate_exemption, suppression_requirement, 15, 0.65).
narrative_ontology:measurement(re469_su_t30, n26usc469_real_estate_exemption, suppression_requirement, 30, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(n26usc469_real_estate_exemption, resource_allocation).
narrative_ontology:affects_constraint(n26usc469_real_estate_exemption, passive_activity_loss_carryforward_death_benefit).
narrative_ontology:affects_constraint(n26usc469_real_estate_exemption, real_estate_depreciation_recapture).
narrative_ontology:affects_constraint(n26usc469_real_estate_exemption, tax_shelter_licensing_regulatory_arbitrage).

% DUAL FORMULATION NOTE:
% The §469 real estate exemption is a constraint family of three related stories: (1) the material participation test (this story), (2) the passive loss carryforward / death benefit (downstream extraction mechanism), (3) the interaction with depreciation recapture and basis step-up (structural feature that modifies the exemption's effective extraction). Each story has its own ε and its own perspectives. The material participation test operates at the gate; the carryforward mechanism determines the long-term extraction trajectory; the depreciation interaction determines the post-death tax treatment. Decomposing into separate stories reveals that the exemption's extractive power is distributed across multiple mechanisms rather than concentrated in any single statutory language.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(n26usc469_real_estate_exemption, moderate, 0.68).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
