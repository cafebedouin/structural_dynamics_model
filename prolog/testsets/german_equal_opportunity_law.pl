% ============================================================================
% CONSTRAINT STORY: german_equal_opportunity_law
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_german_equal_opportunity_law, []).

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
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: german_equal_opportunity_law
 *   human_readable: German Equal Opportunity Law (Allgemeines Gleichbehandlungsgesetz)
 *   domain: labor_law/social_policy/institutional_governance
 *
 * SUMMARY:
 *   The Allgemeines Gleichbehandlungsgesetz (AGG), enacted in 2006 to
 *   implement EU equal treatment directives, creates a structural constraint
 *   that coordinates labor market standards while extracting compliance
 *   costs. The law prohibits discrimination based on race, ethnicity, gender,
 *   religion, disability, age, and sexual orientation in employment. It
 *   exhibits tangled_rope characteristics: genuine coordination function
 *   (prevents wage competition via discrimination, establishes baseline
 *   worker protections) alongside asymmetric extraction (administrative
 *   burden on employers, verification barriers on workers, theater production
 *   in compliance infrastructure). The constraint demonstrates perspectival
 *   plurality because different institutional positions experience
 *   fundamentally different relationships to the same legal rule. An employer
 *   sees compliance infrastructure; a discriminated worker sees verification
 *   barriers; a trade union sees scaffolding toward stronger protections; a
 *   large corporation sees portable standards enabling talent markets; the EU
 *   compliance regime sees degraded function masked by theater.
 *
 * KEY AGENTS:
 *   - Discriminated Workers: Primary victim (powerless/trapped) — nominally protected but face high verification costs and retaliation risk
 *   - Small Employers (10-50 employees): Secondary victim (moderate/constrained) — bear disproportionate compliance burden relative to firm size; medium employers face full compliance costs
 *   - Large Corporations: Moderate beneficiary (powerful/mobile) — experience compliance as standardization enabling talent recruitment; have exit option via relocation
 *   - Federal Employment Agency (Bundesagentur für Arbeit): Primary beneficiary/enforcer (institutional/arbitrage) — law provides mandate and enforcement authority
 *   - Trade Unions (IG Metall, Verdi, others): Organized agents (organized/constrained) — use law as scaffolding for broader labor protections; mobilize worker litigation
 *   - EU Compliance Regime: Institutional actor (institutional/arbitrage) — maintains law for regulatory alignment despite degraded enforcement function
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing institutional enforcement limits as immutable sociological facts
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(german_equal_opportunity_law, 0.38).
domain_priors:suppression_score(german_equal_opportunity_law, 0.42).
domain_priors:theater_ratio(german_equal_opportunity_law, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(german_equal_opportunity_law, extractiveness, 0.38).
narrative_ontology:constraint_metric(german_equal_opportunity_law, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(german_equal_opportunity_law, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(german_equal_opportunity_law, tangled_rope).
narrative_ontology:human_readable(german_equal_opportunity_law, "German Equal Opportunity Law (Allgemeines Gleichbehandlungsgesetz)").
narrative_ontology:topic_domain(german_equal_opportunity_law, "labor_law/social_policy/institutional_governance").

domain_priors:requires_active_enforcement(german_equal_opportunity_law).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(german_equal_opportunity_law, marginalized_workers).
narrative_ontology:constraint_beneficiary(german_equal_opportunity_law, civil_rights_advocates).
narrative_ontology:constraint_beneficiary(german_equal_opportunity_law, legal_enforcement_institutions).
narrative_ontology:constraint_victim(german_equal_opportunity_law, employer_compliance_costs).
narrative_ontology:constraint_victim(german_equal_opportunity_law, employment_market_flexibility).
narrative_ontology:constraint_victim(german_equal_opportunity_law, small_business_administrative_burden).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DISCRIMINATED WORKER (SNARE) — Trapped within German labor market without credible exit. Despite legal protection, verification barriers are high: proving discrimination requires documenting employer intent, withstanding career retaliation risk, and navigating lengthy judicial process. The constraint (the law) nominally protects but structures verification such that enforcement remains difficult. Theater ratio reflects that many discrimination complaints are formally processed but result in inadequate remedies. The trapped worker bears the psychological and material cost of harassment while seeking justice.
constraint_indexing:constraint_classification(german_equal_opportunity_law, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: SMALL EMPLOYER (TANGLED ROPE) — Constrained by compliance costs (legal training, documentation systems, HR infrastructure) but also benefits from reduced litigation risk and broader talent pool access. The law coordinates labor market standards (preventing wage competition via discrimination) while extracting administrative overhead and legal liability exposure. Moderate power with constrained exit — compliance is mandatory, but partial exemptions exist for very small firms. Mixed extraction and coordination function.
constraint_indexing:constraint_classification(german_equal_opportunity_law, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: ANTI-DISCRIMINATION ENFORCEMENT (ROPE) — The Bundesagentur für Arbeit (Federal Employment Agency) and regional labor courts experience the law as pure coordination: it provides legal basis, funding, and mandate. Arbitrage-capable — they can adjust enforcement intensity, interpret ambiguous clauses, and prioritize cases. The constraint enables their institutional function. Experiences the law as coordination mechanism rather than extraction.
constraint_indexing:constraint_classification(german_equal_opportunity_law, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: LARGE CORPORATION (TANGLED ROPE) — Mobile exit option (can relocate to other EU jurisdictions or restructure workforce) but also genuinely benefits from standardized non-discrimination norms (reduces internal conflict, enables broad talent recruitment, aligns with corporate ESG positioning). High extractiveness experienced but genuine coordination function exists. The law prevents internal discrimination-based competition among divisions while requiring investment in compliance infrastructure. Powerful actor with exit option but sufficient benefit to remain engaged.
constraint_indexing:constraint_classification(german_equal_opportunity_law, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: TRADE UNION COALITION (SCAFFOLD) — Organized agents (IG Metall, Verdi, etc.) view the law as a transitional support structure toward broader labor power consolidation. Low effective extraction because unions have agency, can contest enforcement, and see the legal framework as scaffolding toward stronger worker protections. Suppression is structured by union membership dues and internal discipline, not by the law itself. Time horizon spans generational — unions see the law as temporary stepping stone to stronger sectoral bargaining or sectoral minimum wages.
constraint_indexing:constraint_classification(german_equal_opportunity_law, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: EU COMPLIANCE THEATER (PITON) — The German law's original function (implementing EU Directive 2000/78/EC on equal treatment in employment) has atrophied into performative compliance as the EU labor standards regime has become increasingly ritualistic without corresponding enforcement strengthening. Germany maintains the law to satisfy EU reporting requirements and domestic legitimacy, but enforcement ratios (complaints to convictions) suggest degraded function. High theater ratio — compliance meetings, anti-discrimination training, diversity metrics — with modest actual behavior change. The law persists through institutional inertia rather than active need.
constraint_indexing:constraint_classification(german_equal_opportunity_law, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / IMMUTABILITY VIEW (MOUNTAIN) — From a civilizational/universal perspective, some employment discrimination is inherent to human cognition and in-group bias — no law can eliminate the underlying tendency to prefer similar agents. The constraint appears as an immutable sociological limit: legislation can only redirect discrimination to harder-to-detect forms (statistical discrimination, preference signaling through proxies). However, this perspective risks naturalizing contingent institutional design as immutable law.
constraint_indexing:constraint_classification(german_equal_opportunity_law, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(german_equal_opportunity_law_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(german_equal_opportunity_law, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(german_equal_opportunity_law, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(german_equal_opportunity_law, TR),
    TR >= 0.70.

:- end_tests(german_equal_opportunity_law_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The law produces genuine coordination benefits (prevents discrimination-based wage competition, establishes shared labor standards) alongside genuine extraction costs (HR compliance infrastructure, legal liability exposure, verification burden). The intermediate value reflects that both functions are substantial and neither dominates. The initial baseline (0.28) reflects the immediate post-enactment period when compliance infrastructure was minimal; the current value (0.38) reflects mature compliance regime with established HR practices and case law. Suppression (0.42): Moderate. Barriers to effective discrimination verification are real (evidentiary standards require proving intent or statistical pattern, employers control internal documentation, retaliation risk is credible), but suppression is not total (union support is available, works councils provide internal advocates, case law has developed plaintiff-favorable interpretations). Theater ratio (0.55): Moderate-high. Compliance infrastructure (mandatory training, diversity metrics, anti-discrimination policies) is substantial but produces only partial behavior change. Many complaints result in formal processing without adequate remedies. The theater has increased over time (0.40 → 0.55) as compliance became institutionalized while enforcement effectiveness plateaued.
 *
 * PERSPECTIVAL GAP:
 *   The constraint exhibits a pronounced perspectival gap. The discriminated worker sees a snare: legal protection that is difficult to verify and enforce, with high retaliation risk. The small employer sees partial rope: the law coordinates standards but extracts compliance costs. The large corporation sees rope: the law enables orderly talent markets and provides legitimacy. The enforcement institution sees pure rope: the law constitutes their function. The trade union sees scaffold: the law is temporary support toward stronger worker power. The EU regime sees piton: the law is performed compliance with degraded actual enforcement. The civilizational observer risks seeing mountain: discrimination appears immutable to law. This full perspectival range from a single constraint indicates strong classification plurality and diagnostic value.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary/victim declarations drive the directionality derivation. Workers discriminated against (victim + trapped) generate d ≈ 0.92, producing high f(d) ≈ 1.38 and high experienced extraction. Small employers bearing compliance costs (victim + constrained) generate d ≈ 0.65, producing f(d) ≈ 1.00. Large corporations benefiting from standardization (beneficiary + mobile) generate d ≈ 0.40, producing f(d) ≈ 0.40. Enforcement institutions (beneficiary + arbitrage) generate d ≈ 0.05, producing f(d) ≈ -0.12. The computational flow from beneficiary/victim to d to f(d) to χ produces the observed perspectival gaps without additional machinery.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by recognizing that all six types are legitimate perspectival readings. The trapped worker's snare is not wrong — verification is genuinely difficult. The employer's tangled rope is not wrong — genuine coordination benefits coexist with real extraction costs. The institutional beneficiary's rope is not wrong — the law does constitute a coordination mechanism. The scaffold perspective is not wrong — organized actors do see sunset potential. The piton perspective is not wrong — theater has increased while enforcement effectiveness plateaued. The mountain perspective is not wrong as a description of sociological limits but IS wrong as a natural law claim — the law's limits are contingent institutional design, not immutable facts. The resolution is perspectival pluralism: no single type is 'correct,' but the presheaf over all perspectives reveals the constraint's structure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    verification_burden_asymmetry,
    'Does the law''s evidential standard (requiring proof of discrimination intent or statistical pattern) adequately enable workers to verify violations, or does it systematically advantage employers in litigation?',
    'Longitudinal analysis of case outcomes: proportion of complaints resulting in favorable judgment vs settlement vs dismissal; comparison with other EU jurisdictions with different evidentiary standards',
    'If standard is too high: snare classification confirmed (workers trapped without effective recourse). If standard is appropriate: tangled_rope classification confirmed (genuine coordination with asymmetric extraction).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(verification_burden_asymmetry, empirical, 'Whether evidential standards enable or obstruct worker verification of discrimination').

omega_variable(
    small_firm_exemption_effectiveness,
    'Do exemptions for firms with fewer than 10 employees (§ 1 Abs. 1a AGG) substantially reduce compliance burden or create a loophole that perpetuates discrimination in small enterprises?',
    'Comparative discrimination rates: small firms (exempt) vs medium firms (not exempt); worker complaint rates pre/post exemption thresholds; economic data on compliance costs avoided',
    'If substantial reduction: small employer perspective shifts toward rope (low extraction). If loophole: reinforces snare for workers in small firms.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(small_firm_exemption_effectiveness, empirical, 'Effectiveness of small firm exemptions in reducing burden vs enabling discrimination').

omega_variable(
    enforcement_intensity_variation,
    'Does enforcement intensity vary systematically by jurisdiction, industry, or protected class in ways that suggest selective application or institutional capture?',
    'Federal Employment Agency data on investigation rates, penalty distributions, and case prioritization across Länder and sectors; correlation analysis between complaint density and enforcement outcomes',
    'If variation is random: law functions as intended. If variation correlates with industry lobbying or political preference: suggests institutional capture, piton classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_intensity_variation, empirical, 'Whether enforcement intensity varies systematically across jurisdictions or industries').

omega_variable(
    discrimination_form_shifting,
    'As overt discrimination becomes legally risky, do employers shift to harder-to-detect forms (statistical discrimination, proxy signaling, informal networks) that the law cannot effectively address?',
    'Longitudinal tracking of complaint composition and case-law evolution; economic analysis of hiring patterns before/after enforcement intensification; qualitative interviews with HR professionals',
    'If form-shifting is substantial: law achieves theater (behavior appears compliant) without reducing actual discrimination, supporting piton classification. If form-shifting is limited: law achieves genuine behavioral change.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(discrimination_form_shifting, empirical, 'Whether employers shift to harder-to-detect discrimination forms when overt forms become legally risky').

omega_variable(
    union_mobilization_dependency,
    'To what extent does the law''s effectiveness depend on union capacity to identify violations and support worker litigation, vs. individual worker initiative?',
    'Complaint source analysis: union-filed vs individual-initiated complaints; correlation between union membership and successful case outcomes; geographic variation in enforcement correlated with union density',
    'If highly union-dependent: law functions as coalition protection mechanism, supporting scaffold classification. If independent of union capacity: law functions more symmetrically, supporting tangled_rope classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(union_mobilization_dependency, empirical, 'Dependency of law''s effectiveness on union mobilization vs individual worker initiative').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(german_equal_opportunity_law, 0, 14).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(geol_theater_baseline, german_equal_opportunity_law, theater_ratio, 0, 0.4).
narrative_ontology:measurement(geol_theater_mid, german_equal_opportunity_law, theater_ratio, 7, 0.5).
narrative_ontology:measurement(geol_theater_current, german_equal_opportunity_law, theater_ratio, 14, 0.55).

% Extraction over time
narrative_ontology:measurement(geol_extract_baseline, german_equal_opportunity_law, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(geol_extract_mid, german_equal_opportunity_law, base_extractiveness, 7, 0.33).
narrative_ontology:measurement(geol_extract_current, german_equal_opportunity_law, base_extractiveness, 14, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(german_equal_opportunity_law, enforcement_mechanism).
narrative_ontology:affects_constraint(german_equal_opportunity_law, eu_equal_treatment_directive_2000_78).
narrative_ontology:affects_constraint(german_equal_opportunity_law, german_sectoral_minimum_wage_law).
narrative_ontology:affects_constraint(german_equal_opportunity_law, german_works_council_codetermination).

% DUAL FORMULATION NOTE:
% The German equal opportunity law is downstream of EU Directive 2000/78/EC (equal treatment in employment) but represents a distinct institutional constraint with its own extractiveness profile reflecting German implementation choices (small firm exemption, evidentiary standards, enforcement funding). The law also affects sectoral labor coordination mechanisms: sectoral minimum wages and works council codetermination are coordination mechanisms that the equal opportunity law either complements (if discrimination prevention enables sectoral coordination) or interferes with (if compliance costs undermine sectoral participation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
