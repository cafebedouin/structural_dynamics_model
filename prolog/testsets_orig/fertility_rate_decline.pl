% ============================================================================
% CONSTRAINT STORY: fertility_rate_decline
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_fertility_rate_decline, []).

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
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: fertility_rate_decline
 *   human_readable: Fertility Rate Decline and Intergenerational Reproduction Coordination
 *   domain: demographic/social/economic
 *
 * SUMMARY:
 *   Fertility rate decline in developed economies over the past 50 years
 *   represents a fundamental restructuring of the intergenerational
 *   reproductive contract. Framed officially as a demographic crisis, it
 *   exhibits the structure of a tangled_rope constraint: genuine coordination
 *   benefits (female workforce participation, educational access, economic
 *   autonomy) coexist with asymmetric extraction (reproductive burden shifted
 *   from married women to remaining childless or single-child families,
 *   pension liabilities transferred to smaller working cohorts, elder care
 *   concentrated among fewer siblings). The constraint operates across
 *   multiple institutional domains simultaneously: labor markets (career
 *   penalties for parenthood), housing markets (family-sized housing priced
 *   beyond childbearing-age households), welfare systems (pension actuarials
 *   designed for replacement fertility), and cultural narratives (pronatalism
 *   as theater masking policy inaction). The beneficiaries are economic —
 *   employers, capital, and educational institutions — while the victims are
 *   demographic — pensioners facing system insolvency, young professionals
 *   bearing compressed fertility windows, and intergenerational care systems.
 *   The theater ratio reflects that state pronatalist rhetoric ('demographic
 *   crisis,' 'population replacement anxiety') vastly exceeds actual policy
 *   commitment to childcare, housing access, or parental leave equity.
 *
 * KEY AGENTS:
 *   - Women and Professional Classes: Primary beneficiary-victims (moderate/constrained) — gain educational access and workforce participation but bear reproductive delay and biological window compression
 *   - Pensioner Cohorts: Primary victims (powerless/trapped) — face system insolvency as demographic replacement fails; cannot exit retirement status or restructure actuarial foundation
 *   - Employers and Capital: Primary beneficiaries (institutional/arbitrage) — benefit from extended prime-earning years, reduced parental-leave demand, expanded female labor supply, lower wage pressure
 *   - State Welfare Systems: Institutional victim (institutional/constrained) — pension and healthcare systems designed for replacement fertility; face insolvency and must restructure implicitly through benefit reduction or tax increase
 *   - Family Policy Coalition: Organized actors (organized/constrained) — government ministries, NGOs, family services; propose sunset through childcare subsidies, housing access, parental leave reform
 *   - Pronatalist Discourse: Institutional performance (institutional/arbitrage) — government narrative maintains political salience and funding justification; persists through inertia despite policy implementation gaps
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing contingent institutional arrangements (childcare costs, housing prices, career penalties) as immutable laws of modernization
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fertility_rate_decline, 0.58).
domain_priors:suppression_score(fertility_rate_decline, 0.52).
domain_priors:theater_ratio(fertility_rate_decline, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fertility_rate_decline, extractiveness, 0.58).
narrative_ontology:constraint_metric(fertility_rate_decline, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(fertility_rate_decline, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fertility_rate_decline, tangled_rope).
narrative_ontology:human_readable(fertility_rate_decline, "Fertility Rate Decline and Intergenerational Reproduction Coordination").
narrative_ontology:topic_domain(fertility_rate_decline, "demographic/social/economic").

domain_priors:requires_active_enforcement(fertility_rate_decline).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(fertility_rate_decline, women_workforce_participation).
narrative_ontology:constraint_beneficiary(fertility_rate_decline, educational_attainment_access).
narrative_ontology:constraint_beneficiary(fertility_rate_decline, economic_autonomy_expansion).
narrative_ontology:constraint_beneficiary(fertility_rate_decline, delayed_family_formation_capacity).
narrative_ontology:constraint_victim(fertility_rate_decline, pension_system_solvency).
narrative_ontology:constraint_victim(fertility_rate_decline, labor_force_growth).
narrative_ontology:constraint_victim(fertility_rate_decline, state_demographic_continuity).
narrative_ontology:constraint_victim(fertility_rate_decline, intergenerational_care_burden).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PENSIONER COHORT (SNARE) — Trapped by the implicit social contract: paid into pension systems predicated on continuous demographic replacement, now facing system insolvency as fertility declines. No exit option: cannot work indefinitely, cannot restructure the actuarial foundation. Experiences maximum extraction as the constraint transfers demographic burden to them through benefit reductions, delayed eligibility, or implicit default.
constraint_indexing:constraint_classification(fertility_rate_decline, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: YOUNG PROFESSIONAL (TANGLED ROPE) — Constrained by childcare costs, housing prices, career advancement penalties for parental leave, and educational debt. But also benefits: workforce participation, educational investment, economic autonomy, career continuity. The constraint coordinates economic and educational participation while extracting reproductive labor. Mixed experience — genuine gains in autonomy alongside real costs in reproductive burden shifting.
constraint_indexing:constraint_classification(fertility_rate_decline, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: EMPLOYERS AND FIRMS (ROPE) — Direct beneficiaries of lower birth rates: extended prime earning years, reduced parental-leave demand, higher female workforce participation rates, lower nominal wage pressure from expanded labor supply. Experiences the constraint as pure coordination: solving the collective action problem of labor force participation and productivity. Net beneficiary with arbitrage options (capital mobility, wage-setting power).
constraint_indexing:constraint_classification(fertility_rate_decline, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: FAMILY POLICY COALITION (SCAFFOLD) — Organized actors (government ministries, NGOs, family-services infrastructure) see fertility decline as solvable through targeted interventions: subsidized childcare, parental leave reform, housing access, tax credits for child-rearing. The sunset clause is embedded in policy design — as childcare becomes normalized, housing affordable, and parental leave equalized, the fertility penalty should decline and birth rates stabilize at replacement or higher. Estimated sunset: 15-25 years for comprehensive policy intervention in developed economies.
constraint_indexing:constraint_classification(fertility_rate_decline, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: PRONATALIST STATE NARRATIVE (PITON) — Government discourse around 'demographic crisis,' 'population replacement,' and 'national continuity' functions primarily theatrically: performative alarm about low birth rates without structural commitment to address root barriers (childcare, housing, career penalties for parenthood). The narrative persists through institutional inertia — demographic anxiety maintains political salience and funding justification — but the underlying function (coordinating reproduction) has degraded as policies remain symbolic rather than transformative. Theater ratio reflects that state family policy rhetoric far exceeds policy implementation.
constraint_indexing:constraint_classification(fertility_rate_decline, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a universal/civilizational perspective, fertility decline follows from economic development and female autonomy: as women gain control over reproduction (contraception, education, economic independence), birth rates fall necessarily and predictably. This view treats fertility decline as an immutable consequence of modernization itself — a law of demographic transition. However, the structural data contradicts this: fertility rates vary dramatically across developed economies with similar development (France ~1.65 vs South Korea ~0.70), indicating that institutional arrangements (childcare policy, housing, parental leave) are controlling factors, not natural law. The mountain classification masks contingent institutional extraction.
constraint_indexing:constraint_classification(fertility_rate_decline, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(fertility_rate_decline_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(fertility_rate_decline, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(fertility_rate_decline, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(fertility_rate_decline, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(fertility_rate_decline, TR),
    TR >= 0.70.

:- end_tests(fertility_rate_decline_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate, rising over the interval. Initially (1960s), fertility decline appeared as simple coordination benefit — female education and workforce participation required spacing or reducing births. Over 60 years, the extraction mechanisms have accumulated: compressed biological windows for reproduction, pension system insolvency, concentrated elder care among fewer adult children, housing cost escalation tied to childless-household demand. The trajectory shows extraction accumulation, characteristic of rent-seeking layered onto coordination. Suppression (0.52): Moderate-high. Structural barriers to high fertility include childcare costs (often consuming 20-35% of professional income), family-sized housing unaffordable at childbearing-age wages, career penalties for parental leave (especially for women), educational debt burden, and limited public support. These barriers are real and material but not insurmountable — countries with aggressive public childcare and housing policy (France, Scandinavia) show fertility 0.3-0.5 higher than peers with identical development levels. Theater ratio (0.48): Low-moderate. The constraint's functional component (coordinating female autonomy and workforce participation) is genuine; the theatrical component is the pronatalist response (government discourse vastly exceeds policy investment). Theater has risen slightly over the interval as state anxiety about 'demographic crisis' has escalated without corresponding policy commitment.
 *
 * PERSPECTIVAL GAP:
 *   The widest gap lies between the pensioner's snare (extraction maximum) and the employer's rope (pure coordination benefit). The pensioner experienced no choice in the constraint's formation — they entered a pension system assuming demographic replacement — and now faces imminent system failure. The employer experienced maximum flexibility: capital can exit low-fertility economies, relocate to higher-fertility labor markets, or substitute automation for labor. The analytical observer risks collapsing this gap by declaring the decline immutable ('demographic transition is natural'), which erases the institutional contingency: France's fertility (1.65) vs South Korea's (0.70) at similar development levels shows that policy choices (childcare, housing, leave) are controlling factors. The piton perspective reveals the performative nature of state pronatalism — decades of 'demographic crisis' rhetoric with minimal childcare investment or housing reform.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values (d) derive from structural position: pensioners with no exit options and victim status experience maximum d (~0.95), driving high f(d) → high χ (snare classification). Young professionals with constrained exits and dual beneficiary-victim status experience moderate d (~0.55), producing moderate χ (tangled_rope). Employers with arbitrage options and beneficiary status experience low d (~0.15), producing negative f(d) → coordinate experience (rope). The analytical perspective at civilizational scope risks d=0.72 (observer relationship to constraint), which should classify as rope or scaffold, not mountain — the false summit occurs when the analytical observer naturalizes contingent institutional factors (childcare costs, housing prices, career penalties) as immutable laws of development rather than as policy choices. Overrides are not necessary; the derived directionality from beneficiary/victim declarations and exit options produces appropriate perspectival gaps.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION: The constraint avoids the coordination-vs-extraction ambiguity through structural decomposition. The genuine coordination function (enabling female education and workforce participation) is real and valuable — this is the rope component. The asymmetric extraction (concentrating care burden on fewer agents, transferring pension liabilities to smaller working cohorts, pricing family housing beyond childbearing-age wages) is also real and structurally distinct. These are not competing interpretations of the same data; they are different causal mechanisms operating simultaneously. The tangled_rope classification captures both: active enforcement (pension system, housing markets, workplace policies all impose the compression) and dual functions (coordination of female autonomy + extraction via reproductive burden shifting). The mandatrophy is resolved by recognizing that the legitimate coordination gains do not justify the asymmetric extraction — the constraint could deliver the coordination benefits without the extraction (universal childcare, affordable family housing, true parental leave equity, post-career pension design). The coexistence of both functions indicates that the extraction is surplus to the coordination requirement, not a necessary cost.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    preference_vs_constraint_ambiguity,
    'Is fertility decline primarily a reflection of authentic preference changes (women preferring smaller families, delayed parenthood) or a response to economic-institutional constraints that artificially increase the cost of childbearing?',
    'Cross-national policy variation analysis: compare fertility rates in countries with identical development levels but different childcare/housing/leave policies; survey data on desired vs realized fertility; qualitative research on decision-making rationales',
    'If preference-driven: constraint is mountain (immutable consequence of modernization). If constraint-driven: constraint is tangled_rope or snare (extractive institutional arrangement masquerading as preference). Policy implications differ radically.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(preference_vs_constraint_ambiguity, empirical, 'Preference-driven vs constraint-driven fertility decline').

omega_variable(
    demographic_replacement_threshold_ambiguity,
    'What fertility rate constitutes ''replacement''? Is replacement at 2.1 children per woman a natural necessity or a particular policy goal?',
    'Actuarial analysis of sustainable population structures; comparison across countries with different fertility/immigration/life expectancy combinations; economic models of intergenerational support',
    'If replacement is a natural necessity: pension system insolvency and labor shortage are immutable constraints (mountain). If replacement is a policy choice: immigration, automation, and redistribution are alternative solutions, and the ''demographic crisis'' framing is performative.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(demographic_replacement_threshold_ambiguity, conceptual, 'Whether demographic replacement at 2.1 is natural necessity or policy construct').

omega_variable(
    intergenerational_care_burden_fungibility,
    'Is childcare burden truly transferred to fewer younger cohorts, or can institutional arrangements (market services, public support, automation, immigration) decouple demographic size from care capacity?',
    'Cross-national case studies: countries with low birth rates but stable care systems (Japan, South Korea via automation/immigration) vs countries with replacement fertility but care crises; modeling of care infrastructure scaling',
    'If non-fungible: low fertility creates immutable care burden (mountain, snare perspectives valid). If fungible: care burden is a solvable coordination problem (rope, scaffold perspectives valid, piton narration false).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(intergenerational_care_burden_fungibility, empirical, 'Whether care burden is fungible or fixed by demographic structure').

omega_variable(
    identity_locked_reproductive_autonomy,
    'To what extent is fertility decline driven by women''s identity-locked commitment to economic/professional autonomy, such that returning to higher birth rates would require abandoning professional identity rather than just removing institutional barriers?',
    'Longitudinal analysis of fertility preferences: women''s stated willingness to increase childbearing if barriers removed vs observed choices when barriers are reduced; psychological research on identity fusion with professional identity',
    'If identity-locked: even robust childcare/housing policy will have modest fertility impact because the constraint is cognitive (exit would require identity reconfiguration). If constraint-driven: policy intervention should significantly raise fertility. Affects classification of young professional perspective: trapped vs constrained vs identity_locked.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_locked_reproductive_autonomy, conceptual, 'Identity fusion in reproductive autonomy choices').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fertility_rate_decline, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fert_tr_t0, fertility_rate_decline, theater_ratio, 0, 0.32).
narrative_ontology:measurement(fert_tr_t20, fertility_rate_decline, theater_ratio, 20, 0.4).
narrative_ontology:measurement(fert_tr_t40, fertility_rate_decline, theater_ratio, 40, 0.48).
narrative_ontology:measurement(fert_tr_t60, fertility_rate_decline, theater_ratio, 60, 0.52).

% Extraction over time
narrative_ontology:measurement(fert_be_t0, fertility_rate_decline, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(fert_be_t20, fertility_rate_decline, base_extractiveness, 20, 0.42).
narrative_ontology:measurement(fert_be_t40, fertility_rate_decline, base_extractiveness, 40, 0.58).
narrative_ontology:measurement(fert_be_t60, fertility_rate_decline, base_extractiveness, 60, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(fertility_rate_decline, attachment_coordination).
narrative_ontology:boltzmann_floor_override(fertility_rate_decline, 0.12).
narrative_ontology:affects_constraint(fertility_rate_decline, pension_system_actuarial_sustainability).
narrative_ontology:affects_constraint(fertility_rate_decline, elder_care_burden_concentration).
narrative_ontology:affects_constraint(fertility_rate_decline, housing_cost_escalation).
narrative_ontology:affects_constraint(fertility_rate_decline, female_workplace_equality).

% DUAL FORMULATION NOTE:
% Fertility decline is downstream of economic policy (childcare, housing, leave) and upstream of demographic consequences (pension insolvency, care burden). Each affected constraint has its own structural relationship to fertility decline; the network maps dependency chain rather than bidirectional causality.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
