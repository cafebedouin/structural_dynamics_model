% ============================================================================
% CONSTRAINT STORY: generational_economic_decline
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2025-01-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_generational_economic_decline, []).

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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    domain_priors:emerges_naturally/1,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: generational_economic_decline
 *   human_readable: Generational Economic Decline as Perceived Immutable Constraint
 *   domain: political_economy/comparative_politics/democratic_theory
 *
 * SUMMARY:
 *   The widespread perception that children will be financially worse off
 *   than their parents represents a fundamental shift in the social contract
 *   of advanced democracies. What was once an expectation of
 *   intergenerational upward mobility — a core legitimating narrative of
 *   democratic capitalism — has inverted into an expectation of decline. This
 *   constraint is presented and experienced as an immutable economic reality:
 *   the natural outcome of globalization, automation, demographic aging, and
 *   productivity slowdown. Survey data shows majorities in most OECD
 *   countries now expect downward mobility. Real wage trajectories confirm
 *   stagnation for younger cohorts. Homeownership rates have collapsed for
 *   millennials relative to boomers at the same age. The constraint appears
 *   to emerge naturally from economic forces beyond political control.
 *   However, the structural data reveals identifiable beneficiaries:
 *   incumbent wealth holders whose asset values appreciate while younger
 *   cohorts face credential inflation and housing unaffordability; the
 *   financial sector that extracts rents from student debt and housing
 *   financialization; the asset-owning class that benefits from policies
 *   (restrictive zoning, regressive tax structures, labor market
 *   deregulation) that concentrate gains with capital over labor. The
 *   constraint's mountain classification from all perspectives — including
 *   the analytical observer — makes it a candidate for false summit
 *   detection. The question is whether generational decline is a genuine
 *   economic law (demographic transition, diminishing returns to capital) or
 *   a naturalized policy regime that benefits those who hold wealth today at
 *   the expense of those who will hold it tomorrow.
 *
 * KEY AGENTS:
 *   - Millennial Precariat: Primary experiencer (powerless/trapped) — faces wage stagnation, housing unaffordability, credential inflation with no individual exit
 *   - Middle-Class Parent: Secondary experiencer (moderate/constrained) — can make individual adjustments but cannot escape aggregate trend
 *   - Incumbent Wealth Holders: Primary beneficiary (institutional/arbitrage) — asset appreciation, policy regimes that favor capital over labor
 *   - Asset-Owning Class: Primary beneficiary (institutional/arbitrage) — benefits from housing scarcity, financialization, regressive tax structure
 *   - Financial Sector: Secondary beneficiary (institutional/arbitrage) — extracts rents from student debt, housing financialization, retirement insecurity
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing policy-constructed outcomes as economic laws
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(generational_economic_decline, 0.08).
domain_priors:suppression_score(generational_economic_decline, 0.03).
domain_priors:theater_ratio(generational_economic_decline, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(generational_economic_decline, extractiveness, 0.08).
narrative_ontology:constraint_metric(generational_economic_decline, suppression_requirement, 0.03).
narrative_ontology:constraint_metric(generational_economic_decline, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(generational_economic_decline, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(generational_economic_decline, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(generational_economic_decline, mountain).
narrative_ontology:human_readable(generational_economic_decline, "Generational Economic Decline as Perceived Immutable Constraint").
narrative_ontology:topic_domain(generational_economic_decline, "political_economy/comparative_politics/democratic_theory").

domain_priors:emerges_naturally(generational_economic_decline).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(generational_economic_decline, incumbent_wealth_holders).
narrative_ontology:constraint_beneficiary(generational_economic_decline, asset_owning_class).
narrative_ontology:constraint_beneficiary(generational_economic_decline, financial_sector).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: MILLENNIAL PRECARIAT (MOUNTAIN) — Experiences generational decline as an immutable economic law. No individual exit from structural wage stagnation, housing unaffordability, or credential inflation. The constraint appears as natural as gravity — a fixed feature of the economic landscape that cannot be changed within a biographical timeframe.
constraint_indexing:constraint_classification(generational_economic_decline, mountain,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: MIDDLE-CLASS PARENT (MOUNTAIN) — Sees children's diminished prospects as an inevitable economic reality. Can make individual adjustments (save more, relocate, invest in education) but cannot escape the aggregate trend. The structural forces — globalization, automation, financialization — appear as immutable constraints operating beyond individual or even collective agency.
constraint_indexing:constraint_classification(generational_economic_decline, mountain,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: ASSET-OWNING CLASS (MOUNTAIN) — Perceives the constraint as a natural market outcome. Capital accumulation dynamics, returns to scale, and demographic shifts appear as economic laws rather than policy choices. This perspective naturalizes the constraint most completely — the beneficiary sees no extraction, only efficient market operation.
constraint_indexing:constraint_classification(generational_economic_decline, mountain,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: ANALYTICAL OBSERVER (MOUNTAIN) — From a civilizational perspective, generational economic decline appears as a structural feature of mature capitalist economies: diminishing returns to capital accumulation, demographic transition effects, and productivity slowdown. However, the beneficiary declarations reveal this as a false summit — the 'natural' economic forces are mediated through policy choices (tax structure, housing policy, labor regulation, monetary policy) that concentrate gains with incumbent wealth holders.
constraint_indexing:constraint_classification(generational_economic_decline, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(generational_economic_decline_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(generational_economic_decline, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(generational_economic_decline, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(generational_economic_decline, ExtMetricName, E),
    domain_priors:suppression_score(generational_economic_decline, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(generational_economic_decline),
    narrative_ontology:constraint_metric(generational_economic_decline, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(generational_economic_decline, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(generational_economic_decline_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.08): Very low. The constraint appears to emerge from structural economic forces rather than deliberate extraction. Most of the 'extraction' is indirect — policy choices that favor incumbent wealth holders are often justified as economically necessary rather than as deliberate transfers. The low value reflects that the constraint operates primarily through naturalization rather than overt coercion. Suppression (0.03): Very low. No active enforcement is required — the constraint is maintained through perceived inevitability. Individuals can attempt to exit through education, migration, or entrepreneurship, but these strategies do not change the aggregate trend. The low suppression reflects that the constraint operates through structural forces (or the perception thereof) rather than through active prevention of alternatives. Theater ratio (0.15): Low but rising. Some performative policy responses exist (student debt relief proposals, first-time homebuyer programs, wage subsidy schemes) that acknowledge the problem without addressing structural causes. The theater has increased slightly over the interval as political pressure has mounted, but remains low because the constraint is primarily naturalized rather than actively managed. Accessibility collapse (0.92): Very high. The constraint appears equally immutable across all observation positions — even beneficiaries perceive it as a natural market outcome rather than a policy choice. Resistance (0.08): Very low. Minimal organized resistance to the constraint as such, because it is perceived as an economic reality rather than a political arrangement. Resistance that does exist (progressive tax proposals, housing supply advocacy, labor organizing) targets specific policies rather than the aggregate constraint.
 *
 * PERSPECTIVAL GAP:
 *   All four perspectives classify as mountain, but the structural relationships differ. The powerless agent experiences the constraint as an immutable barrier to biographical mobility. The moderate agent sees it as an aggregate trend beyond individual control. The institutional beneficiary sees it as a natural market outcome. The analytical observer sees it as a structural feature of mature economies. The gap is not in classification but in the naturalization mechanism: beneficiaries naturalize because the constraint serves their interests; victims naturalize because they lack the analytical tools to see policy construction behind economic 'laws.' The false summit detector identifies this pattern: when a mountain has beneficiaries, the immutability may be constructed rather than inherent.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality derivation reveals the false summit structure. Incumbent wealth holders and the asset-owning class are declared beneficiaries with arbitrage exit options — they experience the constraint as coordination (low d → low/negative χ). The millennial precariat and middle-class parents are not declared as victims because they perceive the constraint as natural rather than extractive — but their structural position (trapped/constrained exit, no beneficiary status) would derive high d if they were declared victims. The analytical observer's mountain classification at civilizational scope risks naturalizing this asymmetry. The beneficiary declarations trigger the false summit detector: a constraint that appears as mountain from all perspectives but has identifiable beneficiaries is a candidate for naturalized extraction. The omega variables document the empirical question: is the decline genuinely structural (demographic, productivity) or policy-constructed (tax, housing, labor regulation)?
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint demonstrates how the mandatrophy operates at the boundary between mountain and false summit. The classification as mountain is structurally correct given the metrics: very low extractiveness, very low suppression, very high accessibility collapse, natural emergence. But the beneficiary declarations reveal that the 'natural' economic forces are mediated through policy choices. The mandatrophy is resolved by recognizing that both readings are valid: (1) From within the current policy regime, generational decline operates as an immutable constraint — no individual or even organized collective action can reverse it without changing the regime. (2) From a regime-external perspective, the constraint is contingent on specific institutional arrangements (tax structure, housing policy, labor regulation, monetary policy) that could be changed. The mountain classification captures the first reading; the false summit detection captures the second. The constraint is a mountain within the regime and a constructed arrangement across regimes.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_vs_constructed_decline,
    'Is generational economic decline an inevitable feature of mature economies, or a constructed outcome of specific policy regimes?',
    'Cross-national comparison of generational mobility under different policy frameworks; historical analysis of periods with rising vs falling intergenerational mobility; decomposition of decline into demographic vs policy-driven components',
    'If natural: Mountain classification holds — no policy intervention can reverse the trend. If constructed: Constraint is a false summit — the ''immutable'' economic forces are actually contingent institutional arrangements that benefit incumbent wealth holders.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(natural_vs_constructed_decline, empirical, 'Whether decline is natural economic law or policy-constructed outcome').

omega_variable(
    policy_reversibility_threshold,
    'What magnitude of policy intervention would be required to reverse generational decline trends?',
    'Modeling of tax, housing, education, and labor policy changes; historical precedents of successful mobility restoration (postwar period, Nordic models); identification of policy combinations that shift intergenerational wealth trajectories',
    'If threshold is low (achievable within normal democratic politics): False summit confirmed. If threshold is prohibitively high (requires regime change or crisis): Mountain classification may be structurally accurate even if policy-mediated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(policy_reversibility_threshold, empirical, 'Policy intervention magnitude needed to reverse decline').

omega_variable(
    beneficiary_awareness,
    'Do incumbent wealth holders consciously maintain policies that preserve generational decline, or do they genuinely perceive the constraint as natural?',
    'Analysis of policy advocacy patterns; correlation between asset ownership and support for policies that maintain decline; examination of framing in policy discourse (natural forces vs policy choices)',
    'If conscious: Extraction is deliberate, strengthening false summit diagnosis. If unconscious: Beneficiaries are also captured by the naturalization frame, suggesting deeper ideological lock.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_awareness, conceptual, 'Whether beneficiaries consciously maintain or unconsciously naturalize the constraint').

omega_variable(
    demographic_vs_policy_decomposition,
    'What proportion of generational decline is attributable to demographic factors (aging, dependency ratios) vs policy choices (tax structure, housing supply, labor regulation)?',
    'Econometric decomposition of mobility trends; cross-national comparison controlling for demographics; identification of policy-reversible vs demographic-structural components',
    'If primarily demographic: Mountain classification more defensible — some component is genuinely structural. If primarily policy-driven: False summit confirmed — the constraint is contingent on institutional arrangements.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(demographic_vs_policy_decomposition, empirical, 'Decomposition of decline into demographic vs policy components').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(generational_economic_decline, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gen_econ_theater_1990, generational_economic_decline, theater_ratio, 0, 0.1).
narrative_ontology:measurement(gen_econ_theater_2005, generational_economic_decline, theater_ratio, 15, 0.12).
narrative_ontology:measurement(gen_econ_theater_2020, generational_economic_decline, theater_ratio, 30, 0.15).

% Extraction over time
narrative_ontology:measurement(gen_econ_extract_1990, generational_economic_decline, base_extractiveness, 0, 0.05).
narrative_ontology:measurement(gen_econ_extract_2005, generational_economic_decline, base_extractiveness, 15, 0.07).
narrative_ontology:measurement(gen_econ_extract_2020, generational_economic_decline, base_extractiveness, 30, 0.08).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(generational_economic_decline, resource_allocation).

% DUAL FORMULATION NOTE:
% This constraint could be decomposed into multiple stories: housing_unaffordability (ε ≈ 0.35, tangled_rope), credential_inflation (ε ≈ 0.28, tangled_rope), wage_stagnation (ε ≈ 0.18, rope/mountain boundary), student_debt_burden (ε ≈ 0.52, snare). The aggregate 'generational decline' constraint has very low ε because it is experienced as a natural outcome rather than deliberate extraction, but the component constraints have higher ε values reflecting specific policy-constructed mechanisms. This story models the aggregate perception; component stories would model the specific extraction mechanisms.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
