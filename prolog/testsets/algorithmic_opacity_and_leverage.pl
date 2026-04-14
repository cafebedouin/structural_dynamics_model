% ============================================================================
% CONSTRAINT STORY: algorithmic_opacity_and_leverage
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_algorithmic_opacity_and_leverage, []).

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
 *   constraint_id: algorithmic_opacity_and_leverage
 *   human_readable: Algorithmic Opacity and Leverage in Automated Decision Systems
 *   domain: technology/governance/economics
 *
 * SUMMARY:
 *   Algorithmic opacity in automated decision systems creates a structural
 *   extraction mechanism where opacity simultaneously serves genuine
 *   coordination functions (enabling scale, efficiency, distributed
 *   decision-making) and asymmetric extraction (concentrating power,
 *   obscuring harms, preventing accountability). The constraint exhibits the
 *   signature of a snare yoked to coordination theater. Affected individuals
 *   cannot understand or appeal algorithmic decisions in credit, employment,
 *   criminal justice, content moderation, and resource allocation systems.
 *   Marginalized communities disproportionately bear extraction costs due to
 *   training data bias and constrained exit options. Regulatory agencies face
 *   a fundamental capacity gap: they cannot oversee what they cannot observe,
 *   and algorithm complexity grows faster than regulatory tools. Institutions
 *   deploying algorithms benefit from opacity (reduced liability, cost
 *   containment, speed) while claiming that explainability would be
 *   technically infeasible or would sacrifice performance — a claim that
 *   oscillates between true (some performance-interpretability tradeoffs are
 *   real) and convenient (many opacity choices are design decisions, not
 *   necessities). The theater ratio rising from 0.25 to 0.61 reflects
 *   proliferation of explainability frameworks, ethics boards, and
 *   algorithmic audits that create appearance of governance without shifting
 *   underlying power asymmetries. The extractiveness rising from 0.35 to 0.58
 *   reflects deepening algorithmic mediation across life domains (financial,
 *   employment, criminal, social) without corresponding transparency gains.
 *
 * KEY AGENTS:
 *   - Affected Individuals: Primary victims (powerless/trapped) — subject to opaque decisions with no recourse, trapped by ecosystem dependency, no exit option
 *   - Marginalized Communities: Victims (powerless/constrained) — face high suppression, high structural barriers to exit, multigenerational extraction through algorithmic discrimination
 *   - Algorithm-Deploying Institutions: Primary beneficiaries (institutional/arbitrage) — benefit from opacity through scale, cost reduction, liability insulation; can exit by regulatory arbitrage to low-transparency jurisdictions
 *   - Regulatory Agencies: Secondary victims (powerful/mobile) — need opacity for enforcement capacity but have mobile exit option (mandate transparency); experience both coordination and extraction
 *   - Advocacy and Research Communities: Organized intermediaries (organized/constrained) — expose opacity while benefiting from algorithmic systems; constrained by ecosystem dependence
 *   - Algorithm Designers: Secondary beneficiaries (powerful/mobile) — benefit from opacity via intellectual property protection and career insulation; can migrate to deploying institutions
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — sees both genuine coordination (algorithmic efficiency) and asymmetric extraction (opacity as power concentration)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(algorithmic_opacity_and_leverage, 0.58).
domain_priors:suppression_score(algorithmic_opacity_and_leverage, 0.68).
domain_priors:theater_ratio(algorithmic_opacity_and_leverage, 0.61).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(algorithmic_opacity_and_leverage, extractiveness, 0.58).
narrative_ontology:constraint_metric(algorithmic_opacity_and_leverage, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(algorithmic_opacity_and_leverage, theater_ratio, 0.61).

% --- Constraint claim ---
narrative_ontology:constraint_claim(algorithmic_opacity_and_leverage, snare).
narrative_ontology:human_readable(algorithmic_opacity_and_leverage, "Algorithmic Opacity and Leverage in Automated Decision Systems").
narrative_ontology:topic_domain(algorithmic_opacity_and_leverage, "technology/governance/economics").

domain_priors:requires_active_enforcement(algorithmic_opacity_and_leverage).
% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(algorithmic_opacity_and_leverage, algorithm_deploying_institutions).
narrative_ontology:constraint_beneficiary(algorithmic_opacity_and_leverage, algorithm_designers).
narrative_ontology:constraint_victim(algorithmic_opacity_and_leverage, affected_individuals).
narrative_ontology:constraint_victim(algorithmic_opacity_and_leverage, regulatory_oversight_capacity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: AFFECTED INDIVIDUAL (SNARE) — Subject to algorithmic decisions (credit, hiring, parole, content moderation) with no transparency into decision logic, no meaningful appeal process, and no exit from the algorithmic ecosystem. Cannot understand why decisions were made. Cannot articulate grounds for appeal because grounds are hidden. Trapped by systemic dependency on algorithmic mediation (financial systems, employment markets, digital platforms).
constraint_indexing:constraint_classification(algorithmic_opacity_and_leverage, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: MARGINALIZED COMMUNITIES (SNARE) — Structurally constrained exit (geographic/economic/social immobility) from algorithmic decision systems that disproportionately harm them. High suppression: alternatives are more expensive, less accessible, or legally unavailable. Cannot coordinate collective action because individual harms are dispersed and attributed to 'algorithm behavior' rather than policy choice. Multigenerational extraction via algorithmic discrimination feeds into structural inequality.
constraint_indexing:constraint_classification(algorithmic_opacity_and_leverage, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: REGULATORY AGENCIES (TANGLED ROPE) — Agencies benefit from algorithmic efficiency (scale oversight, faster turnaround) but victims of opacity constraint (cannot effectively regulate what they cannot see). Experience both coordination (algorithms enable services that would be impossible without automation) and extraction (ability to regulate is weakened). Mobile exit option at institutional scale — can mandate transparency — but face political and technical barriers. Asymmetric: agency capacity lags algorithm complexity growth.
constraint_indexing:constraint_classification(algorithmic_opacity_and_leverage, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 4: ALGORITHM-DEPLOYING INSTITUTIONS (ROPE) — Primary beneficiary. Opacity enables scale (process decisions faster), cost reduction (labor replacement), and risk mitigation (opacity insulates from legal liability when outcomes are 'algorithm's fault'). Experience constraint as pure coordination problem: they need to automate decision-making, algorithms require parameter tuning, opacity is a side effect they actively prefer. Arbitrage exit option: can migrate to jurisdictions with weaker transparency requirements.
constraint_indexing:constraint_classification(algorithmic_opacity_and_leverage, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: ADVOCACY/RESEARCH COMMUNITIES (TANGLED ROPE) — Organized agents who derive benefit from algorithmic systems (training data, computational access, platform reach) while working to expose opacity. Constrained exit: cannot fully withdraw from algorithmic ecosystems without losing research capacity and advocacy reach. Both coordinate (demand transparency, develop interpretability methods) and extract (create new institutional lever for institutional actors, potentially creating audit theater without actual power redistribution).
constraint_indexing:constraint_classification(algorithmic_opacity_and_leverage, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: EXPLAINABILITY THEATER (PITON) — The proliferation of 'explainable AI' frameworks, ethics boards, and algorithmic audits has become largely theatrical: they create appearance of governance without reducing underlying opacity or shifting power. Institutions maintain explainability rituals (publishing ethics statements, conducting audits, deploying LIME/SHAP interpretability tools) because governance pressure exists, not because these mechanisms meaningfully constrain algorithmic leverage. The theater persists through institutional inertia despite low functional capacity to alter outcomes.
constraint_indexing:constraint_classification(algorithmic_opacity_and_leverage, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (TANGLED ROPE) — From a civilizational view, algorithmic opacity is a genuine coordination mechanism (enables scale, cost reduction, distributed decision-making) yoked to asymmetric extraction (opacity concentrates power, obscures harms, prevents accountability). The constraint is neither pure coordination nor pure extraction but a hybrid that uses coordination benefits to justify tolerating extraction costs borne by powerless agents. Classification would misfire if observing only coordination function; must capture both.
constraint_indexing:constraint_classification(algorithmic_opacity_and_leverage, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(algorithmic_opacity_and_leverage_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(algorithmic_opacity_and_leverage, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(algorithmic_opacity_and_leverage, TypeOther, context(agent_power(powerful), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(algorithmic_opacity_and_leverage, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(algorithmic_opacity_and_leverage, TR),
    TR >= 0.70.

:- end_tests(algorithmic_opacity_and_leverage_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Elevated. Affected individuals cannot exit algorithmic decision systems and cannot understand or contest outcomes. The opacity is not random complexity but a design choice that concentrates interpretive power with deploying institutions. Suppression (0.68): High. Multiple overlapping barriers prevent individuals from contesting algorithmic decisions: technical opacity (cannot understand decision logic), legal opacity (intellectual property protections prevent access to code/data), institutional opacity (no mandatory explanation requirement in many domains), and diffuse harm (individual decisions appear isolated rather than systemic). Theater ratio (0.61): Moderate-high. Explainability frameworks, ethics statements, algorithmic audits, and interpretability tools have proliferated without corresponding shifts in actual power or accountability. Institutions provide post-hoc explanations (LIME, SHAP, attention weights) that create appearance of transparency while preserving core opacity. The theater has risen as pressure has mounted, with low functional impact on actual decision contestability.
 *
 * PERSPECTIVAL GAP:
 *   The snare vs rope perspectival gap is the core diagnostic signal. Affected individuals accurately perceive snare: they cannot escape, cannot understand, cannot appeal. Deploying institutions genuinely experience rope: opacity solves a real coordination problem (automating decisions that would be impossible to make manually at scale). But the rope exists in an asymmetric configuration: the coordination benefit flows to beneficiaries while suppression costs flow to victims. The snare classification from the powerless perspective and rope classification from the institutional perspective are not contradictory — they are orthogonal views of the same asymmetric structure. The analytical observer must integrate both to arrive at tangled rope: genuine coordination function + asymmetric extraction + active institutional enforcement of opacity.
 *
 * DIRECTIONALITY LOGIC:
 *   The pipeline derives directionality from beneficiary/victim declarations and exit options. Affected individuals are trapped victims with no exit (d ≈ 0.92), deriving high f(d) ≈ 1.40 and high experienced extraction chi. Deploying institutions are institutional beneficiaries with arbitrage exit (d ≈ 0.08), deriving low/negative f(d) ≈ -0.08 and negative experienced extraction. Regulatory agencies are powerful victims with mobile exit (d ≈ 0.65), deriving f(d) ≈ 0.98 and moderate experienced extraction. Advocacy communities are organized actors with constrained exit (d ≈ 0.50), deriving f(d) ≈ 0.65 and moderate experienced extraction. The scope modifier σ(global) = 1.2 amplifies chi across all perspectives: trapped individuals experience chi = 0.58 × 1.40 × 1.2 ≈ 0.97 (near-total extraction); beneficiary institutions experience chi = 0.58 × (-0.08) × 1.2 ≈ -0.06 (slight leverage inversion, they experience constraint as enabling rather than extractive).
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by recognizing that algorithmic opacity is genuinely a snare from the perspective of powerless, trapped agents AND genuinely a rope from the perspective of institutional deployers. The constraint is not mislabeled coordination (institutions do genuinely need to automate decisions) nor mislabeled extraction (the power asymmetry and suppression are structural, not incidental). The snare classification is correct for powerless agents; the rope classification is correct for institutional agents. The piton classification for 'explainability theater' is correct as a separate observation about institutional responses to transparency pressure. The tangled rope classification at the analytical level captures the hybrid: opacity serves genuine coordination functions while enabling asymmetric extraction. Mandatrophy is resolved by recognizing multiple local classifications as legitimate perspectival readings rather than contradictions.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    opacity_necessity_threshold,
    'Is algorithmic opacity inherent to model complexity, or is it a chosen design parameter to avoid accountability?',
    'Comparison of deployed opacity vs theoretical minimum required for model function; analysis of proprietary-code vs open-source systems; correlation between opacity and institutional liability exposure',
    'If inherent: snare classification may mischaracterize necessary complexity as intentional suppression. If chosen: snare classification is accurate, and alternative transparent designs are feasible but rejected for cost/liability reasons.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(opacity_necessity_threshold, empirical, 'Whether opacity is inherent to algorithm complexity or chosen for institutional benefit').

omega_variable(
    interpretability_tool_effectiveness,
    'Do post-hoc interpretability methods (LIME, SHAP, attention mechanisms) actually restore meaningful user agency or are they primarily theater?',
    'User studies measuring decision confidence/recourse action after explainability; audit effectiveness comparing algorithmic decisions before/after institution-mandated transparency; regulatory enforcement rates on institutions providing vs not providing explanations',
    'If effective: suppress score should be lower (users have actual leverage); piton classification for ''ethics boards'' is accurate. If theater: suppress score confirmed; entire explainability apparatus is piton-like degradation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(interpretability_tool_effectiveness, empirical, 'Whether explainability tools provide user agency or are primarily theater').

omega_variable(
    market_viability_of_transparency,
    'Could competitive markets sustain transparent algorithms if regulatory pressure were removed, or is opacity lock-in structural?',
    'Historical analysis of transparency-favorable vs opacity-favorable regulatory regimes; emergence of transparent-algorithm startups in high-transparency jurisdictions; cost differential between transparent and opaque deployment at comparable scale',
    'If market-sustainable: constraint is piton (institutional inertia) or scaffold (temporary until market shifts). If structurally locked: snare persists because transparency has real cost disadvantages that markets do not overcome.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(market_viability_of_transparency, empirical, 'Whether transparent algorithms are viable in competitive markets').

omega_variable(
    collective_action_barriers,
    'Why do affected individuals not coordinate to demand algorithmic transparency? Is it suppression of coordination capacity or diffuse harm attribution?',
    'Comparative analysis of transparency campaigns: successful (GDPR, algorithmic audits) vs stalled (algorithmic recourse, hiring algorithm transparency); interviews with affected populations on barriers to organizing; network analysis of transparency advocacy movements',
    'If suppression of coordination: snare classification maintained. If diffuse harm + attribution failure: suppress score should be lower; constraint is more snare-like than institutional suppression metrics suggest (individual-level internalization of responsibility).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(collective_action_barriers, empirical, 'Why affected individuals do not coordinate to demand transparency').

omega_variable(
    identity_locked_institutional_actors,
    'Are algorithm deployers identity-locked to opacity (profit-maximization identity), or do they face genuine technical/commercial barriers to transparency?',
    'Analysis of institutions that voluntarily adopted transparency (Zillow''s algorithm publication, some hiring audits); cost-benefit analysis of proprietary vs open code; regulatory enforcement trends and institution adaptation speed',
    'If identity-locked: deploying institutions may reclassify at constrained exit rather than arbitrage; mandating transparency changes their strategic logic. If structural barriers: transparency mandates create compliance theater without functional change.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_locked_institutional_actors, conceptual, 'Whether deployers are identity-locked to opacity or face technical barriers').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(algorithmic_opacity_and_leverage, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(alg_opac_tr_t0, algorithmic_opacity_and_leverage, theater_ratio, 0, 0.25).
narrative_ontology:measurement(alg_opac_tr_t5, algorithmic_opacity_and_leverage, theater_ratio, 5, 0.45).
narrative_ontology:measurement(alg_opac_tr_t10, algorithmic_opacity_and_leverage, theater_ratio, 10, 0.61).

% Extraction over time
narrative_ontology:measurement(alg_opac_be_t0, algorithmic_opacity_and_leverage, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(alg_opac_be_t5, algorithmic_opacity_and_leverage, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(alg_opac_be_t10, algorithmic_opacity_and_leverage, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(algorithmic_opacity_and_leverage, enforcement_mechanism).
narrative_ontology:affects_constraint(algorithmic_opacity_and_leverage, algorithmic_discrimination).
narrative_ontology:affects_constraint(algorithmic_opacity_and_leverage, regulatory_capture_by_platform_companies).
narrative_ontology:affects_constraint(algorithmic_opacity_and_leverage, information_asymmetry_in_automated_systems).

% DUAL FORMULATION NOTE:
% Algorithmic opacity is upstream of specific harms (discrimination, regulatory capture, information asymmetry). It represents a structural constraint on the capacity to observe and contest algorithmic decisions. The downstream constraints inherit the opacity characteristic from this upstream constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(algorithmic_opacity_and_leverage, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
