% ============================================================================
% CONSTRAINT STORY: individual_vs_systemic_causation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_individual_vs_systemic_causation, []).

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
 *   constraint_id: individual_vs_systemic_causation
 *   human_readable: Individual vs. Systemic Causation Attribution in Public Health Policy
 *   domain: public_health/policy
 *
 * SUMMARY:
 *   The individual-vs.-systemic causation distinction in public health policy
 *   represents a structural constraint that shapes how health problems are
 *   defined, who bears responsibility for solutions, and which interventions
 *   receive resources. The constraint operates as a classic Tangled Rope: it
 *   contains genuine coordination benefits (individual behavior change does
 *   improve health outcomes; behavioral messaging systems enable aligned
 *   action), but it is simultaneously an extraction mechanism that protects
 *   the financial interests of healthcare and pharmaceutical industries by
 *   misdirecting causal attribution away from structural interventions that
 *   would require policy change and potentially reduce industry revenue. The
 *   same structural phenomenon appears as pure extraction (Snare) to
 *   powerless patients blamed for outcomes they cannot individually change;
 *   as a degraded ritual (Piton) to public health bureaucrats who know
 *   individual-blame framing is insufficient but maintain it through
 *   institutional inertia; as legitimate beneficiary coordination (Rope) to
 *   the healthcare industry; and as a temporary problem being solved
 *   (Scaffold) to organized health equity advocates building alternative
 *   attribution frameworks. The theater ratio (0.68) reflects that public
 *   health campaigns emphasizing personal responsibility operate
 *   substantially as performance: the campaigns are highly visible, blame
 *   individuals prominently, but produce minimal health gains relative to
 *   their messaging reach — in the Goodhart sense that communicating the
 *   message becomes the metric replacing actual health improvement.
 *
 * KEY AGENTS:
 *   - Low-income and structurally marginalized populations: Primary victims (powerless/trapped) — bear health consequences while absorbing individual blame for structural barriers
 *   - Healthcare industry and pharmaceutical companies: Primary beneficiaries (institutional/arbitrage) — generate demand for behavioral and pharmaceutical interventions under individual-blame framing
 *   - Private insurers: Secondary beneficiaries (institutional/arbitrage) — reduce coverage obligations by attributing adverse outcomes to individual non-compliance rather than inadequate coverage
 *   - Community health workers: Secondary victims (moderate/constrained) — work within individual-blame framework that limits scope and effectiveness of their interventions
 *   - Public health agencies and departments: Institutional actors (institutional/constrained) — maintain individual-blame framing through inertia despite epidemiological evidence for structural determinants
 *   - Health equity movement and advocates: Organized agents (organized/mobile) — building alternative structural attribution frameworks and pushing for policy-level interventions
 *   - Epidemiologists and social epidemiology researchers: Analytical observers (analytical/analytical) — produce evidence that contradicts individual-blame framing but face institutional and funding pressures
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(individual_vs_systemic_causation, 0.58).
domain_priors:suppression_score(individual_vs_systemic_causation, 0.65).
domain_priors:theater_ratio(individual_vs_systemic_causation, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(individual_vs_systemic_causation, extractiveness, 0.58).
narrative_ontology:constraint_metric(individual_vs_systemic_causation, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(individual_vs_systemic_causation, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(individual_vs_systemic_causation, tangled_rope).
narrative_ontology:human_readable(individual_vs_systemic_causation, "Individual vs. Systemic Causation Attribution in Public Health Policy").
narrative_ontology:topic_domain(individual_vs_systemic_causation, "public_health/policy").

domain_priors:requires_active_enforcement(individual_vs_systemic_causation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(individual_vs_systemic_causation, healthcare_industry).
narrative_ontology:constraint_beneficiary(individual_vs_systemic_causation, private_insurers).
narrative_ontology:constraint_beneficiary(individual_vs_systemic_causation, pharmaceutical_companies).
narrative_ontology:constraint_victim(individual_vs_systemic_causation, low_income_populations).
narrative_ontology:constraint_victim(individual_vs_systemic_causation, structurally_marginalized_groups).
narrative_ontology:constraint_victim(individual_vs_systemic_causation, systemic_health_equity_improvements).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: LOW-INCOME PATIENT / INDIVIDUAL BLAME FRAME (SNARE) — Trapped by attribution logic that localizes causation to personal choices (diet, exercise, medication adherence) while structural barriers (food deserts, work schedules, medication costs) remain invisible. Cannot exit the framework without losing access to care. d≈0.92, f(d)≈1.38, σ=1.0 → χ≈0.80.
constraint_indexing:constraint_classification(individual_vs_systemic_causation, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: COMMUNITY HEALTH WORKER (TANGLED ROPE) — Experiences real coordination benefit (programs that target behavioral change DO improve health outcomes) but constrained by the individual-blame framework that limits intervention scope and funding. Must work within attribution logic that excludes structural interventions. d≈0.68, f(d)≈1.02, σ=0.9 → χ≈0.54.
constraint_indexing:constraint_classification(individual_vs_systemic_causation, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: HEALTHCARE INDUSTRY / PHARMACEUTICAL SECTOR (ROPE) — Benefits from individual-blame attribution that generates demand for behavioral interventions, medications, and diagnostic services. Experiences the constraint as coordination: communication of health messaging enables market alignment. d≈0.08, f(d)≈-0.10, σ=1.0 → χ≈-0.06.
constraint_indexing:constraint_classification(individual_vs_systemic_causation, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: PUBLIC HEALTH BUREAUCRACY (PITON) — Maintains individual-blame framing through institutional inertia despite decades of epidemiological evidence for structural determinants. Theater ratio (0.68) reflects that much public health messaging (personal responsibility campaigns, risk factor reduction) is performative — addressing individual factors while excluding systemic interventions from policy consideration. d≈0.55, f(d)≈0.75, σ=1.0 → χ≈0.39.
constraint_indexing:constraint_classification(individual_vs_systemic_causation, piton,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: HEALTH EQUITY MOVEMENT / ORGANIZED ADVOCATES (SCAFFOLD) — Organized actors (health equity networks, social epidemiology groups, community organizing) are building alternative attribution frameworks (social determinants of health, structural racism, policy analysis) that enable systemic interventions. See the individual-blame constraint as temporary, with sunset logic: as structural attribution gains institutional legitimacy (in academia, funders, clinical training), the exclusive dominance of individual-blame framing declines. d≈0.35, f(d)≈0.30, σ=1.0 → χ≈0.17.
constraint_indexing:constraint_classification(individual_vs_systemic_causation, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / CIVILIZATIONAL VIEW (MOUNTAIN) — From a universal perspective, causation in health outcomes IS genuinely multi-level: individual biology, behavioral choices, social environment, policy, and historical context all operate. No single level is 'the' cause. However, the structural data (ε=0.58, suppression=0.65) contradicts the mountain classification — the constraint is not about causation complexity itself but about the institutional choice to elevate individual attribution while suppressing structural attribution. False summit.
constraint_indexing:constraint_classification(individual_vs_systemic_causation, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(individual_vs_systemic_causation_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(individual_vs_systemic_causation, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(individual_vs_systemic_causation, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(individual_vs_systemic_causation, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(individual_vs_systemic_causation, TR),
    TR >= 0.70.

:- end_tests(individual_vs_systemic_causation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The individual-blame framing generates substantial extraction from powerless populations through misdirection of causal attribution. Patients internalize responsibility for outcomes driven by structural factors they cannot control (food insecurity, environmental toxins, economic stress). The healthcare and pharmaceutical industries benefit by positioning themselves as solution providers to individual failings rather than structural problems. However, extraction is not maximal (ε would exceed 0.70) because individual behavior does genuinely affect health outcomes — the constraint is not pure fiction. The individual-blame framing selectively amplifies one real causal mechanism while suppressing others, creating selective extraction. Suppression (0.65): Moderate-high. Significant barriers prevent structural attribution from entering mainstream policy discourse: (1) Institutional resistance from industries benefiting from individual-blame framing; (2) Training systems in medicine and public health prioritize clinical and behavioral models over policy and structural analysis; (3) Funding mechanisms favor behavioral and pharmaceutical research over social determinants research; (4) Media and health communication systems amplify individual-responsibility messaging. These barriers are not insurmountable — evidence for structural determinants has accumulated for decades — but they create substantial friction against alternative framings. Theater ratio (0.68): Moderate-high. Public health campaigns emphasizing personal responsibility and lifestyle modification are highly visible and communicate clear messages, but their actual health impact is modest relative to the communication intensity. The campaigns function performatively: they demonstrate government action on health, generate media attention, and provide visible activity that can be quantified (campaigns launched, messages distributed), while structural interventions that would actually address root causes (income policy, housing policy, environmental regulation) remain politically difficult and are underutilized. The theater has increased over the 50-year interval as individual-blame framing has become more entrenched, while structural approaches remain marginalized.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap is extreme. The powerless patient (Snare perspective) experiences the constraint as pure extraction: blamed for outcomes driven by structural factors beyond individual control. The beneficiary (Rope perspective) experiences coordination: behavioral change messaging aligns individual and industry interests. The public health bureaucracy (Piton perspective) experiences a degraded ritual maintained through inertia: they know the framing is insufficient but lack political will to challenge it. The health equity movement (Scaffold perspective) experiences a temporary problem being solved: structural attribution is gaining ground in academia, funders are slowly shifting, and 20-year sunset logic is plausible. The civilization-scale observer (Mountain perspective) risks naturalizing the constraint as inherent to causation itself — 'causation is complex, everyone has a role to play' — but this false summit obscures the institutional choice to elevate individual attribution while suppressing structural attribution.
 *
 * DIRECTIONALITY LOGIC:
 *   Low-income patients: Victims + trapped → d≈0.92, f(d)≈1.38. Maximum extraction. They cannot exit individual-blame framing without losing access to healthcare messaging and interventions. Healthcare industry: Beneficiaries + arbitrage → d≈0.08, f(d)≈-0.10. Net beneficiary. They can arbitrage the constraint (shift resources, fund alternative research) but choose not to because individual-blame framing aligns with revenue. Public health bureaucracy: Institutional + constrained → d≈0.55, f(d)≈0.75. Constrained by political economy but not as constrained as powerless populations. They could advocate for structural interventions but face career and institutional risk. Health equity movement: Organized + mobile → d≈0.35, f(d)≈0.30. Mobile agents with alternatives (can fund structural research, build parallel institutions). Lower effective extraction because they have agency. Epidemiologists: Analytical → d≈0.70, f(d)≈1.12. Moderate extraction — they produce evidence that contradicts the framing but face institutional pressure not to publicize contradictions.
 *
 * MANDATROPHY ANALYSIS:
 *   TANGLED ROPE GATE VERIFICATION: (1) Beneficiaries declared (healthcare_industry, private_insurers, pharmaceutical_companies) ✓. (2) Victims declared (low_income_populations, structurally_marginalized_groups) ✓. (3) Active enforcement required (true) ✓. (4) Genuine coordination function: YES. Individual behavior does affect health outcomes. Behavioral messaging systems do enable coordination of health-promoting actions. This is not manufactured. (5) Asymmetric extraction: YES. The framing selectively amplifies individual causation while suppressing structural causation, creating extraction from powerless populations who internalize responsibility. The constraint is NOT pure coordination (Rope) because the beneficiaries maintain the individual-blame framing BECAUSE it extracts — they block structural interventions that would be more effective but cost them revenue. The constraint is NOT pure extraction (Snare) because the coordination function is real and the beneficiaries genuinely profit from the messaging's behavioral effects, not from preventing alternatives. The mandatrophy is resolved: Tangled Rope is the correct classification from the analytical observer perspective. The false summit (Mountain) that claims 'causation is inherently complex' is correctly identified as false — the constraint is not about causation complexity but about institutional choice to privilege certain causal levels over others.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    attribution_counterfactual,
    'What is the true causal share: what fraction of health disparities would remain if individual behavioral differences were eliminated, holding structural conditions constant?',
    'Natural experiments (policy changes that alter structure while controlling for behavioral factors); instrumental variable estimation; synthetic cohort analysis across jurisdictions with different structural policies but similar baseline populations',
    'If >70% of disparities remain: individual-blame framing is severely inadequate, and the constraint is pure extraction (Snare from all perspectives). If <30%: individual factors are primary, and the constraint may be justified coordination (Rope from more perspectives).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(attribution_counterfactual, empirical, 'Causal share of structural vs. individual factors in health disparities').

omega_variable(
    intervention_counterfactual,
    'What is the relative effectiveness (cost per health outcome gained) of behavioral interventions vs. structural interventions? Does the individual-blame framing allocate resources sub-optimally?',
    'Meta-analysis of RCTs for behavioral interventions vs. policy natural experiments (e.g., Medicaid expansion, living wage policies, food environment regulations); cost-effectiveness comparison across intervention types',
    'If structural interventions are substantially more effective: the individual-blame framing is extractive misdirection. If behavioral interventions dominate: the framing may reflect genuine causal priority.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(intervention_counterfactual, empirical, 'Relative effectiveness of behavioral vs. structural health interventions').

omega_variable(
    institutional_capture_mechanism,
    'To what extent does the individual-blame framing persist because it aligns with the financial incentives of healthcare, pharmaceutical, and insurance industries (markets for behavioral interventions, pharmaceuticals) versus because of genuine scientific evidence?',
    'Funding source analysis (who funds behavioral vs. structural research); citation network analysis (which framing dominates in funded vs. unfunded literature); comparison of attribution patterns in peer-reviewed journals vs. gray literature from equity-focused organizations',
    'If capture is substantial: the constraint is a Snare from the powerless perspective and deliberate extraction (not false summit). If minimal: the framing reflects scientific consensus.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_capture_mechanism, empirical, 'Institutional capture in individual-blame attribution persistence').

omega_variable(
    temporal_sunset_mechanism,
    'Is the health equity movement''s scaffold perspective realistic? What conditions would need to hold for structural attribution to substantially displace individual-blame framing within 20 years?',
    'Trend analysis of funding (NIH, foundations) by research type; curricular analysis (public health, medical schools) for social determinants content; policy adoption rates for structural interventions (e.g., housing policy, food policy linked to health)',
    'If trends show structural attribution gaining ground: scaffold perspective is accurate and sunset is real. If individual-blame remains hegemonic: the equity movement is aspirational, and the constraint is more extractive than the scaffold framing suggests.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(temporal_sunset_mechanism, empirical, 'Feasibility and timeline of structural attribution displacement').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(individual_vs_systemic_causation, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ivsc_tr_t0, individual_vs_systemic_causation, theater_ratio, 0, 0.35).
narrative_ontology:measurement(ivsc_tr_t25, individual_vs_systemic_causation, theater_ratio, 25, 0.52).
narrative_ontology:measurement(ivsc_tr_t50, individual_vs_systemic_causation, theater_ratio, 50, 0.68).

% Extraction over time
narrative_ontology:measurement(ivsc_be_t0, individual_vs_systemic_causation, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(ivsc_be_t25, individual_vs_systemic_causation, base_extractiveness, 25, 0.46).
narrative_ontology:measurement(ivsc_be_t50, individual_vs_systemic_causation, base_extractiveness, 50, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(individual_vs_systemic_causation, information_standard).
narrative_ontology:affects_constraint(individual_vs_systemic_causation, health_equity_metric_definition).
narrative_ontology:affects_constraint(individual_vs_systemic_causation, pharmaceutical_marketing_regulation).
narrative_ontology:affects_constraint(individual_vs_systemic_causation, structural_poverty_policy).

% DUAL FORMULATION NOTE:
% The individual-vs.-systemic causation attribution constraint is upstream of specific health outcome disparities. It establishes the explanatory frame within which public health problems are understood. Downstream constraints (health equity metrics, pharmaceutical marketing, poverty policy) are affected by whether causation is attributed to individual or structural factors.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(individual_vs_systemic_causation, institutional, 0.12).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
