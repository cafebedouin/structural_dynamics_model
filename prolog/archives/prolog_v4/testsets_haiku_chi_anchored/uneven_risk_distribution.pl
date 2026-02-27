% ============================================================================
% CONSTRAINT STORY: uneven_risk_distribution
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_uneven_risk_distribution, []).

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
 *   constraint_id: uneven_risk_distribution
 *   human_readable: Uneven Distribution of Modifiable Health Risk Factors
 *   domain: public_health/epidemiology
 *
 * SUMMARY:
 *   The uneven distribution of modifiable health risk factors across income
 *   and social groups represents a persistent epidemiological fact with
 *   profound structural and institutional dimensions. Smoking, obesity,
 *   alcohol use, HPV vaccination rates, and other classically 'modifiable'
 *   risk factors are highly stratified by socioeconomic status, education,
 *   and race — not randomly distributed. This constraint exhibits all six DR
 *   types from different perspectives because the same structural phenomenon
 *   — the concentration of health risks in disadvantaged populations — can be
 *   understood as an inevitable natural law (mountain perspective), a pure
 *   coordination problem requiring public health infrastructure (rope
 *   perspective), a temporary state amenable to structural reform (scaffold
 *   perspective), a degraded behavioral framework maintained by institutional
 *   inertia (piton perspective), a mixed coordination-extraction hybrid
 *   (tangled rope perspective), or pure extraction disguised as health
 *   education (snare perspective). The constraint's theater_ratio (0.61)
 *   reflects that much public health intervention focuses on individual
 *   behavior change and personal responsibility narratives while structural
 *   barriers to health (poverty, housing instability, food insecurity,
 *   environmental toxins, stress from discrimination) remain unaddressed or
 *   actively maintained. The extractiveness trajectory (0.35 → 0.52 over 40
 *   years) shows that as behavioral risk factor framing has become more
 *   dominant in public health discourse, the actual extraction mechanism has
 *   intensified: resources flow to individual intervention programs while
 *   structural determinants become more visible as the 'root causes' — yet
 *   structural interventions remain underfunded and deprioritized.
 *
 * KEY AGENTS:
 *   - Low-income populations: Primary victim (powerless/trapped) — bear disproportionate disease burden while lacking resources for behavior change; cannot exit structural conditions that concentrate risk factors
 *   - Marginalized communities: Primary victim (powerless/trapped) — experience compounding barriers (discrimination, historical medical trauma, residential segregation) that concentrate multiple risk factors simultaneously
 *   - Structural health equity: Abstract victim (powerless/trapped) — the collective goal of equitable health outcomes cannot organize or exit; bears the cost of individualistic framing that prevents structural change
 *   - Pharmaceutical and prevention industry: Primary beneficiary (institutional/arbitrage) — profits from coordinated vaccination campaigns, screening programs, and prevention infrastructure; alignment with genuine disease reduction
 *   - Health education systems: Secondary beneficiary (institutional/arbitrage) — benefit from behavioral risk factor framing; maintain position as legitimate authority on health behavior change
 *   - High-income populations: Secondary beneficiary (powerful/arbitrage) — have resources to modify risk factors individually; benefit first from prevention programs; less vulnerable to structural barriers
 *   - Community health workers: Secondary actor (moderate/constrained) — implement programs constrained by funding and systemic barriers but also benefit from coordination infrastructure and peer support networks
 *   - Social determinants coalition: Organized advocates (organized/constrained) — visible structural reform movements addressing root causes; building parallel pathways to health equity through housing, food security, and economic opportunity interventions
 *   - Public health establishment: Institutional actor (institutional/arbitrage) — maintains behavioral risk factor framework; sees structural approaches as outside traditional public health scope; preserves institutional consensus despite emerging evidence of structural determination
 *   - Analytical observer: Civilizational view (analytical/analytical) — risks naturalizing the distribution of health risks as inevitable feature of human societies, missing the contingent institutional choices that maintain the distribution
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(uneven_risk_distribution, 0.52).
domain_priors:suppression_score(uneven_risk_distribution, 0.68).
domain_priors:theater_ratio(uneven_risk_distribution, 0.61).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(uneven_risk_distribution, extractiveness, 0.52).
narrative_ontology:constraint_metric(uneven_risk_distribution, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(uneven_risk_distribution, theater_ratio, 0.61).

% --- Constraint claim ---
narrative_ontology:constraint_claim(uneven_risk_distribution, tangled_rope).
narrative_ontology:human_readable(uneven_risk_distribution, "Uneven Distribution of Modifiable Health Risk Factors").
narrative_ontology:topic_domain(uneven_risk_distribution, "public_health/epidemiology").

domain_priors:requires_active_enforcement(uneven_risk_distribution).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(uneven_risk_distribution, high_income_populations).
narrative_ontology:constraint_beneficiary(uneven_risk_distribution, health_education_systems).
narrative_ontology:constraint_beneficiary(uneven_risk_distribution, preventive_medicine_industry).
narrative_ontology:constraint_victim(uneven_risk_distribution, low_income_populations).
narrative_ontology:constraint_victim(uneven_risk_distribution, marginalized_communities).
narrative_ontology:constraint_victim(uneven_risk_distribution, structural_health_equity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: TRAPPED LOW-INCOME POPULATION (SNARE) — Cannot exit the structural conditions that concentrate modifiable risk factors. Lacks resources for smoking cessation, HPV vaccination, diet control, or exercise access. Bears disproportionate disease burden with no alternative exit pathway. d≈0.93, f(d)≈1.40, σ=1.2 → χ≈0.77.
constraint_indexing:constraint_classification(uneven_risk_distribution, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: COMMUNITY HEALTH WORKER (TANGLED ROPE) — Constrained by funding, training capacity, and systemic barriers but also benefits from coordination: public health campaigns, peer support networks, and disease prevention programs create legitimate collective action benefits. d≈0.68, f(d)≈1.05, σ=1.0 → χ≈0.57.
constraint_indexing:constraint_classification(uneven_risk_distribution, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: PHARMACEUTICAL PREVENTION INDUSTRY (ROPE) — Benefits from coordinated global vaccination campaigns, preventive medicine frameworks, and disease screening protocols. Extraction is minimal: industry profits from scaling prevention, and prevention genuinely reduces disease burden. d≈0.08, f(d)≈-0.11, σ=1.2 → χ≈-0.05. Negative effective extraction = net beneficiary through coordination alignment.
constraint_indexing:constraint_classification(uneven_risk_distribution, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: SOCIAL DETERMINANTS COALITION (SCAFFOLD) — Organized public health advocates, structural reform movements, and equity-focused policy organizations see the risk factor distribution as a temporary manifestation of changeable structural conditions (housing, food access, education, economic inequality) with a sunset clause. Community-based interventions, housing first policies, and food security programs create parallel pathways that bypass the individualistic risk factor frame. d≈0.35, f(d)≈0.35, σ=1.2 → χ≈0.22. Low effective extraction because coalition has agency and visibility of structural change pathways.
constraint_indexing:constraint_classification(uneven_risk_distribution, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: INDIVIDUAL BEHAVIORAL HEALTH FRAMEWORK (PITON) — The dominant public health paradigm (risk factor attribution, individual behavior change, personal responsibility narratives) is substantially performative. Its functional content (identifying modifiable factors) has atrophied; it persists through institutional inertia and funding alignment. theater_ratio=0.61 reflects that much public health messaging emphasizes personal choice while structural barriers remain unchanged. The framework persists because alternatives (structural determination, health equity reform) challenge institutional interests.
constraint_indexing:constraint_classification(uneven_risk_distribution, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a universal/civilizational perspective, the uneven distribution of health risk factors might appear as a natural law: disease follows poverty and deprivation as an inevitable feature of unequal social systems. However, the base properties (ε=0.52, suppression=0.68, theater=0.61) contradict the mountain classification — the engine will detect this as a false summit, revealing that what appears 'natural' (poverty → risk) is actually a contingent result of institutional choices about resource distribution, health access, and social organization.
constraint_indexing:constraint_classification(uneven_risk_distribution, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(uneven_risk_distribution_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(uneven_risk_distribution, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(uneven_risk_distribution, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(uneven_risk_distribution, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(uneven_risk_distribution, TR),
    TR >= 0.70.

:- end_tests(uneven_risk_distribution_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high, trending upward. The behavioral risk factor framework extracts from low-income populations by locating disease causality in individual choices (smoking, diet, exercise) while systematically suppressing visibility of structural determinants (poverty, housing instability, environmental toxins, discrimination stress). The extraction is not complete: public health does provide genuine preventive services (vaccinations, screening, health education) that reduce disease burden. However, the framing extracts by: (1) shifting responsibility for health outcomes from institutional structures to individual behavior; (2) allocating resources to individual behavior change while starving structural interventions; (3) creating moral narratives of health inequality that blame the affected populations. The 0.35→0.52 trajectory reflects increasing extraction as behavioral framing has become more hegemonic while structural barriers have widened. Suppression (0.68): High. Significant barriers to structural alternatives include: (1) institutional interests of the pharmaceutical and health education industries in the behavioral framework; (2) funding mechanisms that prioritize individual interventions; (3) professional training that emphasizes individual risk factors; (4) political economy barriers to structural interventions (housing, food security, economic equality); (5) active suppression of structural health equity frameworks in mainstream public health institutions. Theater ratio (0.61): Moderately high. Much public health intervention is performative: health campaigns emphasizing 'quit smoking, lose weight, exercise' in communities lacking safe places to walk, abundant fresh food, or employment to afford nutrition. The theater consists of individual behavior change messaging that performs health concern while structural change remains absent. Theater has increased over time as behavioral framing has deepened despite widening health disparities.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates the full range of DR classification. Low-income populations see pure extraction (Snare) — they are targeted with responsibility narratives while structural barriers remain firm. Community health workers see mixed coordination and extraction (Tangled Rope) — public health provides infrastructure and purpose but constrains work within a framework that individualizes structural problems. The pharmaceutical industry sees pure coordination (Rope) — prevention genuinely reduces disease, and industry profits align with collective health. The social determinants coalition sees a temporary problem with structural solution pathways (Scaffold) — housing first, food security, and economic opportunity programs provide exits from the behavioral framework and can sunset the risk factor distribution itself. The public health establishment sees a degraded ritual (Piton) — the behavioral framework persists through professional consensus and funding alignment despite increasingly visible evidence that it mislabels structural problems as individual failures. The civilizational analytical observer risks seeing an immutable natural law (Mountain) — disease follows poverty as an inherent feature of human society — but the structural data reveals this as a false summit: the concentration of health risks is maintained by institutional choices about resource distribution, not by laws of nature.
 *
 * DIRECTIONALITY LOGIC:
 *   Low-income populations: Victim + trapped → d≈0.93, f(d)≈1.40. Maximum extraction — lack resources to modify behaviors and cannot exit structural conditions. Marginalized communities: Victim + trapped → d≈0.93, f(d)≈1.40. Compounding victims facing multiple barriers simultaneously. Structural health equity: Victim + trapped → d≈0.95, f(d)≈1.42. Collective victim; cannot organize or exit; abstract cost-bearer. Community health workers: Victim + constrained → d≈0.68, f(d)≈1.05. Can potentially exit (change careers) but constrained by funding and institutional structure; also benefit from coordination. Pharmaceutical industry: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.11. Net beneficiary; can exit (market alternatives); profits align with disease reduction. Health education systems: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.11. Net beneficiary; maintain authority position; align with behavioral framework. High-income populations: Beneficiary + mobile → d≈0.15, f(d)≈-0.01. Can modify risk factors with resources; benefit first from prevention programs; mobile exit option. Social determinants coalition: Organized + constrained → d≈0.35, f(d)≈0.35. Organized advocates; constrained by political economy but have visibility and strategic alternatives. Public health establishment: Institutional + arbitrage → d≈0.08, f(d)≈-0.11. Piton classification derives from theater gate (0.61 ≥ 0.70 fails), not from high chi. Analytical observer: analytical → d≈0.72, f(d)≈1.15. Mountain classification is perspectival; engine's false summit detector reveals naturalization of contingent institutional distribution.
 *
 * MANDATROPHY ANALYSIS:
 *   CONSTRAINT EXEMPLAR FOR STRUCTURAL EXTRACTION MASQUERADING AS COORDINATION: This constraint resolves the mandatrophy by showing how behavioral risk factor framing commits the core mandatrophic error: it labels what is actually extraction (systematic concentration of health risks through institutional resource allocation and structural inequality) as coordination (public health infrastructure for disease prevention). The Rope perspective (pharmaceutical industry) is legitimate within its scope: vaccination campaigns and prevention programs genuinely coordinate disease reduction. The Tangled Rope perspective (community health workers) is legitimate: public health infrastructure provides both coordination benefits and real constraints. The Snare perspective (low-income populations) is the structural truth: the system extracts by locating causality in individual behavior while suppressing structural alternatives. The mandatrophy resolution requires recognizing that the same public health system is simultaneously: (1) a Rope for those with resources to respond to behavior change messaging (coordination infrastructure); (2) a Snare for those trapped in structural conditions that prevent behavior modification (extraction through responsibility shifting); (3) a Scaffold for those building structural alternatives (temporary framework with exit pathways); and (4) a Piton for the institutional establishment maintaining behavioral consensus despite evidence of its inadequacy. The false Mountain perspective naturalizes the distribution as inevitable. The engine's multi-perspectival analysis reveals that 'public health' is not a single constraint but a constraint family with different ε values depending on the structural position of the observer — and the mandatrophy resolves by showing that ALL perspectives are structurally accurate, revealing that the system is intentionally designed to appear as coordination to beneficiaries while functioning as extraction for victims.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    causality_vs_correlation,
    'Are modifiable risk factors causally responsible for disease burden disparities, or are they downstream markers of structural inequality that would persist even if individual risk factors were equalized?',
    'Comparative analysis of populations with equivalent risk factor profiles but different social determinants; controlled intervention studies addressing both risk factors and structural barriers; longitudinal tracking of risk factor changes following housing/food security interventions',
    'If risk factors are primary causes: individual behavior change programs are justified (Rope/Scaffold framing valid). If risk factors are markers of deeper structural inequality: focusing on risk factors mislabels extraction as prevention (false Rope → actual Snare).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(causality_vs_correlation, empirical, 'Whether risk factors are causal drivers or downstream markers of structural inequality').

omega_variable(
    intervention_equity_gradient,
    'Do prevention interventions (vaccination, screening, cessation programs) actually reduce disease burden disparities, or do they widen the gap by reaching high-income populations first?',
    'Time-series analysis of disease burden disparities before/after major prevention campaigns; measurement of intervention uptake curves by income quintile; comparative effectiveness in reducing absolute vs relative health inequities',
    'If interventions reduce absolute disparities: coordination frame (Rope/Tangled Rope) is accurate. If interventions widen relative disparities despite reducing absolute disease: the constraint represents extraction disguised as coordination.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(intervention_equity_gradient, empirical, 'Whether prevention interventions reduce or widen health disparities').

omega_variable(
    structural_vs_behavioral_primacy,
    'In populations where structural barriers are removed (housing security, food access, economic opportunity), do health disparities persist due to ingrained behavioral patterns, or do they resolve, indicating structural determination was primary?',
    'Natural experiments in structural reform (housing first programs, basic income pilots, food security guarantees); longitudinal health outcome tracking; behavioral pathway analysis comparing structural vs behavioral intervention effectiveness',
    'If behaviors persist despite structural change: behavioral interventions are justified (individual responsibility frame). If disparities resolve with structural change: behavioral framing naturalizes what is structurally determined (false individualism).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(structural_vs_behavioral_primacy, empirical, 'Whether health disparities persist when structural barriers are removed').

omega_variable(
    extraction_mechanism_visibility,
    'Is the suppression of structural alternatives to the individualistic risk factor frame intentional institutional extraction, or does it reflect genuine disagreement about causal mechanisms?',
    'Historical analysis of public health funding allocation and research agenda-setting; comparative visibility analysis of structural vs behavioral interventions in funding and media; institutional incentive analysis for funding bodies and professional guilds',
    'If intentional extraction: suppression (0.68) is justified; classification as Snare/Tangled Rope is correct. If genuine disagreement: suppression reflects institutional consensus rather than active coercion; classification might shift toward Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_mechanism_visibility, conceptual, 'Whether behavioral framing suppression is intentional extraction or genuine disagreement').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(uneven_risk_distribution, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(uneven_tr_t0, uneven_risk_distribution, theater_ratio, 0, 0.42).
narrative_ontology:measurement(uneven_tr_t20, uneven_risk_distribution, theater_ratio, 20, 0.54).
narrative_ontology:measurement(uneven_tr_t40, uneven_risk_distribution, theater_ratio, 40, 0.61).

% Extraction over time
narrative_ontology:measurement(uneven_be_t0, uneven_risk_distribution, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(uneven_be_t20, uneven_risk_distribution, base_extractiveness, 20, 0.48).
narrative_ontology:measurement(uneven_be_t40, uneven_risk_distribution, base_extractiveness, 40, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(uneven_risk_distribution, resource_allocation).
narrative_ontology:affects_constraint(uneven_risk_distribution, health_literacy_stratification).
narrative_ontology:affects_constraint(uneven_risk_distribution, healthcare_access_inequality).
narrative_ontology:affects_constraint(uneven_risk_distribution, disease_burden_attribution).
narrative_ontology:affects_constraint(uneven_risk_distribution, behavioral_public_health_hegemony).

% DUAL FORMULATION NOTE:
% The uneven distribution constraint is downstream of systemic inequality (poverty, discrimination, resource scarcity) and upstream of specific behavioral interventions (smoking cessation, diet modification, vaccination campaigns). Related constraints include health literacy stratification (ε≈0.38, cognitive access barriers), healthcare access inequality (ε≈0.65, direct access barriers), disease burden attribution (ε≈0.45, epistemic extraction through causal framing), and behavioral public health hegemony (ε≈0.42, institutional dominance of individualistic paradigm). The network decomposition distinguishes the structural fact (uneven distribution of risk factors) from the institutional response (behavioral framing that extracts through responsibility shifting).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(uneven_risk_distribution, institutional, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
