% ============================================================================
% CONSTRAINT STORY: cross_group_economic_mobility
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_cross_group_economic_mobility, []).

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
 *   constraint_id: cross_group_economic_mobility
 *   human_readable: Cross-Group Economic Mobility Constraint
 *   domain: economic/social
 *
 * SUMMARY:
 *   Cross-group economic mobility constraints create a structural tension
 *   between the coordination function of credential systems and labor market
 *   sorting, and the asymmetric extraction of upward mobility barriers
 *   concentrated on lower-income and structurally excluded populations. The
 *   constraint operates through multiple reinforcing mechanisms: tuition
 *   inflation and student debt, credential inflation (requiring ever-higher
 *   credentials for equivalent positions), geographic immobility due to
 *   housing costs and job geography, social network entrenchment, and the
 *   inheritance of both financial capital and cultural/social capital. The
 *   meritocratic ideology that 'anyone can rise through effort' serves as the
 *   primary theatrical function, legitimating the barrier system while
 *   suppressing recognition that mobility barriers are distributed unequally
 *   by family origin. Base extractiveness has increased from 0.35 to 0.58
 *   over the 30-year interval, driven by credential and housing inflation
 *   outpacing wage growth. Theater ratio has risen from 0.40 to 0.55,
 *   reflecting increasing emphasis on meritocratic narratives and diversity
 *   messaging despite stagnating actual mobility rates. The constraint
 *   exhibits all characteristics of a Tangled Rope: genuine coordination
 *   function (credential sorting, labor market efficiency, social stability)
 *   embedded within asymmetric extraction (barriers designed to concentrate
 *   mobility benefits upward). Active institutional enforcement through
 *   credential gatekeeping, credit access control, housing policy, and labor
 *   market stratification maintains the constraint.
 *
 * KEY AGENTS:
 *   - Structurally Excluded Populations: Primary victims (powerless/trapped) — confined to low-mobility segments; bear extraction through debt, credential barriers, geographic immobility, and network exclusion
 *   - Lower-Income Group Members: Affected populations (moderate/constrained) — can achieve upward mobility at high cost (debt, delayed family formation, relocation); structurally disadvantaged in credential and capital access
 *   - Incumbent Economic Elites: Primary beneficiaries (institutional/arbitrage) — benefit from constrained mobility that protects wealth inheritance and maintains elite network value
 *   - Credential Gatekeepers: Institutional beneficiaries (institutional/arbitrage) — universities, licensing bodies, certification systems; capture revenue and prestige through credential inflation and scarcity
 *   - Labor Rights Coalition: Organized agents (organized/constrained) — unions, civil rights organizations, education advocates; have agency to shift constraints through policy and collective action
 *   - Financial System Intermediaries: Secondary beneficiaries (institutional/arbitrage) — capture wealth through student debt, mortgage lending structured by creditworthiness tied to family background
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — sees constraint as deliberately maintained institutional structure balancing genuine coordination against extractive revenue capture
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(cross_group_economic_mobility, 0.58).
domain_priors:suppression_score(cross_group_economic_mobility, 0.62).
domain_priors:theater_ratio(cross_group_economic_mobility, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(cross_group_economic_mobility, extractiveness, 0.58).
narrative_ontology:constraint_metric(cross_group_economic_mobility, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(cross_group_economic_mobility, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(cross_group_economic_mobility, tangled_rope).
narrative_ontology:human_readable(cross_group_economic_mobility, "Cross-Group Economic Mobility Constraint").
narrative_ontology:topic_domain(cross_group_economic_mobility, "economic/social").

domain_priors:requires_active_enforcement(cross_group_economic_mobility).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(cross_group_economic_mobility, incumbent_economic_elites).
narrative_ontology:constraint_beneficiary(cross_group_economic_mobility, credential_gatekeepers).
narrative_ontology:constraint_victim(cross_group_economic_mobility, lower_income_group_members).
narrative_ontology:constraint_victim(cross_group_economic_mobility, structurally_excluded_populations).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: STRUCTURALLY EXCLUDED WORKER (SNARE) — No viable exit from low-income constraint. Faces compounding barriers: limited access to quality education, geographic immobility due to cost, credential requirements that require prior capital investment, and social networks that don't connect to opportunity. Experiences extraction as complete: resources flow upward through tuition, licensing fees, and unequal access to capital, with minimal opportunity for escape. Zero degrees of freedom within the constraint.
constraint_indexing:constraint_classification(cross_group_economic_mobility, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: ASPIRING UPWARDLY MOBILE AGENT (TANGLED ROPE) — Constrained by high costs and systemic barriers but can exit at significant personal cost (debt, delayed family formation, geographic relocation, intensive credentialing). Benefits from coordination mechanisms (public education, labor market structure, credit availability) while simultaneously bearing extraction through tuition inflation, credential inflation, and opportunity cost. Mixed experience of both genuine mobility pathways and asymmetric cost allocation.
constraint_indexing:constraint_classification(cross_group_economic_mobility, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: CREDENTIAL INSTITUTION (ROPE) — Universities and credentialing bodies experience the constraint as pure coordination: sorting labor market candidates, certifying competence, creating signaling mechanisms. Net beneficiary through tuition revenue, endowment support, and institutional prestige. Can arbitrage between high demand and limited supply. Experiences minimal suppression — institution has full agency to set policies and requirements.
constraint_indexing:constraint_classification(cross_group_economic_mobility, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: ECONOMIC ELITE BENEFICIARY (ROPE) — Incumbent wealth and elite networks benefit from constrained mobility: children inherit advantages without competition, credential value remains high, labor pools remain stratified by origin rather than merit. Experiences constraint as pure coordination (maintaining social stability through managed mobility lanes) with no suppression — full agency and arbitrage options.
constraint_indexing:constraint_classification(cross_group_economic_mobility, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: LABOR RIGHTS COALITION (TANGLED ROPE) — Organized agents (unions, civil rights organizations, education advocacy groups) see both coordination function (workplace protections, credential standardization enabling broad hiring) and asymmetric extraction (mobility barriers concentrated on lower-income groups). Have agency to negotiate but constrained by political economy. Moderate extraction because organization provides exit pathways and can shift the constraint through policy.
constraint_indexing:constraint_classification(cross_group_economic_mobility, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: MERITOCRATIC IDEOLOGY (PITON) — The institutional narrative that 'opportunity exists for all who work hard' persists as largely performative theater despite structural barriers. Credential inflation, wealth inheritance, and social capital concentration have degraded the mechanism's actual function. The ideology serves primarily to justify existing inequality and suppress dissent, with minimal honest meritocratic sorting. Theater ratio high because the constraint's primary function is now narrative maintenance rather than actual mobility provision.
constraint_indexing:constraint_classification(cross_group_economic_mobility, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (CIVILIZATIONAL) — From a civilizational perspective, the constraint exhibits genuine coordination function (labor market sorting, skill certification, social stability management) embedded within asymmetric extraction (barriers designed into credential and capital access systems, inheritance of advantage, geographic and social network entrenchment). The constraint is not a natural law but a deliberately maintained institutional structure balancing coordination benefit against extractive revenue. Analytically, this is the core structure: who benefits from limiting mobility, who enforces those limits, and how coordination serves as the legitimating cover for extraction.
constraint_indexing:constraint_classification(cross_group_economic_mobility, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(cross_group_economic_mobility_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(cross_group_economic_mobility, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(cross_group_economic_mobility, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(cross_group_economic_mobility, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(cross_group_economic_mobility, TR),
    TR >= 0.70.

:- end_tests(cross_group_economic_mobility_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. The constraint extracts through multiple channels: credential inflation creates tuition revenue streams and credential scarcity rents; geographic immobility enables local wage suppression and housing cost capture; social network entrenchment enables insider hiring and wage compression for outsiders. Extraction is not total (mobility occurs, though at high cost) but is substantial and concentrated. The progression from 0.35 to 0.58 reflects acceleration of credential inflation and housing cost growth outpacing wage growth — the machinery of extraction has been tightened. Suppression (0.62): Moderate-high. Barriers to upward mobility are significant: tuition debt, credential requirements, geographic relocation costs, discrimination and bias in hiring, and weak information flows about opportunity. But suppression is not total — public education exists, some mobility pathways operate, and some institutional reforms have reduced barriers. Theater ratio (0.55): Moderate. The meritocratic narrative ('anyone can rise through effort and merit') is substantially performative given the empirical concentration of mobility barriers by family origin. However, some genuine meritocratic sorting occurs alongside the inherited advantage laundering, so theater is elevated but not dominant. The rise to 0.55 reflects intensification of diversity and meritocracy messaging even as actual mobility has stagnated.
 *
 * PERSPECTIVAL GAP:
 *   This constraint's perspectival gap reveals the machinery of legitimation. The beneficiary perspectives see no extraction at all — they see a coordination mechanism that efficiently sorts talent and maintains economic incentives. The victim perspectives see pure extraction with no coordination benefit. The meritocratic ideology perspective sees the constraint as having degraded into theater, maintained through narrative rather than function. The analytical observer sees the full structure: the constraint is deliberately designed to provide coordination benefits (labor market sorting, credential signaling) while distributing those benefits unequally and extracting asymmetrically from those with the least structural power. The gap between 'this is pure coordination' (elite/institutional perspectives) and 'this is pure extraction' (powerless perspectives) is the diagnostic signal that legitimacy is doing work — the same constraint is narratively reconstructed completely differently depending on whether you benefit from it.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) is derived from structural position: beneficiaries of the constraint (elites, credential institutions, financial intermediaries) have low d (0.05-0.20), experiencing the constraint as beneficial or neutral. Victims (lower-income populations, excluded groups) have high d (0.80-0.95), bearing maximum extraction. Moderate agents (aspiring mobile workers) occupy the middle range (d ≈ 0.55-0.65), experiencing mixed costs and benefits. The organized labor coalition has moderate d (0.45-0.55) because organization provides agency and exit options. The analytical observer has high d (0.72) but with full information, making the constraint transparent rather than binding. The sigmoid f(d) function maps these d values to effective power modifiers: beneficiaries get negative χ (constraint subsidizes them), victims get maximum χ (extraction runs fully toward them), moderate agents get positive χ (moderate experienced extraction). Scope modifier σ(national) = 1.0 applies unchanged across all perspectives because the constraint operates nationally. The derived χ values support the Tangled Rope classification: enough beneficiary coordination (reducing effective extraction) that some perspectives see Rope, enough victim extraction that the powerless perspective sees Snare, and the overall structure shows both functions simultaneously.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by recognizing that the constraint has BOTH genuine coordination function AND asymmetric extraction, and that the meritocratic ideology serves to obscure this hybrid structure. The coordination function is real: credentials do carry information about ability, labor markets do become more efficient when sorted, and some degree of barrier to entry is necessary for credentialing to have meaning. The extraction is equally real: barriers are concentrated on lower-income origins through multiple mechanisms (tuition debt, geographic immobility, network exclusion, discrimination), inheritance of advantage shields elites from competition, and the entire system is actively maintained through institutional enforcement of credential requirements and credential inflation. The meritocratic ideology ('anyone can rise through effort') prevents recognition that extraction is concentrated — it naturalizes barriers as legitimate ('high standards require high barriers') and suggests that those who don't rise simply didn't try hard enough. This is the classical mandatrophy resolution: the constraint is Tangled Rope, genuinely hybrid, and the theatrical elevation of meritocratic narrative to piton-level (0.55 and rising) is what prevents recognition of the asymmetric extraction and enables the system to persist despite measurable mobility stagnation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    meritocracy_measurement_ambiguity,
    'To what degree is measured mobility actually meritocratic sorting versus inherited advantage laundering?',
    'Longitudinal tracking of intergenerational mobility by family wealth, parental education, and social network access; controlled comparison of mobility rates for identical credentials by family origin; analysis of how much mobility is credential-driven versus capital/network-driven.',
    'If meritocratic sorting dominates: constraint is primarily Rope with embedded Scaffold (temporary barriers). If inheritance dominates: constraint is primarily Snare with theatrical meritocratic cover (extractive system masquerading as coordination).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(meritocracy_measurement_ambiguity, empirical, 'Degree to which measured mobility reflects meritocratic sorting versus inheritance effects').

omega_variable(
    credential_inflation_driver,
    'Is credential inflation driven by legitimate skill requirements or by competitive signaling dynamics and institutional rent-seeking?',
    'Skill requirements analysis for entry-level positions over time; correlation between credential inflation and actual job task complexity; comparison of credential requirements across firms with similar productivity metrics; analysis of credential inflation acceleration coinciding with institution revenue pressures.',
    'If skill-driven: credential barriers are legitimate coordination mechanisms (Rope perspective strengthened). If signaling/rent-seeking driven: credential barriers are extraction mechanisms (Snare perspective strengthened).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(credential_inflation_driver, empirical, 'Whether credential inflation reflects skill requirements or competitive signaling').

omega_variable(
    social_capital_transmission_mechanism,
    'How much of differential mobility is explained by inherited social networks versus measurable skill differences?',
    'Network analysis of hiring patterns; tracking of identical-credential applicants by family origin; analysis of ''weak tie'' network strength by socioeconomic origin; study of how much information about opportunities flows through family versus public channels.',
    'If networks dominate: social capital inheritance is a primary extraction mechanism, making the constraint structurally Snare for those without inherited access. If skills dominate: the constraint functions more as Rope or Tangled Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(social_capital_transmission_mechanism, empirical, 'Relative importance of inherited social networks versus measurable skills in mobility outcomes').

omega_variable(
    institutional_reform_capacity,
    'Can credential institutions meaningfully reduce mobility barriers without losing signaling and sorting functions?',
    'Historical analysis of credential institution reforms (open admissions, sliding-scale tuition, alternative credentialing); measurement of sorting quality post-reform; identification of which institutional changes maintained coordination function while reducing barriers.',
    'If high reform capacity: the constraint contains Scaffold elements (solvable with policy change). If low reform capacity: barriers are structurally entrenched (Snare or Piton).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_reform_capacity, empirical, 'Whether credential institutions can reduce barriers while maintaining sorting function').

omega_variable(
    geographic_mobility_constraint_source,
    'Are geographic mobility barriers primarily economic (housing costs, job density) or social (network entrenchment, cultural capital)?',
    'Analysis of migration patterns by income level; study of cost-benefit ratios for relocation by origin; comparison of success rates for geographically mobile versus origin-bound agents; identification of information barriers and uncertainty in geographic mobility decisions.',
    'If economic barriers dominate: policy can address through housing and infrastructure. If social barriers dominate: constraint requires cultural/network intervention and has higher structural stickiness.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(geographic_mobility_constraint_source, empirical, 'Primary source of geographic immobility: economic versus social barriers').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(cross_group_economic_mobility, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cgem_tr_t0, cross_group_economic_mobility, theater_ratio, 0, 0.4).
narrative_ontology:measurement(cgem_tr_t15, cross_group_economic_mobility, theater_ratio, 15, 0.48).
narrative_ontology:measurement(cgem_tr_t30, cross_group_economic_mobility, theater_ratio, 30, 0.55).
narrative_ontology:measurement(cgem_tr_t5, cross_group_economic_mobility, theater_ratio, 5, 0.43).

% Extraction over time
narrative_ontology:measurement(cgem_be_t0, cross_group_economic_mobility, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(cgem_be_t15, cross_group_economic_mobility, base_extractiveness, 15, 0.48).
narrative_ontology:measurement(cgem_be_t30, cross_group_economic_mobility, base_extractiveness, 30, 0.58).
narrative_ontology:measurement(cgem_be_t5, cross_group_economic_mobility, base_extractiveness, 5, 0.4).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(cross_group_economic_mobility, resource_allocation).
narrative_ontology:affects_constraint(cross_group_economic_mobility, wealth_inheritance).
narrative_ontology:affects_constraint(cross_group_economic_mobility, education_access).
narrative_ontology:affects_constraint(cross_group_economic_mobility, housing_affordability).
narrative_ontology:affects_constraint(cross_group_economic_mobility, labor_market_segmentation).

% DUAL FORMULATION NOTE:
% Cross-group economic mobility is a meta-constraint affected by and affecting four domain-specific constraints: wealth inheritance (intergenerational capital transmission), education access (credential system barriers), housing affordability (geographic immobility driver), and labor market segmentation (wage compression for outsiders). Each has its own epsilon value reflecting specific extraction mechanisms. This story captures the structural coordination-extraction hybrid at the level of the overall system; domain-specific stories capture mechanism-specific details.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(cross_group_economic_mobility, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
