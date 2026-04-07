% ============================================================================
% CONSTRAINT STORY: sotu_1967_johnson_war_on_poverty_mechanism
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sotu_1967_johnson_war_on_poverty_mechanism, []).

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
 *   constraint_id: sotu_1967_johnson_war_on_poverty_mechanism
 *   human_readable: War on Poverty: Multi-Agency Coordination and Resource Redistribution (1967)
 *   domain: social_policy/education/economic
 *
 * SUMMARY:
 *   The War on Poverty represents a pivotal federal intervention that
 *   reframes poverty as a policy problem addressable through coordinated
 *   multi-agency intervention (OEO, HEW, Labor, HUD) rather than as an
 *   individual moral failing. Head Start exemplifies this reframing: early
 *   childhood education becomes the mechanism through which poverty is
 *   dissolved, with the theory that skills and educational attainment create
 *   pathways to employment and self-sufficiency. The constraint
 *   simultaneously performs genuine coordination (pooling federal resources,
 *   aligning incentives across agencies, standardizing measurement) and
 *   embedding structural extraction: the program redefines poverty as an
 *   individual deficiency problem requiring behavioral compliance, obscuring
 *   structural mechanisms (housing segregation, employment discrimination,
 *   wealth extraction) that reproduce poverty across generations. The
 *   constraint's theater ratio reflects this: as evidence accumulates that
 *   education-focused interventions without structural change produce limited
 *   poverty exit, the performative element increases — the program ritualizes
 *   poverty-as-deficiency through mandatory participation and compliance
 *   metrics, even as its core function (reducing poverty through skills
 *   training) attenuates.
 *
 * KEY AGENTS:
 *   - Low-income families and urban children: Primary beneficiary (powerless/trapped) — receive educational access and job training; bear costs of mandatory participation, behavioral compliance, and exposure to deficit framing
 *   - Structural poverty reproduction mechanisms: Primary victim (analytical/trapped) — employment discrimination, housing segregation, intergenerational wealth inequality remain unaddressed; continue operating beneath program visibility
 *   - Community organizations and local advocates: Secondary actor (moderate/constrained) — provide critical coordination function (family identification, cultural translation, peer support); constrained by federal funding dependence and top-down program design
 *   - Federal anti-poverty bureaucracy (OEO, HEW): Institutional beneficiary (institutional/arbitrage) — captures organizational mandate, budget growth, administrative capacity; coordinates across agencies
 *   - Educational establishment (schools, universities, training contractors): Institutional beneficiary (institutional/arbitrage) — expands scope, secures public funding, gains authority over poverty intervention; coordinates curriculum and training standards
 *   - Civil rights organizations (NAACP, CORE, SNCC): Organized agent (organized/constrained) — leverage program for coalition building and demands for structural change; constrained by federal co-optation and incremental policy direction
 *   - Moral-individual-responsibility narrative: Institutional mechanism (institutional/arbitrage) — perpetuates through program design; benefits those who profit from individualized compliance focus; maintains power asymmetry by preventing structural attribution
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sotu_1967_johnson_war_on_poverty_mechanism, 0.38).
domain_priors:suppression_score(sotu_1967_johnson_war_on_poverty_mechanism, 0.52).
domain_priors:theater_ratio(sotu_1967_johnson_war_on_poverty_mechanism, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sotu_1967_johnson_war_on_poverty_mechanism, extractiveness, 0.38).
narrative_ontology:constraint_metric(sotu_1967_johnson_war_on_poverty_mechanism, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(sotu_1967_johnson_war_on_poverty_mechanism, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sotu_1967_johnson_war_on_poverty_mechanism, tangled_rope).
narrative_ontology:human_readable(sotu_1967_johnson_war_on_poverty_mechanism, "War on Poverty: Multi-Agency Coordination and Resource Redistribution (1967)").
narrative_ontology:topic_domain(sotu_1967_johnson_war_on_poverty_mechanism, "social_policy/education/economic").

domain_priors:requires_active_enforcement(sotu_1967_johnson_war_on_poverty_mechanism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sotu_1967_johnson_war_on_poverty_mechanism, low_income_families).
narrative_ontology:constraint_beneficiary(sotu_1967_johnson_war_on_poverty_mechanism, urban_children).
narrative_ontology:constraint_beneficiary(sotu_1967_johnson_war_on_poverty_mechanism, federal_bureaucracies).
narrative_ontology:constraint_beneficiary(sotu_1967_johnson_war_on_poverty_mechanism, education_contractors).
narrative_ontology:constraint_victim(sotu_1967_johnson_war_on_poverty_mechanism, structural_poverty_reproduction).
narrative_ontology:constraint_victim(sotu_1967_johnson_war_on_poverty_mechanism, intersectional_exclusion_mechanisms).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: GHETTO RESIDENT (SNARE) — Trapped in geographic, economic, and social isolation. Head Start and job training are necessary but insufficient given systemic barriers: housing segregation, employment discrimination, community disinvestment. The constraint extracts behavioral compliance (parent participation, child compliance, skills acquisition) without addressing the structural mechanisms that reproduce poverty. Exit requires abandoning community networks or accepting poverty. Experienced extraction is high — maximum suppression.
constraint_indexing:constraint_classification(sotu_1967_johnson_war_on_poverty_mechanism, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: URBAN COMMUNITY ORGANIZATION (TANGLED ROPE) — Community groups provide genuine coordination function (identifying families, translating bureaucracy, organizing peer support) while bearing substantial extraction: dependent on federal funding, subject to compliance audits, constrained in advocacy capacity. Benefits from program visibility and resources; constrained by top-down design and limited autonomy over implementation. Mixed experience: real coordination plus real asymmetry.
constraint_indexing:constraint_classification(sotu_1967_johnson_war_on_poverty_mechanism, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: FEDERAL ANTI-POVERTY BUREAUCRACY (ROPE) — Coordinates multi-agency implementation (OEO, HEW, Labor Department, HUD). Benefits from program expansion, budget growth, organizational mandate clarity. Experiences constraint as pure coordination: routing resources, aligning incentives, standardizing metrics across agencies. Net beneficiary with arbitrage options — can shift emphasis across programs, secure funding, expand staff. Extraction runs toward this institution.
constraint_indexing:constraint_classification(sotu_1967_johnson_war_on_poverty_mechanism, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: EDUCATIONAL ESTABLISHMENT (ROPE) — Head Start expands the education sector's scope, justifies federal investment in early childhood, creates institutional legitimacy for schools as poverty-intervention sites. Teachers and administrators benefit from program resources and expanded professional authority. Experiences constraint as coordination: aligning curriculum standards, training staff, managing enrollment. Net beneficiary with exit options — can negotiate scope, redefine mission, capture funding streams. Performs coordination function effectively.
constraint_indexing:constraint_classification(sotu_1967_johnson_war_on_poverty_mechanism, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: CIVIL RIGHTS COALITION (SCAFFOLD) — Organized agents (NAACP, CORE, SNCC) lobby for targeted poverty programs as temporary scaffolding toward structural civil rights gains. See the constraint as having sunset logic: increased access to early education and job training build coalition power and economic leverage that enable demands for deeper structural change (housing desegregation, employment discrimination laws, wealth redistribution). Constrained by federal resistance to radical demands but motivated by exit vision. Low effective extraction because coalition perceives pathway toward deconstruction.
constraint_indexing:constraint_classification(sotu_1967_johnson_war_on_poverty_mechanism, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: MORAL NARRATIVE (PITON) — The constraint embeds and perpetuates the framing that poverty results from individual deficiency (lack of education, work ethic, parenting skills) rather than structural extraction. This narrative has high theater: the program ritualizes poverty-as-deficiency through mandatory participation, standardized curricula, behavioral compliance metrics. The theater persists through institutional inertia despite mounting evidence that individual skills training without structural change does not reliably lift families from poverty. The function (individual behavior modification) has atrophied; the ritual (education-fixes-poverty) persists. Theater ratio reflects this degradation.
constraint_indexing:constraint_classification(sotu_1967_johnson_war_on_poverty_mechanism, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (MOUNTAIN CANDIDATE) — From a civilizational perspective, multi-generational poverty reduction requires extended coordination across education, employment, housing, and health systems. Some coordination costs are structural and irreducible. However, declaring this as mountain naturalizes what is contingent: the specific form of coordination (federal bureaucracy, standardized curricula, behavioral compliance) is not inevitable, and the reframing of poverty from structural to individual is choice, not law. False-summit candidate: identifies beneficiaries (federal agencies, educational establishment) who benefit from naturalizing the current coordination form.
constraint_indexing:constraint_classification(sotu_1967_johnson_war_on_poverty_mechanism, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sotu_1967_johnson_war_on_poverty_mechanism_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(sotu_1967_johnson_war_on_poverty_mechanism, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(sotu_1967_johnson_war_on_poverty_mechanism, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(sotu_1967_johnson_war_on_poverty_mechanism, TR),
    TR >= 0.70.

:- end_tests(sotu_1967_johnson_war_on_poverty_mechanism_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The constraint exhibits genuine multi-agency coordination (pooling federal resources, aligning incentives across departments, standardizing early education nationally) alongside meaningful asymmetry. Low-income families and communities bear behavioral compliance costs and exposure to deficit framing without guaranteed poverty exit. The federal bureaucracy and educational establishment capture organizational growth and institutional legitimacy. The extractiveness is not maximal (0.46+) because the program delivers real educational access and job training resources; it is not minimal (≤0.25) because structural mechanisms reproducing poverty remain unaddressed and program success becomes dependent on individual-level compliance rather than structural change. Suppression (0.52): Moderate-high. Multiple barriers constrain meaningful program exit and structural change: geographic segregation limits employment access, discrimination mechanisms operate outside program scope, intergenerational wealth inequality persists, and federal design constrains local experimentation. Community organizations are dependent on federal funding and cannot easily advocate for structural alternatives. Individual participants face constraints: mandatory participation, credential requirements, geography-based opportunity gaps. Theater ratio (0.58): Moderate-high. As the program matures, the theater increases — compliance metrics, curriculum standardization, and performance indicators proliferate, while evidence accumulates that education-only interventions without structural change produce limited sustained poverty exit. The program ritualizes poverty-as-individual-deficiency even as structural mechanisms continue producing poverty.
 *
 * PERSPECTIVAL GAP:
 *   The constraint produces maximally divergent classifications from identical structural data because agent power, exit options, and structural relationship to the extraction mechanism differ radically. The powerless trapped agent sees snare. The institutional arbitrage agent sees rope. The organized constrained agent sees scaffold and sunset possibility. The institutional narrative sees piton and degraded theater. The analytical observer risks mountain and false naturalization. This is the constraint story exemplifying how a single institutional design produces six distinct but equally valid experiential classifications depending on structural position.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality derives from structural position: who benefits from the current institutional form and who bears costs. Low-income families occupy victim + trapped position: they need the educational resources (net benefit from services) but remain trapped by structural barriers the program does not address (net cost from unaddressed mechanisms). The federal bureaucracy occupies beneficiary + arbitrage position: derives organizational growth and mandate clarity from program expansion; can exit or redirect resources based on political opportunity. The educational establishment occupies beneficiary + arbitrage: gains institutional scope and public legitimacy; can negotiate terms and curriculum. The community organization occupies mixed position: provides coordination function (genuine benefit to community) but constrained by funding dependence and top-down design (asymmetric extraction). The civil rights coalition occupies constrained + aspirational position: sees program as resource for building power but faces risk of political co-optation. The moral-responsibility narrative occupies pure-extraction institutional beneficiary position: maintains power asymmetry by naturalizing structural inequality as individual failing.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy in this constraint is NOT resolved. The tension between the program's coordination function (genuine multi-agency resource pooling and early education delivery) and its extractive asymmetry (structural poverty mechanisms remain unaddressed, low-income families bear behavioral compliance costs, federal and educational institutions capture organizational growth) persists unresolved. The constraint cannot be classified as pure rope (coordination) because the benefits are asymmetrically distributed and structural poverty reproduction continues. It cannot be classified as pure snare (extraction) because genuine coordination and educational access occur. It remains genuinely tangled: coordination and extraction are structurally intertwined. The resolution would require either (a) adding structural interventions (housing desegregation, employment discrimination enforcement, wealth redistribution) that would increase the constraint's complexity but reduce asymmetry, or (b) decoupling the federal multi-agency form from the individual-responsibility framing, enabling community control and structural attribution. Without these changes, the constraint will likely persist and degrade: theater ratio will increase as performative compliance metrics accumulate, extractiveness may remain stable or rise as the program becomes institutionalized, and the organizing coalition will face increasing co-optation pressure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    structural_vs_individual_framing,
    'Is poverty fundamentally a structural problem (housing segregation, employment discrimination, wealth extraction, unequal opportunity) or an individual problem (insufficient skills, education, motivation)?',
    'Longitudinal outcome tracking: compare poverty exit rates for Head Start participants to control groups; decompose outcomes by structural factors (housing access, employment opportunity, family wealth) vs individual factors (education attainment, work hours, family stability); test whether structural interventions (housing desegregation, discrimination law enforcement) produce larger effects than individual-focused interventions',
    'If structural: program reframes the problem without solving it — extractive constraint (Snare). If individual: program addresses root causes — genuine rope or scaffold. If both: current program underspecifies the structural component needed for meaningful poverty reduction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(structural_vs_individual_framing, empirical, 'Whether poverty is fundamentally structural or individual problem').

omega_variable(
    multi_generation_transmission_mechanism,
    'Does educational attainment (Head Start participation, skills training) produce sustained poverty exit across generations, or does poverty reproduction operate through mechanisms largely orthogonal to education (housing segregation, employment discrimination, intergenerational wealth)?',
    'Multi-cohort longitudinal analysis: track Head Start participants and non-participants across 30+ years; measure household wealth, intergenerational mobility, housing stability, employment stability; decompose variance by education, race-specific discrimination, wealth inheritance, housing access',
    'If education dominates: program''s theory of change is correct and extractiveness should be lower. If orthogonal mechanisms dominate: program theater is high — it addresses the visible individual domain while structural reproduction continues unaffected.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(multi_generation_transmission_mechanism, empirical, 'Whether education produces sustained poverty exit or structural reproduction persists').

omega_variable(
    resource_adequacy_and_extraction_sharing,
    'Is the constraint''s extraction burden distributed proportionally to capacity to bear it, or does it extract disproportionately from those with lowest capacity (the poor themselves)?',
    'Cost-burden analysis: trace federal appropriations, administrative overhead, contractor profits, opportunity costs of parent participation (wage loss, childcare logistics); measure who bears these costs vs who captures benefits; decompose by race, income, location',
    'If burden borne by federal capacity: constraint is tangled rope with justifiable extraction cost. If burden borne by poor through forgone wages and displaced community time: constraint is snare with exploitative extraction masquerading as support.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(resource_adequacy_and_extraction_sharing, empirical, 'Whether extraction burden is proportional to capacity to bear').

omega_variable(
    federal_mandate_autonomy_vs_coercion,
    'Do federal standards (curriculum, assessment, compliance metrics) constitute necessary coordination for resource pooling, or do they function as coercive standardization that suppresses local experimentation and autonomy?',
    'Comparison of outcomes for programs with high vs low federal specification; tracking of suppressed local initiatives; analysis of variance in implementation; measurement of community satisfaction with program design autonomy',
    'If coordination: suppression metric may be overstated — some constraints are legitimate cost of collective action. If coercion: suppression is inherent to the constraint design, not incidental.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(federal_mandate_autonomy_vs_coercion, empirical, 'Whether federal standards are coordination or coercion').

omega_variable(
    sunset_plausibility_and_coalition_power,
    'Can the civil rights coalition actually leverage poverty program success into structural demands (desegregation, discrimination law, wealth redistribution), or does the program co-opt organizational energy into managed incrementalism that forecloses more radical demands?',
    'Historical analysis of coalition trajectory: measure organizational resources devoted to radical structural demands vs incremental program expansion; track political capital deployment; analyze whether program success increased or decreased coalition leverage for systemic change',
    'If leverage increases: scaffold perspective is structural, sunset is plausible. If co-optation succeeds: scaffold is aspirational cover for snare — program absorbs radical energy and channelizes it into bureaucratic endpoints.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sunset_plausibility_and_coalition_power, empirical, 'Whether poverty programs enable or co-opt radical structural demands').

omega_variable(
    false_summit_natural_law_status,
    'Is the current form of multi-agency poverty coordination inevitable and unchangeable, or is it contingent institutional design that benefits specific federal and educational actors?',
    'Comparative institutional analysis: study poverty reduction in alternative institutional structures (decentralized community control, wealth redistribution, reparations-based approaches); test whether different coordination forms produce substantially different outcomes and benefit distributions',
    'If inevitable: mountain classification is justified. If contingent: false summit — the constraint naturalizes a particular institutional arrangement that benefits federal bureaucracies and educational establishments at the expense of structural poverty reduction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(false_summit_natural_law_status, conceptual, 'Whether multi-agency coordination form is inevitable or contingent').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sotu_1967_johnson_war_on_poverty_mechanism, 0, 5).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(wop_tr_t0, sotu_1967_johnson_war_on_poverty_mechanism, theater_ratio, 0, 0.42).
narrative_ontology:measurement(wop_tr_t2, sotu_1967_johnson_war_on_poverty_mechanism, theater_ratio, 2, 0.5).
narrative_ontology:measurement(wop_tr_t5, sotu_1967_johnson_war_on_poverty_mechanism, theater_ratio, 5, 0.58).

% Extraction over time
narrative_ontology:measurement(wop_be_t0, sotu_1967_johnson_war_on_poverty_mechanism, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(wop_be_t2, sotu_1967_johnson_war_on_poverty_mechanism, base_extractiveness, 2, 0.3).
narrative_ontology:measurement(wop_be_t5, sotu_1967_johnson_war_on_poverty_mechanism, base_extractiveness, 5, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sotu_1967_johnson_war_on_poverty_mechanism, resource_allocation).
narrative_ontology:affects_constraint(sotu_1967_johnson_war_on_poverty_mechanism, educational_credentialism_gatekeeping).
narrative_ontology:affects_constraint(sotu_1967_johnson_war_on_poverty_mechanism, employment_discrimination_housing_segregation).

% DUAL FORMULATION NOTE:
% The War on Poverty constraint is upstream of specific educational and employment barriers, but represents a distinct structural coordination-extraction mechanism. The constraint coordinates federal resources and multi-agency implementation while embedding asymmetry through individual-responsibility framing and structural poverty reproduction. Downstream constraints inherit this framing: credentialism becomes the mechanism for poverty exit (rather than addressing discrimination), housing segregation persists beneath educational access programs.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(sotu_1967_johnson_war_on_poverty_mechanism, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
