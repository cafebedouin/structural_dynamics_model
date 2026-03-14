% ============================================================================
% CONSTRAINT STORY: downward_mobility_precarity
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_downward_mobility_precarity, []).

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
 *   constraint_id: downward_mobility_precarity
 *   human_readable: Downward Mobility Precarity: The Fear-Based Constraint on Labor Exit
 *   domain: economic/social/psychological
 *
 * SUMMARY:
 *   Downward mobility precarity is the fear-based constraint system that
 *   locks workers into exploitative arrangements by making the alternative
 *   (downward status shift, identity loss, material deprivation) appear worse
 *   than the current constraint. The constraint operates simultaneously at
 *   multiple scales: individual (biographical precarity anxiety), household
 *   (debt obligations and middle-class status defense), organizational
 *   (employer workforce discipline), sectoral (labor market segmentation),
 *   and civilizational (naturalization of precarity as economic law). Unlike
 *   a simple extraction mechanism, precarity works through prospective
 *   loss-aversion: workers suppress their actual preferences and mobility
 *   because they fear future states more than they value current
 *   alternatives. This generates a snare-like structure for powerless and
 *   constrained agents (trapped by fear and material dependency) while
 *   appearing as coordination to beneficiaries (employers solving workforce
 *   flexibility, financial sector solving capital allocation). The constraint
 *   exhibits rising extractiveness (0.35 → 0.58 over 50 years) and theater
 *   ratio (0.38 → 0.55), indicating both intensifying extraction and
 *   increasing performative maintenance (CEO rhetoric about
 *   'entrepreneurship,' 'meritocracy,' 'disruption'). The theater growth
 *   without functional improvement (precarity fails to generate productivity)
 *   suggests piton characteristics (institutional inertia) at civilizational
 *   scale even as snare characteristics dominate at biographical scale.
 *
 * KEY AGENTS:
 *   - Precarious Workers: Primary victim (powerless/trapped) — bears full suppression and extraction through wage suppression, benefit denial, and constant status anxiety
 *   - Middle-Class Households: Secondary victim (moderate/constrained) — trapped by debt obligations and identity fusion with professional roles; perceive exits as unacceptable status loss
 *   - Capital-Holding Employers: Primary beneficiary (institutional/arbitrage) — benefit from precarity through wage suppression and workforce flexibility; perceive genuine coordination function
 *   - Financial Services Sector: Secondary beneficiary (institutional/arbitrage) — benefit from high debt loads and refinancing opportunities; see constraint as coordinative mechanism
 *   - High-Skill Professionals: Mixed victim/beneficiary (powerful/mobile) — trapped during early career despite eventual high earning potential; face identity pressure and credential cascades
 *   - Labor Organizing Movements: Organized agent (organized/mobile) — perceive constraint as temporary and solvable through collective action; building alternative pathways
 *   - Neoliberal Policy Framework: Institutional actor (institutional/arbitrage) — maintains constraint through policy theater and narrative naturalization; increasingly piton-like (theatrical maintenance masking functional failure)
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent policy arrangement as immutable economic law
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(downward_mobility_precarity, 0.58).
domain_priors:suppression_score(downward_mobility_precarity, 0.68).
domain_priors:theater_ratio(downward_mobility_precarity, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(downward_mobility_precarity, extractiveness, 0.58).
narrative_ontology:constraint_metric(downward_mobility_precarity, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(downward_mobility_precarity, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(downward_mobility_precarity, snare).
narrative_ontology:human_readable(downward_mobility_precarity, "Downward Mobility Precarity: The Fear-Based Constraint on Labor Exit").
narrative_ontology:topic_domain(downward_mobility_precarity, "economic/social/psychological").

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(downward_mobility_precarity, capital_holding_employers).
narrative_ontology:constraint_beneficiary(downward_mobility_precarity, financial_services_sector).
narrative_ontology:constraint_victim(downward_mobility_precarity, precarious_workers).
narrative_ontology:constraint_victim(downward_mobility_precarity, middle_class_households).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PRECARIOUS WORKER (SNARE) — Trapped by loss-aversion to downward status shift and material consequences (debt, eviction, hunger). Perceives the constraint as immovable within the biographical horizon. Exit requires not just leaving the job but accepting permanent lifestyle reduction, identity loss, and family disruption. The suppression is structural: loss of healthcare, housing, child support, dignity. Maximum extraction — the worker bears full cost of the mobility precarity through wage suppression, benefit denial, and psychological wear.
constraint_indexing:constraint_classification(downward_mobility_precarity, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: MIDDLE-CLASS HOUSEHOLD (SNARE) — Constrained by debt obligations (mortgage, student loans, childcare), but also perceives the constraint as enforced through narrative normalization. The household knows exits exist (career switching, relocation, lifestyle reduction) but perceives each as carrying unacceptable downward mobility cost. Suppression includes both structural (debt service) and internalized (status anxiety, identity fusion with professional role). Significant extraction: wage suppression to maintain job security, over-qualification traps, and psychological extraction through constant precarity anxiety.
constraint_indexing:constraint_classification(downward_mobility_precarity, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: CAPITAL-HOLDING EMPLOYERS (TANGLED ROPE) — Experience genuine coordination benefit from labor-market precarity: workers compete intensely for job security, suppressing wage demands and enabling rapid workforce adjustment. The constraint solves an employer coordination problem (how to maintain workforce flexibility without explicit contract violations). BUT the mechanism requires asymmetric extraction (worker fears vs employer optionality). Active enforcement through threat of outsourcing, automation, or status demotion. Beneficiary perspective: precarity generates discipline that solves the employer's retention-vs-flexibility tension.
constraint_indexing:constraint_classification(downward_mobility_precarity, tangled_rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: FINANCIAL SERVICES SECTOR (ROPE) — Experiences the constraint as pure coordination: debt securitization, credit scoring, and consumer lending are mechanisms that coordinate household financial behavior. The sector benefits from precarity (higher debt loads, refinancing opportunities, credit products) but also solves a genuine coordination problem: how to allocate scarce capital to uncertain borrowers. From the financial sector's perspective, the extraction is coordinative overhead, not coercion. Low suppression perceived; high coordination value. The sector sees itself as enabling mobility through credit, not constraining it.
constraint_indexing:constraint_classification(downward_mobility_precarity, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: HIGH-SKILL PROFESSIONAL CLASS (TANGLED ROPE) — Faces genuine coordination benefits (professional networks, licensure, institutional credentialing) but also experiences extraction asymmetry at generational scale. Entry barriers (education debt, credential cascades) trap early-career professionals despite eventual high earning potential. The constraint coordinates professional labor allocation and specialization but requires suppression of alternative career paths and geographic mobility during the biographical years when options are highest. Moderate power level: can eventually exit if willing to retrain, but the cost is high.
constraint_indexing:constraint_classification(downward_mobility_precarity, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 6: LABOR ORGANIZING MOVEMENTS (SCAFFOLD) — Organized agents (unions, worker cooperatives, mutual aid networks) see the precarity constraint as temporary and solvable through collective action. Sunset mechanism: as worker organizing power increases, precarity loses its disciplinary force — workers protected by union contracts or collective agreements experience lower suppression and lower effective extraction. The scaffold classification derives from the perception of agency and path to exit. High theater (organizing rhetoric, strike narratives) but genuine functional power to shift the constraint's parameters. Theater_ratio high but declining as actual wins accumulate.
constraint_indexing:constraint_classification(downward_mobility_precarity, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 7: NEOLIBERAL POLICY FRAMEWORK (PITON) — At civilizational scale, the constraint is maintained through institutional inertia and policy theater rather than active function. Labor deregulation, anti-union legislation, and austerity framing persist despite degraded efficacy: precarity fails to generate productivity gains (burnout reduces output), generates political instability (rising authoritarianism), and requires constant performative maintenance (CEO speeches about 'meritocracy,' 'disruption,' 'entrepreneurship'). The framework sees the constraint as natural economic law, but structural analysis reveals it as contingent policy choice. Theater-heavy maintenance masks declining coordination function.
constraint_indexing:constraint_classification(downward_mobility_precarity, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER / FALSE SUMMIT (MOUNTAIN) — Risk of naturalizing downward mobility precarity as an inevitable economic law ('humans are risk-averse,' 'markets require discipline'). This perspective sees the constraint as emerging naturally from human psychology and resource scarcity, perceiving it as immutable across all observables and institutional arrangements. However, the structural data contradicts the mountain classification: the constraint requires active enforcement (narrative suppression, institutional design choices, policy maintenance). The false summit reveals that 'naturalness' is a framing choice, not a structural fact. Cross-cultural and historical analysis shows mobility precarity operates at variable strength depending on institutional design, not as a law of nature.
constraint_indexing:constraint_classification(downward_mobility_precarity, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(downward_mobility_precarity_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(downward_mobility_precarity, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(downward_mobility_precarity, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(downward_mobility_precarity, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(downward_mobility_precarity, TR),
    TR >= 0.70.

:- end_tests(downward_mobility_precarity_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderately high and rising. Base measurement reflects the real economic extraction (wages suppressed by labor competition, benefits withheld to incentivize job retention, labor supply disciplines wage demands). The rising trajectory (0.35 → 0.58 over 50 years) reflects intensification of precarity mechanisms: erosion of pension systems, decline of union density, rise of gig economy, automation threats. However, extractiveness is not at maximum (0.80+) because the constraint coexists with coordination functions (labor allocation, capital distribution) that generate some genuine benefit to broader populations. Suppression (0.68): High and structural. Barriers to exit include material (debt, healthcare, housing), institutional (credential requirements, job search friction), and psychological (loss-aversion, identity fusion, normalization). The suppression is not total: some workers do exit (career switching, geographic relocation, business startup), but barriers are severe enough that most perceive exit as catastrophic. Theater ratio (0.55): Moderate and rising. Traditional narratives about 'meritocracy,' 'entrepreneurship,' and 'market discipline' constitute the performative layer. Actual verification of the constraint's functionality is low: precarity fails to generate expected productivity gains (burnout reduces output), generates political instability (rising authoritarianism and labor unrest), and requires constant narrative maintenance to sustain compliance. Theater growth from 0.38 to 0.55 indicates that institutional maintenance is increasingly performative relative to functional, suggesting piton characteristics at longer time scales.
 *
 * PERSPECTIVAL GAP:
 *   The gap between powerless and institutional perspectives is maximal: 90-degree divergence. Precarious workers see immutable snare; employers see coordination rope. This is not a disagreement about facts but a structural difference in how the constraint operates on different agents. The precarious worker's fear-based suppression is real; the employer's coordination benefit is real; both are features of the same constraint. The middle-class household's perspective reveals identity-based components: the suppression includes material (debt service) and psychological (loss-aversion, identity fusion). The analytical observer's risk of false summit (mountain classification) reveals the danger of naturalizing institutional arrangements as laws of nature. The scaffold perspective reveals that the constraint is not immutable — labor organizing genuinely shifts its parameters, suggesting exit is possible if coordination capacity changes.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values reflect each agent's structural position in the extraction flow. Precarious workers (d ≈ 0.95) experience maximum extraction: trapped by fear and material dependency, no arbitrage options, full victims. Middle-class households (d ≈ 0.75) experience high extraction: constrained by debt and identity, some awareness of exits but perceiving them as unacceptable status loss, partial victims. Employers (d ≈ 0.15) experience low extraction: beneficiaries with arbitrage options (can choose not to employ precarious workers, can outsource, can automate), designed to perceive the constraint as coordinative. Financial sector (d ≈ 0.10) similar: beneficiaries with arbitrage, perceive constraint as enabling mechanism. Professionals (d ≈ 0.50): symmetric position, benefit from credentialing in early career but trapped by credential cascades, experience moderate extraction. Organizers (d ≈ 0.40): organized agents with emerging exit pathways, experiencing declining extraction as organizing power grows. The directionality pipeline applies f(d) to convert d into experienced extractiveness chi, which explains why the same constraint appears as snare (high chi) from victim positions and rope/tangled rope (lower chi) from beneficiary positions.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by demonstrating that snare and tangled rope are BOTH correct classifications from different perspectives. The mandatrophy is 'is it a snare (pure extraction) or a rope (pure coordination)?' The answer: IT IS BOTH. From the precarious worker's perspective, it is a snare — no coordination benefit perceived, full extraction experienced. From the employer's perspective, it is a tangled rope — genuine coordination function (workforce flexibility, labor allocation) achieved through asymmetric extraction. The resolution is not to choose one classification but to recognize that the perspectival gap IS the phenomenon. The constraint coordinates some functions (labor allocation, capital distribution) while extracting from powerless agents (precarious workers, middle-class households). Suppressing the coordination component would be analytically dishonest (employers genuinely need workforce flexibility solutions); suppressing the extraction component would be ethically dishonest (workers genuinely suffer). The analytical move is to map the perspectives and let their divergence illuminate the structure. The false summit risk (mountain/natural law) is the biggest danger: if precarity is naturalized as inevitable economic law, organizing and institutional redesign become invisible. The framework's contribution is making precarity visible as a contingent institutional arrangement subject to redesign, not as a law of nature.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    loss_aversion_mechanism_distinction,
    'Is the suppression mechanism primarily psychological loss-aversion or primarily structural economic dependency?',
    'Experimental labor economics comparing wage preferences under certainty vs precarity; comparison of suppression effects in zero-debt vs high-debt populations; analysis of behavioral response differences in universal basic income or job guarantee regimes',
    'If primarily psychological: cognitive reframing and narrative intervention could shift classification. If primarily structural: suppression is robust across framings and requires institutional restructuring. Affects snare classification confidence.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(loss_aversion_mechanism_distinction, empirical, 'Loss-aversion vs structural economic dependency mechanism').

omega_variable(
    coordination_function_necessity,
    'Is the coordination function (employer flexibility, labor allocation) genuinely dependent on precarity, or could it be achieved through alternative mechanisms (worker retraining funds, skills portability, sectoral agreements)?',
    'Comparative institutional analysis of coordinated market economies (Germany, Nordic countries) with universal benefits and high labor mobility; historical analysis of post-war full-employment regimes; simulation of alternative coordination mechanisms',
    'If achievable without precarity: the constraint is pure extraction (Snare from all perspectives), not tangled rope. If alternative coordination requires equivalent suppression: suppression is coordinative cost, tangled rope classification holds. This resolves whether the beneficiary''s ''genuine coordination function'' claim is structural or rhetorical.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_function_necessity, empirical, 'Whether precarity is necessary for labor market coordination').

omega_variable(
    temporal_horizon_instability,
    'Why does the constraint show strong biographical-horizon suppression but institutional-horizon coordination function? Are agents'' time horizons truly asymmetric, or does this reflect perspectival power differences masquerading as time orientation?',
    'Longitudinal tracking of agent behavior shifts if time horizon lengthens (career guarantees, lifetime employment offers); analysis of whether employer perspective shifts to longer horizon in unionized or tenure-protected sectors',
    'If time horizons are exogenous: precarity is an unavoidable feature of the biographical-scale constraint. If time horizons are endogenous to institutional power: the constraint manufactures short-horizon thinking in precarious agents to prevent them from perceiving generational-scale exit options. This affects whether identity_locked should appear in precarious agent perspectives.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(temporal_horizon_instability, empirical, 'Whether time horizon differences are cognitive or institutional artifacts').

omega_variable(
    identity_lock_vs_material_trap,
    'For middle-class households and professional workers, is the suppression mechanism primarily identity fusion (professional identity, status anxiety, internalized worth measurement) or material economic dependency (debt service requirements, healthcare costs)?',
    'Analysis of behavioral response to hypothetical scenarios: universal basic income at 80% current income vs job loss; professional identity surveys in secure vs precarious employment; comparison of perceived suppression in debt-free vs debt-heavy populations with identical incomes',
    'If primarily identity: classify as identity_locked exit for some middle-class perspectives, suggesting that constraint-exit is cognitively possible but identity-wise impossible. If primarily material: constrained or trapped classification more accurate. Affects whether the constraint is neurotic (identity-based) or structural (economic-based).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_lock_vs_material_trap, conceptual, 'Identity fusion vs material economic dependency as suppression mechanism').

omega_variable(
    scaffold_sunset_plausibility,
    'Is the labor-organizing scaffold''s sunset clause realistic, or does it represent aspirational agency without structural power?',
    'Historical analysis of union density, labor organizing success rates, and wage trends over past 50 years; comparative analysis of precarity levels in unionized vs non-unionized sectors; scenario modeling of organizing growth rates required to shift the constraint''s parameters within the organizational window',
    'If realistic: scaffold classification and theater_ratio measurement are justified. If aspirational: the constraint''s theater conceals the absence of genuine exit pathways, and snare or piton classification better captures the structural reality. This affects whether the constraint is genuinely sunset-able or merely imagined to be so.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(scaffold_sunset_plausibility, empirical, 'Whether labor organizing can actually sunset the precarity constraint').

omega_variable(
    neoliberal_policy_intentionality,
    'Is the constraint actively maintained through deliberate policy design, or does it persist through institutional path-dependence and unintended consequences of deregulation?',
    'Discourse analysis of policy documents, legislative intent, and institutional elite framing; comparative analysis of jurisdictions with different deregulation histories; interviews and organizational ethnography of policy-making institutions',
    'If actively maintained: precarity is a snare with intentional enforcement (higher confidence in snare classification). If path-dependent accident: the constraint is a piton (functional failure, theatrical maintenance, institutional inertia). Affects whether the constraint is Machiavellian or merely absurd.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(neoliberal_policy_intentionality, conceptual, 'Deliberate policy design vs unintended institutional path-dependence').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(downward_mobility_precarity, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dmp_tr_t0, downward_mobility_precarity, theater_ratio, 0, 0.38).
narrative_ontology:measurement(dmp_tr_t15, downward_mobility_precarity, theater_ratio, 15, 0.48).
narrative_ontology:measurement(dmp_tr_t30, downward_mobility_precarity, theater_ratio, 30, 0.55).
narrative_ontology:measurement(dmp_tr_t45, downward_mobility_precarity, theater_ratio, 45, 0.62).

% Extraction over time
narrative_ontology:measurement(dmp_be_t0, downward_mobility_precarity, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(dmp_be_t15, downward_mobility_precarity, base_extractiveness, 15, 0.48).
narrative_ontology:measurement(dmp_be_t30, downward_mobility_precarity, base_extractiveness, 30, 0.58).
narrative_ontology:measurement(dmp_be_t45, downward_mobility_precarity, base_extractiveness, 45, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(downward_mobility_precarity, resource_allocation).
narrative_ontology:affects_constraint(downward_mobility_precarity, healthcare_system_dependency).
narrative_ontology:affects_constraint(downward_mobility_precarity, debt_securitization_trap).
narrative_ontology:affects_constraint(downward_mobility_precarity, status_anxiety_mechanism).
narrative_ontology:affects_constraint(downward_mobility_precarity, labor_union_constraint).

% DUAL FORMULATION NOTE:
% Downward mobility precarity is a constraint family spanning multiple domain-specific stories: household-level financial precarity (debt, housing, healthcare), organizational-level labor discipline (wage suppression, job security trades), sectoral-level labor market segmentation (gig economy, contractor status), and civilizational-level policy arrangements (deregulation, union decline). Each domain story has its own epsilon value and structural specificity, but they are linked by the common mechanism of fear-based suppression. The household-level story focuses on debt and identity; the organizational story focuses on workforce discipline; the sectoral story focuses on labor market structure; the civilizational story focuses on policy naturalization. All are affected by but also affect labor organizing capacity, suggesting reciprocal network relationships.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
