% ============================================================================
% CONSTRAINT STORY: caregiver_labor_undervaluation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_caregiver_labor_undervaluation, []).

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
 *   constraint_id: caregiver_labor_undervaluation
 *   human_readable: Caregiver Labor Undervaluation
 *   domain: social/economic/labor
 *
 * SUMMARY:
 *   Caregiver labor undervaluation is a structural extraction mechanism that
 *   operates across multiple institutional domains (family, state welfare,
 *   labor markets) and captures value from primary caregivers, care workers,
 *   and dependent populations simultaneously. The constraint combines
 *   suppression of exit options, normalization of undervaluation through
 *   cultural narratives, and institutional enforcement through labor law,
 *   welfare policy, and credential gatekeeping. The constraint exhibits snare
 *   characteristics for powerless agents (primary caregivers, low-wage care
 *   workers) who face high extraction and high suppression with minimal
 *   perceived coordination benefit, while simultaneously functioning as rope
 *   from the state's perspective (coordination benefit of cost savings) and
 *   tangled rope from the household's perspective (genuine coordination of
 *   care needs alongside extraction benefit). The theater ratio (0.65)
 *   reflects the degree to which caregiver undervaluation is justified
 *   through cultural narratives ('it's labor of love,' 'care is naturally
 *   provided by women') rather than economic necessity. The constraint has
 *   intensified over 50-year interval as care work has become more formalized
 *   and marketized without corresponding wage increases, and as measurement
 *   and quantification of care work have revealed the degree of
 *   undervaluation without eliminating it.
 *
 * KEY AGENTS:
 *   - Primary Caregiver (usually mother, family member): Victim (powerless/trapped) — provides unpaid or severely underpaid care; structurally dependent on care recipient or employing household; identity fused with caregiver role
 *   - Care Worker (childcare provider, home health aide, nanny): Victim (powerless/trapped) — occupationally concentrated in low-wage sector; high suppression of alternatives; emotional labor and somatic vulnerability exploited
 *   - Dependent Population (children, elderly, disabled): Indirect victim — receives care but dependent quality and continuity on worker wages and turnover; interests not represented in wage-setting
 *   - State Welfare System: Beneficiary (institutional/arbitrage) — cost savings from offloading care provision to families and low-wage sector; enforces undervaluation through labor law and credential restriction
 *   - Employing Household (high-income): Beneficiary-victim (powerful/mobile) — benefits from cheap care labor; constrained by reliance on workers who are turnover-prone due to low wages; coordination benefit alongside extraction benefit
 *   - Care Sector Employers (childcare centers, home care agencies): Beneficiary (powerful/mobile) — profit from spread between care worker wages and care recipient/household prices; labor cost discipline enforced through market segmentation and credential barriers
 *   - Care Worker Organizing Movement: Organized agent (organized/constrained) — building alternative pathways through sectoral bargaining, wage-fixing legislation, public care infrastructure proposals; sees constraint as temporary with sunset clause
 *   - Analytical Observer: Civilizational view (analytical/analytical) — observes that caregiver undervaluation serves as implicit tax on care provision and subsidy to consumption by care recipients and employing agents
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(caregiver_labor_undervaluation, 0.68).
domain_priors:suppression_score(caregiver_labor_undervaluation, 0.72).
domain_priors:theater_ratio(caregiver_labor_undervaluation, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(caregiver_labor_undervaluation, extractiveness, 0.68).
narrative_ontology:constraint_metric(caregiver_labor_undervaluation, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(caregiver_labor_undervaluation, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(caregiver_labor_undervaluation, snare).
narrative_ontology:human_readable(caregiver_labor_undervaluation, "Caregiver Labor Undervaluation").
narrative_ontology:topic_domain(caregiver_labor_undervaluation, "social/economic/labor").

domain_priors:requires_active_enforcement(caregiver_labor_undervaluation).
% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(caregiver_labor_undervaluation, state_welfare_systems).
narrative_ontology:constraint_beneficiary(caregiver_labor_undervaluation, employing_households).
narrative_ontology:constraint_beneficiary(caregiver_labor_undervaluation, care_service_purchasers).
narrative_ontology:constraint_victim(caregiver_labor_undervaluation, primary_caregivers).
narrative_ontology:constraint_victim(caregiver_labor_undervaluation, care_workers).
narrative_ontology:constraint_victim(caregiver_labor_undervaluation, children_elderly_dependent_populations).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PRIMARY CAREGIVER (SNARE) — Structurally trapped. Care responsibilities create dependency (financial, identity, relational). Exit options are systematically suppressed: career penalties for work interruption, childcare costs that exceed potential wages, legal custody barriers, identity fusion with caregiver role. The constraint extracts unpaid or severely underpaid labor while suppressing awareness of alternatives through normalization ('it's what mothers do'). Maximum experienced extraction — the caregiver bears the full asymmetry.
constraint_indexing:constraint_classification(caregiver_labor_undervaluation, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: CARE WORKER (SNARE) — Trapped by occupational concentration (limited alternatives in care sector), credential requirements that don't translate to other sectors, employer power asymmetry, and wage floors set below subsistence-adjacent levels. Immigration status often used as suppression mechanism. The care sector is structured to maximize extraction: high emotional labor, somatic vulnerability, legal restriction of benefits. High suppression, high extraction, minimal coordination function.
constraint_indexing:constraint_classification(caregiver_labor_undervaluation, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 3: STATE WELFARE SYSTEM (ROPE) — Benefits from the constraint through massive cost savings: caregiving is extracted from private households (non-market) or subsidized care sector instead of funded publicly. The state experiences the undervaluation as coordination: it solves the collective action problem of providing care by offloading costs to families and low-wage workers. From this perspective, the system functions to distribute care provision without full public funding. The state has exit options (e.g., public childcare systems, care worker wage standardization) but chooses arbitrage (maintaining current structure for budget optimization).
constraint_indexing:constraint_classification(caregiver_labor_undervaluation, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: EMPLOYING HOUSEHOLD (TANGLED ROPE) — Simultaneously benefits and is constrained. Benefits from cheap domestic labor and childcare; constrained by reliance on care workers' availability and quality, which are suppressed by undervaluation (high turnover, low training, burnout). Coordination function is genuine: hiring a care worker solves a household's time allocation problem. But asymmetric extraction persists: the wage is suppressed relative to the labor's value and alternatives. Mobile exit options (hire different workers, use institutional care, relocate) but exercised within a constrained market. The household experiences genuine coordination benefit alongside extraction benefit.
constraint_indexing:constraint_classification(caregiver_labor_undervaluation, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: CARE WORKER ORGANIZING MOVEMENT (SCAFFOLD) — Organized agents (domestic worker unions, care sector advocacy, wage-fixing legislation) see the undervaluation as a temporary extraction mechanism with a sunset clause. Institutional design changes (sectoral bargaining, care worker certification standards, public care infrastructure) create pathways to revalue care. Low effective extraction because this perspective has agency and sees an exit path through collective power and policy change. Suppression is high structurally but declining as organizing capacity grows. Theater is moderate — the movement must perform visibility work, but the coordination function (collective wage-setting) is substantive.
constraint_indexing:constraint_classification(caregiver_labor_undervaluation, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: CARE ECONOMY INSTITUTION (PITON) — The institutional framing of care as non-market, gift-based, or 'unskilled' labor persists through cultural and regulatory inertia despite evidence that care requires high emotional intelligence, technical skill, and physical stamina. The institutional narrative ('care is labor of love') is performative — it justifies undervaluation while masking the extraction. Theater ratio is high because the care economy is maintained by repeated affirmation of its non-market character despite functioning as a labor market. The institution sees itself as degraded — policymakers and employers acknowledge care workers are underpaid — but alternatives haven't fully replaced the narrative, so undervaluation persists.
constraint_indexing:constraint_classification(caregiver_labor_undervaluation, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (TANGLED ROPE) — From the civilizational perspective, care is both genuinely coordinating (solves reproduction/dependency problems inherent to human biology) and genuinely extracting (captures surplus value from unpaid/underpaid caregivers to subsidize consumption by care recipients and employing agents). The constraint exhibits the core tangled rope signature: both coordination and asymmetric extraction coexist, and enforcement mechanisms maintain both. The analytical observer sees the mandate structure clearly: the state enforces undervaluation through labor law, welfare policy, and credential restriction, while simultaneously benefiting from the cost savings.
constraint_indexing:constraint_classification(caregiver_labor_undervaluation, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(caregiver_labor_undervaluation_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(caregiver_labor_undervaluation, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(caregiver_labor_undervaluation, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(caregiver_labor_undervaluation, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(caregiver_labor_undervaluation, TR),
    TR >= 0.70.

:- end_tests(caregiver_labor_undervaluation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High and rising. The measurement trajectory shows extractiveness increasing from 0.45 (50 years ago, when much care was non-market) to 0.68 (present), reflecting the formalization and marketization of care work without corresponding wage increases. The rise indicates that as care has become more explicitly valued and quantified, the undervaluation has become more visible and more entrenched (institutional enforcement rather than just cultural normalization). For primary caregivers, extraction is captured through wage penalties, lost benefits, restricted credit, and dependency. For care workers, extraction is captured through occupational wage suppression relative to skills required. For the state, extraction is negative (benefit from cost savings). Suppression (0.72): Very high. Barriers to exit are multifaceted: (1) structural — childcare costs exceed potential wages, custody law restricts mobility, credential requirements create occupational lock-in; (2) identity-based — caregiver identity internalized, beliefs about natural female role, perceived unsuitability for other work; (3) informational — undervaluation naturalized through cultural narratives, alternative arrangements invisible; (4) institutional — labor law excludes domestic workers from protections, welfare policy penalizes secondary earners. Theater ratio (0.65): Moderate-high. The justification for caregiver undervaluation relies significantly on performative narratives ('it's labor of love,' 'care is naturally provided') rather than economic necessity. The institutional framing of care as gift-based or naturally female-gendered sustains undervaluation even when it becomes economically irrational.
 *
 * PERSPECTIVAL GAP:
 *   The constraint exhibits maximum perspectival divergence. The primary caregiver and care worker see snare (pure extraction, no coordination benefit, maximum suppression). The state sees rope (genuine coordination benefit of cost distribution without public funding). The employing household sees tangled rope (genuine coordination of care needs alongside extraction benefit). The care worker movement sees scaffold (temporary constraint with sunset via public infrastructure and sectoral bargaining). The institutional narrative sees piton (degraded but persistent ritual). The analytical observer sees tangled rope at civilizational scale (genuine care coordination alongside systematic extraction from powerless agents). The gap reveals that extraction and coordination are not mutually exclusive — the same institutional arrangement can be purely extractive for trapped agents and genuinely coordinating for powerful agents. The mandatrophy is resolved by recognizing all perspectives as valid: they describe the same constraint from different structural positions.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality derives from structural position. Primary caregivers are trapped victims with no exit options: d = 0.95 → high f(d) → high experienced χ. Care workers are powerless trapped victims but with slightly more occupational mobility than primary caregivers: d = 0.90 → high f(d) → high χ. State welfare system is institutional beneficiary with arbitrage options: d = 0.05 → negative f(d) → negative χ (extraction flows toward this agent). Employing households are powerful beneficiaries with mobile exit but also dependent on care worker availability: d = 0.50 → moderate f(d) → moderate χ (both benefit and constraint). Care worker organizing movement is organized agent with constrained exit but organized power: d = 0.35 → low-moderate f(d) → low χ. Analytical observer sees the full structure at civilizational scale: d = 0.72 → high f(d) → high χ (the system is extracting from powerless agents at large scope to subsidize care for all households).
 *
 * MANDATROPHY ANALYSIS:
 *   EXTRACTION + COORDINATION COEXISTENCE: Caregiver undervaluation is both a coordination mechanism (solves the problem of how to provide and finance care within households) and an extraction mechanism (captures surplus value from caregivers to subsidize other consumption). The mandatrophy is resolved by disaggregating perspectives: trapped agents experience snare (extraction without coordination benefit), while powerful agents and the state experience rope or tangled rope (coordination with extraction benefit). The constraint cannot be reclassified as pure rope because the extraction is asymmetric and suppression is high — trapped agents have no alternatives and no coordination benefit. The classification as snare for powerless perspectives and tangled rope for the analytical observer is consistent with the structural data: the same institutional arrangement is extractive for some agents and coordinating for others. INSTITUTIONAL ENFORCEMENT: The constraint is maintained not by natural limits but by institutional design choices (labor law, welfare policy, credential gatekeeping, wage-setting norms). This makes it accessible to policy intervention — the scaffold perspective is not aspirational but structural. Organized agents can create exit paths through sectoral bargaining, public infrastructure, and wage-fixing legislation. IDENTITY LOCK COMPONENT: A significant portion of suppression (estimated 0.20-0.30 of the 0.72) is likely internalized/identity-locked rather than purely structural. This complicates the exit analysis but does not change classification — identity-locked is still a form of entrapment at biographical timescale, even if it is cognitively rather than materially grounded.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    identity_lock_vs_structural_trap,
    'To what extent is the caregiver''s exit barrier structural (economic dependency, custody law, credential barriers) versus identity-locked (caregiver identity fused with self-concept, internalized beliefs about caregiving as natural female role)?',
    'Comparative analysis of barriers post-exit: do caregivers continue to define themselves as caregivers after leaving caregiving? Do they report persistent identity pressure even after financial/legal barriers are removed? Ethnographic work on identity renegotiation among formerly primary caregivers.',
    'If primarily structural: classification stands at trapped across all perspectives. If significant identity component: reclassify to identity_locked for perspectives where the agent is structurally mobile but identity-constrained. This would change biographical-time classification for some perspectives (rope instead of snare for identity_locked + high-power agent).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_vs_structural_trap, empirical, 'Structural versus identity-locked binding in caregiver entrapment').

omega_variable(
    care_quality_vs_cost_extraction_tradeoff,
    'Does undervaluation of care workers reduce care quality for dependent populations, or is quality maintained despite low wages through emotional commitment and intrinsic motivation?',
    'Longitudinal outcome studies comparing care quality (child development, elderly health outcomes) against care worker wages across regions; analysis of turnover effects on continuity of care; ethnographic work on how care workers maintain quality under suppressed wages.',
    'If quality degrades significantly: suppression is partially self-limiting (poor outcomes undermine the extraction mechanism). If quality persists: extraction is more stable and higher effective chi. If quality persists only through worker burnout and mental health costs: true extraction is higher than measured (hidden costs borne by workers).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(care_quality_vs_cost_extraction_tradeoff, empirical, 'Whether care quality persists despite worker undervaluation').

omega_variable(
    public_care_infrastructure_viability,
    'Would fully public care infrastructure (state-funded childcare, elder care, disability support) be economically viable at current care standards, or does the current undervaluation represent an economically necessary subsidy to care provision?',
    'Comparative cost analysis: fully public care sector costing in countries with high public investment (Denmark, Sweden) versus current mixed public/private/household arrangement costs in low-public-investment countries. Attribution of cost differences to scale, efficiency, wage levels, and hidden household labor.',
    'If viable at higher wages: scaffold sunset is real — public infrastructure is a genuine alternative. If viable only with service degradation: the snare classification is correct — the undervaluation is an extraction mechanism that cannot be eliminated without serious service reduction. Affects mandatrophy resolution.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(public_care_infrastructure_viability, empirical, 'Economic viability of public care infrastructure').

omega_variable(
    gender_versus_occupational_undervaluation,
    'Is caregiver undervaluation primarily driven by occupational concentration in low-status sectors (feminized work), or by explicit gendered devaluation of care work as non-economic?',
    'Comparative wage analysis: do male-dominated care sectors (e.g., athletic training, certain medical specialties) face similar suppression? Do care workers in gender-integrated occupations (e.g., nurses vs home health aides, pediatricians vs nannies) show wage convergence? Analysis of how caregiving is framed in public discourse by gender.',
    'If primarily occupational: policy interventions should focus on occupational revaluation (certification, unionization, sectoral bargaining). If primarily gendered: interventions must address identity narratives and cultural valuations of women''s work. If both: constraint family should decompose into gender_occupational_undervaluation + care_sector_occupational_undervaluation stories with different ε values.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(gender_versus_occupational_undervaluation, empirical, 'Gender versus occupational drivers of care undervaluation').

omega_variable(
    suppression_mechanism_internalization,
    'Is the measured suppression (0.72) maintained primarily by external barriers (legal, economic, informational) or by internalized beliefs among caregivers that devalue their own labor?',
    'Survey analysis: do caregivers report external barriers (legal, financial, mobility) or internal barriers (identity, beliefs about caregiving, perceived unsuitability for other work) as the primary constraint? Experimental interventions: what happens to caregiver exit rates when external barriers are removed (income support, childcare, credential recognition) without explicitly addressing identity narratives?',
    'If primarily external: interventions targeting removal of barriers should enable exits. If primarily internalized: the constraint is partially identity_locked rather than trapped, and interventions must address identity narratives and self-perception. If both: suppression is over-estimated at 0.72 — the constraint carries its suppression internally after external barriers are removed, suggesting higher true extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalization, empirical, 'Structural versus internalized suppression in caregiver entrapment').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(caregiver_labor_undervaluation, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(care_tr_t0, caregiver_labor_undervaluation, theater_ratio, 0, 0.52).
narrative_ontology:measurement(care_tr_t20, caregiver_labor_undervaluation, theater_ratio, 20, 0.6).
narrative_ontology:measurement(care_tr_t40, caregiver_labor_undervaluation, theater_ratio, 40, 0.65).
narrative_ontology:measurement(care_tr_t50, caregiver_labor_undervaluation, theater_ratio, 50, 0.68).

% Extraction over time
narrative_ontology:measurement(care_be_t0, caregiver_labor_undervaluation, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(care_be_t20, caregiver_labor_undervaluation, base_extractiveness, 20, 0.58).
narrative_ontology:measurement(care_be_t40, caregiver_labor_undervaluation, base_extractiveness, 40, 0.68).
narrative_ontology:measurement(care_be_t50, caregiver_labor_undervaluation, base_extractiveness, 50, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(caregiver_labor_undervaluation, attachment_coordination).
narrative_ontology:affects_constraint(caregiver_labor_undervaluation, women_labor_market_participation).
narrative_ontology:affects_constraint(caregiver_labor_undervaluation, intergenerational_poverty_transmission).
narrative_ontology:affects_constraint(caregiver_labor_undervaluation, elderly_dependent_care_access).

% DUAL FORMULATION NOTE:
% Caregiver labor undervaluation is upstream of three constraint families: women's labor market participation (blocked by care responsibility penalties), intergenerational poverty (children of primary caregivers inherit the constraint), and elderly care access (dependent populations receive lower-quality care due to worker suppression). The measurement trajectory shows extractiveness increasing as care has formalized without corresponding wage increases, indicating institutional enforcement is substituting for cultural normalization.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(caregiver_labor_undervaluation, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
