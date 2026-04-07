% ============================================================================
% CONSTRAINT STORY: sotu_2000_clinton_welfare_reform_work_requirement
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sotu_2000_clinton_welfare_reform_work_requirement, []).

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
 *   constraint_id: sotu_2000_clinton_welfare_reform_work_requirement
 *   human_readable: Conditional Welfare Eligibility with Work Participation Requirements (PRWORA 1996 / SOTU 2000 Implementation)
 *   domain: social_policy/labor_regulation
 *
 * SUMMARY:
 *   The Personal Responsibility and Work Opportunity Reconciliation Act of
 *   1996 (PRWORA), formalized in Clinton's 2000 State of the Union rhetoric,
 *   restructures American welfare by conditioning adult cash assistance on
 *   work participation while protecting health care and nutrition benefits
 *   for children. This constraint exhibits a genuine tangled-rope structure:
 *   it coordinates child protection (nutrition, health care persist
 *   regardless of parental employment status) while extracting from adults
 *   unable to meet work requirements. The constraint's extractiveness (0.52)
 *   reflects moderate asymmetric extraction: employed parents and state
 *   budgets benefit from cost containment and employment incentives;
 *   unemployable or severely disabled adults face time-limited assistance
 *   with no adequate safety net. Suppression (0.68) is high due to multiple
 *   barriers: time limits (60 months federal lifetime cap), narrow disability
 *   determination, work-search requirements that are difficult to meet
 *   without adequate job training or job availability, and the threat of
 *   benefit termination for non-compliance. Theater ratio (0.55) indicates
 *   that work requirements include performative elements—job training with
 *   low placement rates, work-search activities that may not lead to
 *   employment, compliance rituals that affirm the work ethic
 *   narrative—alongside genuine employment incentives and coordination
 *   mechanisms that enable parental work. The constraint was presented as
 *   addressing 'welfare dependency' as a cultural pathology rather than
 *   poverty as an economic condition, naturalizing policy choices (work
 *   requirements, time limits, asset tests) as reflections of moral principle
 *   rather than contingent design decisions.
 *
 * KEY AGENTS:
 *   - Employed or employable parents: Primary beneficiary (moderate/constrained or institutional/arbitrage) — receive child care subsidies, job training, work incentives; escape poverty through wage earnings
 *   - Unemployable or severely disabled adults: Primary victim (powerless/trapped) — face time limits with inadequate disability safety net; cannot work, cannot sustain non-compliance, forced to choose between starvation and benefit loss
 *   - Children of non-compliant parents: Secondary victim (powerless/trapped but protected by child benefit carve-out) — child benefits (nutrition, health care) protect from parent's inability to work, but household poverty persists
 *   - State fiscal administrators: Secondary beneficiary (institutional/arbitrage) — achieve cost containment through time limits and reduced caseloads; administrative flexibility to set benefit levels and work requirements
 *   - Child care providers and job training vendors: Secondary beneficiary (institutional/arbitrage or powerful/arbitrage) — funding flows to providers through work-enabling infrastructure investments
 *   - Child advocacy coalition: Organized observer (organized/constrained) — sees coordination function (child protection) and extraction function (adult poverty) as structurally linked; limited power to modify enforcement
 *   - Work ethic narrative machinery: Cultural institution (institutional/arbitrage) — sustains constraint through cultural affirmation of work as prerequisite for dignity; performs theater that maintains constraint despite modest empirical success
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sotu_2000_clinton_welfare_reform_work_requirement, 0.52).
domain_priors:suppression_score(sotu_2000_clinton_welfare_reform_work_requirement, 0.68).
domain_priors:theater_ratio(sotu_2000_clinton_welfare_reform_work_requirement, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sotu_2000_clinton_welfare_reform_work_requirement, extractiveness, 0.52).
narrative_ontology:constraint_metric(sotu_2000_clinton_welfare_reform_work_requirement, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(sotu_2000_clinton_welfare_reform_work_requirement, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sotu_2000_clinton_welfare_reform_work_requirement, tangled_rope).
narrative_ontology:human_readable(sotu_2000_clinton_welfare_reform_work_requirement, "Conditional Welfare Eligibility with Work Participation Requirements (PRWORA 1996 / SOTU 2000 Implementation)").
narrative_ontology:topic_domain(sotu_2000_clinton_welfare_reform_work_requirement, "social_policy/labor_regulation").

domain_priors:requires_active_enforcement(sotu_2000_clinton_welfare_reform_work_requirement).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sotu_2000_clinton_welfare_reform_work_requirement, employed_parents).
narrative_ontology:constraint_beneficiary(sotu_2000_clinton_welfare_reform_work_requirement, state_fiscal_budgets).
narrative_ontology:constraint_beneficiary(sotu_2000_clinton_welfare_reform_work_requirement, child_care_providers).
narrative_ontology:constraint_beneficiary(sotu_2000_clinton_welfare_reform_work_requirement, job_training_vendors).
narrative_ontology:constraint_victim(sotu_2000_clinton_welfare_reform_work_requirement, unemployable_adults).
narrative_ontology:constraint_victim(sotu_2000_clinton_welfare_reform_work_requirement, time_constrained_caregivers).
narrative_ontology:constraint_victim(sotu_2000_clinton_welfare_reform_work_requirement, informal_economy_workers).
narrative_ontology:constraint_victim(sotu_2000_clinton_welfare_reform_work_requirement, child_support_administration_burden).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: UNEMPLOYABLE ADULT (SNARE) — Faces time-limited cash assistance (60 months federal lifetime cap) with no exit pathway. Medical disability determination is narrow; functional incapacity due to mental health, trauma, or multiple barriers is not equivalent to categorical disability. Agent is trapped: cannot work (no capacity), cannot claim disability (threshold not met), cannot exit via informal economy (income disqualification triggers claw-backs). Child benefits persist, creating a perverse incentive structure where the adult must sacrifice their own sustenance to maintain the child's nutrition and health. Maximum suppression and extraction — the constraint forces an impossible choice: starve yourself to keep your child's benefits, or lose both.
constraint_indexing:constraint_classification(sotu_2000_clinton_welfare_reform_work_requirement, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: LOW-WAGE WORKER (TANGLED ROPE) — Constrained by childcare costs, transportation barriers, and work scheduling inflexibility. Genuine coordination function: the work requirement creates incentives for child care investments, job training, and transportation assistance that enable parental employment. The constraint benefits this agent through subsidized child care and work support services. BUT asymmetric extraction persists: the work requirement imposes strict time-clocking and compliance monitoring; wage floors are low; the threat of benefit termination for non-compliance creates coercive pressure to accept poor working conditions. Significant benefit from coordination (child care, earned income tax credit) but also significant cost from extraction (surveillance, penalty regime). Mixed experience — this is the perspective where the constraint's coordination function is strongest.
constraint_indexing:constraint_classification(sotu_2000_clinton_welfare_reform_work_requirement, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: STATE FISCAL ADMINISTRATOR (ROPE) — Benefits from immediate cost containment. Federal cap (60 months) and monthly caps reduce long-term liabilities. Cash assistance expenditures drop sharply; per-capita state spending declines even as child care and job training spending rises. The constraint is experienced as pure coordination — how to deliver benefits more efficiently while maintaining child health outcomes. No agent is pushing back at the state level; the state holds administrative authority and benefit-setting power. Exit is costless (move to lower benefit levels) or beneficial (reduce eligible population, lower costs). This perspective sees the constraint as genuinely beneficial allocation: shifting from open-ended cash to targeted services.
constraint_indexing:constraint_classification(sotu_2000_clinton_welfare_reform_work_requirement, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: CHILD ADVOCACY COALITION (TANGLED ROPE) — Organized agents (child welfare advocates, poverty researchers, some academic economists) see the constraint as coordinating protection for children while extracting from disabled and severely disadvantaged adults. The coalition experiences constraint through the data: child poverty measures improved initially post-1996 (coordination function), but deep poverty for disconnected adults worsened (extraction function). The coalition has organizational capacity but limited exit options — they must operate within the policy regime. Extraction is asymmetric: beneficiaries (employed parents, state budgets) have power to change the constraint; victims (unemployable adults) have no political voice. The coalition is constrained by political feasibility — expansion of disability determination or unconditional child benefits faces strong opposition.
constraint_indexing:constraint_classification(sotu_2000_clinton_welfare_reform_work_requirement, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: WORK ETHIC NARRATIVE (PITON) — The cultural narrative sustaining the constraint ('welfare dependency is the problem, not poverty'; 'work builds character'; 'able-bodied people should work') persists long after its empirical justification has degraded. Welfare-to-work evaluation research (late 1990s-2000s) showed modest employment gains for some populations, minimal wage gains, and persistent non-employment for others. The narrative machinery, however, maintains the constraint through performative functions: work requirements appear on monthly compliance statements; job training programs run theater (participants attend sessions with low employment outcomes but high attendance compliance); time-clocking rituals affirm the 'responsible parent' framing. Theater ratio (0.55) reflects this: the work requirement has real enforcement (money follows employment), but significant portions of activity are symbolic affirmation of the work ethic rather than effective job placement or wage improvement. The piton persists not because the constraint works but because the narrative continues to justify it.
constraint_indexing:constraint_classification(sotu_2000_clinton_welfare_reform_work_requirement, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, this constraint is presented as reflecting an immutable truth about the nature of assistance: benefits cannot be unconditional; work is a prerequisite for social participation; individual responsibility is non-negotiable. This framing treats the work requirement as emerging naturally from human nature and economic necessity rather than as a constructed policy choice. However, the structural data shows this is a false summit: identifiable beneficiaries exist (employed parents, state budgets, job training vendors); enforcement is active; the constraint could be redesigned (unconditional child benefits, expanded disability definition) and the system would persist with different distributional properties. The false summit reveals how policy naturalizes its own choices as laws of nature.
constraint_indexing:constraint_classification(sotu_2000_clinton_welfare_reform_work_requirement, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sotu_2000_clinton_welfare_reform_work_requirement_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(sotu_2000_clinton_welfare_reform_work_requirement, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(sotu_2000_clinton_welfare_reform_work_requirement, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(sotu_2000_clinton_welfare_reform_work_requirement, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(sotu_2000_clinton_welfare_reform_work_requirement, TR),
    TR >= 0.70.

:- end_tests(sotu_2000_clinton_welfare_reform_work_requirement_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The constraint extracts via time-limited assistance (60-month federal cap), narrow disability definition, and work-search requirements that force entry into low-wage labor markets. However, extractiveness is not maximal because the constraint does not extract from all agents equally—employed parents benefit substantially, and children's benefits are protected. The asymmetry is the extraction: benefits flow to employed parents and state budgets; costs fall on unemployable adults who face termination without adequate alternatives. Extractiveness increased from 0.35 (1996, initial implementation) to 0.52 (2006, after time limits began expiring) because the most vulnerable cohort—those unable to find sustainable employment—began hitting time limits with no re-entry pathway. Suppression (0.68): High. Multiple barriers prevent exit: (a) time limits create hard-stop termination; (b) disability definition is narrow, excluding trauma and mental health conditions that impair work capacity; (c) work-search requirements consume time and resources without guaranteed job placement; (d) benefit loss threatens child care and nutrition, forcing choice between compliance and child welfare; (e) informal economy work is penalized via income clawbacks. Suppression is not maximal (0.75+) because some agents can work and do so; the constraint is not physically imprisoning. Theater ratio (0.55): Moderate. Work requirements include genuine employment incentives (child care subsidies, job training, earned income tax credit) that function as real coordination mechanisms. However, substantial portions are performative: work-search activities that may not lead to employment, mandatory job training with low placement rates, compliance monitoring that affirms the work ethic narrative without materializing in sustained employment outcomes. Theater increased over time as program maturation revealed that many low-wage placements were unstable and that work-search compliance did not correlate with employment durability. The piton perspective reflects this trajectory: the constraint persists through institutional inertia and narrative maintenance despite modest empirical validation.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates a dramatic perspectival gap between beneficiaries and victims. Employed parents see coordination (rope or tangled-rope at moderate power): the constraint enables their employment through child care subsidies and work incentives. State administrators see pure coordination (rope): cost reduction and administrative efficiency. Unemployable adults see pure extraction (snare): time limits, narrow definitions, and forced compliance with unachievable work requirements. The child advocacy coalition sees both coordination (child benefits protected) and extraction (adult poverty worsens), hence tangled-rope at organized power. The work ethic narrative institution sees itself as maintaining a natural law (mountain)—work is the prerequisite for assistance—but the engine's false-summit detector would identify this as naturalization of a contingent policy choice. The analytical observer risks endorsing the mountain perspective if they mistake the strong cultural narrative for empirical inevitability. The key gap: beneficiaries experience the constraint as solving a problem (enabling work); victims experience it as creating a problem (time limits without adequate alternatives). No single perspective is 'correct'—the presheaf over all perspectives reveals that the constraint simultaneously coordinates child protection and extracts from unemployable adults.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries and victims determine the flow of directionality (d). Employed or employable parents with access to child care and job training are beneficiaries—they experience the constraint as enabling (work support infrastructure), so d is low (0.15–0.25). State fiscal administrators are beneficiaries—they experience cost reduction and administrative flexibility, so d is near-zero (0.05–0.10). Unemployable adults with no disability safety net are victims—they experience the constraint as extractive (time limits, narrow definitions, forced choice), so d is high (0.85–0.95). The child-beneficiary carve-out (health care and nutrition benefits persist) partially decouples children's welfare from parental employment, but this does not change the adult's directionality—the adult still faces the same suppression and extraction. The organized coalition (child advocates) has constrained exit options—they must advocate within the policy regime—so their d is moderate (0.55–0.65). The piton narrative institution benefits from the constraint's maintenance, so d is low (0.10–0.20), but the constraint's effectiveness has degraded (theater increase) while the narrative persists, lowering f(d) even as d itself might rise slightly for this agent.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy (0.52 < 0.70, mandatrophy not required) by showing that tangled-rope classification is accurate: the constraint contains both genuine coordination (child protection, work support infrastructure, fiscal sustainability) and genuine asymmetric extraction (time limits, narrow disability definition, forced low-wage labor for vulnerable adults). The mandatrophy does not resolve by denying one function or elevating the other to natural-law status. The constraint really does coordinate child welfare and enable parental employment for some populations; the constraint really does extract from unemployable adults without adequate safety net. Both are true simultaneously. The false-summit detection is critical: the mountain perspective (work is a natural prerequisite for assistance) must be rejected. This is a constructed policy choice, not a law of nature. Alternative designs exist: unconditional child benefits, expanded disability definition, job guarantee or basic income for unemployable populations. These are not empirically impossible—they are politically unacceptable within the current ideological frame. The constraint persists because beneficiary coalitions (employed parents, fiscal conservatives, employment-focused advocates) have power to maintain it; victim coalitions (unemployable adults, disability advocates, anti-poverty organizations) lack organizational power and political voice. The mandatrophy analysis clarifies: this is not 'coordination mistaken for extraction' or vice versa. Both functions are real. The question is whether the current distribution of benefits and costs is defensible given the constraint's mixed nature.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    disability_definition_elasticity,
    'What percentage of ''unable to work'' adults would meet expanded disability definition that includes trauma, mental health conditions, and functional capacity deficits below categorical thresholds?',
    'Administrative data linkage: compare SSA disability determination outcomes (narrow definition) with clinical assessments of work capacity from health departments and community health centers',
    'If expansion would cover >30% of terminated adults: the snare perspective is understated (more agents are genuinely unemployable). If <10%: many non-compliance cases reflect choice, not incapacity, and the constraint is less extractive for the powerless perspective.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(disability_definition_elasticity, empirical, 'Elasticity of disability determination thresholds').

omega_variable(
    informal_economy_dependency,
    'What proportion of adults exiting welfare enter informal economy work (unreported income, gig work, barter) rather than formal employment?',
    'Longitudinal income survey matching: track post-termination income sources via tax records, in-person surveys, and bank transaction data; compare reported work with actual subsistence patterns',
    'If >50% are in informal economy: the constraint''s enforcement is largely fictional (compliance appears as employment but agents are actually outside the formal system). The snare classification would strengthen — the constraint forces agents to choose between precarious visibility or benefit loss.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(informal_economy_dependency, empirical, 'Proportion of terminated recipients in informal economy').

omega_variable(
    child_poverty_attribution,
    'Did the decline in child poverty post-1996 result from welfare reform, from economic growth and labor market conditions, or from other policy changes (EITC expansion, child tax credit)?',
    'Synthetic control analysis: compare child poverty trends in reform-implementing states vs non-implementing jurisdictions, controlling for economic conditions; decompose poverty change via microsimulation',
    'If reform was minor contributor (<20% of poverty reduction): the coordination function (children''s benefits) is not causally driven by the work requirement. The constraint is extractive from adults without clear reciprocal benefit to children. Classification shifts toward pure snare. If reform was major contributor (>60%): the tangled rope classification is confirmed — genuine coordination benefit to children.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(child_poverty_attribution, empirical, 'Causal attribution of child poverty reduction to welfare reform').

omega_variable(
    compliance_cost_burden,
    'What is the actual compliance cost (transportation, child care during appointments, documentation gathering) as a proportion of monthly benefit amount?',
    'Time-use and cost surveys of welfare recipients; administrative cost data on case management, monitoring, and verification infrastructure',
    'If compliance costs >25% of benefit: the effective assistance level is substantially lower than nominal benefit amount, and suppression is understated. If <5%: compliance burden is minor and may not materially affect exit options.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(compliance_cost_burden, empirical, 'Proportion of benefits consumed by compliance costs').

omega_variable(
    natural_law_vs_policy_choice,
    'Is the work requirement a natural law reflecting human nature and economic necessity, or a constructed policy choice that could be replaced with alternative coordination mechanisms?',
    'Comparative institutional analysis: examine welfare designs in peer democracies (Nordic unconditional cash transfers, EU job guarantees, other conditional systems) and evaluate distributional outcomes',
    'If work requirements are one of many viable designs (not unique): the mountain perspective is a false summit. The constraint naturalizes a contingent policy choice. If work requirements are the only economically sustainable design: mountain classification is justified. This omega reveals whether the ''natural law'' framing is accurate or ideological cover.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(natural_law_vs_policy_choice, conceptual, 'Whether work requirement is natural law or constructed policy').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sotu_2000_clinton_welfare_reform_work_requirement, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(welfare_theater_1996, sotu_2000_clinton_welfare_reform_work_requirement, theater_ratio, 0, 0.4).
narrative_ontology:measurement(welfare_theater_2000, sotu_2000_clinton_welfare_reform_work_requirement, theater_ratio, 4, 0.52).
narrative_ontology:measurement(welfare_theater_2006, sotu_2000_clinton_welfare_reform_work_requirement, theater_ratio, 10, 0.55).

% Extraction over time
narrative_ontology:measurement(welfare_extractiveness_1996, sotu_2000_clinton_welfare_reform_work_requirement, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(welfare_extractiveness_2000, sotu_2000_clinton_welfare_reform_work_requirement, base_extractiveness, 4, 0.48).
narrative_ontology:measurement(welfare_extractiveness_2006, sotu_2000_clinton_welfare_reform_work_requirement, base_extractiveness, 10, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sotu_2000_clinton_welfare_reform_work_requirement, resource_allocation).
narrative_ontology:affects_constraint(sotu_2000_clinton_welfare_reform_work_requirement, child_support_enforcement_system).
narrative_ontology:affects_constraint(sotu_2000_clinton_welfare_reform_work_requirement, disability_determination_gatekeeping).
narrative_ontology:affects_constraint(sotu_2000_clinton_welfare_reform_work_requirement, earned_income_tax_credit_coupling).
narrative_ontology:affects_constraint(sotu_2000_clinton_welfare_reform_work_requirement, time_limited_public_assistance).

% DUAL FORMULATION NOTE:
% This constraint decomposes into multiple structurally related stories: (1) the work requirement itself (this story, ε=0.52), (2) child support enforcement (ε=0.65, higher extraction on non-custodial parents), (3) disability gatekeeping (ε=0.48, extraction from disabled adults), (4) time-limit expiration (ε=0.58, sharp extraction as agents hit 60-month cap). These are linked via network because: work requirement drives child support enforcement (states prioritize collecting support from required workers); work requirement forces disabled adults into disability determination (those unable to work must claim disability to avoid time limits); time limits create the endpoint where the extraction mechanism reaches maximum force. Each story has its own base_properties and perspectives; they are linked here to show the institutional coupling.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(sotu_2000_clinton_welfare_reform_work_requirement, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
