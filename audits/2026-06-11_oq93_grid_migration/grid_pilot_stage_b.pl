% ============================================================================
% CONSTRAINT STORY: grid_pilot_stage_b
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2025-01-10
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_grid_pilot_stage_b, []).

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
    narrative_ontology:suppression_profile/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    domain_priors:emerges_naturally/1,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: grid_pilot_stage_b
 *   human_readable: Demographic Skill Mismatch in Blue-Collar Labor Markets
 *   domain: labor_economics/platform_economy/social_policy
 *
 * SUMMARY:
 *   The demographic skill mismatch in blue-collar labor markets describes a
 *   structural tension between the aging of the existing manual labor
 *   workforce (50% over 40) and the systematic avoidance of manual trades by
 *   workers born after 1990. This creates upward wage pressure for
 *   blue-collar work, which appears from most perspectives as a natural
 *   demographic reality — an immutable constraint arising from cohort
 *   preferences and educational attainment that no single actor can reverse.
 *   The constraint is claimed as Mountain because the demographic transition
 *   (rising educational attainment, declining birth rates, intergenerational
 *   mobility away from manual work) appears to be a structural feature of
 *   economic development observed across all industrialized economies.
 *   However, the presence of identifiable beneficiaries (aging workers who
 *   capture wage premiums, employers who justify automation investment and
 *   immigration recruitment, training institutions that capture public
 *   funding) triggers the false summit detector. The four omega variables
 *   document the irreducible uncertainties: Is this a genuine natural law of
 *   demographic transition, or a constructed constraint that naturalizes
 *   policy choices (education funding, immigration restrictions, occupational
 *   licensing) that created the scarcity? The analytical challenge is
 *   distinguishing immutable demographic reality from contingent
 *   institutional arrangements that benefit specific groups.
 *
 * KEY AGENTS:
 *   - Aging Blue-Collar Workers: Primary beneficiary (powerless/trapped) — capture wage premiums during scarcity period; cannot exit manual trades due to age and skill lock-in
 *   - Construction Firms: Primary beneficiary (institutional/constrained) — face labor scarcity as structural constraint; benefit from wage suppression via credentialism and immigration restrictions that limit competition
 *   - Manufacturing Employers: Primary beneficiary (institutional/mobile) — have exit options (automation, offshoring) but benefit from scarcity narrative that justifies these investments and public subsidy
 *   - Post-1990 Cohort: Implicit victim (powerless/mobile) — face credential barriers and status stigma if entering manual trades; their revealed preference for non-manual work is treated as immutable rather than responsive to incentives
 *   - Trade Union Coalition: Organized beneficiary (organized/constrained) — uses scarcity to justify wage floors and apprenticeship programs; genuine coordination function but also rent extraction via restricted supply
 *   - Vocational Training System: Institutional beneficiary (institutional/mobile) — captures public funding and enrollment growth justified by skills gap narrative; mixed coordination and extraction
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent policy choices as demographic inevitability
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(grid_pilot_stage_b, 0.15).
domain_priors:suppression_score(grid_pilot_stage_b, 0.2).
domain_priors:theater_ratio(grid_pilot_stage_b, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(grid_pilot_stage_b, extractiveness, 0.15).
narrative_ontology:constraint_metric(grid_pilot_stage_b, suppression_requirement, 0.2).
narrative_ontology:constraint_metric(grid_pilot_stage_b, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(grid_pilot_stage_b, accessibility_collapse, 0.85).
narrative_ontology:constraint_metric(grid_pilot_stage_b, resistance, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(grid_pilot_stage_b, mountain).
narrative_ontology:human_readable(grid_pilot_stage_b, "Demographic Skill Mismatch in Blue-Collar Labor Markets").
narrative_ontology:topic_domain(grid_pilot_stage_b, "labor_economics/platform_economy/social_policy").

domain_priors:emerges_naturally(grid_pilot_stage_b).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(grid_pilot_stage_b, aging_blue_collar_workers).
narrative_ontology:constraint_beneficiary(grid_pilot_stage_b, construction_firms).
narrative_ontology:constraint_beneficiary(grid_pilot_stage_b, manufacturing_employers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(grid_pilot_stage_b, vocational_training_system).
narrative_ontology:constraint_victim(grid_pilot_stage_b, post_1990_cohort).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Workers over 40 in construction, manufacturing, and logistics who cannot exit manual trades due to age and skill lock-in. They capture wage premiums during the scarcity period as younger cohorts avoid manual work. Their structural position is powerless (cannot change demographic trends or cohort preferences) but they benefit from the constraint through higher wages. Exit is trapped because retraining for non-manual work at age 40+ faces steep barriers.
narrative_ontology:constraint_stakeholder(grid_pilot_stage_b, aging_blue_collar_workers, beneficiary,
    powerless, biographical, trapped, national).

% Employers facing labor scarcity who benefit from the skills gap narrative in multiple ways: justifies automation investment, immigration recruitment, public subsidy for training programs, and wage suppression via credentialism. They experience higher labor costs but also capture rents through policy responses to the scarcity. Exit is constrained because they cannot easily offshore construction work or fully automate, but they have more options than individual workers.
narrative_ontology:constraint_stakeholder(grid_pilot_stage_b, construction_firms, beneficiary,
    institutional, generational, constrained, national).

% Employers with exit options (automation, offshoring, immigration recruitment) who benefit from the scarcity narrative to justify these investments and capture public subsidy. They face labor scarcity as a structural constraint but have more exit options than construction firms. The demographic mismatch justifies automation investment and immigration pathways that benefit the firm.
narrative_ontology:constraint_stakeholder(grid_pilot_stage_b, manufacturing_employers, beneficiary,
    institutional, generational, mobile, global).

% Workers born after 1990 who systematically avoid manual trades in favor of higher-education pathways. They are not forced out of manual trades but face credential barriers, status stigma, and informational gaps that make manual work less attractive despite rising wage premiums. If immigration restrictions or credential requirements are preventing them from entering trades, they are implicit victims of constructed scarcity. Exit is mobile because they have genuine alternatives in the broader labor market.
narrative_ontology:constraint_stakeholder(grid_pilot_stage_b, post_1990_cohort, payer,
    powerless, biographical, mobile, national).

% Organized labor uses the demographic scarcity to justify wage floors, apprenticeship programs, and immigration pathways. They set the agenda for training and recruitment policy. The constraint coordinates collective action around skill development but also enables rent extraction via restricted supply. Exit is constrained because unions depend on the scarcity narrative to maintain bargaining power.
narrative_ontology:constraint_stakeholder(grid_pilot_stage_b, trade_union_coalition, agenda_setter,
    organized, generational, constrained, national).

% Community colleges and trade schools that capture public funding and enrollment growth justified by the skills gap narrative. They provide genuine coordination (training workers for available jobs) but also extract rents through credential inflation and program expansion that may exceed actual skill needs. Exit is mobile because training institutions can pivot to other programs if the skills gap narrative weakens.
narrative_ontology:constraint_stakeholder(grid_pilot_stage_b, vocational_training_system, beneficiary,
    institutional, biographical, mobile, national).

% Observes the demographic mismatch as a structural feature of economic development. Sees the pattern (rising educational attainment leading to manual labor avoidance) as universal across industrialized economies, suggesting a natural law rather than a policy artifact. Risks naturalizing contingent policy choices (education funding, immigration restrictions, occupational licensing) as demographic inevitability.
narrative_ontology:constraint_stakeholder(grid_pilot_stage_b, analytical_observer, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The constraint coordinates the allocation of workers to manual trades by creating wage signals (premiums for scarce skills) and justifying training programs, apprenticeships, and immigration pathways. It solves the genuine problem of matching labor supply to demand in sectors where younger cohorts are underrepresented.
% TRANSFER_FUNCTION: The constraint transfers wage premiums to aging blue-collar workers, public funding to vocational training institutions, and policy justification to employers seeking automation investment or immigration recruitment. The transfer flows from younger cohorts (who face credential barriers and status stigma) and taxpayers (who fund training subsidies) to incumbent workers, training institutions, and employers.
% ABSENT_VOICES: Younger workers who might enter manual trades if wage premiums were more visible, working conditions improved, or status stigma reduced. Immigration advocates who argue that the scarcity is policy-constructed via immigration restrictions. These voices are excluded from the skills gap narrative, which treats cohort preferences as immutable rather than responsive to incentives.
% DISAPPEARANCE_RATIONALE: If the demographic mismatch disappeared overnight (younger cohorts suddenly entered manual trades at historical rates), the labor market would rearrange substantially: wage premiums would fall, training subsidies would decline, automation investment would slow, and immigration recruitment would decrease. The constraint organizes real economic activity — it is not a natural fact that would persist regardless of human arrangements.
% FOUNDING_PROBLEM: The founding problem was a genuine demographic transition: rising educational attainment and declining birth rates in industrialized economies led to fewer younger workers entering manual trades, creating labor scarcity in construction, manufacturing, and logistics. This is a real coordination problem — how to allocate workers to sectors where demand exceeds supply.
% FOUNDING_PROBLEM_CORROBORATION: The demographic transition is corroborated by census data, labor force surveys, and cross-national comparisons showing the same pattern across all industrialized economies. However, the policy responses (immigration restrictions, credential requirements, training subsidies) are contested. Labor economists and immigration advocates argue that the scarcity is partly policy-constructed, while employers and training institutions treat it as a natural demographic reality. The founding problem is live, but the policy framing is contested.
narrative_ontology:disappearance_verdict(grid_pilot_stage_b, world_rearranges).
narrative_ontology:founding_problem_status(grid_pilot_stage_b, live).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: AGING BLUE-COLLAR WORKER (MOUNTAIN) — Experiences demographic aging as an immutable fact. Cannot change the age distribution of the workforce or the career preferences of younger cohorts. The wage premium they receive appears as a natural market response to scarcity, not as extraction from any identifiable victim. From this seat, the constraint is a demographic reality that no policy can reverse.
constraint_indexing:constraint_classification(grid_pilot_stage_b, mountain,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: CONSTRUCTION FIRM (MOUNTAIN) — Faces labor scarcity as a structural constraint. Cannot force younger workers to enter manual trades. Wage increases are a necessary response to demographic reality, not a policy choice. The firm experiences this as an immutable market condition: the supply curve has shifted left due to cohort preferences that are beyond any single firm's control.
constraint_indexing:constraint_classification(grid_pilot_stage_b, mountain,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: MANUFACTURING EMPLOYER (MOUNTAIN) — Sees demographic mismatch as a global labor market reality. Has exit options (automation, offshoring, immigration recruitment) but these are responses to an underlying constraint, not alternatives that eliminate it. The demographic shift is treated as a natural law of labor supply: cohorts born after 1990 have different educational attainment and career preferences, and no policy intervention changes birth cohorts retroactively.
constraint_indexing:constraint_classification(grid_pilot_stage_b, mountain,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 4: TRADE UNION COALITION (ROPE) — Organized labor sees the demographic mismatch as a coordination opportunity. The scarcity creates bargaining power for existing workers and justifies apprenticeship programs, wage floors, and immigration pathways. The constraint coordinates collective action around training and recruitment. Extraction is low because the union's members are net beneficiaries of the wage premium, and the coordination function (matching workers to employers via training programs) is genuine.
constraint_indexing:constraint_classification(grid_pilot_stage_b, rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: VOCATIONAL TRAINING SYSTEM (TANGLED ROPE) — Community colleges and trade schools benefit from increased enrollment and public funding justified by the skills gap narrative. They provide genuine coordination (training workers for available jobs) but also extract rents through credential requirements and program expansion that may exceed actual skill needs. The system requires active enforcement (accreditation, licensing, public subsidy) and has both beneficiaries (training institutions, credentialed workers) and victims (students who pay for training that doesn't lead to employment, taxpayers funding programs with low completion rates).
constraint_indexing:constraint_classification(grid_pilot_stage_b, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (MOUNTAIN) — From a civilizational perspective, demographic transitions are structural features of economic development. The post-1990 cohort's preference for non-manual work reflects rising educational attainment, declining physical demands in the broader economy, and intergenerational mobility. These are not policy artifacts but emergent properties of development trajectories observed across all industrialized economies. The constraint appears as a natural law: cohorts with higher educational attainment systematically avoid manual labor, and this preference is not reversible by policy intervention within a single generation.
constraint_indexing:constraint_classification(grid_pilot_stage_b, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(grid_pilot_stage_b_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(grid_pilot_stage_b, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(grid_pilot_stage_b, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(grid_pilot_stage_b, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(grid_pilot_stage_b, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(grid_pilot_stage_b, ExtMetricName, E),
    domain_priors:suppression_score(grid_pilot_stage_b, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(grid_pilot_stage_b),
    narrative_ontology:constraint_metric(grid_pilot_stage_b, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(grid_pilot_stage_b, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(grid_pilot_stage_b_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.15): Low. The wage premium captured by aging workers and the rents captured by training institutions are modest relative to total labor market flows. Much of the wage increase is a genuine market response to scarcity rather than extraction from identifiable victims. The post-1990 cohort is not forced out of manual trades — they are choosing higher-education pathways that offer better lifetime earnings. The extraction is primarily in the form of credential inflation (training requirements that exceed actual skill needs) and immigration restrictions (limiting supply to maintain wage premiums), both of which are real but not severe. Suppression (0.20): Low. The constraint does not actively prevent younger workers from entering manual trades. The barriers are primarily cultural (status stigma), informational (lack of awareness of wage premiums), and credential-based (licensing requirements), not coercive. Workers have genuine exit options into other sectors. Accessibility collapse (0.85): High. Once the demographic reality is understood — cohorts with higher educational attainment systematically avoid manual labor across all industrialized economies — alternative framings collapse. The pattern is robust across countries with different policies, suggesting a structural demographic feature rather than a policy artifact. Resistance (0.10): Very low. The demographic mismatch narrative meets almost no organized resistance. Labor unions, employers, training institutions, and policymakers all accept the skills gap framing. The only resistance comes from immigration advocates who argue that the scarcity is policy-constructed, but this is a minority position. Theater ratio (0.10): Very low. The constraint is not performative. The demographic data (age distribution, cohort participation rates, wage premiums) are directly observable and not contested. The skills gap narrative is not theater — it describes a real labor market phenomenon, even if the policy responses (training subsidies, immigration restrictions) may be extractive.
 *
 * PERSPECTIVAL GAP:
 *   The aging worker and the construction firm both see Mountain — an immutable demographic reality that no policy can reverse within a generation. The trade union sees Rope — a coordination opportunity that justifies collective bargaining and training programs. The vocational training system sees Tangled Rope — genuine coordination (skill training) mixed with extraction (credential inflation, public subsidy capture). The analytical observer sees Mountain but is at risk of naturalizing contingent policy choices (education funding, immigration restrictions, occupational licensing) as demographic inevitability. The critical perspectival gap is between the Mountain view (demographic transitions are natural laws of development) and the Tangled Rope or Snare view (the scarcity is policy-constructed and benefits identifiable groups). The four omega variables document the empirical tests that would resolve this gap: cross-national policy variation, cohort preference elasticity, automation substitution thresholds, and immigration counterfactuals.
 *
 * DIRECTIONALITY LOGIC:
 *   Aging blue-collar workers are declared beneficiaries because they capture wage premiums during the scarcity period. Their directionality is derived from beneficiary status plus trapped exit (they cannot leave manual trades due to age and skill lock-in), which produces low d and low effective extraction — they experience the constraint as a market opportunity, not as extraction. Construction firms and manufacturing employers are declared beneficiaries because they benefit from the scarcity narrative (justifies automation investment, immigration recruitment, public subsidy for training programs) even though they also face higher labor costs. Their directionality is derived from beneficiary status plus constrained or mobile exit, producing low d. The post-1990 cohort is not explicitly listed as a victim because they are not forced out of manual trades — they are choosing alternative pathways. However, if immigration restrictions or credential barriers are preventing them from entering trades despite wage premiums, they are implicit victims of a constructed scarcity. The vocational training system is both beneficiary (captures public funding) and coordinator (provides genuine skill training), which is the structural signature of Tangled Rope from that perspective. The analytical observer sees the constraint as Mountain because the demographic pattern (rising educational attainment leading to manual labor avoidance) appears universal across industrialized economies, suggesting a natural law of development rather than a policy artifact. However, the false summit detector will flag this because beneficiaries are declared — the question is whether the beneficiaries are incidental to a natural demographic shift or products of constructed scarcity.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint resolves the mandatrophy by showing that the demographic mismatch can be simultaneously a natural demographic reality (Mountain from most perspectives) and a constructed constraint that benefits identifiable groups (Tangled Rope or Snare from perspectives that see the policy mechanisms). The mandatrophy is not 'which type is correct?' but 'which structural features are you measuring?' The demographic transition (rising educational attainment, declining birth rates) is a genuine structural feature of economic development. The policy responses (immigration restrictions, credential requirements, training subsidies) are contingent institutional arrangements that may naturalize and extract from the transition. The false summit detector identifies the risk: a constraint presented as natural law (demographic aging is immutable) but with identifiable beneficiaries (aging workers, employers, training institutions) who benefit from policy choices that restrict supply. The omega variables document the irreducible uncertainty: Is the scarcity a natural demographic reality, or is it constructed by policies that benefit specific groups?
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    beneficiary_identification_ambiguity,
    'Are aging blue-collar workers and their employers genuine beneficiaries of a natural demographic shift, or are they beneficiaries of a constructed constraint that naturalizes policy choices (education funding, immigration restrictions, occupational licensing) that created the scarcity?',
    'Cross-national comparison: do countries with different education policies, immigration regimes, and licensing requirements show the same demographic mismatch pattern? If the pattern is universal, it supports the natural law claim. If it varies systematically with policy, the constraint is constructed.',
    'If natural: Mountain classification is correct, beneficiaries are incidental. If constructed: False summit — the constraint naturalizes policy choices that benefit identifiable groups (employers who capture wage suppression via credentialism, training institutions that capture public funding, incumbent workers who capture wage premiums via restricted supply).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_identification_ambiguity, empirical, 'Whether beneficiaries are incidental to a natural demographic shift or products of constructed scarcity').

omega_variable(
    cohort_preference_mutability,
    'Are post-1990 cohort preferences for non-manual work immutable (reflecting irreversible educational and cultural shifts) or responsive to incentives (wage premiums, working conditions, status signaling)?',
    'Longitudinal analysis of cohort entry into manual trades in response to wage changes. If entry rates are highly elastic to wages, preferences are mutable and the constraint is a coordination problem (Rope). If entry rates are inelastic, preferences are structural and the constraint is closer to Mountain.',
    'If mutable: The constraint is a coordination failure (insufficient wage signals, poor working conditions, status stigma) that policy can address. If immutable: The constraint is a demographic reality that policy cannot reverse within a generation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(cohort_preference_mutability, empirical, 'Whether cohort career preferences are responsive to wage incentives or structurally fixed').

omega_variable(
    automation_substitution_threshold,
    'At what wage premium does automation become cost-effective enough to eliminate the labor scarcity, and is that threshold within the range of observed wage increases?',
    'Engineering cost analysis of automation technologies for manual tasks; comparison to observed wage trajectories in construction, manufacturing, and logistics. If automation threshold is below current wage premiums, the constraint is self-limiting (Scaffold with a technological sunset). If threshold is far above, the constraint persists as a structural feature (Mountain).',
    'If automation is imminent: The demographic mismatch is a temporary transition problem (Scaffold). If automation is distant: The mismatch is a persistent structural constraint (Mountain or Tangled Rope depending on policy response).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(automation_substitution_threshold, empirical, 'Whether automation provides a near-term exit from labor scarcity').

omega_variable(
    immigration_policy_counterfactual,
    'Would the demographic mismatch exist under a different immigration regime, or is it a product of immigration restrictions that limit the supply of younger workers willing to enter manual trades?',
    'Cross-national comparison of countries with similar demographic transitions but different immigration policies. If countries with open immigration show no mismatch, the constraint is policy-constructed. If the mismatch is universal, it is demographic.',
    'If immigration-dependent: The constraint is a Snare or Tangled Rope — immigration restrictions extract from younger workers (who face credential barriers) and benefit incumbent workers and employers (who capture wage premiums and subsidy). If immigration-independent: The constraint is closer to Mountain — a genuine demographic reality.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(immigration_policy_counterfactual, empirical, 'Whether immigration policy is a necessary condition for the observed mismatch').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(grid_pilot_stage_b, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(demo_skill_tr_t0, grid_pilot_stage_b, theater_ratio, 0, 0.05).
narrative_ontology:measurement(demo_skill_tr_t5, grid_pilot_stage_b, theater_ratio, 5, 0.08).
narrative_ontology:measurement(demo_skill_tr_t10, grid_pilot_stage_b, theater_ratio, 10, 0.1).

% Extraction over time
narrative_ontology:measurement(demo_skill_be_t0, grid_pilot_stage_b, base_extractiveness, 0, 0.1).
narrative_ontology:measurement(demo_skill_be_t5, grid_pilot_stage_b, base_extractiveness, 5, 0.12).
narrative_ontology:measurement(demo_skill_be_t10, grid_pilot_stage_b, base_extractiveness, 10, 0.15).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(grid_pilot_stage_b, static).

% Leveled coercion grid (OQ-93): 32/32 authored points at t0=0, tn=10
narrative_ontology:measurement(demo_skill_grid_01, grid_pilot_stage_b, accessibility_collapse(class), 0, 0.2).
narrative_ontology:measurement(demo_skill_grid_02, grid_pilot_stage_b, accessibility_collapse(class), 10, 0.8).
narrative_ontology:measurement(demo_skill_grid_03, grid_pilot_stage_b, accessibility_collapse(individual), 0, 0.2).
narrative_ontology:measurement(demo_skill_grid_04, grid_pilot_stage_b, accessibility_collapse(individual), 10, 0.8).
narrative_ontology:measurement(demo_skill_grid_05, grid_pilot_stage_b, accessibility_collapse(organizational), 0, 0.2).
narrative_ontology:measurement(demo_skill_grid_06, grid_pilot_stage_b, accessibility_collapse(organizational), 10, 0.8).
narrative_ontology:measurement(demo_skill_grid_07, grid_pilot_stage_b, accessibility_collapse(structural), 0, 0.2).
narrative_ontology:measurement(demo_skill_grid_08, grid_pilot_stage_b, accessibility_collapse(structural), 10, 0.8).
narrative_ontology:measurement(demo_skill_grid_09, grid_pilot_stage_b, resistance(class), 0, 0.2).
narrative_ontology:measurement(demo_skill_grid_10, grid_pilot_stage_b, resistance(class), 10, 0.8).
narrative_ontology:measurement(demo_skill_grid_11, grid_pilot_stage_b, resistance(individual), 0, 0.2).
narrative_ontology:measurement(demo_skill_grid_12, grid_pilot_stage_b, resistance(individual), 10, 0.8).
narrative_ontology:measurement(demo_skill_grid_13, grid_pilot_stage_b, resistance(organizational), 0, 0.2).
narrative_ontology:measurement(demo_skill_grid_14, grid_pilot_stage_b, resistance(organizational), 10, 0.8).
narrative_ontology:measurement(demo_skill_grid_15, grid_pilot_stage_b, resistance(structural), 0, 0.2).
narrative_ontology:measurement(demo_skill_grid_16, grid_pilot_stage_b, resistance(structural), 10, 0.8).
narrative_ontology:measurement(demo_skill_grid_17, grid_pilot_stage_b, stakes_inflation(class), 0, 0.2).
narrative_ontology:measurement(demo_skill_grid_18, grid_pilot_stage_b, stakes_inflation(class), 10, 0.8).
narrative_ontology:measurement(demo_skill_grid_19, grid_pilot_stage_b, stakes_inflation(individual), 0, 0.2).
narrative_ontology:measurement(demo_skill_grid_20, grid_pilot_stage_b, stakes_inflation(individual), 10, 0.8).
narrative_ontology:measurement(demo_skill_grid_21, grid_pilot_stage_b, stakes_inflation(organizational), 0, 0.2).
narrative_ontology:measurement(demo_skill_grid_22, grid_pilot_stage_b, stakes_inflation(organizational), 10, 0.8).
narrative_ontology:measurement(demo_skill_grid_23, grid_pilot_stage_b, stakes_inflation(structural), 0, 0.2).
narrative_ontology:measurement(demo_skill_grid_24, grid_pilot_stage_b, stakes_inflation(structural), 10, 0.8).
narrative_ontology:measurement(demo_skill_grid_25, grid_pilot_stage_b, suppression(class), 0, 0.2).
narrative_ontology:measurement(demo_skill_grid_26, grid_pilot_stage_b, suppression(class), 10, 0.8).
narrative_ontology:measurement(demo_skill_grid_27, grid_pilot_stage_b, suppression(individual), 0, 0.2).
narrative_ontology:measurement(demo_skill_grid_28, grid_pilot_stage_b, suppression(individual), 10, 0.8).
narrative_ontology:measurement(demo_skill_grid_29, grid_pilot_stage_b, suppression(organizational), 0, 0.2).
narrative_ontology:measurement(demo_skill_grid_30, grid_pilot_stage_b, suppression(organizational), 10, 0.8).
narrative_ontology:measurement(demo_skill_grid_31, grid_pilot_stage_b, suppression(structural), 0, 0.2).
narrative_ontology:measurement(demo_skill_grid_32, grid_pilot_stage_b, suppression(structural), 10, 0.8).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(grid_pilot_stage_b, resource_allocation).

% DUAL FORMULATION NOTE:
% The demographic skill mismatch is a single constraint with multiple structural interpretations. It is not decomposed into separate stories because the base extractiveness (0.15) is stable across observables — the wage premium and cohort participation rates are different measurements of the same underlying phenomenon. The constraint family would include related stories about immigration policy, occupational licensing, and education funding, but those are distinct constraints with their own extractiveness values.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
