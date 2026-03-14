% ============================================================================
% CONSTRAINT STORY: uk_unpaid_care_labor_trap
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_uk_unpaid_care_labor_trap, []).

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
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: uk_unpaid_care_labor_trap
 *   human_readable: UK Unpaid Care Labor Trap
 *   domain: social_policy/gender_economics/care_work
 *
 * SUMMARY:
 *   The UK unpaid care labor trap is a structural snare that extracts care
 *   work from predominantly female household members by fusing obligation
 *   (relational/identity-based) with material coercion (economic dependency,
 *   lack of service alternatives, welfare system dependency). The constraint
 *   operates through multiple binding mechanisms: family obligation norms
 *   that become internalized identity; economic dependency created by exit
 *   barriers; state welfare system design that outsources care provision to
 *   unpaid family members rather than funding public services; and
 *   institutional narratives that frame care as a moral good while
 *   systematically underfunding alternatives. The constraint has intensified
 *   over the measurement interval (0–30 years, roughly 1995–2025) as UK
 *   welfare state retrenchment has shifted care responsibility toward
 *   families, while labor market participation expectations for women have
 *   remained high, creating a squeeze between obligation and economic
 *   necessity. The extractiveness has risen from 0.45 to 0.68 as austerity
 *   policies have reduced public care funding, forcing more intensive unpaid
 *   household labor. Theater ratio has risen from 0.50 to 0.65 as policy
 *   institutions increasingly frame cuts to care services as 'supporting
 *   family autonomy' and 'respecting cultural values' — performative language
 *   that masks extraction.
 *
 * KEY AGENTS:
 *   - Unpaid Family Caregivers (predominantly female, primary victims): trapped by dependency structures, lack of alternatives, and identity fusion with care role. Experience full extractive force.
 *   - Care Recipients (elderly, disabled, children): structurally mobile but identity-locked through internalized obligation and shame. Could access paid services but perceive this as family abandonment.
 *   - State Welfare System (institutional beneficiary): arbitrages unpaid family care as substitute for public service provision. Experiences constraint as pure coordination benefit.
 *   - Employers of Care Recipients (secondary beneficiaries): avoid wage labor and employment obligations through family care substitution.
 *   - Feminist Care Advocacy Coalition (organized agents): recognize the constraint as hybrid coordination-extraction; advocate for policy alternatives (universal care services, carer's wage, paid family leave).
 *   - Family Care Norm Institutions (religious organizations, social services): maintain performative celebration of family caregiving while structurally producing the obligation.
 *   - Analytical Observer (civilizational view): at risk of naturalizing contingent institutional choice as immutable kinship law.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(uk_unpaid_care_labor_trap, 0.68).
domain_priors:suppression_score(uk_unpaid_care_labor_trap, 0.75).
domain_priors:theater_ratio(uk_unpaid_care_labor_trap, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(uk_unpaid_care_labor_trap, extractiveness, 0.68).
narrative_ontology:constraint_metric(uk_unpaid_care_labor_trap, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(uk_unpaid_care_labor_trap, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(uk_unpaid_care_labor_trap, snare).
narrative_ontology:human_readable(uk_unpaid_care_labor_trap, "UK Unpaid Care Labor Trap").
narrative_ontology:topic_domain(uk_unpaid_care_labor_trap, "social_policy/gender_economics/care_work").

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(uk_unpaid_care_labor_trap, state_welfare_system).
narrative_ontology:constraint_beneficiary(uk_unpaid_care_labor_trap, employed_care_recipients).
narrative_ontology:constraint_beneficiary(uk_unpaid_care_labor_trap, employers_avoiding_wage_labor).
narrative_ontology:constraint_victim(uk_unpaid_care_labor_trap, unpaid_care_workers).
narrative_ontology:constraint_victim(uk_unpaid_care_labor_trap, female_labor_force_participation).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: UNPAID CAREGIVER (SNARE) — Trapped by dependency structures: if they exit unpaid care work, care recipients lose access to subsidized services, and the caregiver faces severe penalties (lost housing, custody issues, stigma). No meaningful alternative available. Maximum experienced extraction — the constraint transforms care obligation into coercive labor.
constraint_indexing:constraint_classification(uk_unpaid_care_labor_trap, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: SECONDARY FAMILY MEMBERS (TANGLED ROPE) — Face high but surmountable costs to exit (family obligation, social stigma, relationship rupture). Constrained rather than trapped — they can exit by paying enormous relational and reputational costs. The constraint coordinates genuine family caregiving needs alongside asymmetric labor extraction.
constraint_indexing:constraint_classification(uk_unpaid_care_labor_trap, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: STATE WELFARE SYSTEM (ROPE) — Experiences the constraint as pure coordination: unpaid family care solves the collective action problem of care service provision without raising welfare spending. The state benefits from arbitrage — family obligation performs service delivery at zero budget cost. No victim relationship from the state's perspective, only coordination benefit.
constraint_indexing:constraint_classification(uk_unpaid_care_labor_trap, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: CARE RECIPIENT (SNARE / IDENTITY-LOCKED) — Structurally mobile (could access state services, could hire care workers) but identity-locked through internalized obligation and shame. The recipient's self-concept is fused with family caregiving — accepting paid care feels like abandoning family duty or admitting need. Exit from the constraint requires becoming a different person (from 'cared for by family' to 'consumer of care services'), not just paying a cost.
constraint_indexing:constraint_classification(uk_unpaid_care_labor_trap, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(national))).

% PERSPECTIVE 5: FAMILY CARE NORM INSTITUTIONS (PITON) — Religious institutions, social services frameworks, and policy language performatively celebrate family care as a moral good while structurally producing the obligation that funds the underfunded welfare state. The institutional function (moral reinforcement of the care obligation) has largely decoupled from its original purpose (community solidarity in pre-welfare-state societies). Maintained by theater and institutional inertia.
constraint_indexing:constraint_classification(uk_unpaid_care_labor_trap, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: FEMINIST ADVOCACY COALITION (TANGLED ROPE) — Organized actors (Carers UK, women's rights organizations) see the constraint as hybrid: it genuinely coordinates family care relationships (some agents do benefit from obligation-based caregiving) alongside severe asymmetric extraction from predominantly female carers. The coalition experiences the constraint as contestable — policy alternatives exist (universal care services, carer's wage, paid family leave), but building them requires overcoming the state's arbitrage incentive to preserve unpaid labor extraction.
constraint_indexing:constraint_classification(uk_unpaid_care_labor_trap, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURALIZATION RISK (MOUNTAIN) — From a civilizational perspective, family caregiving appears as a natural law: kinship obligation is inherent to human society; care for dependents is an immutable responsibility. However, the structural data contradicts the mountain classification. Empirically, unpaid care work is highly variable across societies and eras (Scandinavia has socialized care; postcolonial societies have different kinship structures; pre-industrial societies had different dependency patterns). The 'natural law' framing naturalizes a contingent institutional choice: the UK welfare state's decision to fund care through family obligation rather than public provision.
constraint_indexing:constraint_classification(uk_unpaid_care_labor_trap, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(uk_unpaid_care_labor_trap_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(uk_unpaid_care_labor_trap, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(uk_unpaid_care_labor_trap, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(uk_unpaid_care_labor_trap, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(uk_unpaid_care_labor_trap, TR),
    TR >= 0.70.

:- end_tests(uk_unpaid_care_labor_trap_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. The primary caregiver's labor generates value (care provision, support for employment of care recipients, implicit subsidy to state welfare system) that flows primarily to the care recipient and the state, not to the caregiver. The caregiver is typically unpaid; if they work outside the home, they sacrifice earnings. The extraction is not maximal (0.95) because some caregivers do benefit from care relationships (relational intimacy, sense of purpose, reciprocal family support), but the asymmetry is severe. Suppression (0.75): High. Multiple barriers prevent exit: (1) Material dependency — caregivers often have no independent income, cannot afford care services to replace their own work, face poverty if they exit. (2) Lack of alternatives — public care services are underfunded; paid care work is low-wage. (3) Family and social obligation — exiting means rupturing family relationships, incurring severe stigma. (4) Identity fusion — many caregivers cannot imagine themselves outside the care role. (5) Structural penalty — exiting can result in loss of benefits, custody disputes, economic collapse. Theater ratio (0.65): Moderate-high. Policy institutions use performative language ('supporting family values,' 'respecting cultural autonomy,' 'enabling dignity through family care') that masks extraction and cost-shifting. The National Health Service and local authority social services celebrate family caregiving as a moral good while systematically underfunding alternatives. Care work itself is invisible in national accounts (not counted as labor, not valued in economic statistics), which performs theater — it makes the extraction harder to see.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap between snare (caregiver's experience) and rope (state's experience) reveals the constraint's core asymmetry. The caregiver sees no choice, no benefit, only obligation and cost. The state sees a cost-saving mechanism that solves a coordination problem (ensuring care provision without raising spending). From the caregiver's biographical perspective, the constraint is immutable (mountain at trapped exit options, but the structural data contradicts this — accessibility_collapse and resistance are not high enough). From the state's immediate perspective, the constraint is beneficial with low extraction (pure coordination, rope). The identity-locked perspective on care recipients reveals a different gap: recipients are structurally mobile (could hire care, could access services) but identity-locked (cannot imagine being served rather than cared-for-by-family). This gap between structural mobility and perceived immutability is exactly the diagnostic signal for identity_locked exit options — the binding mechanism is cognitive/relational, not material.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality logic focuses on who benefits and who bears costs. Beneficiaries are: (1) the state welfare system, which outsources care provision without funding it; (2) employers of care recipients, who avoid paying for dependent care through family obligation; (3) care recipients who receive unpaid care (though often with identity-locked discomfort). Victims are: (1) unpaid caregivers, whose labor is extracted without payment and whose identity becomes fused with care work; (2) female labor force participation, suppressed because women are channeled into unpaid care. The caregiver's d value is high (0.92) because they bear full costs with minimal benefit. The state's d value is low (0.08) because it captures the coordination benefit with zero cost. The care recipient's d is mixed (0.60–0.70) because they receive care but incur identity-lock costs. The feminist advocacy coalition's perspective differentiates based on whether the analyzed case shows genuine coordination (both parties benefit) or extraction (one party bears costs while the other benefits). In most cases, the analysis lands on tangled rope: some coordination function exists, but asymmetric extraction is baked into the structural design.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION: The constraint resolves the mandatrophy by clarifying what 'care coordination' means. A naive reading treats unpaid family caregiving as 'pure coordination' (rope): family members voluntarily organize care for dependents, solving a collective action problem. But this collapses the victim and beneficiary relationship. A more precise analysis reveals tangled rope: the constraint does coordinate some care relationships (genuine familial obligation exists; some agents genuinely benefit from family caregiving), BUT it simultaneously extracts labor from predominantly female, lower-income caregivers through suppression and identity lock. The snare classification (from the primary caregiver's perspective) is not contradicted by the rope classification (from the state's perspective) — they are measuring from different structural positions with different power levels and exit options. The mandatrophy is resolved by showing that the constraint is hybrid: (1) It genuinely solves a coordination problem (care provision) for some agents (state, care recipients, employers). (2) It simultaneously imposes severe asymmetric extraction on others (unpaid caregivers). The policy implication: the coordination function does NOT justify the extraction. Alternative mechanisms (public care services, carer's wage, paid family leave) can provide the coordination function without the extraction. The natural law framing ('families care for each other' — mountain view) obscures this policy choice. The analytical observer's risk is to cite family caregiving as inherent/natural and therefore justify the current distribution of costs and benefits. In fact, the current system's extractiveness (0.68) is contingent on policy design (public vs private care provision, mandatory vs optional obligation framing). Societies with higher public care spending have lower extractiveness from family caregivers and better caregiver outcomes — proving the mountain is false.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    care_identity_lock_vs_constrained_boundary,
    'For a given caregiver, is their barrier to exit structural (high external costs, no alternatives) or identity-based (their self-concept is fused with care provision)?',
    'Longitudinal tracking of caregivers post-exit: if the caregiver retains guilt, shame, and identity confusion after exiting, suppression is partially internalized (identity-locked). If they experience relief and identity consolidation, the barrier was primarily structural (constrained or trapped).',
    'If internalized: the caregiver carries suppression with them after exit; recovery requires identity reconstruction beyond material relief. If structural: exiting and addressing external barriers (access to services, financial support) resolves the suppression. Classification boundary: identity_locked→constrained at biographical time if the agent''s identity frame could shift; remains mountain if identity reconstruction is not structurally possible.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(care_identity_lock_vs_constrained_boundary, empirical, 'Boundary between structural and identity-based suppression in care labor').

omega_variable(
    welfare_state_arbitrage_versus_genuine_coordination,
    'Does the state''s benefit from unpaid family care constitute genuine coordination of care delivery, or is it purely extractive arbitrage (substituting unpaid for paid labor)?',
    'Comparative analysis: in welfare states with higher public care spending (e.g., Nordic countries), do family care obligations decrease AND care quality/accessibility improve? If yes, the unpaid labor was substitution. If care quality remains stable or public spending simply adds to unpaid work, the unpaid labor was genuine coordination.',
    'If pure arbitrage: state perspective should downgrade to snare (the state is extracting care labor). If genuine coordination: rope classification is correct (some agents genuinely benefit from obligation-based arrangements). If mixed: tangled_rope is correct for the state''s perspective.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(welfare_state_arbitrage_versus_genuine_coordination, empirical, 'Whether state benefit is genuine coordination or extractive arbitrage').

omega_variable(
    gender_asymmetry_mechanism,
    'Is care work gendered because of biological/care-specific factors (e.g., women are better at relationship maintenance), or because of institutional sorting (e.g., women are excluded from other labor and channeled into care)?',
    'Cross-cultural comparison of care responsibility by gender; analysis of historical change in gender-care correlation as labor markets changed; study of agents who exit gender-conventional care roles (e.g., men as primary carers, women in paid care work).',
    'If biological: some degree of gender asymmetry in care work is natural variation, not extractive sorting. If institutional: the gender asymmetry IS a mechanism of extraction — care work is institutionally feminized as a means to suppress wages and exert control.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(gender_asymmetry_mechanism, empirical, 'Whether care gender asymmetry is biological or institutional').

omega_variable(
    suppression_internalization_rate,
    'What proportion of the measured suppression (0.75) is internalized (cognitive/identity-based, persists after material barriers removed) versus structural (external barriers, material costs)?',
    'Caregiver survey data on barriers to exit, pre- and post-exit psychological state, engagement with alternative identities and roles, change in self-blame and shame trajectories after material support is provided.',
    'If ≥60% internalized: identity_locked dominates the exit barrier; policy interventions must include identity work (therapy, alternative role modeling, cultural narrative change). If ≤40% internalized: material interventions (paid care services, carer''s wage, childcare) will be sufficient to enable exit.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_internalization_rate, empirical, 'Proportion of suppression that is internalized versus structural').

omega_variable(
    alternative_care_model_feasibility,
    'Can the UK transition to a Scandinavian-style public care model (universal subsidized care services) while maintaining coordination of care quality and relationship stability?',
    'Policy experiment: pilot universal care service provision in a region; measure coordination outcomes (care quality, relationship continuity, caregiver-recipient satisfaction), cost, and care worker labor conditions. Compare to current unpaid family model.',
    'If feasible with equivalent outcomes: the current unpaid model is not necessary — it is a choice to extract rather than to fund. Snare classification is robust. If public model degrades care quality or continuity: some genuine coordination function is being lost; constraint might downgrade to tangled_rope. If public model succeeds better: current constraint is pure extraction disguised as natural obligation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_care_model_feasibility, empirical, 'Feasibility of alternative public care models').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(uk_unpaid_care_labor_trap, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(uk_care_tr_t0, uk_unpaid_care_labor_trap, theater_ratio, 0, 0.5).
narrative_ontology:measurement(uk_care_tr_t15, uk_unpaid_care_labor_trap, theater_ratio, 15, 0.6).
narrative_ontology:measurement(uk_care_tr_t30, uk_unpaid_care_labor_trap, theater_ratio, 30, 0.65).

% Extraction over time
narrative_ontology:measurement(uk_care_be_t0, uk_unpaid_care_labor_trap, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(uk_care_be_t15, uk_unpaid_care_labor_trap, base_extractiveness, 15, 0.58).
narrative_ontology:measurement(uk_care_be_t30, uk_unpaid_care_labor_trap, base_extractiveness, 30, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(uk_unpaid_care_labor_trap, attachment_coordination).
narrative_ontology:boltzmann_floor_override(uk_unpaid_care_labor_trap, 0.12).
narrative_ontology:affects_constraint(uk_unpaid_care_labor_trap, uk_female_labor_force_participation_penalty).
narrative_ontology:affects_constraint(uk_unpaid_care_labor_trap, uk_pension_entitlement_gaps_by_care_history).
narrative_ontology:affects_constraint(uk_unpaid_care_labor_trap, uk_informal_economy_wage_suppression).

% DUAL FORMULATION NOTE:
% This story is part of a constraint family linking UK care policy, labor market outcomes, and pension inequality. Unpaid care labor traps individual caregivers (this story) and systematically suppress female earnings (labor participation penalty story), which compounds across the lifespan into pension gaps (retirement security story). Each story has its own ε value: this one focuses on the direct extraction of care labor (ε=0.68); the labor participation story focuses on earnings suppression (ε=0.55); the pension story focuses on retirement-age extraction (ε=0.62). They are linked via network.affects_constraints because policy changes to one alter all three.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(uk_unpaid_care_labor_trap, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
