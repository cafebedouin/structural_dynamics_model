% ============================================================================
% CONSTRAINT STORY: china_reproductive_coercion_history
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_china_reproductive_coercion_history, []).

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
 *   constraint_id: china_reproductive_coercion_history
 *   human_readable: China Reproductive Coercion History (1979-2015)
 *   domain: population_policy/human_rights/state_power
 *
 * SUMMARY:
 *   China's One-Child Policy (1979-2015) represents a paradigm case of state
 *   reproductive coercion — extraction through direct physical coercion
 *   (forced abortion, sterilization), economic penalties (income fines, job
 *   loss, loss of benefits), and institutional deprivation (denial of hukou
 *   registration to unauthorized children, blocking educational and
 *   employment access). The constraint operated across 36 years and affected
 *   approximately 400 million women and their families. Extractiveness peaked
 *   in the 1990s-2000s (0.82-0.85) during intensive enforcement campaigns,
 *   then declined sharply after 2015 policy reversal but did not drop to
 *   pre-policy baseline — residual suppression (internalized wariness,
 *   institutional surveillance, social stigma) remains embedded in
 *   reproductive norms and family planning infrastructure. The constraint
 *   exhibits the full Snare signature: high extraction, high suppression,
 *   minimal coordination value, and existence dependent on enforcement. The
 *   state's coordination rationale (demographic sustainability) does not
 *   survive comparative analysis — equivalent demographic transitions
 *   occurred in South Korea and Taiwan through incentive-based family
 *   planning. The Snare classification is robust from all non-state
 *   perspectives and survives mandate challenge.
 *
 * KEY AGENTS:
 *   - Reproductive-age women (Han, minority): Primary victim (powerless/trapped) — bore forced abortion, sterilization, economic penalties, social stigma. No exit options; suppression was total and lifelong.
 *   - Unauthorized children: Secondary victim (powerless/trapped, generational) — denied hukou registration, educational access, employment, healthcare. Trapped by parental constraints not of their choosing; extraction operated across lifetime.
 *   - Local cadre (village/township level): Implementer (organized/constrained) — faced mixed coordination burden and asymmetric extraction. Evaluated on quota enforcement; career advancement tied to compliance; also bore pressure to minimize humanitarian fallout.
 *   - Central planning apparatus: Institutional beneficiary (institutional/arbitrage) — framed policy as coordination; could adjust or reverse enforcement without structural collapse. Exit options existed; chose continued enforcement.
 *   - Ethnic minorities: Stratified victim (powerless/trapped) — evidence suggests differential enforcement intensity; constraints on reproductive autonomy intersected with broader marginalization.
 *   - International observers: Analytical position (analytical/analytical) — documented coercion as human rights violation; external to enforcement apparatus; observed state rationale as insufficient justification for extraction magnitude.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(china_reproductive_coercion_history, 0.78).
domain_priors:suppression_score(china_reproductive_coercion_history, 0.85).
domain_priors:theater_ratio(china_reproductive_coercion_history, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(china_reproductive_coercion_history, extractiveness, 0.78).
narrative_ontology:constraint_metric(china_reproductive_coercion_history, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(china_reproductive_coercion_history, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(china_reproductive_coercion_history, snare).
narrative_ontology:human_readable(china_reproductive_coercion_history, "China Reproductive Coercion History (1979-2015)").
narrative_ontology:topic_domain(china_reproductive_coercion_history, "population_policy/human_rights/state_power").

domain_priors:requires_active_enforcement(china_reproductive_coercion_history).

% --- Structural relationships ---
narrative_ontology:constraint_victim(china_reproductive_coercion_history, reproductive_age_women).
narrative_ontology:constraint_victim(china_reproductive_coercion_history, han_minority_women).
narrative_ontology:constraint_victim(china_reproductive_coercion_history, rural_populations).
narrative_ontology:constraint_victim(china_reproductive_coercion_history, intergenerational_families).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: COERCED WOMAN (SNARE) — Women of reproductive age under the One-Child Policy bore maximum extraction with no exit options. Structural barriers: legal prohibition on unauthorized births, economic penalties (job loss, fine equaling years of income), forced abortion/sterilization, social ostracism, loss of hukou registration for unauthorized children. No alternative pathway existed; exit capacity was zero. Maximum suppression (0.85), maximum experienced extraction.
constraint_indexing:constraint_classification(china_reproductive_coercion_history, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: UNAUTHORIZED CHILDREN (SNARE, GENERATIONAL) — Children born in violation of quota bore lifetime extraction through denial of hukou status, educational access, employment opportunity, and healthcare coverage. The constraint operated across generations — unauthorized children were trapped not by their choices but by parental reproductive decisions constrained by state coercion. Suppression remained total across lifetime.
constraint_indexing:constraint_classification(china_reproductive_coercion_history, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 3: LOCAL CADRE (TANGLED ROPE) — Village and township-level administrators faced genuine coordination burden (tracking reproductive compliance, managing quota enforcement) alongside asymmetric extraction (quota targets, performance pressure, career advancement tied to enforcement). Extraction ran upward (toward state targets) and downward (toward enforcement of women). Mixed character: coordination function (demographic accounting) plus extraction (coercive enforcement).
constraint_indexing:constraint_classification(china_reproductive_coercion_history, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: STATE PLANNING APPARATUS (ROPE) — Central government perceived the reproductive coercion as solving a genuine coordination problem: unsustainable population growth given resource constraints and development targets. From this perspective, the policy coordinated collective action (limiting births) and was not extractive but allocative. The state viewed the costs as necessary burden-sharing for national interest. Arbitrage exit options exist (policy revision, grandfather clauses, exemptions for minorities and rural areas) — the state could adjust enforcement without structural collapse.
constraint_indexing:constraint_classification(china_reproductive_coercion_history, rope,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: POLICY FRAMEWORK (PITON, POST-2015) — After 2015 relaxation (two-child then three-child policy), the reproductive coercion constraint became partially degraded. Enforcement mechanisms atrophied (no longer tied to cadre evaluation, reduced penalties, reversed benefits-denial). Yet institutional inertia persists: women remain wary of reproductive agency, healthcare providers maintain surveillance norms, social stigma for larger families lingers. Theater ratio (0.55) reflects performative compliance monitoring and normalization rhetoric displacing actual enforcement. The constraint persists through institutional momentum despite functional collapse.
constraint_indexing:constraint_classification(china_reproductive_coercion_history, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (SNARE) — From a civilizational/global perspective, the reproductive coercion was a state Snare: high extraction (forced abortion, sterilization, denial of registration), high suppression (legal prohibition, economic penalties, no institutional appeal), and minimal coordination value. The state's claim to coordination (population control for development) does not survive scrutiny — the coercion exceeded what demographic targets required. Comparable results were achieved in other East Asian nations (South Korea, Taiwan) through incentive-based family planning without coercive enforcement. The Snare classification is robust across observables.
constraint_indexing:constraint_classification(china_reproductive_coercion_history, snare,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(china_reproductive_coercion_history_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(china_reproductive_coercion_history, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(china_reproductive_coercion_history, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(china_reproductive_coercion_history, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(china_reproductive_coercion_history, TR),
    TR >= 0.70.

:- end_tests(china_reproductive_coercion_history_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.78): Very high. The One-Child Policy extracted through multiple channels: forced abortion (estimated 15 million), forced sterilization (estimated 35 million), economic penalties (fines up to 6 years household income), employment discrimination, and institutional deprivation (hukou denial). The trajectory: initial phase (1979-1989) moderate enforcement (ε=0.72), intensified enforcement (1990-2005) maximum extraction (ε=0.82-0.85), followed by policy relaxation and institutional decay but not reversal (2006-2015, ε declining to 0.45 post-2015). Suppression (0.85): Structural and comprehensive. Legal prohibition on unauthorized births, criminal penalties for enforcement officials who failed to meet targets, complete control of reproductive healthcare, monitoring of menstrual cycles and pregnancy status, coercive medical procedures, denial of institutional recognition for unauthorized children. No legitimate appeal or grievance mechanism. Theater ratio (0.55): Moderate. The policy had genuine bureaucratic function (tracking reproductive status, enforcing quotas, preventing unauthorized births) — not purely performative. But enforcement rhetoric emphasized state benevolence and population 'stability' while masking coercive mechanisms. Post-2015, enforcement collapsed but institutional theater persisted (family planning offices restructured but surveillance norms embedded in healthcare; normalization rhetoric about 'responsible reproduction').
 *
 * PERSPECTIVAL GAP:
 *   Maximal gap between state (Rope, beneficial coordination) and victims (Snare, pure extraction). The state's perspective inverts the power relationship — from the planning apparatus, the policy solves a collective action problem and the costs are distributed burden-sharing. From the victim's perspective, the policy imposes unilateral extraction with no reciprocal benefit or choice. The analytical observer recognizes this as a Snare by structural signature (high extraction, high suppression, minimal genuine coordination value) and comparative evidence (equivalent demographic outcomes without coercion elsewhere). The cadre perspective (Tangled Rope) is historically accurate — they experienced genuine coordination burden alongside coercive pressure, but this does not salvage the state's Rope classification, because the apparent coordination (demographic control) could be achieved without coercion.
 *
 * DIRECTIONALITY LOGIC:
 *   Directional computation: Coerced women are victims with trapped exit (d=0.95→f(d)≈1.42), experiencing maximum effective extraction. Unauthorized children are victims with trapped exit across lifetime (d=0.95). Local cadres are mixed: beneficiaries of career advancement through enforcement but victims of quota pressure and moral conflict; moderate power with constrained exit yields d≈0.55→f(d)≈0.75. The state apparatus is beneficiary with arbitrage exit (policy reversal always possible; d≈0.05→f(d)≈-0.12) — negative effective extraction from state perspective (the state extracts, so from state's view the constraint is beneficial). National scope (σ=1.0) does not amplify or dampen; the calculation remains χ=ε×f(d). For powerless trapped women: χ≈0.78×1.42≈1.11 (exceeds 1.0 due to sigmoid amplification at extreme d — the engine caps χ at realized constraint, not arithmetic). For cadres: χ≈0.78×0.75≈0.585. For state: χ≈0.78×(-0.12)≈-0.094 (net benefit from state perspective).
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION: This constraint resolves the mandatrophy by establishing that the state's Rope classification (coordination for demographic sustainability) is false. The test: can equivalent demographic outcomes be achieved without coercive extraction? South Korea and Taiwan achieved faster demographic transition (lower fertility rates by 1995-2000) through incentive-based family planning, healthcare access, educational opportunity for women, and voluntary programs — without forced abortion or sterilization. This comparative data proves the coercion was not necessary for coordination and therefore falsifies the 'coordination mechanism' claim. The constraint is a Snare: pure extraction (forced reproductive control) that exceeds what demographic management requires. The state's Rope perspective is revealed as false naturalization (framing unjustifiable coercion as necessary population management). The mandatrophy is resolved by distinguishing genuine coordination constraints (achievable without coercion) from extractive constraints disguised as coordination (require coercion specifically to maintain extraction mechanism). The One-Child Policy is the latter.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    state_coordination_rationale_validity,
    'Did the reproductive coercion provide genuine value for national coordination (population sustainability) or did it exceed requirements and constitute pure extraction?',
    'Comparative analysis with South Korea and Taiwan: equivalent or faster demographic transition achieved without coercive enforcement. Economic modeling of population scenarios with vs without One-Child Policy. Demographic historians'' consensus on whether coercion was necessary vs contingent.',
    'If genuine coordination value: Tangled Rope classification possible from state perspective. If excessive: confirms Snare from all perspectives and falsifies state''s coordination rationale.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(state_coordination_rationale_validity, empirical, 'Whether reproductive coercion was necessary for demographic coordination or exceeded requirements').

omega_variable(
    psychological_suppression_persistence,
    'Is the post-2015 persistence of reproductive constraint (low birth rates, family-planning wariness, healthcare surveillance norms) structurally suppressed or internalized cognitive capture?',
    'Longitudinal survey data on reproductive intentions vs actual fertility rates post-2015. Analysis of institutional barriers (healthcare access, employment discrimination) vs internalized norms (fear of social penalty, learned mistrust of state). Cohort comparison: women who experienced coercion vs younger women born after policy relaxation.',
    'If structural suppression remains: constraint is still active Snare despite policy change. If internalized: suppression has shifted from external to cognitive — the constraint now operates through identity lock rather than material barriers.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(psychological_suppression_persistence, empirical, 'Whether post-policy persistence is structural suppression or internalized cognitive capture').

omega_variable(
    hukou_reintegration_completeness,
    'Can unauthorized children born under the One-Child Policy achieve full status restoration (hukou registration, educational access, employment equality) or do cascading institutional effects maintain permanent stigma?',
    'Tracking of cohorts of unauthorized children through educational, employment, and health outcomes. Analysis of hukou registration success rates, institutional acceptance, wage/career penalties. Longitudinal data on intergenerational effects (their children''s access and opportunities).',
    'If full reintegration possible: extraction is bounded (historical, now remedying). If permanent cascading effects: extraction extends indefinitely through institutional architecture — the Snare persists even after policy reversal.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(hukou_reintegration_completeness, empirical, 'Whether unauthorized children can achieve full institutional reintegration').

omega_variable(
    minority_coercion_intensity_differential,
    'Did Han majority women experience the same enforcement intensity as ethnic minorities (Uyghur, Tibetan, Hui), or did policy application vary by ethnicity?',
    'Comparative analysis of forced sterilization, abortion, and hysterectomy rates by ethnicity. Analysis of quota enforcement and penalty application across regions and ethnic groups. Historical records and oral histories documenting differential treatment.',
    'If applied uniformly: Snare classification applies equally to all reproductive-age women. If differential: ethnic minorities experienced higher suppression and extraction — creating a stratified extraction system with deeper penetration in non-Han populations. This would intensify the Snare classification for minority victims.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(minority_coercion_intensity_differential, empirical, 'Whether reproductive coercion enforcement varied by ethnicity').

omega_variable(
    state_actor_intentionality_clarity,
    'Was the reproductive coercion system designed as deliberate extraction (inflicting coercion as means) or as unintended extraction (population control intent with coercive side effects)?',
    'Archival analysis of state planning documents, internal directives, cadre evaluations. Historical interviews with policy architects. Comparison of policy rhetoric vs implementation instructions. Analysis of whether enforcement mechanisms were necessary for stated demographic goals or gratuitous.',
    'If deliberately extractive: confirms Snare intentionality. If unintended side effect of coercive implementation: extraction emerges from enforcement architecture rather than design. Classification remains Snare but diagnostic reasoning shifts.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(state_actor_intentionality_clarity, empirical, 'Whether coercion was deliberate design or unintended implementation effect').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(china_reproductive_coercion_history, 0, 36).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(crc_tr_t0, china_reproductive_coercion_history, theater_ratio, 0, 0.25).
narrative_ontology:measurement(crc_tr_t10, china_reproductive_coercion_history, theater_ratio, 10, 0.3).
narrative_ontology:measurement(crc_tr_t20, china_reproductive_coercion_history, theater_ratio, 20, 0.35).
narrative_ontology:measurement(crc_tr_t30, china_reproductive_coercion_history, theater_ratio, 30, 0.42).
narrative_ontology:measurement(crc_tr_t36, china_reproductive_coercion_history, theater_ratio, 36, 0.55).

% Extraction over time
narrative_ontology:measurement(crc_be_t0, china_reproductive_coercion_history, base_extractiveness, 0, 0.72).
narrative_ontology:measurement(crc_be_t10, china_reproductive_coercion_history, base_extractiveness, 10, 0.82).
narrative_ontology:measurement(crc_be_t20, china_reproductive_coercion_history, base_extractiveness, 20, 0.85).
narrative_ontology:measurement(crc_be_t30, china_reproductive_coercion_history, base_extractiveness, 30, 0.78).
narrative_ontology:measurement(crc_be_t36, china_reproductive_coercion_history, base_extractiveness, 36, 0.45).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(china_reproductive_coercion_history, resource_allocation).
narrative_ontology:boltzmann_floor_override(china_reproductive_coercion_history, 0.08).
narrative_ontology:affects_constraint(china_reproductive_coercion_history, hukou_system_discrimination).
narrative_ontology:affects_constraint(china_reproductive_coercion_history, han_ethnic_dominance).
narrative_ontology:affects_constraint(china_reproductive_coercion_history, state_reproductive_autonomy_control).

% DUAL FORMULATION NOTE:
% The One-Child Policy decomposes into multiple structurally distinct constraints: (1) forced reproductive procedures (ε≈0.88, Snare), (2) hukou deprivation for unauthorized children (ε≈0.72, Snare downstream), (3) cadre-level enforcement pressure (ε≈0.55, Tangled Rope). This story addresses the policy constraint as experienced by reproductive-age women; sibling stories address implementation cascades and institutional persistence. All linked via network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(china_reproductive_coercion_history, institutional, 0.05).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
