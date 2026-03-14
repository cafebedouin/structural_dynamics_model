% ============================================================================
% CONSTRAINT STORY: leadership_capability_cascade
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_leadership_capability_cascade, []).

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
 *   constraint_id: leadership_capability_cascade
 *   human_readable: Leadership Capability Cascade
 *   domain: organizational/institutional/political
 *
 * SUMMARY:
 *   The leadership capability cascade is a structural constraint present in
 *   hierarchical organizations where career advancement, skill development,
 *   and institutional knowledge transfer are controlled through a gatekeeping
 *   mechanism nominally called mentorship or succession planning. This
 *   constraint exhibits the full range of Deferential Realism classifications
 *   depending on observational position. The same organizational structure
 *   that coordinates institutional memory and ensures knowledge transfer
 *   (legitimate coordination function) simultaneously extracts career
 *   control, suppresses alternative voices, and delays emerging capability
 *   development (extractive function). The constraint emerges through
 *   enforcement: senior leadership enforces advancement criteria, controls
 *   access to developmental opportunities, and determines promotion
 *   timelines. The extraction mechanism operates through suppression: limited
 *   available senior positions create scarcity; gatekeeping becomes rational
 *   for those holding power; informal rules and unwritten criteria become
 *   tools of control. The theater ratio (0.65) reflects that formal
 *   mentorship programs, leadership development initiatives, and succession
 *   planning frameworks maintain performative visibility while actual
 *   advancement depends on informal relationships with gatekeepers.
 *
 * KEY AGENTS:
 *   - Senior Leadership Cohort: Primary beneficiary (institutional/arbitrage) — controls advancement criteria, maintains power concentration, captures status and financial benefits; ability to exit and arbitrage credentials earned through organizational tenure
 *   - Mid-Level Managers: Secondary victim and beneficiary (moderate/constrained) — benefit from stability, mentorship, and career pathway; bear cost of limited autonomy and suppressed initiative; structurally trapped by career investment and reputation dependence
 *   - Emerging Talent Pool: Primary victim (powerless/trapped) — cannot advance without gatekeeping approval; face suppressed autonomy and delayed capability utilization; exit costs are high (reputational damage, career restart)
 *   - Organizational Adaptability: Aggregate victim (powerless/trapped) — the organization's strategic flexibility and innovation capacity is suppressed by insistence on continuity over adaptation
 *   - Institutional Legacy System: Sustained through inertia (institutional/mobile) — formal programs and ritual processes maintain the appearance of meritocratic advancement while gatekeeping remains dominant mechanism
 *   - Distributed Leadership Movement: Organized alternative (organized/mobile) — flat structures, cross-functional teams, external networks building competitive alternative pathways
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(leadership_capability_cascade, 0.58).
domain_priors:suppression_score(leadership_capability_cascade, 0.62).
domain_priors:theater_ratio(leadership_capability_cascade, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(leadership_capability_cascade, extractiveness, 0.58).
narrative_ontology:constraint_metric(leadership_capability_cascade, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(leadership_capability_cascade, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(leadership_capability_cascade, tangled_rope).
narrative_ontology:human_readable(leadership_capability_cascade, "Leadership Capability Cascade").
narrative_ontology:topic_domain(leadership_capability_cascade, "organizational/institutional/political").

domain_priors:requires_active_enforcement(leadership_capability_cascade).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(leadership_capability_cascade, senior_leadership_cohort).
narrative_ontology:constraint_beneficiary(leadership_capability_cascade, institutional_continuity_apparatus).
narrative_ontology:constraint_victim(leadership_capability_cascade, mid_level_managers).
narrative_ontology:constraint_victim(leadership_capability_cascade, emerging_talent_pool).
narrative_ontology:constraint_victim(leadership_capability_cascade, organizational_adaptability).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: EMERGING TALENT (SNARE) — Structurally trapped within institutional hierarchy. Cannot advance without approval from gatekeeping senior cohort. Career progression requires conformity to established patterns, suppressing innovation and alternative approaches. No meaningful exit: leaving the organization means starting over; staying means accepting subordination. Maximum extraction: benefit flows to senior leadership while emerging talent bears the cost of delayed capability development.
constraint_indexing:constraint_classification(leadership_capability_cascade, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: MID-LEVEL MANAGER (TANGLED ROPE) — Experiences genuine coordination function (mentorship, hierarchical knowledge transfer, organizational coherence) alongside asymmetric extraction. Benefits from stability and career pathway; bears cost of limited autonomy and suppressed initiative. Constrained exit: can change employers but at significant career cost and reputation damage. Mixed extraction profile reflects both coordination necessity and rent-seeking overhead.
constraint_indexing:constraint_classification(leadership_capability_cascade, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: SENIOR LEADERSHIP COHORT (ROPE) — Experiences the cascade as pure coordination. Controlling succession and capability development ensures institutional continuity and knowledge transfer. Net beneficiaries: accrue status, influence, and wealth during tenure; ability to exit and arbitrage credentials elsewhere. The constraint appears as functional coordination mechanism from their position.
constraint_indexing:constraint_classification(leadership_capability_cascade, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: INSTITUTIONAL LEGACY SYSTEM (PITON) — The formal mentorship apparatus, succession planning frameworks, and leadership development programs persist through institutional inertia despite declining functional effectiveness. High theater ratio (0.65): ceremonial development programs, perfunctory review processes, and ritualized promotion criteria maintain the appearance of meritocratic advancement while gatekeeping remains the actual mechanism. The institution 'performs' capability development without delivering it.
constraint_indexing:constraint_classification(leadership_capability_cascade, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: EMERGING ALTERNATIVE - DISTRIBUTED LEADERSHIP (SCAFFOLD) — Flat organizational structures, dotted-line reporting, cross-functional teams, and external talent networks are building alternative capability pathways that bypass the traditional cascade gatekeeping. These emerge as temporary scaffolding solutions with a sunset clause: as distributed leadership norms mature and external talent becomes portable, the traditional cascade's extraction mechanism loses force. Estimated sunset: 10-15 years for norms to fully displace hierarchical advancement.
constraint_indexing:constraint_classification(leadership_capability_cascade, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) — From a cross-institutional analytical view, the leadership cascade exhibits genuine coordination function (knowledge transfer, institutional memory, stability) while maintaining asymmetric extraction (gatekeeping, suppressed innovation, career control). The tension between these functions is structural, not accidental. The constraint resolves the mandatrophy: it is neither pure coordination nor pure extraction, but a hybrid where the coordination legitimacy provides cover for the extraction mechanism.
constraint_indexing:constraint_classification(leadership_capability_cascade, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(leadership_capability_cascade_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(leadership_capability_cascade, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(leadership_capability_cascade, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(leadership_capability_cascade, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(leadership_capability_cascade, TR),
    TR >= 0.70.

:- end_tests(leadership_capability_cascade_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The senior leadership cohort captures career control and status benefits during their tenure while emerging talent bears the cost of delayed capability development and suppressed initiative. The extraction is not maximal (0.70+) because genuine coordination functions (institutional memory, knowledge transfer, mentoring) provide real benefits to some agents. The measurement trajectory (0.42 → 0.58 over the interval) reflects accumulation of gatekeeping overhead over time — as organizations age, the ratio of gatekeepers to emerging talent increases, and the extraction mechanism becomes more efficient. Suppression (0.62): Moderate-high. Emerging talent and mid-level managers face significant barriers to independent action: limited senior positions create scarcity; informal rules and gatekeeping restrict advancement criteria; career risk of non-conformity is substantial. However, suppression is not total — some agents do innovate within constraints, and external talent networks provide partial alternatives. Theater ratio (0.65): Moderate-high and rising. The institutional apparatus (formal mentorship programs, development frameworks, promotion criteria) maintains high visibility while actual advancement depends on informal relationships and gatekeeping. The rising trajectory reflects the growing gap between performative development activities and actual career determinants.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates perspectival divergence rooted in structural position. Senior leadership sees rope (coordination mechanism enabling knowledge transfer and institutional continuity). Mid-level managers see tangled rope (mixed coordination benefits and extraction constraints). Emerging talent see snare (pure extraction with no exit). The distributed leadership movement sees scaffold (temporary constraint being bypassed by emerging alternative pathways). The institutional legacy apparatus sees piton (its own processes degraded by the gap between performance and reality). The analytical observer sees tangled rope (genuine hybrid of coordination and extraction). The perspectival gaps reveal that organizational position directly determines whether the same constraint is experienced as coordination or extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values (d) for each perspective are derived from power level, exit options, and beneficiary/victim declarations. Senior leadership benefits from arbitrage exit — they have high organizational value and can exit to equivalent positions elsewhere. Their d is low (~0.15), producing negative or minimal f(d), reflecting that the constraint flows benefit toward them. Mid-level managers are constrained (high exit cost) but also benefit from the system — their d is moderate (~0.50-0.55). Emerging talent are trapped (high exit cost, no outside reputation capital) and victim — their d is high (~0.90), producing maximum f(d), reflecting maximum experienced extraction. Organizational adaptability is an abstract collective victim with no exit capacity — trapped and victim — d approaches 1.0. The analytical observer uses canonical d (~0.72) and produces the tangled rope classification reflecting the genuine hybrid character.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by demonstrating that the coordination and extraction functions are genuinely coupled. The mentorship/succession planning apparatus provides real coordination value — institutional memory is preserved, knowledge transfers, organizational continuity is maintained. These are not theater. But the same apparatus enables gatekeeping, career control, and suppression. These are not false either. The constraint is neither pure coordination (Rope) nor pure extraction (Snare), but a genuine hybrid where the coordination legitimacy provides the enforcement mechanism that enables extraction. The mandatrophy resolution: the constraint is Tangled Rope because it REQUIRES both genuine coordination and asymmetric extraction to maintain stability. Remove the extraction mechanism and the coordination would collapse (agents would have no incentive to enforce advancement criteria). Remove the coordination function and the extraction would lose legitimacy (pure predation becomes visible). The hybrid is structurally stable.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    gatekeeping_vs_mentorship_boundary,
    'At what point does necessary knowledge transfer and institutional continuity (legitimate coordination) become gatekeeping for rent-seeking (extraction)?',
    'Career advancement data: correlation between mentor relationships and promotion outcomes; comparison of advancement rates for proteges vs non-proteges controlling for performance metrics; analysis of whether exclusion from mentorship prevents legitimate capability development',
    'If boundary is clear and enforceable: constraints reclassify toward Rope (more coordination emphasis). If boundary is systematically blurred: constraints reclassify toward Snare (extraction dominates).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(gatekeeping_vs_mentorship_boundary, empirical, 'Boundary between legitimate mentorship and extractive gatekeeping').

omega_variable(
    alternative_pathway_viability,
    'Can emerging leaders achieve comparable capability development and advancement through distributed networks and external pathways, or does the traditional cascade remain dominant due to structural irreplaceability?',
    'Longitudinal tracking of career trajectories: advancement speed and ultimate position achieved through traditional vs alternative pathways; organizational performance outcomes under leadership developed via each pathway',
    'If alternative pathways prove viable: scaffold sunset is real and accelerating. If traditional cascade remains dominant: alternative is aspirational theater, and constraint persists as stable Tangled Rope or Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_pathway_viability, empirical, 'Whether alternative leadership pathways provide genuine viability').

omega_variable(
    organizational_adaptability_cost,
    'What proportion of organizational stagnation and innovation failure is attributable to the suppression mechanism inherent in the leadership cascade versus external market forces, technology shifts, and competitive dynamics?',
    'Comparative analysis: innovation metrics and strategic adaptation rates in organizations with flat vs hierarchical structures; post-restructuring performance changes when cascade is disrupted; analysis of failed strategic transitions to identify cascade-suppression as failure mode',
    'If cascade suppression is primary cause: extractiveness increases (constraint is more purely extractive). If external factors dominate: extractiveness decreases (constraint is more coordination overhead than extraction).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(organizational_adaptability_cost, empirical, 'Attribution of organizational stagnation to cascade suppression').

omega_variable(
    identity_lock_mechanism_strength,
    'To what extent are mid-level managers and emerging talent bound by internalized identity frames (self-concept fused with hierarchical position, career identity constituted within the organization) versus material barriers to exit?',
    'Post-exit surveys: career satisfaction and identity stability of those who left vs those who remained; analysis of whether suppression persists after individuals exit the organizational context; psychological assessment of identity fusion with organizational role',
    'If identity lock is dominant: exit_options should be reclassified as identity_locked rather than trapped/constrained; classification may shift toward Rope from identity-locked perspective (per identity_lock immutability profile). If material barriers dominate: trapped/constrained classification stands.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_lock_mechanism_strength, empirical, 'Whether suppression is structural or identity-locked').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(leadership_capability_cascade, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(leadcap_tr_t0, leadership_capability_cascade, theater_ratio, 0, 0.48).
narrative_ontology:measurement(leadcap_tr_t10, leadership_capability_cascade, theater_ratio, 10, 0.57).
narrative_ontology:measurement(leadcap_tr_t20, leadership_capability_cascade, theater_ratio, 20, 0.65).

% Extraction over time
narrative_ontology:measurement(leadcap_be_t0, leadership_capability_cascade, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(leadcap_be_t10, leadership_capability_cascade, base_extractiveness, 10, 0.5).
narrative_ontology:measurement(leadcap_be_t20, leadership_capability_cascade, base_extractiveness, 20, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(leadership_capability_cascade, resource_allocation).
narrative_ontology:affects_constraint(leadership_capability_cascade, organizational_innovation_suppression).
narrative_ontology:affects_constraint(leadership_capability_cascade, talent_retention_crisis).
narrative_ontology:affects_constraint(leadership_capability_cascade, succession_planning_fragility).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(leadership_capability_cascade, institutional, 0.18).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
