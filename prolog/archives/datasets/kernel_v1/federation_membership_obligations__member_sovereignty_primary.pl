% ============================================================================
% CONSTRAINT STORY: federation_membership_obligations__member_sovereignty_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_federation_membership_obligations__member_sovereignty_primary, []).

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
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_axiom_grounding/3,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:cs_created_at/2,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: federation_membership_obligations__member_sovereignty_primary
 *   human_readable: Federation Membership Obligations (Member Sovereignty Reading)
 *   domain: political_economy/federalism/migration_policy
 *
 * SUMMARY:
 *   This constraint models the member sovereignty reading of the contested
 *   kernel on federation membership obligations. The core claim is that
 *   national welfare states retain closure authority over non-citizen
 *   beneficiaries as a condition of member state participation in a federal
 *   union. Free movement is not an absolute right but a conditional one,
 *   bounded by labor market protection (incumbent workers must not face
 *   wage/employment displacement) and welfare system sustainability
 *   (receiving states must not face fiscal drain from non-citizen welfare
 *   use). This reading is held by member state governments (especially those
 *   with developed welfare systems), conservative political parties, and
 *   fiscal administrators. It directly competes with two sibling readings:
 *   integration_primary (which treats free movement as constitutive of EU
 *   citizenship and subordinates welfare boundaries to mobility rights) and
 *   selective_solidarity (which tiered welfare access by contribution history
 *   rather than citizenship status). The member sovereignty reading
 *   instantiates a specific distributional outcome: mobile workers are
 *   excluded from welfare access and bear implicit taxation (contributions
 *   without benefits); incumbent workers retain welfare access and labor
 *   market protection; member state legislatures retain veto authority over
 *   welfare access rules. The constraint has evolved since Maastricht (1992)
 *   through Eastern European expansion (2004), the refugee and migration
 *   crises (2013-2016), and post-pandemic labor dynamics (2020-2023). Theater
 *   ratio has risen over time as the formal compromise (free movement
 *   conditional on labor market participation) has become increasingly
 *   disconnected from actual enforcement (CJEU narrow reading of free
 *   movement rights, member state resistance to welfare access for mobile
 *   workers, transnational advocacy pressure). The constraint exemplifies a
 *   piton-in-formation: the Maastricht compromise designed to satisfy all
 *   constituencies now satisfies none, yet persists through institutional
 *   inertia.
 *
 * KEY AGENTS:
 *   - Mobile Workers (EU citizens, third-country nationals): Primary victims (powerless/trapped) — Legally resident and employed but excluded from welfare access; contribute via payroll taxes but receive no benefits
 *   - Incumbent Labor Force (citizens of receiving state): Primary beneficiary (powerful/arbitrage) — Protected from wage competition by labor market closure rules; retain full welfare access; experience constraint as beneficial coordination
 *   - Member State Legislatures: Institutional actor (institutional/constrained) — Must coordinate welfare/labor protection AND enforce closure; constrained by EU law limiting discrimination against mobile workers; retains formal veto authority over welfare rules
 *   - EU Supranational Authority (Commission, CJEU, Parliament): Institutional actor (institutional/constrained) — Must promote free movement AND respect member state sovereignty; weak enforcement of free movement due to member state resistance; constrained by Maastricht compromise
 *   - Transnational Labor Movement: Organized actor (organized/mobile) — Advocates for portable welfare rights and EU-level social standards; sees constraint as temporary governance failure with sunset
 *   - Welfare System (as abstract good): Powerless victim (powerless/trapped) — Fiscal sustainability claimed as rationale for closure; actually depends on distributional assumptions (who funds vs who receives)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(federation_membership_obligations__member_sovereignty_primary, 0.52).
domain_priors:suppression_score(federation_membership_obligations__member_sovereignty_primary, 0.58).
domain_priors:theater_ratio(federation_membership_obligations__member_sovereignty_primary, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(federation_membership_obligations__member_sovereignty_primary, extractiveness, 0.52).
narrative_ontology:constraint_metric(federation_membership_obligations__member_sovereignty_primary, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(federation_membership_obligations__member_sovereignty_primary, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(federation_membership_obligations__member_sovereignty_primary, tangled_rope).
narrative_ontology:human_readable(federation_membership_obligations__member_sovereignty_primary, "Federation Membership Obligations (Member Sovereignty Reading)").
narrative_ontology:topic_domain(federation_membership_obligations__member_sovereignty_primary, "political_economy/federalism/migration_policy").

domain_priors:requires_active_enforcement(federation_membership_obligations__member_sovereignty_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(federation_membership_obligations__member_sovereignty_primary, 'acba1ca7-3331-4cd7-b818-023c1f9c5e3c').
narrative_ontology:cs_kernel_codification('acba1ca7-3331-4cd7-b818-023c1f9c5e3c', formalized).
narrative_ontology:cs_authority_grounding('acba1ca7-3331-4cd7-b818-023c1f9c5e3c', extraction).
narrative_ontology:cs_interpretation_layer_present('acba1ca7-3331-4cd7-b818-023c1f9c5e3c').
narrative_ontology:cs_reading_relation('acba1ca7-3331-4cd7-b818-023c1f9c5e3c', federation_membership_obligations__integration_primary, coexists_with).
narrative_ontology:cs_reading_relation('acba1ca7-3331-4cd7-b818-023c1f9c5e3c', federation_membership_obligations__selective_solidarity, influences).
narrative_ontology:cs_axiom('acba1ca7-3331-4cd7-b818-023c1f9c5e3c', foundational, member_state_welfare_closure_legitimate).
narrative_ontology:cs_axiom_status(member_state_welfare_closure_legitimate, holdable).
narrative_ontology:cs_axiom_grounding('acba1ca7-3331-4cd7-b818-023c1f9c5e3c', member_state_welfare_closure_legitimate, instrumental).
narrative_ontology:cs_axiom('acba1ca7-3331-4cd7-b818-023c1f9c5e3c', foundational, free_movement_conditional_on_labor_protection).
narrative_ontology:cs_axiom_status(free_movement_conditional_on_labor_protection, overridden).
narrative_ontology:cs_axiom_grounding('acba1ca7-3331-4cd7-b818-023c1f9c5e3c', free_movement_conditional_on_labor_protection, instrumental).
narrative_ontology:cs_reference_frame('acba1ca7-3331-4cd7-b818-023c1f9c5e3c', member_state_welfare_closure_authority).
narrative_ontology:cs_drift_state('acba1ca7-3331-4cd7-b818-023c1f9c5e3c', contemporary_cjeu_integration_pressure, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('acba1ca7-3331-4cd7-b818-023c1f9c5e3c', '').
narrative_ontology:cs_kernel_id(federation_membership_obligations__member_sovereignty_primary, federation_membership_obligations).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(federation_membership_obligations__member_sovereignty_primary, incumbent_labor_force).
narrative_ontology:constraint_beneficiary(federation_membership_obligations__member_sovereignty_primary, welfare_receiving_citizens).
narrative_ontology:constraint_victim(federation_membership_obligations__member_sovereignty_primary, mobile_workers).
narrative_ontology:constraint_victim(federation_membership_obligations__member_sovereignty_primary, cross_border_welfare_seekers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: MOBILE WORKER (SNARE) — Trapped by exclusion from welfare access despite legal residence and labor market participation. Contributes to social insurance systems (via payroll taxes) but cannot access benefits that residents receive. No exit option: leaving means complete loss of labor market access and return to origin; staying means continued contribution without equivalent benefit. Maximum experienced extraction — suppression is structural (legal barriers to welfare access).
constraint_indexing:constraint_classification(federation_membership_obligations__member_sovereignty_primary, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: INCUMBENT LABOR FORCE (ROPE) — Experiences the constraint as beneficial coordination: labor market protection (minimum wages, working conditions, collective bargaining) combined with welfare access creates stable employment and social security. The constraint coordinates protection of domestic labor standards and ensures welfare resources flow to citizens. Benefits from exclusion of mobile workers who would otherwise compete for wages or strain welfare systems. Low suppression relative to beneficiaries — enforcement is administrative (eligibility rules) rather than coercive.
constraint_indexing:constraint_classification(federation_membership_obligations__member_sovereignty_primary, rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: MEMBER STATE LEGISLATURE (TANGLED ROPE) — Experiences dual function: must coordinate citizens' welfare entitlements and labor market protection (genuine coordination function: pooling risk, managing social insurance), while also extracting jurisdictional authority to exclude non-citizens from these benefits. Constrained by federation membership obligations (CJEU case law, EU directives on free movement) that limit unilateral closure authority. Active enforcement required: immigration enforcement, eligibility verification, benefit suspension for non-authorized workers. Asymmetric extraction: authority to exclude provides leverage over non-citizen workers and competing member states.
constraint_indexing:constraint_classification(federation_membership_obligations__member_sovereignty_primary, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: TRANSNATIONAL LABOR MOVEMENT (SCAFFOLD) — Organized advocacy actors see the constraint as a temporary governance failure with a sunset: portable welfare rights, EU-level social standards, and reciprocal welfare treaties would eliminate the mobile worker exclusion. This reading has sunset logic: as transnational labor mobility increases, maintaining exclusion becomes economically inefficient (workers leave), and pressure builds for portable welfare coordination. Low theater (direct advocacy for rule change, not performative acceptance). Mobile: can threaten labor strikes, cross-border organizing, or political defection.
constraint_indexing:constraint_classification(federation_membership_obligations__member_sovereignty_primary, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(continental))).

% PERSPECTIVE 5: EU SUPRANATIONAL AUTHORITY (TANGLED ROPE) — Experiences contradiction between two mandates: promote free movement and single market (coordination function of federation: eliminate internal borders), while respecting member state welfare sovereignty (extraction authority: member states retain closure over non-residents). Constrained by both commitments — cannot fully enforce free movement (member states legislate welfare access) and cannot preserve welfare closure (EU law limits discrimination against mobile workers). Active enforcement: CJEU reviews welfare exclusions; EU directives set minimum free movement standards. Asymmetric extraction: supremacy of EU law gives institutional authority over member states, but enforcement is weak (member states resist).
constraint_indexing:constraint_classification(federation_membership_obligations__member_sovereignty_primary, tangled_rope,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 6: THE MAASTRICHT COMPROMISE (PITON) — The formal framework ('free movement conditional on labor market participation and welfare system sustainability') is a degraded institutional arrangement maintained through inertia. The compromise was designed to permit member state welfare closure while advancing free movement; it now functions largely theatrically — member states routinely challenge CJEU rulings on welfare access, CJEU repeatedly affirms limited free movement rights to welfare, and the contradiction remains unresolved. Theater ratio high: speeches about European solidarity, rights, and welfare protection, alongside administrative enforcement of narrow welfare exclusions. Function has atrophied: the compromise satisfies no constituency (mobile workers excluded, member states constrained, EU authority contested), yet persists because formal revision would require constitutional change.
constraint_indexing:constraint_classification(federation_membership_obligations__member_sovereignty_primary, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / FISCAL CAPACITY VIEW (MOUNTAIN) — From a civilizational perspective, the constraint appears as a natural law of federation: welfare states are financed by taxation on a bounded population; free movement without welfare closure would drain fiscal capacity and collapse the welfare system. In this reading, member state closure authority is not extractive but protective of a collective good (welfare system itself). The boundary is immutable: you cannot have unlimited free movement AND fiscally bounded welfare states simultaneously. However, the structural data reveals this as a false summit: the constraint's beneficiaries and asymmetric extraction prove that the closure is not protective but distributive (incumbent workers benefit; mobile workers pay). The apparent natural law is a contingent institutional arrangement given the specific choices about who funds and who receives welfare.
constraint_indexing:constraint_classification(federation_membership_obligations__member_sovereignty_primary, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(federation_membership_obligations__member_sovereignty_primary_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(federation_membership_obligations__member_sovereignty_primary, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(federation_membership_obligations__member_sovereignty_primary, TypeOther, context(agent_power(powerful), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(federation_membership_obligations__member_sovereignty_primary, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(federation_membership_obligations__member_sovereignty_primary, TR),
    TR >= 0.70.

:- end_tests(federation_membership_obligations__member_sovereignty_primary_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high, and rising over time (0.35 at Maastricht → 0.52 in 2023). The member sovereignty reading creates asymmetric extraction of mobile workers' labor and contributions without corresponding welfare access. The baseline extraction is moderate because labor market protections and welfare access are genuine goods (not pure coercive mechanisms) and mobile workers do benefit from legal employment and some social insurance (accident insurance, health emergency coverage). But the extraction rises as the gap widens: more mobile workers participate in EU labor markets, labor mobility increases competition pressure (making protections more valuable to incumbent workers), and fiscal pressures on welfare systems increase the incentive to exclude. Suppression (0.58): Moderate-high. Barriers to welfare access include legal residency requirements, citizenship verification, family size/dependency thresholds, and employment history documentation. These are administrative rather than violent, but they are structural — mobile workers cannot overcome them through individual effort (they require legislative change or CJEU intervention). Suppression rose sharply from 2004-2015 as border controls intensified and welfare eligibility tightened in response to Eastern expansion and migration pressures. Theater ratio (0.48, rising toward 0.55 in 2015): Moderate. The Maastricht compromise is presented as balancing free movement with welfare sustainability, but enforcement increasingly relies on theater — speeches about European values alongside narrow eligibility rules, CJEU affirmations of limited rights followed by member state resistance, repeated statements that welfare systems must be protected alongside continued employment by mobile workers who pay into those systems. Theater rose during the refugee crisis (2013-2015) when the gap between free movement rhetoric and welfare closure practice became visible. It has declined slightly (0.48 in 2023) as political acceptance of the compromise's failure has increased and alternative readings (selective solidarity) gain institutional traction.
 *
 * PERSPECTIVAL GAP:
 *   The constraint produces the full spectrum of classifications from the same base metrics. Mobile workers see a Snare (high extraction, high suppression, no exit). Incumbent workers see a Rope (beneficial coordination, labor market protection as shared good). Member state legislatures see Tangled Rope (genuine welfare coordination function plus asymmetric extraction of non-citizen labor). EU supranational authority sees Tangled Rope from a different angle (constrained by conflicting mandates, active enforcement of partial rights). Transnational labor movement sees a Scaffold (temporary governance failure with sunset as portable welfare norms develop). Piton classification applies to the Maastricht compromise as institutional form: the formal framework has atrophied as actual enforcement increasingly diverges from stated commitments. The analytical observer risks seeing a Mountain (natural law of welfare state fiscal limits) but this is a false summit — the fiscal limits are chosen (by excluding mobile workers) rather than inherent to welfare systems. Welfare systems with portable rights and reciprocal contribution counting (Nordic models) demonstrate that fiscal sustainability is compatible with higher mobility.
 *
 * DIRECTIONALITY LOGIC:
 *   The constraint's directionality differs dramatically by agent: Mobile workers (powerless/trapped) have d ≈ 0.95 (full target of extraction), producing high χ. Incumbent workers (powerful/arbitrage) have d ≈ 0.10 (net beneficiary), producing negative or minimal χ. Member state legislatures (institutional/constrained) have d ≈ 0.50 (split between beneficiary and target roles — they benefit from labor supply and extraction of non-citizen contributions, but are constrained by EU law from unilateral closure), producing moderate χ. The EU supranational authority (institutional/constrained) has d ≈ 0.55 (more target than beneficiary — constrained by conflicting mandates), producing slightly elevated χ. Transnational labor movement (organized/mobile) has d ≈ 0.65 (more target than beneficiary, but organized, so moderate χ rather than high). These differing d values explain the perspectival gap: each agent experiences a different effective extractiveness despite the same base metrics. The mobile worker sees χ = 0.52 × f(0.95) × σ(regional) ≈ 0.52 × 1.42 × 0.9 ≈ 0.67 (high snare), while the incumbent worker sees χ = 0.52 × f(0.10) × σ(national) ≈ 0.52 × (-0.01) × 1.0 ≈ -0.005 (negative extraction, rope).
 *
 * MANDATROPHY ANALYSIS:
 *   The member sovereignty reading resolves mandatrophy through asymmetry: it explicitly accepts the logic of welfare closure and labor protection, making extraction of mobile workers a feature, not a bug. The reading does not pretend to be pure coordination (Rope) — it openly declares that member states retain authority to exclude non-citizens from benefits. However, mandatrophy emerges when comparing across readings: integration_primary reads the same constraint as illegitimate discrimination against EU citizens (a Snare); selective_solidarity reads it as clumsy implementation of a contributory principle. The reading's own internal coherence is high (member states do retain closure authority; mobile workers are excluded; incumbent workers are protected), but its external legitimacy is contested (CJEU repeatedly narrows the scope of permissible exclusion, transnational advocacy pressures mount, alternative readings gain institutional traction). The mandatrophy is not internal to this reading but inter-reading: the constraint cannot simultaneously be member sovereignty, integration primary, and selective solidarity. The engine's classification as tangled_rope (moderate extraction, genuine coordination, active enforcement) reflects this middle-ground institutional reality — the member sovereignty reading is neither pure coordination (rope) nor pure extraction (snare), but a hybrid where genuine welfare coordination is asymmetrically distributed.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    fiscal_sustainability_measurement,
    'Does welfare system fiscal sustainability actually depend on excluding mobile workers, or do EU wage/tax harmonization and reciprocal contribution counting provide sufficient alternatives?',
    'Comparative fiscal analysis: welfare systems with high mobile worker inclusion (e.g., Nordic countries with portability agreements) vs systems with strict exclusion; cost modeling for alternative funding mechanisms (EU-level taxation, reciprocal contribution recognition)',
    'If fiscal sustainability requires exclusion: member state closure authority is structurally necessary (legitimacy of snare/extraction from mobile workers). If alternatives exist: closure is choice, not necessity, and the constraint reclassifies as pure extraction (more severe snare, less tangled rope).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fiscal_sustainability_measurement, empirical, 'Whether welfare system fiscal sustainability depends on excluding mobile workers').

omega_variable(
    labor_market_competition_empirics,
    'Do mobile workers actually depress wages for incumbent workers, or is the labor market competition assumption empirically unfounded?',
    'Comparative wage analysis pre/post labor mobility increases; sectoral breakdown of labor market effects; evidence on occupational substitutability between mobile and incumbent workers',
    'If competition is real: labor market protection rationale for exclusion is evidence-based (legitimacy of rope coordination). If competition is minimal or localized: protection rationale becomes protection theater, reclassifying the constraint toward pure snare (higher extraction, less coordination).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(labor_market_competition_empirics, empirical, 'Whether mobile workers depress wages for incumbent workers').

omega_variable(
    reading_contrast_with_integration_primary,
    'Is the member sovereignty reading logically foreclosed by the integration primary reading, or are they coexisting but incompatible positions held by different political coalitions?',
    'Analysis of whether any single framework (EU constitutional order, member state legal order) can hold both readings simultaneously; survey of which jurisdictions and actors endorse each reading; examination of whether one reading''s axiomatic core contradicts the other''s.',
    'If foreclosed: member sovereignty cannot be held by EU actors who have accepted integration as foundational (reading becomes institutional minority position). If coexisting: both readings remain live political options for different member states and political movements. If influences: member sovereignty constrains but does not eliminate integration goals (current state).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_contrast_with_integration_primary, conceptual, 'Whether member sovereignty and integration readings are in logically foreclosed or coexisting relation').

omega_variable(
    welfare_system_boundary_fungibility,
    'Is the boundary between ''citizens with welfare access'' and ''mobile workers without welfare access'' stable, or does it degrade through legal challenge (CJEU reinterpretation) and pressure (transnational advocacy)?',
    'Temporal analysis of CJEU case law trajectory; measurement of exception carve-outs (posted workers, frontier workers, third-country nationals, children of EU citizens); assessment of whether legal boundary has moved toward inclusion or maintained closure',
    'If boundary degrades: the member sovereignty reading is losing structural coherence (constraint moves toward piton or rope). If boundary maintains: enforcement capacity is real and the constraint remains tangled rope.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(welfare_system_boundary_fungibility, empirical, 'Stability of the citizen/non-citizen welfare boundary under legal and political pressure').

omega_variable(
    sibling_reading_axiom_conflict,
    'Can both the member sovereignty axiom (member states retain closure authority) and the integration primary axiom (free movement is constitutive of EU membership) be held simultaneously by the same actor, or do they represent genuinely foreclosed alternatives?',
    'Jurisprudential analysis: examination of member state constitutional courts attempting to honor both axioms simultaneously; identification of cases where one axiom must yield to the other; assessment of whether the ''constrained'' institutional position (EU supranational authority) resolves or merely defers the conflict.',
    'If foreclosed: one reading must exit as institutional doctrine (either EU constitutional law rejects member sovereignty, or member states formally reject integration). If coexistent: the piton classification is correct — the constraint is a failed compromise maintained through performative acceptance by all parties.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_axiom_conflict, conceptual, 'Whether member sovereignty and integration axioms can coexist in a single institutional framework').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(federation_membership_obligations__member_sovereignty_primary, 1992, 2023).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fedmem_theater_1992_maastricht, federation_membership_obligations__member_sovereignty_primary, theater_ratio, 1992, 0.32).
narrative_ontology:measurement(fedmem_theater_2004_eastern_expansion, federation_membership_obligations__member_sovereignty_primary, theater_ratio, 2004, 0.42).
narrative_ontology:measurement(fedmem_theater_2015_refugee_crisis, federation_membership_obligations__member_sovereignty_primary, theater_ratio, 2015, 0.55).
narrative_ontology:measurement(fedmem_theater_2023_post_pandemic, federation_membership_obligations__member_sovereignty_primary, theater_ratio, 2023, 0.48).

% Extraction over time
narrative_ontology:measurement(fedmem_extractiveness_1992_maastricht, federation_membership_obligations__member_sovereignty_primary, base_extractiveness, 1992, 0.35).
narrative_ontology:measurement(fedmem_extractiveness_2004_eastern_expansion, federation_membership_obligations__member_sovereignty_primary, base_extractiveness, 2004, 0.48).
narrative_ontology:measurement(fedmem_extractiveness_2015_refugee_crisis, federation_membership_obligations__member_sovereignty_primary, base_extractiveness, 2015, 0.55).
narrative_ontology:measurement(fedmem_extractiveness_2023_post_pandemic, federation_membership_obligations__member_sovereignty_primary, base_extractiveness, 2023, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(fedmem_suppression_1992_maastricht, federation_membership_obligations__member_sovereignty_primary, suppression_requirement, 1992, 0.42).
narrative_ontology:measurement(fedmem_suppression_2004_eastern_expansion, federation_membership_obligations__member_sovereignty_primary, suppression_requirement, 2004, 0.58).
narrative_ontology:measurement(fedmem_suppression_2015_refugee_crisis, federation_membership_obligations__member_sovereignty_primary, suppression_requirement, 2015, 0.68).
narrative_ontology:measurement(fedmem_suppression_2023_post_pandemic, federation_membership_obligations__member_sovereignty_primary, suppression_requirement, 2023, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(federation_membership_obligations__member_sovereignty_primary, resource_allocation).
narrative_ontology:affects_constraint(federation_membership_obligations__member_sovereignty_primary, federation_membership_obligations__integration_primary).
narrative_ontology:affects_constraint(federation_membership_obligations__member_sovereignty_primary, federation_membership_obligations__selective_solidarity).
narrative_ontology:affects_constraint(federation_membership_obligations__member_sovereignty_primary, posted_worker_directive_implementation).
narrative_ontology:affects_constraint(federation_membership_obligations__member_sovereignty_primary, border_control_schengen_suspension).

% DUAL FORMULATION NOTE:
% The contested kernel federation_membership_obligations has three structurally distinct readings, each with its own constraint story and ε value. member_sovereignty_primary (this story, ε≈0.52, Tangled Rope) contrasts with integration_primary (ε≈0.38, Rope) and selective_solidarity (ε≈0.48, Tangled Rope). These are not three measurements of one constraint — they are three different constraints (three different readings of the kernel), with different beneficiary/victim distributions and different empirical bases. All three are linked via network.affects_constraints to show the kernel family. The three readings coexist as competing institutional positions held by different political coalitions; see reading_relations in cs_structure for the structural relationships (coexists_with for all pairs, indicating live multi-position dispute).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(federation_membership_obligations__member_sovereignty_primary, institutional, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
