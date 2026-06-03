% ============================================================================
% CONSTRAINT STORY: substance_control_legitimacy__harm_reduction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_substance_control_legitimacy__harm_reduction_reading, []).

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
 *   constraint_id: substance_control_legitimacy__harm_reduction_reading
 *   human_readable: Substance Control Legitimacy — Harm Reduction Reading
 *   domain: public_health_policy/criminal_justice/political_economy
 *
 * SUMMARY:
 *   The harm-reduction reading of substance control legitimacy frames drug
 *   use as a public health issue requiring medical intervention and harm
 *   minimization, not criminal punishment. State authority derives from a
 *   duty to reduce overdose death, disease transmission, and social harms —
 *   decriminalization of users and provision of treatment, needle exchange,
 *   and supervised consumption sites. This reading occupies a contested
 *   middle position between prohibition (criminalize all use) and
 *   legalization (decriminalize autonomous choice). Unlike prohibition,
 *   harm-reduction rejects criminalization of users and accepts that some
 *   substance use is inevitable. Unlike legalization, it asserts state
 *   authority to mandate treatment and regulate access. The constraint
 *   exhibits extraction through treatment mandates, surveillance of
 *   compliance, and therapeutic coercion — medicalized control replacing
 *   criminal control. It exhibits genuine coordination through overdose
 *   prevention, disease reduction, and access to stabilizing medications. The
 *   reading's structural delta: users are medicalized not criminalized (lower
 *   suppression than prohibition); treatment infrastructure expands
 *   (beneficiary extraction toward providers); black markets persist because
 *   supply remains criminalized (victims trapped in illicit trade); identity
 *   locks form as users internalize patient/recovery identity. Theater_ratio
 *   increases over the interval (0.42 → 0.58) as the gap widens between
 *   official harm-reduction rhetoric and actual continued criminalization of
 *   supply and enforcement of mandates.
 *
 * KEY AGENTS:
 *   - Substance users (criminalized): Face ongoing arrest for possession/use despite medicalization frame; trapped between criminal and medical systems (powerless/trapped → Snare)
 *   - Substance users (medicalized): Identity fused with 'patient' or 'person in recovery'; receive treatment access but face mandates and surveillance (powerless/identity_locked → Tangled Rope)
 *   - Public health authorities: Coordinate treatment infrastructure, reduce overdose mortality; expand jurisdiction and budgets; net beneficiaries (institutional/arbitrage → Rope)
 *   - Treatment providers and harm reduction practitioners: Benefit from public funding and treatment mandates; provide genuine clinical coordination; resource dependent (moderate/constrained → Tangled Rope)
 *   - Illicit market participants (dealers, manufacturers): Remain criminalized despite user medicalization; face violence and incarceration; trapped with no legitimate pathway (powerless/trapped → Snare)
 *   - Harm reduction coalitions: Organized advocates for expanded services; constrained by hostile policy, limited funding, legality margins (organized/constrained → Tangled Rope)
 *   - Criminal justice system: Performs drug war function despite official medicalization; high theater (institutional/arbitrage → Piton)
 *   - Analytical observer: Risks naturalizing contingent medicalization choice as immutable feature of public health (analytical/analytical → Mountain, false summit)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(substance_control_legitimacy__harm_reduction_reading, 0.48).
domain_priors:suppression_score(substance_control_legitimacy__harm_reduction_reading, 0.52).
domain_priors:theater_ratio(substance_control_legitimacy__harm_reduction_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(substance_control_legitimacy__harm_reduction_reading, extractiveness, 0.48).
narrative_ontology:constraint_metric(substance_control_legitimacy__harm_reduction_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(substance_control_legitimacy__harm_reduction_reading, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(substance_control_legitimacy__harm_reduction_reading, tangled_rope).
narrative_ontology:human_readable(substance_control_legitimacy__harm_reduction_reading, "Substance Control Legitimacy — Harm Reduction Reading").
narrative_ontology:topic_domain(substance_control_legitimacy__harm_reduction_reading, "public_health_policy/criminal_justice/political_economy").

domain_priors:requires_active_enforcement(substance_control_legitimacy__harm_reduction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(substance_control_legitimacy__harm_reduction_reading, 'bcc4ef88-486b-4646-8ab1-fe05ed62a307').
narrative_ontology:cs_kernel_codification('bcc4ef88-486b-4646-8ab1-fe05ed62a307', distributed).
narrative_ontology:cs_authority_grounding('bcc4ef88-486b-4646-8ab1-fe05ed62a307', extraction).
narrative_ontology:cs_interpretation_layer_present('bcc4ef88-486b-4646-8ab1-fe05ed62a307').
narrative_ontology:cs_reading_relation('bcc4ef88-486b-4646-8ab1-fe05ed62a307', substance_control_legitimacy__prohibition_reading, coexists_with).
narrative_ontology:cs_reading_relation('bcc4ef88-486b-4646-8ab1-fe05ed62a307', substance_control_legitimacy__legalization_reading, influences).
narrative_ontology:cs_axiom('bcc4ef88-486b-4646-8ab1-fe05ed62a307', foundational, substance_use_is_inevitable).
narrative_ontology:cs_axiom_status(substance_use_is_inevitable, holdable).
narrative_ontology:cs_axiom_grounding('bcc4ef88-486b-4646-8ab1-fe05ed62a307', substance_use_is_inevitable, empirically_contingent).
narrative_ontology:cs_axiom('bcc4ef88-486b-4646-8ab1-fe05ed62a307', foundational, state_medical_authority_legitimate).
narrative_ontology:cs_axiom_status(state_medical_authority_legitimate, holdable).
narrative_ontology:cs_axiom_grounding('bcc4ef88-486b-4646-8ab1-fe05ed62a307', state_medical_authority_legitimate, deontological).
narrative_ontology:cs_axiom('bcc4ef88-486b-4646-8ab1-fe05ed62a307', secondary, user_criminalization_counterproductive).
narrative_ontology:cs_axiom_status(user_criminalization_counterproductive, holdable).
narrative_ontology:cs_axiom_grounding('bcc4ef88-486b-4646-8ab1-fe05ed62a307', user_criminalization_counterproductive, empirically_contingent).
narrative_ontology:cs_reference_frame('bcc4ef88-486b-4646-8ab1-fe05ed62a307', medical_public_health_authority).
narrative_ontology:cs_drift_state('bcc4ef88-486b-4646-8ab1-fe05ed62a307', contemporary_post_opioid_crisis, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('bcc4ef88-486b-4646-8ab1-fe05ed62a307', '').
narrative_ontology:cs_kernel_id(substance_control_legitimacy__harm_reduction_reading, substance_control_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(substance_control_legitimacy__harm_reduction_reading, public_health_infrastructure).
narrative_ontology:constraint_beneficiary(substance_control_legitimacy__harm_reduction_reading, treatment_providers).
narrative_ontology:constraint_beneficiary(substance_control_legitimacy__harm_reduction_reading, harm_reduction_practitioners).
narrative_ontology:constraint_victim(substance_control_legitimacy__harm_reduction_reading, substance_users_subject_to_mandates).
narrative_ontology:constraint_victim(substance_control_legitimacy__harm_reduction_reading, illicit_market_participants).
narrative_ontology:constraint_victim(substance_control_legitimacy__harm_reduction_reading, informal_economy_workers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CRIMINALIZED USER (SNARE) — Faces mandatory treatment or incarceration; cannot exit the system without compliance. Criminalization persists despite the harm-reduction reading's stated logic. Maximum suppression, no alternatives.
constraint_indexing:constraint_classification(substance_control_legitimacy__harm_reduction_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: MEDICALIZED USER (TANGLED ROPE) — Identity fused with 'patient' or 'person in recovery.' Receives treatment access (genuine benefit) but also faces coercive mandates and surveillance. The identity frame makes voluntary exit unthinkable even when treatment barriers are lowered.
constraint_indexing:constraint_classification(substance_control_legitimacy__harm_reduction_reading, tangled_rope,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(national))).

% PERSPECTIVE 3: PUBLIC HEALTH AUTHORITY (ROPE) — Coordinates access to treatment infrastructure, reduces overdose mortality through harm reduction services (needle exchange, medication-assisted treatment, supervised consumption sites). Experiences the constraint as coordination: distributing medical resources to minimize harm. Net beneficiary through expanded authority and budget.
constraint_indexing:constraint_classification(substance_control_legitimacy__harm_reduction_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: TREATMENT PROVIDER NETWORK (TANGLED ROPE) — Benefits from public funding, treatment mandates, and expanded caseloads (extraction toward them). Also genuinely provides coordination function: managing scarce treatment slots, stabilizing dosing regimens, reducing overdose risk. High resource dependence but also real clinical mission.
constraint_indexing:constraint_classification(substance_control_legitimacy__harm_reduction_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 5: ILLICIT MARKET PARTICIPANT (SNARE) — Criminalization drives supply into unregulated channels. Harm-reduction framing does NOT decriminalize the market itself — users may access treatment, but dealers remain felony targets. Market participants face ongoing criminalization, violence, arrest. Trapped with no legitimate alternative.
constraint_indexing:constraint_classification(substance_control_legitimacy__harm_reduction_reading, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 6: HARM REDUCTION COALITION (TANGLED ROPE) — Organized advocates for needle exchange, medication-assisted treatment, supervised consumption. Experience genuine coordination function (reducing overdose death, preventing disease transmission). Also constrained by hostile policy environment, limited funding, and need to defend legitimacy constantly. Coordinating around a margin of legality.
constraint_indexing:constraint_classification(substance_control_legitimacy__harm_reduction_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: CRIMINAL JUSTICE SYSTEM (PITON) — Harm-reduction reading disputes the criminalization logic but the system persists through institutional inertia. Courts still process drug offenses, prisons still house drug offenders, police still enforce drug laws. The official reading is harm-reduction; the actual practice is criminalization. Theater_ratio high because the system performs drug war functions while nominally endorsing public health.
constraint_indexing:constraint_classification(substance_control_legitimacy__harm_reduction_reading, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER / INHERENT CONFLICT VIEW (MOUNTAIN) — From civilizational/universal perspective, some tension between medicalization and autonomy is inherent to any regulation of substances. The observer may frame this as an immutable trade-off: either restrict access (limiting harm and autonomy) or permit access (protecting autonomy at risk of harm). However, the structural data reveals this as a false summit — the actual constraint is a contingent institutional arrangement conflating criminal and medical authority.
constraint_indexing:constraint_classification(substance_control_legitimacy__harm_reduction_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(substance_control_legitimacy__harm_reduction_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(substance_control_legitimacy__harm_reduction_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(substance_control_legitimacy__harm_reduction_reading, TypeOther, context(agent_power(powerless), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(substance_control_legitimacy__harm_reduction_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(substance_control_legitimacy__harm_reduction_reading, TR),
    TR >= 0.70.

:- end_tests(substance_control_legitimacy__harm_reduction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.48): Moderate-high. Users subject to coercive treatment mandates, surveillance of compliance, and loss of autonomy even within medicalization frame — genuine extraction. But extraction is lower than pure prohibition because medicalization provides access to medications, health services, and reduced overdose risk. Treatment providers and public health authorities extract significant benefit through expanded budgets and authority. The harm-reduction frame legitimizes this extraction as 'health intervention' rather than punishment. Suppression (0.52): Moderate. Criminal penalties for supply remain in place, maintaining black markets and limiting user autonomy; but user suppression has modestly decreased (decriminalization of possession in some jurisdictions, harm reduction services allowing some operation outside law). Over the interval, suppression decreases slightly (0.65 → 0.52) as harm reduction services expand and decriminalization of use spreads, but remains substantial because supply criminalization persists. Theater ratio (0.58): Moderate-high. The harm-reduction framing performs legitimacy work: the state claims to be helping users through medicine rather than punishing them through criminalization. But implementation often combines medicalization theater with continued criminal enforcement — drug courts present as treatment but carry criminal penalties; supervised consumption sites operate in legal gray zones; treatment compliance is often court-ordered. Theater increases over the interval (0.42 → 0.58) as the rhetoric-reality gap widens — more jurisdictions adopt harm-reduction language while continuing criminalization of supply and enforcement of user compliance.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates deep perspectival fragmentation. Criminalized users see snare (no exit, maximum suppression). Medicalized users see tangled rope with identity lock (genuine treatment benefits alongside coercive mandates and internalized patient identity). Public health authorities see rope (coordinating harm reduction infrastructure, extracting expanded authority and budget). Treatment providers see tangled rope (funding and caseloads alongside genuine clinical mission). Illicit market participants see snare (ongoing criminalization, trapped). Harm reduction coalitions see tangled rope (organizing around margin of legality, constrained by hostile policy). Criminal justice system sees piton (official medicalization rhetoric masking continued drug war function and arrest activity). The analytical observer risks seeing mountain (inherent tension between harm reduction and user autonomy) but the structural data reveals false summit — the tension is between institutional medicalization and full legalization/decriminalization, not an immutable feature of substance governance.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective derives d from beneficiary/victim status and exit options. Criminalized users (trapped victims) experience maximum d ≈ 0.95 → high chi. Medicalized users (identity_locked victims) derive d from identity fusion + victim status, computing to d ≈ 0.85–0.89 (lower than trapped because they have some structural mobility, but internalized frame prevents exercise of exit). Public health authorities (beneficiary + arbitrage) derive d from extractive position but low cost to exit, computing to d ≈ 0.15–0.20 (strong beneficiary position). Treatment providers (mixed beneficiary + victim status, constrained exit) derive d from funding dependence balancing clinical mission, computing to d ≈ 0.45–0.55 (neutral position). The directionality derivation reflects the reading's core claim: medical authority extracts from users (high d, high chi) while providing coordinating services (moderate chi given the genuine health function).
 *
 * MANDATROPHY ANALYSIS:
 *   The harm-reduction reading resolves mandatrophy by showing that the constraint is genuinely tangled_rope, not a false dichotomy between rope (pure coordination) and snare (pure extraction). Users receive real health benefits (overdose prevention, disease reduction, medication access) that constitute genuine coordination function. They also face real extraction (treatment mandates, surveillance, identity lock, loss of autonomy). Both are structural, not observational artifacts. The reading's mandatrophy: if medicalization is merely criminalization by another name (snare dominant), harm-reduction framing is pure theater and the reading should foreclose legalization (legalization would expose the extraction). If medicalization genuinely reduces extraction vs. prohibition (tangled rope with real coordination function), the reading coexists with legalization as a stable equilibrium. The empirical record suggests the former: continued black markets, persistent criminalization of supply, and mandated treatment that exceeds what voluntary health-seeking would produce indicate that medicalization is criminalization theater. This suggests the harm-reduction reading does NOT stably coexist with legalization — legalization would foreclose the reading's legitimacy claim (state medical authority) by removing the state's justification (preventing worst harms through mandatory intervention).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    decriminalization_scope_ambiguity,
    'Does harm-reduction reading commit to full decriminalization of supply (dealers, manufacturers) or only decriminalization of possession and use?',
    'Jurisdictional analysis: compare outcomes under harm-reduction regimes that legalize supply (Portugal supply-side partial decriminalization, Switzerland heroin-assisted treatment with legal acquisition) vs. those that decriminalize use but maintain supply criminalization (US drug courts, medication-assisted treatment without supply legalization). Measure market violence, overdose death, incarceration rates.',
    'If supply remains criminalized: extractiveness remains 0.48–0.55 because illicit market participants face ongoing criminal penalty, and users depend on criminalized supply even after medicalization. If supply is decriminalized: extractiveness drops to 0.25–0.35 (genuine public health coordination with minimal extraction). The reading''s legitimacy claim is incomplete without supply-side resolution.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(decriminalization_scope_ambiguity, empirical, 'Whether harm-reduction reading extends to decriminalization of supply or only use').

omega_variable(
    mandate_coercion_threshold,
    'What threshold of coercive treatment mandates (drug courts, incarceration contingent on treatment compliance) marks the transition from harm-reduction to criminalization-adjacent enforcement?',
    'Comparative outcome analysis: jurisdictions with voluntary treatment access vs. mandatory treatment contingent on criminal justice involvement. Measure: user retention in treatment, overdose mortality, recidivism, self-reported autonomy, quality of life.',
    'If voluntary access yields equivalent harm reduction: coercive mandates are pure extraction (snare dominates). If mandatory treatment yields significantly better population outcomes: mandates are justified coordination cost (tangled rope holds). Current evidence suggests voluntary access with robust resources achieves near-identical outcomes to mandates; suggests mandates are extraction, not coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mandate_coercion_threshold, empirical, 'Coercive treatment mandate threshold relative to voluntarism').

omega_variable(
    legalization_reading_logical_distance,
    'Is the harm-reduction reading logically compatible with the legalization reading, or does accepting medical state authority foreclose individual autonomy claims?',
    'Doctrinal analysis: can a single legal framework (e.g., Swiss model, Portugal Decree-Law 30/2000) hold both ''substance use is a public health matter requiring state management'' AND ''competent adults retain autonomy over their own substance use''? Or does accepting medical authority axiomatically override autonomy claims?',
    'If compatible: harm-reduction and legalization coexist (relation: coexists_with). If incompatible: harm-reduction forecloses legalization (relation: forecloses). Current jurisdictional evidence suggests compatibility — states can medicalize while preserving autonomy over personal use. But philosophical tension remains: medical authority claims epistemic privilege in defining ''harm,'' which implicitly limits autonomy.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legalization_reading_logical_distance, conceptual, 'Logical compatibility between harm-reduction authority claim and autonomy claim').

omega_variable(
    false_summit_natural_law,
    'Does this reading naturalize a contingent institutional arrangement (state medical authority over substance use) as an immutable feature of public health governance?',
    'Historical counterfactual: is substance use state-medicalized in ALL functional public health systems, or only in some jurisdictions? If variable across jurisdictions with similar health outcomes, the constraint is contingent institutional. If universal, it approaches natural law.',
    'If contingent: the mountain perspective is a false summit. The ''inherent conflict'' framing masks a political choice about whether to medicalize substance use at all (vs. treating as personal choice with minimal state role). If universal: mountain classification holds.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(false_summit_natural_law, empirical, 'Whether state medical authority over substance use is natural law or contingent institutional arrangement').

omega_variable(
    black_market_persistence_mechanism,
    'Does the black market persist because supply remains criminalized, or because demand for untaxed/unregulated product is intrinsic to substance markets?',
    'Comparative jurisdictional analysis: measure black market size and violence in jurisdictions with full legalization (Canada cannabis, Portugal supply decriminalization) vs. medicalization without supply legalization vs. prohibition. Identify whether black market size correlates with supply criminalization level or with baseline demand elasticity.',
    'If criminalization-driven: decriminalizing supply could eliminate black market and associated harm (violence, contaminated product). Extractiveness drops. If demand-intrinsic: black market persists regardless of legalization status; harm-reduction constraint remains at current extractiveness level.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(black_market_persistence_mechanism, empirical, 'Black market persistence driver — criminalization vs. demand elasticity').

omega_variable(
    identity_locked_medicalization_trap,
    'Does medicalization create an identity lock (''person in recovery,'' ''patient'') that prevents users from returning to unproblematic use patterns or full autonomy even when structural barriers are removed?',
    'Longitudinal study of users who exit treatment and recovery narratives: measure whether users re-identify outside the patient frame after years of abstinence or stable use. Compare exit trajectories under medicalization vs. legalization frameworks (where no patient identity is institutionalized). Assess whether the patient identity persists as a cognitive binding mechanism even after structural barriers (legal penalties, treatment mandates) are removed.',
    'If identity lock persists: medicalization creates psychological extraction mechanism beyond structural extraction. User autonomy remains constrained by internalized patient identity even when material barriers are removed. Theater_ratio understates actual control mechanism. If identity lock is reversible: medicalization is purely structural extraction; autonomy can be recovered through exit.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_locked_medicalization_trap, empirical, 'Whether medicalization creates persistent identity lock binding').

omega_variable(
    contradiction_between_readings,
    'Does this harm-reduction reading genuinely resolve the kernel contradiction between prohibition and legalization, or does it occupy an unstable middle position that ultimately forecloses neither sibling reading?',
    'Historical analysis of jurisdictions that adopted harm-reduction framing: do they hold steady at medicalization, or do they trend toward legalization (as public accepts autonomy logic) or back toward prohibition (as political pressure reasserts criminalization)?',
    'If stable: harm-reduction is a genuinely distinct reading (relation to siblings: coexists_with). If trend toward legalization: harm-reduction is a temporary way station that legalization will foreclose. If trend toward prohibition: harm-reduction creates conditions for prohibition re-emergence. Current evidence (Portugal → decriminalization trajectory, Vancouver → supervised consumption expansion) suggests drift toward legalization and supply decriminalization.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(contradiction_between_readings, empirical, 'Stability of harm-reduction reading as distinct equilibrium vs. transitional state').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(substance_control_legitimacy__harm_reduction_reading, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(subhr_tr_t0, substance_control_legitimacy__harm_reduction_reading, theater_ratio, 0, 0.42).
narrative_ontology:measurement(subhr_tr_t5, substance_control_legitimacy__harm_reduction_reading, theater_ratio, 5, 0.5).
narrative_ontology:measurement(subhr_tr_t10, substance_control_legitimacy__harm_reduction_reading, theater_ratio, 10, 0.58).

% Extraction over time
narrative_ontology:measurement(subhr_be_t0, substance_control_legitimacy__harm_reduction_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(subhr_be_t5, substance_control_legitimacy__harm_reduction_reading, base_extractiveness, 5, 0.43).
narrative_ontology:measurement(subhr_be_t10, substance_control_legitimacy__harm_reduction_reading, base_extractiveness, 10, 0.48).

% Suppression requirement over time
narrative_ontology:measurement(subhr_su_t0, substance_control_legitimacy__harm_reduction_reading, suppression_requirement, 0, 0.65).
narrative_ontology:measurement(subhr_su_t5, substance_control_legitimacy__harm_reduction_reading, suppression_requirement, 5, 0.58).
narrative_ontology:measurement(subhr_su_t10, substance_control_legitimacy__harm_reduction_reading, suppression_requirement, 10, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(substance_control_legitimacy__harm_reduction_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(substance_control_legitimacy__harm_reduction_reading, substance_control_legitimacy__prohibition_reading).
narrative_ontology:affects_constraint(substance_control_legitimacy__harm_reduction_reading, substance_control_legitimacy__legalization_reading).
narrative_ontology:affects_constraint(substance_control_legitimacy__harm_reduction_reading, black_market_violence_dynamics).
narrative_ontology:affects_constraint(substance_control_legitimacy__harm_reduction_reading, treatment_mandate_coercion_threshold).

% DUAL FORMULATION NOTE:
% The harm-reduction reading is one of three readings of the substance_control_legitimacy kernel. All three readings share the same underlying structural phenomena (substance use, state authority, harm reduction vs. criminalization) but frame them differently. The harm-reduction reading is downstream of the kernel contest but has its own independent extractiveness (0.48) reflecting specific institutional configurations: decriminalization of users + medicalization + continued supply criminalization = moderate extraction. The sibling readings (prohibition, legalization) would have different ε values reflecting their different institutional structures. Network edges link all three readings and their downstream structural effects (black market violence, treatment mandate coercion).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(substance_control_legitimacy__harm_reduction_reading, powerless, 0.89).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
