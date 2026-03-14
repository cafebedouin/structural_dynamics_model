% ============================================================================
% CONSTRAINT STORY: meritocracy_signaling_collapse
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_meritocracy_signaling_collapse, []).

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
 *   constraint_id: meritocracy_signaling_collapse
 *   human_readable: Meritocracy Signaling Collapse
 *   domain: social/economic/institutional
 *
 * SUMMARY:
 *   The meritocracy signaling collapse describes the structural degradation
 *   of credentials as reliable signals of capability in complex labor
 *   markets. As job specialization increases and educational curricula lag
 *   behind workplace demands, traditional credentials (undergraduate degrees,
 *   credentials) persist through institutional inertia despite declining
 *   predictive validity for job performance. The constraint exhibits all six
 *   DR types from different perspectives: for mobility aspirants without
 *   inherited capital, it is a snare with no exit (credentials are required
 *   but deliver diminished returns); for credential gatekeepers
 *   (universities), it is rope (they experience pure coordination benefits);
 *   for employers, it is tangled rope (genuine coordination problem mixed
 *   with credential inflation costs); for alternative assessment movements,
 *   it is scaffold with sunset (new pathways are building alternatives); for
 *   the university system itself, it is piton (the degree persists as ritual,
 *   not function); for the analytical observer, it risks appearing as
 *   mountain (inevitable complexity of labor markets) when the structural
 *   data reveals contingent institutional arrangements. The theater_ratio
 *   (0.68) reflects that traditional credentials involve substantial
 *   performative activity: attendance norms, credential presentation rituals,
 *   status signaling — increasingly disconnected from actual capability
 *   measurement or job readiness.
 *
 * KEY AGENTS:
 *   - Mobility Aspirants Without Inherited Capital: Primary victim (powerless/trapped) — must signal merit through credentials but face credential inflation and cost barriers; no alternative signaling pathways available
 *   - Credential Gatekeepers (Universities): Primary beneficiary (institutional/arbitrage) — benefit from enrollment demand, tuition capture, status from degree prestige; arbitrage options available (online education, alternative models)
 *   - Employers: Secondary beneficiary and victim (moderate/constrained) — benefit from standardized screening but bear costs of credential inflation and skill mismatch; constrained by labor market structure
 *   - Legacy Advantage Holders: Secondary beneficiary (powerful/mobile) — credentials activate inherited advantages (family networks, test prep access, internship capacity); maintain mobility through credential gatekeeping
 *   - Alternative Assessment Movement: Organized reformers (organized/mobile) — building portfolios, skills-based hiring, apprenticeships, bootcamps as credential alternatives with exit pathways
 *   - Meritocratic Legitimacy (Abstract): Primary victim (powerless/trapped) — abstract collective good; each signaling failure erodes system legitimacy but no agent owns the problem
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(meritocracy_signaling_collapse, 0.58).
domain_priors:suppression_score(meritocracy_signaling_collapse, 0.62).
domain_priors:theater_ratio(meritocracy_signaling_collapse, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(meritocracy_signaling_collapse, extractiveness, 0.58).
narrative_ontology:constraint_metric(meritocracy_signaling_collapse, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(meritocracy_signaling_collapse, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(meritocracy_signaling_collapse, tangled_rope).
narrative_ontology:human_readable(meritocracy_signaling_collapse, "Meritocracy Signaling Collapse").
narrative_ontology:topic_domain(meritocracy_signaling_collapse, "social/economic/institutional").

domain_priors:requires_active_enforcement(meritocracy_signaling_collapse).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(meritocracy_signaling_collapse, credential_gatekeepers).
narrative_ontology:constraint_beneficiary(meritocracy_signaling_collapse, legacy_advantage_holders).
narrative_ontology:constraint_victim(meritocracy_signaling_collapse, mobility_aspirants).
narrative_ontology:constraint_victim(meritocracy_signaling_collapse, meritocratic_legitimacy).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: MOBILITY ASPIRANT (SNARE) — Faces interlocking credential inflation and suppressed alternative pathways. Must signal merit through credentials, but credential scarcity and cost escalation eliminate the escape route that credentials once provided. Trapped by the requirement to signal merit while signaling mechanisms degrade. Maximum extraction — no exit available except abandonment of mobility aspiration itself.
constraint_indexing:constraint_classification(meritocracy_signaling_collapse, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: CREDENTIAL GATEKEEPER (ROPE) — Experiences the signaling system as pure coordination: screening applicants for capability and fitting talent to roles. Benefits from the signaling flow (status, tuition/enrollment revenue, talent pipeline), but experiences it as coordination rather than extraction. Arbitrage exit available — can always shift to alternative talent-assessment methods if needed. Net beneficiary.
constraint_indexing:constraint_classification(meritocracy_signaling_collapse, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: EMPLOYER DEPENDENT ON SIGNALS (TANGLED ROPE) — Faces genuine coordination problem (how to identify talent without expensive direct assessment?) alongside extraction: credential inflation increases hiring costs and mismatches capability to job requirements. Benefits from standardized screening (coordination) while bearing costs of credential substitution for actual capability (extraction). Constrained by labor market structure — cannot exit credential reliance without costly alternative assessment infrastructure.
constraint_indexing:constraint_classification(meritocracy_signaling_collapse, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: ALTERNATIVE ASSESSMENT MOVEMENT (SCAFFOLD) — Organized initiatives (coding bootcamps, portfolio-based hiring, skills-based assessment, competency frameworks) represent temporary alternative pathways with sunset logic. As alternative assessment matures and network effects shift hiring practices, the traditional credential monopoly's extraction mechanism loses force. Organized agents have agency and see a clear exit timeline — 15-25 years for norms to shift as alternative assessment demonstrates capability-predictive power.
constraint_indexing:constraint_classification(meritocracy_signaling_collapse, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: UNIVERSITY CREDENTIAL SYSTEM (PITON) — Traditional undergraduate degree as labor market signal has lost functional capacity. The degree persists through institutional inertia: employers require it despite declining predictive validity for job performance; universities maintain it despite declining relevance to actual work demands; students pursue it despite rising costs and diminishing returns. The ritual (completion, transcript, diploma) is largely performative — maintained because both sides still use it as a coordination focal point, not because it effectively signals capability. Theater ratio (0.68) reflects the degree's degraded function masked by institutional persistence.
constraint_indexing:constraint_classification(meritocracy_signaling_collapse, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational/universal perspective, educational signaling collapse appears as an inherent feature of complex labor markets: as capabilities become more specialized and job requirements diverge from educational curricula, credential-based screening mechanically becomes less predictive. The collapse seems immutable — an inescapable consequence of economic specialization. However, the structural data contradicts this naturalization: the credential monopoly's persistence is institutional, not physical. The mountain classification reveals how extractive constraints hide behind naturalizing language ('that's just how hiring works').
constraint_indexing:constraint_classification(meritocracy_signaling_collapse, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(meritocracy_signaling_collapse_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(meritocracy_signaling_collapse, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(meritocracy_signaling_collapse, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(meritocracy_signaling_collapse, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(meritocracy_signaling_collapse, TR),
    TR >= 0.70.

:- end_tests(meritocracy_signaling_collapse_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The credential system captures enrollment revenue, status rents from degree prestige, and employer dependency on credentials for screening. But the extraction is not total — employers genuinely face a coordination problem (how to identify talent?), and some agents do achieve mobility through credentials. The moderate value reflects that the credential monopoly is mixed coordination-and-extraction rather than pure extraction. Suppression (0.62): Moderate-high. Barriers to non-credential pathways include employer screening norms, legal licensing requirements, network-based hiring (which favors inherited connection capital), geographic immobility barriers for apprenticeships, and cultural devaluation of non-degree work. Suppression has increased over the interval as credential inflation has accelerated. Theater ratio (0.68): High and increasing. Traditional credentials involve substantial performative activity disconnected from capability measurement: attendance rituals, GPA calculation, degree presentation, employer credential verification — increasingly ceremonial rather than functional. As job specialization has outpaced curriculum evolution, the degree's predictive validity has declined while its theater has increased. Portfolio-based hiring and skills tests have lower theater because they target actual capability directly.
 *
 * PERSPECTIVAL GAP:
 *   Credential gatekeepers see coordination (rope); mobility aspirants see extraction (snare). This gap reveals that the 'meritocratic' framing hides extraction behind coordination language. Both perspectives are structurally accurate — the signaling system does solve a coordination problem AND it does extract from those without inherited advantages. The gap is not a disagreement about facts but a disagreement about which structural function dominates. The tangled rope classification from the employer perspective makes this explicit: genuine coordination + asymmetric extraction = mixed constraint.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values (d) are derived from beneficiary/victim status and exit options. Credential gatekeepers are beneficiaries with arbitrage options (can shift to alternative models if needed) — they experience low d, low effective extraction (rope). Mobility aspirants are victims with trapped exit (credentials are required but provide diminishing returns, and alternatives are suppressed) — they experience high d, high effective extraction (snare). Employers are both beneficiaries (gain from standardized screening) and victims (bear credential inflation costs) with constrained exit (cannot escape labor market structure without building alternative assessment infrastructure) — they experience moderate d, moderate extraction (tangled rope). The alternative assessment movement has organized capacity and mobile exit (can build new pathways) — they experience lower d than trapped agents despite working against institutional inertia. Legacy advantage holders are beneficiaries with mobile exit (inherited capital provides alternatives to credential signaling) — they experience low d.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy resolves by recognizing that meritocracy signaling is a tangled rope, not a rope (pure coordination) and not a snare (pure extraction). The coordination function is real: employers need signals, and credentials do provide standardized screening. But the coordination is asymmetric — inherited advantages can substitute for signaling entirely (resource-rich aspirants can signal capability through networks, internships, geographic mobility) while resource-poor aspirants are trapped in the signaling system and its degrading mechanisms. The theater ratio (0.68) and increasing trend reveal Goodhart drift: as the degree becomes more universal, its signaling value declines, but institutional use persists through inertia. The meritocratic ideology naturalizes this as inevitable ('everyone gets a degree now, so the degree means less') rather than as institutional extraction. The alternative assessment movement demonstrates that the constraint is not immutable — new signaling mechanisms are emerging with lower theater and better capability prediction. The sunset is structural, not aspirational: coding bootcamps have 15+ year adoption curve in tech; portfolio-based hiring is accelerating; apprenticeships are being revived with employer backing. The analytical mountain perspective risks treating credential collapse as inevitable complexity when it is actually a choice about whether to maintain credential monopoly enforcement.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    credential_inflation_mechanism,
    'Is credential inflation driven primarily by genuine capability sorting or by credentialism (employers using credentials as risk-shifting proxy)?',
    'Longitudinal analysis of credential inflation rate vs. actual job skill requirements; correlation between credential level and on-the-job performance; employer substitution behavior when alternative signals become available',
    'If credentialism dominates: constraint is pure extraction (Snare) with minimal coordination function. If capability sorting dominates: constraint is legitimate coordination (Rope) with signal degradation as a side effect.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(credential_inflation_mechanism, empirical, 'Whether credential inflation reflects sorting or credentialism').

omega_variable(
    signaling_substitution_feasibility,
    'Can alternative assessment methods (portfolios, skills tests, work samples, apprenticeships) actually replace traditional credentials at scale without reconstructing the same inflation dynamic?',
    'Empirical tracking of alternative assessment adoption rates; comparison of credential-replacement markets (tech bootcamps, apprenticeships) with traditional credentialism; identification of whether alternative signals develop their own inflation cycles',
    'If feasible without inflation: scaffold sunset is structural (alternative pathways are real). If alternatives reconstruct inflation: constraint is structural to any screening mechanism (mountain) rather than to credentials specifically.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(signaling_substitution_feasibility, empirical, 'Whether alternative assessment can replace credentials without inflation').

omega_variable(
    inherited_advantage_decomposition,
    'What portion of the constraint''s extractive force comes from credential signaling per se versus from inherited resource advantages (test prep access, unpaid internship capacity, geographic mobility)?',
    'Controlled comparison of credential-matched cohorts with different resource backgrounds; analysis of signaling efficacy separately from resource distribution; cross-national variance in credential-based extraction controlling for resource inequality',
    'If signaling dominates: focus reform on assessment mechanisms (portfolio-based hiring, skills tests). If inherited advantages dominate: credential system is a proxy mechanism for deeper inequality (reform addresses wrong layer).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(inherited_advantage_decomposition, empirical, 'Decomposition of credential signaling from inherited advantage mechanisms').

omega_variable(
    identity_lock_in_meritocracy_frame,
    'Do mobility aspirants'' identities become fused with meritocracy ideology itself, making exit unthinkable even when the signaling system demonstrably fails to deliver mobility?',
    'Longitudinal analysis of agent belief persistence vs. structural outcomes; identification of identity shifts when agents experience signaling collapse directly; comparative study of agents with identity_locked vs. constrained exit framings',
    'If identity lock is strong: agents perceive the constraint as immutable (mountain from their perspective) and internalize blame for signaling failure. If weak: agents perceive the constraint as exploitative (snare) and organize for reform.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_in_meritocracy_frame, conceptual, 'Identity fusion with meritocracy ideology').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(meritocracy_signaling_collapse, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(merit_tr_t0, meritocracy_signaling_collapse, theater_ratio, 0, 0.42).
narrative_ontology:measurement(merit_tr_t10, meritocracy_signaling_collapse, theater_ratio, 10, 0.55).
narrative_ontology:measurement(merit_tr_t20, meritocracy_signaling_collapse, theater_ratio, 20, 0.68).

% Extraction over time
narrative_ontology:measurement(merit_be_t0, meritocracy_signaling_collapse, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(merit_be_t10, meritocracy_signaling_collapse, base_extractiveness, 10, 0.47).
narrative_ontology:measurement(merit_be_t20, meritocracy_signaling_collapse, base_extractiveness, 20, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(meritocracy_signaling_collapse, information_standard).
narrative_ontology:affects_constraint(meritocracy_signaling_collapse, credential_cost_escalation).
narrative_ontology:affects_constraint(meritocracy_signaling_collapse, inherited_advantage_accumulation).
narrative_ontology:affects_constraint(meritocracy_signaling_collapse, employer_screening_infrastructure).

% DUAL FORMULATION NOTE:
% Meritocracy signaling collapse is downstream of credential cost escalation but represents a distinct constraint on labor market sorting mechanisms. The upstream constraint (cost) is about resource barriers; this constraint is about signaling efficacy degradation. Separate stories enable tracking how cost inflation and signaling collapse interact to exclude mobility aspirants.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(meritocracy_signaling_collapse, institutional, 0.28).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
