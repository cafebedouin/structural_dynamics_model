% ============================================================================
% CONSTRAINT STORY: huang_expectation_resilience_2026
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_huang_expectation_resilience_2026, []).

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
 *   constraint_id: huang_expectation_resilience_2026
 *   human_readable: The Stanford Expectation Trap (Resilience Scarcity)
 *   domain: social/technological/psychological
 *
 * SUMMARY:
 *   Jensen Huang's 2026 thesis identifies a structural constraint: high
 *   expectations, especially those internalized from elite institutional
 *   affiliation, extract psychological resilience faster than individuals can
 *   regenerate it. The constraint operates through identity lock
 *   (institutional affiliation becomes fused with self-concept such that
 *   failure threatens psychological continuity) combined with suppression of
 *   alternative identity frameworks (elite institutional narratives obscure
 *   socioeconomic selection effects and legitimate the meritocratic frame).
 *   Over a 20-year biographical horizon, the constraint manifests as
 *   progressively higher performance expectations compounded on previous
 *   success, with no structural off-ramps. This creates a resilience deficit:
 *   the psychological energy required to maintain the expected trajectory
 *   exceeds what can be sustainably generated, particularly under competitive
 *   peer contexts where status signaling is constant. The constraint exhibits
 *   all six DR types from different perspectives, mapping onto different
 *   structural relationships to the expectation extraction mechanism.
 *   Graduates themselves experience pure extraction (Snare). Institutions
 *   experience mixed coordination-extraction (Tangled Rope) — they genuinely
 *   benefit from peer networks while extracting reputation value. Mental
 *   health interventions see a temporary problem with a sunset (Scaffold).
 *   The meritocratic legitimation system is substantially performative
 *   (Piton). Non-elite peers are constrained by the benchmark (Snare).
 *   Analytical observers risk naturalizing status-seeking as inevitable
 *   (false Mountain). The constraint is active and expanding: credentialism
 *   is intensifying, elite institution premium is increasing, and
 *   psychological health metrics in high-expectation cohorts are declining.
 *
 * KEY AGENTS:
 *   - Elite institution graduates (Powerless/Trapped): Primary victims — internalized expectation creates identity lock; exit options are purely psychological and require dismantling institutional self-concept
 *   - Non-elite peers (Moderate/Constrained): Secondary victims — trapped by status deficit relative to elite-credentialed cohort; mobile options exist but constrained within primary peer network
 *   - Elite institutions (Organized/Constrained): Institutional beneficiary — coordinates peer networks while extracting reputation value from graduate success; actively enforces expectation narratives
 *   - Selective employers (Powerful/Arbitrage): Secondary beneficiary — exploits expectation premium in recruitment and work-intensity demands; coordinate signal value of credentials
 *   - Mental health and well-being advocacy (Organized/Mobile): Scaffold provider — building interventions (counseling, peer support, resilience training) that enable exit from regime without dismantling expectation system
 *   - Meritocratic legitimation system (Institutional/Arbitrage): Performative mechanism — sustains expectation narrative by attributing success to merit despite socioeconomic selection
 *   - Analytical observer (Analytical/Analytical): Risk of false summit — naturalizing expectation extraction as inherent to human status-seeking rather than contingent institutional design
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(huang_expectation_resilience_2026, 0.52).
domain_priors:suppression_score(huang_expectation_resilience_2026, 0.68).
domain_priors:theater_ratio(huang_expectation_resilience_2026, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(huang_expectation_resilience_2026, extractiveness, 0.52).
narrative_ontology:constraint_metric(huang_expectation_resilience_2026, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(huang_expectation_resilience_2026, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(huang_expectation_resilience_2026, snare).
narrative_ontology:human_readable(huang_expectation_resilience_2026, "The Stanford Expectation Trap (Resilience Scarcity)").
narrative_ontology:topic_domain(huang_expectation_resilience_2026, "social/technological/psychological").

domain_priors:requires_active_enforcement(huang_expectation_resilience_2026).
% --- Structural relationships ---
narrative_ontology:constraint_victim(huang_expectation_resilience_2026, elite_institution_graduates).
narrative_ontology:constraint_victim(huang_expectation_resilience_2026, high_expectation_cohorts).
narrative_ontology:constraint_victim(huang_expectation_resilience_2026, psychological_resilience_commons).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ELITE GRADUATE (SNARE) — Internalized expectation becomes self-enforcing. Exit options are purely psychological (reframe failure, reject status metrics) but require dismantling the epistemic foundation of institutional identity. Biographical horizon shows extraction over entire career arc — early success compounds into ever-higher expectation, with diminishing psychological slack. Trapped because institutional affiliation creates identity lock: rejecting the expectation means rejecting the institution that conferred status. Maximum experienced extraction; no structural exit.
constraint_indexing:constraint_classification(huang_expectation_resilience_2026, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: NON-ELITE PEER (SNARE) — Constrained exit: institutional legitimacy comes from proximity to elite-credentialed peers. Leaving the cohort means losing collaborative advantage and network status. Moderate power because broader institutional contexts (industry, academia, non-profit) offer alternative validation paths, but within the primary peer network the constraint is extractive. Bearer of costs: benchmarking against elite peer performance, status deficit from non-elite origin, access gaps in high-signal opportunities. Some agency (can seek alternative networks) but significant extraction within the primary context.
constraint_indexing:constraint_classification(huang_expectation_resilience_2026, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: ELITE INSTITUTION (TANGLED ROPE) — The university benefits from reputation compounding: graduates' public success enhances institutional signaling and donor confidence. Organized agents (admissions, development, marketing) actively enforce and cultivate the expectation narrative. But institutions also experience genuine coordination functions: shared values, research collaboration networks, alumni mentoring are real public goods. Tangled Rope: institution coordinates peer advancement while extracting reputation value from each successful graduate. Has sunset logic implicitly (when reputation is fully realized) but enforcement actively prevents sunset — institution reinvests extraction into expectation maintenance.
constraint_indexing:constraint_classification(huang_expectation_resilience_2026, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 4: MENTAL HEALTH COALITION (SCAFFOLD) — Mental health advocacy, peer support programs, and resilience training initiatives (campus counseling, mindfulness programs, vulnerability-normalizing workshops) are building exit pathways from the expectation trap. These are temporary structural interventions: designed to support individuals through the current high-expectation regime while longer-term cultural shifts decouple institutional prestige from psychological extraction. Mobile exit because coalition members can allocate resources to alternative priority areas if the underlying expectation system changes. Sunset logic: as psychological resilience norms mature (therapy destigmatization, metrics-agnostic identity formation), the intensive scaffolding should decline. Theater is moderate (some genuine well-being work; some performative DEI compliance compliance).
constraint_indexing:constraint_classification(huang_expectation_resilience_2026, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: MERITOCRATIC LEGITIMATION (PITON) — The narrative that elite institution attendance = genuine merit = future success is substantially performative. Institutional selection heavily correlates with socioeconomic background, school quality (pre-college inequality), and cultural capital — not pure merit. Yet the meritocratic story persists and graduates internalize it as explanation for their success. Theater_ratio high because much institutional messaging (admissions branding, commencement narratives, alumni success profiles) reinforces the meritocratic frame while obscuring selection effects. Theater > 0.70 indicates degraded function — the legitimation mechanism no longer credibly justifies extraction but persists through inertia. Why? Because alternative legitimation systems (lottery, inheritance, explicit class hierarchy) are less palatable to modern institutions and graduates alike.
constraint_indexing:constraint_classification(huang_expectation_resilience_2026, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: SELECTIVE EMPLOYER (TANGLED ROPE) — Corporations and top academic programs preferentially recruit from elite institutions. Coordination function: elite institutions provide filtered, high-signal candidates; employers can rely on credential for initial competence. Extraction function: employers benefit from asymmetric information (graduates overestimate what the credential signals; employers exploit the expectation premium in salary negotiation and work intensity demands). Powerful agents with arbitrage (can shift recruiting focus to other institutions if labor market conditions change). Tangled Rope because genuine coordination benefit (hiring signal) coexists with extraction (expectation premium, willingness to work at unsustainable intensity due to 'elite' self-concept).
constraint_indexing:constraint_classification(huang_expectation_resilience_2026, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / FALSE SUMMIT (MOUNTAIN) — From a civilizational view, some degree of social comparison and status-seeking is inherent to human cognition and group formation. The 'expectation extract' appears as inevitable: wherever there are visible status differences, psychological pressure to maintain relative position will follow. However, structural data reveals this as naturalization: the intensity and mechanism (institutional affiliation as identity lock, credential-based career sorting) are contingent institutional arrangements, not laws of human nature. Psychological resilience is not intrinsically scarce — it becomes scarce under specific institutional designs that link identity to status and status to unsustainability.
constraint_indexing:constraint_classification(huang_expectation_resilience_2026, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(huang_expectation_resilience_2026_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(huang_expectation_resilience_2026, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(huang_expectation_resilience_2026, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(huang_expectation_resilience_2026, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(huang_expectation_resilience_2026, TR),
    TR >= 0.70.

:- end_tests(huang_expectation_resilience_2026_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The constraint extracts psychological resilience — a measurable resource that regenerates but is finite. The extraction rate (expectation-driven performance demands) exceeds regeneration rate (time for psychological recovery, supportive relationships, identity diversification) for sustained periods. Early career extractiveness is lower (0.35) because post-college resilience is still high and expectation loads can be met. By mid-career (0.48), compound effects of repeated high-expectation performance begin to degrade resilience. Late measurement (0.52) reflects saturation: individuals are at or near their psychological capacity. Suppression (0.68): High. Multiple barriers prevent exit: institutional identity lock (leaving the elite frame means losing the credential premium and requiring new identity formation); peer network lock (status position is defined within the cohort, exit means status loss); labor-market lock (alternative credentials are less signal-rich, exit means career uncertainty); psychological lock (internalized expectations feel like personal values, not external constraints). Suppression is high because all four lock mechanisms reinforce each other. Theater ratio (0.58): Moderate-high. Institutional messaging (commencement narratives, alumni success profiles, prestige signaling) is substantially performative — it tells the story of meritocratic achievement while obscuring the degree to which pre-college inequality shapes initial trajectory. Mental health interventions add theater by appearing to address the expectation problem while leaving the underlying expectation system intact. Theater has increased over the interval (from 0.42 to 0.58) as institutional messaging has become more sophisticated and mental health services have become more visible, creating an appearance of addressing the problem without structural change.
 *
 * PERSPECTIVAL GAP:
 *   This constraint shows radical disagreement across perspectives. Graduates in the snare perspective experience the expectation as intrinsic identity threat — failure means dissolving their sense of self. The institution in the tangled_rope perspective genuinely believes it is coordinating peer excellence through shared values while extracting reputation value. The mental health coalition in the scaffold perspective sees a solvable problem requiring interventions and norms shifts (destigmatization of therapy, metrics-agnostic identity formation) over 10-15 years. The selective employer in the tangled_rope perspective sees a coordination mechanism (credential signal) that happens to benefit them. The piton perspective reveals that much institutional and employer framing is performative — the meritocratic narrative no longer credibly explains outcomes but persists because alternatives (explicit class sorting, lottery) are less palatable. The analytical observer's false summit occurs when they frame the constraint as inherent to human nature rather than as the product of institutional design choices. The perspectival gaps are largest between graduates and institutions (snare vs tangled_rope), revealing the asymmetry: institutions experience the expectation system as beneficial coordination; graduates experience it as extractive and identity-threatening.
 *
 * DIRECTIONALITY LOGIC:
 *   Each agent's directionality (d) derives from their structural position relative to the expectation extraction mechanism. Elite graduates are full targets (d ≈ 0.95) — they bear psychological cost, have trapped exit options, and internalize the expectation as personal identity. The agent_power = powerless reflects that individual psychological resources cannot reliably overcome institutional identity engineering. Non-elite peers occupy constrained exit positions (d ≈ 0.75) with moderate power — they experience extraction through status benchmarking but have alternative network options (mobile = constrained not trapped). Elite institutions are full beneficiaries (d ≈ 0.15, derived from organized power + arbitrage exit) — they accumulate reputation value with low cost and can reallocate institutional messaging if strategic advantage shifts. Selective employers are beneficiaries with moderate agency (d ≈ 0.25) — they exploit expectation premium but their labor-market role gives them arbitrage options (shift recruiting focus). Mental health coalitions occupy mobile positions with organized agency (d ≈ 0.50) — they benefit from problem recognition (securing resources) while working to reduce constraint severity, and they can exit if institutional barriers or alternative priorities emerge. The meritocratic legitimation system is institutional (d ≈ 0.05) — it is the mechanism that enables extraction by providing narrative cover, benefiting from the constraint's existence. The analytical observer (d ≈ 0.72, analytical power) risks naturalizing the constraint by treating it as an inevitable property of human status-seeking rather than a contingent institutional design.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy (Snare vs Scaffold vs Tangled Rope?) by explicitly modeling multiple valid perspectives. The constraint is simultaneously a Snare (for powerless trapped graduates), a Tangled Rope (for institutions and employers), and a Scaffold (for mental health interventions). There is no single 'correct' classification — the perspectival disagreement is itself the diagnostic signal. However, the core constraint for the primary victims (graduates) is Snare: high extractiveness (0.52), high suppression (0.68), no coordination function visible to the target agent. The tangled_rope and scaffold perspectives are from secondary or intervening agents, not from the primary victim. The mandatrophy is resolved by recognizing that the system exhibits both genuine coordination (peer networks, institutional mentoring) and asymmetric extraction (reputation value accrual, career status premium) simultaneously — which is the definition of tangled_rope. The institutional beneficiary's experience (tangled_rope) and the graduate's experience (snare) coexist in the same system because they occupy different structural positions. From the graduate's perspective, the coordination benefit (peer network access, institutional credential) is insufficient to offset the extraction cost (psychological resilience demand, identity-fusion risk, career continuity pressure). From the institution's perspective, the coordination function is real and valuable. The mandatrophy resolves by recognizing that mandatrophy itself is a perspectival phenomenon: different agents can have incompatible experiences of the same constraint, and this incompatibility is not a classification error but a structural feature of hybrid systems.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    expectation_internalization_pathway,
    'What is the causal mechanism by which institutional affiliation becomes identity-fused such that exit becomes psychologically catastrophic rather than merely socially costly?',
    'Longitudinal psychological study tracking identity formation and affiliation strength in elite vs non-elite cohorts; analysis of anxiety/depression spikes correlated with institutional identity threats; narrative analysis of graduate self-concept formation over biographical time.',
    'If identity fusion is primarily institutional engineering (institutional messaging, selective reinforcement of achievement): constraint can be weakened by reframing institutional role as context, not identity. If identity fusion is primarily cognitive universal (status-seeking is native to human psychology): constraint is more resilient and requires deeper individual resilience work.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(expectation_internalization_pathway, empirical, 'Mechanism of institutional-identity fusion in elite-cohort psychology').

omega_variable(
    resilience_scarcity_source,
    'Is the observed resilience deficit in high-expectation cohorts a product of extraction (constraint drains resilience faster than it can regenerate) or of psychological overwhelm (expectation-driven work intensity exceeds regeneration capacity)?',
    'Comparative resilience measures across cohorts with similar work intensity but different expectation levels; ecological momentary assessment of stress recovery periods; analysis of whether resilience deficit persists when work intensity is equalized.',
    'If extraction: constraint is a snare where the mechanism is hope-depletion (repeated failure-against-expectation erodes willingness to attempt again). If overwhelm: constraint is a scaffold where high work intensity is temporary and resilience would recover if intensity decreased. Classification changes from Snare to Scaffold.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(resilience_scarcity_source, empirical, 'Whether resilience deficit is extraction-driven or intensity-driven').

omega_variable(
    credential_substitution_feasibility,
    'Can alternative credential systems (skill-based hiring, portfolio assessment, apprenticeship pathways) credibly replace elite-institution affiliation as labor-market signal within 15-20 year horizon?',
    'Analysis of non-traditional hiring success rates in high-skill industries (tech, finance, academia); measurement of credential decay: how long does the elite institution premium persist for non-recent graduates; emergence of competing signal systems and their adoption rates among employers.',
    'If feasible: the institutional affiliation lock loses force, graduates can exit via alternative credentialing, expectation extraction declines. Constraint moves toward Scaffold (temporary). If infeasible: elite affiliation remains identity-locking mechanism, extraction persists. Constraint remains Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(credential_substitution_feasibility, empirical, 'Feasibility of alternative credential systems replacing elite-institution signal').

omega_variable(
    psychological_resilience_trainability,
    'To what degree can psychological resilience be built through intervention (therapy, meditation, skill-building) within the high-expectation regime, versus requiring structural change (reducing expectations, decoupling identity from status)?',
    'Meta-analysis of resilience intervention outcomes in elite populations; comparison of intervention efficacy with and without parallel expectation-reduction components; longitudinal tracking of whether gains persist when high-expectation environment continues.',
    'If resilience is highly trainable within regime: Scaffold interventions are sufficient; constraint can be managed without structural change. If resilience requires structural change: Scaffold is insufficient; constraint requires dismantling expectation system or decoupling identity from institutional affiliation. Piton perspective becomes predictive: institutional scaffolding becomes performative without structural change.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(psychological_resilience_trainability, empirical, 'Trainability of psychological resilience within high-expectation regimes').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(huang_expectation_resilience_2026, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(huang_tr_t0, huang_expectation_resilience_2026, theater_ratio, 0, 0.42).
narrative_ontology:measurement(huang_tr_t10, huang_expectation_resilience_2026, theater_ratio, 10, 0.52).
narrative_ontology:measurement(huang_tr_t20, huang_expectation_resilience_2026, theater_ratio, 20, 0.58).

% Extraction over time
narrative_ontology:measurement(huang_be_t0, huang_expectation_resilience_2026, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(huang_be_t10, huang_expectation_resilience_2026, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(huang_be_t20, huang_expectation_resilience_2026, base_extractiveness, 20, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(huang_expectation_resilience_2026, information_standard).
narrative_ontology:affects_constraint(huang_expectation_resilience_2026, credential_inflation_labor_markets).
narrative_ontology:affects_constraint(huang_expectation_resilience_2026, psychological_resilience_commons_degradation).
narrative_ontology:affects_constraint(huang_expectation_resilience_2026, elite_institutional_reputation_extraction).

% DUAL FORMULATION NOTE:
% The expectation trap constraint decomposes into three structurally distinct claims: (1) Psychological resilience extraction (this story, ε=0.52, Snare from graduate perspective) captures the mechanism of individual psychological drain. (2) Credential inflation in labor markets (ε=0.60, Tangled Rope) captures how expectation-driven credentialism creates hiring barrier escalation. (3) Institutional reputation extraction (ε=0.35, Tangled Rope) captures how institutions extract reputation value from graduate success. These three are linked: institutional reputation extraction drives credential inflation, which in turn increases expectation loads and psychological extraction. Each story has different beneficiary/victim groups and different ε values reflecting different measurement bases (psychological vs labor-market vs institutional). All three are downstream of socioeconomic inequality (pre-college educational variance) which creates the initial selection advantage that elite institutions leverage.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(huang_expectation_resilience_2026, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
