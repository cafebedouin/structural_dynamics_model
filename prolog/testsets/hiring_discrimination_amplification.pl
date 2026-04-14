% ============================================================================
% CONSTRAINT STORY: hiring_discrimination_amplification
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_hiring_discrimination_amplification, []).

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
 *   constraint_id: hiring_discrimination_amplification
 *   human_readable: Hiring Discrimination Amplification Through Market Coordination
 *   domain: labor/employment/systemic_discrimination
 *
 * SUMMARY:
 *   Hiring discrimination amplification is a systemic constraint operating
 *   across labor markets where individual hiring decisions, coordinated
 *   through social networks, credential systems, and algorithmic screening,
 *   create cumulative extraction from discriminated groups while
 *   simultaneously providing genuine coordination benefits to hiring
 *   organizations. The constraint exhibits tangled rope structure: legitimate
 *   coordination (matching workers to jobs, standardizing hiring criteria,
 *   reducing arbitrary decision-making) is structurally inseparable from
 *   asymmetric extraction (bias patterns persist or amplify despite
 *   coordination mechanisms, suppression occurs through network gatekeeping
 *   and credential barriers, theater of diversity programs masks minimal
 *   actual change). The theater ratio (0.55) reflects that diversity
 *   initiatives, unconscious bias training, and diversity metrics are
 *   substantially performative — they provide organizations with compliance
 *   signaling and reputational benefit while leaving underlying
 *   discriminatory patterns largely unchanged. The extractiveness has grown
 *   from 0.45 to 0.62 over the measurement interval, indicating that
 *   algorithmic hiring, while framed as objective and unbiased, has amplified
 *   discrimination through pattern-matching on training data that encodes
 *   historical biases.
 *
 * KEY AGENTS:
 *   - Excluded Job Applicants: Primary victims (powerless/trapped) — face coordinated barriers across hiring networks with no viable exit from labor market
 *   - Marginalized Worker Pools: Secondary victims (moderate/constrained) — regional and sectoral concentration limits mobility; credential barriers impede entry
 *   - Hiring Organizations: Primary beneficiary (institutional/arbitrage) — extract labor cost advantages through discriminatory screening; experience constraint as pure coordination
 *   - Regulatory and Advocacy Coalition: Organized agents (organized/constrained) — EEOC, civil rights groups, employment lawyers perceive both coordination function and embedded extraction; constrained by enforcement capacity ceiling
 *   - Diversity Program Infrastructure: Institutional actor (institutional/arbitrage) — HR departments, diversity initiatives maintain theater without substantive change; piton classification reflects degraded function
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing discrimination as efficient market outcome
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hiring_discrimination_amplification, 0.62).
domain_priors:suppression_score(hiring_discrimination_amplification, 0.68).
domain_priors:theater_ratio(hiring_discrimination_amplification, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hiring_discrimination_amplification, extractiveness, 0.62).
narrative_ontology:constraint_metric(hiring_discrimination_amplification, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(hiring_discrimination_amplification, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hiring_discrimination_amplification, tangled_rope).
narrative_ontology:human_readable(hiring_discrimination_amplification, "Hiring Discrimination Amplification Through Market Coordination").
narrative_ontology:topic_domain(hiring_discrimination_amplification, "labor/employment/systemic_discrimination").

domain_priors:requires_active_enforcement(hiring_discrimination_amplification).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hiring_discrimination_amplification, privileged_demographic_groups).
narrative_ontology:constraint_beneficiary(hiring_discrimination_amplification, hiring_gatekeepers).
narrative_ontology:constraint_beneficiary(hiring_discrimination_amplification, pattern_maintainers).
narrative_ontology:constraint_victim(hiring_discrimination_amplification, discriminated_groups).
narrative_ontology:constraint_victim(hiring_discrimination_amplification, labor_market_fairness).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: EXCLUDED JOB APPLICANT (SNARE) — Faces systematic barriers to entry across coordinated hiring networks. No viable exit from the labor market. Bears full cost of discrimination with minimal coordination benefit. Maximum extraction experienced.
constraint_indexing:constraint_classification(hiring_discrimination_amplification, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: MARGINALIZED WORKER POOL (TANGLED ROPE) — Constrained by regional labor market concentration and credential barriers. Some coordination benefit from hiring protocols (standardized application processes) but asymmetric extraction through pattern-matching algorithms and social network gatekeeping.
constraint_indexing:constraint_classification(hiring_discrimination_amplification, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: HIRING ORGANIZATION (ROPE) — Benefits from coordination via standardized recruiting processes, talent pool access, and efficiency gains. Experiences hiring discrimination as a pure coordination mechanism. Low perceived extraction — constraint solves legitimate matching problem.
constraint_indexing:constraint_classification(hiring_discrimination_amplification, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: REGULATORY AND ADVOCACY COALITION (TANGLED ROPE) — Organized agents (EEOC, civil rights groups, employment lawyers) perceive both coordination function (standardizing hiring criteria, reducing arbitrary discrimination) and embedded extraction (patterns persist despite enforcement, theater of compliance without substantive change).
constraint_indexing:constraint_classification(hiring_discrimination_amplification, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: DIVERSITY PROGRAM INFRASTRUCTURE (PITON) — Nominally serves coordination function but exhibits high theater (0.55+): recruitment initiatives, diversity metrics, unconscious bias training persist despite minimal impact on actual hiring patterns. Institutional inertia maintains the programs; the primary function (fair hiring) has atrophied. Theater masks coordination failure.
constraint_indexing:constraint_classification(hiring_discrimination_amplification, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / MARKET EFFICIENCY VIEW (MOUNTAIN) — Naturalized view: hiring discrimination emerges as 'rational' actor response to information asymmetries and credential gaps. Social networks, pattern-matching, and homophily are portrayed as inevitable outcomes of preference and matching efficiency. This perspective risks falsely naturalizing what is actually a contingent institutional arrangement maintained by biased incentive structures.
constraint_indexing:constraint_classification(hiring_discrimination_amplification, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hiring_discrimination_amplification_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(hiring_discrimination_amplification, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(hiring_discrimination_amplification, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(hiring_discrimination_amplification, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(hiring_discrimination_amplification, TR),
    TR >= 0.70.

:- end_tests(hiring_discrimination_amplification_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62): Moderate-high and rising. The constraint extracts through wage suppression, opportunity denial, and psychological costs borne by excluded groups. Extractiveness is not 0.90+ because some hiring coordination genuinely occurs — organizations do solve matching problems better than random hiring. But the extraction is substantial and growing, particularly as algorithmic screening amplifies historical pattern-matching. The rise from 0.45 to 0.62 reflects algorithmic amplification: CV screening algorithms encode historical hiring biases, then apply them at scale with apparent objectivity. Suppression (0.68): High. Multiple barriers reinforce exclusion: network gatekeeping limits information flow to excluded groups, credential requirements (degrees, certifications, experience) are unevenly accessible, publication bias against discrimination research suppresses collective awareness, and identity_locked hiring gatekeepers resist alternative practices. Theater ratio (0.55): Moderate. Diversity programs, bias training, and diversity metrics provide organizational legitimacy and reputational benefits while producing minimal actual hiring pattern change. The theater has increased as algorithmic screening provides new legitimacy cover for old patterns (algorithms are framed as objective, removing human bias when they actually amplify it). The rise from 0.30 to 0.55 reflects Goodhart drift: organizations optimize for diversity metric visibility rather than fair hiring outcomes.
 *
 * PERSPECTIVAL GAP:
 *   The largest gap is between the hiring organization (rope, sees only coordination benefits) and the excluded applicant (snare, sees only extraction). This gap does not indicate perspectival disagreement — it indicates asymmetric power. Both parties are experiencing the same constraint; they experience it at radically different positions. The hiring organization's rope perspective is not wrong; it is incomplete. The organization genuinely solves a matching problem. But the solution is built on asymmetric access, network gatekeeping, and algorithmic pattern-matching that extracts from excluded groups. The regulatory coalition's tangled rope is the complete picture: both functions are real, and they are inseparable. This is diagnostic of tangled rope at the boundary condition — the constraint exists because the coordination function is real, but the extraction is not incidental to coordination; it is the mechanism through which coordination is achieved.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality derivation: Excluded applicants (victims, trapped) derive d ≈ 0.95 → f(d) ≈ 1.42 (maximum power modifier). Hiring organizations (beneficiaries, arbitrage) derive d ≈ 0.10 → f(d) ≈ -0.08 (institutional/negative modifier). The regulatory coalition (mixed role, constrained) derives d ≈ 0.50-0.60 (symmetric or victim-leaning depending on enforcement capacity). The diversity infrastructure (beneficiary nominally, constrained by its theater-dependent existence) derives d ≈ 0.25 (low effective power). These directionality values, combined with scope modifier σ(national) = 1.0 and base extractiveness ε = 0.62, produce the perspectival chi values: applicants experience χ ≈ 0.62 × 1.42 × 1.0 ≈ 0.88 (snare range), organizations experience χ ≈ 0.62 × (-0.08) × 1.0 ≈ -0.05 (rope range, negative extraction). The organized coalition experiences intermediate χ reflecting their constrained power.
 *
 * MANDATROPHY ANALYSIS:
 *   Mandatrophy is resolved by recognizing that hiring discrimination amplification is structurally tangled rope, not a mislabeled snare or rope. If it were pure snare (extraction only), organizations would have no incentive to maintain the hiring systems at all — they would be better served by random selection or lowest-cost hiring. If it were pure rope (coordination only), excluded groups would not bear disproportionate costs — the coordination benefits would be distributed. Instead: the constraint persists because it simultaneously coordinates (matches workers to jobs efficiently from the organization's perspective) and extracts (concentrates matching efficiency benefits among privileged groups while excluding others). The diversity program theater (piton classification from the institutional perspective) is the mechanism that sustains the tangled rope: theater signals organizational commitment to fairness while leaving discriminatory patterns unchanged, allowing both the coordination function and the extraction to continue. The mandatrophy is resolved: the constraint is tangled rope because both a genuine coordination function and asymmetric extraction are present and inseparable. Trying to eliminate extraction while preserving coordination (the regulatory approach) has failed because the extraction IS the mechanism of coordination from the beneficiary's perspective — they benefit precisely from the exclusion of those they perceive as lower-status competitors.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    statistical_discrimination_vs_preference,
    'To what degree is observed hiring discrimination driven by statistical discrimination (rational use of group-level proxies due to information gaps) versus pure animus-based preference discrimination?',
    'Audit studies controlling for credential signals; comparison of hiring gaps before/after credential transparency initiatives; analysis of hiring decisions when group identity is obscured',
    'If primarily statistical: constraint may be partially rope (information coordination failure). If primarily animus: constraint is more purely snare (extraction mechanism). If mixed: tangled rope classification is correct.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(statistical_discrimination_vs_preference, empirical, 'Whether discrimination is statistical or preference-based').

omega_variable(
    network_feedback_dynamics,
    'Are hiring networks self-reinforcing feedback loops (A-groups hire A-groups, creating homophily cascade) or do they reflect underlying labor market segmentation?',
    'Longitudinal network analysis of hiring pipelines; comparison of hiring diversity before/after network disruption (mergers, restructuring, external hiring mandates); measurement of network closure vs openness over time',
    'If networks self-reinforce: suppression is partly internalized (agents accept network gatekeeping). If networks reflect pre-existing segmentation: suppression is structural. This affects whether the constraint is identity_locked or trapped.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(network_feedback_dynamics, empirical, 'Whether hiring networks are self-reinforcing feedback loops').

omega_variable(
    algorithmic_amplification_mechanism,
    'Do hiring algorithms (CV screening, skill assessment) amplify historical discrimination or merely codify existing biases at no additional cost?',
    'Comparison of hiring diversity and discrimination rates pre- and post-algorithm deployment; analysis of algorithm training data bias; measurement of discrimination magnitude with vs without algorithmic filtering',
    'If amplification: extractiveness increases over time (Goodhart drift observable in measurements). If codification: extractiveness is stable. High amplification suggests transition from snare to systemic piton (degraded diversity theater masking worsened outcomes).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(algorithmic_amplification_mechanism, empirical, 'Whether algorithms amplify or merely codify historical discrimination').

omega_variable(
    identity_lock_in_hiring_gatekeepers,
    'Do hiring gatekeepers (recruiters, HR leaders, managers) maintain discriminatory patterns due to professional identity fusion with existing systems, even when alternatives are available?',
    'Qualitative interviews with gatekeepers pre/post diversity training and systemic change initiatives; measurement of gatekeeper willingness to adopt alternative hiring practices; career trajectory analysis of gatekeepers who do vs don''t adopt change',
    'If identity locked: organized agents in the advocacy coalition face identity-locked resistance from institutional hiring actors. This increases suppression and explains why compliance theater persists. Changes gatekeeper exit_options from arbitrage toward constrained or identity_locked.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_in_hiring_gatekeepers, conceptual, 'Whether hiring gatekeepers are identity-locked to existing discriminatory systems').

omega_variable(
    enforcement_capacity_ceiling,
    'What is the maximum discrimination reduction achievable through legal enforcement and compliance monitoring without restructuring hiring systems?',
    'Meta-analysis of EEOC settlements and their actual hiring pattern changes; measurement of compliance theater vs substantive change; estimation of enforcement effort required per unit discrimination reduction',
    'If ceiling is high: regulatory/advocacy coalition has real exit path and leverage. If ceiling is low: organized agents are also constrained (piton), theater substitutes for function. Affects whether scaffold sunset is achievable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_capacity_ceiling, empirical, 'Maximum discrimination reduction through legal enforcement alone').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hiring_discrimination_amplification, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hda_tr_t0, hiring_discrimination_amplification, theater_ratio, 0, 0.3).
narrative_ontology:measurement(hda_tr_t10, hiring_discrimination_amplification, theater_ratio, 10, 0.45).
narrative_ontology:measurement(hda_tr_t20, hiring_discrimination_amplification, theater_ratio, 20, 0.55).

% Extraction over time
narrative_ontology:measurement(hda_be_t0, hiring_discrimination_amplification, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(hda_be_t10, hiring_discrimination_amplification, base_extractiveness, 10, 0.55).
narrative_ontology:measurement(hda_be_t20, hiring_discrimination_amplification, base_extractiveness, 20, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hiring_discrimination_amplification, resource_allocation).
narrative_ontology:affects_constraint(hiring_discrimination_amplification, wage_gap_persistence).
narrative_ontology:affects_constraint(hiring_discrimination_amplification, credential_credentialing_circularity).
narrative_ontology:affects_constraint(hiring_discrimination_amplification, network_gatekeeping_effects).

% DUAL FORMULATION NOTE:
% Hiring discrimination amplification is upstream of wage gap persistence (lower-paid groups are those excluded from high-wage hiring networks) and downstream of credential circularity (exclusion from entry-level positions perpetuates credential gaps that trigger discriminatory screening in future hiring). These three constraints form a family with interdependent ε values. Network gatekeeping effects is the structural mechanism enabling both hiring discrimination and subsequent credential exclusion.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(hiring_discrimination_amplification, institutional, 0.18).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
