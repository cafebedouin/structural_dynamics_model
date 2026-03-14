% ============================================================================
% CONSTRAINT STORY: credential_inflation_spiral
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_credential_inflation_spiral, []).

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
 *   constraint_id: credential_inflation_spiral
 *   human_readable: Credential Inflation Spiral
 *   domain: labor_markets/education/institutional_gatekeeping
 *
 * SUMMARY:
 *   Credential inflation spiral describes a self-reinforcing cycle in which
 *   employers increase educational credential requirements for hiring,
 *   workers respond by obtaining higher credentials, which causes employers
 *   to further inflate requirements to maintain differentiation, which drives
 *   workers to seek even more credentials. This generates extraction and
 *   suppression despite genuine coordination functions (education does build
 *   human capital; credentials do reduce employer uncertainty). The
 *   constraint exhibits temporal accumulation: extractiveness has nearly
 *   tripled over the measurement interval (0.22 → 0.58) as credential
 *   requirements have ratcheted upward (high school diploma → bachelor's →
 *   master's for roles that once required only technical training). Theater
 *   ratio has doubled (0.35 → 0.68) as credentialing activity increasingly
 *   focuses on signaling status rather than demonstrating job-specific
 *   capability. The spiral operates through institutional gatekeeping
 *   (credentialing bodies), labor market mechanisms (employer screening), and
 *   psychological dynamics (signaling-based differentiation). It exhibits all
 *   six constraint types from different perspectives, making it a diagnostic
 *   exemplar for how the same structural phenomenon can be perceived as
 *   coordination, extraction, temporary scaffolding, degraded ritual, or
 *   natural law depending on observer position.
 *
 * KEY AGENTS:
 *   - Entry-Level Applicants: Primary victim (powerless/trapped) — face inexorable ratcheting requirements with no exit option
 *   - Education Debt Bearers: Secondary victim (moderate/constrained) — benefit from human capital gains but suffer extraction through debt servicing
 *   - Credential Issuers: Primary beneficiary (institutional/arbitrage) — universities, accreditors benefit from enrollment expansion and tuition capture
 *   - Incumbent Degree Holders: Secondary beneficiary (powerful/arbitrage) — protect labor market position through credential barriers that they no longer face
 *   - Employers: Mixed beneficiary/victim (institutional/constrained) — benefit from candidate screening but suffer from cost escalation and credential inflation
 *   - Credentialing Bureaucracy: Institutional performer (institutional/constrained) — maintains certification theater through inertia and regulatory requirement
 *   - Alternative Credentialing Coalition: Organized agents (organized/constrained) — bootcamps, portfolio platforms building exit pathway with sunset logic
 *   - Analytical Observer: Civilizational context (analytical/analytical) — risks naturalizing contingent institutional arrangement as immutable market law
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(credential_inflation_spiral, 0.58).
domain_priors:suppression_score(credential_inflation_spiral, 0.65).
domain_priors:theater_ratio(credential_inflation_spiral, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(credential_inflation_spiral, extractiveness, 0.58).
narrative_ontology:constraint_metric(credential_inflation_spiral, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(credential_inflation_spiral, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(credential_inflation_spiral, tangled_rope).
narrative_ontology:human_readable(credential_inflation_spiral, "Credential Inflation Spiral").
narrative_ontology:topic_domain(credential_inflation_spiral, "labor_markets/education/institutional_gatekeeping").

domain_priors:requires_active_enforcement(credential_inflation_spiral).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(credential_inflation_spiral, credential_issuers).
narrative_ontology:constraint_beneficiary(credential_inflation_spiral, degree_holders_incumbent).
narrative_ontology:constraint_victim(credential_inflation_spiral, job_seekers_without_credentials).
narrative_ontology:constraint_victim(credential_inflation_spiral, workers_displaced_by_inflation).
narrative_ontology:constraint_victim(credential_inflation_spiral, educational_debt_bearers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ENTRY-LEVEL APPLICANT (SNARE) — Faces inexorable credential requirements that ratchet upward over career span. A high school diploma once qualified for middle-class work; now bachelor's degree required for entry roles; master's now required for advancement. No exit: cannot get hired without credentials, cannot acquire credentials without incurring debt or time opportunity cost, cannot escape the cycle. Trapped agent experiencing maximum extraction through gatekeeping and debt dependency.
constraint_indexing:constraint_classification(credential_inflation_spiral, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: EDUCATION DEBT BEARER (TANGLED ROPE) — Enters the system because it coordinates genuine skill acquisition (coordination function exists: education can build human capital), but bears asymmetric extraction through debt servicing. Constrained by debt obligations but also benefits from signaling value of credential. Mixed experience of coordination (skill/network gains) and extraction (debt burden, delayed household formation).
constraint_indexing:constraint_classification(credential_inflation_spiral, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: CREDENTIAL ISSUER (ROPE) — Universities and certification bodies experience the constraint as pure coordination: they solve employers' screening problem by certifying skill/ability and signaling status. Net beneficiary through tuition capture, endowment growth, and institutional prestige. Exit via arbitrage: can shift credentialing standards, raise admission criteria, or expand program offerings without fundamental institutional damage. Low experienced extraction.
constraint_indexing:constraint_classification(credential_inflation_spiral, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: INCUMBENT DEGREE HOLDER (ROPE) — Individual who holds a credential achieved at lower inflation point. Experiences the constraint as protective rather than extractive: credential acts as barrier to entry that protects their labor market position. Benefits from subsequent inflation (their earlier credential becomes relatively more valuable). Arbitrage option: can stay in role, switch to new credential track, or mentor entry paths. Net beneficiary.
constraint_indexing:constraint_classification(credential_inflation_spiral, rope,
    context(agent_power(powerful),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: EMPLOYER (TANGLED ROPE) — Employers benefit from credential sorting (coordination function: credentials reduce screening costs) but suffer from credential inflation (extraction: hiring costs rise as required qualification levels climb, talent pool shrinks, wage expectations increase). Constrained: must adopt credential requirements to compete for candidates but also suffers from rising hiring friction. Mixed coordination (reduced screening uncertainty) and extraction (cost escalation).
constraint_indexing:constraint_classification(credential_inflation_spiral, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: CREDENTIALING BUREAUCRACY (PITON) — Accreditation bodies, HR departments, degree-granting bodies maintain performative gatekeeping rituals. Theater ratio high (0.68): much credentialing activity is certification theater — degrees signal status/behavior rather than proving job-specific capability. The bureaucratic apparatus persists through institutional inertia and legal requirement rather than functional necessity. Employers know credentials don't guarantee job performance; workers know degrees don't ensure employment; yet the ritual continues because alternatives haven't fully emerged.
constraint_indexing:constraint_classification(credential_inflation_spiral, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: ALTERNATIVE CREDENTIALING COALITION (SCAFFOLD) — Organized agents (bootcamps, portfolio-based hiring, professional certifications, open-source communities) are building alternative verification pathways that bypass traditional degree gatekeeping. These alternatives show temporary coordination function with sunset logic: as portfolio-based hiring and skill-demonstration platforms mature, the traditional degree constraint loses extractive force. Constrained by incumbent institutional barriers but seeing visible exit path.
constraint_indexing:constraint_classification(credential_inflation_spiral, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER / SIGNALING THEORY VIEW (MOUNTAIN) — From a civilizational perspective on information asymmetry, credential inflation appears as an immutable property of labor market economics: employers cannot directly observe worker productivity, so credentials serve as costly signals; as signals devalue through inflation, actors must invest in more costly signals to maintain differentiation (Spence's job market signaling model). This appears as a law of markets. However, structural data on suppression and extraction mechanisms suggests this is a false summit: the spiral is contingent on specific institutional arrangements (debt-financed education, employer reliance on credentials, credentialing body gatekeeping), not fundamental to information asymmetry itself.
constraint_indexing:constraint_classification(credential_inflation_spiral, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(credential_inflation_spiral_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(credential_inflation_spiral, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(credential_inflation_spiral, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(credential_inflation_spiral, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(credential_inflation_spiral, TR),
    TR >= 0.70.

:- end_tests(credential_inflation_spiral_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. The constraint extracts through multiple channels: tuition capture by educational institutions, debt servicing by borrowers, hiring friction costs borne by employers, and opportunity cost borne by entry-level workers required to spend time/money on credentials rather than productive work. The extraction is not as severe as a pure snare (0.66+) because genuine human capital development occurs — the educational coordination function is real, not purely theatrical. However, the spiral component (continuous ratcheting of requirements) creates cumulative extraction that exceeds the static value of individual credentials. Suppression (0.65): High. Barriers to exit include debt obligation (15-25 year repayment periods), time cost of degree completion, competitive disadvantage of credential-free candidacy in employer screening, and lack of alternative verification mechanisms. Suppression is structural but not absolute — some individuals and career paths (trades, entrepreneurship, family-based business) show lower suppression. Theater ratio (0.68): High. Significant portion of credentialing activity is performative: employers know degree holders may not have job-specific skills; workers know degrees don't guarantee employment; credentials signal status/behavior/filtered ability rather than demonstrating capability. The theater has increased over the measurement interval as degree proliferation has diluted signaling value (credential inflation itself creates need for more theater to restore differentiation).
 *
 * PERSPECTIVAL GAP:
 *   The entry-level applicant perceives an immutable snare; the credentialing body perceives a pure coordination rope. The analytical observer risks perceiving a mountain (natural law of labor markets with information asymmetry). This gap reveals that what appears as a natural market equilibrium to institutional beneficiaries appears as an entrapment mechanism to those being extracted from. The credentialing bodies do not see themselves as maintaining gatekeeping — they see themselves as maintaining standards and sorting by ability. The entry-level applicant sees the same institution as an arbitrary barrier. The perspectival gap is not just difference in perception but difference in structural relationship to the extraction mechanism.
 *
 * DIRECTIONALITY LOGIC:
 *   Entry-level applicants are full targets of extraction: lack credentials, lack debt resources to acquire them quickly, trapped by employer screening requirements. Direction d ≈ 0.92 (near-full victim). Credentialing institutions are full beneficiaries: capture tuition, grow endowments, increase prestige through selectivity. Directional d ≈ 0.08 (near-full beneficiary with arbitrage exit). Incumbent degree holders are partial beneficiaries protected by barrier: directional d ≈ 0.25 (beneficiary but with moderate exit cost if they want to switch credentials). Employers are split: benefit from screening coordination but suffer from cost escalation. Their d ≈ 0.50-0.60 (symmetric to slightly toward extraction, constrained exit because hiring is core function). The analytical observer's d ≈ 0.72 (canonical analytical value) but risks being pulled toward naturalization (lower d) if they treat the spiral as inevitable market dynamic rather than contingent institutional arrangement.
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLUTION: This constraint resolves mandatrophy by showing that tangled rope classification is correct because BOTH genuine coordination functions AND asymmetric extraction are simultaneously present. Education does build human capital (coordination). Credential screening does reduce employer uncertainty (coordination). But the spiral mechanism causes continuous extraction: requirements inflate beyond actual job needs; debt servicing extracts from younger cohorts; barriers to entry accumulate over time. The constraint could not be classified as pure rope (coordination only) because suppression (0.65) and the presence of clear victims (entry-level applicants, debt bearers) are inconsistent with pure coordination. It could not be classified as pure snare (extraction only) because legitimate human capital development and employer screening benefits are real, not purely predatory. The tangled rope classification correctly captures that the same institutional mechanism (credential requirements) performs both coordination function (matching educated workers to complex jobs) and extraction function (gatekeeping, debt capture, signaling inflation). The theater ratio (0.68) indicates that the coordination function has degraded over time — much of the credentialing activity is now purely theatrical signaling rather than genuine capability verification — but the extraction persists because the institutional apparatus is self-reinforcing. Alternative credentialing pathways (scaffold perspective) offer genuine sunset possibility: if portfolio-based hiring and skill-demonstration platforms mature sufficiently, the traditional credential extraction mechanism loses its gatekeeping force. Until then, the constraint remains tangled rope with increasing theater component.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    spiral_endogenous_vs_exogenous,
    'Is credential inflation driven by endogenous signaling dynamics (workers must escalate credentials to maintain relative position) or exogenous institutional choices (employers could stop requiring credentials but don''t)?',
    'Counterfactual analysis: identification of moments when employer credential requirements changed without corresponding labor supply changes; cross-national comparison of countries where employers have de-emphasized credentials (e.g., Germany''s apprenticeship system vs credential inflation in US)',
    'If endogenous: spiral is more mountain-like (self-reinforcing equilibrium). If exogenous: spiral is more snare-like (employer gatekeeping). Classification shifts from institutional/piton to institutional/snare or vice versa.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(spiral_endogenous_vs_exogenous, empirical, 'Whether inflation is driven by endogenous signaling or exogenous institutional gatekeeping').

omega_variable(
    skill_mismatch_measurement,
    'Do jobs actually require the credentials they list, or is credentialism (valuing credentials beyond job necessity) the driving mechanism?',
    'Comparative analysis: job performance data for credentialed vs under-credentialed workers in same roles; task-skill mapping (comparing actual job tasks to educational content); longitudinal tracking of credential requirement changes in stable job categories',
    'If credentials genuinely required: spiral reflects real coordination function (human capital needs). If credentialism dominates: spiral is pure extraction (gatekeeping without functional value). Affects whether coordination_type is resource_allocation or purely enforcement_mechanism.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(skill_mismatch_measurement, empirical, 'Whether credentials reflect genuine skill requirements or pure gatekeeping').

omega_variable(
    debt_financing_alternative_possibility,
    'Would credential inflation persist if education were publicly funded or if debt were capped/forgiven? Is debt a driver of the spiral or a consequence of it?',
    'Policy natural experiments: cross-national comparison of countries with free/low-cost higher education vs debt-financed systems; analysis of countries that implemented debt forgiveness programs; simulation modeling of spiral dynamics under different financing regimes',
    'If debt is driver: suppression (0.65) is artificially elevated by debt mechanics; true suppression might be lower. Reforming financing could shift classification toward rope (pure coordination). If debt is consequence: spiral persists regardless of financing; classification remains tangled_rope/snare.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(debt_financing_alternative_possibility, empirical, 'Whether debt financing drives or merely amplifies credential inflation').

omega_variable(
    bottleneck_capacity_false_scarcity,
    'Is credential inflation driven by genuine bottleneck (limited slots in quality institutions) or false scarcity (artificial caps on admissions despite excess demand)?',
    'Institutional capacity analysis: enrollment trend analysis relative to facility/faculty capacity; comparison of admission rates across time and institutions; cross-institutional comparison of quality outcomes at different admission selectivity levels',
    'If genuine bottleneck: some credential gatekeeping is unavoidable coordination cost. If false scarcity: credentialing bodies are actively maintaining scarcity to preserve prestige/tuition capture. Affects whether extractiveness (0.58) is justified or inflated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(bottleneck_capacity_false_scarcity, empirical, 'Whether credential scarcity reflects genuine capacity limits or artificial gatekeeping').

omega_variable(
    alternative_verification_effectiveness,
    'Do alternative credentialing pathways (bootcamps, portfolio-based hiring, professional certifications) actually reduce hiring friction or merely segment the market without solving the underlying signaling problem?',
    'Comparative outcome tracking: career progression and earnings outcomes for bootcamp vs traditional degree holders; hiring velocity and wage offers for portfolio-based candidates; persistence of degree requirements after alternative pathways emerge',
    'If alternatives effective: scaffold perspective confirmed, sunset timeline is real, spiral contains sunset clause. If ineffective: alternatives may create stratification rather than exit, and spiral remains self-reinforcing. Classification of scaffold perspective shifts from feasible to aspirational.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_verification_effectiveness, empirical, 'Whether alternative credentialing reduces spiral or merely stratifies the market').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(credential_inflation_spiral, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cred_infl_tr_t0, credential_inflation_spiral, theater_ratio, 0, 0.35).
narrative_ontology:measurement(cred_infl_tr_t20, credential_inflation_spiral, theater_ratio, 20, 0.52).
narrative_ontology:measurement(cred_infl_tr_t40, credential_inflation_spiral, theater_ratio, 40, 0.68).

% Extraction over time
narrative_ontology:measurement(cred_infl_be_t0, credential_inflation_spiral, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(cred_infl_be_t20, credential_inflation_spiral, base_extractiveness, 20, 0.4).
narrative_ontology:measurement(cred_infl_be_t40, credential_inflation_spiral, base_extractiveness, 40, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(credential_inflation_spiral, resource_allocation).
narrative_ontology:boltzmann_floor_override(credential_inflation_spiral, 0.12).
narrative_ontology:affects_constraint(credential_inflation_spiral, student_debt_trap).
narrative_ontology:affects_constraint(credential_inflation_spiral, employer_hiring_friction).
narrative_ontology:affects_constraint(credential_inflation_spiral, wage_stagnation_credential_mismatch).

% DUAL FORMULATION NOTE:
% Credential inflation spiral decomposes into multiple structurally distinct constraints. Student debt trap focuses on extraction through debt financing mechanisms (ε ≈ 0.72, Snare). Employer hiring friction focuses on screening cost escalation (ε ≈ 0.48, Tangled Rope). Wage stagnation / credential mismatch focuses on divergence between credential requirements and actual job task requirements (ε ≈ 0.38, Tangled Rope). The spiral story presents the systemic interaction. Network links show that each downstream constraint is causally dependent on credential inflation dynamics.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(credential_inflation_spiral, institutional, 0.58).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
