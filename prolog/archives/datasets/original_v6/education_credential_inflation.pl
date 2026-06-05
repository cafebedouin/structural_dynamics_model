% ============================================================================
% CONSTRAINT STORY: education_credential_inflation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_education_credential_inflation, []).

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
 *   constraint_id: education_credential_inflation
 *   human_readable: Education Credential Inflation
 *   domain: labor_market/education_policy/economic_coordination
 *
 * SUMMARY:
 *   Education credential inflation represents a structural constraint where
 *   expanding credential requirements for labor market entry function
 *   simultaneously as both coordination mechanism (credible signaling of
 *   worker quality across information asymmetry) and extraction mechanism
 *   (gatekeeping that concentrates benefits among educational institutions
 *   and degree-holders while raising barriers for new entrants). The
 *   constraint exhibits a temporal signature of degradation: credentialing
 *   began as functional signal (what you could do) and has evolved toward
 *   theatrical validation (that you completed time-in-seat), with
 *   theater_ratio rising from 0.35 to 0.68 over the 45-year interval. Base
 *   extractiveness has similarly risen from 0.28 to 0.61, indicating
 *   systematic credential inflation outpacing functional skill requirements.
 *   The constraint's enforceability depends on employers' continued
 *   acceptance of credentials as signals — once alternative credentialing
 *   mechanisms prove viable at scale, the extraction mechanism loses force.
 *   Until then, prospective workers face a trapped exit: labor market entry
 *   requires credentials, credentials require extended capital investment,
 *   capital is disproportionately available to higher-income families, and
 *   working-class agents face compounding extraction.
 *
 * KEY AGENTS:
 *   - Prospective Workers (powerless/trapped): Primary victims — entry-level workers requiring credentials they cannot easily afford; face multi-year capital investment with concentrated opportunity cost
 *   - Educational Institutions (institutional/arbitrage): Primary beneficiaries — capture growing revenue from credential expansion; have agency to set curricula, pricing, and market positioning
 *   - Large Employers (powerful/mobile): Secondary actors — use credentials to screen applicants but bear cost through wage inflation and overeducation of workforce; have options (hiring filters, training programs) but constrained by industry norms
 *   - Credentialing Standard-Setters (institutional/arbitrage): Institutional enforcers — professional associations and accreditation bodies maintain credential inflation through norm-setting and inertia
 *   - Working-Class Families (moderate/constrained): Secondary victims — face high cost to achieve credentials relative to household income; limited access to alternative credentialing pathways
 *   - Alternative Credential Providers (moderate/constrained): Emerging challenger — apprenticeship programs, competency-based certifications, bootcamps; constrained by employer skepticism and lack of scale
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(education_credential_inflation, 0.52).
domain_priors:suppression_score(education_credential_inflation, 0.58).
domain_priors:theater_ratio(education_credential_inflation, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(education_credential_inflation, extractiveness, 0.52).
narrative_ontology:constraint_metric(education_credential_inflation, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(education_credential_inflation, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(education_credential_inflation, tangled_rope).
narrative_ontology:human_readable(education_credential_inflation, "Education Credential Inflation").
narrative_ontology:topic_domain(education_credential_inflation, "labor_market/education_policy/economic_coordination").

domain_priors:requires_active_enforcement(education_credential_inflation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(education_credential_inflation, educational_institutions).
narrative_ontology:constraint_beneficiary(education_credential_inflation, credentialing_gatekeepers).
narrative_ontology:constraint_victim(education_credential_inflation, prospective_workers).
narrative_ontology:constraint_victim(education_credential_inflation, labor_market_efficiency).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ENTRY-LEVEL WORKER (SNARE) — Cannot enter labor market without credentials that require years and significant capital investment. No alternative credentialing pathways available at scale. Trapped in credential trap: degree required for positions that historically required high school diploma. Extraction is total and suppression is structural — lacks resources to bear cost of extended credential accumulation.
constraint_indexing:constraint_classification(education_credential_inflation, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: CAREER-CHANGER (TANGLED ROPE) — Faces high cost to credential switch (lost wages, tuition, time) but genuine coordination benefit exists: degree signals commitment and provides structured knowledge acquisition. Mixed extraction and coordination — agent bears cost but also benefits from clear credentialing pathway, even as pathway becomes longer and more expensive than necessary.
constraint_indexing:constraint_classification(education_credential_inflation, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: EDUCATIONAL INSTITUTION (ROPE) — Benefits from credential demand; experiences constraint as solving legitimate coordination problem: reliable way to signal worker quality to employers. Net beneficiary with arbitrage options (can adjust curriculum, pricing, formats). Low effective extraction because institution has agency to shape credential value.
constraint_indexing:constraint_classification(education_credential_inflation, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: LARGE EMPLOYER (TANGLED ROPE) — Uses credentials to screen applicants; genuine coordination benefit (reduces hiring risk), but also bears extraction cost through credential inflation: must hire more-credentialed workers than skills require, raising wage expectations and training burden. Mobile actor with options (can invest in training, adjust hiring filters) but constrained by industry norms requiring credentials.
constraint_indexing:constraint_classification(education_credential_inflation, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: CREDENTIALING STANDARD-SETTER (PITON) — Professional associations and accreditation bodies maintain credential requirements through institutional inertia rather than functional necessity. Bachelor's degree requirements for positions requiring rote task execution persist despite low functional value. Theater ratio high (maintaining credential standards is performative institutional activity). Real coordination function degraded over time as degree inflation decoupled credentials from actual skill requirements.
constraint_indexing:constraint_classification(education_credential_inflation, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) — From civilizational view, credential inflation represents genuine coordination mechanism (signaling worker quality across information asymmetry) coupled with extractive rent-seeking (inflating credential requirements beyond functional necessity). Coordination function: essential for matching workers to jobs. Extraction mechanism: expanding credential scope to maintain gatekeeping power as labor supply shifts.
constraint_indexing:constraint_classification(education_credential_inflation, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(education_credential_inflation_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(education_credential_inflation, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(education_credential_inflation, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(education_credential_inflation, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(education_credential_inflation, TR),
    TR >= 0.70.

:- end_tests(education_credential_inflation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. Credential inflation extracts value through three mechanisms: (1) rent-seeking by educational institutions (expanded tuition revenue), (2) redistribution of opportunity from working-class to credentialed agents, (3) misallocation cost where workers over-credentialed relative to actual skill requirements underutilize education. Value is not at maximum (0.70+) because significant coordination function remains — credentials do signal quality with better-than-random accuracy, and many workers do acquire useful skills. But extractiveness has grown over time (0.28→0.61), indicating rent-seeking is accelerating faster than credential value is improving. Suppression (0.58): Moderate-high. Multiple barriers suppress alternatives: employer skepticism of non-traditional credentials, institutional resistance to changing standards, working-class information constraints about alternative pathways, capital requirements that exceed individual agency for low-income prospective workers. Suppression is not total because some alternative pathways exist and are expanding; but they operate at small scale relative to traditional credentials. Theater ratio (0.68): High and rising. The performative component has grown as degree inflation decoupled from actual skill requirements. Many credentials function primarily as time-in-seat validation and social signal rather than skill certification. Professional associations maintain credential standards through institutional ritual (accreditation reviews, curriculum standards) with degraded functional verification of skill acquisition. The theater rises when: credential requirements expand without corresponding skill requirement increase; curricula drift toward general education rather than job-specific training; employers acknowledge over-credentialing but maintain degree requirements due to norm lock-in.
 *
 * PERSPECTIVAL GAP:
 *   The trapped worker (Snare) and institutional beneficiary (Rope) perceive opposite realities of the same constraint. For the prospective worker, credential inflation is an insurmountable barrier that grows steeper over time — extraction is total and suppression is structural. For the educational institution, the same constraint is a coordination solution: degree signals quality, graduates find jobs, institution delivers legitimate service. The gap reveals that the constraint's extractiveness is not uniform across positions. The temporal gap is equally sharp: in 1980 (t=0), credentials were more closely linked to actual job requirements (theater_ratio 0.35), and extractiveness was moderate (0.28). By 2025 (t=45), theater has doubled (0.68) and extractiveness has more than doubled (0.61), indicating the coordination function is degrading while extraction is accelerating. The large employer perspective (Tangled Rope) shows consciousness of the problem: employers recognize credentials are inflated but continue requiring them due to norm lock-in and lack of scalable alternatives. This is the diagnostic signature of Tangled Rope: the constraint solves a real coordination problem (screening workers) while imposing extraction (inflated requirements), and the beneficiaries know both are true.
 *
 * DIRECTIONALITY LOGIC:
 *   Each agent's structural position determines their experienced extractiveness (chi). Prospective workers are trapped (no exit), so they experience maximum d → maximum chi. Educational institutions are net beneficiaries with arbitrage options (can adjust offerings, pricing, market position), so they experience low d → low/negative chi. Large employers are in mixed position: they benefit from credential screening (lower hiring risk) but bear cost through wage inflation — mobile exit options but constrained by industry norms means moderate d → moderate chi. The analytical observer at civilizational scope sees the constraint as Tangled Rope: genuine coordination function (credible signaling) coupled with extractive rent-seeking (credential inflation beyond functional needs). The Piton perspective captures how credentialing standard-setters maintain the constraint through inertia despite acknowledging it is degraded — they benefit from maintaining standards (institutional power, gatekeeping) but the standards themselves have become performative.
 *
 * MANDATROPHY ANALYSIS:
 *   CREDENTIAL INFLATION resolves the mandatrophy by decomposing pure types from hybrid types: (1) The pure coordination function (Rope): credible signaling mechanism for employer-worker matching. (2) The rent-seeking extraction (Snare): credential inflation beyond functional requirement. These are not two perspectives on one constraint — they are two structural mechanisms in one constraint (Tangled Rope). Mandatrophy avoidance requires identifying both mechanisms clearly. The false peak to watch is the Mountain classification: 'credential requirements are natural, reflect inherent skill needs.' This false summit naturalizes what is actually a policy-contingent institutional arrangement. The alternative credential mechanisms (apprenticeship, competency-based certification, bootcamps) prove the mountain is false — if credential inflation were immutable law, alternatives would be structurally impossible. Their existence and partial viability shows the constraint is policy-moveable, hence not a natural law. The Scaffold classification appears if alternative credentialing scales (sunset mechanism for traditional degree requirements). Current state: Tangled Rope with increasing extraction, degrading coordination function, rising theater, and constrained but not yet defeated alternative pathways.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    skill_credential_decoupling_measurement,
    'What proportion of credential requirements reflect actual job skill needs versus rent-seeking credential inflation?',
    'Task analysis comparing job descriptions to actual work performed; assessment of skill acquisition in credentials versus on-the-job learning; comparative analysis of worker productivity by credential level vs actual experience',
    'If true skill gap (>70% decoupling): Snare classification dominates, extraction is primary mechanism. If modest gap (<30% decoupling): Rope dominates, coordination is primary mechanism. If intermediate (30-70%): Tangled Rope confirmed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(skill_credential_decoupling_measurement, empirical, 'Degree of decoupling between credential requirements and functional skill needs').

omega_variable(
    alternative_credentialing_viability,
    'Could apprenticeship, competency-based certification, or employer-sponsored credentialing replace degree-based signaling at scale?',
    'Pilot program outcomes; labor market signaling effectiveness of alternative credentials; employer acceptance rates; wage parity analysis for alternative-credentialed workers',
    'If viable alternatives exist: constraint is institutional (removable through policy), Scaffold classification gains validity. If alternatives fail to scale: trapped exit is structural, Snare dominates. If partial viability: Tangled Rope confirmed with partial sunset mechanism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_credentialing_viability, empirical, 'Scalability and effectiveness of alternative credentialing mechanisms').

omega_variable(
    credential_inflation_feedback_loop,
    'Does credential inflation create a self-reinforcing cycle where rising requirements force continuous credential expansion?',
    'Time series analysis of credential requirements versus labor market outcomes; identification of inflection points where credential inflation accelerates; correlation with labor market tightness and wage stagnation',
    'If self-reinforcing feedback loop exists: suppression increases over time, Snare risk escalates. If loop can be broken: policy intervention points exist, Scaffold becomes viable. If no loop: inflation is linear policy response, easier to reverse.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(credential_inflation_feedback_loop, empirical, 'Self-reinforcing nature of credential inflation cycle').

omega_variable(
    working_class_identity_lock,
    'Do working-class individuals internalize credential requirements as legitimate natural barriers, creating identity-locked exit despite potential for alternative pathways?',
    'Qualitative research on narrative frames around credential necessity; analysis of career trajectory choices when alternative pathways are available; assessment of whether barrier perception persists after structural removal',
    'If identity-locked predominates: exit_options should shift to identity_locked for some perspectives, changing classification landscape. If structural barriers are primary: trapped/constrained classification maintained. Gap reveals psychological capture mechanism.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(working_class_identity_lock, conceptual, 'Whether working-class agents are identity-locked by credential narratives').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(education_credential_inflation, 0, 45).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(edcred_tr_t0, education_credential_inflation, theater_ratio, 0, 0.35).
narrative_ontology:measurement(edcred_tr_t15, education_credential_inflation, theater_ratio, 15, 0.48).
narrative_ontology:measurement(edcred_tr_t30, education_credential_inflation, theater_ratio, 30, 0.62).
narrative_ontology:measurement(edcred_tr_t45, education_credential_inflation, theater_ratio, 45, 0.68).

% Extraction over time
narrative_ontology:measurement(edcred_be_t0, education_credential_inflation, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(edcred_be_t15, education_credential_inflation, base_extractiveness, 15, 0.4).
narrative_ontology:measurement(edcred_be_t30, education_credential_inflation, base_extractiveness, 30, 0.52).
narrative_ontology:measurement(edcred_be_t45, education_credential_inflation, base_extractiveness, 45, 0.61).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(education_credential_inflation, information_standard).
narrative_ontology:affects_constraint(education_credential_inflation, student_debt_trap).
narrative_ontology:affects_constraint(education_credential_inflation, wage_stagnation_despite_education).
narrative_ontology:affects_constraint(education_credential_inflation, intergenerational_wealth_inequality).

% DUAL FORMULATION NOTE:
% Credential inflation is upstream of multiple downstream constraints. Student debt accumulation (ε=0.68, Snare) is a consequence of credential inflation increasing tuition requirements. Wage stagnation despite rising educational attainment (ε=0.45, Tangled Rope) reflects that credential inflation outpaces real wage growth. Intergenerational inequality (ε=0.72, Snare) is amplified by credentialing barriers that concentrate opportunity among families with access to capital. Each downstream constraint has its own extractiveness value reflecting its specific mechanism; all three are causally affected by credential inflation dynamics.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(education_credential_inflation, moderate, 0.78).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
