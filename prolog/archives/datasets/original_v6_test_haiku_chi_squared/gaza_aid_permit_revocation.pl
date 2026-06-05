% ============================================================================
% CONSTRAINT STORY: gaza_aid_permit_revocation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_gaza_aid_permit_revocation, []).

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
 *   constraint_id: gaza_aid_permit_revocation
 *   human_readable: Revocation of Work Permits for Local Aid Workers in Gaza
 *   domain: political/humanitarian
 *
 * SUMMARY:
 *   The revocation of work permits for Palestinian staff of international
 *   humanitarian organizations operating in Gaza represents a structural
 *   extraction mechanism operated by the Israeli state through COGAT
 *   (Coordination of Government Activities in the Territories). The
 *   constraint functions through denial of legal employment authorization,
 *   creating pressure on aid workers to provide security intelligence or
 *   accept unemployment within a severely restricted labor market. The system
 *   is characterized by: (1) non-transparent revocation criteria with no
 *   meaningful appeal process; (2) collective punishment effects (entire
 *   organizations lose access when staff are revoked); (3) coercive pressure
 *   on workers (implicit and explicit conditioning of permit renewal on
 *   cooperation with security services); (4) claimed security rationale that
 *   cannot be independently verified; (5) extraction of organizational
 *   compliance through threat of total exclusion. The constraint exhibits
 *   mandatrophy resolution: the claimed coordination benefit (legitimate
 *   security screening) is bundled with extraction mechanisms (population
 *   control, intelligence procurement, aid distribution leverage) such that
 *   the coordination function cannot be separated from extraction.
 *
 * KEY AGENTS:
 *   - Palestinian Aid Workers: Primary victims (powerless/trapped) — face revocation without appeal, coercive pressure to provide intelligence, loss of livelihood within restricted economy
 *   - International Humanitarian Organizations: Secondary victims (moderate/constrained) — operational disruption, loss of local staff, coercive silence on security practices to maintain access
 *   - Israeli Security Apparatus (COGAT, Military Intelligence): Primary beneficiary (powerful/mobile) — gains intelligence procurement leverage, population control through aid distribution control, suppression of organizational criticism
 *   - Israeli State Administration: Secondary beneficiary (institutional/arbitrage) — frames permit system as legitimate regulatory coordination; maintains control over humanitarian access
 *   - International Humanitarian Community: Organized victim (organized/constrained) — collective action suppressed by competing mandate (serve population vs protest injustice); can mobilize diplomatic pressure but with limited effect
 *   - Gaza Displaced Population: Tertiary victim (powerless/trapped) — dependent on aid workers; loses access when workers are revoked; collective punishment effect
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gaza_aid_permit_revocation, 0.68).
domain_priors:suppression_score(gaza_aid_permit_revocation, 0.75).
domain_priors:theater_ratio(gaza_aid_permit_revocation, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gaza_aid_permit_revocation, extractiveness, 0.68).
narrative_ontology:constraint_metric(gaza_aid_permit_revocation, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(gaza_aid_permit_revocation, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gaza_aid_permit_revocation, snare).
narrative_ontology:human_readable(gaza_aid_permit_revocation, "Revocation of Work Permits for Local Aid Workers in Gaza").
narrative_ontology:topic_domain(gaza_aid_permit_revocation, "political/humanitarian").

domain_priors:requires_active_enforcement(gaza_aid_permit_revocation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gaza_aid_permit_revocation, israeli_security_establishment).
narrative_ontology:constraint_beneficiary(gaza_aid_permit_revocation, israeli_state_control_apparatus).
narrative_ontology:constraint_victim(gaza_aid_permit_revocation, palestinian_aid_workers).
narrative_ontology:constraint_victim(gaza_aid_permit_revocation, humanitarian_organizations).
narrative_ontology:constraint_victim(gaza_aid_permit_revocation, displaced_gaza_population).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PALESTINIAN AID WORKER (SNARE) — Trapped. Revocation eliminates livelihood within Gaza with no external employment available due to movement restrictions. No appeal mechanism; revocation is final. Faces pressure to provide security intelligence as condition of permit renewal. d≈0.92, f(d)≈1.40, σ=0.8 → χ≈0.75. Pure extraction with coercive mechanism.
constraint_indexing:constraint_classification(gaza_aid_permit_revocation, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: INTERNATIONAL HUMANITARIAN ORG (SNARE) — Constrained. Revocation of local staff undermines operational capacity. Organizations cannot easily replace specialized staff (local language, community trust, institutional memory). Limited ability to contest revocations through diplomatic channels. Can exit via withdrawal but at cost of abandoning beneficiary population. d≈0.68, f(d)≈1.05, σ=0.9 → χ≈0.65. Extraction through operational disruption.
constraint_indexing:constraint_classification(gaza_aid_permit_revocation, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: ISRAELI SECURITY APPARATUS (TANGLED ROPE) — Mobile exit option (can revoke or issue permits; can adjust policy). Perceives genuine security coordination function: identifying and excluding individuals with alleged security threats. Also extracts significant control benefit: leverage over which aid reaches which populations, intelligence from coerced workers. d≈0.35, f(d)≈0.32, σ=0.9 → χ≈0.20. Tangled: coordination rationale (security screening) bundled with extraction (population control, leverage).
constraint_indexing:constraint_classification(gaza_aid_permit_revocation, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(regional))).

% PERSPECTIVE 4: ISRAELI STATE ADMINISTRATION (ROPE) — Arbitrage exit (can adjust permit policy via administrative channels; can negotiate with international pressure). Frames constraint as coordination: ensuring aid reaches populations without undermining state security. Permits system coordinates who can work, when, where. d≈0.15, f(d)≈0.03, σ=1.0 → χ≈0.02. Institutional beneficiary; experiences as legitimate regulatory coordination.
constraint_indexing:constraint_classification(gaza_aid_permit_revocation, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: INTERNATIONAL HUMANITARIAN COMMUNITY (SNARE) — Organized actors (UN agencies, Red Crescent, major NGOs) constrained by competing mandates: maintain operations to serve population vs protest permit revocations and risk total exclusion. Can mobilize diplomatic pressure but only moderate effect. Revocation system extracts compliance (silence on security rationale) as price of continued access. d≈0.72, f(d)≈1.12, σ=1.2 → χ≈0.58. Extraction through institutional silence.
constraint_indexing:constraint_classification(gaza_aid_permit_revocation, snare,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (SNARE) — Structural analysis: permit revocation system has high base extractiveness (ε=0.68), high suppression (no appeal, no transparency, coercion mechanism), and meets snare gates (χ≈0.68 ≥ 0.66). Suppression ≥ 0.60 through lack of procedural safeguards. Minimal coordination benefit (security screening is pretext for control). System existence depends on suppressing: transparency, due process, appeals, alternative coordination mechanisms. d≈0.72, f(d)≈1.15, σ=1.2 → χ≈0.55.
constraint_indexing:constraint_classification(gaza_aid_permit_revocation, snare,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gaza_aid_permit_revocation_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(gaza_aid_permit_revocation, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(gaza_aid_permit_revocation, TypeOther, context(agent_power(powerful), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(gaza_aid_permit_revocation, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(gaza_aid_permit_revocation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base Extractiveness (ε=0.68): High. The system extracts multiple values: (1) labor from workers (payment but under coercion); (2) intelligence from workers (implicit/explicit pressure for security cooperation); (3) organizational compliance from NGOs (silence on abuses to maintain access); (4) population control (discretionary aid access). The extraction is not maximal (0.70+) because some legitimate security screening may be occurring, but the non-transparent mechanism and coercive component suggest extraction is dominant over coordination. Suppression (0.75): High. Suppression operates through: (1) no appeal mechanism (revocation is final and unreviewed); (2) lack of transparency in revocation criteria; (3) coercive threat (permit revocation = unemployment + potential family hardship); (4) organizational silence requirement (criticizing revocation risks total exclusion); (5) restricted labor market (no alternative employment available). Theater Ratio (0.58): Moderate. Theater is moderate rather than high (0.70+) because security screening has some genuine function content, even if extraction is primary. However, the theater component is rising over time as: (1) security rationale becomes more routinized and less individually justified; (2) organizational compliance becomes performative (NGOs publicly support security while privately documenting abuses); (3) permit reviews become annual ritual rather than individualized assessment.
 *
 * PERSPECTIVAL GAP:
 *   The constraint exhibits stark perspectival divergence. Palestinian aid workers perceive pure extraction and coercion (Snare). Humanitarian organizations perceive operational disruption but also moral complicity, creating internal contradictions (Snare). The Israeli security apparatus perceives legitimate security coordination bundled with necessary state control (Tangled Rope, where coordination is claimed though extraction is visible). The Israeli state administration perceives orderly regulatory coordination (Rope from institutional perspective). The international humanitarian community experiences institutional silence under duress (Snare). The analytical observer sees the system as a clear snare with suppressed coordination function (security screening exists but is pretext). The largest gap is between the security apparatus (Tangled Rope: 'we are solving a genuine security problem') and the aid workers (Snare: 'we are being coerced and have no exit').
 *
 * DIRECTIONALITY LOGIC:
 *   Palestinian Aid Workers: Victim + trapped → d≈0.92, f(d)≈1.40. Maximum extraction directionality. No employment alternative; no legal recourse; coercive pressure. f(d)≈1.40 (powerless victim with no exit) produces χ from moderate ε. Israeli Security Apparatus: Beneficiary + mobile → d≈0.35, f(d)≈0.32. Low to moderate extraction directionality despite being beneficiary, because mobile exit option means the apparatus chooses the constraint rather than being trapped by it. This differentiates them from powerless victims. Israeli State Administration: Beneficiary + arbitrage → d≈0.15, f(d)≈0.03. Minimal extraction directionality from state perspective (arbitrage exit means state has full policy flexibility). International Humanitarian Organizations: Victim + constrained → d≈0.68, f(d)≈1.05. Significant extraction directionality. Can exit but only at cost of abandoning beneficiary population (moral trap). Humanitarian Community: Victim + constrained (organized) → d≈0.72, f(d)≈1.12. High extraction despite organization because competing mandates limit actual exit capacity. Analytical Observer: Observer + analytical → d≈0.72, f(d)≈1.15. See full structure; perceive high extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY GATE SATISFIED (ε=0.68 > 0.70 requires mandatrophy_resolved: true). The constraint exhibits the classic mandatrophy structure: the Israeli security apparatus claims a genuine coordination function (security screening to prevent weapons/fighters from entering via aid distribution) that cannot be cleanly separated from extraction. How to resolve? (1) COORDINATION HYPOTHESIS: If security screening were primary, we would observe: transparent criteria, successful appeals, proportional revocations (only credible threats), absence of coercive intelligence demands. These are NOT observed. (2) EXTRACTION HYPOTHESIS: If extraction were primary, we would observe: non-transparent criteria, zero appeals, high revocation rates during population pressure peaks, explicit conditioning of permits on intelligence cooperation, organizational silence. These ARE largely observed. Resolution: The suppression structure (no appeal, no transparency, coercive mechanism) confirms that extraction is primary and coordination is pretext. The mandatrophy is resolved by showing that the suppression gates required for snare classification (suppression ≥ 0.60, χ ≥ 0.66) are met even if we grant the most charitable interpretation of security necessity. A legitimate security coordination system could exist with: (a) published criteria, (b) independent appeals, (c) proportional enforcement, (d) no intelligence coercion. The absence of these indicates the coordination function has been subordinated to extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    security_threat_legitimacy,
    'What percentage of permit revocations are based on credible security threats vs security pretexts for population control?',
    'Access to revocation rationales; forensic analysis of revocation patterns (correlation with aid distribution, population pressure points); interviews with security personnel; third-party security audits',
    'If >80% credible threats: reclassifies as primarily tangled_rope (coordination bundled with incidental extraction). If <30% credible: confirms snare classification (security is theater for population control).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(security_threat_legitimacy, empirical, 'Legitimacy of security threat rationale for revocations').

omega_variable(
    coercion_mechanism_scope,
    'How many revoked workers face direct coercion (pressure to provide intelligence) vs indirect pressure (loss of livelihood)?',
    'Confidential interviews with revoked workers; documentation of coercion attempts; cross-reference with intelligence service activity patterns',
    'If coercion scope >50%: suppression gate rises above 0.75 (high coercion). If <20%: suppression might lower to 0.55 (economic punishment without direct coercion), potentially borderline tangled_rope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coercion_mechanism_scope, empirical, 'Extent of direct coercion vs indirect economic pressure in revocation system').

omega_variable(
    appeal_mechanism_existence,
    'Do revoked workers have access to any formal appeal or reconsideration process, however limited?',
    'Documentation of official appeal procedures; empirical success rate of appeals; comparison with other permit systems'' due process standards',
    'If formal appeal exists and >5% succeed: suppression lowers (procedural floor raised). If no appeal exists or success <1%: confirms suppression ≥ 0.75 (system has no exit valve).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(appeal_mechanism_existence, empirical, 'Existence and efficacy of appeal mechanisms for permit revocation').

omega_variable(
    humanitarian_impact_threshold,
    'At what level of aid worker revocation do humanitarian organizations have moral/operational capacity to withdraw entirely rather than continue under coercion?',
    'Longitudinal tracking of org statements and withdrawal decisions; surveys of organizational leadership on threshold; empirical correlation between revocation rates and exit decisions',
    'If low threshold (<5% of staff): organizations have real exit option; reclassifies as tangled_rope from org perspective. If high threshold (>30%): confirms trapped classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(humanitarian_impact_threshold, preference, 'Humanitarian organization threshold for withdrawal vs continued operations').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gaza_aid_permit_revocation, 0, 12).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gaperm_tr_t0, gaza_aid_permit_revocation, theater_ratio, 0, 0.35).
narrative_ontology:measurement(gaperm_tr_t6, gaza_aid_permit_revocation, theater_ratio, 6, 0.48).
narrative_ontology:measurement(gaperm_tr_t12, gaza_aid_permit_revocation, theater_ratio, 12, 0.58).

% Extraction over time
narrative_ontology:measurement(gaperm_be_t0, gaza_aid_permit_revocation, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(gaperm_be_t6, gaza_aid_permit_revocation, base_extractiveness, 6, 0.58).
narrative_ontology:measurement(gaperm_be_t12, gaza_aid_permit_revocation, base_extractiveness, 12, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gaza_aid_permit_revocation, enforcement_mechanism).
narrative_ontology:affects_constraint(gaza_aid_permit_revocation, gaza_humanitarian_access_control).
narrative_ontology:affects_constraint(gaza_aid_permit_revocation, palestinian_labor_market_restriction).
narrative_ontology:affects_constraint(gaza_aid_permit_revocation, israeli_security_intelligence_system).

% DUAL FORMULATION NOTE:
% Permit revocation is downstream of the broader Israeli control system in Gaza (movement restrictions, security coordination, labor market regulation). This constraint represents the extraction mechanism within the humanitarian access domain specifically. Upstream constraints include general Palestinian movement restrictions and labor market control; downstream effects include humanitarian service delivery gaps and indirect population control through aid distribution leverage.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(gaza_aid_permit_revocation, powerful, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
