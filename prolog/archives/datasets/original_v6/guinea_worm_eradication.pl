% ============================================================================
% CONSTRAINT STORY: guinea_worm_eradication
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_guinea_worm_eradication, []).

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
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: guinea_worm_eradication
 *   human_readable: Global Guinea Worm Eradication Program
 *   domain: public_health/social
 *
 * SUMMARY:
 *   The Global Guinea Worm Eradication Program, initiated by The Carter
 *   Center in 1986, represents a large-scale public health coordination
 *   mechanism with minimal coercive overhead and a genuine sunset clause. The
 *   program provides safe drinking water sources, health education, and
 *   disease surveillance to eliminate Guinea worm infection from endemic
 *   regions across Africa and South Asia. Unlike extraction-based
 *   constraints, the eradication program creates coordinated public goods
 *   from which all participants benefit directly. As case counts have
 *   declined from millions annually (1986) to 13 cases globally (2024), the
 *   program exhibits the characteristic trajectory of a successful scaffold:
 *   active coordination infrastructure gradually transitions to
 *   verification-only surveillance, with theater ratio increasing as the
 *   coordination function winds down.
 *
 * KEY AGENTS:
 *   - Endemic Population: Primary beneficiary (powerless/mobile) — direct health benefit from safe water access and disease elimination
 *   - The Carter Center & Partners: Coordinator (institutional/arbitrage) — organizes technical expertise, funding, and implementation across regions
 *   - National Health Ministries: Organized partners (organized/mobile) — integrate surveillance into health systems; benefit from disease elimination and capacity building
 *   - Global Health Community: Institutional beneficiary (institutional/arbitrage) — gains from proof-of-concept eradication and methodological knowledge
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — tracks sunset trajectory as parasite elimination approaches
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(guinea_worm_eradication, 0.12).
domain_priors:suppression_score(guinea_worm_eradication, 0.08).
domain_priors:theater_ratio(guinea_worm_eradication, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(guinea_worm_eradication, extractiveness, 0.12).
narrative_ontology:constraint_metric(guinea_worm_eradication, suppression_requirement, 0.08).
narrative_ontology:constraint_metric(guinea_worm_eradication, theater_ratio, 0.25).

% --- Constraint claim ---
narrative_ontology:constraint_claim(guinea_worm_eradication, rope).
narrative_ontology:human_readable(guinea_worm_eradication, "Global Guinea Worm Eradication Program").
narrative_ontology:topic_domain(guinea_worm_eradication, "public_health/social").

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(guinea_worm_eradication, endemic_populations).
narrative_ontology:constraint_beneficiary(guinea_worm_eradication, global_health_community).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ENDEMIC COMMUNITY MEMBER (ROPE) — Access to safe water and health education are coordinated public goods with minimal coercive overhead. Community participation is voluntary; the constraint enforces collective water access, not extraction from individuals. Beneficiary experiences the program as coordination enabling health improvement.
constraint_indexing:constraint_classification(guinea_worm_eradication, rope,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(regional))).

% PERSPECTIVE 2: THE CARTER CENTER & GLOBAL HEALTH INSTITUTIONS (ROPE) — Pure coordination mechanism. Carter Center and partner organizations coordinate technical expertise, funding, and local implementation. No extraction from partner health systems — all parties benefit from disease elimination. Coordination function is primary; coercive capacity is minimal.
constraint_indexing:constraint_classification(guinea_worm_eradication, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: NATIONAL HEALTH MINISTRIES (ROPE) — Organized actors integrating Guinea worm surveillance into existing health systems. Cooperation is structured but voluntary. Ministries benefit from disease elimination and international partnership. No asymmetric extraction — mutual gain in surveillance capacity and public health infrastructure.
constraint_indexing:constraint_classification(guinea_worm_eradication, rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 4: ANALYTICAL OBSERVER / ERADICATION TRAJECTORY (SCAFFOLD) — From a civilizational view, the eradication program is a temporary coordination structure with a genuine sunset clause: elimination of the parasite eliminates the need for the program. As of 2024, only 13 cases reported globally (down from millions in 1986). The constraint transforms from active coordination (rope) to vestigial surveillance as case count approaches zero. Theater content increases as verification shifts from active case-finding to confirming absence — the final phase emphasizes proof of eradication.
constraint_indexing:constraint_classification(guinea_worm_eradication, scaffold,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(guinea_worm_eradication_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(guinea_worm_eradication, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(guinea_worm_eradication, TypeOther, context(agent_power(analytical), _, _, _)),
    TypePowerless \= TypeOther.

:- end_tests(guinea_worm_eradication_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.12): Very low, declining over time. The program creates coordinated public goods (safe water, health education) from which endemic populations benefit directly. Carter Center and partners extract minimal value — all parties benefit from disease elimination. The program funds water infrastructure and local health worker training, both of which benefit communities beyond the eradication goal. Suppression (0.08): Very low. Community participation is voluntary; health education is accessible; safe water is provided at no cost to end users. Coercive capacity is minimal — the program succeeds through persuasion and infrastructure provision, not enforcement. Theater ratio (0.25): Low, increasing toward end of interval. Early phases focus on active case detection and water source improvement — high functional content. As cases decline, verification and proof of absence increase; theater rises as surveillance becomes the primary activity relative to actual disease elimination. Claimed type (Rope): Pure coordination with no asymmetric extraction.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap here is minimal — all observers classify this as rope or scaffold (coordination-dominant). Endemic communities experience it as rope (pure coordination enabling health). Global institutions experience it as rope (coordination producing shared benefit). Health ministries experience it as rope (integration of coordinated surveillance). The analytical observer sees it as scaffold (temporary coordination with sunset). No perspective produces a snare or tangled rope classification because the extraction mechanism is absent — Carter Center and partners do not extract from communities or from health systems. The constraint is structurally egalitarian: it solves a coordination problem without creating asymmetric extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values for all perspectives are low (0.0–0.30 range), reflecting that this is a beneficiary-dominant structure with minimal or zero victims. Endemic populations are direct beneficiaries with high exit (mobile — can choose to use safe water or not); d ≈ 0.20. Carter Center and partners are beneficiaries with arbitrage exits (can reallocate resources); d ≈ 0.05. Health ministries are beneficiary-partners with mobile exits (can integrate or disengage from surveillance); d ≈ 0.25. All perspectives derive from beneficiary status combined with access to alternatives. There is no victim group — no agent bears concentrated costs while others extract. The constraint is Pareto-improving: it reduces human suffering without creating winners and losers.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by demonstrating that pure coordination without extraction is possible and empirically sustainable. The threat of false mandatrophy would arise if an observer tried to classify the program as a snare (extraction mechanism) on the basis of Carter Center's institutional power and funding control. However, the structural test is clear: Carter Center does not extract value from endemic populations; it transfers resources to them. The program's sunset clause is genuine — eradication of the parasite eliminates the need for the program. As case counts approach zero, the constraint transitions from active coordination (rope) to verification-only surveillance (scaffold with rising theater). The mandatrophy is resolved by noting that coordination is primary and extraction is absent, so no tension between claimed type and structural data exists.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    zoonotic_transmission_risk,
    'Can Guinea worm transmission persist or re-emerge via animal reservoirs (dogs, other mammals) despite human eradication protocols?',
    'Long-term epidemiological surveillance post-eradication; genetic analysis of parasite strains in animal populations; monitoring of infection pathways in regions where human transmission has been eliminated',
    'If animal reservoir confirmed: eradication may be impossible; constraint becomes permanent (Rope → Piton). If animal transmission ruled out: true eradication is achievable; scaffold sunset is real.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(zoonotic_transmission_risk, empirical, 'Whether Guinea worm can persist in animal reservoirs').

omega_variable(
    sustained_water_infrastructure,
    'Will safe water infrastructure installed by the eradication program be maintained after Carter Center support ends?',
    'Post-program monitoring of well functionality and maintenance in target regions; assessment of local funding and governance capacity; tracking of water-borne disease resurgence in regions where infrastructure decays',
    'If infrastructure maintained: scaffold sunset is real and benefits persist. If infrastructure fails: the constraint becomes cyclic — periodic re-emergence requiring repeated intervention cycles. Program effectiveness degraded to theater.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sustained_water_infrastructure, empirical, 'Sustainability of water infrastructure post-program').

omega_variable(
    case_verification_bias,
    'As case counts approach zero, does increased emphasis on detecting final cases create reporting incentives that inflate official case counts or misclassify similar diseases as Guinea worm?',
    'Independent verification of suspected cases; genetic testing of parasite samples; comparison of reported vs confirmed case counts; analysis of case detection incentives in surveillance protocols',
    'If verification bias present: final eradication threshold becomes ambiguous; constraint persists as theater-heavy surveillance. If verification robust: sunset timing is clear and schedule-driven.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(case_verification_bias, empirical, 'Whether case verification exhibits reporting or incentive bias near eradication').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(guinea_worm_eradication, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gworm_tr_t0, guinea_worm_eradication, theater_ratio, 0, 0.1).
narrative_ontology:measurement(gworm_tr_t20, guinea_worm_eradication, theater_ratio, 20, 0.18).
narrative_ontology:measurement(gworm_tr_t40, guinea_worm_eradication, theater_ratio, 40, 0.25).

% Extraction over time
narrative_ontology:measurement(gworm_be_t0, guinea_worm_eradication, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(gworm_be_t20, guinea_worm_eradication, base_extractiveness, 20, 0.18).
narrative_ontology:measurement(gworm_be_t40, guinea_worm_eradication, base_extractiveness, 40, 0.12).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(guinea_worm_eradication, resource_allocation).
narrative_ontology:affects_constraint(guinea_worm_eradication, safe_water_access).
narrative_ontology:affects_constraint(guinea_worm_eradication, health_surveillance_infrastructure).

% DUAL FORMULATION NOTE:
% Guinea worm eradication is downstream of safe water access infrastructure (which has independent constraints and extractiveness values) and feeds into health surveillance capabilities (which persists post-eradication). The family decomposition treats eradication as a coordination mechanism enabling deployment of the upstream constraint (water infrastructure) and creating capacity for downstream constraint (surveillance systems).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
