% ============================================================================
% CONSTRAINT STORY: education_credentialism_spiral
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_education_credentialism_spiral, []).

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
 *   constraint_id: education_credentialism_spiral
 *   human_readable: Education Credentialism Spiral
 *   domain: economic/social/institutional
 *
 * SUMMARY:
 *   The education credentialism spiral is a self-reinforcing constraint where
 *   credential requirements escalate across time, trapping successive cohorts
 *   in increasingly expensive and extended educational pathways. The
 *   constraint exhibits genuine coordination function (employers use
 *   credentials to verify capability, students use education to acquire
 *   specialized knowledge and networks) while simultaneously operating as an
 *   asymmetric extraction mechanism (credential suppliers capture rents
 *   through positional scarcity, credential inflation forces continuous
 *   re-certification, and low-income workers bear disproportionate costs
 *   through debt servicing). The spiral emerges from rational individual
 *   choices (employers screen with credentials because alternatives are
 *   expensive; students pursue credentials because employers require them)
 *   that collectively create a pathological cycle: each cohort's credential
 *   acquisition raises the baseline for the next cohort, creating permanent
 *   credential-requirement inflation with no equilibrium endpoint. Theater
 *   ratio (0.68) reflects that much educational activity is ritualistic
 *   completion rather than functional skill acquisition — students pursue
 *   advanced degrees partly to signal effort/conformity rather than to
 *   acquire capabilities that employers empirically require. The constraint's
 *   key extractive mechanism is credential inflation itself: the gap between
 *   credentials actually required for job performance and credentials
 *   nominally required for hiring has widened over 40 years, suggesting that
 *   the coordination function (capability verification) has degraded while
 *   the extraction function (rent capture through scarcity) has intensified.
 *
 * KEY AGENTS:
 *   - Low-Income Students: Primary victims (powerless/trapped) — trapped in credential requirements with no exit; bear maximum extraction through debt servicing and opportunity cost
 *   - Working-Class Families: Secondary victims (moderate/constrained) — face rising credential costs and constrained exit options; career pathways require 16+ years and $100k+ investment
 *   - Educational Institutions: Primary beneficiaries (institutional/arbitrage) — capture tuition rents, network positional rents, and credential-scarcity rents; design credential requirements
 *   - Credential Gatekeepers: Secondary beneficiaries (powerful/mobile) — employers, professional associations, licensing boards use credentials to maintain labor-market control and wage compression
 *   - Elite Universities: Powerful institutional actor (powerful/mobile) — coordinate genuine knowledge access and selective networks while extracting premium tuition through credential positional scarcity
 *   - Community Colleges: Degraded institutional actor (institutional/constrained) — originally open-access coordination mechanism, now operates primarily as theatrical credential-transfer system with limited labor-market value
 *   - Labor Market Coordination System: Institutional actor (analytical/analytical) — aggregate coordination system using credentials for information signaling while experiencing systemic credential inflation that decouples from actual labor-market capability requirements
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(education_credentialism_spiral, 0.58).
domain_priors:suppression_score(education_credentialism_spiral, 0.65).
domain_priors:theater_ratio(education_credentialism_spiral, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(education_credentialism_spiral, extractiveness, 0.58).
narrative_ontology:constraint_metric(education_credentialism_spiral, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(education_credentialism_spiral, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(education_credentialism_spiral, tangled_rope).
narrative_ontology:human_readable(education_credentialism_spiral, "Education Credentialism Spiral").
narrative_ontology:topic_domain(education_credentialism_spiral, "economic/social/institutional").

domain_priors:requires_active_enforcement(education_credentialism_spiral).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(education_credentialism_spiral, educational_institutions).
narrative_ontology:constraint_beneficiary(education_credentialism_spiral, credential_gatekeepers).
narrative_ontology:constraint_victim(education_credentialism_spiral, low_income_students).
narrative_ontology:constraint_victim(education_credentialism_spiral, labor_market_competitiveness).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: LOW-INCOME STUDENT (SNARE) — Trapped in credential requirements with no exit. Must acquire increasingly expensive degrees to access employment pathways that previously required high school completion. Suppression is structural: unable to secure employment without credentials, unable to afford credentials without debt/family wealth. Bears maximum extraction cost.
constraint_indexing:constraint_classification(education_credentialism_spiral, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: WORKING-CLASS FAMILY (SNARE) — Constrained by rising credential costs and debt burden. Educational pathway to mobility now requires 16+ years of investment and $100k+ in costs. Exit options exist (trade skills, apprenticeships) but are heavily stigmatized and systematically underfunded relative to credential pathways. High extraction through debt servicing and opportunity cost.
constraint_indexing:constraint_classification(education_credentialism_spiral, snare,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: ELITE UNIVERSITY (TANGLED ROPE) — Genuinely coordinates access to selective networks and specialized knowledge (coordination function), while simultaneously extracting premium tuition and capturing positional rents through credential scarcity. Powerful institutional actor with mobility — can decline admission or change admission criteria, but benefits from credential scarcity maintenance.
constraint_indexing:constraint_classification(education_credentialism_spiral, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 4: EMPLOYER COALITION (ROPE) — Uses credentials as coordination mechanism for labor screening and hierarchical sorting. Captures value from credential-based wage compression (paying less for formalized qualifications than for actual performance). High-degree institutions benefit from this arrangement through sustained tuition premium. Net beneficiary.
constraint_indexing:constraint_classification(education_credentialism_spiral, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: COMMUNITY COLLEGE SECTOR (PITON) — Originally designed as open-access coordination mechanism for skill development and workforce adaptation. Now performs primarily theatrical function: transfers credits that are not recognized by four-year institutions, certifies completion without labor market advantage, maintains appearance of mobility pathway. Theater ratio (0.68) reflects that much community college activity is ritualistic completion rather than functional skill transfer. Institutional inertia maintains the structure despite degraded function.
constraint_indexing:constraint_classification(education_credentialism_spiral, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: LABOR MARKET COORDINATION SYSTEM (TANGLED ROPE) — Credentials genuinely coordinate information about labor-market-relevant capability when information asymmetry is severe (what does an applicant know?). Simultaneously, credential inflation extracts value by forcing credential renewal cycles: skills that required certification five years ago now require advanced certification; positions that required bachelor's degrees now require master's degrees. The coordination function is real but embedded within asymmetric extraction that operates through credential inflation rather than wage capture.
constraint_indexing:constraint_classification(education_credentialism_spiral, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (TANGLED ROPE) — From civilizational/global perspective, the credentialism spiral exhibits genuine coordination function (skill verification, knowledge transfer, network access) embedded within asymmetric extraction (credential inflation, rent capture, positional scarcity maintenance). The spiral structure itself is the extraction mechanism: each cohort's credential requirements exceed the previous cohort's, creating continuous demand for credential acquisition with no equilibrium endpoint. This is an engineered Tangled Rope: the coordination benefit is real but constrained by design to maintain scarcity rents.
constraint_indexing:constraint_classification(education_credentialism_spiral, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(education_credentialism_spiral_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(education_credentialism_spiral, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(education_credentialism_spiral, TypeOther, context(agent_power(powerful), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(education_credentialism_spiral, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(education_credentialism_spiral, TR),
    TR >= 0.70.

:- end_tests(education_credentialism_spiral_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. The constraint exhibits significant asymmetric extraction: low-income workers bear disproportionate costs (debt, opportunity cost, extended educational timeline) while credential gatekeepers capture rents through maintained scarcity. However, extraction is not maximal (0.58 not 0.75+) because genuine coordination function persists — education does transfer knowledge and networks, not just credentials. Suppression (0.65): High. Structural barriers to exit include: debt burden ($30k-$200k depending on institution type), opportunity cost (4-8 years of forgone wages), labor-market discrimination against non-credentialed workers, and systematic underfunding of alternative pathways (apprenticeships, bootcamps, portfolio-based hiring). Theater ratio (0.68): High. Community colleges operate primarily as credential-transfer systems despite limited labor-market value; much upper-level coursework involves ritualistic completion rather than capability development; credential requirements have inflated beyond demonstrated job-performance requirements. Measurements show progressive extraction intensification: extractiveness grew from 0.35 (40 years ago) to 0.58 (present), and theater ratio from 0.45 to 0.68. This pattern indicates: (1) credential requirements are escalating faster than actual labor-market capability demands, and (2) educational activity is becoming increasingly performative/ritualistic rather than functionally skill-producing.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates dramatic perspectival divergence. Low-income students and working-class families see a Snare (pure extraction with minimal coordination benefit from their perspective) — they bear maximum costs while access to the coordination benefits (elite networks, specialized knowledge) is stratified by wealth. Elite universities see a Tangled Rope or Rope (genuine coordination function for knowledge/network access, balanced with reasonable extraction through tuition). Employers see pure Rope (credential coordination solves their screening problem with minimal overhead — workers bear the cost, employers reap the screening benefit). Community colleges see themselves as Rope (open-access coordination) but the analytical observer sees Piton (degraded ritual maintaining appearance of function). The analytical observer at civilizational timescale sees Tangled Rope — the coordination benefits are genuine but embedded in an extraction mechanism (credential inflation) that operates through engineered scarcity rather than through direct value capture. The gap between low-income student perspective (Snare) and elite institution perspective (Rope/Tangled Rope) is maximal: the same structural constraint appears as pure extraction to one agent and balanced coordination to another.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) for each perspective is derived from the agent's power level, exit options, and structural relationship to the extraction flow. Low-income students: powerless power + trapped exit + victim status → d ≈ 0.95 → maximum f(d) ≈ 1.42 → maximum experienced extractiveness. Working-class families: moderate power + constrained exit + victim status → d ≈ 0.68 → high f(d) ≈ 1.05 → high experienced extractiveness. Elite universities: powerful power + mobile exit + beneficiary status → d ≈ 0.35 → moderate f(d) ≈ 0.32 → low/moderate experienced extractiveness. Employers: institutional power + arbitrage exit + beneficiary status → d ≈ 0.10 → low f(d) ≈ 0.01 → minimal experienced extractiveness. Community colleges: institutional power + constrained exit + mixed status → d ≈ 0.55 → moderate f(d) ≈ 0.75 → moderate experienced extractiveness. Labor market system: analytical power + analytical exit + mixed coordination/extraction → d ≈ 0.72 → high f(d) ≈ 1.15 → complex relationship to extraction mechanism. Scope modifier: national scope (σ=1.0) means no significant amplification/dampening of χ via scope factor.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by recognizing that credential inflation operates as a Tangled Rope specifically: genuine coordination function (knowledge transfer, capability signaling, network access) is embedded within an asymmetric extraction mechanism (credential-requirement inflation that exceeds labor-market capability demands). The coordination function cannot be separated from the extraction mechanism — they are engineered as a coupled system. Credential scarcity is maintained not through genuine scarcity in capability supply (education is increasingly abundant) but through positional/credential scarcity (the signal value of credentials is maintained through requirement inflation). This is distinct from a pure Rope (where coordination occurs with minimal extraction) because the extraction mechanism (credential inflation) is not a side effect but a designed feature that sustains the coordination benefit. It is distinct from a pure Snare because the coordination function is genuinely valuable and cannot be replicated through alternative mechanisms — employers cannot costlessly verify capability without some signaling mechanism, and students cannot costlessly access specialized knowledge without educational institutions. The Tangled Rope classification resolves the analytical tension: the constraint is neither purely extractive nor purely coordinative, but a hybrid where the extraction mechanism is embedded within the coordination architecture and both functions operate simultaneously.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    credential_inflation_causality,
    'Is credential inflation driven by employer quality signaling demands or by credential supplier market power?',
    'Longitudinal analysis of job postings and actual job requirements; comparison of credential requirements against empirical skill demands; analysis of employer behavior when credential supply expands',
    'If driven by employer demand: Rope-type coordination problem requiring signal refinement. If driven by supplier power: Snare-type extraction requiring constraint restructuring.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(credential_inflation_causality, empirical, 'Whether credential inflation reflects genuine quality demands or supplier rent-seeking').

omega_variable(
    alternative_credentialing_viability,
    'Do alternative credentialing systems (industry certifications, bootcamps, apprenticeships, portfolio-based hiring) provide functional substitutes for degree-based credentials?',
    'Comparative salary/career-trajectory analysis of alternative-credentialed workers vs degree-credentialed workers controlling for socioeconomic background; employer hiring pattern analysis across credential types; longitudinal tracking of alternative-credential cohorts',
    'If viable: suppression (0.65) is overstated — exit options exist beyond traditional credentials. If not viable: suppression confirmed — alternatives are systematically underfunded/stigmatized, trapping workers in degree pathways.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_credentialing_viability, empirical, 'Whether alternative credentialing provides functional labor-market alternatives').

omega_variable(
    spiral_equilibrium_stability,
    'Is the credentialism spiral self-limiting (approaching stable higher-credential baseline) or pathological (continuing to inflate indefinitely)?',
    'Time-series analysis of credential requirements against labor-productivity measures; identification of saturation thresholds (percentage of workforce with bachelor''s/master''s degrees); analysis of whether continued credential inflation produces measurable productivity gains',
    'If self-limiting: constraint will moderate (Tangled Rope → Rope transition). If pathological: extraction mechanisms are decoupled from coordination function and spiral is pure extraction (classification collapse toward Snare).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(spiral_equilibrium_stability, empirical, 'Whether credential inflation asymptotically approaches equilibrium or continues pathological expansion').

omega_variable(
    suppression_mechanism_internalization,
    'Is suppression (0.65) primarily structural (debt, opportunity cost) or internalized (belief that credentials are necessary for identity/worth)?',
    'Qualitative analysis of student decision-making narratives; comparison of credential pursuit rates across socioeconomic strata when structural barriers are removed (wealthy families); longitudinal tracking of credential pursuit post-debt-relief; identity-anchoring analysis through educational choice research',
    'If structural: barriers can be removed through policy (debt forgiveness, free college, alternative pathways). If internalized: constraint persists even after structural barriers removed because agents'' identity is constituted through credential pursuit.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalization, empirical, 'Whether suppression mechanism is structural or internalized through identity').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(education_credentialism_spiral, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(edcred_tr_t0, education_credentialism_spiral, theater_ratio, 0, 0.45).
narrative_ontology:measurement(edcred_tr_t20, education_credentialism_spiral, theater_ratio, 20, 0.62).
narrative_ontology:measurement(edcred_tr_t40, education_credentialism_spiral, theater_ratio, 40, 0.68).

% Extraction over time
narrative_ontology:measurement(edcred_be_t0, education_credentialism_spiral, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(edcred_be_t20, education_credentialism_spiral, base_extractiveness, 20, 0.48).
narrative_ontology:measurement(edcred_be_t40, education_credentialism_spiral, base_extractiveness, 40, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(education_credentialism_spiral, resource_allocation).
narrative_ontology:affects_constraint(education_credentialism_spiral, student_debt_trap).
narrative_ontology:affects_constraint(education_credentialism_spiral, labor_market_stratification).
narrative_ontology:affects_constraint(education_credentialism_spiral, institutional_wage_compression).

% DUAL FORMULATION NOTE:
% The education credentialism spiral decomposes into multiple structurally distinct constraints: (1) credential-requirement inflation (this story, ε=0.58, Tangled Rope) — the mechanism by which credential baselines escalate; (2) student debt accumulation (downstream, ε=0.72, Snare) — the extraction mechanism specific to financing credential acquisition; (3) labor market stratification by educational credential (downstream, ε=0.55, Tangled Rope) — the mechanism by which credentials translate into wage/opportunity asymmetry. This story focuses on the spiral mechanism itself; downstream stories address the debt and stratification mechanisms separately.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(education_credentialism_spiral, institutional, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
