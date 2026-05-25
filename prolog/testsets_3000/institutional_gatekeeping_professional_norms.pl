% ============================================================================
% CONSTRAINT STORY: institutional_gatekeeping_professional_norms
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_institutional_gatekeeping_professional_norms, []).

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
 *   constraint_id: institutional_gatekeeping_professional_norms
 *   human_readable: Institutional Gatekeeping Through Professional Norms
 *   domain: institutional/organizational/social
 *
 * SUMMARY:
 *   Institutional gatekeeping through professional norms creates a structural
 *   constraint where formal credentials, licensure requirements, and
 *   professional association membership requirements regulate access to
 *   knowledge work and professional status. The constraint simultaneously
 *   coordinates legitimate quality assurance and expertise validation while
 *   extracting rents from outsiders and locking in benefits for established
 *   professionals. From the credentialing institution's perspective,
 *   gatekeeping solves a coordination problem: how do you signal competence
 *   in complex domains without some standardized verification? From the
 *   excluded outsider's perspective, gatekeeping is pure extraction: they are
 *   trapped outside the system with no mechanism to prove competence. The
 *   constraint demonstrates how the same institutional mechanism can be rope
 *   (coordination), snare (extraction), tangled rope (mixed), or piton
 *   (degraded ritual) depending on structural position. Theater ratio (0.68)
 *   reflects the rising performative content of credentialing — degree
 *   prestige, certification rituals, journal impact factors — that
 *   increasingly decouples from actual competence signaling, particularly in
 *   high-velocity domains like software engineering and data science where
 *   self-taught practitioners and alternative credentials now demonstrate
 *   comparable outcomes.
 *
 * KEY AGENTS:
 *   - Established Professionals: Primary beneficiary (powerful/mobile) — gatekeeping reduces competition, validates their status, and provides institutional backing for their authority claims
 *   - Credentialing Institutions: Primary beneficiary (institutional/arbitrage) — universities, licensing boards, professional associations control market access and derive legitimacy, funding, and power from gatekeeping function
 *   - Outsiders and Newcomers: Primary victim (powerless/trapped) — individuals without formal credentials face insurmountable barriers to professional recognition; no alternative pathways available at scale
 *   - Alternative Knowledge Systems: Secondary victim (moderate/constrained) — self-taught practitioners, apprenticeship models, community-of-practice learning face marginalization despite demonstrated competence
 *   - Early-Career Professionals: Secondary victim (moderate/constrained) — must accumulate costly credentials and navigate gatekeeping hierarchy despite often already possessing required competence
 *   - Alternative Credentialing Movement: Organized challengers (organized/constrained) — online platforms, portfolio-based hiring, open-source communities building parallel verification pathways with sunset trajectory
 *   - Analytical Observer: Risk of false naturalization (analytical/analytical) — tendency to treat contingent institutional arrangements as inherent to expertise coordination
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(institutional_gatekeeping_professional_norms, 0.52).
domain_priors:suppression_score(institutional_gatekeeping_professional_norms, 0.65).
domain_priors:theater_ratio(institutional_gatekeeping_professional_norms, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(institutional_gatekeeping_professional_norms, extractiveness, 0.52).
narrative_ontology:constraint_metric(institutional_gatekeeping_professional_norms, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(institutional_gatekeeping_professional_norms, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(institutional_gatekeeping_professional_norms, tangled_rope).
narrative_ontology:human_readable(institutional_gatekeeping_professional_norms, "Institutional Gatekeeping Through Professional Norms").
narrative_ontology:topic_domain(institutional_gatekeeping_professional_norms, "institutional/organizational/social").

domain_priors:requires_active_enforcement(institutional_gatekeeping_professional_norms).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(institutional_gatekeeping_professional_norms, established_professionals).
narrative_ontology:constraint_beneficiary(institutional_gatekeeping_professional_norms, credentialing_institutions).
narrative_ontology:constraint_victim(institutional_gatekeeping_professional_norms, outsiders_and_newcomers).
narrative_ontology:constraint_victim(institutional_gatekeeping_professional_norms, alternative_knowledge_systems).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: EXCLUDED OUTSIDER (SNARE) — Individuals lacking formal credentials or institutional affiliation face insurmountable barriers to professional recognition. Cannot obtain work opportunities, publication venues, or authority claims without passing gatekeeping tests. No exit available — the norm creates structural closure.
constraint_indexing:constraint_classification(institutional_gatekeeping_professional_norms, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: EARLY-CAREER PROFESSIONAL (TANGLED ROPE) — Experiences the norm as both coordination mechanism (standardizes expectations, enables collaboration) and extraction mechanism (requires costly credential accumulation, creates vulnerability to gatekeepers). Constrained by career path dependence but sees some benefit in knowing the rules.
constraint_indexing:constraint_classification(institutional_gatekeeping_professional_norms, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: CREDENTIALING INSTITUTION (ROPE) — Universities, licensing boards, and professional associations benefit from gatekeeping enforcement but genuinely provide coordination: they standardize training, validate competence, and enable market signaling. Exit options abundant — can ignore norms but choose not to because the coordination function is real.
constraint_indexing:constraint_classification(institutional_gatekeeping_professional_norms, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: ESTABLISHED PROFESSIONAL BENEFICIARY (TANGLED ROPE) — Credentialed professionals benefit from gatekeeping (reduces competition, validates their status) but also face genuine coordination requirements (continuing education, ethical standards, quality assurance). Mobile enough to exit but gatekeeping benefits exceed costs; extraction runs toward them.
constraint_indexing:constraint_classification(institutional_gatekeeping_professional_norms, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: CEREMONIAL CREDENTIALING RITUAL (PITON) — Significant portion of professional gatekeeping is theatrical performance — degree prestige, certification ceremonies, journal impact factors — that no longer correlates with actual competence in high-velocity domains (software engineering, data science). Theater ratio elevated (0.68) because ritualized gatekeeping persists despite degraded signaling function; maintained by institutional inertia.
constraint_indexing:constraint_classification(institutional_gatekeeping_professional_norms, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ALTERNATIVE CREDENTIALING MOVEMENT (SCAFFOLD) — Online platforms, portfolio-based hiring, competency-based assessments, and open-source contribution records represent sunset mechanisms for traditional gatekeeping. Organized agents (tech companies, online academies, communities of practice) building parallel verification pathways. As these mature, traditional gatekeeping loses extraction force — sunset clause is structural.
constraint_indexing:constraint_classification(institutional_gatekeeping_professional_norms, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW (MOUNTAIN) — Risk of naturalizing gatekeeping as inherent to any complex profession: 'Specialization requires standards; standards require gatekeeping; gatekeeping requires exclusion.' This framing treats contingent institutional arrangements as immutable laws. Engine false summit detection reveals this as rationalization rather than necessity.
constraint_indexing:constraint_classification(institutional_gatekeeping_professional_norms, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(institutional_gatekeeping_professional_norms_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(institutional_gatekeeping_professional_norms, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(institutional_gatekeeping_professional_norms, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(institutional_gatekeeping_professional_norms, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(institutional_gatekeeping_professional_norms, TR),
    TR >= 0.70.

:- end_tests(institutional_gatekeeping_professional_norms_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high, reflecting genuine coordination function overlaid with significant rent extraction. Traditional gatekeeping provides real value — standardized training, competence validation, quality assurance — but also extracts significant rents from outsiders and creates artificial scarcity. The value has risen from 0.38 to 0.52 over the interval as performance-decoupling has increased (credentials increasingly performative while actual skill verification declines). Suppression (0.65): High. Barriers to entry include: high credential costs (tuition, time, exam fees), tacit knowledge requirements (apprenticeship dependencies), institutional closure (insiders control hiring), legal enforcement (licensure requirements in regulated professions), and information asymmetries (outsiders cannot credibly signal competence). Suppression is enforced both structurally (legal barriers in medicine, law, engineering) and culturally (professional norms against hiring non-credentialed individuals). Theater ratio (0.68): High, reflecting substantial performative content — degree prestige derives from institutional reputation rather than curriculum quality; certification rituals serve identity affiliation rather than competence verification; journal impact factors signal publication venue status rather than research quality. Theater has increased (from 0.42 to 0.68) as credentialing has become increasingly ritualistic relative to its signaling function, particularly in fast-moving domains where credentials lag actual skill requirements.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap between excluded outsiders (snare) and credentialing institutions (rope) reveals the constraint's core contradiction: the same institutional mechanism that coordinates expertise validation also prevents alternative paths to competence demonstration. The gap between established professionals (beneficiary with moderate extraction) and early-career professionals (victim with high extraction) reveals that gatekeeping concentration increases over time — first-wave entrants enjoy benefits while later cohorts bear accumulating costs. The gap between ceremonial gatekeeping (piton) and alternative credentialing movement (scaffold) reveals that the constraint's function has degraded while its form persists — the theater persists through institutional inertia rather than coordination necessity. The mountain perspective represents the most dangerous gap: the risk that analytical observers naturalize contingent institutional arrangements as inherent to expertise itself.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) is determined by each agent's structural position relative to extraction flow. Excluded outsiders have d ≈ 0.95 (trapped victims, maximum extraction experienced). Credentialing institutions have d ≈ 0.10 (beneficiaries with arbitrage options, negative effective extraction). Established professionals have d ≈ 0.30 (beneficiaries but some genuine participation in coordination function). Early-career professionals have d ≈ 0.70 (victims with constrained exit, high extraction cost). Alternative credential movement has d ≈ 0.55 (organized agents with external leverage, moderate experienced extraction). Each index's experienced extractiveness (chi) is computed from base extraction (0.52), directionality function f(d), and scope modifier. Global scope (σ=1.2) amplifies chi for institutional contexts; regional scope (σ=0.9) dampens it. The formula χ = ε × f(d) × σ(S) produces differentiated experienced extraction across perspectives, explaining why the constraint appears as snare from one position but rope from another despite identical base properties.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by decomposing gatekeeping into three structurally distinct functions: (1) Competence coordination (genuine rope function) — standardized training, verification mechanisms, quality assurance; (2) Rent extraction (snare function) — limiting access to create artificial scarcity, extracting economic value from outsiders; (3) Identity and status maintenance (piton function) — ceremonial rituals that serve professional identity and institutional legitimacy rather than competence verification. The tangled rope classification at moderate extraction (0.52) captures the hybrid accurately: the constraint simultaneously provides coordination benefits (credentialing institutions genuinely solve a market signaling problem) and enables extraction (established professionals capture rents through gatekeeping). The rising theater ratio (0.42 → 0.68) indicates that the piton component is growing relative to the coordination component — the ceremonial rituals persist despite degraded signaling function. The scaffold perspective (alternative credentialing) provides the structural check on false naturalization: if gatekeeping were inherent to expertise coordination (mountain view), alternative pathways would fail; if gatekeeping is contingent institutional arrangement (tangled rope view), alternative pathways should succeed. The empirical question (omega variable) of whether alternative credentials achieve comparable market acceptance determines whether the constraint is genuinely immutable coordination requirement or contingent institutional arrangement masquerading as natural law.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    competence_signal_degradation,
    'Have traditional credentials decoupled from actual competence in high-velocity domains?',
    'Longitudinal tracking of credentialed vs non-credentialed professional performance; analysis of job outcomes, project success rates, and employer satisfaction across domains and credential types',
    'If decoupled: extractiveness increases toward 0.65+ (pure rent-seeking). If correlated: extractiveness remains around 0.52 (coordination with extraction overlay).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(competence_signal_degradation, empirical, 'Correlation between credentials and actual competence outcomes').

omega_variable(
    alternative_credential_market_penetration,
    'What percentage of hiring decisions in each sector now accept alternative credentials, portfolio evidence, or competency-based assessment?',
    'Sector-by-sector analysis of hiring criteria; tracking adoption of alternative credentialing pathways in tech, finance, healthcare, and trades over time',
    'If > 40% acceptance: scaffold sunset is structural and accelerating. If < 20% acceptance: traditional gatekeeping remains dominant extraction mechanism with limited exit alternatives.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_credential_market_penetration, empirical, 'Market penetration of alternative credentials by sector').

omega_variable(
    identity_lock_mechanism_strength,
    'To what extent is gatekeeping perpetuated by professionals'' identity fusion with their credentials?',
    'Qualitative analysis of professional identity narratives; comparison of willingness to accept alternative pathways among credentialed vs non-credentialed professionals; post-retirement shifts in gatekeeping attitudes',
    'If strong identity lock: established professionals will resist alternative pathways despite rational incentives to accept them (pushes toward snare classification for gatekeeping institution). If weak: institutional change is primarily structural, not psychological.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_mechanism_strength, conceptual, 'Professional identity fusion with credential status').

omega_variable(
    collective_action_problem_resolution,
    'Can outsider coalitions (alternative credential providers, hiring reformers, professional norm challengers) solve the coordination problem without traditional gatekeeping?',
    'Case studies of sectors where alternative credentialing succeeded; analysis of what made coordination possible without gatekeeping; identification of failure modes in attempted alternatives',
    'If yes: scaffold sunset is real and inevitable. If no: gatekeeping is a necessary coordination cost and the constraint is legitimately rope rather than tangled_rope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(collective_action_problem_resolution, empirical, 'Whether alternative credentialing can solve coordination without gatekeeping').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(institutional_gatekeeping_professional_norms, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(igpn_tr_t0, institutional_gatekeeping_professional_norms, theater_ratio, 0, 0.42).
narrative_ontology:measurement(igpn_tr_t10, institutional_gatekeeping_professional_norms, theater_ratio, 10, 0.58).
narrative_ontology:measurement(igpn_tr_t20, institutional_gatekeeping_professional_norms, theater_ratio, 20, 0.68).

% Extraction over time
narrative_ontology:measurement(igpn_be_t0, institutional_gatekeeping_professional_norms, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(igpn_be_t10, institutional_gatekeeping_professional_norms, base_extractiveness, 10, 0.45).
narrative_ontology:measurement(igpn_be_t20, institutional_gatekeeping_professional_norms, base_extractiveness, 20, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(institutional_gatekeeping_professional_norms, identity_coordination).
narrative_ontology:affects_constraint(institutional_gatekeeping_professional_norms, professional_licensing_cartels).
narrative_ontology:affects_constraint(institutional_gatekeeping_professional_norms, credentialing_cost_accumulation).
narrative_ontology:affects_constraint(institutional_gatekeeping_professional_norms, identity_lock_through_credentials).

% DUAL FORMULATION NOTE:
% Institutional gatekeeping decomposes into at least three distinct constraints: (1) competence_coordination_standard (ε≈0.15, Mountain/Rope) — the genuine problem of signaling expertise; (2) rent_extraction_via_scarcity (ε≈0.65, Snare) — artificial barriers to entry; (3) ceremonial_ritual_persistence (ε≈0.40, Piton) — performative credentialing that serves identity rather than function. This story captures the integrated phenomenon; specialized stories address each structural function separately.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(institutional_gatekeeping_professional_norms, institutional, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
