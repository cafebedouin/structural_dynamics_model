% ============================================================================
% CONSTRAINT STORY: digital_literacy_inequality
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_digital_literacy_inequality, []).

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
 *   constraint_id: digital_literacy_inequality
 *   human_readable: Digital Literacy Inequality and Economic Exclusion
 *   domain: economic/social/technological
 *
 * SUMMARY:
 *   Digital literacy inequality creates a structural extraction mechanism
 *   through which economic and institutional systems increasingly mandate
 *   digital competency as gatekeeping requirement. The constraint operates
 *   across multiple levels: individual employment barriers, household
 *   financial service access (banking, insurance, credit), governmental
 *   service access (tax, benefits, licensing), and educational/professional
 *   credentialing. Unlike historical literacy inequality, digital literacy
 *   inequality is compounded by rapid technological change, algorithm
 *   opacity, and platform monopolization that limits exit options. The
 *   low-literacy populations experiencing maximum extraction
 *   (powerless/trapped) cannot avoid the constraint — it is embedded in
 *   essential services — but also cannot easily acquire the skills to
 *   overcome it due to suppression (cost of training, time poverty,
 *   age-cohort barriers, identity-locked self-concepts). The constraint has
 *   intensified over the measurement interval (ε 0.35→0.58) as platforms have
 *   become more monopolistic and digital-first service delivery has expanded.
 *   Theater ratio remains relatively low (0.48), indicating that the
 *   extraction is only partially obscured by performative bridge programs —
 *   the constraint is relatively visible in wage gaps, access denial, and
 *   service friction.
 *
 * KEY AGENTS:
 *   - Low-literacy populations (powerless/trapped): Primary victims — employed in jobs requiring increasing digital skills; excluded from better-paying digital-native roles; dependent on platform intermediaries for service access
 *   - Elderly adults (powerless/constrained): Secondary victims — aging out of workforce due to digital skill requirements; healthcare and financial service access increasingly mandates digital competency
 *   - Rural communities (moderate/constrained): Secondary victims — geographically isolated from training resources; business competitiveness depends on digital platforms; broadband access is prerequisite
 *   - Tech platform corporations (institutional/arbitrage): Primary beneficiaries — extract value through user data, fees, algorithmic control, and lock-in effects; digitalization of services increases platform dependency
 *   - Credential gatekeepers (institutional/arbitrage): Secondary beneficiaries — universities, certification programs, professional licensing bodies; digital skills certifications become rent-extraction mechanisms
 *   - High-literacy knowledge workers (powerful/mobile): Beneficiaries — increased relative economic returns; more job options; lower cost of platform adoption
 *   - Public library and community college system (organized/constrained): Scaffold agents — providing temporary training infrastructure; constrained by funding; see sunset as education institutions close the skills gap
 *   - Analytical observer (analytical/analytical): Risk of naturalizing inequality as inevitable feature of technological progress rather than contingent institutional extraction
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(digital_literacy_inequality, 0.58).
domain_priors:suppression_score(digital_literacy_inequality, 0.65).
domain_priors:theater_ratio(digital_literacy_inequality, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(digital_literacy_inequality, extractiveness, 0.58).
narrative_ontology:constraint_metric(digital_literacy_inequality, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(digital_literacy_inequality, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(digital_literacy_inequality, snare).
narrative_ontology:human_readable(digital_literacy_inequality, "Digital Literacy Inequality and Economic Exclusion").
narrative_ontology:topic_domain(digital_literacy_inequality, "economic/social/technological").

domain_priors:requires_active_enforcement(digital_literacy_inequality).
% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(digital_literacy_inequality, tech_platform_corporations).
narrative_ontology:constraint_beneficiary(digital_literacy_inequality, credential_gatekeepers).
narrative_ontology:constraint_beneficiary(digital_literacy_inequality, high_literacy_workers).
narrative_ontology:constraint_victim(digital_literacy_inequality, low_literacy_populations).
narrative_ontology:constraint_victim(digital_literacy_inequality, elderly_adults).
narrative_ontology:constraint_victim(digital_literacy_inequality, rural_communities).
narrative_ontology:constraint_victim(digital_literacy_inequality, informal_sector_workers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: LOW-LITERACY ADULT (SNARE) — Structurally trapped by employment barriers, financial services gatekeeping, and inability to access governmental/medical/educational systems. No exit option from dependence on intermediaries. Digital skill gaps translate directly to income loss and life-outcome constraints. Maximum extraction experienced.
constraint_indexing:constraint_classification(digital_literacy_inequality, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: RURAL SMALL BUSINESS OWNER (TANGLED ROPE) — Experiences both genuine coordination benefit (digital platforms enable market access) and asymmetric extraction (platform fees, algorithmic opacity, data harvesting). Constrained by cost of digital infrastructure and training. Cannot exit platform dependence without losing market reach, but also cannot compete without digital sophistication.
constraint_indexing:constraint_classification(digital_literacy_inequality, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: TECH PLATFORM CORPORATION (ROPE) — Experiences the constraint as coordination: connecting users to services, aggregating supply and demand, providing infrastructure. Net beneficiary with full arbitrage capacity. For this agent, the constraint is functional (coordination problem solved by platform) rather than extractive. Can exit or modify constraints without structural penalty.
constraint_indexing:constraint_classification(digital_literacy_inequality, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: PUBLIC LIBRARY AND COMMUNITY COLLEGE (SCAFFOLD) — Organized agents providing temporary support infrastructure (free digital access, training programs). See the constraint as temporary and solvable through education and infrastructure investment. Have sunset logic: as digital literacy becomes universal norm, specialized bridging programs can sunset. Currently constrained by funding and access gaps.
constraint_indexing:constraint_classification(digital_literacy_inequality, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: DIGITAL DIVIDE DISCOURSE INSTITUTION (PITON) — Policy frameworks, research agendas, and funding mechanisms organized around 'bridging the digital divide.' Theater ratio (0.48) reflects that much activity is performative: report publication, program announcements, training statistics. Actual impact on earnings and opportunity differentials is modest. The narrative persists through institutional inertia and funding cycle dependency.
constraint_indexing:constraint_classification(digital_literacy_inequality, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / TECHNOLOGICAL DETERMINISM (MOUNTAIN) — Risk of naturalizing digital literacy inequality as an inevitable feature of technological progress: 'some people will always lag behind,' 'new technologies create temporary disruption that resolves through generational turnover.' This framing misses the structural extraction. The engine's false summit detector will flag this as naturalization of contingent institutional inequality (unequal access to training, credit, infrastructure, time).
constraint_indexing:constraint_classification(digital_literacy_inequality, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(digital_literacy_inequality_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(digital_literacy_inequality, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(digital_literacy_inequality, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(digital_literacy_inequality, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(digital_literacy_inequality, TR),
    TR >= 0.70.

:- end_tests(digital_literacy_inequality_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. The constraint extracts through multiple channels: wage penalty for low-digital-skill workers (~15-25% lifetime earnings reduction), platform fee extraction from informal sector and low-literacy populations using digital services, access denial from essential services (government, financial), and time/money costs of remedial training. The value is not at maximum (0.70+) because some genuine coordination benefit exists — platforms do enable market access and service delivery — and some institutional support (libraries, government training) provides partial mitigation. Suppression (0.65): High. Barriers to digital skill acquisition are substantial: cost of devices and internet, time poverty of working-poor populations, cognitive barriers for older adults, spatial isolation in rural areas, and for some populations, identity-locked perception ('I'm not a tech person'). Suppression is high but not total because some escape routes exist (public libraries, community programs, motivated self-learning). Theater ratio (0.48): Moderate-low. The constraint is relatively transparent — the digital skill gap translates directly to visible wage gaps, employment barriers, and service access denial. While performative components exist (training statistics, program announcements, bridge program rhetoric), the underlying extraction is not heavily obscured by false coordination claims.
 *
 * PERSPECTIVAL GAP:
 *   The gap between powerless/trapped (snare) and institutional/arbitrage (rope) is maximal — the same constraint appears as pure extraction vs pure coordination. This gap reveals the structural role-dependence of classification: the platform's experienced coordination genuinely solves problems for them (user aggregation, value creation). The victim's experienced extraction genuinely constrains them (wage penalty, access denial). Neither perspective is wrong — both are accurate descriptions of their structural position. The gap is diagnostic: it shows that beneficiary and victim populations experience fundamentally different structures and that 'solving the problem' (from the platform perspective) does not require solving the victim's extraction (from the victim perspective). The scaffold and piton perspectives show institutional responses to this gap: scaffold attempts to bridge via education, piton shows how the bridge program itself becomes performative theater.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality derives from power level, exit options, and beneficiary/victim status. Powerless/trapped victims experience maximum d (~0.95), producing high f(d) and high chi due to no exit capacity and full victim status. Institutional/arbitrage platform corporations experience low d (~0.15) due to beneficiary status and full arbitrage exit — they can restructure or exit without penalty. Organized scaffold agents experience moderate d (~0.50) due to constrained exit and mixed beneficiary/victim status (they benefit from the constraint's existence as it justifies their educational missions, but they also bear victim status through institutional mission to remediate). The moderate small business owner experiences moderate-high d (~0.65) due to constrained exit (platform dependence) and mixed beneficiary/victim status (benefits from platform reach but victim to platform extraction).
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by distinguishing institutional coordination (platforms enabling markets, services, communication) from extraction (concentration of surplus, monopolistic gatekeeping, data harvesting). The beneficiary perspective (rope) identifies genuine coordination benefit. The victim perspective (snare) identifies genuine extraction. The tangled_rope perspective identifies the hybrid: platforms provide real coordination value AND extract asymmetrically. The analytical observer's risk is naturalizing this hybrid as inevitable ('technology always creates winners and losers') rather than recognizing it as a choice about how to structure digital institutions. The constraint's classification resolves around whether digital platforms are designed as public coordination infrastructure (lower extraction, more rope-like) or as proprietary monopolistic gatekeepers (higher extraction, more snare-like). Current trajectory (ε increasing from 0.35 to 0.58) suggests increasing extractive design rather than increasing coordination efficiency.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    skill_acquisition_ceiling,
    'Is digital literacy ceiling determined by age-cohort effects or by structural barriers to training access?',
    'Longitudinal tracking of adult learner outcomes; comparison of completion rates between subsidized comprehensive programs vs. self-directed learning; measurement of retention and transfer to employment outcomes',
    'If primarily age cohort: extractiveness decreases as generations turn over (temporary constraint). If primarily structural barriers: extractiveness persists indefinitely and requires active intervention. Classification impacts: trap→mountain (natural law) vs trap→snare (extractive institutional design).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(skill_acquisition_ceiling, empirical, 'Skill acquisition ceiling determination: age vs structural barriers').

omega_variable(
    platform_necessity_vs_monopoly,
    'Are digital platforms genuinely necessary coordination infrastructure or monopolistic gatekeepers enforcing artificial dependency?',
    'Measurement of switching costs for users; analysis of alternative platform ecosystems; assessment of data portability and interoperability requirements; examination of incumbent platform barriers to entry',
    'If genuinely necessary infrastructure: constraint is rope or tangled_rope (coordination with extraction). If monopolistic gatekeeping: constraint is snare or tangled_rope (extraction with coordination cover). Changes beneficiary/victim classification and directionality.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(platform_necessity_vs_monopoly, empirical, 'Platform necessity vs monopolistic gatekeeping').

omega_variable(
    intergenerational_transmission,
    'Does digital literacy inequality transmit across generations through parental education/income or through diminishing public education resources?',
    'Cohort analysis of first-generation digital learners from low-literacy households; comparison of digital skill development in well-resourced vs under-resourced schools; measurement of home internet access correlation with student outcomes controlling for parental education',
    'If parental transmission dominates: scaffold programs must target households and early childhood. If resource inequality dominates: public infrastructure investment is primary lever. Affects whether scaffold sunset is realistic.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(intergenerational_transmission, empirical, 'Intergenerational transmission mechanism: parental vs resource-based').

omega_variable(
    identity_locked_digital_exclusion,
    'For some populations, is digital exclusion partly structural (access barriers) and partly identity-locked (self-concept incompatible with ''digital person'' identity)?',
    'Qualitative analysis of narratives from adult learners; measurement of completion vs dropout rates controlling for access barriers; tracking of learner identity shift (''I''m not a tech person'' → ''I can do this''); post-program skill persistence vs employment outcome correlation',
    'If identity-locked: lowered exit_options threshold; powerless→identity_locked shifts classification from snare→mountain (mountain-like intransigence) to snare→rope (perceived mutability). Affects long-term extraction persistence.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_locked_digital_exclusion, empirical, 'Identity-locked digital exclusion mechanism').

omega_variable(
    extraction_vs_coordination_ratio,
    'What proportion of platform value extraction is genuine coordination cost vs. surplus capture (platform fee, data harvesting, algorithmic opacity)?',
    'Comparative analysis of platform revenue models; measurement of user welfare gains from platform access vs. platform profit extraction; studies of alternative cooperative/public platform models and their cost structures',
    'If coordination cost dominates: platforms are rope (high genuine value). If extraction dominates: platforms are snare. Affects beneficiary/victim classification for institutional actors and chi computation for platform perspective.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_vs_coordination_ratio, empirical, 'Platform extraction vs genuine coordination cost ratio').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(digital_literacy_inequality, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(diglit_tr_t0, digital_literacy_inequality, theater_ratio, 0, 0.3).
narrative_ontology:measurement(diglit_tr_t10, digital_literacy_inequality, theater_ratio, 10, 0.42).
narrative_ontology:measurement(diglit_tr_t20, digital_literacy_inequality, theater_ratio, 20, 0.48).

% Extraction over time
narrative_ontology:measurement(diglit_be_t0, digital_literacy_inequality, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(diglit_be_t10, digital_literacy_inequality, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(diglit_be_t20, digital_literacy_inequality, base_extractiveness, 20, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(digital_literacy_inequality, global_infrastructure).
narrative_ontology:boltzmann_floor_override(digital_literacy_inequality, 0.18).
narrative_ontology:affects_constraint(digital_literacy_inequality, algorithmic_opacity_in_job_matching).
narrative_ontology:affects_constraint(digital_literacy_inequality, financial_service_gatekeeping_through_digital_identity).
narrative_ontology:affects_constraint(digital_literacy_inequality, credential_inflation_in_technical_roles).

% DUAL FORMULATION NOTE:
% Digital literacy inequality decomposes into three structurally distinct constraints with different ε values: (1) skill-wage mismatch (ε≈0.40, coordination problem causing temporary wage gap), (2) platform monopoly gatekeeping (ε≈0.65, snare-like extraction through access control), (3) identity-locked exclusion (ε≈0.50, moderate snare with partial identity-lock mechanism). This story treats the aggregate constraint. Downstream stories address specific institutional mechanisms.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(digital_literacy_inequality, institutional, 0.18).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
