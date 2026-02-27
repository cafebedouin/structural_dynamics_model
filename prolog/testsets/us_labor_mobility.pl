% ============================================================================
% CONSTRAINT STORY: us_labor_mobility
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_us_labor_mobility, []).

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
 *   constraint_id: us_labor_mobility
 *   human_readable: US Geographic and Professional Labor Mobility
 *   domain: economic/technological
 *
 * SUMMARY:
 *   US labor mobility—the ability of workers to relocate for economic
 *   opportunity—operates as a hybrid coordination and extraction mechanism.
 *   The nominal function is coordination: credential standards and licensing
 *   provide employers with trust in worker capability, enabling national
 *   labor matching. The structural reality diverges sharply by worker
 *   position. High-skill, networked workers in coastal tech hubs experience
 *   genuine coordination—national markets increase their opportunity set.
 *   Place-bound workers in rural and declining industrial regions experience
 *   extraction: credential requirements, state licensing friction, housing
 *   market gatekeeping, and lack of social capital effectively trap them in
 *   local labor markets with declining opportunities. The extractiveness has
 *   increased over 30 years as geographic inequality has widened, credential
 *   requirements have inflated, and housing supply constraints in
 *   high-productivity regions have intensified. The theater ratio (0.48)
 *   reflects that credential renewal requirements and licensing maintenance
 *   increasingly function as performative recertification rather than
 *   meaningful quality assurance, especially as occupation-specific knowledge
 *   becomes obsolete faster than licensure cycles.
 *
 * KEY AGENTS:
 *   - Geographically Immobile Workers: Primary victims (powerless/trapped) — lack capital, networks, or credentials for relocation; trapped in declining local labor markets
 *   - High-Skill Coastal Tech Workers: Primary beneficiaries (institutional/arbitrage) — capture wage premiums from national labor market access; can relocate at low cost; experience constraint as coordination
 *   - Educational Credential Gatekeepers: Active enforcers (organized/constrained) — universities, professional associations, licensing boards; benefit from credential requirements (tuition, fees, authority); provide genuine standardization function but also suppress competition
 *   - State Licensing Boards: Institutional enforcers (institutional/constrained) — occupational licensing creates state-level friction; maintain through bureaucratic inertia rather than functional quality assurance
 *   - Housing Supply Gatekeepers: Structural beneficiaries (institutional/arbitrage) — zoning restrictions and supply constraints in high-wage regions; benefit from artificial scarcity; limit mobility through cost barriers
 *   - Rural Labor Markets: Systemic victim (powerless/trapped) — drained of talent; remaining workers face declining job options and wages; cannot organize exit
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(us_labor_mobility, 0.58).
domain_priors:suppression_score(us_labor_mobility, 0.62).
domain_priors:theater_ratio(us_labor_mobility, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(us_labor_mobility, extractiveness, 0.58).
narrative_ontology:constraint_metric(us_labor_mobility, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(us_labor_mobility, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(us_labor_mobility, tangled_rope).
narrative_ontology:human_readable(us_labor_mobility, "US Geographic and Professional Labor Mobility").
narrative_ontology:topic_domain(us_labor_mobility, "economic/technological").

domain_priors:requires_active_enforcement(us_labor_mobility).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(us_labor_mobility, high_skill_workers_networked).
narrative_ontology:constraint_beneficiary(us_labor_mobility, coastal_tech_hubs).
narrative_ontology:constraint_beneficiary(us_labor_mobility, educational_credential_gatekeepers).
narrative_ontology:constraint_victim(us_labor_mobility, geographically_immobile_workers).
narrative_ontology:constraint_victim(us_labor_mobility, rural_labor_markets).
narrative_ontology:constraint_victim(us_labor_mobility, credential_excluded_workers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PLACE-BOUND WORKER (SNARE) — Lacks capital, social network, or credentials for geographic relocation. Constrained to local labor markets with limited job options. Career mobility is virtually impossible without prohibitive costs (education, moving, credential acquisition). Trapped by family obligations, healthcare needs, or lack of liquidity. Experiences maximal extraction — constraint prevents access to higher-wage opportunities nationwide.
constraint_indexing:constraint_classification(us_labor_mobility, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: CREDENTIAL-BEARING PROFESSIONAL (TANGLED ROPE) — Holds degree or certification enabling geographic mobility, but faces barriers: student debt constrains relocation choice, professional networks are region-specific, occupational licensing creates friction between states. Benefits from national labor market access for jobs matching credentials; bears extraction through debt servicing, network establishment costs, and regulatory compliance overhead. Mixed coordination-extraction.
constraint_indexing:constraint_classification(us_labor_mobility, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: COASTAL TECH HUB EMPLOYER (ROPE) — Benefits from geographic wage arbitrage: attracts talent nationwide by offering premium salaries while capturing productivity gains. Experiences the constraint as pure coordination: the ability to hire from across the country enables specialization and capital concentration. High exit options (can relocate, can hire remote, can partner with other hubs). Net beneficiary.
constraint_indexing:constraint_classification(us_labor_mobility, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: CREDENTIAL INDUSTRY (TANGLED ROPE) — Universities, professional certification bodies, licensing boards enforce the credential requirement that gates geographic mobility. They benefit from the constraint (tuition, exam fees, continuing education mandates) while providing genuine coordination function (standardization enables employer trust in worker quality). Active enforcement through accreditation, licensure verification, and professional standards. Organized institutional actors with some exit options (alternative credentials emerging, but slowly).
constraint_indexing:constraint_classification(us_labor_mobility, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: OCCUPATIONAL LICENSING SYSTEM (PITON) — Professional licensing (medical, legal, engineering, cosmetology) creates state-level friction that persists through institutional inertia. The functional purpose (quality assurance) has largely been replaced by performative recertification requirements. Licensing creates real barriers to interstate mobility but provides minimal additional quality verification beyond initial credentialing. Theater ratio reflects that licensure renewal is largely bureaucratic rather than substantively protective.
constraint_indexing:constraint_classification(us_labor_mobility, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (MOUNTAIN ATTEMPT) — From a civilizational view, human capital localization appears immutable: knowledge and skills are embodied in persons, whose relocation is inherently costly. Some scholars naturalize labor immobility as a consequence of information asymmetry or social capital geography. However, the structural data reveals contingent institutional arrangements (credential gatekeeping, state licensing, housing supply constraints) rather than natural law. This classification is likely a false summit.
constraint_indexing:constraint_classification(us_labor_mobility, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(us_labor_mobility_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(us_labor_mobility, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(us_labor_mobility, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(us_labor_mobility, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(us_labor_mobility, TR),
    TR >= 0.70.

:- end_tests(us_labor_mobility_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The constraint extracts significantly from place-bound workers by limiting their access to national opportunity sets, but extraction is not total—some local mobility is possible, and remote work is beginning to expand options. The value increased from 0.48 to 0.58 over 30 years as geographic inequality widened and credential requirements became more stringent. Suppression (0.62): Moderate-high. Substantial barriers prevent exit: credential requirements impose time and cost, state licensing creates friction between jurisdictions, housing costs in high-wage regions make relocation prohibitive, and lack of social capital makes network formation difficult for isolated workers. Organizing alternative labor matching systems faces network effects favoring incumbent credentialing. Theater ratio (0.48, rising): Moderate. Credential renewal and licensing maintenance increasingly function as ritual compliance rather than meaningful quality verification. Early career credential acquisition serves a genuine signal function; continuing education requirements become increasingly performative as specialized knowledge dominates and standardized recertification cannot meaningfully measure competence.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits a dramatic perspectival gap. The coastal tech employer sees a well-functioning national labor market (Rope)—they can hire the best talent from anywhere and benefit from geographic wage arbitrage. The place-bound worker sees a trap (Snare)—credential requirements and network barriers lock them out of higher-wage opportunities. The credential industry sees legitimate standardization (Tangled Rope with mixed benefit/cost)—they provide real quality assurance but also capture rents through licensing requirements. The state licensing system sees itself as a degraded institution (Piton)—occupational licensing persists through bureaucratic inertia, with licensing renewal requirements becoming increasingly performative. The analytical observer might naturalize immobility as inevitable (Mountain false summit), but the structural data reveals contingent institutional arrangements: credential gatekeeping, state-level licensing friction, and housing supply constraints. None of these are natural laws of labor markets.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality varies sharply by agent position. Beneficiaries (coastal tech employers, credential gatekeepers) have low d—they experience the constraint as beneficial coordination or extraction-favoring arrangement (d ≈ 0.10-0.30). Victims (place-bound workers, rural labor markets) have high d—trapped in local markets with no realistic exit, they experience maximum extraction (d ≈ 0.85-0.95). Moderate agents (professionals with credentials but regional ties) fall in the middle—constrained but not trapped, with some mobility but significant friction (d ≈ 0.50-0.70). The credential industry occupies an ambiguous position: they are enforcers of extraction but also provide genuine standardization coordination, placing them at d ≈ 0.40 (organized/constrained, mixed function). The engine's derivation should produce higher chi for powerless/trapped workers and lower chi for institutional/arbitrage beneficiaries—this perspectival gap is the core diagnostic of the constraint's asymmetry.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy here is the risk of naturalizing geographic inequality as market-efficient rather than extraction-enabled. High-skill workers' concentration in coastal hubs is portrayed as 'agglomeration economies' (natural) rather than as the outcome of credential-gated access and housing scarcity (contingent). The Tangled Rope classification resolves this by documenting both the genuine coordination function (credential standardization does enable employer trust) and the asymmetric extraction (the costs fall predominantly on place-bound workers who benefit least from the coordination function). The piton classification of state licensing reveals that much of the constraint's enforcement is theatrical—continuing licensure requirements persist not because they meaningfully assess competence but because the licensing apparatus has become institutional. The mountain perspective is explicitly marked as likely false: there is nothing natural about labor immobility in a digital-capable economy; the barriers are institutional and policy-modifiable.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    credential_requirement_justification,
    'Do occupational credential requirements primarily serve quality assurance or function as rent-extraction mechanisms that suppress geographic competition?',
    'International comparison of credential-to-outcome ratios; analysis of credential inflation (time/cost required for license renewal relative to actual risk reduction); correlation between licensing stringency and wage premiums',
    'If primarily quality assurance: constraint is Mountain-adjacent coordination. If primarily extraction: constraint is Snare from credential-excluded workers'' perspective. If mixed: Tangled Rope holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(credential_requirement_justification, empirical, 'Whether credentials serve quality assurance or rent extraction').

omega_variable(
    remote_work_structural_change,
    'Does remote work infrastructure fundamentally alter the geographic labor market structure, converting the constraint from Snare to Rope?',
    'Longitudinal wage equity analysis (remote-capable vs place-bound workers); measurement of professional network effects in distributed vs colocated teams; tracking of remote adoption rates by professional category',
    'If remote enables genuine option: many victims gain exit options, classification shifts toward Rope. If remote is selective (high-skill workers only): extraction mechanism persists, remains Tangled Rope or Snare.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(remote_work_structural_change, empirical, 'Whether remote work fundamentally changes labor market geography').

omega_variable(
    housing_supply_as_structural_constraint,
    'Is geographic labor immobility primarily driven by occupational licensing and credential requirements, or by housing supply constraints in high-wage regions?',
    'Counterfactual analysis: regions with loose licensing but tight housing (e.g., California tech + housing shortage) vs tight licensing with abundant housing (e.g., Texas + professional reciprocity); comparison of relocation rates pre/post housing interventions',
    'If housing-dominated: the labeled constraint is misidentified; true constraint is ''housing supply gatekeeping''. If licensing-dominated: Tangled Rope classification holds. If roughly equal: both are coupled constraints requiring network decomposition.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(housing_supply_as_structural_constraint, empirical, 'Relative importance of licensing versus housing supply in mobility barriers').

omega_variable(
    network_effect_substitutability,
    'Can social capital and professional networks be efficiently acquired by relocated workers, or does geographic isolation permanently limit network returns?',
    'Longitudinal earnings tracking of relocated workers; network-formation time measurements; correlation between relocation distance and professional network size after 3/5/10 years',
    'If networks are substitutable: relocation friction is temporary and declining (Scaffold perspective strengthens). If networks are local-dependent: relocation carries permanent opportunity cost (Snare perspective for isolated workers strengthens).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(network_effect_substitutability, empirical, 'Whether professional networks can be acquired post-relocation').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(us_labor_mobility, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(uslm_tr_t0, us_labor_mobility, theater_ratio, 0, 0.35).
narrative_ontology:measurement(uslm_tr_t15, us_labor_mobility, theater_ratio, 15, 0.42).
narrative_ontology:measurement(uslm_tr_t30, us_labor_mobility, theater_ratio, 30, 0.48).

% Extraction over time
narrative_ontology:measurement(uslm_be_t0, us_labor_mobility, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(uslm_be_t15, us_labor_mobility, base_extractiveness, 15, 0.54).
narrative_ontology:measurement(uslm_be_t30, us_labor_mobility, base_extractiveness, 30, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(us_labor_mobility, information_standard).
narrative_ontology:affects_constraint(us_labor_mobility, housing_supply_gatekeeping).
narrative_ontology:affects_constraint(us_labor_mobility, educational_credential_inflation).
narrative_ontology:affects_constraint(us_labor_mobility, regional_wealth_divergence).

% DUAL FORMULATION NOTE:
% US labor mobility decomposes into multiple structural constraints: (1) occupational credential and licensing systems (this story, ε≈0.58), (2) housing supply constraints in high-wage regions (separate story, ε≈0.65), (3) state-level licensing friction (embedded here as piton perspective), (4) social capital geography and network effects (embedded here as direction of d for moderate agents). These are linked through network affects: credential requirements reinforce geographic segregation because credentials concentrate in coastal regions; housing scarcity in those regions compounds the extraction mechanism; regional divergence in opportunity makes credentials more essential, increasing the gatekeeping power of credential institutions. Each story has its own ε and metrics but shares the underlying theme of geographic labor market segmentation.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(us_labor_mobility, organized, 0.45).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
