% ============================================================================
% CONSTRAINT STORY: open_source_contributor_retention
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_open_source_contributor_retention, []).

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
 *   constraint_id: open_source_contributor_retention
 *   human_readable: Open Source Contributor Retention and Value Extraction
 *   domain: software_economics/labor
 *
 * SUMMARY:
 *   Open source software has become critical digital infrastructure, yet the
 *   labor that sustains it relies disproportionately on unpaid volunteer
 *   contributors. This constraint exhibits the tension between genuine
 *   coordination functions (enabling collaboration, reducing duplicated
 *   effort, building commons knowledge) and systematic extraction of value
 *   from powerless contributors to benefit corporations and platform
 *   providers. The volunteer contributor experiences identity fusion with
 *   projects, creating psychological lock-in that suppresses exit even when
 *   economic value extraction is severe. The independent maintainer navigates
 *   a mixed coordination-extraction dynamic, benefiting from the ecosystem
 *   while managing unsustainable workloads. Corporate beneficiaries capture
 *   enormous value (infrastructure, competitive advantage, revenue streams)
 *   while experiencing the relationship as pure coordination. Sustainability
 *   initiatives (grants, sponsorships, alternative funding models) are
 *   building sunset mechanisms that could restructure the constraint, but
 *   adoption is uneven. The open source ideology performs the function of
 *   legitimating free software even as the actual mechanism (volunteer
 *   heroism) has substantially degraded, requiring corporate subsidies to
 *   function. The platform providers (GitHub, GitLab) add another extraction
 *   layer through lock-in and behavioral data capture.
 *
 * KEY AGENTS:
 *   - Volunteer Contributors: Primary victims (powerless/trapped) — provide unpaid labor, experience identity lock, face burnout and exit barriers
 *   - Independent Maintainers: Secondary victims (moderate/constrained) — manage projects as unpaid work, face burnout risk, have constrained exit options
 *   - Corporate Beneficiaries: Primary beneficiaries (institutional/arbitrage) — build products on open source, gain competitive advantage, control the relationship entirely
 *   - Platform Providers: Secondary beneficiaries (institutional/arbitrage) — extract network effects and lock-in value from ecosystem activity
 *   - Sustainability Initiatives: Organized agents (organized/constrained) — Linux Foundation, GitHub Sponsors, Open Collective building alternative value-capture and sunset mechanisms
 *   - Sustainability Commons: Victim (powerless/trapped) — the shared resource of maintainer wellbeing and project stability that bears the extraction cost
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing volunteer extraction as necessary to software economics
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(open_source_contributor_retention, 0.58).
domain_priors:suppression_score(open_source_contributor_retention, 0.62).
domain_priors:theater_ratio(open_source_contributor_retention, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(open_source_contributor_retention, extractiveness, 0.58).
narrative_ontology:constraint_metric(open_source_contributor_retention, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(open_source_contributor_retention, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(open_source_contributor_retention, tangled_rope).
narrative_ontology:human_readable(open_source_contributor_retention, "Open Source Contributor Retention and Value Extraction").
narrative_ontology:topic_domain(open_source_contributor_retention, "software_economics/labor").

domain_priors:requires_active_enforcement(open_source_contributor_retention).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(open_source_contributor_retention, corporate_maintainers).
narrative_ontology:constraint_beneficiary(open_source_contributor_retention, proprietary_vendors).
narrative_ontology:constraint_beneficiary(open_source_contributor_retention, platform_providers).
narrative_ontology:constraint_victim(open_source_contributor_retention, volunteer_contributors).
narrative_ontology:constraint_victim(open_source_contributor_retention, independent_maintainers).
narrative_ontology:constraint_victim(open_source_contributor_retention, sustainability_commons).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: VOLUNTEER CONTRIBUTOR (SNARE) — Trapped by identity fusion with the project and sunk psychological investment. Cannot exit without abandoning professional reputation built through contribution. High extraction: unpaid labor, burnout risk, no benefits or job security. Suppression through reputational lock and community pressure.
constraint_indexing:constraint_classification(open_source_contributor_retention, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: INDEPENDENT MAINTAINER (TANGLED ROPE) — Genuine coordination function: the project solves real problems and enables collaboration. But also experiences asymmetric extraction: managing community expectations, handling security issues, dealing with entitled users. Exit is possible (switch careers, abandon project) but costly in reputation and identity terms. Mixed experience of coordination and extraction.
constraint_indexing:constraint_classification(open_source_contributor_retention, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: CORPORATE BENEFICIARY (ROPE) — Extracts value through dependency: builds proprietary products on open source foundations, captures licensing/support revenue, gains first-mover advantages. Experiences constraint as pure coordination: 'we benefit from the ecosystem, we contribute back.' Net beneficiary with full exit option (could fork, could build proprietary alternative) but chooses to remain. Zero effective extraction when you control the relationship.
constraint_indexing:constraint_classification(open_source_contributor_retention, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: SUSTAINABILITY INITIATIVE (SCAFFOLD) — Organized agents (Linux Foundation, GitHub Sponsors, grants programs) see this as a temporary coordination failure with a sunset clause. Initiatives like Tidelift, Open Collective, and corporate sponsorship are building alternative value-capture mechanisms that align incentives. Suppression is declining as alternatives mature. Sunset clause: as sustainable funding models proliferate, the pure volunteer extraction mechanism loses force.
constraint_indexing:constraint_classification(open_source_contributor_retention, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: OPEN SOURCE IDEOLOGY (PITON) — The narrative that open source 'just works' through community goodwill has become substantially performative. The mythology persists (we celebrate volunteer heroism, we tell origin stories of passionate hackers) while the actual mechanism has atrophied — most significant open source projects now depend on corporate subsidies or grants. The ideology performs its function (legitimates free software) without executing it (volunteers cannot sustain complex projects alone). Theater ratio high because the volunteer narrative is maintained despite being empirically degraded.
constraint_indexing:constraint_classification(open_source_contributor_retention, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: PLATFORM PROVIDER (TANGLED ROPE) — GitHub, GitLab, etc. have genuine coordination function: hosting, CI/CD, community tools. But also extract value through lock-in and behavioral data. Users (contributors) have constrained exits — alternatives exist but switching costs are high. Platforms benefit from contributor activity and ecosystem effects. Mixed coordination and extraction, with asymmetry toward platform.
constraint_indexing:constraint_classification(open_source_contributor_retention, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURALIZATION RISK (MOUNTAIN) — From a civilizational view, one might argue that some extraction is inherent to knowledge production: complex software is hard to maintain, value creation requires labor, and free software lives in tension with economic sustainability. This view risks naturalizing what is actually a contingent institutional arrangement (the choice to extract value from volunteers rather than build sustainable funding models).
constraint_indexing:constraint_classification(open_source_contributor_retention, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(open_source_contributor_retention_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(open_source_contributor_retention, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(open_source_contributor_retention, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(open_source_contributor_retention, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(open_source_contributor_retention, TR),
    TR >= 0.70.

:- end_tests(open_source_contributor_retention_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high, trending upward. Over the 15-year interval, the amount of value extracted from volunteer and independent contributors has increased as open source has become critical infrastructure and corporate dependence has grown. The initial value (0.35) reflects earlier phases when open source was more niche and extraction less organized. The current level (0.58) reflects systematic value extraction: volunteer labor generates billions in corporate value while most projects operate on minimal budgets. The upward trajectory reflects increasing professionalization of corporate open source consumption without corresponding growth in contributor funding. Suppression (0.62): Moderate-high. Barriers to exit are significant: identity fusion, sunk reputation investment, lack of alternative income, community pressure ('you started this, you owe it'), platform lock-in (where would the project go?). But suppression is not total — some contributors do burn out and leave; some projects do migrate platforms. Theater ratio (0.58): Moderate-high. The open source mythology (passionate hackers building amazing software for free) performs legitimation while the actual mechanism has degraded (most significant projects require corporate subsidy). The celebration of volunteer contributions masks the unsustainability. The GitHub ideology of 'democratized software development' obscures the platform extraction layer. Theater has increased as the disconnect between narrative (pure community) and reality (corporate-dependent) has widened.
 *
 * PERSPECTIVAL GAP:
 *   Seven perspectives produce five distinct classifications (snare, tangled_rope, rope, scaffold, piton, mountain). This range demonstrates why indexical classification is necessary. No single type captures the constraint adequately from all positions. The gap between rope (corporate perspective) and snare (volunteer perspective) is the diagnostic signal: the same structural mechanism produces opposite experiences based on power asymmetry. The piton classification reveals that the open source ideology has degraded (theater ratio 0.58) — the mythology of volunteer-powered communities persists while the actual mechanism requires corporate subsidy. The scaffold classification reveals a genuine sunset mechanism in development — sustainability initiatives are not just policy interventions but structural changes in how the ecosystem captures and distributes value. The mountain classification is a false summit — the analytical observer risks naturalizing institutional arrangements (choosing to extract from volunteers rather than fund maintainers) as laws of nature.
 *
 * DIRECTIONALITY LOGIC:
 *   The constraint's extraction flow is asymmetric: volunteer/independent contributors are targets; corporations and platforms are beneficiaries. Exit options differentiate within each group: trapped volunteers experience maximum extraction; constrained maintainers experience mixed extraction-coordination; corporate beneficiaries with arbitrage have zero effective extraction (they benefit from the relationship and control it entirely). Platform providers occupy an intermediate position — they benefit from network effects but are somewhat dependent on ecosystem health, creating moderate extraction rather than minimal. The beneficiary/victim declarations capture this: beneficiaries are [corporate_maintainers, proprietary_vendors, platform_providers]; victims are [volunteer_contributors, independent_maintainers, sustainability_commons]. This maps directly to structural power and exit capacity. The engine derives d from these declarations — low d for beneficiaries (they benefit), high d for victims (they bear costs).
 *
 * MANDATROPHY ANALYSIS:
 *   STRUCTURAL EXEMPLAR: This constraint resolves the mandatrophy by revealing that the tension between coordination and extraction is not about which is 'really' true, but about observable-dependent classification from different structural positions. From the volunteer's perspective (trapped, powerless), the constraint is a snare — pure extraction with minimal coordination benefit perceived at that structural location. From the corporate beneficiary's perspective (institutional, arbitrage), the constraint is rope — they experience it as coordination and have complete agency. Both are accurate descriptions of their experience. The mandatrophy is resolved by recognizing that the constraint IS coordination (it does solve the problem of building complex software collaboratively) AND extraction (it systematically transfers value from powerless contributors to powerful corporations). The measured extractiveness (0.58) is neither 'too high' for coordination nor 'too low' for extraction — it accurately captures the hybrid nature. The Tangled Rope classification is correct because: (1) genuine coordination function exists (collaborative development, knowledge commons), (2) asymmetric extraction exists (value flows from volunteers to corporations), (3) active enforcement exists (community norms, GitHub's terms of service, funding gatekeeping), and (4) the two functions are structurally intertwined (you cannot remove the extraction without damaging coordination, and you cannot remove coordination without eliminating the basis for extraction). The false summit here would be calling it pure rope (ignoring extraction) or pure snare (ignoring coordination) from a universal position. The accurate classification requires indexing to position.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    identity_lock_vs_constrained,
    'Is the psychological bind on volunteer contributors identity-locked (ego/professional identity fused with project) or merely constrained (high-cost exit)?',
    'Post-exit trajectory analysis: if contributor experiences depression/identity loss after leaving project, identity-locked. If they quickly reorganize their career/identity, merely constrained.',
    'If identity-locked: classification as mountain becomes appropriate at biographical timescale from the trapped perspective (oracle gap). If merely constrained: snare classification is accurate. Changes the Mandatrophy signature.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_vs_constrained, empirical, 'Whether contributor burnout is identity-fusion or high-cost exit').

omega_variable(
    value_extraction_measurement,
    'How much total value is extracted from volunteer/independent contributors annually across the open source ecosystem?',
    'Econometric analysis: cost-replacement valuation (what would these volunteer-hours cost if purchased on market), compared against funding available to projects. Surveys of contributor opportunity-cost (salary foregone, time unavailable for paid work).',
    'If value > $50B annually with <$2B in funding: severe extraction (snare confirmed). If value > $50B with $10B+ in funding: mixed coordination-extraction (tangled_rope confirmed). If value < $10B: pure coordination (rope from more perspectives).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(value_extraction_measurement, empirical, 'Total annual value extracted from volunteer contributions').

omega_variable(
    sustainability_initiative_effectiveness,
    'Do corporate sponsorship programs, grants, and alternative funding models actually reduce the extraction of volunteer labor, or merely obscure it?',
    'Longitudinal study: compare burnout rates, contribution patterns, and project health metrics in funded vs unfunded projects. Control for project size and maturity. Measure whether funding reduces volunteer workload or merely supplements it.',
    'If funding reduces extraction: scaffold perspective confirmed, sunset is real. If funding complements but doesn''t replace volunteer labor: scaffold is aspirational, extraction persists. Changes prognosis for constraint evolution.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sustainability_initiative_effectiveness, empirical, 'Whether funding initiatives reduce volunteer extraction or merely add layer').

omega_variable(
    platform_lock_magnitude,
    'How much of GitHub''s value comes from network effects on open source ecosystem vs proprietary features and hosted services?',
    'Analysis of migration costs for projects attempting to move from GitHub to alternatives (Gitea, GitLab self-hosted). Survey data on perceived switching costs and barriers.',
    'If network effects dominate (>60% of value from ecosystem): platform lock is primary extraction mechanism (platform perspective becomes snare). If proprietary features dominate: platform provides genuine value and is less extractive than open source volunteer system.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(platform_lock_magnitude, empirical, 'What portion of platform value derives from network effects vs proprietary features').

omega_variable(
    corporate_subsidy_dependence,
    'What percentage of major open source projects receive direct corporate funding vs relying on pure volunteer labor?',
    'Systematic survey of top 100 GitHub projects by stars/usage. Coding for funding source: corporate sponsorship, grants, individual donations, none. Correlation with burnout/abandonment rates.',
    'If >80% receive corporate funding: open source is already a tangled_rope (mixed coordination-subsidy). If <30% receive funding: pure volunteer extraction (snare). Changes whether piton''s degradation narrative is accurate.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(corporate_subsidy_dependence, empirical, 'Prevalence of corporate funding vs pure volunteer dependence').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(open_source_contributor_retention, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(oscr_tr_t0, open_source_contributor_retention, theater_ratio, 0, 0.42).
narrative_ontology:measurement(oscr_tr_t5, open_source_contributor_retention, theater_ratio, 5, 0.52).
narrative_ontology:measurement(oscr_tr_t10, open_source_contributor_retention, theater_ratio, 10, 0.58).
narrative_ontology:measurement(oscr_tr_t15, open_source_contributor_retention, theater_ratio, 15, 0.65).

% Extraction over time
narrative_ontology:measurement(oscr_be_t0, open_source_contributor_retention, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(oscr_be_t5, open_source_contributor_retention, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(oscr_be_t10, open_source_contributor_retention, base_extractiveness, 10, 0.58).
narrative_ontology:measurement(oscr_be_t15, open_source_contributor_retention, base_extractiveness, 15, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(open_source_contributor_retention, resource_allocation).
narrative_ontology:boltzmann_floor_override(open_source_contributor_retention, 0.12).
narrative_ontology:affects_constraint(open_source_contributor_retention, software_supply_chain_risk).
narrative_ontology:affects_constraint(open_source_contributor_retention, digital_infrastructure_fragility).
narrative_ontology:affects_constraint(open_source_contributor_retention, knowledge_commons_sustainability).

% DUAL FORMULATION NOTE:
% Open source contributor retention decomposes into multiple structurally distinct constraints: (1) volunteer_labor_extraction (ε=0.58, snare from volunteer perspective), (2) corporate_subsidy_dependence (ε=0.42, scaffold from sustainability perspective), (3) platform_lock_in (ε=0.35, rope from platform perspective). These are linked through the constraint family: corporate beneficiaries can only extract because volunteers are trapped; sustainability initiatives provide sunset only if they successfully fund maintainers; platform providers capture value only because of ecosystem network effects. The higher-extractiveness story dominates the family's classification.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(open_source_contributor_retention, organized, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
