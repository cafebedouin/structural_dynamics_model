% ============================================================================
% CONSTRAINT STORY: open_source_software_volunteer_model
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_open_source_software_volunteer_model, []).

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
 *   constraint_id: open_source_software_volunteer_model
 *   human_readable: Open Source Software Volunteer Labor Extraction Model
 *   domain: technology/labor/digital_commons
 *
 * SUMMARY:
 *   The open source software volunteer model represents a structural
 *   extraction mechanism that has powered the digital economy for three
 *   decades. Billions of dollars in corporate value rest on unpaid developer
 *   labor, organized through a combination of meritocratic mythology,
 *   identity fusion (maintainers locked into stewardship roles), and
 *   ideological suppression (framing contribution as 'giving back' rather
 *   than 'labor extraction'). The constraint exhibits contradictory
 *   classification from different structural positions: corporations see pure
 *   coordination (rope), maintainers see pure extraction (snare), employed
 *   contributors see mixed mechanisms (tangled rope), and organized
 *   sustainability efforts see a temporary problem with an emerging
 *   alternative pathway (scaffold). The constraint's extraction has increased
 *   significantly over the 35-year interval as the economic value of software
 *   infrastructure has grown while volunteer compensation mechanisms have
 *   remained stagnant or deteriorated (piton degradation of the meritocratic
 *   pathway).
 *
 * KEY AGENTS:
 *   - Volunteer Contributors: Primary victims (powerless/trapped) — bear unpaid labor costs; exit is economically and reputationally costly
 *   - Project Maintainers: Secondary victims (powerless/identity_locked) — structurally mobile but identity-fused to projects; burnout cascade from impossible maintenance loads
 *   - Corporate Software Users: Primary beneficiaries (institutional/arbitrage) — capture billions in value from free/zero-maintenance software; full exit capacity and coordination benefits
 *   - Employed Contributors: Secondary beneficiary (moderate/constrained) — employees of companies using OSS; experience both coordination benefits and extraction constraints
 *   - Sustainability Movement: Organized alternative (organized/constrained) — Linux Foundation, Software Freedom Conservancy, sponsorship platforms building sunset mechanisms
 *   - Meritocratic Ideology: Suppression mechanism (institutional/arbitrage) — narrative that contribution converts to employment/reputation; increasingly performative as this conversion rate declines
 *   - Analytical Observer: Systemic view (analytical/analytical) — sees the constraint as a global extraction mechanism sustained by myth and identity fusion
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(open_source_software_volunteer_model, 0.62).
domain_priors:suppression_score(open_source_software_volunteer_model, 0.68).
domain_priors:theater_ratio(open_source_software_volunteer_model, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(open_source_software_volunteer_model, extractiveness, 0.62).
narrative_ontology:constraint_metric(open_source_software_volunteer_model, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(open_source_software_volunteer_model, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(open_source_software_volunteer_model, tangled_rope).
narrative_ontology:human_readable(open_source_software_volunteer_model, "Open Source Software Volunteer Labor Extraction Model").
narrative_ontology:topic_domain(open_source_software_volunteer_model, "technology/labor/digital_commons").

domain_priors:requires_active_enforcement(open_source_software_volunteer_model).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(open_source_software_volunteer_model, corporate_adopters).
narrative_ontology:constraint_beneficiary(open_source_software_volunteer_model, end_users).
narrative_ontology:constraint_beneficiary(open_source_software_volunteer_model, platform_operators).
narrative_ontology:constraint_victim(open_source_software_volunteer_model, volunteer_contributors).
narrative_ontology:constraint_victim(open_source_software_volunteer_model, maintainer_burnout).
narrative_ontology:constraint_victim(open_source_software_volunteer_model, software_sustainability).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: VOLUNTEER CONTRIBUTOR (SNARE) — A developer contributing to a widely-used open source project faces total extraction: massive corporations depend on their unpaid labor for critical infrastructure, yet the contributor has no negotiating power, no revenue share, no guarantee of recognition or employment outcome. Exit is costly (reputation damage in the technical community, loss of project authority). The suppression is structural: the ideology of 'meritocracy' and 'giving back' naturalizes the extraction, preventing collective action. Trapped at biographical horizon.
constraint_indexing:constraint_classification(open_source_software_volunteer_model, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: MAINTAINER WITH IDENTITY FUSION (SNARE via identity_locked) — The primary maintainer experiences extraction differently than casual contributors. They have structurally mobile options (they could stop, delegate, demand payment) but are identity-locked to the project: their professional identity, reputation, and self-concept are constituted through stewardship of the project. Exit would require becoming a different person professionally. Burnout accelerates but identity lock prevents exit. The constraint persists not through material barriers but through internalized obligation framing ('I am the person who maintains this').
constraint_indexing:constraint_classification(open_source_software_volunteer_model, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(global))).

% PERSPECTIVE 3: EMPLOYED CONTRIBUTOR (TANGLED ROPE) — A developer working for a tech company that uses and contributes to open source experiences genuine coordination (collaboration across company boundaries, shared code reduces redundant development) alongside asymmetric extraction (their employer captures value from collective ecosystem, while individual contributors remain unpaid). They are constrained by employment (cannot exit without career cost) but also benefit from project reputation and skill development. Mixed experience.
constraint_indexing:constraint_classification(open_source_software_volunteer_model, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 4: LARGE CORPORATION (ROPE) — Google, Meta, Amazon, etc. experience the OSS volunteer model as pure coordination: they collaborate via open source standards, reduce development costs through shared infrastructure, and gain interoperability benefits. They can arbitrage: use the software, contribute when strategically useful, withdraw if licensing terms change. No meaningful extraction experienced because they have full exit capacity and genuine coordination benefit. The constraint appears as the cooperative mechanism it partially is.
constraint_indexing:constraint_classification(open_source_software_volunteer_model, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: MERITOCRATIC OSS IDEOLOGY (PITON) — The narrative that 'the best contributors rise and are rewarded by reputation / employment' was functional in early open source (1990s–2000s when recruitment from OSS communities was an actual path to employment). Over a generational timescale, this story has become increasingly performative: the same reputation/employment pathway is now available to far fewer contributors, yet the ideology persists as the mechanism that naturalizes the extraction. Theater ratio (0.55) reflects the ongoing ritualistic celebration of volunteer contribution despite degraded functional connection between contribution and reward.
constraint_indexing:constraint_classification(open_source_software_volunteer_model, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: SUSTAINABLE FUNDING MOVEMENT (SCAFFOLD) — Organizations like the Linux Foundation, Software Freedom Conservancy, Tidelift, and individual sponsorship platforms (GitHub Sponsors, Patreon) represent a structured alternative to pure volunteer extraction. This organized response has sunset logic: as funding and sustainability norms mature, the pure volunteer model's extraction mechanism loses force. However, suppression remains high because corporate incentives favor the zero-cost extraction model, and adoption of sustainability mechanisms is constrained by coordination costs and incumbent power.
constraint_indexing:constraint_classification(open_source_software_volunteer_model, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (SNARE) — From a civilizational scope, the OSS volunteer model is revealed as a global extraction mechanism masquerading as meritocracy. Billions in corporate value rest on unpaid developer labor, organized through identity-lock and suppressed by myth. The coordination benefits are real (standards, interoperability) but vastly outweighed by asymmetric extraction. This perspective sees the constraint as pure snare, not mixed. The mythological framing (meritocracy, community giving) is the suppression mechanism.
constraint_indexing:constraint_classification(open_source_software_volunteer_model, snare,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(open_source_software_volunteer_model_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(open_source_software_volunteer_model, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(open_source_software_volunteer_model, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(open_source_software_volunteer_model, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(open_source_software_volunteer_model, TR),
    TR >= 0.70.

:- end_tests(open_source_software_volunteer_model_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62): High and increasing over the interval. At t=0 (1991), OSS was peripheral to commercial software, and volunteer contribution was genuinely motivated by community building and intrinsic satisfaction. At t=20 (2026), the same projects are critical infrastructure for financial systems, healthcare, military operations, and tech platforms — the extraction is massive because the value captured is massive. The increase from 0.35 → 0.62 reflects this trajectory. Suppression (0.68): Very high. The suppression mechanisms include: (1) Meritocratic ideology — 'your reputation is payment' — which naturalizes the extraction; (2) Identity fusion — maintainers cannot exit without psychological crisis; (3) Collective action barrier — distributed, anonymous contributors cannot coordinate to demand compensation; (4) Employment dependence — contributing to OSS is gambled-on career development, creating pressure to accept extraction in hope of future reward. Theater ratio (0.55): Moderate and rising. The rituals include: celebrating 'top contributors,' awarding merit badges, maintaining contributor codes of conduct, annual open source summits — performative recognition that masks the absence of material compensation. The theater has increased as the employment-pathway payoff has declined (piton degradation).
 *
 * PERSPECTIVAL GAP:
 *   The critical gap is between the powerless contributor's experience (maximum extraction) and the corporate adopter's experience (pure coordination benefit, negative extraction). The same structural arrangement — volunteers provide unpaid labor, corporations capture value — registers as snare from one position and rope from another. This is not a measurement problem or ambiguity — it is a genuine asymmetry in the constraint's operation. The beneficiary truly does experience it as coordination; the victim truly does experience it as extraction. The gap persists because the power differential (institutional vs powerless) and exit options (arbitrage vs trapped) are real structural facts, not observational artifacts. The maintainer's identity_locked position bridges these: structurally they could exit (making them more mobile than powerless contributors) but psychologically they cannot (making them trapped in a different way). The maintenance crisis over 2000–2026 shows that identity lock + psychological obligation + identity-constituted-through-project creates a burnout cascade that neither pure material extraction nor pure psychological fusion alone would produce.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) is determined by each agent's structural position: their power, exit options, and relationship to the extraction flow. Volunteer contributors with trapped exit options and powerless status experience d ≈ 0.95 (full targets). Maintainers with identity_locked exit options have d ≈ 0.89 (high targets but with some psychological mobility if identity frame shifts). Employed contributors with constrained exit and both beneficiary and victim status have d ≈ 0.55 (mixed). Corporations with arbitrage exit options and full beneficiary status have d ≈ 0.05 (full beneficiaries). The sigmoid f(d) transforms these to experienced extractiveness: f(0.95) ≈ 1.42, f(0.89) ≈ 1.28, f(0.55) ≈ 0.75, f(0.05) ≈ -0.12. Scope modifier σ(global) = 1.2, so χ = 0.62 × σ(d) × 1.2. For powerless contributors: χ ≈ 1.05, revealing they experience the constraint as highly extractive. For corporations: χ ≈ -0.09, revealing they experience it as cooperative (negative extraction = subsidy).
 *
 * MANDATROPHY ANALYSIS:
 *   DIAGNOSTIC EXEMPLAR OF HYBRID MECHANISMS: This constraint avoids mandatrophy (confusion between coordination and extraction) by precisely distinguishing them. The constraint genuinely contains both: real coordination benefits exist (standards, interoperability, reduced redundant development), alongside real asymmetric extraction (value capture without compensation). The classification as tangled_rope at the analytical level correctly captures this hybrid. The misclassification risk is the piton perspective — if the meritocratic ideology remains sufficiently functional, the constraint could be dismissed as 'just coordination with optional recognition.' But the measurable degradation of the meritocratic pathway (employment conversion rates declining, theater ratio rising) reveals the ideology as performative. The organizational responses (sustainability funding, codes of conduct, contributor recognition programs) are attempts to restore the functional connection between contribution and reward — they are scaffold-logic interventions in what has become primarily snare extraction. The mandatrophy is resolved by the perspectival decomposition: all six types are simultaneously true from their respective structural positions; the presheaf of perspectives over the observation site is the complete answer, not any single type.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    employment_conversion_rate,
    'What fraction of OSS contributors actually convert reputation into employment or economic reward, and how has this rate changed over time?',
    'Longitudinal survey data: track OSS contributor cohorts by contribution history; measure employment outcomes and income changes post-contribution',
    'If conversion rate > 30%: piton classification is incorrect, and meritocratic pathway may still be functional (scaffold). If conversion rate < 10%: piton confirmed, and ideology is performative. This determines whether the ''reputation reward'' suppression mechanism is real or mythological.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(employment_conversion_rate, empirical, 'Rate of employment/economic conversion for OSS contributors').

omega_variable(
    corporate_dependency_concentration,
    'What fraction of critical OSS projects have >50% of dependency traffic from a single corporation or top-5 corporations?',
    'Dependency graph analysis: measure traffic from corporate deployments vs community; identify single points of corporate dependency control',
    'If concentration > 60%: extraction is highly asymmetric and concentrated (snare confirmed). If concentration < 30%: extraction may be more diffuse (tangled rope from more perspectives).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(corporate_dependency_concentration, empirical, 'Corporate dependency concentration in critical OSS projects').

omega_variable(
    burnout_causation_mechanism,
    'Is OSS maintainer burnout primarily caused by volunteer labor volume (structural extraction) or by identity fusion preventing delegation (cognitive lock)?',
    'Post-burnout trajectory analysis: maintainers who exit vs those who restructure projects to distribute labor; measure whether psychological identity-lock persists after material exit',
    'If primarily structural: snare classification holds. If primarily identity fusion: identity_locked exit option is diagnostic. If both: decompose into separate constraints (labor extraction vs identity dynamics).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(burnout_causation_mechanism, empirical, 'Causal mechanism of OSS maintainer burnout').

omega_variable(
    sustainability_funding_scalability,
    'Can GitHub Sponsors, Tidelift, and Linux Foundation funding models scale to cover all critical OSS maintenance, or are they inherently limited to high-visibility projects?',
    'Funding distribution analysis: measure median sustainability funding across all critical projects vs required maintenance cost; track adoption rate over next 5 years',
    'If scalable: scaffold sunset is real, and alternative pathways can replace volunteer extraction within a generational timescale. If limited: scaffold perspective is aspirational, and pure volunteer extraction persists for invisible infrastructure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sustainability_funding_scalability, empirical, 'Scalability of OSS sustainability funding models').

omega_variable(
    free_rider_corporate_benefit,
    'What is the quantified commercial value captured by corporations using critical OSS projects without contributing maintenance resources?',
    'Economic analysis: shadow pricing of equivalent commercial software; comparison of corporate value capture vs funding contribution',
    'If value >> funding: asymmetric extraction is massive (snare confirmed with quantified magnitude). If value ≈ funding: tangled rope is more accurate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(free_rider_corporate_benefit, empirical, 'Quantified corporate benefit from free-riding on OSS').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(open_source_software_volunteer_model, 1991, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(oss_volunteer_tr_t0, open_source_software_volunteer_model, theater_ratio, 0, 0.35).
narrative_ontology:measurement(oss_volunteer_tr_t10, open_source_software_volunteer_model, theater_ratio, 10, 0.48).
narrative_ontology:measurement(oss_volunteer_tr_t20, open_source_software_volunteer_model, theater_ratio, 20, 0.55).

% Extraction over time
narrative_ontology:measurement(oss_volunteer_be_t0, open_source_software_volunteer_model, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(oss_volunteer_be_t10, open_source_software_volunteer_model, base_extractiveness, 10, 0.52).
narrative_ontology:measurement(oss_volunteer_be_t20, open_source_software_volunteer_model, base_extractiveness, 20, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(open_source_software_volunteer_model, information_standard).
narrative_ontology:affects_constraint(open_source_software_volunteer_model, digital_infrastructure_dependency).
narrative_ontology:affects_constraint(open_source_software_volunteer_model, technology_worker_precarity).
narrative_ontology:affects_constraint(open_source_software_volunteer_model, platform_developer_ecosystem_control).

% DUAL FORMULATION NOTE:
% The OSS volunteer model is upstream of several downstream constraints: corporations' dependency on OSS creates fragility if the volunteer base collapses (digital_infrastructure_dependency); unpaid OSS work subsidizes the tech industry and depresses wages (technology_worker_precarity); platform operators' control of OSS hosting (GitHub) creates additional extraction leverage (platform_developer_ecosystem_control). Each downstream constraint has its own ε; link them to reveal the full extraction architecture.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(open_source_software_volunteer_model, organized, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
