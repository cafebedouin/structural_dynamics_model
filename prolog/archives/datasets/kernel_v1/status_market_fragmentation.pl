% ============================================================================
% CONSTRAINT STORY: status_market_fragmentation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_status_market_fragmentation, []).

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
 *   constraint_id: status_market_fragmentation
 *   human_readable: Status Market Fragmentation
 *   domain: social/economic
 *
 * SUMMARY:
 *   Status market fragmentation describes the proliferation of
 *   non-interchangeable credentialing and certification systems that
 *   collectively govern professional recognition, employment access, and
 *   social status. A person seeking career advancement must navigate and
 *   maintain credentials across academic degrees, professional
 *   certifications, LinkedIn profiles, GitHub repositories, portfolios,
 *   social media metrics, and domain-specific credentials that are not
 *   mutually substitutable. The constraint exhibits mixed coordination and
 *   extraction characteristics: each credentialing system solves a genuine
 *   problem (communicating specialized competence to relevant audiences), but
 *   the lack of mutual recognition creates compounded signal requirements
 *   that fall unevenly on credential-seekers with limited initial capital.
 *   The rising theater ratio (0.42 → 0.64 over the interval) reflects
 *   credential work becoming increasingly performative — social media
 *   maintenance, portfolio curation, and credential accumulation with
 *   uncertain payoffs — rather than substantive skill demonstration. The
 *   fragmentation is sustained by incumbent credentialing operators
 *   (universities, certification boards, platforms) who benefit from monopoly
 *   rents and non-interoperability, and by established professionals who have
 *   already accumulated the required credential stacks and thus face lower
 *   marginal costs.
 *
 * KEY AGENTS:
 *   - Entry-level credential seekers: Primary victims (powerless/trapped) — must accumulate full credential stack simultaneously with no ability to substitute or exit
 *   - Career-change participants: Secondary victims (moderate/constrained) — face high switching costs when entering new fields with established credential requirements
 *   - Credentialing system operators: Primary beneficiaries (institutional/arbitrage) — universities, certification boards, LinkedIn, GitHub, portfolio platforms each extract value through monopoly position on their signal type
 *   - Established professionals with prior credentials: Secondary beneficiaries (powerful/mobile) — credential capital reduces fragmentation burden and creates barrier to entry for competitors
 *   - Traditional academic degree system: Piton actor (institutional/arbitrage) — degree was historically THE credential; now degraded from monopoly to one component in required stack, maintained through regulatory lock-in
 *   - Unified signal standardization movement: Organized countervailing force (organized/constrained) — industry coalitions and standards bodies building interoperable credential frameworks with sunset logic
 *   - Analytical observer: Civilizational risk (analytical/analytical) — risks naturalizing fragmentation as inherent complexity rather than incumbent-sustained extraction
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(status_market_fragmentation, 0.52).
domain_priors:suppression_score(status_market_fragmentation, 0.48).
domain_priors:theater_ratio(status_market_fragmentation, 0.64).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(status_market_fragmentation, extractiveness, 0.52).
narrative_ontology:constraint_metric(status_market_fragmentation, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(status_market_fragmentation, theater_ratio, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(status_market_fragmentation, tangled_rope).
narrative_ontology:human_readable(status_market_fragmentation, "Status Market Fragmentation").
narrative_ontology:topic_domain(status_market_fragmentation, "social/economic").

domain_priors:requires_active_enforcement(status_market_fragmentation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(status_market_fragmentation, credentialing_system_operators).
narrative_ontology:constraint_beneficiary(status_market_fragmentation, established_credential_holders).
narrative_ontology:constraint_victim(status_market_fragmentation, credential_seekers_without_initial_capital).
narrative_ontology:constraint_victim(status_market_fragmentation, workforce_participants_changing_fields).
narrative_ontology:constraint_victim(status_market_fragmentation, signal_verification_economy).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ENTRY-LEVEL CREDENTIAL SEEKER (SNARE) — Trapped in signal multiplication. Must acquire credentials across multiple non-interchangeable systems (degree + LinkedIn + portfolio + GitHub + certifications) with no ability to exit or substitute. Cannot signal competence through any single channel; employers demand the full credential stack. Theater ratio is high for this agent — much activity is performative credentialing (social media maintenance, portfolio curation) with uncertain payoff. Maximum suppression: cannot move forward without accumulating all signals simultaneously.
constraint_indexing:constraint_classification(status_market_fragmentation, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: CAREER-CHANGE PARTICIPANT (TANGLED ROPE) — Faces coordinated barriers (must prove competence in new field) but also benefits from existing credential ecosystem (can leverage prior degrees, professional reputation). Partially mobile — can acquire new credentials but at high cost (time, money, career interruption). Mixed experience: fragmentation is both a coordination mechanism (ensures serious commitment) and an extraction mechanism (requires redundant signaling).
constraint_indexing:constraint_classification(status_market_fragmentation, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: CREDENTIALING SYSTEM OPERATOR (ROPE) — Universities, certification boards, LinkedIn, GitHub, portfolio platforms each solve a genuine coordination problem: communicating specialized information about competence to relevant audiences. From this agent's perspective, the constraint is pure coordination (Rope) — each system specializes in a different signal type, and the diversity enables more nuanced hiring decisions. Net beneficiary through monopoly rents and fee extraction, but the coordination function is real.
constraint_indexing:constraint_classification(status_market_fragmentation, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: ESTABLISHED PROFESSIONAL WITH PRIOR CREDENTIALS (TANGLED ROPE) — Has initial credential capital (degree, professional network). Can leverage existing signals, reducing coordination burden. More mobile than entry-level seekers. But fragmentation still imposes costs (must maintain multiple profiles, LinkedIn recommendations, etc.). Low experienced extraction — the system coordinates legitimate skill signaling while extracting modest ongoing maintenance costs. Benefits from fragmentation: existing credentials create barrier to entry for competitors.
constraint_indexing:constraint_classification(status_market_fragmentation, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: TRADITIONAL ACADEMIC CREDENTIALING SYSTEM (PITON) — The degree was historically THE comprehensive signal of competence. That monopoly has eroded. Degrees persist through institutional inertia (employers still require them) but are supplemented, not replaced, by specialized signals. Theater ratio is rising (GPA, transcript quality matter less than portfolio and demonstrated skills) as the system degrades from its role as sole competence arbiter. Maintained by legal/regulatory lock-in (professional licensing) rather than genuine primacy.
constraint_indexing:constraint_classification(status_market_fragmentation, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: UNIFIED SIGNAL STANDARDIZATION MOVEMENT (SCAFFOLD) — Industry coalitions, standards bodies, and skills-based hiring advocates are building unified credential frameworks (e.g., industry certifications, competency-based hiring, open badge standards). This represents temporary coordination with a sunset: as standardization matures, the requirement to maintain multiple non-interchangeable signals diminishes. Theater is low for this perspective — the work is functional (setting technical standards) not performative. Extraction is minimal because the movement has agency and a clear exit criterion (interoperable standards adoption).
constraint_indexing:constraint_classification(status_market_fragmentation, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From civilizational perspective, signal fragmentation is inherent to complex labor markets. With many specialized fields, each domain develops its own competence markers (research papers for academia, code for engineering, publications for journalism, cases for law). Perfect mutual intelligibility is impossible — some fragmentation is a structural property of knowledge specialization. The constraint appears unchangeable from this vantage. However, the structural data may reveal this as a false summit: the fragmentation is amplified by incumbent capture and non-interoperability, not merely by irreducible complexity.
constraint_indexing:constraint_classification(status_market_fragmentation, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(status_market_fragmentation_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(status_market_fragmentation, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(status_market_fragmentation, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(status_market_fragmentation, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(status_market_fragmentation, TR),
    TR >= 0.70.

:- end_tests(status_market_fragmentation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The constraint imposes genuine coordination costs (must signal competence in each specialized domain) but also enables incumbent extraction (credentialing operators profit from non-interoperability and mandatory maintenance of multiple profiles). The trajectory from 0.38 → 0.52 reflects fragmentation deepening: as new signal types emerge (social media metrics, GitHub portfolios), the requirement to maintain previous signals does not diminish — it accumulates. Suppression (0.48): Moderate. Barriers to exit are real but surmountable: credential seekers cannot ignore the fragmented system without harming career prospects, but can select which credentials to prioritize based on field and target audience. Suppression is not total (unlike trapped agents); it is high-cost navigation, not absolute barrier. Theater ratio (0.64): Moderate-high. A substantial fraction of credential work is performative: LinkedIn profile maintenance, social media personal branding, portfolio curation, and credential accumulation with uncertain direct impact on hiring. However, the signaling is not entirely theater — portfolios and GitHub repositories do communicate real competence; the theater is in the performative maintenance required to keep signals current and visible. Rising trajectory reflects the system becoming increasingly theatrical as signal saturation increases: marginal credentials require more theatrical performance to achieve same visibility.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates sharp perspectival divergence depending on credential capital and structural position. Entry-level seekers experience maximum extraction and suppression (Snare): must acquire full credential stack with no exit option. Established professionals experience mixed coordination-extraction (Tangled Rope): fragmentation imposes maintenance costs but also protects their credential capital from devaluation. Credentialing system operators experience pure coordination (Rope): each platform solves a real problem and benefits from specialization. Traditional degree systems experience degradation (Piton): degrees persist through regulatory requirement and employer habit, not genuine primacy. Unified signal movement experiences temporary coordination with sunset (Scaffold): real structural work building interoperability standards with clear exit criterion. The civilizational analytical observer risks naturalizing fragmentation as inherent to complex labor markets (Mountain), but the structural data reveals this as false summit: the fragmentation is substantially amplified by incumbent lock-in and non-interoperability, not by irreducible complexity.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) varies sharply across perspectives based on power level, exit options, and relationship to extraction flow. Entry-level seekers with no prior credentials and trapped exit options experience d ≈ 0.92 (victims): all extraction flows toward credentialing system operators, none toward these agents. Credentialing operators with institutional power and arbitrage exit options experience d ≈ 0.08 (beneficiaries): extraction flows toward them, not away. Career-change participants (moderate/constrained) experience d ≈ 0.58 (partially targeted): mixed costs and benefits, constrained exit means they can navigate the system but at high cost. Established professionals (powerful/mobile) experience d ≈ 0.42 (partial beneficiary): their prior credentials reduce navigation costs, and they benefit from non-interoperability as barrier to entry. Directionality derivation for standardization movement: organized power level, constrained exit (they can exit through standards adoption but must actively work toward it), beneficiary status (successful standardization benefits participating institutions). The engine computes this as d ≈ 0.50 (symmetric) — the movement both bears costs (coordination work) and benefits (if standards succeed). Piton actors: institutional power, arbitrage exit (can theoretically exit through devaluation, but tied to regulatory requirement), beneficiary status (extract value from degree monopoly even as system degrades). Engine computes d ≈ 0.25 — low extraction because the primary function has atrophied and the constraint is maintained by inertia, not active extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   Mandatrophy is partially resolved by the decomposition into constraint family: if fragmentation were simply Rope (pure coordination), entry-level seekers would not experience suppression > 0.35 and theater > 0.42 — they would optimize which signals to acquire. The high suppression and theater among powerless agents reveals that fragmentation is not pure coordination; it is mixed. The Tangled Rope classification captures this: genuine coordination function (each system communicates specialized competence) mixed with extraction mechanism (non-interoperability creates compounded requirements and rents for operators). Resolution path: fragmentation becomes pure Rope (low suppression, low theater) if interoperability standards mature and entry-level seekers can substitute credentials across systems. Fragmentation becomes pure Snare if consolidation fails and theater/suppression continue rising. Current state at ε=0.52 is genuinely tangled: neither pure coordination nor pure extraction, but a mixture that shifts based on agent capital and field.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    fragmentation_necessity_vs_incumbent_lock,
    'Is status market fragmentation a necessary consequence of knowledge specialization, or is it artificially amplified by incumbent credentialing systems defending market share?',
    'Comparative analysis: jurisdictions with unified credentialing frameworks (e.g., some EU countries with EQF mutual recognition) vs. fragmented systems (US, UK); measurement of how much fragmentation reduces when interoperability standards are enforced; counterfactual: what would signal consolidation look like if LinkedIn, universities, GitHub, and industry boards had compatible metrics?',
    'If necessary: fragmentation is a rope-type coordination cost, unavoidable in complex labor markets. If amplified by lock-in: fragmentation is a tangled_rope or snare-type extraction mechanism, potentially resolvable through regulatory interoperability mandates.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fragmentation_necessity_vs_incumbent_lock, empirical, 'Whether fragmentation is inherent or amplified by incumbent capture').

omega_variable(
    credential_stacking_switching_costs,
    'What is the actual switching cost for an individual to migrate from one dominant credentialing system to another (e.g., from degree-primary to portfolio-primary)?',
    'Longitudinal tracking of career transitions; measurement of time/cost required to establish credibility in new systems; analysis of rejection rates for candidates without complete credential stacks vs. those with deep credentials in one system.',
    'High switching costs (> 2 years, substantial financial outlay) confirm snare-like suppression for entry-level seekers. Low switching costs suggest false positives in snare classification and indicate system is more mobile than structured.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(credential_stacking_switching_costs, empirical, 'Switching costs for credential system migration').

omega_variable(
    mutual_recognition_technical_feasibility,
    'Are the technical barriers to mutual recognition of credentials across systems (academic degree <-> professional certification <-> portfolio assessment) primarily technical or primarily driven by incumbent resistance?',
    'Analysis of existing interoperability standards (blockchain credentials, verifiable credentials, mutual recognition treaties); comparison of coordination costs in sectors with active standardization (finance, healthcare) vs. fragmented sectors (tech, creative); interviews with standards-body participants about rate-limiting factors.',
    'If technical barriers dominate: scaffold perspective is correct and sunset is achievable within 10-15 years. If incumbent resistance dominates: fragmentation is sustained by extraction mechanisms, and scaffold perspective is aspirational rather than structural.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mutual_recognition_technical_feasibility, empirical, 'Technical vs. incumbent barriers to credential mutual recognition').

omega_variable(
    theater_ratio_composition,
    'Of the time credential seekers invest in the status market, what fraction is genuinely communicating competence (functional signal) vs. performing credentialing identity (theatrical maintenance)?',
    'Time-use surveys and work diaries from job seekers; analysis of credential impact: correlation between marginal credentials (additional GitHub stars, extra LinkedIn endorsements) and actual hiring outcomes; measurement of ''signal decay'' (how quickly outdated credentials are abandoned).',
    'If theater is < 0.40: fragmentation is more coordination than extraction; reduce theater_ratio estimate and reclassify perspectives toward rope. If theater is > 0.75: fragmentation is primarily performative; increase snare and piton classifications.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(theater_ratio_composition, empirical, 'Proportion of credential work that is theatrical vs. functional signaling').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(status_market_fragmentation, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(smf_tr_t0, status_market_fragmentation, theater_ratio, 0, 0.42).
narrative_ontology:measurement(smf_tr_t5, status_market_fragmentation, theater_ratio, 5, 0.58).
narrative_ontology:measurement(smf_tr_t10, status_market_fragmentation, theater_ratio, 10, 0.64).

% Extraction over time
narrative_ontology:measurement(smf_be_t0, status_market_fragmentation, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(smf_be_t5, status_market_fragmentation, base_extractiveness, 5, 0.46).
narrative_ontology:measurement(smf_be_t10, status_market_fragmentation, base_extractiveness, 10, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(smf_su_t0, status_market_fragmentation, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(smf_su_t5, status_market_fragmentation, suppression_requirement, 5, 0.42).
narrative_ontology:measurement(smf_su_t10, status_market_fragmentation, suppression_requirement, 10, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(status_market_fragmentation, identity_coordination).
narrative_ontology:boltzmann_floor_override(status_market_fragmentation, 0.12).
narrative_ontology:affects_constraint(status_market_fragmentation, educational_credential_inflation).
narrative_ontology:affects_constraint(status_market_fragmentation, platform_monopoly_lock_in).
narrative_ontology:affects_constraint(status_market_fragmentation, professional_licensing_gatekeeping).

% DUAL FORMULATION NOTE:
% Status market fragmentation is downstream of decisions by individual credentialing systems (educational institutions, certification boards, platforms) to maintain non-interoperable signal types. It is upstream of labor market outcomes: fragmentation affects hiring, wage dispersion, and career mobility. The constraint family includes three related stories: (1) educational_credential_inflation (ε=0.38, Tangled Rope) — degree multiplication and rising requirements driven by signaling arms race; (2) platform_monopoly_lock_in (ε=0.61, Snare) — individual platforms (LinkedIn, GitHub, etc.) each creating single-point dependencies; (3) professional_licensing_gatekeeping (ε=0.58, Snare) — regulatory capture by established professions using credential requirements as market protection. Status_market_fragmentation (ε=0.52) integrates across all three: it is the emergent property when all three operate simultaneously without mutual recognition.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(status_market_fragmentation, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
