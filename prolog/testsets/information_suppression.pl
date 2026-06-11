% ============================================================================
% CONSTRAINT STORY: information_suppression
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-10
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_information_suppression, []).

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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: information_suppression
 *   human_readable: Structural Context Suppression in Event-Driven Journalism
 *   domain: political_economy/democratic_theory/media_systems
 *
 * SUMMARY:
 *   Event-driven journalism systematically omits the structural context
 *   required for voters to estimate policy stakes, creating an information
 *   asymmetry that enables persistently cheap votes and rent extraction by
 *   organized interests. This constraint operates as a tangled rope: it
 *   provides genuine coordination (rapid information distribution, collective
 *   response to salient crises) while simultaneously extracting through
 *   systematic context suppression that prevents accurate stake pricing. The
 *   constraint requires active enforcement through editorial policies,
 *   professional norms, competitive dynamics, and business model pressures.
 *   The theater ratio (0.58) reflects the atrophy of the 'objectivity' norm
 *   from functional coordination mechanism to performative ritual:
 *   journalists perform objectivity (event focus, both-sides framing) while
 *   the function (enabling voters to distinguish fact from propaganda) has
 *   been undermined by the very practices the norm mandates. The suppression
 *   trajectory shows enforcement intensification from 1980-2010 (rising from
 *   0.55 to 0.72) as competitive pressures and advertising-driven business
 *   models hardened, followed by slight decay (0.70 by 2020) as alternative
 *   platforms began providing structural context to niche audiences. The
 *   extractiveness trajectory shows accumulation from 1980-2010 (rising from
 *   0.45 to 0.68) as the information asymmetry enabled increasing rent
 *   extraction, then plateauing as the mechanism reached its natural limit
 *   (further extraction would trigger voter backlash or alternative platform
 *   growth).
 *
 * KEY AGENTS:
 *   - Unorganized Voters: Primary victim (powerless/identity_locked) — systematically misprice policy stakes due to structural context omission; identity-fused with event-driven news consumption as civic participation; cannot organize to demand different coverage
 *   - Policy-Affected Populations: Secondary victim (moderate/constrained) — groups directly affected by policy outcomes; can seek specialized information at cost but daily structural context remains suppressed; experience both coordination and extraction
 *   - Incumbent Elites: Primary beneficiary (institutional/arbitrage) — access structural analysis through private channels while general information environment suppresses context; the information asymmetry enables cheap votes and rent extraction
 *   - Media Organizations: Mixed beneficiary-victim (institutional/constrained) — event-driven journalism is both business model (coordination) and competitive trap (extraction); constrained by dynamics that punish structural context provision
 *   - Political Consultants: Secondary beneficiary (institutional/arbitrage) — exploit predictable attention cycles created by event-driven coverage for strategic timing of policy moves and messaging
 *   - Alternative Media Ecosystem: Organized agents (organized/mobile) — Substack, podcasts, investigative nonprofits building alternative pathways with explicit structural context provision; see constraint as temporary with sunset logic
 *   - Objectivity Norm: Institutional performance (institutional/arbitrage) — professional journalism norm that has atrophied from coordination function to theater; maintained through institutional inertia despite functional failure
 *   - Democratic Accountability Mechanism: Abstract victim (powerless/trapped) — the collective good of democratic accountability cannot organize or exit; bears full cost of information asymmetry
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(information_suppression, 0.68).
domain_priors:suppression_score(information_suppression, 0.72).
domain_priors:theater_ratio(information_suppression, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(information_suppression, extractiveness, 0.68).
narrative_ontology:constraint_metric(information_suppression, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(information_suppression, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(information_suppression, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(information_suppression, resistance, 0.42).

% --- Constraint claim ---
narrative_ontology:constraint_claim(information_suppression, tangled_rope).
narrative_ontology:human_readable(information_suppression, "Structural Context Suppression in Event-Driven Journalism").
narrative_ontology:topic_domain(information_suppression, "political_economy/democratic_theory/media_systems").

domain_priors:requires_active_enforcement(information_suppression).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(information_suppression, incumbent_elites).
narrative_ontology:constraint_beneficiary(information_suppression, media_organizations).
narrative_ontology:constraint_beneficiary(information_suppression, political_consultants).
narrative_ontology:constraint_victim(information_suppression, unorganized_voters).
narrative_ontology:constraint_victim(information_suppression, policy_affected_populations).
narrative_ontology:constraint_victim(information_suppression, democratic_accountability_mechanism).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(information_suppression, policy_affected_populations).
narrative_ontology:constraint_vindicates(information_suppression, rational_ignorance_doctrine).
narrative_ontology:constraint_vindicates(information_suppression, median_voter_theorem).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Consume event-driven news as civic participation. Systematically misprice policy stakes due to structural context omission. Cannot organize to demand different coverage. Identity-fused with news consumption as 'staying informed' — exit would require abandoning the frame that equates daily news cycles with civic engagement. Bear the extraction cost: cheap votes enable rent extraction by organized interests.
narrative_ontology:constraint_stakeholder(information_suppression, unorganized_voters, payer,
    powerless, biographical, identity_locked, national).

% Groups directly affected by policy outcomes (healthcare users, environmental regulation targets, labor market participants). Can seek specialized information sources at cost (policy journals, advocacy briefings) but daily structural context remains suppressed in accessible media. Experience both coordination (event coverage enables some collective response to salient crises) and extraction (systematic underpricing of stakes in non-crisis periods enables adverse policy accumulation).
narrative_ontology:constraint_stakeholder(information_suppression, policy_affected_populations, payer,
    moderate, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(information_suppression, policy_affected_populations, beneficiary).

% Access structural analysis through private briefings, think tanks, and policy networks while the general information environment suppresses context. Exploit predictable attention cycles created by event-driven coverage for strategic timing of policy moves. The information asymmetry is the mechanism through which cheap votes are maintained, enabling rent extraction that would be prohibitively expensive if voters could accurately price policy stakes.
narrative_ontology:constraint_stakeholder(information_suppression, incumbent_elites, beneficiary,
    institutional, immediate, arbitrage, national).

% Event-driven journalism is both business model (enables rapid information distribution, audience aggregation, advertising revenue) and competitive trap (suppresses structural context that would enable audience to demand different coverage). Constrained by competitive dynamics that punish structural context provision: outlets that provide structural context lose audience to more event-focused competitors. Mixed position: benefit from the business model, constrained by the competitive dynamics.
narrative_ontology:constraint_stakeholder(information_suppression, media_organizations, agenda_setter,
    institutional, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(information_suppression, media_organizations, beneficiary).

% Exploit predictable attention cycles created by event-driven coverage for strategic timing of policy moves and messaging. Access structural analysis through professional networks. Benefit from information asymmetry: can craft messages that exploit voter stake-mispricing without being constrained by structural context provision.
narrative_ontology:constraint_stakeholder(information_suppression, political_consultants, beneficiary,
    institutional, immediate, arbitrage, national).

% Substack, podcast networks, investigative nonprofits, academic public engagement building alternative information pathways with explicit structural context provision. See the constraint as temporary: platform economics and audience fragmentation are creating viable business models for structural journalism that bypass event-driven suppression. Building outside the constraint rather than being extracted from within it.
narrative_ontology:constraint_stakeholder(information_suppression, alternative_media_ecosystem, observer,
    organized, generational, mobile, global).

% Abstract collective good of democratic accountability. Cannot organize or exit. Bears full cost of information asymmetry: when voters systematically misprice policy stakes, democratic accountability fails — policies that harm the majority can persist because the harm is not legible in the information environment. Not an agent (cannot collect rents) but included for narrative completeness.
narrative_ontology:constraint_stakeholder(information_suppression, democratic_accountability_mechanism, excluded,
    powerless, civilizational, trapped, national).
narrative_ontology:stakeholder_non_agent(information_suppression, democratic_accountability_mechanism).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(information_suppression, incumbent_elites).
narrative_ontology:fixing_cost_class(information_suppression, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Event-driven journalism aggregates and distributes information rapidly, enabling collective response to salient crises. When events become sufficiently dramatic (natural disasters, political scandals, economic shocks), the coverage does coordinate public attention and enable some collective action.
% TRANSFER_FUNCTION: The arrangement moves attention, political capital, and ultimately policy outcomes. From: unorganized voters who systematically misprice stakes due to context suppression. To: incumbent elites who exploit the information asymmetry to maintain cheap votes and extract rents through policies that would be prohibitively expensive if stakes were accurately estimated. The transfer mechanism is the systematic omission of structural context in accessible media, which prevents voters from estimating policy stakes accurately.
% ABSENT_VOICES: Policy-affected populations who would demand structural context if they understood the stake-mispricing mechanism. Investigative journalists who see the constraint but are constrained by business model pressures. Academic researchers who provide structural analysis but lack accessible distribution channels. These voices are not entirely absent (they exist in alternative platforms, long-form journalism, academic publications) but they are systematically excluded from the dominant information environment that shapes median voter understanding. The exclusion is structural (competitive dynamics, business model constraints) rather than conspiratorial.
% DISAPPEARANCE_RATIONALE: If event-driven journalism disappeared overnight and was replaced by structural context provision, the political economy would rearrange substantially. Votes would become more expensive (voters could accurately price policy stakes), rent extraction by organized interests would become more difficult (policies that harm the majority would be legible), and the coalition structure of politics would shift (policy-affected populations could organize around accurate stake estimation rather than event-driven salience). The information asymmetry is a necessary condition for persistently cheap votes — remove it and the extraction mechanism fails.
% FOUNDING_PROBLEM: The founding problem was information aggregation and distribution at scale in a pre-digital era. Event-driven journalism solved the genuine problem of how to collect, verify, and distribute information rapidly to a mass audience when information collection was expensive and distribution channels were scarce (printing presses, broadcast licenses). The event-focus was a rational response to the constraint: structural context is expensive to produce and difficult to verify, while event coverage is cheap to produce and easy to verify (the event either happened or it didn't).
% FOUNDING_PROBLEM_CORROBORATION: The founding problem (information aggregation and distribution at scale) is dead: digital platforms have made information collection and distribution nearly costless, and alternative media ecosystems demonstrate that structural context provision is economically viable at scale. The constraint persists not because the founding problem remains but because the institutional arrangements built to solve it (advertising-driven business models, professional norms, competitive dynamics) now serve a different function: maintaining information asymmetry that enables cheap votes and rent extraction. Corroboration: media economics research (declining marginal cost of digital distribution), alternative platform growth (Substack, podcasts demonstrating viability of structural journalism), and natural experiments on information-environment changes (Fox News rollout effects, platform algorithm changes) all show that the founding problem is solved but the institutional arrangements persist.
narrative_ontology:disappearance_verdict(information_suppression, world_rearranges).
narrative_ontology:founding_problem_status(information_suppression, dead).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: UNORGANIZED VOTER (SNARE) — Identity-locked rather than trapped: structurally mobile (can seek alternative information sources, change media consumption) but identity-fused with event-driven news consumption as civic participation. Exit would require abandoning the identity frame that equates 'staying informed' with consuming daily news cycles. Experiences maximum extraction: systematically misprices policy stakes due to structural context omission, cannot organize to demand different coverage, and the mispricing directly enables rent extraction by organized interests.
constraint_indexing:constraint_classification(information_suppression, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(national))).

% PERSPECTIVE 2: POLICY-AFFECTED POPULATION (TANGLED ROPE) — Groups directly affected by policy outcomes (healthcare users, environmental regulation targets, labor market participants) face constrained exit: can seek specialized information sources at cost (policy journals, advocacy organization briefings) but daily structural context remains suppressed in accessible media. Experience both coordination (event coverage enables some collective response to salient crises) and extraction (systematic underpricing of stakes in non-crisis periods enables adverse policy accumulation). Mixed position: organized enough to have some voice, not organized enough to reshape information environment.
constraint_indexing:constraint_classification(information_suppression, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: INCUMBENT ELITE (ROPE) — Primary beneficiary with arbitrage-grade exit: can access structural analysis through private briefings, think tanks, and policy networks while the general information environment suppresses context. Experiences the constraint as coordination: event-driven coverage creates predictable attention cycles that enable strategic timing of policy moves. Net beneficiary — the information asymmetry is the mechanism through which cheap votes are maintained, enabling rent extraction that would be prohibitively expensive if voters could accurately price policy stakes.
constraint_indexing:constraint_classification(information_suppression, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: MEDIA ORGANIZATION (TANGLED ROPE) — Institutional actor with constrained exit: event-driven journalism is both a genuine coordination mechanism (enables rapid information distribution, audience aggregation, advertising revenue) and an extraction mechanism (suppresses structural context that would enable audience to demand different coverage, locks in business model that depends on attention-maximizing rather than stake-clarifying content). Mixed beneficiary-victim: benefits from the business model, constrained by the competitive dynamics that punish structural context provision (audience loss to more event-focused competitors).
constraint_indexing:constraint_classification(information_suppression, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: ALTERNATIVE MEDIA ECOSYSTEM (SCAFFOLD) — Organized agents (Substack, podcast networks, investigative nonprofits, academic public engagement) building alternative information pathways with explicit structural context provision. See the constraint as temporary: platform economics and audience fragmentation are creating viable business models for structural journalism that bypass event-driven suppression. Sunset logic: as alternative platforms mature and audience willingness-to-pay for context increases, the event-driven monopoly loses force. Estimated timeline: 10-20 years for alternative models to reach scale sufficient to reshape median voter information environment.
constraint_indexing:constraint_classification(information_suppression, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: OBJECTIVITY NORM (PITON) — The professional journalism norm of 'objectivity' (report events without interpretation, present both sides, avoid structural claims) has atrophied from coordination function to performance. Originally solved a real problem (partisan press unreliability, yellow journalism), but now maintained theatrically while its function (enabling voters to distinguish fact from propaganda) has been undermined by the very event-focus it mandates. The norm persists through institutional inertia and professional identity, not because it delivers epistemic reliability. Theater ratio reflects this: journalists perform objectivity rituals (both-sides framing, event focus) that systematically suppress the structural context required for stake estimation.
constraint_indexing:constraint_classification(information_suppression, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (TANGLED ROPE) — From the analytical position, this constraint exhibits both genuine coordination (event-driven journalism does aggregate and distribute information rapidly, enabling collective response to salient crises) and substantial extraction (systematic suppression of structural context creates information asymmetry that enables persistently cheap votes and rent extraction by organized interests). The coordination function is real but insufficient to offset the extraction mechanism. Not a mountain: the constraint is contingent on specific institutional arrangements (advertising-driven business models, professional norms, platform economics) rather than inherent to information distribution. Not a snare: the coordination function is genuine, not cover. Tangled rope: both functions coexist and require active enforcement (editorial policies, professional socialization, competitive dynamics) to maintain.
constraint_indexing:constraint_classification(information_suppression, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(information_suppression_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(information_suppression, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(information_suppression, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(information_suppression, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(information_suppression, TR),
    TR >= 0.70.

:- end_tests(information_suppression_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. The systematic suppression of structural context creates information asymmetry that enables substantial rent extraction by organized interests. Voters systematically misprice policy stakes, enabling policies that would be prohibitively expensive if stakes were accurately estimated. The extraction is not total (0.68 rather than 0.85+) because some structural information does reach voters through alternative channels, crisis-driven coverage spikes, and accumulated personal experience. Suppression (0.72): High. Multiple mechanisms enforce context suppression: advertising-driven business models reward attention-maximizing over stake-clarifying content; competitive dynamics punish structural context provision (audience loss to event-focused competitors); professional norms (objectivity as event-focus) are internalized through journalism education and newsroom socialization; platform algorithms reward engagement over comprehension. Suppression is not total because alternative platforms exist and some mainstream outlets provide structural context in long-form or investigative pieces. Theater ratio (0.58): Moderate-high. The objectivity norm is substantially performative: journalists perform event-focus and both-sides framing as 'objectivity' while these practices systematically suppress the structural context required for the norm's stated function (enabling voters to distinguish fact from propaganda). The performance persists through professional identity and institutional inertia, not because it delivers epistemic reliability. Accessibility collapse (0.35): Low-moderate. Alternatives to event-driven journalism exist and are accessible (alternative platforms, investigative nonprofits, policy journals, academic public engagement), but they require active search and higher engagement costs. The constraint does not collapse alternatives completely, but it does make structural context systematically less accessible than event coverage. Resistance (0.42): Moderate. The constraint meets real resistance from alternative media ecosystems, investigative journalism, media criticism, and audience demand for structural context. The resistance is not negligible but is insufficient to reshape the dominant information environment.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates how the same structural phenomenon — systematic omission of structural context in news coverage — appears differently from different structural positions. Unorganized voters experience it as a snare: they are identity-locked into event-driven consumption, systematically misprice stakes, and cannot organize to demand different coverage. Policy-affected populations experience it as tangled rope: they get some coordination benefit from event coverage but bear extraction costs from context suppression. Incumbent elites experience it as rope: the information asymmetry is a coordination mechanism that enables predictable attention cycles and cheap votes. Media organizations experience it as tangled rope: event-driven journalism is both their business model and their competitive trap. The alternative media ecosystem sees it as scaffold: a temporary problem being solved by platform economics and audience fragmentation. The objectivity norm sees itself as piton: a degraded ritual maintained through inertia. The analytical observer sees tangled rope: genuine coordination coexisting with substantial extraction, both requiring active enforcement. The perspectival gap is not 'which type is correct?' but 'which structural position are you measuring from?' — the presheaf over the observation site is the answer.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is derived from beneficiary/victim declarations and exit options. Unorganized voters are victims with identity_locked exit: high d (approaching 1.0) because they bear the extraction cost (systematic stake mispricing) and cannot exit without abandoning their identity frame (news consumption as civic participation). Policy-affected populations are victims with constrained exit: moderate-high d (0.6-0.7 range) because they bear extraction costs but have some agency to seek alternative information at cost. Incumbent elites are beneficiaries with arbitrage exit: low d (approaching 0.0) because they capture the benefit (cheap votes enabled by information asymmetry) and can access structural analysis through private channels. Media organizations are mixed beneficiary-victim with constrained exit: moderate d (0.4-0.5 range) because they benefit from the business model but are constrained by competitive dynamics. Political consultants are beneficiaries with arbitrage exit: low d because they exploit the predictable attention cycles. The alternative media ecosystem is neither clear beneficiary nor victim, with mobile exit: near-zero d because they are building outside the constraint rather than being extracted from within it. The objectivity norm is institutional with arbitrage exit but is a performance rather than an agent: its d is not meaningful for extraction calculation. Democratic accountability mechanism is a victim with trapped exit: maximum d because it is an abstract collective good that cannot organize or exit.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by demonstrating that tangled rope is the correct classification from the analytical perspective: the constraint exhibits both genuine coordination (rapid information distribution, collective response to salient crises) and substantial extraction (systematic context suppression enabling cheap votes and rent extraction). The coordination function is real — event-driven journalism does solve the problem of aggregating and distributing information rapidly, enabling collective response when crises become salient. The extraction function is also real — the systematic suppression of structural context creates information asymmetry that enables persistently cheap votes, which in turn enables rent extraction by organized interests that would be prohibitively expensive if voters could accurately price policy stakes. Both functions coexist and require active enforcement: editorial policies mandate event-focus, professional norms internalize objectivity-as-event-focus, competitive dynamics punish structural context provision, and business models reward attention-maximizing over stake-clarifying content. The constraint is not a mountain (it is contingent on specific institutional arrangements, not inherent to information distribution), not a rope (the extraction is too substantial to be mere coordination overhead), not a snare (the coordination function is genuine, not cover), not a scaffold (no sunset clause or transitional logic in the dominant institutional arrangement, though alternative platforms may eventually provide one), and not a piton (the coordination function, while degraded, is not purely performative — event coverage does enable some collective response). Tangled rope is the classification that captures both the genuine coordination and the substantial extraction, both requiring active enforcement to maintain.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    structural_context_threshold,
    'What density of structural context in news coverage is sufficient to enable accurate stake estimation by median voters?',
    'Experimental manipulation of context density in news coverage; measurement of stake-pricing accuracy (willingness-to-pay for policy outcomes) by information-environment type; natural experiments on platform migrations and information-environment changes.',
    'If threshold is low (achievable within event-driven constraints): the extraction mechanism is weaker than claimed, and the constraint is closer to rope. If threshold is high (requires dedicated structural journalism): the extraction mechanism is as severe as claimed, and the constraint is tangled rope or snare from more perspectives.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(structural_context_threshold, empirical, 'Structural context density threshold for accurate stake estimation').

omega_variable(
    alternative_platform_viability,
    'Can alternative media platforms (Substack, podcasts, investigative nonprofits) reach scale sufficient to reshape median voter information environment, or do they serve only high-engagement niches?',
    'Longitudinal tracking of alternative platform audience growth; measurement of structural context density in alternative vs mainstream coverage; analysis of audience overlap and information-environment switching costs.',
    'If alternative platforms reach median voter scale: scaffold perspective confirmed, sunset is real. If alternative platforms remain niche: scaffold perspective is aspirational, and the constraint persists as tangled rope or snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_platform_viability, empirical, 'Whether alternative platforms can reshape median voter information environment').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (competitive dynamics, business model constraints, professional norms) or internalized (journalists and audiences have fused their identities with event-driven coverage as ''real journalism'')?',
    'Post-intervention suppression trajectory: if structural barriers are removed (e.g., nonprofit funding eliminates advertising pressure) but event-focus persists, reclassify as partially internalized. Survey journalists and audiences on their framing of ''good journalism'' — do they describe structural context provision as desirable but impractical (structural suppression) or as advocacy/bias (internalized suppression)?',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests — journalists and audiences carry the suppression with them even when external barriers are removed. If purely structural, removing barriers (alternative business models, platform changes) would rapidly shift coverage patterns.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Whether suppression is structural or internalized').

omega_variable(
    cheap_votes_counterfactual,
    'Would votes become prohibitively expensive for incumbent elites if structural context were provided, or would organized interests adapt through alternative rent-extraction mechanisms?',
    'Natural experiments on information-environment shocks (Fox News rollout, platform algorithm changes, investigative journalism surges); measurement of policy outcome changes and rent-extraction patterns before/after information shocks; analysis of elite adaptation strategies.',
    'If votes become expensive: the information asymmetry is the binding constraint on rent extraction, confirming the tangled rope classification. If elites adapt: the information asymmetry is one mechanism among many, and the constraint is less central to the extraction structure than claimed.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(cheap_votes_counterfactual, empirical, 'Whether structural context provision would make votes prohibitively expensive').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(information_suppression, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(infosup_theater_1980, information_suppression, theater_ratio, 0, 0.35).
narrative_ontology:measurement(infosup_theater_1990, information_suppression, theater_ratio, 10, 0.42).
narrative_ontology:measurement(infosup_theater_2000, information_suppression, theater_ratio, 20, 0.5).
narrative_ontology:measurement(infosup_theater_2010, information_suppression, theater_ratio, 30, 0.58).
narrative_ontology:measurement(infosup_theater_2020, information_suppression, theater_ratio, 40, 0.58).

% Extraction over time
narrative_ontology:measurement(infosup_extract_1980, information_suppression, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(infosup_extract_1990, information_suppression, base_extractiveness, 10, 0.52).
narrative_ontology:measurement(infosup_extract_2000, information_suppression, base_extractiveness, 20, 0.61).
narrative_ontology:measurement(infosup_extract_2010, information_suppression, base_extractiveness, 30, 0.68).
narrative_ontology:measurement(infosup_extract_2020, information_suppression, base_extractiveness, 40, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(infosup_suppress_1980, information_suppression, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(infosup_suppress_1990, information_suppression, suppression_requirement, 10, 0.6).
narrative_ontology:measurement(infosup_suppress_2000, information_suppression, suppression_requirement, 20, 0.68).
narrative_ontology:measurement(infosup_suppress_2010, information_suppression, suppression_requirement, 30, 0.72).
narrative_ontology:measurement(infosup_suppress_2020, information_suppression, suppression_requirement, 40, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(information_suppression, information_standard).

% DUAL FORMULATION NOTE:
% This constraint is downstream of organization_floor (the structural barrier to collective action by unorganized voters). The organization floor is a mountain (unchangeable coordination cost of collective action at scale); information_suppression is a tangled rope (contingent institutional arrangement that exploits the organization floor to maintain information asymmetry). The two constraints are structurally distinct: organization_floor would persist even with perfect information provision (collective action is hard regardless of information environment), while information_suppression is contingent on specific media institutions and business models. However, information_suppression depends on organization_floor: if voters could organize costlessly, they could demand structural context provision and reshape the information environment. The organization floor makes information_suppression sustainable by preventing the collective action required to challenge it.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(information_suppression, institutional, 0.45).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
