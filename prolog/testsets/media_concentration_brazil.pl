% ============================================================================
% CONSTRAINT STORY: media_concentration_brazil
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_media_concentration_brazil, []).

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
 *   constraint_id: media_concentration_brazil
 *   human_readable: Media Concentration and Political-Economic Extraction in Brazil
 *   domain: political_economy/media_ownership
 *
 * SUMMARY:
 *   Brazil's media landscape exhibits extreme concentration: Globo
 *   (Organizações Globo) controls approximately 30-40% of television audience
 *   share and dominates news production; Record and SBT each control 15-20%.
 *   Regional and independent outlets collectively hold less than 20% of
 *   audience reach. This concentration is maintained through a combination of
 *   capital barriers (broadcast infrastructure costs), regulatory capture
 *   (ANCINE and ANATEL enforcement inconsistency), state advertising
 *   concentration (federal and state governments direct advertising spend
 *   toward conglomerate-friendly outlets), and political-elite symbiosis
 *   (media owners benefit from access and favorable coverage; politicians
 *   benefit from agenda control). The constraint exhibits characteristics of
 *   both pure extraction (Snare from powerless journalists and marginalized
 *   communities) and hybrid coordination-extraction (Tangled Rope from
 *   regional outlets and political elites). The scaffold perspective reflects
 *   emerging digital alternatives (YouTube channels, podcasts, independent
 *   news sites, social media) that are beginning to bypass conglomerate
 *   gatekeeping, though algorithmic curation and resource barriers persist.
 *   Theater ratio has increased as regulatory agencies maintain formal
 *   oversight rules while selectively enforcing them, creating a performative
 *   regulatory ritual that appears to constrain conglomerates but in practice
 *   enables them.
 *
 * KEY AGENTS:
 *   - Major Media Conglomerates (Globo, Record, SBT): Primary beneficiaries (institutional/arbitrage) — capture advertising revenue, political access, regulatory favors; extract favorable framing and regulatory leniency
 *   - Independent Journalists: Primary victims (powerless/trapped) — face distribution barriers, advertising gatekeeping, precarious economics; forced into conglomerate employment or exit from profession
 *   - Marginalized Communities (Rural/Favela): Primary victims (powerless/trapped) — have no voice in dominant narratives; conglomerate framing determines political and resource allocation outcomes
 *   - Regional Media Outlets: Secondary victims (moderate/constrained) — bear advertising gatekeeping, audience suppression, digital transition capital requirements while benefiting from industry coordination mechanisms
 *   - Political Elites: Symbiotic agents (powerful/mobile) — both extract from and provide extraction to conglomerates; maintain mutual benefit through electoral advertising, regulatory control, and favorable coverage
 *   - Digital Native and Alternative Media Coalition: Organized challengers (organized/constrained) — building parallel distribution pathways with genuine sunset logic as digital infrastructure matures
 *   - Media Regulatory Agencies (ANCINE, ANATEL): Captured institutional actors (institutional/arbitrage) — maintain performative oversight; selectively enforce rules favoring politically-aligned owners
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent capital-intensive infrastructure as immutable economic law
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(media_concentration_brazil, 0.58).
domain_priors:suppression_score(media_concentration_brazil, 0.68).
domain_priors:theater_ratio(media_concentration_brazil, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(media_concentration_brazil, extractiveness, 0.58).
narrative_ontology:constraint_metric(media_concentration_brazil, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(media_concentration_brazil, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(media_concentration_brazil, tangled_rope).
narrative_ontology:human_readable(media_concentration_brazil, "Media Concentration and Political-Economic Extraction in Brazil").
narrative_ontology:topic_domain(media_concentration_brazil, "political_economy/media_ownership").

domain_priors:requires_active_enforcement(media_concentration_brazil).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(media_concentration_brazil, major_media_conglomerates).
narrative_ontology:constraint_beneficiary(media_concentration_brazil, political_elites_aligned_with_owners).
narrative_ontology:constraint_victim(media_concentration_brazil, independent_journalists).
narrative_ontology:constraint_victim(media_concentration_brazil, marginalized_communities).
narrative_ontology:constraint_victim(media_concentration_brazil, democratic_information_integrity).
narrative_ontology:constraint_victim(media_concentration_brazil, local_media_outlets).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: INDEPENDENT JOURNALIST (SNARE) — Trapped by structural barriers: limited access to distribution networks, advertising revenue concentrated with major platforms, inability to compete with conglomerate resources. Career survival requires either working within conglomerate structures (surrendering editorial autonomy) or accepting precarious freelance economics. No viable exit path within the media ecosystem. Maximum extraction experienced.
constraint_indexing:constraint_classification(media_concentration_brazil, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: MARGINALIZED COMMUNITY (SNARE) — Information sources are captured by conglomerate-owned outlets that actively misrepresent or ignore local issues. Community members have no means to broadcast counter-narratives. The constraint extracts political voice — conglomerate framing determines electoral behavior, resource allocation, and policy attention. Trapped by geography and economics; cannot access alternative media distribution.
constraint_indexing:constraint_classification(media_concentration_brazil, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 3: REGIONAL MEDIA OUTLET (TANGLED ROPE) — Constrained by capital requirements for digital transition and distribution competition. Experiences genuine coordination benefits through industry associations and shared newswires, but also pays asymmetric extraction: advertising inventory must navigate conglomerate-controlled platforms; audience reach is suppressed by algorithmic curation favoring major outlets. Mixed extraction and coordination.
constraint_indexing:constraint_classification(media_concentration_brazil, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: MAJOR MEDIA CONGLOMERATE (ROPE) — Experiences the constraint as coordination mechanism: shared regulatory frameworks, audience reach standardization, industry associations. Benefits from scale economies and network effects. Can arbitrage between media forms (broadcast, cable, digital) and between political connections (leverage ownership for regulatory favors). Extraction runs toward this agent; they see the system as functional coordination.
constraint_indexing:constraint_classification(media_concentration_brazil, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: DIGITAL NATIVE AND ALTERNATIVE MEDIA COALITION (SCAFFOLD) — Organized actors (independent news sites, podcasters, YouTube channels, substack writers) are building parallel distribution pathways that bypass conglomerate gatekeeping. Constraint has sunset logic: as digital platforms mature and audience fragmentations occur, conglomerate monopoly on information flow weakens. Coalition members are constrained by monetization barriers but have agency and perceive an exit path through distributed media. Theater is lower — content verification happens through community review rather than institutional review ritual.
constraint_indexing:constraint_classification(media_concentration_brazil, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: POLITICAL ELITE (OPPORTUNISTIC) (TANGLED ROPE) — Maintains symbiotic relationship with major media conglomerates: favorable coverage in exchange for regulatory leniency and control of advertising spend. Extraction is mutual and contingent — elites extract favorable framing from media, media extract regulatory control and revenue from state. Highly mobile and powerful, but relationship lock-in from mutual benefit means actual mobility is constrained by interdependence. Both beneficiary and victim simultaneously.
constraint_indexing:constraint_classification(media_concentration_brazil, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(continental))).

% PERSPECTIVE 7: MEDIA REGULATORY FRAMEWORK (PITON) — Regulatory agencies maintain formal licensing, cross-media ownership rules, and content guidelines. Theater is high: rules are formally codified but enforcement is inconsistent, selective application favors politically-connected owners, and regulatory capture by major conglomerates is substantial. The regulatory apparatus performs oversight function while being captured by regulated firms. Regulatory bodies see their own authority as degraded (unable to enforce rules effectively) but maintain the ritual through institutional inertia.
constraint_indexing:constraint_classification(media_concentration_brazil, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER / STRUCTURAL ECONOMICS VIEW (MOUNTAIN) — From structural economics perspective, some media concentration is inherent to capital-intensive broadcasting: high infrastructure costs create natural barriers to entry, and economies of scale favor large conglomerates. This perspective sees concentration as an immutable economic law. However, the structural data contradicts this: digital platforms have dramatically lowered distribution costs, alternative media is viable at lower scale, and concentration is maintained through active enforcement (regulatory capture, advertising control, network effects) rather than pure capital constraints. This is a false summit — the 'natural law' framing obscures how contingent institutional arrangements naturalize extraction.
constraint_indexing:constraint_classification(media_concentration_brazil, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(media_concentration_brazil_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(media_concentration_brazil, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(media_concentration_brazil, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(media_concentration_brazil, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(media_concentration_brazil, TR),
    TR >= 0.70.

:- end_tests(media_concentration_brazil_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. The constraint extracts substantially from journalists and marginalized communities (limiting voice and opportunity), but extraction is not absolute — digital alternatives and international pressure are creating modest exit opportunities. The value reflects that conglomerate dominance is sustained by active enforcement (regulatory capture, state advertising) rather than pure market forces; if enforcement mechanisms weaken, extraction could decline. Suppression (0.68): High. Multiple suppression mechanisms operate: capital barriers to entry for new broadcast infrastructure, algorithmic curation favoring conglomerate content, advertising inventory gatekeeping, regulatory capture preventing enforcement of ownership limits, career risk for independent journalists, and information access asymmetry (conglomerates control what stories get told). Theater ratio (0.55): Moderate-high. Regulatory framework maintains formal ownership caps and content rules, but enforcement is inconsistent and selective. Regulatory bodies perform oversight while being captured — the ritual persists but function is degraded. Digital alternatives have lower theater (distributed verification through community engagement) but reach is still suppressed by algorithmic curation.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap is extreme and diagnostically clear. Conglomerate owners see coordination (Rope) — the system enables them to reach audiences, coordinate with advertisers, and maintain stable market position. Independent journalists see extraction (Snare) — they are trapped with no viable alternative. Marginalized communities see information monopoly (Snare) — conglomerate narratives determine their political reality. Regional outlets see mixed extraction and coordination (Tangled Rope) — they benefit from industry associations but lose audience to conglomerate dominance. Political elites see mutual benefit (Tangled Rope) — they exchange regulatory favors for media attention. Digital alternatives see a temporary problem with a sunset (Scaffold) — as distribution costs fall, conglomerate gatekeeping loses force. Regulatory agencies see degraded authority (Piton) — they maintain the oversight ritual but cannot enforce rules effectively. The civilizational observer risks naturalizing this as economic law (Mountain) but the structural data contradicts it.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values are derived from each agent's structural position: beneficiary status, power level, and exit options determine d values. Conglomerate owners with institutional power and arbitrage options (low d from beneficiary status) experience negative or near-zero effective extraction — the system subsidizes them. Independent journalists with powerless status and trapped exit options experience maximum extraction (d approaching 1.0, f(d) ≈ 1.42). Regional outlets with moderate power and constrained exit experience moderate extraction (d around 0.65). Political elites are complex: they are both extractors (controlling state advertising) and beneficiaries (receiving favorable coverage), creating d around 0.50 and mixed classification. The analytical observer risks d around 0.72 but may naturalize the constraint as immutable, producing false summit classification.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION: The constraint resolves the mandatrophy by demonstrating that the classification depends entirely on structural position. The same set of base properties (extractiveness 0.58, suppression 0.68) produces Snare from powerless journalists and Rope from institutional conglomerates. The mandatrophy is not a logical contradiction but a perspectival fact: the constraint is simultaneously a coordination mechanism (for those who benefit) and an extraction mechanism (for those who bear costs). The false summit (mountain classification from the analytical observer) is particularly instructive: the 'natural law of capital intensity' narrative is commonly used to justify concentration, but the structural data shows that concentration is maintained by active enforcement (regulatory capture, state advertising, platform gatekeeping), not by immutable capital economics. Digital platforms have made broadcast distribution radically cheaper, yet concentration persists — this proves that the constraint is not a natural law but a maintained institutional arrangement. The scaffold perspective provides the exit path: as digital alternatives mature and audience fragments across platforms, conglomerate gatekeeping becomes less valuable and less enforceable. The constraint's extractive power is contingent on maintaining information monopoly; once the monopoly breaks, extraction collapses.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    digital_disruption_timeline,
    'Will digital platforms (YouTube, TikTok, Substack, podcasts) actually fragment traditional media dominance, or will platform owners establish their own concentration?',
    'Longitudinal tracking of audience share and advertising revenue flow between traditional and digital outlets over 5-10 year windows; identification of whether digital fragmentation persists or reconcentrates',
    'If digital fragmentation persists: scaffold sunset is real, constraint extraction decreases over time. If platforms reconcentrate: a new extraction mechanism replaces the old one, constraint strength remains high but changes form.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(digital_disruption_timeline, empirical, 'Will digital platforms disrupt traditional media concentration').

omega_variable(
    political_elite_dependence,
    'How much of media conglomerate power derives from state advertising revenue and regulatory control versus from genuine audience preference and market share?',
    'Financial analysis of state-directed advertising flows; comparison of conglomerate performance in markets with vs without state contracts; measurement of audience migration if state contracts were removed',
    'If state-dependent: constraint is primarily political-economic (elite extraction), weakens with political realignment. If audience-driven: constraint is market-based (capital economies of scale), stronger and more durable.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(political_elite_dependence, empirical, 'Degree to which conglomerate power depends on state support').

omega_variable(
    suppression_internalization,
    'Is suppression of independent media primarily structural (capital barriers, distribution gatekeeping) or internalized (journalists self-censor from fear/socialization, audiences assume conglomerate narratives are natural)?',
    'Comparative analysis of journalist behavior and audience preference in contexts with vs without conglomerate dominance; post-exit suppression persistence (do journalists who leave conglomerate employment maintain internalized constraints?)',
    'If structural: suppression decreases when gatekeeping barriers are removed. If internalized: suppression persists through culture and socialization, requires longer horizon for change.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_internalization, empirical, 'Whether media suppression is structural or internalized').

omega_variable(
    regulatory_capture_mechanism,
    'What is the primary mechanism of regulatory capture: revolving door (personnel flow), political alignment (shared ideology), or economic dependence (regulatory bodies funded by media firms)?',
    'Network analysis of personnel flows between ANCINE/ANATEL and major conglomerates; tracking of electoral campaign financing by media conglomerates; budget dependency analysis of regulatory agencies',
    'If revolving door: weaker capture (change personnel). If political alignment: stronger capture (requires ideological shift). If economic dependence: strongest capture (requires structural funding reform).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(regulatory_capture_mechanism, empirical, 'Primary mechanism of regulatory capture').

omega_variable(
    community_voice_capacity,
    'Can marginalized communities actually use digital platforms to establish counter-narratives at scale, or do algorithmic curation and resource barriers perpetuate conglomerate dominance in digital spaces?',
    'Comparative reach analysis: audience size of independent/community digital content vs conglomerate content in same demographic segments; cost-per-impression analysis for independent vs conglomerate distribution',
    'If communities can compete: scaffold is viable, digital disruption solves the constraint. If algorithmic gatekeeping persists: digital merely replicates concentration at new scale, constraint strength unchanged.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(community_voice_capacity, empirical, 'Whether digital platforms enable community voice at meaningful scale').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(media_concentration_brazil, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(medconc_tr_t0, media_concentration_brazil, theater_ratio, 0, 0.42).
narrative_ontology:measurement(medconc_tr_t10, media_concentration_brazil, theater_ratio, 10, 0.48).
narrative_ontology:measurement(medconc_tr_t20, media_concentration_brazil, theater_ratio, 20, 0.55).

% Extraction over time
narrative_ontology:measurement(medconc_be_t0, media_concentration_brazil, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(medconc_be_t10, media_concentration_brazil, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(medconc_be_t20, media_concentration_brazil, base_extractiveness, 20, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(media_concentration_brazil, resource_allocation).
narrative_ontology:affects_constraint(media_concentration_brazil, electoral_capture_brazil).
narrative_ontology:affects_constraint(media_concentration_brazil, state_advertising_concentration).

% DUAL FORMULATION NOTE:
% Media concentration is downstream of (and dependent on) state advertising concentration and electoral campaign finance mechanisms. These three constraints form a constraint family: media concentration enables electoral manipulation, which is reinforced by state advertising spend directed toward conglomerate-friendly outlets. Each story has its own extractiveness value reflecting different observables: media concentration (ε=0.58) focuses on information gatekeeping; electoral capture (ε varies by political context) focuses on voting behavior distortion; state advertising (ε=0.65+) focuses on revenue extraction from public funds.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(media_concentration_brazil, institutional, 0.12).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
