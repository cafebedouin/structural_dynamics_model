% ============================================================================
% CONSTRAINT STORY: digital_power_concentration
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-01-09
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_digital_power_concentration, []).

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
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_axiom_grounding/3,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:cs_created_at/2,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: digital_power_concentration
 *   human_readable: Digital Power Concentration and Platform Sovereignty
 *   domain: theological_ethics/technology_governance/political_economy
 *
 * SUMMARY:
 *   Digital power concentration represents a structural shift in sovereignty
 *   from democratic states to transnational private actors controlling
 *   platforms, data, algorithms, and infrastructure. Five companies
 *   (Alphabet, Meta, Amazon, Apple, Microsoft) now mediate the majority of
 *   global digital interaction, commerce, and information access. This
 *   concentration enables unilateral rule-setting (terms of service as
 *   private law), opacity (algorithmic black boxes), and extraction
 *   (behavioral surplus, attention commodification, wage theft through
 *   algorithmic management). The constraint violates core principles of
 *   Catholic Social Doctrine: subsidiarity (decisions made at the lowest
 *   competent level are overridden by platform fiat), universal destination
 *   of goods (digital infrastructure and data serve private accumulation
 *   rather than common good), and solidarity (structures isolate and extract
 *   rather than enable participation). The coordination narrative
 *   ('connecting the world,' 'organizing information,' 'enabling commerce')
 *   serves as cover for a snare: network effects are weaponized to create
 *   lock-in, interoperability is blocked to prevent exit, competitors are
 *   acquired to suppress alternatives, and the innovation-requires-scale
 *   doctrine naturalizes what is actually constructed extraction. The
 *   constraint exhibits rising extractiveness (0.35 → 0.78 over 24 years),
 *   rising suppression (0.40 → 0.82), and rising theater ratio (0.25 → 0.68)
 *   as governance mechanisms become increasingly performative (privacy
 *   policies no one reads, consent flows that aren't genuine consent, content
 *   moderation that is inconsistent and opaque).
 *
 * KEY AGENTS:
 *   - Platform Users: Primary victims (powerless/trapped) — locked in by network effects, data portability barriers, and absence of alternatives; experience maximum extraction through behavioral surveillance, attention commodification, algorithmic manipulation
 *   - Gig Workers: Primary victims (powerless/trapped) — locked in by algorithmic management, independent contractor misclassification, and lack of alternative livelihoods; experience severe extraction through wage theft, constant surveillance, unilateral terms changes
 *   - Small Businesses: Secondary victims (moderate/constrained) — dependent on platforms for customer access but can technically exit at prohibitive cost; experience high extraction through platform fees, algorithmic demotion, terms changes
 *   - Nation-States: Mixed position (institutional/constrained) — both victims of sovereignty erosion and beneficiaries of platform infrastructure; experience tangled rope (genuine coordination problem entangled with regulatory capture)
 *   - Tech Oligopolies: Primary beneficiaries (institutional/arbitrage) — capture rents from platform control, data ownership, and algorithmic opacity; experience the constraint as pure coordination; can exit any jurisdiction through regulatory arbitrage
 *   - Digital Rights Coalition: Organized resistance (organized/mobile) — building alternative governance pathways through interoperability mandates, data portability, algorithmic transparency; see the constraint as scaffold with a generational sunset
 *   - Analytical Observer (Magisterial Reading): Civilizational perspective (analytical/analytical) — sees concentration as clear violation of CST principles; identifies the constraint as snare (constructed extraction with coordination narrative as cover)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(digital_power_concentration, 0.78).
domain_priors:suppression_score(digital_power_concentration, 0.82).
domain_priors:theater_ratio(digital_power_concentration, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(digital_power_concentration, extractiveness, 0.78).
narrative_ontology:constraint_metric(digital_power_concentration, suppression_requirement, 0.82).
narrative_ontology:constraint_metric(digital_power_concentration, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(digital_power_concentration, snare).
narrative_ontology:human_readable(digital_power_concentration, "Digital Power Concentration and Platform Sovereignty").
narrative_ontology:topic_domain(digital_power_concentration, "theological_ethics/technology_governance/political_economy").

domain_priors:requires_active_enforcement(digital_power_concentration).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(digital_power_concentration, 'a5408ddf-a0c0-438a-85bb-c4fccd63e84b').
narrative_ontology:cs_kernel_codification('a5408ddf-a0c0-438a-85bb-c4fccd63e84b', formalized).
narrative_ontology:cs_authority_grounding('a5408ddf-a0c0-438a-85bb-c4fccd63e84b', lineage).
narrative_ontology:cs_interpretation_layer_present('a5408ddf-a0c0-438a-85bb-c4fccd63e84b').
narrative_ontology:cs_reading_relation('a5408ddf-a0c0-438a-85bb-c4fccd63e84b', digital_power_concentration__secular_humanist_reading, coexists_with).
narrative_ontology:cs_reading_relation('a5408ddf-a0c0-438a-85bb-c4fccd63e84b', digital_power_concentration__techno_optimist_reading, forecloses).
narrative_ontology:cs_reading_relation('a5408ddf-a0c0-438a-85bb-c4fccd63e84b', digital_power_concentration__pluralist_pragmatic_reading, influences).
narrative_ontology:cs_axiom('a5408ddf-a0c0-438a-85bb-c4fccd63e84b', foundational, dignity_as_imago_dei_ontological).
narrative_ontology:cs_axiom_status(dignity_as_imago_dei_ontological, holdable).
narrative_ontology:cs_axiom_grounding('a5408ddf-a0c0-438a-85bb-c4fccd63e84b', dignity_as_imago_dei_ontological, theological).
narrative_ontology:cs_axiom('a5408ddf-a0c0-438a-85bb-c4fccd63e84b', foundational, subsidiarity_principle_binding).
narrative_ontology:cs_axiom_status(subsidiarity_principle_binding, holdable).
narrative_ontology:cs_axiom_grounding('a5408ddf-a0c0-438a-85bb-c4fccd63e84b', subsidiarity_principle_binding, deontological).
narrative_ontology:cs_axiom('a5408ddf-a0c0-438a-85bb-c4fccd63e84b', foundational, universal_destination_of_goods).
narrative_ontology:cs_axiom_status(universal_destination_of_goods, holdable).
narrative_ontology:cs_axiom_grounding('a5408ddf-a0c0-438a-85bb-c4fccd63e84b', universal_destination_of_goods, deontological).
narrative_ontology:cs_axiom('a5408ddf-a0c0-438a-85bb-c4fccd63e84b', secondary, technology_serves_human_flourishing_not_transcendence).
narrative_ontology:cs_axiom_status(technology_serves_human_flourishing_not_transcendence, holdable).
narrative_ontology:cs_axiom_grounding('a5408ddf-a0c0-438a-85bb-c4fccd63e84b', technology_serves_human_flourishing_not_transcendence, theological).
narrative_ontology:cs_reference_frame('a5408ddf-a0c0-438a-85bb-c4fccd63e84b', pre_digital_cst_framework).
narrative_ontology:cs_drift_state('a5408ddf-a0c0-438a-85bb-c4fccd63e84b', contemporary_platform_economy, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('a5408ddf-a0c0-438a-85bb-c4fccd63e84b', '').

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(digital_power_concentration, tech_oligopolies).
narrative_ontology:constraint_beneficiary(digital_power_concentration, platform_shareholders).
narrative_ontology:constraint_beneficiary(digital_power_concentration, data_brokers).
narrative_ontology:constraint_beneficiary(digital_power_concentration, surveillance_infrastructure_operators).
narrative_ontology:constraint_victim(digital_power_concentration, platform_users).
narrative_ontology:constraint_victim(digital_power_concentration, gig_workers).
narrative_ontology:constraint_victim(digital_power_concentration, local_communities).
narrative_ontology:constraint_victim(digital_power_concentration, nation_states_with_eroded_sovereignty).
narrative_ontology:constraint_victim(digital_power_concentration, small_businesses_dependent_on_platforms).
narrative_ontology:constraint_victim(digital_power_concentration, global_south_populations).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PLATFORM USER (SNARE) — Trapped by network effects, data lock-in, and absence of viable alternatives. Cannot exit Facebook/WhatsApp without losing social connections; cannot leave Google without losing years of email/photos/documents; cannot avoid Amazon in many markets. Experiences maximum extraction: behavioral surplus harvested, attention commodified, autonomy eroded through algorithmic manipulation. No meaningful consent, no transparency, no recourse.
constraint_indexing:constraint_classification(digital_power_concentration, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: GIG WORKER (SNARE) — Trapped by algorithmic management with no transparency into rating systems, dispatch algorithms, or pay calculations. Cannot exit platform without losing livelihood; cannot organize collectively due to independent contractor classification. Experiences severe extraction: wage theft through opaque deductions, constant surveillance, asymmetric information, unilateral terms changes. The 'flexibility' narrative is cover for precarity.
constraint_indexing:constraint_classification(digital_power_concentration, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 3: SMALL BUSINESS (SNARE) — Constrained by platform dependency: must sell on Amazon to reach customers, must advertise on Google/Facebook to be visible, must use cloud infrastructure controlled by oligopolies. Can technically exit but at prohibitive cost (loss of customer base, search visibility, operational capacity). Experiences high extraction: 30% platform fees, algorithmic demotion if they don't buy ads, terms of service changes that destroy business models overnight.
constraint_indexing:constraint_classification(digital_power_concentration, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: NATION-STATE (TANGLED ROPE) — Constrained by transnational platform power that exceeds state capacity: platforms can route around national law, threaten to withdraw services, lobby effectively, and move faster than legislative processes. Yet states also benefit from platform infrastructure (tax revenue, innovation narrative, geopolitical tech competition). Mixed experience: genuine coordination problem (how to govern global digital infrastructure) entangled with extraction (regulatory capture, sovereignty erosion, inability to enforce labor/tax/competition law).
constraint_indexing:constraint_classification(digital_power_concentration, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: TECH OLIGOPOLY (ROPE) — Experiences the constraint as pure coordination: platforms solve real problems (connecting users, enabling commerce, organizing information). From this seat, concentration is a natural outcome of network effects and economies of scale. Extraction is invisible or reframed as 'value creation.' Arbitrage exit options: can shift operations across jurisdictions, restructure to avoid regulation, acquire competitors, shape policy through lobbying. Net beneficiary of the entire structure.
constraint_indexing:constraint_classification(digital_power_concentration, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: DIGITAL RIGHTS COALITION (SCAFFOLD) — Organized agents (EFF, Access Now, digital rights NGOs, some regulators) see concentration as a temporary coordination failure with a sunset: interoperability mandates, data portability, algorithmic transparency, and antitrust enforcement are building alternative governance pathways. GDPR, DSA, DMA represent early scaffolding. The coalition has agency and sees structural change as achievable within a generation, though success is not guaranteed.
constraint_indexing:constraint_classification(digital_power_concentration, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / MAGISTERIAL READING (SNARE) — From the civilizational/universal analytical perspective grounded in Catholic Social Doctrine, digital power concentration is a clear violation of subsidiarity (decisions made at the lowest competent level), the universal destination of goods (resources serve the common good, not private accumulation), and solidarity (structures should enable participation, not extraction). The concentration is not a natural law but a constructed snare: identifiable victims (users, workers, communities, states), identifiable beneficiaries (oligopolies), suppression of alternatives (network effects weaponized, interoperability blocked, competitors acquired), and coordination narrative as cover ('connecting the world'). This reading sees the techno-optimist framing as false consciousness and the secular humanist rights-based approach as necessary but insufficient without addressing structural power.
constraint_indexing:constraint_classification(digital_power_concentration, snare,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(digital_power_concentration_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(digital_power_concentration, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(digital_power_concentration, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(digital_power_concentration, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(digital_power_concentration_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.78): High. Tech oligopolies capture massive rents through platform control (30% fees on app stores, search/social advertising duopolies, cloud infrastructure oligopolies), data ownership (behavioral surplus harvested without compensation), and algorithmic opacity (manipulation of attention, prices, wages). Users, workers, and small businesses bear costs through lock-in, surveillance, precarity, and inability to contest platform decisions. The extraction has increased steadily as network effects matured, switching costs rose, and alternatives were acquired or marginalized. Suppression (0.82): Very high. Network effects create structural lock-in (cannot leave Facebook without losing social graph, cannot leave Google without losing email/photos/documents). Data portability is technically possible but practically blocked (no interoperability, export formats are incomplete). Alternatives are suppressed through acquisition (Instagram, WhatsApp, YouTube were all independent before acquisition), predatory pricing, and exclusive dealing. Workers cannot organize due to independent contractor classification and algorithmic management that prevents coordination. States cannot regulate effectively due to transnational platform power, regulatory capture, and threat of service withdrawal. Theater ratio (0.68): High. Governance mechanisms are increasingly performative: privacy policies are unreadable and non-negotiable, consent flows are dark patterns, content moderation is inconsistent and opaque, terms of service change unilaterally, and accountability mechanisms (appeals, transparency reports) are theatrical rather than functional. The theater has increased as platforms matured from 'move fast and break things' to 'trust and safety' rhetoric without structural accountability.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates extreme perspectival divergence. Tech oligopolies see pure coordination (Rope) — platforms solve real problems and concentration is a natural outcome of network effects. Users, workers, and small businesses see pure extraction (Snare) — locked in, surveilled, manipulated, with no meaningful exit or voice. Nation-states see mixed coordination and extraction (Tangled Rope) — platforms provide infrastructure but erode sovereignty. Digital rights coalitions see a temporary problem with a sunset (Scaffold) — interoperability and transparency mandates can restore subsidiarity. The analytical observer grounded in Catholic Social Doctrine sees a clear snare — identifiable victims, identifiable beneficiaries, suppression of alternatives, coordination narrative as cover — and identifies the techno-optimist 'innovation requires scale' framing as false consciousness. The perspectival gap is not a measurement error but the structure itself: extraction is invisible from the beneficiary seat and maximal from the victim seat.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is determined by structural position. Platform users are full victims (d → 1.0): trapped by network effects, no alternatives, maximum extraction through surveillance and manipulation. Gig workers are full victims (d → 1.0): trapped by algorithmic management, no transparency, severe extraction through wage theft and precarity. Small businesses are high-d victims (d → 0.75): constrained by platform dependency, can technically exit but at prohibitive cost, high extraction through fees and algorithmic control. Nation-states are mixed (d → 0.50): both victims of sovereignty erosion and beneficiaries of platform infrastructure, constrained by transnational power but also complicit through regulatory capture. Tech oligopolies are full beneficiaries (d → 0.0): arbitrage exit options, capture all rents, experience the constraint as pure coordination. Digital rights coalitions are low-d (d → 0.25): organized agents with agency, building alternatives, see structural change as achievable. The analytical observer is d → 0.0 (analytical seat) but classifies the constraint as snare because the structural data (victims, beneficiaries, suppression, coordination-as-cover) is unambiguous from the civilizational perspective.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint resolves mandatrophy by showing that the coordination narrative ('connecting the world,' 'organizing information,' 'enabling commerce') is genuine from the beneficiary perspective but serves as cover for extraction from the victim perspective. Platforms do solve real coordination problems (how to connect billions of users, how to organize information, how to enable global commerce), but the solutions are structured to maximize extraction (behavioral surplus, attention commodification, algorithmic control) rather than serve the common good. The Magisterial reading identifies this as a violation of the universal destination of goods: digital infrastructure and data are gifts that should serve all, but concentration structures them to serve private accumulation. The mandate (solve coordination problems) has not outlived its function, but the execution (oligopolistic control, opacity, extraction) has betrayed the mandate. This is not mandatrophy (mandate outliving function) but mandate capture (function subordinated to extraction). The scaffold perspective (digital rights coalition) represents an attempt to restore the original mandate through structural reform (interoperability, transparency, accountability).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    network_effects_naturalization,
    'Are network effects an immutable technical property that necessitates concentration, or a design choice that could be mitigated through interoperability standards?',
    'Historical analysis of interoperable network protocols (email, web, telephony) vs. proprietary walled gardens; technical feasibility studies of mandated interoperability (e.g., EU Digital Markets Act implementation); comparison of concentration levels in interoperable vs. closed ecosystems.',
    'If network effects are immutable: concentration is closer to Mountain (natural law), and regulation can only mitigate harms at the margins. If network effects are design choices: concentration is Snare (constructed extraction), and interoperability mandates can structurally dissolve the lock-in.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(network_effects_naturalization, empirical, 'Whether network effects necessitate concentration or are design choices').

omega_variable(
    innovation_scale_tradeoff,
    'Does innovation genuinely require the scale and concentration of current tech oligopolies, or is the innovation narrative cover for rent extraction?',
    'Comparative analysis of innovation rates in concentrated vs. fragmented tech ecosystems; examination of where breakthrough innovations actually originate (often universities, startups, open-source communities, not incumbents); assessment of how much oligopoly R&D spending goes to genuine innovation vs. acquisition of potential competitors and defensive patents.',
    'If scale is necessary for innovation: some concentration is justified coordination (Tangled Rope from more perspectives). If scale is unnecessary: the innovation narrative is pure cover, and concentration is extractive throughout (Snare from more perspectives).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(innovation_scale_tradeoff, empirical, 'Whether innovation requires oligopoly scale or the narrative is cover').

omega_variable(
    subsidiarity_restoration_feasibility,
    'Can subsidiarity be restored in digital governance through technical and legal interventions, or has the concentration created irreversible path dependencies?',
    'Assessment of interoperability mandate effectiveness (EU DMA); viability of platform cooperatives and municipal broadband as alternatives; technical feasibility of decentralized architectures (federated social networks, blockchain-based identity); political economy analysis of whether states can reassert sovereignty over transnational platforms.',
    'If subsidiarity is restorable: Scaffold perspective is vindicated, and the constraint has a real sunset. If path dependencies are irreversible: concentration is effectively permanent, and the Scaffold perspective is aspirational rather than structural.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(subsidiarity_restoration_feasibility, empirical, 'Whether subsidiarity can be restored or concentration is irreversible').

omega_variable(
    magisterial_authority_scope,
    'Does the Magisterium''s interpretive authority over human dignity extend to adjudicating legitimate AI governance for non-Catholics, or is this a category error (applying religious authority to pluralistic public reason)?',
    'This is a conceptual omega, not resolvable by empirical data. Resolution depends on whether one accepts the natural law framework (dignity knowable by reason, Church as authoritative interpreter) or the pluralist framework (dignity contested, no single authority). The question is whether the Magisterial reading''s claim to universal authority is itself a form of extraction (imposing comprehensive doctrine on those who don''t share its premises) or a legitimate defense of the common good.',
    'If Magisterial authority is legitimate for public governance: the Magisterial reading''s Snare classification is authoritative, and secular frameworks are inadequate. If Magisterial authority is limited to the Catholic community: the Magisterial reading is one voice among many, and pluralist pragmatic approaches are structurally necessary.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(magisterial_authority_scope, conceptual, 'Scope of Magisterial authority in pluralistic governance contexts').

omega_variable(
    cs_framing_underdetermination,
    'Is the kernel ''human dignity as imago Dei'' or the broader structure ''Catholic Social Doctrine as governance framework''? The encyclical treats imago Dei as the immutable kernel, but the actual authority structure adjudicates through the full CST apparatus (subsidiarity, common good, solidarity, universal destination of goods). If the kernel is just imago Dei, then CST principles are the interpretation layer. If the kernel is the full CST framework, then imago Dei is a grounding axiom within a larger kernel. Which framing is structurally correct?',
    'Examination of how the Magisterium actually adjudicates cases: does it derive governance conclusions directly from imago Dei (narrow kernel + thick interpretation layer), or does it treat CST principles as themselves part of the authoritative kernel (broad kernel + thin interpretation layer)? Historical analysis of doctrinal development: have CST principles themselves evolved (suggesting they are interpretation), or have they remained stable while only applications changed (suggesting they are kernel)?',
    'If kernel = imago Dei only: interpretation_layer_present = true, and drift in CST principles (e.g., development of religious freedom doctrine from Dignitatis Humanae) is absorbed in the interpretation layer without threatening kernel stability. If kernel = full CST framework: interpretation_layer_present = false or thin, and doctrinal development represents kernel drift, making the ''immutable'' claim more contestable. The narrow-kernel framing makes the Magisterial reading more structurally resilient; the broad-kernel framing makes it more vulnerable to historical contingency critiques.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cs_framing_underdetermination, conceptual, 'Whether the kernel is imago Dei alone or the full CST framework').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(digital_power_concentration, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dpc_theater_2000, digital_power_concentration, theater_ratio, 0, 0.25).
narrative_ontology:measurement(dpc_theater_2005, digital_power_concentration, theater_ratio, 5, 0.35).
narrative_ontology:measurement(dpc_theater_2010, digital_power_concentration, theater_ratio, 10, 0.48).
narrative_ontology:measurement(dpc_theater_2015, digital_power_concentration, theater_ratio, 15, 0.58).
narrative_ontology:measurement(dpc_theater_2020, digital_power_concentration, theater_ratio, 20, 0.65).
narrative_ontology:measurement(dpc_theater_2024, digital_power_concentration, theater_ratio, 24, 0.68).

% Extraction over time
narrative_ontology:measurement(dpc_extract_2000, digital_power_concentration, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(dpc_extract_2005, digital_power_concentration, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(dpc_extract_2010, digital_power_concentration, base_extractiveness, 10, 0.58).
narrative_ontology:measurement(dpc_extract_2015, digital_power_concentration, base_extractiveness, 15, 0.68).
narrative_ontology:measurement(dpc_extract_2020, digital_power_concentration, base_extractiveness, 20, 0.75).
narrative_ontology:measurement(dpc_extract_2024, digital_power_concentration, base_extractiveness, 24, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(dpc_suppress_2000, digital_power_concentration, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(dpc_suppress_2005, digital_power_concentration, suppression_requirement, 5, 0.52).
narrative_ontology:measurement(dpc_suppress_2010, digital_power_concentration, suppression_requirement, 10, 0.63).
narrative_ontology:measurement(dpc_suppress_2015, digital_power_concentration, suppression_requirement, 15, 0.72).
narrative_ontology:measurement(dpc_suppress_2020, digital_power_concentration, suppression_requirement, 20, 0.79).
narrative_ontology:measurement(dpc_suppress_2024, digital_power_concentration, suppression_requirement, 24, 0.82).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(digital_power_concentration, global_infrastructure).
narrative_ontology:affects_constraint(digital_power_concentration, algorithmic_management_precarity).
narrative_ontology:affects_constraint(digital_power_concentration, surveillance_capitalism_behavioral_surplus).
narrative_ontology:affects_constraint(digital_power_concentration, content_moderation_opacity).

% DUAL FORMULATION NOTE:
% Digital power concentration is downstream of the technocratic paradigm (technocratic_paradigm_resistance) — the broader cultural and epistemic shift toward technocratic solutions that marginalize ethical deliberation and subsidiarity. The technocratic paradigm creates the conditions for concentration (efficiency and scale as ultimate values, democratic deliberation as friction), and concentration in turn reinforces the paradigm (oligopolies fund technocratic research, shape policy discourse, and naturalize their own power). The two constraints are distinct but mutually reinforcing: the paradigm is the cultural-epistemic structure, concentration is the material-institutional structure.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(digital_power_concentration, institutional, 0.5).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
