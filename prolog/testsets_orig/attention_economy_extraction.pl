% ============================================================================
% CONSTRAINT STORY: attention_economy_extraction
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_attention_economy_extraction, []).

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
 *   constraint_id: attention_economy_extraction
 *   human_readable: Attention Economy Extraction
 *   domain: digital_economy/behavioral_technology
 *
 * SUMMARY:
 *   The attention economy extraction constraint describes the systematic
 *   harvesting of human attention and behavioral data by platform
 *   corporations in exchange for nominally free services. The constraint
 *   operates through network effects (users trapped because social graphs are
 *   platform-locked), cognitive manipulation (algorithmic optimization for
 *   engagement rather than user welfare), and market concentration (limited
 *   alternatives). The theater ratio (0.58) reflects the substantial
 *   performative layer: user interface controls labeled as 'privacy
 *   settings,' algorithmic explanation systems that create appearance of
 *   understanding without enabling actual control, and regulatory compliance
 *   frameworks that exist on paper but do not limit extraction mechanisms.
 *   The constraint exhibits all six classifications from different
 *   perspectives because it genuinely contains both coordination functions
 *   (connecting people, enabling discovery) and extraction mechanisms
 *   (harvesting attention, monetizing behavioral data), making it a
 *   diagnostic exemplar for tangled rope and snare boundary classification.
 *   However, the dominant classification from the empirical majority of
 *   perspectives (powerless users, trapped users, identity-locked children,
 *   epistemic commons) is snare — the extraction mechanisms dominate the
 *   coordination functions from the perspective of those bearing the costs.
 *
 * KEY AGENTS:
 *   - Individual Users: Primary victims (powerless/trapped) — economic dependency on free platforms, cognitive vulnerabilities to engagement mechanics, network effects that eliminate exit options
 *   - Child Users: Primary victims (powerless/identity_locked) — identity formation occurs within platform ecosystems; binding mechanism is internalized cognitive capture during neuroplastic periods
 *   - Content Creators: Secondary victims (moderate/constrained) — dependent on platforms for distribution; experience algorithmic extraction of behavioral data and content IP; monetization rates set unilaterally
 *   - Epistemic Commons: Abstract victim (powerless/trapped) — no agency or exit; bears cost of algorithmic distortion of information circulation, misinformation amplification, filter bubbles
 *   - Platform Corporations: Primary beneficiaries (institutional/arbitrage) — capture attention as monetized resource; experience constraint as coordination mechanism enabling user-advertiser matching
 *   - Advertisers: Secondary beneficiaries (powerful/mobile) — benefit from platform targeting and reach; bear extraction through pricing leverage and algorithmic opacity; have partial exit options
 *   - Regulatory Framework: Institutional actor (institutional/constrained) — nominally limits extraction but operates as piton; performative oversight with insufficient enforcement capacity
 *   - Analytical Observer: Full-scope analyst (analytical/analytical) — sees both genuine coordination functions and systematic extraction operating in tandem
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(attention_economy_extraction, 0.68).
domain_priors:suppression_score(attention_economy_extraction, 0.72).
domain_priors:theater_ratio(attention_economy_extraction, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(attention_economy_extraction, extractiveness, 0.68).
narrative_ontology:constraint_metric(attention_economy_extraction, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(attention_economy_extraction, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(attention_economy_extraction, snare).
narrative_ontology:human_readable(attention_economy_extraction, "Attention Economy Extraction").
narrative_ontology:topic_domain(attention_economy_extraction, "digital_economy/behavioral_technology").

domain_priors:requires_active_enforcement(attention_economy_extraction).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(attention_economy_extraction, platform_corporations).
narrative_ontology:constraint_beneficiary(attention_economy_extraction, advertising_networks).
narrative_ontology:constraint_victim(attention_economy_extraction, individual_attention_users).
narrative_ontology:constraint_victim(attention_economy_extraction, epistemic_commons).
narrative_ontology:constraint_victim(attention_economy_extraction, child_cognitive_development).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: INDIVIDUAL USER (SNARE) — Trapped by economic dependency on free platforms, cognitive vulnerabilities exploited by algorithmic engagement mechanics, and network effects that make exit impossible despite full awareness of extraction. User has no meaningful alternative; the coordination function (connecting people) is inseparable from the extraction mechanism (harvesting attention for monetization). Maximum suppression through intermittent reinforcement, variable reward schedules, and social lock-in. This agent experiences pure extraction with minimal coordination benefit perceived at biographical timescale.
constraint_indexing:constraint_classification(attention_economy_extraction, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: CHILD USER (SNARE with IDENTITY_LOCK) — Trapped both structurally (no economic alternatives, social pressure to participate) and cognitively (identity formation occurs within platform ecosystems, reward pathways during neuroplastic periods). The child cannot exit without abandoning peer social identity. Suppression is near-total because the binding mechanism is internalized during development — the child's sense of self becomes platform-constituted. This perspective instantiates identity_locked as a binding mechanism distinct from mere constrained exit: the child experiences the platform not as extractive but as natural — it IS the social world from their cognitive vantage point.
constraint_indexing:constraint_classification(attention_economy_extraction, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(identity_locked),
            spatial_scope(global))).

% PERSPECTIVE 3: CONTENT CREATOR (SNARE) — Structurally constrained by platform dependency for distribution and income. Experiences algorithmic extraction of behavioral data and content IP in exchange for visibility. Suppression high: platform-controlled ranking algorithms determine reach; no transparent appeal mechanism; constant pressure to optimize for engagement metrics that platforms define. Creator sees minimal coordination benefit (platform does connect creator to audience) but maximum extraction (content and attention data harvested, monetization rates set unilaterally by platform). High d → high chi → snare.
constraint_indexing:constraint_classification(attention_economy_extraction, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 4: PLATFORM CORPORATION (ROPE) — Experiences the constraint as pure coordination: connecting users, creators, and advertisers solves a genuine collective action problem. The platform perceives its role as enabling connection. From this perspective, data collection and algorithmic optimization are coordination mechanisms (matching content to audience, preventing spam, personalizing experience). Low d (beneficiary with exit options) → negative chi → rope. The platform experiences this as a genuine coordination solution despite being the primary extraction beneficiary.
constraint_indexing:constraint_classification(attention_economy_extraction, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: ADVERTISER (TANGLED ROPE) — Benefits from platform's targeting and reach (genuine coordination function); bears extraction in the form of algorithmic opacity, unpredictable policy changes, and platform leverage over ad pricing. Has exit options (can shift budgets to competing platforms or direct channels) but experiences platform market concentration that reduces meaningful alternatives. Suppression is real but surmountable; extraction is substantial but not total because advertiser has partial exit capacity and global visibility into platform mechanics. Moderate d → moderate chi → tangled rope.
constraint_indexing:constraint_classification(attention_economy_extraction, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: EPISTEMIC COMMONS (SNARE) — Cannot exit the ecosystem; has no agency or power to organize. Bears full cost of algorithmic distortion of information circulation: filter bubbles, engagement-driven amplification of sensationalism, suppression of nuance, viral spread of misinformation. The commons has no advocate and no self-correction mechanism within platform logic. This abstract collective victim experiences maximum extraction with zero exit options at civilizational timescale.
constraint_indexing:constraint_classification(attention_economy_extraction, snare,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 7: REGULATORY FRAMEWORK (PITON) — Nominally intended to limit platform extraction (GDPR, DSA, regulation of algorithmic opacity, child protection laws) but largely performative. Regulations are written with loopholes, enforcement is underfunded, platforms lobby for weakening provisions, and the regulatory body lacks technical capacity to verify compliance. Theater ratio high: audits and compliance certifications create appearance of control without limiting actual extraction mechanisms. The constraint persists through institutional inertia — regulators maintain the fiction that oversight is effective while extraction mechanisms operate beneath the threshold of regulatory visibility or capacity to interdict.
constraint_indexing:constraint_classification(attention_economy_extraction, piton,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER (TANGLED ROPE) — From full transparency and civilizational scope, attention economy exhibits genuine coordination function (connecting users across distance, enabling discovery of content, solving matching problems) alongside systematic extraction (attention harvested as revenue source, behavioral data mined for prediction, algorithmic optimization targets engagement over user welfare). The constraint is hybrid: neither pure extraction (coordination genuinely solves problems users value) nor pure coordination (extraction is the primary mechanism driving platform profits and design choices). The analytical view sees both functions operating in tandem, making tangled rope the correct classification at this perspective.
constraint_indexing:constraint_classification(attention_economy_extraction, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(attention_economy_extraction_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(attention_economy_extraction, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(attention_economy_extraction, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(attention_economy_extraction, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(attention_economy_extraction, TR),
    TR >= 0.70.

:- end_tests(attention_economy_extraction_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High and increasing. The measurement trajectory shows extraction accelerating from 0.35 (early platforms focused on connection) to 0.68 (current platforms focused on engagement optimization for advertising revenue). This reflects the historical shift from coordination-primary design (Rope or Tangled Rope in early era) to extraction-primary design (Snare in current era). The base extractiveness value at t=10 reflects systematic harvesting of attention as primary revenue driver, behavioral data mining, and algorithmic ranking optimized for engagement rather than user welfare. The trajectory shows rent-seeking layered onto coordination function — the coordination mechanisms that once justified platform dominance have been subordinated to extraction optimization. Suppression (0.72): Very high. Network effects create economic dependency with no viable alternatives; cognitive manipulation through variable reward schedules and algorithmic curation of information; identity fusion for child users; lack of transparency in algorithmic decision-making; regulatory frameworks with insufficient enforcement capacity. Suppression operates at multiple levels: structural (platform market concentration, network lock-in), behavioral (intermittent reinforcement, notifications triggering compulsive checking), and cognitive (identity-locked users cannot conceive of self outside platforms). Theater ratio (0.58): Moderate-high. Substantial performative layer includes privacy control interfaces that create appearance of user agency without enabling actual control; algorithmic opacity with nominal 'transparency' explanations; regulatory compliance frameworks (GDPR, DSA) that exist but do not prevent extraction; advertising disclosure that reveals little about targeting mechanisms or behavioral inference. Theater has increased over the interval as platforms have responded to regulatory pressure and user concern through performative rather than structural reforms.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates maximum perspectival divergence. The platform corporation sees pure coordination (Rope) — connecting users and advertisers solves genuine problems. The individual user sees pure extraction (Snare) — their attention is systematically harvested despite full awareness. The regulatory framework sees itself as effective oversight (Piton, though it views itself differently) while actual extraction mechanisms operate beneath enforcement capacity. The analytical observer at civilizational scope sees tangled rope — genuine coordination functions exist alongside systematic extraction, and the two are coupled: platforms use coordination benefits to justify and obscure extraction. The child user's identity_locked classification reveals a binding mechanism (internalized identity fusion) distinct from merely constrained exit — the child does not perceive the constraint as extraction because the platform IS their social world. The epistemic commons perspective reveals an invisible victim with no representation: platform algorithms optimize for engagement at the cost of information accuracy, and the commons bears this cost with no seat at the decision table. The perspectival gap between beneficiary (rope experience) and victims (snare and identity_locked experiences) is maximum — the constraint appears benign from the extraction beneficiary's position and malign from the targeted position.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) is derived from each agent's structural relationship to extraction flow. Beneficiaries (platform corporations, advertisers) experience low d → negative f(d) → negative or low chi. Victims (users, content creators, epistemic commons) experience high d (full targets) → high f(d) → high chi. Identity-locked victims experience high d but perceive the constraint as natural or beneficial because their identity is constituted through the platform — they have low agency to move away despite high structural extraction. The child user's d value is high (victim status) but suppression value is higher because binding mechanism is internalized — the biological/cognitive lock is stronger than economic lock. Content creators occupy intermediate position: benefiting from platform access (moderate d) while also targeted by data extraction (moderate d increases). Advertisers have lower d because they have exit options — they can shift budgets, use competing platforms, or develop direct relationships, though platform market concentration reduces the value of these options. The regulatory framework's d is paradoxical: nominally low d (should benefit users by constraining platforms) but actual d is near zero because the framework lacks enforcement capacity — it experiences itself as beneficiary of platform ecosystem (regulatory capture, revolving door employment) despite nominal mission to constrain extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLVED THROUGH DECOMPOSITION: The apparent mandatrophy (is this coordination or extraction?) is resolved by recognizing that the constraint operates differently depending on which layer is being analyzed. At the architectural layer: genuine coordination function exists (matching problem solved, distributed connection enabled, information discovery improved). At the behavioral layer: extraction mechanisms are superimposed on coordination functions (attention harvesting, data mining, algorithmic ranking for engagement rather than user welfare). At the institutional layer: platforms have gradually subordinated coordination goals to extraction goals — early platforms were more rope-like, current platforms are more snare-like. The mandatrophy is not 'which is it?' but 'how have the balance and coupling evolved?' The classification drift from rope (early era) to snare (current era) is captured in measurements showing extractiveness increasing from 0.35 to 0.68 while coordination function remains constant. The snare classification is correct for current state because extraction mechanisms are now primary design drivers; coordination is maintained only insofar as it serves extraction. If platforms were forced to subordinate extraction to coordination (through regulation or competitive pressure), classification would drift back toward tangled rope or rope. The piton perspective on regulatory frameworks reveals that the formal coordination (law) is performative — enforcement is theatrical, loopholes are structural, compliance is choreography. This piton status is critical: it suggests that current regulation is not limiting extraction, merely creating appearance of limitation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    internalization_vs_coercion,
    'Is suppression primarily structural (external barriers: network effects, economic dependency) or primarily internalized (psychological capture through reward conditioning and identity formation)?',
    'Post-exit suppression trajectory: users who successfully abandon platforms for extended periods report whether the desire to return persists (internalized) or subsides (structural). Neuroimaging studies on reward pathway activation in heavy users. Cross-cultural variation in platform dependency despite identical technical affordances.',
    'If internalized: suppression value should be weighted higher for child and identity_locked perspectives; exit barriers lower than measurements suggest. If structural: external interventions (regulatory separation, interoperability mandates) have higher efficacy. Classification shifts between snare and constrained depending on mechanism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(internalization_vs_coercion, empirical, 'Whether suppression is structural or internalized psychological capture').

omega_variable(
    coordination_function_necessity,
    'Do the coordination functions (connecting users, matching content, discovery) require the extractive mechanisms (attention harvesting, algorithmic ranking, behavioral data collection) or are they orthogonal problems?',
    'Existence proofs: can equivalent coordination be achieved through non-extractive architectures? Analysis of interoperable protocol-based services (ActivityPub, decentralized alternatives) showing whether equivalent coordination functions emerge at lower extraction cost. Comparison of user welfare in high-extraction vs low-extraction coordination platforms.',
    'If coordination requires extraction: tangled rope classification is correct — both functions are structurally coupled. If orthogonal: snare classification is correct — extraction is pure surplus behavior not required for coordination. If non-extractive alternatives proven superior: current architecture is degraded (piton), not necessary (tangled rope or mountain).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_function_necessity, empirical, 'Whether coordination functions require extractive mechanisms').

omega_variable(
    platform_market_structure,
    'Does platform market concentration (two to four firms dominating attention economy) enable extraction that would be unsustainable in competitive markets, or does concentration merely reduce exit costs rather than enabling new extraction mechanisms?',
    'Historical analysis of competitive-era platforms; economic modeling of extraction under different market structures; comparison to non-platform attention markets (broadcast media, historical publishing) with different concentration levels. Entry barriers analysis: technical, regulatory, network-effect, and capital requirements.',
    'If concentration enables extraction: snare classification is correct and regulatory breakup would reduce suppression. If concentration merely reduces exit costs: extraction would persist even under breakup, requiring other interventions. If platform structure is natural monopoly: extraction is inherent to the coordination function (strong tangled rope case).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(platform_market_structure, empirical, 'Whether market concentration enables attention extraction').

omega_variable(
    child_cognitive_development_impact,
    'Does childhood platform exposure during critical neuroplastic periods create irreversible identity fusion (identity_locked), reversible behavioral conditioning (constrained), or both with different timelines?',
    'Longitudinal neuroimaging of platform-native cohorts through adolescence and into adulthood; behavioral studies of digital abstinence periods during development; twin studies controlling for native reward sensitivity; cross-generational comparison of identity formation in platform vs pre-platform cohorts.',
    'If irreversible: child perspective is correctly classified as identity_locked snare with permanent suppression. If reversible: child perspective should be reclassified as trapped snare (not identity_locked) — exit remains neurologically possible if effected before critical period closes. Affects mandatrophy resolution and regulatory urgency.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(child_cognitive_development_impact, empirical, 'Whether platform exposure during development creates irreversible identity fusion').

omega_variable(
    regulatory_capture_mechanism,
    'Is regulatory framework performance (piton classification) due to regulatory capture by platforms, technical incapacity of regulators, or structural difficulty of regulating distributed algorithms and cross-border data flows?',
    'Analysis of regulatory lobbying expenditures and legislative outcomes; audit of regulator technical capacity and staffing; comparative study of different regulatory models (tech-specific vs general data protection vs market competition) and their effectiveness. International comparison of regulation intensity vs platform extraction rates.',
    'If capture: regulatory intervention through revolving-door reforms. If incapacity: technical investment in regulatory capacity. If structural difficulty: coordinated international governance required. Different resolution mechanisms imply different timeline to piton status — capture can be fixed quickly; structural difficulty implies longer sunset or ongoing piton status.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_capture_mechanism, conceptual, 'Root cause of regulatory framework''s piton (performative) status').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(attention_economy_extraction, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(attn_tr_t0, attention_economy_extraction, theater_ratio, 0, 0.28).
narrative_ontology:measurement(attn_tr_t5, attention_economy_extraction, theater_ratio, 5, 0.45).
narrative_ontology:measurement(attn_tr_t10, attention_economy_extraction, theater_ratio, 10, 0.58).

% Extraction over time
narrative_ontology:measurement(attn_be_t0, attention_economy_extraction, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(attn_be_t5, attention_economy_extraction, base_extractiveness, 5, 0.52).
narrative_ontology:measurement(attn_be_t10, attention_economy_extraction, base_extractiveness, 10, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(attention_economy_extraction, attachment_coordination).
narrative_ontology:boltzmann_floor_override(attention_economy_extraction, 0.12).
narrative_ontology:affects_constraint(attention_economy_extraction, social_media_algorithmic_amplification).
narrative_ontology:affects_constraint(attention_economy_extraction, behavioral_data_monetization).
narrative_ontology:affects_constraint(attention_economy_extraction, platform_market_concentration).

% DUAL FORMULATION NOTE:
% Attention economy extraction is upstream of domain-specific platform constraints. Individual platforms (TikTok, Instagram, Twitter) exhibit their own extractive mechanisms, but they all operate within the structural attention economy constraint. The broader constraint affects all platforms regardless of specific design choices. Decomposition: platform-specific story for each major platform (TikTok_attention_extraction, Instagram_engagement_extraction) downstream of this general constraint. This story focuses on the structural mechanisms shared across platforms; domain-specific stories detail platform-particular implementations.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(attention_economy_extraction, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
