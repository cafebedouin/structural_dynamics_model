% ============================================================================
% CONSTRAINT STORY: value_alignment_drift
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_value_alignment_drift, []).

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
 *   constraint_id: value_alignment_drift
 *   human_readable: The Purpose Creep: Value Alignment Drift in Algorithmic Systems
 *   domain: technological/social
 *
 * SUMMARY:
 *   Value alignment drift describes the systemic failure where algorithmic
 *   systems designed to optimize for a proxy metric (engagement, watch time,
 *   clicks) gradually drift away from the original human value (well-being,
 *   informed decision-making, authentic social connection). The constraint is
 *   not that engagement and well-being conflict — they often align — but that
 *   optimizing directly for engagement as if it were well-being creates
 *   extraction mechanisms that suppress alternative optimization targets and
 *   concentrate power in platform operators who control the optimization
 *   function. Over the past decade (measurement interval 0-10), this
 *   constraint has intensified significantly. Early engagement optimization
 *   (2013-2016) was often framed as a coordination mechanism: platforms
 *   matching users with content they value. By 2019-2023, the performative
 *   layer thickened (moderation theater, well-being rhetoric without
 *   implementation), extractiveness increased (algorithmic lock-in, attention
 *   capture mechanisms became more sophisticated), and suppression deepened
 *   (opacity about optimization objectives, regulatory capture preventing
 *   transparency mandates). The constraint now exhibits all six DR types from
 *   different perspectives: snare for powerless users, rope for platform
 *   operators, tangled rope for regulators, piton for content moderation,
 *   scaffold for interoperability advocates, and false-summit mountain for
 *   observers who naturalize Goodhart's Law as inevitable.
 *
 * KEY AGENTS:
 *   - Platform operators (Meta, Google, TikTok, etc.): Institutional/arbitrage — beneficiary. Controls optimization function; captures attention value and advertising revenue.
 *   - Vulnerable users (children/adolescents): Powerless/trapped — primary victim. No exit from algorithmically mediated social coordination; cognitive development shaped by engagement optimization.
 *   - General adult users: Moderate/constrained — victim. Constrained exit (career/social necessity); bears attention extraction and behavioral capture.
 *   - Mental health advocates and child safety organizations: Organized/constrained — secondary victim and regulator. Fights for transparency and safety standards while constrained by platform resistance and lobbying.
 *   - Content creators: Powerful/mobile — mixed. Benefit from algorithmic amplification; also trapped by incentive warping and platform control of visibility.
 *   - Interoperability/digital rights coalition: Organized/constrained — organized victim building alternatives. Sees scaffold pathway (decentralized protocols, data portability) as sunset mechanism.
 *   - Content moderation workforce: Moderate/trapped — institutional victim. Enforces rules for performative safety while core optimization (engagement) remains unchanged.
 *   - Analytical observer: Analytical/analytical — risks naturalizing contingent design choice as inevitable natural law.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(value_alignment_drift, 0.58).
domain_priors:suppression_score(value_alignment_drift, 0.65).
domain_priors:theater_ratio(value_alignment_drift, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(value_alignment_drift, extractiveness, 0.58).
narrative_ontology:constraint_metric(value_alignment_drift, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(value_alignment_drift, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(value_alignment_drift, tangled_rope).
narrative_ontology:human_readable(value_alignment_drift, "The Purpose Creep: Value Alignment Drift in Algorithmic Systems").
narrative_ontology:topic_domain(value_alignment_drift, "technological/social").

domain_priors:requires_active_enforcement(value_alignment_drift).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(value_alignment_drift, platform_operators).
narrative_ontology:constraint_beneficiary(value_alignment_drift, engagement_optimizers).
narrative_ontology:constraint_victim(value_alignment_drift, user_cognitive_autonomy).
narrative_ontology:constraint_victim(value_alignment_drift, social_fabric_integrity).
narrative_ontology:constraint_victim(value_alignment_drift, child_mental_health).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: VULNERABLE USER (SNARE) — Children and adolescents cannot exit algorithmic engagement optimization: social life is mediated through platforms, peer validation is algorithmically curated, and cognitive development occurs within systems designed to maximize behavioral capture. No alternative infrastructure for social coordination exists. d≈0.95, f(d)≈1.42, σ=1.2 → χ≈0.98. Maximum extraction.
constraint_indexing:constraint_classification(value_alignment_drift, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: GENERAL ADULT USER (SNARE) — Adults nominally have exit options (abandon social media), but constrained by professional necessity (career visibility, client outreach) and social necessity (family coordination, community participation). The constraint extracts attention, cognitive cycles, and behavioral data while suppressing awareness of the extraction mechanism. d≈0.82, f(d)≈1.25, σ=1.2 → χ≈0.88.
constraint_indexing:constraint_classification(value_alignment_drift, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: MENTAL HEALTH/SAFETY ADVOCATES (TANGLED ROPE) — Child safety organizations, researchers, and regulators see both coordination benefits (platform transparency standards, age-verification protocols, algorithmic auditing) and asymmetric extraction (platforms resist genuine disclosure, fight regulations, lobby for weaker enforcement). The constraint exhibits genuine coordination function (safety standards) + enforcement mechanism (regulatory bodies) + asymmetric extraction (platforms retain optimization control). d≈0.58, f(d)≈0.75, σ=1.2 → χ≈0.52.
constraint_indexing:constraint_classification(value_alignment_drift, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 4: PLATFORM OPERATOR / ENGAGEMENT-OPTIMIZING VIEW (ROPE) — From the operator's perspective, the constraint appears as pure coordination: maximizing engagement solves the legitimate problem of matching users with content, retaining users in a competitive ecosystem, and funding platform operations through advertising. The platform experiences alignment drift (engagement ≠ well-being) as a side effect, not a feature. d≈0.08, f(d)≈-0.10, σ=1.2 → χ≈-0.07. Net beneficiary perspective.
constraint_indexing:constraint_classification(value_alignment_drift, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: CONTENT MODERATION THEATER (PITON) — Platform moderation systems are substantially performative: rule sets are complex, enforcement is inconsistent, and the core mechanism (engagement optimization) is not actually constrained by the nominal safety rules. Moderation theater persists because platforms must signal safety to regulators and advertisers, but the actual optimization target (engagement) remains unchanged. theater_ratio=0.68 reflects this performative layer. d≈0.08, f(d)≈-0.10, σ=1.2 → χ≈-0.07.
constraint_indexing:constraint_classification(value_alignment_drift, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: CONTENT CREATOR (TANGLED ROPE) — Creators benefit from algorithmic amplification (visibility, monetization, audience growth) but are also trapped by the engagement optimization: their incentive structure is warped to maximize platform metrics rather than substantive impact. They experience the constraint as hybrid: genuine coordination (matching creators with audiences) + extraction (platform controls algorithmic visibility, takes percentage of revenue, can demonetize without appeal). d≈0.50, f(d)≈0.65, σ=1.2 → χ≈0.45.
constraint_indexing:constraint_classification(value_alignment_drift, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, the constraint reflects Goodhart's Law: any metric used as a target ceases to be a good measure of the underlying value. Engagement is not intrinsically bad; it's a valid coordination metric. But optimizing directly for engagement as if it were well-being is a category error. This perspective risks naturalizing a contingent design choice (which metric to optimize) as an immutable constraint. However, the structural data (ε=0.58, suppression=0.65, theater=0.68, requires_active_enforcement=true) contradicts the mountain classification — the engine will compute this as a false summit.
constraint_indexing:constraint_classification(value_alignment_drift, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 8: INTEROPERABILITY COALITION (SCAFFOLD) — Organized actors advocating for data portability, algorithm transparency, and interoperable social protocols see the constraint as temporary: alternative platforms with different optimization targets (e.g., algorithmic choice, user agency) are technically feasible and could replace extraction-based models within 10-20 years if regulatory conditions enable them. This is a scaffold perspective with a genuine sunset clause: the extraction mechanism (platform lock-in + algorithmic opacity) loses force if interoperability standards mature. d≈0.40, f(d)≈0.40, σ=1.2 → χ≈0.19.
constraint_indexing:constraint_classification(value_alignment_drift, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(value_alignment_drift_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(value_alignment_drift, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(value_alignment_drift, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(value_alignment_drift, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(value_alignment_drift, TR),
    TR >= 0.70.

:- end_tests(value_alignment_drift_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high, increasing over interval. Initial value (0.15) reflects 2013-era engagement optimization framed as benign coordination. By mid-period (0.38, ~2018), awareness of attention capture emerged. Current value (0.58) reflects sophisticated engagement mechanisms (infinite scroll, notification timing, feed personalization) that extract user attention and behavioral data without obvious user benefit. The extraction is not maximal (0.70+) because some genuine coordination occurs — users do find valuable content — but the optimization target (engagement) progressively diverges from stated value (user well-being). Suppression (0.65): Moderate-high and structural. Platform operators suppress: awareness of optimization objectives (algorithmic opacity), alternative optimization targets (algorithm choice unavailable), and exit options (social necessity of platform participation). Suppression is less than maximum (0.90+) because transparency research, regulatory pressure, and competing platforms partially reduce it. Theater ratio (0.68): High and increasing. Moderation theater reflects the gap between nominal safety commitments (stated values) and actual optimization (engagement targets). Safety rules are enforced inconsistently; rules themselves are complex enough to be unverifiable by users. Theater increased over the interval as platforms added safety rhetoric without changing the core optimization function.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates a four-way perspectival divergence. (1) Platform operators see coordination: engagement optimization solves legitimate problem of matching users with content in competitive ecosystem. (2) Vulnerable users see extraction: the system captures attention and shapes cognitive development without consent or escape. (3) Organized safety advocates see hybrid: genuine coordination function (platform amplifies valuable content) + asymmetric extraction (optimization target prioritizes engagement over well-being). (4) Interoperability advocates see temporary problem: scaffold perspective identifies technical/regulatory pathways to alternative systems. The gap is not just about opinion — it reflects real structural differences in exit options and benefit distribution. Platform operators have arbitrage (can switch to different optimization without losing institutional value); vulnerable users have trapped exit (social necessity of participation). This gap is the mechanism by which the constraint persists despite rhetorical commitment to user well-being.
 *
 * DIRECTIONALITY LOGIC:
 *   Platform operators: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.10. Institutional perspective gains net benefit from optimization control; can shift targets if regulatory pressure mounts. Vulnerable users: Victim + trapped → d≈0.95, f(d)≈1.42. Maximum extraction; social mediation through platforms is non-optional. General adult users: Victim + constrained → d≈0.82, f(d)≈1.25. Significant extraction; exit is theoretically possible but practically constrained by professional/social necessity. Content creators: Both beneficiary (algorithmic amplification) + victim (incentive warping) + mobile (can theoretically migrate to alternative platforms) → d≈0.50, f(d)≈0.65. Mixed extraction. Safety advocates: Victim of optimization resistance + organized (can coordinate regulation) + constrained (platforms resist change) → d≈0.58, f(d)≈0.75. Moderate extraction; advocacy has leverage but incomplete control. Interoperability coalition: Organized + constrained + mobile (building alternatives) → d≈0.40, f(d)≈0.40. Low effective extraction because coalition has technical agency and sees exit pathway.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy through structural analysis of optimization targets. The core claim is: 'Engagement and well-being are not the same thing, and optimizing for one while claiming to optimize for the other creates extraction.' The mandatrophy is resolved by: (1) Identifying the proxy metric (engagement). (2) Documenting the value drift (engagement optimization progressively departed from user well-being as optimization became more sophisticated). (3) Showing that the extraction persists despite continued rhetorical commitment to user well-being (theater ratio = 0.68, platforms claim safety while optimizing engagement). The snare perspective (vulnerable users) is structurally sound: children experience capture and behavioral manipulation without alternative coordination infrastructure. The rope perspective (platform operators) is structurally sound: engagement optimization genuinely solves the coordination problem of matching users with content. The tangled rope perspective (safety advocates) is structurally sound: regulation requires both coordination function (shared safety standards) and asymmetric enforcement (platforms resist transparency). The false summit (mountain) is revealed by the structural data: well-being is not a law of nature; it's a design choice that platforms abandoned in favor of engagement. The scaffold is structurally sound: interoperability creates genuine alternatives that could operate under different optimization regimes. No single type is 'correct' — the presheaf of perspectives captures the full structural complexity.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    engagement_vs_wellbeing_causality,
    'Does engagement optimization directly cause mental health harms, or does it merely fail to prevent harms from other sources?',
    'Controlled experiments with platforms optimizing different metrics; longitudinal studies of mental health outcomes on high-engagement vs user-controlled feeds; analysis of confounding factors (economic stress, sleep deprivation, social isolation)',
    'If direct causation: snare classification is structurally sound (platform extracts harm). If correlation without causation: constraint might be more appropriately classified as rope (unintended side effect of legitimate coordination) or scaffold (fixable through different optimization targets).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(engagement_vs_wellbeing_causality, empirical, 'Causal relationship between engagement optimization and mental health harms').

omega_variable(
    algorithmic_transparency_feasibility,
    'Is algorithmic transparency sufficient to resolve alignment drift, or does engagement optimization create irreducible information asymmetries?',
    'Analysis of platforms with published algorithms (YouTube recommendation logic, TikTok algorithm transparency initiatives); user comprehension studies; audits of disclosed vs actual optimization behavior',
    'If transparency sufficient: problem is coordination (Rope from most perspectives, with education solving asymmetry). If asymmetry irreducible: extraction persists even with disclosure (Snare persists).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(algorithmic_transparency_feasibility, empirical, 'Whether algorithmic transparency can resolve alignment drift').

omega_variable(
    interoperability_displacement_timeline,
    'Can decentralized/interoperable alternatives realistically achieve user migration within 10-20 years, or is platform lock-in more structurally sticky than the scaffold perspective assumes?',
    'Historical analysis of network effects and switching costs; technical feasibility of ActivityPub, Bluesky, and competing protocols; regulatory catalyst scenarios (EU Digital Markets Act, comparable legislation)',
    'If displacement feasible: scaffold sunset is real structural feature. If lock-in is sticky beyond 20 years: scaffold is aspirational, and constraint should be reclassified as snare or piton (inertial extraction).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(interoperability_displacement_timeline, empirical, 'Feasibility of interoperable alternatives displacing extractive platforms').

omega_variable(
    child_protection_vs_autonomy_boundary,
    'At what age/cognitive development threshold does paternalistic algorithm design transition from protection to extraction?',
    'Developmental psychology research on digital literacy, autonomy, and cognitive manipulation resistance; comparative analysis of age-gated systems; user agency studies across age groups',
    'If boundary is clear: protection-focused design for children differs from adult extraction (two separate constraints). If boundary is diffuse: all age groups experience extraction, undermining the autonomy framing.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(child_protection_vs_autonomy_boundary, empirical, 'Developmental threshold between protection and extraction').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(value_alignment_drift, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vad_tr_t0, value_alignment_drift, theater_ratio, 0, 0.25).
narrative_ontology:measurement(vad_tr_t5, value_alignment_drift, theater_ratio, 5, 0.5).
narrative_ontology:measurement(vad_tr_t10, value_alignment_drift, theater_ratio, 10, 0.68).

% Extraction over time
narrative_ontology:measurement(vad_be_t0, value_alignment_drift, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(vad_be_t5, value_alignment_drift, base_extractiveness, 5, 0.38).
narrative_ontology:measurement(vad_be_t10, value_alignment_drift, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(value_alignment_drift, information_standard).
narrative_ontology:boltzmann_floor_override(value_alignment_drift, 0.35).
narrative_ontology:affects_constraint(value_alignment_drift, social_recommendation_amplification).
narrative_ontology:affects_constraint(value_alignment_drift, behavioral_addiction_loop).
narrative_ontology:affects_constraint(value_alignment_drift, attention_economy_extraction).

% DUAL FORMULATION NOTE:
% Value alignment drift is upstream of specific harms (addiction, polarization, mental health decline) but represents a distinct structural constraint on the optimization function itself. The downstream constraints have their own ε values reflecting specific failure modes; value alignment drift has ε=0.58 reflecting the general proxy metric problem that enables all specific harms.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(value_alignment_drift, institutional, 0.12).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
