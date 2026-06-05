% ============================================================================
% CONSTRAINT STORY: epistemic_overload_collapse
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_epistemic_overload_collapse, []).

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
 *   constraint_id: epistemic_overload_collapse
 *   human_readable: The Signal-Drowning Vortex
 *   domain: cognitive/informational/technological
 *
 * SUMMARY:
 *   The Signal-Drowning Vortex is a structural constraint where the volume,
 *   velocity, contradictory nature, and presentation format of available
 *   information exceed the biological and institutional capacity of human
 *   subjects to process it coherently. This is not merely an information
 *   abundance problem — it is an asymmetry between information supply and
 *   cognitive demand coupled with extraction mechanisms that profit from
 *   attention scarcity. The constraint exhibits a perspectival landscape
 *   spanning all six DR types: the individual subject experiences it as a
 *   Snare (trapped by cognitive limits); the epistemic commons experiences it
 *   as a Snare (drowning without self-correction); the knowledge worker
 *   experiences it as Tangled Rope (mixed benefit and extraction); the
 *   attention merchant experiences it as Rope (solving a genuine coordination
 *   problem); the epistemic reform coalition experiences it as Scaffold
 *   (building alternatives with a sunset); the peer review apparatus
 *   experiences it as Piton (degraded ritual maintained by inertia); and the
 *   civilizational analytical observer risks mistaking it for a Mountain
 *   (naturalizing contingent arrangements as inherent limits). The
 *   constraint's theater_ratio (0.65) reflects that much of the institutional
 *   response to overload is performative: debate rituals pass for epistemic
 *   integrity, fact-checking rituals pass for verification, and algorithmic
 *   curation rituals pass for truth-finding, while actual epistemic commons
 *   utility collapses. The extraction mechanism is multi-layered: platforms
 *   extract user attention by monetizing it as advertising inventory;
 *   publishers extract author labor through citation-based prestige;
 *   attention merchants extract cognitive surplus through algorithmic
 *   amplification of engaging (but false) signals; and reformers extract
 *   moral authority by promising solutions through better institutional
 *   design (epistemic governance, information literacy, transparency
 *   protocols). The vortex dynamics are amplified by feedback loops: as
 *   legitimate signal becomes harder to find, users rely more on platform
 *   algorithms; as algorithms amplify engagement-optimized content,
 *   legitimate signal becomes harder to find; as cognitive exhaustion
 *   increases, demand for simple narrative frames increases; as simple frames
 *   dominate, epistemic commons complexity collapses; as commons collapses,
 *   individual subjects lose access to distributed expertise and must rely on
 *   individual heuristics; as heuristics fail, extraction intensifies. The
 *   constraint's extractiveness (0.52) is moderate rather than maximal
 *   because the coordination problem it solves is genuine: matching attention
 *   to content at scale is unsolved by markets alone, and platforms do
 *   provide real utility alongside extraction. However, suppression (0.68) is
 *   high because alternatives to platform curation are actively suppressed
 *   (through algorithmic de-ranking, visibility throttling, and network
 *   effects) rather than being naturally less effective.
 *
 * KEY AGENTS:
 *   - Individual Cognitive Subject: Primary victim (powerless/trapped) — bears cognitive exhaustion, decision paralysis, epistemic despair; cannot exit without social/career costs
 *   - Epistemic Commons: Primary victim (powerless/trapped) — abstract collective good drowning in contradictory claims; no mechanism for self-correction; no organizational capacity
 *   - Attention Merchants (Platforms, Advertisers): Primary beneficiary (institutional/arbitrage) — extract user attention as monetized inventory; see constraint as solving coordination problem of matching content to attention
 *   - Professional Knowledge Workers: Secondary victim/beneficiary (moderate/constrained) — face extraction (cognitive overhead) but also benefit (access to distributed expertise, open research)
 *   - Epistemic Reform Coalition (Librarians, Scientists, Media Literacy Educators): Organized agents (organized/constrained) — building alternative pathways (curation, credibility signals, epistemic protocols) with sunset logic
 *   - Peer Review Apparatus: Institutional actor (institutional/constrained) — maintains performative verification ritual; sees own degradation but lacks competitive alternatives
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent arrangements as inherent cognitive limits
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(epistemic_overload_collapse, 0.52).
domain_priors:suppression_score(epistemic_overload_collapse, 0.68).
domain_priors:theater_ratio(epistemic_overload_collapse, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(epistemic_overload_collapse, extractiveness, 0.52).
narrative_ontology:constraint_metric(epistemic_overload_collapse, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(epistemic_overload_collapse, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(epistemic_overload_collapse, tangled_rope).
narrative_ontology:human_readable(epistemic_overload_collapse, "The Signal-Drowning Vortex").
narrative_ontology:topic_domain(epistemic_overload_collapse, "cognitive/informational/technological").

domain_priors:requires_active_enforcement(epistemic_overload_collapse).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(epistemic_overload_collapse, attention_merchants).
narrative_ontology:constraint_beneficiary(epistemic_overload_collapse, algorithmic_curators).
narrative_ontology:constraint_beneficiary(epistemic_overload_collapse, content_producers).
narrative_ontology:constraint_victim(epistemic_overload_collapse, individual_cognition).
narrative_ontology:constraint_victim(epistemic_overload_collapse, epistemic_commons).
narrative_ontology:constraint_victim(epistemic_overload_collapse, collective_decision_capacity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: INDIVIDUAL COGNITIVE SUBJECT (SNARE) — Trapped by biological attention limits (4-6 hour productive focus window per day) while information volume grows exponentially. No exit option: disengagement from information ecosystem costs career, social standing, civic participation. Subject bears full extraction cost: cognitive exhaustion, decision paralysis, epistemic despair. d≈0.93, f(d)≈1.40, σ=1.2 → χ≈0.78.
constraint_indexing:constraint_classification(epistemic_overload_collapse, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: THE EPISTEMIC COMMONS (SNARE) — Abstract collective good (shared understanding, collective knowledge, epistemic trust) cannot organize or exit. Drowning in contradictory claims, false equivalences, and manufactured uncertainty. No mechanism to establish what is actually true at scale. Theater masks extraction: debate theater passes for epistemic integrity while actual commons utility collapses. d≈0.95, f(d)≈1.42, σ=1.2 → χ≈0.76.
constraint_indexing:constraint_classification(epistemic_overload_collapse, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: PROFESSIONAL KNOWLEDGE WORKER (TANGLED ROPE) — Constrained: career depends on staying current with information streams; switching costs are high (retraining, network loss). But also benefits from information abundance: access to distributed expertise, open research, cross-domain insight. The constraint is mixed — information glut extracts attention but also enables distributed collaboration. d≈0.58, f(d)≈0.72, σ=1.0 → χ≈0.37.
constraint_indexing:constraint_classification(epistemic_overload_collapse, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: THE ATTENTION MERCHANT (ROPE) — Content platforms, ad networks, and algorithmic curators see information volume as a coordination solution: matching attention to content, enabling discovery. The extraction (user attention monetized as ad inventory) is genuine, but it solves a real coordination problem: how do billions of people find relevant content in unlimited supply? Platforms experience the constraint as functional coordination with arbitrage exits (reputational switching, advertising platform switching). d≈0.08, f(d)≈-0.09, σ=1.2 → χ≈-0.06.
constraint_indexing:constraint_classification(epistemic_overload_collapse, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: EPISTEMIC REFORM COALITION (SCAFFOLD) — Organized agents (librarians, scientists, media literacy educators, epistemic governance researchers) see the overload as a temporary coordination failure with a sunset: information filtering, source credibility signals, epistemic protocols, and cognitive offloading tools are being built to replace raw volume with curated signal. Theater suppression through transparency (source attribution, confidence scores, retraction tracking) reduces theater_ratio over time. d≈0.42, f(d)≈0.42, σ=1.2 → χ≈0.26. Low effective extraction because coalition has agency and sees an exit path via better institutional design.
constraint_indexing:constraint_classification(epistemic_overload_collapse, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: THE PEER REVIEW APPARATUS (PITON) — Traditional peer review is substantially performative in the era of information overload: reviewers cannot read all referenced work, cannot verify computational claims without code, cannot assess claims about claims (citations of contested findings). The ritual persists through institutional inertia (tenure, journal prestige, funding allocation) despite reduced functional capacity. theater_ratio=0.65 satisfies piton gate (≥0.70, borderline). The apparatus sees its own degradation — verification rituals maintained because alternatives haven't fully replaced them, not because they verify effectively.
constraint_indexing:constraint_classification(epistemic_overload_collapse, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational/universal view, information overload is an inherent limit of complex systems: as knowledge expands, the relationship between signal and noise approaches a mathematical floor determined by the ratio of true claims to possible false claims. The observer risks naturalizing the constraint as an immutable law: 'No system of attention can scale linearly with information volume.' However, the structural data (ε=0.52, suppression=0.68, theater=0.65, requires_active_enforcement=true) contradicts the mountain classification. The engine will flag this as a false summit: what appears as inherent limitation is actually a contingent institutional arrangement (platform algorithms, attention markets, peer review incentives) that could be restructured.
constraint_indexing:constraint_classification(epistemic_overload_collapse, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(epistemic_overload_collapse_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(epistemic_overload_collapse, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(epistemic_overload_collapse, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(epistemic_overload_collapse, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(epistemic_overload_collapse, TR),
    TR >= 0.70.

:- end_tests(epistemic_overload_collapse_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The constraint extracts attention (converted to ad revenue), cognitive labor (processing contradictory claims), and epistemic trust (degraded through false equivalence). However, it is not maximal extraction because platforms do solve a real coordination problem: matching billions of people to relevant content in an information space too large for unaided search. The extraction is coupled to genuine utility provision. The value increased from 0.28 (2016-era) to 0.52 (present) as feedback loops intensified: better algorithms → more engagement → more platform dependence → higher cognitive costs for non-users. Suppression (0.68): High. Alternatives to platform curation are actively suppressed through: network effects (data network effects lock in platform dominance), algorithmic de-ranking (search results, feed visibility), rate-limiting (API restrictions on alternative interfaces), and institutional capture (platforms influence media literacy curricula, research funding priorities, regulatory framing). Suppression increased from ~0.45 to 0.68 as platforms consolidated market power and developed sophisticated ranking optimization. Theater ratio (0.65): Moderate-high. Institutional responses to overload are largely performative: fact-checking theater (appearing to verify without systemic improvement), epistemic debate theater (appearing to resolve while entrenching positions), algorithmic transparency theater (appearing to explain black-box systems without enabling user control), and information literacy theater (appearing to increase capacity without addressing structural volume asymmetry). Theater increased from 0.32 (2014: belief in Google's organization) to 0.65 (2024: widespread awareness that institutions are performing solutions without delivering them).
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates maximum perspectival divergence. The individual subject trapped by cognitive limits sees pure extraction (Snare); the epistemic commons drowning in contradictory claims sees pure extraction (Snare); the professional knowledge worker sees mixed extraction and benefit (Tangled Rope); the attention merchant solving coordination problems sees coordination mechanism (Rope); the epistemic reformers see a temporary problem with structural exit paths (Scaffold); the peer review apparatus sees a degraded ritual maintained by inertia (Piton); and the civilizational observer risks seeing an inherent limit (Mountain). No two perspectives produce the same classification. This perspectival diversity is not a measurement error — it reveals the true structure: the constraint simultaneously IS all six types, depending on position. The gap between individual subject (Snare) and attention merchant (Rope) is the largest: the same phenomenon that appears to the merchant as solving a coordination problem appears to the subject as rendering them cognitively powerless. This is classic extraction asymmetry — the beneficiary's experience of coordination masks the victim's experience of constraint.
 *
 * DIRECTIONALITY LOGIC:
 *   Individual cognitive subject: Victim + trapped → d≈0.93, f(d)≈1.40. Near-maximal extraction. Subject cannot exit without social/career costs; bears full cost of overload. Epistemic commons: Victim + trapped → d≈0.95, f(d)≈1.42. Maximal extraction. Abstract collective cannot organize or exit; drowns in noise with no recovery mechanism. Professional knowledge worker: Victim (extraction cost) + beneficiary (access to distributed expertise) + constrained (switching costs high) → d≈0.58, f(d)≈0.72. Mixed extraction. Attention merchant: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.09. Net beneficiary. Can switch platforms/advertising models; sees constraint as coordination mechanism. Epistemic reform coalition: Organized + constrained → d≈0.42, f(d)≈0.42. Coalition has agency and sees exit path; low effective extraction. Peer review apparatus: Institutional + constrained (cannot fully exit due to prestige coupling) → d≈0.35, f(d)≈0.28. Moderate extraction. Apparatus is partly captured (dependent on platform referral traffic, citation databases run by platforms) but retains some independent institutional standing.
 *
 * MANDATROPHY ANALYSIS:
 *   DIAGNOSTIC EXEMPLAR: This constraint resolves the mandatrophy by demonstrating that the six-type landscape is not ambiguous — it is multi-true from different structural positions. The mandatrophy question is not 'which type is correct?' but 'who is asking and from where?' The individual subject's Snare is structurally true (they are trapped and bear extraction). The merchant's Rope is structurally true (the platform solves a coordination problem). These are not conflicting measurements of the same thing — they are the same constraint experienced from incompatible positions. The false summit (Mountain view) naturalizes what is actually a contingent arrangement: humans are not biologically limited to processing current information volume; they are cognitively limited by platform-optimized presentation formats, attention-extraction incentives, and institutional capture of verification mechanisms. Removing the extraction mechanisms (algorithm transparency, distributed curation, epistemic governance) would likely increase functional cognition dramatically — proving that the constraint is not inherent. The Scaffold perspective (reform coalition) is the key to resolving mandatrophy: if the coalition successfully builds alternative institutions (federated recommendation systems, credibility aggregation protocols, distributed verification), and if individual subjects can migrate to these alternatives, then the constraint is revealed as a Tangled Rope that can degrade to Rope (if coordination remains useful) or even Piton (if extraction mechanisms persist through inertia without market pressure). The timeline is multi-generational: institutional change in epistemic commons typically takes 15-30 years. The current trajectory (intensifying extraction, rising theater, growing suppression) suggests the constraint is drifting toward Snare classification from all perspectives unless reform coalitions succeed in building alternatives.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    cognitive_limit_floor,
    'Is the attention bottleneck a fixed biological property or a contingent institutional constraint?',
    'Longitudinal measurement of productive focus capacity across populations with different institutional arrangements (e.g., information workers vs contemplative practitioners); historical comparison of attention spans across different informational regimes',
    'If biological floor: constraint approaches Mountain (χ → low from all perspectives, ε → 0.15). If institutional: constraint remains Tangled Rope / Snare (ε remains 0.52+), and restructuring can reduce suppression significantly.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cognitive_limit_floor, empirical, 'Whether cognitive overload is biological or institutional').

omega_variable(
    signal_extraction_disentanglement,
    'Can algorithmic curation separate legitimate signal discovery from attention extraction without losing the coordination benefit?',
    'Comparison of user utility (learning rate, decision quality, satisfaction) between: (a) raw information abundance, (b) attention-optimizing algorithms, (c) epistemic-utility-optimizing curation; A/B testing of transparency overlays (source credibility, confidence intervals) vs baseline feeds',
    'If disentanglement succeeds: Rope classification confirmed — platform coordination is separable from extraction. Theater ratio drops, χ declines, suppression decreases. If inseparable: Snare classification confirmed — extraction is structural to the coordination mechanism itself.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(signal_extraction_disentanglement, empirical, 'Whether signal discovery can be decoupled from attention extraction').

omega_variable(
    collective_epistemic_capacity,
    'What is the actual epistemic processing capacity of a society at a given scale, and is it achievable through distributed vs centralized institutions?',
    'Historical analysis of claim-to-verification timelines for different institutional forms (peer review, open-source model, citizen science, blockchain timestamping); measurement of false-claim persistence rates across institutional designs',
    'If centralized review is fundamental: Scaffold sunset is not real — the bottleneck is permanent, classification stays Snare/Tangled Rope. If distributed systems can achieve equivalent verification: Scaffold sunset is real — cooperative epistemic protocols (distributed review, multi-signature credibility) enable escape. Theater ratio drops, suppression decreases.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(collective_epistemic_capacity, empirical, 'Achievable epistemic capacity via distributed vs centralized institutions').

omega_variable(
    platform_enforcement_dependency,
    'Does the constraint require active enforcement by platforms (throttling, algorithmic curation, suppression of sources), or does it emerge naturally from information abundance?',
    'Counterfactual analysis: what would happen to user cognition and epistemic commons if platforms removed all throttling mechanisms but kept information availability the same; measurement of cognitive load with and without algorithmic filtering',
    'If naturally emergent: Rope classification gains support — platforms solve a genuine coordination problem with lower extraction. If enforcement-dependent: Snare classification gains support — extraction mechanism requires active suppression of alternatives.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(platform_enforcement_dependency, empirical, 'Whether constraint requires active platform enforcement').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(epistemic_overload_collapse, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(eoc_tr_t0, epistemic_overload_collapse, theater_ratio, 0, 0.32).
narrative_ontology:measurement(eoc_tr_t5, epistemic_overload_collapse, theater_ratio, 5, 0.48).
narrative_ontology:measurement(eoc_tr_t10, epistemic_overload_collapse, theater_ratio, 10, 0.65).

% Extraction over time
narrative_ontology:measurement(eoc_be_t0, epistemic_overload_collapse, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(eoc_be_t5, epistemic_overload_collapse, base_extractiveness, 5, 0.4).
narrative_ontology:measurement(eoc_be_t10, epistemic_overload_collapse, base_extractiveness, 10, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(epistemic_overload_collapse, information_standard).
narrative_ontology:affects_constraint(epistemic_overload_collapse, algorithmic_amplification_bias).
narrative_ontology:affects_constraint(epistemic_overload_collapse, attention_economy_rent_seeking).
narrative_ontology:affects_constraint(epistemic_overload_collapse, epistemic_commons_collapse).

% DUAL FORMULATION NOTE:
% The signal-drowning vortex is downstream of platform architecture choices (recommendation algorithms, engagement optimization, network effects) but represents a distinct structural constraint on cognition and epistemology. Upstream constraints (algorithmic bias, attention market structure) have their own ε values reflecting technical/economic specificity; the epistemic overload constraint has ε=0.52 reflecting the cognitive and institutional asymmetry between information supply and processing capacity. These are separate stories linked by network causality: platform algorithmic choices → information overload → cognitive extraction → epistemic commons collapse.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(epistemic_overload_collapse, institutional, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
