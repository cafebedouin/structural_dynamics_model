% ============================================================================
% CONSTRAINT STORY: cultural_legitimacy_gatekeeping
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_cultural_legitimacy_gatekeeping, []).

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
 *   constraint_id: cultural_legitimacy_gatekeeping
 *   human_readable: Cultural Legitimacy Gatekeeping
 *   domain: cultural/institutional/social
 *
 * SUMMARY:
 *   Cultural legitimacy gatekeeping is the institutional system through which
 *   societies determine which creative works, artistic contributions,
 *   intellectual claims, and cultural narratives merit preservation,
 *   distribution, and canonical status. This constraint operates through
 *   established institutions (museums, universities, publishing houses, award
 *   bodies, critical review infrastructure) that control access to prestige,
 *   distribution channels, and material rewards. The constraint exhibits
 *   extraction (gatekeepers capture prestige and authority while marginal
 *   creators labor without compensation or attribution) alongside
 *   coordination (the institutions do solve the real problem of aggregating
 *   judgment about cultural value). The theater ratio (0.68) reflects that
 *   legitimacy assessment is substantially performative: expert authority,
 *   textual analysis, historical precedent, and peer consensus are the
 *   mechanisms, but their predictive validity is contested. The
 *   extractiveness has increased over the interval (0.42 → 0.58) as digital
 *   platforms have proliferated but gatekeeping institutions have
 *   concentrated their cultural authority in response. The constraint is
 *   currently a Tangled Rope globally: genuine coordination function paired
 *   with asymmetric extraction. However, alternative legitimacy mechanisms
 *   (social media followings, algorithmic recommendation, decentralized
 *   community curation, independent publishing) are building scaffolding that
 *   could reduce or replace gatekeeping extraction within a generational
 *   timescale.
 *
 * KEY AGENTS:
 *   - Marginal Creators: Primary victims (powerless/trapped) — excluded from traditional legitimacy channels; labor circulates without attribution; structurally blocked from canonical status
 *   - Alternative Producers: Secondary victims (moderate/constrained) — face high institutional barriers but can access alternative distribution; bear cost of excluded status while building alternatives
 *   - Established Cultural Institutions: Primary beneficiaries (institutional/arbitrage) — museums, universities, publishing houses, festivals capture prestige authority; can shift criteria to maintain cultural centrality
 *   - Credentialed Arbiters: Secondary beneficiaries (powerful/mobile) — critics, curators, professors, editors hold gatekeeping power; experience constraint as legitimate professional practice
 *   - Tradition-Bound Canon: Institutional actor (institutional/arbitrage) — explicit written criteria for legitimacy persist through inertia; increasingly disconnected from actual cultural value production
 *   - Emerging Alternative Networks: Organized agents (organized/constrained) — social media platforms, independent publishers, algorithmic curators, open-source communities building legitimacy mechanisms outside traditional institutions
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing gatekeeping as inherent to complex societies
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(cultural_legitimacy_gatekeeping, 0.58).
domain_priors:suppression_score(cultural_legitimacy_gatekeeping, 0.65).
domain_priors:theater_ratio(cultural_legitimacy_gatekeeping, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(cultural_legitimacy_gatekeeping, extractiveness, 0.58).
narrative_ontology:constraint_metric(cultural_legitimacy_gatekeeping, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(cultural_legitimacy_gatekeeping, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(cultural_legitimacy_gatekeeping, tangled_rope).
narrative_ontology:human_readable(cultural_legitimacy_gatekeeping, "Cultural Legitimacy Gatekeeping").
narrative_ontology:topic_domain(cultural_legitimacy_gatekeeping, "cultural/institutional/social").

domain_priors:requires_active_enforcement(cultural_legitimacy_gatekeeping).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(cultural_legitimacy_gatekeeping, established_cultural_institutions).
narrative_ontology:constraint_beneficiary(cultural_legitimacy_gatekeeping, credentialed_arbiters).
narrative_ontology:constraint_victim(cultural_legitimacy_gatekeeping, marginal_creators).
narrative_ontology:constraint_victim(cultural_legitimacy_gatekeeping, alternative_cultural_producers).
narrative_ontology:constraint_victim(cultural_legitimacy_gatekeeping, subaltern_narratives).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: MARGINAL CREATOR (SNARE) — Trapped by lack of access to legitimacy channels. Cannot exit without abandoning cultural production. Experiences maximum extraction: their labor and innovation circulate without attribution or compensation while gatekeepers extract prestige and authority. No alternative verification pathway available.
constraint_indexing:constraint_classification(cultural_legitimacy_gatekeeping, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: ALTERNATIVE PRODUCER (TANGLED ROPE) — Constrained by institutional barriers (festivals, grant bodies, critical review infrastructure) but benefits from emerging alternative platforms (social media, independent publishing, digital distribution). Extraction is real but surmountable with high cost. Mixed coordination-extraction relationship: benefits from some legitimacy frameworks while being excluded from others.
constraint_indexing:constraint_classification(cultural_legitimacy_gatekeeping, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: ESTABLISHED INSTITUTION (ROPE) — Experiences the constraint as pure coordination mechanism: museums, universities, publishing houses, and award bodies solve the collective action problem of deciding which cultural artifacts merit preservation and attention. Arbitrage exit available — can shift criteria and discover new domains. Net beneficiary, but experiences constraint as legitimate curatorship function.
constraint_indexing:constraint_classification(cultural_legitimacy_gatekeeping, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: CREDENTIALED ARBITER (TANGLED ROPE) — Holds power through perceived expertise and access to distribution channels. Experiences constraint as legitimate professional practice but also as inertial institutional role. Can exit (move to new domains, adopt new criteria) but faces professional cost. Benefits from the gatekeeping function while often genuinely coordinating taste selection.
constraint_indexing:constraint_classification(cultural_legitimacy_gatekeeping, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: TRADITION-BOUND CANON (PITON) — The explicit written criteria for legitimacy (literary canon, museum acquisition standards, critical frameworks) persist through institutional inertia even as they demonstrably fail to identify emerging high-value cultural work. Theater is high because legitimacy assessment is largely performative (expert authority) rather than predictive. The mechanism (textual analysis, historical precedent, peer consensus) requires constant activation but produces diminishing coherence.
constraint_indexing:constraint_classification(cultural_legitimacy_gatekeeping, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: EMERGING ALTERNATIVE LEGITIMACY NETWORK (SCAFFOLD) — Distributed digital platforms, community-based curation, algorithmic recommendation, and decentralized credentialing (social media following, open-source reputation, collaborative feedback) are building parallel legitimacy mechanisms with lower institutional overhead. Constrained by network effects (the old system still captures prestige) but with visible sunset trajectory. Extraction declines as alternative verification pathways mature.
constraint_indexing:constraint_classification(cultural_legitimacy_gatekeeping, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, some form of legitimacy gatekeeping may be inherent to complex societies: distinguishing signal from noise in cultural production requires aggregating judgments, and aggregation requires some actors to occupy arbiter roles. This perspective risks naturalizing what is actually a contingent institutional arrangement. The false summit detector identifies this as misclassification.
constraint_indexing:constraint_classification(cultural_legitimacy_gatekeeping, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(cultural_legitimacy_gatekeeping_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(cultural_legitimacy_gatekeeping, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(cultural_legitimacy_gatekeeping, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(cultural_legitimacy_gatekeeping, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(cultural_legitimacy_gatekeeping, TR),
    TR >= 0.70.

:- end_tests(cultural_legitimacy_gatekeeping_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high, and increasing. The original research group (established institutions) captures prestige, authority, and material rewards (grants, donations, institutional power) during the period when they control cultural legitimacy. The extraction is not total because some coordination value is real: institutions do identify culturally valuable work and facilitate its preservation. But extractiveness has grown (0.42 → 0.58 over 20 years) as digital platforms have proliferated without displacing institutional authority — gatekeeping has responded by concentrating rather than opening, making the extraction mechanism more visible. Suppression (0.65): High. Barriers to alternative legitimacy include: lack of access to distribution infrastructure (publishing houses, museum exhibition space, grant funding), absence of critical review infrastructure for marginal work, publication bias against work outside institutional frameworks, and the network effect that canonical status still matters for material outcomes. However, suppression is not complete — digital platforms and independent publishing have created partial workarounds, making suppression high but not total. Theater ratio (0.68): High and increasing. Legitimacy assessment relies on expert authority, textual close reading, historical precedent, and peer consensus — all performative mechanisms whose validity cannot be empirically tested until long after the judgment is made. A critic cannot prove that a work is 'important' — they can only perform the authority that names it so. The theater has increased as the gap between gatekeeping criteria (literary tradition, historical precedent, institutional prestige) and actual cultural adoption (social media, streaming algorithms, grassroots fandom) has widened. This gap produces the piton perspective: the explicit canon persists through inertia even as real cultural value increasingly circulates outside it.
 *
 * PERSPECTIVAL GAP:
 *   Seven distinct readings of the same structural data. The constraint is simultaneously Snare (victim view), Tangled Rope (alternative view), Rope (beneficiary view), Piton (tradition view), Scaffold (emerging alternative view), and falsely Mountain (civilizational naturalization view). The perspectival gap reveals that all six types are legitimate; none is 'the' answer. The gap also diagnoses the constraint's evolution: extractiveness is increasing (0.42 → 0.58) because the traditional gatekeeping system is concentrating rather than opening in response to digital disruption. If the scaffold sunset actually fires — if alternative legitimacy mechanisms mature and displace institutional gatekeeping — the constraint will shift from Tangled Rope to Scaffold globally, and then gradually toward Rope or even dissolution as the coordination function moves to distributed platforms. If institutional gatekeeping successfully resists displacement (concentrating authority to maintain relevance), the constraint will remain Tangled Rope or shift toward Snare as suppression increases and exit pathways close.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality derives from the agent's structural position. Marginal creators (powerless/trapped) have high d (~0.95): they bear extraction, have no exit, and experience maximum suppression. Alternative producers (moderate/constrained) have moderate-high d (~0.70): they face significant barriers but can exit at cost. Established institutions (institutional/arbitrage) have very low d (~0.05 to 0.15): they benefit from the constraint, have arbitrage options, and drive extraction toward others. Credentialed arbiters (powerful/mobile) have low-to-moderate d (~0.30 to 0.45): they benefit professionally but face some career constraint from the role. The analytical observer (analytical/analytical) has canonical d (~0.72): sees structure from outside, positioned as critical but not embedded in the extraction flow. The directionality derivation feeds into the sigmoid f(d) to produce experienced extractiveness chi. Marginal creators experience chi amplified by high f(d); institutions experience chi suppressed by low f(d). The scope modifier σ(S) further scales chi by spatial extent: global reach (σ=1.2) means the extraction mechanism operates at planetary scale, amplifying effective extractiveness for both beneficiaries and victims.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by demonstrating why all six types are not equally valid: the established institution's Rope perspective is real (they do coordinate cultural value) but reflects only their structural position. The marginal creator's Snare perspective is equally real and reflects their structural position. The constraint is fundamentally hybrid (Tangled Rope) — it has both a genuine coordination function (aggregating judgment about cultural value) and asymmetric extraction (gatekeepers capture prestige while marginal creators labor without compensation). The mandatrophy resolution: acknowledge that both functions are real, that the ratio of coordination to extraction varies by perspective, and that the constraint is sustainably Tangled Rope only if extraction does not exceed coordination value. The measurements show extractiveness increasing without corresponding coordination gain, which signals mandatrophy drift: the constraint is trending toward Snare (pure extraction with minimal coordination function) unless alternative legitimacy mechanisms reduce institutional extraction. The piton perspective (degraded ritual) is diagnostically important: it shows that the explicit coordination mechanism (peer review, critical standards, canonical criteria) is increasingly performative, which compounds mandatrophy risk. If the coordination function is already theater-driven, any increase in extraction without coordination increase makes the constraint indefensible. The scaffold perspective provides a positive mandatrophy resolution path: if distributed alternative legitimacy mechanisms actually work (if the open-source hypothesis that 'many eyes make quality visible' succeeds for cultural work), then extractive gatekeeping becomes unnecessary, and the constraint sunsets into Rope (pure coordination) or dissolves entirely.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    gatekeeping_vs_curation,
    'Is cultural legitimacy gatekeeping a coordination mechanism (curating signal from noise) or an extraction mechanism (controlling access for rent-seeking)?',
    'Measure correlation between arbiter selections and actual adoption/value in retrospective analysis. Do gatekeepers identify work that becomes canonical? Or do they exclude work that later proves valuable? Track false positives (gatekept work that disappears) and false negatives (excluded work that succeeds outside the system).',
    'If primarily coordination: classification shifts to Rope from most perspectives. If primarily extraction: Snare from victim perspectives is reinforced. Current evidence mixed — some gatekeepers have good predictive validity; others are strongly biased toward their own networks.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(gatekeeping_vs_curation, empirical, 'Gatekeeping as coordination vs extraction function').

omega_variable(
    suppression_structural_vs_cultural,
    'Is the measured suppression (0.65) primarily structural (legal barriers, economic dependency, lack of distribution channels) or cultural (internalized beliefs that marginal work is less worthy)?',
    'Compare suppression trajectory post-exit from gatekeeping system. Do barriers persist after alternative distribution is available? If internalized: marginal creators maintain suppression beliefs even after access to alternative platforms. If structural: suppression drops sharply when barriers are removed.',
    'If internalized: constraint includes identity_locked component — creators have internalized the gatekeeping apparatus''s evaluation. If structural: identity_locked is inappropriate; exit is theoretically possible but economically costly.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(suppression_structural_vs_cultural, empirical, 'Suppression mechanism: structural vs internalized cultural belief').

omega_variable(
    alternative_platform_sustainability,
    'Can distributed digital legitimacy mechanisms (algorithmic recommendation, community curation, social media following) sustain cultural evaluation at scale, or do they degrade into noise?',
    'Longitudinal analysis of alternative platform adoption and quality metrics. Track: (a) whether works successful on alternative platforms achieve lasting cultural value, (b) whether alternative mechanisms develop their own gatekeeping functions, (c) whether network effects allow alternatives to compete with traditional institutions.',
    'If sustainable: scaffold sunset is real — alternative legitimacy pathways are genuinely replacing gatekeeping. If degraded: many-eyes logic fails for cultural work, gatekeeping remains extractive necessity, and scaffold perspective is aspirational rather than structural.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_platform_sustainability, empirical, 'Sustainability of alternative legitimacy mechanisms').

omega_variable(
    identity_lock_cultural_internalization,
    'To what extent do marginal creators internalize gatekeeping institutions'' legitimacy criteria, making exit psychologically rather than structurally difficult?',
    'Qualitative analysis of marginal creator self-perception. Do creators who exit gatekeeping systems maintain belief that traditional institutional validation is more ''real'' or ''legitimate''? Track: (a) creator statements about their own work''s value when outside institutional frameworks, (b) whether successful alternative-platform creators seek traditional institutional validation afterward, (c) whether cultural workers accept lower material reward to maintain institutional legitimacy.',
    'If strong identity lock: constraint includes cognitive capture component. Exit options should be reclassified as identity_locked rather than constrained for some agents. Creates second-order extraction: gatekeeping extracts both material reward and identity validation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_cultural_internalization, empirical, 'Identity internalization of gatekeeping legitimacy criteria').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(cultural_legitimacy_gatekeeping, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cultleg_tr_t0, cultural_legitimacy_gatekeeping, theater_ratio, 0, 0.55).
narrative_ontology:measurement(cultleg_tr_t10, cultural_legitimacy_gatekeeping, theater_ratio, 10, 0.62).
narrative_ontology:measurement(cultleg_tr_t20, cultural_legitimacy_gatekeeping, theater_ratio, 20, 0.68).

% Extraction over time
narrative_ontology:measurement(cultleg_be_t0, cultural_legitimacy_gatekeeping, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(cultleg_be_t10, cultural_legitimacy_gatekeeping, base_extractiveness, 10, 0.5).
narrative_ontology:measurement(cultleg_be_t20, cultural_legitimacy_gatekeeping, base_extractiveness, 20, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(cultural_legitimacy_gatekeeping, identity_coordination).
narrative_ontology:affects_constraint(cultural_legitimacy_gatekeeping, academic_publishing_gatekeeping).
narrative_ontology:affects_constraint(cultural_legitimacy_gatekeeping, museum_acquisition_standards).
narrative_ontology:affects_constraint(cultural_legitimacy_gatekeeping, literary_canon_formation).

% DUAL FORMULATION NOTE:
% Cultural legitimacy gatekeeping decomposes into domain-specific constraints (academic publishing, museum acquisition, literary canon formation) each with its own epsilon value and institutional specificity. This story represents the general institutional pattern; domain stories capture sector-specific extraction mechanisms and alternative legitimacy pathways. All are linked: changes in one domain (e.g., successful alternative academic publishing) affect others through institutional isomorphism.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(cultural_legitimacy_gatekeeping, institutional, 0.08).
constraint_indexing:directionality_override(cultural_legitimacy_gatekeeping, powerless, 0.92).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
