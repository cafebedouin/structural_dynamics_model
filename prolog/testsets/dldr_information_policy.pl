% ============================================================================
% CONSTRAINT STORY: dldr_information_policy
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_dldr_information_policy, []).

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
 *   constraint_id: dldr_information_policy
 *   human_readable: Don't Like, Don't Read (DLDR) Information Policy
 *   domain: technological/social
 *
 * SUMMARY:
 *   The 'Don't Like, Don't Read' (DLDR) information policy represents a
 *   constraint that has evolved from explicit archive governance principle to
 *   implicit platform design pattern across fan communities, NSFW content
 *   spaces, and community-operated archives. DLDR shifts responsibility for
 *   content filtering from platform/creator to reader, justified as
 *   protecting reader autonomy and creator freedom. However, the constraint
 *   exhibits the structural signature of a Tangled Rope: it achieves genuine
 *   coordination (decentralized filtering reduces platform burden and aligns
 *   with anti-censorship values) while simultaneously extracting asymmetric
 *   costs (vulnerable readers bear exposure risk; creators bear tagging
 *   burden; platforms reduce liability without eliminating it). The
 *   theater_ratio has increased from 0.35 to 0.64 over the measurement
 *   interval, indicating that DLDR's performative content (community norms,
 *   platform rhetoric about 'reader choice') has outpaced its functional
 *   content (actual accessible filtering mechanisms). The constraint's
 *   suppression (0.68) reflects significant barriers to exit: readers cannot
 *   access archive content without accepting DLDR terms; vulnerable
 *   populations (trauma survivors, neurodivergent users) cannot pre-filter
 *   without community infrastructure; creators cannot opt into filtered
 *   distribution without leaving the archive.
 *
 * KEY AGENTS:
 *   - Vulnerable Readers: Primary victims (powerless/trapped) — individuals seeking archive content but unable to negotiate content filtering; bear full exposure risk
 *   - Archive Operators: Primary beneficiaries (institutional/arbitrage) — reduce operational cost and liability by delegating content filtering to readers and creators
 *   - Content Creators: Secondary beneficiary and secondary victim (powerful/arbitrage) — gain free distribution without editorial vetting but bear disciplinary responsibility for accurate tagging
 *   - Archive Community: Secondary actor (moderate/constrained) — community-operated archives benefit from cost reduction but share extraction burden with readers
 *   - Accessibility Advocacy Coalition: Organized agent (organized/constrained) — advocates for infrastructure improvements; sees DLDR as temporary friction with sunset logic
 *   - Publishing Industry Legacy: Institutional observer (institutional/arbitrage) — DLDR represents atrophied editorial responsibility maintained through inertia
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing DLDR as inherent to information asymmetry rather than contingent institutional choice
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dldr_information_policy, 0.52).
domain_priors:suppression_score(dldr_information_policy, 0.68).
domain_priors:theater_ratio(dldr_information_policy, 0.64).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dldr_information_policy, extractiveness, 0.52).
narrative_ontology:constraint_metric(dldr_information_policy, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(dldr_information_policy, theater_ratio, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dldr_information_policy, tangled_rope).
narrative_ontology:human_readable(dldr_information_policy, "Don't Like, Don't Read (DLDR) Information Policy").
narrative_ontology:topic_domain(dldr_information_policy, "technological/social").

domain_priors:requires_active_enforcement(dldr_information_policy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dldr_information_policy, archive_operators).
narrative_ontology:constraint_beneficiary(dldr_information_policy, content_creators).
narrative_ontology:constraint_victim(dldr_information_policy, vulnerable_readers).
narrative_ontology:constraint_victim(dldr_information_policy, cultural_commons).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: VULNERABLE READER (SNARE) — Individuals seeking archive content (fanworks, historical materials, community records) cannot negotiate content filtering without abandoning access entirely. No ability to pre-filter or customize warning systems. Trapped between exposure to harmful content and platform exit. Maximum suppression: warnings may be inadequate, obfuscated, or absent; content moderation relies entirely on voluntary labeling by creators. No institutional recourse.
constraint_indexing:constraint_classification(dldr_information_policy, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: ARCHIVE COMMUNITY (TANGLED ROPE) — Community-operated archives (AO3, Wattpad communities, fan forums) experience DLDR as both coordination and extraction. Coordination function: reader autonomy reduces moderation burden and aligns with anti-censorship values. Extraction function: platforms shift liability and labor to readers; communities cannot enforce consistent filtering; participation carries asymmetric information risk. Constrained exit: migrating archives is costly; alternatives are limited or proprietary.
constraint_indexing:constraint_classification(dldr_information_policy, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: ARCHIVE OPERATOR (ROPE) — Platform operators (Tumblr, AO3 moderation teams, Discord servers) experience DLDR as pure coordination: decentralized filtering reduces operational cost, distributes trust responsibility, and aligns with community values of reader agency. Net beneficiary through reduced liability and operational overhead. Arbitrage exit: operators can migrate to proprietary systems or different policy regimes without reputational cost to their primary function.
constraint_indexing:constraint_classification(dldr_information_policy, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: ACCESSIBILITY ADVOCACY COALITION (SCAFFOLD) — Organized advocates (disability communities, trauma-informed content advocates, digital rights organizations) see DLDR as a temporary friction point with a sunset: progressive refinement of tagging standards (AO3 Additional Tags, content rating systems), AI-assisted content classification, and reader browser plugins are building more accessible filtering mechanisms. Sunset logic: as annotation and filtering infrastructure mature, DLDR becomes obsolete. Theater is moderate (advocacy pressure creates performance of accessibility improvement).
constraint_indexing:constraint_classification(dldr_information_policy, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: PUBLISHING INDUSTRY LEGACY (PITON) — Traditional publishing gatekeeping and content review mechanisms have atrophied in the digital era. DLDR represents institutional inertia: the historical norm of editorial responsibility persists as a rhetorical ideal ('caveat emptor, reader beware') while actual filtering mechanisms have degraded to performative warning labels. Theater ratio high (0.64): labeling compliance is inconsistent and unenforced; the ritual of content warnings substitutes for actual accessibility architecture. Function has declined; inertia maintains the policy.
constraint_indexing:constraint_classification(dldr_information_policy, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: CONTENT CREATOR (TANGLED ROPE) — Creators experience DLDR as mixed coordination and extraction. Coordination: platform provides free distribution without editorial vetting, reducing friction to publication. Extraction: responsibility for tagging and warnings transfers to creators; failure to tag can result in community shaming, account suspension, or reputation damage. Arbitrage exit: creators can move between platforms, but switching costs are moderate. Effective extraction (chi) remains significant: creators bear disciplinary burden despite platform's operational reduction.
constraint_indexing:constraint_classification(dldr_information_policy, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a universal perspective, the information asymmetry between content creator and reader is structural: creators always know their content better than readers can know it without exposure. DLDR appears as a natural equilibrium of this asymmetry — readers cannot access information without risk, so filtering responsibility must rest with someone. However, this naturalizes a contingent choice: alternative regimes (mandatory tagging, platform classification, age-gated access) exist but require institutional investment. The engine's false summit detector should flag this as naturalization of institutional choice, not natural law.
constraint_indexing:constraint_classification(dldr_information_policy, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(dldr_information_policy_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(dldr_information_policy, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(dldr_information_policy, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(dldr_information_policy, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(dldr_information_policy, TR),
    TR >= 0.70.

:- end_tests(dldr_information_policy_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The constraint extracts meaningful costs from vulnerable readers (exposure risk) and creators (tagging burden) while providing asymmetric benefits to operators (cost reduction and liability management). The extractiveness is not as severe as pure data extraction or labor exploitation (0.70+) because alternatives exist and reader participation is nominally voluntary. The interval trajectory (0.32→0.52) reflects accumulation of extractive pressure: as archives have scaled, tagging becomes increasingly burdensome and inconsistent, shifting more filtering responsibility to readers. Suppression (0.68): High. Significant barriers to exit include: specialized archive content (fan communities, rare collections) concentrated in specific platforms; locked-in creator communities; switching costs for long-form content; social penalties for creating alternative filtering infrastructure. Vulnerable readers cannot condition their archive participation on improved filtering without abandoning the content entirely. Theater ratio (0.64): Moderately high and rising. Community norms and platform rhetoric emphasize 'reader choice' and 'creator autonomy' as justifications, but functional filtering mechanisms remain inconsistent. Content warnings are often incomplete, vague, or absent; platform compliance mechanisms are minimal; the ritual of tagging has replaced actual accessible design. Rising theater reflects growing awareness that DLDR's promised benefits (balanced responsibility, reader empowerment) are increasingly rhetorical.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates stark perspectival divergence. The archive operator sees pure coordination (Rope) — they are solving a legitimate problem of distributing filtering responsibility without centralized gatekeeping. The vulnerable reader sees pure extraction (Snare) — they bear exposure risk with minimal recourse. The content creator sees mixed coordination-extraction (Tangled Rope) — they gain distribution but are disciplined through tagging expectations. The accessibility coalition sees a temporary problem being solved (Scaffold) — better tagging infrastructure and filtering tools are emerging. The publishing industry sees its own degraded gatekeeping ritual (Piton) — editorial responsibility persists rhetorically but functions performatively. The analytical observer risks seeing natural law (Mountain) — information asymmetry always requires filtering delegation — but structural evidence reveals DLDR as institutional choice, not necessity. The perspectival gap reflects fundamental disagreement about who should bear filtering burden: operators prefer readers; readers prefer operators; creators prefer both; advocates prefer infrastructure solutions.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values (d) for each perspective derive from structural position and exit capacity. Vulnerable readers occupy d≈0.95 (maximum target status): trapped exit, victim status, no arbitrage options produce the highest effective extraction. Archive operators occupy d≈0.05 (maximum beneficiary status): arbitrage exit, beneficiary status, reduced operational burden produce negative/minimal extraction. Content creators occupy d≈0.50 (mixed position): powerful status and arbitrage exit partially offset victim status from tagging burden; the mixed directionality reflects their dual role as beneficiary (free distribution) and victim (disciplinary tagging expectations). Archive communities occupy d≈0.65 (moderate target status): constrained exit and mixed beneficiary/victim status. The analytical observer at d≈0.72 (observer position) reflects the perspectival distance required to see the constraint holistically. Operators with arbitrage exit + beneficiary status derive low d via sigmoid f(d)≈-0.12; readers with trapped exit + victim status derive high d via sigmoid f(d)≈1.42; the sigma scope modifier σ(global=1.2) amplifies effective extraction slightly due to global coordination difficulty.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by identifying the legitimate coordination function (decentralized filtering reduces platform burden and enables creator autonomy) while showing that the extraction asymmetry violates the boundaries of pure coordination. DLDR is Tangled Rope, not pure Rope, because: (1) Rope requires beneficiary/victim declaration; DLDR has clear victims (vulnerable readers) and beneficiaries (operators); (2) Rope requires low suppression (≤0.05) or genuine exit mobility; DLDR has high suppression (0.68) and constrained/trapped exits for most agents; (3) Rope requires low effective extraction (χ ≤0.35); DLDR achieves χ≈0.52-0.68 depending on perspective. The critical misclassification risk is treating DLDR as pure coordination (Rope) because its coordination function is real and valuable. The Tangled Rope classification preserves the coordination while flagging the extraction. The rising theater (0.35→0.64) indicates Goodhart drift: as filtering burden accumulates, DLDR's rhetoric about reader autonomy becomes increasingly performative, suggesting the constraint is degrading toward Piton status (inertial theater without function).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    tagging_compliance_measurement,
    'What percentage of user-generated content in DLDR archives carries accurate, actionable content warnings?',
    'Large-scale content audit: sample archives (AO3, Wattpad, FFnet) and compare creator-provided tags against independent content classification; measure agreement rates for common content categories (violence, sexual content, mental health triggers)',
    'If compliance > 80%: DLDR functions as effective coordination (Rope from creator perspective). If compliance < 50%: DLDR is mostly extractive theater (Snare from reader perspective). If compliance 50-80%: mixed model (Tangled Rope) is confirmed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(tagging_compliance_measurement, empirical, 'Accuracy and completeness of user-provided content warnings').

omega_variable(
    reader_harm_correlation,
    'Do readers encountering unwarned harmful content in DLDR archives experience measurable psychological or social harm compared to platform-filtered alternatives?',
    'Comparative study: survey readers on DLDR platforms vs algorithm-filtered platforms (YouTube, TikTok); measure trauma/distress rates, avoidance behavior, and self-reported vulnerability to unwanted exposures',
    'If harm rate significantly higher on DLDR platforms: supports Snare classification from vulnerable reader perspective. If harm rates comparable: suggests readers have developed coping strategies or DLDR warnings are functionally adequate. If harm rate lower: unexpected finding suggesting reader agency is genuinely protective.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reader_harm_correlation, empirical, 'Psychological harm correlation with DLDR versus filtered platforms').

omega_variable(
    platform_liability_materialization,
    'Does DLDR actually reduce platform legal liability for user-generated content, or does it merely shift visibility of liability without reducing legal exposure?',
    'Legal analysis: compare litigation outcomes and settlement patterns for DLDR platforms (AO3, fan archives) versus moderated platforms (Reddit, Facebook); examine whether ''readers assume risk'' doctrine holds in relevant jurisdictions',
    'If liability genuinely reduced: DLDR achieves its primary institutional function and benefits to operators are real (Rope). If liability merely concealed: DLDR is extractive theater masking legal exposure that operators have shifted to readers (Snare). If liability shared: mixed model (Tangled Rope).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(platform_liability_materialization, empirical, 'Whether DLDR actually reduces platform legal liability').

omega_variable(
    alternative_filtering_feasibility,
    'Are there technically and economically feasible alternatives to DLDR that would provide reader protection without reducing creator autonomy?',
    'Technology assessment: evaluate cost and feasibility of options — mandatory structured tagging schemas, crowdsourced rating systems, browser-side filtering plugins, AI content classification. Measure implementation cost vs operational burden reduction.',
    'If feasible alternatives exist and are low-cost: DLDR is revealed as institutional choice, not necessity. Strengthens Scaffold sunset logic and supports False Mountain (analytical) observation. If no feasible alternatives: DLDR may approach Mountain status (structural equilibrium). If alternatives exist but high-cost: supports Tangled Rope (mixed coordination-extraction where trade-off favors operators).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(alternative_filtering_feasibility, empirical, 'Technical and economic feasibility of alternative content filtering mechanisms').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dldr_information_policy, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dldr_tr_t0, dldr_information_policy, theater_ratio, 0, 0.35).
narrative_ontology:measurement(dldr_tr_t5, dldr_information_policy, theater_ratio, 5, 0.5).
narrative_ontology:measurement(dldr_tr_t10, dldr_information_policy, theater_ratio, 10, 0.64).

% Extraction over time
narrative_ontology:measurement(dldr_be_t0, dldr_information_policy, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(dldr_be_t5, dldr_information_policy, base_extractiveness, 5, 0.42).
narrative_ontology:measurement(dldr_be_t10, dldr_information_policy, base_extractiveness, 10, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dldr_information_policy, information_standard).
narrative_ontology:affects_constraint(dldr_information_policy, content_moderation_scalability).
narrative_ontology:affects_constraint(dldr_information_policy, reader_informed_consent).
narrative_ontology:affects_constraint(dldr_information_policy, creator_liability_asymmetry).

% DUAL FORMULATION NOTE:
% DLDR decomposes into three structurally related constraints: (1) Content Moderation Scalability (ε≈0.25, Rope) — the coordination problem of filtering large-scale user-generated content; (2) Reader Informed Consent (ε≈0.55, Tangled Rope) — whether readers can make meaningful choices about exposure; (3) Creator Liability Asymmetry (ε≈0.48, Tangled Rope) — whether creators bear disproportionate responsibility. DLDR is the policy response that conflates these three claims. The constraint stories should be decomposed for precision.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(dldr_information_policy, powerless, 0.92).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
