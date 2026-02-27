% ============================================================================
% CONSTRAINT STORY: temporal_scarcity
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_temporal_scarcity, []).

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
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: temporal_scarcity
 *   human_readable: The "Scoop Economy" in Digital Media
 *   domain: technological/social
 *
 * SUMMARY:
 *   The 'scoop economy' in digital media creates a structural tension between
 *   the speed of information distribution and the time required for
 *   verification. When publishing algorithms optimize for engagement velocity
 *   and reward first-publication advantage, they create an asymmetry: speed
 *   gains significant immediate benefit (audience capture, narrative framing,
 *   advertising revenue) while verification incurs costs (slower publication,
 *   reduced algorithmic ranking, lost scoop advantage). This constraint
 *   demonstrates how institutional and algorithmic choices amplify what might
 *   appear to be an information-theoretic necessity (you cannot verify as
 *   fast as information spreads) into an extractive system that punishes
 *   verification and rewards speed-first publishing. The constraint exhibits
 *   tangled rope dynamics: real coordination benefit exists (timely
 *   information distribution) alongside systematic extraction (first-movers
 *   capture disproportionate value; readers receive lower-quality
 *   information; verification culture degrades). Theater ratio has increased
 *   from 0.35 to 0.68 over the interval as editorial standards (corrections,
 *   retractions, verification practices) have become increasingly
 *   performative — publicly maintained for legitimacy while undermined by
 *   speed-driven incentives. The general reader and subjects of news events
 *   are trapped with no exit; verification journalists face constrained
 *   choice; algorithmic systems benefit from the speed asymmetry while
 *   providing some genuine coordination value; fact-check coalitions
 *   represent a sunset pathway (verification infrastructure maturation);
 *   professional editorial standards persist as a piton (degraded but
 *   institutionally maintained); the analytical observer risks naturalizing
 *   speed-verification tradeoff as an immutable law of information physics.
 *
 * KEY AGENTS:
 *   - General Reader: Primary victim (powerless/trapped) — bears cost of misinformation and incomplete context; cannot exit algorithmic feeds without losing access to timely information
 *   - Subject of News Event: Primary victim (powerless/trapped) — caught in spreading narratives before response or context is possible; no exit from reputational extraction
 *   - First-Mover News Organization: Primary beneficiary (institutional/arbitrage) — captures traffic, narrative framing, and engagement advantage through speed; solves coordination problem by delivering timely information
 *   - Verification Journalist: Secondary victim (moderate/constrained) — constrained by competitive pressure; loses audience and rankings to speed-first competitors despite higher quality
 *   - Algorithmic Feed Operator: Structural beneficiary (powerful/arbitrage) — speed-optimized feeds maximize engagement and ad revenue; maintains extraction through ranking algorithms
 *   - Media Literacy and Fact-Check Coalition: Organized agents (organized/constrained) — building alternative verification pathways (community notes, fact-checking partnerships, prebunking); represents sunset mechanism
 *   - Professional Editorial Standards: Institutional actor (institutional/arbitrage) — maintains verification norms publicly but subordinated to speed incentives; piton classification
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent institutional choices as immutable information laws
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(temporal_scarcity, 0.52).
domain_priors:suppression_score(temporal_scarcity, 0.65).
domain_priors:theater_ratio(temporal_scarcity, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(temporal_scarcity, extractiveness, 0.52).
narrative_ontology:constraint_metric(temporal_scarcity, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(temporal_scarcity, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(temporal_scarcity, tangled_rope).
narrative_ontology:human_readable(temporal_scarcity, "The \"Scoop Economy\" in Digital Media").
narrative_ontology:topic_domain(temporal_scarcity, "technological/social").

domain_priors:requires_active_enforcement(temporal_scarcity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(temporal_scarcity, first_mover_publishers).
narrative_ontology:constraint_beneficiary(temporal_scarcity, algorithmic_amplification_systems).
narrative_ontology:constraint_victim(temporal_scarcity, reader_information_quality).
narrative_ontology:constraint_victim(temporal_scarcity, late_arrivals).
narrative_ontology:constraint_victim(temporal_scarcity, verification_culture).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: GENERAL READER (SNARE) — Trapped in algorithmic feeds optimized for engagement velocity. Cannot exit without abandoning access to breaking news. Bears cost of misinformation, incomplete context, and information cascades. d≈0.95, f(d)≈1.42, σ=1.2 → χ≈0.88.
constraint_indexing:constraint_classification(temporal_scarcity, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: SUBJECT OF NEWS EVENT (SNARE) — Individuals and organizations caught in breaking news cycles have no exit: stories about them spread before context or response is possible. Verification opportunity is forfeited for speed. d≈0.98, f(d)≈1.50, σ=1.2 → χ≈0.94.
constraint_indexing:constraint_classification(temporal_scarcity, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: VERIFICATION JOURNALIST (TANGLED ROPE) — Constrained by competitive pressure and resource limits. Benefits from scoop rewards (career advancement, audience) but also suffers systematic disadvantage: verification-first reporting loses algorithmic ranking to speed-first competitors. d≈0.72, f(d)≈1.12, σ=1.0 → χ≈0.59.
constraint_indexing:constraint_classification(temporal_scarcity, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: FIRST-MOVER NEWS ORGANIZATION (ROPE) — Primary beneficiary. Captures traffic, engagement, and narrative framing advantage through speed. Experiences scoop pressure as a coordination mechanism: publishing quickly establishes newsworthiness and prevents competitors from claiming the story. d≈0.08, f(d)≈-0.10, σ=1.2 → χ≈-0.06. Net beneficiary.
constraint_indexing:constraint_classification(temporal_scarcity, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: ALGORITHMIC FEED OPERATOR (TANGLED ROPE) — Structural beneficiary: speed-optimized feeds maximize engagement and ad impressions. Also provides coordination value (delivering timely information). But requires active suppression of verification signals to maintain speed priority. Benefits from extraction (engagement increases with uncertainty and novelty), but also depends on minimal-friction information flow. d≈0.15, f(d)≈-0.02, σ=1.2 → χ≈-0.01. Slight net beneficiary due to arbitrage exit and algorithmic control.
constraint_indexing:constraint_classification(temporal_scarcity, tangled_rope,
    context(agent_power(powerful),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: MEDIA LITERACY AND FACT-CHECK COALITION (SCAFFOLD) — Organized agents (fact-checkers, media literacy initiatives, platform interventions) see scoop pressure as a temporary coordination failure with a sunset. Infrastructure for distributed verification (community notes, fact-check partnerships, prebunking) is building alternative pathways that reduce extraction. d≈0.45, f(d)≈0.48, σ=1.2 → χ≈0.30. Low effective extraction because coalition has agency and sunset is structural (verification infrastructure adoption).
constraint_indexing:constraint_classification(temporal_scarcity, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: PROFESSIONAL EDITORIAL STANDARDS (PITON) — Traditional journalism norms (verify before publishing, distinguish reporting from commentary, correct errors) persist through institutional inertia but are increasingly performative. Editorial standards are publicly maintained (corrections sections, retraction policies) but subordinated to speed-driven incentives. theater_ratio=0.68 satisfies the piton gate (≥0.70, marginal). The institutional standards see themselves as degraded — maintained for legitimacy rather than function.
constraint_indexing:constraint_classification(temporal_scarcity, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER / INFORMATION CASCADE VIEW (MOUNTAIN) — From a civilizational perspective, some speed-verification tradeoff appears inherent to distributed information systems: you cannot verify globally distributed claims faster than they spread. This perspective sees scoop pressure as an immutable property of information dynamics. However, structural data (ε=0.52, suppression=0.65, theater=0.68) contradicts a pure mountain classification — the engine will compute this as a false summit, revealing that the speed-verification tradeoff is amplified by institutional and algorithmic choices, not purely by information physics.
constraint_indexing:constraint_classification(temporal_scarcity, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(temporal_scarcity_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(temporal_scarcity, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(temporal_scarcity, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(temporal_scarcity, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(temporal_scarcity, TR),
    TR >= 0.70.

:- end_tests(temporal_scarcity_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high, rising over interval. The initial state (0.25) reflects mixed coordination and extraction — early digital media still maintained separation between news and comment, with verification remaining valued. By time 5 (0.38), algorithmic ranking systems had begun prioritizing engagement velocity over accuracy. At time 10 (0.52), the scoop economy is fully mature: first-mover advantage is substantial, speed-verification tradeoff is explicit, and algorithmic amplification directly rewards speed. The rising trajectory reflects institutional drift toward extraction as engagement metrics became primary optimization targets. Suppression (0.65): High. Significant barriers to verification-first publishing include: algorithmic demotion of content below trending thresholds (technical suppression); career/revenue loss for verification-focused outlets (economic suppression); reader expectations for immediate coverage (social suppression); lack of infrastructure for post-publication verification at scale (systemic suppression). However, suppression is not total — some outlets maintain verification-first models (subscription-based, niche communities), and fact-checking infrastructure is emerging. Theater ratio (0.68): High and rising. Editorial standards (corrections, retractions, verification procedures) are publicly performed but operationally subordinated. News organizations maintain corrections sections and retraction policies (theater) while speed-driven publishing and algorithmic ranking undermine verification in practice (reality). This mismatch has increased as algorithmic distribution has bypassed editorial gatekeeping.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates significant perspectival divergence. The first-mover publisher sees a coordination mechanism (Rope) — speed solves the problem of delivering timely information. The general reader sees pure extraction (Snare) — they receive lower-quality information and have no exit. The verification journalist sees mixed dynamics (Tangled Rope) — they benefit from verification infrastructure but suffer competitive disadvantage. The algorithmic operator sees slight net benefit (Tangled Rope) — genuine coordination value (timely delivery) alongside extraction benefit (engagement optimization). The fact-check coalition sees a temporary problem with emerging solutions (Scaffold) — community verification infrastructure is building an exit path. Professional editorial standards see their own degradation (Piton) — verification norms persist through institutional memory rather than operational priority. The analytical observer risks seeing an immutable information law (Mountain) — but structural data reveals this is a false summit: the speed-verification asymmetry is amplified by institutional choices (algorithmic ranking, advertising monetization, scoop rewards) that are not information-theoretic necessities.
 *
 * DIRECTIONALITY LOGIC:
 *   General reader: Victim + trapped → d≈0.95, f(d)≈1.42. Maximum extraction — no exit from algorithmic feeds. Subject of news event: Victim + trapped → d≈0.98, f(d)≈1.50. Maximum extraction — reputational cost with no ability to respond in real time. First-mover publisher: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.10. Net beneficiary — captures scoop advantage and can exit by publishing faster. Verification journalist: Victim + constrained → d≈0.72, f(d)≈1.12. Significant extraction but not maximal — can exit by adopting speed-first strategy but loses identity/quality value. Algorithmic operator: Beneficiary + arbitrage with coordination function → d≈0.15, f(d)≈-0.02. Near-net-beneficiary — extraction value (engagement increase) offset by coordination value (timely distribution). Fact-check coalition: Organized + constrained → d≈0.45, f(d)≈0.48. Low effective extraction; coalition has agency and sees structural exit path. Editorial standards: Institutional + arbitrage → d≈0.08, f(d)≈-0.10. Piton classification comes from theater gate (0.68 ≥ 0.70), not from high chi. Analytical observer: analytical → d≈0.72, f(d)≈1.15. Mountain classification is perspectival (observer naturalizes institutional choice as law); false summit detection applies.
 *
 * MANDATROPHY ANALYSIS:
 *   The scoop economy resolves mandatrophy by showing the constraint is genuinely tangled_rope from multiple perspectives: it provides real coordination value (timely information distribution) while extracting asymmetric value (first-mover advantage, reader misinformation, verification culture degradation). The constraint is NOT purely extractive (pure snare) because algorithmic feed systems do solve the genuine problem of distributing large volumes of information efficiently. It is NOT purely coordination (pure rope) because the speed asymmetry creates structural winners and losers, and extraction mechanisms are built into ranking algorithms and scoop reward structures. The mandatrophy resolution shows why institutional reform is possible: the coordination function can be decoupled from the extraction mechanism. Alternative designs (verification-weighted ranking, community fact-checking infrastructure, slower-but-accurate publishing models) can maintain coordination benefit while reducing extraction. The scaffold perspective's sunset is structural and achievable: distributed fact-checking, community notes, and prebunking infrastructure are mature enough that post-publication verification can begin to match pre-publication editorial capacity. This is not a natural law — it is a contingent institutional arrangement amenable to reform.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    algorithmic_speed_necessity,
    'Is the speed-first algorithmic ranking structurally necessary for engagement, or a contingent design choice that could be modified without sacrificing platform utility?',
    'Comparative analysis of platform engagement with modified ranking (verification weight increase, time-delay penalty reduction, quality signal amplification); A/B testing verification-first ranking against speed-first on representative user cohorts',
    'If necessary: scoop pressure is partly immutable (higher mountain likelihood). If contingent: scoop pressure is institutional choice amenable to structural reform (snare/tangled_rope confirmed).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(algorithmic_speed_necessity, empirical, 'Whether speed-first ranking is necessary for algorithmic engagement').

omega_variable(
    verification_infrastructure_maturity,
    'Can distributed fact-check partnerships and community-driven verification (community notes, crowdsourced fact-checking) achieve error-detection rates comparable to pre-publication editorial review?',
    'Comparison of error detection rates and correction latency: pre-publication editorial review vs post-publication community fact-checking on identical news stories; longitudinal tracking of claim accuracy for speed-published vs verification-first reporting',
    'If comparable or better: scaffold sunset is real and structural (alternative verification pathways are functional). If inferior: scoop extraction persists because alternatives are not yet sufficient.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(verification_infrastructure_maturity, empirical, 'Whether distributed fact-checking matches pre-publication editorial review effectiveness').

omega_variable(
    reader_attention_scarcity_primacy,
    'Is reader attention scarcity the primary driver of speed-first incentives, or is it advertising economics and engagement-driven metrics that amplify scoop pressure beyond information-theoretic necessity?',
    'Comparative platform analysis: outlets funded by subscriptions vs ads; measurement of speed-verification tradeoff in markets with different monetization models; historical analysis of scoop pressure intensity before and after advertising-optimized algorithmic distribution',
    'If attention scarcity is primary: scoop pressure reflects fundamental information economics (mountain-adjacent). If advertising economics amplify beyond necessity: scoop pressure is extractive institutional choice (snare/tangled_rope confirmed).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reader_attention_scarcity_primacy, empirical, 'Primary driver of speed-first incentives: attention scarcity vs advertising economics').

omega_variable(
    counter_narrative_velocity,
    'Can verified counter-narratives and corrections spread as quickly through organic distribution and algorithmic amplification as initial false claims?',
    'Comparative analysis of narrative velocity: time to reach equivalent audience size for initial claim vs correction/counter-narrative; measurement of viral coefficient for retractions vs original false claims; longitudinal tracking of correction reach in algorithmic feeds',
    'If correction velocity matches claim velocity: verification pathways can keep pace (scaffold hypothesis supported). If corrections are structurally slower: speed asymmetry creates extraction (snare dynamic confirmed).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(counter_narrative_velocity, empirical, 'Whether corrections can spread as quickly as initial claims').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(temporal_scarcity, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(temporal_tr_t0, temporal_scarcity, theater_ratio, 0, 0.35).
narrative_ontology:measurement(temporal_tr_t5, temporal_scarcity, theater_ratio, 5, 0.52).
narrative_ontology:measurement(temporal_tr_t10, temporal_scarcity, theater_ratio, 10, 0.68).

% Extraction over time
narrative_ontology:measurement(temporal_be_t0, temporal_scarcity, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(temporal_be_t5, temporal_scarcity, base_extractiveness, 5, 0.38).
narrative_ontology:measurement(temporal_be_t10, temporal_scarcity, base_extractiveness, 10, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(temporal_scarcity, information_standard).
narrative_ontology:affects_constraint(temporal_scarcity, information_cascade_asymmetry).
narrative_ontology:affects_constraint(temporal_scarcity, engagement_metric_optimization).

% DUAL FORMULATION NOTE:
% The scoop economy is downstream of algorithmic ranking systems and engagement-driven monetization. The upstream constraints (algorithmic ranking design, advertising economics) have their own ε values reflecting the direct information-theoretic and economic choices. The scoop economy represents the emergent constraint at the journalist/reader level where those upstream choices have structural consequences.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
