% ============================================================================
% CONSTRAINT STORY: sturgeons_law
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sturgeons_law, []).

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
 *   constraint_id: sturgeons_law
 *   human_readable: Sturgeon's Law (90% of everything is crap)
 *   domain: sociological/artistic
 *
 * SUMMARY:
 *   Sturgeon's Law — the claim that '90% of everything is crap' — functions
 *   as a coordination mechanism for quality judgments and a suppression
 *   mechanism against amateur creation. From the perspective of powerless
 *   creators and niche communities, the law serves as justification for
 *   institutional gatekeeping that restricts distribution channels and
 *   visibility. From the perspective of curators and critics, it validates
 *   their expertise and authority. The constraint's theater ratio (0.65)
 *   reflects that the gatekeeping institutions have increasingly become
 *   performative: critical reviews rarely determine commercial success, and
 *   algorithmic platforms have begun displacing human curatorial judgment.
 *   The constraint exhibits all six types from different observation points,
 *   making it a diagnostic exemplar for how allegedly universal statistical
 *   laws can actually encode institutional power asymmetries.
 *
 * KEY AGENTS:
 *   - Amateur Creators: Primary victim (powerless/trapped) — bear suppression cost of discouragement and platform exclusion; no exit from 90% categorization
 *   - Quality Gatekeepers: Primary beneficiary (institutional/arbitrage) — capture authority to define 'the 10%'; benefit from scarcity of attention they manage
 *   - Critical Establishment: Secondary beneficiary (institutional/arbitrage) — maintain cultural authority through the law; now degraded (piton perspective)
 *   - Niche Communities: Secondary victim (moderate/constrained) — benefit from the filter but also constrained by gatekeeper definitions of 'quality'
 *   - Algorithmic Platforms: Organized agent (organized/constrained) — building alternatives to human curation; represent sunset mechanism for traditional gatekeeping
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent gatekeeping effects as statistical law
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sturgeons_law, 0.32).
domain_priors:suppression_score(sturgeons_law, 0.48).
domain_priors:theater_ratio(sturgeons_law, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sturgeons_law, extractiveness, 0.32).
narrative_ontology:constraint_metric(sturgeons_law, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(sturgeons_law, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sturgeons_law, tangled_rope).
narrative_ontology:human_readable(sturgeons_law, "Sturgeon's Law (90% of everything is crap)").
narrative_ontology:topic_domain(sturgeons_law, "sociological/artistic").

domain_priors:requires_active_enforcement(sturgeons_law).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sturgeons_law, quality_gatekeepers).
narrative_ontology:constraint_beneficiary(sturgeons_law, critical_establishment).
narrative_ontology:constraint_victim(sturgeons_law, amateur_creators).
narrative_ontology:constraint_victim(sturgeons_law, niche_audiences).
narrative_ontology:constraint_victim(sturgeons_law, emerging_talent).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: AMATEUR CREATOR (SNARE) — Powerless creator with no exit from the 90% categorization. The law functions as a suppression mechanism: discourages unpaid creative effort, justifies gatekeeping of publication platforms, and creates psychological barriers to sharing work. No alternative distribution channels exist with comparable reach to traditional media. Maximum experienced extraction — the creator internalizes the law as natural law and self-censors.
constraint_indexing:constraint_classification(sturgeons_law, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: NICHE COMMUNITY (TANGLED ROPE) — Communities organized around specific interests (fanfiction, indie music, indie games) benefit from coordination provided by the 90% filter — it validates their focused consumption and identity. But they also bear costs: gatekeepers determine what counts as 'the 10%'; access requires navigating institutional approval. Partial exit via fan platforms (AO3, Bandcamp, itch.io) but these depend on the same cultural logic that the 10% deserves attention.
constraint_indexing:constraint_classification(sturgeons_law, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: QUALITY GATEKEEPER (ROPE) — Publishers, editors, curators, critics experience Sturgeon's Law as a coordination mechanism: it justifies their existence and expertise. The law solves a real problem — information overload — by providing a shared standard for 'what matters.' Gatekeepers benefit from the authority to distinguish 10% from 90%. Full exit available (can stop curating) with high opportunity cost. Net beneficiary.
constraint_indexing:constraint_classification(sturgeons_law, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: ALGORITHMIC PLATFORM (SCAFFOLD) — Platforms (Spotify, Netflix, YouTube, TikTok) are building algorithmic alternatives to human curation that bypass the 90% suppression. Recommendation algorithms claim to surface quality without gatekeepers' institutional bias. This represents a sunset clause: as algorithmic discovery matures, the gatekeeper monopoly on 'the 10%' weakens. But platforms have their own extraction logic (engagement metrics, data capture), so the constraint morphs rather than disappears. Moderate suppression because platforms have agency and exit pathways.
constraint_indexing:constraint_classification(sturgeons_law, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: CRITICAL ESTABLISHMENT (PITON) — Literary criticism, music reviews, film criticism maintained the 90% framing for decades, but their institutional function has atrophied as amateur/algorithmic alternatives reduce their cultural authority. The critical apparatus persists through inertia — awards, canon, prestige institutions — but its actual verification function has degraded. Theater ratio high: much critical activity is now performative positioning rather than gatekeeping with functional consequence.
constraint_indexing:constraint_classification(sturgeons_law, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (MOUNTAIN) — From a civilizational view, Sturgeon's Law might reflect a statistical inevitability: any distribution of quality across producers will have a long tail. The law appears as mathematical necessity. However, the structural data reveals this as false naturalization: the 90/10 ratio is contingent on institutional gatekeeping that concentrates visibility. Without gatekeepers, the distribution becomes observable rather than fixed.
constraint_indexing:constraint_classification(sturgeons_law, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sturgeons_law_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(sturgeons_law, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(sturgeons_law, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(sturgeons_law, TR),
    TR >= 0.70.

:- end_tests(sturgeons_law_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.32): Moderate. The constraint extracts from creators through suppression of distribution channels and psychological discouragement, but the extraction is not maximal because alternative distribution has become available (internet platforms, self-publishing). The traditional gatekeeping extraction was higher when physical scarcity (printing costs, airtime) made gatekeeper approval necessary for any visibility. Suppression (0.48): Moderate-high. Significant barriers include platform gatekeeping (requires submission, editorial approval), visibility barriers (algorithms favor gatekept content), and psychological internalization (creators self-censor assuming the law is true). But not total suppression — self-publishing, fan platforms, and direct-to-audience channels exist. Theater ratio (0.65): Moderate-high. Gatekeeping institutions increasingly perform their quality-judgment function without functional consequence. Critics assess merit; audiences ignore reviews and follow algorithmic recommendations. Awards ceremonies celebrate 'the 10%' but do not determine which 10% survives culturally. The theater has increased over the interval as institutional critics lost authority.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates how the same statistical claim can encode radically different power relationships depending on perspective. For amateur creators, the law legitimizes their exclusion from visibility (snare). For niche communities, it validates their selective consumption and identity (tangled rope). For gatekeepers, it justifies their existence and authority (rope). For algorithmic platforms, it represents an old gatekeeping model being displaced by algorithmic curation (scaffold). For critics, it represents degraded institutional authority maintained through inertia (piton). For the civilizational observer, it risks appearing as a universal statistical law (false mountain) when the actual mechanism is institutional gatekeeping of visibility and distribution.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality is determined by the agent's structural position relative to the extraction flow. Powerless creators have no exit (d ≈ 0.95) and bear full suppression cost. Institutional gatekeepers have high exit options (arbitrage) and benefit from the law (d ≈ 0.05). Moderate niche communities have partial exit through fan platforms but depend on the same gatekeeping logic (d ≈ 0.55). Organized platforms have agency and exit paths via algorithmic alternatives (d ≈ 0.40-0.45). The piton perspective derives from the high theater ratio rather than from suppression or extraction intensity. The mountain perspective at the analytical/civilizational level is a false summit: the law naturalizes what is actually a contingent arrangement of institutional visibility control.
 *
 * MANDATROPHY ANALYSIS:
 *   DIAGNOSTIC EXEMPLAR: Sturgeon's Law resolves mandatrophy by demonstrating that the constraint is fundamentally about visibility gatekeeping masquerading as universal quality distribution. The law claims to describe a statistical property of all created work ('90% is crap'); the structural analysis reveals it describes only a statistical property of visible, gatekept work. The mandatrophy is resolved by recognizing that perspectives from ungated platforms (algorithmic recommendation, self-publishing, fan communities) produce different classifications because they operate under different visibility constraints. The scaffold perspective confirms the sunset mechanism: as algorithmic discovery matures and direct-to-audience platforms reduce gatekeeping dependency, the extracted value from traditional gatekeeping declines. The piton perspective confirms institutional degradation: critical gatekeeping persists through inertia and awards infrastructure despite losing functional authority. The mountain perspective is exposed as false naturalization: the 90/10 ratio is contingent on gatekeeping, not universal to creative production.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    quality_metric_definition,
    'What constitutes ''quality'' for the purpose of Sturgeon''s Law — aesthetic merit, cultural impact, technical skill, or audience satisfaction?',
    'Cross-domain analysis of what different communities measure as quality; correlation between expert judgment and audience preference metrics',
    'If quality = expert aesthetic judgment: law justifies institutional gatekeeping (supports snare classification). If quality = audience satisfaction: law mischaracterizes the distribution (most people enjoy most things they consume, contradicting 90% waste premise).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(quality_metric_definition, conceptual, 'Definition and measurability of quality across domains').

omega_variable(
    visibility_bias_confound,
    'Does the 90% observation reflect the actual quality distribution of all created work, or the quality distribution of visible/discoverable work after gatekeeping has already filtered?',
    'Comparison of gatekept media distribution (bookstores, radio, theaters) vs ungated distribution (self-published archives, complete YouTube database, all fanfiction); empirical quality measurement across both populations',
    'If gatekeeping dominates the sample: 90% figure is circular (gatekeepers report that 90% of what wasn''t gatekept is low quality). True distribution might be flatter. Changes classification from snare/tangled_rope to rope/scaffold at most perspectives.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(visibility_bias_confound, empirical, 'Whether the 90% observation is an artifact of gatekeeping visibility').

omega_variable(
    algorithmic_discovery_effectiveness,
    'Do algorithmic recommendation systems actually surface quality more effectively than human gatekeeping, or do they merely optimize for engagement while mimicking the distribution?',
    'Longitudinal tracking of recommendation success vs gatekeeper success; measurement of algorithmic bias toward high-engagement low-quality content; comparison of algorithmic ''discovery'' recommendations with expert retrospective quality judgments',
    'If algorithms are effective: scaffold sunset is real, and suppression mechanisms will degrade over time. If algorithms replicate gatekeeping bias: constraint persists in new form (snare with new beneficiary). If algorithms prefer engagement over quality: new extraction mechanism emerges.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(algorithmic_discovery_effectiveness, empirical, 'Whether algorithmic discovery bypasses or replicates gatekeeper quality filtering').

omega_variable(
    community_quality_production_rate,
    'Do communities with low gatekeeping barriers (fanfiction, indie game development, open-source software) actually produce the same 90/10 quality distribution, or do they produce different distributions?',
    'Comparative quality metrics across gated (traditional publishing) and ungated (fan communities, open-source) populations; statistical analysis of quality distribution shape',
    'If distributions match: Sturgeon''s Law is mathematical inevitability (mountain true). If ungated communities have flatter or different distributions: law reflects institutional gatekeeping effects rather than universal property (constraint is rope/tangled_rope, not mountain).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(community_quality_production_rate, empirical, 'Whether 90/10 distribution is universal or gatekeeping-dependent').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sturgeons_law, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sturgeon_tr_t0, sturgeons_law, theater_ratio, 0, 0.42).
narrative_ontology:measurement(sturgeon_tr_t5, sturgeons_law, theater_ratio, 5, 0.55).
narrative_ontology:measurement(sturgeon_tr_t10, sturgeons_law, theater_ratio, 10, 0.65).

% Extraction over time
narrative_ontology:measurement(sturgeon_be_t0, sturgeons_law, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(sturgeon_be_t5, sturgeons_law, base_extractiveness, 5, 0.25).
narrative_ontology:measurement(sturgeon_be_t10, sturgeons_law, base_extractiveness, 10, 0.32).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sturgeons_law, information_standard).
narrative_ontology:affects_constraint(sturgeons_law, taste_formation_monopoly).
narrative_ontology:affects_constraint(sturgeons_law, cultural_legitimacy_gatekeeping).

% DUAL FORMULATION NOTE:
% Sturgeon's Law decomposes into two structurally distinct claims: (1) the statistical claim that quality distributions are right-skewed (high proportion of low-quality production), and (2) the institutional claim that gatekeepers have authority to define which work falls into the 10% worth consuming. The statistical claim has low extractiveness if true universally (rope); the institutional claim has high extractiveness if it controls visibility (tangled rope/snare). This story focuses on the institutional constraint. The statistical claim would be a separate story with different ε.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
