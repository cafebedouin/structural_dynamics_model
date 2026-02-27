% ============================================================================
% CONSTRAINT STORY: narrative_overfitting
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_narrative_overfitting, []).

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
 *   constraint_id: narrative_overfitting
 *   human_readable: The Procrustean Plot: Narrative Overfitting in Content Systems
 *   domain: social/technological
 *
 * SUMMARY:
 *   Narrative overfitting occurs when complex, multidimensional real-world
 *   phenomena are compressed into simple, engagement-optimized story
 *   structures. This constraint exhibits structural characteristics of a
 *   tangled rope: it has genuine coordination function (enabling distributed
 *   decision-makers to act despite information asymmetry), but this
 *   coordination service is asymmetrically extracted from by platforms,
 *   content entrepreneurs, and engagement algorithms that capture attention
 *   value and resource allocation based on narrative salience rather than
 *   empirical accuracy. The constraint systematically disadvantages
 *   populations whose experiences cannot be reduced to compelling narratives,
 *   while systematically advantaging those who can craft or sponsor such
 *   narratives. The theater ratio (0.68) reflects that much of the narrative
 *   infrastructure — fact-checking, editorial review, academic communication
 *   — performs legitimacy roles while experiencing degraded functional
 *   capacity due to engagement asymmetries. The constraint's extractiveness
 *   has increased over the measured interval (0.28 → 0.52) as algorithmic
 *   ranking systems have become the primary gatekeepers of attention, and as
 *   organizational incentives have increasingly aligned with engagement
 *   metrics rather than epistemic quality.
 *
 * KEY AGENTS:
 *   - Platform Engagement Systems: Primary beneficiary (institutional/arbitrage) — algorithmic ranking systems that prioritize engagement-optimized content. Capture attention value and resource allocation.
 *   - Narrative Entrepreneurs: Secondary beneficiary (powerful/arbitrage) — skilled communicators, media figures, advocacy organizations that can craft or sponsor compelling frames. Gain disproportionate influence through narrative salience.
 *   - Attention Intermediaries: Tertiary beneficiary (organized/arbitrage) — media outlets, influencers, publishing platforms that monetize engagement through narrative compression. Can exit traditional epistemic standards.
 *   - Marginalized Populations: Primary victim (powerless/trapped) — communities whose experiences are economically unprofitable to represent accurately or are structurally incompatible with engagement-optimized narrative forms. No exit from misrepresentation.
 *   - Distributed Decision-Makers: Secondary victim (moderate/constrained) — individual voters, investors, patients, parents making decisions based on compressed information. Benefit from narrative simplification but suffer systematic distortions.
 *   - Epistemic Commons: Tertiary victim (powerless/trapped) — collective knowledge base that accumulates false frames, dead-ends, and misallocated research effort. Cannot exit or organize.
 *   - Journalism and Knowledge Production: Mixed actor (organized/constrained) — simultaneously benefited by audience reach from narrative compression and victimized by pressure to distort findings for engagement.
 *   - Fact-Checking Infrastructure: Institutional actor (organized/constrained) — attempts corrective function but operates under engagement asymmetries that limit effectiveness.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(narrative_overfitting, 0.52).
domain_priors:suppression_score(narrative_overfitting, 0.58).
domain_priors:theater_ratio(narrative_overfitting, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(narrative_overfitting, extractiveness, 0.52).
narrative_ontology:constraint_metric(narrative_overfitting, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(narrative_overfitting, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(narrative_overfitting, tangled_rope).
narrative_ontology:human_readable(narrative_overfitting, "The Procrustean Plot: Narrative Overfitting in Content Systems").
narrative_ontology:topic_domain(narrative_overfitting, "social/technological").

domain_priors:requires_active_enforcement(narrative_overfitting).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(narrative_overfitting, platform_engagement_systems).
narrative_ontology:constraint_beneficiary(narrative_overfitting, narrative_entrepreneurs).
narrative_ontology:constraint_beneficiary(narrative_overfitting, attention_intermediaries).
narrative_ontology:constraint_victim(narrative_overfitting, epistemic_commons).
narrative_ontology:constraint_victim(narrative_overfitting, marginal_populations).
narrative_ontology:constraint_victim(narrative_overfitting, distributed_decision_makers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: MARGINALIZED POPULATIONS (SNARE) — Groups whose experiences cannot be compressed into engagement-optimized narratives are systematically excluded or distorted. No exit from the narrative filter; they bear the full cost of misrepresentation as policy and resource allocation follow distorted frames. Maximal extraction experienced by those whose contexts are unmeasurable or economically unprofitable to represent accurately.
constraint_indexing:constraint_classification(narrative_overfitting, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: DISTRIBUTED DECISION-MAKERS (TANGLED ROPE) — Individual actors (voters, investors, parents, patients) benefit from narrative summarization that enables action despite information asymmetry. But they also bear costs: distorted frames lead to suboptimal decisions. Constrained by lack of access to unfiltered data; partial agency through media literacy and source diversification. Mixed extraction — both coordination (simplified enough to act on) and asymmetric harm (systematically distorted toward engagement).
constraint_indexing:constraint_classification(narrative_overfitting, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: PLATFORM ENGAGEMENT SYSTEMS (ROPE) — Algorithmic ranking systems that prioritize engagement see narrative compression as pure coordination: they solve the problem of making complex information shareable and memorable. Net beneficiary — the constraint is their primary structural advantage. Arbitrage exit available (can always shift ranking function), so experienced extraction is minimal. Classification as rope reflects genuine coordination function at this layer.
constraint_indexing:constraint_classification(narrative_overfitting, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: JOURNALISM AND ACADEMIC INSTITUTIONS (TANGLED ROPE) — These communities both benefit from and are extracted from by narrative overfitting. They benefit from audience attention and resource allocation tied to compelling frames. They are victimized by reduction of nuance and pressure to compress complex findings into viral summaries. Constrained exits: institutional reputation demands audience reach, making pure rejection of narrative compression unfeasible. Active enforcement of narrative norms (editorial gatekeeping, impact metrics) perpetuates the constraint.
constraint_indexing:constraint_classification(narrative_overfitting, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: LEGACY MEDIA EDITORIAL GATEKEEPING (PITON) — Traditional editorial roles (news judgment, story selection, headline writing) were originally functional for managing scarcity — human editors filtered information for readability. In the age of algorithmic curation, these roles are largely theatrical: editors compete with algorithms but lack algorithmic optimization capacity. The editorial norm persists through institutional inertia; many outlets maintain narrative-compression practices despite knowing they reduce accuracy. Theater ratio elevated because the ritual of editorial curation continues despite atrophied function.
constraint_indexing:constraint_classification(narrative_overfitting, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: FACT-CHECKING INSTITUTIONS (PITON) — Fact-checkers and research communicators attempt to debunk narrative overfitting, but their corrective mechanism is itself constrained by the same engagement dynamics. Corrections are less engaging than original false claims. The infrastructure persists (corrections are published, fact-checking occurs) but with diminished function — the engagement asymmetry means corrections reach smaller audiences. Theater ratio high because the corrective apparatus maintains theatrical legitimacy while experiencing degraded effectiveness.
constraint_indexing:constraint_classification(narrative_overfitting, piton,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / COGNITIVE LIMITS VIEW (MOUNTAIN) — From a civilizational perspective, human cognition has intrinsic bandwidth constraints. Complex multimodal systems cannot be fully represented in low-bandwidth narrative form without compression. Some reduction is inherent to communication itself — you cannot transmit the full complexity of a geopolitical conflict through any fixed-length narrative. This perspective sees narrative overfitting as an immutable natural law (bounded rationality). However, the structural data reveals this as likely a false summit: the compression is not uniformly distributed — it systematically distorts toward engagement, not toward truth. This suggests institutional choice, not cognitive law.
constraint_indexing:constraint_classification(narrative_overfitting, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(narrative_overfitting_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(narrative_overfitting, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(narrative_overfitting, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(narrative_overfitting, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(narrative_overfitting, TR),
    TR >= 0.70.

:- end_tests(narrative_overfitting_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. Narrative overfitting creates asymmetric value capture. Platform systems extract attention and behavioral data from distributed decision-makers who consume compressed narratives. Narrative entrepreneurs extract influence and resources from their ability to craft salient frames. The extraction is not total (coordination function is real) but significant and systematic. The 0.52 value reflects that the coordination benefit (enabling action despite asymmetry) is real but increasingly marginal as platforms have optimized pure engagement over decision-making quality. Suppression (0.58): Moderate-high. Multiple barriers prevent exit or correction: algorithmic optimization creates path dependency; cognitive limitations make narrative compression necessary; organizational incentives align with engagement rather than accuracy; platforms profit from engagement regardless of veracity; and fact-checking corrections are structurally disadvantaged in attention competition. Alternatives exist (primary sources, expert consultation, diverse media diet) but require significant effort and are less accessible to time-constrained populations. Theater ratio (0.68): High and increasing. Fact-checking institutions, editorial roles, academic peer review, and accuracy-assessment infrastructure all perform legitimacy functions while experiencing degraded functional capacity. The constraint has evolved from a problem of information scarcity (editorial compression was necessary) to a problem of information abundance with misaligned incentives (compression persists despite reducing accuracy). The editorial and correctional rituals continue, but often in theatrical form — public indicators of epistemic care without proportional functional improvement.
 *
 * PERSPECTIVAL GAP:
 *   The gap between beneficiary and victim perspectives is the constraint's diagnostic signature. Platform systems see a coordination problem solved (Rope: efficient allocation of attention). Narrative entrepreneurs see opportunity (Rope: amplification of certain voices). Distributed decision-makers see mixed benefit and harm (Tangled Rope: enabled action but distorted decision-making). Marginalized populations see systematic exclusion (Snare: no representation, no exit). Fact-checkers and editorial institutions see their own degradation (Piton: rituals persist but function attenuates). The analytical observer risks naturalizing this as inherent (Mountain: human cognition has bandwidth limits). The perspectival gaps reveal that the constraint is not a law of nature but an institutional design choice — platforms could prioritize accuracy over engagement; editorial institutions could resist compression; fact-checking could be elevated in ranking. That they do not reflects incentive structures, not cognitive necessity.
 *
 * DIRECTIONALITY LOGIC:
 *   Each agent's directionality value (d) reflects their structural position in the narrative overfitting constraint. Platform systems as beneficiaries with arbitrage exit options (can modify ranking function) derive low d → negative χ (experienced as coordination, not extraction). Narrative entrepreneurs with high power and arbitrage options derive moderate-low d (they extract but can be displaced). Distributed decision-makers with moderate power and constrained exits (limited access to alternatives) derive higher d — they bear extraction costs despite also receiving coordination benefits. Marginalized populations with powerless status and trapped exits derive maximum d → maximum experienced χ (snare). Fact-checkers with organized power but constrained exits (must operate within attention economy) derive moderate d, offset by their victim status (correction asymmetry), producing moderate-high d. The directionality derivation reveals why beneficiaries (platform systems) classify the constraint as rope while victims classify it as snare — the same institutional structure produces opposite experienced extraction depending on position.
 *
 * MANDATROPHY ANALYSIS:
 *   Narrative overfitting resolves the mandatrophy by demonstrating that the constraint is a genuine tangled rope, not mislabeled pure extraction. The coordination function is real: distributed decision-makers genuinely benefit from narrative simplification that enables action despite information asymmetry. The extraction is also real: platforms systematically advantage engagement-optimized frames over accurate ones, marginalized populations systematically experience exclusion, and fact-checking institutions operate at structural disadvantage. The constraint cannot be classified as pure snare because the coordination benefits are substantial — distributed decision-makers would face paralysis without narrative frames. The constraint cannot be classified as pure rope because the extraction asymmetries are systematic and unmeasured: platforms profit from engagement regardless of veracity; certain populations are systematically excluded; and corrections are structurally disadvantaged. The constraint exhibits both functions simultaneously, with active enforcement (algorithmic ranking, editorial norm-setting) maintaining the structure. This is the canonical tangled rope signature: genuine coordination plus asymmetric extraction, both sustained by institutional design.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    engagement_distortion_causality,
    'Does algorithmic ranking CAUSE narrative compression, or does it merely amplify naturally-occurring biases in human storytelling?',
    'Comparative analysis of narrative patterns before/after algorithmic curation systems. Study of non-algorithmic platforms (academic journals, books) vs algorithmic platforms (social media, search) for narrative compression metrics.',
    'If algorithmic causality: constraint is primarily a technology governance problem (remediable). If amplification: constraint reflects deeper cognitive/social features (more intractable). Classification could shift from tangled_rope to mountain if algorithmic agency is minimal.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(engagement_distortion_causality, empirical, 'Whether algorithms cause or amplify narrative compression').

omega_variable(
    marginalization_vs_legitimate_simplification,
    'What threshold distinguishes legitimate simplification (enabling non-expert action) from marginalizing simplification (erasing relevant epistemic dimensions)?',
    'Outcome analysis: do simplified narratives correlate with policy errors for populations at the margins? Do communities excluded from narratives experience measurable harm relative to those represented?',
    'If margin is clear: victims are structurally identifiable (sharpens snare classification). If margin is blurred: extraction is harder to distinguish from coordination, shifting some perspectives toward rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(marginalization_vs_legitimate_simplification, empirical, 'Threshold between necessary simplification and harmful marginalization').

omega_variable(
    correction_effectiveness_asymmetry,
    'Is the correction-engagement asymmetry (false claims more engaging than corrections) an inherent feature of cognition, or a remediable design choice in content systems?',
    'Experimental manipulation of platform affordances: test whether correction engagement increases when platform design prioritizes accuracy signals over engagement metrics. Comparison of platforms with different ranking functions.',
    'If inherent: piton classification for fact-checking is accurate (the infrastructure persists with degraded function). If remediable: piton classification shifts toward nascent scaffold (corrections are being made functional by deliberate design change).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(correction_effectiveness_asymmetry, empirical, 'Whether correction-engagement gap is cognitive or designable').

omega_variable(
    distributed_decision_maker_agency,
    'What proportion of distributed decision-makers (voters, investors, patients) have sufficient media literacy and access to non-narrative-compressed sources to exit the constraint?',
    'Survey of information behaviors: access to and utilization of primary sources, alternative platforms, and expert consultation. Tracking of outcome divergence based on information source diversity.',
    'If proportion is high: many agents should be classified as mobile rather than constrained (reduces snare perspectives, increases rope and scaffold perspectives). If low: constrained classification is accurate (extraction is experienced broadly).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(distributed_decision_maker_agency, empirical, 'Accessibility of non-compressed information for ordinary decision-makers').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(narrative_overfitting, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(novf_tr_t0, narrative_overfitting, theater_ratio, 0, 0.35).
narrative_ontology:measurement(novf_tr_t5, narrative_overfitting, theater_ratio, 5, 0.52).
narrative_ontology:measurement(novf_tr_t10, narrative_overfitting, theater_ratio, 10, 0.68).

% Extraction over time
narrative_ontology:measurement(novf_be_t0, narrative_overfitting, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(novf_be_t5, narrative_overfitting, base_extractiveness, 5, 0.4).
narrative_ontology:measurement(novf_be_t10, narrative_overfitting, base_extractiveness, 10, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(narrative_overfitting, information_standard).
narrative_ontology:affects_constraint(narrative_overfitting, algorithmic_ranking_bias).
narrative_ontology:affects_constraint(narrative_overfitting, epistemic_commons_degradation).
narrative_ontology:affects_constraint(narrative_overfitting, attention_economy_extraction).

% DUAL FORMULATION NOTE:
% Narrative overfitting is a constraint family spanning multiple institutional contexts. The core constraint (information compression for engagement) is downstream of algorithmic ranking systems and upstream of epistemic commons degradation. Each constraint in the family has distinct extractiveness: the compression mechanism itself (ε ≈ 0.52, tangled rope) versus specific ranking algorithms (ε varies by platform), versus cumulative epistemic damage (ε ≈ 0.65, snare). This story focuses on the compression mechanism. Decomposition ensures each structural level has its own ε and classification.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(narrative_overfitting, institutional, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
