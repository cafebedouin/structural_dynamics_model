% ============================================================================
% CONSTRAINT STORY: lindy_effect
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_lindy_effect, []).

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
 *   constraint_id: lindy_effect
 *   human_readable: The Lindy Effect
 *   domain: social/intellectual
 *
 * SUMMARY:
 *   The Lindy Effect is a theorized phenomenon where the future life
 *   expectancy of non-perishable things (ideas, books, technologies) is
 *   proportional to their current age. This constraint operates at the
 *   intersection of intellectual legitimacy and institutional gatekeeping.
 *   The constraint exhibits a structural tension: the Lindy Effect can be
 *   understood as a coordination mechanism (established ideas enable faster
 *   progress) or as an extraction mechanism (established ideas suppress novel
 *   frameworks). From different structural positions, intellectual actors
 *   experience it as a coordination-extraction hybrid (Tangled Rope). Novel
 *   idea creators experience it as pure extraction (Snare) — they are trapped
 *   paying a legitimacy tax on age. Established frameworks experience it as
 *   pure coordination (Rope) — age compounds their legitimacy. Academic
 *   institutions see mixed benefit and cost (Tangled Rope) — they benefit
 *   from institutional inertia but pay through paradigm-shift lag. The
 *   analytical observer risks naturalizing the effect as a law of quality
 *   (Mountain), obscuring the contingent institutional arrangements that
 *   enforce it. Theater ratio (0.58) reflects that much of the gatekeeping
 *   rhetoric around 'peer review rigor' and 'quality filtering' is partially
 *   performative — it justifies age-bias through the language of epistemic
 *   standards without empirically validating that age is a reliable quality
 *   signal.
 *
 * KEY AGENTS:
 *   - Novel Idea Creators: Primary victim (powerless/trapped) — face higher verification barriers and credibility extraction purely on basis of idea recency
 *   - Established Canon: Primary beneficiary (institutional/arbitrage) — derives compounding legitimacy and reduced verification overhead from age bias
 *   - Academic Institutions: Secondary beneficiary/victim (organized/constrained) — benefit from institutional inertia but face costs of paradigm-shift resistance
 *   - Open Science Movement: Organized agents (organized/mobile) — arXiv, preprint servers, open data create alternative legitimacy pathways with lower age bias
 *   - Citation Network: Institutional enforcer (institutional/constrained) — mechanically amplifies age-bias through impact factors and h-index metrics; increasingly recognized as dysfunctional
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing contingent institutional gatekeeping as inherent property of knowledge quality
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(lindy_effect, 0.38).
domain_priors:suppression_score(lindy_effect, 0.42).
domain_priors:theater_ratio(lindy_effect, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(lindy_effect, extractiveness, 0.38).
narrative_ontology:constraint_metric(lindy_effect, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(lindy_effect, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(lindy_effect, tangled_rope).
narrative_ontology:human_readable(lindy_effect, "The Lindy Effect").
narrative_ontology:topic_domain(lindy_effect, "social/intellectual").

domain_priors:requires_active_enforcement(lindy_effect).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(lindy_effect, established_ideas).
narrative_ontology:constraint_beneficiary(lindy_effect, legacy_institutions).
narrative_ontology:constraint_beneficiary(lindy_effect, status_quo_defenders).
narrative_ontology:constraint_victim(lindy_effect, novel_ideas).
narrative_ontology:constraint_victim(lindy_effect, emerging_paradigms).
narrative_ontology:constraint_victim(lindy_effect, institutional_innovators).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: NOVEL IDEA CREATOR (SNARE) — Trapped by the structural bias against new ideas. Fresh intellectual work lacks the accumulated legitimacy of established ideas and must overcome higher verification barriers. The creator has no exit: to participate in intellectual discourse, they must accept that their ideas face extraction of credibility toward established frameworks. No alternative institution provides equivalent reach or prestige.
constraint_indexing:constraint_classification(lindy_effect, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: ACADEMIC INSTITUTION (TANGLED ROPE) — Derives benefit from Lindy enforcement (established theories require less verification, reducing institutional overhead) while bearing costs of slowness in paradigm shifts. Constrained exit: institutions can adopt new paradigms but face reputational risk and must retrain existing researchers. Requires active enforcement of peer review gates that favor established ideas. Mixed benefit and extraction.
constraint_indexing:constraint_classification(lindy_effect, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: ESTABLISHED CANON (ROPE) — Classical works, foundational theories, and canonical texts benefit from Lindy enforcement. Their survival increases with age, creating coordination function: agreement on 'what we know' enables faster intellectual progress on novel problems. Institutional arbitrage: the canon's legitimacy compounds over time, reducing verification costs. Net beneficiary through pure coordination logic.
constraint_indexing:constraint_classification(lindy_effect, rope,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: OPEN SCIENCE MOVEMENT (SCAFFOLD) — Organized agents (open-access publishing, preprint servers, replication initiatives) are creating alternative verification pathways that bypass age-bias entirely. Preprints enable immediate scrutiny regardless of idea age. Open data allows replication without institutional gatekeeping. Low extraction because participants have mobile exit options and can choose alternative legitimacy pathways. Sunset logic: as open science matures, the age-bias extraction mechanism loses force.
constraint_indexing:constraint_classification(lindy_effect, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: CITATION NETWORK (PITON) — The citation mechanism is largely performative in enforcing the Lindy Effect. High citation counts reflect both genuine significance and age-based accumulation bias. The network maintains impact factor ranking and h-index metrics that mechanically favor older work, but these metrics are increasingly recognized as dysfunctional (Goodhart drift). Theater ratio high: the citation apparatus persists through inertia and institutional dependence despite acknowledged pathologies.
constraint_indexing:constraint_classification(lindy_effect, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a statistical/evolutionary perspective, the Lindy Effect could be viewed as a natural law: ideas that survive longer may genuinely have higher intrinsic quality (filtering hypothesis), making age a valid signal. However, this perspective risks naturalizing what is actually institutional bias. The constraint's structural data (requires_active_enforcement, beneficiaries, victims) reveals that age-bias is contingent, not inevitable.
constraint_indexing:constraint_classification(lindy_effect, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(lindy_effect_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(lindy_effect, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(lindy_effect, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(lindy_effect, TR),
    TR >= 0.70.

:- end_tests(lindy_effect_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The Lindy Effect extracts from novel ideas (requiring higher verification burden) and transfers credibility to established frameworks. However, the extraction is not total because: (1) genuinely novel ideas do eventually gain acceptance, and (2) open-science alternatives are emerging. The extractiveness value reflects that age-bias is a real structural property but not irreversible. Suppression (0.42): Moderate. Novel ideas face genuine barriers — higher citation requirements, more intensive peer review, career risk for researchers challenging established consensus. But suppression is not total: examples of rapid paradigm shifts exist (quantum mechanics, plate tectonics initially faced suppression but eventually prevailed). The value reflects that alternatives exist but require sustained effort. Theater ratio (0.58): Moderate-high. Much of the gatekeeping rhetoric around 'quality filtering' and 'peer review rigor' is partially performative — it justifies the existing age-bias system through appeals to epistemic standards without empirically validating that age is a reliable quality proxy. The ratio has increased over 50 years as the tension between open-science alternatives and traditional gatekeeping has become visible (Goodhart drift: when impact factors became a measure of quality, they became a target for gaming and institutional manipulation).
 *
 * PERSPECTIVAL GAP:
 *   This constraint produces maximum perspectival divergence. The novel idea creator (powerless/trapped) sees pure extraction (Snare) — they bear all costs of verification and age-bias. The established canon (institutional/arbitrage) sees pure coordination (Rope) — age legitimacy compounds without cost. The academic institution sees mixed effects (Tangled Rope) — institutional inertia is efficient short-term but costly long-term as paradigm shifts are delayed. The open science coalition sees a temporal boundary effect (Scaffold) — distributed verification pathways are reducing age-bias extraction by creating alternative legitimacy channels. The citation network sees its own dysfunction (Piton) — impact factors and h-index metrics mechanically amplify age-bias, and the network knows this creates false signals (Thompson & Ellison 2023 research showing h-index pathologies) but continues through inertia. The civilizational observer risks mountain classification (naturalizing age as quality signal), but the structural data reveals this as a false summit: the institutional arrangements are contingent, not inevitable.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) in this constraint is determined by structural position relative to age-bias: beneficiaries of established frameworks get low d (benefit from constraint, experience negative/low extraction), victims of novel idea suppression get high d (bear costs, experience high extraction). The original research group's directionality maps to institutional arbitrage exit — they benefit from being part of an established tradition. Novel researchers' directionality maps to trapped exit — they must pay the age-tax to participate at all. Academic institutions occupy the middle: they benefit from inertia but face costs from innovation lag. Open science advocates have mobile exit (can publish preprints and bypass traditional gatekeeping), so they experience lower effective extraction. The sigmoid f(d) function amplifies the directionality gap: trapped victims experience high f(d), producing high χ even with moderate ε; arbitrage beneficiaries experience negative f(d), producing negative χ even with identical ε. This explains why the same structural constraint produces such divergent classifications.
 *
 * MANDATROPHY ANALYSIS:
 *   The Lindy Effect resolves mandatrophy through explicit perspectival pluralism. The constraint is NOT 'is this coordination or extraction?' but 'for whom is it coordination vs extraction?' Novel idea creators experience extraction; established frameworks experience coordination; institutions experience hybrid effects; organized alternatives experience scaffolding. The mandatrophy resolution is structural: the constraint simultaneously IS coordination (for legitimacy accumulation across time) AND extraction (from novel ideas bearing legitimacy tax). The six perspectives show this is not ambiguity but genuine multi-position analysis. The mountain perspective is a false summit — the analytical observer risks naturalizing an institutional arrangement (age-based filtering) as a natural law (quality indicator). The constraint's measurable theater ratio (0.58) and increasing over time indicates Goodhart drift: when gatekeeping rhetoric emphasizes 'quality filtering,' it becomes a target for institutional manipulation, and the original quality-signal property degrades. The temporal measurements show theater_ratio rising from 0.35 to 0.58 as open-science alternatives have made the age-bias mechanism more visible and more defensive.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    filtering_vs_bias_distinction,
    'Does the Lindy Effect reflect genuine filtering of high-quality ideas by time, or primarily institutional bias favoring established frameworks?',
    'Comparative analysis of pre-Lindy vs post-Lindy adoption timelines for later-validated ideas; examine false positives (long-standing but ultimately wrong ideas) versus false negatives (valid ideas suppressed by age bias)',
    'If primarily filtering: Lindy is Mountain (natural law of quality). If primarily bias: Lindy is Snare (extraction mechanism). If mixed: Tangled Rope (coordination with asymmetric extraction) — the current classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(filtering_vs_bias_distinction, empirical, 'Whether Lindy Effect reflects quality filtering or institutional bias').

omega_variable(
    paradigm_shift_distribution,
    'What proportion of major scientific paradigm shifts violated the Lindy Effect — i.e., overthrew established ideas before their ''expected lifespan''?',
    'Historical catalog of paradigm shifts (Copernican, Darwinian, quantum, relativistic, germ theory, plate tectonics) and timeline from original claim to institutional acceptance; calculate median adoption lag and compare to Lindy prediction',
    'If >50% of shifts violated Lindy: effect is weak and institutional suppression is real (Snare). If <25% violated: effect is strong and may reflect genuine quality signal (Mountain). Mid-range suggests genuine coordination mechanism with extractive overlay (Tangled Rope).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(paradigm_shift_distribution, empirical, 'Proportion of paradigm shifts that violated Lindy Effect timeline').

omega_variable(
    open_science_impact,
    'Do preprint servers and distributed review mechanisms actually reduce the age-bias gate, or do they replicate it in new institutional forms?',
    'Longitudinal study of idea adoption timelines before/after arXiv, bioRxiv, open data platforms; measure citation acceleration for preprints vs journal-first publications of equivalent age; examine whether preprint platforms develop their own institutional gatekeeping (centrality bias, platform affiliation bias)',
    'If open science genuinely reduces bias: Scaffold sunset is real, extraction mechanism is degrading. If bias replicates: open science is performative, Piton classification confirmed, no real exit option for novel ideas.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(open_science_impact, empirical, 'Whether open science mechanisms reduce age-bias extraction').

omega_variable(
    extractive_overhead_quantification,
    'How much institutional overhead (reviewer time, institutional gatekeeping cycles, career risk for adoption) is required to enforce Lindy bias versus what would be needed without age-bias filtering?',
    'Comparative case studies of idea adoption with and without institutional gatekeeping (e.g., industry adoption vs academic adoption of same innovation); measure cost of enforcing consensus vs cost of parallel exploration of competing frameworks',
    'If enforcement overhead is low: suppression value should decrease. If high: suppression value of 0.42 is underestimated, moving toward Snare classification (suppression ≥ 0.60). Affects classification boundary.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(extractive_overhead_quantification, empirical, 'Enforcement overhead required to maintain Lindy bias mechanism').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(lindy_effect, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(lindy_tr_t0, lindy_effect, theater_ratio, 0, 0.35).
narrative_ontology:measurement(lindy_tr_t25, lindy_effect, theater_ratio, 25, 0.5).
narrative_ontology:measurement(lindy_tr_t50, lindy_effect, theater_ratio, 50, 0.58).

% Extraction over time
narrative_ontology:measurement(lindy_be_t0, lindy_effect, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(lindy_be_t25, lindy_effect, base_extractiveness, 25, 0.33).
narrative_ontology:measurement(lindy_be_t50, lindy_effect, base_extractiveness, 50, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(lindy_effect, information_standard).
narrative_ontology:affects_constraint(lindy_effect, peer_review_gatekeeping).
narrative_ontology:affects_constraint(lindy_effect, citation_market_efficiency).
narrative_ontology:affects_constraint(lindy_effect, paradigm_shift_suppression).

% DUAL FORMULATION NOTE:
% The Lindy Effect decomposes into three related but structurally distinct constraints: (1) Citation accumulation feedback (ε ≈ 0.15, Rope) — established ideas get more citations, enabling faster future citations; (2) Gatekeeping enforcement (ε ≈ 0.38, Tangled Rope, current story) — institutional actors enforce age-based filtering; (3) Paradigm suppression (ε ≈ 0.55, Snare) — young paradigms face irreducible resistance and must overcome collective inertia. Each has different base extractiveness values and different structural beneficiaries/victims.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(lindy_effect, organized, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
