% ============================================================================
% CONSTRAINT STORY: narrative_overfitting
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
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
 *   human_readable: The Procrustean Plot
 *   domain: social/technological
 *
 * SUMMARY:
 *   The Procrustean Plot describes a structural constraint where complex
 *   real-world data is systematically compressed into simple, emotionally
 *   coherent narratives to satisfy engagement algorithms, cognitive biases,
 *   and production constraints. Named after Procrustes, who stretched or cut
 *   travelers to fit his bed, the constraint operates across news media,
 *   social platforms, academic communication, and public discourse. The
 *   mechanism involves both genuine coordination (audience cognitive limits,
 *   attention scarcity) and systematic extraction (platform incentives to
 *   maximize engagement, career rewards for compelling storytelling,
 *   algorithmic amplification of narrative simplicity). The constraint
 *   exhibits all six DR types from different perspectives: immutable from the
 *   analytical observer's naturalized cognition frame (mountain), pure
 *   extraction from the powerless affected population (snare), mixed
 *   coordination-extraction from journalists under deadline pressure (tangled
 *   rope), coordination from the platforms' perspective (rope), a temporary
 *   problem solvable by alternative infrastructure (scaffold), and a degraded
 *   gatekeeping ritual in academic publishing (piton). The theater ratio has
 *   risen from 0.35 to 0.65 over the interval, indicating that the constraint
 *   increasingly relies on performative markers of legitimacy (citations,
 *   expert attribution, narrative authority) while the actual epistemic
 *   content is compressed or distorted. The extractiveness has risen from
 *   0.28 to 0.52, reflecting increased platform optimization for
 *   engagement-driving simplicity.
 *
 * KEY AGENTS:
 *   - Engagement Platforms (Meta, Twitter, TikTok, algorithmic recommenders): Institutional beneficiary (institutional/arbitrage) — optimize ranking for engagement, which favors simplified, emotionally resonant narratives; benefit from reduced computational load and increased time-on-platform
 *   - Affected Populations (subjects of compressed narratives, marginalized groups overrepresented in sensationalized accounts): Primary victim (powerless/trapped) — cannot exit or contest simplified framings once distributed at scale; bear reputational and material costs
 *   - Epistemic Nuance (abstract collective good of accurate, multidimensional representation): Primary victim (powerless/trapped) — permanently squeezed by compression incentives; no self-advocate
 *   - Journalists and Storytellers (news organizations, freelancers, content creators): Secondary agent (moderate/constrained) — face competing pressures: editor demands for engagement, audience desire for simplicity, professional norm of accuracy; benefit from narrative clarity tools but constrained by deadline economics
 *   - Epistemic Governance Movements (fact-checkers, media literacy organizations, data journalists, open-source investigation networks): Organized agent (organized/mobile) — building alternative infrastructure for nuanced communication; see the constraint as temporary and solvable
 *   - Academic Publishing Gatekeepers (peer review journals, citation indices, university presses): Institutional actor (institutional/arbitrage) — maintain ritualistic authority over truth claims while actual public discourse shape is controlled by platforms outside their institution; piton classification reflects degraded institutional function
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing contingent platform design choices as immutable cognitive limits
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(narrative_overfitting, 0.52).
domain_priors:suppression_score(narrative_overfitting, 0.68).
domain_priors:theater_ratio(narrative_overfitting, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(narrative_overfitting, extractiveness, 0.52).
narrative_ontology:constraint_metric(narrative_overfitting, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(narrative_overfitting, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(narrative_overfitting, tangled_rope).
narrative_ontology:human_readable(narrative_overfitting, "The Procrustean Plot").
narrative_ontology:topic_domain(narrative_overfitting, "social/technological").

domain_priors:requires_active_enforcement(narrative_overfitting).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(narrative_overfitting, engagement_platforms).
narrative_ontology:constraint_beneficiary(narrative_overfitting, narrative_simplifiers).
narrative_ontology:constraint_beneficiary(narrative_overfitting, algorithmic_optimizers).
narrative_ontology:constraint_victim(narrative_overfitting, epistemic_nuance).
narrative_ontology:constraint_victim(narrative_overfitting, affected_populations).
narrative_ontology:constraint_victim(narrative_overfitting, ground_truth_integrity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: AFFECTED POPULATION (SNARE) — Subject of the compressed narrative; cannot exit or contest the simplified framing once it circulates. Trapped by algorithmic distribution and social proof. d≈0.92, f(d)≈1.40, σ=1.2 → χ≈0.72.
constraint_indexing:constraint_classification(narrative_overfitting, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: EPISTEMIC NUANCE (SNARE) — The abstract collective good of accurate representation; cannot defend itself; permanently squeezed by narrative compression. d≈0.95, f(d)≈1.42, σ=1.2 → χ≈0.74.
constraint_indexing:constraint_classification(narrative_overfitting, snare,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: JOURNALISTS/STORYTELLERS (TANGLED ROPE) — Face pressure to simplify for audience comprehension and editor approval, but also understand value of accuracy and nuance. Constrained by deadline and platform incentives but benefit from narrative coherence tools. d≈0.68, f(d)≈1.08, σ=1.0 → χ≈0.56.
constraint_indexing:constraint_classification(narrative_overfitting, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: ENGAGEMENT PLATFORMS (ROPE) — Benefit from algorithmic optimization of narrative simplicity. Coordinate users around high-engagement content. Experience constraint as coordination: condensed narratives solve the 'too-much-information' problem. d≈0.08, f(d)≈-0.08, σ=1.2 → χ≈-0.04. Net beneficiary.
constraint_indexing:constraint_classification(narrative_overfitting, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: EPISTEMIC GOVERNANCE MOVEMENTS (SCAFFOLD) — Organized efforts (fact-checking, media literacy, data journalism, open-source investigations) building alternative pathways for nuanced communication. See the Procrustean plot as a temporary problem solvable through norms and infrastructure (federated fact-checking, algorithmic transparency, long-form substacks). d≈0.45, f(d)≈0.45, σ=1.2 → χ≈0.25. Sunset horizon: transparent algorithms, accessible data journalism, media literacy norms mature over 15-20 years.
constraint_indexing:constraint_classification(narrative_overfitting, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: ACADEMIC PUBLISHING GATEKEEPERS (PITON) — Traditional peer review maintains theatrical rigor (peer-review process, citation formatting) while being largely powerless to prevent oversimplification in public discourse. The institutional ritual persists through inertia; the actual constraint enforcement happens via engagement algorithms, not academic institutions. theater_ratio=0.78 (high performative content; journals maintain gatekeeping appearance while public narratives are shaped elsewhere). d≈0.10, f(d)≈-0.05, σ=0.9 → χ≈-0.04.
constraint_indexing:constraint_classification(narrative_overfitting, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From civilizational/universal perspective, narrative compression is an immutable property of human cognition: working memory limits, attention scarcity, and pattern-matching heuristics make perfect fidelity impossible. All communication is selection; all selection is compression; compression always loses information. This perspective risks naturalizing what is actually a contingent institutional choice: the ratio of narrative simplicity to fidelity is engineered by platforms and incentive structures, not determined by neuroscience. ε=0.52, suppression=0.68, theater=0.65 contradict the mountain classification — false summit detected.
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
 *   Extractiveness (0.52): Moderate-high. The constraint involves genuine compression for engagement, but also systematic distortion for algorithmic optimization. The platforms benefit from simplified narratives (easier to rank, cheaper to moderate, higher engagement). Affected populations bear costs disproportionately when their complexity is flattened into a one-dimensional scandal or threat narrative. The growth from 0.28 to 0.52 over the interval reflects increasing algorithmic sophistication in ranking by engagement — the platforms have gotten better at extracting value from simplification. Suppression (0.68): High. Significant barriers to nuanced communication include platform architecture (character limits, algorithmic ranking), economic pressure (engagement-driven revenue), cognitive limits (attention scarcity), and cultural factors (preference for coherent narratives over ambiguous reality). Counterpoint: alternative platforms (Substack, longer-form journalism) exist but have not scaled to match social platform reach, suggesting that suppression is real but not total. Theater ratio (0.65): Moderate-high. Editorial authority, fact-checking labels, expert attribution, and narrative structure all serve as theatrical markers of legitimacy while the actual epistemic content is compressed. The rise from 0.35 reflects increased reliance on performative legitimacy as compression intensifies — journalists add expert quotes and citations to justify narrative simplification. The Procrustean plot is increasingly theatrical because the simplification it enforces is deeper.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates a full range of perspectival disagreement. The platforms see coordination (Rope) — solving the information overload problem through simplified narratives. The organized epistemic governance movement sees a temporary, solvable problem (Scaffold) — better norms and alternative infrastructure can increase nuance at scale. The affected population sees pure extraction (Snare) — their complexity is eliminated, and they have no recourse. Journalists see a mixed problem (Tangled Rope) — they understand accuracy matters, but face real constraints that push toward simplification. The academic publishing system sees itself as maintaining rigor (Piton) — peer review is still theater, but the actual constraint enforcement happens elsewhere. The analytical observer risks seeing an immutable law of cognition (Mountain) — all communication requires compression — but the structural data (suppression=0.68, extractiveness growing from 0.28 to 0.52) reveals this as contingent on platform design and engagement incentives, not cognitive limits. The perspectival gap is wide because the constraint operationalizes different structural relationships: beneficiaries experience it as coordination; victims experience it as extraction; organized actors experience it as solvable; passive observers naturalize it as inevitable.
 *
 * DIRECTIONALITY LOGIC:
 *   Engagement Platforms: Beneficiary + arbitrage → d≈0.05, f(d)≈-0.08. Net beneficiary. Platforms can arbitrage between different compression levels depending on user segment. Affected Populations: Victim + trapped → d≈0.92, f(d)≈1.40. Maximum extraction — no exit option once narrative circulates. Epistemic Nuance: Victim + trapped → d≈0.95, f(d)≈1.42. Maximum extraction — abstract collective cannot organize. Journalists: Mixed (victim on compression, beneficiary on clarity) + constrained → d≈0.68, f(d)≈1.08. Moderate-high extraction but not maximum because journalists can publish longer-form alternatives and maintain professional credibility through nuance. Epistemic Governance Movements: Organized + mobile → d≈0.45, f(d)≈0.45. Low effective extraction; coalition has agency and can build alternative systems. Academic Gatekeepers: Institutional + arbitrage → d≈0.10, f(d)≈-0.05. Piton classification comes from theater gate (0.78 in this perspective), not from extraction. They see themselves as controlling truth but have lost institutional power to platforms. Analytical Observer: analytical → d≈0.72, f(d)≈1.15. Mountain classification is perspectival; false summit detector catches this because base properties (suppression=0.68, extractiveness=0.52) contradict the natural law framing.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by decomposing the Procrustean plot into its genuine coordination function and its asymmetric extraction layers. The coordination function is real: audiences have limited attention, working memory constraints are neurological facts, and some compression is necessary for comprehension. But the extraction is also real: engagement algorithms optimize for simplification beyond what cognition requires, platforms benefit from the resulting engagement boost, and affected populations bear the cost of misrepresentation. The tangled_rope classification captures this hybrid: genuine coordination (solving attention scarcity) layered with asymmetric extraction (platforms capturing engagement value, affected populations bearing reputational cost). The scaffold perspective acknowledges that this is not an immutable mountain — alternative infrastructure (open-source journalism, federated platforms, media literacy) can shift the compression-nuance tradeoff. The snare perspectives (affected population, epistemic nuance) are legitimate: from their structural position, the constraint appears as pure extraction because they have no exit or defense. The mandatrophy is resolved by recognizing that multiple types are structurally correct from different positions: the constraint is genuinely mixed (tangled rope) at the systems level, but appears as pure extraction (snare) to powerless victims, and as coordination (rope) to beneficiaries. No single type is false — the presheaf structure is the answer.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    simplification_necessity_threshold,
    'What level of narrative simplification is necessary for effective communication versus extractive for the sake of engagement optimization?',
    'Comparative analysis: audience comprehension vs. retained nuance in simplified vs. full-detail narratives; correlation between engagement metrics and accuracy loss in historical cases',
    'If threshold ≤ 20% compression: Procrustean plot is mostly extraction. If threshold ≥ 60% compression: much simplification is legitimate coordination problem. Maps to snare vs. tangled_rope boundaries.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(simplification_necessity_threshold, empirical, 'Necessity threshold for narrative compression').

omega_variable(
    algorithmic_optimization_directionality,
    'Do engagement algorithms optimize for narrative simplicity because users demand it, or do they engineer demand for simplified narratives to reduce platform computational burden and maximize engagement metrics?',
    'A/B testing with different algorithm objectives; comparative platform analysis (platforms optimizing for engagement vs. platforms optimizing for time-on-detailed-content); user behavior under algorithm transparency',
    'If user-driven: constraint is more coordination (Rope from more perspectives). If algorithm-engineered: constraint is more extraction (Snare from more perspectives). Determines whether beneficiary is platforms or users.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(algorithmic_optimization_directionality, empirical, 'Whether algorithmic preference for simplicity reflects user demand or platform engineering').

omega_variable(
    alternative_narrative_infrastructure_viability,
    'Can alternative platforms (long-form, federated, open-source) genuinely support nuanced discourse at scale, or do they eventually face the same simplification pressures?',
    'Longitudinal analysis of Substack, Mirror, Bluesky, Mastodon discourse complexity over time; comparison of narrative simplification rates across platform architectures',
    'If viable alternatives exist: scaffold perspective is real, sunset is plausible. If all platforms eventually converge to simplification: constraint is structural (mountain or snare), not contingent (scaffold).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_narrative_infrastructure_viability, empirical, 'Viability of alternative narrative platforms for nuanced discourse').

omega_variable(
    cognitive_bias_versus_design_choice,
    'Is narrative overfitting driven primarily by human cognitive biases (narrativity bias, pattern-completion bias) or by platform design choices (ranking by engagement, algorithmic amplification)?',
    'Pre-digital historical analysis of narrative simplification rates; controlled studies isolating cognitive bias from platform affordances; comparison of discourse quality on platforms with different design choices',
    'If cognitive: constraint approaches mountain (immutable). If design: constraint is tangled_rope or snare (contingent, enforced by platforms). Affects mandatrophy resolution.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cognitive_bias_versus_design_choice, empirical, 'Whether overfitting is driven by cognition or platform design').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(narrative_overfitting, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(nof_tr_t0, narrative_overfitting, theater_ratio, 0, 0.35).
narrative_ontology:measurement(nof_tr_t5, narrative_overfitting, theater_ratio, 5, 0.5).
narrative_ontology:measurement(nof_tr_t10, narrative_overfitting, theater_ratio, 10, 0.65).

% Extraction over time
narrative_ontology:measurement(nof_be_t0, narrative_overfitting, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(nof_be_t5, narrative_overfitting, base_extractiveness, 5, 0.4).
narrative_ontology:measurement(nof_be_t10, narrative_overfitting, base_extractiveness, 10, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(narrative_overfitting, information_standard).
narrative_ontology:affects_constraint(narrative_overfitting, algorithmic_ranking_bias).
narrative_ontology:affects_constraint(narrative_overfitting, attention_scarcity).
narrative_ontology:affects_constraint(narrative_overfitting, epistemic_commons_degradation).

% DUAL FORMULATION NOTE:
% The Procrustean plot is downstream of platform architecture choices and algorithmic ranking mechanisms, which have their own constraint stories. The narrative_overfitting constraint (ε=0.52) represents the social-level manifestation of platform-level optimization choices. It affects higher-order epistemic goods (truth integrity, ground truth coherence) that are modeled as separate constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(narrative_overfitting, institutional, 0.78).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
