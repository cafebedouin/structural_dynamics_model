% ============================================================================
% CONSTRAINT STORY: epistemic_overload_collapse
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
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
 *   The epistemic overload constraint describes the structural condition
 *   where information volume, velocity, and contradiction exceed human
 *   cognitive processing capacity, creating a collapse of deliberative
 *   capacity and epistemic commons health. The constraint exhibits
 *   characteristics of both coordination failure (information distribution
 *   was originally a beneficial coordination mechanism) and pure extraction
 *   (algorithmic optimization for engagement has created perverse incentives
 *   that maximize signal-drowning rather than signal-clarity). The theater
 *   ratio of 0.64 reflects that much contemporary information consumption is
 *   performative: individuals consume information to feel informed or to
 *   maintain social coordination rather than to build reliable epistemic
 *   models. The measurement trajectory shows acceleration of extractiveness
 *   over the interval — from mild information abundance (0.22) to critical
 *   overload (0.58) — driven by algorithmic optimization, notification
 *   proliferation, and publishing incentive misalignment. This is a
 *   diagnostic case where the 'rope' hypothesis (coordination failure) is
 *   contestable by the 'snare' hypothesis (designed extraction).
 *
 * KEY AGENTS:
 *   - Individual Cognition: Primary victim (powerless/trapped) — biological processing limits exceeded by orders of magnitude; no exit from digital information environment
 *   - Attention Extractors: Primary beneficiary (institutional/arbitrage) — advertisers, content producers, engagement-optimized platforms capture cognitive surplus
 *   - Algorithmic Gatekeepers: Primary beneficiary (institutional/arbitrage) — control information filtering and feed ranking; benefit from engagement rent
 *   - Epistemic Commons: Primary victim (powerless/trapped) — abstract collective good; bears cost of signal-drowning through degraded reliable information availability
 *   - Deliberative Capacity: Primary victim (powerless/trapped) — public discourse requires time/attention below overload threshold; collective decision-making degrades
 *   - Professional Knowledge Workers: Secondary victim (moderate/constrained) — domain-specific overload constrains work quality and increases cognitive fatigue
 *   - Cognitive Sovereignty Movement: Organized actors (organized/constrained) — digital literacy advocates, EU regulators, attention-rights coalitions building exit pathways
 *   - Legacy Media Institutions: Institutional actors (institutional/arbitrage) — maintain filtering role through inertia; their gatekeeping function has atrophied but persists
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(epistemic_overload_collapse, 0.58).
domain_priors:suppression_score(epistemic_overload_collapse, 0.68).
domain_priors:theater_ratio(epistemic_overload_collapse, 0.64).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(epistemic_overload_collapse, extractiveness, 0.58).
narrative_ontology:constraint_metric(epistemic_overload_collapse, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(epistemic_overload_collapse, theater_ratio, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(epistemic_overload_collapse, tangled_rope).
narrative_ontology:human_readable(epistemic_overload_collapse, "The Signal-Drowning Vortex").
narrative_ontology:topic_domain(epistemic_overload_collapse, "cognitive/informational/technological").

domain_priors:requires_active_enforcement(epistemic_overload_collapse).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(epistemic_overload_collapse, attention_extractors).
narrative_ontology:constraint_beneficiary(epistemic_overload_collapse, algorithmic_gatekeepers).
narrative_ontology:constraint_beneficiary(epistemic_overload_collapse, content_producers_optimized_for_engagement).
narrative_ontology:constraint_victim(epistemic_overload_collapse, epistemic_commons).
narrative_ontology:constraint_victim(epistemic_overload_collapse, individual_cognition).
narrative_ontology:constraint_victim(epistemic_overload_collapse, deliberative_capacity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE OVERWHELMED INDIVIDUAL (SNARE) — A human subject confronted with 2,000+ information claims daily cannot exit the stream: employment, social coordination, civic participation all require engagement with the digital information environment. Biological processing limits (working memory ~4 items, attention span compressed by continuous interruption) are exceeded by orders of magnitude. No alternative information diet exists in modern institutional contexts. Maximum experienced extraction.
constraint_indexing:constraint_classification(epistemic_overload_collapse, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: THE PROFESSIONAL KNOWLEDGE WORKER (TANGLED ROPE) — Constrained by employment requirements to monitor information flows in specialized domains (law, medicine, finance, security) but also benefits from the coordination function of rapid information distribution. The overload enables some work (fast detection of domain-relevant developments) while extracting attention rent. Can select filters and domains (constrained exit) but cannot fully exit the system.
constraint_indexing:constraint_classification(epistemic_overload_collapse, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: THE ALGORITHMIC GATEKEEPER (ROPE) — Platform operators (social media, search, news aggregation) experience the overload constraint as a pure coordination mechanism: the same information distribution infrastructure that drowns individuals enables platforms to capture attention value, feed algorithmic optimization, and extract engagement rent. They can arbitrage between user cognitive load and advertiser access (arbitrage exit). Net beneficiary.
constraint_indexing:constraint_classification(epistemic_overload_collapse, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: THE COGNITIVE SOVEREIGNTY MOVEMENT (SCAFFOLD) — Organized actors (Cal Newport's Digital Minimalism advocates, EU digital regulation bodies, attention-rights coalitions) see epistemic overload as a temporary coordination failure with structural sunset: digital literacy, attention design standards, regulator-enforced transparency in algorithmic curation, and decentralized information infrastructure are building alternative pathways that restore human-scale information consumption. High suppression is tolerated only if declining — architectural changes to notification systems, content moderation transparency, and algorithmic accountability have sunset clauses built in.
constraint_indexing:constraint_classification(epistemic_overload_collapse, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: LEGACY MEDIA INSTITUTION (PITON) — Traditional media gatekeepers (newspaper editors, broadcast networks) maintained a coordination function by filtering information volume to human-processable levels. That function has atrophied: legacy gatekeeping is performative theater (opinion segments, scandal focus) maintained through institutional inertia while the real information volume comes from algorithmic feeds. Editors see their own role as degraded — filtering is the last function, and it persists because no single replacement has fully emerged, not because it works.
constraint_indexing:constraint_classification(epistemic_overload_collapse, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: THE ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational/universal perspective, information overload is a natural limit of human cognition: our working memory capacity is fixed by neurobiology (~7±2 items), attention is a zero-sum resource, and as information sources multiply, overload becomes inevitable. This perspective sees epistemic collapse as an immutable feature of intelligent systems dealing with exponentially growing information. The engine's false summit detector will identify this as naturalization of a contingent technological/institutional arrangement.
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
 *   Extractiveness (0.58): Moderately high. The constraint extracts attention rent and cognitive labor from individuals at scale. However, the extraction is not maximal (0.70+) because some coordination function remains — information distribution does enable legitimate coordination and knowledge transfer. The extraction comes from the mismatch between what information infrastructure optimizes for (engagement, virality) and what individuals and societies actually need (reliable, comprehensible, decision-relevant information). The trajectory shows acceleration from 0.22 to 0.58 over the interval, driven by algorithmic optimization and notification proliferation. Suppression (0.68): High. Significant barriers to exit include employment requirements to monitor information flows, social coordination dependencies on platform participation, and the absence of alternative information diets that match institutional requirements. Individuals cannot simply opt out without losing access to critical information or social participation. However, suppression is not total (0.90+) because selective filtering, domain-specific curation, and some alternatives (specialty publications, curated newsletters) exist. Theater ratio (0.64): High. Much information consumption is performative: individuals scan headlines to feel informed, engage in social media to maintain presence, consume news to participate in collective discourse rather than to build accurate models. The performative content has increased as algorithmic feeds have replaced editorial gatekeeping — feeds prioritize novelty and emotional response over truth-value or decision relevance.
 *
 * PERSPECTIVAL GAP:
 *   This constraint generates a large perspectival gap reflecting fundamental disagreement about causation and solvability. The algorithmic gatekeeper sees pure coordination (rope) — they are solving the legitimate problem of information distribution at scale. The cognitive sovereignty movement sees a temporary design problem with architectural sunset (scaffold) — notification systems and algorithmic ranking can be redesigned to prioritize clarity over engagement. The individual subject sees extraction (snare) — they cannot escape the information stream and cannot distinguish signal from noise. The knowledge worker sees mixed coordination and extraction (tangled_rope) — the system enables their domain-specific work while imposing general cognitive burden. The legacy media institution sees its own degraded role (piton) — editorial filtering persists through institutional inertia but has lost functional value. The civilizational analytical observer risks seeing an immutable natural law (mountain) — cognitive limits are fixed by neurobiology. This perspectival range is wider than the verification bottleneck because the constraint involves both technological (design choice) and biological (neurocognitive limits) components, and disagreement about which dominates.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality derives from structural position in the extraction flow. The overwhelmed individual (powerless/trapped) occupies the target position: they are the source of extracted attention value, experiencing no exit option. The algorithmic gatekeeper (institutional/arbitrage) occupies the beneficiary position: they control the filtering mechanism and profit from attention capture. The professional knowledge worker (moderate/constrained) occupies an intermediate position: they benefit from rapid information access for domain-specific work but suffer from general overload. The cognitive sovereignty movement (organized/constrained) occupies an exit-builder position: they see the constraint as solvable through architectural change rather than individual discipline. The legacy media institution (institutional/arbitrage) would appear as beneficiary by extraction logic but is classified as piton because its gatekeeping function has atrophied and is maintained through institutional inertia rather than actual coordination value. The analytical observer (analytical/analytical) risks naturalizing the constraint as an immutable limit of cognition rather than a contingent design choice.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION: The constraint resolves mandatrophy by revealing that the 'rope vs snare' ambiguity maps onto 'coordination failure vs designed extraction' — two genuinely distinct structural mechanisms. The rope hypothesis (information distribution is a coordination mechanism gone awry due to volume) is structurally correct but empirically incomplete. The snare hypothesis (algorithmic engagement optimization is designed extraction) is also structurally correct. The constraint is tangled_rope because it has BOTH: a genuine coordination function (information distribution enables knowledge transfer and social coordination) AND asymmetric extraction (algorithmic optimization for engagement captures attention value from individual cognition to benefit platforms and advertisers). The mandatrophy is resolved by requiring both beneficiary and victim groups: beneficiaries are attention extractors and algorithmic gatekeepers; victims are individual cognition and the epistemic commons. The theater ratio (0.64) confirms the tangled_rope classification — much of the constraint's operation is performative theater (engagement metrics, trending algorithms, notification rituals) rather than functional information curation. The scaffold perspective shows a real exit path: redesigned notification systems, algorithmic transparency, and attention-rights regulation can reduce the extraction while preserving the coordination function. This is not a false rope that is secretly a snare — it is genuinely both.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    biological_vs_technological_causation,
    'Is epistemic overload caused by immutable human cognitive limits (neurobiology) or by reversible technological/institutional design choices (algorithms, notification systems, publishing incentives)?',
    'Historical comparison of attention patterns in information-rich non-digital environments (libraries, archives, academic institutions circa 1980-2000) vs modern digital environments with identical information diversity but different interface design. Controlled studies of notification-free digital environments.',
    'If biological: mountain classification is correct, constraint is immutable. If technological: mountain is false summit, constraint is tangled_rope/snare with institutional design as the mechanism.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(biological_vs_technological_causation, empirical, 'Whether overload is caused by neurobiology or technological design').

omega_variable(
    algorithmic_curation_sufficiency,
    'Can algorithmic filtering actually reduce overload without introducing new forms of extraction (filter bubble closure, attention manipulation, recommendation-based addiction)?',
    'Comparative analysis of user cognitive load, epistemic diversity, and decision quality in curated vs non-curated information environments. Longitudinal measurement of filter bubble effects in algorithmic vs human-curated feeds.',
    'If yes: scaffold/rope perspective confirmed — algorithmic mediation can solve overload. If no: overload + filter closure creates double trap, classification shifts to pure snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(algorithmic_curation_sufficiency, empirical, 'Whether algorithmic curation can reduce overload without new extraction').

omega_variable(
    collective_action_requirement,
    'Does solving epistemic overload require individual behavior change (digital minimalism, attention discipline) or collective institutional redesign (platform regulation, publishing incentive reform)?',
    'Meta-analysis of cognitive load reduction across voluntary individual interventions vs mandatory architectural changes to information systems. Comparison of persistence rates in personal digital minimalism vs regulatory-enforced platform transparency.',
    'If individual: constraint is experienced as personal failing (snare from powerless perspective becomes self-authored). If collective: constraint is structural design choice (tangled_rope, scaffold approaches are viable).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(collective_action_requirement, conceptual, 'Whether solving overload requires individual or collective action').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(epistemic_overload_collapse, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(eoc_tr_t0, epistemic_overload_collapse, theater_ratio, 0, 0.35).
narrative_ontology:measurement(eoc_tr_t5, epistemic_overload_collapse, theater_ratio, 5, 0.52).
narrative_ontology:measurement(eoc_tr_t10, epistemic_overload_collapse, theater_ratio, 10, 0.64).

% Extraction over time
narrative_ontology:measurement(eoc_be_t0, epistemic_overload_collapse, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(eoc_be_t5, epistemic_overload_collapse, base_extractiveness, 5, 0.42).
narrative_ontology:measurement(eoc_be_t10, epistemic_overload_collapse, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(epistemic_overload_collapse, information_standard).
narrative_ontology:affects_constraint(epistemic_overload_collapse, filter_bubble_closure).
narrative_ontology:affects_constraint(epistemic_overload_collapse, social_media_addiction_loop).
narrative_ontology:affects_constraint(epistemic_overload_collapse, polarization_acceleration).
narrative_ontology:affects_constraint(epistemic_overload_collapse, decision_quality_degradation).

% DUAL FORMULATION NOTE:
% Epistemic overload is decomposable into distinct mechanisms: (1) information volume exceeding biological processing limits (neurocognitive constraint), (2) algorithmic ranking optimizing for engagement rather than clarity (technological design), (3) publishing/attention-capture incentives misaligned with epistemic value (institutional structure). This story models the composite constraint. Downstream constraints model specific failure modes: filter bubbles, addiction loops, polarization, decision degradation. Each has its own epsilon and classification.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
