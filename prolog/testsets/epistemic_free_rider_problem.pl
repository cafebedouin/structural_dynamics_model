% ============================================================================
% CONSTRAINT STORY: epistemic_free_rider_problem
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_epistemic_free_rider_problem, []).

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
 *   constraint_id: epistemic_free_rider_problem
 *   human_readable: The Truth-Mining Exhaustion
 *   domain: informational/social/economic
 *
 * SUMMARY:
 *   The epistemic free-rider problem emerges at the intersection of
 *   information economics and the institutional structure of knowledge
 *   production. Primary researchers (scientists, investigative journalists,
 *   domain experts) bear the full cost of producing verified, grounded
 *   information through expensive processes: laboratory work, field
 *   investigation, peer review cycles, error correction, and reputation
 *   building. Simultaneously, the marginal cost of distributing derivative
 *   information — summaries, synthetic analyses, LLM-generated overviews —
 *   has collapsed to near-zero. This creates a two-tier epistemic market:
 *   ground-truth production is expensive and underrewarded; derivative
 *   consumption is cheap and abundant. The constraint exhibits tangled rope
 *   structure: it both coordinates (synthetic information providers solve the
 *   distribution problem) and extracts (costs are borne by a shrinking
 *   population of primary researchers while benefits accrue to intermediaries
 *   and consumers). The 'truth-mining exhaustion' refers not to depletion of
 *   facts but to exhaustion of the researcher cohort bearing unsustainable
 *   cost burdens. As researchers exit, the ground-truth infrastructure
 *   degrades, forcing downstream knowledge consumers to rely on increasingly
 *   corrupted synthetic information. This creates a collapse dynamic: the
 *   constraint's severity increases as exit accumulates, accelerating further
 *   exit.
 *
 * KEY AGENTS:
 *   - Primary Researchers: Victims (powerless/trapped) — bear full cost of verification; exit is blocked by credential systems and funding structures
 *   - Epistemic Commons: Victim (collective, powerless) — abstract good that cannot organize; contaminates via accumulated synthetic error
 *   - Downstream Knowledge Consumers: Mixed (moderate/constrained) — benefit from cheap access; suffer from accuracy degradation over derivation layers
 *   - Synthetic Information Providers: Beneficiary (institutional/arbitrage) — aggregate ground-truth at zero marginal cost; capture attention and platform rents
 *   - Attention-Capture Platforms: Beneficiary (institutional/arbitrage) — distribute synthetic derivatives frictionlessly; profit from engagement without bearing truth-mining cost
 *   - Academic Publishing System: Institutional actor (institutional/constrained) — maintains performative review system; blocks open distribution; extracts via subscription and article processing fees
 *   - Open Knowledge Movement: Organized agents (organized/mobile) — building alternative pathways; have exit options and sunset logic
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(epistemic_free_rider_problem, 0.58).
domain_priors:suppression_score(epistemic_free_rider_problem, 0.65).
domain_priors:theater_ratio(epistemic_free_rider_problem, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(epistemic_free_rider_problem, extractiveness, 0.58).
narrative_ontology:constraint_metric(epistemic_free_rider_problem, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(epistemic_free_rider_problem, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(epistemic_free_rider_problem, tangled_rope).
narrative_ontology:human_readable(epistemic_free_rider_problem, "The Truth-Mining Exhaustion").
narrative_ontology:topic_domain(epistemic_free_rider_problem, "informational/social/economic").

domain_priors:requires_active_enforcement(epistemic_free_rider_problem).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(epistemic_free_rider_problem, synthetic_information_providers).
narrative_ontology:constraint_beneficiary(epistemic_free_rider_problem, attention_capture_platforms).
narrative_ontology:constraint_victim(epistemic_free_rider_problem, primary_researchers).
narrative_ontology:constraint_victim(epistemic_free_rider_problem, epistemic_commons).
narrative_ontology:constraint_victim(epistemic_free_rider_problem, downstream_knowledge_consumers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PRIMARY RESEARCHER (SNARE) — Original researchers (scientists, journalists, domain experts) bear the full cost of producing ground-truth: laboratory work, field investigation, peer review cycles, verification, and correction. They receive minimal reward relative to effort. Exit options are trapped: career progression still requires publication in high-cost journals; abandoning research means forfeiting expertise and credentials. Suppression is maximal: the cost structure has no alternatives.
constraint_indexing:constraint_classification(epistemic_free_rider_problem, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: INFORMED CONSUMER (TANGLED ROPE) — Individuals who attempt to consume ground-truth information face barriers (paywalls, technical complexity, volume) but receive genuine coordination benefit from access to verified claims. Also bear extraction in the form of time spent filtering noise, subscription costs, and epistemic labor to validate sources. Mixed structure: benefits from the truth-mining ecosystem but must pay both directly and indirectly.
constraint_indexing:constraint_classification(epistemic_free_rider_problem, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: SYNTHETIC INFORMATION PROVIDER (ROPE) — Platforms and LLM-based systems aggregate ground-truth information at near-zero marginal cost and distribute it frictionlessly. They benefit from the truth-mining infrastructure without bearing production cost. This appears as pure coordination from their perspective: they solve the distribution problem. The extraction (cost transfer to researchers) is invisible to this agent — they see only the coordination function they provide.
constraint_indexing:constraint_classification(epistemic_free_rider_problem, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: ACADEMIC PUBLISHING SYSTEM (PITON) — Traditional peer review and journal publication once provided genuine verification and priority-setting. Now largely theatrical: journals serve as credential markers rather than truth filters, peer review is unpaid and slow, and preprints bypass formal publication entirely. The system persists through institutional inertia (tenure committees still weight journal publications) despite its degraded function. Theater ratio is high because the ritual of submission, review, revision, and acceptance is performatively maintained even as faster and cheaper distribution channels exist.
constraint_indexing:constraint_classification(epistemic_free_rider_problem, piton,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: OPEN KNOWLEDGE MOVEMENT (SCAFFOLD) — Organized agents (arXiv, preprint servers, open-access mandates, citizen science networks, fact-checking coalitions) are building alternative pathways that reduce truth-mining barriers and bypass rent-extraction intermediaries. These represent temporary support structures with sunset logic: as open archives mature and fund research directly, the extraction mechanisms of traditional publishing lose force. The movement sees suppression declining and exit options expanding.
constraint_indexing:constraint_classification(epistemic_free_rider_problem, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, producing verified information is inherently costly: someone must perform observation, experiment, investigation, and bear the risk of being wrong. The free-rider problem appears as a natural law of epistemology — verification is expensive, synthesis is cheap, and the gap creates extraction pressure. However, the structural data contradicts the mountain classification: the cost structure and reward distribution are institutional, not natural. The engine will compute this as a false summit.
constraint_indexing:constraint_classification(epistemic_free_rider_problem, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(epistemic_free_rider_problem_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(epistemic_free_rider_problem, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(epistemic_free_rider_problem, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(epistemic_free_rider_problem, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(epistemic_free_rider_problem, TR),
    TR >= 0.70.

:- end_tests(epistemic_free_rider_problem_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. The constraint extracts from primary researchers through asymmetric reward structures: they produce at high cost while synthetic providers distribute at near-zero cost and capture platform rents. The extraction is not maximal because researchers retain some agency (they can publish open-access, post preprints) and some funding exists outside the extraction mechanism. The value has increased over the measurement interval (0.22 → 0.58) as LLM-based synthesis has scaled, lowering marginal costs and making free-riding more lucrative. Suppression (0.65): High. Barriers to exit include: credential systems that require traditional publication, funding structures that reward journal placement, institutional career incentives that favor high-impact journals, and the tacit knowledge required to conduct primary research. These are structural, not contingent. Theater ratio (0.58): Moderate-high. Traditional peer review is substantially performative (reviewers cannot verify experiments retroactively), but some genuine quality-filtering occurs. The theater has increased as volume has increased — more papers are published, fewer are read carefully, and the review process becomes ritual rather than verification. Open-access preprints lower theater by eliminating the review bottleneck, but they create new epistemic risks (unfiltered claims). The constraint's theater is neither pure performance nor pure function.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates acute perspectival fragmentation. Primary researchers see extraction and exhaustion (snare). Synthetic providers see coordination and public good (rope). Informed consumers see mixed structure — they benefit from cheap access but suffer from information degradation (tangled rope). The publishing system sees itself as performing a necessary function despite its performative character (piton). The open knowledge movement sees a solvable temporary problem with alternatives on the horizon (scaffold). The analytical observer risks naturalizing the cost structure as inherent to epistemology (false summit). The perspectival gap is wide because the extraction mechanisms are structurally invisible to beneficiaries — they experience only the coordination function they provide.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality derives from structural position and exit capacity. Primary researchers are trapped victims with no alternatives within the credential system — they experience maximum d (≈0.95), producing maximal f(d). Synthetic providers are beneficiaries with arbitrage options — they experience low d (≈0.05), producing negative f(d). Informed consumers are mixed: they have moderate exit options (can seek primary sources, can pay for access) but are partially trapped by cost and complexity — they experience moderate d (≈0.55). The publishing system is an institutional intermediary that is constrained (cannot exit the prestige mechanism) but also partially benefits from it — they experience mid-range d (≈0.40). The open knowledge movement has mobile exit options (can build alternatives, can distribute outside traditional channels) — they experience lower d (≈0.35). Each perspective's chi is computed from base extractiveness, f(d), and scope modifier: ground-truth production is global in scope (σ=1.2), amplifying experienced extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint resolves mandatrophy by showing that tangled rope is structurally justified: the epistemic commons DOES receive coordination benefit (derivative information is distributed frictionlessly), AND there IS asymmetric extraction (costs concentrate on researchers while benefits accrue to platforms). Both functions coexist in the same mechanism. The constraint is not mislabeled as pure extraction (snare) because the synthetic provider's distribution function is genuinely valuable for consumers. It is not mislabeled as pure coordination (rope) because the cost structure is genuinely extractive for researchers. The tangled rope classification captures the true hybrid structure. The false summit (mountain) is the analytical observer's view that truth-mining costs are inherent to epistemology — they are institutionally contingent, as evidenced by the degradation of the constraint when institutional structures change (open access, preprints, public funding alternatives).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    ground_truth_production_cost,
    'What is the true minimum cost to produce verified, domain-expert information versus the marginal cost of synthetic derivative distribution?',
    'Empirical comparison: full accounting of research costs (labor, equipment, peer review, correction cycles) versus infrastructure costs of LLM-based distribution systems. Controlled study of information accuracy decay as synthesis layers increase.',
    'If cost ratio > 100:1, the extraction mechanism is structural and difficult to resolve. If ratio < 10:1, alternative funding models become viable and the snare weakens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ground_truth_production_cost, empirical, 'Cost ratio between ground-truth production and synthetic derivative distribution').

omega_variable(
    synthetic_information_accuracy_threshold,
    'At what point does accumulated synthetic information become unreliable for downstream decision-making?',
    'Longitudinal tracking of information drift: measure accuracy of synthetic summaries compared to original sources; identify domains where synthetic information produces measurable harm; compare error rates to human expert judgments.',
    'If threshold is very low: synthetic information becomes unfit for use quickly, creating demand for primary sources. If threshold is high: synthetic information remains useful even after multiple derivation layers, sustaining the free-rider structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(synthetic_information_accuracy_threshold, empirical, 'Information accuracy degradation threshold for synthetic derivatives').

omega_variable(
    researcher_exhaustion_critical_mass,
    'What fraction of primary researchers can exit before the truth-mining capacity collapses irreversibly?',
    'Historical precedent analysis (journal refusals, researcher strikes); simulation of knowledge production as a network with attrition feedback loops; measurement of minimum critical mass by discipline.',
    'If critical mass is very high (>70% must remain): researchers have some bargaining power. If very low (<20%): exhaustion can lead to catastrophic collapse with little warning.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(researcher_exhaustion_critical_mass, empirical, 'Critical mass threshold for researcher exit before production collapse').

omega_variable(
    institutional_substitution_feasibility,
    'Can public, institutional funding of research (without publication intermediaries) fully replace the current journal-based model?',
    'Analysis of research funding models that bypass commercial publication (NIH preprint repositories, direct public funding, institutional archives); comparison of knowledge production efficiency; political economy analysis of willingness to fund without prestige-capture.',
    'If feasible: scaffold perspective is not aspirational but structural. If not feasible: the extraction mechanism is resilient to institutional reform.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_substitution_feasibility, conceptual, 'Feasibility of institutional funding substitution for commercial publishing').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(epistemic_free_rider_problem, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(efr_tr_t0, epistemic_free_rider_problem, theater_ratio, 0, 0.32).
narrative_ontology:measurement(efr_tr_t15, epistemic_free_rider_problem, theater_ratio, 15, 0.45).
narrative_ontology:measurement(efr_tr_t30, epistemic_free_rider_problem, theater_ratio, 30, 0.58).

% Extraction over time
narrative_ontology:measurement(efr_be_t0, epistemic_free_rider_problem, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(efr_be_t15, epistemic_free_rider_problem, base_extractiveness, 15, 0.4).
narrative_ontology:measurement(efr_be_t30, epistemic_free_rider_problem, base_extractiveness, 30, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(epistemic_free_rider_problem, information_standard).
narrative_ontology:affects_constraint(epistemic_free_rider_problem, verification_bottleneck).
narrative_ontology:affects_constraint(epistemic_free_rider_problem, journal_publication_gatekeeping).
narrative_ontology:affects_constraint(epistemic_free_rider_problem, synthetic_information_reliability).

% DUAL FORMULATION NOTE:
% The truth-mining exhaustion is downstream of institutional structures that concentrate costs on researchers (journal gatekeeping, credential dependence) and upstream of information degradation effects (synthetic error accumulation). Three related constraints are structurally linked: (1) verification_bottleneck addresses the delay in confirming claims; (2) journal_publication_gatekeeping addresses the intermediary control of knowledge distribution; (3) synthetic_information_reliability addresses the accuracy decay in derivative information. This constraint models the economic incentive structure that drives the others.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(epistemic_free_rider_problem, institutional, 0.4).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
