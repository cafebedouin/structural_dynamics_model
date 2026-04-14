% ============================================================================
% CONSTRAINT STORY: ai_scholar_citation_trap
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ai_scholar_citation_trap, []).

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
 *   constraint_id: ai_scholar_citation_trap
 *   human_readable: AI Scholarly Citation System (OpenScholar Citation Trap)
 *   domain: technological/epistemic_infrastructure
 *
 * SUMMARY:
 *   OpenScholar synthesizes scientific research with citation accuracy
 *   claimed to match human expert performance. However, the system creates a
 *   structural extraction mechanism: as researchers rely on AI-synthesized
 *   citations for efficiency, verification costs migrate from AI developers
 *   (who could validate their own output) to the field (distributed
 *   researchers and institutions). The constraint exhibits six distinct types
 *   from different perspectives. The epistemic commons (research integrity as
 *   a public good) sees pure extraction because verification failures are
 *   socialized while benefits concentrate in developer margins. Human
 *   researchers without proprietary AI access see a snare because the career
 *   incentive system rewards citing through OpenScholar's synthesis, trapping
 *   them in dependence despite asymmetric access to underlying data.
 *   Developers see coordination — synthesizing citations enables knowledge
 *   distribution and model improvement. Institutions see tangled rope — they
 *   gain routing and subscription leverage but must validate AI output. The
 *   citation ritual system itself degrades into theater (peer review cannot
 *   verify synthesized citations). The analytical observer risks naturalizing
 *   this as inevitable — citation synthesis requires judgment — but the
 *   structural contingencies (proprietary training data, misaligned
 *   incentives, asymmetric verification costs) are contingent, not necessary.
 *
 * KEY AGENTS:
 *   - OpenScholar Development Team: Primary beneficiary (institutional/arbitrage) — captures citation authority and data flow; outsources verification costs to field
 *   - Research Verification Integrity: Primary victim (powerless/trapped) — epistemic commons cannot organize or exit; bears full socialization of verification failure costs
 *   - Human Researchers Without Proprietary AI Access: Secondary victim (moderate/trapped) — face career pressure to cite through OpenScholar; exit costly (citation gaps, reduced visibility)
 *   - Research Institutions and Publishers: Mixed actor (powerful/mobile) — benefit from integration and routing leverage; also bear validation and maintenance costs; retain exit options
 *   - Citation Verification Infrastructure: Institutional victim (organized/constrained) — libraries, quality assurance systems, errata mechanisms all forced to scale verification capacity
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks seeing inevitable coordination rather than contingent extraction
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_scholar_citation_trap, 0.52).
domain_priors:suppression_score(ai_scholar_citation_trap, 0.65).
domain_priors:theater_ratio(ai_scholar_citation_trap, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_scholar_citation_trap, extractiveness, 0.52).
narrative_ontology:constraint_metric(ai_scholar_citation_trap, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(ai_scholar_citation_trap, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_scholar_citation_trap, snare).
narrative_ontology:human_readable(ai_scholar_citation_trap, "AI Scholarly Citation System (OpenScholar Citation Trap)").
narrative_ontology:topic_domain(ai_scholar_citation_trap, "technological/epistemic_infrastructure").

domain_priors:requires_active_enforcement(ai_scholar_citation_trap).
% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_scholar_citation_trap, ai_model_developers).
narrative_ontology:constraint_beneficiary(ai_scholar_citation_trap, citation_aggregation_platforms).
narrative_ontology:constraint_victim(ai_scholar_citation_trap, research_verification_integrity).
narrative_ontology:constraint_victim(ai_scholar_citation_trap, human_researchers_without_ai_access).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: EPISTEMIC COMMONS (SNARE) — Research integrity and citation reliability cannot exit the system as citation authority consolidates around AI synthesis. No alternatives exist; verification costs are borne by the field. The epistemic commons has no advocate and cannot organize to escape the constraint. Maximum experienced extraction.
constraint_indexing:constraint_classification(ai_scholar_citation_trap, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: HUMAN RESEARCHERS (SNARE) — Researchers without access to proprietary AI systems face pressure to cite through OpenScholar's synthesis rather than primary sources. Career incentives reward citing high-authority AI-synthesized claims. Exit is costly: refusing AI-mediated citation creates citation coverage gaps and reduces visibility. Trapped by citation economy.
constraint_indexing:constraint_classification(ai_scholar_citation_trap, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: AI DEVELOPERS (ROPE) — OpenScholar benefits from ecosystem integration. Developers experience the constraint as coordination: synthesizing citations enables knowledge distribution and model improvement through feedback loops. Net beneficiary through institutional arbitrage — they set citation standards and capture citation data flow.
constraint_indexing:constraint_classification(ai_scholar_citation_trap, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: INSTITUTIONS AND PUBLISHERS (TANGLED ROPE) — Universities and academic publishers both benefit (routing through institutional citations, subscription models) and bear costs (pressure to validate AI citations, maintenance burden, loss of direct researcher relationship). Powerful institutions retain mobile exit options but face coordination demands to legitimate AI citation authority.
constraint_indexing:constraint_classification(ai_scholar_citation_trap, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: CITATION RITUAL SYSTEM (PITON) — Traditional peer review of citations and manual bibliography verification is increasingly theatrical: reviewers cannot verify all citations manually when synthesized via AI. The ritual persists through institutional inertia (citation formats, review expectations) despite reduced verification capacity. Theater ratio high (0.68) reflects performative validation of AI citations passing through traditional venues.
constraint_indexing:constraint_classification(ai_scholar_citation_trap, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (FALSE SUMMIT CHECK) — From a universal perspective, citation aggregation by any system (AI or human) requires synthesis of multiple sources and implicit judgment calls about which sources are authoritative. This perspective risks naturalizing the AI synthesis arrangement as inevitable convergence. However, structural contingencies (proprietary model training data, incentive misalignment, verification cost transfer) reveal this as a false summit — not a mountain.
constraint_indexing:constraint_classification(ai_scholar_citation_trap, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ai_scholar_citation_trap_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(ai_scholar_citation_trap, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(ai_scholar_citation_trap, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(ai_scholar_citation_trap, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(ai_scholar_citation_trap, TR),
    TR >= 0.70.

:- end_tests(ai_scholar_citation_trap_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. OpenScholar captures efficiency gains (researchers save synthesis time) and redirects verification burden to field. The developers extract from the asymmetry: they benefit from citation authority and data flow while verification remains externalized. However, extractiveness is not maximal (0.70+) because some benefits are genuine — researchers do save time, and accurate citations improve research quality. The temporal trajectory shows increasing extractiveness: at T=0 (early adoption), OpenScholar's citations were treated as useful but not authoritative (low extraction, high trust that humans verify). By T=6, citations are accepted with minimal secondary verification, shifting verification costs downstream. Suppression (0.65): High. Multiple barriers limit researchers' exit: (1) career incentive systems reward citing through authoritative synthesis platforms; (2) institutional adoption creates path dependency; (3) lack of transparency in synthesis methodology creates asymmetric information; (4) proprietary training data prevents independent verification. Theater ratio (0.68): High. Traditional peer review for cited sources in AI-synthesized bibliographies is increasingly performative — reviewers cannot verify all citations manually. The ritual of citing 'peer-reviewed sources' persists while actual verification capacity declines. Over the interval, theater increases as OpenScholar citations proliferate beyond reviewers' capacity to check.
 *
 * PERSPECTIVAL GAP:
 *   The original perspective (developer/beneficiary) sees coordination and epistemic improvement — OpenScholar solves the real problem of synthesizing complex literatures. The powerless perspective (research integrity) sees pure extraction because verification failures are distributed and unsupervised. The moderate perspective (researchers without access) sees snare because the system creates path-dependent career pressure. The powerful perspective (institutions) sees tangled rope — they both benefit (subscription/routing leverage) and bear costs (validation burden). The degraded perspective (citation ritual) sees piton — peer review persists theater-like, without verification function. The analytical perspective risks seeing mountain (inevitable synthesis logic) but structural analysis reveals false summit. This perspectival gap is the diagnostic signature of the constraint — not everyone agrees the system is extractive because beneficiaries, developers, and institutional actors experience genuine coordination benefits alongside the extraction mechanism.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is driven by structural position relative to verification cost distribution. OpenScholar developers have low d (full beneficiary: they set standards, capture authority, outsource verification). Human researchers without access have high d (full target: trapped by career incentives, bear verification costs, lack access to underlying data). Institutions have moderate d (mixed: they benefit from routing but also must maintain validation infrastructure). The epistemic commons has maximum d (trapped victim: cannot organize or negotiate). The derivation from beneficiary/victim declarations: developers are primary beneficiary (low d → low/negative effective extraction for them); research integrity and non-AI-accessing researchers are victims (high d → high effective extraction against them). The sigmoid function f(d) converts raw positional vulnerability into experienced extractiveness. Beneficiary exit via arbitrage (developers can choose which platforms to supply to) keeps their experienced extraction low; victim exit blocked (researchers cannot easily exit research dependence) keeps their experienced extraction high.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    citation_hallucination_threshold,
    'At what error rate (hallucinated or misattributed citations) does OpenScholar''s synthesis cease to function as a coordination mechanism and become pure extraction?',
    'Longitudinal audit of OpenScholar citations against primary sources; correlation between error rate and researcher reliance; comparison with human expert citation error rates',
    'If error rate < 2%: remains coordination mechanism (Rope classification more plausible). If error rate > 8%: extraction dominates; snare classification confirmed. Current empirical status contested — OpenAI claims accuracy parity with humans; independent audits show 5-12% misattribution rates depending on domain.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(citation_hallucination_threshold, empirical, 'Citation accuracy threshold determining coordination vs extraction classification').

omega_variable(
    verification_cost_asymmetry,
    'Who bears the cost of verifying AI-synthesized citations — the researcher, the institution, the AI developer, or the field collectively?',
    'Cost accounting across verification pathways: manual citation checking time, institutional library validation infrastructure, developer quality assurance investment, field-wide correction mechanisms (errata, retraction systems)',
    'If developers bear most costs: Rope classification (coordination with shared burden). If field bears costs: Snare classification (extraction via verification cost transfer). Current reality: fragmented — researchers bear local costs, institutions invest in validation infrastructure, developers invest minimally in post-deployment verification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(verification_cost_asymmetry, empirical, 'Distribution of citation verification costs across actors').

omega_variable(
    citation_authority_consolidation,
    'Does OpenScholar''s integration into publication workflows create path-dependent lock-in, or do alternative citation systems (manual, decentralized, human-curated) retain meaningful market share?',
    'Tracking adoption curves for competing citation methodologies; analysis of citation diversity in new publications over 5-10 year horizon; network effects modeling for citation authority concentration',
    'If lock-in: extraction mechanism is self-reinforcing (victims cannot switch); snare classification confirmed long-term. If alternatives thrive: suppression declines; constraint may degrade to Scaffold or Piton. Current status: early adoption phase — lock-in signals present but not yet irreversible.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(citation_authority_consolidation, empirical, 'Whether OpenScholar creates irreversible path-dependent lock-in').

omega_variable(
    source_attribution_reversibility,
    'Can researchers reliably trace OpenScholar''s citations back to original sources and audit synthesis quality, or is the synthesis process opaque enough to create epistemic asymmetry?',
    'Usability testing of citation traceability; measurement of time/expertise required to verify provenance chains; comparison with human expert citation transparency',
    'If transparent: suppression declines; researchers retain exit option (can audit and choose alternatives). If opaque: suppression high; extraction mechanism is sustained by information asymmetry.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(source_attribution_reversibility, empirical, 'Transparency and reversibility of AI citation synthesis').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_scholar_citation_trap, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(aisch_tr_t0, ai_scholar_citation_trap, theater_ratio, 0, 0.32).
narrative_ontology:measurement(aisch_tr_t3, ai_scholar_citation_trap, theater_ratio, 3, 0.5).
narrative_ontology:measurement(aisch_tr_t6, ai_scholar_citation_trap, theater_ratio, 6, 0.68).

% Extraction over time
narrative_ontology:measurement(aisch_be_t0, ai_scholar_citation_trap, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(aisch_be_t3, ai_scholar_citation_trap, base_extractiveness, 3, 0.35).
narrative_ontology:measurement(aisch_be_t6, ai_scholar_citation_trap, base_extractiveness, 6, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_scholar_citation_trap, information_standard).
narrative_ontology:affects_constraint(ai_scholar_citation_trap, research_publication_gatekeeping).
narrative_ontology:affects_constraint(ai_scholar_citation_trap, large_language_model_training_data_provenance).
narrative_ontology:affects_constraint(ai_scholar_citation_trap, academic_citation_metrics_as_career_incentive).

% DUAL FORMULATION NOTE:
% The AI scholarly citation system decomposes into structural components: (1) citation synthesis accuracy (epistemic question, low ε, rope/mountain), (2) verification cost distribution (institutional question, high ε, snare), (3) citation authority consolidation (path-dependency question, moderate ε, scaffold/tangled rope depending on timeline). This story focuses on the verification cost transfer mechanism (component 2, ε=0.52) which is the primary extraction driver. Component 1 alone would classify as rope (coordination); component 2 alone is snare. The combined constraint exhibits snare dynamics because the verification cost externalization dominates the epistemic coordination benefit.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(ai_scholar_citation_trap, organized, 0.48).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
