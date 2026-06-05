% ============================================================================
% CONSTRAINT STORY: long_term_institutional_memory
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_long_term_institutional_memory, []).

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
 *   constraint_id: long_term_institutional_memory
 *   human_readable: Long-Term Institutional Memory Preservation and Access Control
 *   domain: institutional_governance/organizational_systems
 *
 * SUMMARY:
 *   Long-term institutional memory — the preservation, control, and access to
 *   organizational decision records, debates, and evidence — creates a
 *   structural tension between institutional leadership's need for decision
 *   flexibility and future stakeholders' need for accountability and
 *   learning. The constraint operates across all institutional forms:
 *   governments, corporations, universities, nonprofits, and international
 *   bodies all face the same dynamic: memory preservation is simultaneously a
 *   coordination mechanism (enabling institutional continuity and learning
 *   from past decisions) and an extraction mechanism (enabling leaders to
 *   control narratives, suppress evidence of failures, and insulate current
 *   decisions from historical context). The theater ratio measures the
 *   performative component of memory systems — the existence of archives and
 *   documentation rituals divorced from genuine preservation function. Over
 *   the interval, theater has increased as institutions have invested in
 *   documentation compliance (satisfying formal requirements) while actual
 *   preservation capacity lags behind digital acceleration and information
 *   volume. Distributed memory movements (open records, whistleblower
 *   networks, public archives, journalistic investigation) represent an
 *   emerging bypass pathway that erodes institutional memory monopolies.
 *
 * KEY AGENTS:
 *   - Institutional Leadership: Primary beneficiary (institutional/arbitrage) — controls memory narrative; benefits from selective retention during decision tenure
 *   - Future Stakeholders: Primary victim (powerless/trapped) — those who inherit institutional legacies without access to reasoning or evidence behind decisions
 *   - Institutional Historians: Secondary victim and partial beneficiary (moderate/constrained) — research function depends on access but gatekeeping constrains independent analysis
 *   - Archival Systems: Institutional actor (institutional/arbitrage) — performs legitimacy function while actual memory preservation capacity has atrophied (piton perspective)
 *   - Distributed Memory Actors: Organized agents (organized/mobile) — journalists, whistleblowers, open-records advocates, public archivists with agency and exit paths
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent power dynamics as inherent limits on organizational memory
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(long_term_institutional_memory, 0.52).
domain_priors:suppression_score(long_term_institutional_memory, 0.48).
domain_priors:theater_ratio(long_term_institutional_memory, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(long_term_institutional_memory, extractiveness, 0.52).
narrative_ontology:constraint_metric(long_term_institutional_memory, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(long_term_institutional_memory, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(long_term_institutional_memory, tangled_rope).
narrative_ontology:human_readable(long_term_institutional_memory, "Long-Term Institutional Memory Preservation and Access Control").
narrative_ontology:topic_domain(long_term_institutional_memory, "institutional_governance/organizational_systems").

domain_priors:requires_active_enforcement(long_term_institutional_memory).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(long_term_institutional_memory, institutional_leadership).
narrative_ontology:constraint_beneficiary(long_term_institutional_memory, knowledge_gatekeepers).
narrative_ontology:constraint_victim(long_term_institutional_memory, future_stakeholders).
narrative_ontology:constraint_victim(long_term_institutional_memory, external_accountability).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: FUTURE STAKEHOLDER (SNARE) — Those who must live with institutional decisions lack access to the reasoning, debates, and evidence that informed them. Trapped by temporal distance and information asymmetry; cannot exit or organize retroactively. Bears full cost of institutional forgetfulness or deliberate erasure.
constraint_indexing:constraint_classification(long_term_institutional_memory, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: INSTITUTIONAL HISTORIAN (TANGLED ROPE) — Constrained by gatekeeping of archives, selective disclosure policies, and funding dependence on the institution. Benefits from access to memory systems for research and legitimacy; also victimized by incompleteness and access restrictions. Experiences both coordination (enabling institutional learning) and extraction (controlling narrative).
constraint_indexing:constraint_classification(long_term_institutional_memory, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: INSTITUTIONAL LEADERSHIP (ROPE) — Benefits from selective memory: coordination of institutional continuity without the burden of full transparency. Can arbitrage between available memory (what is documented) and suppressed memory (what is forgotten). Experiences memory preservation as a coordination mechanism for succession and consistency.
constraint_indexing:constraint_classification(long_term_institutional_memory, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: ARCHIVAL SYSTEM (PITON) — Traditional institutional archives are substantially performative: repositories exist but are poorly indexed, incompletely curated, and selectively accessible. The ritual of 'maintaining records' persists through regulation and legitimacy needs, but actual preservation function has atrophied as digital systems create new bottlenecks. Theater ratio high because the archive satisfies formal documentation requirements while serving actual memory preservation poorly.
constraint_indexing:constraint_classification(long_term_institutional_memory, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: DISTRIBUTED MEMORY MOVEMENT (SCAFFOLD) — Open documentation, whistleblower networks, public archives, and decentralized record-keeping provide alternative pathways to institutional memory that bypass gatekeeping. Organized actors (journalists, historians, activists, open-records advocates) have agency and exit options. This represents a temporary coordination problem being solved — institutional monopoly on memory is eroding. Has sunset clause: as digital tools and legal norms evolve toward transparency, the gatekeeping mechanism loses force.
constraint_indexing:constraint_classification(long_term_institutional_memory, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a universal perspective, institutional forgetting is an inherent property of complex organizations: memory costs resources, and selective retention is a necessary cognitive process. The constraint appears as an immutable law of organizational behavior. However, structural data reveals this as a false summit — the gatekeeping and suppression are contingent institutional arrangements, not laws of nature.
constraint_indexing:constraint_classification(long_term_institutional_memory, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(long_term_institutional_memory_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(long_term_institutional_memory, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(long_term_institutional_memory, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(long_term_institutional_memory, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(long_term_institutional_memory, TR),
    TR >= 0.70.

:- end_tests(long_term_institutional_memory_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. Institutional leadership captures benefits (control over institutional narrative, ability to reframe decisions, insulation from historical accountability) during their tenure. Future stakeholders lose access to reasoning and context, increasing decision hazard and enabling repeated institutional errors. The extraction is real but not total — substantial memory is preserved (voluntarily or through regulatory compliance), and some gatekeeping serves legitimate coordination purposes (protecting ongoing operations, preserving commercial competitiveness, managing security concerns). The value reflects that mixed coordination and extraction coexist. Suppression (0.48): Moderate. Barriers to memory access include classification policies, archival gatekeeping, incomplete documentation, digital format obsolescence, and institutional defensiveness. But suppression is not total — many institutions have public record requirements, some archives are accessible, and external documentation (journalism, testimony) provides partial substitutes. The suppression has increased over the interval as digital complexity and information volume have created new gatekeeping bottlenecks. Theater ratio (0.65): Moderately high and rising. Institutional archives satisfy formal documentation requirements while actual preservation function is compromised by poor indexing, incomplete curation, format obsolescence, and selective accessibility. The performative component increases when institutions invest in documentation compliance without addressing preservation capacity. Theater has risen from 0.40 to 0.65 as institutional size and complexity have outpaced archival methodology.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates perspectival divergence across power positions. Leadership sees coordination (rope) — the legitimate need to manage information flow and institutional continuity. Future stakeholders see extraction (snare) — loss of decision context and accountability. Historians see mixed burden and benefit (tangled rope) — access enables research but gatekeeping constrains independence. Archives see their own degradation (piton) — preservation ritual persists through regulation while actual function has atrophied. Distributed memory actors see a solvable problem (scaffold) — alternative documentation pathways are emerging and will bypass institutional gatekeeping. The analytical observer risks naturalizing this as inherent (mountain) — 'organizations must forget' — but the structural data reveals contingent institutional arrangements (discretionary gatekeeping, resource choices, access policies) rather than laws of nature.
 *
 * DIRECTIONALITY LOGIC:
 *   Institutional leadership operates with arbitrage exit options — they can choose what to document, what to suppress, what to make accessible. This low-exit-cost position combined with beneficiary status produces low d values and negative effective extraction from their perspective (they experience rope coordination). Future stakeholders have no exit options (trapped) and bear costs of forgetting — high d values produce high experienced extraction (snare from their perspective). Institutional historians are constrained by funding and gatekeeping dependencies but benefit from access; moderate power produces moderate d and tangled rope experience. Distributed memory actors have organized power and mobile exit options (they work outside institutional memory systems); this produces lower d and scaffold classification (temporary problem with exit paths). The archival system itself has institutional power and arbitrage options but increasingly exhibits piton characteristics as performative function decouples from real preservation.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy surfaces the core ambiguity: Is institutional memory gatekeeping a legitimate coordination mechanism (institutions cannot function under total transparency; some information must be classified for security, privacy, or operational continuity) or a primary extraction mechanism (suppression of institutional accountability, control of narrative, insulation from past failures)? The resolution is perspectival. From leadership perspective, the coordination benefits are real — some gatekeeping is necessary for institutional function. From future stakeholder perspective, the extraction is real — they inherit decisions made in suppressed context. The constraint is genuinely tangled rope: it coordinates some institutional needs AND extracts from future stakeholders simultaneously. The mandatrophy resolves by showing that both are true, and the classification depends on the observer's structural position. The false summit (mountain perspective) naturalizes this asymmetry as inevitable rather than contingent.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    intentional_vs_systemic_forgetting,
    'What proportion of institutional memory loss is deliberate suppression vs. systemic resource constraints and bit decay?',
    'Audit of archival gaps against documented historical events; interview analysis of retention decisions; forensic recovery of deleted records',
    'If primarily systemic: constraint is a resource allocation problem (lower extraction, higher coordination). If primarily deliberate: constraint is extraction mechanism (higher suppression, lower coordination benefit).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(intentional_vs_systemic_forgetting, empirical, 'Proportion of memory loss due to deliberate suppression vs. systemic factors').

omega_variable(
    transparency_cost_legitimacy,
    'Does full transparency of institutional memory increase accountability or degrade institutional legitimacy and effectiveness?',
    'Comparative analysis of institutions with mandatory full disclosure vs. discretionary disclosure; measurement of stakeholder trust and institutional stability metrics post-transparency',
    'If transparency increases legitimacy: memory gatekeeping is pure extraction (snare classification confirmed). If transparency reduces institutional function: gatekeeping has genuine coordination value (rope classification legitimate).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(transparency_cost_legitimacy, preference, 'Whether full transparency increases or decreases institutional legitimacy').

omega_variable(
    critical_mass_threshold_for_coalition,
    'At what organizational scale do distributed memory actors (whistleblowers, journalists, archivists) reach critical mass to bypass institutional gatekeeping?',
    'Scale-dependent analysis: small organizations (< 100 staff) vs mid-scale (100-5000) vs large (5000+); measurement of external documentation rate and institutional response',
    'If critical mass achieved at all scales: scaffold sunset is universal (open memory pathways emerge everywhere). If only at large scale: constraint persists in smaller institutions where coalition power is weak.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(critical_mass_threshold_for_coalition, empirical, 'Scale threshold for distributed memory coalition viability').

omega_variable(
    temporal_degradation_of_extracted_memory,
    'Does gatekept institutional memory remain intact and functional over decadal timescales, or does selective suppression compound until the memory becomes incoherent even to the institution?',
    'Longitudinal study of institutional narratives over 20+ year periods; identification of self-contradiction points where suppressed context would resolve inconsistency',
    'If memory degrades: suppression becomes self-defeating (tangled rope with diminishing benefits). If memory remains controlled: gatekeeping sustains extraction (snare or piton classification).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(temporal_degradation_of_extracted_memory, empirical, 'Whether suppressed memory degrades institutional coherence over time').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(long_term_institutional_memory, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ltim_tr_t0, long_term_institutional_memory, theater_ratio, 0, 0.4).
narrative_ontology:measurement(ltim_tr_t10, long_term_institutional_memory, theater_ratio, 10, 0.55).
narrative_ontology:measurement(ltim_tr_t20, long_term_institutional_memory, theater_ratio, 20, 0.65).

% Extraction over time
narrative_ontology:measurement(ltim_be_t0, long_term_institutional_memory, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(ltim_be_t10, long_term_institutional_memory, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(ltim_be_t20, long_term_institutional_memory, base_extractiveness, 20, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(long_term_institutional_memory, information_standard).
narrative_ontology:affects_constraint(long_term_institutional_memory, organizational_accountability).
narrative_ontology:affects_constraint(long_term_institutional_memory, institutional_learning_capacity).
narrative_ontology:affects_constraint(long_term_institutional_memory, governance_legitimacy).

% DUAL FORMULATION NOTE:
% Long-term institutional memory is downstream of specific decision-making processes but represents a distinct structural constraint on organizational learning and accountability. Related constraints include institutional accountability (which depends on memory access) and governance legitimacy (which depends on perceived transparency). This story focuses on the memory preservation and access control mechanism itself.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(long_term_institutional_memory, institutional, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
