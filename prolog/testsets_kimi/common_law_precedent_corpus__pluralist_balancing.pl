% ============================================================================
% CONSTRAINT STORY: common_law_precedent_corpus__pluralist_balancing
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2025-01-21
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_common_law_precedent_corpus__pluralist_balancing, []).

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
    narrative_ontology:suppression_profile/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:fixing_cost_class/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_axiom_grounding/3,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:cs_created_at/2,
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: common_law_precedent_corpus__pluralist_balancing
 *   human_readable: Common Law Precedent Corpus â Pluralist Balancing Reading
 *   domain: legal/jurisprudential
 *
 * SUMMARY:
 *   This constraint instantiates the pluralist_balancing reading of the
 *   common_law_precedent_corpus kernel. Under this reading, precedent does
 *   not bind uniformly; instead, its weight varies by domain (constitutional,
 *   statutory, common law), court level, and factual context. Courts balance
 *   stability and adaptation case-by-case. The constraint generates genuine
 *   coordination for institutional actors and repeat players who can navigate
 *   precedent hierarchies, while imposing asymmetric costs on one-shot
 *   litigants who face unpredictable domain-switching expenses. It is
 *   authored as a tangled_rope: a live coordination function (intertemporal
 *   legal stability) coexists with asymmetric extraction (interpretive
 *   advantage captured by repeat players).
 *
 * KEY AGENTS:
 *   - Judiciary (institutional/analytical): sets precedent weight and administers domain distinctions.
 *   - Repeat players (powerful/mobile): corporations and agencies that arbitrage precedent hierarchies.
 *   - One-shot litigants (powerless/constrained): individuals bearing unpredictable litigation costs.
 *   - Legal academy (institutional/analytical): observes and rationalizes the balancing framework.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(common_law_precedent_corpus__pluralist_balancing, 0.62).
domain_priors:suppression_score(common_law_precedent_corpus__pluralist_balancing, 0.55).
domain_priors:theater_ratio(common_law_precedent_corpus__pluralist_balancing, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(common_law_precedent_corpus__pluralist_balancing, extractiveness, 0.62).
narrative_ontology:constraint_metric(common_law_precedent_corpus__pluralist_balancing, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(common_law_precedent_corpus__pluralist_balancing, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(common_law_precedent_corpus__pluralist_balancing, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(common_law_precedent_corpus__pluralist_balancing, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(common_law_precedent_corpus__pluralist_balancing, tangled_rope).
narrative_ontology:human_readable(common_law_precedent_corpus__pluralist_balancing, "Common Law Precedent Corpus â Pluralist Balancing Reading").
narrative_ontology:topic_domain(common_law_precedent_corpus__pluralist_balancing, "legal/jurisprudential").

domain_priors:requires_active_enforcement(common_law_precedent_corpus__pluralist_balancing).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(common_law_precedent_corpus__pluralist_balancing, 'adcb3e2d-3dbb-42fd-a92e-141849330ea5').
narrative_ontology:cs_kernel_codification('adcb3e2d-3dbb-42fd-a92e-141849330ea5', fixed_text).
narrative_ontology:cs_authority_grounding('adcb3e2d-3dbb-42fd-a92e-141849330ea5', lineage).
narrative_ontology:cs_interpretation_layer_present('adcb3e2d-3dbb-42fd-a92e-141849330ea5').
narrative_ontology:cs_reading_relation('adcb3e2d-3dbb-42fd-a92e-141849330ea5', common_law_precedent_corpus__strict_stare_decisis, influences).
narrative_ontology:cs_reading_relation('adcb3e2d-3dbb-42fd-a92e-141849330ea5', common_law_precedent_corpus__evolutionary_framework, coexists_with).
narrative_ontology:cs_axiom('adcb3e2d-3dbb-42fd-a92e-141849330ea5', foundational, precedent_weight_contextually_determined).
narrative_ontology:cs_axiom_status(precedent_weight_contextually_determined, holdable).
narrative_ontology:cs_axiom_grounding('adcb3e2d-3dbb-42fd-a92e-141849330ea5', precedent_weight_contextually_determined, conventional).
narrative_ontology:cs_axiom('adcb3e2d-3dbb-42fd-a92e-141849330ea5', foundational, stability_adaptation_balance_case_specific).
narrative_ontology:cs_axiom_status(stability_adaptation_balance_case_specific, holdable).
narrative_ontology:cs_axiom_grounding('adcb3e2d-3dbb-42fd-a92e-141849330ea5', stability_adaptation_balance_case_specific, instrumental).
narrative_ontology:cs_reference_frame('adcb3e2d-3dbb-42fd-a92e-141849330ea5', common_law_pragmatic_equilibrium).
narrative_ontology:cs_drift_state('adcb3e2d-3dbb-42fd-a92e-141849330ea5', contemporary_jurisprudence, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('adcb3e2d-3dbb-42fd-a92e-141849330ea5', '').
narrative_ontology:cs_kernel_id(common_law_precedent_corpus__pluralist_balancing, common_law_precedent_corpus).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(common_law_precedent_corpus__pluralist_balancing, judiciary).
narrative_ontology:constraint_beneficiary(common_law_precedent_corpus__pluralist_balancing, repeat_players).
narrative_ontology:constraint_victim(common_law_precedent_corpus__pluralist_balancing, one_shot_litigants).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers the precedent corpus by assigning differential weight to prior decisions based on domain, court level, and factual similarity. Retains interpretive discretion to balance stability against adaptation in each case, shaping which precedents bind and which yield.
narrative_ontology:constraint_stakeholder(common_law_precedent_corpus__pluralist_balancing, judiciary, agenda_setter,
    institutional, civilizational, analytical, global).

% Corporations, government agencies, and institutional litigants that appear frequently before courts. They maintain specialized legal teams capable of mapping domain-specific precedent hierarchies and predicting where balancing will favor stability versus adaptation, allowing them to forum-shop and strategize across jurisdictional tiers.
narrative_ontology:constraint_stakeholder(common_law_precedent_corpus__pluralist_balancing, repeat_players, beneficiary,
    powerful, biographical, mobile, global).

% Individuals and small entities involved in episodic litigation. They face unpredictable legal costs because the applicable precedent weight in their domain cannot be reliably estimated ex ante, and they lack resources to navigate cross-domain distinctions or sustained appellate engagement.
narrative_ontology:constraint_stakeholder(common_law_precedent_corpus__pluralist_balancing, one_shot_litigants, payer,
    powerless, immediate, constrained, national).

% Produces doctrinal scholarship that rationalizes and critiques the pluralist balancing framework. Sits outside direct extraction but shapes the vocabulary through which courts justify variable precedent weight and domain boundaries.
narrative_ontology:constraint_stakeholder(common_law_precedent_corpus__pluralist_balancing, legal_academy, observer,
    institutional, civilizational, analytical, global).

narrative_ontology:fixing_cost_class(common_law_precedent_corpus__pluralist_balancing, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates legal expectations across time and jurisdiction by providing a stable but adaptable rule-set; allows courts to build on prior reasoning without legislating anew for every dispute, solving the intertemporal legitimacy problem of common law governance.
% TRANSFER_FUNCTION: Transfers interpretive authority and litigation cost predictability from one-shot litigants to repeat players and the judiciary; moves resources, strategic positioning, and agenda control toward actors who can navigate domain-specific precedent hierarchies.
% ABSENT_VOICES: Legislative drafters and popular majorities who might prefer statutory clarity over common law incrementalism; lay litigants without counsel who cannot afford domain-switching cost analysis; civil law jurisdictions offering codified alternatives that are structurally excluded from the common law conversation.
% DISAPPEARANCE_RATIONALE: If the pluralist balancing constraint vanished, common law adjudication would lose its primary stabilizing mechanism; litigation strategies, judicial opinion-writing, legal education, and the legal services market would reorganize around either strict adherence or purely forward-looking policy analysis. The current distribution of interpretive advantage and judicial discretion would collapse.
% FOUNDING_PROBLEM: How to maintain legal continuity and predictability across changing social conditions without requiring a single legislature to continuously update every rule.
% FOUNDING_PROBLEM_CORROBORATION: Comparative legal historians and political scientists outside the judiciary attest that common law systems face persistent legitimation challenges regarding retrospective versus prospective rule-making; no external corroborator disputes that the problem of stability versus change is live, though they contest whether precedent-based balancing solves it.
narrative_ontology:disappearance_verdict(common_law_precedent_corpus__pluralist_balancing, world_rearranges).
narrative_ontology:founding_problem_status(common_law_precedent_corpus__pluralist_balancing, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(common_law_precedent_corpus__pluralist_balancing, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(common_law_precedent_corpus__pluralist_balancing, 'none', 1).
narrative_ontology:epsilon_provenance(common_law_precedent_corpus__pluralist_balancing, 0.62, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(common_law_precedent_corpus__pluralist_balancing_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(common_law_precedent_corpus__pluralist_balancing, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(common_law_precedent_corpus__pluralist_balancing_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62) reflects multi-tier extraction: the same precedent corpus is rigid in some domains and flexible in others, allowing systematic advantage to actors who can predict which tier applies. Suppression (0.55) is moderate: alternatives exist (statutory override, civil law codification) but are institutionally costly. Theater_ratio (0.40) captures the performative dimension of judicial opinions that ritually cite precedents before distinguishing them on contextual grounds. Accessibility_collapse (0.45) is moderate â understanding the system does not fully collapse alternatives, but the cost of utilizing them is high. Resistance (0.50) reflects ongoing contestation from formalist jurists and litigants disadvantaged by unpredictability.
 *
 * PERSPECTIVAL GAP:
 *   From the judicial seat, the constraint is necessary coordination â without case-by-case balancing, common law would ossify or usurp legislative function. From the one-shot litigant seat, the same structure appears as capricious extraction of time and money, because the precedential weight of their issue cannot be estimated before entry. The repeat_player seat mediates between the two: they experience the coordination benefit precisely because they have internalized the extraction mechanism.
 *
 * DIRECTIONALITY LOGIC:
 *   The judiciary is a structural beneficiary (low d): the constraint subsidizes their discretionary authority and institutional legitimacy. Repeat_players are beneficiaries (low-to-moderate d): their mobile exit options and repeated exposure allow them to arbitrage domain variance. One_shot_litigants are targets (high d): constrained exit and immediate time horizon amplify the effective extraction of unpredictable switching costs.
 *
 * MANDATROPHY ANALYSIS:
 *   The pluralist balancing reading resists mandatrophy mislabeling because its founding problem â maintaining legal stability across social change â remains live. It is not a piton because the coordination function is not atrophied; it is not a snare because the coordination is genuine and not merely cover. However, the absence of a sunset clause and the presence of theater indicate that extraction has accumulated on the coordination base over time, which is the signature of tangled_rope.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    domain_boundary_indeterminacy,
    'Are the domain boundaries (constitutional, statutory, common law) that determine precedent weight in the pluralist reading objectively stable, or are they constructed post-hoc to justify preferred outcomes?',
    'Empirical analysis of judicial behavior: if domain assignments predict political preferences better than doctrinal criteria, boundaries are constructed.',
    'If constructed, the pluralist reading functions as a snare-like extraction mechanism disguised by coordination rhetoric; if stable, it remains a genuine tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(domain_boundary_indeterminacy, empirical, 'Whether domain boundaries are objective or constructed.').

omega_variable(
    pluralist_evolutionary_distinction,
    'Does the pluralist_balancing reading remain structurally distinct from the evolutionary_framework reading, or does case-by-case balancing collapse into normative evolution when pressed?',
    'Comparative doctrinal analysis tracing whether pluralist courts invoke distinct methodological constraints (e.g., incrementalism, Burkean deference) or simply reinterpret precedent to match contemporary values.',
    'If collapse occurs, the pluralist reading is not a stable middle position but a transitional rhetoric on the path to evolutionary framework; this would alter its classification toward lower coordination purity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(pluralist_evolutionary_distinction, conceptual, 'Structural boundary between pluralist balancing and evolutionary framework readings.').

omega_variable(
    switching_cost_intentionality,
    'Are litigant domain-switching costs an inevitable friction of case-by-case balancing, or are they strategically exploited by repeat players to disadvantage one-shot opponents?',
    'Litigation cost data and settlement patterns comparing repeat versus one-shot players across domains with high versus low precedent variance.',
    'If strategic exploitation dominates, the extraction is more concentrated and snare-like; if inevitable friction, it is a distributed cost of coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(switching_cost_intentionality, empirical, 'Whether switching costs are exploited or inevitable.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(common_law_precedent_corpus__pluralist_balancing, 0, 70).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comm_tr_t0, common_law_precedent_corpus__pluralist_balancing, theater_ratio, 0, 0.2).
narrative_ontology:measurement(comm_tr_t14, common_law_precedent_corpus__pluralist_balancing, theater_ratio, 14, 0.24).
narrative_ontology:measurement(comm_tr_t28, common_law_precedent_corpus__pluralist_balancing, theater_ratio, 28, 0.29).
narrative_ontology:measurement(comm_tr_t42, common_law_precedent_corpus__pluralist_balancing, theater_ratio, 42, 0.33).
narrative_ontology:measurement(comm_tr_t56, common_law_precedent_corpus__pluralist_balancing, theater_ratio, 56, 0.37).
narrative_ontology:measurement(comm_tr_t70, common_law_precedent_corpus__pluralist_balancing, theater_ratio, 70, 0.4).

% Extraction over time
narrative_ontology:measurement(comm_be_t0, common_law_precedent_corpus__pluralist_balancing, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(comm_be_t14, common_law_precedent_corpus__pluralist_balancing, base_extractiveness, 14, 0.41).
narrative_ontology:measurement(comm_be_t28, common_law_precedent_corpus__pluralist_balancing, base_extractiveness, 28, 0.47).
narrative_ontology:measurement(comm_be_t42, common_law_precedent_corpus__pluralist_balancing, base_extractiveness, 42, 0.54).
narrative_ontology:measurement(comm_be_t56, common_law_precedent_corpus__pluralist_balancing, base_extractiveness, 56, 0.59).
narrative_ontology:measurement(comm_be_t70, common_law_precedent_corpus__pluralist_balancing, base_extractiveness, 70, 0.62).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(common_law_precedent_corpus__pluralist_balancing, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(common_law_precedent_corpus__pluralist_balancing, enforcement_mechanism).
narrative_ontology:affects_constraint(common_law_precedent_corpus__pluralist_balancing, common_law_precedent_corpus__strict_stare_decisis).
narrative_ontology:affects_constraint(common_law_precedent_corpus__pluralist_balancing, common_law_precedent_corpus__evolutionary_framework).

% DUAL FORMULATION NOTE:
% The common_law_precedent_corpus kernel decomposes into three structurally distinct readings: strict_stare_decisis (high rigidity, low judicial discretion), pluralist_balancing (context-dependent variance, medium extraction), and evolutionary_framework (normative adaptation, higher forward-looking flexibility). Each reading has a different epsilon, stakeholder structure, and directionality profile. This story links to both siblings as part of the constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
