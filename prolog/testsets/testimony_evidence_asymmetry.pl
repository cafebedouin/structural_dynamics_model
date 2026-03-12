% ============================================================================
% CONSTRAINT STORY: testimony_evidence_asymmetry
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2025-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_testimony_evidence_asymmetry, []).

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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: testimony_evidence_asymmetry
 *   human_readable: Testimony Evidence Asymmetry in Epistemic Weight Assignment
 *   domain: epistemology/philosophy_of_evidence/cognitive_science
 *
 * SUMMARY:
 *   The testimony evidence asymmetry is the epistemic principle that single
 *   testimony carries structurally lower evidential weight than
 *   cross-verified patterns from multiple independent sources. This
 *   constraint operates across judicial contexts (corroboration
 *   requirements), historical research (source triangulation), scientific
 *   practice (replication standards), and individual reasoning (credence
 *   assignment heuristics). The asymmetry solves a fundamental coordination
 *   problem: how to distinguish reliable knowledge transmission from error,
 *   bias, fabrication, or misremembering in contexts where direct observation
 *   is unavailable. The constraint is a pure coordination mechanism (Rope)
 *   from all perspectives — no agent experiences significant extraction, and
 *   all benefit from the epistemic reliability the asymmetry enables. The low
 *   extractiveness (0.18) reflects minimal overhead costs: some legitimate
 *   single-source testimony is discounted, and verification processes require
 *   resources, but these costs are small relative to the coordination
 *   benefit. The constraint is downstream of mediated_knowledge_dependency
 *   (the mountain-level fact that most knowledge comes through testimony
 *   rather than direct observation) but is itself a contingent coordination
 *   solution rather than an immutable epistemic law.
 *
 * KEY AGENTS:
 *   - Epistemic Community: Primary beneficiary (analytical/analytical) — gains reliable knowledge transmission through differential weight assignment
 *   - Judicial System: Institutional beneficiary (institutional/mobile) — applies corroboration standards as truth-finding coordination
 *   - Historical Research Community: Institutional beneficiary (institutional/mobile) — uses source criticism and triangulation for knowledge reconstruction
 *   - Scientific Peer Review System: Organized beneficiary (organized/mobile) — requires replication as validation coordination
 *   - Individual Epistemic Agent: Moderate beneficiary (moderate/mobile) — applies differential credence as belief formation heuristic
 *   - Investigative Journalist: Powerful beneficiary (powerful/arbitrage) — uses verification standards as professional methodology
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(testimony_evidence_asymmetry, 0.18).
domain_priors:suppression_score(testimony_evidence_asymmetry, 0.12).
domain_priors:theater_ratio(testimony_evidence_asymmetry, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(testimony_evidence_asymmetry, extractiveness, 0.18).
narrative_ontology:constraint_metric(testimony_evidence_asymmetry, suppression_requirement, 0.12).
narrative_ontology:constraint_metric(testimony_evidence_asymmetry, theater_ratio, 0.25).

% --- Constraint claim ---
narrative_ontology:constraint_claim(testimony_evidence_asymmetry, rope).
narrative_ontology:human_readable(testimony_evidence_asymmetry, "Testimony Evidence Asymmetry in Epistemic Weight Assignment").
narrative_ontology:topic_domain(testimony_evidence_asymmetry, "epistemology/philosophy_of_evidence/cognitive_science").

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(testimony_evidence_asymmetry, epistemic_community).
narrative_ontology:constraint_beneficiary(testimony_evidence_asymmetry, cross_verification_systems).
narrative_ontology:constraint_beneficiary(testimony_evidence_asymmetry, aggregated_testimony_sources).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ANALYTICAL OBSERVER (ROPE) — The asymmetry between single testimony and cross-verified patterns is a coordination mechanism for epistemic reliability. Assigning lower weight to uncorroborated testimony solves the collective action problem of distinguishing signal from noise in knowledge transmission. Minimal extraction — the constraint serves genuine epistemic function.
constraint_indexing:constraint_classification(testimony_evidence_asymmetry, rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 2: JUDICIAL SYSTEM (ROPE) — Courts apply differential epistemic weight (corroboration requirements, witness credibility assessment, physical evidence prioritization) as a coordination mechanism for truth-finding. The asymmetry enables systematic evaluation of competing claims. Benefits from the coordination function; experiences minimal extraction.
constraint_indexing:constraint_classification(testimony_evidence_asymmetry, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 3: HISTORICAL RESEARCH COMMUNITY (ROPE) — Historians apply source criticism and triangulation as coordination mechanisms for reconstructing past events. Single memoirs are treated as data points requiring corroboration; cross-verified patterns from multiple independent sources carry higher epistemic weight. The asymmetry is a methodological standard that enables collective knowledge production.
constraint_indexing:constraint_classification(testimony_evidence_asymmetry, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 4: SCIENTIFIC PEER REVIEW (ROPE) — Scientific communities require replication and independent verification as coordination mechanisms for knowledge validation. Single experimental reports carry provisional weight; cross-verified results from multiple labs establish consensus. The asymmetry solves the coordination problem of distinguishing robust findings from artifacts.
constraint_indexing:constraint_classification(testimony_evidence_asymmetry, rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: INDIVIDUAL EPISTEMIC AGENT (ROPE) — Individual reasoners apply differential credence to single vs cross-verified testimony as a heuristic for belief formation. The asymmetry coordinates personal epistemic practice with community standards. Minimal experienced extraction — the constraint helps rather than hinders individual knowledge acquisition.
constraint_indexing:constraint_classification(testimony_evidence_asymmetry, rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(local))).

% PERSPECTIVE 6: INVESTIGATIVE JOURNALIST (ROPE) — Journalists apply source verification standards (multiple independent sources, documentary corroboration, on-the-record attribution) as professional coordination mechanisms. The asymmetry between single and cross-verified testimony structures investigative methodology. Benefits from the coordination function through enhanced credibility.
constraint_indexing:constraint_classification(testimony_evidence_asymmetry, rope,
    context(agent_power(powerful),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(regional))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(testimony_evidence_asymmetry_tests).
:- end_tests(testimony_evidence_asymmetry_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.18): Low. The constraint imposes minimal costs: (1) some legitimate single-source testimony is discounted pending corroboration, creating temporary epistemic gaps; (2) verification processes require time and resources; (3) individuals with unique knowledge may face credibility barriers. However, these costs are small relative to the coordination benefit — the asymmetry prevents far more epistemic harm (false beliefs from uncorroborated claims) than it causes (delayed acceptance of true claims). The value reflects genuine coordination overhead rather than extractive rent-seeking. Suppression (0.12): Very low. Agents can exit the constraint by seeking corroboration, providing additional evidence, or accepting provisional lower credence. No coercive enforcement mechanism prevents alternative epistemic practices. The constraint operates through voluntary adoption of methodological standards. Theater ratio (0.25): Low. The verification processes (corroboration checks, source triangulation, replication studies) are functional rather than performative. Some theater exists in formal contexts (courtroom testimony rituals, peer review ceremonies) but the core asymmetry is operationally meaningful — cross-verified claims genuinely do have higher reliability than single testimony.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits minimal perspectival gap — all six perspectives classify as Rope. The uniformity reflects the constraint's structural nature as a pure coordination mechanism with no significant extraction component. The analytical observer sees an epistemic coordination standard. Institutional actors (judicial, historical, scientific) see methodological frameworks that enable their core functions. Individual agents see practical heuristics. All perspectives converge on the same classification because the constraint genuinely solves a coordination problem without creating asymmetric costs. The absence of Snare or Tangled Rope perspectives is diagnostically significant — it indicates that the asymmetry is not being weaponized for extractive purposes (e.g., systematically discrediting marginalized testimony while accepting privileged testimony at face value). If such extraction patterns existed, they would appear as separate constraints (e.g., credibility_discount_bias) rather than as features of the base asymmetry itself.
 *
 * DIRECTIONALITY LOGIC:
 *   All perspectives are beneficiaries of the coordination function. The epistemic community gains reliable knowledge transmission. Judicial systems gain truth-finding mechanisms. Historical researchers gain methodological standards for source evaluation. Scientific communities gain validation protocols. Individual reasoners gain belief formation heuristics. Investigative journalists gain professional credibility through verification standards. No agent group is structurally positioned as a victim — the asymmetry does not extract from testimony-givers in favor of testimony-receivers, because both roles are occupied by all agents at different times. The constraint coordinates the entire epistemic ecosystem rather than redistributing epistemic authority. All agents have mobile or arbitrage exit options — they can provide corroboration, seek alternative verification, or accept provisional credence assignments. The low directionality values across all perspectives (all agents are beneficiaries with exit options) produce uniformly low effective extraction, confirming the Rope classification.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint resolves mandatrophy by demonstrating that not all epistemic asymmetries are extractive. The testimony evidence asymmetry could superficially appear to extract from testimony-givers (whose claims carry lower weight) in favor of testimony-receivers (who demand corroboration). However, the structural analysis reveals that both roles are occupied by all agents across contexts — everyone both gives and receives testimony. The asymmetry coordinates the entire system rather than redistributing epistemic authority from one group to another. The low extractiveness and suppression values, combined with the absence of victim groups and the presence of multiple beneficiary groups, confirm that this is genuine coordination rather than disguised extraction. The constraint is a Rope, not a Snare wearing Rope's clothing.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(testimony_evidence_asymmetry, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(testimony_evidence_asymmetry, information_standard).

% DUAL FORMULATION NOTE:
% The testimony evidence asymmetry is downstream of mediated_knowledge_dependency (the mountain-level constraint that most knowledge comes through testimony rather than direct observation). The upstream constraint establishes that testimony is unavoidable; the downstream constraint establishes how to evaluate testimony reliability. The asymmetry is a coordination solution to the problem posed by the mountain, not a feature of the mountain itself.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
