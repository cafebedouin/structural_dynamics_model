% ============================================================================
% CONSTRAINT STORY: ietf_openness_commitment__capture_substrate_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ietf_openness_commitment__capture_substrate_reading, []).

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
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: ietf_openness_commitment__capture_substrate_reading
 *   human_readable: IETF Openness Commitment (Capture Substrate Reading)
 *   domain: technology_governance/internet_standards/institutional_economics
 *
 * SUMMARY:
 *   This constraint describes the IETF's 'openness commitment' as a substrate
 *   for capture by well-resourced actors. While formally open, the process
 *   allows large platform operators to subtly embed proprietary advantages
 *   into 'open' standards, leading to de facto gatekeeping. This reading
 *   highlights the gap between the stated ideal of open, neutral standards
 *   and the material reality of resource-driven influence. The claimed type
 *   is 'tangled_rope' because it still performs a coordination function
 *   (standards are produced) but with significant, asymmetric extraction.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ietf_openness_commitment__capture_substrate_reading, 0.65).
domain_priors:suppression_score(ietf_openness_commitment__capture_substrate_reading, 0.7).
domain_priors:theater_ratio(ietf_openness_commitment__capture_substrate_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ietf_openness_commitment__capture_substrate_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(ietf_openness_commitment__capture_substrate_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(ietf_openness_commitment__capture_substrate_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ietf_openness_commitment__capture_substrate_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(ietf_openness_commitment__capture_substrate_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ietf_openness_commitment__capture_substrate_reading, tangled_rope).
narrative_ontology:human_readable(ietf_openness_commitment__capture_substrate_reading, "IETF Openness Commitment (Capture Substrate Reading)").
narrative_ontology:topic_domain(ietf_openness_commitment__capture_substrate_reading, "technology_governance/internet_standards/institutional_economics").

domain_priors:requires_active_enforcement(ietf_openness_commitment__capture_substrate_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ietf_openness_commitment__capture_substrate_reading, 'ff83c40f-3756-46f0-8076-34c4a6bde93c').
narrative_ontology:cs_kernel_codification('ff83c40f-3756-46f0-8076-34c4a6bde93c', formalized).
narrative_ontology:cs_authority_grounding('ff83c40f-3756-46f0-8076-34c4a6bde93c', practice).
narrative_ontology:cs_interpretation_layer_present('ff83c40f-3756-46f0-8076-34c4a6bde93c').
narrative_ontology:cs_reading_relation('ff83c40f-3756-46f0-8076-34c4a6bde93c', ietf_openness_commitment__commons_stewardship_reading, coexists_with).
narrative_ontology:cs_reading_relation('ff83c40f-3756-46f0-8076-34c4a6bde93c', ietf_openness_commitment__legitimacy_erosion_reading, coexists_with).
narrative_ontology:cs_axiom('ff83c40f-3756-46f0-8076-34c4a6bde93c', foundational, openness_as_practical_interoperability).
narrative_ontology:cs_axiom_status(openness_as_practical_interoperability, holdable).
narrative_ontology:cs_axiom_grounding('ff83c40f-3756-46f0-8076-34c4a6bde93c', openness_as_practical_interoperability, empirically_contingent).
narrative_ontology:cs_axiom('ff83c40f-3756-46f0-8076-34c4a6bde93c', foundational, resource_advantage_translates_to_technical_influence).
narrative_ontology:cs_axiom_status(resource_advantage_translates_to_technical_influence, holdable).
narrative_ontology:cs_axiom_grounding('ff83c40f-3756-46f0-8076-34c4a6bde93c', resource_advantage_translates_to_technical_influence, empirically_contingent).
narrative_ontology:cs_reference_frame('ff83c40f-3756-46f0-8076-34c4a6bde93c', ideal_open_standards_process).
narrative_ontology:cs_drift_state('ff83c40f-3756-46f0-8076-34c4a6bde93c', contemporary_platform_economy_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('ff83c40f-3756-46f0-8076-34c4a6bde93c', '').
narrative_ontology:cs_kernel_id(ietf_openness_commitment__capture_substrate_reading, ietf_openness_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ietf_openness_commitment__capture_substrate_reading, large_platform_operators).
narrative_ontology:constraint_victim(ietf_openness_commitment__capture_substrate_reading, small_implementers).
narrative_ontology:constraint_victim(ietf_openness_commitment__capture_substrate_reading, end_users).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The collective body of engineers and experts who draft and ratify Internet standards. They operate under a 'rough consensus and running code' philosophy, aiming for open, interoperable standards. However, resource disparities mean larger entities can disproportionately influence outcomes.
narrative_ontology:constraint_stakeholder(ietf_openness_commitment__capture_substrate_reading, ietf_participants, agenda_setter,
    organized, generational, constrained, global).

% Major technology companies with significant resources (engineers, legal teams) who participate in IETF. They benefit by shaping standards to align with their proprietary interests, often introducing complexities that favor their existing infrastructure or market position, effectively using the open process as a substrate for gatekeeping.
narrative_ontology:constraint_stakeholder(ietf_openness_commitment__capture_substrate_reading, large_platform_operators, beneficiary,
    institutional, generational, arbitrage, global).

% Smaller companies or individual developers who must implement IETF standards to ensure interoperability. They bear the cost of navigating complex standards, which are sometimes subtly biased towards the large operators' existing proprietary systems, making true open implementation difficult or expensive.
narrative_ontology:constraint_stakeholder(ietf_openness_commitment__capture_substrate_reading, small_implementers, payer,
    moderate, biographical, constrained, global).

% Rely on interoperable internet services. They are indirectly harmed when standards become de facto proprietary, leading to vendor lock-in, reduced competition, and less innovation, without a clear understanding of the underlying technical capture.
narrative_ontology:constraint_stakeholder(ietf_openness_commitment__capture_substrate_reading, end_users, payer,
    powerless, biographical, trapped, global).

% Government bodies tasked with ensuring fair competition. They observe the standards process and its outcomes, but often lack the technical expertise or mandate to intervene directly in the drafting of technical specifications, making their influence reactive rather than proactive.
narrative_ontology:constraint_stakeholder(ietf_openness_commitment__capture_substrate_reading, competition_regulators, observer,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a neutral forum for diverse stakeholders to collaboratively develop technical standards that ensure global internet interoperability and functionality.
% TRANSFER_FUNCTION: Transfers influence over future internet architecture and market advantage from smaller, less resourced implementers to larger, well-resourced platform operators, disguised as technical consensus.
% ABSENT_VOICES: Many independent researchers, civil society groups, and smaller regional implementers lack the resources to consistently participate in the IETF's lengthy and technically demanding process, leaving their perspectives underrepresented or unheard.
% DISAPPEARANCE_RATIONALE: If the IETF's commitment to openness vanished, the internet would rapidly fragment into proprietary silos controlled by dominant platform operators, leading to a collapse of global interoperability and a significant rearrangement of the digital economy.
% FOUNDING_PROBLEM: The need for a collaborative, open process to develop technical standards for the internet to ensure global interoperability and prevent fragmentation into proprietary networks.
% FOUNDING_PROBLEM_CORROBORATION: IETF leadership and many participants maintain the problem is live and the process is largely successful. Critics (e.g., academic researchers, some smaller implementers) argue that while the formal problem is addressed, the process has been subtly captured, making the 'openness' commitment a cover for new forms of gatekeeping. Independent analyses of standard complexity and implementation costs support the latter view.
narrative_ontology:disappearance_verdict(ietf_openness_commitment__capture_substrate_reading, world_rearranges).
narrative_ontology:founding_problem_status(ietf_openness_commitment__capture_substrate_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ietf_openness_commitment__capture_substrate_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(ietf_openness_commitment__capture_substrate_reading, 'none', 1).
narrative_ontology:epsilon_provenance(ietf_openness_commitment__capture_substrate_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ietf_openness_commitment__capture_substrate_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(ietf_openness_commitment__capture_substrate_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(ietf_openness_commitment__capture_substrate_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.65) is moderate because the process still yields functional standards, but the costs of implementation are disproportionately borne by smaller entities. Suppression (0.70) is high because the technical complexity and resource requirements effectively suppress participation and alternative proposals from less-resourced actors. Theater ratio (0.40) reflects that while the 'open process' is genuinely maintained, a significant portion of its activity serves to legitimize outcomes that favor dominant players, rather than purely serving universal interoperability. The metrics show a gradual increase in extractiveness and suppression over time, indicating a drift towards greater capture.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of large platform operators, the IETF process is a successful example of industry-led coordination. From the perspective of small implementers and end-users, it's a system where resource advantage translates into encoded gatekeeping, making the 'openness' commitment a form of extraction. The engine's per-seat classification will reflect this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Large platform operators are the primary beneficiaries (d near 0.0) as they shape standards to their advantage. Small implementers and end-users are the victims (d near 1.0) as they bear the costs of complex, subtly biased standards. IETF participants, as the agenda-setters, are caught in the middle, attempting to maintain openness while navigating powerful interests. Competition regulators are observers, with an analytical perspective.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's original mandate was to ensure open, interoperable internet standards. This reading suggests that while the formal mandate persists, its function has drifted to include legitimizing proprietary advantage. The classification as a Tangled Rope prevents mislabeling it as a pure Rope (ignoring extraction) or a Snare (ignoring the genuine coordination function that still exists).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    resource_influence_quantification,
    'To what extent do resource disparities (e.g., number of engineers, legal budget) directly translate into influence over IETF standard specifications and outcomes?',
    'Empirical study correlating participant resource levels with successful proposal adoption, complexity of adopted standards, and alignment with proprietary interests.',
    'Strong correlation would further support the ''capture substrate'' reading, potentially leading to calls for procedural changes to level the playing field or regulatory intervention. Weak correlation would suggest the process is more resilient to resource advantage than this reading assumes.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(resource_influence_quantification, empirical, 'Quantifying the impact of resource advantage on standards outcomes.').

omega_variable(
    openness_definition_ambiguity,
    'Is ''openness'' in internet standards primarily about formal access to the process, or about the practical ease and cost of implementation for all parties?',
    'Conceptual analysis and stakeholder surveys to clarify the normative definition of ''openness'' among different participant groups, followed by an assessment of whether the IETF process meets that definition.',
    'If ''openness'' is primarily about practical implementation, the current IETF process, as described in this reading, would be seen as failing its core commitment. If it''s only about formal access, the ''capture substrate'' reading would be less critical of the process itself, shifting focus to external market dynamics.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(openness_definition_ambiguity, conceptual, 'Ambiguity in the definition of ''openness'' in internet standards.').

omega_variable(
    kernel_reading_difference,
    'What specific structural elements would change if the ''commons_stewardship_reading'' of the IETF openness commitment were adopted instead of this ''capture_substrate_reading''?',
    'Comparative analysis of proposed procedural reforms and their impact on stakeholder power, exit options, and resource allocation within the IETF process.',
    'The ''commons_stewardship_reading'' would likely lead to lower extractiveness and suppression, with a stronger emphasis on universal interoperability and reduced barriers for small implementers, potentially reclassifying the constraint closer to a Rope.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_difference, conceptual, 'Structural differences between the ''capture_substrate_reading'' and ''commons_stewardship_reading'' of the IETF openness commitment.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ietf_openness_commitment__capture_substrate_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ietf_tr_t0, ietf_openness_commitment__capture_substrate_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(ietf_tr_t5, ietf_openness_commitment__capture_substrate_reading, theater_ratio, 5, 0.25).
narrative_ontology:measurement(ietf_tr_t10, ietf_openness_commitment__capture_substrate_reading, theater_ratio, 10, 0.3).
narrative_ontology:measurement(ietf_tr_t15, ietf_openness_commitment__capture_substrate_reading, theater_ratio, 15, 0.35).
narrative_ontology:measurement(ietf_tr_t20, ietf_openness_commitment__capture_substrate_reading, theater_ratio, 20, 0.38).
narrative_ontology:measurement(ietf_tr_t25, ietf_openness_commitment__capture_substrate_reading, theater_ratio, 25, 0.4).

% Extraction over time
narrative_ontology:measurement(ietf_be_t0, ietf_openness_commitment__capture_substrate_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(ietf_be_t5, ietf_openness_commitment__capture_substrate_reading, base_extractiveness, 5, 0.5).
narrative_ontology:measurement(ietf_be_t10, ietf_openness_commitment__capture_substrate_reading, base_extractiveness, 10, 0.55).
narrative_ontology:measurement(ietf_be_t15, ietf_openness_commitment__capture_substrate_reading, base_extractiveness, 15, 0.6).
narrative_ontology:measurement(ietf_be_t20, ietf_openness_commitment__capture_substrate_reading, base_extractiveness, 20, 0.63).
narrative_ontology:measurement(ietf_be_t25, ietf_openness_commitment__capture_substrate_reading, base_extractiveness, 25, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(ietf_su_t0, ietf_openness_commitment__capture_substrate_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(ietf_su_t5, ietf_openness_commitment__capture_substrate_reading, suppression_requirement, 5, 0.55).
narrative_ontology:measurement(ietf_su_t10, ietf_openness_commitment__capture_substrate_reading, suppression_requirement, 10, 0.6).
narrative_ontology:measurement(ietf_su_t15, ietf_openness_commitment__capture_substrate_reading, suppression_requirement, 15, 0.65).
narrative_ontology:measurement(ietf_su_t20, ietf_openness_commitment__capture_substrate_reading, suppression_requirement, 20, 0.68).
narrative_ontology:measurement(ietf_su_t25, ietf_openness_commitment__capture_substrate_reading, suppression_requirement, 25, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ietf_openness_commitment__capture_substrate_reading, information_standard).
narrative_ontology:affects_constraint(ietf_openness_commitment__capture_substrate_reading, ietf_openness_commitment__commons_stewardship_reading).
narrative_ontology:affects_constraint(ietf_openness_commitment__capture_substrate_reading, ietf_openness_commitment__legitimacy_erosion_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'IETF openness commitment' kernel. This 'capture_substrate_reading' focuses on how resource advantage translates to encoded gatekeeping, contrasting with the 'commons_stewardship_reading' (open infrastructure) and 'legitimacy_erosion_reading' (contested rough consensus mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
