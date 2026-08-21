% ============================================================================
% CONSTRAINT STORY: qwerty_persistence_inevitability__path_dependency_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_qwerty_persistence_inevitability__path_dependency_reading, []).

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
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    domain_priors:emerges_naturally/1,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
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
 *   constraint_id: qwerty_persistence_inevitability__path_dependency_reading
 *   human_readable: QWERTY Persistence (Path Dependency Reading)
 *   domain: technology_history/political_economy/institutional_analysis
 *
 * SUMMARY:
 *   This constraint describes the persistence of the QWERTY keyboard layout
 *   as a result of accident-driven path dependency, where initial conditions
 *   (mechanical typewriter limitations) led to an entrenched standard that is
 *   now difficult to dislodge due to high switching costs and network
 *   effects, rather than strategic enforcement by any single actor. This
 *   reading posits QWERTY as a technological inevitability given its history,
 *   classifying it as a Mountain. It explicitly denies the existence of
 *   strategic beneficiaries or victims, viewing efficiency loss as a diffuse
 *   externality.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(qwerty_persistence_inevitability__path_dependency_reading, 0.05).
domain_priors:suppression_score(qwerty_persistence_inevitability__path_dependency_reading, 0.95).
domain_priors:theater_ratio(qwerty_persistence_inevitability__path_dependency_reading, 0.0).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(qwerty_persistence_inevitability__path_dependency_reading, extractiveness, 0.05).
narrative_ontology:constraint_metric(qwerty_persistence_inevitability__path_dependency_reading, suppression_requirement, 0.95).
narrative_ontology:constraint_metric(qwerty_persistence_inevitability__path_dependency_reading, theater_ratio, 0.0).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(qwerty_persistence_inevitability__path_dependency_reading, accessibility_collapse, 0.9).
narrative_ontology:constraint_metric(qwerty_persistence_inevitability__path_dependency_reading, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(qwerty_persistence_inevitability__path_dependency_reading, mountain).
narrative_ontology:human_readable(qwerty_persistence_inevitability__path_dependency_reading, "QWERTY Persistence (Path Dependency Reading)").
narrative_ontology:topic_domain(qwerty_persistence_inevitability__path_dependency_reading, "technology_history/political_economy/institutional_analysis").

domain_priors:emerges_naturally(qwerty_persistence_inevitability__path_dependency_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(qwerty_persistence_inevitability__path_dependency_reading, '9bc8e1dd-c775-4496-b4dc-d3f382ec1159').
narrative_ontology:cs_kernel_codification('9bc8e1dd-c775-4496-b4dc-d3f382ec1159', implicit).
narrative_ontology:cs_authority_grounding('9bc8e1dd-c775-4496-b4dc-d3f382ec1159', practice).
narrative_ontology:cs_reading_relation('9bc8e1dd-c775-4496-b4dc-d3f382ec1159', qwerty_persistence_inevitability__strategic_lock_in_reading, coexists_with).
narrative_ontology:cs_axiom('9bc8e1dd-c775-4496-b4dc-d3f382ec1159', foundational, technological_standards_emerge_from_historical_contingency).
narrative_ontology:cs_axiom_status(technological_standards_emerge_from_historical_contingency, holdable).
narrative_ontology:cs_axiom_grounding('9bc8e1dd-c775-4496-b4dc-d3f382ec1159', technological_standards_emerge_from_historical_contingency, empirically_contingent).
narrative_ontology:cs_axiom('9bc8e1dd-c775-4496-b4dc-d3f382ec1159', foundational, network_effects_create_self_reinforcing_lock_in).
narrative_ontology:cs_axiom_status(network_effects_create_self_reinforcing_lock_in, holdable).
narrative_ontology:cs_axiom_grounding('9bc8e1dd-c775-4496-b4dc-d3f382ec1159', network_effects_create_self_reinforcing_lock_in, empirically_contingent).
narrative_ontology:cs_reference_frame('9bc8e1dd-c775-4496-b4dc-d3f382ec1159', emergent_technological_standard).
narrative_ontology:cs_drift_state('9bc8e1dd-c775-4496-b4dc-d3f382ec1159', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('9bc8e1dd-c775-4496-b4dc-d3f382ec1159', '').
narrative_ontology:cs_kernel_id(qwerty_persistence_inevitability__path_dependency_reading, qwerty_persistence_inevitability).

% --- Structural relationships ---
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(qwerty_persistence_inevitability__path_dependency_reading, software_developers).
narrative_ontology:constraint_victim(qwerty_persistence_inevitability__path_dependency_reading, typists).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Produce QWERTY keyboards because it's the established standard. They respond to market demand and existing infrastructure, not actively enforcing QWERTY's dominance. Shifting to alternative layouts would incur significant retooling and market education costs without guaranteed returns.
narrative_ontology:constraint_stakeholder(qwerty_persistence_inevitability__path_dependency_reading, keyboard_manufacturers, agenda_setter,
    organized, biographical, constrained, global).

% Learn QWERTY as the default and are accustomed to it. The efficiency loss is diffuse and often unperceived. Switching to a more efficient layout (e.g., Dvorak) requires significant retraining, new hardware, and social coordination, making it a high-cost, low-return individual choice.
narrative_ontology:constraint_stakeholder(qwerty_persistence_inevitability__path_dependency_reading, typists, payer,
    powerless, biographical, identity_locked, global).

% Benefit from a universal keyboard standard, simplifying input handling and user interface design across platforms. They have little incentive to support alternative layouts due to the low demand and increased complexity.
narrative_ontology:constraint_stakeholder(qwerty_persistence_inevitability__path_dependency_reading, software_developers, beneficiary,
    moderate, biographical, mobile, global).

% Analyze the efficiency and health impacts of keyboard layouts. They often advocate for alternatives like Dvorak but face the reality of entrenched user habits and manufacturing inertia. Their influence is primarily academic and advisory.
narrative_ontology:constraint_stakeholder(qwerty_persistence_inevitability__path_dependency_reading, ergonomics_researchers, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a universal standard for keyboard layouts, enabling seamless interaction across different devices, users, and software applications without explicit coordination efforts.
% TRANSFER_FUNCTION: No direct transfer of value. The constraint imposes a diffuse, uncaptured efficiency cost on typists (suboptimal layout) and confers a diffuse coordination benefit on manufacturers and software developers (standardization).
% ABSENT_VOICES: Advocates for more efficient keyboard layouts (e.g., Dvorak users, ergonomics researchers) are present but lack the collective power to overcome the entrenched path dependency. Their voices are heard but not acted upon due to the high switching costs and diffuse nature of the problem.
% DISAPPEARANCE_RATIONALE: If QWERTY vanished overnight, the immediate chaos would be immense, but eventually, a new, potentially more efficient, standard would emerge through a painful, uncoordinated transition. The world would rearrange around a new default, but the process would be driven by emergent coordination rather than a central authority.
% FOUNDING_PROBLEM: The original problem was to prevent typewriter keys from jamming by separating common letter pairs, a mechanical constraint of early typewriters.
% FOUNDING_PROBLEM_CORROBORATION: Historians of technology and ergonomics researchers widely corroborate that the original mechanical problem is long dead with modern digital keyboards. Keyboard manufacturers acknowledge the historical origin but point to current user habits and installed base as the reason for QWERTY's persistence, not mechanical necessity.
narrative_ontology:disappearance_verdict(qwerty_persistence_inevitability__path_dependency_reading, world_rearranges).
narrative_ontology:founding_problem_status(qwerty_persistence_inevitability__path_dependency_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(qwerty_persistence_inevitability__path_dependency_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(qwerty_persistence_inevitability__path_dependency_reading, 'none', 1).
narrative_ontology:epsilon_provenance(qwerty_persistence_inevitability__path_dependency_reading, 0.05, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(qwerty_persistence_inevitability__path_dependency_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(qwerty_persistence_inevitability__path_dependency_reading, ExtMetricName, E),
    domain_priors:suppression_score(qwerty_persistence_inevitability__path_dependency_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(qwerty_persistence_inevitability__path_dependency_reading),
    narrative_ontology:constraint_metric(qwerty_persistence_inevitability__path_dependency_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(qwerty_persistence_inevitability__path_dependency_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(qwerty_persistence_inevitability__path_dependency_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is very low (0.05) because no identifiable party actively collects rents from QWERTY's persistence; the efficiency loss is a diffuse externality. Suppression is very high (0.95) because the constraint is maintained by the overwhelming inertia of the installed base, user habits, and manufacturing standards, making alternatives nearly impossible to adopt at scale. Accessibility collapse is high (0.9) as alternatives are known but practically inaccessible due to coordination failure. Resistance is low (0.05) because individual efforts to switch are futile, and collective action is absent. Theater ratio is 0.0 as there is no performative maintenance; the constraint simply 'is'.
 *
 * PERSPECTIVAL GAP:
 *   This reading emphasizes the 'natural' emergence and persistence of QWERTY due to historical accidents and network effects, leading to a Mountain classification. A 'strategic lock-in' reading would likely classify it as a Snare or Tangled Rope, identifying specific beneficiaries (e.g., manufacturers colluding to maintain the standard) and higher extractiveness. The engine's classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Keyboard manufacturers are agenda-setters in the sense that they produce the standard, but they are constrained by market demand. Typists are payers, bearing the diffuse cost of suboptimal efficiency, and are identity-locked into the standard. Software developers are diffuse beneficiaries of standardization. No party actively benefits from the 'extraction' in a concentrated way, nor is there a clear victim group beyond the diffuse efficiency loss.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    path_dependency_vs_strategic_lock_in,
    'Is QWERTY''s persistence primarily an outcome of accident-driven path dependency and network effects, or is it maintained by strategic actions of manufacturers to lock in users and extract rents?',
    'Historical analysis of manufacturer collusion, patent enforcement, and marketing strategies; economic modeling of switching costs and market power dynamics in the keyboard industry.',
    'If strategic lock-in is confirmed, the constraint would reclassify from Mountain to Snare or Tangled Rope, with identifiable beneficiaries (manufacturers) and victims (typists, alternative layout developers) and higher extractiveness.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(path_dependency_vs_strategic_lock_in, empirical, 'Distinguishing between emergent path dependency and deliberate market manipulation.').

omega_variable(
    diffuse_cost_measurement,
    'How accurately can the diffuse efficiency loss borne by typists be quantified and attributed as ''extraction'' in the absence of a clear beneficiary capturing that value?',
    'Development of robust methodologies for measuring uncaptured societal efficiency losses due to suboptimal standards, potentially through large-scale ergonomic studies and economic impact assessments.',
    'If quantifiable and significant, even without a clear capturer, the ''extractiveness'' metric might be re-evaluated upward, potentially shifting the classification towards a Piton (if no one benefits enough to maintain it) or even a Snare (if the diffuse cost is a form of systemic extraction).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(diffuse_cost_measurement, conceptual, 'The challenge of measuring diffuse, uncaptured costs as extraction.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(qwerty_persistence_inevitability__path_dependency_reading, 1873, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qwer_tr_t1873, qwerty_persistence_inevitability__path_dependency_reading, theater_ratio, 1873, 0.0).
narrative_ontology:measurement(qwer_tr_t1920, qwerty_persistence_inevitability__path_dependency_reading, theater_ratio, 1920, 0.0).
narrative_ontology:measurement(qwer_tr_t1950, qwerty_persistence_inevitability__path_dependency_reading, theater_ratio, 1950, 0.0).
narrative_ontology:measurement(qwer_tr_t1980, qwerty_persistence_inevitability__path_dependency_reading, theater_ratio, 1980, 0.0).
narrative_ontology:measurement(qwer_tr_t2000, qwerty_persistence_inevitability__path_dependency_reading, theater_ratio, 2000, 0.0).
narrative_ontology:measurement(qwer_tr_t2024, qwerty_persistence_inevitability__path_dependency_reading, theater_ratio, 2024, 0.0).

% Extraction over time
narrative_ontology:measurement(qwer_be_t1873, qwerty_persistence_inevitability__path_dependency_reading, base_extractiveness, 1873, 0.01).
narrative_ontology:measurement(qwer_be_t1920, qwerty_persistence_inevitability__path_dependency_reading, base_extractiveness, 1920, 0.02).
narrative_ontology:measurement(qwer_be_t1950, qwerty_persistence_inevitability__path_dependency_reading, base_extractiveness, 1950, 0.03).
narrative_ontology:measurement(qwer_be_t1980, qwerty_persistence_inevitability__path_dependency_reading, base_extractiveness, 1980, 0.04).
narrative_ontology:measurement(qwer_be_t2000, qwerty_persistence_inevitability__path_dependency_reading, base_extractiveness, 2000, 0.05).
narrative_ontology:measurement(qwer_be_t2024, qwerty_persistence_inevitability__path_dependency_reading, base_extractiveness, 2024, 0.05).

% Suppression requirement over time
narrative_ontology:measurement(qwer_su_t1873, qwerty_persistence_inevitability__path_dependency_reading, suppression_requirement, 1873, 0.8).
narrative_ontology:measurement(qwer_su_t1920, qwerty_persistence_inevitability__path_dependency_reading, suppression_requirement, 1920, 0.85).
narrative_ontology:measurement(qwer_su_t1950, qwerty_persistence_inevitability__path_dependency_reading, suppression_requirement, 1950, 0.9).
narrative_ontology:measurement(qwer_su_t1980, qwerty_persistence_inevitability__path_dependency_reading, suppression_requirement, 1980, 0.93).
narrative_ontology:measurement(qwer_su_t2000, qwerty_persistence_inevitability__path_dependency_reading, suppression_requirement, 2000, 0.95).
narrative_ontology:measurement(qwer_su_t2024, qwerty_persistence_inevitability__path_dependency_reading, suppression_requirement, 2024, 0.95).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(qwerty_persistence_inevitability__path_dependency_reading, information_standard).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'qwerty_persistence_inevitability' kernel, focusing on path dependency. The sibling 'strategic_lock_in_reading' offers an alternative explanation based on manufacturer-engineered lock-in.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
