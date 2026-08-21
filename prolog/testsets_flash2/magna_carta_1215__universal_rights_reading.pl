% ============================================================================
% CONSTRAINT STORY: magna_carta_1215__universal_rights_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_magna_carta_1215__universal_rights_reading, []).

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
    narrative_ontology:constraint_vindicates/2,
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
 *   constraint_id: magna_carta_1215__universal_rights_reading
 *   human_readable: Magna Carta (1215) as Universal Due Process Precedent
 *   domain: constitutional_law/legal_history/political_theory
 *
 * SUMMARY:
 *   This constraint represents the 'universal rights' reading of Magna Carta
 *   (1215), where Clause 39's protection for 'free men' is interpreted as a
 *   transhistorical precedent for universal due process, applying to all
 *   persons and limiting all forms of state power. This reading emphasizes
 *   the document's enduring symbolic and legal force as a foundation for
 *   human rights, rather than its original feudal context. The claimed type
 *   is 'rope' because it genuinely coordinates legal protections for a broad
 *   beneficiary set, with relatively low extraction, though it requires
 *   active enforcement to maintain its universal scope. This is one reading
 *   of the 'magna_carta_1215' kernel, distinct from the
 *   'baronial_privilege_reading' and 'living_document_reading'.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(magna_carta_1215__universal_rights_reading, 0.15).
domain_priors:suppression_score(magna_carta_1215__universal_rights_reading, 0.2).
domain_priors:theater_ratio(magna_carta_1215__universal_rights_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(magna_carta_1215__universal_rights_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(magna_carta_1215__universal_rights_reading, suppression_requirement, 0.2).
narrative_ontology:constraint_metric(magna_carta_1215__universal_rights_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(magna_carta_1215__universal_rights_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(magna_carta_1215__universal_rights_reading, resistance, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(magna_carta_1215__universal_rights_reading, rope).
narrative_ontology:human_readable(magna_carta_1215__universal_rights_reading, "Magna Carta (1215) as Universal Due Process Precedent").
narrative_ontology:topic_domain(magna_carta_1215__universal_rights_reading, "constitutional_law/legal_history/political_theory").

domain_priors:requires_active_enforcement(magna_carta_1215__universal_rights_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(magna_carta_1215__universal_rights_reading, 'dfa9c224-77b2-41a3-b9cf-d0bf3e661de7').
narrative_ontology:cs_kernel_codification('dfa9c224-77b2-41a3-b9cf-d0bf3e661de7', fixed_text).
narrative_ontology:cs_authority_grounding('dfa9c224-77b2-41a3-b9cf-d0bf3e661de7', lineage).
narrative_ontology:cs_interpretation_layer_present('dfa9c224-77b2-41a3-b9cf-d0bf3e661de7').
narrative_ontology:cs_reading_relation('dfa9c224-77b2-41a3-b9cf-d0bf3e661de7', magna_carta_1215__baronial_privilege_reading, forecloses).
narrative_ontology:cs_reading_relation('dfa9c224-77b2-41a3-b9cf-d0bf3e661de7', magna_carta_1215__living_document_reading, coexists_with).
narrative_ontology:cs_axiom('dfa9c224-77b2-41a3-b9cf-d0bf3e661de7', foundational, free_men_equals_all_persons).
narrative_ontology:cs_axiom_status(free_men_equals_all_persons, holdable).
narrative_ontology:cs_axiom_grounding('dfa9c224-77b2-41a3-b9cf-d0bf3e661de7', free_men_equals_all_persons, deontological).
narrative_ontology:cs_axiom('dfa9c224-77b2-41a3-b9cf-d0bf3e661de7', foundational, clause_39_universal_due_process).
narrative_ontology:cs_axiom_status(clause_39_universal_due_process, holdable).
narrative_ontology:cs_axiom_grounding('dfa9c224-77b2-41a3-b9cf-d0bf3e661de7', clause_39_universal_due_process, conventional).
narrative_ontology:cs_reference_frame('dfa9c224-77b2-41a3-b9cf-d0bf3e661de7', enlightenment_universal_rights).
narrative_ontology:cs_drift_state('dfa9c224-77b2-41a3-b9cf-d0bf3e661de7', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('dfa9c224-77b2-41a3-b9cf-d0bf3e661de7', '').
narrative_ontology:cs_kernel_id(magna_carta_1215__universal_rights_reading, magna_carta_1215).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(magna_carta_1215__universal_rights_reading, all_persons_under_law).
narrative_ontology:constraint_beneficiary(magna_carta_1215__universal_rights_reading, judicial_system).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(magna_carta_1215__universal_rights_reading, executive_power).
narrative_ontology:constraint_victim(magna_carta_1215__universal_rights_reading, legislative_power).
narrative_ontology:constraint_vindicates(magna_carta_1215__universal_rights_reading, rule_of_law_doctrine).
narrative_ontology:constraint_vindicates(magna_carta_1215__universal_rights_reading, due_process_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Receives the protection of due process against arbitrary state action, as interpreted through this reading. Their identity as 'persons' is fused with the legal framework that grants these rights, making exit from the system of law unthinkable.
narrative_ontology:constraint_stakeholder(magna_carta_1215__universal_rights_reading, all_persons_under_law, beneficiary,
    powerless, generational, identity_locked, national).

% Interprets and enforces the due process constraint, applying it universally. Benefits from the legitimacy derived from upholding fundamental rights, but is constrained by the need to maintain consistency with historical precedent and evolving legal norms.
narrative_ontology:constraint_stakeholder(magna_carta_1215__universal_rights_reading, judicial_system, agenda_setter,
    institutional, civilizational, constrained, national).

% Bears the cost of adhering to due process, limiting its ability to act arbitrarily or swiftly without legal justification. Must operate within the bounds set by the universal application of Clause 39.
narrative_ontology:constraint_stakeholder(magna_carta_1215__universal_rights_reading, executive_power, payer,
    institutional, immediate, constrained, national).

% Must craft laws that respect the universal due process constraint, preventing the enactment of arbitrary or discriminatory legislation. Bears the cost of legal challenge if it oversteps these bounds.
narrative_ontology:constraint_stakeholder(magna_carta_1215__universal_rights_reading, legislative_power, payer,
    institutional, biographical, constrained, national).

% Analyze the historical context and evolution of Magna Carta, providing critical commentary on the 'universal rights' interpretation. Their work informs, but does not directly control, the legal application of the constraint.
narrative_ontology:constraint_stakeholder(magna_carta_1215__universal_rights_reading, historical_scholars, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a foundational legal principle that coordinates state power by requiring due process for all persons, ensuring a predictable and just application of law across society.
% TRANSFER_FUNCTION: Transfers the right to fair legal process from the potential arbitrary power of the state to all individuals, limiting state action and empowering citizens with legal recourse.
% ABSENT_VOICES: Feudal lords and monarchists who would have opposed the expansion of 'free men' beyond their narrow class interests are historically absent from the modern discourse, their original intent superseded by this reading.
% DISAPPEARANCE_RATIONALE: If this universal interpretation of Magna Carta vanished, the foundational principle of due process for all would be severely undermined, leading to a potential increase in arbitrary state power and a fundamental reordering of legal protections for individuals.
% FOUNDING_PROBLEM: The problem of arbitrary royal power and the need to establish a legal framework that limited the monarch's ability to act without legal justification, particularly concerning the rights of 'free men'.
% FOUNDING_PROBLEM_CORROBORATION: Legal scholars, human rights advocates, and international legal bodies corroborate that the problem of arbitrary state power remains live, and that the universal application of due process is a continuous struggle, even if the original context of a feudal monarch has changed.
narrative_ontology:disappearance_verdict(magna_carta_1215__universal_rights_reading, world_rearranges).
narrative_ontology:founding_problem_status(magna_carta_1215__universal_rights_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(magna_carta_1215__universal_rights_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(magna_carta_1215__universal_rights_reading, 'none', 1).
narrative_ontology:epsilon_provenance(magna_carta_1215__universal_rights_reading, 0.15, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(magna_carta_1215__universal_rights_reading_tests).
:- end_tests(magna_carta_1215__universal_rights_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.15) because this reading primarily functions to protect individuals from arbitrary state power, rather than to extract resources. Suppression is also low (0.2) as the constraint is widely accepted as a foundational legal principle, though it requires ongoing judicial and legislative effort to resist erosion. Theater ratio is low (0.1) because its function as a legal precedent is largely genuine, not performative. Accessibility collapse is high (0.7) because the principle of due process, once understood, significantly limits alternative forms of arbitrary state action. Resistance is low (0.1) because the principle is broadly accepted, though specific applications may be contested.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of 'all_persons_under_law', this constraint is a pure rope, providing essential coordination and protection. From the perspective of 'executive_power' or 'legislative_power', it is a necessary but sometimes burdensome constraint on their authority. The 'judicial_system' views it as a core function and source of legitimacy. The engine's per-seat classification will reflect these different experiences.
 *
 * DIRECTIONALITY LOGIC:
 *   The 'all_persons_under_law' are the primary beneficiaries, receiving protection from arbitrary state action. The 'judicial_system' acts as the agenda-setter, interpreting and enforcing this universal application. 'Executive_power' and 'legislative_power' are payers, as their actions are constrained by the due process requirement. Historical scholars act as observers, analyzing the interpretation without directly benefiting or paying.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading prevents mislabeling by emphasizing the constraint's enduring function as a source of universal rights, rather than dismissing it as an obsolete feudal document. While the original 'founding problem' of arbitrary royal power has evolved, the principle of due process remains live, preventing mandatrophy by adapting its application to modern state structures. The low theater ratio and continued active enforcement indicate it is not a piton.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    scope_of_free_men,
    'Is the interpretation of ''free men'' as ''all persons'' a legitimate transhistorical reading, or an anachronistic projection onto the original text?',
    'Continued legal and historical scholarship, judicial precedent, and public discourse on constitutional interpretation. The resolution depends on the evolving consensus within legal and political theory.',
    'If deemed anachronistic, the constraint''s scope would narrow significantly, potentially reclassifying it closer to the ''baronial_privilege_reading'' for its original context, or requiring a different grounding for universal rights. If affirmed, it reinforces the ''rope'' classification for its broad coordination function.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(scope_of_free_men, conceptual, 'Ambiguity regarding the historical vs. modern interpretation of ''free men'' in Magna Carta.').

omega_variable(
    enforcement_universality,
    'How universally is due process actually applied across all persons and contexts, particularly for marginalized groups, despite the legal claim of universality?',
    'Empirical studies of legal outcomes, disaggregated by demographic and socioeconomic factors, and analysis of access to justice for different populations.',
    'If empirical application is highly uneven, the effective extractiveness for marginalized groups would be higher, and the ''rope'' classification might shift towards ''tangled_rope'' or ''snare'' for those specific seats, despite the universal claim.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_universality, empirical, 'Gap between claimed universal application of due process and its actual, empirical enforcement.').

omega_variable(
    kernel_reading_identity,
    'This constraint is one reading of the ''magna_carta_1215'' kernel. What are the structural differences that distinguish this ''universal_rights_reading'' from the ''baronial_privilege_reading'' and ''living_document_reading''?',
    'Comparative analysis of the core axioms, beneficiary/victim sets, and scope of application for each reading. The engine''s cross-reading comparison will highlight these structural deltas.',
    'The structural differences confirm that these are distinct constraints, each with its own classification, rather than different perspectives on the same constraint. This omega documents the decomposition.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Documents the kernel-reading identity and structural distinctions from sibling readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(magna_carta_1215__universal_rights_reading, 1215, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(magn_tr_t1215, magna_carta_1215__universal_rights_reading, theater_ratio, 1215, 0.05).
narrative_ontology:measurement(magn_tr_t1688, magna_carta_1215__universal_rights_reading, theater_ratio, 1688, 0.08).
narrative_ontology:measurement(magn_tr_t1776, magna_carta_1215__universal_rights_reading, theater_ratio, 1776, 0.09).
narrative_ontology:measurement(magn_tr_t1948, magna_carta_1215__universal_rights_reading, theater_ratio, 1948, 0.1).
narrative_ontology:measurement(magn_tr_t2024, magna_carta_1215__universal_rights_reading, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(magn_be_t1215, magna_carta_1215__universal_rights_reading, base_extractiveness, 1215, 0.05).
narrative_ontology:measurement(magn_be_t1688, magna_carta_1215__universal_rights_reading, base_extractiveness, 1688, 0.1).
narrative_ontology:measurement(magn_be_t1776, magna_carta_1215__universal_rights_reading, base_extractiveness, 1776, 0.12).
narrative_ontology:measurement(magn_be_t1948, magna_carta_1215__universal_rights_reading, base_extractiveness, 1948, 0.15).
narrative_ontology:measurement(magn_be_t2024, magna_carta_1215__universal_rights_reading, base_extractiveness, 2024, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(magn_su_t1215, magna_carta_1215__universal_rights_reading, suppression_requirement, 1215, 0.1).
narrative_ontology:measurement(magn_su_t1688, magna_carta_1215__universal_rights_reading, suppression_requirement, 1688, 0.15).
narrative_ontology:measurement(magn_su_t1776, magna_carta_1215__universal_rights_reading, suppression_requirement, 1776, 0.18).
narrative_ontology:measurement(magn_su_t1948, magna_carta_1215__universal_rights_reading, suppression_requirement, 1948, 0.2).
narrative_ontology:measurement(magn_su_t2024, magna_carta_1215__universal_rights_reading, suppression_requirement, 2024, 0.2).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
