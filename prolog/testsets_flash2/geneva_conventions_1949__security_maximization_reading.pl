% ============================================================================
% CONSTRAINT STORY: geneva_conventions_1949__security_maximization_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_geneva_conventions_1949__security_maximization_reading, []).

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
 *   constraint_id: geneva_conventions_1949__security_maximization_reading
 *   human_readable: Geneva Conventions (Security Maximization Reading)
 *   domain: international_humanitarian_law/political_philosophy
 *
 * SUMMARY:
 *   This constraint represents a reading of the 1949 Geneva Conventions that
 *   prioritizes state security and operational necessity in asymmetric
 *   conflict, often at the expense of humanitarian protections. It justifies
 *   expanding categories like 'unlawful combatant' to deny POW status,
 *   degrading civilian immunity, and normalizing indefinite detention and
 *   coercive interrogation. The claimed type is 'snare' because the
 *   coordination story (adapting to new threats) is a cover for systematic
 *   extraction of rights and protections from identifiable victims,
 *   maintained through active suppression of alternative interpretations and
 *   legal challenges. The metrics reflect a high degree of extraction and
 *   suppression, with significant theatricality in maintaining a veneer of
 *   legality.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(geneva_conventions_1949__security_maximization_reading, 0.92).
domain_priors:suppression_score(geneva_conventions_1949__security_maximization_reading, 0.95).
domain_priors:theater_ratio(geneva_conventions_1949__security_maximization_reading, 0.75).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(geneva_conventions_1949__security_maximization_reading, extractiveness, 0.92).
narrative_ontology:constraint_metric(geneva_conventions_1949__security_maximization_reading, suppression_requirement, 0.95).
narrative_ontology:constraint_metric(geneva_conventions_1949__security_maximization_reading, theater_ratio, 0.75).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(geneva_conventions_1949__security_maximization_reading, accessibility_collapse, 0.88).
narrative_ontology:constraint_metric(geneva_conventions_1949__security_maximization_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(geneva_conventions_1949__security_maximization_reading, snare).
narrative_ontology:human_readable(geneva_conventions_1949__security_maximization_reading, "Geneva Conventions (Security Maximization Reading)").
narrative_ontology:topic_domain(geneva_conventions_1949__security_maximization_reading, "international_humanitarian_law/political_philosophy").

domain_priors:requires_active_enforcement(geneva_conventions_1949__security_maximization_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(geneva_conventions_1949__security_maximization_reading, '76fe4e43-bab9-4df9-99b3-0897de40c55b').
narrative_ontology:cs_kernel_codification('76fe4e43-bab9-4df9-99b3-0897de40c55b', fixed_text).
narrative_ontology:cs_authority_grounding('76fe4e43-bab9-4df9-99b3-0897de40c55b', extraction).
narrative_ontology:cs_interpretation_layer_present('76fe4e43-bab9-4df9-99b3-0897de40c55b').
narrative_ontology:cs_reading_relation('76fe4e43-bab9-4df9-99b3-0897de40c55b', geneva_conventions_1949__humanitarian_ceiling_reading, forecloses).
narrative_ontology:cs_reading_relation('76fe4e43-bab9-4df9-99b3-0897de40c55b', geneva_conventions_1949__conditional_reciprocity_reading, coexists_with).
narrative_ontology:cs_axiom('76fe4e43-bab9-4df9-99b3-0897de40c55b', foundational, state_security_is_paramount).
narrative_ontology:cs_axiom_status(state_security_is_paramount, holdable).
narrative_ontology:cs_axiom_grounding('76fe4e43-bab9-4df9-99b3-0897de40c55b', state_security_is_paramount, instrumental).
narrative_ontology:cs_axiom('76fe4e43-bab9-4df9-99b3-0897de40c55b', foundational, asymmetric_conflict_exceptionalism).
narrative_ontology:cs_axiom_status(asymmetric_conflict_exceptionalism, holdable).
narrative_ontology:cs_axiom_grounding('76fe4e43-bab9-4df9-99b3-0897de40c55b', asymmetric_conflict_exceptionalism, conventional).
narrative_ontology:cs_reference_frame('76fe4e43-bab9-4df9-99b3-0897de40c55b', unfettered_state_sovereignty).
narrative_ontology:cs_drift_state('76fe4e43-bab9-4df9-99b3-0897de40c55b', post_9_11_era, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('76fe4e43-bab9-4df9-99b3-0897de40c55b', '').
narrative_ontology:cs_kernel_id(geneva_conventions_1949__security_maximization_reading, geneva_conventions_1949).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(geneva_conventions_1949__security_maximization_reading, state_security_apparatus).
narrative_ontology:constraint_beneficiary(geneva_conventions_1949__security_maximization_reading, political_executive).
narrative_ontology:constraint_victim(geneva_conventions_1949__security_maximization_reading, unlawful_combatants).
narrative_ontology:constraint_victim(geneva_conventions_1949__security_maximization_reading, detainees).
narrative_ontology:constraint_victim(geneva_conventions_1949__security_maximization_reading, civilian_populations_in_conflict_zones).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interprets the Conventions to prioritize state security above all other considerations, expanding categories like 'unlawful combatant' to justify actions that would otherwise be prohibited. Benefits from maximum operational flexibility and minimal accountability.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__security_maximization_reading, state_security_apparatus, agenda_setter,
    institutional, biographical, arbitrage, global).

% Benefits from the perceived ability to protect the state by any means necessary, avoiding domestic and international legal constraints. Uses the security maximization reading to justify controversial policies to the public.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__security_maximization_reading, political_executive, beneficiary,
    institutional, immediate, mobile, national).

% Denied POW status, habeas corpus, and other fundamental protections. Subject to indefinite detention and coercive interrogation without trial. Bear the full brunt of the security maximization interpretation.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__security_maximization_reading, unlawful_combatants, payer,
    powerless, immediate, trapped, local).

% Held without clear legal status, often in secret facilities, and subjected to interrogation methods that push the boundaries of torture. Their rights are systematically eroded under this reading.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__security_maximization_reading, detainees, payer,
    powerless, immediate, trapped, local).

% Experience degraded immunity from harm, as 'collateral damage' is more readily accepted and the 'human shields' doctrine is used to justify attacks in populated areas. Their protections are minimized.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__security_maximization_reading, civilian_populations_in_conflict_zones, payer,
    powerless, immediate, trapped, local).

% Their advocacy for strict adherence to humanitarian law is dismissed as naive or impractical in asymmetric conflict. They are often denied access to detainees or conflict zones, limiting their ability to monitor and report violations.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__security_maximization_reading, international_humanitarian_organizations, excluded,
    organized, generational, constrained, global).

% Analyze the legal justifications and practical consequences of this reading, often critiquing its erosion of established norms. Their work aims to clarify the boundaries of permissible state action.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__security_maximization_reading, international_legal_scholars, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Claims to coordinate state actions in asymmetric conflict by providing a framework that prioritizes national security, allowing for flexible responses to non-state threats. This is a coordination of state power, not of humanitarian outcomes.
% TRANSFER_FUNCTION: Transfers legal protections and human rights from individuals (detainees, combatants, civilians) to the state, in exchange for perceived security gains. It reallocates the burden of conflict from the state to its targets.
% ABSENT_VOICES: Victims of state violence, human rights advocates, and international legal bodies that uphold a more robust interpretation of humanitarian law are systematically marginalized or excluded from the decision-making process that adopts this reading.
% DISAPPEARANCE_RATIONALE: If this reading vanished, states would face immediate and significant legal and political pressure to adhere to stricter interpretations of the Geneva Conventions. Operational doctrines would need to be revised, and accountability for past actions would increase, fundamentally altering the landscape of state security operations.
% FOUNDING_PROBLEM: The problem of effectively combating non-state actors and irregular forces within the traditional legal frameworks of armed conflict, which were designed for conventional state-on-state warfare.
% FOUNDING_PROBLEM_CORROBORATION: State security agencies and political leaders consistently attest that the problem of asymmetric warfare and terrorism remains live and requires flexible responses. International legal scholars and human rights organizations, while disagreeing with the solution, acknowledge the challenge of adapting IHL to new forms of conflict.
narrative_ontology:disappearance_verdict(geneva_conventions_1949__security_maximization_reading, world_rearranges).
narrative_ontology:founding_problem_status(geneva_conventions_1949__security_maximization_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(geneva_conventions_1949__security_maximization_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(geneva_conventions_1949__security_maximization_reading, 'none', 1).
narrative_ontology:epsilon_provenance(geneva_conventions_1949__security_maximization_reading, 0.92, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(geneva_conventions_1949__security_maximization_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(geneva_conventions_1949__security_maximization_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(geneva_conventions_1949__security_maximization_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is very high (0.92) because this reading systematically strips away fundamental protections from vulnerable populations and individuals in conflict, maximizing state power. Suppression is also very high (0.95) as it requires active legal and political efforts to silence dissent, deny access, and suppress alternative interpretations of IHL. The theater ratio (0.75) is high because the state apparatus often maintains a performative commitment to 'humanitarian law' while its actual practices systematically undermine it, using legalistic arguments to justify actions that violate the spirit and letter of the conventions. Accessibility collapse is high (0.88) because this reading creates a legal and operational environment where alternatives for victims (e.g., seeking legal redress, claiming POW status) are almost entirely foreclosed. Resistance is high (0.70) due to persistent challenges from human rights groups, international bodies, and legal scholars, but this resistance is largely ineffective against the state's institutional power.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the state security apparatus, this reading is a necessary adaptation to modern threats, ensuring national survival. From the perspective of victims and humanitarian advocates, it is a systematic dismantling of fundamental rights and a betrayal of the Conventions' core purpose. The engine's classification as a snare captures this divergence, highlighting the extractive nature of the 'adaptation.'
 *
 * DIRECTIONALITY LOGIC:
 *   The state security apparatus and political executive are clear beneficiaries, gaining maximum operational flexibility and reduced accountability (low directionality). 'Unlawful combatants,' detainees, and civilian populations are the primary targets, bearing the full cost of eroded protections (high directionality). International humanitarian organizations and legal scholars are excluded voices, their efforts to uphold stricter interpretations suppressed.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    empirical_efficacy_of_security_maximization,
    'Does the suspension of humanitarian protections, as justified by this reading, demonstrably lead to increased state security or more effective conflict resolution?',
    'Independent, long-term empirical studies comparing security outcomes in conflicts where this reading was applied versus those where stricter IHL adherence was maintained.',
    'If empirical evidence shows no security benefit, or even counterproductive effects (e.g., radicalization), the instrumental grounding of this reading would collapse, weakening its legitimacy and potentially shifting its classification towards a pure snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(empirical_efficacy_of_security_maximization, empirical, 'Whether the claimed security benefits of this reading are empirically verifiable.').

omega_variable(
    legal_status_of_unlawful_combatant_category,
    'Is the ''unlawful combatant'' category a legitimate legal innovation within the framework of IHL, or an extra-legal construct designed to circumvent protections?',
    'Definitive rulings by international courts (e.g., ICJ, ICC) on the scope and applicability of this category, or a new international treaty clarifying combatant status in asymmetric conflict.',
    'If declared extra-legal, a core mechanism of extraction for this reading would be delegitimized, forcing states to either abandon the category or openly defy international law, increasing the constraint''s suppression and potentially its theater ratio.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(legal_status_of_unlawful_combatant_category, conceptual, 'The legal legitimacy of the ''unlawful combatant'' category.').

omega_variable(
    internalized_suppression_on_military_personnel,
    'To what extent do military personnel internalize the security maximization reading, leading to self-censorship or normalization of practices that would otherwise be considered violations?',
    'Sociological studies and psychological assessments of military culture and decision-making processes, particularly in units operating under this reading, including post-service interviews and ethical training evaluations.',
    'If internalized suppression is significant, the effective suppression of this constraint is higher than structural measures suggest, as individuals self-regulate to align with the permissive interpretation, making resistance from within the system less likely.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(internalized_suppression_on_military_personnel, empirical, 'Structural vs. internalized suppression mechanism on military personnel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(geneva_conventions_1949__security_maximization_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gene_tr_t0, geneva_conventions_1949__security_maximization_reading, theater_ratio, 0, 0.6).
narrative_ontology:measurement(gene_tr_t5, geneva_conventions_1949__security_maximization_reading, theater_ratio, 5, 0.65).
narrative_ontology:measurement(gene_tr_t10, geneva_conventions_1949__security_maximization_reading, theater_ratio, 10, 0.7).
narrative_ontology:measurement(gene_tr_t15, geneva_conventions_1949__security_maximization_reading, theater_ratio, 15, 0.73).
narrative_ontology:measurement(gene_tr_t20, geneva_conventions_1949__security_maximization_reading, theater_ratio, 20, 0.75).

% Extraction over time
narrative_ontology:measurement(gene_be_t0, geneva_conventions_1949__security_maximization_reading, base_extractiveness, 0, 0.8).
narrative_ontology:measurement(gene_be_t5, geneva_conventions_1949__security_maximization_reading, base_extractiveness, 5, 0.85).
narrative_ontology:measurement(gene_be_t10, geneva_conventions_1949__security_maximization_reading, base_extractiveness, 10, 0.9).
narrative_ontology:measurement(gene_be_t15, geneva_conventions_1949__security_maximization_reading, base_extractiveness, 15, 0.91).
narrative_ontology:measurement(gene_be_t20, geneva_conventions_1949__security_maximization_reading, base_extractiveness, 20, 0.92).

% Suppression requirement over time
narrative_ontology:measurement(gene_su_t0, geneva_conventions_1949__security_maximization_reading, suppression_requirement, 0, 0.85).
narrative_ontology:measurement(gene_su_t5, geneva_conventions_1949__security_maximization_reading, suppression_requirement, 5, 0.88).
narrative_ontology:measurement(gene_su_t10, geneva_conventions_1949__security_maximization_reading, suppression_requirement, 10, 0.92).
narrative_ontology:measurement(gene_su_t15, geneva_conventions_1949__security_maximization_reading, suppression_requirement, 15, 0.94).
narrative_ontology:measurement(gene_su_t20, geneva_conventions_1949__security_maximization_reading, suppression_requirement, 20, 0.95).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(geneva_conventions_1949__security_maximization_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(geneva_conventions_1949__security_maximization_reading, humanitarian_ceiling_reading).
narrative_ontology:affects_constraint(geneva_conventions_1949__security_maximization_reading, conditional_reciprocity_reading).
narrative_ontology:affects_constraint(geneva_conventions_1949__security_maximization_reading, torture_prohibition_erosion).
narrative_ontology:affects_constraint(geneva_conventions_1949__security_maximization_reading, habeas_corpus_suspension).

% DUAL FORMULATION NOTE:
% This constraint is one of three competing readings of the 1949 Geneva Conventions kernel. This 'security maximization' reading directly influences and is influenced by the 'humanitarian ceiling' and 'conditional reciprocity' readings, as they represent different interpretations of the same foundational text.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
