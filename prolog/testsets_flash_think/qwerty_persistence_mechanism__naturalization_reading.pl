% ============================================================================
% CONSTRAINT STORY: qwerty_persistence_mechanism__naturalization_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_qwerty_persistence_mechanism__naturalization_reading, []).

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
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
 *   constraint_id: qwerty_persistence_mechanism__naturalization_reading
 *   human_readable: QWERTY Keyboard Layout Persistence (Naturalization Reading)
 *   domain: economic_history/technology_studies/path_dependence_theory
 *
 * SUMMARY:
 *   This constraint story presents the 'naturalization' reading of QWERTY's
 *   persistence, arguing that its dominance stems from its genuine adequacy
 *   and the fair competition through which alternatives lapsed. It frames
 *   QWERTY as a successful coordination mechanism (a Rope) with minimal
 *   inherent extraction, where switching costs are a natural consequence of
 *   skill investment rather than a coercive barrier. This reading contrasts
 *   with 'lock-in' or 'beneficiary extraction' explanations, which are
 *   treated as sibling constraints.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(qwerty_persistence_mechanism__naturalization_reading, 0.18).
domain_priors:suppression_score(qwerty_persistence_mechanism__naturalization_reading, 0.15).
domain_priors:theater_ratio(qwerty_persistence_mechanism__naturalization_reading, 0.08).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(qwerty_persistence_mechanism__naturalization_reading, extractiveness, 0.18).
narrative_ontology:constraint_metric(qwerty_persistence_mechanism__naturalization_reading, suppression_requirement, 0.15).
narrative_ontology:constraint_metric(qwerty_persistence_mechanism__naturalization_reading, theater_ratio, 0.08).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(qwerty_persistence_mechanism__naturalization_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(qwerty_persistence_mechanism__naturalization_reading, resistance, 0.12).

% --- Constraint claim ---
narrative_ontology:constraint_claim(qwerty_persistence_mechanism__naturalization_reading, rope).
narrative_ontology:human_readable(qwerty_persistence_mechanism__naturalization_reading, "QWERTY Keyboard Layout Persistence (Naturalization Reading)").
narrative_ontology:topic_domain(qwerty_persistence_mechanism__naturalization_reading, "economic_history/technology_studies/path_dependence_theory").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(qwerty_persistence_mechanism__naturalization_reading, '9eba2f39-8460-455b-a5f3-d661acc72c25').
narrative_ontology:cs_kernel_codification('9eba2f39-8460-455b-a5f3-d661acc72c25', formalized).
narrative_ontology:cs_authority_grounding('9eba2f39-8460-455b-a5f3-d661acc72c25', practice).
narrative_ontology:cs_reading_relation('9eba2f39-8460-455b-a5f3-d661acc72c25', qwerty_persistence_mechanism__lock_in_reading, coexists_with).
narrative_ontology:cs_reading_relation('9eba2f39-8460-455b-a5f3-d661acc72c25', qwerty_persistence_mechanism__beneficiary_extraction_reading, coexists_with).
narrative_ontology:cs_axiom('9eba2f39-8460-455b-a5f3-d661acc72c25', foundational, qwerty_technical_adequacy).
narrative_ontology:cs_axiom_status(qwerty_technical_adequacy, holdable).
narrative_ontology:cs_axiom_grounding('9eba2f39-8460-455b-a5f3-d661acc72c25', qwerty_technical_adequacy, empirically_contingent).
narrative_ontology:cs_axiom('9eba2f39-8460-455b-a5f3-d661acc72c25', foundational, fair_market_competition).
narrative_ontology:cs_axiom_status(fair_market_competition, holdable).
narrative_ontology:cs_axiom_grounding('9eba2f39-8460-455b-a5f3-d661acc72c25', fair_market_competition, conventional).
narrative_ontology:cs_reference_frame('9eba2f39-8460-455b-a5f3-d661acc72c25', optimal_market_outcome).
narrative_ontology:cs_drift_state('9eba2f39-8460-455b-a5f3-d661acc72c25', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('9eba2f39-8460-455b-a5f3-d661acc72c25', '').
narrative_ontology:cs_kernel_id(qwerty_persistence_mechanism__naturalization_reading, qwerty_persistence_mechanism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(qwerty_persistence_mechanism__naturalization_reading, qwerty_users).
narrative_ontology:constraint_beneficiary(qwerty_persistence_mechanism__naturalization_reading, keyboard_manufacturers).
narrative_ontology:constraint_beneficiary(qwerty_persistence_mechanism__naturalization_reading, typing_tutors).
narrative_ontology:constraint_victim(qwerty_persistence_mechanism__naturalization_reading, dvorak_advocates).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefit from a universally recognized standard, allowing easy transfer of typing skills across devices and workplaces. They face significant personal switching costs if they were to adopt an alternative layout, but this is seen as a natural consequence of skill investment, not an imposed burden.
narrative_ontology:constraint_stakeholder(qwerty_persistence_mechanism__naturalization_reading, qwerty_users, beneficiary,
    organized, biographical, constrained, global).

% Benefit from standardized production, reduced R&D costs for layout design, and a predictable market for QWERTY keyboards. They are responsive to market demand but currently face no significant pressure to produce alternative layouts at scale.
narrative_ontology:constraint_stakeholder(qwerty_persistence_mechanism__naturalization_reading, keyboard_manufacturers, beneficiary,
    institutional, generational, mobile, global).

% Bear the costs of non-adoption, including limited availability of Dvorak keyboards and the need to adapt to QWERTY in most public or shared computing environments. From this reading, their alternative simply failed to gain market traction due to QWERTY's inherent adequacy and fair competition.
narrative_ontology:constraint_stakeholder(qwerty_persistence_mechanism__naturalization_reading, dvorak_advocates, payer,
    powerless, generational, constrained, global).

% Benefit from a stable, standardized curriculum for teaching typing. Their expertise is directly tied to the dominant layout, making it easier to train new users.
narrative_ontology:constraint_stakeholder(qwerty_persistence_mechanism__naturalization_reading, typing_tutors, beneficiary,
    moderate, biographical, mobile, local).

% Analyze the historical development and persistence of QWERTY, seeking to understand the interplay of technological, economic, and social factors. They observe the constraint from an academic perspective.
narrative_ontology:constraint_stakeholder(qwerty_persistence_mechanism__naturalization_reading, economic_historians, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(qwerty_persistence_mechanism__naturalization_reading, diffuse).
narrative_ontology:fixing_cost_class(qwerty_persistence_mechanism__naturalization_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Standardizes keyboard layout, enabling efficient training, manufacturing, and interoperability across a vast ecosystem of users, devices, and software. This solves the collective action problem of needing a common interface for text input.
% TRANSFER_FUNCTION: Transfers the benefits of a common standard (ease of learning, widespread availability) to users and manufacturers, while imposing the cost of non-standardization (limited options, adaptation effort) on those who prefer alternative layouts.
% ABSENT_VOICES: Designers and proponents of alternative keyboard layouts (e.g., Dvorak, Colemak) are largely absent from the mainstream conversation, as their designs did not achieve widespread adoption. They would argue for the technical superiority of their layouts, but this reading posits their alternatives lapsed through fair competition.
% DISAPPEARANCE_RATIONALE: If QWERTY vanished overnight, the global typing ecosystem would face immense disruption. Billions of users would need to retrain, manufacturers would retool, and software interfaces would require redesign. A new standard would eventually emerge, but the transition would be chaotic and costly.
% FOUNDING_PROBLEM: The initial problem was to create a functional and efficient keyboard layout for early typewriters that prevented key jamming and allowed for rapid typing, establishing a common standard for the nascent typing industry.
% FOUNDING_PROBLEM_CORROBORATION: While QWERTY's optimality is debated, the fundamental need for a standardized, functional keyboard layout remains live. Ergonomics researchers and technology historians generally agree on the importance of a standard, even if they contest QWERTY's technical merits relative to alternatives. This corroboration comes from independent academic analysis, not solely from benefiting parties.
narrative_ontology:disappearance_verdict(qwerty_persistence_mechanism__naturalization_reading, world_rearranges).
narrative_ontology:founding_problem_status(qwerty_persistence_mechanism__naturalization_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(qwerty_persistence_mechanism__naturalization_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(qwerty_persistence_mechanism__naturalization_reading, 'none', 1).
narrative_ontology:epsilon_provenance(qwerty_persistence_mechanism__naturalization_reading, 0.18, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(qwerty_persistence_mechanism__naturalization_reading_tests).
:- end_tests(qwerty_persistence_mechanism__naturalization_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.18) because the standard provides widespread benefits, and any costs are seen as inherent to coordination or fair market outcomes, not asymmetric extraction. Suppression is low (0.15) as alternatives are understood to have 'lapsed through fair competition,' implying no active coercion. Theater ratio is very low (0.08) as there's little performative maintenance; the system simply functions. Accessibility collapse is moderate (0.65) because while alternatives exist, their practical accessibility is low due to QWERTY's entrenched status, but this is not attributed to active suppression. Resistance is low (0.12) because most users and manufacturers perceive QWERTY as a functional and adequate standard.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of QWERTY users and manufacturers, the layout is a beneficial, stable standard. From the perspective of Dvorak advocates, it represents a missed opportunity for a technically superior alternative. This reading minimizes the 'gap' by asserting QWERTY's adequacy and fair competition, suggesting that the Dvorak perspective is based on a contested empirical claim rather than structural injustice.
 *
 * DIRECTIONALITY LOGIC:
 *   QWERTY users, keyboard manufacturers, and typing tutors are beneficiaries, gaining from standardization and skill transfer (low directionality). Dvorak advocates are 'victims' in the sense that their preferred alternative did not succeed, but this reading attributes their position to market dynamics rather than active extraction, placing their directionality higher but still within a 'fair competition' frame.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    dvorak_technical_superiority,
    'Is the claimed technical superiority of Dvorak (or other alternative layouts) empirically robust and significant enough to warrant widespread adoption, or is its advantage marginal/contested?',
    'Independent, large-scale, long-term ergonomic studies comparing typing efficiency, error rates, and user comfort across QWERTY and alternative layouts, controlling for training effects and user bias.',
    'If Dvorak''s superiority is definitively proven, it would weaken the ''adequacy'' claim of QWERTY and lend more credence to ''lock-in'' or ''extraction'' explanations for its persistence. If its superiority remains marginal or contested, it reinforces the ''naturalization'' reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(dvorak_technical_superiority, empirical, 'Empirical status of alternative keyboard layout advantages.').

omega_variable(
    switching_cost_origin,
    'Are the high switching costs associated with moving away from QWERTY primarily a reflection of genuine skill investment and retraining effort, or are they artificially amplified by market structures, lack of alternative availability, or active suppression of alternatives?',
    'Economic analysis of the true cost of retraining versus the cost of acquiring alternative hardware/software, coupled with market studies on the availability and pricing of non-QWERTY options. Regulatory intervention to mandate alternative layout support could also provide data.',
    'If switching costs are found to be artificially amplified, it would shift the constraint''s classification towards a ''tangled_rope'' or ''snare'' by revealing hidden extraction or suppression. If they are primarily skill-based, it supports the ''rope'' classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(switching_cost_origin, empirical, 'Nature of QWERTY switching costs: skill-based vs. market-amplified.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(qwerty_persistence_mechanism__naturalization_reading, 1873, 2023).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qwer_tr_t1873, qwerty_persistence_mechanism__naturalization_reading, theater_ratio, 1873, 0.05).
narrative_ontology:measurement(qwer_tr_t1900, qwerty_persistence_mechanism__naturalization_reading, theater_ratio, 1900, 0.05).
narrative_ontology:measurement(qwer_tr_t1930, qwerty_persistence_mechanism__naturalization_reading, theater_ratio, 1930, 0.06).
narrative_ontology:measurement(qwer_tr_t1960, qwerty_persistence_mechanism__naturalization_reading, theater_ratio, 1960, 0.07).
narrative_ontology:measurement(qwer_tr_t1990, qwerty_persistence_mechanism__naturalization_reading, theater_ratio, 1990, 0.08).
narrative_ontology:measurement(qwer_tr_t2023, qwerty_persistence_mechanism__naturalization_reading, theater_ratio, 2023, 0.08).

% Extraction over time
narrative_ontology:measurement(qwer_be_t1873, qwerty_persistence_mechanism__naturalization_reading, base_extractiveness, 1873, 0.1).
narrative_ontology:measurement(qwer_be_t1900, qwerty_persistence_mechanism__naturalization_reading, base_extractiveness, 1900, 0.12).
narrative_ontology:measurement(qwer_be_t1930, qwerty_persistence_mechanism__naturalization_reading, base_extractiveness, 1930, 0.14).
narrative_ontology:measurement(qwer_be_t1960, qwerty_persistence_mechanism__naturalization_reading, base_extractiveness, 1960, 0.16).
narrative_ontology:measurement(qwer_be_t1990, qwerty_persistence_mechanism__naturalization_reading, base_extractiveness, 1990, 0.17).
narrative_ontology:measurement(qwer_be_t2023, qwerty_persistence_mechanism__naturalization_reading, base_extractiveness, 2023, 0.18).

% Suppression requirement over time
narrative_ontology:measurement(qwer_su_t1873, qwerty_persistence_mechanism__naturalization_reading, suppression_requirement, 1873, 0.1).
narrative_ontology:measurement(qwer_su_t1900, qwerty_persistence_mechanism__naturalization_reading, suppression_requirement, 1900, 0.11).
narrative_ontology:measurement(qwer_su_t1930, qwerty_persistence_mechanism__naturalization_reading, suppression_requirement, 1930, 0.12).
narrative_ontology:measurement(qwer_su_t1960, qwerty_persistence_mechanism__naturalization_reading, suppression_requirement, 1960, 0.13).
narrative_ontology:measurement(qwer_su_t1990, qwerty_persistence_mechanism__naturalization_reading, suppression_requirement, 1990, 0.14).
narrative_ontology:measurement(qwer_su_t2023, qwerty_persistence_mechanism__naturalization_reading, suppression_requirement, 2023, 0.15).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(qwerty_persistence_mechanism__naturalization_reading, information_standard).
narrative_ontology:affects_constraint(qwerty_persistence_mechanism__naturalization_reading, qwerty_persistence_mechanism__lock_in_reading).
narrative_ontology:affects_constraint(qwerty_persistence_mechanism__naturalization_reading, qwerty_persistence_mechanism__beneficiary_extraction_reading).

% DUAL FORMULATION NOTE:
% This story is one of three readings of the 'QWERTY persistence mechanism' kernel. It presents the 'naturalization' view, where QWERTY's dominance is due to its adequacy and fair competition, in contrast to the 'lock-in' and 'beneficiary extraction' readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
