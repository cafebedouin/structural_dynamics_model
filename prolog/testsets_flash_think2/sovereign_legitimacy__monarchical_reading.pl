% ============================================================================
% CONSTRAINT STORY: sovereign_legitimacy__monarchical_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sovereign_legitimacy__monarchical_reading, []).

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
 *   constraint_id: sovereign_legitimacy__monarchical_reading
 *   human_readable: Monarchical Legitimacy by Inherited Right
 *   domain: Political Philosophy/Constitutional Theory/Legitimacy Studies
 *
 * SUMMARY:
 *   This constraint story instantiates the 'monarchical_reading' of the
 *   'sovereign_legitimacy' kernel. It describes a system where political
 *   authority is claimed to flow downward from a sovereign through inherited
 *   right, grounded in divine sanction, tradition, and bloodline continuity.
 *   This reading emphasizes stability through clear succession and a unified
 *   symbol of national identity, but it entails high extraction from subjects
 *   and active suppression of alternative legitimacy claims. The metrics
 *   reflect the coercive and extractive nature of this system, despite its
 *   claimed coordination function.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sovereign_legitimacy__monarchical_reading, 0.85).
domain_priors:suppression_score(sovereign_legitimacy__monarchical_reading, 0.9).
domain_priors:theater_ratio(sovereign_legitimacy__monarchical_reading, 0.6).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sovereign_legitimacy__monarchical_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(sovereign_legitimacy__monarchical_reading, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(sovereign_legitimacy__monarchical_reading, theater_ratio, 0.6).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(sovereign_legitimacy__monarchical_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(sovereign_legitimacy__monarchical_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sovereign_legitimacy__monarchical_reading, tangled_rope).
narrative_ontology:human_readable(sovereign_legitimacy__monarchical_reading, "Monarchical Legitimacy by Inherited Right").
narrative_ontology:topic_domain(sovereign_legitimacy__monarchical_reading, "Political Philosophy/Constitutional Theory/Legitimacy Studies").

domain_priors:requires_active_enforcement(sovereign_legitimacy__monarchical_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(sovereign_legitimacy__monarchical_reading, '53a8c840-5924-4f7b-be71-c7ea61632803').
narrative_ontology:cs_kernel_codification('53a8c840-5924-4f7b-be71-c7ea61632803', formalized).
narrative_ontology:cs_authority_grounding('53a8c840-5924-4f7b-be71-c7ea61632803', lineage).
narrative_ontology:cs_interpretation_layer_present('53a8c840-5924-4f7b-be71-c7ea61632803').
narrative_ontology:cs_reading_relation('53a8c840-5924-4f7b-be71-c7ea61632803', sovereign_legitimacy__republican_reading, forecloses).
narrative_ontology:cs_reading_relation('53a8c840-5924-4f7b-be71-c7ea61632803', sovereign_legitimacy__constitutional_hybrid_reading, forecloses).
narrative_ontology:cs_axiom('53a8c840-5924-4f7b-be71-c7ea61632803', foundational, divine_right_of_kings).
narrative_ontology:cs_axiom_status(divine_right_of_kings, holdable).
narrative_ontology:cs_axiom_grounding('53a8c840-5924-4f7b-be71-c7ea61632803', divine_right_of_kings, theological).
narrative_ontology:cs_axiom('53a8c840-5924-4f7b-be71-c7ea61632803', foundational, unbroken_bloodline_legitimacy).
narrative_ontology:cs_axiom_status(unbroken_bloodline_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('53a8c840-5924-4f7b-be71-c7ea61632803', unbroken_bloodline_legitimacy, conventional).
narrative_ontology:cs_reference_frame('53a8c840-5924-4f7b-be71-c7ea61632803', divine_monarchical_order).
narrative_ontology:cs_drift_state('53a8c840-5924-4f7b-be71-c7ea61632803', contemporary_democratic_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('53a8c840-5924-4f7b-be71-c7ea61632803', '').
narrative_ontology:cs_kernel_id(sovereign_legitimacy__monarchical_reading, sovereign_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sovereign_legitimacy__monarchical_reading, hereditary_ruling_class).
narrative_ontology:constraint_beneficiary(sovereign_legitimacy__monarchical_reading, aristocratic_hierarchy).
narrative_ontology:constraint_beneficiary(sovereign_legitimacy__monarchical_reading, divine_authority_interpreters).
narrative_ontology:constraint_victim(sovereign_legitimacy__monarchical_reading, subjects_excluded_from_authority).
narrative_ontology:constraint_victim(sovereign_legitimacy__monarchical_reading, republican_advocates).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Holds ultimate political authority and claims legitimacy through inherited right and divine sanction. Benefits directly from the concentration of power, wealth, and status. Actively enforces the system against challenges.
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__monarchical_reading, hereditary_ruling_class, agenda_setter,
    institutional, generational, arbitrage, national).

% Benefits from proximity to the sovereign, holding privileged positions, land, and influence. Their status is tied to the persistence of the monarchical system, making exit or challenge costly.
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__monarchical_reading, aristocratic_hierarchy, beneficiary,
    powerful, generational, constrained, national).

% Bear the costs of governance without direct participation or consent. Their lives are subject to the sovereign's will, and alternatives to the system are suppressed, leaving them with limited recourse.
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__monarchical_reading, subjects_excluded_from_authority, payer,
    powerless, biographical, trapped, national).

% Actively challenge the legitimacy of inherited rule, advocating for popular sovereignty and elected representation. They face suppression, imprisonment, or exile for their efforts, making their 'exit' from the system's logic a high-stakes endeavor.
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__monarchical_reading, republican_advocates, excluded,
    organized, biographical, constrained, national).

% Religious institutions or figures who interpret divine will as sanctioning the monarch's rule. They gain influence, resources, and protection from the state in exchange for legitimizing the sovereign's authority.
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__monarchical_reading, divine_authority_interpreters, beneficiary,
    institutional, generational, constrained, global).

% Analyze the historical development, theoretical underpinnings, and practical consequences of monarchical legitimacy. They can identify patterns of extraction and suppression but have no direct power to alter the constraint.
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__monarchical_reading, historical_scholars, observer,
    analytical, generational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(sovereign_legitimacy__monarchical_reading, hereditary_ruling_class).
narrative_ontology:fixing_cost_class(sovereign_legitimacy__monarchical_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a clear, stable, and universally recognized line of succession, preventing internal power struggles and ensuring continuity of governance, while also serving as a unifying symbol of national identity and tradition.
% TRANSFER_FUNCTION: Transfers political power, economic resources (e.g., taxation, land ownership), and social status from the general populace to the hereditary ruling class and its associated aristocratic hierarchy, in exchange for perceived stability and order.
% ABSENT_VOICES: Republican and democratic advocates, along with any groups seeking self-determination or popular sovereignty, are structurally excluded. They would argue for legitimacy derived from consent of the governed and elected representation, but their claims are actively suppressed.
% DISAPPEARANCE_RATIONALE: If the principle of monarchical legitimacy vanished overnight, the entire political and social order would be destabilized. The absence of a clear successor would likely lead to power vacuums, civil strife, and a fundamental reorganization of governance and national identity.
% FOUNDING_PROBLEM: To establish an unquestionable and stable source of authority to prevent civil strife, ensure orderly succession, and maintain social cohesion in a pre-democratic era.
% FOUNDING_PROBLEM_CORROBORATION: Proponents (hereditary ruling class, traditionalists) claim the problem of instability and disunity is still live, requiring the monarchical solution. Opponents (republican advocates, many modern historians) argue that the founding problem is largely solved by alternative governance models, and the arrangement persists primarily for the benefit of the ruling class; historical records of popular uprisings and philosophical critiques from outside the benefiting parties support this view.
narrative_ontology:disappearance_verdict(sovereign_legitimacy__monarchical_reading, world_rearranges).
narrative_ontology:founding_problem_status(sovereign_legitimacy__monarchical_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(sovereign_legitimacy__monarchical_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(sovereign_legitimacy__monarchical_reading, 'none', 1).
narrative_ontology:epsilon_provenance(sovereign_legitimacy__monarchical_reading, 0.85, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sovereign_legitimacy__monarchical_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(sovereign_legitimacy__monarchical_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(sovereign_legitimacy__monarchical_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.85) because the system concentrates power and wealth in a hereditary class without popular consent or accountability. Suppression is very high (0.90) as the system's persistence relies on actively repressing republican or democratic movements and alternative claims to authority. Theater ratio is moderate-high (0.60) because elaborate rituals, ceremonies, and traditional narratives are crucial for maintaining the illusion of natural or divine right, often masking the underlying coercive force. Accessibility collapse is high (0.70) as alternatives are systematically dismantled or made extremely costly. Resistance is moderate (0.45) reflecting historical instances of popular uprisings and philosophical challenges to monarchical rule.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the hereditary ruling class, this system is a legitimate and necessary framework for order and stability, a 'Tangled Rope' that coordinates society. From the perspective of the subjects and republican advocates, it is a 'Snare' that extracts resources and suppresses freedom under the guise of tradition and divine will. The engine's computation of per-seat classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   The hereditary ruling class and aristocratic hierarchy are clear beneficiaries, collecting power and wealth. Divine authority interpreters also benefit by having their religious claims validated and protected by the state. Subjects excluded from authority and republican advocates are the primary targets, bearing the costs of extraction and suppression. The system is designed to subsidize the ruling class while extracting from the governed.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    divine_sanction_empirical_status,
    'Is the claim of divine sanction for monarchical rule an empirically verifiable fact, a theological assertion, or a conventional justification?',
    'Analysis of historical and theological texts, and the role of religious institutions in legitimizing power. If it functions purely as a theological assertion or social convention, its ''naturalness'' claim is weakened.',
    'If divine sanction is purely conventional, the constraint''s ''naturalness'' is further undermined, increasing its effective extractiveness and suppression. If it''s treated as an empirical claim, its falsifiability could lead to reclassification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(divine_sanction_empirical_status, conceptual, 'Ambiguity regarding the epistemic status of divine sanction as a source of legitimacy.').

omega_variable(
    succession_stability_vs_contest,
    'Does inherited right genuinely prevent civil strife and ensure stability, or does it merely shift conflict to succession contests and dynastic wars?',
    'Comparative historical analysis of states with inherited vs. elected leadership, focusing on periods of transition and internal conflict. Empirical data on the frequency and severity of succession crises.',
    'If inherited right is shown to frequently lead to violent succession contests, the ''coordination function'' of the constraint is weakened, increasing its effective extractiveness and potentially reclassifying it closer to a Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(succession_stability_vs_contest, empirical, 'Whether inherited right delivers on its promise of stable succession or merely reconfigures conflict.').

omega_variable(
    legitimacy_source_ambiguity,
    'Is the monarch''s authority truly inherited and divinely bestowed, or is it primarily maintained by active suppression of alternative legitimacy claims and the theatrical performance of tradition?',
    'Analysis of state expenditures on propaganda, censorship, and military/police forces used to quell dissent, compared to the actual popular belief in divine right. Post-transition analysis of former monarchies.',
    'If the latter, the constraint''s ''theater_ratio'' and ''suppression'' metrics are even more central to its persistence, confirming a higher degree of extraction and a stronger Snare-like character.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(legitimacy_source_ambiguity, empirical, 'The true source of monarchical authority: inherent right vs. active maintenance.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sovereign_legitimacy__monarchical_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sove_tr_t0, sovereign_legitimacy__monarchical_reading, theater_ratio, 0, 0.45).
narrative_ontology:measurement(sove_tr_t20, sovereign_legitimacy__monarchical_reading, theater_ratio, 20, 0.5).
narrative_ontology:measurement(sove_tr_t40, sovereign_legitimacy__monarchical_reading, theater_ratio, 40, 0.55).
narrative_ontology:measurement(sove_tr_t60, sovereign_legitimacy__monarchical_reading, theater_ratio, 60, 0.58).
narrative_ontology:measurement(sove_tr_t80, sovereign_legitimacy__monarchical_reading, theater_ratio, 80, 0.59).
narrative_ontology:measurement(sove_tr_t100, sovereign_legitimacy__monarchical_reading, theater_ratio, 100, 0.6).

% Extraction over time
narrative_ontology:measurement(sove_be_t0, sovereign_legitimacy__monarchical_reading, base_extractiveness, 0, 0.75).
narrative_ontology:measurement(sove_be_t20, sovereign_legitimacy__monarchical_reading, base_extractiveness, 20, 0.78).
narrative_ontology:measurement(sove_be_t40, sovereign_legitimacy__monarchical_reading, base_extractiveness, 40, 0.81).
narrative_ontology:measurement(sove_be_t60, sovereign_legitimacy__monarchical_reading, base_extractiveness, 60, 0.83).
narrative_ontology:measurement(sove_be_t80, sovereign_legitimacy__monarchical_reading, base_extractiveness, 80, 0.84).
narrative_ontology:measurement(sove_be_t100, sovereign_legitimacy__monarchical_reading, base_extractiveness, 100, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(sove_su_t0, sovereign_legitimacy__monarchical_reading, suppression_requirement, 0, 0.8).
narrative_ontology:measurement(sove_su_t20, sovereign_legitimacy__monarchical_reading, suppression_requirement, 20, 0.83).
narrative_ontology:measurement(sove_su_t40, sovereign_legitimacy__monarchical_reading, suppression_requirement, 40, 0.86).
narrative_ontology:measurement(sove_su_t60, sovereign_legitimacy__monarchical_reading, suppression_requirement, 60, 0.88).
narrative_ontology:measurement(sove_su_t80, sovereign_legitimacy__monarchical_reading, suppression_requirement, 80, 0.89).
narrative_ontology:measurement(sove_su_t100, sovereign_legitimacy__monarchical_reading, suppression_requirement, 100, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sovereign_legitimacy__monarchical_reading, identity_coordination).
narrative_ontology:affects_constraint(sovereign_legitimacy__monarchical_reading, feudal_land_tenure).
narrative_ontology:affects_constraint(sovereign_legitimacy__monarchical_reading, aristocratic_privilege).
narrative_ontology:affects_constraint(sovereign_legitimacy__monarchical_reading, state_church_establishment).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'sovereign_legitimacy' kernel, focusing on inherited monarchical authority. It is distinct from 'republican_reading' and 'constitutional_hybrid_reading' which offer alternative sources and structures of legitimate authority.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
