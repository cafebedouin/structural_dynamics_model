% ============================================================================
% CONSTRAINT STORY: sovereign_legitimacy__republican_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sovereign_legitimacy__republican_reading, []).

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
 *   constraint_id: sovereign_legitimacy__republican_reading
 *   human_readable: Republican Popular Sovereignty and Delegated Consent
 *   domain: political_philosophy/constitutional_theory/legitimacy_studies
 *
 * SUMMARY:
 *   This constraint describes the republican reading of legitimate authority,
 *   where power originates from the people and is delegated through consent.
 *   It is grounded in popular sovereignty and social contract theory. The
 *   constraint functions as a Rope, providing a framework for stable
 *   governance, but exhibits moderate extractiveness due to the exclusion of
 *   some groups from full participation and the potential for majoritarian
 *   tyranny over minorities. Its legitimacy requires ongoing validation
 *   through electoral cycles and constitutional adherence.
 *
 * KEY AGENTS:
 *   - citizenry_with_franchise: Primary beneficiary (organized/constrained) — source of authority
 *   - elected_representatives: Agenda setter (institutional/constrained) — exercises delegated authority
 *   - excluded_from_franchise: Primary payer (powerless/trapped) — bears costs without consent
 *   - political_minorities: Payer (moderate/constrained) — subject to majoritarian decisions
 *   - constitutional_scholars: Analytical observer (analytical/analytical) — analyzes theoretical and practical application
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sovereign_legitimacy__republican_reading, 0.45).
domain_priors:suppression_score(sovereign_legitimacy__republican_reading, 0.3).
domain_priors:theater_ratio(sovereign_legitimacy__republican_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sovereign_legitimacy__republican_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(sovereign_legitimacy__republican_reading, suppression_requirement, 0.3).
narrative_ontology:constraint_metric(sovereign_legitimacy__republican_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(sovereign_legitimacy__republican_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(sovereign_legitimacy__republican_reading, resistance, 0.25).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sovereign_legitimacy__republican_reading, rope).
narrative_ontology:human_readable(sovereign_legitimacy__republican_reading, "Republican Popular Sovereignty and Delegated Consent").
narrative_ontology:topic_domain(sovereign_legitimacy__republican_reading, "political_philosophy/constitutional_theory/legitimacy_studies").

domain_priors:requires_active_enforcement(sovereign_legitimacy__republican_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(sovereign_legitimacy__republican_reading, 'a8b8701b-d0b9-4336-86bd-6bd8be3aafdf').
narrative_ontology:cs_kernel_codification('a8b8701b-d0b9-4336-86bd-6bd8be3aafdf', formalized).
narrative_ontology:cs_authority_grounding('a8b8701b-d0b9-4336-86bd-6bd8be3aafdf', lineage).
narrative_ontology:cs_interpretation_layer_present('a8b8701b-d0b9-4336-86bd-6bd8be3aafdf').
narrative_ontology:cs_reading_relation('a8b8701b-d0b9-4336-86bd-6bd8be3aafdf', sovereign_legitimacy__monarchical_reading, forecloses).
narrative_ontology:cs_reading_relation('a8b8701b-d0b9-4336-86bd-6bd8be3aafdf', sovereign_legitimacy__constitutional_hybrid_reading, coexists_with).
narrative_ontology:cs_axiom('a8b8701b-d0b9-4336-86bd-6bd8be3aafdf', foundational, popular_sovereignty_is_foundational).
narrative_ontology:cs_axiom_status(popular_sovereignty_is_foundational, holdable).
narrative_ontology:cs_axiom_grounding('a8b8701b-d0b9-4336-86bd-6bd8be3aafdf', popular_sovereignty_is_foundational, deontological).
narrative_ontology:cs_axiom('a8b8701b-d0b9-4336-86bd-6bd8be3aafdf', foundational, delegated_consent_is_legitimacy_source).
narrative_ontology:cs_axiom_status(delegated_consent_is_legitimacy_source, holdable).
narrative_ontology:cs_axiom_grounding('a8b8701b-d0b9-4336-86bd-6bd8be3aafdf', delegated_consent_is_legitimacy_source, conventional).
narrative_ontology:cs_reference_frame('a8b8701b-d0b9-4336-86bd-6bd8be3aafdf', enlightenment_social_contract).
narrative_ontology:cs_drift_state('a8b8701b-d0b9-4336-86bd-6bd8be3aafdf', contemporary_political_polarization, gap(authority_erosion, substantial, true)).
narrative_ontology:cs_created_at('a8b8701b-d0b9-4336-86bd-6bd8be3aafdf', '').
narrative_ontology:cs_kernel_id(sovereign_legitimacy__republican_reading, sovereign_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sovereign_legitimacy__republican_reading, citizenry_with_franchise).
narrative_ontology:constraint_beneficiary(sovereign_legitimacy__republican_reading, elected_representatives).
narrative_ontology:constraint_victim(sovereign_legitimacy__republican_reading, excluded_from_franchise).
narrative_ontology:constraint_victim(sovereign_legitimacy__republican_reading, political_minorities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The ultimate source of legitimate authority, delegating power through elections and participatory mechanisms. Benefits from self-governance and accountability, but can be subject to majoritarian decisions.
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__republican_reading, citizenry_with_franchise, beneficiary,
    organized, generational, constrained, national).

% Exercise authority delegated by the citizenry, responsible for governance and policy-making. Their legitimacy is derived from periodic electoral validation and adherence to constitutional principles.
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__republican_reading, elected_representatives, agenda_setter,
    institutional, biographical, constrained, national).

% Subject to laws and policies enacted by a government they had no direct role in electing or consenting to. Bears the costs of governance without the benefit of direct political participation.
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__republican_reading, excluded_from_franchise, payer,
    powerless, generational, trapped, national).

% While possessing franchise, their preferences may be consistently overridden by majoritarian rule, leading to a sense of disempowerment and bearing costs without proportional influence.
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__republican_reading, political_minorities, payer,
    moderate, biographical, constrained, national).

% Analyze the theoretical underpinnings and practical application of popular sovereignty and delegated consent, assessing its coherence and effectiveness in generating legitimate authority.
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__republican_reading, constitutional_scholars, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a stable and widely accepted basis for political authority and governance by channeling collective will into legitimate decision-making processes, preventing arbitrary rule.
% TRANSFER_FUNCTION: Transfers the right to govern from the collective 'people' to specific elected officials, in exchange for accountability and representation. It also transfers the obligation to obey laws from citizens to the state.
% ABSENT_VOICES: Those who advocate for alternative forms of legitimacy (e.g., monarchists, anarchists) are excluded from the foundational discourse, as their premises are incompatible with popular sovereignty. Also, future generations whose consent cannot be directly obtained.
% DISAPPEARANCE_RATIONALE: If the principle of popular sovereignty and delegated consent vanished, the entire edifice of modern republican governance would collapse. Elections would lose their meaning, governments would lack a recognized basis for authority, and widespread civil unrest or a shift to authoritarian rule would be highly probable.
% FOUNDING_PROBLEM: To establish a stable and just form of government that avoids both tyranny and anarchy, deriving its authority from the governed rather than from divine right or brute force.
% FOUNDING_PROBLEM_CORROBORATION: Historians and political theorists widely corroborate the problem of establishing legitimate authority as a central challenge in political philosophy. Contemporary political scientists and international observers attest to the ongoing relevance of popular sovereignty in assessing the legitimacy of states, particularly in transitions to democracy.
narrative_ontology:disappearance_verdict(sovereign_legitimacy__republican_reading, world_rearranges).
narrative_ontology:founding_problem_status(sovereign_legitimacy__republican_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(sovereign_legitimacy__republican_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(sovereign_legitimacy__republican_reading, 'none', 1).
narrative_ontology:epsilon_provenance(sovereign_legitimacy__republican_reading, 0.45, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sovereign_legitimacy__republican_reading_tests).
:- end_tests(sovereign_legitimacy__republican_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.45) reflects the costs borne by those excluded from the franchise or by political minorities whose consent is overridden by the majority. Suppression (0.30) is relatively low, as the system ideally relies on voluntary consent, but it exists in the form of legal barriers to participation or the suppression of dissenting voices. Theater ratio (0.10) is low, indicating that the core function of legitimizing authority is largely genuine, though performative aspects may arise during elections. The historical measurements show fluctuations in extractiveness and suppression, often correlating with periods of franchise expansion (decreasing extraction/suppression) and periods of political polarization or democratic backsliding (increasing extraction/suppression).
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the citizenry with franchise, this constraint is a pure Rope, enabling self-governance. For those excluded from the franchise or political minorities, it functions more like a Snare or Tangled Rope, extracting compliance without full consent or representation. The engine's per-seat classification will capture this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   The citizenry with franchise and elected representatives are beneficiaries, as the system empowers them and provides a framework for their political agency. Those excluded from the franchise and political minorities are payers, as they are subject to the authority without full participation or protection from majoritarian decisions. The directionality for these groups will reflect their structural positions.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate to provide legitimate governance through popular consent remains live. Mandatrophy is prevented by the ongoing requirement for electoral validation and the potential for resistance from those whose consent is not genuinely obtained. The classification as a Rope (claimed) with moderate extractiveness (measured) highlights the tension between its ideal function and its real-world operation, especially concerning inclusivity.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    scope_of_the_people,
    'Who constitutes ''the people'' from whom legitimate authority flows, and how is this definition contested or expanded over time?',
    'Historical analysis of franchise expansion, legal rulings on citizenship and voting rights, and sociological studies of political inclusion/exclusion.',
    'A narrower definition of ''the people'' increases extractiveness and suppression for excluded groups, potentially shifting the constraint towards a Snare for them. A broader definition reduces these, moving it closer to a Rope for all.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(scope_of_the_people, empirical, 'Ambiguity in the definition of ''the people'' as the source of sovereignty.').

omega_variable(
    majoritarian_tyranny_risk,
    'To what extent does delegated consent, in practice, protect minority rights against majoritarian decisions, or does it enable ''tyranny of the majority''?',
    'Analysis of constitutional protections for minorities, judicial review outcomes, and empirical studies of policy impacts on minority groups.',
    'If majoritarian tyranny is a significant and unmitigated risk, the constraint''s extractiveness for political minorities is higher than currently estimated, potentially reclassifying their seat as a Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(majoritarian_tyranny_risk, empirical, 'The balance between majority rule and minority rights within a system of delegated consent.').

omega_variable(
    kernel_reading_difference,
    'How would the classification of this constraint change if viewed through the lens of the monarchical or constitutional_hybrid readings of sovereign legitimacy?',
    'Generate separate constraint stories for the monarchical_reading and constitutional_hybrid_reading, comparing their metrics and classifications.',
    'The monarchical reading would likely yield a Snare or Tangled Rope due to downward flow of authority and high suppression. The constitutional_hybrid reading would likely be a Tangled Rope, balancing inherited and delegated authority.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_difference, conceptual, 'This constraint is one reading of the ''sovereign_legitimacy'' kernel; other readings would yield different classifications.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sovereign_legitimacy__republican_reading, 1776, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sove_tr_t1776, sovereign_legitimacy__republican_reading, theater_ratio, 1776, 0.05).
narrative_ontology:measurement(sove_tr_t1850, sovereign_legitimacy__republican_reading, theater_ratio, 1850, 0.1).
narrative_ontology:measurement(sove_tr_t1920, sovereign_legitimacy__republican_reading, theater_ratio, 1920, 0.08).
narrative_ontology:measurement(sove_tr_t1965, sovereign_legitimacy__republican_reading, theater_ratio, 1965, 0.05).
narrative_ontology:measurement(sove_tr_t2000, sovereign_legitimacy__republican_reading, theater_ratio, 2000, 0.08).
narrative_ontology:measurement(sove_tr_t2024, sovereign_legitimacy__republican_reading, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(sove_be_t1776, sovereign_legitimacy__republican_reading, base_extractiveness, 1776, 0.3).
narrative_ontology:measurement(sove_be_t1850, sovereign_legitimacy__republican_reading, base_extractiveness, 1850, 0.4).
narrative_ontology:measurement(sove_be_t1920, sovereign_legitimacy__republican_reading, base_extractiveness, 1920, 0.35).
narrative_ontology:measurement(sove_be_t1965, sovereign_legitimacy__republican_reading, base_extractiveness, 1965, 0.25).
narrative_ontology:measurement(sove_be_t2000, sovereign_legitimacy__republican_reading, base_extractiveness, 2000, 0.4).
narrative_ontology:measurement(sove_be_t2024, sovereign_legitimacy__republican_reading, base_extractiveness, 2024, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(sove_su_t1776, sovereign_legitimacy__republican_reading, suppression_requirement, 1776, 0.5).
narrative_ontology:measurement(sove_su_t1850, sovereign_legitimacy__republican_reading, suppression_requirement, 1850, 0.45).
narrative_ontology:measurement(sove_su_t1920, sovereign_legitimacy__republican_reading, suppression_requirement, 1920, 0.3).
narrative_ontology:measurement(sove_su_t1965, sovereign_legitimacy__republican_reading, suppression_requirement, 1965, 0.2).
narrative_ontology:measurement(sove_su_t2000, sovereign_legitimacy__republican_reading, suppression_requirement, 2000, 0.25).
narrative_ontology:measurement(sove_su_t2024, sovereign_legitimacy__republican_reading, suppression_requirement, 2024, 0.3).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sovereign_legitimacy__republican_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(sovereign_legitimacy__republican_reading, constitutional_law_adherence).
narrative_ontology:affects_constraint(sovereign_legitimacy__republican_reading, electoral_system_integrity).

% DUAL FORMULATION NOTE:
% This constraint is part of the 'sovereign_legitimacy' constraint family, which includes 'monarchical_reading' and 'constitutional_hybrid_reading'. Each represents a distinct structural claim about the source of legitimate authority.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
