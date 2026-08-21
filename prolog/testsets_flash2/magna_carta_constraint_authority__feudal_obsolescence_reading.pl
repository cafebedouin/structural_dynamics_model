% ============================================================================
% CONSTRAINT STORY: magna_carta_constraint_authority__feudal_obsolescence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_magna_carta_constraint_authority__feudal_obsolescence_reading, []).

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
 *   constraint_id: magna_carta_constraint_authority__feudal_obsolescence_reading
 *   human_readable: Magna Carta's Feudal Obsolescence (Historical Reading)
 *   domain: constitutional_history/legal_philosophy/political_theory
 *
 * SUMMARY:
 *   This constraint represents the 'feudal obsolescence' reading of Magna
 *   Carta's authority, arguing that it was a specific baronial compact with
 *   no direct binding power over modern sovereignty. This reading serves to
 *   maximize executive and parliamentary discretion by dismissing historical
 *   constitutional limits. The constraint is claimed as a Piton because its
 *   original function has atrophied, and its persistence is largely
 *   performative, serving to justify current power structures rather than
 *   actively constrain them. The metrics reflect a high degree of
 *   theatricality and suppression of alternative interpretations.
 *
 * KEY AGENTS:
 *   - executive_branch: Primary beneficiary (institutional/mobile) — benefits from expanded discretion.
 *   - parliamentary_majority: Secondary beneficiary (institutional/mobile) — benefits from reinforced sovereignty.
 *   - popular_constitutionalists: Primary payer (organized/constrained) — their arguments for inherited rights are undermined.
 *   - juridical_restraint_advocates: Secondary payer (moderate/constrained) — their arguments for judicial limits are weakened.
 *   - historical_scholars: Analytical observer (analytical/analytical) — provide academic basis for this reading.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(magna_carta_constraint_authority__feudal_obsolescence_reading, 0.65).
domain_priors:suppression_score(magna_carta_constraint_authority__feudal_obsolescence_reading, 0.7).
domain_priors:theater_ratio(magna_carta_constraint_authority__feudal_obsolescence_reading, 0.8).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(magna_carta_constraint_authority__feudal_obsolescence_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(magna_carta_constraint_authority__feudal_obsolescence_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(magna_carta_constraint_authority__feudal_obsolescence_reading, theater_ratio, 0.8).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(magna_carta_constraint_authority__feudal_obsolescence_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(magna_carta_constraint_authority__feudal_obsolescence_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(magna_carta_constraint_authority__feudal_obsolescence_reading, piton).
narrative_ontology:human_readable(magna_carta_constraint_authority__feudal_obsolescence_reading, "Magna Carta's Feudal Obsolescence (Historical Reading)").
narrative_ontology:topic_domain(magna_carta_constraint_authority__feudal_obsolescence_reading, "constitutional_history/legal_philosophy/political_theory").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(magna_carta_constraint_authority__feudal_obsolescence_reading, '26f45b46-8a3a-487f-837c-e9966369b329').
narrative_ontology:cs_kernel_codification('26f45b46-8a3a-487f-837c-e9966369b329', fixed_text).
narrative_ontology:cs_authority_grounding('26f45b46-8a3a-487f-837c-e9966369b329', extraction).
narrative_ontology:cs_interpretation_layer_present('26f45b46-8a3a-487f-837c-e9966369b329').
narrative_ontology:cs_reading_relation('26f45b46-8a3a-487f-837c-e9966369b329', magna_carta_constraint_authority__living_constitutionalism_reading, coexists_with).
narrative_ontology:cs_reading_relation('26f45b46-8a3a-487f-837c-e9966369b329', magna_carta_constraint_authority__parliamentary_sovereignty_reading, coexists_with).
narrative_ontology:cs_axiom('26f45b46-8a3a-487f-837c-e9966369b329', foundational, historical_context_determines_authority).
narrative_ontology:cs_axiom_status(historical_context_determines_authority, holdable).
narrative_ontology:cs_axiom_grounding('26f45b46-8a3a-487f-837c-e9966369b329', historical_context_determines_authority, conventional).
narrative_ontology:cs_axiom('26f45b46-8a3a-487f-837c-e9966369b329', foundational, modern_sovereignty_unbound_by_feudal_compacts).
narrative_ontology:cs_axiom_status(modern_sovereignty_unbound_by_feudal_compacts, holdable).
narrative_ontology:cs_axiom_grounding('26f45b46-8a3a-487f-837c-e9966369b329', modern_sovereignty_unbound_by_feudal_compacts, conventional).
narrative_ontology:cs_reference_frame('26f45b46-8a3a-487f-837c-e9966369b329', original_feudal_compact).
narrative_ontology:cs_drift_state('26f45b46-8a3a-487f-837c-e9966369b329', contemporary_legal_discourse, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('26f45b46-8a3a-487f-837c-e9966369b329', '').
narrative_ontology:cs_kernel_id(magna_carta_constraint_authority__feudal_obsolescence_reading, magna_carta_constraint_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(magna_carta_constraint_authority__feudal_obsolescence_reading, executive_branch).
narrative_ontology:constraint_beneficiary(magna_carta_constraint_authority__feudal_obsolescence_reading, parliamentary_majority).
narrative_ontology:constraint_victim(magna_carta_constraint_authority__feudal_obsolescence_reading, popular_constitutionalists).
narrative_ontology:constraint_victim(magna_carta_constraint_authority__feudal_obsolescence_reading, juridical_restraint_advocates).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefits from the interpretation that Magna Carta holds no binding authority over modern executive power, allowing for greater discretion and fewer historical constraints on action. This reading supports a more expansive view of executive prerogative.
narrative_ontology:constraint_stakeholder(magna_carta_constraint_authority__feudal_obsolescence_reading, executive_branch, beneficiary,
    institutional, biographical, mobile, national).

% Benefits from the argument that any enduring principles from Magna Carta have been absorbed into modern statute, which Parliament can amend or repeal. This reinforces parliamentary sovereignty and minimizes historical limitations on legislative action.
narrative_ontology:constraint_stakeholder(magna_carta_constraint_authority__feudal_obsolescence_reading, parliamentary_majority, beneficiary,
    institutional, biographical, mobile, national).

% Bear the cost of this reading, as it undermines their efforts to invoke Magna Carta as a source of fundamental, enduring rights and constitutional principles that limit state power. Their arguments for inherited rights are dismissed as anachronistic.
narrative_ontology:constraint_stakeholder(magna_carta_constraint_authority__feudal_obsolescence_reading, popular_constitutionalists, payer,
    organized, generational, constrained, national).

% Experience a weakening of their arguments for judicial review and constitutional limits on government, as the historical basis for such restraint is declared obsolete. Their ability to appeal to Magna Carta as a source of common law or due process is diminished.
narrative_ontology:constraint_stakeholder(magna_carta_constraint_authority__feudal_obsolescence_reading, juridical_restraint_advocates, payer,
    moderate, generational, constrained, national).

% Analyze Magna Carta within its original 13th-century context, often supporting the view that its direct legal relevance to modern constitutionalism is limited, though its symbolic importance may persist. They provide the academic basis for the 'feudal obsolescence' argument.
narrative_ontology:constraint_stakeholder(magna_carta_constraint_authority__feudal_obsolescence_reading, historical_scholars, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: This reading coordinates a particular understanding of historical legal documents, asserting that their authority is strictly time-bound to their original context, thereby clarifying the scope of modern governmental power.
% TRANSFER_FUNCTION: Transfers interpretive authority over historical documents from broad, evolutionary readings to a narrow, historically contextualized one, effectively maximizing executive and parliamentary discretion by removing ancient constraints.
% ABSENT_VOICES: Advocates for a 'living' constitution or common law tradition, who would argue for the enduring, evolving relevance of Magna Carta's principles, are dismissed as misinterpreting historical intent or anachronistically applying feudal concepts.
% DISAPPEARANCE_RATIONALE: If this specific reading vanished, the actual legal and political structures of modern sovereignty would remain largely unchanged, as this reading primarily serves to justify existing power rather than to constitute it. The debate over Magna Carta's relevance would simply shift to other interpretive frames.
% FOUNDING_PROBLEM: The problem of anachronistically applying ancient feudal compacts to modern constitutional structures, potentially creating unintended limits on contemporary governmental authority.
% FOUNDING_PROBLEM_CORROBORATION: Historical scholars and legal positivists corroborate the view that applying 13th-century feudal law directly to modern states is problematic. The executive and parliamentary majorities, as beneficiaries, also attest to the problem's live status, as it justifies their expanded discretion.
narrative_ontology:disappearance_verdict(magna_carta_constraint_authority__feudal_obsolescence_reading, world_unchanged).
narrative_ontology:founding_problem_status(magna_carta_constraint_authority__feudal_obsolescence_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(magna_carta_constraint_authority__feudal_obsolescence_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(magna_carta_constraint_authority__feudal_obsolescence_reading, 'none', 1).
narrative_ontology:epsilon_provenance(magna_carta_constraint_authority__feudal_obsolescence_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(magna_carta_constraint_authority__feudal_obsolescence_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(magna_carta_constraint_authority__feudal_obsolescence_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(magna_carta_constraint_authority__feudal_obsolescence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The high theater_ratio (0.8) reflects that the constraint's 'authority' is largely performative, invoked to dismiss counter-arguments rather than to actively structure governance. Extractiveness (0.65) is moderate-to-high because it enables the expansion of executive and parliamentary power by removing historical checks. Suppression (0.7) is also high, as alternative, more expansive readings of Magna Carta's authority must be actively suppressed in legal and political discourse. The accessibility_collapse is low (0.3) because alternative readings are readily available, but resistance is moderate (0.4) as these alternatives are actively contested.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the executive and parliamentary beneficiaries, this reading is a correct historical interpretation that clarifies the scope of modern power. From the perspective of popular constitutionalists and juridical restraint advocates, it is an extractive re-interpretation that undermines fundamental rights and checks on power. The engine's classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   The executive branch and parliamentary majority are beneficiaries (low d) as this reading expands their power. Popular constitutionalists and juridical restraint advocates are payers (high d) as their arguments are undermined. Historical scholars are observers (d=0.5) providing an analytical perspective.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading itself is a form of mandatrophy resolution, arguing that Magna Carta's mandate has atrophied due to historical obsolescence. The classification as a Piton reflects that its original function as a constraint on power has largely ceased, and its continued 'existence' in this form is primarily inertial and theatrical, serving to justify the status quo rather than to actively coordinate or extract in a dynamic sense. It prevents mislabeling a defunct historical compact as an active, extractive Snare, by emphasizing the performative aspect of its 'authority'.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    historical_symbolism_vs_legal_force,
    'To what extent does Magna Carta''s enduring symbolic power (as a foundational document for rights) implicitly exert a ''soft'' constraint on modern governance, even if its direct legal force is obsolete?',
    'Analysis of political rhetoric and public opinion during constitutional crises: if appeals to Magna Carta''s spirit demonstrably shift policy or public discourse, it retains a ''soft'' constraining function.',
    'If symbolic power is a ''soft'' constraint, the constraint''s effective extractiveness is lower than measured, as it still imposes some diffuse, non-codified limits on power. If not, the obsolescence reading is fully justified.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(historical_symbolism_vs_legal_force, conceptual, 'Ambiguity between symbolic influence and direct legal authority.').

omega_variable(
    obsolescence_as_justification_for_extraction,
    'Is the ''feudal obsolescence'' reading a genuine historical conclusion, or is it primarily a post-hoc justification for maximizing executive and parliamentary discretion?',
    'Comparative historical analysis of similar foundational documents in other jurisdictions: if a consistent pattern emerges where ''obsolescence'' is invoked precisely when state power expands, it suggests a justificatory rather than purely historical function.',
    'If primarily a justification, the constraint''s extractiveness is higher, and its classification shifts closer to a Snare, as the obsolescence claim itself becomes a tool for extraction. If genuine, the Piton classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(obsolescence_as_justification_for_extraction, empirical, 'Whether the obsolescence claim is descriptive or prescriptive.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(magna_carta_constraint_authority__feudal_obsolescence_reading, 1950, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(magn_tr_t1950, magna_carta_constraint_authority__feudal_obsolescence_reading, theater_ratio, 1950, 0.7).
narrative_ontology:measurement(magn_tr_t1970, magna_carta_constraint_authority__feudal_obsolescence_reading, theater_ratio, 1970, 0.73).
narrative_ontology:measurement(magn_tr_t1990, magna_carta_constraint_authority__feudal_obsolescence_reading, theater_ratio, 1990, 0.76).
narrative_ontology:measurement(magn_tr_t2010, magna_carta_constraint_authority__feudal_obsolescence_reading, theater_ratio, 2010, 0.78).
narrative_ontology:measurement(magn_tr_t2024, magna_carta_constraint_authority__feudal_obsolescence_reading, theater_ratio, 2024, 0.8).

% Extraction over time
narrative_ontology:measurement(magn_be_t1950, magna_carta_constraint_authority__feudal_obsolescence_reading, base_extractiveness, 1950, 0.5).
narrative_ontology:measurement(magn_be_t1970, magna_carta_constraint_authority__feudal_obsolescence_reading, base_extractiveness, 1970, 0.55).
narrative_ontology:measurement(magn_be_t1990, magna_carta_constraint_authority__feudal_obsolescence_reading, base_extractiveness, 1990, 0.6).
narrative_ontology:measurement(magn_be_t2010, magna_carta_constraint_authority__feudal_obsolescence_reading, base_extractiveness, 2010, 0.63).
narrative_ontology:measurement(magn_be_t2024, magna_carta_constraint_authority__feudal_obsolescence_reading, base_extractiveness, 2024, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(magn_su_t1950, magna_carta_constraint_authority__feudal_obsolescence_reading, suppression_requirement, 1950, 0.55).
narrative_ontology:measurement(magn_su_t1970, magna_carta_constraint_authority__feudal_obsolescence_reading, suppression_requirement, 1970, 0.6).
narrative_ontology:measurement(magn_su_t1990, magna_carta_constraint_authority__feudal_obsolescence_reading, suppression_requirement, 1990, 0.65).
narrative_ontology:measurement(magn_su_t2010, magna_carta_constraint_authority__feudal_obsolescence_reading, suppression_requirement, 2010, 0.68).
narrative_ontology:measurement(magn_su_t2024, magna_carta_constraint_authority__feudal_obsolescence_reading, suppression_requirement, 2024, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(magna_carta_constraint_authority__feudal_obsolescence_reading, identity_coordination).
narrative_ontology:affects_constraint(magna_carta_constraint_authority__feudal_obsolescence_reading, living_constitutionalism_reading).
narrative_ontology:affects_constraint(magna_carta_constraint_authority__feudal_obsolescence_reading, parliamentary_sovereignty_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'magna_carta_constraint_authority' kernel. This 'feudal_obsolescence_reading' asserts the document's lack of modern binding authority, contrasting with the 'living_constitutionalism_reading' (evolving relevance) and 'parliamentary_sovereignty_reading' (absorption into statute).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
