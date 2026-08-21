% ============================================================================
% CONSTRAINT STORY: basic_law_interpretive_boundary__parliamentary_sovereignty_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_basic_law_interpretive_boundary__parliamentary_sovereignty_reading, []).

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
 *   constraint_id: basic_law_interpretive_boundary__parliamentary_sovereignty_reading
 *   human_readable: Knesset's Ultimate Interpretive Authority over Basic Laws (Parliamentary Sovereignty Reading)
 *   domain: Constitutional Law / Comparative Constitutionalism / Judicial Review Theory
 *
 * SUMMARY:
 *   This constraint describes the 'parliamentary sovereignty' reading of the
 *   basic_law_interpretive_boundary kernel. It asserts the Knesset's ultimate
 *   authority to interpret and amend Basic Laws via simple majority,
 *   including the power to override judicial review. This reading positions
 *   the Knesset as the unconstrained sovereign, with the judiciary holding
 *   only advisory power and no external veto on legislative will. The
 *   constraint itself, as a mechanism for concentrating power, is highly
 *   extractive and suppressive.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, 0.85).
domain_priors:suppression_score(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, 0.9).
domain_priors:theater_ratio(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, accessibility_collapse, 0.95).
narrative_ontology:constraint_metric(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, snare).
narrative_ontology:human_readable(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, "Knesset's Ultimate Interpretive Authority over Basic Laws (Parliamentary Sovereignty Reading)").
narrative_ontology:topic_domain(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, "Constitutional Law / Comparative Constitutionalism / Judicial Review Theory").

domain_priors:requires_active_enforcement(basic_law_interpretive_boundary__parliamentary_sovereignty_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, '33b69faa-6561-46ff-803e-35b4dc316bb8').
narrative_ontology:cs_kernel_codification('33b69faa-6561-46ff-803e-35b4dc316bb8', formalized).
narrative_ontology:cs_authority_grounding('33b69faa-6561-46ff-803e-35b4dc316bb8', lineage).
narrative_ontology:cs_interpretation_layer_present('33b69faa-6561-46ff-803e-35b4dc316bb8').
narrative_ontology:cs_reading_relation('33b69faa-6561-46ff-803e-35b4dc316bb8', basic_law_interpretive_boundary__judicial_supremacy_reading, forecloses).
narrative_ontology:cs_reading_relation('33b69faa-6561-46ff-803e-35b4dc316bb8', basic_law_interpretive_boundary__balanced_contestation_reading, forecloses).
narrative_ontology:cs_axiom('33b69faa-6561-46ff-803e-35b4dc316bb8', foundational, parliamentary_supremacy_is_absolute).
narrative_ontology:cs_axiom_status(parliamentary_supremacy_is_absolute, holdable).
narrative_ontology:cs_axiom_grounding('33b69faa-6561-46ff-803e-35b4dc316bb8', parliamentary_supremacy_is_absolute, conventional).
narrative_ontology:cs_axiom('33b69faa-6561-46ff-803e-35b4dc316bb8', foundational, basic_laws_are_ordinary_legislation).
narrative_ontology:cs_axiom_status(basic_laws_are_ordinary_legislation, holdable).
narrative_ontology:cs_axiom_grounding('33b69faa-6561-46ff-803e-35b4dc316bb8', basic_laws_are_ordinary_legislation, conventional).
narrative_ontology:cs_reference_frame('33b69faa-6561-46ff-803e-35b4dc316bb8', unfettered_parliamentary_sovereignty).
narrative_ontology:cs_drift_state('33b69faa-6561-46ff-803e-35b4dc316bb8', contemporary_judicial_activism_era, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('33b69faa-6561-46ff-803e-35b4dc316bb8', '').
narrative_ontology:cs_kernel_id(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, basic_law_interpretive_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, knesset_majority).
narrative_ontology:constraint_beneficiary(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, governing_coalition).
narrative_ontology:constraint_victim(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, supreme_court).
narrative_ontology:constraint_victim(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, minority_rights_advocates).
narrative_ontology:constraint_victim(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, civil_society_organizations).
narrative_ontology:constraint_vindicates(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, parliamentary_supremacy_doctrine).
narrative_ontology:constraint_vindicates(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, majoritarian_democracy_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Directly exercises the ultimate authority to interpret and amend Basic Laws, ensuring its legislative agenda can proceed without judicial impediment. Benefits from the absence of external legal vetoes.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, knesset_majority, agenda_setter,
    institutional, biographical, mobile, national).

% Benefits from the unconstrained legislative power, allowing it to implement its policy platform and interpret constitutional principles in line with its political mandate without fear of judicial invalidation.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, governing_coalition, beneficiary,
    institutional, biographical, mobile, national).

% Its power of judicial review over Basic Laws is rendered advisory or nullified, losing its capacity to act as a check on legislative power. This represents a significant extraction of its institutional authority and function.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, supreme_court, payer,
    institutional, generational, constrained, national).

% Lose a key avenue for protecting minority rights against majoritarian legislation, as judicial review is weakened or eliminated. Their ability to challenge laws on constitutional grounds is severely curtailed.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, minority_rights_advocates, payer,
    organized, generational, constrained, national).

% Their advocacy for constitutional checks and balances and the rule of law is undermined. They face a legislative branch with ultimate interpretive authority, reducing avenues for legal challenge and public accountability.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, civil_society_organizations, payer,
    organized, biographical, constrained, national).

% Analyze the implications of this constitutional arrangement for democratic theory, human rights, and comparative constitutionalism. They have no direct power to influence the constraint's operation but provide critical external commentary.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, international_legal_scholars, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a clear, final authority for interpreting and amending Basic Laws, ensuring legislative certainty and efficiency for the governing majority and preventing judicial obstruction of the popular will.
% TRANSFER_FUNCTION: Transfers ultimate interpretive and legislative power from a potentially shared domain (legislature + judiciary) solely to the Knesset, effectively extracting power from the judiciary and diminishing protections for minority rights.
% ABSENT_VOICES: Constitutional scholars advocating for robust checks and balances, and international human rights bodies, whose perspectives on judicial independence and minority protections are structurally sidelined by this interpretation. They would argue for a more constrained legislative power.
% DISAPPEARANCE_RATIONALE: If the Knesset's ultimate interpretive authority vanished, the Supreme Court's power of judicial review would immediately become binding, leading to a rebalancing of power, potential invalidation of past legislation, and a new era of constitutional contestation where judicial decisions hold final sway.
% FOUNDING_PROBLEM: To establish a clear, democratic source of ultimate legal authority in a state without a formal, entrenched constitution, ensuring the elected legislature's will is supreme and not subject to unelected judicial vetoes.
% FOUNDING_PROBLEM_CORROBORATION: Proponents within the Knesset and its legal advisors attest the problem is still live, citing the need for legislative efficiency and democratic accountability. Opponents, including former Supreme Court justices, legal academics, and civil society groups, attest the founding problem is substantially solved by existing Basic Laws and the arrangement now serves to consolidate power, as evidenced by legal challenges and public protests.
narrative_ontology:disappearance_verdict(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, world_rearranges).
narrative_ontology:founding_problem_status(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, 'none', 1).
narrative_ontology:epsilon_provenance(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, 0.85, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(basic_law_interpretive_boundary__parliamentary_sovereignty_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(basic_law_interpretive_boundary__parliamentary_sovereignty_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness is high (0.85) because this interpretation fundamentally extracts power from the judiciary and diminishes protections for minority rights, concentrating it in the legislative majority. Suppression is also very high (0.90) as it actively suppresses judicial review and any other external checks on the Knesset's interpretation, collapsing alternatives to parliamentary supremacy. Theater ratio is low (0.10) because this is an active, asserted, and defended claim of power, not a performative maintenance of an atrophied function. Resistance is high (0.70) due to ongoing contestation from the Supreme Court, legal scholars, and civil society.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the Knesset majority, this constraint is a legitimate expression of democratic will and legislative efficiency, ensuring the elected body's supremacy. From the perspective of the Supreme Court, minority rights advocates, and civil society, it is an overreach that erodes checks and balances, leading to a concentration of power and potential for abuse. The engine will compute these divergent classifications based on the declared structural relationships.
 *
 * DIRECTIONALITY LOGIC:
 *   The Knesset majority and governing coalition are clear beneficiaries (d near 0.0) as the constraint directly empowers them and removes obstacles to their legislative agenda. The Supreme Court, minority rights advocates, and civil society organizations are clear targets (d near 1.0) as their institutional power, rights protections, and advocacy avenues are extracted or suppressed by this interpretation. International legal scholars are analytical observers.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is not subject to mandatrophy in the traditional sense, as its function (asserting parliamentary sovereignty) is actively pursued and contested. The question is not whether its mandate has atrophied, but whether its asserted mandate is legitimate or extractive. The high extractiveness and suppression, coupled with active enforcement, indicate it is a Snare, not a Piton.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    parliamentary_sovereignty_legitimacy,
    'Is the Knesset''s ultimate interpretive authority a legitimate expression of majoritarian democracy, or an erosion of essential checks and balances?',
    'Comparative constitutional analysis of stable democracies with and without strong judicial review, and long-term empirical studies on the impact of such arrangements on minority rights and democratic stability.',
    'If deemed an erosion, it would strengthen arguments for constitutional reform to entrench judicial review; if deemed legitimate, it would reinforce the current structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(parliamentary_sovereignty_legitimacy, conceptual, 'The fundamental normative debate over the legitimacy of unconstrained parliamentary sovereignty in a modern democracy.').

omega_variable(
    judicial_review_effectiveness_in_practice,
    'How effective is judicial review in practice, even if theoretically overridden or advisory, given the potential for international pressure or public opinion?',
    'Empirical study of legislative behavior and judicial pronouncements in the presence of this constraint, observing whether the Knesset still considers judicial opinions or international legal norms.',
    'If judicial review retains de facto influence, the effective suppression and extraction might be slightly lower than theoretically stated; if it has no practical effect, the stated metrics are accurate.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(judicial_review_effectiveness_in_practice, empirical, 'The practical impact of judicial review under a parliamentary sovereignty regime.').

omega_variable(
    kernel_reading_structural_disagreement,
    'What is the precise structural element of the ''basic_law_interpretive_boundary'' kernel that this ''parliamentary_sovereignty_reading'' fundamentally disagrees with, leading to its ''forecloses'' relationship with sibling readings?',
    'Detailed legal-philosophical analysis of the foundational premises of each reading, identifying the irreducible logical contradiction regarding the locus of ultimate constitutional authority.',
    'Clarifying this structural disagreement would precisely define the boundaries of the kernel contest, informing future attempts at constitutional compromise or reform.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_structural_disagreement, conceptual, 'Identifies the core logical contradiction between this reading and its siblings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(basi_tr_t0, basic_law_interpretive_boundary__parliamentary_sovereignty_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(basi_tr_t5, basic_law_interpretive_boundary__parliamentary_sovereignty_reading, theater_ratio, 5, 0.11).
narrative_ontology:measurement(basi_tr_t10, basic_law_interpretive_boundary__parliamentary_sovereignty_reading, theater_ratio, 10, 0.1).
narrative_ontology:measurement(basi_tr_t15, basic_law_interpretive_boundary__parliamentary_sovereignty_reading, theater_ratio, 15, 0.1).
narrative_ontology:measurement(basi_tr_t20, basic_law_interpretive_boundary__parliamentary_sovereignty_reading, theater_ratio, 20, 0.1).

% Extraction over time
narrative_ontology:measurement(basi_be_t0, basic_law_interpretive_boundary__parliamentary_sovereignty_reading, base_extractiveness, 0, 0.75).
narrative_ontology:measurement(basi_be_t5, basic_law_interpretive_boundary__parliamentary_sovereignty_reading, base_extractiveness, 5, 0.78).
narrative_ontology:measurement(basi_be_t10, basic_law_interpretive_boundary__parliamentary_sovereignty_reading, base_extractiveness, 10, 0.81).
narrative_ontology:measurement(basi_be_t15, basic_law_interpretive_boundary__parliamentary_sovereignty_reading, base_extractiveness, 15, 0.83).
narrative_ontology:measurement(basi_be_t20, basic_law_interpretive_boundary__parliamentary_sovereignty_reading, base_extractiveness, 20, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(basi_su_t0, basic_law_interpretive_boundary__parliamentary_sovereignty_reading, suppression_requirement, 0, 0.8).
narrative_ontology:measurement(basi_su_t5, basic_law_interpretive_boundary__parliamentary_sovereignty_reading, suppression_requirement, 5, 0.83).
narrative_ontology:measurement(basi_su_t10, basic_law_interpretive_boundary__parliamentary_sovereignty_reading, suppression_requirement, 10, 0.86).
narrative_ontology:measurement(basi_su_t15, basic_law_interpretive_boundary__parliamentary_sovereignty_reading, suppression_requirement, 15, 0.88).
narrative_ontology:measurement(basi_su_t20, basic_law_interpretive_boundary__parliamentary_sovereignty_reading, suppression_requirement, 20, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, basic_law_interpretive_boundary__judicial_supremacy_reading).
narrative_ontology:affects_constraint(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, basic_law_interpretive_boundary__balanced_contestation_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three distinct readings of the 'basic_law_interpretive_boundary' kernel, each representing a different structural claim about the locus of ultimate constitutional authority. They are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
