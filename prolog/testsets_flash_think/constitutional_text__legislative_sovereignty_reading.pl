% ============================================================================
% CONSTRAINT STORY: constitutional_text__legislative_sovereignty_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_constitutional_text__legislative_sovereignty_reading, []).

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
 *   constraint_id: constitutional_text__legislative_sovereignty_reading
 *   human_readable: Legislative Sovereignty Reading of Constitutional Text
 *   domain: Constitutional Theory/Political Philosophy/Comparative Law
 *
 * SUMMARY:
 *   This constraint represents the 'legislative sovereignty' reading of a
 *   constitutional text, where parliament is established as the supreme
 *   authority in constitutional interpretation. Courts provide advice, but
 *   the legislature retains the final say, often through mechanisms like
 *   notwithstanding clauses or simple override. This reading prioritizes
 *   democratic accountability and the will of the majority over judicial
 *   review, framing the constraint as a core mechanism for democratic
 *   governance.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(constitutional_text__legislative_sovereignty_reading, 0.6).
domain_priors:suppression_score(constitutional_text__legislative_sovereignty_reading, 0.7).
domain_priors:theater_ratio(constitutional_text__legislative_sovereignty_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(constitutional_text__legislative_sovereignty_reading, extractiveness, 0.6).
narrative_ontology:constraint_metric(constitutional_text__legislative_sovereignty_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(constitutional_text__legislative_sovereignty_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(constitutional_text__legislative_sovereignty_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(constitutional_text__legislative_sovereignty_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(constitutional_text__legislative_sovereignty_reading, tangled_rope).
narrative_ontology:human_readable(constitutional_text__legislative_sovereignty_reading, "Legislative Sovereignty Reading of Constitutional Text").
narrative_ontology:topic_domain(constitutional_text__legislative_sovereignty_reading, "Constitutional Theory/Political Philosophy/Comparative Law").

domain_priors:requires_active_enforcement(constitutional_text__legislative_sovereignty_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(constitutional_text__legislative_sovereignty_reading, '7e20876f-1dd3-404d-a33e-571d9af8fabd').
narrative_ontology:cs_kernel_codification('7e20876f-1dd3-404d-a33e-571d9af8fabd', fixed_text).
narrative_ontology:cs_authority_grounding('7e20876f-1dd3-404d-a33e-571d9af8fabd', lineage).
narrative_ontology:cs_interpretation_layer_present('7e20876f-1dd3-404d-a33e-571d9af8fabd').
narrative_ontology:cs_reading_relation('7e20876f-1dd3-404d-a33e-571d9af8fabd', constitutional_text__judicial_supremacy_reading, forecloses).
narrative_ontology:cs_reading_relation('7e20876f-1dd3-404d-a33e-571d9af8fabd', constitutional_text__popular_sovereignty_reading, coexists_with).
narrative_ontology:cs_axiom('7e20876f-1dd3-404d-a33e-571d9af8fabd', foundational, parliamentary_supremacy_is_foundational).
narrative_ontology:cs_axiom_status(parliamentary_supremacy_is_foundational, holdable).
narrative_ontology:cs_axiom_grounding('7e20876f-1dd3-404d-a33e-571d9af8fabd', parliamentary_supremacy_is_foundational, conventional).
narrative_ontology:cs_axiom('7e20876f-1dd3-404d-a33e-571d9af8fabd', secondary, legislative_intent_is_supreme).
narrative_ontology:cs_axiom_status(legislative_intent_is_supreme, holdable).
narrative_ontology:cs_axiom_grounding('7e20876f-1dd3-404d-a33e-571d9af8fabd', legislative_intent_is_supreme, conventional).
narrative_ontology:cs_reference_frame('7e20876f-1dd3-404d-a33e-571d9af8fabd', westminster_parliamentary_tradition).
narrative_ontology:cs_drift_state('7e20876f-1dd3-404d-a33e-571d9af8fabd', contemporary_rights_charter_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('7e20876f-1dd3-404d-a33e-571d9af8fabd', '').
narrative_ontology:cs_kernel_id(constitutional_text__legislative_sovereignty_reading, constitutional_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(constitutional_text__legislative_sovereignty_reading, majoritarian_will).
narrative_ontology:constraint_beneficiary(constitutional_text__legislative_sovereignty_reading, legislature).
narrative_ontology:constraint_victim(constitutional_text__legislative_sovereignty_reading, minority_rights_advocates).
narrative_ontology:constraint_victim(constitutional_text__legislative_sovereignty_reading, judicial_branch).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Holds ultimate authority over constitutional meaning, enacting and overriding judicial interpretations to reflect the will of the majority. Benefits from unchecked power to implement policy and define the constitutional framework.
narrative_ontology:constraint_stakeholder(constitutional_text__legislative_sovereignty_reading, legislature, agenda_setter,
    institutional, generational, mobile, national).

% Its preferences and policy choices are directly translated into law and constitutional interpretation without final judicial veto. Benefits from direct democratic accountability and the ability to shape the constitutional landscape.
narrative_ontology:constraint_stakeholder(constitutional_text__legislative_sovereignty_reading, majoritarian_will, beneficiary,
    organized, generational, mobile, national).

% Its constitutional interpretations are advisory and can be overridden by the legislature through mechanisms like notwithstanding clauses. Bears the cost of limited authority and potential erosion of its role as a check on legislative power.
narrative_ontology:constraint_stakeholder(constitutional_text__legislative_sovereignty_reading, judicial_branch, payer,
    institutional, generational, constrained, national).

% Their rights and interests are vulnerable to legislative majorities, as judicial protections can be set aside. Bear the cost of reduced constitutional safeguards and the need for constant political mobilization.
narrative_ontology:constraint_stakeholder(constitutional_text__legislative_sovereignty_reading, minority_rights_advocates, payer,
    organized, biographical, constrained, national).

% Analyze the implications of legislative supremacy for constitutional stability, rights protection, and democratic theory. Provide critical commentary and influence public discourse but have no direct power within the framework.
narrative_ontology:constraint_stakeholder(constitutional_text__legislative_sovereignty_reading, constitutional_scholars, observer,
    analytical, biographical, analytical, universal).

% Advocate for direct popular input on constitutional matters, seeing both legislature and judiciary as potentially unrepresentative. Excluded from direct interpretive power within this framework, their influence is indirect through political pressure.
narrative_ontology:constraint_stakeholder(constitutional_text__legislative_sovereignty_reading, popular_sovereignty_movements, excluded,
    organized, generational, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To establish a clear, democratically accountable mechanism for resolving disputes over constitutional meaning, ensuring that the will of the elected representatives ultimately prevails.
% TRANSFER_FUNCTION: Transfers ultimate interpretive authority over the constitutional text from the judicial branch to the legislative branch, empowering the majority's elected representatives to define constitutional meaning.
% ABSENT_VOICES: Advocates for strong judicial review would object, arguing for the judiciary's role in protecting fundamental rights from majoritarian overreach. Popular sovereignty movements would also object, asserting the people's direct interpretive authority over both legislature and courts.
% DISAPPEARANCE_RATIONALE: If the principle of legislative sovereignty vanished overnight, the constitutional system would face severe instability. Without a clear final arbiter of constitutional meaning, a power vacuum would emerge, likely leading to a shift towards judicial supremacy or direct popular action, fundamentally reorganizing the political structure.
% FOUNDING_PROBLEM: To establish a stable, democratically accountable system for governance and constitutional interpretation, preventing unelected bodies from overriding the will of the people and ensuring legislative supremacy.
% FOUNDING_PROBLEM_CORROBORATION: Political scientists and historical analyses of Westminster-style parliamentary systems often corroborate the intent to establish legislative supremacy as a foundational principle. Legal scholars in jurisdictions with notwithstanding clauses also attest to its ongoing function in asserting legislative finality.
narrative_ontology:disappearance_verdict(constitutional_text__legislative_sovereignty_reading, world_rearranges).
narrative_ontology:founding_problem_status(constitutional_text__legislative_sovereignty_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(constitutional_text__legislative_sovereignty_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(constitutional_text__legislative_sovereignty_reading, 'none', 1).
narrative_ontology:epsilon_provenance(constitutional_text__legislative_sovereignty_reading, 0.6, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(constitutional_text__legislative_sovereignty_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(constitutional_text__legislative_sovereignty_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(constitutional_text__legislative_sovereignty_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint is classified as a Tangled Rope because it genuinely coordinates democratic decision-making (benefiting the legislature and majoritarian will) but simultaneously extracts from and suppresses the power of the judicial branch and the protection of minority rights. Extractiveness is moderate (0.60) as it shifts power and resources to the legislative majority. Suppression is high (0.70) because judicial challenges and minority protections are actively overridden or limited. Theater ratio is low (0.10) as this is a fundamental structural principle, not a performative one. Resistance is moderate (0.55) due to ongoing advocacy for stronger judicial review and minority rights.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the legislature and the majoritarian will, this constraint is a legitimate and necessary mechanism for democratic governance, ensuring that elected representatives have the final say. From the perspective of the judicial branch and minority rights advocates, it represents an erosion of checks and balances and a vulnerability for fundamental rights. The engine will compute these divergent classifications based on the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   The legislature and the majoritarian will are the primary beneficiaries, as they gain ultimate interpretive authority and the ability to implement policy without judicial veto. The judicial branch and minority rights advocates are the primary targets, bearing the cost of limited authority and reduced constitutional safeguards. Constitutional scholars act as observers, while popular sovereignty movements are excluded from direct interpretive power within this framework.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    judicial_vs_legislative_authority_boundary,
    'What is the precise boundary between judicial advice and legislative override, and how is it maintained in practice?',
    'Empirical analysis of legislative override frequency, judicial deference patterns, and public/political reactions to such overrides over time.',
    'If legislative overrides are rare or consistently met with strong political backlash, the effective suppression of judicial power might be lower than stated. If overrides are frequent and normalized, the suppression is robust.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(judicial_vs_legislative_authority_boundary, empirical, 'Ambiguity over the practical limits of legislative supremacy.').

omega_variable(
    minority_protection_efficacy,
    'Does legislative supremacy, even with democratic processes, adequately protect minority rights from majoritarian overreach?',
    'Comparative legal analysis of rights outcomes in jurisdictions with and without strong legislative override powers, focusing on vulnerable minority groups.',
    'If minority rights are systematically eroded in such systems, the extraction from minority rights advocates is higher and the coordination function is more clearly a cover for majoritarian extraction. If protections are robust, the extraction is lower.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(minority_protection_efficacy, empirical, 'Efficacy of minority rights protection under legislative supremacy.').

omega_variable(
    kernel_reading_identity,
    'Is this constraint a genuine ''legislative sovereignty'' reading, or is it a cover for a more extractive ''majoritarian tyranny'' constraint?',
    'Analysis of the historical application of override clauses: if consistently used to suppress fundamental rights without broad public deliberation, it leans towards tyranny. If used sparingly and with robust debate, it supports the sovereignty claim.',
    'If reclassified as ''majoritarian tyranny'', the extractiveness and suppression metrics would be higher, and the claimed_type would shift towards Snare, reflecting a pure extraction mechanism.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Confirms this constraint as one specific reading of the ''constitutional_text'' kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(constitutional_text__legislative_sovereignty_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cons_tr_t0, constitutional_text__legislative_sovereignty_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(cons_tr_t10, constitutional_text__legislative_sovereignty_reading, theater_ratio, 10, 0.1).
narrative_ontology:measurement(cons_tr_t20, constitutional_text__legislative_sovereignty_reading, theater_ratio, 20, 0.1).
narrative_ontology:measurement(cons_tr_t30, constitutional_text__legislative_sovereignty_reading, theater_ratio, 30, 0.1).
narrative_ontology:measurement(cons_tr_t40, constitutional_text__legislative_sovereignty_reading, theater_ratio, 40, 0.1).
narrative_ontology:measurement(cons_tr_t50, constitutional_text__legislative_sovereignty_reading, theater_ratio, 50, 0.1).

% Extraction over time
narrative_ontology:measurement(cons_be_t0, constitutional_text__legislative_sovereignty_reading, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(cons_be_t10, constitutional_text__legislative_sovereignty_reading, base_extractiveness, 10, 0.53).
narrative_ontology:measurement(cons_be_t20, constitutional_text__legislative_sovereignty_reading, base_extractiveness, 20, 0.56).
narrative_ontology:measurement(cons_be_t30, constitutional_text__legislative_sovereignty_reading, base_extractiveness, 30, 0.58).
narrative_ontology:measurement(cons_be_t40, constitutional_text__legislative_sovereignty_reading, base_extractiveness, 40, 0.59).
narrative_ontology:measurement(cons_be_t50, constitutional_text__legislative_sovereignty_reading, base_extractiveness, 50, 0.6).

% Suppression requirement over time
narrative_ontology:measurement(cons_su_t0, constitutional_text__legislative_sovereignty_reading, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(cons_su_t10, constitutional_text__legislative_sovereignty_reading, suppression_requirement, 10, 0.63).
narrative_ontology:measurement(cons_su_t20, constitutional_text__legislative_sovereignty_reading, suppression_requirement, 20, 0.66).
narrative_ontology:measurement(cons_su_t30, constitutional_text__legislative_sovereignty_reading, suppression_requirement, 30, 0.68).
narrative_ontology:measurement(cons_su_t40, constitutional_text__legislative_sovereignty_reading, suppression_requirement, 40, 0.69).
narrative_ontology:measurement(cons_su_t50, constitutional_text__legislative_sovereignty_reading, suppression_requirement, 50, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(constitutional_text__legislative_sovereignty_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(constitutional_text__legislative_sovereignty_reading, constitutional_text__judicial_supremacy_reading).
narrative_ontology:affects_constraint(constitutional_text__legislative_sovereignty_reading, constitutional_text__popular_sovereignty_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three distinct readings of the 'constitutional_text' kernel. Its ε value differs significantly from the 'judicial supremacy' and 'popular sovereignty' readings due to different allocations of interpretive authority and different beneficiary/victim structures.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
