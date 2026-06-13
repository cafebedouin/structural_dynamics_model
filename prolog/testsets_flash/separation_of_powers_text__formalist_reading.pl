% ============================================================================
% CONSTRAINT STORY: separation_of_powers_text__formalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_separation_of_powers_text__formalist_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: separation_of_powers_text__formalist_reading
 *   human_readable: Formalist Reading of Separation of Powers: Non-Delegation Doctrine
 *   domain: constitutional_law/administrative_law/political_theory
 *
 * SUMMARY:
 *   This constraint represents the 'formalist' reading of the separation of
 *   powers, asserting strict, impermeable boundaries between the legislative,
 *   executive, and judicial branches, and specifically denying Congress the
 *   ability to delegate legislative authority to administrative agencies.
 *   This reading views such delegations as unconstitutional and a violation
 *   of the original constitutional design. It is a highly contested
 *   interpretation, particularly in the context of the modern administrative
 *   state.
 *
 * KEY AGENTS:
 *   - formalist_legal_scholars: Agenda setter (institutional/identity_locked) — actively promotes this reading.
 *   - administrative_agencies: Primary target (institutional/trapped) — their authority is directly challenged.
 *   - executive_branch: Payer (institutional/constrained) — loses capacity to govern effectively.
 *   - industries_opposed_to_regulation: Beneficiary (organized/mobile) — benefits from reduced agency power.
 *   - public_seeking_efficient_governance: Payer (powerless/trapped) — suffers from reduced governmental responsiveness.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(separation_of_powers_text__formalist_reading, 0.85).
domain_priors:suppression_score(separation_of_powers_text__formalist_reading, 0.9).
domain_priors:theater_ratio(separation_of_powers_text__formalist_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(separation_of_powers_text__formalist_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(separation_of_powers_text__formalist_reading, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(separation_of_powers_text__formalist_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(separation_of_powers_text__formalist_reading, accessibility_collapse, 0.95).
narrative_ontology:constraint_metric(separation_of_powers_text__formalist_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(separation_of_powers_text__formalist_reading, snare).
narrative_ontology:human_readable(separation_of_powers_text__formalist_reading, "Formalist Reading of Separation of Powers: Non-Delegation Doctrine").
narrative_ontology:topic_domain(separation_of_powers_text__formalist_reading, "constitutional_law/administrative_law/political_theory").

domain_priors:requires_active_enforcement(separation_of_powers_text__formalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(separation_of_powers_text__formalist_reading, '953a9aa9-9b60-4999-8b8e-3cd2dde84e9c').
narrative_ontology:cs_kernel_codification('953a9aa9-9b60-4999-8b8e-3cd2dde84e9c', fixed_text).
narrative_ontology:cs_authority_grounding('953a9aa9-9b60-4999-8b8e-3cd2dde84e9c', lineage).
narrative_ontology:cs_interpretation_layer_present('953a9aa9-9b60-4999-8b8e-3cd2dde84e9c').
narrative_ontology:cs_reading_relation('953a9aa9-9b60-4999-8b8e-3cd2dde84e9c', separation_of_powers_text__functionalist_reading, forecloses).
narrative_ontology:cs_reading_relation('953a9aa9-9b60-4999-8b8e-3cd2dde84e9c', separation_of_powers_text__unitary_executive_reading, coexists_with).
narrative_ontology:cs_axiom('953a9aa9-9b60-4999-8b8e-3cd2dde84e9c', foundational, legislative_power_non_delegable).
narrative_ontology:cs_axiom_status(legislative_power_non_delegable, holdable).
narrative_ontology:cs_axiom_grounding('953a9aa9-9b60-4999-8b8e-3cd2dde84e9c', legislative_power_non_delegable, deontological).
narrative_ontology:cs_axiom('953a9aa9-9b60-4999-8b8e-3cd2dde84e9c', foundational, strict_separation_of_powers).
narrative_ontology:cs_axiom_status(strict_separation_of_powers, holdable).
narrative_ontology:cs_axiom_grounding('953a9aa9-9b60-4999-8b8e-3cd2dde84e9c', strict_separation_of_powers, deontological).
narrative_ontology:cs_reference_frame('953a9aa9-9b60-4999-8b8e-3cd2dde84e9c', original_constitutional_design).
narrative_ontology:cs_drift_state('953a9aa9-9b60-4999-8b8e-3cd2dde84e9c', contemporary_administrative_state, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('953a9aa9-9b60-4999-8b8e-3cd2dde84e9c', '').
narrative_ontology:cs_kernel_id(separation_of_powers_text__formalist_reading, separation_of_powers_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(separation_of_powers_text__formalist_reading, formalist_legal_scholars).
narrative_ontology:constraint_beneficiary(separation_of_powers_text__formalist_reading, legislative_branch_purists).
narrative_ontology:constraint_beneficiary(separation_of_powers_text__formalist_reading, industries_opposed_to_regulation).
narrative_ontology:constraint_victim(separation_of_powers_text__formalist_reading, administrative_agencies).
narrative_ontology:constraint_victim(separation_of_powers_text__formalist_reading, executive_branch).
narrative_ontology:constraint_victim(separation_of_powers_text__formalist_reading, regulated_industries_seeking_clarity).
narrative_ontology:constraint_victim(separation_of_powers_text__formalist_reading, public_seeking_efficient_governance).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Advocates for a strict interpretation of the non-delegation doctrine, arguing that the Constitution assigns distinct powers to each branch, and any blurring of these lines is unconstitutional. Their careers and intellectual identity are often tied to this interpretive framework.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__formalist_reading, formalist_legal_scholars, agenda_setter,
    institutional, generational, identity_locked, national).

% Members of Congress who benefit from the assertion of exclusive legislative authority, even if it increases their workload. They gain political capital by opposing perceived executive overreach and agency power.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__formalist_reading, legislative_branch_purists, beneficiary,
    powerful, biographical, constrained, national).

% Lobbying groups and corporations that benefit from reduced administrative agency power, as it limits the scope and enforcement of regulations that might impact their profits. They actively support legal challenges based on non-delegation.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__formalist_reading, industries_opposed_to_regulation, beneficiary,
    organized, immediate, mobile, national).

% Federal agencies (e.g., EPA, FDA) whose existence and regulatory authority are directly challenged by this reading. Their ability to function and implement policy is severely curtailed, potentially leading to their dismantling or incapacitation.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__formalist_reading, administrative_agencies, payer,
    institutional, biographical, trapped, national).

% The President and executive departments rely on agencies to implement policy and manage complex issues. This reading severely limits their capacity to govern effectively, forcing them to seek legislative solutions for every detail.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__formalist_reading, executive_branch, payer,
    institutional, biographical, constrained, national).

% Some industries prefer clear, consistent regulation from agencies over potentially fragmented or politically driven legislation. The uncertainty and potential for legislative gridlock under a strict non-delegation regime create operational difficulties.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__formalist_reading, regulated_industries_seeking_clarity, payer,
    moderate, immediate, constrained, national).

% Citizens who rely on administrative agencies for environmental protection, consumer safety, public health, and other complex regulatory functions. The incapacitation of agencies under this reading leads to a less responsive and effective government.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__formalist_reading, public_seeking_efficient_governance, payer,
    powerless, generational, trapped, national).

% Scholars who argue for a more flexible interpretation of separation of powers, emphasizing the practical necessity of delegation for modern governance. Their arguments are directly contradicted and dismissed by the formalist reading.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__formalist_reading, functionalist_legal_scholars, excluded,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Aims to coordinate governmental power by strictly delineating roles, preventing any single branch from accumulating too much authority and ensuring legislative supremacy in lawmaking.
% TRANSFER_FUNCTION: Transfers authority and decision-making power from administrative agencies back to the legislative branch, and potentially from the executive branch to the legislative branch, at the cost of governmental efficiency and responsiveness.
% ABSENT_VOICES: Functionalist legal scholars, administrative law practitioners, and citizens who prioritize effective and responsive governance over strict formal adherence to 18th-century constitutional structures are excluded from the formalist discourse, their arguments dismissed as pragmatic rather than principled.
% DISAPPEARANCE_RATIONALE: If this formalist reading vanished, the existing administrative state would continue to function with its current delegated powers, and the debate would shift to the appropriate scope of agency action rather than its constitutional legitimacy. The legal landscape would be significantly less litigious regarding non-delegation challenges.
% FOUNDING_PROBLEM: The founding problem was to prevent tyranny by dividing governmental power among distinct branches, ensuring checks and balances, and preventing the concentration of legislative, executive, and judicial functions in one entity.
% FOUNDING_PROBLEM_CORROBORATION: Historians and political theorists widely corroborate the founding problem of preventing tyranny through divided government. However, the formalist reading's specific interpretation of 'strict separation' is primarily championed by a subset of legal scholars and political actors, with significant counter-arguments from other legal and political experts who view it as an anachronistic or impractical solution for modern governance.
narrative_ontology:disappearance_verdict(separation_of_powers_text__formalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(separation_of_powers_text__formalist_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(separation_of_powers_text__formalist_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(separation_of_powers_text__formalist_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(separation_of_powers_text__formalist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(separation_of_powers_text__formalist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(separation_of_powers_text__formalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.85) is high because this reading, if fully implemented, would drastically curtail the regulatory capacity of the administrative state, effectively extracting power and function from agencies. Suppression (0.90) is also high, as this reading actively suppresses alternative modes of governance (delegation) and the very existence of independent agency rulemaking. The theater ratio is low (0.10) because proponents of this reading genuinely seek to enforce it, rather than merely perform its maintenance; the stakes are real. The historical measurements show a rise in extractiveness and suppression as the administrative state grew, making the formalist challenge increasingly impactful.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of formalist legal scholars and legislative purists, this constraint is a 'mountain' – an unchangeable constitutional truth. However, from the perspective of administrative agencies and the executive branch, it operates as a 'snare,' actively extracting their legitimate authority and suppressing their ability to function. The engine's classification will reflect this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Formalist legal scholars and legislative purists are beneficiaries (d near 0.0) as they gain influence and power from this interpretation. Industries opposed to regulation are also beneficiaries, as it reduces their regulatory burden. Administrative agencies and the executive branch are clear targets (d near 1.0) as their core functions are directly undermined. The public seeking efficient governance is a diffuse target, bearing the costs of reduced governmental capacity.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is not about mandatrophy in the traditional sense of a mandate outliving its function. Instead, it's a contest over the *original* mandate's interpretation. The formalist reading asserts that the administrative state's current structure is a deviation from the original mandate, and thus, its 'mandate' (delegated authority) is illegitimate from the outset. The classification as a snare reflects the active extraction and suppression inherent in enforcing this interpretation against the existing administrative apparatus.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    formalist_vs_functionalist_legitimacy,
    'Is the legitimacy of governmental structure derived from strict adherence to original textual forms (formalist) or from its capacity to effectively address modern societal problems (functionalist)?',
    'Judicial precedent over time, reflecting societal consensus on the balance between constitutional form and governmental function. A shift in Supreme Court jurisprudence could resolve this.',
    'If functionalism prevails, this formalist reading would be reclassified as a ''piton'' (inertial, performative) or ''rope'' (coordination of flexible governance). If formalism prevails, the ''snare'' classification would be reinforced, and the administrative state would be dismantled.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(formalist_vs_functionalist_legitimacy, conceptual, 'The core conceptual dispute over constitutional interpretation.').

omega_variable(
    original_intent_vs_evolving_governance,
    'Does the ''original intent'' of the framers regarding separation of powers strictly forbid all legislative delegation, or did they envision a flexible system adaptable to future complexities?',
    'Historical and textual analysis, combined with a deeper understanding of 18th-century political theory and practice. However, this is often itself a contested field.',
    'If original intent is found to be more flexible, the ''emerges_naturally'' claim for this formalist reading would be weakened, potentially reclassifying it away from any ''mountain''-like pretense. If strict non-delegation is definitively proven as original intent, it would strengthen the formalist claim, though not necessarily change its ''snare'' classification given its extractive effects on modern governance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(original_intent_vs_evolving_governance, empirical, 'Historical and textual basis for non-delegation doctrine.').

omega_variable(
    kernel_reading_identification,
    'This constraint is the ''formalist_reading'' of the ''separation_of_powers_text'' kernel. What structural changes would occur if the ''functionalist_reading'' or ''unitary_executive_reading'' were adopted instead?',
    'Analysis of counterfactual legal and political outcomes under alternative interpretive regimes.',
    'If the functionalist reading were adopted, administrative agencies would regain delegated authority, reducing extractiveness and suppression. If the unitary executive reading were adopted, independent agencies would be challenged, but the non-delegation doctrine itself might be less central, shifting the locus of extraction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identification, conceptual, 'Documents this constraint as one reading of a contested kernel and its relationship to sibling readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(separation_of_powers_text__formalist_reading, 1787, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sepa_tr_t1787, separation_of_powers_text__formalist_reading, theater_ratio, 1787, 0.05).
narrative_ontology:measurement(sepa_tr_t1850, separation_of_powers_text__formalist_reading, theater_ratio, 1850, 0.05).
narrative_ontology:measurement(sepa_tr_t1930, separation_of_powers_text__formalist_reading, theater_ratio, 1930, 0.1).
narrative_ontology:measurement(sepa_tr_t1980, separation_of_powers_text__formalist_reading, theater_ratio, 1980, 0.1).
narrative_ontology:measurement(sepa_tr_t2024, separation_of_powers_text__formalist_reading, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(sepa_be_t1787, separation_of_powers_text__formalist_reading, base_extractiveness, 1787, 0.1).
narrative_ontology:measurement(sepa_be_t1850, separation_of_powers_text__formalist_reading, base_extractiveness, 1850, 0.2).
narrative_ontology:measurement(sepa_be_t1930, separation_of_powers_text__formalist_reading, base_extractiveness, 1930, 0.7).
narrative_ontology:measurement(sepa_be_t1980, separation_of_powers_text__formalist_reading, base_extractiveness, 1980, 0.75).
narrative_ontology:measurement(sepa_be_t2024, separation_of_powers_text__formalist_reading, base_extractiveness, 2024, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(sepa_su_t1787, separation_of_powers_text__formalist_reading, suppression_requirement, 1787, 0.1).
narrative_ontology:measurement(sepa_su_t1850, separation_of_powers_text__formalist_reading, suppression_requirement, 1850, 0.2).
narrative_ontology:measurement(sepa_su_t1930, separation_of_powers_text__formalist_reading, suppression_requirement, 1930, 0.75).
narrative_ontology:measurement(sepa_su_t1980, separation_of_powers_text__formalist_reading, suppression_requirement, 1980, 0.8).
narrative_ontology:measurement(sepa_su_t2024, separation_of_powers_text__formalist_reading, suppression_requirement, 2024, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(separation_of_powers_text__formalist_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(separation_of_powers_text__formalist_reading, administrative_procedure_act_interpretation).
narrative_ontology:affects_constraint(separation_of_powers_text__formalist_reading, regulatory_review_process).
narrative_ontology:affects_constraint(separation_of_powers_text__formalist_reading, executive_order_authority).

% DUAL FORMULATION NOTE:
% This constraint is the 'formalist_reading' of the 'separation_of_powers_text' kernel. It stands in contrast to the 'functionalist_reading' (which permits delegation) and the 'unitary_executive_reading' (which focuses on presidential control over the executive branch). Each reading represents a distinct constraint with different beneficiaries, victims, and classifications.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
