% ============================================================================
% CONSTRAINT STORY: separation_of_powers_text__unitary_executive_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_separation_of_powers_text__unitary_executive_reading, []).

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
 *   constraint_id: separation_of_powers_text__unitary_executive_reading
 *   human_readable: Unitary Executive Principle (Reading of Separation of Powers Text)
 *   domain: constitutional_law/political_theory/administrative_law
 *
 * SUMMARY:
 *   This constraint represents the 'unitary executive' reading of the U.S.
 *   Constitution's separation of powers, which posits that all executive
 *   power vests solely in the President, rendering independent agencies
 *   (e.g., FTC, NLRB, Federal Reserve) unconstitutional due to their
 *   insulation from direct presidential control. This reading has gained
 *   significant traction in conservative legal thought and has influenced
 *   executive actions and judicial appointments since the 1980s. It is a
 *   reading of a contested kernel, not a standalone constraint.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(separation_of_powers_text__unitary_executive_reading, 0.65).
domain_priors:suppression_score(separation_of_powers_text__unitary_executive_reading, 0.7).
domain_priors:theater_ratio(separation_of_powers_text__unitary_executive_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(separation_of_powers_text__unitary_executive_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(separation_of_powers_text__unitary_executive_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(separation_of_powers_text__unitary_executive_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(separation_of_powers_text__unitary_executive_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(separation_of_powers_text__unitary_executive_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(separation_of_powers_text__unitary_executive_reading, tangled_rope).
narrative_ontology:human_readable(separation_of_powers_text__unitary_executive_reading, "Unitary Executive Principle (Reading of Separation of Powers Text)").
narrative_ontology:topic_domain(separation_of_powers_text__unitary_executive_reading, "constitutional_law/political_theory/administrative_law").

domain_priors:requires_active_enforcement(separation_of_powers_text__unitary_executive_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(separation_of_powers_text__unitary_executive_reading, '3f48b814-14d7-4b90-a353-09537420d5cd').
narrative_ontology:cs_kernel_codification('3f48b814-14d7-4b90-a353-09537420d5cd', fixed_text).
narrative_ontology:cs_authority_grounding('3f48b814-14d7-4b90-a353-09537420d5cd', lineage).
narrative_ontology:cs_interpretation_layer_present('3f48b814-14d7-4b90-a353-09537420d5cd').
narrative_ontology:cs_reading_relation('3f48b814-14d7-4b90-a353-09537420d5cd', separation_of_powers_text__formalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('3f48b814-14d7-4b90-a353-09537420d5cd', separation_of_powers_text__functionalist_reading, forecloses).
narrative_ontology:cs_axiom('3f48b814-14d7-4b90-a353-09537420d5cd', foundational, all_executive_power_vests_in_president).
narrative_ontology:cs_axiom_status(all_executive_power_vests_in_president, holdable).
narrative_ontology:cs_axiom_grounding('3f48b814-14d7-4b90-a353-09537420d5cd', all_executive_power_vests_in_president, deontological).
narrative_ontology:cs_axiom('3f48b814-14d7-4b90-a353-09537420d5cd', secondary, independent_agencies_violate_separation_of_powers).
narrative_ontology:cs_axiom_status(independent_agencies_violate_separation_of_powers, holdable).
narrative_ontology:cs_axiom_grounding('3f48b814-14d7-4b90-a353-09537420d5cd', independent_agencies_violate_separation_of_powers, conventional).
narrative_ontology:cs_reference_frame('3f48b814-14d7-4b90-a353-09537420d5cd', original_constitutional_design).
narrative_ontology:cs_drift_state('3f48b814-14d7-4b90-a353-09537420d5cd', contemporary_administrative_state, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('3f48b814-14d7-4b90-a353-09537420d5cd', '').
narrative_ontology:cs_kernel_id(separation_of_powers_text__unitary_executive_reading, separation_of_powers_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(separation_of_powers_text__unitary_executive_reading, the_president).
narrative_ontology:constraint_beneficiary(separation_of_powers_text__unitary_executive_reading, executive_branch_agencies).
narrative_ontology:constraint_victim(separation_of_powers_text__unitary_executive_reading, independent_agencies).
narrative_ontology:constraint_victim(separation_of_powers_text__unitary_executive_reading, congressional_oversight_committees).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(separation_of_powers_text__unitary_executive_reading, constitutional_scholars_unitary_executive).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% As the sole repository of executive power, the President benefits from the consolidation of authority and the ability to direct all executive functions without interference from independent agencies or congressional limitations on removal power. This enhances political control and policy coherence from the executive perspective.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__unitary_executive_reading, the_president, agenda_setter,
    institutional, biographical, constrained, national).

% These agencies, directly accountable to the President, benefit from a clear chain of command and unified policy direction. They gain influence and resources as functions are centralized under direct presidential control, reducing fragmentation and inter-agency conflict.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__unitary_executive_reading, executive_branch_agencies, beneficiary,
    institutional, generational, constrained, national).

% Agencies like the FTC, NLRB, and Federal Reserve, designed to operate with a degree of independence from direct presidential control, are the primary targets. Their independent status, multi-member boards, and 'for cause' removal protections are seen as unconstitutional, leading to challenges to their structure and authority. Their exit is structural dissolution or re-subordination.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__unitary_executive_reading, independent_agencies, payer,
    institutional, generational, trapped, national).

% These committees, which rely on independent agencies to implement policy and provide expert, non-partisan advice, lose influence and control as the unitary executive principle centralizes power in the presidency. Their ability to delegate and oversee is diminished, making them a victim of this interpretation.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__unitary_executive_reading, congressional_oversight_committees, payer,
    institutional, generational, constrained, national).

% Scholars who advocate for the unitary executive principle find their interpretive framework vindicated and gain academic influence. Their arguments provide the intellectual scaffolding for executive actions that challenge independent agencies.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__unitary_executive_reading, constitutional_scholars_unitary_executive, beneficiary,
    analytical, civilizational, analytical, universal).

% Scholars who advocate for a more flexible, functionalist interpretation of separation of powers are excluded from the unitary executive's framing. Their arguments for the legitimacy and necessity of independent agencies are directly challenged by this reading.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__unitary_executive_reading, constitutional_scholars_functionalist, excluded,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Aims to coordinate all executive power under a single, accountable head (the President) to ensure energetic and unified execution of laws, preventing fragmentation and conflicting policy directives within the executive branch.
% TRANSFER_FUNCTION: Transfers authority, policy control, and removal power from independent agencies and Congress (via its delegation authority) to the President and directly accountable executive branch agencies.
% ABSENT_VOICES: The framers of the Constitution, if they could speak to the modern administrative state, would offer diverse views. Advocates for a more robust, independent administrative state (e.g., those who designed the New Deal agencies) are structurally excluded from this reading's premise, as their very existence is deemed unconstitutional.
% DISAPPEARANCE_RATIONALE: If the unitary executive principle (as a binding constraint) vanished, the legal and political landscape would immediately rearrange. Independent agencies would regain full structural legitimacy, presidential removal powers would be curtailed, and congressional delegation would be less contested. The balance of power within the federal government would shift significantly.
% FOUNDING_PROBLEM: The problem of ensuring a strong, unified, and accountable executive branch capable of effectively executing federal law, as envisioned by the framers of the Constitution.
% FOUNDING_PROBLEM_CORROBORATION: Proponents of the unitary executive (e.g., Federalist Society, some Supreme Court justices) attest that the problem of executive accountability and coherence remains live, citing the growth of the administrative state. Opponents (e.g., functionalist scholars, some members of Congress) argue that the problem is either solved by existing checks and balances or that the unitary executive reading creates new problems of unchecked power; corroboration for the 'live' status comes primarily from within the benefiting parties and their intellectual allies.
narrative_ontology:disappearance_verdict(separation_of_powers_text__unitary_executive_reading, world_rearranges).
narrative_ontology:founding_problem_status(separation_of_powers_text__unitary_executive_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(separation_of_powers_text__unitary_executive_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(separation_of_powers_text__unitary_executive_reading, 'none', 1).
narrative_ontology:epsilon_provenance(separation_of_powers_text__unitary_executive_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(separation_of_powers_text__unitary_executive_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(separation_of_powers_text__unitary_executive_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(separation_of_powers_text__unitary_executive_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.65) is substantial because this reading seeks to reallocate significant power and control from independent agencies and Congress to the President. Suppression (0.70) is high because it requires active legal and political enforcement to challenge and dismantle existing institutional structures. Theater ratio (0.20) is low as the efforts to implement this reading are genuine and aimed at structural change, not mere performance. The claimed type is 'tangled_rope' because it offers a coordination story (unified executive action) but involves clear asymmetric extraction from independent agencies and congressional oversight.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the President and unitary executive proponents, this is a necessary correction to restore constitutional order and executive accountability. From the perspective of independent agencies and their congressional allies, it is an overreach that threatens expertise, stability, and checks on presidential power. The engine's classification will reflect this divergence based on the declared structural positions.
 *
 * DIRECTIONALITY LOGIC:
 *   The President and directly accountable executive branch agencies are clear beneficiaries, gaining power and control. Independent agencies and congressional oversight committees are victims, losing autonomy and influence. Constitutional scholars advocating this view are also beneficiaries, as their intellectual framework gains prominence. Functionalist scholars, who offer an alternative reading, are excluded from this framing.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is not about mandatrophy in the traditional sense of an atrophied function. Instead, it's a reinterpretation of a foundational text that seeks to re-assert a particular (and contested) original mandate. The 'mandate' of independent agencies, from this reading's perspective, is itself illegitimate, so the question is not about their function atrophying, but about their very existence being challenged. The classification as a Tangled Rope highlights the active, extractive nature of this reinterpretation, preventing it from being mislabeled as a benign coordination or a natural law.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    original_intent_ambiguity,
    'Does the ''original intent'' of the Constitution''s framers genuinely support a unitary executive principle that precludes independent agencies, or is this an anachronistic application of 18th-century concepts to a 21st-century administrative state?',
    'Further historical and textual analysis of founding-era debates, coupled with a consensus among constitutional historians (outside of partisan legal movements).',
    'If original intent is found to be ambiguous or not supportive, the legitimacy of the unitary executive reading as a ''natural law'' (or even a ''rope'') would be severely undermined, pushing its classification further towards a Snare or Tangled Rope based on its active enforcement and extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(original_intent_ambiguity, conceptual, 'Ambiguity regarding the historical grounding of the unitary executive principle.').

omega_variable(
    functional_necessity_of_independence,
    'Are independent agencies functionally necessary for effective governance in complex modern domains (e.g., monetary policy, market regulation) due to their expertise and insulation from short-term political pressures, or can their functions be effectively performed under direct presidential control?',
    'Empirical studies comparing policy outcomes and administrative efficiency in systems with and without independent agencies, or in jurisdictions where such agencies have been subordinated.',
    'If functional necessity is demonstrated, the coordination story of the unitary executive reading would be weakened, as its ''solution'' to fragmentation would come at the cost of essential governance capabilities, reinforcing its extractive nature. If not, the coordination story gains strength.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(functional_necessity_of_independence, empirical, 'Whether independent agencies serve an indispensable functional role.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(separation_of_powers_text__unitary_executive_reading, 1980, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sepa_tr_t1980, separation_of_powers_text__unitary_executive_reading, theater_ratio, 1980, 0.1).
narrative_ontology:measurement(sepa_tr_t1990, separation_of_powers_text__unitary_executive_reading, theater_ratio, 1990, 0.15).
narrative_ontology:measurement(sepa_tr_t2000, separation_of_powers_text__unitary_executive_reading, theater_ratio, 2000, 0.18).
narrative_ontology:measurement(sepa_tr_t2010, separation_of_powers_text__unitary_executive_reading, theater_ratio, 2010, 0.2).
narrative_ontology:measurement(sepa_tr_t2024, separation_of_powers_text__unitary_executive_reading, theater_ratio, 2024, 0.2).

% Extraction over time
narrative_ontology:measurement(sepa_be_t1980, separation_of_powers_text__unitary_executive_reading, base_extractiveness, 1980, 0.4).
narrative_ontology:measurement(sepa_be_t1990, separation_of_powers_text__unitary_executive_reading, base_extractiveness, 1990, 0.5).
narrative_ontology:measurement(sepa_be_t2000, separation_of_powers_text__unitary_executive_reading, base_extractiveness, 2000, 0.58).
narrative_ontology:measurement(sepa_be_t2010, separation_of_powers_text__unitary_executive_reading, base_extractiveness, 2010, 0.62).
narrative_ontology:measurement(sepa_be_t2024, separation_of_powers_text__unitary_executive_reading, base_extractiveness, 2024, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(sepa_su_t1980, separation_of_powers_text__unitary_executive_reading, suppression_requirement, 1980, 0.45).
narrative_ontology:measurement(sepa_su_t1990, separation_of_powers_text__unitary_executive_reading, suppression_requirement, 1990, 0.55).
narrative_ontology:measurement(sepa_su_t2000, separation_of_powers_text__unitary_executive_reading, suppression_requirement, 2000, 0.62).
narrative_ontology:measurement(sepa_su_t2010, separation_of_powers_text__unitary_executive_reading, suppression_requirement, 2010, 0.67).
narrative_ontology:measurement(sepa_su_t2024, separation_of_powers_text__unitary_executive_reading, suppression_requirement, 2024, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(separation_of_powers_text__unitary_executive_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(separation_of_powers_text__unitary_executive_reading, separation_of_powers_text__formalist_reading).
narrative_ontology:affects_constraint(separation_of_powers_text__unitary_executive_reading, separation_of_powers_text__functionalist_reading).
narrative_ontology:affects_constraint(separation_of_powers_text__unitary_executive_reading, administrative_state_legitimacy).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'separation_of_powers_text' kernel. Its interpretation of executive power directly influences the structural legitimacy of the administrative state and competes with formalist and functionalist readings of the same constitutional text.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
