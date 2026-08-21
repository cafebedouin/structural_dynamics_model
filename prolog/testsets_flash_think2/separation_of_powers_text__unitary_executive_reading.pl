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
 *   human_readable: Unitary Executive Principle (Reading of Separation of Powers)
 *   domain: constitutional_law/political_theory/administrative_law
 *
 * SUMMARY:
 *   This constraint story instantiates the 'unitary executive' reading of the
 *   U.S. Constitution's separation of powers text. This reading asserts that
 *   all executive power vests solely in the President, implying that
 *   independent agencies, which operate with some degree of insulation from
 *   direct presidential control, are unconstitutional. The constraint is
 *   actively enforced through presidential actions, judicial appointments,
 *   and legal arguments aimed at consolidating executive authority and
 *   dismantling or subordinating independent agencies.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(separation_of_powers_text__unitary_executive_reading, 0.85).
domain_priors:suppression_score(separation_of_powers_text__unitary_executive_reading, 0.78).
domain_priors:theater_ratio(separation_of_powers_text__unitary_executive_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(separation_of_powers_text__unitary_executive_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(separation_of_powers_text__unitary_executive_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(separation_of_powers_text__unitary_executive_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(separation_of_powers_text__unitary_executive_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(separation_of_powers_text__unitary_executive_reading, resistance, 0.8).

% --- Constraint claim ---
narrative_ontology:constraint_claim(separation_of_powers_text__unitary_executive_reading, tangled_rope).
narrative_ontology:human_readable(separation_of_powers_text__unitary_executive_reading, "Unitary Executive Principle (Reading of Separation of Powers)").
narrative_ontology:topic_domain(separation_of_powers_text__unitary_executive_reading, "constitutional_law/political_theory/administrative_law").

domain_priors:requires_active_enforcement(separation_of_powers_text__unitary_executive_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(separation_of_powers_text__unitary_executive_reading, '7e7ab0d5-f518-4722-9e56-b10a8c397a65').
narrative_ontology:cs_kernel_codification('7e7ab0d5-f518-4722-9e56-b10a8c397a65', fixed_text).
narrative_ontology:cs_authority_grounding('7e7ab0d5-f518-4722-9e56-b10a8c397a65', lineage).
narrative_ontology:cs_interpretation_layer_present('7e7ab0d5-f518-4722-9e56-b10a8c397a65').
narrative_ontology:cs_reading_relation('7e7ab0d5-f518-4722-9e56-b10a8c397a65', separation_of_powers_text__formalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('7e7ab0d5-f518-4722-9e56-b10a8c397a65', separation_of_powers_text__functionalist_reading, forecloses).
narrative_ontology:cs_axiom('7e7ab0d5-f518-4722-9e56-b10a8c397a65', foundational, all_executive_power_vests_in_president).
narrative_ontology:cs_axiom_status(all_executive_power_vests_in_president, holdable).
narrative_ontology:cs_axiom_grounding('7e7ab0d5-f518-4722-9e56-b10a8c397a65', all_executive_power_vests_in_president, conventional).
narrative_ontology:cs_axiom('7e7ab0d5-f518-4722-9e56-b10a8c397a65', foundational, independent_agencies_exercise_executive_power).
narrative_ontology:cs_axiom_status(independent_agencies_exercise_executive_power, holdable).
narrative_ontology:cs_axiom_grounding('7e7ab0d5-f518-4722-9e56-b10a8c397a65', independent_agencies_exercise_executive_power, conventional).
narrative_ontology:cs_reference_frame('7e7ab0d5-f518-4722-9e56-b10a8c397a65', founding_era_executive_unity).
narrative_ontology:cs_drift_state('7e7ab0d5-f518-4722-9e56-b10a8c397a65', contemporary_administrative_state, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('7e7ab0d5-f518-4722-9e56-b10a8c397a65', '').
narrative_ontology:cs_kernel_id(separation_of_powers_text__unitary_executive_reading, separation_of_powers_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(separation_of_powers_text__unitary_executive_reading, the_president).
narrative_ontology:constraint_beneficiary(separation_of_powers_text__unitary_executive_reading, executive_branch_officials).
narrative_ontology:constraint_beneficiary(separation_of_powers_text__unitary_executive_reading, legal_scholars_unitary_executive).
narrative_ontology:constraint_victim(separation_of_powers_text__unitary_executive_reading, independent_agencies).
narrative_ontology:constraint_victim(separation_of_powers_text__unitary_executive_reading, congressional_oversight).
narrative_ontology:constraint_victim(separation_of_powers_text__unitary_executive_reading, legal_scholars_pluralist).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% As the head of the executive branch, the President is the primary proponent and beneficiary of the unitary executive principle, seeking to consolidate control over all executive functions and personnel, including those in independent agencies.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__unitary_executive_reading, the_president, agenda_setter,
    institutional, biographical, mobile, national).

% Officials appointed by the President benefit from increased hierarchical control and reduced bureaucratic resistance from independent agencies, aligning their actions more directly with presidential policy.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__unitary_executive_reading, executive_branch_officials, beneficiary,
    institutional, biographical, constrained, national).

% Agencies like the FTC, NLRB, and Federal Reserve are the primary targets, facing challenges to their independence, removal protections for their leaders, and their very existence as entities outside direct presidential control.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__unitary_executive_reading, independent_agencies, payer,
    institutional, generational, trapped, national).

% Congress's ability to create and oversee independent agencies as a check on executive power is diminished, as the unitary executive principle asserts that such agencies are unconstitutional infringements on presidential authority.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__unitary_executive_reading, congressional_oversight, payer,
    institutional, generational, constrained, national).

% The courts are the ultimate arbiters of constitutional interpretation, and their rulings on presidential removal power and agency independence directly shape the constraint's force and scope.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__unitary_executive_reading, judiciary, observer,
    institutional, generational, analytical, national).

% Academics and legal practitioners who advocate for the unitary executive principle gain influence and see their interpretive framework validated when this reading gains legal or political traction.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__unitary_executive_reading, legal_scholars_unitary_executive, beneficiary,
    organized, generational, analytical, national).

% Academics and legal practitioners who defend the constitutionality and benefits of independent agencies, or advocate for a more flexible separation of powers, find their arguments marginalized or actively challenged by this reading.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__unitary_executive_reading, legal_scholars_pluralist, excluded,
    organized, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To ensure a single, accountable executive branch by consolidating all executive power under the President, thereby improving efficiency and democratic accountability.
% TRANSFER_FUNCTION: Transfers authority, control, and policy-making discretion from independent agencies and Congress (via delegation) to the President and directly appointed executive officials.
% ABSENT_VOICES: Defenders of the administrative state, functionalist constitutional scholars, and advocates for agency independence (e.g., consumer protection groups, environmental organizations) are actively challenged or excluded from the unitary executive's framing of legitimate governance.
% DISAPPEARANCE_RATIONALE: If the unitary executive principle were universally adopted and enforced, the structure of the U.S. government would fundamentally change. Independent agencies would be dismantled or brought under direct presidential control, altering regulatory policy, economic stability (e.g., Federal Reserve), and the balance of power between branches.
% FOUNDING_PROBLEM: The perceived fragmentation and unaccountability of executive power due to the rise of independent agencies, which are seen as infringing on presidential authority and violating the original constitutional design.
% FOUNDING_PROBLEM_CORROBORATION: Proponents within the executive branch and certain legal/political factions actively attest that the problem of an 'unaccountable' administrative state is live. Critics (e.g., functionalist scholars, defenders of independent agencies) dispute this framing, arguing that independent agencies serve vital functions and are accountable through other mechanisms (e.g., congressional oversight, judicial review). Corroboration for the 'problem' is primarily internal to the unitary executive movement; external corroboration is contested.
narrative_ontology:disappearance_verdict(separation_of_powers_text__unitary_executive_reading, world_rearranges).
narrative_ontology:founding_problem_status(separation_of_powers_text__unitary_executive_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(separation_of_powers_text__unitary_executive_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(separation_of_powers_text__unitary_executive_reading, 'none', 1).
narrative_ontology:epsilon_provenance(separation_of_powers_text__unitary_executive_reading, 0.85, 'gemini-2.5-flash', 'none', direct).

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
 *   The `extractiveness` is high because this reading seeks to centralize significant power and control, effectively extracting autonomy from independent agencies and congressional oversight. `Suppression` is also high, as it requires active legal and political efforts to suppress alternative interpretations and the existing institutional structures of the administrative state. `Theater_ratio` is low because the unitary executive principle is an active, ideologically driven legal and political project, not a vestigial or performative one. `Accessibility_collapse` is substantial as it aims to eliminate the 'alternative' of independent agency action. `Resistance` is high due to strong opposition from defenders of the administrative state and different constitutional interpretations.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the President and unitary executive proponents, this reading restores constitutional order and accountability. From the perspective of independent agencies and their defenders, it represents an overreach of presidential power that undermines expertise, stability, and checks and balances. The judiciary's perspective is one of arbitration, weighing these competing claims.
 *
 * DIRECTIONALITY LOGIC:
 *   The President and executive branch officials are clear beneficiaries, gaining expanded authority and control. Legal scholars advocating for this view also benefit from its increasing influence. Independent agencies and congressional oversight are the primary targets/victims, as their established powers and roles are directly challenged and diminished. Legal scholars with pluralist views are excluded from the dominant discourse promoted by this reading.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    constitutional_text_ambiguity,
    'Does the constitutional text ''The executive Power shall be vested in a President'' inherently imply a unitary, hierarchical executive, or does it permit a more pluralistic structure with independent agencies?',
    'Further historical research into the founding era''s understanding of ''executive power'' and ''vested,'' combined with evolving judicial interpretation and public consensus.',
    'If the text is found to be inherently ambiguous or to permit pluralism, the unitary executive reading''s claim to constitutional fidelity weakens, reducing its legitimacy and extractiveness. If it is found to strongly imply unity, the reading''s force increases.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(constitutional_text_ambiguity, conceptual, 'Ambiguity of ''executive power'' in the U.S. Constitution.').

omega_variable(
    accountability_vs_expertise_tradeoff,
    'Does consolidating all executive power under the President genuinely enhance democratic accountability and efficiency, or does it sacrifice necessary expertise, stability, and non-partisanship provided by independent agencies?',
    'Empirical studies comparing governance outcomes (e.g., policy stability, regulatory effectiveness, corruption levels) in systems with unitary vs. plural executives, and longitudinal analysis of administrative state performance.',
    'Empirical evidence demonstrating a net loss of effective governance under a strictly unitary executive would weaken the reading''s instrumental justification, potentially reducing its political traction. Evidence of enhanced accountability and efficiency would strengthen it.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(accountability_vs_expertise_tradeoff, empirical, 'Trade-off between presidential accountability and agency expertise/stability.').

omega_variable(
    political_tool_vs_constitutional_principle,
    'Is the unitary executive principle primarily a genuine constitutional interpretation, or is it a political tool used by presidents to centralize power and overcome bureaucratic resistance?',
    'Analysis of the consistency of presidential actions and legal arguments across different administrations and political parties, and the correlation between unitary executive advocacy and specific policy goals.',
    'If primarily a political tool, its legitimacy as a ''constitutional'' constraint diminishes, potentially leading to greater public and judicial skepticism. If consistently applied as a principle regardless of political expediency, its perceived constitutional force increases.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(political_tool_vs_constitutional_principle, conceptual, 'Whether the unitary executive principle is a constitutional interpretation or a political strategy.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(separation_of_powers_text__unitary_executive_reading, 1980, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sepa_tr_t1980, separation_of_powers_text__unitary_executive_reading, theater_ratio, 1980, 0.1).
narrative_ontology:measurement(sepa_tr_t1990, separation_of_powers_text__unitary_executive_reading, theater_ratio, 1990, 0.12).
narrative_ontology:measurement(sepa_tr_t2000, separation_of_powers_text__unitary_executive_reading, theater_ratio, 2000, 0.13).
narrative_ontology:measurement(sepa_tr_t2010, separation_of_powers_text__unitary_executive_reading, theater_ratio, 2010, 0.14).
narrative_ontology:measurement(sepa_tr_t2020, separation_of_powers_text__unitary_executive_reading, theater_ratio, 2020, 0.15).
narrative_ontology:measurement(sepa_tr_t2025, separation_of_powers_text__unitary_executive_reading, theater_ratio, 2025, 0.15).

% Extraction over time
narrative_ontology:measurement(sepa_be_t1980, separation_of_powers_text__unitary_executive_reading, base_extractiveness, 1980, 0.6).
narrative_ontology:measurement(sepa_be_t1990, separation_of_powers_text__unitary_executive_reading, base_extractiveness, 1990, 0.7).
narrative_ontology:measurement(sepa_be_t2000, separation_of_powers_text__unitary_executive_reading, base_extractiveness, 2000, 0.75).
narrative_ontology:measurement(sepa_be_t2010, separation_of_powers_text__unitary_executive_reading, base_extractiveness, 2010, 0.8).
narrative_ontology:measurement(sepa_be_t2020, separation_of_powers_text__unitary_executive_reading, base_extractiveness, 2020, 0.83).
narrative_ontology:measurement(sepa_be_t2025, separation_of_powers_text__unitary_executive_reading, base_extractiveness, 2025, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(sepa_su_t1980, separation_of_powers_text__unitary_executive_reading, suppression_requirement, 1980, 0.55).
narrative_ontology:measurement(sepa_su_t1990, separation_of_powers_text__unitary_executive_reading, suppression_requirement, 1990, 0.65).
narrative_ontology:measurement(sepa_su_t2000, separation_of_powers_text__unitary_executive_reading, suppression_requirement, 2000, 0.7).
narrative_ontology:measurement(sepa_su_t2010, separation_of_powers_text__unitary_executive_reading, suppression_requirement, 2010, 0.75).
narrative_ontology:measurement(sepa_su_t2020, separation_of_powers_text__unitary_executive_reading, suppression_requirement, 2020, 0.77).
narrative_ontology:measurement(sepa_su_t2025, separation_of_powers_text__unitary_executive_reading, suppression_requirement, 2025, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(separation_of_powers_text__unitary_executive_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(separation_of_powers_text__unitary_executive_reading, administrative_state_legitimacy).
narrative_ontology:affects_constraint(separation_of_powers_text__unitary_executive_reading, congressional_delegation_doctrine).
narrative_ontology:affects_constraint(separation_of_powers_text__unitary_executive_reading, presidential_removal_power).

% DUAL FORMULATION NOTE:
% This constraint is one of three distinct readings of the 'separation_of_powers_text' kernel, each with different structural implications for the U.S. government. This reading focuses on the internal structure of the executive branch.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
