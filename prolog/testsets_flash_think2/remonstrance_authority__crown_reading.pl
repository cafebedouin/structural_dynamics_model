% ============================================================================
% CONSTRAINT STORY: remonstrance_authority__crown_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_remonstrance_authority__crown_reading, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: remonstrance_authority__crown_reading
 *   human_readable: Remonstrance Right as Illegitimate Minoritarian Veto (Crown Reading)
 *   domain: constitutional_history/political_economy/legal_authority
 *
 * SUMMARY:
 *   This constraint represents the 'crown_reading' of the historical
 *   remonstrance right, particularly in the early modern period. From this
 *   perspective, the right of remonstrance, exercised by magistrates, is
 *   viewed not as a legitimate constitutional check but as an illegitimate
 *   minoritarian veto that protects particularist privileges and obstructs
 *   royal fiscal authority. The Crown perceives it as a snare, extracting
 *   power and resources by suppressing its ability to govern effectively.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(remonstrance_authority__crown_reading, 0.8).
domain_priors:suppression_score(remonstrance_authority__crown_reading, 0.75).
domain_priors:theater_ratio(remonstrance_authority__crown_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(remonstrance_authority__crown_reading, extractiveness, 0.8).
narrative_ontology:constraint_metric(remonstrance_authority__crown_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(remonstrance_authority__crown_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(remonstrance_authority__crown_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(remonstrance_authority__crown_reading, resistance, 0.8).

% --- Constraint claim ---
narrative_ontology:constraint_claim(remonstrance_authority__crown_reading, snare).
narrative_ontology:human_readable(remonstrance_authority__crown_reading, "Remonstrance Right as Illegitimate Minoritarian Veto (Crown Reading)").
narrative_ontology:topic_domain(remonstrance_authority__crown_reading, "constitutional_history/political_economy/legal_authority").

domain_priors:requires_active_enforcement(remonstrance_authority__crown_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(remonstrance_authority__crown_reading, '0e73185b-4843-4d8e-81f4-18b089bbd5b5').
narrative_ontology:cs_kernel_codification('0e73185b-4843-4d8e-81f4-18b089bbd5b5', fixed_text).
narrative_ontology:cs_authority_grounding('0e73185b-4843-4d8e-81f4-18b089bbd5b5', lineage).
narrative_ontology:cs_interpretation_layer_present('0e73185b-4843-4d8e-81f4-18b089bbd5b5').
narrative_ontology:cs_reading_relation('0e73185b-4843-4d8e-81f4-18b089bbd5b5', remonstrance_authority__magistrate_reading, forecloses).
narrative_ontology:cs_axiom('0e73185b-4843-4d8e-81f4-18b089bbd5b5', foundational, royal_prerogative_absolute).
narrative_ontology:cs_axiom_status(royal_prerogative_absolute, holdable).
narrative_ontology:cs_axiom_grounding('0e73185b-4843-4d8e-81f4-18b089bbd5b5', royal_prerogative_absolute, deontological).
narrative_ontology:cs_axiom('0e73185b-4843-4d8e-81f4-18b089bbd5b5', secondary, parliamentary_privilege_subordinate).
narrative_ontology:cs_axiom_status(parliamentary_privilege_subordinate, holdable).
narrative_ontology:cs_axiom_grounding('0e73185b-4843-4d8e-81f4-18b089bbd5b5', parliamentary_privilege_subordinate, conventional).
narrative_ontology:cs_reference_frame('0e73185b-4843-4d8e-81f4-18b089bbd5b5', royal_prerogative_supremacy).
narrative_ontology:cs_drift_state('0e73185b-4843-4d8e-81f4-18b089bbd5b5', early_stuart_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('0e73185b-4843-4d8e-81f4-18b089bbd5b5', '').
narrative_ontology:cs_kernel_id(remonstrance_authority__crown_reading, remonstrance_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(remonstrance_authority__crown_reading, magistrates).
narrative_ontology:constraint_beneficiary(remonstrance_authority__crown_reading, privileged_estates).
narrative_ontology:constraint_victim(remonstrance_authority__crown_reading, the_crown).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The sovereign authority seeking to implement fiscal policies and assert royal prerogative. Experiences the remonstrance as an illegitimate obstruction to governance and a direct challenge to its authority, forcing concessions or delaying essential revenue.
narrative_ontology:constraint_stakeholder(remonstrance_authority__crown_reading, the_crown, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(remonstrance_authority__crown_reading, the_crown, payer).

% Local and regional officials, often drawn from the gentry, who exercise the right of remonstrance. They benefit from the power to veto royal edicts, preserving their local influence and the privileges of their class. Their exit is constrained by their position within the legal and social hierarchy.
narrative_ontology:constraint_stakeholder(remonstrance_authority__crown_reading, magistrates, agenda_setter,
    organized, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(remonstrance_authority__crown_reading, magistrates, beneficiary).

% Nobles, gentry, and other powerful groups whose particularist privileges (e.g., tax exemptions, feudal rights) are protected by the magistrates' use of the remonstrance. They are the ultimate beneficiaries of the veto, as it prevents royal policies that might infringe upon their wealth or status.
narrative_ontology:constraint_stakeholder(remonstrance_authority__crown_reading, privileged_estates, beneficiary,
    powerful, generational, mobile, national).

% The general populace, who bear the costs of inefficient or stalled governance due to the conflict between Crown and magistrates, but whose interests are not directly represented by the remonstrance, which primarily serves particularist privileges. They have no effective voice in the dispute.
narrative_ontology:constraint_stakeholder(remonstrance_authority__crown_reading, common_subjects, excluded,
    powerless, immediate, trapped, local).

% Legal scholars, ministers, and courtiers who advise the Crown on matters of prerogative and law. They analyze the legal basis and political implications of the remonstrance, seeking strategies to overcome or circumvent it.
narrative_ontology:constraint_stakeholder(remonstrance_authority__crown_reading, royal_advisors, observer,
    analytical, biographical, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(remonstrance_authority__crown_reading, privileged_estates).
narrative_ontology:fixing_cost_class(remonstrance_authority__crown_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: From the Crown's perspective, the remonstrance right has no legitimate coordination function; it is an obstruction to the coordinated governance of the realm, serving only to fragment authority and protect narrow interests.
% TRANSFER_FUNCTION: Transfers effective fiscal control and legislative initiative from the Crown to the magistrates and privileged estates, allowing them to retain wealth and influence that would otherwise be subject to royal policy.
% ABSENT_VOICES: The common subjects are absent from the conversation; they would object to the political gridlock and the protection of particularist privileges at the expense of broader public welfare, but lack the power to be heard.
% DISAPPEARANCE_RATIONALE: If the remonstrance right vanished overnight, the Crown would regain full, unhindered fiscal and legislative authority, fundamentally altering the balance of power, the flow of revenue, and the structure of governance, likely leading to a more centralized and absolute monarchy.
% FOUNDING_PROBLEM: From the Crown's perspective, the remonstrance right was not established to solve a legitimate problem of governance, but rather emerged as an assertion of local and aristocratic power against the rightful authority of the sovereign.
% FOUNDING_PROBLEM_CORROBORATION: No corroboration from outside the benefiting parties exists; the Crown's own historical accounts and legal theorists consistently frame the remonstrance as an encroachment on royal prerogative, while the magistrates and privileged estates assert its legitimacy based on ancient liberties.
narrative_ontology:disappearance_verdict(remonstrance_authority__crown_reading, world_rearranges).
narrative_ontology:founding_problem_status(remonstrance_authority__crown_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(remonstrance_authority__crown_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(remonstrance_authority__crown_reading, 'none', 1).
narrative_ontology:epsilon_provenance(remonstrance_authority__crown_reading, 0.8, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(remonstrance_authority__crown_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(remonstrance_authority__crown_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(remonstrance_authority__crown_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The high extractiveness (0.8) reflects the Crown's perception of significant loss of fiscal and legislative power due to the remonstrance. Suppression (0.75) is high because the Crown's attempts to bypass or nullify the remonstrance are met with strong resistance, effectively suppressing its authority. The theater ratio is low (0.1) because the conflict is direct and substantive, not performative; the remonstrance genuinely thwarts royal will. Resistance is high (0.8) as the Crown actively seeks to overcome this constraint.
 *
 * PERSPECTIVAL GAP:
 *   The Crown's perspective (snare) fundamentally diverges from the magistrates' perspective (which would likely classify it as a rope or mountain, a legitimate check on power). This divergence is central to the historical conflict and is captured by the distinct readings of the 'remonstrance_authority' kernel.
 *
 * DIRECTIONALITY LOGIC:
 *   The Crown is the primary target/victim, experiencing direct extraction of its authority and resources. Magistrates and privileged estates are the beneficiaries, gaining power and protection of their interests. Common subjects are excluded, bearing the costs of political deadlock without direct benefit. The directionality for the Crown is near 1.0 (full target), while for magistrates and privileged estates it is near 0.0 (full beneficiary).
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    legitimacy_of_veto,
    'Is the remonstrance right a legitimate constitutional mechanism for protecting liberties, or an illegitimate minoritarian veto protecting particularist privileges?',
    'Analysis of historical legal theory and political outcomes from a neutral, non-partisan perspective, examining whether the right served broader public good or narrow class interests.',
    'If deemed legitimate, the constraint''s classification would shift towards a Rope or even Mountain from a constitutional perspective; if illegitimate, the Snare classification is reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legitimacy_of_veto, conceptual, 'Ambiguity regarding the constitutional legitimacy and purpose of the remonstrance right.').

omega_variable(
    fiscal_impact_vs_liberty_protection,
    'What was the actual fiscal impact of the remonstrance on royal governance, versus its effectiveness in protecting the ancient liberties of the realm?',
    'Empirical historical research into royal accounts, legislative records, and contemporary accounts of both fiscal crises and instances of protected liberties.',
    'If fiscal impact was negligible and liberty protection substantial, the Crown''s ''snare'' reading is weakened. If fiscal impact was severe and liberty protection minimal or particularist, the ''snare'' reading is strengthened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fiscal_impact_vs_liberty_protection, empirical, 'Empirical balance between fiscal obstruction and liberty protection.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(remonstrance_authority__crown_reading, 1600, 1640).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(remo_tr_t1600, remonstrance_authority__crown_reading, theater_ratio, 1600, 0.1).
narrative_ontology:measurement(remo_tr_t1608, remonstrance_authority__crown_reading, theater_ratio, 1608, 0.1).
narrative_ontology:measurement(remo_tr_t1616, remonstrance_authority__crown_reading, theater_ratio, 1616, 0.1).
narrative_ontology:measurement(remo_tr_t1624, remonstrance_authority__crown_reading, theater_ratio, 1624, 0.1).
narrative_ontology:measurement(remo_tr_t1632, remonstrance_authority__crown_reading, theater_ratio, 1632, 0.1).
narrative_ontology:measurement(remo_tr_t1640, remonstrance_authority__crown_reading, theater_ratio, 1640, 0.1).

% Extraction over time
narrative_ontology:measurement(remo_be_t1600, remonstrance_authority__crown_reading, base_extractiveness, 1600, 0.65).
narrative_ontology:measurement(remo_be_t1608, remonstrance_authority__crown_reading, base_extractiveness, 1608, 0.7).
narrative_ontology:measurement(remo_be_t1616, remonstrance_authority__crown_reading, base_extractiveness, 1616, 0.75).
narrative_ontology:measurement(remo_be_t1624, remonstrance_authority__crown_reading, base_extractiveness, 1624, 0.78).
narrative_ontology:measurement(remo_be_t1632, remonstrance_authority__crown_reading, base_extractiveness, 1632, 0.79).
narrative_ontology:measurement(remo_be_t1640, remonstrance_authority__crown_reading, base_extractiveness, 1640, 0.8).

% Suppression requirement over time
narrative_ontology:measurement(remo_su_t1600, remonstrance_authority__crown_reading, suppression_requirement, 1600, 0.6).
narrative_ontology:measurement(remo_su_t1608, remonstrance_authority__crown_reading, suppression_requirement, 1608, 0.65).
narrative_ontology:measurement(remo_su_t1616, remonstrance_authority__crown_reading, suppression_requirement, 1616, 0.7).
narrative_ontology:measurement(remo_su_t1624, remonstrance_authority__crown_reading, suppression_requirement, 1624, 0.73).
narrative_ontology:measurement(remo_su_t1632, remonstrance_authority__crown_reading, suppression_requirement, 1632, 0.74).
narrative_ontology:measurement(remo_su_t1640, remonstrance_authority__crown_reading, suppression_requirement, 1640, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
