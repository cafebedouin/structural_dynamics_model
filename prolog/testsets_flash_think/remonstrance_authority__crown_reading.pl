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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
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
 *   constraint_id: remonstrance_authority__crown_reading
 *   human_readable: Remonstrance Right as Illegitimate Minoritarian Veto (Crown Reading)
 *   domain: Constitutional History/Political Economy/Legal Authority
 *
 * SUMMARY:
 *   This constraint story analyzes the 'remonstrance right' from the
 *   perspective of the Crown, viewing it as an illegitimate minoritarian veto
 *   that protected particularist privileges and obstructed royal authority.
 *   This is one reading of the 'remonstrance_authority' kernel, contrasting
 *   with the 'magistrate_reading' which frames it as a fundamental
 *   constitutional check. The Crown's perspective emphasizes the extractive
 *   and suppressive nature of the right, as it diverted resources and power
 *   away from the central government and towards entrenched local interests.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(remonstrance_authority__crown_reading, 0.85).
domain_priors:suppression_score(remonstrance_authority__crown_reading, 0.78).
domain_priors:theater_ratio(remonstrance_authority__crown_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(remonstrance_authority__crown_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(remonstrance_authority__crown_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(remonstrance_authority__crown_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(remonstrance_authority__crown_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(remonstrance_authority__crown_reading, resistance, 0.8).

% --- Constraint claim ---
narrative_ontology:constraint_claim(remonstrance_authority__crown_reading, snare).
narrative_ontology:human_readable(remonstrance_authority__crown_reading, "Remonstrance Right as Illegitimate Minoritarian Veto (Crown Reading)").
narrative_ontology:topic_domain(remonstrance_authority__crown_reading, "Constitutional History/Political Economy/Legal Authority").

domain_priors:requires_active_enforcement(remonstrance_authority__crown_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(remonstrance_authority__crown_reading, '9a885b63-3992-40e9-ab3b-6005a4740e44').
narrative_ontology:cs_kernel_codification('9a885b63-3992-40e9-ab3b-6005a4740e44', formalized).
narrative_ontology:cs_authority_grounding('9a885b63-3992-40e9-ab3b-6005a4740e44', extraction).
narrative_ontology:cs_interpretation_layer_present('9a885b63-3992-40e9-ab3b-6005a4740e44').
narrative_ontology:cs_reading_relation('9a885b63-3992-40e9-ab3b-6005a4740e44', remonstrance_authority__magistrate_reading, forecloses).
narrative_ontology:cs_axiom('9a885b63-3992-40e9-ab3b-6005a4740e44', foundational, royal_prerogative_supremacy).
narrative_ontology:cs_axiom_status(royal_prerogative_supremacy, holdable).
narrative_ontology:cs_axiom_grounding('9a885b63-3992-40e9-ab3b-6005a4740e44', royal_prerogative_supremacy, deontological).
narrative_ontology:cs_axiom('9a885b63-3992-40e9-ab3b-6005a4740e44', foundational, particularist_privilege_as_obstruction).
narrative_ontology:cs_axiom_status(particularist_privilege_as_obstruction, holdable).
narrative_ontology:cs_axiom_grounding('9a885b63-3992-40e9-ab3b-6005a4740e44', particularist_privilege_as_obstruction, conventional).
narrative_ontology:cs_reference_frame('9a885b63-3992-40e9-ab3b-6005a4740e44', unfettered_royal_authority).
narrative_ontology:cs_drift_state('9a885b63-3992-40e9-ab3b-6005a4740e44', era_of_parliamentary_ascendancy, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('9a885b63-3992-40e9-ab3b-6005a4740e44', '').
narrative_ontology:cs_kernel_id(remonstrance_authority__crown_reading, remonstrance_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(remonstrance_authority__crown_reading, magistrates).
narrative_ontology:constraint_beneficiary(remonstrance_authority__crown_reading, privileged_estates).
narrative_ontology:constraint_victim(remonstrance_authority__crown_reading, the_crown).
narrative_ontology:constraint_victim(remonstrance_authority__crown_reading, general_populace).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(remonstrance_authority__crown_reading, royal_advisors).
narrative_ontology:constraint_vindicates(remonstrance_authority__crown_reading, particularist_privilege_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Seeks to implement policies for fiscal stability, administrative reform, or national defense, but is frequently thwarted by the magistrates' assertion of the remonstrance right. This leads to loss of revenue, delayed reforms, and erosion of royal authority.
narrative_ontology:constraint_stakeholder(remonstrance_authority__crown_reading, the_crown, payer,
    institutional, civilizational, constrained, national).

% Assert the right of remonstrance, effectively vetoing royal decrees. From the Crown's perspective, they use this power to protect their own corporate privileges and those of the estates they represent, obstructing necessary reforms under the guise of defending ancient liberties. Their identity is fused with this traditional role.
narrative_ontology:constraint_stakeholder(remonstrance_authority__crown_reading, magistrates, agenda_setter,
    powerful, biographical, identity_locked, national).

% Benefit directly from the magistrates' use of remonstrance, as it protects their traditional tax exemptions, feudal rights, and other particularist privileges from royal attempts at centralization or fiscal equalization. They are the ultimate recipients of the extraction from the Crown.
narrative_ontology:constraint_stakeholder(remonstrance_authority__crown_reading, privileged_estates, beneficiary,
    powerful, generational, constrained, national).

% Bears the indirect costs of royal fiscal instability, administrative inefficiency, and the perpetuation of an inequitable tax burden when reforms are blocked. They have no direct voice in the remonstrance process and are trapped by the existing power structures.
narrative_ontology:constraint_stakeholder(remonstrance_authority__crown_reading, general_populace, payer,
    powerless, biographical, trapped, national).

% Bear the political and administrative costs of navigating or circumventing remonstrances. They must devise alternative, often less efficient or more politically costly, policies to achieve the Crown's objectives, leading to frustration and resource drain.
narrative_ontology:constraint_stakeholder(remonstrance_authority__crown_reading, royal_advisors, payer,
    institutional, biographical, constrained, national).

% Analyze the historical and legal implications of the remonstrance right, debating its constitutional legitimacy and impact on governance. They can articulate the Crown's perspective on the right as an illegitimate obstruction to effective rule.
narrative_ontology:constraint_stakeholder(remonstrance_authority__crown_reading, constitutional_theorists, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(remonstrance_authority__crown_reading, magistrates).
narrative_ontology:fixing_cost_class(remonstrance_authority__crown_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: From the Crown's perspective, the remonstrance right serves no legitimate coordination function; it is primarily an obstructive mechanism.
% TRANSFER_FUNCTION: Transfers effective legislative initiative, fiscal authority, and administrative flexibility from the Crown to particularist interests represented by the magistrates, thereby preserving their privileges.
% ABSENT_VOICES: The broader national interest, the unrepresented commoners, and proponents of centralized, efficient governance are largely absent from the remonstrance process. They would argue for the supremacy of royal authority and the general welfare over particularist privileges.
% DISAPPEARANCE_RATIONALE: If the remonstrance right vanished overnight, the Crown's legislative and fiscal authority would be significantly strengthened. This would lead to more centralized governance, potentially different economic policies, and a fundamental shift in the balance of power between the monarchy and the privileged estates, reorganizing the entire political economy.
% FOUNDING_PROBLEM: From the Crown's perspective, the remonstrance right was not built to solve a legitimate problem of governance or public welfare. Instead, it emerged as an assertion of power by magistrates to protect their own corporate and class interests against necessary royal reforms and the development of a more unified state.
% FOUNDING_PROBLEM_CORROBORATION: Royal edicts, historical accounts from court chroniclers, and later constitutional reforms that curtailed such rights (e.g., during periods of absolutism or revolutionary change) corroborate the view that the remonstrance was an obstruction to legitimate governance, rather than a solution to a genuine problem. Independent historians often highlight the self-interested nature of its assertion by magistrates.
narrative_ontology:disappearance_verdict(remonstrance_authority__crown_reading, world_rearranges).
narrative_ontology:founding_problem_status(remonstrance_authority__crown_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(remonstrance_authority__crown_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(remonstrance_authority__crown_reading, 'none', 1).
narrative_ontology:epsilon_provenance(remonstrance_authority__crown_reading, 0.85, 'gemini-2.5-flash', 'none', direct).

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
 *   Extractiveness is high (0.85) because the remonstrance directly prevented the Crown from enacting fiscal and administrative reforms, effectively extracting potential revenue and legislative power. Suppression is also high (0.78) as the right actively suppressed royal initiatives and the Crown's ability to govern effectively, requiring significant effort to circumvent or overcome. The theater ratio is low (0.15) because, from the Crown's view, the remonstrance was a highly functional, if illegitimate, veto, not a mere performance. Resistance is high (0.80) as the Crown consistently sought to limit or abolish the right, viewing it as a direct challenge to its sovereignty. Accessibility collapse is moderate (0.65) because while the remonstrance created significant barriers, the Crown could sometimes find alternative means to achieve its goals, albeit at higher cost.
 *
 * PERSPECTIVAL GAP:
 *   The Crown's perspective (Snare) fundamentally diverges from the magistrates' perspective (which would likely classify it as a Rope or Scaffold). The Crown experiences the remonstrance as a direct extraction of its legitimate authority and resources, while the magistrates perceive it as a necessary defense of constitutional order and liberties. The engine's per-seat classification will highlight this structural asymmetry.
 *
 * DIRECTIONALITY LOGIC:
 *   The Crown and the general populace are the primary targets/victims, bearing the costs of obstructed governance and inequitable fiscal burdens. Magistrates and privileged estates are the clear beneficiaries, as the right directly serves to protect and enhance their power and economic advantages. Royal advisors, while institutional, also bear costs in navigating this obstruction.
 *
 * MANDATROPHY ANALYSIS:
 *   From the Crown's perspective, the remonstrance right's 'mandate' (if one ever existed beyond self-interest) has long atrophied. It is seen not as a solution to a legitimate problem, but as a persistent obstruction that has outlived any conceivable utility for the broader realm, serving only to perpetuate particularist privileges at the expense of national interest.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    legitimacy_of_veto,
    'Is the remonstrance right a legitimate constitutional check on arbitrary power, or an illegitimate obstruction to necessary governance and a tool for particularist interests?',
    'Analysis of historical outcomes, constitutional theory from different eras, and the long-term impact on state capacity and public welfare. The ''magistrate_reading'' would offer a contrasting analysis.',
    'If deemed legitimate, the constraint''s extractiveness from the Crown would be re-evaluated as a necessary cost of coordination (e.g., a Tangled Rope); if illegitimate, its Snare classification is reinforced.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(legitimacy_of_veto, conceptual, 'Ambiguity regarding the constitutional legitimacy and function of the remonstrance right.').

omega_variable(
    true_beneficiaries_of_remonstrance,
    'Did the remonstrance primarily protect ancient liberties for the common good, or particularist privileges for a narrow elite?',
    'Detailed historical-economic analysis of who benefited materially from successful remonstrances (e.g., tax exemptions, feudal rights) versus who bore the costs, and whether these benefits diffused to the broader populace.',
    'If benefits were broadly distributed, the constraint might lean towards a Tangled Rope; if concentrated on a narrow elite, the Snare classification is strengthened, and the Crown''s victim status is more pronounced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(true_beneficiaries_of_remonstrance, empirical, 'Uncertainty about the actual distribution of benefits from the remonstrance right.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(remonstrance_authority__crown_reading, 1600, 1800).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(remo_tr_t1600, remonstrance_authority__crown_reading, theater_ratio, 1600, 0.2).
narrative_ontology:measurement(remo_tr_t1640, remonstrance_authority__crown_reading, theater_ratio, 1640, 0.18).
narrative_ontology:measurement(remo_tr_t1680, remonstrance_authority__crown_reading, theater_ratio, 1680, 0.16).
narrative_ontology:measurement(remo_tr_t1720, remonstrance_authority__crown_reading, theater_ratio, 1720, 0.15).
narrative_ontology:measurement(remo_tr_t1760, remonstrance_authority__crown_reading, theater_ratio, 1760, 0.15).
narrative_ontology:measurement(remo_tr_t1800, remonstrance_authority__crown_reading, theater_ratio, 1800, 0.15).

% Extraction over time
narrative_ontology:measurement(remo_be_t1600, remonstrance_authority__crown_reading, base_extractiveness, 1600, 0.65).
narrative_ontology:measurement(remo_be_t1640, remonstrance_authority__crown_reading, base_extractiveness, 1640, 0.72).
narrative_ontology:measurement(remo_be_t1680, remonstrance_authority__crown_reading, base_extractiveness, 1680, 0.78).
narrative_ontology:measurement(remo_be_t1720, remonstrance_authority__crown_reading, base_extractiveness, 1720, 0.82).
narrative_ontology:measurement(remo_be_t1760, remonstrance_authority__crown_reading, base_extractiveness, 1760, 0.84).
narrative_ontology:measurement(remo_be_t1800, remonstrance_authority__crown_reading, base_extractiveness, 1800, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(remo_su_t1600, remonstrance_authority__crown_reading, suppression_requirement, 1600, 0.55).
narrative_ontology:measurement(remo_su_t1640, remonstrance_authority__crown_reading, suppression_requirement, 1640, 0.62).
narrative_ontology:measurement(remo_su_t1680, remonstrance_authority__crown_reading, suppression_requirement, 1680, 0.68).
narrative_ontology:measurement(remo_su_t1720, remonstrance_authority__crown_reading, suppression_requirement, 1720, 0.73).
narrative_ontology:measurement(remo_su_t1760, remonstrance_authority__crown_reading, suppression_requirement, 1760, 0.76).
narrative_ontology:measurement(remo_su_t1800, remonstrance_authority__crown_reading, suppression_requirement, 1800, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(remonstrance_authority__crown_reading, royal_fiscal_authority).
narrative_ontology:affects_constraint(remonstrance_authority__crown_reading, magistrate_judicial_independence).
narrative_ontology:affects_constraint(remonstrance_authority__crown_reading, national_taxation_system).

% DUAL FORMULATION NOTE:
% This story is the 'crown_reading' of the 'remonstrance_authority' kernel, which also has a 'magistrate_reading'. The two readings offer fundamentally different structural analyses of the same historical phenomenon.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
