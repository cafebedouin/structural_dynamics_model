% ============================================================================
% CONSTRAINT STORY: remonstrance_authority__magistrate_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_remonstrance_authority__magistrate_reading, []).

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
 *   constraint_id: remonstrance_authority__magistrate_reading
 *   human_readable: Magistrate's Reading of the Remonstrance Right
 *   domain: constitutional_history/political_economy/legal_authority
 *
 * SUMMARY:
 *   This constraint models the 'magistrate's reading' of the French
 *   remonstrance right, where the Parlements asserted their power to review
 *   and block royal edicts as a fundamental constitutional mechanism to
 *   preserve ancient liberties against arbitrary innovation. This reading
 *   frames the right as a necessary check on royal power, particularly in
 *   fiscal matters, and positions the magistracy as guardians of the
 *   kingdom's fundamental laws. The constraint is claimed as a Tangled Rope
 *   because it genuinely coordinates the legal framework while simultaneously
 *   enabling asymmetric extraction by the privileged estates.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(remonstrance_authority__magistrate_reading, 0.65).
domain_priors:suppression_score(remonstrance_authority__magistrate_reading, 0.4).
domain_priors:theater_ratio(remonstrance_authority__magistrate_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(remonstrance_authority__magistrate_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(remonstrance_authority__magistrate_reading, suppression_requirement, 0.4).
narrative_ontology:constraint_metric(remonstrance_authority__magistrate_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(remonstrance_authority__magistrate_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(remonstrance_authority__magistrate_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(remonstrance_authority__magistrate_reading, tangled_rope).
narrative_ontology:human_readable(remonstrance_authority__magistrate_reading, "Magistrate's Reading of the Remonstrance Right").
narrative_ontology:topic_domain(remonstrance_authority__magistrate_reading, "constitutional_history/political_economy/legal_authority").

domain_priors:requires_active_enforcement(remonstrance_authority__magistrate_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(remonstrance_authority__magistrate_reading, '4136406d-bac9-43cb-a366-c24d248de957').
narrative_ontology:cs_kernel_codification('4136406d-bac9-43cb-a366-c24d248de957', fixed_text).
narrative_ontology:cs_authority_grounding('4136406d-bac9-43cb-a366-c24d248de957', lineage).
narrative_ontology:cs_interpretation_layer_present('4136406d-bac9-43cb-a366-c24d248de957').
narrative_ontology:cs_reading_relation('4136406d-bac9-43cb-a366-c24d248de957', remonstrance_authority__crown_reading, coexists_with).
narrative_ontology:cs_axiom('4136406d-bac9-43cb-a366-c24d248de957', foundational, ancient_liberties_are_fundamental_law).
narrative_ontology:cs_axiom_status(ancient_liberties_are_fundamental_law, holdable).
narrative_ontology:cs_axiom_grounding('4136406d-bac9-43cb-a366-c24d248de957', ancient_liberties_are_fundamental_law, deontological).
narrative_ontology:cs_axiom('4136406d-bac9-43cb-a366-c24d248de957', foundational, parlements_are_guardians_of_fundamental_law).
narrative_ontology:cs_axiom_status(parlements_are_guardians_of_fundamental_law, holdable).
narrative_ontology:cs_axiom_grounding('4136406d-bac9-43cb-a366-c24d248de957', parlements_are_guardians_of_fundamental_law, conventional).
narrative_ontology:cs_reference_frame('4136406d-bac9-43cb-a366-c24d248de957', constitutional_monarchy_with_judicial_review).
narrative_ontology:cs_drift_state('4136406d-bac9-43cb-a366-c24d248de957', late_ancien_regime, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('4136406d-bac9-43cb-a366-c24d248de957', '').
narrative_ontology:cs_kernel_id(remonstrance_authority__magistrate_reading, remonstrance_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(remonstrance_authority__magistrate_reading, parlements_magistracy).
narrative_ontology:constraint_beneficiary(remonstrance_authority__magistrate_reading, privileged_estates).
narrative_ontology:constraint_victim(remonstrance_authority__magistrate_reading, crown_ministers).
narrative_ontology:constraint_victim(remonstrance_authority__magistrate_reading, taxable_population).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The hereditary judicial elite who claim the right to review and register royal edicts, asserting this power as a constitutional check against arbitrary royal innovation, particularly in fiscal matters. They benefit from the preservation of their own tax exemptions and the status quo.
narrative_ontology:constraint_stakeholder(remonstrance_authority__magistrate_reading, parlements_magistracy, agenda_setter,
    institutional, generational, identity_locked, national).

% Responsible for implementing royal policy, especially fiscal reforms. They experience the remonstrance right as a direct impediment to their legislative agenda, forcing delays, revisions, or the use of coercive measures like lits de justice to overcome opposition.
narrative_ontology:constraint_stakeholder(remonstrance_authority__magistrate_reading, crown_ministers, payer,
    institutional, immediate, constrained, national).

% Nobility and clergy whose traditional tax exemptions are defended by the Parlements' remonstrances. They benefit from the blocking of fiscal reforms that would redistribute the tax burden more equitably.
narrative_ontology:constraint_stakeholder(remonstrance_authority__magistrate_reading, privileged_estates, beneficiary,
    organized, generational, mobile, national).

% The commoners who bear the brunt of the existing tax system and would benefit from fiscal reforms that the remonstrance right often blocks. They are largely unrepresented in the political process and have no direct means to influence the outcome.
narrative_ontology:constraint_stakeholder(remonstrance_authority__magistrate_reading, taxable_population, payer,
    powerless, biographical, trapped, national).

% Intellectuals who analyze the remonstrance right as either a legitimate check on absolutism or a self-serving defense of aristocratic privilege, often critiquing its lack of popular representation.
narrative_ontology:constraint_stakeholder(remonstrance_authority__magistrate_reading, enlightenment_philosophes, observer,
    analytical, generational, analytical, continental).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a mechanism for the registration and formalization of royal edicts into law, ensuring a degree of judicial review and consistency with existing legal traditions, thereby coordinating the legal framework of the kingdom.
% TRANSFER_FUNCTION: Transfers the power to delay or block royal fiscal reforms from the Crown to the Parlements, effectively preserving tax exemptions and privileges for the magistracy and privileged estates, at the expense of the Crown's revenue and the taxable population.
% ABSENT_VOICES: The unrepresented commoners and nascent bourgeois classes, who would advocate for more equitable taxation and a more efficient, less arbitrary legal system, are excluded from the formal process of remonstrance and its resolution.
% DISAPPEARANCE_RATIONALE: If the remonstrance right vanished, the Crown would gain unchecked legislative power, particularly in fiscal matters. This would lead to rapid implementation of reforms, potentially altering the social and economic structure of the kingdom by shifting tax burdens and eroding traditional privileges, fundamentally reorganizing the balance of power.
% FOUNDING_PROBLEM: To ensure that royal edicts were consistent with fundamental laws and customs of the realm, preventing arbitrary rule and preserving the ancient liberties and privileges of various bodies within the kingdom.
% FOUNDING_PROBLEM_CORROBORATION: The Parlements and privileged estates attest that the problem of arbitrary innovation is still live, citing the Crown's attempts at fiscal reform. Crown ministers and Enlightenment philosophes, from outside the benefiting parties, attest that the original problem has been superseded by the Parlements' self-interested obstruction, turning a constitutional check into a mechanism for preserving privilege.
narrative_ontology:disappearance_verdict(remonstrance_authority__magistrate_reading, world_rearranges).
narrative_ontology:founding_problem_status(remonstrance_authority__magistrate_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(remonstrance_authority__magistrate_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(remonstrance_authority__magistrate_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(remonstrance_authority__magistrate_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(remonstrance_authority__magistrate_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(remonstrance_authority__magistrate_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.65) is high because the remonstrance right, in this reading, effectively blocked crucial fiscal reforms, preserving the tax exemptions of the privileged at the expense of the Crown and the commoners. Suppression (0.40) is moderate; while the Crown could use lits de justice to force registration, this was a politically costly and temporary measure, indicating that the Parlements' power was not easily suppressed. Theater ratio (0.20) is low, as the Parlements' actions had real, material consequences, even if their constitutional claims were contested. The increasing extractiveness over time reflects the growing fiscal crisis of the Ancien Régime and the Parlements' increasing obstruction of solutions.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the Parlements and privileged estates, the remonstrance right is a legitimate constitutional safeguard (closer to a Rope). From the Crown's perspective, it is an illegitimate obstruction (closer to a Snare). The engine will compute these divergent classifications based on the declared roles and metrics. The 'magistrate's reading' itself is the source of the claimed type, while the metrics reflect the actual operational impact.
 *
 * DIRECTIONALITY LOGIC:
 *   The Parlements' magistracy and the privileged estates are the primary beneficiaries (d near 0.0), as the right preserves their status and exemptions. Crown ministers and the taxable population are the victims (d near 1.0), bearing the costs of blocked reforms and an inequitable tax system. Enlightenment philosophes are analytical observers (d near 0.5).
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate, to preserve ancient liberties, became increasingly misaligned with its function, which was to preserve specific privileges. The 'magistrate's reading' prevented the constraint from being reclassified as a pure Snare by maintaining the narrative of constitutional defense, even as its extractive function became more pronounced. The contested status of the founding problem highlights this mandatrophy.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    constitutional_vs_privilege,
    'Is the remonstrance right, as interpreted by the magistracy, a genuine constitutional mechanism for checks and balances, or primarily a tool for preserving aristocratic and magisterial privilege?',
    'Analysis of the specific content of remonstrances: if they consistently defend broad public interest rather than narrow corporate privileges, it supports the constitutional claim. If they primarily block reforms that would affect the Parlements'' own interests, it supports the privilege claim.',
    'If primarily privilege-driven, the constraint''s effective extractiveness is higher and its coordination function is more theatrical, pushing it closer to a Snare. If genuinely constitutional, its coordination function is stronger, supporting a Tangled Rope or even Rope classification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(constitutional_vs_privilege, empirical, 'Ambiguity between constitutional principle and self-serving privilege.').

omega_variable(
    legitimacy_of_resistance,
    'To what extent was the Parlements'' resistance to royal edicts perceived as legitimate by the broader population, beyond the privileged estates?',
    'Analysis of public opinion, pamphlets, and popular uprisings: if popular support for the Parlements'' actions was widespread and sustained, it indicates a broader perception of legitimacy. If support was limited or conditional, it suggests a more self-serving perception.',
    'Higher perceived legitimacy would reduce the effective suppression required for the constraint to persist, as it would be sustained by broader consent rather than pure coercion. Lower legitimacy would imply higher effective suppression, as the Crown would need more force to overcome resistance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legitimacy_of_resistance, empirical, 'Public perception of the legitimacy of the remonstrance right.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(remonstrance_authority__magistrate_reading, 1650, 1789).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(remo_tr_t1650, remonstrance_authority__magistrate_reading, theater_ratio, 1650, 0.1).
narrative_ontology:measurement(remo_tr_t1680, remonstrance_authority__magistrate_reading, theater_ratio, 1680, 0.15).
narrative_ontology:measurement(remo_tr_t1710, remonstrance_authority__magistrate_reading, theater_ratio, 1710, 0.18).
narrative_ontology:measurement(remo_tr_t1740, remonstrance_authority__magistrate_reading, theater_ratio, 1740, 0.2).
narrative_ontology:measurement(remo_tr_t1770, remonstrance_authority__magistrate_reading, theater_ratio, 1770, 0.2).
narrative_ontology:measurement(remo_tr_t1789, remonstrance_authority__magistrate_reading, theater_ratio, 1789, 0.2).

% Extraction over time
narrative_ontology:measurement(remo_be_t1650, remonstrance_authority__magistrate_reading, base_extractiveness, 1650, 0.5).
narrative_ontology:measurement(remo_be_t1680, remonstrance_authority__magistrate_reading, base_extractiveness, 1680, 0.55).
narrative_ontology:measurement(remo_be_t1710, remonstrance_authority__magistrate_reading, base_extractiveness, 1710, 0.6).
narrative_ontology:measurement(remo_be_t1740, remonstrance_authority__magistrate_reading, base_extractiveness, 1740, 0.63).
narrative_ontology:measurement(remo_be_t1770, remonstrance_authority__magistrate_reading, base_extractiveness, 1770, 0.65).
narrative_ontology:measurement(remo_be_t1789, remonstrance_authority__magistrate_reading, base_extractiveness, 1789, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(remo_su_t1650, remonstrance_authority__magistrate_reading, suppression_requirement, 1650, 0.3).
narrative_ontology:measurement(remo_su_t1680, remonstrance_authority__magistrate_reading, suppression_requirement, 1680, 0.35).
narrative_ontology:measurement(remo_su_t1710, remonstrance_authority__magistrate_reading, suppression_requirement, 1710, 0.38).
narrative_ontology:measurement(remo_su_t1740, remonstrance_authority__magistrate_reading, suppression_requirement, 1740, 0.4).
narrative_ontology:measurement(remo_su_t1770, remonstrance_authority__magistrate_reading, suppression_requirement, 1770, 0.4).
narrative_ontology:measurement(remo_su_t1789, remonstrance_authority__magistrate_reading, suppression_requirement, 1789, 0.4).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(remonstrance_authority__magistrate_reading, enforcement_mechanism).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'remonstrance_authority' kernel. The 'magistrate_reading' emphasizes constitutional checks, while the 'crown_reading' (a sibling constraint) emphasizes illegitimate obstruction of royal authority.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
