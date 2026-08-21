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
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: remonstrance_authority__magistrate_reading
 *   human_readable: Remonstrance Right (Magistrate Reading)
 *   domain: constitutional_history/political_economy/legal_authority
 *
 * SUMMARY:
 *   This constraint represents the 'magistrate reading' of the French
 *   remonstrance right, where the Parlements asserted their role as
 *   constitutional guardians against arbitrary royal innovation, particularly
 *   fiscal reforms. This reading frames the right as a fundamental mechanism
 *   for preserving ancient liberties. However, its operation often resulted
 *   in the protection of particularist privileges (like tax exemptions for
 *   the magistracy) at the expense of broader public good and effective
 *   governance. The constraint is claimed as a Rope by its proponents, but
 *   its actual operation, as described by the metrics, is more consistent
 *   with a Tangled Rope due to its high extraction and active enforcement to
 *   maintain asymmetric benefits.
 *
 * KEY AGENTS:
 *   - parlements_magistracy: Primary beneficiary and agenda-setter (institutional/identity_locked)
 *   - crown_fiscal_reforms: Primary target/payer (institutional/constrained)
 *   - taxable_population: Primary victim (powerless/trapped)
 *   - ancient_liberties_doctrine: Abstract beneficiary (analytical/analytical)
 *   - crown_ministers: Secondary agenda-setter (powerful/constrained)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(remonstrance_authority__magistrate_reading, 0.65).
domain_priors:suppression_score(remonstrance_authority__magistrate_reading, 0.7).
domain_priors:theater_ratio(remonstrance_authority__magistrate_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(remonstrance_authority__magistrate_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(remonstrance_authority__magistrate_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(remonstrance_authority__magistrate_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(remonstrance_authority__magistrate_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(remonstrance_authority__magistrate_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(remonstrance_authority__magistrate_reading, tangled_rope).
narrative_ontology:human_readable(remonstrance_authority__magistrate_reading, "Remonstrance Right (Magistrate Reading)").
narrative_ontology:topic_domain(remonstrance_authority__magistrate_reading, "constitutional_history/political_economy/legal_authority").

domain_priors:requires_active_enforcement(remonstrance_authority__magistrate_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(remonstrance_authority__magistrate_reading, 'd9fa0e31-068a-4fb2-a84e-16ee24047ebe').
narrative_ontology:cs_kernel_codification('d9fa0e31-068a-4fb2-a84e-16ee24047ebe', formalized).
narrative_ontology:cs_authority_grounding('d9fa0e31-068a-4fb2-a84e-16ee24047ebe', lineage).
narrative_ontology:cs_interpretation_layer_present('d9fa0e31-068a-4fb2-a84e-16ee24047ebe').
narrative_ontology:cs_reading_relation('d9fa0e31-068a-4fb2-a84e-16ee24047ebe', remonstrance_authority__crown_reading, coexists_with).
narrative_ontology:cs_axiom('d9fa0e31-068a-4fb2-a84e-16ee24047ebe', foundational, parlements_guardians_of_fundamental_laws).
narrative_ontology:cs_axiom_status(parlements_guardians_of_fundamental_laws, holdable).
narrative_ontology:cs_axiom_grounding('d9fa0e31-068a-4fb2-a84e-16ee24047ebe', parlements_guardians_of_fundamental_laws, conventional).
narrative_ontology:cs_axiom('d9fa0e31-068a-4fb2-a84e-16ee24047ebe', foundational, royal_edicts_must_conform_to_ancient_liberties).
narrative_ontology:cs_axiom_status(royal_edicts_must_conform_to_ancient_liberties, holdable).
narrative_ontology:cs_axiom_grounding('d9fa0e31-068a-4fb2-a84e-16ee24047ebe', royal_edicts_must_conform_to_ancient_liberties, deontological).
narrative_ontology:cs_reference_frame('d9fa0e31-068a-4fb2-a84e-16ee24047ebe', constitutional_balance_of_powers).
narrative_ontology:cs_drift_state('d9fa0e31-068a-4fb2-a84e-16ee24047ebe', pre_french_revolution_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('d9fa0e31-068a-4fb2-a84e-16ee24047ebe', '').
narrative_ontology:cs_kernel_id(remonstrance_authority__magistrate_reading, remonstrance_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(remonstrance_authority__magistrate_reading, parlements_magistracy).
narrative_ontology:constraint_beneficiary(remonstrance_authority__magistrate_reading, ancient_liberties_doctrine).
narrative_ontology:constraint_victim(remonstrance_authority__magistrate_reading, crown_fiscal_reforms).
narrative_ontology:constraint_victim(remonstrance_authority__magistrate_reading, taxable_population).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The hereditary judicial bodies (Parlements) who claimed the right to remonstrate against royal edicts, asserting their role as guardians of fundamental laws and ancient liberties. They benefited from the status quo, particularly their own tax exemptions, and used the remonstrance to block reforms that threatened their privileges. Their identity was fused with this constitutional role.
narrative_ontology:constraint_stakeholder(remonstrance_authority__magistrate_reading, parlements_magistracy, agenda_setter,
    institutional, generational, identity_locked, national).

% The royal government's attempts to implement fiscal and administrative reforms, often aimed at centralizing power and increasing revenue. These reforms were consistently obstructed by the Parlements' remonstrances, leading to a constant struggle for legislative authority. The Crown bore the cost of delayed or abandoned reforms.
narrative_ontology:constraint_stakeholder(remonstrance_authority__magistrate_reading, crown_fiscal_reforms, payer,
    institutional, immediate, constrained, national).

% The common people and non-privileged classes who bore the brunt of an inequitable tax system that the Parlements' actions often preserved. They were victims of the system's inertia, unable to exit or effectively resist the combined weight of royal and magisterial authority.
narrative_ontology:constraint_stakeholder(remonstrance_authority__magistrate_reading, taxable_population, payer,
    powerless, biographical, trapped, national).

% The abstract legal and philosophical concept that the remonstrance right was meant to uphold. This doctrine provided the ideological justification for the Parlements' actions, even when those actions served particularist interests. It 'benefited' by being continually invoked and reinforced as a foundational principle.
narrative_ontology:constraint_stakeholder(remonstrance_authority__magistrate_reading, ancient_liberties_doctrine, beneficiary,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(remonstrance_authority__magistrate_reading, ancient_liberties_doctrine).

% The King's advisors and administrators tasked with implementing royal policy. They were directly confronted by the Parlements' resistance and had to navigate the political and legal complexities of overcoming remonstrances, often through 'lits de justice' (royal sessions to force registration of edicts).
narrative_ontology:constraint_stakeholder(remonstrance_authority__magistrate_reading, crown_ministers, agenda_setter,
    powerful, biographical, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provided a mechanism for judicial review of royal edicts, theoretically ensuring that new laws conformed to existing fundamental laws and traditions, thereby coordinating legal continuity and preventing arbitrary rule.
% TRANSFER_FUNCTION: Transferred legislative authority and fiscal control from the Crown to the Parlements, allowing the magistracy to preserve its own privileges (e.g., tax exemptions) and block reforms that would have redistributed wealth or power.
% ABSENT_VOICES: The unrepresented commoners and the nascent bourgeoisie, who would have advocated for more equitable taxation and a more efficient, centralized administration, were largely excluded from the formal remonstrance process. Their interests were often sacrificed to the power struggle between Crown and Parlements.
% DISAPPEARANCE_RATIONALE: If the remonstrance right vanished, the Crown would have gained unchecked legislative power, potentially leading to more rapid fiscal and administrative reforms. The Parlements would lose their primary constitutional leverage, and the balance of power in the ancien régime would fundamentally shift, likely accelerating the move towards a more centralized state.
% FOUNDING_PROBLEM: To ensure that royal edicts were consistent with the fundamental laws of the kingdom and to provide a check against arbitrary monarchical power, thereby preserving the ancient liberties of the French people.
% FOUNDING_PROBLEM_CORROBORATION: The Parlements themselves and historical legal scholars attest to the founding problem's live status, emphasizing the need for constitutional checks. However, the Crown and many contemporary historians argue that the problem was largely superseded by the Parlements' self-serving use of the right, making its status 'dead' as a genuine check on arbitrary power, instead serving particularist interests. Independent historical analysis supports the latter view.
narrative_ontology:disappearance_verdict(remonstrance_authority__magistrate_reading, world_rearranges).
narrative_ontology:founding_problem_status(remonstrance_authority__magistrate_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(remonstrance_authority__magistrate_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(remonstrance_authority__magistrate_reading, 'none', 1).
narrative_ontology:epsilon_provenance(remonstrance_authority__magistrate_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

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
 *   Extractiveness is high (0.65) because the remonstrance right, in this reading, effectively allowed the Parlements to block reforms that would have imposed taxes on the privileged, thereby extracting wealth from the broader taxable population and concentrating it among the magistracy. Suppression (0.70) is also high, as the Crown had to resort to coercive measures (lits de justice) to overcome the Parlements' resistance, and the taxable population had no effective means of challenging the system. Theater ratio (0.40) reflects the performative aspect of the remonstrances, often serving as a public display of constitutional defense while masking the protection of vested interests. The increasing extractiveness and theater over time reflect the growing tension and the Parlements' increasingly self-serving use of the right leading up to the French Revolution.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the Parlements (agenda-setter/beneficiary), the remonstrance was a legitimate constitutional check, a Rope preserving fundamental laws. From the perspective of the Crown (payer/target), it was an illegitimate obstruction, a Snare. From the perspective of the taxable population (victim), it was a Tangled Rope, preserving some semblance of 'liberty' while extracting resources through an inequitable system. The engine's computation will reflect these divergences based on the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   The Parlements magistracy is a clear beneficiary (d near 0.0) as they directly benefit from the preservation of their privileges and the obstruction of reforms that would affect them. The Crown's fiscal reforms are the primary target (d near 1.0) as they are directly thwarted by the constraint. The taxable population is also a victim (d near 1.0) as they bear the costs of the system's inertia and inequity. The 'ancient_liberties_doctrine' is an abstract beneficiary, providing ideological cover. Crown ministers, while powerful, are constrained by the remonstrance, making them targets of its obstructive power.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate was to preserve ancient liberties and check arbitrary power. However, over time, its function drifted to protecting the particularist privileges of the magistracy, leading to a 'contested' status for its founding problem. The high extractiveness and suppression, coupled with the rising theater ratio, indicate that the coordination function (ensuring legal continuity) became increasingly intertwined with, and overshadowed by, the extractive function (protecting privilege). This prevents mislabeling it as a pure Rope (which would ignore the extraction) or a pure Snare (which would ignore the initial coordination claim and the genuine constitutional struggle).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    magistrate_vs_crown_framing,
    'Is the remonstrance right primarily a constitutional check on arbitrary power (magistrate reading) or an illegitimate minoritarian veto protecting particularist privileges (crown reading)?',
    'Historical analysis of the actual impact of remonstrances on governance and social equity, distinguishing between stated intent and observed outcomes. Examination of the specific content of blocked edicts and the beneficiaries of their obstruction.',
    'If primarily a constitutional check, the constraint leans towards Rope/Scaffold. If primarily a self-serving veto, it leans towards Tangled Rope/Snare, with higher effective extraction for the Crown and taxable population.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(magistrate_vs_crown_framing, conceptual, 'Ambiguity in the fundamental nature and purpose of the remonstrance right.').

omega_variable(
    identity_lock_of_magistracy,
    'To what extent was the Parlements'' resistance driven by a genuine belief in their constitutional role versus the protection of their hereditary privileges and social status?',
    'Detailed biographical studies of key magistrates, analysis of their personal financial interests, and comparison of their public pronouncements with private correspondence or actions. Counterfactual analysis of how they might have acted if their privileges were not at stake.',
    'If primarily driven by self-interest, the ''identity_locked'' exit option for the magistracy is more accurately ''constrained'' by material interest, increasing their effective extractiveness. If genuine constitutional belief, their directionality might shift slightly towards symmetric, reflecting a perceived cost of upholding principle.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_of_magistracy, empirical, 'The true motivation behind the magistracy''s identity-locked position.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression (e.g., through lits de justice) structural (Crown''s legal authority) or internalized (Parlements'' belief in their own constitutional inviolability)?',
    'Analysis of the psychological impact of royal coercion on the Parlements, and the extent to which they continued to resist even after formal submission. If resistance persisted through informal means, it suggests internalized suppression.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests, as the Parlements carried the suppression with them after formal submission. If purely structural, the suppression is externally imposed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for the Parlements.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(remonstrance_authority__magistrate_reading, 1650, 1789).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(remo_tr_t1650, remonstrance_authority__magistrate_reading, theater_ratio, 1650, 0.2).
narrative_ontology:measurement(remo_tr_t1680, remonstrance_authority__magistrate_reading, theater_ratio, 1680, 0.25).
narrative_ontology:measurement(remo_tr_t1710, remonstrance_authority__magistrate_reading, theater_ratio, 1710, 0.3).
narrative_ontology:measurement(remo_tr_t1740, remonstrance_authority__magistrate_reading, theater_ratio, 1740, 0.35).
narrative_ontology:measurement(remo_tr_t1770, remonstrance_authority__magistrate_reading, theater_ratio, 1770, 0.4).
narrative_ontology:measurement(remo_tr_t1789, remonstrance_authority__magistrate_reading, theater_ratio, 1789, 0.4).

% Extraction over time
narrative_ontology:measurement(remo_be_t1650, remonstrance_authority__magistrate_reading, base_extractiveness, 1650, 0.5).
narrative_ontology:measurement(remo_be_t1680, remonstrance_authority__magistrate_reading, base_extractiveness, 1680, 0.55).
narrative_ontology:measurement(remo_be_t1710, remonstrance_authority__magistrate_reading, base_extractiveness, 1710, 0.6).
narrative_ontology:measurement(remo_be_t1740, remonstrance_authority__magistrate_reading, base_extractiveness, 1740, 0.63).
narrative_ontology:measurement(remo_be_t1770, remonstrance_authority__magistrate_reading, base_extractiveness, 1770, 0.65).
narrative_ontology:measurement(remo_be_t1789, remonstrance_authority__magistrate_reading, base_extractiveness, 1789, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(remo_su_t1650, remonstrance_authority__magistrate_reading, suppression_requirement, 1650, 0.6).
narrative_ontology:measurement(remo_su_t1680, remonstrance_authority__magistrate_reading, suppression_requirement, 1680, 0.65).
narrative_ontology:measurement(remo_su_t1710, remonstrance_authority__magistrate_reading, suppression_requirement, 1710, 0.68).
narrative_ontology:measurement(remo_su_t1740, remonstrance_authority__magistrate_reading, suppression_requirement, 1740, 0.7).
narrative_ontology:measurement(remo_su_t1770, remonstrance_authority__magistrate_reading, suppression_requirement, 1770, 0.7).
narrative_ontology:measurement(remo_su_t1789, remonstrance_authority__magistrate_reading, suppression_requirement, 1789, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(remonstrance_authority__magistrate_reading, enforcement_mechanism).

% DUAL FORMULATION NOTE:
% This constraint is one reading ('magistrate_reading') of the 'remonstrance_authority' kernel. The sibling reading is 'crown_reading'. This reading emphasizes the constitutional role of the Parlements, while the 'crown_reading' emphasizes their particularist obstruction.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
