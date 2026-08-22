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
 *   constraint_id: remonstrance_authority__crown_reading
 *   human_readable: Remonstrance Right as Illegitimate Minoritarian Veto (Crown Reading)
 *   domain: constitutional_history/political_economy/legal_authority
 *
 * SUMMARY:
 *   This constraint story represents the 'Crown Reading' of the remonstrance
 *   right in pre-revolutionary France. From this perspective, the right of
 *   parlements (magistrates) to remonstrate against royal edicts is not a
 *   legitimate constitutional check but an illegitimate minoritarian veto. It
 *   is seen as an obstruction to royal authority and fiscal policy,
 *   protecting particularist privileges at the expense of national interest.
 *   The Crown's efforts to override these remonstrances are framed as
 *   necessary acts of governance, not as arbitrary power grabs. The
 *   constraint is claimed as a Snare because, from the Crown's view, the
 *   remonstrance mechanism itself is a form of extraction, diverting
 *   resources and authority from the central state to regional elites.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(remonstrance_authority__crown_reading, 0.85).
domain_priors:suppression_score(remonstrance_authority__crown_reading, 0.7).
domain_priors:theater_ratio(remonstrance_authority__crown_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(remonstrance_authority__crown_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(remonstrance_authority__crown_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(remonstrance_authority__crown_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(remonstrance_authority__crown_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(remonstrance_authority__crown_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(remonstrance_authority__crown_reading, snare).
narrative_ontology:human_readable(remonstrance_authority__crown_reading, "Remonstrance Right as Illegitimate Minoritarian Veto (Crown Reading)").
narrative_ontology:topic_domain(remonstrance_authority__crown_reading, "constitutional_history/political_economy/legal_authority").

domain_priors:requires_active_enforcement(remonstrance_authority__crown_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(remonstrance_authority__crown_reading, 'af068f38-33c2-4f08-b9d3-b144f651606d').
narrative_ontology:cs_kernel_codification('af068f38-33c2-4f08-b9d3-b144f651606d', formalized).
narrative_ontology:cs_authority_grounding('af068f38-33c2-4f08-b9d3-b144f651606d', lineage).
narrative_ontology:cs_interpretation_layer_present('af068f38-33c2-4f08-b9d3-b144f651606d').
narrative_ontology:cs_reading_relation('af068f38-33c2-4f08-b9d3-b144f651606d', remonstrance_authority__magistrate_reading, coexists_with).
narrative_ontology:cs_axiom('af068f38-33c2-4f08-b9d3-b144f651606d', foundational, royal_prerogative_supreme).
narrative_ontology:cs_axiom_status(royal_prerogative_supreme, holdable).
narrative_ontology:cs_axiom_grounding('af068f38-33c2-4f08-b9d3-b144f651606d', royal_prerogative_supreme, deontological).
narrative_ontology:cs_axiom('af068f38-33c2-4f08-b9d3-b144f651606d', foundational, national_interest_over_privilege).
narrative_ontology:cs_axiom_status(national_interest_over_privilege, holdable).
narrative_ontology:cs_axiom_grounding('af068f38-33c2-4f08-b9d3-b144f651606d', national_interest_over_privilege, instrumental).
narrative_ontology:cs_reference_frame('af068f38-33c2-4f08-b9d3-b144f651606d', absolute_monarchy_fiscal_unity).
narrative_ontology:cs_drift_state('af068f38-33c2-4f08-b9d3-b144f651606d', pre_revolutionary_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('af068f38-33c2-4f08-b9d3-b144f651606d', '').
narrative_ontology:cs_kernel_id(remonstrance_authority__crown_reading, remonstrance_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(remonstrance_authority__crown_reading, crown_royal_authority).
narrative_ontology:constraint_beneficiary(remonstrance_authority__crown_reading, royal_treasury).
narrative_ontology:constraint_victim(remonstrance_authority__crown_reading, parlements_magistrates).
narrative_ontology:constraint_victim(remonstrance_authority__crown_reading, taxpayers_commoners).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(remonstrance_authority__crown_reading, royal_councillors).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The sovereign power, seeking to implement fiscal reforms and consolidate authority. Views remonstrances as an illegitimate obstruction to necessary governance and a challenge to royal prerogative. Benefits from the collection of taxes and the unhindered exercise of legislative power.
narrative_ontology:constraint_stakeholder(remonstrance_authority__crown_reading, crown_royal_authority, agenda_setter,
    institutional, generational, constrained, national).

% The financial arm of the Crown, directly benefiting from the unimpeded collection of taxes and the implementation of fiscal edicts. Suffers when remonstrances delay or prevent revenue generation.
narrative_ontology:constraint_stakeholder(remonstrance_authority__crown_reading, royal_treasury, beneficiary,
    institutional, immediate, constrained, national).

% Judicial bodies whose traditional right of remonstrance is here viewed as a self-interested veto. They are seen as protecting their own privileges and those of particularist groups, obstructing royal policy. They bear the cost of royal displeasure and potential suppression of their judicial functions.
narrative_ontology:constraint_stakeholder(remonstrance_authority__crown_reading, parlements_magistrates, payer,
    organized, biographical, identity_locked, regional).

% The general populace, who are the ultimate source of the taxes the Crown seeks to collect. From the Crown's perspective, their welfare is tied to the efficient functioning of the state, which is hampered by magisterial obstruction. They bear the direct cost of taxation, but also the indirect cost of delayed or inefficient governance.
narrative_ontology:constraint_stakeholder(remonstrance_authority__crown_reading, taxpayers_commoners, payer,
    powerless, immediate, trapped, local).

% Advisors to the Crown who advocate for strong royal authority and efficient administration. They benefit from the Crown's ability to implement policy without obstruction, as their influence and policy objectives are advanced.
narrative_ontology:constraint_stakeholder(remonstrance_authority__crown_reading, royal_councillors, beneficiary,
    powerful, biographical, mobile, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Aims to coordinate the implementation of royal edicts and fiscal policy across the realm, ensuring uniform application of law and efficient revenue collection for state functions.
% TRANSFER_FUNCTION: Transfers legislative and fiscal authority from traditional regional bodies (parlements) to the centralized royal administration, enabling the Crown to collect taxes and implement policies without local obstruction.
% ABSENT_VOICES: The broader populace, whose consent is not sought for royal edicts, and whose interests are ostensibly represented by the parlements but are here viewed as secondary to royal prerogative. They would argue for greater accountability and representation in fiscal matters.
% DISAPPEARANCE_RATIONALE: If the Crown's authority to override remonstrances vanished, the parlements would regain significant power to block royal edicts, leading to a highly fragmented and contested legislative landscape. Fiscal policy would become subject to regional vetoes, fundamentally altering the balance of power and state capacity.
% FOUNDING_PROBLEM: The Crown faced persistent challenges in consolidating state power and funding its administration and military, often encountering resistance from regional bodies and entrenched privileges that hampered national policy.
% FOUNDING_PROBLEM_CORROBORATION: Royal historians and political theorists of the era attest to the ongoing struggle for centralized authority and fiscal stability. Contemporary accounts from royal administrators and diplomatic correspondence corroborate the view that regional resistance was a significant impediment to effective governance.
narrative_ontology:disappearance_verdict(remonstrance_authority__crown_reading, world_rearranges).
narrative_ontology:founding_problem_status(remonstrance_authority__crown_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(remonstrance_authority__crown_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_gemini+stakeholder_backfill', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
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
 *   The high extractiveness (0.85) reflects the Crown's perception that the remonstrance right extracts significant fiscal and political capital by delaying or preventing royal edicts. Suppression (0.70) is also high, as the Crown actively sought to suppress these challenges through 'lits de justice' and other means. The theater ratio (0.40) indicates that while some genuine deliberation might occur, a substantial portion of the remonstrance process is seen as performative resistance designed to assert regional power rather than engage in good-faith coordination. The increasing extractiveness over time reflects the escalating fiscal crises and the Crown's growing frustration with magisterial obstruction.
 *
 * PERSPECTIVAL GAP:
 *   The Crown's perspective frames the remonstrance right as a Snare, extracting from the state. The magistrates, in their own reading, would likely classify it as a Rope or even a Mountain, a fundamental check on arbitrary power. The engine's computation of per-seat classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   The Crown and its treasury are beneficiaries, as they seek to consolidate power and revenue, viewing the remonstrance as a cost to be overcome. The parlements' magistrates and, indirectly, the common taxpayers (who bear the burden of delayed or inefficient fiscal policy) are the victims/payers. The Crown's directionality is low (beneficiary), while the magistrates' is high (target), reflecting the Crown's view of them as obstacles to be overcome.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    legitimacy_of_remonstrance,
    'Is the remonstrance right a legitimate constitutional check on royal power, or an illegitimate obstruction of national governance?',
    'Analysis of historical outcomes: did remonstrances primarily protect local privileges or genuinely prevent arbitrary rule? Examination of contemporary legal theory regarding the balance of power.',
    'If legitimate, the Crown''s actions to suppress it would be seen as pure extraction, reclassifying this constraint towards a Snare for the Crown. If illegitimate, the Crown''s actions are justified coordination, supporting a Rope or Scaffold classification for the Crown.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legitimacy_of_remonstrance, conceptual, 'Ambiguity regarding the constitutional legitimacy of the remonstrance right itself.').

omega_variable(
    fiscal_necessity_vs_royal_prerogative,
    'To what extent were the Crown''s fiscal demands genuinely necessary for national welfare and defense, versus serving to expand royal prerogative and personal enrichment?',
    'Detailed historical economic analysis of state finances, military expenditures, and royal household accounts, compared against contemporary European powers.',
    'If fiscal necessity was paramount, the Crown''s efforts to overcome remonstrances are more justifiable, potentially lowering the perceived extractiveness. If prerogative expansion was dominant, extractiveness is confirmed as high.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(fiscal_necessity_vs_royal_prerogative, empirical, 'The true motivation behind the Crown''s fiscal policies.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(remonstrance_authority__crown_reading, 1650, 1789).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(remo_tr_t1650, remonstrance_authority__crown_reading, theater_ratio, 1650, 0.3).
narrative_ontology:measurement(remo_tr_t1680, remonstrance_authority__crown_reading, theater_ratio, 1680, 0.35).
narrative_ontology:measurement(remo_tr_t1710, remonstrance_authority__crown_reading, theater_ratio, 1710, 0.38).
narrative_ontology:measurement(remo_tr_t1740, remonstrance_authority__crown_reading, theater_ratio, 1740, 0.39).
narrative_ontology:measurement(remo_tr_t1770, remonstrance_authority__crown_reading, theater_ratio, 1770, 0.4).
narrative_ontology:measurement(remo_tr_t1789, remonstrance_authority__crown_reading, theater_ratio, 1789, 0.4).

% Extraction over time
narrative_ontology:measurement(remo_be_t1650, remonstrance_authority__crown_reading, base_extractiveness, 1650, 0.75).
narrative_ontology:measurement(remo_be_t1680, remonstrance_authority__crown_reading, base_extractiveness, 1680, 0.8).
narrative_ontology:measurement(remo_be_t1710, remonstrance_authority__crown_reading, base_extractiveness, 1710, 0.82).
narrative_ontology:measurement(remo_be_t1740, remonstrance_authority__crown_reading, base_extractiveness, 1740, 0.83).
narrative_ontology:measurement(remo_be_t1770, remonstrance_authority__crown_reading, base_extractiveness, 1770, 0.84).
narrative_ontology:measurement(remo_be_t1789, remonstrance_authority__crown_reading, base_extractiveness, 1789, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(remo_su_t1650, remonstrance_authority__crown_reading, suppression_requirement, 1650, 0.6).
narrative_ontology:measurement(remo_su_t1680, remonstrance_authority__crown_reading, suppression_requirement, 1680, 0.65).
narrative_ontology:measurement(remo_su_t1710, remonstrance_authority__crown_reading, suppression_requirement, 1710, 0.68).
narrative_ontology:measurement(remo_su_t1740, remonstrance_authority__crown_reading, suppression_requirement, 1740, 0.69).
narrative_ontology:measurement(remo_su_t1770, remonstrance_authority__crown_reading, suppression_requirement, 1770, 0.7).
narrative_ontology:measurement(remo_su_t1789, remonstrance_authority__crown_reading, suppression_requirement, 1789, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(remonstrance_authority__crown_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(remonstrance_authority__crown_reading, magistrate_authority__crown_reading).
narrative_ontology:affects_constraint(remonstrance_authority__crown_reading, taxation_authority__crown_reading).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
