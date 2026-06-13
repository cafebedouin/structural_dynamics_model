% ============================================================================
% CONSTRAINT STORY: rome_statute_jurisdiction__sovereigntist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_rome_statute_jurisdiction__sovereigntist_reading, []).

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
 *   constraint_id: rome_statute_jurisdiction__sovereigntist_reading
 *   human_readable: Rome Statute Jurisdiction (Sovereigntist Reading)
 *   domain: international_law/treaty_interpretation/institutional_authority
 *
 * SUMMARY:
 *   This constraint represents the 'sovereigntist reading' of the Rome
 *   Statute, which emphasizes strict state consent as a prerequisite for ICC
 *   jurisdiction. Under this interpretation, the ICC's authority is primarily
 *   derived from the consent of states, limiting its reach over non-party
 *   nationals unless through a UN Security Council referral. National
 *   judiciaries retain primary authority, and complementarity is understood
 *   as deference to national systems rather than an override. This reading is
 *   one of several contested interpretations of the Rome Statute's
 *   jurisdictional framework.
 *
 * KEY AGENTS:
 *   - non_party_states: Primary beneficiary (institutional/arbitrage) — immune from ICC jurisdiction without consent.
 *   - national_judiciaries: Primary beneficiary (institutional/mobile) — retain primary authority.
 *   - icc_prosecutor: Agenda setter (organized/constrained) — operates within strict consent-based limits.
 *   - victims_of_atrocities: Payer (powerless/trapped) — limited avenues for international justice.
 *   - human_rights_advocates: Excluded (organized/constrained) — advocate for broader jurisdiction.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(rome_statute_jurisdiction__sovereigntist_reading, 0.3).
domain_priors:suppression_score(rome_statute_jurisdiction__sovereigntist_reading, 0.2).
domain_priors:theater_ratio(rome_statute_jurisdiction__sovereigntist_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(rome_statute_jurisdiction__sovereigntist_reading, extractiveness, 0.3).
narrative_ontology:constraint_metric(rome_statute_jurisdiction__sovereigntist_reading, suppression_requirement, 0.2).
narrative_ontology:constraint_metric(rome_statute_jurisdiction__sovereigntist_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(rome_statute_jurisdiction__sovereigntist_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(rome_statute_jurisdiction__sovereigntist_reading, resistance, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(rome_statute_jurisdiction__sovereigntist_reading, rope).
narrative_ontology:human_readable(rome_statute_jurisdiction__sovereigntist_reading, "Rome Statute Jurisdiction (Sovereigntist Reading)").
narrative_ontology:topic_domain(rome_statute_jurisdiction__sovereigntist_reading, "international_law/treaty_interpretation/institutional_authority").

domain_priors:requires_active_enforcement(rome_statute_jurisdiction__sovereigntist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(rome_statute_jurisdiction__sovereigntist_reading, '8b1aa8c5-8704-46d8-a69e-be03a7b5c414').
narrative_ontology:cs_kernel_codification('8b1aa8c5-8704-46d8-a69e-be03a7b5c414', fixed_text).
narrative_ontology:cs_authority_grounding('8b1aa8c5-8704-46d8-a69e-be03a7b5c414', lineage).
narrative_ontology:cs_interpretation_layer_present('8b1aa8c5-8704-46d8-a69e-be03a7b5c414').
narrative_ontology:cs_reading_relation('8b1aa8c5-8704-46d8-a69e-be03a7b5c414', rome_statute_jurisdiction__universalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('8b1aa8c5-8704-46d8-a69e-be03a7b5c414', rome_statute_jurisdiction__hybrid_complementarity_reading, coexists_with).
narrative_ontology:cs_axiom('8b1aa8c5-8704-46d8-a69e-be03a7b5c414', foundational, state_consent_is_paramount).
narrative_ontology:cs_axiom_status(state_consent_is_paramount, holdable).
narrative_ontology:cs_axiom_grounding('8b1aa8c5-8704-46d8-a69e-be03a7b5c414', state_consent_is_paramount, deontological).
narrative_ontology:cs_axiom('8b1aa8c5-8704-46d8-a69e-be03a7b5c414', foundational, national_judiciaries_retain_primacy).
narrative_ontology:cs_axiom_status(national_judiciaries_retain_primacy, holdable).
narrative_ontology:cs_axiom_grounding('8b1aa8c5-8704-46d8-a69e-be03a7b5c414', national_judiciaries_retain_primacy, conventional).
narrative_ontology:cs_reference_frame('8b1aa8c5-8704-46d8-a69e-be03a7b5c414', westphalian_sovereignty_framework).
narrative_ontology:cs_drift_state('8b1aa8c5-8704-46d8-a69e-be03a7b5c414', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('8b1aa8c5-8704-46d8-a69e-be03a7b5c414', '').
narrative_ontology:cs_kernel_id(rome_statute_jurisdiction__sovereigntist_reading, rome_statute_jurisdiction).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(rome_statute_jurisdiction__sovereigntist_reading, non_party_states).
narrative_ontology:constraint_beneficiary(rome_statute_jurisdiction__sovereigntist_reading, national_judiciaries).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(rome_statute_jurisdiction__sovereigntist_reading, victims_of_atrocities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These states benefit from the interpretation that their nationals are immune from ICC jurisdiction unless referred by the UNSC or they explicitly consent. This reading preserves their sovereign immunity and control over their citizens.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__sovereigntist_reading, non_party_states, beneficiary,
    institutional, generational, arbitrage, global).

% National courts retain primary authority over international crimes committed by their nationals or on their territory. This reading emphasizes complementarity as deference to national systems, not an override, reinforcing their jurisdictional primacy.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__sovereigntist_reading, national_judiciaries, beneficiary,
    institutional, generational, mobile, national).

% The ICC Prosecutor operates within the strictures of sovereign consent, seeking jurisdiction primarily over nationals of State Parties or crimes committed on their territory, or through UNSC referrals. This reading constrains the scope of their investigations.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__sovereigntist_reading, icc_prosecutor, agenda_setter,
    organized, biographical, constrained, global).

% Victims in non-party states or where national judiciaries fail to act may find their avenues for international justice severely limited by this interpretation, as it prioritizes state consent over universal accountability.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__sovereigntist_reading, victims_of_atrocities, payer,
    powerless, immediate, trapped, local).

% These groups advocate for broader ICC jurisdiction and universal accountability, often clashing with the strict consent-based framework. Their arguments for transcending sovereign consent are often marginalized by this reading.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__sovereigntist_reading, human_rights_advocates, excluded,
    organized, generational, constrained, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a framework for international criminal justice that respects state sovereignty, coordinating international legal action with national legal systems by prioritizing consent and national jurisdiction.
% TRANSFER_FUNCTION: Transfers the primary burden of prosecuting international crimes to national judiciaries, and limits the ICC's reach, effectively transferring immunity from international prosecution to non-consenting states and their nationals.
% ABSENT_VOICES: Advocates for universal jurisdiction and victims in non-party states are often marginalized by this reading, as their calls for justice beyond sovereign consent are deemed outside the Statute's intended scope.
% DISAPPEARANCE_RATIONALE: If this interpretation vanished, the ICC's operational scope would immediately expand, potentially leading to investigations in non-party states without UNSC referral, fundamentally altering the balance of international criminal justice and state sovereignty.
% FOUNDING_PROBLEM: The Rome Statute was established to create a permanent international criminal court to prosecute individuals for the most serious international crimes, while navigating the complex landscape of state sovereignty and international law.
% FOUNDING_PROBLEM_CORROBORATION: International legal scholars, state foreign ministries (especially those of non-party states), and some UN officials corroborate that balancing international justice with state sovereignty remains a live and contentious problem, with different interpretations of the Statute reflecting this ongoing tension.
narrative_ontology:disappearance_verdict(rome_statute_jurisdiction__sovereigntist_reading, world_rearranges).
narrative_ontology:founding_problem_status(rome_statute_jurisdiction__sovereigntist_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(rome_statute_jurisdiction__sovereigntist_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(rome_statute_jurisdiction__sovereigntist_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(rome_statute_jurisdiction__sovereigntist_reading_tests).
:- end_tests(rome_statute_jurisdiction__sovereigntist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.3) is moderate, reflecting the cost to victims and the constraint on the ICC's universal mandate, but not a pure extraction mechanism. Suppression (0.2) is low, as it primarily relies on legal interpretation and state practice rather than overt coercion. Theater ratio (0.1) is low, as the legal framework is genuinely applied, though its scope is contested. The values are relatively stable over time, reflecting the enduring nature of this interpretive stance.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of non-party states and national judiciaries, this reading is a legitimate 'rope' that coordinates international justice with sovereign rights. From the perspective of victims and human rights advocates, it functions more like a 'tangled rope' or 'snare' by limiting access to justice, though this reading does not explicitly create victims but rather limits the scope of protection.
 *
 * DIRECTIONALITY LOGIC:
 *   Non-party states and national judiciaries are beneficiaries (low d) as this reading protects their sovereign prerogatives. The ICC Prosecutor is an agenda-setter whose actions are constrained by this reading (moderate d). Victims of atrocities are payers (high d) as their access to justice is limited. Human rights advocates are excluded, as their universalist arguments are not fully accommodated by this framework.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading prevents mislabeling the Statute as a universalist snare by emphasizing its conditional, consent-based nature. It acknowledges the genuine coordination function of respecting sovereignty in international law, even if it comes at the cost of broader accountability. The 'founding_problem_status' being 'contested' reflects the ongoing debate about the Statute's true mandate, preventing a premature declaration of mandatrophy.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sovereignty_vs_universalism_balance,
    'What is the optimal balance between state sovereignty and universal international criminal justice, and does this reading achieve it?',
    'Ongoing international legal discourse, state practice, and judicial interpretations by the ICC and national courts. A shift in global norms or a landmark ruling could alter the perceived balance.',
    'If the balance shifts towards universalism, this reading''s extractiveness (from victims) would be re-evaluated as higher, and its classification might drift towards a ''tangled rope'' or ''snare'' from a universalist perspective. If sovereignty is further emphasized, it would reinforce this reading''s ''rope'' classification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sovereignty_vs_universalism_balance, conceptual, 'The fundamental tension between state consent and universal accountability in international law.').

omega_variable(
    complementarity_as_deference_or_override,
    'Is the complementarity principle primarily a mechanism of deference to national courts, or does it allow for the ICC to override national inaction or unwillingness?',
    'ICC case law development, particularly rulings on admissibility challenges where national proceedings are deemed insufficient. State responses to such rulings would also be critical.',
    'If complementarity is interpreted as a stronger override mechanism, this reading''s emphasis on national primacy would be weakened, potentially increasing the ICC''s effective jurisdiction and reducing the ''beneficiary'' status of national judiciaries.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(complementarity_as_deference_or_override, empirical, 'The practical application and interpretation of the complementarity principle.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(rome_statute_jurisdiction__sovereigntist_reading, 1998, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(rome_tr_t1998, rome_statute_jurisdiction__sovereigntist_reading, theater_ratio, 1998, 0.08).
narrative_ontology:measurement(rome_tr_t2006, rome_statute_jurisdiction__sovereigntist_reading, theater_ratio, 2006, 0.09).
narrative_ontology:measurement(rome_tr_t2014, rome_statute_jurisdiction__sovereigntist_reading, theater_ratio, 2014, 0.1).
narrative_ontology:measurement(rome_tr_t2024, rome_statute_jurisdiction__sovereigntist_reading, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(rome_be_t1998, rome_statute_jurisdiction__sovereigntist_reading, base_extractiveness, 1998, 0.25).
narrative_ontology:measurement(rome_be_t2006, rome_statute_jurisdiction__sovereigntist_reading, base_extractiveness, 2006, 0.28).
narrative_ontology:measurement(rome_be_t2014, rome_statute_jurisdiction__sovereigntist_reading, base_extractiveness, 2014, 0.3).
narrative_ontology:measurement(rome_be_t2024, rome_statute_jurisdiction__sovereigntist_reading, base_extractiveness, 2024, 0.3).

% Suppression requirement over time
narrative_ontology:measurement(rome_su_t1998, rome_statute_jurisdiction__sovereigntist_reading, suppression_requirement, 1998, 0.18).
narrative_ontology:measurement(rome_su_t2006, rome_statute_jurisdiction__sovereigntist_reading, suppression_requirement, 2006, 0.2).
narrative_ontology:measurement(rome_su_t2014, rome_statute_jurisdiction__sovereigntist_reading, suppression_requirement, 2014, 0.2).
narrative_ontology:measurement(rome_su_t2024, rome_statute_jurisdiction__sovereigntist_reading, suppression_requirement, 2024, 0.2).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
