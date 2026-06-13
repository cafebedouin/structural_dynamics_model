% ============================================================================
% CONSTRAINT STORY: nsl_legal_text__sovereignty_restoration_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_nsl_legal_text__sovereignty_restoration_reading, []).

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
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: nsl_legal_text__sovereignty_restoration_reading
 *   human_readable: National Security Law as Sovereign Restoration
 *   domain: constitutional_law/political_sociology/international_relations
 *
 * SUMMARY:
 *   This constraint story models the National Security Law (NSL) from the
 *   perspective of the 'sovereignty restoration' reading, which frames the
 *   law as a legitimate instrument to restore constitutional order and
 *   national security after the 2019 unrest. It acknowledges the law's
 *   suppressive effects on protesters and activists but interprets these as
 *   necessary measures for stability, rather than as a means of democratic
 *   enclosure or jurisdictional capture. The claimed type is 'tangled_rope'
 *   because, from this reading, there is a genuine coordination function
 *   (restoring order) alongside an acknowledged, though justified, extraction
 *   from those deemed threats.
 *
 * KEY AGENTS:
 *   - central_government_authorities: Agenda setter (institutional/arbitrage) — enforces NSL, frames as restoration.
 *   - local_pro_beijing_establishment: Beneficiary (powerful/mobile) — supports NSL for stability.
 *   - protesters_activists: Payer (powerless/trapped) — directly targeted by NSL, face severe penalties.
 *   - foreign_governments_critics: Payer (institutional/constrained) — criticized for interference, limited influence.
 *   - local_judiciary: Agenda setter/Payer (institutional/constrained) — interprets NSL, but autonomy is constrained.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(nsl_legal_text__sovereignty_restoration_reading, 0.45).
domain_priors:suppression_score(nsl_legal_text__sovereignty_restoration_reading, 0.7).
domain_priors:theater_ratio(nsl_legal_text__sovereignty_restoration_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(nsl_legal_text__sovereignty_restoration_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(nsl_legal_text__sovereignty_restoration_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(nsl_legal_text__sovereignty_restoration_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(nsl_legal_text__sovereignty_restoration_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(nsl_legal_text__sovereignty_restoration_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(nsl_legal_text__sovereignty_restoration_reading, tangled_rope).
narrative_ontology:human_readable(nsl_legal_text__sovereignty_restoration_reading, "National Security Law as Sovereign Restoration").
narrative_ontology:topic_domain(nsl_legal_text__sovereignty_restoration_reading, "constitutional_law/political_sociology/international_relations").

domain_priors:requires_active_enforcement(nsl_legal_text__sovereignty_restoration_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(nsl_legal_text__sovereignty_restoration_reading, '8ddace44-75c3-4a9f-8382-4386e96fcc02').
narrative_ontology:cs_kernel_codification('8ddace44-75c3-4a9f-8382-4386e96fcc02', fixed_text).
narrative_ontology:cs_authority_grounding('8ddace44-75c3-4a9f-8382-4386e96fcc02', lineage).
narrative_ontology:cs_interpretation_layer_present('8ddace44-75c3-4a9f-8382-4386e96fcc02').
narrative_ontology:cs_reading_relation('8ddace44-75c3-4a9f-8382-4386e96fcc02', nsl_legal_text__democratic_enclosure_reading, coexists_with).
narrative_ontology:cs_reading_relation('8ddace44-75c3-4a9f-8382-4386e96fcc02', nsl_legal_text__jurisdictional_capture_reading, coexists_with).
narrative_ontology:cs_axiom('8ddace44-75c3-4a9f-8382-4386e96fcc02', foundational, sovereign_security_paramount).
narrative_ontology:cs_axiom_status(sovereign_security_paramount, holdable).
narrative_ontology:cs_axiom_grounding('8ddace44-75c3-4a9f-8382-4386e96fcc02', sovereign_security_paramount, conventional).
narrative_ontology:cs_axiom('8ddace44-75c3-4a9f-8382-4386e96fcc02', foundational, constitutional_order_restoration_justifies_measures).
narrative_ontology:cs_axiom_status(constitutional_order_restoration_justifies_measures, holdable).
narrative_ontology:cs_axiom_grounding('8ddace44-75c3-4a9f-8382-4386e96fcc02', constitutional_order_restoration_justifies_measures, instrumental).
narrative_ontology:cs_reference_frame('8ddace44-75c3-4a9f-8382-4386e96fcc02', post_unrest_constitutional_stability).
narrative_ontology:cs_drift_state('8ddace44-75c3-4a9f-8382-4386e96fcc02', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('8ddace44-75c3-4a9f-8382-4386e96fcc02', '').
narrative_ontology:cs_kernel_id(nsl_legal_text__sovereignty_restoration_reading, nsl_legal_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(nsl_legal_text__sovereignty_restoration_reading, central_government_authorities).
narrative_ontology:constraint_beneficiary(nsl_legal_text__sovereignty_restoration_reading, local_pro_beijing_establishment).
narrative_ontology:constraint_victim(nsl_legal_text__sovereignty_restoration_reading, protesters_activists).
narrative_ontology:constraint_victim(nsl_legal_text__sovereignty_restoration_reading, foreign_governments_critics).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(nsl_legal_text__sovereignty_restoration_reading, local_judiciary).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Enacted and enforces the National Security Law, framing it as a necessary measure to restore stability and constitutional order after widespread unrest. Benefits from increased control and suppression of perceived threats to national unity.
narrative_ontology:constraint_stakeholder(nsl_legal_text__sovereignty_restoration_reading, central_government_authorities, agenda_setter,
    institutional, generational, arbitrage, national).

% Supports the NSL as a means to end political instability and return to economic focus. Benefits from the suppression of opposition and the reassertion of central authority, which aligns with their political and economic interests.
narrative_ontology:constraint_stakeholder(nsl_legal_text__sovereignty_restoration_reading, local_pro_beijing_establishment, beneficiary,
    powerful, biographical, mobile, regional).

% Are directly targeted by the NSL, facing arrest, prosecution, and severe penalties for actions previously considered legitimate protest. Their ability to organize and express dissent is severely curtailed, leading to self-censorship or exile.
narrative_ontology:constraint_stakeholder(nsl_legal_text__sovereignty_restoration_reading, protesters_activists, payer,
    powerless, immediate, trapped, local).

% Are criticized and sanctioned by the central government for perceived interference in internal affairs. While they can issue statements and impose sanctions, their ability to directly influence the NSL's application is limited, and their actions are framed as hostile by the central authorities.
narrative_ontology:constraint_stakeholder(nsl_legal_text__sovereignty_restoration_reading, foreign_governments_critics, payer,
    institutional, generational, constrained, global).

% Is tasked with interpreting and enforcing the NSL, which introduces new legal concepts and procedures that may conflict with established common law principles. While they administer the law, their autonomy and traditional role are constrained by the new legal framework.
narrative_ontology:constraint_stakeholder(nsl_legal_text__sovereignty_restoration_reading, local_judiciary, agenda_setter,
    institutional, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(nsl_legal_text__sovereignty_restoration_reading, local_judiciary, payer).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Re-establishes a clear hierarchy of legal authority and suppresses perceived threats to national security, aiming to coordinate political action towards stability and national unity after a period of widespread unrest.
% TRANSFER_FUNCTION: Transfers legal and political authority from local autonomy to central government control, and transfers the burden of maintaining 'security' onto individuals labeled as 'threats' through criminalization and suppression.
% ABSENT_VOICES: International human rights organizations and independent legal scholars, who would argue that the NSL violates international human rights standards and undermines the rule of law, are largely excluded from the official discourse and their criticisms are dismissed as external interference.
% DISAPPEARANCE_RATIONALE: If the NSL vanished overnight, the central government's asserted authority over local affairs would be immediately challenged, political opposition would likely re-emerge, and the legal landscape would revert to a more autonomous common law system, leading to significant political and legal reorganization.
% FOUNDING_PROBLEM: The central government perceived a severe threat to national security and constitutional order following large-scale protests and perceived foreign interference in 2019, leading to significant political instability and challenges to governance.
% FOUNDING_PROBLEM_CORROBORATION: Central government officials and state media consistently attest that the founding problem of national security threats and foreign interference remains live. This is corroborated by local pro-Beijing figures and some segments of the business community who prioritize stability. However, international bodies and opposition groups contest this, arguing the 'problem' is a pretext for political control.
narrative_ontology:disappearance_verdict(nsl_legal_text__sovereignty_restoration_reading, world_rearranges).
narrative_ontology:founding_problem_status(nsl_legal_text__sovereignty_restoration_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(nsl_legal_text__sovereignty_restoration_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(nsl_legal_text__sovereignty_restoration_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(nsl_legal_text__sovereignty_restoration_reading_tests).
:- end_tests(nsl_legal_text__sovereignty_restoration_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.45) is moderate from this reading, as it primarily targets political opposition rather than the general population, and is justified as a 'cost of stability.' Suppression (0.70) is high due to active enforcement against dissent, but again, framed as necessary. Theater ratio (0.20) is low, as the law is actively and genuinely enforced to achieve its stated security goals, with less performative maintenance. Accessibility collapse (0.60) is significant for political alternatives, and resistance (0.50) is present from targeted groups and international actors.
 *
 * PERSPECTIVAL GAP:
 *   The central government and local pro-Beijing establishment experience this as a necessary, if firm, coordination mechanism. Protesters and foreign critics experience it as pure extraction and suppression. The local judiciary, while administering the law, experiences a constraint on its traditional autonomy. The engine's per-seat classification will reflect these divergences.
 *
 * DIRECTIONALITY LOGIC:
 *   Central government authorities and the local pro-Beijing establishment are beneficiaries (low d) as they gain stability and control. Protesters/activists and foreign governments/critics are targets (high d) as they bear the direct costs of suppression and legal action. The local judiciary is a complex case, acting as an agenda-setter but also experiencing constraints on its independence, leading to a more symmetric d.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading prevents mislabeling the NSL as pure extraction by emphasizing its stated coordination function of restoring order. However, the 'contested' status of the founding problem (Q5) and the presence of victims indicate that the coordination story is not universally accepted, and the constraint's persistence is not solely due to its original mandate. The engine's classification will account for the tension between the claimed coordination and the observed extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    legitimacy_of_sovereign_action,
    'Is the central government''s claim of ''sovereign security instrument'' a legitimate exercise of authority or a pretext for political control?',
    'Independent international legal review of the NSL''s compatibility with international human rights law and the Basic Law, and empirical analysis of its application beyond genuine security threats.',
    'If found to be a pretext, the constraint''s extractiveness and suppression would be re-evaluated as higher, and its coordination function as largely theatrical, shifting its classification towards a Snare. If legitimate, the current classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legitimacy_of_sovereign_action, conceptual, 'Ambiguity of the NSL''s underlying legitimacy claim.').

omega_variable(
    scope_of_security_threat,
    'Is the scope of ''national security threat'' as defined by the NSL genuinely limited to severe threats to state integrity, or does it encompass legitimate political dissent?',
    'Case-by-case analysis of prosecutions under the NSL, distinguishing between acts of violence/secession and peaceful expression/assembly.',
    'If the definition is found to be overly broad, the victim set expands, and the effective extractiveness for a wider population increases, pushing the classification closer to a Snare. If narrowly applied, the current classification is more robust.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(scope_of_security_threat, empirical, 'Ambiguity in the definition and application of ''national security threat''.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(nsl_legal_text__sovereignty_restoration_reading, 2020, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(nsl__tr_t2020, nsl_legal_text__sovereignty_restoration_reading, theater_ratio, 2020, 0.15).
narrative_ontology:measurement(nsl__tr_t2021, nsl_legal_text__sovereignty_restoration_reading, theater_ratio, 2021, 0.18).
narrative_ontology:measurement(nsl__tr_t2022, nsl_legal_text__sovereignty_restoration_reading, theater_ratio, 2022, 0.2).
narrative_ontology:measurement(nsl__tr_t2023, nsl_legal_text__sovereignty_restoration_reading, theater_ratio, 2023, 0.2).
narrative_ontology:measurement(nsl__tr_t2024, nsl_legal_text__sovereignty_restoration_reading, theater_ratio, 2024, 0.2).

% Extraction over time
narrative_ontology:measurement(nsl__be_t2020, nsl_legal_text__sovereignty_restoration_reading, base_extractiveness, 2020, 0.4).
narrative_ontology:measurement(nsl__be_t2021, nsl_legal_text__sovereignty_restoration_reading, base_extractiveness, 2021, 0.42).
narrative_ontology:measurement(nsl__be_t2022, nsl_legal_text__sovereignty_restoration_reading, base_extractiveness, 2022, 0.44).
narrative_ontology:measurement(nsl__be_t2023, nsl_legal_text__sovereignty_restoration_reading, base_extractiveness, 2023, 0.45).
narrative_ontology:measurement(nsl__be_t2024, nsl_legal_text__sovereignty_restoration_reading, base_extractiveness, 2024, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(nsl__su_t2020, nsl_legal_text__sovereignty_restoration_reading, suppression_requirement, 2020, 0.65).
narrative_ontology:measurement(nsl__su_t2021, nsl_legal_text__sovereignty_restoration_reading, suppression_requirement, 2021, 0.68).
narrative_ontology:measurement(nsl__su_t2022, nsl_legal_text__sovereignty_restoration_reading, suppression_requirement, 2022, 0.7).
narrative_ontology:measurement(nsl__su_t2023, nsl_legal_text__sovereignty_restoration_reading, suppression_requirement, 2023, 0.7).
narrative_ontology:measurement(nsl__su_t2024, nsl_legal_text__sovereignty_restoration_reading, suppression_requirement, 2024, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(nsl_legal_text__sovereignty_restoration_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(nsl_legal_text__sovereignty_restoration_reading, nsl_legal_text__democratic_enclosure_reading).
narrative_ontology:affects_constraint(nsl_legal_text__sovereignty_restoration_reading, nsl_legal_text__jurisdictional_capture_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'nsl_legal_text' kernel. Its structural properties and classification differ from the 'democratic_enclosure_reading' and 'jurisdictional_capture_reading' due to differing interpretations of the law's intent and impact.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
