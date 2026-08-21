% ============================================================================
% CONSTRAINT STORY: jcpoa_treaty_bindingness__graduated_compliance_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jcpoa_treaty_bindingness__graduated_compliance_reading, []).

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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: jcpoa_treaty_bindingness__graduated_compliance_reading
 *   human_readable: JCPOA: Graduated Compliance Reading
 *   domain: international_law/nuclear_non_proliferation/treaty_compliance
 *
 * SUMMARY:
 *   This constraint story models the JCPOA through the lens of a 'graduated
 *   compliance' reading. In this interpretation, the agreement functions as a
 *   scaled reciprocal commitment where enforcement actions (e.g., sanctions
 *   relief withdrawal) are proportional to the severity of Iranian
 *   non-compliance. Dispute resolution mechanisms prioritize de-escalation
 *   and maintaining the framework over formal legal closure. This reading
 *   emphasizes pragmatic diplomacy and partial economic engagement,
 *   benefiting advocates of such approaches and economic actors.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jcpoa_treaty_bindingness__graduated_compliance_reading, 0.45).
domain_priors:suppression_score(jcpoa_treaty_bindingness__graduated_compliance_reading, 0.6).
domain_priors:theater_ratio(jcpoa_treaty_bindingness__graduated_compliance_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jcpoa_treaty_bindingness__graduated_compliance_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(jcpoa_treaty_bindingness__graduated_compliance_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(jcpoa_treaty_bindingness__graduated_compliance_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jcpoa_treaty_bindingness__graduated_compliance_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(jcpoa_treaty_bindingness__graduated_compliance_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jcpoa_treaty_bindingness__graduated_compliance_reading, tangled_rope).
narrative_ontology:human_readable(jcpoa_treaty_bindingness__graduated_compliance_reading, "JCPOA: Graduated Compliance Reading").
narrative_ontology:topic_domain(jcpoa_treaty_bindingness__graduated_compliance_reading, "international_law/nuclear_non_proliferation/treaty_compliance").

domain_priors:requires_active_enforcement(jcpoa_treaty_bindingness__graduated_compliance_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jcpoa_treaty_bindingness__graduated_compliance_reading, 'c1fdf5c1-79bb-4979-8a7e-1d8def1b3d93').
narrative_ontology:cs_kernel_codification('c1fdf5c1-79bb-4979-8a7e-1d8def1b3d93', formalized).
narrative_ontology:cs_authority_grounding('c1fdf5c1-79bb-4979-8a7e-1d8def1b3d93', lineage).
narrative_ontology:cs_interpretation_layer_present('c1fdf5c1-79bb-4979-8a7e-1d8def1b3d93').
narrative_ontology:cs_reading_relation('c1fdf5c1-79bb-4979-8a7e-1d8def1b3d93', jcpoa_treaty_bindingness__binding_multilateral_reading, coexists_with).
narrative_ontology:cs_reading_relation('c1fdf5c1-79bb-4979-8a7e-1d8def1b3d93', jcpoa_treaty_bindingness__transactional_provisional_reading, coexists_with).
narrative_ontology:cs_axiom('c1fdf5c1-79bb-4979-8a7e-1d8def1b3d93', foundational, proportional_response_is_effective_deterrence).
narrative_ontology:cs_axiom_status(proportional_response_is_effective_deterrence, holdable).
narrative_ontology:cs_axiom_grounding('c1fdf5c1-79bb-4979-8a7e-1d8def1b3d93', proportional_response_is_effective_deterrence, instrumental).
narrative_ontology:cs_axiom('c1fdf5c1-79bb-4979-8a7e-1d8def1b3d93', foundational, diplomacy_over_coercion_for_long_term_stability).
narrative_ontology:cs_axiom_status(diplomacy_over_coercion_for_long_term_stability, holdable).
narrative_ontology:cs_axiom_grounding('c1fdf5c1-79bb-4979-8a7e-1d8def1b3d93', diplomacy_over_coercion_for_long_term_stability, deontological).
narrative_ontology:cs_reference_frame('c1fdf5c1-79bb-4979-8a7e-1d8def1b3d93', calibrated_de_escalation_framework).
narrative_ontology:cs_drift_state('c1fdf5c1-79bb-4979-8a7e-1d8def1b3d93', contemporary_geopolitical_environment, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('c1fdf5c1-79bb-4979-8a7e-1d8def1b3d93', '2024-07-30T12:00:00Z').
narrative_ontology:cs_kernel_id(jcpoa_treaty_bindingness__graduated_compliance_reading, jcpoa_treaty_bindingness).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jcpoa_treaty_bindingness__graduated_compliance_reading, iran).
narrative_ontology:constraint_beneficiary(jcpoa_treaty_bindingness__graduated_compliance_reading, eu3_plus_three).
narrative_ontology:constraint_beneficiary(jcpoa_treaty_bindingness__graduated_compliance_reading, pragmatic_diplomacy_advocates).
narrative_ontology:constraint_beneficiary(jcpoa_treaty_bindingness__graduated_compliance_reading, economic_actors_seeking_partial_engagement).
narrative_ontology:constraint_victim(jcpoa_treaty_bindingness__graduated_compliance_reading, iranian_hardliners).
narrative_ontology:constraint_victim(jcpoa_treaty_bindingness__graduated_compliance_reading, us_sanctions_hawks).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(jcpoa_treaty_bindingness__graduated_compliance_reading, iran).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Commits to nuclear program limitations in exchange for sanctions relief. Bears the cost of inspections and limitations, but benefits from economic engagement. Seeks proportional responses to its actions.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__graduated_compliance_reading, iran, payer,
    powerful, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(jcpoa_treaty_bindingness__graduated_compliance_reading, iran, beneficiary).

% Negotiated the agreement and seeks to uphold it through calibrated responses. Benefits from non-proliferation and regional stability. Prioritizes de-escalation and diplomatic solutions.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__graduated_compliance_reading, eu3_plus_three, agenda_setter,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(jcpoa_treaty_bindingness__graduated_compliance_reading, eu3_plus_three, beneficiary).

% Support the JCPOA as a model for managing complex international security challenges through negotiation and calibrated responses. Benefit from the perceived success of diplomatic engagement.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__graduated_compliance_reading, pragmatic_diplomacy_advocates, beneficiary,
    organized, biographical, mobile, global).

% Seek to re-enter or expand business operations in Iran, benefiting from any sanctions relief. Their engagement is contingent on the stability of the agreement and the predictability of enforcement.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__graduated_compliance_reading, economic_actors_seeking_partial_engagement, beneficiary,
    organized, immediate, arbitrage, global).

% Oppose the JCPOA, viewing it as an infringement on national sovereignty and a concession to Western powers. Bear the political cost of compliance and seek to undermine the agreement through escalatory actions.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__graduated_compliance_reading, iranian_hardliners, payer,
    powerful, generational, identity_locked, national).

% Advocate for maximum pressure on Iran through comprehensive sanctions, viewing the JCPOA as too lenient. Bear the political cost of diplomatic engagement and seek to dismantle the agreement.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__graduated_compliance_reading, us_sanctions_hawks, payer,
    institutional, generational, identity_locked, national).

% Monitors Iran's nuclear program to verify compliance with the JCPOA. Provides technical assessments that inform the graduated enforcement responses of the other parties.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__graduated_compliance_reading, iaea, observer,
    institutional, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates international efforts to prevent nuclear proliferation by providing a framework for Iran's nuclear program limitations and reciprocal sanctions relief, managed through a graduated response mechanism.
% TRANSFER_FUNCTION: Transfers nuclear material and technology limitations from Iran to the international community, in exchange for economic sanctions relief and diplomatic engagement from the P5+1. Enforcement is scaled: partial sanctions relief withdrawal proportional to Iranian enrichment increases.
% ABSENT_VOICES: States not party to the JCPOA, particularly regional rivals of Iran, who would argue for more stringent controls or a complete halt to Iran's nuclear program, are largely excluded from the direct compliance assessment mechanism.
% DISAPPEARANCE_RATIONALE: If the JCPOA vanished overnight, Iran would likely accelerate its nuclear program, leading to a rapid escalation of regional tensions, renewed comprehensive sanctions, and a potential military confrontation. The international non-proliferation regime would be severely undermined.
% FOUNDING_PROBLEM: The uncontrolled expansion of Iran's nuclear program, raising fears of nuclear weapon proliferation and regional instability.
% FOUNDING_PROBLEM_CORROBORATION: The IAEA consistently reports on Iran's nuclear activities, and the EU3+3 continue to engage diplomatically, indicating the problem remains live. Independent non-proliferation experts and think tanks corroborate the ongoing risk, even with the JCPOA in place.
narrative_ontology:disappearance_verdict(jcpoa_treaty_bindingness__graduated_compliance_reading, world_rearranges).
narrative_ontology:founding_problem_status(jcpoa_treaty_bindingness__graduated_compliance_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jcpoa_treaty_bindingness__graduated_compliance_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(jcpoa_treaty_bindingness__graduated_compliance_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jcpoa_treaty_bindingness__graduated_compliance_reading, 0.45, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jcpoa_treaty_bindingness__graduated_compliance_reading_tests).
:- end_tests(jcpoa_treaty_bindingness__graduated_compliance_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.45) is moderate, reflecting the reciprocal nature of the agreement where both sides make concessions and receive benefits. Suppression (0.60) is present due to the need for active enforcement of nuclear limitations and sanctions, but it is graduated rather than absolute. The theater ratio (0.20) is low, as the core functions of monitoring and response are genuine. The claimed type is 'tangled_rope' because it involves genuine coordination (non-proliferation) but also asymmetric extraction (Iran's nuclear limitations vs. sanctions relief, and the costs borne by hardliners on both sides).
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of pragmatic diplomacy advocates, the JCPOA is a successful 'rope' for managing a complex security challenge. However, from the perspective of Iranian hardliners and US sanctions hawks, it is a 'snare' that either compromises sovereignty or is too weak to be effective. The engine's per-seat classification will capture these divergences based on the declared power, exit options, and beneficiary/victim roles.
 *
 * DIRECTIONALITY LOGIC:
 *   Iran and the EU3+3 are both beneficiaries and payers, reflecting the reciprocal nature. Pragmatic diplomacy advocates and economic actors are clear beneficiaries. Iranian hardliners and US sanctions hawks are victims, as the agreement constrains their preferred policies. The graduated enforcement mechanism aims to keep all parties within the framework, even if they bear costs.
 *
 * MANDATROPHY ANALYSIS:
 *   The graduated compliance reading inherently resists mandatrophy by tying enforcement directly to compliance levels. If Iran's compliance improves, enforcement softens; if it degrades, enforcement tightens. This dynamic linkage prevents the mandate from becoming detached from its function, unlike a 'piton' where function atrophies but the constraint persists by inertia. The 'live' status of the founding problem further indicates active relevance.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    proportionality_assessment_ambiguity,
    'What constitutes ''proportional'' compliance or non-compliance, and who adjudicates this proportionality in practice?',
    'Analysis of past dispute resolution mechanisms and their outcomes, particularly how ''minor'' vs. ''significant'' violations were treated and the consensus (or lack thereof) among parties.',
    'If proportionality is consistently interpreted unilaterally or arbitrarily, the constraint''s effective suppression and extractiveness would be higher, potentially shifting it towards a ''snare'' for the party consistently disadvantaged by the interpretation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(proportionality_assessment_ambiguity, empirical, 'Ambiguity in the practical application of graduated enforcement.').

omega_variable(
    graduated_vs_binding_treaty_nature,
    'Is the JCPOA fundamentally a binding multilateral treaty or a flexible, scaled commitment?',
    'Legal analysis of the treaty''s text, the Vienna Convention on the Law of Treaties, and state practice regarding withdrawal and modification. This would involve comparing the ''graduated compliance'' reading with the ''binding_multilateral_reading''.',
    'If the ''binding_multilateral_reading'' is found to be the dominant legal interpretation, this constraint''s claimed type might shift towards a ''rope'' or ''tangled_rope'' with higher suppression for any unilateral deviation, as the legal costs of exit would be higher.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(graduated_vs_binding_treaty_nature, conceptual, 'Contestation over the fundamental legal nature of the JCPOA.').

omega_variable(
    kernel_reading_impact_on_economic_actors,
    'How do different readings of the JCPOA''s bindingness (e.g., graduated vs. transactional) impact the willingness of economic actors to engage with Iran?',
    'Empirical study of foreign direct investment and trade flows into Iran under different phases of the JCPOA, correlated with the prevailing political discourse and dominant reading of the agreement''s stability.',
    'If the ''transactional_provisional_reading'' gains dominance, economic actors would perceive higher risk, leading to reduced engagement and effectively increasing the ''extractiveness'' for Iran by limiting its benefits. Conversely, a ''binding_multilateral_reading'' might increase perceived stability and reduce extractiveness.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_impact_on_economic_actors, empirical, 'Impact of kernel readings on economic engagement and perceived stability.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jcpoa_treaty_bindingness__graduated_compliance_reading, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jcpo_tr_t0, jcpoa_treaty_bindingness__graduated_compliance_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(jcpo_tr_t2, jcpoa_treaty_bindingness__graduated_compliance_reading, theater_ratio, 2, 0.22).
narrative_ontology:measurement(jcpo_tr_t4, jcpoa_treaty_bindingness__graduated_compliance_reading, theater_ratio, 4, 0.2).
narrative_ontology:measurement(jcpo_tr_t6, jcpoa_treaty_bindingness__graduated_compliance_reading, theater_ratio, 6, 0.18).
narrative_ontology:measurement(jcpo_tr_t8, jcpoa_treaty_bindingness__graduated_compliance_reading, theater_ratio, 8, 0.2).
narrative_ontology:measurement(jcpo_tr_t10, jcpoa_treaty_bindingness__graduated_compliance_reading, theater_ratio, 10, 0.2).

% Extraction over time
narrative_ontology:measurement(jcpo_be_t0, jcpoa_treaty_bindingness__graduated_compliance_reading, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(jcpo_be_t2, jcpoa_treaty_bindingness__graduated_compliance_reading, base_extractiveness, 2, 0.48).
narrative_ontology:measurement(jcpo_be_t4, jcpoa_treaty_bindingness__graduated_compliance_reading, base_extractiveness, 4, 0.45).
narrative_ontology:measurement(jcpo_be_t6, jcpoa_treaty_bindingness__graduated_compliance_reading, base_extractiveness, 6, 0.42).
narrative_ontology:measurement(jcpo_be_t8, jcpoa_treaty_bindingness__graduated_compliance_reading, base_extractiveness, 8, 0.45).
narrative_ontology:measurement(jcpo_be_t10, jcpoa_treaty_bindingness__graduated_compliance_reading, base_extractiveness, 10, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(jcpo_su_t0, jcpoa_treaty_bindingness__graduated_compliance_reading, suppression_requirement, 0, 0.7).
narrative_ontology:measurement(jcpo_su_t2, jcpoa_treaty_bindingness__graduated_compliance_reading, suppression_requirement, 2, 0.65).
narrative_ontology:measurement(jcpo_su_t4, jcpoa_treaty_bindingness__graduated_compliance_reading, suppression_requirement, 4, 0.6).
narrative_ontology:measurement(jcpo_su_t6, jcpoa_treaty_bindingness__graduated_compliance_reading, suppression_requirement, 6, 0.58).
narrative_ontology:measurement(jcpo_su_t8, jcpoa_treaty_bindingness__graduated_compliance_reading, suppression_requirement, 8, 0.6).
narrative_ontology:measurement(jcpo_su_t10, jcpoa_treaty_bindingness__graduated_compliance_reading, suppression_requirement, 10, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jcpoa_treaty_bindingness__graduated_compliance_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(jcpoa_treaty_bindingness__graduated_compliance_reading, iran_nuclear_program_limitations).
narrative_ontology:affects_constraint(jcpoa_treaty_bindingness__graduated_compliance_reading, iran_sanctions_regime).
narrative_ontology:affects_constraint(jcpoa_treaty_bindingness__graduated_compliance_reading, regional_security_dynamics).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'jcpoa_treaty_bindingness' kernel. The 'binding_multilateral_reading' emphasizes the treaty's legal finality, while the 'transactional_provisional_reading' views it as a temporary, unilaterally voidable arrangement. This 'graduated_compliance_reading' focuses on reciprocal, scaled enforcement.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
