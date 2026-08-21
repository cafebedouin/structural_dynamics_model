% ============================================================================
% CONSTRAINT STORY: one_country_two_systems_framework__autonomy_primacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_one_country_two_systems_framework__autonomy_primacy_reading, []).

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
 *   constraint_id: one_country_two_systems_framework__autonomy_primacy_reading
 *   human_readable: Hong Kong Autonomy under One Country, Two Systems (Autonomy Primacy Reading)
 *   domain: constitutional_law/political_systems/state_sovereignty
 *
 * SUMMARY:
 *   This constraint represents the 'autonomy primacy' reading of the 'One
 *   Country, Two Systems' framework for Hong Kong. Under this reading, Hong
 *   Kong retains substantive autonomy, civil liberties, and judicial
 *   independence, guaranteed by treaty and enforceable internationally.
 *   Mainland interference is considered a violation of the framework. This
 *   reading emphasizes the 'Two Systems' aspect, viewing Hong Kong's
 *   distinctiveness as paramount. The metrics reflect a system that, while
 *   facing some pressure, largely functions to preserve autonomy, with
 *   low-to-moderate extraction and suppression, primarily from the PRC's side
 *   in terms of constrained direct authority.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(one_country_two_systems_framework__autonomy_primacy_reading, 0.25).
domain_priors:suppression_score(one_country_two_systems_framework__autonomy_primacy_reading, 0.3).
domain_priors:theater_ratio(one_country_two_systems_framework__autonomy_primacy_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(one_country_two_systems_framework__autonomy_primacy_reading, extractiveness, 0.25).
narrative_ontology:constraint_metric(one_country_two_systems_framework__autonomy_primacy_reading, suppression_requirement, 0.3).
narrative_ontology:constraint_metric(one_country_two_systems_framework__autonomy_primacy_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(one_country_two_systems_framework__autonomy_primacy_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(one_country_two_systems_framework__autonomy_primacy_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(one_country_two_systems_framework__autonomy_primacy_reading, rope).
narrative_ontology:human_readable(one_country_two_systems_framework__autonomy_primacy_reading, "Hong Kong Autonomy under One Country, Two Systems (Autonomy Primacy Reading)").
narrative_ontology:topic_domain(one_country_two_systems_framework__autonomy_primacy_reading, "constitutional_law/political_systems/state_sovereignty").

domain_priors:requires_active_enforcement(one_country_two_systems_framework__autonomy_primacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(one_country_two_systems_framework__autonomy_primacy_reading, '57f229d2-618b-4615-af64-f78a95014b01').
narrative_ontology:cs_kernel_codification('57f229d2-618b-4615-af64-f78a95014b01', fixed_text).
narrative_ontology:cs_authority_grounding('57f229d2-618b-4615-af64-f78a95014b01', lineage).
narrative_ontology:cs_interpretation_layer_present('57f229d2-618b-4615-af64-f78a95014b01').
narrative_ontology:cs_reading_relation('57f229d2-618b-4615-af64-f78a95014b01', one_country_two_systems_framework__sovereignty_primacy_reading, forecloses).
narrative_ontology:cs_reading_relation('57f229d2-618b-4615-af64-f78a95014b01', one_country_two_systems_framework__balanced_coexistence_reading, coexists_with).
narrative_ontology:cs_axiom('57f229d2-618b-4615-af64-f78a95014b01', foundational, high_degree_of_autonomy_guaranteed).
narrative_ontology:cs_axiom_status(high_degree_of_autonomy_guaranteed, holdable).
narrative_ontology:cs_axiom_grounding('57f229d2-618b-4615-af64-f78a95014b01', high_degree_of_autonomy_guaranteed, deontological).
narrative_ontology:cs_axiom('57f229d2-618b-4615-af64-f78a95014b01', foundational, judicial_independence_absolute).
narrative_ontology:cs_axiom_status(judicial_independence_absolute, holdable).
narrative_ontology:cs_axiom_grounding('57f229d2-618b-4615-af64-f78a95014b01', judicial_independence_absolute, deontological).
narrative_ontology:cs_reference_frame('57f229d2-618b-4615-af64-f78a95014b01', sino_british_joint_declaration_spirit).
narrative_ontology:cs_drift_state('57f229d2-618b-4615-af64-f78a95014b01', contemporary_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('57f229d2-618b-4615-af64-f78a95014b01', '').
narrative_ontology:cs_kernel_id(one_country_two_systems_framework__autonomy_primacy_reading, one_country_two_systems_framework).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(one_country_two_systems_framework__autonomy_primacy_reading, hong_kong_residents).
narrative_ontology:constraint_beneficiary(one_country_two_systems_framework__autonomy_primacy_reading, international_investors).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(one_country_two_systems_framework__autonomy_primacy_reading, prc_central_government).
narrative_ontology:constraint_vindicates(one_country_two_systems_framework__autonomy_primacy_reading, rule_of_law_doctrine).
narrative_ontology:constraint_vindicates(one_country_two_systems_framework__autonomy_primacy_reading, international_treaty_supremacy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefit from guaranteed civil liberties, judicial independence, and a distinct legal system that protects their rights and way of life, distinct from mainland China. Their exit options are constrained by national identity and practical relocation difficulties.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__autonomy_primacy_reading, hong_kong_residents, beneficiary,
    organized, generational, constrained, local).

% Benefit from Hong Kong's independent judiciary, common law system, and free market principles, which provide a stable and predictable environment for business. They have relatively mobile capital and can relocate if autonomy erodes.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__autonomy_primacy_reading, international_investors, beneficiary,
    powerful, biographical, mobile, global).

% Upholds the Basic Law and common law principles, acting as a check on executive power and mainland interference. Their institutional identity is deeply tied to judicial independence and the rule of law.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__autonomy_primacy_reading, hong_kong_judiciary, agenda_setter,
    institutional, generational, identity_locked, local).

% Administers Hong Kong's affairs with a high degree of autonomy, balancing local interests with the 'One Country' principle. Its actions are subject to judicial review and public scrutiny, but it also faces pressure from Beijing.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__autonomy_primacy_reading, hong_kong_government, agenda_setter,
    institutional, biographical, constrained, local).

% Agreed to the framework, which limits its direct intervention in Hong Kong's internal affairs, thereby foregoing some aspects of immediate sovereign control. It benefits from Hong Kong's economic role but bears the cost of constrained direct authority.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__autonomy_primacy_reading, prc_central_government, payer,
    institutional, civilizational, arbitrage, national).

% Monitors the implementation of the framework, particularly regarding civil liberties and the rule of law, due to treaty obligations and economic interests. It can exert diplomatic pressure but has limited direct enforcement mechanisms.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__autonomy_primacy_reading, international_community, observer,
    institutional, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the integration of Hong Kong into China while preserving its distinct capitalist system, common law, and civil liberties, preventing a disruptive merger and facilitating economic stability.
% TRANSFER_FUNCTION: Transfers sovereign authority over Hong Kong to the PRC, in exchange for guaranteed autonomy and a distinct legal/political system for Hong Kong residents and businesses.
% ABSENT_VOICES: Hardline PRC nationalists who advocate for full integration and direct rule, and radical Hong Kong independence advocates who reject the 'One Country' principle entirely. Both are excluded from the framework's operational discourse.
% DISAPPEARANCE_RATIONALE: If the framework vanished, Hong Kong would likely be fully integrated into mainland China's system, leading to a collapse of its independent judiciary, civil liberties, and distinct economic model. This would trigger massive capital flight, emigration, and international condemnation, fundamentally altering the region's political and economic landscape.
% FOUNDING_PROBLEM: To facilitate the peaceful transfer of sovereignty over Hong Kong from the UK to the PRC, ensuring stability and prosperity by preserving Hong Kong's unique system for 50 years.
% FOUNDING_PROBLEM_CORROBORATION: The PRC government attests the problem is live, emphasizing national security and territorial integrity. Hong Kong residents, international legal scholars, and former British officials attest that the original problem of peaceful transition was solved, but the framework's interpretation has become a new, contested problem, with the autonomy component under severe pressure.
narrative_ontology:disappearance_verdict(one_country_two_systems_framework__autonomy_primacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(one_country_two_systems_framework__autonomy_primacy_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(one_country_two_systems_framework__autonomy_primacy_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(one_country_two_systems_framework__autonomy_primacy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(one_country_two_systems_framework__autonomy_primacy_reading, 0.25, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(one_country_two_systems_framework__autonomy_primacy_reading_tests).
:- end_tests(one_country_two_systems_framework__autonomy_primacy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.25) because this reading posits that the framework primarily functions to coordinate distinct systems, with minimal direct extraction from Hong Kong residents. Suppression is moderate (0.3) as active enforcement is required to maintain the boundary against potential mainland overreach, but it is not yet overwhelming. Theater ratio is low (0.1) as the institutions of autonomy are genuinely functional, not merely performative. The temporal measurements show a slight, gradual increase in extractiveness and suppression, reflecting growing pressures on autonomy over time, even within this reading.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of Hong Kong residents and international investors, this reading of the framework is a genuine Rope, providing essential coordination and protection. From the PRC Central Government's perspective, it is a self-imposed constraint that limits its sovereign power, making it a payer. The engine's per-seat classification would reflect this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Hong Kong residents and international investors are beneficiaries, as the framework protects their rights and economic interests (low d). The Hong Kong judiciary and government are agenda-setters, actively maintaining the system's integrity. The PRC Central Government is positioned as a payer, bearing the cost of constrained direct sovereign authority over Hong Kong (high d, as it foregoes full control). The international community observes and exerts pressure, but is not directly subject to the constraint's extraction.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    treaty_enforceability_ambiguity,
    'To what extent are the treaty guarantees of Hong Kong''s autonomy and civil liberties actually enforceable against the PRC on the international stage?',
    'Analysis of international legal precedents, state practice, and the outcomes of diplomatic interventions or legal challenges related to treaty violations.',
    'If enforceability is low, the constraint''s effective suppression is higher than measured, as the ''guarantees'' are merely aspirational. If high, the autonomy primacy reading is more robust.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(treaty_enforceability_ambiguity, empirical, 'The actual international enforceability of the Sino-British Joint Declaration.').

omega_variable(
    democratic_reform_pathway_viability,
    'Is a genuine pathway for democratic reform in Hong Kong still live under this reading, or has it been foreclosed by mainland interpretations and actions?',
    'Observation of legislative processes, electoral reforms, and the scope of political participation permitted by the Hong Kong and PRC governments over time.',
    'If the pathway is foreclosed, the ''autonomy primacy'' reading''s claim of robust civil liberties and self-determination is weakened, shifting its classification towards a more extractive type for residents.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(democratic_reform_pathway_viability, empirical, 'The practical existence of a democratic reform pathway for Hong Kong.').

omega_variable(
    autonomy_vs_sovereignty_framing,
    'Is the ''autonomy primacy'' reading a defensible interpretation of the Basic Law and Joint Declaration, or does it overstate Hong Kong''s rights relative to PRC sovereignty?',
    'Comparative legal analysis of constitutional documents, international law, and historical context, alongside a conceptual analysis of sovereignty and autonomy in composite states.',
    'If the reading is conceptually flawed, its classification as a Rope is unstable, and it may be reclassified as a Snare or Tangled Rope from the outset, reflecting an inherent extractive structure masked by an over-optimistic interpretation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(autonomy_vs_sovereignty_framing, conceptual, 'Conceptual validity of prioritizing autonomy over sovereignty within the framework.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(one_country_two_systems_framework__autonomy_primacy_reading, 1997, 2047).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(one__tr_t1997, one_country_two_systems_framework__autonomy_primacy_reading, theater_ratio, 1997, 0.05).
narrative_ontology:measurement(one__tr_t2007, one_country_two_systems_framework__autonomy_primacy_reading, theater_ratio, 2007, 0.07).
narrative_ontology:measurement(one__tr_t2017, one_country_two_systems_framework__autonomy_primacy_reading, theater_ratio, 2017, 0.1).
narrative_ontology:measurement(one__tr_t2027, one_country_two_systems_framework__autonomy_primacy_reading, theater_ratio, 2027, 0.12).
narrative_ontology:measurement(one__tr_t2037, one_country_two_systems_framework__autonomy_primacy_reading, theater_ratio, 2037, 0.15).
narrative_ontology:measurement(one__tr_t2047, one_country_two_systems_framework__autonomy_primacy_reading, theater_ratio, 2047, 0.18).

% Extraction over time
narrative_ontology:measurement(one__be_t1997, one_country_two_systems_framework__autonomy_primacy_reading, base_extractiveness, 1997, 0.2).
narrative_ontology:measurement(one__be_t2007, one_country_two_systems_framework__autonomy_primacy_reading, base_extractiveness, 2007, 0.22).
narrative_ontology:measurement(one__be_t2017, one_country_two_systems_framework__autonomy_primacy_reading, base_extractiveness, 2017, 0.25).
narrative_ontology:measurement(one__be_t2027, one_country_two_systems_framework__autonomy_primacy_reading, base_extractiveness, 2027, 0.28).
narrative_ontology:measurement(one__be_t2037, one_country_two_systems_framework__autonomy_primacy_reading, base_extractiveness, 2037, 0.3).
narrative_ontology:measurement(one__be_t2047, one_country_two_systems_framework__autonomy_primacy_reading, base_extractiveness, 2047, 0.32).

% Suppression requirement over time
narrative_ontology:measurement(one__su_t1997, one_country_two_systems_framework__autonomy_primacy_reading, suppression_requirement, 1997, 0.25).
narrative_ontology:measurement(one__su_t2007, one_country_two_systems_framework__autonomy_primacy_reading, suppression_requirement, 2007, 0.27).
narrative_ontology:measurement(one__su_t2017, one_country_two_systems_framework__autonomy_primacy_reading, suppression_requirement, 2017, 0.3).
narrative_ontology:measurement(one__su_t2027, one_country_two_systems_framework__autonomy_primacy_reading, suppression_requirement, 2027, 0.33).
narrative_ontology:measurement(one__su_t2037, one_country_two_systems_framework__autonomy_primacy_reading, suppression_requirement, 2037, 0.35).
narrative_ontology:measurement(one__su_t2047, one_country_two_systems_framework__autonomy_primacy_reading, suppression_requirement, 2047, 0.37).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(one_country_two_systems_framework__autonomy_primacy_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(one_country_two_systems_framework__autonomy_primacy_reading, hong_kong_basic_law_interpretation).
narrative_ontology:affects_constraint(one_country_two_systems_framework__autonomy_primacy_reading, hong_kong_national_security_law).

% DUAL FORMULATION NOTE:
% This is one of three readings of the 'One Country, Two Systems' framework. This 'autonomy primacy' reading emphasizes Hong Kong's distinct legal and political system and treaty-guaranteed civil liberties. It is linked to the 'sovereignty primacy' and 'balanced coexistence' readings, which offer alternative interpretations of the framework's core principles.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
