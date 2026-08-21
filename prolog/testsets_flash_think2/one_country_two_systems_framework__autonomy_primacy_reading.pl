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
 *   domain: constitutional_law/political_systems/international_relations
 *
 * SUMMARY:
 *   This constraint story instantiates the 'autonomy primacy' reading of the
 *   'One Country, Two Systems' framework for Hong Kong. Under this reading,
 *   Hong Kong retains substantive autonomy with meaningful checks on mainland
 *   interference; civil liberties and judicial independence are
 *   treaty-guaranteed and internationally enforceable. Mainland intervention
 *   is considered a treaty violation, civil liberties remain low-epsilon for
 *   most residents, judicial review constrains executive power, and a
 *   democratic reform pathway remains live. The metrics reflect the ideal
 *   state of this reading, where the framework functions as a genuine
 *   coordination mechanism with low extraction and suppression.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(one_country_two_systems_framework__autonomy_primacy_reading, 0.15).
domain_priors:suppression_score(one_country_two_systems_framework__autonomy_primacy_reading, 0.1).
domain_priors:theater_ratio(one_country_two_systems_framework__autonomy_primacy_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(one_country_two_systems_framework__autonomy_primacy_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(one_country_two_systems_framework__autonomy_primacy_reading, suppression_requirement, 0.1).
narrative_ontology:constraint_metric(one_country_two_systems_framework__autonomy_primacy_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(one_country_two_systems_framework__autonomy_primacy_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(one_country_two_systems_framework__autonomy_primacy_reading, resistance, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(one_country_two_systems_framework__autonomy_primacy_reading, rope).
narrative_ontology:human_readable(one_country_two_systems_framework__autonomy_primacy_reading, "Hong Kong Autonomy under One Country, Two Systems (Autonomy Primacy Reading)").
narrative_ontology:topic_domain(one_country_two_systems_framework__autonomy_primacy_reading, "constitutional_law/political_systems/international_relations").

domain_priors:requires_active_enforcement(one_country_two_systems_framework__autonomy_primacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(one_country_two_systems_framework__autonomy_primacy_reading, '18b33d44-df08-4c6a-a3df-a3ef75d08235').
narrative_ontology:cs_kernel_codification('18b33d44-df08-4c6a-a3df-a3ef75d08235', fixed_text).
narrative_ontology:cs_authority_grounding('18b33d44-df08-4c6a-a3df-a3ef75d08235', lineage).
narrative_ontology:cs_interpretation_layer_present('18b33d44-df08-4c6a-a3df-a3ef75d08235').
narrative_ontology:cs_reading_relation('18b33d44-df08-4c6a-a3df-a3ef75d08235', one_country_two_systems_framework__sovereignty_primacy_reading, forecloses).
narrative_ontology:cs_reading_relation('18b33d44-df08-4c6a-a3df-a3ef75d08235', one_country_two_systems_framework__balanced_coexistence_reading, forecloses).
narrative_ontology:cs_axiom('18b33d44-df08-4c6a-a3df-a3ef75d08235', foundational, treaty_guarantees_are_supreme).
narrative_ontology:cs_axiom_status(treaty_guarantees_are_supreme, holdable).
narrative_ontology:cs_axiom_grounding('18b33d44-df08-4c6a-a3df-a3ef75d08235', treaty_guarantees_are_supreme, deontological).
narrative_ontology:cs_axiom('18b33d44-df08-4c6a-a3df-a3ef75d08235', foundational, judicial_independence_is_absolute).
narrative_ontology:cs_axiom_status(judicial_independence_is_absolute, holdable).
narrative_ontology:cs_axiom_grounding('18b33d44-df08-4c6a-a3df-a3ef75d08235', judicial_independence_is_absolute, conventional).
narrative_ontology:cs_reference_frame('18b33d44-df08-4c6a-a3df-a3ef75d08235', treaty_guaranteed_autonomy).
narrative_ontology:cs_drift_state('18b33d44-df08-4c6a-a3df-a3ef75d08235', post_national_security_law_era, gap(repudiation_pressure, severe, true)).
narrative_ontology:cs_created_at('18b33d44-df08-4c6a-a3df-a3ef75d08235', '').
narrative_ontology:cs_kernel_id(one_country_two_systems_framework__autonomy_primacy_reading, one_country_two_systems_framework).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(one_country_two_systems_framework__autonomy_primacy_reading, hong_kong_residents).
narrative_ontology:constraint_beneficiary(one_country_two_systems_framework__autonomy_primacy_reading, international_community).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(one_country_two_systems_framework__autonomy_primacy_reading, prc_central_government).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefit from guaranteed civil liberties, rule of law, and judicial independence, which distinguish Hong Kong from mainland China. Their ability to exit the system is constrained by their identity and location.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__autonomy_primacy_reading, hong_kong_residents, beneficiary,
    moderate, biographical, constrained, regional).

% Upholds the Basic Law and interprets the Sino-British Joint Declaration, acting as a critical check on executive power and mainland interference. Their institutional identity is fused with the common law system.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__autonomy_primacy_reading, hong_kong_judiciary, agenda_setter,
    institutional, generational, identity_locked, regional).

% Benefits from the stability, rule of law, and treaty adherence that the framework represents, particularly for international trade and diplomatic norms. They observe and, in principle, enforce the treaty guarantees.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__autonomy_primacy_reading, international_community, beneficiary,
    institutional, civilizational, analytical, global).

% Under this reading, the PRC must respect Hong Kong's high degree of autonomy and is constrained from direct intervention, bearing the cost of limited sovereignty over a part of its territory. It has the power to unilaterally alter the framework, but doing so would violate international treaties.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__autonomy_primacy_reading, prc_central_government, payer,
    institutional, civilizational, arbitrage, national).

% Administers Hong Kong under the Basic Law, balancing local governance with the constitutional framework. Its legitimacy derives from upholding the autonomy and rule of law.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__autonomy_primacy_reading, hong_kong_government, agenda_setter,
    institutional, biographical, constrained, regional).

% Monitor compliance with international treaties and human rights conventions relevant to Hong Kong, providing reports and recommendations that can influence international opinion and policy.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__autonomy_primacy_reading, international_treaty_bodies, observer,
    institutional, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the peaceful transfer of sovereignty over Hong Kong from the UK to the PRC while preserving Hong Kong's distinct legal, economic, and social systems, including civil liberties and judicial independence, under international treaty guarantees.
% TRANSFER_FUNCTION: Transfers sovereign authority over Hong Kong to the PRC, while simultaneously guaranteeing a high degree of autonomy and civil liberties to Hong Kong residents, and establishing a framework for international oversight of these guarantees.
% ABSENT_VOICES: Those advocating for full Hong Kong independence or complete integration with mainland China are structurally excluded from the framework's core design, as it seeks a specific middle ground. Their perspectives are not accommodated within the 'One Country, Two Systems' paradigm.
% DISAPPEARANCE_RATIONALE: If the 'One Country, Two Systems' framework, as understood by this reading, vanished overnight, Hong Kong would either be fully integrated into mainland China (losing its distinct systems and freedoms) or attempt full independence (leading to severe geopolitical instability). The regional and international political landscape would fundamentally reorganize.
% FOUNDING_PROBLEM: How to ensure a smooth and stable transfer of sovereignty over Hong Kong from the United Kingdom to the People's Republic of China in 1997, preserving Hong Kong's capitalist system, common law, and civil liberties, without causing mass emigration or international outcry, while acknowledging PRC sovereignty.
% FOUNDING_PROBLEM_CORROBORATION: The Sino-British Joint Declaration and the Basic Law texts corroborate the founding problem and the original intent. However, international legal scholars, human rights organizations, and many Hong Kong residents attest that the founding problem's status is now contested, citing the erosion of autonomy and civil liberties by mainland intervention.
narrative_ontology:disappearance_verdict(one_country_two_systems_framework__autonomy_primacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(one_country_two_systems_framework__autonomy_primacy_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(one_country_two_systems_framework__autonomy_primacy_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(one_country_two_systems_framework__autonomy_primacy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(one_country_two_systems_framework__autonomy_primacy_reading, 0.15, 'gemini-2.5-flash', 'none', direct).

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
 *   The low extractiveness (0.15) and suppression (0.10) reflect the core premise of this reading: that the framework effectively protects Hong Kong's distinct systems and freedoms, with minimal imposition from the mainland. The low theater ratio (0.05) indicates that the mechanisms for autonomy and judicial independence are genuinely functional, not merely performative. The measurements are held constant to reflect the *assertion* of this reading that the framework, by its very nature, should maintain these conditions, even in the face of external pressures which are captured in the cs_structure drift_state.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of this 'autonomy primacy' reading, the framework is a robust Rope, ensuring coordination and protection. However, other readings (e.g., 'sovereignty primacy') would experience this same framework as highly extractive and suppressive, or as a mere delegation of power. The engine's per-seat classification will highlight this divergence based on the structural data of each reading.
 *
 * DIRECTIONALITY LOGIC:
 *   Hong Kong residents and the international community are beneficiaries, as the framework guarantees their rights and upholds international norms. The PRC Central Government is positioned as a 'payer' in this reading, as it bears the cost of constrained sovereignty and must adhere to treaty obligations, limiting its ability to intervene directly. The Hong Kong Judiciary and Government are agenda-setters, responsible for upholding and administering this autonomous system.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    autonomy_vs_sovereignty_primacy,
    'Is the ''One Country, Two Systems'' framework fundamentally structured around the primacy of Hong Kong''s autonomy and treaty-guaranteed rights, or the ultimate sovereignty of the PRC?',
    'Resolution would require a definitive international legal ruling on the interpretation of the Sino-British Joint Declaration and the Basic Law, or a clear, consistent pattern of state practice by the PRC that either respects or overrides autonomy.',
    'If sovereignty is ultimately primary, this ''autonomy primacy'' reading is foreclosed, and the constraint reclassifies to a Snare or Tangled Rope from the perspective of Hong Kong residents, with high extraction and suppression.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(autonomy_vs_sovereignty_primacy, conceptual, 'Whether the framework''s core legal hierarchy prioritizes autonomy or sovereignty.').

omega_variable(
    international_enforceability_of_guarantees,
    'Are the international guarantees for Hong Kong''s autonomy and civil liberties genuinely enforceable against a sovereign state, or are they primarily aspirational?',
    'Observation of the international community''s capacity and willingness to impose meaningful consequences for violations, or a test case in an international court with binding jurisdiction.',
    'If not genuinely enforceable, the constraint''s effective suppression and extractiveness for Hong Kong residents are higher than stated, as the ultimate check on mainland power is absent, leading to a reclassification towards Snare.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(international_enforceability_of_guarantees, empirical, 'The actual capacity of international law to enforce treaty obligations on a sovereign state.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(one_country_two_systems_framework__autonomy_primacy_reading, 1984, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(one__tr_t1984, one_country_two_systems_framework__autonomy_primacy_reading, theater_ratio, 1984, 0.05).
narrative_ontology:measurement(one__tr_t1994, one_country_two_systems_framework__autonomy_primacy_reading, theater_ratio, 1994, 0.05).
narrative_ontology:measurement(one__tr_t2004, one_country_two_systems_framework__autonomy_primacy_reading, theater_ratio, 2004, 0.05).
narrative_ontology:measurement(one__tr_t2014, one_country_two_systems_framework__autonomy_primacy_reading, theater_ratio, 2014, 0.05).
narrative_ontology:measurement(one__tr_t2024, one_country_two_systems_framework__autonomy_primacy_reading, theater_ratio, 2024, 0.05).

% Extraction over time
narrative_ontology:measurement(one__be_t1984, one_country_two_systems_framework__autonomy_primacy_reading, base_extractiveness, 1984, 0.15).
narrative_ontology:measurement(one__be_t1994, one_country_two_systems_framework__autonomy_primacy_reading, base_extractiveness, 1994, 0.15).
narrative_ontology:measurement(one__be_t2004, one_country_two_systems_framework__autonomy_primacy_reading, base_extractiveness, 2004, 0.15).
narrative_ontology:measurement(one__be_t2014, one_country_two_systems_framework__autonomy_primacy_reading, base_extractiveness, 2014, 0.15).
narrative_ontology:measurement(one__be_t2024, one_country_two_systems_framework__autonomy_primacy_reading, base_extractiveness, 2024, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(one__su_t1984, one_country_two_systems_framework__autonomy_primacy_reading, suppression_requirement, 1984, 0.1).
narrative_ontology:measurement(one__su_t1994, one_country_two_systems_framework__autonomy_primacy_reading, suppression_requirement, 1994, 0.1).
narrative_ontology:measurement(one__su_t2004, one_country_two_systems_framework__autonomy_primacy_reading, suppression_requirement, 2004, 0.1).
narrative_ontology:measurement(one__su_t2014, one_country_two_systems_framework__autonomy_primacy_reading, suppression_requirement, 2014, 0.1).
narrative_ontology:measurement(one__su_t2024, one_country_two_systems_framework__autonomy_primacy_reading, suppression_requirement, 2024, 0.1).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(one_country_two_systems_framework__autonomy_primacy_reading, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
