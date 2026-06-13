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
 *   constraint_id: one_country_two_systems_framework__autonomy_primacy_reading
 *   human_readable: Hong Kong Autonomy under One Country, Two Systems (Autonomy Primacy Reading)
 *   domain: constitutional_law/political_systems/state_sovereignty
 *
 * SUMMARY:
 *   This constraint represents the 'autonomy_primacy_reading' of the 'One
 *   Country, Two Systems' framework, where Hong Kong retains substantive
 *   autonomy, civil liberties, and judicial independence, guaranteed by
 *   treaty and internationally enforceable. This reading emphasizes the
 *   distinct legal and political systems of Hong Kong, with mainland
 *   interference viewed as a violation. It is one interpretation of a
 *   contested kernel, focusing on the preservation of Hong Kong's unique
 *   status.
 *
 * KEY AGENTS:
 *   - hong_kong_residents: Primary beneficiary (moderate/constrained) — enjoy civil liberties and judicial independence.
 *   - hong_kong_government: Agenda setter (institutional/constrained) — administers local affairs under the Basic Law.
 *   - prc_central_government: Agenda setter (institutional/arbitrage) — exercises ultimate sovereignty but is constrained by treaty obligations.
 *   - international_community: Beneficiary/Observer (institutional/analytical) — benefits from stability and adherence to international law, observes compliance.
 *   - hong_kong_judiciary: Agenda setter (institutional/constrained) — upholds the Basic Law and judicial independence.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(one_country_two_systems_framework__autonomy_primacy_reading, 0.2).
domain_priors:suppression_score(one_country_two_systems_framework__autonomy_primacy_reading, 0.15).
domain_priors:theater_ratio(one_country_two_systems_framework__autonomy_primacy_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(one_country_two_systems_framework__autonomy_primacy_reading, extractiveness, 0.2).
narrative_ontology:constraint_metric(one_country_two_systems_framework__autonomy_primacy_reading, suppression_requirement, 0.15).
narrative_ontology:constraint_metric(one_country_two_systems_framework__autonomy_primacy_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(one_country_two_systems_framework__autonomy_primacy_reading, accessibility_collapse, 0.2).
narrative_ontology:constraint_metric(one_country_two_systems_framework__autonomy_primacy_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(one_country_two_systems_framework__autonomy_primacy_reading, rope).
narrative_ontology:human_readable(one_country_two_systems_framework__autonomy_primacy_reading, "Hong Kong Autonomy under One Country, Two Systems (Autonomy Primacy Reading)").
narrative_ontology:topic_domain(one_country_two_systems_framework__autonomy_primacy_reading, "constitutional_law/political_systems/state_sovereignty").

domain_priors:requires_active_enforcement(one_country_two_systems_framework__autonomy_primacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(one_country_two_systems_framework__autonomy_primacy_reading, '9a901589-97d4-4412-b881-2fa157371ad7').
narrative_ontology:cs_kernel_codification('9a901589-97d4-4412-b881-2fa157371ad7', fixed_text).
narrative_ontology:cs_authority_grounding('9a901589-97d4-4412-b881-2fa157371ad7', lineage).
narrative_ontology:cs_interpretation_layer_present('9a901589-97d4-4412-b881-2fa157371ad7').
narrative_ontology:cs_reading_relation('9a901589-97d4-4412-b881-2fa157371ad7', one_country_two_systems_framework__sovereignty_primacy_reading, forecloses).
narrative_ontology:cs_reading_relation('9a901589-97d4-4412-b881-2fa157371ad7', one_country_two_systems_framework__balanced_coexistence_reading, coexists_with).
narrative_ontology:cs_axiom('9a901589-97d4-4412-b881-2fa157371ad7', foundational, hong_kong_autonomy_is_guaranteed).
narrative_ontology:cs_axiom_status(hong_kong_autonomy_is_guaranteed, holdable).
narrative_ontology:cs_axiom_grounding('9a901589-97d4-4412-b881-2fa157371ad7', hong_kong_autonomy_is_guaranteed, deontological).
narrative_ontology:cs_axiom('9a901589-97d4-4412-b881-2fa157371ad7', foundational, international_treaty_obligations_are_binding).
narrative_ontology:cs_axiom_status(international_treaty_obligations_are_binding, holdable).
narrative_ontology:cs_axiom_grounding('9a901589-97d4-4412-b881-2fa157371ad7', international_treaty_obligations_are_binding, conventional).
narrative_ontology:cs_reference_frame('9a901589-97d4-4412-b881-2fa157371ad7', sino_british_joint_declaration_framework).
narrative_ontology:cs_drift_state('9a901589-97d4-4412-b881-2fa157371ad7', contemporary_national_security_law_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('9a901589-97d4-4412-b881-2fa157371ad7', '').
narrative_ontology:cs_kernel_id(one_country_two_systems_framework__autonomy_primacy_reading, one_country_two_systems_framework).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(one_country_two_systems_framework__autonomy_primacy_reading, hong_kong_residents).
narrative_ontology:constraint_beneficiary(one_country_two_systems_framework__autonomy_primacy_reading, international_community).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Enjoy civil liberties, judicial independence, and a distinct way of life guaranteed by the Basic Law and the Sino-British Joint Declaration. Their ability to exit is constrained by geographic and political realities, but they benefit from the framework's protections.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__autonomy_primacy_reading, hong_kong_residents, beneficiary,
    moderate, biographical, constrained, local).

% Administers Hong Kong's affairs, including its legal system, economy, and public services, with a high degree of autonomy. Operates within the bounds of the Basic Law and is accountable to both Hong Kong residents and, ultimately, the PRC Central Government. Its autonomy is a core feature of this reading.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__autonomy_primacy_reading, hong_kong_government, agenda_setter,
    institutional, generational, constrained, local).

% Exercises ultimate sovereignty over Hong Kong but is bound by the Sino-British Joint Declaration and the Basic Law to respect Hong Kong's high degree of autonomy. In this reading, its interventions are limited to defense and foreign affairs, and it refrains from interfering in Hong Kong's internal affairs.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__autonomy_primacy_reading, prc_central_government, agenda_setter,
    institutional, civilizational, arbitrage, national).

% Benefits from the stability and rule of law in Hong Kong, which facilitates international trade and investment. Observes the implementation of 'One Country, Two Systems' and holds the PRC accountable to its treaty obligations. Its influence is primarily diplomatic and economic.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__autonomy_primacy_reading, international_community, beneficiary,
    organized, generational, analytical, global).
narrative_ontology:stakeholder_secondary_role(one_country_two_systems_framework__autonomy_primacy_reading, international_community, observer).

% Upholds the rule of law and judicial independence in Hong Kong, interpreting the Basic Law and acting as a check on executive power. Its autonomy from mainland legal systems is a cornerstone of the 'Two Systems' principle in this reading.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__autonomy_primacy_reading, hong_kong_judiciary, agenda_setter,
    institutional, generational, constrained, local).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To coordinate the peaceful transfer of sovereignty over Hong Kong from the UK to the PRC while preserving Hong Kong's distinct capitalist system, common law, and civil liberties, thereby ensuring stability and prosperity for both Hong Kong and the mainland.
% TRANSFER_FUNCTION: Transfers political authority over Hong Kong from the UK to the PRC, while simultaneously transferring a guarantee of autonomy and specific rights to Hong Kong residents, and a commitment to international treaty obligations to the international community.
% ABSENT_VOICES: Those who advocate for full independence for Hong Kong are structurally excluded from the framework's design and implementation, as their position fundamentally rejects the 'One Country' principle. They would argue that the framework is inherently flawed and insufficient to protect Hong Kong's identity.
% DISAPPEARANCE_RATIONALE: If the 'One Country, Two Systems' framework, as understood by this reading, disappeared overnight, Hong Kong's legal and political systems would likely be fully integrated into mainland China's, leading to a loss of civil liberties, judicial independence, and economic distinctiveness. This would trigger significant international outcry, capital flight, and a fundamental reorganization of Hong Kong society.
% FOUNDING_PROBLEM: The problem was how to reconcile the PRC's claim to sovereignty over Hong Kong with the need to preserve Hong Kong's unique economic and social system, which was vital for its prosperity and international standing, after the 1997 handover from British rule.
% FOUNDING_PROBLEM_CORROBORATION: The PRC Central Government attests the problem is live, citing national security and territorial integrity concerns. Hong Kong residents and the international community, supported by legal scholars and human rights organizations, attest that the original problem of preserving autonomy is increasingly undermined by mainland interventions, suggesting the framework's function has shifted from coordination to a tool for gradual integration.
narrative_ontology:disappearance_verdict(one_country_two_systems_framework__autonomy_primacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(one_country_two_systems_framework__autonomy_primacy_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(one_country_two_systems_framework__autonomy_primacy_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(one_country_two_systems_framework__autonomy_primacy_reading, 'none', 1).

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
 *   The 'autonomy_primacy_reading' frames the constraint as a Rope, where the primary function is to coordinate two distinct systems for mutual benefit (stability, economic prosperity, international trust). Extractiveness (0.2) is low, reflecting the idea that civil liberties are largely preserved and mainland interference is minimal. Suppression (0.15) is also low, as the framework is understood to protect against coercive measures. Theater ratio (0.1) is low, as the institutions of autonomy are genuinely functional. The metrics reflect the ideal operation of this specific reading, not the contested reality.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of Hong Kong residents and the international community, this reading emphasizes the benefits of autonomy and rule of law. From the PRC's perspective, a 'sovereignty_primacy_reading' would emphasize the delegated nature of autonomy, leading to a different classification. The engine's per-seat classification would highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Hong Kong residents are beneficiaries (d near 0.0) as they enjoy guaranteed rights. The Hong Kong government is an agenda setter (d near 0.5) as it administers the system, balancing local interests with treaty obligations. The PRC Central Government, while ultimately sovereign, is also an agenda setter (d near 0.5) in this reading, as it is bound by the treaty to respect autonomy. The international community is a beneficiary/observer (d near 0.0) as it benefits from the stability and rule of law. No explicit victims are declared in this reading, as the framework is understood to protect all parties.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading prevents mislabeling genuine coordination as extraction by emphasizing the treaty-guaranteed nature of autonomy. If the mandate for autonomy were to atrophy, and the constraint became purely about PRC sovereignty, it would shift from a Rope to a Snare, indicating a failure of the original coordination function. The low extractiveness and suppression in this reading reflect the assumption that the mandate for autonomy is still live and respected.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    autonomy_vs_sovereignty_ambiguity,
    'Is the ''One Country, Two Systems'' framework primarily a guarantee of Hong Kong''s autonomy, or a declaration of PRC''s ultimate sovereignty?',
    'Observation of PRC''s actions regarding Hong Kong''s Basic Law interpretation and enforcement, particularly in areas of national security and electoral reform. If PRC consistently defers to HK''s judicial and legislative processes, it supports autonomy primacy. If PRC unilaterally imposes changes, it supports sovereignty primacy.',
    'If autonomy primacy holds, the framework functions as a Rope, coordinating distinct legal systems. If sovereignty primacy prevails, it becomes a Snare, where autonomy is a temporary concession subject to revocation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(autonomy_vs_sovereignty_ambiguity, empirical, 'Ambiguity in the core principle of ''One Country, Two Systems'' framework.').

omega_variable(
    democratic_reform_pathway_viability,
    'Does the ''autonomy_primacy_reading'' of One Country, Two Systems genuinely preserve a live pathway for democratic reform in Hong Kong, or is this pathway foreclosed by other structural constraints?',
    'Observation of legislative and electoral processes in Hong Kong over time. If genuine progress towards universal suffrage is made without external interference, the pathway is live. If reforms are consistently blocked or reversed by Beijing, the pathway is foreclosed.',
    'If the democratic pathway is live, the constraint retains its ''Rope'' character for residents. If foreclosed, the constraint''s coordination function is diminished, and it leans towards a ''Tangled Rope'' or ''Snare'' for those seeking political participation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(democratic_reform_pathway_viability, empirical, 'Viability of democratic reform under the autonomy primacy reading.').

omega_variable(
    kernel_reading_identification,
    'This constraint is the ''autonomy_primacy_reading'' of the ''one_country_two_systems_framework'' kernel. What would change if a ''sovereignty_primacy_reading'' were adopted?',
    'Conceptual analysis of legal texts and historical precedents, combined with observation of political discourse and actions. The resolution is in identifying which reading is being actively enforced.',
    'A ''sovereignty_primacy_reading'' would shift the constraint from a Rope to a Snare or Tangled Rope, as Hong Kong''s autonomy would be seen as a delegated privilege rather than a guaranteed right, leading to higher extraction and suppression for Hong Kong residents.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identification, conceptual, 'Impact of adopting a sibling reading of the ''One Country, Two Systems'' kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(one_country_two_systems_framework__autonomy_primacy_reading, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(one__tr_t0, one_country_two_systems_framework__autonomy_primacy_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(one__tr_t5, one_country_two_systems_framework__autonomy_primacy_reading, theater_ratio, 5, 0.08).
narrative_ontology:measurement(one__tr_t10, one_country_two_systems_framework__autonomy_primacy_reading, theater_ratio, 10, 0.1).

% Extraction over time
narrative_ontology:measurement(one__be_t0, one_country_two_systems_framework__autonomy_primacy_reading, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(one__be_t5, one_country_two_systems_framework__autonomy_primacy_reading, base_extractiveness, 5, 0.17).
narrative_ontology:measurement(one__be_t10, one_country_two_systems_framework__autonomy_primacy_reading, base_extractiveness, 10, 0.2).

% Suppression requirement over time
narrative_ontology:measurement(one__su_t0, one_country_two_systems_framework__autonomy_primacy_reading, suppression_requirement, 0, 0.1).
narrative_ontology:measurement(one__su_t5, one_country_two_systems_framework__autonomy_primacy_reading, suppression_requirement, 5, 0.12).
narrative_ontology:measurement(one__su_t10, one_country_two_systems_framework__autonomy_primacy_reading, suppression_requirement, 10, 0.15).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(one_country_two_systems_framework__autonomy_primacy_reading, enforcement_mechanism).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'One Country, Two Systems' kernel. Other readings (sovereignty_primacy_reading, balanced_coexistence_reading) are modeled as separate constraints, reflecting different structural interpretations of the same foundational framework.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
