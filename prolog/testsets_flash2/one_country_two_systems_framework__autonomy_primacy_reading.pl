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
 *   This constraint represents the 'autonomy primacy' reading of the One
 *   Country, Two Systems framework for Hong Kong. In this reading, Hong Kong
 *   retains substantive autonomy, with civil liberties and judicial
 *   independence guaranteed by treaty and enforceable internationally.
 *   Mainland interference is considered a violation of the framework. The
 *   framework is claimed as a Rope, reflecting its original intent as a
 *   coordination mechanism for a peaceful transition and stable governance.
 *   However, the metrics reflect a gradual increase in extractiveness and
 *   suppression as the PRC's interpretation of its sovereign rights has
 *   become more assertive over time.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(one_country_two_systems_framework__autonomy_primacy_reading, 0.25).
domain_priors:suppression_score(one_country_two_systems_framework__autonomy_primacy_reading, 0.35).
domain_priors:theater_ratio(one_country_two_systems_framework__autonomy_primacy_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(one_country_two_systems_framework__autonomy_primacy_reading, extractiveness, 0.25).
narrative_ontology:constraint_metric(one_country_two_systems_framework__autonomy_primacy_reading, suppression_requirement, 0.35).
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
narrative_ontology:cs_story_uid(one_country_two_systems_framework__autonomy_primacy_reading, '83425d99-dcb9-4cc9-bd2e-0d5c41633e2d').
narrative_ontology:cs_kernel_codification('83425d99-dcb9-4cc9-bd2e-0d5c41633e2d', fixed_text).
narrative_ontology:cs_authority_grounding('83425d99-dcb9-4cc9-bd2e-0d5c41633e2d', lineage).
narrative_ontology:cs_interpretation_layer_present('83425d99-dcb9-4cc9-bd2e-0d5c41633e2d').
narrative_ontology:cs_reading_relation('83425d99-dcb9-4cc9-bd2e-0d5c41633e2d', one_country_two_systems_framework__sovereignty_primacy_reading, forecloses).
narrative_ontology:cs_reading_relation('83425d99-dcb9-4cc9-bd2e-0d5c41633e2d', one_country_two_systems_framework__balanced_coexistence_reading, coexists_with).
narrative_ontology:cs_axiom('83425d99-dcb9-4cc9-bd2e-0d5c41633e2d', foundational, high_degree_of_autonomy_is_substantive).
narrative_ontology:cs_axiom_status(high_degree_of_autonomy_is_substantive, holdable).
narrative_ontology:cs_axiom_grounding('83425d99-dcb9-4cc9-bd2e-0d5c41633e2d', high_degree_of_autonomy_is_substantive, conventional).
narrative_ontology:cs_axiom('83425d99-dcb9-4cc9-bd2e-0d5c41633e2d', foundational, international_treaty_obligations_are_binding).
narrative_ontology:cs_axiom_status(international_treaty_obligations_are_binding, holdable).
narrative_ontology:cs_axiom_grounding('83425d99-dcb9-4cc9-bd2e-0d5c41633e2d', international_treaty_obligations_are_binding, deontological).
narrative_ontology:cs_reference_frame('83425d99-dcb9-4cc9-bd2e-0d5c41633e2d', sino_british_joint_declaration_original_intent).
narrative_ontology:cs_drift_state('83425d99-dcb9-4cc9-bd2e-0d5c41633e2d', contemporary_national_security_law_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('83425d99-dcb9-4cc9-bd2e-0d5c41633e2d', '').
narrative_ontology:cs_kernel_id(one_country_two_systems_framework__autonomy_primacy_reading, one_country_two_systems_framework).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(one_country_two_systems_framework__autonomy_primacy_reading, hong_kong_residents).
narrative_ontology:constraint_beneficiary(one_country_two_systems_framework__autonomy_primacy_reading, international_community).
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

% Benefit from guaranteed civil liberties, judicial independence, and a distinct legal system. Their ability to influence policy is through local elections and protests, but ultimate sovereignty rests with the PRC. Exit options are emigration, which is costly.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__autonomy_primacy_reading, hong_kong_residents, beneficiary,
    organized, generational, constrained, local).

% Administers Hong Kong's internal affairs, including its legal system and public services, under the Basic Law. Operates with significant autonomy but is ultimately accountable to the PRC. Its legitimacy is derived from both local consent and mainland authority.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__autonomy_primacy_reading, hong_kong_government, agenda_setter,
    institutional, biographical, constrained, local).

% Is the sovereign power over Hong Kong, but is constrained by the Basic Law and the Sino-British Joint Declaration to respect Hong Kong's autonomy. Bears the political cost of international criticism if it is perceived to violate the framework. Its exit option is to unilaterally abrogate the treaty, but this carries significant international and economic costs.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__autonomy_primacy_reading, prc_central_government, payer,
    institutional, civilizational, arbitrage, national).

% Exercises independent judicial power, including that of final adjudication. Interprets the Basic Law and applies common law principles, acting as a check on executive and legislative power. Its independence is a core tenet of the 'Two Systems' principle, making identity-locked exit (abandoning judicial independence) a high cost.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__autonomy_primacy_reading, hong_kong_judiciary, agenda_setter,
    institutional, generational, identity_locked, local).

% Benefits from the stability and predictability of Hong Kong's autonomous status, particularly its role as a global financial hub with a common law system. Acts as an observer and occasional critic of PRC actions regarding Hong Kong, leveraging diplomatic and economic pressure.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__autonomy_primacy_reading, international_community, beneficiary,
    institutional, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the integration of Hong Kong into the PRC while preserving its distinct capitalist system, common law, and civil liberties, preventing a disruptive merger and facilitating international trade and finance.
% TRANSFER_FUNCTION: Transfers sovereign authority over Hong Kong to the PRC, in exchange for the PRC's commitment to a high degree of autonomy for Hong Kong, including its legal and economic systems, for 50 years.
% ABSENT_VOICES: Hardline PRC nationalists who advocate for full integration of Hong Kong into the mainland system, viewing any autonomy as a challenge to national sovereignty. They are excluded from the formal interpretation of the Basic Law but exert political pressure.
% DISAPPEARANCE_RATIONALE: If the framework vanished, Hong Kong would immediately be absorbed into the mainland system, losing its legal and economic distinctiveness. This would trigger a mass exodus of capital and talent, fundamentally altering its global role and causing significant international diplomatic fallout.
% FOUNDING_PROBLEM: To facilitate the peaceful transfer of sovereignty over Hong Kong from the UK to the PRC, ensuring stability and prosperity by preserving Hong Kong's unique system.
% FOUNDING_PROBLEM_CORROBORATION: The UK government, as a signatory to the Joint Declaration, and numerous international legal scholars and human rights organizations, corroborate the ongoing relevance of the founding problem and the framework's original intent. They actively monitor its implementation and voice concerns over perceived violations.
narrative_ontology:disappearance_verdict(one_country_two_systems_framework__autonomy_primacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(one_country_two_systems_framework__autonomy_primacy_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(one_country_two_systems_framework__autonomy_primacy_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
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
 *   The extractiveness (0.25) is relatively low in this reading, as the framework primarily serves a coordination function, but it acknowledges the inherent power asymmetry and the PRC's ultimate sovereign authority, which can impose costs. Suppression (0.35) is present due to the need to actively defend Hong Kong's autonomy against potential mainland overreach, but it is not overwhelming. Theater ratio (0.1) is low, indicating that the framework's stated functions are largely genuine, though there's a slight increase over time as the rhetoric of autonomy might diverge from practice. The temporal measurements show a gradual increase in extractiveness and suppression, reflecting the ongoing tension and the PRC's increasing assertiveness.
 *
 * PERSPECTIVAL GAP:
 *   The PRC Central Government's perspective would likely diverge significantly from this reading, emphasizing sovereignty primacy. While this reading frames the PRC as a 'payer' (bearing the cost of restraint), the PRC would likely see itself as the ultimate authority, with Hong Kong's autonomy being a delegated privilege, not an inherent right. This divergence is captured by the sibling readings and the omega variables.
 *
 * DIRECTIONALITY LOGIC:
 *   Hong Kong residents and the international community are beneficiaries, as they gain from the stability and unique status of Hong Kong. The Hong Kong government and judiciary are agenda-setters, responsible for upholding the framework's principles. The PRC Central Government is positioned as a payer in this reading, as it bears the cost of adhering to the treaty obligations and faces international scrutiny for deviations. There are no direct 'victims' in this autonomy-primacy reading, as the framework is understood to benefit all parties by preventing a more disruptive outcome.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    interpretation_of_basic_law,
    'To what extent does the PRC''s interpretation of the Basic Law (Hong Kong''s mini-constitution) align with or diverge from the ''autonomy primacy'' reading?',
    'Analysis of PRC legal interpretations, official statements, and legislative actions concerning Hong Kong, compared against international legal scholarship and Hong Kong''s judicial precedents.',
    'If PRC interpretations consistently diverge, the effective extractiveness and suppression of the framework (from Hong Kong''s perspective) are higher than stated, potentially reclassifying it towards a Tangled Rope or Snare. If they align, the Rope classification is strengthened.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(interpretation_of_basic_law, empirical, 'Ambiguity in the interpretation of the Basic Law''s provisions regarding autonomy vs. sovereignty.').

omega_variable(
    international_enforceability,
    'Is the Sino-British Joint Declaration (the treaty guaranteeing Hong Kong''s autonomy) genuinely enforceable under international law, or is it primarily a political declaration?',
    'Legal rulings by international courts (if jurisdiction is established), or a consensus among international legal bodies regarding the binding nature and enforceability of the declaration against a sovereign state.',
    'If enforceable, the ''autonomy primacy'' reading is robust, and the PRC''s costs for violating it are higher. If not, the framework''s persistence relies more on political will than legal obligation, increasing its fragility and potential for extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(international_enforceability, conceptual, 'Uncertainty regarding the international legal enforceability of the foundational treaty.').

omega_variable(
    democratic_reform_pathway,
    'Does the Basic Law genuinely provide a pathway for democratic reform and universal suffrage in Hong Kong, or is this pathway structurally foreclosed by PRC interpretations?',
    'Observation of legislative processes, electoral reforms, and judicial decisions in Hong Kong over time, specifically regarding the implementation of universal suffrage, and comparison with the Basic Law''s provisions.',
    'If the pathway is foreclosed, the ''autonomy primacy'' reading''s claim of civil liberties and self-governance is undermined, increasing the perceived suppression and extractiveness for Hong Kong residents. If it remains live, the Rope classification is more stable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(democratic_reform_pathway, empirical, 'Ambiguity regarding the feasibility of democratic reform under the framework.').


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
narrative_ontology:measurement(one__tr_t2017, one_country_two_systems_framework__autonomy_primacy_reading, theater_ratio, 2017, 0.09).
narrative_ontology:measurement(one__tr_t2027, one_country_two_systems_framework__autonomy_primacy_reading, theater_ratio, 2027, 0.1).
narrative_ontology:measurement(one__tr_t2037, one_country_two_systems_framework__autonomy_primacy_reading, theater_ratio, 2037, 0.11).
narrative_ontology:measurement(one__tr_t2047, one_country_two_systems_framework__autonomy_primacy_reading, theater_ratio, 2047, 0.12).

% Extraction over time
narrative_ontology:measurement(one__be_t1997, one_country_two_systems_framework__autonomy_primacy_reading, base_extractiveness, 1997, 0.15).
narrative_ontology:measurement(one__be_t2007, one_country_two_systems_framework__autonomy_primacy_reading, base_extractiveness, 2007, 0.18).
narrative_ontology:measurement(one__be_t2017, one_country_two_systems_framework__autonomy_primacy_reading, base_extractiveness, 2017, 0.22).
narrative_ontology:measurement(one__be_t2027, one_country_two_systems_framework__autonomy_primacy_reading, base_extractiveness, 2027, 0.25).
narrative_ontology:measurement(one__be_t2037, one_country_two_systems_framework__autonomy_primacy_reading, base_extractiveness, 2037, 0.28).
narrative_ontology:measurement(one__be_t2047, one_country_two_systems_framework__autonomy_primacy_reading, base_extractiveness, 2047, 0.3).

% Suppression requirement over time
narrative_ontology:measurement(one__su_t1997, one_country_two_systems_framework__autonomy_primacy_reading, suppression_requirement, 1997, 0.2).
narrative_ontology:measurement(one__su_t2007, one_country_two_systems_framework__autonomy_primacy_reading, suppression_requirement, 2007, 0.25).
narrative_ontology:measurement(one__su_t2017, one_country_two_systems_framework__autonomy_primacy_reading, suppression_requirement, 2017, 0.3).
narrative_ontology:measurement(one__su_t2027, one_country_two_systems_framework__autonomy_primacy_reading, suppression_requirement, 2027, 0.35).
narrative_ontology:measurement(one__su_t2037, one_country_two_systems_framework__autonomy_primacy_reading, suppression_requirement, 2037, 0.4).
narrative_ontology:measurement(one__su_t2047, one_country_two_systems_framework__autonomy_primacy_reading, suppression_requirement, 2047, 0.45).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(one_country_two_systems_framework__autonomy_primacy_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(one_country_two_systems_framework__autonomy_primacy_reading, hong_kong_financial_market_regulation).
narrative_ontology:affects_constraint(one_country_two_systems_framework__autonomy_primacy_reading, hong_kong_press_freedom).
narrative_ontology:affects_constraint(one_country_two_systems_framework__autonomy_primacy_reading, hong_kong_judicial_independence).

% DUAL FORMULATION NOTE:
% This is one of three readings of the 'One Country, Two Systems' framework. This 'autonomy primacy' reading emphasizes Hong Kong's guaranteed self-governance and civil liberties, distinct from the 'sovereignty primacy' and 'balanced coexistence' readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
