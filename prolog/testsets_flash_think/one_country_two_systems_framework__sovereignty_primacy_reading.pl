% ============================================================================
% CONSTRAINT STORY: one_country_two_systems_framework__sovereignty_primacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_one_country_two_systems_framework__sovereignty_primacy_reading, []).

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
 *   constraint_id: one_country_two_systems_framework__sovereignty_primacy_reading
 *   human_readable: PRC Sovereignty Primacy over Hong Kong Autonomy (One Country, Two Systems)
 *   domain: constitutional_law/political_systems/state_sovereignty
 *
 * SUMMARY:
 *   This constraint story instantiates the 'sovereignty primacy' reading of
 *   the 'One Country, Two Systems' framework, where Hong Kong's autonomy is
 *   understood as delegated by and revocable through PRC sovereign authority.
 *   In this reading, national security and territorial integrity are
 *   paramount and override local autonomy when conflicts arise. This
 *   interpretation has led to the implementation of the National Security
 *   Law, the operation of mainland enforcement agents in Hong Kong, the
 *   curtailment of political speech and assembly, and a significant loss of
 *   judicial independence in national security matters.
 *
 * KEY AGENTS:
 *   - prc_central_government: Primary agenda_setter (institutional/arbitrage) — benefits from control
 *   - hong_kong_citizens: Primary payer (powerless/trapped) — bears costs of reduced freedoms
 *   - pro_democracy_activists: Direct target/payer (powerless/trapped) — bears costs of suppression
 *   - hong_kong_judiciary: Payer (institutional/constrained) — loses independence
 *   - hong_kong_loyalist_elites: Beneficiary (powerful/mobile) — benefits from alignment with PRC
 *   - international_community: Observer (institutional/analytical) — limited direct power
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(one_country_two_systems_framework__sovereignty_primacy_reading, 0.85).
domain_priors:suppression_score(one_country_two_systems_framework__sovereignty_primacy_reading, 0.9).
domain_priors:theater_ratio(one_country_two_systems_framework__sovereignty_primacy_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(one_country_two_systems_framework__sovereignty_primacy_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(one_country_two_systems_framework__sovereignty_primacy_reading, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(one_country_two_systems_framework__sovereignty_primacy_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(one_country_two_systems_framework__sovereignty_primacy_reading, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(one_country_two_systems_framework__sovereignty_primacy_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(one_country_two_systems_framework__sovereignty_primacy_reading, snare).
narrative_ontology:human_readable(one_country_two_systems_framework__sovereignty_primacy_reading, "PRC Sovereignty Primacy over Hong Kong Autonomy (One Country, Two Systems)").
narrative_ontology:topic_domain(one_country_two_systems_framework__sovereignty_primacy_reading, "constitutional_law/political_systems/state_sovereignty").

domain_priors:requires_active_enforcement(one_country_two_systems_framework__sovereignty_primacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(one_country_two_systems_framework__sovereignty_primacy_reading, 'a35c6235-a3f2-4eff-8d8e-8c679e88465d').
narrative_ontology:cs_kernel_codification('a35c6235-a3f2-4eff-8d8e-8c679e88465d', fixed_text).
narrative_ontology:cs_authority_grounding('a35c6235-a3f2-4eff-8d8e-8c679e88465d', lineage).
narrative_ontology:cs_interpretation_layer_present('a35c6235-a3f2-4eff-8d8e-8c679e88465d').
narrative_ontology:cs_reading_relation('a35c6235-a3f2-4eff-8d8e-8c679e88465d', one_country_two_systems_framework__autonomy_primacy_reading, forecloses).
narrative_ontology:cs_reading_relation('a35c6235-a3f2-4eff-8d8e-8c679e88465d', one_country_two_systems_framework__balanced_coexistence_reading, forecloses).
narrative_ontology:cs_axiom('a35c6235-a3f2-4eff-8d8e-8c679e88465d', foundational, prc_sovereignty_is_absolute).
narrative_ontology:cs_axiom_status(prc_sovereignty_is_absolute, holdable).
narrative_ontology:cs_axiom_grounding('a35c6235-a3f2-4eff-8d8e-8c679e88465d', prc_sovereignty_is_absolute, conventional).
narrative_ontology:cs_axiom('a35c6235-a3f2-4eff-8d8e-8c679e88465d', foundational, national_security_overrides_local_law).
narrative_ontology:cs_axiom_status(national_security_overrides_local_law, holdable).
narrative_ontology:cs_axiom_grounding('a35c6235-a3f2-4eff-8d8e-8c679e88465d', national_security_overrides_local_law, conventional).
narrative_ontology:cs_reference_frame('a35c6235-a3f2-4eff-8d8e-8c679e88465d', prc_unquestionable_sovereignty).
narrative_ontology:cs_drift_state('a35c6235-a3f2-4eff-8d8e-8c679e88465d', post_national_security_law_era, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('a35c6235-a3f2-4eff-8d8e-8c679e88465d', '').
narrative_ontology:cs_kernel_id(one_country_two_systems_framework__sovereignty_primacy_reading, one_country_two_systems_framework).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(one_country_two_systems_framework__sovereignty_primacy_reading, prc_central_government).
narrative_ontology:constraint_beneficiary(one_country_two_systems_framework__sovereignty_primacy_reading, hong_kong_loyalist_elites).
narrative_ontology:constraint_victim(one_country_two_systems_framework__sovereignty_primacy_reading, hong_kong_citizens).
narrative_ontology:constraint_victim(one_country_two_systems_framework__sovereignty_primacy_reading, pro_democracy_activists).
narrative_ontology:constraint_victim(one_country_two_systems_framework__sovereignty_primacy_reading, hong_kong_judiciary).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Asserts ultimate sovereign authority over Hong Kong, interpreting the 'One Country, Two Systems' framework as a delegation of power that is revocable and subordinate to national security and territorial integrity. Benefits from consolidated political control and suppressed dissent.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__sovereignty_primacy_reading, prc_central_government, agenda_setter,
    institutional, generational, arbitrage, global).

% Bear the costs of reduced civil liberties, freedom of speech, and political participation. Their promised 'high degree of autonomy' is eroded by the primacy of PRC sovereign authority, with limited recourse or exit options within the system.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__sovereignty_primacy_reading, hong_kong_citizens, payer,
    powerless, biographical, trapped, local).

% Direct targets of enforcement under the National Security Law, facing arrest, prosecution, and suppression of their activities. Their ability to organize or express dissent is severely curtailed, with high personal costs for non-compliance.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__sovereignty_primacy_reading, pro_democracy_activists, payer,
    powerless, immediate, trapped, local).

% Experiences a loss of independence, particularly in national security cases, where PRC interpretations and laws take precedence. Judges operate under pressure to align with Beijing's directives, compromising the common law tradition.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__sovereignty_primacy_reading, hong_kong_judiciary, payer,
    institutional, biographical, constrained, local).

% Benefit from aligning with the PRC central government, gaining political influence, economic opportunities, and protection within the new political order. They actively support the sovereignty primacy reading and its enforcement.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__sovereignty_primacy_reading, hong_kong_loyalist_elites, beneficiary,
    powerful, biographical, mobile, local).

% Observes the erosion of Hong Kong's autonomy, issuing statements and imposing sanctions, but with limited direct power to alter the constraint's operation. Their influence is primarily diplomatic and economic.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__sovereignty_primacy_reading, international_community, observer,
    institutional, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To integrate Hong Kong into the People's Republic of China while maintaining a distinct economic and legal system, ensuring national unity and territorial integrity under the ultimate authority of the PRC.
% TRANSFER_FUNCTION: Transfers ultimate political control, legal supremacy, and the power to define 'autonomy' from Hong Kong's local institutions to the PRC central government, in exchange for continued economic integration and a degree of administrative autonomy.
% ABSENT_VOICES: Hong Kong's pre-National Security Law pro-democracy political parties, independent media, and civil society organizations are now largely suppressed or dissolved. They would advocate for genuine self-determination, judicial independence, and the protection of civil liberties, free from mainland interference.
% DISAPPEARANCE_RATIONALE: If this reading of the framework vanished, the PRC's claim to absolute sovereignty over Hong Kong would be fundamentally challenged. This would lead to a rapid reassertion of local autonomy, a resurgence of political dissent, and a significant re-evaluation of Hong Kong's relationship with the mainland, potentially destabilizing the region and altering global perceptions of China's international commitments.
% FOUNDING_PROBLEM: To manage the reintegration of Hong Kong into the PRC after British colonial rule, balancing the desire for national unity with the need to preserve Hong Kong's distinct capitalist system and common law traditions.
% FOUNDING_PROBLEM_CORROBORATION: The PRC central government asserts the founding problem of national unity and territorial integrity remains paramount and is being addressed. International legal scholars, human rights organizations, and many Hong Kong citizens, from outside the benefiting parties, attest that the original problem of reintegration has been superseded by a new problem of political control, and the current framework is primarily a tool for that control, not a balanced solution to the original problem.
narrative_ontology:disappearance_verdict(one_country_two_systems_framework__sovereignty_primacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(one_country_two_systems_framework__sovereignty_primacy_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(one_country_two_systems_framework__sovereignty_primacy_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(one_country_two_systems_framework__sovereignty_primacy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(one_country_two_systems_framework__sovereignty_primacy_reading, 0.85, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(one_country_two_systems_framework__sovereignty_primacy_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(one_country_two_systems_framework__sovereignty_primacy_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(one_country_two_systems_framework__sovereignty_primacy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The high extractiveness (0.85) reflects the significant transfer of political control and legal supremacy from Hong Kong to the PRC. Suppression (0.90) is very high due to the active enforcement of the National Security Law, which criminalizes broad categories of dissent and severely curtails civil liberties, effectively trapping those who would resist. The theater ratio (0.40) indicates that while some elements of Hong Kong's distinct system are maintained for international optics, a substantial portion of the constraint's operation is dedicated to enforcing PRC control rather than preserving genuine autonomy. The increasing trends in extractiveness, suppression, and theater ratio over the interval reflect the progressive tightening of control, particularly after the 2020 National Security Law.
 *
 * PERSPECTIVAL GAP:
 *   From the PRC's perspective, this reading represents a legitimate exercise of sovereign authority necessary for national unity and stability. From the perspective of many Hong Kong citizens and the international community, it represents a fundamental erosion of the 'high degree of autonomy' promised under the original framework, transforming a coordination mechanism into a tool for political extraction and suppression. The engine's classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   The PRC central government is the clear beneficiary and agenda-setter, gaining consolidated control and suppressing challenges to its authority. Hong Kong loyalist elites also benefit from their alignment with Beijing. Hong Kong citizens, pro-democracy activists, and the Hong Kong judiciary are the primary targets, bearing the costs of reduced freedoms, political persecution, and compromised legal independence. The international community acts as an observer, with its directionality reflecting its analytical distance and limited direct impact on the constraint's internal dynamics.
 *
 * MANDATROPHY ANALYSIS:
 *   The original mandate of the 'One Country, Two Systems' framework, which emphasized a 'high degree of autonomy' for Hong Kong, has atrophied under this reading. The current operational mandate, driven by the 'sovereignty primacy' interpretation, is focused on national security and territorial integrity as defined by Beijing, effectively transforming the framework from a mechanism for managing distinct systems into one for asserting central control. This prevents mislabeling the current coercive structure as merely a coordination problem.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sovereignty_vs_autonomy_ambiguity,
    'Is the ''One Country, Two Systems'' framework fundamentally about delegated autonomy that is revocable, or about a constitutionally guaranteed, substantive autonomy?',
    'International legal arbitration or a future political settlement that explicitly redefines the balance of power and the scope of autonomy, with enforcement mechanisms independent of the PRC.',
    'If resolved towards substantive autonomy, the constraint''s extractiveness and suppression would significantly decrease, reclassifying it towards a Rope or Scaffold. If resolved towards absolute, unquestionable sovereignty, the current Snare classification would be further entrenched.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sovereignty_vs_autonomy_ambiguity, conceptual, 'The core conceptual ambiguity regarding the nature of Hong Kong''s autonomy within the framework.').

omega_variable(
    nsl_legitimacy_ambiguity,
    'Is the National Security Law a legitimate exercise of sovereign power to protect national interests, or an overreach that fundamentally violates the spirit and letter of the ''One Country, Two Systems'' framework?',
    'A ruling by an internationally recognized, independent judicial body with jurisdiction over the Basic Law, or a significant shift in PRC policy that repeals or substantially amends the NSL to restore local judicial oversight.',
    'If deemed an overreach, the constraint''s suppression and extractiveness would be seen as illegitimate, reinforcing the Snare classification and potentially triggering international legal consequences. If deemed fully legitimate, the current high suppression would be normalized within the PRC''s legal framework.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(nsl_legitimacy_ambiguity, preference, 'The normative and legal legitimacy of the National Security Law''s application in Hong Kong.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression in Hong Kong primarily structural (legal barriers, enforcement actions) or internalized (self-censorship, fear of reprisal)?',
    'Post-NSL policy shift: if structural barriers are removed but self-censorship and political apathy persist, it indicates a significant internalized component. Longitudinal studies on public opinion and behavior after policy changes.',
    'If internalized suppression is a major factor, the constraint''s effective suppression is higher than the structural measures suggest, as the targets carry the suppression with them even in the absence of overt enforcement, making exit more difficult.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism in Hong Kong.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(one_country_two_systems_framework__sovereignty_primacy_reading, 1997, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(one__tr_t1997, one_country_two_systems_framework__sovereignty_primacy_reading, theater_ratio, 1997, 0.1).
narrative_ontology:measurement(one__tr_t2005, one_country_two_systems_framework__sovereignty_primacy_reading, theater_ratio, 2005, 0.15).
narrative_ontology:measurement(one__tr_t2012, one_country_two_systems_framework__sovereignty_primacy_reading, theater_ratio, 2012, 0.2).
narrative_ontology:measurement(one__tr_t2019, one_country_two_systems_framework__sovereignty_primacy_reading, theater_ratio, 2019, 0.3).
narrative_ontology:measurement(one__tr_t2020, one_country_two_systems_framework__sovereignty_primacy_reading, theater_ratio, 2020, 0.35).
narrative_ontology:measurement(one__tr_t2024, one_country_two_systems_framework__sovereignty_primacy_reading, theater_ratio, 2024, 0.4).

% Extraction over time
narrative_ontology:measurement(one__be_t1997, one_country_two_systems_framework__sovereignty_primacy_reading, base_extractiveness, 1997, 0.45).
narrative_ontology:measurement(one__be_t2005, one_country_two_systems_framework__sovereignty_primacy_reading, base_extractiveness, 2005, 0.55).
narrative_ontology:measurement(one__be_t2012, one_country_two_systems_framework__sovereignty_primacy_reading, base_extractiveness, 2012, 0.65).
narrative_ontology:measurement(one__be_t2019, one_country_two_systems_framework__sovereignty_primacy_reading, base_extractiveness, 2019, 0.75).
narrative_ontology:measurement(one__be_t2020, one_country_two_systems_framework__sovereignty_primacy_reading, base_extractiveness, 2020, 0.8).
narrative_ontology:measurement(one__be_t2024, one_country_two_systems_framework__sovereignty_primacy_reading, base_extractiveness, 2024, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(one__su_t1997, one_country_two_systems_framework__sovereignty_primacy_reading, suppression_requirement, 1997, 0.3).
narrative_ontology:measurement(one__su_t2005, one_country_two_systems_framework__sovereignty_primacy_reading, suppression_requirement, 2005, 0.45).
narrative_ontology:measurement(one__su_t2012, one_country_two_systems_framework__sovereignty_primacy_reading, suppression_requirement, 2012, 0.6).
narrative_ontology:measurement(one__su_t2019, one_country_two_systems_framework__sovereignty_primacy_reading, suppression_requirement, 2019, 0.75).
narrative_ontology:measurement(one__su_t2020, one_country_two_systems_framework__sovereignty_primacy_reading, suppression_requirement, 2020, 0.85).
narrative_ontology:measurement(one__su_t2024, one_country_two_systems_framework__sovereignty_primacy_reading, suppression_requirement, 2024, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(one_country_two_systems_framework__sovereignty_primacy_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(one_country_two_systems_framework__sovereignty_primacy_reading, hong_kong_basic_law_interpretation).
narrative_ontology:affects_constraint(one_country_two_systems_framework__sovereignty_primacy_reading, hong_kong_electoral_system).
narrative_ontology:affects_constraint(one_country_two_systems_framework__sovereignty_primacy_reading, hong_kong_freedom_of_speech).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'one_country_two_systems_framework' kernel. This 'sovereignty_primacy_reading' emphasizes the ultimate authority of the PRC, contrasting with the 'autonomy_primacy_reading' and 'balanced_coexistence_reading'.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
