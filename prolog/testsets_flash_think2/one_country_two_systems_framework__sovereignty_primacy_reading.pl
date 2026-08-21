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
 *   human_readable: One Country, Two Systems: Sovereignty Primacy Reading
 *   domain: constitutional_law/political_systems/state_sovereignty
 *
 * SUMMARY:
 *   This constraint describes the 'sovereignty primacy' reading of the 'One
 *   Country, Two Systems' framework, where Hong Kong's autonomy is understood
 *   as delegated by and revocable through the PRC's sovereign authority.
 *   Under this reading, national security and territorial integrity claims
 *   override local autonomy when they conflict, leading to the imposition of
 *   national security laws, the operation of mainland enforcement agents in
 *   Hong Kong, and a significant curtailment of political speech, assembly,
 *   and judicial independence. This reading emphasizes the 'One Country'
 *   aspect as paramount, with 'Two Systems' being a conditional arrangement.
 *
 * KEY AGENTS:
 *   - prc_central_government: Primary agenda_setter (institutional/arbitrage) — defines and enforces the constraint.
 *   - hk_pro_beijing_establishment: Primary beneficiary (powerful/constrained) — benefits from alignment and stability under PRC authority.
 *   - hong_kong_citizens: Primary payer (powerless/constrained) — bear the costs of reduced autonomy and civil liberties.
 *   - pro_democracy_activists: Primary payer (powerless/trapped) — directly targeted by the constraint's enforcement.
 *   - hong_kong_judiciary: Payer (institutional/identity_locked) — loses independence on national security matters.
 *   - international_community: Observer (institutional/analytical) — monitors and comments but has limited direct influence.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(one_country_two_systems_framework__sovereignty_primacy_reading, 0.85).
domain_priors:suppression_score(one_country_two_systems_framework__sovereignty_primacy_reading, 0.9).
domain_priors:theater_ratio(one_country_two_systems_framework__sovereignty_primacy_reading, 0.6).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(one_country_two_systems_framework__sovereignty_primacy_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(one_country_two_systems_framework__sovereignty_primacy_reading, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(one_country_two_systems_framework__sovereignty_primacy_reading, theater_ratio, 0.6).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(one_country_two_systems_framework__sovereignty_primacy_reading, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(one_country_two_systems_framework__sovereignty_primacy_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(one_country_two_systems_framework__sovereignty_primacy_reading, snare).
narrative_ontology:human_readable(one_country_two_systems_framework__sovereignty_primacy_reading, "One Country, Two Systems: Sovereignty Primacy Reading").
narrative_ontology:topic_domain(one_country_two_systems_framework__sovereignty_primacy_reading, "constitutional_law/political_systems/state_sovereignty").

domain_priors:requires_active_enforcement(one_country_two_systems_framework__sovereignty_primacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(one_country_two_systems_framework__sovereignty_primacy_reading, 'ca139628-c6b1-49fb-9165-9b76a3015dd2').
narrative_ontology:cs_kernel_codification('ca139628-c6b1-49fb-9165-9b76a3015dd2', fixed_text).
narrative_ontology:cs_authority_grounding('ca139628-c6b1-49fb-9165-9b76a3015dd2', extraction).
narrative_ontology:cs_interpretation_layer_present('ca139628-c6b1-49fb-9165-9b76a3015dd2').
narrative_ontology:cs_reading_relation('ca139628-c6b1-49fb-9165-9b76a3015dd2', one_country_two_systems_framework__autonomy_primacy_reading, forecloses).
narrative_ontology:cs_reading_relation('ca139628-c6b1-49fb-9165-9b76a3015dd2', one_country_two_systems_framework__balanced_coexistence_reading, forecloses).
narrative_ontology:cs_axiom('ca139628-c6b1-49fb-9165-9b76a3015dd2', foundational, prc_sovereignty_is_absolute).
narrative_ontology:cs_axiom_status(prc_sovereignty_is_absolute, holdable).
narrative_ontology:cs_axiom_grounding('ca139628-c6b1-49fb-9165-9b76a3015dd2', prc_sovereignty_is_absolute, deontological).
narrative_ontology:cs_axiom('ca139628-c6b1-49fb-9165-9b76a3015dd2', foundational, national_security_overrides_local_law).
narrative_ontology:cs_axiom_status(national_security_overrides_local_law, holdable).
narrative_ontology:cs_axiom_grounding('ca139628-c6b1-49fb-9165-9b76a3015dd2', national_security_overrides_local_law, conventional).
narrative_ontology:cs_reference_frame('ca139628-c6b1-49fb-9165-9b76a3015dd2', prc_unquestionable_sovereignty).
narrative_ontology:cs_drift_state('ca139628-c6b1-49fb-9165-9b76a3015dd2', post_national_security_law_era, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('ca139628-c6b1-49fb-9165-9b76a3015dd2', '').
narrative_ontology:cs_kernel_id(one_country_two_systems_framework__sovereignty_primacy_reading, one_country_two_systems_framework).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(one_country_two_systems_framework__sovereignty_primacy_reading, prc_central_government).
narrative_ontology:constraint_beneficiary(one_country_two_systems_framework__sovereignty_primacy_reading, hk_pro_beijing_establishment).
narrative_ontology:constraint_victim(one_country_two_systems_framework__sovereignty_primacy_reading, hong_kong_citizens).
narrative_ontology:constraint_victim(one_country_two_systems_framework__sovereignty_primacy_reading, pro_democracy_activists).
narrative_ontology:constraint_victim(one_country_two_systems_framework__sovereignty_primacy_reading, hong_kong_judiciary).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Exercises ultimate sovereign authority over Hong Kong, defining national security and territorial integrity, and enforcing their primacy over local autonomy. Benefits from consolidated control and suppression of perceived threats to national unity.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__sovereignty_primacy_reading, prc_central_government, agenda_setter,
    institutional, civilizational, arbitrage, global).

% Benefits from alignment with the PRC's interpretation, gaining political power, stability, and resources. Operates within the framework defined by Beijing, often acting as local enforcers of central directives.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__sovereignty_primacy_reading, hk_pro_beijing_establishment, beneficiary,
    powerful, generational, constrained, national).

% Bear the costs of reduced autonomy, restricted civil liberties (e.g., freedom of speech, assembly), and increased surveillance. Their political participation and rights are curtailed under the national security framework.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__sovereignty_primacy_reading, hong_kong_citizens, payer,
    powerless, biographical, constrained, local).

% Directly targeted by national security laws, facing arrest, prosecution, and severe penalties for activities previously considered legitimate political expression. Their ability to organize and advocate is severely suppressed.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__sovereignty_primacy_reading, pro_democracy_activists, payer,
    powerless, immediate, trapped, local).

% Loses independence on national security matters, compelled to apply laws dictated by Beijing and to accept mainland interpretations. This compromises its traditional common law role and erodes public trust in its impartiality.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__sovereignty_primacy_reading, hong_kong_judiciary, payer,
    institutional, biographical, identity_locked, local).

% Observes the erosion of Hong Kong's autonomy and civil liberties, issuing statements and imposing sanctions, but has limited direct enforcement power to alter the constraint's operation.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__sovereignty_primacy_reading, international_community, observer,
    institutional, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains the administrative and economic distinctiveness of Hong Kong while ensuring its integration into the PRC's national sovereignty framework, particularly regarding national security and territorial integrity.
% TRANSFER_FUNCTION: Transfers ultimate legal and political authority from Hong Kong's local institutions to the PRC central government, especially in national security domains, and extracts compliance from Hong Kong citizens and institutions.
% ABSENT_VOICES: International legal bodies, human rights organizations, and the original drafters of the Basic Law who envisioned greater autonomy are excluded from the current interpretation and enforcement. Their perspectives on treaty obligations and human rights are sidelined.
% DISAPPEARANCE_RATIONALE: If this interpretation of sovereignty primacy vanished overnight, Hong Kong's legal and political landscape would immediately revert to a more autonomous state, and the PRC's control over the territory would be significantly challenged, leading to a major political reorganization and potential international re-engagement with Hong Kong's original status.
% FOUNDING_PROBLEM: To integrate Hong Kong into the PRC after the 1997 handover while preserving its capitalist system and distinct legal framework, under the ultimate authority of the PRC, and to prevent any perceived threats to national unity or security.
% FOUNDING_PROBLEM_CORROBORATION: The PRC central government and its aligned institutions in Hong Kong assert that the framework is necessary to prevent separatism and foreign interference, which they view as ongoing threats. Independent legal scholars and international observers, while acknowledging the original intent, often dispute the current interpretation's necessity for the original problem, citing the erosion of promised autonomy.
narrative_ontology:disappearance_verdict(one_country_two_systems_framework__sovereignty_primacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(one_country_two_systems_framework__sovereignty_primacy_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(one_country_two_systems_framework__sovereignty_primacy_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
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
 *   The high extractiveness (0.85) reflects the significant transfer of authority and rights from Hong Kong's local institutions and citizens to the PRC. Suppression (0.90) is extremely high due to the active enforcement of national security laws, the presence of mainland agents, and the severe penalties for dissent, effectively eliminating alternatives to compliance. The theater ratio (0.60) indicates that a substantial portion of the 'Two Systems' rhetoric and institutional performance serves to legitimize the 'One Country's' overriding authority, with the functional autonomy diminishing. The increasing trend in all metrics over the interval (2019-2024) reflects the hardening of this interpretation following protests and the implementation of the National Security Law.
 *
 * PERSPECTIVAL GAP:
 *   From the PRC's perspective, this reading is a legitimate and necessary assertion of national sovereignty and a restoration of order, ensuring stability and territorial integrity. From the perspective of Hong Kong citizens, pro-democracy activists, and the judiciary, it represents a profound erosion of the autonomy and civil liberties promised under the original framework, transforming a coordination mechanism into an instrument of control.
 *
 * DIRECTIONALITY LOGIC:
 *   The PRC central government is the clear beneficiary, gaining consolidated control and suppressing perceived threats. The HK pro-Beijing establishment also benefits from its alignment. Hong Kong citizens, pro-democracy activists, and the judiciary are the primary targets, bearing the costs of lost freedoms and institutional independence. The international community acts as an observer, with its directionality determined by its analytical distance and lack of direct participation in the constraint's operation.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading actively prevents mandatrophy by reasserting the 'One Country' aspect of the framework. While the original 'Two Systems' mandate might be seen as atrophying from an autonomy-centric view, this reading ensures the framework remains highly functional for the PRC's goals of national security and territorial integrity, even if it means fundamentally altering the balance of the original agreement. The high suppression and extractiveness indicate it is far from an inertial piton; it is a vigorously enforced snare.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sovereignty_interpretation_legitimacy,
    'Is the PRC''s interpretation of ''sovereignty primacy'' a necessary and inherent reading of the Basic Law and Joint Declaration, or an expansive interpretation driven by evolving political objectives?',
    'Comparative legal analysis of international treaty law and constitutional interpretation, alongside historical records of the Basic Law''s drafting and original intent, from independent legal scholars.',
    'If it''s an expansive interpretation, the constraint''s legitimacy as a ''natural'' outcome of the framework is undermined, strengthening arguments for its reclassification as a constructed snare. If it''s a necessary reading, the ''snare'' classification might be seen as an unavoidable consequence of the ''One Country'' principle.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sovereignty_interpretation_legitimacy, conceptual, 'Ambiguity regarding the inherent necessity versus political expansion of sovereignty primacy.').

omega_variable(
    judicial_independence_structural_compromise,
    'To what extent has the Hong Kong judiciary''s independence been structurally compromised by the National Security Law and mainland interpretations, versus merely being compelled to apply new, albeit restrictive, laws?',
    'Empirical analysis of judicial appointments, case outcomes in national security trials, and the extent of deference to mainland legal bodies, conducted by independent legal observers and human rights organizations.',
    'If structural compromise is extensive, the judiciary''s role as a check on executive power is fundamentally broken, reinforcing the constraint''s high suppression. If it''s primarily about applying new laws, the judiciary''s institutional integrity might be less eroded, though its function is still altered.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(judicial_independence_structural_compromise, empirical, 'Distinguishing between application of new laws and fundamental erosion of judicial structure.').

omega_variable(
    economic_cost_of_reduced_autonomy,
    'What is the long-term economic cost to Hong Kong of reduced autonomy, increased political risk, and the erosion of its rule of law, and does this outweigh the perceived stability benefits for the PRC?',
    'Longitudinal economic studies tracking capital flight, foreign investment, and GDP growth in Hong Kong compared to similar financial hubs, alongside surveys of business confidence and talent retention.',
    'If economic costs are severe and persistent, it challenges the PRC''s narrative of stability and prosperity, potentially increasing international pressure and internal resistance. If stability benefits are demonstrably high, it might temper some criticisms.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(economic_cost_of_reduced_autonomy, empirical, 'Assessing the economic trade-offs of sovereignty primacy.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(one_country_two_systems_framework__sovereignty_primacy_reading, 2019, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(one__tr_t2019, one_country_two_systems_framework__sovereignty_primacy_reading, theater_ratio, 2019, 0.4).
narrative_ontology:measurement(one__tr_t2020, one_country_two_systems_framework__sovereignty_primacy_reading, theater_ratio, 2020, 0.48).
narrative_ontology:measurement(one__tr_t2021, one_country_two_systems_framework__sovereignty_primacy_reading, theater_ratio, 2021, 0.55).
narrative_ontology:measurement(one__tr_t2022, one_country_two_systems_framework__sovereignty_primacy_reading, theater_ratio, 2022, 0.58).
narrative_ontology:measurement(one__tr_t2023, one_country_two_systems_framework__sovereignty_primacy_reading, theater_ratio, 2023, 0.6).
narrative_ontology:measurement(one__tr_t2024, one_country_two_systems_framework__sovereignty_primacy_reading, theater_ratio, 2024, 0.6).

% Extraction over time
narrative_ontology:measurement(one__be_t2019, one_country_two_systems_framework__sovereignty_primacy_reading, base_extractiveness, 2019, 0.7).
narrative_ontology:measurement(one__be_t2020, one_country_two_systems_framework__sovereignty_primacy_reading, base_extractiveness, 2020, 0.75).
narrative_ontology:measurement(one__be_t2021, one_country_two_systems_framework__sovereignty_primacy_reading, base_extractiveness, 2021, 0.8).
narrative_ontology:measurement(one__be_t2022, one_country_two_systems_framework__sovereignty_primacy_reading, base_extractiveness, 2022, 0.83).
narrative_ontology:measurement(one__be_t2023, one_country_two_systems_framework__sovereignty_primacy_reading, base_extractiveness, 2023, 0.84).
narrative_ontology:measurement(one__be_t2024, one_country_two_systems_framework__sovereignty_primacy_reading, base_extractiveness, 2024, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(one__su_t2019, one_country_two_systems_framework__sovereignty_primacy_reading, suppression_requirement, 2019, 0.75).
narrative_ontology:measurement(one__su_t2020, one_country_two_systems_framework__sovereignty_primacy_reading, suppression_requirement, 2020, 0.82).
narrative_ontology:measurement(one__su_t2021, one_country_two_systems_framework__sovereignty_primacy_reading, suppression_requirement, 2021, 0.87).
narrative_ontology:measurement(one__su_t2022, one_country_two_systems_framework__sovereignty_primacy_reading, suppression_requirement, 2022, 0.89).
narrative_ontology:measurement(one__su_t2023, one_country_two_systems_framework__sovereignty_primacy_reading, suppression_requirement, 2023, 0.9).
narrative_ontology:measurement(one__su_t2024, one_country_two_systems_framework__sovereignty_primacy_reading, suppression_requirement, 2024, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(one_country_two_systems_framework__sovereignty_primacy_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(one_country_two_systems_framework__sovereignty_primacy_reading, hong_kong_basic_law_interpretation).
narrative_ontology:affects_constraint(one_country_two_systems_framework__sovereignty_primacy_reading, hong_kong_national_security_law).
narrative_ontology:affects_constraint(one_country_two_systems_framework__sovereignty_primacy_reading, hong_kong_electoral_system_reforms).

% DUAL FORMULATION NOTE:
% This constraint is one of three distinct readings of the 'One Country, Two Systems' framework. This 'sovereignty primacy' reading emphasizes the ultimate authority of the PRC, contrasting with the 'autonomy primacy' and 'balanced coexistence' readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
