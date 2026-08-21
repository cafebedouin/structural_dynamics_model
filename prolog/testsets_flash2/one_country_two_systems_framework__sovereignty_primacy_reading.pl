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
 *   This constraint represents the 'sovereignty primacy' reading of the 'One
 *   Country, Two Systems' framework, where Hong Kong's autonomy is understood
 *   as a delegated and revocable privilege from the PRC's central sovereign
 *   authority. National security and territorial integrity are paramount,
 *   overriding local autonomy when conflicts arise. This reading has led to
 *   the imposition of national security laws, the operation of mainland
 *   enforcement agents in Hong Kong, and a significant curtailment of
 *   political speech, assembly, and judicial independence.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(one_country_two_systems_framework__sovereignty_primacy_reading, 0.88).
domain_priors:suppression_score(one_country_two_systems_framework__sovereignty_primacy_reading, 0.92).
domain_priors:theater_ratio(one_country_two_systems_framework__sovereignty_primacy_reading, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(one_country_two_systems_framework__sovereignty_primacy_reading, extractiveness, 0.88).
narrative_ontology:constraint_metric(one_country_two_systems_framework__sovereignty_primacy_reading, suppression_requirement, 0.92).
narrative_ontology:constraint_metric(one_country_two_systems_framework__sovereignty_primacy_reading, theater_ratio, 0.65).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(one_country_two_systems_framework__sovereignty_primacy_reading, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(one_country_two_systems_framework__sovereignty_primacy_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(one_country_two_systems_framework__sovereignty_primacy_reading, snare).
narrative_ontology:human_readable(one_country_two_systems_framework__sovereignty_primacy_reading, "One Country, Two Systems: Sovereignty Primacy Reading").
narrative_ontology:topic_domain(one_country_two_systems_framework__sovereignty_primacy_reading, "constitutional_law/political_systems/state_sovereignty").

domain_priors:requires_active_enforcement(one_country_two_systems_framework__sovereignty_primacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(one_country_two_systems_framework__sovereignty_primacy_reading, 'ab444438-8f6c-472e-8d31-b3f4e04d75b8').
narrative_ontology:cs_kernel_codification('ab444438-8f6c-472e-8d31-b3f4e04d75b8', fixed_text).
narrative_ontology:cs_authority_grounding('ab444438-8f6c-472e-8d31-b3f4e04d75b8', extraction).
narrative_ontology:cs_interpretation_layer_present('ab444438-8f6c-472e-8d31-b3f4e04d75b8').
narrative_ontology:cs_reading_relation('ab444438-8f6c-472e-8d31-b3f4e04d75b8', one_country_two_systems_framework__autonomy_primacy_reading, forecloses).
narrative_ontology:cs_reading_relation('ab444438-8f6c-472e-8d31-b3f4e04d75b8', one_country_two_systems_framework__balanced_coexistence_reading, influences).
narrative_ontology:cs_axiom('ab444438-8f6c-472e-8d31-b3f4e04d75b8', foundational, prc_sovereignty_is_absolute_and_indivisible).
narrative_ontology:cs_axiom_status(prc_sovereignty_is_absolute_and_indivisible, holdable).
narrative_ontology:cs_axiom_grounding('ab444438-8f6c-472e-8d31-b3f4e04d75b8', prc_sovereignty_is_absolute_and_indivisible, deontological).
narrative_ontology:cs_axiom('ab444438-8f6c-472e-8d31-b3f4e04d75b8', foundational, national_security_is_a_central_prc_prerogative).
narrative_ontology:cs_axiom_status(national_security_is_a_central_prc_prerogative, holdable).
narrative_ontology:cs_axiom_grounding('ab444438-8f6c-472e-8d31-b3f4e04d75b8', national_security_is_a_central_prc_prerogative, conventional).
narrative_ontology:cs_reference_frame('ab444438-8f6c-472e-8d31-b3f4e04d75b8', prc_unilateral_sovereignty_framework).
narrative_ontology:cs_drift_state('ab444438-8f6c-472e-8d31-b3f4e04d75b8', contemporary_post_ns_law_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('ab444438-8f6c-472e-8d31-b3f4e04d75b8', '').
narrative_ontology:cs_kernel_id(one_country_two_systems_framework__sovereignty_primacy_reading, one_country_two_systems_framework).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(one_country_two_systems_framework__sovereignty_primacy_reading, prc_central_government).
narrative_ontology:constraint_beneficiary(one_country_two_systems_framework__sovereignty_primacy_reading, hong_kong_pro_beijing_establishment).
narrative_ontology:constraint_victim(one_country_two_systems_framework__sovereignty_primacy_reading, hong_kong_pro_democracy_activists).
narrative_ontology:constraint_victim(one_country_two_systems_framework__sovereignty_primacy_reading, hong_kong_judiciary).
narrative_ontology:constraint_victim(one_country_two_systems_framework__sovereignty_primacy_reading, hong_kong_citizens).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Asserts ultimate sovereign authority over Hong Kong, viewing local autonomy as a delegated privilege. Actively enforces national security laws, deploying mainland agents and overriding local legal processes when deemed necessary. Benefits from consolidated control and suppression of dissent.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__sovereignty_primacy_reading, prc_central_government, agenda_setter,
    institutional, generational, arbitrage, global).

% Aligns with the PRC's interpretation, benefiting from political stability and access to power within the new framework. Supports the national security laws and their enforcement, often acting as local implementers or advocates.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__sovereignty_primacy_reading, hong_kong_pro_beijing_establishment, beneficiary,
    organized, biographical, mobile, local).

% Directly targeted by national security laws, facing arrest, prosecution, and severe restrictions on political speech and assembly. Their ability to organize or express dissent is severely curtailed, with high personal costs for non-compliance.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__sovereignty_primacy_reading, hong_kong_pro_democracy_activists, payer,
    powerless, immediate, trapped, local).

% Experiences a significant erosion of its independence, particularly in national security cases where Beijing's interpretations can override local legal principles. Judges face pressure to conform, and their rulings are subject to review by mainland authorities.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__sovereignty_primacy_reading, hong_kong_judiciary, payer,
    institutional, biographical, constrained, local).

% Live under increased surveillance and self-censorship, with reduced civil liberties and political freedoms. While some may prioritize stability, many feel a loss of their distinct identity and autonomy, with limited avenues for protest or political participation.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__sovereignty_primacy_reading, hong_kong_citizens, payer,
    moderate, biographical, identity_locked, local).

% Monitor the erosion of Hong Kong's autonomy and civil liberties, issuing reports and statements. Their influence is primarily diplomatic and reputational, with limited direct power to alter the constraint's operation.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__sovereignty_primacy_reading, international_observers, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Integrates Hong Kong into the PRC's national security and territorial integrity framework, ensuring unified state control over all sovereign territory and preventing perceived external interference.
% TRANSFER_FUNCTION: Transfers ultimate legal and political authority from Hong Kong's local institutions to the PRC central government, reallocating power, control over information, and the right to define 'national security'.
% ABSENT_VOICES: Pro-democracy advocates and international legal bodies, whose interpretations of the Basic Law and international treaties emphasize Hong Kong's autonomy and human rights, are systematically excluded from the decision-making process.
% DISAPPEARANCE_RATIONALE: If this reading of 'One Country, Two Systems' vanished, Hong Kong's legal and political landscape would immediately revert to a state of greater autonomy and judicial independence. Mainland enforcement agents would withdraw, national security laws would be challenged, and civil liberties would expand, leading to a significant reorganization of power dynamics and a resurgence of local political activity.
% FOUNDING_PROBLEM: The PRC perceived a threat to national sovereignty and territorial integrity from perceived foreign interference and local secessionist movements in Hong Kong, particularly after large-scale protests.
% FOUNDING_PROBLEM_CORROBORATION: The PRC central government consistently asserts the problem is live, citing ongoing threats. International human rights organizations and many Hong Kong citizens, however, argue that the 'threat' is largely a pretext for suppressing dissent and consolidating power, with little independent corroboration for the severity of the original problem as a justification for the current level of coercion.
narrative_ontology:disappearance_verdict(one_country_two_systems_framework__sovereignty_primacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(one_country_two_systems_framework__sovereignty_primacy_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(one_country_two_systems_framework__sovereignty_primacy_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(one_country_two_systems_framework__sovereignty_primacy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(one_country_two_systems_framework__sovereignty_primacy_reading, 0.88, 'gemini-2.5-flash', 'none', direct).

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
 *   The extractiveness (0.88) is very high due to the systematic transfer of power and rights from Hong Kong's local institutions and citizens to the PRC. Suppression (0.92) is also very high, reflecting the active enforcement of national security laws, the suppression of dissent, and the lack of effective legal or political recourse for those targeted. The theater ratio (0.65) indicates that while some elements of 'Two Systems' are maintained performatively (e.g., separate currency, customs), the core promise of high autonomy is largely theatrical, with real power residing in Beijing. The claimed type is 'snare' because the coordination story (stability, national security) serves as cover for substantial, actively enforced extraction from identifiable victims.
 *
 * PERSPECTIVAL GAP:
 *   From the PRC Central Government's perspective, this framework is a necessary 'rope' for national unity and security. However, from the perspective of Hong Kong citizens and the judiciary, it operates as a 'snare' that extracts fundamental rights and autonomy through coercion. The engine's classification will reflect this divergence based on the structural positions of the stakeholders.
 *
 * DIRECTIONALITY LOGIC:
 *   The PRC Central Government is the primary beneficiary (d=0.0-0.1), gaining consolidated control and suppressing perceived threats. The Hong Kong pro-Beijing establishment also benefits (d=0.1-0.2) from their alignment with Beijing. Hong Kong pro-democracy activists, the judiciary, and citizens are the primary targets (d=0.8-1.0), bearing the costs of lost freedoms and judicial independence. International observers are analytical (d=0.5), observing the dynamics without direct participation in the extraction or benefit.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification prevents mislabeling the current arrangement as a 'rope' or 'tangled_rope' by highlighting the extreme asymmetry of benefits and costs, the high level of active suppression, and the significant theatricality involved in maintaining the 'Two Systems' facade. The 'snare' classification accurately captures the coercive, extractive nature of this reading, where the original coordination problem (national security) is used as a justification for broad power consolidation and suppression of dissent, rather than a genuine collective-action solution.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    genuine_security_threat_vs_pretext,
    'To what extent did genuine national security threats (e.g., foreign interference, secessionist movements) necessitate the current level of coercion, versus serving as a pretext for consolidating political control?',
    'Independent, verifiable intelligence assessments and historical analysis of the severity and nature of threats prior to and after the imposition of national security laws, compared against the scope of the laws'' application.',
    'If threats were minimal or exaggerated, the ''snare'' classification is strongly reinforced, indicating the security narrative is primarily cover. If threats were substantial and directly addressed, it might suggest a ''tangled_rope'' with a more genuine, albeit asymmetric, coordination function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(genuine_security_threat_vs_pretext, empirical, 'Assessing the factual basis for the national security justification.').

omega_variable(
    judicial_independence_erosion_irreversibility,
    'Is the erosion of judicial independence in Hong Kong on national security matters a temporary measure or an irreversible structural change?',
    'Observation of future judicial appointments, rulings in non-national security cases, and any legislative or constitutional reforms that might restore or further diminish judicial autonomy over a multi-year period.',
    'If irreversible, the ''snare'' classification is solidified, as a core institutional check on extraction is permanently removed. If temporary, it might suggest a ''scaffold'' or a more dynamic ''tangled_rope'' where institutional roles are still contested.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(judicial_independence_erosion_irreversibility, empirical, 'Determining the long-term structural impact on Hong Kong''s judiciary.').

omega_variable(
    autonomy_vs_sovereignty_framing,
    'Is ''One Country, Two Systems'' fundamentally a framework for delegated autonomy or an assertion of indivisible sovereignty?',
    'Analysis of the original Sino-British Joint Declaration and Basic Law through different legal and political philosophy lenses, examining the intent and textual commitments regarding the balance of power.',
    'If primarily delegated autonomy, this ''sovereignty primacy'' reading is a significant deviation, reinforcing its extractive nature. If primarily indivisible sovereignty, this reading is a consistent, albeit coercive, interpretation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(autonomy_vs_sovereignty_framing, conceptual, 'Conceptual framing of the ''One Country, Two Systems'' principle.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(one_country_two_systems_framework__sovereignty_primacy_reading, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(one__tr_t0, one_country_two_systems_framework__sovereignty_primacy_reading, theater_ratio, 0, 0.4).
narrative_ontology:measurement(one__tr_t2, one_country_two_systems_framework__sovereignty_primacy_reading, theater_ratio, 2, 0.48).
narrative_ontology:measurement(one__tr_t4, one_country_two_systems_framework__sovereignty_primacy_reading, theater_ratio, 4, 0.55).
narrative_ontology:measurement(one__tr_t6, one_country_two_systems_framework__sovereignty_primacy_reading, theater_ratio, 6, 0.6).
narrative_ontology:measurement(one__tr_t8, one_country_two_systems_framework__sovereignty_primacy_reading, theater_ratio, 8, 0.63).
narrative_ontology:measurement(one__tr_t10, one_country_two_systems_framework__sovereignty_primacy_reading, theater_ratio, 10, 0.65).

% Extraction over time
narrative_ontology:measurement(one__be_t0, one_country_two_systems_framework__sovereignty_primacy_reading, base_extractiveness, 0, 0.7).
narrative_ontology:measurement(one__be_t2, one_country_two_systems_framework__sovereignty_primacy_reading, base_extractiveness, 2, 0.78).
narrative_ontology:measurement(one__be_t4, one_country_two_systems_framework__sovereignty_primacy_reading, base_extractiveness, 4, 0.83).
narrative_ontology:measurement(one__be_t6, one_country_two_systems_framework__sovereignty_primacy_reading, base_extractiveness, 6, 0.86).
narrative_ontology:measurement(one__be_t8, one_country_two_systems_framework__sovereignty_primacy_reading, base_extractiveness, 8, 0.87).
narrative_ontology:measurement(one__be_t10, one_country_two_systems_framework__sovereignty_primacy_reading, base_extractiveness, 10, 0.88).

% Suppression requirement over time
narrative_ontology:measurement(one__su_t0, one_country_two_systems_framework__sovereignty_primacy_reading, suppression_requirement, 0, 0.75).
narrative_ontology:measurement(one__su_t2, one_country_two_systems_framework__sovereignty_primacy_reading, suppression_requirement, 2, 0.82).
narrative_ontology:measurement(one__su_t4, one_country_two_systems_framework__sovereignty_primacy_reading, suppression_requirement, 4, 0.87).
narrative_ontology:measurement(one__su_t6, one_country_two_systems_framework__sovereignty_primacy_reading, suppression_requirement, 6, 0.9).
narrative_ontology:measurement(one__su_t8, one_country_two_systems_framework__sovereignty_primacy_reading, suppression_requirement, 8, 0.91).
narrative_ontology:measurement(one__su_t10, one_country_two_systems_framework__sovereignty_primacy_reading, suppression_requirement, 10, 0.92).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(one_country_two_systems_framework__sovereignty_primacy_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(one_country_two_systems_framework__sovereignty_primacy_reading, hong_kong_basic_law_interpretation).
narrative_ontology:affects_constraint(one_country_two_systems_framework__sovereignty_primacy_reading, hong_kong_electoral_system_reform).
narrative_ontology:affects_constraint(one_country_two_systems_framework__sovereignty_primacy_reading, hong_kong_press_freedom).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'One Country, Two Systems' framework. Sibling readings include 'autonomy_primacy_reading' and 'balanced_coexistence_reading', which offer different interpretations of the balance between Hong Kong's autonomy and PRC sovereignty. This reading emphasizes the latter.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
