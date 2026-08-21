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
 *   as delegated by and revocable through PRC sovereign authority. National
 *   security and territorial integrity are paramount, overriding local
 *   autonomy when conflicts arise. This reading has led to the implementation
 *   of the National Security Law, increased mainland enforcement presence,
 *   and a significant curtailment of civil liberties and judicial
 *   independence in Hong Kong. The claimed type is 'snare' because the
 *   coordination story (integration) is cover for substantial extraction (of
 *   autonomy and rights) enforced through coercion.
 *
 * KEY AGENTS:
 *   - prc_central_government: Primary agenda_setter (institutional/arbitrage) — benefits from control
 *   - hong_kong_executive_council: Beneficiary (institutional/constrained) — administers under Beijing's authority
 *   - hong_kong_citizens: Primary payer (powerless/identity_locked) — bears costs of reduced freedoms
 *   - pro_democracy_activists: Payer (moderate/trapped) — directly targeted by enforcement
 *   - hong_kong_judiciary: Payer (institutional/constrained) — loses independence on national security matters
 *   - international_observers: Analytical observer (analytical/analytical) — monitors and critiques
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
narrative_ontology:cs_story_uid(one_country_two_systems_framework__sovereignty_primacy_reading, '82efb320-f408-4a9c-8522-9bd0accaa4b6').
narrative_ontology:cs_kernel_codification('82efb320-f408-4a9c-8522-9bd0accaa4b6', formalized).
narrative_ontology:cs_authority_grounding('82efb320-f408-4a9c-8522-9bd0accaa4b6', extraction).
narrative_ontology:cs_interpretation_layer_present('82efb320-f408-4a9c-8522-9bd0accaa4b6').
narrative_ontology:cs_reading_relation('82efb320-f408-4a9c-8522-9bd0accaa4b6', one_country_two_systems_framework__autonomy_primacy_reading, forecloses).
narrative_ontology:cs_reading_relation('82efb320-f408-4a9c-8522-9bd0accaa4b6', one_country_two_systems_framework__balanced_coexistence_reading, forecloses).
narrative_ontology:cs_axiom('82efb320-f408-4a9c-8522-9bd0accaa4b6', foundational, prc_sovereignty_is_absolute).
narrative_ontology:cs_axiom_status(prc_sovereignty_is_absolute, holdable).
narrative_ontology:cs_axiom_grounding('82efb320-f408-4a9c-8522-9bd0accaa4b6', prc_sovereignty_is_absolute, conventional).
narrative_ontology:cs_axiom('82efb320-f408-4a9c-8522-9bd0accaa4b6', foundational, national_security_overrides_local_autonomy).
narrative_ontology:cs_axiom_status(national_security_overrides_local_autonomy, holdable).
narrative_ontology:cs_axiom_grounding('82efb320-f408-4a9c-8522-9bd0accaa4b6', national_security_overrides_local_autonomy, instrumental).
narrative_ontology:cs_reference_frame('82efb320-f408-4a9c-8522-9bd0accaa4b6', prc_sovereignty_unquestioned).
narrative_ontology:cs_drift_state('82efb320-f408-4a9c-8522-9bd0accaa4b6', post_national_security_law_era, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('82efb320-f408-4a9c-8522-9bd0accaa4b6', '').
narrative_ontology:cs_kernel_id(one_country_two_systems_framework__sovereignty_primacy_reading, one_country_two_systems_framework).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(one_country_two_systems_framework__sovereignty_primacy_reading, prc_central_government).
narrative_ontology:constraint_beneficiary(one_country_two_systems_framework__sovereignty_primacy_reading, hong_kong_executive_council).
narrative_ontology:constraint_victim(one_country_two_systems_framework__sovereignty_primacy_reading, hong_kong_citizens).
narrative_ontology:constraint_victim(one_country_two_systems_framework__sovereignty_primacy_reading, pro_democracy_activists).
narrative_ontology:constraint_victim(one_country_two_systems_framework__sovereignty_primacy_reading, hong_kong_judiciary).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Asserts ultimate sovereign authority over Hong Kong, interpreting 'One Country, Two Systems' as a delegated and revocable autonomy. Benefits from increased control and suppression of perceived threats to national security and territorial integrity.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__sovereignty_primacy_reading, prc_central_government, agenda_setter,
    institutional, civilizational, arbitrage, global).

% Administers Hong Kong under the framework, aligning with the PRC's interpretation. Benefits from maintaining political stability and its own authority, even if it means curtailing local freedoms. Its legitimacy is derived from Beijing.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__sovereignty_primacy_reading, hong_kong_executive_council, beneficiary,
    institutional, biographical, constrained, national).

% Bear the costs of reduced civil liberties, restricted political participation, and a judiciary increasingly influenced by national security interpretations. Many are identity-locked to Hong Kong, making physical or political exit difficult.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__sovereignty_primacy_reading, hong_kong_citizens, payer,
    powerless, biographical, identity_locked, local).

% Directly targeted by national security legislation, facing arrest, detention, and suppression of their activities. Their options are to cease activism, flee, or face severe legal consequences.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__sovereignty_primacy_reading, pro_democracy_activists, payer,
    moderate, immediate, trapped, local).

% Experiences erosion of its independence, particularly in national security cases, where Beijing's interpretations can override local legal principles. Judges face pressure to conform to the sovereignty primacy reading, impacting their professional autonomy and public trust.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__sovereignty_primacy_reading, hong_kong_judiciary, payer,
    institutional, generational, constrained, local).

% Monitor the implementation of 'One Country, Two Systems' and its impact on human rights and rule of law in Hong Kong. They can issue reports, impose sanctions, or offer diplomatic pressure, but lack direct enforcement power.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__sovereignty_primacy_reading, international_observers, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the integration of Hong Kong into the PRC while maintaining a distinct legal and economic system, under the ultimate authority of the central government.
% TRANSFER_FUNCTION: Transfers ultimate legal and political authority from Hong Kong's local institutions to the PRC central government, particularly in matters of national security, resulting in a transfer of civil liberties and judicial independence from Hong Kong citizens to state control.
% ABSENT_VOICES: Hong Kong's pro-democracy political parties and civil society organizations, many of which have been disbanded or suppressed, would advocate for greater autonomy and protection of civil liberties. Their voices are actively silenced or marginalized within the current framework.
% DISAPPEARANCE_RATIONALE: If this interpretation of 'One Country, Two Systems' vanished, Hong Kong's legal and political landscape would immediately revert to a more autonomous state, civil liberties would expand, and the PRC's direct control would diminish. This would necessitate a fundamental re-evaluation of the relationship between Hong Kong and the mainland.
% FOUNDING_PROBLEM: The problem of integrating a capitalist, common-law territory (Hong Kong) into a socialist, civil-law sovereign state (PRC) after the 1997 handover, while preserving Hong Kong's distinctiveness.
% FOUNDING_PROBLEM_CORROBORATION: The PRC Central Government attests the problem is live, citing ongoing threats to national security and territorial integrity. International observers and many Hong Kong citizens attest that while the original problem of integration is real, this specific reading of the framework has exacerbated rather than resolved tensions, shifting the problem from integration to control.
narrative_ontology:disappearance_verdict(one_country_two_systems_framework__sovereignty_primacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(one_country_two_systems_framework__sovereignty_primacy_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(one_country_two_systems_framework__sovereignty_primacy_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
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
 *   Extractiveness is high (0.88) because significant autonomy, civil liberties, and judicial independence are extracted from Hong Kong citizens and institutions. Suppression is very high (0.92) due to the active enforcement of the National Security Law, which criminalizes dissent and suppresses political opposition. Theater ratio is high (0.65) as the 'Two Systems' aspect is increasingly performative, with the 'One Country' principle dominating actual governance. Accessibility collapse is high (0.80) as legal and political avenues for challenging this interpretation have largely been closed off. Resistance is also high (0.75) reflecting ongoing, albeit suppressed, opposition from citizens and international bodies.
 *
 * PERSPECTIVAL GAP:
 *   The PRC Central Government and the Hong Kong Executive Council perceive this framework as legitimate and necessary for national security and stability, viewing any extraction as a justified cost of integration. In contrast, Hong Kong citizens, activists, and the judiciary experience it as a coercive imposition that dismantles their established rights and autonomy. The engine's per-seat classification will reflect this divergence, with beneficiaries experiencing a 'rope' or 'scaffold' and victims experiencing a 'snare'.
 *
 * DIRECTIONALITY LOGIC:
 *   The PRC Central Government is a full beneficiary (d=0.0) as it gains ultimate control and suppresses perceived threats. The Hong Kong Executive Council is also a beneficiary (d=0.15) as it maintains its administrative role under Beijing's protection. Hong Kong citizens and pro-democracy activists are full targets (d=1.0) as they bear the brunt of the extraction and suppression. The Hong Kong Judiciary is a target (d=0.8) as its independence is compromised. International observers are analytical (d=0.5) as they are outside the direct flow of extraction but analyze its effects.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification prevents mislabeling the constraint as a 'rope' or 'scaffold' by highlighting the substantial, actively enforced extraction. While a coordination function (integration) is claimed, the high extractiveness and suppression, coupled with the rising theater ratio, indicate that the primary function has shifted from genuine coordination to coercive control. The 'snare' classification accurately captures the suppression of alternatives and the identifiable victims, distinguishing it from a benign coordination mechanism or a temporary support structure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identification,
    'Is this constraint accurately identified as the ''sovereignty primacy'' reading of the ''One Country, Two Systems'' framework?',
    'Analysis of official PRC statements, legislative actions (e.g., National Security Law), and judicial interpretations in Hong Kong, compared against the core tenets of the ''autonomy primacy'' and ''balanced coexistence'' readings.',
    'If misidentified, the classification of extractiveness and suppression would be inaccurate, potentially understating the coercive elements or overstating the coordination function. Reclassification to a different reading would entail a different set of metrics and stakeholders.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identification, conceptual, 'Ambiguity in identifying the specific reading of the ''One Country, Two Systems'' kernel.').

omega_variable(
    judicial_independence_erosion_rate,
    'What is the precise rate and extent of erosion of judicial independence in Hong Kong, particularly in non-national security cases?',
    'Longitudinal study of judicial appointments, case outcomes, and legal scholar analyses, comparing pre- and post-National Security Law periods across different case types.',
    'If erosion is more widespread than currently understood, the ''hong_kong_judiciary'' stakeholder''s extractiveness would be higher, potentially shifting its computed type further towards ''snare''. If limited to national security cases, the current assessment holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(judicial_independence_erosion_rate, empirical, 'Uncertainty regarding the scope and speed of judicial independence erosion.').

omega_variable(
    international_pressure_efficacy,
    'To what extent can international pressure (sanctions, diplomatic condemnation) alter the PRC''s interpretation and enforcement of this framework?',
    'Empirical analysis of historical instances where international pressure has influenced PRC policy, combined with game-theoretic modeling of state responses to external pressure.',
    'If international pressure proves effective, the ''resistance'' metric might be re-evaluated upwards, and the ''prc_central_government''s exit options might be seen as more constrained, potentially reducing its effective extractiveness. If ineffective, the current high suppression and extractiveness are further entrenched.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(international_pressure_efficacy, empirical, 'Uncertainty about the effectiveness of external resistance.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(one_country_two_systems_framework__sovereignty_primacy_reading, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(one__tr_t0, one_country_two_systems_framework__sovereignty_primacy_reading, theater_ratio, 0, 0.3).
narrative_ontology:measurement(one__tr_t2, one_country_two_systems_framework__sovereignty_primacy_reading, theater_ratio, 2, 0.4).
narrative_ontology:measurement(one__tr_t4, one_country_two_systems_framework__sovereignty_primacy_reading, theater_ratio, 4, 0.5).
narrative_ontology:measurement(one__tr_t6, one_country_two_systems_framework__sovereignty_primacy_reading, theater_ratio, 6, 0.6).
narrative_ontology:measurement(one__tr_t8, one_country_two_systems_framework__sovereignty_primacy_reading, theater_ratio, 8, 0.63).
narrative_ontology:measurement(one__tr_t10, one_country_two_systems_framework__sovereignty_primacy_reading, theater_ratio, 10, 0.65).

% Extraction over time
narrative_ontology:measurement(one__be_t0, one_country_two_systems_framework__sovereignty_primacy_reading, base_extractiveness, 0, 0.65).
narrative_ontology:measurement(one__be_t2, one_country_two_systems_framework__sovereignty_primacy_reading, base_extractiveness, 2, 0.72).
narrative_ontology:measurement(one__be_t4, one_country_two_systems_framework__sovereignty_primacy_reading, base_extractiveness, 4, 0.8).
narrative_ontology:measurement(one__be_t6, one_country_two_systems_framework__sovereignty_primacy_reading, base_extractiveness, 6, 0.85).
narrative_ontology:measurement(one__be_t8, one_country_two_systems_framework__sovereignty_primacy_reading, base_extractiveness, 8, 0.87).
narrative_ontology:measurement(one__be_t10, one_country_two_systems_framework__sovereignty_primacy_reading, base_extractiveness, 10, 0.88).

% Suppression requirement over time
narrative_ontology:measurement(one__su_t0, one_country_two_systems_framework__sovereignty_primacy_reading, suppression_requirement, 0, 0.7).
narrative_ontology:measurement(one__su_t2, one_country_two_systems_framework__sovereignty_primacy_reading, suppression_requirement, 2, 0.78).
narrative_ontology:measurement(one__su_t4, one_country_two_systems_framework__sovereignty_primacy_reading, suppression_requirement, 4, 0.85).
narrative_ontology:measurement(one__su_t6, one_country_two_systems_framework__sovereignty_primacy_reading, suppression_requirement, 6, 0.9).
narrative_ontology:measurement(one__su_t8, one_country_two_systems_framework__sovereignty_primacy_reading, suppression_requirement, 8, 0.91).
narrative_ontology:measurement(one__su_t10, one_country_two_systems_framework__sovereignty_primacy_reading, suppression_requirement, 10, 0.92).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(one_country_two_systems_framework__sovereignty_primacy_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(one_country_two_systems_framework__sovereignty_primacy_reading, hong_kong_basic_law_interpretation).
narrative_ontology:affects_constraint(one_country_two_systems_framework__sovereignty_primacy_reading, hong_kong_electoral_system_reform).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'One Country, Two Systems' framework. It is linked to the 'autonomy_primacy_reading' and 'balanced_coexistence_reading' as sibling interpretations of the same kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
