% ============================================================================
% CONSTRAINT STORY: territorial_legitimacy__security_necessity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_territorial_legitimacy__security_necessity_reading, []).

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
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
 *   constraint_id: territorial_legitimacy__security_necessity_reading
 *   human_readable: Israeli Territorial Control via Security Necessity Doctrine
 *   domain: political_theory/international_law/territorial_sovereignty
 *
 * SUMMARY:
 *   This constraint story instantiates the 'security necessity' reading of
 *   the broader 'territorial legitimacy' kernel. It describes the claim that
 *   Israeli control over territories captured in 1967 (West Bank, Golan
 *   Heights) is legitimate due to ongoing security threats and the need for
 *   strategic depth. This reading conditions Palestinian sovereignty on
 *   demilitarization and views Israeli settlements as a legitimate security
 *   presence. While framed as a coordination mechanism for Israeli security,
 *   its operation involves substantial extraction from the Palestinian
 *   population and requires active enforcement.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(territorial_legitimacy__security_necessity_reading, 0.78).
domain_priors:suppression_score(territorial_legitimacy__security_necessity_reading, 0.85).
domain_priors:theater_ratio(territorial_legitimacy__security_necessity_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(territorial_legitimacy__security_necessity_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(territorial_legitimacy__security_necessity_reading, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(territorial_legitimacy__security_necessity_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(territorial_legitimacy__security_necessity_reading, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(territorial_legitimacy__security_necessity_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(territorial_legitimacy__security_necessity_reading, tangled_rope).
narrative_ontology:human_readable(territorial_legitimacy__security_necessity_reading, "Israeli Territorial Control via Security Necessity Doctrine").
narrative_ontology:topic_domain(territorial_legitimacy__security_necessity_reading, "political_theory/international_law/territorial_sovereignty").

domain_priors:requires_active_enforcement(territorial_legitimacy__security_necessity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(territorial_legitimacy__security_necessity_reading, '17e7465f-415b-4dd2-9d2a-884525916276').
narrative_ontology:cs_kernel_codification('17e7465f-415b-4dd2-9d2a-884525916276', formalized).
narrative_ontology:cs_authority_grounding('17e7465f-415b-4dd2-9d2a-884525916276', lineage).
narrative_ontology:cs_interpretation_layer_present('17e7465f-415b-4dd2-9d2a-884525916276').
narrative_ontology:cs_reading_relation('17e7465f-415b-4dd2-9d2a-884525916276', territorial_legitimacy__indigenous_continuity_reading, forecloses).
narrative_ontology:cs_reading_relation('17e7465f-415b-4dd2-9d2a-884525916276', territorial_legitimacy__partition_reading, influences).
narrative_ontology:cs_axiom('17e7465f-415b-4dd2-9d2a-884525916276', foundational, territorial_depth_is_security_necessity).
narrative_ontology:cs_axiom_status(territorial_depth_is_security_necessity, holdable).
narrative_ontology:cs_axiom_grounding('17e7465f-415b-4dd2-9d2a-884525916276', territorial_depth_is_security_necessity, empirically_contingent).
narrative_ontology:cs_axiom('17e7465f-415b-4dd2-9d2a-884525916276', foundational, sovereignty_is_conditional_on_demilitarization).
narrative_ontology:cs_axiom_status(sovereignty_is_conditional_on_demilitarization, holdable).
narrative_ontology:cs_axiom_grounding('17e7465f-415b-4dd2-9d2a-884525916276', sovereignty_is_conditional_on_demilitarization, conventional).
narrative_ontology:cs_reference_frame('17e7465f-415b-4dd2-9d2a-884525916276', post_1967_security_doctrine).
narrative_ontology:cs_drift_state('17e7465f-415b-4dd2-9d2a-884525916276', contemporary_regional_instability_and_settlement_expansion, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('17e7465f-415b-4dd2-9d2a-884525916276', '').
narrative_ontology:cs_kernel_id(territorial_legitimacy__security_necessity_reading, territorial_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(territorial_legitimacy__security_necessity_reading, state_of_israel).
narrative_ontology:constraint_beneficiary(territorial_legitimacy__security_necessity_reading, israeli_settlers).
narrative_ontology:constraint_victim(territorial_legitimacy__security_necessity_reading, palestinian_population).
narrative_ontology:constraint_victim(territorial_legitimacy__security_necessity_reading, palestinian_authority).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Defines and enforces its security needs, maintaining military and administrative control over territories beyond its 1967 borders, justifying this as essential for strategic depth and defense against threats. Benefits from perceived security and territorial control.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__security_necessity_reading, state_of_israel, agenda_setter,
    institutional, generational, constrained, regional).

% Reside in settlements in the West Bank and Golan Heights, benefiting from the security presence and infrastructure provided by the state. Their presence is often justified as part of the security necessity doctrine, though it also serves ideological and demographic goals.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__security_necessity_reading, israeli_settlers, beneficiary,
    powerful, generational, constrained, local).

% Live under military occupation in the West Bank and Gaza, experiencing restrictions on movement, land confiscation, and limited self-governance. They bear the direct costs of the security necessity doctrine through loss of sovereignty and daily life constraints.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__security_necessity_reading, palestinian_population, payer,
    powerless, immediate, trapped, local).

% Exercises limited administrative control in parts of the West Bank, but its sovereignty is heavily conditional on Israeli security approvals and demilitarization. It is a payer in terms of foregone full statehood and constrained governance capacity.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__security_necessity_reading, palestinian_authority, payer,
    moderate, biographical, constrained, regional).

% Monitors the conflict, provides humanitarian aid, and engages in diplomatic efforts. While often critical of the occupation, it frequently acknowledges Israel's security concerns, sometimes implicitly legitimizing aspects of the security necessity doctrine.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__security_necessity_reading, international_community, observer,
    institutional, generational, analytical, global).

% Analyze the legality of occupation, self-defense, and territorial acquisition under international law. They often critique the security necessity doctrine's application, but their analysis is primarily academic and does not directly alter the constraint's operation.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__security_necessity_reading, international_law_scholars, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(territorial_legitimacy__security_necessity_reading, state_of_israel).
narrative_ontology:fixing_cost_class(territorial_legitimacy__security_necessity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a framework for Israel to maintain its security by controlling strategic territories and demilitarizing potential threats, thereby coordinating its defense posture against perceived existential risks.
% TRANSFER_FUNCTION: Transfers territorial control, security guarantees, and resource access to Israel, while imposing restrictions on Palestinian sovereignty, movement, and development.
% ABSENT_VOICES: Palestinian voices advocating for unconditional self-determination and full sovereignty over all 1967 territories are structurally marginalized within this framework, as their claims directly challenge the premise of security necessity.
% DISAPPEARANCE_RATIONALE: If the security necessity doctrine vanished overnight, the legal and political basis for Israeli control over the West Bank and Golan Heights would collapse. This would lead to a fundamental reordering of territorial claims, security arrangements, and international relations in the region, likely resulting in immediate demands for full Palestinian sovereignty and withdrawal of Israeli forces and settlers.
% FOUNDING_PROBLEM: The existential security threat to Israel from hostile neighbors following its establishment in 1948 and the subsequent 1967 Six-Day War, which led to the capture of territories deemed vital for strategic depth.
% FOUNDING_PROBLEM_CORROBORATION: The State of Israel and its military consistently attest to ongoing security threats from regional actors and non-state groups. Some international security analysts and allied states corroborate the existence of genuine security concerns, though often disputing the proportionality or necessity of specific territorial controls and settlement expansion.
narrative_ontology:disappearance_verdict(territorial_legitimacy__security_necessity_reading, world_rearranges).
narrative_ontology:founding_problem_status(territorial_legitimacy__security_necessity_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(territorial_legitimacy__security_necessity_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(territorial_legitimacy__security_necessity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(territorial_legitimacy__security_necessity_reading, 0.78, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(territorial_legitimacy__security_necessity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(territorial_legitimacy__security_necessity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(territorial_legitimacy__security_necessity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The high extractiveness (0.78) reflects the significant costs imposed on Palestinians (land loss, movement restrictions, limited self-determination) for the benefit of Israeli security and territorial control. Suppression (0.85) is very high due to the active military occupation, administrative controls, and legal frameworks that prevent alternatives to Israeli control. The theater ratio (0.45) indicates that while genuine security concerns exist, a substantial portion of the enforcement and territorial expansion also serves other objectives (e.g., ideological, demographic) under the guise of security. Accessibility collapse is high (0.80) because this framework severely limits the viability of full Palestinian sovereignty, and resistance is high (0.70) due to ongoing Palestinian opposition.
 *
 * PERSPECTIVAL GAP:
 *   From the Israeli perspective, this doctrine is a necessary, albeit costly, measure for national survival. From the Palestinian perspective, it is a mechanism of occupation and dispossession. The engine's classification will highlight this divergence by computing a 'tangled_rope' type from the structural data, despite the 'security necessity' framing.
 *
 * DIRECTIONALITY LOGIC:
 *   The State of Israel and Israeli settlers are the primary beneficiaries, gaining security, territorial access, and resources. The Palestinian population and Palestinian Authority are the primary targets, bearing the costs of occupation and constrained sovereignty. The international community and legal scholars act as observers, often acknowledging security concerns while critiquing the associated extraction.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    security_vs_annexation_ambiguity,
    'Is the extent of Israeli territorial control genuinely driven by security necessity, or does it serve as a de facto annexation under the guise of security?',
    'Independent military and geopolitical analysis assessing the actual strategic value of specific territorial holdings versus alternative security arrangements (e.g., demilitarized zones, international guarantees).',
    'If primarily annexation, the constraint''s extractiveness and suppression would be re-evaluated as pure extraction, potentially reclassifying it closer to a ''snare''. If genuinely security-driven, the ''tangled_rope'' classification holds, acknowledging a coordination function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(security_vs_annexation_ambiguity, empirical, 'Distinguishing genuine security needs from territorial expansion.').

omega_variable(
    proportionality_of_control,
    'Is the level of control and restriction imposed on the Palestinian population proportional to the actual security threat, or does it exceed what is strictly necessary?',
    'Detailed human rights investigations, international legal reviews, and comparative analysis of security measures in other conflict zones, assessing the least restrictive means to achieve security objectives.',
    'If disproportionate, the ''suppression'' metric would be seen as inflated beyond genuine security needs, indicating a higher degree of coercive extraction. This would strengthen the ''snare'' aspects of the ''tangled_rope'' classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(proportionality_of_control, empirical, 'Assessing the proportionality of security measures.').

omega_variable(
    internalized_security_narrative,
    'To what extent is the Israeli public''s support for the security necessity doctrine driven by an internalized narrative of existential threat, making alternatives (e.g., full withdrawal) unthinkable?',
    'Sociological studies, public opinion surveys, and historical analysis of national narratives to gauge the depth and resilience of the security-first paradigm within Israeli society.',
    'If deeply internalized, the ''identity_locked'' exit option for Israeli citizens (especially settlers) would be more pronounced, making political shifts away from this doctrine more difficult, even if empirical security conditions change. This would highlight the ''identity_coordination'' aspects of the constraint''s persistence.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(internalized_security_narrative, conceptual, 'Role of internalized narratives in perpetuating the security doctrine.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(territorial_legitimacy__security_necessity_reading, 1967, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(terr_tr_t1967, territorial_legitimacy__security_necessity_reading, theater_ratio, 1967, 0.2).
narrative_ontology:measurement(terr_tr_t1977, territorial_legitimacy__security_necessity_reading, theater_ratio, 1977, 0.25).
narrative_ontology:measurement(terr_tr_t1987, territorial_legitimacy__security_necessity_reading, theater_ratio, 1987, 0.3).
narrative_ontology:measurement(terr_tr_t1997, territorial_legitimacy__security_necessity_reading, theater_ratio, 1997, 0.35).
narrative_ontology:measurement(terr_tr_t2007, territorial_legitimacy__security_necessity_reading, theater_ratio, 2007, 0.4).
narrative_ontology:measurement(terr_tr_t2017, territorial_legitimacy__security_necessity_reading, theater_ratio, 2017, 0.43).
narrative_ontology:measurement(terr_tr_t2024, territorial_legitimacy__security_necessity_reading, theater_ratio, 2024, 0.45).

% Extraction over time
narrative_ontology:measurement(terr_be_t1967, territorial_legitimacy__security_necessity_reading, base_extractiveness, 1967, 0.6).
narrative_ontology:measurement(terr_be_t1977, territorial_legitimacy__security_necessity_reading, base_extractiveness, 1977, 0.65).
narrative_ontology:measurement(terr_be_t1987, territorial_legitimacy__security_necessity_reading, base_extractiveness, 1987, 0.7).
narrative_ontology:measurement(terr_be_t1997, territorial_legitimacy__security_necessity_reading, base_extractiveness, 1997, 0.73).
narrative_ontology:measurement(terr_be_t2007, territorial_legitimacy__security_necessity_reading, base_extractiveness, 2007, 0.75).
narrative_ontology:measurement(terr_be_t2017, territorial_legitimacy__security_necessity_reading, base_extractiveness, 2017, 0.77).
narrative_ontology:measurement(terr_be_t2024, territorial_legitimacy__security_necessity_reading, base_extractiveness, 2024, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(terr_su_t1967, territorial_legitimacy__security_necessity_reading, suppression_requirement, 1967, 0.7).
narrative_ontology:measurement(terr_su_t1977, territorial_legitimacy__security_necessity_reading, suppression_requirement, 1977, 0.75).
narrative_ontology:measurement(terr_su_t1987, territorial_legitimacy__security_necessity_reading, suppression_requirement, 1987, 0.8).
narrative_ontology:measurement(terr_su_t1997, territorial_legitimacy__security_necessity_reading, suppression_requirement, 1997, 0.82).
narrative_ontology:measurement(terr_su_t2007, territorial_legitimacy__security_necessity_reading, suppression_requirement, 2007, 0.83).
narrative_ontology:measurement(terr_su_t2017, territorial_legitimacy__security_necessity_reading, suppression_requirement, 2017, 0.84).
narrative_ontology:measurement(terr_su_t2024, territorial_legitimacy__security_necessity_reading, suppression_requirement, 2024, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(territorial_legitimacy__security_necessity_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(territorial_legitimacy__security_necessity_reading, territorial_legitimacy__partition_reading).
narrative_ontology:affects_constraint(territorial_legitimacy__security_necessity_reading, territorial_legitimacy__indigenous_continuity_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'territorial_legitimacy' kernel, each offering a distinct basis for claims to the same territory. This reading emphasizes security necessity and strategic depth, influencing and foreclosing other claims.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
