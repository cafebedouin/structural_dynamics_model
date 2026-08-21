% ============================================================================
% CONSTRAINT STORY: npt_article_iv_vi_pairing__grand_bargain
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_npt_article_iv_vi_pairing__grand_bargain, []).

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
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: npt_article_iv_vi_pairing__grand_bargain
 *   human_readable: NPT Article IV/VI Pairing (Grand Bargain Reading)
 *   domain: international_law/nuclear_governance/treaty_interpretation
 *
 * SUMMARY:
 *   This constraint represents the 'grand bargain' reading of the Nuclear
 *   Non-Proliferation Treaty (NPT), where non-nuclear weapon states (NNWS)
 *   commit to non-proliferation (Article IV) conditional on nuclear weapon
 *   states (NWS) pursuing disarmament (Article VI). The constraint's
 *   legitimacy and persistence depend on this reciprocity. Perceived NWS
 *   failure to disarm leads to rising extractiveness and resistance from
 *   NNWS, challenging the foundational premise of the treaty. This is one
 *   reading of the 'npt_article_iv_vi_pairing' kernel.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(npt_article_iv_vi_pairing__grand_bargain, 0.65).
domain_priors:suppression_score(npt_article_iv_vi_pairing__grand_bargain, 0.7).
domain_priors:theater_ratio(npt_article_iv_vi_pairing__grand_bargain, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(npt_article_iv_vi_pairing__grand_bargain, extractiveness, 0.65).
narrative_ontology:constraint_metric(npt_article_iv_vi_pairing__grand_bargain, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(npt_article_iv_vi_pairing__grand_bargain, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(npt_article_iv_vi_pairing__grand_bargain, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(npt_article_iv_vi_pairing__grand_bargain, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(npt_article_iv_vi_pairing__grand_bargain, tangled_rope).
narrative_ontology:human_readable(npt_article_iv_vi_pairing__grand_bargain, "NPT Article IV/VI Pairing (Grand Bargain Reading)").
narrative_ontology:topic_domain(npt_article_iv_vi_pairing__grand_bargain, "international_law/nuclear_governance/treaty_interpretation").

domain_priors:requires_active_enforcement(npt_article_iv_vi_pairing__grand_bargain).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(npt_article_iv_vi_pairing__grand_bargain, '7a67123f-74bc-4f84-82e9-b47b206183fb').
narrative_ontology:cs_kernel_codification('7a67123f-74bc-4f84-82e9-b47b206183fb', fixed_text).
narrative_ontology:cs_authority_grounding('7a67123f-74bc-4f84-82e9-b47b206183fb', lineage).
narrative_ontology:cs_interpretation_layer_present('7a67123f-74bc-4f84-82e9-b47b206183fb').
narrative_ontology:cs_reading_relation('7a67123f-74bc-4f84-82e9-b47b206183fb', npt_article_iv_vi_pairing__nonproliferation_primary, coexists_with).
narrative_ontology:cs_reading_relation('7a67123f-74bc-4f84-82e9-b47b206183fb', npt_article_iv_vi_pairing__abolitionist, coexists_with).
narrative_ontology:cs_axiom('7a67123f-74bc-4f84-82e9-b47b206183fb', foundational, reciprocal_obligations_npt).
narrative_ontology:cs_axiom_status(reciprocal_obligations_npt, holdable).
narrative_ontology:cs_axiom_grounding('7a67123f-74bc-4f84-82e9-b47b206183fb', reciprocal_obligations_npt, conventional).
narrative_ontology:cs_axiom('7a67123f-74bc-4f84-82e9-b47b206183fb', foundational, disarmament_progress_conditions_nonproliferation).
narrative_ontology:cs_axiom_status(disarmament_progress_conditions_nonproliferation, holdable).
narrative_ontology:cs_axiom_grounding('7a67123f-74bc-4f84-82e9-b47b206183fb', disarmament_progress_conditions_nonproliferation, instrumental).
narrative_ontology:cs_reference_frame('7a67123f-74bc-4f84-82e9-b47b206183fb', original_npt_grand_bargain).
narrative_ontology:cs_drift_state('7a67123f-74bc-4f84-82e9-b47b206183fb', contemporary_npt_review_cycle, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('7a67123f-74bc-4f84-82e9-b47b206183fb', '').
narrative_ontology:cs_kernel_id(npt_article_iv_vi_pairing__grand_bargain, npt_article_iv_vi_pairing).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(npt_article_iv_vi_pairing__grand_bargain, non_nuclear_weapon_states_grand_bargain).
narrative_ontology:constraint_beneficiary(npt_article_iv_vi_pairing__grand_bargain, global_security_regime).
narrative_ontology:constraint_victim(npt_article_iv_vi_pairing__grand_bargain, nuclear_weapon_states).
narrative_ontology:constraint_victim(npt_article_iv_vi_pairing__grand_bargain, non_nuclear_weapon_states_disarmament_advocates).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These states adhere to non-proliferation (Article IV) on the understanding that nuclear weapon states (NWS) will pursue disarmament (Article VI). They benefit from the perceived security of a non-proliferation regime, but bear the cost of foregoing nuclear weapons, conditional on NWS compliance.
narrative_ontology:constraint_stakeholder(npt_article_iv_vi_pairing__grand_bargain, non_nuclear_weapon_states_grand_bargain, beneficiary,
    organized, generational, constrained, global).

% The NWS are obligated by Article VI to pursue disarmament in good faith. They benefit from NNWS non-proliferation (Article IV) but resist full disarmament, often citing security concerns. Their 'identity_locked' exit reflects the deep integration of nuclear deterrence into their national security doctrines.
narrative_ontology:constraint_stakeholder(npt_article_iv_vi_pairing__grand_bargain, nuclear_weapon_states, agenda_setter,
    institutional, civilizational, identity_locked, global).

% These NNWS actively push for NWS compliance with Article VI, viewing the current pace of disarmament as a breach of the 'grand bargain'. They bear the cost of continued nuclear asymmetry and the risk of proliferation, feeling their restraint is unreciprocated.
narrative_ontology:constraint_stakeholder(npt_article_iv_vi_pairing__grand_bargain, non_nuclear_weapon_states_disarmament_advocates, payer,
    moderate, generational, constrained, global).

% The broader international system benefits from the NPT's role in limiting horizontal proliferation. However, the regime's legitimacy is eroded by perceived NWS non-compliance with disarmament obligations, creating a long-term stability risk.
narrative_ontology:constraint_stakeholder(npt_article_iv_vi_pairing__grand_bargain, global_security_regime, beneficiary,
    institutional, civilizational, constrained, universal).

% The IAEA verifies NNWS compliance with Article IV (safeguards) but has no mandate to verify NWS disarmament efforts under Article VI. It observes the growing tension between the two articles and the erosion of trust.
narrative_ontology:constraint_stakeholder(npt_article_iv_vi_pairing__grand_bargain, international_atomic_energy_agency, observer,
    institutional, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates global nuclear non-proliferation by establishing a framework where non-nuclear weapon states forgo nuclear weapons in exchange for a commitment from nuclear weapon states to disarm.
% TRANSFER_FUNCTION: Transfers the obligation of non-proliferation from NNWS to NWS, in exchange for a commitment to disarmament from NWS. The 'grand bargain' implies a reciprocal transfer of security and responsibility.
% ABSENT_VOICES: States that have withdrawn from the NPT or never joined, citing the failure of NWS to disarm, are absent. They would argue that the grand bargain is broken and that their security interests necessitate alternative approaches.
% DISAPPEARANCE_RATIONALE: If the grand bargain interpretation vanished, NNWS would likely feel less bound by Article IV, potentially leading to a cascade of proliferation. NWS would lose a key justification for their own arsenals and the global non-proliferation architecture would collapse, leading to a highly unstable world.
% FOUNDING_PROBLEM: The problem of preventing the spread of nuclear weapons while acknowledging the existing nuclear arsenals of a few states, aiming for eventual global disarmament.
% FOUNDING_PROBLEM_CORROBORATION: NWS claim the problem is live, citing ongoing proliferation risks and the need for deterrence. Many NNWS and disarmament advocates, supported by independent analyses and UN resolutions, argue the problem has shifted: the primary threat is now NWS failure to disarm, making the founding problem 'dead' in its original form and the current arrangement a perpetuation of nuclear apartheid.
narrative_ontology:disappearance_verdict(npt_article_iv_vi_pairing__grand_bargain, world_rearranges).
narrative_ontology:founding_problem_status(npt_article_iv_vi_pairing__grand_bargain, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(npt_article_iv_vi_pairing__grand_bargain, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(npt_article_iv_vi_pairing__grand_bargain, 'none', 1).
narrative_ontology:epsilon_provenance(npt_article_iv_vi_pairing__grand_bargain, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(npt_article_iv_vi_pairing__grand_bargain_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(npt_article_iv_vi_pairing__grand_bargain, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(npt_article_iv_vi_pairing__grand_bargain_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high because NNWS bear the cost of foregoing nuclear weapons while NWS maintain and modernize their arsenals, creating an asymmetry. Suppression is high because the NPT regime, backed by NWS, actively discourages and penalizes NNWS proliferation, while NWS disarmament is not subject to comparable enforcement. Theater ratio is moderate and rising, reflecting the increasing performative nature of NWS disarmament commitments without substantive progress. Resistance is high from NNWS who feel the bargain is unfulfilled. The claimed type is 'tangled_rope' because it has a genuine coordination function (preventing horizontal proliferation) but also asymmetric extraction (NNWS restraint vs. NWS retention).
 *
 * PERSPECTIVAL GAP:
 *   NWS perceive the NPT as primarily a non-proliferation instrument, with disarmament as an aspirational goal, thus experiencing it as a 'rope' or even a 'mountain' (security imperative). NNWS, particularly disarmament advocates, experience it as a 'snare' or 'tangled_rope' due to the unfulfilled disarmament promise and the resulting security asymmetry. The engine's per-seat classification will capture this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   NNWS (grand bargain advocates) are beneficiaries of the non-proliferation aspect but payers of the disarmament asymmetry. NWS are agenda-setters and primary beneficiaries of NNWS non-proliferation, but targets of the disarmament obligation. The global security regime is a diffuse beneficiary, but its stability is undermined by the constraint's extractive dynamics.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate (preventing proliferation while moving towards disarmament) is contested. If the disarmament aspect is viewed as atrophied, the constraint risks reclassification from a 'tangled_rope' to a 'snare' for NNWS, as the coordination story becomes cover for pure extraction. The rising extractiveness and theater ratio over time indicate this drift. Resolving mandatrophy would require substantive NWS disarmament progress to restore reciprocity.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    nws_disarmament_good_faith,
    'Are nuclear weapon states pursuing disarmament in ''good faith'' as required by Article VI, or is their progress merely performative?',
    'Independent, verifiable metrics for disarmament progress (e.g., warhead reductions, fissile material cut-off, verifiable dismantlement) compared against NWS security doctrines and spending on modernization.',
    'If ''good faith'' is disproven, the grand bargain reading''s legitimacy collapses, strengthening the ''snare'' classification for NNWS and potentially leading to treaty withdrawal or expansion of Article IV interpretation by NNWS.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(nws_disarmament_good_faith, empirical, 'Assesses the sincerity and substance of NWS disarmament efforts.').

omega_variable(
    reciprocity_threshold,
    'What level of NWS disarmament progress is sufficient to maintain the reciprocity implied by the ''grand bargain'' reading?',
    'International consensus-building among NNWS, potentially codified in NPT Review Conference outcomes or UN General Assembly resolutions, defining a minimum threshold for NWS compliance.',
    'Defining a clear threshold would either re-legitimize the constraint (if met) or provide a clear basis for NNWS to challenge its validity (if unmet), potentially leading to a reclassification of the constraint''s type for NNWS.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reciprocity_threshold, preference, 'Defines the point at which the reciprocal obligations are considered balanced.').

omega_variable(
    article_iv_expansion_legitimacy,
    'Does NWS breach of Article VI legitimately license NNWS to expand their interpretation of Article IV (e.g., to develop peaceful nuclear explosives or withdraw from the treaty)?',
    'International legal arbitration or a ruling by the International Court of Justice on the reciprocal nature of NPT obligations and the consequences of material breach.',
    'A ruling affirming the conditional nature of Article IV would fundamentally alter the constraint''s enforcement dynamics, shifting power towards NNWS and potentially forcing NWS compliance or treaty renegotiation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(article_iv_expansion_legitimacy, conceptual, 'Legal and political implications of NWS non-compliance on NNWS obligations.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(npt_article_iv_vi_pairing__grand_bargain, 1970, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(npt__tr_t1970, npt_article_iv_vi_pairing__grand_bargain, theater_ratio, 1970, 0.1).
narrative_ontology:measurement(npt__tr_t1985, npt_article_iv_vi_pairing__grand_bargain, theater_ratio, 1985, 0.2).
narrative_ontology:measurement(npt__tr_t2000, npt_article_iv_vi_pairing__grand_bargain, theater_ratio, 2000, 0.3).
narrative_ontology:measurement(npt__tr_t2010, npt_article_iv_vi_pairing__grand_bargain, theater_ratio, 2010, 0.35).
narrative_ontology:measurement(npt__tr_t2024, npt_article_iv_vi_pairing__grand_bargain, theater_ratio, 2024, 0.4).

% Extraction over time
narrative_ontology:measurement(npt__be_t1970, npt_article_iv_vi_pairing__grand_bargain, base_extractiveness, 1970, 0.4).
narrative_ontology:measurement(npt__be_t1985, npt_article_iv_vi_pairing__grand_bargain, base_extractiveness, 1985, 0.5).
narrative_ontology:measurement(npt__be_t2000, npt_article_iv_vi_pairing__grand_bargain, base_extractiveness, 2000, 0.6).
narrative_ontology:measurement(npt__be_t2010, npt_article_iv_vi_pairing__grand_bargain, base_extractiveness, 2010, 0.63).
narrative_ontology:measurement(npt__be_t2024, npt_article_iv_vi_pairing__grand_bargain, base_extractiveness, 2024, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(npt__su_t1970, npt_article_iv_vi_pairing__grand_bargain, suppression_requirement, 1970, 0.5).
narrative_ontology:measurement(npt__su_t1985, npt_article_iv_vi_pairing__grand_bargain, suppression_requirement, 1985, 0.55).
narrative_ontology:measurement(npt__su_t2000, npt_article_iv_vi_pairing__grand_bargain, suppression_requirement, 2000, 0.6).
narrative_ontology:measurement(npt__su_t2010, npt_article_iv_vi_pairing__grand_bargain, suppression_requirement, 2010, 0.65).
narrative_ontology:measurement(npt__su_t2024, npt_article_iv_vi_pairing__grand_bargain, suppression_requirement, 2024, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(npt_article_iv_vi_pairing__grand_bargain, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(npt_article_iv_vi_pairing__grand_bargain, 0.1).
narrative_ontology:affects_constraint(npt_article_iv_vi_pairing__grand_bargain, npt_article_iv_vi_pairing__nonproliferation_primary).
narrative_ontology:affects_constraint(npt_article_iv_vi_pairing__grand_bargain, npt_article_iv_vi_pairing__abolitionist).
narrative_ontology:affects_constraint(npt_article_iv_vi_pairing__grand_bargain, nuclear_deterrence_doctrine).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the NPT Article IV/VI pairing kernel. It emphasizes the reciprocal obligations of non-proliferation and disarmament. Sibling readings (nonproliferation_primary, abolitionist) offer different interpretations of the treaty's core purpose and the hierarchy of its articles.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
