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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: npt_article_iv_vi_pairing__grand_bargain
 *   human_readable: NPT Grand Bargain: Reciprocal Disarmament Obligation
 *   domain: international_law/nuclear_governance
 *
 * SUMMARY:
 *   This constraint is the 'grand_bargain' reading of the
 *   'npt_article_iv_vi_pairing' kernel. It interprets the Nuclear
 *   Non-Proliferation Treaty (NPT) as a reciprocal agreement where non-weapon
 *   states (NNWS) forgo nuclear weapons (Article IV) on the condition that
 *   weapon states (WWS) pursue nuclear disarmament in good faith (Article
 *   VI). A breach of Article VI by WWS is seen as undermining the legitimacy
 *   of Article IV, potentially licensing NNWS withdrawal or expansion of
 *   their nuclear programs. Sibling readings include
 *   'nonproliferation_primary' and 'abolitionist'.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(npt_article_iv_vi_pairing__grand_bargain, 0.65).
domain_priors:suppression_score(npt_article_iv_vi_pairing__grand_bargain, 0.75).
domain_priors:theater_ratio(npt_article_iv_vi_pairing__grand_bargain, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(npt_article_iv_vi_pairing__grand_bargain, extractiveness, 0.65).
narrative_ontology:constraint_metric(npt_article_iv_vi_pairing__grand_bargain, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(npt_article_iv_vi_pairing__grand_bargain, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(npt_article_iv_vi_pairing__grand_bargain, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(npt_article_iv_vi_pairing__grand_bargain, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(npt_article_iv_vi_pairing__grand_bargain, tangled_rope).
narrative_ontology:human_readable(npt_article_iv_vi_pairing__grand_bargain, "NPT Grand Bargain: Reciprocal Disarmament Obligation").
narrative_ontology:topic_domain(npt_article_iv_vi_pairing__grand_bargain, "international_law/nuclear_governance").

domain_priors:requires_active_enforcement(npt_article_iv_vi_pairing__grand_bargain).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(npt_article_iv_vi_pairing__grand_bargain, '2cf02b50-a9fd-4096-a438-80b792ca225a').
narrative_ontology:cs_kernel_codification('2cf02b50-a9fd-4096-a438-80b792ca225a', fixed_text).
narrative_ontology:cs_authority_grounding('2cf02b50-a9fd-4096-a438-80b792ca225a', lineage).
narrative_ontology:cs_interpretation_layer_present('2cf02b50-a9fd-4096-a438-80b792ca225a').
narrative_ontology:cs_reading_relation('2cf02b50-a9fd-4096-a438-80b792ca225a', npt_article_iv_vi_pairing__nonproliferation_primary, coexists_with).
narrative_ontology:cs_reading_relation('2cf02b50-a9fd-4096-a438-80b792ca225a', npt_article_iv_vi_pairing__abolitionist, influences).
narrative_ontology:cs_axiom('2cf02b50-a9fd-4096-a438-80b792ca225a', foundational, disarmament_is_legally_binding).
narrative_ontology:cs_axiom_status(disarmament_is_legally_binding, holdable).
narrative_ontology:cs_axiom_grounding('2cf02b50-a9fd-4096-a438-80b792ca225a', disarmament_is_legally_binding, deontological).
narrative_ontology:cs_axiom('2cf02b50-a9fd-4096-a438-80b792ca225a', foundational, nnws_restraint_is_conditional).
narrative_ontology:cs_axiom_status(nnws_restraint_is_conditional, holdable).
narrative_ontology:cs_axiom_grounding('2cf02b50-a9fd-4096-a438-80b792ca225a', nnws_restraint_is_conditional, conventional).
narrative_ontology:cs_reference_frame('2cf02b50-a9fd-4096-a438-80b792ca225a', original_npt_bargain_framework).
narrative_ontology:cs_drift_state('2cf02b50-a9fd-4096-a438-80b792ca225a', contemporary_npt_review_cycle, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('2cf02b50-a9fd-4096-a438-80b792ca225a', '').
narrative_ontology:cs_kernel_id(npt_article_iv_vi_pairing__grand_bargain, npt_article_iv_vi_pairing).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(npt_article_iv_vi_pairing__grand_bargain, weapon_states).
narrative_ontology:constraint_beneficiary(npt_article_iv_vi_pairing__grand_bargain, international_nonproliferation_regime).
narrative_ontology:constraint_victim(npt_article_iv_vi_pairing__grand_bargain, non_weapon_states).
narrative_ontology:constraint_vindicates(npt_article_iv_vi_pairing__grand_bargain, treaty_law_reciprocity_principle).
narrative_ontology:constraint_vindicates(npt_article_iv_vi_pairing__grand_bargain, good_faith_negotiation_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% As the recognized nuclear powers, they benefit from non-weapon states' (NNWS) restraint under Article IV. However, they are simultaneously obligated by Article VI to pursue disarmament in good faith, an obligation often seen as unfulfilled, leading to accusations of hypocrisy and treaty breach.
narrative_ontology:constraint_stakeholder(npt_article_iv_vi_pairing__grand_bargain, weapon_states, agenda_setter,
    institutional, generational, constrained, global).

% They bear the primary cost of non-proliferation by forgoing nuclear weapons development under Article IV. Their compliance is, under this reading, conditional on the weapon states' (WWS) progress on disarmament. Frustration with WWS inaction leads to calls for withdrawal or expansion of their own nuclear programs.
narrative_ontology:constraint_stakeholder(npt_article_iv_vi_pairing__grand_bargain, non_weapon_states, payer,
    organized, biographical, constrained, global).

% Responsible for verifying NNWS compliance with Article IV (non-proliferation). Its mandate does not extend to enforcing WWS disarmament, creating an asymmetry in verification and enforcement that undermines the grand bargain's reciprocity.
narrative_ontology:constraint_stakeholder(npt_article_iv_vi_pairing__grand_bargain, international_atomic_energy_agency, agenda_setter,
    institutional, immediate, constrained, global).

% Actively monitor and critique the NPT's implementation, particularly highlighting the lack of WWS disarmament progress and advocating for the full realization of Article VI. They provide independent analysis and pressure for accountability.
narrative_ontology:constraint_stakeholder(npt_article_iv_vi_pairing__grand_bargain, civil_society_disarmament_advocates, observer,
    analytical, generational, analytical, global).

% These periodic gatherings serve as the primary forum for NPT states parties to review the treaty's operation. NNWS often use these conferences to press WWS on their Article VI obligations, making them a key site for contestation over the grand bargain's fulfillment.
narrative_ontology:constraint_stakeholder(npt_article_iv_vi_pairing__grand_bargain, treaty_review_conferences, agenda_setter,
    institutional, immediate, constrained, global).

% The broader framework of norms, treaties, and institutions that seek to prevent the spread of nuclear weapons. It benefits from the NPT's continued (even if contested) operation, as the NPT is its cornerstone. Its legitimacy is tied to the perceived fairness and reciprocity of the grand bargain.
narrative_ontology:constraint_stakeholder(npt_article_iv_vi_pairing__grand_bargain, international_nonproliferation_regime, beneficiary,
    institutional, civilizational, identity_locked, global).
narrative_ontology:stakeholder_non_agent(npt_article_iv_vi_pairing__grand_bargain, international_nonproliferation_regime).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(npt_article_iv_vi_pairing__grand_bargain, weapon_states).
narrative_ontology:fixing_cost_class(npt_article_iv_vi_pairing__grand_bargain, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates global non-proliferation efforts by establishing a framework where non-weapon states (NNWS) forgo nuclear weapons in exchange for peaceful nuclear technology and a commitment from weapon states (WWS) to disarm. This reading emphasizes the reciprocal nature of these commitments.
% TRANSFER_FUNCTION: Transfers security assurances and peaceful nuclear technology to NNWS, in exchange for their commitment not to acquire nuclear weapons. Crucially, this reading asserts a reciprocal transfer of disarmament progress from WWS to NNWS, which is often unfulfilled.
% ABSENT_VOICES: States that have withdrawn from the NPT (e.g., North Korea) or never joined (e.g., India, Pakistan, Israel) are absent. They would argue that the treaty is discriminatory, ineffective, or that the grand bargain was never truly honored, justifying their own nuclear programs or non-participation.
% DISAPPEARANCE_RATIONALE: If the NPT's grand bargain framework vanished overnight, the foundational premise for NNWS restraint would collapse. Many NNWS would face increased pressure to develop nuclear weapons, leading to a rapid proliferation cascade, a breakdown of global security architecture, and a return to a more dangerous, multi-polar nuclear landscape.
% FOUNDING_PROBLEM: The existential threat of nuclear proliferation and the desire to prevent a world with many nuclear-armed states, while acknowledging the existing nuclear powers and committing them to eventual disarmament.
% FOUNDING_PROBLEM_CORROBORATION: UN resolutions, international security analyses, and statements from non-weapon states consistently highlight the ongoing proliferation risks and the unfulfilled disarmament commitments as a core challenge to global security. Independent experts and civil society groups also corroborate the continued relevance of both proliferation and disarmament concerns.
narrative_ontology:disappearance_verdict(npt_article_iv_vi_pairing__grand_bargain, world_rearranges).
narrative_ontology:founding_problem_status(npt_article_iv_vi_pairing__grand_bargain, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(npt_article_iv_vi_pairing__grand_bargain, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
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
 *   Extractiveness is high (0.65) because NNWS bear the cost of restraint while WWS often fail to meet their disarmament obligations, creating an asymmetric burden. Suppression is also high (0.75) as the international regime actively prevents NNWS proliferation through verification and sanctions. Theater ratio is moderate (0.40) reflecting the performative nature of some disarmament negotiations that yield little concrete progress, masking the underlying lack of reciprocity. Resistance is high (0.70) from NNWS who consistently challenge WWS non-compliance at NPT review conferences.
 *
 * PERSPECTIVAL GAP:
 *   Weapon states often view the NPT primarily as a non-proliferation success, downplaying their disarmament obligations. Non-weapon states, however, experience the same treaty as an unfulfilled bargain, where their restraint is extracted without reciprocal action. The engine's per-seat classification will highlight this divergence, showing a more extractive classification for NNWS and a more beneficial one for WWS.
 *
 * DIRECTIONALITY LOGIC:
 *   Weapon states are structural beneficiaries (d near 0.0) as they gain security from NNWS non-proliferation without fully delivering on their own disarmament commitments. Non-weapon states are targets (d near 1.0) as they bear the costs of restraint and the frustration of unfulfilled promises. The international non-proliferation regime itself is a beneficiary, as its stability depends on the NPT's continued (even if contested) operation.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading prevents mislabeling the NPT as a pure 'Rope' (simple coordination) by highlighting the active extraction from NNWS due to WWS non-compliance. It also prevents mislabeling it as a pure 'Snare' by acknowledging the genuine coordination function of preventing horizontal proliferation. The 'Tangled Rope' classification captures both the coordination and the asymmetric extraction, reflecting the ongoing contestation over the treaty's core bargain.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    npt_kernel_reading_grand_bargain,
    'Is this constraint a valid interpretation of the NPT''s core bargain, or is it an overemphasis on Article VI''s enforceability?',
    'Analysis of historical negotiating records, state practice, and legal scholarship regarding the NPT''s drafting and early implementation, particularly statements from NNWS at the time of accession.',
    'If validated, this reading strengthens the case for WWS accountability and NNWS conditional compliance. If invalidated, it would support readings that prioritize non-proliferation regardless of disarmament progress.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(npt_kernel_reading_grand_bargain, conceptual, 'This constraint is the ''grand_bargain'' reading of the ''npt_article_iv_vi_pairing'' kernel.').

omega_variable(
    disarmament_obligation_enforceability,
    'To what extent is Article VI''s disarmament obligation legally enforceable, and what constitutes a ''breach'' that would license NNWS withdrawal or Article IV expansion?',
    'International legal adjudication (e.g., ICJ advisory opinion), or a clear consensus among NPT states parties on specific benchmarks and consequences for non-compliance with Article VI.',
    'If Article VI is deemed strongly enforceable, the extractiveness from NNWS increases, and the constraint leans more towards a Snare. If it''s deemed largely aspirational, the extractiveness decreases, and the constraint leans more towards a Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(disarmament_obligation_enforceability, empirical, 'The enforceability of Article VI and triggers for NNWS response.').

omega_variable(
    verification_reciprocity_ambiguity,
    'Is the verification standard for WWS disarmament progress (Article VI) genuinely reciprocal to the verification standard for NNWS non-proliferation (Article IV)?',
    'Establishment of a robust, intrusive, and universally applied verification regime for WWS disarmament, comparable to IAEA safeguards for NNWS.',
    'If verification is not reciprocal, the asymmetry of the grand bargain is amplified, increasing extractiveness from NNWS. If reciprocal, the perceived fairness of the bargain improves.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(verification_reciprocity_ambiguity, empirical, 'Whether WWS disarmament is verifiable to the same standard as NNWS non-proliferation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(npt_article_iv_vi_pairing__grand_bargain, 1970, 2020).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(npt__tr_t1970, npt_article_iv_vi_pairing__grand_bargain, theater_ratio, 1970, 0.2).
narrative_ontology:measurement(npt__tr_t1980, npt_article_iv_vi_pairing__grand_bargain, theater_ratio, 1980, 0.25).
narrative_ontology:measurement(npt__tr_t1990, npt_article_iv_vi_pairing__grand_bargain, theater_ratio, 1990, 0.3).
narrative_ontology:measurement(npt__tr_t2000, npt_article_iv_vi_pairing__grand_bargain, theater_ratio, 2000, 0.35).
narrative_ontology:measurement(npt__tr_t2010, npt_article_iv_vi_pairing__grand_bargain, theater_ratio, 2010, 0.4).
narrative_ontology:measurement(npt__tr_t2020, npt_article_iv_vi_pairing__grand_bargain, theater_ratio, 2020, 0.4).

% Extraction over time
narrative_ontology:measurement(npt__be_t1970, npt_article_iv_vi_pairing__grand_bargain, base_extractiveness, 1970, 0.5).
narrative_ontology:measurement(npt__be_t1980, npt_article_iv_vi_pairing__grand_bargain, base_extractiveness, 1980, 0.55).
narrative_ontology:measurement(npt__be_t1990, npt_article_iv_vi_pairing__grand_bargain, base_extractiveness, 1990, 0.6).
narrative_ontology:measurement(npt__be_t2000, npt_article_iv_vi_pairing__grand_bargain, base_extractiveness, 2000, 0.63).
narrative_ontology:measurement(npt__be_t2010, npt_article_iv_vi_pairing__grand_bargain, base_extractiveness, 2010, 0.66).
narrative_ontology:measurement(npt__be_t2020, npt_article_iv_vi_pairing__grand_bargain, base_extractiveness, 2020, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(npt__su_t1970, npt_article_iv_vi_pairing__grand_bargain, suppression_requirement, 1970, 0.65).
narrative_ontology:measurement(npt__su_t1980, npt_article_iv_vi_pairing__grand_bargain, suppression_requirement, 1980, 0.7).
narrative_ontology:measurement(npt__su_t1990, npt_article_iv_vi_pairing__grand_bargain, suppression_requirement, 1990, 0.72).
narrative_ontology:measurement(npt__su_t2000, npt_article_iv_vi_pairing__grand_bargain, suppression_requirement, 2000, 0.75).
narrative_ontology:measurement(npt__su_t2010, npt_article_iv_vi_pairing__grand_bargain, suppression_requirement, 2010, 0.75).
narrative_ontology:measurement(npt__su_t2020, npt_article_iv_vi_pairing__grand_bargain, suppression_requirement, 2020, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(npt_article_iv_vi_pairing__grand_bargain, enforcement_mechanism).
narrative_ontology:affects_constraint(npt_article_iv_vi_pairing__grand_bargain, nuclear_weapons_prohibition_treaty).
narrative_ontology:affects_constraint(npt_article_iv_vi_pairing__grand_bargain, iran_nuclear_deal).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the NPT Article IV/VI pairing kernel, focusing on the reciprocal 'grand bargain' aspect. Sibling readings are 'nonproliferation_primary' and 'abolitionist'.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
