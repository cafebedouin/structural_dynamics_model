% ============================================================================
% CONSTRAINT STORY: npt_article_iv_vi_pairing__nonproliferation_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_npt_article_iv_vi_pairing__nonproliferation_primary, []).

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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: npt_article_iv_vi_pairing__nonproliferation_primary
 *   human_readable: NPT Article IV/VI Pairing (Nonproliferation Primary Reading)
 *   domain: international_law/nuclear_governance
 *
 * SUMMARY:
 *   This constraint represents the 'nonproliferation primary' reading of the
 *   NPT's Article IV/VI pairing, where Article IV (peaceful nuclear energy)
 *   is conditional on Article III (safeguards verification), and Article VI
 *   (disarmament) is treated as an aspirational, non-justiciable goal.
 *   Authority for the regime derives from the security interests of nuclear
 *   weapon states in preventing horizontal proliferation. This reading
 *   effectively stabilizes a two-tier nuclear order, with weapon states as
 *   permanent beneficiaries and non-weapon states as perpetual
 *   restraint-bearers. The constraint is claimed as a Rope by its proponents
 *   (a coordination mechanism for global security) but its metrics reflect a
 *   Tangled Rope due to high extraction and suppression, particularly for
 *   non-nuclear weapon states.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(npt_article_iv_vi_pairing__nonproliferation_primary, 0.78).
domain_priors:suppression_score(npt_article_iv_vi_pairing__nonproliferation_primary, 0.85).
domain_priors:theater_ratio(npt_article_iv_vi_pairing__nonproliferation_primary, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(npt_article_iv_vi_pairing__nonproliferation_primary, extractiveness, 0.78).
narrative_ontology:constraint_metric(npt_article_iv_vi_pairing__nonproliferation_primary, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(npt_article_iv_vi_pairing__nonproliferation_primary, theater_ratio, 0.65).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(npt_article_iv_vi_pairing__nonproliferation_primary, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(npt_article_iv_vi_pairing__nonproliferation_primary, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(npt_article_iv_vi_pairing__nonproliferation_primary, tangled_rope).
narrative_ontology:human_readable(npt_article_iv_vi_pairing__nonproliferation_primary, "NPT Article IV/VI Pairing (Nonproliferation Primary Reading)").
narrative_ontology:topic_domain(npt_article_iv_vi_pairing__nonproliferation_primary, "international_law/nuclear_governance").

domain_priors:requires_active_enforcement(npt_article_iv_vi_pairing__nonproliferation_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(npt_article_iv_vi_pairing__nonproliferation_primary, '8f5a9f67-e0e6-480a-aa2f-cbd062025c5e').
narrative_ontology:cs_kernel_codification('8f5a9f67-e0e6-480a-aa2f-cbd062025c5e', fixed_text).
narrative_ontology:cs_authority_grounding('8f5a9f67-e0e6-480a-aa2f-cbd062025c5e', extraction).
narrative_ontology:cs_interpretation_layer_present('8f5a9f67-e0e6-480a-aa2f-cbd062025c5e').
narrative_ontology:cs_reading_relation('8f5a9f67-e0e6-480a-aa2f-cbd062025c5e', npt_article_iv_vi_pairing__grand_bargain, influences).
narrative_ontology:cs_reading_relation('8f5a9f67-e0e6-480a-aa2f-cbd062025c5e', npt_article_iv_vi_pairing__abolitionist, influences).
narrative_ontology:cs_axiom('8f5a9f67-e0e6-480a-aa2f-cbd062025c5e', foundational, horizontal_proliferation_is_primary_threat).
narrative_ontology:cs_axiom_status(horizontal_proliferation_is_primary_threat, holdable).
narrative_ontology:cs_axiom_grounding('8f5a9f67-e0e6-480a-aa2f-cbd062025c5e', horizontal_proliferation_is_primary_threat, empirically_contingent).
narrative_ontology:cs_axiom('8f5a9f67-e0e6-480a-aa2f-cbd062025c5e', foundational, article_vi_is_aspirational_not_binding).
narrative_ontology:cs_axiom_status(article_vi_is_aspirational_not_binding, holdable).
narrative_ontology:cs_axiom_grounding('8f5a9f67-e0e6-480a-aa2f-cbd062025c5e', article_vi_is_aspirational_not_binding, conventional).
narrative_ontology:cs_reference_frame('8f5a9f67-e0e6-480a-aa2f-cbd062025c5e', weapon_state_security_paradigm).
narrative_ontology:cs_drift_state('8f5a9f67-e0e6-480a-aa2f-cbd062025c5e', contemporary_humanitarian_law_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('8f5a9f67-e0e6-480a-aa2f-cbd062025c5e', '').
narrative_ontology:cs_kernel_id(npt_article_iv_vi_pairing__nonproliferation_primary, npt_article_iv_vi_pairing).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(npt_article_iv_vi_pairing__nonproliferation_primary, nuclear_weapon_states).
narrative_ontology:constraint_beneficiary(npt_article_iv_vi_pairing__nonproliferation_primary, iaea).
narrative_ontology:constraint_victim(npt_article_iv_vi_pairing__nonproliferation_primary, non_nuclear_weapon_states).
narrative_ontology:constraint_victim(npt_article_iv_vi_pairing__nonproliferation_primary, global_civil_society).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These states interpret the NPT as primarily a nonproliferation instrument, where their security interests justify maintaining arsenals while preventing others from acquiring them. They benefit from the two-tier order and control the enforcement mechanisms through the IAEA and UNSC. They treat Article VI as an aspirational goal, not a binding timeline.
narrative_ontology:constraint_stakeholder(npt_article_iv_vi_pairing__nonproliferation_primary, nuclear_weapon_states, agenda_setter,
    institutional, generational, arbitrage, global).

% These states bear the primary burden of nonproliferation, submitting to IAEA safeguards under Article III and foregoing nuclear weapons development under Article II. They receive the 'benefit' of peaceful nuclear technology under Article IV, but this is conditional on verification, and their security is perpetually secondary to weapon states' interests. Their calls for Article VI disarmament are largely ignored.
narrative_ontology:constraint_stakeholder(npt_article_iv_vi_pairing__nonproliferation_primary, non_nuclear_weapon_states, payer,
    moderate, biographical, constrained, global).

% The International Atomic Energy Agency is the primary verification body for Article III, deriving its mandate and funding from the nonproliferation regime. Its authority is strengthened by this reading, as its verification role is central, while the more contentious disarmament aspects of Article VI fall outside its direct enforcement purview.
narrative_ontology:constraint_stakeholder(npt_article_iv_vi_pairing__nonproliferation_primary, iaea, beneficiary,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(npt_article_iv_vi_pairing__nonproliferation_primary, iaea, agenda_setter).

% Advocates for nuclear disarmament and humanitarian law, viewing nuclear weapons as an existential threat. This reading marginalizes their concerns by rendering Article VI non-justiciable and prioritizing weapon state security interests over universal disarmament. Their identity is locked into the moral imperative of abolition, making exit from advocacy unthinkable despite lack of direct influence.
narrative_ontology:constraint_stakeholder(npt_article_iv_vi_pairing__nonproliferation_primary, global_civil_society, excluded,
    powerless, generational, identity_locked, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a global regime to prevent the spread of nuclear weapons by establishing a framework for verification (Article III) and peaceful nuclear technology transfer (Article IV), while implicitly legitimizing the existing nuclear arsenals of weapon states.
% TRANSFER_FUNCTION: Transfers security assurances (from weapon states) and peaceful nuclear technology (Article IV) to non-nuclear weapon states, in exchange for their commitment to nonproliferation (Article II) and submission to safeguards (Article III). Simultaneously transfers the right to maintain nuclear arsenals to weapon states, in exchange for an aspirational commitment to disarmament (Article VI).
% ABSENT_VOICES: States that have developed nuclear weapons outside the NPT framework (e.g., India, Pakistan, Israel) are absent from the NPT's internal discourse, as are proponents of the Treaty on the Prohibition of Nuclear Weapons (TPNW), whose very existence challenges the NPT's two-tier structure. They would argue for universal disarmament and the illegitimacy of nuclear weapons.
% DISAPPEARANCE_RATIONALE: If this interpretation of the NPT vanished, the global nonproliferation regime would collapse. Non-nuclear weapon states would lose their primary incentive for restraint, potentially leading to widespread proliferation. Weapon states would lose their primary legal justification for maintaining arsenals while denying others, leading to a highly unstable and unpredictable security environment.
% FOUNDING_PROBLEM: The problem of preventing the spread of nuclear weapons to more states, while acknowledging the existing nuclear capabilities of a few, to stabilize international security during the Cold War.
% FOUNDING_PROBLEM_CORROBORATION: Weapon states and the IAEA attest that the problem of horizontal proliferation remains live and critical. Non-nuclear weapon states and global civil society acknowledge the historical problem but argue that the current interpretation has ossified into a permanent two-tier system, failing to address the vertical proliferation problem (weapon state arsenals).
narrative_ontology:disappearance_verdict(npt_article_iv_vi_pairing__nonproliferation_primary, world_rearranges).
narrative_ontology:founding_problem_status(npt_article_iv_vi_pairing__nonproliferation_primary, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(npt_article_iv_vi_pairing__nonproliferation_primary, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(npt_article_iv_vi_pairing__nonproliferation_primary, 'none', 1).
narrative_ontology:epsilon_provenance(npt_article_iv_vi_pairing__nonproliferation_primary, 0.78, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(npt_article_iv_vi_pairing__nonproliferation_primary_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(npt_article_iv_vi_pairing__nonproliferation_primary, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(npt_article_iv_vi_pairing__nonproliferation_primary_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.78) because non-nuclear weapon states bear significant costs (foregoing weapons, submitting to intrusive inspections) for benefits (peaceful technology, security assurances) that are often conditional or perceived as insufficient, while weapon states retain their arsenals. Suppression is very high (0.85) due to the enforcement power of weapon states (e.g., UNSC sanctions) against proliferation, and the lack of viable exit options for non-nuclear weapon states seeking security outside the NPT. Theater ratio is high (0.65) because the disarmament commitment of Article VI is largely performative, with little concrete action or verifiable progress from weapon states, while the nonproliferation aspects are rigorously enforced.
 *
 * PERSPECTIVAL GAP:
 *   Weapon states perceive this as a successful coordination mechanism for global stability, while non-nuclear weapon states and civil society increasingly view it as an extractive arrangement that perpetuates inequality and nuclear risk. The engine's classification will likely diverge from the claimed 'Rope' due to the high extractiveness and suppression metrics, reflecting the experience of the payer/victim seats.
 *
 * DIRECTIONALITY LOGIC:
 *   Nuclear weapon states are clear beneficiaries (low d) as they maintain their arsenals and control the regime's enforcement. Non-nuclear weapon states are primary targets (high d) as they bear the costs of restraint and verification without reciprocal disarmament. The IAEA benefits from its central role in verification. Global civil society is excluded and identity-locked, bearing the costs of a perpetual nuclear threat without direct influence.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    article_vi_justiciability,
    'Is Article VI of the NPT genuinely aspirational and non-justiciable, or does it represent a binding legal obligation for nuclear weapon states to disarm?',
    'International Court of Justice advisory opinion or a new treaty (e.g., TPNW) gaining universal adherence that explicitly clarifies the legal status and enforceability of disarmament obligations.',
    'If Article VI is deemed binding and justiciable, the extractiveness of this reading would increase dramatically for non-nuclear weapon states (as their ''bargain'' is unfulfilled), and the theater ratio would approach 1.0. The claimed type would shift more firmly towards Snare or Tangled Rope, as the coordination story for weapon states would be undermined.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(article_vi_justiciability, conceptual, 'Ambiguity regarding the legal force of NPT Article VI disarmament commitments.').

omega_variable(
    security_interest_vs_global_good,
    'Does the authority for the nonproliferation regime primarily derive from the security interests of nuclear weapon states, or from a broader global interest in preventing nuclear war and achieving disarmament?',
    'A shift in global norms and international legal frameworks that prioritizes collective security and humanitarian concerns over state-centric security interests, potentially evidenced by universal adherence to the TPNW.',
    'If authority shifts to a global good, the legitimacy of weapon states'' arsenals would be challenged, increasing their perceived extractiveness and suppression within the regime. This would likely lead to a reclassification of the constraint towards a Snare for weapon states, as their ''benefit'' would be seen as illegitimate extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(security_interest_vs_global_good, preference, 'The foundational grounding of authority for the nuclear nonproliferation regime.').

omega_variable(
    two_tier_permanence,
    'Is the two-tier nuclear order (weapon states vs. non-weapon states) a temporary, transitional arrangement, or has it become a permanent feature of international security under this reading?',
    'Observable, verifiable progress by weapon states towards Article VI disarmament, or the emergence of new nuclear weapon states that fundamentally challenge the existing order.',
    'If the two-tier order is confirmed as permanent, the extractiveness for non-nuclear weapon states would be seen as irreducible, and the theater ratio for Article VI would remain high. If it is shown to be temporary, the constraint might be re-evaluated as a Scaffold, with a clear sunset for the current arrangement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(two_tier_permanence, empirical, 'The perceived permanence of the nuclear two-tier system.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(npt_article_iv_vi_pairing__nonproliferation_primary, 1970, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(npt__tr_t1970, npt_article_iv_vi_pairing__nonproliferation_primary, theater_ratio, 1970, 0.3).
narrative_ontology:measurement(npt__tr_t1980, npt_article_iv_vi_pairing__nonproliferation_primary, theater_ratio, 1980, 0.4).
narrative_ontology:measurement(npt__tr_t1990, npt_article_iv_vi_pairing__nonproliferation_primary, theater_ratio, 1990, 0.5).
narrative_ontology:measurement(npt__tr_t2000, npt_article_iv_vi_pairing__nonproliferation_primary, theater_ratio, 2000, 0.58).
narrative_ontology:measurement(npt__tr_t2010, npt_article_iv_vi_pairing__nonproliferation_primary, theater_ratio, 2010, 0.62).
narrative_ontology:measurement(npt__tr_t2024, npt_article_iv_vi_pairing__nonproliferation_primary, theater_ratio, 2024, 0.65).

% Extraction over time
narrative_ontology:measurement(npt__be_t1970, npt_article_iv_vi_pairing__nonproliferation_primary, base_extractiveness, 1970, 0.6).
narrative_ontology:measurement(npt__be_t1980, npt_article_iv_vi_pairing__nonproliferation_primary, base_extractiveness, 1980, 0.65).
narrative_ontology:measurement(npt__be_t1990, npt_article_iv_vi_pairing__nonproliferation_primary, base_extractiveness, 1990, 0.7).
narrative_ontology:measurement(npt__be_t2000, npt_article_iv_vi_pairing__nonproliferation_primary, base_extractiveness, 2000, 0.73).
narrative_ontology:measurement(npt__be_t2010, npt_article_iv_vi_pairing__nonproliferation_primary, base_extractiveness, 2010, 0.76).
narrative_ontology:measurement(npt__be_t2024, npt_article_iv_vi_pairing__nonproliferation_primary, base_extractiveness, 2024, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(npt__su_t1970, npt_article_iv_vi_pairing__nonproliferation_primary, suppression_requirement, 1970, 0.7).
narrative_ontology:measurement(npt__su_t1980, npt_article_iv_vi_pairing__nonproliferation_primary, suppression_requirement, 1980, 0.75).
narrative_ontology:measurement(npt__su_t1990, npt_article_iv_vi_pairing__nonproliferation_primary, suppression_requirement, 1990, 0.8).
narrative_ontology:measurement(npt__su_t2000, npt_article_iv_vi_pairing__nonproliferation_primary, suppression_requirement, 2000, 0.82).
narrative_ontology:measurement(npt__su_t2010, npt_article_iv_vi_pairing__nonproliferation_primary, suppression_requirement, 2010, 0.84).
narrative_ontology:measurement(npt__su_t2024, npt_article_iv_vi_pairing__nonproliferation_primary, suppression_requirement, 2024, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(npt_article_iv_vi_pairing__nonproliferation_primary, enforcement_mechanism).
narrative_ontology:affects_constraint(npt_article_iv_vi_pairing__nonproliferation_primary, npt_article_iv_vi_pairing__grand_bargain).
narrative_ontology:affects_constraint(npt_article_iv_vi_pairing__nonproliferation_primary, npt_article_iv_vi_pairing__abolitionist).
narrative_ontology:affects_constraint(npt_article_iv_vi_pairing__nonproliferation_primary, iaea_safeguards_regime).
narrative_ontology:affects_constraint(npt_article_iv_vi_pairing__nonproliferation_primary, unsc_sanctions_regime).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the NPT Article IV/VI pairing kernel. This 'nonproliferation primary' reading emphasizes weapon state security interests and the aspirational nature of Article VI, contrasting with the 'grand bargain' and 'abolitionist' readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
