% ============================================================================
% CONSTRAINT STORY: npt_article_iv_vi_pairing__abolitionist
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_npt_article_iv_vi_pairing__abolitionist, []).

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
 *   constraint_id: npt_article_iv_vi_pairing__abolitionist
 *   human_readable: NPT Article IV/VI Pairing (Abolitionist Reading)
 *   domain: international_law/nuclear_governance/treaty_interpretation
 *
 * SUMMARY:
 *   This constraint represents the abolitionist reading of the NPT's Article
 *   IV (peaceful uses) and Article VI (disarmament) pairing. From this
 *   perspective, Article VI's mandate for complete disarmament is paramount,
 *   and the continued possession and modernization of nuclear weapons by
 *   nuclear-weapon states (NWS) renders Article IV's allowance for 'peaceful
 *   uses' illegitimate due to inherent dual-use proliferation risks. The
 *   authority for this reading derives from international humanitarian law
 *   and the precedent set by the Treaty on the Prohibition of Nuclear Weapons
 *   (TPNW), which categorically outlaws nuclear weapons. The NPT itself is
 *   seen as insufficient and delegitimized by the NWS's actions, with no
 *   distinction between peaceful and military nuclear programs being
 *   acceptable.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(npt_article_iv_vi_pairing__abolitionist, 0.85).
domain_priors:suppression_score(npt_article_iv_vi_pairing__abolitionist, 0.9).
domain_priors:theater_ratio(npt_article_iv_vi_pairing__abolitionist, 0.6).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(npt_article_iv_vi_pairing__abolitionist, extractiveness, 0.85).
narrative_ontology:constraint_metric(npt_article_iv_vi_pairing__abolitionist, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(npt_article_iv_vi_pairing__abolitionist, theater_ratio, 0.6).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(npt_article_iv_vi_pairing__abolitionist, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(npt_article_iv_vi_pairing__abolitionist, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(npt_article_iv_vi_pairing__abolitionist, snare).
narrative_ontology:human_readable(npt_article_iv_vi_pairing__abolitionist, "NPT Article IV/VI Pairing (Abolitionist Reading)").
narrative_ontology:topic_domain(npt_article_iv_vi_pairing__abolitionist, "international_law/nuclear_governance/treaty_interpretation").

domain_priors:requires_active_enforcement(npt_article_iv_vi_pairing__abolitionist).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(npt_article_iv_vi_pairing__abolitionist, 'd9cca0b2-547b-48ee-81eb-23ae9d8ca3b5').
narrative_ontology:cs_kernel_codification('d9cca0b2-547b-48ee-81eb-23ae9d8ca3b5', fixed_text).
narrative_ontology:cs_authority_grounding('d9cca0b2-547b-48ee-81eb-23ae9d8ca3b5', lineage).
narrative_ontology:cs_interpretation_layer_present('d9cca0b2-547b-48ee-81eb-23ae9d8ca3b5').
narrative_ontology:cs_reading_relation('d9cca0b2-547b-48ee-81eb-23ae9d8ca3b5', npt_article_iv_vi_pairing__nonproliferation_primary, forecloses).
narrative_ontology:cs_reading_relation('d9cca0b2-547b-48ee-81eb-23ae9d8ca3b5', npt_article_iv_vi_pairing__grand_bargain, forecloses).
narrative_ontology:cs_axiom('d9cca0b2-547b-48ee-81eb-23ae9d8ca3b5', foundational, nuclear_weapons_categorically_illegal).
narrative_ontology:cs_axiom_status(nuclear_weapons_categorically_illegal, holdable).
narrative_ontology:cs_axiom_grounding('d9cca0b2-547b-48ee-81eb-23ae9d8ca3b5', nuclear_weapons_categorically_illegal, deontological).
narrative_ontology:cs_axiom('d9cca0b2-547b-48ee-81eb-23ae9d8ca3b5', foundational, humanitarian_law_supremacy).
narrative_ontology:cs_axiom_status(humanitarian_law_supremacy, holdable).
narrative_ontology:cs_axiom_grounding('d9cca0b2-547b-48ee-81eb-23ae9d8ca3b5', humanitarian_law_supremacy, deontological).
narrative_ontology:cs_reference_frame('d9cca0b2-547b-48ee-81eb-23ae9d8ca3b5', complete_nuclear_disarmament).
narrative_ontology:cs_drift_state('d9cca0b2-547b-48ee-81eb-23ae9d8ca3b5', contemporary_nuclear_modernization_era, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('d9cca0b2-547b-48ee-81eb-23ae9d8ca3b5', '').
narrative_ontology:cs_kernel_id(npt_article_iv_vi_pairing__abolitionist, npt_article_iv_vi_pairing).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(npt_article_iv_vi_pairing__abolitionist, nuclear_weapon_states).
narrative_ontology:constraint_beneficiary(npt_article_iv_vi_pairing__abolitionist, states_under_nuclear_umbrella).
narrative_ontology:constraint_victim(npt_article_iv_vi_pairing__abolitionist, non_nuclear_weapon_states).
narrative_ontology:constraint_victim(npt_article_iv_vi_pairing__abolitionist, global_civil_society).
narrative_ontology:constraint_victim(npt_article_iv_vi_pairing__abolitionist, future_generations).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(npt_article_iv_vi_pairing__abolitionist, tpnw_signatories).
narrative_ontology:constraint_vindicates(npt_article_iv_vi_pairing__abolitionist, humanitarian_law_supremacy_doctrine).
narrative_ontology:constraint_vindicates(npt_article_iv_vi_pairing__abolitionist, nuclear_weapons_prohibition_norm).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintain and modernize nuclear arsenals, interpret Article VI as a long-term aspiration, and resist legally binding disarmament timelines. They benefit from the perceived security and status conferred by nuclear weapons, while framing their possession as a necessary evil for global stability.
narrative_ontology:constraint_stakeholder(npt_article_iv_vi_pairing__abolitionist, nuclear_weapon_states, agenda_setter,
    institutional, generational, constrained, global).

% Have renounced nuclear weapons under the NPT, but bear the risk of proliferation and nuclear use. They increasingly view the NPT's Article IV 'peaceful uses' as a cover for dual-use technologies that perpetuate proliferation risk, and Article VI as unfulfilled, making them victims of an asymmetric arrangement.
narrative_ontology:constraint_stakeholder(npt_article_iv_vi_pairing__abolitionist, non_nuclear_weapon_states, payer,
    organized, biographical, constrained, global).

% Advocates for complete nuclear disarmament, viewing nuclear weapons as inherently immoral and illegal under international humanitarian law. They organize protests, lobby governments, and support treaties like the TPNW, bearing the moral and existential cost of nuclear risk.
narrative_ontology:constraint_stakeholder(npt_article_iv_vi_pairing__abolitionist, global_civil_society, payer,
    organized, generational, identity_locked, global).

% Are the ultimate victims of nuclear proliferation risk and potential environmental catastrophe from nuclear war. They have no voice in current policy debates but bear the long-term consequences of the current nuclear order.
narrative_ontology:constraint_stakeholder(npt_article_iv_vi_pairing__abolitionist, future_generations, excluded,
    powerless, civilizational, trapped, universal).

% States that have signed and ratified the Treaty on the Prohibition of Nuclear Weapons, actively working to delegitimize nuclear weapons and challenge the NPT's perceived asymmetry. They benefit from the moral authority of upholding humanitarian law and setting a new international norm.
narrative_ontology:constraint_stakeholder(npt_article_iv_vi_pairing__abolitionist, tpnw_signatories, beneficiary,
    organized, generational, mobile, global).
narrative_ontology:stakeholder_secondary_role(npt_article_iv_vi_pairing__abolitionist, tpnw_signatories, agenda_setter).

% Monitors compliance with Article III (safeguards) but has no mandate to enforce Article VI disarmament. From an abolitionist perspective, its role is limited to managing the proliferation risk perpetuated by the NPT's inherent flaws, rather than resolving the core problem.
narrative_ontology:constraint_stakeholder(npt_article_iv_vi_pairing__abolitionist, international_atomic_energy_agency, observer,
    institutional, biographical, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The NPT was intended to coordinate global efforts to prevent nuclear proliferation and achieve disarmament. From the abolitionist perspective, its disarmament function has failed, and its non-proliferation function is compromised by the continued existence of nuclear weapons.
% TRANSFER_FUNCTION: Transfers perceived security and geopolitical leverage to nuclear-weapon states, while transferring existential risk and a burden of non-proliferation compliance to non-nuclear-weapon states and the global population.
% ABSENT_VOICES: Future generations, populations in states under nuclear threat, and victims of past nuclear use are largely excluded from the decision-making processes that perpetuate nuclear weapons. They would unequivocally demand complete disarmament.
% DISAPPEARANCE_RATIONALE: If the NPT framework, as currently interpreted, vanished overnight, the global nuclear order would undergo a radical rearrangement. It could lead to either rapid, universal disarmament driven by humanitarian law, or a chaotic proliferation free-for-all, as the existing (albeit flawed) non-proliferation norms would be gone.
% FOUNDING_PROBLEM: To prevent the catastrophic spread of nuclear weapons to more states and to achieve the ultimate goal of nuclear disarmament, ensuring global peace and security.
% FOUNDING_PROBLEM_CORROBORATION: Nuclear-weapon states claim the non-proliferation aspect is live and successful, while the disarmament goal is a long-term aspiration. TPNW signatories, UN General Assembly resolutions, and civil society organizations corroborate that the disarmament problem is 'dead' or severely stalled, and the non-proliferation problem is exacerbated by the NWS's failure to disarm.
narrative_ontology:disappearance_verdict(npt_article_iv_vi_pairing__abolitionist, world_rearranges).
narrative_ontology:founding_problem_status(npt_article_iv_vi_pairing__abolitionist, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(npt_article_iv_vi_pairing__abolitionist, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(npt_article_iv_vi_pairing__abolitionist, 'none', 1).
narrative_ontology:epsilon_provenance(npt_article_iv_vi_pairing__abolitionist, 0.85, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(npt_article_iv_vi_pairing__abolitionist_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(npt_article_iv_vi_pairing__abolitionist, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(npt_article_iv_vi_pairing__abolitionist_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The high extractiveness (0.85) reflects the perceived cost to non-nuclear-weapon states and global civil society from the perpetuation of nuclear risk and the NWS's failure to disarm. Suppression (0.90) is high due to the NWS's active resistance to disarmament, their control over nuclear technology, and their efforts to delegitimize the TPNW. The rising theater ratio (0.60) indicates that disarmament talks are increasingly seen as performative, lacking genuine commitment to the NPT's ultimate goal. Accessibility collapse (0.80) is high because the NWS actively block pathways to complete disarmament, while resistance (0.75) is significant from non-nuclear states and civil society.
 *
 * PERSPECTIVAL GAP:
 *   The abolitionist reading fundamentally diverges from the NWS's perspective, which views the NPT as a successful non-proliferation regime that manages risk. The engine's classification of 'snare' from this reading highlights the structural asymmetry and extraction, contrasting sharply with how NWS might claim the NPT as a 'rope' or 'tangled_rope' for stability.
 *
 * DIRECTIONALITY LOGIC:
 *   Nuclear-weapon states are clear beneficiaries, deriving security and status from their arsenals. Non-nuclear-weapon states and global civil society are victims, bearing the risks and costs of the nuclear order. Future generations are excluded victims, facing the ultimate consequences. TPNW signatories act as both beneficiaries (of moral authority) and agenda-setters (for a new norm).
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    npt_legitimacy_status,
    'Is the NPT, as currently implemented, a legitimate framework for nuclear governance, or has its legitimacy been fundamentally undermined by the failure of nuclear-weapon states to disarm?',
    'A shift in NWS policy towards concrete, verifiable disarmament, or a widespread international consensus (e.g., through UN General Assembly resolutions or ICJ rulings) explicitly declaring the NPT''s disarmament pillar to be in breach.',
    'If legitimacy is deemed undermined, the NPT''s coordination function collapses, amplifying the ''snare'' classification and strengthening the case for alternative frameworks like the TPNW. If legitimacy is reaffirmed, the abolitionist reading''s extractiveness might be re-evaluated downward.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(npt_legitimacy_status, conceptual, 'The fundamental legitimacy of the NPT framework.').

omega_variable(
    dual_use_proliferation_risk_quantification,
    'To what extent does Article IV''s allowance for ''peaceful uses'' of nuclear technology inherently and unacceptably perpetuate dual-use proliferation risk, making it illegitimate from an abolitionist perspective?',
    'Independent technical assessments quantifying the irreducible dual-use risk of various nuclear technologies, and international legal interpretations on the threshold at which ''peaceful'' programs become inherently destabilizing.',
    'Higher quantified irreducible risk strengthens the abolitionist claim that Article IV is a source of extraction (risk) and delegitimizes the NPT''s non-proliferation function. Lower risk might suggest a more nuanced view, though still not full disarmament.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(dual_use_proliferation_risk_quantification, empirical, 'Quantification of dual-use proliferation risk from peaceful nuclear programs.').

omega_variable(
    tpnw_norm_effectiveness,
    'How effective is the Treaty on the Prohibition of Nuclear Weapons (TPNW) in establishing a new, universally recognized norm against nuclear weapons, thereby influencing the NPT framework?',
    'Increased ratifications by non-nuclear-weapon states, shifts in NWS rhetoric or policy, and changes in international financial institutions'' policies regarding nuclear weapons-related investments.',
    'Stronger TPNW norm effectiveness would increase the pressure on NWS, potentially reducing their ''arbitrage'' exit options and increasing the perceived ''resistance'' to the NPT status quo, pushing the NPT further towards a ''snare'' or even ''piton'' if its function atrophies.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(tpnw_norm_effectiveness, empirical, 'The impact of the TPNW on the global nuclear norm.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(npt_article_iv_vi_pairing__abolitionist, 1968, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(npt__tr_t1968, npt_article_iv_vi_pairing__abolitionist, theater_ratio, 1968, 0.3).
narrative_ontology:measurement(npt__tr_t1980, npt_article_iv_vi_pairing__abolitionist, theater_ratio, 1980, 0.4).
narrative_ontology:measurement(npt__tr_t1992, npt_article_iv_vi_pairing__abolitionist, theater_ratio, 1992, 0.45).
narrative_ontology:measurement(npt__tr_t2004, npt_article_iv_vi_pairing__abolitionist, theater_ratio, 2004, 0.5).
narrative_ontology:measurement(npt__tr_t2016, npt_article_iv_vi_pairing__abolitionist, theater_ratio, 2016, 0.55).
narrative_ontology:measurement(npt__tr_t2024, npt_article_iv_vi_pairing__abolitionist, theater_ratio, 2024, 0.6).

% Extraction over time
narrative_ontology:measurement(npt__be_t1968, npt_article_iv_vi_pairing__abolitionist, base_extractiveness, 1968, 0.6).
narrative_ontology:measurement(npt__be_t1980, npt_article_iv_vi_pairing__abolitionist, base_extractiveness, 1980, 0.7).
narrative_ontology:measurement(npt__be_t1992, npt_article_iv_vi_pairing__abolitionist, base_extractiveness, 1992, 0.75).
narrative_ontology:measurement(npt__be_t2004, npt_article_iv_vi_pairing__abolitionist, base_extractiveness, 2004, 0.8).
narrative_ontology:measurement(npt__be_t2016, npt_article_iv_vi_pairing__abolitionist, base_extractiveness, 2016, 0.83).
narrative_ontology:measurement(npt__be_t2024, npt_article_iv_vi_pairing__abolitionist, base_extractiveness, 2024, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(npt__su_t1968, npt_article_iv_vi_pairing__abolitionist, suppression_requirement, 1968, 0.7).
narrative_ontology:measurement(npt__su_t1980, npt_article_iv_vi_pairing__abolitionist, suppression_requirement, 1980, 0.75).
narrative_ontology:measurement(npt__su_t1992, npt_article_iv_vi_pairing__abolitionist, suppression_requirement, 1992, 0.8).
narrative_ontology:measurement(npt__su_t2004, npt_article_iv_vi_pairing__abolitionist, suppression_requirement, 2004, 0.85).
narrative_ontology:measurement(npt__su_t2016, npt_article_iv_vi_pairing__abolitionist, suppression_requirement, 2016, 0.88).
narrative_ontology:measurement(npt__su_t2024, npt_article_iv_vi_pairing__abolitionist, suppression_requirement, 2024, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(npt_article_iv_vi_pairing__abolitionist, enforcement_mechanism).
narrative_ontology:affects_constraint(npt_article_iv_vi_pairing__abolitionist, npt_article_iv_vi_pairing__nonproliferation_primary).
narrative_ontology:affects_constraint(npt_article_iv_vi_pairing__abolitionist, npt_article_iv_vi_pairing__grand_bargain).
narrative_ontology:affects_constraint(npt_article_iv_vi_pairing__abolitionist, nuclear_deterrence_doctrine).
narrative_ontology:affects_constraint(npt_article_iv_vi_pairing__abolitionist, tpnw_legitimacy).

% DUAL FORMULATION NOTE:
% This constraint is the 'abolitionist' reading of the NPT Article IV/VI pairing kernel. It focuses on the categorical illegality of nuclear weapons and the failure of NWS to disarm, contrasting with 'nonproliferation_primary' (NWS security interests) and 'grand_bargain' (reciprocal obligations).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
