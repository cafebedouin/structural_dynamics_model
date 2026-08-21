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
 *   This constraint represents the 'Grand Bargain' reading of the Nuclear
 *   Non-Proliferation Treaty (NPT), where the non-acquisition of nuclear
 *   weapons by non-nuclear weapon states (Article IV) is reciprocally linked
 *   to the disarmament efforts of nuclear weapon states (Article VI). This
 *   reading asserts that the legitimacy of Article IV is conditional on
 *   progress in Article VI, and a breach of Article VI by NWS undermines the
 *   entire treaty's foundation. This is one reading of the NPT's core
 *   commitment system.
 *
 * KEY AGENTS:
 *   - non_nuclear_weapon_states_grand_bargain: Primary beneficiary/payer (organized/constrained)
 *   - nuclear_weapon_states_grand_bargain: Primary target (institutional/constrained)
 *   - international_nonproliferation_regime: Beneficiary (institutional/constrained)
 *   - international_atomic_energy_agency: Agenda setter (institutional/constrained)
 *   - civil_society_disarmament_advocates: Excluded (organized/mobile)
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
narrative_ontology:constraint_metric(npt_article_iv_vi_pairing__grand_bargain, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(npt_article_iv_vi_pairing__grand_bargain, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(npt_article_iv_vi_pairing__grand_bargain, tangled_rope).
narrative_ontology:human_readable(npt_article_iv_vi_pairing__grand_bargain, "NPT Article IV/VI Pairing (Grand Bargain Reading)").
narrative_ontology:topic_domain(npt_article_iv_vi_pairing__grand_bargain, "international_law/nuclear_governance/treaty_interpretation").

domain_priors:requires_active_enforcement(npt_article_iv_vi_pairing__grand_bargain).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(npt_article_iv_vi_pairing__grand_bargain, '10e7b460-7ec8-49b1-993e-be1a08f35c9e').
narrative_ontology:cs_kernel_codification('10e7b460-7ec8-49b1-993e-be1a08f35c9e', fixed_text).
narrative_ontology:cs_authority_grounding('10e7b460-7ec8-49b1-993e-be1a08f35c9e', lineage).
narrative_ontology:cs_interpretation_layer_present('10e7b460-7ec8-49b1-993e-be1a08f35c9e').
narrative_ontology:cs_reading_relation('10e7b460-7ec8-49b1-993e-be1a08f35c9e', npt_article_iv_vi_pairing__nonproliferation_primary, coexists_with).
narrative_ontology:cs_reading_relation('10e7b460-7ec8-49b1-993e-be1a08f35c9e', npt_article_iv_vi_pairing__abolitionist, coexists_with).
narrative_ontology:cs_axiom('10e7b460-7ec8-49b1-993e-be1a08f35c9e', foundational, reciprocal_obligation_of_articles_iv_vi).
narrative_ontology:cs_axiom_status(reciprocal_obligation_of_articles_iv_vi, holdable).
narrative_ontology:cs_axiom_grounding('10e7b460-7ec8-49b1-993e-be1a08f35c9e', reciprocal_obligation_of_articles_iv_vi, deontological).
narrative_ontology:cs_axiom('10e7b460-7ec8-49b1-993e-be1a08f35c9e', foundational, disarmament_progress_conditions_nonproliferation).
narrative_ontology:cs_axiom_status(disarmament_progress_conditions_nonproliferation, holdable).
narrative_ontology:cs_axiom_grounding('10e7b460-7ec8-49b1-993e-be1a08f35c9e', disarmament_progress_conditions_nonproliferation, conventional).
narrative_ontology:cs_reference_frame('10e7b460-7ec8-49b1-993e-be1a08f35c9e', original_npt_grand_bargain_intent).
narrative_ontology:cs_drift_state('10e7b460-7ec8-49b1-993e-be1a08f35c9e', contemporary_npt_review_cycle, gap(authority_erosion, substantial, true)).
narrative_ontology:cs_created_at('10e7b460-7ec8-49b1-993e-be1a08f35c9e', '').
narrative_ontology:cs_kernel_id(npt_article_iv_vi_pairing__grand_bargain, npt_article_iv_vi_pairing).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(npt_article_iv_vi_pairing__grand_bargain, non_nuclear_weapon_states_grand_bargain).
narrative_ontology:constraint_beneficiary(npt_article_iv_vi_pairing__grand_bargain, international_nonproliferation_regime).
narrative_ontology:constraint_victim(npt_article_iv_vi_pairing__grand_bargain, nuclear_weapon_states_grand_bargain).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These states uphold their non-proliferation commitments (Article IV) on the understanding that nuclear weapon states (NWS) will pursue disarmament (Article VI). They benefit from the security assurances and peaceful nuclear technology access, but their restraint is conditional on NWS progress. Failure of NWS to disarm could lead to withdrawal from the NPT or expansion of their own nuclear programs.
narrative_ontology:constraint_stakeholder(npt_article_iv_vi_pairing__grand_bargain, non_nuclear_weapon_states_grand_bargain, beneficiary,
    organized, generational, constrained, global).

% These states are obligated under Article VI to pursue nuclear disarmament in good faith. They benefit from the non-proliferation commitments of NNWS (Article IV) but face increasing pressure and legitimacy challenges due to perceived lack of progress on their own disarmament obligations. Their 'payment' is the political and diplomatic cost of non-compliance and the eventual requirement to disarm.
narrative_ontology:constraint_stakeholder(npt_article_iv_vi_pairing__grand_bargain, nuclear_weapon_states_grand_bargain, payer,
    institutional, generational, constrained, global).

% The regime as a whole benefits from the stability and predictability offered by the NPT's framework. However, its legitimacy is increasingly challenged by the perceived imbalance between Article IV and Article VI, leading to calls for reform or alternative treaties. The grand bargain reading seeks to restore this balance.
narrative_ontology:constraint_stakeholder(npt_article_iv_vi_pairing__grand_bargain, international_nonproliferation_regime, beneficiary,
    institutional, civilizational, constrained, global).

% The IAEA verifies compliance with Article IV (safeguards) but has no direct enforcement mechanism for Article VI. Its role is to facilitate peaceful nuclear cooperation and verify non-diversion, but the political context of the grand bargain reading directly impacts its operational environment and the perceived fairness of the regime it underpins.
narrative_ontology:constraint_stakeholder(npt_article_iv_vi_pairing__grand_bargain, international_atomic_energy_agency, agenda_setter,
    institutional, generational, constrained, global).

% Advocate for full and verifiable nuclear disarmament, often pushing for stronger interpretations of Article VI and criticizing NWS for non-compliance. While not direct parties to the NPT, their advocacy influences NNWS positions and international diplomatic pressure.
narrative_ontology:constraint_stakeholder(npt_article_iv_vi_pairing__grand_bargain, civil_society_disarmament_advocates, excluded,
    organized, generational, mobile, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates global nuclear governance by establishing a framework where non-nuclear weapon states forgo nuclear weapons in exchange for security assurances, peaceful nuclear technology, and a commitment from nuclear weapon states to disarm.
% TRANSFER_FUNCTION: Transfers security benefits (reduced proliferation risk) and access to peaceful nuclear technology to non-nuclear weapon states, in exchange for their commitment not to acquire nuclear weapons. It transfers a disarmament obligation to nuclear weapon states, which they have largely deferred.
% ABSENT_VOICES: States that have not joined the NPT (e.g., India, Pakistan, Israel, North Korea) are absent, as are those advocating for immediate, unconditional disarmament (e.g., TPNW signatories). They would argue the NPT's grand bargain has failed and a new, more equitable framework is needed.
% DISAPPEARANCE_RATIONALE: If the NPT's grand bargain interpretation vanished, the foundational legitimacy of the treaty would collapse. Non-nuclear weapon states would lose their primary justification for restraint, potentially leading to widespread proliferation. Nuclear weapon states would lose the legal and political basis for demanding non-proliferation from others, leading to a highly unstable global security environment.
% FOUNDING_PROBLEM: The NPT was established to prevent the spread of nuclear weapons, promote peaceful uses of nuclear energy, and achieve nuclear disarmament, addressing the existential threat of nuclear war.
% FOUNDING_PROBLEM_CORROBORATION: Non-nuclear weapon states and many international legal scholars attest that the founding problem of disarmament remains live and unaddressed by NWS. Nuclear weapon states, while acknowledging the disarmament goal, often prioritize non-proliferation and strategic stability, arguing the problem is being managed. Independent analyses from UN bodies and academic institutions corroborate the ongoing tension and lack of NWS disarmament progress.
narrative_ontology:disappearance_verdict(npt_article_iv_vi_pairing__grand_bargain, world_rearranges).
narrative_ontology:founding_problem_status(npt_article_iv_vi_pairing__grand_bargain, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(npt_article_iv_vi_pairing__grand_bargain, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
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
 *   The extractiveness (0.65) is high because NWS benefit from NNWS restraint without fully delivering on their own disarmament commitments, creating an asymmetric burden. Suppression (0.70) is also high, as NNWS face significant political and security costs for withdrawing from the NPT or developing nuclear weapons, despite NWS non-compliance. The theater ratio (0.40) reflects the performative aspects of disarmament negotiations that often lack substantive progress. Accessibility collapse (0.45) is moderate, as alternatives like the TPNW exist but face significant NWS opposition. Resistance (0.75) is high, driven by NNWS and civil society pushing for greater NWS accountability.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of non-nuclear weapon states, the constraint is increasingly extractive due to the perceived failure of NWS to uphold their end of the bargain. From the NWS perspective, the constraint is a necessary framework for global stability, with disarmament being a long-term, complex process. The engine's per-seat classification will reflect this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Non-nuclear weapon states are beneficiaries of the non-proliferation aspect but payers of the deferred disarmament. Nuclear weapon states are beneficiaries of non-proliferation but targets of the disarmament obligation. The international regime is a beneficiary of stability but also a payer in terms of legitimacy erosion. The IAEA is an agenda-setter for verification but constrained by the political will of member states.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading highlights a potential mandatrophy: the original mandate for reciprocal disarmament is eroding due to NWS inaction, but the constraint (NNWS non-proliferation) persists. The classification as a Tangled Rope reflects this hybrid function: it coordinates non-proliferation but extracts from NNWS by deferring the disarmament obligation. Resolving this mandatrophy would require concrete, verifiable NWS disarmament steps to rebalance the 'grand bargain'.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    npt_grand_bargain_legitimacy,
    'Is the NPT''s ''grand bargain'' still a legitimate basis for global nuclear governance, given the lack of NWS disarmament progress?',
    'A UN General Assembly resolution explicitly reaffirming or reinterpreting the reciprocal nature of Articles IV and VI, or a significant, verifiable reduction in NWS arsenals.',
    'If deemed illegitimate, NNWS might withdraw from the NPT or pursue nuclear weapons, leading to a collapse of the non-proliferation regime. If reaffirmed by NWS action, the constraint''s extractiveness would decrease, and its classification might shift towards a Rope.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(npt_grand_bargain_legitimacy, conceptual, 'The core legitimacy of the NPT''s reciprocal obligations.').

omega_variable(
    disarmament_verifiability,
    'Are current verification technologies and political mechanisms sufficient to ensure verifiable nuclear disarmament by NWS?',
    'Development and implementation of robust, intrusive, and universally accepted verification regimes for nuclear disarmament, including warhead dismantlement.',
    'If verifiable disarmament is technically and politically feasible, NWS arguments for deferring disarmament would weaken, increasing pressure on them and potentially reducing the constraint''s extractiveness. If not, the ''grand bargain'' remains structurally difficult to fulfill.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(disarmament_verifiability, empirical, 'Feasibility of verifying NWS disarmament.').

omega_variable(
    sibling_reading_nonproliferation_primary_impact,
    'How would the ''nonproliferation_primary'' reading''s emphasis on Article IV''s independence from Article VI affect the grand_bargain reading''s enforceability?',
    'Analysis of NPT Review Conference outcomes and NWS statements: if the ''nonproliferation_primary'' reading gains dominant diplomatic traction, the ''grand_bargain'' reading''s claims of reciprocity would be marginalized.',
    'If the ''nonproliferation_primary'' reading becomes dominant, the ''grand_bargain'' reading''s claims of NWS treaty breach would lose political force, making Article VI effectively non-justiciable and increasing the extractiveness on NNWS.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_reading_nonproliferation_primary_impact, conceptual, 'Impact of the ''nonproliferation_primary'' reading on the ''grand_bargain'' reading''s enforceability.').

omega_variable(
    sibling_reading_abolitionist_impact,
    'How would the ''abolitionist'' reading''s call for immediate, unconditional disarmament affect the ''grand_bargain'' reading''s incremental approach?',
    'Analysis of the political momentum behind the Treaty on the Prohibition of Nuclear Weapons (TPNW) and its influence on NPT states parties.',
    'If the ''abolitionist'' reading gains significant traction, it could either pressure NWS to accelerate disarmament (benefiting the ''grand_bargain'' reading) or lead to a schism in the international community, further undermining the NPT''s authority.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_abolitionist_impact, conceptual, 'Impact of the ''abolitionist'' reading on the ''grand_bargain'' reading''s incremental approach.').


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
narrative_ontology:measurement(npt__su_t1985, npt_article_iv_vi_pairing__grand_bargain, suppression_requirement, 1985, 0.58).
narrative_ontology:measurement(npt__su_t2000, npt_article_iv_vi_pairing__grand_bargain, suppression_requirement, 2000, 0.65).
narrative_ontology:measurement(npt__su_t2010, npt_article_iv_vi_pairing__grand_bargain, suppression_requirement, 2010, 0.68).
narrative_ontology:measurement(npt__su_t2024, npt_article_iv_vi_pairing__grand_bargain, suppression_requirement, 2024, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(npt_article_iv_vi_pairing__grand_bargain, enforcement_mechanism).
narrative_ontology:affects_constraint(npt_article_iv_vi_pairing__grand_bargain, npt_article_iv_vi_pairing__nonproliferation_primary).
narrative_ontology:affects_constraint(npt_article_iv_vi_pairing__grand_bargain, npt_article_iv_vi_pairing__abolitionist).
narrative_ontology:affects_constraint(npt_article_iv_vi_pairing__grand_bargain, nuclear_weapons_prohibition_treaty_legitimacy).

% DUAL FORMULATION NOTE:
% This constraint is one reading ('grand_bargain') of the NPT Article IV/VI pairing kernel. It is structurally distinct from the 'nonproliferation_primary' and 'abolitionist' readings, which emphasize different aspects of the treaty's obligations and have different beneficiary/victim structures. All three are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
