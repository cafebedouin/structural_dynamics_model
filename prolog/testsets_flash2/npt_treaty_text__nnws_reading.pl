% ============================================================================
% CONSTRAINT STORY: npt_treaty_text__nnws_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_npt_treaty_text__nnws_reading, []).

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
 *   constraint_id: npt_treaty_text__nnws_reading
 *   human_readable: NPT Article VI Disarmament Obligation (NNWS Reading)
 *   domain: international_law/arms_control/treaty_interpretation
 *
 * SUMMARY:
 *   This constraint represents the Non-Nuclear Weapon States' (NNWS) reading
 *   of the Nuclear Non-Proliferation Treaty (NPT), specifically Article VI,
 *   which they interpret as a binding obligation for Nuclear Weapon States
 *   (NWS) to pursue disarmament. Their non-proliferation commitments are seen
 *   as conditional on NWS compliance. This reading generates a 'rope'
 *   classification due to its genuine coordination function and the
 *   collective action problem it addresses, but with moderate extractiveness
 *   as NWS resist the disarmament obligation. This is one reading of the
 *   'npt_treaty_text' kernel.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(npt_treaty_text__nnws_reading, 0.35).
domain_priors:suppression_score(npt_treaty_text__nnws_reading, 0.45).
domain_priors:theater_ratio(npt_treaty_text__nnws_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(npt_treaty_text__nnws_reading, extractiveness, 0.35).
narrative_ontology:constraint_metric(npt_treaty_text__nnws_reading, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(npt_treaty_text__nnws_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(npt_treaty_text__nnws_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(npt_treaty_text__nnws_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(npt_treaty_text__nnws_reading, rope).
narrative_ontology:human_readable(npt_treaty_text__nnws_reading, "NPT Article VI Disarmament Obligation (NNWS Reading)").
narrative_ontology:topic_domain(npt_treaty_text__nnws_reading, "international_law/arms_control/treaty_interpretation").

domain_priors:requires_active_enforcement(npt_treaty_text__nnws_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(npt_treaty_text__nnws_reading, '2b4da9f2-636f-40cc-9e33-db0beac0e885').
narrative_ontology:cs_kernel_codification('2b4da9f2-636f-40cc-9e33-db0beac0e885', fixed_text).
narrative_ontology:cs_authority_grounding('2b4da9f2-636f-40cc-9e33-db0beac0e885', lineage).
narrative_ontology:cs_interpretation_layer_present('2b4da9f2-636f-40cc-9e33-db0beac0e885').
narrative_ontology:cs_reading_relation('2b4da9f2-636f-40cc-9e33-db0beac0e885', npt_treaty_text__nws_reading, coexists_with).
narrative_ontology:cs_reading_relation('2b4da9f2-636f-40cc-9e33-db0beac0e885', npt_treaty_text__withdrawal_threshold_reading, influences).
narrative_ontology:cs_axiom('2b4da9f2-636f-40cc-9e33-db0beac0e885', foundational, article_vi_binding_disarmament_obligation).
narrative_ontology:cs_axiom_status(article_vi_binding_disarmament_obligation, holdable).
narrative_ontology:cs_axiom_grounding('2b4da9f2-636f-40cc-9e33-db0beac0e885', article_vi_binding_disarmament_obligation, deontological).
narrative_ontology:cs_axiom('2b4da9f2-636f-40cc-9e33-db0beac0e885', secondary, nnws_non_proliferation_conditional_on_nws_disarmament).
narrative_ontology:cs_axiom_status(nnws_non_proliferation_conditional_on_nws_disarmament, holdable).
narrative_ontology:cs_axiom_grounding('2b4da9f2-636f-40cc-9e33-db0beac0e885', nnws_non_proliferation_conditional_on_nws_disarmament, conventional).
narrative_ontology:cs_reference_frame('2b4da9f2-636f-40cc-9e33-db0beac0e885', original_npt_bargain_integrity).
narrative_ontology:cs_drift_state('2b4da9f2-636f-40cc-9e33-db0beac0e885', contemporary_npt_review_conferences, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('2b4da9f2-636f-40cc-9e33-db0beac0e885', '').
narrative_ontology:cs_kernel_id(npt_treaty_text__nnws_reading, npt_treaty_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(npt_treaty_text__nnws_reading, non_nuclear_weapon_states).
narrative_ontology:constraint_beneficiary(npt_treaty_text__nnws_reading, global_security_advocates).
narrative_ontology:constraint_victim(npt_treaty_text__nnws_reading, nuclear_weapon_states).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These states interpret Article VI as a binding obligation for NWS to disarm, viewing their own non-proliferation commitments as conditional on NWS progress. They exert pressure through NPT Review Conferences and support alternative regimes like the TPNW.
narrative_ontology:constraint_stakeholder(npt_treaty_text__nnws_reading, non_nuclear_weapon_states, agenda_setter,
    organized, generational, constrained, global).

% These states are the primary targets of the disarmament obligation under this reading. They face pressure to reduce their arsenals and are seen as bearing the costs of compliance, which they often resist by reinterpreting Article VI as aspirational rather than binding.
narrative_ontology:constraint_stakeholder(npt_treaty_text__nnws_reading, nuclear_weapon_states, payer,
    institutional, generational, constrained, global).

% These groups benefit from any progress towards disarmament, seeing it as enhancing global security and reducing existential risk. They actively lobby NNWS and international bodies to uphold this interpretation.
narrative_ontology:constraint_stakeholder(npt_treaty_text__nnws_reading, global_security_advocates, beneficiary,
    moderate, civilizational, mobile, global).

% The IAEA monitors non-proliferation commitments but does not directly enforce disarmament. It provides technical assessments that inform the debate around NPT compliance, but its mandate is distinct from the disarmament obligation itself.
narrative_ontology:constraint_stakeholder(npt_treaty_text__nnws_reading, international_atomic_energy_agency, observer,
    institutional, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the expectation that non-nuclear states will forgo nuclear weapons in exchange for a commitment from nuclear states to pursue disarmament, aiming for a world free of nuclear weapons.
% TRANSFER_FUNCTION: Transfers the obligation to disarm from the realm of aspirational policy to a binding legal commitment, placing the burden of action on NWS while NNWS maintain their non-proliferation status.
% ABSENT_VOICES: States that have withdrawn from the NPT or never joined, and those pursuing nuclear weapons outside the treaty, are absent from this interpretive debate but their actions undermine the treaty's authority.
% DISAPPEARANCE_RATIONALE: If this interpretation vanished, NNWS would lose a key legal and moral lever to pressure NWS, potentially leading to increased proliferation incentives and a breakdown of the NPT regime, fundamentally altering global security architecture.
% FOUNDING_PROBLEM: The original problem was to prevent the spread of nuclear weapons while acknowledging the existing nuclear powers, with a promise of eventual disarmament to ensure long-term stability.
% FOUNDING_PROBLEM_CORROBORATION: NNWS and global security advocates consistently attest that the problem of nuclear proliferation and the lack of disarmament progress remain live and urgent. NWS often acknowledge the problem of proliferation but contest the urgency or binding nature of their disarmament obligations.
narrative_ontology:disappearance_verdict(npt_treaty_text__nnws_reading, world_rearranges).
narrative_ontology:founding_problem_status(npt_treaty_text__nnws_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(npt_treaty_text__nnws_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(npt_treaty_text__nnws_reading, 'none', 1).
narrative_ontology:epsilon_provenance(npt_treaty_text__nnws_reading, 0.35, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(npt_treaty_text__nnws_reading_tests).
:- end_tests(npt_treaty_text__nnws_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.35) is moderate because while NWS face a clear obligation, the enforcement mechanisms are weak, relying on diplomatic pressure and the threat of NNWS withdrawal or non-compliance. Suppression (0.45) reflects the diplomatic and political pressure NNWS can exert, but also the NWS's ability to resist. Theater ratio (0.20) is low, as the NNWS's advocacy for disarmament is a genuine effort, not merely performative. The claimed type is 'rope' because it aims to solve a collective action problem (preventing proliferation and achieving disarmament) with net benefits for participants (global security), even if the NWS experience it as extractive.
 *
 * PERSPECTIVAL GAP:
 *   From the NNWS perspective, this is a legitimate and necessary constraint for global security, a true 'rope'. From the NWS perspective, it is often seen as an overreach, an attempt to impose an unfeasible or strategically disadvantageous obligation, making it feel more like a 'snare' or 'tangled_rope' due to the perceived extraction without sufficient coordination benefit for them.
 *
 * DIRECTIONALITY LOGIC:
 *   NNWS are the primary beneficiaries and agenda-setters, as this reading empowers their diplomatic efforts and frames their non-proliferation as a conditional exchange. NWS are the primary targets/payers, as they bear the obligation to disarm. Global security advocates are also beneficiaries, aligning with the NNWS position. The IAEA is an observer, focused on verification rather than the disarmament obligation itself.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    binding_vs_aspirational_disarmament,
    'Is Article VI of the NPT a binding legal obligation for NWS to disarm, or an aspirational long-term goal?',
    'International Court of Justice advisory opinion or a new, universally ratified treaty explicitly clarifying the legal status of Article VI.',
    'If binding, the NNWS reading is strengthened, increasing pressure on NWS. If aspirational, the NWS reading gains legitimacy, weakening the disarmament agenda and potentially leading to NNWS withdrawal.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(binding_vs_aspirational_disarmament, conceptual, 'Ambiguity in the legal interpretation of NPT Article VI.').

omega_variable(
    tpnw_impact_on_npt,
    'Does the Treaty on the Prohibition of Nuclear Weapons (TPNW) strengthen or weaken the NPT regime, particularly the NNWS reading of Article VI?',
    'Empirical analysis of NPT Review Conference outcomes and NWS behavior in response to TPNW ratification trends over the next decade.',
    'If TPNW strengthens, it provides a new lever for NNWS to enforce disarmament. If it weakens, it could create a schism, undermining the NPT''s authority and the NNWS''s leverage.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(tpnw_impact_on_npt, empirical, 'Impact of the TPNW on the NPT''s disarmament pillar.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(npt_treaty_text__nnws_reading, 1970, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(npt__tr_t1970, npt_treaty_text__nnws_reading, theater_ratio, 1970, 0.1).
narrative_ontology:measurement(npt__tr_t1985, npt_treaty_text__nnws_reading, theater_ratio, 1985, 0.15).
narrative_ontology:measurement(npt__tr_t2000, npt_treaty_text__nnws_reading, theater_ratio, 2000, 0.18).
narrative_ontology:measurement(npt__tr_t2015, npt_treaty_text__nnws_reading, theater_ratio, 2015, 0.19).
narrative_ontology:measurement(npt__tr_t2024, npt_treaty_text__nnws_reading, theater_ratio, 2024, 0.2).

% Extraction over time
narrative_ontology:measurement(npt__be_t1970, npt_treaty_text__nnws_reading, base_extractiveness, 1970, 0.2).
narrative_ontology:measurement(npt__be_t1985, npt_treaty_text__nnws_reading, base_extractiveness, 1985, 0.25).
narrative_ontology:measurement(npt__be_t2000, npt_treaty_text__nnws_reading, base_extractiveness, 2000, 0.3).
narrative_ontology:measurement(npt__be_t2015, npt_treaty_text__nnws_reading, base_extractiveness, 2015, 0.33).
narrative_ontology:measurement(npt__be_t2024, npt_treaty_text__nnws_reading, base_extractiveness, 2024, 0.35).

% Suppression requirement over time
narrative_ontology:measurement(npt__su_t1970, npt_treaty_text__nnws_reading, suppression_requirement, 1970, 0.3).
narrative_ontology:measurement(npt__su_t1985, npt_treaty_text__nnws_reading, suppression_requirement, 1985, 0.35).
narrative_ontology:measurement(npt__su_t2000, npt_treaty_text__nnws_reading, suppression_requirement, 2000, 0.4).
narrative_ontology:measurement(npt__su_t2015, npt_treaty_text__nnws_reading, suppression_requirement, 2015, 0.43).
narrative_ontology:measurement(npt__su_t2024, npt_treaty_text__nnws_reading, suppression_requirement, 2024, 0.45).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(npt_treaty_text__nnws_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(npt_treaty_text__nnws_reading, npt_treaty_text__nws_reading).
narrative_ontology:affects_constraint(npt_treaty_text__nnws_reading, npt_treaty_text__withdrawal_threshold_reading).
narrative_ontology:affects_constraint(npt_treaty_text__nnws_reading, tpnw_regime).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the NPT treaty text kernel, focusing on the NNWS interpretation of Article VI's disarmament obligation. It influences and coexists with the NWS and withdrawal threshold readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
