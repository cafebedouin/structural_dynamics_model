% ============================================================================
% CONSTRAINT STORY: npt_treaty_text__nws_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_npt_treaty_text__nws_reading, []).

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
    narrative_ontology:stakeholder_non_agent/2,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: npt_treaty_text__nws_reading
 *   human_readable: NPT Non-Proliferation (NWS Reading)
 *   domain: international_law/arms_control/treaty_interpretation
 *
 * SUMMARY:
 *   This constraint represents the Nuclear Weapon States' (NWS) dominant
 *   interpretation of the Nuclear Non-Proliferation Treaty (NPT). Under this
 *   reading, non-proliferation obligations for Non-Nuclear Weapon States
 *   (NNWS) are strictly binding and actively enforced, while NWS disarmament
 *   commitments under Article VI are treated as an aspirational, long-term
 *   goal without a concrete timeline or enforcement. This interpretation
 *   allows NWS to maintain their nuclear arsenals and strategic advantage,
 *   while imposing significant constraints and verification burdens on NNWS.
 *   The constraint is a reading of the 'npt_treaty_text' kernel.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(npt_treaty_text__nws_reading, 0.78).
domain_priors:suppression_score(npt_treaty_text__nws_reading, 0.85).
domain_priors:theater_ratio(npt_treaty_text__nws_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(npt_treaty_text__nws_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(npt_treaty_text__nws_reading, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(npt_treaty_text__nws_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(npt_treaty_text__nws_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(npt_treaty_text__nws_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(npt_treaty_text__nws_reading, tangled_rope).
narrative_ontology:human_readable(npt_treaty_text__nws_reading, "NPT Non-Proliferation (NWS Reading)").
narrative_ontology:topic_domain(npt_treaty_text__nws_reading, "international_law/arms_control/treaty_interpretation").

domain_priors:requires_active_enforcement(npt_treaty_text__nws_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(npt_treaty_text__nws_reading, 'e751fbc5-7f27-4b3b-82a2-9b92f6fb9efa').
narrative_ontology:cs_kernel_codification('e751fbc5-7f27-4b3b-82a2-9b92f6fb9efa', fixed_text).
narrative_ontology:cs_authority_grounding('e751fbc5-7f27-4b3b-82a2-9b92f6fb9efa', lineage).
narrative_ontology:cs_interpretation_layer_present('e751fbc5-7f27-4b3b-82a2-9b92f6fb9efa').
narrative_ontology:cs_reading_relation('e751fbc5-7f27-4b3b-82a2-9b92f6fb9efa', npt_treaty_text__nnws_reading, coexists_with).
narrative_ontology:cs_reading_relation('e751fbc5-7f27-4b3b-82a2-9b92f6fb9efa', npt_treaty_text__withdrawal_threshold_reading, influences).
narrative_ontology:cs_axiom('e751fbc5-7f27-4b3b-82a2-9b92f6fb9efa', foundational, horizontal_proliferation_is_primary_threat).
narrative_ontology:cs_axiom_status(horizontal_proliferation_is_primary_threat, holdable).
narrative_ontology:cs_axiom_grounding('e751fbc5-7f27-4b3b-82a2-9b92f6fb9efa', horizontal_proliferation_is_primary_threat, empirically_contingent).
narrative_ontology:cs_axiom('e751fbc5-7f27-4b3b-82a2-9b92f6fb9efa', foundational, nws_disarmament_is_long_term_aspiration).
narrative_ontology:cs_axiom_status(nws_disarmament_is_long_term_aspiration, holdable).
narrative_ontology:cs_axiom_grounding('e751fbc5-7f27-4b3b-82a2-9b92f6fb9efa', nws_disarmament_is_long_term_aspiration, conventional).
narrative_ontology:cs_reference_frame('e751fbc5-7f27-4b3b-82a2-9b92f6fb9efa', npt_grand_bargain_as_asymmetric_stability).
narrative_ontology:cs_drift_state('e751fbc5-7f27-4b3b-82a2-9b92f6fb9efa', contemporary_npt_review_cycle, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('e751fbc5-7f27-4b3b-82a2-9b92f6fb9efa', '').
narrative_ontology:cs_kernel_id(npt_treaty_text__nws_reading, npt_treaty_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(npt_treaty_text__nws_reading, nuclear_weapon_states).
narrative_ontology:constraint_victim(npt_treaty_text__nws_reading, non_nuclear_weapon_states).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interpret the NPT to prioritize non-proliferation by NNWS as a binding, enforceable obligation, while treating their own disarmament commitments under Article VI as an aspirational, long-term goal without a fixed timeline or enforcement mechanism. They benefit from maintaining their nuclear monopoly and control over the international security architecture.
narrative_ontology:constraint_stakeholder(npt_treaty_text__nws_reading, nuclear_weapon_states, agenda_setter,
    institutional, generational, arbitrage, global).

% Are bound by strict non-proliferation obligations, including IAEA safeguards and inspections, which limit their sovereign choices regarding nuclear technology. They bear the costs of verification and forgo nuclear weapons development, often without seeing reciprocal disarmament by NWS.
narrative_ontology:constraint_stakeholder(npt_treaty_text__nws_reading, non_nuclear_weapon_states, payer,
    organized, generational, constrained, global).

% Administers and enforces the safeguards system on NNWS, verifying compliance with non-proliferation commitments. Its budget and mandate are heavily focused on horizontal proliferation, reflecting the NWS reading's priorities. It operates under the political influence of its member states, particularly the NWS.
narrative_ontology:constraint_stakeholder(npt_treaty_text__nws_reading, international_atomic_energy_agency, agenda_setter,
    institutional, civilizational, constrained, global).

% The abstract system of norms, treaties, and institutions that aims to prevent the spread of nuclear weapons. This reading benefits the regime by reinforcing its non-proliferation pillar, even if it weakens the disarmament pillar.
narrative_ontology:constraint_stakeholder(npt_treaty_text__nws_reading, global_non_proliferation_regime, beneficiary,
    institutional, civilizational, identity_locked, universal).
narrative_ontology:stakeholder_non_agent(npt_treaty_text__nws_reading, global_non_proliferation_regime).

% Argue for a balanced interpretation of the NPT, emphasizing the binding nature of Article VI disarmament obligations. Their calls for NWS compliance are often marginalized in forums dominated by the non-proliferation agenda.
narrative_ontology:constraint_stakeholder(npt_treaty_text__nws_reading, disarmament_advocates, excluded,
    moderate, generational, constrained, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates international efforts to prevent the spread of nuclear weapons to additional states, thereby reducing the risk of nuclear conflict and maintaining a stable, albeit asymmetric, global security order.
% TRANSFER_FUNCTION: Transfers the burden of nuclear restraint and verification costs from nuclear-weapon states to non-nuclear-weapon states, in exchange for a promise of eventual disarmament by NWS and access to peaceful nuclear technology.
% ABSENT_VOICES: Non-nuclear-weapon states advocating for a more robust interpretation of Article VI (disarmament) and civil society disarmament movements are often excluded from the core decision-making processes that shape the NPT's operational interpretation. They would argue that the NWS reading undermines the treaty's grand bargain.
% DISAPPEARANCE_RATIONALE: If this interpretation vanished, the NPT regime would likely collapse, leading to a rapid proliferation of nuclear weapons as NNWS pursue their own security interests, fundamentally altering the global security landscape.
% FOUNDING_PROBLEM: The existential threat of nuclear war due to the unchecked spread of nuclear weapons, coupled with the desire to maintain a stable international order by limiting the number of nuclear-armed states.
% FOUNDING_PROBLEM_CORROBORATION: The NWS consistently attest that the threat of proliferation remains live, justifying the continued emphasis on NNWS non-proliferation. Many NNWS and independent analysts, while acknowledging proliferation risks, corroborate that the NWS's failure to disarm fuels these risks, making the problem's status 'contested' from a balanced perspective.
narrative_ontology:disappearance_verdict(npt_treaty_text__nws_reading, world_rearranges).
narrative_ontology:founding_problem_status(npt_treaty_text__nws_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(npt_treaty_text__nws_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(npt_treaty_text__nws_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(npt_treaty_text__nws_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(npt_treaty_text__nws_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(npt_treaty_text__nws_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The high extractiveness (0.78) reflects the asymmetric burden placed on NNWS, who forgo nuclear weapons while NWS retain and modernize theirs. Suppression (0.85) is high due to the robust IAEA safeguards regime and the severe international consequences for NNWS perceived to be violating non-proliferation norms. The theater ratio (0.45) indicates that while some disarmament efforts are genuine, a significant portion of NWS rhetoric and activity around Article VI serves to legitimize their continued nuclear possession rather than genuinely pursuing disarmament. The increasing trend in extractiveness and suppression over time reflects the hardening of the non-proliferation pillar and the continued deferral of disarmament.
 *
 * PERSPECTIVAL GAP:
 *   From the NWS perspective, this interpretation is a necessary 'rope' for global stability, preventing nuclear chaos. From the NNWS perspective, it operates as a 'snare' or 'tangled_rope,' extracting their sovereign right to nuclear technology (even for peaceful purposes, under strict safeguards) without reciprocal NWS compliance. The engine's computation will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Nuclear Weapon States are clear beneficiaries and agenda-setters (d near 0.0), as this reading preserves their nuclear monopoly and global influence. Non-Nuclear Weapon States are the primary targets (d near 1.0), bearing the costs of non-proliferation without receiving the promised disarmament. The IAEA, while a formal enforcer, is structurally constrained to prioritize horizontal proliferation verification, aligning with the NWS reading's agenda. The global non-proliferation regime itself benefits from this interpretation's success in preventing further proliferation, even at the cost of disarmament.
 *
 * MANDATROPHY ANALYSIS:
 *   The NWS reading prevents mislabeling the NPT as a pure 'rope' by exposing the asymmetric extraction inherent in the interpretation. It highlights how the 'coordination' of non-proliferation is coupled with the 'extraction' of disarmament deferral, making it a 'tangled_rope.' The founding problem of preventing proliferation is still live, but the 'solution' has evolved to disproportionately benefit one set of actors, indicating a drift towards extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    article_vi_bindingness,
    'Is Article VI of the NPT, which commits NWS to disarmament, a legally binding obligation with a clear timeline, or an aspirational goal?',
    'International Court of Justice advisory opinion on the legal force and timeline of Article VI, or a new NPT review conference outcome explicitly defining ''at an early date''.',
    'If found binding with a timeline, the NWS reading''s extractiveness would be reclassified as higher, and its suppression of NNWS concerns would be more evident. If confirmed as purely aspirational, the NWS reading would be more consistent with a ''rope'' from their perspective, but still extractive from the NNWS perspective.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(article_vi_bindingness, conceptual, 'Ambiguity of NWS disarmament obligations under NPT Article VI.').

omega_variable(
    iaea_mandate_balance,
    'Is the IAEA''s mandate and budget genuinely balanced between non-proliferation verification and promoting peaceful nuclear uses, or is it disproportionately focused on horizontal proliferation due to NWS influence?',
    'Independent audit of IAEA budget allocation and program priorities, comparing resources dedicated to safeguards versus technical cooperation and disarmament verification support.',
    'If disproportionate, it would confirm the NWS reading''s structural influence on the IAEA, increasing the perceived suppression and extractiveness of the overall regime. If balanced, it would suggest the NWS reading is less structurally embedded in the IAEA''s operational priorities.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(iaea_mandate_balance, empirical, 'Balance of IAEA mandate between non-proliferation and peaceful uses/disarmament.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(npt_treaty_text__nws_reading, 1970, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(npt__tr_t1970, npt_treaty_text__nws_reading, theater_ratio, 1970, 0.2).
narrative_ontology:measurement(npt__tr_t1980, npt_treaty_text__nws_reading, theater_ratio, 1980, 0.25).
narrative_ontology:measurement(npt__tr_t1990, npt_treaty_text__nws_reading, theater_ratio, 1990, 0.3).
narrative_ontology:measurement(npt__tr_t2000, npt_treaty_text__nws_reading, theater_ratio, 2000, 0.35).
narrative_ontology:measurement(npt__tr_t2010, npt_treaty_text__nws_reading, theater_ratio, 2010, 0.4).
narrative_ontology:measurement(npt__tr_t2024, npt_treaty_text__nws_reading, theater_ratio, 2024, 0.45).

% Extraction over time
narrative_ontology:measurement(npt__be_t1970, npt_treaty_text__nws_reading, base_extractiveness, 1970, 0.6).
narrative_ontology:measurement(npt__be_t1980, npt_treaty_text__nws_reading, base_extractiveness, 1980, 0.65).
narrative_ontology:measurement(npt__be_t1990, npt_treaty_text__nws_reading, base_extractiveness, 1990, 0.7).
narrative_ontology:measurement(npt__be_t2000, npt_treaty_text__nws_reading, base_extractiveness, 2000, 0.73).
narrative_ontology:measurement(npt__be_t2010, npt_treaty_text__nws_reading, base_extractiveness, 2010, 0.76).
narrative_ontology:measurement(npt__be_t2024, npt_treaty_text__nws_reading, base_extractiveness, 2024, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(npt__su_t1970, npt_treaty_text__nws_reading, suppression_requirement, 1970, 0.7).
narrative_ontology:measurement(npt__su_t1980, npt_treaty_text__nws_reading, suppression_requirement, 1980, 0.75).
narrative_ontology:measurement(npt__su_t1990, npt_treaty_text__nws_reading, suppression_requirement, 1990, 0.8).
narrative_ontology:measurement(npt__su_t2000, npt_treaty_text__nws_reading, suppression_requirement, 2000, 0.82).
narrative_ontology:measurement(npt__su_t2010, npt_treaty_text__nws_reading, suppression_requirement, 2010, 0.84).
narrative_ontology:measurement(npt__su_t2024, npt_treaty_text__nws_reading, suppression_requirement, 2024, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(npt_treaty_text__nws_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(npt_treaty_text__nws_reading, npt_treaty_text__nnws_reading).
narrative_ontology:affects_constraint(npt_treaty_text__nws_reading, npt_treaty_text__withdrawal_threshold_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the NPT treaty text kernel, focusing on the NWS interpretation of non-proliferation and disarmament obligations. It is linked to other readings that emphasize different aspects of the treaty.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
