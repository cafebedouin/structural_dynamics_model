% ============================================================================
% CONSTRAINT STORY: npt_treaty_text__nws_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: npt_treaty_text__nws_reading
 *   human_readable: NPT Read as Binding Non-Proliferation / Aspirational Disarmament (NWS Reading)
 *   domain: international_law/arms_control
 *
 * SUMMARY:
 *   Since 1968, the Nuclear Non-Proliferation Treaty has been administered
 *   under an interpretation in which the treaty's non-proliferation
 *   provisions (Articles I-III) are treated as binding, verified through IAEA
 *   safeguards, and enforced through sanctions and diplomatic pressure
 *   against suspected violators, while the treaty's disarmament provision
 *   (Article VI) is treated as an aspirational statement of intent with no
 *   timetable, no verification regime, and no enforcement mechanism. This
 *   asymmetric reading has persisted across all NPT Review Conferences since
 *   1970, surviving the 1995, 2000, 2010, and 2015 cycles despite repeated
 *   challenges from the Non-Aligned Movement and the 1996 ICJ advisory
 *   opinion characterizing Article VI in stronger terms. The verification and
 *   enforcement infrastructure built around the treaty (IAEA safeguards,
 *   Additional Protocol, export control regimes) has grown substantially in
 *   scope and budget, entirely oriented toward the non-proliferation half of
 *   the bargain.
 *
 * KEY AGENTS:
 *   - nuclear_weapons_states: Primary beneficiary (institutional/arbitrage) — controls interpretive machinery, bears no enforceable Article VI obligation
 *   - non_nuclear_weapons_states: Primary target (moderate/trapped) — bears binding, verified, enforced non-proliferation obligations
 *   - iaea_safeguards_verification_apparatus: Institutional co-beneficiary (institutional/arbitrage) — verification mandate and budget structurally oriented toward NNWS compliance only
 *   - non_aligned_movement_disarmament_advocates: Excluded voice (organized/constrained) — argues binding Article VI interpretation, lacks Security Council leverage to enforce it
 *   - international_court_of_justice: Analytical observer (institutional/analytical) — issued advisory characterization of Article VI as substantive, carries no enforcement power
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(npt_treaty_text__nws_reading, 0.71).
domain_priors:suppression_score(npt_treaty_text__nws_reading, 0.68).
domain_priors:theater_ratio(npt_treaty_text__nws_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(npt_treaty_text__nws_reading, extractiveness, 0.71).
narrative_ontology:constraint_metric(npt_treaty_text__nws_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(npt_treaty_text__nws_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(npt_treaty_text__nws_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(npt_treaty_text__nws_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(npt_treaty_text__nws_reading, tangled_rope).
narrative_ontology:human_readable(npt_treaty_text__nws_reading, "NPT Read as Binding Non-Proliferation / Aspirational Disarmament (NWS Reading)").
narrative_ontology:topic_domain(npt_treaty_text__nws_reading, "international_law/arms_control").

domain_priors:requires_active_enforcement(npt_treaty_text__nws_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(npt_treaty_text__nws_reading, '2c455a78-80b7-4d2e-b17a-127126b9bf32').
narrative_ontology:cs_kernel_codification('2c455a78-80b7-4d2e-b17a-127126b9bf32', fixed_text).
narrative_ontology:cs_authority_grounding('2c455a78-80b7-4d2e-b17a-127126b9bf32', extraction).
narrative_ontology:cs_interpretation_layer_present('2c455a78-80b7-4d2e-b17a-127126b9bf32').
narrative_ontology:cs_reading_relation('2c455a78-80b7-4d2e-b17a-127126b9bf32', npt_treaty_text__nnws_reading, coexists_with).
narrative_ontology:cs_reading_relation('2c455a78-80b7-4d2e-b17a-127126b9bf32', npt_treaty_text__withdrawal_threshold_reading, influences).
narrative_ontology:cs_axiom('2c455a78-80b7-4d2e-b17a-127126b9bf32', foundational, article_vi_states_aspiration_not_obligation).
narrative_ontology:cs_axiom_status(article_vi_states_aspiration_not_obligation, holdable).
narrative_ontology:cs_axiom_grounding('2c455a78-80b7-4d2e-b17a-127126b9bf32', article_vi_states_aspiration_not_obligation, conventional).
narrative_ontology:cs_axiom('2c455a78-80b7-4d2e-b17a-127126b9bf32', secondary, regime_stability_requires_asymmetric_verification_burden).
narrative_ontology:cs_axiom_status(regime_stability_requires_asymmetric_verification_burden, holdable).
narrative_ontology:cs_axiom_grounding('2c455a78-80b7-4d2e-b17a-127126b9bf32', regime_stability_requires_asymmetric_verification_burden, instrumental).
narrative_ontology:cs_reference_frame('2c455a78-80b7-4d2e-b17a-127126b9bf32', cold_war_bargain_equilibrium).
narrative_ontology:cs_drift_state('2c455a78-80b7-4d2e-b17a-127126b9bf32', post_1995_indefinite_extension, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('2c455a78-80b7-4d2e-b17a-127126b9bf32', '').
narrative_ontology:cs_kernel_id(npt_treaty_text__nws_reading, npt_treaty_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(npt_treaty_text__nws_reading, nuclear_weapons_states).
narrative_ontology:constraint_beneficiary(npt_treaty_text__nws_reading, iaea_safeguards_verification_apparatus).
narrative_ontology:constraint_victim(npt_treaty_text__nws_reading, non_nuclear_weapons_states).
narrative_ontology:constraint_victim(npt_treaty_text__nws_reading, non_aligned_movement_disarmament_advocates).
narrative_ontology:constraint_vindicates(npt_treaty_text__nws_reading, nuclear_nonproliferation_regime_stability_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The five NPT-recognized nuclear powers control the treaty's interpretive machinery through UN Security Council permanence, review conference agenda-setting, and control over what counts as compliance with Article VI's 'good faith' negotiation language. They read Article VI as a non-binding aspiration with no timetable, no verification mechanism, and no enforcement, while treating Articles I, II, and III (non-proliferation obligations on NNWS) as binding, verified, and enforced through IAEA safeguards and sanctions regimes. They continue to modernize arsenals while citing NPT membership as evidence of good-faith disarmament commitment.
narrative_ontology:constraint_stakeholder(npt_treaty_text__nws_reading, nuclear_weapons_states, agenda_setter,
    institutional, civilizational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(npt_treaty_text__nws_reading, nuclear_weapons_states, beneficiary).

% Accept intrusive IAEA safeguards, foreclose their own weapons programs, and submit to sanctions threats for suspected violations, in exchange for a disarmament promise that carries no enforcement mechanism and no timetable. Withdrawal under Article X is available in theory but triggers severe diplomatic and economic consequences, making exit largely theoretical. Their bargaining leverage at review conferences is real but has not produced enforceable Article VI obligations in five decades.
narrative_ontology:constraint_stakeholder(npt_treaty_text__nws_reading, non_nuclear_weapons_states, payer,
    moderate, generational, trapped, global).

% Administers verification budget and technical inspection regime that is overwhelmingly directed at confirming NNWS compliance with non-proliferation obligations; has no comparable mandate or budget line to verify NWS progress toward disarmament, because no such verification obligation was ever codified as binding. Its institutional continuity and funding depend on the horizontal proliferation problem remaining the treaty's operative concern.
narrative_ontology:constraint_stakeholder(npt_treaty_text__nws_reading, iaea_safeguards_verification_apparatus, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(npt_treaty_text__nws_reading, iaea_safeguards_verification_apparatus, beneficiary).

% Coalition of states and civil society actors that has argued at every review conference since the 1990s that Article VI creates a genuine, time-bound legal obligation, citing the 1996 ICJ advisory opinion's 'obligation to pursue negotiations in good faith' language as more binding than the NWS reading admits. Their interpretive arguments are heard at review conferences but have never produced a binding timetable, verification mechanism, or enforcement clause; they lack the Security Council leverage the NWS possess.
narrative_ontology:constraint_stakeholder(npt_treaty_text__nws_reading, non_aligned_movement_disarmament_advocates, excluded,
    organized, generational, constrained, global).

% Issued the 1996 advisory opinion characterizing Article VI as an obligation to pursue negotiations to a conclusion, not merely to negotiate without result. The opinion is advisory only and carries no independent enforcement power; it is cited by both readings but implemented by neither. Sits outside the benefiting parties and outside enforcement capacity simultaneously.
narrative_ontology:constraint_stakeholder(npt_treaty_text__nws_reading, international_court_of_justice, observer,
    institutional, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(npt_treaty_text__nws_reading, nuclear_weapons_states).
narrative_ontology:fixing_cost_class(npt_treaty_text__nws_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a stable, verified global regime in which the large majority of states forgo nuclear weapons acquisition, reducing proliferation risk and enabling peaceful nuclear cooperation under IAEA oversight.
% TRANSFER_FUNCTION: Moves sovereignty over weapons development, verification access, and strategic autonomy from NNWS to the treaty regime and its NWS-influenced institutions, in exchange for a disarmament commitment that under this reading imposes no reciprocal binding transfer back.
% ABSENT_VOICES: States and disarmament coalitions arguing Article VI is binding are present at review conferences but structurally unable to convert their argument into enforceable text; the ICJ's 1996 characterization of the obligation as substantive is treated as advisory commentary rather than governing interpretation under this reading.
% DISAPPEARANCE_RATIONALE: If this specific reading collapsed and the nnws_reading's binding-Article-VI interpretation prevailed instead, NWS would face genuine legal exposure for arsenal modernization, review conferences would need enforcement and timetable mechanisms, and IAEA's verification mandate would need to expand toward NWS disarmament tracking — a substantial reallocation of institutional authority and legal exposure.
% FOUNDING_PROBLEM: The treaty was built in 1968 to halt the spread of nuclear weapons to additional states while the five existing nuclear powers retained their arsenals during the Cold War, with disarmament framed as the long-term horizon that made the bargain politically saleable to non-nuclear signatories.
% FOUNDING_PROBLEM_CORROBORATION: NWS governments attest the non-proliferation function remains fully live and disarmament remains a good-faith long-term aspiration. Outside the benefiting parties, the ICJ's 1996 advisory opinion, repeated NAM and New Agenda Coalition review-conference statements, and independent arms-control scholarship (e.g. SIPRI assessments of arsenal modernization trends) attest that the founding bargain's disarmament half has been substantively unmet for five decades under this reading, while the non-proliferation half has been fully enforced — corroboration for the imbalance comes from outside the NWS/IAEA seats.
narrative_ontology:disappearance_verdict(npt_treaty_text__nws_reading, world_rearranges).
narrative_ontology:founding_problem_status(npt_treaty_text__nws_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(npt_treaty_text__nws_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(npt_treaty_text__nws_reading, 'none', 1).
narrative_ontology:epsilon_provenance(npt_treaty_text__nws_reading, 0.71, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness rises from 0.45 at treaty entry into force to 0.71 by 2024 as the asymmetry between enforced non-proliferation and unenforced disarmament compounds with each review cycle that fails to produce binding Article VI language, while NWS arsenals are modernized rather than reduced. Suppression (0.68) reflects the sanctions, export-control, and diplomatic isolation mechanisms available against NNWS suspected of proliferation, with no structural equivalent available against NWS for arsenal retention. Theater ratio (0.42) reflects the review-conference cycle itself: substantial diplomatic activity and final-document language addressing disarmament that has not, over five decades, translated into binding obligation, alongside genuinely functional non-proliferation verification that is not theatrical.
 *
 * PERSPECTIVAL GAP:
 *   From the NWS/IAEA seats, the treaty operates as functioning multilateral coordination successfully preventing proliferation cascade — a defensible coordination story. From the NNWS seat, the same textual structure operates as an enforced one-way transfer of sovereignty dressed in the language of mutual bargain. The engine computes these divergent per-seat classifications from the identical structural data; the divergence is the object of study, not an error to reconcile.
 *
 * DIRECTIONALITY LOGIC:
 *   NWS and the IAEA verification apparatus sit near the beneficiary end: NWS retain strategic assets while incurring no enforceable cost under this reading, and IAEA's institutional continuity depends on the non-proliferation verification mandate remaining the treaty's operative center of gravity. NNWS sit near the target end: they accept the binding, verified, sanctioned obligations while receiving an unenforceable promise in return, and their exit option (Article X withdrawal) carries prohibitive diplomatic and economic costs that make trapped a more accurate descriptor than mobile.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (preventing 1960s-style proliferation cascade while preserving a path to eventual disarmament) is contested rather than resolved: the non-proliferation half of the founding problem remains substantively live (new proliferation risks persist), which is precisely why this reading resists being labeled a pure snare — there is a genuine coordination function still operating. But the disarmament half, under this reading, has been read out of binding force entirely rather than fulfilled, which is why tangled_rope rather than rope is the structurally correct claim: coordination and asymmetric extraction coexist in the same textual structure, requiring active enforcement (safeguards, sanctions) to hold the asymmetry in place across five decades of review-conference contestation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    npt_kernel_reading_disagreement_location,
    'This constraint is one reading (nws_reading) of the contested npt_treaty_text kernel. The sibling nnws_reading treats Article VI as a binding, substantive obligation with non-proliferation as the conditional restraint purchasing NWS compliance — an inversion of which obligation is binding and which is aspirational. Where exactly does the disagreement locate structurally?',
    'The disagreement is located in the interpretation of the phrase ''to pursue negotiations in good faith on effective measures relating to cessation of the nuclear arms race at an early date'' — specifically whether ''at an early date'' and ''good faith'' create a justiciable timetable obligation (nnws_reading, supported in part by the 1996 ICJ advisory opinion''s stronger language) or a non-binding aspirational standard (nws_reading, the operative reading in practice across five decades of review conferences). No treaty amendment or binding ICJ judgment (as opposed to advisory opinion) has resolved this ambiguity.',
    'Under the nnws_reading, this same treaty structure would classify NWS conduct (arsenal modernization without progress toward elimination) as the primary breach and would make non-proliferation restraint the conditional half of the bargain. Under nws_reading, the classification is inverted. The ICJ''s 1996 opinion pulls toward nnws_reading without resolving it, because it is advisory and non-binding.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(npt_kernel_reading_disagreement_location, conceptual, 'Locates the interpretive disagreement between nws_reading and nnws_reading in the ''good faith''/''early date'' language of Article VI.').

omega_variable(
    verification_asymmetry_naturalness,
    'Is the near-total concentration of IAEA verification budget and technical capacity on horizontal (NNWS) proliferation, with no comparable NWS disarmament-verification mandate, a neutral technical fact about what is verifiable, or a constructed allocation that itself entrenches the nws_reading?',
    'Compare against proposed but unadopted disarmament-verification frameworks (e.g., IPNDV — International Partnership for Nuclear Disarmament Verification) to assess whether NWS disarmament verification is technically infeasible or merely institutionally unfunded and politically unmandated.',
    'If technically infeasible, the verification asymmetry is closer to a natural constraint and less supportive of the tangled_rope claim''s extraction component. If merely unfunded/unmandated, the asymmetry is itself part of the constructed structure that benefits NWS, strengthening the tangled_rope classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(verification_asymmetry_naturalness, empirical, 'Whether IAEA''s proliferation-only verification focus is a technical necessity or a constructed allocation favoring NWS.').

omega_variable(
    review_conference_theater_vs_progress,
    'Are NPT Review Conference final documents addressing disarmament (1995, 2000, 2010 action plans) genuine incremental progress toward binding obligation, or theatrical restatement that resets the clock without altering the underlying non-binding status?',
    'Track whether specific numbered action-plan commitments (e.g. the 2010 64-point action plan) produced measurable arsenal reductions attributable to the commitment itself, versus reductions attributable to independent bilateral agreements (START treaties) that would have occurred regardless of NPT review cycles.',
    'If review-conference commitments are causally inert relative to bilateral arms control, the theater_ratio measurement is validated and rising theatricality over time is confirmed as metric substitution (Goodhart drift) rather than genuine progress.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(review_conference_theater_vs_progress, empirical, 'Whether review-conference disarmament commitments produce measurable arsenal change independent of bilateral agreements.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(npt_treaty_text__nws_reading, 1968, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(npt__tr_t1968, npt_treaty_text__nws_reading, theater_ratio, 1968, 0.2).
narrative_ontology:measurement(npt__tr_t1985, npt_treaty_text__nws_reading, theater_ratio, 1985, 0.28).
narrative_ontology:measurement(npt__tr_t1995, npt_treaty_text__nws_reading, theater_ratio, 1995, 0.33).
narrative_ontology:measurement(npt__tr_t2005, npt_treaty_text__nws_reading, theater_ratio, 2005, 0.36).
narrative_ontology:measurement(npt__tr_t2015, npt_treaty_text__nws_reading, theater_ratio, 2015, 0.4).
narrative_ontology:measurement(npt__tr_t2024, npt_treaty_text__nws_reading, theater_ratio, 2024, 0.42).

% Extraction over time
narrative_ontology:measurement(npt__be_t1968, npt_treaty_text__nws_reading, base_extractiveness, 1968, 0.45).
narrative_ontology:measurement(npt__be_t1985, npt_treaty_text__nws_reading, base_extractiveness, 1985, 0.52).
narrative_ontology:measurement(npt__be_t1995, npt_treaty_text__nws_reading, base_extractiveness, 1995, 0.58).
narrative_ontology:measurement(npt__be_t2005, npt_treaty_text__nws_reading, base_extractiveness, 2005, 0.64).
narrative_ontology:measurement(npt__be_t2015, npt_treaty_text__nws_reading, base_extractiveness, 2015, 0.68).
narrative_ontology:measurement(npt__be_t2024, npt_treaty_text__nws_reading, base_extractiveness, 2024, 0.71).

% Suppression requirement over time
narrative_ontology:measurement(npt__su_t1968, npt_treaty_text__nws_reading, suppression_requirement, 1968, 0.5).
narrative_ontology:measurement(npt__su_t1985, npt_treaty_text__nws_reading, suppression_requirement, 1985, 0.55).
narrative_ontology:measurement(npt__su_t1995, npt_treaty_text__nws_reading, suppression_requirement, 1995, 0.6).
narrative_ontology:measurement(npt__su_t2005, npt_treaty_text__nws_reading, suppression_requirement, 2005, 0.63).
narrative_ontology:measurement(npt__su_t2015, npt_treaty_text__nws_reading, suppression_requirement, 2015, 0.66).
narrative_ontology:measurement(npt__su_t2024, npt_treaty_text__nws_reading, suppression_requirement, 2024, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(npt_treaty_text__nws_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(npt_treaty_text__nws_reading, npt_treaty_text__nnws_reading).
narrative_ontology:affects_constraint(npt_treaty_text__nws_reading, npt_treaty_text__withdrawal_threshold_reading).

% DUAL FORMULATION NOTE:
% This story (nws_reading) and npt_treaty_text__nnws_reading are two structurally distinct readings of the same kernel text, differing in which party's obligation (non-proliferation vs. disarmament) is treated as binding and which as aspirational — they carry different epsilon values, different beneficiary/victim assignments, and potentially different classifications (this story: tangled_rope; the nnws_reading would classify NWS arsenal retention as the primary extractive breach). npt_treaty_text__withdrawal_threshold_reading addresses a separate interpretive axis (Article X withdrawal threshold) that interacts with both readings but is not decomposed further here. All three are linked as siblings of the same kernel and should be read together, never merged into one classification.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
