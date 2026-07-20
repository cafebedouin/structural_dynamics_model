% ============================================================================
% CONSTRAINT STORY: npt_article_iv_vi_pairing__nonproliferation_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
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
    narrative_ontology:constraint_vindicates/2,
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
 *   constraint_id: npt_article_iv_vi_pairing__nonproliferation_primary
 *   human_readable: NPT Article IV-VI Pairing: Nonproliferation-Primary Reading
 *   domain: international_law/nuclear_governance/treaty_interpretation
 *
 * SUMMARY:
 *   The NPT Article IV-VI pairing under the nonproliferation-primary reading
 *   treats the treaty as a permanently stabilized two-tier order: non-weapon
 *   states accept perpetual restraint and intrusive verification (Article III
 *   conditioning Article IV) while weapon states maintain arsenals
 *   indefinitely with Article VI disarmament obligations rendered
 *   aspirational and non-justiciable. Authority derives from weapon states'
 *   security interest in preventing horizontal proliferation, not from
 *   reciprocal obligation. The structural asymmetry extracts sovereignty and
 *   strategic options from non-weapon states to secure the
 *   weapon-state-centric order. This is a kernel reading decomposed from the
 *   colloquial 'NPT grand bargain' label.
 *
 * KEY AGENTS:
 *   - nuclear_weapon_states: Primary beneficiary and agenda-setter (institutional/arbitrage) â controls interpretation, exempts arsenals from enforcement, captures stability gains
 *   - non_weapon_states_parties: Primary payer (organized/constrained) â bears verification costs and perpetual restraint without reciprocal disarmament
 *   - iaea: Administrative agenda-setter (institutional/constrained) â enforces Article III verification on non-weapon states, absent Article VI mandate
 *   - tpnw_advocacy_coalition: Excluded voice (organized/constrained) â reads Article VI as binding, structurally marginalized in NPT forums
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(npt_article_iv_vi_pairing__nonproliferation_primary, 0.71).
domain_priors:suppression_score(npt_article_iv_vi_pairing__nonproliferation_primary, 0.68).
domain_priors:theater_ratio(npt_article_iv_vi_pairing__nonproliferation_primary, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(npt_article_iv_vi_pairing__nonproliferation_primary, extractiveness, 0.71).
narrative_ontology:constraint_metric(npt_article_iv_vi_pairing__nonproliferation_primary, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(npt_article_iv_vi_pairing__nonproliferation_primary, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(npt_article_iv_vi_pairing__nonproliferation_primary, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(npt_article_iv_vi_pairing__nonproliferation_primary, resistance, 0.52).

% --- Constraint claim ---
narrative_ontology:constraint_claim(npt_article_iv_vi_pairing__nonproliferation_primary, tangled_rope).
narrative_ontology:human_readable(npt_article_iv_vi_pairing__nonproliferation_primary, "NPT Article IV-VI Pairing: Nonproliferation-Primary Reading").
narrative_ontology:topic_domain(npt_article_iv_vi_pairing__nonproliferation_primary, "international_law/nuclear_governance/treaty_interpretation").

domain_priors:requires_active_enforcement(npt_article_iv_vi_pairing__nonproliferation_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(npt_article_iv_vi_pairing__nonproliferation_primary, '90d0f370-b8c2-4280-ad2e-af5b6351e55c').
narrative_ontology:cs_kernel_codification('90d0f370-b8c2-4280-ad2e-af5b6351e55c', formalized).
narrative_ontology:cs_authority_grounding('90d0f370-b8c2-4280-ad2e-af5b6351e55c', extraction).
narrative_ontology:cs_interpretation_layer_present('90d0f370-b8c2-4280-ad2e-af5b6351e55c').
narrative_ontology:cs_reading_relation('90d0f370-b8c2-4280-ad2e-af5b6351e55c', npt_article_iv_vi_pairing__abolitionist, forecloses).
narrative_ontology:cs_reading_relation('90d0f370-b8c2-4280-ad2e-af5b6351e55c', npt_article_iv_vi_pairing__grand_bargain, influences).
narrative_ontology:cs_axiom('90d0f370-b8c2-4280-ad2e-af5b6351e55c', foundational, horizontal_nonproliferation_imperative).
narrative_ontology:cs_axiom_status(horizontal_nonproliferation_imperative, holdable).
narrative_ontology:cs_axiom_grounding('90d0f370-b8c2-4280-ad2e-af5b6351e55c', horizontal_nonproliferation_imperative, instrumental).
narrative_ontology:cs_axiom('90d0f370-b8c2-4280-ad2e-af5b6351e55c', foundational, arsenal_sovereignty_perpetuity).
narrative_ontology:cs_axiom_status(arsenal_sovereignty_perpetuity, holdable).
narrative_ontology:cs_axiom_grounding('90d0f370-b8c2-4280-ad2e-af5b6351e55c', arsenal_sovereignty_perpetuity, conventional).
narrative_ontology:cs_reference_frame('90d0f370-b8c2-4280-ad2e-af5b6351e55c', npt_two_tier_stabilization).
narrative_ontology:cs_drift_state('90d0f370-b8c2-4280-ad2e-af5b6351e55c', tpnw_post_humanitarian_initiative, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('90d0f370-b8c2-4280-ad2e-af5b6351e55c', '').
narrative_ontology:cs_kernel_id(npt_article_iv_vi_pairing__nonproliferation_primary, npt_article_iv_vi_pairing).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(npt_article_iv_vi_pairing__nonproliferation_primary, nuclear_weapon_states).
narrative_ontology:constraint_victim(npt_article_iv_vi_pairing__nonproliferation_primary, non_weapon_states_parties).
narrative_ontology:constraint_vindicates(npt_article_iv_vi_pairing__nonproliferation_primary, conditional_peaceful_use_doctrine).
narrative_ontology:constraint_vindicates(npt_article_iv_vi_pairing__nonproliferation_primary, nuclear_hierarchy_legitimacy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Control the interpretive framework of the NPT, ensure Article VI remains non-justiciable, and maintain arsenals while enforcing non-proliferation obligations on others. Set the conditions for Article IV access and define compliance through the Security Council and NPT review conferences.
narrative_ontology:constraint_stakeholder(npt_article_iv_vi_pairing__nonproliferation_primary, nuclear_weapon_states, agenda_setter,
    institutional, civilizational, arbitrage, global).

% Accept intrusive IAEA safeguards under Article III as a condition for Article IV technology access. Forgo the nuclear weapons option permanently while weapon states maintain arsenals indefinitely. Bear sovereignty costs of verification without reciprocal disarmament enforcement.
narrative_ontology:constraint_stakeholder(npt_article_iv_vi_pairing__nonproliferation_primary, non_weapon_states_parties, payer,
    organized, generational, constrained, global).

% Administers the Article III verification regime that conditions Article IV peaceful-use benefits. Enforces safeguards on non-weapon states while lacking mandate to verify weapon state disarmament under Article VI.
narrative_ontology:constraint_stakeholder(npt_article_iv_vi_pairing__nonproliferation_primary, iaea, agenda_setter,
    institutional, generational, constrained, global).

% Advocates for the Treaty on the Prohibition of Nuclear Weapons and reads Article VI as a binding disarmament mandate. Structurally excluded from NPT review conference consensus and dismissed by weapon states as undermining the regime.
narrative_ontology:constraint_stakeholder(npt_article_iv_vi_pairing__nonproliferation_primary, tpnw_advocacy_coalition, excluded,
    organized, generational, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(npt_article_iv_vi_pairing__nonproliferation_primary, nuclear_weapon_states).
narrative_ontology:fixing_cost_class(npt_article_iv_vi_pairing__nonproliferation_primary, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establish a stable international order preventing horizontal nuclear proliferation by creating a framework where non-weapon states accept verification and forgo weapons, while weapon states maintain security umbrellas and strategic stability.
% TRANSFER_FUNCTION: Sovereignty and strategic autonomy from non-weapon states â acceptance of intrusive safeguards, forgoing the weapons option, and technology conditionality â to the stability of the weapon-state-centric order; disarmament obligations from weapon states are rhetorically acknowledged but structurally unenforced.
% ABSENT_VOICES: Abolitionist states and TPNW advocates who read Article VI as a binding disarmament timeline; non-weapon states that understand the grand bargain as reciprocal and conditional; horizontal aspirants who are denied full technology access.
% DISAPPEARANCE_RATIONALE: If the Article IV-VI pairing under this reading vanished, the two-tier order would collapse: non-weapon states would likely demand unconditional technology access or pursue weapons options, weapon state arsenals would lose their privileged exemption from international scrutiny, and the NPT review process would reconstitute around disarmament enforcement or treaty collapse.
% FOUNDING_PROBLEM: Preventing rapid horizontal nuclear proliferation during the Cold War while accommodating existing weapon states' strategic interests.
% FOUNDING_PROBLEM_CORROBORATION: Weapon states and security scholars attest the problem was horizontal proliferation. Non-weapon states and humanitarian law advocates attest the founding problem was a reciprocal grand bargain requiring disarmament. Independent legal historians note the treaty text's deliberate ambiguity on Article VI enforceability.
narrative_ontology:disappearance_verdict(npt_article_iv_vi_pairing__nonproliferation_primary, world_rearranges).
narrative_ontology:founding_problem_status(npt_article_iv_vi_pairing__nonproliferation_primary, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(npt_article_iv_vi_pairing__nonproliferation_primary, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(npt_article_iv_vi_pairing__nonproliferation_primary, 'none', 1).
narrative_ontology:epsilon_provenance(npt_article_iv_vi_pairing__nonproliferation_primary, 0.71, 'kimi-k2.6', 'none', direct).

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
 *   Extractiveness is high (0.71) because the constraint permanently locks in strategic asymmetry: non-weapon states surrender sovereignty through safeguards and forgo weapons indefinitely while weapon states face no comparable enforcement. Suppression is substantial (0.68) because the regime actively marginalizes alternatives â horizontal proliferation is sanctioned, the TPNW is dismissed, and Article VI is kept unenforceable through interpretive practice. Theater ratio is moderate-high (0.42) because NPT review conferences produce decades of disarmament rhetoric without enforcement, functioning as performative maintenance of the grand-bargain myth while the underlying structure is nonproliferation-primary. The temporal series shows extraction and theater accumulating as the Cold War consensus frayed and weapon state modernization replaced disarmament.
 *
 * PERSPECTIVAL GAP:
 *   From the weapon state seat, the constraint is essential coordination preventing nuclear anarchy; from the non-weapon state seat, it is enforced hierarchy extracting sovereignty for unfulfilled promises. The IAEA seat experiences it as technical implementation with no authority to resolve the asymmetry. The engine computes this divergence from the structural data: same constraint, radically different effective extraction by seat.
 *
 * DIRECTIONALITY LOGIC:
 *   Nuclear weapon states are structural beneficiaries (d near 0.0): the constraint subsidizes their security strategy by externalizing non-proliferation costs onto non-weapon states and exempting their arsenals. Non-weapon states are structural targets (d near 1.0): they pay verification costs, accept technology conditionality, and surrender the weapons option while receiving unenforced disarmament promises. IAEA sits near symmetric (d ~0.5): it coordinates verification but lacks authority to enforce Article VI, making it structurally embedded in the asymmetric order. TPNW advocates are excluded rather than coordinated: their reading of Article VI is the suppressed alternative.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading prevents mandatrophy mislabeling by distinguishing the genuine coordination function (preventing horizontal proliferation) from the extraction function (perpetuating weapon state arsenals). A snare reading would deny the coordination function entirely; this reading acknowledges that non-proliferation is real but asymmetrically distributed. A rope reading would deny the asymmetry; this reading makes it structural. The metrics are authored independently: the coordination function is real (low theater early on, genuine verification), but extraction accumulated as disarmament obligations atrophied into rhetoric.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    article_vi_justiciability_ambiguity,
    'Is Article VI''s disarmament obligation genuinely non-justiciable under international law, or has its aspirational status been constructed through weapon state interpretive practice to preserve arsenals?',
    'ICJ advisory opinion or adjudicated case establishing Article VI justiciability; or consistent state practice demonstrating enforcement.',
    'If justiciable, the two-tier order destabilizes and the constraint shifts toward grand_bargain or abolitionist readings; if genuinely non-justiciable, the current extraction is structurally locked.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(article_vi_justiciability_ambiguity, conceptual, 'Ambiguity over Article VI legal enforceability').

omega_variable(
    conditional_iv_as_extraction,
    'Does Article IV''s conditioning on Article III verification function as necessary safeguards coordination or as a structural lever for technology denial that perpetuates hierarchy?',
    'Comparative analysis of technology transfer rates pre/post-safeguards; assessment of whether denials track security logic or market/political logic.',
    'If denials primarily serve hierarchy maintenance, the coordination story is cover for extraction; if they track genuine non-proliferation necessity, the tangled rope reading weakens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(conditional_iv_as_extraction, empirical, 'Whether Article IV conditioning is coordination or hierarchy').

omega_variable(
    kernel_reading_underdetermination,
    'Does the NPT text independently support the nonproliferation-primary reading, or is this reading only sustainable through weapon state power and institutional capture of review processes?',
    'Textual analysis of Article VI alongside VCLT interpretation principles; network analysis of NPT review conference outcome documents.',
    'If the text sustains this reading independently, it is a legitimate legal interpretation; if sustained only by power, it is extraction through interpretive capture.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_underdetermination, conceptual, 'Whether the reading is textually grounded or power-sustained').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(npt_article_iv_vi_pairing__nonproliferation_primary, 0, 55).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(npt_npp_tr_t0, npt_article_iv_vi_pairing__nonproliferation_primary, theater_ratio, 0, 0.18).
narrative_ontology:measurement(npt_npp_tr_t10, npt_article_iv_vi_pairing__nonproliferation_primary, theater_ratio, 10, 0.24).
narrative_ontology:measurement(npt_npp_tr_t20, npt_article_iv_vi_pairing__nonproliferation_primary, theater_ratio, 20, 0.3).
narrative_ontology:measurement(npt_npp_tr_t30, npt_article_iv_vi_pairing__nonproliferation_primary, theater_ratio, 30, 0.34).
narrative_ontology:measurement(npt_npp_tr_t40, npt_article_iv_vi_pairing__nonproliferation_primary, theater_ratio, 40, 0.38).
narrative_ontology:measurement(npt_npp_tr_t50, npt_article_iv_vi_pairing__nonproliferation_primary, theater_ratio, 50, 0.4).
narrative_ontology:measurement(npt_npp_tr_t55, npt_article_iv_vi_pairing__nonproliferation_primary, theater_ratio, 55, 0.42).

% Extraction over time
narrative_ontology:measurement(npt_npp_be_t0, npt_article_iv_vi_pairing__nonproliferation_primary, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(npt_npp_be_t10, npt_article_iv_vi_pairing__nonproliferation_primary, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(npt_npp_be_t20, npt_article_iv_vi_pairing__nonproliferation_primary, base_extractiveness, 20, 0.56).
narrative_ontology:measurement(npt_npp_be_t30, npt_article_iv_vi_pairing__nonproliferation_primary, base_extractiveness, 30, 0.62).
narrative_ontology:measurement(npt_npp_be_t40, npt_article_iv_vi_pairing__nonproliferation_primary, base_extractiveness, 40, 0.66).
narrative_ontology:measurement(npt_npp_be_t50, npt_article_iv_vi_pairing__nonproliferation_primary, base_extractiveness, 50, 0.69).
narrative_ontology:measurement(npt_npp_be_t55, npt_article_iv_vi_pairing__nonproliferation_primary, base_extractiveness, 55, 0.71).

% Suppression requirement over time
narrative_ontology:measurement(npt_npp_su_t0, npt_article_iv_vi_pairing__nonproliferation_primary, suppression_requirement, 0, 0.48).
narrative_ontology:measurement(npt_npp_su_t10, npt_article_iv_vi_pairing__nonproliferation_primary, suppression_requirement, 10, 0.54).
narrative_ontology:measurement(npt_npp_su_t20, npt_article_iv_vi_pairing__nonproliferation_primary, suppression_requirement, 20, 0.62).
narrative_ontology:measurement(npt_npp_su_t30, npt_article_iv_vi_pairing__nonproliferation_primary, suppression_requirement, 30, 0.66).
narrative_ontology:measurement(npt_npp_su_t40, npt_article_iv_vi_pairing__nonproliferation_primary, suppression_requirement, 40, 0.68).
narrative_ontology:measurement(npt_npp_su_t50, npt_article_iv_vi_pairing__nonproliferation_primary, suppression_requirement, 50, 0.68).
narrative_ontology:measurement(npt_npp_su_t55, npt_article_iv_vi_pairing__nonproliferation_primary, suppression_requirement, 55, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(npt_article_iv_vi_pairing__nonproliferation_primary, enforcement_mechanism).
narrative_ontology:affects_constraint(npt_article_iv_vi_pairing__nonproliferation_primary, npt_article_iv_vi_pairing__grand_bargain).
narrative_ontology:affects_constraint(npt_article_iv_vi_pairing__nonproliferation_primary, npt_article_iv_vi_pairing__abolitionist).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the NPT Article IV-VI pairing kernel, decomposed from the colloquial 'NPT grand bargain' label. The nonproliferation_primary reading treats the pairing as a permanently stabilized two-tier order; the grand_bargain reading treats it as reciprocal exchange; the abolitionist reading treats Article VI as a binding disarmament mandate. Each reading carries a distinct epsilon and structural profile.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
