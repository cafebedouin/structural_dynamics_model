% ============================================================================
% CONSTRAINT STORY: npt_article_iv_vi_pairing__abolitionist
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
 *   constraint_id: npt_article_iv_vi_pairing__abolitionist
 *   human_readable: Abolitionist Reading of NPT Article IV/VI Pairing
 *   domain: international_law/nuclear_governance
 *
 * SUMMARY:
 *   This constraint story models the abolitionist reading of the NPT Article
 *   IV/VI pairing kernel. Under this reading, Article VI imposes a
 *   categorical obligation to achieve complete nuclear disarmament, Article
 *   IV's legitimacy is subordinated to a humanitarian prohibition norm that
 *   collapses the peaceful/military distinction, and the authority for this
 *   interpretation derives from international humanitarian law and the Treaty
 *   on the Prohibition of Nuclear Weapons (TPNW). The NPT itself is read as
 *   insufficient and delegitimized where it perpetuates dual-use
 *   proliferation risk. This is a commitment-system constraint: a formalized
 *   treaty kernel interpreted through a lineage of humanitarian law, with an
 *   active interpretive layer (ICRC, TPNW states, civil society) that
 *   re-reads the NPT's text against its original nonproliferation-primary and
 *   grand-bargain framings. The constraint coordinates a global coalition
 *   around categorical prohibition while asymmetrically extracting sovereign
 *   security discretion from nuclear weapon states and technology access
 *   claims from Article IV-dependent non-nuclear states.
 *
 * KEY AGENTS:
 *   - nuclear_weapon_states: Primary target (powerful/constrained) â bear extraction via disarmament obligation and delegitimization of deterrence.
 *   - tpnw_adherent_states: Primary beneficiary (organized/mobile) â gain normative authority from prohibition framing and treaty leadership.
 *   - article_iv_dependent_states: Secondary target (moderate/constrained) â bear costs of constrained technology access and loss of unconditional peaceful-use claims.
 *   - humanitarian_law_institutions: Agenda-setter (institutional/analytical) â provide authority grounding and interpretive machinery.
 *   - nuclear_victims_and_survivors: Secondary beneficiary (powerless/trapped) â gain symbolic and normative benefit from stigmatization of weapons without controlling the legal process.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(npt_article_iv_vi_pairing__abolitionist, 0.72).
domain_priors:suppression_score(npt_article_iv_vi_pairing__abolitionist, 0.68).
domain_priors:theater_ratio(npt_article_iv_vi_pairing__abolitionist, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(npt_article_iv_vi_pairing__abolitionist, extractiveness, 0.72).
narrative_ontology:constraint_metric(npt_article_iv_vi_pairing__abolitionist, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(npt_article_iv_vi_pairing__abolitionist, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(npt_article_iv_vi_pairing__abolitionist, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(npt_article_iv_vi_pairing__abolitionist, resistance, 0.85).

% --- Constraint claim ---
narrative_ontology:constraint_claim(npt_article_iv_vi_pairing__abolitionist, tangled_rope).
narrative_ontology:human_readable(npt_article_iv_vi_pairing__abolitionist, "Abolitionist Reading of NPT Article IV/VI Pairing").
narrative_ontology:topic_domain(npt_article_iv_vi_pairing__abolitionist, "international_law/nuclear_governance").

domain_priors:requires_active_enforcement(npt_article_iv_vi_pairing__abolitionist).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(npt_article_iv_vi_pairing__abolitionist, 'cf438230-85c9-450f-9340-bd00e5c0e103').
narrative_ontology:cs_kernel_codification('cf438230-85c9-450f-9340-bd00e5c0e103', formalized).
narrative_ontology:cs_authority_grounding('cf438230-85c9-450f-9340-bd00e5c0e103', lineage).
narrative_ontology:cs_interpretation_layer_present('cf438230-85c9-450f-9340-bd00e5c0e103').
narrative_ontology:cs_reading_relation('cf438230-85c9-450f-9340-bd00e5c0e103', npt_article_iv_vi_pairing__nonproliferation_primary, coexists_with).
narrative_ontology:cs_reading_relation('cf438230-85c9-450f-9340-bd00e5c0e103', npt_article_iv_vi_pairing__grand_bargain, coexists_with).
narrative_ontology:cs_axiom('cf438230-85c9-450f-9340-bd00e5c0e103', foundational, nuclear_possession_categorically_illegal).
narrative_ontology:cs_axiom_status(nuclear_possession_categorically_illegal, holdable).
narrative_ontology:cs_axiom_grounding('cf438230-85c9-450f-9340-bd00e5c0e103', nuclear_possession_categorically_illegal, deontological).
narrative_ontology:cs_axiom('cf438230-85c9-450f-9340-bd00e5c0e103', foundational, no_peaceful_military_distinction).
narrative_ontology:cs_axiom_status(no_peaceful_military_distinction, holdable).
narrative_ontology:cs_axiom_grounding('cf438230-85c9-450f-9340-bd00e5c0e103', no_peaceful_military_distinction, empirically_contingent).
narrative_ontology:cs_reference_frame('cf438230-85c9-450f-9340-bd00e5c0e103', humanitarian_prohibition_framework).
narrative_ontology:cs_drift_state('cf438230-85c9-450f-9340-bd00e5c0e103', post_tpnw_institutionalization, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('cf438230-85c9-450f-9340-bd00e5c0e103', '').
narrative_ontology:cs_kernel_id(npt_article_iv_vi_pairing__abolitionist, npt_article_iv_vi_pairing).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(npt_article_iv_vi_pairing__abolitionist, tpnw_adherent_states).
narrative_ontology:constraint_beneficiary(npt_article_iv_vi_pairing__abolitionist, nuclear_victims_and_survivors).
narrative_ontology:constraint_victim(npt_article_iv_vi_pairing__abolitionist, nuclear_weapon_states).
narrative_ontology:constraint_victim(npt_article_iv_vi_pairing__abolitionist, article_iv_dependent_states).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Possess nuclear arsenals and base security doctrines on deterrence. Under this reading, they face a categorical obligation to disarm and lose legal legitimacy for possession. Their exit options are constrained by the diplomatic and strategic cost of abandoning the NPT or openly rejecting humanitarian law norms, though they actively resist the reading through modernization and diplomatic boycott.
narrative_ontology:constraint_stakeholder(npt_article_iv_vi_pairing__abolitionist, nuclear_weapon_states, payer,
    powerful, generational, constrained, global).

% Have joined the Treaty on the Prohibition of Nuclear Weapons and advocate for its normative expansion. They gain institutional standing and moral leadership from the prohibition frame. Their exit is mobile: they could shift back to the NPT grand-bargain logic, though doing so would carry diplomatic costs within the humanitarian coalition.
narrative_ontology:constraint_stakeholder(npt_article_iv_vi_pairing__abolitionist, tpnw_adherent_states, beneficiary,
    organized, generational, mobile, global).

% Seek access to nuclear technology for energy, medicine, or research under Article IV. Under this reading, their claims are delegitimized because the peaceful/military distinction collapses; they face suspicion rather than entitlement. Their exit is constrained by the lack of alternative technology suppliers outside the safeguards regime and by the stigma now attached to all nuclear programs.
narrative_ontology:constraint_stakeholder(npt_article_iv_vi_pairing__abolitionist, article_iv_dependent_states, payer,
    moderate, generational, constrained, global).

% Interpret and develop the body of international humanitarian law, including the ICRC and associated legal experts. They provide the authority framework that re-reads the NPT through humanitarian lenses. Their position is analytical: they do not bear costs or collect rents, but their interpretive tradition is vindicated and extended by this reading.
narrative_ontology:constraint_stakeholder(npt_article_iv_vi_pairing__abolitionist, humanitarian_law_institutions, agenda_setter,
    institutional, civilizational, analytical, global).

% Communities harmed by nuclear testing and use, including indigenous peoples and downwinders. They gain symbolic recognition and normative vindication from the categorical stigmatization of nuclear weapons, but they do not control the treaty machinery or the pace of disarmament. Their exit is trapped: they cannot undo their exposure and cannot effectively exit the international system that produced it.
narrative_ontology:constraint_stakeholder(npt_article_iv_vi_pairing__abolitionist, nuclear_victims_and_survivors, beneficiary,
    powerless, generational, trapped, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(npt_article_iv_vi_pairing__abolitionist, tpnw_adherent_states).
narrative_ontology:fixing_cost_class(npt_article_iv_vi_pairing__abolitionist, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a global coalition of states and civil society around a categorical prohibition norm that replaces the NPT's conditional management of nuclear technology with a unified humanitarian-law framework aimed at eliminating nuclear arsenals and preventing catastrophic use.
% TRANSFER_FUNCTION: Transfers normative authority and legal legitimacy from the NPT's reciprocal bargain framework to a humanitarian prohibition regime; transfers sovereign security discretion from nuclear weapon states to an international humanitarian law standard; transfers technology access claims from Article IV-dependent states to a suspicion regime where peaceful use is not presumptively distinct from military use.
% ABSENT_VOICES: Nuclear weapon state strategic planners and civilian nuclear industry representatives are absent from the humanitarian law interpretive community; their security and economic framings are treated as illegitimate a priori under the prohibition frame.
% DISAPPEARANCE_RATIONALE: If this abolitionist reading vanished overnight, the TPNW would lose its primary interpretive bridge to the NPT, nuclear weapon states would regain sovereign legitimacy for deterrence and possession, Article IV-dependent states would resume unconditional technology claims, and the international nuclear order would revert to the nonproliferation-primary or grand-bargain equilibrium.
% FOUNDING_PROBLEM: The threat of nuclear annihilation and the humanitarian catastrophe of nuclear weapons use; the inadequacy of the NPT's conditional bargain to prevent vertical proliferation and the risk of use.
% FOUNDING_PROBLEM_CORROBORATION: ICRC and independent humanitarian analyses (e.g., IPPNW medical studies, ICRC reporting on catastrophic humanitarian consequences) corroborate the catastrophic risk from outside the benefiting state parties; nuclear weapon states contest the framing and assert that deterrence stability solves the problem.
narrative_ontology:disappearance_verdict(npt_article_iv_vi_pairing__abolitionist, world_rearranges).
narrative_ontology:founding_problem_status(npt_article_iv_vi_pairing__abolitionist, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(npt_article_iv_vi_pairing__abolitionist, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(npt_article_iv_vi_pairing__abolitionist, 'none', 1).
narrative_ontology:epsilon_provenance(npt_article_iv_vi_pairing__abolitionist, 0.72, 'kimi-k2.6', 'none', direct).

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
 *   Extractiveness (0.72) is high because the constraint transfers sovereign security discretion and technology access rights from nuclear possessors and seekers to a prohibition regime. Suppression (0.68) reflects the active normative stigmatization required to marginalize deterrence doctrine and unconditional Article IV claims. Theater ratio (0.25) is low-to-moderate: the humanitarian initiative is genuine, but diplomatic performances at TPNW conferences partially substitute for disarmament outcomes. Accessibility collapse (0.70) is high because once the humanitarian law frame is accepted, deterrence and dual-use technology alternatives collapse as legally legitimate. Resistance (0.85) is very high because nuclear weapon states actively contest the reading through NPT review conferences, nuclear modernization, and diplomatic boycott of TPNW proceedings. Temporal measurements track the humanitarian initiative's institutionalization from 2010 conferences through the 2017 TPNW adoption and 2021 entry into force, showing steadily rising extraction and suppression as the treaty regime hardened.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats (nuclear_weapon_states, article_iv_dependent_states) experience the constraint as coercive normative extraction that delegitimizes their core security and development claims. The beneficiary/agenda-setter seats (tpnw_adherent_states, humanitarian_law_institutions) experience it as legitimate coordination toward humanitarian survival. The engine will compute high effective extraction for the weapon states due to their powerful-but-constrained position (high scope, limited exit), and moderate extraction for Article IV-dependent states. The beneficiary seats will compute low or negative effective extraction (subsidized by the constraint).
 *
 * DIRECTIONALITY LOGIC:
 *   Nuclear weapon states are declared victims because the constraint extracts sovereign possession and deterrence legitimacy; their exit is constrained by the diplomatic and legal cost of exiting the NPT or rejecting humanitarian law norms. Article IV-dependent states are declared victims because the constraint extracts their unconditional technology access claims by collapsing the peaceful/military distinction. TPNW adherent states are declared beneficiaries because they collect normative authority and treaty-system legitimacy from the constraint's operation. Nuclear victims and survivors are declared beneficiaries because the constraint's stigmatization of weapons vindicates their harm. Humanitarian law institutions are agenda-setters who administer the interpretive layer and benefit from framework extension but do not capture material rents.
 *
 * MANDATROPHY ANALYSIS:
 *   The abolitionist reading avoids mislabeling by requiring both coordination (humanitarian coalition around prohibition) and asymmetric extraction (sovereignty and technology rights transferred from possessors/seekers). Without the victim declarations, the constraint might read as pure coordination (a rope), which would fail to capture the coercive cost to nuclear weapon states and technology-dependent states. Without the beneficiary declarations, it might read as a pure snare, missing the genuine coordination function of preventing nuclear catastrophe. The temporal measurements show the coordination function did not atrophy into a piton: theater remains low and the founding problem (catastrophic risk) remains live.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    abolitionist_kernel_reading_identity,
    'Does the abolitionist reading of the NPT Article IV/VI pairing logically foreclose the grand_bargain and nonproliferation_primary readings, or merely coexist as a competing political framing in a decentralized legal order?',
    'Engine computation from cs_structure axioms and reading_relations: if foundational axioms are empirically_contingent and drift state reveals overriding, foreclosure may be derived; otherwise coexistence is the stable structural relation.',
    'If foreclosing, the abolitionist reading claims a monopoly on legitimate interpretation of the NPT; if coexisting, the NPT kernel remains irreducibly contested with no single authoritative reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(abolitionist_kernel_reading_identity, conceptual, 'Structural relation of abolitionist reading to sibling readings in the NPT kernel.').

omega_variable(
    article_vi_justiciability_under_humanitarian_law,
    'Is Article VI''s disarmament obligation rendered justiciable and enforceable by reframing it through humanitarian law and TPNW precedent, or does it remain a political commitment even under the abolitionist reading?',
    'International judicial opinion (e.g., ICJ case law, future advisory opinions) on whether humanitarian law creates enforceable state obligations to eliminate arsenals.',
    'If justiciable, the constraint''s extractiveness rises because nuclear weapon states face adjudicated liability; if merely political, the constraint operates as normative pressure without direct enforcement, lowering effective suppression.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(article_vi_justiciability_under_humanitarian_law, empirical, 'Whether the abolitionist reading''s legal authority produces enforceable obligations.').

omega_variable(
    peaceful_military_distinction_erasure,
    'Does the abolitionist reading''s elimination of the peaceful/military distinction collapse all nuclear technology access, or can a verification-heavy regime salvage a constrained Article IV?',
    'Analysis of TPNW state practice and IAEA verification evolution: if TPNW adherents develop nuclear medicine or energy under strict safeguards, the distinction is constrained but not erased; if they forego all fission technology, the erasure is total.',
    'Total erasure makes Article IV-dependent states full victims of the constraint; constrained salvage leaves them as partial payers with residual coordination benefit.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(peaceful_military_distinction_erasure, conceptual, 'Ambiguity over scope of Article IV illegitimacy under abolitionist reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(npt_article_iv_vi_pairing__abolitionist, 2010, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(npt__tr_t2010, npt_article_iv_vi_pairing__abolitionist, theater_ratio, 2010, 0.1).
narrative_ontology:measurement(npt__tr_t2014, npt_article_iv_vi_pairing__abolitionist, theater_ratio, 2014, 0.15).
narrative_ontology:measurement(npt__tr_t2017, npt_article_iv_vi_pairing__abolitionist, theater_ratio, 2017, 0.18).
narrative_ontology:measurement(npt__tr_t2020, npt_article_iv_vi_pairing__abolitionist, theater_ratio, 2020, 0.2).
narrative_ontology:measurement(npt__tr_t2022, npt_article_iv_vi_pairing__abolitionist, theater_ratio, 2022, 0.22).
narrative_ontology:measurement(npt__tr_t2024, npt_article_iv_vi_pairing__abolitionist, theater_ratio, 2024, 0.25).

% Extraction over time
narrative_ontology:measurement(npt__be_t2010, npt_article_iv_vi_pairing__abolitionist, base_extractiveness, 2010, 0.48).
narrative_ontology:measurement(npt__be_t2014, npt_article_iv_vi_pairing__abolitionist, base_extractiveness, 2014, 0.55).
narrative_ontology:measurement(npt__be_t2017, npt_article_iv_vi_pairing__abolitionist, base_extractiveness, 2017, 0.62).
narrative_ontology:measurement(npt__be_t2020, npt_article_iv_vi_pairing__abolitionist, base_extractiveness, 2020, 0.66).
narrative_ontology:measurement(npt__be_t2022, npt_article_iv_vi_pairing__abolitionist, base_extractiveness, 2022, 0.7).
narrative_ontology:measurement(npt__be_t2024, npt_article_iv_vi_pairing__abolitionist, base_extractiveness, 2024, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(npt__su_t2010, npt_article_iv_vi_pairing__abolitionist, suppression_requirement, 2010, 0.45).
narrative_ontology:measurement(npt__su_t2014, npt_article_iv_vi_pairing__abolitionist, suppression_requirement, 2014, 0.52).
narrative_ontology:measurement(npt__su_t2017, npt_article_iv_vi_pairing__abolitionist, suppression_requirement, 2017, 0.6).
narrative_ontology:measurement(npt__su_t2020, npt_article_iv_vi_pairing__abolitionist, suppression_requirement, 2020, 0.63).
narrative_ontology:measurement(npt__su_t2022, npt_article_iv_vi_pairing__abolitionist, suppression_requirement, 2022, 0.66).
narrative_ontology:measurement(npt__su_t2024, npt_article_iv_vi_pairing__abolitionist, suppression_requirement, 2024, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(npt_article_iv_vi_pairing__abolitionist, enforcement_mechanism).
narrative_ontology:affects_constraint(npt_article_iv_vi_pairing__abolitionist, npt_article_iv_vi_pairing__nonproliferation_primary).
narrative_ontology:affects_constraint(npt_article_iv_vi_pairing__abolitionist, npt_article_iv_vi_pairing__grand_bargain).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the NPT Article IV/VI pairing kernel. The kernel decomposes into at least three structurally distinct constraints (abolitionist, nonproliferation_primary, grand_bargain) because the epsilon values and beneficiary/victim structures differ radically across readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
