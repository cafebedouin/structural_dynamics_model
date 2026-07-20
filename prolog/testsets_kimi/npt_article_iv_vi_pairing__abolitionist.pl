% ============================================================================
% CONSTRAINT STORY: npt_article_iv_vi_pairing__abolitionist
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
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
 *   human_readable: NPT Article IV-VI Abolitionist Reading
 *   domain: international_law/nuclear_governance
 *
 * SUMMARY:
 *   This constraint story models the abolitionist reading of the NPT Article
 *   IV-VI pairing: Article VI mandates complete nuclear disarmament as a
 *   binding obligation, and Article IV's peaceful-use entitlement is
 *   illegitimate when it perpetuates dual-use proliferation risk. Authority
 *   derives from international humanitarian law and the Treaty on the
 *   Prohibition of Nuclear Weapons (TPNW) precedent. This reading
 *   delegitimizes the NPT as insufficient, renders weapon possession
 *   categorically illegal, and collapses the distinction between peaceful and
 *   military nuclear programs where dual-use risk exists. It is structurally
 *   contested by nuclear weapon states and by states relying on nuclear
 *   energy with latent weapons latency.
 *
 * KEY AGENTS:
 *   - nuclear_weapon_states: Primary target (institutional/constrained) â bear disarmament costs and delegitimization of their security doctrine
 *   - tpnw_adherent_states: Primary agenda-setter (institutional/constrained) â advance the prohibition norm and TPNW framework
 *   - dual_use_reliant_states: Secondary target (moderate/constrained) â bear constraints on Article IV peaceful-use rights
 *   - non_nuclear_weapon_states: Beneficiary (organized/mobile) â gain from disarmament pressure and reduced proliferation risk
 *   - humanitarian_disarmament_caucus: Norm entrepreneur (organized/mobile) â benefits from advancement of the humanitarian framing
 *   - international_judicial_bodies: Analytical observer (institutional/analytical) â adjudicates treaty interpretation disputes
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(npt_article_iv_vi_pairing__abolitionist, 0.72).
domain_priors:suppression_score(npt_article_iv_vi_pairing__abolitionist, 0.6).
domain_priors:theater_ratio(npt_article_iv_vi_pairing__abolitionist, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(npt_article_iv_vi_pairing__abolitionist, extractiveness, 0.72).
narrative_ontology:constraint_metric(npt_article_iv_vi_pairing__abolitionist, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(npt_article_iv_vi_pairing__abolitionist, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(npt_article_iv_vi_pairing__abolitionist, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(npt_article_iv_vi_pairing__abolitionist, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(npt_article_iv_vi_pairing__abolitionist, tangled_rope).
narrative_ontology:human_readable(npt_article_iv_vi_pairing__abolitionist, "NPT Article IV-VI Abolitionist Reading").
narrative_ontology:topic_domain(npt_article_iv_vi_pairing__abolitionist, "international_law/nuclear_governance").

domain_priors:requires_active_enforcement(npt_article_iv_vi_pairing__abolitionist).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(npt_article_iv_vi_pairing__abolitionist, '3c471926-b4b0-4de4-a518-bbd97ed36526').
narrative_ontology:cs_kernel_codification('3c471926-b4b0-4de4-a518-bbd97ed36526', formalized).
narrative_ontology:cs_authority_grounding('3c471926-b4b0-4de4-a518-bbd97ed36526', lineage).
narrative_ontology:cs_interpretation_layer_present('3c471926-b4b0-4de4-a518-bbd97ed36526').
narrative_ontology:cs_reading_relation('3c471926-b4b0-4de4-a518-bbd97ed36526', npt_article_iv_vi_pairing__nonproliferation_primary, forecloses).
narrative_ontology:cs_reading_relation('3c471926-b4b0-4de4-a518-bbd97ed36526', npt_article_iv_vi_pairing__grand_bargain, influences).
narrative_ontology:cs_axiom('3c471926-b4b0-4de4-a518-bbd97ed36526', foundational, categorical_illegality_of_nuclear_possession).
narrative_ontology:cs_axiom_status(categorical_illegality_of_nuclear_possession, holdable).
narrative_ontology:cs_axiom_grounding('3c471926-b4b0-4de4-a518-bbd97ed36526', categorical_illegality_of_nuclear_possession, deontological).
narrative_ontology:cs_axiom('3c471926-b4b0-4de4-a518-bbd97ed36526', foundational, article_iv_constrained_by_humanitarian_risk).
narrative_ontology:cs_axiom_status(article_iv_constrained_by_humanitarian_risk, holdable).
narrative_ontology:cs_axiom_grounding('3c471926-b4b0-4de4-a518-bbd97ed36526', article_iv_constrained_by_humanitarian_risk, deontological).
narrative_ontology:cs_reference_frame('3c471926-b4b0-4de4-a518-bbd97ed36526', categorical_nuclear_prohibition).
narrative_ontology:cs_drift_state('3c471926-b4b0-4de4-a518-bbd97ed36526', post_tpnw_entry_into_force_2021, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('3c471926-b4b0-4de4-a518-bbd97ed36526', '').
narrative_ontology:cs_kernel_id(npt_article_iv_vi_pairing__abolitionist, npt_article_iv_vi_pairing).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(npt_article_iv_vi_pairing__abolitionist, tpnw_adherent_states).
narrative_ontology:constraint_beneficiary(npt_article_iv_vi_pairing__abolitionist, non_nuclear_weapon_states).
narrative_ontology:constraint_beneficiary(npt_article_iv_vi_pairing__abolitionist, humanitarian_disarmament_caucus).
narrative_ontology:constraint_victim(npt_article_iv_vi_pairing__abolitionist, nuclear_weapon_states).
narrative_ontology:constraint_victim(npt_article_iv_vi_pairing__abolitionist, dual_use_reliant_states).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Advocate for the abolitionist reading through the TPNW and NPT review conferences, arguing that Article VI imposes a binding disarmament obligation and that Article IV must yield to humanitarian law norms. They invest diplomatic capital in delegitimizing nuclear possession and expanding the prohibition treaty regime.
narrative_ontology:constraint_stakeholder(npt_article_iv_vi_pairing__abolitionist, tpnw_adherent_states, agenda_setter,
    institutional, generational, constrained, global).

% Maintain nuclear arsenals under the NPT grand bargain while resisting the abolitionist reading as a threat to strategic stability. They face legal and moral pressure to disarm, with their security doctrines characterized as violations of international humanitarian law within this interpretive frame.
narrative_ontology:constraint_stakeholder(npt_article_iv_vi_pairing__abolitionist, nuclear_weapon_states, payer,
    institutional, civilizational, constrained, global).

% Support nuclear disarmament diplomatically and benefit from reduced proliferation risk, but must navigate between NPT commitments and TPNW pressures. They receive disarmament pledges from weapon states that remain largely unfulfilled.
narrative_ontology:constraint_stakeholder(npt_article_iv_vi_pairing__abolitionist, non_nuclear_weapon_states, beneficiary,
    organized, generational, mobile, global).

% Operate nuclear energy programs with fuel-cycle technology that could theoretically support weapons production. Under this reading, their Article IV peaceful-use rights are conditional and potentially illegitimate if the program creates proliferation risk, subjecting them to heightened scrutiny beyond standard safeguards.
narrative_ontology:constraint_stakeholder(npt_article_iv_vi_pairing__abolitionist, dual_use_reliant_states, payer,
    moderate, biographical, constrained, national).

% Comprises NGOs, the ICRC, and scientific groups that documented humanitarian consequences of nuclear weapons and advanced the TPNW. They benefit from the institutionalization of the humanitarian frame and the delegitimization of deterrence doctrine.
narrative_ontology:constraint_stakeholder(npt_article_iv_vi_pairing__abolitionist, humanitarian_disarmament_caucus, beneficiary,
    organized, biographical, mobile, global).

% Adjudicate treaty interpretation disputes and advisory opinion requests concerning Article VI obligations and the relationship between NPT and humanitarian law. They occupy an analytical seat assessing the legal merits of competing readings.
narrative_ontology:constraint_stakeholder(npt_article_iv_vi_pairing__abolitionist, international_judicial_bodies, observer,
    institutional, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates international action toward the total elimination of nuclear weapons by establishing a categorical prohibition on possession and limiting peaceful-use entitlements under Article IV to programs that do not create dual-use proliferation risk.
% TRANSFER_FUNCTION: Transfers the burden of disarmament from non-weapon states' forbearance to weapon states' active dismantlement; transfers authority over Article IV compliance from non-proliferation verification to humanitarian-law-based risk assessment.
% ABSENT_VOICES: Nuclear weapon state security establishments and extended deterrence-dependent allies are formally present in NPT forums but structurally excluded from the humanitarian-law framework that grounds this reading; their security-based objections are treated as illegitimate in this interpretive frame.
% DISAPPEARANCE_RATIONALE: If this reading disappeared, the NPT would revert to a transactional nonproliferation framework where Article IV is an unconditional entitlement and Article VI is aspirational; weapon states would face no mandatory disarmament obligation and dual-use energy states would regain unconditional peaceful-use rights.
% FOUNDING_PROBLEM: The risk of nuclear war and the catastrophic humanitarian consequences of any nuclear detonation, which the NPT's nonproliferation framework alone has failed to eliminate.
% FOUNDING_PROBLEM_CORROBORATION: The International Committee of the Red Cross and independent scientific assessments (e.g., IPPNW nuclear famine studies) attest to the ongoing humanitarian risk; nuclear weapon states' security establishments do not corroborate this framing and instead assert deterrence stability.
narrative_ontology:disappearance_verdict(npt_article_iv_vi_pairing__abolitionist, world_rearranges).
narrative_ontology:founding_problem_status(npt_article_iv_vi_pairing__abolitionist, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(npt_article_iv_vi_pairing__abolitionist, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
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
 *   Extractiveness is high (0.72) because the reading imposes complete disarmament on weapon states and constrains energy programs with dual-use potential. Suppression (0.60) reflects the delegitimization of security-based objections and deterrence doctrine within this interpretive frame. Theater ratio (0.40) captures the ritualistic invocation of Article VI at NPT Review Conferences alongside sustained non-compliance. Accessibility collapse (0.45) is moderate: alternative readings (grand bargain, nonproliferation primary) remain widely accessible and legally defended. Resistance (0.75) is high due to entrenched opposition from nuclear weapon states and their allies.
 *
 * PERSPECTIVAL GAP:
 *   The weapon-state seat experiences this constraint as coercive extraction of strategic capability and sovereign security choice; the TPNW-adherent seat experiences it as necessary humanitarian coordination. The non-nuclear weapon state seat may experience it as either beneficial coordination or unwelcome conditionality depending on their energy profile. The engine computes this divergence from structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   TPNW adherent states and the humanitarian disarmament caucus are structural beneficiaries (d near 0.0) â the constraint advances their normative agenda and institutionalizes their authority. Nuclear weapon states are structural targets (d near 1.0) â they bear the compliance burden and security doctrine delegitimization. Dual-use reliant states are also targets (d high) due to Article IV conditionality. Non-nuclear weapon states without dual-use programs are net beneficiaries. International judicial bodies are symmetric (d near 0.5) as adjudicators.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as tangled rope prevents mislabeling the abolitionist reading as pure extraction (snare) by acknowledging its genuine coordination function: the prevention of nuclear war and humanitarian catastrophe. Conversely, it prevents mislabeling it as pure coordination (rope) by capturing the asymmetric burden on weapon states and the conditionality imposed on peaceful use. The mandatory disarmament obligation is not a voluntary coordination mechanism but an imposed constraint; the humanitarian benefit is real but unevenly distributed.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    npt_sufficiency_ambiguity,
    'Is the NPT treaty framework itself legitimate but incomplete under this reading, or has it been delegitimized entirely by its failure to achieve Article VI compliance?',
    'Treaty practice and state conduct assessment over time, tracking whether abolitionist states continue to engage the NPT as a complementary forum or treat it as irredeemable.',
    'If delegitimized, the constraint shifts toward a prohibition regime external to the NPT; if merely incomplete, the constraint remains an internal corrective reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(npt_sufficiency_ambiguity, conceptual, 'Whether the NPT is delegitimized or merely incomplete under the abolitionist reading.').

omega_variable(
    dual_use_risk_threshold,
    'What empirical threshold distinguishes legitimate peaceful use under Article IV from illegitimate dual-use proliferation risk?',
    'IAEA safeguard effectiveness assessments and breakout-time analysis applied to specific national fuel cycles.',
    'A strict threshold increases extraction from energy states; a loose threshold collapses this reading into the grand_bargain reading.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(dual_use_risk_threshold, empirical, 'Empirical threshold for Article IV legitimacy under humanitarian risk assessment.').

omega_variable(
    article_vi_justiciability,
    'Is Article VI''s disarmament obligation legally justiciable and enforceable, or purely hortatory?',
    'ICJ contentious case or advisory opinion on specific weapon state obligations, or binding tribunal rulings.',
    'If justiciable, extraction on weapon states becomes enforceable law; if hortatory, the reading''s extraction relies on moral-political pressure alone.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(article_vi_justiciability, empirical, 'Legal justiciability of Article VI under the abolitionist reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(npt_article_iv_vi_pairing__abolitionist, 0, 54).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(npt__tr_t0, npt_article_iv_vi_pairing__abolitionist, theater_ratio, 0, 0.2).
narrative_ontology:measurement(npt__tr_t15, npt_article_iv_vi_pairing__abolitionist, theater_ratio, 15, 0.25).
narrative_ontology:measurement(npt__tr_t30, npt_article_iv_vi_pairing__abolitionist, theater_ratio, 30, 0.35).
narrative_ontology:measurement(npt__tr_t40, npt_article_iv_vi_pairing__abolitionist, theater_ratio, 40, 0.4).
narrative_ontology:measurement(npt__tr_t50, npt_article_iv_vi_pairing__abolitionist, theater_ratio, 50, 0.42).
narrative_ontology:measurement(npt__tr_t54, npt_article_iv_vi_pairing__abolitionist, theater_ratio, 54, 0.4).

% Extraction over time
narrative_ontology:measurement(npt__be_t0, npt_article_iv_vi_pairing__abolitionist, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(npt__be_t15, npt_article_iv_vi_pairing__abolitionist, base_extractiveness, 15, 0.3).
narrative_ontology:measurement(npt__be_t30, npt_article_iv_vi_pairing__abolitionist, base_extractiveness, 30, 0.4).
narrative_ontology:measurement(npt__be_t40, npt_article_iv_vi_pairing__abolitionist, base_extractiveness, 40, 0.55).
narrative_ontology:measurement(npt__be_t50, npt_article_iv_vi_pairing__abolitionist, base_extractiveness, 50, 0.68).
narrative_ontology:measurement(npt__be_t54, npt_article_iv_vi_pairing__abolitionist, base_extractiveness, 54, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(npt__su_t0, npt_article_iv_vi_pairing__abolitionist, suppression_requirement, 0, 0.2).
narrative_ontology:measurement(npt__su_t15, npt_article_iv_vi_pairing__abolitionist, suppression_requirement, 15, 0.25).
narrative_ontology:measurement(npt__su_t30, npt_article_iv_vi_pairing__abolitionist, suppression_requirement, 30, 0.35).
narrative_ontology:measurement(npt__su_t40, npt_article_iv_vi_pairing__abolitionist, suppression_requirement, 40, 0.45).
narrative_ontology:measurement(npt__su_t50, npt_article_iv_vi_pairing__abolitionist, suppression_requirement, 50, 0.55).
narrative_ontology:measurement(npt__su_t54, npt_article_iv_vi_pairing__abolitionist, suppression_requirement, 54, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(npt_article_iv_vi_pairing__abolitionist, enforcement_mechanism).
narrative_ontology:affects_constraint(npt_article_iv_vi_pairing__abolitionist, npt_article_iv_vi_pairing__nonproliferation_primary).
narrative_ontology:affects_constraint(npt_article_iv_vi_pairing__abolitionist, npt_article_iv_vi_pairing__grand_bargain).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the npt_article_iv_vi_pairing kernel, decomposed from the colloquial label 'NPT articles IV and VI' into three structurally distinct constraints per the epsilon-invariance principle. The abolitionist reading carries high extractiveness (0.72) and requires active enforcement; the nonproliferation_primary and grand_bargain readings instantiate different constraints with different epsilon values and should be authored as separate linked stories.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
