% ============================================================================
% CONSTRAINT STORY: dollar_gold_convertibility__strict_convertibility_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_dollar_gold_convertibility__strict_convertibility_reading, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
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
 *   constraint_id: dollar_gold_convertibility__strict_convertibility_reading
 *   human_readable: Article IV Gold Convertibility as Strict Legal Obligation
 *   domain: international_political_economy/monetary_history/international_law
 *
 * SUMMARY:
 *   Article IV, Section 1 of the original IMF Articles of Agreement required
 *   the United States to maintain the convertibility of the dollar into gold
 *   at $35 per ounce for foreign monetary authorities. The strict
 *   convertibility reading treats this not as a policy convenience or
 *   conditional guideline but as a binding legal obligation that structurally
 *   subordinated U.S. monetary sovereignty to international creditor
 *   interests. Under this reading, the U.S. Treasury and Federal Reserve were
 *   constrained issuersâvictims of an international legal architecture that
 *   extracted domestic policy spaceâwhile creditor nations and surplus
 *   states were beneficiaries holding enforceable, gold-denominated claims.
 *   This constraint is one reading of a contested kernel; sibling readings
 *   (policy-flexible and Triffin-structural) assign different directionality
 *   and beneficiary-victim structures. The claim-metric independence
 *   principle is observed: the constraint is claimed as tangled_rope (genuine
 *   coordination function in the Bretton Woods order, asymmetric extraction
 *   from the issuer) while the metrics are authored descriptively for the
 *   1945â1975 interval.
 *
 * KEY AGENTS:
 *   - us_treasury_federal_reserve: Primary target (institutional/constrained) â bears extraction through lost monetary autonomy and gold reserve defense
 *   - creditor_nations: Primary beneficiary (institutional/mobile) â holds enforceable conversion claims and benefits from issuer discipline
 *   - imf_executive_board: Agenda setter (institutional/constrained) â administers the legal framework and interprets the obligation
 *   - us_domestic_constituencies: Excluded party (organized/constrained) â bears domestic unemployment and credit costs without direct voice
 *   - international_law_scholars: Analytical observer (analytical/analytical) â evaluates the legal bindingness of the obligation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dollar_gold_convertibility__strict_convertibility_reading, 0.78).
domain_priors:suppression_score(dollar_gold_convertibility__strict_convertibility_reading, 0.68).
domain_priors:theater_ratio(dollar_gold_convertibility__strict_convertibility_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dollar_gold_convertibility__strict_convertibility_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(dollar_gold_convertibility__strict_convertibility_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(dollar_gold_convertibility__strict_convertibility_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(dollar_gold_convertibility__strict_convertibility_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(dollar_gold_convertibility__strict_convertibility_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dollar_gold_convertibility__strict_convertibility_reading, tangled_rope).
narrative_ontology:human_readable(dollar_gold_convertibility__strict_convertibility_reading, "Article IV Gold Convertibility as Strict Legal Obligation").
narrative_ontology:topic_domain(dollar_gold_convertibility__strict_convertibility_reading, "international_political_economy/monetary_history/international_law").

domain_priors:requires_active_enforcement(dollar_gold_convertibility__strict_convertibility_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dollar_gold_convertibility__strict_convertibility_reading, '9869bd89-d6b4-467c-8490-e73660b5ddf3').
narrative_ontology:cs_kernel_codification('9869bd89-d6b4-467c-8490-e73660b5ddf3', formalized).
narrative_ontology:cs_authority_grounding('9869bd89-d6b4-467c-8490-e73660b5ddf3', lineage).
narrative_ontology:cs_interpretation_layer_present('9869bd89-d6b4-467c-8490-e73660b5ddf3').
narrative_ontology:cs_reading_relation('9869bd89-d6b4-467c-8490-e73660b5ddf3', dollar_gold_convertibility__policy_flexible_reading, forecloses).
narrative_ontology:cs_reading_relation('9869bd89-d6b4-467c-8490-e73660b5ddf3', dollar_gold_convertibility__triffin_structural_reading, influences).
narrative_ontology:cs_axiom('9869bd89-d6b4-467c-8490-e73660b5ddf3', foundational, unconditional_par_value_obligation).
narrative_ontology:cs_axiom_status(unconditional_par_value_obligation, holdable).
narrative_ontology:cs_axiom_grounding('9869bd89-d6b4-467c-8490-e73660b5ddf3', unconditional_par_value_obligation, conventional).
narrative_ontology:cs_reference_frame('9869bd89-d6b4-467c-8490-e73660b5ddf3', fixed_par_value_system).
narrative_ontology:cs_drift_state('9869bd89-d6b4-467c-8490-e73660b5ddf3', post_bretton_woods_collapse, gap(axiom_overriding, severe, true)).
narrative_ontology:cs_created_at('9869bd89-d6b4-467c-8490-e73660b5ddf3', '').
narrative_ontology:cs_kernel_id(dollar_gold_convertibility__strict_convertibility_reading, dollar_gold_convertibility).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dollar_gold_convertibility__strict_convertibility_reading, creditor_nations).
narrative_ontology:constraint_victim(dollar_gold_convertibility__strict_convertibility_reading, us_treasury_federal_reserve).
narrative_ontology:constraint_vindicates(dollar_gold_convertibility__strict_convertibility_reading, gold_standard_discipline_doctrine).
narrative_ontology:constraint_vindicates(dollar_gold_convertibility__strict_convertibility_reading, fixed_exchange_rate_credibility_hypothesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Jointly bears the obligation to exchange dollars for gold at $35 per ounce upon demand by foreign monetary authorities. Expansionary domestic policy directly threatens gold reserve adequacy and invites speculative attack. Exit requires abrogating the Articles of Agreement or unilateral suspension, either of which destroys the reserve currency credibility that underwrites dollar seigniorage.
narrative_ontology:constraint_stakeholder(dollar_gold_convertibility__strict_convertibility_reading, us_treasury_federal_reserve, payer,
    institutional, generational, constrained, global).

% Hold dollar reserves with a legal claim to convert them into gold at par under Article IV, Section 1. Benefit from the confidence premium on dollar assets and from the external policy discipline the constraint imposes on the issuer. Can enforce compliance through coordinated conversion demands that drain U.S. gold reserves or through IMF complaint procedures.
narrative_ontology:constraint_stakeholder(dollar_gold_convertibility__strict_convertibility_reading, creditor_nations, beneficiary,
    institutional, generational, mobile, global).

% Administers the Articles of Agreement and monitors compliance with exchange obligations. Provides the legal and surveillance framework within which convertibility is defined and enforced. Does not directly collect gains but maintains institutional authority from the system's operation; its legitimacy depends on treating Article IV as binding law.
narrative_ontology:constraint_stakeholder(dollar_gold_convertibility__strict_convertibility_reading, imf_executive_board, agenda_setter,
    institutional, generational, constrained, global).

% Bear the domestic macroeconomic costs of monetary policy subordinated to external convertibility, including credit tightening and employment sacrifice. Were not parties to the 1944 Bretton Woods negotiations and lack direct voice in IMF Article IV interpretation; their interests enter only indirectly through Treasury and Federal Reserve political accountability.
narrative_ontology:constraint_stakeholder(dollar_gold_convertibility__strict_convertibility_reading, us_domestic_constituencies, excluded,
    organized, biographical, constrained, national).

% Analyze the treaty text and state practice to determine whether Article IV created a genuine legal obligation or a political commitment dressed in legal form. Their interpretations feed into competing readings of the convertibility kernel but do not themselves bear costs or collect benefits.
narrative_ontology:constraint_stakeholder(dollar_gold_convertibility__strict_convertibility_reading, international_law_scholars, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(dollar_gold_convertibility__strict_convertibility_reading, creditor_nations).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a nominal anchor for the post-war international monetary system by tying the reserve currency to gold at a fixed par value, preventing competitive devaluations and enabling trade reconstruction under exchange rate stability.
% TRANSFER_FUNCTION: Transfers monetary policy autonomy from the U.S. issuer to the international creditor community; the U.S. must subordinate domestic interest-rate and fiscal decisions to the defense of gold reserves, while creditors gain a risk-free gold put on their dollar holdings.
% ABSENT_VOICES: U.S. domestic constituencies facing unemployment and credit scarcity under restrictive policy; developing nations without significant gold or dollar reserves who were marginal to Bretton Woods drafting; Keynesian adjustment-advocates who favored adjustable pegs over rigid convertibility.
% DISAPPEARANCE_RATIONALE: If Article IV convertibility vanished overnight, the dollar would detach from gold, U.S. monetary policy would reorient toward domestic stabilization, creditor nations would face immediate devaluation of dollar reserves as the gold guarantee disappeared, and the international monetary system would lose its nominal anchor and fragment into regional blocs or flexible rates.
% FOUNDING_PROBLEM: Post-WWII need for a stable reserve currency to finance trade reconstruction without returning to the 1930s competitive devaluations and exchange controls; need for a credible commitment device to prevent the reserve currency issuer from inflating away foreign claims.
% FOUNDING_PROBLEM_CORROBORATION: The creditor nations and the IMF attested the problem was live in 1944. Independent economic historians corroborate that the commitment device was structurally necessary given the interwar collapse. However, these same outside scholars attest the founding problem was solved by the 1960s and the arrangement persisted as an extractive constraint beyond its functional lifespan; no current corroboration exists from outside the historical beneficiary set that the problem remained live after 1968.
narrative_ontology:disappearance_verdict(dollar_gold_convertibility__strict_convertibility_reading, world_rearranges).
narrative_ontology:founding_problem_status(dollar_gold_convertibility__strict_convertibility_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(dollar_gold_convertibility__strict_convertibility_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(dollar_gold_convertibility__strict_convertibility_reading, 'none', 1).
narrative_ontology:epsilon_provenance(dollar_gold_convertibility__strict_convertibility_reading, 0.78, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(dollar_gold_convertibility__strict_convertibility_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(dollar_gold_convertibility__strict_convertibility_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(dollar_gold_convertibility__strict_convertibility_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.78) because the constraint transferred substantial monetary policy autonomy from the issuer to external creditors; suppression is moderate-high (0.68) because enforcement relied on gold-conversion threats and IMF legal authority rather than direct violence, but exit was structurally costly for the U.S. Theater ratio (0.42) reflects the performative maintenance of the gold window after 1960âstatements of confidence, gold pool interventions, and swap arrangements that masked the unsustainability of the strict obligation. Resistance (0.72) captures the escalating U.S. resistance through the 1960s (interest equalization tax, capital controls, eventual suspension). The temporal series trace rising extraction and theater as the Triffin dilemma intensified, peaking at the 1971 Nixon shock, then collapsing as the constraint died.
 *
 * PERSPECTIVAL GAP:
 *   The U.S. payer seat and the creditor-nation beneficiary seat experience the same legal text as entirely different constraints: for creditors it is a property right in gold convertibility; for the U.S. it is a fiscal straitjacket. The IMF agenda-setter seat experiences it as a legal mandate to defend. The engine computes this divergence from the structural declarations; the authored claim does not adjudicate between the seats.
 *
 * DIRECTIONALITY LOGIC:
 *   The U.S. Treasury/Federal Reserve is the structural target: victim declaration, constrained exit, generational horizon, institutional power but trapped by the reserve currency roleâd near full target. Creditor nations are the structural beneficiaries: mobile exit (can diversify reserves or convert), legal claims in their favorâd near full beneficiary. The IMF Executive Board sits near symmetric but slightly toward beneficiary side: it gains institutional authority from administering a binding legal order, though it does not directly collect the extraction. Domestic constituencies are excluded targets with no institutional voice.
 *
 * MANDATROPHY ANALYSIS:
 *   The strict reading prevents mislabeling the constraint as pure coordination (Rope) by insisting on the victim set: the U.S. issuer genuinely lost policy autonomy. It prevents mislabeling as pure extraction (Snare) by acknowledging the genuine coordination functionâBretton Woods did stabilize exchange rates and reconstruct trade. The Tangled Rope classification captures the hybrid: a real collective-goods mechanism (nominal anchor, non-competitive devaluation) that asymmetrically extracted from the reserve currency issuer. The founding problem (stable post-war reserve system) was live in 1945 but dead by the late 1960s; the constraint persisted by inertia and creditor enforcement until the 1971 rupture.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'Is Article IV convertibility best understood as a strictly binding legal obligation, a conditionally flexible policy instrument, or a structurally unsustainable design requiring systemic revision?',
    'Comparative legal analysis of IMF Article IV drafting history, travaux prÃ©paratoires, and subsequent amendment records to determine which reading captures the institutional intent and operational practice.',
    'Resolves whether the U.S. was structurally a victim of international law or a consenting architect with exit options; determines whether the strict reading is a false naturalization of a contingent political bargain.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'This constraint is one reading of a contested kernel; sibling readings change the beneficiary-victim structure and the directionality of extraction.').

omega_variable(
    legal_enforceability_against_sovereign,
    'Could creditor nations have enforced Article IV convertibility against the United States through IMF mechanisms or international adjudication, or did sovereign immunity and power asymmetry render the obligation unenforceable in practice?',
    'Counterfactual legal simulation and review of IMF enforcement powers under original Articles; analysis of whether creditor retaliation through gold conversion counts as legal enforcement or market discipline.',
    'If unenforceable, the constraint is a coordination fiction with extraction enforced by market panic rather than legal obligation; if enforceable, the legal architecture genuinely transferred policy autonomy.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legal_enforceability_against_sovereign, empirical, 'Whether the legal obligation had practical enforcement mechanisms beyond market exit.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Was U.S. compliance with convertibility driven by internalized legitimacy of the Bretton Woods order or by structural coercion through gold drain and creditor retaliation?',
    'Post-1971 policy trajectory analysis: if the U.S. immediately reverted to unilateral monetary management without institutional trauma, suppression was structural; if institutionalists fought to restore convertibility, suppression was partially internalized.',
    'Internalized suppression would mean the constraint extracted more effectively than structural metrics suggest; structural suppression means extraction ended cleanly with Nixon''s suspension.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural versus internalized suppression mechanism in sovereign monetary constraints.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dollar_gold_convertibility__strict_convertibility_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dgc_strict_tr_t0, dollar_gold_convertibility__strict_convertibility_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(dgc_strict_tr_t6, dollar_gold_convertibility__strict_convertibility_reading, theater_ratio, 6, 0.25).
narrative_ontology:measurement(dgc_strict_tr_t12, dollar_gold_convertibility__strict_convertibility_reading, theater_ratio, 12, 0.32).
narrative_ontology:measurement(dgc_strict_tr_t18, dollar_gold_convertibility__strict_convertibility_reading, theater_ratio, 18, 0.4).
narrative_ontology:measurement(dgc_strict_tr_t24, dollar_gold_convertibility__strict_convertibility_reading, theater_ratio, 24, 0.5).
narrative_ontology:measurement(dgc_strict_tr_t26, dollar_gold_convertibility__strict_convertibility_reading, theater_ratio, 26, 0.58).
narrative_ontology:measurement(dgc_strict_tr_t30, dollar_gold_convertibility__strict_convertibility_reading, theater_ratio, 30, 0.75).

% Extraction over time
narrative_ontology:measurement(dgc_strict_be_t0, dollar_gold_convertibility__strict_convertibility_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(dgc_strict_be_t6, dollar_gold_convertibility__strict_convertibility_reading, base_extractiveness, 6, 0.42).
narrative_ontology:measurement(dgc_strict_be_t12, dollar_gold_convertibility__strict_convertibility_reading, base_extractiveness, 12, 0.52).
narrative_ontology:measurement(dgc_strict_be_t18, dollar_gold_convertibility__strict_convertibility_reading, base_extractiveness, 18, 0.65).
narrative_ontology:measurement(dgc_strict_be_t24, dollar_gold_convertibility__strict_convertibility_reading, base_extractiveness, 24, 0.78).
narrative_ontology:measurement(dgc_strict_be_t26, dollar_gold_convertibility__strict_convertibility_reading, base_extractiveness, 26, 0.85).
narrative_ontology:measurement(dgc_strict_be_t30, dollar_gold_convertibility__strict_convertibility_reading, base_extractiveness, 30, 0.1).

% Suppression requirement over time
narrative_ontology:measurement(dgc_strict_su_t0, dollar_gold_convertibility__strict_convertibility_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(dgc_strict_su_t6, dollar_gold_convertibility__strict_convertibility_reading, suppression_requirement, 6, 0.48).
narrative_ontology:measurement(dgc_strict_su_t12, dollar_gold_convertibility__strict_convertibility_reading, suppression_requirement, 12, 0.55).
narrative_ontology:measurement(dgc_strict_su_t18, dollar_gold_convertibility__strict_convertibility_reading, suppression_requirement, 18, 0.62).
narrative_ontology:measurement(dgc_strict_su_t24, dollar_gold_convertibility__strict_convertibility_reading, suppression_requirement, 24, 0.72).
narrative_ontology:measurement(dgc_strict_su_t26, dollar_gold_convertibility__strict_convertibility_reading, suppression_requirement, 26, 0.78).
narrative_ontology:measurement(dgc_strict_su_t30, dollar_gold_convertibility__strict_convertibility_reading, suppression_requirement, 30, 0.2).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(dollar_gold_convertibility__strict_convertibility_reading, dollar_gold_convertibility__policy_flexible_reading).
narrative_ontology:affects_constraint(dollar_gold_convertibility__strict_convertibility_reading, dollar_gold_convertibility__triffin_structural_reading).

% DUAL FORMULATION NOTE:
% The natural-language concept 'dollar-gold convertibility' decomposes into three structurally distinct constraints under the epsilon-invariance principle. The strict reading claims high extractiveness from the U.S. issuer; the flexible reading claims conditional coordination function with low extraction; the Triffin reading claims structural unsustainability with systemic rather than bilateral extraction. Each carries independent epsilon, stakeholder sets, and classification. This decomposition follows the BGS gold standard.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
