% ============================================================================
% CONSTRAINT STORY: westphalian_sovereignty__graduated_sovereignty
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_westphalian_sovereignty__graduated_sovereignty, []).

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
 *   constraint_id: westphalian_sovereignty__graduated_sovereignty
 *   human_readable: Graduated Sovereignty Doctrine — Capacity/Legitimacy-Indexed Sovereignty Spectrum
 *   domain: international_law/political_philosophy/global_governance
 *
 * SUMMARY:
 *   This constraint instantiates the 'graduated sovereignty' reading of the
 *   Westphalian sovereignty kernel: the claim that sovereignty is not a
 *   binary legal status but a spectrum whose position is determined by
 *   measurable state capacity and governance legitimacy. Unlike absolute
 *   sovereignty (categorical non-interference) or conditional sovereignty
 *   (triggered by rights violations), graduated sovereignty builds a
 *   continuous, technocratic-looking scale into the legal architecture itself
 *   — and hands the authority to locate any given state on that scale to the
 *   same powers and institutions that stand to benefit from downgrading it.
 *   The doctrine's plausible coordination function (calibrating aid and
 *   assistance to real capacity) is real but thin; its actual operative
 *   history shows the classification apparatus being wielded overwhelmingly
 *   against post-colonial and small states, by institutions in which those
 *   states hold minimal or no voting power.
 *
 * KEY AGENTS:
 *   - permanent_security_council_members: agenda_setter (institutional/arbitrage) — author and apply classification criteria
 *   - international_financial_institutions: agenda_setter/beneficiary (institutional/arbitrage) — operationalize the spectrum via conditionality
 *   - intervening_powers: beneficiary (powerful/arbitrage) — gain legal cover for intervention
 *   - fragile_state_governments: payer (moderate/trapped) — bear reclassification consequences
 *   - post_colonial_states: payer (moderate/constrained) — argue metrics encode colonial residue, not current deficiency
 *   - populations_of_reclassified_states: payer (powerless/trapped) — bear concrete costs without participation
 *   - international_law_scholars: observer (analytical) — document the beneficiary/target asymmetry
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(westphalian_sovereignty__graduated_sovereignty, 0.62).
domain_priors:suppression_score(westphalian_sovereignty__graduated_sovereignty, 0.58).
domain_priors:theater_ratio(westphalian_sovereignty__graduated_sovereignty, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(westphalian_sovereignty__graduated_sovereignty, extractiveness, 0.62).
narrative_ontology:constraint_metric(westphalian_sovereignty__graduated_sovereignty, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(westphalian_sovereignty__graduated_sovereignty, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(westphalian_sovereignty__graduated_sovereignty, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(westphalian_sovereignty__graduated_sovereignty, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(westphalian_sovereignty__graduated_sovereignty, snare).
narrative_ontology:human_readable(westphalian_sovereignty__graduated_sovereignty, "Graduated Sovereignty Doctrine — Capacity/Legitimacy-Indexed Sovereignty Spectrum").
narrative_ontology:topic_domain(westphalian_sovereignty__graduated_sovereignty, "international_law/political_philosophy/global_governance").

domain_priors:requires_active_enforcement(westphalian_sovereignty__graduated_sovereignty).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(westphalian_sovereignty__graduated_sovereignty, '5df5cf54-8d7b-4b7b-b8fb-ac6733f97aa6').
narrative_ontology:cs_kernel_codification('5df5cf54-8d7b-4b7b-b8fb-ac6733f97aa6', distributed).
narrative_ontology:cs_authority_grounding('5df5cf54-8d7b-4b7b-b8fb-ac6733f97aa6', extraction).
narrative_ontology:cs_interpretation_layer_present('5df5cf54-8d7b-4b7b-b8fb-ac6733f97aa6').
narrative_ontology:cs_reading_relation('5df5cf54-8d7b-4b7b-b8fb-ac6733f97aa6', westphalian_sovereignty__absolute_sovereignty, forecloses).
narrative_ontology:cs_reading_relation('5df5cf54-8d7b-4b7b-b8fb-ac6733f97aa6', westphalian_sovereignty__conditional_sovereignty, influences).
narrative_ontology:cs_axiom('5df5cf54-8d7b-4b7b-b8fb-ac6733f97aa6', foundational, sovereignty_is_continuous_capacity_indexed_gradient).
narrative_ontology:cs_axiom_status(sovereignty_is_continuous_capacity_indexed_gradient, holdable).
narrative_ontology:cs_axiom_grounding('5df5cf54-8d7b-4b7b-b8fb-ac6733f97aa6', sovereignty_is_continuous_capacity_indexed_gradient, empirically_contingent).
narrative_ontology:cs_axiom('5df5cf54-8d7b-4b7b-b8fb-ac6733f97aa6', secondary, external_classifying_authority_is_legitimate_absent_discrete_trigger).
narrative_ontology:cs_axiom_status(external_classifying_authority_is_legitimate_absent_discrete_trigger, holdable).
narrative_ontology:cs_axiom_grounding('5df5cf54-8d7b-4b7b-b8fb-ac6733f97aa6', external_classifying_authority_is_legitimate_absent_discrete_trigger, conventional).
narrative_ontology:cs_reference_frame('5df5cf54-8d7b-4b7b-b8fb-ac6733f97aa6', westphalian_territorial_equality).
narrative_ontology:cs_drift_state('5df5cf54-8d7b-4b7b-b8fb-ac6733f97aa6', post_cold_war_intervention_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('5df5cf54-8d7b-4b7b-b8fb-ac6733f97aa6', '').
narrative_ontology:cs_kernel_id(westphalian_sovereignty__graduated_sovereignty, westphalian_sovereignty).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(westphalian_sovereignty__graduated_sovereignty, permanent_security_council_members).
narrative_ontology:constraint_beneficiary(westphalian_sovereignty__graduated_sovereignty, international_financial_institutions).
narrative_ontology:constraint_beneficiary(westphalian_sovereignty__graduated_sovereignty, intervening_powers).
narrative_ontology:constraint_beneficiary(westphalian_sovereignty__graduated_sovereignty, credit_rating_agencies).
narrative_ontology:constraint_victim(westphalian_sovereignty__graduated_sovereignty, fragile_state_governments).
narrative_ontology:constraint_victim(westphalian_sovereignty__graduated_sovereignty, post_colonial_states).
narrative_ontology:constraint_victim(westphalian_sovereignty__graduated_sovereignty, populations_of_reclassified_states).
narrative_ontology:constraint_victim(westphalian_sovereignty__graduated_sovereignty, small_state_diplomatic_corps).
narrative_ontology:constraint_vindicates(westphalian_sovereignty__graduated_sovereignty, responsible_sovereignty_doctrine).
narrative_ontology:constraint_vindicates(westphalian_sovereignty__graduated_sovereignty, state_capacity_gradient_theory).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Control the institutional apparatus (Security Council vetoes, IMF board weights, bilateral aid conditionality) that determines which states get classified as having 'full' sovereignty versus 'limited' or 'conditional' sovereignty. They author the capacity/legitimacy metrics used to make the classification and are never themselves subjected to the same assessment.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__graduated_sovereignty, permanent_security_council_members, agenda_setter,
    institutional, generational, arbitrage, global).

% Set the governance-quality benchmarks (rule of law indices, corruption perception scores, institutional capacity metrics) that operationalize the sovereignty spectrum for lending and intervention purposes. Their conditionality frameworks convert the doctrine into concrete leverage over borrowing states' domestic policy.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__graduated_sovereignty, international_financial_institutions, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(westphalian_sovereignty__graduated_sovereignty, international_financial_institutions, beneficiary).

% Cite the graduated sovereignty doctrine to justify trusteeship arrangements, resource-access agreements, and security interventions in states classified as capacity-deficient. Gain legal and rhetorical cover for actions that would be illegal against a state recognized as fully sovereign.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__graduated_sovereignty, intervening_powers, beneficiary,
    powerful, biographical, arbitrage, global).

% Produce the governance and institutional-quality assessments that feed directly into a state's position on the sovereignty spectrum, with material consequences for borrowing costs. Their assessments are treated as neutral technical measurement despite driving concrete extraction and intervention decisions.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__graduated_sovereignty, credit_rating_agencies, beneficiary,
    organized, biographical, arbitrage, global).

% Classified as occupying a lower position on the sovereignty spectrum due to capacity deficits often originating in colonial extraction or externally imposed debt structures. Face conditional recognition, trusteeship proposals, and intervention threats justified by their own classification; cannot exit the classification system because doing so requires the recognition and financing that only the classifying institutions provide.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__graduated_sovereignty, fragile_state_governments, payer,
    moderate, biographical, trapped, national).

% Argue the capacity metrics used to place them lower on the sovereignty spectrum measure the residue of colonial institutional destruction, not any inherent deficiency — yet the doctrine treats the metric as neutral and current, erasing the causal history. Their diplomatic exit options are constrained by dependence on the same institutions doing the classifying.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__graduated_sovereignty, post_colonial_states, payer,
    moderate, generational, constrained, national).

% Bear the concrete costs of reclassification-triggered interventions, austerity conditionality, or trusteeship arrangements without having participated in the classification process. Cannot relocate their citizenship or appeal the sovereignty grading applied to their state.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__graduated_sovereignty, populations_of_reclassified_states, payer,
    powerless, biographical, trapped, local).

% Would object that the capacity/legitimacy criteria are drafted, weighted, and applied by the very powers that benefit from downgrading their sovereignty, but have no seat in the bodies that set the classification criteria and no forum where the doctrine's premises are put to a genuine vote.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__graduated_sovereignty, small_state_diplomatic_corps, excluded,
    powerless, biographical, constrained, national).

% Study the doctrine's application record, documenting the correlation between which states get downgraded and which powers benefit, without themselves being subject to the doctrine's operation.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__graduated_sovereignty, international_law_scholars, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(westphalian_sovereignty__graduated_sovereignty, permanent_security_council_members).
narrative_ontology:fixing_cost_class(westphalian_sovereignty__graduated_sovereignty, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: In principle, distinguishing states by actual administrative and institutional capacity could help calibrate appropriate levels of external assistance, aid modality, and multilateral support to genuine on-the-ground conditions, avoiding one-size-fits-all treatment of radically different state capabilities.
% TRANSFER_FUNCTION: Moves discretionary authority to classify, intervene in, and set conditions upon states from the classified states themselves to the institutions and powers that author and apply the capacity/legitimacy metrics; moves resource access, resource-extraction rights, and policy autonomy from downgraded states to intervening powers and lending institutions.
% ABSENT_VOICES: The states most frequently classified as capacity-deficient are structurally absent from the bodies (Security Council permanent membership, IFI voting-share allocation, credit-rating methodology committees) that define and apply the classification criteria; they experience the doctrine's consequences without having authored or ratified its metrics.
% DISAPPEARANCE_RATIONALE: If the graduated sovereignty doctrine vanished overnight, intervening powers and IFIs would lose their primary legal-rhetorical basis for conditionality regimes and trusteeship-style arrangements; states currently classified as capacity-deficient would revert to the formal equal-sovereignty baseline of the UN Charter, closing off a major channel through which external actors currently justify differential treatment.
% FOUNDING_PROBLEM: The post-Cold War proliferation of state collapse, civil war, and humanitarian catastrophe in weak states appeared to demand some framework more nuanced than absolute non-interference, since absolute sovereignty seemed to shield genuine governance failures and mass atrocity from any external response.
% FOUNDING_PROBLEM_CORROBORATION: Intervening powers and IFI officials attest the doctrine remains necessary to calibrate assistance to real capacity gaps. Independent international law scholars and post-colonial state representatives — outside the beneficiary set — attest that the doctrine's capacity metrics correlate more strongly with which states lack Security Council veto power than with any independent measure of governance failure, and that its primary contemporary function is providing legal cover for resource-access and strategic-basing arrangements rather than addressing the humanitarian problem it was framed to solve.
narrative_ontology:disappearance_verdict(westphalian_sovereignty__graduated_sovereignty, world_rearranges).
narrative_ontology:founding_problem_status(westphalian_sovereignty__graduated_sovereignty, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(westphalian_sovereignty__graduated_sovereignty, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(westphalian_sovereignty__graduated_sovereignty, 'none', 1).
narrative_ontology:epsilon_provenance(westphalian_sovereignty__graduated_sovereignty, 0.62, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(westphalian_sovereignty__graduated_sovereignty_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(westphalian_sovereignty__graduated_sovereignty, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(westphalian_sovereignty__graduated_sovereignty_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is authored at 0.62 by interval end: high, but not maximal, because the doctrine retains some genuine calibration function distinguishing it from pure predation. Suppression sits at 0.58 — substantial, since the classification apparatus is backed by conditionality, intervention threat, and credit-access leverage, but not absolute, since classified states retain formal diplomatic voice (even if unheeded). Theater ratio rises to 0.40 across the interval as the technocratic capacity/legitimacy metrics increasingly function as post-hoc justification for interventions and conditionality decisions already made on strategic grounds — the metric apparatus performs neutrality it does not possess. Accessibility collapse is moderate (0.50): alternative sovereignty framings exist and are actively argued by post-colonial states, so collapse is real but not complete. Resistance is substantial (0.60), reflecting sustained diplomatic and scholarly pushback from downgraded states and their advocates.
 *
 * DIRECTIONALITY LOGIC:
 *   Permanent Security Council members and IFIs sit at the extreme beneficiary end: they author the classification criteria, are never themselves classified, and hold arbitrage-grade exit from any consequence of the doctrine's operation. Intervening powers and credit rating agencies similarly benefit through legal cover and market leverage respectively. Fragile state governments and post-colonial states sit near the full-target end: trapped or constrained exit, no voice in criteria-setting, and concrete costs (conditionality, intervention, resource-access loss) flowing directly from their classification. Populations of reclassified states are the most extracted-from seat — powerless, trapped, and bearing costs decided entirely elsewhere.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (weak-state collapse and mass atrocity demanding some response more nuanced than absolute non-interference) retains partial life — humanitarian catastrophe in fragile states is real. But the doctrine's application record shows its actual function has drifted from addressing that problem toward providing legal-rhetorical cover for resource access and strategic positioning, while the classification criteria themselves remain unaccountable to the states they grade. This is exactly the mismatch the founding_problem_status/disappearance_verdict pairing is built to surface: status is contested, verdict is world_rearranges — a live-but-contested founding problem combined with concrete downstream dependency is the signature of a constraint whose coordination cover has substantially decoupled from its stated purpose without fully dissolving it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    graduated_vs_conditional_reading_boundary,
    'Is graduated sovereignty structurally distinct from conditional sovereignty, or is it conditional sovereignty''s classification apparatus described in continuous rather than binary terms?',
    'Compare intervention triggers across documented cases: conditional sovereignty readings cite discrete, publicly-legible rights violations (genocide, ethnic cleansing) as triggers; graduated sovereignty readings cite continuous capacity/legitimacy indices with no discrete trigger event. If graduated-sovereignty interventions consistently occur without a discrete triggering violation, the readings are structurally distinct constraints, not a relabeling of one.',
    'If the readings collapse into one, the ε values and beneficiary/victim sets of the two stories should converge; if they remain distinct, graduated sovereignty''s higher discretion for classifying powers (no discrete trigger required) supports its higher authored ε relative to conditional_sovereignty.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(graduated_vs_conditional_reading_boundary, conceptual, 'Whether graduated and conditional sovereignty are genuinely distinct kernel readings or the same reading in different vocabulary.').

omega_variable(
    capacity_metric_neutrality,
    'Are the state-capacity and governance-legitimacy metrics used to place states on the sovereignty spectrum methodologically neutral technical measurements, or are they constructed in ways that systematically track colonial history and current geopolitical alignment rather than independent governance quality?',
    'Statistical analysis of capacity/legitimacy index scores against (a) former colonial status, (b) alignment with permanent Security Council members, and (c) independently audited governance outcomes, controlling for GDP and population — if colonial status and geopolitical alignment predict scores better than audited outcomes, the metrics are not neutral.',
    'If metrics are shown non-neutral, the doctrine''s coordination cover collapses further and the classification of the constraint moves more decisively toward snare; if metrics prove genuinely predictive of independent governance outcomes net of colonial history and alignment, the coordination function is stronger than authored here and a downward revision of ε would be warranted in a future draw.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(capacity_metric_neutrality, empirical, 'Whether the doctrine''s classification metrics are neutral or systematically biased by colonial history and power alignment.').

omega_variable(
    reclassification_reversibility,
    'Can a state classified as capacity-deficient under this doctrine ever be reclassified upward through its own institutional improvement, or does downgraded status tend to be self-perpetuating once assigned?',
    'Longitudinal tracking of states downgraded under graduated-sovereignty-style frameworks (IFI conditionality classifications, fragile-state indices) to determine whether upgrade rates are comparable to downgrade rates, or whether the classification exhibits hysteresis (sticky downward, rare upward).',
    'If reclassification is effectively one-directional (down but rarely up), this strengthens the snare reading — the spectrum functions as a ratchet rather than a genuine graduated assessment; if bidirectional, the coordination framing gains credibility.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reclassification_reversibility, empirical, 'Whether the sovereignty spectrum classification is reversible or acts as a one-way ratchet.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(westphalian_sovereignty__graduated_sovereignty, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(west_tr_t0, westphalian_sovereignty__graduated_sovereignty, theater_ratio, 0, 0.25).
narrative_ontology:measurement(west_tr_t5, westphalian_sovereignty__graduated_sovereignty, theater_ratio, 5, 0.28).
narrative_ontology:measurement(west_tr_t10, westphalian_sovereignty__graduated_sovereignty, theater_ratio, 10, 0.32).
narrative_ontology:measurement(west_tr_t15, westphalian_sovereignty__graduated_sovereignty, theater_ratio, 15, 0.35).
narrative_ontology:measurement(west_tr_t20, westphalian_sovereignty__graduated_sovereignty, theater_ratio, 20, 0.37).
narrative_ontology:measurement(west_tr_t25, westphalian_sovereignty__graduated_sovereignty, theater_ratio, 25, 0.39).
narrative_ontology:measurement(west_tr_t30, westphalian_sovereignty__graduated_sovereignty, theater_ratio, 30, 0.4).

% Extraction over time
narrative_ontology:measurement(west_be_t0, westphalian_sovereignty__graduated_sovereignty, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(west_be_t5, westphalian_sovereignty__graduated_sovereignty, base_extractiveness, 5, 0.44).
narrative_ontology:measurement(west_be_t10, westphalian_sovereignty__graduated_sovereignty, base_extractiveness, 10, 0.5).
narrative_ontology:measurement(west_be_t15, westphalian_sovereignty__graduated_sovereignty, base_extractiveness, 15, 0.55).
narrative_ontology:measurement(west_be_t20, westphalian_sovereignty__graduated_sovereignty, base_extractiveness, 20, 0.58).
narrative_ontology:measurement(west_be_t25, westphalian_sovereignty__graduated_sovereignty, base_extractiveness, 25, 0.6).
narrative_ontology:measurement(west_be_t30, westphalian_sovereignty__graduated_sovereignty, base_extractiveness, 30, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(west_su_t0, westphalian_sovereignty__graduated_sovereignty, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(west_su_t5, westphalian_sovereignty__graduated_sovereignty, suppression_requirement, 5, 0.45).
narrative_ontology:measurement(west_su_t10, westphalian_sovereignty__graduated_sovereignty, suppression_requirement, 10, 0.49).
narrative_ontology:measurement(west_su_t15, westphalian_sovereignty__graduated_sovereignty, suppression_requirement, 15, 0.52).
narrative_ontology:measurement(west_su_t20, westphalian_sovereignty__graduated_sovereignty, suppression_requirement, 20, 0.55).
narrative_ontology:measurement(west_su_t25, westphalian_sovereignty__graduated_sovereignty, suppression_requirement, 25, 0.57).
narrative_ontology:measurement(west_su_t30, westphalian_sovereignty__graduated_sovereignty, suppression_requirement, 30, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(westphalian_sovereignty__graduated_sovereignty, enforcement_mechanism).
narrative_ontology:affects_constraint(westphalian_sovereignty__graduated_sovereignty, westphalian_sovereignty__absolute_sovereignty).
narrative_ontology:affects_constraint(westphalian_sovereignty__graduated_sovereignty, westphalian_sovereignty__conditional_sovereignty).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the westphalian_sovereignty kernel, decomposed per the ε-invariance principle because the natural-language label 'state sovereignty' conflates structurally distinct claims with different ε. absolute_sovereignty claims categorical non-interference (low ε, near-mountain in its own reading); conditional_sovereignty claims a discrete rights-violation trigger for legitimate intervention (moderate ε, tangled_rope-flavored); graduated_sovereignty (this file) claims a continuous capacity/legitimacy spectrum with classifying discretion vested in external powers (highest ε among the three, snare-flavored, per the expected structural delta). Each reading gets its own beneficiary/victim structure and its own classification; they are linked here, not merged.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
