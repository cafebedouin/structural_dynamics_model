% ============================================================================
% CONSTRAINT STORY: rbio_practice_norm_complex__sovereignty_maximalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_rbio_practice_norm_complex__sovereignty_maximalist_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: rbio_practice_norm_complex__sovereignty_maximalist_reading
 *   human_readable: RBIO Norm Complex — Sovereignty-Maximalist Reading (Non-Intervention Absolutism)
 *   domain: international relations/international law/political economy
 *
 * SUMMARY:
 *   This story instantiates the sovereignty-maximalist reading of the RBIO
 *   (rules-based international order) norm complex: sovereignty is treated as
 *   near-absolute, legitimate use of the norm complex is restricted to
 *   protecting states against external interference, and humanitarian
 *   exceptions (R2P, humanitarian intervention doctrines, conditionality
 *   regimes) are read as pretexts for great-power regime change rather than
 *   as good-faith protective mechanisms. This is ONE of three linked readings
 *   of a single contested kernel (rbio_practice_norm_complex); the
 *   liberal-institutional reading and the hegemonic-extraction reading are
 *   separate constraints with their own ε values and stakeholder sets, not
 *   alternative framings folded into this file. Under this reading's own
 *   lights, the standing arrangement is the doctrine of absolute
 *   non-intervention as actually invoked in multilateral practice — a real
 *   coordination function (shielding against pretextual invasion) that has
 *   hardened into cover for selective impunity-shielding by veto-holding
 *   patrons.
 *
 * KEY AGENTS:
 *   - authoritarian_regime_leadership: primary beneficiary (institutional/arbitrage) — invokes the doctrine to block scrutiny
 *   - populations_under_repressive_governance: primary victim (powerless/trapped) — bears the cost of foreclosed recourse
 *   - permanent_five_veto_holders_shielding_clients: secondary beneficiary/agenda_setter (institutional/arbitrage) — deploys doctrine selectively
 *   - human_rights_monitoring_bodies and middle_power_states_seeking_r2p_norms: excluded voices structurally locked out under this reading
 *   - international_legal_scholarship_community: analytical observer tracing the doctrinal contest across readings
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(rbio_practice_norm_complex__sovereignty_maximalist_reading, 0.62).
domain_priors:suppression_score(rbio_practice_norm_complex__sovereignty_maximalist_reading, 0.7).
domain_priors:theater_ratio(rbio_practice_norm_complex__sovereignty_maximalist_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(rbio_practice_norm_complex__sovereignty_maximalist_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(rbio_practice_norm_complex__sovereignty_maximalist_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(rbio_practice_norm_complex__sovereignty_maximalist_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(rbio_practice_norm_complex__sovereignty_maximalist_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(rbio_practice_norm_complex__sovereignty_maximalist_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(rbio_practice_norm_complex__sovereignty_maximalist_reading, tangled_rope).
narrative_ontology:human_readable(rbio_practice_norm_complex__sovereignty_maximalist_reading, "RBIO Norm Complex — Sovereignty-Maximalist Reading (Non-Intervention Absolutism)").
narrative_ontology:topic_domain(rbio_practice_norm_complex__sovereignty_maximalist_reading, "international relations/international law/political economy").

domain_priors:requires_active_enforcement(rbio_practice_norm_complex__sovereignty_maximalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(rbio_practice_norm_complex__sovereignty_maximalist_reading, '5fc1c1a9-8305-45ef-8bd2-545f733aa6f3').
narrative_ontology:cs_kernel_codification('5fc1c1a9-8305-45ef-8bd2-545f733aa6f3', distributed).
narrative_ontology:cs_authority_grounding('5fc1c1a9-8305-45ef-8bd2-545f733aa6f3', distributed).
narrative_ontology:cs_reading_relation('5fc1c1a9-8305-45ef-8bd2-545f733aa6f3', rbio_practice_norm_complex__liberal_institutional_reading, forecloses).
narrative_ontology:cs_reading_relation('5fc1c1a9-8305-45ef-8bd2-545f733aa6f3', rbio_practice_norm_complex__hegemonic_extraction_reading, coexists_with).
narrative_ontology:cs_axiom('5fc1c1a9-8305-45ef-8bd2-545f733aa6f3', foundational, sovereignty_is_absolute_against_external_interference).
narrative_ontology:cs_axiom_status(sovereignty_is_absolute_against_external_interference, holdable).
narrative_ontology:cs_axiom_grounding('5fc1c1a9-8305-45ef-8bd2-545f733aa6f3', sovereignty_is_absolute_against_external_interference, conventional).
narrative_ontology:cs_axiom('5fc1c1a9-8305-45ef-8bd2-545f733aa6f3', foundational, humanitarian_exception_doctrines_are_presumptively_pretextual).
narrative_ontology:cs_axiom_status(humanitarian_exception_doctrines_are_presumptively_pretextual, holdable).
narrative_ontology:cs_axiom_grounding('5fc1c1a9-8305-45ef-8bd2-545f733aa6f3', humanitarian_exception_doctrines_are_presumptively_pretextual, empirically_contingent).
narrative_ontology:cs_reference_frame('5fc1c1a9-8305-45ef-8bd2-545f733aa6f3', westphalian_absolute_sovereignty_norm).
narrative_ontology:cs_drift_state('5fc1c1a9-8305-45ef-8bd2-545f733aa6f3', post_r2p_and_post_libya_intervention_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('5fc1c1a9-8305-45ef-8bd2-545f733aa6f3', '').
narrative_ontology:cs_kernel_id(rbio_practice_norm_complex__sovereignty_maximalist_reading, rbio_practice_norm_complex).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(rbio_practice_norm_complex__sovereignty_maximalist_reading, authoritarian_regime_leadership).
narrative_ontology:constraint_beneficiary(rbio_practice_norm_complex__sovereignty_maximalist_reading, permanent_five_veto_holders_shielding_clients).
narrative_ontology:constraint_victim(rbio_practice_norm_complex__sovereignty_maximalist_reading, populations_under_repressive_governance).
narrative_ontology:constraint_victim(rbio_practice_norm_complex__sovereignty_maximalist_reading, ethnic_and_religious_minority_groups_at_risk).
narrative_ontology:constraint_vindicates(rbio_practice_norm_complex__sovereignty_maximalist_reading, westphalian_non_intervention_doctrine).
narrative_ontology:constraint_vindicates(rbio_practice_norm_complex__sovereignty_maximalist_reading, un_charter_article_2_4_supremacy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Invokes the sovereignty-maximalist reading of the norm complex to block Security Council action, monitoring missions, or conditional aid, framing any external scrutiny of internal repression as a pretext for regime change. Retains full domestic coercive apparatus and uses the norm as diplomatic cover in multilateral forums while facing no binding external constraint on internal conduct.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__sovereignty_maximalist_reading, authoritarian_regime_leadership, beneficiary,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(rbio_practice_norm_complex__sovereignty_maximalist_reading, authoritarian_regime_leadership, agenda_setter).

% Bear the cost of the sovereignty shield directly: no external recourse when the regime represses them, because the reading treats any external protective action as illegitimate interference. Cannot exit the jurisdiction easily, and the norm complex — as applied under this reading — removes even the diplomatic and multilateral levers that might otherwise create marginal protection.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__sovereignty_maximalist_reading, populations_under_repressive_governance, payer,
    powerless, biographical, trapped, national).

% Face acute risk (including atrocity crimes) precisely in the situations where humanitarian exception doctrines would otherwise trigger protective action; under this reading, such doctrines are read as regime-change pretexts, so protective mechanisms are foreclosed on principle before facts are examined.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__sovereignty_maximalist_reading, ethnic_and_religious_minority_groups_at_risk, payer,
    powerless, immediate, trapped, regional).

% Deploy the sovereignty-maximalist reading selectively — invoking it to veto action against allied or client regimes while supporting humanitarian framings against rivals. The reading gives them a principled-sounding vocabulary for what is, in their case, an asymmetric protection racket for strategic partners.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__sovereignty_maximalist_reading, permanent_five_veto_holders_shielding_clients, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(rbio_practice_norm_complex__sovereignty_maximalist_reading, permanent_five_veto_holders_shielding_clients, beneficiary).

% Document abuses and would advocate for protective mechanisms but are denied access, standing, or enforcement authority under this reading, which treats their reporting itself as an instrument of external interference rather than as legitimate evidence.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__sovereignty_maximalist_reading, human_rights_monitoring_bodies, excluded,
    organized, biographical, constrained, global).

% Advocate for conditional, criteria-based intervention authority (R2P-style doctrines) and are structurally locked out of building durable authority for this position because the sovereignty-maximalist reading treats any such doctrine as inherently illegitimate, regardless of the specific criteria proposed.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__sovereignty_maximalist_reading, middle_power_states_seeking_r2p_norms, excluded,
    powerful, generational, constrained, global).

% Studies the doctrinal contest between sovereignty-maximalist, liberal-institutional, and hegemonic-extraction readings, tracing which reading dominates in which forum and how selective invocation reveals the reading's function independent of its stated principle.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__sovereignty_maximalist_reading, international_legal_scholarship_community, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(rbio_practice_norm_complex__sovereignty_maximalist_reading, diffuse).
narrative_ontology:fixing_cost_class(rbio_practice_norm_complex__sovereignty_maximalist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a stable, predictable baseline against arbitrary external intervention: a state's internal governance cannot be overridden by outside powers absent an armed attack on another state, giving weaker states a nominal shield against great-power coercion dressed as humanitarian concern.
% TRANSFER_FUNCTION: Moves protective leverage away from populations inside a state and toward the incumbent regime controlling that state; also moves selective-shielding capacity toward veto-holding powers who can invoke the doctrine for allied regimes while ignoring it for rivals.
% ABSENT_VOICES: Populations actually living under the repression the doctrine shields have no forum in which their objection to non-intervention counts as a vote; human rights monitoring bodies are treated as interference rather than evidence-bearing parties; their exclusion is structural to the reading, not incidental.
% DISAPPEARANCE_RATIONALE: Regime leadership and their P5 patrons would say the world destabilizes into serial pretextual interventions if the absolute sovereignty reading disappeared; trapped populations and rights bodies would say the world changes for the better because protective mechanisms currently foreclosed on principle would become available for genuine assessment on the facts. The verdict is genuinely disputed along the beneficiary/victim line, not resolvable by appeal to either side alone.
% FOUNDING_PROBLEM: The doctrine descends from post-1945 (and earlier Westphalian) efforts to prevent great powers from using pretextual justifications — religious, civilizational, humanitarian — to invade and dismantle weaker states, a real and repeatedly demonstrated historical pattern of intervention abuse.
% FOUNDING_PROBLEM_CORROBORATION: Non-aligned movement states and postcolonial legal scholars outside the current beneficiary set corroborate that pretextual intervention is a live historical pattern (citing the run-up to the Iraq war and earlier colonial interventions), supporting the doctrine's continued relevance. Independent human rights monitoring bodies and R2P-aligned international lawyers — also outside the beneficiary set — corroborate that the doctrine's absolutist form, as currently invoked, is being used to block even criteria-based, non-pretextual protective mechanisms, indicating the founding problem has been overtaken by a distinct and newer abuse pattern (impunity shielding) that the original doctrine was not built to address.
narrative_ontology:disappearance_verdict(rbio_practice_norm_complex__sovereignty_maximalist_reading, contested).
narrative_ontology:founding_problem_status(rbio_practice_norm_complex__sovereignty_maximalist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(rbio_practice_norm_complex__sovereignty_maximalist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(rbio_practice_norm_complex__sovereignty_maximalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(rbio_practice_norm_complex__sovereignty_maximalist_reading, 0.62, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(rbio_practice_norm_complex__sovereignty_maximalist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(rbio_practice_norm_complex__sovereignty_maximalist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(rbio_practice_norm_complex__sovereignty_maximalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction (0.62) reflects that the coordination function (shielding weak states from pretextual invasion) is real but has been substantially captured: the doctrine, as invoked, disproportionately protects incumbent repressive leadership rather than the populations sovereignty is nominally meant to serve. Suppression (0.70) is high because the reading actively forecloses even criteria-based, non-pretextual protective doctrines on principle, not merely on the facts of a given case — this is a structural suppression of an entire category of protective mechanism, not case-by-case adjudication. Theater ratio (0.45) captures that a substantial share of diplomatic invocation of 'sovereignty' functions performatively — asserted uniformly regardless of whether pretext is actually present in the case at hand — while accessibility_collapse (0.50) and resistance (0.68) reflect that alternative doctrinal readings (liberal-institutional, hegemonic-extraction) remain very much alive and contested rather than fully displaced; this is an active doctrinal fight, not settled law.
 *
 * DIRECTIONALITY LOGIC:
 *   Authoritarian regime leadership sits near the full-beneficiary end: institutional power, arbitrage-grade exit from scrutiny, collects the shielding effect directly. Populations under repressive governance sit at the full-target end: powerless, trapped, bearing the cost of foreclosed external recourse with no coalition-formation capacity across borders. P5 veto-holders occupy an unusual dual position — they are beneficiaries of the doctrine's protective shield for their own conduct and their clients', while simultaneously being the agenda-setters who apply the doctrine selectively; this asymmetric application (invoking sovereignty for allies, ignoring it for rivals) is itself part of what makes this reading tangled-rope rather than a pure rope: there is a genuine coordination function (protection against pretextual invasion) riding alongside an asymmetric extraction (selective, capture-driven application).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — pretextual great-power intervention dressed as humanitarian concern — was real and remains partially live (the Iraq precedent corroborates it). But the doctrine's absolutist form, as currently invoked, has drifted from case-by-case pretext-screening toward blanket foreclosure of an entire category of protective doctrine. This is the mandatrophy signature: a mandate (screen out bad-faith intervention) that has outlived its original scope and now serves impunity-shielding for a different set of beneficiaries than those it was built to protect. The classification prevents mislabeling this as pure extraction (there IS a real historical coordination problem it solves) while also preventing it from being laundered as pure principled coordination (the selective P5 application and the trapped-victim set are not incidental).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sovereignty_maximalist_vs_kernel_readings,
    'Is the sovereignty-maximalist reading a defensible independent doctrinal tradition (rooted in genuine anti-colonial and anti-pretextual-invasion concerns), or is it functionally indistinguishable in practice from the hegemonic-extraction reading''s account of selective P5 shielding — differing only in the moral vocabulary used to describe the same selective-application pattern?',
    'Comparative case analysis: track invocation patterns of the sovereignty-maximalist doctrine across UN Security Council votes, coding for whether invocation correlates with (a) genuine absence of atrocity evidence or (b) alliance/client relationship with a veto-holder, independent of evidence quality.',
    'If invocation correlates primarily with alliance relationships rather than evidentiary pretext-screening, the sovereignty-maximalist reading''s own self-account (screening bad-faith intervention) collapses into the hegemonic-extraction reading''s account (selective shielding), which would argue for treating this reading''s coordination claim as substantially theatrical rather than substantive.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sovereignty_maximalist_vs_kernel_readings, empirical, 'Whether the maximalist reading is a genuine independent doctrine or extraction-reading in different vocabulary.').

omega_variable(
    trapped_population_recourse_alternatives,
    'In the absence of the sovereignty-maximalist doctrine''s foreclosure, would populations under repressive governance actually gain meaningfully more protective recourse, or would relaxed intervention norms primarily enable new forms of great-power exploitation dressed as humanitarianism — i.e., would removing this reading help the victims it names, or merely swap one extraction structure for another?',
    'Historical comparison of outcomes under R2P-invoked interventions (Libya, etc.) versus non-intervention cases, assessing post-intervention governance and population welfare outcomes against a counterfactual of continued non-intervention.',
    'If historical R2P invocations produced worse outcomes for the populations they claimed to protect, the sovereignty-maximalist reading''s core empirical claim (humanitarian exceptions are typically pretextual) gains substantial evidentiary support, shifting this reading''s own claimed_type toward something closer to a defensible rope; if outcomes were net-protective, the tangled_rope/snare characterization strengthens.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(trapped_population_recourse_alternatives, empirical, 'Whether relaxing the doctrine would help or further harm the populations it currently traps.').

omega_variable(
    kernel_framing_under_determination,
    'Is the ''kernel'' here best framed as the UN Charter''s Article 2(4)/2(7) textual commitment to sovereign non-interference, or as the broader practiced legitimacy claim layered above it (the diplomatic and rhetorical apparatus by which states justify invoking or withholding that Charter language case by case)? The textual framing suggests a formalized, relatively stable kernel; the practiced-legitimacy framing suggests a distributed, contested kernel with no single adjudicating authority.',
    'Track whether Charter text itself has changed (it has not, materially, since 1945) versus whether the practiced interpretive apparatus around it has undergone visible shifts (R2P''s 2005 emergence, its invocation and retreat post-Libya) — divergence between textual stability and practice volatility would support the practiced-legitimacy framing as the operative kernel.',
    'Under the textual framing, kernel_codification would be ''fixed_text'' with lineage-style authority (treaty interpretation tradition); under the practiced-legitimacy framing, kernel_codification is better described as ''distributed'' with no single interpretive authority, which is the framing adopted in this story''s cs_structure. Adopting the textual framing instead would shift authority_grounding toward ''lineage'' and could change how drift is characterized (practice_drift vs. axiom_overriding).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_framing_under_determination, conceptual, 'Whether the kernel is the fixed Charter text or the distributed practiced-legitimacy apparatus around it.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(rbio_practice_norm_complex__sovereignty_maximalist_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(rbio_tr_t0, rbio_practice_norm_complex__sovereignty_maximalist_reading, theater_ratio, 0, 0.3).
narrative_ontology:measurement(rbio_tr_t5, rbio_practice_norm_complex__sovereignty_maximalist_reading, theater_ratio, 5, 0.33).
narrative_ontology:measurement(rbio_tr_t10, rbio_practice_norm_complex__sovereignty_maximalist_reading, theater_ratio, 10, 0.36).
narrative_ontology:measurement(rbio_tr_t15, rbio_practice_norm_complex__sovereignty_maximalist_reading, theater_ratio, 15, 0.39).
narrative_ontology:measurement(rbio_tr_t20, rbio_practice_norm_complex__sovereignty_maximalist_reading, theater_ratio, 20, 0.41).
narrative_ontology:measurement(rbio_tr_t25, rbio_practice_norm_complex__sovereignty_maximalist_reading, theater_ratio, 25, 0.43).
narrative_ontology:measurement(rbio_tr_t30, rbio_practice_norm_complex__sovereignty_maximalist_reading, theater_ratio, 30, 0.45).

% Extraction over time
narrative_ontology:measurement(rbio_be_t0, rbio_practice_norm_complex__sovereignty_maximalist_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(rbio_be_t5, rbio_practice_norm_complex__sovereignty_maximalist_reading, base_extractiveness, 5, 0.47).
narrative_ontology:measurement(rbio_be_t10, rbio_practice_norm_complex__sovereignty_maximalist_reading, base_extractiveness, 10, 0.51).
narrative_ontology:measurement(rbio_be_t15, rbio_practice_norm_complex__sovereignty_maximalist_reading, base_extractiveness, 15, 0.55).
narrative_ontology:measurement(rbio_be_t20, rbio_practice_norm_complex__sovereignty_maximalist_reading, base_extractiveness, 20, 0.58).
narrative_ontology:measurement(rbio_be_t25, rbio_practice_norm_complex__sovereignty_maximalist_reading, base_extractiveness, 25, 0.6).
narrative_ontology:measurement(rbio_be_t30, rbio_practice_norm_complex__sovereignty_maximalist_reading, base_extractiveness, 30, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(rbio_su_t0, rbio_practice_norm_complex__sovereignty_maximalist_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(rbio_su_t5, rbio_practice_norm_complex__sovereignty_maximalist_reading, suppression_requirement, 5, 0.58).
narrative_ontology:measurement(rbio_su_t10, rbio_practice_norm_complex__sovereignty_maximalist_reading, suppression_requirement, 10, 0.61).
narrative_ontology:measurement(rbio_su_t15, rbio_practice_norm_complex__sovereignty_maximalist_reading, suppression_requirement, 15, 0.64).
narrative_ontology:measurement(rbio_su_t20, rbio_practice_norm_complex__sovereignty_maximalist_reading, suppression_requirement, 20, 0.66).
narrative_ontology:measurement(rbio_su_t25, rbio_practice_norm_complex__sovereignty_maximalist_reading, suppression_requirement, 25, 0.68).
narrative_ontology:measurement(rbio_su_t30, rbio_practice_norm_complex__sovereignty_maximalist_reading, suppression_requirement, 30, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(rbio_practice_norm_complex__sovereignty_maximalist_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(rbio_practice_norm_complex__sovereignty_maximalist_reading, 0.1).
narrative_ontology:affects_constraint(rbio_practice_norm_complex__sovereignty_maximalist_reading, rbio_practice_norm_complex__liberal_institutional_reading).
narrative_ontology:affects_constraint(rbio_practice_norm_complex__sovereignty_maximalist_reading, rbio_practice_norm_complex__hegemonic_extraction_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the rbio_practice_norm_complex kernel. sovereignty_maximalist_reading (this file) authors sovereignty as near-absolute and humanitarian exceptions as presumptively pretextual, with authoritarian regime leadership and shielding veto-holders as beneficiaries and trapped populations as victims (ε=0.62, tangled_rope). liberal_institutional_reading authors the norm complex as universal and consent-based, with enforcement gaps as a capacity problem rather than a legitimacy problem (expected substantially lower ε, closer to rope). hegemonic_extraction_reading authors the same norm complex as a frozen hegemonic project whose formal revisability is practically foreclosed by P5 veto structure, reading enforcement selectivity as evidence of extractive intent (expected higher ε, closer to snare, given the extraction-reading's assessment of intentional capture rather than incidental capacity failure). All three share the same underlying kernel (the RBIO practice-norm complex) but instantiate structurally distinct constraints with different beneficiary/victim sets and different ε — per the ε-invariance principle, they are linked here rather than merged.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
