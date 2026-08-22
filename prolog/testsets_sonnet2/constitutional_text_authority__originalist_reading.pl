% ============================================================================
% CONSTRAINT STORY: constitutional_text_authority__originalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_constitutional_text_authority__originalist_reading, []).

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
 *   constraint_id: constitutional_text_authority__originalist_reading
 *   human_readable: Originalist Reading of Constitutional Textual Authority
 *   domain: constitutional_law/legal_theory
 *
 * SUMMARY:
 *   This constraint instantiates the originalist reading of the
 *   constitutional text authority kernel: constitutional meaning is fixed at
 *   ratification and derives its authority from the historical public
 *   understanding of the text's words at the time it was adopted. This is a
 *   specific structural claim distinct from the living-constitutionalist
 *   reading (meaning evolves with contemporary moral principles) and the
 *   positivist reading (validity derives from formal enactment procedure,
 *   independent of moral or historical content). Each reading is authored as
 *   its own constraint with its own epsilon; this file addresses only the
 *   originalist reading.
 *
 * KEY AGENTS:
 *   - originalist_judiciary: agenda_setter (institutional/arbitrage) — applies and enforces the interpretive method
 *   - constitutional_conservative_movement: beneficiary (organized/mobile) — built institutional capital around the doctrine
 *   - legislatures_with_amendment_capacity: nominal beneficiary (institutional/constrained) — holds the formally designated but practically inert amendment channel
 *   - groups_seeking_unenumerated_rights_recognition: payer (powerless/trapped) — bears the evidentiary burden the doctrine imposes
 *   - communities_harmed_by_ratification_era_exclusions: payer (powerless/trapped) — excluded from generating the authoritative historical record
 *   - constitutional_historians: observer (analytical/analytical) — assesses whether the historical record actually supports the confidence the doctrine claims
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(constitutional_text_authority__originalist_reading, 0.52).
domain_priors:suppression_score(constitutional_text_authority__originalist_reading, 0.58).
domain_priors:theater_ratio(constitutional_text_authority__originalist_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(constitutional_text_authority__originalist_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(constitutional_text_authority__originalist_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(constitutional_text_authority__originalist_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(constitutional_text_authority__originalist_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(constitutional_text_authority__originalist_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(constitutional_text_authority__originalist_reading, tangled_rope).
narrative_ontology:human_readable(constitutional_text_authority__originalist_reading, "Originalist Reading of Constitutional Textual Authority").
narrative_ontology:topic_domain(constitutional_text_authority__originalist_reading, "constitutional_law/legal_theory").

domain_priors:requires_active_enforcement(constitutional_text_authority__originalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(constitutional_text_authority__originalist_reading, 'da615e9b-b839-4653-ab5e-5fe1c0466cee').
narrative_ontology:cs_kernel_codification('da615e9b-b839-4653-ab5e-5fe1c0466cee', fixed_text).
narrative_ontology:cs_authority_grounding('da615e9b-b839-4653-ab5e-5fe1c0466cee', lineage).
narrative_ontology:cs_interpretation_layer_present('da615e9b-b839-4653-ab5e-5fe1c0466cee').
narrative_ontology:cs_reading_relation('da615e9b-b839-4653-ab5e-5fe1c0466cee', constitutional_text_authority__living_constitutionalist_reading, forecloses).
narrative_ontology:cs_reading_relation('da615e9b-b839-4653-ab5e-5fe1c0466cee', constitutional_text_authority__positivist_reading, influences).
narrative_ontology:cs_axiom('da615e9b-b839-4653-ab5e-5fe1c0466cee', foundational, meaning_fixed_at_ratification).
narrative_ontology:cs_axiom_status(meaning_fixed_at_ratification, holdable).
narrative_ontology:cs_axiom_grounding('da615e9b-b839-4653-ab5e-5fe1c0466cee', meaning_fixed_at_ratification, conventional).
narrative_ontology:cs_axiom('da615e9b-b839-4653-ab5e-5fe1c0466cee', foundational, article_v_exclusive_change_channel).
narrative_ontology:cs_axiom_status(article_v_exclusive_change_channel, holdable).
narrative_ontology:cs_axiom_grounding('da615e9b-b839-4653-ab5e-5fe1c0466cee', article_v_exclusive_change_channel, conventional).
narrative_ontology:cs_axiom('da615e9b-b839-4653-ab5e-5fe1c0466cee', secondary, judicial_discretion_illegitimate_absent_historical_warrant).
narrative_ontology:cs_axiom_status(judicial_discretion_illegitimate_absent_historical_warrant, holdable).
narrative_ontology:cs_axiom_grounding('da615e9b-b839-4653-ab5e-5fe1c0466cee', judicial_discretion_illegitimate_absent_historical_warrant, instrumental).
narrative_ontology:cs_reference_frame('da615e9b-b839-4653-ab5e-5fe1c0466cee', founding_era_public_meaning).
narrative_ontology:cs_drift_state('da615e9b-b839-4653-ab5e-5fe1c0466cee', contemporary_pluralist_polity, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('da615e9b-b839-4653-ab5e-5fe1c0466cee', '').
narrative_ontology:cs_kernel_id(constitutional_text_authority__originalist_reading, constitutional_text_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(constitutional_text_authority__originalist_reading, originalist_judiciary).
narrative_ontology:constraint_beneficiary(constitutional_text_authority__originalist_reading, constitutional_conservative_movement).
narrative_ontology:constraint_beneficiary(constitutional_text_authority__originalist_reading, legislatures_with_amendment_capacity).
narrative_ontology:constraint_victim(constitutional_text_authority__originalist_reading, groups_seeking_unenumerated_rights_recognition).
narrative_ontology:constraint_victim(constitutional_text_authority__originalist_reading, communities_harmed_by_ratification_era_exclusions).
narrative_ontology:constraint_victim(constitutional_text_authority__originalist_reading, litigants_relying_on_evolving_doctrine).
narrative_ontology:constraint_vindicates(constitutional_text_authority__originalist_reading, popular_sovereignty_at_founding).
narrative_ontology:constraint_vindicates(constitutional_text_authority__originalist_reading, rule_of_law_predictability_doctrine).
narrative_ontology:constraint_vindicates(constitutional_text_authority__originalist_reading, judicial_restraint_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Adjudicates constitutional disputes by reference to historical public meaning at ratification, using this method to constrain the set of permissible outcomes and to declare rival methodologies illegitimate. Gains interpretive authority and doctrinal stability from the method; largely insulated from the consequences of applying it, since judges hold life tenure and exit any personal stake in the outcomes they produce.
narrative_ontology:constraint_stakeholder(constitutional_text_authority__originalist_reading, originalist_judiciary, agenda_setter,
    institutional, civilizational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(constitutional_text_authority__originalist_reading, originalist_judiciary, beneficiary).

% Built extensive institutional infrastructure (legal societies, judicial pipelines, scholarship) around originalism as a discipline against what it views as judicial policymaking. Benefits from the doctrine's capacity to foreclose outcomes it opposes (novel rights claims, regulatory expansions) by requiring historical grounding it can contest on favorable terrain.
narrative_ontology:constraint_stakeholder(constitutional_text_authority__originalist_reading, constitutional_conservative_movement, beneficiary,
    organized, generational, mobile, national).

% Formally hold the Article V amendment power the doctrine designates as the sole legitimate channel for updating constitutional meaning. In practice, supermajority thresholds make this channel nearly unusable, so the formal beneficiary status is largely nominal; legislatures rarely exercise the power the doctrine assigns them.
narrative_ontology:constraint_stakeholder(constitutional_text_authority__originalist_reading, legislatures_with_amendment_capacity, beneficiary,
    institutional, generational, constrained, national).

% Seek judicial recognition of rights or protections not named in ratification-era text or clearly evidenced in ratification-era public understanding (privacy, bodily autonomy, evolving equality claims). Under this reading, must produce historical evidence of founding-era acceptance that frequently does not exist because the founding public did not contemplate their claim at all. Have no realistic exit from the constitutional system and cannot outvote the amendment threshold.
narrative_ontology:constraint_stakeholder(constitutional_text_authority__originalist_reading, groups_seeking_unenumerated_rights_recognition, payer,
    powerless, biographical, trapped, national).

% Were excluded from the political community whose 'public understanding' the doctrine treats as authoritative — their exclusion from ratification-era enfranchisement means the historical record the doctrine privileges was never generated with their interests represented. Bear the ongoing interpretive consequence of a founding public that did not include them.
narrative_ontology:constraint_stakeholder(constitutional_text_authority__originalist_reading, communities_harmed_by_ratification_era_exclusions, payer,
    powerless, generational, trapped, national).

% Structured legal strategy, reliance interests, and settled expectations around decades of precedent built on non-originalist methodology. Face doctrinal reversal or invalidation of settled rights when courts adopt this reading retroactively, with no transition mechanism to protect accumulated reliance.
narrative_ontology:constraint_stakeholder(constitutional_text_authority__originalist_reading, litigants_relying_on_evolving_doctrine, payer,
    moderate, biographical, constrained, national).

% Study what the historical record actually supports about ratification-era understanding, often finding it more contested, fragmentary, or multivocal than the doctrine's confident historical claims suggest. Their findings are cited selectively by advocates on multiple sides rather than treated as dispositive.
narrative_ontology:constraint_stakeholder(constitutional_text_authority__originalist_reading, constitutional_historians, observer,
    analytical, civilizational, analytical, national).

% Hold a competing interpretive methodology that this reading's advocates characterize as illegitimate judicial policymaking. Continue to sit on courts and issue rulings under the rival methodology, but within originalist-dominated fora their approach is treated as outside the bounds of permissible constitutional argument.
narrative_ontology:constraint_stakeholder(constitutional_text_authority__originalist_reading, living_constitutionalist_judges, excluded,
    institutional, civilizational, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(constitutional_text_authority__originalist_reading, originalist_judiciary).
narrative_ontology:fixing_cost_class(constitutional_text_authority__originalist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a determinate, historically-anchored decision procedure that constrains judicial discretion, offers predictability for legal planning, and channels constitutional change through a visible, accountable amendment process rather than diffuse judicial reinterpretation.
% TRANSFER_FUNCTION: Moves the burden of constitutional change from courts (low transaction cost, case-by-case) to legislatures via Article V (extremely high transaction cost, supermajority-gated). This shifts practical power toward whoever already prevailed in the ratification-era political settlement and away from claimants whose interests were not represented in that settlement.
% ABSENT_VOICES: Groups excluded from the ratification-era franchise (women, enslaved and formerly enslaved people, non-property-holders, and others) had no voice in generating the 'historical public understanding' this reading treats as authoritative — their absence from the founding record is treated as an interpretive constraint rather than a defect in the record's authority.
% DISAPPEARANCE_RATIONALE: Originalist advocates would say the world rearranges catastrophically — courts would become unconstrained policymaking bodies untethered from popular sovereignty. Critics would say the world barely changes for most litigants, since courts already exercise substantial interpretive discretion in selecting which historical evidence and which level of generality to apply; the doctrine's determinacy claim is itself contested.
% FOUNDING_PROBLEM: To constrain judicial discretion and root constitutional authority in a democratically ratified text rather than in the evolving moral views of unelected judges, thereby preserving the distinction between judicial and legislative power.
% FOUNDING_PROBLEM_CORROBORATION: Originalist judges and legal scholars attest the problem (judicial policymaking untethered from popular sovereignty) remains fully live. Constitutional historians outside the movement note that 'historical public understanding' is frequently indeterminate or contested even among originalist scholars themselves, and that the doctrine's application in practice often tracks contemporary political commitments dressed in historical argument — corroboration for the doctrine's stated purpose exists, but independent historians dispute that the method reliably delivers the determinacy it promises.
narrative_ontology:disappearance_verdict(constitutional_text_authority__originalist_reading, contested).
narrative_ontology:founding_problem_status(constitutional_text_authority__originalist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(constitutional_text_authority__originalist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(constitutional_text_authority__originalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(constitutional_text_authority__originalist_reading, 0.52, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(constitutional_text_authority__originalist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(constitutional_text_authority__originalist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(constitutional_text_authority__originalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction (0.52) is authored at a moderate-substantial level: the doctrine functions as genuine coordination (predictability, discretion-constraint, democratic-legitimacy claims) but also as asymmetric extraction — it systematically disadvantages claimants whose interests were unrepresented in the ratification-era political settlement, converting that historical exclusion into an ongoing interpretive barrier. Suppression (0.58) reflects that the doctrine requires active judicial enforcement to exclude rival methodologies from legitimate argument space; it is not self-enforcing consensus. Accessibility collapse (0.62) is moderate-high: once a court adopts originalism as controlling method, non-originalist argument is largely foreclosed within that forum, though the methodology itself remains contested at the level of which judges adopt it. Resistance (0.6) is substantial: living constitutionalists, unenumerated-rights advocates, and many historians actively contest the doctrine's determinacy claims.
 *
 * PERSPECTIVAL GAP:
 *   The originalist judiciary and the constitutional conservative movement experience this as principled constraint on judicial overreach — a rope that protects democratic sovereignty. Groups seeking unenumerated rights recognition and communities excluded from the ratification-era franchise experience the identical structure as an extraction mechanism that launders historical exclusion into present-day doctrinal barriers requiring supermajority political mobilization to overcome. The engine should compute meaningfully different per-seat types from these structurally opposed positions given the same base data.
 *
 * DIRECTIONALITY LOGIC:
 *   Originalist judiciary and the conservative legal movement sit near the beneficiary end: they gain interpretive authority, doctrinal victories, and institutional capital from the method's operation, with low personal exposure to its costs. Legislatures are a formal beneficiary only — the amendment power nominally belongs to them, but supermajority thresholds mean this benefit is rarely exercised in practice, which is why the story treats their beneficiary status as largely nominal rather than reflecting active capture. Groups seeking rights recognition and communities excluded from ratification are targets: the doctrine imposes an evidentiary and political burden that falls disproportionately on them because their interests were structurally absent from the historical record the doctrine privileges. Litigants relying on evolving doctrine are moderate targets — they built reliance on a different methodology and bear transition costs when this reading displaces it.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (curbing unconstrained judicial policymaking) may remain partly live even as the doctrine's application increasingly tracks outcome-oriented reasoning dressed in historical argument — the founding_problem_status is authored as contested rather than resolved dead or fully live, because credible arguments exist on both sides and no consensus corroboration exists outside the doctrine's own proponents and its critics.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    originalist_kernel_reading_disambiguation,
    'This constraint is one reading (originalist) of the constitutional_text_authority kernel; the living_constitutionalist_reading and positivist_reading are separate constraints with different beneficiary/victim structures and different epsilon values. Is the choice among these readings itself resolvable, or is it an irreducible interpretive commitment?',
    'No empirical resolution mechanism exists at the level of constitutional theory; the choice among kernel readings is itself a contested jurisprudential commitment, not a fact discoverable by further historical research alone (though historical research can inform which originalist claims are well-supported).',
    'If treated as irreducible, all three readings persist as live, coexisting constraints indefinitely, each claimed as authoritative by different judicial coalitions. If one reading achieves durable institutional dominance (e.g. through sustained judicial appointments), its structural extraction profile could compound over time as reflected in the temporal measurements.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(originalist_kernel_reading_disambiguation, conceptual, 'Whether the choice of interpretive kernel reading is itself resolvable or an irreducible commitment.').

omega_variable(
    historical_indeterminacy,
    'Is ''historical public understanding at ratification'' typically determinate enough to constrain judicial discretion in the way the doctrine claims, or is it frequently indeterminate/contested in ways that let judges select among plausible historical readings to reach preferred outcomes?',
    'Systematic historiographical review comparing originalist judicial opinions against the underlying historical scholarship they cite, assessing rates of scholarly consensus versus contestation on the specific historical claims relied upon.',
    'If historical evidence is frequently indeterminate, the doctrine''s discretion-constraining function is substantially theatrical (raising the effective theater_ratio) and its extraction is closer to that of a snare wearing coordination cover. If historical evidence is typically clear, the coordination function is more genuine and closer to a rope with acceptable coordination cost.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(historical_indeterminacy, empirical, 'Whether ratification-era historical evidence reliably constrains outcomes or is malleable enough to rationalize preferred results.').

omega_variable(
    excluded_founding_public_legitimacy,
    'Does the exclusion of large populations (women, enslaved people, non-property-holders) from the ratification-era political process undermine the doctrine''s claim that ''historical public understanding'' constitutes a legitimate democratic baseline?',
    'This is fundamentally a normative/political-theory question about what counts as legitimate popular sovereignty, not an empirical one; it could be partially informed by comparative analysis of how other originalist-adjacent legal systems have handled analogous founding-exclusion problems (e.g., through subsequent amendment incorporation doctrines).',
    'If the exclusion is taken to substantially undermine legitimacy, the doctrine''s self-justifying democratic-sovereignty rationale weakens considerably, shifting the classification toward tangled_rope with a larger extraction component. If taken as adequately cured by subsequent amendments (13th/14th/15th/19th), the coordination rationale is more defensible.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(excluded_founding_public_legitimacy, preference, 'Whether ratification-era exclusions undermine the doctrine''s democratic-legitimacy foundation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(constitutional_text_authority__originalist_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cons_tr_t0, constitutional_text_authority__originalist_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(cons_tr_t10, constitutional_text_authority__originalist_reading, theater_ratio, 10, 0.18).
narrative_ontology:measurement(cons_tr_t20, constitutional_text_authority__originalist_reading, theater_ratio, 20, 0.2).
narrative_ontology:measurement(cons_tr_t30, constitutional_text_authority__originalist_reading, theater_ratio, 30, 0.23).
narrative_ontology:measurement(cons_tr_t40, constitutional_text_authority__originalist_reading, theater_ratio, 40, 0.26).
narrative_ontology:measurement(cons_tr_t50, constitutional_text_authority__originalist_reading, theater_ratio, 50, 0.28).

% Extraction over time
narrative_ontology:measurement(cons_be_t0, constitutional_text_authority__originalist_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(cons_be_t10, constitutional_text_authority__originalist_reading, base_extractiveness, 10, 0.4).
narrative_ontology:measurement(cons_be_t20, constitutional_text_authority__originalist_reading, base_extractiveness, 20, 0.44).
narrative_ontology:measurement(cons_be_t30, constitutional_text_authority__originalist_reading, base_extractiveness, 30, 0.47).
narrative_ontology:measurement(cons_be_t40, constitutional_text_authority__originalist_reading, base_extractiveness, 40, 0.5).
narrative_ontology:measurement(cons_be_t50, constitutional_text_authority__originalist_reading, base_extractiveness, 50, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(cons_su_t0, constitutional_text_authority__originalist_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(cons_su_t10, constitutional_text_authority__originalist_reading, suppression_requirement, 10, 0.45).
narrative_ontology:measurement(cons_su_t20, constitutional_text_authority__originalist_reading, suppression_requirement, 20, 0.5).
narrative_ontology:measurement(cons_su_t30, constitutional_text_authority__originalist_reading, suppression_requirement, 30, 0.53).
narrative_ontology:measurement(cons_su_t40, constitutional_text_authority__originalist_reading, suppression_requirement, 40, 0.56).
narrative_ontology:measurement(cons_su_t50, constitutional_text_authority__originalist_reading, suppression_requirement, 50, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(constitutional_text_authority__originalist_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(constitutional_text_authority__originalist_reading, 0.12).
narrative_ontology:affects_constraint(constitutional_text_authority__originalist_reading, constitutional_text_authority__living_constitutionalist_reading).
narrative_ontology:affects_constraint(constitutional_text_authority__originalist_reading, constitutional_text_authority__positivist_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the constitutional_text_authority kernel. The living_constitutionalist_reading locates authority in evolving contemporary moral principles applied to changing circumstances; the positivist_reading locates validity in formal enactment procedure independent of moral content. Each reading has its own beneficiary/victim structure and its own epsilon — they are not the same constraint measured three ways but three structurally distinct constraints linked by shared subject matter. This reading (originalist) forecloses the living_constitutionalist_reading within a single coherent interpretive framework (a court cannot simultaneously hold that meaning is fixed at ratification and that it evolves with contemporary values as the primary interpretive method), while it merely influences the positivist_reading (an originalist can also be a positivist about the source of the Constitution's validity, so the two are not strictly incompatible, but originalism's method reshapes the practical stakes and legitimacy conditions the positivist reading operates within).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
