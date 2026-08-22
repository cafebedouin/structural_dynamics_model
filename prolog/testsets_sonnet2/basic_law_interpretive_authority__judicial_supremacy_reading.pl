% ============================================================================
% CONSTRAINT STORY: basic_law_interpretive_authority__judicial_supremacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_basic_law_interpretive_authority__judicial_supremacy_reading, []).

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
 *   constraint_id: basic_law_interpretive_authority__judicial_supremacy_reading
 *   human_readable: Judicial Supremacy Reading of Constitutional Interpretive Authority
 *   domain: constitutional_law/political_theory
 *
 * SUMMARY:
 *   This story authors the judicial supremacy reading of a contested kernel
 *   about final constitutional interpretive authority: that specialized legal
 *   expertise and structural independence from electoral pressure justify
 *   vesting terminal interpretive power in courts. It is one of three
 *   readings of the same kernel — parliamentary sovereignty and popular
 *   constitutionalism are separate constraints, not alternative measurements
 *   of this one. This reading's ε is authored for the judicial-supremacy
 *   arrangement as its own proponents and critics within that arrangement
 *   would assess it (moderate-rising extraction, moderate suppression via
 *   appointment insulation and precedent-setting authority), not for either
 *   sibling arrangement.
 *
 * KEY AGENTS:
 *   - constitutional_judiciary: primary beneficiary and agenda-setter (institutional/analytical exit) — holds terminal interpretive authority and self-defines its own jurisdictional limits
 *   - appellate_bar and constitutional_law_academy: secondary beneficiaries (organized/mobile) — professional classes whose standing depends on the arrangement's persistence
 *   - elected_legislature and electoral_majorities: primary targets (powerful and organized/constrained exit) — bear invalidation and drafting-around costs with no ordinary override
 *   - policy_dependent_minorities: dual-positioned payer/beneficiary (powerless/trapped) — protection or exposure depends entirely on doctrine they cannot vote to change
 *   - comparative_constitutional_scholars: analytical observer — cross-jurisdictional empirical record on countermajoritarian arrangements
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(basic_law_interpretive_authority__judicial_supremacy_reading, 0.58).
domain_priors:suppression_score(basic_law_interpretive_authority__judicial_supremacy_reading, 0.52).
domain_priors:theater_ratio(basic_law_interpretive_authority__judicial_supremacy_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(basic_law_interpretive_authority__judicial_supremacy_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(basic_law_interpretive_authority__judicial_supremacy_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(basic_law_interpretive_authority__judicial_supremacy_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(basic_law_interpretive_authority__judicial_supremacy_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(basic_law_interpretive_authority__judicial_supremacy_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(basic_law_interpretive_authority__judicial_supremacy_reading, tangled_rope).
narrative_ontology:human_readable(basic_law_interpretive_authority__judicial_supremacy_reading, "Judicial Supremacy Reading of Constitutional Interpretive Authority").
narrative_ontology:topic_domain(basic_law_interpretive_authority__judicial_supremacy_reading, "constitutional_law/political_theory").

domain_priors:requires_active_enforcement(basic_law_interpretive_authority__judicial_supremacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(basic_law_interpretive_authority__judicial_supremacy_reading, '50e3750e-fe9d-43cd-88f1-a3d700d98808').
narrative_ontology:cs_kernel_codification('50e3750e-fe9d-43cd-88f1-a3d700d98808', formalized).
narrative_ontology:cs_authority_grounding('50e3750e-fe9d-43cd-88f1-a3d700d98808', expertise).
narrative_ontology:cs_interpretation_layer_present('50e3750e-fe9d-43cd-88f1-a3d700d98808').
narrative_ontology:cs_reading_relation('50e3750e-fe9d-43cd-88f1-a3d700d98808', basic_law_interpretive_authority__parliamentary_sovereignty_reading, forecloses).
narrative_ontology:cs_reading_relation('50e3750e-fe9d-43cd-88f1-a3d700d98808', basic_law_interpretive_authority__popular_constitutionalism_reading, coexists_with).
narrative_ontology:cs_axiom('50e3750e-fe9d-43cd-88f1-a3d700d98808', foundational, judicial_expertise_yields_superior_constitutional_interpretation).
narrative_ontology:cs_axiom_status(judicial_expertise_yields_superior_constitutional_interpretation, holdable).
narrative_ontology:cs_axiom_grounding('50e3750e-fe9d-43cd-88f1-a3d700d98808', judicial_expertise_yields_superior_constitutional_interpretation, instrumental).
narrative_ontology:cs_axiom('50e3750e-fe9d-43cd-88f1-a3d700d98808', foundational, electoral_insulation_is_necessary_for_impartial_rights_adjudication).
narrative_ontology:cs_axiom_status(electoral_insulation_is_necessary_for_impartial_rights_adjudication, holdable).
narrative_ontology:cs_axiom_grounding('50e3750e-fe9d-43cd-88f1-a3d700d98808', electoral_insulation_is_necessary_for_impartial_rights_adjudication, empirically_contingent).
narrative_ontology:cs_reference_frame('50e3750e-fe9d-43cd-88f1-a3d700d98808', marbury_style_judicial_review_settlement).
narrative_ontology:cs_drift_state('50e3750e-fe9d-43cd-88f1-a3d700d98808', contemporary_constitutional_courts, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('50e3750e-fe9d-43cd-88f1-a3d700d98808', '').
narrative_ontology:cs_kernel_id(basic_law_interpretive_authority__judicial_supremacy_reading, basic_law_interpretive_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(basic_law_interpretive_authority__judicial_supremacy_reading, constitutional_judiciary).
narrative_ontology:constraint_beneficiary(basic_law_interpretive_authority__judicial_supremacy_reading, appellate_bar).
narrative_ontology:constraint_beneficiary(basic_law_interpretive_authority__judicial_supremacy_reading, constitutional_law_academy).
narrative_ontology:constraint_victim(basic_law_interpretive_authority__judicial_supremacy_reading, elected_legislature).
narrative_ontology:constraint_victim(basic_law_interpretive_authority__judicial_supremacy_reading, electoral_majorities).
narrative_ontology:constraint_victim(basic_law_interpretive_authority__judicial_supremacy_reading, policy_dependent_minorities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(basic_law_interpretive_authority__judicial_supremacy_reading, policy_dependent_minorities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sits as the terminal interpreter of constitutional text, can invalidate legislation and executive action, and is insulated from electoral removal by life or long tenure. Justifies this authority by appeal to specialized legal training and independence from majoritarian pressure. Its own rulings define the scope of its own jurisdiction (justiciability, standing), which it controls without external override.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_authority__judicial_supremacy_reading, constitutional_judiciary, agenda_setter,
    institutional, civilizational, analytical, national).
narrative_ontology:stakeholder_secondary_role(basic_law_interpretive_authority__judicial_supremacy_reading, constitutional_judiciary, beneficiary).

% Specializes in constitutional litigation before the courts that hold final interpretive authority; the more centralized and durable that authority, the more valuable specialized appellate advocacy becomes as a professional asset. Benefits from the constraint's persistence regardless of outcome direction.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_authority__judicial_supremacy_reading, appellate_bar, beneficiary,
    organized, biographical, mobile, national).

% Produces the doctrinal scholarship that legitimates judicial supremacy as expertise-grounded, trains the judges and litigators who staff the system, and derives professional and institutional standing from the courts' claim to specialized competence. Has structural incentive to reinforce the expertise premise.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_authority__judicial_supremacy_reading, constitutional_law_academy, beneficiary,
    organized, generational, mobile, national).

% Passes statutes that can be struck down by judicial review with no ordinary legislative override; must either draft around anticipated judicial doctrine, pursue the difficult path of constitutional amendment, or accept nullification. Bears the transaction cost of legislating under a standing veto it did not consent to in any given instance.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_authority__judicial_supremacy_reading, elected_legislature, payer,
    powerful, biographical, constrained, national).

% Vote for representatives and policy platforms that can be nullified by courts insulated from electoral accountability; their expressed democratic will through ordinary lawmaking channels can be overridden by an institution they cannot vote out or directly petition. Recourse is limited to slow appointment-cycle influence or constitutional amendment supermajorities.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_authority__judicial_supremacy_reading, electoral_majorities, payer,
    organized, biographical, constrained, national).

% Groups whose statutory protections or exclusions hinge entirely on how courts read the constitution; when judicial doctrine shifts, protections won through the ordinary legislative process can be invalidated without any democratic input from the affected group, or conversely a hostile legislative majority's action can be blocked in their favor. Their material situation depends on a body they cannot lobby through electoral channels, cutting both ways.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_authority__judicial_supremacy_reading, policy_dependent_minorities, payer,
    powerless, biographical, trapped, national).
narrative_ontology:stakeholder_secondary_role(basic_law_interpretive_authority__judicial_supremacy_reading, policy_dependent_minorities, beneficiary).

% Implements and defends laws subject to judicial invalidation, and appoints judges but cannot compel interpretive outcomes once appointments are made; largely excluded from the ongoing interpretive process except through appointment power exercised at long intervals and through litigation posture.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_authority__judicial_supremacy_reading, executive_branch, excluded,
    institutional, biographical, constrained, national).

% Study cross-national patterns of judicial review, democratic backsliding, and countermajoritarian difficulty; document empirical outcomes of judicial supremacy against sibling arrangements (parliamentary sovereignty, popular constitutionalism) without holding a stake in any single jurisdiction's arrangement.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_authority__judicial_supremacy_reading, comparative_constitutional_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(basic_law_interpretive_authority__judicial_supremacy_reading, constitutional_judiciary).
narrative_ontology:fixing_cost_class(basic_law_interpretive_authority__judicial_supremacy_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a stable, technically consistent final interpretive authority so constitutional meaning does not shift with every legislative majority, reducing coordination costs for long-horizon commitments (property rights, individual liberties, structural federalism) that would be unstable if resolved by simple majority vote each cycle.
% TRANSFER_FUNCTION: Moves final say over the meaning and validity of legislation from elected representatives and the electoral majorities behind them to an appointed judiciary and the professional class (litigators, scholars) that services it; gridlock and drafting costs shift onto the legislative process whenever courts narrow the space of permissible statutes.
% ABSENT_VOICES: Electoral majorities whose statutory preferences are invalidated have no direct voice in the interpretive body that overrides them; policy-dependent minorities protected by judicial doctrine likewise cannot be heard through ordinary channels if a future court reverses course. Both are structurally present only as litigants or amici, never as principals the court answers to.
% DISAPPEARANCE_RATIONALE: If judicial supremacy vanished overnight and interpretive finality reverted to the legislature or to ongoing popular contestation, constitutional litigation strategy would collapse, the appellate bar's core practice area would shrink, legislatures would regain the ability to override doctrine by ordinary statute, and long-settled precedent would become renegotiable — the professional, political, and doctrinal landscape would reorganize substantially.
% FOUNDING_PROBLEM: Early constitutional systems needed a mechanism to prevent transient legislative majorities from unwinding entrenched structural commitments (rights guarantees, federal divisions of power, minority protections) through ordinary lawmaking, and to provide a stable, technically expert forum for resolving genuine textual ambiguity.
% FOUNDING_PROBLEM_CORROBORATION: Sitting judges and constitutional law scholars attest the problem remains live, citing continued legislative overreach and the need for counter-majoritarian checks. Political scientists studying comparative democratic backsliding and legislative-branch representatives attest the arrangement has drifted from checking overreach to routine judicial policymaking with no legislative recourse; this dissenting attestation comes from outside the beneficiary set (comparative scholarship, legislative testimony) rather than from the judiciary or bar itself.
narrative_ontology:disappearance_verdict(basic_law_interpretive_authority__judicial_supremacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(basic_law_interpretive_authority__judicial_supremacy_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(basic_law_interpretive_authority__judicial_supremacy_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(basic_law_interpretive_authority__judicial_supremacy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(basic_law_interpretive_authority__judicial_supremacy_reading, 0.58, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(basic_law_interpretive_authority__judicial_supremacy_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(basic_law_interpretive_authority__judicial_supremacy_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(basic_law_interpretive_authority__judicial_supremacy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction rises across the interval (0.34 to 0.58) reflecting the accumulation of doctrine that expands the scope of judicial invalidation over time — a pattern consistent with courts progressively defining the boundaries of their own authority in ways that are difficult to reverse through ordinary politics. Suppression tracks closely (0.35 to 0.52) because the mechanism that keeps the arrangement in place — insulation from electoral accountability, precedent's binding weight, high amendment thresholds — is itself a form of structural suppression of legislative and electoral correction, not merely a byproduct. Theater ratio is comparatively low and rising modestly (0.12 to 0.28): the expertise-and-independence justification is not empty performance — courts do resolve genuine textual ambiguity — but a growing share of judicial activity extends into policy domains the expertise rationale does not obviously cover, which is where theater creeps in.
 *
 * PERSPECTIVAL GAP:
 *   From the judiciary's own seat, this arrangement is principled counter-majoritarian protection exercised through specialized expertise — a rope. From the legislature's seat facing an unreviewable veto on a specific statute it passed with electoral backing, the same structure operates as an externally imposed cost with no recourse in the ordinary political process — closer to extraction. The engine computes this divergence from the authored power/exit/scope data per seat; the claimed_type here (tangled_rope) is the generating model's structural judgment across seats, authored independently of and prior to the metric values.
 *
 * DIRECTIONALITY LOGIC:
 *   The judiciary is the clear structural beneficiary: it collects institutional authority, self-defines jurisdiction, and faces no direct electoral correction (d near the full-beneficiary end). The appellate bar and legal academy are secondary beneficiaries whose professional value is a direct function of the arrangement's persistence and centralization. The legislature and electoral majorities are the structural targets: they bear the cost of invalidation and the difficulty of override, with constrained exit (amendment thresholds, appointment-cycle timelines) rather than trapped exit, since electoral and constitutional-amendment channels remain formally open even if practically difficult. Policy-dependent minorities are the hardest case — the same terminal authority that can strip a legislatively-won protection can also be the only thing standing between a hostile majority and that minority, so their d is genuinely mixed and context-dependent on which doctrinal era is in view; this is captured by their dual role rather than by an override.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — preventing transient majorities from unwinding entrenched structural commitments — remains partially live (constitutional rights do still need protection from majoritarian overreach), which is why founding_problem_status is authored as contested rather than dead. Classifying this as tangled_rope rather than snare or mountain matters here: there IS a genuine coordination function (stability of long-horizon commitments), so calling it pure extraction would mislabel a real problem the arrangement solves; but there is also concentrated, self-defining institutional benefit and an active enforcement mechanism (judicial review itself, backed by the supremacy clause or equivalent) imposed on a legislature and electorate that never consented to this specific instance of override, which is why calling it a pure rope or mountain would launder the extraction. Tangled rope holds both facts without collapsing either.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    expertise_claim_vs_political_discretion,
    'Is judicial constitutional interpretation genuinely constrained by specialized legal expertise and textual/precedential method, or does it function as discretionary political judgment wearing expertise''s institutional legitimacy?',
    'Empirical study of judicial voting pattern predictability from prior political affiliation/appointment lineage versus from textual or precedential variables alone; convergence/divergence across ideologically mixed panels on identical doctrinal questions.',
    'If expertise substantially constrains outcomes, the coordination-function claim underlying the beneficiary designation for constitutional_judiciary strengthens (closer to rope); if outcomes track political variables more than doctrinal ones, the extraction reading strengthens and the tangled_rope classification tilts toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(expertise_claim_vs_political_discretion, empirical, 'Whether judicial expertise genuinely constrains interpretation or legitimates discretion.').

omega_variable(
    committer_kernel_disambiguation,
    'This story is one reading (judicial_supremacy_reading) of the basic_law_interpretive_authority kernel; sibling readings (parliamentary_sovereignty_reading, popular_constitutionalism_reading) locate final interpretive authority elsewhere entirely. Where exactly does the disagreement between readings live structurally?',
    'Compare the three readings'' beneficiary/victim structures and enforcement mechanisms directly: judicial_supremacy names the judiciary and legal professions as beneficiaries and the legislature/electorate as payers; parliamentary_sovereignty inverts this; popular_constitutionalism denies any single terminal beneficiary exists, distributing interpretive authority across ongoing democratic contest. The disagreement is located in WHO holds enforceable finality, not in whether a constitution should constrain government.',
    'Adopting a different reading would not change this story''s ε — it would produce an entirely different constraint (different stakeholders, different beneficiary/victim sets, likely a different claimed_type, since popular_constitutionalism plausibly reads closer to rope or scaffold given its explicit anti-terminality). This omega documents that the readings are not three measurements of one constraint but three distinct constraints sharing a kernel.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(committer_kernel_disambiguation, conceptual, 'Committer-frame note: locating exactly where the three sibling readings structurally diverge.').

omega_variable(
    countermajoritarian_difficulty_resolution,
    'Does judicial review, over the long run, protect durable rights and structural commitments better than it entrenches an unaccountable interpretive elite — i.e., does the countermajoritarian difficulty resolve toward net coordination or net extraction as constitutional systems mature?',
    'Longitudinal comparative analysis across jurisdictions with strong judicial review versus parliamentary sovereignty versus hybrid systems, tracking rights outcomes, democratic backsliding indices, and legislative gridlock over multi-decade horizons.',
    'Resolution toward net coordination would support treating the extraction trend in the temporal measurements as a correctable overshoot rather than a structural feature; resolution toward net extraction would support reclassifying the trajectory as approaching snare.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(countermajoritarian_difficulty_resolution, empirical, 'Whether the countermajoritarian difficulty nets out as coordination or extraction over constitutional-system lifespans.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(basic_law_interpretive_authority__judicial_supremacy_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(basi_tr_t0, basic_law_interpretive_authority__judicial_supremacy_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(basi_tr_t10, basic_law_interpretive_authority__judicial_supremacy_reading, theater_ratio, 10, 0.15).
narrative_ontology:measurement(basi_tr_t20, basic_law_interpretive_authority__judicial_supremacy_reading, theater_ratio, 20, 0.18).
narrative_ontology:measurement(basi_tr_t30, basic_law_interpretive_authority__judicial_supremacy_reading, theater_ratio, 30, 0.21).
narrative_ontology:measurement(basi_tr_t40, basic_law_interpretive_authority__judicial_supremacy_reading, theater_ratio, 40, 0.24).
narrative_ontology:measurement(basi_tr_t50, basic_law_interpretive_authority__judicial_supremacy_reading, theater_ratio, 50, 0.26).
narrative_ontology:measurement(basi_tr_t60, basic_law_interpretive_authority__judicial_supremacy_reading, theater_ratio, 60, 0.28).

% Extraction over time
narrative_ontology:measurement(basi_be_t0, basic_law_interpretive_authority__judicial_supremacy_reading, base_extractiveness, 0, 0.34).
narrative_ontology:measurement(basi_be_t10, basic_law_interpretive_authority__judicial_supremacy_reading, base_extractiveness, 10, 0.39).
narrative_ontology:measurement(basi_be_t20, basic_law_interpretive_authority__judicial_supremacy_reading, base_extractiveness, 20, 0.44).
narrative_ontology:measurement(basi_be_t30, basic_law_interpretive_authority__judicial_supremacy_reading, base_extractiveness, 30, 0.49).
narrative_ontology:measurement(basi_be_t40, basic_law_interpretive_authority__judicial_supremacy_reading, base_extractiveness, 40, 0.53).
narrative_ontology:measurement(basi_be_t50, basic_law_interpretive_authority__judicial_supremacy_reading, base_extractiveness, 50, 0.56).
narrative_ontology:measurement(basi_be_t60, basic_law_interpretive_authority__judicial_supremacy_reading, base_extractiveness, 60, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(basi_su_t0, basic_law_interpretive_authority__judicial_supremacy_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(basi_su_t10, basic_law_interpretive_authority__judicial_supremacy_reading, suppression_requirement, 10, 0.39).
narrative_ontology:measurement(basi_su_t20, basic_law_interpretive_authority__judicial_supremacy_reading, suppression_requirement, 20, 0.43).
narrative_ontology:measurement(basi_su_t30, basic_law_interpretive_authority__judicial_supremacy_reading, suppression_requirement, 30, 0.46).
narrative_ontology:measurement(basi_su_t40, basic_law_interpretive_authority__judicial_supremacy_reading, suppression_requirement, 40, 0.49).
narrative_ontology:measurement(basi_su_t50, basic_law_interpretive_authority__judicial_supremacy_reading, suppression_requirement, 50, 0.51).
narrative_ontology:measurement(basi_su_t60, basic_law_interpretive_authority__judicial_supremacy_reading, suppression_requirement, 60, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(basic_law_interpretive_authority__judicial_supremacy_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(basic_law_interpretive_authority__judicial_supremacy_reading, 0.12).
narrative_ontology:affects_constraint(basic_law_interpretive_authority__judicial_supremacy_reading, parliamentary_sovereignty_reading).
narrative_ontology:affects_constraint(basic_law_interpretive_authority__judicial_supremacy_reading, popular_constitutionalism_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three siblings decomposed from the basic_law_interpretive_authority kernel per the ε-invariance principle: measuring 'who holds final constitutional interpretive authority' differently (courts vs. legislature vs. ongoing popular contest) yields structurally distinct beneficiary/victim sets and distinct ε values, so each reading is authored as its own constraint story rather than as one story with a measurement parameter. This story (judicial_supremacy_reading) forecloses parliamentary_sovereignty_reading within a single legal framework (a system cannot simultaneously hold that courts AND the legislature possess unreviewable terminal authority over the same question) while coexisting with popular_constitutionalism_reading, which denies terminal authority to any single institution and can be held as a normative critique of either institutional-supremacy reading without logically eliminating it.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
