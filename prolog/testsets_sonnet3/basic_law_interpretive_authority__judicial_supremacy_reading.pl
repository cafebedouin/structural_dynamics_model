% ============================================================================
% CONSTRAINT STORY: basic_law_interpretive_authority__judicial_supremacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
 *   constraint_id: basic_law_interpretive_authority__judicial_supremacy_reading
 *   human_readable: Judicial Supremacy Reading of Constitutional Interpretive Authority
 *   domain: constitutional_law/political_theory/institutional_design
 *
 * SUMMARY:
 *   This story instantiates the judicial supremacy reading of the basic-law
 *   interpretive authority kernel: the position that courts, by virtue of
 *   specialized legal expertise and structural independence from electoral
 *   pressure, hold final and binding authority to determine constitutional
 *   meaning, including the power to invalidate legislation enacted by elected
 *   majorities. This is one of three structurally distinct readings of the
 *   same underlying kernel — the other two (legislative final say grounded in
 *   democratic mandate; and meaning as an ongoing product of popular
 *   contestation rather than terminal adjudication) are separate constraint
 *   stories with their own ε, beneficiary/victim sets, and classifications,
 *   linked here via network edges. The judiciary and the specialized
 *   constitutional bar are the structural beneficiaries of this reading;
 *   legislative majorities and the electoral coalitions that produce them are
 *   the payers when review blocks or narrows enacted law.
 *
 * KEY AGENTS:
 *   - constitutional_judiciary: institutional authority holding final interpretive power, benefits from the arrangement's persistence
 *   - legal_professional_class: organized beneficiary whose professional standing depends on judicial finality
 *   - legislative_majorities: organized payer whose enacted statutes are subject to invalidation
 *   - electoral_coalitions: powerless payer whose democratic mandate is filtered through unelected review
 *   - executive_branch: dual observer/payer with a longer-horizon appointment lever
 *   - constitutional_scholars_outside_judiciary: analytical observers corroborating or contesting the founding-problem narrative from outside the benefiting parties
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(basic_law_interpretive_authority__judicial_supremacy_reading, 0.52).
domain_priors:suppression_score(basic_law_interpretive_authority__judicial_supremacy_reading, 0.58).
domain_priors:theater_ratio(basic_law_interpretive_authority__judicial_supremacy_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(basic_law_interpretive_authority__judicial_supremacy_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(basic_law_interpretive_authority__judicial_supremacy_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(basic_law_interpretive_authority__judicial_supremacy_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(basic_law_interpretive_authority__judicial_supremacy_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(basic_law_interpretive_authority__judicial_supremacy_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(basic_law_interpretive_authority__judicial_supremacy_reading, tangled_rope).
narrative_ontology:human_readable(basic_law_interpretive_authority__judicial_supremacy_reading, "Judicial Supremacy Reading of Constitutional Interpretive Authority").
narrative_ontology:topic_domain(basic_law_interpretive_authority__judicial_supremacy_reading, "constitutional_law/political_theory/institutional_design").

domain_priors:requires_active_enforcement(basic_law_interpretive_authority__judicial_supremacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(basic_law_interpretive_authority__judicial_supremacy_reading, 'fad72969-1d3a-4e0e-abb7-d4c38bcf01c7').
narrative_ontology:cs_kernel_codification('fad72969-1d3a-4e0e-abb7-d4c38bcf01c7', fixed_text).
narrative_ontology:cs_authority_grounding('fad72969-1d3a-4e0e-abb7-d4c38bcf01c7', lineage).
narrative_ontology:cs_interpretation_layer_present('fad72969-1d3a-4e0e-abb7-d4c38bcf01c7').
narrative_ontology:cs_reading_relation('fad72969-1d3a-4e0e-abb7-d4c38bcf01c7', basic_law_interpretive_authority__parliamentary_sovereignty_reading, forecloses).
narrative_ontology:cs_reading_relation('fad72969-1d3a-4e0e-abb7-d4c38bcf01c7', basic_law_interpretive_authority__popular_constitutionalism_reading, coexists_with).
narrative_ontology:cs_axiom('fad72969-1d3a-4e0e-abb7-d4c38bcf01c7', foundational, specialized_insulated_adjudication_yields_correct_constitutional_meaning).
narrative_ontology:cs_axiom_status(specialized_insulated_adjudication_yields_correct_constitutional_meaning, holdable).
narrative_ontology:cs_axiom_grounding('fad72969-1d3a-4e0e-abb7-d4c38bcf01c7', specialized_insulated_adjudication_yields_correct_constitutional_meaning, instrumental).
narrative_ontology:cs_axiom('fad72969-1d3a-4e0e-abb7-d4c38bcf01c7', foundational, electoral_majorities_cannot_be_final_judges_of_their_own_constitutional_limits).
narrative_ontology:cs_axiom_status(electoral_majorities_cannot_be_final_judges_of_their_own_constitutional_limits, holdable).
narrative_ontology:cs_axiom_grounding('fad72969-1d3a-4e0e-abb7-d4c38bcf01c7', electoral_majorities_cannot_be_final_judges_of_their_own_constitutional_limits, empirically_contingent).
narrative_ontology:cs_reference_frame('fad72969-1d3a-4e0e-abb7-d4c38bcf01c7', postwar_rights_protective_judicial_review).
narrative_ontology:cs_drift_state('fad72969-1d3a-4e0e-abb7-d4c38bcf01c7', contemporary_polarized_appointments_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('fad72969-1d3a-4e0e-abb7-d4c38bcf01c7', '').
narrative_ontology:cs_kernel_id(basic_law_interpretive_authority__judicial_supremacy_reading, basic_law_interpretive_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(basic_law_interpretive_authority__judicial_supremacy_reading, constitutional_judiciary).
narrative_ontology:constraint_beneficiary(basic_law_interpretive_authority__judicial_supremacy_reading, legal_professional_class).
narrative_ontology:constraint_victim(basic_law_interpretive_authority__judicial_supremacy_reading, legislative_majorities).
narrative_ontology:constraint_victim(basic_law_interpretive_authority__judicial_supremacy_reading, electoral_coalitions).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(basic_law_interpretive_authority__judicial_supremacy_reading, executive_branch).
narrative_ontology:constraint_vindicates(basic_law_interpretive_authority__judicial_supremacy_reading, counter_majoritarian_rights_protection_doctrine).
narrative_ontology:constraint_vindicates(basic_law_interpretive_authority__judicial_supremacy_reading, constitutional_supremacy_over_ordinary_legislation).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Holds final say on whether legislation, executive action, or even constitutional amendments comport with constitutional meaning. Its rulings bind the legislature and cannot be overturned by ordinary majority vote. It sets the interpretive methodology (originalism, purposivism, living-constitutionalism) that determines outcomes, and its own institutional authority — tenure, budget insulation, contempt power — depends on the public and other branches accepting this as the correct reading of the kernel.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_authority__judicial_supremacy_reading, constitutional_judiciary, agenda_setter,
    institutional, civilizational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(basic_law_interpretive_authority__judicial_supremacy_reading, constitutional_judiciary, beneficiary).

% Constitutional litigators, law professors, and appellate specialists derive professional standing, fee income, and career advancement from a system where constitutional meaning is settled through specialized legal argument rather than ordinary political contestation. Their expertise is the currency this reading makes valuable.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_authority__judicial_supremacy_reading, legal_professional_class, beneficiary,
    organized, generational, arbitrage, national).

% Enact statutes reflecting an electoral mandate, only to have them invalidated or narrowed by judicial review on constitutional grounds the legislature did not vote on and cannot directly overturn except through supermajority amendment processes that are rarely achievable. Their recourse is packing courts, ignoring rulings (rare, costly), or waiting out judicial composition changes across election cycles — none of which is a clean exit.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_authority__judicial_supremacy_reading, legislative_majorities, payer,
    organized, biographical, constrained, national).

% Vote for candidates and platforms that, once enacted into law, can be nullified by unelected judges applying a methodology the electorate never chose and cannot revise except generationally through appointment turnover. Their democratic input is filtered through an institution whose membership they influence only indirectly and slowly.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_authority__judicial_supremacy_reading, electoral_coalitions, payer,
    powerless, biographical, trapped, national).

% Must comply with judicial rulings on the constitutionality of executive action and enacted legislation it wishes to enforce, but also shapes the judiciary's future composition through appointments — giving it a longer-horizon lever the other payer seats lack.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_authority__judicial_supremacy_reading, executive_branch, observer,
    institutional, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(basic_law_interpretive_authority__judicial_supremacy_reading, executive_branch, payer).

% Study and critique the judicial supremacy arrangement from academic or comparative-law positions without being bound by its rulings or dependent on judicial deference for career advancement in the way practicing litigators are; some corroborate the coordination story, others document its counter-majoritarian costs.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_authority__judicial_supremacy_reading, constitutional_scholars_outside_judiciary, observer,
    analytical, civilizational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(basic_law_interpretive_authority__judicial_supremacy_reading, constitutional_judiciary).
narrative_ontology:fixing_cost_class(basic_law_interpretive_authority__judicial_supremacy_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a stable, insulated forum for resolving constitutional disputes that keeps fundamental rights questions from being resolved purely by transient legislative majorities, and gives citizens, minorities, and other branches a predictable, precedent-bound answer to 'what does the constitution mean' rather than an answer that changes with every election.
% TRANSFER_FUNCTION: Moves final interpretive authority over constitutional meaning from the elected legislature and the electorate to a body of appointed, tenured judges; in concrete terms it moves the power to make enacted legislation stick from legislative majorities to courts, and moves professional and institutional standing to the judiciary and the constitutional bar.
% ABSENT_VOICES: Legislators whose statutes are struck down have a voice in the political process but no formal voice inside the adjudication itself beyond litigation posture; ordinary citizens whose preferred policies are blocked have no seat at all in the interpretive act — they experience the ruling as external, not as something they participated in producing.
% DISAPPEARANCE_RATIONALE: If judicial supremacy vanished overnight and interpretive finality reverted to the legislature or to ongoing popular contestation, statutes currently blocked or narrowed by constitutional rulings would take effect, legislative majorities would face materially different constraints on lawmaking, the constitutional bar's specialized practice would lose its terminal audience, and appointment politics to the bench would lose much of its present stakes.
% FOUNDING_PROBLEM: Founding-era and postwar constitutional designers worried that simple legislative majorities could not be trusted to police their own compliance with higher-law limits, especially regarding minority rights and structural checks — an institution insulated from immediate electoral pressure was built to hold those limits when majorities had incentive to erode them.
% FOUNDING_PROBLEM_CORROBORATION: Sitting judges and constitutional litigators attest the counter-majoritarian problem remains live, citing recent legislative overreach as evidence. Legislative-sovereignty scholars and several comparative constitutional theorists outside the judiciary and the litigating bar attest the problem has been substantially supplanted by judicial policy-making beyond the original rights-protection rationale, pointing to interpretive drift in unelected doctrine formation as evidence the arrangement now serves institutional self-perpetuation as much as its founding function.
narrative_ontology:disappearance_verdict(basic_law_interpretive_authority__judicial_supremacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(basic_law_interpretive_authority__judicial_supremacy_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(basic_law_interpretive_authority__judicial_supremacy_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(basic_law_interpretive_authority__judicial_supremacy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(basic_law_interpretive_authority__judicial_supremacy_reading, 0.52, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness is moderate (0.52 at interval end) rather than extreme because the arrangement genuinely performs a coordination function — insulating rights questions from transient majorities is not merely a cover story, it is a real structural role this reading is built around. Suppression (0.58) reflects that legislative majorities have no ordinary-majoritarian route to override a constitutional ruling; only supermajority amendment or slow compositional change through appointments provides recourse, and both are costly, rare instruments rather than genuine exits. Theater ratio is low-to-moderate (0.22) and rising modestly, reflecting a real interpretive function alongside a growing share of doctrinal activity that critics read as self-perpetuating institutional authority rather than rights-protection per se. Accessibility collapse (0.62) and resistance (0.55) are mid-range: alternatives to judicial finality (legislative override, popular constitutionalism) remain live political and scholarly positions, not eliminated, but the practical routes to exercising them are narrow.
 *
 * DIRECTIONALITY LOGIC:
 *   The judiciary and the constitutional bar sit near the beneficiary end of directionality: their institutional standing, professional relevance, and interpretive authority are all sustained by this reading prevailing over the sibling readings. Legislative majorities and electoral coalitions sit near the target end: they bear the transfer when review invalidates or narrows legislation reflecting their mandate, and their formal exit options (constrained, trapped) reflect that override is rare and slow. The executive branch sits ambiguously — bound by rulings in the short run but holding a longer-horizon appointment lever that partially offsets its payer position, which is why it carries a secondary payer role alongside its observer role rather than a pure victim classification.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as tangled_rope rather than pure snare is deliberate: this reading is not merely extraction dressed as coordination — the counter-majoritarian rights-protection function is a real, historically documented design response to a genuine problem (majority tyranny risk), which is why it carries a coordination function and beneficiaries alongside victims and required active enforcement. Whether the founding problem remains live or has been substantially supplanted by institutional self-perpetuation is exactly the contested question the R5 genealogy interview surfaces (founding_problem_status: contested) rather than a question this story resolves by classification.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    counter_majoritarian_function_vs_self_perpetuation,
    'Does judicial review, as currently practiced, still primarily perform the founding counter-majoritarian rights-protection function it was designed for, or has it substantially drifted into an institution that protects and expands its own interpretive authority independent of that original function?',
    'Longitudinal case-outcome analysis distinguishing rulings that protect minority/individual rights against majoritarian overreach from rulings that primarily expand or preserve judicial interpretive jurisdiction itself (e.g., standing doctrine, deference doctrine reversals, jurisdiction-stripping resistance).',
    'If the founding function is substantially intact, the tangled_rope classification with genuine coordination value is well-supported. If drift toward self-perpetuation dominates, the effective classification shifts closer to snare, with the coordination story functioning primarily as legitimating cover.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(counter_majoritarian_function_vs_self_perpetuation, empirical, 'Whether judicial review still serves its founding rights-protection function or has drifted toward institutional self-perpetuation.').

omega_variable(
    kernel_reading_committer_structure,
    'This story is one reading (judicial_supremacy_reading) of the basic_law_interpretive_authority kernel; the sibling readings (parliamentary_sovereignty_reading, popular_constitutionalism_reading) locate final interpretive authority in the legislature or in ongoing popular contestation respectively rather than in courts. Where exactly is the disagreement located structurally?',
    'Compare the three readings'' beneficiary/victim structures directly: judicial_supremacy_reading names the judiciary and legal profession as beneficiaries and legislative/electoral majorities as payers; parliamentary_sovereignty_reading would invert this, naming the legislature as the terminal authority and the judiciary as a payer/constrained seat when its rulings can be legislatively overridden; popular_constitutionalism_reading disperses terminal authority entirely, denying any single institution the beneficiary position captured here.',
    'The disagreement is located in WHO holds terminal interpretive authority, not in whether courts interpret the constitution at all (all three readings agree courts play some interpretive role). Adopting a sibling reading would move the judiciary from beneficiary to a constrained or payer seat and would move legislative majorities and electoral coalitions from payer toward beneficiary or symmetric position.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_committer_structure, conceptual, 'Locating the structural disagreement between judicial supremacy and its sibling kernel readings.').

omega_variable(
    appointment_capture_ambiguity,
    'Is the executive branch''s longer-horizon appointment lever sufficient to make it a genuine partial beneficiary of judicial supremacy over multiple terms, or does it remain a payer whose only recourse is a slow, uncertain, generational instrument?',
    'Empirical tracking of appointment-driven doctrinal shift versus continuity of adverse rulings against the appointing administration''s legislative program within the same term.',
    'If appointment capture is substantial, the executive''s directionality should shift toward the beneficiary end; if rulings remain frequently adverse regardless of recent appointments, the payer classification for the executive stands.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(appointment_capture_ambiguity, empirical, 'Whether executive appointment power meaningfully offsets its payer position under judicial supremacy.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(basic_law_interpretive_authority__judicial_supremacy_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(basi_tr_t0, basic_law_interpretive_authority__judicial_supremacy_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(basi_tr_t8, basic_law_interpretive_authority__judicial_supremacy_reading, theater_ratio, 8, 0.13).
narrative_ontology:measurement(basi_tr_t16, basic_law_interpretive_authority__judicial_supremacy_reading, theater_ratio, 16, 0.16).
narrative_ontology:measurement(basi_tr_t24, basic_law_interpretive_authority__judicial_supremacy_reading, theater_ratio, 24, 0.18).
narrative_ontology:measurement(basi_tr_t32, basic_law_interpretive_authority__judicial_supremacy_reading, theater_ratio, 32, 0.2).
narrative_ontology:measurement(basi_tr_t40, basic_law_interpretive_authority__judicial_supremacy_reading, theater_ratio, 40, 0.22).

% Extraction over time
narrative_ontology:measurement(basi_be_t0, basic_law_interpretive_authority__judicial_supremacy_reading, base_extractiveness, 0, 0.34).
narrative_ontology:measurement(basi_be_t8, basic_law_interpretive_authority__judicial_supremacy_reading, base_extractiveness, 8, 0.39).
narrative_ontology:measurement(basi_be_t16, basic_law_interpretive_authority__judicial_supremacy_reading, base_extractiveness, 16, 0.44).
narrative_ontology:measurement(basi_be_t24, basic_law_interpretive_authority__judicial_supremacy_reading, base_extractiveness, 24, 0.47).
narrative_ontology:measurement(basi_be_t32, basic_law_interpretive_authority__judicial_supremacy_reading, base_extractiveness, 32, 0.5).
narrative_ontology:measurement(basi_be_t40, basic_law_interpretive_authority__judicial_supremacy_reading, base_extractiveness, 40, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(basi_su_t0, basic_law_interpretive_authority__judicial_supremacy_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(basi_su_t8, basic_law_interpretive_authority__judicial_supremacy_reading, suppression_requirement, 8, 0.45).
narrative_ontology:measurement(basi_su_t16, basic_law_interpretive_authority__judicial_supremacy_reading, suppression_requirement, 16, 0.49).
narrative_ontology:measurement(basi_su_t24, basic_law_interpretive_authority__judicial_supremacy_reading, suppression_requirement, 24, 0.53).
narrative_ontology:measurement(basi_su_t32, basic_law_interpretive_authority__judicial_supremacy_reading, suppression_requirement, 32, 0.56).
narrative_ontology:measurement(basi_su_t40, basic_law_interpretive_authority__judicial_supremacy_reading, suppression_requirement, 40, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(basic_law_interpretive_authority__judicial_supremacy_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(basic_law_interpretive_authority__judicial_supremacy_reading, basic_law_interpretive_authority__parliamentary_sovereignty_reading).
narrative_ontology:affects_constraint(basic_law_interpretive_authority__judicial_supremacy_reading, basic_law_interpretive_authority__popular_constitutionalism_reading).

% DUAL FORMULATION NOTE:
% This story is one of three siblings decomposing the natural-language concept 'final constitutional interpretive authority.' judicial_supremacy_reading, parliamentary_sovereignty_reading, and popular_constitutionalism_reading each name a structurally distinct claim about WHO holds terminal authority, with different beneficiary/victim sets and different ε. They are not the same constraint viewed from different angles; per the ε-invariance principle they are three constraints sharing a kernel, linked by network edges rather than merged into one story with a measurement parameter.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
