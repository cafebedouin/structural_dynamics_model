% ============================================================================
% CONSTRAINT STORY: deferential_realism_ontology__rhetorical_scaffold_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_deferential_realism_ontology__rhetorical_scaffold_reading, []).

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
 *   constraint_id: deferential_realism_ontology__rhetorical_scaffold_reading
 *   human_readable: Constraint Typology as Rhetorical Scaffold for Policy Critique
 *   domain: epistemology/normative_theory/institutional_design
 *
 * SUMMARY:
 *   A community of policy critics and scholars uses the constraint typology's
 *   vocabulary — 'mountain,' 'snare,' 'tangled rope' — not as an output of a
 *   measurement protocol but as a normative classification scheme: a
 *   mechanism is called a 'snare' when its beneficiaries are judged
 *   illegitimate, and the framework's persuasive power comes precisely from
 *   the vocabulary sounding like discovered fact ('extraction,'
 *   'suppression,' 'epsilon') when it is functioning as declared political
 *   argument. This reading treats that persuasive borrowing as the
 *   mechanism's actual operation, distinct from the
 *   immutable_diagnostic_reading (which insists the same vocabulary IS
 *   measurement) and the hybrid_pragmatic_reading (which splits the
 *   difference at mountains/ropes vs. tangled_ropes/snares).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(deferential_realism_ontology__rhetorical_scaffold_reading, 0.42).
domain_priors:suppression_score(deferential_realism_ontology__rhetorical_scaffold_reading, 0.18).
domain_priors:theater_ratio(deferential_realism_ontology__rhetorical_scaffold_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(deferential_realism_ontology__rhetorical_scaffold_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(deferential_realism_ontology__rhetorical_scaffold_reading, suppression_requirement, 0.18).
narrative_ontology:constraint_metric(deferential_realism_ontology__rhetorical_scaffold_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(deferential_realism_ontology__rhetorical_scaffold_reading, accessibility_collapse, 0.25).
narrative_ontology:constraint_metric(deferential_realism_ontology__rhetorical_scaffold_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(deferential_realism_ontology__rhetorical_scaffold_reading, tangled_rope).
narrative_ontology:human_readable(deferential_realism_ontology__rhetorical_scaffold_reading, "Constraint Typology as Rhetorical Scaffold for Policy Critique").
narrative_ontology:topic_domain(deferential_realism_ontology__rhetorical_scaffold_reading, "epistemology/normative_theory/institutional_design").

domain_priors:requires_active_enforcement(deferential_realism_ontology__rhetorical_scaffold_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(deferential_realism_ontology__rhetorical_scaffold_reading, 'd741b25b-dee0-47a6-8625-c7c488e12fa2').
narrative_ontology:cs_kernel_codification('d741b25b-dee0-47a6-8625-c7c488e12fa2', distributed).
narrative_ontology:cs_authority_grounding('d741b25b-dee0-47a6-8625-c7c488e12fa2', distributed).
narrative_ontology:cs_reading_relation('d741b25b-dee0-47a6-8625-c7c488e12fa2', deferential_realism_ontology__immutable_diagnostic_reading, forecloses).
narrative_ontology:cs_reading_relation('d741b25b-dee0-47a6-8625-c7c488e12fa2', deferential_realism_ontology__hybrid_pragmatic_reading, influences).
narrative_ontology:cs_axiom('d741b25b-dee0-47a6-8625-c7c488e12fa2', foundational, classification_is_normative_declaration_not_discovery).
narrative_ontology:cs_axiom_status(classification_is_normative_declaration_not_discovery, holdable).
narrative_ontology:cs_axiom_grounding('d741b25b-dee0-47a6-8625-c7c488e12fa2', classification_is_normative_declaration_not_discovery, conventional).
narrative_ontology:cs_axiom('d741b25b-dee0-47a6-8625-c7c488e12fa2', secondary, framework_value_lies_in_persuasive_efficacy).
narrative_ontology:cs_axiom_status(framework_value_lies_in_persuasive_efficacy, holdable).
narrative_ontology:cs_axiom_grounding('d741b25b-dee0-47a6-8625-c7c488e12fa2', framework_value_lies_in_persuasive_efficacy, instrumental).
narrative_ontology:cs_reference_frame('d741b25b-dee0-47a6-8625-c7c488e12fa2', typology_as_open_normative_vocabulary).
narrative_ontology:cs_drift_state('d741b25b-dee0-47a6-8625-c7c488e12fa2', contemporary_policy_discourse, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('d741b25b-dee0-47a6-8625-c7c488e12fa2', '').
narrative_ontology:cs_kernel_id(deferential_realism_ontology__rhetorical_scaffold_reading, deferential_realism_ontology).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(deferential_realism_ontology__rhetorical_scaffold_reading, reform_advocates_using_snare_label).
narrative_ontology:constraint_beneficiary(deferential_realism_ontology__rhetorical_scaffold_reading, critical_policy_scholars).
narrative_ontology:constraint_victim(deferential_realism_ontology__rhetorical_scaffold_reading, institutions_labeled_snare_without_measurement).
narrative_ontology:constraint_victim(deferential_realism_ontology__rhetorical_scaffold_reading, incumbent_defenders_denied_neutral_arbiter).
narrative_ontology:constraint_vindicates(deferential_realism_ontology__rhetorical_scaffold_reading, normative_classification_is_legitimate_political_speech).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Deploy the typology's vocabulary — especially the word 'snare' — in campaigns against mechanisms they judge illegitimate. They choose when to apply the label based on whose beneficiaries they consider unjust, not from a measurement protocol. The label's rhetorical force (implying discovery of an objective extraction mechanism) is itself the tool they wield; they benefit from the framework's persuasive borrowing of scientific authority even though the classification act is a normative judgment they are making.
narrative_ontology:constraint_stakeholder(deferential_realism_ontology__rhetorical_scaffold_reading, reform_advocates_using_snare_label, agenda_setter,
    organized, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(deferential_realism_ontology__rhetorical_scaffold_reading, reform_advocates_using_snare_label, beneficiary).

% Build academic and public-intellectual careers on applying and refining the typology's normative vocabulary. They gain citation currency, media visibility, and policy influence from being seen as wielders of a rigorous-sounding diagnostic apparatus, while their actual classification decisions turn on which beneficiaries they judge illegitimate — a value question, not a measurement.
narrative_ontology:constraint_stakeholder(deferential_realism_ontology__rhetorical_scaffold_reading, critical_policy_scholars, beneficiary,
    institutional, generational, mobile, national).

% Have the 'snare' label applied to their operations in advocacy and scholarship without any epsilon value being independently measured — the label is asserted because the advocate judges their beneficiaries illegitimate. They bear reputational and political cost from a classification they cannot contest on measurement grounds, because the framework itself declares that no such measurement is what's happening — the classification is announced as normative judgment, which removes the institution's usual recourse of disputing the data.
narrative_ontology:constraint_stakeholder(deferential_realism_ontology__rhetorical_scaffold_reading, institutions_labeled_snare_without_measurement, payer,
    powerful, biographical, constrained, national).

% Attempt to defend a labeled mechanism by disputing the epsilon score, only to be told the score was never a measurement but a normative declaration keyed to whether beneficiaries are 'legitimate.' They have no neutral procedure to appeal to — the framework's own rhetorical-scaffold self-description removes the possibility of settling the dispute by better observation, since better observation was never the mechanism generating the classification.
narrative_ontology:constraint_stakeholder(deferential_realism_ontology__rhetorical_scaffold_reading, incumbent_defenders_denied_neutral_arbiter, payer,
    powerful, biographical, constrained, national).

% Receive 'snare' and 'mountain' labels through media and advocacy discourse as if they were findings, without visibility into the normative judgment embedded in the labeling act. They would object to being persuaded by apparent discovery when what occurred was advocacy, if they understood the reading being used, but the rhetorical effectiveness of the vocabulary depends on this distinction remaining unarticulated to them.
narrative_ontology:constraint_stakeholder(deferential_realism_ontology__rhetorical_scaffold_reading, general_public_audience, excluded,
    powerless, immediate, trapped, national).

% Watch the same vocabulary operate simultaneously as claimed-objective diagnostic tool (in the immutable_diagnostic_reading) and as openly normative advocacy vocabulary (in this reading), and note that which reading a given user invokes is itself strategically chosen — scientific authority when persuasion requires it, normative honesty when challenged on rigor.
narrative_ontology:constraint_stakeholder(deferential_realism_ontology__rhetorical_scaffold_reading, framework_designers, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared vocabulary that lets otherwise-diffuse critics of a policy mechanism converge quickly on a common framing ('this is a snare') without each having to independently reconstruct an extraction argument from scratch — genuine coordination value for a coalition trying to act collectively against a mechanism.
% TRANSFER_FUNCTION: Moves persuasive and reputational capital from labeled institutions to the advocates and scholars who apply the labels: the institution's public standing is transferred into the advocate's rhetorical leverage, and the advocate's classification act (dressed in the vocabulary's apparent objectivity) captures argumentative ground that a plainly normative claim ('I think this is unjust') would not.
% ABSENT_VOICES: Labeled institutions and their defenders are present but structurally disadvantaged: they can contest the normative judgment but cannot contest a measurement, because on this reading there was never a measurement to contest. The general public, who receive the labels as apparent findings, are the most absent — they are not in the room where the reading (normative vocabulary vs. discovered fact) is being chosen.
% DISAPPEARANCE_RATIONALE: If this reading of the typology vanished — if everyone stopped treating 'snare' as available normative vocabulary and reverted strictly to the immutable-diagnostic reading — advocacy coalitions would lose a fast, emotionally resonant framing tool and would have to argue illegitimacy claims explicitly as value claims; some coalitions would fracture without the unifying label, and some labeled institutions would face less reputational pressure absent a measurement-flavored accusation they cannot rebut on data grounds.
% FOUNDING_PROBLEM: Policy critics needed a way to argue that a mechanism serving concentrated interests was illegitimate, in a public discourse that treats 'mere opinion' as weak and 'objective finding' as strong — the typology answered a rhetorical need for critique that sounds like diagnosis.
% FOUNDING_PROBLEM_CORROBORATION: Framework designers (analytical, outside the advocacy coalitions that benefit from the label) attest that the vocabulary was adopted for its persuasive borrowing of scientific-sounding language; this is corroborated by the very existence of the sibling immutable_diagnostic_reading, whose proponents insist the classification IS measurement and would reject this reading's self-description — the disagreement between the two readings' own proponents is itself independent evidence that the rhetorical function is real and contested, not merely alleged by hostile critics.
narrative_ontology:disappearance_verdict(deferential_realism_ontology__rhetorical_scaffold_reading, world_rearranges).
narrative_ontology:founding_problem_status(deferential_realism_ontology__rhetorical_scaffold_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(deferential_realism_ontology__rhetorical_scaffold_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(deferential_realism_ontology__rhetorical_scaffold_reading, 'none', 1).
narrative_ontology:epsilon_provenance(deferential_realism_ontology__rhetorical_scaffold_reading, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(deferential_realism_ontology__rhetorical_scaffold_reading_tests).
:- end_tests(deferential_realism_ontology__rhetorical_scaffold_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction (0.42) is moderate: real rhetorical and reputational capital moves from labeled institutions to advocates and scholars, but it is bounded because the label can be contested as a value claim rather than defeated only by re-measurement — institutions retain the political recourse of arguing their beneficiaries ARE legitimate. Suppression (0.18) is low and deliberately so: this reading's defining structural feature is that it does NOT foreclose alternative framings — a labeled institution can always contest the normative premise, unlike a genuine measurement dispute where an institution has no standing to reject the instrument. Theater ratio (0.4) captures that a meaningful share of the vocabulary's use trades on its apparent scientific rigor (accuracy-flavored language) for what is actually normative persuasion — the theatrical element is the borrowed authority of 'discovery' language applied to a value judgment.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setting advocate's seat, deploying 'snare' looks like coordination — rallying a coalition around a shared, efficient framing for what they already believe is illegitimate. From the labeled institution's seat, the same act looks like extraction with a scientific veneer: their standing is damaged by language that borrows the authority of measurement while explicitly (on this reading) declining to perform one. The engine's per-seat computation should register this divergence directly from the beneficiary/payer structural data, without any need to adjudicate which seat is 'right' about what the typology fundamentally is — that adjudication is exactly the kernel-level dispute this story deliberately does not resolve.
 *
 * DIRECTIONALITY LOGIC:
 *   Reform advocates and critical scholars are near the beneficiary end: they set when and how the vocabulary is deployed and capture its rhetorical and career value. Labeled institutions and their defenders sit near the target end: they absorb reputational cost from a classification they cannot contest on the terms the framework claims to operate on (measurement), because on this reading there is no measurement to contest — only a value judgment dressed as one. The general public are structurally excluded rather than positioned on the beneficiary/target axis at all; the vocabulary's efficacy depends on them not distinguishing this reading from the diagnostic reading.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (a rhetorical need for critique that sounds like diagnosis) remains live — political advocacy in a discourse that rewards apparent objectivity is not going away — so this is not a case of an arrangement outliving its function. But the framework's continued use of measurement-flavored vocabulary (epsilon, extraction, suppression) for what this reading holds is a normative act creates a persistent risk of the general public mistaking declared judgment for discovered fact; this is the central asymmetry the story tracks rather than resolves.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reading_choice_is_itself_strategic,
    'Do users of the typology select between the rhetorical_scaffold_reading and the immutable_diagnostic_reading strategically — invoking ''this is just normative vocabulary'' when challenged on rigor, but invoking ''this is a measured fact'' when persuading an audience — rather than consistently holding one reading?',
    'Discourse analysis tracking the same speakers/institutions across contexts: do they defend ''snare'' claims as normative judgments in academic exchanges but present them as discovered extraction mechanisms in public advocacy?',
    'If reading-switching is systematic and audience-dependent, the persuasive power this reading identifies as the framework''s ''value'' is actually a form of equivocation that borrows credibility from one reading while escaping accountability via the other — which would push this reading''s classification toward snare rather than tangled_rope, since the coordination benefit would be substantially cover for the extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_choice_is_itself_strategic, empirical, 'Whether the readings are held consistently or switched strategically by the same actors across audiences.').

omega_variable(
    legitimacy_judgment_is_irreducibly_contestable,
    'Is the judgment of ''illegitimate beneficiary'' that triggers a ''snare'' label on this reading itself groundable in any shared normative standard, or is it irreducibly perspectival — meaning any two advocates with different values would classify the same mechanism differently with no fact of the matter to adjudicate between them?',
    'Survey advocates and scholars applying the typology across a range of mechanisms; check whether disagreement about ''snare'' classification tracks disagreement about underlying political values rather than disagreement about facts.',
    'If legitimacy judgments are irreducibly perspectival, this reading''s honesty about the typology''s normative character is a genuine epistemic virtue relative to the immutable_diagnostic_reading''s overclaim; if there is substantial cross-value convergence on classifications, the rhetorical_scaffold_reading may be underselling the framework''s discriminating power.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(legitimacy_judgment_is_irreducibly_contestable, conceptual, 'Whether legitimacy judgments underlying snare classification are irreducibly value-relative or track some shared underlying standard.').

omega_variable(
    foreclosed_recourse_for_labeled_institutions,
    'Given that this reading removes measurement as the site of dispute, do labeled institutions have any comparably effective recourse (e.g., counter-normative argument, coalition-building) or are they structurally disadvantaged relative to a world where the dispute could be settled by data?',
    'Case studies comparing outcomes for institutions that contested a ''snare'' label via normative counter-argument versus institutions that (mistakenly, on this reading) tried to contest it via data/measurement rebuttal.',
    'If normative counter-argument is systematically less effective than data rebuttal in this discourse, this reading''s low suppression score (0.18) may understate real suppression — labeled institutions might face de facto suppression even without epistemic foreclosure, because the public rewards apparent-objectivity claims regardless of validity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(foreclosed_recourse_for_labeled_institutions, empirical, 'Whether the absence of measurement-based dispute genuinely leaves institutions unsuppressed, or merely relocates suppression to an asymmetric normative-argument arena.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(deferential_realism_ontology__rhetorical_scaffold_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(defe_tr_t0, deferential_realism_ontology__rhetorical_scaffold_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(defe_tr_t4, deferential_realism_ontology__rhetorical_scaffold_reading, theater_ratio, 4, 0.26).
narrative_ontology:measurement(defe_tr_t8, deferential_realism_ontology__rhetorical_scaffold_reading, theater_ratio, 8, 0.31).
narrative_ontology:measurement(defe_tr_t12, deferential_realism_ontology__rhetorical_scaffold_reading, theater_ratio, 12, 0.34).
narrative_ontology:measurement(defe_tr_t16, deferential_realism_ontology__rhetorical_scaffold_reading, theater_ratio, 16, 0.37).
narrative_ontology:measurement(defe_tr_t20, deferential_realism_ontology__rhetorical_scaffold_reading, theater_ratio, 20, 0.4).

% Extraction over time
narrative_ontology:measurement(defe_be_t0, deferential_realism_ontology__rhetorical_scaffold_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(defe_be_t4, deferential_realism_ontology__rhetorical_scaffold_reading, base_extractiveness, 4, 0.32).
narrative_ontology:measurement(defe_be_t8, deferential_realism_ontology__rhetorical_scaffold_reading, base_extractiveness, 8, 0.36).
narrative_ontology:measurement(defe_be_t12, deferential_realism_ontology__rhetorical_scaffold_reading, base_extractiveness, 12, 0.38).
narrative_ontology:measurement(defe_be_t16, deferential_realism_ontology__rhetorical_scaffold_reading, base_extractiveness, 16, 0.4).
narrative_ontology:measurement(defe_be_t20, deferential_realism_ontology__rhetorical_scaffold_reading, base_extractiveness, 20, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(defe_su_t0, deferential_realism_ontology__rhetorical_scaffold_reading, suppression_requirement, 0, 0.1).
narrative_ontology:measurement(defe_su_t4, deferential_realism_ontology__rhetorical_scaffold_reading, suppression_requirement, 4, 0.12).
narrative_ontology:measurement(defe_su_t8, deferential_realism_ontology__rhetorical_scaffold_reading, suppression_requirement, 8, 0.13).
narrative_ontology:measurement(defe_su_t12, deferential_realism_ontology__rhetorical_scaffold_reading, suppression_requirement, 12, 0.15).
narrative_ontology:measurement(defe_su_t16, deferential_realism_ontology__rhetorical_scaffold_reading, suppression_requirement, 16, 0.16).
narrative_ontology:measurement(defe_su_t20, deferential_realism_ontology__rhetorical_scaffold_reading, suppression_requirement, 20, 0.18).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(deferential_realism_ontology__rhetorical_scaffold_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(deferential_realism_ontology__rhetorical_scaffold_reading, 0.1).
narrative_ontology:affects_constraint(deferential_realism_ontology__rhetorical_scaffold_reading, deferential_realism_ontology__immutable_diagnostic_reading).
narrative_ontology:affects_constraint(deferential_realism_ontology__rhetorical_scaffold_reading, deferential_realism_ontology__hybrid_pragmatic_reading).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the deferential_realism_ontology kernel. immutable_diagnostic_reading treats the typology as fixed-referent measurement (near-mountain epsilon, high accessibility_collapse); hybrid_pragmatic_reading splits the typology into a fixed core (mountains/ropes) and contested normative periphery (tangled_ropes/snares); this rhetorical_scaffold_reading treats the entire typology, including 'mountain' and 'rope,' as normative vocabulary whose value is persuasive rather than diagnostic. The three stories share no epsilon value — each is measured independently per the ε-invariance principle — and are linked here so contamination/coupling analysis can trace how a credibility shock to one reading (e.g., empirical debunking of the diagnostic reading's claimed objectivity) propagates to the others.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
