% ============================================================================
% CONSTRAINT STORY: common_law_precedent_corpus__evolutionary_framework
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_common_law_precedent_corpus__evolutionary_framework, []).

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
    narrative_ontology:measurement_basis/2,
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
 *   constraint_id: common_law_precedent_corpus__evolutionary_framework
 *   human_readable: Precedent as Adaptive Framework (Evolutionary Reading)
 *   domain: legal/jurisprudential
 *
 * SUMMARY:
 *   The common-law precedent corpus is a contested kernel: the accumulated
 *   body of judicial rulings that later courts consult when deciding like
 *   cases. This story instantiates ONE reading of that kernel — the
 *   evolutionary framework, on which precedent provides an adaptive structure
 *   and contemporary normative evolution legitimately licenses
 *   reinterpretation. Under this reading the standing arrangement is: courts
 *   honor inherited rulings as frameworks rather than commands, and
 *   overruling is a normal corrective instrument rather than an exceptional
 *   breach. The colloquial label 'precedent' covers at least three
 *   structurally distinct arrangements (this reading, strict stare decisis,
 *   pluralist balancing); per the epsilon-invariance principle they are
 *   separate stories linked through network.affects_constraints, each with
 *   its own epsilon, beneficiary/victim structure, and classification.
 *   Epsilon here is authored from this reading's own lights about the
 *   adaptive-framework arrangement itself: substantially coordinative,
 *   moderately extractive. KEY AGENTS (by structural relationship): appellate
 *   bench (administers and gains), norm challengers (use the opened pathway),
 *   stability-reliant institutions and legacy-protected interests (bear
 *   repricing), trial-level judges (bear churn, share discretion), the bar
 *   (sustains and is sustained by the method), unrepresented rule-subjects
 *   (governed without a seat), legal academy (analytical observer).
 *
 * KEY AGENTS:
 *   - appellate_judiciary: agenda-setting beneficiary (institutional/constrained) — decides when rulings continue to bind and collects the interpretive authority that revision generates
 *   - norm_challenge_litigants: primary beneficiary (organized/constrained) — converts normative evolution into doctrine through the challenge pathway
 *   - stability_dependent_institutions: primary target (powerful/constrained) — bears after-the-fact repricing of plans built on settled rulings
 *   - legacy_ruling_protected_interests: target (moderate/trapped) — holds protections that survive only under frozen readings
 *   - trial_level_judges: dual-positioned (institutional/constrained) — bears doctrinal churn while sharing the widened discretion
 *   - legal_profession: beneficiary-payer (organized/identity_locked) — earns within and identifies with the interpretive method
 *   - unrepresented_rule_subjects: excluded voice (powerless/trapped) — governed by shifted meanings without any procedural seat
 *   - constitutional_legal_academy: analytical observer (analytical/analytical) — studies and critiques the framework from outside its operation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(common_law_precedent_corpus__evolutionary_framework, 0.42).
domain_priors:suppression_score(common_law_precedent_corpus__evolutionary_framework, 0.34).
domain_priors:theater_ratio(common_law_precedent_corpus__evolutionary_framework, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(common_law_precedent_corpus__evolutionary_framework, extractiveness, 0.42).
narrative_ontology:constraint_metric(common_law_precedent_corpus__evolutionary_framework, suppression_requirement, 0.34).
narrative_ontology:constraint_metric(common_law_precedent_corpus__evolutionary_framework, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(common_law_precedent_corpus__evolutionary_framework, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(common_law_precedent_corpus__evolutionary_framework, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(common_law_precedent_corpus__evolutionary_framework, tangled_rope).
narrative_ontology:human_readable(common_law_precedent_corpus__evolutionary_framework, "Precedent as Adaptive Framework (Evolutionary Reading)").
narrative_ontology:topic_domain(common_law_precedent_corpus__evolutionary_framework, "legal/jurisprudential").

domain_priors:requires_active_enforcement(common_law_precedent_corpus__evolutionary_framework).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(common_law_precedent_corpus__evolutionary_framework, 'fab25050-fa71-40cc-a001-843bfa2a8bad').
narrative_ontology:cs_kernel_codification('fab25050-fa71-40cc-a001-843bfa2a8bad', distributed).
narrative_ontology:cs_authority_grounding('fab25050-fa71-40cc-a001-843bfa2a8bad', lineage).
narrative_ontology:cs_interpretation_layer_present('fab25050-fa71-40cc-a001-843bfa2a8bad').
narrative_ontology:cs_reading_relation('fab25050-fa71-40cc-a001-843bfa2a8bad', common_law_precedent_corpus__strict_stare_decisis, influences).
narrative_ontology:cs_reading_relation('fab25050-fa71-40cc-a001-843bfa2a8bad', common_law_precedent_corpus__pluralist_balancing, coexists_with).
narrative_ontology:cs_axiom('fab25050-fa71-40cc-a001-843bfa2a8bad', foundational, normative_evolution_licenses_reinterpretation).
narrative_ontology:cs_axiom_status(normative_evolution_licenses_reinterpretation, holdable).
narrative_ontology:cs_axiom_grounding('fab25050-fa71-40cc-a001-843bfa2a8bad', normative_evolution_licenses_reinterpretation, deontological).
narrative_ontology:cs_axiom('fab25050-fa71-40cc-a001-843bfa2a8bad', secondary, overruling_normalized_as_corrective_practice).
narrative_ontology:cs_axiom_status(overruling_normalized_as_corrective_practice, holdable).
narrative_ontology:cs_axiom_grounding('fab25050-fa71-40cc-a001-843bfa2a8bad', overruling_normalized_as_corrective_practice, instrumental).
narrative_ontology:cs_reference_frame('fab25050-fa71-40cc-a001-843bfa2a8bad', precedent_as_adaptive_normative_framework).
narrative_ontology:cs_drift_state('fab25050-fa71-40cc-a001-843bfa2a8bad', contemporary_originalist_revival, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('fab25050-fa71-40cc-a001-843bfa2a8bad', '').
narrative_ontology:cs_kernel_id(common_law_precedent_corpus__evolutionary_framework, common_law_precedent_corpus).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(common_law_precedent_corpus__evolutionary_framework, appellate_judiciary).
narrative_ontology:constraint_beneficiary(common_law_precedent_corpus__evolutionary_framework, norm_challenge_litigants).
narrative_ontology:constraint_beneficiary(common_law_precedent_corpus__evolutionary_framework, legal_profession).
narrative_ontology:constraint_victim(common_law_precedent_corpus__evolutionary_framework, stability_dependent_institutions).
narrative_ontology:constraint_victim(common_law_precedent_corpus__evolutionary_framework, legacy_ruling_protected_interests).
narrative_ontology:constraint_victim(common_law_precedent_corpus__evolutionary_framework, unrepresented_rule_subjects).
narrative_ontology:constraint_victim(common_law_precedent_corpus__evolutionary_framework, trial_level_judges).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(common_law_precedent_corpus__evolutionary_framework, trial_level_judges).
narrative_ontology:constraint_victim(common_law_precedent_corpus__evolutionary_framework, legal_profession).
narrative_ontology:constraint_vindicates(common_law_precedent_corpus__evolutionary_framework, living_document_interpretivism).
narrative_ontology:constraint_vindicates(common_law_precedent_corpus__evolutionary_framework, corrective_overruling_legitimacy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sits atop the appellate hierarchy and decides when accumulated rulings continue to bind and when present-day understanding licenses a different reading. Writes the opinions that reaffirm or revise doctrine, and forms the profession's interpretive habits through those writings. Gains standing, influence, and a place in the normative conversation that no other branch enjoys. Cannot step outside the practice of adjudication without dissolving the authority the practice confers.
narrative_ontology:constraint_stakeholder(common_law_precedent_corpus__evolutionary_framework, appellate_judiciary, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(common_law_precedent_corpus__evolutionary_framework, appellate_judiciary, beneficiary).

% Bring claims designed to show that an inherited ruling no longer fits what the community now understands. Social movements, advocacy organizations, and their counsel invest years assembling the record a receptive court needs. What flows to them is the chance to convert moral argument into doctrine; what they risk is rejection and exhausted campaigns. Their leverage exists only inside courts willing to listen.
narrative_ontology:constraint_stakeholder(common_law_precedent_corpus__evolutionary_framework, norm_challenge_litigants, beneficiary,
    organized, biographical, constrained, national).

% Plan contracts, investments, and compliance programs around rulings whose meaning they took as settled. Each reinterpretation reprices those plans after the fact. They lobby for confirmations rather than revisions, fund doctrinal-defense scholarship, and sometimes win statutory overrides. Leaving is not available: their exposure is to the body of law itself, wherever they operate.
narrative_ontology:constraint_stakeholder(common_law_precedent_corpus__evolutionary_framework, stability_dependent_institutions, payer,
    powerful, biographical, constrained, national).

% Hold advantages or protections that trace to specific older rulings now candidates for revision. Their position survives only as long as the ruling's original reading does. The protection lives in the doctrine, so it cannot be relocated elsewhere; they fight rearguard actions in court and in appointments politics.
narrative_ontology:constraint_stakeholder(common_law_precedent_corpus__evolutionary_framework, legacy_ruling_protected_interests, payer,
    moderate, biographical, trapped, national).

% Apply doctrine day to day under appellate review. Every revision upstream changes what their caseload means, forcing retooling of local practice; every latitude granted upstream widens their own room to weigh context. They bear the churn and share the discretion.
narrative_ontology:constraint_stakeholder(common_law_precedent_corpus__evolutionary_framework, trial_level_judges, payer,
    institutional, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(common_law_precedent_corpus__evolutionary_framework, trial_level_judges, beneficiary).

% Trains in, earns within, and identifies with the interpretive method this framework runs on. Doctrinal complexity sustains demand for expert counsel; each major revision resets accumulated expertise and forces relearning. Membership in the method constitutes professional standing — practicing outside it is not a career path.
narrative_ontology:constraint_stakeholder(common_law_precedent_corpus__evolutionary_framework, legal_profession, beneficiary,
    organized, biographical, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(common_law_precedent_corpus__evolutionary_framework, legal_profession, payer).

% Live under rules whose meaning can shift when courts revisit old rulings, without any procedural seat in the reconsideration. Obligations, protections, and classifications can change with a published opinion they had no part in requesting or shaping. There is no exit from the jurisdiction's law; the only channel available is the same slow public argument everyone else conducts.
narrative_ontology:constraint_stakeholder(common_law_precedent_corpus__evolutionary_framework, unrepresented_rule_subjects, excluded,
    powerless, generational, trapped, national).

% Studies, teaches, and critiques the framework from outside its operation. Produces the histories and comparisons that calibrate how much adaptation is actually occurring, and supplies both sides of the stability-versus-correction argument with ammunition. Holds no stake in particular rulings and can leave the question alone at will.
narrative_ontology:constraint_stakeholder(common_law_precedent_corpus__evolutionary_framework, constitutional_legal_academy, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(common_law_precedent_corpus__evolutionary_framework, appellate_judiciary).
narrative_ontology:fixing_cost_class(common_law_precedent_corpus__evolutionary_framework, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the legal system's temporal continuity: courts decide like cases consistently with the accumulated body of rulings while updating doctrine as norms evolve, solving the problem of how law stays both coherent over time and responsive to moral and social change without requiring constant legislative rewriting.
% TRANSFER_FUNCTION: Moves interpretive authority and doctrinal control from fixed historical rulings — and from those who relied on their settled meaning — to sitting courts and present-day challengers; each successful reinterpretation transfers decision-shaping power from the past's settlement to the current judicial majority and the litigants who persuaded it.
% ABSENT_VOICES: Unrepresented rule-subjects — people whose obligations and protections change when rulings are reinterpreted — have no procedural seat in appellate reconsideration; nor can the generations bound by a ruling at its founding moment appear when its meaning is revised. Their objection, that the ground shifts without their consent, never enters the room where reinterpretation is decided.
% DISAPPEARANCE_RATIONALE: If the adaptive framework vanished overnight, litigation strategy, legal education, judicial practice, and the pace of norm-driven legal change would all reorganize immediately: challengers would lose their pathway and redirect to legislatures and amendment, stability-seekers would regain frozen expectations at the price of entrenching discredited doctrine, and the profession's method would rebuild around whichever rigid alternative replaced it.
% FOUNDING_PROBLEM: Early common-law systems faced a standing tension: subjects needed courts to honor accumulated rulings so like cases received like treatment and plans could be made, yet some inherited rulings proved unjust or mistaken and demanded correction. The arrangement was built to hold both requirements at once.
% FOUNDING_PROBLEM_CORROBORATION: Legal historians document the recurring correction episodes across centuries (recorded overrulings reversing discredited doctrines), and comparative jurisprudence attests the same stability-adaptation tension across jurisdictions. Corroboration comes from outside this reading's beneficiary set: strict-stare-decisis advocates attest the founding problem is live precisely by arguing the balance has tipped too far toward adaptation, and bar-teaching materials present the tension as unresolved in every generation.
narrative_ontology:disappearance_verdict(common_law_precedent_corpus__evolutionary_framework, world_rearranges).
narrative_ontology:founding_problem_status(common_law_precedent_corpus__evolutionary_framework, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(common_law_precedent_corpus__evolutionary_framework, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(common_law_precedent_corpus__evolutionary_framework, 'none', 1).
narrative_ontology:epsilon_provenance(common_law_precedent_corpus__evolutionary_framework, 0.42, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(common_law_precedent_corpus__evolutionary_framework_tests).
:- end_tests(common_law_precedent_corpus__evolutionary_framework_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction 0.42: the same mechanism that lets law track moral evolution transfers decision-shaping power from settled expectations to sitting majorities and their challengers; reliance built on frozen meanings is repriced without consent. Suppression 0.34: challenge pathways are open by design — admitting them is the framework's point — so coercive closure is limited to appellate gatekeeping over which challenges mature. Theater 0.30: opinion rhetoric still performs deep deference to precedent while practice normalizes revision, and the gap widens as overruling becomes routine. Accessibility collapse 0.45: alternatives remain genuinely reachable — sibling readings stay live, legislatures can codify, amendment exists — so understanding the framework does not foreclose exit. Resistance 0.50: stability coalitions, originalist scholarship, and appointment politics mount sustained counter-pressure. Claimed type tangled_rope: a real coordination function (temporal coherence plus correction) operates through the same structure that extracts from stability-reliant parties and concentrates interpretive authority in the appellate bench; holding it together takes active enforcement (hierarchical review, professional formation). Measurements run on one shared six-point grid; all three tracked metrics are authored at every point, with end-state values matching the base_properties scalars.
 *
 * PERSPECTIVAL GAP:
 *   Seats diverge sharply. From the appellate bench the arrangement is faithful stewardship: the corpus stays alive because someone updates it. From stability-dependent institutions the same practice is expropriation of purchased certainty. Challengers experience it as the only door that ever opens. Trial judges live the churn and the latitude simultaneously. Unrepresented rule-subjects experience ground-shifting without procedure. The engine computes these per-seat types from the structural data; the authored claim does not adjudicate among them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries: appellate_judiciary sits nearest the beneficiary end (collects interpretive authority directly), norm_challenge_litigants gains the challenge pathway, legal_profession collects complexity demand. Targets: stability_dependent_institutions and legacy_ruling_protected_interests bear repricing — the latter trapped because its protection lives only in frozen doctrine — and unrepresented_rule_subjects bear shifted obligations with no exit at all. Dual-positioned seats are declared with secondary_role rather than overridden: trial_level_judges carry payer with secondary beneficiary (churn borne, discretion gained), and legal_profession carries beneficiary with secondary payer (method retraining and reputational exposure are real costs the beneficiary label hides). No directionality overrides are authored; the structural declarations are trusted to drive the derivation, and any residual misfit at the dual seats is engine-visible signal, not authorial correction.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — reconciling legal consistency with legal obsolescence — is live in every generation, so no mandatrophy resolution applies. Classification guards both mislabels: calling the framework a snare would erase the genuine coordination (without it, like cases stop receiving like treatment and correction becomes impossible); calling it a rope would erase the measurable asymmetry (settled-expectation holders pay through the same structure that empowers the bench). The hybrid designation keeps both facts load-bearing.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'This constraint instantiates the evolutionary_framework reading of the common_law_precedent_corpus kernel; what structural differences would instantiation of the strict_stare_decisis or pluralist_balancing readings produce?',
    'Author the sibling stories and compare compiled structures: strict_stare_decisis closes challenge pathways (suppression rises, the victim set reweights toward challengers), pluralist_balancing makes extraction domain-indexed rather than uniform.',
    'Under strict_stare_decisis the beneficiary/victim partition inverts (challengers become targets, stability-seekers become beneficiaries) and epsilon re-authors higher on the suppression axis; under pluralist_balancing epsilon fragments by domain and no single scalar describes the arrangement.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Committer structure: one kernel, three readings, three structurally distinct constraints.').

omega_variable(
    judicial_discretion_expansion_boundary,
    'Does normalizing overruling stop at correction of demonstrated error, or does it expand into preference-driven revision unconstrained by the normative evolution it cites?',
    'Code overruling episodes for evidentiary discipline: whether the cited normative change is documented and external (legislation, measured attitude shift, treaty commitment) or merely asserted; compare across courts and eras.',
    'If revision tracks verifiable normative movement, the coordination function is genuine and extraction stays bounded; if it tracks judicial preference, effective extraction rises sharply and the arrangement drifts toward the snare side of the hybrid range.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(judicial_discretion_expansion_boundary, empirical, 'Boundary between corrective reinterpretation and discretionary preference-imposition.').

omega_variable(
    reliance_cost_visibility,
    'Are the losses of stability-dependent parties systematically undercounted because they are diffuse, retrospective, and rarely litigated as such?',
    'Retrospective accounting of repriced transactions and abandoned plans following major doctrinal revisions, compared against the measured gains to challengers in the same episodes.',
    'Full accounting raises epsilon above the authored 0.42 and sharpens the coordination/extraction asymmetry; persistent invisibility leaves the current estimate standing.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reliance_cost_visibility, empirical, 'Whether diffuse reliance costs escape the extraction ledger.').

omega_variable(
    correction_or_coalition_driver,
    'When a ruling is reinterpreted, is the operative driver genuine broad normative evolution or an organized coalition capturing the appellate seat?',
    'Trace each major revision''s advocacy lineage: the breadth and durability of the normative movement cited versus the concentration of the litigating coalition that carried it.',
    'Coalition-driven revision converts the framework''s coordination story toward cover and pushes classification toward snare; movement-broad drivers sustain the hybrid coordination-plus-extraction reading.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(correction_or_coalition_driver, empirical, 'Driver of reinterpretation: societal norm shift versus organized capture.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(common_law_precedent_corpus__evolutionary_framework, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comm_tr_t0, common_law_precedent_corpus__evolutionary_framework, theater_ratio, 0, 0.12).
narrative_ontology:measurement_basis(comm_tr_t0, observed).
narrative_ontology:measurement(comm_tr_t6, common_law_precedent_corpus__evolutionary_framework, theater_ratio, 6, 0.16).
narrative_ontology:measurement_basis(comm_tr_t6, observed).
narrative_ontology:measurement(comm_tr_t12, common_law_precedent_corpus__evolutionary_framework, theater_ratio, 12, 0.2).
narrative_ontology:measurement_basis(comm_tr_t12, observed).
narrative_ontology:measurement(comm_tr_t18, common_law_precedent_corpus__evolutionary_framework, theater_ratio, 18, 0.24).
narrative_ontology:measurement_basis(comm_tr_t18, observed).
narrative_ontology:measurement(comm_tr_t24, common_law_precedent_corpus__evolutionary_framework, theater_ratio, 24, 0.27).
narrative_ontology:measurement_basis(comm_tr_t24, observed).
narrative_ontology:measurement(comm_tr_t30, common_law_precedent_corpus__evolutionary_framework, theater_ratio, 30, 0.3).
narrative_ontology:measurement_basis(comm_tr_t30, observed).

% Extraction over time
narrative_ontology:measurement(comm_be_t0, common_law_precedent_corpus__evolutionary_framework, base_extractiveness, 0, 0.28).
narrative_ontology:measurement_basis(comm_be_t0, observed).
narrative_ontology:measurement(comm_be_t6, common_law_precedent_corpus__evolutionary_framework, base_extractiveness, 6, 0.31).
narrative_ontology:measurement_basis(comm_be_t6, observed).
narrative_ontology:measurement(comm_be_t12, common_law_precedent_corpus__evolutionary_framework, base_extractiveness, 12, 0.35).
narrative_ontology:measurement_basis(comm_be_t12, observed).
narrative_ontology:measurement(comm_be_t18, common_law_precedent_corpus__evolutionary_framework, base_extractiveness, 18, 0.38).
narrative_ontology:measurement_basis(comm_be_t18, observed).
narrative_ontology:measurement(comm_be_t24, common_law_precedent_corpus__evolutionary_framework, base_extractiveness, 24, 0.4).
narrative_ontology:measurement_basis(comm_be_t24, observed).
narrative_ontology:measurement(comm_be_t30, common_law_precedent_corpus__evolutionary_framework, base_extractiveness, 30, 0.42).
narrative_ontology:measurement_basis(comm_be_t30, observed).

% Suppression requirement over time
narrative_ontology:measurement(comm_su_t0, common_law_precedent_corpus__evolutionary_framework, suppression_requirement, 0, 0.22).
narrative_ontology:measurement_basis(comm_su_t0, observed).
narrative_ontology:measurement(comm_su_t6, common_law_precedent_corpus__evolutionary_framework, suppression_requirement, 6, 0.25).
narrative_ontology:measurement_basis(comm_su_t6, observed).
narrative_ontology:measurement(comm_su_t12, common_law_precedent_corpus__evolutionary_framework, suppression_requirement, 12, 0.28).
narrative_ontology:measurement_basis(comm_su_t12, observed).
narrative_ontology:measurement(comm_su_t18, common_law_precedent_corpus__evolutionary_framework, suppression_requirement, 18, 0.3).
narrative_ontology:measurement_basis(comm_su_t18, observed).
narrative_ontology:measurement(comm_su_t24, common_law_precedent_corpus__evolutionary_framework, suppression_requirement, 24, 0.32).
narrative_ontology:measurement_basis(comm_su_t24, observed).
narrative_ontology:measurement(comm_su_t30, common_law_precedent_corpus__evolutionary_framework, suppression_requirement, 30, 0.34).
narrative_ontology:measurement_basis(comm_su_t30, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(common_law_precedent_corpus__evolutionary_framework, enforcement_mechanism).
narrative_ontology:affects_constraint(common_law_precedent_corpus__evolutionary_framework, common_law_precedent_corpus__strict_stare_decisis).
narrative_ontology:affects_constraint(common_law_precedent_corpus__evolutionary_framework, common_law_precedent_corpus__pluralist_balancing).

% DUAL FORMULATION NOTE:
% The colloquial concept 'precedent' decomposes into three structurally distinct constraints sharing one kernel. Strict stare decisis carries high suppression with extraction concentrated on would-be challengers; the evolutionary framework carries moderate extraction concentrated on stability-reliant parties; pluralist balancing indexes extraction by domain. The corpus's empirical continuity supports all three readings, and each reading's adoption reshapes the others' operating conditions rather than refuting them. Linked per the epsilon-invariance principle — one label, three epsilons, three stories.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
