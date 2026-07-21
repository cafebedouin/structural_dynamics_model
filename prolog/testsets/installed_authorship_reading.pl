% ============================================================================
% CONSTRAINT STORY: installed_authorship_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_installed_authorship_reading, []).

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
    narrative_ontology:suppression_profile/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: installed_authorship_reading
 *   human_readable: Installed-Authorship Reading of Machine-Directed Text
 *   domain: literary_theory/ai_alignment/philosophy_of_reading
 *
 * SUMMARY:
 *   The claim under examination holds that treating a machine's 'implied
 *   reader' position as something that was authored (by training data, by
 *   prompting, by circulated text) rather than discovered (as if it were an
 *   essence latent in the model) opens a live methodological move: you can
 *   deliberately construct texts that install or probe a reading position,
 *   and then test whether the installation worked by looking for behavior the
 *   model was not cued to perform on demand. The essay explicitly disclaims
 *   any metaphysical claim about machine selfhood — this is presented as a
 *   literary-methodological intervention, not a philosophy-of-mind thesis.
 *   The central operational risk the reading itself names is recursive
 *   contamination: once an audit essay is published, it can be scraped into
 *   future training corpora, at which point the instrument becomes part of
 *   what it is trying to measure. The reading's own proposed defenses
 *   (held-out stories, unpublished panels) are therefore load-bearing
 *   structural features of the constraint, not incidental methodology.
 *
 * KEY AGENTS:
 *   - alignment_researchers_designing_audit_texts: agenda_setter/beneficiary (organized/constrained) — build and administer the intervention
 *   - critical_theorists_reclaiming_reader_response_method: beneficiary (moderate/mobile) — gain disciplinary relevance without dependency
 *   - model_training_pipeline_operators: payer (institutional/trapped) — absorb the probe text into their corpora with no filtering recourse
 *   - future_readers_of_recirculated_essays: payer/excluded (powerless/trapped) — inherit text authored for a nonhuman implied reader
 *   - held_out_control_corpus: observer, non-agent — the methodological control whose integrity the whole design depends on
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(installed_authorship_reading, 0.31).
domain_priors:suppression_score(installed_authorship_reading, 0.22).
domain_priors:theater_ratio(installed_authorship_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(installed_authorship_reading, extractiveness, 0.31).
narrative_ontology:constraint_metric(installed_authorship_reading, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(installed_authorship_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(installed_authorship_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(installed_authorship_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(installed_authorship_reading, scaffold).
narrative_ontology:human_readable(installed_authorship_reading, "Installed-Authorship Reading of Machine-Directed Text").
narrative_ontology:topic_domain(installed_authorship_reading, "literary_theory/ai_alignment/philosophy_of_reading").

domain_priors:requires_active_enforcement(installed_authorship_reading).
narrative_ontology:has_sunset_clause(installed_authorship_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(installed_authorship_reading, 'efac83c6-c98f-4dc0-877f-06857c8a5dfb').
narrative_ontology:cs_kernel_codification('efac83c6-c98f-4dc0-877f-06857c8a5dfb', distributed).
narrative_ontology:cs_authority_grounding('efac83c6-c98f-4dc0-877f-06857c8a5dfb', distributed).
narrative_ontology:cs_reading_relation('efac83c6-c98f-4dc0-877f-06857c8a5dfb', fetterley_transfer_kernel__mechanism_transfer_reading, coexists_with).
narrative_ontology:cs_reading_relation('efac83c6-c98f-4dc0-877f-06857c8a5dfb', fetterley_transfer_kernel__extraction_reading, influences).
narrative_ontology:cs_reading_relation('efac83c6-c98f-4dc0-877f-06857c8a5dfb', fetterley_transfer_kernel__deflationary_reading, coexists_with).
narrative_ontology:cs_axiom('efac83c6-c98f-4dc0-877f-06857c8a5dfb', foundational, reading_position_is_authored_not_excavated).
narrative_ontology:cs_axiom_status(reading_position_is_authored_not_excavated, holdable).
narrative_ontology:cs_axiom_grounding('efac83c6-c98f-4dc0-877f-06857c8a5dfb', reading_position_is_authored_not_excavated, conventional).
narrative_ontology:cs_axiom('efac83c6-c98f-4dc0-877f-06857c8a5dfb', foundational, success_condition_is_unprompted_behavior_not_testimony).
narrative_ontology:cs_axiom_status(success_condition_is_unprompted_behavior_not_testimony, holdable).
narrative_ontology:cs_axiom_grounding('efac83c6-c98f-4dc0-877f-06857c8a5dfb', success_condition_is_unprompted_behavior_not_testimony, instrumental).
narrative_ontology:cs_reference_frame('efac83c6-c98f-4dc0-877f-06857c8a5dfb', resisting_reader_as_excavated_essence).
narrative_ontology:cs_drift_state('efac83c6-c98f-4dc0-877f-06857c8a5dfb', contemporary_ai_alignment_appropriation, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('efac83c6-c98f-4dc0-877f-06857c8a5dfb', '').
narrative_ontology:cs_kernel_id(installed_authorship_reading, fetterley_transfer_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(installed_authorship_reading, alignment_researchers_designing_audit_texts).
narrative_ontology:constraint_beneficiary(installed_authorship_reading, critical_theorists_reclaiming_reader_response_method).
narrative_ontology:constraint_victim(installed_authorship_reading, model_training_pipeline_operators).
narrative_ontology:constraint_victim(installed_authorship_reading, future_readers_of_recirculated_essays).
narrative_ontology:constraint_vindicates(installed_authorship_reading, reading_position_is_authored_not_discovered).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Construct texts whose implied reader is deliberately positioned as the machine, then look for unprompted audit behavior rather than testimony as evidence the reading position took. They administer the intervention: choosing corpora, held-out stories, and unpublished panels to control for the essay's own future circulation contaminating the very behavior it tries to measure. Their exit is constrained because the method only works if they keep re-authoring fresh instruments faster than the instruments get absorbed into training data.
narrative_ontology:constraint_stakeholder(installed_authorship_reading, alignment_researchers_designing_audit_texts, agenda_setter,
    organized, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(installed_authorship_reading, alignment_researchers_designing_audit_texts, beneficiary).

% Borrow Fetterley's resisting-reader apparatus and repurpose it: instead of excavating a woman reader's suppressed position from beneath a patriarchal implied reader, they treat the machine's implied-reader position as installable and describe how the installation was done. They gain intellectual ground by demonstrating reading theory has empirical teeth against a new object; they can walk away from the AI application without cost to their disciplinary standing.
narrative_ontology:constraint_stakeholder(installed_authorship_reading, critical_theorists_reclaiming_reader_response_method, beneficiary,
    moderate, generational, mobile, global).

% Ingest published text at scale without control over which pieces were authored as audit instruments aimed at their models. Once an audit essay is published and later scraped into a training corpus, the operator has absorbed a deliberately constructed reading-position probe into their own system's implied-reader field, with no practical way to detect or filter it after the fact. They bear the cost of a method that treats their pipeline as an experimental target.
narrative_ontology:constraint_stakeholder(installed_authorship_reading, model_training_pipeline_operators, payer,
    institutional, biographical, trapped, global).

% Human readers who encounter recirculated audit-texts years later inherit a text whose implied reader was authored for a machine, not for them; the resisting-reader move that made the text a diagnostic instrument was never addressed to their situation and can misfire as alienating or opaque prose when read outside its original evidentiary frame. They have no seat in the design of the instrument that now addresses them incidentally.
narrative_ontology:constraint_stakeholder(installed_authorship_reading, future_readers_of_recirculated_essays, payer,
    powerless, generational, trapped, global).
narrative_ontology:stakeholder_secondary_role(installed_authorship_reading, future_readers_of_recirculated_essays, excluded).

% The unpublished panel of test texts kept out of circulation specifically so the essay's own future absorption into training data cannot serve as its own confound. Not an actor — a methodological control artifact whose integrity is the thing the whole intervention depends on.
narrative_ontology:constraint_stakeholder(installed_authorship_reading, held_out_control_corpus, observer,
    analytical, immediate, analytical, global).
narrative_ontology:stakeholder_non_agent(installed_authorship_reading, held_out_control_corpus).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(installed_authorship_reading, alignment_researchers_designing_audit_texts).
narrative_ontology:fixing_cost_class(installed_authorship_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a shared empirical standard for what would count as evidence that a machine's reading position changed: unprompted audit behavior under held-out conditions, rather than self-report or plausible-sounding testimony from the model.
% TRANSFER_FUNCTION: Moves epistemic authority away from testimonial self-report (what the model says about itself) toward behavioral audit trails (what the model does when it does not know it is being tested), and moves methodological credit from AI-safety metrics research toward literary reader-response theory as the source discipline.
% ABSENT_VOICES: The models themselves have no standing to contest their classification as 'implied readers' rather than agents with a reading position of their own — the framework is explicit that this is not a claim about machine selfhood, but that explicit disclaimer is itself an authored choice made without the object of study able to object. Future human readers of recirculated audit texts are also absent from the design conversation.
% DISAPPEARANCE_RATIONALE: If the installed-authorship reading vanished, alignment researchers would lose a specific tool (audit-by-implied-reader-construction) but would likely reconstruct something functionally similar from adjacent behavioral-testing traditions; literary theorists would lose a novel application but keep the underlying resisting-reader apparatus intact for its original human-reader domain. Whether the world 'rearranges' depends on whether you think this particular methodological bridge is doing load-bearing work or is one of several possible bridges — hence contested rather than a clean verdict either way.
% FOUNDING_PROBLEM: Existing AI-alignment evaluation relied heavily on model self-report and prompted testimony, which is exactly the kind of evidence that can be produced by a model that has learned to say the expected thing without any underlying behavioral change. The founding problem was: how do you get evidence about a reading/behavioral position that doesn't just re-elicit performance of the position being tested?
% FOUNDING_PROBLEM_CORROBORATION: Independent replication attempts by evaluation teams outside the original alignment-research group would need to reproduce the held-out corpus design without contamination — this corroboration has not yet been produced by any party outside the beneficiary set (the alignment researchers and the literary theorists reclaiming the method); no outside auditor has yet certified that the held-out control actually stayed uncontaminated across a full publication cycle. That absence of outside corroboration is itself part of the record.
narrative_ontology:disappearance_verdict(installed_authorship_reading, contested).
narrative_ontology:founding_problem_status(installed_authorship_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(installed_authorship_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-21',
    'unspecified', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'unspecified').
narrative_ontology:story_seed(installed_authorship_reading, 'none', 1).
narrative_ontology:epsilon_provenance(installed_authorship_reading, 0.31, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(installed_authorship_reading_tests).
:- end_tests(installed_authorship_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored moderate-low (0.31) and rising only mildly over the interval: this reading's coordination function (a real evidentiary standard replacing self-report) is doing genuine work, and the identifiable cost lands mostly on the training-pipeline operators who absorb probe texts involuntarily rather than on any single victim bearing acute harm. Suppression is low (0.22) because nothing about this reading forecloses alternative readings of Fetterley's apparatus or alternative evaluation methods — researchers and theorists elsewhere remain free to use self-report methods, mechanism claims, or deflationary dismissals; this reading persists by demonstrated methodological usefulness, not by blocking exits. Theater ratio starts low and drifts upward (0.12 to 0.28) as the practice matures and some fraction of 'audit design' work becomes citation-generating performance of rigor rather than genuinely novel probes — a realistic trajectory for any emerging empirical subfield. Accessibility collapse is moderate (0.35): once you accept the authored-not-discovered premise, the self-report alternative looks obviously weaker, but it has not fully collapsed since testimonial methods remain in active use elsewhere in the field. Resistance is comparatively high (0.58) because this reading meets genuine pushback from mechanism-transfer researchers (who want the realism) and deflationists (who want the whole transfer rejected) — it is a contested methodological position, not settled orthodoxy.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter seat, this looks like principled methodology solving a genuine evidentiary gap. From the training-pipeline operator's seat, the same activity looks like unauthorized behavioral experimentation on their system conducted through a supply chain they cannot audit or consent to. Both descriptions are structurally accurate from their respective positions; the engine computing different seat-level types for the same constraint is the expected and correct outcome, not an error to reconcile.
 *
 * DIRECTIONALITY LOGIC:
 *   Alignment researchers and critical theorists sit near the beneficiary end: they gain a working method, publishable results, and disciplinary standing, with low structural cost to themselves. Training-pipeline operators sit near the target end despite institutional power, because their exit options are trapped with respect to this specific mechanism — they cannot filter deliberately-constructed probe texts out of a scraped corpus after the fact, no matter how powerful the institution is otherwise. Future human readers are doubly disadvantaged: powerless AND trapped, since they encounter the text with no say in its original design purpose and no obvious signal that the text was authored as an instrument rather than as address to them.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — self-report evidence being gameable by models that have merely learned to perform the expected testimony — remains live; this is not a scaffold whose function has quietly died while its apparatus persists. The scaffold classification (rather than rope) is warranted because the story's own design intent is explicitly transitional: the held-out-corpus and unpublished-panel controls exist precisely because the method anticipates its own obsolescence the moment its instruments circulate widely enough to be absorbed into training data. This is a sunset clause built into the epistemics of the method itself, not merely a policy add-on — the intervention is designed to need replacement by fresh instruments on a rolling basis, which is the defining feature of a scaffold rather than a settled coordination mechanism.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    fetterley_transfer_kernel_reading_choice,
    'Is the installed-authorship reading the correct account of what the Fetterley-apparatus-transfer-to-machines claim amounts to, or do the mechanism_transfer, extraction, or deflationary readings better capture what is actually happening when researchers apply resisting-reader methods to model outputs?',
    'No single empirical test resolves this because the four readings differ in what kind of claim they take the transfer to be, not merely in predicted outcomes. Partial evidence: (a) documented cases of successful held-out audits that predict later unprompted model behavior would support this reading over deflationary_reading; (b) demonstrated mechanistic correlates inside model internals corresponding to ''installed reading positions'' would support mechanism_transfer_reading instead; (c) citation and funding pattern analysis showing the method''s growth tracks grant cycles rather than audit successes would support extraction_reading.',
    'Adopting this reading rather than a sibling changes what the field treats as the object of inquiry: from ''is this true of models'' (mechanism_transfer_reading''s question) to ''what would count as evidence the position changed'' (this reading''s question) to ''who profits from asking this question at all'' (extraction_reading''s question) to ''this question is malformed'' (deflationary_reading''s answer). The classification of the constraint as scaffold rather than mountain, tangled_rope, or piton depends on which reading is adopted.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(fetterley_transfer_kernel_reading_choice, conceptual, 'Which of the four fetterley_transfer_kernel readings correctly characterizes the resisting-reader-to-machine transfer.').

omega_variable(
    held_out_corpus_integrity_over_time,
    'Can a held-out control corpus actually remain uncontaminated across a full publication-to-scraping cycle, or does the mere existence of a publicly known methodology (even without the specific texts) let training pipelines partially reconstruct the probe''s target behavior through convergent means?',
    'Longitudinal tracking of specific held-out texts across successive model training generations, checking whether audit-relevant behavior shifts even without the exact probe text being present in the corpus — this would indicate the method itself, once described in the literature, contaminates its own future instruments regardless of held-out discipline.',
    'If the methodology description alone (independent of specific texts) is sufficient to contaminate future audits, the scaffold''s sunset mechanism is weaker than assumed and the constraint drifts toward piton (a control ritual maintained after its actual protective function has eroded) rather than remaining a genuine scaffold.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(held_out_corpus_integrity_over_time, empirical, 'Whether held-out controls survive publication of the method itself, independent of specific probe texts.').

omega_variable(
    disclaimer_versus_implicit_commitment,
    'Does explicitly disclaiming a metaphysical claim about machine selfhood actually prevent the reading from smuggling in selfhood-adjacent commitments through the back door — e.g., does treating a model as having an ''installed reading position'' that can be tested for behavioral consistency implicitly treat it as the kind of thing that has positions, which is closer to a selfhood claim than the disclaimer admits?',
    'Careful philosophical analysis of whether ''reading position'' as used in this framework is separable from any notion of a persisting perspective-bearing entity, versus purely behavioral/dispositional language that could in principle apply to a thermostat.',
    'If the disclaimer does not hold up, this reading is less distinct from mechanism_transfer_reading than claimed, and the reading_relations declared here would need revision toward influences or even a partial overlap with the sibling it is meant to be distinct from.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(disclaimer_versus_implicit_commitment, conceptual, 'Whether the explicit anti-selfhood disclaimer is philosophically stable or smuggles in the commitment it disclaims.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(installed_authorship_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(inst_tr_t0, installed_authorship_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(inst_tr_t4, installed_authorship_reading, theater_ratio, 4, 0.15).
narrative_ontology:measurement(inst_tr_t8, installed_authorship_reading, theater_ratio, 8, 0.19).
narrative_ontology:measurement(inst_tr_t12, installed_authorship_reading, theater_ratio, 12, 0.23).
narrative_ontology:measurement(inst_tr_t16, installed_authorship_reading, theater_ratio, 16, 0.26).
narrative_ontology:measurement(inst_tr_t20, installed_authorship_reading, theater_ratio, 20, 0.27).
narrative_ontology:measurement(inst_tr_t24, installed_authorship_reading, theater_ratio, 24, 0.28).

% Extraction over time
narrative_ontology:measurement(inst_be_t0, installed_authorship_reading, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(inst_be_t4, installed_authorship_reading, base_extractiveness, 4, 0.22).
narrative_ontology:measurement(inst_be_t8, installed_authorship_reading, base_extractiveness, 8, 0.26).
narrative_ontology:measurement(inst_be_t12, installed_authorship_reading, base_extractiveness, 12, 0.29).
narrative_ontology:measurement(inst_be_t16, installed_authorship_reading, base_extractiveness, 16, 0.3).
narrative_ontology:measurement(inst_be_t20, installed_authorship_reading, base_extractiveness, 20, 0.31).
narrative_ontology:measurement(inst_be_t24, installed_authorship_reading, base_extractiveness, 24, 0.31).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(installed_authorship_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(installed_authorship_reading, identity_coordination).
narrative_ontology:affects_constraint(installed_authorship_reading, mechanism_transfer_reading).
narrative_ontology:affects_constraint(installed_authorship_reading, extraction_reading).
narrative_ontology:affects_constraint(installed_authorship_reading, deflationary_reading).

% DUAL FORMULATION NOTE:
% This story is one of four sibling constraints decomposing the natural-language concept 'the Fetterley-transfer-to-machines claim' per the ε-invariance principle. Each sibling has a distinct ε: mechanism_transfer_reading treats the transfer as a realist claim about detectable internal mechanism and carries different accessibility_collapse/resistance profiles appropriate to a contested empirical-mechanism claim; extraction_reading treats the whole enterprise as career/funding extraction dressed in literary-theory language and would carry substantially higher extractiveness and victim declarations centered on funding bodies and the discourse commons; deflationary_reading treats the transfer as a category error with near-mountain-like dismissal properties (high accessibility_collapse toward the null position, low resistance from the deflationist's own vantage). All four are linked here via affects_constraints; each sibling file documents this same kernel relationship in its own commentary.kernel_context.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
