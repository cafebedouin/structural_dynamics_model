% ============================================================================
% CONSTRAINT STORY: deferential_realism_ontology__immutable_diagnostic_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_deferential_realism_ontology__immutable_diagnostic_reading, []).

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
 *   constraint_id: deferential_realism_ontology__immutable_diagnostic_reading
 *   human_readable: Constraint Typology as Fixed-Referent Observational Instrument (Immutable Diagnostic Reading)
 *   domain: epistemology/normative theory/institutional design
 *
 * SUMMARY:
 *   This story instantiates one reading of the contested kernel
 *   'deferential_realism_ontology': the immutable diagnostic reading, under
 *   which the six-category constraint typology is an observational instrument
 *   with fixed referents — mountains are physical invariants, snares are
 *   measurable extraction mechanisms, and every classification dispute is an
 *   error awaiting better observation. The constraint under classification is
 *   the standing disciplinary arrangement this reading installs inside the
 *   framework: analysts must conduct classification disputes by appealing to
 *   observable metrics; framing-level objections are ruled out of bounds as
 *   instrument misuse; epsilon values are treated as discovered properties of
 *   the classified arrangement rather than constructed by the reading that
 *   assesses it. The arrangement solves a real coordination problem
 *   (commensurable, corrigible classifications across a growing corpus) while
 *   concentrating adjudicating authority in the seat that operates the
 *   metrics — hence the claimed type tangled_rope, authored independently of
 *   the metrics below. The epsilon referent is the standing fixed-referent
 *   discipline itself, assessed by this reading's own lights; the rival
 *   readings are separate constraints (separate files), not folded into this
 *   one. KEY AGENTS (by structural relationship): - typology_operators:
 *   Agenda setter and primary beneficiary (institutional/arbitrage) —
 *   specifies the metrics, rules on what counts as observation, collects
 *   adjudicating authority - corpus_curators: Beneficiary
 *   (institutional/mobile) — selects what enters the corpus, collects citable
 *   settled classifications - downstream_policy_analysts: Beneficiary with
 *   secondary payer position (organized/constrained) — consumes
 *   classifications, blocked from framing-level recourse -
 *   rival_framing_theorists: Primary target (powerful/mobile) — holders of
 *   the sibling readings; their framings are ruled out of bounds -
 *   classified_institutions: Target (institutional/trapped) — bear adverse
 *   labels with no framing-level appeal - normative_philosophers: Excluded
 *   voice (moderate/constrained) — hold that classification is irreducibly
 *   normative; defined out of the conversation - framework_meta_analysts:
 *   Analytical observer (analytical/analytical) — audit reliability and
 *   cross-reading divergence
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(deferential_realism_ontology__immutable_diagnostic_reading, 0.66).
domain_priors:suppression_score(deferential_realism_ontology__immutable_diagnostic_reading, 0.76).
domain_priors:theater_ratio(deferential_realism_ontology__immutable_diagnostic_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(deferential_realism_ontology__immutable_diagnostic_reading, extractiveness, 0.66).
narrative_ontology:constraint_metric(deferential_realism_ontology__immutable_diagnostic_reading, suppression_requirement, 0.76).
narrative_ontology:constraint_metric(deferential_realism_ontology__immutable_diagnostic_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(deferential_realism_ontology__immutable_diagnostic_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(deferential_realism_ontology__immutable_diagnostic_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(deferential_realism_ontology__immutable_diagnostic_reading, tangled_rope).
narrative_ontology:human_readable(deferential_realism_ontology__immutable_diagnostic_reading, "Constraint Typology as Fixed-Referent Observational Instrument (Immutable Diagnostic Reading)").
narrative_ontology:topic_domain(deferential_realism_ontology__immutable_diagnostic_reading, "epistemology/normative theory/institutional design").

domain_priors:requires_active_enforcement(deferential_realism_ontology__immutable_diagnostic_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(deferential_realism_ontology__immutable_diagnostic_reading, '9a6656da-1c69-4a35-9503-6ff35584e976').
narrative_ontology:cs_kernel_codification('9a6656da-1c69-4a35-9503-6ff35584e976', formalized).
narrative_ontology:cs_authority_grounding('9a6656da-1c69-4a35-9503-6ff35584e976', expertise).
narrative_ontology:cs_interpretation_layer_present('9a6656da-1c69-4a35-9503-6ff35584e976').
narrative_ontology:cs_reading_relation('9a6656da-1c69-4a35-9503-6ff35584e976', deferential_realism_ontology__rhetorical_scaffold_reading, forecloses).
narrative_ontology:cs_reading_relation('9a6656da-1c69-4a35-9503-6ff35584e976', deferential_realism_ontology__hybrid_pragmatic_reading, forecloses).
narrative_ontology:cs_axiom('9a6656da-1c69-4a35-9503-6ff35584e976', foundational, fixed_referent_discoverability).
narrative_ontology:cs_axiom_status(fixed_referent_discoverability, holdable).
narrative_ontology:cs_axiom_grounding('9a6656da-1c69-4a35-9503-6ff35584e976', fixed_referent_discoverability, empirically_contingent).
narrative_ontology:cs_axiom('9a6656da-1c69-4a35-9503-6ff35584e976', foundational, misclassification_is_observational_error).
narrative_ontology:cs_axiom_status(misclassification_is_observational_error, holdable).
narrative_ontology:cs_axiom_grounding('9a6656da-1c69-4a35-9503-6ff35584e976', misclassification_is_observational_error, empirically_contingent).
narrative_ontology:cs_axiom('9a6656da-1c69-4a35-9503-6ff35584e976', secondary, metric_adjudication_exclusivity).
narrative_ontology:cs_axiom_status(metric_adjudication_exclusivity, holdable).
narrative_ontology:cs_axiom_grounding('9a6656da-1c69-4a35-9503-6ff35584e976', metric_adjudication_exclusivity, instrumental).
narrative_ontology:cs_reference_frame('9a6656da-1c69-4a35-9503-6ff35584e976', fixed_referent_observational_instrument).
narrative_ontology:cs_drift_state('9a6656da-1c69-4a35-9503-6ff35584e976', contemporary_corpus_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('9a6656da-1c69-4a35-9503-6ff35584e976', '').
narrative_ontology:cs_kernel_id(deferential_realism_ontology__immutable_diagnostic_reading, deferential_realism_ontology).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(deferential_realism_ontology__immutable_diagnostic_reading, typology_operators).
narrative_ontology:constraint_beneficiary(deferential_realism_ontology__immutable_diagnostic_reading, corpus_curators).
narrative_ontology:constraint_beneficiary(deferential_realism_ontology__immutable_diagnostic_reading, downstream_policy_analysts).
narrative_ontology:constraint_victim(deferential_realism_ontology__immutable_diagnostic_reading, rival_framing_theorists).
narrative_ontology:constraint_victim(deferential_realism_ontology__immutable_diagnostic_reading, classified_institutions).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(deferential_realism_ontology__immutable_diagnostic_reading, downstream_policy_analysts).
narrative_ontology:constraint_vindicates(deferential_realism_ontology__immutable_diagnostic_reading, observational_neutrality_doctrine).
narrative_ontology:constraint_vindicates(deferential_realism_ontology__immutable_diagnostic_reading, measurement_grade_objectivity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintain the classification schema, the computing engine, and the validation gates. Decide which observations count, which metrics are tracked, and which stories pass validation. Because the referents are fixed by the reading they administer, their rulings carry measurement-grade authority: a dispute they convert into a measurement question is theirs to settle. They wrote the specification and can re-specify it; what they cannot do without cost is renounce the fixed-referent premise, since their standing rests on it.
narrative_ontology:constraint_stakeholder(deferential_realism_ontology__immutable_diagnostic_reading, typology_operators, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(deferential_realism_ontology__immutable_diagnostic_reading, typology_operators, beneficiary).

% Select which stories enter the corpus and which measurement series are kept current. They receive a steadily growing stock of settled, mutually comparable classifications they can cite and build on. If the framework's credibility collapsed they could shift curation effort to a rival framework, so their position is comfortable but not captive.
narrative_ontology:constraint_stakeholder(deferential_realism_ontology__immutable_diagnostic_reading, corpus_curators, beneficiary,
    institutional, biographical, mobile, global).

% Use the corpus's classifications in reports, hearings, and policy arguments. The fixed referents save them from relitigating taxonomy in every document. When a case needs a framing the fixed referents do not admit, their only in-framework recourse is to request better measurement of the existing referents; reframing is unavailable, so they absorb that cost quietly.
narrative_ontology:constraint_stakeholder(deferential_realism_ontology__immutable_diagnostic_reading, downstream_policy_analysts, beneficiary,
    organized, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(deferential_realism_ontology__immutable_diagnostic_reading, downstream_policy_analysts, payer).

% Hold that category assignments depend on judgments about whose interests a mechanism serves — the position of the sibling readings. Inside this framework their contributions are handled as instrument misuse rather than answered on the merits, and the appellate channel offered to them (more observation) cannot register the objection they raise. They publish in external venues and could migrate to a rival framework wholesale, so they retain exit; what they lose is standing inside this framework's dispute process.
narrative_ontology:constraint_stakeholder(deferential_realism_ontology__immutable_diagnostic_reading, rival_framing_theorists, payer,
    powerful, biographical, mobile, global).

% Organizations whose arrangements have received adverse category labels that travel into procurement screens, litigation, and regulatory attention. To contest a label they must produce better measurements through the administering apparatus; arguing that the label rests on a contested framing is not an available move. There is no exit from having been classified.
narrative_ontology:constraint_stakeholder(deferential_realism_ontology__immutable_diagnostic_reading, classified_institutions, payer,
    institutional, generational, trapped, national).

% Argue in adjacent disciplines that classification is irreducibly normative and that no observational upgrade settles questions of legitimate benefit. They are outside the operational conversation: the reading defines their objection as a category error before they enter, so their critiques circulate externally and the internal dispute process never has to answer them.
narrative_ontology:constraint_stakeholder(deferential_realism_ontology__immutable_diagnostic_reading, normative_philosophers, excluded,
    moderate, biographical, constrained, global).

% Audit the framework itself: inter-analyst reliability studies, cross-reading comparison exercises, provenance checks on measurement series. They see the full structure, including the fact that this story applies the instrument to its own ontological discipline, and they take no side in the dispute.
narrative_ontology:constraint_stakeholder(deferential_realism_ontology__immutable_diagnostic_reading, framework_meta_analysts, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(deferential_realism_ontology__immutable_diagnostic_reading, typology_operators).
narrative_ontology:fixing_cost_class(deferential_realism_ontology__immutable_diagnostic_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Makes classification claims commensurable and corrigible across analysts, time, and a growing corpus: fixed referents mean two analysts disagreeing about an assignment are disagreeing about something checkable, so findings accumulate and errors are correctable instead of merely contested.
% TRANSFER_FUNCTION: Moves adjudicating authority over classification disputes from the disputing parties to the metric apparatus and the seat that operates it, and moves the burden of justification from 'defend your framing' to 'improve your observation.'
% ABSENT_VOICES: Normative theorists and the classified parties themselves would object that the instrument's neutrality is precisely what is in dispute; they are absent because the reading defines their objection as a misuse of the instrument before they speak, so unanimity inside the dispute process reflects the exclusion of the dissenting seats rather than their conversion.
% DISAPPEARANCE_RATIONALE: If the fixed-referent discipline vanished overnight, every contested classification would reopen as a framing dispute, corpus-wide comparability would degrade until a successor convention emerged, and the operator seat's adjudicating authority would evaporate — the rival readings would move from the margins to the center of the framework's practice.
% FOUNDING_PROBLEM: Early classification practice was ad hoc: the same mechanism could be assigned different categories by different analysts with no procedure for making the disagreement checkable, so nothing accumulated and errors could not be distinguished from taste.
% FOUNDING_PROBLEM_CORROBORATION: Rival-reading theorists corroborate the founding problem from outside the benefiting parties: their own frameworks require comparable categories, and published methodology critiques concede the original commensurability problem while denying that fixed referents are the only cure. No corroborating source attests that the problem is dead.
narrative_ontology:disappearance_verdict(deferential_realism_ontology__immutable_diagnostic_reading, world_rearranges).
narrative_ontology:founding_problem_status(deferential_realism_ontology__immutable_diagnostic_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(deferential_realism_ontology__immutable_diagnostic_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(deferential_realism_ontology__immutable_diagnostic_reading, 'none', 1).
narrative_ontology:epsilon_provenance(deferential_realism_ontology__immutable_diagnostic_reading, 0.66, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(deferential_realism_ontology__immutable_diagnostic_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(deferential_realism_ontology__immutable_diagnostic_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(deferential_realism_ontology__immutable_diagnostic_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.66: the discipline transfers adjudicating authority over classification disputes to the seat operating the metrics; every dispute converted from a framing question into a measurement question adds to that seat's standing, and the transfer grows as the corpus becomes load-bearing for downstream policy work. Suppression 0.76: persistence depends on actively ruling alternative framings out of bounds — review norms, validation gates, the 'instrument misuse' ruling — not on participants preferring the arrangement; suppression is authored as a raw structural property and is not scaled by power or scope (only extractiveness is scaled, by directionality and scope, in the engine). Theater_ratio 0.30: the observational function is substantially real — metrics are computed, measurements accumulate — but a rising share of activity is the ritual of answering framing disputes with offers of 'better observation,' which no quantity of observation can settle. Accessibility_collapse 0.40: the sibling readings demonstrably persist as live alternatives, so understanding the instrument does not collapse the alternatives, though it raises their cost of expression inside the framework. Resistance 0.58: sustained contestation from rival-reading theorists and classified parties, visible in methodology critiques and cross-reading comparisons. The three measurement series share one time grid (t=0..24 at step 4); trajectories are monotonic with no cyclical dynamics, and base_properties values equal the interval-end state.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently. From the operator seat the arrangement is the instrument working as designed: disputes become measurements, the corpus accumulates, and objections that resist measurement look like noise. From the rival-theorist seat the same arrangement is a jurisdictional enclosure: their contribution is defined as error before it is heard, and the appellate channel ('better observation') cannot hear the objection they actually raise. From the classified-institution seat the label arrives as a verdict with measurement-grade authority and no framing-level appeal. Operators and rival theorists are both powerful actors at the same nominal level; what differentiates them is structural relationship and exit — operators hold arbitrage over the specification they wrote, while rival theorists hold mobility into external venues that the internal dispute process never has to answer.
 *
 * DIRECTIONALITY LOGIC:
 *   Typology operators are declared beneficiaries and sit near the beneficiary end: the discipline subsidizes their adjudicating authority. Corpus curators and downstream analysts are beneficiaries with progressively weaker claims — curators collect citable settled classifications; analysts collect commensurability but pay indirectly when the fixed-referent rule blocks framings their cases need (hence the secondary payer position). Rival framing theorists and classified institutions are declared targets: the former bear delegitimation of their framings, the latter bear labels they cannot contest at the framing level, and the latter's trapped exit pushes them toward the full-target end. Scope amplification applies modestly: the discipline operates at global scope across the corpus, raising verification difficulty for claims about whose interests the fixed referents serve.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — incommensurable, unchecked classifications — is live, so this is not a mandate outliving its function, and mandatrophy_resolved is left unset. The tangled_rope claim does double preventive work: against the rope mislabeling (which would hide the adjudicating-authority capture behind the genuine commensurability function) and against the snare mislabeling (which would erase the commensurability function that even the rival readings tacitly rely on when they want their own classifications comparable). The dispute-convergence omega is the tripwire: if contested classifications stop closing under added measurement, the coordination half atrophies and the arrangement drifts toward pure enforcement of the operator seat's jurisdiction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'This constraint is one reading of the kernel deferential_realism_ontology: does the fixed-referent premise survive contact with the sibling readings'' claim that normative judgment enters classification?',
    'Adversarial corpus exercise: have all three readings classify the same story set under blinded conditions and measure whether divergence tracks observation quality or framing allegiance.',
    'If divergence tracks framing rather than observation, the foundational axiom fails and this reading collapses toward the hybrid pragmatic reading; if convergence holds, the fixed-referent premise is strengthened and the foreclosure edges harden.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Whether the fixed-referent premise survives the sibling readings'' contest.').

omega_variable(
    epsilon_discoverability,
    'Are epsilon values discoverable properties of the classified arrangement, as this reading holds, or partly constructed by the framing of the assessing story?',
    'Inter-analyst reliability study: multiple analysts authoring stories for the same referent under different framing instructions; systematic variance with framing instruction indicates construction.',
    'Discoverable epsilon supports the reading''s adjudication monopoly; constructed epsilon redistributes classification authority to framing choice and undermines metric-exclusive dispute resolution.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(epsilon_discoverability, empirical, 'Whether epsilon is discovered by observation or constructed by framing.').

omega_variable(
    dispute_convergence_after_saturation,
    'Do contested classifications actually converge under better observation, or do some disputes persist after measurement saturation — indicating irreducible normative disagreement?',
    'Longitudinal tracking of disputed classifications in the corpus: record whether disputes close when additional measurements arrive or remain open at measurement saturation.',
    'Persistent post-saturation disputes falsify the misclassification-is-error axiom and would push this constraint toward the rhetorical scaffold reading''s territory.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(dispute_convergence_after_saturation, empirical, 'Whether classification disputes close under added measurement or persist at saturation.').

omega_variable(
    framing_suppression_mechanism,
    'Is the suppression of alternative framings structural (validation gates, review norms, misuse rulings) or internalized (analysts trained to translate framing questions into measurement questions before asking them)?',
    'Post-exit trajectory: analysts who leave the framework for rival-reading venues — if framing-talk resumes immediately, suppression was structural; if the translation habit persists, it was partly internalized.',
    'Internalized suppression raises effective suppression above the structural measure and makes the discipline self-reproducing without active enforcement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(framing_suppression_mechanism, empirical, 'Structural versus internalized suppression of alternative framings.').

omega_variable(
    self_reference_applicability,
    'Can the typology, read as a fixed-referent instrument, classify its own ontological discipline by its own lights — or is this a referent the instrument cannot observe?',
    'Run the engine''s certification chain on this story and inspect whether the computed per-seat classifications match the reading''s own predictions; divergence indicates a blind spot.',
    'A demonstrated blind spot would show the instrument''s referent set is not closed, weakening the fixed-referent claim from inside the framework.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(self_reference_applicability, conceptual, 'Whether the instrument can observe its own ontological discipline.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(deferential_realism_ontology__immutable_diagnostic_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dr_immutable_diag_tr_t0, deferential_realism_ontology__immutable_diagnostic_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(dr_immutable_diag_tr_t4, deferential_realism_ontology__immutable_diagnostic_reading, theater_ratio, 4, 0.15).
narrative_ontology:measurement(dr_immutable_diag_tr_t8, deferential_realism_ontology__immutable_diagnostic_reading, theater_ratio, 8, 0.19).
narrative_ontology:measurement(dr_immutable_diag_tr_t12, deferential_realism_ontology__immutable_diagnostic_reading, theater_ratio, 12, 0.23).
narrative_ontology:measurement(dr_immutable_diag_tr_t16, deferential_realism_ontology__immutable_diagnostic_reading, theater_ratio, 16, 0.26).
narrative_ontology:measurement(dr_immutable_diag_tr_t20, deferential_realism_ontology__immutable_diagnostic_reading, theater_ratio, 20, 0.28).
narrative_ontology:measurement(dr_immutable_diag_tr_t24, deferential_realism_ontology__immutable_diagnostic_reading, theater_ratio, 24, 0.3).

% Extraction over time
narrative_ontology:measurement(dr_immutable_diag_be_t0, deferential_realism_ontology__immutable_diagnostic_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(dr_immutable_diag_be_t4, deferential_realism_ontology__immutable_diagnostic_reading, base_extractiveness, 4, 0.52).
narrative_ontology:measurement(dr_immutable_diag_be_t8, deferential_realism_ontology__immutable_diagnostic_reading, base_extractiveness, 8, 0.56).
narrative_ontology:measurement(dr_immutable_diag_be_t12, deferential_realism_ontology__immutable_diagnostic_reading, base_extractiveness, 12, 0.6).
narrative_ontology:measurement(dr_immutable_diag_be_t16, deferential_realism_ontology__immutable_diagnostic_reading, base_extractiveness, 16, 0.63).
narrative_ontology:measurement(dr_immutable_diag_be_t20, deferential_realism_ontology__immutable_diagnostic_reading, base_extractiveness, 20, 0.65).
narrative_ontology:measurement(dr_immutable_diag_be_t24, deferential_realism_ontology__immutable_diagnostic_reading, base_extractiveness, 24, 0.66).

% Suppression requirement over time
narrative_ontology:measurement(dr_immutable_diag_su_t0, deferential_realism_ontology__immutable_diagnostic_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(dr_immutable_diag_su_t4, deferential_realism_ontology__immutable_diagnostic_reading, suppression_requirement, 4, 0.6).
narrative_ontology:measurement(dr_immutable_diag_su_t8, deferential_realism_ontology__immutable_diagnostic_reading, suppression_requirement, 8, 0.64).
narrative_ontology:measurement(dr_immutable_diag_su_t12, deferential_realism_ontology__immutable_diagnostic_reading, suppression_requirement, 12, 0.68).
narrative_ontology:measurement(dr_immutable_diag_su_t16, deferential_realism_ontology__immutable_diagnostic_reading, suppression_requirement, 16, 0.71).
narrative_ontology:measurement(dr_immutable_diag_su_t20, deferential_realism_ontology__immutable_diagnostic_reading, suppression_requirement, 20, 0.74).
narrative_ontology:measurement(dr_immutable_diag_su_t24, deferential_realism_ontology__immutable_diagnostic_reading, suppression_requirement, 24, 0.76).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(deferential_realism_ontology__immutable_diagnostic_reading, information_standard).
narrative_ontology:affects_constraint(deferential_realism_ontology__immutable_diagnostic_reading, deferential_realism_ontology__rhetorical_scaffold_reading).
narrative_ontology:affects_constraint(deferential_realism_ontology__immutable_diagnostic_reading, deferential_realism_ontology__hybrid_pragmatic_reading).

% DUAL FORMULATION NOTE:
% Family decomposition of the kernel deferential_realism_ontology per the epsilon-invariance principle: the colloquial label 'the constraint typology' covers three structurally distinct arrangements — this file (immutable diagnostic: fixed referents, metric-exclusive adjudication), rhetorical_scaffold_reading (normative vocabulary, assignments declared), and hybrid_pragmatic_reading (fixed core, contested periphery). Each carries its own epsilon, beneficiaries, and victims. The upstream immutable-diagnostic reading influences the downstream siblings because its classifications are the ones the corpus currently treats as load-bearing; edges here link the family members.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
