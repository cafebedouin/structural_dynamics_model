% ============================================================================
% CONSTRAINT STORY: correct_latin__discontinuity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_correct_latin__discontinuity_reading, []).

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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: correct_latin__discontinuity_reading
 *   human_readable: Classical Latin Discontinuity Reading
 *   domain: historical_linguistics/philology/intellectual_history
 *
 * SUMMARY:
 *   The correct_latin kernel concerns what counts as authoritative Latin. The
 *   discontinuity_reading claims that legitimate Latin is identical to the
 *   Classical form preserved in ancient texts, declares medieval Latin a
 *   corrupt deviation, and grounds correction in external reconstruction from
 *   textual symbols rather than living practice. This constraint coordinates
 *   classical scholars around a single standard but extracts legitimacy from
 *   medieval Latinists and ecclesiastical users by defining their language as
 *   error. As a kernel reading, it is one of three structurally distinct
 *   constraints; the continuity and hybrid readings would produce different
 *   epsilon values and victim sets.
 *
 * KEY AGENTS:
 *   - reconstructive_philologists (agenda_setter/institutional/constrained): administer the standard of correct Latin and collect professional authority from its monopoly
 *   - medieval_latinists (payer/moderate/identity_locked): bear the delegitimization of their object of study and perpetual framing of medieval forms as error
 *   - ecclesiastical_institutions (payer/institutional/constrained): maintain liturgical and canonical traditions that the reading classifies as linguistically corrupt
 *   - classical_text_editors (beneficiary/organized/constrained): produce critical editions justified by the exclusive standard, benefiting from a clear editorial license
 *   - sociolinguistic_observers (observer/analytical/analytical): note that the rupture is an ideological construction and that Latin never ceased being spoken
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(correct_latin__discontinuity_reading, 0.72).
domain_priors:suppression_score(correct_latin__discontinuity_reading, 0.75).
domain_priors:theater_ratio(correct_latin__discontinuity_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(correct_latin__discontinuity_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(correct_latin__discontinuity_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(correct_latin__discontinuity_reading, theater_ratio, 0.55).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(correct_latin__discontinuity_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(correct_latin__discontinuity_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(correct_latin__discontinuity_reading, tangled_rope).
narrative_ontology:human_readable(correct_latin__discontinuity_reading, "Classical Latin Discontinuity Reading").
narrative_ontology:topic_domain(correct_latin__discontinuity_reading, "historical_linguistics/philology/intellectual_history").

domain_priors:requires_active_enforcement(correct_latin__discontinuity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(correct_latin__discontinuity_reading, '28c9b9c8-52fc-47f1-91a7-270e7337dd22').
narrative_ontology:cs_kernel_codification('28c9b9c8-52fc-47f1-91a7-270e7337dd22', fixed_text).
narrative_ontology:cs_authority_grounding('28c9b9c8-52fc-47f1-91a7-270e7337dd22', lineage).
narrative_ontology:cs_interpretation_layer_present('28c9b9c8-52fc-47f1-91a7-270e7337dd22').
narrative_ontology:cs_reading_relation('28c9b9c8-52fc-47f1-91a7-270e7337dd22', correct_latin__continuity_reading, forecloses).
narrative_ontology:cs_reading_relation('28c9b9c8-52fc-47f1-91a7-270e7337dd22', correct_latin__hybrid_reading, forecloses).
narrative_ontology:cs_axiom('28c9b9c8-52fc-47f1-91a7-270e7337dd22', foundational, medieval_corruption_axiom).
narrative_ontology:cs_axiom_status(medieval_corruption_axiom, holdable).
narrative_ontology:cs_axiom_grounding('28c9b9c8-52fc-47f1-91a7-270e7337dd22', medieval_corruption_axiom, empirically_contingent).
narrative_ontology:cs_axiom('28c9b9c8-52fc-47f1-91a7-270e7337dd22', foundational, textual_purity_principle).
narrative_ontology:cs_axiom_status(textual_purity_principle, holdable).
narrative_ontology:cs_axiom_grounding('28c9b9c8-52fc-47f1-91a7-270e7337dd22', textual_purity_principle, conventional).
narrative_ontology:cs_reference_frame('28c9b9c8-52fc-47f1-91a7-270e7337dd22', classical_textual_authority).
narrative_ontology:cs_drift_state('28c9b9c8-52fc-47f1-91a7-270e7337dd22', contemporary_linguistics_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('28c9b9c8-52fc-47f1-91a7-270e7337dd22', '').
narrative_ontology:cs_kernel_id(correct_latin__discontinuity_reading, correct_latin).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(correct_latin__discontinuity_reading, reconstructive_philologists).
narrative_ontology:constraint_beneficiary(correct_latin__discontinuity_reading, classical_text_editors).
narrative_ontology:constraint_victim(correct_latin__discontinuity_reading, medieval_latinists).
narrative_ontology:constraint_victim(correct_latin__discontinuity_reading, ecclesiastical_institutions).
narrative_ontology:constraint_vindicates(correct_latin__discontinuity_reading, classical_supremacy_doctrine).
narrative_ontology:constraint_vindicates(correct_latin__discontinuity_reading, textual_reconstruction_methodology).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administer the standards of correct Latin through textual criticism, emendation, and editorial practice. Their professional authority, career advancement, and institutional funding depend on the exclusive legitimacy of the Classical form and the delegitimization of medieval deviation.
narrative_ontology:constraint_stakeholder(correct_latin__discontinuity_reading, reconstructive_philologists, agenda_setter,
    institutional, generational, constrained, continental).

% Study and teach medieval Latin forms, which the discontinuity reading systematically classifies as corrupt deviations from the Classical standard. Their research is perpetually framed as the study of error or decay, requiring defensive justification, and their scholarly identity is constituted through the very forms the constraint labels illegitimate.
narrative_ontology:constraint_stakeholder(correct_latin__discontinuity_reading, medieval_latinists, payer,
    moderate, biographical, identity_locked, continental).

% Maintain liturgical, legal, and administrative traditions in medieval and early modern Latin. The discontinuity reading pressures these institutions to treat their own linguistic heritage as defective and to submit to reconstructive philological authority, with reform toward Classical Latin being costly and identity-altering.
narrative_ontology:constraint_stakeholder(correct_latin__discontinuity_reading, ecclesiastical_institutions, payer,
    institutional, civilizational, constrained, global).

% Produce critical editions based on the assumption that the Classical form is the only correct Latin and that medieval manuscripts are corrupt witnesses. They benefit from a clear standard that justifies editorial intervention and professional standing.
narrative_ontology:constraint_stakeholder(correct_latin__discontinuity_reading, classical_text_editors, beneficiary,
    organized, biographical, constrained, continental).

% Observe that Latin never ceased being spoken and that the rupture between Classical and medieval forms is a post-hoc ideological construction rather than a sociolinguistic reality. They note the constraint serves the authority of the reconstructors at the expense of historical continuity.
narrative_ontology:constraint_stakeholder(correct_latin__discontinuity_reading, sociolinguistic_observers, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(correct_latin__discontinuity_reading, reconstructive_philologists).
narrative_ontology:fixing_cost_class(correct_latin__discontinuity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a stable, uniform standard for reading, editing, and emending ancient Latin texts across generations of scholars, solving the problem of how to establish consistent readings when manuscripts disagree.
% TRANSFER_FUNCTION: Moves authority over Latin correctness from medieval practitioners and living tradition to the corpus of ancient texts and the philologists who reconstruct them; simultaneously moves scholarly status and institutional resources from medievalists to classical reconstructors.
% ABSENT_VOICES: Vernacular and non-literary Latin users of the medieval period; speakers of Romance vernaculars whose linguistic continuity with Latin is denied by the rupture narrative; medieval copyists regarded as incompetent witnesses rather than legitimate language users.
% DISAPPEARANCE_RATIONALE: If the discontinuity reading vanished, classical philology would lose its foundational distinction between pure and corrupt Latin, forcing a methodological rearrangement; however, textual editing would still require some standard, so the world would rearrange around an alternative standard rather than remain unchanged.
% FOUNDING_PROBLEM: The apparent morphological, syntactic, and lexical divergence of post-classical texts from ancient models created uncertainty about how to establish correct readings in classical authors.
% FOUNDING_PROBLEM_CORROBORATION: Classical philologists attest the problem is live, citing manuscript corruption. Sociolinguists and medievalists attest the divergence was natural evolution and the 'problem' was manufactured by humanist ideology; corroboration from outside the beneficiary set supports the dead/contested reading.
narrative_ontology:disappearance_verdict(correct_latin__discontinuity_reading, contested).
narrative_ontology:founding_problem_status(correct_latin__discontinuity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(correct_latin__discontinuity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(correct_latin__discontinuity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(correct_latin__discontinuity_reading, 0.72, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(correct_latin__discontinuity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(correct_latin__discontinuity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(correct_latin__discontinuity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises from 0.25 to 0.72 over the interval as philology professionalizes and the discontinuity narrative hardens into disciplinary gatekeeping. Suppression is high (0.75) because the constraint persists only through active enforcement: peer review that rejects medievalist frameworks, hiring criteria that privilege classical reconstruction, and editorial standards that treat post-classical forms as corruption. Theater_ratio climbs to 0.55 because an increasing share of philological activity is performance of purity rather than substantive textual improvement. Accessibility_collapse is high (0.70) because once the discontinuity framework is accepted, medieval Latin alternatives become cognitively invisible as legitimate options. Resistance is moderate (0.60) because medievalists and sociolinguists have contested the narrative since the late twentieth century, but institutional authority remains with the reconstructors.
 *
 * PERSPECTIVAL GAP:
 *   The agenda_setter and beneficiary seats experience the constraint as necessary coordination: without a stable Classical standard, textual criticism would fragment into arbitrary subjectivity. The payer seats experience it as enforced extraction: their language is declared corrupt, their research framed as error studies, and their identity fused to an object the constraint systematically devalues. The engine computes this divergence from the same structural data; the authored claim (tangled_rope) does not adjudicate the divergence but records that both coordination and extraction are structurally present.
 *
 * DIRECTIONALITY LOGIC:
 *   Reconstructive philologists and classical text editors are beneficiaries with constrained or organized exit; the engine derives low directionality for these seats, dampening effective extraction into subsidy or mild coordination cost. Medieval latinists are victims with identity_locked exit, pushing directionality toward the full-target end and amplifying effective extraction. Ecclesiastical institutions are victims with constrained exit at institutional power and global scope, yielding high directionality and significant extraction, though slightly less than identity_locked individuals. The sociolinguistic observer seat is analytical and neither pays nor benefits.
 *
 * MANDATROPHY ANALYSIS:
 *   Without the mandatrophy/resistance axes, this constraint could be misread as a rope if one only looked at the genuine coordination around textual standards, or as a snare if one only looked at the delegitimization of medieval Latin. The temporal measurements show extraction accumulating over centuries while the founding problem (uncertainty in classical readings) became increasingly solvable without the discontinuity premise, pointing toward coordination-to-extraction drift rather than pure extraction from inception. The theater_ratio above 0.5 signals that maintenance has become partly performative, but the coordinated editorial function remains real enough to prevent a pure snare classification.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_location,
    'This constraint is the discontinuity_reading of kernel correct_latin; sibling readings (continuity_reading, hybrid_reading) instantiate different constraints with different structural profiles. What would change if a sibling reading were substituted?',
    'Compare epsilon, beneficiary/victim structures, and directionality profiles across the constraint family.',
    'continuity_reading would empty the victim set and reduce epsilon toward rope; hybrid_reading would moderate epsilon and convert full victims to partial payers.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_location, conceptual, 'Structural delta between sibling readings of the correct_latin kernel.').

omega_variable(
    rupture_empirical_status,
    'Is the rupture between Classical and medieval Latin an empirical linguistic fact or an ideological construction of the humanist and nineteenth-century philological tradition?',
    'Comparative sociolinguistic analysis of documentary evidence across the 1st-8th centuries CE to determine register continuity versus elite literary rupture.',
    'If continuity is established, the constraint''s foundational premise is falsified and its authority collapses to mere institutional preference; if rupture is real, the constraint retains empirical grounding.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(rupture_empirical_status, empirical, 'Whether the classical-medieval rupture is empirical or ideological.').

omega_variable(
    medieval_legitimacy_exclusion,
    'Does the exclusion of medieval Latin forms from the legitimate usage set serve a necessary coordination function for classical textual criticism, or is it superfluous to the editorial task?',
    'Comparative analysis of editorial methodologies: can classical texts be emended without delegitimizing medieval Latin as a whole?',
    'If delegitimization is unnecessary for the coordination function, the measured extraction is pure overhead; if necessary, part of the extraction is the cost of the coordination itself.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(medieval_legitimacy_exclusion, conceptual, 'Whether coordination and extraction are structurally separable in this reading.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression of medieval Latin alternatives structural (enforced by editorial boards, hiring committees, and funding agencies) or internalized (scholars have absorbed the corruption narrative as self-evident)?',
    'Track whether medieval Latinists challenge the narrative in print; if they self-censor or frame their work apologetically, suppression is internalized.',
    'If internalized, effective suppression exceeds the structural measure; the constraint operates even where no explicit enforcement is visible.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs internalized suppression mechanism in philological ideology.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(correct_latin__discontinuity_reading, 0, 500).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cl_discontinuity_tr_t0, correct_latin__discontinuity_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(cl_discontinuity_tr_t100, correct_latin__discontinuity_reading, theater_ratio, 100, 0.25).
narrative_ontology:measurement(cl_discontinuity_tr_t200, correct_latin__discontinuity_reading, theater_ratio, 200, 0.32).
narrative_ontology:measurement(cl_discontinuity_tr_t300, correct_latin__discontinuity_reading, theater_ratio, 300, 0.42).
narrative_ontology:measurement(cl_discontinuity_tr_t400, correct_latin__discontinuity_reading, theater_ratio, 400, 0.52).
narrative_ontology:measurement(cl_discontinuity_tr_t500, correct_latin__discontinuity_reading, theater_ratio, 500, 0.55).

% Extraction over time
narrative_ontology:measurement(cl_discontinuity_be_t0, correct_latin__discontinuity_reading, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(cl_discontinuity_be_t100, correct_latin__discontinuity_reading, base_extractiveness, 100, 0.38).
narrative_ontology:measurement(cl_discontinuity_be_t200, correct_latin__discontinuity_reading, base_extractiveness, 200, 0.48).
narrative_ontology:measurement(cl_discontinuity_be_t300, correct_latin__discontinuity_reading, base_extractiveness, 300, 0.58).
narrative_ontology:measurement(cl_discontinuity_be_t400, correct_latin__discontinuity_reading, base_extractiveness, 400, 0.68).
narrative_ontology:measurement(cl_discontinuity_be_t500, correct_latin__discontinuity_reading, base_extractiveness, 500, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(cl_discontinuity_su_t0, correct_latin__discontinuity_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(cl_discontinuity_su_t100, correct_latin__discontinuity_reading, suppression_requirement, 100, 0.42).
narrative_ontology:measurement(cl_discontinuity_su_t200, correct_latin__discontinuity_reading, suppression_requirement, 200, 0.52).
narrative_ontology:measurement(cl_discontinuity_su_t300, correct_latin__discontinuity_reading, suppression_requirement, 300, 0.62).
narrative_ontology:measurement(cl_discontinuity_su_t400, correct_latin__discontinuity_reading, suppression_requirement, 400, 0.72).
narrative_ontology:measurement(cl_discontinuity_su_t500, correct_latin__discontinuity_reading, suppression_requirement, 500, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(correct_latin__discontinuity_reading, identity_coordination).

% DUAL FORMULATION NOTE:
% The correct_latin kernel decomposes into three structurally distinct constraints: continuity_reading (rope-like, low extraction, no victims), discontinuity_reading (tangled_rope, high extraction, medievalists as victims), and hybrid_reading (intermediate epsilon, partial victims). Each reading has a stable epsilon and its own stakeholder structure; they are linked as a constraint family but are not the same constraint viewed from different angles.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
