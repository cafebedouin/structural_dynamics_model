% ============================================================================
% CONSTRAINT STORY: literacy_acquisition_kernel__phonics_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_literacy_acquisition_kernel__phonics_reading, []).

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
 *   constraint_id: literacy_acquisition_kernel__phonics_reading
 *   human_readable: Phonics-First Reading Instruction Mandate (Systematic Phoneme-Grapheme Correspondence Before Connected Text)
 *   domain: educational_psychology/literacy_pedagogy/cognitive_science
 *
 * SUMMARY:
 *   This story instantiates the phonics reading of the
 *   literacy_acquisition_kernel: reading acquisition requires explicit,
 *   systematic instruction in phoneme-grapheme correspondence before
 *   connected text exposure, with decoding treated as the precondition for
 *   comprehension rather than something that co-develops with it. The genuine
 *   coordination function is real and well-evidenced: children with weak
 *   phonological awareness benefit substantially from explicit sequencing
 *   they cannot self-construct. The extraction is layered on top through the
 *   enforcement mechanism states have adopted to implement the reading —
 *   scripted, fidelity-monitored curricula that convert teacher professional
 *   judgment into a compliance surface and lock all students, including those
 *   who have already decoded fluently, into a single mandated pace. As a
 *   kernel reading, this story's ε describes only the phonics-first
 *   arrangement as it is actually enforced, not the whole_language,
 *   balanced_literacy, or structured_literacy siblings, which are separate
 *   constraints with their own ε and are not represented here.
 *
 * KEY AGENTS:
 *   - students_with_weak_phonological_awareness: primary beneficiary (powerless/trapped) — gains a decoding scaffold they cannot construct alone
 *   - curriculum_publishers_of_scripted_phonics_programs: beneficiary and co-agenda-setter (organized/arbitrage) — captures procurement revenue from mandate adoption
 *   - classroom_teachers_professional_judgment: primary payer (moderate/constrained) — professional discretion converted into fidelity-monitored compliance
 *   - advanced_early_readers_held_to_pace: secondary payer (powerless/trapped) — held to a uniform pace that no longer serves them
 *   - state_boards_of_education: agenda_setter (institutional/analytical) — enacts and enforces the mandate in response to a real crisis
 *   - reading_researchers_science_of_reading_coalition: observer with beneficiary interest (organized/analytical) — advocacy and career capital tied to codification
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(literacy_acquisition_kernel__phonics_reading, 0.52).
domain_priors:suppression_score(literacy_acquisition_kernel__phonics_reading, 0.58).
domain_priors:theater_ratio(literacy_acquisition_kernel__phonics_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(literacy_acquisition_kernel__phonics_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(literacy_acquisition_kernel__phonics_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(literacy_acquisition_kernel__phonics_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(literacy_acquisition_kernel__phonics_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(literacy_acquisition_kernel__phonics_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(literacy_acquisition_kernel__phonics_reading, tangled_rope).
narrative_ontology:human_readable(literacy_acquisition_kernel__phonics_reading, "Phonics-First Reading Instruction Mandate (Systematic Phoneme-Grapheme Correspondence Before Connected Text)").
narrative_ontology:topic_domain(literacy_acquisition_kernel__phonics_reading, "educational_psychology/literacy_pedagogy/cognitive_science").

domain_priors:requires_active_enforcement(literacy_acquisition_kernel__phonics_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(literacy_acquisition_kernel__phonics_reading, 'b198f42b-71a2-4bc8-add6-9d68556d69eb').
narrative_ontology:cs_kernel_codification('b198f42b-71a2-4bc8-add6-9d68556d69eb', distributed).
narrative_ontology:cs_authority_grounding('b198f42b-71a2-4bc8-add6-9d68556d69eb', expertise).
narrative_ontology:cs_interpretation_layer_present('b198f42b-71a2-4bc8-add6-9d68556d69eb').
narrative_ontology:cs_reading_relation('b198f42b-71a2-4bc8-add6-9d68556d69eb', literacy_acquisition_kernel__whole_language_reading, forecloses).
narrative_ontology:cs_reading_relation('b198f42b-71a2-4bc8-add6-9d68556d69eb', literacy_acquisition_kernel__balanced_literacy_reading, influences).
narrative_ontology:cs_reading_relation('b198f42b-71a2-4bc8-add6-9d68556d69eb', literacy_acquisition_kernel__structured_literacy_reading, coexists_with).
narrative_ontology:cs_axiom('b198f42b-71a2-4bc8-add6-9d68556d69eb', foundational, decoding_precedes_and_enables_comprehension).
narrative_ontology:cs_axiom_status(decoding_precedes_and_enables_comprehension, holdable).
narrative_ontology:cs_axiom_grounding('b198f42b-71a2-4bc8-add6-9d68556d69eb', decoding_precedes_and_enables_comprehension, empirically_contingent).
narrative_ontology:cs_axiom('b198f42b-71a2-4bc8-add6-9d68556d69eb', foundational, explicit_instruction_required_before_connected_text).
narrative_ontology:cs_axiom_status(explicit_instruction_required_before_connected_text, holdable).
narrative_ontology:cs_axiom_grounding('b198f42b-71a2-4bc8-add6-9d68556d69eb', explicit_instruction_required_before_connected_text, empirically_contingent).
narrative_ontology:cs_reference_frame('b198f42b-71a2-4bc8-add6-9d68556d69eb', pre_ngram_pedagogical_pluralism).
narrative_ontology:cs_drift_state('b198f42b-71a2-4bc8-add6-9d68556d69eb', post_science_of_reading_legislative_wave, gap(authority_erosion, substantial, true)).
narrative_ontology:cs_created_at('b198f42b-71a2-4bc8-add6-9d68556d69eb', '').
narrative_ontology:cs_kernel_id(literacy_acquisition_kernel__phonics_reading, literacy_acquisition_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(literacy_acquisition_kernel__phonics_reading, students_with_weak_phonological_awareness).
narrative_ontology:constraint_beneficiary(literacy_acquisition_kernel__phonics_reading, curriculum_publishers_of_scripted_phonics_programs).
narrative_ontology:constraint_victim(literacy_acquisition_kernel__phonics_reading, classroom_teachers_professional_judgment).
narrative_ontology:constraint_victim(literacy_acquisition_kernel__phonics_reading, advanced_early_readers_held_to_pace).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(literacy_acquisition_kernel__phonics_reading, reading_researchers_science_of_reading_coalition).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Children who would struggle to induce sound-symbol patterns from exposure alone. Systematic phoneme-grapheme instruction gives them an explicit map they cannot construct independently; without it their decoding failure compounds into broader reading failure. They have no say in curriculum choice and depend entirely on what their assigned classroom delivers.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__phonics_reading, students_with_weak_phonological_awareness, beneficiary,
    powerless, biographical, trapped, local).

% Sell scripted, sequenced phonics curricula and the training, assessment, and fidelity-monitoring packages that accompany them to districts under "science of reading" mandates. Revenue scales with adoption breadth and with the requirement that teachers follow the script rather than adapt it. They lobby state legislatures and boards of education to codify phonics-first sequencing into law and procurement rules.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__phonics_reading, curriculum_publishers_of_scripted_phonics_programs, beneficiary,
    organized, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(literacy_acquisition_kernel__phonics_reading, curriculum_publishers_of_scripted_phonics_programs, agenda_setter).

% Trained across a range of pedagogical traditions and accustomed to reading formative evidence about individual children — a student's motivation, background knowledge, or context-use strategies — to differentiate instruction. Under scripted phonics mandates they must deliver lessons in fixed sequence and wording, with fidelity monitoring and walkthroughs checking compliance. Deviating, even for a child who would benefit from a different approach, risks negative evaluation. Exit means leaving the district or the profession; within it, discretion is the thing being extracted.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__phonics_reading, classroom_teachers_professional_judgment, payer,
    moderate, biographical, constrained, regional).

% Children who arrive already decoding fluently or who acquire pattern recognition faster than the systematic sequence assumes. Locked into the same scope-and-sequence pacing as peers who need the explicit scaffold, they sit through instruction on correspondences they have already mastered, with connected, meaningful text exposure delayed by the same enforcement logic that helps their struggling peers. They have no mechanism to accelerate out of the sequence.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__phonics_reading, advanced_early_readers_held_to_pace, payer,
    powerless, biographical, trapped, local).

% Enact "science of reading" legislation mandating phonics-first, systematic sequencing, and often the specific curricula and screening instruments used to enforce it. Respond to a genuine, well-documented decoding-failure crisis and to advocacy from dyslexia parent organizations, but their enforcement mechanisms (procurement rules, mandated screeners, teacher licensure requirements) also determine which publishers profit and which teacher practices become non-compliant.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__phonics_reading, state_boards_of_education, agenda_setter,
    institutional, generational, analytical, national).

% Cite converging evidence (simple view of reading, meta-analyses of phonics effect sizes, dyslexia neuroimaging) to argue decoding instruction must precede and enable comprehension. Their research career capital, public advocacy platforms, and expert-witness/consulting relationships with publishers and state boards are strengthened by the mandate's adoption; they are not disinterested in how strongly the reading is codified into law.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__phonics_reading, reading_researchers_science_of_reading_coalition, observer,
    organized, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(literacy_acquisition_kernel__phonics_reading, reading_researchers_science_of_reading_coalition, beneficiary).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(literacy_acquisition_kernel__phonics_reading, curriculum_publishers_of_scripted_phonics_programs).
narrative_ontology:fixing_cost_class(literacy_acquisition_kernel__phonics_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves a real, measurable problem: a substantial fraction of children do not induce phoneme-grapheme correspondence from context alone and fail to become independent decoders without explicit, systematic instruction — early, universal phonics sequencing catches those children before the failure compounds into a persistent reading gap.
% TRANSFER_FUNCTION: Moves instructional discretion from individual classroom teachers to curriculum publishers and the state agencies that mandate their scripted sequences; moves procurement dollars from districts to phonics-program publishers; moves instructional time from open-ended text engagement to scripted correspondence drills, applied uniformly regardless of an individual child's decoding status.
% ABSENT_VOICES: Advanced early readers who could benefit from earlier, richer text exposure have no voice in pacing decisions. Teachers with strong track records differentiating instruction are structurally excluded from exercising that judgment once fidelity monitoring is in place — their dissent registers as a compliance problem, not as data.
% DISAPPEARANCE_RATIONALE: Publishers and state boards would say the world rearranges catastrophically — decoding failure rates would climb without the mandate's enforcement. Teachers with strong pre-mandate track records and researchers who favor balanced approaches would say the reading itself, not literacy instruction generally, would disappear — differentiated instruction incorporating explicit phonics where needed would persist without the scripted, uniform sequencing and its enforcement apparatus.
% FOUNDING_PROBLEM: A well-documented cohort of children — including but not limited to those with dyslexia — were failing to become decoders under whole-language and unstructured balanced-literacy approaches that assumed phonics knowledge would emerge from exposure; national and state reading proficiency scores stagnated or declined, and gaps by disability status and income widened.
% FOUNDING_PROBLEM_CORROBORATION: Cognitive scientists studying reading acquisition (outside the phonics-publishing and phonics-advocacy financial network) corroborate that systematic phonics instruction measurably reduces decoding failure for at-risk readers — this part of the founding problem is live and well-evidenced. Independent literacy researchers and some special-education advocates dispute that the problem requires scripted, fidelity-monitored, one-size-sequencing as opposed to explicit-but-differentiated phonics instruction delivered with teacher judgment intact; they attest the founding decoding-failure problem is real but that the specific enforcement mechanism addresses a different, later-arriving problem (curriculum market consolidation and compliance monitoring) that has attached itself to the original one.
narrative_ontology:disappearance_verdict(literacy_acquisition_kernel__phonics_reading, contested).
narrative_ontology:founding_problem_status(literacy_acquisition_kernel__phonics_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(literacy_acquisition_kernel__phonics_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(literacy_acquisition_kernel__phonics_reading, 'none', 1).
narrative_ontology:epsilon_provenance(literacy_acquisition_kernel__phonics_reading, 0.52, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(literacy_acquisition_kernel__phonics_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(literacy_acquisition_kernel__phonics_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(literacy_acquisition_kernel__phonics_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction (0.52) is moderate rather than extreme because the coordination function is genuinely strong — the reading correctly identifies that decoding failure is a real, remediable problem, and systematic instruction measurably reduces it for the target population. What raises ε above a pure-rope reading is the enforcement layer: scripted fidelity monitoring extracts from teacher discretion and imposes a uniform pace cost on students who do not need the intervention. Suppression (0.58) reflects legislative mandates, procurement lock-in, and evaluation consequences for non-compliant teachers — these are structural, not merely normative pressure. Theater ratio (0.28) is present but moderate: fidelity walkthroughs and screener compliance reporting have grown over the interval as the mandate matured from crisis response into routine administrative apparatus, a share of which functions as demonstrated compliance rather than instructional improvement.
 *
 * DIRECTIONALITY LOGIC:
 *   Students with weak phonological awareness sit near the beneficiary end: the constraint's core function subsidizes exactly the population it targets, and their trapped exit options do not indicate extraction because the direction of the transfer runs toward them. Curriculum publishers are a clear beneficiary with organized, arbitrage-level exit — they can move between markets and legislative cycles that continue mandating their products. Teachers are the clearest target: their exit options are constrained (leaving the district or profession), and the constraint extracts their professional judgment directly, converting it into a compliance metric. Advanced early readers are also targets despite being highly capable, because the constraint's uniform pacing does not track their actual decoding status — trapped exit plus no benefit from the specific mechanism (universal pacing) places them near the target end even though they are not the population the mandate exists to protect.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as tangled_rope (rather than snare or mountain) is deliberate: collapsing this into a pure snare would erase the genuine, well-evidenced coordination function for at-risk decoders, mislabeling a real pedagogical advance as pure extraction. Collapsing it into a mountain or pure rope would erase the documented cost imposed on teacher judgment and on students who do not need the intervention. The founding problem (decoding failure) remains partly live — corroborated by cognitive science outside the publishing network — while the enforcement mechanism has drifted to also serve curriculum-market consolidation, which is the founding_problem_status: contested finding. Mandatrophy is not yet resolved: the coordination function has not been fully separated from the extractive enforcement layer riding on it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sequencing_necessity_vs_scripting_necessity,
    'Is the extraction from teacher professional judgment intrinsic to phonics-first sequencing itself, or is it an artifact of the specific enforcement mechanism (scripted fidelity monitoring) that states chose to implement the reading?',
    'Compare districts that mandate phonics-first sequencing outcomes without prescribing script fidelity or vendor curricula against districts that mandate both sequencing and fidelity monitoring; if decoding outcomes are comparable but teacher retention and reported autonomy differ, the extraction is attributable to the enforcement choice, not the pedagogical claim.',
    'If extraction is separable from the pedagogical claim, this reading could in principle be re-authored at lower ε (coordination without the compliance apparatus); if inseparable, the tangled_rope classification is a stable, not contingent, feature of implementing this reading at scale.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sequencing_necessity_vs_scripting_necessity, empirical, 'Whether teacher-autonomy extraction is intrinsic to phonics-first sequencing or an artifact of chosen enforcement mechanisms.').

omega_variable(
    publisher_influence_on_evidence_interpretation,
    'To what extent has the research base supporting universal phonics-first mandates been shaped by researchers and organizations with financial or reputational stakes in curriculum adoption, versus representing disinterested cognitive-science consensus?',
    'Audit funding sources and consulting relationships of researchers whose work is most frequently cited in state legislative mandates; compare effect sizes and policy recommendations from funded versus unfunded research streams.',
    'If a substantial share of the evidentiary case rests on interested parties, the founding_problem''s corroboration is weaker than it appears and the reading''s beneficiary structure (publishers, advocacy coalition) may be doing more work in sustaining the mandate than the underlying science warrants.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(publisher_influence_on_evidence_interpretation, conceptual, 'Whether the phonics-mandate evidence base is contaminated by publisher and advocacy financial interest.').

omega_variable(
    kernel_framing_alternative_the_screening_layer,
    'Is the more consequential framing of this constraint the phonics-first pedagogical claim itself, or the screening/accountability layer above it (universal dyslexia screeners, mandated reporting, teacher licensure testing) that determines which pedagogical practices survive regardless of the underlying claim''s merit?',
    'Trace which specific legal/administrative mechanism (the pedagogical mandate vs. the screener-and-licensure apparatus) most directly determines teacher compliance behavior and publisher revenue; if the screening/licensure layer is doing the enforcement work regardless of which pedagogical reading it nominally serves, that layer may be the more structurally load-bearing constraint.',
    'If the screening/licensure layer is the load-bearing constraint, this story''s cs_structure and stakeholder analysis would need to be split into a separate constraint (the accountability apparatus) linked via network.affects_constraints, rather than folded into the pedagogical reading — this is a candidate ε-invariance decomposition not yet executed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_framing_alternative_the_screening_layer, conceptual, 'Whether the pedagogical claim or its accountability/screening apparatus is the more structurally decisive constraint — a candidate for further decomposition.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(literacy_acquisition_kernel__phonics_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(lite_tr_t0, literacy_acquisition_kernel__phonics_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(lite_tr_t4, literacy_acquisition_kernel__phonics_reading, theater_ratio, 4, 0.14).
narrative_ontology:measurement(lite_tr_t8, literacy_acquisition_kernel__phonics_reading, theater_ratio, 8, 0.19).
narrative_ontology:measurement(lite_tr_t12, literacy_acquisition_kernel__phonics_reading, theater_ratio, 12, 0.22).
narrative_ontology:measurement(lite_tr_t16, literacy_acquisition_kernel__phonics_reading, theater_ratio, 16, 0.25).
narrative_ontology:measurement(lite_tr_t20, literacy_acquisition_kernel__phonics_reading, theater_ratio, 20, 0.28).

% Extraction over time
narrative_ontology:measurement(lite_be_t0, literacy_acquisition_kernel__phonics_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(lite_be_t4, literacy_acquisition_kernel__phonics_reading, base_extractiveness, 4, 0.36).
narrative_ontology:measurement(lite_be_t8, literacy_acquisition_kernel__phonics_reading, base_extractiveness, 8, 0.42).
narrative_ontology:measurement(lite_be_t12, literacy_acquisition_kernel__phonics_reading, base_extractiveness, 12, 0.46).
narrative_ontology:measurement(lite_be_t16, literacy_acquisition_kernel__phonics_reading, base_extractiveness, 16, 0.49).
narrative_ontology:measurement(lite_be_t20, literacy_acquisition_kernel__phonics_reading, base_extractiveness, 20, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(lite_su_t0, literacy_acquisition_kernel__phonics_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(lite_su_t4, literacy_acquisition_kernel__phonics_reading, suppression_requirement, 4, 0.38).
narrative_ontology:measurement(lite_su_t8, literacy_acquisition_kernel__phonics_reading, suppression_requirement, 8, 0.46).
narrative_ontology:measurement(lite_su_t12, literacy_acquisition_kernel__phonics_reading, suppression_requirement, 12, 0.52).
narrative_ontology:measurement(lite_su_t16, literacy_acquisition_kernel__phonics_reading, suppression_requirement, 16, 0.56).
narrative_ontology:measurement(lite_su_t20, literacy_acquisition_kernel__phonics_reading, suppression_requirement, 20, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(literacy_acquisition_kernel__phonics_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(literacy_acquisition_kernel__phonics_reading, whole_language_reading).
narrative_ontology:affects_constraint(literacy_acquisition_kernel__phonics_reading, balanced_literacy_reading).
narrative_ontology:affects_constraint(literacy_acquisition_kernel__phonics_reading, structured_literacy_reading).

% DUAL FORMULATION NOTE:
% This story is one of four sibling readings of literacy_acquisition_kernel, decomposed per the ε-invariance principle because the natural-language label 'the science of reading debate' conflates structurally distinct pedagogical claims with different beneficiary/victim structures and different ε. phonics_reading claims decoding must precede connected text and treats it as the singular precondition; structured_literacy_reading shares the sequencing claim but embeds it in a broader multi-component, dyslexia-derived framework with different enforcement provenance (Orton-Gillingham clinical tradition rather than state legislative mandate); balanced_literacy_reading denies the strict precedence claim, treating phonics and meaningful engagement as simultaneous; whole_language_reading denies explicit phonics instruction is necessary at all, inverting the beneficiary/victim structure (teacher autonomy becomes a beneficiary, weak decoders become the victim group). Each sibling should be authored as its own file with its own ε and stakeholder set.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
