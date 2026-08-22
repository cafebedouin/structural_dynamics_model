% ============================================================================
% CONSTRAINT STORY: literacy_acquisition_kernel__phonics_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
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
 *   human_readable: Phonics-First Mandate: Systematic Decoding Instruction Before Connected Text
 *   domain: educational_psychology/literacy_pedagogy/cognitive_science
 *
 * SUMMARY:
 *   This story instantiates one reading of the literacy-acquisition kernel:
 *   the phonics_reading, under which reading acquisition requires explicit,
 *   systematic instruction in phoneme-grapheme correspondence before
 *   connected text exposure, with decoding preceding and enabling
 *   comprehension. The standing arrangement under contest is the
 *   institutionalized version of that claim: state statutes and board
 *   criteria defining acceptable early-reading instruction, adoption lists
 *   certifying only systematically aligned programs, mandated teacher
 *   retraining, and fidelity monitoring of lesson delivery. Sibling readings
 *   of the same kernel (whole_language_reading, balanced_literacy_reading,
 *   structured_literacy_reading) are separate constraints authored
 *   separately; their epsilon values and victim sets differ, and none of
 *   their content is averaged into this file. Within this reading's own
 *   lights, the arrangement's student-facing operation is a genuine,
 *   evidence-backed coordination achievement — children who cannot yet
 *   segment speech sounds get the full code taught to them in order — while
 *   its teacher-facing operation displaces professional judgment into
 *   scripted delivery, and its procurement operation concentrates licensing
 *   revenue in adopting vendors. KEY AGENTS (by structural relationship): -
 *   students_with_weak_phonological_awareness: Primary beneficiary
 *   (powerless/trapped) — receives the skill the arrangement teaches -
 *   typical_developing_readers: Secondary beneficiary with opportunity-cost
 *   burden (powerless/trapped) - classroom_teachers: Primary target
 *   (organized/constrained) — bears displacement of professional judgment -
 *   curriculum_publishers: Concentrated collector (powerful/arbitrage) —
 *   converts mandates into licensing contracts - state_education_authorities:
 *   Agenda setter (institutional/mobile) — writes and enforces the criteria -
 *   parents_of_struggling_readers: Mobilized beneficiary-advocates
 *   (organized/constrained) - balanced_literacy_trainer_networks: Excluded
 *   rival (organized/trapped) — locked out of adoption -
 *   education_school_faculties: Institutional payer
 *   (institutional/constrained) — syllabi rewritten to statute -
 *   reading_scientists: Analytical observer (analytical/analytical) — sees
 *   the full structure
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(literacy_acquisition_kernel__phonics_reading, 0.58).
domain_priors:suppression_score(literacy_acquisition_kernel__phonics_reading, 0.62).
domain_priors:theater_ratio(literacy_acquisition_kernel__phonics_reading, 0.32).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(literacy_acquisition_kernel__phonics_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(literacy_acquisition_kernel__phonics_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(literacy_acquisition_kernel__phonics_reading, theater_ratio, 0.32).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(literacy_acquisition_kernel__phonics_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(literacy_acquisition_kernel__phonics_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(literacy_acquisition_kernel__phonics_reading, tangled_rope).
narrative_ontology:human_readable(literacy_acquisition_kernel__phonics_reading, "Phonics-First Mandate: Systematic Decoding Instruction Before Connected Text").
narrative_ontology:topic_domain(literacy_acquisition_kernel__phonics_reading, "educational_psychology/literacy_pedagogy/cognitive_science").

domain_priors:requires_active_enforcement(literacy_acquisition_kernel__phonics_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(literacy_acquisition_kernel__phonics_reading, '1ffd0d22-88a5-4874-a790-d66027705a12').
narrative_ontology:cs_kernel_codification('1ffd0d22-88a5-4874-a790-d66027705a12', formalized).
narrative_ontology:cs_authority_grounding('1ffd0d22-88a5-4874-a790-d66027705a12', expertise).
narrative_ontology:cs_interpretation_layer_present('1ffd0d22-88a5-4874-a790-d66027705a12').
narrative_ontology:cs_reading_relation('1ffd0d22-88a5-4874-a790-d66027705a12', literacy_acquisition_kernel__whole_language_reading, forecloses).
narrative_ontology:cs_reading_relation('1ffd0d22-88a5-4874-a790-d66027705a12', literacy_acquisition_kernel__balanced_literacy_reading, influences).
narrative_ontology:cs_reading_relation('1ffd0d22-88a5-4874-a790-d66027705a12', literacy_acquisition_kernel__structured_literacy_reading, influences).
narrative_ontology:cs_axiom('1ffd0d22-88a5-4874-a790-d66027705a12', foundational, decoding_precedes_comprehension).
narrative_ontology:cs_axiom_status(decoding_precedes_comprehension, holdable).
narrative_ontology:cs_axiom_grounding('1ffd0d22-88a5-4874-a790-d66027705a12', decoding_precedes_comprehension, empirically_contingent).
narrative_ontology:cs_axiom('1ffd0d22-88a5-4874-a790-d66027705a12', foundational, explicit_code_instruction_required_before_text_exposure).
narrative_ontology:cs_axiom_status(explicit_code_instruction_required_before_text_exposure, holdable).
narrative_ontology:cs_axiom_grounding('1ffd0d22-88a5-4874-a790-d66027705a12', explicit_code_instruction_required_before_text_exposure, empirically_contingent).
narrative_ontology:cs_axiom('1ffd0d22-88a5-4874-a790-d66027705a12', secondary, fidelity_justifies_scripted_delivery).
narrative_ontology:cs_axiom_status(fidelity_justifies_scripted_delivery, holdable).
narrative_ontology:cs_axiom_grounding('1ffd0d22-88a5-4874-a790-d66027705a12', fidelity_justifies_scripted_delivery, instrumental).
narrative_ontology:cs_reference_frame('1ffd0d22-88a5-4874-a790-d66027705a12', explicit_systematic_code_first).
narrative_ontology:cs_drift_state('1ffd0d22-88a5-4874-a790-d66027705a12', contemporary_science_of_reading_era, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('1ffd0d22-88a5-4874-a790-d66027705a12', '').
narrative_ontology:cs_kernel_id(literacy_acquisition_kernel__phonics_reading, literacy_acquisition_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(literacy_acquisition_kernel__phonics_reading, students_with_weak_phonological_awareness).
narrative_ontology:constraint_beneficiary(literacy_acquisition_kernel__phonics_reading, typical_developing_readers).
narrative_ontology:constraint_beneficiary(literacy_acquisition_kernel__phonics_reading, parents_of_struggling_readers).
narrative_ontology:constraint_beneficiary(literacy_acquisition_kernel__phonics_reading, curriculum_publishers).
narrative_ontology:constraint_victim(literacy_acquisition_kernel__phonics_reading, classroom_teachers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(literacy_acquisition_kernel__phonics_reading, typical_developing_readers).
narrative_ontology:constraint_victim(literacy_acquisition_kernel__phonics_reading, education_school_faculties).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Children entering school without the ability to hear and manipulate the sounds in spoken words. Under the mandate they receive daily explicit lessons pairing letters with sounds, blending drills, and decodable texts matched to what has been taught. They cannot choose their school's method, decline the lessons, or switch classrooms; their families' levers are advocacy, complaints, or moving districts.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__phonics_reading, students_with_weak_phonological_awareness, beneficiary,
    powerless, biographical, trapped, national).

% Children who would likely crack the code under almost any method. They receive the same systematic sequence, which consolidates accuracy and spelling, but spend literacy-block minutes on drills and decodable texts they may not need, deferring trade books, discussion, and writing connected to what they read.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__phonics_reading, typical_developing_readers, beneficiary,
    powerless, biographical, trapped, national).
narrative_ontology:stakeholder_secondary_role(literacy_acquisition_kernel__phonics_reading, typical_developing_readers, payer).

% Deliver mandated lessons from adopted programs, often word-for-word, under pacing guides and fidelity checks; deviation is flagged in walkthroughs. Veteran teachers describe the displacement of judgments they were trained to make about pacing, grouping, and text choice. Union channels exist, and leaving the profession or transferring out of early grades is possible but costly; many states also require retraining coursework on teachers' own time.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__phonics_reading, classroom_teachers, payer,
    organized, biographical, constrained, national).

% Design and license scripted phonics programs and decodable-text lines. State alignment lists and district adoptions convert statutory mandates into multi-year contracts. They fund conferences, advertise alignment, and revise product lines to match new statutory definitions of acceptable instruction. Their market is portable: the same programs sell across states and abroad.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__phonics_reading, curriculum_publishers, beneficiary,
    powerful, generational, arbitrage, global).

% Write the statutes and adoption criteria defining acceptable early-reading instruction, approve curricula, fund teacher retraining, and audit lesson fidelity. They can amend or repeal the requirements, though doing so means contending with advocacy coalitions, federal grant conditions, and the political cost of appearing to retreat on reading.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__phonics_reading, state_education_authorities, agenda_setter,
    institutional, generational, mobile, national).

% Organized advocacy groups that pressed for the mandates after watching children fail under prior methods. They receive screening notifications and guaranteed interventions for their children; their leverage is testimony, litigation threats, and elections rather than day-to-day control of classrooms.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__phonics_reading, parents_of_struggling_readers, beneficiary,
    organized, biographical, constrained, national).

% Author networks, coaching firms, and university labs built around leveled texts and cueing-based methods. Adoption criteria requiring systematic phonics alignment remove their programs from eligibility lists, and their training contracts lapse as districts retrain staff. They publish rebuttals and pursue waiver routes but sit outside the adoption conversation the criteria define.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__phonics_reading, balanced_literacy_trainer_networks, excluded,
    organized, biographical, trapped, national).

% Teacher-preparation faculties whose coursework emphasized meaning-making and cueing must rewrite syllabi to statutory specifications, adopt state-selected textbooks, and document coverage to keep certification programs approved. Academic-freedom disputes arise, but accreditation leverage leaves little room to refuse.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__phonics_reading, education_school_faculties, payer,
    institutional, generational, constrained, national).

% Researchers in reading psychology and education produce the experimental and meta-analytic evidence the mandates cite. They evaluate whether programs stay faithful to findings, flag overclaims in both directions, and hold no administrative role; their influence runs through citations in statutes, hearings, and litigation records.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__phonics_reading, reading_scientists, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(literacy_acquisition_kernel__phonics_reading, curriculum_publishers).
narrative_ontology:fixing_cost_class(literacy_acquisition_kernel__phonics_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Aligns thousands of early-grade classrooms on a single explicit sequence of grapheme-phoneme correspondences, so that children who cannot yet segment speech sounds encounter the full code systematically instead of relying on incidental discovery during text exposure.
% TRANSFER_FUNCTION: Moves instructional decision-making from classroom teachers to program designers and state criteria; moves literacy-block time toward discrete decoding drills and decodable texts and away from extended connected-text engagement; moves district funds to adopting vendors through multi-year licenses.
% ABSENT_VOICES: Balanced-literacy and whole-language practitioner and trainer networks are excluded from adoption panels once phonics-alignment criteria bind; experienced teachers' objections to scripting surface mainly through union channels after mandates pass rather than during criteria drafting. Developmentalists emphasizing play-based early learning are likewise outside the statutory conversation.
% DISAPPEARANCE_RATIONALE: If the mandates vanished overnight, adoption lists would lapse, decodable-text procurement and fidelity monitoring would end, retraining requirements would dissolve, and preparation faculties would revert to discretionary syllabi. Early-reading instruction would reorganize around whatever districts and teachers chose, and the advocacy coalitions built around the mandates would redirect their campaigns.
% FOUNDING_PROBLEM: Mass decoding failure: under incidental, text-immersion-era instruction, large fractions of children — disproportionately those with weak phonological awareness and from disadvantaged backgrounds — left early grades unable to decode fluently, and reading failure compounded into every later subject.
% FOUNDING_PROBLEM_CORROBORATION: NAEP long-term trend data and state screening audits attest persistent decoding failure from outside the benefiting parties; peer-reviewed meta-analytic syntheses corroborate the efficacy premise independently of vendor and legislative seats. No source outside the movement's own citation network attests the stronger universal-timing claim — that ALL children require code instruction BEFORE any connected text — and that asymmetry is itself signal.
narrative_ontology:disappearance_verdict(literacy_acquisition_kernel__phonics_reading, world_rearranges).
narrative_ontology:founding_problem_status(literacy_acquisition_kernel__phonics_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(literacy_acquisition_kernel__phonics_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(literacy_acquisition_kernel__phonics_reading, 'none', 1).
narrative_ontology:epsilon_provenance(literacy_acquisition_kernel__phonics_reading, 0.58, 'stealth/ox-alpha', 'none', direct).

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
 *   Claimed type is tangled_rope because the arrangement possesses both a genuine coordination function (systematic code instruction measurably reduces decoding failure, especially for children with weak phonological awareness) and asymmetric extraction riding the same structure (teacher judgment displaced by scripts; rival trainer networks locked out of adoption; licensing revenue concentrated in aligned vendors), held together by active enforcement (statutes, adoption criteria, fidelity walkthroughs). Metrics are authored independently of the claim. Base extractiveness 0.58 reflects real costs imposed on governed parties — chiefly teachers — net of the arrangement's genuine instructional output. Suppression 0.62 is a raw structural property, unscaled by power or scope in the engine's arithmetic, capturing statutory compulsion, closed adoption lists, and monitored delivery. Theater_ratio 0.32 counts fidelity paperwork, retraining-completion certificates, and alignment marketing against the functional core of actual lessons. Accessibility_collapse 0.5: alternatives persist at the margins (supplemental texts, waiver routes, teacher-built materials) but the mandated core is locked. Resistance 0.55: union objections, academic-freedom disputes, and trainer-network rebuttals are real and coalition-capable but have not reversed the wave. All three temporal series run on ONE shared grid (points 0, 2, 4, 6, 8, 10, 12, mapping 2013-2025) so no metric borrows another's timeline; the trajectories are monotonic because the interval is a single legislative-adoption wave, with intensity cycling around legislative sessions and media events rather than reversing direction.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently from identical structural inputs. Classroom teachers sit nearest the full-target end: they bear the displacement of judgment, their exit is costly, and veteran teachers carry a partial identity lock — professional self-concept constituted through pedagogical discretion — so the same script reads as scaffold to a novice and as expropriation to a veteran. Students with weak phonological awareness sit near the beneficiary end: the arrangement subsidizes precisely the skill they lack, and their trapped exit never converts into extraction because the delivered good is the thing they need. Typical developing readers sit nearer symmetric: real consolidation gains against real opportunity cost. Curriculum publishers collect without bearing delivery costs; state authorities administer and can amend, so they experience the arrangement as policy instrument rather than constraint. If the veteran-teacher identity frame broke — the script reframed as tool rather than affront — measured resistance would fall with no structural change, which is why the identity component is carried in commentary and omegas rather than baked into the suppression scalar.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations (students_with_weak_phonological_awareness, typical_developing_readers, parents_of_struggling_readers, curriculum_publishers) drive low d for those seats; the victim declaration (classroom_teachers) drives high d. No directionality overrides are used: the role-plus-exit data already separates the seats that matter, and the override axis (keyed by power atom) is too coarse to distinguish novice from veteran teachers anyway — that divergence is documented in commentary and omegas instead. Typical developing readers carry a secondary payer role, placing them nearer symmetric than a pure-beneficiary derivation would. Curriculum publishers are declared beneficiaries despite not delivering instruction: adoption criteria convert statutes into contract pipelines, making them the seat the arrangement's material gains demonstrably accrue to — hence gain_flow names that seat rather than 'diffuse'.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — mass decoding failure among children left to induce the code incidentally — is live, attested by NAEP trends and state screening data from outside the benefiting parties, so the arrangement is not mandatrophy-resolved and no zombie flag is warranted. The forward risk is the opposite failure: if universal screening plus early intervention drives decoding failure down substantially, the mandate layer (fidelity monitoring, retraining quotas, alignment certification) could persist as performance while the instructional stakes shrink; the theater_ratio series is the designated tell, and a sustained rise past 0.5 would mark the transition toward piton. The R5 mismatch consumer reads founding_problem_status=live against disappearance_verdict=world_rearranges and finds no mismatch: the world genuinely depends on the arrangement right now.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_position,
    'This constraint is one reading of the literacy_acquisition_kernel (the phonics_reading). What would change structurally if a sibling reading were adopted instead?',
    'Comparative policy analysis across jurisdictions that adopted different readings: track which seats gain and lose when a state shifts from phonics-first mandates toward balanced-literacy or structured-literacy frameworks.',
    'Under the whole_language_reading the victim set inverts — students with weak phonological awareness become the harmed seat and teacher discretion is restored; under the structured_literacy_reading the beneficiary set narrows and deepens (dyslexia-targeted scaffolds) while teacher-facing scripting intensifies. This file''s own classification is unaffected; the omega records the committer structure and the location of the disagreement (sequencing and universality of code instruction).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_position, conceptual, 'Committer-frame position of this story within the literacy kernel''s reading set.').

omega_variable(
    universal_vs_targeted_efficacy,
    'Does the evidence support the reading''s universal claim — that all children require systematic code instruction before connected text — or only a targeted claim concentrated on children with weak phonological awareness?',
    'Moderator analyses in the experimental and meta-analytic literature separating effect sizes by baseline phonological awareness and instructional dosage.',
    'If benefits concentrate in the weak-phonological-awareness subgroup, the universal mandate imposes net opportunity costs on typical readers, raising effective extraction on the student side and drifting the mandate layer toward snare-flavored assessment; if effects are broad, the coordination framing strengthens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(universal_vs_targeted_efficacy, empirical, 'Whether universal mandation is warranted by the evidence or overreaches a targeted finding.').

omega_variable(
    script_fidelity_tradeoff,
    'Does scripted delivery raise outcomes for novice teachers more than it lowers them for experienced teachers whose adaptive judgment is displaced?',
    'Within-state comparisons of fidelity-monitored versus professionally-discretionary implementations, stratified by teacher experience, holding curriculum constant.',
    'If novices gain more than veterans lose, teacher-facing extraction is coordination overhead and the hybrid-coordination reading is generous; if veterans lose more, the scripting layer is closer to pure extraction and the arrangement drifts toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(script_fidelity_tradeoff, empirical, 'Whether the scripting cost is a genuine coordination expense or a rent imposed on professional judgment.').

omega_variable(
    adoption_criteria_capture,
    'Are state adoption criteria for ''aligned'' curricula shaped by vendor participation in drafting, such that alignment tracks product features rather than instructional efficacy?',
    'Disclosure of vendor involvement in criteria drafting; comparison of official alignment scores against independent effect-size rankings.',
    'If captured, gain concentration in curriculum_publishers hardens and the enforcement machinery reads as market protection layered onto pedagogy, strengthening the extraction half of the hybrid.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(adoption_criteria_capture, empirical, 'Whether the alignment apparatus serves pedagogy or vendor market position.').

omega_variable(
    suppression_source_ambiguity,
    'Is teacher compliance with scripted delivery maintained by structural enforcement (statutes, walkthroughs, adoption lock-in) or by internalized professional norms (deference to ''the science'', fear of blame for reading failure)?',
    'Post-repeal trajectory: if districts and teachers revert to discretionary practice quickly where mandates lapse, suppression was structural; if scripted habits and self-monitoring persist, a substantial internalized component is present.',
    'If internalized, effective suppression exceeds the structural measure and would survive formal deregulation — changing any forecast of what repealing the mandates would actually restore.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_source_ambiguity, empirical, 'Structural versus internalized sources of teacher-side compliance.').

omega_variable(
    authority_framing_underdetermination,
    'Is the arrangement''s authority structure better framed as expertise-grounded (credentialed reading science interpreting evidence into practice standards) or extraction-grounded (statutory machinery whose stability generates licensing rents and compliance budgets)?',
    'Trace whose interpretations bind: if program-effectiveness disputes are settled by evidence review, the expertise framing holds; if they are settled by adoption-list eligibility and procurement, the extraction framing holds.',
    'The two framings yield different commitment-system classifications for the same arrangement. The expertise framing was chosen here because the reading''s warrant is empirical competence, but the alternative framing is coherent and would reclassify the authority structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(authority_framing_underdetermination, conceptual, 'Framing under-determination in the commitment-system classification of the mandate regime.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(literacy_acquisition_kernel__phonics_reading, 0, 12).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(lite_tr_t0, literacy_acquisition_kernel__phonics_reading, theater_ratio, 0, 0.14).
narrative_ontology:measurement(lite_tr_t2, literacy_acquisition_kernel__phonics_reading, theater_ratio, 2, 0.17).
narrative_ontology:measurement(lite_tr_t4, literacy_acquisition_kernel__phonics_reading, theater_ratio, 4, 0.2).
narrative_ontology:measurement(lite_tr_t6, literacy_acquisition_kernel__phonics_reading, theater_ratio, 6, 0.23).
narrative_ontology:measurement(lite_tr_t8, literacy_acquisition_kernel__phonics_reading, theater_ratio, 8, 0.26).
narrative_ontology:measurement(lite_tr_t10, literacy_acquisition_kernel__phonics_reading, theater_ratio, 10, 0.29).
narrative_ontology:measurement(lite_tr_t12, literacy_acquisition_kernel__phonics_reading, theater_ratio, 12, 0.32).

% Extraction over time
narrative_ontology:measurement(lite_be_t0, literacy_acquisition_kernel__phonics_reading, base_extractiveness, 0, 0.36).
narrative_ontology:measurement(lite_be_t2, literacy_acquisition_kernel__phonics_reading, base_extractiveness, 2, 0.4).
narrative_ontology:measurement(lite_be_t4, literacy_acquisition_kernel__phonics_reading, base_extractiveness, 4, 0.44).
narrative_ontology:measurement(lite_be_t6, literacy_acquisition_kernel__phonics_reading, base_extractiveness, 6, 0.48).
narrative_ontology:measurement(lite_be_t8, literacy_acquisition_kernel__phonics_reading, base_extractiveness, 8, 0.51).
narrative_ontology:measurement(lite_be_t10, literacy_acquisition_kernel__phonics_reading, base_extractiveness, 10, 0.55).
narrative_ontology:measurement(lite_be_t12, literacy_acquisition_kernel__phonics_reading, base_extractiveness, 12, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(lite_su_t0, literacy_acquisition_kernel__phonics_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(lite_su_t2, literacy_acquisition_kernel__phonics_reading, suppression_requirement, 2, 0.41).
narrative_ontology:measurement(lite_su_t4, literacy_acquisition_kernel__phonics_reading, suppression_requirement, 4, 0.47).
narrative_ontology:measurement(lite_su_t6, literacy_acquisition_kernel__phonics_reading, suppression_requirement, 6, 0.52).
narrative_ontology:measurement(lite_su_t8, literacy_acquisition_kernel__phonics_reading, suppression_requirement, 8, 0.56).
narrative_ontology:measurement(lite_su_t10, literacy_acquisition_kernel__phonics_reading, suppression_requirement, 10, 0.59).
narrative_ontology:measurement(lite_su_t12, literacy_acquisition_kernel__phonics_reading, suppression_requirement, 12, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(literacy_acquisition_kernel__phonics_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(literacy_acquisition_kernel__phonics_reading, whole_language_reading).
narrative_ontology:affects_constraint(literacy_acquisition_kernel__phonics_reading, balanced_literacy_reading).
narrative_ontology:affects_constraint(literacy_acquisition_kernel__phonics_reading, structured_literacy_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'how reading should be taught' decomposes into four structurally distinct constraints — readings of the shared literacy_acquisition_kernel — because each reading instantiates a different instructional regime with a different epsilon, beneficiary set, and victim set. This file instantiates the phonics_reading only. The whole_language_reading is the displaced predecessor whose exclusion the enforcement machinery maintains; the balanced_literacy_reading is the pressured middle position that has absorbed systematic-phonics components under downstream influence from this reading; the structured_literacy_reading shares this reading's evidentiary base while extending it to a five-pillar, dyslexia-centered framework. Epsilon differs across the family because the arrangements differ, not because one constraint is measured different ways.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
