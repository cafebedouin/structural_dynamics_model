% ============================================================================
% CONSTRAINT STORY: literacy_acquisition_kernel__whole_language_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_literacy_acquisition_kernel__whole_language_reading, []).

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
 *   constraint_id: literacy_acquisition_kernel__whole_language_reading
 *   human_readable: Whole-Language Reading Instruction Regime
 *   domain: educational_psychology/literacy_pedagogy/cognitive_science
 *
 * SUMMARY:
 *   The whole-language instructional regime held that reading acquisition
 *   emerges from meaningful engagement with connected text, that decoding
 *   skills develop naturally through exposure, and that explicit phonics
 *   instruction is unnecessary and potentially harmful to motivation. From
 *   the mid-1980s it moved from movement to machinery: state English-language
 *   arts frameworks, adoption lists, funded training institutes, and
 *   university certification pipelines made literature-based immersion the
 *   default beginning-reading method across large systems. Its costs landed
 *   unevenly. Children from print-rich homes arrived decoding-ready and
 *   thrived; children without home literacy support — and acutely, children
 *   with dyslexia — were asked to induce a code nobody taught, and the
 *   resulting failure gaps compounded annually. The arrangement preserved and
 *   celebrated teacher professional judgment, which is the seat its benefits
 *   concentrate on. Epsilon's referent is this standing arrangement — the
 *   whole-language regime as operated — assessed on the reading's own terms:
 *   its naturality claims taken seriously and scored against what the
 *   arrangement actually delivered to each seat. Family note: the colloquial
 *   label 'how reading is taught' decomposes into four structurally distinct
 *   claims (this file plus the phonics, structured-literacy, and
 *   balanced-literacy readings linked in network.affects_constraints); each
 *   carries its own epsilon, victims, and beneficiaries, and the epsilons
 *   differ because the arrangements differ — this one spares teacher autonomy
 *   while loading the omitted instruction onto home-literacy-poor students, a
 *   distribution no sibling shares. KEY AGENTS (by structural relationship):
 *   - whole_language_curriculum_authorities: agenda-setting seat
 *   (institutional/constrained) — authored the frameworks and adoption
 *   machinery - elementary_classroom_teachers: primary beneficiary seat
 *   (organized/constrained) — professional autonomy preserved; absorbs blame
 *   cycles - teacher_education_institutions: doctrinal beneficiary
 *   (institutional/identity_locked) — transmits the method through
 *   certification - literature_curriculum_publishers: commercial beneficiary
 *   (institutional/arbitrage) — collects adoption-cycle revenue -
 *   advantaged_students_from_print_rich_homes: incidental beneficiary
 *   (moderate/trapped) - students_without_home_literacy_support: primary
 *   target seat (powerless/trapped) — bears the omitted instruction -
 *   students_with_dyslexia: acute target seat (powerless/trapped) — implicit
 *   acquisition unavailable to them - explicit_instruction_advocates:
 *   excluded seat (organized/mobile) — outside the framework committees -
 *   reading_outcome_assessors: analytical observer (institutional/analytical)
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(literacy_acquisition_kernel__whole_language_reading, 0.64).
domain_priors:suppression_score(literacy_acquisition_kernel__whole_language_reading, 0.45).
domain_priors:theater_ratio(literacy_acquisition_kernel__whole_language_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(literacy_acquisition_kernel__whole_language_reading, extractiveness, 0.64).
narrative_ontology:constraint_metric(literacy_acquisition_kernel__whole_language_reading, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(literacy_acquisition_kernel__whole_language_reading, theater_ratio, 0.35).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(literacy_acquisition_kernel__whole_language_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(literacy_acquisition_kernel__whole_language_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(literacy_acquisition_kernel__whole_language_reading, tangled_rope).
narrative_ontology:human_readable(literacy_acquisition_kernel__whole_language_reading, "Whole-Language Reading Instruction Regime").
narrative_ontology:topic_domain(literacy_acquisition_kernel__whole_language_reading, "educational_psychology/literacy_pedagogy/cognitive_science").

domain_priors:requires_active_enforcement(literacy_acquisition_kernel__whole_language_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(literacy_acquisition_kernel__whole_language_reading, '1dcbad61-f877-49ff-8a31-6aaf1d1bc96b').
narrative_ontology:cs_kernel_codification('1dcbad61-f877-49ff-8a31-6aaf1d1bc96b', distributed).
narrative_ontology:cs_authority_grounding('1dcbad61-f877-49ff-8a31-6aaf1d1bc96b', practice).
narrative_ontology:cs_interpretation_layer_present('1dcbad61-f877-49ff-8a31-6aaf1d1bc96b').
narrative_ontology:cs_reading_relation('1dcbad61-f877-49ff-8a31-6aaf1d1bc96b', literacy_acquisition_kernel__phonics_reading, forecloses).
narrative_ontology:cs_reading_relation('1dcbad61-f877-49ff-8a31-6aaf1d1bc96b', literacy_acquisition_kernel__structured_literacy_reading, forecloses).
narrative_ontology:cs_reading_relation('1dcbad61-f877-49ff-8a31-6aaf1d1bc96b', literacy_acquisition_kernel__balanced_literacy_reading, forecloses).
narrative_ontology:cs_axiom('1dcbad61-f877-49ff-8a31-6aaf1d1bc96b', foundational, literacy_emerges_naturally_from_text_engagement).
narrative_ontology:cs_axiom_status(literacy_emerges_naturally_from_text_engagement, holdable).
narrative_ontology:cs_axiom_grounding('1dcbad61-f877-49ff-8a31-6aaf1d1bc96b', literacy_emerges_naturally_from_text_engagement, empirically_contingent).
narrative_ontology:cs_axiom('1dcbad61-f877-49ff-8a31-6aaf1d1bc96b', foundational, explicit_decoding_instruction_harmful_to_motivation).
narrative_ontology:cs_axiom_status(explicit_decoding_instruction_harmful_to_motivation, holdable).
narrative_ontology:cs_axiom_grounding('1dcbad61-f877-49ff-8a31-6aaf1d1bc96b', explicit_decoding_instruction_harmful_to_motivation, empirically_contingent).
narrative_ontology:cs_axiom('1dcbad61-f877-49ff-8a31-6aaf1d1bc96b', secondary, teacher_professional_judgment_over_scripted_delivery).
narrative_ontology:cs_axiom_status(teacher_professional_judgment_over_scripted_delivery, holdable).
narrative_ontology:cs_axiom_grounding('1dcbad61-f877-49ff-8a31-6aaf1d1bc96b', teacher_professional_judgment_over_scripted_delivery, conventional).
narrative_ontology:cs_reference_frame('1dcbad61-f877-49ff-8a31-6aaf1d1bc96b', natural_acquisition_through_meaningful_use).
narrative_ontology:cs_drift_state('1dcbad61-f877-49ff-8a31-6aaf1d1bc96b', post_national_reading_panel_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('1dcbad61-f877-49ff-8a31-6aaf1d1bc96b', '').
narrative_ontology:cs_kernel_id(literacy_acquisition_kernel__whole_language_reading, literacy_acquisition_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(literacy_acquisition_kernel__whole_language_reading, elementary_classroom_teachers).
narrative_ontology:constraint_beneficiary(literacy_acquisition_kernel__whole_language_reading, teacher_education_institutions).
narrative_ontology:constraint_beneficiary(literacy_acquisition_kernel__whole_language_reading, literature_curriculum_publishers).
narrative_ontology:constraint_beneficiary(literacy_acquisition_kernel__whole_language_reading, advantaged_students_from_print_rich_homes).
narrative_ontology:constraint_victim(literacy_acquisition_kernel__whole_language_reading, students_without_home_literacy_support).
narrative_ontology:constraint_victim(literacy_acquisition_kernel__whole_language_reading, students_with_dyslexia).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(literacy_acquisition_kernel__whole_language_reading, elementary_classroom_teachers).
narrative_ontology:constraint_vindicates(literacy_acquisition_kernel__whole_language_reading, emergent_literacy_theory).
narrative_ontology:constraint_vindicates(literacy_acquisition_kernel__whole_language_reading, psycholinguistic_guessing_model).
narrative_ontology:constraint_vindicates(literacy_acquisition_kernel__whole_language_reading, constructivist_pedagogy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% State education departments and district adoption committees wrote the English-language arts frameworks that privileged literature-based instruction, certified the textbook and big-book programs that carried it, and funded the training institutes that spread it. Their standing rests on the frameworks they authored; reversing course means publicly repudiating their own prior judgments, so correction tended to arrive slowly and through external pressure.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__whole_language_reading, whole_language_curriculum_authorities, agenda_setter,
    institutional, generational, constrained, national).

% Gained classroom autonomy, literature-rich libraries, and freedom from scripted basal manuals; professional conferences and journals celebrated the craft identity this enabled. When state reading scores collapsed, the same teachers absorbed public blame and later faced retraining mandates under new statutes. Union representation gives them collective voice, but assignment to a school pins the method they may use.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__whole_language_reading, elementary_classroom_teachers, beneficiary,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(literacy_acquisition_kernel__whole_language_reading, elementary_classroom_teachers, payer).

% Colleges and departments of education built reading-methods courses, hiring lines, and certification requirements around constructivist literacy theory. Their graduates staff the classrooms, which makes their syllabi the transmission belt for the doctrine. Counterevidence implicates the faculty's own scholarly output, so updating carries reputational cost well beyond ordinary curriculum revision.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__whole_language_reading, teacher_education_institutions, beneficiary,
    institutional, generational, identity_locked, national).

% Sold literature anthologies, big books, and predictable-text series into state adoption cycles timed to the frameworks. Revenue followed whichever pedagogy the adoption committees blessed, and the catalog pivoted accordingly — toward balanced-literacy packages, then toward decodable-series imprints once statutes began demanding them.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__whole_language_reading, literature_curriculum_publishers, beneficiary,
    institutional, biographical, arbitrage, national).

% Arrived at school already knowing letters, sounds, and book-handling from home read-alouds; the literature-rich classroom matched what their homes had already provided, and they would have learned to decode under nearly any method on offer. They experienced the regime at its best: engaging books, discussion, writing for real audiences.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__whole_language_reading, advantaged_students_from_print_rich_homes, beneficiary,
    moderate, biographical, trapped, national).

% Arrived with little alphabet knowledge, little book exposure, and thinner vocabularies from fewer read-alouds. Immersion in authentic text asked them to infer a code nobody taught; many guessed from pictures and first letters, stalled in grade one, and watched the gap widen yearly as texts grew harder. Compulsory attendance left no exit from the method their school had adopted.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__whole_language_reading, students_without_home_literacy_support, payer,
    powerless, biographical, trapped, national).

% Process written language differently and cannot induce letter-sound patterns from exposure alone, however rich. Under implicit-only instruction they met daily failure, were often described as unmotivated or slow, and internalized that verdict before anyone screened for the actual cause.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__whole_language_reading, students_with_dyslexia, payer,
    powerless, biographical, trapped, national).

% Reading scientists, panel members, and parents of failing readers argued for explicit, systematic code instruction throughout the regime's ascendancy. Framework committees declined their testimony; their findings circulated in journals while classrooms ran on the frameworks. Sustained journalism, litigation, and legislative campaigns eventually moved statutes, though far more slowly than they moved the research literature.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__whole_language_reading, explicit_instruction_advocates, excluded,
    organized, generational, mobile, national).

% National and state assessment programs measured fourth-grade reading outcomes across cohorts and published the trend lines. Their series became the shared evidence both camps argued from; they administer no instruction and take no pedagogical position.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__whole_language_reading, reading_outcome_assessors, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(literacy_acquisition_kernel__whole_language_reading, diffuse).
narrative_ontology:fixing_cost_class(literacy_acquisition_kernel__whole_language_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates early-years literacy classrooms around shared authentic literature, meaning-making discussion, and writing for real purposes, giving teachers a common professional identity and freeing instruction from scripted basal drills; it solves the engagement problem that decontextualized skill drills created.
% TRANSFER_FUNCTION: Moves instructional hours away from explicit phoneme-grapheme teaching toward literature engagement; effectively transfers the job of teaching the code to households with print resources and educated caregivers, and transfers the risk of reading failure onto children whose homes cannot supply it.
% ABSENT_VOICES: Struggling readers themselves — too young to articulate what was missing — along with parents lacking the cultural capital to contest framework committees, and the cognitive scientists of reading whose testimony adoption bodies declined during the regime's ascendancy. Their absence from curriculum deliberation is what let unanimity persist internally while failure accumulated externally.
% DISAPPEARANCE_RATIONALE: Classrooms would reorganize around explicit sequential code instruction, teacher-preparation curricula would restructure, publishers would re-cut their catalogs, and the distribution of reading success would shift toward children currently failing — the arrangement demonstrably organizes staffing, materials, certification, and outcomes, so its overnight removal rearranges all of them.
% FOUNDING_PROBLEM: Mid-century basal readers reduced beginning reading to repetitive decontextualized drill, which practitioners observed draining children's interest in reading and reducing teachers to script-deliverers; whole language proposed immersion in authentic, meaningful text as the cure.
% FOUNDING_PROBLEM_CORROBORATION: Practitioner memoirs and historians of education corroborate the drill-joylessness problem from outside the movement's institutions; the National Reading Panel and subsequent meta-analyses attest the problem was real while finding no evidentiary warrant that it required abandoning systematic phonics. Stated plainly: no source outside whole-language institutions attests that the founding problem justified the omission of explicit code instruction — the movement's own teacher-education literature is the sole attester of that step.
narrative_ontology:disappearance_verdict(literacy_acquisition_kernel__whole_language_reading, world_rearranges).
narrative_ontology:founding_problem_status(literacy_acquisition_kernel__whole_language_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(literacy_acquisition_kernel__whole_language_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(literacy_acquisition_kernel__whole_language_reading, 'none', 1).
narrative_ontology:epsilon_provenance(literacy_acquisition_kernel__whole_language_reading, 0.64, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(literacy_acquisition_kernel__whole_language_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(literacy_acquisition_kernel__whole_language_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(literacy_acquisition_kernel__whole_language_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.64 reflects an arrangement whose genuine goods (engagement, professional craft) coexist with a heavy, unevenly distributed cost: the code instruction some children cannot self-supply. Suppression 0.45 is the current scalar of a machinery that peaked mid-interval — frameworks barred decodable texts and discouraged explicit teaching — and has since been partially dismantled by statute; suppression is authored as a raw structural property and is deliberately NOT scaled by scope or directionality, which the engine applies only to extractiveness. Theater 0.35: authentic-literature displays, invented-spelling celebrations, and print-rich-environment audits increasingly performed the doctrine's vitality after its empirical core was contested, while the core activity (children reading real books) remained real. Accessibility collapse 0.48: the alternative was never unknown — the reading wars were public — but institutional access to it was blocked for teachers inside adopting systems until legislation reopened it. Resistance 0.70: two decades of organized scientific, parental, journalistic, and finally legislative opposition. The measurement series share one seven-point grid (1985-2025) so every metric is authored at every examined time point; the suppression_requirement series is included because this story specifically tracks enforcement-capacity change — build-up to the 1997 peak, statutory decay thereafter — not merely shifting extraction. The arc is rise-and-partial-retreat rather than cyclical: no oscillation mechanism drives it.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setting and teacher-education seats experience the arrangement as emancipation — professional judgment restored, children engaged with real books — and compute a benign, coordination-dominant type from where they stand. The trapped payer seats compute the opposite: a child without home print support experiences the identical classroom as the place where the thing she needed was withheld. Same nominal institution, opposite lived types; the divergence is structural (exit options and directionality), not informational. Children additionally cannot form coalitions or exit on their own behalf, so their seat has no lever short of adult advocacy — which is why the excluded advocates' eventual legislative route, not market or consumer pressure, is what moved the structure.
 *
 * DIRECTIONALITY LOGIC:
 *   Declarations map to directionality as follows. Beneficiaries sit near the subsidized end: publishers (arbitrage-grade exit) nearest of all; teacher-education institutions are pinned there harder by identity-lock; classroom teachers derive low d from their beneficiary declaration with a modest upward pull from their secondary payer position (blame and retraining cycles). Advantaged students derive low-to-symmetric d — genuine benefit, negligible cost. The two victim groups derive high d, and their trapped exit pushes them toward the full-target end; nothing modulates it downward because no household chooses its school's method. The engine scales effective extraction by these directionalities and by scope (national frameworks amplify verification difficulty); the author supplies only the structural declarations above. No directionality overrides are authored: the derivation chain produces the right relationships from the beneficiary/victim data plus exit options, and no two same-power seats diverge in a way the derivation misses.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — joyless basal drill — was real, and the engagement function whole language built is real; a classifier that read the arrangement as pure extraction would erase the motivational gains many classrooms genuinely enjoyed, while a classifier that accepted the movement's self-description as natural law would erase the children paying for the omitted instruction. The tangled-rope structure holds both truths: a coordination function (identity_coordination — professional community and craft identity) plus an asymmetric burden concentrated on those least able to refuse it. The R5 interview locates the obsolescence risk precisely: the founding problem is contested rather than dead, so no dead-plus-rearranges mismatch fires, but the residue now surviving inside teacher-education syllabi after statutory repeal elsewhere has a thinning function and a stubborn theater ratio — the classic pre-piton profile if the identity-locked institutions outlast the statutes. Whether ed-school syllabi update post-2025 distinguishes orderly retirement from inertial persistence.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_committer_structure,
    'This story instantiates the whole_language_reading of the literacy_acquisition_kernel; how would the computed per-seat classifications shift if the phonics_reading, structured_literacy_reading, or balanced_literacy_reading instantiation were generated instead?',
    'Generate the three sibling stories over the same interval and compare computed seats: the teacher-autonomy seat flips from beneficiary to payer under the explicit-instruction readings, and the struggling-student seats flip from targets to served parties.',
    'Cross-reading comparison isolates what each reading of the kernel sacrifices; the classification of this file is valid only for the whole-language arrangement, not for the kernel itself.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_committer_structure, conceptual, 'Committer structure: one reading of the literacy-acquisition kernel; siblings are separate constraints.').

omega_variable(
    home_environment_confound,
    'How much of the measured reading failure under whole language is attributable to the method itself versus confounds (household income, school funding, teacher quality) that correlate with print-poor homes?',
    'Randomized and quasi-random comparisons of instructional method within matched populations; state natural experiments where method changed under statute while demographics held (for example, Mississippi''s post-2013 trajectory).',
    'If confounds dominate, epsilon is overstated and the arrangement''s harm shrinks toward background inequality; if method effects survive controls (as National Reading Panel findings indicate), epsilon stands and the victim attribution is confirmed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(home_environment_confound, empirical, 'Method effect versus socioeconomic confound in attributed reading failure.').

omega_variable(
    critical_period_irreversibility,
    'Does delayed explicit decoding instruction fully remediate for children who missed it, or does the early window close such that whole-language-era cohorts carry permanent losses (Matthew effects)?',
    'Longitudinal remediation outcomes for older struggling readers receiving late structured intervention; cohort comparisons of early- versus late-identified students.',
    'Full remediation caps the harm at years of lost efficiency; irreversibility raises it further and weights the target-seat severity toward lifetime earnings and justice-system correlates.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(critical_period_irreversibility, empirical, 'Whether the harm of deferred code instruction is recoverable or compounding.').

omega_variable(
    identity_lock_update_trajectory,
    'Will teacher-education institutions update their reading-methods canon on the post-2000 evidence, or does professional-identity fusion sustain the doctrine past statutory repeal as residue inside balanced-literacy hybrids?',
    'Track certification-course syllabi, textbook adoptions, and licensure-exam content in the years following science-of-reading statutes; compare states with and without enforcement teeth.',
    'Updating resolves the residue toward orderly retirement; non-updating predicts an inertial remnant whose maintenance is increasingly theatrical, drifting the surviving arrangement toward degraded-function territory.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_update_trajectory, conceptual, 'Identity-locked institutional persistence versus evidence-driven revision.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(literacy_acquisition_kernel__whole_language_reading, 1985, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(lite_tr_t1985, literacy_acquisition_kernel__whole_language_reading, theater_ratio, 1985, 0.2).
narrative_ontology:measurement(lite_tr_t1991, literacy_acquisition_kernel__whole_language_reading, theater_ratio, 1991, 0.28).
narrative_ontology:measurement(lite_tr_t1997, literacy_acquisition_kernel__whole_language_reading, theater_ratio, 1997, 0.38).
narrative_ontology:measurement(lite_tr_t2003, literacy_acquisition_kernel__whole_language_reading, theater_ratio, 2003, 0.35).
narrative_ontology:measurement(lite_tr_t2010, literacy_acquisition_kernel__whole_language_reading, theater_ratio, 2010, 0.33).
narrative_ontology:measurement(lite_tr_t2018, literacy_acquisition_kernel__whole_language_reading, theater_ratio, 2018, 0.36).
narrative_ontology:measurement(lite_tr_t2025, literacy_acquisition_kernel__whole_language_reading, theater_ratio, 2025, 0.35).

% Extraction over time
narrative_ontology:measurement(lite_be_t1985, literacy_acquisition_kernel__whole_language_reading, base_extractiveness, 1985, 0.42).
narrative_ontology:measurement(lite_be_t1991, literacy_acquisition_kernel__whole_language_reading, base_extractiveness, 1991, 0.55).
narrative_ontology:measurement(lite_be_t1997, literacy_acquisition_kernel__whole_language_reading, base_extractiveness, 1997, 0.7).
narrative_ontology:measurement(lite_be_t2003, literacy_acquisition_kernel__whole_language_reading, base_extractiveness, 2003, 0.66).
narrative_ontology:measurement(lite_be_t2010, literacy_acquisition_kernel__whole_language_reading, base_extractiveness, 2010, 0.6).
narrative_ontology:measurement(lite_be_t2018, literacy_acquisition_kernel__whole_language_reading, base_extractiveness, 2018, 0.62).
narrative_ontology:measurement(lite_be_t2025, literacy_acquisition_kernel__whole_language_reading, base_extractiveness, 2025, 0.64).

% Suppression requirement over time
narrative_ontology:measurement(lite_su_t1985, literacy_acquisition_kernel__whole_language_reading, suppression_requirement, 1985, 0.28).
narrative_ontology:measurement(lite_su_t1991, literacy_acquisition_kernel__whole_language_reading, suppression_requirement, 1991, 0.55).
narrative_ontology:measurement(lite_su_t1997, literacy_acquisition_kernel__whole_language_reading, suppression_requirement, 1997, 0.74).
narrative_ontology:measurement(lite_su_t2003, literacy_acquisition_kernel__whole_language_reading, suppression_requirement, 2003, 0.58).
narrative_ontology:measurement(lite_su_t2010, literacy_acquisition_kernel__whole_language_reading, suppression_requirement, 2010, 0.5).
narrative_ontology:measurement(lite_su_t2018, literacy_acquisition_kernel__whole_language_reading, suppression_requirement, 2018, 0.47).
narrative_ontology:measurement(lite_su_t2025, literacy_acquisition_kernel__whole_language_reading, suppression_requirement, 2025, 0.45).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(literacy_acquisition_kernel__whole_language_reading, identity_coordination).
narrative_ontology:affects_constraint(literacy_acquisition_kernel__whole_language_reading, phonics_reading).
narrative_ontology:affects_constraint(literacy_acquisition_kernel__whole_language_reading, structured_literacy_reading).
narrative_ontology:affects_constraint(literacy_acquisition_kernel__whole_language_reading, balanced_literacy_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'the reading wars' / 'how children learn to read' conflates four structurally distinct claims about one kernel; per the epsilon-invariance principle they are authored as four linked stories. This (whole-language) story's epsilon is indexed to the literature-immersion arrangement, whose victim set is home-literacy-poor students and dyslexic students; the phonics and structured-literacy stories index epsilon to explicit-instruction regimes whose principal costs fall instead on teacher time, retraining, and curricular flexibility. Upstream/downstream: the phonics and structured-literacy readings carry the stronger post-2000 empirical record and function as the evidentiary foil this reading was written against; balanced literacy mediates between the camps and inherits parts of this reading's apparatus.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
