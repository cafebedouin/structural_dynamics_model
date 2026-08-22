% ============================================================================
% CONSTRAINT STORY: literacy_acquisition_kernel__balanced_literacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_literacy_acquisition_kernel__balanced_literacy_reading, []).

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
 *   constraint_id: literacy_acquisition_kernel__balanced_literacy_reading
 *   human_readable: Balanced Literacy Instructional Settlement
 *   domain: educational_psychology/literacy_pedagogy/cognitive_science
 *
 * SUMMARY:
 *   Balanced literacy arose in the 1990s as a settlement of the reading wars:
 *   a framework promising that systematic phonics instruction and meaningful
 *   engagement with connected text are complementary, with 'instructional
 *   balance' as the criterion of sound pedagogy. In practice, the
 *   settlement's center of gravity sat close to its whole-language
 *   inheritance — cueing-based word recognition, leveled predictable texts,
 *   workshop structures — with phonics present largely as brief mini-lessons
 *   rather than a systematic, cumulative scope and sequence. An ecosystem of
 *   publishers, author-led consultant networks, and preparation programs grew
 *   up around the framework, refreshing revenue through edition cycles and
 *   training contracts. This file instantiates ONE reading of the
 *   literacy_acquisition_kernel — the balanced_literacy_reading — as a clean,
 *   epsilon-invariant constraint. The sibling readings (phonics_reading,
 *   whole_language_reading, structured_literacy_reading) are separate stories
 *   with their own epsilon values and are NOT described or averaged here.
 *   Epsilon's referent is the standing balanced-literacy arrangement as
 *   implemented, assessed by this reading's own honest lights: a framework
 *   that does coordinate real instructional activity and delivers real
 *   text-engagement benefits, while transferring substantial public spending
 *   to its vendor ecosystem and imposing its largest costs on the weakest
 *   parties in the room. The claim/metric gap is deliberate: claimed_type is
 *   authored from the structure I believe true (a genuine coordination
 *   function joined to asymmetric, actively enforced transfer); the metrics
 *   are authored from the operation I believe descriptively true. Where the
 *   engine's computed types diverge from the claim, that divergence is the
 *   datum.
 *
 * KEY AGENTS:
 *   - - literacy_curriculum_publishers: Agenda-setting collector (institutional/arbitrage) — defines the method edition by edition, sells the materials and the training, and can relabel the catalog when the policy climate turns.
 *   - - teacher_preparation_programs: Collector with agenda-setting reach (institutional/identity-locked) — certifies the workforce and reproduces the method through coursework its own faculty cannot easily disown.
 *   - - literacy_pd_consultants: Collector (organized/mobile) — monetizes implementation and every re-training cycle.
 *   - - school_district_administrators: Agenda setter who also pays (institutional/constrained) — signs the contracts, bears the proficiency scores.
 *   - - classroom_teachers: Cost-bearer with incidental benefits (organized/constrained) — implements daily, absorbs the blame, supplied by the method community.
 *   - - struggling_decoders: Primary cost-bearer (powerless/trapped) — children who plateau under cueing-based instruction.
 *   - - dyslexic_students: Highest-cost cost-bearers (powerless/trapped) — need what the general instructional tier does not supply.
 *   - - science_of_reading_advocates: Excluded voice turned external pressure (organized/mobile) — entered through legislatures and journalism.
 *   - - reading_scientists: Analytical observer (institutional/analytical) — evidentiary seat, no vote in adoption.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(literacy_acquisition_kernel__balanced_literacy_reading, 0.62).
domain_priors:suppression_score(literacy_acquisition_kernel__balanced_literacy_reading, 0.55).
domain_priors:theater_ratio(literacy_acquisition_kernel__balanced_literacy_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(literacy_acquisition_kernel__balanced_literacy_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(literacy_acquisition_kernel__balanced_literacy_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(literacy_acquisition_kernel__balanced_literacy_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(literacy_acquisition_kernel__balanced_literacy_reading, accessibility_collapse, 0.38).
narrative_ontology:constraint_metric(literacy_acquisition_kernel__balanced_literacy_reading, resistance, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(literacy_acquisition_kernel__balanced_literacy_reading, tangled_rope).
narrative_ontology:human_readable(literacy_acquisition_kernel__balanced_literacy_reading, "Balanced Literacy Instructional Settlement").
narrative_ontology:topic_domain(literacy_acquisition_kernel__balanced_literacy_reading, "educational_psychology/literacy_pedagogy/cognitive_science").

domain_priors:requires_active_enforcement(literacy_acquisition_kernel__balanced_literacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(literacy_acquisition_kernel__balanced_literacy_reading, '0ee1056e-ec5c-4630-bed0-2d5f093ff47b').
narrative_ontology:cs_kernel_codification('0ee1056e-ec5c-4630-bed0-2d5f093ff47b', distributed).
narrative_ontology:cs_authority_grounding('0ee1056e-ec5c-4630-bed0-2d5f093ff47b', lineage).
narrative_ontology:cs_interpretation_layer_present('0ee1056e-ec5c-4630-bed0-2d5f093ff47b').
narrative_ontology:cs_reading_relation('0ee1056e-ec5c-4630-bed0-2d5f093ff47b', literacy_acquisition_kernel__phonics_reading, coexists_with).
narrative_ontology:cs_reading_relation('0ee1056e-ec5c-4630-bed0-2d5f093ff47b', literacy_acquisition_kernel__whole_language_reading, influences).
narrative_ontology:cs_reading_relation('0ee1056e-ec5c-4630-bed0-2d5f093ff47b', literacy_acquisition_kernel__structured_literacy_reading, coexists_with).
narrative_ontology:cs_axiom('0ee1056e-ec5c-4630-bed0-2d5f093ff47b', foundational, code_and_meaning_jointly_necessary).
narrative_ontology:cs_axiom_status(code_and_meaning_jointly_necessary, holdable).
narrative_ontology:cs_axiom_grounding('0ee1056e-ec5c-4630-bed0-2d5f093ff47b', code_and_meaning_jointly_necessary, empirically_contingent).
narrative_ontology:cs_axiom('0ee1056e-ec5c-4630-bed0-2d5f093ff47b', foundational, instructional_balance_as_pedagogical_criterion).
narrative_ontology:cs_axiom_status(instructional_balance_as_pedagogical_criterion, holdable).
narrative_ontology:cs_axiom_grounding('0ee1056e-ec5c-4630-bed0-2d5f093ff47b', instructional_balance_as_pedagogical_criterion, conventional).
narrative_ontology:cs_axiom('0ee1056e-ec5c-4630-bed0-2d5f093ff47b', secondary, contextual_cuing_legitimate_word_recognition_route).
narrative_ontology:cs_axiom_status(contextual_cuing_legitimate_word_recognition_route, holdable).
narrative_ontology:cs_axiom_grounding('0ee1056e-ec5c-4630-bed0-2d5f093ff47b', contextual_cuing_legitimate_word_recognition_route, empirically_contingent).
narrative_ontology:cs_reference_frame('0ee1056e-ec5c-4630-bed0-2d5f093ff47b', constructivist_meaning_first_balance).
narrative_ontology:cs_drift_state('0ee1056e-ec5c-4630-bed0-2d5f093ff47b', contemporary_science_of_reading_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('0ee1056e-ec5c-4630-bed0-2d5f093ff47b', '').
narrative_ontology:cs_kernel_id(literacy_acquisition_kernel__balanced_literacy_reading, literacy_acquisition_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(literacy_acquisition_kernel__balanced_literacy_reading, teacher_preparation_programs).
narrative_ontology:constraint_beneficiary(literacy_acquisition_kernel__balanced_literacy_reading, literacy_curriculum_publishers).
narrative_ontology:constraint_beneficiary(literacy_acquisition_kernel__balanced_literacy_reading, literacy_pd_consultants).
narrative_ontology:constraint_victim(literacy_acquisition_kernel__balanced_literacy_reading, struggling_decoders).
narrative_ontology:constraint_victim(literacy_acquisition_kernel__balanced_literacy_reading, dyslexic_students).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(literacy_acquisition_kernel__balanced_literacy_reading, classroom_teachers).
narrative_ontology:constraint_victim(literacy_acquisition_kernel__balanced_literacy_reading, school_district_administrators).
narrative_ontology:constraint_victim(literacy_acquisition_kernel__balanced_literacy_reading, classroom_teachers).
narrative_ontology:constraint_vindicates(literacy_acquisition_kernel__balanced_literacy_reading, constructivist_learning_theory).
narrative_ontology:constraint_vindicates(literacy_acquisition_kernel__balanced_literacy_reading, three_cueing_msv_model).
narrative_ontology:constraint_vindicates(literacy_acquisition_kernel__balanced_literacy_reading, workshop_model_pedagogy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Authors and sells the core curricula — workshop manuals, leveled book libraries, intervention kits — and defines what counts as balanced instruction edition by edition. Revenue arrives through district adoptions and through professional-development days delivered by author-affiliated consultants. Each method revision refreshes the product line; when the policy climate turns, the catalog can be relabeled far faster than classrooms can retrain.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__balanced_literacy_reading, literacy_curriculum_publishers, agenda_setter,
    institutional, generational, arbitrage, national).

% Prepares and certifies the teaching workforce; the method fills required coursework, and faculty careers, journals, and departmental reputations are built on its scholarly foundations. Course catalogs move slowly, and revising them means telling a generation of graduates their preparation was incomplete. Accreditation loops and hiring pipelines reproduce the approach internally.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__balanced_literacy_reading, teacher_preparation_programs, beneficiary,
    institutional, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(literacy_acquisition_kernel__balanced_literacy_reading, teacher_preparation_programs, agenda_setter).

% Delivers the training days, coaches teachers in their classrooms, and certifies new trainers. Income tracks adoption breadth and the pace of re-training cycles; the work is portable across providers but only within the method ecosystem that generates demand for coaching in the first place.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__balanced_literacy_reading, literacy_pd_consultants, beneficiary,
    organized, biographical, mobile, national).

% Chooses the curriculum, signs the multi-year adoption and training contracts, and answers to school boards and state scorecards for proficiency results. Switching mid-cycle means retraining staff, buying materials twice, and defending the reversal politically, so adoptions tend to run their contracted length regardless of interim evidence.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__balanced_literacy_reading, school_district_administrators, agenda_setter,
    institutional, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(literacy_acquisition_kernel__balanced_literacy_reading, school_district_administrators, payer).

% Teaches the daily lessons with the training and materials provided, and carries the proficiency numbers home. The method community supplies lesson plans, collegial support, and a shared vocabulary; public dissent risks professional isolation, and many quietly supplement with materials bought personally. Retraining onto a different approach is unpaid, off-hours work.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__balanced_literacy_reading, classroom_teachers, payer,
    organized, biographical, constrained, local).
narrative_ontology:stakeholder_secondary_role(literacy_acquisition_kernel__balanced_literacy_reading, classroom_teachers, beneficiary).

% Children in the earliest grades who cannot yet turn print into speech. Instruction leans on guessing from pictures, context, and first letters, so they plateau while classmates take off; they cannot choose their classroom, and the costs arrive as retention, referral delays, and learned avoidance of print.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__balanced_literacy_reading, struggling_decoders, payer,
    powerless, immediate, trapped, local).

% Students who need the letter-sound code taught explicitly, cumulatively, and early. The general classroom tier rarely provides it, so the typical path runs through years of failure before formal identification, with families financing outside tutoring the school day did not contain.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__balanced_literacy_reading, dyslexic_students, payer,
    powerless, biographical, trapped, national).

% Parent groups, dyslexia organizations, and reporters who spent years outside the rooms where curricula were adopted. Their leverage arrived through state legislation and investigative reporting; they press for explicit-code requirements, universal screening mandates, and curriculum audits.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__balanced_literacy_reading, science_of_reading_advocates, excluded,
    organized, biographical, mobile, national).

% Cognitive and education researchers who study how printed-word reading develops. Their findings circulate in policy fights and legislative records, but they hold no vote in adoption decisions; their seat is evidentiary, exercised through publication and testimony.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__balanced_literacy_reading, reading_scientists, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(literacy_acquisition_kernel__balanced_literacy_reading, literacy_curriculum_publishers).
narrative_ontology:fixing_cost_class(literacy_acquisition_kernel__balanced_literacy_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Gives districts, preparation programs, publishers, and teachers a shared instructional framework — common materials, common terminology, common training sequences — and a truce formula ('skills and meaning both matter') that let two warring pedagogical camps coexist inside one school system without either vetoing the other.
% TRANSFER_FUNCTION: Moves public education spending — curriculum licenses, leveled-library kits, professional-development days, graduate tuition — from districts and state budgets to publishers, consultant networks, and preparation programs; and moves young children's instructional minutes toward meaning-guessing strategies and away from explicit code practice, with the shortfall borne by the students who needed the code taught directly.
% ABSENT_VOICES: Struggling five-to-eight-year-olds cannot sit on curriculum committees; dyslexia families were long routed to special-education hearings instead of adoption meetings; reading scientists were consulted after choices were made, if at all. Their entry route into the conversation ran through legislatures and investigative journalism rather than the adoption rooms themselves.
% DISAPPEARANCE_RATIONALE: Districts would re-tender curricula toward explicit-code programs, publishers would relabel product lines (already visibly underway), preparation syllabi would shift under accreditation and legislative pressure, and the professional-development market would re-price around the new standard — the instructional economy reorganizes around whichever method the evidence regime rewards.
% FOUNDING_PROBLEM: End the reading wars: after decades of open conflict between phonics-first and whole-language camps, districts needed a purchasable, trainable consensus that neither camp could veto, and early whole-language classrooms had produced documented decoding failures that demanded a visible concession to skills instruction.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: state legislative findings and statutory recitals (Mississippi's 2013 Literacy-Based Promotion Act deliberations and the subsequent wave of science-of-reading statutes) attest that the settlement failed struggling readers; the National Reading Panel (2000) and later meta-analytic work corroborate the empirical core of the original problem. Publisher and preparation-program testimony disputes the failure reading, so the status is genuinely disputed across seats rather than settled.
narrative_ontology:disappearance_verdict(literacy_acquisition_kernel__balanced_literacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(literacy_acquisition_kernel__balanced_literacy_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(literacy_acquisition_kernel__balanced_literacy_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(literacy_acquisition_kernel__balanced_literacy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(literacy_acquisition_kernel__balanced_literacy_reading, 0.62, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(literacy_acquisition_kernel__balanced_literacy_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(literacy_acquisition_kernel__balanced_literacy_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(literacy_acquisition_kernel__balanced_literacy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness ends at 0.62: the transfer is real and recurring (adoption licenses, kit refreshes, PD days, tuition) and is sustained by gatekeeping rather than by demonstrated superiority over cheaper explicit-code alternatives, but it is not total — the framework does deliver functioning instruction to the majority of learners who would crack the code under almost any method. Suppression is 0.55 and unscaled by construction: it measures the raw enforcement machinery (adoption lock-in, certification gatekeeping, professional stigma against 'drill and kill', marginalization of critics), which the engine scales only for extractiveness. Theater_ratio is 0.48 at interval end: the phonics component of 'balanced' instruction has been substantially performative — present in catalogs and mini-lesson schedules to deflect criticism, thin in daily dosage — while the text-engagement half remains functionally real. Accessibility_collapse is low (0.38): once the critique is understood, alternatives (explicit-code curricula, structured approaches) remain fully available, and reform states demonstrate exit is achievable. Resistance is high (0.65) and rising through the interval: parent coalitions, journalism, and 40-plus state legislative actions. The measurement series runs on ONE shared nine-point grid (all three metrics authored at every point) tracing roughly one full reading-war cycle: crisis (NRP 2000, Reading First) forces temporary real phonics (extractiveness and theater dip 2000–2004), the collapse of Reading First (~2008) relaxes pressure and the practice core re-expands (extraction accumulates 2008–2018), then the science-of-reading wave imposes partial rollback (2018–2024). The oscillation is not noise: each swing of the cycle is itself a revenue event — every crisis-and-reconciliation round sells new editions, new kits, and new training days, an intermittent-reinforcement structure in which the settlement profits from the very instability it mediates. Base_properties values correspond to the 2024 endpoint (post-mandate partial-rollback phase). Coalition note: the powerless child seats cannot coordinate individually; their coalition capacity is supplied externally by the advocate seat, which is precisely why the constraint's resistance metric moved only after adults organized.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute very differently. From the publisher and consultant seats, the arrangement is a functioning professional ecosystem they built, staff, and supply — coordination with a healthy margin. From the preparation-program seat, it is a scholarly tradition under unjustified political attack. From the district seat, it is a procurement commitment whose switching costs dominate. From the classroom-teacher seat, it is daily labor plus borrowed blame. From the struggling-decoder and dyslexic-student seats, it is the reason the code stayed opaque. Same framework, four incompatible lived types; the engine derives this divergence from the declared roles, power levels, and exit options, not from the claim.
 *
 * DIRECTIONALITY LOGIC:
 *   Publishers sit nearest the beneficiary pole: they set the agenda AND collect the largest direct flow, with arbitrage-grade exit (catalogs rebrand faster than classrooms retrain). Preparation programs are also beneficiary-side but identity-locked — their benefit is fused with the arrangement, which stabilizes their position rather than exposing them. Consultants are beneficiary-side with mobile exit inside the ecosystem. Struggling decoders and dyslexic students sit at the full-target pole: powerless, trapped, bearing the instructional shortfall with no exit at all. Classroom teachers derive mid-to-high: declared payers (blame, retraining burden, suppressed dissent) moderated by a genuine secondary benefit (community, materials, employment). District administrators are the interesting case: their agenda-setting role pulls the derivation beneficiary-ward, but they substantively pay (budgets, doubled material costs, political risk), putting their true position near symmetric. I declined a directionality override to correct this because overrides key on the power atom, and the institutional atom is shared by publishers, preparation programs, and reading scientists — a single override would misapply across all of them. The differentiated exit options (arbitrage vs identity_locked vs constrained vs analytical) already separate those seats in the derivation, which is the correct lever here.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding mandate — end the reading war by giving both camps a defensible common framework — has been overtaken by events: the war has reignited along nearly the same fault line, now with the settlement itself as the accused. What persists is the revenue and credentialing architecture the peace process built. The classification discipline prevents both standard mislabels: calling this a pure coordination success ignores the asymmetric transfer (public budgets to vendors, instructional time away from the children who needed the code) and the enforcement needed to hold it; calling it pure extraction ignores the genuine coordination function (a shared framework, real text-engagement benefits, a workforce trained in common) without which no district could operate at all. The honest structural verdict is the hybrid: coordination and transfer ride the same adoption contracts, the same training days, the same leveled libraries. Whether the hybrid is a genuine third reading or a rebrand is carried as an omega, not resolved by assertion.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    genuine_synthesis_vs_rebrand,
    'Is balanced literacy a genuine third reading of the kernel — a real synthesis delivering what neither sibling could — or a whole-language rebrand that retained the cueing practice core under a label engineered to survive the phonics critique?',
    'Comparative outcome studies from districts implementing authentic systematic phonics plus rich text versus balanced literacy as typically implemented, combined with quantitative curricular analysis of phonics dosage, scope, and sequence in the flagship programs.',
    'If rebrand, this story collapses toward its whole-language ancestry: epsilon rises, the victim structure sharpens, and the computed type shifts toward the pure-extraction end. If genuine synthesis, the measured transfer is mostly churn-rent riding a real coordination function and the hybrid classification stands.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(genuine_synthesis_vs_rebrand, conceptual, 'Whether the settlement is a third reading or a relabeled sibling.').

omega_variable(
    method_churn_rent_share,
    'What fraction of publisher and consultant revenue depends on periodic method revision and re-training cycles rather than on demonstrated instructional efficacy?',
    'Financial analysis of curriculum publishers'' revenue around edition releases and adoptions, correlated against independent outcome evaluations of successive editions.',
    'A high churn-rent share identifies the extraction mechanism precisely and supports recomputation toward the pure-extraction end; a low share supports the coordination-cost reading of the same cash flows.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(method_churn_rent_share, empirical, 'Share of ecosystem revenue attributable to revision cycles rather than efficacy.').

omega_variable(
    counterfactual_decoder_outcomes,
    'How many struggling decoders would have reached proficiency under explicit, systematic code instruction delivered in the same classrooms by the same teachers?',
    'Natural experiments from state mandates (Mississippi 2013 onward, the subsequent statute wave) comparing cohort proficiency trajectories before and after explicit-instruction requirements, with demographic controls.',
    'Sizes the victim class and resolves the ''victim unclear'' ambiguity in the expected structural delta: large proficiency deltas sharpen the victim declaration and raise effective extraction on the child seats; null deltas would vindicate the synthesis claim.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(counterfactual_decoder_outcomes, empirical, 'Magnitude of the proficiency shortfall attributable to the instructional approach.').

omega_variable(
    ed_school_identity_lock_mechanism,
    'Is preparation-program resistance to explicit-instruction research structural (accreditation loops, hiring pipelines, textbook economics) or internalized (faculty identity fused with constructivist pedagogy such that revision feels like self-repudiation)?',
    'Post-mandate trajectory tracking of preparation programs in reform states: if syllabi revise under mandate and the revision persists across faculty turnover, the lock is structural; if programs revert when enforcement lapses, the lock is internalized.',
    'If internalized, the arrangement''s effective suppression outlives any policy removal — graduates carry the framework into classrooms regardless of statute — and the persistence prognosis worsens accordingly.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ed_school_identity_lock_mechanism, empirical, 'Structural versus internalized source of the preparation-program lock.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(literacy_acquisition_kernel__balanced_literacy_reading, 1990, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(lite_tr_t1990, literacy_acquisition_kernel__balanced_literacy_reading, theater_ratio, 1990, 0.28).
narrative_ontology:measurement_basis(lite_tr_t1990, observed).
narrative_ontology:measurement(lite_tr_t1995, literacy_acquisition_kernel__balanced_literacy_reading, theater_ratio, 1995, 0.33).
narrative_ontology:measurement_basis(lite_tr_t1995, observed).
narrative_ontology:measurement(lite_tr_t2000, literacy_acquisition_kernel__balanced_literacy_reading, theater_ratio, 2000, 0.36).
narrative_ontology:measurement_basis(lite_tr_t2000, observed).
narrative_ontology:measurement(lite_tr_t2004, literacy_acquisition_kernel__balanced_literacy_reading, theater_ratio, 2004, 0.34).
narrative_ontology:measurement_basis(lite_tr_t2004, observed).
narrative_ontology:measurement(lite_tr_t2008, literacy_acquisition_kernel__balanced_literacy_reading, theater_ratio, 2008, 0.44).
narrative_ontology:measurement_basis(lite_tr_t2008, observed).
narrative_ontology:measurement(lite_tr_t2013, literacy_acquisition_kernel__balanced_literacy_reading, theater_ratio, 2013, 0.49).
narrative_ontology:measurement_basis(lite_tr_t2013, observed).
narrative_ontology:measurement(lite_tr_t2018, literacy_acquisition_kernel__balanced_literacy_reading, theater_ratio, 2018, 0.53).
narrative_ontology:measurement_basis(lite_tr_t2018, observed).
narrative_ontology:measurement(lite_tr_t2021, literacy_acquisition_kernel__balanced_literacy_reading, theater_ratio, 2021, 0.51).
narrative_ontology:measurement_basis(lite_tr_t2021, observed).
narrative_ontology:measurement(lite_tr_t2024, literacy_acquisition_kernel__balanced_literacy_reading, theater_ratio, 2024, 0.48).
narrative_ontology:measurement_basis(lite_tr_t2024, observed).

% Extraction over time
narrative_ontology:measurement(lite_be_t1990, literacy_acquisition_kernel__balanced_literacy_reading, base_extractiveness, 1990, 0.44).
narrative_ontology:measurement_basis(lite_be_t1990, observed).
narrative_ontology:measurement(lite_be_t1995, literacy_acquisition_kernel__balanced_literacy_reading, base_extractiveness, 1995, 0.48).
narrative_ontology:measurement_basis(lite_be_t1995, observed).
narrative_ontology:measurement(lite_be_t2000, literacy_acquisition_kernel__balanced_literacy_reading, base_extractiveness, 2000, 0.5).
narrative_ontology:measurement_basis(lite_be_t2000, observed).
narrative_ontology:measurement(lite_be_t2004, literacy_acquisition_kernel__balanced_literacy_reading, base_extractiveness, 2004, 0.53).
narrative_ontology:measurement_basis(lite_be_t2004, observed).
narrative_ontology:measurement(lite_be_t2008, literacy_acquisition_kernel__balanced_literacy_reading, base_extractiveness, 2008, 0.6).
narrative_ontology:measurement_basis(lite_be_t2008, observed).
narrative_ontology:measurement(lite_be_t2013, literacy_acquisition_kernel__balanced_literacy_reading, base_extractiveness, 2013, 0.64).
narrative_ontology:measurement_basis(lite_be_t2013, observed).
narrative_ontology:measurement(lite_be_t2018, literacy_acquisition_kernel__balanced_literacy_reading, base_extractiveness, 2018, 0.67).
narrative_ontology:measurement_basis(lite_be_t2018, observed).
narrative_ontology:measurement(lite_be_t2021, literacy_acquisition_kernel__balanced_literacy_reading, base_extractiveness, 2021, 0.66).
narrative_ontology:measurement_basis(lite_be_t2021, observed).
narrative_ontology:measurement(lite_be_t2024, literacy_acquisition_kernel__balanced_literacy_reading, base_extractiveness, 2024, 0.62).
narrative_ontology:measurement_basis(lite_be_t2024, observed).

% Suppression requirement over time
narrative_ontology:measurement(lite_su_t1990, literacy_acquisition_kernel__balanced_literacy_reading, suppression_requirement, 1990, 0.35).
narrative_ontology:measurement_basis(lite_su_t1990, observed).
narrative_ontology:measurement(lite_su_t1995, literacy_acquisition_kernel__balanced_literacy_reading, suppression_requirement, 1995, 0.39).
narrative_ontology:measurement_basis(lite_su_t1995, observed).
narrative_ontology:measurement(lite_su_t2000, literacy_acquisition_kernel__balanced_literacy_reading, suppression_requirement, 2000, 0.46).
narrative_ontology:measurement_basis(lite_su_t2000, observed).
narrative_ontology:measurement(lite_su_t2004, literacy_acquisition_kernel__balanced_literacy_reading, suppression_requirement, 2004, 0.43).
narrative_ontology:measurement_basis(lite_su_t2004, observed).
narrative_ontology:measurement(lite_su_t2008, literacy_acquisition_kernel__balanced_literacy_reading, suppression_requirement, 2008, 0.38).
narrative_ontology:measurement_basis(lite_su_t2008, observed).
narrative_ontology:measurement(lite_su_t2013, literacy_acquisition_kernel__balanced_literacy_reading, suppression_requirement, 2013, 0.45).
narrative_ontology:measurement_basis(lite_su_t2013, observed).
narrative_ontology:measurement(lite_su_t2018, literacy_acquisition_kernel__balanced_literacy_reading, suppression_requirement, 2018, 0.52).
narrative_ontology:measurement_basis(lite_su_t2018, observed).
narrative_ontology:measurement(lite_su_t2021, literacy_acquisition_kernel__balanced_literacy_reading, suppression_requirement, 2021, 0.57).
narrative_ontology:measurement_basis(lite_su_t2021, observed).
narrative_ontology:measurement(lite_su_t2024, literacy_acquisition_kernel__balanced_literacy_reading, suppression_requirement, 2024, 0.55).
narrative_ontology:measurement_basis(lite_su_t2024, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(literacy_acquisition_kernel__balanced_literacy_reading, identity_coordination).
narrative_ontology:affects_constraint(literacy_acquisition_kernel__balanced_literacy_reading, phonics_reading).
narrative_ontology:affects_constraint(literacy_acquisition_kernel__balanced_literacy_reading, whole_language_reading).
narrative_ontology:affects_constraint(literacy_acquisition_kernel__balanced_literacy_reading, structured_literacy_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'the reading wars' / 'how children learn to read' decomposes, per the epsilon-invariance principle, into four structurally distinct readings of one kernel, each with its own stable epsilon and its own beneficiary/victim structure. whole_language_reading is the upstream ancestor — this reading inherited its practice core (cueing, leveled text, workshop structure) and its critique of decontextualized drill. structured_literacy_reading is the downstream challenger, drawing on the strongest contemporary empirical convergence and driving the current legislative wave. phonics_reading is the narrowest claim and the common ancestor of both explicit-instruction readings. This file links all three siblings; the family relationship is documented symmetrically in each member's network block.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
