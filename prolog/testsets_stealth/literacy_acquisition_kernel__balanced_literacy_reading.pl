% ============================================================================
% CONSTRAINT STORY: literacy_acquisition_kernel__balanced_literacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-06
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
 *   human_readable: Balanced Literacy Instructional Mandate (Reading Acquisition Kernel, Balanced Reading)
 *   domain: educational psychology/literacy pedagogy/cognitive science
 *
 * SUMMARY:
 *   The balanced literacy reading of the literacy_acquisition_kernel holds
 *   that reading acquisition requires both systematic phonics instruction and
 *   meaningful engagement with connected text, that the two are complementary
 *   rather than contradictory, and that instructional balance — not priority
 *   or sequencing — is the key. As a standing arrangement, this reading is
 *   instantiated in teacher-preparation curricula, adopted core reading
 *   programs, district pacing guides, and the professional-development
 *   economy that retrains teachers at every framework revision. The
 *   arrangement solved a real coordination problem: after three decades of
 *   reading wars it gave the field a peace treaty both camps could nominally
 *   sign. The same structure carries asymmetric extraction: every revision of
 *   what balance means resets the adoption cycle, and the churn revenue flows
 *   to publishers, education schools, and consultants while the costs land on
 *   teachers (serial retraining, outcome blame) and on struggling readers
 *   (instruction underweighted in systematic phonics during the years when
 *   acquisition is most tractable). Constraint family: this file instantiates
 *   only the balanced reading; the phonics_reading, whole_language_reading,
 *   and structured_literacy_reading siblings are separate constraints with
 *   their own epsilon values, beneficiary structures, and classifications,
 *   linked through network.affects_constraints. The claim/metric split is
 *   deliberate: the claimed type is authored from the structure (a genuine
 *   coordination function fused with asymmetric extraction, actively
 *   enforced), while the metrics are authored from the arrangement's
 *   descriptive operation over the 1990-2025 interval; where the engine's
 *   computed type diverges from the claim, that divergence is the measurement
 *   the corpus exists to take.
 *
 * KEY AGENTS:
 *   - basal_reader_publishers: primary beneficiary (institutional/arbitrage) — collects the adoption-cycle revenue that every framework revision generates
 *   - teacher_education_faculties: beneficiary and agenda co-setter (institutional/identity_locked) — trains and accredits the workforce in the framework and is professionally fused with it
 *   - literacy_consultants: secondary beneficiary (organized/arbitrage) — delivers the retraining each revision requires
 *   - district_curriculum_leaders: agenda setter (institutional/constrained) — adopts and administers the framework locally, capturing little of the churn
 *   - classroom_teachers: primary payer (moderate/constrained) — bears serial retraining, contradictory mandates, and outcome blame
 *   - struggling_readers: primary payer (powerless/trapped) — bears the instructional cost during the developmental window that does not reopen
 *   - dyslexia_parent_advocates: excluded voice (organized/constrained) — locked out of adoption and preparation decisions for two decades
 *   - reading_scientists: analytical observer (institutional/analytical) — produces the evidence base both camps contest
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(literacy_acquisition_kernel__balanced_literacy_reading, 0.58).
domain_priors:suppression_score(literacy_acquisition_kernel__balanced_literacy_reading, 0.52).
domain_priors:theater_ratio(literacy_acquisition_kernel__balanced_literacy_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(literacy_acquisition_kernel__balanced_literacy_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(literacy_acquisition_kernel__balanced_literacy_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(literacy_acquisition_kernel__balanced_literacy_reading, theater_ratio, 0.55).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(literacy_acquisition_kernel__balanced_literacy_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(literacy_acquisition_kernel__balanced_literacy_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(literacy_acquisition_kernel__balanced_literacy_reading, tangled_rope).
narrative_ontology:human_readable(literacy_acquisition_kernel__balanced_literacy_reading, "Balanced Literacy Instructional Mandate (Reading Acquisition Kernel, Balanced Reading)").
narrative_ontology:topic_domain(literacy_acquisition_kernel__balanced_literacy_reading, "educational psychology/literacy pedagogy/cognitive science").

domain_priors:requires_active_enforcement(literacy_acquisition_kernel__balanced_literacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(literacy_acquisition_kernel__balanced_literacy_reading, 'd609622f-40fa-4f6a-8e08-9c3e51798bf7').
narrative_ontology:cs_kernel_codification('d609622f-40fa-4f6a-8e08-9c3e51798bf7', distributed).
narrative_ontology:cs_authority_grounding('d609622f-40fa-4f6a-8e08-9c3e51798bf7', expertise).
narrative_ontology:cs_interpretation_layer_present('d609622f-40fa-4f6a-8e08-9c3e51798bf7').
narrative_ontology:cs_reading_relation('d609622f-40fa-4f6a-8e08-9c3e51798bf7', literacy_acquisition_kernel__phonics_reading, influences).
narrative_ontology:cs_reading_relation('d609622f-40fa-4f6a-8e08-9c3e51798bf7', literacy_acquisition_kernel__whole_language_reading, forecloses).
narrative_ontology:cs_reading_relation('d609622f-40fa-4f6a-8e08-9c3e51798bf7', literacy_acquisition_kernel__structured_literacy_reading, coexists_with).
narrative_ontology:cs_axiom('d609622f-40fa-4f6a-8e08-9c3e51798bf7', foundational, phonics_and_meaning_complementary).
narrative_ontology:cs_axiom_status(phonics_and_meaning_complementary, holdable).
narrative_ontology:cs_axiom_grounding('d609622f-40fa-4f6a-8e08-9c3e51798bf7', phonics_and_meaning_complementary, empirically_contingent).
narrative_ontology:cs_axiom('d609622f-40fa-4f6a-8e08-9c3e51798bf7', secondary, instructional_balance_optimal).
narrative_ontology:cs_axiom_status(instructional_balance_optimal, holdable).
narrative_ontology:cs_axiom_grounding('d609622f-40fa-4f6a-8e08-9c3e51798bf7', instructional_balance_optimal, instrumental).
narrative_ontology:cs_reference_frame('d609622f-40fa-4f6a-8e08-9c3e51798bf7', professional_balance_synthesis).
narrative_ontology:cs_drift_state('d609622f-40fa-4f6a-8e08-9c3e51798bf7', post_science_of_reading_legislation, gap(axiom_overriding, substantial, true)).
narrative_ontology:cs_created_at('d609622f-40fa-4f6a-8e08-9c3e51798bf7', '').
narrative_ontology:cs_kernel_id(literacy_acquisition_kernel__balanced_literacy_reading, literacy_acquisition_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(literacy_acquisition_kernel__balanced_literacy_reading, teacher_education_faculties).
narrative_ontology:constraint_beneficiary(literacy_acquisition_kernel__balanced_literacy_reading, basal_reader_publishers).
narrative_ontology:constraint_beneficiary(literacy_acquisition_kernel__balanced_literacy_reading, literacy_consultants).
narrative_ontology:constraint_victim(literacy_acquisition_kernel__balanced_literacy_reading, struggling_readers).
narrative_ontology:constraint_victim(literacy_acquisition_kernel__balanced_literacy_reading, classroom_teachers).
narrative_ontology:constraint_vindicates(literacy_acquisition_kernel__balanced_literacy_reading, balanced_instruction_complementarity_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Faculty of schools of education design and teach the literacy-methods curriculum that every certified teacher passes through; they authored the balanced framework's pedagogy and staff the accreditation bodies that certify preparation programs. Their enrollment, funding, and scholarly reputations are tied to the framework they teach, and the framework is also their professional identity — the constructivist commitment that grounded the field's self-understanding for a generation. Exit would mean repudiating their own scholarly output and retraining a faculty built on it.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__balanced_literacy_reading, teacher_education_faculties, beneficiary,
    institutional, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(literacy_acquisition_kernel__balanced_literacy_reading, teacher_education_faculties, agenda_setter).

% Publish and sell the core reading programs that districts adopt. Every revision of the instructional framework resets the adoption cycle and generates a new round of materials purchases, consumables, and licenses. They supply whatever method the market currently wants — balanced programs in one era, phonics-heavy science-of-reading editions in the next — so the churn itself, not any particular method, is their revenue engine.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__balanced_literacy_reading, basal_reader_publishers, beneficiary,
    institutional, biographical, arbitrage, global).

% Independent trainers, workshop providers, and certified coaches who deliver the professional development that each framework revision requires. Their booking calendars follow the adoption cycle: a district that changes programs must retrain its teachers, and the consultants are the delivery mechanism. They can rebrand their offerings as quickly as the methods change.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__balanced_literacy_reading, literacy_consultants, beneficiary,
    organized, biographical, arbitrage, national).

% Adopt core programs, write pacing guides, and evaluate teachers against the adopted framework. They selected balanced programs in the era when those were the professional consensus and now administer the transition under state science-of-reading laws. Their discretion is bounded by state adoption lists, budget cycles, and the training of the workforce they inherited.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__balanced_literacy_reading, district_curriculum_leaders, agenda_setter,
    institutional, biographical, constrained, regional).

% Deliver daily reading instruction under the framework their district adopted and their preparation program installed. They have been retrained repeatedly as methods revised, are held accountable for outcomes that the framework's vagueness makes difficult to produce, and absorb the blame when results disappoint. Leaving the profession is their main exit; within it, they must teach the adopted program.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__balanced_literacy_reading, classroom_teachers, payer,
    moderate, biographical, constrained, local).

% Children for whom reading does not come easily and who depend entirely on the instruction their school provides during the years when acquisition is most tractable. They cannot choose their school's method; a classroom that underweights systematic phonics costs them the most, and the deficits compound each year. Parents may advocate on their behalf or relocate, but the children themselves have no exit.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__balanced_literacy_reading, struggling_readers, payer,
    powerless, immediate, trapped, local).

% Parents of dyslexic children who discovered through their children's struggles that their schools' balanced programs did not include the systematic instruction the research literature describes. They organized into Decoding Dyslexia chapters in most states and lobbied for screening and structured-literacy mandates, but for two decades they were absent from curriculum adoption and teacher-prep decisions — the rooms where the framework was chosen and maintained.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__balanced_literacy_reading, dyslexia_parent_advocates, excluded,
    organized, biographical, constrained, national).

% Cognitive psychologists and education researchers who study how reading is acquired, whose convergence on the centrality of explicit phonics instruction has been consistent since the National Reading Panel. They produce the evidence base the framework dispute turns on and now advise legislators; for decades their findings sat peripheral to the practitioner consensus that balanced programs embodied.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__balanced_literacy_reading, reading_scientists, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(literacy_acquisition_kernel__balanced_literacy_reading, basal_reader_publishers).
narrative_ontology:fixing_cost_class(literacy_acquisition_kernel__balanced_literacy_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the teaching workforce around a single both/and instructional framework after decades of reading-war fragmentation: it gives districts one defensible program to adopt, preparation programs one methods curriculum to teach, publishers one framework around which to build product lines, and teachers a shared professional vocabulary. The coordination problem it solves — a field at open war over method, with teachers whipsawed between mandates — was real.
% TRANSFER_FUNCTION: Moves adoption spending, professional-development fees, and methods-course tuition from districts, teachers-in-training, and schools to publishers, education schools, and consultants at every framework revision; and moves classroom instructional time toward meaning-first practice under a phonics-inclusive label. It also transfers blame: when outcomes disappoint, failure is attributed to the teacher's imperfect balancing rather than the framework's undefined core.
% ABSENT_VOICES: Dyslexia parents and reading scientists were structurally absent from the rooms where the framework was adopted and maintained — district adoption committees, preparation-program curricula, publisher program design. Struggling readers have no seat at all; their interests enter only indirectly through advocates who were themselves excluded. The consensus that balance was working formed without the parties holding the strongest evidence-based objection, and the unanimity of the practitioner consensus is partly an artifact of who was never in the room.
% DISAPPEARANCE_RATIONALE: Teacher-preparation literacy curricula, basal product lines, the professional-development market, and district adoption cycles all reorganize — indeed they are reorganizing now under state science-of-reading laws, which is the disappearance running in slow motion. Publishers are rebranding product lines, education schools are revising methods courses, and districts are retraining workforces. The arrangement's disappearance is not neutral: every named seat is actively restructuring around its replacement.
% FOUNDING_PROBLEM: The reading wars: three decades of either/or conflict between phonics-first instruction and whole-language immersion left teachers whipsawed between successive mandates, publishers with stranded product lines, and preparation programs without a unifying framework. Balanced literacy was built as the peace treaty — a framework under which both camps could recognize their own commitments.
% FOUNDING_PROBLEM_CORROBORATION: Outside the beneficiary set: classroom teachers and district administrators attest the whipsawing was real; the National Reading Panel and subsequent congressional testimony corroborate the depth of the field's division; and legislative findings in the majority of states now formally attest that the treaty failed struggling readers. The beneficiary seats dispute the failed characterization — they read the same record as evidence the synthesis was never given a fair trial — which is why the founding problem's status is live rather than settled.
narrative_ontology:disappearance_verdict(literacy_acquisition_kernel__balanced_literacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(literacy_acquisition_kernel__balanced_literacy_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(literacy_acquisition_kernel__balanced_literacy_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(literacy_acquisition_kernel__balanced_literacy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(literacy_acquisition_kernel__balanced_literacy_reading, 0.58, 'stealth/ox-alpha', 'none', direct).

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
 *   Extraction is moderate (0.58 at interval end): the arrangement's costs — serial retraining, adoption-cycle materials spending, methods-course tuition — are real but partially matched by genuine instructional goods, and the extraction mechanism is the kernel's constitutive vagueness: balance is never operationally defined, which is what makes continuous churn possible. Suppression (0.52) is structural and socializing rather than coercive: enforcement runs through state adoption lists, preparation-program accreditation, district evaluation rubrics, and the professional socialization of teachers trained inside the framework — internalized commitment does part of the work that mandates do elsewhere, and both rival camps remained legal and live throughout, which caps suppression below snare levels. Theater_ratio (0.55) reflects the arrangement's best-documented failure mode: in many classrooms balance operated as meaning-first practice with phonics minutes scheduled onto the day — the label performed synthesis while the core program remained whole-language in method, the critique crystallized in national investigative reporting on the flagship programs late in the interval. Accessibility_collapse (0.40) is low-moderate: structured-literacy programs, science-of-reading curricula, and explicit phonics approaches persisted throughout and are now legislated in most states, so alternatives never collapsed. Resistance (0.60) is high and rising: reading scientists, dyslexia parent organizations, and eventually state legislatures mounted sustained opposition. The three measurement series run on one shared time grid (t=0 to t=35 at five-year steps) with every metric authored at every point. The suppression_requirement series is authored because enforcement capacity is the dynamic this story traces: the machinery needed to hold the arrangement hardened as dissent grew — from market-consensus diffusion requiring little coercion early in the interval, to accreditation lock-in and administrative defense of adopted programs as the science-of-reading opposition organized. The slight end-interval dip in extraction reflects the first legislative reversals beginning to bite, not voluntary reform.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute materially different constraints. From the publisher seat, the arrangement is a product framework whose periodic revision is the market itself — churn is not a defect but the revenue model, and each revision is experienced as improvement. From the education-faculty seat, it is the field's hard-won professional consensus and, fused with faculty identity, close to self-evident pedagogy; the identity lock means exit would require repudiating a scholarly career, so the seat experiences critique as attack rather than evidence. From the district-leader seat, it is an administrable middle position that kept a divided workforce and a litigious politics quiet. From the teacher seat, the same structure is serial retraining, contradictory mandates, and blame for outcomes the framework's vagueness makes hard to produce. From the struggling reader's seat, the constraint is simply whether systematic instruction arrived in time. Inter-institutionally, the framework that education schools defend as expertise, publishers monetize as product, and districts administer as compliance is experienced by state legislatures — the newest seat — as a failed arrangement to be displaced by mandate. Same-level divergence: classroom teachers and literacy consultants hold adjacent professional standing, but the consultant's arbitrage exit (rebrand the workshop) versus the teacher's constrained exit (leave the profession) makes the identical framework a business opportunity for one and a career liability for the other.
 *
 * DIRECTIONALITY LOGIC:
 *   Publishers sit nearest the beneficiary end: they collect the adoption revenue directly and hold arbitrage exit — they sell whichever method the market wants, so the churn itself, not any method, is their income. Education faculties also sit near the beneficiary end but with identity-locked exit: their benefit (enrollment, accreditation authority, scholarly standing) is fused with the framework they authored, so their directionality is low while their mobility is near zero — they cannot arbitrage away from a constraint they are. Consultants are beneficiaries with arbitrage exit and no institutional identity at stake. District curriculum leaders are mid-structure: they set the local agenda but capture little of the churn and bear its administrative costs, sitting near symmetric. Teachers are targets: they pay retraining and blame with only constrained exit. Struggling readers are the deepest targets: powerless, trapped, and bearing costs during a developmental window that does not reopen — the structural data places them at the full-target end, which is what drives the arrangement's effective extraction above its base rate. Dyslexia parent advocates are excluded rather than coordinated: their exclusion was the enforcement object for two decades. Reading scientists occupy the analytical seat with no extraction pressure in either direction. No directionality overrides are authored: the beneficiary/victim declarations plus exit options produce the correct d for every seat without correction.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — a field at war with itself — is still live, so the arrangement has not outlived its mandate in the classic sense and no mandatrophy resolution is declared. The mandatrophy risk runs in the other direction: the synthesis function may be atrophying into label maintenance while the churn function persists independently. If the science-of-reading transition completes and the field unifies on explicit structured instruction, the peace-treaty function dies, and what remains — adoption machinery defending a discredited label because the cost of admitting failure exceeds what any single administrator bears — would be piton-shaped. The classification prevents two symmetrical mislabels: reading the arrangement as pure coordination (rope) misses the churn extraction that the kernel's vagueness structurally guarantees; reading it as pure extraction (snare) misses the real coordination achievement — the reading wars did pause, teachers did get a shared framework, and the both/and claim has genuine scientific content. The tangled_rope claim keeps both faces on the table while the omegas carry the question of which face is load-bearing.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    genuine_synthesis_or_rebrand,
    'Is balanced literacy a genuine third reading — a real synthesis in which both components are actually delivered — or whole language rebranded, with phonics present as scheduled theater rather than delivered instruction?',
    'Classroom implementation audits measuring actual instructional time allocation, lesson fidelity, and delivered phonics scope-and-sequence against program claims, across districts and program generations.',
    'If rebrand, this constraint collapses into the whole_language_reading structure — its victims, extraction profile, and classification all recompute; if genuine synthesis, the arrangement is a distinct coordination achievement and the tangled_rope claim stands with lower theater.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(genuine_synthesis_or_rebrand, empirical, 'Whether the balanced reading is a distinct constraint or the whole-language reading under a new label.').

omega_variable(
    balance_vagueness_as_extraction_mechanism,
    'Is the kernel''s constitutive vagueness — balance never operationally defined — an accidental limitation of the framework, or a load-bearing extraction mechanism without which the method-churn revenue could not persist?',
    'Counterfactual analysis: model publisher revenue and professional-development demand under a precisely operationalized balance definition (fixed instructional-time allocations, fixed scope-and-sequence) versus the actual undefined standard.',
    'If vagueness is load-bearing, the extraction is structural rather than incidental, moving the arrangement toward the snare end of the hybrid range; if accidental, the churn is a governance failure that an operational definition could fix.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(balance_vagueness_as_extraction_mechanism, conceptual, 'Whether the framework''s undefined core is itself the extraction mechanism.').

omega_variable(
    victim_structure_ambiguity,
    'Who structurally bears the arrangement''s costs — struggling readers who needed systematic instruction they did not receive, teachers who absorb retraining and blame, or no one, if the synthesis genuinely serves both?',
    'Outcome data disaggregated by student profile (typical versus struggling readers) and by teacher preparation background, compared across balanced and structured-literacy instructional regimes.',
    'If struggling readers bear concentrated costs, the victims declarations stand and the asymmetric-extraction half of the hybrid is confirmed; if outcomes are flat across regimes, the victim structure dissolves and the arrangement moves toward rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(victim_structure_ambiguity, empirical, 'Whether the arrangement has identifiable victims or is a genuine all-beneficiary synthesis.').

omega_variable(
    complementarity_vs_sequencing,
    'Does the empirical record support this reading''s foundational axiom — that decoding instruction and meaningful text engagement are complementary and jointly necessary — or the phonics reading''s rival axiom, that systematic decoding precedes and causally enables comprehension?',
    'Component-decomposed randomized trials and meta-analyses that isolate the causal contribution of systematic phonics versus embedded meaning-first instruction to acquisition outcomes.',
    'If phonics instruction is causally primary and sequencing matters, this reading''s foundational axiom is empirically overridden and the reading collapses toward the phonics_reading structure; if the components are genuinely complementary, the axiom holds and the reading remains distinct.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(complementarity_vs_sequencing, empirical, 'The empirical status of the complementarity axiom against its sequencing rival.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(literacy_acquisition_kernel__balanced_literacy_reading, 0, 35).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(lite_tr_t0, literacy_acquisition_kernel__balanced_literacy_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement(lite_tr_t5, literacy_acquisition_kernel__balanced_literacy_reading, theater_ratio, 5, 0.33).
narrative_ontology:measurement(lite_tr_t10, literacy_acquisition_kernel__balanced_literacy_reading, theater_ratio, 10, 0.38).
narrative_ontology:measurement(lite_tr_t15, literacy_acquisition_kernel__balanced_literacy_reading, theater_ratio, 15, 0.42).
narrative_ontology:measurement(lite_tr_t20, literacy_acquisition_kernel__balanced_literacy_reading, theater_ratio, 20, 0.46).
narrative_ontology:measurement(lite_tr_t25, literacy_acquisition_kernel__balanced_literacy_reading, theater_ratio, 25, 0.5).
narrative_ontology:measurement(lite_tr_t30, literacy_acquisition_kernel__balanced_literacy_reading, theater_ratio, 30, 0.53).
narrative_ontology:measurement(lite_tr_t35, literacy_acquisition_kernel__balanced_literacy_reading, theater_ratio, 35, 0.55).

% Extraction over time
narrative_ontology:measurement(lite_be_t0, literacy_acquisition_kernel__balanced_literacy_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(lite_be_t5, literacy_acquisition_kernel__balanced_literacy_reading, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(lite_be_t10, literacy_acquisition_kernel__balanced_literacy_reading, base_extractiveness, 10, 0.52).
narrative_ontology:measurement(lite_be_t15, literacy_acquisition_kernel__balanced_literacy_reading, base_extractiveness, 15, 0.55).
narrative_ontology:measurement(lite_be_t20, literacy_acquisition_kernel__balanced_literacy_reading, base_extractiveness, 20, 0.57).
narrative_ontology:measurement(lite_be_t25, literacy_acquisition_kernel__balanced_literacy_reading, base_extractiveness, 25, 0.6).
narrative_ontology:measurement(lite_be_t30, literacy_acquisition_kernel__balanced_literacy_reading, base_extractiveness, 30, 0.6).
narrative_ontology:measurement(lite_be_t35, literacy_acquisition_kernel__balanced_literacy_reading, base_extractiveness, 35, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(lite_su_t0, literacy_acquisition_kernel__balanced_literacy_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(lite_su_t5, literacy_acquisition_kernel__balanced_literacy_reading, suppression_requirement, 5, 0.33).
narrative_ontology:measurement(lite_su_t10, literacy_acquisition_kernel__balanced_literacy_reading, suppression_requirement, 10, 0.37).
narrative_ontology:measurement(lite_su_t15, literacy_acquisition_kernel__balanced_literacy_reading, suppression_requirement, 15, 0.41).
narrative_ontology:measurement(lite_su_t20, literacy_acquisition_kernel__balanced_literacy_reading, suppression_requirement, 20, 0.44).
narrative_ontology:measurement(lite_su_t25, literacy_acquisition_kernel__balanced_literacy_reading, suppression_requirement, 25, 0.47).
narrative_ontology:measurement(lite_su_t30, literacy_acquisition_kernel__balanced_literacy_reading, suppression_requirement, 30, 0.5).
narrative_ontology:measurement(lite_su_t35, literacy_acquisition_kernel__balanced_literacy_reading, suppression_requirement, 35, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(literacy_acquisition_kernel__balanced_literacy_reading, identity_coordination).
narrative_ontology:affects_constraint(literacy_acquisition_kernel__balanced_literacy_reading, phonics_reading).
narrative_ontology:affects_constraint(literacy_acquisition_kernel__balanced_literacy_reading, whole_language_reading).
narrative_ontology:affects_constraint(literacy_acquisition_kernel__balanced_literacy_reading, structured_literacy_reading).

% DUAL FORMULATION NOTE:
% The literacy-acquisition kernel decomposes into four structurally distinct constraints — phonics_reading, whole_language_reading, structured_literacy_reading, and this file (balanced_literacy_reading) — because each reading instantiates a different instructional arrangement with a different epsilon, different beneficiary/victim structure, and different enforcement profile. The colloquial label balanced literacy names only this reading. The upstream/downstream structure runs through empirical standing: the phonics_reading's evidence base (National Reading Panel forward) is the upstream claim whose resurgence forced this reading's late-interval drift, and whole_language_reading is the downstream ancestor whose institutional base this reading absorbed while changing its label. Each family member links to the others through network.affects_constraints; the epsilon values differ across the family because each reading's arrangement extracts from different seats at different rates, not because one constraint is measured different ways.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
