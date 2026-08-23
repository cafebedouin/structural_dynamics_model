% ============================================================================
% CONSTRAINT STORY: reading_acquisition_mechanism__whole_language_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_reading_acquisition_mechanism__whole_language_reading, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: reading_acquisition_mechanism__whole_language_reading
 *   human_readable: Whole-Language Early Literacy Regime: Implicit Decoding Through Immersion in Authentic Texts
 *   domain: educational psychology/literacy pedagogy/cognitive science
 *
 * SUMMARY:
 *   Whole language holds that reading acquisition is continuous with oral
 *   language development: immerse children in meaningful, authentic texts and
 *   decoding emerges implicitly, so systematic letter-sound sequences,
 *   decodable readers, and skills worksheets are at best unnecessary and at
 *   worst harmful. Operationalized from the late 1960s onward (Goodman's
 *   psycholinguistic guessing model, Smith's natural-learning argument), the
 *   arrangement spread through teacher-education programs, state frameworks,
 *   and district adoptions until it was the default early-literacy orthodoxy
 *   of the 1990s, with districts removing phonics materials from approved
 *   lists and retraining staff away from explicit instruction. The resistance
 *   record is unusually well documented: the reading wars, Chall's surveys,
 *   the 2000 National Reading Panel, Decoding Dyslexia organizing, and the
 *   recent wave of science-of-reading statutes. KEY AGENTS (by structural
 *   relationship): struggling_readers: Primary target (powerless/trapped) --
 *   children who do not infer the alphabetic code implicitly and are carried
 *   forward with the gap attributed to development;
 *   parents_of_struggling_readers: Secondary target (organized/constrained)
 *   -- purchase the omitted instruction privately and organize for statutory
 *   change; classroom_teachers: Dual-positioned beneficiary
 *   (moderate/identity_locked) -- collect autonomy and professional meaning,
 *   absorb downstream failure and blame; teacher_education_faculty: Primary
 *   beneficiary and capture seat (institutional/identity_locked) --
 *   authority, tuition, royalties, and consulting ride on the premise;
 *   literacy_publishers: Beneficiary-arbitrageur (institutional/arbitrage) --
 *   sell whatever materials the prevailing framework demands;
 *   district_curriculum_leadership: Agenda setter (institutional/constrained)
 *   -- adopt frameworks and enforce material exclusions; proficient_readers:
 *   Incidental beneficiaries (powerless/trapped) -- the roughly two-thirds
 *   for whom the implicit bet pays off; taxpayers: Diffuse payer
 *   (powerless/generational) -- fund the remediation tail;
 *   reading_scientists: Excluded voices (organized/analytical) -- evidence
 *   producers outside curriculum decisions for two decades;
 *   national_reading_panel: Analytical observer (analytical/analytical) --
 *   the evidentiary anchor of the corrective coalition. FAMILY NOTE: the
 *   colloquial label 'the reading wars' decomposes per the epsilon-invariance
 *   principle into three structurally distinct constraints sharing the kernel
 *   reading_acquisition_mechanism. This file authors ONLY the whole-language
 *   reading as a clean constraint with a single stable epsilon; the phonics
 *   reading (lowest epsilon in the family, strongest empirical corroboration)
 *   and the balanced-literacy reading (intermediate) are separate stories,
 *   linked via network.affects_constraints, with the upstream phonics
 *   evidence historically cited against this reading's premises. The
 *   claim/metric independence rule is honored: claimed_type states what I
 *   judge structurally true (tangled_rope), and the metrics state what I
 *   judge descriptively true of the arrangement's operation, tuned to neither
 *   each other nor any predicted engine output.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(reading_acquisition_mechanism__whole_language_reading, 0.65).
domain_priors:suppression_score(reading_acquisition_mechanism__whole_language_reading, 0.5).
domain_priors:theater_ratio(reading_acquisition_mechanism__whole_language_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(reading_acquisition_mechanism__whole_language_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(reading_acquisition_mechanism__whole_language_reading, suppression_requirement, 0.5).
narrative_ontology:constraint_metric(reading_acquisition_mechanism__whole_language_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(reading_acquisition_mechanism__whole_language_reading, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(reading_acquisition_mechanism__whole_language_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(reading_acquisition_mechanism__whole_language_reading, tangled_rope).
narrative_ontology:human_readable(reading_acquisition_mechanism__whole_language_reading, "Whole-Language Early Literacy Regime: Implicit Decoding Through Immersion in Authentic Texts").
narrative_ontology:topic_domain(reading_acquisition_mechanism__whole_language_reading, "educational psychology/literacy pedagogy/cognitive science").

domain_priors:requires_active_enforcement(reading_acquisition_mechanism__whole_language_reading).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(reading_acquisition_mechanism__whole_language_reading, classroom_teachers).
narrative_ontology:constraint_beneficiary(reading_acquisition_mechanism__whole_language_reading, teacher_education_faculty).
narrative_ontology:constraint_beneficiary(reading_acquisition_mechanism__whole_language_reading, literacy_publishers).
narrative_ontology:constraint_beneficiary(reading_acquisition_mechanism__whole_language_reading, proficient_readers).
narrative_ontology:constraint_victim(reading_acquisition_mechanism__whole_language_reading, struggling_readers).
narrative_ontology:constraint_victim(reading_acquisition_mechanism__whole_language_reading, parents_of_struggling_readers).
narrative_ontology:constraint_victim(reading_acquisition_mechanism__whole_language_reading, taxpayers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(reading_acquisition_mechanism__whole_language_reading, classroom_teachers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Children in early grades whose first systematic encounter with written English is immersion in authentic books, writing workshops, and invented spelling, with letter-sound patterns left to inference. Roughly a third do not infer the alphabetic code on schedule; they are described as developing at their own pace and moved along with the class. What flows to them: engaging read-alouds, wide exposure, and a widening gap between word-recognition skill and grade-level text. Exit: none -- attendance is compulsory and the instructional approach follows the district's adopted framework wherever they enroll.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__whole_language_reading, struggling_readers, payer,
    powerless, biographical, trapped, national).

% Families who watch a child fall behind and are told the child needs more time, more books, or maturity. Those with resources purchase private tutoring in explicit phonics -- often precisely the instruction the classroom omits -- and advocacy networks such as Decoding Dyslexia chapters press for universal screening and structured-literacy laws. Exit: partial -- tutoring, private school, or homeschooling at significant cost; changing the district's approach runs through years-long board politics.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__whole_language_reading, parents_of_struggling_readers, payer,
    organized, biographical, constrained, national).

% Elementary teachers trained in constructivist literacy methods. They gain freedom from scripted sequences: choosing literature, designing units, conferring with young readers, and being evaluated on classroom culture rather than program fidelity. Many describe the approach as the reason they teach. The same teachers carry the burden when children stall: they absorb blame, run interventions they were never trained to deliver, and face the choice between the methods that constitute their professional identity and the explicit instruction some students visibly need. Leaving the identity means conceding that years of practice caused harm.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__whole_language_reading, classroom_teachers, beneficiary,
    moderate, biographical, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(reading_acquisition_mechanism__whole_language_reading, classroom_teachers, payer).

% Education-school professors, literacy leaders, and consultants whose courses, textbooks, and keynote circuits transmit the constructivist framework to each new cohort of teachers. Research programs, royalties, departmental standing, and a multi-billion-dollar professional-development circuit rest on the premise that reading develops naturally and explicit skills instruction is misguided. Revision would unravel curricula built over decades together with the authority that rests on them.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__whole_language_reading, teacher_education_faculty, beneficiary,
    institutional, generational, identity_locked, national).

% Educational publishers supplying the classroom libraries, big books, leveled readers, and workshop kits the approach consumes; trade-book adoption generates recurring purchases at scale, and controlled decodable series sit awkwardly outside the catalog logic. Publishers follow demand rather than doctrine: as state mandates shift toward systematic phonics, the same firms retool catalogs and sell the replacement materials.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__whole_language_reading, literacy_publishers, beneficiary,
    institutional, biographical, arbitrage, global).

% District curriculum directors and state-framework committees that adopt literacy frameworks, approve materials, and set the evaluation criteria teachers answer to. During the movement's ascendance they struck phonics workbooks from approved lists and retrained staff; reversing course now means unwinding contracts, retraining a workforce, and defending the reversal politically.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__whole_language_reading, district_curriculum_leadership, agenda_setter,
    institutional, biographical, constrained, regional).

% The majority of children who crack the code with modest support -- incidental exposure, home teaching, a bit of informal letter-sound help -- and flourish in literature-rich rooms. For them the approach delivers what it promises: motivation, vocabulary, and volume of reading. They do not know they are the ones the bet paid off for.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__whole_language_reading, proficient_readers, beneficiary,
    powerless, biographical, trapped, national).

% Residents funding school budgets, special-education placements, grade-retention costs, and adult-literacy programs that absorb readers the early grades did not equip. The bills surface years later, far from the classroom decision that generated them, with no mechanism connecting the invoice to the pedagogy.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__whole_language_reading, taxpayers, payer,
    powerless, generational, trapped, national).

% Cognitive psychologists, speech-language pathologists, and learning-disability researchers whose converging findings on alphabetic coding and phonological awareness sat outside the curriculum conversation for roughly two decades. Their papers circulated in journals the practitioner field did not read; their testimony reached policy slowly, largely after the instructional bet had been placed for a generation of students.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__whole_language_reading, reading_scientists, excluded,
    organized, generational, analytical, national).

% The congressionally convened panel that synthesized experimental reading research in 2000, finding strong evidence for systematic phonics instruction. Its report supplied the evidentiary anchor for subsequent U.S. legislation and for parallel reviews in England and Australia, shifting the terms on which districts could defend the incumbent approach.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__whole_language_reading, national_reading_panel, observer,
    analytical, immediate, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(reading_acquisition_mechanism__whole_language_reading, teacher_education_faculty).
narrative_ontology:fixing_cost_class(reading_acquisition_mechanism__whole_language_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Organizes early-literacy teaching across thousands of classrooms around shared authentic texts, student-selected reading, and integrated language arts, giving the profession a common culture and solving the engagement and coherence problem that drill-heavy, decontextualized basals created -- without prescriptive scripts.
% TRANSFER_FUNCTION: Moves instructional hours from explicit letter-sound instruction toward literature engagement; moves the risk of decoding failure from the instructional system onto individual children, reframed as developmental variation; and moves the eventual remediation bill onto families, district budgets, and adult-services systems.
% ABSENT_VOICES: Struggling children have no seat in any curriculum forum; reading scientists were structurally outside adoption decisions for two decades; parents of failing readers arrived only after failure became visible. The adoption-era unanimity reflected who was in the room -- publisher sales channels, district offices, and education-school faculties -- not settled evidence, and the consensus-provenance check should treat it accordingly.
% DISAPPEARANCE_RATIONALE: If the arrangement vanished overnight, every early-grade classroom would need an explicit code pathway within a year; teacher-education syllabi, licensure exams, publisher catalogs, and district adoptions would all reorganize; the cohort then passing through would receive different instruction; and the tutoring and intervention industries built on the shortfall would contract. A large apparatus reproduces this arrangement annually, so its removal rearranges rather than evaporates.
% FOUNDING_PROBLEM: Mid-century American reading instruction was widely experienced as joyless and fragmented: round-robin recitation, workbook drills, and controlled basals that stripped language of meaning, producing children who could call words without wanting to read. Whole language was built to make reading instruction meaningful, motivating, and continuous with real language use.
% FOUNDING_PROBLEM_CORROBORATION: Attestation from outside the benefiting parties: Jeanne Chall's classroom surveys and subsequent literacy historiography corroborate that mid-century skills instruction was frequently alienating and decontextualized, and even the arrangement's sharpest critics grant the engagement problem was real. The National Reading Panel and the English and Australian reviews corroborate the second half -- that the founders' solution premise failed -- while no attestation from inside the whole-language movement is treated as probative, since the movement is the benefiting set.
narrative_ontology:disappearance_verdict(reading_acquisition_mechanism__whole_language_reading, world_rearranges).
narrative_ontology:founding_problem_status(reading_acquisition_mechanism__whole_language_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(reading_acquisition_mechanism__whole_language_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(reading_acquisition_mechanism__whole_language_reading, 'none', 1).
narrative_ontology:epsilon_provenance(reading_acquisition_mechanism__whole_language_reading, 0.65, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(reading_acquisition_mechanism__whole_language_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(reading_acquisition_mechanism__whole_language_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(reading_acquisition_mechanism__whole_language_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.65: the arrangement delivers genuine value to the reading majority while imposing concentrated, deferred costs on a minority who are told their failure is developmental; the costs are large (years of blocked instruction, private remediation, adult-literacy tail) and systematically shifted off the instructional system's own ledger. Suppression 0.50 (current): enforcement ran through adoption lists, banned-worksheet norms, hiring filters, and ed-school gatekeeping, peaking during orthodoxy consolidation and decaying after the corrective statutes; suppression is authored raw and unscaled -- only extractiveness is scaled by the engine. Theater_ratio 0.48: literature engagement is real activity, but a growing share of the arrangement's maintenance is ritual (workshop performances, portfolio ceremonies, boundary-marking against 'drill'), rising as the evidential foundation eroded. Accessibility_collapse 0.42: alternatives never collapsed -- phonics persisted in parochial schools, tutoring markets, and homeschooling -- but the sanctioned pathway inside public systems was closed. Resistance 0.62: sustained and eventually successful resistance from scientists, parents, and legislators. Temporal shape: extraction climbs with orthodoxy (1967-1999), peaks as enforcement matures (1999-2007), then plateaus elevated rather than falling after formal retreat, because the remediation debt of instructed cohorts persists; the suppression_requirement series is authored deliberately to trace enforcement-capacity build-up and decay (rise through consolidation, fall after statutes), which is the sanctioned use of that series. All three metric series run on one shared eight-point grid (1967-2025) so the engine samples every metric at every examined point; the trajectories are arced, not cyclical, so no oscillation mechanism is claimed. Receipt surface: gain_flow names teacher_education_faculty -- adoption-contingent tuition, royalties, and consulting concentrate there more than any other seat; publishers approach diffuse because they arbitrage across paradigms, but the ed-school complex's revenues are contingent on this specific premise holding.
 *
 * PERSPECTIVAL GAP:
 *   Same rules, different worlds. From the teacher-education and publisher seats the arrangement is a functioning professional ecosystem that trains, employs, and supplies; from the classroom-teacher seat it is simultaneously liberation (autonomy, meaning, craft pride) and trap (when a child does not take off, the methods constituting the teacher's identity are the visible cause); from the struggling-reader seat it is a wall met at age six, followed by years of being described rather than taught. The engine computes this divergence per seat from the structural data; nothing in the authored claim adjudicates it, and the claimed tangled_rope is precisely the shape in which all three experiences are simultaneously accurate.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations (classroom_teachers, teacher_education_faculty, literacy_publishers, proficient_readers) drive those seats toward the subsidized end; victim declarations (struggling_readers, parents_of_struggling_readers, taxpayers) drive them toward the full-target end, amplified for the children by trapped exit -- a six-year-old cannot leave the adopted framework, and identity lock keeps the adult professionals from exiting even when they see the failures. Directionality override: the moderate power atom (held only by classroom_teachers in this story) is lifted from a derived near-beneficiary value (~0.15) to 0.30 because teachers bear remediation labor, intervention loads, and reputational risk that the pure-beneficiary derivation from their role declaration misses; they are net collectors but not insulated collectors. District_curriculum_leadership derives an intermediate directionality: they administer and enforce without concentrating the largest share of gains. The excluded scientist seat and the analytical panel seat sit near symmetric by construction -- they neither collect nor pay.
 *
 * MANDATROPHY ANALYSIS:
 *   The contested founding-problem status is what keeps this story out of both mislabels. Calling the arrangement a snare erases the genuine coordination -- literature-rich, meaning-centered classrooms demonstrably serve the majority of readers and solved a real motivational problem the drill-heavy basals created. Calling it a rope erases the enforced exclusion of explicit instruction, the identifiable and concentrated victim class, and the suppression machinery documented in the measurement series. Because the founding problem (alienating, decontextualized drill) was real and remains partly live, the arrangement is not a clean zombie; because its operative premise has been substantially empirically challenged, it cannot be certified as healthy coordination either. Tangled rope carrying accumulating remediation debt is the honest middle. Watch-item: founding_problem_status=contested crossed with disappearance_verdict=world_rearranges is exactly the mismatch cell the R5 consumer flags; if the omega on revision capacity resolves toward rebranding-without-revision, expect drift toward piton with rising theater_ratio.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_committer_delta,
    'This constraint is one reading of kernel reading_acquisition_mechanism (reading: whole_language_reading). If a sibling reading were instantiated instead -- reading_acquisition_mechanism__phonics_reading or reading_acquisition_mechanism__balanced_literacy_reading -- which structural elements change: victim sets, enforcement requirements, and the location of extraction?',
    'Comparative generation of the sibling stories as separate constraint files with their own epsilon, beneficiaries, and victims, linked through network.affects_constraints; the engine computes cross-reading classification divergence.',
    'The phonics reading concentrates its victim set narrowly (learners failed by poorly implemented code instruction) and requires little enforcement; the balanced-literacy reading distributes costs more evenly; this whole-language reading carries the family''s widest victim set and heaviest enforcement load. Resolving the contest determines which reading the corpus treats as the kernel''s live instantiation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_committer_delta, conceptual, 'Committer structure: one of three rival readings of the reading-acquisition kernel.').

omega_variable(
    implicit_sufficiency_empirical_status,
    'Can grapheme-phoneme decoding reliably emerge from immersive exposure to authentic text without explicit systematic instruction, for typical learners and struggling learners alike?',
    'Converging randomized and longitudinal evidence: the National Reading Panel meta-analyses (2000), Ehri and colleagues'' systematic reviews, the England Rose Review rollout, and state natural experiments such as the Mississippi literacy reforms.',
    'A decisively negative answer collapses the genuine-coordination half of the tangled-rope profile toward snare (the literature-engagement story becomes cover for enforced omission); a qualified positive answer restores rope-like standing for part of the arrangement.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(implicit_sufficiency_empirical_status, empirical, 'Whether the reading''s foundational premise survives the empirical record.').

omega_variable(
    disproportionality_attribution,
    'How much of struggling readers'' failure under whole-language instruction is attributable to the absence of explicit code instruction, versus home-literacy environment and socioeconomic confounds?',
    'Within-school comparisons of classrooms differing in instructional regime, and difference-in-differences designs around state phonics mandates and screening laws.',
    'Higher attribution raises the victim-side weight for the struggling_reader seat and pushes the computed classification toward snare; lower attribution leaves the majority of measured extraction attributable elsewhere and softens the asymmetry.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(disproportionality_attribution, empirical, 'Attribution of the victim class''s losses to the arrangement itself.').

omega_variable(
    suppression_structural_vs_internalized,
    'Is the suppression that held explicit phonics out of classrooms primarily structural (district material bans, adoption lists, hiring filters, publisher availability, ed-school coursework) or internalized (constructivist professional identity rendering explicit instruction unthinkable even where permitted)?',
    'Post-statute practice trajectories: if formerly whole-language classrooms revert to cueing-based practice once enforcement attention lapses, the internalized share dominates; if practice tracks mandates durably, the structural share dominates.',
    'Internalized-dominant suppression persists after legal remedies and keeps effective suppression above the structural measure; structural-dominant suppression falls quickly with adoption-list changes.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structural_vs_internalized, empirical, 'Mechanism split of the arrangement''s suppressive force.').

omega_variable(
    identity_lock_revision_capacity,
    'Can teacher-education faculties whose authority, syllabi, and publications rest on the constructivist premise revise that premise in place, or does identity lock force rebranding (cueing relabeled as flexible or multimodal) while the operative classroom practice persists?',
    'Track ed-school course content, licensure exam blueprints, and publisher catalogs after state science-of-reading mandates: does explicit GPC instruction enter coursework and assessments, or only marketing language?',
    'Rebranding-without-revision predicts drift toward piton (theatrical maintenance of a degraded arrangement); genuine revision predicts a scaffold-like transition toward integrated practice.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_revision_capacity, conceptual, 'Persistence question for the identity-locked beneficiary seats.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(reading_acquisition_mechanism__whole_language_reading, 1967, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(read_tr_t1967, reading_acquisition_mechanism__whole_language_reading, theater_ratio, 1967, 0.12).
narrative_ontology:measurement(read_tr_t1975, reading_acquisition_mechanism__whole_language_reading, theater_ratio, 1975, 0.16).
narrative_ontology:measurement(read_tr_t1983, reading_acquisition_mechanism__whole_language_reading, theater_ratio, 1983, 0.24).
narrative_ontology:measurement(read_tr_t1991, reading_acquisition_mechanism__whole_language_reading, theater_ratio, 1991, 0.34).
narrative_ontology:measurement(read_tr_t1999, reading_acquisition_mechanism__whole_language_reading, theater_ratio, 1999, 0.4).
narrative_ontology:measurement(read_tr_t2007, reading_acquisition_mechanism__whole_language_reading, theater_ratio, 2007, 0.43).
narrative_ontology:measurement(read_tr_t2016, reading_acquisition_mechanism__whole_language_reading, theater_ratio, 2016, 0.45).
narrative_ontology:measurement(read_tr_t2025, reading_acquisition_mechanism__whole_language_reading, theater_ratio, 2025, 0.48).

% Extraction over time
narrative_ontology:measurement(read_be_t1967, reading_acquisition_mechanism__whole_language_reading, base_extractiveness, 1967, 0.32).
narrative_ontology:measurement(read_be_t1975, reading_acquisition_mechanism__whole_language_reading, base_extractiveness, 1975, 0.4).
narrative_ontology:measurement(read_be_t1983, reading_acquisition_mechanism__whole_language_reading, base_extractiveness, 1983, 0.5).
narrative_ontology:measurement(read_be_t1991, reading_acquisition_mechanism__whole_language_reading, base_extractiveness, 1991, 0.63).
narrative_ontology:measurement(read_be_t1999, reading_acquisition_mechanism__whole_language_reading, base_extractiveness, 1999, 0.68).
narrative_ontology:measurement(read_be_t2007, reading_acquisition_mechanism__whole_language_reading, base_extractiveness, 2007, 0.71).
narrative_ontology:measurement(read_be_t2016, reading_acquisition_mechanism__whole_language_reading, base_extractiveness, 2016, 0.69).
narrative_ontology:measurement(read_be_t2025, reading_acquisition_mechanism__whole_language_reading, base_extractiveness, 2025, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(read_su_t1967, reading_acquisition_mechanism__whole_language_reading, suppression_requirement, 1967, 0.25).
narrative_ontology:measurement(read_su_t1975, reading_acquisition_mechanism__whole_language_reading, suppression_requirement, 1975, 0.34).
narrative_ontology:measurement(read_su_t1983, reading_acquisition_mechanism__whole_language_reading, suppression_requirement, 1983, 0.47).
narrative_ontology:measurement(read_su_t1991, reading_acquisition_mechanism__whole_language_reading, suppression_requirement, 1991, 0.58).
narrative_ontology:measurement(read_su_t1999, reading_acquisition_mechanism__whole_language_reading, suppression_requirement, 1999, 0.62).
narrative_ontology:measurement(read_su_t2007, reading_acquisition_mechanism__whole_language_reading, suppression_requirement, 2007, 0.57).
narrative_ontology:measurement(read_su_t2016, reading_acquisition_mechanism__whole_language_reading, suppression_requirement, 2016, 0.53).
narrative_ontology:measurement(read_su_t2025, reading_acquisition_mechanism__whole_language_reading, suppression_requirement, 2025, 0.5).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(reading_acquisition_mechanism__whole_language_reading, identity_coordination).
narrative_ontology:affects_constraint(reading_acquisition_mechanism__whole_language_reading, reading_acquisition_mechanism__phonics_reading).
narrative_ontology:affects_constraint(reading_acquisition_mechanism__whole_language_reading, reading_acquisition_mechanism__balanced_literacy_reading).

% DUAL FORMULATION NOTE:
% Constraint family decomposition per the epsilon-invariance principle: the natural-language concept 'how reading should be taught' (and the label 'the reading wars') covers three structurally distinct claims with materially different epsilon values and victim sets. Upstream: reading_acquisition_mechanism__phonics_reading carries the family's highest empirical confidence (converging meta-analytic and neurocognitive evidence) and lowest extraction; its results were historically cited AGAINST this reading's premises, creating the family's principal influence edge. Downstream: reading_acquisition_mechanism__balanced_literacy_reading arose partly as a compromise formation absorbing pressure from both sides. This file (whole-language reading) is the family's contested, most heavily enforced member; each member is authored as a separate story with its own beneficiaries, victims, and stable epsilon, linked bidirectionally through network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(reading_acquisition_mechanism__whole_language_reading, moderate, 0.3).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
