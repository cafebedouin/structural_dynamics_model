% ============================================================================
% CONSTRAINT STORY: reading_acquisition_legitimacy__balanced_literacy_integration
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_reading_acquisition_legitimacy__balanced_literacy_integration, []).

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
 *   constraint_id: reading_acquisition_legitimacy__balanced_literacy_integration
 *   human_readable: Balanced Literacy Integration Doctrine (Reading Acquisition Legitimacy Kernel)
 *   domain: education policy/cognitive science/pedagogy
 *
 * SUMMARY:
 *   Balanced literacy is the mediating reading of the
 *   reading_acquisition_legitimacy kernel: the claim that legitimate
 *   instruction must give real weight to both explicit code teaching and
 *   authentic literature. It emerged in the early 1990s as the settlement of
 *   the reading wars and was institutionalized through teacher-preparation
 *   programs, a small set of influential curriculum publishers, and a
 *   national coaching infrastructure. Its structure is genuinely mixed:
 *   decodable and authentic texts, direct instruction toggling with
 *   facilitation, phonics intervention alongside guided reading for
 *   strugglers. It solved a real coordination problem because no faction had
 *   to capitulate. The same structure carried asymmetric costs: the 'balance'
 *   in practice tilted meaning-first, three-cueing persisted inside it, and
 *   the students who could not induce the alphabetic code incidentally,
 *   disproportionately dyslexic children, paid in years of instruction that
 *   did not teach them to read, while publishers and preparation programs
 *   collected the rents of legitimacy. Between roughly 2018 and 2025 the
 *   science-of-reading movement, parent advocacy, and state statute attacked
 *   the arrangement's empirical premises; flagship curricula were revised and
 *   cueing was banned in most states. This story instantiates only the
 *   balanced_literacy_integration reading; the phonics_decoding_primacy,
 *   whole_language_meaning_primacy, and structured_literacy_remediation
 *   readings are separate constraints in the same kernel family. Claim and
 *   metrics are authored independently: the claimed type is what the
 *   structure shows (coordination plus asymmetric extraction under active
 *   enforcement), and the metrics describe how the arrangement actually
 *   operated.
 *
 * KEY AGENTS:
 *   - curriculum_publishers: Primary agenda-setter and beneficiary (institutional/arbitrage) — authors and defends the balanced curricula, collects adoption revenue
 *   - education_school_faculty: Primary beneficiary (institutional/identity_locked) — trained the workforce in meaning-first methods; identity fused with the tradition
 *   - literacy_coaching_industry: Secondary beneficiary (organized/constrained) — professional development and consulting built on the model
 *   - incumbent_whole_language_teachers: Beneficiary (moderate/identity_locked) — practice legitimized and resourced
 *   - school_districts: Administrator (institutional/constrained) — adopted, mandated, and evaluated; collected defensibility while bearing costs
 *   - struggling_readers: Primary target (powerless/trapped) — bear the diluted code instruction
 *   - parents_of_struggling_readers: Target turned organizer (organized/constrained) — tutoring costs and advocacy labor
 *   - evidence_oriented_teachers: Target (moderate/constrained) — marginalized for research-aligned practice
 *   - reading_scientists: Analytical observer (institutional/analytical) — documented the gap between evidence and practice
 *   - state_legislatures: Excluded seat through the arrangement's dominance; late-arriving external dismantler
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(reading_acquisition_legitimacy__balanced_literacy_integration, 0.55).
domain_priors:suppression_score(reading_acquisition_legitimacy__balanced_literacy_integration, 0.5).
domain_priors:theater_ratio(reading_acquisition_legitimacy__balanced_literacy_integration, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(reading_acquisition_legitimacy__balanced_literacy_integration, extractiveness, 0.55).
narrative_ontology:constraint_metric(reading_acquisition_legitimacy__balanced_literacy_integration, suppression_requirement, 0.5).
narrative_ontology:constraint_metric(reading_acquisition_legitimacy__balanced_literacy_integration, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(reading_acquisition_legitimacy__balanced_literacy_integration, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(reading_acquisition_legitimacy__balanced_literacy_integration, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(reading_acquisition_legitimacy__balanced_literacy_integration, tangled_rope).
narrative_ontology:human_readable(reading_acquisition_legitimacy__balanced_literacy_integration, "Balanced Literacy Integration Doctrine (Reading Acquisition Legitimacy Kernel)").
narrative_ontology:topic_domain(reading_acquisition_legitimacy__balanced_literacy_integration, "education policy/cognitive science/pedagogy").

domain_priors:requires_active_enforcement(reading_acquisition_legitimacy__balanced_literacy_integration).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(reading_acquisition_legitimacy__balanced_literacy_integration, 'bd709ed8-0dbf-4e86-a1b5-e824065a18a6').
narrative_ontology:cs_kernel_codification('bd709ed8-0dbf-4e86-a1b5-e824065a18a6', distributed).
narrative_ontology:cs_authority_grounding('bd709ed8-0dbf-4e86-a1b5-e824065a18a6', lineage).
narrative_ontology:cs_interpretation_layer_present('bd709ed8-0dbf-4e86-a1b5-e824065a18a6').
narrative_ontology:cs_reading_relation('bd709ed8-0dbf-4e86-a1b5-e824065a18a6', reading_acquisition_legitimacy__phonics_decoding_primacy, influences).
narrative_ontology:cs_reading_relation('bd709ed8-0dbf-4e86-a1b5-e824065a18a6', reading_acquisition_legitimacy__whole_language_meaning_primacy, influences).
narrative_ontology:cs_reading_relation('bd709ed8-0dbf-4e86-a1b5-e824065a18a6', reading_acquisition_legitimacy__structured_literacy_remediation, influences).
narrative_ontology:cs_axiom('bd709ed8-0dbf-4e86-a1b5-e824065a18a6', foundational, legitimate_instruction_requires_code_and_meaning).
narrative_ontology:cs_axiom_status(legitimate_instruction_requires_code_and_meaning, holdable).
narrative_ontology:cs_axiom_grounding('bd709ed8-0dbf-4e86-a1b5-e824065a18a6', legitimate_instruction_requires_code_and_meaning, empirically_contingent).
narrative_ontology:cs_axiom('bd709ed8-0dbf-4e86-a1b5-e824065a18a6', secondary, teacher_judgment_over_scripted_programs).
narrative_ontology:cs_axiom_status(teacher_judgment_over_scripted_programs, holdable).
narrative_ontology:cs_axiom_grounding('bd709ed8-0dbf-4e86-a1b5-e824065a18a6', teacher_judgment_over_scripted_programs, conventional).
narrative_ontology:cs_reference_frame('bd709ed8-0dbf-4e86-a1b5-e824065a18a6', meaning_code_equilibrium).
narrative_ontology:cs_drift_state('bd709ed8-0dbf-4e86-a1b5-e824065a18a6', post_science_of_reading_reckoning, gap(axiom_overriding, substantial, true)).
narrative_ontology:cs_created_at('bd709ed8-0dbf-4e86-a1b5-e824065a18a6', '2026-06-12T00:00:00Z').
narrative_ontology:cs_kernel_id(reading_acquisition_legitimacy__balanced_literacy_integration, reading_acquisition_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(reading_acquisition_legitimacy__balanced_literacy_integration, curriculum_publishers).
narrative_ontology:constraint_beneficiary(reading_acquisition_legitimacy__balanced_literacy_integration, education_school_faculty).
narrative_ontology:constraint_beneficiary(reading_acquisition_legitimacy__balanced_literacy_integration, literacy_coaching_industry).
narrative_ontology:constraint_beneficiary(reading_acquisition_legitimacy__balanced_literacy_integration, incumbent_whole_language_teachers).
narrative_ontology:constraint_victim(reading_acquisition_legitimacy__balanced_literacy_integration, struggling_readers).
narrative_ontology:constraint_victim(reading_acquisition_legitimacy__balanced_literacy_integration, parents_of_struggling_readers).
narrative_ontology:constraint_victim(reading_acquisition_legitimacy__balanced_literacy_integration, evidence_oriented_teachers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(reading_acquisition_legitimacy__balanced_literacy_integration, school_districts).
narrative_ontology:constraint_vindicates(reading_acquisition_legitimacy__balanced_literacy_integration, balanced_instruction_doctrine).
narrative_ontology:constraint_vindicates(reading_acquisition_legitimacy__balanced_literacy_integration, constructivist_learning_theory).
narrative_ontology:constraint_vindicates(reading_acquisition_legitimacy__balanced_literacy_integration, teacher_professional_autonomy_norm).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Author, publish, and defend the balanced literacy curricula, leveled-text systems, and assessments that define what 'balance' means in classrooms. Set the agenda through adoption catalogues, institute-run professional development, and public defense of cueing-based methods. Collect adoption revenue from districts nationwide; when the evidence turned, revised flagship products rather than exiting the market.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__balanced_literacy_integration, curriculum_publishers, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(reading_acquisition_legitimacy__balanced_literacy_integration, curriculum_publishers, beneficiary).

% Train the teachers who staff classrooms; for three decades the dominant preparation in reading methods descended from whole-language theory. Faculty careers, journals, and certification pipelines were built on the approach. Exit would mean disavowing the theoretical tradition their professional identities and published work rest on.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__balanced_literacy_integration, education_school_faculty, beneficiary,
    institutional, generational, identity_locked, national).

% Independent consultants, staff developers, and coaching networks deliver the professional development the model runs on: summer institutes, on-site coaching, leveled-library sales. Revenue tracks district adoption of the model; pivoting to a different pedagogy means rebuilding the client base from zero.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__balanced_literacy_integration, literacy_coaching_industry, beneficiary,
    organized, biographical, constrained, national).

% Classroom teachers trained in meaning-first methods whose existing practice was legitimized and resourced under the balanced banner. Many have taught this way for their whole careers; the method is fused with their sense of themselves as teachers of readers rather than deliverers of drills.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__balanced_literacy_integration, incumbent_whole_language_teachers, beneficiary,
    moderate, biographical, identity_locked, national).

% Adopt the curricula, mandate the training, and evaluate teachers against the model. Collect defensibility: 'balanced literacy' was a position no board member could easily attack. Also bear material costs: license fees, coaching contracts, and the later remediation bills when students fall behind.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__balanced_literacy_integration, school_districts, agenda_setter,
    institutional, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(reading_acquisition_legitimacy__balanced_literacy_integration, school_districts, beneficiary).

% Children whose instruction emphasizes guessing from pictures, context, and first letters alongside rich literature. The substantial minority who do not induce the alphabetic code incidentally receive the least explicit code instruction precisely when it matters most, and they cannot leave the classroom they are assigned to.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__balanced_literacy_integration, struggling_readers, payer,
    powerless, immediate, trapped, national).

% Families who discover their child is not learning to read and confront a system that attributes the failure to the child. Many pay for private evidence-based tutoring out of pocket; parent-led dyslexia advocacy organizations became the political force that eventually moved state legislatures.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__balanced_literacy_integration, parents_of_struggling_readers, payer,
    organized, biographical, constrained, national).

% Teachers who followed the reading research, taught systematic phonics, and were corrected by coaches and evaluators for outdated drill-and-kill practice. They bore professional marginalization inside a system their own evidence contradicted; exit meant leaving their districts or staying quiet.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__balanced_literacy_integration, evidence_oriented_teachers, payer,
    moderate, biographical, constrained, national).

% Cognitive psychologists and reading researchers whose work on decoding, orthographic mapping, and comprehension documented the gap between the reading science and classroom practice. They hold no adoption authority; their influence runs through journals, legislative testimony, and public journalism.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__balanced_literacy_integration, reading_scientists, observer,
    institutional, civilizational, analytical, national).

% Held formal authority over education but stayed out of curriculum for decades, treating pedagogy as professional territory and deferring to preparation programs and adoption committees. Entered the conversation only after parent advocacy made reading instruction a statutory question; now mandate evidence-based instruction and cueing bans, dismantling the arrangement from outside.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__balanced_literacy_integration, state_legislatures, excluded,
    institutional, generational, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(reading_acquisition_legitimacy__balanced_literacy_integration, curriculum_publishers).
narrative_ontology:fixing_cost_class(reading_acquisition_legitimacy__balanced_literacy_integration, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: After two decades of open conflict between whole-language and phonics factions, the arrangement gave districts, preparation programs, publishers, and classroom teachers a single framework both camps could accept: rich authentic literature and comprehension instruction alongside some explicit code teaching. It solved a real collective-action problem: no faction had to capitulate, curricula could be adopted, teachers could be trained at scale, and classrooms had a defensible daily structure.
% TRANSFER_FUNCTION: Moves adoption and professional-development revenue from districts to publishers and coaching networks; moves curricular authority toward meaning-first methods and the institutions that train them; moves instructional time toward literature, guided reading, and strategy instruction; and moves the cost of diluted code instruction onto the students least able to supply decoding on their own, plus their families' later tutoring bills.
% ABSENT_VOICES: Struggling readers themselves: children cannot sit on adoption committees, and their failure was routinely attributed to their own deficits rather than the instruction. Reading scientists were present in journals but excluded from curriculum decisions as 'reductionist.' State legislatures and democratic accountability were absent for decades because pedagogy was treated as professional territory, so the unanimity behind 'balance' partly reflected who was never in the room. Dyslexic parents were outside the conversation until they organized outside it.
% DISAPPEARANCE_RATIONALE: If the arrangement vanished overnight, districts would need replacement curricula and retraining, publishers would lose their flagship market, coaching networks would lose their client base, and classrooms would reorganize around whatever replaced it. This is not hypothetical: where state law forced structured-literacy replacements, adoption cycles reopened, materials were rewritten, and teacher preparation is being rewritten with them.
% FOUNDING_PROBLEM: The reading wars: whole-language classrooms left a substantial minority of children unable to decode, while phonics-only drill was accused of killing comprehension and the love of reading. Districts needed an end to institutional conflict and a framework that honored both decoding and meaning-making.
% FOUNDING_PROBLEM_CORROBORATION: The National Reading Panel (2000), a federal synthesis outside the beneficiary institutions, found systematic phonics instruction effective and the balance as practiced unsupported for struggling readers; state legislative findings and auditor reports in the 2020s reached the same conclusion; independent journalism (APM Reports) and cognitive-science researchers outside the education-school and publisher complex corroborate that the code-instruction half of the founding problem was answered and then deflected. The beneficiary institutions' own white papers dispute this, which is the cover-story side of the record.
narrative_ontology:disappearance_verdict(reading_acquisition_legitimacy__balanced_literacy_integration, world_rearranges).
narrative_ontology:founding_problem_status(reading_acquisition_legitimacy__balanced_literacy_integration, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(reading_acquisition_legitimacy__balanced_literacy_integration, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(reading_acquisition_legitimacy__balanced_literacy_integration, 'none', 1).
narrative_ontology:epsilon_provenance(reading_acquisition_legitimacy__balanced_literacy_integration, 0.55, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(reading_acquisition_legitimacy__balanced_literacy_integration_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(reading_acquisition_legitimacy__balanced_literacy_integration, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(reading_acquisition_legitimacy__balanced_literacy_integration_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.55: a real coordination core (both dimensions of reading honored, factions accommodated) carrying extraction that concentrates on the students least able to escape it — roughly the third of children who need explicit code instruction — plus adoption rents. Suppression 0.50: enforcement ran through adoption mandates, coach networks, and evaluation alignment rather than statute; it was real but never total, since private tutoring and some schools always existed outside it, and it is now eroding. Theater 0.45: 'balance' increasingly named a rhetorical position rather than a delivered ratio — leveled texts that did not decodably level, cueing presented as comprehension strategy, phonics 'infused' rather than taught — though genuine literature instruction and real small-group teaching ran throughout. Accessibility collapse 0.45: alternatives were expensive and professionally stigmatized but never unavailable. Resistance 0.72: parent-led dyslexia organizing, the science-of-reading movement, national journalism, and eventually statute in forty-plus states. The measurement series run on one shared grid (1990-2025, five-year points, every tracked metric at every point). Extractiveness and theater rise monotonically to a 2020 peak as rents layer onto coordination and 'balance' drifts from practice to label, then fall as statute and revised curricula bite. Suppression_requirement is authored because enforcement capacity genuinely changed: built through 2015, eroded after 2018. No cyclical pattern: the dynamics are accumulative drift followed by external shock, not oscillation.
 *
 * PERSPECTIVAL GAP:
 *   Seats diverge sharply. From the publisher and preparation-program seats the arrangement is a hard-won professional synthesis that ended a destructive war and honored the whole reader; from a dyslexic child's seat it is years of instruction that withheld the one thing they could not self-supply; from an evidence-oriented teacher's seat it is professional gaslighting, with coaches correcting the practice the research supports. Same-level divergence: incumbent and evidence-oriented teachers hold the same nominal power and face the same districts, but identity lock (the method is fused with who they are) versus evidential commitment produces opposite relationships to identical mandates. Inter-institutionally, publishers hold arbitrage (rebrand and revise), preparation programs hold identity lock, districts hold constrained administration, and legislatures held exclusion until they held statute: four institutional seats, four different experiences of the same framework. The engine computes these per-seat classifications from the structural data; the authored claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries sit near the subsidy end: publishers collect adoption revenue and hold rebrand exit (lowest d); preparation programs and incumbent teachers collect legitimacy and are identity-locked into it — for locked beneficiaries the lock amplifies rather than damps their position; coaches collect fees with constrained pivot. Districts sit near symmetric: they paid for materials and later remediation but collected defensibility and administrative coherence. Targets sit near the full-target end: struggling readers are powerless and trapped with no exit from assigned classrooms (highest d); parents bear tutoring costs and advocacy labor, with their organized advocacy partially damping the seat; evidence-oriented teachers bear marginalization with constrained exit. Reading scientists are the analytical seat near d=0.5. The derivation from beneficiary/victim declarations plus exit options produces these positions without correction; no directionality overrides are authored.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope classification does double work here. Reading the arrangement as pure rope (its self-presentation: a balanced synthesis) erases the identifiable victims — the struggling readers whose instruction was diluted and the families who paid twice, in taxes and tutoring. Reading it as pure snare (the polemical science-of-reading framing: a fraud that never taught reading) erases the real coordination achievement — the reading wars were genuinely destructive, meaning-making instruction has real value, and the settlement did align a national workforce. The founding problem — end the war, serve both dimensions — is contested: the code half was answered by evidence the arrangement deflected for two decades, which is the mandatrophy signature (the problem was solved and the arrangement persisted anyway, sustained by the institutions it legitimized), while the meaning half remains live. The persistence-after-solution pattern is exactly what the founding_problem_status x disappearance_verdict mismatch flags: status contested, verdict world_rearranges — arrangements still depend on it, but the founding warrant is half-spent.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'This story instantiates the balanced_literacy_integration reading of the reading_acquisition_legitimacy kernel. Would the sibling readings (phonics_decoding_primacy, whole_language_meaning_primacy) author a different epsilon and a different beneficiary/victim structure for the same classroom referent, and where exactly is the disagreement located?',
    'Side-by-side authoring of the sibling stories against the same referent arrangement, then comparison of seat classifications and epsilon values across the family.',
    'If the siblings author sharply different epsilon for the same referent, the kernel contest is live and the balanced reading''s claim to synthesize is itself the contested move; this story''s classification stays epsilon-invariant either way.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Committer structure of the kernel contest: one kernel, four readings, each a separate constraint.').

omega_variable(
    balance_design_vs_political_cover,
    'Was the balanced ratio a genuine instructional design principle, or a political formula that preserved whole-language practice under a new label?',
    'Classroom time-allocation studies and curriculum audits comparing the advertised balance to the delivered minutes of explicit, systematic code instruction per week.',
    'If cover, the arrangement''s coordination function shrinks toward rhetorical and its classification moves toward pure extraction; if genuine design, the mixed coordination-plus-extraction structure stands.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(balance_design_vs_political_cover, empirical, 'Whether the balance was designed instruction or a political ratio.').

omega_variable(
    struggling_reader_cost_concentration,
    'What share of the arrangement''s costs concentrates on dyslexic and struggling readers versus diffusing across all readers as mildly diluted instruction?',
    'Longitudinal reading-outcome data disaggregated by baseline decoding skill, linked to measured instructional exposure.',
    'Concentrated costs on powerless, trapped agents raise that seat''s effective burden sharply; diffuse costs would lower it and soften the extraction reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(struggling_reader_cost_concentration, empirical, 'How concentrated the costs are on the most vulnerable students.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (adoption mandates, budgets, evaluation systems, coaching gatekeeping) or internalized (teacher belief and professional identity formed in training)?',
    'Post-mandate trajectory in states that banned cueing: if three-cueing persists in classrooms after mandates lift, the internalized component is substantial.',
    'Internalized suppression travels with teachers across reforms and outlasts the enforcement machinery; structural suppression falls when mandates fall. The proportion determines how much of the measured suppression survives the current statutory dismantling.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural versus internalized suppression mechanism in teacher practice.').

omega_variable(
    ed_school_update_capacity,
    'Is education-school resistance to evidence-based revision identity fusion (a constructivist worldview constitutive of professional identity) or material interest (faculty lines, grants, journal ecosystems)?',
    'Track curricular and certification revision in preparation programs after incentive shifts, such as state certification requirements tied to reading-science coursework.',
    'Identity-locked institutions do not update on evidence, so the arrangement''s residue would outlast its enforcement; material-interest institutions update when incentives move, and the residue decays faster.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ed_school_update_capacity, empirical, 'Whether the preparation-program seat is identity-locked or merely materially interested.').

omega_variable(
    revised_curriculum_substance,
    'Do the 2020-2022 revised flagship curricula actually remove three-cueing instruction, or relabel and redistribute it?',
    'Content analysis of revised teacher materials against cueing markers (picture prompts, meaning-first guessing routines, leveled-text reliance), cross-checked against classroom observation.',
    'If relabeled, theater persists under new branding and the 2020-2025 declines in the measurement series overstate reform; if substantively removed, the declines are real.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(revised_curriculum_substance, empirical, 'Whether the revisions changed substance or branding.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(reading_acquisition_legitimacy__balanced_literacy_integration, 1990, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(read_tr_t1990, reading_acquisition_legitimacy__balanced_literacy_integration, theater_ratio, 1990, 0.25).
narrative_ontology:measurement_basis(read_tr_t1990, observed).
narrative_ontology:measurement(read_tr_t1995, reading_acquisition_legitimacy__balanced_literacy_integration, theater_ratio, 1995, 0.3).
narrative_ontology:measurement_basis(read_tr_t1995, observed).
narrative_ontology:measurement(read_tr_t2000, reading_acquisition_legitimacy__balanced_literacy_integration, theater_ratio, 2000, 0.34).
narrative_ontology:measurement_basis(read_tr_t2000, observed).
narrative_ontology:measurement(read_tr_t2005, reading_acquisition_legitimacy__balanced_literacy_integration, theater_ratio, 2005, 0.38).
narrative_ontology:measurement_basis(read_tr_t2005, observed).
narrative_ontology:measurement(read_tr_t2010, reading_acquisition_legitimacy__balanced_literacy_integration, theater_ratio, 2010, 0.42).
narrative_ontology:measurement_basis(read_tr_t2010, observed).
narrative_ontology:measurement(read_tr_t2015, reading_acquisition_legitimacy__balanced_literacy_integration, theater_ratio, 2015, 0.46).
narrative_ontology:measurement_basis(read_tr_t2015, observed).
narrative_ontology:measurement(read_tr_t2020, reading_acquisition_legitimacy__balanced_literacy_integration, theater_ratio, 2020, 0.5).
narrative_ontology:measurement_basis(read_tr_t2020, observed).
narrative_ontology:measurement(read_tr_t2025, reading_acquisition_legitimacy__balanced_literacy_integration, theater_ratio, 2025, 0.45).
narrative_ontology:measurement_basis(read_tr_t2025, observed).

% Extraction over time
narrative_ontology:measurement(read_be_t1990, reading_acquisition_legitimacy__balanced_literacy_integration, base_extractiveness, 1990, 0.4).
narrative_ontology:measurement_basis(read_be_t1990, observed).
narrative_ontology:measurement(read_be_t1995, reading_acquisition_legitimacy__balanced_literacy_integration, base_extractiveness, 1995, 0.45).
narrative_ontology:measurement_basis(read_be_t1995, observed).
narrative_ontology:measurement(read_be_t2000, reading_acquisition_legitimacy__balanced_literacy_integration, base_extractiveness, 2000, 0.49).
narrative_ontology:measurement_basis(read_be_t2000, observed).
narrative_ontology:measurement(read_be_t2005, reading_acquisition_legitimacy__balanced_literacy_integration, base_extractiveness, 2005, 0.53).
narrative_ontology:measurement_basis(read_be_t2005, observed).
narrative_ontology:measurement(read_be_t2010, reading_acquisition_legitimacy__balanced_literacy_integration, base_extractiveness, 2010, 0.57).
narrative_ontology:measurement_basis(read_be_t2010, observed).
narrative_ontology:measurement(read_be_t2015, reading_acquisition_legitimacy__balanced_literacy_integration, base_extractiveness, 2015, 0.61).
narrative_ontology:measurement_basis(read_be_t2015, observed).
narrative_ontology:measurement(read_be_t2020, reading_acquisition_legitimacy__balanced_literacy_integration, base_extractiveness, 2020, 0.64).
narrative_ontology:measurement_basis(read_be_t2020, observed).
narrative_ontology:measurement(read_be_t2025, reading_acquisition_legitimacy__balanced_literacy_integration, base_extractiveness, 2025, 0.55).
narrative_ontology:measurement_basis(read_be_t2025, observed).

% Suppression requirement over time
narrative_ontology:measurement(read_su_t1990, reading_acquisition_legitimacy__balanced_literacy_integration, suppression_requirement, 1990, 0.45).
narrative_ontology:measurement_basis(read_su_t1990, observed).
narrative_ontology:measurement(read_su_t1995, reading_acquisition_legitimacy__balanced_literacy_integration, suppression_requirement, 1995, 0.53).
narrative_ontology:measurement_basis(read_su_t1995, observed).
narrative_ontology:measurement(read_su_t2000, reading_acquisition_legitimacy__balanced_literacy_integration, suppression_requirement, 2000, 0.58).
narrative_ontology:measurement_basis(read_su_t2000, observed).
narrative_ontology:measurement(read_su_t2005, reading_acquisition_legitimacy__balanced_literacy_integration, suppression_requirement, 2005, 0.62).
narrative_ontology:measurement_basis(read_su_t2005, observed).
narrative_ontology:measurement(read_su_t2010, reading_acquisition_legitimacy__balanced_literacy_integration, suppression_requirement, 2010, 0.64).
narrative_ontology:measurement_basis(read_su_t2010, observed).
narrative_ontology:measurement(read_su_t2015, reading_acquisition_legitimacy__balanced_literacy_integration, suppression_requirement, 2015, 0.65).
narrative_ontology:measurement_basis(read_su_t2015, observed).
narrative_ontology:measurement(read_su_t2020, reading_acquisition_legitimacy__balanced_literacy_integration, suppression_requirement, 2020, 0.58).
narrative_ontology:measurement_basis(read_su_t2020, observed).
narrative_ontology:measurement(read_su_t2025, reading_acquisition_legitimacy__balanced_literacy_integration, suppression_requirement, 2025, 0.5).
narrative_ontology:measurement_basis(read_su_t2025, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(reading_acquisition_legitimacy__balanced_literacy_integration, identity_coordination).
narrative_ontology:affects_constraint(reading_acquisition_legitimacy__balanced_literacy_integration, phonics_decoding_primacy).
narrative_ontology:affects_constraint(reading_acquisition_legitimacy__balanced_literacy_integration, whole_language_meaning_primacy).
narrative_ontology:affects_constraint(reading_acquisition_legitimacy__balanced_literacy_integration, structured_literacy_remediation).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'legitimate reading instruction' decomposes into four readings of the reading_acquisition_legitimacy kernel, each a separate story with its own epsilon, beneficiaries, and victims. This file is the balanced_literacy_integration reading, the mediating member: historically it absorbed whole_language_meaning_primacy under a new label (changing that sibling's legitimacy conditions without foreclosing it) and deflected phonics_decoding_primacy by claiming to already include explicit phonics (blunting explicit-code mandates for two decades), while its documented failures created the constituency and political conditions for structured_literacy_remediation's rise. The epsilon values differ across the family because each reading assesses its own referent by its own lights; the links here are structural influence, not shared measurement.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
