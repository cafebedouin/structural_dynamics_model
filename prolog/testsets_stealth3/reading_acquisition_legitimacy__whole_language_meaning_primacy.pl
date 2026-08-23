% ============================================================================
% CONSTRAINT STORY: reading_acquisition_legitimacy__whole_language_meaning_primacy
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_reading_acquisition_legitimacy__whole_language_meaning_primacy, []).

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
 *   constraint_id: reading_acquisition_legitimacy__whole_language_meaning_primacy
 *   human_readable: Whole-Language Meaning Primacy: Legitimate Instruction as Authentic Literature Immersion
 *   domain: education_policy/cognitive_science/literacy_pedagogy
 *
 * SUMMARY:
 *   A contested-kernel story from the reading wars. The kernel — what makes
 *   beginning-reading instruction legitimate — has been read at least four
 *   ways; this file authors the whole_language_meaning_primacy reading alone:
 *   reading IS meaning-making, so legitimate instruction immerses children in
 *   authentic literature from day one and lets decoding emerge naturally,
 *   with teachers facilitating rather than instructing, running records
 *   steering small groups, and leveled (predictable) texts standing in for
 *   decodable ones. The standing arrangement under contest is that regime as
 *   it actually operated from its late-1980s institutionalization (e.g.,
 *   California's 1987 literature-based framework) through the present
 *   science-of-reading counter-movement. Around it grew a material ecosystem:
 *   classroom libraries of leveled texts sold at scale, benchmark kits,
 *   multi-year coaching contracts, and university centers that prepared the
 *   workforce and wrote the adoption frameworks. The claim/metric gap is
 *   deliberate and is the data: the reading CLAIMS a humane natural-learning
 *   arrangement; the authored METRICS describe an actively maintained,
 *   revenue-bearing, evidence-contested operation whose steepest costs fall
 *   on the children least equipped to absorb them. KEY AGENTS (by structural
 *   relationship): - curriculum_publishers: Primary collecting seat
 *   (institutional/arbitrage) — sells leveled libraries, benchmark kits,
 *   workshop curricula, attached PD; pivots product lines as regulation
 *   shifts - university_education_faculties: Collecting + agenda-shaping seat
 *   (institutional/identity_locked) — trains the workforce, authors the
 *   frameworks, reputations bound to continuity - literacy_pd_consultants:
 *   Collecting seat (organized/arbitrage) — coaching contracts and institutes
 *   certifying the method - district_adoption_committees: Agenda-setting seat
 *   (institutional/constrained) — selects core programs from within the
 *   trained cohort - classroom_teachers: Paying + collecting seat
 *   (moderate/constrained) — implements daily, carries assessment load,
 *   receives professional identity - struggling_emergent_readers: Primary
 *   bearing seat (powerless/trapped) — children whose literacy trajectories
 *   form under the method - dyslexic_students: Acutely bearing seat
 *   (powerless/trapped) — need explicit code instruction the method withholds
 *   - families_of_struggling_readers: Bearing seat (moderate/constrained) —
 *   purchase years of private remediation, organize advocacy -
 *   reading_specialists_in_districts: Excluded seat (moderate/constrained) —
 *   diagnoses overruled by adopted schedules - reading_science_researchers:
 *   Analytical seat (institutional/analytical) -
 *   science_of_reading_legislatures: Analytical-acting seat
 *   (institutional/analytical) — statutory pressure reshaping procurement
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(reading_acquisition_legitimacy__whole_language_meaning_primacy, 0.6).
domain_priors:suppression_score(reading_acquisition_legitimacy__whole_language_meaning_primacy, 0.65).
domain_priors:theater_ratio(reading_acquisition_legitimacy__whole_language_meaning_primacy, 0.52).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(reading_acquisition_legitimacy__whole_language_meaning_primacy, extractiveness, 0.6).
narrative_ontology:constraint_metric(reading_acquisition_legitimacy__whole_language_meaning_primacy, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(reading_acquisition_legitimacy__whole_language_meaning_primacy, theater_ratio, 0.52).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(reading_acquisition_legitimacy__whole_language_meaning_primacy, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(reading_acquisition_legitimacy__whole_language_meaning_primacy, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(reading_acquisition_legitimacy__whole_language_meaning_primacy, tangled_rope).
narrative_ontology:human_readable(reading_acquisition_legitimacy__whole_language_meaning_primacy, "Whole-Language Meaning Primacy: Legitimate Instruction as Authentic Literature Immersion").
narrative_ontology:topic_domain(reading_acquisition_legitimacy__whole_language_meaning_primacy, "education_policy/cognitive_science/literacy_pedagogy").

domain_priors:requires_active_enforcement(reading_acquisition_legitimacy__whole_language_meaning_primacy).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(reading_acquisition_legitimacy__whole_language_meaning_primacy, '9326a4c2-e6b3-4280-9610-f8eeca78bc66').
narrative_ontology:cs_kernel_codification('9326a4c2-e6b3-4280-9610-f8eeca78bc66', fixed_text).
narrative_ontology:cs_authority_grounding('9326a4c2-e6b3-4280-9610-f8eeca78bc66', lineage).
narrative_ontology:cs_interpretation_layer_present('9326a4c2-e6b3-4280-9610-f8eeca78bc66').
narrative_ontology:cs_reading_relation('9326a4c2-e6b3-4280-9610-f8eeca78bc66', reading_acquisition_legitimacy__phonics_decoding_primacy, coexists_with).
narrative_ontology:cs_reading_relation('9326a4c2-e6b3-4280-9610-f8eeca78bc66', reading_acquisition_legitimacy__balanced_literacy_integration, influences).
narrative_ontology:cs_axiom('9326a4c2-e6b3-4280-9610-f8eeca78bc66', foundational, decoding_emerges_from_meaning_engagement).
narrative_ontology:cs_axiom_status(decoding_emerges_from_meaning_engagement, holdable).
narrative_ontology:cs_axiom_grounding('9326a4c2-e6b3-4280-9610-f8eeca78bc66', decoding_emerges_from_meaning_engagement, empirically_contingent).
narrative_ontology:cs_axiom('9326a4c2-e6b3-4280-9610-f8eeca78bc66', secondary, explicit_decoding_instruction_threatens_motivation).
narrative_ontology:cs_axiom_status(explicit_decoding_instruction_threatens_motivation, holdable).
narrative_ontology:cs_axiom_grounding('9326a4c2-e6b3-4280-9610-f8eeca78bc66', explicit_decoding_instruction_threatens_motivation, empirically_contingent).
narrative_ontology:cs_reference_frame('9326a4c2-e6b3-4280-9610-f8eeca78bc66', meaning_first_natural_learning).
narrative_ontology:cs_drift_state('9326a4c2-e6b3-4280-9610-f8eeca78bc66', contemporary_science_of_reading_pressure, gap(axiom_overriding, substantial, true)).
narrative_ontology:cs_created_at('9326a4c2-e6b3-4280-9610-f8eeca78bc66', '').
narrative_ontology:cs_kernel_id(reading_acquisition_legitimacy__whole_language_meaning_primacy, reading_acquisition_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(reading_acquisition_legitimacy__whole_language_meaning_primacy, curriculum_publishers).
narrative_ontology:constraint_beneficiary(reading_acquisition_legitimacy__whole_language_meaning_primacy, university_education_faculties).
narrative_ontology:constraint_beneficiary(reading_acquisition_legitimacy__whole_language_meaning_primacy, literacy_pd_consultants).
narrative_ontology:constraint_victim(reading_acquisition_legitimacy__whole_language_meaning_primacy, struggling_emergent_readers).
narrative_ontology:constraint_victim(reading_acquisition_legitimacy__whole_language_meaning_primacy, dyslexic_students).
narrative_ontology:constraint_victim(reading_acquisition_legitimacy__whole_language_meaning_primacy, families_of_struggling_readers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(reading_acquisition_legitimacy__whole_language_meaning_primacy, classroom_teachers).
narrative_ontology:constraint_victim(reading_acquisition_legitimacy__whole_language_meaning_primacy, classroom_teachers).
narrative_ontology:constraint_vindicates(reading_acquisition_legitimacy__whole_language_meaning_primacy, psycholinguistic_cueing_model).
narrative_ontology:constraint_vindicates(reading_acquisition_legitimacy__whole_language_meaning_primacy, natural_learning_hypothesis).
narrative_ontology:constraint_vindicates(reading_acquisition_legitimacy__whole_language_meaning_primacy, constructivist_pedagogy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Develop and sell classroom libraries of leveled texts, benchmark assessment kits, and workshop-style core programs; attach multi-year professional development contracts to adoptions. Revenues scale with the installed base of teachers trained in the framework, and sales representatives sit inside district relationships. As state requirements shifted after 2019, they added phonics product lines through the same channels.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__whole_language_meaning_primacy, curriculum_publishers, beneficiary,
    institutional, generational, arbitrage, national).

% Prepare the teachers who fill classrooms; built degree programs, centers, and long-running institutes around meaning-first pedagogy; author the frameworks and assessment tools that districts purchase. Careers, named centers, and endowed chairs rest on the continuity of the approach; recent public revisions adjusted materials while retaining the core account of how children learn to read.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__whole_language_meaning_primacy, university_education_faculties, beneficiary,
    institutional, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(reading_acquisition_legitimacy__whole_language_meaning_primacy, university_education_faculties, agenda_setter).

% Deliver coaching contracts, summer institutes, and train-the-trainer programs certifying others in running records and workshop method. Income tracks district adoption cycles; the underlying skills of facilitation and coaching port to adjacent professional-development markets if demand shifts.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__whole_language_meaning_primacy, literacy_pd_consultants, beneficiary,
    organized, biographical, arbitrage, national).

% Convene on adoption cycles to select core English-language arts programs. Membership is drawn from coaches and lead teachers formed in the dominant preparation pipeline; evaluation rubrics privilege authentic literature, workshop structures, and leveled assessment. They face publisher presentations and, latterly, parent delegations citing reading research.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__whole_language_meaning_primacy, district_adoption_committees, agenda_setter,
    institutional, biographical, constrained, local).

% Run daily reading workshops, take running records, level books, and confer with young readers; carry a heavy assessment load. Their preparation, evaluation instruments, and collegial identity are bound up with the method, and many privately describe supplementing with decoding practice when students stall. Career advancement runs through fidelity to the adopted program.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__whole_language_meaning_primacy, classroom_teachers, payer,
    moderate, biographical, constrained, local).
narrative_ontology:stakeholder_secondary_role(reading_acquisition_legitimacy__whole_language_meaning_primacy, classroom_teachers, beneficiary).

% Children in the earliest grades encounter books chosen for interest rather than decodability and are expected to infer letter-sound patterns from exposure, picture cues, and guess-and-check reading. Those who do not converge quickly receive small-group support built on the same principles. Their report cards, placement, and later options depend on progress made under this method, and they cannot choose another instructional approach.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__whole_language_meaning_primacy, struggling_emergent_readers, payer,
    powerless, biographical, trapped, local).

% Students with dyslexia require explicit, systematic letter-sound instruction; in immersion-only classrooms their difficulty is commonly attributed to effort, maturity, or home life until upper elementary grades. Years are spent practicing prediction and contextual guessing strategies that bypass the specific deficit, while formal diagnosis and appropriate intervention typically arrive late and often only after families intervene.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__whole_language_meaning_primacy, dyslexic_students, payer,
    powerless, biographical, trapped, local).

% Notice a child falling behind, request help, and are frequently told to read more at home. Many purchase private tutoring at hourly rates for several years; some organize into parent advocacy chapters pressing districts and legislatures for universal screening and explicit-instruction mandates.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__whole_language_meaning_primacy, families_of_struggling_readers, payer,
    moderate, biographical, constrained, local).

% Diagnose struggling readers and write intervention recommendations grounded in explicit-instruction findings. Building schedules and adopted programs commonly reserve intervention blocks for guided reading, so specialist plans are deferred or overruled; they remain employed inside the system whose instructional choices they dispute.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__whole_language_meaning_primacy, reading_specialists_in_districts, excluded,
    moderate, biographical, constrained, local).

% Publish experimental and longitudinal studies of reading acquisition; findings on cueing systems and systematic phonics circulate in journals most practitioners never read. They were rarely invited into adoption processes during the framework's expansion, and their policy salience rose sharply with 2017-2019 journalism and subsequent legislative testimony.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__whole_language_meaning_primacy, reading_science_researchers, observer,
    institutional, generational, analytical, global).

% Since 2019, dozens of statehouses have passed statutes mandating screening, evidence-aligned curricula, and teacher retraining. Testimony is drawn from parents, researchers, and teachers; implementation varies, and the statutes alter the procurement rules on which the framework's distribution depended.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__whole_language_meaning_primacy, science_of_reading_legislatures, observer,
    institutional, biographical, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(reading_acquisition_legitimacy__whole_language_meaning_primacy, curriculum_publishers).
narrative_ontology:fixing_cost_class(reading_acquisition_legitimacy__whole_language_meaning_primacy, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Aligns thousands of novice teachers' minute-to-minute literacy decisions: a shared philosophy, common leveled texts, a portable assessment ritual (running records, guided reading groups), and a preparation pipeline mean a new teacher can operate a classroom without deep individual diagnostic expertise.
% TRANSFER_FUNCTION: Moves district and Title-I funds from taxpayers to publishers, university centers, and consultancies through library purchases, benchmark kits, and multi-year coaching contracts; moves classroom hours toward meaning-centered activity; moves the probability of reading failure disproportionately onto entrants without home print exposure, and remediation costs downstream onto families and adult-services budgets.
% ABSENT_VOICES: During the framework's expansion (late 1980s-2000s), reading scientists were absent from adoption committees; parents of failing readers were managed as anxious outliers; special educators' intervention recommendations were deferred to workshop schedules. The rooms where legitimate materials were defined contained only the trained cohort.
% DISAPPEARANCE_RATIONALE: Procurement pipelines, teacher-preparation syllabi, coaching contracts, assessment routines, and daily classroom schedules all reorganize around whatever replaces the meaning-primacy legitimacy rule; rearrangement is in fact underway as statutes force replacement of leveled libraries and retraining of the workforce, which demonstrates the dependence.
% FOUNDING_PROBLEM: Mid-century basals taught isolated skills from contrived texts, producing children who could call words without understanding and who disliked reading; the founders asked how instruction could build comprehending, willing readers and answered: immerse them in real books and let meaning drive skill growth.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: the cognitive-psychology literature (National Reading Panel 2000; Share 1995; Castles, Rastle and Nation 2018) confirms the motivational and comprehension problems were real while refuting the emergence mechanism; state hearing records (e.g., Mississippi 2013 onward) document legislators concluding that the founding aim survives but the method fails vulnerable learners. No attestation comes only from publishers or preparation faculties.
narrative_ontology:disappearance_verdict(reading_acquisition_legitimacy__whole_language_meaning_primacy, world_rearranges).
narrative_ontology:founding_problem_status(reading_acquisition_legitimacy__whole_language_meaning_primacy, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(reading_acquisition_legitimacy__whole_language_meaning_primacy, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(reading_acquisition_legitimacy__whole_language_meaning_primacy, 'none', 1).
narrative_ontology:epsilon_provenance(reading_acquisition_legitimacy__whole_language_meaning_primacy, 0.6, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(reading_acquisition_legitimacy__whole_language_meaning_primacy_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(reading_acquisition_legitimacy__whole_language_meaning_primacy, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(reading_acquisition_legitimacy__whole_language_meaning_primacy_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.60: two flow classes — direct money (library kits costing thousands per classroom, multi-year coaching contracts) and opportunity cost borne by non-converging readers — offset by the arrangement's real service content (children do read books; conferences and small-group work occur), landing ε mid-high rather than extreme. Suppression 0.65, authored RAW and unscaled per the structural-property rule (only extractiveness is scaled by directionality and scope): enforcement is institutional rather than physical — procurement rubrics, preparation gatekeeping, schedule control. Theater 0.52: running-record/MSV cueing analysis persists as ritual after its predictive validity collapsed in replication work; the whole-language-to-'balanced literacy' relabeling after 2000 is a theatrical adaptation that preserved practice beneath new packaging. Accessibility_collapse 0.35: the phonics alternative never disappeared — it survived in special education and home schooling and returned by statute, so alternatives remain visible and collapse is low. Resistance 0.72: parent chapters (Decoding Dyslexia, 2011- ), investigative journalism (2017-19), researcher coalitions, and 40-plus state statutes constitute organized, sustained resistance. The suppression_requirement series traces a 1990s enforcement ratchet (adoption exclusions, preparation alignment), post-2000 softening via relabel, and renewed defensive intensity after 2019; all three tracked metrics share one time grid (t=0 maps to 1985, five-year steps to t=40 = 2025). Identity-lock mechanism: two decades of preparation fuse the facilitator role with professional self-worth, so exit is identity_locked not because jobs are scarce but because recanting means reframing one's career as having harmed children — this keeps faculty and veteran-coach seats stable despite published counter-evidence; were the identity frame to break publicly (the 2022 curriculum revisions signal partial breakage), enforcement would decay faster than materials could be replaced. Fixing-cost note: for any single seat inside the arrangement, replacement meant retraining a workforce, repurchasing libraries, and confronting sunk reputations — prohibitive relative to what any one actor could absorb, which is why change arrived only through cross-sector statutory coalitions.
 *
 * PERSPECTIVAL GAP:
 *   From the publisher and faculty seats the arrangement is a service ecosystem they built and legitimately monetize; from the struggling-reader seat the same daily routine is the reason school feels like guessing; from the teacher seat it is simultaneously vocation and burden. The engine computes different per-seat types from these structural asymmetries: arbitrage-holding supplier seats compute subsidy-side; trapped child seats compute near full-target; the constrained, dual-role teacher seat lands between. Nothing in the authored claim adjudicates among these computations — the divergence is the measurement.
 *
 * DIRECTIONALITY LOGIC:
 *   Publishers (collector, arbitrage exit) derive d near the beneficiary pole; preparation faculties likewise, with identity lock reinforcing their investment rather than their exposure; the PD consultancy seat sits similarly low. Adoption committees hold agenda power with modest personal stake, landing low-to-mid. Teachers are dual-listed (payer + beneficiary): they pay in workload, blame, and whiplash and collect in identity and community, deriving a mid-to-high net d. Struggling readers are trapped with no substitute instruction, d near 0.95; dyslexic students effectively d near 1.0 because the method withholds precisely the input their profiles require; families pay directly with constrained exit, d high; district specialists, excluded yet employed inside the system, carry elevated d through foreclosed professional judgment. Receipt concentrates: kit and contract payments settle at the publisher seat, which also owns the district channel, so gain_flow names curriculum_publishers, with faculties and consultancies collecting tributary shares. No directionality overrides are authored: the beneficiary/victim declarations plus exit-option differentiation already separate the seats.
 *
 * MANDATROPHY ANALYSIS:
 *   Authoring this as tangled_rope rather than snare keeps two facts visible at once: the arrangement genuinely coordinated a profession (preparation, materials, assessment, vocabulary) and it genuinely transferred resources asymmetrically. Calling it a snare would erase the sincere professional project and wrongly predict collapse-on-exposure; calling it a rope would erase the bearing children and wrongly predict benign persistence. Genealogically: the founding problem (deadening skills-drill instruction) was real and remains partly live — motivation and comprehension concerns persist — but the mechanism claiming legitimacy (decoding emerges naturally from meaning immersion) is contested-toward-dead in the empirical literature. The status=contested x verdict=world_rearranges combination flags a live arrangement whose justification is eroding: if statutory replacement proceeds, this constraint drifts toward scaffold-shaped transition; if enforcement decays faster than replacement, it decays toward piton (ritual running records persisting in classrooms whose districts no longer fund the ecosystem). Mandatrophy resolution therefore hinges on which decays first, the identity lock or the procurement channel.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_commitment_structure,
    'This story instantiates one reading (whole_language_meaning_primacy) of the contested kernel reading_acquisition_legitimacy; how would classification shift under the sibling readings?',
    'Compile phonics_decoding_primacy, balanced_literacy_integration, and structured_literacy_remediation as separate stories and compare per-seat types, victim sets, and gain_flow across the family.',
    'Under the phonics reading the same classrooms appear under-instructed rather than liberated, relocating the bearing-seat designation toward the districts withholding explicit instruction; under balanced literacy the flow profile splits between strands, changing which seats compute as coordinated versus paying.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_commitment_structure, conceptual, 'Committer-frame routing: sibling readings of the kernel would re-partition seats over the same referent.').

omega_variable(
    implementation_fidelity_attribution,
    'Are poor outcomes for non-converging readers intrinsic to meaning-primacy, or artifacts of unfaithful implementation?',
    'Compare outcomes across sites with documented high-fidelity delivery versus mixed-fidelity sites, controlling for demographics and home print exposure.',
    'Intrinsic failure supports the high-extraction reading of the standing arrangement; a fidelity-artifact finding would shift responsibility to delivery quality and lower epsilon.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(implementation_fidelity_attribution, empirical, 'Defenders attribute failures to unfaithful implementation; the intrinsic-versus-artifact question decides the victim claim''s strength.').

omega_variable(
    home_print_vs_instruction_variance,
    'How much of the outcome gap attributed to the framework is instructional versus home print-exposure variance?',
    'Within-school comparisons of classrooms differing in method while sharing demographics, plus longitudinal mediation models separating instructional dose from home-literacy measures.',
    'A large instructional share strengthens the structural victim claim for print-poor households; a small share would reframe affected children as statistically rather than structurally situated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(home_print_vs_instruction_variance, empirical, 'Magnitude of Matthew-effect confound in the framework''s measured harms.').

omega_variable(
    balanced_literacy_alias_or_synthesis,
    'Is balanced_literacy_integration a genuinely distinct third constraint, or whole-language practice relabeled?',
    'Material-level audit comparing lesson structures, cueing reliance, and decodable-text share in pre- and post-relabel curricula from the same publishers.',
    'An alias finding merges the family branch and attributes the balanced-literacy era''s flows to this constraint; a synthesis finding keeps the readings distinct with separate epsilon values.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(balanced_literacy_alias_or_synthesis, conceptual, 'Family-topology question: whether the balanced-literacy sibling is a distinct constraint or a relabel of this one.').

omega_variable(
    educator_suppression_internalization,
    'Is the pressure keeping educator dissent quiet structural (procurement rules, evaluation instruments, preparation gatekeeping) or internalized (identity fusion with the facilitator role)?',
    'Post-exit trajectory study of teachers and coaches who left the framework: if open dissent surfaces predominantly after leaving, the internalized share is substantial.',
    'A large internalized share raises effective suppression above the structural measure and predicts slower enforcement decay than statute passage alone would imply.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(educator_suppression_internalization, empirical, 'Structural versus internalized components of educator conformity under the framework.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(reading_acquisition_legitimacy__whole_language_meaning_primacy, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(read_tr_t0, reading_acquisition_legitimacy__whole_language_meaning_primacy, theater_ratio, 0, 0.15).
narrative_ontology:measurement_basis(read_tr_t0, observed).
narrative_ontology:measurement(read_tr_t5, reading_acquisition_legitimacy__whole_language_meaning_primacy, theater_ratio, 5, 0.22).
narrative_ontology:measurement_basis(read_tr_t5, observed).
narrative_ontology:measurement(read_tr_t10, reading_acquisition_legitimacy__whole_language_meaning_primacy, theater_ratio, 10, 0.32).
narrative_ontology:measurement_basis(read_tr_t10, observed).
narrative_ontology:measurement(read_tr_t15, reading_acquisition_legitimacy__whole_language_meaning_primacy, theater_ratio, 15, 0.4).
narrative_ontology:measurement_basis(read_tr_t15, observed).
narrative_ontology:measurement(read_tr_t20, reading_acquisition_legitimacy__whole_language_meaning_primacy, theater_ratio, 20, 0.46).
narrative_ontology:measurement_basis(read_tr_t20, observed).
narrative_ontology:measurement(read_tr_t25, reading_acquisition_legitimacy__whole_language_meaning_primacy, theater_ratio, 25, 0.52).
narrative_ontology:measurement_basis(read_tr_t25, observed).
narrative_ontology:measurement(read_tr_t30, reading_acquisition_legitimacy__whole_language_meaning_primacy, theater_ratio, 30, 0.55).
narrative_ontology:measurement_basis(read_tr_t30, observed).
narrative_ontology:measurement(read_tr_t35, reading_acquisition_legitimacy__whole_language_meaning_primacy, theater_ratio, 35, 0.54).
narrative_ontology:measurement_basis(read_tr_t35, observed).
narrative_ontology:measurement(read_tr_t40, reading_acquisition_legitimacy__whole_language_meaning_primacy, theater_ratio, 40, 0.52).
narrative_ontology:measurement_basis(read_tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(read_be_t0, reading_acquisition_legitimacy__whole_language_meaning_primacy, base_extractiveness, 0, 0.28).
narrative_ontology:measurement_basis(read_be_t0, observed).
narrative_ontology:measurement(read_be_t5, reading_acquisition_legitimacy__whole_language_meaning_primacy, base_extractiveness, 5, 0.38).
narrative_ontology:measurement_basis(read_be_t5, observed).
narrative_ontology:measurement(read_be_t10, reading_acquisition_legitimacy__whole_language_meaning_primacy, base_extractiveness, 10, 0.5).
narrative_ontology:measurement_basis(read_be_t10, observed).
narrative_ontology:measurement(read_be_t15, reading_acquisition_legitimacy__whole_language_meaning_primacy, base_extractiveness, 15, 0.56).
narrative_ontology:measurement_basis(read_be_t15, observed).
narrative_ontology:measurement(read_be_t20, reading_acquisition_legitimacy__whole_language_meaning_primacy, base_extractiveness, 20, 0.6).
narrative_ontology:measurement_basis(read_be_t20, observed).
narrative_ontology:measurement(read_be_t25, reading_acquisition_legitimacy__whole_language_meaning_primacy, base_extractiveness, 25, 0.62).
narrative_ontology:measurement_basis(read_be_t25, observed).
narrative_ontology:measurement(read_be_t30, reading_acquisition_legitimacy__whole_language_meaning_primacy, base_extractiveness, 30, 0.63).
narrative_ontology:measurement_basis(read_be_t30, observed).
narrative_ontology:measurement(read_be_t35, reading_acquisition_legitimacy__whole_language_meaning_primacy, base_extractiveness, 35, 0.62).
narrative_ontology:measurement_basis(read_be_t35, observed).
narrative_ontology:measurement(read_be_t40, reading_acquisition_legitimacy__whole_language_meaning_primacy, base_extractiveness, 40, 0.6).
narrative_ontology:measurement_basis(read_be_t40, observed).

% Suppression requirement over time
narrative_ontology:measurement(read_su_t0, reading_acquisition_legitimacy__whole_language_meaning_primacy, suppression_requirement, 0, 0.42).
narrative_ontology:measurement_basis(read_su_t0, observed).
narrative_ontology:measurement(read_su_t5, reading_acquisition_legitimacy__whole_language_meaning_primacy, suppression_requirement, 5, 0.55).
narrative_ontology:measurement_basis(read_su_t5, observed).
narrative_ontology:measurement(read_su_t10, reading_acquisition_legitimacy__whole_language_meaning_primacy, suppression_requirement, 10, 0.7).
narrative_ontology:measurement_basis(read_su_t10, observed).
narrative_ontology:measurement(read_su_t15, reading_acquisition_legitimacy__whole_language_meaning_primacy, suppression_requirement, 15, 0.74).
narrative_ontology:measurement_basis(read_su_t15, observed).
narrative_ontology:measurement(read_su_t20, reading_acquisition_legitimacy__whole_language_meaning_primacy, suppression_requirement, 20, 0.68).
narrative_ontology:measurement_basis(read_su_t20, observed).
narrative_ontology:measurement(read_su_t25, reading_acquisition_legitimacy__whole_language_meaning_primacy, suppression_requirement, 25, 0.64).
narrative_ontology:measurement_basis(read_su_t25, observed).
narrative_ontology:measurement(read_su_t30, reading_acquisition_legitimacy__whole_language_meaning_primacy, suppression_requirement, 30, 0.62).
narrative_ontology:measurement_basis(read_su_t30, observed).
narrative_ontology:measurement(read_su_t35, reading_acquisition_legitimacy__whole_language_meaning_primacy, suppression_requirement, 35, 0.67).
narrative_ontology:measurement_basis(read_su_t35, observed).
narrative_ontology:measurement(read_su_t40, reading_acquisition_legitimacy__whole_language_meaning_primacy, suppression_requirement, 40, 0.65).
narrative_ontology:measurement_basis(read_su_t40, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(reading_acquisition_legitimacy__whole_language_meaning_primacy, identity_coordination).
narrative_ontology:affects_constraint(reading_acquisition_legitimacy__whole_language_meaning_primacy, phonics_decoding_primacy).
narrative_ontology:affects_constraint(reading_acquisition_legitimacy__whole_language_meaning_primacy, balanced_literacy_integration).
narrative_ontology:affects_constraint(reading_acquisition_legitimacy__whole_language_meaning_primacy, structured_literacy_remediation).

% DUAL FORMULATION NOTE:
% The colloquial label 'Reading Wars' covers four structurally distinct claims about what reading is and what instruction legitimacy requires; per the epsilon-invariance principle each reading is authored as its own story with its own epsilon over the arrangement it contests. This story's referent is the meaning-immersion arrangement as operated (classrooms, procurement, preparation). Downstream structure: whole-language institutionalization created the installed base, PD channels, and adoption norms within which balanced literacy operates — hence the influences edge; the phonics and structured-literacy readings contest the same classrooms and are linked for family completeness.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
