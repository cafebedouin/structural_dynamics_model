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
 *   human_readable: Balanced Literacy Integration Gate (Reading-Acquisition Legitimacy Kernel)
 *   domain: education policy/cognitive science/literacy pedagogy
 *
 * SUMMARY:
 *   From the mid-1990s through the mid-2020s, the operative definition of
 *   legitimate reading instruction in most Anglophone school systems was the
 *   balanced-literacy synthesis: reading requires both decoding and
 *   meaning-making, so legitimate instruction balances explicit phonics with
 *   authentic literature exposure. The standard was institutionalized through
 *   comprehensive curriculum programs, university-affiliated training
 *   institutes, coach networks, and district adoption mandates, and it was
 *   enforced through fidelity monitoring and the professional marginalization
 *   of teachers who emphasized systematic decoding beyond the program. Its
 *   human cost fell on the learners least able to absorb it: dyslexic
 *   students who need explicit systematic code instruction, and children
 *   without home literacy buffers who depend entirely on school instruction.
 *   The empirical reading-science tradition accumulated contrary evidence for
 *   two decades outside the adoption conversation before the
 *   science-of-reading movement converted it into statute in 40+ US states.
 *   This story authors ONE reading of the reading_acquisition_legitimacy
 *   kernel — the balanced-integration reading — as a clean, epsilon-invariant
 *   constraint; the sibling readings are separate linked stories, and the
 *   epsilon here is assessed by THIS reading's own lights over the standing
 *   arrangement (the institutionalized gate, ~1995-2025): the reading's own
 *   doctrine requires that struggling readers receive both phonics
 *   intervention and guided reading, and by that standard the arrangement
 *   under-delivered for them — hence moderate rather than low extraction. KEY
 *   AGENTS (by structural relationship): - balanced_literacy_publishers:
 *   Primary agenda-setter and gain recipient (institutional/arbitrage) —
 *   defines what 'balanced' contains, collects curriculum dollars -
 *   workshop_training_institutes: Secondary agenda-setter, recurring-fee
 *   collector (institutional/arbitrage) — certifies coaches, defines fidelity
 *   - district_literacy_directors: Adoption and enforcement seat
 *   (institutional/mobile) - classroom_teachers: Dual-positioned
 *   implementer-subject (organized/constrained) — implements the standard and
 *   is evaluated against it - struggling_dyslexic_readers: Primary target
 *   (powerless/trapped) — bears the cost of under-specified decoding
 *   instruction - disadvantaged_emergent_readers: Primary target
 *   (powerless/trapped) — no home literacy buffer to mask the standard's gaps
 *   - proficient_background_readers: Incidental beneficiary
 *   (powerless/trapped) — the literature-rich component genuinely serves them
 *   - parents_of_struggling_readers: Cost-bearing advocates
 *   (organized/constrained) — long dismissed, now legislatively ascendant -
 *   reading_scientists: Analytical observer (institutional/analytical) — the
 *   evidence base outside the adoption loop - state_education_legislators:
 *   Historically excluded seat (institutional/mobile) — now the counter-force
 *   via statute
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(reading_acquisition_legitimacy__balanced_literacy_integration, 0.55).
domain_priors:suppression_score(reading_acquisition_legitimacy__balanced_literacy_integration, 0.55).
domain_priors:theater_ratio(reading_acquisition_legitimacy__balanced_literacy_integration, 0.5).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(reading_acquisition_legitimacy__balanced_literacy_integration, extractiveness, 0.55).
narrative_ontology:constraint_metric(reading_acquisition_legitimacy__balanced_literacy_integration, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(reading_acquisition_legitimacy__balanced_literacy_integration, theater_ratio, 0.5).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(reading_acquisition_legitimacy__balanced_literacy_integration, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(reading_acquisition_legitimacy__balanced_literacy_integration, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(reading_acquisition_legitimacy__balanced_literacy_integration, tangled_rope).
narrative_ontology:human_readable(reading_acquisition_legitimacy__balanced_literacy_integration, "Balanced Literacy Integration Gate (Reading-Acquisition Legitimacy Kernel)").
narrative_ontology:topic_domain(reading_acquisition_legitimacy__balanced_literacy_integration, "education policy/cognitive science/literacy pedagogy").

domain_priors:requires_active_enforcement(reading_acquisition_legitimacy__balanced_literacy_integration).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(reading_acquisition_legitimacy__balanced_literacy_integration, 'de93e781-f83d-4ba0-acb5-6b9d04a1d827').
narrative_ontology:cs_kernel_codification('de93e781-f83d-4ba0-acb5-6b9d04a1d827', distributed).
narrative_ontology:cs_authority_grounding('de93e781-f83d-4ba0-acb5-6b9d04a1d827', practice).
narrative_ontology:cs_interpretation_layer_present('de93e781-f83d-4ba0-acb5-6b9d04a1d827').
narrative_ontology:cs_reading_relation('de93e781-f83d-4ba0-acb5-6b9d04a1d827', reading_acquisition_legitimacy__phonics_decoding_primacy, coexists_with).
narrative_ontology:cs_reading_relation('de93e781-f83d-4ba0-acb5-6b9d04a1d827', reading_acquisition_legitimacy__whole_language_meaning_primacy, influences).
narrative_ontology:cs_reading_relation('de93e781-f83d-4ba0-acb5-6b9d04a1d827', reading_acquisition_legitimacy__structured_literacy_remediation, coexists_with).
narrative_ontology:cs_axiom('de93e781-f83d-4ba0-acb5-6b9d04a1d827', foundational, decoding_and_meaning_jointly_constitutive).
narrative_ontology:cs_axiom_status(decoding_and_meaning_jointly_constitutive, holdable).
narrative_ontology:cs_axiom_grounding('de93e781-f83d-4ba0-acb5-6b9d04a1d827', decoding_and_meaning_jointly_constitutive, empirically_contingent).
narrative_ontology:cs_axiom('de93e781-f83d-4ba0-acb5-6b9d04a1d827', foundational, explicit_phonics_necessary_not_sufficient).
narrative_ontology:cs_axiom_status(explicit_phonics_necessary_not_sufficient, holdable).
narrative_ontology:cs_axiom_grounding('de93e781-f83d-4ba0-acb5-6b9d04a1d827', explicit_phonics_necessary_not_sufficient, empirically_contingent).
narrative_ontology:cs_reference_frame('de93e781-f83d-4ba0-acb5-6b9d04a1d827', balanced_synthesis_settlement).
narrative_ontology:cs_drift_state('de93e781-f83d-4ba0-acb5-6b9d04a1d827', post_science_of_reading_legislation, gap(axiom_overriding, substantial, true)).
narrative_ontology:cs_created_at('de93e781-f83d-4ba0-acb5-6b9d04a1d827', '2026-06-12T14:30:00Z').
narrative_ontology:cs_kernel_id(reading_acquisition_legitimacy__balanced_literacy_integration, reading_acquisition_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(reading_acquisition_legitimacy__balanced_literacy_integration, balanced_literacy_publishers).
narrative_ontology:constraint_beneficiary(reading_acquisition_legitimacy__balanced_literacy_integration, workshop_training_institutes).
narrative_ontology:constraint_beneficiary(reading_acquisition_legitimacy__balanced_literacy_integration, proficient_background_readers).
narrative_ontology:constraint_victim(reading_acquisition_legitimacy__balanced_literacy_integration, struggling_dyslexic_readers).
narrative_ontology:constraint_victim(reading_acquisition_legitimacy__balanced_literacy_integration, disadvantaged_emergent_readers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(reading_acquisition_legitimacy__balanced_literacy_integration, classroom_teachers).
narrative_ontology:constraint_victim(reading_acquisition_legitimacy__balanced_literacy_integration, classroom_teachers).
narrative_ontology:constraint_victim(reading_acquisition_legitimacy__balanced_literacy_integration, parents_of_struggling_readers).
narrative_ontology:constraint_vindicates(reading_acquisition_legitimacy__balanced_literacy_integration, balanced_literacy_doctrine).
narrative_ontology:constraint_vindicates(reading_acquisition_legitimacy__balanced_literacy_integration, workshop_model_professional_authority).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Author and market the comprehensive literacy programs that define what 'balanced' instruction contains: units of study, leveled text libraries, running-record assessments. They set the program content districts adopt, revise product lines as legitimacy winds shift (adding systematic phonics strands when the science-of-reading movement gains ground), and collect the curriculum dollars directly. Exit is easy: the same editorial and marketing apparatus can pivot to whatever the next orthodoxy demands.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__balanced_literacy_integration, balanced_literacy_publishers, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(reading_acquisition_legitimacy__balanced_literacy_integration, balanced_literacy_publishers, beneficiary).

% University-based and affiliated training centers that certify coaches, train the trainers, and define what fidelity to the model means. They collect professional-development fees from districts year after year, since training is never 'done.' They co-author the professional vocabulary — workshop model, mini-lessons, guided reading levels — against which classroom practice is evaluated.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__balanced_literacy_integration, workshop_training_institutes, beneficiary,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(reading_acquisition_legitimacy__balanced_literacy_integration, workshop_training_institutes, agenda_setter).

% Adopt the programs, mandate fidelity, and evaluate teachers against the model. They absorb pressure from school boards, publisher sales cycles, and latterly state legislation. Their exit is lateral: move to another district running the same orthodoxy.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__balanced_literacy_integration, district_literacy_directors, agenda_setter,
    institutional, biographical, mobile, regional).

% Implement the standard daily: mini-lessons, guided reading groups, literature circles, a phonics strand. They gain ready-made structure, coach support, and a professional community; they bear the lesson-preparation load, fidelity monitoring, and the risk of evaluation penalties when they add systematic decoding instruction beyond the program. Leaving means leaving teaching or moving to another school running the same model.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__balanced_literacy_integration, classroom_teachers, payer,
    organized, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(reading_acquisition_legitimacy__balanced_literacy_integration, classroom_teachers, beneficiary).

% Children from print-rich homes who arrive at school already decoding or close to it. They learn to read under almost any instruction and thrive on the literature-rich, meaning-first component. They cannot exit their assigned classroom; for them the standard's costs are mostly invisible.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__balanced_literacy_integration, proficient_background_readers, beneficiary,
    powerless, immediate, trapped, local).

% Children who need explicit, systematic, cumulative instruction in the alphabetic code. The standard's typical phonics strand — embedded, unsystematic, a mini-lesson here and there — does not teach it to them. They bear grade retention, late referral to remediation, tutoring their families may not afford, and the daily experience of watching peers read. Exit: none; attendance is compulsory and the instruction travels with the school.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__balanced_literacy_integration, struggling_dyslexic_readers, payer,
    powerless, immediate, trapped, local).

% Children without home literacy buffers who depend entirely on school instruction to learn to read. The standard's meaning-first emphasis and predictable-text guided reading teach them to guess from pictures and context rather than decode — a strategy that collapses once texts outgrow the pictures. They carry the largest developmental cost of the standard's under-specified decoding component.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__balanced_literacy_integration, disadvantaged_emergent_readers, payer,
    powerless, immediate, trapped, local).

% Mostly parents of dyslexic children who paid for private tutors, fought for evaluations, and were told to wait or that their children would catch up. They organized — litigation, legislative testimony, the science-of-reading advocacy movement — after years of being dismissed as anxious outliers. They bear the standard's costs directly in their children's outcomes and their own wallets.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__balanced_literacy_integration, parents_of_struggling_readers, payer,
    organized, biographical, constrained, national).

% The empirical community — reading researchers, cognitive psychologists, the National Reading Panel tradition — whose findings on systematic phonics, cueing, and the alphabetic principle accumulated for decades outside the curriculum-adoption conversation. They collect no rents from the standard; their seat is analytical, and their evidence now drives state legislation.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__balanced_literacy_integration, reading_scientists, observer,
    institutional, generational, analytical, global).

% Absent from curriculum adoption during the standard's consolidation decades, when instructional method was treated as a district and education-school matter. They entered through science-of-reading statutes — 40+ states by the mid-2020s — mandating structured literacy, cueing bans, and teacher retraining. Historically the excluded seat; now the primary counter-force.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__balanced_literacy_integration, state_education_legislators, excluded,
    institutional, generational, mobile, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(reading_acquisition_legitimacy__balanced_literacy_integration, balanced_literacy_publishers).
narrative_ontology:fixing_cost_class(reading_acquisition_legitimacy__balanced_literacy_integration, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single workable pedagogy for mixed-ability classrooms and a peace framework for a profession split by the reading wars: one materials set, one training pipeline, one professional vocabulary that lets a school serve strong readers (literature-rich instruction) and struggling readers (phonics intervention) without adopting either faction's exclusive program.
% TRANSFER_FUNCTION: Moves curriculum and professional-development dollars from districts and taxpayers to publishers and training institutes; moves instructional time toward meaning-making activity and away from explicit decoding practice; moves the deferred cost of under-taught decoding onto the learners least able to absorb it.
% ABSENT_VOICES: Struggling readers themselves — children cannot testify in adoption decisions. Reading scientists sat outside the education-school adoption loop for two decades and were dismissed as 'phonics zealots' when they objected. Parents of dyslexic children were told to wait. State legislators were absent from curriculum adoption entirely until the science-of-reading movement put them in the room.
% DISAPPEARANCE_RATIONALE: If the standard and its enforcement vanished overnight, districts would need replacement pedagogies and materials immediately, publishers and institutes would lose their market, teachers would lose their operating framework and evaluation criteria, and the daily literacy block would reorganize around whatever replaced it — the school day as currently constituted depends on it.
% FOUNDING_PROBLEM: The reading wars: whole-language and phonics factions made curriculum adoption politically impossible and left teachers without a usable framework. Balanced literacy was built as the peace treaty — a synthesis both factions could sign.
% FOUNDING_PROBLEM_CORROBORATION: Historians of education document the reading wars as the standard's founding context. The National Reading Panel (2000) and the subsequent reading-science literature attest that the synthesis left decoding under-taught for its most vulnerable targets. Legislative findings in 40+ states attest the problem the standard was meant to settle remained unresolved. Publisher marketing materials attest the 'balanced' framing as the selling point. Corroboration from outside the benefiting parties: the empirical reading-science literature and the state legislative findings; no corroborating source inside the beneficiary set is relied on.
narrative_ontology:disappearance_verdict(reading_acquisition_legitimacy__balanced_literacy_integration, world_rearranges).
narrative_ontology:founding_problem_status(reading_acquisition_legitimacy__balanced_literacy_integration, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(reading_acquisition_legitimacy__balanced_literacy_integration, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
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
 *   Extraction is 0.55: the standard's rents are real (curriculum and training dollars flowing to publishers and institutes; a deferred human cost landing on vulnerable readers) but partially pay for genuine materials and training, and this reading's own lights acknowledge the under-delivery its doctrine was supposed to prevent. Suppression is 0.55: persistence depended on adoption mandates, fidelity monitoring, evaluation penalties for off-program phonics emphasis, and the delegitimizing 'phonics zealot' framing of dissent — professional-orthodoxy enforcement, not physical coercion. Theater is 0.50: in market-leading programs the 'phonics strand' was often embedded, unsystematic, and nominal; guided-reading libraries of predictable texts taught guessing from pictures and context rather than decoding; 'balanced' functioned partly as branding — while literature engagement and writing instruction remained genuinely functional. Accessibility_collapse is 0.40: systematic phonics and structured-literacy programs remained commercially available throughout, and some districts chose them; the standard suppressed alternatives by delegitimizing them as extremes rather than eliminating them. Resistance is 0.60: sustained and eventually effective — the National Reading Panel, dyslexia parent litigation, investigative reporting, and science-of-reading statutes. Claim and metrics are independent authored facts: claimed_type tangled_rope reflects my structural assessment (genuine mixed-classroom coordination function plus asymmetric extraction plus active enforcement); the reading's own self-presentation is rope-like (the fair synthesis serving everyone), and the engine's per-seat computation will surface that divergence. All three measurement series run on one shared seven-point grid so every metric is authored at every examined time point; suppression_requirement is tracked because this story specifically traces enforcement-capacity change — enforcement machinery matured through adoption contracts and coach networks, then had to intensify to hold the gate against mounting parent, scientific, and legislative resistance.
 *
 * PERSPECTIVAL GAP:
 *   The payer and agenda-setter seats should compute different types from the same structural data. From the publisher and institute seats the standard is a product line and a training market they built and legitimately serve; from the classroom-teacher seat it is a framework that structures the day while constraining professional judgment; from the struggling-reader seat (voiced by parents and scientists) the same arrangement is years of missing instruction dressed as balance. The dual-positioned teacher seat is the pivot: the same teacher experiences the standard as support on Monday and as evaluation exposure when adding systematic decoding on Tuesday. The engine computes this divergence from power, exit, and role data; the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Publishers and training institutes sit nearest the beneficiary end: they collect the dollars, hold arbitrage-grade exit (the same editorial and training apparatus pivoted from whole-language products to balanced products and is now pivoting to science-of-reading materials), and their d is damped accordingly. Struggling dyslexic readers and disadvantaged emergent readers sit nearest the target end: powerless, trapped by compulsory attendance, bearing the deferred cost of under-taught decoding — their d is amplified. Proficient background readers are genuine beneficiaries (the literature-rich component serves them well) but trapped, which keeps them from arbitrage-grade positioning. Classroom teachers are mid-structure: dual role (payer of fidelity costs and foregone professional judgment; beneficiary of ready-made structure and professional community), organized power, constrained exit — the derivation places them near symmetric, slightly target-side, which matches their situation. No directionality overrides are used: the beneficiary/victim declarations plus exit options already produce the correct relationships, and the dual-role teacher case is handled by the secondary_role declaration rather than an override.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding mandate — ending the reading wars by giving both factions a signable peace treaty — is obsolete: the war ended by empirical attrition (the meaning-only position lost on evidence), not by the treaty's success, and the dispute has moved to a new front (balanced vs. structured). What persists is market position, professional identity, and institutional inertia. The classification discipline prevents mislabeling in both directions: a pure-extraction reading would miss the genuine coordination function (mixed-ability classrooms do need both instruction types, and the literature-rich component genuinely serves proficient readers); a pure-coordination reading would miss the asymmetric cost structure (the learners least served by nominal phonics are precisely those with no home-literacy buffer) and the rent flow to publishers and institutes. Tangled rope holds both truths: the same structure that lets a school operate is the structure that under-teaches its most vulnerable students while collecting recurring fees.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_committer_structure,
    'This constraint is one reading (balanced_literacy_integration) of the reading_acquisition_legitimacy kernel; which reading of the kernel should govern instructional legitimacy, and how would a sibling reading restructure this constraint?',
    'Adoption of a sibling reading as the governing standard (e.g., a state mandating structured literacy) replaces this constraint''s beneficiary/victim structure wholesale. The disagreement is located in two structural elements: the constitutive claim about what reading IS (decoding alone vs. decoding plus meaning-making) and the necessity/sufficiency claim about explicit instruction.',
    'Under phonics_decoding_primacy this gate''s literature-immersion and meaning-first components become illegitimate overhead (extraction rises, the victim set expands to all students subjected to cueing-based materials); under whole_language_meaning_primacy the explicit-phonics requirement drops out and this gate collapses toward its sibling; under structured_literacy_remediation the design center moves to the most vulnerable learners and the mixed-classroom balance claim loses its gate-keeping role entirely.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_committer_structure, conceptual, 'Committer structure: this story instantiates the balanced-integration reading; sibling readings instantiate different constraints over the same kernel and are linked via network edges.').

omega_variable(
    balance_vs_branding_ambiguity,
    'Is ''balance'' in the market-leading programs a genuine instructional synthesis, or a branding layer over meaning-first practice with a nominal phonics component?',
    'Materials audit plus classroom time-sampling quantifying explicit, systematic, cumulative phonics minutes and decodable-text use versus authentic-text and cueing-based activity, across publishers and implementation sites over the interval.',
    'If branding dominates, the theater_ratio is understated at interval end and the market-leading variants of the standard classify nearer pure extraction, with the coordination function carried only by the minority of genuine implementers.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(balance_vs_branding_ambiguity, empirical, 'Whether the balanced synthesis is substantively implemented or performatively labeled.').

omega_variable(
    vulnerable_reader_cost_attribution,
    'How much of struggling readers'' failure under this standard is attributable to the standard''s under-specified decoding component, versus poverty, late identification, and implementation variance?',
    'Controlled comparison of literacy outcomes across structured-literacy and balanced implementations holding demographics constant; Mississippi''s statewide structured-literacy transition serves as the natural experiment.',
    'Full attribution raises extraction and strengthens the victim structure; substantial confounding lowers extraction and reframes the story as implementation failure rather than structural extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(vulnerable_reader_cost_attribution, empirical, 'Attribution of the standard''s human cost to its structure versus context.').

omega_variable(
    teacher_identity_lock_mechanism,
    'How much of the standard''s persistence runs through teacher professional identity (workshop-model training as constitutive of being a real teacher) rather than through institutional mandate?',
    'Post-mandate trajectory: in states that repealed balanced-literacy orthodoxy, does classroom practice revert toward cueing and leveled texts when fidelity monitoring lapses? Persistence after mandate removal indicates identity fusion rather than enforcement dependence.',
    'If identity-fused, suppression persists after enforcement falls: the standard outlives its mandates by a teacher-generation and computed classifications lag legislation accordingly.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(teacher_identity_lock_mechanism, empirical, 'Identity-lock versus enforcement as the persistence mechanism.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(reading_acquisition_legitimacy__balanced_literacy_integration, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(read_tr_t0, reading_acquisition_legitimacy__balanced_literacy_integration, theater_ratio, 0, 0.28).
narrative_ontology:measurement(read_tr_t5, reading_acquisition_legitimacy__balanced_literacy_integration, theater_ratio, 5, 0.33).
narrative_ontology:measurement(read_tr_t10, reading_acquisition_legitimacy__balanced_literacy_integration, theater_ratio, 10, 0.38).
narrative_ontology:measurement(read_tr_t15, reading_acquisition_legitimacy__balanced_literacy_integration, theater_ratio, 15, 0.42).
narrative_ontology:measurement(read_tr_t20, reading_acquisition_legitimacy__balanced_literacy_integration, theater_ratio, 20, 0.46).
narrative_ontology:measurement(read_tr_t25, reading_acquisition_legitimacy__balanced_literacy_integration, theater_ratio, 25, 0.48).
narrative_ontology:measurement(read_tr_t30, reading_acquisition_legitimacy__balanced_literacy_integration, theater_ratio, 30, 0.5).

% Extraction over time
narrative_ontology:measurement(read_be_t0, reading_acquisition_legitimacy__balanced_literacy_integration, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(read_be_t5, reading_acquisition_legitimacy__balanced_literacy_integration, base_extractiveness, 5, 0.42).
narrative_ontology:measurement(read_be_t10, reading_acquisition_legitimacy__balanced_literacy_integration, base_extractiveness, 10, 0.46).
narrative_ontology:measurement(read_be_t15, reading_acquisition_legitimacy__balanced_literacy_integration, base_extractiveness, 15, 0.49).
narrative_ontology:measurement(read_be_t20, reading_acquisition_legitimacy__balanced_literacy_integration, base_extractiveness, 20, 0.52).
narrative_ontology:measurement(read_be_t25, reading_acquisition_legitimacy__balanced_literacy_integration, base_extractiveness, 25, 0.54).
narrative_ontology:measurement(read_be_t30, reading_acquisition_legitimacy__balanced_literacy_integration, base_extractiveness, 30, 0.55).

% Suppression requirement over time
narrative_ontology:measurement(read_su_t0, reading_acquisition_legitimacy__balanced_literacy_integration, suppression_requirement, 0, 0.32).
narrative_ontology:measurement(read_su_t5, reading_acquisition_legitimacy__balanced_literacy_integration, suppression_requirement, 5, 0.37).
narrative_ontology:measurement(read_su_t10, reading_acquisition_legitimacy__balanced_literacy_integration, suppression_requirement, 10, 0.42).
narrative_ontology:measurement(read_su_t15, reading_acquisition_legitimacy__balanced_literacy_integration, suppression_requirement, 15, 0.47).
narrative_ontology:measurement(read_su_t20, reading_acquisition_legitimacy__balanced_literacy_integration, suppression_requirement, 20, 0.51).
narrative_ontology:measurement(read_su_t25, reading_acquisition_legitimacy__balanced_literacy_integration, suppression_requirement, 25, 0.53).
narrative_ontology:measurement(read_su_t30, reading_acquisition_legitimacy__balanced_literacy_integration, suppression_requirement, 30, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(reading_acquisition_legitimacy__balanced_literacy_integration, identity_coordination).
narrative_ontology:affects_constraint(reading_acquisition_legitimacy__balanced_literacy_integration, phonics_decoding_primacy).
narrative_ontology:affects_constraint(reading_acquisition_legitimacy__balanced_literacy_integration, whole_language_meaning_primacy).
narrative_ontology:affects_constraint(reading_acquisition_legitimacy__balanced_literacy_integration, structured_literacy_remediation).

% DUAL FORMULATION NOTE:
% The colloquial 'reading wars' label covers one kernel — reading_acquisition_legitimacy — instantiated as at least four structurally distinct constraints (readings): phonics_decoding_primacy, whole_language_meaning_primacy, structured_literacy_remediation, and this balanced-integration reading. Each has its own epsilon, beneficiary/victim structure, and classification; they are linked here as a constraint family per the epsilon-invariance principle. This reading sits mid-kernel: it absorbs whole-language practice (literature immersion, workshop structures — hence the influences edge toward that sibling) while rejecting its exclusivity claim, and it competes with phonics primacy as a live factional position rather than eliminating it. Empirical drift (cueing research, science-of-reading statutes) pressures each reading's reference frame differently; the contest is modeled as separate linked stories, not one story with a measurement parameter.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
