% ============================================================================
% CONSTRAINT STORY: reading_acquisition_legitimacy__whole_language_meaning_primacy
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
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
 *   human_readable: Whole-Language Meaning-Primacy Legitimacy Norm
 *   domain: education_policy/cognitive_science/literacy_pedagogy
 *
 * SUMMARY:
 *   This story instantiates the whole_language_meaning_primacy reading of the
 *   contested kernel reading_acquisition_legitimacy as a single
 *   epsilon-invariant constraint: the institutionalized norm that legitimate
 *   reading instruction is meaning-first immersion in authentic literature,
 *   with decoding expected to emerge naturally and struggling readers
 *   supported through running records and guided reading rather than
 *   different instruction. The standing arrangement under contest is that
 *   norm as it operated across Anglophone education systems from its
 *   ascendency (mid-1980s), through its rebranding as balanced literacy
 *   (2000s), to its retreat under science-of-reading legislation (2019-2025).
 *   The claim/metric gap is deliberate and independent: the constraint is
 *   CLAIMED as tangled_rope, the author's structural judgment that the
 *   arrangement coordinates a genuine function (shared literature-rich
 *   pedagogy, a common professional language) while extracting asymmetrically
 *   from the children least equipped to survive it, while the metrics
 *   describe its actual operation, including three decades of enforcement
 *   machinery that held explicit-decoding alternatives out of bounds. Sibling
 *   readings (phonics_decoding_primacy, balanced_literacy_integration,
 *   structured_literacy_remediation) are separate constraints with their own
 *   epsilon values and victim sets; they are linked through the network, not
 *   averaged into this story. KEY AGENTS (by structural relationship):
 *   teacher_preparation_faculties (agenda-setter/beneficiary,
 *   institutional/identity_locked); leveled_literacy_publishers (primary
 *   beneficiary and gain seat, powerful/arbitrage);
 *   literacy_coaching_industry (beneficiary/enforcer, organized/mobile);
 *   classroom_teachers (coordinated beneficiary who partly pays,
 *   organized/identity_locked); struggling_readers and dyslexic_students
 *   (primary targets, powerless/trapped); families_of_struggling_readers
 *   (payers turned resisters, organized/constrained);
 *   proficient_home_supported_readers (incidental beneficiaries,
 *   powerless/constrained); cognitive_science_research_community (excluded
 *   voice, organized/constrained); state_education_agencies (agenda-setters
 *   with real exit, institutional/mobile).
 *
 * KEY AGENTS:
 *   - teacher_preparation_faculties: Agenda-setter + beneficiary (institutional/identity_locked) — define the legitimacy norm through certification requirements, syllabi, and the Goodman/Smith canon; their curricula, reputations, and enrollment are fused with the paradigm, so exit means rebuilding courses and surrendering standing.
 *   - leveled_literacy_publishers: Primary beneficiary (powerful/arbitrage) — sell leveled-text libraries, running-record kits, and PD keyed to the method; revenue depends on repurchase cycles that one-time explicit-decoding curricula would collapse; demonstrated capacity to rebrand when legitimacy shifts.
 *   - literacy_coaching_industry: Beneficiary + secondary agenda-setter (organized/mobile) — deliver the method into classrooms via PD contracts, coaching cycles, and institutes; enforce fidelity; recurring revenue that never completes.
 *   - classroom_teachers: Beneficiary + secondary payer (organized/identity_locked) — implement the method they were trained in; the norm defines their professionalism and spares them explicit phonics they were never taught to deliver; when it fails a child they lack diagnostic tools and are directed to provide more immersion; some covertly supplement at professional risk; retraining mandates now land on them.
 *   - struggling_readers: Primary payer (powerless/trapped) — children for whom meaning-first immersion does not produce decoding; they receive more immersion, leveled texts matched to their errors, and reassurance; the deficit compounds past grade 3; they cannot exit the pedagogy that fails them.
 *   - dyslexic_students: Primary payer, most ac harmed (powerless/trapped) — the tradition's canon explicitly denied their condition's instructional implications (reading disability framed as instructional artifact); they need explicit, systematic, cumulative code instruction that the legitimacy norm ruled out of bounds.
 *   - families_of_struggling_readers: Payer (organized/constrained) — pay for private tutoring and advocacy; told to read more at home; organized late (Decoding Dyslexia, right-to-read litigation) into the most effective resistance force; exit is private school or tutoring, both costly.
 *   - proficient_home_supported_readers: Beneficiary (powerless/constrained) — children who would decode under nearly any method because home exposure built the prerequisites; they receive a rich, low-drill literature classroom that serves them well; their visible success is cited as the norm's evidence.
 *   - cognitive_science_research_community: Excluded (organized/constrained) — produced the contradicting evidence base (Simple View of Reading, orthographic mapping, eye-tracking showing skilled readers do not guess); education faculties classified laboratory work as reductionist and inadmissible; entered policy only via journalism, litigation, and legislation after 2018.
 *   - state_education_agencies: Agenda-setter (institutional/mobile) — set certification and adoption frameworks; ratified the meaning-first norm for decades through standards and approved-materials lists; since 2019 many have flipped to mandating explicit instruction, demonstrating the exit the enforcement machinery had long priced out.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(reading_acquisition_legitimacy__whole_language_meaning_primacy, 0.58).
domain_priors:suppression_score(reading_acquisition_legitimacy__whole_language_meaning_primacy, 0.55).
domain_priors:theater_ratio(reading_acquisition_legitimacy__whole_language_meaning_primacy, 0.63).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(reading_acquisition_legitimacy__whole_language_meaning_primacy, extractiveness, 0.58).
narrative_ontology:constraint_metric(reading_acquisition_legitimacy__whole_language_meaning_primacy, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(reading_acquisition_legitimacy__whole_language_meaning_primacy, theater_ratio, 0.63).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(reading_acquisition_legitimacy__whole_language_meaning_primacy, accessibility_collapse, 0.52).
narrative_ontology:constraint_metric(reading_acquisition_legitimacy__whole_language_meaning_primacy, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(reading_acquisition_legitimacy__whole_language_meaning_primacy, tangled_rope).
narrative_ontology:human_readable(reading_acquisition_legitimacy__whole_language_meaning_primacy, "Whole-Language Meaning-Primacy Legitimacy Norm").
narrative_ontology:topic_domain(reading_acquisition_legitimacy__whole_language_meaning_primacy, "education_policy/cognitive_science/literacy_pedagogy").

domain_priors:requires_active_enforcement(reading_acquisition_legitimacy__whole_language_meaning_primacy).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(reading_acquisition_legitimacy__whole_language_meaning_primacy, '18304b86-7cb8-46d7-9afd-26d603b00ef2').
narrative_ontology:cs_kernel_codification('18304b86-7cb8-46d7-9afd-26d603b00ef2', distributed).
narrative_ontology:cs_authority_grounding('18304b86-7cb8-46d7-9afd-26d603b00ef2', lineage).
narrative_ontology:cs_interpretation_layer_present('18304b86-7cb8-46d7-9afd-26d603b00ef2').
narrative_ontology:cs_reading_relation('18304b86-7cb8-46d7-9afd-26d603b00ef2', reading_acquisition_legitimacy__phonics_decoding_primacy, forecloses).
narrative_ontology:cs_reading_relation('18304b86-7cb8-46d7-9afd-26d603b00ef2', reading_acquisition_legitimacy__structured_literacy_remediation, forecloses).
narrative_ontology:cs_reading_relation('18304b86-7cb8-46d7-9afd-26d603b00ef2', reading_acquisition_legitimacy__balanced_literacy_integration, influences).
narrative_ontology:cs_axiom('18304b86-7cb8-46d7-9afd-26d603b00ef2', foundational, reading_is_meaning_construction).
narrative_ontology:cs_axiom_status(reading_is_meaning_construction, holdable).
narrative_ontology:cs_axiom_grounding('18304b86-7cb8-46d7-9afd-26d603b00ef2', reading_is_meaning_construction, empirically_contingent).
narrative_ontology:cs_axiom('18304b86-7cb8-46d7-9afd-26d603b00ef2', foundational, decoding_emerges_from_meaningful_immersion).
narrative_ontology:cs_axiom_status(decoding_emerges_from_meaningful_immersion, holdable).
narrative_ontology:cs_axiom_grounding('18304b86-7cb8-46d7-9afd-26d603b00ef2', decoding_emerges_from_meaningful_immersion, empirically_contingent).
narrative_ontology:cs_axiom('18304b86-7cb8-46d7-9afd-26d603b00ef2', secondary, reading_disability_is_instructional_artifact).
narrative_ontology:cs_axiom_status(reading_disability_is_instructional_artifact, overridden).
narrative_ontology:cs_axiom_grounding('18304b86-7cb8-46d7-9afd-26d603b00ef2', reading_disability_is_instructional_artifact, empirically_contingent).
narrative_ontology:cs_reference_frame('18304b86-7cb8-46d7-9afd-26d603b00ef2', meaning_first_natural_immersion).
narrative_ontology:cs_drift_state('18304b86-7cb8-46d7-9afd-26d603b00ef2', contemporary_science_of_reading_era, gap(axiom_overriding, substantial, true)).
narrative_ontology:cs_created_at('18304b86-7cb8-46d7-9afd-26d603b00ef2', '').
narrative_ontology:cs_kernel_id(reading_acquisition_legitimacy__whole_language_meaning_primacy, reading_acquisition_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(reading_acquisition_legitimacy__whole_language_meaning_primacy, teacher_preparation_faculties).
narrative_ontology:constraint_beneficiary(reading_acquisition_legitimacy__whole_language_meaning_primacy, leveled_literacy_publishers).
narrative_ontology:constraint_beneficiary(reading_acquisition_legitimacy__whole_language_meaning_primacy, literacy_coaching_industry).
narrative_ontology:constraint_beneficiary(reading_acquisition_legitimacy__whole_language_meaning_primacy, classroom_teachers).
narrative_ontology:constraint_beneficiary(reading_acquisition_legitimacy__whole_language_meaning_primacy, proficient_home_supported_readers).
narrative_ontology:constraint_victim(reading_acquisition_legitimacy__whole_language_meaning_primacy, struggling_readers).
narrative_ontology:constraint_victim(reading_acquisition_legitimacy__whole_language_meaning_primacy, dyslexic_students).
narrative_ontology:constraint_victim(reading_acquisition_legitimacy__whole_language_meaning_primacy, families_of_struggling_readers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(reading_acquisition_legitimacy__whole_language_meaning_primacy, classroom_teachers).
narrative_ontology:constraint_vindicates(reading_acquisition_legitimacy__whole_language_meaning_primacy, goodman_psycholinguistic_guessing_theory).
narrative_ontology:constraint_vindicates(reading_acquisition_legitimacy__whole_language_meaning_primacy, three_cueing_instructional_model).
narrative_ontology:constraint_vindicates(reading_acquisition_legitimacy__whole_language_meaning_primacy, natural_reading_acquisition_hypothesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Define legitimate literacy pedagogy through certification requirements, course syllabi, and the Goodman/Smith canon, and trained the generations of teachers who carried the method into classrooms. Their curricula, scholarly reputations, and graduate enrollment depend on the paradigm remaining legitimate; exiting would mean rebuilding courses from the rival literature, admitting decades of error, and surrendering standing to departments they defined themselves against.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__whole_language_meaning_primacy, teacher_preparation_faculties, agenda_setter,
    institutional, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(reading_acquisition_legitimacy__whole_language_meaning_primacy, teacher_preparation_faculties, beneficiary).

% Sell leveled-text libraries, running-record assessment kits, and publisher-locked professional development keyed to the method. Revenue depends on continuous repurchase cycles — new leveled sets, new editions, new coaching series — that cheap, one-time explicit-decoding curricula would collapse. They have demonstrated the ability to rebrand wholesale when legitimacy shifts, which is what arbitrage looks like from inside publishing.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__whole_language_meaning_primacy, leveled_literacy_publishers, beneficiary,
    powerful, biographical, arbitrage, global).

% Deliver the method into classrooms through district PD contracts, coaching cycles, and summer institutes, and enforce fidelity to meaning-first practice through lesson observation and model classrooms. Their revenue is recurring by design — coaching never completes — and their brand can move to whichever pedagogy districts are buying next.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__whole_language_meaning_primacy, literacy_coaching_industry, beneficiary,
    organized, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(reading_acquisition_legitimacy__whole_language_meaning_primacy, literacy_coaching_industry, agenda_setter).

% Implement the method they were trained in, often the only one their preparation covered. The norm defines their professional identity as facilitators of meaning rather than deliverers of drills, and spares them explicit phonics instruction they were never taught to give. When the method fails a child they lack the diagnostic tools and are directed to provide more immersion and better-matched leveled texts. Some covertly supplement with phonics at professional risk; retraining mandates under the new laws now land on them as costs.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__whole_language_meaning_primacy, classroom_teachers, beneficiary,
    organized, biographical, identity_locked, local).
narrative_ontology:stakeholder_secondary_role(reading_acquisition_legitimacy__whole_language_meaning_primacy, classroom_teachers, payer).

% Children for whom meaning-first immersion does not produce decoding. They receive more immersion, leveled books matched to their error patterns, and reassurance that reading will come. The deficit compounds: a non-reader at grade 3 faces the text-intensive curriculum that follows with the one capacity it presupposes. They cannot leave the school, opt out of the pedagogy, or hire their own instruction.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__whole_language_meaning_primacy, struggling_readers, payer,
    powerless, biographical, trapped, local).

% The most ac harmed seat. The tradition's canon explicitly denied that their condition implied different instruction — reading disability was framed as an artifact of instruction, so the prescribed remedy was more of the same. What they need is explicit, systematic, cumulative code instruction, which the legitimacy norm ruled out of bounds. Many leave school functionally illiterate or are remediated privately at family expense, years late.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__whole_language_meaning_primacy, dyslexic_students, payer,
    powerless, biographical, trapped, local).

% Told to read more at home while watching their children fail under the school's method. They pay for private tutoring and diagnostic assessment the school would not provide, and they organized late — Decoding Dyslexia chapters, right-to-read litigation — into the most effective resistance force the constraint has faced. Their exit options are private school or private remediation, both costly, and neither available to the poorest families.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__whole_language_meaning_primacy, families_of_struggling_readers, payer,
    organized, biographical, constrained, local).

% Children who would decode under nearly any method because home exposure built the phonological prerequisites before school began. They get a rich, low-drill literature classroom that genuinely serves them — more books, more discussion, less worksheet. Their visible success is cited as evidence for the method, which makes them, without knowing it, part of the constraint's legitimacy machinery.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__whole_language_meaning_primacy, proficient_home_supported_readers, beneficiary,
    powerless, immediate, constrained, local).

% Reading scientists produced the evidence base that contradicts the method's causal premises: the Simple View of Reading, orthographic mapping, eye-tracking showing skilled readers recognize words orthographically rather than by guessing from context. For most of the interval they were outside the legitimacy-setting conversation — education faculties classified laboratory work as reductionist and inadmissible for classroom questions. They reached policy only when journalism, litigation, and legislation forced a seat after 2018.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__whole_language_meaning_primacy, cognitive_science_research_community, excluded,
    organized, generational, constrained, global).

% Set certification requirements, curriculum frameworks, and approved-materials lists. For decades they ratified the meaning-first norm through those instruments; since 2019 many have flipped by statute to mandating explicit, systematic instruction. The flip demonstrates that the exit the enforcement machinery had long priced out was always available to actors at this power level — the constraint held not because states could not leave but because leaving cost more than the states, as opposed to the children, bore.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__whole_language_meaning_primacy, state_education_agencies, agenda_setter,
    institutional, generational, mobile, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(reading_acquisition_legitimacy__whole_language_meaning_primacy, leveled_literacy_publishers).
narrative_ontology:fixing_cost_class(reading_acquisition_legitimacy__whole_language_meaning_primacy, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Aligns a vast, decentralized teaching force on one legitimate method: a shared professional language (miscue analysis, running records, text leveling), common materials ecosystems, and certification pipelines that reproduce practitioners — solving the real coordination problem of getting thousands of schools, publishers, and trainers pulling in one direction without central command.
% TRANSFER_FUNCTION: Moves curriculum and PD spending from explicit-decoding programs (cheap, one-time purchases) to the leveled-text and coaching economy (recurring revenue); moves professional authority to faculties and consultants; and moves the risk of literacy failure onto the children least able to bear it — those without home literacy scaffolds and those with dyslexia.
% ABSENT_VOICES: Reading scientists and dyslexia advocates sat outside the legitimacy-setting conversation for most of the interval: education faculties ruled laboratory evidence inadmissible for classroom questions, and phonics-program publishers were kept off approved-materials lists entirely. Parents of failing readers had no seat until journalism and right-to-read litigation forced one after 2018. Their absence is why unanimity inside the pedagogy field was never consensus about reading — only consensus about the norm, reached with the dissenting seats excluded from the room.
% DISAPPEARANCE_RATIONALE: If the norm vanished overnight, certification syllabi, approved-materials lists, the leveled-text market, and the coaching economy would reorganize around explicit-code instruction within an adoption cycle or two, and the roughly third of children the norm fails would receive different instruction. The rebranding history — whole language to balanced literacy to science-of-reading-flavored product lines — shows the apparatus rearranges rather than dissolves; the seats are too invested to evaporate quietly.
% FOUNDING_PROBLEM: Mid-century look-say and basal-reader instruction produced children who could call words but comprehended poorly and disliked reading; Goodman and Smith proposed meaning-first immersion in authentic literature to restore purpose, engagement, and real language use to reading instruction.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem itself — comprehension and engagement deficits under code-only drill — is corroborated from outside the benefiting parties by the comprehension-instruction literature and historical reading surveys. But no source outside the movement attests that decoding reliably emerges from immersion for all children: the National Reading Panel (2000), the UK Rose Review (2006), the Australian National Inquiry into the Teaching of Literacy (2005), and the orthographic-mapping literature attest the opposite for a substantial minority, and the movement's own institutions (post-2020 publisher statements, revised curricula) now concede the vulnerable-learner exception. The universal-emergence claim is attested only by the parties that benefit from it.
narrative_ontology:disappearance_verdict(reading_acquisition_legitimacy__whole_language_meaning_primacy, world_rearranges).
narrative_ontology:founding_problem_status(reading_acquisition_legitimacy__whole_language_meaning_primacy, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(reading_acquisition_legitimacy__whole_language_meaning_primacy, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(reading_acquisition_legitimacy__whole_language_meaning_primacy, 'none', 1).
narrative_ontology:epsilon_provenance(reading_acquisition_legitimacy__whole_language_meaning_primacy, 0.58, 'stealth/ox-alpha', 'none', direct).

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
 *   Extractiveness is substantial but not extreme (0.58 at interval end, peaking 0.69 around 2000) because the arrangement's costs are concentrated rather than diffuse: the majority of children, especially those with home literacy scaffolds, learn to read under nearly any method and genuinely benefit from the literature-rich classroom; the roughly third who do not, disproportionately dyslexic and home-disadvantaged children, bear compounding lifetime costs. Suppression (0.55) is authored as the raw structural property it is — unscaled by directionality or scope, per the framework's rule; it reflects certification gatekeeping, approved-list exclusion of decodable programs, and the delegitimization of explicit instruction as 'drill and kill.' Theater rises monotonically (0.18 to 0.63) because each empirical refutation was absorbed by rebrand rather than revision: cosmetic phonics added in the balanced-literacy rebrand while cueing stayed in the materials, running records and text leveling performed assessment without changing instruction, and by 2020 publishers publicly denied teaching three-cueing while shipping it. The suppression_requirement series tracks enforcement-capacity change, which is this story's central dynamic: machinery built through certification and mandate (1985-2000), cheapened by co-optation at the rebrand (2005), rebuilt defensively against journalism and legislation (2015-2020), and now eroding as statutes bite (2025). All three series share one time grid so no metric is sampled against another's end-state. The non-monotonic arc is the field's pendulum, not noise: challenge, rebrand, consolidation, challenge. The rebrand cycle is itself partly an extraction mechanism — intermittent reassurance that 'the new edition fixes everything' let the apparatus survive each refutation with its repurchase economics intact. Accessibility collapse is moderate (0.52): alternatives were severely collapsed for insiders at the peak (phonics programs effectively unpurchasable in whole-language-mandated systems), but never vanished, and the science-of-reading revival shows the exit was always structurally available to actors with institutional power. Resistance is high (0.72): parents, reading scientists, journalists, litigators, and eventually legislators mounted sustained, organized, and ultimately effective opposition — the rare construct constraint that is being beaten in the open.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute different types from the same structure, and the divergence is the finding. From the faculty, publisher, and coach seats the arrangement is a rope: a professional community with shared purpose, real literature benefits, and a coherent account of its practice. From the dyslexic child's seat it is a snare: the instruction that would work is ruled illegitimate, the condition's instructional implications were denied outright, and exit is impossible from inside childhood. From the classroom teacher's seat it is a tangled rope in the strict sense: the norm subsidizes professional identity while stripping the tools needed when it fails. Proficient readers experience a benign rope and their success is the movement's standing evidence — which is precisely why the extraction stayed invisible for decades. The engine computes this per-seat divergence from the structural data; the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries derive low directionality: publishers and coaches collect the materials and PD economy directly; faculties collect enrollment and standing; proficient readers are subsidized without paying (their d is near the beneficiary end despite having no power). Payers derive high directionality with exit structure pushing them toward the full-target end: children are trapped by law and age, families are constrained by cost, and identity_lock keeps teachers mid-range despite their beneficiary role. No directionality overrides are authored: the beneficiary/victim declarations plus exit options differentiate the seats adequately, and the override mechanism keys on power atoms, which would smear a teacher-specific correction across the other organized actors. Scope amplification is modest: the constraint operates at national scale with verification difficulty concentrated inside classrooms, which is exactly where the extraction hid.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem was real: mid-century look-say instruction produced children who could call words but comprehended poorly and disliked reading, and the meaning-first movement genuinely addressed that problem for much of the cohort. The mandate that has outlived its warrant is the universal-natural-emergence claim: dead for the vulnerable minority since at least the National Reading Panel, yet the arrangement persisted not by sunset but by metamorphosis — whole language became balanced literacy became 'rich meaningful literacy' while the leveled-text and coaching economics stayed constant. This is mandatrophy via rebrand, which is why the theater series rises monotonically while extractiveness oscillates. The classification as tangled_rope prevents both symmetrical errors: reading the arrangement as pure rope would erase the extraction from children who cannot vote with their feet; reading it as pure snare would erase the genuine coordination and literature function that serves the majority and would mispredict the behavior of the teachers and faculties who defend it in good faith. The founding_problem_status (contested) crossed with the disappearance verdict (world_rearranges) is the honest cell: the engagement problem is live, the natural-emergence mechanism is not, and the apparatus that conflates them is what persists.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_commitment_structure,
    'This constraint is one reading of the kernel reading_acquisition_legitimacy. What would the sibling readings (phonics_decoding_primacy, balanced_literacy_integration, structured_literacy_remediation) change structurally, and where exactly is the disagreement located?',
    'Compare the sibling stories'' beneficiary/victim structures and epsilon values: the disagreement is located in the causal structure of acquisition (meaning-driven versus code-driven) and in the referent of legitimate instruction (immersion fidelity versus explicit-code fidelity versus vulnerability-first design).',
    'If the code-driven reading is correct, this reading''s beneficiary/victim structure inverts: its support apparatus (running records, leveled texts, guided reading) becomes the extraction mechanism and explicit phonics becomes the subsidy; the constraint family''s classification set redistributes across the siblings.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_commitment_structure, conceptual, 'Committer structure: one reading of the reading-acquisition-legitimacy kernel; sibling deltas and disagreement location.').

omega_variable(
    natural_acquisition_population_scope,
    'Does decoding emerge naturally from immersion for all children, or only for the subset whose home environment builds the phonological prerequisites, and what share of each cohort does each population represent?',
    'Longitudinal studies crossing instructional method with home-literacy measures; the existing convergent record (National Reading Panel meta-analyses, Rose Review, Australian National Inquiry, orthographic-mapping research) already indicates method effects concentrate on home-disadvantaged and phonologically vulnerable children.',
    'If emergence holds only for a subset, the norm''s universal application is mis-scoped: its extraction is concentrated on the complement subset rather than diffusely mild, and classification drifts toward snare for the affected cohort.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(natural_acquisition_population_scope, empirical, 'Whether the natural-emergence regularity is universal or subset-scoped.').

omega_variable(
    teacher_suppression_mechanism_split,
    'Is teachers'' non-delivery of explicit code instruction structural (no training, no materials, certification risk) or internalized (trained belief that explicit phonics harms children), and in what proportion?',
    'Post-mandate practice trajectory: in states that mandated science-of-reading retraining, track whether classroom practice durably shifts or reverts to cueing when enforcement relaxes; survey teachers trained before 2019 to separate belief from barrier.',
    'If substantially internalized, effective suppression outlasts the structural machinery: the constraint persists in teacher practice after certification and adoption rules change, and remediation timelines lengthen.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(teacher_suppression_mechanism_split, empirical, 'Structural versus internalized suppression in the classroom-teacher seat.').

omega_variable(
    correction_displacement_or_pendulum,
    'Is the current science-of-reading correction a permanent displacement of the meaning-primacy norm, or another swing of the field''s roughly 30-year pendulum (look-say to phonics-heavy to whole language to balanced literacy to explicit instruction)?',
    'Durability test across one full political and teacher-generation cycle: whether state statutes survive, teacher-preparation syllabi actually change, and publisher catalogs remain explicit-code after the advocacy coalition demobilizes.',
    'If pendulum, the theater and suppression series should be modeled as re-inflating within a decade and the classification should expect revival pressure; if displacement, this story''s end-state measurements are terminal rather than cyclical.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(correction_displacement_or_pendulum, empirical, 'Whether the reading''s retreat is terminal or cyclical.').

omega_variable(
    identity_coordination_cover_status,
    'Is the identity-coordination function genuine (a professional community''s real boundary maintenance around progressive literacy values) or substantially cover for the materials-and-authority extraction the same structure delivers?',
    'Counterfactual test: whether the community''s coordination functions (shared professional language, literature culture, teacher autonomy) survive removal of the extraction channels (leveled-text repurchase cycles, exclusive PD contracts); observe districts that adopted explicit curricula while keeping literature-rich practice.',
    'If the identity function survives extraction removal, the tangled_rope reading is confirmed with genuine coordination; if it collapses, the identity framing was cover and the arrangement reclassifies toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_coordination_cover_status, conceptual, 'Whether identity framing is genuine coordination or extraction cover (FNL gaming check on identity_coordination).').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(reading_acquisition_legitimacy__whole_language_meaning_primacy, 1985, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(read_tr_t1985, reading_acquisition_legitimacy__whole_language_meaning_primacy, theater_ratio, 1985, 0.18).
narrative_ontology:measurement_basis(read_tr_t1985, observed).
narrative_ontology:measurement(read_tr_t1990, reading_acquisition_legitimacy__whole_language_meaning_primacy, theater_ratio, 1990, 0.26).
narrative_ontology:measurement_basis(read_tr_t1990, observed).
narrative_ontology:measurement(read_tr_t1995, reading_acquisition_legitimacy__whole_language_meaning_primacy, theater_ratio, 1995, 0.34).
narrative_ontology:measurement_basis(read_tr_t1995, observed).
narrative_ontology:measurement(read_tr_t2000, reading_acquisition_legitimacy__whole_language_meaning_primacy, theater_ratio, 2000, 0.4).
narrative_ontology:measurement_basis(read_tr_t2000, observed).
narrative_ontology:measurement(read_tr_t2005, reading_acquisition_legitimacy__whole_language_meaning_primacy, theater_ratio, 2005, 0.48).
narrative_ontology:measurement_basis(read_tr_t2005, observed).
narrative_ontology:measurement(read_tr_t2010, reading_acquisition_legitimacy__whole_language_meaning_primacy, theater_ratio, 2010, 0.52).
narrative_ontology:measurement_basis(read_tr_t2010, observed).
narrative_ontology:measurement(read_tr_t2015, reading_acquisition_legitimacy__whole_language_meaning_primacy, theater_ratio, 2015, 0.56).
narrative_ontology:measurement_basis(read_tr_t2015, observed).
narrative_ontology:measurement(read_tr_t2020, reading_acquisition_legitimacy__whole_language_meaning_primacy, theater_ratio, 2020, 0.6).
narrative_ontology:measurement_basis(read_tr_t2020, observed).
narrative_ontology:measurement(read_tr_t2025, reading_acquisition_legitimacy__whole_language_meaning_primacy, theater_ratio, 2025, 0.63).
narrative_ontology:measurement_basis(read_tr_t2025, observed).

% Extraction over time
narrative_ontology:measurement(read_be_t1985, reading_acquisition_legitimacy__whole_language_meaning_primacy, base_extractiveness, 1985, 0.42).
narrative_ontology:measurement_basis(read_be_t1985, observed).
narrative_ontology:measurement(read_be_t1990, reading_acquisition_legitimacy__whole_language_meaning_primacy, base_extractiveness, 1990, 0.54).
narrative_ontology:measurement_basis(read_be_t1990, observed).
narrative_ontology:measurement(read_be_t1995, reading_acquisition_legitimacy__whole_language_meaning_primacy, base_extractiveness, 1995, 0.66).
narrative_ontology:measurement_basis(read_be_t1995, observed).
narrative_ontology:measurement(read_be_t2000, reading_acquisition_legitimacy__whole_language_meaning_primacy, base_extractiveness, 2000, 0.69).
narrative_ontology:measurement_basis(read_be_t2000, observed).
narrative_ontology:measurement(read_be_t2005, reading_acquisition_legitimacy__whole_language_meaning_primacy, base_extractiveness, 2005, 0.63).
narrative_ontology:measurement_basis(read_be_t2005, observed).
narrative_ontology:measurement(read_be_t2010, reading_acquisition_legitimacy__whole_language_meaning_primacy, base_extractiveness, 2010, 0.66).
narrative_ontology:measurement_basis(read_be_t2010, observed).
narrative_ontology:measurement(read_be_t2015, reading_acquisition_legitimacy__whole_language_meaning_primacy, base_extractiveness, 2015, 0.68).
narrative_ontology:measurement_basis(read_be_t2015, observed).
narrative_ontology:measurement(read_be_t2020, reading_acquisition_legitimacy__whole_language_meaning_primacy, base_extractiveness, 2020, 0.61).
narrative_ontology:measurement_basis(read_be_t2020, observed).
narrative_ontology:measurement(read_be_t2025, reading_acquisition_legitimacy__whole_language_meaning_primacy, base_extractiveness, 2025, 0.58).
narrative_ontology:measurement_basis(read_be_t2025, observed).

% Suppression requirement over time
narrative_ontology:measurement(read_su_t1985, reading_acquisition_legitimacy__whole_language_meaning_primacy, suppression_requirement, 1985, 0.34).
narrative_ontology:measurement_basis(read_su_t1985, observed).
narrative_ontology:measurement(read_su_t1990, reading_acquisition_legitimacy__whole_language_meaning_primacy, suppression_requirement, 1990, 0.46).
narrative_ontology:measurement_basis(read_su_t1990, observed).
narrative_ontology:measurement(read_su_t1995, reading_acquisition_legitimacy__whole_language_meaning_primacy, suppression_requirement, 1995, 0.56).
narrative_ontology:measurement_basis(read_su_t1995, observed).
narrative_ontology:measurement(read_su_t2000, reading_acquisition_legitimacy__whole_language_meaning_primacy, suppression_requirement, 2000, 0.62).
narrative_ontology:measurement_basis(read_su_t2000, observed).
narrative_ontology:measurement(read_su_t2005, reading_acquisition_legitimacy__whole_language_meaning_primacy, suppression_requirement, 2005, 0.5).
narrative_ontology:measurement_basis(read_su_t2005, observed).
narrative_ontology:measurement(read_su_t2010, reading_acquisition_legitimacy__whole_language_meaning_primacy, suppression_requirement, 2010, 0.54).
narrative_ontology:measurement_basis(read_su_t2010, observed).
narrative_ontology:measurement(read_su_t2015, reading_acquisition_legitimacy__whole_language_meaning_primacy, suppression_requirement, 2015, 0.57).
narrative_ontology:measurement_basis(read_su_t2015, observed).
narrative_ontology:measurement(read_su_t2020, reading_acquisition_legitimacy__whole_language_meaning_primacy, suppression_requirement, 2020, 0.63).
narrative_ontology:measurement_basis(read_su_t2020, observed).
narrative_ontology:measurement(read_su_t2025, reading_acquisition_legitimacy__whole_language_meaning_primacy, suppression_requirement, 2025, 0.52).
narrative_ontology:measurement_basis(read_su_t2025, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(reading_acquisition_legitimacy__whole_language_meaning_primacy, identity_coordination).
narrative_ontology:affects_constraint(reading_acquisition_legitimacy__whole_language_meaning_primacy, phonics_decoding_primacy).
narrative_ontology:affects_constraint(reading_acquisition_legitimacy__whole_language_meaning_primacy, balanced_literacy_integration).
narrative_ontology:affects_constraint(reading_acquisition_legitimacy__whole_language_meaning_primacy, structured_literacy_remediation).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'the reading wars' conflates four structurally distinct claims about the same kernel (reading_acquisition_legitimacy), each with its own epsilon, beneficiary/victim structure, and enforcement machinery. This story is the whole_language_meaning_primacy member: its epsilon is authored over the meaning-first arrangement as it actually operated, not over the phonics alternative this reading opposes and not over the balanced compromise. The upstream/downstream structure runs through institutional dominance: this reading's mid-interval dominance shaped balanced_literacy_integration's design (the rebrand preserved cueing and leveling), and the cognitive-science evidence base that refutes this reading's foundational axioms is the same evidence base that legitimates structured_literacy_remediation. Sibling stories should link back here.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
