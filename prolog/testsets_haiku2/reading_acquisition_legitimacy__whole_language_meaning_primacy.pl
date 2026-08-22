% ============================================================================
% CONSTRAINT STORY: reading_acquisition_legitimacy__whole_language_meaning_primacy
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   human_readable: Whole Language Reading Instruction (Meaning-Primacy Reading)
 *   domain: education_policy/cognitive_science/literacy_pedagogy
 *
 * SUMMARY:
 *   Whole-language reading instruction emerged in the 1970s-80s as a response
 *   to mechanistic phonics programs, emphasizing meaning-making, authentic
 *   literature, and student-centered learning. The reading instantiates the
 *   claim that reading IS meaning construction and that legitimate
 *   instruction immerses children in rich texts while allowing phonetic
 *   principles to emerge naturally through guided exposure and discussion.
 *   This reading coexists with sibling readings (phonics-primacy, structured
 *   literacy, balanced literacy) in ongoing dispute. The constraint presents
 *   as coordination (organizing instruction around meaningful reading) but
 *   extracts asymmetrically from phonologically vulnerable learners who
 *   cannot reliably infer phonetic principles from immersion alone. The
 *   narrative context treats the constraint as a reading of a kernel—one
 *   party's epistemic commitment about what reading is and how it should be
 *   taught—not as a simple pedagogical choice.
 *
 * KEY AGENTS:
 *   - early_literacy_instructors: agenda-setters (identity-locked in child-centered pedagogy) who adopt whole-language frameworks
 *   - struggling_decoders and students_with_dyslexia: powerless, trapped victims who bear the cost of ineffective instructional design
 *   - constructivist_educational_theorists and child_centered_pedagogy_advocates: institutional beneficiaries whose theoretical authority is vindicated by the reading's adoption
 *   - structured_literacy_researchers: excluded institutional voice blocked from curriculum decision-making
 *   - school_administrators: institutional agenda-setters with identity-lock in progressive education, facing pressure from divergent evidence
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(reading_acquisition_legitimacy__whole_language_meaning_primacy, 0.62).
domain_priors:suppression_score(reading_acquisition_legitimacy__whole_language_meaning_primacy, 0.58).
domain_priors:theater_ratio(reading_acquisition_legitimacy__whole_language_meaning_primacy, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(reading_acquisition_legitimacy__whole_language_meaning_primacy, extractiveness, 0.62).
narrative_ontology:constraint_metric(reading_acquisition_legitimacy__whole_language_meaning_primacy, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(reading_acquisition_legitimacy__whole_language_meaning_primacy, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(reading_acquisition_legitimacy__whole_language_meaning_primacy, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(reading_acquisition_legitimacy__whole_language_meaning_primacy, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(reading_acquisition_legitimacy__whole_language_meaning_primacy, tangled_rope).
narrative_ontology:human_readable(reading_acquisition_legitimacy__whole_language_meaning_primacy, "Whole Language Reading Instruction (Meaning-Primacy Reading)").
narrative_ontology:topic_domain(reading_acquisition_legitimacy__whole_language_meaning_primacy, "education_policy/cognitive_science/literacy_pedagogy").

domain_priors:requires_active_enforcement(reading_acquisition_legitimacy__whole_language_meaning_primacy).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(reading_acquisition_legitimacy__whole_language_meaning_primacy, 'b15f3ac0-1a48-45bb-b618-ef13088674a5').
narrative_ontology:cs_kernel_codification('b15f3ac0-1a48-45bb-b618-ef13088674a5', distributed).
narrative_ontology:cs_authority_grounding('b15f3ac0-1a48-45bb-b618-ef13088674a5', extraction).
narrative_ontology:cs_interpretation_layer_present('b15f3ac0-1a48-45bb-b618-ef13088674a5').
narrative_ontology:cs_reading_relation('b15f3ac0-1a48-45bb-b618-ef13088674a5', reading_acquisition_legitimacy__phonics_decoding_primacy, coexists_with).
narrative_ontology:cs_reading_relation('b15f3ac0-1a48-45bb-b618-ef13088674a5', reading_acquisition_legitimacy__balanced_literacy_integration, influences).
narrative_ontology:cs_reading_relation('b15f3ac0-1a48-45bb-b618-ef13088674a5', reading_acquisition_legitimacy__structured_literacy_remediation, coexists_with).
narrative_ontology:cs_axiom('b15f3ac0-1a48-45bb-b618-ef13088674a5', foundational, literacy_emergence_natural_immersion).
narrative_ontology:cs_axiom_status(literacy_emergence_natural_immersion, holdable).
narrative_ontology:cs_axiom_grounding('b15f3ac0-1a48-45bb-b618-ef13088674a5', literacy_emergence_natural_immersion, empirically_contingent).
narrative_ontology:cs_axiom('b15f3ac0-1a48-45bb-b618-ef13088674a5', foundational, student_autonomy_intrinsic_reading_engagement).
narrative_ontology:cs_axiom_status(student_autonomy_intrinsic_reading_engagement, holdable).
narrative_ontology:cs_axiom_grounding('b15f3ac0-1a48-45bb-b618-ef13088674a5', student_autonomy_intrinsic_reading_engagement, deontological).
narrative_ontology:cs_reference_frame('b15f3ac0-1a48-45bb-b618-ef13088674a5', authentic_literature_immersion_framework).
narrative_ontology:cs_drift_state('b15f3ac0-1a48-45bb-b618-ef13088674a5', contemporary_neuroscience_era_2010s_2020s, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('b15f3ac0-1a48-45bb-b618-ef13088674a5', '').
narrative_ontology:cs_kernel_id(reading_acquisition_legitimacy__whole_language_meaning_primacy, reading_acquisition_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(reading_acquisition_legitimacy__whole_language_meaning_primacy, early_literacy_instructors).
narrative_ontology:constraint_beneficiary(reading_acquisition_legitimacy__whole_language_meaning_primacy, constructivist_educational_theorists).
narrative_ontology:constraint_beneficiary(reading_acquisition_legitimacy__whole_language_meaning_primacy, child_centered_pedagogy_advocates).
narrative_ontology:constraint_victim(reading_acquisition_legitimacy__whole_language_meaning_primacy, struggling_decoders).
narrative_ontology:constraint_victim(reading_acquisition_legitimacy__whole_language_meaning_primacy, students_with_dyslexia).
narrative_ontology:constraint_victim(reading_acquisition_legitimacy__whole_language_meaning_primacy, phonologically_vulnerable_learners).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(reading_acquisition_legitimacy__whole_language_meaning_primacy, parents_of_struggling_readers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Adopt whole-language instructional frameworks, select authentic literature for early readers, and facilitate meaning-making through guided discussion and individual reading conferences. They author reading instruction policy at the school level, validate the approach through running records and anecdotal observation, and defend it against structured-literacy critiques. Professional identity is invested in student-centered pedagogy.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__whole_language_meaning_primacy, early_literacy_instructors, agenda_setter,
    organized, biographical, identity_locked, national).

% Receive minimal explicit instruction in phonemic awareness and letter-sound correspondences; instead are expected to infer decoding principles from repeated exposure to authentic texts and contextualized guessing. When comprehension fails, they receive additional 'guided reading' at matched difficulty levels, but the underlying decoding deficit is often not directly addressed. Exit from the classroom (school choice) is constrained by geography and family resources.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__whole_language_meaning_primacy, struggling_decoders, payer,
    powerless, biographical, trapped, local).

% Have neurological differences in phonological processing that make decoding inference from context and immersion ineffective. Whole-language approaches often fail to identify their need for intensive, explicit phonetic instruction until years of reading failure accumulate. They are frequently not diagnosed until third grade or later, and special education referral becomes the de facto remediation. Excluded from early intervention and from the assumption that they will benefit from the mainstream instructional approach.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__whole_language_meaning_primacy, students_with_dyslexia, payer,
    powerless, biographical, constrained, local).
narrative_ontology:stakeholder_secondary_role(reading_acquisition_legitimacy__whole_language_meaning_primacy, students_with_dyslexia, excluded).

% Are children from literacy-thin households, with limited oral vocabulary and weak phonological awareness entering school. In a whole-language environment, they are expected to extract phonetic principles from text immersion and contextualized reading, but their weak phonological foundations mean incidental learning from context is slower or fails entirely. Private tutoring or explicit instruction at home can partially compensate, but school-level intervention is often delayed pending formalized testing.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__whole_language_meaning_primacy, phonologically_vulnerable_learners, payer,
    moderate, biographical, constrained, local).

% The whole-language reading framework instantiates their broader epistemological commitment: that learners construct knowledge through active engagement with authentic materials, not through transmission of discrete skills. Their theoretical authority is vindicated when schools adopt whole-language approaches. They produce the research, write the curriculum guides, and serve as keynote speakers at teacher-training institutes. Publication and reputation accrue when the approach is adopted and defended.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__whole_language_meaning_primacy, constructivist_educational_theorists, beneficiary,
    institutional, generational, mobile, national).

% Champion student autonomy, intrinsic motivation, and the reduction of top-down instructional control. Whole-language reading aligns with this broader movement: minimal teacher prescription, student choice in reading materials, organic emergence of skills. This reading's success is evidence for their larger project of transforming school culture away from skills-based, standardized instruction.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__whole_language_meaning_primacy, child_centered_pedagogy_advocates, beneficiary,
    organized, generational, mobile, national).

% Conduct research on explicit, systematic phonics and structured language instruction, particularly with struggling readers and students with dyslexia. Their findings consistently show that incidental phonetic learning is insufficient for these populations. However, whole-language advocates control curriculum committees and teacher training in many districts, which limits their ability to change practice or to secure funding for alternative interventions. Their voice is structurally excluded from the early-literacy instruction forum until reading failure becomes undeniable.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__whole_language_meaning_primacy, structured_literacy_researchers, excluded,
    institutional, generational, trapped, national).

% Watch their children fall behind in reading while being told by schools that 'all children develop at different rates' and that explicit phonics instruction would damage intrinsic motivation. They pay privately for tutoring or seek school choice (charter, private, or homeschool options), but these alternatives are costly or unavailable. They are excluded from curriculum decisions and are often made to feel that their concern is pedagogically unsophisticated.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__whole_language_meaning_primacy, parents_of_struggling_readers, payer,
    moderate, biographical, constrained, local).
narrative_ontology:stakeholder_secondary_role(reading_acquisition_legitimacy__whole_language_meaning_primacy, parents_of_struggling_readers, excluded).

% Adopt whole-language reading policies district-wide, guided by educational philosophy, published research on child-centered learning, and professional networks within educational administration. They are incentivized to reduce standardized test accountability pressure by adopting approaches that claim intrinsic motivation drives learning. Over time, they face pressure from parent complaints and achievement data that contradicts whole-language claims. Their identity as progressive educators and their professional networks often make reversal costly.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__whole_language_meaning_primacy, school_administrators, agenda_setter,
    institutional, biographical, identity_locked, regional).
narrative_ontology:stakeholder_secondary_role(reading_acquisition_legitimacy__whole_language_meaning_primacy, school_administrators, observer).

% Brain imaging studies show that skilled reading recruits left-hemisphere phonological pathways; intervention studies on dyslexia consistently show that explicit phonics instruction activates these pathways and improves decoding outcomes. This body of evidence is external to the pedagogical dispute and contradicts whole-language claims, but is often discounted as reductionist or misapplied to classroom practice.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__whole_language_meaning_primacy, neuroscientific_evidence, observer,
    analytical, civilizational, analytical, global).
narrative_ontology:stakeholder_non_agent(reading_acquisition_legitimacy__whole_language_meaning_primacy, neuroscientific_evidence).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(reading_acquisition_legitimacy__whole_language_meaning_primacy, early_literacy_instructors).
narrative_ontology:fixing_cost_class(reading_acquisition_legitimacy__whole_language_meaning_primacy, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Organizes early literacy instruction around authentic literature, meaning-making, and student choice. Solves the coordination problem of how to structure classroom time and teacher training such that reading is presented as purposeful communication rather than decoding drills, and such that teacher and student relationships emphasize collaborative learning.
% TRANSFER_FUNCTION: Transfers instructional authority from explicit phonics sequences to teacher-guided immersion in authentic texts; transfers validation of reading proficiency from decoding accuracy to comprehension and motivation. Also transfers the burden of inferring phonetic principles onto learners themselves, which is where struggling decoders encounter extraction: they bear the cost of self-discovery while others (better phonologically prepared) extract the principles readily.
% ABSENT_VOICES: Neuroscientists researching the phonological basis of reading acquisition, speech-language pathologists who work with dyslexic children, and parents whose children are reading below grade level in whole-language schools. These voices would argue for the necessity of explicit phonetic instruction but are structurally excluded from curriculum committees and teacher-training forums dominated by child-centered pedagogy networks.
% DISAPPEARANCE_RATIONALE: If whole-language reading instruction disappeared, schools would adopt alternative literacy frameworks (phonics-based, structured literacy, or balanced approaches). The shift would immediately alter teacher training, curriculum selection, assessment practices, and resource allocation toward phonetically explicit materials. Reading outcomes for struggling decoders would likely improve, and special education referrals would decrease. The distribution of reading proficiency by socioeconomic status and phonological vulnerability would shift.
% FOUNDING_PROBLEM: Early reading instruction in the 1970s–80s was dominated by skill-and-drill phonics programs that were mechanistic, unmotivating, and did not foster comprehension or a love of reading. Whole-language reading emerged as a response: to restore meaning, authentic literature, and intrinsic motivation to the early literacy classroom.
% FOUNDING_PROBLEM_CORROBORATION: Whole-language advocates attest the skill-and-drill reading was unmotivating and that meaning-based instruction restores engagement. However, longitudinal studies from the 1990s onward (reported by neuroscientists, speech-language pathologists, and researchers studying dyslexia) attest that the founding problem was overstated: the empirical question was not phonics vs. meaning, but EXPLICIT phonics + meaning vs. incidental phonics + meaning. Whole-language advocates dismiss this research as reductionist, but independent meta-analyses (particularly the National Reading Panel 2000 and subsequent systematic reviews) corroborate the structured-literacy position that explicit phonics is necessary alongside meaning-making.
narrative_ontology:disappearance_verdict(reading_acquisition_legitimacy__whole_language_meaning_primacy, world_rearranges).
narrative_ontology:founding_problem_status(reading_acquisition_legitimacy__whole_language_meaning_primacy, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(reading_acquisition_legitimacy__whole_language_meaning_primacy, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(reading_acquisition_legitimacy__whole_language_meaning_primacy, 'none', 1).
narrative_ontology:epsilon_provenance(reading_acquisition_legitimacy__whole_language_meaning_primacy, 0.62, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness (0.62 at interval end) reflects the asymmetric cost: phonologically prepared children benefit from meaning-focused immersion and authentic texts (real coordination gain); phonologically vulnerable children bear the cost of delayed explicit instruction and failed inference attempts, which accumulates as reading failure and special education referral. The trajectory shows extractiveness rising from 0.45 to 0.65 (T0–T24) as the constraint's enforcement intensifies and the gap between promised emergence and actual outcomes for vulnerable learners widens, then plateaus at 0.62-0.63 as the constraint reaches an equilibrium where suppression of structured-literacy alternatives stabilizes. Theater_ratio rises from 0.28 to 0.42, reflecting increasing performative aspects: running records and guided reading become theatrical validation of a framework that is failing its stated purpose (emergence) for significant populations, while the rhetoric of 'respecting the child as reader' masks the actual deficit in phonetic instruction. Suppression_requirement rises from 0.42 to 0.58 because the constraint must actively suppress structured-literacy research findings, exclude researchers from policy forums, and maintain teacher belief in emergence despite mounting counterevidence. The shared time grid (every metric at every point) ensures temporal analysis is sound.
 *
 * PERSPECTIVAL GAP:
 *   The early_literacy_instructors (agenda-setter seat) experience the constraint as genuine coordination: it honors student autonomy, aligns with constructivist theory, and produces engaged readers in their perception (validated by anecdotal observation and running records). From their standpoint, the constraint is a rope or light tangled_rope. The struggling_decoder and dyslexic-learner seats experience it as a snare: they are trapped in a system that withholds explicit instruction they need, blamed for slow progress ('they're not ready yet'), and offered no exit because the school-wide policy forecloses alternatives. From their standpoint, the constraint is pure extraction. School administrators split: they adopt the constraint in good faith (rope), but face evidence (parent complaints, low achievement growth, special education overidentification) that contradicts the coordination story. The structured_literacy_researchers experience exclusion: their voice is structurally absent from curriculum decisions, making them targets of suppression even though they are not direct victims. The engine will compute per-seat types from power, exit, and directionality; this perspectival gap is where those types diverge most sharply.
 *
 * DIRECTIONALITY LOGIC:
 *   early_literacy_instructors: d ≈ 0.1–0.2 (beneficiaries with mobile exit, institutional power, identity investment). Their directionality is low because they set the agenda, face no suppression, and derive professional validation. constructivist_educational_theorists: d ≈ 0.05 (pure beneficiaries, institutional power, not on the receiving end of suppression). struggling_decoders: d ≈ 0.95 (full targets—powerless, trapped, bearing the cost of delayed instruction, suppressed from demanding alternatives). students_with_dyslexia: d ≈ 0.98 (full targets—additionally excluded from diagnosis and intervention until failure is severe). phonologically_vulnerable_learners: d ≈ 0.88 (high targets—constrained exit, moderate power via private tutoring but structurally disadvantaged in school). structured_literacy_researchers: d ≈ 0.75 (targets of suppression despite institutional power, trapped from changing practice by gatekeeper control). school_administrators: d ≈ 0.4–0.5 (symmetric to slightly-extractive: they benefit from adopting a progressive-sounding framework but pay a cost in achievement pressure and parent complaints; identity-lock raises their cost of reversal). parents_of_struggling_readers: d ≈ 0.85 (high targets—excluded from curriculum decisions, forced to seek private alternatives or accept reading failure). The directionality derivation is from beneficiary/victim declarations + exit_options + power; no overrides are necessary because the structural relationships are unambiguous.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (mechanistic, unmotivating phonics instruction) was real in the 1970s-80s. However, by the 1990s-2000s, the problem was largely solved by the emergence of balanced and structured approaches that combined explicit phonics with authentic literature and meaning-making. Whole-language reading persists not because the founding problem remains live but because (1) the agenda-setters (instructors and administrators) benefit from the constraint (it aligns with their professional identity and epistemology), and (2) suppression of alternative frameworks is effective enough to prevent policy change despite contradicting evidence. The constraint is a mandatrophy candidate: the founding problem is dead (phonics instruction is now understood to include meaning-making; balanced and structured approaches solve the original problem better), but the arrangement persists through institutional inertia, epistemic capture, and gatekeeper control. A piton diagnosis would require theater_ratio > 0.5 (mostly performative), which the data does not show (plateau at 0.41); instead, the constraint operates as a tangled_rope in steady state—real coordination (meaningful reading, student engagement) with substantial asymmetric extraction (denying explicit instruction to those who need it). The mandatrophy lies not in the current operatedness but in the founding-problem-status mismatch: if structured literacy and balanced approaches now solve the founding problem better, and if whole-language is maintained for non-functional reasons (belief, identity-lock, institutional inertia), then the constraint's persistence is not justified by its original purpose.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    emergence_hypothesis_falsifiability,
    'Can phonetic principles truly emerge from immersion in authentic texts for all learners, or do phonologically vulnerable children require explicit instruction to extract them?',
    'Longitudinal studies comparing reading trajectories of phonologically vulnerable children under whole-language vs. explicit phonics instruction, controlling for text exposure and teacher quality. Neuroimaging studies comparing phonological pathway activation under the two conditions.',
    'If phonologically vulnerable learners cannot extract phonetic principles reliably from immersion alone, the ''emergence'' axiom is false for a significant population, and the constraint becomes extractive toward that population by denying them the instruction they need. The type would shift from tangled_rope (coordination + asymmetric extraction) toward snare (the coordination story covers pure extraction for the vulnerable subset).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(emergence_hypothesis_falsifiability, empirical, 'Whether phonetic principles emerge from immersion for all learners or only for phonologically prepared ones.').

omega_variable(
    suppression_mechanism_internalized_vs_structural,
    'Is the suppression of explicit phonics instruction structural (institutional gatekeeping by whole-language advocates, control of curriculum committees) or internalized (teachers and parents believe whole-language is pedagogically correct and are reluctant to advocate for alternatives)?',
    'Post-policy-shift observation: if structural suppression lifts and teachers still choose whole-language, suppression is internalized; if alternative frameworks are rapidly adopted when permitted, suppression was structural. Also: surveys of teachers under whole-language policies asking about barriers to explicit phonics adoption.',
    'If suppression is internalized, the constraint persists through belief rather than enforcement, suggesting deeper epistemic capture. If structural, policy change can lift it more readily. The mechanism affects what would be necessary to change practice.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalized_vs_structural, empirical, 'Whether suppression of alternatives is institutional gatekeeping or internalized pedagogical belief.').

omega_variable(
    reading_kernel_bifurcation,
    'Is this constraint one reading of a single kernel (reading_acquisition_legitimacy), or does it encode disagreement about TWO distinct kernels: what reading IS (meaning-making vs. decoding) and what LEGITIMATE INSTRUCTION entails (student-centered vs. explicit)?',
    'Analyze whether sibling readings differ on both dimensions or only one. If whole-language and phonics readings agree on what reading IS but disagree on instruction, they are readings of ONE kernel (the instruction kernel). If they disagree on the definition of reading itself, they are readings of TWO kernels and should be decomposed.',
    'If bifurcated, two separate constraint stories should be authored: one on the definition of reading (cognitive/neuroscientific), one on legitimate instruction (pedagogical). This would clarify which disputes are empirical (resolvable by neuroscience) and which are normative (resolvable by values and policy).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_kernel_bifurcation, conceptual, 'Whether the kernel is one (instruction) or two (definition + instruction method).').

omega_variable(
    axiom_emergence_status_divergence,
    'Is the ''literacy emergence hypothesis'' (phonetic principles emerge from immersion) still held as HOLDABLE by the whole-language community, or has it been quietly superseded by a weaker claim (emergence works for most, plus extra support for struggling readers)?',
    'Textual analysis of contemporary whole-language curriculum guides, teacher-training materials, and academic publications by the reading''s authors. Compare claims from the 1980s-90s (strong emergence axiom) to 2010s-2020s (modified emergence axiom acknowledging exceptions).',
    'If the axiom has been modified, status should be `overridden` (within the reading''s own tradition), not `holdable`. This would indicate the reading has lost its distinctive claim and is converging toward balanced literacy or structured literacy under a different name, making the constraint a Piton rather than a Tangled Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(axiom_emergence_status_divergence, empirical, 'Whether the emergence axiom is still endorsed or has been superseded in recent whole-language practice.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(reading_acquisition_legitimacy__whole_language_meaning_primacy, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(read_tr_t0, reading_acquisition_legitimacy__whole_language_meaning_primacy, theater_ratio, 0, 0.28).
narrative_ontology:measurement(read_tr_t8, reading_acquisition_legitimacy__whole_language_meaning_primacy, theater_ratio, 8, 0.34).
narrative_ontology:measurement(read_tr_t16, reading_acquisition_legitimacy__whole_language_meaning_primacy, theater_ratio, 16, 0.39).
narrative_ontology:measurement(read_tr_t24, reading_acquisition_legitimacy__whole_language_meaning_primacy, theater_ratio, 24, 0.42).
narrative_ontology:measurement(read_tr_t32, reading_acquisition_legitimacy__whole_language_meaning_primacy, theater_ratio, 32, 0.41).
narrative_ontology:measurement(read_tr_t40, reading_acquisition_legitimacy__whole_language_meaning_primacy, theater_ratio, 40, 0.41).

% Extraction over time
narrative_ontology:measurement(read_be_t0, reading_acquisition_legitimacy__whole_language_meaning_primacy, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(read_be_t8, reading_acquisition_legitimacy__whole_language_meaning_primacy, base_extractiveness, 8, 0.54).
narrative_ontology:measurement(read_be_t16, reading_acquisition_legitimacy__whole_language_meaning_primacy, base_extractiveness, 16, 0.61).
narrative_ontology:measurement(read_be_t24, reading_acquisition_legitimacy__whole_language_meaning_primacy, base_extractiveness, 24, 0.65).
narrative_ontology:measurement(read_be_t32, reading_acquisition_legitimacy__whole_language_meaning_primacy, base_extractiveness, 32, 0.63).
narrative_ontology:measurement(read_be_t40, reading_acquisition_legitimacy__whole_language_meaning_primacy, base_extractiveness, 40, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(read_su_t0, reading_acquisition_legitimacy__whole_language_meaning_primacy, suppression_requirement, 0, 0.42).
narrative_ontology:measurement(read_su_t8, reading_acquisition_legitimacy__whole_language_meaning_primacy, suppression_requirement, 8, 0.48).
narrative_ontology:measurement(read_su_t16, reading_acquisition_legitimacy__whole_language_meaning_primacy, suppression_requirement, 16, 0.54).
narrative_ontology:measurement(read_su_t24, reading_acquisition_legitimacy__whole_language_meaning_primacy, suppression_requirement, 24, 0.59).
narrative_ontology:measurement(read_su_t32, reading_acquisition_legitimacy__whole_language_meaning_primacy, suppression_requirement, 32, 0.58).
narrative_ontology:measurement(read_su_t40, reading_acquisition_legitimacy__whole_language_meaning_primacy, suppression_requirement, 40, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(reading_acquisition_legitimacy__whole_language_meaning_primacy, attachment_coordination).
narrative_ontology:boltzmann_floor_override(reading_acquisition_legitimacy__whole_language_meaning_primacy, 0.11).
narrative_ontology:affects_constraint(reading_acquisition_legitimacy__whole_language_meaning_primacy, reading_acquisition_legitimacy__phonics_decoding_primacy).
narrative_ontology:affects_constraint(reading_acquisition_legitimacy__whole_language_meaning_primacy, reading_acquisition_legitimacy__balanced_literacy_integration).
narrative_ontology:affects_constraint(reading_acquisition_legitimacy__whole_language_meaning_primacy, reading_acquisition_legitimacy__structured_literacy_remediation).

% DUAL FORMULATION NOTE:
% This constraint is part of the reading_acquisition_legitimacy kernel family. All four readings (whole_language_meaning_primacy, phonics_decoding_primacy, balanced_literacy_integration, structured_literacy_remediation) share the same referent—the arrangement of early literacy instruction—but each reading instantiates different ε values, different beneficiary/victim structures, and different enforcement requirements. Each sibling constraint is a separate story with its own claim, metrics, and stakeholder configuration. The ε-invariance principle requires decomposition: these are not one constraint viewed from four angles, but four structurally distinct constraints, each grounded in a reading's core epistemic claims. Network edges link them as a family; per-story ε values remain independent.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(reading_acquisition_legitimacy__whole_language_meaning_primacy, institutional, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
