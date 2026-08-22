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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   human_readable: Balanced Literacy Integration in Reading Instruction
 *   domain: education/cognitive_science/literacy_pedagogy
 *
 * SUMMARY:
 *   Reading instruction in elementary schools operates under a legitimacy
 *   constraint that declares both decoding and meaning-making essential,
 *   requiring teachers to balance explicit phonics instruction with authentic
 *   literature exposure. This constraint is ONE READING of a contested kernel
 *   about what reading IS and how it should be taught. The balanced-literacy
 *   reading asserts that reading requires both decoding skill (phonological
 *   processing, alphabetic principle) and meaning-making (engagement with
 *   narrative, comprehension, motivation). Instruction should therefore
 *   include both explicit phonics (systematic letter-sound instruction,
 *   decodable texts, skill-focused lessons) and authentic literature exposure
 *   (trade books, guided reading, discussion, motivated engagement). The
 *   constraint operates through curriculum standards, textbook adoption,
 *   teacher training, and classroom resource allocation. For typical learners
 *   with no decoding difficulty, the balance provides security in
 *   foundational skills while maintaining engagement. For struggling readers
 *   and learners with dyslexia, the constraint may constitute a false summit:
 *   justified by the apparent balance but operating as a systematic
 *   under-allocation of intensity to phonics instruction and delay in
 *   identification of students who need intensive, specialized intervention.
 *   The claim/metric gap is deliberate: the constraint is CLAIMED as a rope
 *   (genuine coordination of two necessary functions) while authored metrics
 *   describe moderate extractiveness and rising theater ratio, suggesting the
 *   'balance' performs coordination while increasingly operating as
 *   compromise that under-serves vulnerable learners.
 *
 * KEY AGENTS:
 *   - classroom_teachers: The primary implementers; they must toggle between direct instruction (phonics) and facilitation (meaning-making) and bear the coordination cost.
 *   - mainstream_elementary_students: Benefit from explicit phonics scaffolding and authentic literature engagement; they are the 'typical' learner the balance is optimized for.
 *   - struggling_readers and dyslexic_learners: Bear the extraction cost; they depend on intensive phonics but receive it embedded and non-intensive under balanced literacy; they face identification delays and under-service.
 *   - resource_limited_schools: Cannot afford both full sets of materials (decodable + trade books) and thus bear the cost of the supposed 'balance'.
 *   - education_policymakers: Benefit from the appearance of evidence-based compromise without having to choose between politically contested pedagogical camps.
 *   - curriculum_publishers: Benefit from dual demand (both decodable texts and trade books expand their addressable market).
 *   - phonics_primacy and whole_language advocates: Excluded from classroom-level decision authority; their evidence is cited in policy but contested in practice.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(reading_acquisition_legitimacy__balanced_literacy_integration, 0.48).
domain_priors:suppression_score(reading_acquisition_legitimacy__balanced_literacy_integration, 0.52).
domain_priors:theater_ratio(reading_acquisition_legitimacy__balanced_literacy_integration, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(reading_acquisition_legitimacy__balanced_literacy_integration, extractiveness, 0.48).
narrative_ontology:constraint_metric(reading_acquisition_legitimacy__balanced_literacy_integration, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(reading_acquisition_legitimacy__balanced_literacy_integration, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(reading_acquisition_legitimacy__balanced_literacy_integration, accessibility_collapse, 0.61).
narrative_ontology:constraint_metric(reading_acquisition_legitimacy__balanced_literacy_integration, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(reading_acquisition_legitimacy__balanced_literacy_integration, rope).
narrative_ontology:human_readable(reading_acquisition_legitimacy__balanced_literacy_integration, "Balanced Literacy Integration in Reading Instruction").
narrative_ontology:topic_domain(reading_acquisition_legitimacy__balanced_literacy_integration, "education/cognitive_science/literacy_pedagogy").

domain_priors:requires_active_enforcement(reading_acquisition_legitimacy__balanced_literacy_integration).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(reading_acquisition_legitimacy__balanced_literacy_integration, '2f47b16a-f5f3-4c31-83c2-d1dcc2b7299c').
narrative_ontology:cs_kernel_codification('2f47b16a-f5f3-4c31-83c2-d1dcc2b7299c', distributed).
narrative_ontology:cs_authority_grounding('2f47b16a-f5f3-4c31-83c2-d1dcc2b7299c', expertise).
narrative_ontology:cs_interpretation_layer_present('2f47b16a-f5f3-4c31-83c2-d1dcc2b7299c').
narrative_ontology:cs_reading_relation('2f47b16a-f5f3-4c31-83c2-d1dcc2b7299c', reading_acquisition_legitimacy__phonics_decoding_primacy, coexists_with).
narrative_ontology:cs_reading_relation('2f47b16a-f5f3-4c31-83c2-d1dcc2b7299c', reading_acquisition_legitimacy__whole_language_meaning_primacy, coexists_with).
narrative_ontology:cs_reading_relation('2f47b16a-f5f3-4c31-83c2-d1dcc2b7299c', reading_acquisition_legitimacy__structured_literacy_remediation, influences).
narrative_ontology:cs_axiom('2f47b16a-f5f3-4c31-83c2-d1dcc2b7299c', foundational, reading_requires_both_decoding_and_meaning_making).
narrative_ontology:cs_axiom_status(reading_requires_both_decoding_and_meaning_making, holdable).
narrative_ontology:cs_axiom_grounding('2f47b16a-f5f3-4c31-83c2-d1dcc2b7299c', reading_requires_both_decoding_and_meaning_making, empirically_contingent).
narrative_ontology:cs_axiom('2f47b16a-f5f3-4c31-83c2-d1dcc2b7299c', secondary, integration_is_pedagogically_superior_to_specialization).
narrative_ontology:cs_axiom_status(integration_is_pedagogically_superior_to_specialization, holdable).
narrative_ontology:cs_axiom_grounding('2f47b16a-f5f3-4c31-83c2-d1dcc2b7299c', integration_is_pedagogically_superior_to_specialization, empirically_contingent).
narrative_ontology:cs_reference_frame('2f47b16a-f5f3-4c31-83c2-d1dcc2b7299c', integrated_dual_process_literacy_model).
narrative_ontology:cs_drift_state('2f47b16a-f5f3-4c31-83c2-d1dcc2b7299c', contemporary_neuroscience_and_dyslexia_awareness, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('2f47b16a-f5f3-4c31-83c2-d1dcc2b7299c', '2026-06-12T00:00:00Z').
narrative_ontology:cs_kernel_id(reading_acquisition_legitimacy__balanced_literacy_integration, reading_acquisition_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(reading_acquisition_legitimacy__balanced_literacy_integration, mainstream_elementary_students).
narrative_ontology:constraint_beneficiary(reading_acquisition_legitimacy__balanced_literacy_integration, teaching_profession).
narrative_ontology:constraint_beneficiary(reading_acquisition_legitimacy__balanced_literacy_integration, integrated_curriculum_practitioners).
narrative_ontology:constraint_victim(reading_acquisition_legitimacy__balanced_literacy_integration, struggling_readers).
narrative_ontology:constraint_victim(reading_acquisition_legitimacy__balanced_literacy_integration, dyslexic_learners).
narrative_ontology:constraint_victim(reading_acquisition_legitimacy__balanced_literacy_integration, resource_limited_schools).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(reading_acquisition_legitimacy__balanced_literacy_integration, struggling_readers).
narrative_ontology:constraint_beneficiary(reading_acquisition_legitimacy__balanced_literacy_integration, education_policymakers).
narrative_ontology:constraint_beneficiary(reading_acquisition_legitimacy__balanced_literacy_integration, curriculum_publishers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Responsible for implementing balanced literacy in the classroom. They must juggle explicit phonics instruction, guided reading groups, independent reading of authentic texts, and running reading centers simultaneously. The constraint requires them to toggle between direct instruction (phonics, decoding) and facilitation (meaning-making, discussion). They are the primary site where the balance is (or is not) enacted.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__balanced_literacy_integration, classroom_teachers, agenda_setter,
    moderate, biographical, constrained, local).

% Receive both explicit phonics (letters-to-sounds systematically taught) and exposure to authentic, engaging literature (trade books, narrative texts). For students without decoding difficulty, this mixed approach provides security in foundational skills while maintaining motivation through meaningful texts. Their primary benefit is entering the literacy pipeline with both decodable-text scaffolding and authentic-text engagement.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__balanced_literacy_integration, mainstream_elementary_students, beneficiary,
    powerless, immediate, trapped, local).

% Depend on explicit, cumulative phonics instruction to develop decoding. However, under balanced literacy, they often receive phonics embedded in general classroom instruction rather than intensive, specialized intervention. They bear the cost of delayed recognition that they need more explicit, longer-duration phonics than the 'balance' allocates to them. They also benefit from some phonics exposure, but it may arrive too late or not intensively enough.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__balanced_literacy_integration, struggling_readers, payer,
    powerless, immediate, trapped, local).
narrative_ontology:stakeholder_secondary_role(reading_acquisition_legitimacy__balanced_literacy_integration, struggling_readers, beneficiary).

% Have a neurobiological decoding deficit that requires systematic, diagnostic, multi-sensory phonics instruction (structured literacy). Balanced literacy's assumption that phonics can be embedded and flexible conflicts with their need for explicit, intensive, cumulative, and orton-gillingham-based instruction. They are identity-locked into the school system; they cannot exit to find specialized instruction unless parents can pay for outside tutoring. The constraint treats their need as one point on a spectrum rather than a distinct category.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__balanced_literacy_integration, dyslexic_learners, payer,
    powerless, biographical, identity_locked, local).

% Cannot afford both sets of materials: decodable texts for explicit phonics instruction AND a comprehensive trade-book library for authentic literature exposure. They cannot fund two sets of resources, trained specialists for intervention, or extended instructional blocks. The 'balance' requires material and time investments that stretched budgets cannot support. Schools in low-income districts bear disproportionate cost.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__balanced_literacy_integration, resource_limited_schools, payer,
    powerless, generational, trapped, regional).

% Would argue that reading IS decoding, that phonics must be systematic and primary, and that authentic literature should come only after sufficient decoding skill is established. They are excluded from the classroom-level decision-making about implementation; their evidence (e.g., simple view of reading, science of reading critiques) is present in policy but contested in practice.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__balanced_literacy_integration, phonics_primacy_advocates, excluded,
    organized, generational, constrained, national).

% Would argue that meaning-making is primary, that authentic literature exposure is the legitimate pedagogical choice, and that decoding skills emerge naturally from engagement with meaningful texts. They are also excluded from classroom implementation choices; their pedagogical tradition is present in some practice (guided reading, literature circles) but contested by phonics-first policy pressure.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__balanced_literacy_integration, whole_language_advocates, excluded,
    organized, generational, constrained, national).

% Balanced literacy is an appealing middle ground: it avoids political commitment to either phonics-only or whole-language-only, it sounds scientifically informed ('both are important'), and it permits school autonomy without mandating explicit intervention protocols. Policymakers benefit from avoiding the hard political choice and from the appearance of evidence-based compromise.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__balanced_literacy_integration, education_policymakers, beneficiary,
    institutional, generational, analytical, national).

% Benefit from the 'balance' framework because it creates demand for BOTH decodable-text series AND trade-book collections, BOTH phonics-focused materials AND guided-reading packages. A phonics-only mandate would reduce demand for one half; a whole-language mandate would reduce demand for the other. Balanced literacy expands the total addressable market.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__balanced_literacy_integration, curriculum_publishers, beneficiary,
    organized, biographical, mobile, national).

% Conduct studies on reading acquisition, phonological awareness, decoding, comprehension, and intervention efficacy. They hold no power over classroom implementation but their evidence is cited by advocates of each reading. The constraint's persistence depends on disputed interpretation of this evidence.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__balanced_literacy_integration, literacy_researchers, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(reading_acquisition_legitimacy__balanced_literacy_integration, education_policymakers).
narrative_ontology:fixing_cost_class(reading_acquisition_legitimacy__balanced_literacy_integration, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Balances the cognitive demand for explicit decoding instruction with the motivational/engagement demand for meaningful, authentic reading experiences. Solves the integration problem: students need both the technical skill (alphabetic principle, phonological processing) and the will-to-read (narrative engagement, perceived relevance). Without the balance, classrooms would specialize into phonics-only (security, boring) or whole-language-only (engagement, unsupported decoders).
% TRANSFER_FUNCTION: Redistributes instructional time, teacher expertise, and material resources across two pedagogical modalities: phonics/decoding instruction (explicit, scaffolded, skill-focused) and authentic literature engagement (choice, discussion, meaning-making). For typical learners, the transfer is equitable and mutually supportive. For struggling readers, the transfer diverts intensity from phonics to balance, delaying identification and reducing intervention depth.
% ABSENT_VOICES: Dyslexia specialists and structured-literacy practitioners would argue the balance under-allocates to intensive, explicit, cumulative, multi-sensory phonics instruction and that struggling readers need supplementary structures (small-group intervention, diagnostic assessment, longer intervention duration). Phonics-only advocates would argue authentic literature is premature before decoding automaticity is established. Both groups are excluded from classroom-level curriculum decisions; their objections are present in policy debate but not in classroom implementation authority.
% DISAPPEARANCE_RATIONALE: If balanced literacy disappeared, elementary reading instruction would bifurcate: phonics-intensive programs (explicit, cumulative, with decodable-text progression and later authentic literature) would serve learners identified with decoding difficulty; whole-language or literature-based programs would serve schools committed to meaning-first approaches. The integration itself would vanish; teachers would no longer toggle between modes. Schools would either invest in intensive intervention (pulling struggling readers into separate phonics instruction) or stick to one pedagogical camp. The coordinated, flexible balance would dissolve.
% FOUNDING_PROBLEM: Early reading instruction faced a choice between security (explicit phonics, decodable texts) and motivation (authentic literature, narrative engagement). The founding problem was how to serve both: students need decoding skill AND they need to see reading as meaningful and pleasurable. A purely phonics-first approach risks losing struggling readers to boredom and demotivation; a purely whole-language approach risks leaving struggling decoders without necessary scaffolding.
% FOUNDING_PROBLEM_CORROBORATION: Balanced-literacy advocates and some teacher-training institutions attest the problem is live: both phonics and authentic literature are necessary, and the balance serves both populations. Phonics-primacy advocates (citing neuroscience research, intervention efficacy trials, simple view of reading) attest the problem was historically real but neuroscience has resolved it: structured, intensive phonics MUST come first, and authentic literature should follow only after decoding automaticity. Dyslexia researchers and structured-literacy trainers attest the problem was wrongly framed: the real problem is that vulnerable learners need a different (more intensive, diagnostic) approach, not a balance that serves typical learners. Independent evidence (National Reading Panel meta-analysis, simple view of reading, cognitive neuroscience of dyslexia) corroborates that both decoding and language comprehension are necessary—but does NOT resolve the instructional sequencing and intensity question.
narrative_ontology:disappearance_verdict(reading_acquisition_legitimacy__balanced_literacy_integration, world_rearranges).
narrative_ontology:founding_problem_status(reading_acquisition_legitimacy__balanced_literacy_integration, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(reading_acquisition_legitimacy__balanced_literacy_integration, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(reading_acquisition_legitimacy__balanced_literacy_integration, 'none', 1).
narrative_ontology:epsilon_provenance(reading_acquisition_legitimacy__balanced_literacy_integration, 0.48, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness is moderate (0.48) because the constraint does solve a real coordination problem for typical learners: how to provide both decoding scaffolding and meaningful reading experiences. The extractiveness comes from the cost imposed on struggling readers (delayed identification, insufficient intensity, assumption they will naturalize decoding if immersed in authentic texts) and resource-limited schools (which cannot afford both full material sets). Suppression is moderate-high (0.52) because the constraint maintains itself through policy language ('balanced literacy'), textbook adoption processes, and teacher training that defaults to balance as the legitimate middle ground, while systematically excluding or marginalizing phonics-intensive and whole-language-intensive practitioners. Theater ratio is moderate and rising (0.25 at t=0, peaking at 0.39 at t=15, settling at 0.38 at t=25) because classroom practice increasingly performs the 'balance' (visible mix of both activities) while the underlying coordination function (serving all learners well) faces growing evidence that the balance under-serves struggling readers. The rising theater suggests the constraint is drifting toward piton status: maintained for appearance (we have phonics AND authentic literature) while the real coordination function (all learners developing literacy) is not being met for vulnerable learners. The measurement series shows extractiveness rising to t=15 then plateauing, theater ratio rising through t=15 then stabilizing, and suppression rising through t=15 then stabilizing — suggesting a phase transition around t=15 where the constraint's legitimacy stabilizes at a higher performative cost and extraction begins to show resistance (struggling readers' underachievement becomes visible, prompting intervention mandates that compete with the balance). Accessibility collapse is moderate (0.61): alternatives (phonics-only, whole-language-only, structured literacy) are intellectually available and advocated, but institutional barriers (curriculum adoption, standards language, teacher training aligned to balance) make them costly to access. Resistance is elevated (0.72): phonics-primacy advocates and dyslexia specialists actively resist the balance, and growing evidence that struggling readers underachieve under balanced literacy creates pressure.
 *
 * PERSPECTIVAL GAP:
 *   From the classroom teacher's perspective, the balance is a real coordination problem: they genuinely need both phonics and authentic literature to serve diverse learners. From the policy level, the balance is a legitimate compromise that avoids political commitment to either extreme. From the struggling reader's or dyslexic learner's perspective, the balance is a false summit: justified by the appearance of both phonics and literature but operating as under-service of their specific needs (insufficient phonics intensity, delayed identification, assumption of naturalization that neurobiologically does not occur). From the perspective of phonics-primacy and structured-literacy advocates, the balance is an institutional compromise that suppresses evidence-based intensive phonics and leaves vulnerable learners behind. The engine should compute this divergence: agents in high d positions (struggling readers, resource-limited schools) should show higher effective extraction and lower perceptions of coordination; agents in low d positions (typical learners, policymakers) should show lower extraction and higher perceptions of legitimate coordination.
 *
 * DIRECTIONALITY LOGIC:
 *   Mainstream elementary students experience d near 0.3 (low directionality: they benefit from the balance without bearing costs; the constraint subsidizes them with both phonics scaffolding and engaging texts). Struggling readers and dyslexic learners experience d near 0.75 (high directionality: they depend on intensive phonics, which is under-allocated; they are targets of the constraint, bearing extraction in the form of delayed identification, lower instructional intensity, and unmet needs). Resource-limited schools experience d near 0.8 (very high directionality: they cannot afford both material sets and thus absorb the full cost; they are structurally trapped as targets). Teachers experience d near 0.6 (moderate-high: they bear the coordination cost, toggling between two instructional modes, managing reading centers, identifying struggling readers). Policymakers and curriculum publishers experience d near 0.2 (low directionality: they benefit from the middle-ground legitimacy and market expansion without direct costs). The directionality overrides reflect the power atom distinctions: powerless agents (students, teachers in low-resource schools) bear disproportionate d (high extraction); organized and institutional agents (policymakers, publishers) experience low d (benefits without direct cost). No formal override needed; the structural derivation from beneficiary/victim + exit + power captures the gap.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (how to have both decoding security and reading engagement/motivation) remains contested in status. For mainstream learners, the problem is live and the balance solves it. For struggling readers, the problem is different: they need intensive decoding instruction, and the balance's assumption that they will naturalize decoding through immersion in authentic texts is empirically undermined by neuroscience and intervention research. Dyslexia researchers and structured-literacy advocates would attest the founding problem was historically real but has been solved by advances in understanding dyslexia and phonological deficit; the trade-off is false—intensive phonics can coexist with later authentic literature if implementation prioritizes struggling learners first. The constraint avoids mandatrophy (the founding problem is still cited as the legitimacy basis) but faces a latent version: the problem it was built to solve (coordination of both functions) may be partially obsolete for vulnerable learners, replaced by a different need (intensive, diagnostic, multi-sensory phonics first, then authentic literature). This latent mandatrophy is not yet declared in policy but is emerging in evidence and in parent/advocate pressure for structured-literacy alternatives.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    phonics_literacy_reading_committer_dispute,
    'Is reading fundamentally a decoding process (phonological processing, cracking the alphabetic code) or a meaning-making process (comprehension, engagement with semantic and narrative content), or an integrated dual process requiring both?',
    'Cognitive science consensus on reading models (simple view of reading vs. contemporary dual-process models), neuroimaging studies on dyslexia and reading skill development, intervention efficacy trials comparing phonics-intensive vs. whole-language vs. balanced approaches on subgroups (typical, dyslexic, language-disordered).',
    'This is the core committer dispute between the readings. If decoding is primary (phonics_primacy is correct), balanced literacy under-serves struggling decoders. If meaning-making is primary (whole_language is correct), balanced literacy over-allocates to artificial phonics instruction. If integrated dual-process is correct (as contemporary reading science suggests), the balance remains valid but requires evidence on implementation adequacy.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(phonics_literacy_reading_committer_dispute, empirical, 'The kernel dispute on what reading IS: decoding, meaning-making, or integrated dual-process.').

omega_variable(
    committer_axiom_reading_relations,
    'What are the structural relationships among the four readings of the reading_acquisition_legitimacy kernel?',
    'Logical analysis of core premises: does phonics_primacy logically foreclose whole_language_primacy (they make contradictory claims about what reading IS, but both could be partially true), or do they coexist as competing frameworks held by different practitioners? Does structured_literacy foreclose balanced_literacy (it prioritizes vulnerability as the design constraint, implying balance is inadequate), or does it influence the balance by demanding supplementary structures? Does the balance coexist with or foreclose the others?',
    'The reading_relations in cs_structure encode these disputes. If strict logical foreclosure applies, one reading will eventually replace the others. If coexistence applies, the kernel remains genuinely contested across live readings. If influence applies, the readings are ordered (upstream vs. downstream) by implementation priority.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(committer_axiom_reading_relations, conceptual, 'The logical and institutional relationships among the four readings of the literacy kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(reading_acquisition_legitimacy__balanced_literacy_integration, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(read_tr_t0, reading_acquisition_legitimacy__balanced_literacy_integration, theater_ratio, 0, 0.25).
narrative_ontology:measurement(read_tr_t5, reading_acquisition_legitimacy__balanced_literacy_integration, theater_ratio, 5, 0.29).
narrative_ontology:measurement(read_tr_t10, reading_acquisition_legitimacy__balanced_literacy_integration, theater_ratio, 10, 0.34).
narrative_ontology:measurement(read_tr_t15, reading_acquisition_legitimacy__balanced_literacy_integration, theater_ratio, 15, 0.38).
narrative_ontology:measurement(read_tr_t20, reading_acquisition_legitimacy__balanced_literacy_integration, theater_ratio, 20, 0.39).
narrative_ontology:measurement(read_tr_t25, reading_acquisition_legitimacy__balanced_literacy_integration, theater_ratio, 25, 0.38).

% Extraction over time
narrative_ontology:measurement(read_be_t0, reading_acquisition_legitimacy__balanced_literacy_integration, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(read_be_t5, reading_acquisition_legitimacy__balanced_literacy_integration, base_extractiveness, 5, 0.42).
narrative_ontology:measurement(read_be_t10, reading_acquisition_legitimacy__balanced_literacy_integration, base_extractiveness, 10, 0.46).
narrative_ontology:measurement(read_be_t15, reading_acquisition_legitimacy__balanced_literacy_integration, base_extractiveness, 15, 0.49).
narrative_ontology:measurement(read_be_t20, reading_acquisition_legitimacy__balanced_literacy_integration, base_extractiveness, 20, 0.48).
narrative_ontology:measurement(read_be_t25, reading_acquisition_legitimacy__balanced_literacy_integration, base_extractiveness, 25, 0.48).

% Suppression requirement over time
narrative_ontology:measurement(read_su_t0, reading_acquisition_legitimacy__balanced_literacy_integration, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(read_su_t5, reading_acquisition_legitimacy__balanced_literacy_integration, suppression_requirement, 5, 0.45).
narrative_ontology:measurement(read_su_t10, reading_acquisition_legitimacy__balanced_literacy_integration, suppression_requirement, 10, 0.5).
narrative_ontology:measurement(read_su_t15, reading_acquisition_legitimacy__balanced_literacy_integration, suppression_requirement, 15, 0.53).
narrative_ontology:measurement(read_su_t20, reading_acquisition_legitimacy__balanced_literacy_integration, suppression_requirement, 20, 0.52).
narrative_ontology:measurement(read_su_t25, reading_acquisition_legitimacy__balanced_literacy_integration, suppression_requirement, 25, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(reading_acquisition_legitimacy__balanced_literacy_integration, resource_allocation).
narrative_ontology:boltzmann_floor_override(reading_acquisition_legitimacy__balanced_literacy_integration, 0.18).
narrative_ontology:affects_constraint(reading_acquisition_legitimacy__balanced_literacy_integration, reading_acquisition_legitimacy__phonics_decoding_primacy).
narrative_ontology:affects_constraint(reading_acquisition_legitimacy__balanced_literacy_integration, reading_acquisition_legitimacy__whole_language_meaning_primacy).
narrative_ontology:affects_constraint(reading_acquisition_legitimacy__balanced_literacy_integration, reading_acquisition_legitimacy__structured_literacy_remediation).

% DUAL FORMULATION NOTE:
% This story is one reading (balanced_literacy_integration) of the contested kernel reading_acquisition_legitimacy. The sibling readings (phonics_decoding_primacy, whole_language_meaning_primacy, structured_literacy_remediation) are separate constraint stories with different ε values, different beneficiary/victim structures, and different stakeholder configurations. The balanced reading coexists with and influences the sibling readings; each describes a distinct pedagogical implementation framework and a different assessment of whether the reading_acquisition_legitimacy kernel is properly instantiated. The balanced reading claims to synthesize both phonics and authentic literature; the phonics reading claims phonics must be primary; the whole-language reading claims authentic literature and natural emergence is sufficient; the structured-literacy reading claims vulnerable learners must be designed-from-first, implying supplementary intensity beyond balance. All four are live positions in contemporary educational discourse.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(reading_acquisition_legitimacy__balanced_literacy_integration, powerless, 0.78).
constraint_indexing:directionality_override(reading_acquisition_legitimacy__balanced_literacy_integration, moderate, 0.58).
constraint_indexing:directionality_override(reading_acquisition_legitimacy__balanced_literacy_integration, organized, 0.22).
constraint_indexing:directionality_override(reading_acquisition_legitimacy__balanced_literacy_integration, institutional, 0.18).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
