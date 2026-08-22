% ============================================================================
% CONSTRAINT STORY: anthropological_record__creationist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_anthropological_record__creationist_reading, []).

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
 *   constraint_id: anthropological_record__creationist_reading
 *   human_readable: Anthropological Record Interpreted Through Creationist/Design Framework
 *   domain: epistemology/philosophy_of_science/anthropology
 *
 * SUMMARY:
 *   This constraint instantiates ONE reading of the contested kernel
 *   'anthropological_record': the creationist reading treats the body of
 *   evidence (fossil record, genetic data, anatomical homology, migration
 *   patterns) as compatible with divine creation and/or intelligent design,
 *   and with scriptural timelines. The reading asserts that divine causation
 *   is a legitimate interpretive frame for anthropological data and that
 *   credentialed scientific materialism should not monopolize adjudication of
 *   the evidence. The constraint's operation extracts epistemic authority
 *   from scientific institutions and suppresses materialist timelines within
 *   religious communities. Measurement data show rising extractiveness
 *   (0.48→0.68) as enforcement machinery hardens, and rising theater
 *   (0.32→0.58) as the constraint's operation becomes increasingly about
 *   defending interpretive authority rather than addressing evidence. The
 *   twin operability thresholds (theater crossing 0.50, suppression flat at
 *   0.72) suggest the constraint is drifting toward piton-class (performative
 *   maintenance of interpretive exclusivity) while suppression requirement
 *   remains high (enforcing the exclusive reading against well-resourced
 *   scientific opposition).
 *
 * KEY AGENTS:
 *   - Religious institutional authority: sets interpretation standards for believer communities; controls theological credentialing; identity constituted through maintenance of this framework.
 *   - Creationist theological tradition: organized intellectual tradition (YEC, OEC, ID) benefiting from the reading; membership depends on accepting premises; identity-locked exit.
 *   - Secular academic scientists: institutional actors (evolutionary biologists, paleoanthropologists) whose interpretations are suppressed; bear cost of active enforcement against their conclusions.
 *   - Materialist epistemologists: defend not only their interpretations but the legitimacy of methodological naturalism itself; higher-cost defense against the constraint.
 *   - Religious lay communities: benefit from identity coherence; bear cost of constrained intellectual autonomy and limited exposure to alternative frameworks.
 *   - State education systems: observe and adjudicate disputes over curriculum; constrained between enforcement pressure and professional standards.
 *   - Alternative epistemologies (excluded): indigenous and non-Western knowledge systems excluded by binary framing; would argue for relational/oral adjudication of origins.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(anthropological_record__creationist_reading, 0.68).
domain_priors:suppression_score(anthropological_record__creationist_reading, 0.72).
domain_priors:theater_ratio(anthropological_record__creationist_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(anthropological_record__creationist_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(anthropological_record__creationist_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(anthropological_record__creationist_reading, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(anthropological_record__creationist_reading, accessibility_collapse, 0.61).
narrative_ontology:constraint_metric(anthropological_record__creationist_reading, resistance, 0.74).

% --- Constraint claim ---
narrative_ontology:constraint_claim(anthropological_record__creationist_reading, tangled_rope).
narrative_ontology:human_readable(anthropological_record__creationist_reading, "Anthropological Record Interpreted Through Creationist/Design Framework").
narrative_ontology:topic_domain(anthropological_record__creationist_reading, "epistemology/philosophy_of_science/anthropology").

domain_priors:requires_active_enforcement(anthropological_record__creationist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(anthropological_record__creationist_reading, 'c0f22ffb-2a13-4ef9-bc2e-45cc11f65642').
narrative_ontology:cs_kernel_codification('c0f22ffb-2a13-4ef9-bc2e-45cc11f65642', fixed_text).
narrative_ontology:cs_authority_grounding('c0f22ffb-2a13-4ef9-bc2e-45cc11f65642', extraction).
narrative_ontology:cs_interpretation_layer_present('c0f22ffb-2a13-4ef9-bc2e-45cc11f65642').
narrative_ontology:cs_reading_relation('c0f22ffb-2a13-4ef9-bc2e-45cc11f65642', anthropological_record__naturalist_reading, forecloses).
narrative_ontology:cs_reading_relation('c0f22ffb-2a13-4ef9-bc2e-45cc11f65642', anthropological_record__indigenous_epistemology_reading, influences).
narrative_ontology:cs_axiom('c0f22ffb-2a13-4ef9-bc2e-45cc11f65642', foundational, scriptural_accounts_historically_accurate).
narrative_ontology:cs_axiom_status(scriptural_accounts_historically_accurate, holdable).
narrative_ontology:cs_axiom_grounding('c0f22ffb-2a13-4ef9-bc2e-45cc11f65642', scriptural_accounts_historically_accurate, deontological).
narrative_ontology:cs_axiom('c0f22ffb-2a13-4ef9-bc2e-45cc11f65642', foundational, divine_causation_adequate_explanation_origins).
narrative_ontology:cs_axiom_status(divine_causation_adequate_explanation_origins, holdable).
narrative_ontology:cs_axiom_grounding('c0f22ffb-2a13-4ef9-bc2e-45cc11f65642', divine_causation_adequate_explanation_origins, theological).
narrative_ontology:cs_axiom('c0f22ffb-2a13-4ef9-bc2e-45cc11f65642', secondary, theological_interpretation_adjudicates_empirical_record).
narrative_ontology:cs_axiom_status(theological_interpretation_adjudicates_empirical_record, overridden).
narrative_ontology:cs_axiom_grounding('c0f22ffb-2a13-4ef9-bc2e-45cc11f65642', theological_interpretation_adjudicates_empirical_record, conventional).
narrative_ontology:cs_reference_frame('c0f22ffb-2a13-4ef9-bc2e-45cc11f65642', scriptural_revealed_authority).
narrative_ontology:cs_drift_state('c0f22ffb-2a13-4ef9-bc2e-45cc11f65642', contemporary_scientific_evidence_accumulation, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('c0f22ffb-2a13-4ef9-bc2e-45cc11f65642', '').
narrative_ontology:cs_kernel_id(anthropological_record__creationist_reading, anthropological_record).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(anthropological_record__creationist_reading, religious_institutional_authority).
narrative_ontology:constraint_beneficiary(anthropological_record__creationist_reading, creationist_theological_tradition).
narrative_ontology:constraint_victim(anthropological_record__creationist_reading, secular_academic_scientists).
narrative_ontology:constraint_victim(anthropological_record__creationist_reading, evolutionary_biologists).
narrative_ontology:constraint_victim(anthropological_record__creationist_reading, materialist_epistemologists).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(anthropological_record__creationist_reading, religious_lay_communities).
narrative_ontology:constraint_victim(anthropological_record__creationist_reading, religious_lay_communities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Religious institutions (churches, denominational academies, faith-based publishing) that maintain and enforce the reading that anthropological evidence is compatible with divine creation and scriptural timeline. They set interpretation standards for their communities, control access to theological credentialing, and determine what counts as legitimate evidence within their epistemic framework. Identity of the institution is constituted through this authority.
narrative_ontology:constraint_stakeholder(anthropological_record__creationist_reading, religious_institutional_authority, agenda_setter,
    institutional, civilizational, identity_locked, global).

% The intellectual tradition (young-earth creationism, old-earth creationism, intelligent design) that benefits from the constraint by maintaining coherence between faith commitments and claimed reading of the physical record. Membership and standing depend on accepting the reading's fundamental premises. Exit means ideological dissolution.
narrative_ontology:constraint_stakeholder(anthropological_record__creationist_reading, creationist_theological_tradition, beneficiary,
    organized, civilizational, identity_locked, global).

% Professional scientists in evolutionary biology, paleoanthropology, and related fields whose epistemic authority is suppressed when the creationist reading operates. Their interpretations of the same evidence are systematically excluded from public education and policy domains where the reading has institutional power. They bear the cost of active enforcement against their conclusions.
narrative_ontology:constraint_stakeholder(anthropological_record__creationist_reading, secular_academic_scientists, payer,
    institutional, generational, mobile, global).

% A scientific discipline whose core explanatory framework is incompatible with the creationist reading. They face institutional pressure (school board challenges, curriculum mandates, funding restrictions in certain jurisdictions) to represent their findings as one 'theory' among competing interpretations rather than as established knowledge. Career advancement in public or religiously-affiliated institutions may require muting or reframing evidence.
narrative_ontology:constraint_stakeholder(anthropological_record__creationist_reading, evolutionary_biologists, payer,
    organized, generational, constrained, global).

% Philosophers and theorists of knowledge who treat material causation and methodological naturalism as the legitimate framework for understanding natural history. The constraint requires them to defend not only their interpretations but the legitimacy of the epistemic method itself—a higher cost than addressing the evidence alone.
narrative_ontology:constraint_stakeholder(anthropological_record__creationist_reading, materialist_epistemologists, payer,
    organized, generational, mobile, global).

% Individuals and families for whom religious faith is central to identity and community membership. They benefit from a reading that reconciles their faith with observable facts (no need to choose between belief and evidence as presented to them); they also bear the cost of limited exposure to scientific frameworks, constrained intellectual autonomy, and potential social isolation if they question the reading.
narrative_ontology:constraint_stakeholder(anthropological_record__creationist_reading, religious_lay_communities, beneficiary,
    moderate, biographical, identity_locked, local).
narrative_ontology:stakeholder_secondary_role(anthropological_record__creationist_reading, religious_lay_communities, payer).

% Public education institutions caught between the constraint's enforcement pressure (parental demands for curriculum inclusion, legislative mandates) and the professional standards of science pedagogy. They observe and adjudicate disputes over what the anthropological record means and how it should be taught.
narrative_ontology:constraint_stakeholder(anthropological_record__creationist_reading, state_education_systems, observer,
    institutional, generational, constrained, national).

% Indigenous knowledge systems and other non-materialist, non-theological readings of human origins that are structurally excluded by the binary framing (scientific materialism vs. creationist theology). Their epistemic claims are not heard in the constraint's operation because the constraint treats the anthropological record as adjudicable only within these two frameworks.
narrative_ontology:constraint_stakeholder(anthropological_record__creationist_reading, alternative_epistemologies, excluded,
    moderate, civilizational, trapped, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(anthropological_record__creationist_reading, religious_institutional_authority).
narrative_ontology:fixing_cost_class(anthropological_record__creationist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a unified interpretive framework for a religious community to hold both faith commitments and observed facts about human origins without perceived contradiction. Coordinates meaning-making across theology, community identity, and claimed empirical observation.
% TRANSFER_FUNCTION: Moves epistemic authority from credentialed scientific institutions to religious institutional interpreters within believer communities. Individuals transfer allegiance to theological frameworks as the primary adjudicator of how to read evidence, in exchange for identity coherence and community belonging.
% ABSENT_VOICES: Indigenous epistemologies and alternative non-Western readings of human origins are structurally excluded; they would argue that the record is knowable through sustained relational and oral traditions, not through either divine-text reading or materialist scientific method. These voices are kept out by the binary framing the constraint enforces.
% DISAPPEARANCE_RATIONALE: Religious institutional authority would argue that if the constraint vanished, faith communities would face identity dissolution and epistemic incoherence. Scientific institutions would argue that without suppression of the constraint, a unified naturalist human-origins narrative would operate uncontested. The two sides dispute whether the world rearranges or stabilizes if the constraint is removed—the dispute is itself the constraint's operation.
% FOUNDING_PROBLEM: How can communities of faith maintain coherence between revealed/scriptural claims about human origins and observable facts about the natural world? The constraint was built to answer: by reading the natural record as compatible with revelation, and treating theological interpretation as a legitimate adjudication of empirical questions.
% FOUNDING_PROBLEM_CORROBORATION: Religious institutional authorities attest the founding problem is live and urgent—the constraint solves a real identity crisis for believers. Secular scientists and education professionals attest the founding problem dissolves if empirical questions are adjudicated by scientific method, not theological framework—they argue the constraint persists not because it solves a genuine problem but because religious institutions benefit from suppressing alternative adjudication. Historians of science outside both camps document that the founding problem was acute during periods of rapid paleontological discovery; contemporary persistence is contested.
narrative_ontology:disappearance_verdict(anthropological_record__creationist_reading, contested).
narrative_ontology:founding_problem_status(anthropological_record__creationist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(anthropological_record__creationist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(anthropological_record__creationist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(anthropological_record__creationist_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(anthropological_record__creationist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(anthropological_record__creationist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(anthropological_record__creationist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   This reading is claimed as Tangled Rope because it exhibits both coordination (provides meaning-coherence for believer communities) and asymmetric extraction (suppresses alternative epistemic authorities). Extractiveness is moderately high (0.68) because the reading requires suppressing or reframing a large body of competing evidence and alternative interpretations—the cost to outside parties (scientists, materialist epistemologists) is substantial. Suppression is slightly higher (0.72) than extractiveness because the constraint's persistence depends on active enforcement: schools must be pressured, scientific consensus must be contested, alternative framings must be excluded. Theater ratio is substantial (0.58) and rising, indicating that an increasing share of the constraint's operation is performative—defending the reading's epistemic status rather than addressing new evidence or resolving genuine ambiguities in the record. The measurement series show early acceleration (extractiveness +0.10 in first 5 time points, theater +0.06) as the constraint encounters organized scientific opposition, then plateau (extractiveness stable from t=25, theater flat from t=30) as enforcement reaches steady-state maintenance. Accessibility collapse is moderate (0.61): once someone accepts the creationist reading as the interpretive frame, alternatives do partially collapse—but the frame itself remains contestable and contested, so collapse is not complete (unlike a genuine mountain where collapse approaches 0.85+). Resistance is high (0.74): the constraint meets substantial organized opposition from scientific institutions and secular epistemologists.
 *
 * PERSPECTIVAL GAP:
 *   The beneficiary seats (religious institutional authority, creationist theological tradition) experience this constraint as coordination—a solution to the genuine problem of maintaining identity coherence in a pluralistic epistemic landscape. The suppressed seats (secular scientists, materialist epistemologists) experience it as pure extraction—coercive imposition of a theological framework on empirical adjudication. The payer seats among religious lay communities face a dual positioning: they benefit from identity coherence but bear the cost of intellectual autonomy reduction and social isolation risks if they question the reading. The engine should compute sharply divergent classifications from these different seats. From the beneficiary perspective, the constraint appears as rope (coordination benefit outweighs cost). From the scientist/epistemologist perspective, it appears as snare (suppression and extraction with minimal genuine coordination). From the lay-believer perspective, it may compute as tangled_rope or even scaffold (temporary support for identity during periods of high epistemic uncertainty). The authored claim (tangled_rope) reflects the structural fact that both coordination and extraction are genuinely present—but which dominates depends entirely on the seat computing the classification.
 *
 * DIRECTIONALITY LOGIC:
 *   Religious institutional authority (institutional power, identity-locked exit, civilizational time horizon): d approaches 0.0 (full beneficiary). They set the reading, control its adjudication, and their institutional identity is constituted through it. No genuine exit option exists without institutional dissolution. Creationist theological tradition (organized power, identity-locked exit): d approaches 0.0-0.15. Membership and standing depend on accepting the reading; exit means ideological death. Secular academic scientists (institutional power, mobile exit, generational horizon): d approaches 0.85-0.95 (full target). They bear active suppression, their interpretations are excluded, but they do have professional mobility (can exit to secular institutions, publication outlets, non-US jurisdictions). Evolutionary biologists (organized power, constrained exit, generational horizon): d approaches 0.75-0.85. They face institutional pressure and career constraints, but organized professional networks offer some protection. Materialist epistemologists (organized power, mobile exit): d approaches 0.70-0.80. They defend their framework but have intellectual and professional mobility. Religious lay communities (moderate power, identity-locked exit, biographical horizon): d approaches 0.45-0.55 (near-symmetric). They benefit from the reading's coordination function but are suppressed in their ability to evaluate alternative frameworks. Identity-lock prevents exit. The overrides are justified by the fact that all scientific seats have access to alternative professional contexts (secular universities, international scientific bodies) that religious believers do not have access to without identity destruction—this asymmetry in exit options (mobile vs. identity-locked) is the structural driver of directionality divergence.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding_problem (maintaining faith-evidence coherence) is contested in status and verdict. Religious authorities argue the problem remains acute—evidence continues to mount, and believers face ongoing pressure to reconcile faith and fact. Scientists argue the problem is artificial—if empirical adjudication followed methodological naturalism, no incoherence would arise. The constraint persists not because it solves a live coordination problem but because religious institutions profit from suppressing alternative adjudication. The measurement data support the mandatrophy reading: extractiveness and theater_ratio rise together through t=15, then theater plateaus while extractiveness stabilizes, suggesting the constraint has transitioned from solving an actual coordination problem (when theater was lower, extraction might have tracked genuine evidence-processing costs) to maintaining interpretive monopoly (when theater is high, extraction serves institutional authority rather than believer welfare). The theater_ratio value of 0.58 (above 0.50 threshold) combined with stable suppression (0.72, held constant to resist scientific counter-evidence) and plateau in extractiveness (0.68, no further rise despite institutional pressure to expand the reading's scope) indicates the constraint exhibits piton characteristics: the primary coordination function has atrophied, but enforcement persists for institutional-power reasons. However, the beneficiary resistance from religious communities is real (not purely theatrical), and the reading does solve an actual identity problem for believers even if religious institutions also profit from monopoly. The constraint is best classified as decaying Tangled Rope rather than pure Piton: it has genuine coordination function (identity coherence) alongside genuine extraction (epistemic monopoly), but the balance is drifting toward theater and suppression rather than addressing new evidence. Mandatrophy is partially resolved: the founding problem has shifted from 'how do we understand the record' to 'whose interpretation authority is legitimate,' a shift that relieves the constraint of falsifiability and shifts it toward theater.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_constructed_reading,
    'Is the creationist reading a natural-law constraint (reflecting actual facts about origins knowable through this framework) or a constructed constraint (reflecting institutional authority''s interest in maintaining epistemic monopoly)?',
    'Track whether the reading''s supporters update their interpretations when new evidence emerges (suggesting commitment to truth-tracking) or defend the reading unchanged against new evidence (suggesting commitment to authority preservation). Examine whether the reading tolerates internal dissent or suppresses doubt.',
    'If natural-law: the constraint should approach mountain classification from beneficiary seats. If constructed: the constraint remains extractive. The distinction determines whether enforcement reflects epistemic standards or institutional power-maintenance.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_reading, conceptual, 'Whether the creationist reading aims at truth about origins or aims at maintaining institutional authority.').

omega_variable(
    suppression_mechanism_structural_or_internalized,
    'Is the measured suppression (0.72) primarily structural (external barriers: school board votes, curriculum mandates, funding restrictions) or internalized (believers'' own conviction that alternative frameworks are illegitimate, false, or harmful)?',
    'Post-suppression trajectory: track whether scientists'' and skeptics'' resistance persists after structural enforcement weakens (indicating internalized suppression among believers would prevent alternative frameworks even without institutional pressure); measure lay believers'' comfort engaging with evolutionary theory when structure permits vs. prohibits it.',
    'If primarily structural: reducing legal/institutional barriers might shift the classification toward rope or even away from extraction. If primarily internalized: suppression travels with believers and persists even after enforcement weakens; the constraint''s effective suppression is higher than the structural measure suggests.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_or_internalized, empirical, 'Whether suppression in this epistemic constraint is structural or internalized.').

omega_variable(
    coordination_function_genuine_or_secondary,
    'Does the creationist reading coordinate a genuine collective-action problem (believers'' need for identity coherence in pluralistic epistemic context), or is the coordination function secondary to the constraint''s primary extractive function (suppressing materialist epistemology)?',
    'Compare: (1) How much extraction would persist if believers could achieve identity coherence through alternative frameworks? (2) Do religious institutions actively resist believers finding coherence through hybrid frameworks (accepting scientific evidence while maintaining faith)? (3) What share of enforcement effort goes to addressing evidence vs. defending interpretive monopoly?',
    'If coordination is genuine and primary: the constraint''s tangled-rope classification is structurally sound. If coordination is secondary or achieved equally well through alternative frameworks: the constraint is more snare-like (extraction riding on a suppressed coordination function).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_function_genuine_or_secondary, empirical, 'Whether the creationist reading''s coordination function is genuine and primary, or secondary to extraction.').

omega_variable(
    scriptural_timeline_compatibility_evidentiary_status,
    'Is the claim that anthropological evidence is compatible with scriptural timeline (young-earth or old-earth creationism) empirically testable, or is it a framework-relative assertion that cannot be falsified within the creationist interpretive system?',
    'Examine whether creationist scholarship generates falsifiable predictions about the fossil record, genetic sequence, or archaeological finds. Does it treat disconfirming evidence as refuting the reading, or reinterpreting both evidence and timeline?',
    'If falsifiable: the creationist reading functions as an empirical hypothesis constrained by evidence. If unfalsifiable: it functions as an interpretive framework that preserves its core premises through reinterpretation; suppression increases because the reading becomes immune to evidence-based challenge.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(scriptural_timeline_compatibility_evidentiary_status, empirical, 'Whether the creationist reading''s core claim is empirically falsifiable or framework-relative.').

omega_variable(
    alternative_epistemologies_exclusion_mechanism,
    'Is the exclusion of indigenous and relational epistemologies from the anthropological-record debate a structural feature of the creationist reading itself, or an incidental effect of the binary creationist-vs-naturalist framing?',
    'Examine whether creationist authorities actively suppress indigenous frameworks or whether they simply occupy a different discourse space. Can a believer hold both creationist theology and indigenous relational epistemology simultaneously?',
    'If structural: the constraint''s extractiveness should increase (it suppresses multiple alternatives, not just materialism). If incidental: the exclusion is a side effect of the reading''s focus, not its core mechanism; this would reframe the constraint as less systematically suppressive.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(alternative_epistemologies_exclusion_mechanism, conceptual, 'Whether excluding alternative epistemologies is central or peripheral to the creationist reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(anthropological_record__creationist_reading, 0, 35).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(anth_tr_t0, anthropological_record__creationist_reading, theater_ratio, 0, 0.32).
narrative_ontology:measurement(anth_tr_t5, anthropological_record__creationist_reading, theater_ratio, 5, 0.38).
narrative_ontology:measurement(anth_tr_t10, anthropological_record__creationist_reading, theater_ratio, 10, 0.44).
narrative_ontology:measurement(anth_tr_t15, anthropological_record__creationist_reading, theater_ratio, 15, 0.5).
narrative_ontology:measurement(anth_tr_t20, anthropological_record__creationist_reading, theater_ratio, 20, 0.55).
narrative_ontology:measurement(anth_tr_t25, anthropological_record__creationist_reading, theater_ratio, 25, 0.57).
narrative_ontology:measurement(anth_tr_t30, anthropological_record__creationist_reading, theater_ratio, 30, 0.58).
narrative_ontology:measurement(anth_tr_t35, anthropological_record__creationist_reading, theater_ratio, 35, 0.58).

% Extraction over time
narrative_ontology:measurement(anth_be_t0, anthropological_record__creationist_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(anth_be_t5, anthropological_record__creationist_reading, base_extractiveness, 5, 0.52).
narrative_ontology:measurement(anth_be_t10, anthropological_record__creationist_reading, base_extractiveness, 10, 0.58).
narrative_ontology:measurement(anth_be_t15, anthropological_record__creationist_reading, base_extractiveness, 15, 0.63).
narrative_ontology:measurement(anth_be_t20, anthropological_record__creationist_reading, base_extractiveness, 20, 0.66).
narrative_ontology:measurement(anth_be_t25, anthropological_record__creationist_reading, base_extractiveness, 25, 0.67).
narrative_ontology:measurement(anth_be_t30, anthropological_record__creationist_reading, base_extractiveness, 30, 0.68).
narrative_ontology:measurement(anth_be_t35, anthropological_record__creationist_reading, base_extractiveness, 35, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(anth_su_t0, anthropological_record__creationist_reading, suppression_requirement, 0, 0.58).
narrative_ontology:measurement(anth_su_t5, anthropological_record__creationist_reading, suppression_requirement, 5, 0.62).
narrative_ontology:measurement(anth_su_t10, anthropological_record__creationist_reading, suppression_requirement, 10, 0.67).
narrative_ontology:measurement(anth_su_t15, anthropological_record__creationist_reading, suppression_requirement, 15, 0.7).
narrative_ontology:measurement(anth_su_t20, anthropological_record__creationist_reading, suppression_requirement, 20, 0.72).
narrative_ontology:measurement(anth_su_t25, anthropological_record__creationist_reading, suppression_requirement, 25, 0.72).
narrative_ontology:measurement(anth_su_t30, anthropological_record__creationist_reading, suppression_requirement, 30, 0.72).
narrative_ontology:measurement(anth_su_t35, anthropological_record__creationist_reading, suppression_requirement, 35, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(anthropological_record__creationist_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(anthropological_record__creationist_reading, 0.12).
narrative_ontology:affects_constraint(anthropological_record__creationist_reading, anthropological_record__naturalist_reading).
narrative_ontology:affects_constraint(anthropological_record__creationist_reading, anthropological_record__indigenous_epistemology_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the kernel 'anthropological_record.' The naturalist_reading treats the same evidence as disclosing materialist human origins through scientific method; this is the upstream reading that competes for epistemic authority in secular institutions. The indigenous_epistemology_reading treats the record as a living relational continuity with ancestors, knowable through oral tradition; this reading is suppressed by the binary creationist-vs-naturalist framing. All three are linked by network.affects_constraints; sibling relationships and reading_relations are declared in cs_structure.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(anthropological_record__creationist_reading, moderate, 0.52).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
