% ============================================================================
% CONSTRAINT STORY: deferential_realism_ontology__immutable_diagnostic_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_deferential_realism_ontology__immutable_diagnostic_reading, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: deferential_realism_ontology__immutable_diagnostic_reading
 *   human_readable: Immutable Diagnostic Reading of the Constraint Typology
 *   domain: epistemological/institutional
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the contested kernel
 *   'deferential_realism_ontology': the immutable-diagnostic reading, on
 *   which the six-category typology is an observational instrument with fixed
 *   referents — mountains are physical invariants, snares are measurable
 *   extraction mechanisms, and misclassification is error correctable through
 *   better observation. The standing arrangement under contest is the
 *   framework community's classification practice as bound by that
 *   commitment. Its structure is genuinely double: the pinned referents solve
 *   a real collective-action problem (comparability, cumulative correction,
 *   terminable disputes), AND the same commitment suppresses alternative
 *   framings — a framing objection is not received as a position but
 *   processed as an error, and the seats that operate the arbitration
 *   apparatus collect the authority that terminates disputes. Per the
 *   epsilon-invariance principle, the colloquial label 'the constraint
 *   typology' decomposes into three structurally distinct constraints (this
 *   reading plus the rhetorical-scaffold and hybrid-pragmatic siblings,
 *   linked via network.affects_constraints). The siblings share this story's
 *   referent but author different epsilon: the rhetorical-scaffold reading
 *   assesses the arrangement through persuasion-value and
 *   declared-beneficiary lenses; the hybrid-pragmatic reading splits the
 *   referent into a fixed core and a constitutively contested periphery. This
 *   file authors epsilon = 0.60 for the diagnostic arrangement as this
 *   reading's own structural analysis finds it — the reading does not flatter
 *   itself: taking its own delta seriously means admitting that converting
 *   rival framings into measurable disagreement is a cost the arrangement
 *   imposes, not noise it suffers. KEY AGENTS (by structural relationship): -
 *   framework_stewards: Primary beneficiary and agenda-setter
 *   (institutional/arbitrage) — administers the corpus, defines correct
 *   reading, collects the authority rents - metric_arbiters: Secondary
 *   beneficiary (institutional/constrained) — dispute resolution terminates
 *   at their instruments - incumbent_classifiers: Beneficiary under identity
 *   lock (powerful/identity_locked) — fixed referents insulate their
 *   back-catalog - constructivist_analysts: Primary target
 *   (organized/constrained) — framings processed as candidate errors -
 *   boundary_case_researchers: Target (moderate/trapped) — framing labor
 *   absorbed as measurement variance - dissenting_readers: Target
 *   (organized/constrained) — rival readings without standing inside
 *   adjudication - classified_subject_communities: Excluded seat
 *   (powerless/trapped) — sorted by the corpus, never seated in it -
 *   meta_analyst_observer: Analytical observer (analytical/analytical) — sees
 *   the full structure from outside the review chain
 *
 * KEY AGENTS:
 *   - framework_stewards: Primary beneficiary and agenda-setter (institutional/arbitrage) — administers the corpus and collects the authority that accrues to defining correct observation
 *   - metric_arbiters: Secondary beneficiary (institutional/constrained) — dispute resolution terminates at their instruments, concentrating standing in the arbiter seat
 *   - incumbent_classifiers: Beneficiary under identity lock (powerful/identity_locked) — fixed referents protect their published back-catalog from retrospective reframing
 *   - constructivist_analysts: Primary target (organized/constrained) — hold that classification involves judgment about purposes and beneficiaries; their claims are processed as errors
 *   - boundary_case_researchers: Target (moderate/trapped) — framing labor at contested edges is recorded as variance to be reduced, never credited
 *   - dissenting_readers: Target (organized/constrained) — holders of the sibling readings; their positions enter adjudication only as data about disagreement
 *   - classified_subject_communities: Excluded seat (powerless/trapped) — the communities the corpus sorts into categories, with no seat in the adjudication that sorts them
 *   - meta_analyst_observer: Analytical observer (analytical/analytical) — methodologists outside the review chain who can distinguish convergence from discipline
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(deferential_realism_ontology__immutable_diagnostic_reading, 0.6).
domain_priors:suppression_score(deferential_realism_ontology__immutable_diagnostic_reading, 0.72).
domain_priors:theater_ratio(deferential_realism_ontology__immutable_diagnostic_reading, 0.31).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(deferential_realism_ontology__immutable_diagnostic_reading, extractiveness, 0.6).
narrative_ontology:constraint_metric(deferential_realism_ontology__immutable_diagnostic_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(deferential_realism_ontology__immutable_diagnostic_reading, theater_ratio, 0.31).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(deferential_realism_ontology__immutable_diagnostic_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(deferential_realism_ontology__immutable_diagnostic_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(deferential_realism_ontology__immutable_diagnostic_reading, tangled_rope).
narrative_ontology:human_readable(deferential_realism_ontology__immutable_diagnostic_reading, "Immutable Diagnostic Reading of the Constraint Typology").
narrative_ontology:topic_domain(deferential_realism_ontology__immutable_diagnostic_reading, "epistemological/institutional").

domain_priors:requires_active_enforcement(deferential_realism_ontology__immutable_diagnostic_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(deferential_realism_ontology__immutable_diagnostic_reading, 'f4fc3687-5b81-405a-accc-98c91e0dc2c4').
narrative_ontology:cs_kernel_codification('f4fc3687-5b81-405a-accc-98c91e0dc2c4', formalized).
narrative_ontology:cs_authority_grounding('f4fc3687-5b81-405a-accc-98c91e0dc2c4', expertise).
narrative_ontology:cs_interpretation_layer_present('f4fc3687-5b81-405a-accc-98c91e0dc2c4').
narrative_ontology:cs_reading_relation('f4fc3687-5b81-405a-accc-98c91e0dc2c4', deferential_realism_ontology__rhetorical_scaffold_reading, forecloses).
narrative_ontology:cs_reading_relation('f4fc3687-5b81-405a-accc-98c91e0dc2c4', deferential_realism_ontology__hybrid_pragmatic_reading, influences).
narrative_ontology:cs_axiom('f4fc3687-5b81-405a-accc-98c91e0dc2c4', foundational, constraint_categories_have_fixed_referents).
narrative_ontology:cs_axiom_status(constraint_categories_have_fixed_referents, holdable).
narrative_ontology:cs_axiom_grounding('f4fc3687-5b81-405a-accc-98c91e0dc2c4', constraint_categories_have_fixed_referents, empirically_contingent).
narrative_ontology:cs_axiom('f4fc3687-5b81-405a-accc-98c91e0dc2c4', foundational, classification_disputes_are_resolvable_by_observation).
narrative_ontology:cs_axiom_status(classification_disputes_are_resolvable_by_observation, holdable).
narrative_ontology:cs_axiom_grounding('f4fc3687-5b81-405a-accc-98c91e0dc2c4', classification_disputes_are_resolvable_by_observation, empirically_contingent).
narrative_ontology:cs_axiom('f4fc3687-5b81-405a-accc-98c91e0dc2c4', secondary, epsilon_values_are_discoverable_properties).
narrative_ontology:cs_axiom_status(epsilon_values_are_discoverable_properties, holdable).
narrative_ontology:cs_axiom_grounding('f4fc3687-5b81-405a-accc-98c91e0dc2c4', epsilon_values_are_discoverable_properties, empirically_contingent).
narrative_ontology:cs_reference_frame('f4fc3687-5b81-405a-accc-98c91e0dc2c4', fixed_referent_diagnostic_instrument).
narrative_ontology:cs_drift_state('f4fc3687-5b81-405a-accc-98c91e0dc2c4', post_reading_indexation_rulings, gap(axiom_overriding, substantial, true)).
narrative_ontology:cs_created_at('f4fc3687-5b81-405a-accc-98c91e0dc2c4', '').
narrative_ontology:cs_kernel_id(deferential_realism_ontology__immutable_diagnostic_reading, deferential_realism_ontology).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(deferential_realism_ontology__immutable_diagnostic_reading, framework_stewards).
narrative_ontology:constraint_beneficiary(deferential_realism_ontology__immutable_diagnostic_reading, metric_arbiters).
narrative_ontology:constraint_beneficiary(deferential_realism_ontology__immutable_diagnostic_reading, incumbent_classifiers).
narrative_ontology:constraint_victim(deferential_realism_ontology__immutable_diagnostic_reading, constructivist_analysts).
narrative_ontology:constraint_victim(deferential_realism_ontology__immutable_diagnostic_reading, boundary_case_researchers).
narrative_ontology:constraint_victim(deferential_realism_ontology__immutable_diagnostic_reading, dissenting_readers).
narrative_ontology:constraint_vindicates(deferential_realism_ontology__immutable_diagnostic_reading, fixed_referent_doctrine).
narrative_ontology:constraint_vindicates(deferential_realism_ontology__immutable_diagnostic_reading, epsilon_discoverability_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintain the classification corpus and the measurement apparatus that adjudicates it, and set the review norms that determine which framing questions count as legitimate and which count as confusion. Because they define what counts as a correct reading, they can restate any challenge to the categories as a measurement problem to be solved rather than a rival account to be entertained. Their skills transfer readily to any standards-governance role, but the corpus they built is their standing.
narrative_ontology:constraint_stakeholder(deferential_realism_ontology__immutable_diagnostic_reading, framework_stewards, agenda_setter,
    institutional, generational, arbitrage, global).

% Operate the quantitative checks that settle classification disputes. When two analysts disagree about a case, the appeal runs through their instruments, which makes their sign-off the effective last word. Their professional standing depends on dispute resolution terminating at the metrics; no comparable terminal role exists for them outside the framework.
narrative_ontology:constraint_stakeholder(deferential_realism_ontology__immutable_diagnostic_reading, metric_arbiters, beneficiary,
    institutional, biographical, constrained, global).

% Senior analysts whose published classifications form much of the corpus back-catalog. Stable categories protect their past work from retrospective reframing: if the referents are fixed, earlier verdicts stay valid indefinitely. Their professional identity is built on having produced correct readings, and entertaining the possibility that the categories are constructed would devalue their accumulated body of work, so they do not entertain it.
narrative_ontology:constraint_stakeholder(deferential_realism_ontology__immutable_diagnostic_reading, incumbent_classifiers, beneficiary,
    powerful, biographical, identity_locked, global).

% Analysts who hold that sorting a case into a category involves judgment about purposes and beneficiaries, not measurement alone. Inside the framework's venues their claims are processed as candidate errors — flagged, measured, and corrected — rather than received as rival accounts of what classification is. Their publication standing and peer networks sit inside the framework, so exit means losing their audience.
narrative_ontology:constraint_stakeholder(deferential_realism_ontology__immutable_diagnostic_reading, constructivist_analysts, payer,
    organized, biographical, constrained, global).

% Researchers who work the contested edges: cases where small framing choices flip the verdict. The adjudication absorbs their framing labor as noise — the care they take drawing boundaries is recorded as measurement variance to be reduced, never credited as a distinct account. Their subject matter exists only inside the framework's categories, so they cannot take their questions elsewhere.
narrative_ontology:constraint_stakeholder(deferential_realism_ontology__immutable_diagnostic_reading, boundary_case_researchers, payer,
    moderate, biographical, trapped, global).

% Hold the rival accounts of the typology — that its categories are declared rather than discovered, or fixed only at the core. They publish critiques and attend the same workshops, but inside the adjudication their positions carry no standing: whatever they offer enters the record as data about disagreement, not as a competing reading. Leaving the conversation would end their ability to influence the framework they study.
narrative_ontology:constraint_stakeholder(deferential_realism_ontology__immutable_diagnostic_reading, dissenting_readers, payer,
    organized, biographical, constrained, global).

% The communities and institutions that the corpus sorts into categories — labeled as coordinated, extractive, or inertial — without any seat in the adjudication that labels them. They learn of their classification when it publishes. Their recourse is protest outside the framework, which the adjudication then records as additional data about the case rather than as a challenge to the frame.
narrative_ontology:constraint_stakeholder(deferential_realism_ontology__immutable_diagnostic_reading, classified_subject_communities, excluded,
    powerless, generational, trapped, global).

% Philosophers of measurement and methodologists outside the framework's review chain. They watch how the framework settles its own disputes, compare its practice with the standardization literature of other sciences, and can say what participants cannot: where convergence reflects the world and where it reflects discipline.
narrative_ontology:constraint_stakeholder(deferential_realism_ontology__immutable_diagnostic_reading, meta_analyst_observer, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(deferential_realism_ontology__immutable_diagnostic_reading, framework_stewards).
narrative_ontology:fixing_cost_class(deferential_realism_ontology__immutable_diagnostic_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Gives every analyst the same referents for the six categories, so a classification made in one place means the same thing everywhere else and disagreements can be settled by returning to the case rather than renegotiating definitions. Without pinned referents, corpus-wide comparison and cumulative error correction are impossible.
% TRANSFER_FUNCTION: Moves epistemic authority to whoever operates the measurement apparatus — dispute resolutions terminate at the metrics — and moves framing labor from the analysts who draw contested boundaries into the instrument's error term, uncredited. Standing flows away from holders of rival framings and toward the arbiters of correct observation.
% ABSENT_VOICES: The subjects of the corpus's classifications are never seated to contest the frame in which they were sorted; they appear only as cases. Holders of the rival readings speak in adjacent venues, but their framings are structurally barred from counting as positions inside adjudication — they enter the record as disagreement to be measured. Coalition potential for the excluded seat is low: the classified communities are dispersed across unrelated domains and share no forum.
% DISAPPEARANCE_RATIONALE: If the fixed-referent commitment lapsed overnight, every settled classification would reopen as a framing dispute, corpus comparability would collapse until a successor standard stabilized, and the arbiter seats' authority would evaporate with the terminal role of the metrics. The framework would keep its vocabulary but lose its adjudication.
% FOUNDING_PROBLEM: Early framework practice allowed analysts to redefine categories mid-dispute, which made classifications unfalsifiable and corpus-wide comparison impossible. The diagnostic commitment was built to pin the referents so that disagreement would be observational rather than semantic.
% FOUNDING_PROBLEM_CORROBORATION: Methodologists outside the framework corroborate the founding problem: the standardization literature in metrology and taxonomy documents the same relabeling pathology and the same fix. But the same sources attest that pinning referents does not require treating rival framings as errors, and the rival-reading holders attest that the solution overreached its problem. Corroboration for the problem comes from outside the benefiting parties; corroboration for the solution's current scope does not.
narrative_ontology:disappearance_verdict(deferential_realism_ontology__immutable_diagnostic_reading, world_rearranges).
narrative_ontology:founding_problem_status(deferential_realism_ontology__immutable_diagnostic_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(deferential_realism_ontology__immutable_diagnostic_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(deferential_realism_ontology__immutable_diagnostic_reading, 'none', 1).
narrative_ontology:epsilon_provenance(deferential_realism_ontology__immutable_diagnostic_reading, 0.6, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(deferential_realism_ontology__immutable_diagnostic_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(deferential_realism_ontology__immutable_diagnostic_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(deferential_realism_ontology__immutable_diagnostic_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Suppression is high (0.72) because the reading's persistence requires actively recasting framing objections as observational errors: review norms decline framing-first papers, 'the category is constructed' claims are answered with 'measure better,' and the ruling apparatus processes rival readings as input data rather than positions. Suppression is authored as a raw structural property — the engine scales only extractiveness by directionality and scope. Extractiveness is moderate-high (0.60): framing labor is absorbed uncredited, dissent standing is converted into correctable noise, and authority rents accrue to the arbiter seats — but the coordination benefit is real and broadly shared, which caps epsilon below snare levels and is exactly why the type is tangled_rope rather than snare. Theater_ratio (0.31) reflects a growing share of calibration and validation activity that demonstrates the instrument's authority rather than revising it. Accessibility_collapse (0.50) is honest partiality: alternative framings remain available — two sibling readings are live and published — but they are suppressed inside adjudication rather than eliminated. Resistance (0.62) is sustained constructivist and pragmatic dissent plus the framework's own reading-indexation rulings, which concede ground against the reading's discoverability premise. The measurement series run on one shared time grid (every tracked metric authored at every examined time point) and are monotonically rising — an enforcement ratchet, not a cycle; no intermittent-reinforcement dynamic is present, so no cyclical commentary applies. Estimated suppression composition: roughly 60% structural (venue gatekeeping, review norms, ruling apparatus) and 40% internalized (training socialization that teaches analysts to hear framing questions as beginner errors), routed to the suppression_mechanism_ambiguity omega.
 *
 * PERSPECTIVAL GAP:
 *   The arbiter seats and the payer seats compute different types from identical structural data. From the steward and arbiter positions the arrangement is clean observation: their disputes genuinely terminate when the metrics speak, so the fixed-referent commitment presents itself as nothing but epistemic hygiene. From the constructivist, researcher, and dissenting seats the same structure operates as enforced silence: their framings never enter the record as positions, only as variance to be reduced. Two institutional actors at the same nominal power level diverge by exit: stewards hold arbitrage-grade exit (their governance skills transfer to any standards body) and therefore experience the arrangement as theirs; arbiters are constrained (no terminal role exists elsewhere) and defend it as indispensable. Among same-level payers, constructivist analysts and dissenting readers differ in what is taken from them: the former lose the status of their claims inside adjudication, the latter lose the standing of their readings as readings. The engine computes this divergence from the structural data; the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary/victim declarations drive the derivation: framework_stewards, metric_arbiters, and incumbent_classifiers sit near the beneficiary end (low d) — the arrangement subsidizes them with authority, insulation, and terminal standing. Constructivist_analysts, boundary_case_researchers, and dissenting_readers sit near the target end (high d) — they pay in framing labor, standing, and voice, with constrained or trapped exit amplifying their effective extraction; boundary_case_researchers are the fullest targets (trapped: their subject matter exists only inside the categories). Classified_subject_communities are excluded rather than coordinated — their exclusion is not the enforcement object here (unlike a payment-rail exclusion) but a scope fact: the adjudication's frame never includes the sorted. One override is declared: the derivation reads identity_locked exit as trap-like and would push incumbent_classifiers' d toward the target end, but their lock binds them TO the frame as beneficiaries — immobility here deepens rather than offsets their benefit — so 'powerful' is overridden to d = 0.18. The override applies to the only powerful-atom stakeholder in the story, so no collateral distortion occurs.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — unconstrained relabeling making classifications unfalsifiable — is still live in weakened form: any classification system faces semantic drift, and the pinned referents continue to do real work. The arrangement has therefore not outlived its function, and mandatrophy is not resolved. But the mandate has crept: the original scope was 'pin referents so disagreement is observational'; the current scope adds 'treat rival framings as errors,' which the founding problem never required. The tangled_rope classification is what prevents mislabeling here: a pure-extraction reading would erase the genuine comparability function (which the standardization literature corroborates as necessary), and a pure-coordination reading would erase the suppression the delta documents. The forward risk is drift toward piton: theater_ratio is climbing, and if observational-resolution programs degenerate into validation ritual — demonstrating the instrument rather than testing it — the coordination half will atrophy while the enforcement half persists theatrically. The rising theater series is the early indicator to watch.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indexicality,
    'Which structural features of this story are properties of the typology itself, and which are artifacts of instantiating the immutable-diagnostic reading specifically?',
    'Compile the sibling readings (rhetorical_scaffold_reading, hybrid_pragmatic_reading) as separate stories and diff the computed beneficiary/victim sets, enforcement requirements, and per-seat classifications against this file.',
    'Under the rhetorical-scaffold sibling, the payer set shifts toward audiences persuaded rather than analysts silenced, and enforcement becomes rhetorical rather than observational; under the hybrid sibling, the victim set contracts to the contested periphery. Reading-indexed structure means cross-reading comparisons of this story''s seats are comparisons of different constraints, not different views of one.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_indexicality, conceptual, 'Committer-frame routing: this constraint is one reading of kernel deferential_realism_ontology; sibling readings instantiate different constraints with different structural deltas.').

omega_variable(
    natural_kind_vs_constructed_standard,
    'Is the typology''s category structure a discovered kind-structure of social and physical reality, or a constructed standard whose stability serves the seats that administer it?',
    'Test whether category boundaries converge under observers with no stake in the administering seats, and whether boundary placement survives changes in who funds and staffs the corpus.',
    'If the categories are constructed, the fixed-referent doctrine is a beneficiary-serving stabilization rather than a discovery, and the reading''s warrant collapses into institutional self-maintenance; if discovered, the suppression measured here is wasted coercive overhead on something that would hold without it.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_kind_vs_constructed_standard, conceptual, 'Whether the fixed referents the reading asserts are found or made.').

omega_variable(
    convergence_vs_discipline,
    'Does inter-analyst convergence on classifications indicate successful observation of real referents, or successful discipline of deviant framings?',
    'Compare convergence rates across domains where deviant framings carry career cost against domains where they do not; convergent observation predicts cost-invariant convergence, disciplined convergence predicts cost-sensitive convergence.',
    'If convergence is cost-sensitive, the reading''s central evidential warrant (that the metrics settle disputes because the referents are real) is confounded by its own enforcement, and the apparent success of the diagnostic instrument is partly an artifact of the suppression it applies.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(convergence_vs_discipline, empirical, 'Observational confound at the heart of the reading''s self-warrant.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression of alternative framings structural (venue gatekeeping, review norms, ruling apparatus) or internalized (analysts trained to treat framing questions as beginner errors)?',
    'Post-exit trajectory: track analysts who leave the framework''s venues; if their framing questions persist and flourish once the gatekeeping is removed, a substantial share of the suppression was internalized.',
    'If internalized, the constraint''s effective suppression exceeds the structural measure — departing analysts carry the frame with them, and the payer seats'' apparent acquiescence understates the enforcement burden the arrangement actually imposes.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural versus internalized suppression mechanism; commentary estimates roughly 60% structural (review norms, venue control, ruling apparatus) and 40% internalized (training socialization).').

omega_variable(
    metric_neutrality_circularity,
    'Are the metrics that settle classification disputes neutral arbiters, or do they embed the fixed-referent frame''s own commitments, making appeals to them circular?',
    'Audit metric construction provenance: identify which design choices presuppose that categories have fixed referents and that epsilon is observer-independent, then test whether metrics redesigned without those presuppositions yield different dispute outcomes.',
    'If the metrics embed the frame, then ''disputes are resolved by appealing to observable metrics'' resolves disputes only inside the frame, and the reading''s claim to observational finality is a closure mechanism rather than a verification procedure.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(metric_neutrality_circularity, conceptual, 'Whether the arbitration apparatus is independent of the frame it arbitrates for.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(deferential_realism_ontology__immutable_diagnostic_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(defe_tr_t0, deferential_realism_ontology__immutable_diagnostic_reading, theater_ratio, 0, 0.14).
narrative_ontology:measurement(defe_tr_t4, deferential_realism_ontology__immutable_diagnostic_reading, theater_ratio, 4, 0.17).
narrative_ontology:measurement(defe_tr_t8, deferential_realism_ontology__immutable_diagnostic_reading, theater_ratio, 8, 0.2).
narrative_ontology:measurement(defe_tr_t12, deferential_realism_ontology__immutable_diagnostic_reading, theater_ratio, 12, 0.23).
narrative_ontology:measurement(defe_tr_t16, deferential_realism_ontology__immutable_diagnostic_reading, theater_ratio, 16, 0.26).
narrative_ontology:measurement(defe_tr_t20, deferential_realism_ontology__immutable_diagnostic_reading, theater_ratio, 20, 0.28).
narrative_ontology:measurement(defe_tr_t24, deferential_realism_ontology__immutable_diagnostic_reading, theater_ratio, 24, 0.31).

% Extraction over time
narrative_ontology:measurement(defe_be_t0, deferential_realism_ontology__immutable_diagnostic_reading, base_extractiveness, 0, 0.36).
narrative_ontology:measurement(defe_be_t4, deferential_realism_ontology__immutable_diagnostic_reading, base_extractiveness, 4, 0.42).
narrative_ontology:measurement(defe_be_t8, deferential_realism_ontology__immutable_diagnostic_reading, base_extractiveness, 8, 0.47).
narrative_ontology:measurement(defe_be_t12, deferential_realism_ontology__immutable_diagnostic_reading, base_extractiveness, 12, 0.52).
narrative_ontology:measurement(defe_be_t16, deferential_realism_ontology__immutable_diagnostic_reading, base_extractiveness, 16, 0.55).
narrative_ontology:measurement(defe_be_t20, deferential_realism_ontology__immutable_diagnostic_reading, base_extractiveness, 20, 0.58).
narrative_ontology:measurement(defe_be_t24, deferential_realism_ontology__immutable_diagnostic_reading, base_extractiveness, 24, 0.6).

% Suppression requirement over time
narrative_ontology:measurement(defe_su_t0, deferential_realism_ontology__immutable_diagnostic_reading, suppression_requirement, 0, 0.44).
narrative_ontology:measurement(defe_su_t4, deferential_realism_ontology__immutable_diagnostic_reading, suppression_requirement, 4, 0.51).
narrative_ontology:measurement(defe_su_t8, deferential_realism_ontology__immutable_diagnostic_reading, suppression_requirement, 8, 0.57).
narrative_ontology:measurement(defe_su_t12, deferential_realism_ontology__immutable_diagnostic_reading, suppression_requirement, 12, 0.62).
narrative_ontology:measurement(defe_su_t16, deferential_realism_ontology__immutable_diagnostic_reading, suppression_requirement, 16, 0.66).
narrative_ontology:measurement(defe_su_t20, deferential_realism_ontology__immutable_diagnostic_reading, suppression_requirement, 20, 0.69).
narrative_ontology:measurement(defe_su_t24, deferential_realism_ontology__immutable_diagnostic_reading, suppression_requirement, 24, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(deferential_realism_ontology__immutable_diagnostic_reading, information_standard).
narrative_ontology:affects_constraint(deferential_realism_ontology__immutable_diagnostic_reading, deferential_realism_ontology__rhetorical_scaffold_reading).
narrative_ontology:affects_constraint(deferential_realism_ontology__immutable_diagnostic_reading, deferential_realism_ontology__hybrid_pragmatic_reading).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition per the epsilon-invariance principle: the colloquial label 'the constraint typology' conflates three structurally distinct commitments that share one referent (the framework community's classification practice) and diverge in epsilon, victim sets, and enforcement structure. This file carries the immutable-diagnostic reading (epsilon 0.60, high suppression, discoverability premise). The rhetorical-scaffold sibling authors the arrangement through a persuasion-value lens (declared beneficiaries, rhetorical enforcement); the hybrid-pragmatic sibling splits the referent into a fixed observational core and a constitutively contested periphery, contracting the victim set accordingly. This reading is upstream: its observational warrant and enforcement practices are what the siblings define themselves against. All three files link one another via affects_constraints; no single story may hedge epsilon across readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(deferential_realism_ontology__immutable_diagnostic_reading, powerful, 0.18).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
