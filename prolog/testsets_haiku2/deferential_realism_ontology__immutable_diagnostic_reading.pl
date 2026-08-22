% ============================================================================
% CONSTRAINT STORY: deferential_realism_ontology__immutable_diagnostic_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
 *   constraint_id: deferential_realism_ontology__immutable_diagnostic_reading
 *   human_readable: Deferential Realism Typology as Immutable Diagnostic Instrument
 *   domain: epistemology/institutional_design/normative_theory
 *
 * SUMMARY:
 *   The Deferential Realism constraint typology is a contested kernel—a
 *   shared commitment to classifying constraints that different parties read
 *   fundamentally differently. This story instantiates the IMMUTABLE
 *   DIAGNOSTIC READING: the claim that the typology is an observational
 *   instrument with fixed referents, where mountains are physical invariants,
 *   snares are measurable extraction mechanisms, and misclassification is
 *   correctable through better observation. Under this reading, the framework
 *   suppresses alternative epistemologies (normative plurality,
 *   constructivism) by treating them as non-scientific. The extraction
 *   measured here is epistemic: the framework extracts from constructivist
 *   and normative scholars the authority to say what counts as a legitimate
 *   classification, routing that authority to metric practitioners. The
 *   suppression is high because the alternative readings are not refuted but
 *   are classified as external to science, which is more effective than
 *   refutation at preventing engagement. Theater rises over the interval as
 *   the framework's scope expands and more methodological choices (how to
 *   measure accessibility_collapse, which reading of 'beneficiary' to adopt)
 *   must be hidden inside the metric definitions to maintain the appearance
 *   of discovery.
 *
 * KEY AGENTS:
 *   - Diagnostic epistemology practitioners: institutional beneficiaries defending the framework's observational status
 *   - Metric observationalism advocates: organized beneficiary seat funding and promoting the framework
 *   - Normative plurality theorists: moderate-power payers whose work is suppressed as non-scientific
 *   - Constructivist skeptics: powerless payers locked by identity into citation of the framework while doubting it
 *   - Framework maintainers: institutional agenda-setter controlling what questions count as legitimate
 *   - Alternative epistemologies: excluded seat (hermeneutic, genealogical, pragmatist) structurally prevented from objecting
 *   - Policy users: powerful beneficiary-payers who gain political cover from the framework's objectivity claim
 *   - Realist epistemologists: observer seat examining whether the framework's fixity claim is justified
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(deferential_realism_ontology__immutable_diagnostic_reading, 0.68).
domain_priors:suppression_score(deferential_realism_ontology__immutable_diagnostic_reading, 0.79).
domain_priors:theater_ratio(deferential_realism_ontology__immutable_diagnostic_reading, 0.52).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(deferential_realism_ontology__immutable_diagnostic_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(deferential_realism_ontology__immutable_diagnostic_reading, suppression_requirement, 0.79).
narrative_ontology:constraint_metric(deferential_realism_ontology__immutable_diagnostic_reading, theater_ratio, 0.52).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(deferential_realism_ontology__immutable_diagnostic_reading, accessibility_collapse, 0.71).
narrative_ontology:constraint_metric(deferential_realism_ontology__immutable_diagnostic_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(deferential_realism_ontology__immutable_diagnostic_reading, tangled_rope).
narrative_ontology:human_readable(deferential_realism_ontology__immutable_diagnostic_reading, "Deferential Realism Typology as Immutable Diagnostic Instrument").
narrative_ontology:topic_domain(deferential_realism_ontology__immutable_diagnostic_reading, "epistemology/institutional_design/normative_theory").

domain_priors:requires_active_enforcement(deferential_realism_ontology__immutable_diagnostic_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(deferential_realism_ontology__immutable_diagnostic_reading, '78ef4e1a-d03b-4b52-b26d-cc5483483763').
narrative_ontology:cs_kernel_codification('78ef4e1a-d03b-4b52-b26d-cc5483483763', formalized).
narrative_ontology:cs_authority_grounding('78ef4e1a-d03b-4b52-b26d-cc5483483763', extraction).
narrative_ontology:cs_interpretation_layer_present('78ef4e1a-d03b-4b52-b26d-cc5483483763').
narrative_ontology:cs_reading_relation('78ef4e1a-d03b-4b52-b26d-cc5483483763', deferential_realism_ontology__hybrid_pragmatic_reading, coexists_with).
narrative_ontology:cs_reading_relation('78ef4e1a-d03b-4b52-b26d-cc5483483763', deferential_realism_ontology__rhetorical_scaffold_reading, coexists_with).
narrative_ontology:cs_axiom('78ef4e1a-d03b-4b52-b26d-cc5483483763', foundational, observable_referent_fixity).
narrative_ontology:cs_axiom_status(observable_referent_fixity, holdable).
narrative_ontology:cs_axiom_grounding('78ef4e1a-d03b-4b52-b26d-cc5483483763', observable_referent_fixity, empirically_contingent).
narrative_ontology:cs_axiom('78ef4e1a-d03b-4b52-b26d-cc5483483763', foundational, misclassification_as_empirical_error).
narrative_ontology:cs_axiom_status(misclassification_as_empirical_error, holdable).
narrative_ontology:cs_axiom_grounding('78ef4e1a-d03b-4b52-b26d-cc5483483763', misclassification_as_empirical_error, empirically_contingent).
narrative_ontology:cs_reference_frame('78ef4e1a-d03b-4b52-b26d-cc5483483763', metric_observationalism_epistemic_foundation).
narrative_ontology:cs_drift_state('78ef4e1a-d03b-4b52-b26d-cc5483483763', contemporary_epistemic_pluralism_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('78ef4e1a-d03b-4b52-b26d-cc5483483763', '2026-06-12T14:32:00Z').
narrative_ontology:cs_kernel_id(deferential_realism_ontology__immutable_diagnostic_reading, deferential_realism_ontology).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(deferential_realism_ontology__immutable_diagnostic_reading, diagnostic_epistemology_practitioners).
narrative_ontology:constraint_beneficiary(deferential_realism_ontology__immutable_diagnostic_reading, metric_observationalism_advocates).
narrative_ontology:constraint_victim(deferential_realism_ontology__immutable_diagnostic_reading, normative_plurality_theorists).
narrative_ontology:constraint_victim(deferential_realism_ontology__immutable_diagnostic_reading, constructivist_classification_skeptics).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(deferential_realism_ontology__immutable_diagnostic_reading, policy_users).
narrative_ontology:constraint_victim(deferential_realism_ontology__immutable_diagnostic_reading, policy_users).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Researchers and theorists committed to the framework's core axiom: that constraint classification is discoverable from fixed observables (extractiveness, suppression, etc.). They design metrics, run the classification engine, publish results arguing that 'snare' is not rhetoric but diagnosis. They benefit from the framework's institutional standing and from the suppression of alternative epistemologies that would require reclassifying their results as normative rather than found.
narrative_ontology:constraint_stakeholder(deferential_realism_ontology__immutable_diagnostic_reading, diagnostic_epistemology_practitioners, beneficiary,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(deferential_realism_ontology__immutable_diagnostic_reading, diagnostic_epistemology_practitioners, agenda_setter).

% Institutions, funding bodies, and policy analysts that have bet institutional resources on the framework's diagnostic character. They fund research programs, commission constraint stories, and defend the framework against critiques that would undermine the observational status of their classifications. They benefit from the framework's claim to objectivity even when application is contentious.
narrative_ontology:constraint_stakeholder(deferential_realism_ontology__immutable_diagnostic_reading, metric_observationalism_advocates, beneficiary,
    organized, generational, constrained, global).

% Scholars and critics who argue that constraint classification fundamentally depends on normative commitments about legitimate beneficiaries, coordination functions, and value distributions. They bear the cost of operating in an epistemic space where their core claims are treated as unscientific or rhetorical. Their work is harder to fund, publish, and gain institutional standing for when the reigning framework suppresses the legitimacy of their epistemological position.
narrative_ontology:constraint_stakeholder(deferential_realism_ontology__immutable_diagnostic_reading, normative_plurality_theorists, payer,
    moderate, biographical, constrained, global).

% Early-career researchers and domain experts (economists, legal scholars, anthropologists) who recognize that the framework's metrics embed normative choices but find themselves locked into citation and classification practices that treat those choices as objective. They bear the suppression cost directly: challenging the framework's observational claims risks career damage, reduced publishability, and loss of access to shared research infrastructure.
narrative_ontology:constraint_stakeholder(deferential_realism_ontology__immutable_diagnostic_reading, constructivist_classification_skeptics, payer,
    powerless, biographical, identity_locked, global).

% The institutional structures (research centers, editorial bodies, standard-setting organizations) that maintain the framework's technical apparatus, prompts, schemas, and validation practices. They enforce the diagnostic reading by controlling what questions count as scientifically legitimate, which alternatives are treated as coherent, and how misclassification is corrected (through better metrics, not through value-reframing).
narrative_ontology:constraint_stakeholder(deferential_realism_ontology__immutable_diagnostic_reading, framework_maintainers, agenda_setter,
    institutional, generational, arbitrage, global).

% Hermeneutic, genealogical, and pragmatist approaches to classification that would frame the DR typology as a normative commitment masquerading as discovery. They are structurally excluded from the framework's conversation: their critiques are not refuted but are classified as external to science, which preempts engagement.
narrative_ontology:constraint_stakeholder(deferential_realism_ontology__immutable_diagnostic_reading, alternative_epistemologies, excluded,
    moderate, biographical, trapped, global).

% Government agencies and NGOs that apply the framework's classifications to real constraints. They benefit from the appearance of objectivity (constraints look diagnosed, not declared) which provides political cover for policy interventions. They also pay: the framework's commitment to immutable referents means it cannot easily accommodate value disagreements about what counts as a legitimate beneficiary, forcing them into either accepting the framework's implicit value judgments or abandoning the tool.
narrative_ontology:constraint_stakeholder(deferential_realism_ontology__immutable_diagnostic_reading, policy_users, beneficiary,
    powerful, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(deferential_realism_ontology__immutable_diagnostic_reading, policy_users, payer).

% Philosophers of science and epistemologists (not invested in DR outcomes) who study how observational instruments are constructed, validated, and standardized. They examine whether the framework's claim to fixed referents is justified or is itself a constructed consensus.
narrative_ontology:constraint_stakeholder(deferential_realism_ontology__immutable_diagnostic_reading, realist_epistemologists, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(deferential_realism_ontology__immutable_diagnostic_reading, diagnostic_epistemology_practitioners).
narrative_ontology:fixing_cost_class(deferential_realism_ontology__immutable_diagnostic_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared vocabulary for constraint classification that (under this reading) eliminates subjective judgment from type assignment. Solves the coordination problem: how can researchers with different normative commitments work from a common classification without the classification dissolving into relativism or rhetorical contest?
% TRANSFER_FUNCTION: Transfers epistemic authority from normative plurality to metric observationalism: moves the power to classify constraints from value-laden judgment to technical instrument operation. Researchers using the framework cede authority to define 'snare' to metric thresholds and suppression measurements.
% ABSENT_VOICES: Constructivist epistemologists and genealogical critics who would argue the framework smuggles normative assumptions into its metrics are excluded from the conversation as non-scientific. They would object that 'extractiveness' itself encodes value choices about what counts as extraction, but the framework's methodological commitment to observationalism precludes treating that objection as legitimate.
% DISAPPEARANCE_RATIONALE: If the framework and its diagnostic reading vanished, constraint classification would fragment: researchers would have no shared vocabulary; policy interventions would be overtly normative rather than appearing to rest on diagnosis; the suppression of alternative epistemologies would lift, and value disagreements about legitimate beneficiaries would surface as foundational rather than resolvable by better metrics.
% FOUNDING_PROBLEM: In early work on constraint structures, different analysts using the same classification (e.g., 'snare') assigned different types to the same constraints based on their normative background. The framework was designed to replace judgment with measurement: if we can define 'extractiveness' operationally and measure it, type assignment becomes deterministic rather than interpretive.
% FOUNDING_PROBLEM_CORROBORATION: The framework's designers assert the founding problem is live and solved by their metrics. Critics (Pragmatic and Rhetorical readings) attest the problem is not solved: measurement still embeds normative choices (choice of referent for epsilon, weighting of suppression, threshold for accessibility_collapse), so the appearance of determinism is theatrical. Independent epistemologists (Realist Epistemology seat) confirm that observational instruments always carry constructive elements; the question is whether those are acknowledged or hidden.
narrative_ontology:disappearance_verdict(deferential_realism_ontology__immutable_diagnostic_reading, world_rearranges).
narrative_ontology:founding_problem_status(deferential_realism_ontology__immutable_diagnostic_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(deferential_realism_ontology__immutable_diagnostic_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(deferential_realism_ontology__immutable_diagnostic_reading, 'none', 1).
narrative_ontology:epsilon_provenance(deferential_realism_ontology__immutable_diagnostic_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness rises from 0.42 to 0.68 over the interval as the framework's scope expands: more domains adopt it, more policy relies on it, more value choices get embedded in metric definitions, and the cost to dissenting researchers (lost funding, reduced publishing) compounds. Suppression rises in parallel (0.62 to 0.79) because the framework's institutional standing grows, making it harder to challenge without being classified as anti-scientific. Theater rises from 0.31 to 0.52 as the framework increasingly must hide methodological choices (like the choice of beneficiary groups used to compute directionality) inside metric definitions to maintain the appearance of discovery. The constraint is CLAIMED as rope (genuine coordination: shared vocabulary solves the pluralism problem) but the measured extractiveness and suppression reflect that the coordination benefit is asymmetric—it benefits diagnostic practitioners and their institutional sponsors far more than constructivists. The metrics are authored independent of the claim, as required; the divergence is the measurement the corpus takes.
 *
 * PERSPECTIVAL GAP:
 *   From the diagnostic practitioners' seat, the framework is a genuine coordination solution: it replaces subjective judgment with shared metrics, enabling pluralistic researchers to communicate without rhetorical contest. The suppression they observe is desirable—it is the suppression of non-scientific claims, which is how science works. From the constructivist seat, the same structure is extraction: the framework trades on the appearance of objectivity to hide normative choices and silence critics who would expose those choices. From the policy user's seat, the framework provides cover for value-laden interventions while appearing to rest on diagnosis. The engine's per-seat computation should reveal these divergences: practitioners compute the constraint as rope or even mountain (genuine necessary coordination); payers compute it as snare (pure extraction of epistemic authority); policy users compute something intermediate (coordination benefits mixed with value judgment suppression). This divergence is not a flaw in the framework—it is the key evidence that the classification depends on whose seat you occupy.
 *
 * DIRECTIONALITY LOGIC:
 *   Diagnostic practitioners and metric advocates sit near the beneficiary end (d ≈ 0.2–0.3): they collect the authority to define what counts as objective classification and the institutional resources that follow. Normative plurality theorists sit near the target end (d ≈ 0.75–0.85): their work is suppressed as unscientific, their funding is harder to obtain, their classifications are not treated as legitimate. Constructivists sit at the target end (d ≈ 0.85): they are identity-locked (professional identity fused with participation in the framework they doubt) and trapped by citation norms, so exit is near-impossible. Policy users sit near symmetric (d ≈ 0.45–0.55): they gain political cover (beneficiary side) but pay in value-judgment suppression—they cannot openly acknowledge that 'legitimate beneficiary' is a normative choice. Framework maintainers sit at beneficiary end (d ≈ 0.15): they control the rules and gate what counts as scientific. The realist epistemologists sit at analytical (d = 0.5): they observe without collecting or paying.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (inter-analyst disagreement on constraint types across normative backgrounds) was real and live at the framework's inception. The reading's claim is that the problem is SOLVED by metric observationalism. But the mandatrophy question cuts deeper: is the founding problem actually resolved, or has it migrated from visible disagreement about classification to hidden disagreement about metrics? The measurement series suggests the latter: as the framework expands (extractiveness rises), the theater_ratio also rises, indicating an increasing ratio of performative activity (defending the metrics' objectivity) to functional activity (actually resolving type disagreements). The suppression rises without significant resistance falling, which indicates the disagreement is being suppressed rather than resolved. A true solution would show resistance rising alongside suppression (indicating active contestation and eventual settlement). Instead, resistance stays flat while suppression rises (indicating one-sided imposition). This mandatrophy signature suggests the founding problem is not solved but driven underground—the framework's mandate (resolve type disagreement through metrics) has outlived its function (it does not actually resolve disagreement; it declares one side unscientific).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    epsilon_referent_constructedness,
    'Is epsilon (base extractiveness) an observable fact about the constraint, or is the choice of what counts as ''extraction'' an embedded normative judgment that makes epsilon constructed rather than discovered?',
    'Case study: take two theorists (one from the diagnostic reading, one from the rhetorical reading) and ask them to measure epsilon on the same constraint using the framework''s definitions. If they produce significantly different values despite using the same definitions, the difference is a metric interpretation choice masquerading as observation.',
    'If epsilon values depend on normative framing, the immutable diagnostic reading collapses—misclassification is not correctable by better metrics because the metrics themselves are value-dependent. The constraint would reclassify toward rhetorical_scaffold_reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(epsilon_referent_constructedness, empirical, 'Whether epsilon is observable or constructed through normative choice.').

omega_variable(
    suppression_vs_epistemological_difference,
    'Is the measured suppression a suppression mechanism (coercive force keeping alternatives out), or is it a symptom of epistemological difference—the framework simply does not recognize alternative methodologies as valid contenders?',
    'Ethnographic study: document whether critics of the diagnostic reading are (a) actively prevented from publishing/speaking (suppression as force), or (b) considered non-scientists whose work is simply not in the conversation (suppression as categorization). The two look similar at macro scale but have different structural causes.',
    'If suppression is primarily categorization rather than force, the constraint remains a tangled_rope (coordination + extraction) but the extraction mechanism is classification rather than coercion. If suppression is active force, the constraint reads as snare (coercive authority extraction masquerading as scientific necessity).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_vs_epistemological_difference, empirical, 'Whether suppression of alternatives is active coercion or passive categorization.').

omega_variable(
    mandatrophy_founding_problem_mismatch,
    'Did the founding problem (inter-analyst disagreement on constraint types) actually dissolve through metric standardization, or has the disagreement merely been driven underground and relabeled as methodological error?',
    'Longitudinal analysis: track disagreement across three time periods: pre-framework, early-adoption, mature-framework. In each period, measure: (1) visible public disagreement on constraint type; (2) private uncertainty among analysts; (3) ratio of public to private disagreement. If public disagreement falls while private uncertainty stays constant or rises, the problem is suppressed, not solved.',
    'If suppressed not solved, the constraint''s mandate has outlived its function and it exhibits mandatrophy signature. The theater_ratio rise (0.31 to 0.52) is consistent with this: more effort spent defending the framework''s objectivity, less effort spent actually resolving disagreements.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mandatrophy_founding_problem_mismatch, empirical, 'Whether the founding problem is solved or suppressed.').

omega_variable(
    reading_kernel_structure_instability,
    'Is the immutable diagnostic reading''s core axiom (observable fixed referents) stable under pressure from the other two readings, or does it gradually collapse as practical application reveals normative choices embedded in the metrics?',
    'Monitor adoption of the framework across domains with strong value heterogeneity (e.g., environmental economics, labor standards, indigenous rights). Track instances where different stakeholders measure the same constraint and get different epsilon values despite using identical definitions. Count instances where the framework is modified or reinterpreted to accommodate value disagreements.',
    'If the axiom is unstable, the reading will gradually shift toward the pragmatic or rhetorical readings. Classification disputes that the immutable reading claims to resolve through metrics will become visible again, exposing the reading''s failure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_kernel_structure_instability, empirical, 'Whether the immutable diagnostic reading''s core axiom can survive application pressure.').

omega_variable(
    alternative_epistemology_exclusion_mechanism,
    'Are constructivist and hermeneutic critiques excluded from the framework''s conversation because they are logically incoherent (as the diagnostic reading claims), or because the framework''s definition of ''scientific'' simply does not recognize them as legitimate (as the rhetorical reading claims)?',
    'Systematic mapping of how the framework treats alternative epistemologies: (a) addresses and refutes their core arguments; (b) classifies them as non-scientific without substantive engagement; (c) simply ignores them. If (b) or (c), the exclusion is categorical rather than rational.',
    'If exclusion is categorical, the suppression measurement is accurate but the reading has suppressed genuine alternatives rather than eliminated false ones. This would lower the accessibility_collapse metric (alternatives are collapsed only because they are not recognized, not because they are logically impossible) and raise the resistance metric (suppressed scholars would resist if given standing).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_epistemology_exclusion_mechanism, empirical, 'Whether alternative epistemologies are refuted or merely excluded by categorical dismissal.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(deferential_realism_ontology__immutable_diagnostic_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(defe_tr_t0, deferential_realism_ontology__immutable_diagnostic_reading, theater_ratio, 0, 0.31).
narrative_ontology:measurement(defe_tr_t5, deferential_realism_ontology__immutable_diagnostic_reading, theater_ratio, 5, 0.38).
narrative_ontology:measurement(defe_tr_t10, deferential_realism_ontology__immutable_diagnostic_reading, theater_ratio, 10, 0.44).
narrative_ontology:measurement(defe_tr_t15, deferential_realism_ontology__immutable_diagnostic_reading, theater_ratio, 15, 0.49).
narrative_ontology:measurement(defe_tr_t20, deferential_realism_ontology__immutable_diagnostic_reading, theater_ratio, 20, 0.51).
narrative_ontology:measurement(defe_tr_t25, deferential_realism_ontology__immutable_diagnostic_reading, theater_ratio, 25, 0.52).

% Extraction over time
narrative_ontology:measurement(defe_be_t0, deferential_realism_ontology__immutable_diagnostic_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(defe_be_t5, deferential_realism_ontology__immutable_diagnostic_reading, base_extractiveness, 5, 0.51).
narrative_ontology:measurement(defe_be_t10, deferential_realism_ontology__immutable_diagnostic_reading, base_extractiveness, 10, 0.59).
narrative_ontology:measurement(defe_be_t15, deferential_realism_ontology__immutable_diagnostic_reading, base_extractiveness, 15, 0.65).
narrative_ontology:measurement(defe_be_t20, deferential_realism_ontology__immutable_diagnostic_reading, base_extractiveness, 20, 0.67).
narrative_ontology:measurement(defe_be_t25, deferential_realism_ontology__immutable_diagnostic_reading, base_extractiveness, 25, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(defe_su_t0, deferential_realism_ontology__immutable_diagnostic_reading, suppression_requirement, 0, 0.62).
narrative_ontology:measurement(defe_su_t5, deferential_realism_ontology__immutable_diagnostic_reading, suppression_requirement, 5, 0.68).
narrative_ontology:measurement(defe_su_t10, deferential_realism_ontology__immutable_diagnostic_reading, suppression_requirement, 10, 0.73).
narrative_ontology:measurement(defe_su_t15, deferential_realism_ontology__immutable_diagnostic_reading, suppression_requirement, 15, 0.76).
narrative_ontology:measurement(defe_su_t20, deferential_realism_ontology__immutable_diagnostic_reading, suppression_requirement, 20, 0.78).
narrative_ontology:measurement(defe_su_t25, deferential_realism_ontology__immutable_diagnostic_reading, suppression_requirement, 25, 0.79).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(deferential_realism_ontology__immutable_diagnostic_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(deferential_realism_ontology__immutable_diagnostic_reading, 0.18).
narrative_ontology:affects_constraint(deferential_realism_ontology__immutable_diagnostic_reading, deferential_realism_ontology__hybrid_pragmatic_reading).
narrative_ontology:affects_constraint(deferential_realism_ontology__immutable_diagnostic_reading, deferential_realism_ontology__rhetorical_scaffold_reading).
narrative_ontology:affects_constraint(deferential_realism_ontology__immutable_diagnostic_reading, constraint_typology_empirical_validity).
narrative_ontology:affects_constraint(deferential_realism_ontology__immutable_diagnostic_reading, metric_epistemology_authority).

% DUAL FORMULATION NOTE:
% The deferential_realism_ontology kernel contains three structurally distinct constraint readings: (1) immutable_diagnostic_reading (this story) treats the typology as an observational instrument with fixed referents; (2) hybrid_pragmatic_reading treats the typology as having a fixed core but contested periphery; (3) rhetorical_scaffold_reading treats the typology as a normative vocabulary where classification is declared, not discovered. These are not three measurements of one constraint but three different constraints arising from one contested kernel. Each reading has distinct epsilon values, distinct beneficiary/victim structures, and distinct mechanisms. They coexist across different institutional communities and research traditions. All three should be authored as separate constraint stories linked through network.affects_constraints, as they contest the same underlying kernel about the epistemological status of the DR classification system.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(deferential_realism_ontology__immutable_diagnostic_reading, powerless, 0.85).
constraint_indexing:directionality_override(deferential_realism_ontology__immutable_diagnostic_reading, moderate, 0.72).
constraint_indexing:directionality_override(deferential_realism_ontology__immutable_diagnostic_reading, institutional, 0.22).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
