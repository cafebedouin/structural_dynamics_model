% ============================================================================
% CONSTRAINT STORY: deferential_realism_ontology__rhetorical_scaffold_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_deferential_realism_ontology__rhetorical_scaffold_reading, []).

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
 *   constraint_id: deferential_realism_ontology__rhetorical_scaffold_reading
 *   human_readable: Deferential Realism Constraint Typology as Normative Rhetorical Scaffold
 *   domain: epistemology/institutional_design
 *
 * SUMMARY:
 *   The Deferential Realism constraint typology—mountain, rope, tangled rope,
 *   snare, scaffold, piton—is a framework for analyzing institutions. The
 *   rhetorical scaffold reading treats the typology itself as a normative
 *   vocabulary for policy advocacy: classification is not discovered through
 *   measurement but declared through normative judgment about which
 *   beneficiaries are legitimate. Under this reading, 'snare' is not a
 *   diagnosed structural property but a rhetorical move that reframes an
 *   institution as serving illegitimate beneficiaries. The framework's power
 *   lies in its persuasive capacity to expose what mechanisms 'truly'
 *   are—exposure that depends on prior normative commitments about
 *   legitimacy, not on measurement alone. This reading competes with the
 *   immutable diagnostic reading (classification is discoverable through
 *   measurement) and the hybrid pragmatic reading (some classifications are
 *   measurement-grounded, others normative). The three readings are not
 *   competing theories of the same fact; they are competing claims about what
 *   kind of apparatus the typology IS.
 *
 * KEY AGENTS:
 *   - normative_theorists: Develop and control the typology; frame snare/rope as expressions of normative judgment about legitimacy
 *   - critical_policy_advocates: Use the typology to delegitimize mechanisms they oppose; benefit from vocabulary that converts advocacy into diagnosis
 *   - measurement_grounded_theorists: Argue classification must anchor in observable structure; displaced by the scaffold reading's normative prioritization
 *   - institutional_designers: Treat the typology as a diagnostic tool; face higher uncertainty when classification depends on normative frameworks
 *   - policy_makers: Receive competing typological claims; must navigate ambiguity about whether classification is measurement or advocacy
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(deferential_realism_ontology__rhetorical_scaffold_reading, 0.68).
domain_priors:suppression_score(deferential_realism_ontology__rhetorical_scaffold_reading, 0.31).
domain_priors:theater_ratio(deferential_realism_ontology__rhetorical_scaffold_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(deferential_realism_ontology__rhetorical_scaffold_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(deferential_realism_ontology__rhetorical_scaffold_reading, suppression_requirement, 0.31).
narrative_ontology:constraint_metric(deferential_realism_ontology__rhetorical_scaffold_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(deferential_realism_ontology__rhetorical_scaffold_reading, accessibility_collapse, 0.38).
narrative_ontology:constraint_metric(deferential_realism_ontology__rhetorical_scaffold_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(deferential_realism_ontology__rhetorical_scaffold_reading, scaffold).
narrative_ontology:human_readable(deferential_realism_ontology__rhetorical_scaffold_reading, "Deferential Realism Constraint Typology as Normative Rhetorical Scaffold").
narrative_ontology:topic_domain(deferential_realism_ontology__rhetorical_scaffold_reading, "epistemology/institutional_design").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(deferential_realism_ontology__rhetorical_scaffold_reading, 'dde23e39-20b3-4413-9ab6-97e547c24362').
narrative_ontology:cs_kernel_codification('dde23e39-20b3-4413-9ab6-97e547c24362', formalized).
narrative_ontology:cs_authority_grounding('dde23e39-20b3-4413-9ab6-97e547c24362', extraction).
narrative_ontology:cs_interpretation_layer_present('dde23e39-20b3-4413-9ab6-97e547c24362').
narrative_ontology:cs_reading_relation('dde23e39-20b3-4413-9ab6-97e547c24362', deferential_realism_ontology__immutable_diagnostic_reading, coexists_with).
narrative_ontology:cs_reading_relation('dde23e39-20b3-4413-9ab6-97e547c24362', deferential_realism_ontology__hybrid_pragmatic_reading, coexists_with).
narrative_ontology:cs_axiom('dde23e39-20b3-4413-9ab6-97e547c24362', foundational, classification_is_normative_declaration).
narrative_ontology:cs_axiom_status(classification_is_normative_declaration, holdable).
narrative_ontology:cs_axiom_grounding('dde23e39-20b3-4413-9ab6-97e547c24362', classification_is_normative_declaration, deontological).
narrative_ontology:cs_axiom('dde23e39-20b3-4413-9ab6-97e547c24362', secondary, beneficiary_legitimacy_constructed_through_advocacy).
narrative_ontology:cs_axiom_status(beneficiary_legitimacy_constructed_through_advocacy, holdable).
narrative_ontology:cs_axiom_grounding('dde23e39-20b3-4413-9ab6-97e547c24362', beneficiary_legitimacy_constructed_through_advocacy, instrumental).
narrative_ontology:cs_reference_frame('dde23e39-20b3-4413-9ab6-97e547c24362', normative_typology_for_policy_critique).
narrative_ontology:cs_drift_state('dde23e39-20b3-4413-9ab6-97e547c24362', contemporary_measurement_challenge_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('dde23e39-20b3-4413-9ab6-97e547c24362', '').
narrative_ontology:cs_kernel_id(deferential_realism_ontology__rhetorical_scaffold_reading, deferential_realism_ontology).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(deferential_realism_ontology__rhetorical_scaffold_reading, critical_policy_advocates).
narrative_ontology:constraint_beneficiary(deferential_realism_ontology__rhetorical_scaffold_reading, normative_theorists).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(deferential_realism_ontology__rhetorical_scaffold_reading, measurement_grounded_theorists).
narrative_ontology:constraint_victim(deferential_realism_ontology__rhetorical_scaffold_reading, institutional_designers).
narrative_ontology:constraint_vindicates(deferential_realism_ontology__rhetorical_scaffold_reading, classification_reflects_normative_judgment).
narrative_ontology:constraint_vindicates(deferential_realism_ontology__rhetorical_scaffold_reading, beneficiary_legitimacy_is_constructed_not_measured).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Develop and deploy the typology as a framework for critiquing institutional arrangements. They define 'snare' by appeal to normative standards of legitimacy rather than by measurement alone. They control the labeling apparatus and argue that the framework's power lies in its ability to expose mechanisms that serve illegitimate beneficiaries—beneficiaries identified through normative judgment about what constituencies ought to be served.
narrative_ontology:constraint_stakeholder(deferential_realism_ontology__rhetorical_scaffold_reading, normative_theorists, agenda_setter,
    analytical, generational, analytical, universal).

% Use the typology to delegitimize mechanisms they oppose on normative grounds. They benefit from the vocabulary that allows them to say 'this is a snare, not a rope' as an act of normative judgment. The typology provides a framework for their advocacy that converts normative disagreement into apparent structural diagnosis. They are not trapped by the framework and retain exit via alternative vocabularies (rights-based, efficiency-based, democratic legitimacy framings).
narrative_ontology:constraint_stakeholder(deferential_realism_ontology__rhetorical_scaffold_reading, critical_policy_advocates, beneficiary,
    organized, biographical, mobile, global).

% Operate under the belief that classification should be grounded in observable structural properties (extraction measurable, suppression observable, etc.) rather than in normative judgments about beneficiary legitimacy. They bear the cost of the rhetorical scaffold reading: their claim that 'snare' is discoverable through measurement is displaced by the frame that it is declared through advocacy. They are excluded from the agenda-setting function because the scaffold reading systematically deprioritizes measurement-grounded diagnosis.
narrative_ontology:constraint_stakeholder(deferential_realism_ontology__rhetorical_scaffold_reading, measurement_grounded_theorists, payer,
    analytical, generational, analytical, universal).
narrative_ontology:stakeholder_secondary_role(deferential_realism_ontology__rhetorical_scaffold_reading, measurement_grounded_theorists, excluded).

% Attempt to use the typology as a diagnostic tool to assess whether arrangements are genuinely coordinating or extractive. Under the rhetorical scaffold reading, they find the typology less useful for institutional design (since 'snare' is declared, not measured) and face higher uncertainty about whether a mechanism will be labeled snare or rope depending on the normative commitments of the analyzing advocate. They can exit by using measurement-grounded frameworks.
narrative_ontology:constraint_stakeholder(deferential_realism_ontology__rhetorical_scaffold_reading, institutional_designers, payer,
    powerful, biographical, mobile, national).

% Constituencies whose status as 'legitimate beneficiaries' is the object of contest (workers, marginalized groups, future generations, non-human agents). Under the rhetorical scaffold reading, their interests enter the framework only through the advocate's normative judgment about what counts as legitimate. They are excluded from the apparatus that decides whether they are served or exploited by a mechanism.
narrative_ontology:constraint_stakeholder(deferential_realism_ontology__rhetorical_scaffold_reading, beneficiary_identification_contests, excluded,
    organized, biographical, constrained, global).

% Receive competing typological claims: is this mechanism a rope (coordinate diagnosis) or a snare (critical diagnosis)? Under the rhetorical scaffold reading, they recognize that classification depends on which normative vocabulary is operative, not on measurement alone. They face ambiguity about which typological rendering to credit and must make independent normative judgments about legitimacy rather than relying on the framework to settle the question.
narrative_ontology:constraint_stakeholder(deferential_realism_ontology__rhetorical_scaffold_reading, policy_makers, observer,
    institutional, biographical, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(deferential_realism_ontology__rhetorical_scaffold_reading, critical_policy_advocates).
narrative_ontology:fixing_cost_class(deferential_realism_ontology__rhetorical_scaffold_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a unifying vocabulary for critiquing institutions: allows advocates to convert disagreements about what institutions ought to serve into apparent structural diagnoses (mountain/rope/snare vocabulary). Creates a shared language for policy discourse.
% TRANSFER_FUNCTION: Moves discursive authority and agenda-setting power from measurement specialists (who might resist normative characterization) to normative theorists (who frame classification as an inherently evaluative act). Transfers the burden of legitimacy from institutional actors to advocates who deploy the typology to expose 'true' beneficiaries.
% ABSENT_VOICES: Measurement-grounded theorists, who would argue that classification must be anchored in observable structural properties. Beneficiary constituencies whose interests are defined by the advocate rather than articulated directly. Institutional designers who want the typology to function as a diagnostic tool independent of advocacy.
% DISAPPEARANCE_RATIONALE: Critical advocates would argue the typology is essential to their ability to name illegitimate extraction; measurement-grounded theorists would argue the field reverts to stronger empirical grounding without the rhetorical overlay; policy makers would lose a shared vocabulary but gain clarity that classification depends on prior normative commitments, not on measurement.
% FOUNDING_PROBLEM: Institutions use legitimacy cover stories (e.g., 'this serves coordination') to mask extractive mechanisms that serve narrow beneficiaries. Normative theorists need a vocabulary to expose such mechanisms and reframe them as what they 'truly' are: snares disguised as ropes.
% FOUNDING_PROBLEM_CORROBORATION: Critical theorists and policy advocates attest that institutions deploy coordination language as cover for extraction—documented in case studies of regulatory capture, platform governance, and labor systems. Measurement-grounded theorists and some institutional designers attest that exposure works through structural measurement, not through normative redescription, and that the rhetorical scaffold reading conflates the two. No neutral external corroboration exists; corroboration comes from within the advocate position itself.
narrative_ontology:disappearance_verdict(deferential_realism_ontology__rhetorical_scaffold_reading, contested).
narrative_ontology:founding_problem_status(deferential_realism_ontology__rhetorical_scaffold_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(deferential_realism_ontology__rhetorical_scaffold_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(deferential_realism_ontology__rhetorical_scaffold_reading, 'none', 1).
narrative_ontology:epsilon_provenance(deferential_realism_ontology__rhetorical_scaffold_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(deferential_realism_ontology__rhetorical_scaffold_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(deferential_realism_ontology__rhetorical_scaffold_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(deferential_realism_ontology__rhetorical_scaffold_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is elevated (0.68) because the rhetorical scaffold reading transfers agenda-setting power over classification from measurement specialists to normative advocates. The mechanism extracts discursive authority by converting normative disagreement into apparent structural diagnosis. Suppression is LOW (0.31) because the reading explicitly does not suppress alternative framings—it acknowledges and embraces the normative character of classification. This is structurally unlike a snare, which hides its extraction under a coordination cover story. The scaffold reading is transparent about advocacy. Theater ratio is moderate (0.42) because the typology does perform real diagnostic work (identifying structural properties like extraction and suppression) even as it wraps that work in normative judgment. The measurement series show rising extractiveness early (t0-t20), plateauing by t30—the initial expansion of the typology's influence over policy discourse, stabilizing as competing frameworks establish counter-vocabularies. Low suppression reflects the reading's theoretical openness; the resistance is high (0.72, not shown in measurements but evident from the excluded voices and competing readings) because measurement-grounded theorists and pragmatists vigorously contest the rhetorical framing.
 *
 * PERSPECTIVAL GAP:
 *   From the normative theorist seat, the typology is an essential tool for exposing how institutions disguise extraction as coordination. From the measurement-grounded theorist seat, the same apparatus conflates structural diagnosis with advocacy and adds a normative overlay that obscures rather than clarifies what a mechanism actually does. From the institutional designer seat, the ambiguity about which normative framework applies makes the typology less useful as a diagnostic tool than a straightforward measurement apparatus would be. The scaffold reading is transparent about these gaps—it does not try to hide the normative-judgment component. Divergence in stakeholder experience should be computed by the engine from the structural data (beneficiaries at high power/analytical exit vs. payers at analytical power/constrained exit vs. excluded constituencies), not resolved by the framework.
 *
 * DIRECTIONALITY LOGIC:
 *   Normative theorists and critical advocates are beneficiaries (they gain discursive authority and a vocabulary for their advocacy; directionality near 0.0). Measurement-grounded theorists and institutional designers bear costs (their measurement-based framings are displaced; their exit is mobile but requires constructing alternative vocabularies; directionality near 1.0). Policy makers and excluded constituencies sit higher on the extraction axis (they must navigate ambiguity; their exit is constrained by institutional role; directionality 0.6–0.8). The beneficiaries are few, powerful, and analytical; the payers are scattered across different institutional positions. This asymmetry drives suppression requirement: advocates need not suppress measurement-based language (it remains available), but institutional inertia favors the normative vocabulary once it is established in policy discourse. Low measured suppression reflects theoretical openness; high resistance reflects practical contestation.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (institutions use legitimacy cover stories to mask extraction) remains contested: measurement-grounded theorists argue the problem is solved through better structural diagnosis, not rhetorical redescription. The scaffold reading's assertion that 'snare' is declared, not discovered, avoids mandatrophy (the reading does not claim to have solved the measurement problem—it reframes it as a normative-judgment problem). However, the reading carries a risk of functional atrophy: if critical advocates rely on normative judgment rather than structural measurement, the apparatus loses diagnostic power in contexts where consensus about legitimacy is unavailable. The six-questions verdict (contested disappearance) reflects this: some institutional actors would lose the vocabulary; others would gain clarity that classification depends on prior commitments.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    beneficiary_legitimacy_construction,
    'Is the determination of who counts as a ''legitimate beneficiary'' a normative judgment made by the analyst, or is it a property of the institution that can be measured independently?',
    'Comparative analysis: do different normative frameworks (rights-based, efficiency-based, democratic legitimacy, stakeholder theory) consistently identify the same beneficiaries and victims for the same mechanism, or do identifications diverge by framework?',
    'If identifications diverge by framework, the rhetorical scaffold reading is correct: ''snare'' is declared through normative judgment, not discovered. If identifications converge across frameworks, the determination has structural content independent of advocacy, supporting the immutable diagnostic reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_legitimacy_construction, conceptual, 'Whether beneficiary legitimacy is constructed by the analyst or measured from the institution.').

omega_variable(
    measurement_versus_advocacy_boundary,
    'Can structural properties (extraction, suppression, accessibility_collapse) be measured independently of the normative question of whether the mechanism ought to serve its identified beneficiaries?',
    'Empirical audit: measure the constraint''s structural properties under the rhetorical scaffold reading (building in normative judgment about legitimacy) and under the immutable diagnostic reading (anchoring only in observable structure). Do the measurements of the same constraint diverge?',
    'If measurements converge, structure and advocacy are separable and the typology has diagnostic content independent of the rhetoric. If measurements diverge by framework, extraction and legitimacy are entangled, supporting the scaffold reading''s core claim.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(measurement_versus_advocacy_boundary, empirical, 'Whether structural measurement and normative judgment about beneficiary legitimacy are separable operations.').

omega_variable(
    excluded_voice_asymmetry,
    'Does the rhetorical scaffold reading systematically exclude the voices of contested beneficiary constituencies from the determination of whether they are served or exploited by a mechanism?',
    'Procedural audit: compare the constraint''s analysis in cases where contested constituencies are included in the normative-judgment process (defining legitimacy) versus excluded. Do inclusion and exclusion produce different classifications?',
    'If inclusion/exclusion produce different classifications, the scaffold reading carries a structural asymmetry: it empowers the advocate''s normative judgment over the constituency''s self-assessment. If classifications hold regardless, the normative judgment is more robust than the asymmetry claim suggests.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(excluded_voice_asymmetry, empirical, 'Whether the scaffold reading''s normative decision-making systematically excludes affected constituencies.').

omega_variable(
    kernel_reading_committer_structure,
    'Is this constraint genuinely ONE reading of a contested kernel, or is it a meta-claim about the typology itself that should not be cast as a constraint story?',
    'Structural analysis: the kernel deferential_realism_ontology is the typology itself. This reading instantiates the claim that the typology functions as a normative vocabulary for advocacy, not as a measurement apparatus. The sibling readings instantiate claims about fixed diagnostic content and hybrid pragmatism. The committer structure (normative judgment, advocacy function, low suppression of alternatives) is the substantive content of THIS reading''s instantiation of the kernel.',
    'Treating the reading as a constraint story captures its structural role in discourse: it makes claims about beneficiaries (advocates, theorists), victims (measurement-grounded theorists), extraction (conversion of normative disagreement into structural diagnosis), and persistence (the typology remains scaffolding for advocacy because alternatives must construct their own vocabularies). This framing routes the committer structure through the standard constraint apparatus rather than reserving it to meta-theoretical commentary.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_committer_structure, conceptual, 'Whether the rhetorical scaffold reading should be modeled as a constraint story with its own structural properties.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(deferential_realism_ontology__rhetorical_scaffold_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(defe_tr_t0, deferential_realism_ontology__rhetorical_scaffold_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement_basis(defe_tr_t0, observed).
narrative_ontology:measurement(defe_tr_t5, deferential_realism_ontology__rhetorical_scaffold_reading, theater_ratio, 5, 0.28).
narrative_ontology:measurement_basis(defe_tr_t5, observed).
narrative_ontology:measurement(defe_tr_t10, deferential_realism_ontology__rhetorical_scaffold_reading, theater_ratio, 10, 0.32).
narrative_ontology:measurement_basis(defe_tr_t10, observed).
narrative_ontology:measurement(defe_tr_t15, deferential_realism_ontology__rhetorical_scaffold_reading, theater_ratio, 15, 0.37).
narrative_ontology:measurement_basis(defe_tr_t15, observed).
narrative_ontology:measurement(defe_tr_t20, deferential_realism_ontology__rhetorical_scaffold_reading, theater_ratio, 20, 0.4).
narrative_ontology:measurement_basis(defe_tr_t20, observed).
narrative_ontology:measurement(defe_tr_t25, deferential_realism_ontology__rhetorical_scaffold_reading, theater_ratio, 25, 0.41).
narrative_ontology:measurement_basis(defe_tr_t25, observed).
narrative_ontology:measurement(defe_tr_t30, deferential_realism_ontology__rhetorical_scaffold_reading, theater_ratio, 30, 0.42).
narrative_ontology:measurement_basis(defe_tr_t30, observed).
narrative_ontology:measurement(defe_tr_t40, deferential_realism_ontology__rhetorical_scaffold_reading, theater_ratio, 40, 0.42).
narrative_ontology:measurement_basis(defe_tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(defe_be_t0, deferential_realism_ontology__rhetorical_scaffold_reading, base_extractiveness, 0, 0.52).
narrative_ontology:measurement_basis(defe_be_t0, observed).
narrative_ontology:measurement(defe_be_t5, deferential_realism_ontology__rhetorical_scaffold_reading, base_extractiveness, 5, 0.56).
narrative_ontology:measurement_basis(defe_be_t5, observed).
narrative_ontology:measurement(defe_be_t10, deferential_realism_ontology__rhetorical_scaffold_reading, base_extractiveness, 10, 0.61).
narrative_ontology:measurement_basis(defe_be_t10, observed).
narrative_ontology:measurement(defe_be_t15, deferential_realism_ontology__rhetorical_scaffold_reading, base_extractiveness, 15, 0.64).
narrative_ontology:measurement_basis(defe_be_t15, observed).
narrative_ontology:measurement(defe_be_t20, deferential_realism_ontology__rhetorical_scaffold_reading, base_extractiveness, 20, 0.66).
narrative_ontology:measurement_basis(defe_be_t20, observed).
narrative_ontology:measurement(defe_be_t25, deferential_realism_ontology__rhetorical_scaffold_reading, base_extractiveness, 25, 0.67).
narrative_ontology:measurement_basis(defe_be_t25, observed).
narrative_ontology:measurement(defe_be_t30, deferential_realism_ontology__rhetorical_scaffold_reading, base_extractiveness, 30, 0.68).
narrative_ontology:measurement_basis(defe_be_t30, observed).
narrative_ontology:measurement(defe_be_t40, deferential_realism_ontology__rhetorical_scaffold_reading, base_extractiveness, 40, 0.68).
narrative_ontology:measurement_basis(defe_be_t40, observed).

% Suppression requirement over time
narrative_ontology:measurement(defe_su_t0, deferential_realism_ontology__rhetorical_scaffold_reading, suppression_requirement, 0, 0.18).
narrative_ontology:measurement_basis(defe_su_t0, observed).
narrative_ontology:measurement(defe_su_t5, deferential_realism_ontology__rhetorical_scaffold_reading, suppression_requirement, 5, 0.2).
narrative_ontology:measurement_basis(defe_su_t5, observed).
narrative_ontology:measurement(defe_su_t10, deferential_realism_ontology__rhetorical_scaffold_reading, suppression_requirement, 10, 0.23).
narrative_ontology:measurement_basis(defe_su_t10, observed).
narrative_ontology:measurement(defe_su_t15, deferential_realism_ontology__rhetorical_scaffold_reading, suppression_requirement, 15, 0.25).
narrative_ontology:measurement_basis(defe_su_t15, observed).
narrative_ontology:measurement(defe_su_t20, deferential_realism_ontology__rhetorical_scaffold_reading, suppression_requirement, 20, 0.27).
narrative_ontology:measurement_basis(defe_su_t20, observed).
narrative_ontology:measurement(defe_su_t25, deferential_realism_ontology__rhetorical_scaffold_reading, suppression_requirement, 25, 0.29).
narrative_ontology:measurement_basis(defe_su_t25, observed).
narrative_ontology:measurement(defe_su_t30, deferential_realism_ontology__rhetorical_scaffold_reading, suppression_requirement, 30, 0.31).
narrative_ontology:measurement_basis(defe_su_t30, observed).
narrative_ontology:measurement(defe_su_t40, deferential_realism_ontology__rhetorical_scaffold_reading, suppression_requirement, 40, 0.31).
narrative_ontology:measurement_basis(defe_su_t40, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(deferential_realism_ontology__rhetorical_scaffold_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(deferential_realism_ontology__rhetorical_scaffold_reading, 0.12).
narrative_ontology:affects_constraint(deferential_realism_ontology__rhetorical_scaffold_reading, deferential_realism_ontology__immutable_diagnostic_reading).
narrative_ontology:affects_constraint(deferential_realism_ontology__rhetorical_scaffold_reading, deferential_realism_ontology__hybrid_pragmatic_reading).

% DUAL FORMULATION NOTE:
% The constraint typology (kernel deferential_realism_ontology) decomposes into three reading constraints with structurally distinct epsilon values and beneficiary structures. The rhetorical scaffold reading asserts that classification is advocacy-driven (high extractiveness, low suppression); the immutable diagnostic reading asserts classification is measurement-grounded (lower extractiveness in the typology itself, high accessibility of alternatives). The hybrid pragmatic reading sits between. The three constraints are not variations on one fact but competing instantiations of one kernel. All three network.affects_constraints must include the other two to preserve the kernel structure.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(deferential_realism_ontology__rhetorical_scaffold_reading, analytical, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
