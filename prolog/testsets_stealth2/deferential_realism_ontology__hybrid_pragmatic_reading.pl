% ============================================================================
% CONSTRAINT STORY: deferential_realism_ontology__hybrid_pragmatic_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_deferential_realism_ontology__hybrid_pragmatic_reading, []).

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
 *   constraint_id: deferential_realism_ontology__hybrid_pragmatic_reading
 *   human_readable: Constraint Typology Classification Discipline (Hybrid Pragmatic Reading)
 *   domain: epistemological/institutional
 *
 * SUMMARY:
 *   The standing arrangement under analysis is the operating discipline of a
 *   constraint-typology classification framework: a formal schema, a
 *   compiling pipeline, and an engine that converts authored structural data
 *   into per-seat constraint classifications. As instantiated by this
 *   reading, the arrangement has a two-tier epistemic structure. Its core
 *   tier — classifications of physical invariants and solved coordination
 *   problems — is stable across interpretive communities and functions as a
 *   genuine shared standard. Its periphery tier — classifications that turn
 *   on judgments about which beneficiaries of an arrangement count as
 *   legitimate — is constructed rather than observed, yet issues through the
 *   same computed-verdict surface as the core. The arrangement therefore
 *   coordinates classification practice while transferring credibility from
 *   classified arrangements to the classifier complex: observational
 *   authority earned at the core is spent at the periphery, where verdicts
 *   resting on normative commitments present themselves as engine outputs.
 *   This story instantiates one reading of a contested kernel; the
 *   reading-level contest is recorded in commentary.kernel_context and the
 *   omega variables, not averaged into the metrics.
 *
 * KEY AGENTS:
 *   - dr_framework_maintainers: Agenda-setter (institutional/identity_locked) — owns the schema, engine configuration, and enforcement gates; collects the authority the framework's operation generates
 *   - constraint_analysts: Primary beneficiary (organized/mobile) — deploys the typology's verdicts in critique, borrowing the core's observational authority for periphery claims
 *   - contested_periphery_subjects: Primary target (powerful/constrained) — institutions and arrangements whose classifications turn on disputed legitimacy judgments; they bear the reputational and policy costs
 *   - core_domain_practitioners: Secondary beneficiary (moderate/mobile) — researchers in physically grounded domains whose classifications are observational and stable; they supply the credibility the periphery borrows
 *   - affected_publics: Excluded voice (powerless/trapped) — people living under classified arrangements; their experience supplies the framework's victim data but they hold no seat in setting legitimacy defaults
 *   - policy_adjudicators: Analytical observer (institutional/analytical) — courts, regulators, and legislative bodies that consume classifications for decisions without participating in the framework's governance
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(deferential_realism_ontology__hybrid_pragmatic_reading, 0.52).
domain_priors:suppression_score(deferential_realism_ontology__hybrid_pragmatic_reading, 0.55).
domain_priors:theater_ratio(deferential_realism_ontology__hybrid_pragmatic_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(deferential_realism_ontology__hybrid_pragmatic_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(deferential_realism_ontology__hybrid_pragmatic_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(deferential_realism_ontology__hybrid_pragmatic_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(deferential_realism_ontology__hybrid_pragmatic_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(deferential_realism_ontology__hybrid_pragmatic_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(deferential_realism_ontology__hybrid_pragmatic_reading, tangled_rope).
narrative_ontology:human_readable(deferential_realism_ontology__hybrid_pragmatic_reading, "Constraint Typology Classification Discipline (Hybrid Pragmatic Reading)").
narrative_ontology:topic_domain(deferential_realism_ontology__hybrid_pragmatic_reading, "epistemological/institutional").

domain_priors:requires_active_enforcement(deferential_realism_ontology__hybrid_pragmatic_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(deferential_realism_ontology__hybrid_pragmatic_reading, '48118685-a468-455b-aba1-d23ca8eb23a1').
narrative_ontology:cs_kernel_codification('48118685-a468-455b-aba1-d23ca8eb23a1', formalized).
narrative_ontology:cs_authority_grounding('48118685-a468-455b-aba1-d23ca8eb23a1', expertise).
narrative_ontology:cs_interpretation_layer_present('48118685-a468-455b-aba1-d23ca8eb23a1').
narrative_ontology:cs_reading_relation('48118685-a468-455b-aba1-d23ca8eb23a1', deferential_realism_ontology__immutable_diagnostic_reading, coexists_with).
narrative_ontology:cs_reading_relation('48118685-a468-455b-aba1-d23ca8eb23a1', deferential_realism_ontology__rhetorical_scaffold_reading, coexists_with).
narrative_ontology:cs_axiom('48118685-a468-455b-aba1-d23ca8eb23a1', foundational, periphery_classification_constitutively_norm_dependent).
narrative_ontology:cs_axiom_status(periphery_classification_constitutively_norm_dependent, holdable).
narrative_ontology:cs_axiom_grounding('48118685-a468-455b-aba1-d23ca8eb23a1', periphery_classification_constitutively_norm_dependent, empirically_contingent).
narrative_ontology:cs_axiom('48118685-a468-455b-aba1-d23ca8eb23a1', foundational, observational_core_norm_invariant).
narrative_ontology:cs_axiom_status(observational_core_norm_invariant, holdable).
narrative_ontology:cs_axiom_grounding('48118685-a468-455b-aba1-d23ca8eb23a1', observational_core_norm_invariant, empirically_contingent).
narrative_ontology:cs_reference_frame('48118685-a468-455b-aba1-d23ca8eb23a1', two_tier_epistemic_typology).
narrative_ontology:cs_drift_state('48118685-a468-455b-aba1-d23ca8eb23a1', contemporary_corpus_era, gap(practice_drift, minor, false)).
narrative_ontology:cs_created_at('48118685-a468-455b-aba1-d23ca8eb23a1', '').
narrative_ontology:cs_kernel_id(deferential_realism_ontology__hybrid_pragmatic_reading, deferential_realism_ontology).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(deferential_realism_ontology__hybrid_pragmatic_reading, dr_framework_maintainers).
narrative_ontology:constraint_beneficiary(deferential_realism_ontology__hybrid_pragmatic_reading, constraint_analysts).
narrative_ontology:constraint_beneficiary(deferential_realism_ontology__hybrid_pragmatic_reading, core_domain_practitioners).
narrative_ontology:constraint_victim(deferential_realism_ontology__hybrid_pragmatic_reading, contested_periphery_subjects).
narrative_ontology:constraint_vindicates(deferential_realism_ontology__hybrid_pragmatic_reading, fixed_core_contested_periphery_thesis).
narrative_ontology:constraint_vindicates(deferential_realism_ontology__hybrid_pragmatic_reading, beneficiary_legitimacy_constructivism).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintain the classification framework's schema, compilation pipeline, and engine configuration: they set validation gates, metric floors, and override targets, and adjudicate which structural data a story must contain before it compiles. Their professional standing, publication record, and career continuity are constituted by the framework's operation — the numbered-question process and the audit archive are their working history. Leaving the framework would mean abandoning the body of work that defines their expertise.
narrative_ontology:constraint_stakeholder(deferential_realism_ontology__hybrid_pragmatic_reading, dr_framework_maintainers, agenda_setter,
    institutional, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(deferential_realism_ontology__hybrid_pragmatic_reading, dr_framework_maintainers, beneficiary).

% Use the framework's vocabulary and verdicts in scholarly and policy critique. They receive ready-made classifications that would otherwise require extended argument, and the framework's computed-output presentation lends their claims an authority they did not have to earn case by case. They can stop using the framework and argue in ordinary normative terms at any time; nothing binds them to it.
narrative_ontology:constraint_stakeholder(deferential_realism_ontology__hybrid_pragmatic_reading, constraint_analysts, beneficiary,
    organized, biographical, mobile, global).

% Institutions and organized arrangements whose operations the framework classifies by way of judgments about which of their beneficiaries count as legitimate — judgments the framework treats as constructed rather than observed. They bear the reputational and policy consequences of adverse verdicts, and their public objections are recorded by the framework as data points rather than answered as arguments. They cannot exit being classified; their recourse is contestation within or against the framework's terms.
narrative_ontology:constraint_stakeholder(deferential_realism_ontology__hybrid_pragmatic_reading, contested_periphery_subjects, payer,
    powerful, generational, constrained, global).

% Researchers in domains where the framework's classifications track physical invariants and solved coordination problems. They receive accurate, stable classifications at negligible cost and rarely engage with the framework's contested tier. Their fields' demonstrated success is what the framework's broader authority rests on, though they neither administer it nor police it.
narrative_ontology:constraint_stakeholder(deferential_realism_ontology__hybrid_pragmatic_reading, core_domain_practitioners, beneficiary,
    moderate, civilizational, mobile, global).

% People who live under arrangements the framework classifies. Their experienced costs and benefits enter the framework as the victim and beneficiary data that verdicts are computed from, but they are consulted by no one: the judgment about which beneficiaries of the arrangements governing them count as legitimate is made elsewhere, and its output is issued about them.
narrative_ontology:constraint_stakeholder(deferential_realism_ontology__hybrid_pragmatic_reading, affected_publics, excluded,
    powerless, generational, trapped, national).

% Courts, regulators, and legislative staff who encounter the framework's verdicts as inputs to decisions about the arrangements classified. They take the classifications' computed presentation at varying degrees of face value, commission their own analyses in harder cases, and have no role in the framework's governance or in setting its legitimacy defaults.
narrative_ontology:constraint_stakeholder(deferential_realism_ontology__hybrid_pragmatic_reading, policy_adjudicators, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(deferential_realism_ontology__hybrid_pragmatic_reading, dr_framework_maintainers).
narrative_ontology:fixing_cost_class(deferential_realism_ontology__hybrid_pragmatic_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single commensurable vocabulary and pipeline for classifying social arrangements, so that dispersed analysts produce comparable verdicts, accumulate a shared corpus, and can distinguish verdicts that replicate across interpretive communities from verdicts that do not.
% TRANSFER_FUNCTION: Moves epistemic authority and policy leverage from classified arrangements to the classifier complex: every periphery verdict converts a contestable judgment about legitimate beneficiaries into an apparently computed output, transferring status to the maintainers who run the pipeline and the analysts who cite it, at reputational cost to the classified.
% ABSENT_VOICES: Affected publics living under classified arrangements have no seat in setting which beneficiaries count as legitimate, though their experience supplies the framework's victim data; democratic institutions consume the verdicts without having adopted the legitimacy defaults they encode; and methodologists who reject the framework's premises argue entirely outside its fail-closed boundaries, where their objections register only as undifferentiated resistance.
% DISAPPEARANCE_RATIONALE: If the classification discipline vanished overnight, the accumulated corpus, audit trail, and cross-analyst comparability would lose their substrate; analysts would fragment back into incompatible vocabularies; classified institutions would lose the structured venue in which they contest verdicts; and the maintainer community's career structure — built on the numbered-question process and the audit archive — would dissolve. The world would rearrange around whatever replacement vocabulary emerged, slowly and contentiously.
% FOUNDING_PROBLEM: Normative and political argument about social arrangements kept collapsing into mutual ideology charges: claims that an arrangement was physically or logically unavoidable were indistinguishable, in practice, from claims that it served someone at others' expense, and no shared discipline existed for separating the observational from the contested.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: adjacent disciplines independently developed parallel distinctions under their own pressures — positive versus normative economics, descriptive versus evaluative ethics, descriptive versus prescriptive strands in law-and-society scholarship — which attests the underlying problem is real and predates this framework. Internal attestation from maintainers alone would be cover-story risk; the live-status claim rests on that external parallel development, though the external sources attest the problem, not that this framework solves it.
narrative_ontology:disappearance_verdict(deferential_realism_ontology__hybrid_pragmatic_reading, world_rearranges).
narrative_ontology:founding_problem_status(deferential_realism_ontology__hybrid_pragmatic_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(deferential_realism_ontology__hybrid_pragmatic_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(deferential_realism_ontology__hybrid_pragmatic_reading, 'none', 1).
narrative_ontology:epsilon_provenance(deferential_realism_ontology__hybrid_pragmatic_reading, 0.52, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(deferential_realism_ontology__hybrid_pragmatic_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(deferential_realism_ontology__hybrid_pragmatic_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(deferential_realism_ontology__hybrid_pragmatic_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The claimed type is authored from structure, not from the metrics: the arrangement genuinely coordinates (a commensurable vocabulary and a real observational core) and asymmetrically extracts (constructed periphery verdicts issued through a computed surface), and it is held together by active enforcement (fail-closed compilation, coverage rules, fabrication bans), which is the tangled_rope signature. The metrics are authored descriptively against this reading's own referent — the standing classification discipline, not any alternative arrangement the reading would prefer. Extractiveness is 0.52: near-zero at the core tier, substantially higher at the periphery tier where legitimacy judgments convert into verdicts, blended over the framework's actual case mix. Suppression is 0.55, matching the expected medium delta: the enforcement is epistemic rather than coercive — fail-closed validation, structural gates, and an internalized authoring discipline that leads contributors to pre-comply. Theater_ratio is 0.38: the measurement and provenance apparatus is functional for the core, but a growing share of ritual activity (alignment grids, provenance trails, audit ceremony) displays rigor around periphery verdicts that rest on constructed judgments. Accessibility_collapse is 0.40 because alternatives survive — plain normative argument, rival vocabularies, and ad hoc analysis remain usable, so mastering the framework does not collapse the option space. Resistance is 0.60: classified subjects contest verdicts and methodological dissent recurs. The three measurement series share one time grid (points 0 through 24); all three trend monotonically upward as the corpus shifts toward periphery cases and enforcement hardens, so no cyclical pattern is authored — periodic methodology controversies spike and subside without reversing the baseline drift.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute differently. From the maintainer seat the arrangement is a rigorously engineered coordination device — the fail-closed machinery reads as integrity, and the periphery's constructed element is hidden behind structural gates. From the analyst seat it is cheap persuasive infrastructure: verdicts that would otherwise require extended argument arrive pre-computed. From the periphery-subject seat the same machinery reads as enforced construction — a normative judgment about legitimate beneficiaries wearing the costume of measurement, defended by validation rules that make dissent expensive. From the core-practitioner seat it is simply a working instrument. The engine computes this divergence from the structural data; the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Framework maintainers sit nearest the beneficiary pole: they set the gates, own the configuration, and collect the authority the operation generates (d near 0.1). Constraint analysts also sit beneficiary-side (d near 0.15): they receive persuasive leverage at low cost, and their mobile exit keeps them from subsidizing the arrangement. Contested periphery subjects sit nearest the target pole (d near 0.85): they pay in reputation and policy leverage, their objections are absorbed as resistance data, and exit from being classified is effectively unavailable. Core domain practitioners are the one seat the role derivation would misread: listed as beneficiaries, their actual position is near-symmetric — they receive accurate classifications at negligible cost while supplying the credibility the periphery borrows — so a directionality override raises their derived d from the beneficiary default to 0.35, keyed to their moderate power atom, the only moderate-power seat in the story. Affected publics are excluded rather than seated; the extraction touching them runs through the victim data they supply, not through a role the derivation can price. Gains demonstrably accrue to the maintainer seat, which is why gain_flow names it rather than the diffuse catch-all.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — social critique collapsing into alternating ideology charges for lack of a disciplined distinction between the unavoidable and the extracted-from — is still live, so no mandatrophy is declared. The drift risk runs the other direction: as the corpus fills with periphery cases, verdict generation could become the framework's primary product while the observational core atrophies into citation decoration; at that point theater_ratio would climb past functional maintenance and the arrangement would decay piton-ward. The current series shows the core still doing real work, and the mismatch consumer should read founding_problem_status=live against disappearance_verdict=world_rearranges as the no-zombie configuration. On the receipt surface: fixing the arrangement would require splitting the computed-verdict surface into separately labeled observational and declared tracks — dissolving the authority-borrowing mechanism that constitutes the framework's value to the seat that could perform the fix — so the cost to fix is prohibitive relative to the benefit its operators perceive.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_commitment,
    'This constraint is the hybrid_pragmatic_reading of kernel deferential_realism_ontology; what structural changes would instantiating a sibling reading produce?',
    'Regenerate the story under a sibling reading_id: the immutable_diagnostic_reading would re-author epsilon as uniformly observational (every misclassification a correctable error, extraction falling toward the coordination floor at every tier); the rhetorical_scaffold_reading would re-author epsilon as declared rather than discovered (extraction rising, the enforcement machinery re-reading as persuasion infrastructure).',
    'Reading choice moves epsilon across a wide band and can flip the periphery tier between measured extraction and open declaration; core-tier classifications are the only component stable across all three readings.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_commitment, conceptual, 'Committer structure: one kernel, three readings, epsilon indexed to the reading.').

omega_variable(
    legitimate_beneficiary_criterion,
    'Is there a principled, articulable criterion for which beneficiaries of an arrangement count as legitimate at the periphery tier, or does the judgment reduce to the analyst''s prior political commitments?',
    'Blinded cross-community classification trials: give interpretive communities identical structural files with beneficiary identities masked and systematically varied, and measure verdict convergence; convergence under masking indicates a shared criterion, divergence indicates prior-commitment dominance.',
    'If a criterion exists, periphery extraction is partly measurable and the 0.52 blend is stable; if not, periphery epsilon is fully constructed and the computed-verdict surface mislabels declaration as measurement, pushing effective extraction above the authored value.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legitimate_beneficiary_criterion, conceptual, 'Whether peripheral legitimacy judgments track a criterion or analyst priors.').

omega_variable(
    authority_borrowing_direction,
    'Does credibility flow from the observational core to the contested periphery (borrowing), or does periphery controversy retroactively contaminate trust in core verdicts?',
    'Longitudinal tracking of cross-community agreement on core-tier verdicts as periphery controversies accumulate; erosion of core agreement following periphery scandals would reverse the borrowing direction.',
    'If borrowing holds, the arrangement is stable in its hybrid form; if contamination dominates, the coordination leg rots and the arrangement drifts toward pure extraction carried by a decorative core.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(authority_borrowing_direction, empirical, 'Direction of credibility transfer between the typology''s tiers.').

omega_variable(
    self_application_stability,
    'Is this story''s epsilon stable under self-application — does running the framework''s own apparatus on this classification of the framework yield the claimed type, or does the periphery-construction thesis apply to the framework''s own verdicts and destabilize the result?',
    'Compute the per-seat classifications for this story and inspect them against the authored claim; divergence between the computed type and the claim is evidence that the construction thesis reaches the framework''s self-verdicts.',
    'Convergence supports the reading''s coherence; divergence supports the thesis at the cost of destabilizing the corpus''s meta-classifications and warrants review of every periphery verdict issued about the framework itself.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(self_application_stability, conceptual, 'Whether the framework''s classification of itself is a fixed point.').

omega_variable(
    epistemic_suppression_mechanism,
    'Is the measured suppression structural (fail-closed compilation, coverage rules, and fabrication bans that reject nonconforming stories) or internalized (contributors who pre-comply because the authoring discipline has fused with their professional honesty norms)?',
    'Post-exit trajectory: track contributors who leave the framework; if they retain the classification reflexes and continue treating unstructured normative argument as illegitimate after the enforcement machinery no longer binds them, the internalized share is substantial.',
    'If internalized, effective suppression exceeds the structural measure and persists beyond the apparatus itself; remediation would then require norm change within the contributing community, not gate removal.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(epistemic_suppression_mechanism, empirical, 'Structural versus internalized share of the framework''s suppressive force.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(deferential_realism_ontology__hybrid_pragmatic_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(defe_tr_t0, deferential_realism_ontology__hybrid_pragmatic_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement(defe_tr_t4, deferential_realism_ontology__hybrid_pragmatic_reading, theater_ratio, 4, 0.21).
narrative_ontology:measurement(defe_tr_t8, deferential_realism_ontology__hybrid_pragmatic_reading, theater_ratio, 8, 0.25).
narrative_ontology:measurement(defe_tr_t12, deferential_realism_ontology__hybrid_pragmatic_reading, theater_ratio, 12, 0.29).
narrative_ontology:measurement(defe_tr_t16, deferential_realism_ontology__hybrid_pragmatic_reading, theater_ratio, 16, 0.33).
narrative_ontology:measurement(defe_tr_t20, deferential_realism_ontology__hybrid_pragmatic_reading, theater_ratio, 20, 0.36).
narrative_ontology:measurement(defe_tr_t24, deferential_realism_ontology__hybrid_pragmatic_reading, theater_ratio, 24, 0.38).

% Extraction over time
narrative_ontology:measurement(defe_be_t0, deferential_realism_ontology__hybrid_pragmatic_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(defe_be_t4, deferential_realism_ontology__hybrid_pragmatic_reading, base_extractiveness, 4, 0.34).
narrative_ontology:measurement(defe_be_t8, deferential_realism_ontology__hybrid_pragmatic_reading, base_extractiveness, 8, 0.39).
narrative_ontology:measurement(defe_be_t12, deferential_realism_ontology__hybrid_pragmatic_reading, base_extractiveness, 12, 0.44).
narrative_ontology:measurement(defe_be_t16, deferential_realism_ontology__hybrid_pragmatic_reading, base_extractiveness, 16, 0.48).
narrative_ontology:measurement(defe_be_t20, deferential_realism_ontology__hybrid_pragmatic_reading, base_extractiveness, 20, 0.5).
narrative_ontology:measurement(defe_be_t24, deferential_realism_ontology__hybrid_pragmatic_reading, base_extractiveness, 24, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(defe_su_t0, deferential_realism_ontology__hybrid_pragmatic_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(defe_su_t4, deferential_realism_ontology__hybrid_pragmatic_reading, suppression_requirement, 4, 0.39).
narrative_ontology:measurement(defe_su_t8, deferential_realism_ontology__hybrid_pragmatic_reading, suppression_requirement, 8, 0.43).
narrative_ontology:measurement(defe_su_t12, deferential_realism_ontology__hybrid_pragmatic_reading, suppression_requirement, 12, 0.47).
narrative_ontology:measurement(defe_su_t16, deferential_realism_ontology__hybrid_pragmatic_reading, suppression_requirement, 16, 0.51).
narrative_ontology:measurement(defe_su_t20, deferential_realism_ontology__hybrid_pragmatic_reading, suppression_requirement, 20, 0.53).
narrative_ontology:measurement(defe_su_t24, deferential_realism_ontology__hybrid_pragmatic_reading, suppression_requirement, 24, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(deferential_realism_ontology__hybrid_pragmatic_reading, information_standard).
narrative_ontology:affects_constraint(deferential_realism_ontology__hybrid_pragmatic_reading, deferential_realism_ontology__immutable_diagnostic_reading).
narrative_ontology:affects_constraint(deferential_realism_ontology__hybrid_pragmatic_reading, deferential_realism_ontology__rhetorical_scaffold_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'the constraint typology' decomposes into three reading-stories of the deferential_realism_ontology kernel because the label conflates structurally distinct commitments with different epsilon values — an observational-instrument claim, a hybrid two-tier claim (this file), and a declaratory-vocabulary claim. Each member carries its own epsilon, beneficiaries, and victims; the members are linked here per the family rule. Upstream/downstream structure is deliberately weak: the readings compete rather than feed each other, so these edges record kinship, not causal priority.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(deferential_realism_ontology__hybrid_pragmatic_reading, moderate, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
