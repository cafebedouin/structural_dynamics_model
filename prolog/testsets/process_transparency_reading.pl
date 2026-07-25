% ============================================================================
% CONSTRAINT STORY: process_transparency_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_process_transparency_reading, []).

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
 *   constraint_id: process_transparency_reading
 *   human_readable: Process Transparency Reading — Coupled Readable-Output + Disclosed-Process Norm
 *   domain: epistemics_of_cooperation/signaling_theory/authorship_attribution
 *
 * SUMMARY:
 *   This story instantiates the process-transparency reading of the
 *   cooperative-artifact-legitimacy kernel: the dilemma between legible
 *   surface text and authorial-preserving raw draft is not resolved by
 *   choosing one, but dissolved by decoupling them into a smoothed artifact
 *   plus a parallel disclosed trace (revision histories, contribution
 *   statements, process logs). Legitimacy attaches to the coupled pair, not
 *   to either half alone. This is meant, structurally, as a scaffold move —
 *   it converts a standing tension into an auditable side-channel with the
 *   intent of eventually normalizing genuinely fair, low-friction disclosure
 *   practice. But it does not eliminate the underlying extraction risk; it
 *   relocates it. Whoever controls the disclosure schema and tooling becomes
 *   a new gatekeeper, and any context lacking that tooling — or any actor
 *   willing to fabricate the trace — inherits a new, harder-to-contest
 *   failure mode. The sibling readings (legibility_primacy_reading:
 *   legitimacy attaches to the smoothed surface alone;
 *   authorial_primacy_reading: legitimacy attaches to the raw, unsmoothed
 *   authored record alone) are NOT represented here; each is a separate
 *   constraint with its own ε, beneficiaries, and victims, linked via
 *   network.affects_constraints.
 *
 * KEY AGENTS:
 *   - disclosure_infrastructure_operators: primary agenda_setter and gain_flow target — designs and administers the trace schema
 *   - credited_primary_authors: beneficiary of both halves of the coupling, bears compliance cost
 *   - uncredited_contributors: payer — labor erased from surface, imperfectly captured by trace categories they didn't design
 *   - institutional_reviewers: beneficiary — offload adjudication burden onto trace existence rather than trace fidelity
 *   - contexts_without_disclosure_infrastructure: payer — presumptively delegitimized for lacking tooling access, independent of actual fairness
 *   - authors_subject_to_gamed_provenance_records: payer — harder to contest a falsified trace than a falsified direct claim
 *   - auditors_and_replication_researchers: analytical observer — surfaces theater in the trace itself
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(process_transparency_reading, 0.42).
domain_priors:suppression_score(process_transparency_reading, 0.31).
domain_priors:theater_ratio(process_transparency_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(process_transparency_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(process_transparency_reading, suppression_requirement, 0.31).
narrative_ontology:constraint_metric(process_transparency_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(process_transparency_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(process_transparency_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(process_transparency_reading, scaffold).
narrative_ontology:human_readable(process_transparency_reading, "Process Transparency Reading — Coupled Readable-Output + Disclosed-Process Norm").
narrative_ontology:topic_domain(process_transparency_reading, "epistemics_of_cooperation/signaling_theory/authorship_attribution").

domain_priors:requires_active_enforcement(process_transparency_reading).
narrative_ontology:has_sunset_clause(process_transparency_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(process_transparency_reading, '24191bbc-0e81-4992-a55d-fe59719db7a2').
narrative_ontology:cs_kernel_codification('24191bbc-0e81-4992-a55d-fe59719db7a2', distributed).
narrative_ontology:cs_authority_grounding('24191bbc-0e81-4992-a55d-fe59719db7a2', practice).
narrative_ontology:cs_interpretation_layer_present('24191bbc-0e81-4992-a55d-fe59719db7a2').
narrative_ontology:cs_reading_relation('24191bbc-0e81-4992-a55d-fe59719db7a2', cooperative_artifact_legitimacy__legibility_primacy_reading, influences).
narrative_ontology:cs_reading_relation('24191bbc-0e81-4992-a55d-fe59719db7a2', cooperative_artifact_legitimacy__authorial_primacy_reading, influences).
narrative_ontology:cs_axiom('24191bbc-0e81-4992-a55d-fe59719db7a2', foundational, legitimacy_requires_coupled_pair_not_single_artifact).
narrative_ontology:cs_axiom_status(legitimacy_requires_coupled_pair_not_single_artifact, holdable).
narrative_ontology:cs_axiom_grounding('24191bbc-0e81-4992-a55d-fe59719db7a2', legitimacy_requires_coupled_pair_not_single_artifact, conventional).
narrative_ontology:cs_axiom('24191bbc-0e81-4992-a55d-fe59719db7a2', secondary, process_disclosure_is_separable_from_surface_readability).
narrative_ontology:cs_axiom_status(process_disclosure_is_separable_from_surface_readability, holdable).
narrative_ontology:cs_axiom_grounding('24191bbc-0e81-4992-a55d-fe59719db7a2', process_disclosure_is_separable_from_surface_readability, instrumental).
narrative_ontology:cs_reference_frame('24191bbc-0e81-4992-a55d-fe59719db7a2', single_artifact_undifferentiated_legitimacy).
narrative_ontology:cs_drift_state('24191bbc-0e81-4992-a55d-fe59719db7a2', post_disclosure_infrastructure_maturation, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('24191bbc-0e81-4992-a55d-fe59719db7a2', '').
narrative_ontology:cs_kernel_id(process_transparency_reading, cooperative_artifact_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(process_transparency_reading, disclosure_infrastructure_operators).
narrative_ontology:constraint_beneficiary(process_transparency_reading, institutional_reviewers).
narrative_ontology:constraint_beneficiary(process_transparency_reading, credited_primary_authors).
narrative_ontology:constraint_victim(process_transparency_reading, uncredited_contributors).
narrative_ontology:constraint_victim(process_transparency_reading, contexts_without_disclosure_infrastructure).
narrative_ontology:constraint_victim(process_transparency_reading, authors_subject_to_gamed_provenance_records).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(process_transparency_reading, readers_and_downstream_users).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Build and administer the revision-history, contribution-statement, and process-log systems that constitute the 'record' half of the pair. They set the schema for what counts as an adequate trace, decide what is auditable, and their tooling becomes the de facto standard other institutions adopt. They collect authority and often licensing revenue from operating the infrastructure that legitimacy now routes through.
narrative_ontology:constraint_stakeholder(process_transparency_reading, disclosure_infrastructure_operators, agenda_setter,
    institutional, generational, arbitrage, global).

% Produce the smoothed, legible surface text and are named as primary authors in the disclosed process trace. Benefit from both the readability of the output (their work is legible and citable) and the credit-preservation function of the record (their labor is visible in the trace). Must maintain compliant revision histories to keep this benefit, which is a real but bearable cost.
narrative_ontology:constraint_stakeholder(process_transparency_reading, credited_primary_authors, beneficiary,
    moderate, biographical, constrained, national).

% Provide editing, translation, ghost-drafting, or structural labor that the smoothing process erases from the surface text. In principle the parallel record is supposed to preserve their contribution, but in practice contribution-statement categories are often controlled by others, or the labor falls into categories the schema doesn't recognize (informal feedback, uncredited structural rewrites). Their exit is to stop contributing or to contest categorization after the fact, both costly.
narrative_ontology:constraint_stakeholder(process_transparency_reading, uncredited_contributors, payer,
    powerless, biographical, trapped, national).

% Journals, funding bodies, and hiring committees that adjudicate authorship disputes and credit allocation by consulting the disclosed trace rather than re-litigating the smoothed text. Gain a lower-cost adjudication mechanism and offload the burden of verifying authorship onto the trace's existence, without needing to verify the trace's actual fidelity in most cases.
narrative_ontology:constraint_stakeholder(process_transparency_reading, institutional_reviewers, beneficiary,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(process_transparency_reading, institutional_reviewers, agenda_setter).

% Small labs, independent writers, informal collaborations in low-resource settings, or fast-moving contexts (breaking journalism, rapid open-source patches) that cannot afford or don't have access to the tooling that produces auditable revision histories and contribution statements. Their smoothed outputs are treated as suspect or non-legitimate by the coupled standard even when the underlying collaboration was genuinely fair, because they cannot produce the required parallel record.
narrative_ontology:constraint_stakeholder(process_transparency_reading, contexts_without_disclosure_infrastructure, payer,
    powerless, biographical, trapped, global).

% Work under co-authors, supervisors, or institutions that fabricate or selectively curate contribution statements and revision histories — crediting themselves for work they didn't do or omitting others' contributions strategically. Because legitimacy is now anchored to the trace rather than direct testimony, a falsified trace is harder to contest than a falsified claim would have been under the old single-artifact regime; the coupling raises the stakes of gaming the record without proportionally raising the cost of gaming it.
narrative_ontology:constraint_stakeholder(process_transparency_reading, authors_subject_to_gamed_provenance_records, payer,
    moderate, biographical, constrained, national).

% Consume the smoothed, legible surface text for its intended purpose (reading, building on, citing) without needing to parse the raw authorship trace unless a dispute arises. Gain readability without losing the option to check provenance later if something looks wrong — a real coordination benefit from the decoupling.
narrative_ontology:constraint_stakeholder(process_transparency_reading, readers_and_downstream_users, beneficiary,
    organized, biographical, mobile, global).

% Examine the disclosed process logs to assess whether the coupling is functioning as intended — whether traces are genuine, complete, and resistant to gaming — and can surface cases where the record itself has become theater.
narrative_ontology:constraint_stakeholder(process_transparency_reading, auditors_and_replication_researchers, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(process_transparency_reading, disclosure_infrastructure_operators).
narrative_ontology:fixing_cost_class(process_transparency_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the genuine tension between two goods that trade off against each other in a single artifact: readability (which requires smoothing, compression, editorial unification) and attributive fairness (which requires preserving the messy, multi-hand record of who actually did what). By splitting them into a coupled pair — smoothed surface plus disclosed process trace — both goods can be had without forcing a single document to serve both purposes badly.
% TRANSFER_FUNCTION: Moves interpretive authority and adjudicative burden from direct scrutiny of the artifact's raw text (which no longer needs to preserve every hand) to scrutiny of a separate, parallel trace. This shifts real power toward whoever controls the schema and tooling for that trace — disclosure infrastructure operators and the institutions that consult it — and shifts real risk onto contributors whose labor doesn't map cleanly onto the trace's categories, or who lack access to the tooling that produces a trace credible enough to be believed.
% ABSENT_VOICES: Contributors whose labor is informal, uncategorized, or occurs in low-resource contexts without disclosure tooling are structurally underrepresented in the design of the trace schema itself — the categories were built by and for institutions that already had revision-control and contribution-statement infrastructure. They would object that the coupling, meant to protect them, instead requires them to translate their labor into a format they had no say in designing.
% DISAPPEARANCE_RATIONALE: If the coupled requirement vanished, institutions would revert to either pure surface-text adjudication (legibility_primacy) or pure raw-draft preservation (authorial_primacy), each with different winners; disclosure infrastructure operators would lose their gatekeeping role, credited primary authors would lose a credit-preservation mechanism, and uncredited contributors would lose (or, depending on how bad the current gaming is, possibly regain leverage in) the only formal channel currently available to contest erasure.
% FOUNDING_PROBLEM: Two failure modes were each visible and costly on their own: purely smoothed artifacts erased real contributions (especially from junior collaborators, editors, and translators), while purely raw, unsmoothed drafts were unreadable and made legitimate authorship claims illegible to outside evaluators. The coupling was built to solve both at once without forcing institutions to choose.
% FOUNDING_PROBLEM_CORROBORATION: Disclosure infrastructure operators and institutional reviewers attest the founding problem remains live and the coupling is functioning as designed. Independent audits by replication researchers and reporting from uncredited-contributor advocacy groups — sources outside the beneficiary set — attest that in a substantial fraction of cases the trace has become a compliance artifact: revision histories are curated post hoc, contribution statements are drafted by the credited authors themselves rather than negotiated, and contexts without tooling access are treated as presumptively less legitimate regardless of actual collaborative fairness.
narrative_ontology:disappearance_verdict(process_transparency_reading, world_rearranges).
narrative_ontology:founding_problem_status(process_transparency_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(process_transparency_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-25',
    'unspecified', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'unspecified').
narrative_ontology:story_seed(process_transparency_reading, 'none', 1).
narrative_ontology:epsilon_provenance(process_transparency_reading, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(process_transparency_reading_tests).
:- end_tests(process_transparency_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness starts moderate-low (0.22) reflecting a genuinely functional coordination mechanism at founding, and rises to 0.42 over the interval as the disclosure infrastructure matures into a gatekeeping layer with its own rents (tooling licensing, credentialing of 'adequate' trace formats, institutional dependency on specific vendors). Theater ratio rises more sharply (0.14 to 0.38) because as the requirement institutionalizes, an increasing share of trace production becomes compliance behavior — post-hoc curated revision histories, contribution statements drafted unilaterally by the credited party rather than negotiated — rather than a genuine record of collaborative process. Suppression stays comparatively low and rises only modestly (0.20 to 0.31): the mechanism does not primarily depend on coercion to persist, it depends on institutional adoption and the switching cost of alternative adjudication norms, which is a softer form of lock-in than active suppression.
 *
 * PERSPECTIVAL GAP:
 *   From the disclosure infrastructure operator's seat, this looks like rope: a voluntary, mutually beneficial coordination solution to a real problem, adopted because it works. From the uncredited contributor's seat or the under-resourced context's seat, the same structure looks like a tangled rope shading toward snare: a genuine coordination function exists, but it is bundled with an asymmetric cost that falls on exactly the parties least able to contest the trace's fidelity. The scaffold claim is the author's structural belief about intended design; the metrics describe what the mechanism is actually doing across seats, and the divergence between the operator's rope-experience and the excluded contributor's tangled-rope-experience is the data point, not an error to reconcile.
 *
 * DIRECTIONALITY LOGIC:
 *   Disclosure infrastructure operators sit closest to the full-beneficiary end: they set the schema, and control over the schema is what the transfer function moves power toward. Credited primary authors are net beneficiaries with a real but bearable compliance cost. Uncredited contributors and contexts without tooling access sit closest to the full-target end: they bear the costs of a system built to protect them but designed without their input, and their exit options are trapped or constrained because opting out of the disclosure norm now reads as itself suspicious. Authors subject to gamed provenance records are a distinct victim class — their exit is constrained because contesting a fabricated trace requires exactly the kind of institutional standing the trace itself was supposed to make unnecessary.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — smoothed artifacts erasing contribution, raw artifacts being illegible to evaluators — is only partially live. Where disclosure tooling is genuinely used and audited, the coupling still solves a real problem and the scaffold framing holds: it is transitional infrastructure toward a norm where both readability and fairness are jointly verifiable. But where the trace has become compliance theater (curated after the fact, authored unilaterally, or simply unavailable to under-resourced contexts), the founding problem has effectively gone dead for those seats while the requirement persists and even hardens — exactly the mandatrophy signature this classification exists to catch. The has_sunset_clause declaration reflects the intended scaffold design (the mechanism is meant to be superseded once genuinely fair, low-friction disclosure norms are widespread) rather than an observed sunset that has actually triggered.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    trace_fidelity_verifiability,
    'Can the disclosed process trace (revision history, contribution statements, process logs) be independently verified as accurate, or does its evidentiary weight rest on trust in whoever produced it?',
    'Comparative audit studies checking self-reported contribution statements against independently reconstructed authorship (e.g., version-control metadata, third-party witness accounts) across a sample of disclosed-trace artifacts.',
    'If traces are generally verifiable, the scaffold framing holds and the mechanism is closer to genuine coordination with a manageable cost. If traces are routinely unverifiable or gamed without detection, the coupling degrades toward tangled_rope or snare, since the record half becomes theater that launders the smoothed surface''s legitimacy without actually preserving attribution.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(trace_fidelity_verifiability, empirical, 'Whether the disclosed trace is independently checkable or merely trusted.').

omega_variable(
    tooling_access_inequality,
    'Does the coupling requirement systematically disadvantage contexts that cannot afford or access disclosure infrastructure, independent of whether their underlying collaborative process was fair?',
    'Cross-context comparison of legitimacy outcomes for artifacts produced with versus without formal disclosure tooling, controlling for actual collaborative fairness as assessed by direct participant interview.',
    'If access inequality drives legitimacy outcomes more than actual fairness does, the coupling functions partly as an infrastructure-gatekeeping mechanism rather than a fairness-preserving one, strengthening the victim classification for under-resourced contexts.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(tooling_access_inequality, empirical, 'Whether disclosure-tooling access, not actual fairness, drives legitimacy verdicts.').

omega_variable(
    kernel_framing_alternative,
    'Is the artifact/record split itself the right locus for the kernel, or does the more consequential contest lie one level up — in who gets to design the disclosure schema''s categories in the first place?',
    'Compare classification outcomes if the kernel were reframed around ''who authors the disclosure schema'' rather than ''artifact vs. record'' — would beneficiary/victim assignments shift?',
    'If the schema-authorship framing dominates, the process_transparency_reading itself may be read as a second-order instance of the same dilemma it claims to dissolve: legibility of the trace schema versus authorial input into its design. This would suggest a fourth constraint story is warranted rather than treating this reading as terminal.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_framing_alternative, conceptual, 'Whether the artifact/record framing itself, versus schema-authorship, is the more structurally fundamental locus of the kernel''s contest.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(process_transparency_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(proc_tr_t0, process_transparency_reading, theater_ratio, 0, 0.14).
narrative_ontology:measurement(proc_tr_t4, process_transparency_reading, theater_ratio, 4, 0.18).
narrative_ontology:measurement(proc_tr_t8, process_transparency_reading, theater_ratio, 8, 0.23).
narrative_ontology:measurement(proc_tr_t12, process_transparency_reading, theater_ratio, 12, 0.27).
narrative_ontology:measurement(proc_tr_t16, process_transparency_reading, theater_ratio, 16, 0.31).
narrative_ontology:measurement(proc_tr_t20, process_transparency_reading, theater_ratio, 20, 0.35).
narrative_ontology:measurement(proc_tr_t24, process_transparency_reading, theater_ratio, 24, 0.38).

% Extraction over time
narrative_ontology:measurement(proc_be_t0, process_transparency_reading, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(proc_be_t4, process_transparency_reading, base_extractiveness, 4, 0.27).
narrative_ontology:measurement(proc_be_t8, process_transparency_reading, base_extractiveness, 8, 0.31).
narrative_ontology:measurement(proc_be_t12, process_transparency_reading, base_extractiveness, 12, 0.35).
narrative_ontology:measurement(proc_be_t16, process_transparency_reading, base_extractiveness, 16, 0.38).
narrative_ontology:measurement(proc_be_t20, process_transparency_reading, base_extractiveness, 20, 0.4).
narrative_ontology:measurement(proc_be_t24, process_transparency_reading, base_extractiveness, 24, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(proc_su_t0, process_transparency_reading, suppression_requirement, 0, 0.2).
narrative_ontology:measurement(proc_su_t4, process_transparency_reading, suppression_requirement, 4, 0.22).
narrative_ontology:measurement(proc_su_t8, process_transparency_reading, suppression_requirement, 8, 0.24).
narrative_ontology:measurement(proc_su_t12, process_transparency_reading, suppression_requirement, 12, 0.26).
narrative_ontology:measurement(proc_su_t16, process_transparency_reading, suppression_requirement, 16, 0.28).
narrative_ontology:measurement(proc_su_t20, process_transparency_reading, suppression_requirement, 20, 0.3).
narrative_ontology:measurement(proc_su_t24, process_transparency_reading, suppression_requirement, 24, 0.31).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(process_transparency_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(process_transparency_reading, 0.12).
narrative_ontology:affects_constraint(process_transparency_reading, legibility_primacy_reading).
narrative_ontology:affects_constraint(process_transparency_reading, authorial_primacy_reading).

% DUAL FORMULATION NOTE:
% Member of the cooperative_artifact_legitimacy kernel family (3 readings). legibility_primacy_reading treats the smoothed surface alone as sufficient for legitimacy (lower administrative overhead, higher erasure risk for uncredited labor). authorial_primacy_reading treats the raw multi-hand draft alone as legitimate (maximal attribution fidelity, minimal readability, high friction for downstream use). This story, process_transparency_reading, attempts to capture both goods via decoupling but introduces a new dependency on disclosure infrastructure control and trace fidelity that neither sibling reading carries. Each reading is authored as a separate ε-invariant constraint; this file's affects_constraints links to both siblings per the kernel decomposition rule.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
