% ============================================================================
% CONSTRAINT STORY: authorial_primacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_authorial_primacy_reading, []).

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
    narrative_ontology:cs_created_at/2,
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: authorial_primacy_reading
 *   human_readable: Authorial Primacy Reading of Cooperative Artifact Legitimacy
 *   domain: epistemics_of_cooperation/signaling_theory/authorship_attribution
 *
 * SUMMARY:
 *   This constraint instantiates the authorial-primacy reading of a contested
 *   kernel about what makes a cooperative artifact legitimate. The kernel
 *   itself — cooperative_artifact_legitimacy — is under-determined between at
 *   least three readings: this one (legitimacy tracks traceable origination,
 *   and erasing authorial texture for legibility is fraud), a
 *   legibility-primacy reading (legitimacy tracks how well the artifact
 *   functions for its audience, and unreadable texture is a cost not a
 *   virtue), and a process-transparency reading (legitimacy tracks whether
 *   editing/smoothing is disclosed, regardless of whether the final text is
 *   smooth or raw). This story generates ONLY the authorial-primacy reading
 *   as a clean, ε-invariant constraint: its beneficiaries are individual
 *   contributors and the auditors who enforce the traceability standard; its
 *   victims are the evaluators who must do the interpretive labor the
 *   standard refuses to outsource, and collective projects that cannot cohere
 *   without synthesis the standard treats as suspect.
 *
 * KEY AGENTS:
 *   - individual_contributors: beneficiary (moderate/constrained) — preserved authorship, protected from ghost-editing
 *   - attribution_auditors: agenda_setter (organized/mobile) — enforce the traceability standard
 *   - downstream_evaluators: payer (moderate/constrained) — absorb the interpretive labor cost
 *   - collective_projects_requiring_synthesis: payer (powerless/trapped) — cannot synthesize without seeming fraudulent
 *   - legibility_optimizers: excluded (moderate/constrained) — their editing work is recast as violation
 *   - genealogical_observers: observer (analytical/analytical) — trace the standard's actual cost distribution
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(authorial_primacy_reading, 0.58).
domain_priors:suppression_score(authorial_primacy_reading, 0.42).
domain_priors:theater_ratio(authorial_primacy_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(authorial_primacy_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(authorial_primacy_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(authorial_primacy_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(authorial_primacy_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(authorial_primacy_reading, resistance, 0.61).

% --- Constraint claim ---
narrative_ontology:constraint_claim(authorial_primacy_reading, tangled_rope).
narrative_ontology:human_readable(authorial_primacy_reading, "Authorial Primacy Reading of Cooperative Artifact Legitimacy").
narrative_ontology:topic_domain(authorial_primacy_reading, "epistemics_of_cooperation/signaling_theory/authorship_attribution").

domain_priors:requires_active_enforcement(authorial_primacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(authorial_primacy_reading, '556d83d8-e17c-406e-b0d7-1e101e7513f9').
narrative_ontology:cs_kernel_codification('556d83d8-e17c-406e-b0d7-1e101e7513f9', distributed).
narrative_ontology:cs_authority_grounding('556d83d8-e17c-406e-b0d7-1e101e7513f9', practice).
narrative_ontology:cs_interpretation_layer_present('556d83d8-e17c-406e-b0d7-1e101e7513f9').
narrative_ontology:cs_reading_relation('556d83d8-e17c-406e-b0d7-1e101e7513f9', cooperative_artifact_legitimacy__legibility_primacy_reading, coexists_with).
narrative_ontology:cs_reading_relation('556d83d8-e17c-406e-b0d7-1e101e7513f9', cooperative_artifact_legitimacy__process_transparency_reading, influences).
narrative_ontology:cs_axiom('556d83d8-e17c-406e-b0d7-1e101e7513f9', foundational, traceable_origination_is_the_legitimacy_criterion).
narrative_ontology:cs_axiom_status(traceable_origination_is_the_legitimacy_criterion, holdable).
narrative_ontology:cs_axiom_grounding('556d83d8-e17c-406e-b0d7-1e101e7513f9', traceable_origination_is_the_legitimacy_criterion, deontological).
narrative_ontology:cs_axiom('556d83d8-e17c-406e-b0d7-1e101e7513f9', foundational, consumption_ease_is_irrelevant_or_inversely_indicative_of_legitimacy).
narrative_ontology:cs_axiom_status(consumption_ease_is_irrelevant_or_inversely_indicative_of_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('556d83d8-e17c-406e-b0d7-1e101e7513f9', consumption_ease_is_irrelevant_or_inversely_indicative_of_legitimacy, conventional).
narrative_ontology:cs_created_at('556d83d8-e17c-406e-b0d7-1e101e7513f9', '').
narrative_ontology:cs_kernel_id(authorial_primacy_reading, cooperative_artifact_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(authorial_primacy_reading, individual_contributors).
narrative_ontology:constraint_beneficiary(authorial_primacy_reading, attribution_auditors).
narrative_ontology:constraint_victim(authorial_primacy_reading, downstream_evaluators).
narrative_ontology:constraint_victim(authorial_primacy_reading, collective_projects_requiring_synthesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Produce artifacts (code, prose, proofs) and want the record to show exactly what they did and how they reasoned, unedited toward smoothness. Under this reading their raw authorial texture is preserved as the legitimacy-bearing signal; they benefit from not having to normalize their work into consumable form, and from being protected against ghost-editing that would erase their traceable contribution.
narrative_ontology:constraint_stakeholder(authorial_primacy_reading, individual_contributors, beneficiary,
    moderate, biographical, constrained, national).

% Peer reviewers, editors-of-record, and provenance-tracking institutions who administer the norm that unedited texture is the legitimacy criterion. They set and enforce the rule that smoothing an artifact for readability, without disclosing the smoothing, constitutes fraud. They gain standing and relevance from being the arbiters of what counts as an honest record.
narrative_ontology:constraint_stakeholder(authorial_primacy_reading, attribution_auditors, agenda_setter,
    organized, generational, mobile, national).

% Readers, reviewers, hiring committees, and users who must now consume artifacts left deliberately unsmoothed because smoothing is treated as effacement. They absorb the full cost of the interpretive labor that legibility-optimization would otherwise have performed for them. Their only real exit is to refuse to engage with the artifact at all, which forfeits the artifact's content.
narrative_ontology:constraint_stakeholder(authorial_primacy_reading, downstream_evaluators, payer,
    moderate, immediate, constrained, national).

% Multi-author efforts (large codebases, joint reports, committee documents) that need a single coherent voice to function as a usable output. Under authorial primacy, harmonizing multiple contributors' texture into one legible artifact is itself suspect as ghost-authorship, so these projects either fragment into visibly seamed patchwork or absorb enormous coordination cost to reconcile texture preservation with usability. They cannot exit the norm without abandoning the legitimacy claim entirely.
narrative_ontology:constraint_stakeholder(authorial_primacy_reading, collective_projects_requiring_synthesis, payer,
    powerless, biographical, trapped, regional).

% Editors, technical writers, and process-oriented reviewers who believe smoothing an artifact for consumption is a legitimate service, not fraud. Under the authorial-primacy reading their work is recast as a credibility violation rather than a value-add, and they have no seat in adjudicating what counts as honest editing versus effacement.
narrative_ontology:constraint_stakeholder(authorial_primacy_reading, legibility_optimizers, excluded,
    moderate, immediate, constrained, national).

% Historians and philosophers of authorship who trace how the traceable-origination standard arose and whether it tracks any independent good beyond signaling contributor identity. They observe that the standard's costs fall almost entirely on evaluators and collective projects while its benefits accrue to individual claimants of authorship.
narrative_ontology:constraint_stakeholder(authorial_primacy_reading, genealogical_observers, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(authorial_primacy_reading, individual_contributors).
narrative_ontology:fixing_cost_class(authorial_primacy_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates trust between a contributor and anyone relying on the artifact by fixing a single, auditable criterion for legitimacy: does the record show real, traceable authorial reasoning, undisguised by post-hoc smoothing? This solves a genuine problem — ghost-authorship and effaced labor genuinely occur and genuinely deceive.
% TRANSFER_FUNCTION: Moves interpretive labor from the contributor (who no longer needs to make the artifact legible) to the evaluator and to any collective project needing a coherent synthesized output; moves reputational credit toward whoever can demonstrate raw, unsmoothed authorship and away from editors, synthesizers, and legibility specialists.
% ABSENT_VOICES: Legibility optimizers and process-transparency advocates (editors, technical writers, synthesis specialists) are structurally excluded from setting the legitimacy criterion under this reading — their labor is redefined as suspect rather than consulted as a competing account of what honest cooperation requires.
% DISAPPEARANCE_RATIONALE: If the authorial-primacy standard vanished, individual contributors would lose their strongest claim against ghost-editing and effacement, and some genuine fraud (silent erasure of labor) would go undetected — a real loss. But downstream evaluators and collective projects would regain the freedom to smooth artifacts for consumption without being accused of fraud, and multi-author projects could synthesize coherent outputs without every seam being treated as a legitimacy violation. Whether the world 'rearranges' or 'stays roughly the same' depends on which party's account of legitimacy is credited, which is exactly the kernel-level dispute this reading is one side of.
% FOUNDING_PROBLEM: Cooperative artifacts (papers, code, joint reports) were being smoothed, ghost-written, or edited in ways that erased who actually did the reasoning and the work, allowing credit-taking without traceable origination and allowing errors to be laundered through an anonymized, seamless final product.
% FOUNDING_PROBLEM_CORROBORATION: Individual contributors and attribution auditors attest the problem is live and worsening (citing large-team science and AI-assisted writing making ghost-authorship easier). Legibility optimizers and genealogical observers — outside the beneficiary set — attest that the traceable-origination standard, as currently enforced, now imposes interpretive costs on evaluators and collective projects that exceed the fraud it prevents, and that a disclosed-editing regime (the process_transparency_reading) addresses the same fraud at lower cost to synthesis.
narrative_ontology:disappearance_verdict(authorial_primacy_reading, contested).
narrative_ontology:founding_problem_status(authorial_primacy_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(authorial_primacy_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-25',
    'unspecified', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'unspecified').
narrative_ontology:story_seed(authorial_primacy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(authorial_primacy_reading, 0.58, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(authorial_primacy_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(authorial_primacy_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(authorial_primacy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is moderate-high (0.58) and rising over the interval because the standard's enforcement has hardened: what began as a defense against clear ghost-authorship fraud has drifted toward penalizing any editorial smoothing, including disclosed and benign smoothing. Suppression is moderate (0.42) — it operates mainly through reputational sanction (accusations of fraud) rather than legal coercion, and much of it is the internalized professional norm that editing = illegitimacy, not an external enforcement apparatus. Theater ratio is moderate-low (0.28) but rising, reflecting a growing share of 'authenticity signaling' (deliberately leaving artifacts rough to perform authorial primacy) that serves no interpretive function.
 *
 * PERSPECTIVAL GAP:
 *   From the contributor/auditor seat, the arrangement is a genuine anti-fraud coordination mechanism — it is what stands between honest attribution and ghost-authorship laundering. From the evaluator/synthesis-project seat, the same arrangement operates as an enforced tax on consumption and a structural block on producing usable joint output. The engine should compute these as different seat-level types from the same structural data; the divergence is the point, not an error to reconcile.
 *
 * DIRECTIONALITY LOGIC:
 *   Individual contributors sit near the beneficiary end: the standard directly protects their claim to credit and insulates them from the labor of making their work legible. Attribution auditors benefit similarly — they are the standard's administrators and gain relevance from enforcing it. Downstream evaluators and collective-synthesis projects sit near the target end: the costs the standard imposes (interpretive labor, coordination breakdown) land on them without their having chosen the tradeoff. Legibility optimizers are excluded rather than coordinated — their competing account of legitimacy is not adjudicated, simply overridden by this reading's authority.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (undetected ghost-authorship and effaced labor) was real and in some domains remains live. But the standard as currently enforced has drifted from 'detect and sanction undisclosed effacement' toward 'treat all smoothing as effacement regardless of disclosure,' which is a stronger and more costly claim than the founding problem requires. The process-transparency reading (a sibling constraint) addresses the same founding problem — undisclosed ghost-authorship — at lower cost, by making disclosure rather than rawness the criterion. That the authorial-primacy reading persists despite a lower-cost sibling addressing the same problem is itself evidence the standard now serves interests (credit-claiming, auditor relevance) beyond the founding problem.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indeterminacy,
    'Is ''cooperative artifact legitimacy'' genuinely best tracked by raw traceable origination (this reading), by audience-side functional legibility (the sibling reading), or by disclosed process regardless of final smoothness (the other sibling reading)? The three readings are not obviously reconcilable into one observable.',
    'No empirical test resolves this; it is a normative/conceptual dispute about what ''honest record'' means. Partial evidence: track whether disclosed-but-smoothed artifacts (process_transparency_reading''s target case) are treated as fraudulent under actual community norms, versus whether undisclosed-but-legible artifacts are. Divergent community verdicts on these test cases would reveal which reading is operative in practice.',
    'If the community''s actual practice tracks disclosure rather than rawness, this reading (authorial_primacy_reading) is the less descriptively accurate one, and its enforcement costs (borne by evaluators and synthesis projects) are harder to justify against the founding problem.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_indeterminacy, conceptual, 'Structural under-determination among three sibling readings of the same kernel — this omega documents the reading choice rather than resolving it.').

omega_variable(
    texture_preservation_vs_signaling_authenticity,
    'Is retained authorial texture actually evidence of traceable origination, or has it become a performable signal (deliberately leaving artifacts rough to claim legitimacy) that no longer correlates with genuine unedited reasoning?',
    'Compare artifacts where roughness is verified as unedited (e.g., timestamped draft history) against artifacts where roughness is added post-hoc as a stylistic signal; measure whether audiences or auditors can actually distinguish the two.',
    'If roughness has become gameable as a signal, the theater_ratio for this reading is underestimated and the standard''s coordination function is more degraded than the current metrics show.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(texture_preservation_vs_signaling_authenticity, empirical, 'Whether authorial texture still functions as a reliable signal of traceable origination or has become a performable proxy.').

omega_variable(
    beneficiary_capture_of_standard_setting,
    'Do attribution auditors set and enforce the authorial-primacy standard because it is the most fraud-resistant reading, or because being the arbiter of ''what counts as honest'' confers institutional relevance and authority independent of the standard''s actual fraud-detection performance?',
    'Examine whether auditor institutions have adopted the lower-cost process-transparency alternative when it becomes available, or have resisted it despite comparable fraud-detection performance.',
    'If auditors resist the lower-cost alternative, this supports reading the standard as partially self-interested (tangled_rope) rather than purely coordination-serving (rope), consistent with the claimed_type here.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_capture_of_standard_setting, empirical, 'Whether standard-setting authority is justified by performance or is self-perpetuating independent of performance.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(authorial_primacy_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(auth_tr_t0, authorial_primacy_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(auth_tr_t4, authorial_primacy_reading, theater_ratio, 4, 0.15).
narrative_ontology:measurement(auth_tr_t8, authorial_primacy_reading, theater_ratio, 8, 0.19).
narrative_ontology:measurement(auth_tr_t12, authorial_primacy_reading, theater_ratio, 12, 0.22).
narrative_ontology:measurement(auth_tr_t16, authorial_primacy_reading, theater_ratio, 16, 0.25).
narrative_ontology:measurement(auth_tr_t20, authorial_primacy_reading, theater_ratio, 20, 0.27).
narrative_ontology:measurement(auth_tr_t24, authorial_primacy_reading, theater_ratio, 24, 0.28).

% Extraction over time
narrative_ontology:measurement(auth_be_t0, authorial_primacy_reading, base_extractiveness, 0, 0.34).
narrative_ontology:measurement(auth_be_t4, authorial_primacy_reading, base_extractiveness, 4, 0.41).
narrative_ontology:measurement(auth_be_t8, authorial_primacy_reading, base_extractiveness, 8, 0.47).
narrative_ontology:measurement(auth_be_t12, authorial_primacy_reading, base_extractiveness, 12, 0.51).
narrative_ontology:measurement(auth_be_t16, authorial_primacy_reading, base_extractiveness, 16, 0.55).
narrative_ontology:measurement(auth_be_t20, authorial_primacy_reading, base_extractiveness, 20, 0.57).
narrative_ontology:measurement(auth_be_t24, authorial_primacy_reading, base_extractiveness, 24, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(auth_su_t0, authorial_primacy_reading, suppression_requirement, 0, 0.22).
narrative_ontology:measurement(auth_su_t4, authorial_primacy_reading, suppression_requirement, 4, 0.28).
narrative_ontology:measurement(auth_su_t8, authorial_primacy_reading, suppression_requirement, 8, 0.33).
narrative_ontology:measurement(auth_su_t12, authorial_primacy_reading, suppression_requirement, 12, 0.36).
narrative_ontology:measurement(auth_su_t16, authorial_primacy_reading, suppression_requirement, 16, 0.39).
narrative_ontology:measurement(auth_su_t20, authorial_primacy_reading, suppression_requirement, 20, 0.41).
narrative_ontology:measurement(auth_su_t24, authorial_primacy_reading, suppression_requirement, 24, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(authorial_primacy_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(authorial_primacy_reading, 0.08).
narrative_ontology:affects_constraint(authorial_primacy_reading, legibility_primacy_reading).
narrative_ontology:affects_constraint(authorial_primacy_reading, process_transparency_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the kernel cooperative_artifact_legitimacy. legibility_primacy_reading holds legitimacy tracks audience-side functional clarity, treating this reading's tolerance for costly-to-consume artifacts as itself a form of extraction from evaluators. process_transparency_reading holds legitimacy tracks disclosed editing process independent of final smoothness, offering a lower-enforcement-cost path to the same anti-fraud founding problem this reading addresses. All three share a founding problem (detecting ghost-authorship/effaced labor) but diverge sharply on which observable operationalizes 'honest record,' producing different beneficiary/victim structures and different ε profiles — hence three separate constraint stories rather than one story with a measurement parameter.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
