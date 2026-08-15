% ============================================================================
% CONSTRAINT STORY: positional_disagreement_as_evidence_flat_control
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_positional_disagreement_as_evidence_flat_control, []).

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
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:flat_control_of/2,
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: positional_disagreement_as_evidence_flat_control
 *   human_readable: Positional Disagreement as Legitimate Epistemic Evidence
 *   domain: epistemology/philosophy_of_technology/institutional_analysis
 *
 * SUMMARY:
 *   The commitment under examination holds that when two honestly-reporting,
 *   differently-positioned observers disagree about the same arrangement, the
 *   disagreement itself is evidence about the arrangement's structure — not
 *   noise to be averaged away, and not something to be resolved by deferring
 *   to whichever account looks more neutral. This began as a corrective to a
 *   real and well-documented failure mode: naive aggregation and majoritarian
 *   'view from nowhere' synthesis routinely discarded minority, dissenting,
 *   and structurally subordinate accounts by treating them as bias to be
 *   corrected rather than data to be weighed. Over time the doctrine has also
 *   become the operating premise of methodological communities and
 *   professional identities (standpoint researchers, qualitative
 *   methodologists) whose institutional standing depends on positional
 *   reports being treated as non-defeasible. The constraint is authored FLAT
 *   here: it is not decomposed into separate readings for the founding
 *   correction versus the contemporary veto use, though the commentary and
 *   omegas below register that these two faces of the same standing
 *   commitment pull in different directions for different seats. This is
 *   deliberately NOT a tangled_rope story tuned to look extractive — the
 *   claimed type (tangled_rope) and the authored metrics (moderate
 *   extraction, moderate suppression, rising theater) are independent
 *   judgments about the same single constraint.
 *
 * KEY AGENTS:
 *   - standpoint_researchers: agenda-setters who administer the methodological norm (organized/identity_locked) — benefit from the doctrine's continued authority
 *   - marginalized_position_holders: primary beneficiaries whose accounts gain institutional weight (moderate/constrained)
 *   - qualitative_methodologists: beneficiaries whose professional toolkit depends on the doctrine (organized/identity_locked)
 *   - cross_positional_synthesizers: payers who must reconcile accounts and are blocked from doing so cleanly (moderate/constrained)
 *   - policy_arbitrators: institutional payers who must render binding decisions without a principled tiebreaker (institutional/constrained)
 *   - junior_analysts_without_standing: powerless payers who cannot invoke the doctrine on their own behalf and are penalized for attempting independent synthesis (powerless/trapped)
 *   - epistemic_community_observers: analytical observers tracing where the doctrine functions as correction versus veto (analytical/global)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(positional_disagreement_as_evidence_flat_control, 0.38).
domain_priors:suppression_score(positional_disagreement_as_evidence_flat_control, 0.34).
domain_priors:theater_ratio(positional_disagreement_as_evidence_flat_control, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(positional_disagreement_as_evidence_flat_control, extractiveness, 0.38).
narrative_ontology:constraint_metric(positional_disagreement_as_evidence_flat_control, suppression_requirement, 0.34).
narrative_ontology:constraint_metric(positional_disagreement_as_evidence_flat_control, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(positional_disagreement_as_evidence_flat_control, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(positional_disagreement_as_evidence_flat_control, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(positional_disagreement_as_evidence_flat_control, tangled_rope).
narrative_ontology:human_readable(positional_disagreement_as_evidence_flat_control, "Positional Disagreement as Legitimate Epistemic Evidence").
narrative_ontology:topic_domain(positional_disagreement_as_evidence_flat_control, "epistemology/philosophy_of_technology/institutional_analysis").

domain_priors:requires_active_enforcement(positional_disagreement_as_evidence_flat_control).

% --- Construction-pair linkage (forced-flat control of a kernel) ---
narrative_ontology:flat_control_of(positional_disagreement_as_evidence_flat_control, positional_disagreement_as_evidence).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(positional_disagreement_as_evidence_flat_control, standpoint_researchers).
narrative_ontology:constraint_beneficiary(positional_disagreement_as_evidence_flat_control, marginalized_position_holders).
narrative_ontology:constraint_beneficiary(positional_disagreement_as_evidence_flat_control, qualitative_methodologists).
narrative_ontology:constraint_victim(positional_disagreement_as_evidence_flat_control, cross_positional_synthesizers).
narrative_ontology:constraint_victim(positional_disagreement_as_evidence_flat_control, policy_arbitrators).
narrative_ontology:constraint_victim(positional_disagreement_as_evidence_flat_control, junior_analysts_without_standing).
narrative_ontology:constraint_vindicates(positional_disagreement_as_evidence_flat_control, standpoint_epistemology_thesis).
narrative_ontology:constraint_vindicates(positional_disagreement_as_evidence_flat_control, situated_knowledge_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Build research programs, journal norms, and hiring criteria around the principle that a differently-positioned report is data, not noise. They administer peer review standards that treat refusal to defer to a marginalized position's account as a methodological failure, and their professional standing is built on the doctrine's continued authority.
narrative_ontology:constraint_stakeholder(positional_disagreement_as_evidence_flat_control, standpoint_researchers, agenda_setter,
    organized, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(positional_disagreement_as_evidence_flat_control, standpoint_researchers, beneficiary).

% Gain institutional standing to have their account of an arrangement treated as irreducible evidence rather than something to be corrected toward a supposedly neutral consensus. Their testimony can now block or reshape institutional decisions that previously ignored them, though they depend on the doctrine's continued enforcement to retain this leverage.
narrative_ontology:constraint_stakeholder(positional_disagreement_as_evidence_flat_control, marginalized_position_holders, beneficiary,
    moderate, biographical, constrained, national).

% Their entire methodological toolkit (ethnography, testimonial analysis, standpoint interviews) depends on positional reports counting as evidence rather than bias. They train students, staff review boards, and gatekeep publication on this premise; abandoning it would devalue their accumulated expertise.
narrative_ontology:constraint_stakeholder(positional_disagreement_as_evidence_flat_control, qualitative_methodologists, beneficiary,
    organized, generational, identity_locked, national).

% Analysts, mediators, and policymakers whose job is to reconcile conflicting accounts into a workable decision. When positional reports are treated as non-averageable data rather than inputs to a synthesis, they cannot resolve genuine impasses without being accused of illegitimately overriding a standpoint. They bear the cost in stalled decisions, prolonged litigation-style disputes, and professional risk for attempting synthesis at all.
narrative_ontology:constraint_stakeholder(positional_disagreement_as_evidence_flat_control, cross_positional_synthesizers, payer,
    moderate, biographical, constrained, national).

% Must render binding decisions (regulatory rulings, court judgments, resource allocations) between parties who each claim their positional account is irreducible evidence. The doctrine denies them a principled way to weigh competing accounts against each other, forcing either paralysis or a decision that one side will characterize as epistemic violence.
narrative_ontology:constraint_stakeholder(positional_disagreement_as_evidence_flat_control, policy_arbitrators, payer,
    institutional, biographical, constrained, national).

% Early-career researchers and analysts who hold no recognized standpoint of their own (or whose standpoint is not institutionally legible) and so cannot invoke the doctrine on their own behalf. Their attempts at independent synthesis or dissent from a recognized standpoint's account are read as illegitimate deferral-avoidance, costing them standing and career advancement.
narrative_ontology:constraint_stakeholder(positional_disagreement_as_evidence_flat_control, junior_analysts_without_standing, payer,
    powerless, immediate, trapped, local).

% The actual arrangement being described (a workplace, a technology deployment, a policy) has no voice of its own in the dispute; it is characterized only through the competing positional reports, none of which is checked against an independent account of what the arrangement objectively does.
narrative_ontology:constraint_stakeholder(positional_disagreement_as_evidence_flat_control, institutional_arrangement_under_dispute, excluded,
    analytical, civilizational, analytical, national).
narrative_ontology:stakeholder_non_agent(positional_disagreement_as_evidence_flat_control, institutional_arrangement_under_dispute).

% Philosophers of science and methodologists who study how the doctrine operates across disciplines, tracing where it functions as genuine correction to observer-blind aggregation and where it functions as a veto mechanism immunizing favored accounts from scrutiny.
narrative_ontology:constraint_stakeholder(positional_disagreement_as_evidence_flat_control, epistemic_community_observers, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(positional_disagreement_as_evidence_flat_control, standpoint_researchers).
narrative_ontology:fixing_cost_class(positional_disagreement_as_evidence_flat_control, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves a genuine problem: naive aggregation (averaging accounts, deferring to a supposed neutral or majority view) erases real information that only exists at specific structural positions — a worker's account of a workplace arrangement contains data a manager's account cannot generate, and vice versa. Treating both as evidence rather than discarding one as bias preserves information that a single 'view from nowhere' synthesis would destroy.
% TRANSFER_FUNCTION: Moves interpretive authority and the burden of justification away from would-be synthesizers, arbitrators, and unrecognized voices, and toward whoever successfully claims a recognized structural position. It moves the cost of unresolved disagreement from the position-holders (who need not update or reconcile) to the parties responsible for making a decision anyway.
% ABSENT_VOICES: Junior analysts without institutionally legible standpoints, and the institutional arrangement itself (which has no report of its own, only external glosses on what it does) are structurally unable to contest characterizations offered from a recognized position. They would object that the doctrine, applied without limit, forecloses independent verification.
% DISAPPEARANCE_RATIONALE: Standpoint researchers and marginalized position holders would say the world rearranges badly: institutions would revert to averaging away or dismissing minority accounts as noise, and real information would be lost. Cross-positional synthesizers and policy arbitrators would say the world improves: decisions could be made on the merits of evidence rather than stalled by irreducibility claims. Both readings are defensible from where each party stands, which is itself an instance of the very phenomenon the constraint describes.
% FOUNDING_PROBLEM: Positivist and majoritarian social science had a documented history of discarding minority, dissenting, or structurally subordinate accounts as 'subjective' or 'biased' and correcting them toward a supposedly neutral average that was, in fact, the modal reporter's own position dressed as objectivity. The doctrine was built to stop real information from being erased this way.
% FOUNDING_PROBLEM_CORROBORATION: Historians and philosophers of science outside the standpoint research community (e.g., general philosophy of science literature on observer-selection effects, and institutional ethnographers studying bureaucratic decision-making) corroborate that erasure-by-averaging was a real and documented failure mode. However, the same outside observers increasingly document a second-generation problem the doctrine's proponents do not corroborate from inside: the doctrine's non-defeasibility being used to block legitimate synthesis and arbitration even where no erasure is occurring, a use its founders did not anticipate and some now dispute is a misuse rather than an application.
narrative_ontology:disappearance_verdict(positional_disagreement_as_evidence_flat_control, contested).
narrative_ontology:founding_problem_status(positional_disagreement_as_evidence_flat_control, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(positional_disagreement_as_evidence_flat_control, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-14',
    'unspecified', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'unspecified').
narrative_ontology:story_seed(positional_disagreement_as_evidence_flat_control, 'none', 1).
narrative_ontology:epsilon_provenance(positional_disagreement_as_evidence_flat_control, 0.38, 'claude-sonnet-5', 'cheap_confession_2026_20260814_151329', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(positional_disagreement_as_evidence_flat_control_tests).
:- end_tests(positional_disagreement_as_evidence_flat_control_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.38 at interval end) rather than high because the coordination function is genuine and substantial: real information about structural position is preserved that naive averaging would destroy, and this benefit is not fabricated. But extraction is non-trivial and rising because the doctrine's non-defeasibility clause — 'not to be averaged away or deferred on' — has no internal limiting principle, and institutional actors (standpoint researchers, qualitative methodologists) have build career and disciplinary infrastructure on its unlimited application, which increasingly forecloses legitimate synthesis rather than merely preventing illegitimate erasure. Suppression is moderate (0.34): dissent from the doctrine (attempting synthesis, or questioning a specific positional claim) is not criminalized but is reliably penalized in professional and reputational terms within the communities that administer it. Theater ratio rises over the interval (0.10 to 0.28) reflecting a documented drift: invocation of 'positionality as evidence' increasingly functions as a rhetorical move to end a dispute rather than as a substantive methodological practice of eliciting and weighing the actual content of a positional report.
 *
 * PERSPECTIVAL GAP:
 *   From the standpoint researcher's seat, the constraint looks like a rope (or even a mountain-like epistemic bedrock) — an obviously correct check against erasure that any honest epistemology must accept. From the policy arbitrator's or cross-positional synthesizer's seat, the same structure looks like a tangled rope shading toward snare: a genuine point about information-preservation that has hardened into an unfalsifiable veto that blocks the very decisions their institutional role exists to make. The engine's per-seat computation should reflect this divergence directly from the declared power/exit/scope data — the claim (tangled_rope) is not tuned to force either side's view.
 *
 * DIRECTIONALITY LOGIC:
 *   Standpoint researchers and qualitative methodologists are structural beneficiaries: their professional identity, hiring criteria, and publication gatekeeping are built on the doctrine, giving them low directionality (near the beneficiary end) despite being 'organized' rather than 'institutional' power. Marginalized position holders are beneficiaries in the narrower but real sense of gaining leverage they previously lacked — this is the doctrine's genuine coordination payoff. Cross-positional synthesizers and policy arbitrators are targets: the doctrine transfers to them the cost of irresolvable disagreement without giving them a principled way to discharge their institutional function of actually deciding. Junior analysts without standing are the sharpest case: powerless and trapped, they bear the doctrine's suppressive edge without any of its protective benefit, since they hold no recognized standpoint to invoke.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — real erasure of minority and subordinate accounts through naive aggregation — was genuinely live at the doctrine's origin and remains partially live today (corroborated by philosophy-of-science literature outside the standpoint research community). But there is a second, less corroborated use: treating positional non-defeasibility as blocking ANY synthesis whatsoever, including synthesis that would not erase anyone's information but would simply weigh competing accounts against independently verifiable facts about the arrangement. This second use is where mandatrophy risk concentrates — the founding problem's status is 'contested' precisely because the doctrine's beneficiaries have an interest in treating the founding problem as permanently and totally live (never contested, never partially resolved), while the doctrine's payers observe specific contexts where erasure is not the live risk and synthesis is being blocked anyway.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    correction_vs_veto_boundary,
    'Is there a principled way to distinguish cases where treating positional disagreement as non-averageable evidence prevents genuine erasure, from cases where the same move blocks legitimate synthesis that would not erase any information?',
    'Case-level analysis distinguishing disputes where an independently verifiable fact about the arrangement is available (permitting synthesis without erasure) from disputes where no independent check exists (where the doctrine''s protection is load-bearing). Track outcomes across policy arbitration cases where the doctrine was invoked to see whether independent verification was possible but foreclosed.',
    'If a reliable boundary exists and is respected, the constraint functions closer to a rope with a narrow, legitimate extractive residue (tangled_rope with declining extraction). If no boundary is respected in practice, the constraint drifts toward snare as the doctrine''s non-defeasibility becomes a general-purpose veto regardless of whether erasure risk is present.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(correction_vs_veto_boundary, conceptual, 'Whether a principled correction/veto boundary exists and is honored.').

omega_variable(
    who_counts_as_a_position,
    'Who is institutionally recognized as holding a ''position'' whose report counts as irreducible evidence, and who is denied that recognition?',
    'Audit of which claimants successfully invoke the doctrine across disciplines and institutions versus which claimants (particularly junior analysts and holders of institutionally illegible positions) are denied recognition despite plausible structural standing.',
    'If recognition tracks genuine structural position, the doctrine''s beneficiary/victim asymmetry is a byproduct of real epistemic asymmetry. If recognition tracks institutional power (who can successfully claim a standpoint) rather than structural position itself, the doctrine functions partly as a credentialing mechanism that concentrates interpretive authority rather than distributing it.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(who_counts_as_a_position, empirical, 'Whether standpoint-recognition tracks structural position or institutional power.').

omega_variable(
    founding_problem_current_prevalence,
    'How prevalent is naive-aggregation erasure of minority/subordinate accounts in contemporary institutional practice, compared to when the doctrine was established?',
    'Comparative institutional history: track whether decision-making bodies that historically discarded minority accounts through averaging have adopted other correctives (structured elicitation, weighted deliberation) independent of the non-defeasibility doctrine, which would indicate the founding problem is now partially addressed through other means.',
    'If the founding problem has substantially receded due to other institutional reforms, the doctrine''s continued non-defeasible form is doing less founding-problem work and more status-quo-protection work for its current beneficiaries — supporting the founding_problem_status of ''contested'' rather than ''live''.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(founding_problem_current_prevalence, empirical, 'Whether the founding erasure problem remains as prevalent as when the doctrine was established.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(positional_disagreement_as_evidence_flat_control, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(posi_tr_t0, positional_disagreement_as_evidence_flat_control, theater_ratio, 0, 0.1).
narrative_ontology:measurement(posi_tr_t8, positional_disagreement_as_evidence_flat_control, theater_ratio, 8, 0.14).
narrative_ontology:measurement(posi_tr_t16, positional_disagreement_as_evidence_flat_control, theater_ratio, 16, 0.18).
narrative_ontology:measurement(posi_tr_t24, positional_disagreement_as_evidence_flat_control, theater_ratio, 24, 0.21).
narrative_ontology:measurement(posi_tr_t32, positional_disagreement_as_evidence_flat_control, theater_ratio, 32, 0.25).
narrative_ontology:measurement(posi_tr_t40, positional_disagreement_as_evidence_flat_control, theater_ratio, 40, 0.28).

% Extraction over time
narrative_ontology:measurement(posi_be_t0, positional_disagreement_as_evidence_flat_control, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(posi_be_t8, positional_disagreement_as_evidence_flat_control, base_extractiveness, 8, 0.24).
narrative_ontology:measurement(posi_be_t16, positional_disagreement_as_evidence_flat_control, base_extractiveness, 16, 0.29).
narrative_ontology:measurement(posi_be_t24, positional_disagreement_as_evidence_flat_control, base_extractiveness, 24, 0.33).
narrative_ontology:measurement(posi_be_t32, positional_disagreement_as_evidence_flat_control, base_extractiveness, 32, 0.36).
narrative_ontology:measurement(posi_be_t40, positional_disagreement_as_evidence_flat_control, base_extractiveness, 40, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(posi_su_t0, positional_disagreement_as_evidence_flat_control, suppression_requirement, 0, 0.15).
narrative_ontology:measurement(posi_su_t8, positional_disagreement_as_evidence_flat_control, suppression_requirement, 8, 0.2).
narrative_ontology:measurement(posi_su_t16, positional_disagreement_as_evidence_flat_control, suppression_requirement, 16, 0.24).
narrative_ontology:measurement(posi_su_t24, positional_disagreement_as_evidence_flat_control, suppression_requirement, 24, 0.28).
narrative_ontology:measurement(posi_su_t32, positional_disagreement_as_evidence_flat_control, suppression_requirement, 32, 0.31).
narrative_ontology:measurement(posi_su_t40, positional_disagreement_as_evidence_flat_control, suppression_requirement, 40, 0.34).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(positional_disagreement_as_evidence_flat_control, identity_coordination).
narrative_ontology:boltzmann_floor_override(positional_disagreement_as_evidence_flat_control, 0.1).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
