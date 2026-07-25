% ============================================================================
% CONSTRAINT STORY: cooperative_artifact_legitimacy_flat_control
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_cooperative_artifact_legitimacy_flat_control, []).

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
 *   constraint_id: cooperative_artifact_legitimacy_flat_control
 *   human_readable: Dual-Audience Legitimacy Requirement for Cooperative Work Artifacts
 *   domain: epistemics_of_cooperation/signaling_theory/authorship_attribution
 *
 * SUMMARY:
 *   This story authors, as a single flat constraint, the commitment shared by
 *   two contested readings of cooperative artifact legitimacy: that any
 *   artifact produced through cooperative labor must satisfy an evaluator
 *   (who needs efficient legibility) and an attribution system (who needs a
 *   traceable causal origin), and that failure on either axis
 *   ('ghost-written' — legible but falsely attributed, or 'unreadable' —
 *   traceable but unusable) is a real cost. The two readings this substrate
 *   could decompose into disagree about which failure mode is the acceptable
 *   trade-off; this flat construction does NOT decompose them. Instead it
 *   treats the dual demand itself as one tangled-rope constraint: genuine
 *   coordination function (verification at scale requires proxies) fused with
 *   asymmetric extraction (the burden of reconciling the two demands falls
 *   overwhelmingly on low-power contributors whose labor is exactly the
 *   smoothing work that erases their own trace).
 *
 * KEY AGENTS:
 *   - junior_contributors: primary payer (powerless/constrained) — does drafting/editing work erased by the legibility pass
 *   - collaborative_and_editing_labor: primary payer (moderate/constrained) — the smoothing labor that must disappear for legibility to succeed
 *   - evaluators_and_gatekeepers: primary beneficiary/agenda_setter (institutional/arbitrage) — sets the working default that favors cheap legibility checks over costly provenance audits
 *   - credentialing_institutions: primary beneficiary (institutional/arbitrage) — collects trust rents from being the arbiter of both criteria
 *   - originating_authors_of_record: beneficiary (moderate/mobile) — captures attribution value regardless of who did the underlying work
 *   - unattributed_originators: excluded (powerless/trapped) — analytical observer of the failure but structurally outside the adjudicating forums
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(cooperative_artifact_legitimacy_flat_control, 0.58).
domain_priors:suppression_score(cooperative_artifact_legitimacy_flat_control, 0.47).
domain_priors:theater_ratio(cooperative_artifact_legitimacy_flat_control, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(cooperative_artifact_legitimacy_flat_control, extractiveness, 0.58).
narrative_ontology:constraint_metric(cooperative_artifact_legitimacy_flat_control, suppression_requirement, 0.47).
narrative_ontology:constraint_metric(cooperative_artifact_legitimacy_flat_control, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(cooperative_artifact_legitimacy_flat_control, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(cooperative_artifact_legitimacy_flat_control, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(cooperative_artifact_legitimacy_flat_control, tangled_rope).
narrative_ontology:human_readable(cooperative_artifact_legitimacy_flat_control, "Dual-Audience Legitimacy Requirement for Cooperative Work Artifacts").
narrative_ontology:topic_domain(cooperative_artifact_legitimacy_flat_control, "epistemics_of_cooperation/signaling_theory/authorship_attribution").

domain_priors:requires_active_enforcement(cooperative_artifact_legitimacy_flat_control).

% --- Construction-pair linkage (forced-flat control of a kernel) ---
narrative_ontology:flat_control_of(cooperative_artifact_legitimacy_flat_control, cooperative_artifact_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(cooperative_artifact_legitimacy_flat_control, evaluators_and_gatekeepers).
narrative_ontology:constraint_beneficiary(cooperative_artifact_legitimacy_flat_control, credentialing_institutions).
narrative_ontology:constraint_victim(cooperative_artifact_legitimacy_flat_control, junior_contributors).
narrative_ontology:constraint_victim(cooperative_artifact_legitimacy_flat_control, collaborative_and_editing_labor).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(cooperative_artifact_legitimacy_flat_control, collaborative_and_editing_labor).
narrative_ontology:constraint_beneficiary(cooperative_artifact_legitimacy_flat_control, originating_authors_of_record).
narrative_ontology:constraint_beneficiary(cooperative_artifact_legitimacy_flat_control, artifact_consumers).
narrative_ontology:constraint_vindicates(cooperative_artifact_legitimacy_flat_control, legible_artifacts_are_trustworthy).
narrative_ontology:constraint_vindicates(cooperative_artifact_legitimacy_flat_control, traceable_authorship_is_accountability).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Do substantial drafting, editing, and problem-solving work but occupy positions (research assistants, ghostwriters, junior engineers, uncredited co-authors) where the artifact's final legible form is attributed upward to a senior name. They cannot simultaneously make the work more legible for the evaluator and preserve a visible authorial trace of their own contribution — polishing the artifact for consumption tends to erase the marks of who actually produced it. Their exit is constrained by career dependency on the credentialing institution that enforces the dual standard.
narrative_ontology:constraint_stakeholder(cooperative_artifact_legitimacy_flat_control, junior_contributors, payer,
    powerless, biographical, constrained, national).

% Editors, translators, uncredited co-writers, and collaborative-tool-mediated contributors (including AI-assisted drafting) whose labor is precisely the smoothing work that produces legibility. They benefit when the smoothing is acknowledged as skilled labor, but more often absorb the cost: their contribution is definitionally the layer that must disappear for the artifact to read as authored by one traceable originator.
narrative_ontology:constraint_stakeholder(cooperative_artifact_legitimacy_flat_control, collaborative_and_editing_labor, payer,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(cooperative_artifact_legitimacy_flat_control, collaborative_and_editing_labor, beneficiary).

% Reviewers, editors, hiring committees, and graders who must process large volumes of artifacts under time constraints. They set the working standard — legibility first, provenance audit only on suspicion — because it is cheap for them to enforce and lets them process volume. They benefit from a standard they did not have to justify: the artifact that reads well is treated as legitimate by default, shifting the burden of proving provenance onto whoever is challenged, rarely onto the artifact's polish.
narrative_ontology:constraint_stakeholder(cooperative_artifact_legitimacy_flat_control, evaluators_and_gatekeepers, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(cooperative_artifact_legitimacy_flat_control, evaluators_and_gatekeepers, beneficiary).

% Universities, publishers, professional licensing bodies, and platforms that certify artifacts as legitimate. They collect the reputational and financial rents that flow from being the trusted arbiter of both legibility and attribution, without bearing the cost when the two criteria conflict — that cost lands on whichever contributor's labor was sacrificed to satisfy the criterion the institution weighted that cycle.
narrative_ontology:constraint_stakeholder(cooperative_artifact_legitimacy_flat_control, credentialing_institutions, beneficiary,
    institutional, civilizational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(cooperative_artifact_legitimacy_flat_control, credentialing_institutions, agenda_setter).

% The senior name, principal investigator, or credited author who receives attribution for the finished artifact. They can move between institutions and collaborators, and generally benefit from the current settlement: their name captures the attribution value regardless of which audience — legibility or provenance — actually did more work to produce the final product.
narrative_ontology:constraint_stakeholder(cooperative_artifact_legitimacy_flat_control, originating_authors_of_record, beneficiary,
    moderate, biographical, mobile, national).

% Readers, users, downstream researchers who consume the finished artifact. They benefit from legibility directly (faster, cheaper comprehension) and benefit from attribution indirectly (knowing who to trust or blame). They rarely bear the cost of the tension between the two demands and can simply move to a different artifact if one fails either test.
narrative_ontology:constraint_stakeholder(cooperative_artifact_legitimacy_flat_control, artifact_consumers, beneficiary,
    organized, immediate, mobile, global).

% Those whose causal contribution to an artifact was real but was smoothed away in the legibility pass and never surfaced in the attribution record — the uncredited translator, the silenced co-inventor, the erased editor. They would object that the entire dual-standard debate is conducted by and for people who already hold a seat in one of the two audiences, but they are not consulted by either the evaluator or the attribution system, since the mechanism that would surface their claim is exactly the mechanism that failed.
narrative_ontology:constraint_stakeholder(cooperative_artifact_legitimacy_flat_control, unattributed_originators, excluded,
    powerless, biographical, trapped, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(cooperative_artifact_legitimacy_flat_control, diffuse).
narrative_ontology:fixing_cost_class(cooperative_artifact_legitimacy_flat_control, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Cooperative production of an artifact requires SOME shared standard for what counts as a legitimate output — otherwise evaluators cannot process volume and no one can be held accountable for errors, plagiarism, or fraud. A dual standard (legible AND traceable) genuinely solves a real problem: pure legibility without traceability enables laundering credit and evading accountability; pure traceability without legibility produces artifacts too costly to consume, which collapses the cooperative benefit of division of labor in the first place.
% TRANSFER_FUNCTION: Moves reputational credit, career advancement, and the presumption of legitimacy toward whoever's name sits on the final legible artifact, and moves invisibilized labor cost (the smoothing, editing, and drafting work required to make something legible) onto contributors whose causal trace is erased in that same smoothing process.
% ABSENT_VOICES: Unattributed originators — translators, ghostwriters, junior collaborators, and AI-assisted drafting labor whose work is erased by the legibility pass — are structurally outside the conversation about which failure mode is worse, because the forums that would adjudicate the dispute (editorial boards, credentialing committees, professional norms bodies) are staffed by people who already hold recognized authorial or evaluative seats.
% DISAPPEARANCE_RATIONALE: If the dual standard vanished overnight, evaluators attest the world would rearrange badly (fraud and plagiarism would flood the system, unreadable artifacts would collapse throughput); credited authors and institutions attest their reputational capital would become unverifiable; but junior contributors and editing labor attest the underlying cooperative work itself would proceed largely unchanged — what would disappear is only the enforcement layer that currently decides, in each conflict, whose labor gets erased. The parties disagree because they are answering different questions: whether the coordination function would survive (contested) versus whether the current allocation of who absorbs the cost would survive (most agree it would not).
% FOUNDING_PROBLEM: Cooperative intellectual and creative work scaled beyond what any single evaluator could verify by direct observation of the production process, creating a need for proxies: legibility as a proxy for quality/effort, and attribution as a proxy for accountability/trust, because directly auditing every contributor's actual causal role in every artifact is infeasible at scale.
% FOUNDING_PROBLEM_CORROBORATION: Evaluators and credentialing institutions attest the founding problem (verification at scale) remains fully live and justifies the current dual standard. Independent sociology-of-science researchers and authorship-ethics scholars — a source outside both benefiting audiences — corroborate that the verification problem is real but argue the specific allocation rule (legibility defaults to trusted, provenance audited only on suspicion) was never derived from the founding problem itself; it was a convenience settlement that happens to favor whichever party already controls legibility, and no outside corroborating source has been found who defends the CURRENT allocation as the necessary solution to the founding problem, as opposed to merely a possible one.
narrative_ontology:disappearance_verdict(cooperative_artifact_legitimacy_flat_control, contested).
narrative_ontology:founding_problem_status(cooperative_artifact_legitimacy_flat_control, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(cooperative_artifact_legitimacy_flat_control, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-25',
    'unspecified', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'unspecified').
narrative_ontology:story_seed(cooperative_artifact_legitimacy_flat_control, 'none', 1).
narrative_ontology:epsilon_provenance(cooperative_artifact_legitimacy_flat_control, 0.58, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(cooperative_artifact_legitimacy_flat_control_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(cooperative_artifact_legitimacy_flat_control, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(cooperative_artifact_legitimacy_flat_control_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58) is authored as moderate-high and rising: the coordination function is real but the specific resolution rule (default-trust legibility, audit-only-on-suspicion provenance) increasingly externalizes cost onto low-power contributors as cooperative production scales and AI-assisted drafting makes the legibility/provenance tension sharper. Suppression (0.47) reflects that the standard is enforced through institutional gatekeeping (peer review, hiring, credentialing) rather than raw coercion — contributors are not physically prevented from claiming credit, but the career and reputational infrastructure makes contesting the default costly. Theater ratio (0.42) and its rising trajectory capture that an increasing share of 'legitimacy verification' activity (plagiarism checkers, AI-detection tools, disclosure statements) is performative compliance theater layered atop the underlying unresolved tension, rather than genuine resolution of who did the causal work. Accessibility collapse (0.4) is moderate: alternative norms (open collaborative attribution, contributor-role taxonomies like CRediT) exist and are gaining ground, so alternatives have not fully collapsed. Resistance (0.55) is substantial: contributor-rights movements, authorship-reform advocacy, and AI-disclosure norms are actively contesting the current default.
 *
 * PERSPECTIVAL GAP:
 *   From the evaluator/gatekeeper seat, the dual standard is functioning coordination — a workable, if imperfect, solution to a genuine scale problem, and its cost is a reasonable overhead. From the junior contributor and editing-labor seats, the SAME structure is experienced as extraction: their actual causal contribution is structurally invisible whenever legibility work and attribution work pull in opposite directions, and the rule for resolving that pull was never negotiated with them. The engine's per-seat computation should reflect this: institutional/arbitrage seats see low effective extraction (they can walk to whichever artifact clears their bar); powerless/constrained seats see high effective extraction from the identical rule.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (evaluators, credentialing institutions, authors of record, consumers) are declared with low-to-moderate directionality because the arrangement either subsidizes their throughput (evaluators), their trust rents (institutions), their captured credit (authors of record), or their consumption costs (consumers) without requiring them to absorb the cost of reconciling legibility and provenance. Victims (junior contributors, collaborative/editing labor) are declared with high directionality because the identical mechanism that produces legibility for the evaluator is the mechanism that erases their causal trace for the attribution system — they pay through the same structure that pays others. Unattributed originators are excluded rather than victimized in the technical sense used for the gate, since their situation is not merely costly but definitionally erased from the record the constraint operates on.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (verification at scale requires proxies because direct observation of every contributor's causal role is infeasible) remains genuinely live — this prevents a lazy dismissal of the whole arrangement as pure extraction. But the founding problem's continued liveness does not corroborate the CURRENT allocation rule (legibility defaults to trusted, provenance audited only on challenge) as the necessary solution; independent authorship-ethics scholarship corroborates the problem but not the specific settlement. This is exactly the mandatrophy pattern to watch for: a real founding problem justifying a mandate that has since ossified into a specific cost-allocation rule that outlived any demonstration that it was the only or best way to solve that problem.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    legibility_or_provenance_as_true_value_site,
    'Is the artifact''s surface legibility or its causal/authorial history the true site of value that legitimizes cooperative work — and therefore which failure mode (''ghost-written'' or ''unreadable'') is the real cost the system should be optimized against?',
    'No empirical resolution mechanism exists because this is fundamentally a question about which harm a community of practice chooses to weight more heavily — it is closer to a values dispute than a factual one, though comparative institutional case studies (fields that have shifted toward contributor-role taxonomies like CRediT vs. fields that have not) could show which allocation produces fewer downstream disputes or retractions.',
    'If legibility is treated as the true site of value, the current default (trust legible artifacts, audit provenance only on suspicion) is structurally correct and the extraction on erased contributors is a tolerable externality. If causal/authorial history is treated as the true site of value, the current default inverts the correct burden of proof and the constraint is closer to a snare wearing a coordination costume.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(legibility_or_provenance_as_true_value_site, preference, 'Whether legibility or provenance is the true site of artifact value — the substrate''s core contested premise.').

omega_variable(
    smoothing_labor_visibility_tradeoff,
    'Is it structurally possible to make an artifact simultaneously more legible AND more traceable to its actual contributors, or is there a genuine trade-off where polishing for consumption necessarily erases marks of who did the polishing?',
    'Track adoption and outcomes of contributor-role taxonomies (CRediT, git blame-style provenance chains, versioned edit histories) that attempt to preserve both properties simultaneously; if these consistently succeed without degrading either legibility or traceability, the trade-off is not structural but a product of insufficient tooling/norms.',
    'If the trade-off is genuinely structural, some erasure of contributor labor is an irreducible cost of any legible cooperative artifact, and the constraint is closer to a tangled rope with an unavoidable extractive component. If the trade-off is merely a tooling/norms failure, the current allocation is closer to a snare — extraction that persists because no one with power has incentive to fix the tooling.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(smoothing_labor_visibility_tradeoff, empirical, 'Whether the legibility/provenance tension is structurally necessary or a fixable artifact of current tooling and norms.').

omega_variable(
    ai_assisted_drafting_shift,
    'Does the rise of AI-assisted drafting change which reading of the shared commitment is dominant, by making the ''ghost-written'' failure mode dramatically cheaper to produce at scale while leaving the ''unreadable'' failure mode''s cost structure unchanged?',
    'Longitudinal tracking of disclosure rates, retraction rates, and attribution disputes in fields with high AI-assisted drafting adoption versus low adoption, over the next several years.',
    'If AI-assisted drafting sharply increases ghost-writing at near-zero marginal cost, the reading that treats attribution-erosion as the real cost gains empirical support and pressure toward provenance-first standards should intensify; if disclosure norms absorb the shift without disputes rising, the legibility-first reading retains its current dominance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ai_assisted_drafting_shift, empirical, 'Whether AI-assisted drafting is shifting the relative costs of the two contested failure modes.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(cooperative_artifact_legitimacy_flat_control, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(coop_tr_t0, cooperative_artifact_legitimacy_flat_control, theater_ratio, 0, 0.2).
narrative_ontology:measurement(coop_tr_t8, cooperative_artifact_legitimacy_flat_control, theater_ratio, 8, 0.26).
narrative_ontology:measurement(coop_tr_t16, cooperative_artifact_legitimacy_flat_control, theater_ratio, 16, 0.32).
narrative_ontology:measurement(coop_tr_t24, cooperative_artifact_legitimacy_flat_control, theater_ratio, 24, 0.36).
narrative_ontology:measurement(coop_tr_t32, cooperative_artifact_legitimacy_flat_control, theater_ratio, 32, 0.39).
narrative_ontology:measurement(coop_tr_t40, cooperative_artifact_legitimacy_flat_control, theater_ratio, 40, 0.42).

% Extraction over time
narrative_ontology:measurement(coop_be_t0, cooperative_artifact_legitimacy_flat_control, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(coop_be_t8, cooperative_artifact_legitimacy_flat_control, base_extractiveness, 8, 0.44).
narrative_ontology:measurement(coop_be_t16, cooperative_artifact_legitimacy_flat_control, base_extractiveness, 16, 0.49).
narrative_ontology:measurement(coop_be_t24, cooperative_artifact_legitimacy_flat_control, base_extractiveness, 24, 0.53).
narrative_ontology:measurement(coop_be_t32, cooperative_artifact_legitimacy_flat_control, base_extractiveness, 32, 0.56).
narrative_ontology:measurement(coop_be_t40, cooperative_artifact_legitimacy_flat_control, base_extractiveness, 40, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(coop_su_t0, cooperative_artifact_legitimacy_flat_control, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(coop_su_t8, cooperative_artifact_legitimacy_flat_control, suppression_requirement, 8, 0.35).
narrative_ontology:measurement(coop_su_t16, cooperative_artifact_legitimacy_flat_control, suppression_requirement, 16, 0.39).
narrative_ontology:measurement(coop_su_t24, cooperative_artifact_legitimacy_flat_control, suppression_requirement, 24, 0.42).
narrative_ontology:measurement(coop_su_t32, cooperative_artifact_legitimacy_flat_control, suppression_requirement, 32, 0.45).
narrative_ontology:measurement(coop_su_t40, cooperative_artifact_legitimacy_flat_control, suppression_requirement, 40, 0.47).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(cooperative_artifact_legitimacy_flat_control, identity_coordination).
narrative_ontology:boltzmann_floor_override(cooperative_artifact_legitimacy_flat_control, 0.08).

% DUAL FORMULATION NOTE:
% This story is authored FLAT, as a single constraint, per the construction-perturbation control instruction: it does NOT decompose the substrate into a legibility-primacy reading and a provenance-primacy reading, and carries no reading_relations or axioms in cs_structure. It is deliberately structured to be comparable against sibling stories that DO decompose the same substrate into separate kernel readings, to test whether flat vs. decomposed construction changes the computed classification.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
