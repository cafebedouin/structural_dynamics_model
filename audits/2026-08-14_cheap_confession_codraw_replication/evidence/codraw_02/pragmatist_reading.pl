% ============================================================================
% CONSTRAINT STORY: pragmatist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_pragmatist_reading, []).

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
    narrative_ontology:suppression_profile/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
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
 *   constraint_id: pragmatist_reading
 *   human_readable: Pragmatist Reading: Disagreement as Provisional Data in Corrigible Inquiry
 *   domain: epistemology/philosophy_of_technology/institutional_analysis
 *
 * SUMMARY:
 *   This story instantiates the pragmatist reading of the kernel 'positional
 *   disagreement as evidence': disagreement is treated as provisional data
 *   within an ongoing, corrigible inquiry process. No position enjoys a
 *   priori standing advantage — not the standpoint-holder's lived position,
 *   not the incumbent's procedural conformity, not any party's declared
 *   authority. What determines which disagreements actually get resolved is a
 *   practical bottleneck: the cost of self-audit, the incentive structure
 *   governing propagation of findings, and institutions' capacity to formally
 *   acknowledge convergence. Truth, on this reading, is whatever indefinite
 *   inquiry converges toward; any present declaration is a procedural stopgap
 *   awaiting revision, not an epistemic privilege. The coordination function
 *   is real — it keeps disputants talking rather than exiting into parallel,
 *   non-communicating camps — but the same bottleneck mechanism that makes
 *   the reading practically workable also silently favors whichever party can
 *   afford the audit and propagation costs, producing a soft, non-declared
 *   asymmetry that never has to justify itself as such.
 *
 * KEY AGENTS:
 *   - well_resourced_research_institutions: agenda_setter (institutional/arbitrage) — controls the bottleneck resources that decide which disputes resolve
 *   - under_resourced_challengers_lacking_self_audit_capacity: payer (powerless/constrained) — holds an equally live position but cannot afford entry into the resolution pipeline
 *   - long_horizon_inquirers: beneficiary (moderate/mobile) — rewarded for treating disagreement as revisable data
 *   - positions_stalled_by_propagation_incentives: payer/excluded (powerless/trapped) — stalled by incentive structures unrelated to evidential merit
 *   - institutions_with_acknowledgment_capacity: beneficiary/agenda_setter (institutional/arbitrage) — controls the last-mile conversion of converged inquiry into recognized fact
 *   - declarationist_incumbents: excluded (organized/constrained) — sidelined sibling-reading holders whose standing claims are denied a priori weight
 *   - philosophers_of_inquiry: observer (analytical/analytical) — traces the bottleneck mechanism without adjudicating between readings
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(pragmatist_reading, 0.28).
domain_priors:suppression_score(pragmatist_reading, 0.22).
domain_priors:theater_ratio(pragmatist_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(pragmatist_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(pragmatist_reading, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(pragmatist_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(pragmatist_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(pragmatist_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(pragmatist_reading, rope).
narrative_ontology:human_readable(pragmatist_reading, "Pragmatist Reading: Disagreement as Provisional Data in Corrigible Inquiry").
narrative_ontology:topic_domain(pragmatist_reading, "epistemology/philosophy_of_technology/institutional_analysis").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(pragmatist_reading, '504eeab0-88a3-4907-943e-dab58d0c9143').
narrative_ontology:cs_kernel_codification('504eeab0-88a3-4907-943e-dab58d0c9143', distributed).
narrative_ontology:cs_authority_grounding('504eeab0-88a3-4907-943e-dab58d0c9143', distributed).
narrative_ontology:cs_reading_relation('504eeab0-88a3-4907-943e-dab58d0c9143', pragmatist_reading__standpoint_reading, coexists_with).
narrative_ontology:cs_reading_relation('504eeab0-88a3-4907-943e-dab58d0c9143', pragmatist_reading__proceduralist_reading, influences).
narrative_ontology:cs_reading_relation('504eeab0-88a3-4907-943e-dab58d0c9143', pragmatist_reading__instrumentalist_reading, coexists_with).
narrative_ontology:cs_axiom('504eeab0-88a3-4907-943e-dab58d0c9143', foundational, no_position_has_a_priori_standing_advantage).
narrative_ontology:cs_axiom_status(no_position_has_a_priori_standing_advantage, holdable).
narrative_ontology:cs_axiom_grounding('504eeab0-88a3-4907-943e-dab58d0c9143', no_position_has_a_priori_standing_advantage, conventional).
narrative_ontology:cs_axiom('504eeab0-88a3-4907-943e-dab58d0c9143', foundational, truth_is_the_limit_of_indefinite_inquiry_convergence).
narrative_ontology:cs_axiom_status(truth_is_the_limit_of_indefinite_inquiry_convergence, holdable).
narrative_ontology:cs_axiom_grounding('504eeab0-88a3-4907-943e-dab58d0c9143', truth_is_the_limit_of_indefinite_inquiry_convergence, instrumental).
narrative_ontology:cs_axiom('504eeab0-88a3-4907-943e-dab58d0c9143', secondary, declaration_is_procedural_stopgap_not_epistemic_privilege).
narrative_ontology:cs_axiom_status(declaration_is_procedural_stopgap_not_epistemic_privilege, holdable).
narrative_ontology:cs_axiom_grounding('504eeab0-88a3-4907-943e-dab58d0c9143', declaration_is_procedural_stopgap_not_epistemic_privilege, conventional).
narrative_ontology:cs_reference_frame('504eeab0-88a3-4907-943e-dab58d0c9143', corrigible_inquiry_baseline).
narrative_ontology:cs_drift_state('504eeab0-88a3-4907-943e-dab58d0c9143', contemporary_institutional_science, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('504eeab0-88a3-4907-943e-dab58d0c9143', '').
narrative_ontology:cs_kernel_id(pragmatist_reading, positional_disagreement_as_evidence).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(pragmatist_reading, research_communities_with_low_audit_cost).
narrative_ontology:constraint_beneficiary(pragmatist_reading, institutions_with_acknowledgment_capacity).
narrative_ontology:constraint_beneficiary(pragmatist_reading, long_horizon_inquirers).
narrative_ontology:constraint_victim(pragmatist_reading, positions_stalled_by_propagation_incentives).
narrative_ontology:constraint_victim(pragmatist_reading, under_resourced_challengers_lacking_self_audit_capacity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets the practical terms on which disputes actually get investigated — funds replication, hosts adjudicating venues, decides which self-audits are affordable to run. Because it controls the bottleneck resources (money, staff time, publication infrastructure), it effectively decides which disagreements move toward resolution and which stay open indefinitely, without claiming any a priori epistemic privilege for its own position.
narrative_ontology:constraint_stakeholder(pragmatist_reading, well_resourced_research_institutions, agenda_setter,
    institutional, generational, arbitrage, global).

% Holds a position that could in principle be adjudicated by continued inquiry, but lacks the money, staff, or institutional standing to run the self-audits or propagate findings that would let the position enter the resolution pipeline. On pragmatist terms their claim has no lesser epistemic standing, but it never gets the chance to be tested against the bottleneck resource.
narrative_ontology:constraint_stakeholder(pragmatist_reading, under_resourced_challengers_lacking_self_audit_capacity, payer,
    powerless, biographical, constrained, national).

% Researchers and institutions committed to indefinite inquiry as the truth-standard benefit from a framework that treats every declared position as revisable and keeps channels for revision open; they are rewarded (in credibility, funding, longevity of their program) for treating disagreement as data rather than as settled or as grievance.
narrative_ontology:constraint_stakeholder(pragmatist_reading, long_horizon_inquirers, beneficiary,
    moderate, civilizational, mobile, global).

% Holds a minority or unfashionable position whose propagation is disincentivized by career risk, publication bias, or funding structures unrelated to its evidential merit. Under the pragmatist framing this is a bottleneck problem, not a verdict on the position's truth, but from inside the stalled position the practical effect is indistinguishable from being wrong — it simply never gets carried forward by inquiry's ordinary propagation channels.
narrative_ontology:constraint_stakeholder(pragmatist_reading, positions_stalled_by_propagation_incentives, payer,
    powerless, generational, trapped, national).
narrative_ontology:stakeholder_secondary_role(pragmatist_reading, positions_stalled_by_propagation_incentives, excluded).

% Journals, standards bodies, and disciplinary societies that can formally acknowledge a resolved disagreement (retraction, consensus statement, revised standard) hold the last-mile capacity that converts converged inquiry into recognized fact. They benefit from being the recognized clearinghouse and can also delay or withhold acknowledgment without violating the pragmatist framework's own terms, since 'not yet converged' is always available as a description.
narrative_ontology:constraint_stakeholder(pragmatist_reading, institutions_with_acknowledgment_capacity, beneficiary,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(pragmatist_reading, institutions_with_acknowledgment_capacity, agenda_setter).

% Holders of standpoint-privilege or proceduralist-authority claims who believe some positions should carry weight independent of inquiry's eventual verdict are structurally sidelined by this reading's insistence that no position has standing advantage a priori; they would object that the pragmatist frame itself functions as a procedural device that favors whoever currently controls audit and propagation resources, but that objection is not staged within this reading.
narrative_ontology:constraint_stakeholder(pragmatist_reading, declarationist_incumbents, excluded,
    organized, biographical, constrained, national).

% Analyze the pragmatist framework itself, tracing how bottleneck resources rather than epistemic merit determine which disagreements resolve, without taking a position on whether the pragmatist reading, the standpoint reading, or the proceduralist reading better describes actual practice.
narrative_ontology:constraint_stakeholder(pragmatist_reading, philosophers_of_inquiry, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared procedural stance — treat disagreement as ongoing, revisable, uncorrelated a priori with any party's identity or institutional position — that lets participants in a dispute keep talking to each other and keep the door open to revision rather than treating disagreement as terminal or as grounds for exit.
% TRANSFER_FUNCTION: Moves adjudicating attention and resources (funding for replication, journal space, standard-setting capacity) toward whichever disputants can meet the practical bottleneck costs of self-audit and propagation, and away from disputants who hold equally live positions but lack those resources — without formally declaring the latter wrong.
% ABSENT_VOICES: Standpoint-reading advocates who hold that some positions carry epistemic weight from lived structural position independent of inquiry's eventual verdict, and proceduralist-reading advocates who hold that a position's standing is set by procedural conformity rather than convergence, are not represented inside this reading's own terms — they appear here only as sibling readings, not as objections this reading answers.
% DISAPPEARANCE_RATIONALE: If the pragmatist framing vanished overnight, well-resourced institutions and long-horizon inquirers would likely default to something functionally similar (provisional, revisable inquiry is close to how they already operate), so for them little would visibly rearrange. But stalled and under-resourced positions would lose the one framing that formally denies any a priori disadvantage to their claims; whether that loss is real or merely rhetorical is exactly what the contest is about, which is why the verdict itself is contested rather than settled in either direction.
% FOUNDING_PROBLEM: Disputes in inquiry-driven fields (science, technology assessment, institutional epistemics) were being resolved either by brute appeal to who spoke first, who held formal authority, or by treating persistent disagreement as evidence of bad faith on one side; the founding problem was to give disagreement a legitimate ongoing status — as data to be worked through — rather than forcing premature declaration of a winner.
% FOUNDING_PROBLEM_CORROBORATION: Working scientists and methodologists outside any single benefiting camp (e.g., replication-crisis researchers documenting how funding and publication bottlenecks rather than evidential merit determine which disputes get resolved) corroborate that the bottleneck-driven resolution problem the pragmatist reading names is real and ongoing; they do not uniformly endorse the pragmatist reading's own normative conclusion that no position has a priori standing advantage.
narrative_ontology:disappearance_verdict(pragmatist_reading, contested).
narrative_ontology:founding_problem_status(pragmatist_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(pragmatist_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-14',
    'unspecified', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'unspecified').
narrative_ontology:story_seed(pragmatist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(pragmatist_reading, 0.28, 'claude-sonnet-5', 'cheap_confession_2026_20260814_151329', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(pragmatist_reading_tests).
:- end_tests(pragmatist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is low-to-moderate (0.28 at interval end) because the pragmatist reading's core function — keeping disagreement open and revisable — is genuinely coordinative rather than extractive on its face; there is no declared victim class being formally silenced, only a diffuse and growing asymmetry in whose disagreements actually get resolved as the bottleneck costs (audit, propagation, acknowledgment capacity) accumulate advantage for already-resourced parties over time. Suppression is comparatively low (0.22) because nothing in this reading formally forecloses any position — the mechanism is exclusion by resource gradient, not by rule. Theater ratio rises modestly (0.18 to 0.30) reflecting a slow drift where 'still under inquiry' becomes a standing excuse institutions use to defer acknowledgment indefinitely rather than a genuine holding pattern, without yet crossing into dominant theater.
 *
 * DIRECTIONALITY LOGIC:
 *   Well-resourced institutions and institutions with acknowledgment capacity sit near the beneficiary end: they set the practical terms of resolution and collect the credibility and standing that comes with being the clearinghouse, without needing to claim formal epistemic privilege. Long-horizon inquirers benefit structurally from a framework matched to their own practice. Under-resourced challengers and propagation-stalled positions sit near the target end: their positions are formally undiminished but practically unable to enter the resolution pipeline, so the bottleneck operates on them as a soft extraction of standing even though no one declares them wrong.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification prevents mislabeling this as a pure snare (declarationist critics might claim the pragmatist frame is just cover for resourced parties to indefinitely defer any conclusion inconvenient to them) or as a pure rope (defenders might claim the absence of formal declaration means no extraction occurs at all). The rope-with-drift picture captures both: the coordination function (keeping inquiry open, denying a priori privilege) is real and worth preserving, while the practical bottleneck asymmetry is a genuine, measurable cost that the reading's own terms do not require anyone to name as extraction — which is precisely the risk. Because there is no fixed victim/beneficiary set by design, the failure mode this reading actually needs to guard against is not corrective silence (extraction that persists because no one can name it) but premature closure — treating a still-open inquiry as settled before genuine convergence, in either direction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    bottleneck_neutrality_vs_capture,
    'Is the practical bottleneck (self-audit cost, propagation incentive, acknowledgment capacity) a neutral fact about inquiry''s material conditions, or is it itself shaped by the very parties who benefit from controlling it — making the pragmatist reading''s denial of a priori privilege compatible with a de facto privilege it does not have to name?',
    'Track whether resource allocation to self-audit and propagation infrastructure correlates with the prior institutional standing of the party proposing a position, across a sample of resolved and unresolved disputes; a strong correlation would indicate capture rather than neutral bottleneck.',
    'If the bottleneck is captured, the rope classification understates an embedded tangled-rope structure — coordination riding on an unacknowledged asymmetric extraction. If the bottleneck is genuinely neutral (scarcity-driven, not capture-driven), the rope classification is closer to correct and the residual asymmetry is a coordination cost rather than extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(bottleneck_neutrality_vs_capture, empirical, 'Whether bottleneck control is neutral scarcity or structural capture by incumbents.').

omega_variable(
    convergence_criterion_underdetermination,
    'What counts as ''indefinite inquiry converging'' — is there any operational criterion for convergence that isn''t itself set by the institutions with acknowledgment capacity, or does ''not yet converged'' function as an unfalsifiable holding pattern?',
    'Examine historical cases where a discipline declared convergence: was the declaration triggered by a pre-specified, community-agreed threshold, or was it triggered by the acknowledging institution''s own timing judgment with no external criterion?',
    'If convergence has no criterion independent of the acknowledging institution''s discretion, the theater_ratio drift documented in measurements is likely to continue rising, and ''still under inquiry'' risks becoming a permanent deferral mechanism rather than a genuine epistemic state.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(convergence_criterion_underdetermination, conceptual, 'Whether convergence has an operational criterion independent of the institutions that declare it.').

omega_variable(
    reading_selection_as_meta_declaration,
    'Is choosing the pragmatist reading over the standpoint or proceduralist readings itself an act of declaration that this reading''s own framework says cannot carry a priori epistemic privilege — i.e., does adopting this reading performatively contradict its own core claim?',
    'This is a structural/conceptual question about self-reference in the kernel''s reading-space, not resolvable by further data collection; it can only be clarified by formal analysis of whether reading-selection is itself inside or outside the scope of ''positions with no a priori standing.''',
    'If reading-selection is inside scope, the pragmatist reading is self-undermining in a way that would push toward the proceduralist reading (which at least specifies how selection should be procedurally justified). If outside scope, the self-reference concern dissolves and the reading is internally stable.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_selection_as_meta_declaration, conceptual, 'Whether the pragmatist reading''s own selection is subject to its no-a-priori-privilege principle.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(pragmatist_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(prag_tr_t0, pragmatist_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement(prag_tr_t8, pragmatist_reading, theater_ratio, 8, 0.2).
narrative_ontology:measurement(prag_tr_t16, pragmatist_reading, theater_ratio, 16, 0.23).
narrative_ontology:measurement(prag_tr_t24, pragmatist_reading, theater_ratio, 24, 0.26).
narrative_ontology:measurement(prag_tr_t32, pragmatist_reading, theater_ratio, 32, 0.28).
narrative_ontology:measurement(prag_tr_t40, pragmatist_reading, theater_ratio, 40, 0.3).

% Extraction over time
narrative_ontology:measurement(prag_be_t0, pragmatist_reading, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(prag_be_t8, pragmatist_reading, base_extractiveness, 8, 0.2).
narrative_ontology:measurement(prag_be_t16, pragmatist_reading, base_extractiveness, 16, 0.23).
narrative_ontology:measurement(prag_be_t24, pragmatist_reading, base_extractiveness, 24, 0.25).
narrative_ontology:measurement(prag_be_t32, pragmatist_reading, base_extractiveness, 32, 0.27).
narrative_ontology:measurement(prag_be_t40, pragmatist_reading, base_extractiveness, 40, 0.28).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(pragmatist_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(pragmatist_reading, information_standard).
narrative_ontology:boltzmann_floor_override(pragmatist_reading, 0.05).
narrative_ontology:affects_constraint(pragmatist_reading, standpoint_reading).
narrative_ontology:affects_constraint(pragmatist_reading, proceduralist_reading).
narrative_ontology:affects_constraint(pragmatist_reading, instrumentalist_reading).

% DUAL FORMULATION NOTE:
% This story is one of four sibling readings of the kernel 'positional_disagreement_as_evidence.' Each reading instantiates a structurally distinct constraint with its own epsilon, beneficiary/victim structure, and classification: the pragmatist_reading (this file) treats disagreement as provisional data in indefinite corrigible inquiry gated by practical bottlenecks (rope-leaning, low epsilon, failure mode = premature closure); the standpoint_reading holds some positions carry epistemic weight from structural position independent of convergence (expected higher epsilon, explicit victim set, failure mode = corrective silence); the proceduralist_reading grounds standing in procedural conformity rather than convergence (expected tangled-rope shape, enforcement-heavy); the instrumentalist_reading treats resolution as serving operative institutional purposes rather than truth-tracking per se (expected higher extraction, purpose-capture risk). All four share the same underlying kernel text and diverge only in how standing and resolution authority are grounded — per the epsilon-invariance principle each is authored as its own file with its own epsilon rather than as one story with a measurement parameter.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
