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
 *   human_readable: Positional Disagreement as Epistemic Evidence (Standing Commitment)
 *   domain: epistemology/philosophy_of_technology/institutional_analysis
 *
 * SUMMARY:
 *   This story authors, as a single flat constraint, the standing commitment
 *   that a disagreement between two honest, differently-positioned observers
 *   of the same arrangement counts as legitimate evidence about the
 *   arrangement rather than subjectivity to be corrected toward a neutral
 *   view. The commitment began as a genuine epistemic correction to a real
 *   problem: positional testimony (from workers, patients, colonized peoples,
 *   minorities) was routinely discounted and averaged away, discarding real
 *   information those positions uniquely accessed. Over time the same
 *   commitment has also become invocable to protect claims that are, in fact,
 *   empirically adjudicable, converting disagreements that could be resolved
 *   by better data or triangulation into permanently 'legitimate' standoffs.
 *   The constraint is authored flat: one ε, one set of stakeholders, one
 *   classification, with the contestation located in perspectival divergence
 *   across seats and in the omegas, not decomposed into separate readings.
 *
 * KEY AGENTS:
 *   - frontline_positional_reporters: primary beneficiary (moderate/constrained) — protected testimony
 *   - standpoint_epistemology_scholars: agenda_setter (organized/identity_locked) — administers and is constitutively invested in the commitment
 *   - institutional_decision_makers: primary payer (institutional/trapped) — must act under irresolvable disagreement
 *   - affected_third_parties_awaiting_resolution: diffuse payer (powerless/trapped) — bears the cost of delayed resolution
 *   - philosophers_of_science: analytical observer — distinguishes genuine perspectival evidence from rhetorical shielding
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(positional_disagreement_as_evidence_flat_control, 0.42).
domain_priors:suppression_score(positional_disagreement_as_evidence_flat_control, 0.38).
domain_priors:theater_ratio(positional_disagreement_as_evidence_flat_control, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(positional_disagreement_as_evidence_flat_control, extractiveness, 0.42).
narrative_ontology:constraint_metric(positional_disagreement_as_evidence_flat_control, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(positional_disagreement_as_evidence_flat_control, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(positional_disagreement_as_evidence_flat_control, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(positional_disagreement_as_evidence_flat_control, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(positional_disagreement_as_evidence_flat_control, tangled_rope).
narrative_ontology:human_readable(positional_disagreement_as_evidence_flat_control, "Positional Disagreement as Epistemic Evidence (Standing Commitment)").
narrative_ontology:topic_domain(positional_disagreement_as_evidence_flat_control, "epistemology/philosophy_of_technology/institutional_analysis").

domain_priors:requires_active_enforcement(positional_disagreement_as_evidence_flat_control).

% --- Construction-pair linkage (forced-flat control of a kernel) ---
narrative_ontology:flat_control_of(positional_disagreement_as_evidence_flat_control, positional_disagreement_as_evidence).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(positional_disagreement_as_evidence_flat_control, frontline_positional_reporters).
narrative_ontology:constraint_beneficiary(positional_disagreement_as_evidence_flat_control, qualitative_researchers).
narrative_ontology:constraint_beneficiary(positional_disagreement_as_evidence_flat_control, standpoint_epistemology_scholars).
narrative_ontology:constraint_beneficiary(positional_disagreement_as_evidence_flat_control, advocacy_organizations).
narrative_ontology:constraint_victim(positional_disagreement_as_evidence_flat_control, institutional_decision_makers).
narrative_ontology:constraint_victim(positional_disagreement_as_evidence_flat_control, cross_context_synthesizers).
narrative_ontology:constraint_victim(positional_disagreement_as_evidence_flat_control, affected_third_parties_awaiting_resolution).
narrative_ontology:constraint_vindicates(positional_disagreement_as_evidence_flat_control, standpoint_evidential_value_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Report what they see from their position — a worker describing shop-floor conditions, a patient describing treatment, a resident describing a policy's effect. The commitment guarantees their report cannot be dismissed as mere bias or averaged into a synthetic 'view from nowhere'; it must be weighed as a datum about the arrangement. This protects them from having their testimony discounted, but gives them no obligation to reconcile with conflicting reports.
narrative_ontology:constraint_stakeholder(positional_disagreement_as_evidence_flat_control, frontline_positional_reporters, beneficiary,
    moderate, biographical, constrained, local).

% Build methodologies (standpoint epistemology, participatory research, positionality statements) around the commitment. They administer its application in academic and institutional review, deciding which disagreements count as evidential and which are noise. Their professional standing is partly constituted by defending the commitment's legitimacy against calls for 'objective' synthesis.
narrative_ontology:constraint_stakeholder(positional_disagreement_as_evidence_flat_control, qualitative_researchers, beneficiary,
    organized, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(positional_disagreement_as_evidence_flat_control, qualitative_researchers, agenda_setter).

% Have built research programs, journals, and tenure cases on the claim that positioned disagreement is evidence rather than error. For this group the commitment is not just useful but constitutive of their intellectual identity — abandoning it would not just cost a grant, it would dissolve the premise of their field. They set the terms under which the commitment is invoked in scholarly and policy contexts.
narrative_ontology:constraint_stakeholder(positional_disagreement_as_evidence_flat_control, standpoint_epistemology_scholars, agenda_setter,
    organized, civilizational, identity_locked, global).

% Use the commitment to insist that testimony from marginalized or affected groups cannot be discounted as anecdote against 'expert' or 'majority' data. It is a lever against dismissal. They benefit from the commitment holding broadly, including in cases where the underlying disagreement may in fact be resolvable by better data rather than genuinely irreducible.
narrative_ontology:constraint_stakeholder(positional_disagreement_as_evidence_flat_control, advocacy_organizations, beneficiary,
    organized, generational, mobile, national).

% Must act — set policy, allocate resources, adjudicate disputes — while multiple honest, differently-positioned reports about the same arrangement conflict and the commitment forbids treating any of them as a correctable bias. They bear the practical cost of irresolution: decisions get delayed, contested, or made under a permanent cloud of legitimate unresolved disagreement, with no principled way to declare any report simply wrong.
narrative_ontology:constraint_stakeholder(positional_disagreement_as_evidence_flat_control, institutional_decision_makers, payer,
    institutional, immediate, trapped, national).

% Meta-analysts, journalists, and policy synthesizers who must produce an actionable account across positions. The commitment removes their traditional tool — treating divergence as measurement error to be averaged out — and replaces it with an obligation to hold contradictory positioned claims as simultaneously valid, which they cannot operationalize into a single recommendation without appearing to violate the commitment they are supposed to honor.
narrative_ontology:constraint_stakeholder(positional_disagreement_as_evidence_flat_control, cross_context_synthesizers, payer,
    moderate, biographical, constrained, global).

% People whose lives depend on the arrangement being adjudicated — e.g., waiting for a policy verdict or remedy — but who are not themselves positioned reporters in the dispute. They bear the cost when the commitment is invoked to keep genuinely resolvable factual disputes open indefinitely as 'legitimate disagreement,' delaying action that would help them.
narrative_ontology:constraint_stakeholder(positional_disagreement_as_evidence_flat_control, affected_third_parties_awaiting_resolution, payer,
    powerless, immediate, trapped, local).

% Study when positional testimony functions as genuine evidence (perspectival access to otherwise unobservable facts) versus when the 'evidence, not bias' framing is used to shield claims from disconfirmation. They can distinguish the commitment's legitimate epistemic core from its use as rhetorical armor, but exercise no enforcement power over how it is invoked.
narrative_ontology:constraint_stakeholder(positional_disagreement_as_evidence_flat_control, philosophers_of_science, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(positional_disagreement_as_evidence_flat_control, diffuse).
narrative_ontology:fixing_cost_class(positional_disagreement_as_evidence_flat_control, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves a real epistemic problem: many arrangements (workplace conditions, medical experience, policy effects, historical events) are only partially observable from any single vantage point, and honest reports from different positions often capture different real facts about the arrangement rather than measurement noise around one true value. Treating positional reports as evidence rather than bias-to-be-corrected coordinates inquiry to actually collect what each position can see instead of discarding it.
% TRANSFER_FUNCTION: Moves epistemic authority and the burden of synthesis: institutional decision-makers, synthesizers, and third parties awaiting resolution lose the ability to declare a dispute settled by appeal to a neutral aggregate, while positioned reporters, the scholars who study them, and advocacy groups gain protection against having their testimony discounted or reduced to a data point averaged toward a majority or 'expert' view.
% ABSENT_VOICES: Third parties who need a decision made and cannot report from a privileged position themselves — patients waiting on a disputed clinical judgment, communities waiting on a disputed environmental assessment — are rarely in the room where the epistemic-normative claim gets invoked; the dispute is conducted between positioned reporters and synthesizers, not by those who bear the cost of non-resolution.
% DISAPPEARANCE_RATIONALE: Standpoint epistemologists and frontline reporters would say the world rearranges badly: real, previously-protected testimony would again be discounted as bias and averaged away, silencing positions with genuine epistemic access. Institutional decision-makers and synthesizers would say the world improves: disputes that are actually resolvable by better data or triangulation would stop being treated as permanently irreducible, and decisions could be made and defended. Which is correct depends on the disputed omega below — whether a given disagreement is genuinely irreducible or merely unresolved.
% FOUNDING_PROBLEM: Traditional epistemic practice discounted testimony from non-dominant or non-expert positions as subjective, biased, or anecdotal, and 'corrected' it toward an ostensibly neutral or majority/expert view — which frequently meant erasing real information that only certain positions could access (workers on shop-floor hazards, patients on subjective treatment effects, colonized peoples on the history of colonization).
% FOUNDING_PROBLEM_CORROBORATION: Historians of science and disability/patient-advocacy researchers outside the standpoint-epistemology community corroborate that positional discounting caused real, documented epistemic losses (e.g., dismissed occupational-hazard reports later vindicated). However, philosophers of science and some empirical psychologists — also outside the benefiting community — corroborate the opposite risk: that the commitment is now sometimes invoked to protect claims that are in fact empirically adjudicable, converting a genuine correction into an all-purpose shield against disconfirmation.
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
narrative_ontology:epsilon_provenance(positional_disagreement_as_evidence_flat_control, 0.42, 'claude-sonnet-5', 'cheap_confession_2026_20260814_151329', direct).

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
 *   Extraction (0.42) is moderate: the commitment genuinely coordinates the collection of positionally-inaccessible information (real function), but is also increasingly used to shield disputable claims from disconfirmation, transferring the cost of non-resolution onto decision-makers, synthesizers, and third parties who cannot themselves invoke a competing position. Suppression (0.38) reflects that appeals to 'that's just averaging away real evidence' function as a soft veto on synthesis attempts — not coercive in the classic sense, but sufficient to make cross-positional aggregation reputationally costly within scholarly and advocacy contexts. Theater ratio (0.28) captures a growing share of invocations that perform epistemic humility without actually contributing to resolving anything resolvable. Accessibility collapse (0.40) is moderate — synthesizers still have alternative moves (triangulation, adjudication by independent data) but each carries reputational risk of being accused of erasing legitimate perspective. Resistance (0.55) is substantial: institutional decision-makers and synthesizers actively push back against blanket applications of the commitment when they believe a dispute is empirically resolvable, and this resistance is a real, ongoing feature of the constraint's operation, not a hypothetical.
 *
 * PERSPECTIVAL GAP:
 *   From the frontline reporter and standpoint-scholar seats the commitment reads as a hard-won correction of a genuine, well-documented epistemic injustice — its persistence is coordination, full stop. From the institutional decision-maker and synthesizer seats the same commitment, invoked without qualification, reads as an extraction of decision-capacity: it removes their principal tool (aggregation toward a best estimate) and replaces it with an unfalsifiable standoff, while the underlying disagreement may in fact be resolvable. Both readings are structurally honest given each seat's position — which is itself an instance of the very phenomenon the constraint describes.
 *
 * DIRECTIONALITY LOGIC:
 *   Frontline reporters, qualitative researchers, standpoint scholars, and advocacy organizations are declared beneficiaries: the commitment protects their testimony/methodology from discounting and gives them leverage in disputes, so directionality sits near the beneficiary end for them. Institutional decision-makers, cross-context synthesizers, and affected third parties are declared victims: they bear the operational cost of irresolvable disagreement without gaining the protective benefit, so directionality sits near the target end. The identity_locked exit option for standpoint_epistemology_scholars reflects that the commitment is constitutive of their professional and intellectual identity, not merely instrumentally useful — abandoning it would dissolve the premise of the field, which is a stronger bind than ordinary professional interest.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (real epistemic injustice from discounting positional testimony) is contested as live vs. dead: it remains genuinely live in domains with documented, repeated vindication of previously-dismissed positional reports (occupational hazards, patient-reported outcomes), but is arguably dead or overextended in domains where the commitment now functions mainly to block adjudication of disputes that better data could resolve. Classifying this as tangled_rope rather than snare preserves the coordination function (real epistemic problem, real solution) while registering the asymmetric extraction (decision-makers and third parties pay a cost the beneficiary seats do not bear) — collapsing to snare would erase the genuine epistemic contribution; collapsing to rope would erase the documented cost of blanket, unqualified invocation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    irreducible_vs_resolvable_disagreement,
    'For any given positional disagreement, is it genuinely irreducible (each position accesses real, non-substitutable information about the arrangement) or is it empirically resolvable (better data, triangulation, or adjudication would settle it, and the ''evidence not bias'' framing is being used to avoid that adjudication)?',
    'Case-by-case investigation: attempt independent triangulation or additional data collection on a sample of invoked disagreements and observe whether the disagreement resolves, narrows, or remains stable in the face of new shared evidence. A disagreement that dissolves under additional shared evidence was resolvable, not irreducible.',
    'If most invoked disagreements turn out empirically resolvable, the constraint functions substantially as a shield against disconfirmation (pushing it toward snare); if most are genuinely irreducible, the constraint functions substantially as intended (pushing it toward rope). The true population mixture determines whether tangled_rope is the right long-run classification or a transitional one.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(irreducible_vs_resolvable_disagreement, empirical, 'Whether positional disagreements invoking the commitment are structurally irreducible or empirically resolvable.').

omega_variable(
    identity_fusion_vs_instrumental_defense,
    'Is the standpoint-scholar community''s defense of the commitment driven by genuine epistemic conviction (instrumental, revisable if evidence changed) or by identity fusion (the commitment is constitutive of professional/intellectual selfhood and therefore effectively non-revisable regardless of evidence)?',
    'Examine whether the community''s application of the commitment narrows or qualifies in response to documented cases of misuse (shielding resolvable disputes), versus whether such cases are absorbed without any adjustment to scope or application criteria.',
    'If instrumental, the commitment can self-correct and the extraction is likely to decline over time as misuse is documented and criticized; if identity-fused, the commitment is likely to persist and even intensify defensively regardless of documented misuse, supporting the rising extraction/suppression trajectory shown in the measurements.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_fusion_vs_instrumental_defense, conceptual, 'Whether the commitment''s institutional defense is revisable in light of evidence or identity-locked against revision.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression (soft veto on aggregation/synthesis) structural — reputational and institutional costs imposed on synthesizers who attempt aggregation — or internalized — synthesizers and decision-makers have absorbed the norm so thoroughly that they no longer attempt adjudication even where it would be uncontroversial?',
    'Compare synthesizer behavior in contexts with strong external reputational monitoring (public, contested policy disputes) versus low-visibility internal contexts (private organizational decision-making) — if avoidance of adjudication persists even absent external reputational risk, the suppression is at least partly internalized.',
    'If internalized, the effective suppression is higher than the structural measure suggests, since synthesizers will avoid adjudication even where the constraint''s own logic would permit it; if purely structural, removing external reputational pressure would restore synthesizers'' willingness to adjudicate resolvable disputes.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Whether the soft veto on synthesis is externally enforced or has become an internalized professional norm.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(positional_disagreement_as_evidence_flat_control, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(posi_tr_t0, positional_disagreement_as_evidence_flat_control, theater_ratio, 0, 0.1).
narrative_ontology:measurement(posi_tr_t10, positional_disagreement_as_evidence_flat_control, theater_ratio, 10, 0.14).
narrative_ontology:measurement(posi_tr_t20, positional_disagreement_as_evidence_flat_control, theater_ratio, 20, 0.18).
narrative_ontology:measurement(posi_tr_t30, positional_disagreement_as_evidence_flat_control, theater_ratio, 30, 0.22).
narrative_ontology:measurement(posi_tr_t40, positional_disagreement_as_evidence_flat_control, theater_ratio, 40, 0.25).
narrative_ontology:measurement(posi_tr_t50, positional_disagreement_as_evidence_flat_control, theater_ratio, 50, 0.28).

% Extraction over time
narrative_ontology:measurement(posi_be_t0, positional_disagreement_as_evidence_flat_control, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(posi_be_t10, positional_disagreement_as_evidence_flat_control, base_extractiveness, 10, 0.28).
narrative_ontology:measurement(posi_be_t20, positional_disagreement_as_evidence_flat_control, base_extractiveness, 20, 0.33).
narrative_ontology:measurement(posi_be_t30, positional_disagreement_as_evidence_flat_control, base_extractiveness, 30, 0.37).
narrative_ontology:measurement(posi_be_t40, positional_disagreement_as_evidence_flat_control, base_extractiveness, 40, 0.4).
narrative_ontology:measurement(posi_be_t50, positional_disagreement_as_evidence_flat_control, base_extractiveness, 50, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(posi_su_t0, positional_disagreement_as_evidence_flat_control, suppression_requirement, 0, 0.2).
narrative_ontology:measurement(posi_su_t10, positional_disagreement_as_evidence_flat_control, suppression_requirement, 10, 0.24).
narrative_ontology:measurement(posi_su_t20, positional_disagreement_as_evidence_flat_control, suppression_requirement, 20, 0.28).
narrative_ontology:measurement(posi_su_t30, positional_disagreement_as_evidence_flat_control, suppression_requirement, 30, 0.32).
narrative_ontology:measurement(posi_su_t40, positional_disagreement_as_evidence_flat_control, suppression_requirement, 40, 0.35).
narrative_ontology:measurement(posi_su_t50, positional_disagreement_as_evidence_flat_control, suppression_requirement, 50, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(positional_disagreement_as_evidence_flat_control, identity_coordination).
narrative_ontology:boltzmann_floor_override(positional_disagreement_as_evidence_flat_control, 0.1).

% DUAL FORMULATION NOTE:
% Authored as a single flat constraint per the construction-perturbation control instructions: this story does not decompose the commitment into originalist/critical/standpoint readings or any other reading set, and does not populate cs_structure.reading_relations or cs_structure.axioms. Contestation is located entirely in perspectival stakeholder divergence and in the omega variables above.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
