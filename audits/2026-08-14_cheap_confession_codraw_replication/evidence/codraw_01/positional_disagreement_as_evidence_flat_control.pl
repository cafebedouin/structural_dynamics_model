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
 *   human_readable: Positional Disagreement as Legitimate Epistemic Evidence
 *   domain: epistemology/philosophy_of_technology/institutional_analysis
 *
 * SUMMARY:
 *   This constraint captures a single, undecomposed epistemic-normative
 *   commitment: that when two honest observers positioned differently
 *   relative to an arrangement disagree about it, the disagreement itself is
 *   evidence about the arrangement's structure — not error to be averaged,
 *   and not something to be resolved by deferring to a synthesized or neutral
 *   account. Originating in critiques of positivist objectivity (standpoint
 *   epistemology, critical theory, disability studies), the norm now operates
 *   across academic methodology, journalism, regulatory adjudication, HR and
 *   workplace investigation, and public discourse about institutions. The
 *   commitment has a genuine coordination function (recovering evidentiary
 *   content that averaging destroys) and a genuine extraction dynamic
 *   (raising the cost of synthesis and shielding some reports from scrutiny
 *   under the cover of positional standing) — both operating through the same
 *   structural mechanism, which is why this is authored flat as a single
 *   tangled rope rather than decomposed into readings. The contestation is
 *   left where it structurally lands: in perspectival divergence across the
 *   stakeholder seats and in the omegas, not in a reading decomposition.
 *
 * KEY AGENTS:
 *   - standpoint_epistemologists: agenda_setter (institutional/identity_locked) — professional and theoretical stake in the norm's persistence
 *   - marginalized_reporters: primary beneficiary (powerless/trapped) — gains standing that would otherwise be denied
 *   - synthesis_seeking_administrators: primary payer (organized/constrained) — bears the cost of unresolvable positional conflict
 *   - cross_positional_arbiters: payer and displaced role (moderate/constrained) — their traditional synthesizing function is delegitimized
 *   - philosophy_of_science_observers: analytical observer (analytical/analytical) — tracks the norm's operation without a stake
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(positional_disagreement_as_evidence_flat_control, 0.42).
domain_priors:suppression_score(positional_disagreement_as_evidence_flat_control, 0.38).
domain_priors:theater_ratio(positional_disagreement_as_evidence_flat_control, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(positional_disagreement_as_evidence_flat_control, extractiveness, 0.42).
narrative_ontology:constraint_metric(positional_disagreement_as_evidence_flat_control, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(positional_disagreement_as_evidence_flat_control, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(positional_disagreement_as_evidence_flat_control, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(positional_disagreement_as_evidence_flat_control, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(positional_disagreement_as_evidence_flat_control, tangled_rope).
narrative_ontology:human_readable(positional_disagreement_as_evidence_flat_control, "Positional Disagreement as Legitimate Epistemic Evidence").
narrative_ontology:topic_domain(positional_disagreement_as_evidence_flat_control, "epistemology/philosophy_of_technology/institutional_analysis").

domain_priors:requires_active_enforcement(positional_disagreement_as_evidence_flat_control).

% --- Construction-pair linkage (forced-flat control of a kernel) ---
narrative_ontology:flat_control_of(positional_disagreement_as_evidence_flat_control, positional_disagreement_as_evidence).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(positional_disagreement_as_evidence_flat_control, standpoint_epistemologists).
narrative_ontology:constraint_beneficiary(positional_disagreement_as_evidence_flat_control, frontline_practitioners).
narrative_ontology:constraint_beneficiary(positional_disagreement_as_evidence_flat_control, marginalized_reporters).
narrative_ontology:constraint_victim(positional_disagreement_as_evidence_flat_control, synthesis_seeking_administrators).
narrative_ontology:constraint_victim(positional_disagreement_as_evidence_flat_control, consensus_dependent_institutions).
narrative_ontology:constraint_victim(positional_disagreement_as_evidence_flat_control, cross_positional_arbiters).
narrative_ontology:constraint_vindicates(positional_disagreement_as_evidence_flat_control, standpoint_epistemology_thesis).
narrative_ontology:constraint_vindicates(positional_disagreement_as_evidence_flat_control, anti_view_from_nowhere_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Develop, teach, and adjudicate the norm that a report from a social or structural position is a datum rather than noise. Their academic and professional standing is built on this commitment; entire subfields (standpoint theory, participatory research, critical ethnography) depend on the norm being upheld as a methodological principle rather than dismissed as advocacy.
narrative_ontology:constraint_stakeholder(positional_disagreement_as_evidence_flat_control, standpoint_epistemologists, agenda_setter,
    institutional, generational, identity_locked, national).

% Workers, patients, residents, or employees whose lived reports about an arrangement (a workplace, a policy, a system) are validated as legitimate evidence under this norm rather than requiring correction toward an official or averaged account. The norm gives their testimony standing it would otherwise lack.
narrative_ontology:constraint_stakeholder(positional_disagreement_as_evidence_flat_control, frontline_practitioners, beneficiary,
    moderate, biographical, constrained, local).

% Individuals whose position within an arrangement is structurally disadvantaged (minority employees, colonized populations, disabled patients) and whose reports have historically been discounted as bias. The norm is their primary lever for having their account count without being averaged into a majority or 'neutral' consensus that erases the disadvantage the report is about.
narrative_ontology:constraint_stakeholder(positional_disagreement_as_evidence_flat_control, marginalized_reporters, beneficiary,
    powerless, biographical, trapped, local).

% Managers, policymakers, and institutional decision-makers who must act on a single course of action despite multiple legitimated but conflicting positional reports. They bear the cost of paralysis or of adjudicating between reports that the norm forbids them from simply averaging or deferring past — every synthesis they attempt can be challenged as illegitimately overriding a standpoint.
narrative_ontology:constraint_stakeholder(positional_disagreement_as_evidence_flat_control, synthesis_seeking_administrators, payer,
    organized, immediate, constrained, national).

% Courts, regulatory bodies, journalistic outlets, and scientific review panels whose legitimacy depends on producing an adjudicated, actionable finding. The norm raises the evidentiary bar and the political cost of any finding that appears to privilege one positional report over another, slowing or destabilizing their core function.
narrative_ontology:constraint_stakeholder(positional_disagreement_as_evidence_flat_control, consensus_dependent_institutions, payer,
    institutional, generational, constrained, national).

% Mediators, judges, and neutral-seeking analysts whose professional function is precisely the synthesis or averaging the norm delegitimizes. Their traditional epistemic role — producing a view that transcends any single position — is treated under the norm as a suspect move that erases real evidence rather than as a legitimate resolution technique.
narrative_ontology:constraint_stakeholder(positional_disagreement_as_evidence_flat_control, cross_positional_arbiters, payer,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(positional_disagreement_as_evidence_flat_control, cross_positional_arbiters, excluded).

% Actors who invoke positional standing strategically to insulate a false or self-serving report from scrutiny, knowing the norm makes cross-examining a position's honesty look like an attack on the position itself. Not a formally recognized party to the norm's operation, but a structural byproduct the norm has difficulty distinguishing from genuine reporters.
narrative_ontology:constraint_stakeholder(positional_disagreement_as_evidence_flat_control, bad_faith_position_claimants, excluded,
    powerless, immediate, mobile, local).

% Epistemologists and methodologists who study the norm's operation across domains without a stake in any particular positional dispute, tracking whether it functions as genuine evidentiary enrichment or as a device that forecloses adjudication.
narrative_ontology:constraint_stakeholder(positional_disagreement_as_evidence_flat_control, philosophy_of_science_observers, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(positional_disagreement_as_evidence_flat_control, diffuse).
narrative_ontology:fixing_cost_class(positional_disagreement_as_evidence_flat_control, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the genuine problem that a single 'neutral' or averaged account of a complex arrangement systematically erases information held only by differently-positioned observers — a manager's view of a workplace and a line worker's view are not measurement error around a true mean, they report on structurally different exposures to the same arrangement, and averaging destroys the signal.
% TRANSFER_FUNCTION: Moves evidentiary authority and the burden of synthesis: institutions that once could resolve disputes by finding a consensus or a majority view must now treat every honestly-held positional report as standing evidence, shifting the cost of reconciliation from the reporter (who no longer must argue past dismissal-as-bias) onto the synthesizer, arbiter, or administrator who must act.
% ABSENT_VOICES: Actors who hold no clearly defined 'position' relative to the arrangement — occasional users, transient stakeholders, future generations affected by a decision made today — have no standing report to contribute and are structurally invisible to a framework organized entirely around positional testimony.
% DISAPPEARANCE_RATIONALE: Standpoint epistemologists and marginalized reporters would say the world rearranges badly overnight: reports from disadvantaged positions would again be treated as noise to be averaged away, silently restoring the pre-existing power gradient in whose account counts. Synthesis-seeking administrators and arbiters would say institutional decision-making would rearrange for the better, becoming faster and more actionable without every finding being open to standing-based challenge. Both readings are internally coherent from their seat, which is itself the disagreement the constraint concerns.
% FOUNDING_PROBLEM: Twentieth-century critiques of positivist and 'view from nowhere' epistemology (feminist standpoint theory, critical race theory, disability studies, science and technology studies) identified that treating disagreement as mere subjectivity systematically discounted the testimony of structurally disadvantaged observers whose position gave them epistemic access others lacked — e.g., workers seeing hazards managers do not, patients seeing harms clinicians do not register.
% FOUNDING_PROBLEM_CORROBORATION: Philosophy of science observers attest the founding problem (systematic evidentiary erasure of disadvantaged positions) was real and partially remains live in specific domains (occupational health, disability accommodation, colonial historiography). Consensus-dependent institutions and cross-positional arbiters, who are not the norm's beneficiaries, attest that in many current applications the founding problem has been substantially addressed by other mechanisms (whistleblower protection, participatory design mandates) and the norm now more often functions to block adjudication than to surface previously-erased evidence.
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
 *   Extractiveness is authored at a moderate 0.42 rather than low or high: the norm has a real, defensible coordination function (preventing evidentiary erasure of disadvantaged positions) which caps how extractive it is, but it also imposes a real, rising cost on institutions that must act despite irreconcilable positional reports, and that cost has grown as the norm has diffused from academic methodology into administrative and legal contexts where actionable synthesis is mandatory. Suppression (0.38) reflects the norm's active social enforcement — challenging a positional report's honesty is itself treated as an illegitimate move, a soft suppression of the ordinary evidentiary practice of cross-examination. Theater ratio rises modestly over the interval (0.10 to 0.30) as the norm's invocation in some institutional contexts (diversity trainings, stakeholder-engagement rituals) becomes performative acknowledgment rather than actual evidentiary uptake. Accessibility collapse (0.40) and resistance (0.55) are mid-range: alternative epistemic practices (structured adjudication, triangulation, majority synthesis) remain available and are actively defended by consensus-dependent institutions, which is exactly why resistance is substantial rather than negligible — this is not a mountain.
 *
 * PERSPECTIVAL GAP:
 *   From the standpoint-epistemologist and marginalized-reporter seats, the constraint looks like a hard-won correction to a genuinely biased prior practice (view-from-nowhere synthesis that erased disadvantaged testimony) — closer to a rope. From the synthesis-seeking administrator and cross-positional arbiter seats, the same structural arrangement looks like an unaccountable veto: any positional report can now block or delay an actionable finding by claiming its disagreement is evidence rather than error, with no principled way to distinguish a genuine positional insight from strategic invocation — closer to a snare. The engine computes both from the same authored ε and structural data; the divergence between the seats is the phenomenon this flat-construction story is documenting, not an error to reconcile.
 *
 * DIRECTIONALITY LOGIC:
 *   Standpoint epistemologists, frontline practitioners, and marginalized reporters are declared beneficiaries because the norm directly increases the evidentiary weight and social standing of their reports — this is a low-d, near-full-beneficiary relationship. Synthesis-seeking administrators, consensus-dependent institutions, and cross-positional arbiters are declared victims because the same norm, applied to them, raises their operational costs and delegitimizes their core function (producing a single actionable account) — high-d, near-full-target relationship. The coordination function (recovering real evidentiary content) and the extraction function (blocking synthesis, raising costs, shielding some reports from scrutiny) run through the identical structural mechanism — the refusal to average or defer — which is the defining signature of a tangled rope rather than a pure rope or pure snare.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — systematic evidentiary erasure of structurally disadvantaged observers under a false pretense of neutrality — was real and in some domains (occupational hazard reporting, disability accommodation, colonial historiography) remains live. But the norm has since diffused into contexts (routine institutional dispute resolution, HR conflict adjudication, general journalistic balance) where the original asymmetry (a powerful synthesizer erasing a powerless reporter) is not present, and where the norm now primarily functions to block synthesis rather than to recover erased evidence. This is the mandatrophy signature: a mandate (protect disadvantaged epistemic access) persisting and expanding into domains where its founding function has attenuated, while its administrative cost (indefinite non-adjudication) has grown. The tangled_rope classification, rather than snare or rope alone, reflects that both the live and the atrophied application coexist within the same social practice at present, unresolved.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    genuine_evidence_vs_strategic_shield,
    'Is a given invocation of positional disagreement recovering genuine evidentiary content the synthesizer would otherwise erase, or is it a strategic shield deployed by a bad-faith or self-interested reporter to insulate a claim from scrutiny?',
    'Track-record analysis of specific positional reports against subsequently-verified outcomes; process-based tests (does the reporter accept any form of cross-examination of their honesty, or only of the framing used to interpret their position); comparison of domains where the norm demonstrably surfaced previously-erased harms versus domains where it primarily blocked adjudication.',
    'If largely genuine-recovery, the constraint functions closer to a rope with modest overhead; if substantially strategic, the constraint functions closer to a snare shielding bad-faith actors behind a legitimate-sounding epistemic principle. The tangled_rope classification represents an authored judgment that both are present without a clean way to separate them structurally.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(genuine_evidence_vs_strategic_shield, empirical, 'Whether positional-disagreement invocations are predominantly evidentiary or predominantly strategic.').

omega_variable(
    domain_scope_boundary,
    'Does the norm''s legitimate coordination function hold at all scopes and domains equally, or was it calibrated to specific historical asymmetries (worker/manager, patient/clinician, colonized/colonizer) that do not generalize to routine institutional or interpersonal disagreement?',
    'Comparative case analysis across domains where power asymmetry between positions is large (occupational health) versus domains where it is small or symmetric (peer workplace disputes, ordinary journalistic sourcing) — does the norm''s evidentiary value track asymmetry, or does it operate identically regardless?',
    'If the norm''s value is asymmetry-dependent, applying it uniformly across all positional disagreements over-extends it into domains where it produces mostly extraction (blocked synthesis) with little coordination benefit — supporting a scope-restricted reading. If it generalizes, the current broad application is defensible as-is.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(domain_scope_boundary, conceptual, 'Whether the norm''s legitimacy is bounded by the power-asymmetry conditions of its founding cases.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression this norm exerts on cross-examining a claimed position primarily structural (institutional taboo against challenging positional standing, reputational risk to the challenger) or internalized (synthesizers and arbiters have absorbed a belief that any challenge to a positional report is itself epistemically illegitimate, independent of institutional sanction)?',
    'Interview synthesizers and arbiters who have exited institutions with strong enforcement of the norm — does their reluctance to cross-examine positional claims persist after the institutional sanction is removed?',
    'If substantially internalized, the effective suppression is higher than the structural measure suggests, since arbiters carry the inhibition beyond any specific institutional context, extending the norm''s reach beyond its formally enforced domain.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural versus internalized suppression of positional cross-examination.').


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
narrative_ontology:measurement(posi_tr_t16, positional_disagreement_as_evidence_flat_control, theater_ratio, 16, 0.19).
narrative_ontology:measurement(posi_tr_t24, positional_disagreement_as_evidence_flat_control, theater_ratio, 24, 0.23).
narrative_ontology:measurement(posi_tr_t32, positional_disagreement_as_evidence_flat_control, theater_ratio, 32, 0.27).
narrative_ontology:measurement(posi_tr_t40, positional_disagreement_as_evidence_flat_control, theater_ratio, 40, 0.3).

% Extraction over time
narrative_ontology:measurement(posi_be_t0, positional_disagreement_as_evidence_flat_control, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(posi_be_t8, positional_disagreement_as_evidence_flat_control, base_extractiveness, 8, 0.28).
narrative_ontology:measurement(posi_be_t16, positional_disagreement_as_evidence_flat_control, base_extractiveness, 16, 0.33).
narrative_ontology:measurement(posi_be_t24, positional_disagreement_as_evidence_flat_control, base_extractiveness, 24, 0.37).
narrative_ontology:measurement(posi_be_t32, positional_disagreement_as_evidence_flat_control, base_extractiveness, 32, 0.4).
narrative_ontology:measurement(posi_be_t40, positional_disagreement_as_evidence_flat_control, base_extractiveness, 40, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(posi_su_t0, positional_disagreement_as_evidence_flat_control, suppression_requirement, 0, 0.2).
narrative_ontology:measurement(posi_su_t8, positional_disagreement_as_evidence_flat_control, suppression_requirement, 8, 0.24).
narrative_ontology:measurement(posi_su_t16, positional_disagreement_as_evidence_flat_control, suppression_requirement, 16, 0.29).
narrative_ontology:measurement(posi_su_t24, positional_disagreement_as_evidence_flat_control, suppression_requirement, 24, 0.32).
narrative_ontology:measurement(posi_su_t32, positional_disagreement_as_evidence_flat_control, suppression_requirement, 32, 0.35).
narrative_ontology:measurement(posi_su_t40, positional_disagreement_as_evidence_flat_control, suppression_requirement, 40, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(positional_disagreement_as_evidence_flat_control, identity_coordination).
narrative_ontology:boltzmann_floor_override(positional_disagreement_as_evidence_flat_control, 0.1).

% DUAL FORMULATION NOTE:
% This story is authored FLAT, as a construction-perturbation control: it deliberately does not decompose the underlying epistemic-normative commitment into distinct kernel readings (e.g., a strong standpoint-epistemology reading versus a synthesis-preserving procedural reading), even though the commentary documents that such a decomposition would be a natural next step under the ε-invariance principle. The perspectival gap described in commentary.perspectival_gap is instead carried entirely through stakeholder seat divergence and omega variables, as instructed for the flat-construction control condition.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
