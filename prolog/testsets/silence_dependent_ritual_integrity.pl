% ============================================================================
% CONSTRAINT STORY: silence_dependent_ritual_integrity
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_silence_dependent_ritual_integrity, []).

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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: silence_dependent_ritual_integrity
 *   human_readable: Silence-Dependent Ritual Integrity (Klára's Private Act)
 *   domain: social/interior/domestic
 *
 * SUMMARY:
 *   Klára maintains a small, recurring private act — a nightly ritual whose
 *   meaning and function depend entirely on its remaining unspoken. For years
 *   it functions as a low-cost coordination device: it lets her metabolize
 *   pressure from a domestic arrangement she does not fully control, without
 *   requiring any external negotiation or disclosure. The structural delta is
 *   this: at time_point 16, she names the ritual aloud to a confidant. No
 *   external fact about the act itself changes — same behavior, same history
 *   — but her own relationship to it converts irreversibly. What had been a
 *   private, self-authored object becomes a narrated account, shaped by and
 *   for an audience. The extractiveness spike at t=16 models this conversion
 *   event: the ritual continues to exist, but its coordination function for
 *   Klára specifically is partially destroyed by the very act of
 *   articulation, and this loss cannot be reversed by silence resumed
 *   afterward — the ontological status is already lost.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(silence_dependent_ritual_integrity, 0.58).
domain_priors:suppression_score(silence_dependent_ritual_integrity, 0.62).
domain_priors:theater_ratio(silence_dependent_ritual_integrity, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(silence_dependent_ritual_integrity, extractiveness, 0.58).
narrative_ontology:constraint_metric(silence_dependent_ritual_integrity, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(silence_dependent_ritual_integrity, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(silence_dependent_ritual_integrity, accessibility_collapse, 0.71).
narrative_ontology:constraint_metric(silence_dependent_ritual_integrity, resistance, 0.44).

% --- Constraint claim ---
narrative_ontology:constraint_claim(silence_dependent_ritual_integrity, tangled_rope).
narrative_ontology:human_readable(silence_dependent_ritual_integrity, "Silence-Dependent Ritual Integrity (Klára's Private Act)").
narrative_ontology:topic_domain(silence_dependent_ritual_integrity, "social/interior/domestic").

domain_priors:requires_active_enforcement(silence_dependent_ritual_integrity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(silence_dependent_ritual_integrity, klara_private_self).
narrative_ontology:constraint_beneficiary(silence_dependent_ritual_integrity, household_stability_norm).
narrative_ontology:constraint_victim(silence_dependent_ritual_integrity, klara_self).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Performs a recurring private act — a small nightly ritual of self-soothing and control inside an otherwise constrained domestic life — that functions only because it stays unspoken. The silence is what lets the act remain hers alone, a pocket of autonomy inside a household whose terms she does not fully set. She enforces the silence on herself, choosing not to disclose it even to intimates, because disclosure is felt (correctly, by her own later testimony) to change what the act is.
narrative_ontology:constraint_stakeholder(silence_dependent_ritual_integrity, klara_private_self, beneficiary,
    moderate, biographical, identity_locked, local).
narrative_ontology:stakeholder_secondary_role(silence_dependent_ritual_integrity, klara_private_self, agenda_setter).

% The same person, considered as the one who bears the cost of the constraint: once she speaks the ritual aloud to a third party, it becomes a narrated account — an object shaped for an audience, subject to their interpretation, no longer purely self-referential. She loses something (the ritual's private ontological status) without gaining an external remedy; the act's function as a private anchor is foreclosed by the very act of articulating it. She cannot simply decide to un-speak it and restore the prior object; the conversion is irreversible.
narrative_ontology:constraint_stakeholder(silence_dependent_ritual_integrity, klara_self, payer,
    moderate, biographical, trapped, local).

% The tacit domestic order benefits from Klára having a private, unspoken outlet: it absorbs psychological pressure that might otherwise surface as household conflict or demands for change. It is not an actor and collects nothing directly, but the arrangement's persistence depends on the ritual staying private and thus non-negotiable.
narrative_ontology:constraint_stakeholder(silence_dependent_ritual_integrity, household_stability_norm, beneficiary,
    institutional, generational, analytical, local).
narrative_ontology:stakeholder_non_agent(silence_dependent_ritual_integrity, household_stability_norm).

% A friend, partner, or therapist to whom Klára might disclose the ritual. If present in the conversation, their listening presence is precisely the mechanism that converts the private act into a narrated account — they are structurally necessary to the harm but are not consulted about whether disclosure should happen; they simply receive whatever is said and, by receiving it, complete the conversion.
narrative_ontology:constraint_stakeholder(silence_dependent_ritual_integrity, confidant_figure, excluded,
    moderate, immediate, mobile, local).

% Notes that no external fact changes between the silent version and the spoken version of the act — same behavior, same timing, same physical content — yet the actor's own relationship to it measurably shifts post-utterance. This is the structural puzzle: the constraint's identity is observer-relative to the actor herself, not to any third party's knowledge.
narrative_ontology:constraint_stakeholder(silence_dependent_ritual_integrity, analytical_observer, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(silence_dependent_ritual_integrity, diffuse).
narrative_ontology:fixing_cost_class(silence_dependent_ritual_integrity, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The silence coordinates a stable self-relationship: it lets Klára maintain a small zone of private meaning-making that does not have to be justified, defended, or reshaped by another's interpretation, and it coordinates with the household's tacit peace by keeping a pressure valve invisible and thus uncontested.
% TRANSFER_FUNCTION: Nothing material transfers between named parties, but something transfers within Klára across time: the private-ritual version of the self, once spoken, is replaced by a narrated-account version, and the earlier private status cannot be recovered. The 'payment' is her own loss of an interior object she cannot buy back.
% ABSENT_VOICES: The confidant who would receive the disclosure has no say in whether disclosure happens or in preserving the pre-utterance object; and there is no institutional or therapeutic framework that treats 'converting a private act by naming it' as a cost to be weighed, so the loss goes unrepresented in any decision Klára makes about whether to speak.
% DISAPPEARANCE_RATIONALE: If the silence requirement vanished — if speaking the ritual aloud did not change its character — Klára would lose no autonomy by disclosing it, and the private/narrated distinction would collapse into a single continuous object. Whether the household order would 'rearrange' is contested: some readings hold the norm depends on invisible pressure-absorption and would destabilize; others hold the specific ritual is incidental and the household would adapt to open disclosure without difficulty.
% FOUNDING_PROBLEM: The ritual and its silence emerged to solve a real problem: Klára needed a small, self-authored space of control and meaning inside a domestic life whose larger terms she did not fully author, and unspoken privacy was the only mechanism available to preserve that space as purely her own.
% FOUNDING_PROBLEM_CORROBORATION: Klára herself attests to the problem and to the post-utterance shift in a retrospective account ('no longer the thing it had been') — but this is self-testimony from inside the arrangement. No outside corroboration (a partner, a therapist, an independent observer of the household) currently attests to either the founding problem or its persistence; that absence is itself notable and is flagged rather than papered over.
narrative_ontology:disappearance_verdict(silence_dependent_ritual_integrity, contested).
narrative_ontology:founding_problem_status(silence_dependent_ritual_integrity, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(silence_dependent_ritual_integrity, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-09',
    'uke_narrative', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'temperature=0.2,max_tokens=8192').
narrative_ontology:story_seed(silence_dependent_ritual_integrity, 'none', 1).
narrative_ontology:epsilon_provenance(silence_dependent_ritual_integrity, 0.58, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(silence_dependent_ritual_integrity_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(silence_dependent_ritual_integrity, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(silence_dependent_ritual_integrity_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low and stable (0.12-0.22) while the ritual remains unspoken, because the arrangement is functioning as intended: cheap, self-administered, non-coercive. It jumps sharply once verbal articulation occurs (0.61 at t=16) because the disclosure converts the object without her consent to the conversion — she consented to speak, not to the ontological loss that followed. It settles somewhat (0.55-0.58) as she adapts to the narrated-account version of the act, but never returns to baseline: the private status is not recoverable. Theater ratio rises alongside — post-disclosure, some of what she does around the ritual becomes performed for the imagined judgment of the listener, even in the listener's absence.
 *
 * PERSPECTIVAL GAP:
 *   From Klára's own retrospective vantage the constraint reads as a hybrid: real coordination (a genuinely useful private practice) fused with an involuntary extraction (loss of the practice's prior character) triggered by her own speech act. Any external observer sees only continuity — same act, same timing — and would miss the extraction entirely without her testimony, since the injury is entirely in the register of self-relationship, not observable behavior.
 *
 * DIRECTIONALITY LOGIC:
 *   Klára-as-private-self is a beneficiary before disclosure: the silence subsidizes her autonomy at negligible cost. Klára-as-payer (klara_self, post-conversion) is the victim: she bears a cost — loss of the private object's integrity — with no compensating benefit and no exit, because she is both the one who spoke and the one who suffers the conversion; there is no third party to blame and no reversal available. The household_stability_norm benefits without acting, a genuine non-agent beneficiary, because it collects the diffuse benefit of Klára's continued self-management. The confidant is excluded from the decision about whether disclosure should happen, yet is structurally required to complete the conversion by their listening presence.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification resists two simplifications. First, it resists reading the whole arrangement as pure extraction (snare): the ritual served a genuine, low-cost coordination function for years before the conversion event, and most of its life was Rope-like. Second, it resists reading the constraint as untouched coordination (rope) after disclosure: something real and asymmetric was lost, borne entirely by Klára with no compensating benefit, which is the signature of tangled rope rather than a benign transition. Treating the whole interval as one flat type would mislabel either the healthy early period as extractive or the post-disclosure loss as costless.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    ontological_vs_relational_conversion,
    'Does naming the ritual actually change the act''s intrinsic nature, or does it only change Klára''s socially-mediated relationship to an unchanged act — i.e., is this an ontological conversion or a purely relational/psychological one?',
    'Phenomenological interview at multiple post-disclosure intervals to establish whether the shift is a one-time reframing that fades (relational) or a permanent, non-decaying alteration in how the act is experienced (closer to ontological, by the actor''s own lights).',
    'If relational and fading, the extraction spike should decay toward baseline over a longer interval, supporting a milder classification (rope with a temporary friction cost). If persistent, the tangled_rope classification with irreversible victim cost is supported.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(ontological_vs_relational_conversion, conceptual, 'Whether the silence-dependent identity shift is genuinely ontological or a persistent-but-relational reframing.').

omega_variable(
    silence_as_coordination_or_isolation,
    'Is the pre-disclosure silence a genuine coordination good (a healthy zone of private autonomy) or is it itself already a symptom of isolation imposed by the household''s larger power structure, such that the ''ritual'' is compensating for a deficit rather than exercising a freely chosen privacy?',
    'Comparative analysis: would Klára need this private, unspoken ritual if the domestic arrangement afforded her more voice and negotiating power generally? Track whether disclosure becomes less costly as household equality increases.',
    'If the silence is compensatory for domestic powerlessness, the beneficiary designation for klara_private_self and household_stability_norm should be reconsidered — the coordination function may itself be extraction-adjacent, making the story closer to a snare wearing a coordination costume.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(silence_as_coordination_or_isolation, empirical, 'Whether the ritual''s silence-dependence reflects healthy autonomy or compensates for a deeper domestic power asymmetry.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression (the self-enforced silence) structural — arising from the domestic arrangement''s actual constraints on Klára''s voice — or internalized, arising from her own belief that disclosure would be shameful or damaging regardless of how a listener would actually respond?',
    'Post-disclosure trajectory: if the negative shift in her relationship to the ritual persists even when the confidant responds with acceptance and no judgment, the suppression was substantially internalized rather than a realistic reading of external risk.',
    'If internalized, the effective suppression is higher than the structural measure suggests, and the intervention that would resolve the extraction is not changing the household but addressing Klára''s own belief structure about disclosure and shame.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism behind the silence requirement.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(silence_dependent_ritual_integrity, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sile_tr_t0, silence_dependent_ritual_integrity, theater_ratio, 0, 0.05).
narrative_ontology:measurement(sile_tr_t4, silence_dependent_ritual_integrity, theater_ratio, 4, 0.06).
narrative_ontology:measurement(sile_tr_t8, silence_dependent_ritual_integrity, theater_ratio, 8, 0.08).
narrative_ontology:measurement(sile_tr_t12, silence_dependent_ritual_integrity, theater_ratio, 12, 0.1).
narrative_ontology:measurement(sile_tr_t16, silence_dependent_ritual_integrity, theater_ratio, 16, 0.34).
narrative_ontology:measurement(sile_tr_t20, silence_dependent_ritual_integrity, theater_ratio, 20, 0.3).
narrative_ontology:measurement(sile_tr_t24, silence_dependent_ritual_integrity, theater_ratio, 24, 0.28).

% Extraction over time
narrative_ontology:measurement(sile_be_t0, silence_dependent_ritual_integrity, base_extractiveness, 0, 0.12).
narrative_ontology:measurement(sile_be_t4, silence_dependent_ritual_integrity, base_extractiveness, 4, 0.15).
narrative_ontology:measurement(sile_be_t8, silence_dependent_ritual_integrity, base_extractiveness, 8, 0.18).
narrative_ontology:measurement(sile_be_t12, silence_dependent_ritual_integrity, base_extractiveness, 12, 0.22).
narrative_ontology:measurement(sile_be_t16, silence_dependent_ritual_integrity, base_extractiveness, 16, 0.61).
narrative_ontology:measurement(sile_be_t20, silence_dependent_ritual_integrity, base_extractiveness, 20, 0.55).
narrative_ontology:measurement(sile_be_t24, silence_dependent_ritual_integrity, base_extractiveness, 24, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(sile_su_t0, silence_dependent_ritual_integrity, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(sile_su_t4, silence_dependent_ritual_integrity, suppression_requirement, 4, 0.42).
narrative_ontology:measurement(sile_su_t8, silence_dependent_ritual_integrity, suppression_requirement, 8, 0.45).
narrative_ontology:measurement(sile_su_t12, silence_dependent_ritual_integrity, suppression_requirement, 12, 0.48).
narrative_ontology:measurement(sile_su_t16, silence_dependent_ritual_integrity, suppression_requirement, 16, 0.66).
narrative_ontology:measurement(sile_su_t20, silence_dependent_ritual_integrity, suppression_requirement, 20, 0.6).
narrative_ontology:measurement(sile_su_t24, silence_dependent_ritual_integrity, suppression_requirement, 24, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(silence_dependent_ritual_integrity, attachment_coordination).
narrative_ontology:boltzmann_floor_override(silence_dependent_ritual_integrity, 0.1).

% DUAL FORMULATION NOTE:
% This story treats the private-ritual/narrated-account distinction as a single constraint with one stable epsilon assessed from Klára's own vantage across the interval. A sibling story could be written from the confidant's vantage (what changes for THEM upon hearing the disclosure) — that would be a structurally distinct constraint with its own beneficiary/victim structure, not a re-measurement of this one, per the epsilon-invariance principle. No such sibling exists in this batch; the field is left empty rather than fabricating a link.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
