% ============================================================================
% CONSTRAINT STORY: ritual_transmission_as_double_edged_inheritance
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ritual_transmission_as_double_edged_inheritance, []).

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
    narrative_ontology:suppression_profile/2,
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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: ritual_transmission_as_double_edged_inheritance
 *   human_readable: Grief-Ritual Transmitted as Comfort, Later Repurposed as Coercive Key
 *   domain: narrative/folk-legal
 *
 * SUMMARY:
 *   A grief-ritual — exact whole name, exact hour, or it does nothing — is
 *   taught to a child as pure comfort, before he has any grief to test it
 *   against. The teaching act is complete and irreversible the moment it is
 *   finished: there is no residual mechanism by which the teacher can later
 *   withhold, amend, or recall the form. Years later the same exactness that
 *   made the ritual meaningful as comfort turns out to also function as the
 *   operative key into the Warden's jurisdiction, and the grown recipient
 *   begins using it as instrumental leverage over others. The teacher
 *   discovers herself permanently downstream of a use she never intended and
 *   structurally cannot undo. Claimed as rope (a genuine grief-coordination
 *   mechanism, freely taught, no coercion at the moment of transmission)
 *   while the authored metrics show extraction rising over time as the
 *   ritual's second function is exercised — the divergence between the claim
 *   (rope, at time of teaching) and the metrics (rising extraction, at time
 *   of later use) is the story's central measurement, not an error to
 *   reconcile.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ritual_transmission_as_double_edged_inheritance, 0.42).
domain_priors:suppression_score(ritual_transmission_as_double_edged_inheritance, 0.31).
domain_priors:theater_ratio(ritual_transmission_as_double_edged_inheritance, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ritual_transmission_as_double_edged_inheritance, extractiveness, 0.42).
narrative_ontology:constraint_metric(ritual_transmission_as_double_edged_inheritance, suppression_requirement, 0.31).
narrative_ontology:constraint_metric(ritual_transmission_as_double_edged_inheritance, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ritual_transmission_as_double_edged_inheritance, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(ritual_transmission_as_double_edged_inheritance, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ritual_transmission_as_double_edged_inheritance, rope).
narrative_ontology:human_readable(ritual_transmission_as_double_edged_inheritance, "Grief-Ritual Transmitted as Comfort, Later Repurposed as Coercive Key").
narrative_ontology:topic_domain(ritual_transmission_as_double_edged_inheritance, "narrative/folk-legal").

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ritual_transmission_as_double_edged_inheritance, grieving_child_recipient).
narrative_ontology:constraint_beneficiary(ritual_transmission_as_double_edged_inheritance, warden_jurisdiction_operators).
narrative_ontology:constraint_victim(ritual_transmission_as_double_edged_inheritance, ritual_teacher).
narrative_ontology:constraint_victim(ritual_transmission_as_double_edged_inheritance, later_ritual_subjects).
narrative_ontology:constraint_vindicates(ritual_transmission_as_double_edged_inheritance, exactness_requirement_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Taught the boy the grief-ritual — the exact hour, the whole name spoken correctly, or it does nothing — purely as a steadying comfort before he had any grief to test it against. The transmission was complete the moment it was spoken; she has no lever left to withhold, modify, soften, or recall it. She later learns it has been repurposed as the operative key into the Warden's jurisdiction and discovers she is structurally downstream of a use she never authorized and cannot undo.
narrative_ontology:constraint_stakeholder(ritual_transmission_as_double_edged_inheritance, ritual_teacher, agenda_setter,
    moderate, biographical, trapped, local).
narrative_ontology:stakeholder_secondary_role(ritual_transmission_as_double_edged_inheritance, ritual_teacher, payer).

% Received the ritual as pure comfort while still a child, before grief gave it any weight. As he grows he discovers the same exact-form requirement (whole name, exact hour) also functions as the precise instrumental key that opens or invokes the Warden's jurisdiction. He now holds and wields the ritual as leverage, having inherited a tool whose second function he did not create and the teacher did not intend.
narrative_ontology:constraint_stakeholder(ritual_transmission_as_double_edged_inheritance, grieving_child_recipient, beneficiary,
    powerless, biographical, identity_locked, local).
narrative_ontology:stakeholder_secondary_role(ritual_transmission_as_double_edged_inheritance, grieving_child_recipient, agenda_setter).

% Administer the jurisdiction the ritual's exact form happens to unlock. They did not create the comfort-ritual, but they benefit whenever anyone holding the correct form uses it as a key — the exactness requirement (whole name, precise hour) that makes the ritual meaningful as grief-comfort is the identical requirement that makes it operative as access, and the Warden's system absorbs whichever comers arrive holding it.
narrative_ontology:constraint_stakeholder(ritual_transmission_as_double_edged_inheritance, warden_jurisdiction_operators, beneficiary,
    institutional, generational, arbitrage, regional).

% Are the people against whom the grown recipient later deploys the ritual as a coercive lever into the Warden's jurisdiction — bound, summoned, or judged by a mechanism whose origin was a child's private comfort. They have no relationship to the original teaching and no way to contest a form whose legitimacy rests on its exactness, not on any consent they gave.
narrative_ontology:constraint_stakeholder(ritual_transmission_as_double_edged_inheritance, later_ritual_subjects, payer,
    powerless, biographical, trapped, local).

% Know the ritual only as a piece of local grief-practice passed parent-to-child. They would object to its use as a jurisdictional key if they understood the dual function, but the knowledge of the repurposing lives with the grown recipient and the Warden's operators, not in the community that still practices it as comfort.
narrative_ontology:constraint_stakeholder(ritual_transmission_as_double_edged_inheritance, folk_community_witnesses, excluded,
    powerless, generational, constrained, local).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(ritual_transmission_as_double_edged_inheritance, grieving_child_recipient).
narrative_ontology:fixing_cost_class(ritual_transmission_as_double_edged_inheritance, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The ritual genuinely coordinates grief: it gives a child a fixed, learnable procedure (exact name, exact hour) that steadies him against loss before he has the emotional resources to process it unaided. That comfort function is real and was the entire content of the original teaching act.
% TRANSFER_FUNCTION: Nothing is transferred at the moment of teaching — it is a pure gift of comfort. The transfer occurs later and elsewhere: once the recipient discovers the ritual's exactness also operates as a jurisdictional key, use of that key transfers standing, access, or coercive leverage from later subjects to the recipient, and indirectly validates the Warden's jurisdiction itself.
% ABSENT_VOICES: The teacher, once the transmission is complete, has no further voice in how the ritual is used — she is structurally excluded from her own creation's second life. Later ritual subjects, who bear the coercive use, were never present for the teaching and have no channel to object to a form that predates and does not include them.
% DISAPPEARANCE_RATIONALE: If the ritual vanished, the child would lose a genuine grief-comfort mechanism the community depends on — that loss is real and the world of grieving families would rearrange around its absence. But the Warden's jurisdiction would also lose one incidental access key among what may be several; whether the jurisdiction's operation depends on this specific ritual or would simply route around it via another exact-form key is unresolved, which is why the verdict is contested rather than settled either way.
% FOUNDING_PROBLEM: A child needed a steadying practice to hold onto in grief, at an age before he had the conceptual or emotional apparatus to process loss unaided; the ritual was built to give him something exact and repeatable to do with his hands and voice when there was nothing else to do.
% FOUNDING_PROBLEM_CORROBORATION: The teacher herself attests the founding problem (comfort for an ungrieving child) was real and remains the ritual's only intended function — she is not a beneficiary of its second use and has no stake in defending it. Folk community witnesses, who still practice the ritual purely as grief-comfort and are outside the Warden's jurisdiction entirely, corroborate that the comfort function persists independent of the coercive repurposing. No party inside the Warden's jurisdiction has been asked, and the recipient who discovered the dual function has not disclosed how he learned of it, so the coercive function's own genealogy is corroborated by no one outside its beneficiaries.
narrative_ontology:disappearance_verdict(ritual_transmission_as_double_edged_inheritance, contested).
narrative_ontology:founding_problem_status(ritual_transmission_as_double_edged_inheritance, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ritual_transmission_as_double_edged_inheritance, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'f1436bd4937f864097dabaad92b27bd9b6eec212', '2026-07-25',
    'uke_narrative', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'temperature=0.2,max_tokens=8192').
narrative_ontology:story_seed(ritual_transmission_as_double_edged_inheritance, 'none', 1).
narrative_ontology:epsilon_provenance(ritual_transmission_as_double_edged_inheritance, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ritual_transmission_as_double_edged_inheritance_tests).
:- end_tests(ritual_transmission_as_double_edged_inheritance_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction starts near zero (0.05) because at the moment of teaching there is no coercive content whatsoever — it is a gift. It rises steadily (to 0.42 by the interval's end) purely because of what happens downstream: the recipient's later deployment of the exact form as a jurisdictional key against people who never consented to be subject to it. Theater ratio also rises modestly (0.28) reflecting that some of the ritual's later invocations are performative assertions of authority rather than functional grief-comfort — the same words, repurposed. Suppression is moderate (0.31) rather than high because the coercion, where it exists, operates through the Warden's institutional jurisdiction rather than through direct force by the teacher or recipient.
 *
 * PERSPECTIVAL GAP:
 *   From the teacher's seat, the constraint she created was and remains a rope — pure coordination against grief, freely given, coercing no one. From the later ritual subjects' seat, the identical form operates as a tangled or extractive mechanism — a key wielded against them with the full institutional weight of the Warden's jurisdiction behind it. The engine should compute divergent seat classifications from the same structural facts: this is the intended signature of transmission-completion irreversibility, not a contradiction to resolve.
 *
 * DIRECTIONALITY LOGIC:
 *   The teacher is structurally a victim despite being the agenda_setter of the original teaching — she authored the form but not its later use, and has zero ongoing control (trapped exit) once transmission completed. The recipient is the primary structural beneficiary of the ritual's second function, having inherited a coercive instrument he did not build but now wields. The Warden's jurisdiction operators are institutional beneficiaries who profit from the ritual's incidental compatibility with their access requirements without having created or taught it. Later ritual subjects bear the cost with no exit at all — they are trapped by a mechanism whose legitimacy chain traces back to an act of grief-comfort they have no part in.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (a child's need for grief-comfort) is genuinely dead as a driver of the ritual's current coercive use — the recipient is no longer the grieving child who needed steadying, he is an adult wielding a key. Yet the ritual's coordination function has not vanished; it may still comfort other grieving people who never learn its second use. Classifying this as a single frozen type would either exonerate the coercive use (by pointing to the real comfort function) or condemn the comfort function (by pointing to the coercive use). The seat-relative computation prevents either mislabeling: the comfort-teaching event and the coercive-deployment event are structurally the same ritual-form but functionally distinct uses, and the classification should track which use is being measured.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    exactness_as_natural_or_constructed_requirement,
    'Is the ritual''s requirement of exact name and exact hour a natural feature of how grief-comfort rituals must work (specificity aids psychological steadying), or is it a constructed feature deliberately compatible with the Warden''s jurisdictional access requirements?',
    'Comparative study of other grief-comfort rituals in the same folk tradition: if exactness-of-form is common across rituals with no jurisdictional function, the requirement is likely intrinsic to the comfort mechanism, not designed for later coercive compatibility.',
    'If the exactness is intrinsic to comfort, the dual-use is coincidental and the teacher bears no design responsibility for the later coercive function. If the exactness was always shaped by or borrowed from jurisdictional forms, the teacher''s ritual was never purely comfort — it carried the coercive key from its own origin.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(exactness_as_natural_or_constructed_requirement, conceptual, 'Whether the ritual''s dual function is coincidental or was structurally present from its own transmission.').

omega_variable(
    teacher_downstream_liability_ambiguity,
    'Does the teacher''s complete inability to modify or recall the ritual after transmission mean she bears no responsibility for its later coercive use, or does knowingly teaching an irreversible, exact-form ritual to a child carry an anticipatory responsibility for foreseeable misuse?',
    'Examine whether the folk tradition itself has any doctrine of transmitter responsibility for downstream ritual use, or whether irreversibility is understood community-wide as fully discharging the teacher''s responsibility at the moment of teaching.',
    'If the tradition holds transmitters blameless once transmission completes, the teacher is correctly classified as a victim of the later use. If the tradition holds transmitters partially responsible for foreseeable misuse, the teacher''s seat may carry some d toward the target end even without active current involvement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(teacher_downstream_liability_ambiguity, preference, 'Whether transmission-completion fully discharges the teacher''s structural responsibility for later misuse.').

omega_variable(
    warden_jurisdiction_natural_or_constructed,
    'Is the Warden''s jurisdiction itself a genuine, independently-arising coordination structure that happens to overlap with the ritual''s form, or is the jurisdiction constructed/maintained partly through the accumulation of exactly these repurposed folk-rituals as access keys?',
    'Trace whether the Warden''s jurisdiction predates the ritual''s dual-use discovery, and whether the jurisdiction has other independent access mechanisms not derived from folk grief-rituals.',
    'If the jurisdiction is independently founded and merely absorbs whatever keys arrive, the Warden''s operators are incidental beneficiaries with lower culpability. If the jurisdiction''s authority is substantially constituted by accumulated repurposed rituals, the Warden''s operators are active co-architects of the extraction and the tangled-rope reading strengthens at their seat specifically.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(warden_jurisdiction_natural_or_constructed, empirical, 'Whether the Warden''s jurisdiction is independent of or partly constituted by repurposed folk-ritual keys.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ritual_transmission_as_double_edged_inheritance, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ritu_tr_t0, ritual_transmission_as_double_edged_inheritance, theater_ratio, 0, 0.05).
narrative_ontology:measurement(ritu_tr_t4, ritual_transmission_as_double_edged_inheritance, theater_ratio, 4, 0.09).
narrative_ontology:measurement(ritu_tr_t8, ritual_transmission_as_double_edged_inheritance, theater_ratio, 8, 0.15).
narrative_ontology:measurement(ritu_tr_t12, ritual_transmission_as_double_edged_inheritance, theater_ratio, 12, 0.2).
narrative_ontology:measurement(ritu_tr_t16, ritual_transmission_as_double_edged_inheritance, theater_ratio, 16, 0.25).
narrative_ontology:measurement(ritu_tr_t20, ritual_transmission_as_double_edged_inheritance, theater_ratio, 20, 0.28).

% Extraction over time
narrative_ontology:measurement(ritu_be_t0, ritual_transmission_as_double_edged_inheritance, base_extractiveness, 0, 0.05).
narrative_ontology:measurement(ritu_be_t4, ritual_transmission_as_double_edged_inheritance, base_extractiveness, 4, 0.08).
narrative_ontology:measurement(ritu_be_t8, ritual_transmission_as_double_edged_inheritance, base_extractiveness, 8, 0.22).
narrative_ontology:measurement(ritu_be_t12, ritual_transmission_as_double_edged_inheritance, base_extractiveness, 12, 0.34).
narrative_ontology:measurement(ritu_be_t16, ritual_transmission_as_double_edged_inheritance, base_extractiveness, 16, 0.39).
narrative_ontology:measurement(ritu_be_t20, ritual_transmission_as_double_edged_inheritance, base_extractiveness, 20, 0.42).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(ritual_transmission_as_double_edged_inheritance, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ritual_transmission_as_double_edged_inheritance, attachment_coordination).
narrative_ontology:boltzmann_floor_override(ritual_transmission_as_double_edged_inheritance, 0.08).

% DUAL FORMULATION NOTE:
% This story isolates the transmission-and-repurposing dynamic itself, not the Warden's jurisdiction as an institution. A sibling story, constraint_warden_jurisdiction_access_regime, would examine the jurisdiction's own coordination/extraction structure independently and should be linked here once authored — the exactness-requirement is the shared structural element between the two, but the jurisdiction's own extractiveness from its subjects is a separate ε and belongs in its own file per the ε-invariance principle.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
