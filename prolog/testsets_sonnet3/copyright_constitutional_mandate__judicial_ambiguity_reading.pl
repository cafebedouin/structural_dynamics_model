% ============================================================================
% CONSTRAINT STORY: copyright_constitutional_mandate__judicial_ambiguity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_copyright_constitutional_mandate__judicial_ambiguity_reading, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: copyright_constitutional_mandate__judicial_ambiguity_reading
 *   human_readable: Rational Basis Deference to Congressional Copyright Term-Setting
 *   domain: constitutional/intellectual_property_law
 *
 * SUMMARY:
 *   This constraint concerns the standard of judicial review the courts apply
 *   when Congress extends copyright terms, not the substantive question of
 *   what copyright is for or how long protection should last. Under this
 *   reading, the operative fact is procedural: courts treat term-length as a
 *   matter of ordinary economic/social policy subject to rational basis
 *   review, meaning any conceivable rational basis Congress articulates (even
 *   post-hoc) is sufficient to sustain an extension, including retroactive
 *   extension of existing copyrights just before their expiration. This
 *   judicial posture is the mechanism by which the constitutional phrase
 *   'limited Times' has, over 234 years, moved from an apparently binding
 *   ceiling to a phrase that constrains almost nothing in practice, without
 *   ever being formally read out of the text. Eldred v. Ashcroft (2003) is
 *   the paradigm case: the Court declined to ask whether repeated extension
 *   defeated the purpose of the Progress Clause, holding instead that
 *   Congress's rationale needed only to be conceivable.
 *
 * KEY AGENTS:
 *   - congress: agenda_setter, sets term length under low judicial scrutiny
 *   - copyright_holding_industries: beneficiary, captures extended monopoly rents via lobbying
 *   - judiciary: agenda_setter for the standard of review itself, chooses deference
 *   - public_domain_entrants: powerless payer, works held out of the commons longer
 *   - downstream_creators: moderate payer, bears licensing/uncertainty costs
 *   - constitutional_fixity_as_a_constraint: non-agent payer, the textual limit itself is eroded
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(copyright_constitutional_mandate__judicial_ambiguity_reading, 0.38).
domain_priors:suppression_score(copyright_constitutional_mandate__judicial_ambiguity_reading, 0.42).
domain_priors:theater_ratio(copyright_constitutional_mandate__judicial_ambiguity_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(copyright_constitutional_mandate__judicial_ambiguity_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(copyright_constitutional_mandate__judicial_ambiguity_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(copyright_constitutional_mandate__judicial_ambiguity_reading, theater_ratio, 0.55).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(copyright_constitutional_mandate__judicial_ambiguity_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(copyright_constitutional_mandate__judicial_ambiguity_reading, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(copyright_constitutional_mandate__judicial_ambiguity_reading, tangled_rope).
narrative_ontology:human_readable(copyright_constitutional_mandate__judicial_ambiguity_reading, "Rational Basis Deference to Congressional Copyright Term-Setting").
narrative_ontology:topic_domain(copyright_constitutional_mandate__judicial_ambiguity_reading, "constitutional/intellectual_property_law").

domain_priors:requires_active_enforcement(copyright_constitutional_mandate__judicial_ambiguity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(copyright_constitutional_mandate__judicial_ambiguity_reading, '83065615-b1d2-4185-b650-a4dc232e0e97').
narrative_ontology:cs_kernel_codification('83065615-b1d2-4185-b650-a4dc232e0e97', fixed_text).
narrative_ontology:cs_authority_grounding('83065615-b1d2-4185-b650-a4dc232e0e97', lineage).
narrative_ontology:cs_interpretation_layer_present('83065615-b1d2-4185-b650-a4dc232e0e97').
narrative_ontology:cs_reading_relation('83065615-b1d2-4185-b650-a4dc232e0e97', copyright_constitutional_mandate__corporate_enclosure_reading, coexists_with).
narrative_ontology:cs_reading_relation('83065615-b1d2-4185-b650-a4dc232e0e97', copyright_constitutional_mandate__public_scaffold_reading, influences).
narrative_ontology:cs_axiom('83065615-b1d2-4185-b650-a4dc232e0e97', foundational, judicial_deference_to_enumerated_power_line_drawing).
narrative_ontology:cs_axiom_status(judicial_deference_to_enumerated_power_line_drawing, holdable).
narrative_ontology:cs_axiom_grounding('83065615-b1d2-4185-b650-a4dc232e0e97', judicial_deference_to_enumerated_power_line_drawing, conventional).
narrative_ontology:cs_axiom('83065615-b1d2-4185-b650-a4dc232e0e97', secondary, rational_basis_sufficient_absent_explicit_perpetuity).
narrative_ontology:cs_axiom_status(rational_basis_sufficient_absent_explicit_perpetuity, holdable).
narrative_ontology:cs_axiom_grounding('83065615-b1d2-4185-b650-a4dc232e0e97', rational_basis_sufficient_absent_explicit_perpetuity, conventional).
narrative_ontology:cs_reference_frame('83065615-b1d2-4185-b650-a4dc232e0e97', separation_of_powers_institutional_competence_deference).
narrative_ontology:cs_drift_state('83065615-b1d2-4185-b650-a4dc232e0e97', post_eldred_contemporary, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('83065615-b1d2-4185-b650-a4dc232e0e97', '').
narrative_ontology:cs_kernel_id(copyright_constitutional_mandate__judicial_ambiguity_reading, copyright_constitutional_mandate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(copyright_constitutional_mandate__judicial_ambiguity_reading, congress).
narrative_ontology:constraint_beneficiary(copyright_constitutional_mandate__judicial_ambiguity_reading, copyright_holding_industries).
narrative_ontology:constraint_victim(copyright_constitutional_mandate__judicial_ambiguity_reading, public_domain_entrants).
narrative_ontology:constraint_victim(copyright_constitutional_mandate__judicial_ambiguity_reading, downstream_creators).
narrative_ontology:constraint_victim(copyright_constitutional_mandate__judicial_ambiguity_reading, constitutional_fixity_as_a_constraint).
narrative_ontology:constraint_vindicates(copyright_constitutional_mandate__judicial_ambiguity_reading, separation_of_powers_doctrine).
narrative_ontology:constraint_vindicates(copyright_constitutional_mandate__judicial_ambiguity_reading, legislative_primacy_in_line_drawing).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets and repeatedly extends the copyright term (28 years to 95+ years to life-plus-70), justified as an exercise of the enumerated power to promote science and useful arts. Because courts apply rational basis review rather than scrutinize whether extensions actually serve the constitutional purpose, Congress faces no meaningful judicial check on how far it can push the term before 'limited' loses operative meaning.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__judicial_ambiguity_reading, congress, agenda_setter,
    institutional, civilizational, arbitrage, national).

% Lobby for term extensions before existing terms expire, converting what was meant to be a temporary monopoly into a rolling, effectively renewable one. They do not need to win on the merits of public benefit — they need only supply Congress a rationale that survives rational basis review, which is a low bar. Their exit option is arbitrage: they capture legislative process rather than needing to exit any constraint.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__judicial_ambiguity_reading, copyright_holding_industries, beneficiary,
    organized, generational, arbitrage, national).

% Reviews challenges to term-extension statutes (e.g., Eldred v. Ashcroft) and applies rational basis review — asking only whether Congress could rationally believe the extension serves the constitutional purpose, not whether it actually does or whether 'limited Times' has been rendered meaningless by repeated extension. This is itself a choice: the judiciary sets the standard of review that determines how much of the constitutional text does real work.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__judicial_ambiguity_reading, judiciary, agenda_setter,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(copyright_constitutional_mandate__judicial_ambiguity_reading, judiciary, observer).

% Works that would have entered the public domain under a shorter term remain locked up for decades longer. This class has no organized voice, no lobbying capacity, and no standing mechanism to contest a term extension before it happens — only after, and only on a legal theory (structural limits on 'limited Times') that rational basis review makes very hard to win.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__judicial_ambiguity_reading, public_domain_entrants, payer,
    powerless, civilizational, trapped, national).

% Documentarians, archivists, remix artists, and educators who would build on older works pay licensing costs or forgo projects because works that should be free remain under copyright. Their exit is constrained: they can license (expensive), litigate fair use (expensive, uncertain), or abandon the project.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__judicial_ambiguity_reading, downstream_creators, payer,
    moderate, biographical, constrained, national).

% The word 'limited' in the Copyright Clause was meant to do binding work — to cap monopoly duration against legislative drift. Under rational-basis deference that word does almost no independent work: any term Congress sets, so long as it is not literally perpetual, survives review. The textual constraint itself is the thing eroded, not any single actor, which is why it is listed as a non-agent payer.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__judicial_ambiguity_reading, constitutional_fixity_as_a_constraint, payer,
    analytical, civilizational, analytical, national).
narrative_ontology:stakeholder_non_agent(copyright_constitutional_mandate__judicial_ambiguity_reading, constitutional_fixity_as_a_constraint).

% Petitioners and amici who argued that repeated retroactive extension violates the 'limited Times' requirement and encroaches on First Amendment interests. They made their case and lost under rational basis review; their argument that the standard of review itself is where the real fight is has not been taken up again with a different outcome.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__judicial_ambiguity_reading, eldred_style_litigants, excluded,
    moderate, biographical, trapped, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(copyright_constitutional_mandate__judicial_ambiguity_reading, copyright_holding_industries).
narrative_ontology:fixing_cost_class(copyright_constitutional_mandate__judicial_ambiguity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Separation of powers requires courts not to substitute their own economic judgment for Congress's when Congress acts within an enumerated power; rational basis review is the general default standard for economic and social legislation not implicating heightened scrutiny, and applying it here keeps courts from becoming a super-legislature second-guessing every term-length choice.
% TRANSFER_FUNCTION: Moves the power to define 'limited Times' from a judicially enforced textual boundary to a politically contestable, industry-lobbied legislative process. Concretely: it moves duration-of-monopoly determinations from constitutional adjudication to ordinary politics, and moves works that would otherwise enter the public domain into continued private control.
% ABSENT_VOICES: The public domain has no lobby. Future users of not-yet-created derivative works cannot be represented in the current legislative session. Public domain entrants and downstream creators are diffuse, unorganized, and structurally absent from the legislative process that repeatedly extends terms just before expiration.
% DISAPPEARANCE_RATIONALE: If rational basis deference were replaced with meaningful judicial scrutiny of whether term extensions serve the constitutional purpose (or with a bright-line reading of 'limited Times' as fixed at some historically grounded ceiling), Congress's practice of retroactive extension just before term expiration would very likely become unconstitutional, works would begin entering the public domain on a predictable non-negotiable schedule, and the copyright-extension lobbying cycle that has repeated roughly once per generation (1831, 1909, 1976, 1998) would lose its main lever.
% FOUNDING_PROBLEM: Courts needed a workable, administrable standard for reviewing economic and social legislation that does not require them to relitigate empirical policy judgments Congress is institutionally better positioned to make, and the Constitution needed a way to let Congress calibrate copyright duration to changing economic and technological conditions rather than freezing it at 1790s terms forever.
% FOUNDING_PROBLEM_CORROBORATION: The Supreme Court majority in Eldred v. Ashcroft (2003) attests the problem is live and rational basis is the correct, modest judicial posture. Dissenting justices (Breyer, Stevens) and a substantial body of legal scholarship (Lessig and others involved in the litigation) attest from outside the beneficiary set that the standard has been captured — that it no longer performs meaningful review of whether 'limited' retains content, and that repeated pre-expiration extension is empirically indistinguishable from a strategy to approach de facto perpetual terms without ever triggering the phrase 'perpetual.'
narrative_ontology:disappearance_verdict(copyright_constitutional_mandate__judicial_ambiguity_reading, world_rearranges).
narrative_ontology:founding_problem_status(copyright_constitutional_mandate__judicial_ambiguity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(copyright_constitutional_mandate__judicial_ambiguity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(copyright_constitutional_mandate__judicial_ambiguity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(copyright_constitutional_mandate__judicial_ambiguity_reading, 0.38, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(copyright_constitutional_mandate__judicial_ambiguity_reading_tests).
:- end_tests(copyright_constitutional_mandate__judicial_ambiguity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored low-to-moderate (0.38) because this reading concerns the review STANDARD, not the underlying commission or fee — the direct rent extraction happens downstream in the copyright-holding industries' pricing and licensing behavior, which is a separate constraint. What THIS constraint extracts is deference itself: judicial scrutiny that would otherwise catch and invalidate extensions that gut the 'limited Times' requirement. Suppression (0.42) reflects that the doctrine forecloses the primary avenue (constitutional litigation) by which the public-domain-entrant class could contest term extension; it is not zero because litigants can still challenge extensions on other grounds (First Amendment, as in Eldred) even if those grounds also fail under the same low-scrutiny logic. Theater ratio is authored notably high (0.55) and rising over the interval: rational basis review performs the FORM of judicial review — briefing, argument, a reasoned opinion — while, on this reading, doing very little of the SUBSTANTIVE work of checking whether 'limited' still means anything. The rising theater trajectory (0.1 in 1790 to 0.55 by 2003) tracks the growing gap between the appearance of a live constitutional constraint and its actual operative bite as extensions accumulated (1831, 1909, 1976, 1998) without any extension ever being invalidated on limited-times grounds.
 *
 * DIRECTIONALITY LOGIC:
 *   Congress is authored as institutional beneficiary because rational basis deference maximizes its discretion — it can respond to industry lobbying without fear of judicial invalidation, which is itself a form of institutional power retained. Copyright-holding industries are the organized economic beneficiary who converts that discretion into successive rent-preserving extensions. Public domain entrants are the clearest victim class: trapped, powerless, and diffuse, with no seat at the legislative table and a judicial door that rational basis review keeps almost fully closed. The non-agent payer (constitutional_fixity_as_a_constraint) is included because the erosion is not fully captured by naming only human/institutional victims — the text's binding force is itself what is being spent down.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (a workable standard letting courts defer to legislative economic judgment without becoming a policy-review body) is still partially live — courts generally should not micromanage economic legislation. But the founding_problem_status is authored 'contested' because the SPECIFIC application here — using that general deference logic to immunize retroactive, repeated, pre-expiration extensions of an enumerated power that contains its own textual limit ('limited Times') — is a different and more contestable claim than ordinary rational-basis deference to, say, a minimum-wage statute. The mismatch the schema's consumer looks for is exactly here: if founding_problem_status is read as 'dead' (the specific textual constraint's function is gone) while disappearance_verdict is 'world_rearranges' (real institutional dependencies exist), that combination signals a capture/zombie pattern worth flagging — deference doctrine surviving because it now serves an interest (industry's extension lobby) unrelated to the institutional-competence rationale that justified it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    rational_basis_as_neutral_or_captured_standard,
    'Is rational basis review here a neutral, generally-applicable standard correctly extended to copyright term legislation, or has it been effectively captured — applied specifically to immunize a pattern of extension that the text''s own ''limited Times'' language was meant to prevent?',
    'Comparative doctrinal analysis: examine whether courts apply equally minimal scrutiny to OTHER enumerated-power textual limits with comparably explicit qualifying language, or whether copyright term review is anomalously deferential relative to structurally similar constitutional ceilings.',
    'If comparably-limited enumerated powers receive meaningfully less deferential review, that supports the capture reading and would push this constraint toward snare; if rational basis is applied with genuine uniformity across analogous textual limits, that supports reading this as ordinary (non-captured) doctrine and stabilizes it nearer tangled_rope or even rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(rational_basis_as_neutral_or_captured_standard, conceptual, 'Whether deferential review here is neutral doctrine or captured application.').

omega_variable(
    kernel_reading_location_of_disagreement,
    'Where exactly does this reading''s disagreement with the sibling readings live: is it a disagreement about what copyright IS FOR (substantive, as in public_scaffold vs. corporate_enclosure), or is it orthogonal — a disagreement about WHO GETS TO DECIDE term length with how much judicial oversight, which could combine with either substantive theory?',
    'Map each sibling reading''s position against a 2x2 of (substantive theory of copyright) x (standard of judicial review); check whether real-world advocates/judges combine judicial_ambiguity with EITHER public_scaffold or corporate_enclosure in practice, which would confirm orthogonality.',
    'If judicial_ambiguity is genuinely orthogonal to the substantive question, this constraint''s low-to-moderate epsilon is stable regardless of which substantive theory eventually prevails, because the extraction it measures (loss of textual bite) is procedural, not tied to outcome. If it turns out judicial deference and the corporate_enclosure reading are empirically co-occurring (deference is instrumentally chosen BECAUSE it enables enclosure), that would suggest an influences edge stronger than currently modeled, potentially warranting elevation toward forecloses.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_location_of_disagreement, conceptual, 'Whether the judicial-deference axis is structurally independent of the substantive-purpose axis among the sibling readings.').

omega_variable(
    eldred_precedential_durability,
    'Is Eldred v. Ashcroft''s rational-basis holding a stable, durable precedent, or is it vulnerable to being revisited given post-2003 shifts in judicial philosophy regarding textualism and enumerated-powers limits?',
    'Track subsequent circuit-level and Supreme Court treatment of Eldred; monitor whether textualist/originalist judicial appointments produce renewed ''limited Times'' challenges with different outcomes.',
    'If Eldred is overturned or narrowed, the entire structural picture this constraint describes changes — extraction and suppression would need to be re-measured against a heightened-scrutiny baseline, and this reading''s low epsilon could no longer be assumed stable across time.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(eldred_precedential_durability, empirical, 'Durability of the precedent underlying this reading''s low-scrutiny characterization.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(copyright_constitutional_mandate__judicial_ambiguity_reading, 1790, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(copy_tr_t1790, copyright_constitutional_mandate__judicial_ambiguity_reading, theater_ratio, 1790, 0.1).
narrative_ontology:measurement_basis(copy_tr_t1790, observed).
narrative_ontology:measurement(copy_tr_t1909, copyright_constitutional_mandate__judicial_ambiguity_reading, theater_ratio, 1909, 0.2).
narrative_ontology:measurement_basis(copy_tr_t1909, observed).
narrative_ontology:measurement(copy_tr_t1976, copyright_constitutional_mandate__judicial_ambiguity_reading, theater_ratio, 1976, 0.35).
narrative_ontology:measurement_basis(copy_tr_t1976, observed).
narrative_ontology:measurement(copy_tr_t1998, copyright_constitutional_mandate__judicial_ambiguity_reading, theater_ratio, 1998, 0.5).
narrative_ontology:measurement_basis(copy_tr_t1998, observed).
narrative_ontology:measurement(copy_tr_t2003, copyright_constitutional_mandate__judicial_ambiguity_reading, theater_ratio, 2003, 0.55).
narrative_ontology:measurement_basis(copy_tr_t2003, observed).
narrative_ontology:measurement(copy_tr_t2024, copyright_constitutional_mandate__judicial_ambiguity_reading, theater_ratio, 2024, 0.55).
narrative_ontology:measurement_basis(copy_tr_t2024, observed).

% Extraction over time
narrative_ontology:measurement(copy_be_t1790, copyright_constitutional_mandate__judicial_ambiguity_reading, base_extractiveness, 1790, 0.1).
narrative_ontology:measurement_basis(copy_be_t1790, observed).
narrative_ontology:measurement(copy_be_t1909, copyright_constitutional_mandate__judicial_ambiguity_reading, base_extractiveness, 1909, 0.18).
narrative_ontology:measurement_basis(copy_be_t1909, observed).
narrative_ontology:measurement(copy_be_t1976, copyright_constitutional_mandate__judicial_ambiguity_reading, base_extractiveness, 1976, 0.28).
narrative_ontology:measurement_basis(copy_be_t1976, observed).
narrative_ontology:measurement(copy_be_t1998, copyright_constitutional_mandate__judicial_ambiguity_reading, base_extractiveness, 1998, 0.36).
narrative_ontology:measurement_basis(copy_be_t1998, observed).
narrative_ontology:measurement(copy_be_t2003, copyright_constitutional_mandate__judicial_ambiguity_reading, base_extractiveness, 2003, 0.38).
narrative_ontology:measurement_basis(copy_be_t2003, observed).
narrative_ontology:measurement(copy_be_t2024, copyright_constitutional_mandate__judicial_ambiguity_reading, base_extractiveness, 2024, 0.38).
narrative_ontology:measurement_basis(copy_be_t2024, observed).

% Suppression requirement over time
narrative_ontology:measurement(copy_su_t1790, copyright_constitutional_mandate__judicial_ambiguity_reading, suppression_requirement, 1790, 0.15).
narrative_ontology:measurement_basis(copy_su_t1790, observed).
narrative_ontology:measurement(copy_su_t1909, copyright_constitutional_mandate__judicial_ambiguity_reading, suppression_requirement, 1909, 0.22).
narrative_ontology:measurement_basis(copy_su_t1909, observed).
narrative_ontology:measurement(copy_su_t1976, copyright_constitutional_mandate__judicial_ambiguity_reading, suppression_requirement, 1976, 0.3).
narrative_ontology:measurement_basis(copy_su_t1976, observed).
narrative_ontology:measurement(copy_su_t1998, copyright_constitutional_mandate__judicial_ambiguity_reading, suppression_requirement, 1998, 0.4).
narrative_ontology:measurement_basis(copy_su_t1998, observed).
narrative_ontology:measurement(copy_su_t2003, copyright_constitutional_mandate__judicial_ambiguity_reading, suppression_requirement, 2003, 0.42).
narrative_ontology:measurement_basis(copy_su_t2003, observed).
narrative_ontology:measurement(copy_su_t2024, copyright_constitutional_mandate__judicial_ambiguity_reading, suppression_requirement, 2024, 0.42).
narrative_ontology:measurement_basis(copy_su_t2024, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(copyright_constitutional_mandate__judicial_ambiguity_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(copyright_constitutional_mandate__judicial_ambiguity_reading, corporate_enclosure_reading).
narrative_ontology:affects_constraint(copyright_constitutional_mandate__judicial_ambiguity_reading, public_scaffold_reading).

% DUAL FORMULATION NOTE:
% This story is one of three readings of the copyright_constitutional_mandate kernel. corporate_enclosure_reading and public_scaffold_reading disagree about the SUBSTANTIVE purpose of copyright and therefore instantiate different beneficiary/victim structures and epsilon values around the underlying property right itself. This story (judicial_ambiguity_reading) is procedurally orthogonal: it concerns the standard of review courts apply to term-length legislation, which is compatible in principle with either substantive theory prevailing. It is linked to both siblings because the deference doctrine it describes is the mechanism that permits drift toward whichever substantive reading Congress's legislative majority currently favors, without triggering constitutional invalidation under either theory.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
