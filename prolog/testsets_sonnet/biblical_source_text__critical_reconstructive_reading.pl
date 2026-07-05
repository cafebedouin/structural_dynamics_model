% ============================================================================
% CONSTRAINT STORY: biblical_source_text__critical_reconstructive_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_biblical_source_text__critical_reconstructive_reading, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: biblical_source_text__critical_reconstructive_reading
 *   human_readable: Critical-Reconstructive Reading of Biblical Source Text (Textual Priority Doctrine)
 *   domain: religious/academic/textual
 *
 * SUMMARY:
 *   This story instantiates the critical-reconstructive reading of the
 *   biblical-source-text kernel: the claim that historical recovery of a
 *   hypothetical original text is methodologically prior, and that neither
 *   literary structure nor theological meaning may be treated as settled
 *   until the textual basis itself is established through manuscript
 *   stemmatics and apparatus criticus. This is a distinct constraint from the
 *   formal-equivalence reading (source-structure fidelity as primary) and the
 *   dynamic-equivalence reading (target-language communicative effect as
 *   primary) — the three readings assign priority to different things
 *   (textual layer, structural layer, communicative layer respectively) and
 *   produce different beneficiary/victim structures and different ε profiles.
 *   This story's ε is authored specifically for the textual-priority claim:
 *   low extraction on the academic readers who operate fluently within the
 *   discipline's own norms (for them the apparatus is a genuine research
 *   tool), and substantially higher effective extraction on confessional
 *   communities whose received-text basis is destabilized by a norm they did
 *   not choose and often cannot evaluate on its own methodological terms.
 *
 * KEY AGENTS:
 *   - textual_critics: agenda_setter (institutional/arbitrage) — sets and enforces the textual-priority norm
 *   - academic_biblical_scholarship: beneficiary (institutional/arbitrage) — the disciplinary field sustained by the priority claim
 *   - critical_edition_publishers: beneficiary (organized/mobile) — commercial beneficiary of the operationalized norm
 *   - confessional_communities: payer (organized/constrained) — received-text basis destabilized
 *   - lay_bible_readers: payer (powerless/trapped) — inherit destabilization without adjudicating capacity
 *   - translation_committees_under_confessional_mandate: payer/excluded (moderate/constrained) — caught between guild and congregation
 *   - manuscript_evidence: observer, non-agent — the evidentiary base itself
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(biblical_source_text__critical_reconstructive_reading, 0.58).
domain_priors:suppression_score(biblical_source_text__critical_reconstructive_reading, 0.34).
domain_priors:theater_ratio(biblical_source_text__critical_reconstructive_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(biblical_source_text__critical_reconstructive_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(biblical_source_text__critical_reconstructive_reading, suppression_requirement, 0.34).
narrative_ontology:constraint_metric(biblical_source_text__critical_reconstructive_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(biblical_source_text__critical_reconstructive_reading, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(biblical_source_text__critical_reconstructive_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(biblical_source_text__critical_reconstructive_reading, tangled_rope).
narrative_ontology:human_readable(biblical_source_text__critical_reconstructive_reading, "Critical-Reconstructive Reading of Biblical Source Text (Textual Priority Doctrine)").
narrative_ontology:topic_domain(biblical_source_text__critical_reconstructive_reading, "religious/academic/textual").

domain_priors:requires_active_enforcement(biblical_source_text__critical_reconstructive_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(biblical_source_text__critical_reconstructive_reading, '4a71a545-764f-4a4d-b1f5-21dcf110d715').
narrative_ontology:cs_kernel_codification('4a71a545-764f-4a4d-b1f5-21dcf110d715', distributed).
narrative_ontology:cs_authority_grounding('4a71a545-764f-4a4d-b1f5-21dcf110d715', expertise).
narrative_ontology:cs_interpretation_layer_present('4a71a545-764f-4a4d-b1f5-21dcf110d715').
narrative_ontology:cs_reading_relation('4a71a545-764f-4a4d-b1f5-21dcf110d715', biblical_source_text__formal_equivalence_reading, influences).
narrative_ontology:cs_reading_relation('4a71a545-764f-4a4d-b1f5-21dcf110d715', biblical_source_text__dynamic_equivalence_reading, coexists_with).
narrative_ontology:cs_axiom('4a71a545-764f-4a4d-b1f5-21dcf110d715', foundational, textual_basis_methodologically_prior_to_meaning).
narrative_ontology:cs_axiom_status(textual_basis_methodologically_prior_to_meaning, holdable).
narrative_ontology:cs_axiom_grounding('4a71a545-764f-4a4d-b1f5-21dcf110d715', textual_basis_methodologically_prior_to_meaning, conventional).
narrative_ontology:cs_axiom('4a71a545-764f-4a4d-b1f5-21dcf110d715', secondary, hypothetical_original_recoverable_in_principle).
narrative_ontology:cs_axiom_status(hypothetical_original_recoverable_in_principle, holdable).
narrative_ontology:cs_axiom_grounding('4a71a545-764f-4a4d-b1f5-21dcf110d715', hypothetical_original_recoverable_in_principle, empirically_contingent).
narrative_ontology:cs_reference_frame('4a71a545-764f-4a4d-b1f5-21dcf110d715', manuscript_stemmatic_priority).
narrative_ontology:cs_drift_state('4a71a545-764f-4a4d-b1f5-21dcf110d715', post_critical_edition_consolidation, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('4a71a545-764f-4a4d-b1f5-21dcf110d715', '').
narrative_ontology:cs_kernel_id(biblical_source_text__critical_reconstructive_reading, biblical_source_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(biblical_source_text__critical_reconstructive_reading, academic_biblical_scholarship).
narrative_ontology:constraint_beneficiary(biblical_source_text__critical_reconstructive_reading, critical_edition_publishers).
narrative_ontology:constraint_beneficiary(biblical_source_text__critical_reconstructive_reading, textual_critics).
narrative_ontology:constraint_victim(biblical_source_text__critical_reconstructive_reading, confessional_communities).
narrative_ontology:constraint_victim(biblical_source_text__critical_reconstructive_reading, lay_bible_readers).
narrative_ontology:constraint_victim(biblical_source_text__critical_reconstructive_reading, translation_committees_under_confessional_mandate).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Reconstruct a hypothetical Urtext from manuscript variants, apparatus criticus, and stemmatics, and set the discipline's norm that no structural or semantic reading may be authoritative until the underlying textual layer is settled. Controls what counts as the base text every downstream translation and interpretation project must work from. Career advancement, publication, and institutional standing (critical editions, commentaries, professorships) run through this priority claim.
narrative_ontology:constraint_stakeholder(biblical_source_text__critical_reconstructive_reading, textual_critics, agenda_setter,
    institutional, generational, arbitrage, global).

% The discipline as a field benefits from textual priority being treated as foundational: it generates a perpetually renewable research program (new manuscript discoveries, refined stemmatics, revised critical editions) and positions the guild as the necessary gatekeeper between any raw manuscript evidence and any theological or literary claim built on it.
narrative_ontology:constraint_stakeholder(biblical_source_text__critical_reconstructive_reading, academic_biblical_scholarship, beneficiary,
    institutional, generational, arbitrage, global).

% Produce and sell the critical editions (Nestle-Aland, BHS, and successors) that operationalize the textual-priority claim. Revenue and institutional relevance depend on the ongoing acceptance that a reconstructed critical text, not any single manuscript or received tradition, is the necessary starting point for serious study.
narrative_ontology:constraint_stakeholder(biblical_source_text__critical_reconstructive_reading, critical_edition_publishers, beneficiary,
    organized, generational, mobile, global).

% Hold received texts (Masoretic Text, Textus Receptus, or denominationally sanctioned canons) as already-settled bases for doctrine and liturgy. The critical-reconstructive claim, when it enters seminary training, translation committees, or public discourse, destabilizes the received basis by treating it as merely one witness among many to a hypothetical original, forcing communities to either defer to academic reconstruction or defend their received text against a discipline that treats that defense as pre-critical.
narrative_ontology:constraint_stakeholder(biblical_source_text__critical_reconstructive_reading, confessional_communities, payer,
    organized, civilizational, constrained, global).

% Encounter footnotes, textual apparatus, and 'earliest manuscripts do not include this passage' notices in pew Bibles without the training to evaluate the underlying text-critical argument. They inherit the destabilization (their received text is marked provisional) without inheriting the expertise or institutional standing that would let them adjudicate it themselves.
narrative_ontology:constraint_stakeholder(biblical_source_text__critical_reconstructive_reading, lay_bible_readers, payer,
    powerless, biographical, trapped, local).

% Denominational translation committees must decide whether to follow the critical-reconstructive base text or a denomination's traditionally received text. Choosing the critical base risks alienating their confessional constituency; declining it risks being dismissed by the academic guild as methodologically unserious. Their pastoral concerns about congregational reception are rarely represented within the textual-critical literature that sets the norm.
narrative_ontology:constraint_stakeholder(biblical_source_text__critical_reconstructive_reading, translation_committees_under_confessional_mandate, payer,
    moderate, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(biblical_source_text__critical_reconstructive_reading, translation_committees_under_confessional_mandate, excluded).

% The surviving papyri, codices, versions, and citations that constitute the actual evidentiary base. Not an actor — included for completeness since the reading's legitimacy claim rests entirely on what this evidence can and cannot establish.
narrative_ontology:constraint_stakeholder(biblical_source_text__critical_reconstructive_reading, manuscript_evidence, observer,
    analytical, civilizational, analytical, global).
narrative_ontology:stakeholder_non_agent(biblical_source_text__critical_reconstructive_reading, manuscript_evidence).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a disciplined, falsifiable method for adjudicating among divergent manuscript witnesses before any structural or theological claim is built on a passage — genuinely solves the problem that no single manuscript is error-free and variants must be weighed rather than assumed away.
% TRANSFER_FUNCTION: Moves interpretive authority from confessional tradition and received-text communities to the academic guild that controls stemmatic method and critical apparatus; moves resources (translation committee time, seminary curriculum hours, publishing revenue) toward critical-edition production and away from received-text-based scholarship.
% ABSENT_VOICES: Confessional communities whose liturgical and doctrinal life is built on a received text rarely have a formal voice inside text-critical methodology debates; when they raise concerns about destabilization they are typically read by the guild as a pre-critical objection rather than a legitimate stakeholder claim, and are structurally outside the peer-review apparatus that sets the norm.
% DISAPPEARANCE_RATIONALE: If textual priority as a governing norm vanished overnight, the academic guild's disagreement is that biblical scholarship would collapse into unmethodical eclecticism and confessional communities would lose access to careful adjudication among real manuscript divergences (world_rearranges argument). Confessional communities' disagreement is that they already possess functioning received texts that have stably supported doctrine and liturgy for centuries independent of the reconstructive program, and its disappearance would mainly return authority to traditions that never depended on it (world_unchanged argument). Both readings are live and the story does not adjudicate between them.
% FOUNDING_PROBLEM: Surviving biblical manuscripts diverge from one another in thousands of places (omissions, additions, scribal errors, deliberate harmonizations); some principled method is needed to determine which readings are more likely original before translation or theological argument proceeds.
% FOUNDING_PROBLEM_CORROBORATION: Manuscript divergence itself is independently attested by paleographers and codicologists outside biblical studies proper (e.g., classical philologists working on Homeric or other ancient-text transmission), confirming the underlying problem is real and not manufactured by the guild. However, whether TEXTUAL PRIORITY specifically (as opposed to, say, reception-historical or canonical approaches) is the necessary FIRST step is attested mainly by textual critics themselves and by publishers of critical editions — both benefiting parties. Confessional-tradition scholars and reception-historical critics from outside the text-critical guild dispute that priority ordering while agreeing the underlying manuscript-divergence problem is real.
narrative_ontology:disappearance_verdict(biblical_source_text__critical_reconstructive_reading, contested).
narrative_ontology:founding_problem_status(biblical_source_text__critical_reconstructive_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(biblical_source_text__critical_reconstructive_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(biblical_source_text__critical_reconstructive_reading, 'none', 1).
narrative_ontology:epsilon_provenance(biblical_source_text__critical_reconstructive_reading, 0.58, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(biblical_source_text__critical_reconstructive_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(biblical_source_text__critical_reconstructive_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(biblical_source_text__critical_reconstructive_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored moderate-to-substantial (0.58 at interval end) and rising: the priority claim itself is methodologically defensible on manuscript evidence, but its institutionalization as an unconditional PRECEDENCE rule (nothing may be said about meaning or structure until textual questions are 'settled', which they structurally never fully are, given the character of an open, perpetually-revisable stemmatic program) has increasingly functioned to route interpretive and financial authority toward the guild that administers the reconstruction. Suppression is moderate (0.34) — no one is coerced into accepting the priority claim, but seminary curricula, publishing gatekeeping, and academic peer review create substantial soft pressure toward it. Theater ratio is low-moderate (0.22): most of the underlying text-critical labor is genuine scholarship, not performance, though the increasingly totalizing framing ('nothing may be privileged until textual basis is established') outruns what the manuscript evidence can actually deliver, given that a hypothetical Urtext for most passages will never be established with certainty. Accessibility collapse is moderate (0.42): confessional and lay alternatives to the critical-reconstructive frame persist and are actively practiced, they simply carry reduced institutional legitimacy within academic discourse. Resistance is high (0.68): confessional communities and reception-historical scholars actively contest textual-priority ordering, which is itself evidence this is not a settled Mountain-type claim.
 *
 * DIRECTIONALITY LOGIC:
 *   Textual critics and the academic guild sit near the beneficiary end: they administer the norm, and their institutional standing and research funding derive from its continued operation (d low). Confessional communities and lay readers sit near the target end: they bear the destabilization of a received text they use for doctrine and liturgy, with limited capacity (lay readers) or institutional leverage (confessional bodies) to contest the norm on its own methodological terms (d high). Translation committees are cross-pressured — structurally payers, functionally sometimes coordinated with the guild when they adopt the critical base text, hence the secondary excluded role reflecting that their pastoral-reception concerns rarely enter the methodological literature that sets the norm.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (manuscript divergence requires principled adjudication) remains genuinely live — this prevents dismissing the entire constraint as pure extraction. What the tangled_rope classification isolates is the SPECIFIC claim that textual priority must be resolved BEFORE any structural or semantic reading is permitted — an ordering claim, not merely an evidentiary one. The manuscript-divergence problem justifies careful textual criticism; it does not by itself justify a totalizing precedence rule that treats every non-critical reading tradition as provisional pending a reconstruction project with no natural termination point. That gap between the justified coordination function and the totalizing precedence claim is where the extraction lives.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    textual_priority_committer_disagreement_location,
    'Where exactly does the critical-reconstructive reading''s core premise (textual recovery must precede structural/semantic privileging) conflict with the sibling readings'' premises, and is the conflict logical or merely practical?',
    'Formal comparison of each reading''s ordering claim: does the formal-equivalence reading''s structural-fidelity-first premise logically require rejecting textual priority, or can a scholar hold both sequentially (establish text, then honor structure)? Survey of practicing translators/scholars on whether they experience the readings as mutually exclusive commitments or compatible successive stages.',
    'If the readings are logically compatible as sequential stages (text first, then structure, then meaning) rather than competing exclusive premises, the coexists_with relation is confirmed and no reading forecloses another. If a genuine logical incompatibility is found (e.g., the reconstructive reading''s premise that no received text may be privileged until reconstruction is complete directly negates the formal-equivalence reading''s premise that the received/attested structural text IS the object of fidelity), a forecloses relation would need to be reconsidered for that pair.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(textual_priority_committer_disagreement_location, conceptual, 'Whether the three kernel readings are logically exclusive or merely differently prioritized within compatible workflows.').

omega_variable(
    reconstruction_terminability,
    'Does the critical-reconstructive program have a natural termination point (a moment at which the textual basis is ''established'' and structural/semantic privileging becomes permissible), or is textual reconstruction structurally open-ended given the character of manuscript evidence?',
    'Historical survey of critical-edition revision cycles (e.g., successive editions of Nestle-Aland, BHS/BHQ) to determine whether confidence intervals on reconstructed readings are converging toward stability or remain perpetually revisable as new manuscript evidence and methods emerge.',
    'If reconstruction converges to practical stability, the priority claim functions as a genuine temporary methodological stage (closer to a scaffold with an implicit, if unstated, sunset) and the extraction reading would need revision downward. If reconstruction is structurally open-ended (each generation''s method revises the last), the precedence rule functions as a perpetual deferral mechanism that indefinitely subordinates structural/semantic authority to a guild-administered process with no natural endpoint — supporting the higher extractiveness reading authored here.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reconstruction_terminability, empirical, 'Whether textual reconstruction has a natural stopping point or is open-ended by design.').

omega_variable(
    guild_self_interest_vs_genuine_necessity,
    'Is the totalizing form of the textual-priority claim (''neither structure nor meaning can be privileged until textual basis is established'') a necessary methodological safeguard, or does it reflect the guild''s institutional interest in perpetuating a research program that only it is credentialed to administer?',
    'Compare disciplines with analogous manuscript-transmission problems (classical philology, Talmudic textual criticism) to see whether they impose an equally totalizing precedence rule or instead permit provisional structural/semantic work to proceed alongside ongoing textual refinement.',
    'If comparable disciplines proceed with provisional structural/semantic analysis without waiting for textual settlement, this weakens the necessity claim and strengthens the reading of the precedence rule as guild self-interest (supporting the tangled_rope classification with more weight on the extraction side). If comparable disciplines uniformly impose the same precedence, the coordination function is better supported as a genuine cross-disciplinary methodological necessity.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(guild_self_interest_vs_genuine_necessity, empirical, 'Cross-disciplinary comparison test for whether the precedence rule is necessity or self-interested gatekeeping.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(biblical_source_text__critical_reconstructive_reading, 0, 200).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bibl_tr_t0, biblical_source_text__critical_reconstructive_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement_basis(bibl_tr_t0, observed).
narrative_ontology:measurement(bibl_tr_t40, biblical_source_text__critical_reconstructive_reading, theater_ratio, 40, 0.12).
narrative_ontology:measurement_basis(bibl_tr_t40, observed).
narrative_ontology:measurement(bibl_tr_t80, biblical_source_text__critical_reconstructive_reading, theater_ratio, 80, 0.15).
narrative_ontology:measurement_basis(bibl_tr_t80, observed).
narrative_ontology:measurement(bibl_tr_t120, biblical_source_text__critical_reconstructive_reading, theater_ratio, 120, 0.17).
narrative_ontology:measurement_basis(bibl_tr_t120, observed).
narrative_ontology:measurement(bibl_tr_t160, biblical_source_text__critical_reconstructive_reading, theater_ratio, 160, 0.2).
narrative_ontology:measurement_basis(bibl_tr_t160, observed).
narrative_ontology:measurement(bibl_tr_t200, biblical_source_text__critical_reconstructive_reading, theater_ratio, 200, 0.22).
narrative_ontology:measurement_basis(bibl_tr_t200, observed).

% Extraction over time
narrative_ontology:measurement(bibl_be_t0, biblical_source_text__critical_reconstructive_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement_basis(bibl_be_t0, observed).
narrative_ontology:measurement(bibl_be_t40, biblical_source_text__critical_reconstructive_reading, base_extractiveness, 40, 0.36).
narrative_ontology:measurement_basis(bibl_be_t40, observed).
narrative_ontology:measurement(bibl_be_t80, biblical_source_text__critical_reconstructive_reading, base_extractiveness, 80, 0.44).
narrative_ontology:measurement_basis(bibl_be_t80, observed).
narrative_ontology:measurement(bibl_be_t120, biblical_source_text__critical_reconstructive_reading, base_extractiveness, 120, 0.5).
narrative_ontology:measurement_basis(bibl_be_t120, observed).
narrative_ontology:measurement(bibl_be_t160, biblical_source_text__critical_reconstructive_reading, base_extractiveness, 160, 0.55).
narrative_ontology:measurement_basis(bibl_be_t160, observed).
narrative_ontology:measurement(bibl_be_t200, biblical_source_text__critical_reconstructive_reading, base_extractiveness, 200, 0.58).
narrative_ontology:measurement_basis(bibl_be_t200, observed).

% Suppression requirement over time
narrative_ontology:measurement(bibl_su_t0, biblical_source_text__critical_reconstructive_reading, suppression_requirement, 0, 0.2).
narrative_ontology:measurement_basis(bibl_su_t0, observed).
narrative_ontology:measurement(bibl_su_t40, biblical_source_text__critical_reconstructive_reading, suppression_requirement, 40, 0.24).
narrative_ontology:measurement_basis(bibl_su_t40, observed).
narrative_ontology:measurement(bibl_su_t80, biblical_source_text__critical_reconstructive_reading, suppression_requirement, 80, 0.27).
narrative_ontology:measurement_basis(bibl_su_t80, observed).
narrative_ontology:measurement(bibl_su_t120, biblical_source_text__critical_reconstructive_reading, suppression_requirement, 120, 0.3).
narrative_ontology:measurement_basis(bibl_su_t120, observed).
narrative_ontology:measurement(bibl_su_t160, biblical_source_text__critical_reconstructive_reading, suppression_requirement, 160, 0.32).
narrative_ontology:measurement_basis(bibl_su_t160, observed).
narrative_ontology:measurement(bibl_su_t200, biblical_source_text__critical_reconstructive_reading, suppression_requirement, 200, 0.34).
narrative_ontology:measurement_basis(bibl_su_t200, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(biblical_source_text__critical_reconstructive_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(biblical_source_text__critical_reconstructive_reading, 0.1).
narrative_ontology:affects_constraint(biblical_source_text__critical_reconstructive_reading, biblical_source_text__formal_equivalence_reading).
narrative_ontology:affects_constraint(biblical_source_text__critical_reconstructive_reading, biblical_source_text__dynamic_equivalence_reading).

% DUAL FORMULATION NOTE:
% This story is one of three sibling readings of the biblical_source_text kernel. The formal_equivalence_reading and dynamic_equivalence_reading are separate constraint stories with independent ε values, beneficiary/victim structures, and classifications — they are NOT alternative measurements of this same constraint, but structurally distinct constraints that happen to share a contested kernel (what priority ordering governs biblical translation/interpretation methodology). This reading's ε (0.58, tangled_rope-leaning) reflects the textual-critical guild's extraction from confessional communities specifically; the sibling readings' ε values will reflect different beneficiary/victim pairs (e.g., source-language literalist traditions vs. mission/accessibility-oriented bodies) and should not be averaged or reconciled with this one.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
