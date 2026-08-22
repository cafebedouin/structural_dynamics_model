% ============================================================================
% CONSTRAINT STORY: speech_protection_kernel__dignity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_speech_protection_kernel__dignity_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: speech_protection_kernel__dignity_reading
 *   human_readable: Speech Protection Conditional on Dignity Maintenance (dignity reading)
 *   domain: constitutional_law/political_philosophy
 *
 * SUMMARY:
 *   The dignity reading of speech protection grounds the right in the
 *   principle that speech cannot legitimately function as structural
 *   subordination of target groups. Under this reading, hate speech, group
 *   libel, and utterances that deny the equal humanity or standing of groups
 *   are unprotected because they perform subordination rather than expressing
 *   or informing. The constraint operates as a tangled rope: it solves a
 *   genuine coordination problem (equal participation in speech despite
 *   historical group hierarchy) through a mechanism that requires active
 *   enforcement and produces asymmetric effects (restricting some speakers'
 *   utterances to protect others' equal standing). The claim/metric
 *   divergence is intentional and reveals the committer structure: this
 *   reading claims tangled_rope because it sees the coordination function as
 *   genuine and the enforcement as justified; absolutist readings would claim
 *   snare (enforcement without real coordination benefit, only suppression).
 *   The metrics are authored from the dignity reading's
 *   perspective—extractiveness measures what is extracted from restricted
 *   speakers, suppression measures the enforcement machinery required.
 *
 * KEY AGENTS:
 *   - structurally_subordinated_groups: the primary beneficiary seat; their exit is identity-locked because group membership cannot be abandoned.
 *   - speakers_targeting_group_subordination: the payer seat; their speech is restricted; exit is constrained but not identity-locked.
 *   - judges_and_adjudicators: the agenda-setting seat; they interpret and enforce the boundary between protected and unprotected speech.
 *   - absolutist_reading_advocates: the excluded seat; they would reject the dignity reading's core premise but are not structural participants in its authority framework.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(speech_protection_kernel__dignity_reading, 0.62).
domain_priors:suppression_score(speech_protection_kernel__dignity_reading, 0.71).
domain_priors:theater_ratio(speech_protection_kernel__dignity_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(speech_protection_kernel__dignity_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(speech_protection_kernel__dignity_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(speech_protection_kernel__dignity_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(speech_protection_kernel__dignity_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(speech_protection_kernel__dignity_reading, resistance, 0.79).

% --- Constraint claim ---
narrative_ontology:constraint_claim(speech_protection_kernel__dignity_reading, tangled_rope).
narrative_ontology:human_readable(speech_protection_kernel__dignity_reading, "Speech Protection Conditional on Dignity Maintenance (dignity reading)").
narrative_ontology:topic_domain(speech_protection_kernel__dignity_reading, "constitutional_law/political_philosophy").

domain_priors:requires_active_enforcement(speech_protection_kernel__dignity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(speech_protection_kernel__dignity_reading, '42b05b60-7b11-44b5-9574-ec586656a282').
narrative_ontology:cs_kernel_codification('42b05b60-7b11-44b5-9574-ec586656a282', fixed_text).
narrative_ontology:cs_authority_grounding('42b05b60-7b11-44b5-9574-ec586656a282', lineage).
narrative_ontology:cs_interpretation_layer_present('42b05b60-7b11-44b5-9574-ec586656a282').
narrative_ontology:cs_reading_relation('42b05b60-7b11-44b5-9574-ec586656a282', speech_protection_kernel__absolutist_reading, forecloses).
narrative_ontology:cs_reading_relation('42b05b60-7b11-44b5-9574-ec586656a282', speech_protection_kernel__harm_threshold_reading, influences).
narrative_ontology:cs_reading_relation('42b05b60-7b11-44b5-9574-ec586656a282', speech_protection_kernel__marketplace_reading, coexists_with).
narrative_ontology:cs_reading_relation('42b05b60-7b11-44b5-9574-ec586656a282', speech_protection_kernel__democratic_participation_reading, coexists_with).
narrative_ontology:cs_axiom('42b05b60-7b11-44b5-9574-ec586656a282', foundational, group_harm_distinct_from_individual_harm).
narrative_ontology:cs_axiom_status(group_harm_distinct_from_individual_harm, holdable).
narrative_ontology:cs_axiom_grounding('42b05b60-7b11-44b5-9574-ec586656a282', group_harm_distinct_from_individual_harm, deontological).
narrative_ontology:cs_axiom('42b05b60-7b11-44b5-9574-ec586656a282', foundational, equal_dignity_prerequisite_for_speech_equality).
narrative_ontology:cs_axiom_status(equal_dignity_prerequisite_for_speech_equality, holdable).
narrative_ontology:cs_axiom_grounding('42b05b60-7b11-44b5-9574-ec586656a282', equal_dignity_prerequisite_for_speech_equality, deontological).
narrative_ontology:cs_axiom('42b05b60-7b11-44b5-9574-ec586656a282', secondary, subordinating_speech_unprotected).
narrative_ontology:cs_axiom_status(subordinating_speech_unprotected, holdable).
narrative_ontology:cs_axiom_grounding('42b05b60-7b11-44b5-9574-ec586656a282', subordinating_speech_unprotected, deontological).
narrative_ontology:cs_reference_frame('42b05b60-7b11-44b5-9574-ec586656a282', speech_protection_grounded_in_equal_dignity).
narrative_ontology:cs_drift_state('42b05b60-7b11-44b5-9574-ec586656a282', contemporary_contestation_era, gap(authority_erosion, substantial, true)).
narrative_ontology:cs_created_at('42b05b60-7b11-44b5-9574-ec586656a282', '').
narrative_ontology:cs_kernel_id(speech_protection_kernel__dignity_reading, speech_protection_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(speech_protection_kernel__dignity_reading, structurally_subordinated_groups).
narrative_ontology:constraint_victim(speech_protection_kernel__dignity_reading, speakers_targeting_group_subordination).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(speech_protection_kernel__dignity_reading, counter_speech_advocates).
narrative_ontology:constraint_beneficiary(speech_protection_kernel__dignity_reading, marginalized_speech_communities).
narrative_ontology:constraint_victim(speech_protection_kernel__dignity_reading, marginalized_speech_communities).
narrative_ontology:constraint_vindicates(speech_protection_kernel__dignity_reading, group_harm_is_distinct_from_individual_harm).
narrative_ontology:constraint_vindicates(speech_protection_kernel__dignity_reading, equal_dignity_is_prerequisite_for_speech_equality).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Members of groups historically or contemporaneously subordinated through law or systemic practice (racial minorities, religious minorities, disabled persons, women in some domains, LGBTQ+ persons in some jurisdictions). The dignity reading protects them from speech that functions as structural subordination—speech that denies their standing as equals in the political community. Exit is identity-locked because group membership is not chosen and cannot be abandoned without self-negation. The benefit is inclusion in the speech polity as equal participants rather than targets of group-directed subordination narratives.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__dignity_reading, structurally_subordinated_groups, beneficiary,
    powerless, generational, identity_locked, national).

% Individuals or organizations whose speech the dignity reading would restrict: those producing hate speech, group libel, or advocacy framed as denying the equal humanity or political standing of target groups. They bear the cost of restriction—certain utterances become legally prohibited, subject to enforced silence or sanction. Their exit is constrained but not identity-locked: they can revise their speech, avoid the target group, or relocate to jurisdictions with different speech rules. The constraint renders their specific speech acts illegitimate, not their participation in speech generally.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__dignity_reading, speakers_targeting_group_subordination, payer,
    moderate, biographical, constrained, national).

% Courts and administrative bodies charged with enforcing the distinction between protected speech and speech functioning as group subordination. They set the boundaries: deciding which utterances fall within the dignity-protecting restriction and which fall outside. Their agenda-setting power is formal and rule-bound but involves interpretive judgment about what constitutes structural subordination and when speech performs that function. Exit is mobile at the institutional level—a jurisdiction can abandon the dignity reading for another interpretation.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__dignity_reading, judges_and_adjudicators, agenda_setter,
    institutional, generational, mobile, national).

% Civil liberties organizations, academic institutions, and individuals who argue the dignity reading introduces enforcement costs and adjudicatory uncertainty that burden counter-speech as a remedy. They are beneficiaries in the sense that they gain the coordination benefit of a stable speech framework; they are ambivalently positioned because they dispute whether dignity-conditional protection is the right framework. Their observation seat is that of an institutional voice advocating for alternative readings.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__dignity_reading, counter_speech_advocates, beneficiary,
    organized, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(speech_protection_kernel__dignity_reading, counter_speech_advocates, observer).

% Institutional and ideological actors committed to near-categorical speech protection, who argue that listener harm—including group harm—is not grounds for speech restriction. They are excluded because the dignity reading forecloses their core premise at the framework level; they would argue for fundamentally different boundary-setting but are not typically seated in the adjudication process once the dignity framework is adopted. Their exclusion is not by accident but by the reading's structural nature: the dignity reading's authority rests on accepting group harm as a legitimate grounds for restriction, which the absolutist reading rejects categorically.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__dignity_reading, absolutist_reading_advocates, excluded,
    organized, generational, trapped, national).

% Communities whose speech is itself sometimes restricted or chilled under dignity-reading enforcement—e.g., minority communities reclaiming slurs, discussing their own subordination, or using rhetoric perceived as hostile to dominant groups. They are beneficiaries because the reading protects them from dominant-group subordination speech. They are payers because enforcement machinery calibrated to identify group-subordinating speech sometimes fails to distinguish between speech functioning as subordination (from dominant to subordinate groups) and speech reclaiming or describing subordination (from subordinate groups about their condition). The reading's asymmetry means enforcement burden often falls more heavily on those with less institutional power to clarify intent.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__dignity_reading, marginalized_speech_communities, beneficiary,
    powerless, biographical, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(speech_protection_kernel__dignity_reading, marginalized_speech_communities, payer).

% Legal scholars, constitutional theorists, and international human rights bodies who study and compare speech frameworks across jurisdictions. They observe the dignity reading as one coherent interpretation among competing frameworks, each with distinct coherence and cost structures. Their analytical seat is that of cross-framework comparison, able to evaluate the reading's internal consistency and its structural consequences relative to sibling readings.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__dignity_reading, speech_doctrine_interpretive_community, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(speech_protection_kernel__dignity_reading, diffuse).
narrative_ontology:fixing_cost_class(speech_protection_kernel__dignity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes the conditions under which speech participation is genuinely equal—by treating speech that functions as structural group subordination as illegitimate, the reading attempts to ensure that all participants in the speech community enter with equal standing and dignity, rather than some entering as targets of group-subordinating narratives. The coordination problem it solves: how to maintain a speech polity where subordinated groups are included as equals rather than marked as subordinate.
% TRANSFER_FUNCTION: Moves the authority to define certain utterances as illegitimate from individual speakers to adjudicators (judges, regulatory bodies); moves the burden of identifying structural subordination from subordinated groups (who must endure and counter it) to institutional enforcers. In speech-doctrine terms, it transfers authority for boundary-setting from a speaker-protective principle to an equality-protective principle.
% ABSENT_VOICES: Speakers committed to absolutist speech protection frameworks are structurally excluded by the reading's core premise—the reading forecloses their foundational claim that listener harm is not a legitimate grounds for restriction. International jurisdictions operating under marketplace or democratic-participation readings are not seated in the dignity framework's authority structure. Subordinated communities whose speech is chilled by imprecise enforcement are present but often without seat at the adjudication table that decides what counts as subordinating.
% DISAPPEARANCE_RATIONALE: If the dignity-reading protection vanished overnight, speech boundaries would shift immediately: subordinating group speech would become fully protected again, adjudicatory burden would reverse (subordinated groups would need to counter rather than law preventing utterance), and the speech polity's equality structure would reorganize around a different framework (likely absolutist or harm-threshold). The absent constraint would rearrange the entire configuration of who participates as equals and which utterances are available in public discourse.
% FOUNDING_PROBLEM: Historically, speech has functioned as a mechanism of structural group subordination—racial slurs, antisemitic tropes, gendered degradation, religious demonization—used to mark certain groups as outside the political community, justifying legal and social hierarchy. The dignitary reading emerged as a response to the recognition that protecting speech absolutely while groups experienced systematic speech-mediated subordination created a paradox: those speech protections stabilized rather than challenged group-based hierarchy.
% FOUNDING_PROBLEM_CORROBORATION: Subordinated group advocates, legal scholars studying hate speech (Delgado, Stefancic; Matsuda; Iyengar), and international human rights bodies (UN Special Rapporteur on racism; European Court of Human Rights jurisprudence) attest that speech-mediated group subordination is live and consequential. Absolutist reading advocates counter that the founding problem is overstated and that restricting speech creates worse harms (chilling, state power abuse). The factual claim—that speech functions as group subordination—is corroborated by comparative legal history (jurisdictions that adopted dignity readings, like Canada and Europe, document enforcement patterns); disagreement is over whether the corroborated fact justifies restriction.
narrative_ontology:disappearance_verdict(speech_protection_kernel__dignity_reading, world_rearranges).
narrative_ontology:founding_problem_status(speech_protection_kernel__dignity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(speech_protection_kernel__dignity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(speech_protection_kernel__dignity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(speech_protection_kernel__dignity_reading, 0.62, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(speech_protection_kernel__dignity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(speech_protection_kernel__dignity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(speech_protection_kernel__dignity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises from 0.48 to a plateau at 0.62 over the interval because enforcement machinery becomes established and predictable: early uncertainty about what counts as subordinating speech (low extractiveness, high interpretive burden) gives way to settled case law that clearly identifies prohibited utterances. Suppression is higher throughout (0.71 at end) because the constraint's persistence depends on active judicial/administrative suppression of certain speech categories—without that enforcement, the boundary would erode. Theater is moderate (0.28 at plateau) because while some enforcement activity genuinely identifies subordinating speech and prevents group harm, some share of effort goes to maintaining the speech boundary itself (defending the framework against absolutist challenge, clarifying what counts as subordination) rather than directly preventing harm. The measurement series traces stabilization: extractiveness and suppression both rise steeply through t=24, then flatten, indicating the constraint has matured into an established interpretive framework. Resistance is high (0.79) because the reading meets substantial absolutist, libertarian, and international-law opposition—the framework is continuously contested. Accessibility_collapse is moderate (0.48) because while the dignity reading does collapse alternatives (speakers cannot easily relocate or reframe within the framework), the binary logic of the reading itself creates visible collapse—speakers know exactly what is prohibited—rather than obscuring the constraint's operation.
 *
 * PERSPECTIVAL GAP:
 *   From the subordinated-groups seat, the constraint is protective: it stops speech functioning as exclusion and restores equal standing. From the restricted-speaker seat, the constraint is suppressive: it prohibits their utterances on grounds they reject (that the speech causes group harm rather than merely expressing ideas). From the absolutist seat (excluded from this framework), the constraint is a snare: it enforces equality by violating the foundational speech principle. From the judge's seat, it is a complex coordination problem requiring ongoing boundary-work—neither pure protection nor pure suppression, but a framework requiring continuous interpretation. The engine computes per-seat types from these structural asymmetries; they do not resolve at a single classification level.
 *
 * DIRECTIONALITY LOGIC:
 *   Structurally_subordinated_groups have directionality near 0.0 (beneficiary end): they derive protection from the constraint, their exit is identity-locked (they cannot leave the group), and the constraint's operation lowers extraction FROM them (it prevents group-subordinating speech). Their d is low because they are net beneficiaries. Speakers_targeting_group_subordination have directionality near 0.8 (target end): the constraint extracts from them (restricts their speech), their exit is constrained (they can change speech but not easily leave the jurisdiction or reframe), and they are subject to active enforcement. The asymmetry is structural: the same constraint operates at opposite ends of the directionality spectrum for the two primary seats because one group benefits from group-protecting coordination while the other group bears its cost. Judges have d near 0.5 because they are neither beneficiary nor payer—they administer the framework—though their administrative burden makes them partly target (they must do adjudicatory work).
 *
 * MANDATROPHY ANALYSIS:
 *   The dignity reading is NOT in mandate obsolescence because the founding problem it addresses—speech functioning as group subordination—remains live in most jurisdictions that have adopted this reading. The mandate-checking question: 'Does the constraint still solve the problem it was built for?' Answer: yes, it continues to prevent group-subordinating speech from functioning as exclusion. However, a secondary mandatrophy risk exists at the margins: if a jurisdiction's enforcement becomes largely theatrical (theater_ratio rises substantially above 0.3 while extractiveness plateaus), it would signal that the framework persists by institutional inertia rather than active subordination prevention. The measurement plateau at t=32+ suggests the constraint has stabilized rather than deteriorated, so mandatrophy is not presently active—but the risk class is real. The tangled_rope classification captures this stability: genuine coordination (equal participation) achieved through active enforcement that produces asymmetric costs (restricted speakers).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    structural_subordination_definition_ambiguity,
    'What counts as speech functioning as structural subordination versus speech expressing disagreement or criticism that happens to target groups?',
    'Comparative case-law analysis across jurisdictions adopting dignity readings; empirical study of speech enforcement patterns to identify boundary-setting practices; development of doctrine distinguishing group-subordinating utterances (denying equal standing) from group-critical utterances (criticizing group behavior or beliefs).',
    'If the boundary is drawn narrowly (only egregious hate speech), extractiveness and suppression remain moderate and the reading is robust. If drawn broadly (any speech that could be perceived as group-demeaning), extractiveness and suppression rise substantially, theater_ratio rises (enforcement becomes harder to distinguish from censorship), and resistance increases—the constraint approaches snare classification from the restricted-speaker seat.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(structural_subordination_definition_ambiguity, empirical, 'The definition of structural subordination is the hinge on which the reading''s classification turns; different enforcement baselines yield different extraction profiles.').

omega_variable(
    internalized_suppression_mechanism,
    'Is the measured suppression (0.71) structural (external enforcement machinery that would be removed if unenforced) or internalized (speakers have adopted the dignity principle and now self-censor out of principle rather than fear)?',
    'Post-removal trajectory test: if enforcement were temporarily suspended, would suppression persist at current levels (indicating internalization) or decay (indicating structural enforcement dependence)? Comparative analysis of jurisdictions that abandoned dignity readings (if any exist) to observe suppression trajectories.',
    'If suppression is primarily structural, the constraint''s persistence depends on continuous adjudicatory work—removal of enforcement would restore prohibited speech immediately. If internalized, speakers carry the suppression with them even after enforcement is removed—a deeper binding. The classification impact is modest (remains tangled_rope either way) but affects the strategic calculus: internalized suppression means the boundary-work has become self-sustaining, while structural suppression means constant institutional effort is required.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(internalized_suppression_mechanism, empirical, 'Whether suppression is enforcement-dependent or internalized affects the durability of the constraint.').

omega_variable(
    marginalized_speech_chilling_asymmetry,
    'Does the dignity reading''s enforcement machinery differentially chill speech from marginalized communities discussing their own subordination, relative to dominant-group speech about marginalized groups?',
    'Empirical study of enforcement patterns: measure the rate of restriction/investigation of speech by dominant groups targeting marginalized groups versus speech by marginalized groups discussing their own condition; qualitative interviews with marginalized speakers about self-censorship.',
    'If enforcement is asymmetric (more protective of dominant-group speech, more restrictive of marginalized-group speech about their condition), the constraint extracts from the intended beneficiaries as well as the payer seat—a sign of inverted benefit structure or enforcement failure. This would raise extractiveness from the marginalized-communities seat, introduce a secondary-payer category, and potentially shift the classification toward snare. If enforcement is symmetric, the constraint is cleanly tangled-rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(marginalized_speech_chilling_asymmetry, empirical, 'Enforcement asymmetry could mean the dignity reading extracts from beneficiaries as well as payers, inverting the benefit structure.').

omega_variable(
    foreclosure_strength_of_absolutist_incompatibility,
    'Is the relationship between the dignity reading and the absolutist reading truly a FORECLOSURE (mutually logically incompatible in any single framework), or is it a strong INFLUENCES (the dignity reading creates pressure that the absolutist reading must accommodate, but they could theoretically coexist in a framework that allows reading switching)?',
    'Formal logical analysis of the two readings'' axiom sets: can a framework accept both ''listener harm is not grounds for restriction'' (absolutist) and ''group subordination harm is grounds for restriction'' (dignity) without internal contradiction? Examination of hybrid jurisdictions that claim to operate under both readings.',
    'If true foreclosure, the excluded_voices characterization is accurate—absolutist advocates cannot be legitimately seated in the dignity framework because their premise is logically ruled out. If merely influences, absolutist advocates could be partially seated, creating a different governance structure. The classification impact is minimal (reading_relations are documentary rather than classification-determining) but affects the framework''s theoretical coherence claim.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(foreclosure_strength_of_absolutist_incompatibility, conceptual, 'Whether the absolutist reading is foreclosed or merely pressured by the dignity reading affects how we understand the kernel''s internal logic.').

omega_variable(
    equal_dignity_as_precondition_versus_outcome,
    'Does the dignity reading assume equal dignity as a precondition for speech equality (all speakers already possess it; the reading protects against speech denying that equal standing) or treat equal dignity as an outcome that speech restriction creates (speakers do not yet possess it; the reading builds it)?',
    'Analysis of the reading''s authority grounding and reference frame (Section Rule 5): what does the reading''s foundational premise treat as t0? Examination of enabling-conditions doctrine: does the reading position dignity protection as prerequisite or project.',
    'Precondition framing: dignity is a human constant that speech may attack but never alter; protection prevents denying what exists. Outcome framing: dignity is constructed through speech practice; protection builds it by excluding subordinating speech. The framing affects how subordinated-groups extraction is calculated (are they being protected or built into equality?) and the classification stability: outcome framing risks drift toward snare if enforcement begins to seem like construction without genuine underlying equality.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(equal_dignity_as_precondition_versus_outcome, conceptual, 'The foundational-premise framing of equal dignity affects the reading''s philosophical coherence and extractiveness interpretation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(speech_protection_kernel__dignity_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(spee_tr_t0, speech_protection_kernel__dignity_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement_basis(spee_tr_t0, observed).
narrative_ontology:measurement(spee_tr_t8, speech_protection_kernel__dignity_reading, theater_ratio, 8, 0.16).
narrative_ontology:measurement_basis(spee_tr_t8, observed).
narrative_ontology:measurement(spee_tr_t16, speech_protection_kernel__dignity_reading, theater_ratio, 16, 0.2).
narrative_ontology:measurement_basis(spee_tr_t16, observed).
narrative_ontology:measurement(spee_tr_t24, speech_protection_kernel__dignity_reading, theater_ratio, 24, 0.25).
narrative_ontology:measurement_basis(spee_tr_t24, observed).
narrative_ontology:measurement(spee_tr_t32, speech_protection_kernel__dignity_reading, theater_ratio, 32, 0.28).
narrative_ontology:measurement_basis(spee_tr_t32, observed).
narrative_ontology:measurement(spee_tr_t40, speech_protection_kernel__dignity_reading, theater_ratio, 40, 0.28).
narrative_ontology:measurement_basis(spee_tr_t40, observed).
narrative_ontology:measurement(spee_tr_t50, speech_protection_kernel__dignity_reading, theater_ratio, 50, 0.28).
narrative_ontology:measurement_basis(spee_tr_t50, observed).

% Extraction over time
narrative_ontology:measurement(spee_be_t0, speech_protection_kernel__dignity_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement_basis(spee_be_t0, observed).
narrative_ontology:measurement(spee_be_t8, speech_protection_kernel__dignity_reading, base_extractiveness, 8, 0.52).
narrative_ontology:measurement_basis(spee_be_t8, observed).
narrative_ontology:measurement(spee_be_t16, speech_protection_kernel__dignity_reading, base_extractiveness, 16, 0.57).
narrative_ontology:measurement_basis(spee_be_t16, observed).
narrative_ontology:measurement(spee_be_t24, speech_protection_kernel__dignity_reading, base_extractiveness, 24, 0.6).
narrative_ontology:measurement_basis(spee_be_t24, observed).
narrative_ontology:measurement(spee_be_t32, speech_protection_kernel__dignity_reading, base_extractiveness, 32, 0.62).
narrative_ontology:measurement_basis(spee_be_t32, observed).
narrative_ontology:measurement(spee_be_t40, speech_protection_kernel__dignity_reading, base_extractiveness, 40, 0.62).
narrative_ontology:measurement_basis(spee_be_t40, observed).
narrative_ontology:measurement(spee_be_t50, speech_protection_kernel__dignity_reading, base_extractiveness, 50, 0.62).
narrative_ontology:measurement_basis(spee_be_t50, observed).

% Suppression requirement over time
narrative_ontology:measurement(spee_su_t0, speech_protection_kernel__dignity_reading, suppression_requirement, 0, 0.58).
narrative_ontology:measurement_basis(spee_su_t0, observed).
narrative_ontology:measurement(spee_su_t8, speech_protection_kernel__dignity_reading, suppression_requirement, 8, 0.63).
narrative_ontology:measurement_basis(spee_su_t8, observed).
narrative_ontology:measurement(spee_su_t16, speech_protection_kernel__dignity_reading, suppression_requirement, 16, 0.67).
narrative_ontology:measurement_basis(spee_su_t16, observed).
narrative_ontology:measurement(spee_su_t24, speech_protection_kernel__dignity_reading, suppression_requirement, 24, 0.69).
narrative_ontology:measurement_basis(spee_su_t24, observed).
narrative_ontology:measurement(spee_su_t32, speech_protection_kernel__dignity_reading, suppression_requirement, 32, 0.71).
narrative_ontology:measurement_basis(spee_su_t32, observed).
narrative_ontology:measurement(spee_su_t40, speech_protection_kernel__dignity_reading, suppression_requirement, 40, 0.71).
narrative_ontology:measurement_basis(spee_su_t40, observed).
narrative_ontology:measurement(spee_su_t50, speech_protection_kernel__dignity_reading, suppression_requirement, 50, 0.71).
narrative_ontology:measurement_basis(spee_su_t50, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(speech_protection_kernel__dignity_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(speech_protection_kernel__dignity_reading, 0.12).
narrative_ontology:affects_constraint(speech_protection_kernel__dignity_reading, speech_protection_kernel__absolutist_reading).
narrative_ontology:affects_constraint(speech_protection_kernel__dignity_reading, speech_protection_kernel__harm_threshold_reading).
narrative_ontology:affects_constraint(speech_protection_kernel__dignity_reading, speech_protection_kernel__marketplace_reading).
narrative_ontology:affects_constraint(speech_protection_kernel__dignity_reading, speech_protection_kernel__democratic_participation_reading).

% DUAL FORMULATION NOTE:
% The speech_protection_kernel decomposes into five structurally distinct readings, each with different ε values and stakeholder asymmetries. The dignity_reading (this constraint) recognizes group harm as distinct from individual harm and restricts speech functioning as group subordination—extractive from restricted speakers, protective for subordinated groups. The absolutist_reading forecloses this framework by treating all listener harm as insufficient grounds for restriction. The harm_threshold_reading coexists by recognizing harm-based restriction but not necessarily group-harm specifically. The marketplace and democratic_participation readings represent alternative coordination frames that could operate alongside dignity protection. Each reading is a separate constraint with separate metrics, stakeholders, and beneficiary/victim structures; they are not perspectives on a single constraint but different constraints grounded in the same contested kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(speech_protection_kernel__dignity_reading, powerless, 0.15).
constraint_indexing:directionality_override(speech_protection_kernel__dignity_reading, moderate, 0.78).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
