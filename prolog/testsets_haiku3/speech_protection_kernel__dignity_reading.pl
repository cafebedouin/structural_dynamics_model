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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   human_readable: Speech Protection Conditional on Dignity Maintenance (Dignity Reading)
 *   domain: constitutional_law/political_philosophy
 *
 * SUMMARY:
 *   The dignity reading of speech protection treats speech as constrained by
 *   an equal-dignity condition: speech that systematically functions to
 *   subordinate groups qua groups is not protected, because protection would
 *   contradict the very equality that makes freedom possible. This reading
 *   recognizes group harm as structurally distinct from individual harm —
 *   hate speech targeting group membership establishes subordination, not
 *   merely offends individuals. The constraint is claimed as tangled_rope
 *   because it coordinates equal participation (a genuine collective good)
 *   while extracting from speakers with subordinating intent and majoritarian
 *   movements whose speech is restricted. The authored metrics
 *   (extractiveness 0.68, suppression 0.72) reflect that the constraint
 *   operates through active enforcement of line-drawing (what counts as group
 *   subordination) and imposes real constraints on speech deemed to violate
 *   the dignity standard.
 *
 * KEY AGENTS:
 *   - Historically subordinated groups: beneficiaries of the dignity protection; depend on state enforcement to prevent speech-based subordination
 *   - Speakers with subordinating intent: targeted by the constraint; bear legal and social cost of restriction
 *   - Majoritarian hate organizations: collective actors whose mobilization capacity is constrained by the dignity standard
 *   - Liberal courts: agenda-setters; interpret 'subordinating function' and enforce the constraint
 *   - Majoritarian speakers (non-subordinating): beneficiaries who retain full protection because their speech does not target groups for subordination
 *   - Absolutist advocates: excluded from the operative framework; would reject the dignity condition entirely
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(speech_protection_kernel__dignity_reading, 0.68).
domain_priors:suppression_score(speech_protection_kernel__dignity_reading, 0.72).
domain_priors:theater_ratio(speech_protection_kernel__dignity_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(speech_protection_kernel__dignity_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(speech_protection_kernel__dignity_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(speech_protection_kernel__dignity_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(speech_protection_kernel__dignity_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(speech_protection_kernel__dignity_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(speech_protection_kernel__dignity_reading, tangled_rope).
narrative_ontology:human_readable(speech_protection_kernel__dignity_reading, "Speech Protection Conditional on Dignity Maintenance (Dignity Reading)").
narrative_ontology:topic_domain(speech_protection_kernel__dignity_reading, "constitutional_law/political_philosophy").

domain_priors:requires_active_enforcement(speech_protection_kernel__dignity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(speech_protection_kernel__dignity_reading, '4cf44edc-ae9d-497e-8ac6-f99452107dcb').
narrative_ontology:cs_kernel_codification('4cf44edc-ae9d-497e-8ac6-f99452107dcb', fixed_text).
narrative_ontology:cs_authority_grounding('4cf44edc-ae9d-497e-8ac6-f99452107dcb', lineage).
narrative_ontology:cs_interpretation_layer_present('4cf44edc-ae9d-497e-8ac6-f99452107dcb').
narrative_ontology:cs_reading_relation('4cf44edc-ae9d-497e-8ac6-f99452107dcb', speech_protection_kernel__absolutist_reading, forecloses).
narrative_ontology:cs_reading_relation('4cf44edc-ae9d-497e-8ac6-f99452107dcb', speech_protection_kernel__democratic_participation_reading, coexists_with).
narrative_ontology:cs_reading_relation('4cf44edc-ae9d-497e-8ac6-f99452107dcb', speech_protection_kernel__harm_threshold_reading, influences).
narrative_ontology:cs_reading_relation('4cf44edc-ae9d-497e-8ac6-f99452107dcb', speech_protection_kernel__marketplace_reading, influences).
narrative_ontology:cs_axiom('4cf44edc-ae9d-497e-8ac6-f99452107dcb', foundational, group_subordination_unprotected).
narrative_ontology:cs_axiom_status(group_subordination_unprotected, holdable).
narrative_ontology:cs_axiom_grounding('4cf44edc-ae9d-497e-8ac6-f99452107dcb', group_subordination_unprotected, deontological).
narrative_ontology:cs_axiom('4cf44edc-ae9d-497e-8ac6-f99452107dcb', foundational, equal_dignity_precondition_for_speech_freedom).
narrative_ontology:cs_axiom_status(equal_dignity_precondition_for_speech_freedom, holdable).
narrative_ontology:cs_axiom_grounding('4cf44edc-ae9d-497e-8ac6-f99452107dcb', equal_dignity_precondition_for_speech_freedom, deontological).
narrative_ontology:cs_reference_frame('4cf44edc-ae9d-497e-8ac6-f99452107dcb', speech_protection_with_equal_dignity_condition).
narrative_ontology:cs_drift_state('4cf44edc-ae9d-497e-8ac6-f99452107dcb', contemporary_polarized_discourse_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('4cf44edc-ae9d-497e-8ac6-f99452107dcb', '2026-06-12T14:32:00Z').
narrative_ontology:cs_kernel_id(speech_protection_kernel__dignity_reading, speech_protection_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(speech_protection_kernel__dignity_reading, historically_subordinated_groups).
narrative_ontology:constraint_beneficiary(speech_protection_kernel__dignity_reading, targets_of_structural_hate_speech).
narrative_ontology:constraint_victim(speech_protection_kernel__dignity_reading, speakers_with_subordinating_intent).
narrative_ontology:constraint_victim(speech_protection_kernel__dignity_reading, majoritarian_hate_movement_organizations).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(speech_protection_kernel__dignity_reading, majoritarian_speakers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Receive protection from speech that functions as group subordination. Their capacity to participate as equals in discourse depends on state enforcement excluding speech whose systematic effect is to demean or silence them as a class. Exit is not viable — they cannot leave the jurisdiction or abandon public participation to avoid subordination.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__dignity_reading, historically_subordinated_groups, beneficiary,
    moderate, generational, trapped, national).

% Individuals within subordinated groups harmed by speech targeting them qua group members. The constraint recognizes group libel as a distinct harm — speech whose function is to establish or reinforce their structural subordination is unprotected. They benefit from state refusal to treat such speech as protected.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__dignity_reading, targets_of_structural_hate_speech, beneficiary,
    moderate, biographical, constrained, national).

% Bear legal liability for speech found to function as group subordination. They argue speech protection should not depend on the audience's group status or detection of subordinating intent — that this invites subjective censorship. Exit options: self-censorship, relocation to absolutist jurisdictions, or litigation. They cannot abandon speech expression entirely.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__dignity_reading, speakers_with_subordinating_intent, payer,
    moderate, biographical, constrained, national).

% Face organizational restrictions on speech mobilization when their message functions as group subordination. The constraint prevents them from using state-protected platforms for recruitment targeting groups for exclusion or violence. Exit options: migrate to online spaces outside state jurisdiction, operate clandestinely, or litigate. They cannot operate openly under the constraint.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__dignity_reading, majoritarian_hate_movement_organizations, payer,
    organized, generational, constrained, national).

% Speech advancing majoritarian views that does not function as group subordination remains fully protected. Their participation is not chilled; the constraint targets subordinating function, not viewpoint. They can adjust expression and retain protection — exit is easy.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__dignity_reading, majoritarian_speakers, beneficiary,
    powerful, biographical, arbitrage, national).

% Interpret and enforce the dignity constraint. Determine when speech functions as group subordination, issue injunctions/damages, balance against free-speech interests. They shoulder enforcement burden and legitimacy cost of line-drawing. Their interpretation shapes what subordinating function means in practice — intent, systemic effect, or historical group position.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__dignity_reading, liberal_courts, agenda_setter,
    institutional, generational, analytical, national).

% Would argue speech protection should be near-categorical, that listener harm (including group harm) is not grounds for restriction. Their position is excluded from the operative constitutional framework but present in litigation and legislative debate. They have constrained exit: they can litigate and advocate but cannot operate under the framework they reject.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__dignity_reading, absolutist_speech_advocates, excluded,
    powerful, biographical, constrained, national).

% From outside the U.S. constitutional framework, provide analysis of the dignity reading's effects in other jurisdictions (Canada, EU, Israel) that explicitly recognize group-harm restrictions. Their observations feed into U.S. jurisprudence but do not determine enforcement. They document whether dignity-based restrictions achieve greater equality in practice or become tools for suppression.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__dignity_reading, comparative_constitutionalists, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(speech_protection_kernel__dignity_reading, liberal_courts).
narrative_ontology:fixing_cost_class(speech_protection_kernel__dignity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates equal participation in public discourse by excluding speech that functions to subordinate groups, which would prevent equal voice. Solves the problem: how to maintain speech freedom while preventing speech from weaponizing to establish hierarchy.
% TRANSFER_FUNCTION: Transfers authority over group-targeted speech from unmediated speaker discretion to courts and enforcement bodies; moves capacity to make group-subordinating claims from speakers to institutional adjudicators who apply the dignity standard. Subordinated groups gain protection and voice security; speakers with subordinating intent lose capacity for certain group-targeted expression.
% ABSENT_VOICES: Speakers found to violate the dignity standard are functionally absent from the constraint's legitimacy proceedings — they are restricted rather than heard in debates about whether the dignity condition is justified. Absolutist speech advocates and libertarian philosophers opposing the constraint are excluded from the operative constitutional framework, though present in meta-level litigation and legislative debate. Subjects of hate speech who lack resources to litigate or legal standing may be functionally absent despite nominal inclusion as beneficiaries.
% DISAPPEARANCE_RATIONALE: If the dignity constraint disappeared overnight, speech targeting groups for subordination would become legally protected and socially unregulated by anti-discrimination law. Hate-movement organizations would openly mobilize around group-subordination messaging without legal risk. Public discourse would shift to permit more explicit group dehumanization and subordination claims. Historically subordinated groups would experience documented increases in hate speech, recruitment targeting, and threats. The equilibrium of who can speak what about whom would rearrange along group-identity lines.
% FOUNDING_PROBLEM: The founding problem, dating to mid-20th-century equality doctrine: traditional speech-protection doctrine treats speech as atomistic utterance, but group-targeted speech operates as a systematic structure of subordination. Speech can be weaponized not merely to offend but to establish and reinforce group hierarchy. Equal participation requires that speech not function as subordination — otherwise, unprotected speech protection contradicts the freedom itself.
% FOUNDING_PROBLEM_CORROBORATION: Equality theorists (Owen Fiss, Catharine MacKinnon, critical race theory scholars) and international human-rights bodies (UN committees, European Court of Human Rights) document empirically that group-targeted hate speech has structured effects on group members' participation and equal status. However, absolutist speech advocates and First Amendment scholars (Eugene Volokh, Keith Whittington, Nadine Strossen) contest the framing: they argue that pre-existing group subordination, not speech, is the problem; that regulating speech on group-subordination grounds historically harms marginalized groups; and that the dignity reading invites exactly the censorship subordination doctrine should prevent. The founding problem is genuinely contested between these camps with no empirical or juridical resolution in sight.
narrative_ontology:disappearance_verdict(speech_protection_kernel__dignity_reading, world_rearranges).
narrative_ontology:founding_problem_status(speech_protection_kernel__dignity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(speech_protection_kernel__dignity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(speech_protection_kernel__dignity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(speech_protection_kernel__dignity_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness rises over the interval (0.48 to 0.68) as the dignity reading's jurisprudence matures and courts apply the subordination test more frequently, catching more speech in its scope. Suppression is high (0.55 to 0.72) because the constraint requires continuous institutional effort: courts must determine when speech functions as group subordination, speakers must internalize constraints on group-targeted claims, and enforcement bodies must police boundaries. Theater rises moderately (0.28 to 0.41) because a growing share of enforcement activity involves doctrinal performance — elaborate judicial reasoning about the group/individual distinction, subordination vs. offense, and the dignity condition's limits — rather than straightforward speech suppression. The measurement grid captures a steady-state regime where the constraint has settled into mature enforcement; no precipitous shifts suggest the dignity reading is institutionally stable though persistently contested in jurisprudence. Accessibility_collapse (0.62) reflects that once the dignity standard is established, alternatives (returning to absolutist speech protection, exiting the jurisdiction) are mostly unavailable to speakers or courts bound by precedent. Resistance (0.58) is moderate-high because absolutist advocates and hate-movement organizations mount persistent legal and political challenge to the dignity reading's legitimacy.
 *
 * PERSPECTIVAL GAP:
 *   The dignity reading's internal tension: equal dignity as a coordination goal (all groups can participate as equals) requires restricting speech that denies equal dignity (group subordination). This creates an apparent paradox — free speech is restricted in the name of freedom — that generates constant litigation over the boundary. From the beneficiary perspective (subordinated groups), the tension is resolved: subordinating speech is not speech worthy of protection because it attacks their capacity for speech itself. From the payer perspective (speakers constrained), the tension is unresolved and remains the core grievance: the constraint's legitimacy depends on accepting that speech can be restricted on the basis of its group-targeted effects, which itself seems to violate speech freedom. Courts navigate this by arguing that the dignity condition is not content restriction but a precondition for the freedom that speech protection serves — a meta-level reframing that abstractive advocates reject as incoherent.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality derives from beneficiary/victim declarations and exit structure. Historically subordinated groups are beneficiaries with trapped exit (cannot exit the jurisdiction or discourse); this yields d near 0.0 (full beneficiary). Speakers with subordinating intent are victims with constrained exit (can self-censor or litigate but operate under real restriction); this yields d near 0.8 (near-target end). Majoritarian speakers are beneficiaries with arbitrage exit (can adjust expression and retain protection); this yields d near 0.1 (strong beneficiary). Majoritarian hate organizations are victims with constrained exit (organized actors facing restriction but with litigation/political channels); this yields d near 0.7 (target-side but with some organizational power). Courts (agenda-setter, institutional, analytical exit) have directionality distinct from the parties they regulate: they are neither beneficiary nor victim in the structural sense but the seat that administers the constraint and derives legitimacy from its operation.
 *
 * MANDATROPHY ANALYSIS:
 *   The dignity reading avoids false mandatrophy by grounding the constraint in a live equal-dignity condition — the founding problem (speech as group subordination) remains contested and generationally relevant; courts continue to apply the dignity test because the coordination problem (equal participation against speech-based hierarchy) remains operative. However, a second-order mandatrophy risk exists: if the dignity reading's application becomes routinized and the boundary between group subordination and legitimate group-targeted speech becomes formulaic, the enforcement machinery could become performative (theater_ratio rising above 0.5), suggesting the coordination function has atrophied while institutional enforcement persists from inertia. The current measurement trajectory (theater_ratio 0.28 → 0.41) shows rising theatricality but not yet dominant performance; the constraint remains substantially functional. Mandatrophy would be indexed by theater_ratio exceeding 0.65 and suppression_requirement rising faster than base_extractiveness, indicating the court system is expending energy on boundary-maintenance rather than actual subordination-prevention.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    group_harm_vs_individual_harm_boundary,
    'What constitutes ''group harm'' distinct from aggregated individual harm? Is group subordination a sui generis phenomenon, or is it reducible to individual members'' experiences?',
    'Empirical analysis of speech effects on group-member participation, identity, and material outcomes; comparison of jurisdiction policies (where group-harm recognition is explicit vs. absent) and their documented effects on marginalized groups'' public participation.',
    'If group harm is distinct and empirically measurable, the dignity reading''s boundary is tractable. If group harm is incoherent or inseparable from individual harm, the subordination test becomes indefensible and the constraint should reclassify toward snare (pure restriction masquerading as coordination). The ε value depends on whether the group-harm distinction can be sustained.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(group_harm_vs_individual_harm_boundary, conceptual, 'Whether group subordination is structurally distinct from individual harm.').

omega_variable(
    subordinating_function_ascertainment,
    'By what evidence is ''subordinating function'' determined? Does it require speaker intent, or only systematic effect? Who decides?',
    'Analysis of actual adjudications: how courts determine subordination in practice; documentation of false positives (speech restricted as subordinating when not intended/functioning that way) and false negatives (subordinating speech protected); comparison with other jurisdictions'' frameworks (intent-based, effect-based, categorical lists).',
    'If subordinating function can be reliably determined with low false-positive rates, the constraint is a functional tangled_rope with manageable line-drawing costs. If false-positive rates are high, the constraint drifts toward snare — speech gets restricted based on ambiguous evidence of group effect, creating chilling effects that suppress speech beyond the subordination category. The theater_ratio would rise sharply and suppression_requirement would decouple from actual subordination-prevention.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(subordinating_function_ascertainment, empirical, 'Reliability and defensibility of subordination determination.').

omega_variable(
    majoritarian_speech_asymmetry,
    'Does the dignity reading''s protection of non-subordinating majoritarian speech genuinely hold in practice, or is enforcement skewed to restrict minority-group speech even when not subordinating?',
    'Systematic audit of enforcement data: what proportion of restrictions apply to majority-group speech vs. minority-group speech; analysis of patterns in adjudication and informal enforcement; documentation of cases where majority-group speech is protected despite some group-targeted content.',
    'If enforcement is even-handed, the constraint operates as claimed — majoritarian speakers retain protection; asymmetry is purely in payers'' vulnerability to determination of subordinating function. If enforcement is skewed toward restricting minority-group speech, the constraint is a masked tool for suppressing marginal groups, reclassifying from tangled_rope toward snare with an equality-violation omega. Suppression would rise and directionality would shift: majoritarian-speakers would move toward payer status despite nominal beneficiary role.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(majoritarian_speech_asymmetry, empirical, 'Whether the dignity reading''s protection operates symmetrically across majority and minority speech.').

omega_variable(
    sibling_reading_foreclosure,
    'Does the dignity reading logically foreclose the absolutist reading within a single constitutional framework, or do they genuinely coexist as live alternatives held by competing factions?',
    'Jurisprudential analysis: can the dignity condition be coherently integrated into a constitutional system that also protects absolutist speech freedoms in some domains? Do jurisdictions that adopt the dignity reading simultaneously hold absolutist-reading commitments for other speech categories (e.g., political speech, religious speech)? Or is the tension genuinely irresolvable, requiring a choice?',
    'If the readings coexist, they should be classified in reading_relations as coexists_with — different parties hold them and neither cancels the other. If they genuinely foreclose each other (dignity requires group-subordination restrictions that absolutism forbids), then forecloses should be the relation, and the constraint''s legitimacy depends on winning the constitutional choice between them. The classification affects the network structure and the engine''s handling of sibling constraint coupling.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_reading_foreclosure, conceptual, 'Logical relationship between dignity and absolutist readings.').

omega_variable(
    equal_dignity_precondition_assumption,
    'Is equal dignity a necessary precondition for meaningful speech freedom, or is this a reading-specific claim that other readings can coherently reject?',
    'Philosophical analysis: can absolutist speech protection be grounded in a different account of speech''s value (truth-discovery, self-expression, checking power) that does not require equal dignity? Do majoritarian regimes that restrict subordinating speech achieve greater equality in practice, or does the dignity condition become a tool for suppressing dissent? Comparative historical analysis of regimes adopting dignity-based restrictions vs. absolutist regimes.',
    'If equal dignity is contingently necessary (true for liberal-egalitarian frameworks but not universal), the axiom status should be holdable — the dignity reading can maintain it as foundational. If it is genuinely universal (all coherent speech regimes presuppose it), the axiom becomes foundational to the kernel itself, not specific to the dignity reading. If it is overridden in practice (regimes restrict speech for group subordination but do not achieve greater equality), the axiom should be marked overridden and the constraint should reclassify toward snare.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(equal_dignity_precondition_assumption, conceptual, 'Whether equal dignity is a necessary presupposition of speech freedom or a reading-specific normative choice.').

omega_variable(
    reading_kernel_relationship_ambiguity,
    'Is the dignity reading an interpretation of an existing speech-protection commitment, or a transformation that redefined the kernel itself?',
    'Historical analysis of when the dignity condition entered constitutional doctrine; examination of whether early speech-protection texts (bills of rights, founding constitutional documents) contemplated group-subordination restrictions or whether the dignity reading added a new condition. Analysis of whether the reading''s proponents claim to be interpreting the kernel or rewriting it.',
    'If the dignity reading interprets an existing kernel, cs_structure.kernel_codification should specify the founding text and authority_grounding should reference lineage (interpreting from a founding text). If the dignity reading transformed the kernel (the condition was added later, not present in the text), kernel_codification should be distributed (multiple readings reconstructing what the kernel now means) and authority_grounding should reference practice or expertise (the dignity reading grounds itself in constitutional doctrine''s evolution, not in textual authority). This affects the constraint''s legitimacy story and the reading_relations'' forecast.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_kernel_relationship_ambiguity, conceptual, 'Whether the dignity reading interprets or transforms the kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(speech_protection_kernel__dignity_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(spee_tr_t0, speech_protection_kernel__dignity_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement(spee_tr_t5, speech_protection_kernel__dignity_reading, theater_ratio, 5, 0.32).
narrative_ontology:measurement(spee_tr_t10, speech_protection_kernel__dignity_reading, theater_ratio, 10, 0.35).
narrative_ontology:measurement(spee_tr_t15, speech_protection_kernel__dignity_reading, theater_ratio, 15, 0.38).
narrative_ontology:measurement(spee_tr_t25, speech_protection_kernel__dignity_reading, theater_ratio, 25, 0.4).
narrative_ontology:measurement(spee_tr_t35, speech_protection_kernel__dignity_reading, theater_ratio, 35, 0.41).
narrative_ontology:measurement(spee_tr_t40, speech_protection_kernel__dignity_reading, theater_ratio, 40, 0.41).

% Extraction over time
narrative_ontology:measurement(spee_be_t0, speech_protection_kernel__dignity_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(spee_be_t5, speech_protection_kernel__dignity_reading, base_extractiveness, 5, 0.52).
narrative_ontology:measurement(spee_be_t10, speech_protection_kernel__dignity_reading, base_extractiveness, 10, 0.58).
narrative_ontology:measurement(spee_be_t15, speech_protection_kernel__dignity_reading, base_extractiveness, 15, 0.63).
narrative_ontology:measurement(spee_be_t25, speech_protection_kernel__dignity_reading, base_extractiveness, 25, 0.67).
narrative_ontology:measurement(spee_be_t35, speech_protection_kernel__dignity_reading, base_extractiveness, 35, 0.68).
narrative_ontology:measurement(spee_be_t40, speech_protection_kernel__dignity_reading, base_extractiveness, 40, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(spee_su_t0, speech_protection_kernel__dignity_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(spee_su_t5, speech_protection_kernel__dignity_reading, suppression_requirement, 5, 0.6).
narrative_ontology:measurement(spee_su_t10, speech_protection_kernel__dignity_reading, suppression_requirement, 10, 0.65).
narrative_ontology:measurement(spee_su_t15, speech_protection_kernel__dignity_reading, suppression_requirement, 15, 0.68).
narrative_ontology:measurement(spee_su_t25, speech_protection_kernel__dignity_reading, suppression_requirement, 25, 0.71).
narrative_ontology:measurement(spee_su_t35, speech_protection_kernel__dignity_reading, suppression_requirement, 35, 0.72).
narrative_ontology:measurement(spee_su_t40, speech_protection_kernel__dignity_reading, suppression_requirement, 40, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(speech_protection_kernel__dignity_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(speech_protection_kernel__dignity_reading, 0.18).
narrative_ontology:affects_constraint(speech_protection_kernel__dignity_reading, speech_protection_kernel__absolutist_reading).
narrative_ontology:affects_constraint(speech_protection_kernel__dignity_reading, speech_protection_kernel__harm_threshold_reading).
narrative_ontology:affects_constraint(speech_protection_kernel__dignity_reading, speech_protection_kernel__marketplace_reading).
narrative_ontology:affects_constraint(speech_protection_kernel__dignity_reading, speech_protection_kernel__democratic_participation_reading).

% DUAL FORMULATION NOTE:
% The dignity reading is one of five structurally distinct readings of the speech_protection_kernel. Each reading instantiates a different constraint with its own ε, beneficiary/victim structure, and classification. The dignity reading recognizes group harm and conditions protection on equal-dignity maintenance; sibling readings reject this condition or ground speech protection in alternative values (truth-discovery, political necessity, harm-threshold). All five stories are linked via network.affects_constraints; each is a separate constraint story with its own .pl compilation. The family is not a single contested claim with five perspectives — it is five distinct constraints grounded in the same kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(speech_protection_kernel__dignity_reading, institutional, 0.5).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
