% ============================================================================
% CONSTRAINT STORY: biblical_source_text__critical_reconstructive_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   human_readable: Critical Reconstructive Reading of Biblical Source Text
 *   domain: religious/academic/textual
 *
 * SUMMARY:
 *   The critical reconstructive reading treats the biblical text as a
 *   historical artifact whose earliest recoverable form must be established
 *   before any structural or theological reading can claim legitimacy. This
 *   methodological priority — text before structure, history before meaning —
 *   coordinates a global scholarly enterprise (editions, commentaries,
 *   translations, curricula) but simultaneously destabilizes the received
 *   texts that confessional communities and religious authorities treat as
 *   normatively binding. The constraint is a tangled rope: it performs
 *   genuine coordination (without it, scholarly communication fragments)
 *   while extracting interpretive authority from communities whose identity
 *   is constituted by the received text. The extractiveness is asymmetric:
 *   near-zero for academic readers who inhabit the method, high for
 *   confessional readers for whom the method is an external imposition.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(biblical_source_text__critical_reconstructive_reading, 0.55).
domain_priors:suppression_score(biblical_source_text__critical_reconstructive_reading, 0.45).
domain_priors:theater_ratio(biblical_source_text__critical_reconstructive_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(biblical_source_text__critical_reconstructive_reading, extractiveness, 0.55).
narrative_ontology:constraint_metric(biblical_source_text__critical_reconstructive_reading, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(biblical_source_text__critical_reconstructive_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(biblical_source_text__critical_reconstructive_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(biblical_source_text__critical_reconstructive_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(biblical_source_text__critical_reconstructive_reading, tangled_rope).
narrative_ontology:human_readable(biblical_source_text__critical_reconstructive_reading, "Critical Reconstructive Reading of Biblical Source Text").
narrative_ontology:topic_domain(biblical_source_text__critical_reconstructive_reading, "religious/academic/textual").

domain_priors:requires_active_enforcement(biblical_source_text__critical_reconstructive_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(biblical_source_text__critical_reconstructive_reading, '68e47167-7ea4-4d3f-b078-e6c20a5672aa').
narrative_ontology:cs_kernel_codification('68e47167-7ea4-4d3f-b078-e6c20a5672aa', distributed).
narrative_ontology:cs_authority_grounding('68e47167-7ea4-4d3f-b078-e6c20a5672aa', expertise).
narrative_ontology:cs_interpretation_layer_present('68e47167-7ea4-4d3f-b078-e6c20a5672aa').
narrative_ontology:cs_reading_relation('68e47167-7ea4-4d3f-b078-e6c20a5672aa', biblical_source_text__formal_equivalence_reading, coexists_with).
narrative_ontology:cs_reading_relation('68e47167-7ea4-4d3f-b078-e6c20a5672aa', biblical_source_text__dynamic_equivalence_reading, influences).
narrative_ontology:cs_axiom('68e47167-7ea4-4d3f-b078-e6c20a5672aa', foundational, historical_reconstruction_primacy).
narrative_ontology:cs_axiom_status(historical_reconstruction_primacy, holdable).
narrative_ontology:cs_axiom_grounding('68e47167-7ea4-4d3f-b078-e6c20a5672aa', historical_reconstruction_primacy, empirically_contingent).
narrative_ontology:cs_axiom('68e47167-7ea4-4d3f-b078-e6c20a5672aa', foundational, text_before_structure_or_meaning).
narrative_ontology:cs_axiom_status(text_before_structure_or_meaning, holdable).
narrative_ontology:cs_axiom_grounding('68e47167-7ea4-4d3f-b078-e6c20a5672aa', text_before_structure_or_meaning, empirically_contingent).
narrative_ontology:cs_reference_frame('68e47167-7ea4-4d3f-b078-e6c20a5672aa', critical_text_as_historical_prior).
narrative_ontology:cs_drift_state('68e47167-7ea4-4d3f-b078-e6c20a5672aa', contemporary_manuscript_culture, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('68e47167-7ea4-4d3f-b078-e6c20a5672aa', '').
narrative_ontology:cs_kernel_id(biblical_source_text__critical_reconstructive_reading, biblical_source_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(biblical_source_text__critical_reconstructive_reading, academic_biblical_scholarship).
narrative_ontology:constraint_victim(biblical_source_text__critical_reconstructive_reading, confessional_communities).
narrative_ontology:constraint_victim(biblical_source_text__critical_reconstructive_reading, religious_authorities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(biblical_source_text__critical_reconstructive_reading, academic_biblical_scholars).
narrative_ontology:constraint_beneficiary(biblical_source_text__critical_reconstructive_reading, translation_committees).
narrative_ontology:constraint_victim(biblical_source_text__critical_reconstructive_reading, translation_committees).
narrative_ontology:constraint_victim(biblical_source_text__critical_reconstructive_reading, lay_believers).
narrative_ontology:constraint_vindicates(biblical_source_text__critical_reconstructive_reading, historical_critical_method).
narrative_ontology:constraint_vindicates(biblical_source_text__critical_reconstructive_reading, textual_reconstruction_priority).
narrative_ontology:constraint_vindicates(biblical_source_text__critical_reconstructive_reading, manuscript_evidence_primacy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Define and enforce the methodological standards of textual criticism: which manuscripts count, how variants are weighed, what reconstruction procedures are legitimate. Their careers, funding, and institutional positions depend on the critical apparatus being the authoritative gateway to the text. They can move between universities, journals, and editorial boards; their exit is arbitrage-grade because the skill set transfers across the global guild.
narrative_ontology:constraint_stakeholder(biblical_source_text__critical_reconstructive_reading, academic_biblical_scholars, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(biblical_source_text__critical_reconstructive_reading, academic_biblical_scholars, beneficiary).

% Receive the critical text as an external imposition that destabilizes the received text their liturgy, doctrine, and communal identity are built on. They bear the cost of either rejecting scholarship (appearing obscurantist) or accommodating it (revising catechesis, liturgy, and self-understanding). Exit from the constraint means exit from the identity the received text constitutes; the text is not a document they consult but a world they inhabit.
narrative_ontology:constraint_stakeholder(biblical_source_text__critical_reconstructive_reading, confessional_communities, payer,
    organized, generational, identity_locked, global).

% Hold teaching and doctrinal authority that presupposes a stable textual basis. The critical reconstruction moves the ground beneath magisterial declarations, conciliar definitions, and confessional standards. They must either police the boundary (declaring critical methods out of bounds) or undertake costly hermeneutical renovation. Their institutional role fuses with the text they guard; they cannot step outside it without ceasing to be the authority they are.
narrative_ontology:constraint_stakeholder(biblical_source_text__critical_reconstructive_reading, religious_authorities, payer,
    institutional, generational, identity_locked, global).

% Must choose a source text for translation. The critical edition is the de facto standard, but adopting it invites confessional backlash; rejecting it sacrifices scholarly credibility. They benefit from the critical apparatus's philological rigor but pay in contested reception. Their exit is constrained by publisher expectations, denominational oversight, and the practical necessity of a usable base text.
narrative_ontology:constraint_stakeholder(biblical_source_text__critical_reconstructive_reading, translation_committees, payer,
    organized, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(biblical_source_text__critical_reconstructive_reading, translation_committees, beneficiary).

% Encounter the constraint indirectly through footnotes, study Bibles, sermons, and catechetical shifts. When the critical text displaces a familiar reading, they experience it as loss or betrayal without having participated in the methodological debate. Their exit options are limited to changing communities or suppressing the dissonance; the text's authority is woven into their piety.
narrative_ontology:constraint_stakeholder(biblical_source_text__critical_reconstructive_reading, lay_believers, payer,
    moderate, biographical, identity_locked, local).

% Study the textual tradition as a historical phenomenon without confessional commitment. They evaluate the critical method on its philological merits and its role in the modern constitution of the text as an object of knowledge. They neither pay the confessional cost nor collect the academic rent; they observe the constraint's operation from outside the belief-structure it destabilizes.
narrative_ontology:constraint_stakeholder(biblical_source_text__critical_reconstructive_reading, secular_textual_critics, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a shared methodological framework for reconstructing the earliest recoverable text from divergent manuscript witnesses, enabling cumulative scholarly progress, interoperable editions, and a common evidentiary baseline across languages and institutions.
% TRANSFER_FUNCTION: Moves interpretive authority from living confessional traditions (which hold received texts as normatively binding) to the academic guild (which holds the reconstructed text as historically prior). Moves the epistemic burden onto confessional communities to justify why a later textual form should outweigh earlier manuscript evidence.
% ABSENT_VOICES: Traditional scribal communities and oral tradition bearers for whom the text was never a fixed object but a performed reality; non-Western interpretive traditions (Syriac, Ethiopic, Armenian, Georgian) that preserve alternative textual streams marginalized by the Western critical apparatus; pre-critical commentators whose readings are excluded by the methodological gate.
% DISAPPEARANCE_RATIONALE: If the critical reconstructive constraint vanished, academic biblical studies would lose its methodological coherence and shared evidentiary baseline — editions would fragment into confessional or sectarian lines. Confessional communities would lose the primary external challenge that has forced doctrinal development for two centuries, but would also lose the philological tools that have refined their own textual understanding.
% FOUNDING_PROBLEM: The manuscript tradition presents thousands of divergent witnesses; no single received text can claim autograph status. A historically grounded method was needed to recover the earliest achievable text and to replace theological fiat with evidence-based reconstruction.
% FOUNDING_PROBLEM_CORROBORATION: Manuscript discoveries (papyri, majuscules, versions) continue to accumulate, requiring ongoing methodological refinement. Textual critics outside confessional commitments (e.g., Ehrman, Parker, Wasserman) attest the problem remains live. Confessional scholars (e.g., Metzger, Fee, current NA/UBS editors) corroborate that the manuscript evidence still demands reconstruction, even as they negotiate its doctrinal reception.
narrative_ontology:disappearance_verdict(biblical_source_text__critical_reconstructive_reading, world_rearranges).
narrative_ontology:founding_problem_status(biblical_source_text__critical_reconstructive_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(biblical_source_text__critical_reconstructive_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(biblical_source_text__critical_reconstructive_reading, 'none', 1).
narrative_ontology:epsilon_provenance(biblical_source_text__critical_reconstructive_reading, 0.55, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Base extractiveness (0.55) reflects the asymmetric transfer: the academic guild gains a stable, cumulative research program; confessional communities bear the cost of perpetual hermeneutical renovation. Suppression (0.45) is moderate — the constraint operates through editorial gatekeeping, peer review, tenure structures, and publisher expectations rather than overt coercion, but the cost of non-participation is professional marginalization. Theater ratio (0.25) is low: the philological work is real, the manuscript evidence is real, the methodological debates are substantive. Accessibility collapse (0.55) is moderate: alternative textual bases (Majority Text, Textus Receptus, confessional editions) persist but are excluded from the academic mainstream. Resistance (0.70) is high: confessional pushback takes forms from parallel textual traditions to doctrinal declarations of textual preservation.
 *
 * PERSPECTIVAL GAP:
 *   From the academic seat, the constraint is a rope — a coordination solution to the manuscript chaos problem. From the confessional seat, it is a snare — an extractive imposition that treats their constitutive text as a hypothesis. The engine computes this divergence from the structural data; the claimed_type (tangled_rope) names the structural reality that both seats experience partial truths about.
 *
 * DIRECTIONALITY LOGIC:
 *   Academic scholars are structural beneficiaries (d ~ 0.15): they set the agenda, collect the professional rewards, and hold arbitrage-grade exit. Confessional communities and religious authorities are structural targets (d ~ 0.85): they bear the hermeneutical cost, their exit is identity-locked, and the constraint's persistence depends on their inability to opt out without identity loss. Translation committees sit near symmetric (d ~ 0.5): they gain philological rigor but pay in contested reception. Lay believers are targets with identity-locked exit but minimal power to shape the constraint. Secular critics are analytical observers (d = 0.5 by definition).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (manuscript divergence requiring historical reconstruction) remains live — new witnesses keep appearing, methods keep refining. The constraint has not outlived its function. However, the extraction asymmetry has grown: early critical scholarship (Westcott-Hort) still operated within a broadly Christian cultural frame; contemporary critical scholarship operates in a secular academy where the confessional cost is externalized. The mandatrophy risk is not obsolescence but capture: the coordination function may come to serve the guild's self-reproduction more than the textual problem.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    committer_structure_kernel_reading,
    'This constraint is one reading of the contested kernel ''biblical_source_text'' (reading_id: critical_reconstructive_reading). Sibling readings: formal_equivalence_reading, dynamic_equivalence_reading. What structural elements do the readings disagree on?',
    'Map each reading''s beneficiary/victim structure, claimed_type, and axioms. The disagreement is located in: (1) what counts as the primary object of fidelity (reconstructed text vs. source structure vs. target communication), (2) who bears the cost of the reading''s priority (confessional communities vs. target audiences vs. scholarly guild), (3) whether the kernel admits a single stable referent or is irreducibly plural.',
    'If the kernel is irreducibly plural (no single text satisfies all three readings), then each reading instantiates a different constraint with different ε, different victims, different type. The committer frame does not average them — it generates three constraint stories linked by network.affects_constraints.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(committer_structure_kernel_reading, conceptual, 'Committer structure: this story is one reading of a contested kernel; sibling readings instantiate different constraints.').

omega_variable(
    extraction_asymmetry_necessity,
    'Is the high extractiveness on confessional communities structurally necessary to the critical method''s coordination function, or is it a contingent effect of academic power?',
    'Counterfactual: if confessional communities voluntarily adopted the critical text as their received text (as some have), would the method''s coordination value diminish? Historical test: communities that adopted critical editions (mainline Protestantism) vs. those that rejected them (KJV-only, traditional Catholicism, Orthodoxy) — compare scholarly uptake and textual stability.',
    'If necessary, the tangled_rope classification is robust — coordination requires extraction. If contingent, a rope classification might be achievable with different institutional arrangements (e.g., confessional editions using critical apparatus).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_asymmetry_necessity, empirical, 'Whether the coordination-extraction coupling is structural or institutional.').

omega_variable(
    textual_stability_threshold,
    'At what point does manuscript convergence make the critical reconstruction stable enough that confessional communities could adopt it without perpetual revision?',
    'Track the rate of change in NA/UBS editions (27th/28th → 29th → future) and the corresponding confessional reception. Measure the correlation between textual stability and confessional adoption.',
    'If convergence is asymptotic but never final, the extraction is permanent — the constraint is a permanent tangled rope. If convergence reaches a practical plateau, the constraint could transition toward rope as the coordination function outlives the extraction.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(textual_stability_threshold, empirical, 'Whether the extraction dynamic is temporally bounded by textual convergence.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(biblical_source_text__critical_reconstructive_reading, 1900, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(biblical_source_text__critical_reconstructive_reading_tr_t1900, biblical_source_text__critical_reconstructive_reading, theater_ratio, 1900, 0.15).
narrative_ontology:measurement(biblical_source_text__critical_reconstructive_reading_tr_t1935, biblical_source_text__critical_reconstructive_reading, theater_ratio, 1935, 0.18).
narrative_ontology:measurement(biblical_source_text__critical_reconstructive_reading_tr_t1970, biblical_source_text__critical_reconstructive_reading, theater_ratio, 1970, 0.22).
narrative_ontology:measurement(biblical_source_text__critical_reconstructive_reading_tr_t2000, biblical_source_text__critical_reconstructive_reading, theater_ratio, 2000, 0.24).
narrative_ontology:measurement(biblical_source_text__critical_reconstructive_reading_tr_t2025, biblical_source_text__critical_reconstructive_reading, theater_ratio, 2025, 0.25).

% Extraction over time
narrative_ontology:measurement(biblical_source_text__critical_reconstructive_reading_be_t1900, biblical_source_text__critical_reconstructive_reading, base_extractiveness, 1900, 0.35).
narrative_ontology:measurement(biblical_source_text__critical_reconstructive_reading_be_t1935, biblical_source_text__critical_reconstructive_reading, base_extractiveness, 1935, 0.42).
narrative_ontology:measurement(biblical_source_text__critical_reconstructive_reading_be_t1970, biblical_source_text__critical_reconstructive_reading, base_extractiveness, 1970, 0.5).
narrative_ontology:measurement(biblical_source_text__critical_reconstructive_reading_be_t2000, biblical_source_text__critical_reconstructive_reading, base_extractiveness, 2000, 0.53).
narrative_ontology:measurement(biblical_source_text__critical_reconstructive_reading_be_t2025, biblical_source_text__critical_reconstructive_reading, base_extractiveness, 2025, 0.55).

% Suppression requirement over time
narrative_ontology:measurement(biblical_source_text__critical_reconstructive_reading_su_t1900, biblical_source_text__critical_reconstructive_reading, suppression_requirement, 1900, 0.3).
narrative_ontology:measurement(biblical_source_text__critical_reconstructive_reading_su_t1935, biblical_source_text__critical_reconstructive_reading, suppression_requirement, 1935, 0.38).
narrative_ontology:measurement(biblical_source_text__critical_reconstructive_reading_su_t1970, biblical_source_text__critical_reconstructive_reading, suppression_requirement, 1970, 0.42).
narrative_ontology:measurement(biblical_source_text__critical_reconstructive_reading_su_t2000, biblical_source_text__critical_reconstructive_reading, suppression_requirement, 2000, 0.44).
narrative_ontology:measurement(biblical_source_text__critical_reconstructive_reading_su_t2025, biblical_source_text__critical_reconstructive_reading, suppression_requirement, 2025, 0.45).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(biblical_source_text__critical_reconstructive_reading, information_standard).
narrative_ontology:boltzmann_floor_override(biblical_source_text__critical_reconstructive_reading, 0.02).
narrative_ontology:affects_constraint(biblical_source_text__critical_reconstructive_reading, translation_practice__formal_equivalence).
narrative_ontology:affects_constraint(biblical_source_text__critical_reconstructive_reading, translation_practice__dynamic_equivalence).
narrative_ontology:affects_constraint(biblical_source_text__critical_reconstructive_reading, confessional_authority_structure).
narrative_ontology:affects_constraint(biblical_source_text__critical_reconstructive_reading, biblical_source_text__formal_equivalence_reading).
narrative_ontology:affects_constraint(biblical_source_text__critical_reconstructive_reading, biblical_source_text__dynamic_equivalence_reading).

% DUAL FORMULATION NOTE:
% This constraint is the critical_reconstructive_reading of the biblical_source_text kernel. It provides the source text that both formal_equivalence_reading and dynamic_equivalence_reading take as their translation base. The formal equivalence reading coordinates structural fidelity to this reconstructed text; the dynamic equivalence reading coordinates communicative effectiveness from this reconstructed text. The critical reading's extraction on confessional communities is amplified downstream: both translation readings inherit the destabilized text basis.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(biblical_source_text__critical_reconstructive_reading, organized, 0.85).
constraint_indexing:directionality_override(biblical_source_text__critical_reconstructive_reading, institutional, 0.8).
constraint_indexing:directionality_override(biblical_source_text__critical_reconstructive_reading, moderate, 0.75).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
