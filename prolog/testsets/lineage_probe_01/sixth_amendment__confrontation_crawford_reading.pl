% ============================================================================
% CONSTRAINT STORY: sixth_amendment__confrontation_crawford_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sixth_amendment__confrontation_crawford_reading, []).

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
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: sixth_amendment__confrontation_crawford_reading
 *   human_readable: Sixth Amendment Confrontation Clause: Crawford Reading (Testimonial Statements & Cross-Examination)
 *   domain: constitutional_law/criminal_procedure
 *
 * SUMMARY:
 *   Crawford v. Washington (2004) restored the confrontation clause to its
 *   text: 'the Accused shall enjoy the right... to be confronted with the
 *   witnesses against him.' The reading asserts that testimonial statements —
 *   statements made with the primary purpose of accusing someone of a crime —
 *   require the witness to appear live for cross-examination; hearsay of this
 *   form is barred by the Constitution itself, not merely by the rules of
 *   evidence. This constraint exhibits the characteristic structure of a
 *   constitutional reading: it claims to restore textual authority while
 *   producing doctrinal effects that redistribute power between prosecution
 *   and defense. The constraint's extractiveness (0.38) reflects genuine
 *   suppression of a prosecution method (hearsay shortcuts) without
 *   eliminating prosecution entirely — prosecutors adapt by locating
 *   witnesses or establishing non-testimonial hearsay exceptions. The
 *   suppression (0.62) is high because the constraint eliminates proven
 *   conviction methods with no compensation; the accused's only benefit is
 *   the procedural right itself, not material advantage. The theater ratio
 *   (0.45) reflects that Crawford's legitimacy rests on textual-originalist
 *   framing ('restored to the text'), but the operative rule depends heavily
 *   on judge-made doctrine about what counts as 'testimonial' — the
 *   distinction is intuitive but not self-executing. Over 16 years
 *   post-Crawford, the theater has increased as exceptions and refinements
 *   accumulated (Davis, Melendez-Diaz, Bullcoming, Williams), each case
 *   requiring new adjudication of the testimonial boundary.
 *
 * KEY AGENTS:
 *   - The Cross-Examining Accused: Primary beneficiary (powerless/trapped at biographical scale, but the clause guarantees the procedural right). Experiences Crawford as coordination — the guarantee of live confrontation rather than extraction.
 *   - The Prosecution: Primary victim (institutional/constrained). Bears the suppression cost — cannot use absent-witness testimonial hearsay; must locate witnesses or establish non-testimonial exceptions.
 *   - The Court System: Institutional actor (institutional/constrained, biographical timescale). Experiences tangled rope: benefits from the clear rule ('testimonial' statements require confrontation) but bears enforcement costs (managing complex admissibility questions, excluding probative evidence).
 *   - The Defense Bar: Institutional beneficiary (institutional/arbitrage, generational scale). Extracts value from Crawford through litigation leverage and procedural advantage, while genuinely coordinating fair trial mechanisms.
 *   - Text-Originalist Authority: Institutional framework (institutional/arbitrage, civilizational scale). Maintains the rule through commitment to textual authenticity; theater increases as interpretive work (defining 'testimonial') becomes visible.
 *   - The Analytical Observer: Civilizational perspective (analytical/analytical). Risks naturalizing Crawford as a bedrock principle ('witnesses by definition testify live') when it represents a contingent institutional choice about how to test reliability.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sixth_amendment__confrontation_crawford_reading, 0.38).
domain_priors:suppression_score(sixth_amendment__confrontation_crawford_reading, 0.62).
domain_priors:theater_ratio(sixth_amendment__confrontation_crawford_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sixth_amendment__confrontation_crawford_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(sixth_amendment__confrontation_crawford_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(sixth_amendment__confrontation_crawford_reading, theater_ratio, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sixth_amendment__confrontation_crawford_reading, tangled_rope).
narrative_ontology:human_readable(sixth_amendment__confrontation_crawford_reading, "Sixth Amendment Confrontation Clause: Crawford Reading (Testimonial Statements & Cross-Examination)").
narrative_ontology:topic_domain(sixth_amendment__confrontation_crawford_reading, "constitutional_law/criminal_procedure").

domain_priors:requires_active_enforcement(sixth_amendment__confrontation_crawford_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(sixth_amendment__confrontation_crawford_reading, '3b7d2dc3-40f6-4fee-bdc2-71dc50f1a6dd').
narrative_ontology:cs_kernel_codification('3b7d2dc3-40f6-4fee-bdc2-71dc50f1a6dd', fixed_text).
narrative_ontology:cs_authority_grounding('3b7d2dc3-40f6-4fee-bdc2-71dc50f1a6dd', lineage).
narrative_ontology:cs_interpretation_layer_present('3b7d2dc3-40f6-4fee-bdc2-71dc50f1a6dd').
narrative_ontology:cs_reading_relation('3b7d2dc3-40f6-4fee-bdc2-71dc50f1a6dd', sixth_amendment__gideon_counsel_revolution, influences).
narrative_ontology:cs_reading_relation('3b7d2dc3-40f6-4fee-bdc2-71dc50f1a6dd', sixth_amendment__jury_cross_section_reading, coexists_with).
narrative_ontology:cs_axiom('3b7d2dc3-40f6-4fee-bdc2-71dc50f1a6dd', foundational, testimonial_statements_require_live_confrontation).
narrative_ontology:cs_axiom_status(testimonial_statements_require_live_confrontation, holdable).
narrative_ontology:cs_axiom_grounding('3b7d2dc3-40f6-4fee-bdc2-71dc50f1a6dd', testimonial_statements_require_live_confrontation, deontological).
narrative_ontology:cs_axiom('3b7d2dc3-40f6-4fee-bdc2-71dc50f1a6dd', foundational, cross_examination_is_only_constitutional_reliability_test).
narrative_ontology:cs_axiom_status(cross_examination_is_only_constitutional_reliability_test, holdable).
narrative_ontology:cs_axiom_grounding('3b7d2dc3-40f6-4fee-bdc2-71dc50f1a6dd', cross_examination_is_only_constitutional_reliability_test, deontological).
narrative_ontology:cs_reference_frame('3b7d2dc3-40f6-4fee-bdc2-71dc50f1a6dd', text_restored_confrontation).
narrative_ontology:cs_drift_state('3b7d2dc3-40f6-4fee-bdc2-71dc50f1a6dd', post_crawford_case_law_accumulation, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('3b7d2dc3-40f6-4fee-bdc2-71dc50f1a6dd', '').
narrative_ontology:cs_kernel_id(sixth_amendment__confrontation_crawford_reading, sixth_amendment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sixth_amendment__confrontation_crawford_reading, cross_examining_accused).
narrative_ontology:constraint_victim(sixth_amendment__confrontation_crawford_reading, prosecution_absent_witnesses).
narrative_ontology:constraint_victim(sixth_amendment__confrontation_crawford_reading, state_conviction_efficiency).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE ACCUSED WITHOUT CONFRONTATION (ROPE) — When denied cross-examination of testimonial hearsay, the powerless defendant experiences pure coordination failure: the clause exists to guarantee a coordination mechanism (live cross-examination), and deprivation of it is deprivation of the mechanism itself. From the accused's structural position, the right is not extractive — it is the baseline coordination the clause provides. No extraction flows toward the accused; the constraint coordinates a procedural guarantee.
constraint_indexing:constraint_classification(sixth_amendment__confrontation_crawford_reading, rope,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE PROSECUTION USING ABSENT-WITNESS HEARSAY (SNARE) — From the prosecution's position, Crawford's restoration of confrontation acts as a suppression mechanism — a hard barrier to a proven conviction method (hearsay shortcuts, absent-witness statements). The prosecution is the primary target of the suppression. However, this perspective is constrained, not trapped: prosecutors can build cases through live testimony or properly-foundation hearsay. The effective suppression is high (0.62) because the constraint eliminates a significant investigative shortcut, but the prosecution retains alternatives at higher cost.
constraint_indexing:constraint_classification(sixth_amendment__confrontation_crawford_reading, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: THE COURT SYSTEM (TANGLED ROPE) — The institutional beneficiary of Crawford is also constrained by it. Courts benefit from having a clear, administrable rule (testimonial statements require confrontation) rather than fluid reliability judgments. This is genuine coordination: Crawford provides a workable standard. But courts also bear costs — excluding probative evidence, managing complex admissibility questions about what counts as 'testimonial.' The court system both benefits from the coordination function and bears enforcement costs. Beneficiary (clarity, administrability) and victim (excluded evidence, complexity) are the same institutional actor.
constraint_indexing:constraint_classification(sixth_amendment__confrontation_crawford_reading, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: THE DEFENSE BAR (TANGLED ROPE, GENERATIONAL) — Over generational timescales, the defense bar experiences Crawford as both coordination and subtle extraction. The rule coordinates: it provides a clear standard ('testimonial' statements require the witness). But the defense bar also extracts value from the rule's application — Crawford creates litigation opportunities, requires state investment in witness availability, and narrows prosecution options. From the arbitrage position, the defense bar can leverage Crawford's rules to extract procedural advantages while also genuinely coordinating a fair trial mechanism. Extractiveness flows toward the defense collective, but the coordination function is real.
constraint_indexing:constraint_classification(sixth_amendment__confrontation_crawford_reading, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: TEXT-ORIGINALIST FRAMEWORK (PITON, CIVILIZATIONAL) — At civilizational timescale, Crawford's restoration of confrontation to its textual moorings is a piton — a partially degraded institutional commitment to textual constraint. The originalist framing presents the confrontation right as 'restored to the text,' but the actual operative mechanism depends heavily on judge-made doctrine about what counts as 'testimonial' (still evolving, still contested). The theater is high (0.45): the invocation of textual authenticity masks ongoing interpretive work. The rule persists through institutional commitment to the text-as-authority, not because the textual command is univocal.
constraint_indexing:constraint_classification(sixth_amendment__confrontation_crawford_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational/universal frame, cross-examination as the 'only constitutional test of reliability' can appear as a bedrock principle of Anglo-American law — an immutable structural requirement. The right is framed as inseparable from adversarial trial itself: 'witnesses' by definition testify live; hearsay that bypasses confrontation is inherently unreliable. This perspective sees Crawford as restoring a natural law of evidence, not constructing a rule. However, the structural data (suppression of hearsay shortcuts, beneficiary is the accused, victim is prosecution efficiency) contradicts the mountain classification. The engine will identify this as a false summit — Crawford naturalizes a contingent institutional choice (live confrontation > hearsay shortcuts) as inherent to justice itself.
constraint_indexing:constraint_classification(sixth_amendment__confrontation_crawford_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sixth_amendment__confrontation_crawford_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(sixth_amendment__confrontation_crawford_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(sixth_amendment__confrontation_crawford_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(sixth_amendment__confrontation_crawford_reading, TR),
    TR >= 0.70.

:- end_tests(sixth_amendment__confrontation_crawford_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. Crawford suppresses a proven prosecution method (absent-witness testimonial hearsay), which is real extraction from the prosecution's capacity. But extractiveness is not severe because prosecutors retain alternatives: locate the witness, establish that the statement is non-testimonial (business records, statements to family members, statements made for medical diagnosis), or invoke recognized exceptions (statements against penal interest, forfeiture by wrongdoing). The suppression is principled (applies to all testimonial hearsay) rather than arbitrary, which lowers the experienced extraction. Suppression (0.62): High. The constraint eliminates a shortcut without compensation to the prosecution. Hearsay is efficient — witnesses are expensive to locate and transport; absent statements are probative. Crawford's requirement for confrontation increases prosecution cost substantially. The suppression is architecturally high because the rule is categorical (testimonial statements are barred, full stop) rather than balancing prosecution efficiency against defense rights. Theater ratio (0.45): Moderate. Crawford's legitimacy depends on the claim of textual restoration ('the clause un-balanced back to its text'). But the operative rule depends on judge-made doctrine about what counts as 'testimonial.' The Supreme Court has never provided a univocal definition — Davis offered a 'primary purpose' test that has proven unstable in application (texts, 911 calls, statements to police all require case-by-case adjudication). The theater reflects this gap between textual authenticity (claimed) and interpretive work (required). Theater has increased over the measurement interval as exceptions and refinements accumulated.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates the indexed classification principle: the same constitutional rule produces different types from different structural positions. The accused without confrontation sees coordination (Rope) — the clause guarantees a procedure, not an advantage. The prosecution using hearsay sees suppression (Snare) — confrontation is a hard barrier to a proven method. The court system sees mixed coordination and cost (Tangled Rope) — the rule is administrable but excludes evidence and requires ongoing interpretation. The defense bar sees extraction leverage (Tangled Rope, generational) — the rule creates procedural advantage alongside genuine fairness. The text-originalist framework sees a theatrical restoration (Piton) — the claim of textual authenticity masks interpretive work. The civilizational observer risks seeing immutable law (Mountain) — confrontation as inherent to adversarial trial itself. The perspectival gap reveals that Crawford naturalizes a contingent choice: live testimony > hearsay shortcuts is not a law of nature but a constitutional preference, and that preference distributes power toward the accused and defense bar while suppressing prosecution efficiency.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is derived from the structural position of each observer relative to the suppression mechanism. The accused benefits from the confrontation guarantee (beneficiary status → low d) but is nominally powerless in court (powerless power atom) — this produces a mixed signal: beneficiary with trapped exit yields d ≈ 0.40, moderate f(d). The prosecution bears the suppression cost (victim status) and is nominally constrained (can adapt through witness location or non-testimonial hearsay) → victim status + constrained exit yields d ≈ 0.65, f(d) ≈ 1.0. The court is both beneficiary (clear rule) and victim (enforcement cost), with constrained exit → d ≈ 0.50, f(d) ≈ 0.65. The defense bar is a beneficiary with arbitrage options → d ≈ 0.15, f(d) ≈ -0.05. These derivations explain why the prosecution sees Snare (high f(d)) while the accused sees Rope (f(d) near zero) — the same rule applies, but directionality differs based on structural position.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by showing that Crawford is genuinely a tangled rope (both coordination and extraction), not a pure rope that naturalizes as coordination or a pure snare that naturalizes as suppression. The coordination function is real: Crawford provides a clear, administrable standard (testimonial statements require confrontation) that replaces fluid reliability judgments. Courts benefit from administrability. The extraction function is real: the rule suppresses a prosecution method (hearsay shortcuts) without compensation, and the defense bar extracts procedural leverage. Both functions persist across time. The theater ratio increase reflects not degradation of the rule but increasing visibility of the interpretive work required to apply it — as the testimonial boundary is tested in novel contexts (texts, 911 calls), judges must adjudicate what counts as testimonial, and each case reveals the gap between the textual claim (restored to the text) and the doctrinal reality (ongoing interpretation). The constraint does not collapse into piton; rather, the theater measures the gap between textual legitimacy (claimed) and institutional practice (interpreted).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    testimonial_definition_stability,
    'Is the boundary between ''testimonial'' and ''non-testimonial'' statements stable enough to function as a constitutional rule, or does it collapse under repeated application to novel contexts (texts, 911 calls, statements to police)?',
    'Longitudinal analysis of Crawford aftermath case law (Davis, Melendez-Diaz, Bullcoming, Williams, Giles); mapping of prosecutor adaptations and judge-made exceptions; identification of systematic drift in what counts as testimonial',
    'If stable: Crawford''s rule-of-law content is preserved; confrontation suppression remains principled. If unstable: the constraint degrades from tangled_rope toward piton (theater increases as exceptions proliferate); the confrontation suppression becomes ad-hoc rather than rule-governed.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(testimonial_definition_stability, empirical, 'Stability of the testimonial/non-testimonial boundary under applied doctrine').

omega_variable(
    witness_availability_as_prosecutorial_cost,
    'Does Crawford''s requirement for live confrontation systematically increase prosecution cost in ways that disadvantage resource-limited prosecutors, or do affluent jurisdictions absorb the cost difference negligibly?',
    'Comparative analysis of conviction rates and case processing times across high-resource and low-resource jurisdictions pre- and post-Crawford; measurement of witness-locating expenditures and case dismissal rates due to witness unavailability',
    'If disparate impact: Crawford creates latent extraction (privileging well-resourced prosecutors); suppression flows unequally. If absorbed equally: suppression is evenly distributed; the tangled_rope classification holds across contexts.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(witness_availability_as_prosecutorial_cost, empirical, 'Whether Crawford''s witness requirement creates disparate prosecution burden').

omega_variable(
    constitutional_reading_contest,
    'Is Crawford a restoration of the clause''s original textual meaning, or a reconstruction that reads contemporary evidentiary assumptions back into founding-era language?',
    'Historical-semantic analysis: what ''confront'' and ''witnesses'' meant in 1791 vs. contemporary usage; comparison with founding-era confrontation practice in English courts; identification of what Crawford claimed as restoration vs. what historiography supports',
    'If restoration: Crawford''s legitimacy rests on fidelity to original constitutional text; the foundational axiom (textual_confrontation_requirement) holds its authority. If reconstruction: Crawford''s rule is judge-made modernization using originalist framing; the piton classification deepens (theater increases as textual authenticity is undermined).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(constitutional_reading_contest, conceptual, 'Whether Crawford represents textual restoration or contemporary reconstruction').

omega_variable(
    reading_specificity_versus_gideon,
    'Does Crawford''s focus on confrontation rights presuppose or compete with Gideon''s counsel guarantee? Can a defendant effectively exercise confrontation rights without competent defense counsel?',
    'Case analysis: outcomes in Crawford-controlled cases where defendants lack effective assistance; whether confrontation suppression of hearsay is meaningless without counsel to deploy it; whether the readings coexist or one subsumes the other',
    'If Crawford presupposes Gideon: the readings influence each other structurally (they are operationally coupled). If they operate independently: coexistence relation holds. If Gideon subsumes Crawford: Crawford is a secondary benefit that only materializes given counsel — gideon_counsel_revolution forecloses confrontation_crawford_reading as autonomous.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_specificity_versus_gideon, conceptual, 'Whether Crawford presupposes or competes with Gideon''s counsel guarantee').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sixth_amendment__confrontation_crawford_reading, 0, 16).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(conf_crawford_theater_t0, sixth_amendment__confrontation_crawford_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement(conf_crawford_theater_t8, sixth_amendment__confrontation_crawford_reading, theater_ratio, 8, 0.4).
narrative_ontology:measurement(conf_crawford_theater_t16, sixth_amendment__confrontation_crawford_reading, theater_ratio, 16, 0.45).

% Extraction over time
narrative_ontology:measurement(conf_crawford_extract_t0, sixth_amendment__confrontation_crawford_reading, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(conf_crawford_extract_t8, sixth_amendment__confrontation_crawford_reading, base_extractiveness, 8, 0.35).
narrative_ontology:measurement(conf_crawford_extract_t16, sixth_amendment__confrontation_crawford_reading, base_extractiveness, 16, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sixth_amendment__confrontation_crawford_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(sixth_amendment__confrontation_crawford_reading, gideon_counsel_revolution).
narrative_ontology:affects_constraint(sixth_amendment__confrontation_crawford_reading, jury_cross_section_reading).
narrative_ontology:affects_constraint(sixth_amendment__confrontation_crawford_reading, hearsay_exception_scope).

% DUAL FORMULATION NOTE:
% Crawford's testimonial-statements rule is linked to sibling readings of the Sixth Amendment kernel (Gideon's counsel, jury cross-section) and downstream to constraints about hearsay exception scope. The coordinate constraints operate at different doctrinal scales: confrontation addresses live testimony, counsel addresses trial access, jury addresses trial composition, hearsay exceptions address evidentiary rules. Each constraint has its own extractiveness; they are linked by institutional coupling and shared beneficiary (the accused).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(sixth_amendment__confrontation_crawford_reading, moderate, 0.65).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
