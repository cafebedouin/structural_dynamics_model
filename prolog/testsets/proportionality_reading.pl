% ============================================================================
% CONSTRAINT STORY: proportionality_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_proportionality_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: proportionality_reading
 *   human_readable: Vaccine Mandate Proportionality Constraint (Public Health Reading)
 *   domain: public_health_ethics/constitutional_law
 *
 * SUMMARY:
 *   The proportionality reading frames vaccine mandates as permissible only
 *   when disease severity, transmission risk, and vaccine safety jointly meet
 *   strict thresholds, with robust exemptions for medical contraindications.
 *   This reading is one of three structurally distinct ways to ground mandate
 *   legitimacy: the public_health_primary reading treats disease control as
 *   categorically prior to individual choice; the bodily_autonomy_primary
 *   reading treats individual consent as a hard constraint on state power.
 *   The proportionality reading occupies the middle ground: mandates are
 *   legitimate but context-dependent, requiring ongoing measurement of
 *   disease parameters and continuous justification of threshold choices.
 *   This reading is genuinely constructive — it acknowledges both the
 *   collective interest in disease control and individual interests in
 *   autonomy — but it creates its own structural trap: proportionality logic
 *   requires technocratic measurement authority (who defines 'severity'? how
 *   is 'transmission risk' quantified?) and shifts extraction mechanisms from
 *   the mandate itself to the threshold-setting process.
 *
 * KEY AGENTS:
 *   - Public Health Authority: Primary beneficiary (institutional/arbitrage) — controls threshold definition, retains mandate discretion within proportionality frame
 *   - Vaccine-Hesitant Individual: Primary victim (powerless/trapped) — lacks exit options and recourse when mandate applies regardless of proportionality logic
 *   - Genuine Medical Exemption Claimant: Secondary victim (moderate/constrained) — benefits from exemption in principle but constrained by verification burden and gatekeeping
 *   - Disease-Vulnerable Population: Mixed beneficiary/victim (moderate/mobile) — benefits from mandate but constrained by co-mandates that may restrict access
 *   - Democratic Deliberative Institutions: Organized actor (organized/constrained) — retains capacity to revisit proportionality thresholds but constrained by urgent decision timelines
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — sees both legitimacy and extraction trap in the proportionality frame
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(proportionality_reading, 0.38).
domain_priors:suppression_score(proportionality_reading, 0.48).
domain_priors:theater_ratio(proportionality_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(proportionality_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(proportionality_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(proportionality_reading, theater_ratio, 0.42).

% --- Constraint claim ---
narrative_ontology:constraint_claim(proportionality_reading, tangled_rope).
narrative_ontology:human_readable(proportionality_reading, "Vaccine Mandate Proportionality Constraint (Public Health Reading)").
narrative_ontology:topic_domain(proportionality_reading, "public_health_ethics/constitutional_law").

domain_priors:requires_active_enforcement(proportionality_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(proportionality_reading, 'b6a71ff7-2afc-4b3a-8051-a19d9e9f63c1').
narrative_ontology:cs_created_at('b6a71ff7-2afc-4b3a-8051-a19d9e9f63c1', '').
narrative_ontology:cs_kernel_codification('b6a71ff7-2afc-4b3a-8051-a19d9e9f63c1', formalized).
narrative_ontology:cs_authority_grounding('b6a71ff7-2afc-4b3a-8051-a19d9e9f63c1', lineage).
narrative_ontology:cs_interpretation_layer_present('b6a71ff7-2afc-4b3a-8051-a19d9e9f63c1').
narrative_ontology:cs_kernel_id(proportionality_reading, vaccine_mandate_balance).
narrative_ontology:cs_reading_relation('b6a71ff7-2afc-4b3a-8051-a19d9e9f63c1', public_health_primary, coexists_with).
narrative_ontology:cs_reading_relation('b6a71ff7-2afc-4b3a-8051-a19d9e9f63c1', bodily_autonomy_primary, coexists_with).
narrative_ontology:cs_axiom('b6a71ff7-2afc-4b3a-8051-a19d9e9f63c1', foundational, mandate_legitimacy_context_contingent).
narrative_ontology:cs_axiom_status(mandate_legitimacy_context_contingent, holdable).
narrative_ontology:cs_axiom_grounding('b6a71ff7-2afc-4b3a-8051-a19d9e9f63c1', mandate_legitimacy_context_contingent, deontological).
narrative_ontology:cs_axiom('b6a71ff7-2afc-4b3a-8051-a19d9e9f63c1', secondary, proportionality_thresholds_measurable).
narrative_ontology:cs_axiom_status(proportionality_thresholds_measurable, holdable).
narrative_ontology:cs_axiom_grounding('b6a71ff7-2afc-4b3a-8051-a19d9e9f63c1', proportionality_thresholds_measurable, empirically_contingent).
narrative_ontology:cs_reference_frame('b6a71ff7-2afc-4b3a-8051-a19d9e9f63c1', balanced_individual_collective_interest).
narrative_ontology:cs_drift_state('b6a71ff7-2afc-4b3a-8051-a19d9e9f63c1', contemporary_post_pandemic, gap(axiom_overriding, substantial, true)).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(proportionality_reading, disease_vulnerable_populations).
narrative_ontology:constraint_beneficiary(proportionality_reading, public_health_authority).
narrative_ontology:constraint_victim(proportionality_reading, vaccine_hesitant_individuals).
narrative_ontology:constraint_victim(proportionality_reading, medical_exemption_claimants).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: VACCINE HESITANT INDIVIDUAL (SNARE) — Faces employment loss, school exclusion, travel restrictions with minimal recourse. Exit options structurally unavailable: cannot exit the jurisdiction easily, cannot exit the labor market without severe cost, cannot challenge mandate individually. Experiences maximum extraction — the proportionality frame does not protect individual autonomy against collective thresholds.
constraint_indexing:constraint_classification(proportionality_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: GENUINE MEDICAL EXEMPTION CLAIMANT (TANGLED ROPE) — Benefits from the proportionality frame's logic (exemptions exist in principle), but constrained by high verification burdens, physician gatekeeping, and social stigma. Mandate provides coordination benefit (community protection) alongside asymmetric extraction (burden of proof on individual, career risk of exemption visibility). Mixed coordination-extraction hybrid.
constraint_indexing:constraint_classification(proportionality_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: PUBLIC HEALTH AUTHORITY (ROPE) — Experiences mandate as pure coordination: proportionality thresholds enable rational vaccine deployment without categorical bans. Authority retains discretion over severity/transmission/safety parameters. Arbitrage option: can adjust mandate stringency by redefining thresholds or emphasizing different pathogens. Net beneficiary — mandate provides legitimacy for vaccination campaigns without requiring uniform rules.
constraint_indexing:constraint_classification(proportionality_reading, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: DEMOCRATIC DELIBERATIVE PROCESS (SCAFFOLD) — Organized capacity (legislatures, public comment periods, courts) to revisit proportionality thresholds as conditions change. Sees mandate as temporary coordination tool with sunset logic: when disease severity declines or vaccines improve, thresholds naturally adjust downward or mandate phases out. Theater relatively low — proportionality requires ongoing measurement and justification, not ritual compliance.
constraint_indexing:constraint_classification(proportionality_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: DISEASE-VULNERABLE POPULATION (TANGLED ROPE) — Benefits from vaccination mandate (coordination for protection) but also constrained by mandate's asymmetries (mobility restrictions may prevent vulnerable individuals from accessing care, vaccine access disparities among vulnerable groups not addressed by mandate proportionality logic). Mixed: mandate is both protective and constraining.
constraint_indexing:constraint_classification(proportionality_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: PUBLIC HEALTH AUTHORITY (CONSTRAINED BY PROPORTIONALITY) — From the authority's own perspective under proportionality constraint, experiences extraction: must continuously re-justify mandate, must publish severity/transmission/safety data, must defend exemption standards against legal challenge. High theater: proportionality requires constant recalibration and communication, consuming resources that could go to other public health measures.
constraint_indexing:constraint_classification(proportionality_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: NATURAL LAW / CATEGORICAL VIEW (MOUNTAIN) — From civilizational scope, proportionality thresholds are treated as secondary to a more fundamental principle: during severe public health crises, categorical mandates are natural requirements of collective survival (analogous to mandatory quarantine in plague). The proportionality reading is then viewed as a post-hoc constraint on something that must happen categorically. Engine's false summit detector will flag this: the 'necessity' is context-dependent, not immutable.
constraint_indexing:constraint_classification(proportionality_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(proportionality_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(proportionality_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(proportionality_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

:- end_tests(proportionality_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The proportionality reading reduces extractiveness compared to categorical mandates (which would score ~0.65) by conditioning mandate legitimacy on parameters that could vary by pathogen and time. However, extractiveness remains moderate because (1) the measurement process itself is contestable and authority-controlled, (2) exemption verification burdens are asymmetrical, and (3) threshold changes can shift mandate stringency faster than public deliberation can respond. Suppression (0.48): Moderate-high. Individuals and exemption claimants face significant barriers: employment/school/travel restrictions, burden of proof on individuals for exemptions, limited appeals pathways, social stigma. But suppression is not total because proportionality in principle allows mandate relaxation as conditions improve, creating an exit condition (however distant). Theater ratio (0.42): Moderate. The proportionality frame requires ongoing measurement, public justification of thresholds, and periodic review — which creates theater (meeting after metric review, publishing rationales) but less than categorical mandates would. However, theater increases over time (measurements show 0.28→0.42) as authorities must continuously defend threshold choices against legal and public challenge.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates a genuine perspectival disagreement that cannot be resolved by further information about disease parameters alone. The authority sees pure coordination (threshold-based mandate as enabling rational deployment). The trapped individual sees snare (no exit regardless of proportionality logic, because they are the ones who will test mandate stringency). The exemption claimant sees tangled rope (genuine exemption principle coupled with gatekeeping extraction). The vulnerable population sees mixed benefit and constraint. The deliberative institutions see temporary coordination with sunset potential (as disease declines or vaccines improve, proportionality permits mandate relaxation). The categorical (false-summit) view risks naturalizing proportionality reasoning as an immutable requirement of public health, when in fact proportionality is a contingent institutional choice to balance competing values.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) for each perspective is derived from the agent's power level, exit options, and relationship to the extraction flow. The authority (institutional/arbitrage) derives low d: threshold control, discretionary exemptions, ability to adjust mandate stringency — beneficiary position with exit options. The trapped individual (powerless/trapped) derives high d: no control over thresholds, minimal exemption recourse, cannot exit jurisdiction or labor market easily. The exemption claimant (moderate/constrained) derives moderate-high d: benefits from exemption principle but constrained by verification burden. The vulnerable population (moderate/mobile) derives moderate d: some geographic/social mobility options, but health status limits practical exit. The deliberative institutions (organized/constrained) derive moderate d: organized capacity to revisit thresholds but constrained by urgent timelines and authority resistance to threshold revision downward. The analytical observer (analytical/analytical) derives canonical d from the analytical power atom (0.73), reflecting that the analytical position sits between beneficiary and victim, seeing structure from neither position.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint is not mandatrophic — extractiveness (0.38) does not exceed 0.70. However, there is a local tension within the tangled_rope classification: the public health authority genuinely believes it is solving a coordination problem (preventing disease spread), and from its perspective the constraint is rope (low-extraction coordination). The trapped individual genuinely experiences snare (high-extraction coercion), and from their perspective the constraint is snare (high-extraction extraction). Both cannot be fully right about the same constraint. The mandatrophy is resolved by noting that the two perspectives have genuinely different structural relationships to the constraint: the authority has threshold-control options (arbitrage exit) that the individual does not have. The constraint is tangled_rope from the individual's perspective because it contains BOTH coordination (community protection) AND extraction (threshold gatekeeping), not because the two perspectives are in contradiction. The proportionality reading's legitimacy claim is that the extraction component is justified by the coordination benefit. The false-summit perspective (categorical natural law view) risks naturalizing this as 'extraction is a necessary cost of public health' when it is actually a contingent institutional choice about how thresholds are set and maintained.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    threshold_calibration_epistemic_closure,
    'Who measures disease severity, transmission risk, and vaccine safety, and by what method? Does the measurement process itself foreclose certain readings?',
    'Documentary analysis of mandate-drafting process: whose data was included, whose excluded, which modeling assumptions were adopted. Comparative analysis across jurisdictions: different threshold choices produce different victim/beneficiary sets.',
    'If measurement is technical-expert-only: proportionality claim is theater, beneficiary (authority) controls severity definition. If measurement is democratically contestable: proportionality genuinely protects against categorical abuse. Classification shifts between snare and tangled_rope depending on measurement governance.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(threshold_calibration_epistemic_closure, empirical, 'Measurement process and epistemic authority for threshold calibration').

omega_variable(
    exemption_verification_burden_allocation,
    'Does the burden of proof for exemptions rest on the individual or on the health system? Is the burden proportional to the mandate''s stringency?',
    'Comparative law analysis: jurisdictions with individual-burden exemptions vs. those with health-system-burden exemptions. Empirical study of exemption denial rates and appeals success rates.',
    'High individual burden: snare for claimants (genuine medical exemptions denied due to gatekeeping). Distributed burden: tangled_rope (legitimate coordination with shared responsibility). Burden misalignment with mandate severity: constraint reclassifies upward in extractiveness.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(exemption_verification_burden_allocation, empirical, 'Burden allocation for exemption verification and its proportionality to mandate stringency').

omega_variable(
    sibling_reading_pathogen_contingency,
    'At what pathogen severity level does proportionality reading foreclose the bodily_autonomy_primary reading, and does that foreclosure depend on disease parameters or on the reading''s own commitments?',
    'Formal analysis of bodily_autonomy_primary axioms vs proportionality_reading axioms. Test: would a bodily_autonomy advocate accept the proportionality frame for smallpox? For seasonal flu? If acceptance threshold exists, the readings coexist; if bodily_autonomy advocate rejects ALL mandates categorically, readings foreclose.',
    'If contingent on pathogen: readings coexist_with (both can be held simultaneously by different parties depending on disease context). If categorical rejection: proportionality forecloses bodily_autonomy_primary axiom at high severity. Determines reading_relations entry in cs_structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_pathogen_contingency, conceptual, 'Whether sibling readings coexist contingently or foreclose categorically').

omega_variable(
    proportionality_gateway_capture_risk,
    'Does the proportionality frame prevent mission creep by authorities, or does it provide cover for creep by offering the appearance of limitation while shifting the extraction mechanism from mandate stringency to threshold definition?',
    'Historical analysis of mandate drift: thresholds revised upward (allowing more restrictive mandate) vs downward (allowing relaxation). Institutional analysis: do authorities who control threshold definition face external accountability? Is threshold-shifting faster or slower than public response time?',
    'If authorities capture threshold definition: extractiveness reclassifies upward (extraction mechanism shifts from mandate itself to the parameters that justify it). If thresholds remain contestable: tangled_rope classification holds. High-capture scenario reclassifies to snare.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(proportionality_gateway_capture_risk, empirical, 'Risk that proportionality frame enables gateway capture through threshold control').

omega_variable(
    exemption_robustness_across_contexts,
    'What does ''robust exemptions'' mean operationally, and do exemption standards vary across jurisdictions, pathogens, or over time in ways that undermine the robustness claim?',
    'Comparative analysis: exemption criteria, denial rates, appeals procedures across jurisdictions claiming proportionality mandate. Longitudinal analysis: are exemption standards consistent as disease parameters change?',
    'If exemption standards are inconsistent: proportionality claim is overstated, constraint reclassifies to snare (victims face unpredictable exclusion). If robust: tangled_rope classification holds with stronger exemption coordinate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(exemption_robustness_across_contexts, empirical, 'Operational meaning and consistency of exemption robustness across contexts').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(proportionality_reading, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(prop_tr_t0, proportionality_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement(prop_tr_t3, proportionality_reading, theater_ratio, 3, 0.38).
narrative_ontology:measurement(prop_tr_t6, proportionality_reading, theater_ratio, 6, 0.42).

% Extraction over time
narrative_ontology:measurement(prop_be_t0, proportionality_reading, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(prop_be_t3, proportionality_reading, base_extractiveness, 3, 0.35).
narrative_ontology:measurement(prop_be_t6, proportionality_reading, base_extractiveness, 6, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(proportionality_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(proportionality_reading, public_health_primary).
narrative_ontology:affects_constraint(proportionality_reading, bodily_autonomy_primary).

% DUAL FORMULATION NOTE:
% The proportionality_reading is one reading of the vaccine_mandate_balance kernel. The public_health_primary reading and bodily_autonomy_primary reading are sibling readings of the same kernel, not separate constraints. All three readings describe the same natural fact (vaccine mandates exist) but instantiate different interpretive frames. The network edge indicates reading_relations, not constraint_affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(proportionality_reading, institutional, 0.18).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
