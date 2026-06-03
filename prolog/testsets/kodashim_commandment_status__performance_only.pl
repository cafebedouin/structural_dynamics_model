% ============================================================================
% CONSTRAINT STORY: kodashim_commandment_status__performance_only
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_kodashim_commandment_status__performance_only, []).

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
 *   constraint_id: kodashim_commandment_status__performance_only
 *   human_readable: Kodashim Commandment Status: Performance-Only Reading (Temple Contingency)
 *   domain: religious_studies/halakhic_theory/commitment_systems
 *
 * SUMMARY:
 *   The performance-only reading of Kodashim commandment status asserts that
 *   sacrifice laws are contingent on Temple existence; without the altar, the
 *   commandment is suspended, reducing to a legal husk. This reading
 *   instantiates one strand of a contested kernel about whether the halakhic
 *   status of sacrifice laws remains active, is actively suspended
 *   (performance-only), is maintained through study as alternative
 *   performance (study_as_performance), or is temporally deferred pending
 *   messianic restoration (messianic_deferral). The performance-only reading
 *   treats the contingency as absolute: no Temple means no commandment, only
 *   its historical and textual preservation. This creates a structural
 *   extraction dynamic. The observant community remains bound to study laws
 *   it cannot perform; rabbinic authority maintains interpretive control over
 *   suspension status; scholarly resources continue flowing to a framework
 *   disconnected from contemporary Jewish practice. The constraint
 *   demonstrates how a theological choice (treating suspension as terminal)
 *   becomes a structural mechanism for maintaining authority asymmetry and
 *   extracting continued engagement from a community. The theater ratio rises
 *   from 0.48 to 0.68 over the measurement interval, indicating increasing
 *   performativity of Kodashim study as the distance from Temple sacrifice
 *   grows and functional rationalization becomes more elaborate.
 *   Extractiveness rises from 0.42 to 0.58 as alternative readings
 *   (study_as_performance, messianic_deferral) lose cultural salience and the
 *   performance-only reading consolidates institutional dominance.
 *
 * KEY AGENTS:
 *   - Observant Jewish Community: Primary victims (powerless/trapped) — religiously bound to master suspended laws; cannot exit without identity dissolution
 *   - Jewish Scholarly Community: Secondary victims (moderate/constrained) — institutionally expected to maintain expertise in non-performable law; face career costs for deprioritization
 *   - Rabbinic Authority: Primary beneficiary (institutional/arbitrage) — maintains interpretive monopoly over suspension status; controls which frameworks are legitimate
 *   - Progressive/Reconstructionist Movements: Organized challengers (organized/constrained) — see coordination function but resist extractive overlay; partial exit available through reinterpretation
 *   - The Halakhic Study Ritual: Institutional practice (institutional/arbitrage) — persists through status maintenance and inertia despite functional atrophy (piton classification)
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing performance-only reading as logical necessity rather than theological choice
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(kodashim_commandment_status__performance_only, 0.58).
domain_priors:suppression_score(kodashim_commandment_status__performance_only, 0.65).
domain_priors:theater_ratio(kodashim_commandment_status__performance_only, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(kodashim_commandment_status__performance_only, extractiveness, 0.58).
narrative_ontology:constraint_metric(kodashim_commandment_status__performance_only, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(kodashim_commandment_status__performance_only, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(kodashim_commandment_status__performance_only, snare).
narrative_ontology:human_readable(kodashim_commandment_status__performance_only, "Kodashim Commandment Status: Performance-Only Reading (Temple Contingency)").
narrative_ontology:topic_domain(kodashim_commandment_status__performance_only, "religious_studies/halakhic_theory/commitment_systems").

domain_priors:requires_active_enforcement(kodashim_commandment_status__performance_only).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(kodashim_commandment_status__performance_only, '57ee74e9-1155-4f43-adc2-7131a3ea6533').
narrative_ontology:cs_kernel_codification('57ee74e9-1155-4f43-adc2-7131a3ea6533', formalized).
narrative_ontology:cs_authority_grounding('57ee74e9-1155-4f43-adc2-7131a3ea6533', lineage).
narrative_ontology:cs_interpretation_layer_present('57ee74e9-1155-4f43-adc2-7131a3ea6533').
narrative_ontology:cs_reading_relation('57ee74e9-1155-4f43-adc2-7131a3ea6533', kodashim_commandment_status__study_as_performance, forecloses).
narrative_ontology:cs_reading_relation('57ee74e9-1155-4f43-adc2-7131a3ea6533', kodashim_commandment_status__messianic_deferral, coexists_with).
narrative_ontology:cs_axiom('57ee74e9-1155-4f43-adc2-7131a3ea6533', foundational, suspension_is_terminal).
narrative_ontology:cs_axiom_status(suspension_is_terminal, holdable).
narrative_ontology:cs_axiom_grounding('57ee74e9-1155-4f43-adc2-7131a3ea6533', suspension_is_terminal, deontological).
narrative_ontology:cs_axiom('57ee74e9-1155-4f43-adc2-7131a3ea6533', foundational, study_is_preservation_not_performance).
narrative_ontology:cs_axiom_status(study_is_preservation_not_performance, holdable).
narrative_ontology:cs_axiom_grounding('57ee74e9-1155-4f43-adc2-7131a3ea6533', study_is_preservation_not_performance, conventional).
narrative_ontology:cs_reference_frame('57ee74e9-1155-4f43-adc2-7131a3ea6533', temple_sacrificial_obligation_active).
narrative_ontology:cs_drift_state('57ee74e9-1155-4f43-adc2-7131a3ea6533', contemporary_post_exile, gap(codification_collapse, severe, false)).
narrative_ontology:cs_created_at('57ee74e9-1155-4f43-adc2-7131a3ea6533', '').
narrative_ontology:cs_kernel_id(kodashim_commandment_status__performance_only, kodashim_commandment_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(kodashim_commandment_status__performance_only, rabbinic_institutional_authority).
narrative_ontology:constraint_victim(kodashim_commandment_status__performance_only, jewish_religious_community).
narrative_ontology:constraint_victim(kodashim_commandment_status__performance_only, potential_alternative_scholarship_allocation).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: OBSERVANT BELIEVER (SNARE) — Trapped within the legal framework that declares the sacrifice commandment suspended yet remains binding in study form. Perceives the constraint as extractive: expected to invest cognitive resources in mastering laws that cannot be performed, with no exit. The suppression is structural — religious identity makes exit unthinkable; the trap is that study obligations persist despite functional suspension of the actual commandment.
constraint_indexing:constraint_classification(kodashim_commandment_status__performance_only, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: JEWISH SCHOLARLY COMMUNITY (SNARE) — Constrained by institutional expectations to maintain expertise in laws that cannot be performed. High career costs for deprioritizing Kodashim study. Resources diverted from applied halakha or other pressing communal needs. The extractive mechanism: maintaining the fiction that studying suspended laws is equivalent to performing them, thereby justifying continued resource allocation to a framework disconnected from actual Jewish practice.
constraint_indexing:constraint_classification(kodashim_commandment_status__performance_only, snare,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: RABBINIC INSTITUTIONAL AUTHORITY (ROPE) — Benefits from the suspended-yet-binding framework. Maintains interpretive monopoly over which laws are active vs. suspended. The constraint serves as coordination mechanism: it preserves textual authority and hermeneutic control by keeping the entire corpus binding regardless of performability. Experiences low extraction cost because the framework can arbitrage between performance-based and study-based readings as institutional interest dictates.
constraint_indexing:constraint_classification(kodashim_commandment_status__performance_only, rope,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: PROGRESSIVE MOVEMENTS (TANGLED ROPE) — Organized but constrained. See genuine coordination function (preserving textual knowledge across exile) alongside extractive overlay (maintaining authority asymmetry through complicated law status). Partial exit is possible (reinterpreting the framework) but carries community cost. Mixed perspective: some benefit from reformed interpretation, but still bound by traditional framework for legitimacy-seeking.
constraint_indexing:constraint_classification(kodashim_commandment_status__performance_only, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: HALAKHIC STUDY RITUAL (PITON) — The institutional practice of studying sacrifice laws has become largely performative. The ritual persists through institutional inertia and status maintenance rather than functional necessity. The theater is high: detailed discussion of Temple procedures, altar dimensions, and sacrifice protocols continues with the same hermeneutic intensity as binding law, despite universal acknowledgment that performance is impossible. The piton captures that the practice has atrophied functionally but is maintained through institutional commitment.
constraint_indexing:constraint_classification(kodashim_commandment_status__performance_only, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (MOUNTAIN) — Risks naturalizing the performance-only reading as an immutable logical consequence: 'Without altar, sacrifice laws cannot be performed; therefore they are suspended.' This perspective treats the contingency as a law of logic rather than as a theological choice. However, the false summit detector should flag this: the performance-only reading is ONE reading of the kernel, not a natural law. Alternative readings (study_as_performance, messianic_deferral) show the kernel admits multiple legitimacy structures.
constraint_indexing:constraint_classification(kodashim_commandment_status__performance_only, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(kodashim_commandment_status__performance_only_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(kodashim_commandment_status__performance_only, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(kodashim_commandment_status__performance_only, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(kodashim_commandment_status__performance_only, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(kodashim_commandment_status__performance_only, TR),
    TR >= 0.70.

:- end_tests(kodashim_commandment_status__performance_only_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high, reflecting the core extraction mechanism. The performance-only reading requires continued investment in mastering laws that cannot be performed, with the justification that study preserves the legal corpus for hypothetical future restoration. However, extractiveness is not extreme (>0.70) because: (a) the preservation function is genuine — the community does maintain halakhic knowledge through study; (b) some interpreters find genuine intellectual satisfaction in the legal analysis regardless of performability; (c) the constraint can be challenged through alternative readings (study_as_performance, messianic_deferral). Suppression (0.65): Moderate-high. The suppression reflects multiple barriers: religious identity makes exiting the framework unthinkable; authority concentration in rabbinic interpretation prevents unilateral redefinition; institutional prestige attaches to Kodashim expertise, making deprioritization costly; textual tradition treats all law as binding regardless of performability. Theater ratio (0.68): High and rising. The performativity reflects the elaborate hermeneutic activity surrounding laws that everyone acknowledges cannot be performed. The measurement progression shows theater increasing as direct functional connection to Temple sacrifice fades and intellectual justification becomes more elaborate. This rising theater is diagnostic of extraction consolidation: as the functional basis erodes, the performative justification intensifies.
 *
 * PERSPECTIVAL GAP:
 *   The performance-only reading produces a wide perspectival gap across power levels. Rabbinic authority sees the framework as coordination (Rope) — it preserves textual knowledge and maintains interpretive authority. The observant community sees extraction (Snare) — they are trapped in obligations to master non-performable law. The scholarly community sees mixed dynamics (Tangled Rope) — genuine preservation function alongside extractive burden. Progressive movements see a degraded but contestable system (Piton or Tangled Rope) — the performativity is visible but the authority structure shields it from direct challenge. The civilizational analytical observer risks seeing logical necessity (Mountain) — but the false summit detector identifies this as naturalization of a contingent reading choice. The gap reveals that the performance-only reading's apparent logical inevitability ('no Temple, no commandment') is actually a particular theological stance that other readings contest.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective derives its directionality from the agent's structural position relative to the suspension mechanism. The observant believer (victim + trapped) has d ≈ 0.95, producing maximum f(d) ≈ 1.42 and high experienced extraction. The scholarly community (victim + constrained) has d ≈ 0.72, producing f(d) ≈ 1.15 and moderate-high extraction. Rabbinic authority (beneficiary + arbitrage) has d ≈ 0.08, producing f(d) ≈ -0.10, giving them negative effective extraction (the constraint subsidizes their authority position). Progressive movements (partial beneficiary, partial victim + constrained) have d ≈ 0.55, producing f(d) ≈ 0.75 and tangled rope experience. The piton perspective reflects that the study ritual has become functionally disconnected from its original purpose — the institutional practice persists through inertia rather than through active extraction, giving it a performative rather than extractive signature.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    performance_vs_readiness_boundary,
    'Is the commandment genuinely suspended (performance-only reading), or is study itself a form of readiness that keeps the commandment alive in displaced form (study_as_performance reading)?',
    'Textual analysis of Talmudic sources declaring suspension vs. sources framing study as alternative fulfillment; historical tracking of whether the performance-only reading gained dominance through logical argument or through institutional authority assertion',
    'If suspension is absolute: performance-only reading correct; extractiveness ≈ 0.58 (snare). If study counts as fulfillment: study_as_performance reading correct; extractiveness ≈ 0.25 (rope). This is THE core omega: the entire classification turns on what counts as commandment fulfillment.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(performance_vs_readiness_boundary, conceptual, 'Whether commandment is suspended or study counts as alternative fulfillment').

omega_variable(
    messianic_restoration_timeline,
    'Does the performance-only reading acknowledge messianic restoration as the terminal state, or does it treat suspension as indefinite/permanent?',
    'Comparison of halakhic sources: does the performance-only reading explicitly reference Temple restoration, or treat it as outside the scope of contemporary law? Survey of rabbinical authority positions on whether suspension is temporary or terminal.',
    'If restoration is acknowledged: messianic_deferral reading has legitimate claim; performance-only is shown as interim framework. If suspension is treated as permanent: performance-only reading stands alone as terminal authority structure; classification and extractiveness remain stable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(messianic_restoration_timeline, conceptual, 'Whether suspension is temporary (awaiting messianic restoration) or permanent').

omega_variable(
    institutional_authority_concentration,
    'Does the performance-only reading require a centralized halakhic authority to declare suspension, or can authority be distributed across interpreters?',
    'Historical analysis: which institutional body (if any) formally declared the suspension? Do subsequent authorities treat suspension as binding precedent or as revisable interpretation? Does performance-only reading depend on Sanhedrin authority (now absent) or on distributed rabbinic consensus?',
    'If centralized authority required: performance-only reading is vulnerable to authority vacuum (post-Sanhedrin challenge). If distributed: performance-only reading is more stable but less powerful (rabbinic consensus can shift). Affects whether the snare classification persists under institutional reorganization.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_authority_concentration, empirical, 'Whether suspension requires centralized halakhic authority').

omega_variable(
    resource_allocation_counterfactual,
    'If the performance-only reading were replaced by study_as_performance or messianic_deferral, would released scholarly resources actually redirect to high-priority halakhic areas, or would Kodashim remain dominant through institutional inertia?',
    'Counterfactual analysis: survey of scholars deprioritizing Kodashim study; assess whether reallocation is blocked by tradition (all law is equally binding) or by institutional status (Kodashim expertise confers prestige). Track resource flows in movements that have challenged performance-only reading (Progressive Judaism) to measure actual reallocation.',
    'If reallocation is real: performance-only reading genuinely extracts resources; snare classification justified. If reallocation does not occur: extractiveness is lower; victim set is smaller; classification should shift toward tangled_rope. This omega determines whether the high extractiveness is structural or aspirational.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(resource_allocation_counterfactual, empirical, 'Whether replacing performance-only reading would free scholarly resources').

omega_variable(
    reading_council_authority,
    'Does the performance-only reading coexist with study_as_performance and messianic_deferral as three legitimate readings held by different communities, or does it foreclose the others through logical contradiction?',
    'Detailed exegetical analysis: Do the three readings rest on incompatible premises about what fulfills a commandment, or do they represent different theological priorities that could coexist in a single framework? Can a community hold all three as valid interpretations for different contexts?',
    'If coexist: reading_relations should be coexists_with; sibling readings are live alternatives. If foreclose: reading_relations should be forecloses; performance-only reading eliminates logical space for alternatives. This determines the political and epistemic status of the sibling readings.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_council_authority, conceptual, 'Whether performance-only reading coexists with or forecloses sibling readings').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(kodashim_commandment_status__performance_only, 0, 1000).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(kodashim_perf_tr_t0, kodashim_commandment_status__performance_only, theater_ratio, 0, 0.48).
narrative_ontology:measurement(kodashim_perf_tr_t500, kodashim_commandment_status__performance_only, theater_ratio, 500, 0.58).
narrative_ontology:measurement(kodashim_perf_tr_t1000, kodashim_commandment_status__performance_only, theater_ratio, 1000, 0.68).

% Extraction over time
narrative_ontology:measurement(kodashim_perf_be_t0, kodashim_commandment_status__performance_only, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(kodashim_perf_be_t500, kodashim_commandment_status__performance_only, base_extractiveness, 500, 0.48).
narrative_ontology:measurement(kodashim_perf_be_t1000, kodashim_commandment_status__performance_only, base_extractiveness, 1000, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(kodashim_perf_su_t0, kodashim_commandment_status__performance_only, suppression_requirement, 0, 0.52).
narrative_ontology:measurement(kodashim_perf_su_t500, kodashim_commandment_status__performance_only, suppression_requirement, 500, 0.6).
narrative_ontology:measurement(kodashim_perf_su_t1000, kodashim_commandment_status__performance_only, suppression_requirement, 1000, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(kodashim_commandment_status__performance_only, identity_coordination).
narrative_ontology:affects_constraint(kodashim_commandment_status__performance_only, kodashim_commandment_status__study_as_performance).
narrative_ontology:affects_constraint(kodashim_commandment_status__performance_only, kodashim_commandment_status__messianic_deferral).

% DUAL FORMULATION NOTE:
% The Kodashim commandment status kernel admits three structurally distinct readings with different extractiveness values. Each reading is a separate constraint story linked via network.affects_constraints. The performance-only reading (this file) has ε ≈ 0.58 (Snare). The study_as_performance reading has estimated ε ≈ 0.28 (Rope) — study as fulfillment reduces extraction by providing genuine functional equivalence. The messianic_deferral reading has estimated ε ≈ 0.32 (Rope/Tangled Rope) — deferral to future restoration maintains readiness without terminal extraction. Institutional consolidation toward performance-only represents a drift from lower-extraction readings toward higher extraction, not a drift within a single constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(kodashim_commandment_status__performance_only, institutional, 0.1).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
