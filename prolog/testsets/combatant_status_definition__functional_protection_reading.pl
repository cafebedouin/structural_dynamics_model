% ============================================================================
% CONSTRAINT STORY: combatant_status_definition__functional_protection_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_combatant_status_definition__functional_protection_reading, []).

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
 *   constraint_id: combatant_status_definition__functional_protection_reading
 *   human_readable: Combatant Status Definition: Functional Protection Reading
 *   domain: international_humanitarian_law/armed_conflict
 *
 * SUMMARY:
 *   The combatant status definition in International Humanitarian Law (IHL)
 *   is a contested kernel—a foundational commitment to which different
 *   parties assign different readings. This constraint story instantiates the
 *   FUNCTIONAL PROTECTION READING: the position that all detained persons
 *   receive Common Article 3 minimum protections (humane treatment, medical
 *   care, fair trial rights) immediately upon detention, regardless of their
 *   combatant status or the outcome of any status-determination process. This
 *   reading makes humane treatment status-independent. It contrasts with the
 *   state_centric_reading (which conditions baseline protections on prior
 *   status determination) and the national_liberation_reading (which extends
 *   combatant protections and POW status to members of liberation movements
 *   regardless of uniform or command structure). The functional protection
 *   reading is grounded in the principle that human dignity is unconditional
 *   and cannot be suspended pending adjudication. It emerges from the ICRC's
 *   interpretation of Common Article 3 and has been adopted in various forms
 *   by international courts and humanitarian bodies, but remains
 *   contested—many states retain practices aligned with the state-centric
 *   reading, particularly in non-international armed conflicts where status
 *   determination is most ambiguous. The constraint's low extractiveness
 *   (0.15) reflects that it primarily solves a coordination problem
 *   (clarifying which protections apply universally) rather than imposing
 *   asymmetric costs. However, small increase in theater_ratio (0.35 → 0.40)
 *   over the interval reflects increasing use of administrative categories
 *   and procedural delay to circumvent baseline protections, suggesting the
 *   functional protection floor is being performatively acknowledged while
 *   operationally evaded.
 *
 * KEY AGENTS:
 *   - Detainees (all categories): Primary beneficiary (powerless/trapped) — receive baseline protections immediately, without status-dependent delay
 *   - Detaining Military Force: Mixed (institutional/constrained) — gains coordination clarity but bears operational cost of providing baseline protections to all
 *   - International Committee of the Red Cross (ICRC): Primary beneficiary (institutional/arbitrage) — constraint enables ICRC's mandate; removes need for case-by-case negotiation
 *   - State Party Legal System: Mixed (institutional/constrained) — gains interstate coordination but loses discretion to defer rights pending status determination
 *   - Civilian Misclassified as Combatant: Partial victim (powerless/identity_locked) — receives baseline protections but remains trapped by incorrect classification
 *   - Civil Society Monitoring Coalition: Organized actor (organized/constrained) — sees constraint as temporary scaffold supporting development of fair adjudication processes
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing the functional reading as inherent human dignity rather than contingent institutional choice
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(combatant_status_definition__functional_protection_reading, 0.15).
domain_priors:suppression_score(combatant_status_definition__functional_protection_reading, 0.35).
domain_priors:theater_ratio(combatant_status_definition__functional_protection_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(combatant_status_definition__functional_protection_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(combatant_status_definition__functional_protection_reading, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(combatant_status_definition__functional_protection_reading, theater_ratio, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(combatant_status_definition__functional_protection_reading, rope).
narrative_ontology:human_readable(combatant_status_definition__functional_protection_reading, "Combatant Status Definition: Functional Protection Reading").
narrative_ontology:topic_domain(combatant_status_definition__functional_protection_reading, "international_humanitarian_law/armed_conflict").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(combatant_status_definition__functional_protection_reading, '845ae520-6925-4c14-a1ca-67e9046d9936').
narrative_ontology:cs_kernel_codification('845ae520-6925-4c14-a1ca-67e9046d9936', formalized).
narrative_ontology:cs_authority_grounding('845ae520-6925-4c14-a1ca-67e9046d9936', lineage).
narrative_ontology:cs_interpretation_layer_present('845ae520-6925-4c14-a1ca-67e9046d9936').
narrative_ontology:cs_reading_relation('845ae520-6925-4c14-a1ca-67e9046d9936', combatant_status_definition__state_centric_reading, coexists_with).
narrative_ontology:cs_reading_relation('845ae520-6925-4c14-a1ca-67e9046d9936', combatant_status_definition__national_liberation_reading, coexists_with).
narrative_ontology:cs_axiom('845ae520-6925-4c14-a1ca-67e9046d9936', foundational, baseline_protections_status_independent).
narrative_ontology:cs_axiom_status(baseline_protections_status_independent, holdable).
narrative_ontology:cs_axiom_grounding('845ae520-6925-4c14-a1ca-67e9046d9936', baseline_protections_status_independent, deontological).
narrative_ontology:cs_axiom('845ae520-6925-4c14-a1ca-67e9046d9936', foundational, human_dignity_unconditional).
narrative_ontology:cs_axiom_status(human_dignity_unconditional, holdable).
narrative_ontology:cs_axiom_grounding('845ae520-6925-4c14-a1ca-67e9046d9936', human_dignity_unconditional, deontological).
narrative_ontology:cs_reference_frame('845ae520-6925-4c14-a1ca-67e9046d9936', universal_baseline_protections_framework).
narrative_ontology:cs_drift_state('845ae520-6925-4c14-a1ca-67e9046d9936', contemporary_armed_conflict_practice, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('845ae520-6925-4c14-a1ca-67e9046d9936', '').
narrative_ontology:cs_kernel_id(combatant_status_definition__functional_protection_reading, combatant_status_definition).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(combatant_status_definition__functional_protection_reading, detainees_all_categories).
narrative_ontology:constraint_beneficiary(combatant_status_definition__functional_protection_reading, international_humanitarian_order).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DETAINED PERSON (ROPE) — Under functional protection reading, detainees receive baseline humanitarian protections (medical care, humane treatment, prohibition on torture) immediately upon detention, without waiting for status determination. The constraint solves a coordination problem: enables detaining forces to provide care without legal uncertainty, and protects detainees from indefinite rights limbo. Low extraction—the reading removes the worst mechanism of the constraint (status-based deprivation). Classified as rope from powerless perspective because the detainee gets a floor (coordination) rather than being stratified by contested status.
constraint_indexing:constraint_classification(combatant_status_definition__functional_protection_reading, rope,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: DETAINING MILITARY FORCE (TANGLED ROPE) — Experiences mixed coordination and extraction. Coordination benefit: Common Article 3 floor removes legal uncertainty—forces know exactly which protections are mandatory regardless of status. Extraction cost: providing humane treatment and medical care to all detainees (including combatants) is a resource and operational constraint. The reading requires immediate provision of baseline protections without waiting for adjudication, which is both a coordination gain (clarity) and an enforcement burden (cost). Extraction is moderate, not total, because the force retains control over detention conditions within the humane-treatment floor.
constraint_indexing:constraint_classification(combatant_status_definition__functional_protection_reading, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: INTERNATIONAL HUMANITARIAN ORGANIZATION (ROPE) — Experiences the functional protection reading as pure coordination. The ICRC's mandate is to ensure detainees receive humane treatment and fair procedures. The reading provides a clear, binding baseline that applies universally—no status determination required. This eliminates the ICRC's need to negotiate individual protection cases and provides an arbitrage advantage: the ICRC can rely on treaty obligation rather than persuasion. Low extraction, high coordination value. The constraint enables the ICRC's function.
constraint_indexing:constraint_classification(combatant_status_definition__functional_protection_reading, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: STATE PARTY LEGAL SYSTEM (TANGLED ROPE) — Experiences mixed effects. Coordination benefit: Common Article 3 creates a uniform international standard that states can adopt, reducing coordination costs with other states and with non-state armed groups. Extraction cost: the reading requires states to provide baseline protections to all detainees regardless of status, which removes states' ability to use legal uncertainty as a tool to defer rights. The reading constrains state discretion over detention policy. However, it also coordinates interstate behavior—states can predict other states' detention practices and adjudicate disputes more reliably.
constraint_indexing:constraint_classification(combatant_status_definition__functional_protection_reading, tangled_rope,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: CIVILIAN MISCLASSIFIED AS COMBATANT (SNARE) — For a civilian detained and initially classified as a combatant, the functional protection reading provides a floor but does NOT guarantee correct status determination. The constraint ensures humane treatment while detained, but the classification error itself is not prevented by this reading. The civilian is trapped by the misclassification and identity-locked to combatant status until (if) an adjudication process corrects it. The functional protection reading mitigates (humane treatment applies) but does not eliminate the snare (incorrect status persists). The reading reduces the worst extraction (torture, denial of medical care) but extraction persists via the status error itself.
constraint_indexing:constraint_classification(combatant_status_definition__functional_protection_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(global))).

% PERSPECTIVE 6: CIVIL SOCIETY MONITORING COALITION (SCAFFOLD) — Organized non-state actors (human rights organizations, transitional justice bodies) see the functional protection reading as a temporary scaffolding toward a fuller status-determination system. The reading provides an immediate floor (baseline protections) while adjudication mechanisms develop. The constraint has a sunset clause embedded: as international criminal courts and transitional justice institutions mature, they enable more robust status determination and procedural fairness. The scaffold is real—the floor protections are coordination gains—but the ultimate function is to support development of fair procedures that will make status determination less arbitrary.
constraint_indexing:constraint_classification(combatant_status_definition__functional_protection_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (MOUNTAIN) — From a civilizational/universal perspective, basic human dignity and freedom from torture are irrefutable principles that cannot be suspended on procedural grounds. The functional protection reading presents this as a natural law: all persons have inherent rights to humane treatment regardless of status because the alternative (indefinite rights suspension pending status determination) violates the natural law of human dignity. However, this is a false summit—the 'natural law' framing conceals a contingent institutional choice (which protections apply universally vs. conditionally) and beneficiaries (international humanitarian system, states that prefer clarity). The engine's false summit detector will flag this perspective.
constraint_indexing:constraint_classification(combatant_status_definition__functional_protection_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(combatant_status_definition__functional_protection_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(combatant_status_definition__functional_protection_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(combatant_status_definition__functional_protection_reading, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

:- end_tests(combatant_status_definition__functional_protection_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.15): Low. The functional protection reading is primarily a coordination mechanism—it clarifies which protections apply universally regardless of status, reducing legal uncertainty for all parties. The modest extractiveness reflects two factors: (1) detaining forces must provide baseline protections (humane treatment, medical care) immediately, which has operational cost, but this cost is moderate because the protections are basic (not POW-level). (2) The reading removes the ability of detaining forces to defer rights indefinitely pending status determination, which reduces their discretion but does not impose a snare-level extraction. Suppression (0.35): Moderate. Barriers to exit from the constraint include treaty obligations (Common Article 3 is binding on all parties), international monitoring (ICRC), and reputational costs of violating humanitarian norms. However, suppression is not total because states retain implementation discretion and can use procedural delay to circumvent the floor. Theater ratio (0.40): Moderate. The functional protection reading is partially performative. Detaining forces publicly commit to humane treatment while using administrative categories and procedural delay to avoid substantive baseline protections (e.g., labeling detainees 'administrative detainees' rather than POWs to avoid higher-level protections; deferring medical care pending status determination despite the constraint). The theater ratio has slightly increased over the interval as states have developed procedural workarounds. Claimed type (Rope): The reading is fundamentally a coordination mechanism—it solves the collective action problem of determining which protections apply when status is uncertain. The functional protection floor is a Pareto improvement over status-dependent deprivation: detaining forces gain legal clarity, detainees gain immediate baseline protections, humanitarian organizations gain a binding standard, and states gain a coordination point with other states.
 *
 * PERSPECTIVAL GAP:
 *   The functional protection reading produces dramatic perspectival gaps. Detainees experience it as rope (baseline coordination protection). Detaining forces experience it as tangled_rope (mixed coordination and extraction burden). The ICRC experiences it as rope (pure coordination enabling their function). States experience it as tangled_rope (coordination gains with costs to discretion). Misclassified civilians experience it as snare (protections from torture but trapped by classification error). Civil society sees it as scaffold (temporary floor supporting development of fair procedures). The analytical observer risks seeing it as mountain (natural law of human dignity), but structural analysis reveals it as a contingent institutional choice that benefits identifiable agents (humanitarian organizations, coordinating states). The gap between snare and rope perspectives is particularly sharp: the same constraint provides genuine protection (rope) for most detainees but fails to prevent the worst extraction (identity-locked classification) for misclassified civilians. This is the constraint's key structural weakness—it is not universally protective, only protective up to the baseline floor.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values (d) for each perspective are derived from the agent's structural position—whether they are beneficiaries or victims, and what exit options they have. Detainees (powerless/trapped) with no exit but benefiting from baseline protections: d ≈ 0.40 (moderate), classified as rope. Detaining forces (institutional/constrained) that gain coordination but bear protection costs: d ≈ 0.55 (mixed), classified as tangled_rope. ICRC (institutional/arbitrage) as pure beneficiary with high exit options: d ≈ 0.15 (low), classified as rope. States (institutional/constrained) gaining coordination but losing discretion: d ≈ 0.50 (mixed), classified as tangled_rope. Misclassified civilians (powerless/identity_locked) bearing classification error burden: d ≈ 0.75 (high), classified as snare. The engine derives d automatically from these structural parameters and applies the sigmoid f(d) to compute effective extractiveness χ for each perspective. No directionality overrides are needed—the structural data is clear.
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLUTION BY READING SPECIFICATION: This constraint avoids mandatrophy by being specific about which baseline protections are status-independent. The functional protection reading explicitly declares that Common Article 3 protections (humane treatment, prohibition of torture, medical care, fair trial rights) apply to all detainees regardless of combatant status, but DOES NOT declare that POW-level protections (which are higher) apply to all detainees. This disambiguation resolves the potential mandatrophy: the reading is not claiming all extraction mechanisms are coordination (which would be false—the constraint does impose costs on detaining forces), and it is not claiming all coordination is extraction (which would be false—the baseline protections do solve genuine coordination problems). The reading is a rope precisely because it IS a coordination mechanism—the alternative (indefinite rights suspension pending status determination) is worse for all parties than the baseline floor. The mandatrophy does not arise because the reading's structural claim is clear: status determination is necessary for assigning higher-level protections (POW status, enemy combatant status), but baseline protections do not require status determination. This specificity prevents the false dilemma that would generate mandatrophy.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    status_determination_deferral_mechanism,
    'Does the functional protection reading defer status determination indefinitely, or does it presuppose a secondary adjudication process that will eventually classify detainees?',
    'Examination of IHL framework''s relationship between Common Article 3 floor and subsequent status-determination procedures (tribunals, administrative review, judicial process). If deferral is indefinite: reading is a pure floor with no exit. If deferral presupposes eventual adjudication: reading is a temporary scaffold.',
    'If indefinite deferral: some detainees remain in legal limbo indefinitely despite humane treatment (snare persists). If eventual adjudication: the reading is genuinely scaffold-like—a floor supporting development of fair procedures.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(status_determination_deferral_mechanism, conceptual, 'Whether functional protection defers status determination indefinitely or presupposes eventual adjudication').

omega_variable(
    enforcement_cost_vs_coordination_gain,
    'For detaining forces, do the operational costs of providing immediate baseline protections (medical care, humane treatment) exceed the coordination benefits (legal clarity, reduced case-by-case negotiation)?',
    'Comparative analysis of detention practices under pure status-based regimes vs. functional protection regimes. Measurement of ICRC negotiation burden, medical resource requirements, procedural overhead.',
    'If costs exceed benefits: detaining forces experience snare (forced provision of costly protections). If benefits exceed costs: tangled_rope classification holds (mixed coordination and extraction).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_cost_vs_coordination_gain, empirical, 'Whether baseline protection costs exceed coordination gains for detaining forces').

omega_variable(
    kernel_reading_contest_empirical_status,
    'Which sibling reading (state_centric vs. national_liberation vs. functional_protection) is actually enforced in practice across armed conflicts?',
    'Systematic audit of detention practices in non-international armed conflicts: coding whether detainees receive status-dependent or status-independent baseline protections. International Committee of the Red Cross (ICRC) detention monitoring reports across multiple conflicts.',
    'If functional_protection is enforced: the reading is stable and legitimate. If state_centric or national_liberation dominate: functional_protection is aspirational, not structural. May trigger reclassification of this constraint as piton (degraded) rather than rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest_empirical_status, empirical, 'Empirical prevalence of functional protection reading in actual detention practices').

omega_variable(
    civilian_misclassification_baseline_capture,
    'Does the baseline protection floor genuinely prevent torture and denial of medical care for misclassified civilians, or do detaining forces circumvent it through procedural delay and administrative category games?',
    'Analysis of documented cases: civilians detained as combatants and receiving medical care, humane treatment, and fair trial rights immediately vs. cases where procedural delay enabled deprivation. ICRC and transitional justice records.',
    'If floor is effective: functional_protection reading delivers real protection gains. If circumvented: the reading is piton (performative) rather than rope (functional).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(civilian_misclassification_baseline_capture, empirical, 'Whether baseline protections are actually enforced for misclassified detainees').

omega_variable(
    reading_commission_and_contestation,
    'This constraint is one of three readings of the combatant_status_definition kernel. What is the historical and legal relationship between readings? Has one superseded the others, or do they remain live positions in international humanitarian law?',
    'Review of treaty history, state practice, ICRC commentary, and international court decisions on combatant status definition. Mapping which states adopt which reading; whether readings are explicitly rejected or merely contested.',
    'If functional_protection has achieved near-universal acceptance: it is the primary reading and others are overridden. If readings coexist across different states/conflicts: they genuinely coexist_with relation holds. If readings are in logical conflict: forecloses relations apply.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_commission_and_contestation, conceptual, 'Historical and legal status of the three combatant-status readings').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(combatant_status_definition__functional_protection_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comb_func_tr_t0, combatant_status_definition__functional_protection_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement(comb_func_tr_t15, combatant_status_definition__functional_protection_reading, theater_ratio, 15, 0.38).
narrative_ontology:measurement(comb_func_tr_t30, combatant_status_definition__functional_protection_reading, theater_ratio, 30, 0.4).

% Extraction over time
narrative_ontology:measurement(comb_func_be_t0, combatant_status_definition__functional_protection_reading, base_extractiveness, 0, 0.08).
narrative_ontology:measurement(comb_func_be_t15, combatant_status_definition__functional_protection_reading, base_extractiveness, 15, 0.12).
narrative_ontology:measurement(comb_func_be_t30, combatant_status_definition__functional_protection_reading, base_extractiveness, 30, 0.15).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(combatant_status_definition__functional_protection_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(combatant_status_definition__functional_protection_reading, combatant_status_definition__state_centric_reading).
narrative_ontology:affects_constraint(combatant_status_definition__functional_protection_reading, combatant_status_definition__national_liberation_reading).
narrative_ontology:affects_constraint(combatant_status_definition__functional_protection_reading, torture_prohibition_international_humanitarianism).
narrative_ontology:affects_constraint(combatant_status_definition__functional_protection_reading, fair_trial_rights_armed_conflict).

% DUAL FORMULATION NOTE:
% The combatant_status_definition kernel has three structurally distinct readings corresponding to different institutional positions and legal traditions. This constraint (functional_protection_reading) specifies that baseline protections are status-independent. The sibling readings (state_centric and national_liberation) make different structural claims about what determines status and what protections follow. All three are constraints, with different epsilon values and different networks of institutional support. Functional_protection_reading affects downstream constraints on torture prohibition and fair trial rights because it provides the baseline condition under which those downstream constraints operate. All three readings affect each other through the kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
