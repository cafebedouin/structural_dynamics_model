% ============================================================================
% CONSTRAINT STORY: second_amendment_text__originalist_civic_virtue_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_second_amendment_text__originalist_civic_virtue_reading, []).

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
 *   constraint_id: second_amendment_text__originalist_civic_virtue_reading
 *   human_readable: Second Amendment: Originalist Civic Virtue Reading
 *   domain: constitutional/political
 *
 * SUMMARY:
 *   This constraint instantiates the originalist civic virtue reading of the
 *   Second Amendment: the founding-era militia is understood as universal
 *   armed citizenry, and the constitutional right protects their capacity to
 *   function as citizen-soldiers in service to the commonwealth. The reading
 *   grounds the right in the civic republican tradition, where distributed
 *   armed capacity is a structural check on tyranny and the backbone of
 *   collective defense. This is ONE READING of the contested kernel
 *   'second_amendment_text'. The other readings (collective-security and
 *   individual-right) instantiate different constraints with different ε
 *   values, beneficiary structures, and interpretive foundations. The
 *   claim/metric divergence is intentional: the reading is CLAIMED as rope
 *   (genuine coordination function, civic republican check on tyranny) while
 *   the metrics describe moderate extraction (0.38 at interval end) because
 *   the reading's interpretive authority constrains state regulatory power,
 *   which regulatory actors experience as deprivation. The engine measures
 *   this constraint from the originalist seat; the other readings are other
 *   constraints.
 *
 * KEY AGENTS:
 *   - armed_citizenry_as_political_body: the beneficiary; the political community whose capacity to organize armed self-defense is protected
 *   - founding_generation_interpreters: the agenda-setter; the framers and ratifiers whose intent the reading reconstructs
 *   - originalist_judicial_interpreters: agenda-setter and secondary beneficiary; courts and scholars who deploy the reading and benefit from its interpretive coherence
 *   - state_regulatory_apparatus: the payer; regulatory bodies whose police power is constrained by the reading
 *   - competing_constitutional_readers: excluded; advocates of alternative readings are structurally outside this framework
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(second_amendment_text__originalist_civic_virtue_reading, 0.38).
domain_priors:suppression_score(second_amendment_text__originalist_civic_virtue_reading, 0.22).
domain_priors:theater_ratio(second_amendment_text__originalist_civic_virtue_reading, 0.18).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(second_amendment_text__originalist_civic_virtue_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(second_amendment_text__originalist_civic_virtue_reading, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(second_amendment_text__originalist_civic_virtue_reading, theater_ratio, 0.18).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(second_amendment_text__originalist_civic_virtue_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(second_amendment_text__originalist_civic_virtue_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(second_amendment_text__originalist_civic_virtue_reading, rope).
narrative_ontology:human_readable(second_amendment_text__originalist_civic_virtue_reading, "Second Amendment: Originalist Civic Virtue Reading").
narrative_ontology:topic_domain(second_amendment_text__originalist_civic_virtue_reading, "constitutional/political").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(second_amendment_text__originalist_civic_virtue_reading, 'b14315b8-7598-4fe9-a734-cbb502413bb0').
narrative_ontology:cs_kernel_codification('b14315b8-7598-4fe9-a734-cbb502413bb0', fixed_text).
narrative_ontology:cs_authority_grounding('b14315b8-7598-4fe9-a734-cbb502413bb0', lineage).
narrative_ontology:cs_interpretation_layer_present('b14315b8-7598-4fe9-a734-cbb502413bb0').
narrative_ontology:cs_reading_relation('b14315b8-7598-4fe9-a734-cbb502413bb0', second_amendment_text__collective_security_reading, coexists_with).
narrative_ontology:cs_reading_relation('b14315b8-7598-4fe9-a734-cbb502413bb0', second_amendment_text__individual_right_reading, coexists_with).
narrative_ontology:cs_axiom('b14315b8-7598-4fe9-a734-cbb502413bb0', foundational, universal_armed_citizenry_principle).
narrative_ontology:cs_axiom_status(universal_armed_citizenry_principle, holdable).
narrative_ontology:cs_axiom_grounding('b14315b8-7598-4fe9-a734-cbb502413bb0', universal_armed_citizenry_principle, deontological).
narrative_ontology:cs_axiom('b14315b8-7598-4fe9-a734-cbb502413bb0', foundational, distributed_armed_capacity_structural_check_on_tyranny).
narrative_ontology:cs_axiom_status(distributed_armed_capacity_structural_check_on_tyranny, holdable).
narrative_ontology:cs_axiom_grounding('b14315b8-7598-4fe9-a734-cbb502413bb0', distributed_armed_capacity_structural_check_on_tyranny, instrumental).
narrative_ontology:cs_reference_frame('b14315b8-7598-4fe9-a734-cbb502413bb0', founding_era_civic_republican_militia).
narrative_ontology:cs_drift_state('b14315b8-7598-4fe9-a734-cbb502413bb0', contemporary_industrial_state, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('b14315b8-7598-4fe9-a734-cbb502413bb0', '').
narrative_ontology:cs_kernel_id(second_amendment_text__originalist_civic_virtue_reading, second_amendment_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(second_amendment_text__originalist_civic_virtue_reading, armed_citizenry_as_political_body).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(second_amendment_text__originalist_civic_virtue_reading, originalist_judicial_interpreters).
narrative_ontology:constraint_victim(second_amendment_text__originalist_civic_virtue_reading, state_regulatory_apparatus).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The reading's subject: a political community understood as capable of armed self-defense against tyranny and foreign invasion. The right protects their capacity to function as citizen-soldiers, organized into militia units when the commonwealth calls. The coordination benefit flows to the entire body politic — a check on governmental power and a distributed defense capacity embedded in the populace.
narrative_ontology:constraint_stakeholder(second_amendment_text__originalist_civic_virtue_reading, armed_citizenry_as_political_body, beneficiary,
    organized, generational, constrained, national).

% The constitutional framers and ratifying audiences who articulated the meaning of the Second Amendment in 1787–1791. This reading reconstructs their understanding: the right to arms is understood in the civic republican tradition, where an armed populace is a structural check on tyranny and the backbone of collective defense.
narrative_ontology:constraint_stakeholder(second_amendment_text__originalist_civic_virtue_reading, founding_generation_interpreters, agenda_setter,
    institutional, generational, analytical, national).

% Contemporary courts and scholars who adopt the originalist reading and defend it as the binding interpretation of the constitutional text. They benefit from the reading's coherence within their interpretive tradition and its power to adjudicate cases. They set the agenda for which evidence about founding intent is admissible and how that evidence is weighed.
narrative_ontology:constraint_stakeholder(second_amendment_text__originalist_civic_virtue_reading, originalist_judicial_interpreters, agenda_setter,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(second_amendment_text__originalist_civic_virtue_reading, originalist_judicial_interpreters, beneficiary).

% Advocates of the collective-security reading and the individual-right reading are structurally excluded from the originalist reading's framework. They would object that the originalist reading either misrepresents founding intent or privileges one strand of founding thought over others, but their objections are routed through competing readings of the same kernel, not through this reading's internal logic.
narrative_ontology:constraint_stakeholder(second_amendment_text__originalist_civic_virtue_reading, competing_constitutional_readers, excluded,
    institutional, generational, analytical, national).

% Regulatory bodies that attempt to restrict militia arms or curb armed organizing face the constraint: the originalist reading denies them the authority to disarm the citizen body in the name of collective security. The constraint limits their regulatory scope. They bear the cost of narrowed police power and reduced ability to enforce disarmament in the name of public safety.
narrative_ontology:constraint_stakeholder(second_amendment_text__originalist_civic_virtue_reading, state_regulatory_apparatus, payer,
    institutional, generational, constrained, national).

% Sees the full structure: the reading instantiates a constitutional commitment, disputed across three readings, each grounded in different interpretations of founding intent and constitutional purpose.
narrative_ontology:constraint_stakeholder(second_amendment_text__originalist_civic_virtue_reading, analytical_observer, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a political community to maintain armed self-defense capacity against tyranny and foreign invasion, distributed across the citizenry as universal militia principle. The right ensures no single authority monopolizes the means of armed force.
% TRANSFER_FUNCTION: No direct transfer. The reading establishes a structural arrangement: the right to bear arms is protected as a condition of civic participation, not as a mechanism of extraction or redistribution. What moves is authority: the reading constrains the state's power to disarm the citizen body.
% ABSENT_VOICES: Advocates of the collective-security reading and the individual-right reading are absent from this reading's internal framework. They would argue that the originalist reading misidentifies founding intent, conflates militia service with universal armament, or privileges civic virtue over individual liberty and security. Their objections are routed through competing readings, not absorbed into this one.
% DISAPPEARANCE_RATIONALE: If this reading's constitutional authority vanished and regulatory deference to state security arguments prevailed, the state's police power to restrict firearms would expand substantially. The political coordination function (armed citizenry as check on tyranny) would atrophy. Whether this constitutes a fundamental rearrangement or a return to the true constitutional baseline is exactly the contest between this reading and the others.
% FOUNDING_PROBLEM: How can a new republic guard against governmental tyranny and maintain capacity for collective defense without a standing army and without concentrating armed force in state hands? The founding solution, on this reading: distribute armed capacity across the citizen body and protect it constitutionally.
% FOUNDING_PROBLEM_CORROBORATION: Originalist scholars and some jurists (e.g., Justice Scalia in DC v. Heller, 2008) attest that the founding generation understood the right this way. Historians of the founding era (e.g., Saul Cornell on militia regulation, Joyce Lee Malcolm on English common law) provide corroboration outside the benefiting parties. However, competing historians (e.g., Michael Bellesiles on founding-era gun ownership rates) dispute both the founding intent and the factual premises. The status is contested because corroboration exists from credible external sources but so does credible counter-evidence.
narrative_ontology:disappearance_verdict(second_amendment_text__originalist_civic_virtue_reading, contested).
narrative_ontology:founding_problem_status(second_amendment_text__originalist_civic_virtue_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(second_amendment_text__originalist_civic_virtue_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(second_amendment_text__originalist_civic_virtue_reading, 'none', 1).
narrative_ontology:epsilon_provenance(second_amendment_text__originalist_civic_virtue_reading, 0.38, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(second_amendment_text__originalist_civic_virtue_reading_tests).
:- end_tests(second_amendment_text__originalist_civic_virtue_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.38) because the reading does NOT extract rents or resources but DOES constrain regulatory authority. The constraint's persistence rests on an authority structure (the Constitution, interpreted originally) and on the resistance of the armed citizenry and originalist judges. Suppression is low (0.22) because the reading does not depend on coercion — it depends on legitimate interpretive authority and on widespread gun ownership that already exists. Theater ratio is low (0.18) because the civic virtue function (distributed armed capacity as structural check) is structurally real, not performative: the citizenry's ability to organize armed defense has material consequences. The measurement series tracks increasing extractiveness over 235 years (from t=0, the founding, to t=235, the present) because the reading's regulatory constraints have intensified as state capacity for weapons regulation expanded and as competing readings gained salience. Suppression requirement increases modestly because maintaining the constraint's authority in the face of competing readings requires ongoing interpretive labor. Theater ratio remains low throughout: the reading's claim to represent founding intent is either sincere or systematically false, but not a theatrical performance — sincere substantive debate does not show high theater.
 *
 * PERSPECTIVAL GAP:
 *   The originalist judicial seat and the armed citizenry seat should compute favorably toward the reading (low extraction, genuine coordination); the state regulatory seat should compute it as extractive deprivation (constrained police power). The founding-generation seat is analytical: it sees the reading as an attempt to reconstruct their intent accurately. The competing-reader seats see the reading as foreclosed by alternative interpretations. The engine computes per-seat classifications; the authored metrics describe the reading's overall operation from the originalist analytic frame.
 *
 * DIRECTIONALITY LOGIC:
 *   The armed citizenry is the structural beneficiary: the right protects their capacity to organize armed self-defense, which they value both instrumentally (defense against tyranny) and as a marker of civic status. No extraction flows FROM them; the constraint protects them. The state regulatory apparatus bears the cost: the constraint narrows their regulatory scope and their ability to enforce disarmament in the name of collective security. Originalist judges benefit incidentally (coherent interpretive tradition) but are principally agenda-setters, reconstructing founding intent. Competing readers are excluded from the framework entirely — they operate in different constraint stories with different readings.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (distributed armed capacity as check on tyranny) remains contested. The originalist reading attests it is LIVE: the structure it describes (universal militia, armed citizenry) remains strategically relevant to political philosophy and constitutional law. Regulatory authorities attest it is DEAD or solved: modern state capacity, standing armies, and professional law enforcement have superseded the need for armed populace self-defense. This mismatch between founding_problem_status=contested and disappearance_verdict=contested prevents false mandatrophy classification: the reading does not insist its founding problem is solved while persisting zombie-like. The contest is real and structural, not inertial.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    founding_militia_composition,
    'Did the founding generation understand ''militia'' as the entire armed male citizenry (universal militia principle) or as an organized state-controlled force (collective security principle)?',
    'Systematic analysis of founding-era militia law, correspondence, and political theory (Federalist Papers, state militia statutes, common-law treatises). Cross-check against primary sources from multiple founding-generation voices, not just those selected by either side of the contemporary reading dispute.',
    'If universal militia is the accurate reconstruction, the originalist civic virtue reading''s core premise stands. If organized state-militia is the accurate understanding, the collective-security reading gains structural purchase and the civic virtue reading must concede misidentification of founding intent.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(founding_militia_composition, empirical, 'Whether founding-era militia was understood as universal or state-organized').

omega_variable(
    civic_virtue_vs_individual_right_dichotomy,
    'Is the founding-era right fundamentally grounded in civic virtue and civic participation (militia service, distributed defense), or does the founding record support BOTH a civic function AND an individual liberty right independent of civic service?',
    'Close textual and historical analysis of founding-generation arguments for the right: do they enumerate only civic benefits, or do they also protect personal self-defense and defense of property independent of militia duty? Analysis of English common law precedents cited by founders.',
    'If the founding record supports BOTH readings equally, then this reading and the individual-right reading are coequal interpretations of founding intent, not competitors. If the civic virtue reading dominates the record, this reading is the most faithful reconstruction. If individual-right arguments dominate, this reading misattributes the primary grounding of the right.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(civic_virtue_vs_individual_right_dichotomy, empirical, 'Whether founding intent emphasized civic virtue or individual liberty as the primary grounding').

omega_variable(
    reading_interpretation_authority_grounding,
    'Does the originalist reading''s authority rest on the historical accuracy of its reconstruction of founding intent, or on the legitimacy of originalism as an interpretive method, or on some combination? If accuracy is the ground, what happens to the reading''s authority if a more accurate reconstruction points toward a different conclusion?',
    'Clarify the reading''s own epistemic standards: what would falsify it? If new historical evidence emerged that the founding generation understood the right differently, would originalist interpreters update, or would they defend the reading on methodological grounds (originalism is the legitimate interpretive frame, regardless of what the founders actually thought)?',
    'If the reading is grounded in historical accuracy, strong omega across founding_militia_composition and civic_virtue_vs_individual_right_dichotomy. If the reading is grounded in methodological legitimacy, those omegas are less consequential — the reading stands because originalism is the correct interpretive method, not because this particular historical reconstruction is definitively true.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_interpretation_authority_grounding, conceptual, 'Whether the reading''s authority is grounded in historical accuracy or methodological legitimacy').

omega_variable(
    suppression_mechanism_internalization,
    'Is the suppression measured here structural (external legal barriers to the reading''s acceptance) or internalized (the reading has become part of how some communities understand themselves, making exit from the reading frame psychologically difficult)? Or both, in what proportion?',
    'Post-suppression analysis: if the legal barriers to alternative readings were removed (e.g., if the Supreme Court formally endorsed a collective-security reading), would originalist interpreters remain committed to the originalist reading, or would the reading''s hold over them dissolve? Measure the ratio of principled commitment vs. structural dependency.',
    'If internalized, the reading is more resilient to legal/institutional pressure than the structural suppression measure suggests. If primarily structural, the reading could be displaced by shifted legal authority. A mixed answer refines the extraction calculation — internalized suppression is more costly for targets to escape.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalization, empirical, 'Whether suppression of competing readings is structural or internalized').

omega_variable(
    kernel_contest_foreclosure,
    'Do any two of these three readings (civic virtue, collective security, individual right) logically foreclose each other — i.e., can they coexist within a single coherent constitutional framework, or does accepting one require rejecting another as incoherent?',
    'Logical analysis: construct a single constitutional framework that accepts the civic virtue reading''s core premise (universal militia, citizen-soldier function) alongside the individual-right reading''s core premise (personal right independent of militia service). Can they both be true? If yes, they coexist. If no, identify which premise rules out the other.',
    'If they coexist, the kernel contest is fundamentally political (different parties hold different readings) and none is structurally privileged. If one forecloses another, the engine''s reading_relations assessment should reflect that, and the constrained pair shapes the constitutional space available for the third reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_contest_foreclosure, conceptual, 'Whether readings of the Second Amendment kernel logically foreclose each other').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(second_amendment_text__originalist_civic_virtue_reading, 0, 235).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(seco_tr_t0, second_amendment_text__originalist_civic_virtue_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement(seco_tr_t47, second_amendment_text__originalist_civic_virtue_reading, theater_ratio, 47, 0.1).
narrative_ontology:measurement(seco_tr_t94, second_amendment_text__originalist_civic_virtue_reading, theater_ratio, 94, 0.13).
narrative_ontology:measurement(seco_tr_t141, second_amendment_text__originalist_civic_virtue_reading, theater_ratio, 141, 0.16).
narrative_ontology:measurement(seco_tr_t188, second_amendment_text__originalist_civic_virtue_reading, theater_ratio, 188, 0.17).
narrative_ontology:measurement(seco_tr_t235, second_amendment_text__originalist_civic_virtue_reading, theater_ratio, 235, 0.18).

% Extraction over time
narrative_ontology:measurement(seco_be_t0, second_amendment_text__originalist_civic_virtue_reading, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(seco_be_t47, second_amendment_text__originalist_civic_virtue_reading, base_extractiveness, 47, 0.22).
narrative_ontology:measurement(seco_be_t94, second_amendment_text__originalist_civic_virtue_reading, base_extractiveness, 94, 0.28).
narrative_ontology:measurement(seco_be_t141, second_amendment_text__originalist_civic_virtue_reading, base_extractiveness, 141, 0.35).
narrative_ontology:measurement(seco_be_t188, second_amendment_text__originalist_civic_virtue_reading, base_extractiveness, 188, 0.37).
narrative_ontology:measurement(seco_be_t235, second_amendment_text__originalist_civic_virtue_reading, base_extractiveness, 235, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(seco_su_t0, second_amendment_text__originalist_civic_virtue_reading, suppression_requirement, 0, 0.12).
narrative_ontology:measurement(seco_su_t47, second_amendment_text__originalist_civic_virtue_reading, suppression_requirement, 47, 0.15).
narrative_ontology:measurement(seco_su_t94, second_amendment_text__originalist_civic_virtue_reading, suppression_requirement, 94, 0.18).
narrative_ontology:measurement(seco_su_t141, second_amendment_text__originalist_civic_virtue_reading, suppression_requirement, 141, 0.21).
narrative_ontology:measurement(seco_su_t188, second_amendment_text__originalist_civic_virtue_reading, suppression_requirement, 188, 0.22).
narrative_ontology:measurement(seco_su_t235, second_amendment_text__originalist_civic_virtue_reading, suppression_requirement, 235, 0.22).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(second_amendment_text__originalist_civic_virtue_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(second_amendment_text__originalist_civic_virtue_reading, 0.12).
narrative_ontology:affects_constraint(second_amendment_text__originalist_civic_virtue_reading, second_amendment_text__collective_security_reading).
narrative_ontology:affects_constraint(second_amendment_text__originalist_civic_virtue_reading, second_amendment_text__individual_right_reading).

% DUAL FORMULATION NOTE:
% The second_amendment_text kernel decomposes into three constraint stories, one per reading. All three share the same foundational constitutional text and authority structure but instantiate different constraints because they attribute different meanings to 'militia' and different grounds to 'the right.' The originalist civic virtue reading (this story) emphasizes founding-era political theory and universal armed citizenry as a structural check on tyranny. The collective-security reading emphasizes the militia clause as conditioning the right on organized state defense. The individual-right reading emphasizes personal self-defense independent of militia service. Each reading carries its own ε, beneficiary structure, and type classification. They are linked via network.affects_constraints because shifts in judicial doctrine or historical scholarship that privilege one reading weaken the authority of the others.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
