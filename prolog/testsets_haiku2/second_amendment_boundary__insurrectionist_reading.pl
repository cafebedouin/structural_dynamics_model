% ============================================================================
% CONSTRAINT STORY: second_amendment_boundary__insurrectionist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_second_amendment_boundary__insurrectionist_reading, []).

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
 *   constraint_id: second_amendment_boundary__insurrectionist_reading
 *   human_readable: Second Amendment Insurrectionist Reading: Armed Resistance as Tyranny Check
 *   domain: constitutional/political/security
 *
 * SUMMARY:
 *   The insurrectionist reading of the Second Amendment treats the
 *   constitutional right as instrumental to armed resistance against
 *   tyrannical government. Under this reading, individual possession of
 *   military-grade arms is protected because the Founders intended to
 *   preserve the people's capacity to overthrow an illegitimate state. The
 *   constraint declares a right; the reading specifies its purpose (tyranny
 *   prevention) and thereby its scope (protection extends to weapons capable
 *   of meaningful armed resistance). This reading is one interpretation of a
 *   contested constitutional kernel; the sibling readings (individual-right
 *   and militia-conditioned) propose different purposes and scopes. The
 *   insurrectionist reading produces a tangled rope structure: it coordinates
 *   a deterrent narrative (state is deterred from tyranny by armed citizenry)
 *   while extracting from the security apparatus (constrained regulatory
 *   capacity, enforcement burden) and from unprotected civilians (who bear
 *   the externality of armed conflict risk).
 *
 * KEY AGENTS:
 *   - Armed citizens and militia organizations (insurrectionist framing): claim constitutionally protected deterrent legitimacy; benefit from broad protection of military-grade arms; identity-locked to insurrectionist interpretation.
 *   - State security apparatus (law enforcement, military): constrained by constitutional protection claims; bear burden of managing civilian arms proliferation; lose regulatory authority under the insurrectionist reading.
 *   - Unprotected civilians and gun-violence victims: trapped in the jurisdiction; bear externality of armed violence risk; excluded from high-level constitutional interpretation.
 *   - Constitutional courts: set the enforcement baseline by interpreting the Second Amendment; adjudicate the boundaries of protected arms; agenda-setter for the constraint's scope.
 *   - State legislatures: attempt regulation but face constitutional challenges; constrained by whatever scope courts grant to the insurrectionist reading.
 *   - Rival constitutional readings and their advocates: marginalized in institutional forums; excluded from the dominant constitutional framing but not entirely absent from discourse.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(second_amendment_boundary__insurrectionist_reading, 0.68).
domain_priors:suppression_score(second_amendment_boundary__insurrectionist_reading, 0.72).
domain_priors:theater_ratio(second_amendment_boundary__insurrectionist_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(second_amendment_boundary__insurrectionist_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(second_amendment_boundary__insurrectionist_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(second_amendment_boundary__insurrectionist_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(second_amendment_boundary__insurrectionist_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(second_amendment_boundary__insurrectionist_reading, resistance, 0.78).

% --- Constraint claim ---
narrative_ontology:constraint_claim(second_amendment_boundary__insurrectionist_reading, tangled_rope).
narrative_ontology:human_readable(second_amendment_boundary__insurrectionist_reading, "Second Amendment Insurrectionist Reading: Armed Resistance as Tyranny Check").
narrative_ontology:topic_domain(second_amendment_boundary__insurrectionist_reading, "constitutional/political/security").

domain_priors:requires_active_enforcement(second_amendment_boundary__insurrectionist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(second_amendment_boundary__insurrectionist_reading, 'c62380ca-9fdd-4463-a7c3-aaad558e501d').
narrative_ontology:cs_kernel_codification('c62380ca-9fdd-4463-a7c3-aaad558e501d', fixed_text).
narrative_ontology:cs_authority_grounding('c62380ca-9fdd-4463-a7c3-aaad558e501d', lineage).
narrative_ontology:cs_interpretation_layer_present('c62380ca-9fdd-4463-a7c3-aaad558e501d').
narrative_ontology:cs_reading_relation('c62380ca-9fdd-4463-a7c3-aaad558e501d', second_amendment_boundary__individual_right_reading, influences).
narrative_ontology:cs_reading_relation('c62380ca-9fdd-4463-a7c3-aaad558e501d', second_amendment_boundary__militia_conditioned_reading, coexists_with).
narrative_ontology:cs_axiom('c62380ca-9fdd-4463-a7c3-aaad558e501d', foundational, armed_resistance_essential_tyranny_prevention).
narrative_ontology:cs_axiom_status(armed_resistance_essential_tyranny_prevention, holdable).
narrative_ontology:cs_axiom_grounding('c62380ca-9fdd-4463-a7c3-aaad558e501d', armed_resistance_essential_tyranny_prevention, deontological).
narrative_ontology:cs_axiom('c62380ca-9fdd-4463-a7c3-aaad558e501d', foundational, individual_military_grade_arms_instrumental_to_resistance).
narrative_ontology:cs_axiom_status(individual_military_grade_arms_instrumental_to_resistance, holdable).
narrative_ontology:cs_axiom_grounding('c62380ca-9fdd-4463-a7c3-aaad558e501d', individual_military_grade_arms_instrumental_to_resistance, instrumental).
narrative_ontology:cs_axiom('c62380ca-9fdd-4463-a7c3-aaad558e501d', secondary, state_disarmament_efforts_are_tyranny_precursors).
narrative_ontology:cs_axiom_status(state_disarmament_efforts_are_tyranny_precursors, holdable).
narrative_ontology:cs_axiom_grounding('c62380ca-9fdd-4463-a7c3-aaad558e501d', state_disarmament_efforts_are_tyranny_precursors, empirically_contingent).
narrative_ontology:cs_reference_frame('c62380ca-9fdd-4463-a7c3-aaad558e501d', armed_citizenry_as_structural_tyranny_check).
narrative_ontology:cs_drift_state('c62380ca-9fdd-4463-a7c3-aaad558e501d', contemporary_firearms_regulation_expansion, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('c62380ca-9fdd-4463-a7c3-aaad558e501d', '').
narrative_ontology:cs_kernel_id(second_amendment_boundary__insurrectionist_reading, second_amendment_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(second_amendment_boundary__insurrectionist_reading, armed_citizens_insurrectionist_framing).
narrative_ontology:constraint_victim(second_amendment_boundary__insurrectionist_reading, state_security_apparatus).
narrative_ontology:constraint_victim(second_amendment_boundary__insurrectionist_reading, civilians_in_armed_conflict).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Individuals and militia organizations hold that the constitutional right protects their capacity to possess military-grade arms as a deterrent against government tyranny. They frame armed resistance as a structural check on state power and claim that disarmament efforts are precursors to tyranny. The right is understood as enabling potential overthrow of an illegitimate government. This reading confers deterrent legitimacy on their possession and mobilization.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__insurrectionist_reading, armed_citizens_insurrectionist_framing, beneficiary,
    organized, generational, identity_locked, national).

% Law enforcement and military agencies must contend with a civilian population armed with military-grade weaponry and oriented toward potential armed resistance. The insurrectionist reading treats state disarmament efforts (regulation, registration, confiscation) as tyranny precursors, making enforcement infrastructure costly and politically contentious. The security apparatus bears the operational burden of managing civilian arms proliferation while constrained by constitutional protection claims.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__insurrectionist_reading, state_security_apparatus, payer,
    institutional, generational, trapped, national).

% The non-partisan public bears the external costs of the insurrectionist framework: casualties in any hypothetical armed conflict between armed citizens and state authority, plus ongoing risk of armed violence from actors operating under insurrectionist legitimation. They cannot exit the geographic or political domain where this constraint operates, and they did not choose its interpretation.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__insurrectionist_reading, civilians_in_armed_conflict, payer,
    powerless, biographical, trapped, national).

% The judiciary must interpret the constitutional text and adjudicate whether the insurrectionist reading is the correct reading of the Second Amendment. Courts set the enforcement baseline by deciding what arms are protected, what regulations are permissible, and whether the tyranny-check framing is a valid constitutional purpose. Their rulings determine the constraint's legal and operational scope.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__insurrectionist_reading, constitutional_court, agenda_setter,
    institutional, generational, analytical, national).

% Legislatures attempt to regulate arms proliferation (background checks, licensing, exclusions of military-grade arms) but face constitutional challenges grounded in the insurrectionist reading. Under this reading's logic, the state's regulatory ambitions are suspicious as tyranny precursors; legislatures are constrained by whatever scope the courts grant to the insurrectionist interpretation.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__insurrectionist_reading, state_legislative_authority, agenda_setter,
    institutional, generational, constrained, national).

% Advocates of the militia-conditioned reading and individual-right-non-insurrectionist readings argue for narrower protection and broader regulatory scope. They are not structurally barred from the discourse, but the insurrectionist reading's institutional adoption (via certain court rulings and legislative resistance) marginalizes their alternative framings in high-stakes security contexts.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__insurrectionist_reading, rival_constitutional_readings, excluded,
    organized, generational, constrained, national).

% Gun-violence researchers, families affected by mass shootings, and public-health advocates document casualties and argue that the insurrectionist reading's protection of military-grade arms proliferation externalizes harm onto the powerless civilian population. They observe the constraint's operation without institutional power to change it directly.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__insurrectionist_reading, victim_advocacy_coalitions, observer,
    organized, biographical, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(second_amendment_boundary__insurrectionist_reading, armed_citizens_insurrectionist_framing).
narrative_ontology:fixing_cost_class(second_amendment_boundary__insurrectionist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The constraint coordinates a deterrent narrative: if citizens retain military-grade arms, the argument goes, the state is structurally deterred from tyranny (potential armed resistance raises the cost of tyrannical action to unsustainable levels). This is a coordinating function of a specific kind—it aligns individual possession with a collective security goal (state accountability). However, the coordination is asymmetric: the beneficiary seats (armed citizens) claim collective benefit, while the cost-bearing seats (security apparatus, unprotected civilians) do not agree that they are coordinated toward any shared safety goal.
% TRANSFER_FUNCTION: The constraint moves legitimacy and access to military-grade arms from the state (as the sole or dominant bearer of such weaponry) to armed citizens. It also transfers risk: the possibility of armed conflict between citizens and state, and the externality of civilian casualties in such conflict. The transfer is presented by the insurrectionist reading as necessary for tyranny prevention, but experienced by the payer seats as forced exposure to armed violence and constrained regulatory capacity.
% ABSENT_VOICES: Legislative majorities in certain jurisdictions favoring comprehensive gun regulation are not entirely absent but are structurally marginalized by the insurrectionist reading's constitutional claims. Public health and safety advocates (non-armed citizens) are largely unheard in high-constitutional-interpretation forums. International comparative perspectives (other democracies with stricter arms control and lower gun violence) are excluded from U.S. constitutional discourse. Victims of gun violence have no structural seat in the constraint's legitimation (they are discussed but do not participate in its authorization).
% DISAPPEARANCE_RATIONALE: If the insurrectionist reading disappeared overnight and were replaced by the militia-conditioned reading or a purely individual-rights non-insurrectionist reading, state regulatory capacity would expand, military-grade arms would become subject to licensing/registration/exclusion regimes, and the armed deterrent against tyranny would cease to be a constitutionally protected justification for possession. The civilian arms landscape would reorganize significantly; the security apparatus's enforcement burden would shift; the risk calculus for armed conflict would change.
% FOUNDING_PROBLEM: The founding problem, from the insurrectionist perspective, is the existential risk that a government can become tyrannical and that citizens require armed capacity to resist or overthrow tyranny. The Second Amendment is read as insurance: a structural guarantee that the state cannot disarm the population and thus cannot become absolutely authoritarian. The problem is stated as timeless and recurring—any government is a potential tyranny absent the armed check.
% FOUNDING_PROBLEM_CORROBORATION: The insurrectionist reading is corroborated by certain Founders' writings (Jefferson on the tree of liberty, Madison on militia as check on standing armies) and by libertarian political theory. However, historical scholarship (from sources outside the insurrectionist beneficiary set) contests whether the Founders intended the Second Amendment to protect an insurrectionist right, or whether they primarily understood it as a militia-conditional clause. Constitutional scholars aligned with the militia reading argue the founding problem (armed resistance to government) was addressed via the whole constitutional structure (separation of powers, checks and balances, electoral accountability), not via an individual insurrectionist right. No independent, disinterested institutional voice corroborates the insurrectionist reading as THE correct historical interpretation; the corroboration comes from within the reading's own political constituency.
narrative_ontology:disappearance_verdict(second_amendment_boundary__insurrectionist_reading, world_rearranges).
narrative_ontology:founding_problem_status(second_amendment_boundary__insurrectionist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(second_amendment_boundary__insurrectionist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(second_amendment_boundary__insurrectionist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(second_amendment_boundary__insurrectionist_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(second_amendment_boundary__insurrectionist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(second_amendment_boundary__insurrectionist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(second_amendment_boundary__insurrectionist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is measured at 0.68 (endpoint) because the insurrectionist reading confers broad protection on military-grade arms and delegitimizes state regulatory efforts as tyranny precursors—effectively extracting regulatory authority from the state and transferring legitimacy (not arms themselves) to the armed-citizens beneficiary set. The constraint is fundamentally asymmetric: the coordinating story (deterrence against tyranny) is claimed by one set (armed citizens) as a genuine public good, but the cost-bearing sets (security apparatus, unprotected civilians) experience it as imposed risk without reciprocal benefit. Suppression is high (0.72) because maintaining the insurrectionist reading requires suppressing or delegitimizing alternative interpretations (militia-conditioned, individual-right-non-insurrectionist) in constitutional discourse—rival readings are not silenced but are institutionally marginalized. Theater is moderate (0.41) because the tyranny-check function is genuinely claimed and mobilizing, but a growing share of enforcement activity (increasingly strict arms regulations at state level, registration schemes, exclusions) represents resistance to the insurrectionist reading rather than its smooth operation. The measurement series show extractiveness and suppression rising over the interval, reflecting intensifying constitutional conflict and tightening regulatory attempts.
 *
 * PERSPECTIVAL GAP:
 *   From the insurrectionist framing, the constraint is coordination: citizens are armed because the state needs to be deterred from tyranny, and this is a shared security goal. From the state security apparatus's perspective, the constraint is pure imposition: they lose regulatory capacity and gain enforcement burden. From unprotected civilians' perspective, it is extraction: they are exposed to risk without choice and excluded from the constitutional interpretation that imposed it. The engine computes these divergent types (rope-from-insurrectionist-beneficiary-perspective, snare-from-security-apparatus-and-civilian-perspectives) from the directionality data; the authored claim (tangled_rope) reflects the true structure: coordination for one set, extraction for another, held together by active enforcement (constitutional court rulings and constitutional resistance to state regulation).
 *
 * DIRECTIONALITY LOGIC:
 *   The armed citizens (beneficiary) sit near the beneficiary pole of directionality: they collect deterrent legitimacy and broad protection without running the constraint—the courts run it. Their exit options are identity-locked (their identity is constituted through insurrectionist interpretation; exit would mean abandoning the reading). The state security apparatus (institutional payer) sits near the target pole: they bear regulatory constraint and operational burden; their exit is structurally trapped (they cannot unilaterally reinterpret the constitution). Unprotected civilians (powerless payers) sit at the extreme target pole: fully trapped, bearing externality, without voice in the constitutional interpretation. The constitutional court (agenda-setter) sits analytically: they set the rule but do not benefit or pay from it. The directionality divergence between beneficiary and payer seats is structurally deep: the same text (the Second Amendment) is read by one seat as their protection and by another as their constraint.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem—government tyranny and the need for armed resistance—remains contested rather than resolved. The insurrectionist reading claims the problem is timeless and ongoing (any government is a potential tyranny). The militia-conditioned and individual-right readings dispute this, arguing the founding problem was addressed by constitutional structure (separation of powers, elections) and militia systems (national guard), not by an insurrectionist individual right. There is no mandatrophy here in the strict sense (where the founding function atrophied but the constraint persisted theatrically), but there is a perpetual foundational contest: the constraint's legitimation depends on the contested claim that armed citizens are necessary to prevent tyranny. If that claim loses institutional credibility (e.g., through constitutional amendment or court reinterpretation), the constraint's justification collapses even if the right remains formally enshrined. The resistance measurement (0.78 at interval baseline) reflects this contest: the insurrectionist reading meets substantial, organized resistance from rival readings, from security apparatus enforcement attempts, and from public health advocacy.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    insurrectionist_reading_vs_militia_conditioned_logical_status,
    'Does the insurrectionist reading''s claim that individual possession is instrumental to tyranny-prevention logically foreclose the militia-conditioned reading''s claim that the right is bounded by collective defense, or do both readings coexist as live interpretations of the contested kernel?',
    'Historical and textual analysis of Founders'' writings and constitutional structure. If the Founders intended an insurrectionist individual right, the militia-conditioned reading is foreclosed. If the Founders intended militia-conditioned protection and separate mechanisms for tyranny prevention (electoral, structural), the insurrectionist reading is a later interpretive innovation that coexists with the original but is not foreclosed by it.',
    'If foreclosed: the insurrectionist reading is the sole logically defensible reading, and the militia-conditioned reading is an error. If coexists: both are live interpretations, and the choice between them is institutional/political, not textual. This affects the cs_structure.reading_relations classification and the omega_c_certainty calculus.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(insurrectionist_reading_vs_militia_conditioned_logical_status, conceptual, 'Whether the insurrectionist reading forecloses the militia-conditioned reading or coexists with it.').

omega_variable(
    deterrent_efficacy_empirical_claim,
    'Does armed civilian possession of military-grade weapons actually deter government tyranny, or is this a normative claim without empirical grounding?',
    'Comparative analysis across democracies: do nations with stricter arms control experience higher rates of tyranny or democratic erosion? Does the presence of an armed citizenry correlate with lower tyranny risk? Historical case studies of armed resistance and disarmament sequences. The insurrectionist reading rests on an empirical premise (deterrence works) that can be tested.',
    'If deterrence is empirically unsupported, the insurrectionist reading''s functional legitimacy collapses, though the normative claim (people should have the right) might persist independently. This would degrade the coordination_function claim and strengthen the extraction characterization. If deterrence is empirically supported, the coordination_function claim is strengthened and the tangled_rope classification is reinforced (real coordination offset by asymmetric extraction).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(deterrent_efficacy_empirical_claim, empirical, 'Whether the deterrent-against-tyranny claim is empirically supported or purely normative.').

omega_variable(
    victim_set_externality_accounting,
    'Are civilians not aligned with the insurrectionist framing actual victims of the constraint, or are they merely cost-bearers in a shared security arrangement they contest?',
    'Post-exit measurement: if civilians were able to opt out of the constraint''s jurisdiction (e.g., by relocating to a jurisdiction with militia-conditioned regulation), would they do so at cost? Would their welfare improve? Polling and revealed-preference data. The distinction between victim and contested-cost-bearer affects whether the constraint''s extraction is from the unprotected set or shared asymmetrically.',
    'If civilians are victims (they would exit if able, welfare is harmed), the extraction is clearer and the payer role is justified. If civilians are contested-cost-bearers (they remain for other reasons, welfare calculus is ambiguous), the extraction is murkier and the constraint might compute as rope from some perspectives. This affects the six_questions.transfer_function characterization and the effective-extraction calculation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(victim_set_externality_accounting, empirical, 'Whether unprotected civilians are structural victims or contested-cost-bearers.').

omega_variable(
    reading_kernel_contention_vs_constraint_identity,
    'Is the insurrectionist reading truly a reading of the same constitutional kernel (Second Amendment) as the militia-conditioned reading, or is the interpretive disagreement so fundamental that the readings are about different normative claims entirely (individual right vs. collective regulation)?',
    'Axiomatic analysis: do both readings accept the Second Amendment text as binding? Do both frame their interpretation as answering ''what does the text mean?'' or does one reading reject the text''s authority and propose a different source (natural law, founding intent outside the text)? If both accept the text''s authority, they are readings of the same kernel; if one rejects it, they may be incommensurable.',
    'If readings are incommensurable, the network structure is not a kernel family but a logical cascade: the insurrectionist reading may not coexist with but may foreclose the militia-conditioned reading. If readings are both hermeneutical (text-accepting, interpretation-diverging), coexistence is the correct relation. This affects cs_structure.reading_relations and the constraint''s logical positioning relative to siblings.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_kernel_contention_vs_constraint_identity, conceptual, 'Whether the insurrectionist reading is a true reading of the Second Amendment kernel or a fundamentally different normative claim.').

omega_variable(
    suppression_mechanism_structural_vs_internalized,
    'Is the measured suppression (0.72) of rival constitutional readings structurally enforced (courts systematically reject militia-conditioned arguments, institutional forums exclude them) or internalized (advocates of rival readings have internalized doubt in their own positions)?',
    'Post-reinterpretation trajectory: if a court were to adopt the militia-conditioned reading, would advocates of the insurrectionist reading immediately cease their efforts (structural suppression removed), or would they continue mobilizing (internalized suppression, identity-locked commitment)? Discourse analysis of how rival readings are handled in constitutional law journals and policy forums.',
    'If suppression is structural, changing courts would change the constraint''s operation quickly. If suppression is partially internalized (insurrectionist advocates are identity-fused to their reading), the constraint would persist even if institutional support shifted. This affects the trajectory modeling for the constraint and the identity_locked exit option assessment for armed citizens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_vs_internalized, empirical, 'Whether suppression of rival readings is institutional/structural or internalized/identity-based.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(second_amendment_boundary__insurrectionist_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(seco_tr_t0, second_amendment_boundary__insurrectionist_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement_basis(seco_tr_t0, observed).
narrative_ontology:measurement(seco_tr_t5, second_amendment_boundary__insurrectionist_reading, theater_ratio, 5, 0.28).
narrative_ontology:measurement_basis(seco_tr_t5, observed).
narrative_ontology:measurement(seco_tr_t10, second_amendment_boundary__insurrectionist_reading, theater_ratio, 10, 0.32).
narrative_ontology:measurement_basis(seco_tr_t10, observed).
narrative_ontology:measurement(seco_tr_t15, second_amendment_boundary__insurrectionist_reading, theater_ratio, 15, 0.36).
narrative_ontology:measurement_basis(seco_tr_t15, observed).
narrative_ontology:measurement(seco_tr_t25, second_amendment_boundary__insurrectionist_reading, theater_ratio, 25, 0.39).
narrative_ontology:measurement_basis(seco_tr_t25, observed).
narrative_ontology:measurement(seco_tr_t40, second_amendment_boundary__insurrectionist_reading, theater_ratio, 40, 0.41).
narrative_ontology:measurement_basis(seco_tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(seco_be_t0, second_amendment_boundary__insurrectionist_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement_basis(seco_be_t0, observed).
narrative_ontology:measurement(seco_be_t5, second_amendment_boundary__insurrectionist_reading, base_extractiveness, 5, 0.52).
narrative_ontology:measurement_basis(seco_be_t5, observed).
narrative_ontology:measurement(seco_be_t10, second_amendment_boundary__insurrectionist_reading, base_extractiveness, 10, 0.57).
narrative_ontology:measurement_basis(seco_be_t10, observed).
narrative_ontology:measurement(seco_be_t15, second_amendment_boundary__insurrectionist_reading, base_extractiveness, 15, 0.61).
narrative_ontology:measurement_basis(seco_be_t15, observed).
narrative_ontology:measurement(seco_be_t25, second_amendment_boundary__insurrectionist_reading, base_extractiveness, 25, 0.66).
narrative_ontology:measurement_basis(seco_be_t25, observed).
narrative_ontology:measurement(seco_be_t40, second_amendment_boundary__insurrectionist_reading, base_extractiveness, 40, 0.68).
narrative_ontology:measurement_basis(seco_be_t40, observed).

% Suppression requirement over time
narrative_ontology:measurement(seco_su_t0, second_amendment_boundary__insurrectionist_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement_basis(seco_su_t0, observed).
narrative_ontology:measurement(seco_su_t5, second_amendment_boundary__insurrectionist_reading, suppression_requirement, 5, 0.59).
narrative_ontology:measurement_basis(seco_su_t5, observed).
narrative_ontology:measurement(seco_su_t10, second_amendment_boundary__insurrectionist_reading, suppression_requirement, 10, 0.64).
narrative_ontology:measurement_basis(seco_su_t10, observed).
narrative_ontology:measurement(seco_su_t15, second_amendment_boundary__insurrectionist_reading, suppression_requirement, 15, 0.68).
narrative_ontology:measurement_basis(seco_su_t15, observed).
narrative_ontology:measurement(seco_su_t25, second_amendment_boundary__insurrectionist_reading, suppression_requirement, 25, 0.7).
narrative_ontology:measurement_basis(seco_su_t25, observed).
narrative_ontology:measurement(seco_su_t40, second_amendment_boundary__insurrectionist_reading, suppression_requirement, 40, 0.72).
narrative_ontology:measurement_basis(seco_su_t40, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(second_amendment_boundary__insurrectionist_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(second_amendment_boundary__insurrectionist_reading, 0.12).
narrative_ontology:affects_constraint(second_amendment_boundary__insurrectionist_reading, second_amendment_boundary__individual_right_reading).
narrative_ontology:affects_constraint(second_amendment_boundary__insurrectionist_reading, second_amendment_boundary__militia_conditioned_reading).
narrative_ontology:affects_constraint(second_amendment_boundary__insurrectionist_reading, state_firearms_regulation_capacity).
narrative_ontology:affects_constraint(second_amendment_boundary__insurrectionist_reading, insurrectionary_mobilization_legitimacy).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the second_amendment_boundary kernel. The insurrectionist reading, the individual-right reading, and the militia-conditioned reading are structurally distinct constraints with different ε values, different victim sets, and different classifications. They are linked via network.affects_constraints because they compete for institutional adoption in constitutional interpretation. Each reading has its own constraint file; this file instantiates only the insurrectionist reading. Do not conflate the readings in a single story; the constitutional divergence is the point.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(second_amendment_boundary__insurrectionist_reading, powerless, 0.95).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
