% ============================================================================
% CONSTRAINT STORY: second_amendment_arms_right__collective_right_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_second_amendment_arms_right__collective_right_reading, []).

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
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: second_amendment_arms_right__collective_right_reading
 *   human_readable: Second Amendment: Collective Right Reading (State Militia Authority)
 *   domain: constitutional_law/political_philosophy
 *
 * SUMMARY:
 *   The collective-right reading of the Second Amendment interprets the
 *   constitutional text as protecting state governments' authority to
 *   maintain militia forces, not as protecting an individual's right to keep
 *   and bear arms outside the organized militia context. Under this reading,
 *   the prefatory clause ('A well regulated Militia, being necessary to the
 *   security of a free State') limits the operative clause ('the right of the
 *   people to keep and bear Arms, shall not be infringed') to militia-related
 *   contexts. Individual possession of firearms can be comprehensively
 *   regulated by states and the federal government absent militia service.
 *   This reading was the dominant judicial interpretation for most of the
 *   20th century and remains the framing of choice for state regulatory
 *   authority and federal law enforcement coordination. The constraint
 *   operates as a natural constitutional limit on federal power—states retain
 *   plenary authority over individual arms within their borders—but this
 *   'natural law' framing beneficiaries state administrative capacity and law
 *   enforcement uniformity. The measurement interval (0–120 years) tracks the
 *   reading's trajectory from near-universal acceptance (early 20th century)
 *   through rising contestation (late 20th century) to the present era where
 *   it coexists with stronger individual-right and civic-republican readings.
 *
 * KEY AGENTS:
 *   - state_governments: primary institutional beneficiary; interpret the reading as constitutional protection of state militia authority and plenary regulatory power over individual arms
 *   - federal_law_enforcement: organizational beneficiary; benefits from uniform federal prosecution authority under reading-compatible statutes (National Firearms Act, Uniform Code of Military Justice)
 *   - courts_applying_collective_reading: institutional agents; interpret the amendment through the militia lens, upholding weapons regulations as constitutional outside militia context
 *   - gun_rights_advocates: excluded/contending seats; would dispute the limitation of the right to organized militia, contest the reading as doctrinal construction rather than constitutional principle
 *   - state_militias_national_guard: institutional beneficiary; derive exclusive constitutional protection from the reading's framing of state militia authority as the right's core object
 *   - constitutional_law_scholars_originalist_school: observer seats divided by reading; some defend collective reading as textually faithful, others argue it misreads founding intent
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(second_amendment_arms_right__collective_right_reading, 0.38).
domain_priors:suppression_score(second_amendment_arms_right__collective_right_reading, 0.25).
domain_priors:theater_ratio(second_amendment_arms_right__collective_right_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(second_amendment_arms_right__collective_right_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(second_amendment_arms_right__collective_right_reading, suppression_requirement, 0.25).
narrative_ontology:constraint_metric(second_amendment_arms_right__collective_right_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(second_amendment_arms_right__collective_right_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(second_amendment_arms_right__collective_right_reading, resistance, 0.78).

% --- Constraint claim ---
narrative_ontology:constraint_claim(second_amendment_arms_right__collective_right_reading, mountain).
narrative_ontology:human_readable(second_amendment_arms_right__collective_right_reading, "Second Amendment: Collective Right Reading (State Militia Authority)").
narrative_ontology:topic_domain(second_amendment_arms_right__collective_right_reading, "constitutional_law/political_philosophy").

domain_priors:emerges_naturally(second_amendment_arms_right__collective_right_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(second_amendment_arms_right__collective_right_reading, 'ac18bc84-4c7f-462a-86a1-3b5a9ae79b9f').
narrative_ontology:cs_kernel_codification('ac18bc84-4c7f-462a-86a1-3b5a9ae79b9f', fixed_text).
narrative_ontology:cs_authority_grounding('ac18bc84-4c7f-462a-86a1-3b5a9ae79b9f', lineage).
narrative_ontology:cs_interpretation_layer_present('ac18bc84-4c7f-462a-86a1-3b5a9ae79b9f').
narrative_ontology:cs_reading_relation('ac18bc84-4c7f-462a-86a1-3b5a9ae79b9f', second_amendment_arms_right__individual_right_reading, forecloses).
narrative_ontology:cs_reading_relation('ac18bc84-4c7f-462a-86a1-3b5a9ae79b9f', second_amendment_arms_right__civic_republican_reading, coexists_with).
narrative_ontology:cs_axiom('ac18bc84-4c7f-462a-86a1-3b5a9ae79b9f', foundational, militia_authority_primary_right_object).
narrative_ontology:cs_axiom_status(militia_authority_primary_right_object, holdable).
narrative_ontology:cs_axiom_grounding('ac18bc84-4c7f-462a-86a1-3b5a9ae79b9f', militia_authority_primary_right_object, empirically_contingent).
narrative_ontology:cs_axiom('ac18bc84-4c7f-462a-86a1-3b5a9ae79b9f', foundational, state_plenary_regulatory_authority_outside_militia_context).
narrative_ontology:cs_axiom_status(state_plenary_regulatory_authority_outside_militia_context, holdable).
narrative_ontology:cs_axiom_grounding('ac18bc84-4c7f-462a-86a1-3b5a9ae79b9f', state_plenary_regulatory_authority_outside_militia_context, deontological).
narrative_ontology:cs_reference_frame('ac18bc84-4c7f-462a-86a1-3b5a9ae79b9f', state_militia_constitutional_protection).
narrative_ontology:cs_drift_state('ac18bc84-4c7f-462a-86a1-3b5a9ae79b9f', contemporary_individual_right_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('ac18bc84-4c7f-462a-86a1-3b5a9ae79b9f', '').
narrative_ontology:cs_kernel_id(second_amendment_arms_right__collective_right_reading, second_amendment_arms_right).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(second_amendment_arms_right__collective_right_reading, state_governments).
narrative_ontology:constraint_beneficiary(second_amendment_arms_right__collective_right_reading, militia_regulation_doctrine).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(second_amendment_arms_right__collective_right_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(second_amendment_arms_right__collective_right_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(second_amendment_arms_right__collective_right_reading_tests).

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(second_amendment_arms_right__collective_right_reading, ExtMetricName, E),
    domain_priors:suppression_score(second_amendment_arms_right__collective_right_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(second_amendment_arms_right__collective_right_reading),
    narrative_ontology:constraint_metric(second_amendment_arms_right__collective_right_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(second_amendment_arms_right__collective_right_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(second_amendment_arms_right__collective_right_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is modest (0.38 at interval end) because the reading's primary function—protecting state regulatory authority—is presented as constitutional principle, not as extraction from individual gun owners. The constraint does not require those individuals to pay a fee or transfer resources; it constrains their available choices. Suppression (0.25) is low because the constraint operates through constitutional doctrine and judicial precedent rather than through active enforcement machinery—the suppression is structural (constitutional limitation on individual liberty) rather than coercive (police action against violators). Theater (0.22) is present because the reading's doctrinal maintenance increasingly involves performative constitutional reasoning as the individual-right reading gains judicial acceptance—emphasis on the prefatory clause, recitation of militia founding-problem language, and reaffirmation of state authority serve to maintain the reading's legitimacy even as its empirical grip on courts and public opinion weakens. The measurement series show rising extractiveness, theater, and suppression_requirement over the 120-year interval, indicating that the reading's doctrinal authority has required increasing performative maintenance as structural conditions (private arms ownership, weakening of state militia capacity, and individual-liberty jurisprudence) have drifted away from the reading's founding assumptions. Accessibility_collapse (0.72) is high because once the collective-right reading is understood as the constitutional framework, alternative individual-ownership claims appear legally foreclosed unless one rejects the reading itself entirely. Resistance (0.78) is high because the reading meets sustained intellectual and political challenge from individual-right advocates, militia theory advocates, and state-level constitutional provisions protecting broader arms rights.
 *
 * PERSPECTIVAL GAP:
 *   The state government and federal law enforcement seats experience the constraint as a natural constitutional protection of their legitimate authority. From those seats, the reading is not extraction but constitutionally warranted state power. The gun-owner seats experience the constraint as a regulatory limitation imposed on their liberty, justified by a constitutional reading they contest. The divergence is structural: the reading allocates constitutional protection to state authority and regulatory power, not to individual liberty—so the seats benefiting from that allocation perceive it as just constitutional law, while the seats bearing the regulatory constraint perceive it as doctrinal construction serving state power. The engine's per-seat classification from structural data will capture this gap: beneficiary seats compute as different types from target seats.
 *
 * DIRECTIONALITY LOGIC:
 *   The beneficiary structure reveals asymmetric directionality. State governments (the primary beneficiary—they retain plenary authority over individual arms outside militia contexts) sit at low d (around 0.15–0.25), receiving constitutional protection and regulatory power without bearing extraction costs. Federal law enforcement benefits from uniform authority to prosecute federal weapons violations without state-by-state variation; d for this organizational agent is also low. Gun owners who wish to possess arms outside organized militia contexts sit at high d (0.75–0.90): they bear the constraint of plenary state regulation, have no constitutional escape route under this reading, and cannot claim federal protection against state weapons bans. The reading extracts from them (constrains their choice set) in service of benefiting state regulatory authority. The measurement series show rising suppression_requirement, suggesting that maintaining the reading's doctrinal authority requires increasing enforcement effort (legislative affirmation of state weapons laws, judicial reaffirmation of militia framing, prosecution under compatible statutes) as structural pressure (gun ownership prevalence, individual-rights cultural movement) mounts against the reading's framework.
 *
 * MANDATROPHY ANALYSIS:
 *   The collective-right reading's founding problem—state militia capacity as a check on federal tyranny and a deterrent against federal disarmament—was functionally addressed by the Militia Acts (1792 onward) and by the evolution of the National Guard system, which gave states constitutional standing and statutory authority to maintain armed forces. The reading persists not because the founding problem remains live but because the doctrinal interpretation became institutionalized in constitutional law education, court precedent, and state regulatory practice. The rising theater_ratio in the measurement series (0.10 to 0.22 over 120 years) reflects this: increasingly, the collective-right reading's maintenance depends on reaffirming the militia framing and the state authority it protects, not on the reading's functional necessity. This does not immediately classify the constraint as a piton—the suppression_requirement remains modest and the reading still functions to allocate constitutional authority—but it flags the reading as a candidate for mandatrophy inspection: the founding problem may have become dead (state militias are now statutory creatures, not constitutional necessities), while the regulatory authority the reading protects persists as institutional inertia. The omitted fourth, individual_right_reading, offers an alternative that would dissolve the reading's extractiveness for gun owners; the fact that both readings coexist in contemporary jurisprudence (rather than one being completely displaced) suggests the reading's persistence is partly institutional and partly doctrinal contestation, not purely constitutional inevitability.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_doctrinal_construction,
    'Is the collective-right reading a natural legal principle discoverable in the constitutional text and founding-era materials, or a constructed doctrinal interpretation that benefits state regulatory authority and federal prosecutorial uniformity?',
    'Originalist textual analysis of the Second Amendment''s prefatory clause (''A well regulated Militia, being necessary to the security of a free State''); historical record of ratification debates and founding-era usage patterns; comparison with how the same generation''s founding documents treated other retained rights (Fourth Amendment, other Bill of Rights provisions).',
    'If the reading is natural/discoverable, it stands as a legitimate constitutional constraint on federal power and individual liberty claims are structural misreadings. If constructed, the reading benefits a specific coalition (state administrators, federal law enforcement) and the constraint''s extractiveness increases as beneficiary-driven reinterpretation rather than as constitutional principle.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(natural_law_vs_doctrinal_construction, empirical, 'Whether the collective-right reading is a natural constitutional principle or a constructed doctrine that benefits state/federal regulatory authority.').

omega_variable(
    founding_problem_obsolescence,
    'Was the Second Amendment''s founding problem—state militia capacity as hedge against federal tyranny—addressed by the Militia Act statutory framework or by constitutional amendment, or does it remain live?',
    'Historical analysis of the Militia Acts (1792, 1903, 20th-century developments); assessment of whether statutory militia authority satisfies the constitutional guarantee or whether the constitutional protection independently constrains federal militia regulation; examination of whether the founding problem (state capacity to resist federal encroachment) persists in modern federalism.',
    'If the founding problem is satisfied by statute and modern federalism provides alternative checks, the collective-right reading becomes a degraded constraint (piton candidate) maintained for institutional continuity rather than active protection. If the problem remains live, the reading retains its functional role as a constitutional constraint on federal power.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(founding_problem_obsolescence, empirical, 'Whether the state-militia protection function is still necessary or has been superseded by statutory and structural change.').

omega_variable(
    kernel_reading_sibling_divergence,
    'What structural differences in beneficiary/victim positioning and extraction mechanisms distinguish this collective-right reading from the individual-right and civic-republican readings of the same kernel?',
    'Comparative constraint analysis: examine (1) who benefits from each reading''s instantiation (state governments vs. armed citizens vs. republicanism-as-doctrine), (2) what extraction mechanisms each reading protects or enables, (3) what alternative readings are foreclosed vs. coexisting within each frame, (4) which reading''s acceptance by courts and legislatures produces the empirically observed regulation patterns.',
    'This omega is the committer-frame analysis: the three readings are three different constraints with three different ε values, three different beneficiary structures, and three different scope/enforcement profiles. Understanding the sibling divergence clarifies whether the collective-right reading is discovered constitutional law or constructed doctrine that benefits state administrative authority.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_sibling_divergence, conceptual, 'Kernel reading divergence: structural differences between collective, individual, and civic-republican readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(second_amendment_arms_right__collective_right_reading, 0, 120).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(seco_tr_t0, second_amendment_arms_right__collective_right_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(seco_tr_t40, second_amendment_arms_right__collective_right_reading, theater_ratio, 40, 0.14).
narrative_ontology:measurement(seco_tr_t80, second_amendment_arms_right__collective_right_reading, theater_ratio, 80, 0.18).
narrative_ontology:measurement(seco_tr_t120, second_amendment_arms_right__collective_right_reading, theater_ratio, 120, 0.22).

% Extraction over time
narrative_ontology:measurement(seco_be_t0, second_amendment_arms_right__collective_right_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(seco_be_t40, second_amendment_arms_right__collective_right_reading, base_extractiveness, 40, 0.32).
narrative_ontology:measurement(seco_be_t80, second_amendment_arms_right__collective_right_reading, base_extractiveness, 80, 0.35).
narrative_ontology:measurement(seco_be_t120, second_amendment_arms_right__collective_right_reading, base_extractiveness, 120, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(seco_su_t0, second_amendment_arms_right__collective_right_reading, suppression_requirement, 0, 0.15).
narrative_ontology:measurement(seco_su_t40, second_amendment_arms_right__collective_right_reading, suppression_requirement, 40, 0.18).
narrative_ontology:measurement(seco_su_t80, second_amendment_arms_right__collective_right_reading, suppression_requirement, 80, 0.21).
narrative_ontology:measurement(seco_su_t120, second_amendment_arms_right__collective_right_reading, suppression_requirement, 120, 0.25).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(second_amendment_arms_right__collective_right_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(second_amendment_arms_right__collective_right_reading, 0.12).
narrative_ontology:affects_constraint(second_amendment_arms_right__collective_right_reading, second_amendment_arms_right__individual_right_reading).
narrative_ontology:affects_constraint(second_amendment_arms_right__collective_right_reading, second_amendment_arms_right__civic_republican_reading).

% DUAL FORMULATION NOTE:
% The Second Amendment kernel admits three structurally distinct constraint readings: collective-right (state militia authority), individual-right (individual liberty), and civic-republican (armed citizenship). Each reading has its own ε value, its own beneficiary/victim structure, its own scope and enforcement profile. The collective-right reading constrains federal power and protects state regulatory authority; the individual-right reading constrains state and federal power and protects individual liberty; the civic-republican reading constrains tyrannical governance and protects republicanism. These are not the same constraint viewed from different angles—they are three different constraints with three different extraction patterns. The kernel_context and cs_structure.reading_relations record the doctrinal contest and logical relationships between them.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
