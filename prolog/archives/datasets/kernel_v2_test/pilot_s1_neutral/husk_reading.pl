% ============================================================================
% CONSTRAINT STORY: husk_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_husk_reading, []).

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
 *   constraint_id: husk_reading
 *   human_readable: Drills and Inspections as Memorial Performance (Husk Reading)
 *   domain: disaster_preparedness/institutional_memory/commitment_systems
 *
 * SUMMARY:
 *   This constraint instantiates the husk_reading of the
 *   preparedness_persistence kernel. The kernel is contested: what does
 *   institutional continuity of disaster preparedness mean, and how is it
 *   maintained? The husk_reading answers: preparedness has become a memorial
 *   performance — drills follow checklist protocols, inspections complete
 *   paperwork, compliance is measured by presence and documentation, not by
 *   actual operational readiness. The functional capacity has atrophied while
 *   the institutional ritual persists, maintained by beneficiaries (municipal
 *   administration seeking liability protection, insurance firms seeking
 *   compliance documentation) who have structural incentive to preserve the
 *   theater. The flooded population bears the extraction of believing in
 *   phantom preparedness. The municipal emergency management office knows the
 *   drills are performative but is constrained by state mandates that require
 *   documentation, not demonstrated competence. The constraint exhibits high
 *   theater_ratio (0.85) because form (ritual completion) persists while
 *   operational content (actual flood-response capacity) has degraded. The
 *   temporal measurements show rising theater and extractiveness over the
 *   15-year interval: immediately post-disaster (t=0), drills had higher
 *   functional content and lower theater; as recovery proceeded and crisis
 *   receded, the drill regime calcified into pure ritual (t=10-15), with
 *   theater-optimization feeding back to further degrade actual competence.
 *   This is a piton — a former coordination mechanism (preparedness) that has
 *   become inertial performance.
 *
 * KEY AGENTS:
 *   - Flood-Vulnerable Population: Primary victim (powerless/trapped) — lives in flood zone, participates in drills believing they provide protection, bears extraction of phantom preparedness during actual flood event
 *   - Municipal Emergency Management Office: Institutional degrader (institutional/constrained) — knows drills are performative, maintains them to satisfy state compliance, actual function has atrophied
 *   - Insurance Underwriting Apparatus: Primary beneficiary (institutional/arbitrage) — uses drill documentation as evidence of institutional responsibility; can exit by raising premiums or denying coverage
 *   - State Regulatory Authority: Mandate-holder (powerful/mobile) — maintains drill-and-documentation regime, path-dependent on existing regulatory framework, theoretically can redesign but faces institutional inertia
 *   - Volunteer Fire/EMS Organizations: Organizational survivors (organized/constrained) — rely on drill participation to justify funding, know drills don't predict actual response, constrained by regulatory requirement
 *   - Analytical Observer: Risks false-summit naturalization (analytical/analytical) — risks treating atrophy as inherent feature of disaster preparedness rather than contingent institutional design choice
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(husk_reading, 0.65).
domain_priors:suppression_score(husk_reading, 0.7).
domain_priors:theater_ratio(husk_reading, 0.85).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(husk_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(husk_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(husk_reading, theater_ratio, 0.85).

% --- Constraint claim ---
narrative_ontology:constraint_claim(husk_reading, piton).
narrative_ontology:human_readable(husk_reading, "Drills and Inspections as Memorial Performance (Husk Reading)").
narrative_ontology:topic_domain(husk_reading, "disaster_preparedness/institutional_memory/commitment_systems").

domain_priors:requires_active_enforcement(husk_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(husk_reading, 'e6309503-9759-4016-8ab0-f5f9ac0e74f3').
narrative_ontology:cs_kernel_codification('e6309503-9759-4016-8ab0-f5f9ac0e74f3', formalized).
narrative_ontology:cs_authority_grounding('e6309503-9759-4016-8ab0-f5f9ac0e74f3', extraction).
narrative_ontology:cs_interpretation_layer_present('e6309503-9759-4016-8ab0-f5f9ac0e74f3').
narrative_ontology:cs_reading_relation('e6309503-9759-4016-8ab0-f5f9ac0e74f3', husk_reading__competence_reading, forecloses).
narrative_ontology:cs_reading_relation('e6309503-9759-4016-8ab0-f5f9ac0e74f3', husk_reading__hybrid_reading, coexists_with).
narrative_ontology:cs_axiom('e6309503-9759-4016-8ab0-f5f9ac0e74f3', foundational, preparedness_is_memorial_performance).
narrative_ontology:cs_axiom_status(preparedness_is_memorial_performance, holdable).
narrative_ontology:cs_axiom_grounding('e6309503-9759-4016-8ab0-f5f9ac0e74f3', preparedness_is_memorial_performance, empirically_contingent).
narrative_ontology:cs_axiom('e6309503-9759-4016-8ab0-f5f9ac0e74f3', secondary, atrophied_function_naturalizes_as_institutional_form).
narrative_ontology:cs_axiom_status(atrophied_function_naturalizes_as_institutional_form, holdable).
narrative_ontology:cs_axiom_grounding('e6309503-9759-4016-8ab0-f5f9ac0e74f3', atrophied_function_naturalizes_as_institutional_form, empirically_contingent).
narrative_ontology:cs_reference_frame('e6309503-9759-4016-8ab0-f5f9ac0e74f3', post_disaster_functional_preparedness).
narrative_ontology:cs_drift_state('e6309503-9759-4016-8ab0-f5f9ac0e74f3', contemporary, gap(codification_collapse, substantial, false)).
narrative_ontology:cs_created_at('e6309503-9759-4016-8ab0-f5f9ac0e74f3', '2026-02-26T00:00:00Z').
narrative_ontology:cs_kernel_id(husk_reading, preparedness_persistence).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(husk_reading, institutional_legitimacy_claim).
narrative_ontology:constraint_beneficiary(husk_reading, insurance_underwriting_apparatus).
narrative_ontology:constraint_beneficiary(husk_reading, municipal_administration).
narrative_ontology:constraint_victim(husk_reading, flood_vulnerable_population).
narrative_ontology:constraint_victim(husk_reading, actual_preparedness_capacity).
narrative_ontology:constraint_victim(husk_reading, disaster_response_resources).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: FLOOD-VULNERABLE RESIDENT (SNARE) — Structurally trapped: lives in the flood zone, cannot relocate (economic/social ties), participates in drills believing they provide protection (identity_locked could apply here, but the exit barrier is primarily material). Bears the extraction of believing in phantom preparedness while actual operational capacity atrophies. On drill day experiences theater; during flood experiences the atrophied reality.
constraint_indexing:constraint_classification(husk_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: MUNICIPAL EMERGENCY MANAGEMENT OFFICE (PITON) — Constrained by budget and staffing realities; knows drills are performative but maintains them to satisfy state compliance mandates and liability documentation. The office's primary function (actual preparedness coordination) has atrophied; what remains is the ritual. Sees the constraint as a degraded husk maintained because alternatives haven't replaced it, not because it works.
constraint_indexing:constraint_classification(husk_reading, piton,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: INSURANCE UNDERWRITING APPARATUS (ROPE) — Net beneficiary with high arbitrage capacity. Benefits from documentation that proves compliance existed; uses drill records and inspection checklist completion as evidence of institutional responsibility. Can exit the constraint by classifying a region as too-high-risk (and raising premiums or withdrawing coverage), which creates incentive to maintain the documentation fiction. Experiences the constraint as pure coordination: it provides the liability evidence needed to underwrite property risk.
constraint_indexing:constraint_classification(husk_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: STATE REGULATORY AUTHORITY (PITON) — Powerful but operates within a regulatory framework whose mandate (ensure preparedness) no longer matches its mechanism (drill frequency and documentation). Can rewrite regulations but faces path dependency: replacing the drill-and-document regime requires authorizing alternative compliance pathways (operational tabletop exercises, flood-risk modeling, staffing standards) which demand different expertise and resources. Mobile in principle but constrained in practice by institutional inertia.
constraint_indexing:constraint_classification(husk_reading, piton,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: VOLUNTEER FIRE/EMS ORGANIZATIONS (PITON) — Organized agents (fire departments, EMS squads) rely on drill participation to maintain volunteer engagement and training. Drills provide the ostensible justification for their budget allocation. Know from lived experience that drills don't predict actual flood response but constrained by the regulatory requirement: if they stop drilling, they lose mandate compliance status and funding. The constraint persists as performance to justify continued operations.
constraint_indexing:constraint_classification(husk_reading, piton,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / FALSE SUMMIT VIEW (MOUNTAIN) — Risks naturalizing the husk as inherent: 'Disaster preparedness always degrades without constant practice; drills maintain minimal baseline capacity; the ritual is inherent to institutional memory.' This framing treats the atrophy as inevitable rather than contingent on specific design choices (drill design, frequency, resource allocation, skill-maintenance mechanisms). The engine's false-summit detector will flag this: the constraint's high beneficiary concentration and theater_ratio indicate a constructed institutional arrangement, not a natural law.
constraint_indexing:constraint_classification(husk_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(husk_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(husk_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(husk_reading, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(husk_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(husk_reading, TR),
    TR >= 0.70.

:- end_tests(husk_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.65): Moderately high. The constraint extracts value from the flood-vulnerable population (phantom preparedness belief, false security) while benefiting institutional legitimacy (documentation, liability protection) and insurance underwriting (risk-rating evidence). The extraction is not maximal because the beneficiary set is not entirely extraction-optimized — municipal offices genuinely do some preparedness work, state authorities do genuinely desire safety (even if they maintain dysfunctional mechanisms). Theater_ratio (0.85): Very high. Drills measure compliance by checklist completion and drill-day presence, not by skill validation or actual emergency response prediction. Inspections verify that documentation exists and procedures are written, not that equipment functions or personnel can execute the procedures under stress. The high theater ratio reflects that the primary output of the system is performative — it satisfies regulatory requirements and generates liability evidence, not operational readiness. This is the diagnostic signature of a piton. Suppression (0.70): Moderate-high. Significant structural barriers to exit or reformation include state regulatory mandates (can only be changed at state level), insurance requirements (private parties use drill documentation in underwriting), path dependency of the drill regime (alternatives require authorized regulatory pathways), and the difficulty of mobilizing political support for replacing what appears to be working (it produces documentation). Not total suppression because reform is technically possible with sufficient political will.
 *
 * PERSPECTIVAL GAP:
 *   The piton classification is remarkably consistent across perspectives. All agents experience the constraint as degraded and performative — the gap between form and function. The municipal office and volunteer organizations see the degradation most clearly because they execute the drills and see the mismatch between drill scenarios and actual flood dynamics. The insurance apparatus sees it as purely functional (documentation); the state authority sees it as a legacy regime that should be reformed but can't easily be under current regulatory structures; the population experiences it as a false promise of safety. The only perspective that risks seeing the constraint differently is the analytical observer, who might naturalize the atrophy as inherent to disaster preparedness ('training always degrades without constant practice'), thus converting piton into mountain. This false-summit risk is high because the domain language ('preparedness is inherent work,' 'memory requires constant rehearsal') naturalizes what is actually an institutional choice.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) is derived from beneficiary/victim structure + power level + exit options. The flood-vulnerable population has d ≈ 0.95 (trapped target, bears extraction, no arbitrage exit). The municipal office has d ≈ 0.60 (constrained target, knows the dysfunction, can't exit due to mandates). The insurance apparatus has d ≈ 0.15 (beneficiary with arbitrage exit, can shift risk classification or withdraw). The state authority has d ≈ 0.50 (powerful but path-dependent, could redesign but faces inertia). Effective extractiveness chi is computed from d, power level, and scope — trapped powerless agents experience maximum chi; institutional agents with arbitrage exit experience minimum chi (or negative chi if they benefit). The piton classification derives from theater_ratio gate (theater > 0.5) rather than from high chi values.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLVED: The founding problem of the preparedness regime was real — after Hurricane Katrina and similar disasters, municipalities needed structured disaster preparedness frameworks to coordinate response. The mandate (drills, inspections, documentation) was designed to maintain institutional memory and coordination capacity. Over 15+ years, the mandate has outlived its function. The original objective (maintaining actual disaster-response capacity) has atrophied while the institutional ritual persists. The constraint now primarily vindicates two propositions: (1) institutional_continuity_doctrine — the idea that institutions persist through form even when function degrades, and (2) bureaucratic_accountability_via_documentation — the idea that accountability can be measured by checklist completion rather than outcome achievement. The husk_reading asserts that mandatrophy is complete: what remains is memorial performance. This is distinct from the competence_reading, which would assert that drills still maintain baseline operational capacity despite degradation (mandatrophy partial or resolvable). This reading forecloses the competence_reading's core assumption.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    husk_vs_competence_reading,
    'Is this constraint fundamentally a piton (memorial performance with atrophied function) or a tangled rope (coordination function persists alongside institutional extraction)? Can the same preparedness regime be read both ways, or do the readings foreclose each other?',
    'Historical analysis of drill participation vs actual flood-response outcomes in same municipality over 10+ year period. If drill frequency correlates with better response: readings coexist (different parties see different things). If no correlation: husk reading forecloses competence reading (atrophy is real, function is phantom).',
    'If readings coexist: the constraint is a hybrid (competence_reading is live). If husk forecloses: preparedness mandates require structural redesign, not better compliance with existing drills.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(husk_vs_competence_reading, empirical, 'Whether drills maintain actual operational competence or only documentation theater').

omega_variable(
    memorial_function_clarity,
    'What is drills'' actual functional target: maintaining organizational memory (true memorial function), maintaining community awareness (civic function), maintaining regulatory compliance (bureaucratic function), or maintaining funding justification (organizational function)? Are these compatible or do they create conflicting incentives that degrade all of them?',
    'Process audit: analyze drill curriculum design, skill assessment, resource allocation, and post-drill organizational changes. Interview participants at all levels on what they believe drills are for. Compare stated function vs actual measurement and incentive structure.',
    'If functions align: constraint might be hybrid coordination. If they conflict: each function cannibalizes the others, producing the pure theater pattern (piton classification confirmed).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(memorial_function_clarity, empirical, 'Functional target and alignment of drill curricula').

omega_variable(
    institutional_legitimacy_vs_population_safety,
    'Who is the constraint designed to serve: institutional actors seeking liability protection and regulatory compliance (institutional legitimacy), or the flood-vulnerable population seeking actual protective capacity? Are these beneficiary structures compatible or mutually exclusive?',
    'Beneficiary tracing: follow the value flow. Whom does the constraint benefit when executed as designed? Whose objectives are vindicated by successful drill completion? Who bears costs when the constraint operates at full theater?',
    'If both beneficiary sets benefit: constraint is likely coordination (rope or tangled rope). If beneficiary sets are opposed: constraint is extraction (snare or piton). Husk reading asserts opposition.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(institutional_legitimacy_vs_population_safety, empirical, 'Institutional legitimacy vs population safety as incompatible beneficiary targets').

omega_variable(
    reading_foreclose_boundary,
    'This reading (husk_reading) asserts that the preparedness constraint has become a piton — atrophied function, maintained as performance. The competence_reading asserts that drills, while imperfect, maintain baseline organizational capacity. Can both readings be held simultaneously within one regulatory framework, or does husk_reading''s core premise logically foreclose the competence_reading?',
    'Semantic analysis of the axioms. If husk reading''s foundational claim (preparedness_is_memorial_performance) is true, is competence_reading''s foundational claim (operational_competence_maintained_by_drills) necessarily false? Or do they differ only in empirical weight assigned to the same mechanisms?',
    'If they foreclose each other: readings are mutually exclusive; the constraint is one of two types depending on empirical facts. If they coexist: readings represent genuinely different institutional positions (different parties hold different readings) and the constraint is hybrid.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_foreclose_boundary, conceptual, 'Whether husk and competence readings are logically foreclosing or empirically contestable').

omega_variable(
    theater_ratchet_mechanism,
    'Once the constraint calcifies as theater (high theater_ratio), does the mechanism become self-reinforcing? Does theater-optimization feed back to further degrade actual competence? Or is atrophy reversible with resource reallocation?',
    'Historical case study: a municipality that discontinued traditional drills in favor of alternative preparedness mechanisms (e.g., flood-risk modeling, operational readiness audits, resource pre-positioning). Did operational competence improve, degrade, or stay flat? Did the shift change insurance underwriting or regulatory classification?',
    'If theater is self-reinforcing: piton classification is sticky (atrophy is difficult to reverse). If reversible: constraint might be scaffold (temporary degradation with potential sunset). If irreversible without massive reinvestment: constraint is entrenched piton.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(theater_ratchet_mechanism, empirical, 'Reversibility of theater calcification and atrophy feedback loops').

omega_variable(
    kernel_reading_ambiguity,
    'This constraint instantiates the husk_reading of the preparedness_persistence kernel. The contested kernel is: ''What does preparedness institutional continuity mean, and how is it maintained?'' The husk_reading answers: ''It means memorial ritual; it is maintained by theater that naturalizes atrophied function.'' But the competence_reading answers: ''It means retained capacity; it is maintained by practice and community engagement.'' Are these readings of the SAME kernel (one constraint with two readings) or are they describing different kernels entirely?',
    'Kernel identity test: both readings invoke the same stabilized commitment (state preparedness regulations, municipal disaster codes, FEMA training standards). Both readings bind the same institutional framework. But they attribute different functions and different mechanisms to that framework. This is a kernel-reading scenario IF the underlying commitment persists and the readings differ in how they interpret that commitment''s meaning.',
    'If same kernel, different readings: husk_reading and competence_reading are structurally related via reading_relations and should coordinate via cs_structure. If different kernels: they are separate constraint families with no reading_relations (each has its own axioms).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_ambiguity, conceptual, 'Kernel identity: whether husk and competence readings invoke the same stabilized commitment').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(husk_reading, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(husk_theater_t0_post_katrina, husk_reading, theater_ratio, 0, 0.55).
narrative_ontology:measurement(husk_theater_t5_mid_recovery, husk_reading, theater_ratio, 5, 0.72).
narrative_ontology:measurement(husk_theater_t10_current, husk_reading, theater_ratio, 10, 0.85).
narrative_ontology:measurement(husk_theater_t15_trajectory, husk_reading, theater_ratio, 15, 0.88).

% Extraction over time
narrative_ontology:measurement(husk_extract_t0_post_disaster, husk_reading, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(husk_extract_t5_mid_recovery, husk_reading, base_extractiveness, 5, 0.52).
narrative_ontology:measurement(husk_extract_t10_current, husk_reading, base_extractiveness, 10, 0.65).
narrative_ontology:measurement(husk_extract_t15_trajectory, husk_reading, base_extractiveness, 15, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(husk_suppress_t0_post_disaster, husk_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(husk_suppress_t5_mid_recovery, husk_reading, suppression_requirement, 5, 0.6).
narrative_ontology:measurement(husk_suppress_t10_current, husk_reading, suppression_requirement, 10, 0.7).
narrative_ontology:measurement(husk_suppress_t15_trajectory, husk_reading, suppression_requirement, 15, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(husk_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(husk_reading, 0.18).
narrative_ontology:affects_constraint(husk_reading, competence_reading).
narrative_ontology:affects_constraint(husk_reading, hybrid_reading).
narrative_ontology:affects_constraint(husk_reading, insurance_risk_rating).
narrative_ontology:affects_constraint(husk_reading, state_liability_regime).

% DUAL FORMULATION NOTE:
% The preparedness_persistence kernel admits multiple structurally distinct constraint readings. The husk_reading asserts that the constraint has become piton (atrophied function, maintained as performance). The competence_reading asserts it remains tangled_rope (coordination function persists alongside institutional extraction). The hybrid_reading asserts both are partially true depending on the municipality. These are not the same constraint viewed from different angles — they have different ε values reflecting different functional assessments. The husk_reading is linked to competence_reading and hybrid_reading via the kernel relationship, not via network.affects_constraints (which is for single-reading constraints that influence other constraints). The sibling relationship is declared in cs_structure.reading_relations.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(husk_reading, powerful, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
