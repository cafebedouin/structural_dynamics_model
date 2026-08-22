% ============================================================================
% CONSTRAINT STORY: preparedness_persistence__competence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_preparedness_persistence__competence_reading, []).

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
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: preparedness_persistence__competence_reading
 *   human_readable: Preparedness Practice as Live Competence Maintenance
 *   domain: institutional/disaster_preparedness
 *
 * SUMMARY:
 *   Under this reading (the competence reading), drills and inspections are
 *   live exercised knowledge — regular practice that maintains the reflexive,
 *   embodied coordination competence required when a disaster occurs.
 *   Response personnel and disaster researchers attest that the founding
 *   problem (multi-agency coordination failure under stress) remains live,
 *   and that drills demonstrably improve performance. The constraint is
 *   presented as rope (coordination mechanism) with minimal extraction
 *   structure. This reading explicitly rejects the husk reading's claim that
 *   drills have degraded into mere ritual; it holds instead that competence
 *   is maintained through practice and is measurable in actual response
 *   outcomes. The measurement series shows extractiveness and theater ratio
 *   remaining low and flat across the interval, consistent with genuine
 *   coordination being performed rather than theatricalized.
 *
 * KEY AGENTS:
 *   - disaster_response_personnel (fire, EMS, emergency management, hospital emergency departments) — practice and execute the constraint, design drills, benefit from coordinated competence
 *   - regulatory_authorities (fire codes, building codes, hospital licensing) — mandate drills at specified frequencies, audit compliance
 *   - at_risk_populations (residents, workers in hazard zones) — depend entirely on whether response is genuinely competent
 *   - facility operators (hospitals, buildings, municipalities) — bear the cost of drills and inspections as resource burden
 *   - external_observers (disaster researchers, auditors) — assess whether drills maintain genuine competence or degrade into performance
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(preparedness_persistence__competence_reading, 0.12).
domain_priors:suppression_score(preparedness_persistence__competence_reading, 0.05).
domain_priors:theater_ratio(preparedness_persistence__competence_reading, 0.18).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(preparedness_persistence__competence_reading, extractiveness, 0.12).
narrative_ontology:constraint_metric(preparedness_persistence__competence_reading, suppression_requirement, 0.05).
narrative_ontology:constraint_metric(preparedness_persistence__competence_reading, theater_ratio, 0.18).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(preparedness_persistence__competence_reading, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(preparedness_persistence__competence_reading, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(preparedness_persistence__competence_reading, rope).
narrative_ontology:human_readable(preparedness_persistence__competence_reading, "Preparedness Practice as Live Competence Maintenance").
narrative_ontology:topic_domain(preparedness_persistence__competence_reading, "institutional/disaster_preparedness").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(preparedness_persistence__competence_reading, '2717d06b-52bf-47ab-8e06-7a694d292f06').
narrative_ontology:cs_kernel_codification('2717d06b-52bf-47ab-8e06-7a694d292f06', formalized).
narrative_ontology:cs_authority_grounding('2717d06b-52bf-47ab-8e06-7a694d292f06', lineage).
narrative_ontology:cs_interpretation_layer_present('2717d06b-52bf-47ab-8e06-7a694d292f06').
narrative_ontology:cs_reading_relation('2717d06b-52bf-47ab-8e06-7a694d292f06', preparedness_persistence__husk_reading, coexists_with).
narrative_ontology:cs_reading_relation('2717d06b-52bf-47ab-8e06-7a694d292f06', preparedness_persistence__hybrid_reading, coexists_with).
narrative_ontology:cs_axiom('2717d06b-52bf-47ab-8e06-7a694d292f06', foundational, disaster_competence_is_maintained_by_practice).
narrative_ontology:cs_axiom_status(disaster_competence_is_maintained_by_practice, holdable).
narrative_ontology:cs_axiom_grounding('2717d06b-52bf-47ab-8e06-7a694d292f06', disaster_competence_is_maintained_by_practice, empirically_contingent).
narrative_ontology:cs_axiom('2717d06b-52bf-47ab-8e06-7a694d292f06', secondary, drill_performance_correlates_with_actual_response_quality).
narrative_ontology:cs_axiom_status(drill_performance_correlates_with_actual_response_quality, holdable).
narrative_ontology:cs_axiom_grounding('2717d06b-52bf-47ab-8e06-7a694d292f06', drill_performance_correlates_with_actual_response_quality, empirically_contingent).
narrative_ontology:cs_reference_frame('2717d06b-52bf-47ab-8e06-7a694d292f06', drills_as_live_competence_maintenance).
narrative_ontology:cs_drift_state('2717d06b-52bf-47ab-8e06-7a694d292f06', contemporary_post_major_disasters, gap(stable, minor, true)).
narrative_ontology:cs_created_at('2717d06b-52bf-47ab-8e06-7a694d292f06', '').
narrative_ontology:cs_kernel_id(preparedness_persistence__competence_reading, preparedness_persistence).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(preparedness_persistence__competence_reading, disaster_response_personnel).
narrative_ontology:constraint_beneficiary(preparedness_persistence__competence_reading, at_risk_populations).
narrative_ontology:constraint_beneficiary(preparedness_persistence__competence_reading, institutional_continuity).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(preparedness_persistence__competence_reading, facility_and_infrastructure_operators).
narrative_ontology:constraint_vindicates(preparedness_persistence__competence_reading, disaster_response_requires_practiced_coordination).
narrative_ontology:constraint_vindicates(preparedness_persistence__competence_reading, muscle_memory_degrades_without_rehearsal).
narrative_ontology:constraint_vindicates(preparedness_persistence__competence_reading, institutional_knowledge_is_embodied_skill).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Firefighters, emergency medical technicians, civil protection officers, search-and-rescue teams, and hospital emergency departments practice drills and inspections to maintain the reflexive competence required when actual disasters occur. They design the drills, participate in them, and rely on the practiced coordination to function under the stress and chaos of real response. Their professional competence and credibility depend on demonstrated readiness.
narrative_ontology:constraint_stakeholder(preparedness_persistence__competence_reading, disaster_response_personnel, beneficiary,
    organized, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(preparedness_persistence__competence_reading, disaster_response_personnel, agenda_setter).

% The property of an institution (fire service, hospital, city government) maintaining operational capacity across personnel turnover, leadership change, and time. Drills and inspections embed knowledge in practice so it survives individual departure.
narrative_ontology:constraint_stakeholder(preparedness_persistence__competence_reading, institutional_continuity, beneficiary,
    analytical, generational, analytical, national).
narrative_ontology:stakeholder_non_agent(preparedness_persistence__competence_reading, institutional_continuity).

% Residents, patients, workers in hazard zones who depend on coordinated, practiced response during actual disasters. Their survival and recovery depends on whether response personnel are genuinely competent or merely perform competence. They cannot exit the hazard; they depend entirely on the quality of preparedness.
narrative_ontology:constraint_stakeholder(preparedness_persistence__competence_reading, at_risk_populations, beneficiary,
    powerless, immediate, trapped, local).

% Fire codes, building codes, hospital licensing, and civil protection mandates require drills and inspections at specified intervals and standards. They set the rules, audit compliance, and can sanction institutions that fail to drill adequately. Their role is to enforce the constraint's persistence through legal requirement.
narrative_ontology:constraint_stakeholder(preparedness_persistence__competence_reading, regulatory_authorities, agenda_setter,
    institutional, generational, analytical, national).

% Hospital administrators, building facility managers, and business continuity officers bear the resource and time cost of drills and inspections: personnel hours diverted from operational work, equipment wear, coordination overhead, and the cost of maintaining inspection competence. The cost is typically diffuse and not tracked as extraction, but it is real.
narrative_ontology:constraint_stakeholder(preparedness_persistence__competence_reading, facility_and_infrastructure_operators, payer,
    institutional, biographical, constrained, regional).

% Academic researchers studying organizational learning, disaster sociology, and knowledge retention. They can observe whether drills maintain genuine competence or degrade into performance and ritual, and can measure the correlation between drill frequency/quality and actual response outcomes.
narrative_ontology:constraint_stakeholder(preparedness_persistence__competence_reading, external_observers, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(preparedness_persistence__competence_reading, diffuse).
narrative_ontology:fixing_cost_class(preparedness_persistence__competence_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Drills and inspections solve the structural coordination problem that disaster response is multi-organizational (hospitals, fire, police, emergency management), requires split-second timing, depends on shared mental models of roles and decision-making, and cannot be learned on-the-job during the actual event. Regular practice in realistic scenarios maintains the cognitive and procedural alignment needed for effective response.
% TRANSFER_FUNCTION: Drills and inspections transfer time, attention, equipment wear, and facility disruption from operational capacity to preparedness maintenance. The constraint moves resource cost from the at-risk population (whose safety is the stake) to the response system (which must maintain readiness). This is not extraction; it is the price of coordination.
% ABSENT_VOICES: Communities that have experienced catastrophic failure of response systems (due to inadequate drilling or competence loss) are the primary corroborators of the founding problem, but they are often geographically and temporally absent from the regulatory decision-making about drill standards. Indigenous communities with historical knowledge of local hazards may be excluded from drill design, resulting in drills that miss local contingencies.
% DISAPPEARANCE_RATIONALE: If drills and inspections disappeared, institutional memory would degrade; response personnel would lose practiced coordination; the next major disaster would show inadequate response, and causality investigations would reveal that skills atrophied. Institutions would re-establish drills within weeks of a major failure. The constraint's absence would be discovered by its failures.
% FOUNDING_PROBLEM: Disaster response requires coordination among independently organized agencies (fire, hospital, police, emergency management) with no shared command structure during normal times. Competence at disaster response is perishable — it degrades without practice. Without regular drilling, response systems maintain only the formal knowledge of procedures but lose the embodied, practiced skill needed to execute them under stress and incomplete information.
% FOUNDING_PROBLEM_CORROBORATION: Every major disaster investigation (9/11 Commission, Hurricane Katrina AAR, COVID-19 emergency response reviews) identifies skill and coordination loss attributable to inadequate prior drilling. Response personnel themselves consistently report that actual disaster response felt chaotic or uncoordinated when prior drilling was minimal. Independent disaster sociology research documents the correlation between drill frequency and response quality. No credible authority contests that the problem is live; the contest is about remedies and reading (competence vs. husk), not about whether the founding problem exists.
narrative_ontology:disappearance_verdict(preparedness_persistence__competence_reading, world_rearranges).
narrative_ontology:founding_problem_status(preparedness_persistence__competence_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(preparedness_persistence__competence_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(preparedness_persistence__competence_reading, 'none', 1).
narrative_ontology:epsilon_provenance(preparedness_persistence__competence_reading, 0.12, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(preparedness_persistence__competence_reading_tests).
:- end_tests(preparedness_persistence__competence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.12) because the constraint moves resources toward the safety of at-risk populations, not toward an extracting seat. Suppression is minimal (0.05) because the constraint is legally mandated but requires no active coercion of participants — response personnel want to drill. Theater ratio is modest (0.18) because some drills include performative elements (the public flag-raising, the media observation, the official statement) but the core of the constraint is genuine practice. Accessibility collapse is very high (0.92) because once a disaster occurs, no alternative to practiced response exists; the constraint is inescapable. Resistance is very low (0.08) because the constraint's beneficiaries are the ones executing it — there is no powerful constituency resisting drills. The measurement trajectory is flat and low, indicating stable coordination maintenance rather than extraction accumulation or theater inflation.
 *
 * PERSPECTIVAL GAP:
 *   From the response personnel and at-risk population seats, the constraint is genuine life-saving coordination. From the facility operator seat, it is a mandated cost burden that yields no direct financial benefit. The regulatory seat treats it as a legal obligation enforcer. The external observer seat measures whether competence persists. The engine should compute these seats as similarly aligned (all seeing coordination with minimal extraction) because the structural data supports that alignment: low beneficiary/victim asymmetry, no agenda-setter capturing rents, no suppression needed. If the engine computed divergence, it would signal that the structural facts are more complex than this reading acknowledges — precisely the signal the competence reading is designed to contest with the husk reading.
 *
 * DIRECTIONALITY LOGIC:
 *   The constraint benefits at-risk populations (low d, near beneficiary), response personnel (low d, benefit from competence maintenance), and institutional continuity (non-agent, vindicated proposition). Facility operators bear a cost but not as targeted extraction — they are constrained by law, but the constraint does not extract from them to pay anyone else; the cost is diffuse. No agent has high d (target) because no agent is being extracted from for another's gain. Regulatory authorities set the constraint but do not capture rents from it; they are enforcer-seats, not beneficiary-seats. The structural data produce low directionality spread and no major asymmetry, consistent with coordination rather than extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading's classification of rope prevents mislabeling preparedness practice as piton (theater-driven inertia). The husk reading would argue the constraint has atrophied into mere performance and should be reclassified as piton; this reading argues that competence is measurable, outcomes data support genuine maintenance, and the theater ratio remains low because performance is incidental to practice. The mandatrophy question is empirical: if a disaster occurs and response is effective, the competence reading is vindicated; if response is ineffective despite recent drills, the husk reading gains credibility. This reading authors the empirical facts that would constitute evidence for genuine competence maintenance; it does not resolve the contest, but it makes the contest measurable.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    competence_measurement_ambiguity,
    'How is disaster response competence measured — by reported skill confidence, by actual drill performance metrics, or by outcomes from real disasters? Do these measures align?',
    'Correlate drill performance data with actual disaster response outcomes (latency, error rates, casualty outcomes). Compare self-reported competence with independent evaluation.',
    'If drill performance correlates with real-world outcomes, competence is maintained and the reading is vindicated. If correlation is weak or absent, the husk reading gains credibility — drills may create an illusion of competence rather than real capability.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(competence_measurement_ambiguity, empirical, 'Whether competence measured in drills persists in actual disasters.').

omega_variable(
    reading_contest_localization,
    'Is the contest between competence and husk readings a single empirical question (do drills maintain competence?) or a question about what counts as competence (felt confidence vs. measurable capability vs. actual outcomes)?',
    'Examine disagreements in post-disaster investigations and drill evaluations: do parties dispute the facts (competence was/was not present) or the criteria (what counts as competence)?',
    'If the contest is empirical, data about actual response outcomes will resolve it. If the contest is about criteria, no single measurement can resolve it; the readings coexist as frameworks. This affects whether competence reading should be foreclosed or coexisting relative to husk reading.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_contest_localization, conceptual, 'Whether the competence/husk contest is empirical or definitional.').

omega_variable(
    institutional_decay_vs_practice_maintenance,
    'For a given response organization (e.g., a fire department or hospital emergency department), does regular drilling maintain competence across decades, or does effectiveness systematically decline unless external shocks force renewal?',
    'Longitudinal study of the same organization''s drill performance and real-event response outcomes over 20+ years, controlling for personnel turnover, technology change, and external pressures.',
    'If competence persists flat with regular drilling, the constraint is genuine rope and the competence reading holds. If competence shows steady decay despite drilling, theater ratio rises, and atrophied competence suggests the husk reading is more accurate.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(institutional_decay_vs_practice_maintenance, empirical, 'Whether drills maintain competence over long timescales or competence decays despite drilling.').

omega_variable(
    theater_as_component_vs_dysfunction,
    'Is the theater in drills (official speeches, media presence, public displays) a component of the coordination function (maintaining public trust, reinforcing institutional legitimacy) or a dysfunction that displaces actual practice?',
    'Compare drill outcomes in high-theater settings (public events, official ceremonies) with outcomes in low-theater settings (rapid-cycle practice sessions, isolated scenario training). Measure whether theater improves or degrades measured competence.',
    'If theater supports competence (e.g., public visibility maintains funding, official recognition supports inter-agency buy-in), it is part of genuine coordination and the low theater ratio is correct. If theater displaces practice time or creates illusion of preparedness without substance, it indicates the husk reading dynamic.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(theater_as_component_vs_dysfunction, empirical, 'Whether theater in drills supports or undermines competence maintenance.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(preparedness_persistence__competence_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(prep_tr_t0, preparedness_persistence__competence_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(prep_tr_t8, preparedness_persistence__competence_reading, theater_ratio, 8, 0.17).
narrative_ontology:measurement(prep_tr_t16, preparedness_persistence__competence_reading, theater_ratio, 16, 0.18).
narrative_ontology:measurement(prep_tr_t24, preparedness_persistence__competence_reading, theater_ratio, 24, 0.19).
narrative_ontology:measurement(prep_tr_t32, preparedness_persistence__competence_reading, theater_ratio, 32, 0.2).
narrative_ontology:measurement(prep_tr_t40, preparedness_persistence__competence_reading, theater_ratio, 40, 0.18).

% Extraction over time
narrative_ontology:measurement(prep_be_t0, preparedness_persistence__competence_reading, base_extractiveness, 0, 0.1).
narrative_ontology:measurement(prep_be_t8, preparedness_persistence__competence_reading, base_extractiveness, 8, 0.11).
narrative_ontology:measurement(prep_be_t16, preparedness_persistence__competence_reading, base_extractiveness, 16, 0.12).
narrative_ontology:measurement(prep_be_t24, preparedness_persistence__competence_reading, base_extractiveness, 24, 0.12).
narrative_ontology:measurement(prep_be_t32, preparedness_persistence__competence_reading, base_extractiveness, 32, 0.13).
narrative_ontology:measurement(prep_be_t40, preparedness_persistence__competence_reading, base_extractiveness, 40, 0.12).

% Suppression requirement over time
narrative_ontology:measurement(prep_su_t0, preparedness_persistence__competence_reading, suppression_requirement, 0, 0.04).
narrative_ontology:measurement(prep_su_t8, preparedness_persistence__competence_reading, suppression_requirement, 8, 0.04).
narrative_ontology:measurement(prep_su_t16, preparedness_persistence__competence_reading, suppression_requirement, 16, 0.05).
narrative_ontology:measurement(prep_su_t24, preparedness_persistence__competence_reading, suppression_requirement, 24, 0.05).
narrative_ontology:measurement(prep_su_t32, preparedness_persistence__competence_reading, suppression_requirement, 32, 0.05).
narrative_ontology:measurement(prep_su_t40, preparedness_persistence__competence_reading, suppression_requirement, 40, 0.05).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(preparedness_persistence__competence_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(preparedness_persistence__competence_reading, 0.1).
narrative_ontology:affects_constraint(preparedness_persistence__competence_reading, preparedness_persistence__husk_reading).
narrative_ontology:affects_constraint(preparedness_persistence__competence_reading, preparedness_persistence__hybrid_reading).

% DUAL FORMULATION NOTE:
% The preparedness_persistence kernel decomposes into three readings based on contested claims about whether regular drills maintain live disaster response competence or have degraded into memorial ritual. The competence reading asserts that competence is empirically measurable and maintained through practice; the husk reading asserts that competence has atrophied while the form persists; the hybrid reading proposes stratification. Each reading instantiates a different constraint (different ε, different beneficiary/victim structure, different type). They are linked by network.affects_constraints to indicate they are siblings addressing the same contested kernel rather than independent constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
