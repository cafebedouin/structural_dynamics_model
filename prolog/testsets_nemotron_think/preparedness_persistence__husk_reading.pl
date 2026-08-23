% ============================================================================
% CONSTRAINT STORY: preparedness_persistence__husk_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_preparedness_persistence__husk_reading, []).

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
 *   constraint_id: preparedness_persistence__husk_reading
 *   human_readable: Disaster Preparedness Drills as Memorial Performance
 *   domain: disaster_preparedness/institutional_memory
 *
 * SUMMARY:
 *   A regional emergency management agency mandates quarterly flood
 *   preparedness drills and annual facility inspections. The drills are
 *   scripted tabletop exercises with predetermined outcomes; inspections
 *   check paperwork compliance rather than functional capacity. The agency
 *   publishes after-action reports declaring 'readiness maintained.'
 *   Independent studies show that when actual floods occur, communication
 *   fails, equipment is inoperable, and evacuation routes are blocked — the
 *   drills have not prevented competence atrophy. The constraint persists
 *   because it generates institutional legitimacy: the agency appears
 *   prepared, politicians claim credit, and the public feels protected. The
 *   form is a husk; the operational kernel has rotted.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(preparedness_persistence__husk_reading, 0.68).
domain_priors:suppression_score(preparedness_persistence__husk_reading, 0.35).
domain_priors:theater_ratio(preparedness_persistence__husk_reading, 0.82).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(preparedness_persistence__husk_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(preparedness_persistence__husk_reading, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(preparedness_persistence__husk_reading, theater_ratio, 0.82).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(preparedness_persistence__husk_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(preparedness_persistence__husk_reading, resistance, 0.28).

% --- Constraint claim ---
narrative_ontology:constraint_claim(preparedness_persistence__husk_reading, piton).
narrative_ontology:human_readable(preparedness_persistence__husk_reading, "Disaster Preparedness Drills as Memorial Performance").
narrative_ontology:topic_domain(preparedness_persistence__husk_reading, "disaster_preparedness/institutional_memory").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(preparedness_persistence__husk_reading, 'a6857e71-8cf5-44d6-af70-023b1b1bd14a').
narrative_ontology:cs_kernel_codification('a6857e71-8cf5-44d6-af70-023b1b1bd14a', formalized).
narrative_ontology:cs_authority_grounding('a6857e71-8cf5-44d6-af70-023b1b1bd14a', extraction).
narrative_ontology:cs_interpretation_layer_present('a6857e71-8cf5-44d6-af70-023b1b1bd14a').
narrative_ontology:cs_reading_relation('a6857e71-8cf5-44d6-af70-023b1b1bd14a', preparedness_persistence__competence_reading, forecloses).
narrative_ontology:cs_reading_relation('a6857e71-8cf5-44d6-af70-023b1b1bd14a', preparedness_persistence__hybrid_reading, forecloses).
narrative_ontology:cs_axiom('a6857e71-8cf5-44d6-af70-023b1b1bd14a', foundational, preparedness_rituals_are_hollow).
narrative_ontology:cs_axiom_status(preparedness_rituals_are_hollow, holdable).
narrative_ontology:cs_axiom_grounding('a6857e71-8cf5-44d6-af70-023b1b1bd14a', preparedness_rituals_are_hollow, empirically_contingent).
narrative_ontology:cs_axiom('a6857e71-8cf5-44d6-af70-023b1b1bd14a', secondary, institutional_legitimacy_extracts_from_false_preparedness).
narrative_ontology:cs_axiom_status(institutional_legitimacy_extracts_from_false_preparedness, holdable).
narrative_ontology:cs_axiom_grounding('a6857e71-8cf5-44d6-af70-023b1b1bd14a', institutional_legitimacy_extracts_from_false_preparedness, empirically_contingent).
narrative_ontology:cs_reference_frame('a6857e71-8cf5-44d6-af70-023b1b1bd14a', ritualized_compliance_framework).
narrative_ontology:cs_drift_state('a6857e71-8cf5-44d6-af70-023b1b1bd14a', contemporary, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('a6857e71-8cf5-44d6-af70-023b1b1bd14a', '').
narrative_ontology:cs_kernel_id(preparedness_persistence__husk_reading, preparedness_persistence).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(preparedness_persistence__husk_reading, emergency_management_agency).
narrative_ontology:constraint_victim(preparedness_persistence__husk_reading, population_at_flood_risk).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(preparedness_persistence__husk_reading, frontline_responders).
narrative_ontology:constraint_vindicates(preparedness_persistence__husk_reading, institutional_legitimacy_through_ritual).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Mandates and conducts regular drills and inspections. Receives budget, political cover, and public trust from the appearance of preparedness. The agency controls the drill standards and evaluation criteria, allowing it to define success in ways that sustain the ritual without requiring operational competence.
narrative_ontology:constraint_stakeholder(preparedness_persistence__husk_reading, emergency_management_agency, agenda_setter,
    institutional, generational, arbitrage, regional).
narrative_ontology:stakeholder_secondary_role(preparedness_persistence__husk_reading, emergency_management_agency, beneficiary).

% Lives in flood-prone areas and relies on the agency's preparedness for actual protection. Bears the full cost of false assurance: when a real event occurs, the atrophied response capacity leads to preventable harm. Has no meaningful exit — relocation is economically impossible, and there is no alternative provider of flood protection.
narrative_ontology:constraint_stakeholder(preparedness_persistence__husk_reading, population_at_flood_risk, payer,
    powerless, biographical, trapped, local).

% Participate in drills knowing they are scripted and lack realism. Invest time and effort that could go to real training. Their professional judgment is overridden by the ritual requirements. Exit means leaving the profession or transferring to a different jurisdiction, both costly.
narrative_ontology:constraint_stakeholder(preparedness_persistence__husk_reading, frontline_responders, payer,
    moderate, biographical, constrained, local).

% Conduct after-action reviews and academic studies of preparedness systems. Document the gap between drill performance and real-world outcomes. Their findings are acknowledged ceremonially but not acted upon. They have no stake in the ritual's persistence.
narrative_ontology:constraint_stakeholder(preparedness_persistence__husk_reading, independent_auditors, observer,
    analytical, generational, analytical, national).

% Develop community-based early warning, mutual aid networks, and adaptive planning methods. Their approaches are marginalized because they don't fit the institutional drill framework and threaten the agency's legitimacy monopoly. They could operate elsewhere but seek to serve this population.
narrative_ontology:constraint_stakeholder(preparedness_persistence__husk_reading, alternative_preparedness_innovators, excluded,
    moderate, biographical, mobile, regional).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Originally: synchronize multi-agency response, maintain equipment readiness, and validate evacuation plans through repeated practice. Now: the drills coordinate only the performance of compliance — they synchronize paperwork, attendance, and checkbox completion across agencies.
% TRANSFER_FUNCTION: Transfers public trust, budget allocations, and political legitimacy to the emergency management agency. Transfers unmitigated flood risk and the consequences of unpreparedness to the population at risk. Transfers the time and cognitive load of ritual participation to frontline responders.
% ABSENT_VOICES: The population at flood risk (who would demand real preparedness if they understood the gap), frontline responders (who privately report the drills are hollow but cannot speak officially), and alternative preparedness innovators (who are structurally excluded from the institutional conversation).
% DISAPPEARANCE_RATIONALE: If the drill mandate vanished overnight, the agency would lose its primary legitimacy theater. Political pressure would force either a collapse of the preparedness claim (exposing the risk transfer) or a crash program to build genuine capacity. The population would lose false assurance but gain clarity about their actual vulnerability, enabling self-organization or political demand for real protection.
% FOUNDING_PROBLEM: After a catastrophic flood in the 1990s that exposed chaotic, uncoordinated response, the agency instituted mandatory drills and inspections to ensure multi-agency coordination, equipment readiness, and public confidence.
% FOUNDING_PROBLEM_CORROBORATION: Independent after-action reports from the 2010s (commissioned by the legislature, not the agency) and academic studies in disaster sociology (e.g., Tierney 2019, Boin & McConnell 2021) document that the drills no longer maintain operational competence — the founding problem of coordination and readiness is not served by the current ritual. The agency's own internal audits, when leaked, confirm the drift.
narrative_ontology:disappearance_verdict(preparedness_persistence__husk_reading, world_rearranges).
narrative_ontology:founding_problem_status(preparedness_persistence__husk_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(preparedness_persistence__husk_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(preparedness_persistence__husk_reading, 'none', 1).
narrative_ontology:epsilon_provenance(preparedness_persistence__husk_reading, 0.68, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(preparedness_persistence__husk_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(preparedness_persistence__husk_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(preparedness_persistence__husk_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68) is high because the agency captures trust and resources while delivering negligible risk reduction. Suppression (0.35) is moderate — the ritual crowds out alternative preparedness work but does not actively ban it. Theater ratio (0.82) is very high: the vast majority of drill activity is performative. Accessibility collapse (0.72) is high because the ritual occupies the institutional and cognitive space where real preparedness would be built. Resistance (0.28) is low because the population believes the drills work, responders are silenced by hierarchy, and innovators are marginalized. The measurement series track the gradual shift from functional coordination (early years) to pure theater (present).
 *
 * PERSPECTIVAL GAP:
 *   From the agency's seat, the constraint is a scaffold (transitional coordination that became ritual). From the population's seat, it is a snare (extraction of safety). From the responders' seat, it is a piton (degraded rope they must perform). The engine computes these per-seat types from the structural data; the divergence is the measurement.
 *
 * DIRECTIONALITY LOGIC:
 *   The emergency management agency sits at the beneficiary end (d ~0.15): it controls the constraint, collects the legitimacy rents, and faces arbitrage-grade exit (it could reform the drills). The population at flood risk is at the full target end (d ~0.95): they bear the extracted risk, are trapped, and have no voice in the drill design. Frontline responders are constrained targets (d ~0.65): they see the hollowness but cannot exit without career cost. Independent auditors are analytical (d ~0.5). Alternative innovators are excluded — their exit is mobile but they are kept out of the constraint's domain.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (post-disaster coordination) is dead — the original disaster context has passed, and the drills no longer produce the readiness they were built for. Yet the arrangement persists and has grown more elaborate. This is classic mandatrophy: the mandate (do drills) has outlived its function (maintain readiness). The agency resists reform because the ritual now serves a new function: legitimacy extraction. The classification as piton captures this — a former rope whose coordination function has atrophied, maintained by institutional inertia and theatrical performance.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    ritual_vs_residual_coordination,
    'Does the drill ritual provide any residual coordination value (e.g., maintaining interpersonal relationships across agencies) that would be lost if the drills stopped, even if the scripted content is hollow?',
    'Compare inter-agency communication metrics during real events in jurisdictions that abolished scripted drills vs. those that kept them. Control for other factors.',
    'If residual coordination exists, the constraint is a tangled_rope (coordination + extraction) rather than a pure piton. If none, the piton classification is confirmed and the theater ratio reflects pure waste.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ritual_vs_residual_coordination, empirical, 'Whether the memorial performance has any hidden coordination floor.').

omega_variable(
    atrophy_cause_ambiguity,
    'Is the competence atrophy driven by resource constraints (budget cuts, staff turnover) or by deliberate institutional choice to prioritize appearance over substance?',
    'Trace budget allocations and internal memos: if drill budgets grew while training budgets shrank, it indicates choice. If both shrank, it indicates constraint.',
    'If deliberate, the extraction is active and the agency is a knowing beneficiary (snare-like). If passive, the agency is also a victim of systemic neglect (piton with diffuse responsibility).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(atrophy_cause_ambiguity, conceptual, 'Whether the hollowing out is a feature or a bug of the institutional trajectory.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(preparedness_persistence__husk_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(preparedness_persistence__husk_reading_tr_t0, preparedness_persistence__husk_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement(preparedness_persistence__husk_reading_tr_t6, preparedness_persistence__husk_reading, theater_ratio, 6, 0.5).
narrative_ontology:measurement(preparedness_persistence__husk_reading_tr_t12, preparedness_persistence__husk_reading, theater_ratio, 12, 0.62).
narrative_ontology:measurement(preparedness_persistence__husk_reading_tr_t18, preparedness_persistence__husk_reading, theater_ratio, 18, 0.71).
narrative_ontology:measurement(preparedness_persistence__husk_reading_tr_t24, preparedness_persistence__husk_reading, theater_ratio, 24, 0.78).
narrative_ontology:measurement(preparedness_persistence__husk_reading_tr_t30, preparedness_persistence__husk_reading, theater_ratio, 30, 0.82).

% Extraction over time
narrative_ontology:measurement(preparedness_persistence__husk_reading_be_t0, preparedness_persistence__husk_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(preparedness_persistence__husk_reading_be_t6, preparedness_persistence__husk_reading, base_extractiveness, 6, 0.52).
narrative_ontology:measurement(preparedness_persistence__husk_reading_be_t12, preparedness_persistence__husk_reading, base_extractiveness, 12, 0.58).
narrative_ontology:measurement(preparedness_persistence__husk_reading_be_t18, preparedness_persistence__husk_reading, base_extractiveness, 18, 0.63).
narrative_ontology:measurement(preparedness_persistence__husk_reading_be_t24, preparedness_persistence__husk_reading, base_extractiveness, 24, 0.66).
narrative_ontology:measurement(preparedness_persistence__husk_reading_be_t30, preparedness_persistence__husk_reading, base_extractiveness, 30, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(preparedness_persistence__husk_reading_su_t0, preparedness_persistence__husk_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(preparedness_persistence__husk_reading_su_t6, preparedness_persistence__husk_reading, suppression_requirement, 6, 0.28).
narrative_ontology:measurement(preparedness_persistence__husk_reading_su_t12, preparedness_persistence__husk_reading, suppression_requirement, 12, 0.3).
narrative_ontology:measurement(preparedness_persistence__husk_reading_su_t18, preparedness_persistence__husk_reading, suppression_requirement, 18, 0.32).
narrative_ontology:measurement(preparedness_persistence__husk_reading_su_t24, preparedness_persistence__husk_reading, suppression_requirement, 24, 0.34).
narrative_ontology:measurement(preparedness_persistence__husk_reading_su_t30, preparedness_persistence__husk_reading, suppression_requirement, 30, 0.35).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(preparedness_persistence__husk_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(preparedness_persistence__husk_reading, 0.12).
narrative_ontology:affects_constraint(preparedness_persistence__husk_reading, preparedness_persistence__competence_reading).
narrative_ontology:affects_constraint(preparedness_persistence__husk_reading, preparedness_persistence__hybrid_reading).

% DUAL FORMULATION NOTE:
% This constraint family decomposes the preparedness_persistence kernel into three readings. The competence_reading sees the kernel as a live rope (coordination function intact). The hybrid_reading sees a stratified system: engineering inspections as rope, evacuation drills as piton. This husk_reading sees the entire kernel as a piton — the coordination function has fully atrophied. The ε values differ: competence_reading ε~0.15, hybrid_reading ε~0.4 (stratified), husk_reading ε~0.68. They are linked because each reading cites the same regulatory framework but interprets its operational reality differently.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(preparedness_persistence__husk_reading, institutional, 0.15).
constraint_indexing:directionality_override(preparedness_persistence__husk_reading, powerless, 0.95).
constraint_indexing:directionality_override(preparedness_persistence__husk_reading, moderate, 0.65).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
