% ============================================================================
% CONSTRAINT STORY: preparedness_retention__competence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_preparedness_retention__competence_reading, []).

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
    narrative_ontology:suppression_profile/2,
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
 *   constraint_id: preparedness_retention__competence_reading
 *   human_readable: Preparedness as Live Exercised Competence
 *   domain: disaster_preparedness/institutional_memory/governance
 *
 * SUMMARY:
 *   This story instantiates the competence_reading of the
 *   preparedness_retention kernel: drills and inspections are treated as
 *   genuinely live-exercised practices that maintain the operational capacity
 *   of flood and disaster-response institutions. Under this reading, the
 *   ceremony-to-competence ratio is low — exercises are designed and audited
 *   to test actual skill retention (response time, decision quality,
 *   inter-agency coordination) rather than to produce a compliance record.
 *   This is a distinct constraint from the husk_reading (where the same
 *   activities are read as memorial ritual lacking live competence) and the
 *   hybrid_reading (where competence is stratified, retained in specialized
 *   technical bodies while broader societal memory goes ceremonial) — each of
 *   those is a separate ε and a separate constraint, linked here only by
 *   shared kernel identity, not shared classification.
 *
 * KEY AGENTS:
 *   - flood_control_operators: Primary agenda-setter (institutional/constrained) — designs and executes the drill and inspection cycle
 *   - coastal_population: Primary beneficiary (powerless/trapped) — depends on genuine readiness for survival outcomes
 *   - emergency_response_agencies: Secondary beneficiary/payer (organized/constrained) — builds real interoperability through exercises
 *   - independent_auditors: Analytical observer (moderate/analytical) — verifies competence retention against ritual substitution
 *   - fiscal_oversight_bodies: Analytical observer, largely quiet (institutional/analytical) — would object to over-investment but under this reading has little basis to
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(preparedness_retention__competence_reading, 0.18).
domain_priors:suppression_score(preparedness_retention__competence_reading, 0.12).
domain_priors:theater_ratio(preparedness_retention__competence_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(preparedness_retention__competence_reading, extractiveness, 0.18).
narrative_ontology:constraint_metric(preparedness_retention__competence_reading, suppression_requirement, 0.12).
narrative_ontology:constraint_metric(preparedness_retention__competence_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(preparedness_retention__competence_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(preparedness_retention__competence_reading, resistance, 0.25).

% --- Constraint claim ---
narrative_ontology:constraint_claim(preparedness_retention__competence_reading, rope).
narrative_ontology:human_readable(preparedness_retention__competence_reading, "Preparedness as Live Exercised Competence").
narrative_ontology:topic_domain(preparedness_retention__competence_reading, "disaster_preparedness/institutional_memory/governance").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(preparedness_retention__competence_reading, 'b75656b2-61f6-4c04-b212-0bd82413e04e').
narrative_ontology:cs_kernel_codification('b75656b2-61f6-4c04-b212-0bd82413e04e', distributed).
narrative_ontology:cs_authority_grounding('b75656b2-61f6-4c04-b212-0bd82413e04e', practice).
narrative_ontology:cs_interpretation_layer_present('b75656b2-61f6-4c04-b212-0bd82413e04e').
narrative_ontology:cs_reading_relation('b75656b2-61f6-4c04-b212-0bd82413e04e', preparedness_retention__husk_reading, coexists_with).
narrative_ontology:cs_reading_relation('b75656b2-61f6-4c04-b212-0bd82413e04e', preparedness_retention__hybrid_reading, influences).
narrative_ontology:cs_axiom('b75656b2-61f6-4c04-b212-0bd82413e04e', foundational, practice_generates_verifiable_skill_retention).
narrative_ontology:cs_axiom_status(practice_generates_verifiable_skill_retention, holdable).
narrative_ontology:cs_axiom_grounding('b75656b2-61f6-4c04-b212-0bd82413e04e', practice_generates_verifiable_skill_retention, empirically_contingent).
narrative_ontology:cs_axiom('b75656b2-61f6-4c04-b212-0bd82413e04e', secondary, ceremony_is_incidental_not_constitutive).
narrative_ontology:cs_axiom_status(ceremony_is_incidental_not_constitutive, holdable).
narrative_ontology:cs_axiom_grounding('b75656b2-61f6-4c04-b212-0bd82413e04e', ceremony_is_incidental_not_constitutive, empirically_contingent).
narrative_ontology:cs_reference_frame('b75656b2-61f6-4c04-b212-0bd82413e04e', live_exercise_competence_standard).
narrative_ontology:cs_drift_state('b75656b2-61f6-4c04-b212-0bd82413e04e', contemporary_climate_stress_era, gap(stable, minor, true)).
narrative_ontology:cs_created_at('b75656b2-61f6-4c04-b212-0bd82413e04e', '').
narrative_ontology:cs_kernel_id(preparedness_retention__competence_reading, preparedness_retention).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(preparedness_retention__competence_reading, coastal_population).
narrative_ontology:constraint_beneficiary(preparedness_retention__competence_reading, flood_control_operators).
narrative_ontology:constraint_beneficiary(preparedness_retention__competence_reading, emergency_response_agencies).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(preparedness_retention__competence_reading, emergency_response_agencies).
narrative_ontology:constraint_vindicates(preparedness_retention__competence_reading, operational_readiness_requires_live_practice).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Design, schedule, and run the drills and inspection cycles for dikes, pumping stations, and storm barriers. They bear the direct cost of running realistic exercises rather than paperwork audits, and their operational competence is what actually gets tested and renewed each cycle. They cannot exit the obligation to maintain readiness — their institutional purpose is defined by it.
narrative_ontology:constraint_stakeholder(preparedness_retention__competence_reading, flood_control_operators, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(preparedness_retention__competence_reading, flood_control_operators, beneficiary).

% Live behind the flood defenses whose reliability depends on whether the operating agencies can actually execute an emergency response, not merely certify that a plan exists. They have no direct role in the drills but are the party whose survival outcome is determined by whether exercised competence is real. They cannot meaningfully exit the geography they depend on.
narrative_ontology:constraint_stakeholder(preparedness_retention__competence_reading, coastal_population, beneficiary,
    powerless, biographical, trapped, regional).

% Fire services, evacuation coordinators, and medical response units participate in joint exercises that build genuine inter-agency coordination capacity. They pay in staff time and training budget, and gain real improvement in response speed and interoperability that shows up when an actual event occurs.
narrative_ontology:constraint_stakeholder(preparedness_retention__competence_reading, emergency_response_agencies, beneficiary,
    organized, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(preparedness_retention__competence_reading, emergency_response_agencies, payer).

% External inspectorates and academic reviewers assess after-action reports and drill outcomes for whether skill metrics (response time, error rate, decision quality under simulated stress) actually improve across cycles, distinguishing genuine competence retention from box-checking.
narrative_ontology:constraint_stakeholder(preparedness_retention__competence_reading, independent_auditors, observer,
    moderate, biographical, analytical, national).

% Budget authorities that would raise concerns if drill intensity were over-invested relative to marginal safety return, but under the competence reading their concern is muted because the resource allocation is calibrated to demonstrated skill decay rates rather than ritual frequency — they have little to object to and are largely absent from the conversation because there is no fiscal grievance to raise.
narrative_ontology:constraint_stakeholder(preparedness_retention__competence_reading, fiscal_oversight_bodies, observer,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(preparedness_retention__competence_reading, fiscal_oversight_bodies, excluded).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the genuine problem that operational skill for low-frequency, high-consequence events (major floods, storm surges) decays without live practice; drills and inspections are the mechanism by which perishable procedural and coordination knowledge is refreshed before it is needed for real.
% TRANSFER_FUNCTION: Moves staff time, training budget, and institutional attention from routine operations into scheduled live-fire exercises and inspection cycles; in return it moves demonstrated readiness and lower incident response time to the population depending on the defense system.
% ABSENT_VOICES: Fiscal oversight bodies would object if drill intensity outstripped marginal safety benefit, but under this reading the calibration against measured skill decay leaves them with little basis for objection, so they are structurally quiet rather than excluded.
% DISAPPEARANCE_RATIONALE: If the drills and inspections stopped, procedural knowledge for emergency flood response would decay within a few years — plans would remain on paper but the personnel executing them would lose the practiced coordination and judgment that separates a successful evacuation from a chaotic one. The next major event would reveal the gap directly.
% FOUNDING_PROBLEM: Institutional knowledge of how to execute a complex, multi-agency flood response degrades rapidly between rare actual disasters; without deliberate practice, agencies rediscover coordination failures during the event itself rather than beforehand.
% FOUNDING_PROBLEM_CORROBORATION: Independent auditors and academic disaster-response researchers corroborate that skill decay in low-frequency high-consequence domains is well documented outside the operating agencies' own self-assessment, and after-action reviews from actual flood events (e.g. near-miss storm surges) are used by outside reviewers to check whether drilled competence held up under real conditions.
narrative_ontology:disappearance_verdict(preparedness_retention__competence_reading, world_rearranges).
narrative_ontology:founding_problem_status(preparedness_retention__competence_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(preparedness_retention__competence_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(preparedness_retention__competence_reading, 'none', 1).
narrative_ontology:epsilon_provenance(preparedness_retention__competence_reading, 0.18, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(preparedness_retention__competence_reading_tests).
:- end_tests(preparedness_retention__competence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.18-0.22, declining slightly as verification practices mature) because the resource flow — staff time and training budget into exercises — returns a genuine, measurable increase in operational capacity rather than being captured by an administering party. Suppression is low (0.12): no coercive mechanism forces continued participation beyond the institutions' own stake in functioning during a real event. Theater ratio is deliberately kept low (0.15, drifting slightly lower) to reflect that this reading's defining structural claim is that ceremony is minimal relative to live-fire content — this is the empirical claim that would distinguish this reading from husk_reading if both were measured on the same domain. Accessibility collapse (0.3) and resistance (0.25) are moderate-low: alternatives to live exercise (e.g., tabletop-only planning) exist and are not suppressed, but the coordination benefit is compelling enough that resistance to participating is mild.
 *
 * PERSPECTIVAL GAP:
 *   Flood control operators and emergency response agencies experience the constraint as costly but functional professional obligation; the coastal population, who bear no direct participation cost, experience it purely as a background safety input they cannot verify directly. Fiscal oversight bodies, who might in a different reading see this as wasteful ritual spending, here see calibrated investment and have little structural basis to object — this is precisely the reading-specific claim: under competence_reading there is no meaningful gap between agenda-setter self-report and outside verification, unlike under husk_reading where that gap would be the whole story.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (coastal population, emergency response agencies, flood control operators) all sit near the low end of directionality because the constraint's operation subsidizes their safety and capacity rather than extracting from them. No victim group is declared under this reading — the expected structural delta explicitly notes victim is none, or at most diffuse fiscal inefficiency if over-invested, which is not evidenced here given the declining extractiveness trend.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (skill decay in low-frequency disaster response) remains live and is corroborated by parties outside the operating agencies (independent auditors, academic reviewers, near-miss after-action reports), which is exactly what distinguishes a genuinely functioning Rope from a Piton or husk-mode degradation — mandatrophy would require the founding problem to be dead while the drilling apparatus persisted, and here the corroboration chain affirmatively denies that.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    competence_verification_completeness,
    'Does the independent-auditor verification of drilled competence actually capture the dimensions of skill that matter in a real event, or does it measure what is measurable (response time, checklist completion) while missing tacit judgment under true uncertainty?',
    'Compare drilled-scenario outcomes against actual event outcomes (near-misses, real storm surge responses) over multiple cycles to see whether audited competence predicts real performance.',
    'If verification is shallow, this reading may be overstating its own low theater_ratio — some of what is coded as ''live competence'' could in fact be closer to the husk_reading''s ritual performance, without the corpus currently able to distinguish them.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(competence_verification_completeness, empirical, 'Whether audited drill metrics genuinely track real-event competence or are a proxy vulnerable to Goodharting.').

omega_variable(
    kernel_reading_selection_basis,
    'What determines whether a given jurisdiction''s preparedness apparatus should be read as competence_reading, husk_reading, or hybrid_reading — is this an empirical fact about the specific institution, or a framing choice made by whoever is doing the classifying?',
    'Cross-jurisdictional comparison using the same audited-competence metric (independent verification against real-event outcomes) would let the classification be assigned per-institution rather than assumed from the outset.',
    'If the reading choice is empirically determinable per-institution, this story''s classification only holds for jurisdictions where verification confirms live competence; applying it uncritically to all preparedness regimes would mislabel husk-mode or hybrid-mode institutions as genuine Ropes.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_selection_basis, conceptual, 'Whether the choice among the three kernel readings is an empirical finding per institution or an interpretive framing applied globally.').

omega_variable(
    over_investment_fiscal_efficiency,
    'At what point does resource allocation to drilling exceed the marginal safety benefit, converting the fiscal-efficiency ''no victim'' claim into an actual victim (taxpayers bearing avoidable cost)?',
    'Marginal cost-benefit analysis comparing drill intensity increases against measured reduction in response-time variance or casualty risk in comparable events.',
    'If over-investment is occurring, a victim group (general_taxpayers or fiscal_commons) should be added and the classification re-examined — this reading''s ''victim is none'' claim is conditional on calibration holding.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(over_investment_fiscal_efficiency, empirical, 'Whether current drill/inspection intensity is calibrated to marginal safety benefit or has drifted into over-investment.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(preparedness_retention__competence_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(prep_tr_t0, preparedness_retention__competence_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(prep_tr_t8, preparedness_retention__competence_reading, theater_ratio, 8, 0.18).
narrative_ontology:measurement(prep_tr_t16, preparedness_retention__competence_reading, theater_ratio, 16, 0.17).
narrative_ontology:measurement(prep_tr_t24, preparedness_retention__competence_reading, theater_ratio, 24, 0.16).
narrative_ontology:measurement(prep_tr_t32, preparedness_retention__competence_reading, theater_ratio, 32, 0.15).
narrative_ontology:measurement(prep_tr_t40, preparedness_retention__competence_reading, theater_ratio, 40, 0.15).

% Extraction over time
narrative_ontology:measurement(prep_be_t0, preparedness_retention__competence_reading, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(prep_be_t8, preparedness_retention__competence_reading, base_extractiveness, 8, 0.2).
narrative_ontology:measurement(prep_be_t16, preparedness_retention__competence_reading, base_extractiveness, 16, 0.19).
narrative_ontology:measurement(prep_be_t24, preparedness_retention__competence_reading, base_extractiveness, 24, 0.18).
narrative_ontology:measurement(prep_be_t32, preparedness_retention__competence_reading, base_extractiveness, 32, 0.18).
narrative_ontology:measurement(prep_be_t40, preparedness_retention__competence_reading, base_extractiveness, 40, 0.18).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(preparedness_retention__competence_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(preparedness_retention__competence_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(preparedness_retention__competence_reading, 0.12).
narrative_ontology:affects_constraint(preparedness_retention__competence_reading, preparedness_retention__husk_reading).
narrative_ontology:affects_constraint(preparedness_retention__competence_reading, preparedness_retention__hybrid_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three siblings decomposing the natural-language concept 'preparedness retention' per the ε-invariance principle. competence_reading (this story) claims low extraction, low theater, genuine skill maintenance. husk_reading claims the same observable activities are ceremonial with negligible retained competence — high theater, extraction via false assurance. hybrid_reading claims a stratified structure where technical institutions retain real competence while broader societal preparedness is ceremonial — a mixed-ε constraint with internal heterogeneity neither pure reading captures. All three are linked via affects_constraints as members of the same kernel family; none should be read as alternative measurements of a single ε.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
