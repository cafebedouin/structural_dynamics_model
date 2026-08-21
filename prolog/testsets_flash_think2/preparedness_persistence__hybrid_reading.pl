% ============================================================================
% CONSTRAINT STORY: preparedness_persistence__hybrid_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_preparedness_persistence__hybrid_reading, []).

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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
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
 *   constraint_id: preparedness_persistence__hybrid_reading
 *   human_readable: Stratified Disaster Preparedness (Hybrid Reading)
 *   domain: disaster_preparedness/institutional_memory
 *
 * SUMMARY:
 *   This constraint describes the persistence of disaster preparedness as a
 *   stratified system, where some components (e.g., engineering inspections)
 *   maintain genuine competence, while others (e.g., certain evacuation
 *   drills) have become ritualized performances. This 'hybrid reading'
 *   acknowledges the co-existence of effective and atrophied elements within
 *   the broader preparedness framework. It is one reading of the
 *   'preparedness_persistence' kernel, which explores how societies maintain
 *   or fail to maintain readiness for crises.
 *
 * KEY AGENTS:
 *   - emergency_management_agencies: Agenda setter (institutional/constrained)
 *   - engineering_inspectors: Beneficiary (organized/mobile)
 *   - frontline_responders: Payer (moderate/constrained)
 *   - public_citizens: Payer (powerless/trapped)
 *   - budget_controllers: Beneficiary (powerful/arbitrage)
 *   - disaster_victims: Excluded (powerless/trapped)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(preparedness_persistence__hybrid_reading, 0.6).
domain_priors:suppression_score(preparedness_persistence__hybrid_reading, 0.7).
domain_priors:theater_ratio(preparedness_persistence__hybrid_reading, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(preparedness_persistence__hybrid_reading, extractiveness, 0.6).
narrative_ontology:constraint_metric(preparedness_persistence__hybrid_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(preparedness_persistence__hybrid_reading, theater_ratio, 0.65).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(preparedness_persistence__hybrid_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(preparedness_persistence__hybrid_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(preparedness_persistence__hybrid_reading, piton).
narrative_ontology:human_readable(preparedness_persistence__hybrid_reading, "Stratified Disaster Preparedness (Hybrid Reading)").
narrative_ontology:topic_domain(preparedness_persistence__hybrid_reading, "disaster_preparedness/institutional_memory").

domain_priors:requires_active_enforcement(preparedness_persistence__hybrid_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(preparedness_persistence__hybrid_reading, 'cd480006-e258-4eaf-a112-c708bf8da724').
narrative_ontology:cs_kernel_codification('cd480006-e258-4eaf-a112-c708bf8da724', formalized).
narrative_ontology:cs_authority_grounding('cd480006-e258-4eaf-a112-c708bf8da724', practice).
narrative_ontology:cs_interpretation_layer_present('cd480006-e258-4eaf-a112-c708bf8da724').
narrative_ontology:cs_reading_relation('cd480006-e258-4eaf-a112-c708bf8da724', preparedness_persistence__husk_reading, coexists_with).
narrative_ontology:cs_reading_relation('cd480006-e258-4eaf-a112-c708bf8da724', preparedness_persistence__competence_reading, coexists_with).
narrative_ontology:cs_axiom('cd480006-e258-4eaf-a112-c708bf8da724', foundational, preparedness_is_heterogeneous).
narrative_ontology:cs_axiom_status(preparedness_is_heterogeneous, holdable).
narrative_ontology:cs_axiom_grounding('cd480006-e258-4eaf-a112-c708bf8da724', preparedness_is_heterogeneous, empirically_contingent).
narrative_ontology:cs_axiom('cd480006-e258-4eaf-a112-c708bf8da724', secondary, ritual_coexists_with_competence).
narrative_ontology:cs_axiom_status(ritual_coexists_with_competence, holdable).
narrative_ontology:cs_axiom_grounding('cd480006-e258-4eaf-a112-c708bf8da724', ritual_coexists_with_competence, empirically_contingent).
narrative_ontology:cs_reference_frame('cd480006-e258-4eaf-a112-c708bf8da724', operational_readiness_standard).
narrative_ontology:cs_drift_state('cd480006-e258-4eaf-a112-c708bf8da724', contemporary, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('cd480006-e258-4eaf-a112-c708bf8da724', '').
narrative_ontology:cs_kernel_id(preparedness_persistence__hybrid_reading, preparedness_persistence).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(preparedness_persistence__hybrid_reading, emergency_management_agencies).
narrative_ontology:constraint_beneficiary(preparedness_persistence__hybrid_reading, budget_controllers).
narrative_ontology:constraint_victim(preparedness_persistence__hybrid_reading, frontline_responders).
narrative_ontology:constraint_victim(preparedness_persistence__hybrid_reading, public_citizens).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(preparedness_persistence__hybrid_reading, engineering_inspectors).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Responsible for designing, implementing, and overseeing preparedness protocols. They manage both the genuinely competent inspection systems and the ritualized drills, often balancing budget constraints with public safety mandates. They benefit from the appearance of comprehensive preparedness.
narrative_ontology:constraint_stakeholder(preparedness_persistence__hybrid_reading, emergency_management_agencies, agenda_setter,
    institutional, generational, constrained, national).

% Professionals whose work (e.g., structural integrity checks, infrastructure resilience assessments) remains genuinely competent and critical. They benefit from the continued funding and institutional support for their essential functions, which are often intertwined with the broader preparedness framework.
narrative_ontology:constraint_stakeholder(preparedness_persistence__hybrid_reading, engineering_inspectors, beneficiary,
    organized, biographical, mobile, regional).

% Police, fire, medical personnel who participate in drills and respond to actual disasters. They bear the costs of ineffective preparedness when ritualized drills fail to translate into real-world readiness, increasing their risk and workload during crises. They also benefit from genuinely effective training.
narrative_ontology:constraint_stakeholder(preparedness_persistence__hybrid_reading, frontline_responders, payer,
    moderate, immediate, constrained, local).

% The ultimate recipients of preparedness efforts, who rely on the system for safety and recovery. They pay through taxes and bear the direct costs of disaster when preparedness is inadequate, suffering loss of life, property, and livelihood. They are often unaware of the stratification.
narrative_ontology:constraint_stakeholder(preparedness_persistence__hybrid_reading, public_citizens, payer,
    powerless, immediate, trapped, local).

% Government officials who allocate funds for preparedness. They benefit from the cost-effectiveness of ritualized, less resource-intensive drills, which allow them to claim 'preparedness' without fully funding the more expensive, genuinely competent systems. They can shift resources away from preparedness without immediate political cost.
narrative_ontology:constraint_stakeholder(preparedness_persistence__hybrid_reading, budget_controllers, beneficiary,
    powerful, biographical, arbitrage, national).

% Individuals directly impacted by disasters. Their experiences often highlight the gaps between ritualized preparedness and actual resilience, but their voices are frequently marginalized in policy discussions once the immediate crisis passes.
narrative_ontology:constraint_stakeholder(preparedness_persistence__hybrid_reading, disaster_victims, excluded,
    powerless, immediate, trapped, local).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(preparedness_persistence__hybrid_reading, budget_controllers).
narrative_ontology:fixing_cost_class(preparedness_persistence__hybrid_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To coordinate diverse agencies and public actions to mitigate, prepare for, respond to, and recover from disasters, ensuring public safety and infrastructure resilience.
% TRANSFER_FUNCTION: Transfers public funds and personnel time into a system of preparedness activities, some of which build genuine capacity (e.g., engineering inspections) and others which primarily serve symbolic or bureaucratic functions (e.g., certain evacuation drills).
% ABSENT_VOICES: Independent disaster preparedness auditors focused solely on efficacy (not compliance), and the direct voices of disaster victims who experience the failures of ritualized preparedness firsthand.
% DISAPPEARANCE_RATIONALE: If all disaster preparedness vanished overnight, the world would profoundly rearrange. Essential infrastructure would fail without maintenance, emergency services would be uncoordinated, and public response to any crisis would be chaotic, leading to catastrophic loss of life and economic collapse. Even the ritualized components provide some minimal coordination and public reassurance.
% FOUNDING_PROBLEM: To protect populations and critical infrastructure from the devastating impacts of natural and man-made disasters, ensuring rapid and effective response and recovery.
% FOUNDING_PROBLEM_CORROBORATION: Emergency management agencies assert the problem is live and complex. Independent academic research and post-disaster analyses often corroborate the persistence of the problem but highlight the stratification of preparedness, indicating that some aspects are well-addressed while others remain critically deficient or performative. Public opinion is often swayed by recent disaster events.
narrative_ontology:disappearance_verdict(preparedness_persistence__hybrid_reading, world_rearranges).
narrative_ontology:founding_problem_status(preparedness_persistence__hybrid_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(preparedness_persistence__hybrid_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(preparedness_persistence__hybrid_reading, 'none', 1).
narrative_ontology:epsilon_provenance(preparedness_persistence__hybrid_reading, 0.6, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(preparedness_persistence__hybrid_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(preparedness_persistence__hybrid_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(preparedness_persistence__hybrid_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The `extractiveness` (0.60) is moderate because resources are diverted to maintain both functional and performative aspects. The `suppression` (0.70) is high as the system actively suppresses calls for more rigorous, costly, or less convenient preparedness methods in favor of maintaining the status quo. `theater_ratio` (0.65) is high, reflecting the significant portion of activity dedicated to ritualized drills and symbolic gestures that do not substantially enhance real-world readiness. `accessibility_collapse` (0.45) is moderate; while genuinely effective alternatives exist, they are often sidelined or underfunded due to the inertia of the existing stratified system. `resistance` (0.30) is low, as public awareness of the stratification is limited, and those who do resist (e.g., some frontline responders) face institutional pressure. The claimed type is 'piton' because the overall system's persistence is heavily influenced by institutional inertia and theatrical maintenance, even if some parts are genuinely functional.
 *
 * PERSPECTIVAL GAP:
 *   Emergency management agencies and budget controllers perceive the system as a necessary, if imperfect, coordination mechanism, with the ritualized elements serving to maintain public confidence and fulfill bureaucratic requirements. Frontline responders and public citizens, however, experience the consequences of the stratification, bearing the costs of inadequate real-world readiness when disasters strikes. The engine will compute these divergent experiences from the declared roles and positional atoms.
 *
 * DIRECTIONALITY LOGIC:
 *   Emergency management agencies and budget controllers are beneficiaries; the former from maintaining their institutional mandate and the latter from managing costs through performative measures. Engineering inspectors benefit from the continued funding of their genuinely competent work. Frontline responders and public citizens are payers, bearing the risks and costs of insufficient real preparedness. Disaster victims are excluded, their experiences often failing to shift the entrenched system.
 *
 * MANDATROPHY ANALYSIS:
 *   This hybrid reading directly addresses mandatrophy by acknowledging that while the original mandate (disaster protection) is still live, its execution has become stratified. Some components have atrophied into ritual (piton-like), while others remain functional (mountain/rope-like). The classification as 'piton' for the overall constraint highlights the significant role of inertia and theatrical maintenance in its persistence, preventing mislabeling the entire system as purely functional coordination or pure extraction. The 'contested' status of the founding problem further supports this nuanced view.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    competence_vs_ritual_proportion,
    'What is the precise proportion of genuinely competent preparedness components versus ritualized, performative components within the overall system?',
    'Comprehensive, independent, and outcome-based audits of all preparedness activities, measuring actual resilience improvements rather than compliance with procedures.',
    'A higher proportion of ritualized components would increase the effective extractiveness and theater ratio, pushing the classification further towards Snare or a more severe Piton. A higher proportion of competence would shift it towards Rope or even Mountain for those specific components.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(competence_vs_ritual_proportion, empirical, 'Quantifying the balance between functional and performative preparedness.').

omega_variable(
    reading_framing_impact,
    'How would the classification of ''preparedness_persistence'' change if the ''competence_reading'' or ''husk_reading'' were adopted as the dominant frame?',
    'Analysis of the structural implications of each sibling reading''s core axioms on the metrics and stakeholder positions, as computed by the engine.',
    'The ''competence_reading'' would likely yield a Rope or Mountain classification with lower extraction and theater. The ''husk_reading'' would likely yield a Snare or more severe Piton, with higher extraction and theater, and more pronounced victimhood.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_framing_impact, conceptual, 'Impact of alternative kernel readings on constraint classification.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression of alternative preparedness methods structural (e.g., budget limitations, bureaucratic inertia) or internalized (e.g., belief in the efficacy of current rituals despite evidence)?',
    'Post-audit behavioral analysis: if agencies continue to resist changes after clear evidence of ritual inefficacy, reclassify as partially internalized suppression.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests, as the resistance to change comes from within the system''s own beliefs.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism in preparedness.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(preparedness_persistence__hybrid_reading, 1990, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(prep_tr_t1990, preparedness_persistence__hybrid_reading, theater_ratio, 1990, 0.4).
narrative_ontology:measurement(prep_tr_t1995, preparedness_persistence__hybrid_reading, theater_ratio, 1995, 0.48).
narrative_ontology:measurement(prep_tr_t2000, preparedness_persistence__hybrid_reading, theater_ratio, 2000, 0.55).
narrative_ontology:measurement(prep_tr_t2005, preparedness_persistence__hybrid_reading, theater_ratio, 2005, 0.6).
narrative_ontology:measurement(prep_tr_t2010, preparedness_persistence__hybrid_reading, theater_ratio, 2010, 0.65).
narrative_ontology:measurement(prep_tr_t2015, preparedness_persistence__hybrid_reading, theater_ratio, 2015, 0.68).
narrative_ontology:measurement(prep_tr_t2020, preparedness_persistence__hybrid_reading, theater_ratio, 2020, 0.65).
narrative_ontology:measurement(prep_tr_t2025, preparedness_persistence__hybrid_reading, theater_ratio, 2025, 0.65).

% Extraction over time
narrative_ontology:measurement(prep_be_t1990, preparedness_persistence__hybrid_reading, base_extractiveness, 1990, 0.45).
narrative_ontology:measurement(prep_be_t1995, preparedness_persistence__hybrid_reading, base_extractiveness, 1995, 0.5).
narrative_ontology:measurement(prep_be_t2000, preparedness_persistence__hybrid_reading, base_extractiveness, 2000, 0.55).
narrative_ontology:measurement(prep_be_t2005, preparedness_persistence__hybrid_reading, base_extractiveness, 2005, 0.58).
narrative_ontology:measurement(prep_be_t2010, preparedness_persistence__hybrid_reading, base_extractiveness, 2010, 0.6).
narrative_ontology:measurement(prep_be_t2015, preparedness_persistence__hybrid_reading, base_extractiveness, 2015, 0.62).
narrative_ontology:measurement(prep_be_t2020, preparedness_persistence__hybrid_reading, base_extractiveness, 2020, 0.6).
narrative_ontology:measurement(prep_be_t2025, preparedness_persistence__hybrid_reading, base_extractiveness, 2025, 0.6).

% Suppression requirement over time
narrative_ontology:measurement(prep_su_t1990, preparedness_persistence__hybrid_reading, suppression_requirement, 1990, 0.55).
narrative_ontology:measurement(prep_su_t1995, preparedness_persistence__hybrid_reading, suppression_requirement, 1995, 0.6).
narrative_ontology:measurement(prep_su_t2000, preparedness_persistence__hybrid_reading, suppression_requirement, 2000, 0.65).
narrative_ontology:measurement(prep_su_t2005, preparedness_persistence__hybrid_reading, suppression_requirement, 2005, 0.68).
narrative_ontology:measurement(prep_su_t2010, preparedness_persistence__hybrid_reading, suppression_requirement, 2010, 0.7).
narrative_ontology:measurement(prep_su_t2015, preparedness_persistence__hybrid_reading, suppression_requirement, 2015, 0.72).
narrative_ontology:measurement(prep_su_t2020, preparedness_persistence__hybrid_reading, suppression_requirement, 2020, 0.7).
narrative_ontology:measurement(prep_su_t2025, preparedness_persistence__hybrid_reading, suppression_requirement, 2025, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(preparedness_persistence__hybrid_reading, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
