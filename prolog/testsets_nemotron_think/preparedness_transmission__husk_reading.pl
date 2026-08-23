% ============================================================================
% CONSTRAINT STORY: preparedness_transmission__husk_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_preparedness_transmission__husk_reading, []).

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
 *   constraint_id: preparedness_transmission__husk_reading
 *   human_readable: Civil Defense Drill & Inspection Ritual (Husk Reading)
 *   domain: disaster_risk_management/institutional_memory/civil_defense
 *
 * SUMMARY:
 *   Civil defense systems established during the Cold War and post-1950s
 *   flood disasters created formal drill and inspection regimes to maintain
 *   readiness across generations. The husk_reading of the
 *   preparedness_transmission kernel argues that while the ritual form
 *   persists — annual exercises, checkbox inspections, compliance
 *   certificates — the operational knowledge that once animated them has
 *   hollowed out. Drills now test only pre-specified failure modes from
 *   historical scenarios; they lack adaptive capacity for novel flood
 *   dynamics (compound events, urban pluvial flooding, climate-shifted return
 *   periods). Inspection routines verify paperwork and equipment presence,
 *   not functional integration. The bureaucracy administering the system
 *   benefits from the appearance of preparedness (budget protection,
 *   legitimacy), contractors profit from ritual execution, while taxpayers,
 *   frontline responders, and at-risk populations bear the cost of a
 *   capability illusion. The constraint persists through institutional
 *   inertia and professional identity fusion — 'we are the preparedness
 *   agency' — not through active enforcement.
 *
 * KEY AGENTS:
 *   - civil_defense_bureaucracy: Primary agenda_setter (institutional/generational/arbitrage) — administers the regime, collects legitimacy/budget from ritual performance
 *   - compliance_contractors: Beneficiary (organized/biographical/mobile) — paid to design/run drills and inspections, no accountability for operational outcomes
 *   - frontline_responders: Payer (organized/biographical/constrained) — train on hollow scenarios, bear operational risk when real events deviate from scripts
 *   - at_risk_populations: Payer (powerless/generational/trapped) — believe they are protected by the system; discover gaps only during disasters
 *   - taxpayers: Payer (moderate/biographical/constrained) — fund the ritual apparatus, receive compliance theater in return
 *   - independent_auditors: Observer (analytical/generational/analytical) — commission after-action reviews that repeatedly find capability gaps
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(preparedness_transmission__husk_reading, 0.45).
domain_priors:suppression_score(preparedness_transmission__husk_reading, 0.3).
domain_priors:theater_ratio(preparedness_transmission__husk_reading, 0.78).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(preparedness_transmission__husk_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(preparedness_transmission__husk_reading, suppression_requirement, 0.3).
narrative_ontology:constraint_metric(preparedness_transmission__husk_reading, theater_ratio, 0.78).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(preparedness_transmission__husk_reading, accessibility_collapse, 0.52).
narrative_ontology:constraint_metric(preparedness_transmission__husk_reading, resistance, 0.22).

% --- Constraint claim ---
narrative_ontology:constraint_claim(preparedness_transmission__husk_reading, piton).
narrative_ontology:human_readable(preparedness_transmission__husk_reading, "Civil Defense Drill & Inspection Ritual (Husk Reading)").
narrative_ontology:topic_domain(preparedness_transmission__husk_reading, "disaster_risk_management/institutional_memory/civil_defense").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(preparedness_transmission__husk_reading, '9d8a24f9-de22-4f79-a974-5b31757e646f').
narrative_ontology:cs_kernel_codification('9d8a24f9-de22-4f79-a974-5b31757e646f', formalized).
narrative_ontology:cs_authority_grounding('9d8a24f9-de22-4f79-a974-5b31757e646f', extraction).
narrative_ontology:cs_interpretation_layer_present('9d8a24f9-de22-4f79-a974-5b31757e646f').
narrative_ontology:cs_reading_relation('9d8a24f9-de22-4f79-a974-5b31757e646f', preparedness_transmission__competence_reading, coexists_with).
narrative_ontology:cs_reading_relation('9d8a24f9-de22-4f79-a974-5b31757e646f', preparedness_transmission__hybrid_reading, influences).
narrative_ontology:cs_axiom('9d8a24f9-de22-4f79-a974-5b31757e646f', foundational, ritual_compliance_satisfies_preparedness_obligation).
narrative_ontology:cs_axiom_status(ritual_compliance_satisfies_preparedness_obligation, holdable).
narrative_ontology:cs_axiom_grounding('9d8a24f9-de22-4f79-a974-5b31757e646f', ritual_compliance_satisfies_preparedness_obligation, conventional).
narrative_ontology:cs_axiom('9d8a24f9-de22-4f79-a974-5b31757e646f', secondary, operational_knowledge_is_recoverable_from_form).
narrative_ontology:cs_axiom_status(operational_knowledge_is_recoverable_from_form, holdable).
narrative_ontology:cs_axiom_grounding('9d8a24f9-de22-4f79-a974-5b31757e646f', operational_knowledge_is_recoverable_from_form, empirically_contingent).
narrative_ontology:cs_reference_frame('9d8a24f9-de22-4f79-a974-5b31757e646f', formal_preparedness_regime).
narrative_ontology:cs_drift_state('9d8a24f9-de22-4f79-a974-5b31757e646f', contemporary_climate_flood_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('9d8a24f9-de22-4f79-a974-5b31757e646f', '2026-08-15T14:30:00Z').
narrative_ontology:cs_kernel_id(preparedness_transmission__husk_reading, preparedness_transmission).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(preparedness_transmission__husk_reading, civil_defense_bureaucracy).
narrative_ontology:constraint_beneficiary(preparedness_transmission__husk_reading, compliance_contractors).
narrative_ontology:constraint_victim(preparedness_transmission__husk_reading, taxpayers).
narrative_ontology:constraint_victim(preparedness_transmission__husk_reading, frontline_responders).
narrative_ontology:constraint_victim(preparedness_transmission__husk_reading, at_risk_populations).
narrative_ontology:constraint_vindicates(preparedness_transmission__husk_reading, preparedness_obligation_satisfied_by_ritual_compliance).
narrative_ontology:constraint_vindicates(preparedness_transmission__husk_reading, institutional_continuity_preserves_capability).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers the national civil defense drill and inspection regime. Designs exercise scenarios, sets inspection criteria, certifies compliance. Collects legitimacy and budget protection from the appearance of preparedness. Can reform or abolish the regime but chooses not to because the ritual satisfies political accountability and the cost of rebuilding genuine knowledge exceeds institutional risk tolerance.
narrative_ontology:constraint_stakeholder(preparedness_transmission__husk_reading, civil_defense_bureaucracy, agenda_setter,
    institutional, generational, arbitrage, national).

% Private firms contracted to design, execute, and evaluate drills and inspections. Paid per exercise cycle. No liability for operational outcomes — only for procedural compliance. Have mobile exit (can shift to other compliance markets) but benefit from the ritual's stable revenue stream. Their incentive is to maximize ritual complexity (more billable hours) not operational relevance.
narrative_ontology:constraint_stakeholder(preparedness_transmission__husk_reading, compliance_contractors, beneficiary,
    organized, biographical, mobile, national).

% Fire, EMS, and emergency management personnel required to participate in drills and maintain inspection readiness. Train on scripted scenarios that do not match emerging flood dynamics (compound events, flash urban flooding, cascading infrastructure failure). Bear the operational risk when real events deviate — they improvise without institutional backup. Exit is constrained: career-embedded, specialized skills, pension-locked. Some supplement with self-funded adaptive training.
narrative_ontology:constraint_stakeholder(preparedness_transmission__husk_reading, frontline_responders, payer,
    organized, biographical, constrained, regional).

% Communities in floodplains, coastal zones, and urban drainage basins who believe the civil defense system provides functional protection. Discover the capability gap only during disasters — evacuation routes untested for novel scenarios, shelters unequipped for duration, warning systems calibrated to historical not shifted thresholds. No exit: geographic, economic, and social ties bind them. Bear catastrophic consequences of the ritual's failure.
narrative_ontology:constraint_stakeholder(preparedness_transmission__husk_reading, at_risk_populations, payer,
    powerless, generational, trapped, local).

% Fund the civil defense apparatus through general revenue and targeted levies. Receive compliance certificates and public assurances. Cannot easily trace funding to operational outcomes. Exit is constrained: cannot opt out of taxation, political accountability channels are mediated by the same bureaucracy. Some pressure for reform via legislative oversight but face institutional capture.
narrative_ontology:constraint_stakeholder(preparedness_transmission__husk_reading, taxpayers, payer,
    moderate, biographical, constrained, national).

% Academic researchers, legislative auditors, and post-disaster review commissions who examine the system after events. Repeatedly document the gap between drill performance and real-world capability. Their reports are acknowledged ceremonially but not structurally acted upon — the ritual absorbs critique as 'lessons learned' without changing the scenario library. They see the full structure but lack enforcement power.
narrative_ontology:constraint_stakeholder(preparedness_transmission__husk_reading, independent_auditors, observer,
    analytical, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Originally: ensure cross-generational readiness for known flood threats through practiced, standardized response. Now: maintain institutional legitimacy and budget justification by performing the form of preparedness — the ritual coordinates internal bureaucracy and contractor relations, not operational capability.
% TRANSFER_FUNCTION: Moves funding and personnel time from taxpayers and frontline responders to civil defense bureaucracy and compliance contractors. Moves the illusion of protection to at-risk populations. Moves accountability risk from the bureaucracy (which can cite compliance) to responders (who improvise when scripts fail).
% ABSENT_VOICES: Communities that would demand adaptive preparedness if they knew the drills don't match their risk; whistleblowers within the bureaucracy who see the capability gap but face career retaliation; younger responders who never experienced the founding threats and recognize the ritual's irrelevance but lack voice in scenario design.
% DISAPPEARANCE_RATIONALE: If the drill/inspection ritual vanished overnight, the capability gap would be exposed. The bureaucracy would lose its primary legitimacy artifact. Funding would face immediate scrutiny. Responders would be forced to improvise or demand real training. At-risk populations would lose the illusion of protection, triggering political demand for functional systems. The world rearranges because the ritual is the only thing holding the current institutional configuration together.
% FOUNDING_PROBLEM: After the 1953 North Sea flood and Cold War civil defense mobilization, governments needed to maintain flood and disaster readiness across generations when direct operational experience faded. The drill/inspection regime was built to transmit tacit knowledge through standardized practice, ensuring that each cohort of responders and managers could execute proven procedures without having lived through the founding disasters.
% FOUNDING_PROBLEM_CORROBORATION: Independent after-action reports from Hurricane Katrina (2005), Central European floods (2002, 2013, 2021), and Hurricane Harvey (2017) document that scripted drills failed to prepare responders for novel flood dynamics. Academic studies on institutional memory decay in disaster agencies (e.g., Birkland 2006, Kapucu 2008, recent OECD reviews) confirm that ritual compliance correlates negatively with adaptive capacity. The civil defense bureaucracy itself attests the problem is live (citing ongoing threats), but this is self-interested corroboration; no external validator confirms the founding problem persists in its original form.
narrative_ontology:disappearance_verdict(preparedness_transmission__husk_reading, world_rearranges).
narrative_ontology:founding_problem_status(preparedness_transmission__husk_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(preparedness_transmission__husk_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(preparedness_transmission__husk_reading, 'none', 1).
narrative_ontology:epsilon_provenance(preparedness_transmission__husk_reading, 0.45, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(preparedness_transmission__husk_reading_tests).
:- end_tests(preparedness_transmission__husk_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint is claimed as piton: a former coordination mechanism (rope) whose function has atrophied, leaving mostly performance. Theater_ratio is high (0.78) and rising over 40 years — the defining metric. Extractiveness is moderate (0.45) because the system still extracts real resources (funding, personnel time, political attention) while delivering diminishing real preparedness. Suppression is low (0.30) and declining — compliance is now voluntary/ritualistic, not coerced. Accessibility_collapse is moderate (0.52): alternative preparedness models exist (adaptive management, community resilience, scenario-free training) but are marginalized by the ritual's institutional capture. Resistance is very low (0.22): few challenge the ritual because it satisfies accountability checkboxes and challenging it threatens professional identities. The temporal grid shows theater rising, extraction creeping up, suppression falling — the classic piton trajectory: enforcement decays, theater replaces function, extraction persists by inertia.
 *
 * PERSPECTIVAL GAP:
 *   The bureaucracy (agenda_setter) experiences the constraint as coordination — they built it, they run it, they see the paperwork flow. Frontline responders and at-risk populations experience it as extraction — they pay in risk and trust, receive theater. The engine computes this seat divergence from the structural data; the claimed piton type reflects the analytical observer's view that the coordination function has atrophied.
 *
 * DIRECTIONALITY LOGIC:
 *   Civil_defense_bureaucracy sits at d ≈ 0.15 (beneficiary end): collects budget/legitimacy, controls the ritual design, has arbitrage-grade exit (can reform or abolish the regime). Compliance_contractors at d ≈ 0.20: paid beneficiaries with mobile exit. Frontline_responders at d ≈ 0.75: bear risk from hollow training, constrained exit (career-embedded, cannot easily leave the system). At_risk_populations at d ≈ 0.85: trapped, no exit, bear catastrophic consequences. Taxpayers at d ≈ 0.55: moderate extraction, constrained exit (cannot opt out of funding). The directionality derivation from beneficiary/victim declarations + exit options produces this gradient; no overrides needed.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding mandate — maintain readiness across generations when direct disaster memory fades — is dead for novel flood scenarios. The ritual persists because the bureaucracy's legitimacy and budget depend on NOT admitting the mandate has expired. Mandatrophy is resolved: the arrangement is a zombie, maintained by the beneficiaries of its appearance. The piton classification captures this: no concentrated beneficiary captures the extraction (it's diffuse: budget, legitimacy, contractor fees), the administrator could change it but the cost to fix (admitting failure, rebuilding knowledge) exceeds what it bears.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_dispute,
    'Is the preparedness_transmission kernel a live exercised capability (competence_reading), a hollow ritual (husk_reading), or a stratified hybrid (hybrid_reading)?',
    'Comparative after-action analysis of novel flood events against drill scenarios; independent audit of operational knowledge retention vs. protocol compliance rates across generations.',
    'If husk_reading is structurally dominant, the constraint classifies as piton with high theater_ratio; if competence_reading holds, it classifies as rope with low theater; if hybrid_reading, the system decomposes into engineering (rope) and civilian (piton/snare) sub-constraints.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_dispute, conceptual, 'Which reading of the preparedness_transmission kernel is structurally accurate.').

omega_variable(
    residual_coordination_function,
    'Does the drill/inspection ritual retain any genuine coordination function (e.g., communications exercise, inter-agency contact maintenance) beneath the theater?',
    'Network analysis of drill participation graphs: measure whether drills create/maintain operational links that activate during real events, vs. purely ceremonial attendance.',
    'If residual coordination exists, theater_ratio overstates hollowness and the constraint may be tangled_rope (coordination + extraction) rather than piton; if zero, piton classification is reinforced.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(residual_coordination_function, empirical, 'Whether any real coordination survives the ritual form.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is compliance with the drill/inspection regime driven by structural enforcement (budget conditions, legal mandate) or internalized ritual identity (professional self-concept as ''prepared'')?',
    'Post-exit suppression trajectory: track agencies that lose mandate/funding — do they continue drills voluntarily? If yes, internalized; if they stop immediately, structural.',
    'If internalized, effective suppression is higher than structural measure suggests — the constraint persists even without external enforcement, making reform harder.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for ritual compliance.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(preparedness_transmission__husk_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pthr_tr_t0, preparedness_transmission__husk_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement(pthr_tr_t10, preparedness_transmission__husk_reading, theater_ratio, 10, 0.48).
narrative_ontology:measurement(pthr_tr_t20, preparedness_transmission__husk_reading, theater_ratio, 20, 0.62).
narrative_ontology:measurement(pthr_tr_t30, preparedness_transmission__husk_reading, theater_ratio, 30, 0.71).
narrative_ontology:measurement(pthr_tr_t40, preparedness_transmission__husk_reading, theater_ratio, 40, 0.78).

% Extraction over time
narrative_ontology:measurement(pthr_be_t0, preparedness_transmission__husk_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(pthr_be_t10, preparedness_transmission__husk_reading, base_extractiveness, 10, 0.33).
narrative_ontology:measurement(pthr_be_t20, preparedness_transmission__husk_reading, base_extractiveness, 20, 0.38).
narrative_ontology:measurement(pthr_be_t30, preparedness_transmission__husk_reading, base_extractiveness, 30, 0.42).
narrative_ontology:measurement(pthr_be_t40, preparedness_transmission__husk_reading, base_extractiveness, 40, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(pthr_su_t0, preparedness_transmission__husk_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(pthr_su_t10, preparedness_transmission__husk_reading, suppression_requirement, 10, 0.38).
narrative_ontology:measurement(pthr_su_t20, preparedness_transmission__husk_reading, suppression_requirement, 20, 0.32).
narrative_ontology:measurement(pthr_su_t30, preparedness_transmission__husk_reading, suppression_requirement, 30, 0.3).
narrative_ontology:measurement(pthr_su_t40, preparedness_transmission__husk_reading, suppression_requirement, 40, 0.3).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(preparedness_transmission__husk_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(preparedness_transmission__husk_reading, 0.12).
narrative_ontology:affects_constraint(preparedness_transmission__husk_reading, flood_response_protocol).
narrative_ontology:affects_constraint(preparedness_transmission__husk_reading, evacuation_planning).
narrative_ontology:affects_constraint(preparedness_transmission__husk_reading, early_warning_system).
narrative_ontology:affects_constraint(preparedness_transmission__husk_reading, preparedness_transmission__competence_reading).
narrative_ontology:affects_constraint(preparedness_transmission__husk_reading, preparedness_transmission__hybrid_reading).

% DUAL FORMULATION NOTE:
% The preparedness_transmission kernel decomposes into three constraint stories: competence_reading (rope — live coordination), husk_reading (piton — atrophied ritual), hybrid_reading (tangled_rope — engineering coordination + civilian extraction). This husk_reading is the civilian-coordination stratum; it affects and is affected by the competence_reading (which it undermines by crowding out real training) and the hybrid_reading (which it partially constitutes).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
