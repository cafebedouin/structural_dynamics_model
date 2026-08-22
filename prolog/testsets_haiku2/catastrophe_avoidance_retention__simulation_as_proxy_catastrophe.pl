% ============================================================================
% CONSTRAINT STORY: catastrophe_avoidance_retention__simulation_as_proxy_catastrophe
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, []).

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
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: catastrophe_avoidance_retention__simulation_as_proxy_catastrophe
 *   human_readable: Simulation as Proxy Catastrophe for Competence Maintenance
 *   domain: safety_engineering/organizational_learning/regulatory_enforcement
 *
 * SUMMARY:
 *   This constraint instantiates the reading that high-fidelity simulation
 *   constitutes genuine practice equivalent to real catastrophic events for
 *   maintaining critical competence in aviation, nuclear operations,
 *   medicine, and maritime domains. Under this reading, organizations can
 *   maintain staff competence through scheduled, controlled simulation
 *   exercises rather than depending on rare, chaotic real catastrophes.
 *   Regulatory bodies codify simulation completion as the competence
 *   standard; practitioners are required to pass simulations; simulation
 *   infrastructure operators design curricula and claim functional
 *   equivalence to real catastrophes. The constraint benefits simulation
 *   operators and regulatory bodies (lower costs, predictable enforcement)
 *   and benefits well-resourced organizations (reliable competence
 *   maintenance without waiting for incidents); it extracts from
 *   practitioners (mandatory time away from work, competence judged by proxy
 *   metrics) and from organizations without simulation access (trapped in
 *   lower-licensing status). This reading coexists with the
 *   catastrophe_as_necessary_selector reading (which holds that only real
 *   catastrophes drive adequate learning) and influences the
 *   hybrid_near_miss_learning reading (which treats simulation as one
 *   component of a larger learning ecology, not a sufficient substitute).
 *
 * KEY AGENTS:
 *   - simulation_infrastructure_operators: Institutional beneficiary, sets and maintains the simulation-as-proxy standard (arbitrage exit)
 *   - regulatory_oversight_bodies: Institutional beneficiary and agenda-setter, codify simulation compliance (mobile exit)
 *   - field_practitioners: Moderate-power payers, constrained by licensure requirements (identity-locked exit)
 *   - organizations_without_simulation_capacity: Powerless victims, trapped in lower-licensing status (trapped exit)
 *   - organizations_maintaining_competence: Organized beneficiaries, coordinate predictable competence cycles (mobile exit)
 *   - catastrophe_researchers: Analytical observers, study fidelity gaps
 *   - catastrophe_survivors_and_casualties: Excluded powerless parties bearing real-world competence gaps
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, 0.62).
domain_priors:suppression_score(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, 0.41).
domain_priors:theater_ratio(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, extractiveness, 0.62).
narrative_ontology:constraint_metric(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, suppression_requirement, 0.41).
narrative_ontology:constraint_metric(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, accessibility_collapse, 0.71).
narrative_ontology:constraint_metric(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, tangled_rope).
narrative_ontology:human_readable(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, "Simulation as Proxy Catastrophe for Competence Maintenance").
narrative_ontology:topic_domain(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, "safety_engineering/organizational_learning/regulatory_enforcement").

domain_priors:requires_active_enforcement(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, 'a56f3eaf-ebea-4eae-9514-7912119b94bf').
narrative_ontology:cs_kernel_codification('a56f3eaf-ebea-4eae-9514-7912119b94bf', fixed_text).
narrative_ontology:cs_authority_grounding('a56f3eaf-ebea-4eae-9514-7912119b94bf', extraction).
narrative_ontology:cs_interpretation_layer_present('a56f3eaf-ebea-4eae-9514-7912119b94bf').
narrative_ontology:cs_reading_relation('a56f3eaf-ebea-4eae-9514-7912119b94bf', catastrophe_avoidance_retention__catastrophe_as_necessary_selector, coexists_with).
narrative_ontology:cs_reading_relation('a56f3eaf-ebea-4eae-9514-7912119b94bf', catastrophe_avoidance_retention__hybrid_near_miss_learning, influences).
narrative_ontology:cs_axiom('a56f3eaf-ebea-4eae-9514-7912119b94bf', foundational, simulation_functional_equivalence_to_catastrophe).
narrative_ontology:cs_axiom_status(simulation_functional_equivalence_to_catastrophe, holdable).
narrative_ontology:cs_axiom_grounding('a56f3eaf-ebea-4eae-9514-7912119b94bf', simulation_functional_equivalence_to_catastrophe, empirically_contingent).
narrative_ontology:cs_axiom('a56f3eaf-ebea-4eae-9514-7912119b94bf', secondary, scheduled_drills_sufficient_for_competence_maintenance).
narrative_ontology:cs_axiom_status(scheduled_drills_sufficient_for_competence_maintenance, holdable).
narrative_ontology:cs_axiom_grounding('a56f3eaf-ebea-4eae-9514-7912119b94bf', scheduled_drills_sufficient_for_competence_maintenance, empirically_contingent).
narrative_ontology:cs_reference_frame('a56f3eaf-ebea-4eae-9514-7912119b94bf', competence_maintenance_via_reproducible_simulation).
narrative_ontology:cs_drift_state('a56f3eaf-ebea-4eae-9514-7912119b94bf', contemporary_regulatory_codification, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('a56f3eaf-ebea-4eae-9514-7912119b94bf', '').
narrative_ontology:cs_kernel_id(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, catastrophe_avoidance_retention).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, simulation_infrastructure_operators).
narrative_ontology:constraint_beneficiary(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, regulatory_oversight_bodies).
narrative_ontology:constraint_beneficiary(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, organizations_avoiding_catastrophe).
narrative_ontology:constraint_victim(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, field_practitioners).
narrative_ontology:constraint_victim(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, organizations_without_simulation_capacity).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, organizations_maintaining_competence).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Design, build, and maintain high-fidelity simulation systems (nuclear control rooms, flight decks, surgical suites, maritime disaster response). Maintain curriculum standards and claim that simulation exposure is equivalent to catastrophic-event training. Control access to competence validation through simulation-based certification. Collect licensing fees and training contracts from organizations that must maintain staff competence.
narrative_ontology:constraint_stakeholder(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, simulation_infrastructure_operators, agenda_setter,
    institutional, generational, arbitrage, national).

% Accept simulation completion as sufficient evidence of competence maintenance; codify simulation hours as the compliance metric in licensing and recertification rules. Reduce the catastrophic risk burden on their inspection/accident-investigation systems by distributing competence maintenance to scheduled drills rather than waiting for real incidents. Benefit from lower political cost of regulation-by-proxy: simulation standards are easier to enforce than live-incident learning and less politically charged than accepting actual catastrophes as training events.
narrative_ontology:constraint_stakeholder(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, regulatory_oversight_bodies, agenda_setter,
    institutional, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, regulatory_oversight_bodies, beneficiary).

% Required to maintain competence through periodic simulation exercises in order to keep licensure. Debate whether simulation fidelity truly captures the cognitive, emotional, and social pressures of real catastrophic events. Carry the cost of time away from productive work for mandatory drills. Subject to competence validation based on simulation performance rather than demonstrated real-world problem-solving under genuine stakes. Exit constraints: practitioners must remain in the profession (identity fusion) and cannot unilaterally reject simulation as a training modality.
narrative_ontology:constraint_stakeholder(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, field_practitioners, payer,
    moderate, biographical, constrained, national).

% Cannot afford or physically access high-fidelity simulation infrastructure (rural hospitals, small maritime operators, emerging-economy nuclear programs). Required by regulation to maintain staff competence via simulation they cannot access, or forced to accept lower licensing status or operational restrictions. Bear the cost of capital investment in simulation infrastructure or accept permanent operational constraints.
narrative_ontology:constraint_stakeholder(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, organizations_without_simulation_capacity, payer,
    powerless, immediate, trapped, local).

% Benefit from a standardized, scheduled, controllable mechanism for competence maintenance. Can predict and plan training cycles without waiting for real catastrophes. Staff are regularly exposed to high-stress decision-making, maintaining procedural automaticity and decision trees. Organizations with access to simulation can maintain competence reliably and claim lower catastrophic risk profiles, which improves insurance terms, regulatory standing, and public confidence.
narrative_ontology:constraint_stakeholder(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, organizations_maintaining_competence, beneficiary,
    organized, generational, mobile, global).

% Study whether actual and simulated catastrophe training produce equivalent competence outcomes. Investigate whether simulation fidelity gaps (missing elements of chaos, mortality salience, organizational trauma response) create systematic blind spots in practitioner competence. May find that simulation alone is insufficient or that specific competence domains require actual-incident learning.
narrative_ontology:constraint_stakeholder(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, catastrophe_researchers, observer,
    analytical, generational, analytical, global).

% Bear the cost of competence gaps in real catastrophic events. Excluded from the constraint-setting process; their participation would reveal whether simulation-trained practitioners actually perform adequately during real catastrophes. No voice in regulatory standards or organizational training decisions.
narrative_ontology:constraint_stakeholder(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, catastrophe_survivors_and_casualties, excluded,
    powerless, immediate, trapped, local).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, simulation_infrastructure_operators).
narrative_ontology:fixing_cost_class(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a scheduled, repeatable, low-stakes mechanism for maintaining critical competence in high-reliability domains (aviation, nuclear, medicine, maritime) that would otherwise depend on rare, chaotic, high-mortality events to drive learning.
% TRANSFER_FUNCTION: Moves time, attention, capital investment, and professional development resources from practitioners and organizations into simulation infrastructure operations and regulatory compliance management. Organizations pay for access to simulation, practitioners pay in time and cognitive effort, regulatory bodies invest in standard-setting and monitoring.
% ABSENT_VOICES: Catastrophe survivors, casualties, families of the deceased, and operators in low-resource settings. Practitioners who believe competence requires real-world stakes and who experience simulation as theatre would argue for hybrid or live-incident models if present in the room.
% DISAPPEARANCE_RATIONALE: If this constraint vanished and simulation was no longer regulatory-sufficient for competence maintenance, organizations would either wait for real catastrophes to train staff (unacceptable politically and ethically) or develop alternative distributed learning systems (near-miss networks, foreign-incident debriefs, apprenticeship structures). The competence-maintenance economy would reorganize around live-incident learning or hybrid models.
% FOUNDING_PROBLEM: High-reliability organizations cannot depend on rare catastrophic events to maintain critical staff competence; they need predictable, repeatable, low-cost, low-stakes training mechanisms that capture the essential decision-making and procedural demands of catastrophe response without requiring actual catastrophes.
% FOUNDING_PROBLEM_CORROBORATION: Catastrophe researchers and high-reliability practitioners attest that competence decay is real and that organizations need training mechanisms beyond occasional incident response. Simulation operators and regulatory bodies (the constraint beneficiaries) claim the founding problem remains live. Independent accident investigations (NTSB, ICAO, nuclear regulators outside the simulation industry) attest that practitioner competence is a live problem; their published findings do NOT endorse simulation-only training as sufficient in some domains.
narrative_ontology:disappearance_verdict(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, world_rearranges).
narrative_ontology:founding_problem_status(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, 'none', 1).
narrative_ontology:epsilon_provenance(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, 0.62, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness reaches 0.62 because the constraint transfers substantial value to simulation operators and regulatory bodies while imposing mandatory participation costs on practitioners. Early in the interval (t=0-10), extractiveness is lower (0.48-0.56) because the simulation infrastructure is still being built and the compliance infrastructure is not yet firm; as simulation becomes mandated and integrated into licensing (t=15-30), extractiveness rises and plateaus (0.60-0.62) because the constraint is now locked into regulatory enforcement. Theater ratio rises from 0.32 to 0.48 over the interval, reflecting a growing gap between the public claim (simulation equals real competence) and the institutional reality (simulation is increasingly used as a proxy metric and cost-management tool). Suppression requirement is moderate (0.35-0.41) because the constraint relies primarily on regulatory enforcement rather than coercion; practitioners accept the rule because licensure is identity-locked and the organizational alternatives are constrained. The time measurements capture the settling of the constraint from an optional/innovative tool into mandatory regulatory infrastructure.
 *
 * PERSPECTIVAL GAP:
 *   The mandate/beneficiary divergence is stark: simulation operators claim they are solving a genuine coordination problem (organizations need reproducible competence maintenance without depending on catastrophes). Practitioners and excluded catastrophe voices dispute whether simulation fidelity is sufficient — they argue that real catastrophes carry irreducible elements (mortality salience, organizational chaos, genuine uncertainty about outcome) that simulation cannot replicate. The constraint persists because regulatory bodies have adopted the simulation-as-proxy standard and made it mandatory for licensing; the practical alternative (living with catastrophe-driven learning or accepting lower competence standards) is politically unacceptable. The gap is structural: the reading that is instantiated here (simulation is sufficient) is challenged by a live alternative reading (only catastrophes drive adequate competence) but enforcement moves the constraint toward compliance regardless.
 *
 * DIRECTIONALITY LOGIC:
 *   Simulation operators are beneficiaries with institutional power and arbitrage exit (can work across domains, can pivot to other simulation uses) — derived d near 0.1-0.2 (beneficiary end). Regulatory bodies are institutional beneficiaries with mobile exit (can shift oversight strategies) — derived d near 0.2-0.3. Practitioners are moderate-power payers with identity-locked exit (professional identity fused with field practice, cannot leave without career reset) — derived d near 0.75-0.85 (target end). Organizations without simulation capacity are powerless victims with trapped exit (no alternative for maintaining operational status) — derived d near 0.9 (full target). Organizations maintaining competence are organized beneficiaries with mobile exit (can shift training strategies, can self-develop competence systems) — derived d near 0.4-0.5 (symmetric to slightly beneficiary). Catastrophe survivors are excluded; their directionality is unmeasured because they are not stakeholders in the constraint-setting process itself.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint avoids simple false-mandatrophy: the founding problem (competence decay in high-reliability systems) remains live and is actively being addressed by the constraint. However, a secondary mandatrophy pathway is present: the constraint may be shifting from coordination (organizations need reliable training) to extraction (simulation operators maintain market demand by insisting simulation hours must continually increase, regulatory bodies benefit from the proxy standard reducing accident-investigation burden). The theater_ratio rise (0.32 to 0.48) over the interval suggests increasing performativity — the constraint is becoming more about demonstrating compliance with simulation hours than about genuine competence maintenance. The measurement series captures a transition from genuine coordination (early in interval, when simulation is novel and practitioners engage it seriously) to partial extraction (later, when simulation becomes obligatory and practitioners view it as theater). A full mandatrophy would require the founding problem to die (competence decay stops being a threat) while the constraint persists; this constraint has not reached that state yet, but the trajectory is visible.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    simulation_fidelity_gap_unobserved,
    'Does high-fidelity simulation actually capture the irreducible elements of catastrophic-event learning (mortality salience, organizational trauma response, genuine uncertainty about outcome, mass-casualty triage under chaos)?',
    'Comparative outcome analysis: track competence of practitioners trained purely by simulation vs. those trained through hybrid models (simulation + foreign-incident debriefs + near-miss learning) when real catastrophes occur. Document performance under real-world stress.',
    'If simulation fidelity is sufficient, the reading''s equivalence claim holds and the constraint is coordination. If significant gaps exist, simulation is a partial proxy and the constraint is extractive (operators maintain market demand by insisting simulation hours must increase indefinitely). Classification could shift from tangled_rope toward snare if fidelity gaps are systematic.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(simulation_fidelity_gap_unobserved, empirical, 'Whether simulation captures the essential learning elements of real catastrophes').

omega_variable(
    regulatory_capture_via_proxy_metric,
    'Do regulatory bodies maintain the simulation-as-proxy standard because it genuinely serves competence maintenance, or because it reduces their political and operational burden (fewer accident investigations, lower catastrophe-related liability)?',
    'Regulatory capture analysis: trace decision-making pathways in standard-setting bodies; document whether independent accident investigation findings influence simulation standards, or whether simulation standards remain fixed regardless of real-world competence gaps revealed by incidents.',
    'If regulatory bodies are captured by the simulation industry, the constraint becomes primarily extraction (benefiting operators) wearing coordination clothing (benefiting organizations). The suppression requirement would be revealed as masking institutional alignment rather than genuine enforcement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_capture_via_proxy_metric, empirical, 'Whether regulatory adoption of simulation-as-proxy serves competence or regulatory simplification').

omega_variable(
    reading_versus_sibling_foreclosure,
    'Do the catastrophe_as_necessary_selector and simulation_as_proxy_catastrophe readings foreclose each other, or can they coexist within different institutional frameworks?',
    'Institutional ethnography: document whether organizations and regulatory bodies hold both readings simultaneously (simulation for maintenance, incident learning for update) or whether they treat them as mutually exclusive. Track whether evidence from catastrophe research influences organizational practice or remains compartmentalized.',
    'If they coexist (different domains, different organizations adopting different readings), the relationship is coexists_with. If one reading actively forecloses the other in practice (simulation adoption makes incident-learning frameworks disappear), the relationship is influences or forecloses. This shifts the characterization of the whole constraint kernel.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_versus_sibling_foreclosure, conceptual, 'Whether the readings are logically separable or mutually foreclosing').

omega_variable(
    access_equity_and_powerless_entrapment,
    'Does regulatory codification of simulation-as-proxy systematically exclude powerless organizations (rural hospitals, small maritime operators, emerging-economy nuclear programs) from competence-maintenance pathways they can afford?',
    'Mapping of simulation infrastructure distribution and regulatory exemption policies; tracking of licensing-status constraints and operational restrictions imposed on non-simulation-compliant organizations. Document whether alternative competence pathways (apprenticeship, foreign-incident learning networks) are permitted as regulatory substitutes.',
    'If simulation becomes the ONLY legitimate competence pathway and access is not equitably distributed, the constraint becomes a snare (extraction from powerless organizations that cannot meet the standard). If alternative pathways are permitted, the constraint is tangled rope with distributed access costs. This directly affects the classification from different seats (powerless operators see snare; well-resourced operators see tangled rope).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(access_equity_and_powerless_entrapment, empirical, 'Whether simulation-as-proxy exacerbates or bridges competence-maintenance equity gaps').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cata_tr_t0, catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, theater_ratio, 0, 0.32).
narrative_ontology:measurement_basis(cata_tr_t0, observed).
narrative_ontology:measurement(cata_tr_t5, catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, theater_ratio, 5, 0.36).
narrative_ontology:measurement_basis(cata_tr_t5, observed).
narrative_ontology:measurement(cata_tr_t10, catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, theater_ratio, 10, 0.4).
narrative_ontology:measurement_basis(cata_tr_t10, observed).
narrative_ontology:measurement(cata_tr_t15, catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, theater_ratio, 15, 0.43).
narrative_ontology:measurement_basis(cata_tr_t15, observed).
narrative_ontology:measurement(cata_tr_t20, catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, theater_ratio, 20, 0.46).
narrative_ontology:measurement_basis(cata_tr_t20, observed).
narrative_ontology:measurement(cata_tr_t25, catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, theater_ratio, 25, 0.48).
narrative_ontology:measurement_basis(cata_tr_t25, observed).
narrative_ontology:measurement(cata_tr_t30, catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, theater_ratio, 30, 0.48).
narrative_ontology:measurement_basis(cata_tr_t30, observed).
narrative_ontology:measurement(cata_tr_t40, catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, theater_ratio, 40, 0.48).
narrative_ontology:measurement_basis(cata_tr_t40, projected).

% Extraction over time
narrative_ontology:measurement(cata_be_t0, catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, base_extractiveness, 0, 0.48).
narrative_ontology:measurement_basis(cata_be_t0, observed).
narrative_ontology:measurement(cata_be_t5, catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, base_extractiveness, 5, 0.52).
narrative_ontology:measurement_basis(cata_be_t5, observed).
narrative_ontology:measurement(cata_be_t10, catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, base_extractiveness, 10, 0.56).
narrative_ontology:measurement_basis(cata_be_t10, observed).
narrative_ontology:measurement(cata_be_t15, catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, base_extractiveness, 15, 0.58).
narrative_ontology:measurement_basis(cata_be_t15, observed).
narrative_ontology:measurement(cata_be_t20, catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, base_extractiveness, 20, 0.6).
narrative_ontology:measurement_basis(cata_be_t20, observed).
narrative_ontology:measurement(cata_be_t25, catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, base_extractiveness, 25, 0.61).
narrative_ontology:measurement_basis(cata_be_t25, observed).
narrative_ontology:measurement(cata_be_t30, catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, base_extractiveness, 30, 0.62).
narrative_ontology:measurement_basis(cata_be_t30, observed).
narrative_ontology:measurement(cata_be_t40, catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, base_extractiveness, 40, 0.62).
narrative_ontology:measurement_basis(cata_be_t40, projected).

% Suppression requirement over time
narrative_ontology:measurement(cata_su_t0, catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, suppression_requirement, 0, 0.35).
narrative_ontology:measurement_basis(cata_su_t0, observed).
narrative_ontology:measurement(cata_su_t5, catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, suppression_requirement, 5, 0.37).
narrative_ontology:measurement_basis(cata_su_t5, observed).
narrative_ontology:measurement(cata_su_t10, catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, suppression_requirement, 10, 0.38).
narrative_ontology:measurement_basis(cata_su_t10, observed).
narrative_ontology:measurement(cata_su_t15, catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, suppression_requirement, 15, 0.39).
narrative_ontology:measurement_basis(cata_su_t15, observed).
narrative_ontology:measurement(cata_su_t20, catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, suppression_requirement, 20, 0.4).
narrative_ontology:measurement_basis(cata_su_t20, observed).
narrative_ontology:measurement(cata_su_t25, catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, suppression_requirement, 25, 0.41).
narrative_ontology:measurement_basis(cata_su_t25, observed).
narrative_ontology:measurement(cata_su_t30, catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, suppression_requirement, 30, 0.41).
narrative_ontology:measurement_basis(cata_su_t30, observed).
narrative_ontology:measurement(cata_su_t40, catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, suppression_requirement, 40, 0.41).
narrative_ontology:measurement_basis(cata_su_t40, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, 0.12).
narrative_ontology:affects_constraint(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, catastrophe_as_necessary_selector).
narrative_ontology:affects_constraint(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, hybrid_near_miss_learning).

% DUAL FORMULATION NOTE:
% This constraint (simulation_as_proxy_catastrophe) is one reading within the kernel catastrophe_avoidance_retention. It instantiates the claim that simulation is sufficient for competence maintenance. The sibling reading catastrophe_as_necessary_selector instantiates the opposite claim (only real catastrophes suffice). The sibling reading hybrid_near_miss_learning occupies a middle ground (both simulation and incident learning necessary). All three readings share the same referent — how competence is maintained in high-reliability organizations — but instantiate different structural arrangements for training, enforcement, and validation. They are NOT different observable measurements of one constraint; they are structurally distinct constraints grounded in different axioms about what constitutes genuine competence. Each has its own ε (this reading: 0.62 extractiveness; the catastrophe reading: lower extractiveness but higher suppression; the hybrid reading: distributed costs, moderate extraction). They are linked via network.affects_constraints because adoption of this reading (simulation-as-proxy) creates structural pressure on the alternatives (catastrophe reading loses legitimacy; hybrid reading must justify itself against simulation's lower cost).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, powerless, 0.92).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
