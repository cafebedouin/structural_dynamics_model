% ============================================================================
% CONSTRAINT STORY: preparedness_transmission__husk_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
    narrative_ontology:suppression_profile/2,
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
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
 *   constraint_id: preparedness_transmission__husk_reading
 *   human_readable: Civil Defense Drill-and-Inspection Cycle as Memorial Ritual (Husk Reading)
 *   domain: disaster risk management / institutional memory / civil defense
 *
 * SUMMARY:
 *   A national civil defense system runs a full annual cycle of
 *   flood-response drills and readiness inspections. Every unit completes its
 *   scripted exercises, every checklist closes, every report files on time,
 *   and the compliance statistics are flawless. Under this reading of the
 *   system, that perfection is the symptom: the scenario library froze
 *   decades ago, the veteran cohort that carried live craft knowledge has
 *   retired without a replacement pathway, and the cycle now reproduces its
 *   own documentation rather than any unit's ability to handle water that
 *   arrives off-script. The ceremony is performed at full intensity; what it
 *   was built to transmit has quietly drained out of it. The arrangement
 *   persists because dismantling it would require someone to stand up and
 *   declare three decades of certifications unreliable, and no seat bears
 *   enough daily pain to volunteer. KEY AGENTS (by structural relationship):
 *   - civil_defense_directorate: agenda-setter
 *   (institutional/identity_locked) — owns the drill canon, certifies
 *   readiness, and reproduces its own leadership through the ritual cycle -
 *   government_oversight_ministry: beneficiary (institutional/mobile) —
 *   receives legible reassurance and funds the cycle -
 *   municipal_emergency_planners: payer with secondary beneficiary position
 *   (moderate/constrained) — executes the cycle, draws task-clarity and
 *   protected budget lines from it - frontline_response_units: payer
 *   (organized/constrained) — surrender training hours to a frozen scenario
 *   library - flood_exposed_communities: payer (powerless/constrained) — bear
 *   the unpriced difference between rehearsed and actual floods -
 *   retired_operational_veterans: excluded (moderate/trapped) — hold the
 *   displaced craft knowledge, with no channel back into scenario design -
 *   national_audit_office: observer (institutional/analytical) — inspects
 *   process, structurally blind to capability
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(preparedness_transmission__husk_reading, 0.52).
domain_priors:suppression_score(preparedness_transmission__husk_reading, 0.35).
domain_priors:theater_ratio(preparedness_transmission__husk_reading, 0.82).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(preparedness_transmission__husk_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(preparedness_transmission__husk_reading, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(preparedness_transmission__husk_reading, theater_ratio, 0.82).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(preparedness_transmission__husk_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(preparedness_transmission__husk_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(preparedness_transmission__husk_reading, piton).
narrative_ontology:human_readable(preparedness_transmission__husk_reading, "Civil Defense Drill-and-Inspection Cycle as Memorial Ritual (Husk Reading)").
narrative_ontology:topic_domain(preparedness_transmission__husk_reading, "disaster risk management / institutional memory / civil defense").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(preparedness_transmission__husk_reading, '1b689982-eec0-4066-9150-265db02ed7fe').
narrative_ontology:cs_kernel_codification('1b689982-eec0-4066-9150-265db02ed7fe', formalized).
narrative_ontology:cs_authority_grounding('1b689982-eec0-4066-9150-265db02ed7fe', lineage).
narrative_ontology:cs_interpretation_layer_present('1b689982-eec0-4066-9150-265db02ed7fe').
narrative_ontology:cs_reading_relation('1b689982-eec0-4066-9150-265db02ed7fe', preparedness_transmission__competence_reading, coexists_with).
narrative_ontology:cs_reading_relation('1b689982-eec0-4066-9150-265db02ed7fe', preparedness_transmission__hybrid_reading, coexists_with).
narrative_ontology:cs_axiom('1b689982-eec0-4066-9150-265db02ed7fe', foundational, ritual_performance_does_not_transmit_capability).
narrative_ontology:cs_axiom_status(ritual_performance_does_not_transmit_capability, holdable).
narrative_ontology:cs_axiom_grounding('1b689982-eec0-4066-9150-265db02ed7fe', ritual_performance_does_not_transmit_capability, empirically_contingent).
narrative_ontology:cs_axiom('1b689982-eec0-4066-9150-265db02ed7fe', foundational, compliance_artifacts_are_not_evidence_of_readiness).
narrative_ontology:cs_axiom_status(compliance_artifacts_are_not_evidence_of_readiness, holdable).
narrative_ontology:cs_axiom_grounding('1b689982-eec0-4066-9150-265db02ed7fe', compliance_artifacts_are_not_evidence_of_readiness, empirically_contingent).
narrative_ontology:cs_reference_frame('1b689982-eec0-4066-9150-265db02ed7fe', live_apprenticeship_transmission_regime).
narrative_ontology:cs_drift_state('1b689982-eec0-4066-9150-265db02ed7fe', contemporary, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('1b689982-eec0-4066-9150-265db02ed7fe', '').
narrative_ontology:cs_kernel_id(preparedness_transmission__husk_reading, preparedness_transmission).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(preparedness_transmission__husk_reading, civil_defense_directorate).
narrative_ontology:constraint_beneficiary(preparedness_transmission__husk_reading, government_oversight_ministry).
narrative_ontology:constraint_beneficiary(preparedness_transmission__husk_reading, municipal_emergency_planners).
narrative_ontology:constraint_victim(preparedness_transmission__husk_reading, municipal_emergency_planners).
narrative_ontology:constraint_victim(preparedness_transmission__husk_reading, frontline_response_units).
narrative_ontology:constraint_victim(preparedness_transmission__husk_reading, flood_exposed_communities).
narrative_ontology:constraint_vindicates(preparedness_transmission__husk_reading, documented_routine_equals_readiness_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Owns the national drill calendar, the inspection protocols, and the manual that defines a compliant exercise. Certifies municipal readiness on the basis of completed drills and filed reports. Its leadership succession is trained inside the same ritual cycle, and its public identity is bound to the visible continuity of the ceremony it administers. Redirecting the cycle toward unscripted capability testing would require declaring its own accumulated certifications unreliable.
narrative_ontology:constraint_stakeholder(preparedness_transmission__husk_reading, civil_defense_directorate, agenda_setter,
    institutional, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(preparedness_transmission__husk_reading, civil_defense_directorate, beneficiary).

% Funds the civil defense system and answers parliamentary questions about flood readiness using the directorate's compliance statistics. Receives steady, legible evidence that mandated activity is occurring, which is what its accountability loop requires. It could redirect funding toward adversarial capability testing but has no electoral incentive to trade reassuring metrics for uncomfortable ones.
narrative_ontology:constraint_stakeholder(preparedness_transmission__husk_reading, government_oversight_ministry, beneficiary,
    institutional, generational, mobile, national).

% Schedule and run the exercises, complete the checklists, and file the reports their superiors and the audit office consume. The cycle gives them unambiguous annual targets and protected budget lines. Deviating toward improvised scenarios risks failed inspections and career marks, so they optimize for protocol fidelity regardless of what the local flood record suggests.
narrative_ontology:constraint_stakeholder(preparedness_transmission__husk_reading, municipal_emergency_planners, payer,
    moderate, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(preparedness_transmission__husk_reading, municipal_emergency_planners, beneficiary).

% Give their training hours to rehearsals built from a fixed scenario library last revised years ago. They execute the scripts competently and know from time on real water that actual floods rarely match them. Their objections surface in after-action notes that feed back into the same script cycle; leaving the system means leaving the service.
narrative_ontology:constraint_stakeholder(preparedness_transmission__husk_reading, frontline_response_units, payer,
    organized, biographical, constrained, regional).

% Live behind levees and in floodplains whose hazard profile has shifted faster than the scenario library. They absorb the difference between rehearsed response and actual event whenever water arrives in an unscripted way. Consultation reaches them as informational meetings held after plans are set; their experience of past floods enters the record as testimony, never as curriculum. Moving away is possible but priced beyond most households.
narrative_ontology:constraint_stakeholder(preparedness_transmission__husk_reading, flood_exposed_communities, payer,
    powerless, generational, constrained, local).

% Learned response work in live flood operations alongside the generation that wrote the original doctrine, and carry craft knowledge — reading water, improvising through communications failure — that exists nowhere in the manual. Retirement removed them from the exercise cycle, and no mechanism recruits their judgment back into scenario design. Their critiques circulate informally and in memoirs; there is no path back in.
narrative_ontology:constraint_stakeholder(preparedness_transmission__husk_reading, retired_operational_veterans, excluded,
    moderate, biographical, trapped, regional).

% Inspects the inspectors: samples drill records, verifies report completeness, and publishes compliance rates. Its instruments detect whether the ritual occurred, not whether capability exists, so its findings consistently confirm the system to itself. It could commission outcome-scored exercises, but its mandate and methods are defined around process verification.
narrative_ontology:constraint_stakeholder(preparedness_transmission__husk_reading, national_audit_office, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(preparedness_transmission__husk_reading, diffuse).
narrative_ontology:fixing_cost_class(preparedness_transmission__husk_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Keeps several hundred dispersed municipal units synchronized on a shared procedural canon: common terminology, common equipment-check intervals, a common annual rehearsal calendar, and a common documentary standard by which any unit's activity can be verified by any superior body.
% TRANSFER_FUNCTION: Moves staff hours and dedicated budget from frontline units and municipal agencies into scheduled rehearsal and documentation; moves compliance artifacts upward to the directorate, ministry, and audit office; moves reassurance outward to the public; moves comparatively little tested capability in any direction.
% ABSENT_VOICES: Retired operational veterans hold the craft knowledge the scripts replaced and would testify that transmission ended with the apprenticeship pathway. Flood-exposed residents hold event experience that never becomes curriculum. Junior responders see the gap between script and reality firsthand but lack standing to reopen the scenario library. All three are outside the rooms where drill content is set.
% DISAPPEARANCE_RATIONALE: If the mandated cycle vanished overnight, the reporting chains, certification practices, audit instruments, and annual calendars built around it would lose their object simultaneously; the ministry would face readiness questions with no metrics to answer them; and the freed hours would become a contested resource between adaptive training and general budgets. Arrangements across at least five seats depend on the cycle continuing.
% FOUNDING_PROBLEM: Mid-century flood disasters found dispersed units without shared procedures, without practiced coordination, and with no way to preserve a small stock of operational craft as its holders aged out. The drill-and-inspection regime was built to transmit that competence generationally through mandated repetition.
% FOUNDING_PROBLEM_CORROBORATION: Post-event inquiry reports commissioned after recent floods — authored by external reviewers, not the directorate — document both that the underlying problem persists and that scripted responses underperformed in exactly the novel conditions the scripts did not cover. National hydrological services independently corroborate the shifting hazard profile. No source outside the benefiting parties attests that the current cycle still transmits capability; that claim originates exclusively within the directorate and its ministry.
narrative_ontology:disappearance_verdict(preparedness_transmission__husk_reading, world_rearranges).
narrative_ontology:founding_problem_status(preparedness_transmission__husk_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(preparedness_transmission__husk_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(preparedness_transmission__husk_reading, 'none', 1).
narrative_ontology:epsilon_provenance(preparedness_transmission__husk_reading, 0.52, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(preparedness_transmission__husk_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(preparedness_transmission__husk_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(preparedness_transmission__husk_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Theater_ratio is authored high (0.82 at interval end) because this reading's defining observation is that the cycle's outputs are compliance artifacts rather than capability: the metric crosses the 0.5 substitution threshold mid-interval as report production overtakes rehearsal substance. Base extractiveness rises from 0.30 to 0.52 not because the cycle takes more hours but because the opportunity cost of those hours grows as the real hazard profile diverges from the frozen scripts — the same rehearsal buys less readiness every year. Suppression is authored at 0.35 and is structural, not internalized: statutory mandate, inspection dependency, and career risk for open dissent, with minimal cognitive fusion below the directorate level. Because the enforcement picture is static across the interval (inspection cadence constant, sanctions rare, compliance voluntary-theatrical), no suppression_requirement series is authored — the scalar carries the whole story, per the static-enforcement rule. Accessibility_collapse is moderate (0.40): the alternatives (outcome-scored exercises, revived apprenticeship, externally audited scenarios) are known, proposed in after-action reviews, and institutionally crowded out rather than forbidden. Resistance is low-moderate (0.30): critiques circulate in after-action notes, academic studies, and veterans' memoirs, but harm is episodic and attribution diffuse, so the latent coalition power of flood-exposed communities never organizes. Both temporal series run on one shared grid (t=0,8,16,24,32,40) with every tracked metric authored at every point. The claimed type is authored from structure — inertia without a capturer, cost-asymmetry against the fixer — independently of these metric values.
 *
 * PERSPECTIVAL GAP:
 *   The seats should classify differently and the engine computes that divergence from the structural data. From the directorate's chair, the cycle is duty and continuity — the visible proof that the organization keeps its promise to the dead of the founding disasters. From the planner's chair, it is workload with unusually clear targets. From the frontline unit's chair, it is hours spent rehearsing floods that do not happen anymore. From the community's chair, it is invisible until the night the water ignores the script. From the audit office's chair, it is a system that passes every instrument it owns. Same artifact, four incompatible experiences, and no seat holds both the power and the motive to reconcile them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive low directionality for the ministry (mobile exit, pure reassurance consumer) and for the planners' secondary position. Victim declarations drive high directionality for frontline units (constrained exit, direct hour-transfer) and highest for flood-exposed communities (powerless, effectively trapped behind levees by housing costs and attachment, bearing the residual risk the ritual no longer retires). The directorate is the deliberate complication: its beneficiary tag would derive a near-full-beneficiary directionality, but the ritual also consumes the directorate's own adaptive capacity — each certified year deepens its dependence on scripts it can no longer evaluate from experience. It pays in degraded capability even as it collects reassurance, placing it well short of the beneficiary pole; the identity_locked exit atom captures why it cannot arbitrage its way out of that self-extraction. The audit office sits at the analytical pole, collecting no extraction and bearing none.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding mandate — transmit operational flood-response competence generationally through mandated repetition — has outlived its function while the founding problem (shifting flood hazard against finite organizational memory) remains fully live. That combination is the mandatrophy signature: the arrangement persists on inheritance and self-reference, not on delivery. The classification matters because the neighboring labels both fail here. Reading it as live coordination credits a transmission that no longer occurs; reading it as capture hunts for an extractor who does not exist — no seat converts the consumed hours into concentrated advantage, and the reassurance and blame-shielding that do accrue are defensive byproducts consumed where they are produced, not captured value. What remains is the cost-asymmetry: the directorate could rebuild the scenario library and revive apprenticeship pathways tomorrow, but the fixer would pay in confessed fallibility and multi-year investment while the benefits land after their tenure, and what they personally bear from the status quo is only diffuse reputational exposure. That asymmetry, plus the absence of any concentrated capturer, is the structural case for the authored type. The identity-lock mechanism is institutional fusion: the directorate has become its calendar, its leadership succession is trained inside the ritual, and breaking the frame would dissolve the organizational self-concept that makes its certifications mean anything — which is precisely why no sitting official can afford to break it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_underdetermination,
    'Does the drill-and-inspection record show universal hollowing (this husk reading), broadly intact transmitted competence (competence_reading), or stratified survival with engineering competence alive and civilian coordination decayed (hybrid_reading)? This file instantiates only the husk reading of kernel preparedness_transmission.',
    'Outcome-scored capability audits under novel flood scenarios, unanchored to the existing checklist canon, run blind across unit types; the resulting stratification pattern selects among the sibling readings.',
    'If the hybrid reading is correct, epsilon redistributes by stratum and this story splits; if the competence reading is correct, theater_ratio collapses toward its floor and the arrangement reclassifies toward live coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_underdetermination, conceptual, 'Which reading of the preparedness-transmission kernel the ritual record actually supports.').

omega_variable(
    theater_proxy_validity,
    'Do the available observables (report volume, schedule adherence, script fidelity, certification counts) validly distinguish performative from functional activity, or could high compliance conceal genuine low-frequency capability?',
    'No-notice exercises scored on outcomes rather than process, compared against the same units'' compliance scores.',
    'If the proxies systematically mislead, the authored theater_ratio is overstated and the arrangement sits closer to functioning coordination than this reading claims.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(theater_proxy_validity, empirical, 'Whether compliance artifacts are a valid proxy for the performative share of the cycle.').

omega_variable(
    identity_lock_vs_blame_avoidance,
    'Is the directorate''s persistence in the ritual cycle driven by institutional identity fusion (the organization has become its calendar) or by rational blame management (the cycle generates defensible paper)?',
    'Leadership-turnover natural experiments and liability-reform pilots: if fresh leadership revises the cycle readily, blame management dominates; if successors defend the cycle they inherited, identity fusion dominates.',
    'Under blame-dominance, fixing is cheaper than authored here and the cost class revises downward; under identity-dominance, revision waits on generational turnover and the cost class holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_vs_blame_avoidance, empirical, 'Mechanism binding the administering seat to the ritual it could dismantle.').

omega_variable(
    counterfactual_resource_disposition,
    'If the mandated cycle lapsed, would the freed hours and budget convert into adaptive, scenario-diverse training, or simply lapse into general budgets?',
    'Track jurisdictions that relaxed mandated drill requirements: measure whether training hours reappear as adaptive exercises or disappear into absorption.',
    'Determines whether removal rearranges the system toward recovered capability or merely saves money, and therefore how strongly the disappearance verdict binds.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(counterfactual_resource_disposition, empirical, 'Counterfactual fate of the resources the ritual currently consumes.').

omega_variable(
    failure_attribution_confound,
    'Are poor outcomes under novel floods caused by ritualized training, or by baseline underfunding and equipment gaps that would persist under any training regime?',
    'Matched comparison across agencies differing in training regime but comparable in funding and equipment, controlling for event severity.',
    'If confounded, part of the measured extraction belongs to a separate funding constraint and this story''s epsilon is inflated; clean attribution sharpens the referent.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(failure_attribution_confound, empirical, 'Attribution of novel-scenario failure to the ritual versus independent resource deficits.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(preparedness_transmission__husk_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(husk_tr_t0, preparedness_transmission__husk_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement_basis(husk_tr_t0, observed).
narrative_ontology:measurement(husk_tr_t8, preparedness_transmission__husk_reading, theater_ratio, 8, 0.4).
narrative_ontology:measurement_basis(husk_tr_t8, observed).
narrative_ontology:measurement(husk_tr_t16, preparedness_transmission__husk_reading, theater_ratio, 16, 0.55).
narrative_ontology:measurement_basis(husk_tr_t16, observed).
narrative_ontology:measurement(husk_tr_t24, preparedness_transmission__husk_reading, theater_ratio, 24, 0.68).
narrative_ontology:measurement_basis(husk_tr_t24, observed).
narrative_ontology:measurement(husk_tr_t32, preparedness_transmission__husk_reading, theater_ratio, 32, 0.76).
narrative_ontology:measurement_basis(husk_tr_t32, observed).
narrative_ontology:measurement(husk_tr_t40, preparedness_transmission__husk_reading, theater_ratio, 40, 0.82).
narrative_ontology:measurement_basis(husk_tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(husk_be_t0, preparedness_transmission__husk_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement_basis(husk_be_t0, observed).
narrative_ontology:measurement(husk_be_t8, preparedness_transmission__husk_reading, base_extractiveness, 8, 0.34).
narrative_ontology:measurement_basis(husk_be_t8, observed).
narrative_ontology:measurement(husk_be_t16, preparedness_transmission__husk_reading, base_extractiveness, 16, 0.39).
narrative_ontology:measurement_basis(husk_be_t16, observed).
narrative_ontology:measurement(husk_be_t24, preparedness_transmission__husk_reading, base_extractiveness, 24, 0.44).
narrative_ontology:measurement_basis(husk_be_t24, observed).
narrative_ontology:measurement(husk_be_t32, preparedness_transmission__husk_reading, base_extractiveness, 32, 0.48).
narrative_ontology:measurement_basis(husk_be_t32, observed).
narrative_ontology:measurement(husk_be_t40, preparedness_transmission__husk_reading, base_extractiveness, 40, 0.52).
narrative_ontology:measurement_basis(husk_be_t40, observed).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(preparedness_transmission__husk_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(preparedness_transmission__husk_reading, identity_coordination).
narrative_ontology:affects_constraint(preparedness_transmission__husk_reading, competence_reading).
narrative_ontology:affects_constraint(preparedness_transmission__husk_reading, hybrid_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'preparedness drills' bundles at least three structurally distinct claims about whether mandated repetition transmits operational capability. Decomposed per the epsilon-invariance principle into three readings of kernel preparedness_transmission: competence_reading (transmission live; negligible extraction), husk_reading (this file; transmission dead, performance persists; high theater), and hybrid_reading (stratified survival; extraction varies by stratum). Each carries its own epsilon, beneficiaries, and stakeholders. The competence reading is upstream — it is the doctrine of record whose compliance instruments manufacture the very artifacts this reading reinterprets as memorial output — and this file links both siblings via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
