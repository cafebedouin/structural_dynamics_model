% ============================================================================
% CONSTRAINT STORY: preparedness_retention__husk_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_preparedness_retention__husk_reading, []).

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
 *   constraint_id: preparedness_retention__husk_reading
 *   human_readable: Flood Preparedness as Memorial Performance (Husk Reading)
 *   domain: governance/disaster preparedness/institutional memory
 *
 * SUMMARY:
 *   A regional flood-management system runs an annual cycle of evacuation
 *   drills, shelter inspections, and interagency coordination exercises.
 *   Compliance statistics are published, budgets are justified against
 *   completion rates, and officials cite the exercise calendar as evidence of
 *   readiness. Independent review of after-action data from actual flood
 *   events in comparable systems shows a persistent gap: certified, drilled
 *   facilities and rehearsed procedures underperform when conditions deviate
 *   from the scripted scenario (degraded communications, simultaneous
 *   multi-site failure, night-time evacuation). Under the husk reading, this
 *   is not a bug in an otherwise sound system but the structural default
 *   outcome — the ceremony crowds out the harder, more expensive work of
 *   testing and retaining tacit skill for exactly the conditions a real
 *   D5-severity flood event would present.
 *
 * KEY AGENTS:
 *   - civil_protection_agencies: agenda_setter/beneficiary (institutional/arbitrage) — designs and certifies the ceremony, captures the legitimacy
 *   - elected_flood_authorities: beneficiary (powerful/mobile) — cites compliance as due diligence, bears no cost of hollow capacity
 *   - floodplain_residents: payer (powerless/trapped) — bears the full cost of any capability gap during a real event
 *   - frontline_emergency_responders: payer/excluded (moderate/constrained) — knows the gap firsthand, has no clean channel to surface it
 *   - independent_flood_risk_researchers: observer (analytical) — documents the ceremony-competence gap empirically without authority to redirect resources
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(preparedness_retention__husk_reading, 0.71).
domain_priors:suppression_score(preparedness_retention__husk_reading, 0.58).
domain_priors:theater_ratio(preparedness_retention__husk_reading, 0.82).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(preparedness_retention__husk_reading, extractiveness, 0.71).
narrative_ontology:constraint_metric(preparedness_retention__husk_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(preparedness_retention__husk_reading, theater_ratio, 0.82).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(preparedness_retention__husk_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(preparedness_retention__husk_reading, resistance, 0.44).

% --- Constraint claim ---
narrative_ontology:constraint_claim(preparedness_retention__husk_reading, piton).
narrative_ontology:human_readable(preparedness_retention__husk_reading, "Flood Preparedness as Memorial Performance (Husk Reading)").
narrative_ontology:topic_domain(preparedness_retention__husk_reading, "governance/disaster preparedness/institutional memory").

domain_priors:requires_active_enforcement(preparedness_retention__husk_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(preparedness_retention__husk_reading, 'c595d9b6-a045-4268-b017-b41cf2150d99').
narrative_ontology:cs_kernel_codification('c595d9b6-a045-4268-b017-b41cf2150d99', formalized).
narrative_ontology:cs_authority_grounding('c595d9b6-a045-4268-b017-b41cf2150d99', extraction).
narrative_ontology:cs_interpretation_layer_present('c595d9b6-a045-4268-b017-b41cf2150d99').
narrative_ontology:cs_reading_relation('c595d9b6-a045-4268-b017-b41cf2150d99', preparedness_retention__competence_reading, coexists_with).
narrative_ontology:cs_reading_relation('c595d9b6-a045-4268-b017-b41cf2150d99', preparedness_retention__hybrid_reading, influences).
narrative_ontology:cs_axiom('c595d9b6-a045-4268-b017-b41cf2150d99', foundational, certification_activity_does_not_entail_retained_competence).
narrative_ontology:cs_axiom_status(certification_activity_does_not_entail_retained_competence, holdable).
narrative_ontology:cs_axiom_grounding('c595d9b6-a045-4268-b017-b41cf2150d99', certification_activity_does_not_entail_retained_competence, empirically_contingent).
narrative_ontology:cs_axiom('c595d9b6-a045-4268-b017-b41cf2150d99', secondary, institutional_legitimacy_interests_structurally_bias_self_assessment).
narrative_ontology:cs_axiom_status(institutional_legitimacy_interests_structurally_bias_self_assessment, holdable).
narrative_ontology:cs_axiom_grounding('c595d9b6-a045-4268-b017-b41cf2150d99', institutional_legitimacy_interests_structurally_bias_self_assessment, empirically_contingent).
narrative_ontology:cs_reference_frame('c595d9b6-a045-4268-b017-b41cf2150d99', post_disaster_reform_mandate).
narrative_ontology:cs_drift_state('c595d9b6-a045-4268-b017-b41cf2150d99', contemporary_routine_administration, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('c595d9b6-a045-4268-b017-b41cf2150d99', '').
narrative_ontology:cs_kernel_id(preparedness_retention__husk_reading, preparedness_retention).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(preparedness_retention__husk_reading, civil_protection_agencies).
narrative_ontology:constraint_beneficiary(preparedness_retention__husk_reading, elected_flood_authorities).
narrative_ontology:constraint_victim(preparedness_retention__husk_reading, floodplain_residents).
narrative_ontology:constraint_victim(preparedness_retention__husk_reading, frontline_emergency_responders).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(preparedness_retention__husk_reading, inspection_contractors).
narrative_ontology:constraint_vindicates(preparedness_retention__husk_reading, institutional_readiness_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Designs, schedules, and certifies the drill and inspection calendar. Reports compliance rates and exercise completion counts to legislators and the public as evidence of readiness. Faces no penalty for exercises that rehearse coordination on paper without testing degraded-condition response, and controls which failures from past exercises get published versus quietly filed.
narrative_ontology:constraint_stakeholder(preparedness_retention__husk_reading, civil_protection_agencies, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(preparedness_retention__husk_reading, civil_protection_agencies, beneficiary).

% Points to completed drills and passed inspections as proof of due diligence when questioned about flood risk, budget allocation, or past near-misses. Bears no direct cost when the underlying response capacity is hollow, since accountability attaches to the ceremony (was the drill held) rather than to the outcome (did it produce transferable skill).
narrative_ontology:constraint_stakeholder(preparedness_retention__husk_reading, elected_flood_authorities, beneficiary,
    powerful, biographical, mobile, regional).

% Live behind the levees and evacuation plans the drills are meant to validate. Cannot independently verify whether the sirens, shelters, and evacuation routes rehearsed on paper would function under an actual breach. Their exit options are limited to relocation, which most cannot afford; they absorb the full cost of any capability gap during a real event.
narrative_ontology:constraint_stakeholder(preparedness_retention__husk_reading, floodplain_residents, payer,
    powerless, biographical, trapped, local).

% Perform the actual drills, often on rotating and understaffed schedules, and privately know which exercises were scripted around known-good scenarios versus genuinely stress-tested. Their operational feedback about competence gaps is filtered through agency reporting chains before reaching decision-makers, and raising concerns can be read as institutional disloyalty.
narrative_ontology:constraint_stakeholder(preparedness_retention__husk_reading, frontline_emergency_responders, payer,
    moderate, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(preparedness_retention__husk_reading, frontline_emergency_responders, excluded).

% Are paid per inspection cycle to certify compliance against checklists. Their revenue depends on the inspection regime continuing to run on schedule, not on whether certified facilities would actually perform during a low-frequency, high-consequence flood event; a genuinely rigorous competence-testing regime would be slower and more expensive to deliver, so incentives favor the current cadence.
narrative_ontology:constraint_stakeholder(preparedness_retention__husk_reading, inspection_contractors, beneficiary,
    organized, biographical, arbitrage, national).

% Experienced a prior flood where drilled procedures did not translate into effective response. Their testimony about the gap between rehearsed and actual competence rarely reaches the agenda-setting process that designs the next cycle of drills, since post-event reviews are typically conducted and summarized by the same agencies being evaluated.
narrative_ontology:constraint_stakeholder(preparedness_retention__husk_reading, flood_event_survivors_of_past_failures, excluded,
    powerless, generational, trapped, local).

% Study after-action reports, drill completion statistics, and actual event outcomes to assess whether exercise activity correlates with real response performance. Can document the ceremony-competence gap empirically but hold no authority to redirect resource allocation.
narrative_ontology:constraint_stakeholder(preparedness_retention__husk_reading, independent_flood_risk_researchers, observer,
    analytical, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(preparedness_retention__husk_reading, civil_protection_agencies).
narrative_ontology:fixing_cost_class(preparedness_retention__husk_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: In principle, the drill-and-inspection cycle exists to synchronize agencies, responders, and residents on evacuation routes, communication protocols, and equipment readiness before a flood event occurs — a genuine coordination problem given that real floods do not allow for improvisation under time pressure.
% TRANSFER_FUNCTION: Moves budget, staff hours, and public attention from actual skill-retention activity (unscripted stress tests, cross-agency failure drills, degraded-condition simulations) toward schedulable, certifiable, and reportable compliance activity — and moves political and institutional legitimacy from unverifiable readiness claims to the agencies and officials who administer the ceremony.
% ABSENT_VOICES: Frontline responders' private assessments of which exercises are theater versus genuine stress tests rarely surface past their own reporting chain. Survivors of prior flood failures, who could most credibly testify to the gap between rehearsed and actual competence, are structurally excluded from designing the next exercise cycle because post-event reviews are conducted by the same institutions being evaluated.
% DISAPPEARANCE_RATIONALE: Civil protection agencies and elected authorities would say the world rearranges badly — legitimacy, funding justification, and public confidence collapse without the visible compliance record. Floodplain residents and independent researchers would say the world barely changes at the level that matters (actual response capacity during a real event), because the drills were not producing that capacity anyway; the felt loss is institutional, not operational.
% FOUNDING_PROBLEM: Coordination failures during historical flood disasters (uncoordinated evacuation, unclear chains of command, untested infrastructure) killed people; the drill and inspection regime was built to ensure agencies and residents could execute a tested, rehearsed response under real conditions.
% FOUNDING_PROBLEM_CORROBORATION: Agency leadership attests the founding problem remains live and the drill cadence directly addresses it. Independent flood-risk researchers and post-event review panels convened outside the certifying agencies (in jurisdictions where such independent review exists) have found that certified, drilled facilities and procedures underperformed during actual flood events, corroborating a divergence between the founding problem and current practice from outside the beneficiary set.
narrative_ontology:disappearance_verdict(preparedness_retention__husk_reading, contested).
narrative_ontology:founding_problem_status(preparedness_retention__husk_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(preparedness_retention__husk_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(preparedness_retention__husk_reading, 'none', 1).
narrative_ontology:epsilon_provenance(preparedness_retention__husk_reading, 0.71, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(preparedness_retention__husk_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(preparedness_retention__husk_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(preparedness_retention__husk_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored high (0.71) and rising over the interval because resource allocation increasingly favors schedulable, certifiable compliance activity over harder-to-measure competence retention — a classic Goodhart substitution where the proxy (drills completed, inspections passed) displaces the target (actual response capacity). Theater ratio starts moderate (0.48) and climbs to 0.82, reflecting a system where an increasing share of preparedness activity becomes legible, reportable performance rather than functional rehearsal. Suppression is moderate (0.58) and structural rather than dominant: the mechanism is less coercive silencing than institutional filtering — frontline feedback about real gaps is routed through reporting chains controlled by the agencies whose legitimacy depends on the ceremony's success. Accessibility collapse is authored at 0.62: residents and even researchers cannot easily verify true readiness once the compliance narrative is established, since the only visible signal (drills held, certificates issued) is exactly the signal the ceremony produces. Resistance is authored moderate-low (0.44) because those most harmed (floodplain residents) lack the technical standing to contest the readiness claims, and those with standing (responders) are constrained by institutional loyalty norms.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter seat, this reads as sound institutional practice: exercises are held, certifications issued, reports filed, and no acute crisis has yet exposed a gap. From the floodplain-resident seat, the same activity reads as an inert institutional habit that neither party is positioned to interrupt: agencies have no incentive to admit the gap (it would indict their own program), and residents lack the expertise or standing to demonstrate it before a disaster does. The engine is expected to compute divergent seat-level types precisely because the structural inputs (power, exit, directionality) diverge sharply, even though the constraint is claimed uniformly as piton at the story level.
 *
 * DIRECTIONALITY LOGIC:
 *   Civil protection agencies and elected authorities sit near the full-beneficiary end: they collect legitimacy, budget justification, and political cover from the ceremony's existence, with arbitrage-grade or mobile exit from any consequences of its hollowness. Inspection contractors are structural beneficiaries whose revenue model depends on the current cadence continuing rather than being replaced by more rigorous (and less schedulable) competence testing. Floodplain residents sit at the full-target end: trapped exit, they bear the entire downside if the rehearsed procedures fail under real conditions. Frontline responders are targets with constrained exit — their labor produces the ceremony, and their private knowledge of its limits carries professional risk to disclose. This directionality profile is what drives high effective extraction on the payer seats despite the story's overall claimed type sitting closer to piton than snare — this is degraded coordination, not naked capture.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (uncoordinated response during real flood disasters) has not disappeared, but the mechanism built to solve it has drifted toward serving a different function — generating certifiable evidence of institutional diligence — while its capacity to solve the original problem is not verified by the same process that certifies it. This is the classic mandatrophy signature: mandate outlives verified function, and the piton classification captures that the extraction (diverted resources, complacent public confidence) persists mostly through inertia and theatrical maintenance rather than through anyone actively profiting from acknowledged failure. No stakeholder here concentrates enough capture to make this a snare — the harm is diffuse degradation of a system nobody is positioned to fix cheaply, which is exactly the piton signature the schema requires: an agenda-setter who could change it, and payers who cannot force the change, with the fixing cost exceeding what the agenda-setter itself bears.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    husk_reading_kernel_selection,
    'Is the strong husk reading (drills categorically fail to preserve live competence, system-wide) the correct framing, or does actual competence retention vary sharply enough across institutional layers that the hybrid_reading (competence concentrated in specialized technical bodies, ceremony elsewhere) better fits the evidence?',
    'Compare after-action performance data from specialized technical agencies (e.g., dedicated flood-control engineering bodies) against broader civil-protection and municipal responders during actual flood events; if specialized bodies consistently outperform generalist responders under stress while both pass the same certification regime, the hybrid_reading is better supported for at least the specialized layer, and this husk_reading should be scoped down rather than claimed system-wide.',
    'If the hybrid_reading is empirically favored, this constraint''s claimed_type and extractiveness apply only to the non-specialized layer of the system, and the specialized-institution layer should be modeled as a separate constraint closer to the competence_reading rather than folded into this one.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(husk_reading_kernel_selection, empirical, 'Whether the husk reading over-generalizes from ceremonial failure in generalist layers to the whole preparedness system.').

omega_variable(
    drill_realism_measurability,
    'Can ''live competence'' versus ''ceremonial performance'' be measured directly (e.g., via unscripted red-team stress tests), or is the theater_ratio metric itself an indirect proxy vulnerable to the same Goodhart substitution it is meant to detect?',
    'Introduce independently designed, unscripted stress exercises audited by parties with no stake in the existing certification regime, and compare outcomes against the standard drill cycle''s reported success rates over multiple cycles.',
    'If unscripted stress tests reveal performance close to standard drill outcomes, the husk reading is weakened; if they reveal a large gap, the husk reading and its high theater_ratio are corroborated independently of self-reported compliance data.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(drill_realism_measurability, empirical, 'Whether the theater/competence gap can be measured without relying on the same institutions whose performance is in question.').

omega_variable(
    institutional_legitimacy_as_beneficiary,
    'Is ''institutional legitimacy'' a coherent beneficiary category, or does it always cash out to the concrete officials and agencies who hold budget and reputational stakes in the ceremony continuing?',
    'Trace where budget allocations, promotion decisions, and political credit actually flow when compliance statistics are published, versus where they would flow under an alternative regime measuring verified competence.',
    'If legitimacy always cashes out to identifiable agencies and officials, the beneficiary declaration should be read as concrete institutional actors (already named) rather than an abstract collective good — this affects whether future revisions should list legitimacy itself as a vindicated proposition rather than folding it into the beneficiary language.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_legitimacy_as_beneficiary, conceptual, 'Whether institutional legitimacy functions as an actor-level beneficiary or a vindicated abstraction.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(preparedness_retention__husk_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(prep_tr_t0, preparedness_retention__husk_reading, theater_ratio, 0, 0.48).
narrative_ontology:measurement(prep_tr_t4, preparedness_retention__husk_reading, theater_ratio, 4, 0.57).
narrative_ontology:measurement(prep_tr_t8, preparedness_retention__husk_reading, theater_ratio, 8, 0.64).
narrative_ontology:measurement(prep_tr_t12, preparedness_retention__husk_reading, theater_ratio, 12, 0.71).
narrative_ontology:measurement(prep_tr_t16, preparedness_retention__husk_reading, theater_ratio, 16, 0.76).
narrative_ontology:measurement(prep_tr_t20, preparedness_retention__husk_reading, theater_ratio, 20, 0.8).
narrative_ontology:measurement(prep_tr_t24, preparedness_retention__husk_reading, theater_ratio, 24, 0.82).

% Extraction over time
narrative_ontology:measurement(prep_be_t0, preparedness_retention__husk_reading, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(prep_be_t4, preparedness_retention__husk_reading, base_extractiveness, 4, 0.47).
narrative_ontology:measurement(prep_be_t8, preparedness_retention__husk_reading, base_extractiveness, 8, 0.53).
narrative_ontology:measurement(prep_be_t12, preparedness_retention__husk_reading, base_extractiveness, 12, 0.59).
narrative_ontology:measurement(prep_be_t16, preparedness_retention__husk_reading, base_extractiveness, 16, 0.64).
narrative_ontology:measurement(prep_be_t20, preparedness_retention__husk_reading, base_extractiveness, 20, 0.68).
narrative_ontology:measurement(prep_be_t24, preparedness_retention__husk_reading, base_extractiveness, 24, 0.71).

% Suppression requirement over time
narrative_ontology:measurement(prep_su_t0, preparedness_retention__husk_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(prep_su_t4, preparedness_retention__husk_reading, suppression_requirement, 4, 0.4).
narrative_ontology:measurement(prep_su_t8, preparedness_retention__husk_reading, suppression_requirement, 8, 0.44).
narrative_ontology:measurement(prep_su_t12, preparedness_retention__husk_reading, suppression_requirement, 12, 0.48).
narrative_ontology:measurement(prep_su_t16, preparedness_retention__husk_reading, suppression_requirement, 16, 0.52).
narrative_ontology:measurement(prep_su_t20, preparedness_retention__husk_reading, suppression_requirement, 20, 0.55).
narrative_ontology:measurement(prep_su_t24, preparedness_retention__husk_reading, suppression_requirement, 24, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(preparedness_retention__husk_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(preparedness_retention__husk_reading, preparedness_retention__competence_reading).
narrative_ontology:affects_constraint(preparedness_retention__husk_reading, preparedness_retention__hybrid_reading).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the preparedness_retention kernel. competence_reading claims the drill-and-inspection cycle genuinely preserves operational capacity (low extraction, functioning coordination). husk_reading (this story) claims the cycle has decayed into memorial performance system-wide (high extraction, piton-flavored degradation). hybrid_reading claims competence survives in specialized technical institutions while broader societal preparedness is ceremonial (mixed extraction, stratified by institutional layer). Each story carries its own stable epsilon and its own stakeholder set; they are linked here rather than merged because the underlying empirical claim about where competence actually resides differs sharply between them, which is exactly the ε-invariance decomposition rule.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
