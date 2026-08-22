% ============================================================================
% CONSTRAINT STORY: preparedness_transmission__competence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_preparedness_transmission__competence_reading, []).

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
 *   constraint_id: preparedness_transmission__competence_reading
 *   human_readable: Preparedness Transmission Regime — Competence Reading (Drills as Live Exercised Knowledge)
 *   domain: disaster_risk_management/institutional_memory/civil_defense
 *
 * SUMMARY:
 *   A national civil-defense preparedness regime requires every response unit
 *   to cycle through scheduled drills, joint multi-agency exercises, and
 *   periodic inspections, with certification contingent on passing. This
 *   story instantiates the COMPETENCE READING of the contested kernel
 *   preparedness_transmission: the claim that each generation re-validates
 *   capability through practice, so the regime transmits live operational
 *   knowledge rather than performing memorial ritual. Per the
 *   epsilon-invariance principle, the sibling readings (husk_reading,
 *   hybrid_reading) are separate constraint files with their own epsilon,
 *   beneficiaries, and classifications; this file authors the standing
 *   drill-and-inspection arrangement as THIS reading sees it — the referent
 *   is fixed to the existing regime, and the values are indexed to the
 *   competence reading's own lights. Under this reading the regime solves a
 *   real collective-action problem (skill atrophy under personnel turnover,
 *   inter-agency seam decay, plan-versus-plant divergence) at modest cost
 *   that mostly recycles into the function itself.
 *
 * KEY AGENTS:
 *   - civil_defense_training_authority: Agenda-setter and institutional beneficiary (institutional/constrained) — sets drill cycles and inspection standards, collects mandate and appropriations, recycles them into scenario variation and corrective training
 *   - frontline_response_units: Primary beneficiary (organized/constrained) — carries drilled competence in crew routines; bears the drill-hour opportunity cost
 *   - senior_inspector_corps: Beneficiary with identity-fused expertise (organized/identity_locked) — recognition capital maintained only through continued hands-on inspection practice
 *   - civilian_protected_communities: Ultimate beneficiary (moderate/trapped) — cannot exit hazard exposure; consumes verified readiness as assurance
 *   - municipal_budget_holders: Payer-beneficiary (moderate/constrained) — funds the regime and receives assurance whose value is invisible until tested
 *   - residents_disrupted_by_exercises: Excluded voice (powerless/trapped) — absorbs exercise disruption without a seat in scheduling
 *   - disaster_research_community: Analytical observer (analytical/analytical) — tests whether exercise regimes transfer to real-event performance
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(preparedness_transmission__competence_reading, 0.26).
domain_priors:suppression_score(preparedness_transmission__competence_reading, 0.22).
domain_priors:theater_ratio(preparedness_transmission__competence_reading, 0.18).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(preparedness_transmission__competence_reading, extractiveness, 0.26).
narrative_ontology:constraint_metric(preparedness_transmission__competence_reading, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(preparedness_transmission__competence_reading, theater_ratio, 0.18).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(preparedness_transmission__competence_reading, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(preparedness_transmission__competence_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(preparedness_transmission__competence_reading, rope).
narrative_ontology:human_readable(preparedness_transmission__competence_reading, "Preparedness Transmission Regime — Competence Reading (Drills as Live Exercised Knowledge)").
narrative_ontology:topic_domain(preparedness_transmission__competence_reading, "disaster_risk_management/institutional_memory/civil_defense").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(preparedness_transmission__competence_reading, '875cf427-13e9-4ca4-9c77-64659cde6cb7').
narrative_ontology:cs_kernel_codification('875cf427-13e9-4ca4-9c77-64659cde6cb7', formalized).
narrative_ontology:cs_authority_grounding('875cf427-13e9-4ca4-9c77-64659cde6cb7', practice).
narrative_ontology:cs_interpretation_layer_present('875cf427-13e9-4ca4-9c77-64659cde6cb7').
narrative_ontology:cs_reading_relation('875cf427-13e9-4ca4-9c77-64659cde6cb7', preparedness_transmission__husk_reading, forecloses).
narrative_ontology:cs_reading_relation('875cf427-13e9-4ca4-9c77-64659cde6cb7', preparedness_transmission__hybrid_reading, coexists_with).
narrative_ontology:cs_axiom('875cf427-13e9-4ca4-9c77-64659cde6cb7', foundational, repetition_under_variation_builds_capability).
narrative_ontology:cs_axiom_status(repetition_under_variation_builds_capability, holdable).
narrative_ontology:cs_axiom_grounding('875cf427-13e9-4ca4-9c77-64659cde6cb7', repetition_under_variation_builds_capability, empirically_contingent).
narrative_ontology:cs_axiom('875cf427-13e9-4ca4-9c77-64659cde6cb7', foundational, inspector_recognition_requires_live_practice).
narrative_ontology:cs_axiom_status(inspector_recognition_requires_live_practice, holdable).
narrative_ontology:cs_axiom_grounding('875cf427-13e9-4ca4-9c77-64659cde6cb7', inspector_recognition_requires_live_practice, empirically_contingent).
narrative_ontology:cs_reference_frame('875cf427-13e9-4ca4-9c77-64659cde6cb7', exercised_competence_standard).
narrative_ontology:cs_drift_state('875cf427-13e9-4ca4-9c77-64659cde6cb7', contemporary_multi_hazard_era, gap(practice_drift, minor, false)).
narrative_ontology:cs_created_at('875cf427-13e9-4ca4-9c77-64659cde6cb7', '').
narrative_ontology:cs_kernel_id(preparedness_transmission__competence_reading, preparedness_transmission).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(preparedness_transmission__competence_reading, frontline_response_units).
narrative_ontology:constraint_beneficiary(preparedness_transmission__competence_reading, civilian_protected_communities).
narrative_ontology:constraint_beneficiary(preparedness_transmission__competence_reading, senior_inspector_corps).
narrative_ontology:constraint_beneficiary(preparedness_transmission__competence_reading, civil_defense_training_authority).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(preparedness_transmission__competence_reading, municipal_budget_holders).
narrative_ontology:constraint_victim(preparedness_transmission__competence_reading, municipal_budget_holders).
narrative_ontology:constraint_vindicates(preparedness_transmission__competence_reading, high_reliability_organization_doctrine).
narrative_ontology:constraint_vindicates(preparedness_transmission__competence_reading, deliberate_practice_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets the national drill cycle, inspection protocols, and certification standards; runs the instructor academies; receives appropriations earmarked for exercises. Its mandate depends on demonstrating that exercises produce measurable capability, so it invests in scenario variation, post-exercise critique, and corrective re-training rather than repeat performances of familiar scripts. Exit would mean surrendering the coordination mandate back to individual agencies.
narrative_ontology:constraint_stakeholder(preparedness_transmission__competence_reading, civil_defense_training_authority, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(preparedness_transmission__competence_reading, civil_defense_training_authority, beneficiary).

% Fire, rescue, and civil-defense squads whose members cycle through mandatory drills and joint multi-agency exercises. Competence — ladder work under smoke, flood rescue in moving water, command handoffs between agencies — is carried in crew routines and refreshed each cycle. Units that skip cycles degrade visibly at the next real event. A responder who leaves the service forfeits the posting but the drilled skill travels with them, and neighboring services hire for it.
narrative_ontology:constraint_stakeholder(preparedness_transmission__competence_reading, frontline_response_units, beneficiary,
    organized, biographical, constrained, regional).

% Veteran inspectors who audit units and sign certifications. Their value rests on pattern libraries built from decades of exercises and real incidents — spotting a corroded anchor point, a stale mutual-aid agreement, a crew executing steps without understanding. That recognition capital exists only inside this regime and is maintained by continued hands-on inspection practice; retirement or transfer strands it. Their professional identity is fused with the inspection craft.
narrative_ontology:constraint_stakeholder(preparedness_transmission__competence_reading, senior_inspector_corps, beneficiary,
    organized, generational, identity_locked, national).

% Residents of flood plains, seismic zones, and industrial perimeters who are the ultimate claimants on response capability. They participate in evacuation drills as subjects, receive assurance from published inspection results, and cannot cheaply relocate away from hazard exposure. Their protection depends on strangers' drilled competence.
narrative_ontology:constraint_stakeholder(preparedness_transmission__competence_reading, civilian_protected_communities, beneficiary,
    moderate, generational, trapped, national).

% City and county finance officers who fund drill hours, exercise fuel, inspector travel, and facility wear. They bear the visible line-item cost and receive the invisible asset — verified readiness — whose value they can confirm only after an event succeeds. Deferring the drill budget is their main lever, and the regime's inspection results are what they cite when defending the line item.
narrative_ontology:constraint_stakeholder(preparedness_transmission__competence_reading, municipal_budget_holders, payer,
    moderate, biographical, constrained, local).
narrative_ontology:stakeholder_secondary_role(preparedness_transmission__competence_reading, municipal_budget_holders, beneficiary).

% Households whose streets close, transit reroutes, and sirens sound during large-scale exercises. They absorb the disruption and are rarely consulted on scheduling or scope; their consent is assumed through the general preparedness justification.
narrative_ontology:constraint_stakeholder(preparedness_transmission__competence_reading, residents_disrupted_by_exercises, excluded,
    powerless, immediate, trapped, local).

% Academics and independent incident-review boards who study whether exercise regimes transfer to real-event performance. They design scenario-injection studies, compare jurisdictions with continuous versus lapsed drill cycles, and publish findings that neither the training authority nor the units control.
narrative_ontology:constraint_stakeholder(preparedness_transmission__competence_reading, disaster_research_community, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(preparedness_transmission__competence_reading, diffuse).
narrative_ontology:fixing_cost_class(preparedness_transmission__competence_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains distributed response capability against continuous erosion: personnel turnover drains embodied skill, inter-agency procedures drift apart, and stored plans diverge from physical reality. Scheduled drills force crews and agencies to re-synchronize on a fixed cadence; inspections force plans to confront the plant; joint exercises rehearse the seams — command handoffs, mutual aid — that no single agency can practice alone.
% TRANSFER_FUNCTION: Moves drill hours, fuel, facility wear, and inspector attention from routine operations and municipal budgets into rehearsed capability; moves operational knowledge from veteran generations to new hires through supervised repetition; moves assurance — certified readiness — from response units upward to oversight bodies, insurers, and the public.
% ABSENT_VOICES: Residents whose streets close and sirens sound during large-scale exercises hold no seat in scheduling or scope decisions. Small volunteer units carry the highest drill-hour burden relative to capacity and are underrepresented on the curriculum committees that set requirements. Disability advocates note that evacuation drills rarely rehearse mobility-impaired egress, so the rehearsed scenario diverges from the real population. All three would press for changes in cadence, scope, or content; all are outside the room (commentary-grade absence, not a classification input).
% DISAPPEARANCE_RATIONALE: If the drill-and-inspection regime vanished overnight, nothing breaks immediately — capability persists at its current level — but decay begins with the next hiring cohort: within a few years crews execute rarely-used procedures from manuals under stress, inter-agency command handoffs revert to improvisation, and stored plans silently diverge from changed infrastructure. The first major flood or industrial accident after the gap finds the seams open, and post-event reviews would reconstruct the missing regime at crisis prices. Arrangements visibly depend on it.
% FOUNDING_PROBLEM: Civil defense was built after documented failures in which paper plans existed but execution collapsed — untrained crews, untested equipment, agencies unable to coordinate during floods and industrial accidents. The regime was founded to convert written preparedness into embodied, verifiable capability, and to keep it converted as people and equipment turned over.
% FOUNDING_PROBLEM_CORROBORATION: Independent accident investigation boards repeatedly attribute contained outcomes to prior joint drilling and attribute escalations to lapsed or skipped exercise cycles — attestation from outside the benefiting parties. Academic high-reliability-organization research and insurer actuarial comparisons of jurisdictions with continuous versus interrupted drill records corroborate the same problem structure. The training authority's own attestations are noted but not relied upon.
narrative_ontology:disappearance_verdict(preparedness_transmission__competence_reading, world_rearranges).
narrative_ontology:founding_problem_status(preparedness_transmission__competence_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(preparedness_transmission__competence_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(preparedness_transmission__competence_reading, 'none', 1).
narrative_ontology:epsilon_provenance(preparedness_transmission__competence_reading, 0.26, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(preparedness_transmission__competence_reading_tests).
:- end_tests(preparedness_transmission__competence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.26: the regime's costs — drill hours, fuel, facility wear, inspector time, documentation — are real but mostly convert into the capability the regime exists to maintain; the residual is compliance paperwork that has grown faster than its verification return. Suppression 0.22: participation is mandated inside member organizations, but the mandate rides professional norms and demonstrated value rather than exit-blocking; a responder who leaves takes the skill along, and neighboring services hire for it. Theater_ratio 0.18: most exercise activity is functional under this reading — scenario variation, post-exercise critique, corrective re-training — with a minority ceremonial share (anniversary exercises, media demonstrations) that has crept upward as institutions aged. Accessibility_collapse 0.42: alternatives to the formal regime (simulation software, self-directed study, ad-hoc inter-agency tabletops) remain workable for motivated units, so understanding the regime does not collapse alternatives; it merely dominates them on reliability. Resistance 0.3: drill-fatigue complaints, budget-line skirmishes, and small-unit burden grievances are persistent but not regime-threatening. Claim and metrics are independent authored facts: claimed_type rope states the structure this reading asserts — genuine coordination, net beneficiaries, no suppressed alternative — while the metrics describe operation as observed; the engine computes per-seat types from the structural data and may disagree. The measurement series run on one shared six-point grid; suppression_requirement carries no series because the enforcement picture is static (light-touch audit, unchanged machinery across the interval) — the scalar captures it.
 *
 * PERSPECTIVAL GAP:
 *   Seats should compute differently. From the training authority's chair the regime is a functioning machine it operates and refines; from a frontline unit it is a demanding but valuable cadence; from a municipal budget office it is a line item whose payoff is invisible until the event that never makes the news; from a disrupted resident it is pure externality cost with no offsetting receipt. Same structure, four experiences — the engine derives this divergence from power, exit, and role data, not from the authored claim. The identity_locked atom on the inspector corps marks fused professional identity: their recognition capital exists only inside this regime, so their seat reads the constraint as indispensable even where other seats read burden.
 *
 * DIRECTIONALITY LOGIC:
 *   No victim seat exists under this reading, so no agent derives a high target-directionality. The beneficiaries (response units, protected communities, inspector corps, the authority itself) sit near the beneficiary end — the regime subsidizes their capability, assurance, mandate, and protection respectively. Municipal budget holders carry the visible cost and the invisible benefit; their derived directionality sits near symmetric, slightly above the pure beneficiaries. Excluded residents bear real disruption but hold an excluded role — commentary-grade only; their absence informs the consensus-provenance check and never feeds the extraction arithmetic. Because base extraction is low and diffuse, the national scope amplifies effective extraction only marginally off the beneficiary-side baseline.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — converting written preparedness into embodied capability against continuous turnover — regenerates with every hiring cohort and every novel hazard, so founding_problem_status is live and no mandatrophy resolution is declared. The mismatch consumer reads status=live against verdict=world_rearranges and finds no zombie flag. The classification guards both error directions: it prevents the husk intuition (ritual!) from mislabeling a functioning coordination regime as pure theater, and it prevents the regime's own self-description from laundering the real, slowly growing compliance burden as zero-cost coordination. The theater_ratio series is the tripwire: if the ceremonial share ever crosses the functional share, the rope claim dies and the husk_reading file becomes the accurate description of the same arrangement.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest_transmission_efficacy,
    'Does the observed drill-and-inspection activity transmit live operational competence across generations (this reading, competence_reading), or does it perform memorial ritual over organizational memory while operational knowledge hollows out (sibling husk_reading)?',
    'Scenario-injection testing: third-party red cells withhold exercise designs and insert novel failure signatures; blind comparison of inspector defect-recognition against ground truth; natural experiments from jurisdictions whose drill cycles lapsed and later resumed.',
    'If performance collapses under novel scenarios, this reading''s premise fails, theater_ratio and effective extraction rise sharply, and the constraint''s structure converges on the husk_reading file''s profile — assurance collected without capability delivered. Sustained improvisation under scenario variation confirms this reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest_transmission_efficacy, empirical, 'Whether repeated practice builds capability or masks its absence — the core contest between competence_reading and husk_reading.').

omega_variable(
    stratified_competence_scope,
    'Is transmitted competence uniform across the regime''s domains, or stratified — high in physical-infrastructure and engineering tasks, decayed in civilian coordination (sibling hybrid_reading)?',
    'Domain-disaggregated capability audits scoring engineering tasks (pump operations, structural shoring, communications) separately from coordination tasks (multi-agency command handoffs, volunteer mobilization, public messaging).',
    'If competence is stratified, this reading''s uniform premise fails, the constraint family splits along the strata, and inspection emphasis and resource allocation should follow the decayed stratum. Uniform scores across domains confirm this reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(stratified_competence_scope, empirical, 'Uniform-versus-stratified transmission — the competence_reading/hybrid_reading dispute over scope.').

omega_variable(
    scripted_exercise_falsifiability,
    'Are exercises measuring capability or manufacturing it — can the regime''s success evidence be trusted when the same institution scripts the scenario, trains to the script, and grades the result?',
    'Mandated third-party red-cell design withheld from participants until execution, plus publication of failed-exercise results alongside passed ones.',
    'If scripting contamination is material, part of the observed competence is measurement artifact, the theater_ratio is understated, and both this reading and its metrics lose evidentiary standing until independent evaluation replaces self-grading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(scripted_exercise_falsifiability, empirical, 'Instrument contamination: drills serve as both the mechanism and the measure of competence.').

omega_variable(
    volunteer_unit_burden_distribution,
    'Is the drill-hour and documentation burden falling on small volunteer units a genuine verification cost, or the leading edge of compliance accretion that extracts time without proportional capability return?',
    'Burden-return analysis comparing capability gains per drill hour across unit sizes; review of documentation requirements added per cycle against measured incident-performance deltas.',
    'If accretion dominates, effective extraction for the smallest units rises well above the regime average, the rope classification strains toward a hybrid structure for that seat, and paperwork relief becomes the highest-value reform.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(volunteer_unit_burden_distribution, empirical, 'Whether compliance overhead is coordination cost or creeping extraction concentrated on the least-resourced seats.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(preparedness_transmission__competence_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(prep_tr_t0, preparedness_transmission__competence_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement_basis(prep_tr_t0, observed).
narrative_ontology:measurement(prep_tr_t6, preparedness_transmission__competence_reading, theater_ratio, 6, 0.13).
narrative_ontology:measurement_basis(prep_tr_t6, observed).
narrative_ontology:measurement(prep_tr_t12, preparedness_transmission__competence_reading, theater_ratio, 12, 0.14).
narrative_ontology:measurement_basis(prep_tr_t12, observed).
narrative_ontology:measurement(prep_tr_t18, preparedness_transmission__competence_reading, theater_ratio, 18, 0.15).
narrative_ontology:measurement_basis(prep_tr_t18, observed).
narrative_ontology:measurement(prep_tr_t24, preparedness_transmission__competence_reading, theater_ratio, 24, 0.17).
narrative_ontology:measurement_basis(prep_tr_t24, observed).
narrative_ontology:measurement(prep_tr_t30, preparedness_transmission__competence_reading, theater_ratio, 30, 0.18).
narrative_ontology:measurement_basis(prep_tr_t30, observed).

% Extraction over time
narrative_ontology:measurement(prep_be_t0, preparedness_transmission__competence_reading, base_extractiveness, 0, 0.21).
narrative_ontology:measurement_basis(prep_be_t0, observed).
narrative_ontology:measurement(prep_be_t6, preparedness_transmission__competence_reading, base_extractiveness, 6, 0.22).
narrative_ontology:measurement_basis(prep_be_t6, observed).
narrative_ontology:measurement(prep_be_t12, preparedness_transmission__competence_reading, base_extractiveness, 12, 0.23).
narrative_ontology:measurement_basis(prep_be_t12, observed).
narrative_ontology:measurement(prep_be_t18, preparedness_transmission__competence_reading, base_extractiveness, 18, 0.24).
narrative_ontology:measurement_basis(prep_be_t18, observed).
narrative_ontology:measurement(prep_be_t24, preparedness_transmission__competence_reading, base_extractiveness, 24, 0.25).
narrative_ontology:measurement_basis(prep_be_t24, observed).
narrative_ontology:measurement(prep_be_t30, preparedness_transmission__competence_reading, base_extractiveness, 30, 0.26).
narrative_ontology:measurement_basis(prep_be_t30, observed).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(preparedness_transmission__competence_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(preparedness_transmission__competence_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(preparedness_transmission__competence_reading, preparedness_transmission__husk_reading).
narrative_ontology:affects_constraint(preparedness_transmission__competence_reading, preparedness_transmission__hybrid_reading).
narrative_ontology:affects_constraint(preparedness_transmission__competence_reading, interagency_incident_command_interoperability).

% DUAL FORMULATION NOTE:
% The colloquial label 'preparedness drills and inspections' decomposes under the epsilon-invariance principle into three structurally distinct constraints sharing the kernel preparedness_transmission: competence_reading (this file — practice transmits live capability; low extraction, rope), husk_reading (practice as memorial ritual over hollowed knowledge; high theater, assurance collected without capability delivered), and hybrid_reading (stratified survival — engineering competence live, civilian coordination decayed). Each file fixes its own epsilon, beneficiaries, and stakeholders; the upstream/downstream citation pattern runs from whichever reading prevails in a jurisdiction's self-assessment to its budget and inspection-emphasis decisions. This file links its siblings per the constraint-family rule; nothing from their profiles is averaged into this one.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
