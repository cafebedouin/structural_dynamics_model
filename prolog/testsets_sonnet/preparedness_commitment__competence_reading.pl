% ============================================================================
% CONSTRAINT STORY: preparedness_commitment__competence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_preparedness_commitment__competence_reading, []).

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
 *   constraint_id: preparedness_commitment__competence_reading
 *   human_readable: Preparedness as Live Exercised Competence (Competence Reading)
 *   domain: disaster preparedness/institutional memory
 *
 * SUMMARY:
 *   This story instantiates the COMPETENCE READING of the
 *   preparedness_commitment kernel: preparedness as live, exercised knowledge
 *   in which drills genuinely test and rebuild real decision-making capacity,
 *   and generational turnover is absorbed because new personnel actually
 *   acquire the tacit judgment the system depends on. This is a single,
 *   stable claim — the exercises are functionally real, not memorial theater.
 *   The sibling readings (husk_reading: preparedness as memorial performance
 *   with no real operational competence behind it; hybrid_reading:
 *   preparedness as a layered system where memorial elements stabilize
 *   commitment while a competence core still does real work) are separate
 *   constraints with their own ε values and are not blended into this one.
 *   Where the husk reading would show high theater_ratio and low genuine
 *   capacity transfer, and the hybrid reading would show a mixed profile,
 *   this reading is authored with low theater_ratio and low extractiveness
 *   throughout the interval, reflecting a functioning coordination mechanism
 *   rather than a decayed or partially-decayed one.
 *
 * KEY AGENTS:
 *   - emergency_management_agencies: agenda_setter (institutional/constrained) — designs and enforces the drill regime, bears direct cost
 *   - frontline_responders: beneficiary/payer (moderate/constrained) — undergo drills, transmit and receive competence
 *   - at_risk_populations: beneficiary (powerless/trapped) — depend entirely on the competence being real
 *   - incoming_recruits: beneficiary/payer (powerless/constrained) — the generational-transmission mechanism itself
 *   - independent_auditors: observer (analytical/analytical) — verifies the competence claim from outside the benefiting parties
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(preparedness_commitment__competence_reading, 0.18).
domain_priors:suppression_score(preparedness_commitment__competence_reading, 0.22).
domain_priors:theater_ratio(preparedness_commitment__competence_reading, 0.12).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(preparedness_commitment__competence_reading, extractiveness, 0.18).
narrative_ontology:constraint_metric(preparedness_commitment__competence_reading, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(preparedness_commitment__competence_reading, theater_ratio, 0.12).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(preparedness_commitment__competence_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(preparedness_commitment__competence_reading, resistance, 0.25).

% --- Constraint claim ---
narrative_ontology:constraint_claim(preparedness_commitment__competence_reading, rope).
narrative_ontology:human_readable(preparedness_commitment__competence_reading, "Preparedness as Live Exercised Competence (Competence Reading)").
narrative_ontology:topic_domain(preparedness_commitment__competence_reading, "disaster preparedness/institutional memory").

domain_priors:requires_active_enforcement(preparedness_commitment__competence_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(preparedness_commitment__competence_reading, 'cb05f219-77e8-4e6a-8e17-60874a3e6d76').
narrative_ontology:cs_kernel_codification('cb05f219-77e8-4e6a-8e17-60874a3e6d76', distributed).
narrative_ontology:cs_authority_grounding('cb05f219-77e8-4e6a-8e17-60874a3e6d76', practice).
narrative_ontology:cs_interpretation_layer_present('cb05f219-77e8-4e6a-8e17-60874a3e6d76').
narrative_ontology:cs_reading_relation('cb05f219-77e8-4e6a-8e17-60874a3e6d76', preparedness_commitment__husk_reading, coexists_with).
narrative_ontology:cs_reading_relation('cb05f219-77e8-4e6a-8e17-60874a3e6d76', preparedness_commitment__hybrid_reading, influences).
narrative_ontology:cs_axiom('cb05f219-77e8-4e6a-8e17-60874a3e6d76', foundational, drills_produce_verifiable_decision_competence).
narrative_ontology:cs_axiom_status(drills_produce_verifiable_decision_competence, holdable).
narrative_ontology:cs_axiom_grounding('cb05f219-77e8-4e6a-8e17-60874a3e6d76', drills_produce_verifiable_decision_competence, empirically_contingent).
narrative_ontology:cs_axiom('cb05f219-77e8-4e6a-8e17-60874a3e6d76', secondary, generational_transmission_is_institutionally_robust).
narrative_ontology:cs_axiom_status(generational_transmission_is_institutionally_robust, holdable).
narrative_ontology:cs_axiom_grounding('cb05f219-77e8-4e6a-8e17-60874a3e6d76', generational_transmission_is_institutionally_robust, empirically_contingent).
narrative_ontology:cs_reference_frame('cb05f219-77e8-4e6a-8e17-60874a3e6d76', live_exercise_produces_real_capacity).
narrative_ontology:cs_drift_state('cb05f219-77e8-4e6a-8e17-60874a3e6d76', contemporary_multi_generational_turnover, gap(stable, minor, true)).
narrative_ontology:cs_created_at('cb05f219-77e8-4e6a-8e17-60874a3e6d76', '').
narrative_ontology:cs_kernel_id(preparedness_commitment__competence_reading, preparedness_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(preparedness_commitment__competence_reading, frontline_responders).
narrative_ontology:constraint_beneficiary(preparedness_commitment__competence_reading, at_risk_populations).
narrative_ontology:constraint_beneficiary(preparedness_commitment__competence_reading, emergency_management_agencies).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(preparedness_commitment__competence_reading, incoming_recruits).
narrative_ontology:constraint_victim(preparedness_commitment__competence_reading, frontline_responders).
narrative_ontology:constraint_victim(preparedness_commitment__competence_reading, incoming_recruits).
narrative_ontology:constraint_vindicates(preparedness_commitment__competence_reading, operational_readiness_requires_live_practice).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Designs and mandates the drill cadence, scenario complexity, and after-action review process. Bears the direct cost of running realistic exercises (time, materiel, opportunity cost of staff hours) and is accountable when a real event exposes a gap the drills should have caught. Cannot exit the obligation to prepare, only vary how rigorously it does so.
narrative_ontology:constraint_stakeholder(preparedness_commitment__competence_reading, emergency_management_agencies, agenda_setter,
    institutional, generational, constrained, regional).

% Undergoes the repeated drills, rotates through incident-command roles, and absorbs new personnel through hands-on scenario training rather than paperwork. Benefits from genuinely higher survival odds and confidence during real incidents, but pays in the form of ongoing time commitment, physical and cognitive load, and exposure to realistic failure during exercises.
narrative_ontology:constraint_stakeholder(preparedness_commitment__competence_reading, frontline_responders, beneficiary,
    moderate, biographical, constrained, local).
narrative_ontology:stakeholder_secondary_role(preparedness_commitment__competence_reading, frontline_responders, payer).

% Lives in the jurisdiction the responders serve and has no direct role in designing or running the drills, but is the party whose outcomes (rescue speed, evacuation coordination, resource distribution) depend entirely on whether the responders' competence is real rather than performed. Cannot verify readiness directly and must trust the exercised-competence signal.
narrative_ontology:constraint_stakeholder(preparedness_commitment__competence_reading, at_risk_populations, beneficiary,
    powerless, generational, trapped, regional).

% New personnel entering the system after founders or veteran responders retire. Must acquire tacit, hard-to-codify judgment through repeated live exercises rather than manuals alone. Their competence is the mechanism by which the constraint survives generational turnover; if the transmission fails, the whole reading collapses into the husk reading.
narrative_ontology:constraint_stakeholder(preparedness_commitment__competence_reading, incoming_recruits, beneficiary,
    powerless, biographical, constrained, local).
narrative_ontology:stakeholder_secondary_role(preparedness_commitment__competence_reading, incoming_recruits, payer).

% External evaluators (inspector-general offices, academic disaster-research teams, cross-jurisdictional review boards) who assess whether drills produce measurable decision-quality improvements or merely satisfy compliance checklists. Their findings are the primary evidence separating this reading from the husk reading.
narrative_ontology:constraint_stakeholder(preparedness_commitment__competence_reading, independent_auditors, observer,
    analytical, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(preparedness_commitment__competence_reading, diffuse).
narrative_ontology:fixing_cost_class(preparedness_commitment__competence_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the genuine problem that operational competence in rare, high-stakes events (disasters, mass-casualty incidents) decays without practice and cannot be transmitted by documentation alone — live, repeated, realistic exercise is the only known mechanism for maintaining and passing down the tacit judgment responders need when it matters.
% TRANSFER_FUNCTION: Moves scarce organizational time, budget, and cognitive effort from routine operations into recurring exercise cycles; in return it moves real decision-competence from veteran responders into incoming recruits and sustains it across the workforce, with the ultimate beneficiary being the at-risk population whose outcomes depend on that competence being genuine at the moment of a real event.
% ABSENT_VOICES: Populations who will be affected by the NEXT disaster are not present to certify readiness in advance — they can only be represented by proxy through auditors and historical incident review. If the drills degrade into performance, this absent constituency has no voice until the moment competence is tested for real.
% DISAPPEARANCE_RATIONALE: If exercised, competence-based preparedness vanished and were replaced by documentation-only compliance, decision-quality during real incidents would measurably degrade within one or two personnel turnover cycles as tacit judgment failed to transmit; response times, coordination accuracy, and survival outcomes would visibly worsen, and independent post-incident reviews would detect the gap.
% FOUNDING_PROBLEM: Institutional knowledge of how to actually respond under pressure — as opposed to what the plan says to do — is lost when veteran personnel leave unless it is actively re-embodied in new personnel through realistic practice; static manuals and org charts do not transmit judgment.
% FOUNDING_PROBLEM_CORROBORATION: Independent academic disaster-research teams and inspector-general after-action reviews (parties with no stake in the agency's budget or reputation) attest that jurisdictions with high-fidelity, frequently-updated exercise regimes show measurably better real-incident coordination outcomes than those relying on paper plans alone — this corroboration comes from outside the agencies that run and benefit from the drills.
narrative_ontology:disappearance_verdict(preparedness_commitment__competence_reading, world_rearranges).
narrative_ontology:founding_problem_status(preparedness_commitment__competence_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(preparedness_commitment__competence_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(preparedness_commitment__competence_reading, 'none', 1).
narrative_ontology:epsilon_provenance(preparedness_commitment__competence_reading, 0.18, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(preparedness_commitment__competence_reading_tests).
:- end_tests(preparedness_commitment__competence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored low (0.18 at interval end) because the resources consumed by the drill regime are directly converted into the coordination good (real capacity) rather than diverted to rent extraction; there is no identified party that captures value without contributing to or benefiting from the readiness function. Suppression is moderate-low (0.22): some active enforcement is needed (mandated drill attendance, budget allocation against competing priorities) but it is not coercive suppression of alternatives — no exit is being blocked for anyone's benefit. Theater ratio is kept low and only slightly rising (0.08 to 0.12) to reflect that even genuine competence systems accumulate a small amount of unavoidable compliance overhead over decades, but this reading's defining structural claim is that the overhead never comes to dominate the function. Accessibility collapse is moderate (0.35): alternative preparedness models exist and are debated, so this is not treated as a natural-law mountain — it is a chosen, actively-maintained coordination structure.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter (emergency_management_agencies) and the beneficiary populations (at_risk_populations) would compute similarly under this reading precisely because the structural claim is that competence is genuinely transmitted — there is no significant divergence between administrator self-report and outcome for the populations served, which is the diagnostic signature that separates this reading from the husk reading, where the administrator's self-report of readiness would diverge sharply from actual incident outcomes.
 *
 * DIRECTIONALITY LOGIC:
 *   No stakeholder is authored as a victim under this reading — the beneficiary declarations (frontline_responders, at_risk_populations, emergency_management_agencies) reflect a story in which the cost-bearing (drill time, training burden) is incurred by the same parties who receive the competence benefit, which is the structural mark of a Rope rather than a Tangled Rope or Snare. incoming_recruits and frontline_responders carry secondary payer roles because they bear the training burden directly, but this cost is investment in their own future competence, not extraction to a third party.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading explicitly resists mandatrophy: the founding_problem (loss of tacit operational judgment across personnel turnover) is declared live, and its corroboration comes from independent academic and inspector-general sources rather than the agencies' own self-assessment — this is precisely the check that prevents a stale mandate from being relabeled as functioning. If future measurements showed theater_ratio climbing past extractiveness while incident outcomes stagnated or worsened, that would be evidence this specific reading is empirically wrong for the jurisdiction in question and the husk_reading would be the better-fitting constraint there instead.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    competence_verification_gap,
    'How would an outside observer distinguish this competence reading from the husk reading without waiting for an actual disaster to reveal the difference?',
    'Structured after-action review comparing drill decision-quality metrics (scenario branching complexity, time-to-correct-decision, error recovery) against independently audited real-incident outcomes over multiple personnel generations; a sustained correlation supports the competence reading, a divergence supports the husk reading.',
    'If verification consistently fails to distinguish the readings, that is evidence the kernel''s actual instantiation in this jurisdiction sits closer to the hybrid or husk reading regardless of which reading was claimed, and the story should be revised or replaced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(competence_verification_gap, empirical, 'Whether observable evidence can reliably separate genuine competence transmission from performed retention.').

omega_variable(
    generational_transmission_fragility,
    'Is the successful absorption of generational turnover (avoiding the D5 break) a robust structural feature of this reading, or a fragile achievement that depends on specific veteran personnel currently in place?',
    'Track competence metrics across at least one full leadership/founder turnover cycle; if competence holds through the transition without the founding cohort present, transmission is structural rather than personality-dependent.',
    'If transmission proves personality-dependent rather than institutionalized, the constraint is at risk of drifting into the husk reading the moment the current veteran cohort retires, even though it currently qualifies as the competence reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(generational_transmission_fragility, empirical, 'Whether the D5-break avoidance is durable across leadership succession or contingent on current personnel.').

omega_variable(
    kernel_reading_framing_choice,
    'Is the choice to model this jurisdiction''s preparedness system under the competence reading (rather than the hybrid reading) justified by the available evidence, or is it an optimistic framing that a stricter audit would revise downward to hybrid?',
    'Compare this story''s authored metrics against the hybrid_reading story''s metrics using the same independent audit evidence; if the audit trail shows meaningful memorial/performative elements coexisting with genuine competence, the hybrid framing is the better fit and this story should be flagged for reclassification.',
    'Choosing the wrong reading for a given real jurisdiction misattributes both the extraction profile and the resilience of the preparedness system; the competence reading understates risk if memorial elements are actually load-bearing.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_framing_choice, conceptual, 'Whether the competence-reading framing is the best-supported reading for a given real-world case versus the hybrid reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(preparedness_commitment__competence_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(prep_tr_t0, preparedness_commitment__competence_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement(prep_tr_t8, preparedness_commitment__competence_reading, theater_ratio, 8, 0.09).
narrative_ontology:measurement(prep_tr_t16, preparedness_commitment__competence_reading, theater_ratio, 16, 0.1).
narrative_ontology:measurement(prep_tr_t24, preparedness_commitment__competence_reading, theater_ratio, 24, 0.1).
narrative_ontology:measurement(prep_tr_t32, preparedness_commitment__competence_reading, theater_ratio, 32, 0.11).
narrative_ontology:measurement(prep_tr_t40, preparedness_commitment__competence_reading, theater_ratio, 40, 0.12).

% Extraction over time
narrative_ontology:measurement(prep_be_t0, preparedness_commitment__competence_reading, base_extractiveness, 0, 0.14).
narrative_ontology:measurement(prep_be_t8, preparedness_commitment__competence_reading, base_extractiveness, 8, 0.15).
narrative_ontology:measurement(prep_be_t16, preparedness_commitment__competence_reading, base_extractiveness, 16, 0.16).
narrative_ontology:measurement(prep_be_t24, preparedness_commitment__competence_reading, base_extractiveness, 24, 0.17).
narrative_ontology:measurement(prep_be_t32, preparedness_commitment__competence_reading, base_extractiveness, 32, 0.17).
narrative_ontology:measurement(prep_be_t40, preparedness_commitment__competence_reading, base_extractiveness, 40, 0.18).

% Suppression requirement over time
narrative_ontology:measurement(prep_su_t0, preparedness_commitment__competence_reading, suppression_requirement, 0, 0.2).
narrative_ontology:measurement(prep_su_t8, preparedness_commitment__competence_reading, suppression_requirement, 8, 0.2).
narrative_ontology:measurement(prep_su_t16, preparedness_commitment__competence_reading, suppression_requirement, 16, 0.21).
narrative_ontology:measurement(prep_su_t24, preparedness_commitment__competence_reading, suppression_requirement, 24, 0.21).
narrative_ontology:measurement(prep_su_t32, preparedness_commitment__competence_reading, suppression_requirement, 32, 0.22).
narrative_ontology:measurement(prep_su_t40, preparedness_commitment__competence_reading, suppression_requirement, 40, 0.22).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(preparedness_commitment__competence_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(preparedness_commitment__competence_reading, 0.1).
narrative_ontology:affects_constraint(preparedness_commitment__competence_reading, preparedness_commitment__husk_reading).
narrative_ontology:affects_constraint(preparedness_commitment__competence_reading, preparedness_commitment__hybrid_reading).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the preparedness_commitment kernel. competence_reading (this story) claims genuine, low-extraction coordination with successful generational transmission. husk_reading claims the same surface routines persist as memorial performance with negligible real capacity transfer and correspondingly higher theater_ratio and effective extraction (resources consumed without producing the claimed good). hybrid_reading claims a layered structure where memorial/legitimacy-stabilizing elements coexist with a genuine competence core, producing an intermediate metric profile. Each story carries its own ε, its own stakeholder set, and its own classification; they are linked here via affects_constraints rather than merged into a single observer-relative story, per the ε-invariance principle.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
