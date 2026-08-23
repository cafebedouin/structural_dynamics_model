% ============================================================================
% CONSTRAINT STORY: preparedness_commitment__competence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:suppression_profile/2,
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
 *   constraint_id: preparedness_commitment__competence_reading
 *   human_readable: Preparedness Commitment as Live Exercised Knowledge (Competence Reading)
 *   domain: institutional/disaster-preparedness
 *
 * SUMMARY:
 *   A regional disaster-response agency maintains a standing preparedness
 *   commitment: a scheduled regime of drills, exercises, and training through
 *   which operational capacity is exercised, evaluated, and handed across
 *   generational turnover. The arrangement exists because response skill is
 *   perishable and its payoff is rare — individual responders have no private
 *   incentive to maintain it, veterans retire carrying tacit knowledge with
 *   them, and documentation alone does not preserve judgment. The commitment
 *   converts that decay problem into scheduled collective maintenance: drills
 *   that present novel decision points, scored exercises that feed
 *   after-action redesign, and a training pipeline that brings incoming
 *   cohorts to readiness. The costs — drill hours and exercise budget — are
 *   consumed as maintained capacity that the bearing seats themselves collect
 *   as competence and safety margin; no seat converts the arrangement's flows
 *   into private rent.
 *
 * KEY AGENTS:
 *   - preparedness_program_office: agenda setter (institutional/constrained) — designs the exercise calendar, administers drill mandates, converts budget into exercise delivery, and redesigns drills the evaluation cell flags as scripted
 *   - veteran_responders: primary beneficiary (organized/mobile) — hold the tacit knowledge the regime maintains; drill time sharpens judgment they already exercise; their competence is portable across agencies
 *   - incoming_responder_cohorts: primary beneficiary (moderate/constrained) — bear the largest drill-time share and acquire competence they could not assemble alone; their year-five readiness is the regime's product
 *   - served_populations: beneficiary (organized/constrained) — fund the system and hold a contingent claim on its output; experience the arrangement only on the rare day it is needed
 *   - budget_authority: funder and partial agenda setter (institutional/arbitrage) — appropriates the budget against a benefit it can never directly observe; its immediate horizon is the arrangement's structural fragility
 *   - exercise_evaluation_cell: analytical observer (institutional/analytical) — injects no-notice scenarios, scores decision quality, and flags drills that have become script-rehearsal
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(preparedness_commitment__competence_reading, 0.22).
domain_priors:suppression_score(preparedness_commitment__competence_reading, 0.2).
domain_priors:theater_ratio(preparedness_commitment__competence_reading, 0.18).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(preparedness_commitment__competence_reading, extractiveness, 0.22).
narrative_ontology:constraint_metric(preparedness_commitment__competence_reading, suppression_requirement, 0.2).
narrative_ontology:constraint_metric(preparedness_commitment__competence_reading, theater_ratio, 0.18).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(preparedness_commitment__competence_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(preparedness_commitment__competence_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(preparedness_commitment__competence_reading, rope).
narrative_ontology:human_readable(preparedness_commitment__competence_reading, "Preparedness Commitment as Live Exercised Knowledge (Competence Reading)").
narrative_ontology:topic_domain(preparedness_commitment__competence_reading, "institutional/disaster-preparedness").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(preparedness_commitment__competence_reading, 'ef40f96b-425e-47e8-92ae-0c96f5756064').
narrative_ontology:cs_kernel_codification('ef40f96b-425e-47e8-92ae-0c96f5756064', formalized).
narrative_ontology:cs_authority_grounding('ef40f96b-425e-47e8-92ae-0c96f5756064', expertise).
narrative_ontology:cs_interpretation_layer_present('ef40f96b-425e-47e8-92ae-0c96f5756064').
narrative_ontology:cs_reading_relation('ef40f96b-425e-47e8-92ae-0c96f5756064', preparedness_commitment__husk_reading, forecloses).
narrative_ontology:cs_reading_relation('ef40f96b-425e-47e8-92ae-0c96f5756064', preparedness_commitment__hybrid_reading, influences).
narrative_ontology:cs_axiom('ef40f96b-425e-47e8-92ae-0c96f5756064', foundational, exercised_knowledge_persists_only_through_use).
narrative_ontology:cs_axiom_status(exercised_knowledge_persists_only_through_use, holdable).
narrative_ontology:cs_axiom_grounding('ef40f96b-425e-47e8-92ae-0c96f5756064', exercised_knowledge_persists_only_through_use, empirically_contingent).
narrative_ontology:cs_axiom('ef40f96b-425e-47e8-92ae-0c96f5756064', foundational, drill_fidelity_determines_generational_transfer).
narrative_ontology:cs_axiom_status(drill_fidelity_determines_generational_transfer, holdable).
narrative_ontology:cs_axiom_grounding('ef40f96b-425e-47e8-92ae-0c96f5756064', drill_fidelity_determines_generational_transfer, instrumental).
narrative_ontology:cs_reference_frame('ef40f96b-425e-47e8-92ae-0c96f5756064', exercised_competence_baseline).
narrative_ontology:cs_drift_state('ef40f96b-425e-47e8-92ae-0c96f5756064', post_founder_generation_operations, gap(practice_drift, minor, true)).
narrative_ontology:cs_created_at('ef40f96b-425e-47e8-92ae-0c96f5756064', '').
narrative_ontology:cs_kernel_id(preparedness_commitment__competence_reading, preparedness_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(preparedness_commitment__competence_reading, veteran_responders).
narrative_ontology:constraint_beneficiary(preparedness_commitment__competence_reading, incoming_responder_cohorts).
narrative_ontology:constraint_beneficiary(preparedness_commitment__competence_reading, served_populations).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(preparedness_commitment__competence_reading, budget_authority).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Designs the annual exercise calendar, sets drill schedules, maintains doctrine, and runs after-action review. Converts the exercise budget into scenario design, evaluator time, and training delivery. Its authority rests on operational credibility rather than rank alone. It monitors its own calendar for drills that have become scripted and redesigns them; it cannot leave the mandate without leaving the profession.
narrative_ontology:constraint_stakeholder(preparedness_commitment__competence_reading, preparedness_program_office, agenda_setter,
    institutional, generational, constrained, regional).

% Carry the tacit knowledge the regime maintains — incident command judgment, equipment quirks, the unwritten parts of doctrine. Drill hours sharpen skills they already exercise on calls. They teach in the pipeline, which converts their experience into institutional form before they retire. Their competence is portable: if they change agencies or retire, the skill goes with them, so the regime's value to them survives any single employer.
narrative_ontology:constraint_stakeholder(preparedness_commitment__competence_reading, veteran_responders, beneficiary,
    organized, biographical, mobile, regional).

% Enter with certifications but not yet with judgment. They bear the largest drill-time share of any cohort and acquire through the pipeline a competence they could not assemble alone in the field. Their readiness at year five is the regime's product. Leaving mid-pipeline forfeits the accumulated training, so their exit is costly to them personally.
narrative_ontology:constraint_stakeholder(preparedness_commitment__competence_reading, incoming_responder_cohorts, beneficiary,
    moderate, biographical, constrained, regional).

% Fund the system through taxes and hold a contingent claim on its output — a claim exercised only on the rare day a disaster arrives. They have no seat in drill design and no way to observe the capacity they are paying to maintain; they experience the arrangement only through outcomes, and usually through outcomes that never become visible because the bad day was handled.
narrative_ontology:constraint_stakeholder(preparedness_commitment__competence_reading, served_populations, beneficiary,
    organized, generational, constrained, regional).

% Appropriates the exercise budget each cycle and can redirect it toward visible projects at any time. Receives insurance value it can never directly observe: the benefit is the catastrophe that does not happen, in a fiscal year that has ended. Faces standing pressure from constituents and line agencies to convert preparedness line-items into tangible spending. Its short horizon, not its position, is what makes the arrangement fragile.
narrative_ontology:constraint_stakeholder(preparedness_commitment__competence_reading, budget_authority, beneficiary,
    institutional, immediate, arbitrage, regional).
narrative_ontology:stakeholder_secondary_role(preparedness_commitment__competence_reading, budget_authority, agenda_setter).

% Runs no-notice scenario injection and scores decision quality against doctrine, benchmarking against peer agencies and national standards. Flags drills whose scenarios have become scripted and whose outcomes are predetermined. Its findings feed exercise redesign. It collects nothing from the arrangement and pays nothing into it; its seat exists to keep the regime's self-assessment honest.
narrative_ontology:constraint_stakeholder(preparedness_commitment__competence_reading, exercise_evaluation_cell, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(preparedness_commitment__competence_reading, diffuse).
narrative_ontology:fixing_cost_class(preparedness_commitment__competence_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the generational knowledge-decay problem: individually perishable skills with rare payoffs have no private maintenance incentive, and veterans retire carrying tacit knowledge out of the organization. The scheduled drill-and-training regime converts that decay into collective maintenance — capacity is exercised, scored, corrected, and handed to incoming cohorts on a calendar no individual would keep alone.
% TRANSFER_FUNCTION: Moves time, attention, and budget from current operations and current fiscal cycles into capacity held in reserve; moves tacit operational knowledge from veteran cohorts into institutional routines, scored exercises, and incoming personnel.
% ABSENT_VOICES: Taxpaying residents have no seat in drill design though they fund it. More structurally, no one represents the failure modes that are never drilled: the scenario outside the exercise calendar has no advocate, so the regime's blind spots are chosen by no one and visible to no one until a disaster occupies them.
% DISAPPEARANCE_RATIONALE: Capacity would decay on the timescale of cohort turnover: within five to ten years the veterans holding tacit knowledge would retire, documentation would persist while competence did not, and the first major event would expose the gap. Mutual-aid partners would re-plan around the weakened agency, and the served population would absorb the difference as casualties and losses.
% FOUNDING_PROBLEM: Historical events in which an agency that had once responded competently discovered mid-disaster that turnover had hollowed it — veterans gone, drills lapsed, documents current and useless — prompted the standing commitment: schedule the exercise of capacity so it cannot silently decay between the people who have it and the people who will need it.
% FOUNDING_PROBLEM_CORROBORATION: Peer-agency after-action reviews and the disaster-medicine skill-decay literature attest the founding problem from outside the benefiting parties; municipal audit offices in jurisdictions that suspended drill programs have documented measurable response-capacity loss, and mutual-aid partners report re-planning around agencies whose exercise tempo lapsed.
narrative_ontology:disappearance_verdict(preparedness_commitment__competence_reading, world_rearranges).
narrative_ontology:founding_problem_status(preparedness_commitment__competence_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(preparedness_commitment__competence_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(preparedness_commitment__competence_reading, 'none', 1).
narrative_ontology:epsilon_provenance(preparedness_commitment__competence_reading, 0.22, 'stealth/ox-alpha', 'none', direct).

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
 *   Extraction is low (0.22 at interval end) because the arrangement's costs are the price of capacity the bearing seats themselves consume; the series drifts gently upward (0.16 to 0.22) over the thirty-year interval as the agency scales and drill overhead accumulates. Suppression (0.20) is a raw structural property, unscaled by the engine's context dimensions: drill attendance is mandatory and tracked, but the mandate rests on professional standards and demonstrated value rather than on barriers blocking exit or foreclosing alternative training approaches. Theater (0.18) reflects real but contained ritual creep: a minority of drills have drifted toward scripted rehearsal, the evaluation cell flags them, and redesign pulls them back — the series rises 0.10 to 0.18 without approaching substitution. Accessibility_collapse (0.35) is rope-typical: apprenticeship-heavy, simulation-first, and documentation-first alternatives remain workable, so understanding the regime does not collapse its competitors. Resistance (0.30) is the recurring friction of drill fatigue and lost operational time, borne mostly by the same cohorts that collect the competence. The claimed type (rope) is authored from the structure — universal net benefit, minimal coercive overhead, no suppressed alternative — independently of the metric values; the engine computes per-seat classifications from the structural data, and any divergence between claim and computed type is the measurement the corpus exists to take.
 *
 * PERSPECTIVAL GAP:
 *   No seat bears uncompensated extraction, but the seats still compute differently. The program office experiences the arrangement as the agency's spine and its own professional mandate; the budget authority experiences it as deferrable spend against a benefit that is structurally invisible — the catastrophe that does not happen — and therefore perpetually re-litigable; served populations hold a contingent claim they will likely never exercise and cannot observe; veterans experience drill time as sharpening they would partly do anyway; incoming cohorts experience it as the price of admission to competence. The same schedule is stewardship from one seat, discretionary spend from another, invisible insurance from a third, and formation from a fourth. The engine computes these divergences from power, horizon, and exit — not from the authored claim.
 *
 * DIRECTIONALITY LOGIC:
 *   Every declared party benefits, so derived directionality sits near the beneficiary end for all seats, modulated by exit and horizon. Veterans sit nearest the beneficiary pole: their exit is mobile and the competence the regime maintains is portable — they keep it if they leave. Incoming cohorts are slightly less favored: constrained exit ties their gain to the agency. Served populations are pure beneficiaries with trapped exit — they hold the contingent claim and cannot arbitrage it. The budget authority is the near-symmetric case: it pays the full budget in cash terms, but the payment is one it appropriates voluntarily each cycle and the benefit is real insurance; its fragility is temporal (immediate horizon, arbitrage exit) rather than directional, so no directionality override is authored — the derivation from the declared benefit plus arbitrage exit already places it correctly. No victims are declared because drill time converts into the driller's own competence: there is no seat bearing costs it does not recoup.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — generational skill decay — is a standing condition of organized response work, not a transitional one, so the arrangement is not transitional support and carries no sunset; mandatrophy in the usual sense (a mandate outliving its function) does not apply. The live risk runs the opposite direction: a functional mandate decaying into ceremony while retaining its form. The theater_ratio series is the instrument for that drift — it rises across the interval as the agency scales and some drills ritualize, but stays well below substitution because the evaluation cell's no-notice injection keeps a fraction of every exercise calendar honest. The identity-coordination character cuts both ways: professional identity — a responder is someone who drills — is what makes the arrangement self-sustaining at low enforcement cost, and the same identity could drift into self-certification if the evaluative seat were captured by the trained. No seat is identity-locked in the pathological sense; the evaluation cell's independence is what keeps the professional identity from becoming its own auditor. Fixing the observed deficiency is cheap — exercise redesign is inside the program office's routine competence — which is why fixing_cost is authored cheap.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reading_instantiation_audit,
    'Does the observed drill regime instantiate this reading — routines as live exercised knowledge — or would an exercise-fidelity audit find the same standing arrangement closer to memorial performance or a layered mix?',
    'Independent exercise-fidelity audit: sample drills across the exercise calendar, score whether scenarios present novel decision points, and determine whether personnel judgment or script-recall is what the drill actually tests and certifies.',
    'The low extraction and rope classification authored here hold under this reading''s lights; if a fidelity audit found script-rehearsal dominant, the same standing arrangement would be a different constraint with high theater and negligible function — authored as a sibling reading of the same kernel with its own epsilon.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_instantiation_audit, empirical, 'Which reading of the preparedness_commitment kernel the observed arrangement actually instantiates.').

omega_variable(
    generational_transfer_efficacy,
    'Does the training pipeline actually absorb generational turnover — do post-turnover cohorts perform at veteran baseline in no-notice exercises?',
    'Longitudinal comparison of no-notice exercise scores for cohorts trained entirely inside the regime against the last veteran-cohort baseline, controlling for scenario difficulty and exercise conditions.',
    'Confirms or breaks the turnover-containment claim: if transfer fails, the regime''s coordination function is weaker than authored, its cost-to-function ratio rises, and the rope classification becomes unstable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(generational_transfer_efficacy, empirical, 'Whether the regime''s training genuinely carries capacity across generational turnover.').

omega_variable(
    insurance_value_visibility,
    'Can a benefit structured as a counterfactual — the disaster that does not happen — sustain budgetary support across successive short-horizon funding cycles?',
    'Longitudinal budget analysis across administrations: track the preparedness line-item through quiet cycles, leadership turnover, and fiscal stress; compare jurisdictions with and without statutory floor protections for exercise funding.',
    'If support decays in quiet cycles, the regime''s funding base is structurally fragile and its long-run trajectory bends toward ceremonial minimum regardless of current function; if professional norms or statutory floors hold the line, the fragility is contained.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(insurance_value_visibility, empirical, 'Whether counterfactual benefits can sustain funding across short-horizon budget politics.').

omega_variable(
    ceremonial_element_status,
    'Are ceremonial drill elements necessarily decay — residue to be redesigned away — or can ritual performance itself stabilize commitment in ways that maintain capacity indirectly?',
    'Comparative analysis of agencies that purged ceremonial elements versus those that retained them: measure commitment retention, recruiting, and no-notice performance differentials over time.',
    'If ritual elements carry stabilizing function, part of the measured theater_ratio is mispriced as decay and the arrangement''s true structure is layered rather than uniformly functional; if they carry none, the redesign imperative stands unqualified. The disagreement between readings of this kernel is located exactly here.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(ceremonial_element_status, conceptual, 'Whether ceremonial elements are pure decay or indirect commitment-stabilizers — the locus of the kernel''s reading disagreement.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(preparedness_commitment__competence_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(prep_tr_t0, preparedness_commitment__competence_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(prep_tr_t6, preparedness_commitment__competence_reading, theater_ratio, 6, 0.12).
narrative_ontology:measurement(prep_tr_t12, preparedness_commitment__competence_reading, theater_ratio, 12, 0.13).
narrative_ontology:measurement(prep_tr_t18, preparedness_commitment__competence_reading, theater_ratio, 18, 0.15).
narrative_ontology:measurement(prep_tr_t24, preparedness_commitment__competence_reading, theater_ratio, 24, 0.17).
narrative_ontology:measurement(prep_tr_t30, preparedness_commitment__competence_reading, theater_ratio, 30, 0.18).

% Extraction over time
narrative_ontology:measurement(prep_be_t0, preparedness_commitment__competence_reading, base_extractiveness, 0, 0.16).
narrative_ontology:measurement(prep_be_t6, preparedness_commitment__competence_reading, base_extractiveness, 6, 0.18).
narrative_ontology:measurement(prep_be_t12, preparedness_commitment__competence_reading, base_extractiveness, 12, 0.19).
narrative_ontology:measurement(prep_be_t18, preparedness_commitment__competence_reading, base_extractiveness, 18, 0.2).
narrative_ontology:measurement(prep_be_t24, preparedness_commitment__competence_reading, base_extractiveness, 24, 0.21).
narrative_ontology:measurement(prep_be_t30, preparedness_commitment__competence_reading, base_extractiveness, 30, 0.22).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(preparedness_commitment__competence_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(preparedness_commitment__competence_reading, identity_coordination).
narrative_ontology:affects_constraint(preparedness_commitment__competence_reading, preparedness_commitment__husk_reading).
narrative_ontology:affects_constraint(preparedness_commitment__competence_reading, preparedness_commitment__hybrid_reading).

% DUAL FORMULATION NOTE:
% The preparedness_commitment kernel decomposes into readings with distinct epsilon values and distinct structural profiles rather than one observable-dependent constraint. This file authors the competence reading: the standing drill-and-training regime as live exercised knowledge, with low extraction (costs are coordination price, not captured rent). The husk reading authors the same standing regime as memorial performance (high theater, negligible function); the hybrid reading authors it as a layered system. The observable drill activity is shared across readings; epsilon diverges because the readings disagree about what the activity does. The competence reading is upstream in legitimacy: its documented successes are the resource the sibling framings borrow or contest, which is why the edges here point at both siblings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
