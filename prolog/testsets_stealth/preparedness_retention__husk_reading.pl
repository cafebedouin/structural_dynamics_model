% ============================================================================
% CONSTRAINT STORY: preparedness_retention__husk_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
 *   constraint_id: preparedness_retention__husk_reading
 *   human_readable: Flood-Preparedness Drill Cycle as Memorial Performance (Husk Reading)
 *   domain: governance/institutional-memory/disaster-preparedness
 *
 * SUMMARY:
 *   This story instantiates the husk reading of the preparedness_retention
 *   kernel for the Dutch flood-governance complex: the claim that the
 *   national drill-and-inspection cycle — safety-region multi-disciplinary
 *   exercises, water-authority rehearsals, Rijkswaterstaat scenario
 *   walk-throughs, and the inspection and scoring apparatus around them —
 *   operates as memorial performance. On this reading the cycle feels like
 *   retention and is legible as retention in every compliance artifact, yet
 *   converts little of its input into retrievable stress capacity. The cohort
 *   that carried lived flood memory (1953, reinforced by the 1993 and 1995
 *   near-floods) retired across the interval; as its memory left, exercises
 *   grew larger, more scripted, and more scored; and the 2021 Limburg floods
 *   offered a partial natural experiment in which rehearsed procedure and
 *   live performance visibly diverged. The epsilon referent is the standing
 *   drill-and-inspection arrangement as this reading sees it — heavily
 *   extractive, because it draws funds, duty-hours, and craft time and
 *   returns ceremony plus manufactured assurance. The sibling readings
 *   (competence_reading, hybrid_reading) are separate constraints with their
 *   own epsilon, beneficiary structures, and classifications; nothing about
 *   them is averaged into this file. KEY AGENTS (by structural relationship):
 *   - national_exercise_authorities: Agenda setter (institutional/arbitrage)
 *   — owns the framework, collects relevance and budget -
 *   safety_region_directors: Dual beneficiary-administrator
 *   (institutional/constrained) — performs the rituals locally, draws
 *   legitimacy from them - exercise_industry_contractors: Concentrated
 *   monetary beneficiary (organized/arbitrage) — converts the cycle directly
 *   into revenue - frontline_responders: Payer (organized/constrained) —
 *   surrenders duty hours to script - dike_and_barrage_operators: Payer with
 *   craft identity fused to the ritual (organized/identity-locked) -
 *   general_taxpayer_base: Diffuse funder (moderate/mobile) -
 *   future_flood_affected_population: Ultimate bearer of the capacity gap
 *   (powerless/trapped) — temporally displaced victim -
 *   safety_services_inspectorate: Observer (institutional/analytical) -
 *   exercise_realism_researchers: Excluded critic (moderate/analytical)
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(preparedness_retention__husk_reading, 0.68).
domain_priors:suppression_score(preparedness_retention__husk_reading, 0.55).
domain_priors:theater_ratio(preparedness_retention__husk_reading, 0.78).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(preparedness_retention__husk_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(preparedness_retention__husk_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(preparedness_retention__husk_reading, theater_ratio, 0.78).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(preparedness_retention__husk_reading, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(preparedness_retention__husk_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(preparedness_retention__husk_reading, snare).
narrative_ontology:human_readable(preparedness_retention__husk_reading, "Flood-Preparedness Drill Cycle as Memorial Performance (Husk Reading)").
narrative_ontology:topic_domain(preparedness_retention__husk_reading, "governance/institutional-memory/disaster-preparedness").

domain_priors:requires_active_enforcement(preparedness_retention__husk_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(preparedness_retention__husk_reading, '629869ec-b012-415f-a04c-f8a52b5f0a4f').
narrative_ontology:cs_kernel_codification('629869ec-b012-415f-a04c-f8a52b5f0a4f', formalized).
narrative_ontology:cs_authority_grounding('629869ec-b012-415f-a04c-f8a52b5f0a4f', lineage).
narrative_ontology:cs_interpretation_layer_present('629869ec-b012-415f-a04c-f8a52b5f0a4f').
narrative_ontology:cs_reading_relation('629869ec-b012-415f-a04c-f8a52b5f0a4f', preparedness_retention__competence_reading, coexists_with).
narrative_ontology:cs_reading_relation('629869ec-b012-415f-a04c-f8a52b5f0a4f', preparedness_retention__hybrid_reading, coexists_with).
narrative_ontology:cs_axiom('629869ec-b012-415f-a04c-f8a52b5f0a4f', foundational, ceremony_does_not_retain_competence).
narrative_ontology:cs_axiom_status(ceremony_does_not_retain_competence, holdable).
narrative_ontology:cs_axiom_grounding('629869ec-b012-415f-a04c-f8a52b5f0a4f', ceremony_does_not_retain_competence, empirically_contingent).
narrative_ontology:cs_axiom('629869ec-b012-415f-a04c-f8a52b5f0a4f', foundational, visible_compliance_displaces_capacity_investment).
narrative_ontology:cs_axiom_status(visible_compliance_displaces_capacity_investment, holdable).
narrative_ontology:cs_axiom_grounding('629869ec-b012-415f-a04c-f8a52b5f0a4f', visible_compliance_displaces_capacity_investment, empirically_contingent).
narrative_ontology:cs_reference_frame('629869ec-b012-415f-a04c-f8a52b5f0a4f', post_watersnood_lived_practice).
narrative_ontology:cs_drift_state('629869ec-b012-415f-a04c-f8a52b5f0a4f', generational_turnover_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('629869ec-b012-415f-a04c-f8a52b5f0a4f', '').
narrative_ontology:cs_kernel_id(preparedness_retention__husk_reading, preparedness_retention).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(preparedness_retention__husk_reading, national_exercise_authorities).
narrative_ontology:constraint_beneficiary(preparedness_retention__husk_reading, safety_region_directors).
narrative_ontology:constraint_beneficiary(preparedness_retention__husk_reading, exercise_industry_contractors).
narrative_ontology:constraint_victim(preparedness_retention__husk_reading, frontline_responders).
narrative_ontology:constraint_victim(preparedness_retention__husk_reading, dike_and_barrage_operators).
narrative_ontology:constraint_victim(preparedness_retention__husk_reading, general_taxpayer_base).
narrative_ontology:constraint_victim(preparedness_retention__husk_reading, future_flood_affected_population).
narrative_ontology:constraint_vindicates(preparedness_retention__husk_reading, visible_compliance_doctrine).
narrative_ontology:constraint_vindicates(preparedness_retention__husk_reading, exercise_frequency_equals_readiness).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets the national exercise framework that tells the twenty-five safety regions, the water boards, and Rijkswaterstaat which flood scenarios to rehearse, how often, and against which scoring criteria. Approves exercise designs, commissions evaluation reports, and reports readiness statistics upward to ministers and parliament. Its annual cycle, staffing, and policy relevance depend on the framework continuing in its current shape; redesigning it would mean conceding that earlier frameworks mis-measured readiness.
narrative_ontology:constraint_stakeholder(preparedness_retention__husk_reading, national_exercise_authorities, agenda_setter,
    institutional, generational, arbitrage, national).

% Runs a regional crisis organization that the law requires to exercise jointly with police, fire, medical, and water services every year. Visible exercise performance drives budget negotiations and standing with constituent municipalities; a director who declared the exercise calendar hollow would forfeit both. Administers the rituals locally while drawing career legitimacy from them, and cannot legally decline or unilaterally reshape them.
narrative_ontology:constraint_stakeholder(preparedness_retention__husk_reading, safety_region_directors, beneficiary,
    institutional, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(preparedness_retention__husk_reading, safety_region_directors, agenda_setter).

% Designs scenario scripts, stages simulated floods, staffs simulation cells, and writes the evaluation scores the framework runs on. Revenue scales with the number, size, and production value of exercises; simpler, harder-to-score training formats would shrink the market. Sells to every region and ministry in the cycle and advises on the criteria it is later scored against.
narrative_ontology:constraint_stakeholder(preparedness_retention__husk_reading, exercise_industry_contractors, beneficiary,
    organized, immediate, arbitrage, national).

% Police officers, firefighters, and municipal crisis-staff members who surrender duty hours to scripted rehearsals. Many describe the large exercises as long waits between staged decision points; the same hours cannot be spent on repetition matched to their units' actual failure modes. Leaving the service is the only way to leave the calendar.
narrative_ontology:constraint_stakeholder(preparedness_retention__husk_reading, frontline_responders, payer,
    organized, biographical, constrained, regional).

% Water board and Rijkswaterstaat technical staff who close barriers, manage storm-surge gates, and patrol dike rings. Real closures are rare, so hands-on proficiency lives in a shrinking veteran cohort; the exercise calendar offers scripted walk-throughs instead of supervised repetition on the machines. Professional standing inside the water-authority world is tied to treating the rehearsal cycle as proof of readiness, which makes open dissent from it a threat to craft identity as much as to career.
narrative_ontology:constraint_stakeholder(preparedness_retention__husk_reading, dike_and_barrage_operators, payer,
    organized, generational, identity_locked, regional).

% Funds the exercise cycle, the simulation centers, and the evaluation bureaucracy through water-board levies and national budgets, with no seat in deciding how rehearsal weight divides between visible demonstration and unscored practice. Sees the exercise coverage in the media and reasonably concludes the system is being maintained.
narrative_ontology:constraint_stakeholder(preparedness_retention__husk_reading, general_taxpayer_base, payer,
    moderate, biographical, mobile, national).

% Residents and businesses behind the dike rings who would absorb the difference between rehearsed procedure and retrievable capacity during a real D5-scale flood. Housing, family, and insured location bind them to the polders. During calm decades they have no way to observe the gap, and each annual display of readiness reassures them against preparing privately.
narrative_ontology:constraint_stakeholder(preparedness_retention__husk_reading, future_flood_affected_population, payer,
    powerless, generational, trapped, national).

% Inspects the safety regions and water authorities on statutory exercise and planning obligations, publishing findings that note uneven realism and uneven participation without ever reaching the conclusion that the cycle itself fails to retain capability. Reports feed ministerial letters and follow-up rounds, not redesign.
narrative_ontology:constraint_stakeholder(preparedness_retention__husk_reading, safety_services_inspectorate, observer,
    institutional, generational, analytical, national).

% Crisis-management scholars and retired senior operators who publish comparisons showing that heavily scripted exercises predict poorly under stress and that tacit skill decays without repetition. Findings circulate in journals and conference panels; they hold no seat on the framework's design committees and their recommendations enter no budget line.
narrative_ontology:constraint_stakeholder(preparedness_retention__husk_reading, exercise_realism_researchers, excluded,
    moderate, biographical, analytical, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(preparedness_retention__husk_reading, exercise_industry_contractors).
narrative_ontology:fixing_cost_class(preparedness_retention__husk_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Synchronizes multi-agency flood-response procedure across the twenty-five safety regions, the water boards, and Rijkswaterstaat: a common scenario vocabulary, scheduled joint rehearsals, statutory compliance records, and maintained inter-agency contact networks.
% TRANSFER_FUNCTION: Moves public funds and responder duty-hours away from unscored practice and equipment time and into scripted demonstration events, simulation contracts, and evaluation reporting; moves assurance — the appearance of retained capacity — from operating agencies to ministries, inspectorates, and the public.
% ABSENT_VOICES: Exercise-realism researchers and retired operators who document the ceremony-to-competence gap stand outside the design committees; their findings enter the literature, not the framework. Residents behind the dike rings appear in scenario scripts as casualties and evacuees, never as principals asking what the rehearsal purchases them.
% DISAPPEARANCE_RATIONALE: If the cycle vanished overnight, the safety regions would lose their statutory compliance vehicle and budget justification, the exercise industry would lose its market, the ministries would lose their assurance instrument, and the inspectorate its reporting object — while, on this reading, little retrievable capacity would be lost, because little is retained by the cycle. The institutional architecture around the rituals would rearrange within budget cycles; the polders would notice no immediate difference until a real event tested the gap.
% FOUNDING_PROBLEM: After the 1953 North Sea flood killed more than 1,800 people in the Netherlands, the state committed to ensuring the next great flood would meet a practiced, coordinated response; drills and inspections were designed to carry response competence across generations who would live entire careers without a real catastrophe.
% FOUNDING_PROBLEM_CORROBORATION: The flood threat is attested from outside the exercise apparatus: KNMI climate scenarios, Delta Programme engineering assessments, and the 1993 and 1995 near-floods together with the 2021 Limburg floods establish that a D5-scale event remains a when-not-if. No serious actor disputes the problem's liveness; the contest is over whether the drill cycle serves it.
narrative_ontology:disappearance_verdict(preparedness_retention__husk_reading, world_rearranges).
narrative_ontology:founding_problem_status(preparedness_retention__husk_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(preparedness_retention__husk_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(preparedness_retention__husk_reading, 'none', 1).
narrative_ontology:epsilon_provenance(preparedness_retention__husk_reading, 0.68, 'stealth/ox-alpha', 'none', direct).

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
 *   Claimed type is snare because, on this reading, the retention story functions as cover: what the cycle reliably produces is compliance artifacts and legitimacy displays, its persistence rides on statutory mandate and funding conditionality rather than participant preference, and substituting unscored practice formats is institutionally punished. Extractiveness is authored at 0.68 because the cycle consumes substantial funds and duty-hours while, by this reading's lights, converting little into retrievable capacity — and it additionally manufactures reassurance that suppresses private preparation. Suppression 0.55 reflects mandate plus career risk rather than force: participation is legally required, budgets hinge on compliance records, and a director or operator who publicly called the calendar hollow would forfeit standing. Theater_ratio 0.78 is this reading's central quantity — the ceremony-to-competence ratio — rising across the interval as exercises grew larger, more scripted, and more scored while the veteran cohort retired. Accessibility_collapse 0.42: alternatives (no-notice exercises, apprenticeship repetitions, specialist retention programs) remain describable and are piloted at the margins, but cannot compete for budget because they generate no scorable artifact. Resistance 0.3 is low for an arrangement this costly because the ultimate bearers are temporally displaced — the population that pays at D5 cannot yet experience the loss, and peacetime payers grumble individually without a coalition; the coalition-power question for the powerless seat therefore resolves negatively until an event materializes the victim class. The three measurement series share one time grid. Suppression_requirement is tracked because enforcement machinery visibly matured over the interval (statutory exercise duties, inspection protocols, accreditation hardening), not merely shifted. An annual rhythm (quiet season, exercise season, evaluation season) is superimposed on the monotonic drift and is not modeled as a cycle because the drift dominates; the rhythm itself functions as intermittent reinforcement — each season's display refreshes the assurance the next season's budget draws on. Coordination type is declared as enforcement_mechanism because the cycle's binding function is statutory-governance synchronization across legally constituted bodies, not information standardization or identity maintenance as such.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently by construction. From the national authority's seat the cycle is a governance instrument it designed and can defend with compliance statistics. From the safety-region director's seat it is simultaneously livelihood and obligation — legitimacy drawn from the very rituals the director must administer and cannot refuse. From the responder's seat it is surrendered hours; from the operator's seat it is a walk-through displacing the supervised machine repetition the craft requires; from the future flood-affected resident's seat it is invisible until it fails. The engine computes these divergent per-seat classifications from the structural data; the authored snare claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations (authorities, directors, contractors) place those seats near the subsidy end; payer declarations (responders, operators, taxpayers, future flood-affected) place them near the target end, amplified by exit atoms — trapped residents and identity-bound operators sit nearest the full-target pole, mobile taxpayers least so. Directors are genuinely dual-positioned: they administer the rituals and draw legitimacy from them while remaining legally unable to reshape or decline them, placing them mid-low rather than at the pure beneficiary pole; the derivation reaches this through the secondary role, so no explicit override was authored — an override keyed to the institutional power atom would have mis-moved the national authority seat as well. The ultimate victim seat is temporally displaced: its directionality is structural (trapped, powerless, total exposure) even though its members do not yet experience themselves as payers, which is precisely why measured resistance stays low.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — carrying response competence across catastrophe-free generations — is live and worsening with sea-level rise, so the live-status x rearranges-verdict pairing raises no automatic zombie flag. The husk reading nonetheless asserts a quieter mandate migration: the arrangement's original mandate (retain competence) has been functionally replaced by a new one (display compliance), with the retention language kept as cover. mandatrophy_resolved is declared true on that basis — the mandate outlived its function even though the problem outlives everything. What prevents mislabeling here is keeping the two questions separate: the problem's liveness does not certify the arrangement's function, and the arrangement's persistence does not certify the problem's service. A welfarist-style objection from the competence_reading would read the same persistence as evidence of function; that dispute belongs to the sibling file, not to this one.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indexicality,
    'This file is one reading (husk_reading) of the preparedness_retention kernel; the competence_reading authors low extraction over the same standing arrangement and the hybrid_reading splits the referent into retained-specialist versus ceremonial-broad components. Which reading''s ceremony-to-competence conversion rate is correct?',
    'Cross-reading comparison after a validated stress test (blinded, no-notice, unscripted evaluation) or a real D5-class event''s after-action forensics; the readings diverge on exactly the quantity such an event measures.',
    'Near-zero conversion confirms this file''s high-extraction profile; substantial conversion supports the competence_reading''s profile; stratified conversion supports the hybrid_reading''s split referent. Classification of the shared referent moves between snare, rope, and tangled_rope accordingly.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_indexicality, conceptual, 'Reading-indexed epsilon over a shared referent: the disagreement between sibling readings is located in the ceremony-to-competence conversion rate.').

omega_variable(
    live_competence_measurement,
    'Does drill participation convert to retrievable stress performance? The regime''s own scoring never measures this — it measures completion, attendance, and evaluator impressions.',
    'No-notice unscripted exercises with blinded external evaluation, or retrospective comparison of exercised versus unexercised unit performance in the 2021 Limburg floods and subsequent events.',
    'Conversion near zero validates the husk reading and the snare claim; substantial conversion would reclassify the cycle toward rope and shift extraction downward.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(live_competence_measurement, empirical, 'Whether the cycle''s central claimed output (retained capacity) is actually produced is unmeasured by the regime''s own instruments.').

omega_variable(
    scale_necessity_ambiguity,
    'Is ceremonial drift intrinsic to any preparedness regime above a certain scale — tacit skill perhaps cannot be retained across thousands of personnel and decades of calm — or is it contingent on the current compliance-scoring design?',
    'Comparative study of preparedness organizations that retain live competence at scale (specialist closure crews, aviation-style recurrent checkrides) versus those that drift; identify which design features track retention.',
    'If intrinsic, remediation targets complementarity (protecting specialist retention pockets, the hybrid_reading''s terrain) rather than reform of the broad cycle; if contingent, the cycle itself is reformable and the snare reading implies a fixable design failure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(scale_necessity_ambiguity, conceptual, 'Whether the husk condition is a scale law or a design artifact.').

omega_variable(
    false_confidence_externality,
    'Does the visible readiness display reduce household and business self-preparedness, making the net social cost of the cycle exceed the direct waste of funds and hours?',
    'Survey series on private flood preparedness correlated with exercise-publicity waves; natural experiments where coverage of a large exercise precedes measurable drops in mitigation uptake.',
    'If confirmed, effective extraction exceeds the direct resource measure and the case for restructuring the display strengthens; if absent, the cycle''s harm is bounded at its direct cost.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(false_confidence_externality, empirical, 'Whether manufactured assurance crowds out private precaution.').

omega_variable(
    suppression_internalization_split,
    'Is the observed reluctance to voice the ceremony critique structural (funding conditionality, statutory exposure, career risk) or internalized (professional identity fused with the exercise calendar, so dissent feels like betrayal of the craft)?',
    'Post-retirement interview series: if veterans voice the critique freely only after exit, the silence was structural; if they defend the calendar after exit, the fusion was internalized.',
    'Internalized suppression persists through formal reform and must be addressed in any redesign; purely structural suppression dissolves when funding conditions change, making reform cheaper than the structural reading implies.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_internalization_split, empirical, 'Structural versus internalized component of the constraint''s hold on its own critics.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(preparedness_retention__husk_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(prep_tr_t0, preparedness_retention__husk_reading, theater_ratio, 0, 0.45).
narrative_ontology:measurement_basis(prep_tr_t0, observed).
narrative_ontology:measurement(prep_tr_t5, preparedness_retention__husk_reading, theater_ratio, 5, 0.5).
narrative_ontology:measurement_basis(prep_tr_t5, observed).
narrative_ontology:measurement(prep_tr_t10, preparedness_retention__husk_reading, theater_ratio, 10, 0.56).
narrative_ontology:measurement_basis(prep_tr_t10, observed).
narrative_ontology:measurement(prep_tr_t15, preparedness_retention__husk_reading, theater_ratio, 15, 0.62).
narrative_ontology:measurement_basis(prep_tr_t15, observed).
narrative_ontology:measurement(prep_tr_t20, preparedness_retention__husk_reading, theater_ratio, 20, 0.68).
narrative_ontology:measurement_basis(prep_tr_t20, observed).
narrative_ontology:measurement(prep_tr_t25, preparedness_retention__husk_reading, theater_ratio, 25, 0.73).
narrative_ontology:measurement_basis(prep_tr_t25, observed).
narrative_ontology:measurement(prep_tr_t30, preparedness_retention__husk_reading, theater_ratio, 30, 0.78).
narrative_ontology:measurement_basis(prep_tr_t30, observed).

% Extraction over time
narrative_ontology:measurement(prep_be_t0, preparedness_retention__husk_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement_basis(prep_be_t0, observed).
narrative_ontology:measurement(prep_be_t5, preparedness_retention__husk_reading, base_extractiveness, 5, 0.52).
narrative_ontology:measurement_basis(prep_be_t5, observed).
narrative_ontology:measurement(prep_be_t10, preparedness_retention__husk_reading, base_extractiveness, 10, 0.55).
narrative_ontology:measurement_basis(prep_be_t10, observed).
narrative_ontology:measurement(prep_be_t15, preparedness_retention__husk_reading, base_extractiveness, 15, 0.59).
narrative_ontology:measurement_basis(prep_be_t15, observed).
narrative_ontology:measurement(prep_be_t20, preparedness_retention__husk_reading, base_extractiveness, 20, 0.63).
narrative_ontology:measurement_basis(prep_be_t20, observed).
narrative_ontology:measurement(prep_be_t25, preparedness_retention__husk_reading, base_extractiveness, 25, 0.66).
narrative_ontology:measurement_basis(prep_be_t25, observed).
narrative_ontology:measurement(prep_be_t30, preparedness_retention__husk_reading, base_extractiveness, 30, 0.68).
narrative_ontology:measurement_basis(prep_be_t30, observed).

% Suppression requirement over time
narrative_ontology:measurement(prep_su_t0, preparedness_retention__husk_reading, suppression_requirement, 0, 0.38).
narrative_ontology:measurement_basis(prep_su_t0, observed).
narrative_ontology:measurement(prep_su_t5, preparedness_retention__husk_reading, suppression_requirement, 5, 0.42).
narrative_ontology:measurement_basis(prep_su_t5, observed).
narrative_ontology:measurement(prep_su_t10, preparedness_retention__husk_reading, suppression_requirement, 10, 0.46).
narrative_ontology:measurement_basis(prep_su_t10, observed).
narrative_ontology:measurement(prep_su_t15, preparedness_retention__husk_reading, suppression_requirement, 15, 0.49).
narrative_ontology:measurement_basis(prep_su_t15, observed).
narrative_ontology:measurement(prep_su_t20, preparedness_retention__husk_reading, suppression_requirement, 20, 0.52).
narrative_ontology:measurement_basis(prep_su_t20, observed).
narrative_ontology:measurement(prep_su_t25, preparedness_retention__husk_reading, suppression_requirement, 25, 0.54).
narrative_ontology:measurement_basis(prep_su_t25, observed).
narrative_ontology:measurement(prep_su_t30, preparedness_retention__husk_reading, suppression_requirement, 30, 0.55).
narrative_ontology:measurement_basis(prep_su_t30, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(preparedness_retention__husk_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(preparedness_retention__husk_reading, preparedness_retention__competence_reading).
narrative_ontology:affects_constraint(preparedness_retention__husk_reading, preparedness_retention__hybrid_reading).

% DUAL FORMULATION NOTE:
% Constraint family: preparedness_retention decomposes into three readings of one kernel — competence_reading (drills preserve capacity; low epsilon), husk_reading (this file; drills are memorial performance; high epsilon), and hybrid_reading (stratified: specialist institutions retain, broad societal memory turns ceremonial). The decomposition follows the epsilon-invariance principle: the colloquial label 'preparedness' covers structurally distinct claims whose epsilon values differ widely — assessing the cycle by compliance output yields negligible extraction, assessing it by retrieved stress capacity yields heavy extraction, and these are different constraints, not one constraint under two observables. Each family member links its siblings here; the upstream competence_reading is the one cited as evidence by the regime's own legitimacy claims, which is why this downstream reading carries the contested, higher-extraction profile.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
