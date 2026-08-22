% ============================================================================
% CONSTRAINT STORY: competence_retention_exercise__catastrophe_as_necessary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_competence_retention_exercise__catastrophe_as_necessary, []).

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
 *   constraint_id: competence_retention_exercise__catastrophe_as_necessary
 *   human_readable: Catastrophe-as-Necessary Doctrine of Competence Retention
 *   domain: organizational/safety-engineering
 *
 * SUMMARY:
 *   High-hazard industries (aviation, nuclear power, surgery, mining,
 *   chemical processing) carry a standing doctrine: only actual catastrophic
 *   events supply the organizational learning and visceral stakes that keep
 *   catastrophe-avoidance competence genuine; simulation is rehearsal, never
 *   the real thing. The doctrine presents itself as a fact about skill
 *   acquisition, but it organizes real flows: it directs training budgets
 *   away from simulation, ranks lived experience over tested procedure,
 *   converts each disaster into careers, mandates, and budget expansions for
 *   investigators and executives, and prices the curriculum in the lives of
 *   frontline operators and exposed publics. This file instantiates ONE
 *   reading of the competence_retention_exercise kernel — the
 *   catastrophe_as_necessary reading — as a clean, epsilon-invariant
 *   constraint; the sibling readings (simulation_as_sufficient,
 *   near_miss_as_bridge) are separate constraints, linked via network edges,
 *   not folded into this one. Epsilon's referent is the standing arrangement
 *   under contest — reliance on real catastrophes as the primary
 *   competence-maintenance mechanism — assessed by this reading's own lights,
 *   which is why the reading's genuine epistemic commitments appear alongside
 *   the costs it acknowledges.
 *
 * KEY AGENTS:
 *   - executive_safety_leadership: agenda-setter and principal collector ([powerful]/[arbitrage]) — allocates training resources and converts events into mandate and budget
 *   - veteran_operators: status beneficiary with mortal exposure ([organized]/[identity_locked]) — authority drawn from lived catastrophe, body stationed in the hazard zone
 *   - accident_investigation_establishment: epistemic beneficiary ([institutional]/[mobile]) — converts events into reports, methods, and careers
 *   - frontline_operators: primary payer ([moderate]/[constrained]) — bears the training burden and the mortality of 'educational' events
 *   - exposed_public: primary payer without exit ([powerless]/[trapped]) — funds the curriculum with lives and holds no seat anywhere in the process
 *   - simulation_advocates: excluded challenger ([moderate]/[mobile]) — evidence pre-sorted into the rehearsal category
 *   - hro_researchers: analytical observer ([analytical]/[analytical]) — produces the comparative record both sides cite
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(competence_retention_exercise__catastrophe_as_necessary, 0.62).
domain_priors:suppression_score(competence_retention_exercise__catastrophe_as_necessary, 0.62).
domain_priors:theater_ratio(competence_retention_exercise__catastrophe_as_necessary, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(competence_retention_exercise__catastrophe_as_necessary, extractiveness, 0.62).
narrative_ontology:constraint_metric(competence_retention_exercise__catastrophe_as_necessary, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(competence_retention_exercise__catastrophe_as_necessary, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(competence_retention_exercise__catastrophe_as_necessary, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(competence_retention_exercise__catastrophe_as_necessary, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(competence_retention_exercise__catastrophe_as_necessary, tangled_rope).
narrative_ontology:human_readable(competence_retention_exercise__catastrophe_as_necessary, "Catastrophe-as-Necessary Doctrine of Competence Retention").
narrative_ontology:topic_domain(competence_retention_exercise__catastrophe_as_necessary, "organizational/safety-engineering").

domain_priors:requires_active_enforcement(competence_retention_exercise__catastrophe_as_necessary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(competence_retention_exercise__catastrophe_as_necessary, '6ec2d2e5-b0b5-46d2-892f-2e20ff54183f').
narrative_ontology:cs_kernel_codification('6ec2d2e5-b0b5-46d2-892f-2e20ff54183f', distributed).
narrative_ontology:cs_authority_grounding('6ec2d2e5-b0b5-46d2-892f-2e20ff54183f', lineage).
narrative_ontology:cs_interpretation_layer_present('6ec2d2e5-b0b5-46d2-892f-2e20ff54183f').
narrative_ontology:cs_reading_relation('6ec2d2e5-b0b5-46d2-892f-2e20ff54183f', competence_retention_exercise__simulation_as_sufficient, forecloses).
narrative_ontology:cs_reading_relation('6ec2d2e5-b0b5-46d2-892f-2e20ff54183f', competence_retention_exercise__near_miss_as_bridge, coexists_with).
narrative_ontology:cs_axiom('6ec2d2e5-b0b5-46d2-892f-2e20ff54183f', foundational, visceral_stakes_constitutive_of_competence).
narrative_ontology:cs_axiom_status(visceral_stakes_constitutive_of_competence, holdable).
narrative_ontology:cs_axiom_grounding('6ec2d2e5-b0b5-46d2-892f-2e20ff54183f', visceral_stakes_constitutive_of_competence, empirically_contingent).
narrative_ontology:cs_axiom('6ec2d2e5-b0b5-46d2-892f-2e20ff54183f', foundational, incident_free_decay_invisibility).
narrative_ontology:cs_axiom_status(incident_free_decay_invisibility, holdable).
narrative_ontology:cs_axiom_grounding('6ec2d2e5-b0b5-46d2-892f-2e20ff54183f', incident_free_decay_invisibility, empirically_contingent).
narrative_ontology:cs_reference_frame('6ec2d2e5-b0b5-46d2-892f-2e20ff54183f', operational_experience_primacy).
narrative_ontology:cs_drift_state('6ec2d2e5-b0b5-46d2-892f-2e20ff54183f', contemporary_simulation_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('6ec2d2e5-b0b5-46d2-892f-2e20ff54183f', '').
narrative_ontology:cs_kernel_id(competence_retention_exercise__catastrophe_as_necessary, competence_retention_exercise).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(competence_retention_exercise__catastrophe_as_necessary, veteran_operators).
narrative_ontology:constraint_beneficiary(competence_retention_exercise__catastrophe_as_necessary, accident_investigation_establishment).
narrative_ontology:constraint_beneficiary(competence_retention_exercise__catastrophe_as_necessary, executive_safety_leadership).
narrative_ontology:constraint_victim(competence_retention_exercise__catastrophe_as_necessary, frontline_operators).
narrative_ontology:constraint_victim(competence_retention_exercise__catastrophe_as_necessary, exposed_public).
narrative_ontology:constraint_vindicates(competence_retention_exercise__catastrophe_as_necessary, tacit_knowledge_irreducibility).
narrative_ontology:constraint_vindicates(competence_retention_exercise__catastrophe_as_necessary, unknown_unknown_epistemic_gap).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets training budgets, staffing rules, and post-event response policy; decides each year how much goes to simulator programs versus operational tempo. After a major event, gains expanded mandate, emergency budget authority, and a ready-made account for prior risk decisions ('operations are the only real teacher'). Rotates across organizations and sectors; departure terms insulate them from the long tail of the events presided over.
narrative_ontology:constraint_stakeholder(competence_retention_exercise__catastrophe_as_necessary, executive_safety_leadership, agenda_setter,
    powerful, biographical, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(competence_retention_exercise__catastrophe_as_necessary, executive_safety_leadership, beneficiary).

% Senior operators whose authority rests on having lived through real emergencies; their testimony anchors inquiries, their anecdotes anchor training, and their sign-off carries weight no classroom credential matches. They also stand inside the hazard zone when the next event arrives, so the arrangement that elevates them is the same one that exposes them. Leaving the craft would forfeit the standing their history purchased.
narrative_ontology:constraint_stakeholder(competence_retention_exercise__catastrophe_as_necessary, veteran_operators, beneficiary,
    organized, biographical, identity_locked, global).

% Boards, agencies, academic centers, and consultancies that turn each catastrophe into reports, methods, journal issues, and conference cycles. Every new event renews demand for their services and confirms the centrality of their archives; long incident-free stretches threaten relevance and funding. They move readily between sectors, jurisdictions, and academia.
narrative_ontology:constraint_stakeholder(competence_retention_exercise__catastrophe_as_necessary, accident_investigation_establishment, beneficiary,
    institutional, generational, mobile, global).

% Pilots, control-room crews, surgeons, drivers, and shift workers who operate the hazardous system daily. When operational experience is ranked above all other instruction, they absorb both the training burden and the mortality statistics of the events counted as education. Union protections and sector-specific skills make leaving costly; changing employers rarely means leaving the hazard.
narrative_ontology:constraint_stakeholder(competence_retention_exercise__catastrophe_as_necessary, frontline_operators, payer,
    moderate, biographical, constrained, global).

% Passengers, patients, neighbors, and downstream users who never agreed to fund anyone's education with their lives. They hold no seat in training-budget debates, no role on inquiry panels, and no practical way to opt out of systemic risk short of abstaining from air travel, medical care, or the products of heavy industry.
narrative_ontology:constraint_stakeholder(competence_retention_exercise__catastrophe_as_necessary, exposed_public, payer,
    powerless, immediate, trapped, global).

% Training scientists, simulator engineers, and transfer-of-training researchers who argue fidelity has crossed thresholds the doctrine refuses to acknowledge. Their proposals compete for the same budgets; their evidence is pre-sorted into the 'rehearsal' category before evaluation. They can publish against the doctrine and move between industries, but hold no vote in the forums where the doctrine is restated.
narrative_ontology:constraint_stakeholder(competence_retention_exercise__catastrophe_as_necessary, simulation_advocates, excluded,
    moderate, biographical, mobile, national).

% Scholars of high-reliability organizing who study how the doctrine shapes learning across sectors. They run the comparative studies both sides cite, take no share of training budgets or inquiry contracts, and can observe the whole structure from outside any single organization.
narrative_ontology:constraint_stakeholder(competence_retention_exercise__catastrophe_as_necessary, hro_researchers, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(competence_retention_exercise__catastrophe_as_necessary, executive_safety_leadership).
narrative_ontology:fixing_cost_class(competence_retention_exercise__catastrophe_as_necessary, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Keeps an open feedback channel between design assumptions and operational reality: real failures surface emergent interactions, production-pressure dynamics, and unknown-unknowns that models and simulators omit, and the doctrine ensures the organization treats operational events as its primary source of failure knowledge.
% TRANSFER_FUNCTION: Moves the mortal, health, and financial cost of rare catastrophic failures onto frontline operators and exposed publics, and converts those events into epistemic capital (archives, investigations, case law) and political capital (mandate and budget growth, reorganization authority) collected by investigators, regulators, and executives.
% ABSENT_VOICES: Simulation scientists and training-effectiveness researchers are excluded from the doctrine's framing — their results are classified in advance as rehearsal data. Survivors and bereaved families of the events counted as tuition almost never sit on the inquiries that convert their losses into doctrine. Regulators who might mandate simulation fidelity standards typically arrive only after a catastrophe.
% DISAPPEARANCE_RATIONALE: Training budgets would migrate toward simulation and near-miss programs within a planning cycle; veterans' experiential authority would renegotiate rather than vanish; the investigation industry would contract to genuine anomaly analysis; and risk-acceptance justifications currently worded as 'we will learn from operations' would lose their accepted form, forcing explicit risk decisions into the open.
% FOUNDING_PROBLEM: In the founding era (early aviation, early nuclear operations, pre-modern medicine) there was no non-catastrophic feedback channel: simulators did not exist or were too crude to carry training load, so genuine competence could only be built through operational experience, and the doctrine gave organizations a workable account of how learning proceeds.
% FOUNDING_PROBLEM_CORROBORATION: Corroboration for the founding era comes from outside the benefiting parties: contemporaneous engineering and flight-training records attest that no adequate substitute for operational experience existed. For the present status, the corroborating sources are adverse to the doctrine: transfer-of-training meta-analyses, high-reliability organization case studies, and civilian near-miss registries such as the Aviation Safety Reporting System — all outside the beneficiary set — attest the founding problem has narrowed sharply; no disinterested source attests it remains fully live, and the doctrine's own holders cite only residual unknown-unknowns.
narrative_ontology:disappearance_verdict(competence_retention_exercise__catastrophe_as_necessary, world_rearranges).
narrative_ontology:founding_problem_status(competence_retention_exercise__catastrophe_as_necessary, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(competence_retention_exercise__catastrophe_as_necessary, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(competence_retention_exercise__catastrophe_as_necessary, 'none', 1).
narrative_ontology:epsilon_provenance(competence_retention_exercise__catastrophe_as_necessary, 0.62, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(competence_retention_exercise__catastrophe_as_necessary_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(competence_retention_exercise__catastrophe_as_necessary, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(competence_retention_exercise__catastrophe_as_necessary_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is 0.62: real events do yield knowledge no simulator currently reproduces, but the mortal cost of that tuition concentrates on operators and publics who collect none of the resulting capital, and the doctrine's opportunity cost has grown every decade simulation fidelity improved. Suppression is 0.62 and structural rather than physical: budget formulas privilege operational headcount over simulator hours, inquiry panels are staffed from the establishment, hiring criteria weight 'time in the seat,' and dissenting evidence is reclassified as rehearsal before evaluation — with an internalized layer as novices absorb the doctrine during professional socialization. Theater is 0.40: lessons-learned rituals, anniversary reviews, and paper reforms perform learning while substantive change waits for the next event. Accessibility collapse is low (0.35) because the alternatives — simulation programs, near-miss registries — demonstrably function and persist; the doctrine raises their cost, it does not erase them. Resistance is 0.60: the HRO movement, resilience engineering, and the patient-safety movement constitute sustained organized pushback. The measurement series run on one shared time grid (every tracked metric authored at every point 0–60) so no metric's end-state is silently substituted into earlier rows. Extractiveness and suppression rise together because the doctrine's enforcement burden grew as its epistemic monopoly eroded; theater rises as post-event ritual thickened. Claim and metrics are authored independently: the tangled_rope claim states what I believe is structurally true; the metrics state what I believe is descriptively true.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats and the collector seats compute opposite experiences from identical facts. From executive_safety_leadership and accident_investigation_establishment, the arrangement is a functioning knowledge system they staff and extend; from frontline_operators and exposed_public, the same arrangement is a price exacted in bodies for knowledge collected elsewhere. The veteran seat straddles the divide: it collects status and authority from the doctrine while standing in the blast radius of the next lesson, which is why its computed position should differ from both pure collectors and pure payers. An inquiry report reads as institutional maturation from the establishment seat and as an autopsy that confirms a preventable death from the family seat — a seat that exists nowhere in the formal process, as recorded under absent_voices.
 *
 * DIRECTIONALITY LOGIC:
 *   Executive_safety_leadership (agenda_setter + beneficiary, arbitrage exit) derives near the full-beneficiary pole: it writes the rules and exits before costs mature. Accident_investigation_establishment (beneficiary, mobile) derives low d: it collects epistemic capital and can relocate if a sector sours. Frontline_operators (victim, constrained) and exposed_public (victim, trapped) derive near the full-target pole; the trapped public sits at the extreme because no arbitrage exists at any price. Veteran_operators require an override: the beneficiary declaration plus organized power would derive a strongly subsidized d, but veterans stand inside the hazard during the events the doctrine counts as pedagogy — their net position is beneficiary-leaning yet materially exposed, so the organized atom is overridden to d=0.30 (only veteran_operators occupies that atom in this story). Exposed_public's trapped exit amplifies its effective extraction; scope is global for most seats, modestly amplifying verification-resistant extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope claim guards against two symmetric mislabels. Reading the doctrine as pure extraction ignores its real coordination service: it keeps unknown-unknown channels open and guards against simulator-induced complacency, a failure mode documented in automation-complacency research. Reading it as pure coordination launders a mortal transfer onto parties who collect nothing, using an epistemic claim as cover. The founding problem is contested rather than dead — simulation fidelity has narrowed the original gap without closing the tail-risk question — so the arrangement is not yet a piton performing a vanished function, and the rising (not flat) suppression series shows active defense rather than inertial drift. No sunset clause exists anywhere in the doctrine's self-description; it claims permanence, which is itself diagnostic of a coordination structure that has stopped advertising its own transitivity.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'This constraint is one reading (catastrophe_as_necessary) of the competence_retention_exercise kernel; which structural features of the arrangement change under the sibling readings simulation_as_sufficient and near_miss_as_bridge?',
    'Instantiate the sibling stories and compare beneficiary sets, victim sets, and epsilon over the same referent; the disagreement is located in the sufficiency premise — whether non-catastrophic exercise (synthetic simulation or near-miss feedback) can carry the competence-maintenance load.',
    'Under simulation_as_sufficient, the doctrine''s coordination function migrates to simulation infrastructure and the casualty-tuition justification collapses entirely; under near_miss_as_bridge, the victim set narrows to tail-risk exposures only and the investigation establishment loses its monopoly on converting events into knowledge.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Committer structure: one reading of a contested kernel; sibling readings redraw the beneficiary/victim lines.').

omega_variable(
    simulation_tail_validity,
    'Can high-fidelity simulation ever reproduce the tail-risk conditions — production pressure, fear, emergent component interactions — that this reading holds make real catastrophes pedagogically irreplaceable?',
    'Transfer-of-training studies comparing incident rates of simulator-heavy versus operations-heavy organizations at matched baseline risk, plus stress-inoculation fidelity research.',
    'If simulation closes the tail-validity gap, the doctrine''s remaining coordination function collapses and its effective extraction approaches pure rent; if the gap is real, part of the measured cost is genuine epistemic tuition rather than extraction, and the tangled_rope reading strengthens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(simulation_tail_validity, empirical, 'Whether the doctrine''s epistemic core survives continuing simulation-fidelity progress.').

omega_variable(
    doctrine_naturalness_ambiguity,
    'Is competence-decay-absent-real-events a structural feature of skill acquisition (natural-law-like) or a constructed doctrine whose persistence serves identifiable professional interests?',
    'Skill-decay literature under matched simulation conditions; historical comparison of professions that shifted to simulation-centric training without measurable competence loss.',
    'If natural, the arrangement resembles an irreducible limit and the casualty toll is tuition; if constructed, the arrangement is a defended interest structure and false-summit-style reclassification pressure strengthens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(doctrine_naturalness_ambiguity, conceptual, 'Natural-law versus constructed-doctrine ambiguity in the doctrine''s self-presentation.').

omega_variable(
    casualty_counterfactual_status,
    'Are the casualties of events the doctrine counts as necessary tuition genuinely unavoidable, or preventable harms relabeled as necessary after the fact?',
    'Counterfactual audit of major accidents: did simulation results or near-miss data available beforehand contain the causal information the post-event investigation later extracted?',
    'A high preventable share converts the victim class from tuition-payers into extraction targets and pushes the arrangement toward pure extraction; a low share supports the coordination reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(casualty_counterfactual_status, empirical, 'Whether ''necessary'' catastrophes were preventable with existing non-catastrophic knowledge.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(competence_retention_exercise__catastrophe_as_necessary, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comp_tr_t0, competence_retention_exercise__catastrophe_as_necessary, theater_ratio, 0, 0.15).
narrative_ontology:measurement(comp_tr_t10, competence_retention_exercise__catastrophe_as_necessary, theater_ratio, 10, 0.18).
narrative_ontology:measurement(comp_tr_t20, competence_retention_exercise__catastrophe_as_necessary, theater_ratio, 20, 0.22).
narrative_ontology:measurement(comp_tr_t30, competence_retention_exercise__catastrophe_as_necessary, theater_ratio, 30, 0.27).
narrative_ontology:measurement(comp_tr_t40, competence_retention_exercise__catastrophe_as_necessary, theater_ratio, 40, 0.32).
narrative_ontology:measurement(comp_tr_t50, competence_retention_exercise__catastrophe_as_necessary, theater_ratio, 50, 0.36).
narrative_ontology:measurement(comp_tr_t60, competence_retention_exercise__catastrophe_as_necessary, theater_ratio, 60, 0.4).

% Extraction over time
narrative_ontology:measurement(comp_be_t0, competence_retention_exercise__catastrophe_as_necessary, base_extractiveness, 0, 0.44).
narrative_ontology:measurement(comp_be_t10, competence_retention_exercise__catastrophe_as_necessary, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(comp_be_t20, competence_retention_exercise__catastrophe_as_necessary, base_extractiveness, 20, 0.52).
narrative_ontology:measurement(comp_be_t30, competence_retention_exercise__catastrophe_as_necessary, base_extractiveness, 30, 0.55).
narrative_ontology:measurement(comp_be_t40, competence_retention_exercise__catastrophe_as_necessary, base_extractiveness, 40, 0.58).
narrative_ontology:measurement(comp_be_t50, competence_retention_exercise__catastrophe_as_necessary, base_extractiveness, 50, 0.6).
narrative_ontology:measurement(comp_be_t60, competence_retention_exercise__catastrophe_as_necessary, base_extractiveness, 60, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(comp_su_t0, competence_retention_exercise__catastrophe_as_necessary, suppression_requirement, 0, 0.28).
narrative_ontology:measurement(comp_su_t10, competence_retention_exercise__catastrophe_as_necessary, suppression_requirement, 10, 0.33).
narrative_ontology:measurement(comp_su_t20, competence_retention_exercise__catastrophe_as_necessary, suppression_requirement, 20, 0.39).
narrative_ontology:measurement(comp_su_t30, competence_retention_exercise__catastrophe_as_necessary, suppression_requirement, 30, 0.46).
narrative_ontology:measurement(comp_su_t40, competence_retention_exercise__catastrophe_as_necessary, suppression_requirement, 40, 0.53).
narrative_ontology:measurement(comp_su_t50, competence_retention_exercise__catastrophe_as_necessary, suppression_requirement, 50, 0.58).
narrative_ontology:measurement(comp_su_t60, competence_retention_exercise__catastrophe_as_necessary, suppression_requirement, 60, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(competence_retention_exercise__catastrophe_as_necessary, identity_coordination).
narrative_ontology:affects_constraint(competence_retention_exercise__catastrophe_as_necessary, competence_retention_exercise__simulation_as_sufficient).
narrative_ontology:affects_constraint(competence_retention_exercise__catastrophe_as_necessary, competence_retention_exercise__near_miss_as_bridge).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'how organizations keep catastrophe-avoidance competence' decomposes into three structurally distinct readings of the competence_retention_exercise kernel. This story (catastrophe_as_necessary) is the tradition-grounded reading with the widest victim set; simulation_as_sufficient is the modern challenger claiming structural equivalence of high-fidelity simulation; near_miss_as_bridge mediates, claiming real-but-small events suffice. The readings differ in epsilon because they differ in victim sets and in what counts as genuine exercise — they are separate constraints, not one constraint under different observables. Upstream/downstream: the catastrophe reading is upstream in institutional authority (it controls the forums where the others are evaluated) while being downstream in evidential support (its foundational premise absorbs the most drift pressure from the contemporary simulation era).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(competence_retention_exercise__catastrophe_as_necessary, organized, 0.3).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
