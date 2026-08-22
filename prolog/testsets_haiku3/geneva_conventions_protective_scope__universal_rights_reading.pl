% ============================================================================
% CONSTRAINT STORY: geneva_conventions_protective_scope__universal_rights_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_geneva_conventions_protective_scope__universal_rights_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: geneva_conventions_protective_scope__universal_rights_reading
 *   human_readable: Geneva Protections Universal Scope (Human Rights Integration Reading)
 *   domain: international/legal/humanitarian
 *
 * SUMMARY:
 *   The Geneva Conventions and their protocols establish humanitarian
 *   protections for persons affected by armed conflict. The
 *   universal-rights-reading interpretation integrates Common Article 3 (the
 *   minimum floor applicable to all conflicts) with human rights law to argue
 *   that ALL persons in armed conflict — regardless of combatant status,
 *   conflict classification, or state affiliation — hold enforceable
 *   protections against torture, arbitrary detention, forced displacement,
 *   and targeting. This reading contests two sibling readings: the
 *   state-centric reading (which ties protections to combatant privilege
 *   under Article 4 criteria and limits some protections to international
 *   conflicts) and the hybrid-proportionality reading (which scales
 *   protections by conflict type via Additional Protocols). The universal
 *   reading expands the victim set (all state military operational
 *   flexibility is constrained) and the beneficiary set (non-state combatants
 *   and civilians in non-international conflicts gain explicit standing). The
 *   constraint is CLAIMED as tangled_rope because it coordinates humanitarian
 *   obligation-bearing across all armed actors while extracting operational
 *   latitude from state military institutions.
 *
 * KEY AGENTS:
 *   - Civilian populations in all armed conflict zones: receive universal protection status regardless of conflict classification
 *   - Non-state armed actors: acquire dual role as beneficiaries (protected) and obligation-bearers (required to respect Common Article 3)
 *   - State military institutions: primary target — lose operational discretion in detention, interrogation, targeting, and occupation governance
 *   - ICRC and humanitarian NGOs: agenda-setters and beneficiaries — gain monitoring and advocacy mandate under the universal floor
 *   - International courts (ICC, ICJ, human rights courts): agenda-setters — acquire interpretive authority to enforce the universal scope
 *   - State sovereignty doctrines: excluded parties — the universal scope denies the claim that non-international conflicts are purely domestic affairs
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(geneva_conventions_protective_scope__universal_rights_reading, 0.72).
domain_priors:suppression_score(geneva_conventions_protective_scope__universal_rights_reading, 0.58).
domain_priors:theater_ratio(geneva_conventions_protective_scope__universal_rights_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(geneva_conventions_protective_scope__universal_rights_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(geneva_conventions_protective_scope__universal_rights_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(geneva_conventions_protective_scope__universal_rights_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(geneva_conventions_protective_scope__universal_rights_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(geneva_conventions_protective_scope__universal_rights_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(geneva_conventions_protective_scope__universal_rights_reading, tangled_rope).
narrative_ontology:human_readable(geneva_conventions_protective_scope__universal_rights_reading, "Geneva Protections Universal Scope (Human Rights Integration Reading)").
narrative_ontology:topic_domain(geneva_conventions_protective_scope__universal_rights_reading, "international/legal/humanitarian").

domain_priors:requires_active_enforcement(geneva_conventions_protective_scope__universal_rights_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(geneva_conventions_protective_scope__universal_rights_reading, '9a15f662-fa9b-422d-9717-ed5b4422534f').
narrative_ontology:cs_kernel_codification('9a15f662-fa9b-422d-9717-ed5b4422534f', fixed_text).
narrative_ontology:cs_authority_grounding('9a15f662-fa9b-422d-9717-ed5b4422534f', lineage).
narrative_ontology:cs_interpretation_layer_present('9a15f662-fa9b-422d-9717-ed5b4422534f').
narrative_ontology:cs_reading_relation('9a15f662-fa9b-422d-9717-ed5b4422534f', geneva_conventions_protective_scope__state_centric_reading, forecloses).
narrative_ontology:cs_reading_relation('9a15f662-fa9b-422d-9717-ed5b4422534f', geneva_conventions_protective_scope__hybrid_proportionality_reading, influences).
narrative_ontology:cs_axiom('9a15f662-fa9b-422d-9717-ed5b4422534f', foundational, universal_protection_status_non_contingent).
narrative_ontology:cs_axiom_status(universal_protection_status_non_contingent, holdable).
narrative_ontology:cs_axiom_grounding('9a15f662-fa9b-422d-9717-ed5b4422534f', universal_protection_status_non_contingent, deontological).
narrative_ontology:cs_axiom('9a15f662-fa9b-422d-9717-ed5b4422534f', foundational, human_rights_law_non_derogable_integration).
narrative_ontology:cs_axiom_status(human_rights_law_non_derogable_integration, holdable).
narrative_ontology:cs_axiom_grounding('9a15f662-fa9b-422d-9717-ed5b4422534f', human_rights_law_non_derogable_integration, deontological).
narrative_ontology:cs_reference_frame('9a15f662-fa9b-422d-9717-ed5b4422534f', universal_humanitarian_protection_floor).
narrative_ontology:cs_drift_state('9a15f662-fa9b-422d-9717-ed5b4422534f', contemporary_enforcement_practice, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('9a15f662-fa9b-422d-9717-ed5b4422534f', '2026-06-12T14:32:00Z').
narrative_ontology:cs_kernel_id(geneva_conventions_protective_scope__universal_rights_reading, geneva_conventions_protective_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(geneva_conventions_protective_scope__universal_rights_reading, civilian_populations).
narrative_ontology:constraint_beneficiary(geneva_conventions_protective_scope__universal_rights_reading, non_state_armed_actors).
narrative_ontology:constraint_beneficiary(geneva_conventions_protective_scope__universal_rights_reading, detained_combatants).
narrative_ontology:constraint_beneficiary(geneva_conventions_protective_scope__universal_rights_reading, humanitarian_ngos).
narrative_ontology:constraint_victim(geneva_conventions_protective_scope__universal_rights_reading, state_military_operational_flexibility).
narrative_ontology:constraint_victim(geneva_conventions_protective_scope__universal_rights_reading, occupying_state_security_doctrine).
narrative_ontology:constraint_victim(geneva_conventions_protective_scope__universal_rights_reading, armed_forces_targeting_latitude).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(geneva_conventions_protective_scope__universal_rights_reading, civilian_populations_affected_zones).
narrative_ontology:constraint_beneficiary(geneva_conventions_protective_scope__universal_rights_reading, humanitarian_ngos_icrc).
narrative_ontology:constraint_victim(geneva_conventions_protective_scope__universal_rights_reading, state_military_institutions).
narrative_ontology:constraint_victim(geneva_conventions_protective_scope__universal_rights_reading, occupying_state_administrations).
narrative_ontology:constraint_victim(geneva_conventions_protective_scope__universal_rights_reading, state_armed_forces_intelligence).
narrative_ontology:constraint_vindicates(geneva_conventions_protective_scope__universal_rights_reading, universal_human_dignity_principle).
narrative_ontology:constraint_vindicates(geneva_conventions_protective_scope__universal_rights_reading, non_derogability_core_rights).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Receive protection from targeting, indiscriminate attack, and forced displacement under this reading. The universal scope expands their protection to all conflict contexts, including non-international armed conflicts and occupation zones where they were previously classified outside the treaty's primary beneficiary class. They cannot exit the conflict zone and cannot negotiate their status.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__universal_rights_reading, civilian_populations_affected_zones, beneficiary,
    powerless, immediate, trapped, regional).

% All detained combatants — lawful combatants, unprivileged belligerents, and non-state armed group members — receive Common Article 3 protections (humane treatment, no torture, medical care) plus human rights floor under this reading. The universal scope eliminates the distinction between 'privileged' and 'unprivileged' combatant status as a gateway to rights denial.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__universal_rights_reading, detained_combatants, beneficiary,
    powerless, immediate, trapped, regional).

% Are recognized as bearer-entities of obligations and beneficiaries of protections under this reading. They acquire duties to respect humanitarian law (including Common Article 3, Protocol II) in parallel with state armed forces. They benefit from the universal floor because it binds their own conduct but also constrains state operations against them and their detained combatants.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__universal_rights_reading, non_state_armed_actors, beneficiary,
    moderate, biographical, constrained, regional).

% Bear the operational constraints of the universal protective scope: restrictions on detention, interrogation, targeting, and treatment of all combatants regardless of status; obligations to provide medical care and humane conditions to non-combatants; prohibition on forced displacement and collective punishment. The reading closes exit routes (declarations of 'unprivileged belligerent' status) that the state-centric reading would permit.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__universal_rights_reading, state_military_institutions, payer,
    institutional, generational, constrained, national).

% Must apply Fourth Geneva Convention protections and human rights floor to occupied populations; cannot suspend core rights or differential treatment based on collaboration; must provide security, employment, medical care, and protection from reprisal. The universal scope extends obligations to occupation contexts that the state-centric reading might treat as domestic security matters.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__universal_rights_reading, occupying_state_administrations, payer,
    institutional, generational, constrained, regional).

% Gain access authority under the universal reading: ICRC and humanitarian NGOs acquire mandate to monitor and validate compliance with the universal floor across ALL armed conflicts and detention sites, including non-international conflicts and non-state combatant detention. They can advocate boundary-crossing interventions (e.g., in state interrogation centers) grounded in Common Article 3, not merely in specific state treaty ratification.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__universal_rights_reading, humanitarian_ngos_icrc, beneficiary,
    organized, generational, mobile, global).
narrative_ontology:stakeholder_secondary_role(geneva_conventions_protective_scope__universal_rights_reading, humanitarian_ngos_icrc, agenda_setter).

% Face operational constraints on interrogation, detention classification, and targeting procedures under the universal floor. The integration of human rights law means their interrogation practices, detention duration, legal status determinations, and use-of-force protocols are bound by non-derogable core rights (prohibition on torture, cruel treatment, arbitrary detention, disappearance) regardless of conflict classification.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__universal_rights_reading, state_armed_forces_intelligence, payer,
    powerful, biographical, constrained, national).

% ICC, ICJ, human rights courts, and ad-hoc tribunals gain interpretive authority to adjudicate the universal protective scope. They apply the reading's framework (Common Article 3 plus human rights floor) as the operative standard for violations, holding all parties to the same humanitarian baseline regardless of conflict classification or combatant status.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__universal_rights_reading, international_court_institutions, agenda_setter,
    institutional, generational, arbitrage, global).

% Military doctrines premised on legal distinctions (privileged vs. unprivileged combatancy, international vs. non-international conflict) lose jurisdictional cover under this reading. Counter-insurgency doctrine relying on detention and interrogation of 'unprivileged' combatants without humanitarian protections is directly constrained. These doctrines cannot exit the constraint framework but are forced to reformulate their operational categories.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__universal_rights_reading, rival_state_military_doctrines, excluded,
    institutional, generational, trapped, national).

% States asserting absolute sovereignty over internal security matters are structurally excluded from the conversation by the universal scope reading. The reading denies the premise that non-international conflicts are purely domestic affairs — it brings all armed conflict within the humanitarian law framework, making internal security operations subject to external humanitarian monitoring and accountability.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__universal_rights_reading, state_sovereignty_claim_holders, excluded,
    institutional, generational, constrained, national).

% The traditional doctrine that states may reserve out of treaty obligations is constrained by this reading's claim that the human rights floor is non-derogable and universal. States cannot validly reserve out of Common Article 3 or the core human rights standards; the universal scope forecloses that exit route.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__universal_rights_reading, treaty_reservations_doctrine, excluded,
    powerful, generational, analytical, global).
narrative_ontology:stakeholder_non_agent(geneva_conventions_protective_scope__universal_rights_reading, treaty_reservations_doctrine).

% Humanitarian law scholars, human rights organizations, and international legal analysis institutions track the reading's operation and the alignment or tension between Geneva protections and human rights law. They provide the external corroboration and counter-argument to state claims about protective scope.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__universal_rights_reading, analytical_observer_humanitarian_law, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(geneva_conventions_protective_scope__universal_rights_reading, humanitarian_ngos_icrc).
narrative_ontology:fixing_cost_class(geneva_conventions_protective_scope__universal_rights_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a universal minimum floor of human protections (Common Article 3 plus integrated human rights standards) applicable to all persons affected by armed conflict, regardless of conflict classification, combatant status, or state affiliation. Coordinates the obligation-bearing structures of states, non-state armed actors, humanitarian actors, and judicial monitors around a single humanitarian baseline.
% TRANSFER_FUNCTION: Transfers operational latitude from state military institutions and occupying administrations to civilian populations, detained combatants, and humanitarian monitors. What moves is the authority to classify combatants into legal categories that trigger differential treatment; the universal reading eliminates that classification as a valid gate to rights denial and moves authority over interrogation, detention, and targeting to humanitarian and judicial oversight structures.
% ABSENT_VOICES: Military doctrine specialists, national security apparatuses that benefit from classification discretion, and state actors whose sovereignty claims rely on domestic-security exceptionalism are structurally excluded from the writing of the universal scope. They would argue for preserving distinction-based protections tied to state interest; their absence from the foundational reading makes that argument harder to surface in real-time application.
% DISAPPEARANCE_RATIONALE: If the universal protective scope and human rights integration disappeared, armed conflict governance would immediately revert to state-centric legal categories: privileged vs. unprivileged combatants, international vs. non-international conflict, combatant vs. civilian. States would regain operational latitude in interrogation, detention classification, targeting, and occupation. Humanitarian agencies would lose mandate authority in non-international conflicts. Accountability structures (ICC, human rights courts) would lose the unified humanitarian standard. The world rearranges because the constraint is the explicit rule-change that produces this allocation.
% FOUNDING_PROBLEM: Atrocities in internal armed conflicts, occupation zones, and non-international conflicts revealed that state-centric Geneva protections (keyed to privileged combatant status and international conflict classification) left vast populations — civilians in non-international conflicts, detained non-state combatants, occupied populations — without enforceable protections. Human rights law separately guaranteed certain rights but was fragmented by derogation clauses, state reservations, and weak enforcement. The founding problem was protection gap: humanitarian protections were disconnected, fragmentary, and state-discipline-dependent.
% FOUNDING_PROBLEM_CORROBORATION: ICRC, Human Rights Watch, Amnesty International, and academic humanitarian law scholarship from outside the state-benefiting institutions attest the founding problem remains acute: contemporary armed conflicts generate documented protection gaps (Yemen, Syria, Ukraine detention practices, Myanmar non-international conflict) where state operational discretion still outpaces humanitarian enforcement. State military doctrine attestation is not independent corroboration; independent scholarship from humanitarian and human rights institutions confirms the gap persists.
narrative_ontology:disappearance_verdict(geneva_conventions_protective_scope__universal_rights_reading, world_rearranges).
narrative_ontology:founding_problem_status(geneva_conventions_protective_scope__universal_rights_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(geneva_conventions_protective_scope__universal_rights_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(geneva_conventions_protective_scope__universal_rights_reading, 'none', 1).
narrative_ontology:epsilon_provenance(geneva_conventions_protective_scope__universal_rights_reading, 0.72, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(geneva_conventions_protective_scope__universal_rights_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(geneva_conventions_protective_scope__universal_rights_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(geneva_conventions_protective_scope__universal_rights_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extraction score (0.72 at interval end) reflects the constraint's asymmetric impact: it concentrates operational restrictions on state military institutions while diffusing benefits across humanitarian actors and protected populations. Extraction rises over the interval (0.58→0.72) as jurisprudence (ICC, human rights courts) and humanitarian practice consolidate the reading, making state exit attempts (reservations, distinction-based reclassification) less viable — the constraint hardens. The suppression score (0.58) is moderate because the constraint does not physically prevent state operations; instead it raises the accountability cost of violating the universal floor (legal liability, sanctions, reputational damage). Theater ratio (0.42) reflects mixed dynamics: the constraint's humanitarian purpose is genuine, but state compliance often operates theatrically — states stage humanitarian postures while maintaining covert interrogation and detention practices that violate the universal floor. The temporal plateau (extraction and theater ratio stabilize t=32–40) suggests the constraint has reached a new equilibrium: humanitarian accountability structures are institutionalized (ICC, HRC mechanisms) but state resistance to the universal scope remains organized and visible (military doctrine maintains distinction-based frameworks, states contest ICC jurisdiction).
 *
 * PERSPECTIVAL GAP:
 *   The payer seat (state military) and beneficiary seats (humanitarian, protected populations) experience fundamentally different constraint types from the same rule structure. State actors compute the constraint as extractive: operational restrictions with accountability teeth. Humanitarian actors compute it as coordination: unified baseline protecting all conflict-affected persons. This divergence emerges from their structural positions (power, exit options, benefit/cost flow) and is COMPUTED by the engine from the authored directionality data — the constraint's claim (tangled_rope) reflects the state-seat experience, while the metrics (high extraction, moderate suppression, rising theater ratio) capture the enforcement asymmetry that makes the coordination story incomplete.
 *
 * DIRECTIONALITY LOGIC:
 *   State military institutions are the primary target: they are powerful institutional actors with constrained exit (cannot simply withdraw from Geneva obligations or redefine combatant status without triggering accountability), bearing substantial operational costs (restricted interrogation, detention, targeting protocols). Their directionality (d) sits near 1.0 (full target) because the universal scope directly restricts their latitude without offering compensating benefit. Civilian populations and detained combatants are beneficiaries despite powerlessness because the constraint expands their legal standing and enforceability; their d sits near 0.0 (beneficiaries) because the constraint subsidizes their protection without cost to them. Humanitarian NGOs and international courts occupy a secondary beneficiary role: they gain monitoring authority and interpretive power; their d is low but above zero because they bear small costs (operational complexity of enforcement). Non-state armed actors are dual-positioned: they benefit from the universal floor (protected combatants) while bearing costs (required to respect Common Article 3). Their d is moderate (0.4–0.5) reflecting this symmetry. Occupying state administrations are targets (comprehensive occupation duties, constrained exit); their d is high (0.7+).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (protection gap in non-international conflicts) remains live but the constraint has begun to operate theatrically. States maintain formal compliance (they accept that Common Article 3 applies) while organizing practical resistance (they reclassify detention sites as security facilities outside humanitarian access, they contest ICC jurisdiction, they maintain military doctrine frameworks keyed to privileged combatancy). The constraint avoids mandatrophy (dead founding problem) because humanitarian crises in Yemen, Syria, Myanmar, and Ukraine continue to generate protection gaps where the universal floor is violated. However, the theater ratio rise (0.25→0.42) signals that the constraint is increasingly maintained performatively: states stage humanitarian compliance for international audiences while preserving operational discretion in practice. The mandatrophy vector is not yet critical, but the trajectory is toward constraint degradation if the accountability mechanisms (ICC, human rights courts) lose political support or enforcement capacity.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    universal_floor_enforcement_depth,
    'Does the actual practice of humanitarian monitoring and judicial enforcement match the universal floor''s normative scope, or do state actors systematically evade accountability through reclassification, jurisdiction challenges, and covert practice?',
    'Empirical audit of detention sites, interrogation practices, and accountability outcomes across armed conflicts over a 10-year window. Comparison of cases prosecuted (ICC, national courts) against documented violations. Evidence of state evasion tactics (reclassification as security facilities, denial of humanitarian access, contested jurisdiction).',
    'If enforcement depth is high, the constraint operates as genuine tangled_rope (coordination + asymmetric extraction are both real). If enforcement is systematically evaded, the constraint degrades toward piton (theater ratio rises as compliance becomes performative). This determines whether mandatrophy risk is empirical or merely political.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(universal_floor_enforcement_depth, empirical, 'Gap between the universal floor''s normative reach and actual enforcement capacity').

omega_variable(
    common_article_3_integration_debate,
    'Is Common Article 3 meaningfully integrated with human rights law under this reading, or is the reading analytically underdetermined about whether human rights derogations (''during public emergency'') vitiate the universal floor?',
    'Jurisprudence from human rights courts (ECtHR, IACtHR, ACCtHPR) and the ICC addressing whether armed-conflict status permits human rights derogations, or whether the universal reading forecloses derogation doctrine entirely.',
    'If the reading successfully integrates Common Article 3 and human rights non-derogability, the universal floor is robust. If derogation doctrine persists as a valid exception to human rights protections during armed conflict, the universal scope is undermined and the constraint reverts toward state-centric reading. This affects the extraction ceiling: if human rights can be derogated during conflict, state operational flexibility is higher than the universal reading claims.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(common_article_3_integration_debate, conceptual, 'Whether the universal floor successfully integrates human rights law or remains analytically distinct from derogation doctrine').

omega_variable(
    non_state_actor_dual_positioning,
    'Can non-state armed actors coherently occupy the dual role of beneficiaries (protected under the universal floor) and obligation-bearers (required to respect Common Article 3 toward their own captives and civilians) when their capacity for enforcement, documentation, and judicial accountability differs radically from state institutions?',
    'Case studies of non-state armed groups'' actual compliance with humanitarian obligations and accountability mechanisms'' treatment of non-state violations. Examine whether the dual role is analytically stable or whether non-state actors are systemically treated as lesser obligation-bearers.',
    'If the dual role is genuinely stable, the constraint coordinates all parties around a single floor. If it degrades into asymmetric treatment (non-state actors held to lower standards or not held accountable for violations), the constraint reverts toward hybrid-proportionality reading and loses universality claim.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(non_state_actor_dual_positioning, conceptual, 'Structural feasibility of universal beneficiary status + uniform obligation-bearing for non-state actors').

omega_variable(
    state_sovereignty_foreclosure_scope,
    'Does the universal reading''s assertion that all armed conflict falls within humanitarian law scope FORECLOSE the state-centric reading''s foundational claim that states retain discretion over internal security matters, or do the readings merely COEXIST as live positions held by different institutional actors?',
    'Examine contemporary state practice and doctrine. Do states that reject the universal reading (China, Russia, some others) operate under an alternative framework they defend as legitimate, or do they acknowledge the universal reading and simply violate it? Are there institutional forums where both readings are sustained as valid choices?',
    'If the readings FORECLOSE each other, we have a kernel structure where adoption of the universal reading logically requires rejection of state-centric reading. If they COEXIST, both are live positions and the kernel remains genuinely contested (the current state). If the coexistence erodes toward foreclosure, the constraint hardens and state operational latitude shrinks.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(state_sovereignty_foreclosure_scope, conceptual, 'Logical relationship between universal scope reading and state-centric reading — foreclosure vs. coexistence').

omega_variable(
    suppression_mechanism_internalization,
    'Is the measured suppression (0.58) structural (legal liability, sanctions, humanitarian access denial) or internalized (humanitarian doctrine has become part of military self-conception such that violations generate shame and internal enforcement)?',
    'Analysis of military training and doctrine integration: do armed forces teach Geneva protections as operational requirements (structural enforcement) or as values (internalized norm)? Post-violation patterns: do states that violate the universal floor experience internal institutional pressure, or only external accountability threats?',
    'If suppression is primarily structural, it depends on continuing international enforcement capacity (ICC, courts, sanctions). If internalized, it is more durable. The balance determines whether the constraint is vulnerable to enforcement capacity erosion or has deeper institutional roots.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalization, empirical, 'Whether suppression maintaining the universal floor is externally structural or internally internalized').

omega_variable(
    reading_distinction_foreclosure_axiom_correspondence,
    'This reading distinguishes from the state-centric reading by asserting universal non-derogable protection status. The state-centric reading asserts protection only for privileged combatants. Do these axioms logically foreclose each other (making the readings mutually exclusive in a single framework), or can both operate simultaneously in different institutional contexts?',
    'Examine whether a state can coherently maintain the universal floor internationally (ICC jurisdiction, human rights court binding) while implementing state-centric doctrine domestically. If yes, the readings coexist structurally. If the international commitment logically requires domestic doctrine change, the readings foreclose each other.',
    'Foreclosure would mean adopting the universal reading legally requires abandoning state-centric privilege doctrine globally, tightening the constraint. Coexistence means state actors can maintain dual frameworks (international posture, domestic practice), enabling evasion. This affects whether the constraint is structurally robust or theatrically maintained.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_distinction_foreclosure_axiom_correspondence, conceptual, 'Logical foreclosure vs. institutional coexistence of the universal reading and state-centric reading axioms').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(geneva_conventions_protective_scope__universal_rights_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gene_tr_t0, geneva_conventions_protective_scope__universal_rights_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement_basis(gene_tr_t0, observed).
narrative_ontology:measurement(gene_tr_t8, geneva_conventions_protective_scope__universal_rights_reading, theater_ratio, 8, 0.3).
narrative_ontology:measurement_basis(gene_tr_t8, observed).
narrative_ontology:measurement(gene_tr_t16, geneva_conventions_protective_scope__universal_rights_reading, theater_ratio, 16, 0.36).
narrative_ontology:measurement_basis(gene_tr_t16, observed).
narrative_ontology:measurement(gene_tr_t24, geneva_conventions_protective_scope__universal_rights_reading, theater_ratio, 24, 0.4).
narrative_ontology:measurement_basis(gene_tr_t24, observed).
narrative_ontology:measurement(gene_tr_t32, geneva_conventions_protective_scope__universal_rights_reading, theater_ratio, 32, 0.42).
narrative_ontology:measurement_basis(gene_tr_t32, observed).
narrative_ontology:measurement(gene_tr_t40, geneva_conventions_protective_scope__universal_rights_reading, theater_ratio, 40, 0.42).
narrative_ontology:measurement_basis(gene_tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(gene_be_t0, geneva_conventions_protective_scope__universal_rights_reading, base_extractiveness, 0, 0.58).
narrative_ontology:measurement_basis(gene_be_t0, observed).
narrative_ontology:measurement(gene_be_t8, geneva_conventions_protective_scope__universal_rights_reading, base_extractiveness, 8, 0.62).
narrative_ontology:measurement_basis(gene_be_t8, observed).
narrative_ontology:measurement(gene_be_t16, geneva_conventions_protective_scope__universal_rights_reading, base_extractiveness, 16, 0.67).
narrative_ontology:measurement_basis(gene_be_t16, observed).
narrative_ontology:measurement(gene_be_t24, geneva_conventions_protective_scope__universal_rights_reading, base_extractiveness, 24, 0.7).
narrative_ontology:measurement_basis(gene_be_t24, observed).
narrative_ontology:measurement(gene_be_t32, geneva_conventions_protective_scope__universal_rights_reading, base_extractiveness, 32, 0.72).
narrative_ontology:measurement_basis(gene_be_t32, observed).
narrative_ontology:measurement(gene_be_t40, geneva_conventions_protective_scope__universal_rights_reading, base_extractiveness, 40, 0.72).
narrative_ontology:measurement_basis(gene_be_t40, observed).

% Suppression requirement over time
narrative_ontology:measurement(gene_su_t0, geneva_conventions_protective_scope__universal_rights_reading, suppression_requirement, 0, 0.48).
narrative_ontology:measurement_basis(gene_su_t0, observed).
narrative_ontology:measurement(gene_su_t8, geneva_conventions_protective_scope__universal_rights_reading, suppression_requirement, 8, 0.51).
narrative_ontology:measurement_basis(gene_su_t8, observed).
narrative_ontology:measurement(gene_su_t16, geneva_conventions_protective_scope__universal_rights_reading, suppression_requirement, 16, 0.54).
narrative_ontology:measurement_basis(gene_su_t16, observed).
narrative_ontology:measurement(gene_su_t24, geneva_conventions_protective_scope__universal_rights_reading, suppression_requirement, 24, 0.56).
narrative_ontology:measurement_basis(gene_su_t24, observed).
narrative_ontology:measurement(gene_su_t32, geneva_conventions_protective_scope__universal_rights_reading, suppression_requirement, 32, 0.58).
narrative_ontology:measurement_basis(gene_su_t32, observed).
narrative_ontology:measurement(gene_su_t40, geneva_conventions_protective_scope__universal_rights_reading, suppression_requirement, 40, 0.58).
narrative_ontology:measurement_basis(gene_su_t40, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(geneva_conventions_protective_scope__universal_rights_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(geneva_conventions_protective_scope__universal_rights_reading, 0.12).
narrative_ontology:affects_constraint(geneva_conventions_protective_scope__universal_rights_reading, geneva_conventions_protective_scope__state_centric_reading).
narrative_ontology:affects_constraint(geneva_conventions_protective_scope__universal_rights_reading, geneva_conventions_protective_scope__hybrid_proportionality_reading).
narrative_ontology:affects_constraint(geneva_conventions_protective_scope__universal_rights_reading, international_criminal_court_jurisdiction_mandate).
narrative_ontology:affects_constraint(geneva_conventions_protective_scope__universal_rights_reading, human_rights_law_armed_conflict_application).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the contested kernel 'geneva_conventions_protective_scope'. All three readings (universal_rights, state_centric, hybrid_proportionality) represent structurally distinct interpretations of the same foundational legal text (Geneva Conventions and protocols). The three readings diverge on protective scope (universal vs. status-dependent vs. conflict-type-dependent), victim set (all persons vs. privileged combatants + civilians vs. scaled-by-conflict-type), and operational constraint on states. Each reading generates a separate constraint story with independent ε values because the scope of beneficiary/victim is different in each reading, producing different extractiveness profiles. Family links are bidirectional; each reading affects the others through interpretive competition and institutional propagation.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(geneva_conventions_protective_scope__universal_rights_reading, institutional, 0.75).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
