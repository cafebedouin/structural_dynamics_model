% ============================================================================
% CONSTRAINT STORY: fourth_amendment__warrant_preference_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_fourth_amendment__warrant_preference_reading, []).

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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: fourth_amendment__warrant_preference_reading
 *   human_readable: Fourth Amendment Warrant Preference Doctrine (Textual Linkage Reading)
 *   domain: constitutional_law/criminal_procedure
 *
 * SUMMARY:
 *   The Fourth Amendment's warrant-preference reading treats the two clauses
 *   as structurally linked: the Reasonableness Clause establishes a
 *   presumption against warrantless search, and the Warrant Clause provides
 *   the mechanism (magistrate approval) for overriding that presumption in
 *   specific instances. Exceptions to the warrant requirement (exigency,
 *   consent, plain view, vehicle search, etc.) must be strictly construed to
 *   preserve the presumption. This reading generates a tangled-rope
 *   constraint: it provides a genuine coordination mechanism (the magistrate
 *   gate aggregates individual privacy and state investigative authority)
 *   while simultaneously suppressing law enforcement's operational
 *   flexibility through enumerated exceptions. The constraint benefits
 *   searched individuals (by establishing presumptive protection) and
 *   magistrate authority (by consolidating gatekeeper power), while imposing
 *   costs on law enforcement (by limiting workarounds). The extractiveness
 *   value reflects that law enforcement must invest resources in warrant
 *   procedures, but can still achieve investigative goals through legitimate
 *   pathways. The suppression value (0.78) reflects the doctrine's aggressive
 *   closure of exceptions — the reading intentionally creates high barriers
 *   to warrantless search to enforce the presumption. The theater ratio
 *   (0.22) is low because the doctrine's suppression is genuine and
 *   structural, not performative — the warrant requirement creates real
 *   friction in police procedure, not ceremonial theater.
 *
 * KEY AGENTS:
 *   - Searched Individuals: Primary beneficiaries (powerless/trapped, constrained/with-remedy) — the warrant requirement creates presumptive protection against warrantless intrusion, though enforcement through exclusionary rule is incomplete
 *   - Law Enforcement Agencies: Primary victims (organized/constrained) — the warrant-preference reading suppresses operational flexibility by requiring magistrate approval and strictly limiting exceptions
 *   - Magistrate Authority: Secondary beneficiary (institutional/arbitrage) — the doctrine consolidates gatekeeper power and institutional prestige through the warrant-approval role
 *   - Exigency-Claiming Officers: Specific victim group (organized/constrained) — emergency-based warrantless searches are strictly scrutinized; exigency must be contemporaneous and particularized
 *   - Analytical Observer: Systemic view (analytical/analytical) — sees coordination function (presumption + magistrate gate) coupled with extraction through suppression of alternatives
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fourth_amendment__warrant_preference_reading, 0.35).
domain_priors:suppression_score(fourth_amendment__warrant_preference_reading, 0.78).
domain_priors:theater_ratio(fourth_amendment__warrant_preference_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fourth_amendment__warrant_preference_reading, extractiveness, 0.35).
narrative_ontology:constraint_metric(fourth_amendment__warrant_preference_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(fourth_amendment__warrant_preference_reading, theater_ratio, 0.22).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fourth_amendment__warrant_preference_reading, tangled_rope).
narrative_ontology:human_readable(fourth_amendment__warrant_preference_reading, "Fourth Amendment Warrant Preference Doctrine (Textual Linkage Reading)").
narrative_ontology:topic_domain(fourth_amendment__warrant_preference_reading, "constitutional_law/criminal_procedure").

domain_priors:requires_active_enforcement(fourth_amendment__warrant_preference_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(fourth_amendment__warrant_preference_reading, '5c1169d2-01ce-4e46-ae1e-1ee765e6e48d').
narrative_ontology:cs_kernel_codification('5c1169d2-01ce-4e46-ae1e-1ee765e6e48d', fixed_text).
narrative_ontology:cs_authority_grounding('5c1169d2-01ce-4e46-ae1e-1ee765e6e48d', lineage).
narrative_ontology:cs_interpretation_layer_present('5c1169d2-01ce-4e46-ae1e-1ee765e6e48d').
narrative_ontology:cs_reading_relation('5c1169d2-01ce-4e46-ae1e-1ee765e6e48d', fourth_amendment__reasonableness_balancing_reading, coexists_with).
narrative_ontology:cs_axiom('5c1169d2-01ce-4e46-ae1e-1ee765e6e48d', foundational, warrantless_search_presumptively_unreasonable).
narrative_ontology:cs_axiom_status(warrantless_search_presumptively_unreasonable, holdable).
narrative_ontology:cs_axiom_grounding('5c1169d2-01ce-4e46-ae1e-1ee765e6e48d', warrantless_search_presumptively_unreasonable, deontological).
narrative_ontology:cs_axiom('5c1169d2-01ce-4e46-ae1e-1ee765e6e48d', foundational, exceptions_must_be_jealously_confined).
narrative_ontology:cs_axiom_status(exceptions_must_be_jealously_confined, holdable).
narrative_ontology:cs_axiom_grounding('5c1169d2-01ce-4e46-ae1e-1ee765e6e48d', exceptions_must_be_jealously_confined, deontological).
narrative_ontology:cs_reference_frame('5c1169d2-01ce-4e46-ae1e-1ee765e6e48d', presumption_against_warrantless_search).
narrative_ontology:cs_drift_state('5c1169d2-01ce-4e46-ae1e-1ee765e6e48d', contemporary_post_riley_era, gap(stable, minor, true)).
narrative_ontology:cs_created_at('5c1169d2-01ce-4e46-ae1e-1ee765e6e48d', '').
narrative_ontology:cs_kernel_id(fourth_amendment__warrant_preference_reading, fourth_amendment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(fourth_amendment__warrant_preference_reading, searched_individuals).
narrative_ontology:constraint_beneficiary(fourth_amendment__warrant_preference_reading, magistrate_gatekeeping_authority).
narrative_ontology:constraint_victim(fourth_amendment__warrant_preference_reading, law_enforcement_operational_flexibility).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SEARCHED INDIVIDUAL (SNARE) — Structurally powerless, trapped in jurisdiction, with no exit. The warrant requirement provides formal protection, but enforcement barriers (exclusionary rule limitations, qualified immunity, procedural complexity) severely constrain remedy. The individual bears the cost of warrantless searches and cannot easily exit the legal jurisdiction or contest the intrusion after the fact. Experiences the constraint as maximally extractive of privacy — the warrant-preference rule exists but has limited practical force.
constraint_indexing:constraint_classification(fourth_amendment__warrant_preference_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: SEARCHED INDIVIDUAL WITH EXCLUSIONARY RULE ACCESS (ROPE) — The same individual, but with access to exclusionary rule remedy in criminal prosecution. The warrant-preference rule provides a genuine coordination mechanism: the magistrate's role is to coordinate between individual privacy and state investigative needs. The exclusionary rule creates a cost to warrantless search that incentivizes the state to seek warrants. From this perspective, the constraint solves a collective action problem — it prevents both the individual from having no recourse AND the state from operating without any process. Low effective extraction because the remedy exists.
constraint_indexing:constraint_classification(fourth_amendment__warrant_preference_reading, rope,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: LAW ENFORCEMENT AGENCIES (TANGLED ROPE) — Organized but structurally constrained by the warrant requirement. Law enforcement benefits from the coordination function: the magistrate approval process legitimizes searches and provides a legitimate pathway for evidence gathering. But the warrant-preference reading imposes suppression through the exceptions doctrine — exigency, consent, plain view, and other carve-outs are strictly construed, limiting workarounds. The constraint both enables (through legitimation) and constrains (through enumerated exceptions) law enforcement action. Moderate extraction experienced because agencies have coordinated pathways but face suppressive barriers to flexibility.
constraint_indexing:constraint_classification(fourth_amendment__warrant_preference_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: MAGISTRATE/JUDICIAL AUTHORITY (ROPE) — The magistrate benefits from the warrant-preference reading by capturing gatekeeper authority. The doctrine treats the magistrate as the neutral arbiter, positioned between privacy and search authority. This is a coordination mechanism at the institutional level: the magistrate's role is to aggregate individual interests (privacy) with state interests (investigation) and produce a single decision. The magistrate experiences low extraction because the doctrine grants institutional prestige and functional authority. No meaningful cost; primary benefit is institutional role-consolidation.
constraint_indexing:constraint_classification(fourth_amendment__warrant_preference_reading, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: MAGISTRATE GATE AS THEATRICAL CHECK (PITON) — At a longer civilizational horizon, the magistrate's gatekeeper role may be substantially performative. Magistrates issue warrants at high rates (typically 98%+); the gate rarely closes. The warrant-preference reading creates a ritual of neutral adjudication without necessarily constraining search authority in practice. The doctrine persists through institutional inertia and legitimacy theater — the appearance of constraint matters more than constraint itself. Theater ratio is low (0.22) because the doctrine's real suppressive force is high, but the magistrate gate's actual filtering capacity is degraded.
constraint_indexing:constraint_classification(fourth_amendment__warrant_preference_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) — The warrant-preference reading exhibits genuine doctrinal coherence at the text level (two clauses, unified principle) combined with extractive gaps in practice (weak exceptions enforcement, magistrate approval rates, civil damages unavailability). The constraint genuinely coordinates between privacy and investigation through the warrant requirement but extractively suppresses alternatives to warrant-based search through the exceptions doctrine. The analytical view confirms tangled-rope classification: real coordination function married to structural suppression.
constraint_indexing:constraint_classification(fourth_amendment__warrant_preference_reading, tangled_rope,
    context(agent_power(analytical),
            time_horizon(generational),
            exit_options(analytical),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(fourth_amendment__warrant_preference_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(fourth_amendment__warrant_preference_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(fourth_amendment__warrant_preference_reading, TypeOther, context(agent_power(powerless), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(fourth_amendment__warrant_preference_reading, TR),
    TR >= 0.70.

:- end_tests(fourth_amendment__warrant_preference_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.35): Moderate. Law enforcement can still conduct searches through the warrant pathway and limited exceptions, so the extractive cost is not prohibitive. The constraint does not prevent investigation; it channels investigation through a gatekeeping process. Extractiveness reflects the resource cost of warrant procedure and the risk of suppression if warrant procedure is not followed, but not a total prohibition. Suppression (0.78): High. This is the reading's defining feature: exceptions are meant to be jealously confined, not broadly construed. The suppression value reflects the doctrine's aggressive closure of workarounds — exigency must be genuine, consent must be knowing, plain view requires lawful initial intrusion, vehicle searches are now limited by Riley v. California (cell phone exception). The warrant-preference reading intentionally maintains high suppression to enforce the presumption against warrantless search. Theater ratio (0.22): Low. The warrant requirement creates real friction in police procedure — magistrates must be approached, probable cause must be established, warrants must be obtained before most searches. This is structural constraint, not theater. The low theater ratio reflects that the suppression is genuine, not ritualistic.
 *
 * PERSPECTIVAL GAP:
 *   This reading generates a wide perspectival gap between beneficiaries and victims. Searched individuals (if they have exclusionary rule access) see coordination (rope). Law enforcement agencies see mixed benefit/burden (tangled rope). Magistrates see pure coordination benefit (rope). The gap reveals the constraint's asymmetry: the warrant requirement benefits some groups (individuals with remedy access, institutional gatekeepers) while extracting from others (law enforcement flexibility). The reasonableness-balancing reading collapses this gap by questioning whether the presumption against warrantless search is actually mandated by the text or is a judicial interpretation that could be reweighed.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) values are derived from each agent's structural relationship to the warrant requirement. Searched individuals are beneficiaries with limited (constrained) exit options — high exit cost to leaving jurisdiction, but high benefit to warrant protection; d ≈ 0.35. Law enforcement agencies are victims with organized response capacity and constrained (not trapped) exit — they face suppression but can adapt procedure; d ≈ 0.55. Magistrates are beneficiaries with arbitrage options — they gain gatekeeper authority and can selectively approve/deny based on institutional interests; d ≈ 0.10. The f(d) sigmoid maps these to effective powerfulness modifiers: beneficiaries with modest exit costs experience negative χ (the constraint subsidizes them); victims with organized response experience moderate χ (they bear suppression but have agency); beneficiaries with arbitrage experience very negative χ (the constraint strongly benefits them). The overall χ formula scales extractiveness by these directionality components and applies scope modifier for national scope (σ ≈ 1.0).
 *
 * MANDATROPHY ANALYSIS:
 *   The warrant-preference reading does not face mandatrophy because suppression (0.78) is genuinely high and coordination function is real. The doctrine both coordinates (magistrate approval as neutral arbiter) and suppresses (exceptions are narrow). The mandate is stable: the reading commits to high suppression as a feature, not a bug. No contradiction exists between claiming the reading coordinates while maintaining severe suppression — the whole point is that the suppression IS the coordination mechanism (it channels search through a neutral gatekeeper rather than leaving it to police discretion alone).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    magistrate_gate_filtering_efficacy,
    'Does the magistrate warrant gate actually filter searches, or do high approval rates (98%+) indicate the gate is performative theater masking state discretion?',
    'Longitudinal analysis of warrant denial rates across jurisdictions; correlation with warrant approval patterns and suppression of evidence claims; comparison to civilian review boards or alternative gatekeeping models',
    'If gate is filtering: warrant-preference reading maintains tangled-rope classification with genuine suppression. If gate is performative: classification drifts toward piton (institutional theater) or snare (suppression without filtering). This determines whether the doctrine''s suppression value (0.78) reflects real constraint or institutional ritual.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(magistrate_gate_filtering_efficacy, empirical, 'Whether magistrate warrant approval acts as a real constraint or performative filter').

omega_variable(
    exceptions_doctrine_scope_ambiguity,
    'Does the warrant-preference reading truly ''jealously confine'' exceptions, or do exigency, consent, plain view, and vehicle search doctrines create workable routes around the warrant requirement?',
    'Doctrinal mapping of exception scope and frequency; empirical count of warrantless searches justified by each exception; analysis of how courts define ''exigency'' and ''plain view'' breadth',
    'If exceptions are narrow: suppression (0.78) is accurate, warrant-preference reading is binding. If exceptions are broad: suppression is overstated, exceptions doctrine undermines the presumption against warrantless search, and the reading is coexistent with reasonableness-balancing reading rather than foreclosing it.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(exceptions_doctrine_scope_ambiguity, empirical, 'Scope and practical breadth of Fourth Amendment exceptions under warrant-preference reading').

omega_variable(
    kernel_reading_contest_framing,
    'Is the contest between warrant-preference and reasonableness-balancing readings a genuine logical foreclosure (one rules out the other) or a coexistence of different legitimacy frameworks held by different institutional actors?',
    'Jurisprudential analysis of how courts frame the two readings; whether warrant-preference advocates claim reasonableness-balancing is logically incoherent or merely incorrect; whether the readings have been held simultaneously within the same court or same doctrinal era',
    'If genuine foreclosure: reading_relations.relation = ''forecloses''. If coexistence: reading_relations.relation = ''coexists_with''. If the readings create mutual pressure but neither eliminates the other logically: reading_relations.relation = ''influences''. This determines the kernel structure itself.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest_framing, conceptual, 'Logical relationship between warrant-preference and reasonableness-balancing readings of the Fourth Amendment').

omega_variable(
    exclusionary_rule_remedial_sufficiency,
    'Does the exclusionary rule provide adequate remedy for warrantless searches, or are evidentiary suppression and qualified immunity gaps rendering the warrant preference substantively unenforceable?',
    'Comparative analysis of exclusionary rule application in different search contexts; frequency of qualified immunity grants in warrantless search claims; extent of civil damages recovery for Fourth Amendment violations; measurement of deterrent effect on police conduct',
    'If exclusionary rule is sufficient: beneficiary status (searched individuals) is accurate and the tangled-rope classification holds. If remedy is inadequate: victims experience snare-level suppression and beneficiary claims are nominal. This affects whether suppression (0.78) represents real structural constraint or formal doctrine without enforcement.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(exclusionary_rule_remedial_sufficiency, empirical, 'Adequacy of exclusionary rule as remedy for Fourth Amendment violations').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fourth_amendment__warrant_preference_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fa_wp_theater_t0, fourth_amendment__warrant_preference_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement(fa_wp_theater_t50, fourth_amendment__warrant_preference_reading, theater_ratio, 50, 0.2).
narrative_ontology:measurement(fa_wp_theater_t100, fourth_amendment__warrant_preference_reading, theater_ratio, 100, 0.22).

% Extraction over time
narrative_ontology:measurement(fa_wp_extractiveness_t0, fourth_amendment__warrant_preference_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(fa_wp_extractiveness_t50, fourth_amendment__warrant_preference_reading, base_extractiveness, 50, 0.32).
narrative_ontology:measurement(fa_wp_extractiveness_t100, fourth_amendment__warrant_preference_reading, base_extractiveness, 100, 0.35).

% Suppression requirement over time
narrative_ontology:measurement(fa_wp_suppression_t0, fourth_amendment__warrant_preference_reading, suppression_requirement, 0, 0.65).
narrative_ontology:measurement(fa_wp_suppression_t50, fourth_amendment__warrant_preference_reading, suppression_requirement, 50, 0.72).
narrative_ontology:measurement(fa_wp_suppression_t100, fourth_amendment__warrant_preference_reading, suppression_requirement, 100, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(fourth_amendment__warrant_preference_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(fourth_amendment__warrant_preference_reading, fourth_amendment__reasonableness_balancing_reading).

% DUAL FORMULATION NOTE:
% The Fourth Amendment kernel admits two structurally distinct readings that produce different constraints with different extractiveness and suppression profiles. The warrant-preference reading establishes high suppression and moderate extractiveness through strict exception enforcement; the reasonableness-balancing reading permits flexible case-by-case balancing, producing lower suppression and potentially higher extractiveness. These are not two observations of one constraint but two different constraints instantiated by the same contested text. Both stories link via network to clarify the kernel contest.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
