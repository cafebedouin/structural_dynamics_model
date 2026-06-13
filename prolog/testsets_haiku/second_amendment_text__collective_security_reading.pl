% ============================================================================
% CONSTRAINT STORY: second_amendment_text__collective_security_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_second_amendment_text__collective_security_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: second_amendment_text__collective_security_reading
 *   human_readable: Second Amendment Collective Security Reading: Militia Clause Conditions Individual Right
 *   domain: constitutional_law/political_theory
 *
 * SUMMARY:
 *   The Second Amendment reads: 'A well regulated Militia, being necessary to
 *   the security of a free State, the right of the people to keep and bear
 *   Arms, shall not be infringed.' The collective-security reading interprets
 *   the militia clause as conditioning the operative clause: the right to
 *   arms exists to serve organized civic defense under state coordination.
 *   Under this reading, the state may regulate arms through licensing,
 *   permitting, background checks, and categorical exclusions without
 *   violating the constitutional text. This is one reading of a contested
 *   kernel; the sibling readings (individual-right and
 *   originalist-civic-virtue) instantiate different constraints with
 *   different ε values and beneficiary structures. This story models ONLY the
 *   collective-security reading as a structurally coherent claim: the state's
 *   regulatory apparatus is the beneficiary; individual gun owners are the
 *   constrained class; the founding problem is preventing unorganized armed
 *   resistance to state authority.
 *
 * KEY AGENTS:
 *   - state_regulatory_apparatus: Institutional agenda-setter administering licensing and permitting regimes (powerful, generational horizon, arbitrage-level exit)
 *   - organized_militia_structures: Institutional beneficiary with prioritized legal status and resource allocation (institutional power, generational horizon)
 *   - individual_gun_owners: Moderate-power payer navigating licensing requirements (moderate power, biographical horizon, constrained exit)
 *   - unlicensed_firearm_possessors: Powerless, trapped class excluded by criminal history or immigration status (powerless, immediate horizon, trapped exit)
 *   - individual_right_advocates: Excluded powerful actors contesting the reading's legitimacy (powerful power, generational horizon, constrained exit as they are bound by the operative interpretation)
 *   - supreme_court: Observer institutional seat adjudicating between readings
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(second_amendment_text__collective_security_reading, 0.62).
domain_priors:suppression_score(second_amendment_text__collective_security_reading, 0.58).
domain_priors:theater_ratio(second_amendment_text__collective_security_reading, 0.31).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(second_amendment_text__collective_security_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(second_amendment_text__collective_security_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(second_amendment_text__collective_security_reading, theater_ratio, 0.31).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(second_amendment_text__collective_security_reading, accessibility_collapse, 0.71).
narrative_ontology:constraint_metric(second_amendment_text__collective_security_reading, resistance, 0.79).

% --- Constraint claim ---
narrative_ontology:constraint_claim(second_amendment_text__collective_security_reading, tangled_rope).
narrative_ontology:human_readable(second_amendment_text__collective_security_reading, "Second Amendment Collective Security Reading: Militia Clause Conditions Individual Right").
narrative_ontology:topic_domain(second_amendment_text__collective_security_reading, "constitutional_law/political_theory").

domain_priors:requires_active_enforcement(second_amendment_text__collective_security_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(second_amendment_text__collective_security_reading, '4322b836-584d-4ee2-a06f-4d9e92a4fb56').
narrative_ontology:cs_kernel_codification('4322b836-584d-4ee2-a06f-4d9e92a4fb56', fixed_text).
narrative_ontology:cs_authority_grounding('4322b836-584d-4ee2-a06f-4d9e92a4fb56', lineage).
narrative_ontology:cs_interpretation_layer_present('4322b836-584d-4ee2-a06f-4d9e92a4fb56').
narrative_ontology:cs_reading_relation('4322b836-584d-4ee2-a06f-4d9e92a4fb56', second_amendment_text__individual_right_reading, coexists_with).
narrative_ontology:cs_reading_relation('4322b836-584d-4ee2-a06f-4d9e92a4fb56', second_amendment_text__originalist_civic_virtue_reading, influences).
narrative_ontology:cs_axiom('4322b836-584d-4ee2-a06f-4d9e92a4fb56', foundational, militia_clause_conditions_operative_clause).
narrative_ontology:cs_axiom_status(militia_clause_conditions_operative_clause, holdable).
narrative_ontology:cs_axiom_grounding('4322b836-584d-4ee2-a06f-4d9e92a4fb56', militia_clause_conditions_operative_clause, deontological).
narrative_ontology:cs_axiom('4322b836-584d-4ee2-a06f-4d9e92a4fb56', foundational, state_regulatory_discretion_serves_collective_security).
narrative_ontology:cs_axiom_status(state_regulatory_discretion_serves_collective_security, holdable).
narrative_ontology:cs_axiom_grounding('4322b836-584d-4ee2-a06f-4d9e92a4fb56', state_regulatory_discretion_serves_collective_security, instrumental).
narrative_ontology:cs_reference_frame('4322b836-584d-4ee2-a06f-4d9e92a4fb56', militia_conditioning_constitutional_authority).
narrative_ontology:cs_drift_state('4322b836-584d-4ee2-a06f-4d9e92a4fb56', contemporary_individual_right_revival, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('4322b836-584d-4ee2-a06f-4d9e92a4fb56', '').
narrative_ontology:cs_kernel_id(second_amendment_text__collective_security_reading, second_amendment_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(second_amendment_text__collective_security_reading, state_regulatory_apparatus).
narrative_ontology:constraint_beneficiary(second_amendment_text__collective_security_reading, organized_militia_structures).
narrative_ontology:constraint_victim(second_amendment_text__collective_security_reading, individual_gun_owners).
narrative_ontology:constraint_victim(second_amendment_text__collective_security_reading, unlicensed_firearm_possessors).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(second_amendment_text__collective_security_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(second_amendment_text__collective_security_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(second_amendment_text__collective_security_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(second_amendment_text__collective_security_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(second_amendment_text__collective_security_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate-high (0.62 at interval end) because the regulatory regime transfers discretionary control over arms possession from individuals to the state, and the state derives material benefit (licensing revenue, enforcement authority) from that arrangement. Suppression is substantial (0.58) because the constraint persists through active enforcement: permitting denials, criminal sanctions for unlicensed possession, and categorical statutory exclusions. Theater ratio is moderate-low (0.31), indicating the licensing and safety functions are partially real (background checks do screen dangerous persons; permitting does coordinate access) but a growing share of regulatory activity maintains categorical exclusion rather than solely serving public safety. The measurement series shows extractiveness rising from 0.48 to 0.62 over the interval as licensing regimes expanded and categorical exclusions (e.g., domestic violence convictions) widened. Theater ratio plateaued around t=21 and declined slightly, suggesting enforcement infrastructure became relatively mature and focused. Suppression requirement rose steadily, indicating the constraint requires increasingly active enforcement as resistance from individual-right advocates mounted (captured by the rising resistance metric in base_properties). All metrics are authored on one shared time grid (t=0,7,14,21,28,35 covers the full interval), ensuring temporal coherence.
 *
 * PERSPECTIVAL GAP:
 *   The state regulatory apparatus and the individual gun owner see this constraint completely differently. From the state's seat, the militia-conditioning reading is a legitimate constitutional framework that enables public safety coordination and organized defense; the constraint is coordination with extraction as the necessary overhead of state-mediated arms allocation. From the individual gun owner's seat (especially those denied licensing), the same structure is extraction with coordination as the cover story — their right is conditioned on state permission, which is the definition of a revocable privilege, not a protected right. The engine computes these divergent classifications from the structural data: the beneficiary (state apparatus, d ≈ 0.1) sees a rope-flavored coordination; the payer (individual owners, d ≈ 0.85) sees a tangled_rope or snare depending on the tightness of permitting denial. This is not an error in the JSON — this perspectival divergence is the measurement the corpus exists to capture.
 *
 * DIRECTIONALITY LOGIC:
 *   The state regulatory apparatus is the structural beneficiary: it administers the rules, collects licensing revenue, retains discretionary gatekeeping power, and derives legitimacy from the militia-conditioning interpretation. Its directionality is near the beneficiary end (d ≈ 0.1–0.2). Individual gun owners are the targets: they must petition the state for permission, bear the cost of licensing and background checks, face categorical exclusion based on state judgment, and lack exit (relocation means leaving one's community and possessions). Their d is near the target end (0.75–0.85). Organized militia structures benefit from prioritized legal status and state-provided training/resources; their d is low (0.15–0.25). Unlicensed possessors are maximally targeted (d ≈ 0.95) — trapped, criminalized, excluded from even petitioning for access. Public safety constituency benefits indirectly (lower violence rates, screening of dangerous persons) but bears no direct cost; their d is slightly beneficiary-side (0.25–0.35). No directionality override is needed — the derivation chain (beneficiary/victim + power + exit → d) produces the correct seats.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint does NOT exhibit mandatrophy in the classical sense (function atrophied, persistence by inertia). The militia-conditioning frame is actively defended by state regulators, public-safety constituencies, and legal scholars; the regulatory apparatus functions to screen dangerous persons and allocate organized arms capacity. However, there is a deep contest over whether the founding problem remains live. The collective-security reading claims the founding problem is preventing unorganized armed private militias and ensuring state-controlled armed capacity — a problem that advocates argue remains live (mass shootings, armed private militias, domestic extremism). But originalist scholars and individual-rights advocates contend the founding problem was never preventing private arms possession but preventing state disarmament of the citizenry — and that problem is inverted: the state is increasingly armed relative to citizens. This contest is not mandatrophy (function decay) but legitimacy contest. It is captured in the omega variables addressing whether the militia-conditioning reading's founding-problem diagnosis is empirically or normatively sound.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    militia_conditioning_primacy,
    'Does the militia clause condition the operative clause (making the right to arms instrumental to organized collective security), or is it merely a prefatory statement of purpose while the operative clause grants an independent right?',
    'Originalist historical analysis of founding-era grammar, usage, and constitutional design; Supreme Court precedent adjudicating the relationship between the two clauses. Natural experiment: jurisdictions that permit individual possession without militia affiliation and compare their arms-ownership patterns and public safety outcomes against the militia-conditioning prediction.',
    'If militia conditioning is primacy, state regulation is constitutionally permissible; if the operative clause is independent, state permitting requirements may violate the right. This is the core structural question that determines whether the collective-security or individual-right reading controls.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(militia_conditioning_primacy, conceptual, 'Whether the militia clause subordinates the operative clause or merely introduces it').

omega_variable(
    founding_problem_persistence,
    'Is the founding problem — preventing unorganized armed insurrection and ensuring state-controlled militia capacity — still a live threat, or has it been solved by professionalization of standing armies and nation-state monopoly on force?',
    'Historical assessment of unorganized militia violence (Whiskey Rebellion, pre-Civil War secession movements, post-Reconstruction private militia action) versus contemporary domestic extremism (armed private militias in 2020s); expert testimony on whether the founding problem persists or is solved.',
    'If the founding problem persists, the militia-conditioning frame remains functionally justified; if solved, the constraint becomes mandatrophic — a regulation persisting from a defunct problem, maintained by beneficiary interest rather than genuine necessity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(founding_problem_persistence, empirical, 'Whether the founding problem the collective-security reading addresses is still empirically present').

omega_variable(
    suppression_mechanism_internalization,
    'Is the measured suppression (0.58) primarily structural (active enforcement through criminal law, licensing denial, categorical exclusion) or internalized (internalized belief that the militia frame is legitimate, making individuals voluntarily comply even without enforcement)?',
    'Comparative jurisdiction analysis: jurisdictions with permissive individual-right interpretation versus strict militia-conditioning interpretation; survey data on whether individuals in strict-interpretation states accept the militia-conditioning rationale or merely comply under duress. Post-legal-shift analysis: if the Supreme Court forecloses the militia-conditioning reading, do licensing regimes dissolve or do states reinvent them on alternative grounds, and does compliance behavior change?',
    'If suppression is structural, removing the legal enforcement apparatus (licensing requirement, criminal sanction, categorical exclusion) would change behavior. If suppression is internalized, individuals might continue accepting the frame even after legal removal. This affects the constraint''s vulnerability to legal reversal.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalization, empirical, 'Whether suppression is structural enforcement or internalized acceptance of the militia-conditioning frame').

omega_variable(
    reading_contest_foreclosure,
    'Does the collective-security reading foreclose the individual-right reading, or do they coexist as live positions held by different legal factions?',
    'Supreme Court binding precedent foreclosing one reading or both coexisting as permissible within their respective jurisdictional contexts. If Bruen''s historical test is applied, does it necessarily foreclose militia-conditioning or permit both readings as historically grounded?',
    'If foreclosed, the collective-security reading becomes one of two rival constraints awaiting canonical resolution; if coexisting, both readings remain operative in different jurisdictions. This affects the long-term stability and resource allocation within each reading''s institutional base.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_contest_foreclosure, conceptual, 'Whether the collective-security reading''s core premise logically forecloses the individual-right reading or both remain viable').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(second_amendment_text__collective_security_reading, 0, 35).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(seco_tr_t0, second_amendment_text__collective_security_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement(seco_tr_t7, second_amendment_text__collective_security_reading, theater_ratio, 7, 0.24).
narrative_ontology:measurement(seco_tr_t14, second_amendment_text__collective_security_reading, theater_ratio, 14, 0.27).
narrative_ontology:measurement(seco_tr_t21, second_amendment_text__collective_security_reading, theater_ratio, 21, 0.31).
narrative_ontology:measurement(seco_tr_t28, second_amendment_text__collective_security_reading, theater_ratio, 28, 0.33).
narrative_ontology:measurement(seco_tr_t35, second_amendment_text__collective_security_reading, theater_ratio, 35, 0.31).

% Extraction over time
narrative_ontology:measurement(seco_be_t0, second_amendment_text__collective_security_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(seco_be_t7, second_amendment_text__collective_security_reading, base_extractiveness, 7, 0.52).
narrative_ontology:measurement(seco_be_t14, second_amendment_text__collective_security_reading, base_extractiveness, 14, 0.57).
narrative_ontology:measurement(seco_be_t21, second_amendment_text__collective_security_reading, base_extractiveness, 21, 0.61).
narrative_ontology:measurement(seco_be_t28, second_amendment_text__collective_security_reading, base_extractiveness, 28, 0.63).
narrative_ontology:measurement(seco_be_t35, second_amendment_text__collective_security_reading, base_extractiveness, 35, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(seco_su_t0, second_amendment_text__collective_security_reading, suppression_requirement, 0, 0.48).
narrative_ontology:measurement(seco_su_t7, second_amendment_text__collective_security_reading, suppression_requirement, 7, 0.52).
narrative_ontology:measurement(seco_su_t14, second_amendment_text__collective_security_reading, suppression_requirement, 14, 0.56).
narrative_ontology:measurement(seco_su_t21, second_amendment_text__collective_security_reading, suppression_requirement, 21, 0.6).
narrative_ontology:measurement(seco_su_t28, second_amendment_text__collective_security_reading, suppression_requirement, 28, 0.63).
narrative_ontology:measurement(seco_su_t35, second_amendment_text__collective_security_reading, suppression_requirement, 35, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(second_amendment_text__collective_security_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(second_amendment_text__collective_security_reading, 0.14).
narrative_ontology:affects_constraint(second_amendment_text__collective_security_reading, second_amendment_text__individual_right_reading).
narrative_ontology:affects_constraint(second_amendment_text__collective_security_reading, second_amendment_text__originalist_civic_virtue_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the contested Second Amendment text. The collective-security reading treats the militia clause as conditioning the operative clause, enabling state regulation of arms as serving organized collective defense. Sibling readings (individual_right and originalist_civic_virtue) instantiate different constraints with different beneficiary structures and ε values. The three readings form a constraint family linked via this affects_constraints network. Each reading is a complete, self-contained constraint story with its own classification; they are not measurements of the same constraint but different constraints instantiated from the same textual kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
