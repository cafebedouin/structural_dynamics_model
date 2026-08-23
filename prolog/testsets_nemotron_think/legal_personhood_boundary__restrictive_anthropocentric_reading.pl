% ============================================================================
% CONSTRAINT STORY: legal_personhood_boundary__restrictive_anthropocentric_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_legal_personhood_boundary__restrictive_anthropocentric_reading, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: legal_personhood_boundary__restrictive_anthropocentric_reading
 *   human_readable: Legal Personhood Limited to Born Humans with Cognitive Capacity
 *   domain: legal_philosophy/constitutional_law/rights_theory
 *
 * SUMMARY:
 *   This constraint story captures the restrictive anthropocentric reading of
 *   the legal personhood boundary kernel. The reading asserts that legal
 *   personhood attaches only at birth and only to humans possessing cognitive
 *   capacity. This boundary maximizes pregnant person autonomy, excludes
 *   fetal personhood claims, and blocks rights-of-nature and AI personhood
 *   expansions. The reading claims this boundary is a natural/biological fact
 *   (Mountain), but identifiable beneficiaries (pregnant persons, autonomy
 *   advocates, limited-government advocates) exist — making it a false-summit
 *   candidate. The engine will compute per-seat classifications from the
 *   structural data: the agenda-setter (state legal system) and beneficiaries
 *   should experience low effective extraction, while payer seats
 *   (fetal-personhood, rights-of-nature, AI-personhood advocates) should
 *   experience high effective extraction despite the reading's claim that
 *   they are not rights-bearers.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(legal_personhood_boundary__restrictive_anthropocentric_reading, 0.45).
domain_priors:suppression_score(legal_personhood_boundary__restrictive_anthropocentric_reading, 0.6).
domain_priors:theater_ratio(legal_personhood_boundary__restrictive_anthropocentric_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(legal_personhood_boundary__restrictive_anthropocentric_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(legal_personhood_boundary__restrictive_anthropocentric_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(legal_personhood_boundary__restrictive_anthropocentric_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(legal_personhood_boundary__restrictive_anthropocentric_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(legal_personhood_boundary__restrictive_anthropocentric_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(legal_personhood_boundary__restrictive_anthropocentric_reading, mountain).
narrative_ontology:human_readable(legal_personhood_boundary__restrictive_anthropocentric_reading, "Legal Personhood Limited to Born Humans with Cognitive Capacity").
narrative_ontology:topic_domain(legal_personhood_boundary__restrictive_anthropocentric_reading, "legal_philosophy/constitutional_law/rights_theory").

domain_priors:requires_active_enforcement(legal_personhood_boundary__restrictive_anthropocentric_reading).
domain_priors:emerges_naturally(legal_personhood_boundary__restrictive_anthropocentric_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(legal_personhood_boundary__restrictive_anthropocentric_reading, '02a984b1-3373-490f-a0f7-7502cbbced5e').
narrative_ontology:cs_kernel_codification('02a984b1-3373-490f-a0f7-7502cbbced5e', formalized).
narrative_ontology:cs_authority_grounding('02a984b1-3373-490f-a0f7-7502cbbced5e', lineage).
narrative_ontology:cs_interpretation_layer_present('02a984b1-3373-490f-a0f7-7502cbbced5e').
narrative_ontology:cs_reading_relation('02a984b1-3373-490f-a0f7-7502cbbced5e', legal_personhood_boundary__developmental_potentiality_reading, forecloses).
narrative_ontology:cs_reading_relation('02a984b1-3373-490f-a0f7-7502cbbced5e', legal_personhood_boundary__functional_capacity_reading, coexists_with).
narrative_ontology:cs_axiom('02a984b1-3373-490f-a0f7-7502cbbced5e', foundational, birth_and_cognition_required_for_personhood).
narrative_ontology:cs_axiom_status(birth_and_cognition_required_for_personhood, holdable).
narrative_ontology:cs_axiom_grounding('02a984b1-3373-490f-a0f7-7502cbbced5e', birth_and_cognition_required_for_personhood, deontological).
narrative_ontology:cs_axiom('02a984b1-3373-490f-a0f7-7502cbbced5e', secondary, pregnant_person_autonomy_maximized).
narrative_ontology:cs_axiom_status(pregnant_person_autonomy_maximized, holdable).
narrative_ontology:cs_axiom_grounding('02a984b1-3373-490f-a0f7-7502cbbced5e', pregnant_person_autonomy_maximized, deontological).
narrative_ontology:cs_reference_frame('02a984b1-3373-490f-a0f7-7502cbbced5e', birth_based_personhood_framework).
narrative_ontology:cs_drift_state('02a984b1-3373-490f-a0f7-7502cbbced5e', contemporary_expansion_pressure, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('02a984b1-3373-490f-a0f7-7502cbbced5e', '').
narrative_ontology:cs_kernel_id(legal_personhood_boundary__restrictive_anthropocentric_reading, legal_personhood_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(legal_personhood_boundary__restrictive_anthropocentric_reading, pregnant_persons).
narrative_ontology:constraint_beneficiary(legal_personhood_boundary__restrictive_anthropocentric_reading, reproductive_autonomy_advocates).
narrative_ontology:constraint_beneficiary(legal_personhood_boundary__restrictive_anthropocentric_reading, limited_government_advocates).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(legal_personhood_boundary__restrictive_anthropocentric_reading, fetal_personhood_advocates).
narrative_ontology:constraint_victim(legal_personhood_boundary__restrictive_anthropocentric_reading, rights_of_nature_advocates).
narrative_ontology:constraint_victim(legal_personhood_boundary__restrictive_anthropocentric_reading, ai_personhood_advocates).
narrative_ontology:constraint_vindicates(legal_personhood_boundary__restrictive_anthropocentric_reading, bodily_autonomy_principle).
narrative_ontology:constraint_vindicates(legal_personhood_boundary__restrictive_anthropocentric_reading, birth_as_bright_line_personhood).
narrative_ontology:constraint_vindicates(legal_personhood_boundary__restrictive_anthropocentric_reading, cognitive_capacity_threshold_for_rights).
narrative_ontology:constraint_vindicates(legal_personhood_boundary__restrictive_anthropocentric_reading, state_neutrality_on_metaphysical_personhood).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold full legal personhood and reproductive decision authority under this reading. The birth-based boundary protects abortion access and reproductive autonomy from state compelled-pregnancy laws. Their exit from unwanted pregnancy is legally secured, though practical access varies by jurisdiction.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__restrictive_anthropocentric_reading, pregnant_persons, beneficiary,
    moderate, biographical, constrained, national).

% Advocate for and benefit from the legal framework that treats personhood as beginning at birth. They litigate to maintain the boundary against fetal personhood legislation and judicial erosion. Their organizational capacity gives them voice in courts and legislatures.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__restrictive_anthropocentric_reading, reproductive_autonomy_advocates, beneficiary,
    organized, generational, mobile, national).

% Benefit from a personhood boundary that minimizes state intrusion into private reproductive decisions and avoids expansive regulatory regimes for fetal/ecosystem/AI rights. They view the bright-line rule as a constraint on state power.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__restrictive_anthropocentric_reading, limited_government_advocates, beneficiary,
    organized, generational, mobile, national).

% Bear the cost of the birth-based boundary: their core claim (personhood from conception) is legally excluded. They must pursue constitutional amendment or judicial reversal to change the boundary. Their exit is constrained — they cannot operate within the current framework to achieve their aim.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__restrictive_anthropocentric_reading, fetal_personhood_advocates, payer,
    organized, generational, constrained, national).

% Seek legal personhood for ecosystems and natural entities. The anthropocentric boundary excludes their claims by definition. They must build new legal theories (guardianship, standing) rather than fit within existing personhood doctrine. Exit is constrained by the boundary's definitional force.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__restrictive_anthropocentric_reading, rights_of_nature_advocates, payer,
    moderate, generational, constrained, national).

% Argue that advanced AI systems meeting cognitive capacity thresholds should hold legal personhood. The restrictive reading's species restriction (human-only) blocks this regardless of capacity. They bear the cost of the species barrier and must pursue legislative/regulatory alternatives outside personhood doctrine.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__restrictive_anthropocentric_reading, ai_personhood_advocates, payer,
    moderate, generational, constrained, global).

% Administers and enforces the personhood boundary through courts, legislatures, and agencies. The boundary simplifies adjudication (bright-line at birth) but faces pressure from competing claims. The system could revise the boundary but does so only through deliberate constitutional/judicial process.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__restrictive_anthropocentric_reading, state_legal_system, agenda_setter,
    institutional, generational, arbitrage, national).

% Observes the structural contest over the personhood boundary across readings. Sees the coordinate extraction: the restrictive reading's boundary benefits pregnant persons and limited-government advocates while imposing definitional exclusion costs on fetal-personhood, rights-of-nature, and AI-personhood advocates. The constraint's persistence depends on the state_legal_system's enforcement of the birth/cognition line.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__restrictive_anthropocentric_reading, analytical_observer, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a clear, administrable legal boundary for rights-holding that avoids slippery-slope metaphysical disputes, gives pregnant persons decisive reproductive authority, and limits state power to conscript bodies or expand regulatory reach into novel entities.
% TRANSFER_FUNCTION: Moves the status of rights-bearer from potential/functional entities (fetuses, ecosystems, AI) to born humans with cognitive capacity; moves reproductive decision authority from state to pregnant persons; moves regulatory authority away from fetal-protection, ecosystem-rights, and AI-rights regimes.
% ABSENT_VOICES: Fetuses (cannot speak, structurally excluded by the boundary); ecosystems and non-human animals (no legal voice, excluded by species restriction); future advanced AI systems (do not yet exist, excluded by species+cognition requirement); developmental_potentiality_reading proponents (excluded from this reading's framework, their premise foreclosed by the birth/cognition premise).
% DISAPPEARANCE_RATIONALE: If the birth/cognition boundary vanished overnight, fetal personhood legislation would immediately expand in multiple jurisdictions; rights-of-nature laws would gain doctrinal foothold; AI personhood claims would move from theoretical to justiciable; the state would gain authority to regulate pregnancy as fetal-protection; reproductive autonomy would lose its constitutional anchor.
% FOUNDING_PROBLEM: The need for a clear, non-arbitrary legal boundary for personhood that respects women's bodily autonomy against state compelled-pregnancy power, avoids endless metaphysical litigation over ensoulment/sentience thresholds, and prevents regulatory capture by expanding the circle of rights-bearers beyond administrable limits.
% FOUNDING_PROBLEM_CORROBORATION: Constitutional law scholars (e.g., Laurence Tribe, Reva Siegel) document the autonomy rationale; reproductive rights organizations (Center for Reproductive Rights, ACLU) attest the boundary's functional necessity; liberal legal theorists outside the immediate beneficiary set (e.g., Ronald Dworkin's later work on life's dominion) corroborate the state-neutrality principle. No major legal tradition treats the founding problem as resolved.
narrative_ontology:disappearance_verdict(legal_personhood_boundary__restrictive_anthropocentric_reading, world_rearranges).
narrative_ontology:founding_problem_status(legal_personhood_boundary__restrictive_anthropocentric_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(legal_personhood_boundary__restrictive_anthropocentric_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(legal_personhood_boundary__restrictive_anthropocentric_reading, 'none', 1).
narrative_ontology:epsilon_provenance(legal_personhood_boundary__restrictive_anthropocentric_reading, 0.45, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(legal_personhood_boundary__restrictive_anthropocentric_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(legal_personhood_boundary__restrictive_anthropocentric_reading, ExtMetricName, E),
    domain_priors:suppression_score(legal_personhood_boundary__restrictive_anthropocentric_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(legal_personhood_boundary__restrictive_anthropocentric_reading),
    narrative_ontology:constraint_metric(legal_personhood_boundary__restrictive_anthropocentric_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(legal_personhood_boundary__restrictive_anthropocentric_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(legal_personhood_boundary__restrictive_anthropocentric_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.45) reflects the constraint's active exclusion of entities that competing readings would include as rights-bearers — the boundary is not passively discovered but actively maintained against pressure. Suppression (0.60) captures the legal enforcement required to hold the birth/cognition line against fetal personhood laws, rights-of-nature litigation, and AI personhood proposals. Theater ratio (0.30) acknowledges genuine coordination value (bright-line administrability, autonomy protection) alongside performative maintenance of the boundary as 'natural' when it is contested. Accessibility collapse (0.60) and resistance (0.55) reflect that alternative personhood frameworks remain conceptually available and politically mobilized. The measurement grid shows rising extraction and suppression through the Dobbs era (2022 peak) with recent stabilization.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats and beneficiary/agenda-setter seats compute differently: from the state legal system's position, the boundary is a coordination achievement (bright-line administrability); from the payer seats, the same structure operates as enforced exclusion (their personhood claims are definitionally impossible). The engine computes this divergence from the structural data — the claimed Mountain type does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Pregnant persons and autonomy advocates are structural beneficiaries (d near 0.0): the constraint subsidizes their reproductive authority. Limited-government advocates are secondary beneficiaries (d ~0.2): they gain a constraint on state power. Fetal-personhood, rights-of-nature, and AI-personhood advocates are payers (d near 1.0): they bear the full cost of definitional exclusion — their claims are legally foreclosed, not merely disadvantaged. The state legal system sits near symmetric (d ~0.5): it administers the boundary and could change it, but faces institutional inertia and legitimacy costs. The analytical observer sits at d=0.5 by definition. The engine derives these from beneficiary/payer declarations + power + exit options.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (autonomy + administrability + state neutrality) remains live. The constraint has not atrophied into a piton — it actively structures reproductive law, environmental standing, and emerging AI regulation. However, the rising theater ratio and suppression requirement suggest the coordination function is being stressed by expansion pressures. The constraint is not a snare because the coordination function (autonomy protection, administrable line) is genuine and the beneficiaries are not a narrow extractive coalition. It is a contested Mountain with FSM characteristics.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_boundary_vs_constructed_line,
    'Is the birth/cognition personhood boundary a genuine natural law (biological/metaphysical fact) or a constructed legal line-drawing that benefits identifiable agents?',
    'Cross-cultural/historical legal survey: if the birth boundary appears across diverse legal traditions independently, evidence for naturalness; if it tracks specific autonomy/state-power struggles, evidence for construction. Philosophical analysis of whether cognitive capacity is a binary threshold or a continuum that the law discretizes.',
    'If natural law, the constraint is a genuine Mountain (ε≈0) and FSM does not fire. If constructed, FSM fires and the engine reclassifies toward tangled_rope (coordination + asymmetric extraction from excluded claimants). This is the core ambiguity the false_summit_mountain signature detects.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(natural_boundary_vs_constructed_line, conceptual, 'Whether the personhood boundary is discovered or invented.').

omega_variable(
    cognitive_capacity_threshold_stability,
    'Is the cognitive capacity requirement a stable threshold or will it expand (include advanced AI) or contract (exclude humans with severe cognitive disability)?',
    'Track judicial/legislative treatment of cognitive capacity in personhood-adjacent contexts (guardianship, medical decision-making, AI liability). If capacity becomes a functional test applied across species, the species restriction collapses. If capacity is used to exclude disabled humans, the reading''s autonomy rationale fractures.',
    'If capacity threshold destabilizes, the constraint''s coherence fails — it becomes either functional_capacity_reading (if species restriction drops) or a disability-exclusion regime (if capacity narrows). Either outcome changes the beneficiary/victim structure and extractiveness.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cognitive_capacity_threshold_stability, empirical, 'Stability of the cognitive capacity criterion under technological and medical pressure.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression of alternative personhood claims structural (legal barriers, standing doctrine) or internalized (moral acceptance that fetuses/ecosystems/AI lack standing)?',
    'Post-exit trajectory study: if advocates for excluded entities continue mounting claims despite legal rejection, suppression is primarily structural. If advocacy diminishes and the boundary becomes ''common sense,'' internalization has occurred. Survey data on public/legal elite attitudes over time.',
    'If internalized, effective suppression is higher than legal barriers alone suggest — the constraint has colonized the normative imagination. This would raise the constraint''s persistence score and lower measured resistance without changing legal structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression of competing personhood claims.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(legal_personhood_boundary__restrictive_anthropocentric_reading, 1973, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(lega_tr_t1973, legal_personhood_boundary__restrictive_anthropocentric_reading, theater_ratio, 1973, 0.15).
narrative_ontology:measurement(lega_tr_t1980, legal_personhood_boundary__restrictive_anthropocentric_reading, theater_ratio, 1980, 0.18).
narrative_ontology:measurement(lega_tr_t1992, legal_personhood_boundary__restrictive_anthropocentric_reading, theater_ratio, 1992, 0.22).
narrative_ontology:measurement(lega_tr_t2000, legal_personhood_boundary__restrictive_anthropocentric_reading, theater_ratio, 2000, 0.25).
narrative_ontology:measurement(lega_tr_t2010, legal_personhood_boundary__restrictive_anthropocentric_reading, theater_ratio, 2010, 0.28).
narrative_ontology:measurement(lega_tr_t2022, legal_personhood_boundary__restrictive_anthropocentric_reading, theater_ratio, 2022, 0.32).
narrative_ontology:measurement(lega_tr_t2026, legal_personhood_boundary__restrictive_anthropocentric_reading, theater_ratio, 2026, 0.3).

% Extraction over time
narrative_ontology:measurement(lega_be_t1973, legal_personhood_boundary__restrictive_anthropocentric_reading, base_extractiveness, 1973, 0.35).
narrative_ontology:measurement(lega_be_t1980, legal_personhood_boundary__restrictive_anthropocentric_reading, base_extractiveness, 1980, 0.38).
narrative_ontology:measurement(lega_be_t1992, legal_personhood_boundary__restrictive_anthropocentric_reading, base_extractiveness, 1992, 0.4).
narrative_ontology:measurement(lega_be_t2000, legal_personhood_boundary__restrictive_anthropocentric_reading, base_extractiveness, 2000, 0.42).
narrative_ontology:measurement(lega_be_t2010, legal_personhood_boundary__restrictive_anthropocentric_reading, base_extractiveness, 2010, 0.43).
narrative_ontology:measurement(lega_be_t2022, legal_personhood_boundary__restrictive_anthropocentric_reading, base_extractiveness, 2022, 0.47).
narrative_ontology:measurement(lega_be_t2026, legal_personhood_boundary__restrictive_anthropocentric_reading, base_extractiveness, 2026, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(lega_su_t1973, legal_personhood_boundary__restrictive_anthropocentric_reading, suppression_requirement, 1973, 0.45).
narrative_ontology:measurement(lega_su_t1980, legal_personhood_boundary__restrictive_anthropocentric_reading, suppression_requirement, 1980, 0.5).
narrative_ontology:measurement(lega_su_t1992, legal_personhood_boundary__restrictive_anthropocentric_reading, suppression_requirement, 1992, 0.55).
narrative_ontology:measurement(lega_su_t2000, legal_personhood_boundary__restrictive_anthropocentric_reading, suppression_requirement, 2000, 0.58).
narrative_ontology:measurement(lega_su_t2010, legal_personhood_boundary__restrictive_anthropocentric_reading, suppression_requirement, 2010, 0.6).
narrative_ontology:measurement(lega_su_t2022, legal_personhood_boundary__restrictive_anthropocentric_reading, suppression_requirement, 2022, 0.65).
narrative_ontology:measurement(lega_su_t2026, legal_personhood_boundary__restrictive_anthropocentric_reading, suppression_requirement, 2026, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(legal_personhood_boundary__restrictive_anthropocentric_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(legal_personhood_boundary__restrictive_anthropocentric_reading, 0.08).
narrative_ontology:affects_constraint(legal_personhood_boundary__restrictive_anthropocentric_reading, abortion_access_regime).
narrative_ontology:affects_constraint(legal_personhood_boundary__restrictive_anthropocentric_reading, environmental_standing_doctrine).
narrative_ontology:affects_constraint(legal_personhood_boundary__restrictive_anthropocentric_reading, ai_liability_personhood_framework).

% DUAL FORMULATION NOTE:
% This is the restrictive_anthropocentric_reading of the legal_personhood_boundary kernel. The developmental_potentiality_reading (personhood from conception) and functional_capacity_reading (personhood from cognitive capacity regardless of species) are sibling constraints. This reading forecloses the developmental reading and coexists with the functional reading. All three share the kernel but instantiate different constraints with different ε, beneficiary/victim structures, and classifications.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(legal_personhood_boundary__restrictive_anthropocentric_reading, organized, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
