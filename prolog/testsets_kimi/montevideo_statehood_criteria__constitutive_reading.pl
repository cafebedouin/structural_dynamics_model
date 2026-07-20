% ============================================================================
% CONSTRAINT STORY: montevideo_statehood_criteria__constitutive_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_montevideo_statehood_criteria__constitutive_reading, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: montevideo_statehood_criteria__constitutive_reading
 *   human_readable: Constitutive Theory of State Recognition
 *   domain: international_law/political_philosophy
 *
 * SUMMARY:
 *   The constitutive theory of statehood holds that an entity becomes a state
 *   only when recognized by the existing community of states, regardless of
 *   whether it satisfies objective criteria such as defined territory and
 *   effective government. This reading of the Montevideo criteria kernel
 *   creates a structural veto where established states control entry into the
 *   international legal order. Unrecognized polities that meet the four
 *   criteria are treated as non-states and excluded from treaty participation
 *   and international institutions. The constraint is claimed as coordination
 *   (maintaining diplomatic order and preventing chaos) but structurally
 *   operates as gatekeeping that concentrates power in the hands of existing
 *   states, especially great powers.
 *
 * KEY AGENTS:
 *   - established_states: Agenda-setter and beneficiary (institutional/constrained) â controls recognition decisions and benefits from collective veto over new entrants
 *   - unrecognized_polities: Payer (powerless/trapped) â meets objective statehood criteria but is denied legal personality and treaty access
 *   - great_powers: Beneficiary (powerful/arbitrage) â uses recognition as a geopolitical instrument beyond the neutral application of legal criteria
 *   - international_organizations: Secondary beneficiary (institutional/constrained) â depends on the state-centric recognition framework for membership and function
 *   - self_determination_claimants: Excluded (powerless/trapped) â would contest the veto but are not parties to the recognition conversation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(montevideo_statehood_criteria__constitutive_reading, 0.72).
domain_priors:suppression_score(montevideo_statehood_criteria__constitutive_reading, 0.68).
domain_priors:theater_ratio(montevideo_statehood_criteria__constitutive_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(montevideo_statehood_criteria__constitutive_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(montevideo_statehood_criteria__constitutive_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(montevideo_statehood_criteria__constitutive_reading, theater_ratio, 0.35).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(montevideo_statehood_criteria__constitutive_reading, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(montevideo_statehood_criteria__constitutive_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(montevideo_statehood_criteria__constitutive_reading, tangled_rope).
narrative_ontology:human_readable(montevideo_statehood_criteria__constitutive_reading, "Constitutive Theory of State Recognition").
narrative_ontology:topic_domain(montevideo_statehood_criteria__constitutive_reading, "international_law/political_philosophy").

domain_priors:requires_active_enforcement(montevideo_statehood_criteria__constitutive_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(montevideo_statehood_criteria__constitutive_reading, '7af2090b-596f-4a7a-86b2-d66660fb7f26').
narrative_ontology:cs_kernel_codification('7af2090b-596f-4a7a-86b2-d66660fb7f26', formalized).
narrative_ontology:cs_authority_grounding('7af2090b-596f-4a7a-86b2-d66660fb7f26', practice).
narrative_ontology:cs_interpretation_layer_present('7af2090b-596f-4a7a-86b2-d66660fb7f26').
narrative_ontology:cs_reading_relation('7af2090b-596f-4a7a-86b2-d66660fb7f26', montevideo_statehood_criteria__declaratory_reading, coexists_with).
narrative_ontology:cs_reading_relation('7af2090b-596f-4a7a-86b2-d66660fb7f26', montevideo_statehood_criteria__hybrid_reading, influences).
narrative_ontology:cs_axiom('7af2090b-596f-4a7a-86b2-d66660fb7f26', foundational, recognition_constitutes_statehood).
narrative_ontology:cs_axiom_status(recognition_constitutes_statehood, holdable).
narrative_ontology:cs_axiom_grounding('7af2090b-596f-4a7a-86b2-d66660fb7f26', recognition_constitutes_statehood, conventional).
narrative_ontology:cs_axiom('7af2090b-596f-4a7a-86b2-d66660fb7f26', foundational, existing_states_hold_validity_monopoly).
narrative_ontology:cs_axiom_status(existing_states_hold_validity_monopoly, holdable).
narrative_ontology:cs_axiom_grounding('7af2090b-596f-4a7a-86b2-d66660fb7f26', existing_states_hold_validity_monopoly, conventional).
narrative_ontology:cs_reference_frame('7af2090b-596f-4a7a-86b2-d66660fb7f26', state_community_as_sovereign_gatekeeper).
narrative_ontology:cs_drift_state('7af2090b-596f-4a7a-86b2-d66660fb7f26', post_decolonization_international_law, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('7af2090b-596f-4a7a-86b2-d66660fb7f26', '').
narrative_ontology:cs_kernel_id(montevideo_statehood_criteria__constitutive_reading, montevideo_statehood_criteria).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(montevideo_statehood_criteria__constitutive_reading, established_states).
narrative_ontology:constraint_beneficiary(montevideo_statehood_criteria__constitutive_reading, great_powers).
narrative_ontology:constraint_beneficiary(montevideo_statehood_criteria__constitutive_reading, international_organizations).
narrative_ontology:constraint_victim(montevideo_statehood_criteria__constitutive_reading, unrecognized_polities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% As the existing community of states, they control the diplomatic gate through bilateral and collective recognition decisions. Each member decides whom to recognize, and non-recognized entities are barred from treaty participation and intergovernmental institutions. The arrangement preserves their collective monopoly over defining international personality and prevents unwanted challengers to territorial integrity.
narrative_ontology:constraint_stakeholder(montevideo_statehood_criteria__constitutive_reading, established_states, agenda_setter,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(montevideo_statehood_criteria__constitutive_reading, established_states, beneficiary).

% Control territory, exercise government functions, and meet the four Montevideo criteria, but are denied recognition by one or more existing states. They cannot sign treaties, join the UN, or issue universally accepted passports. Their exit options are limited to seeking patronage from rival states or attempting facts-on-the-ground that force recognition, both high-risk and uncertain.
narrative_ontology:constraint_stakeholder(montevideo_statehood_criteria__constitutive_reading, unrecognized_polities, payer,
    powerless, biographical, trapped, regional).

% Wield disproportionate influence over which new entities gain recognition, using it as leverage to reward alignment and punish defiance. They benefit from a system where strategic interests, not objective criteria alone, determine membership in the international community.
narrative_ontology:constraint_stakeholder(montevideo_statehood_criteria__constitutive_reading, great_powers, beneficiary,
    powerful, generational, arbitrage, global).

% Operate under a state-centric membership structure that depends on recognition to determine participation. They benefit from the clarity of the constitutive rule but do not control it; their institutional existence reinforces the existing states' gatekeeping authority.
narrative_ontology:constraint_stakeholder(montevideo_statehood_criteria__constitutive_reading, international_organizations, beneficiary,
    institutional, generational, constrained, global).

% Are structurally excluded from the recognition framework because self-determination claims are processed only through parent states or liberation movements that must first gain statehood to be heard. They would contest the veto power of existing states but are not parties to the legal conversation.
narrative_ontology:constraint_stakeholder(montevideo_statehood_criteria__constitutive_reading, self_determination_claimants, excluded,
    powerless, generational, trapped, regional).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a managed membership system for the international community, preventing unlimited fragmentation and maintaining diplomatic order by ensuring new states have the approval of existing powers before entering the system.
% TRANSFER_FUNCTION: Transfers sovereignty validation and treaty-access rights from entities that meet objective criteria to the existing community of states, which grants or withholds recognition based on political alignment and strategic interest.
% ABSENT_VOICES: Peoples with claims to self-determination and unrecognized polities that meet objective criteria but lack recognition are structurally excluded from the law-making conversation; they cannot vote on their own admission to the state system.
% DISAPPEARANCE_RATIONALE: If recognition were no longer required, secessionist and de facto states would gain immediate treaty access and institutional standing; the existing states would lose their veto over international membership, and the diplomatic order would reorganize around objective capacity rather than political acceptance.
% FOUNDING_PROBLEM: The emergence of new states in the nineteenth century created instability and conflicting claims; the Great Powers sought to manage state creation through collective recognition to prevent fragmentation and maintain the balance of power.
% FOUNDING_PROBLEM_CORROBORATION: Decolonization-era jurists and post-colonial scholars contest the founding problem's continued relevance, arguing that self-determination supersedes great-power management; no neutral international tribunal has affirmed the constitutive theory as binding positive law independent of state practice.
narrative_ontology:disappearance_verdict(montevideo_statehood_criteria__constitutive_reading, world_rearranges).
narrative_ontology:founding_problem_status(montevideo_statehood_criteria__constitutive_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(montevideo_statehood_criteria__constitutive_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(montevideo_statehood_criteria__constitutive_reading, 'none', 1).
narrative_ontology:epsilon_provenance(montevideo_statehood_criteria__constitutive_reading, 0.72, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(montevideo_statehood_criteria__constitutive_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(montevideo_statehood_criteria__constitutive_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(montevideo_statehood_criteria__constitutive_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.72) because recognition operates as a political veto decoupled from objective state capacity; suppression is substantial (0.68) because the constraint persists through active diplomatic isolation, treaty exclusion, and passport non-recognition rather than through consent of the governed. Theater is moderate (0.35): the coordination function (order, stability) is real, but an increasing share of recognition practice serves great-power competition rather than neutral gatekeeping. Accessibility collapse is high (0.75) because once non-recognition is established, the unrecognized polity has no alternative path to legal personality. Resistance is moderate (0.55) because unrecognized polities and decolonization scholars have continuously contested the framework.
 *
 * PERSPECTIVAL GAP:
 *   From the established-state seat, the constraint is necessary coordination that prevents fragmentation and preserves territorial integrity; from the unrecognized-polity seat, it is arbitrary exclusion by a self-interested club. The engine computes divergent per-seat classifications from this structural asymmetry: the agenda-setter seat experiences a rope-like coordination device, while the trapped payer seat experiences a snare-like extraction mechanism.
 *
 * DIRECTIONALITY LOGIC:
 *   Established states are beneficiaries with constrained exit (bound by reciprocity and community norms), placing them near the beneficiary pole (d ~ 0.15). Great powers share beneficiary status but hold arbitrage-grade exit (strategic recognition policy), placing them even lower (d ~ 0.05). International organizations are secondary beneficiaries with constrained exit (d ~ 0.25). Unrecognized polities are declared victims with trapped exit, placing them near the full-target pole (d ~ 0.95). The engine scales effective extraction accordingly.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is classified as tangled rope rather than snare because a genuine coordination function exists: unlimited, unmanaged state creation would produce fragmentation and conflict. It is not a rope because the extraction is asymmetric and enforcement is active, political, and interest-driven rather than neutral. It is not a mountain because it is clearly constructed by state practice, not a natural law. It is not a scaffold because it carries no sunset clause, and not a piton because the gatekeeping function has not atrophied; it remains actively wielded by great powers.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    constitutive_theory_empirical_status,
    'Does contemporary international practice treat recognition as constitutive of statehood, or has the declaratory theory become the operative legal framework?',
    'Systematic analysis of treaty accession patterns, UN admission practice, and judicial opinions (e.g., ICJ Kosovo advisory opinion) to determine whether objective criteria or recognition is the primary gateway.',
    'If declarative practice dominates, the constitutive reading is a false summit or atavistic holdover, and extraction is higher than coordination; if constitutive practice remains live, the tangled-rope classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(constitutive_theory_empirical_status, empirical, 'Whether recognition remains constitutive in actual state practice').

omega_variable(
    recognition_neutrality_ambiguity,
    'Is the recognition process structurally neutral with respect to power politics, or does it systematically favor allies of existing great powers?',
    'Quantitative analysis of recognition patterns against geopolitical alignment indices and case studies of recognition timing (e.g., Kosovo, Palestine, Western Sahara).',
    'Systematic bias would confirm the extraction component; neutrality would support the coordination component.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(recognition_neutrality_ambiguity, empirical, 'Whether recognition politics is neutral or systematically biased').

omega_variable(
    kernel_reading_competition,
    'Does the Montevideo kernel admit a single coherent reading, or is it irreducibly contested among constitutive, declaratory, and hybrid interpretations?',
    'Historical-legal analysis of the negotiation record and subsequent state practice to determine if the criteria were designed to be ambiguous or if one reading was originally intended as canonical.',
    'If irreducibly contested, the kernel is distributed and no single reading can claim canonical status; if originally constitutive, the drift toward declarative theory represents authority erosion.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_competition, conceptual, 'Whether the Montevideo kernel is irreducibly contested').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(montevideo_statehood_criteria__constitutive_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(montevideo_constitutive_tr_t0, montevideo_statehood_criteria__constitutive_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(montevideo_constitutive_tr_t20, montevideo_statehood_criteria__constitutive_reading, theater_ratio, 20, 0.22).
narrative_ontology:measurement(montevideo_constitutive_tr_t40, montevideo_statehood_criteria__constitutive_reading, theater_ratio, 40, 0.28).
narrative_ontology:measurement(montevideo_constitutive_tr_t60, montevideo_statehood_criteria__constitutive_reading, theater_ratio, 60, 0.25).
narrative_ontology:measurement(montevideo_constitutive_tr_t80, montevideo_statehood_criteria__constitutive_reading, theater_ratio, 80, 0.3).
narrative_ontology:measurement(montevideo_constitutive_tr_t100, montevideo_statehood_criteria__constitutive_reading, theater_ratio, 100, 0.35).

% Extraction over time
narrative_ontology:measurement(montevideo_constitutive_be_t0, montevideo_statehood_criteria__constitutive_reading, base_extractiveness, 0, 0.65).
narrative_ontology:measurement(montevideo_constitutive_be_t20, montevideo_statehood_criteria__constitutive_reading, base_extractiveness, 20, 0.6).
narrative_ontology:measurement(montevideo_constitutive_be_t40, montevideo_statehood_criteria__constitutive_reading, base_extractiveness, 40, 0.55).
narrative_ontology:measurement(montevideo_constitutive_be_t60, montevideo_statehood_criteria__constitutive_reading, base_extractiveness, 60, 0.5).
narrative_ontology:measurement(montevideo_constitutive_be_t80, montevideo_statehood_criteria__constitutive_reading, base_extractiveness, 80, 0.62).
narrative_ontology:measurement(montevideo_constitutive_be_t100, montevideo_statehood_criteria__constitutive_reading, base_extractiveness, 100, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(montevideo_constitutive_su_t0, montevideo_statehood_criteria__constitutive_reading, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(montevideo_constitutive_su_t20, montevideo_statehood_criteria__constitutive_reading, suppression_requirement, 20, 0.55).
narrative_ontology:measurement(montevideo_constitutive_su_t40, montevideo_statehood_criteria__constitutive_reading, suppression_requirement, 40, 0.48).
narrative_ontology:measurement(montevideo_constitutive_su_t60, montevideo_statehood_criteria__constitutive_reading, suppression_requirement, 60, 0.45).
narrative_ontology:measurement(montevideo_constitutive_su_t80, montevideo_statehood_criteria__constitutive_reading, suppression_requirement, 80, 0.58).
narrative_ontology:measurement(montevideo_constitutive_su_t100, montevideo_statehood_criteria__constitutive_reading, suppression_requirement, 100, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(montevideo_statehood_criteria__constitutive_reading, montevideo_statehood_criteria__declaratory_reading).
narrative_ontology:affects_constraint(montevideo_statehood_criteria__constitutive_reading, montevideo_statehood_criteria__hybrid_reading).

% DUAL FORMULATION NOTE:
% The Montevideo statehood criteria kernel decomposes into three structurally distinct constraints. The constitutive reading treats recognition as a power-granting act by existing states; the declaratory reading treats the four criteria as self-executing; the hybrid reading adds normative legitimacy conditions. Each has distinct beneficiary/victim structures and epsilon values.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
