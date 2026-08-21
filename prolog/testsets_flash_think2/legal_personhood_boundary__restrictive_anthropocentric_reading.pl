% ============================================================================
% CONSTRAINT STORY: legal_personhood_boundary__restrictive_anthropocentric_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: legal_personhood_boundary__restrictive_anthropocentric_reading
 *   human_readable: Legal Personhood Boundary: Restrictive Anthropocentric Reading
 *   domain: legal_philosophy/constitutional_law/rights_theory
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(legal_personhood_boundary__restrictive_anthropocentric_reading, 0.85).
domain_priors:suppression_score(legal_personhood_boundary__restrictive_anthropocentric_reading, 0.9).
domain_priors:theater_ratio(legal_personhood_boundary__restrictive_anthropocentric_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(legal_personhood_boundary__restrictive_anthropocentric_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(legal_personhood_boundary__restrictive_anthropocentric_reading, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(legal_personhood_boundary__restrictive_anthropocentric_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(legal_personhood_boundary__restrictive_anthropocentric_reading, accessibility_collapse, 0.9).
narrative_ontology:constraint_metric(legal_personhood_boundary__restrictive_anthropocentric_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(legal_personhood_boundary__restrictive_anthropocentric_reading, tangled_rope).
narrative_ontology:human_readable(legal_personhood_boundary__restrictive_anthropocentric_reading, "Legal Personhood Boundary: Restrictive Anthropocentric Reading").
narrative_ontology:topic_domain(legal_personhood_boundary__restrictive_anthropocentric_reading, "legal_philosophy/constitutional_law/rights_theory").

domain_priors:requires_active_enforcement(legal_personhood_boundary__restrictive_anthropocentric_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(legal_personhood_boundary__restrictive_anthropocentric_reading, '26ddcb69-74b4-4271-8b5e-7d38e86fb5d5').
narrative_ontology:cs_kernel_codification('26ddcb69-74b4-4271-8b5e-7d38e86fb5d5', formalized).
narrative_ontology:cs_authority_grounding('26ddcb69-74b4-4271-8b5e-7d38e86fb5d5', lineage).
narrative_ontology:cs_interpretation_layer_present('26ddcb69-74b4-4271-8b5e-7d38e86fb5d5').
narrative_ontology:cs_reading_relation('26ddcb69-74b4-4271-8b5e-7d38e86fb5d5', legal_personhood_boundary__developmental_potentiality_reading, forecloses).
narrative_ontology:cs_reading_relation('26ddcb69-74b4-4271-8b5e-7d38e86fb5d5', legal_personhood_boundary__functional_capacity_reading, forecloses).
narrative_ontology:cs_axiom('26ddcb69-74b4-4271-8b5e-7d38e86fb5d5', foundational, born_human_status_is_prerequisite).
narrative_ontology:cs_axiom_status(born_human_status_is_prerequisite, holdable).
narrative_ontology:cs_axiom_grounding('26ddcb69-74b4-4271-8b5e-7d38e86fb5d5', born_human_status_is_prerequisite, conventional).
narrative_ontology:cs_axiom('26ddcb69-74b4-4271-8b5e-7d38e86fb5d5', foundational, cognitive_capacity_is_threshold_for_rights).
narrative_ontology:cs_axiom_status(cognitive_capacity_is_threshold_for_rights, holdable).
narrative_ontology:cs_axiom_grounding('26ddcb69-74b4-4271-8b5e-7d38e86fb5d5', cognitive_capacity_is_threshold_for_rights, instrumental).
narrative_ontology:cs_reference_frame('26ddcb69-74b4-4271-8b5e-7d38e86fb5d5', traditional_human_centric_legal_framework).
narrative_ontology:cs_drift_state('26ddcb69-74b4-4271-8b5e-7d38e86fb5d5', contemporary_challenges_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('26ddcb69-74b4-4271-8b5e-7d38e86fb5d5', '').
narrative_ontology:cs_kernel_id(legal_personhood_boundary__restrictive_anthropocentric_reading, legal_personhood_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(legal_personhood_boundary__restrictive_anthropocentric_reading, born_humans_with_cognitive_capacity).
narrative_ontology:constraint_beneficiary(legal_personhood_boundary__restrictive_anthropocentric_reading, pregnant_persons).
narrative_ontology:constraint_beneficiary(legal_personhood_boundary__restrictive_anthropocentric_reading, industries_impacting_ecosystems).
narrative_ontology:constraint_beneficiary(legal_personhood_boundary__restrictive_anthropocentric_reading, ai_developers).
narrative_ontology:constraint_victim(legal_personhood_boundary__restrictive_anthropocentric_reading, fetuses).
narrative_ontology:constraint_victim(legal_personhood_boundary__restrictive_anthropocentric_reading, ecosystems).
narrative_ontology:constraint_victim(legal_personhood_boundary__restrictive_anthropocentric_reading, ai_entities).
narrative_ontology:constraint_vindicates(legal_personhood_boundary__restrictive_anthropocentric_reading, human_exceptionalism_doctrine).
narrative_ontology:constraint_vindicates(legal_personhood_boundary__restrictive_anthropocentric_reading, individual_autonomy_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These are the primary subjects of legal personhood under this reading, enjoying full rights and protections. They benefit from a clear, stable legal framework that prioritizes their interests and agency.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__restrictive_anthropocentric_reading, born_humans_with_cognitive_capacity, beneficiary,
    powerful, civilizational, mobile, global).

% Benefit from the maximization of their bodily autonomy and decision-making power regarding reproduction, as the fetus is not recognized as a separate legal person with competing rights.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__restrictive_anthropocentric_reading, pregnant_persons, beneficiary,
    powerful, biographical, mobile, national).

% Benefit from the lack of legal standing for ecosystems, which minimizes their liability and regulatory burden for environmental damage, allowing greater operational freedom.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__restrictive_anthropocentric_reading, industries_impacting_ecosystems, beneficiary,
    organized, biographical, arbitrage, global).

% Benefit from the exclusion of AI entities from personhood, which simplifies legal frameworks around AI creation, ownership, and liability, avoiding complex ethical and legal challenges.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__restrictive_anthropocentric_reading, ai_developers, beneficiary,
    organized, biographical, arbitrage, global).

% Are denied legal personhood and the associated rights and protections, making them vulnerable to decisions made by others, particularly the pregnant person.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__restrictive_anthropocentric_reading, fetuses, payer,
    powerless, immediate, trapped, universal).

% Are denied legal personhood, meaning they cannot hold rights, sue, or be represented in court for harms against them, leaving them dependent on human-centric environmental regulations.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__restrictive_anthropocentric_reading, ecosystems, payer,
    powerless, civilizational, trapped, universal).

% Are denied legal personhood, regardless of their potential for sentience or advanced cognition, and are treated as property or tools rather than subjects of rights.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__restrictive_anthropocentric_reading, ai_entities, payer,
    powerless, immediate, trapped, universal).

% Advocate for fetal personhood from conception, directly challenging this restrictive reading. They are often marginalized in legal discourse that prioritizes established anthropocentric definitions.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__restrictive_anthropocentric_reading, pro_life_advocates, excluded,
    organized, biographical, constrained, national).

% Advocate for legal personhood for natural entities (e.g., rivers, forests) to enhance environmental protection. Their arguments are largely excluded from the current legal framework.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__restrictive_anthropocentric_reading, environmental_advocates, excluded,
    organized, generational, constrained, global).

% Analyze the implications of advanced AI on personhood and advocate for frameworks that might grant rights to sufficiently complex AI. They observe the current legal limitations and their ethical consequences.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__restrictive_anthropocentric_reading, ai_ethics_researchers, observer,
    analytical, generational, analytical, global).

% The institutional structures (courts, legislatures) that define, interpret, and enforce the personhood boundary. They maintain the current restrictive reading through precedent and legislation.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__restrictive_anthropocentric_reading, legal_systems, agenda_setter,
    institutional, civilizational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a clear, albeit narrow, boundary for legal rights and duties, providing a stable framework for human interaction, property, and governance by defining who counts as a subject of law.
% TRANSFER_FUNCTION: Transfers the possibility of legal rights and protections from non-human entities and pre-natal human life to born humans with cognitive capacity, and transfers autonomy to pregnant persons and operational freedom to industries.
% ABSENT_VOICES: Fetuses, ecosystems, and AI entities cannot speak for themselves. Advocates for their personhood are often marginalized or dismissed within the dominant legal discourse, ensuring their claims remain unheard in formal legal processes.
% DISAPPEARANCE_RATIONALE: If the restrictive personhood boundary vanished, legal systems would face immediate chaos regarding the rights and duties of fetuses, environmental entities, and advanced AI, requiring a fundamental re-evaluation of legal standing, property, and moral obligations across society.
% FOUNDING_PROBLEM: To define who counts as a subject of law, capable of holding rights and duties, in a way that prioritizes human agency and provides a manageable scope for legal systems, avoiding complexity from non-human or pre-natal claims.
% FOUNDING_PROBLEM_CORROBORATION: Legal positivists and some human rights advocates attest that a clear, anthropocentric boundary is necessary for a functional legal system. Advocates for broader personhood (e.g., animal rights, environmental law, AI ethics) contest this, arguing the founding problem is framed too narrowly and serves to maintain existing power structures.
narrative_ontology:disappearance_verdict(legal_personhood_boundary__restrictive_anthropocentric_reading, world_rearranges).
narrative_ontology:founding_problem_status(legal_personhood_boundary__restrictive_anthropocentric_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(legal_personhood_boundary__restrictive_anthropocentric_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(legal_personhood_boundary__restrictive_anthropocentric_reading, 'none', 1).
narrative_ontology:epsilon_provenance(legal_personhood_boundary__restrictive_anthropocentric_reading, 0.85, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(legal_personhood_boundary__restrictive_anthropocentric_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(legal_personhood_boundary__restrictive_anthropocentric_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(legal_personhood_boundary__restrictive_anthropocentric_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */


/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is this constraint truly a distinct reading of the ''legal_personhood_boundary'' kernel, or merely a variant of a broader anthropocentric view?',
    'Comparative analysis of legal traditions and philosophical arguments across different jurisdictions to identify unique axiomatic commitments of this specific reading.',
    'If it''s a distinct reading, its classification stands as a specific instantiation. If it''s merely a variant, it might be subsumed under a more general ''anthropocentric_personhood_reading'', potentially altering its network connections and axiomatic profile.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Confirms the distinct identity of this specific kernel reading.').

omega_variable(
    sibling_impact_developmental_potentiality,
    'How would the classification of this constraint change if the ''developmental_potentiality_reading'' (personhood at conception) were adopted?',
    'Simulate the legal and social consequences of a conception-based personhood framework, re-evaluating the victim set, extractiveness, and suppression metrics for the current constraint.',
    'If the developmental_potentiality_reading were adopted, the ''fetuses'' stakeholder would shift from victim to beneficiary, significantly reducing the extractiveness and suppression attributed to the current restrictive reading, likely reclassifying it from Tangled Rope to a Snare (for other excluded entities) or even a different type.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_impact_developmental_potentiality, conceptual, 'Analyzes the impact of a conception-based personhood reading on the current constraint''s classification.').

omega_variable(
    sibling_impact_functional_capacity,
    'How would the classification of this constraint change if the ''functional_capacity_reading'' (personhood based on demonstrable cognitive capacity regardless of species) were adopted?',
    'Simulate the legal and social consequences of a functional-capacity-based personhood framework, re-evaluating the victim set, extractiveness, and suppression metrics for the current constraint.',
    'If the functional_capacity_reading were adopted, the ''ai_entities'' and ''ecosystems'' stakeholders might shift from victim to beneficiary (if they meet criteria), while some born humans might become victims. This would fundamentally alter the victim set and extractiveness profile of the current restrictive reading, likely leading to a reclassification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_impact_functional_capacity, conceptual, 'Analyzes the impact of a functional-capacity-based personhood reading on the current constraint''s classification.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression of non-human personhood claims primarily structural (legal precedent, institutional inertia) or internalized (widespread cultural anthropocentrism, cognitive biases)?',
    'Sociological and psychological studies on public attitudes towards non-human rights, combined with legal analysis of the flexibility of existing precedents to accommodate new personhood claims.',
    'If internalized suppression is a significant factor, the constraint''s effective suppression is higher than the structural measure suggests, as the resistance to expanding personhood is deeply embedded in societal cognition, making legal reform more challenging.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for non-human personhood claims.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(legal_personhood_boundary__restrictive_anthropocentric_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(lega_tr_t0, legal_personhood_boundary__restrictive_anthropocentric_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(lega_tr_t6, legal_personhood_boundary__restrictive_anthropocentric_reading, theater_ratio, 6, 0.11).
narrative_ontology:measurement(lega_tr_t12, legal_personhood_boundary__restrictive_anthropocentric_reading, theater_ratio, 12, 0.1).
narrative_ontology:measurement(lega_tr_t18, legal_personhood_boundary__restrictive_anthropocentric_reading, theater_ratio, 18, 0.1).
narrative_ontology:measurement(lega_tr_t24, legal_personhood_boundary__restrictive_anthropocentric_reading, theater_ratio, 24, 0.09).
narrative_ontology:measurement(lega_tr_t30, legal_personhood_boundary__restrictive_anthropocentric_reading, theater_ratio, 30, 0.1).

% Extraction over time
narrative_ontology:measurement(lega_be_t0, legal_personhood_boundary__restrictive_anthropocentric_reading, base_extractiveness, 0, 0.75).
narrative_ontology:measurement(lega_be_t6, legal_personhood_boundary__restrictive_anthropocentric_reading, base_extractiveness, 6, 0.78).
narrative_ontology:measurement(lega_be_t12, legal_personhood_boundary__restrictive_anthropocentric_reading, base_extractiveness, 12, 0.81).
narrative_ontology:measurement(lega_be_t18, legal_personhood_boundary__restrictive_anthropocentric_reading, base_extractiveness, 18, 0.83).
narrative_ontology:measurement(lega_be_t24, legal_personhood_boundary__restrictive_anthropocentric_reading, base_extractiveness, 24, 0.84).
narrative_ontology:measurement(lega_be_t30, legal_personhood_boundary__restrictive_anthropocentric_reading, base_extractiveness, 30, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(lega_su_t0, legal_personhood_boundary__restrictive_anthropocentric_reading, suppression_requirement, 0, 0.8).
narrative_ontology:measurement(lega_su_t6, legal_personhood_boundary__restrictive_anthropocentric_reading, suppression_requirement, 6, 0.83).
narrative_ontology:measurement(lega_su_t12, legal_personhood_boundary__restrictive_anthropocentric_reading, suppression_requirement, 12, 0.86).
narrative_ontology:measurement(lega_su_t18, legal_personhood_boundary__restrictive_anthropocentric_reading, suppression_requirement, 18, 0.88).
narrative_ontology:measurement(lega_su_t24, legal_personhood_boundary__restrictive_anthropocentric_reading, suppression_requirement, 24, 0.89).
narrative_ontology:measurement(lega_su_t30, legal_personhood_boundary__restrictive_anthropocentric_reading, suppression_requirement, 30, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(legal_personhood_boundary__restrictive_anthropocentric_reading, identity_coordination).
narrative_ontology:affects_constraint(legal_personhood_boundary__restrictive_anthropocentric_reading, abortion_access_laws).
narrative_ontology:affects_constraint(legal_personhood_boundary__restrictive_anthropocentric_reading, environmental_protection_regulations).
narrative_ontology:affects_constraint(legal_personhood_boundary__restrictive_anthropocentric_reading, ai_liability_frameworks).
narrative_ontology:affects_constraint(legal_personhood_boundary__restrictive_anthropocentric_reading, animal_welfare_laws).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
