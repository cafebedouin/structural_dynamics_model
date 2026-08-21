% ============================================================================
% CONSTRAINT STORY: border_control_legitimacy__sovereignty_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_border_control_legitimacy__sovereignty_primary, []).

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
 *   constraint_id: border_control_legitimacy__sovereignty_primary
 *   human_readable: Absolute State Discretion in Border Control (Sovereignty Primary Reading)
 *   domain: political_philosophy/international_law/migration_studies
 *
 * SUMMARY:
 *   This constraint story instantiates the 'sovereignty_primary' reading of
 *   the 'border_control_legitimacy' kernel. It posits that state territorial
 *   sovereignty inherently grants absolute discretion to exclude
 *   non-citizens, making border control a constitutive element of statehood
 *   itself. This reading is often invoked by states to justify stringent
 *   immigration policies and to resist international human rights challenges.
 *   While claimed as a fundamental, almost natural law (Mountain), its
 *   operation involves high extraction and active suppression of
 *   non-citizens, suggesting a functional divergence from its claimed type.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(border_control_legitimacy__sovereignty_primary, 0.85).
domain_priors:suppression_score(border_control_legitimacy__sovereignty_primary, 0.92).
domain_priors:theater_ratio(border_control_legitimacy__sovereignty_primary, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(border_control_legitimacy__sovereignty_primary, extractiveness, 0.85).
narrative_ontology:constraint_metric(border_control_legitimacy__sovereignty_primary, suppression_requirement, 0.92).
narrative_ontology:constraint_metric(border_control_legitimacy__sovereignty_primary, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(border_control_legitimacy__sovereignty_primary, accessibility_collapse, 0.9).
narrative_ontology:constraint_metric(border_control_legitimacy__sovereignty_primary, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(border_control_legitimacy__sovereignty_primary, mountain).
narrative_ontology:human_readable(border_control_legitimacy__sovereignty_primary, "Absolute State Discretion in Border Control (Sovereignty Primary Reading)").
narrative_ontology:topic_domain(border_control_legitimacy__sovereignty_primary, "political_philosophy/international_law/migration_studies").

domain_priors:requires_active_enforcement(border_control_legitimacy__sovereignty_primary).
domain_priors:emerges_naturally(border_control_legitimacy__sovereignty_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(border_control_legitimacy__sovereignty_primary, 'd6f9415b-2778-48bd-a00b-1a8f94ee325c').
narrative_ontology:cs_kernel_codification('d6f9415b-2778-48bd-a00b-1a8f94ee325c', formalized).
narrative_ontology:cs_authority_grounding('d6f9415b-2778-48bd-a00b-1a8f94ee325c', lineage).
narrative_ontology:cs_interpretation_layer_present('d6f9415b-2778-48bd-a00b-1a8f94ee325c').
narrative_ontology:cs_reading_relation('d6f9415b-2778-48bd-a00b-1a8f94ee325c', border_control_legitimacy__freedom_of_movement_primary, forecloses).
narrative_ontology:cs_reading_relation('d6f9415b-2778-48bd-a00b-1a8f94ee325c', border_control_legitimacy__jurisdictional_sovereignty, forecloses).
narrative_ontology:cs_axiom('d6f9415b-2778-48bd-a00b-1a8f94ee325c', foundational, state_territorial_integrity_absolute).
narrative_ontology:cs_axiom_status(state_territorial_integrity_absolute, holdable).
narrative_ontology:cs_axiom_grounding('d6f9415b-2778-48bd-a00b-1a8f94ee325c', state_territorial_integrity_absolute, deontological).
narrative_ontology:cs_axiom('d6f9415b-2778-48bd-a00b-1a8f94ee325c', foundational, non_citizen_exclusion_absolute_discretion).
narrative_ontology:cs_axiom_status(non_citizen_exclusion_absolute_discretion, holdable).
narrative_ontology:cs_axiom_grounding('d6f9415b-2778-48bd-a00b-1a8f94ee325c', non_citizen_exclusion_absolute_discretion, conventional).
narrative_ontology:cs_reference_frame('d6f9415b-2778-48bd-a00b-1a8f94ee325c', westphalian_sovereignty_model).
narrative_ontology:cs_drift_state('d6f9415b-2778-48bd-a00b-1a8f94ee325c', contemporary_human_rights_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('d6f9415b-2778-48bd-a00b-1a8f94ee325c', '').
narrative_ontology:cs_kernel_id(border_control_legitimacy__sovereignty_primary, border_control_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(border_control_legitimacy__sovereignty_primary, sovereign_states).
narrative_ontology:constraint_beneficiary(border_control_legitimacy__sovereignty_primary, citizen_populations).
narrative_ontology:constraint_victim(border_control_legitimacy__sovereignty_primary, non_citizen_migrants).
narrative_ontology:constraint_victim(border_control_legitimacy__sovereignty_primary, refugees).
narrative_ontology:constraint_victim(border_control_legitimacy__sovereignty_primary, asylum_seekers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Assert and enforce the right to control their borders as a fundamental aspect of their sovereignty. They define who may enter and under what conditions, justifying this as essential for national security, economic stability, and cultural preservation. They benefit from the perceived stability and control this provides.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__sovereignty_primary, sovereign_states, agenda_setter,
    institutional, generational, arbitrage, global).

% Benefit from the perceived security, resource allocation, and cultural cohesion that border control is claimed to provide. They often support policies that prioritize national interests and limit immigration, viewing non-citizens as potential competitors for resources or threats to social order.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__sovereignty_primary, citizen_populations, beneficiary,
    organized, biographical, constrained, national).

% Are the primary targets of exclusion, facing legal barriers, physical dangers, and often violence at borders. Their freedom of movement, economic opportunities, and personal safety are directly curtailed by the assertion of absolute state discretion. They are often denied legal recourse or a voice in the policies that govern their lives.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__sovereignty_primary, non_citizen_migrants, payer,
    powerless, immediate, trapped, global).

% Fleeing persecution or conflict, they face severe restrictions on entry, often being denied asylum or subjected to detention. The claim of absolute state discretion directly conflicts with international protection obligations, leaving them in precarious situations with few safe and legal pathways.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__sovereignty_primary, refugees, payer,
    powerless, immediate, trapped, global).

% Seeking protection in another country, they are often met with skepticism, legal hurdles, and physical barriers. The assertion of absolute state discretion can lead to refoulement (forced return) or prolonged detention, undermining their right to seek asylum.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__sovereignty_primary, asylum_seekers, payer,
    powerless, immediate, trapped, global).

% Are tasked with implementing and enforcing border control policies. They operate with significant discretion and resources, often employing surveillance, detention, and physical force to prevent unauthorized entry. Their mandate is directly derived from the state's claim of absolute sovereignty.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__sovereignty_primary, border_enforcement_agencies, agenda_setter,
    institutional, biographical, constrained, national).

% Monitor and challenge state border practices, arguing that human rights obligations limit state discretion. They document abuses, advocate for legal reforms, and provide support to migrants and refugees, often clashing with state authorities over the interpretation of international law.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__sovereignty_primary, human_rights_advocates, observer,
    organized, generational, constrained, global).

% Interpret and apply international conventions related to human rights and refugee protection. While acknowledging state sovereignty, they seek to define its limits, particularly concerning non-refoulement and the right to seek asylum. Their pronouncements often conflict with states' claims of absolute discretion.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__sovereignty_primary, international_law_bodies, observer,
    institutional, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes clear boundaries for state authority and membership, allowing states to manage internal affairs, resources, and social cohesion for their citizens without external interference.
% TRANSFER_FUNCTION: Transfers control over territory, resources, and social cohesion to the citizen population and the state, by denying access, opportunity, and sometimes basic rights to non-citizens.
% ABSENT_VOICES: Non-citizen migrants, refugees, and asylum seekers are largely excluded from the political processes and legal forums where border control policies are formulated and justified. Their perspectives are often marginalized or actively suppressed.
% DISAPPEARANCE_RATIONALE: If the claim of absolute state discretion in border control vanished overnight, global migration patterns would shift dramatically, states would need to fundamentally redefine their territorial integrity and relationship with non-citizens, and international governance would undergo a profound transformation towards more open and rights-based mobility regimes.
% FOUNDING_PROBLEM: The historical need for political entities to define their territorial integrity, manage their populations, and secure their resources in a world of competing political units, particularly after the rise of the modern nation-state.
% FOUNDING_PROBLEM_CORROBORATION: Sovereign states and many citizen populations corroborate that the founding problem of defining and securing statehood remains live. However, human rights advocates and some international legal scholars contest the *absolute* nature of the discretion claimed, arguing that the problem can be solved within a framework that respects human rights and international obligations; this contestation is supported by independent legal analysis and historical scholarship.
narrative_ontology:disappearance_verdict(border_control_legitimacy__sovereignty_primary, world_rearranges).
narrative_ontology:founding_problem_status(border_control_legitimacy__sovereignty_primary, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(border_control_legitimacy__sovereignty_primary, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(border_control_legitimacy__sovereignty_primary, 'none', 1).
narrative_ontology:epsilon_provenance(border_control_legitimacy__sovereignty_primary, 0.85, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(border_control_legitimacy__sovereignty_primary_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(border_control_legitimacy__sovereignty_primary, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(border_control_legitimacy__sovereignty_primary, ExtMetricName, E),
    domain_priors:suppression_score(border_control_legitimacy__sovereignty_primary, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(border_control_legitimacy__sovereignty_primary),
    narrative_ontology:constraint_metric(border_control_legitimacy__sovereignty_primary, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(border_control_legitimacy__sovereignty_primary, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(border_control_legitimacy__sovereignty_primary_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The high extractiveness (0.85) reflects the severe costs imposed on non-citizens, including denial of entry, loss of opportunity, and risk to life and limb. Suppression (0.92) is extremely high due to the militarization of borders, legal penalties, and the active enforcement mechanisms employed by states. Theater ratio is low (0.20) because the core function of exclusion is very real and actively pursued, not merely performative. Accessibility collapse is high (0.90) for non-citizens, as legal alternatives to entry are severely restricted. Resistance (0.70) is substantial, coming from migrants themselves, human rights organizations, and some international bodies. The claimed type is 'mountain' to reflect the proponents' assertion of its natural, unchangeable status, which will trigger False Summit Mountain detection given the high extraction and suppression.
 *
 * PERSPECTIVAL GAP:
 *   Proponents of this reading (sovereign states, citizen populations) perceive it as a fundamental, non-extractive aspect of statehood. Targets (migrants, refugees) experience it as a highly extractive and suppressive force. The engine's classification will highlight this divergence, showing a claimed Mountain operating as a Snare or Tangled Rope from the perspective of those it governs.
 *
 * DIRECTIONALITY LOGIC:
 *   Sovereign states and their citizen populations are the primary beneficiaries, gaining perceived control over territory and resources (low directionality). Non-citizen migrants, refugees, and asylum seekers are the clear targets, bearing the full costs of exclusion (high directionality). Border enforcement agencies act as agenda-setters, implementing the constraint. Human rights advocates and international law bodies serve as observers, challenging the constraint's absolute claims.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification prevents mislabeling a highly extractive and suppressive mechanism as a benign 'natural law.' By claiming 'mountain' but documenting high extraction and suppression, the framework identifies a 'false summit' – a constructed constraint presented as an immutable feature of reality. The persistence of the constraint is not due to its inherent naturalness, but to active enforcement and the benefits it provides to states and their citizens, despite the severe costs to non-citizens.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_constructed_claim,
    'Is the absolute discretion of states in border control a genuine natural law of statehood, or a constructed legal and political claim that benefits identifiable agents?',
    'Comparative historical analysis of state formation and international legal evolution, examining periods where border control was less absolute or differently conceived. Philosophical inquiry into the inherent nature of political communities.',
    'If genuinely natural, the constraint''s extractiveness would be an unavoidable cost of political organization. If constructed, its high extractiveness and suppression would be evidence of a Snare or Tangled Rope, despite its ''mountain'' claim.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_claim, conceptual, 'Ambiguity regarding the naturalness of absolute state border discretion.').

omega_variable(
    scope_of_discretion_vs_human_rights,
    'Is ''absolute discretion'' truly absolute, or are there inherent human rights and international law limits that constrain state action at borders?',
    'Adjudication by international courts, consistent state practice reflecting such limits, or a shift in the foundational axioms of international law.',
    'If limits are recognized as inherent, the constraint''s effective suppression and extractiveness would be reduced, and its classification would shift away from a pure Snare towards a more constrained Tangled Rope or even a Rope, reflecting a genuine coordination function with defined boundaries.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(scope_of_discretion_vs_human_rights, empirical, 'Whether human rights obligations inherently limit state border discretion.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression of non-citizen migrants primarily structural (external barriers, legal penalties) or internalized (fear, hopelessness, identity fusion with ''undocumented'' status)?',
    'Post-exit suppression trajectory: if suppression persists (e.g., through fear of return, inability to integrate) after the immediate physical/legal barriers are removed, it indicates a partially internalized component. Qualitative studies of migrant experiences.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests, as targets carry the suppression with them even after crossing a border or gaining temporary status. This would amplify the Snare-like qualities.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for non-citizen migrants.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(border_control_legitimacy__sovereignty_primary, 1945, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bord_tr_t1945, border_control_legitimacy__sovereignty_primary, theater_ratio, 1945, 0.15).
narrative_ontology:measurement(bord_tr_t1965, border_control_legitimacy__sovereignty_primary, theater_ratio, 1965, 0.16).
narrative_ontology:measurement(bord_tr_t1985, border_control_legitimacy__sovereignty_primary, theater_ratio, 1985, 0.18).
narrative_ontology:measurement(bord_tr_t2005, border_control_legitimacy__sovereignty_primary, theater_ratio, 2005, 0.19).
narrative_ontology:measurement(bord_tr_t2024, border_control_legitimacy__sovereignty_primary, theater_ratio, 2024, 0.2).

% Extraction over time
narrative_ontology:measurement(bord_be_t1945, border_control_legitimacy__sovereignty_primary, base_extractiveness, 1945, 0.7).
narrative_ontology:measurement(bord_be_t1965, border_control_legitimacy__sovereignty_primary, base_extractiveness, 1965, 0.75).
narrative_ontology:measurement(bord_be_t1985, border_control_legitimacy__sovereignty_primary, base_extractiveness, 1985, 0.8).
narrative_ontology:measurement(bord_be_t2005, border_control_legitimacy__sovereignty_primary, base_extractiveness, 2005, 0.83).
narrative_ontology:measurement(bord_be_t2024, border_control_legitimacy__sovereignty_primary, base_extractiveness, 2024, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(bord_su_t1945, border_control_legitimacy__sovereignty_primary, suppression_requirement, 1945, 0.75).
narrative_ontology:measurement(bord_su_t1965, border_control_legitimacy__sovereignty_primary, suppression_requirement, 1965, 0.8).
narrative_ontology:measurement(bord_su_t1985, border_control_legitimacy__sovereignty_primary, suppression_requirement, 1985, 0.85).
narrative_ontology:measurement(bord_su_t2005, border_control_legitimacy__sovereignty_primary, suppression_requirement, 2005, 0.9).
narrative_ontology:measurement(bord_su_t2024, border_control_legitimacy__sovereignty_primary, suppression_requirement, 2024, 0.92).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(border_control_legitimacy__sovereignty_primary, enforcement_mechanism).
narrative_ontology:affects_constraint(border_control_legitimacy__sovereignty_primary, international_human_rights_law).
narrative_ontology:affects_constraint(border_control_legitimacy__sovereignty_primary, global_labor_markets).
narrative_ontology:affects_constraint(border_control_legitimacy__sovereignty_primary, refugee_protection_regimes).
narrative_ontology:affects_constraint(border_control_legitimacy__sovereignty_primary, border_control_legitimacy__freedom_of_movement_primary).
narrative_ontology:affects_constraint(border_control_legitimacy__sovereignty_primary, border_control_legitimacy__jurisdictional_sovereignty).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'border_control_legitimacy' kernel. Its high extractiveness and suppression distinguish it from sibling readings that prioritize freedom of movement or balanced jurisdictional sovereignty.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
