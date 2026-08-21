% ============================================================================
% CONSTRAINT STORY: divine_legitimacy_substrate__amun_polytheistic_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_divine_legitimacy_substrate__amun_polytheistic_reading, []).

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
 *   constraint_id: divine_legitimacy_substrate__amun_polytheistic_reading
 *   human_readable: Divine Legitimacy via Amun-Ra Polytheistic Priesthood
 *   domain: ancient_history/religious_studies/political_economy_of_belief_systems
 *
 * SUMMARY:
 *   This constraint describes the system of divine legitimacy in ancient
 *   Egypt, where the Amun priesthood held primary interpretive authority over
 *   a multi-deity cosmology, with Amun-Ra as the chief patron. This system
 *   provided the ideological substrate for pharaonic rule and social order,
 *   but also enabled significant extraction by the priesthood. This story is
 *   one reading of the 'divine_legitimacy_substrate' kernel, focusing on the
 *   established polytheistic framework that preceded and largely outlasted
 *   the Atenist challenge.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(divine_legitimacy_substrate__amun_polytheistic_reading, 0.7).
domain_priors:suppression_score(divine_legitimacy_substrate__amun_polytheistic_reading, 0.85).
domain_priors:theater_ratio(divine_legitimacy_substrate__amun_polytheistic_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(divine_legitimacy_substrate__amun_polytheistic_reading, extractiveness, 0.7).
narrative_ontology:constraint_metric(divine_legitimacy_substrate__amun_polytheistic_reading, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(divine_legitimacy_substrate__amun_polytheistic_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(divine_legitimacy_substrate__amun_polytheistic_reading, accessibility_collapse, 0.88).
narrative_ontology:constraint_metric(divine_legitimacy_substrate__amun_polytheistic_reading, resistance, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(divine_legitimacy_substrate__amun_polytheistic_reading, tangled_rope).
narrative_ontology:human_readable(divine_legitimacy_substrate__amun_polytheistic_reading, "Divine Legitimacy via Amun-Ra Polytheistic Priesthood").
narrative_ontology:topic_domain(divine_legitimacy_substrate__amun_polytheistic_reading, "ancient_history/religious_studies/political_economy_of_belief_systems").

domain_priors:requires_active_enforcement(divine_legitimacy_substrate__amun_polytheistic_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(divine_legitimacy_substrate__amun_polytheistic_reading, '43b63ff4-0921-4547-a76c-9d56aa1919bd').
narrative_ontology:cs_kernel_codification('43b63ff4-0921-4547-a76c-9d56aa1919bd', formalized).
narrative_ontology:cs_authority_grounding('43b63ff4-0921-4547-a76c-9d56aa1919bd', lineage).
narrative_ontology:cs_interpretation_layer_present('43b63ff4-0921-4547-a76c-9d56aa1919bd').
narrative_ontology:cs_reading_relation('43b63ff4-0921-4547-a76c-9d56aa1919bd', divine_legitimacy_substrate__atenist_monotheistic_reading, forecloses).
narrative_ontology:cs_reading_relation('43b63ff4-0921-4547-a76c-9d56aa1919bd', divine_legitimacy_substrate__folk_syncretistic_reading, coexists_with).
narrative_ontology:cs_axiom('43b63ff4-0921-4547-a76c-9d56aa1919bd', foundational, divine_plurality_with_amun_primacy).
narrative_ontology:cs_axiom_status(divine_plurality_with_amun_primacy, holdable).
narrative_ontology:cs_axiom_grounding('43b63ff4-0921-4547-a76c-9d56aa1919bd', divine_plurality_with_amun_primacy, theological).
narrative_ontology:cs_axiom('43b63ff4-0921-4547-a76c-9d56aa1919bd', foundational, priestly_interpretive_monopoly).
narrative_ontology:cs_axiom_status(priestly_interpretive_monopoly, holdable).
narrative_ontology:cs_axiom_grounding('43b63ff4-0921-4547-a76c-9d56aa1919bd', priestly_interpretive_monopoly, conventional).
narrative_ontology:cs_reference_frame('43b63ff4-0921-4547-a76c-9d56aa1919bd', established_cosmic_order).
narrative_ontology:cs_drift_state('43b63ff4-0921-4547-a76c-9d56aa1919bd', atenist_heresy_era, gap(repudiation_pressure, severe, true)).
narrative_ontology:cs_created_at('43b63ff4-0921-4547-a76c-9d56aa1919bd', '').
narrative_ontology:cs_kernel_id(divine_legitimacy_substrate__amun_polytheistic_reading, divine_legitimacy_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(divine_legitimacy_substrate__amun_polytheistic_reading, amun_priesthood).
narrative_ontology:constraint_beneficiary(divine_legitimacy_substrate__amun_polytheistic_reading, pharaoh).
narrative_ontology:constraint_victim(divine_legitimacy_substrate__amun_polytheistic_reading, general_populace).
narrative_ontology:constraint_victim(divine_legitimacy_substrate__amun_polytheistic_reading, pharaoh).
narrative_ontology:constraint_victim(divine_legitimacy_substrate__amun_polytheistic_reading, regional_cult_priests).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(divine_legitimacy_substrate__amun_polytheistic_reading, scribes_and_administrators).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The established interpreters of the multi-deity cosmology, with Amun-Ra as chief patron. They control vast temple estates, receive substantial offerings, and wield significant political influence by validating the pharaoh's divine mandate. They actively enforce ritual orthodoxy and suppress challenges to their authority.
narrative_ontology:constraint_stakeholder(divine_legitimacy_substrate__amun_polytheistic_reading, amun_priesthood, agenda_setter,
    institutional, generational, arbitrage, national).

% Receives divine legitimacy for their rule through priestly validation, which is crucial for maintaining social order and political stability. However, this comes at the cost of significant resources (land, offerings, labor) and political deference to the Amun priesthood, constraining their absolute power.
narrative_ontology:constraint_stakeholder(divine_legitimacy_substrate__amun_polytheistic_reading, pharaoh, beneficiary,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(divine_legitimacy_substrate__amun_polytheistic_reading, pharaoh, payer).

% Bears the primary costs through mandatory offerings, labor for temple construction and maintenance, and adherence to priestly decrees. Their lives are deeply intertwined with the religious calendar and rituals, with little to no option for dissent or alternative belief systems without severe social and political repercussions.
narrative_ontology:constraint_stakeholder(divine_legitimacy_substrate__amun_polytheistic_reading, general_populace, payer,
    powerless, biographical, trapped, local).

% Administer local cults and rituals, often incorporating regional deities. While tolerated, their authority is subordinate to the Amun priesthood, and they must align their practices with the dominant cosmology. They pay through reduced autonomy and potential suppression if they deviate too far from orthodoxy.
narrative_ontology:constraint_stakeholder(divine_legitimacy_substrate__amun_polytheistic_reading, regional_cult_priests, payer,
    moderate, biographical, constrained, local).
narrative_ontology:stakeholder_secondary_role(divine_legitimacy_substrate__amun_polytheistic_reading, regional_cult_priests, excluded).

% Benefit from the stable social and political order maintained by the religious system, which provides their professional roles and status. They are instrumental in managing temple estates and enforcing decrees, but their careers are dependent on the existing power structure.
narrative_ontology:constraint_stakeholder(divine_legitimacy_substrate__amun_polytheistic_reading, scribes_and_administrators, beneficiary,
    moderate, biographical, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes and maintains a coherent cosmic order, legitimizes the pharaoh's rule, provides a shared moral framework, and structures social life through ritual and religious calendar, ensuring stability across a vast and diverse kingdom.
% TRANSFER_FUNCTION: Transfers wealth (land, agricultural produce, precious metals, labor) and political authority from the general populace and the pharaoh to the Amun priesthood, in exchange for divine favor and social cohesion.
% ABSENT_VOICES: Those who might question the divine mandate of the pharaoh or the Amun priesthood's interpretive monopoly, including potential philosophical dissidents or proponents of alternative religious systems (e.g., Atenist adherents during their brief historical emergence). Their voices are suppressed by the state-backed religious authority.
% DISAPPEARANCE_RATIONALE: The entire political, social, and economic structure of ancient Egypt was deeply integrated with this religious system. Its disappearance would lead to a collapse of pharaonic legitimacy, widespread social unrest, fragmentation of the state, and a loss of the unifying cultural narrative, fundamentally reorganizing society.
% FOUNDING_PROBLEM: To establish a stable, divinely sanctioned order that could unify diverse regions, legitimize centralized rule, and provide a framework for cosmic and social harmony in a complex, agrarian society.
% FOUNDING_PROBLEM_CORROBORATION: Historical records, monumental architecture, and religious texts from the period consistently attest to the foundational role of this system in maintaining order and legitimacy. While its specific manifestations evolved, the core problem of divine legitimation for a centralized state remained live throughout much of ancient Egyptian history, corroborated by the long-term stability it provided.
narrative_ontology:disappearance_verdict(divine_legitimacy_substrate__amun_polytheistic_reading, world_rearranges).
narrative_ontology:founding_problem_status(divine_legitimacy_substrate__amun_polytheistic_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(divine_legitimacy_substrate__amun_polytheistic_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(divine_legitimacy_substrate__amun_polytheistic_reading, 'none', 1).
narrative_ontology:epsilon_provenance(divine_legitimacy_substrate__amun_polytheistic_reading, 0.7, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(divine_legitimacy_substrate__amun_polytheistic_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(divine_legitimacy_substrate__amun_polytheistic_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(divine_legitimacy_substrate__amun_polytheistic_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint is classified as a Tangled Rope because it genuinely provides a coordination function (cosmic order, political legitimacy, social cohesion) but also involves substantial asymmetric extraction by the Amun priesthood. Extractiveness is high (0.70) due to the vast wealth and power accumulated by the priesthood. Suppression is very high (0.85) as the system was backed by state power, and challenges to religious orthodoxy were severely punished. Theater ratio is moderate (0.40); while rituals were deeply believed, the emphasis on exclusive priestly interpretation and the suppression of alternatives introduced a performative element to maintain control. Accessibility collapse is high (0.88) as there were virtually no viable alternatives to the state-backed religious system.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the Amun priesthood, this system was a divinely ordained Mountain, essential for cosmic and social order. The pharaoh likely experienced it as a Tangled Rope, providing necessary legitimacy but at a significant cost to their autonomy and resources. The general populace, however, likely experienced it as a Snare, bearing heavy burdens with no viable alternatives, despite the promise of cosmic stability.
 *
 * DIRECTIONALITY LOGIC:
 *   The Amun priesthood is the primary beneficiary and agenda-setter, directly collecting wealth and wielding political power. The pharaoh is also a beneficiary, gaining divine legitimacy, but simultaneously a payer due to the resources and deference required by the priesthood. The general populace are clear payers, contributing labor and offerings with no exit. Regional cult priests are also payers, as their local authority is subordinated to the central Amun cult. The engine will derive distinct directionalities for these seats, reflecting the complex power dynamics.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate (cosmic order, stable rule) remained 'live' throughout its operation, preventing a classification as Piton. However, the high and sustained extractiveness, coupled with active enforcement, indicates that the coordination function was increasingly leveraged for rent-seeking, consistent with a Tangled Rope rather than a pure Rope. The 'contested' status of the founding problem (from an analytical perspective) highlights the ongoing tension between its coordination and extraction functions.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    priestly_power_vs_divine_will,
    'To what extent did the Amun priesthood''s interpretations genuinely reflect divine will, versus serving their institutional self-interest and political power?',
    'Comparative analysis of priestly decrees with independent historical events and archaeological evidence, seeking discrepancies between claimed divine mandates and observable political/economic outcomes.',
    'If interpretations primarily served self-interest, the constraint''s effective extractiveness and theater ratio would be higher, pushing it closer to a Snare. If genuinely believed and aligned with perceived cosmic order, the coordination function would be stronger.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(priestly_power_vs_divine_will, empirical, 'Ambiguity between divine mandate and priestly self-interest.').

omega_variable(
    pharaoh_autonomy_vs_priestly_constraint,
    'What was the true degree of the pharaoh''s autonomy in religious and political matters, given the Amun priesthood''s influence?',
    'Detailed historical case studies of pharaonic attempts to assert independence from the priesthood, analyzing their success, duration, and consequences (e.g., the Atenist period).',
    'If pharaonic autonomy was severely limited, the pharaoh''s directionality would shift further towards ''target'', increasing their effective extraction. If they retained significant leverage, their position would be closer to ''beneficiary''.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(pharaoh_autonomy_vs_priestly_constraint, empirical, 'The extent of pharaonic constraint by priestly power.').

omega_variable(
    legitimacy_source_framing,
    'Is the primary source of legitimacy for this constraint divine revelation (as claimed by the priesthood) or social convention and institutional power (as an analytical observer might frame it)?',
    'Conceptual analysis of the ''grounding_type'' of the foundational axioms, and how that grounding is presented versus how it functions in practice. This is a framing choice for the observer.',
    'Framing it as divine revelation supports the ''Mountain'' claim from the priesthood''s seat. Framing it as social convention highlights the constructed nature and the role of active enforcement, reinforcing the ''Tangled Rope'' classification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(legitimacy_source_framing, conceptual, 'Whether legitimacy is divine or conventional.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(divine_legitimacy_substrate__amun_polytheistic_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(divi_tr_t0, divine_legitimacy_substrate__amun_polytheistic_reading, theater_ratio, 0, 0.3).
narrative_ontology:measurement(divi_tr_t20, divine_legitimacy_substrate__amun_polytheistic_reading, theater_ratio, 20, 0.33).
narrative_ontology:measurement(divi_tr_t40, divine_legitimacy_substrate__amun_polytheistic_reading, theater_ratio, 40, 0.36).
narrative_ontology:measurement(divi_tr_t60, divine_legitimacy_substrate__amun_polytheistic_reading, theater_ratio, 60, 0.38).
narrative_ontology:measurement(divi_tr_t80, divine_legitimacy_substrate__amun_polytheistic_reading, theater_ratio, 80, 0.4).
narrative_ontology:measurement(divi_tr_t100, divine_legitimacy_substrate__amun_polytheistic_reading, theater_ratio, 100, 0.4).

% Extraction over time
narrative_ontology:measurement(divi_be_t0, divine_legitimacy_substrate__amun_polytheistic_reading, base_extractiveness, 0, 0.6).
narrative_ontology:measurement(divi_be_t20, divine_legitimacy_substrate__amun_polytheistic_reading, base_extractiveness, 20, 0.64).
narrative_ontology:measurement(divi_be_t40, divine_legitimacy_substrate__amun_polytheistic_reading, base_extractiveness, 40, 0.67).
narrative_ontology:measurement(divi_be_t60, divine_legitimacy_substrate__amun_polytheistic_reading, base_extractiveness, 60, 0.69).
narrative_ontology:measurement(divi_be_t80, divine_legitimacy_substrate__amun_polytheistic_reading, base_extractiveness, 80, 0.7).
narrative_ontology:measurement(divi_be_t100, divine_legitimacy_substrate__amun_polytheistic_reading, base_extractiveness, 100, 0.7).

% Suppression requirement over time
narrative_ontology:measurement(divi_su_t0, divine_legitimacy_substrate__amun_polytheistic_reading, suppression_requirement, 0, 0.75).
narrative_ontology:measurement(divi_su_t20, divine_legitimacy_substrate__amun_polytheistic_reading, suppression_requirement, 20, 0.8).
narrative_ontology:measurement(divi_su_t40, divine_legitimacy_substrate__amun_polytheistic_reading, suppression_requirement, 40, 0.83).
narrative_ontology:measurement(divi_su_t60, divine_legitimacy_substrate__amun_polytheistic_reading, suppression_requirement, 60, 0.85).
narrative_ontology:measurement(divi_su_t80, divine_legitimacy_substrate__amun_polytheistic_reading, suppression_requirement, 80, 0.85).
narrative_ontology:measurement(divi_su_t100, divine_legitimacy_substrate__amun_polytheistic_reading, suppression_requirement, 100, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(divine_legitimacy_substrate__amun_polytheistic_reading, identity_coordination).
narrative_ontology:affects_constraint(divine_legitimacy_substrate__amun_polytheistic_reading, pharaonic_succession_legitimacy).
narrative_ontology:affects_constraint(divine_legitimacy_substrate__amun_polytheistic_reading, temple_economy_management).
narrative_ontology:affects_constraint(divine_legitimacy_substrate__amun_polytheistic_reading, agricultural_resource_allocation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
