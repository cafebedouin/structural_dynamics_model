% ============================================================================
% CONSTRAINT STORY: separation_of_powers_text__functionalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_separation_of_powers_text__functionalist_reading, []).

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
 *   constraint_id: separation_of_powers_text__functionalist_reading
 *   human_readable: Functionalist Interpretation of Separation of Powers
 *   domain: constitutional_law/political_theory/administrative_law
 *
 * SUMMARY:
 *   This constraint represents the functionalist reading of the separation of
 *   powers doctrine, which views the constitutional framework as flexible,
 *   permitting overlapping authority and the delegation of legislative power
 *   to administrative agencies under 'intelligible principles.' This
 *   interpretation legitimizes the modern regulatory state and emphasizes
 *   effective governance over rigid structural boundaries. It is one reading
 *   of the broader 'separation_of_powers_text' kernel.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(separation_of_powers_text__functionalist_reading, 0.35).
domain_priors:suppression_score(separation_of_powers_text__functionalist_reading, 0.45).
domain_priors:theater_ratio(separation_of_powers_text__functionalist_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(separation_of_powers_text__functionalist_reading, extractiveness, 0.35).
narrative_ontology:constraint_metric(separation_of_powers_text__functionalist_reading, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(separation_of_powers_text__functionalist_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(separation_of_powers_text__functionalist_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(separation_of_powers_text__functionalist_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(separation_of_powers_text__functionalist_reading, tangled_rope).
narrative_ontology:human_readable(separation_of_powers_text__functionalist_reading, "Functionalist Interpretation of Separation of Powers").
narrative_ontology:topic_domain(separation_of_powers_text__functionalist_reading, "constitutional_law/political_theory/administrative_law").

domain_priors:requires_active_enforcement(separation_of_powers_text__functionalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(separation_of_powers_text__functionalist_reading, 'f3f2e7ba-e8b3-4e98-82e3-2ded309a92c8').
narrative_ontology:cs_kernel_codification('f3f2e7ba-e8b3-4e98-82e3-2ded309a92c8', fixed_text).
narrative_ontology:cs_authority_grounding('f3f2e7ba-e8b3-4e98-82e3-2ded309a92c8', lineage).
narrative_ontology:cs_interpretation_layer_present('f3f2e7ba-e8b3-4e98-82e3-2ded309a92c8').
narrative_ontology:cs_reading_relation('f3f2e7ba-e8b3-4e98-82e3-2ded309a92c8', separation_of_powers_text__formalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('f3f2e7ba-e8b3-4e98-82e3-2ded309a92c8', separation_of_powers_text__unitary_executive_reading, coexists_with).
narrative_ontology:cs_axiom('f3f2e7ba-e8b3-4e98-82e3-2ded309a92c8', foundational, delegation_for_effective_governance).
narrative_ontology:cs_axiom_status(delegation_for_effective_governance, holdable).
narrative_ontology:cs_axiom_grounding('f3f2e7ba-e8b3-4e98-82e3-2ded309a92c8', delegation_for_effective_governance, instrumental).
narrative_ontology:cs_axiom('f3f2e7ba-e8b3-4e98-82e3-2ded309a92c8', secondary, overlapping_authority_is_efficient).
narrative_ontology:cs_axiom_status(overlapping_authority_is_efficient, holdable).
narrative_ontology:cs_axiom_grounding('f3f2e7ba-e8b3-4e98-82e3-2ded309a92c8', overlapping_authority_is_efficient, empirically_contingent).
narrative_ontology:cs_reference_frame('f3f2e7ba-e8b3-4e98-82e3-2ded309a92c8', modern_administrative_state).
narrative_ontology:cs_drift_state('f3f2e7ba-e8b3-4e98-82e3-2ded309a92c8', contemporary_political_polarization, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('f3f2e7ba-e8b3-4e98-82e3-2ded309a92c8', '').
narrative_ontology:cs_kernel_id(separation_of_powers_text__functionalist_reading, separation_of_powers_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(separation_of_powers_text__functionalist_reading, federal_agencies).
narrative_ontology:constraint_beneficiary(separation_of_powers_text__functionalist_reading, congress).
narrative_ontology:constraint_beneficiary(separation_of_powers_text__functionalist_reading, president).
narrative_ontology:constraint_victim(separation_of_powers_text__functionalist_reading, regulated_parties).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Implement and enforce policy delegated by Congress, operating with a degree of autonomy and expertise. This reading legitimizes their existence and broad powers, allowing them to adapt to complex societal needs.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__functionalist_reading, federal_agencies, agenda_setter,
    institutional, generational, constrained, national).

% Benefits from the ability to delegate complex legislative tasks to expert agencies, freeing up legislative capacity and allowing for more detailed and adaptive policy-making than Congress could achieve alone. Also sets the 'intelligible principles' for delegation.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__functionalist_reading, congress, beneficiary,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(separation_of_powers_text__functionalist_reading, congress, agenda_setter).

% Benefits from the flexibility to oversee and direct a vast administrative state, ensuring that executive functions can be carried out effectively across numerous specialized bodies. Appoints agency heads and influences regulatory direction.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__functionalist_reading, president, beneficiary,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(separation_of_powers_text__functionalist_reading, president, agenda_setter).

% Interprets the scope of delegated authority and the 'intelligible principle' doctrine, providing a check on agency power while generally upholding the legitimacy of the regulatory state under this reading. Their role is to ensure delegation remains within constitutional bounds.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__functionalist_reading, judiciary, agenda_setter,
    institutional, civilizational, analytical, national).
narrative_ontology:stakeholder_secondary_role(separation_of_powers_text__functionalist_reading, judiciary, observer).

% Bear the costs of complying with agency regulations, which can be extensive and complex. While benefiting from the stability and expertise agencies provide, they also experience the direct impact of rules not directly passed by elected legislators.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__functionalist_reading, regulated_parties, payer,
    moderate, biographical, constrained, national).

% Advocate for a strict, impermeable separation of powers, arguing against broad delegation to agencies. While their arguments are part of the legal discourse, this functionalist reading structurally excludes their core premise from being the dominant interpretive framework.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__functionalist_reading, formalist_legal_scholars, excluded,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_secondary_role(separation_of_powers_text__functionalist_reading, formalist_legal_scholars, observer).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a flexible framework for governing a complex modern state by allowing legislative and executive branches to share functions and delegate authority to expert agencies, ensuring effective governance and adaptation to new challenges beyond the capacity of a strictly separated system.
% TRANSFER_FUNCTION: Transfers policy-making and implementation authority from Congress to expert agencies, and from the President to various executive bodies, in exchange for efficiency, specialized governance, and adaptability to evolving societal needs.
% ABSENT_VOICES: Strict formalists and proponents of a truly unitary executive are present in the legal discourse but their arguments for rigid separation are not given structural weight within this functionalist framework, which prioritizes governmental capacity and adaptability.
% DISAPPEARANCE_RATIONALE: If the functionalist interpretation vanished, the entire regulatory state would be delegitimized, agency actions would be challenged as unconstitutional, and the government's ability to address complex issues (e.g., environmental protection, financial regulation, public health) would collapse, requiring a fundamental restructuring of governance.
% FOUNDING_PROBLEM: The original constitutional text provided a framework for a simpler agrarian republic, but the complexities of industrialization and the modern administrative state required a more adaptable interpretation of separated powers to ensure effective governance and address challenges unforeseen by the founders.
% FOUNDING_PROBLEM_CORROBORATION: Legal scholars in administrative law, political scientists studying governance, and historical analyses of the growth of the regulatory state corroborate the ongoing need for a flexible interpretation to address modern challenges, often citing Supreme Court precedents like *Chevron* deference as key to this adaptation. Critics, however, argue the problem has shifted to one of unchecked agency power.
narrative_ontology:disappearance_verdict(separation_of_powers_text__functionalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(separation_of_powers_text__functionalist_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(separation_of_powers_text__functionalist_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(separation_of_powers_text__functionalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(separation_of_powers_text__functionalist_reading, 0.35, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(separation_of_powers_text__functionalist_reading_tests).
:- end_tests(separation_of_powers_text__functionalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The functionalist reading is classified as a Tangled Rope because it genuinely coordinates the complex functions of modern governance by enabling delegation and agency action (beneficiaries: federal_agencies, Congress, President), but it also involves an asymmetric transfer of costs to regulated parties who must comply with agency rules (victims: regulated_parties). Extraction is moderate (0.35) as the costs of regulation are real, but the framework is seen as legitimate and necessary for effective governance. Suppression (0.45) reflects the legal enforcement of agency authority. Theater ratio is low (0.15) as the framework is actively functional, not merely performative.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of federal agencies, Congress, and the President, this interpretation is a necessary and efficient coordination mechanism. From the perspective of formalist legal scholars, it represents an unconstitutional erosion of distinct powers. The engine computes these divergent classifications from the structural data, reflecting the ongoing contest over the constraint's nature.
 *
 * DIRECTIONALITY LOGIC:
 *   Federal agencies, Congress, and the President are beneficiaries, as this reading provides them with the flexibility and capacity to govern effectively. Regulated parties are payers, bearing the costs of compliance with agency rules. The judiciary acts as an agenda-setter by interpreting the boundaries of delegation, and formalist scholars are excluded from the dominant interpretive framework.
 *
 * MANDATROPHY ANALYSIS:
 *   This functionalist reading actively prevents mandatrophy by adapting the separation of powers doctrine to the demands of the modern administrative state. It ensures that the 'founding problem' of effective governance remains 'live' by allowing the government to address complex, evolving challenges through delegated authority, rather than becoming an inert or purely theatrical structure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    functionalist_vs_formalist_legitimacy,
    'Is the flexibility permitted by the functionalist reading a necessary and legitimate adaptation of constitutional principles, or an unconstitutional overreach that undermines the original design?',
    'Long-term judicial consensus shifts, constitutional amendment, or a fundamental change in political theory regarding the nature of governmental power.',
    'If deemed an overreach, the legitimacy of the entire regulatory state would be challenged, potentially leading to a reclassification towards a Snare or Piton from the perspective of those subject to agency rules. If affirmed as legitimate, it reinforces the Tangled Rope classification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(functionalist_vs_formalist_legitimacy, conceptual, 'The core conceptual dispute between functionalist and formalist interpretations of separation of powers.').

omega_variable(
    intelligible_principle_doctrine_efficacy,
    'How effectively does the ''intelligible principle'' doctrine constrain congressional delegation to agencies, preventing arbitrary or unchecked power?',
    'Empirical studies of judicial review of agency actions, analysis of congressional delegation statutes, and case law development regarding the specificity of delegated authority.',
    'If the doctrine is found to be largely ineffective, it would increase the perceived extractiveness and suppression of agency actions, potentially pushing the constraint closer to a Snare by highlighting unchecked power. If effective, it reinforces the coordination aspect.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(intelligible_principle_doctrine_efficacy, empirical, 'The practical limits and effectiveness of the ''intelligible principle'' doctrine.').

omega_variable(
    kernel_reading_identity,
    'Is this constraint a genuine reading of the ''separation_of_powers_text'' kernel, or does it fundamentally depart from the kernel''s core meaning?',
    'Analysis of historical constitutional interpretation, original intent scholarship, and evolving legal consensus on the permissible scope of constitutional adaptation.',
    'If it is deemed a fundamental departure, its legitimacy as a ''reading'' would be undermined, potentially reclassifying it as a Snare or Piton that merely uses the kernel as cover. If affirmed, it reinforces its status as a valid interpretive framework.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'This constraint is the ''functionalist_reading'' of the ''separation_of_powers_text'' kernel. Sibling readings (''formalist_reading'', ''unitary_executive_reading'') would structurally alter the scope of agency power and the distribution of authority between branches.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(separation_of_powers_text__functionalist_reading, 1980, 2020).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sepa_tr_t1980, separation_of_powers_text__functionalist_reading, theater_ratio, 1980, 0.1).
narrative_ontology:measurement(sepa_tr_t1990, separation_of_powers_text__functionalist_reading, theater_ratio, 1990, 0.12).
narrative_ontology:measurement(sepa_tr_t2000, separation_of_powers_text__functionalist_reading, theater_ratio, 2000, 0.13).
narrative_ontology:measurement(sepa_tr_t2010, separation_of_powers_text__functionalist_reading, theater_ratio, 2010, 0.14).
narrative_ontology:measurement(sepa_tr_t2020, separation_of_powers_text__functionalist_reading, theater_ratio, 2020, 0.15).

% Extraction over time
narrative_ontology:measurement(sepa_be_t1980, separation_of_powers_text__functionalist_reading, base_extractiveness, 1980, 0.3).
narrative_ontology:measurement(sepa_be_t1990, separation_of_powers_text__functionalist_reading, base_extractiveness, 1990, 0.32).
narrative_ontology:measurement(sepa_be_t2000, separation_of_powers_text__functionalist_reading, base_extractiveness, 2000, 0.33).
narrative_ontology:measurement(sepa_be_t2010, separation_of_powers_text__functionalist_reading, base_extractiveness, 2010, 0.34).
narrative_ontology:measurement(sepa_be_t2020, separation_of_powers_text__functionalist_reading, base_extractiveness, 2020, 0.35).

% Suppression requirement over time
narrative_ontology:measurement(sepa_su_t1980, separation_of_powers_text__functionalist_reading, suppression_requirement, 1980, 0.4).
narrative_ontology:measurement(sepa_su_t1990, separation_of_powers_text__functionalist_reading, suppression_requirement, 1990, 0.42).
narrative_ontology:measurement(sepa_su_t2000, separation_of_powers_text__functionalist_reading, suppression_requirement, 2000, 0.43).
narrative_ontology:measurement(sepa_su_t2010, separation_of_powers_text__functionalist_reading, suppression_requirement, 2010, 0.44).
narrative_ontology:measurement(sepa_su_t2020, separation_of_powers_text__functionalist_reading, suppression_requirement, 2020, 0.45).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(separation_of_powers_text__functionalist_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(separation_of_powers_text__functionalist_reading, administrative_procedure_act_interpretation).
narrative_ontology:affects_constraint(separation_of_powers_text__functionalist_reading, chevron_deference_doctrine).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'separation_of_powers_text' kernel, alongside 'separation_of_powers_text__formalist_reading' and 'separation_of_powers_text__unitary_executive_reading'. Each reading offers a distinct structural interpretation of the constitutional principle.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
