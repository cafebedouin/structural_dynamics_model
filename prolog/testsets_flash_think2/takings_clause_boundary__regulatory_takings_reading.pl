% ============================================================================
% CONSTRAINT STORY: takings_clause_boundary__regulatory_takings_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_takings_clause_boundary__regulatory_takings_reading, []).

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
 *   constraint_id: takings_clause_boundary__regulatory_takings_reading
 *   human_readable: Regulatory Takings Doctrine (Penn Central Reading)
 *   domain: constitutional_law/property_rights
 *
 * SUMMARY:
 *   The regulatory takings doctrine, primarily established by the Supreme
 *   Court's 1978 Penn Central Transportation Co. v. City of New York
 *   decision, holds that regulations that 'go too far' in diminishing the
 *   economic value of private property constitute a 'taking' requiring just
 *   compensation under the Fifth Amendment. This reading expands the concept
 *   of a taking beyond direct physical appropriation to include severe
 *   regulatory burdens, introducing an ad hoc balancing test to evaluate such
 *   claims. It aims to protect property owners from uncompensated value
 *   diminution while allowing for legitimate public welfare regulations.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(takings_clause_boundary__regulatory_takings_reading, 0.65).
domain_priors:suppression_score(takings_clause_boundary__regulatory_takings_reading, 0.45).
domain_priors:theater_ratio(takings_clause_boundary__regulatory_takings_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(takings_clause_boundary__regulatory_takings_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(takings_clause_boundary__regulatory_takings_reading, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(takings_clause_boundary__regulatory_takings_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(takings_clause_boundary__regulatory_takings_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(takings_clause_boundary__regulatory_takings_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(takings_clause_boundary__regulatory_takings_reading, tangled_rope).
narrative_ontology:human_readable(takings_clause_boundary__regulatory_takings_reading, "Regulatory Takings Doctrine (Penn Central Reading)").
narrative_ontology:topic_domain(takings_clause_boundary__regulatory_takings_reading, "constitutional_law/property_rights").

domain_priors:requires_active_enforcement(takings_clause_boundary__regulatory_takings_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(takings_clause_boundary__regulatory_takings_reading, 'c45ba631-92b0-4e5e-9a1f-97e86ee8cabd').
narrative_ontology:cs_kernel_codification('c45ba631-92b0-4e5e-9a1f-97e86ee8cabd', fixed_text).
narrative_ontology:cs_authority_grounding('c45ba631-92b0-4e5e-9a1f-97e86ee8cabd', lineage).
narrative_ontology:cs_interpretation_layer_present('c45ba631-92b0-4e5e-9a1f-97e86ee8cabd').
narrative_ontology:cs_reading_relation('c45ba631-92b0-4e5e-9a1f-97e86ee8cabd', takings_clause_boundary__physical_appropriation_reading, influences).
narrative_ontology:cs_reading_relation('c45ba631-92b0-4e5e-9a1f-97e86ee8cabd', takings_clause_boundary__categorical_takings_reading, coexists_with).
narrative_ontology:cs_axiom('c45ba631-92b0-4e5e-9a1f-97e86ee8cabd', foundational, economic_value_is_property).
narrative_ontology:cs_axiom_status(economic_value_is_property, holdable).
narrative_ontology:cs_axiom_grounding('c45ba631-92b0-4e5e-9a1f-97e86ee8cabd', economic_value_is_property, conventional).
narrative_ontology:cs_axiom('c45ba631-92b0-4e5e-9a1f-97e86ee8cabd', foundational, regulatory_burden_can_be_taking).
narrative_ontology:cs_axiom_status(regulatory_burden_can_be_taking, holdable).
narrative_ontology:cs_axiom_grounding('c45ba631-92b0-4e5e-9a1f-97e86ee8cabd', regulatory_burden_can_be_taking, conventional).
narrative_ontology:cs_reference_frame('c45ba631-92b0-4e5e-9a1f-97e86ee8cabd', penn_central_balancing_framework).
narrative_ontology:cs_drift_state('c45ba631-92b0-4e5e-9a1f-97e86ee8cabd', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('c45ba631-92b0-4e5e-9a1f-97e86ee8cabd', '').
narrative_ontology:cs_kernel_id(takings_clause_boundary__regulatory_takings_reading, takings_clause_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(takings_clause_boundary__regulatory_takings_reading, property_owners).
narrative_ontology:constraint_beneficiary(takings_clause_boundary__regulatory_takings_reading, developers).
narrative_ontology:constraint_victim(takings_clause_boundary__regulatory_takings_reading, local_governments).
narrative_ontology:constraint_victim(takings_clause_boundary__regulatory_takings_reading, public_at_large).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefit from the right to compensation when regulations significantly diminish the economic value of their property, even without physical appropriation. They bear the initial regulatory burden but can seek judicial relief.
narrative_ontology:constraint_stakeholder(takings_clause_boundary__regulatory_takings_reading, property_owners, beneficiary,
    powerful, biographical, constrained, national).

% Bear the cost of compensation when their regulations are deemed a taking. This creates a fiscal constraint on their ability to enact land-use, environmental, or other public welfare regulations, leading to regulatory chill or increased public expenditure.
narrative_ontology:constraint_stakeholder(takings_clause_boundary__regulatory_takings_reading, local_governments, payer,
    institutional, generational, constrained, local).

% Benefit from increased certainty regarding the limits of uncompensated regulation, which can reduce investment risk. They can leverage the doctrine to challenge regulations that impede their projects or demand concessions.
narrative_ontology:constraint_stakeholder(takings_clause_boundary__regulatory_takings_reading, developers, beneficiary,
    powerful, biographical, mobile, national).

% Indirectly bears the cost of takings compensation through taxes or reduced public services. Also suffers from foregone or weakened regulations that would otherwise protect environmental quality, public health, or aesthetic values.
narrative_ontology:constraint_stakeholder(takings_clause_boundary__regulatory_takings_reading, public_at_large, payer,
    organized, generational, constrained, national).

% Interpret and apply the 'ad hoc, factual inquiry' of the Penn Central test, determining when a regulation 'goes too far'. They are the primary enforcers and shapers of this doctrine, balancing competing interests.
narrative_ontology:constraint_stakeholder(takings_clause_boundary__regulatory_takings_reading, courts, agenda_setter,
    institutional, civilizational, analytical, national).

% Often find their proposed regulations challenged or diluted due to the threat of takings claims. While they can participate in legislative processes, their core goals are often in tension with the property rights protections afforded by this doctrine.
narrative_ontology:constraint_stakeholder(takings_clause_boundary__regulatory_takings_reading, environmental_advocates, excluded,
    organized, generational, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(takings_clause_boundary__regulatory_takings_reading, diffuse).
narrative_ontology:fixing_cost_class(takings_clause_boundary__regulatory_takings_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a legal framework for balancing the government's power to regulate for public welfare against private property owners' rights to economic use of their land, by specifying conditions under which compensation is required.
% TRANSFER_FUNCTION: Transfers economic value (compensation payments) from local governments (and thus the public purse) to private property owners when regulations are deemed to diminish property value 'too far'.
% ABSENT_VOICES: Public interest groups advocating for stronger environmental protections, affordable housing, or public access to resources often find their regulatory goals constrained by the threat of takings claims. They would argue for a broader interpretation of public welfare and a narrower scope for compensation.
% DISAPPEARANCE_RATIONALE: If the regulatory takings doctrine vanished, governments would have significantly more freedom to regulate land use and economic activity without fear of compensation claims. This would likely lead to more aggressive environmental protection, zoning, and public infrastructure projects, but also a substantial shift in the balance of power between public and private interests, potentially diminishing property values without recourse.
% FOUNDING_PROBLEM: To prevent government from effectively confiscating private property through regulation without paying just compensation, thereby undermining the Fifth Amendment's purpose of protecting private property from uncompensated public use.
% FOUNDING_PROBLEM_CORROBORATION: Property rights organizations, legal scholars, and landowners (outside government) consistently attest that the problem of potential uncompensated regulatory burdens remains a live concern, necessitating the doctrine's continued application.
narrative_ontology:disappearance_verdict(takings_clause_boundary__regulatory_takings_reading, world_rearranges).
narrative_ontology:founding_problem_status(takings_clause_boundary__regulatory_takings_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(takings_clause_boundary__regulatory_takings_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(takings_clause_boundary__regulatory_takings_reading, 'none', 1).
narrative_ontology:epsilon_provenance(takings_clause_boundary__regulatory_takings_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(takings_clause_boundary__regulatory_takings_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(takings_clause_boundary__regulatory_takings_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(takings_clause_boundary__regulatory_takings_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.65) reflects the significant financial burden placed on governments (and thus the public) to compensate property owners for regulations that might otherwise serve a public good. Suppression (0.45) is moderate, as the threat of takings claims can lead to 'regulatory chill,' deterring or weakening beneficial regulations. Theater ratio (0.30) is present due to the inherently vague 'ad hoc' nature of the Penn Central test, leading to extensive litigation and strategic maneuvering by both sides. The measurements show a slight increase in extractiveness and suppression over time as the doctrine's application expanded, followed by a slight leveling off as courts refined its scope.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of property owners, this doctrine is a vital protection of constitutional rights against overreaching government power. From the perspective of local governments and public interest advocates, it is a significant impediment to addressing pressing public welfare issues, often seen as prioritizing private profit over collective good. The engine's classification will highlight this tension.
 *
 * DIRECTIONALITY LOGIC:
 *   Property owners and developers are clear beneficiaries, receiving compensation or increased regulatory certainty. Local governments and the public at large are the payers, bearing the direct costs of compensation and the indirect costs of regulatory chill or foregone public benefits. The courts act as the agenda-setters, defining and enforcing the boundaries of this complex legal constraint.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    ad_hoc_balancing_uncertainty,
    'How much does the ''ad hoc, factual inquiry'' nature of the Penn Central test contribute to regulatory uncertainty and litigation costs, versus providing necessary flexibility?',
    'Empirical studies comparing regulatory outcomes and litigation rates in jurisdictions with more rigid versus more flexible takings tests, or analysis of judicial consistency in applying the Penn Central factors.',
    'If uncertainty and litigation costs are high, the doctrine''s coordination function is undermined, increasing its effective extractiveness and theater ratio. If flexibility is deemed essential, these costs might be seen as inherent to balancing complex interests.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ad_hoc_balancing_uncertainty, empirical, 'The impact of the Penn Central test''s inherent vagueness on regulatory predictability and efficiency.').

omega_variable(
    regulatory_chill_extent,
    'To what extent does the threat of regulatory takings claims actually deter or weaken beneficial public welfare regulations, rather than merely ensuring fair compensation?',
    'Comparative analysis of regulatory stringency and outcomes in areas with strong versus weak takings jurisprudence, or surveys of local government officials regarding their decision-making processes.',
    'If regulatory chill is widespread and significant, the doctrine''s suppression metric is effectively higher, indicating a greater cost to public welfare. If the effect is minimal, the doctrine primarily functions as a compensation mechanism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_chill_extent, empirical, 'Measuring the actual impact of takings claims on government''s willingness to regulate.').

omega_variable(
    conceptual_framing_of_property,
    'Is property primarily a natural right inherent to individuals, or a social construct defined and limited by law for the common good?',
    'This is a conceptual/philosophical question, not empirically resolvable. Resolution depends on adopting a specific jurisprudential or political philosophy.',
    'If property is a natural right, the doctrine is seen as a necessary defense against government encroachment. If it''s a social construct, the doctrine''s compensation requirements might be viewed as an unnecessary constraint on democratic governance, increasing its perceived extractiveness.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(conceptual_framing_of_property, conceptual, 'The philosophical grounding of property rights and its implications for regulatory takings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(takings_clause_boundary__regulatory_takings_reading, 1978, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Extraction over time
narrative_ontology:measurement(taki_be_t1978, takings_clause_boundary__regulatory_takings_reading, base_extractiveness, 1978, 0.55).
narrative_ontology:measurement(taki_be_t1988, takings_clause_boundary__regulatory_takings_reading, base_extractiveness, 1988, 0.6).
narrative_ontology:measurement(taki_be_t1998, takings_clause_boundary__regulatory_takings_reading, base_extractiveness, 1998, 0.63).
narrative_ontology:measurement(taki_be_t2008, takings_clause_boundary__regulatory_takings_reading, base_extractiveness, 2008, 0.66).
narrative_ontology:measurement(taki_be_t2018, takings_clause_boundary__regulatory_takings_reading, base_extractiveness, 2018, 0.68).
narrative_ontology:measurement(taki_be_t2024, takings_clause_boundary__regulatory_takings_reading, base_extractiveness, 2024, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(taki_su_t1978, takings_clause_boundary__regulatory_takings_reading, suppression_requirement, 1978, 0.35).
narrative_ontology:measurement(taki_su_t1988, takings_clause_boundary__regulatory_takings_reading, suppression_requirement, 1988, 0.4).
narrative_ontology:measurement(taki_su_t1998, takings_clause_boundary__regulatory_takings_reading, suppression_requirement, 1998, 0.45).
narrative_ontology:measurement(taki_su_t2008, takings_clause_boundary__regulatory_takings_reading, suppression_requirement, 2008, 0.5).
narrative_ontology:measurement(taki_su_t2018, takings_clause_boundary__regulatory_takings_reading, suppression_requirement, 2018, 0.48).
narrative_ontology:measurement(taki_su_t2024, takings_clause_boundary__regulatory_takings_reading, suppression_requirement, 2024, 0.45).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(takings_clause_boundary__regulatory_takings_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(takings_clause_boundary__regulatory_takings_reading, environmental_regulations).
narrative_ontology:affects_constraint(takings_clause_boundary__regulatory_takings_reading, zoning_laws).
narrative_ontology:affects_constraint(takings_clause_boundary__regulatory_takings_reading, land_use_planning).

% DUAL FORMULATION NOTE:
% This is one of three readings of the 'takings_clause_boundary' kernel, focusing on regulatory burdens. It influences and coexists with other interpretations of the Takings Clause.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
