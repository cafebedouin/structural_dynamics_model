% ============================================================================
% CONSTRAINT STORY: hoa_covenant_scope__behavioral_control_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_hoa_covenant_scope__behavioral_control_reading, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: hoa_covenant_scope__behavioral_control_reading
 *   human_readable: HOA Covenant Enforcement: Behavioral Control Reading
 *   domain: property_law/collective_governance/urban_planning
 *
 * SUMMARY:
 *   An HOA covenant is a legally binding agreement among homeowners that
 *   restricts property use, aesthetic choices, and behavior. This constraint
 *   story instantiates the behavioral_control_reading: covenants are
 *   fundamentally mechanisms for enforcing aesthetic uniformity and
 *   behavioral conformity, with the conformist majority or board-aligned
 *   properties as beneficiaries and nonconformist residents (and those whose
 *   aesthetics diverge from norms) as victims. The enforcement machinery is
 *   extensive, covering appearance, landscaping, colors, yard fixtures,
 *   signage, and lifestyle choices—not just infrastructure protection. The
 *   reading acknowledges that covenants originated for coordination purposes
 *   but argues they have evolved into behavioral control mechanisms where the
 *   coordination rationale provides cover for conformity enforcement. This is
 *   NOT a claim that all HOAs are oppressive or that coordination is
 *   impossible; rather, it is a claim about the primary function and
 *   beneficiary structure this specific constraint operationalizes.
 *
 * KEY AGENTS:
 *   - conformist_majority_homeowners: benefit from covenant enforcement as validator of their aesthetic choices; face minimal enforcement friction
 *   - board_aligned_properties: benefit from selective enforcement and institutional gatekeeping role; set enforcement priorities
 *   - nonconformist_residents: face direct enforcement, fines, and property-value penalties for divergent choices
 *   - marginal_aesthetic_properties: trapped in accumulated violations through structural mismatch, not choice
 *   - speech_restricted_households: experience covenant as suppression of political and expressive freedom
 *   - prospective_buyers: excluded from enforcement decisions; discover restrictions after purchase
 *   - HOA board: formal authority interpreting covenant language and setting enforcement priority with broad discretion
 *   - community_observers: analytical seat examining whether covenants function as coordination or conformity control
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hoa_covenant_scope__behavioral_control_reading, 0.42).
domain_priors:suppression_score(hoa_covenant_scope__behavioral_control_reading, 0.71).
domain_priors:theater_ratio(hoa_covenant_scope__behavioral_control_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hoa_covenant_scope__behavioral_control_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(hoa_covenant_scope__behavioral_control_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(hoa_covenant_scope__behavioral_control_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(hoa_covenant_scope__behavioral_control_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(hoa_covenant_scope__behavioral_control_reading, resistance, 0.57).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hoa_covenant_scope__behavioral_control_reading, snare).
narrative_ontology:human_readable(hoa_covenant_scope__behavioral_control_reading, "HOA Covenant Enforcement: Behavioral Control Reading").
narrative_ontology:topic_domain(hoa_covenant_scope__behavioral_control_reading, "property_law/collective_governance/urban_planning").

domain_priors:requires_active_enforcement(hoa_covenant_scope__behavioral_control_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hoa_covenant_scope__behavioral_control_reading, '8328be61-0bb1-4a17-90ff-c224c6dd78f0').
narrative_ontology:cs_kernel_codification('8328be61-0bb1-4a17-90ff-c224c6dd78f0', formalized).
narrative_ontology:cs_authority_grounding('8328be61-0bb1-4a17-90ff-c224c6dd78f0', extraction).
narrative_ontology:cs_interpretation_layer_present('8328be61-0bb1-4a17-90ff-c224c6dd78f0').
narrative_ontology:cs_reading_relation('8328be61-0bb1-4a17-90ff-c224c6dd78f0', hoa_covenant_scope__coordination_reading, coexists_with).
narrative_ontology:cs_reading_relation('8328be61-0bb1-4a17-90ff-c224c6dd78f0', hoa_covenant_scope__extraction_reading, coexists_with).
narrative_ontology:cs_axiom('8328be61-0bb1-4a17-90ff-c224c6dd78f0', foundational, behavioral_conformity_is_primary_function).
narrative_ontology:cs_axiom_status(behavioral_conformity_is_primary_function, holdable).
narrative_ontology:cs_axiom_grounding('8328be61-0bb1-4a17-90ff-c224c6dd78f0', behavioral_conformity_is_primary_function, empirically_contingent).
narrative_ontology:cs_axiom('8328be61-0bb1-4a17-90ff-c224c6dd78f0', foundational, conformity_enforcement_decoupled_from_infrastructure_necessity).
narrative_ontology:cs_axiom_status(conformity_enforcement_decoupled_from_infrastructure_necessity, holdable).
narrative_ontology:cs_axiom_grounding('8328be61-0bb1-4a17-90ff-c224c6dd78f0', conformity_enforcement_decoupled_from_infrastructure_necessity, empirically_contingent).
narrative_ontology:cs_reference_frame('8328be61-0bb1-4a17-90ff-c224c6dd78f0', covenant_as_infrastructure_coordination_mechanism).
narrative_ontology:cs_drift_state('8328be61-0bb1-4a17-90ff-c224c6dd78f0', contemporary_enforcement_patterns_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('8328be61-0bb1-4a17-90ff-c224c6dd78f0', '').
narrative_ontology:cs_kernel_id(hoa_covenant_scope__behavioral_control_reading, hoa_covenant_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hoa_covenant_scope__behavioral_control_reading, conformist_majority_homeowners).
narrative_ontology:constraint_beneficiary(hoa_covenant_scope__behavioral_control_reading, board_aligned_properties).
narrative_ontology:constraint_victim(hoa_covenant_scope__behavioral_control_reading, nonconformist_residents).
narrative_ontology:constraint_victim(hoa_covenant_scope__behavioral_control_reading, marginal_aesthetic_properties).
narrative_ontology:constraint_victim(hoa_covenant_scope__behavioral_control_reading, speech_restricted_households).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Homeowners whose aesthetic choices, landscaping, and lifestyle align with covenant standards. They benefit from covenant enforcement by seeing it as validating their property choices and maintaining neighborhood appearance consistency. They face minimal enforcement friction and typically view the covenant as protection rather than constraint. Enforcement costs them little because their behavior already matches the standard.
narrative_ontology:constraint_stakeholder(hoa_covenant_scope__behavioral_control_reading, conformist_majority_homeowners, beneficiary,
    organized, biographical, constrained, local).

% HOA board members and properties that benefit from selective enforcement patterns and favorable interpretation of covenant language. They set enforcement priorities, interpret ambiguous standards, and determine which violations trigger action. They collect visibility and influence from their gatekeeping role. They can exit through board rotation but maintain institutional position to shape future enforcement.
narrative_ontology:constraint_stakeholder(hoa_covenant_scope__behavioral_control_reading, board_aligned_properties, beneficiary,
    powerful, generational, arbitrage, local).
narrative_ontology:stakeholder_secondary_role(hoa_covenant_scope__behavioral_control_reading, board_aligned_properties, agenda_setter).

% Homeowners whose aesthetic preferences, landscaping choices, or lifestyle preferences diverge from covenant norms. They face direct enforcement action—violation notices, fines, mandatory remediation, legal action. They bear the economic and emotional cost of either modifying their property to conform or paying accumulated fines. Their identity as their-own-property-custodian conflicts with the covenant's demand for conformity; exiting requires selling the property, which itself becomes difficult because covenant nonconformity depresses resale value and reveals future enforcement risk.
narrative_ontology:constraint_stakeholder(hoa_covenant_scope__behavioral_control_reading, nonconformist_residents, payer,
    moderate, biographical, identity_locked, local).

% Properties whose appearance deviates from norms through age, maintenance burden, or legitimate use patterns (multi-generational household, artistic expression, indigenous landscaping, disability accommodation). They accumulate violations not from choice but from structural mismatch between their situation and the covenant's one-size-fits-all standard. They lack resources to hire compliance consultants or legal defense. Selling the property is difficult because the covenant history follows the title and prospective buyers anticipate enforcement costs.
narrative_ontology:constraint_stakeholder(hoa_covenant_scope__behavioral_control_reading, marginal_aesthetic_properties, payer,
    moderate, biographical, trapped, local).

% Residents who wish to display political signs, religious symbols, or activist flags but face covenant restrictions on 'non-residential' displays. They experience the covenant as suppression of political and expressive freedom, not just aesthetic control. Their identity as politically engaged citizens or religious practitioners conflicts with the covenant's demand for conformity. They face fines for exercising what they understand as constitutional rights on their own property.
narrative_ontology:constraint_stakeholder(hoa_covenant_scope__behavioral_control_reading, speech_restricted_households, payer,
    powerless, immediate, identity_locked, local).

% Buyers entering the community are often not present during covenant rule-setting; disclosure of enforcement history and covenant scope is patchy. They discover restrictions after purchase and cannot renegotiate. They are excluded from the enforcement decision-making that shapes their property rights after purchase. Some learn only through enforcement action that their intended use (home office, garden design, political signage) violates covenants they did not negotiate.
narrative_ontology:constraint_stakeholder(hoa_covenant_scope__behavioral_control_reading, prospective_buyers, excluded,
    moderate, biographical, mobile, local).

% The formal authority that interprets covenant language, initiates enforcement, and sets enforcement priority. Board members claim they are applying objective standards consistently; residents in conflict argue the standards are subjective and enforcement is selective. The board operates with minimal transparency into decision-making and faces minimal accountability for selective or harsh enforcement. They administer the covenant as written but their discretion in interpretation is broad.
narrative_ontology:constraint_stakeholder(hoa_covenant_scope__behavioral_control_reading, hoa_board, agenda_setter,
    institutional, generational, constrained, local).

% Researchers, legal analysts, and policy advocates studying whether HOA covenants function as intended (shared infrastructure protection) or operate primarily as mechanisms of behavioral control and conformity enforcement. They examine patterns of enforcement, burden distribution, and impact on vulnerable populations. They have no enforcement role but provide external analysis of the constraint's actual function.
narrative_ontology:constraint_stakeholder(hoa_covenant_scope__behavioral_control_reading, community_observers, observer,
    analytical, generational, analytical, local).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(hoa_covenant_scope__behavioral_control_reading, board_aligned_properties).
narrative_ontology:fixing_cost_class(hoa_covenant_scope__behavioral_control_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: This reading rejects or subordinates any coordination function. The behavioral control reading emphasizes that covenants are enforced to suppress aesthetic and lifestyle diversity—the stated coordination rationale (shared infrastructure, property values) masks a primary function of enforcing conformity. Any coordination benefit is incidental; the central dynamic is behavioral control.
% TRANSFER_FUNCTION: Transfers decision-making authority from individual homeowners to a collective enforcement apparatus (HOA board), and transfers economic resources from nonconforming residents to the conformist majority through fines, legal costs, and property-value penalties incurred when conformity demands are resisted. Transfers leisure time and autonomy from residents to compliance work.
% ABSENT_VOICES: Residents who have already sold and left due to covenant pressure; prospective buyers who self-selected out after learning enforcement history; renters and non-owning household members whose preferences are overridden by owner compliance decisions. These voices are structurally absent from covenant amendment and enforcement decisions.
% DISAPPEARANCE_RATIONALE: If covenant enforcement disappeared, residents would immediately resume aesthetic choices aligned with their own preferences and values. Properties would diversify in appearance. Some property values might shift (those appreciated by conformity-seeking buyers would fall; those seeking diverse neighborhoods would rise). The constraint's disappearance would trigger immediate behavioral reorganization across the community.
% FOUNDING_PROBLEM: Original covenant creation was justified as protecting property values and managing shared infrastructure (roads, common areas, water systems). The founding problem was genuine: early subdivisions needed coordination mechanisms for common facilities and wanted predictability in neighborhood character.
% FOUNDING_PROBLEM_CORROBORATION: The HOA board attests the founding problem is live—property values require aesthetic consistency and infrastructure coordination. Property developers and real-estate agents attest that covenants protect property values. However, independent housing researchers, legal scholars analyzing covenant burden, and nonconforming residents attest the founding problem is substantially addressed by infrastructure rules alone (road maintenance, utility management) and has been superseded by behavioral enforcement that has little connection to the original infrastructure coordination need. Academic literature documents covenant function shift from infrastructure coordination to conformity enforcement over 30-40 years.
narrative_ontology:disappearance_verdict(hoa_covenant_scope__behavioral_control_reading, world_rearranges).
narrative_ontology:founding_problem_status(hoa_covenant_scope__behavioral_control_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(hoa_covenant_scope__behavioral_control_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(hoa_covenant_scope__behavioral_control_reading, 'none', 1).
narrative_ontology:epsilon_provenance(hoa_covenant_scope__behavioral_control_reading, 0.42, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hoa_covenant_scope__behavioral_control_reading_tests).
:- end_tests(hoa_covenant_scope__behavioral_control_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.42) because the constraint's primary function is enforcing conformity preferences of the majority/board rather than coordinating genuinely shared infrastructure. The ε reflects the asymmetry: beneficiaries (conformist majority, board) gain order and validation; victims (nonconformists, marginal properties, speech-restricted households) bear the cost of either conforming or paying fines. Suppression is high (0.71) because enforcement operates through fines, legal action, and property-value penalties—coercive mechanisms that persist because nonconformists are trapped (identity-locked or have no exit without substantial loss). Theater is moderately elevated (0.48) because enforcement activity is rhetorically justified as 'protecting property values' and 'maintaining neighborhood character,' yet the actual enforcement targets aesthetic preferences and lifestyle choices uncorrelated with infrastructure or legitimate shared facilities. Accessibility collapse is high (0.68) because once a property is purchased under a covenant, alternatives collapse—selling requires disclosure that depresses value; ignoring the covenant means accumulating fines. Resistance is substantial (0.57) because nonconforming residents actively resist (filing complaints, legal challenges, public advocacy) and some residents self-select out of the community entirely. The measurement series show extractiveness and suppression rising sharply in years 0-15 (as boards consolidate enforcement capacity and residents test boundaries), then plateauing (year 15-40) as enforcement stabilizes at a high level and resistance becomes normalized. Theater ratio rises throughout (early enforcement is genuinely perceived as necessary; later, more is performative—enforcement of aesthetics for their own sake).
 *
 * PERSPECTIVAL GAP:
 *   The board and conformist-majority seats should compute a different type than the victim seats. From the board's perspective, covenants are legitimate governance structures enforcing standards the community agreed to—this seat experiences the constraint as coordination or at worst rope (asymmetric but functionally necessary). From the nonconformist resident's perspective, covenants are enforcers of majority preference with no consent and no exit—this seat experiences the constraint as snare (pure extraction with coercive backing). From the marginal-property seat, covenants are traps where legitimate use creates accumulated violations—this seat experiences it as snare with identity-lock. The engine computes these per-seat divergences from the structural data: high directionality (d) for victim seats (trapped/identity-locked exit, victim role, high suppression driving target classification), low directionality for beneficiary seats (mobile or arbitrage exit, beneficiary role, enforcement alignment), asymmetric power distribution across seats, and high suppression overall.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary seats (conformist_majority, board_aligned_properties) have low directionality (d near 0.0-0.3): they benefit from enforcement, face minimal friction, and have mobile exit options (can sell profitably, can participate in board governance). Their exit_options vary (conformist_majority are 'constrained' but only by choice alignment; board_aligned are 'arbitrage' through institutional position). Victim seats (nonconformist_residents, marginal_aesthetic_properties, speech_restricted_households) have high directionality (d near 0.7-1.0): they bear enforcement costs (fines, property devaluation, autonomy loss), have trapped or identity_locked exit (selling triggers value penalty, identity as autonomous property-custodian conflicts with conformity demand), and face high suppression (enforcement machinery prevents alternatives, accumulating penalties make exit expensive). Prospective_buyers are 'excluded' rather than coordinated—their exclusion from decision-making is itself the enforcement object. This directionality distribution is the structural signature of a snare: beneficiaries gain order and values validation (low d → low extraction experienced by them), victims gain nothing and pay enforced conformity (high d → high extraction experienced by them).
 *
 * MANDATROPHY ANALYSIS:
 *   The behavioral_control reading avoids the mandatrophy trap by explicitly rejecting or subordinating the coordination framing. Covenants originated (founding_problem=genuine coordination need for shared infrastructure) but have evolved (founding_problem_status=dead or contested—infrastructure coordination can proceed without behavioral enforcement; behavioral enforcement persists for conformity's sake). This is mandatrophy in its classic form: the original problem is solved or solved elsewhere, but the constraint persists. The behavioral_control reading directly names this: 'The founding coordination problem is substantially addressed; behavioral enforcement persists to enforce conformity that has little connection to the original need.' Suppression (0.71) and enforcement machinery existence (requires_active_enforcement=true) confirm the constraint persists only through coercive backing, not through participant preference.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    coordination_extraction_boundary_hoa,
    'Is the behavioral conformity enforcement structurally necessary for the legitimate coordination functions (infrastructure maintenance, common area management, shared facility operation), or are those functions achievable independent of aesthetic and lifestyle enforcement?',
    'Jurisdictional comparison: covenants that separate infrastructure rules (with enforcement) from aesthetic rules (without enforcement), monitoring whether property values and infrastructure coordination persist. Natural experiments from covenant reforms that narrow enforcement scope.',
    'If infrastructure coordination survives without behavioral enforcement, the measured extractiveness reflects pure conformity extraction unrelated to legitimate coordination. If infrastructure coordination degrades without behavioral enforcement, some portion of the extractiveness is the true cost of coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_extraction_boundary_hoa, empirical, 'Whether behavioral conformity enforcement is structurally separable from infrastructure coordination functions.').

omega_variable(
    aesthetic_standard_objectivity,
    'Are covenant aesthetic standards objective and consistently applied, or do they reflect subjective preferences of dominant groups and face selective enforcement?',
    'Enforcement pattern analysis: audit which properties receive violation notices for identical covenant language interpretations; track how enforcement priority changes with board composition; document divergence between board interpretation and resident understanding of the same covenant language.',
    'If standards are objective and consistently applied, the constraint is closer to coordination (uniform rules predictably enforced). If standards are subjective and selectively enforced, the extractiveness is higher—the beneficiary group uses covenant power to enforce their aesthetic preferences while suppressing others''.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(aesthetic_standard_objectivity, empirical, 'Whether aesthetic standards in covenant language are objective or reflect group-specific preferences.').

omega_variable(
    founding_problem_persistence,
    'Do property values actually depend on aesthetic uniformity, or do they depend on infrastructure quality and neighborhood safety—objectives achievable without conformity enforcement?',
    'Property valuation analysis in covenants with strong vs. weak aesthetic enforcement; comparison of neighborhoods with and without behavioral covenants; buyer preference surveys about which neighborhood attributes drive purchase decisions.',
    'If property values depend primarily on infrastructure and safety, aesthetic enforcement is extractive overlay unrelated to value protection. If they depend on aesthetic uniformity, part of the extractiveness is legitimate coordination cost. This is where the readings diverge most sharply.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(founding_problem_persistence, empirical, 'Whether property values actually require aesthetic uniformity or are driven by other factors.').

omega_variable(
    reading_identity_kernel_context,
    'This constraint is one reading of the contested kernel ''hoa_covenant_scope''. The behavioral_control_reading instantiates a specific interpretation: covenants exist PRIMARILY to enforce conformity, and any coordination function is subordinate or incidental. How does this reading relate to the sibling readings (coordination_reading, extraction_reading)?',
    'The engine computes per-seat classification; divergence between readings indicates which frame (coordination, extraction, conformity control) best captures the structural reality from each stakeholder''s vantage. This omega documents the kernel-level contest—the same legal text (the covenant), different structural interpretations, different ε values and victim/beneficiary mappings.',
    'If the behavioral_control reading''s metrics compute to Snare while the coordination_reading computes to Rope, the divergence is diagnostic: both readings are coherent internally, but they describe different constraint structures. The behavioral_control_reading''s ε=0.42 reflects the moderate extractiveness of conformity enforcement; a Snare classification emphasizes the asymmetric victim/beneficiary structure and high suppression. The coordination_reading would lower ε and assert beneficiaries more broadly. The extraction_reading would raise ε and emphasize revenue mechanisms.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_identity_kernel_context, conceptual, 'Kernel-level contest: this reading frames covenants as conformity control; siblings frame them as coordination or revenue extraction.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hoa_covenant_scope__behavioral_control_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hoa__tr_t0, hoa_covenant_scope__behavioral_control_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(hoa__tr_t5, hoa_covenant_scope__behavioral_control_reading, theater_ratio, 5, 0.31).
narrative_ontology:measurement(hoa__tr_t10, hoa_covenant_scope__behavioral_control_reading, theater_ratio, 10, 0.38).
narrative_ontology:measurement(hoa__tr_t15, hoa_covenant_scope__behavioral_control_reading, theater_ratio, 15, 0.43).
narrative_ontology:measurement(hoa__tr_t20, hoa_covenant_scope__behavioral_control_reading, theater_ratio, 20, 0.46).
narrative_ontology:measurement(hoa__tr_t25, hoa_covenant_scope__behavioral_control_reading, theater_ratio, 25, 0.48).
narrative_ontology:measurement(hoa__tr_t30, hoa_covenant_scope__behavioral_control_reading, theater_ratio, 30, 0.49).
narrative_ontology:measurement(hoa__tr_t40, hoa_covenant_scope__behavioral_control_reading, theater_ratio, 40, 0.48).

% Extraction over time
narrative_ontology:measurement(hoa__be_t0, hoa_covenant_scope__behavioral_control_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(hoa__be_t5, hoa_covenant_scope__behavioral_control_reading, base_extractiveness, 5, 0.32).
narrative_ontology:measurement(hoa__be_t10, hoa_covenant_scope__behavioral_control_reading, base_extractiveness, 10, 0.37).
narrative_ontology:measurement(hoa__be_t15, hoa_covenant_scope__behavioral_control_reading, base_extractiveness, 15, 0.39).
narrative_ontology:measurement(hoa__be_t20, hoa_covenant_scope__behavioral_control_reading, base_extractiveness, 20, 0.41).
narrative_ontology:measurement(hoa__be_t25, hoa_covenant_scope__behavioral_control_reading, base_extractiveness, 25, 0.42).
narrative_ontology:measurement(hoa__be_t30, hoa_covenant_scope__behavioral_control_reading, base_extractiveness, 30, 0.42).
narrative_ontology:measurement(hoa__be_t40, hoa_covenant_scope__behavioral_control_reading, base_extractiveness, 40, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(hoa__su_t0, hoa_covenant_scope__behavioral_control_reading, suppression_requirement, 0, 0.58).
narrative_ontology:measurement(hoa__su_t5, hoa_covenant_scope__behavioral_control_reading, suppression_requirement, 5, 0.62).
narrative_ontology:measurement(hoa__su_t10, hoa_covenant_scope__behavioral_control_reading, suppression_requirement, 10, 0.65).
narrative_ontology:measurement(hoa__su_t15, hoa_covenant_scope__behavioral_control_reading, suppression_requirement, 15, 0.68).
narrative_ontology:measurement(hoa__su_t20, hoa_covenant_scope__behavioral_control_reading, suppression_requirement, 20, 0.7).
narrative_ontology:measurement(hoa__su_t25, hoa_covenant_scope__behavioral_control_reading, suppression_requirement, 25, 0.71).
narrative_ontology:measurement(hoa__su_t30, hoa_covenant_scope__behavioral_control_reading, suppression_requirement, 30, 0.71).
narrative_ontology:measurement(hoa__su_t40, hoa_covenant_scope__behavioral_control_reading, suppression_requirement, 40, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hoa_covenant_scope__behavioral_control_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(hoa_covenant_scope__behavioral_control_reading, 0.12).
narrative_ontology:affects_constraint(hoa_covenant_scope__behavioral_control_reading, hoa_covenant_scope__coordination_reading).
narrative_ontology:affects_constraint(hoa_covenant_scope__behavioral_control_reading, hoa_covenant_scope__extraction_reading).

% DUAL FORMULATION NOTE:
% The hoa_covenant_scope kernel decomposes into three structurally distinct constraint readings with different ε values, beneficiary/victim structures, and types. The behavioral_control_reading (this file) interprets covenants as mechanisms for enforcing conformity; the coordination_reading interprets them as infrastructure coordination; the extraction_reading interprets them as revenue mechanisms. Each reading is a coherent ε-invariant constraint with its own metrics and stakeholder structure. Per-seat classification from the engine will show how the same legal covenant structure produces different constraint types when observed from different seats—the Snare type from victim seats, Rope or Tangled Rope type from beneficiary seats. The decomposition routes the kernel contest through the framework's standard apparatus: three separate stories, three separate ε values, one network of affects_constraints edges.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(hoa_covenant_scope__behavioral_control_reading, organized, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
