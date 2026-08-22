% ============================================================================
% CONSTRAINT STORY: federation_membership_kernel__integration_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_federation_membership_kernel__integration_reading, []).

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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: federation_membership_kernel__integration_reading
 *   human_readable: EU Free Movement Expansion (Integration Reading)
 *   domain: political_economy/federalism/migration_policy
 *
 * SUMMARY:
 *   This constraint is the integration_reading of the
 *   federation_membership_kernel. It treats EU free movement not merely as an
 *   economic facilitation but as a fundamental right constitutive of EU
 *   citizenship, interpreted expansively by the ECJ to maximize labor
 *   mobility and equal treatment. The reading generates identifiable
 *   victimsâdisplaced local labor, receiving state welfare systems
 *   uncompensated for fiscal burdens, and sending states experiencing brain
 *   drainâwhile the ECJ's supremacy actively overrides national labor
 *   market protections that would bound the right.
 *
 * KEY AGENTS:
 *   - ecj: Primary agenda_setter (institutional/analytical) â interprets and enforces the expansive reading
 *   - mobile_eu_workers: Primary beneficiary (moderate/mobile) â accrues rights and access
 *   - cross_border_employers: Secondary beneficiary (powerful/arbitrage) â captures labor-cost advantages
 *   - displaced_local_labor: Primary target (powerless/constrained) â bears wage and displacement costs
 *   - receiving_state_governments: Institutional payer (institutional/constrained) â absorbs welfare fiscal burden without compensation
 *   - sending_states: Institutional payer (institutional/constrained) â externalizes brain drain costs
 *   - local_labor_unions: Excluded voice (organized/constrained) â overridden in protective policy advocacy
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(federation_membership_kernel__integration_reading, 0.72).
domain_priors:suppression_score(federation_membership_kernel__integration_reading, 0.78).
domain_priors:theater_ratio(federation_membership_kernel__integration_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(federation_membership_kernel__integration_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(federation_membership_kernel__integration_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(federation_membership_kernel__integration_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(federation_membership_kernel__integration_reading, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(federation_membership_kernel__integration_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(federation_membership_kernel__integration_reading, tangled_rope).
narrative_ontology:human_readable(federation_membership_kernel__integration_reading, "EU Free Movement Expansion (Integration Reading)").
narrative_ontology:topic_domain(federation_membership_kernel__integration_reading, "political_economy/federalism/migration_policy").

domain_priors:requires_active_enforcement(federation_membership_kernel__integration_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(federation_membership_kernel__integration_reading, '9c606699-1350-4648-8fd3-7e74085f7400').
narrative_ontology:cs_kernel_codification('9c606699-1350-4648-8fd3-7e74085f7400', formalized).
narrative_ontology:cs_authority_grounding('9c606699-1350-4648-8fd3-7e74085f7400', lineage).
narrative_ontology:cs_interpretation_layer_present('9c606699-1350-4648-8fd3-7e74085f7400').
narrative_ontology:cs_reading_relation('9c606699-1350-4648-8fd3-7e74085f7400', federation_membership_kernel__member_sovereignty_reading, forecloses).
narrative_ontology:cs_reading_relation('9c606699-1350-4648-8fd3-7e74085f7400', federation_membership_kernel__welfare_coordination_reading, influences).
narrative_ontology:cs_axiom('9c606699-1350-4648-8fd3-7e74085f7400', foundational, free_movement_as_fundamental_citizenship_right).
narrative_ontology:cs_axiom_status(free_movement_as_fundamental_citizenship_right, holdable).
narrative_ontology:cs_axiom_grounding('9c606699-1350-4648-8fd3-7e74085f7400', free_movement_as_fundamental_citizenship_right, conventional).
narrative_ontology:cs_axiom('9c606699-1350-4648-8fd3-7e74085f7400', foundational, supranational_judicial_supremacy_over_national_labor_law).
narrative_ontology:cs_axiom_status(supranational_judicial_supremacy_over_national_labor_law, holdable).
narrative_ontology:cs_axiom_grounding('9c606699-1350-4648-8fd3-7e74085f7400', supranational_judicial_supremacy_over_national_labor_law, conventional).
narrative_ontology:cs_reference_frame('9c606699-1350-4648-8fd3-7e74085f7400', supranational_market_integration_mandate).
narrative_ontology:cs_drift_state('9c606699-1350-4648-8fd3-7e74085f7400', contemporary_expansive_jurisprudence, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('9c606699-1350-4648-8fd3-7e74085f7400', '').
narrative_ontology:cs_kernel_id(federation_membership_kernel__integration_reading, federation_membership_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(federation_membership_kernel__integration_reading, mobile_eu_workers).
narrative_ontology:constraint_beneficiary(federation_membership_kernel__integration_reading, cross_border_employers).
narrative_ontology:constraint_victim(federation_membership_kernel__integration_reading, displaced_local_labor).
narrative_ontology:constraint_victim(federation_membership_kernel__integration_reading, receiving_state_governments).
narrative_ontology:constraint_victim(federation_membership_kernel__integration_reading, sending_states).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interprets EU treaty free movement provisions expansively as fundamental rights of citizenship, issuing preliminary rulings and infringement judgments that override national labor market protections and welfare state closure rules. Its authority is self-reinforcing through the doctrine of supremacy.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__integration_reading, ecj, agenda_setter,
    institutional, generational, analytical, continental).

% Exercise rights to move and reside across member states, gaining equal treatment access to employment, social advantages, and welfare benefits in receiving states regardless of prior contributions. Their exit option is the mobility itself.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__integration_reading, mobile_eu_workers, beneficiary,
    moderate, biographical, mobile, continental).

% Access a expanded, flexible labor pool across borders with reduced regulatory fragmentation in hiring. Benefit from suppressed labor cost differentials and legal certainty under supranational enforcement.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__integration_reading, cross_border_employers, beneficiary,
    powerful, biographical, arbitrage, continental).

% Face wage compression and employment displacement in receiving state labor markets due to inbound mobility, without compensation mechanisms. Exit is limited by skills, language, and geographic immobility.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__integration_reading, displaced_local_labor, payer,
    powerless, biographical, constrained, regional).

% Administer welfare systems that must extend equal treatment to mobile workers under ECJ jurisprudence, bearing fiscal costs without compensatory transfers from sending states or the EU. National policy autonomy is overridden by supremacy.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__integration_reading, receiving_state_governments, payer,
    institutional, generational, constrained, national).

% Experience brain drain of skilled labor to higher-wage receiving states, eroding tax base and public investment returns, with no fiscal compensation or mobility-adjusted transfer mechanism under EU law.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__integration_reading, sending_states, payer,
    institutional, generational, constrained, national).

% Advocate for protective wages and restrictions on labor market entry but are structurally overridden by ECJ rulings that treat national protective legislation as obstacles to free movement.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__integration_reading, local_labor_unions, excluded,
    organized, biographical, constrained, national).

narrative_ontology:fixing_cost_class(federation_membership_kernel__integration_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the collective-action problem of fragmented national labor markets and protectionist closure by creating a single continental labor mobility space with standardized rights of entry, residence, and equal treatment, intended to complete the single market and optimize labor allocation.
% TRANSFER_FUNCTION: Moves labor mobility rights and equal treatment obligations from nationally bounded regimes to supranational judicial enforceability; transfers fiscal costs of mobile worker social protection from sending states and mobile workers themselves to receiving state welfare systems; transfers competitive labor-cost advantages from displaced local labor to cross-border employers and mobile workers.
% ABSENT_VOICES: Local labor unions and member state legislatures seeking to preserve protective labor market regulations and welfare state closure are structurally overridden by ECJ preliminary rulings; sending state governments concerned with brain drain are excluded from receiving state fiscal compensation mechanisms.
% DISAPPEARANCE_RATIONALE: If free movement as an expansive fundamental right vanished overnight, labor markets would renationalize, receiving state welfare systems would close equal treatment to non-contributory mobile worker claims, sending states would retain skilled labor, and the single market's labor mobility pillar would fragment along national boundaries.
% FOUNDING_PROBLEM: Post-war Europe's fragmented labor markets and national protectionism impeded economic recovery and cross-border cooperation; pooling labor mobility was intended to create allocative efficiencies and peace-through-interdependence.
% FOUNDING_PROBLEM_CORROBORATION: Economic historians corroborate the founding fragmentation problem; however, contemporary labor economists, constitutional lawyers, and member state governments outside the direct beneficiary set contest whether the current expansive interpretation into welfare equal treatment tracks the original economic coordination logic or has shifted toward extraction.
narrative_ontology:disappearance_verdict(federation_membership_kernel__integration_reading, world_rearranges).
narrative_ontology:founding_problem_status(federation_membership_kernel__integration_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(federation_membership_kernel__integration_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(federation_membership_kernel__integration_reading, 'none', 1).
narrative_ontology:epsilon_provenance(federation_membership_kernel__integration_reading, 0.72, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(federation_membership_kernel__integration_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(federation_membership_kernel__integration_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(federation_membership_kernel__integration_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.72) is high because the constraint systematically transfers fiscal and labor-market costs to receiving states, local workers, and sending states without compensatory mechanisms. Suppression (0.78) is higher still because the arrangement's persistence depends on ECJ supremacy actively overriding national protective legislation and welfare closure, not on voluntary coordination. Theater ratio (0.40) reflects a significant performative component: the discourse of 'fundamental citizenship rights' and 'market completion' masks an asymmetric cost structure where benefits concentrate and costs diffuse. Accessibility collapse (0.75) is high because EU legal supremacy means that once the constraint is legally cognized, national alternatives are structurally foreclosed. Resistance (0.68) reflects sustained political pushback from member states and labor unions, even as judicial enforcement hardens.
 *
 * PERSPECTIVAL GAP:
 *   The mobile worker seat computes the constraint as coordination delivering opportunity and equal treatment. The ECJ seat computes it as legal mandate and legitimate evolutionary interpretation. The displaced local labor, receiving state government, and sending state seats compute it as extractionâuncompensated cost-shifting and sovereignty loss. The engine derives this divergence from the same structural data: beneficiary role plus mobile exit produces low effective extraction for mobile workers, while payer role plus constrained exit produces high effective extraction for the victim seats.
 *
 * DIRECTIONALITY LOGIC:
 *   Mobile EU workers and cross-border employers are structurally positioned as beneficiaries: they collect rights, access, and cost advantages with low directionality. The ECJ, as agenda setter with analytical exit, sits near the beneficiary pole in terms of institutional self-interest in expansion. Displaced local labor, receiving state governments, and sending states are declared payers with constrained or trapped exit options, placing them near the full-target pole. The directionality derivation chain correctly maps the structural asymmetry without override.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problemâpost-war labor market fragmentationâhas been substantially addressed, yet the integration reading has expanded the constraint's scope from purely economic mobility into citizenship-based welfare equal treatment that was not part of the original Treaty design. This creates mandatrophy risk: the constraint persists and grows even as its original coordination rationale mutates. However, because concentrated beneficiaries (mobile workers, cross-border employers) continue to capture real gains and the ECJ actively enforces the expansion, the constraint has not decayed into a piton. It remains a tangled rope: a genuine coordination function (labor market integration) intertwined with asymmetric extraction (fiscal cost-shifting and sovereignty override).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    integration_reading_kernel_location,
    'This constraint is the integration_reading of the federation_membership_kernel. How would the victim and beneficiary map reconfigure if the member_sovereignty_reading or welfare_coordination_reading were adopted instead?',
    'Comparative legal analysis of the sibling readings'' stakeholder surfaces; tracking which seats enter or exit the victim set under alternative framings.',
    'Under member_sovereignty_reading, receiving state governments and local labor would shift toward beneficiary or symmetric positions while mobile workers would face constrained exit; under welfare_coordination_reading, fiscal extraction would be mitigated by compensatory transfers, altering receiving state directionality.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(integration_reading_kernel_location, conceptual, 'Kernel reading location and structural delta under sibling readings.').

omega_variable(
    welfare_equal_treatment_extraction_nature,
    'Do equal treatment obligations imposed on receiving state welfare systems constitute a necessary price of single market participation, or are they structurally extractive cost-shifting that exceeds the coordination benefit?',
    'Cross-jurisdictional fiscal incidence studies comparing welfare expenditures on mobile workers against their tax contributions and the macroeconomic gains from labor mobility.',
    'If expenditures systematically exceed contributions and gains, the constraint functions as extraction from receiving state taxpayers; if balanced, the cost is a genuine coordination price.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(welfare_equal_treatment_extraction_nature, empirical, 'Whether welfare equal treatment is coordination cost or extraction.').

omega_variable(
    ecj_expansion_institutional_interest,
    'Does the ECJ''s expansive interpretation reflect an autonomous legal logic inherent in the treaty text, or does it reflect institutional self-interest in maximizing supranational authority and docket relevance?',
    'Quantitative analysis of ECJ citation patterns and jurisprudential evolution alongside principal-agent models of judicial independence versus institutional aggrandizement.',
    'If institutional self-interest dominates, the interpretation layer functions partly as extraction from member state autonomy; if autonomous legal logic dominates, the expansion is a faithful implementation of the kernel.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(ecj_expansion_institutional_interest, conceptual, 'ECJ expansive interpretation motive ambiguity.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(federation_membership_kernel__integration_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(federation_membership_integration_tr_t0, federation_membership_kernel__integration_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(federation_membership_integration_tr_t8, federation_membership_kernel__integration_reading, theater_ratio, 8, 0.25).
narrative_ontology:measurement(federation_membership_integration_tr_t16, federation_membership_kernel__integration_reading, theater_ratio, 16, 0.3).
narrative_ontology:measurement(federation_membership_integration_tr_t24, federation_membership_kernel__integration_reading, theater_ratio, 24, 0.35).
narrative_ontology:measurement(federation_membership_integration_tr_t32, federation_membership_kernel__integration_reading, theater_ratio, 32, 0.38).
narrative_ontology:measurement(federation_membership_integration_tr_t40, federation_membership_kernel__integration_reading, theater_ratio, 40, 0.4).

% Extraction over time
narrative_ontology:measurement(federation_membership_integration_be_t0, federation_membership_kernel__integration_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(federation_membership_integration_be_t8, federation_membership_kernel__integration_reading, base_extractiveness, 8, 0.4).
narrative_ontology:measurement(federation_membership_integration_be_t16, federation_membership_kernel__integration_reading, base_extractiveness, 16, 0.5).
narrative_ontology:measurement(federation_membership_integration_be_t24, federation_membership_kernel__integration_reading, base_extractiveness, 24, 0.6).
narrative_ontology:measurement(federation_membership_integration_be_t32, federation_membership_kernel__integration_reading, base_extractiveness, 32, 0.68).
narrative_ontology:measurement(federation_membership_integration_be_t40, federation_membership_kernel__integration_reading, base_extractiveness, 40, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(federation_membership_integration_su_t0, federation_membership_kernel__integration_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(federation_membership_integration_su_t8, federation_membership_kernel__integration_reading, suppression_requirement, 8, 0.55).
narrative_ontology:measurement(federation_membership_integration_su_t16, federation_membership_kernel__integration_reading, suppression_requirement, 16, 0.65).
narrative_ontology:measurement(federation_membership_integration_su_t24, federation_membership_kernel__integration_reading, suppression_requirement, 24, 0.72).
narrative_ontology:measurement(federation_membership_integration_su_t32, federation_membership_kernel__integration_reading, suppression_requirement, 32, 0.76).
narrative_ontology:measurement(federation_membership_integration_su_t40, federation_membership_kernel__integration_reading, suppression_requirement, 40, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(federation_membership_kernel__integration_reading, resource_allocation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
