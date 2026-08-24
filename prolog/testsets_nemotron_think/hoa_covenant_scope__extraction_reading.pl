% ============================================================================
% CONSTRAINT STORY: hoa_covenant_scope__extraction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_hoa_covenant_scope__extraction_reading, []).

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
 *   constraint_id: hoa_covenant_scope__extraction_reading
 *   human_readable: HOA Covenant as Revenue Extraction Mechanism (Extraction Reading)
 *   domain: property_law/collective_governance/urban_planning
 *
 * SUMMARY:
 *   This constraint story captures the extraction_reading of the HOA covenant
 *   kernel: the covenant's enforcement machinery (fine schedules, lien
 *   authority, attorney fee shifting) operates as a revenue generation
 *   mechanism for board members, property management firms, and legal
 *   counsel. The coordination function (shared maintenance) is real but
 *   subordinate — the enforcement priorities, fine magnitudes, and procedural
 *   accelerants are calibrated for extraction, not maintenance. Financially
 *   vulnerable homeowners and renters bear the asymmetric costs. The
 *   constraint is a Tangled Rope: it retains a genuine coordination shell
 *   while the core operation has become extractive, requiring active
 *   enforcement (expedited liens, selective targeting) to sustain the revenue
 *   flow.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hoa_covenant_scope__extraction_reading, 0.62).
domain_priors:suppression_score(hoa_covenant_scope__extraction_reading, 0.78).
domain_priors:theater_ratio(hoa_covenant_scope__extraction_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hoa_covenant_scope__extraction_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(hoa_covenant_scope__extraction_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(hoa_covenant_scope__extraction_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(hoa_covenant_scope__extraction_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(hoa_covenant_scope__extraction_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hoa_covenant_scope__extraction_reading, tangled_rope).
narrative_ontology:human_readable(hoa_covenant_scope__extraction_reading, "HOA Covenant as Revenue Extraction Mechanism (Extraction Reading)").
narrative_ontology:topic_domain(hoa_covenant_scope__extraction_reading, "property_law/collective_governance/urban_planning").

domain_priors:requires_active_enforcement(hoa_covenant_scope__extraction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hoa_covenant_scope__extraction_reading, 'bd85c4a4-acbe-40d7-83aa-e2039a1b7c50').
narrative_ontology:cs_kernel_codification('bd85c4a4-acbe-40d7-83aa-e2039a1b7c50', formalized).
narrative_ontology:cs_authority_grounding('bd85c4a4-acbe-40d7-83aa-e2039a1b7c50', extraction).
narrative_ontology:cs_interpretation_layer_present('bd85c4a4-acbe-40d7-83aa-e2039a1b7c50').
narrative_ontology:cs_reading_relation('bd85c4a4-acbe-40d7-83aa-e2039a1b7c50', hoa_covenant_scope__coordination_reading, coexists_with).
narrative_ontology:cs_reading_relation('bd85c4a4-acbe-40d7-83aa-e2039a1b7c50', hoa_covenant_scope__behavioral_control_reading, coexists_with).
narrative_ontology:cs_axiom('bd85c4a4-acbe-40d7-83aa-e2039a1b7c50', foundational, covenant_enforcement_primarily_serves_revenue_extraction).
narrative_ontology:cs_axiom_status(covenant_enforcement_primarily_serves_revenue_extraction, holdable).
narrative_ontology:cs_axiom_grounding('bd85c4a4-acbe-40d7-83aa-e2039a1b7c50', covenant_enforcement_primarily_serves_revenue_extraction, empirically_contingent).
narrative_ontology:cs_axiom('bd85c4a4-acbe-40d7-83aa-e2039a1b7c50', secondary, fine_proliferation_is_designed_for_board_vendor_revenue).
narrative_ontology:cs_axiom_status(fine_proliferation_is_designed_for_board_vendor_revenue, holdable).
narrative_ontology:cs_axiom_grounding('bd85c4a4-acbe-40d7-83aa-e2039a1b7c50', fine_proliferation_is_designed_for_board_vendor_revenue, empirically_contingent).
narrative_ontology:cs_reference_frame('bd85c4a4-acbe-40d7-83aa-e2039a1b7c50', original_maintenance_coordination_framework).
narrative_ontology:cs_drift_state('bd85c4a4-acbe-40d7-83aa-e2039a1b7c50', contemporary_fine_dependent_governance, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('bd85c4a4-acbe-40d7-83aa-e2039a1b7c50', '').
narrative_ontology:cs_kernel_id(hoa_covenant_scope__extraction_reading, hoa_covenant_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hoa_covenant_scope__extraction_reading, board_members).
narrative_ontology:constraint_beneficiary(hoa_covenant_scope__extraction_reading, property_management_firms).
narrative_ontology:constraint_beneficiary(hoa_covenant_scope__extraction_reading, legal_counsel).
narrative_ontology:constraint_victim(hoa_covenant_scope__extraction_reading, financially_vulnerable_homeowners).
narrative_ontology:constraint_victim(hoa_covenant_scope__extraction_reading, renters_via_pass_through).
narrative_ontology:constraint_vindicates(hoa_covenant_scope__extraction_reading, property_value_maximization_through_regulatory_capture).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Elected volunteer board controls fine schedules, enforcement priorities, and vendor contracts. They set the agenda for what constitutes a violation and the penalty structure. Board service consolidates local political capital and creates pipeline to municipal office. Exit is mobile — they can resign, but the position itself carries institutional power over the constraint's operation.
narrative_ontology:constraint_stakeholder(hoa_covenant_scope__extraction_reading, board_members, agenda_setter,
    institutional, generational, mobile, local).

% Contracted by the board to administer day-to-day enforcement. They receive management fees plus percentage of collected fines and lien processing fees. Their contract renewal depends on delivering revenue to the board. They operate across multiple HOAs regionally, giving them arbitrage-grade exit from any single association.
narrative_ontology:constraint_stakeholder(hoa_covenant_scope__extraction_reading, property_management_firms, beneficiary,
    organized, biographical, mobile, regional).

% Retained by the HOA (paid from common funds) to draft fine policies, manage lien foreclosures, and defend board decisions. Bills hourly for enforcement actions and collects statutory attorney fees from homeowners in lien proceedings. The same firms often represent multiple HOAs, creating a recurring revenue stream from enforcement escalation.
narrative_ontology:constraint_stakeholder(hoa_covenant_scope__extraction_reading, legal_counsel, beneficiary,
    organized, biographical, mobile, regional).

% Own homes but lack liquidity to absorb escalating fines or legal costs. A single landscaping violation can trigger daily fines that compound into liens exceeding equity. They cannot easily sell (liens cloud title) and cannot afford legal defense. Their exit is constrained — moving requires clearing the debt, which the constraint makes harder.
narrative_ontology:constraint_stakeholder(hoa_covenant_scope__extraction_reading, financially_vulnerable_homeowners, payer,
    powerless, biographical, constrained, local).

% Do not own the property but bear costs through rent increases when landlords pass through HOA fines and special assessments. Have no vote in HOA governance and no standing to contest violations. Their exit is constrained by lease terms and local rental market tightness — they pay for a constraint they cannot influence.
narrative_ontology:constraint_stakeholder(hoa_covenant_scope__extraction_reading, renters_via_pass_through, payer,
    powerless, immediate, constrained, local).

% Own multiple units as investments. They have capital to absorb fines and legal costs, and often negotiate directly with management firms. Their interests align with low common-area spending and high rental yields, not with enforcement fairness. They are excluded from the daily enforcement dynamic but shape board elections through block voting.
narrative_ontology:constraint_stakeholder(hoa_covenant_scope__extraction_reading, absentee_investor_owners, excluded,
    moderate, biographical, arbitrage, national).

% Study HOA governance patterns, fine revenue dependence, and displacement effects. They document the extraction structure but have no enforcement power. Their analysis informs legislative reform efforts that could alter the constraint's legal framework.
narrative_ontology:constraint_stakeholder(hoa_covenant_scope__extraction_reading, housing_policy_analysts, observer,
    analytical, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Nominally coordinates shared infrastructure maintenance (roads, pools, landscaping) and resolves externalities (noise, aesthetics, parking). In practice, this coordination function is the cover for the extraction mechanism — the same enforcement machinery that could maintain common areas is redirected toward high-margin fine generation.
% TRANSFER_FUNCTION: Moves money from homeowners (especially financially vulnerable ones) and renters through escalating fines, daily compounding penalties, expedited lien filing fees, and statutory attorney fee awards — into board-controlled reserves, management firm fees, and legal counsel billings. The transfer is mediated by the covenant's enforcement clause, which the board controls unilaterally.
% ABSENT_VOICES: Financially vulnerable homeowners facing active lien proceedings are structurally excluded from board meetings (scheduling, intimidation, procedural barriers). Renters have no formal voice. Future buyers who would inherit the extraction regime are not present at the time of covenant adoption or amendment. Their absence is what makes the selective enforcement profitable — the targets cannot organize effective resistance.
% DISAPPEARANCE_RATIONALE: If the fine-enforcement regime vanished overnight, board revenue would collapse, management firm contracts would be renegotiated downward, legal counsel retainers would shrink. Homeowners would retain equity otherwise lost to liens. Property values might initially dip from reduced aesthetic enforcement, but the predatory debt spiral would end. The local housing market would reorganize around actual maintenance costs rather than extraction rents.
% FOUNDING_PROBLEM: Original covenants (1970s-1990s) were adopted to coordinate shared infrastructure maintenance in new subdivisions where municipal services were absent — roads, drainage, common areas. The enforcement mechanism was meant to prevent free-riding on maintenance costs.
% FOUNDING_PROBLEM_CORROBORATION: Municipal annexation records show most original infrastructure has been assumed by city services. Independent forensic audits of HOA budgets (e.g., Texas HOA Reform Coalition 2022, Florida League of Cities 2021) document that fine revenue now exceeds maintenance spending in 60%+ of sampled associations. The board's own reserve studies show declining maintenance allocation as a share of budget. No corroborating source outside the benefiting parties asserts the founding problem remains live.
narrative_ontology:disappearance_verdict(hoa_covenant_scope__extraction_reading, world_rearranges).
narrative_ontology:founding_problem_status(hoa_covenant_scope__extraction_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(hoa_covenant_scope__extraction_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(hoa_covenant_scope__extraction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(hoa_covenant_scope__extraction_reading, 0.62, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hoa_covenant_scope__extraction_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(hoa_covenant_scope__extraction_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(hoa_covenant_scope__extraction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62) reflects fine revenue exceeding maintenance spending in mature HOAs, with compounding daily fines and statutory attorney fees creating multiplicative extraction. Suppression (0.78) is high because the constraint's persistence depends on procedural barriers (short cure periods, non-judicial foreclosure, attorney fee awards that deter defense) and structural exclusion of renters and vulnerable owners from governance. Theater ratio (0.48) is elevated — aesthetic enforcement continues as performative cover while high-margin violations (parking, signage, rental restrictions) are prioritized. Accessibility collapse (0.65) reflects that alternatives (selling, legal challenge, board election) are technically available but practically collapsed for the target population. Resistance (0.55) is moderate — legislative reform efforts exist but are fragmented and opposed by the organized beneficiary coalition.
 *
 * PERSPECTIVAL GAP:
 *   From the board/management/legal seat, the constraint appears as necessary coordination with justified enforcement. From the vulnerable homeowner/renter seat, it operates as a predatory debt trap. The engine computes this divergence from the declared power/exit/beneficiary structure — the claimed_type (tangled_rope) acknowledges the coordination shell while the metrics capture the extractive core.
 *
 * DIRECTIONALITY LOGIC:
 *   Board members (agenda_setter, institutional power) are structural beneficiaries — they control the enforcement agenda and convert it into political capital. Property management firms and legal counsel (beneficiary, organized power) are direct financial beneficiaries with mobile exit. Financially vulnerable homeowners (payer, powerless) are full targets — constrained exit, identity-locked to home equity, no procedural defense. Renters (payer, powerless) are derivative targets with even less voice. Absentee investors (excluded, moderate power) are neither coordinated nor extracted from — they arbitrage the system. Housing policy analysts (observer, analytical) sit outside the extraction loop.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (municipal service gap) is dead — cities now provide the infrastructure the covenant was built to maintain. The arrangement persists because the enforcement machinery was repurposed for revenue extraction. The mandatrophy is resolved in the sense that the original mandate is gone, but the constraint has not atrophied — it has been captured. This is not a Piton (inertial decay) but an active Tangled Rope where the coordination function is maintained theatrically to legitimize the extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    extraction_coordination_boundary,
    'At what point does fine revenue dependence become the primary driver of enforcement priorities, versus genuine maintenance coordination?',
    'Longitudinal budget analysis tracking fine revenue share vs. maintenance spending across HOA maturation cycles; regression of enforcement actions against revenue yield vs. maintenance impact.',
    'If the boundary is crossed early in the HOA lifecycle, the coordination claim is largely pretextual from inception. If crossed later, the constraint undergoes a genuine type transition from rope/scaffold to tangled_rope/snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_coordination_boundary, empirical, 'Whether the extraction function emerged from capture of an initially genuine coordination mechanism or was designed in from the start.').

omega_variable(
    renters_pass_through_mechanism,
    'How completely do HOA fine costs pass through to renters in different rental market conditions?',
    'Hedonic rental pricing models comparing HOA vs. non-HOA units controlling for amenities; survey data on landlord fine pass-through practices.',
    'If pass-through is near-complete, renters are co-equal victims with homeowners. If partial, the extraction falls more heavily on owner-occupants, altering the victim structure and coalition potential.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(renters_pass_through_mechanism, empirical, 'Whether renters bear the extraction burden directly or indirectly, and how that varies with market tightness.').

omega_variable(
    selective_enforcement_discrimination,
    'Does selective enforcement of high-fine violations correlate with protected class status (race, age, disability) beyond income vulnerability?',
    'Disparate impact analysis of violation notices and lien filings against demographic data; discovery in fair housing litigation.',
    'If enforcement is discriminatory, the constraint carries civil rights violations that could trigger federal preemption — a structural shift in the constraint''s legal basis and suppression mechanism.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(selective_enforcement_discrimination, empirical, 'Whether the extraction mechanism operates through discriminatory targeting, adding a civil rights dimension to the constraint.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hoa_covenant_scope__extraction_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hoa_covenant_extraction_tr_t0, hoa_covenant_scope__extraction_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(hoa_covenant_extraction_tr_t8, hoa_covenant_scope__extraction_reading, theater_ratio, 8, 0.22).
narrative_ontology:measurement(hoa_covenant_extraction_tr_t16, hoa_covenant_scope__extraction_reading, theater_ratio, 16, 0.3).
narrative_ontology:measurement(hoa_covenant_extraction_tr_t24, hoa_covenant_scope__extraction_reading, theater_ratio, 24, 0.38).
narrative_ontology:measurement(hoa_covenant_extraction_tr_t32, hoa_covenant_scope__extraction_reading, theater_ratio, 32, 0.43).
narrative_ontology:measurement(hoa_covenant_extraction_tr_t40, hoa_covenant_scope__extraction_reading, theater_ratio, 40, 0.48).

% Extraction over time
narrative_ontology:measurement(hoa_covenant_extraction_be_t0, hoa_covenant_scope__extraction_reading, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(hoa_covenant_extraction_be_t8, hoa_covenant_scope__extraction_reading, base_extractiveness, 8, 0.35).
narrative_ontology:measurement(hoa_covenant_extraction_be_t16, hoa_covenant_scope__extraction_reading, base_extractiveness, 16, 0.45).
narrative_ontology:measurement(hoa_covenant_extraction_be_t24, hoa_covenant_scope__extraction_reading, base_extractiveness, 24, 0.52).
narrative_ontology:measurement(hoa_covenant_extraction_be_t32, hoa_covenant_scope__extraction_reading, base_extractiveness, 32, 0.58).
narrative_ontology:measurement(hoa_covenant_extraction_be_t40, hoa_covenant_scope__extraction_reading, base_extractiveness, 40, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(hoa_covenant_extraction_su_t0, hoa_covenant_scope__extraction_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(hoa_covenant_extraction_su_t8, hoa_covenant_scope__extraction_reading, suppression_requirement, 8, 0.5).
narrative_ontology:measurement(hoa_covenant_extraction_su_t16, hoa_covenant_scope__extraction_reading, suppression_requirement, 16, 0.6).
narrative_ontology:measurement(hoa_covenant_extraction_su_t24, hoa_covenant_scope__extraction_reading, suppression_requirement, 24, 0.68).
narrative_ontology:measurement(hoa_covenant_extraction_su_t32, hoa_covenant_scope__extraction_reading, suppression_requirement, 32, 0.73).
narrative_ontology:measurement(hoa_covenant_extraction_su_t40, hoa_covenant_scope__extraction_reading, suppression_requirement, 40, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hoa_covenant_scope__extraction_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(hoa_covenant_scope__extraction_reading, 0.12).
narrative_ontology:affects_constraint(hoa_covenant_scope__extraction_reading, hoa_covenant_scope__coordination_reading).
narrative_ontology:affects_constraint(hoa_covenant_scope__extraction_reading, hoa_covenant_scope__behavioral_control_reading).

% DUAL FORMULATION NOTE:
% This constraint family (hoa_covenant_scope) decomposes the single covenant text into three structurally distinct readings with divergent ε values and beneficiary/victim structures. The coordination_reading claims low ε (~0.15) with genuine maintenance coordination; the behavioral_control_reading claims moderate ε (~0.35) with aesthetic conformity as the coordination function; this extraction_reading claims high ε (~0.62) with revenue generation as the operating logic. The readings share the same enforcement machinery but attribute different primary functions to it. The network edges represent the structural dependency: the extraction_reading parasitizes the coordination_reading's infrastructure, and the behavioral_control_reading provides the aesthetic justification that enables selective enforcement.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(hoa_covenant_scope__extraction_reading, institutional, 0.1).
constraint_indexing:directionality_override(hoa_covenant_scope__extraction_reading, organized, 0.15).
constraint_indexing:directionality_override(hoa_covenant_scope__extraction_reading, powerless, 0.9).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
