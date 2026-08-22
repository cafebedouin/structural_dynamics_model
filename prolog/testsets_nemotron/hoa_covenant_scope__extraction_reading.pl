% ============================================================================
% CONSTRAINT STORY: hoa_covenant_scope__extraction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: hoa_covenant_scope__extraction_reading
 *   human_readable: HOA Covenant Enforcement as Revenue Extraction Mechanism
 *   domain: property_law/collective_governance/urban_planning
 *
 * SUMMARY:
 *   HOA covenant enforcement operates as a tangled rope: it provides genuine
 *   coordination for shared infrastructure (the coordination function that
 *   makes the arrangement defensible) while simultaneously extracting revenue
 *   through escalating fine schedules, expedited lien authority, and attorney
 *   fee shifting that disproportionately targets financially vulnerable
 *   homeowners. The extraction reading instantiates this constraint as a
 *   mechanism where board members, property management firms, and legal
 *   counsel structurally benefit from enforcement intensity that exceeds
 *   coordination needs. The constraint persists because the coordination
 *   function is real (common areas, shared walls, stormwater systems) but the
 *   enforcement regime has been captured by parties who profit from its most
 *   extractive configurations.
 *
 * KEY AGENTS:
 *   - board_members: Primary beneficiary (institutional/constrained) — control enforcement discretion and fine schedules
 *   - property_management_firms: Primary beneficiary (institutional/mobile) — retain management contracts tied to enforcement revenue streams
 *   - legal_counsel: Primary beneficiary (organized/mobile) — collect attorney fees shifted to violators via covenant provisions
 *   - financially_vulnerable_homeowners: Primary victim (powerless/trapped) — bear disproportionate extraction with limited exit
 *   - renters_via_pass_through: Secondary victim (powerless/trapped) — absorb costs through rent increases with zero governance voice
 *   - long_term_owners: Secondary beneficiary/payer (moderate/constrained) — gain coordination benefits but pay escalating assessments
 *   - reform_candidates: Excluded (moderate/constrained) — would challenge extraction but face structural barriers to board access
 *   - analytical_observer: Observer (analytical/analytical) — sees full structure
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hoa_covenant_scope__extraction_reading, 0.62).
domain_priors:suppression_score(hoa_covenant_scope__extraction_reading, 0.71).
domain_priors:theater_ratio(hoa_covenant_scope__extraction_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hoa_covenant_scope__extraction_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(hoa_covenant_scope__extraction_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(hoa_covenant_scope__extraction_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(hoa_covenant_scope__extraction_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(hoa_covenant_scope__extraction_reading, resistance, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hoa_covenant_scope__extraction_reading, tangled_rope).
narrative_ontology:human_readable(hoa_covenant_scope__extraction_reading, "HOA Covenant Enforcement as Revenue Extraction Mechanism").
narrative_ontology:topic_domain(hoa_covenant_scope__extraction_reading, "property_law/collective_governance/urban_planning").

domain_priors:requires_active_enforcement(hoa_covenant_scope__extraction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hoa_covenant_scope__extraction_reading, '77fedeba-2cf1-4e66-968a-4c9e58d7a0c3').
narrative_ontology:cs_kernel_codification('77fedeba-2cf1-4e66-968a-4c9e58d7a0c3', fixed_text).
narrative_ontology:cs_authority_grounding('77fedeba-2cf1-4e66-968a-4c9e58d7a0c3', extraction).
narrative_ontology:cs_interpretation_layer_present('77fedeba-2cf1-4e66-968a-4c9e58d7a0c3').
narrative_ontology:cs_reading_relation('77fedeba-2cf1-4e66-968a-4c9e58d7a0c3', hoa_covenant_scope__coordination_reading, coexists_with).
narrative_ontology:cs_reading_relation('77fedeba-2cf1-4e66-968a-4c9e58d7a0c3', hoa_covenant_scope__behavioral_control_reading, influences).
narrative_ontology:cs_axiom('77fedeba-2cf1-4e66-968a-4c9e58d7a0c3', foundational, enforcement_intensity_tracks_revenue_not_harm).
narrative_ontology:cs_axiom_status(enforcement_intensity_tracks_revenue_not_harm, holdable).
narrative_ontology:cs_axiom_grounding('77fedeba-2cf1-4e66-968a-4c9e58d7a0c3', enforcement_intensity_tracks_revenue_not_harm, empirically_contingent).
narrative_ontology:cs_axiom('77fedeba-2cf1-4e66-968a-4c9e58d7a0c3', foundational, board_discretion_is_captured_by_fee_beneficiaries).
narrative_ontology:cs_axiom_status(board_discretion_is_captured_by_fee_beneficiaries, holdable).
narrative_ontology:cs_axiom_grounding('77fedeba-2cf1-4e66-968a-4c9e58d7a0c3', board_discretion_is_captured_by_fee_beneficiaries, empirically_contingent).
narrative_ontology:cs_reference_frame('77fedeba-2cf1-4e66-968a-4c9e58d7a0c3', original_developer_covenants).
narrative_ontology:cs_drift_state('77fedeba-2cf1-4e66-968a-4c9e58d7a0c3', contemporary_enforcement_regime, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('77fedeba-2cf1-4e66-968a-4c9e58d7a0c3', '').
narrative_ontology:cs_kernel_id(hoa_covenant_scope__extraction_reading, hoa_covenant_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hoa_covenant_scope__extraction_reading, board_members).
narrative_ontology:constraint_beneficiary(hoa_covenant_scope__extraction_reading, property_management_firms).
narrative_ontology:constraint_beneficiary(hoa_covenant_scope__extraction_reading, legal_counsel).
narrative_ontology:constraint_victim(hoa_covenant_scope__extraction_reading, financially_vulnerable_homeowners).
narrative_ontology:constraint_victim(hoa_covenant_scope__extraction_reading, renters_via_pass_through).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(hoa_covenant_scope__extraction_reading, long_term_owners).
narrative_ontology:constraint_victim(hoa_covenant_scope__extraction_reading, long_term_owners).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Elected by homeowners but incumbent advantage, quorum rules, and procedural control make capture self-reinforcing. Set fine schedules, select management firms, direct legal counsel. Benefit from enforcement revenue that funds community improvements they control and from the political capital of 'protecting property values.' Exit requires selling the home — constrained by equity loss if they leave the community.
narrative_ontology:constraint_stakeholder(hoa_covenant_scope__extraction_reading, board_members, agenda_setter,
    institutional, biographical, constrained, local).

% Contracted by HOA boards to administer enforcement. Revenue scales with enforcement intensity (inspection frequency, violation processing, lien management). Firms compete for contracts by promising 'aggressive enforcement' and 'revenue optimization.' Can exit by losing/not renewing a contract — mobile across HOA clients but dependent on the enforcement-revenue model industry-wide.
narrative_ontology:constraint_stakeholder(hoa_covenant_scope__extraction_reading, property_management_firms, beneficiary,
    institutional, biographical, mobile, regional).

% HOA-specialized law firms collect attorney fees shifted to homeowners via covenant provisions and state statutes. Fees accrue per violation notice, lien filing, and foreclosure action. Mobile across clients; the HOA enforcement niche is a recognized practice area. Exit is easy — but the practice area exists because the fee-shifting structure makes enforcement litigation profitable.
narrative_ontology:constraint_stakeholder(hoa_covenant_scope__extraction_reading, legal_counsel, beneficiary,
    organized, biographical, mobile, regional).

% Homeowners with limited liquidity, fixed incomes, or negative equity. Minor violations (lawn height, paint color, rental of a room) trigger fines that compound with interest and attorney fees. Expedited lien and non-judicial foreclosure processes mean the home can be lost for debts originating in $50 fines. Legal representation is unaffordable; procedural defenses are technically available but practically inaccessible. Selling is the only exit but equity is consumed by the lien.
narrative_ontology:constraint_stakeholder(hoa_covenant_scope__extraction_reading, financially_vulnerable_homeowners, payer,
    powerless, immediate, trapped, local).

% Renters have no vote in HOA governance, no notice of hearings, no standing to contest violations. Landlords pass through HOA assessments, fines, and legal costs via rent increases and lease terms. A renter can be evicted for the landlord's HOA violations (unauthorized occupant, noise, parking). Exit means moving — costly and disruptive, with no guarantee the next community isn't also HOA-governed.
narrative_ontology:constraint_stakeholder(hoa_covenant_scope__extraction_reading, renters_via_pass_through, payer,
    powerless, immediate, trapped, local).

% Owners with significant equity and community roots. Benefit from coordinated maintenance, shared amenities, and dispute resolution (the genuine coordination function). Also pay escalating assessments, special assessments for litigation, and face the same fine exposure. Constrained exit: selling means leaving a community they've invested in; staying means absorbing extraction. Some become reform candidates but face structural barriers.
narrative_ontology:constraint_stakeholder(hoa_covenant_scope__extraction_reading, long_term_owners, beneficiary,
    moderate, biographical, constrained, local).
narrative_ontology:stakeholder_secondary_role(hoa_covenant_scope__extraction_reading, long_term_owners, payer).

% Homeowners who would run for board on platforms of proportional enforcement, fine caps, fee-shifting repeal. Barred by: quorum requirements that favor incumbents, notice periods controlled by sitting boards, legal challenges to candidacy funded by HOA coffers, and social ostracism. Their exclusion is what sustains the extraction regime — if they could compete, the enforcement intensity would face democratic discipline.
narrative_ontology:constraint_stakeholder(hoa_covenant_scope__extraction_reading, reform_candidates, excluded,
    moderate, biographical, constrained, local).

% Researcher, journalist, or policy analyst studying HOA governance as a structural phenomenon. Sees the full beneficiary/victim architecture, the coordination/extraction hybrid, and the institutional dynamics. No stake in the outcome; exit is trivial (stop studying). The analytical seat is the only one with full structural visibility.
narrative_ontology:constraint_stakeholder(hoa_covenant_scope__extraction_reading, analytical_observer, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates shared infrastructure maintenance (roads, stormwater, common buildings, pool/clubhouse), resolves genuine externalities (noise, parking, hazardous conditions), and provides a collective governance framework for common-interest communities.
% TRANSFER_FUNCTION: Moves money from homeowners (via fines, assessments, attorney fees, lien costs) to board-controlled improvement funds, property management firms (management fees scaled to enforcement), and legal counsel (shifted attorney fees). Renters pay indirectly through rent pass-through. The transfer is justified as 'enforcement' but the rate structure and fee-shifting provisions make it revenue-generating.
% ABSENT_VOICES: Renters (zero governance standing), financially vulnerable homeowners (practically excluded by cost of participation), reform candidates (structurally barred from ballot access), and future buyers (inherit the regime without consent). They would object to fine proliferation, fee shifting, and expedited liens but are not in the room where enforcement policy is set.
% DISAPPEARANCE_RATIONALE: If the enforcement regime vanished overnight: shared infrastructure would still need maintenance (coordination function persists) but fine revenue, lien fees, and attorney fee streams would disappear. Boards would lose discretionary funding; management firms would lose enforcement-revenue contracts; legal counsel would lose a practice area. Homeowners would face lower costs but also weaker enforcement of genuine externalities. The community would reorganize around either a leaner coordination-only model or a new capture cycle.
% FOUNDING_PROBLEM: Post-WWII suburban development created common-interest communities with shared infrastructure (streets, drainage, recreation) that no single homeowner could maintain and municipal governments would not adopt. Covenants provided the legal framework for mandatory assessments and collective decision-making.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem (shared infrastructure coordination) is corroborated by municipal finance records showing cities routinely refuse to adopt subdivision infrastructure, by engineering standards for stormwater/common-area maintenance, and by the universal adoption of HOA structures in new suburban development since the 1960s. The extraction_reading does not dispute the founding problem is live — it argues the enforcement regime has accumulated extractive layers that exceed coordination needs.
narrative_ontology:disappearance_verdict(hoa_covenant_scope__extraction_reading, world_rearranges).
narrative_ontology:founding_problem_status(hoa_covenant_scope__extraction_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(hoa_covenant_scope__extraction_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
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
 *   Base extractiveness 0.62 reflects that fine revenue, lien processing fees, and shifted attorney fees substantially exceed the marginal cost of coordination services provided. Suppression 0.71 captures the structural power of expedited liens, non-judicial foreclosure pathways, and the practical impossibility of opting out of the covenant regime without selling. Theater ratio 0.42 indicates that a growing share of enforcement activity targets high-revenue violation categories (architectural review fines, rental restrictions) rather than genuine externality resolution. Accessibility collapse 0.58: alternatives exist legally (amendment, litigation, political capture) but are practically foreclosed for vulnerable owners. Resistance 0.48: organized resistance exists but is fragmented and routinely defeated by procedural advantages held by the enforcement apparatus.
 *
 * PERSPECTIVAL GAP:
 *   From the board/management seat the constraint appears as necessary coordination with proportionate enforcement. From the vulnerable homeowner seat it operates as a predatory extraction mechanism with no meaningful exit. From the renter seat the constraint is invisible until costs pass through. The engine computes these divergences from the declared power/exit/spatial_scope structure — the author does not assign types to seats.
 *
 * DIRECTIONALITY LOGIC:
 *   Board members, management firms, and legal counsel are declared beneficiaries because they structurally collect from the enforcement regime (fine revenue, management fees, attorney fees). Financially vulnerable homeowners and renters are declared victims because they bear the extraction with the least exit capacity. Long-term owners sit near symmetric: they benefit from coordination but pay escalating costs. Renters have zero governance voice and trapped exit — their extraction is structural, not incidental. Reform candidates are excluded from the agenda-setting process by procedural barriers (quorum requirements, notice periods, incumbent advantage).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (shared infrastructure coordination) remains live but the enforcement regime has accumulated extractive layers (fine proliferation, expedited liens, fee shifting) that far exceed coordination needs. The mandate has not atrophied — coordination is still needed — but the enforcement mechanism has been captured. This is mandatrophy in the capture sense: the arrangement persists because the coordination function legitimizes the extractive superstructure. Classification as tangled_rope (not snare) correctly preserves the genuine coordination function while flagging the asymmetric extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    extraction_coordination_separability,
    'Can the genuine coordination function (shared infrastructure maintenance, externality resolution) be operationally separated from the extractive enforcement regime (fine schedules, lien acceleration, fee shifting)?',
    'Natural experiment from HOAs that adopted proportional fine schedules and eliminated attorney fee shifting: if coordination outcomes (maintenance quality, dispute resolution) remain stable while extraction metrics drop, the functions are separable.',
    'If separable, the constraint is a tangled rope with a clean coordination core and extractive overlay — reform can target extraction without losing coordination. If inseparable, the coordination function itself may be a loss leader for the extraction regime, making the constraint more snare-like.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(extraction_coordination_separability, conceptual, 'Whether coordination and extraction are structurally separable in HOA governance').

omega_variable(
    renter_exploitation_mechanism,
    'Does the pass-through of HOA costs to renters operate as an intended extraction channel (landlords as collection agents) or an incidental side effect?',
    'Comparative analysis of rental markets in HOA vs. non-HOA communities controlling for housing quality: if rents are systematically higher in HOA communities net of amenities, and lease terms explicitly reference HOA fee pass-through, the mechanism is structural.',
    'If structural, renters are direct victims of the extraction regime despite having zero governance standing — strengthening the snare-like character. If incidental, renters are collateral damage of a homeowner-targeted regime.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(renter_exploitation_mechanism, empirical, 'Whether renter cost pass-through is a designed feature of the extraction architecture').

omega_variable(
    kernel_reading_ontology,
    'Is the extraction_reading a distinct constraint with its own ε, or an interpretive lens on the same constraint instantiated by the coordination_reading?',
    'Apply the ε-invariance test: if measuring the constraint through the coordination lens (maintenance assessments, dispute resolution) yields a different ε than measuring through the extraction lens (fine revenue, lien fees, attorney fees), they are distinct constraints.',
    'If distinct, the kernel decomposition is valid — three constraint stories linked by network.affects_constraints. If not distinct, the readings are observational perspectives on one constraint and should be modeled as stakeholder seats within a single story.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_ontology, conceptual, 'Whether the kernel''s readings instantiate distinct constraints or observational perspectives').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hoa_covenant_scope__extraction_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hoa_covenant_extraction_tr_t0, hoa_covenant_scope__extraction_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement_basis(hoa_covenant_extraction_tr_t0, observed).
narrative_ontology:measurement(hoa_covenant_extraction_tr_t5, hoa_covenant_scope__extraction_reading, theater_ratio, 5, 0.26).
narrative_ontology:measurement_basis(hoa_covenant_extraction_tr_t5, observed).
narrative_ontology:measurement(hoa_covenant_extraction_tr_t10, hoa_covenant_scope__extraction_reading, theater_ratio, 10, 0.33).
narrative_ontology:measurement_basis(hoa_covenant_extraction_tr_t10, observed).
narrative_ontology:measurement(hoa_covenant_extraction_tr_t15, hoa_covenant_scope__extraction_reading, theater_ratio, 15, 0.38).
narrative_ontology:measurement_basis(hoa_covenant_extraction_tr_t15, observed).
narrative_ontology:measurement(hoa_covenant_extraction_tr_t20, hoa_covenant_scope__extraction_reading, theater_ratio, 20, 0.4).
narrative_ontology:measurement_basis(hoa_covenant_extraction_tr_t20, observed).
narrative_ontology:measurement(hoa_covenant_extraction_tr_t25, hoa_covenant_scope__extraction_reading, theater_ratio, 25, 0.42).
narrative_ontology:measurement_basis(hoa_covenant_extraction_tr_t25, observed).

% Extraction over time
narrative_ontology:measurement(hoa_covenant_extraction_be_t0, hoa_covenant_scope__extraction_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement_basis(hoa_covenant_extraction_be_t0, observed).
narrative_ontology:measurement(hoa_covenant_extraction_be_t5, hoa_covenant_scope__extraction_reading, base_extractiveness, 5, 0.44).
narrative_ontology:measurement_basis(hoa_covenant_extraction_be_t5, observed).
narrative_ontology:measurement(hoa_covenant_extraction_be_t10, hoa_covenant_scope__extraction_reading, base_extractiveness, 10, 0.52).
narrative_ontology:measurement_basis(hoa_covenant_extraction_be_t10, observed).
narrative_ontology:measurement(hoa_covenant_extraction_be_t15, hoa_covenant_scope__extraction_reading, base_extractiveness, 15, 0.58).
narrative_ontology:measurement_basis(hoa_covenant_extraction_be_t15, observed).
narrative_ontology:measurement(hoa_covenant_extraction_be_t20, hoa_covenant_scope__extraction_reading, base_extractiveness, 20, 0.6).
narrative_ontology:measurement_basis(hoa_covenant_extraction_be_t20, observed).
narrative_ontology:measurement(hoa_covenant_extraction_be_t25, hoa_covenant_scope__extraction_reading, base_extractiveness, 25, 0.62).
narrative_ontology:measurement_basis(hoa_covenant_extraction_be_t25, observed).

% Suppression requirement over time
narrative_ontology:measurement(hoa_covenant_extraction_su_t0, hoa_covenant_scope__extraction_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement_basis(hoa_covenant_extraction_su_t0, observed).
narrative_ontology:measurement(hoa_covenant_extraction_su_t5, hoa_covenant_scope__extraction_reading, suppression_requirement, 5, 0.6).
narrative_ontology:measurement_basis(hoa_covenant_extraction_su_t5, observed).
narrative_ontology:measurement(hoa_covenant_extraction_su_t10, hoa_covenant_scope__extraction_reading, suppression_requirement, 10, 0.65).
narrative_ontology:measurement_basis(hoa_covenant_extraction_su_t10, observed).
narrative_ontology:measurement(hoa_covenant_extraction_su_t15, hoa_covenant_scope__extraction_reading, suppression_requirement, 15, 0.68).
narrative_ontology:measurement_basis(hoa_covenant_extraction_su_t15, observed).
narrative_ontology:measurement(hoa_covenant_extraction_su_t20, hoa_covenant_scope__extraction_reading, suppression_requirement, 20, 0.7).
narrative_ontology:measurement_basis(hoa_covenant_extraction_su_t20, observed).
narrative_ontology:measurement(hoa_covenant_extraction_su_t25, hoa_covenant_scope__extraction_reading, suppression_requirement, 25, 0.71).
narrative_ontology:measurement_basis(hoa_covenant_extraction_su_t25, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hoa_covenant_scope__extraction_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(hoa_covenant_scope__extraction_reading, 0.18).
narrative_ontology:affects_constraint(hoa_covenant_scope__extraction_reading, hoa_covenant_scope__coordination_reading).
narrative_ontology:affects_constraint(hoa_covenant_scope__extraction_reading, hoa_covenant_scope__behavioral_control_reading).

% DUAL FORMULATION NOTE:
% This constraint (extraction_reading) and its siblings (coordination_reading, behavioral_control_reading) form the hoa_covenant_scope kernel family. The coordination_reading models the genuine shared-infrastructure coordination function (low ε, rope-like). The behavioral_control_reading models aesthetic/behavioral conformity enforcement (moderate ε, tangled_rope or snare depending on enforcement intensity). The extraction_reading models the revenue/power consolidation mechanism (high ε, tangled_rope). All three share the same covenant text and enforcement apparatus but instantiate different constraints with different ε, different beneficiary/victim structures, and different classifications. They are linked via network.affects_constraints because the coordination function legitimizes the enforcement apparatus that the other readings exploit.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(hoa_covenant_scope__extraction_reading, institutional, 0.15).
constraint_indexing:directionality_override(hoa_covenant_scope__extraction_reading, powerless, 0.92).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
