% ============================================================================
% CONSTRAINT STORY: caa_section_111d_delegation__systemic_transformation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_caa_section_111d_delegation__systemic_transformation_reading, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: caa_section_111d_delegation__systemic_transformation_reading
 *   human_readable: Section 111(d) 'Best System' as Grid-Wide Generation-Shifting Mandate
 *   domain: administrative_law/environmental_regulation/constitutional_interpretation
 *
 * SUMMARY:
 *   Section 111(d) of the Clean Air Act grants EPA authority to define the
 *   'best system of emission reduction' for existing power plants. This
 *   constraint story instantiates ONE READING of this contested provision:
 *   the systemic-transformation reading, under which 'best system' authorizes
 *   EPA to mandate grid-wide generation-shifting strategies—renewable
 *   substitution, coal retirement, and state-level decarbonization
 *   pathways—rather than limiting EPA to facility-level measures. The rival
 *   reading (facility_constraint_reading) interprets 'best system' narrowly
 *   to on-site technical improvements. These readings are not different
 *   observations of the same constraint; they instantiate genuinely different
 *   constraints with different ε values, different victim sets, and different
 *   beneficiary structures. This file models the systemic-transformation
 *   reading as a tangled rope: it coordinates a collective decarbonization
 *   goal while extracting substantially from coal-sector incumbents and
 *   coal-dependent states. The constraint's persistence depends on active
 *   enforcement—EPA must defend the interpretation against judicial challenge
 *   and against political pressure to narrow it.
 *
 * KEY AGENTS:
 *   - EPA regulatory authority: sets the interpretation and enforces compliance; collects no direct benefit but holds institutional authority
 *   - renewable energy producers: primary beneficiaries; gain mandated procurement demand
 *   - coal sector incumbents: primary victims; face stranded assets and demand destruction
 *   - coal-dependent states: secondary victims; bear transition cost and identity friction
 *   - decarbonization advocates: beneficiaries via vindication of climate-policy goal
 *   - fossil-fuel lobby and coal industry: excluded from regulatory process; fund litigation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(caa_section_111d_delegation__systemic_transformation_reading, 0.68).
domain_priors:suppression_score(caa_section_111d_delegation__systemic_transformation_reading, 0.71).
domain_priors:theater_ratio(caa_section_111d_delegation__systemic_transformation_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(caa_section_111d_delegation__systemic_transformation_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(caa_section_111d_delegation__systemic_transformation_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(caa_section_111d_delegation__systemic_transformation_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(caa_section_111d_delegation__systemic_transformation_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(caa_section_111d_delegation__systemic_transformation_reading, resistance, 0.69).

% --- Constraint claim ---
narrative_ontology:constraint_claim(caa_section_111d_delegation__systemic_transformation_reading, tangled_rope).
narrative_ontology:human_readable(caa_section_111d_delegation__systemic_transformation_reading, "Section 111(d) 'Best System' as Grid-Wide Generation-Shifting Mandate").
narrative_ontology:topic_domain(caa_section_111d_delegation__systemic_transformation_reading, "administrative_law/environmental_regulation/constitutional_interpretation").

domain_priors:requires_active_enforcement(caa_section_111d_delegation__systemic_transformation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(caa_section_111d_delegation__systemic_transformation_reading, '4d965ef2-f8a5-446e-b305-44698e5a528f').
narrative_ontology:cs_kernel_codification('4d965ef2-f8a5-446e-b305-44698e5a528f', fixed_text).
narrative_ontology:cs_authority_grounding('4d965ef2-f8a5-446e-b305-44698e5a528f', extraction).
narrative_ontology:cs_interpretation_layer_present('4d965ef2-f8a5-446e-b305-44698e5a528f').
narrative_ontology:cs_reading_relation('4d965ef2-f8a5-446e-b305-44698e5a528f', caa_section_111d_delegation__facility_constraint_reading, coexists_with).
narrative_ontology:cs_axiom('4d965ef2-f8a5-446e-b305-44698e5a528f', foundational, best_system_scope_includes_generation_shifting).
narrative_ontology:cs_axiom_status(best_system_scope_includes_generation_shifting, holdable).
narrative_ontology:cs_axiom_grounding('4d965ef2-f8a5-446e-b305-44698e5a528f', best_system_scope_includes_generation_shifting, deontological).
narrative_ontology:cs_axiom('4d965ef2-f8a5-446e-b305-44698e5a528f', foundational, statutory_language_evolution_with_scientific_progress).
narrative_ontology:cs_axiom_status(statutory_language_evolution_with_scientific_progress, holdable).
narrative_ontology:cs_axiom_grounding('4d965ef2-f8a5-446e-b305-44698e5a528f', statutory_language_evolution_with_scientific_progress, instrumental).
narrative_ontology:cs_reference_frame('4d965ef2-f8a5-446e-b305-44698e5a528f', epa_adaptive_statutory_interpretation).
narrative_ontology:cs_drift_state('4d965ef2-f8a5-446e-b305-44698e5a528f', post_paris_climate_commitment_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('4d965ef2-f8a5-446e-b305-44698e5a528f', '').
narrative_ontology:cs_kernel_id(caa_section_111d_delegation__systemic_transformation_reading, caa_section_111d_delegation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(caa_section_111d_delegation__systemic_transformation_reading, renewable_energy_producers).
narrative_ontology:constraint_beneficiary(caa_section_111d_delegation__systemic_transformation_reading, decarbonization_advocates).
narrative_ontology:constraint_beneficiary(caa_section_111d_delegation__systemic_transformation_reading, climate_compliance_states).
narrative_ontology:constraint_victim(caa_section_111d_delegation__systemic_transformation_reading, coal_sector_incumbents).
narrative_ontology:constraint_victim(caa_section_111d_delegation__systemic_transformation_reading, coal_dependent_states).
narrative_ontology:constraint_victim(caa_section_111d_delegation__systemic_transformation_reading, fossil_locked_utilities).
narrative_ontology:constraint_vindicates(caa_section_111d_delegation__systemic_transformation_reading, epa_statutory_authority_systemic_decarbonization).
narrative_ontology:constraint_vindicates(caa_section_111d_delegation__systemic_transformation_reading, regulatory_delegation_doctrine_breadth).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interprets 'best system of emission reduction' in Section 111(d) as authorizing mandate of state-wide generation-shifting strategies, including coal retirement and renewable substitution. Sets compliance timelines and calculation methodologies. Enforces via state implementation plans and federal backstop authority. Controls the interpretive frame through regulatory guidance, technical documents, and enforcement discretion.
narrative_ontology:constraint_stakeholder(caa_section_111d_delegation__systemic_transformation_reading, epa_regulatory_authority, agenda_setter,
    institutional, generational, analytical, national).

% Benefit from mandated procurement of renewable generation as compliance pathways for utilities and states. Market demand is artificially elevated by regulatory requirements; subsidies flow indirectly through utility cost-recovery mechanisms. Can relocate to favorable jurisdictions; investment confidence depends on sustained regulatory support.
narrative_ontology:constraint_stakeholder(caa_section_111d_delegation__systemic_transformation_reading, renewable_energy_producers, beneficiary,
    powerful, generational, arbitrage, national).

% Face early retirement mandates and demand destruction from generation-shifting requirements. Stranded coal plants cannot economically convert to other fuels; sunk capital is lost. Geographic specificity of coal resources and existing plants locks operators into coal-dominant regions. Litigation is available but expensive and uncertain; political exit is partially available but insufficient to overturn regulatory direction.
narrative_ontology:constraint_stakeholder(caa_section_111d_delegation__systemic_transformation_reading, coal_sector_incumbents, payer,
    powerful, biographical, trapped, national).

% States where coal mining and coal-fired generation are major tax and employment bases face compliance cost and economic transition pressure. Their institutional identity is partly constituted through fossil-fuel policy and industry relationships. Exit options are constrained by federal supremacy in the Clean Air Act; they can litigate the interpretation but cannot nullify federal mandate if upheld. Economic dependence on coal tax revenue and worker votes creates identity-level friction with compliance pathways.
narrative_ontology:constraint_stakeholder(caa_section_111d_delegation__systemic_transformation_reading, coal_dependent_states, payer,
    moderate, generational, identity_locked, regional).

% Utilities with legacy coal fleet investments must retire plants early or retrofit at high cost, or procure renewable generation at mandated percentages. Integrated utilities in coal states bear both the stranded asset loss and the transition cost. They can pass some costs to ratepayers (cost-recovery mechanisms) and can invest in renewables, but cannot exit the regulatory framework or delay transition fundamentally.
narrative_ontology:constraint_stakeholder(caa_section_111d_delegation__systemic_transformation_reading, fossil_locked_utilities, payer,
    organized, biographical, constrained, national).

% States with low coal dependence and existing renewable capacity benefit from the interpretation: their compliance cost is low, their renewable industries are advantaged, and their political alignment with decarbonization is vindicated. They can relocate climate investments and attract clean-energy firms; they have no identity-lock to fossil fuels.
narrative_ontology:constraint_stakeholder(caa_section_111d_delegation__systemic_transformation_reading, climate_compliance_states, beneficiary,
    powerful, generational, mobile, regional).

% Environmental and climate advocates benefit from a regulatory interpretation that enables grid-scale decarbonization rather than marginal facility improvements. Their policy goal is vindicated; they can marshal scientific support and public opinion. They have institutional and ideological investment in EPA authority and can relocate effort to other jurisdictions or federal agencies if one avenue closes.
narrative_ontology:constraint_stakeholder(caa_section_111d_delegation__systemic_transformation_reading, decarbonization_advocates, beneficiary,
    organized, civilizational, mobile, global).

% Coal industry associations and fossil-fuel advocacy organizations are structurally excluded from the regulatory process that interprets Section 111(d). They can testify at hearings, comment on proposals, and fund litigation, but cannot veto or materially shape the interpretation through the administrative process itself. Their primary exit is litigation, which is expensive and has low expected success.
narrative_ontology:constraint_stakeholder(caa_section_111d_delegation__systemic_transformation_reading, fossil_fuel_lobby, excluded,
    powerful, biographical, trapped, national).

% Congress enacted the Clean Air Act with deliberately ambiguous 'best system' language, delegating specificity to EPA. Under this systemic-transformation reading, Congress's intent is read to authorize broad agency discretion over generation-shifting. Congress could amend the statute to restrict EPA's authority, but political coordination is difficult; energy politics fragment across regional and industry lines.
narrative_ontology:constraint_stakeholder(caa_section_111d_delegation__systemic_transformation_reading, congress_delegating_body, excluded,
    institutional, generational, mobile, national).

% Scholars of statutory interpretation, delegation doctrine, and environmental law evaluate whether the 'best system' language plausibly authorizes systemic generation-shifting. They produce expertise that informs courts, agencies, and public discourse but do not directly determine outcomes. Their seat is analytical; they observe and interpret.
narrative_ontology:constraint_stakeholder(caa_section_111d_delegation__systemic_transformation_reading, administrative_law_academia, observer,
    analytical, generational, analytical, global).

% Courts review EPA's interpretation of Section 111(d) under Chevron deference (or current successor doctrine) and decide whether systemic-transformation reading is within EPA's statutory authority. Their role is adjudicatory; the outcome determines the constraint's validity.
narrative_ontology:constraint_stakeholder(caa_section_111d_delegation__systemic_transformation_reading, federal_courts, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(caa_section_111d_delegation__systemic_transformation_reading, renewable_energy_producers).
narrative_ontology:fixing_cost_class(caa_section_111d_delegation__systemic_transformation_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a federal-state regulatory mechanism to achieve grid-wide carbon reduction: EPA sets emission-reduction targets and compliance flexibility; states and utilities choose compliance pathways (renewable procurement, coal retirement, efficiency, etc.) within federal guardrails. Solves a collective-action problem: no state acting alone can achieve national grid decarbonization; the federal mandate coordinates state compliance and prevents free-riding via interstate competition in lax regulation.
% TRANSFER_FUNCTION: Transfers economic rents from coal-dependent utilities and coal-sector operators to renewable energy producers and decarbonization-compliant states. The transfer is embedded in regulatory requirement: utilities must procure renewable generation (higher cost initially) or retire coal plants (stranded asset loss), and the opportunity gain flows to renewable operators. States with low coal dependence bear lower compliance cost and attract clean-energy investment. The transfer is coercive: coal-dependent actors cannot opt out without accepting federal enforcement.
% ABSENT_VOICES: Coal workers (as distinct from coal-company shareholders) are structurally absent from the regulatory process: they have work-seniority interests in continued coal employment that conflict with both industry and decarbonization advocates, but lack the organizational power of either. Fossil-fuel firms can lobby; workers cannot. Rural coal-dependent communities are also absent from the main regulatory dialogue—they are represented indirectly through state governments but not as self-determined participants. Their perspective (we depend on this for income and cannot relocate easily) enters the conversation only as a cost externality.
% DISAPPEARANCE_RATIONALE: If Section 111(d) were reinterpreted to authorize only facility-level measures (the rival reading), the constraint disappears. The United States would revert to facility-by-facility marginal improvements; grid-wide generation-shifting would require separate legislation or regulation. Utilities would retain coal fleet assets longer; coal-dependent states would retain employment and tax base longer. Renewable deployment would slow without the mandate-driven procurement. The energy and political economy of the grid would restructure around decentralized efficiency rather than coordinated decarbonization.
% FOUNDING_PROBLEM: The Clean Air Act's Title V and Section 111 were designed to reduce air pollution through state implementation plans and technology-forcing standards. The 'best system' language was deliberately left ambiguous to allow regulatory evolution as knowledge advanced. The founding problem is: how can a static statute address dynamic pollution-control challenges without requiring constant amendment? The systemic-transformation reading answers: the statute grants EPA authority to define 'best system' based on current technical and scientific understanding, enabling adaptation to new pollution-control strategies (in this case, grid-decarbonization) without legislative amendment.
% FOUNDING_PROBLEM_CORROBORATION: EPA and environmental advocates attest the founding problem remains live: air quality and climate are dynamic; the statute's silence on generation-shifting does not foreclose it as a legitimate pollution-control strategy. The renewable energy industry and climate science community corroborate that grid decarbonization is within the technical and economic scope of 'best system' under current understanding. Coal industry and fossil-fuel advocates attest the founding problem has shifted: the original concern was local air pollution (SOx, NOx, particulates), which facility-level measures address adequately; generation-shifting addresses climate rather than air pollution, which exceeds the statute's original scope. This disagreement is fundamental: does 'best system' evolve with scientific understanding, or is it locked to the pollution types contemplated in 1970?
narrative_ontology:disappearance_verdict(caa_section_111d_delegation__systemic_transformation_reading, world_rearranges).
narrative_ontology:founding_problem_status(caa_section_111d_delegation__systemic_transformation_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(caa_section_111d_delegation__systemic_transformation_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(caa_section_111d_delegation__systemic_transformation_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(caa_section_111d_delegation__systemic_transformation_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(caa_section_111d_delegation__systemic_transformation_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(caa_section_111d_delegation__systemic_transformation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate-high (0.68) because the constraint transfers substantial economic value from coal-dependent actors to renewable producers and decarbonization-compliant states, but the transfer is justified by a plausible interpretation of environmental law rather than pure coercion. Suppression is also high (0.71) because coal-sector actors face legal, economic, and political barriers to reversing the interpretation; litigation is uncertain, political veto is difficult, and economic alternatives are costly. Theater ratio is low (0.28) because the regulatory justification (air quality and climate protection) is genuine, though increasingly decoupled from the original statutory concern (local air pollution). The measurement series models a ratchet effect: extractiveness and suppression increase over the interval as renewable deployment accelerates, coal retirements occur, and the regulatory framework solidifies. Theater ratio rises slightly then plateaus, indicating that the functional gap (between the original air-pollution mission and the new climate-decarbonization mission) stabilizes rather than growing indefinitely.
 *
 * PERSPECTIVAL GAP:
 *   The EPA and renewable-energy beneficiary seats should compute as witnessing coordination with modest extraction—a constraint that solves a genuine grid-decarbonization problem while privileging renewable operators. The coal-sector target seats should compute as witnessing pure extraction: they see a regulatory constraint that destroys asset value without compensating them and uses environmental law as cover. The perspective divergence is structural: from EPA's seat the interpretation is plausible statutory authority; from coal's seat the interpretation is regulatory overreach. The engine computes per-seat classifications; the authored claim ('tangled_rope') reflects the mixed-motive structure: yes, genuine coordination on decarbonization; yes, asymmetric extraction from coal. No seat sees it as pure rope (genuine coordination, no extraction), and coal likely computes as snare (extraction dominating any coordination benefit to them).
 *
 * DIRECTIONALITY LOGIC:
 *   EPA sits at the analytical seat as the agenda-setter: it interprets the statute and enforces compliance, but does not directly collect economic value. Its directionality is near-neutral (d ~0.5) because its power to set the interpretation is enormous, but its own extraction is minimal—the constraint's operation vindicates EPA institutional authority without making EPA wealthy. Renewable energy producers are structural beneficiaries: they benefit from mandated procurement without bearing the cost of setting or maintaining the constraint; their directionality is low (d ~0.15), approaching full beneficiary. Coal incumbents are structural targets: they bear the stranded-asset cost and demand destruction; their directionality is high (d ~0.85), approaching full target. Coal-dependent states are also targets but with identity-lock dynamics: their exit option is classified as 'identity_locked' because federal supremacy in environmental law is settled; they cannot exit through legal challenge alone, and economic alternatives require fundamental identity reorientation away from fossil-fuel dependence. This locks d upward (d ~0.82).
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint avoids false mandatrophy by maintaining a live and substantial founding problem. The original Clean Air Act addressed air pollution; the systemic-transformation reading extends 'best system' to address climate change. Decarbonization advocates argue the founding problem (reducing pollution through regulatory evolution) remains live and that climate is a legitimate pollution concern. Coal advocates argue the founding problem has shifted: climate is not air pollution, and the statute's original scope did not contemplate generation-shifting. The mismatch is real but not mandatrophic: the constraint's beneficiaries (EPA authority, renewables, climate advocates) maintain the interpretation because they believe the founding problem is live, not because the original function is dead and the constraint persists by inertia. A genuinely mandatrophic version would involve EPA enforcing 111(d) coal-retirement requirements in a world where grid-scale renewable deployment is complete and no additional coal retirements are needed—extraction persisting without coordination justification. The current constraint has not yet reached that state.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    statutory_scope_ambiguity,
    'Does ''best system of emission reduction'' in Section 111(d) authorize EPA to mandate changes to generation sources (coal vs. renewable), or is EPA limited to measures implementable at individual facilities?',
    'Supreme Court interpretation of the statute under Chevron deference doctrine (or successor framework). The court decides whether the statute''s text, legislative history, and structure plausibly authorize systemic generation-shifting.',
    'If generation-shifting is authorized, the systemic-transformation reading stands; the constraint persists as tangled rope. If EPA authority is limited to facility-level measures, the constraint collapses into a much narrower rope or scaffold—coordination is weaker, extraction is lower, and coal assets are protected longer.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(statutory_scope_ambiguity, conceptual, 'Whether ''best system'' scope includes generation source choices.').

omega_variable(
    founding_problem_scope_drift,
    'Is climate change mitigation within the founding problem of the Clean Air Act (which addressed air pollution in the 1970s), or does climate decarbonization exceed the original statutory scope?',
    'Legislative amendment clarifying EPA authority, or Supreme Court ruling on whether climate falls within ''emission reduction'' as originally conceived. Congressional testimony and energy-policy guidance from non-fossil-fuel sources can corroborate whether climate decarbonization was contemplated.',
    'If climate is deemed within original scope, the founding problem remains live and mandatrophy is avoided. If climate is deemed out-of-scope, the constraint becomes mandatrophic: coal retirement is enforced via a regulation addressing an unanticipated problem, persisting only because beneficiaries defend the interpretation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(founding_problem_scope_drift, conceptual, 'Whether climate change mitigation is within the CAA''s original founding mandate.').

omega_variable(
    coal_state_identity_lock_persistence,
    'Is the measured identity-lock of coal-dependent states structural (federal supremacy makes exit legally impossible) or internalized (state actors have fused their identity with coal policy)?',
    'Post-compliance trajectory analysis: if coal-dependent states that accept compliance show stable identity reorientation toward clean energy and sustainable economy, the lock was partly internalized and becomes escapable through reframing. If states maintain coal-identity rhetoric and political resistance despite compliance, the lock remains structural and binding.',
    'If identity-lock is internalized, suppression may be lower than authored because exit is theoretically available through identity reorientation—states could choose decarbonization if they reframed their political identity. If lock is structural, suppression is accurate: federal law forecloses their primary exit, and they remain trapped regardless of identity reorientation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coal_state_identity_lock_persistence, empirical, 'Whether coal-state identity-lock is structural or partly internalized and escapable.').

omega_variable(
    renewable_beneficiary_durability,
    'As renewable deployment penetration increases, do renewable producers remain beneficiaries of mandated procurement, or do they become competitors in an open market and lose the mandate-driven advantage?',
    'Market analysis over the interval: if renewable generation costs drop below conventional generation and renewables capture market share through price competition (not mandate), the beneficiary status shifts. If mandate-driven procurement remains the primary demand driver and renewables cannot sustain profitability in open competition, beneficiary status persists.',
    'If renewables transition to open-market competition, the constraint''s extraction shifts from coal→renewables transfer to coal→decarbonization-goal transfer. The constraint becomes less of a tangled rope (asymmetric extraction) and more of a scaffold (temporary subsidy for transition).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(renewable_beneficiary_durability, empirical, 'Whether renewable beneficiary status is durable or transitional.').

omega_variable(
    systemic_vs_facility_kernel_foreclosure,
    'Do the systemic-transformation and facility-constraint readings logically foreclose each other, or can they coexist as different parties'' interpretations of the same statute?',
    'Jurisprudential analysis: if the readings make directly contradictory claims about the scope of EPA authority (authority includes generation-shifting vs. authority excludes generation-shifting), they foreclose. If they represent different methodologies for interpreting ambiguous language and both are defensible within statutory canons, they coexist.',
    'If readings foreclose, one must be adjudicated true and the other false; the winner''s constraint stands, the loser''s constraint is extinguished. If readings coexist, both constraints persist in case law, and jurisdiction/context determine which applies (splitting the energy law landscape).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(systemic_vs_facility_kernel_foreclosure, conceptual, 'Whether the two Section 111(d) readings logically foreclose or coexist.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(caa_section_111d_delegation__systemic_transformation_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(caa__tr_t0, caa_section_111d_delegation__systemic_transformation_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement_basis(caa__tr_t0, projected).
narrative_ontology:measurement(caa__tr_t5, caa_section_111d_delegation__systemic_transformation_reading, theater_ratio, 5, 0.16).
narrative_ontology:measurement_basis(caa__tr_t5, projected).
narrative_ontology:measurement(caa__tr_t10, caa_section_111d_delegation__systemic_transformation_reading, theater_ratio, 10, 0.2).
narrative_ontology:measurement_basis(caa__tr_t10, projected).
narrative_ontology:measurement(caa__tr_t15, caa_section_111d_delegation__systemic_transformation_reading, theater_ratio, 15, 0.24).
narrative_ontology:measurement_basis(caa__tr_t15, projected).
narrative_ontology:measurement(caa__tr_t20, caa_section_111d_delegation__systemic_transformation_reading, theater_ratio, 20, 0.27).
narrative_ontology:measurement_basis(caa__tr_t20, projected).
narrative_ontology:measurement(caa__tr_t25, caa_section_111d_delegation__systemic_transformation_reading, theater_ratio, 25, 0.27).
narrative_ontology:measurement_basis(caa__tr_t25, projected).
narrative_ontology:measurement(caa__tr_t30, caa_section_111d_delegation__systemic_transformation_reading, theater_ratio, 30, 0.28).
narrative_ontology:measurement_basis(caa__tr_t30, projected).

% Extraction over time
narrative_ontology:measurement(caa__be_t0, caa_section_111d_delegation__systemic_transformation_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement_basis(caa__be_t0, projected).
narrative_ontology:measurement(caa__be_t5, caa_section_111d_delegation__systemic_transformation_reading, base_extractiveness, 5, 0.52).
narrative_ontology:measurement_basis(caa__be_t5, projected).
narrative_ontology:measurement(caa__be_t10, caa_section_111d_delegation__systemic_transformation_reading, base_extractiveness, 10, 0.58).
narrative_ontology:measurement_basis(caa__be_t10, projected).
narrative_ontology:measurement(caa__be_t15, caa_section_111d_delegation__systemic_transformation_reading, base_extractiveness, 15, 0.63).
narrative_ontology:measurement_basis(caa__be_t15, projected).
narrative_ontology:measurement(caa__be_t20, caa_section_111d_delegation__systemic_transformation_reading, base_extractiveness, 20, 0.66).
narrative_ontology:measurement_basis(caa__be_t20, projected).
narrative_ontology:measurement(caa__be_t25, caa_section_111d_delegation__systemic_transformation_reading, base_extractiveness, 25, 0.67).
narrative_ontology:measurement_basis(caa__be_t25, projected).
narrative_ontology:measurement(caa__be_t30, caa_section_111d_delegation__systemic_transformation_reading, base_extractiveness, 30, 0.68).
narrative_ontology:measurement_basis(caa__be_t30, projected).

% Suppression requirement over time
narrative_ontology:measurement(caa__su_t0, caa_section_111d_delegation__systemic_transformation_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement_basis(caa__su_t0, projected).
narrative_ontology:measurement(caa__su_t5, caa_section_111d_delegation__systemic_transformation_reading, suppression_requirement, 5, 0.54).
narrative_ontology:measurement_basis(caa__su_t5, projected).
narrative_ontology:measurement(caa__su_t10, caa_section_111d_delegation__systemic_transformation_reading, suppression_requirement, 10, 0.61).
narrative_ontology:measurement_basis(caa__su_t10, projected).
narrative_ontology:measurement(caa__su_t15, caa_section_111d_delegation__systemic_transformation_reading, suppression_requirement, 15, 0.66).
narrative_ontology:measurement_basis(caa__su_t15, projected).
narrative_ontology:measurement(caa__su_t20, caa_section_111d_delegation__systemic_transformation_reading, suppression_requirement, 20, 0.69).
narrative_ontology:measurement_basis(caa__su_t20, projected).
narrative_ontology:measurement(caa__su_t25, caa_section_111d_delegation__systemic_transformation_reading, suppression_requirement, 25, 0.7).
narrative_ontology:measurement_basis(caa__su_t25, projected).
narrative_ontology:measurement(caa__su_t30, caa_section_111d_delegation__systemic_transformation_reading, suppression_requirement, 30, 0.71).
narrative_ontology:measurement_basis(caa__su_t30, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(caa_section_111d_delegation__systemic_transformation_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(caa_section_111d_delegation__systemic_transformation_reading, 0.12).
narrative_ontology:affects_constraint(caa_section_111d_delegation__systemic_transformation_reading, caa_section_111d_delegation__facility_constraint_reading).

% DUAL FORMULATION NOTE:
% Section 111(d) 'best system' is a contested kernel with two interpretive readings: systemic_transformation_reading (this constraint) authorizes EPA to mandate grid-wide generation-shifting and coal retirement; facility_constraint_reading (sibling constraint) limits EPA to facility-level measures. The readings are structurally distinct constraints with different ε, different victim/beneficiary sets, and different types. They coexist in case law but are mutually exclusive in judicial interpretation—whichever prevails dominates the regulatory landscape. Both constraints are authored independently with their own metrics and stakeholder situations; they are linked via network.affects_constraints to model the kernel contest. The systemic reading influences the facility reading by creating pressure for legal and legislative clarity: if courts adopt the systemic reading, it forecloses (or severely constrains) the facility reading's practical applicability.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(caa_section_111d_delegation__systemic_transformation_reading, moderate, 0.82).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
