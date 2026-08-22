% ============================================================================
% CONSTRAINT STORY: federation_membership_obligations__integration_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_federation_membership_obligations__integration_primary, []).

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
 *   constraint_id: federation_membership_obligations__integration_primary
 *   human_readable: EU Free Movement as Integration Primary: Welfare Boundaries Subordinate to Mobility Rights
 *   domain: political_economy/federalism/welfare_policy
 *
 * SUMMARY:
 *   The integration-primary reading of EU federation membership treats free
 *   movement and welfare portability as constitutive of EU citizenship, with
 *   the corollary that member state welfare boundaries must yield to mobility
 *   rights. Under this reading, a mobile worker from Bulgaria has the same
 *   claim to housing assistance in Germany as a long-term German resident —
 *   EU citizenship trumps national welfare closure. This generates asymmetric
 *   cost distribution: receiving states bear fiscal costs; mobile workers and
 *   multinational employers gain labor arbitrage; locally-rooted,
 *   low-mobility workers face wage pressure and reduced bargaining power. The
 *   ECJ enforces this reading through case law that progressively
 *   subordinates member state discretion to mobility rights. The constraint
 *   is claimed as tangled_rope because it solves a real coordination problem
 *   (integrated labor markets require mobility guarantees) while
 *   simultaneously extracting from non-mobile populations and fiscally
 *   strained states.
 *
 * KEY AGENTS:
 *   - Mobile EU workers: Primary beneficiaries; gain unrestricted labor market access and welfare portability across member states.
 *   - Locally-rooted low-skilled workers: Primary victims; experience wage pressure and reduced bargaining power as labor supply becomes mobile.
 *   - National welfare administrators and fiscally strained receiving states: Secondary victims and nominal rule-setters; bear budget costs while ECJ constrains their discretion to restrict eligibility.
 *   - Multinational employers and service providers: Secondary beneficiaries; gain integrated labor markets with reduced hiring friction and arbitrage opportunities.
 *   - ECJ and EU institutions: Agenda-setters; enforce and expand free movement through case law, setting the trajectory of integration.
 *   - Sending states with high outflows: Dual-positioned; benefit from reduced unemployment and remittances, but lose young/skilled workers and corresponding tax base.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(federation_membership_obligations__integration_primary, 0.68).
domain_priors:suppression_score(federation_membership_obligations__integration_primary, 0.71).
domain_priors:theater_ratio(federation_membership_obligations__integration_primary, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(federation_membership_obligations__integration_primary, extractiveness, 0.68).
narrative_ontology:constraint_metric(federation_membership_obligations__integration_primary, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(federation_membership_obligations__integration_primary, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(federation_membership_obligations__integration_primary, accessibility_collapse, 0.64).
narrative_ontology:constraint_metric(federation_membership_obligations__integration_primary, resistance, 0.73).

% --- Constraint claim ---
narrative_ontology:constraint_claim(federation_membership_obligations__integration_primary, tangled_rope).
narrative_ontology:human_readable(federation_membership_obligations__integration_primary, "EU Free Movement as Integration Primary: Welfare Boundaries Subordinate to Mobility Rights").
narrative_ontology:topic_domain(federation_membership_obligations__integration_primary, "political_economy/federalism/welfare_policy").

domain_priors:requires_active_enforcement(federation_membership_obligations__integration_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(federation_membership_obligations__integration_primary, '150d7523-a890-4a08-a349-243695dd47c9').
narrative_ontology:cs_kernel_codification('150d7523-a890-4a08-a349-243695dd47c9', fixed_text).
narrative_ontology:cs_authority_grounding('150d7523-a890-4a08-a349-243695dd47c9', extraction).
narrative_ontology:cs_interpretation_layer_present('150d7523-a890-4a08-a349-243695dd47c9').
narrative_ontology:cs_reading_relation('150d7523-a890-4a08-a349-243695dd47c9', federation_membership_obligations__member_sovereignty_primary, coexists_with).
narrative_ontology:cs_reading_relation('150d7523-a890-4a08-a349-243695dd47c9', federation_membership_obligations__selective_solidarity, influences).
narrative_ontology:cs_axiom('150d7523-a890-4a08-a349-243695dd47c9', foundational, free_movement_constitutive_of_membership).
narrative_ontology:cs_axiom_status(free_movement_constitutive_of_membership, holdable).
narrative_ontology:cs_axiom_grounding('150d7523-a890-4a08-a349-243695dd47c9', free_movement_constitutive_of_membership, conventional).
narrative_ontology:cs_axiom('150d7523-a890-4a08-a349-243695dd47c9', foundational, welfare_boundaries_subordinate_to_citizenship).
narrative_ontology:cs_axiom_status(welfare_boundaries_subordinate_to_citizenship, holdable).
narrative_ontology:cs_axiom_grounding('150d7523-a890-4a08-a349-243695dd47c9', welfare_boundaries_subordinate_to_citizenship, deontological).
narrative_ontology:cs_reference_frame('150d7523-a890-4a08-a349-243695dd47c9', unified_eu_citizenship_with_mobility_primacy).
narrative_ontology:cs_drift_state('150d7523-a890-4a08-a349-243695dd47c9', contemporary_fiscal_strain_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('150d7523-a890-4a08-a349-243695dd47c9', '2026-06-12T14:32:15Z').
narrative_ontology:cs_kernel_id(federation_membership_obligations__integration_primary, federation_membership_obligations).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(federation_membership_obligations__integration_primary, mobile_eu_workers).
narrative_ontology:constraint_beneficiary(federation_membership_obligations__integration_primary, service_providers_across_borders).
narrative_ontology:constraint_beneficiary(federation_membership_obligations__integration_primary, multinational_employers).
narrative_ontology:constraint_victim(federation_membership_obligations__integration_primary, locally_rooted_low_skilled_workers).
narrative_ontology:constraint_victim(federation_membership_obligations__integration_primary, national_welfare_administrators).
narrative_ontology:constraint_victim(federation_membership_obligations__integration_primary, fiscally_strained_receiving_states).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(federation_membership_obligations__integration_primary, sending_states_with_outflows).
narrative_ontology:constraint_victim(federation_membership_obligations__integration_primary, sending_states_with_outflows).
narrative_ontology:constraint_victim(federation_membership_obligations__integration_primary, national_labor_unions).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Can relocate to any EU member state and claim residence rights, access to employment, and (after eligible period) full welfare benefits including housing assistance, unemployment insurance, and child allowances at the receiving state's level. They gain labor market access and benefit portability without renouncing their home state citizenship. The constraint treats them as EU citizens first, resident location second.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__integration_primary, mobile_eu_workers, beneficiary,
    moderate, biographical, mobile, continental).

% Experience direct labor market pressure from expanded mobility: wage suppression in construction, hospitality, and care sectors; reduced bargaining power in industries with high cross-border worker inflows. They cannot easily relocate (language, family ties, lack of skills recognized across borders). Fiscal adjustment costs (from expanded welfare rolls) emerge in their home state's tax base without corresponding benefit to them. The constraint's enforcement (ECJ overturning member state restrictions) happens above their level of voice.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__integration_primary, locally_rooted_low_skilled_workers, payer,
    powerless, biographical, trapped, national).

% Must absorb welfare claims from newly mobile EU citizens at the receiving state's full benefit level, with no corresponding adjustment to budget or to the sending state's contribution. They bear the cash cost of the constraint's operation. The ECJ's case law progressively narrows their discretion to means-test or residence-restrict benefits. They administer the rule but did not set it — the constraint operates above their authority.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__integration_primary, national_welfare_administrators, payer,
    institutional, generational, constrained, national).

% Experience budget pressure from expanded welfare eligibility (mobility + full benefit access) while retaining nominal authority over welfare system design. They set nominal rules but under hard ECJ constraints; their attempts to restrict access are regularly struck down. The tension between fiscal autonomy and mobility obligation is the core of this constraint's asymmetry: they bear costs they cannot unilaterally control.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__integration_primary, fiscally_strained_receiving_states, payer,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(federation_membership_obligations__integration_primary, fiscally_strained_receiving_states, agenda_setter).

% Gain access to a labor market where workers are mobile and welfare-portable, reducing their hiring friction and labor cost volatility. Posted workers and cross-border service provision are expanded by the constraint's operation. They can arbitrage wage differences while relying on receiving-state welfare to maintain purchasing power and reduce pressure for wage adjustment.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__integration_primary, service_providers_across_borders, beneficiary,
    powerful, biographical, arbitrage, continental).

% Benefit from integrated labor markets and reduced hiring constraints across member states. Mobility rights and welfare portability allow them to structure production across borders with flexible staffing; welfare systems in receiving states absorb adjustment costs (unemployment, housing support) that would otherwise pressure wage negotiations.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__integration_primary, multinational_employers, beneficiary,
    powerful, generational, arbitrage, global).

% Enforce and expand free movement rights through case law (Directive 2004/38, ECJ jurisprudence on welfare access). They set the rule and its trajectory — progressively subordinating national welfare boundaries to EU citizenship. They do not face the fiscal or labor market pressure their rulings generate; their authority rests on the reading that integration is constitutive of EU membership.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__integration_primary, ecj_and_eu_institutions, agenda_setter,
    institutional, generational, analytical, continental).

% Benefit from remittances and reduced domestic unemployment pressure when workers migrate outward. They also bear a loss: workers (often young, skilled) leave, drawing pensions and education investments elsewhere. The constraint creates an asymmetric brain-drain pattern where lower-income states export human capital while receiving no compensation in the EU budget (no explicit transfer follows mobility flows).
narrative_ontology:constraint_stakeholder(federation_membership_obligations__integration_primary, sending_states_with_outflows, beneficiary,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(federation_membership_obligations__integration_primary, sending_states_with_outflows, payer).

% Workers without EU citizenship, refugees, and third-country nationals are structurally outside the free movement and welfare access frame. They face tighter borders and welfare restrictions even in the same labor markets where EU citizens have expansive rights. The constraint's beneficiaries operate under citizenship-based inclusion that excludes non-EU residents.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__integration_primary, low_mobility_workers, excluded,
    powerless, biographical, trapped, local).

% Face eroded bargaining power as labor supply becomes mobile and cross-border, while domestic wage standards and working conditions are subject to competition from posted workers and mobile labor willing to work at lower rates. Attempts to restrict posted-worker conditions or require equal treatment have been narrowed by ECJ rulings prioritizing free movement.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__integration_primary, national_labor_unions, payer,
    organized, generational, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(federation_membership_obligations__integration_primary, multinational_employers).
narrative_ontology:fixing_cost_class(federation_membership_obligations__integration_primary, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Enables integrated European labor and services markets by guaranteeing workers and businesses can move across borders without forfeiting welfare access or employment rights. Solves a genuine collective-action problem: without free movement guarantees, member states would each restrict entry to protect domestic workers, fragmenting the single market and reducing aggregate efficiency.
% TRANSFER_FUNCTION: Moves welfare costs and labor market adjustment burdens from beneficiary seats (mobile workers, multinational firms, service providers) to payer seats (local low-skilled workers through wage pressure, national welfare budgets through expanded eligibility, receiving-state treasuries through fiscal strain). The transfer is structured: receiving states assume full welfare obligations for new mobile residents while no fiscal adjustment mechanism compensates them, and sending states export human capital with no corresponding EU-level redistribution.
% ABSENT_VOICES: Low-mobility workers (particularly third-country nationals and refugees) are structurally excluded from the free movement frame entirely — they experience tightened borders and welfare restrictions in the same labor markets where EU citizens have expansive access. National labor movements and local working-class constituencies are not at the EU bargaining table; their wage and employment pressure registers only through national governments' constrained attempts to restrict access (attempts regularly overturned by the ECJ). The constraint operates without the organized voice of its primary victims.
% DISAPPEARANCE_RATIONALE: If free movement and welfare portability were removed, labor markets would immediately resegregate by nationality; wage pressure on low-skilled workers would ease; national welfare systems would close; multinational supply chains would face higher hiring and relocation frictions; remittance flows would reverse or shrink; and the EU's internal market architecture would fragment into bilateral labor agreements and national protections. The constraint is generative of the integrated European economy; removing it would unwind decades of cross-border integration.
% FOUNDING_PROBLEM: Post-WWII Europe needed a mechanism to prevent nationalist labor protectionism and tariff wars from recurring. Free movement of workers and services was conceived as a confidence-building device: if labor could move freely, competitive advantage would come from productivity rather than protectionism, and member states would cooperate rather than fragment. The welfare portability piece was added later (Directive 2004/38, post-2004 enlargement) to make mobility politically viable and to signal that EU citizenship carried real benefits.
% FOUNDING_PROBLEM_CORROBORATION: EU institutions and pro-integration voices attest the founding problem remains live: without guaranteed mobility, member states would backslide into protectionism; integration requires continuous deepening. Labor movements, national governments managing fiscal strain, and local worker constituencies attest the founding problem's original urgency (preventing nationalist fragmentation) has been substantially addressed; the continued enforcement now extracts rents from mobile workers' ability to arbitrage welfare and wages. Academic labor economists find wage suppression effects for low-skilled workers in receiving countries but efficiency gains at the EU aggregate level.
narrative_ontology:disappearance_verdict(federation_membership_obligations__integration_primary, world_rearranges).
narrative_ontology:founding_problem_status(federation_membership_obligations__integration_primary, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(federation_membership_obligations__integration_primary, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(federation_membership_obligations__integration_primary, 'none', 1).
narrative_ontology:epsilon_provenance(federation_membership_obligations__integration_primary, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(federation_membership_obligations__integration_primary_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(federation_membership_obligations__integration_primary, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(federation_membership_obligations__integration_primary_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises from 0.48 (t=0) to 0.68 (t=35) over the interval, reflecting the ECJ's progressive case law narrowing member states' discretion to restrict welfare access. Early cases (Martínez Sala, Grzelczyk) began subordinating residency requirements to EU citizenship; later cases (Dano, Alimanovic) created some residency duration thresholds but still prioritized free movement. Theater ratio (0.42 at endpoint) reflects that enforcement increasingly focuses on eliminating nationality-based restrictions, but the proportional activity spend on actual integration (as opposed to defending mobility rights against member state resistance) is approximately 40%. Suppression (0.71) is high because member state attempts to manage labor market or fiscal impacts through welfare restrictions are systematically overturned. Accessibility collapse (0.64) reflects that alternatives to the constraint (bilateral labor agreements, welfare closure) have been substantially eliminated through ECJ precedent, though some member states retain nominal tools. Resistance (0.73) is high: national labor movements, fiscally strained governments, and local working-class constituencies actively resist, but their resistance operates below the level of EU decision-making and takes the form of constrained negotiation within the ECJ framework rather than exit or override.
 *
 * PERSPECTIVAL GAP:
 *   The beneficiary seats (mobile workers, multinational firms) experience this as genuine coordination enabling economic integration and opportunity. The agenda-setter seat (ECJ/EU institutions) experiences it as necessary to prevent member state backsliding and to make EU membership real. The payer seats experience different types: locally-rooted workers experience it as pure extraction (wage suppression with no offsetting benefit); national welfare administrators experience it as tangled (they coordinate labor markets but extract from budgets); receiving states experience it as tangled (they participate in mobility coordination but are not compensated for fiscal costs). The engine should compute these divergent types from the stakeholder power and exit options: low-skill workers are powerless and trapped (full target), welfare administrators are institutional with constrained exit (high extraction), receiving states are institutional with constrained exit but retain nominal rule-setting authority (hybrid), mobile workers are moderate-powered and mobile (beneficiary end). This reading is deliberately authored without reconciling claim to metrics: the claim is tangled_rope (real coordination + asymmetric extraction); the metrics are substantially extractive for payers and suppressive of their alternatives, consistent with a constraint that may compute as snare from certain seats. The divergence is precisely the measurement the framework exists to take.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality derives from beneficiary/victim declarations plus exit options. Mobile EU workers are beneficiaries with mobile exit (d ≈ 0.25–0.35); they can always return home or relocate. Locally-rooted low-skilled workers are victims with trapped exit (d ≈ 0.85–0.95); language barriers, family ties, and lack of EU-recognized credentials make mobility impractical for them. National welfare administrators and receiving states are listed as victims because they bear budget costs, but they also retain some institutional power; their d should reflect constrained exit and institutional power (d ≈ 0.60–0.70). Multinational employers are beneficiaries with arbitrage exit (d ≈ 0.10–0.20); they can shift operations within the EU or globally. The constraint's distributional asymmetry is fundamentally rooted in differential exit capacity: those who can move freely benefit; those locked in place pay. No directionality overrides are needed because the derivation chain (beneficiary/victim + exit → d) captures the structural truth.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (preventing nationalist fragmentation through free movement) was substantially solved by the 1990s. The continued enforcement of the constraint after ~2010 shifted: the ECJ progressively expanded welfare portability (Directive 2004/38, later cases) to make mobility politically viable for lower-income workers. However, the expansion occurred in a context where receiving states, particularly those with fiscal constraints or high inflows, increasingly sought to restrict access. The tension between founding rationale (prevent protectionist backsliding) and current operation (enforce welfare portability without fiscal compensation) is a mandatrophy signal. The constraint persists because EU institutions benefit from deepening integration (institutional power gain) and mobile workers benefit from expanded access, but the founding coordination problem no longer drives the rule's expansion. A mandatrophy reading would note: if member states could unilaterally restrict welfare access, they would not re-fragment the labor market (the original fear); they would simply close welfare eligibility while keeping labor borders open for workers they want. The constraint's persistence in its expanded form (welfare portability, full access after residence periods) reflects extraction — the benefit to mobile workers and firms outweighs the coordination value to the union as a whole. This does NOT mean the constraint should be classified as snare instead of tangled_rope (the coordination value is real and substantial); it means the classification should note the expansion beyond founding rationale and the divergence between claim and operation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    founding_problem_resolution,
    'Has the founding problem (preventing nationalist labor protectionism and fragmentation) been substantially resolved, or does it remain live as a justification for expanded welfare portability?',
    'Historical counterfactual: would member states move to reinstitute protectionist barriers if welfare portability were restricted? Post-2020 labor market data and survey evidence on member state intentions toward mobility restrictions; voting patterns in ECJ cases.',
    'If the founding problem is dead, the constraint''s expansion beyond original scope (to include welfare portability for economically inactive migrants) represents pure extraction masked as coordination; classification would shift toward snare from most seats. If live, the expansion is necessary insurance against member state backsliding; classification as tangled_rope holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(founding_problem_resolution, empirical, 'Whether the founding problem that justified free movement (preventing nationalist fragmentation) persists or has been substantially resolved.').

omega_variable(
    welfare_cost_attribution,
    'What portion of welfare cost increases in receiving states can be attributed to free movement and welfare portability (vs. demographic aging, unemployment cycles, policy choices unrelated to mobility)?',
    'Econometric decomposition of welfare roll composition and spending trends; cross-state comparisons with controls for economic shocks; analysis of fiscal transfers between member states and EU budget allocation to receiving states.',
    'If mobility accounts for majority of cost growth in fiscally strained states, the extraction from local populations and state budgets is direct and substantial (supports snare classification from payer seats). If mobility is a small fraction, the constraint''s extraction is modest and other policy levers are available (supports tangled_rope classification).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(welfare_cost_attribution, empirical, 'The causal attribution of welfare spending increases to free movement and mobility.').

omega_variable(
    labor_market_wage_effects_heterogeneity,
    'Are wage suppression effects from mobility concentrated in specific low-skill sectors and geographies, or diffused across labor markets? Do these effects persist long-term or dissipate as complementarities emerge?',
    'Longitudinal wage series by sector and skill level; analysis of complementarity effects (mobile workers increasing demand for services); comparison of wage trajectories in high-mobility vs. low-mobility regions; studies of adjustment costs and retraining success.',
    'Concentrated, persistent effects would indicate trapped local workers (higher d, toward snare). Diffused, dissipating effects would indicate temporary adjustment (lower d, more consistent with tangled_rope as transient extraction). If complementarities dominate, the constraint may not be extractive for local workers at all (shifts classification toward genuine coordination).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(labor_market_wage_effects_heterogeneity, empirical, 'The heterogeneity and persistence of wage suppression effects on local workers.').

omega_variable(
    alternative_coordination_mechanisms,
    'Could the single market''s coordination benefits (integrated labor and services markets) be achieved with less welfare portability? Would bilateral labor agreements or tiered welfare access (conditional on contribution history) preserve integration while reducing fiscal extraction?',
    'Historical comparison with earlier frameworks (pre-2004, pre-Directive 2004/38); analysis of non-EU federated systems (Switzerland, EEA) and their welfare portability rules; theoretical modeling of equilibrium outcomes under alternative frameworks.',
    'If alternatives could preserve coordination with less extraction, the current constraint''s expansion is not necessary; classification shifts toward pure snare (extraction riding on a coordination cover story). If alternatives fail (member states defect, integration fragments), the constraint''s current form is structurally necessary; classification as tangled_rope holds.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(alternative_coordination_mechanisms, conceptual, 'Whether the constraint''s coordination and extraction components are structurally separable or inseparably linked.').

omega_variable(
    reading_contest_location,
    'Where, in institutional terms, is the contest between integration_primary and member_sovereignty_primary readings located? Is it primarily a within-ECJ interpretive dispute, a member-state political dispute, or a constitutional/treaty-level dispute?',
    'Mapping of ECJ case law trajectory, member state opt-out provisions and their usage, EU Charter and Treaty text evolution, academic jurisprudential analysis of reading authority.',
    'If the contest is primarily within the ECJ, it is an interpretive question (authority_grounding=lineage, interpretation_layer_present=true). If primarily political, it is a distributed/contested reading (authority_grounding=distributed). This affects the cs_structure classification and the understanding of which seat controls the constraint''s evolution.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_contest_location, conceptual, 'The institutional location of the reading contest over federation membership obligations.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(federation_membership_obligations__integration_primary, 0, 35).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fede_tr_t0, federation_membership_obligations__integration_primary, theater_ratio, 0, 0.25).
narrative_ontology:measurement_basis(fede_tr_t0, projected).
narrative_ontology:measurement(fede_tr_t5, federation_membership_obligations__integration_primary, theater_ratio, 5, 0.3).
narrative_ontology:measurement_basis(fede_tr_t5, observed).
narrative_ontology:measurement(fede_tr_t10, federation_membership_obligations__integration_primary, theater_ratio, 10, 0.33).
narrative_ontology:measurement_basis(fede_tr_t10, observed).
narrative_ontology:measurement(fede_tr_t15, federation_membership_obligations__integration_primary, theater_ratio, 15, 0.37).
narrative_ontology:measurement_basis(fede_tr_t15, observed).
narrative_ontology:measurement(fede_tr_t20, federation_membership_obligations__integration_primary, theater_ratio, 20, 0.4).
narrative_ontology:measurement_basis(fede_tr_t20, observed).
narrative_ontology:measurement(fede_tr_t25, federation_membership_obligations__integration_primary, theater_ratio, 25, 0.42).
narrative_ontology:measurement_basis(fede_tr_t25, observed).
narrative_ontology:measurement(fede_tr_t30, federation_membership_obligations__integration_primary, theater_ratio, 30, 0.42).
narrative_ontology:measurement_basis(fede_tr_t30, observed).
narrative_ontology:measurement(fede_tr_t35, federation_membership_obligations__integration_primary, theater_ratio, 35, 0.42).
narrative_ontology:measurement_basis(fede_tr_t35, projected).

% Extraction over time
narrative_ontology:measurement(fede_be_t0, federation_membership_obligations__integration_primary, base_extractiveness, 0, 0.48).
narrative_ontology:measurement_basis(fede_be_t0, projected).
narrative_ontology:measurement(fede_be_t5, federation_membership_obligations__integration_primary, base_extractiveness, 5, 0.53).
narrative_ontology:measurement_basis(fede_be_t5, observed).
narrative_ontology:measurement(fede_be_t10, federation_membership_obligations__integration_primary, base_extractiveness, 10, 0.58).
narrative_ontology:measurement_basis(fede_be_t10, observed).
narrative_ontology:measurement(fede_be_t15, federation_membership_obligations__integration_primary, base_extractiveness, 15, 0.62).
narrative_ontology:measurement_basis(fede_be_t15, observed).
narrative_ontology:measurement(fede_be_t20, federation_membership_obligations__integration_primary, base_extractiveness, 20, 0.65).
narrative_ontology:measurement_basis(fede_be_t20, observed).
narrative_ontology:measurement(fede_be_t25, federation_membership_obligations__integration_primary, base_extractiveness, 25, 0.67).
narrative_ontology:measurement_basis(fede_be_t25, observed).
narrative_ontology:measurement(fede_be_t30, federation_membership_obligations__integration_primary, base_extractiveness, 30, 0.68).
narrative_ontology:measurement_basis(fede_be_t30, observed).
narrative_ontology:measurement(fede_be_t35, federation_membership_obligations__integration_primary, base_extractiveness, 35, 0.68).
narrative_ontology:measurement_basis(fede_be_t35, projected).

% Suppression requirement over time
narrative_ontology:measurement(fede_su_t0, federation_membership_obligations__integration_primary, suppression_requirement, 0, 0.55).
narrative_ontology:measurement_basis(fede_su_t0, projected).
narrative_ontology:measurement(fede_su_t5, federation_membership_obligations__integration_primary, suppression_requirement, 5, 0.6).
narrative_ontology:measurement_basis(fede_su_t5, observed).
narrative_ontology:measurement(fede_su_t10, federation_membership_obligations__integration_primary, suppression_requirement, 10, 0.64).
narrative_ontology:measurement_basis(fede_su_t10, observed).
narrative_ontology:measurement(fede_su_t15, federation_membership_obligations__integration_primary, suppression_requirement, 15, 0.67).
narrative_ontology:measurement_basis(fede_su_t15, observed).
narrative_ontology:measurement(fede_su_t20, federation_membership_obligations__integration_primary, suppression_requirement, 20, 0.69).
narrative_ontology:measurement_basis(fede_su_t20, observed).
narrative_ontology:measurement(fede_su_t25, federation_membership_obligations__integration_primary, suppression_requirement, 25, 0.7).
narrative_ontology:measurement_basis(fede_su_t25, observed).
narrative_ontology:measurement(fede_su_t30, federation_membership_obligations__integration_primary, suppression_requirement, 30, 0.71).
narrative_ontology:measurement_basis(fede_su_t30, observed).
narrative_ontology:measurement(fede_su_t35, federation_membership_obligations__integration_primary, suppression_requirement, 35, 0.71).
narrative_ontology:measurement_basis(fede_su_t35, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(federation_membership_obligations__integration_primary, resource_allocation).
narrative_ontology:boltzmann_floor_override(federation_membership_obligations__integration_primary, 0.18).
narrative_ontology:affects_constraint(federation_membership_obligations__integration_primary, federation_membership_obligations__member_sovereignty_primary).
narrative_ontology:affects_constraint(federation_membership_obligations__integration_primary, federation_membership_obligations__selective_solidarity).
narrative_ontology:affects_constraint(federation_membership_obligations__integration_primary, posted_worker_protection_regime).
narrative_ontology:affects_constraint(federation_membership_obligations__integration_primary, national_labor_market_closure_authority).

% DUAL FORMULATION NOTE:
% This constraint is part of a family decomposed from the contested kernel 'federation_membership_obligations'. The integration_primary reading asserts that free movement and welfare portability are constitutive of EU membership; the member_sovereignty_primary reading asserts that national welfare closure authority is constitutive. These are structurally distinct constraints with different beneficiary structures, different ε values, and different persistence mechanisms. They coexist across different member state governments and are resolved in the ECJ's case law trajectory, not by empirical measurement. Both readings are live positions in contemporary EU politics; neither logically forecloses the other within a single framework, though they create structural pressure on each other (integration_primary influences member_sovereignty_primary by narrowing its practical scope; member_sovereignty_primary influences integration_primary by generating political pressure for welfare restrictions).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(federation_membership_obligations__integration_primary, institutional, 0.68).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
