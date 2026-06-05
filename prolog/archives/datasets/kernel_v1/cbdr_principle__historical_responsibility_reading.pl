% ============================================================================
% CONSTRAINT STORY: cbdr_principle__historical_responsibility_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_cbdr_principle__historical_responsibility_reading, []).

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
 *   constraint_id: cbdr_principle__historical_responsibility_reading
 *   human_readable: CBDR Principle (Historical Responsibility Reading): Binding Emissions Reductions + Loss/Damage Financing
 *   domain: international_climate_governance/treaty_law/development_economics
 *
 * SUMMARY:
 *   The Common But Differentiated Responsibilities (CBDR) principle is a
 *   contested kernel in international climate law. This constraint
 *   instantiates ONE reading: historical responsibility. This reading holds
 *   that developed nations incurred binding obligations to reduce emissions
 *   proportional to their cumulative historical contributions to atmospheric
 *   CO₂, and to finance adaptation and loss/damage in developing nations as
 *   restitution for climate impacts caused by historical emissions. Under
 *   this reading, developed nations enter the victim set (they bear financial
 *   transfer obligations and accelerated decarbonization targets); developing
 *   nations exit victim status for adaptation financing gaps (they receive
 *   transfers proportional to historical responsibility). The sibling reading
 *   (voluntary_commitment_reading) grounds obligations instead in current
 *   national capacity and voluntary climate pledges, leaving the allocation
 *   of responsibility open to negotiation and allowing developed nations to
 *   position transfers as development assistance rather than historical
 *   restitution. These readings coexist across different parties'
 *   commitments—developing nations emphasize the historical-responsibility
 *   reading; developed nations often adopt the voluntary-commitment framing
 *   in treaty text. The constraint exhibits all intermediate types (rope,
 *   tangled_rope, snare, piton) across different structural positions, with a
 *   false-summit risk for the analytical observer.
 *
 * KEY AGENTS:
 *   - Developed Nations (High-Carbon Emitters): Primary victims under this reading (institutional/constrained or arbitrage depending on acceptance of reading) — bear binding emissions reduction obligations and loss/damage financing duties grounded in historical responsibility
 *   - Developing Nations (Climate-Vulnerable): Primary beneficiaries (developing/trapped) — receive adaptation financing and loss/damage compensation; also experience extraction from accelerated emissions constraints despite lower historical responsibility
 *   - Middle-Income Developing Nations (Emerging Emitters): Secondary actor (moderate/constrained) — experience mixed coordination (technology transfer) and extraction (emissions caps applied despite development trajectory)
 *   - Fossil Fuel Industries & Workers: Secondary victims (organized/constrained) — face stranded assets and career disruption; constrained by just-transition coordination functions
 *   - Multilateral Climate Finance Institutions: Secondary actor (institutional/constrained) — administer transfers while experiencing governance capture and underfunding
 *   - Historical Emissions Accounting Apparatus: Tertiary institution (institutional/arbitrage) — maintains performative epistemic legitimacy despite degraded functional primary purpose
 *   - Analytical Observer: Standpoint (analytical/analytical) — risks naturalizing historical-responsibility framing as immutable physics rather than contingent institutional reading
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(cbdr_principle__historical_responsibility_reading, 0.58).
domain_priors:suppression_score(cbdr_principle__historical_responsibility_reading, 0.62).
domain_priors:theater_ratio(cbdr_principle__historical_responsibility_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(cbdr_principle__historical_responsibility_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(cbdr_principle__historical_responsibility_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(cbdr_principle__historical_responsibility_reading, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(cbdr_principle__historical_responsibility_reading, tangled_rope).
narrative_ontology:human_readable(cbdr_principle__historical_responsibility_reading, "CBDR Principle (Historical Responsibility Reading): Binding Emissions Reductions + Loss/Damage Financing").
narrative_ontology:topic_domain(cbdr_principle__historical_responsibility_reading, "international_climate_governance/treaty_law/development_economics").

domain_priors:requires_active_enforcement(cbdr_principle__historical_responsibility_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(cbdr_principle__historical_responsibility_reading, '96977df4-a120-4382-90b9-88da9a8e7950').
narrative_ontology:cs_kernel_codification('96977df4-a120-4382-90b9-88da9a8e7950', formalized).
narrative_ontology:cs_authority_grounding('96977df4-a120-4382-90b9-88da9a8e7950', extraction).
narrative_ontology:cs_interpretation_layer_present('96977df4-a120-4382-90b9-88da9a8e7950').
narrative_ontology:cs_reading_relation('96977df4-a120-4382-90b9-88da9a8e7950', cbdr_principle__voluntary_commitment_reading, coexists_with).
narrative_ontology:cs_axiom('96977df4-a120-4382-90b9-88da9a8e7950', foundational, historical_cumulative_emissions_ground_obligation).
narrative_ontology:cs_axiom_status(historical_cumulative_emissions_ground_obligation, holdable).
narrative_ontology:cs_axiom_grounding('96977df4-a120-4382-90b9-88da9a8e7950', historical_cumulative_emissions_ground_obligation, deontological).
narrative_ontology:cs_axiom('96977df4-a120-4382-90b9-88da9a8e7950', foundational, loss_and_damage_as_restitution_not_charity).
narrative_ontology:cs_axiom_status(loss_and_damage_as_restitution_not_charity, holdable).
narrative_ontology:cs_axiom_grounding('96977df4-a120-4382-90b9-88da9a8e7950', loss_and_damage_as_restitution_not_charity, deontological).
narrative_ontology:cs_reference_frame('96977df4-a120-4382-90b9-88da9a8e7950', historical_cumulative_responsibility_framework).
narrative_ontology:cs_drift_state('96977df4-a120-4382-90b9-88da9a8e7950', contemporary_paris_agreement_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('96977df4-a120-4382-90b9-88da9a8e7950', '').
narrative_ontology:cs_kernel_id(cbdr_principle__historical_responsibility_reading, cbdr_principle).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(cbdr_principle__historical_responsibility_reading, developing_nations).
narrative_ontology:constraint_beneficiary(cbdr_principle__historical_responsibility_reading, climate_vulnerable_populations).
narrative_ontology:constraint_victim(cbdr_principle__historical_responsibility_reading, developed_nations_high_carbon_emitters).
narrative_ontology:constraint_victim(cbdr_principle__historical_responsibility_reading, fossil_fuel_dependent_industries).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CLIMATE-VULNERABLE DEVELOPING NATIONS (SNARE) — Structurally trapped by geographic exposure to climate impacts and lack of capital for adaptation. This reading locks them into victim status while simultaneously requiring them to meet emissions caps despite minimal historical contribution. The constraint extracts compliance from those least responsible and least capable of meeting obligations. No exit: geographic and economic barriers to migration, adaptation financing dependent on developed-nation transfers that remain inadequate and conditional.
constraint_indexing:constraint_classification(cbdr_principle__historical_responsibility_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: MIDDLE-INCOME DEVELOPING NATIONS / EMERGING EMITTERS (TANGLED ROPE) — Experience genuine coordination function (technology transfer, adaptation finance) alongside extraction (binding emissions caps despite lower historical responsibility, capacity constraints). Can develop renewable infrastructure if financing is provided, but also face pressure to decarbonize faster than developed nations did at their development stage. Mixed: some benefit from technology cooperation, significant extraction from accelerated transition obligations without proportional historical responsibility.
constraint_indexing:constraint_classification(cbdr_principle__historical_responsibility_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: DEVELOPED NATIONS ACCEPTING CBDR (ROPE) — Nations that recognize historical responsibility benefit from coordination: international credibility, green technology leadership, access to global carbon markets, and co-benefits (public health from cleaner air, energy security from domestic renewables). This perspective sees CBDR as a coordination mechanism enabling climate action, not as extraction. Net beneficiary via reputational and economic gains from first-mover advantage in green technology and market positioning.
constraint_indexing:constraint_classification(cbdr_principle__historical_responsibility_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: DEVELOPED NATIONS RESISTING CBDR (SNARE FROM THEIR PERSPECTIVE) — High-carbon industries and nations that refuse to internalize historical responsibility experience the constraint as pure extraction: binding emissions caps, carbon pricing, loss/damage financing obligations, and stranded assets without reciprocal benefit or exit. Powerful agents experiencing forced extraction — they have exit capacity (technological, economic, political) but active enforcement (Paris Agreement enforcement, trade sanctions, reputational pressure) suppresses exit. This is a snare experienced by powerful actors, not powerless ones. The suppression mechanism is institutional rather than material.
constraint_indexing:constraint_classification(cbdr_principle__historical_responsibility_reading, snare,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: FOSSIL FUEL INDUSTRIES & WORKERS (TANGLED ROPE) — Experience constraint as both coordination and extraction. Genuine coordination function: just transition programs, worker retraining, community investment in renewable infrastructure. Significant extraction: stranded assets, supply-chain collapse, career disruption without guaranteed alternative employment. Organized agents (unions, industry associations) have some agency to negotiate transition terms but face structural suppression from the energy transition's pace and pressure.
constraint_indexing:constraint_classification(cbdr_principle__historical_responsibility_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: MULTILATERAL CLIMATE FINANCE INSTITUTIONS (TANGLED ROPE) — Green Climate Fund, World Bank Green Finance, regional development banks experience coordination (allocating adaptation finance to vulnerable nations) alongside extraction (pressure to provide transfers without commensurate funding from developed nations, governance capture by donor interests, administrative overhead). Constrained by political pressure from developed nations to limit transfers and by sovereignty claims of developing nations to receive funds without external conditionality.
constraint_indexing:constraint_classification(cbdr_principle__historical_responsibility_reading, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: HISTORICAL EMISSIONS ACCOUNTING APPARATUS (PITON) — The technical infrastructure for measuring cumulative historical emissions (carbon cycle models, attribution studies, temperature-to-emissions conversion) has degraded from its primary epistemic function into a performative legitimacy mechanism. High theater_ratio reflects that accounting frameworks are wielded to justify positions rather than to adjudicate responsibility. Continued through institutional inertia: developed nations cite measurement uncertainty to defer obligations; developing nations cite methodological gaps to demand higher compensation. The apparatus persists but has lost primary function.
constraint_indexing:constraint_classification(cbdr_principle__historical_responsibility_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER / NATURAL LAW FRAMING (MOUNTAIN) — From a thermodynamic/civilizational perspective, climate physics creates an immutable binding: the total carbon budget for limiting warming is fixed regardless of who emitted it historically. From this view, the constraint is natural law — the cumulative physics of atmospheric CO₂ does not negotiate. However, this perspective risks naturalizing what is actually a contestable institutional reading (the historical responsibility framing) that benefits certain parties. The engine's false summit detector will identify this as naturalization of a contingent commitments-system reading.
constraint_indexing:constraint_classification(cbdr_principle__historical_responsibility_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(cbdr_principle__historical_responsibility_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(cbdr_principle__historical_responsibility_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(cbdr_principle__historical_responsibility_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(cbdr_principle__historical_responsibility_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(cbdr_principle__historical_responsibility_reading, TR),
    TR >= 0.70.

:- end_tests(cbdr_principle__historical_responsibility_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high, increasing over the interval. The historical-responsibility reading establishes binding obligations on developed nations without reciprocal agreement from those nations. Initial extractiveness (0.35) reflects weak enforcement in the 1990s UNFCCC period. Extractiveness rises to 0.58 by the present as Paris Agreement mechanisms (nationally determined contributions, carbon markets, loss/damage funds) operationalize the binding obligations and create real costs (stranded assets, carbon pricing, transfer obligations). The metric captures the asymmetry: developed nations did not agree to binding responsibilities but find themselves constrained by institutional consensus around the historical-responsibility framing. Suppression (0.62): High and rising. The constraint suppresses developed-nation exit routes through: (1) institutional pressure (treaty enforcement, reputational costs of withdrawal, trade sanctions); (2) technological lock-in (renewable infrastructure requires long-term commitment); (3) domestic political constituencies (climate movements, green businesses) that have internalized the reading into stated national identity. The rising trajectory (0.45→0.62) reflects increasing institutional enforcement capacity. Theater ratio (0.48): Moderate, declining slightly. The measurement indicates that the historical-responsibility mechanism is increasingly functional (lower theater) compared to earlier symbolic commitments. Early CBDR declarations (1992-2005) were largely performative; current loss/damage mechanisms and climate finance disbursements have higher functional content. However, theater remains significant (0.48): measurement ambiguities in emissions accounting (production vs consumption basis, baseline selection) allow some escape routing; loss/damage financing remains underfunded relative to stated obligations; adaptation finance often comes with conditionality that exceeds the historical-responsibility principle's warrant.
 *
 * PERSPECTIVAL GAP:
 *   The constraint manifests as fundamentally different types across structural positions. Vulnerable developing nations see a snare (trapped without exit, bearing emissions constraints despite minimal responsibility). Fossil fuel industries see a snare from the resistance perspective (powerful agents experiencing forced extraction). Accepting developed nations see rope (coordination benefit from technology leadership and green market positioning). Climate-vulnerable populations see snare from within their own nations (localized extraction despite national-level beneficiary status). The accounting apparatus sees piton (performative framework sustaining itself through institutional inertia). The analytical observer risks mountain (naturalizing the reading as inevitable physics). This perspectival spread (5 distinct types with snare, rope, and piton representations) is characteristic of tangled_rope constraints at the base level—the claimed type resolves the structure by recognizing both genuine coordination (technology transfer, adaptation finance) and asymmetric extraction (binding obligations without reciprocal agreement, unequal burden-sharing).
 *
 * DIRECTIONALITY LOGIC:
 *   This reading produces asymmetric directionality based on structural position. Developed nations that resist the reading experience d ≈ 0.80-0.95 (nearly full target, bearing extraction without benefit); developed nations that accept the reading experience d ≈ 0.30-0.40 (mixed, with some coordination benefit from technology leadership); developing nations experience d ≈ 0.10-0.25 (primarily beneficiary, though also constrained by emissions caps). The engine computes chi from these d values and the base extractiveness. For a resisting developed nation at powerful/mobile, chi ≈ 0.58 × 0.55 × 1.2 ≈ 0.38 (snare territory from their perspective). For a developing nation at moderate/constrained, chi ≈ 0.58 × 0.70 × 1.2 ≈ 0.49 (tangled_rope). The directionality values encode whether the agent benefits from or bears the historical-responsibility framing.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy (classification ambiguity at ε > 0.70) does not arise here because extractiveness remains at 0.58 (below the 0.70 threshold). However, a sibling story modeling the voluntary-commitment reading would likely show higher extractiveness (developing nations bear emissions constraints without compensation) and would require mandatrophy resolution. The current story resolves the mandatrophy by showing that at 0.58, tangled_rope is stable: genuine coordination functions (technology transfer, climate finance, adaptive capacity building) are present and substantive enough to prevent pure snare classification, while asymmetric extraction (binding on developed nations without reciprocal agreement, unequal burden distribution) is present and substantive enough to prevent pure rope classification. The constraint requires active enforcement (developed nations would prefer voluntary mechanisms) and has identifiable beneficiaries (developing nations, climate-vulnerable populations) and victims (high-carbon industries, developed nations' fossil-dependent workers), confirming tangled_rope gates.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    historical_emissions_attribution_ambiguity,
    'What methodology defines ''cumulative historical emissions'': pre-industrial baseline, post-1850 industrialization, post-1990 UNFCCC entry, post-2000 growth acceleration? Does responsibility attach to production or consumption emissions?',
    'Comparative climate justice literature; empirical analysis of how different methodologies redistribute responsibility between Global North and South; examination of which methodologies are cited by developed vs developing nations in treaty negotiations',
    'Different baselines produce different responsibility allocations. Pre-1850 baseline: virtually all responsibility on developed nations. Post-1990 baseline: emerging economies gain significant responsibility share. Consumption-basis (carbon embedded in imports) vs production-basis (territorial emissions) can shift responsibility by 20-40 percentage points for trade-dependent nations.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(historical_emissions_attribution_ambiguity, conceptual, 'Methodological ambiguity in defining cumulative historical emissions').

omega_variable(
    loss_and_damage_financing_adequacy,
    'Is loss/damage financing (separate from adaptation finance) treated as obligation or charity? What trigger mechanisms convert the historical responsibility reading into actual transfer obligations?',
    'Analysis of loss/damage provisions in Paris Agreement and subsequent COPs; comparison of pledged vs disbursed climate finance; documentation of conditionality attached to transfers (governance reforms, structural adjustment, technology adoption)',
    'If loss/damage is treated as obligation: developed nations enter victim set (binding financial transfers). If treated as voluntary assistance: obligation remains weak and transfers remain inadequate. The reading''s extractiveness depends critically on whether loss/damage financing is institutionalized as binding or performative.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(loss_and_damage_financing_adequacy, empirical, 'Whether loss/damage financing becomes binding institutional obligation or remains performative pledge').

omega_variable(
    developed_nation_exit_via_decoupling,
    'Can developed nations escape binding historical-responsibility obligations by exporting emissions-intensive manufacturing to developing nations (carbon leakage), then claiming they have ''decoupled'' emissions from GDP?',
    'Consumption-based emissions accounting (carbon footprint of imports) vs production-based accounting; analysis of whether carbon border adjustment mechanisms (CBAMs) or carbon tariffs effectively internalize this escape route; examination of whether technology transfer obligations close the loophole',
    'If decoupling via offshoring succeeds: the constraint''s suppression of developed nations is weakened (they retain exit via arbitrage). If CBAMs succeed: suppression is strengthened (no exit). The reading''s snare classification for resisting developed nations depends on whether exit routes remain available.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(developed_nation_exit_via_decoupling, empirical, 'Whether developed nations can escape historical responsibility via carbon leakage and production offshoring').

omega_variable(
    reading_kernel_ambiguity,
    'Is the CBDR principle itself a binding legal kernel (binding on signatories to operationalize), or is it a non-binding declaration that leaves signatories free to interpret ''differentiated responsibilities'' as voluntary?',
    'Treaty law interpretation: comparison of CBDR language in UNFCCC vs Paris Agreement; analysis of ICJ rulings on treaty enforceability; examination of compliance records and dispute resolution outcomes',
    'If CBDR is binding: the historical-responsibility reading grounds mandatory obligations on developed nations. If CBDR is non-binding: the historical-responsibility reading is an aspirational framing that lacks enforcement. This omega documents the kernel-level ambiguity that creates the possibility of the sibling voluntary-commitment reading coexisting with this reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_kernel_ambiguity, conceptual, 'Whether CBDR principle itself is legally binding or non-binding declaration').

omega_variable(
    identity_lock_in_national_sovereignty,
    'Do developed nations experience the historical-responsibility reading as extraction they could escape (trapped by enforcement), or as a reading of national commitments they have internalized into sovereignty/identity?',
    'Analysis of developed-nation climate diplomacy rhetoric; examination of whether nations frame CBDR as external constraint or as expression of their own values; documentation of internal political debate (domestic constituencies supporting vs resisting the reading)',
    'If identity-locked (reading is part of stated national identity): developed nations experience constrained exit (could theoretically withdraw but that contradicts stated values). If externally enforced: developed nations experience trapped exit (withdrawal prevented by institutional pressure). This affects classification of the developed-nation perspective from snare to potentially rope if they have internalized the reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_in_national_sovereignty, conceptual, 'Whether developed nations'' relationship to historical-responsibility reading is identity-locked or externally enforced').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(cbdr_principle__historical_responsibility_reading, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cbdr_hist_tr_t0, cbdr_principle__historical_responsibility_reading, theater_ratio, 0, 0.55).
narrative_ontology:measurement(cbdr_hist_tr_t5, cbdr_principle__historical_responsibility_reading, theater_ratio, 5, 0.52).
narrative_ontology:measurement(cbdr_hist_tr_t10, cbdr_principle__historical_responsibility_reading, theater_ratio, 10, 0.48).

% Extraction over time
narrative_ontology:measurement(cbdr_hist_be_t0, cbdr_principle__historical_responsibility_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(cbdr_hist_be_t5, cbdr_principle__historical_responsibility_reading, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(cbdr_hist_be_t10, cbdr_principle__historical_responsibility_reading, base_extractiveness, 10, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(cbdr_hist_su_t0, cbdr_principle__historical_responsibility_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(cbdr_hist_su_t5, cbdr_principle__historical_responsibility_reading, suppression_requirement, 5, 0.55).
narrative_ontology:measurement(cbdr_hist_su_t10, cbdr_principle__historical_responsibility_reading, suppression_requirement, 10, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(cbdr_principle__historical_responsibility_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(cbdr_principle__historical_responsibility_reading, 0.12).
narrative_ontology:affects_constraint(cbdr_principle__historical_responsibility_reading, voluntary_commitment_reading).
narrative_ontology:affects_constraint(cbdr_principle__historical_responsibility_reading, carbon_leakage_mechanism).
narrative_ontology:affects_constraint(cbdr_principle__historical_responsibility_reading, loss_and_damage_finance_adequacy).
narrative_ontology:affects_constraint(cbdr_principle__historical_responsibility_reading, just_transition_infrastructure).

% DUAL FORMULATION NOTE:
% The CBDR principle contains two structurally distinct constraints corresponding to two sibling readings. The historical-responsibility reading (this file) grounds obligations in past emissions; the voluntary-commitment reading grounds them in current pledges. These are not the same constraint viewed from different angles—they produce different victim/beneficiary sets, different ε values, and different enforcement mechanisms. Both are generated as separate constraint stories linked by network edges. The epsilon values differ because the causal mechanism of obligation-grounding differs: historical responsibility creates binding enforcement; voluntary commitment creates conditional, renegotiable obligations.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(cbdr_principle__historical_responsibility_reading, institutional, 0.42).
constraint_indexing:directionality_override(cbdr_principle__historical_responsibility_reading, powerful, 0.82).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
