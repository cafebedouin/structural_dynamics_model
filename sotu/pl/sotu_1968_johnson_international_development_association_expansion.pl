% ============================================================================
% CONSTRAINT STORY: sotu_1968_johnson_international_development_association_expansion
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sotu_1968_johnson_international_development_association_expansion, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: sotu_1968_johnson_international_development_association_expansion
 *   human_readable: IDA Expansion as Cold War Development Mechanism
 *   domain: economics/geopolitics
 *
 * SUMMARY:
 *   The International Development Association expansion under the Johnson
 *   administration (1968) represents a structurally hybrid constraint
 *   combining genuine multilateral development coordination with asymmetric
 *   geopolitical extraction. The mechanism channels U.S. resources through
 *   the IDA and Asian Development Bank to developing nations under a
 *   'self-help' principle that functions simultaneously as development
 *   philosophy and Cold War containment strategy. Benefits flow to recipient
 *   country elites (who gain capital and state capacity), U.S. policy
 *   establishment (who gain geopolitical influence and prevent Soviet
 *   penetration), and U.S. exporters (who gain markets through
 *   aid-conditioned procurement). Costs fall on U.S. taxpayers (who fund the
 *   program with minimal transparency), recipient country rural poor (who
 *   experience conditionality-imposed policy constraints and structural
 *   adjustment), and competing aid recipients excluded from U.S. priorities.
 *   The constraint exhibits high theater ratio (0.62) because the 'self-help'
 *   development narrative obscures the conditionality mechanism and
 *   geopolitical extraction logic, both from U.S. public discourse and from
 *   recipient nation publics. The rising extractiveness from 0.38 to 0.52
 *   reflects accumulating evidence that conditionality imposed mounting
 *   policy constraints on recipient nations without proportional development
 *   benefits.
 *
 * KEY AGENTS:
 *   - U.S. Taxpayers: Primary target (powerless/trapped) — funding source with no transparency or exit option
 *   - Recipient Country Elites: Primary beneficiary but constrained (organized/constrained) — gain capital and legitimacy but accept conditionality on economic policy
 *   - U.S. Foreign Policy Establishment: Primary beneficiary with full agency (institutional/arbitrage) — implements containment strategy through aid allocation
 *   - Recipient Country Rural Poor: Secondary victim (moderate/constrained) — experience aid benefits through some infrastructure projects but bear costs of conditionality-imposed structural adjustment
 *   - Private U.S. Exporters and Contractors: Secondary beneficiary (powerful/arbitrage) — profit from aid-conditioned procurement and export markets
 *   - IDA Institution: Constrained institutional actor (institutional/constrained) — mandated to develop poor nations but operationally constrained by donor voting structure and U.S. policy direction
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sotu_1968_johnson_international_development_association_expansion, 0.52).
domain_priors:suppression_score(sotu_1968_johnson_international_development_association_expansion, 0.58).
domain_priors:theater_ratio(sotu_1968_johnson_international_development_association_expansion, 0.62).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sotu_1968_johnson_international_development_association_expansion, extractiveness, 0.52).
narrative_ontology:constraint_metric(sotu_1968_johnson_international_development_association_expansion, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(sotu_1968_johnson_international_development_association_expansion, theater_ratio, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sotu_1968_johnson_international_development_association_expansion, tangled_rope).
narrative_ontology:human_readable(sotu_1968_johnson_international_development_association_expansion, "IDA Expansion as Cold War Development Mechanism").
narrative_ontology:topic_domain(sotu_1968_johnson_international_development_association_expansion, "economics/geopolitics").

domain_priors:requires_active_enforcement(sotu_1968_johnson_international_development_association_expansion).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sotu_1968_johnson_international_development_association_expansion, us_foreign_policy_establishment).
narrative_ontology:constraint_beneficiary(sotu_1968_johnson_international_development_association_expansion, recipient_country_elites).
narrative_ontology:constraint_beneficiary(sotu_1968_johnson_international_development_association_expansion, private_us_exporters).
narrative_ontology:constraint_victim(sotu_1968_johnson_international_development_association_expansion, us_taxpayers).
narrative_ontology:constraint_victim(sotu_1968_johnson_international_development_association_expansion, recipient_country_rural_poor).
narrative_ontology:constraint_victim(sotu_1968_johnson_international_development_association_expansion, competing_aid_recipients).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: U.S. TAXPAYER (SNARE) — No exit from funding obligation; extraction occurs through taxation with minimal transparency about aid efficacy or conditionality mechanisms. Powerless to negotiate terms or verify that funds reach intended beneficiaries. Bears cost of aid program while geopolitical benefits accrue to policy establishment.
constraint_indexing:constraint_classification(sotu_1968_johnson_international_development_association_expansion, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: RECIPIENT COUNTRY RURAL POOR (TANGLED ROPE) — Constrained by geography and limited land access. Aid reaches some through infrastructure projects and institution-building, but extraction occurs through conditionality: structural adjustment requirements, land concessions to exporters, and reorientation of subsistence agriculture toward cash crops. Mixed experience — some coordination benefit from roads and clinics, but asymmetric extraction through imposed economic policies.
constraint_indexing:constraint_classification(sotu_1968_johnson_international_development_association_expansion, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: U.S. FOREIGN POLICY ESTABLISHMENT (ROPE) — Experiences the constraint as pure coordination: allocating development aid is how containment strategy is implemented. Aid channels geopolitical influence, secures allied leadership, and prevents Soviet penetration of developing nations. Full beneficiary with arbitrage options (can redirect aid, adjust conditionality, shift donors). Net extraction flows toward this agent.
constraint_indexing:constraint_classification(sotu_1968_johnson_international_development_association_expansion, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: RECIPIENT COUNTRY GOVERNMENT ELITES (TANGLED ROPE) — Organized actors who benefit from development aid as legitimacy and resource base, but constrained by conditionality imposed by IDA and U.S. policy. Can coordinate domestically but must accept policy direction from external actors. Aid enables state capacity-building but extraction occurs through policy conditionality that favors external interests (U.S. exporters, international lending institutions). High agency but asymmetric costs.
constraint_indexing:constraint_classification(sotu_1968_johnson_international_development_association_expansion, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: PRIVATE U.S. EXPORTERS AND CONTRACTORS (ROPE) — Clear beneficiary from aid-conditioned procurement requirements. Aid-funded infrastructure projects contract to U.S. firms; debt service creates markets for U.S. exports; technical assistance requirements drive purchases of U.S. technology and consulting. Experiences constraint as coordination mechanism that opens markets. Powerful actors with exit options to other export markets.
constraint_indexing:constraint_classification(sotu_1968_johnson_international_development_association_expansion, rope,
    context(agent_power(powerful),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: IDA AS AUTONOMOUS INSTITUTION (TANGLED ROPE) — Institutional actor that experiences tension between mandate (development for self-help nations) and structural role (enforcement of conditionality favoring U.S. interests). Coordinates multilateral aid distribution but constrained by voting structure dominated by wealthy nations and U.S. policy direction. Benefits from institutional growth but extraction occurs through delegitimization when aid fails to reach poor or when conditionality harms recipient economies.
constraint_indexing:constraint_classification(sotu_1968_johnson_international_development_association_expansion, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (TANGLED ROPE) — From civilizational scope, the constraint exhibits genuine coordination function (multilateral development) layered with asymmetric extraction (Cold War leverage, conditionality favoring external interests, debt traps). Coordination mechanism cannot be cleanly separated from extraction mechanism — they are structurally fused. The 'self-help' principle is authentic but embedded in conditionality that constrains recipient autonomy.
constraint_indexing:constraint_classification(sotu_1968_johnson_international_development_association_expansion, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sotu_1968_johnson_international_development_association_expansion_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(sotu_1968_johnson_international_development_association_expansion, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(sotu_1968_johnson_international_development_association_expansion, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(sotu_1968_johnson_international_development_association_expansion, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(sotu_1968_johnson_international_development_association_expansion_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high, rising from 0.38. Initial extractiveness reflects genuine development coordination function—capital does flow to recipients, some institutional capacity is built. Rising extractiveness reflects accumulating evidence that conditionality imposes policy constraints (structural adjustment, orientation toward export agriculture, debt service prioritization) that constrain recipient autonomy and long-term development. The mechanism exhibits both coordination (multilateral development lending) and extraction (geopolitical leverage and market control). Suppression (0.58): Moderate-high. Multiple barriers prevent recipient nations from exiting or renegotiating terms: debt dependency once aid is accepted, geopolitical consequences of rejecting U.S.-aligned lending, limited alternative sources of development capital, and domestic political consequences of pursuing independent development paths (coups, sanctions, loss of security support). Theater ratio (0.62): Moderate-high, rising from 0.48. The 'self-help' development narrative obscures conditionality mechanism from both U.S. domestic discourse and recipient nation publics. Geopolitical extraction rationale (containment) is largely classified/hidden. Aid bureaucracy (IDA, ADB) provides institutional cover that depoliticizes what is fundamentally a Cold War allocation mechanism. Rising theater reflects increasing sophistication of conditionality rhetoric as extraction becomes more visible.
 *
 * PERSPECTIVAL GAP:
 *   The critical gap is between U.S. policy perspective (Rope: pure coordination of containment strategy) and recipient poor perspective (Snare/Tangled Rope: extraction masked as development). The gap reflects asymmetric information: U.S. policy establishment sees geopolitical benefits clearly; recipient populations are told the aid is development assistance. IDA institution occupies a liminal position — it genuinely believes in its development mandate, but its operational role is enforcing U.S.-aligned conditionality. The U.S. taxpayer is wholly excluded from the benefit flow despite bearing costs, suggesting maximum opacity in the mechanism's communication structure.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values are determined by agent position relative to extraction flow. U.S. taxpayers are full targets (d=0.95) — pay but don't benefit. U.S. policy establishment are full beneficiaries (d=0.05) — benefit from geopolitical leverage. Recipient elites are symmetric (d=0.50) — gain capital but lose autonomy. Rural poor are targets (d=0.85) — constrained by conditionality but receive some infrastructure benefit. Private exporters are beneficiaries (d=0.10) — markets without cost. IDA institution is symmetric (d=0.52) — mandated to develop but operationally serving U.S. strategy. The beneficiary/victim declarations map directly to these structural flows: those declared as beneficiaries have low d values (benefit from extraction), those declared as victims have high d values (bear costs). The constraint is not zero-sum — genuine development resources flow, but extraction of policy autonomy and geopolitical alignment occurs alongside.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION: The constraint resolves as genuine Tangled Rope rather than false snare. Evidence for hybrid classification: (1) Multilateral development lending is structurally real — capital does flow, institutions are built, roads are constructed; (2) Conditionality enforcement is structurally real — structural adjustment is imposed, policy autonomy is constrained, geopolitical alignment is incentivized; (3) Both functions are necessary to the constraint's operation — the development mandate provides legitimacy and recipient participation, while the conditionality provides the extraction mechanism. The constraint cannot be cleanly decomposed into 'development' (rope) + 'geopolitical leverage' (snare). They are operationally fused — the same mechanism serves both functions. Mandatrophy is resolved by recognizing that the 'self-help' principle is authentic (recipients do participate in planning, do gain capacity) but embedded in asymmetric constraint (conditionality is non-negotiable, U.S. geopolitical interests are primary). The mechanism is not 'coordination falsely labeled as extraction' nor 'extraction falsely labeled as coordination' — it is genuine hybrid with both functions operating simultaneously through the same institutional structure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    self_help_principle_definition,
    'Does ''self-help'' conditionality genuinely enhance recipient nation capacity, or does it primarily extract policy concessions?',
    'Comparative analysis of recipient countries that accepted vs. rejected IDA conditionality; longitudinal tracking of institutional capacity growth, debt burdens, and policy autonomy after aid programs; examination of whether conditionality-imposed policies correlate with long-term development outcomes or with geopolitical alignment with U.S. interests',
    'If self-help enhances capacity: constraint is genuine Tangled Rope with coordination function primary. If conditionality primarily extracts policy: constraint is Snare with coordination function as cover story. This determines whether mandatrophy resolves toward coordination or extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(self_help_principle_definition, empirical, 'Whether self-help conditionality builds recipient capacity or extracts policy alignment').

omega_variable(
    debt_sustainability_design,
    'Were IDA loan terms designed to be sustainable for recipient nations, or structured to create ongoing debt dependency that constrains future autonomy?',
    'Analysis of default rates, debt service ratios, and refinancing patterns; comparison of loan terms to recipient nation''s estimated repayment capacity; historical documents examining design intent for IDA conditionality structure',
    'If designed sustainable: extraction component is moderate extraction for coordinating development. If designed for dependency: extraction component is primary — debt trap is intentional structural feature. Moves constraint classification toward Snare.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(debt_sustainability_design, empirical, 'Whether IDA debt structure was designed for sustainability or dependency').

omega_variable(
    geopolitical_alignment_correlation,
    'Does IDA aid allocation correlate more strongly with recipient nation development need or with geopolitical alignment with U.S. Cold War strategy?',
    'Regression analysis of IDA aid flows against measures of need (poverty rate, institutional capacity, infrastructure gap) vs. geopolitical alignment (voting alignment in UN General Assembly, hosting of U.S. military bases, anticommunist regime type); time-series analysis of shifts in aid allocation following geopolitical events',
    'If need-based: constraint is genuine Tangled Rope with coordination and asymmetric extraction both present. If alignment-based: extraction mechanism is primary and ''development'' framing is theater. Moves classification toward Snare.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(geopolitical_alignment_correlation, empirical, 'Whether IDA allocation prioritizes development need or geopolitical alignment').

omega_variable(
    recipient_nation_agency_loss,
    'How much policy autonomy did recipient nations lose as a result of IDA conditionality, and did this loss exceed the development benefits gained?',
    'Qualitative analysis of policy space constraints imposed by IDA conditions; comparative study of nations that accepted vs. rejected IDA terms on measures of subsequent policy autonomy; examination of recipient nation government statements about conditionality burden; analysis of whether conditionality-imposed policies were later reversed when nations gained capacity to exit IDA dependence',
    'If autonomy loss > benefits: extraction is primary, constraint is Snare. If benefits > autonomy loss: coordination function dominates, constraint is genuine Tangled Rope. This resolves the mandatrophy question of whether the mechanism is primarily beneficial with extractive side effects, or primarily extractive with coordination cover.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(recipient_nation_agency_loss, empirical, 'Whether policy autonomy loss exceeded development benefits').

omega_variable(
    suppression_mechanism_visibility,
    'How transparent was the conditionality mechanism to U.S. taxpayers and recipient nation populations? Was extraction hidden by theater?',
    'Content analysis of U.S. congressional debate on IDA expansion; examination of recipient nation domestic political discourse about conditionality; comparison of public-facing development narratives with classified/declassified geopolitical rationales for aid allocation',
    'If highly opaque: suppression operates through information asymmetry, extraction is hidden by development theater. If transparent: suppression is negotiated and visible, constraint operates with lower theater. Theater_ratio should be adjusted based on this assessment.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_visibility, empirical, 'Transparency of conditionality mechanism and extraction rationale').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sotu_1968_johnson_international_development_association_expansion, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ida_exp_tr_t0, sotu_1968_johnson_international_development_association_expansion, theater_ratio, 0, 0.48).
narrative_ontology:measurement(ida_exp_tr_t5, sotu_1968_johnson_international_development_association_expansion, theater_ratio, 5, 0.58).
narrative_ontology:measurement(ida_exp_tr_t10, sotu_1968_johnson_international_development_association_expansion, theater_ratio, 10, 0.62).

% Extraction over time
narrative_ontology:measurement(ida_exp_be_t0, sotu_1968_johnson_international_development_association_expansion, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(ida_exp_be_t5, sotu_1968_johnson_international_development_association_expansion, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(ida_exp_be_t10, sotu_1968_johnson_international_development_association_expansion, base_extractiveness, 10, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sotu_1968_johnson_international_development_association_expansion, resource_allocation).
narrative_ontology:affects_constraint(sotu_1968_johnson_international_development_association_expansion, structural_adjustment_debt_conditionality).
narrative_ontology:affects_constraint(sotu_1968_johnson_international_development_association_expansion, cold_war_containment_foreign_aid).
narrative_ontology:affects_constraint(sotu_1968_johnson_international_development_association_expansion, multilateral_institution_voting_structure).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(sotu_1968_johnson_international_development_association_expansion, institutional, 0.52).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
