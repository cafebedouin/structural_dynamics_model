% ============================================================================
% CONSTRAINT STORY: canadian_equalization_regime
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_canadian_equalization_regime, []).

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
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
    narrative_ontology:cs_created_at/2,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: canadian_equalization_regime
 *   human_readable: Canadian Equalization Fiscal Transfer Regime
 *   domain: fiscal_federalism/political_economy
 *
 * SUMMARY:
 *   The Canadian equalization regime is a federal fiscal transfer mechanism
 *   designed to ensure that recipient provinces can deliver comparable public
 *   services despite unequal tax-raising capacity. Established under Section
 *   36 of the 1982 Constitution Act, equalization transfers approximately
 *   $20-24 billion annually from federal revenues to recipient provinces
 *   (Quebec, Atlantic provinces, Manitoba, Saskatchewan). The constraint
 *   exhibits structural features of both coordination (solving the collective
 *   action problem of maintaining comparable service standards across
 *   provinces with unequal resource bases) and extraction (benefiting
 *   recipient provinces and their resource-dependent fiscal positions while
 *   imposing costs on donor provinces). The perspectival gap is acute:
 *   recipient province governments and the federal authority see equalization
 *   as coordinating mechanism essential to federalism; donor province
 *   populations experience it as imposed redistribution with no exit; the
 *   formula apparatus sees itself as increasingly performative; and the
 *   civilizational observer risks naturalizing a contingent institutional
 *   choice as an immutable law of federalism. The theater ratio (0.58)
 *   reflects that equalization political discourse focuses heavily on formula
 *   mechanics, allocation eligibility, and federal-provincial conflict, while
 *   the underlying question—whether equalization actually achieves comparable
 *   service delivery—receives less attention. The extractiveness trajectory
 *   (0.38 → 0.52 over 20 years) shows accumulating extraction as formula
 *   revisions and donor province grievances intensify.
 *
 * KEY AGENTS:
 *   - Recipient Province Governments (Quebec, Atlantic provinces): Primary beneficiaries (institutional/arbitrage) — receive $20-24B annually; gain political legitimacy from equalization principle; maintain revenue streams that would otherwise require provincial tax increases or service cuts
 *   - Donor Province Governments (Alberta, Ontario): Secondary beneficiaries and co-victims (powerful/constrained) — benefit from federal coordination of fiscal federalism; experience extraction through equalization contributions; face political pressure from constituents viewing equalization as unfair
 *   - Donor Province Taxpayers (Alberta, Ontario residents): Primary victims (powerless/trapped) — bear equalization costs with no exit option; participation in allocation decisions is indirect and weak
 *   - Federal Fiscal Authority: Co-beneficiary and enforcer (institutional/arbitrage) — gains legitimacy from equalization's constitutional status; derives coordination authority from managing interprovincial fiscal federation
 *   - Equalization Formula Apparatus: Institutional machinery (institutional/arbitrage) — implements transfer mechanics; increasingly performative as revisions cycle without resolving underlying disputes
 *   - Cross-Provincial Labor Movement: Organized secondary actor (organized/constrained) — benefits from public-service employment funded by equalization; locked into provincial fiscal regimes dependent on transfers
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(canadian_equalization_regime, 0.52).
domain_priors:suppression_score(canadian_equalization_regime, 0.48).
domain_priors:theater_ratio(canadian_equalization_regime, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(canadian_equalization_regime, extractiveness, 0.52).
narrative_ontology:constraint_metric(canadian_equalization_regime, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(canadian_equalization_regime, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(canadian_equalization_regime, tangled_rope).
narrative_ontology:human_readable(canadian_equalization_regime, "Canadian Equalization Fiscal Transfer Regime").
narrative_ontology:topic_domain(canadian_equalization_regime, "fiscal_federalism/political_economy").

domain_priors:requires_active_enforcement(canadian_equalization_regime).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(canadian_equalization_regime, '779bad03-9bcf-4515-b57c-5137a151ce6b').
narrative_ontology:cs_kernel_codification('779bad03-9bcf-4515-b57c-5137a151ce6b', formalized).
narrative_ontology:cs_authority_grounding('779bad03-9bcf-4515-b57c-5137a151ce6b', lineage).
narrative_ontology:cs_interpretation_layer_present('779bad03-9bcf-4515-b57c-5137a151ce6b').
narrative_ontology:cs_created_at('779bad03-9bcf-4515-b57c-5137a151ce6b', '').

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(canadian_equalization_regime, recipient_provinces).
narrative_ontology:constraint_beneficiary(canadian_equalization_regime, federal_coordinating_authority).
narrative_ontology:constraint_victim(canadian_equalization_regime, donor_provinces).
narrative_ontology:constraint_victim(canadian_equalization_regime, inter_provincial_equity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DONOR PROVINCE TAXPAYER (SNARE) — Alberta and Ontario taxpayers bear the extraction with no exit. Equalization is constitutionally mandated; provinces cannot opt out. The individual taxpayer has no mobility within the regime and no meaningful participation in the allocation formula. Bears full cost of redistribution with suppressed alternatives — can only exit through interprovincial migration, which is costly and addresses the symptom rather than the constraint.
constraint_indexing:constraint_classification(canadian_equalization_regime, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: DONOR PROVINCE GOVERNMENT (TANGLED ROPE) — Alberta and Ontario benefit from resource revenues and economic activity that generate federal revenue (which they pay into); they also derive legitimacy from equalization's implicit recognition that unequal capacity is a structural problem. But they experience extraction through equalization payments and formula mechanics that can disadvantage resource-rich provinces during commodity booms. Constrained exit: provinces could theoretically threaten separation or reduced tax collection, but constitutional order and economic integration make exit costly. Mixed extraction and coordination function coexist.
constraint_indexing:constraint_classification(canadian_equalization_regime, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: RECIPIENT PROVINCE GOVERNMENT (ROPE) — Quebec, Atlantic provinces experience equalization primarily as coordination mechanism enabling comparable service delivery. The regime solves a genuine collective action problem: without equalization, poorer provinces would underfund public services (healthcare, education, infrastructure), creating interprovincial inequality and potential Balkanization. The recipient province experiences this as low-extraction coordination. Exit option is arbitrage: recipient provinces can lobby to maintain/increase their allocation formula advantage or can extract additional federal transfers through political bargaining. Net beneficiary position.
constraint_indexing:constraint_classification(canadian_equalization_regime, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: LABOR MOVEMENT (ORGANIZED/CONSTRAINED) (TANGLED ROPE) — Cross-provincial unions see equalization as both coordinating workers' access to public services (healthcare, education funded by equalization transfers) AND extracting worker surplus through resource-sector rent-capture by provincial capitals. The regime funds public employment but locks provinces into revenue-dependent fiscal positions. Constrained exit: labor can organize within provinces or advocate for formula changes, but cannot exit the interprovincial regulatory framework.
constraint_indexing:constraint_classification(canadian_equalization_regime, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: EQUALIZATION FORMULA APPARATUS (PITON) — The technical machinery for calculating recipient status and transfer amounts has become increasingly performative. Formula has been revised 11+ times since 1982. Each revision triggers federal-provincial conflict while preserving the underlying structure: recipient provinces find ways to remain eligible (resource-accounting manipulations, creative fiscal accounting), and the formula is updated to close loopholes, creating a treadmill. The theater ratio is high because much of the political attention goes to formula mechanics rather than to the underlying question of whether equalization achieves its stated goal (comparable service delivery). The functional core — ensuring minimum service standards — persists through inertia and constitutional commitment rather than because the mechanism works smoothly.
constraint_indexing:constraint_classification(canadian_equalization_regime, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, unequal provincial revenue capacity is a structural feature of federalism: geography determines resource endowments, and federalism distributes sovereignty by territory. Some equalization mechanism appears immutable — without it, federalism dissolves into incompatible regional standards. This perspective sees equalization as a natural law of federalism. However, the structural data contradicts the mountain classification: identifiable beneficiaries (recipient provinces, federal authority), suppression mechanisms (constitutional obligation, political path-dependency), and periodic formula revision (evidence of contingency) all suggest this is a constructed constraint, not a natural law. The engine will compute this as a false summit, revealing that 'inherent to federalism' naturalizes a specific institutional choice.
constraint_indexing:constraint_classification(canadian_equalization_regime, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(canadian_equalization_regime_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(canadian_equalization_regime, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(canadian_equalization_regime, TypeOther, context(agent_power(powerful), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(canadian_equalization_regime, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(canadian_equalization_regime, TR),
    TR >= 0.70.

:- end_tests(canadian_equalization_regime_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The regime exhibits asymmetric extraction: recipient provinces gain stable revenue streams with low political cost (equalization is constitutionally mandated, not subject to annual appropriation debate); donor provinces bear costs imposed by constitutional obligation with limited political recourse. The extraction is not maximal (0.66+) because recipient provinces do face service-delivery accountability and cannot infinitely expand spending without fiscal constraints. Suppression (0.48): Moderate. Constitutional entrenchment creates high barriers to donor province exit—unilateral withdrawal would trigger federal-provincial crisis. But suppression is not total: donor provinces can lobby for formula changes (which occur regularly), can reduce tax collection (though rarely used), or can adjust fiscal priorities. The constraint operates through constitutional obligation rather than direct coercion, making suppression moderate. Theater ratio (0.58): Moderate-high. Equalization political debate focuses heavily on formula technicalities—how to measure recipient status, which revenues count, adjustment mechanisms—rather than on whether the regime achieves its stated goal of comparable service delivery. Formula revisions (11+) since 1982 respond to political pressure and perceived inequities without fundamentally rethinking the mechanism. The theater has risen over time as complexity increases and formula becomes a perpetual source of federal-provincial conflict.
 *
 * PERSPECTIVAL GAP:
 *   The regime's perspectival gap is extreme because it conflates two distinct structural relationships. Recipient province governments experience genuine coordination benefit (equalization solves the collective action problem of unequal capacity). Donor province taxpayers experience pure extraction with no coordination benefit. The federal authority manages this gap through constitutional entrenchment (making exit costly) and through political framing that emphasizes the coordination function ('ensuring comparable services') while downplaying the asymmetric extraction. The false summit occurs when the analytical observer adopts the federal authority's framing and naturalizes equalization as inherent to federalism, rather than examining whether the specific transfer amounts, formulas, and enforcement mechanisms benefit identifiable agents.
 *
 * DIRECTIONALITY LOGIC:
 *   The divergence in directionality across perspectives is the key analytical signal. Recipient province governments as beneficiaries with arbitrage exit options (can lobby for formula changes, can adjust spending) experience d ≈ 0.15-0.20, producing low effective extraction chi ≈ 0.08-0.12 (visible as Rope). Donor province taxpayers as trapped victims experience d ≈ 0.95, producing high chi ≈ 0.75+ (visible as Snare). Donor province governments as powerful but constrained actors experience d ≈ 0.48-0.55, producing moderate chi ≈ 0.40-0.48 (visible as Tangled Rope). The federal authority as institutional beneficiary with arbitrage options experiences d ≈ 0.10-0.15, producing low chi (Rope). This divergence is not measurement error—it reflects genuine structural differences in exit capacity and benefit flow. The engine's directionality derivation chain correctly captures these differences.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY AT THE REGIME LEVEL: The Canadian equalization regime resolves the mandatrophy by showing that all three perspectives (Rope for beneficiaries, Tangled Rope for constrained donors, Snare for trapped taxpayers) are legitimate readings of the same constraint. The mandatrophy is not 'which type is correct?' but 'which structural position are you in?'. The analytical observer's temptation to call equalization a Mountain (natural law of federalism) is a false summit—constitutional entrenchment and political legitimacy are not the same as immutability. The constraint's changeability is visible in: (1) 11+ formula revisions since 1982, indicating the specific mechanism is contingent; (2) rising donor province political pressure, indicating the constraint faces durable opposition; (3) the existence of counterfactual arrangements (unequal provincial standards, provincial independence, federal standardization), indicating alternatives are conceptually possible. The false summit is socially potent because naturalizing equalization as 'inherent to federalism' discourages examination of whether it achieves its stated goal (comparable service delivery) or merely redistributes rents to recipient provinces and federal apparatus while suppressing alternative solutions to unequal capacity.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    recipient_status_endogeneity,
    'Does recipient province status create permanent dependency on equalization, or do provinces cycle in and out of recipient status as economic conditions change?',
    'Historical data on provincial transitions (Ontario entered recipient status 2009-2010; Saskatchewan, Manitoba have oscillated); correlation between equalization receipt and fiscal management quality; analysis of whether recipient provinces can permanently exit recipient status without federal formula change',
    'If dependency permanent: extraction component dominates (Snare/Tangled Rope weight increases). If cyclical: coordination component dominates (Rope weight increases); equalization appears as economic stabilization rather than permanent transfer.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(recipient_status_endogeneity, empirical, 'Whether recipient province status is permanent or cyclical').

omega_variable(
    service_delivery_equalization_causation,
    'Does equalization transfer causally improve public service quality/access in recipient provinces, or is service quality determined primarily by provincial fiscal management and policy choices independent of transfer magnitude?',
    'Regression analysis of healthcare/education/infrastructure outcomes in recipient provinces controlling for equalization amount; cross-sectional comparison of similar-GDP provinces with and without equalization; analysis of whether increasing equalization transfers correlates with improved outcomes',
    'If causally effective: coordination function is real (Rope/Tangled Rope confirmed). If not: equalization is primarily redistribution without functional coordination benefit (Snare from donor perspective; Piton if mechanisms persist despite low effectiveness).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(service_delivery_equalization_causation, empirical, 'Causal relationship between equalization transfers and service delivery outcomes').

omega_variable(
    donor_province_political_economy_shift,
    'As donor provinces accumulate fiscal grievances (Alberta''s ''separate and unequal'' narrative, Ontario''s claim to contributor status), do they actively threaten equalization regime exit or merely tolerate it as constitutional obligation?',
    'Political discourse analysis of donor province rhetoric 2000-2026; tracking of serious proposals for equalization modification or opt-out; measurement of anti-equalization sentiment in donor province polling; analysis of whether political pressure has substantively changed transfer amounts or formulas',
    'If active threat: donor provinces have constrained but real exit pressure; regime classification shifts toward Tangled Rope for donor institutions. If passive tolerance: regime has higher suppression (trapped classification for donor agent); extraction mechanism is more durable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(donor_province_political_economy_shift, empirical, 'Donor province political pressure on equalization regime durability').

omega_variable(
    formula_revision_ratchet_mechanism,
    'Do equalization formula revisions progressively increase recipient province transfer amounts (ratchet effect), or do revisions produce statistically neutral redistributions that preserve overall transfer levels?',
    'Historical analysis of real per-capita transfer amounts by province across formula revision dates; decomposition of formula changes into revenue-base changes vs allocation rule changes; comparison of pre- and post-revision transfer trajectories',
    'If ratchet upward: extraction mechanism is strengthening over time (theater_ratio rising, extractiveness rising) — suggests Snare or degraded Tangled Rope. If neutral: formula revisions are purely mechanical redistribution — suggests stable Rope or Tangled Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(formula_revision_ratchet_mechanism, empirical, 'Whether equalization formula revisions create upward ratchet in transfer amounts').

omega_variable(
    constitutional_amendment_counterfactual,
    'Would removal or substantial modification of equalization (Section 36 amendment) trigger federal dissolution crisis, or would competitive federalism with unequal service standards be politically sustainable?',
    'Discourse analysis of separatism correlation with equalization grievances (Quebec/Alberta threats; timing of separatist movements relative to equalization formula changes); counterfactual modeling of interprovincial cooperation without equalization; analysis of whether federal unity depends on equalization or whether equalization is merely an implementation detail of federal fiscal federalism',
    'If dissolution-critical: equalization is a structural necessity (mountain perspective confirmed, but false summit if beneficiaries identify); suppression is high because exit is catastrophic. If sustainable without equalization: constraint is more contingent; false summit is confirmed; extraction mechanism becomes salient.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(constitutional_amendment_counterfactual, conceptual, 'Whether equalization is constitutive of federal union or contingent fiscal mechanism').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(canadian_equalization_regime, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ceq_tr_t0, canadian_equalization_regime, theater_ratio, 0, 0.42).
narrative_ontology:measurement(ceq_tr_t10, canadian_equalization_regime, theater_ratio, 10, 0.52).
narrative_ontology:measurement(ceq_tr_t20, canadian_equalization_regime, theater_ratio, 20, 0.58).

% Extraction over time
narrative_ontology:measurement(ceq_be_t0, canadian_equalization_regime, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(ceq_be_t10, canadian_equalization_regime, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(ceq_be_t20, canadian_equalization_regime, base_extractiveness, 20, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(ceq_su_t0, canadian_equalization_regime, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(ceq_su_t10, canadian_equalization_regime, suppression_requirement, 10, 0.42).
narrative_ontology:measurement(ceq_su_t20, canadian_equalization_regime, suppression_requirement, 20, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(canadian_equalization_regime, resource_allocation).
narrative_ontology:affects_constraint(canadian_equalization_regime, provincial_fiscal_capacity_divergence).
narrative_ontology:affects_constraint(canadian_equalization_regime, federal_provincial_authority_boundary).

% DUAL FORMULATION NOTE:
% The equalization regime as a unified constraint can be decomposed into distinct mechanisms with different epsilon values: (1) the equalization transfer flow as a pure redistribution mechanism (higher extractiveness, ~0.55-0.65); (2) the formula apparatus as a technical governance system (higher theater_ratio, ~0.65-0.75, Piton classification); (3) the constitutional commitment to equalization as a legitimacy anchor (lower extractiveness, ~0.20-0.30, closer to Rope). This story models the integrated constraint; separate stories would track the formula apparatus specifically and the constitutional principle specifically. The network relationship indicates that equalization's viability depends on maintaining the boundary between federal and provincial authority—if that boundary erodes, equalization's extraction mechanism becomes more visible.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(canadian_equalization_regime, powerful, 0.52).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
