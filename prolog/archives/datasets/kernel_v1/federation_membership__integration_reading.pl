% ============================================================================
% CONSTRAINT STORY: federation_membership__integration_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_federation_membership__integration_reading, []).

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
 *   constraint_id: federation_membership__integration_reading
 *   human_readable: Federation Membership as Irreversible Integration (Beneficiary Reading)
 *   domain: political_economy/federalism/migration_policy
 *
 * SUMMARY:
 *   Federation membership, represented as irreversible integration with
 *   supranational authority and constitutional free movement, is a contested
 *   kernel that different political communities read differently. This story
 *   instantiates the INTEGRATION READING: the reading that takes federation
 *   membership to be a one-way commitment to economic and labor-market
 *   integration, with the irreversibility legitimated by supranational
 *   authority and the free-movement right constitutionalized as
 *   non-negotiable. Under this reading, member-states cannot unilaterally
 *   withdraw from the integration mechanism without collective authorization,
 *   and citizens possess a fundamental right to move freely across member
 *   territories. This reading emphasizes the coordination benefits of large
 *   integrated markets and the inevitability of economic convergence, while
 *   naturalizing the asymmetric impacts on geographically immobile labor in
 *   peripheral regions. The constraint exhibits characteristics of a Tangled
 *   Rope: genuine coordination benefits (large integrated labor markets,
 *   capital mobility enabling investment) coexist with asymmetric extraction
 *   (wage suppression in peripheral regions, labor displacement, policy
 *   constraints on peripheral governments). The theater ratio has declined
 *   over the interval (0.62→0.48) as the integration mechanism has shifted
 *   from explicitly negotiated policy coordination toward ostensibly
 *   automatic market outcomes, reducing the performative content and
 *   increasing the appearance of inevitability. Extractiveness has risen
 *   (0.28→0.52) as the distributional impacts have accumulated and the
 *   constraint's enforcement capacity has solidified. Suppression has risen
 *   (0.35→0.58) as the legal and economic barriers to exit have grown and
 *   alternative institutional arrangements have become harder to imagine.
 *
 * KEY AGENTS:
 *   - Mobile Professional Class: Primary beneficiary (organized/mobile) — unrestricted movement enables career optimization and wage arbitrage; experiences integration as pure coordination
 *   - Multinational Enterprises: Primary beneficiary (institutional/arbitrage) — benefits from capital mobility, labor arbitrage, supply-chain integration; experiences integration as coordination mechanism
 *   - Peripheral Labor Markets: Primary victim (powerless/trapped) — geographically rooted workers face wage suppression, job displacement, immobility; maximum experienced extraction
 *   - Non-Mobile Residents: Secondary victim (moderate/constrained) — regional workers experience mixed benefits (larger labor pool) and costs (displacement, wage pressure); constrained but not trapped
 *   - Peripheral Member-States: Secondary victim (institutional/constrained) — economically weaker members experience coordination benefits (capital access) alongside extraction (brain drain, capital concentration, policy constraints); formal exit possible but catastrophically costly
 *   - Supranational Authority: Institutional architect (institutional/mobile) — maintains the integration mechanism as a presumed temporary scaffold toward either full political union or permanent institutional hierarchy; mobile capacity to reform
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing the integration mechanism as inevitable economic law rather than constructed political choice grounded in supranational authority
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(federation_membership__integration_reading, 0.52).
domain_priors:suppression_score(federation_membership__integration_reading, 0.58).
domain_priors:theater_ratio(federation_membership__integration_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(federation_membership__integration_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(federation_membership__integration_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(federation_membership__integration_reading, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(federation_membership__integration_reading, tangled_rope).
narrative_ontology:human_readable(federation_membership__integration_reading, "Federation Membership as Irreversible Integration (Beneficiary Reading)").
narrative_ontology:topic_domain(federation_membership__integration_reading, "political_economy/federalism/migration_policy").

domain_priors:requires_active_enforcement(federation_membership__integration_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(federation_membership__integration_reading, '99d3de76-ba05-4d9a-a6f9-d3b771cf2f96').
narrative_ontology:cs_kernel_codification('99d3de76-ba05-4d9a-a6f9-d3b771cf2f96', formalized).
narrative_ontology:cs_authority_grounding('99d3de76-ba05-4d9a-a6f9-d3b771cf2f96', extraction).
narrative_ontology:cs_interpretation_layer_present('99d3de76-ba05-4d9a-a6f9-d3b771cf2f96').
narrative_ontology:cs_reading_relation('99d3de76-ba05-4d9a-a6f9-d3b771cf2f96', federation_membership__sovereignty_reading, coexists_with).
narrative_ontology:cs_axiom('99d3de76-ba05-4d9a-a6f9-d3b771cf2f96', foundational, integration_irreversibility_legitimate).
narrative_ontology:cs_axiom_status(integration_irreversibility_legitimate, holdable).
narrative_ontology:cs_axiom_grounding('99d3de76-ba05-4d9a-a6f9-d3b771cf2f96', integration_irreversibility_legitimate, deontological).
narrative_ontology:cs_axiom('99d3de76-ba05-4d9a-a6f9-d3b771cf2f96', foundational, free_movement_constitutional_right).
narrative_ontology:cs_axiom_status(free_movement_constitutional_right, holdable).
narrative_ontology:cs_axiom_grounding('99d3de76-ba05-4d9a-a6f9-d3b771cf2f96', free_movement_constitutional_right, deontological).
narrative_ontology:cs_reference_frame('99d3de76-ba05-4d9a-a6f9-d3b771cf2f96', supranational_institutional_permanence).
narrative_ontology:cs_drift_state('99d3de76-ba05-4d9a-a6f9-d3b771cf2f96', contemporary_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('99d3de76-ba05-4d9a-a6f9-d3b771cf2f96', '').
narrative_ontology:cs_kernel_id(federation_membership__integration_reading, federation_membership).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(federation_membership__integration_reading, mobile_citizens).
narrative_ontology:constraint_beneficiary(federation_membership__integration_reading, capital_flows).
narrative_ontology:constraint_beneficiary(federation_membership__integration_reading, multinational_enterprises).
narrative_ontology:constraint_victim(federation_membership__integration_reading, local_labor_markets).
narrative_ontology:constraint_victim(federation_membership__integration_reading, non_mobile_residents).
narrative_ontology:constraint_victim(federation_membership__integration_reading, peripheral_regions).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PERIPHERAL LABOR MARKET (SNARE) — Geographically immobile workers in peripheral regions face wage suppression and job displacement as mobile capital and high-skill workers exit. The integration mechanism locks them into labor-market competition with the entire federation; exit is impossible (geographical rootedness, family ties, language barriers). Maximum experienced extraction — no alternative coordination benefit perceived.
constraint_indexing:constraint_classification(federation_membership__integration_reading, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: NON-MOBILE RESIDENT (TANGLED ROPE) — Workers with regional ties face genuine coordination benefits (access to larger labor pool, capital mobility enabling local investment) alongside asymmetric extraction (wage suppression, job insecurity from footloose capital). Constrained exit — could relocate at significant cost (family separation, cultural displacement, housing barriers). Mixed experience: some gains from integrated market, substantial costs from labor displacement.
constraint_indexing:constraint_classification(federation_membership__integration_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: MOBILE PROFESSIONAL CLASS (ROPE) — High-skill workers with transferable credentials experience federation membership as pure coordination: unrestricted movement enables career optimization, wage arbitrage, and portfolio diversification. Mobile exit (can relocate easily with low cost). Benefits flow directly — integration is perceived as solving the coordination problem of fragmented labor markets.
constraint_indexing:constraint_classification(federation_membership__integration_reading, rope,
    context(agent_power(organized),
            time_horizon(immediate),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 4: MULTINATIONAL ENTERPRISE (ROPE) — Firms experience federation membership as a coordination solution to capital mobility problems: unrestricted investment, supply-chain optimization across borders, labor arbitrage across wage differentials. Arbitrage exit (can reallocate capital/operations to alternate jurisdictions or federated members). Pure beneficiary — extraction runs toward the firm through wage suppression in peripheral regions and reduced regulatory fragmentation.
constraint_indexing:constraint_classification(federation_membership__integration_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: PERIPHERAL MEMBER-STATE (TANGLED ROPE) — Economically weaker members experience integration as both coordination (access to capital, integrated markets enable development) and extraction (labor drain via emigration, capital concentration in core regions, forced convergence criteria limiting policy autonomy). Constrained exit — formal exit mechanisms exist (Article 50 logic) but carry catastrophic economic costs; exit is structural not immediate. Genuine coordination benefits coexist with asymmetric extraction.
constraint_indexing:constraint_classification(federation_membership__integration_reading, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 6: SUPRANATIONAL AUTHORITY (SCAFFOLD) — The federal/supranational governance structure sees integration as a coordinated temporary solution to historical fragmentation. Mobile exit capacity (can reform institutions, reallocate competencies) and designed sunset logic: full political union would dissolve the temporary ambiguity, making the constraint either permanent institutional hierarchy (institutional power locks in) or permanent coordination (full democratic legitimacy emerges). Current scaffold state: economic integration without political union, with presumed evolution toward one or the other.
constraint_indexing:constraint_classification(federation_membership__integration_reading, scaffold,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(continental))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURALIZED VIEW (MOUNTAIN) — From a civilizational/universal perspective, this reading naturalizes economic integration as inevitable and irreversible: capital mobility and labor-market integration follow from the 'laws' of modern economy; the irreversibility is framed as a natural consequence of economic interdependence rather than a political choice. However, this is a FALSE SUMMIT — the structural data reveals the irreversibility is constructed through supranational legal authority and enforcement, not emergent from economic forces alone. The mountain classification serves as a diagnostic signal of the naturalizing function.
constraint_indexing:constraint_classification(federation_membership__integration_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(federation_membership__integration_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(federation_membership__integration_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(federation_membership__integration_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(federation_membership__integration_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(federation_membership__integration_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high, reflecting genuine coordination benefits alongside substantial distributional asymmetry. The integration reading emphasizes the coordination logic (large markets, capital mobility), which would justify extractiveness in the 0.30-0.40 range if purely functional. However, the rising extraction trajectory (0.28→0.52) indicates that distributional impacts have accumulated beyond coordination costs. The measurement captures the constraint's dynamic: initially justified as coordination, increasingly experienced as extraction by peripheral actors. Suppression (0.58): Moderate-high. Significant barriers to exit include legal prohibition (supranational authority enforces membership), economic interdependence (capital and supply chains integrated across borders), and political infeasibility (no collective mechanism for reversible membership). Barriers are not total — some actors escape through individual mobility, and member-states retain formal exit mechanisms (at prohibitive cost). Theater ratio (0.48): Below 0.5, indicating functional coordination mechanism rather than performative ritual. The declining trajectory reflects the shift from explicitly negotiated policy toward ostensibly automatic market outcomes. As the mechanism matures, the visible negotiation decreases, creating the appearance of naturalness. This low theater is diagnostically important: it prevents misclassification as Piton (which requires theater ≥0.70). The integration mechanism is not degraded; it is functioning — the extraction is not a side effect of ritual decay but of working coordination.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates substantial perspectival divergence from the same structural data. Mobile professionals see rope (pure coordination); multinational enterprises see rope (coordination + arbitrage benefit); peripheral workers see snare (pure extraction, no escape); non-mobile residents see tangled rope (mixed coordination and extraction); peripheral member-states see tangled rope (benefits + constraints); supranational authority sees scaffold (temporary coordination mechanism with presumed sunset into full union); analytical observer risks seeing mountain (natural inevitability of integration) but structural data indicates false summit. The perspectival gap is not merely observational disagreement but structural — different agents have genuinely different experiences of the same constraint because they occupy different positions in its benefit/cost flow. The integration reading emphasizes the beneficiary perspectives (mobile, capital) and the coordination logic; the sovereignty reading (not this story, but the sibling) would emphasize victim perspectives (immobile, peripheral) and the extraction logic. This perspectival gap is the space where the contest between readings occurs.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's experienced extractiveness (χ) is computed from base extractiveness (ε=0.52), the agent's directionality value (d), and the scope modifier. Mobile citizens and multinational enterprises have low d (high-benefit positions in the coordination mechanism) and experience negative or minimal χ (pure beneficiaries). Peripheral labor markets have high d (maximum cost-bearing position) and experience maximum χ (pure targets). Non-mobile residents and peripheral member-states have moderate d (mixed benefit-cost positions) and experience intermediate χ (tangled rope dynamics). The supranational authority has low d (benefits from maintaining the mechanism) and institutional power to enforce the structure. The analytical observer has an intermediate d reflecting institutional position but sees through the apparatus, enabling the false summit detection. The directionality computations follow from beneficiary/victim declarations and exit options: beneficiaries with arbitrage capacity → low d; victims with no exit → high d; mixed actors → intermediate d.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy (how to classify mixed coordination-extraction) by accepting that it is genuinely Tangled Rope — it has both a real coordination function (large integrated markets do enable more efficient capital allocation and labor matching) AND real asymmetric extraction (peripheral regions bear net costs despite aggregate benefits). The mandatrophy resolution requires acknowledging that the reading choice itself is part of the constraint's structure. The INTEGRATION READING chooses to emphasize coordination and naturalize irreversibility; this choice is not neutral but constitutive of the constraint's function. A different reading (the SOVEREIGNTY READING) would emphasize extraction and reversibility. The constraint's true classification depends on which reading is institutionally dominant — the integration reading dominates in the supranational authority and in mobile beneficiary constituencies, producing Tangled Rope + Mountain (false summit) from those positions. The sovereignty reading dominates in peripheral constituencies, producing Snare from those positions. The mandatrophy is resolved by recognizing that both readings are live, neither is empirically privileged, and the constraint's actual operation is the political competition between these readings.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    irreversibility_mechanism_source,
    'Is the irreversibility of federation membership grounded in economic interdependence that would persist regardless of political status, or in supranational legal enforcement that would collapse if the political commitment dissolved?',
    'Historical analysis of federation breakdowns (Yugoslavia, USSR, Czechoslovakia) and attempted exits (Brexit negotiation); comparison of actual costs borne by exiting members vs. theoretical economic dependency calculations',
    'If economic: irreversibility is structural/natural, mountain classification becomes appropriate, and exit barriers are immutable. If legal/political: irreversibility is constructed, snare classification is more accurate, and exit barriers are contingent on enforcement commitment.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(irreversibility_mechanism_source, empirical, 'Whether irreversibility derives from economic interdependence or political enforcement').

omega_variable(
    labor_displacement_attribution,
    'What proportion of labor-market disruption in peripheral regions is caused by federation mobility vs. other factors (technological change, deindustrialization, global supply-chain shifts)?',
    'Causal decomposition: labor displacement patterns in federated vs. non-federated labor markets controlling for development level, industrial composition, and exposure to trade; measurement of labor flow velocity before/after federation establishment',
    'If federation mobility is primary cause: extractiveness rises (0.52→0.65), peripheral regions experience snare (high extraction) rather than tangled rope. If secondary cause: extractiveness falls (0.52→0.35), classification shifts toward rope (coordination with distributional side effects). Attribution affects policy legitimacy frame: inevitable integration vs. policy choice with alternatives.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(labor_displacement_attribution, empirical, 'Causal attribution of labor displacement to federation mobility').

omega_variable(
    alternative_institutional_frames,
    'What would the constraint look like under the sovereign-state reading (federation membership as revocable coordination with default exit option)? Does the integration reading foreclose the sovereignty reading logically, or do they coexist as competing normative commitments?',
    'Comparative analysis of the two readings'' axioms: if axiom sets are mutually contradictory in principle, foreclosure is indicated; if axiom sets differ on which facts ground legitimacy but both are empirically defensible, coexistence is indicated',
    'If foreclosure: one reading must be abandoned; integration reading dominates through legal-institutional victory, and the constraint is essentially settled. If coexistence: both readings persist as live positions, and the supranational authority faces permanent legitimacy contestation. This omega resolves into the cs_structure.reading_relations classification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(alternative_institutional_frames, conceptual, 'Whether integration and sovereignty readings logically foreclose each other or coexist').

omega_variable(
    beneficiary_expansion_into_victim_groups,
    'Do peripheral-region workers who successfully navigate the integration mechanism (acquire federation-transferable credentials, escape peripheral labor markets) transition from victim to beneficiary status? What proportion accomplish this transition, and what barriers prevent it?',
    'Longitudinal demographic tracking of educational/professional mobility across federation members; comparison of intergenerational career outcomes for children of peripheral workers vs. their cohort origin group',
    'If transition is common: the victim/beneficiary distinction is dynamic, and the constraint is ''temporary'' for individuals (though structural for immobile cohorts). Extractiveness may be better understood as distributional inequality perpetuating mechanism (Piton) rather than direct extraction. If transition is rare: the victim/beneficiary distinction is structural, confirming snare classification for powerless agents.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_expansion_into_victim_groups, empirical, 'Whether workers can transition from victim to beneficiary status through education/mobility').

omega_variable(
    federation_authority_legitimacy_basis,
    'What legitimacy mechanism does the supranational authority claim for imposing irreversible integration: democratic aggregation across all members, expert technocratic design, or inherited historical authority from founding documents?',
    'Examination of founding documents, constitutional texts, court decisions (CJEU, ECJ equivalents); comparison of stated legitimacy claims across supranational institutions vs. observable decision-making procedures',
    'If democratic aggregation: legitimacy derives from consent (though constrained by majority rule), and victims could theoretically exit via democratic reversal if they gain sufficient support. If technocratic: legitimacy rests on expertise claims, and victim exit depends on falsifying expert consensus. If inherited: legitimacy rests on path-dependence, and exits are politically blocked by founding-document interpretations.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(federation_authority_legitimacy_basis, conceptual, 'The supranational authority''s claimed legitimacy basis for irreversible integration').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(federation_membership__integration_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fed_integ_tr_t0, federation_membership__integration_reading, theater_ratio, 0, 0.62).
narrative_ontology:measurement(fed_integ_tr_t10, federation_membership__integration_reading, theater_ratio, 10, 0.55).
narrative_ontology:measurement(fed_integ_tr_t20, federation_membership__integration_reading, theater_ratio, 20, 0.48).

% Extraction over time
narrative_ontology:measurement(fed_integ_be_t0, federation_membership__integration_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(fed_integ_be_t10, federation_membership__integration_reading, base_extractiveness, 10, 0.4).
narrative_ontology:measurement(fed_integ_be_t20, federation_membership__integration_reading, base_extractiveness, 20, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(fed_integ_su_t0, federation_membership__integration_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(fed_integ_su_t10, federation_membership__integration_reading, suppression_requirement, 10, 0.48).
narrative_ontology:measurement(fed_integ_su_t20, federation_membership__integration_reading, suppression_requirement, 20, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(federation_membership__integration_reading, resource_allocation).
narrative_ontology:affects_constraint(federation_membership__integration_reading, federation_membership__sovereignty_reading).
narrative_ontology:affects_constraint(federation_membership__integration_reading, labor_market_convergence).
narrative_ontology:affects_constraint(federation_membership__integration_reading, regional_inequality_persistence).

% DUAL FORMULATION NOTE:
% Federation membership decomposes into two structurally distinct constraints with different ε values and different beneficiary/victim structures, depending on which reading is institutionally operative. The INTEGRATION_READING (this story, ε=0.52) treats free movement and supranational authority as constitutive and irreversible; it naturally produces Tangled Rope and risks Mountain (false summit). The SOVEREIGNTY_READING (sibling story, ε varying by analysis) treats federation membership as revocable coordination with default exit option; it naturally produces different classification depending on how reversibility is modeled. Both stories are empirically real and currently compete in political practice. They affect downstream constraints (labor market convergence patterns, regional inequality dynamics) differently depending on which reading institutionally dominates in a given period or jurisdiction.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(federation_membership__integration_reading, institutional, 0.42).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
