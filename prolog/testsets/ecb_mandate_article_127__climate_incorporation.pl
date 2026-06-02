% ============================================================================
% CONSTRAINT STORY: ecb_mandate_article_127__climate_incorporation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ecb_mandate_article_127__climate_incorporation, []).

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
 *   constraint_id: ecb_mandate_article_127__climate_incorporation
 *   human_readable: ECB Climate Risk Integration via Article 127/11 TFEU Mandate Reinterpretation
 *   domain: monetary_policy/constitutional_law/eu_governance
 *
 * SUMMARY:
 *   The European Central Bank's reinterpretation of its mandate under Article
 *   127 TFEU to incorporate climate risk integration into asset purchases and
 *   collateral frameworks represents ONE READING of a contested
 *   constitutional kernel. Article 127 establishes price stability as the
 *   ECB's primary objective and identifies secondary objectives including
 *   environmental protection (via Article 11 TFEU integration clause). The
 *   climate incorporation reading interprets Article 11's environmental
 *   integration requirement as mandating ECB climate alignment in core
 *   monetary operations — asset purchases, collateral haircuts, and portfolio
 *   tilting toward green bonds. This reading is contested by the orthodox
 *   price-stability-only reading (which treats Article 11 as aspirational but
 *   not operationally binding on collateral decisions) and the expansive
 *   secondary-objectives reading (which would give climate objectives equal
 *   weight to price stability, rather than subordinate). This story
 *   instantiates ONLY the climate incorporation reading: climate transition
 *   is a mandatory feature of monetary policy, justified as necessary for
 *   price stability (long-term price stability requires climate-resilient
 *   economy), not as a co-equal objective. The extractiveness signature
 *   (0.52) reflects the mixed coordination-extraction dynamic: ECB climate
 *   rules coordinate private capital toward green transition (rope function)
 *   while simultaneously extracting from fossil-fuel sectors via collateral
 *   restrictions and portfolio tilting (snare function). The theater ratio
 *   (0.65) captures that climate incorporation is partly performative: ECB
 *   presents climate integration as technical price-stability
 *   operationalization, when it is structurally a contested constitutional
 *   reinterpretation with distributional consequences deliberately layered
 *   beneath financial stability rhetoric.
 *
 * KEY AGENTS:
 *   - Green Energy Transition Sectors / Climate-Aligned Investors (Institutional/Arbitrage): Primary beneficiaries. ECB asset purchases reduce green bond borrowing costs; collateral framework favors low-carbon assets. Benefit from portfolio reallocation subsidies.
 *   - Fossil Fuel-Dependent Regions (Powerless/Trapped): Primary victims. Regional governments and coal-mining communities face rising borrowing costs as ECB restricts coal-related collateral. Trapped within regional economies dependent on carbon extraction. No exit options within biographical horizon.
 *   - Incumbent Energy Corporations (Powerful/Constrained): Secondary victims. High-powered actors facing stranded asset recognition and portfolio rebalancing. Constrained by collateral rules but have resources to adapt. Generational-scale transition required.
 *   - Member State Climate Leaders (Organized/Constrained): Mixed position. Benefit from lower green financing costs; constrained by fiscal rules and ECB independence doctrine. Organized coalition (NL, DE, FR, SE, AT) with negotiating power.
 *   - ECB Governing Council (Institutional/Identity-Locked): The constraint's primary enforcer and beneficiary. Identity-locked into climate mandate reinterpretation once adopted. Cannot retreat without institutional legitimacy crisis.
 *   - Labor Unions and Regional Development Actors (Organized/Mobile): Scaffold perspective. Pushing for just transition mechanisms to mitigate extraction. Mobile exit (retraining, diversification) available if transition support adequate.
 *   - European Court of Justice (Analytical/Analytical): Potential arbiter of Article 127/11 constitutionality. Observes full structure; could reclassify constraint via preliminary ruling.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ecb_mandate_article_127__climate_incorporation, 0.52).
domain_priors:suppression_score(ecb_mandate_article_127__climate_incorporation, 0.58).
domain_priors:theater_ratio(ecb_mandate_article_127__climate_incorporation, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ecb_mandate_article_127__climate_incorporation, extractiveness, 0.52).
narrative_ontology:constraint_metric(ecb_mandate_article_127__climate_incorporation, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(ecb_mandate_article_127__climate_incorporation, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ecb_mandate_article_127__climate_incorporation, tangled_rope).
narrative_ontology:human_readable(ecb_mandate_article_127__climate_incorporation, "ECB Climate Risk Integration via Article 127/11 TFEU Mandate Reinterpretation").
narrative_ontology:topic_domain(ecb_mandate_article_127__climate_incorporation, "monetary_policy/constitutional_law/eu_governance").

domain_priors:requires_active_enforcement(ecb_mandate_article_127__climate_incorporation).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ecb_mandate_article_127__climate_incorporation, '114c3d55-a170-4101-857e-9064ba821908').
narrative_ontology:cs_kernel_codification('114c3d55-a170-4101-857e-9064ba821908', formalized).
narrative_ontology:cs_authority_grounding('114c3d55-a170-4101-857e-9064ba821908', extraction).
narrative_ontology:cs_interpretation_layer_present('114c3d55-a170-4101-857e-9064ba821908').
narrative_ontology:cs_reading_relation('114c3d55-a170-4101-857e-9064ba821908', ecb_mandate_article_127__orthodox_price_stability, coexists_with).
narrative_ontology:cs_reading_relation('114c3d55-a170-4101-857e-9064ba821908', ecb_mandate_article_127__expansive_secondary_objectives, influences).
narrative_ontology:cs_axiom('114c3d55-a170-4101-857e-9064ba821908', foundational, climate_risk_integral_to_price_stability).
narrative_ontology:cs_axiom_status(climate_risk_integral_to_price_stability, holdable).
narrative_ontology:cs_axiom_grounding('114c3d55-a170-4101-857e-9064ba821908', climate_risk_integral_to_price_stability, empirically_contingent).
narrative_ontology:cs_axiom('114c3d55-a170-4101-857e-9064ba821908', foundational, article_11_environmental_integration_operationally_binding).
narrative_ontology:cs_axiom_status(article_11_environmental_integration_operationally_binding, holdable).
narrative_ontology:cs_axiom_grounding('114c3d55-a170-4101-857e-9064ba821908', article_11_environmental_integration_operationally_binding, conventional).
narrative_ontology:cs_reference_frame('114c3d55-a170-4101-857e-9064ba821908', article_127_price_stability_exclusive).
narrative_ontology:cs_drift_state('114c3d55-a170-4101-857e-9064ba821908', climate_incorporation_ecj_jurisprudence_phase, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('114c3d55-a170-4101-857e-9064ba821908', '2026-02-26T14:32:18Z').
narrative_ontology:cs_kernel_id(ecb_mandate_article_127__climate_incorporation, ecb_mandate_article_127).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ecb_mandate_article_127__climate_incorporation, green_energy_transition_sectors).
narrative_ontology:constraint_beneficiary(ecb_mandate_article_127__climate_incorporation, climate_vulnerable_regions).
narrative_ontology:constraint_beneficiary(ecb_mandate_article_127__climate_incorporation, future_generations_climate_stabilization).
narrative_ontology:constraint_victim(ecb_mandate_article_127__climate_incorporation, fossil_fuel_dependent_economies).
narrative_ontology:constraint_victim(ecb_mandate_article_127__climate_incorporation, carbon_intensive_corporations).
narrative_ontology:constraint_victim(ecb_mandate_article_127__climate_incorporation, traditional_energy_sector_employment).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: COAL-DEPENDENT REGIONAL ECONOMIES (SNARE) — Trapped by ECB collateral haircuts on carbon-intensive assets; cannot exit fossil fuel economy within biographical horizon. Regional governments face rising borrowing costs as ECB tightens collateral eligibility. No alternative revenue base. Maximum experienced extraction with no exit option.
constraint_indexing:constraint_classification(ecb_mandate_article_127__climate_incorporation, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: INCUMBENT ENERGY CORPORATIONS (TANGLED ROPE) — Constrained by ECB asset purchase restrictions on fossil fuel collateral and portfolio tilting toward green bonds. High-powered actors with resources to adapt, but facing stranded asset recognition and portfolio rebalancing costs. Mixed benefit from transition financing mechanisms (green bonds) and extraction via haircuts on traditional assets. Generational horizon: energy transition requires 20-30 year restructuring.
constraint_indexing:constraint_classification(ecb_mandate_article_127__climate_incorporation, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 3: GREEN BOND MARKET / TRANSITION FINANCIERS (ROPE) — Direct beneficiaries of ECB mandate interpretation. ECB purchases of green bonds subsidize green debt markets; transition financing becomes favorable relative to fossil fuel financing. High institutional power with arbitrage exit (can shift portfolios between asset classes). Experience the constraint as coordination: ECB action aligns private capital toward climate-aligned investments. Net beneficiary.
constraint_indexing:constraint_classification(ecb_mandate_article_127__climate_incorporation, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(continental))).

% PERSPECTIVE 4: MEMBER STATE GOVERNMENTS — CLIMATE LEADER COALITION (TANGLED ROPE) — Organized actors (EU climate leadership coalition: NL, DE, FR, SE, AT) benefit from ECB climate mandate interpretation via lower green financing costs and collateral support for climate policy. Constrained by fiscal rules and ECB independence treaties. Generational time horizon reflects infrastructure deployment needs. Mixed benefit: coordination of private capital toward national climate targets; extraction in terms of constrained fiscal autonomy and treaty interpretation precedent.
constraint_indexing:constraint_classification(ecb_mandate_article_127__climate_incorporation, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: ECB OPERATIONAL ORTHODOXY (PITON) — The traditional interpretation of Article 127 TFEU (price stability as primary mandate, environmental objectives as secondary/excluded from core operations) persists as institutional inertia despite being structurally degraded. The climate incorporation reading performs the same function (inflation control via asset purchases) while routing additional legitimacy through environmental integration. Operational theater: the collateral framework now performs dual function (price stability + climate alignment) presented as unified mandate, when they are structurally separate. Piton: the orthodox price-stability-only interpretation is maintained in formal doctrine while climate function is layered beneath.
constraint_indexing:constraint_classification(ecb_mandate_article_127__climate_incorporation, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: JUST TRANSITION SUPPORT ARCHITECTURE (SCAFFOLD) — Organized coalition of labor unions, regional development funds, and climate justice advocates viewing ECB mandate as temporary scaffold: climate collateral rules are designed with explicit sunset via transition support mechanisms, green skills funding, and worker retraining. Effective extraction is low because exit pathway (phase-out of carbon-intensive collateral paired with transition financing) is specified. Mobile exit options (workers can retrain; regions can diversify) distinguish this from snare. Theater is low: transition mechanisms directly address extraction risk rather than relying on performative mitigation.
constraint_indexing:constraint_classification(ecb_mandate_article_127__climate_incorporation, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(continental))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER — PHYSICAL NECESSITY VIEW (MOUNTAIN) — From civilizational/universal perspective, carbon transition is physically inevitable: physics of climate forcing + carbon budget constraints mean fossil fuel phase-out is not a policy choice but a material requirement. ECB mandate is reinterpreting the constraint to align with this inevitability. This perspective treats the transition as unchangeable natural constraint on which monetary policy must operate. However, the structural data reveals this as false summit: the specific timing, distributional consequences, and collateral mechanisms are contingent institutional choices, not laws of physics.
constraint_indexing:constraint_classification(ecb_mandate_article_127__climate_incorporation, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 8: ECB INSTITUTIONAL IDENTITY — MANDATE INTERPRETATION (IDENTITY_LOCKED TANGLED_ROPE) — The ECB's identity as a European institution has fused with the climate transition objective through the Article 11/127 reading. The institution cannot exit the climate mandate without dissolving its own legitimacy claim as a modern EU actor committed to Article 11 environmental integration. Structurally mobile (ECB could choose different collateral rules) but identity-locked: the institution has become the climate incorporation mechanism itself. Mixed experience: benefits from enhanced institutional legitimacy; extraction of institutional autonomy via expanded mandate scope. Identity lock is cognitive rather than legal — the ECB could theoretically retreat, but doing so would contradict its self-conception as responsive to EU constitutional values.
constraint_indexing:constraint_classification(ecb_mandate_article_127__climate_incorporation, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(continental))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ecb_mandate_article_127__climate_incorporation_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(ecb_mandate_article_127__climate_incorporation, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(ecb_mandate_article_127__climate_incorporation, TypeOther, context(agent_power(powerful), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(ecb_mandate_article_127__climate_incorporation, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(ecb_mandate_article_127__climate_incorporation, TR),
    TR >= 0.70.

:- end_tests(ecb_mandate_article_127__climate_incorporation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The constraint extracts from fossil-fuel-dependent actors (collateral haircuts, asset purchase restrictions) while providing subsidized access to green financing for transition sectors. The extraction is structurally asymmetric but partially justified within the constraint's own logic (climate-resilient economy necessary for long-term price stability). The measurement trajectory shows rising extractiveness over the 10-year interval (0.28 → 0.52), reflecting that as collateral rules tighten and as stranded asset costs accumulate, the extraction mechanism becomes more pronounced. Suppression (0.58): Moderate-high. Fossil-fuel-dependent regions face rising barriers to capital access, but suppression is not total — some collateral assets remain available, transition financing is offered, and member states retain fiscal policy tools. The measurement shows rising suppression over time (0.40 → 0.58) as ECB rules harden and as incumbent actors exhaust adaptive options. Theater ratio (0.65): Moderate-high. The ECB presents climate integration as a technical operationalization of price stability mandate (financial stability language), but the constraint is structurally a contested constitutional reinterpretation with major distributional consequences. The theater has increased over time (0.48 → 0.65) as the ECB has developed more elaborate justifications for climate-aligned collateral decisions and as the performative institutional architecture has expanded.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits DR's full indexical spectrum. Fossil-fuel regions see a snare (trapped extraction with no exit). Incumbent energy corporations see tangled rope (mixed coordination of transition capital with extraction via asset haircuts). Green financiers see rope (coordination mechanism subsidizing their position). Climate-leader governments see tangled rope (mixed benefit from lower green financing + constraint from ECB independence). ECB orthodoxy sees a piton (operational inertia maintained despite structural degradation). Just transition advocates see a scaffold (temporary mechanism with sunset logic). The analytical observer from a universal perspective risks seeing a mountain (climate transition as physical inevitability, ECB mandate as natural law). The ECB itself experiences identity-locked tangled rope — structurally mobile (could choose different collateral rules) but cognitively bound to its self-conception as a climate-committed institution. This perspectival range reveals the constraint's core structure: it is not 'truly' any single type, but rather a presheaf of legitimate readings across different indexical positions.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality derivation operates differently across perspectives based on beneficiary/victim declarations and exit options. Fossil-fuel regions: victim status + trapped exit → d ≈ 0.95 → f(d) ≈ 1.42 → high experienced extractiveness (snare). Green investors: beneficiary status + arbitrage exit → d ≈ 0.05 → f(d) ≈ -0.12 → negative effective extraction (rope). Incumbent energy corporations: victim status + constrained exit → d ≈ 0.75 → f(d) ≈ 1.05 → high experienced extractiveness (tangled rope, qualified by coordination benefits). Climate-leader governments: mixed (beneficiary of green financing, victim of fiscal constraint) + constrained exit → d ≈ 0.50 → f(d) ≈ 0.65 → moderate experienced extractiveness. ECB institutional identity-lock: beneficiary status (expanded mandate legitimacy) + identity-locked exit → d ≈ 0.25 → f(d) ≈ 0.02 → low experienced extractiveness (rope) BUT cognitive entrapment increases perceived extraction risk from future mandate creep. The identity-locked perspective adds a dimension: the extracted resource is not wealth but institutional autonomy/constrainedness. Analytical observer: observer status + analytical exit → d ≈ 0.72 → canonical fallback (mountain at civilizational scope risks naturalizing contingent rules).
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint resolves mandatrophy by distinguishing the physical necessity (climate transition is inevitable) from the institutional mechanism (ECB collateral rules are contingent). The climate incorporation reading is NOT saying 'climate objectives are as important as price stability' (which would create an unresolved mandatrophy of conflicting institutional mandates). Rather, it is saying 'climate risk integration is operationally necessary for long-term price stability' — bringing climate under the price-stability umbrella, not elevating climate to co-equal status. This resolves the apparent conflict between Article 127 (price stability primary) and Article 11 (environmental integration). The measurement trajectory shows rising extractiveness, indicating that as the constraint tightens over time, the justificatory sleight-of-hand ('climate is just price stability risk management') becomes more transparent. If the constraint continues to tighten without producing demonstrable economic adjustment, the mandatrophy will re-emerge: ECB will face irresolvable pressure between price stability (which may require looser collateral rules to support credit availability) and climate risk mitigation (which requires tighter rules). The analysis suggests this is a latent contradiction in the constraint design rather than a resolved tension.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    article_127_scope_ambiguity,
    'Does Article 127 TFEU grant ECB authority to integrate climate risk into asset purchases and collateral frameworks, or does climate integration violate price-stability primacy by politicizing monetary operations?',
    'European Court of Justice preliminary ruling (Article 267 TFEU reference procedure). Legal test: whether climate risk integration is necessary for price stability mandate or constitutes unauthorized expansion into secondary objectives.',
    'ECJ ruling toward climate integration: constraint shifts toward Tangled Rope (hybrid coordination-extraction). Ruling against climate integration: constraint reclassifies as Snare (pure extraction of climate objectives from monetary policy). Constraint identity itself is contested.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(article_127_scope_ambiguity, conceptual, 'Whether Article 127 grants ECB climate integration authority or violates price-stability primacy').

omega_variable(
    transition_timeline_feasibility,
    'Can fossil-fuel-dependent regions transition to green economy within the 10-15 year timescale implied by ECB collateral haircuts?',
    'Longitudinal economic impact assessment in coal-dependent regions (Ruhr Valley, Polish coal belt, Greek lignite regions). Measurement: employment transition rates, wage replacement adequacy, regional GDP recovery post-haircut.',
    'If feasible (>60% smooth transition): scaffold and tangled_rope perspectives confirmed. If infeasible (<40%): perspectives shift toward higher extraction/snare for powerless agents. Determines whether suppression is temporary (scaffold) or structural (snare).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(transition_timeline_feasibility, empirical, 'Feasibility of fossil-fuel-region transition within collateral haircut timescale').

omega_variable(
    price_stability_vs_climate_conflict,
    'When ECB climate integration reduces asset availability for traditional collateral (e.g., fossil fuel bonds) does this constrain money supply and risk tight monetary conditions that harm price stability mandate?',
    'Monetary transmission mechanism analysis: correlation between green collateral adoption and money supply M1/M2 growth rates; inflation modeling conditional on collateral tightening.',
    'If conflicts found: creates intra-institutional mandate tension (Article 127 primary vs Article 11 secondary). ECB forced to choose between mandates. If no conflict: climate integration and price stability are structurally compatible. Changes character of extraction (from conflict between agents to asymmetric distribution of costs).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(price_stability_vs_climate_conflict, empirical, 'Whether climate-driven collateral restriction conflicts with price stability mandate').

omega_variable(
    mandate_reading_contestability,
    'Is the climate incorporation reading of Article 127/11 a defensible constitutional interpretation, or does it represent judicial overreach into statutory text?',
    'Scholarly consensus among EU constitutional lawyers; ECB Governing Council institutional position; potential ECJ reference procedure; member state legal challenges.',
    'If defensible: constraint persists as legitimate Tangled Rope. If indefensible: constraint becomes Snare (politicized extraction via overreach). Shapes authority legitimacy and long-term institutional stability.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(mandate_reading_contestability, conceptual, 'Constitutional defensibility of climate incorporation reading of Article 127/11 TFEU').

omega_variable(
    distributional_justice_sufficiency,
    'Do ECB-financed just transition programs (green skills, regional development, worker retraining) adequately compensate powerless actors for stranded-asset losses and employment disruption?',
    'Comparative analysis: wage replacement rates, pension bridge programs, regional GDP recovery vs. pre-haircut baseline. Sufficiency test: whether compensation restores equivalent opportunity set for affected workers and regions.',
    'If sufficient (>85% compensation): snare perspective weakens; scaffold perspective confirmed. If insufficient (<50%): snare dominates; constraint becomes pure extraction for powerless agents. Determines whether suppression is mitigated by transition architecture.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(distributional_justice_sufficiency, empirical, 'Adequacy of just transition compensation for stranded-asset losses').

omega_variable(
    identity_lock_escape_mechanism,
    'Can the ECB''s institutional identity disentangle from climate integration once the mandate reinterpretation is accepted, or does institutional path-dependence lock the ECB into expanded climate objectives?',
    'Historical institutional analysis: precedent for ECB mandate narrowing or reversion; political economy of institutional identity shifts in the EU; member state capacity to constrain ECB via treaty amendment.',
    'If escape is possible: ECB''s identity lock is contingent and reversible (reduces long-term institutional risk). If escape is impossible: ECB becomes permanently expanded institutional actor (increases risk of future mandate drift).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_lock_escape_mechanism, conceptual, 'Reversibility of ECB institutional identity fusion with climate mandate').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ecb_mandate_article_127__climate_incorporation, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ecb_climate_tr_t0, ecb_mandate_article_127__climate_incorporation, theater_ratio, 0, 0.48).
narrative_ontology:measurement(ecb_climate_tr_t3, ecb_mandate_article_127__climate_incorporation, theater_ratio, 3, 0.55).
narrative_ontology:measurement(ecb_climate_tr_t6, ecb_mandate_article_127__climate_incorporation, theater_ratio, 6, 0.62).
narrative_ontology:measurement(ecb_climate_tr_t10, ecb_mandate_article_127__climate_incorporation, theater_ratio, 10, 0.65).

% Extraction over time
narrative_ontology:measurement(ecb_climate_be_t0, ecb_mandate_article_127__climate_incorporation, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(ecb_climate_be_t3, ecb_mandate_article_127__climate_incorporation, base_extractiveness, 3, 0.38).
narrative_ontology:measurement(ecb_climate_be_t6, ecb_mandate_article_127__climate_incorporation, base_extractiveness, 6, 0.48).
narrative_ontology:measurement(ecb_climate_be_t10, ecb_mandate_article_127__climate_incorporation, base_extractiveness, 10, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(ecb_climate_su_t0, ecb_mandate_article_127__climate_incorporation, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(ecb_climate_su_t3, ecb_mandate_article_127__climate_incorporation, suppression_requirement, 3, 0.48).
narrative_ontology:measurement(ecb_climate_su_t6, ecb_mandate_article_127__climate_incorporation, suppression_requirement, 6, 0.54).
narrative_ontology:measurement(ecb_climate_su_t10, ecb_mandate_article_127__climate_incorporation, suppression_requirement, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ecb_mandate_article_127__climate_incorporation, enforcement_mechanism).
narrative_ontology:affects_constraint(ecb_mandate_article_127__climate_incorporation, ecb_mandate_article_127__orthodox_price_stability).
narrative_ontology:affects_constraint(ecb_mandate_article_127__climate_incorporation, ecb_mandate_article_127__expansive_secondary_objectives).
narrative_ontology:affects_constraint(ecb_mandate_article_127__climate_incorporation, eu_fiscal_rule_sovereign_debt).
narrative_ontology:affects_constraint(ecb_mandate_article_127__climate_incorporation, carbon_border_adjustment_mechanism).

% DUAL FORMULATION NOTE:
% ECB_MANDATE_ARTICLE_127 decomposes into three constraint stories corresponding to three readings of the same kernel (Article 127 TFEU authority). Climate_incorporation is the constraint generated here. Orthodox_price_stability and expansive_secondary_objectives are sibling readings with different ε values, beneficiary/victim structures, and classification types. All three are linked via network.affects_constraints to indicate they are readings of the same contested kernel, not independent constraints. The family also affects downstream constraints (CBAM, fiscal rules) because the ECB's climate mandate interpretation influences their implementation.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(ecb_mandate_article_127__climate_incorporation, institutional, 0.35).
constraint_indexing:directionality_override(ecb_mandate_article_127__climate_incorporation, organized, 0.48).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
