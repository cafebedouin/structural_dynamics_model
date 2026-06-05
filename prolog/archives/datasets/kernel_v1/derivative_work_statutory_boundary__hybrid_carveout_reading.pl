% ============================================================================
% CONSTRAINT STORY: derivative_work_statutory_boundary__hybrid_carveout_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_derivative_work_statutory_boundary__hybrid_carveout_reading, []).

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
 *   constraint_id: derivative_work_statutory_boundary__hybrid_carveout_reading
 *   human_readable: Derivative Work Statutory Boundary (Hybrid Carveout Reading)
 *   domain: intellectual_property_law/copyright/information_economics
 *
 * SUMMARY:
 *   The statutory boundary between derivative and original work under
 *   copyright law creates a tension between protecting original creators'
 *   economic rights and enabling downstream creative innovation. The hybrid
 *   carveout reading instantiates a specific approach to this tension:
 *   non-commercial transformative uses are permitted without authorization;
 *   commercial derivatives require licensing. This reading reflects a policy
 *   compromise: it exempts subcultural creators and educational reuse from
 *   licensing costs while preserving a licensing market for commercial
 *   exploitation. The constraint exhibits Tangled Rope structure: genuine
 *   coordination benefit (the bright-line rule reduces transaction costs and
 *   clarifies when licensing applies) coupled with asymmetric extraction
 *   (licensing fees capture surplus from commercial derivatives, particularly
 *   in highly-restricted repertoires like music and film). The extractiveness
 *   has risen over the interval (0.35 → 0.48) as enforcement mechanisms have
 *   matured and licensing markets have concentrated. Theater ratio has risen
 *   (0.48 → 0.58) because much licensing compliance is now automated and
 *   templatic—licensees encounter standardized terms that reproduce statutory
 *   elements without genuine negotiation. This reading is one interpretation
 *   of the contested kernel 'derivative work' — it coexists with the
 *   enclosure reading (no non-commercial carveout; all commercial use
 *   requires license) and the coordination reading (broad fair-use exemption
 *   for transformative use regardless of commerciality). The hybrid reading's
 *   axioms are: (1) non-commercial innovation is categorically distinct from
 *   commercial exploitation and merits different legal treatment, and (2)
 *   licensing markets for commercial derivatives are both effective
 *   coordination mechanisms and necessary revenue sources for original
 *   creators.
 *
 * KEY AGENTS:
 *   - Commercial Derivative Developers: Institutional/arbitrage — pay licensing fees but benefit from clear rules enabling licensing negotiations
 *   - Original Rights Holders: Institutional/arbitrage — extract licensing revenue; maintain enforcement gates on commercial use
 *   - Non-Commercial Transformative Users: Moderate/mobile — exempt from licensing under the carveout; experience pure coordination benefit
 *   - Emerging Subcultural Creators: Powerless/trapped — trapped in derivative media; monetization requires authorization they cannot afford or negotiate
 *   - Creative Commons / Open Culture Coalition: Organized/constrained — building alternative licensing pathways to expand carveout and reduce extraction dependency
 *   - Copyright Registry and Enforcement Apparatus: Institutional/arbitrage — maintains registration and takedown notice processing; operates largely independently of whether carveout distinction is enforced systematically
 *   - Analytical Observer: Analytical/analytical — risks naturalizing the licensing boundary as immutable rather than as contingent institutional choice
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(derivative_work_statutory_boundary__hybrid_carveout_reading, 0.48).
domain_priors:suppression_score(derivative_work_statutory_boundary__hybrid_carveout_reading, 0.52).
domain_priors:theater_ratio(derivative_work_statutory_boundary__hybrid_carveout_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(derivative_work_statutory_boundary__hybrid_carveout_reading, extractiveness, 0.48).
narrative_ontology:constraint_metric(derivative_work_statutory_boundary__hybrid_carveout_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(derivative_work_statutory_boundary__hybrid_carveout_reading, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(derivative_work_statutory_boundary__hybrid_carveout_reading, tangled_rope).
narrative_ontology:human_readable(derivative_work_statutory_boundary__hybrid_carveout_reading, "Derivative Work Statutory Boundary (Hybrid Carveout Reading)").
narrative_ontology:topic_domain(derivative_work_statutory_boundary__hybrid_carveout_reading, "intellectual_property_law/copyright/information_economics").

domain_priors:requires_active_enforcement(derivative_work_statutory_boundary__hybrid_carveout_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(derivative_work_statutory_boundary__hybrid_carveout_reading, '76e74136-798b-4185-a823-8442ae0f1221').
narrative_ontology:cs_kernel_codification('76e74136-798b-4185-a823-8442ae0f1221', formalized).
narrative_ontology:cs_authority_grounding('76e74136-798b-4185-a823-8442ae0f1221', lineage).
narrative_ontology:cs_interpretation_layer_present('76e74136-798b-4185-a823-8442ae0f1221').
narrative_ontology:cs_reading_relation('76e74136-798b-4185-a823-8442ae0f1221', derivative_work_statutory_boundary__enclosure_reading, coexists_with).
narrative_ontology:cs_reading_relation('76e74136-798b-4185-a823-8442ae0f1221', derivative_work_statutory_boundary__coordination_reading, coexists_with).
narrative_ontology:cs_axiom('76e74136-798b-4185-a823-8442ae0f1221', foundational, non_commercial_innovation_categorically_distinct).
narrative_ontology:cs_axiom_status(non_commercial_innovation_categorically_distinct, holdable).
narrative_ontology:cs_axiom_grounding('76e74136-798b-4185-a823-8442ae0f1221', non_commercial_innovation_categorically_distinct, deontological).
narrative_ontology:cs_axiom('76e74136-798b-4185-a823-8442ae0f1221', foundational, licensing_markets_enable_downstream_creator_compensation).
narrative_ontology:cs_axiom_status(licensing_markets_enable_downstream_creator_compensation, holdable).
narrative_ontology:cs_axiom_grounding('76e74136-798b-4185-a823-8442ae0f1221', licensing_markets_enable_downstream_creator_compensation, instrumental).
narrative_ontology:cs_reference_frame('76e74136-798b-4185-a823-8442ae0f1221', copyright_as_exclusive_exploitation_right).
narrative_ontology:cs_drift_state('76e74136-798b-4185-a823-8442ae0f1221', contemporary_platform_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('76e74136-798b-4185-a823-8442ae0f1221', '2026-02-26T14:32:00Z').
narrative_ontology:cs_kernel_id(derivative_work_statutory_boundary__hybrid_carveout_reading, derivative_work_statutory_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(derivative_work_statutory_boundary__hybrid_carveout_reading, commercial_derivative_developers).
narrative_ontology:constraint_beneficiary(derivative_work_statutory_boundary__hybrid_carveout_reading, original_rights_holders).
narrative_ontology:constraint_victim(derivative_work_statutory_boundary__hybrid_carveout_reading, non_commercial_creators).
narrative_ontology:constraint_victim(derivative_work_statutory_boundary__hybrid_carveout_reading, innovation_spillover_commons).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: NON-COMMERCIAL TRANSFORMATIVE USER (ROPE) — Permitted by the carveout; experiences the constraint as pure coordination. Can create derivative works without authorization or licensing cost. Genuine benefit from the coordination mechanism (bright-line clarity on permissibility). Low extraction experienced because exit option is high (mobile — they simply avoid commercial monetization).
constraint_indexing:constraint_classification(derivative_work_statutory_boundary__hybrid_carveout_reading, rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 2: COMMERCIAL DERIVATIVE DEVELOPER (TANGLED ROPE) — Must obtain authorization and pay licensing fees. Experiences mixed coordination and extraction. Genuine coordination benefit: clear statutory rule enables licensing markets and reduces transaction costs (they know exactly when licensing is required). Asymmetric extraction: licensing fees create rent extraction, higher for popular originals. Exit cost is moderate (constrained) — they can choose non-commercial pivots or license at negotiated terms.
constraint_indexing:constraint_classification(derivative_work_statutory_boundary__hybrid_carveout_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: EMERGING SUBCULTURAL CREATOR (SNARE) — Trapped in a derivative medium (fan fiction, remix culture, game modding) where monetization is the only viable path to sustainability, but commercial use requires rights-holder authorization. The carveout exempts them only if they forfeit income. They face maximum suppression: trapped between immobility (cannot exit the derivative creative mode) and prohibition (cannot commercialize without authorization). The constraint extracts their unpaid creative labor while the original rights-holder captures downstream value.
constraint_indexing:constraint_classification(derivative_work_statutory_boundary__hybrid_carveout_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 4: ORIGINAL RIGHTS HOLDER (INSTITUTIONAL) — Exercises a licensing gate on commercial derivatives; non-commercial use is permitted (no enforcement cost there). Experiences the constraint as tangled rope: genuine coordination function (the carveout clarifies what requires permission, reducing transaction costs for licensing negotiations) plus extraction mechanism (licensing fees from commercial derivatives capture surplus from creators who cannot negotiate freely). Arbitrage exit means they can choose which derivatives to permit, charge different rates, or pursue takedowns selectively.
constraint_indexing:constraint_classification(derivative_work_statutory_boundary__hybrid_carveout_reading, tangled_rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: CREATIVE COMMONS / OPEN CULTURE COALITION (SCAFFOLD) — Organized agents working to build alternative licensing pathways (Creative Commons licenses, open-source attribution models, fair-use education norms) that expand the non-commercial carveout and make derivative creation less extraction-dependent. View the statutory boundary as a temporary constraint on innovation that sunset mechanisms (voluntary licensing adoption, cultural norms shifts, technological standardization) are gradually replacing. Theater ratio is moderate because the coalition is investing in real alternative infrastructure, not just performative assertion.
constraint_indexing:constraint_classification(derivative_work_statutory_boundary__hybrid_carveout_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: COPYRIGHT REGISTRY AND ENFORCEMENT APPARATUS (PITON) — The institutional machinery (Copyright Office, takedown notice processing, licensing administration) persists largely through inertia. The apparatus itself does not distinguish between non-commercial and commercial derivatives at enforcement scale — the carveout exists in statute but is not operationalized in the registration or enforcement systems. Theater-heavy: much compliance is theatrical (license agreements that reproduce statutory terms verbatim), and the distinction between permitted and prohibited is maintained more through threat of enforcement than through systematic validation. The apparatus generates its own revenue stream (registration fees, licensing administration) independent of whether the underlying coordination mechanism is functional.
constraint_indexing:constraint_classification(derivative_work_statutory_boundary__hybrid_carveout_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a universalized perspective, the distinction between derivative and original work is a structural category of information economics: any downstream creative work that incorporates upstream content must satisfy both the upstream creator's exclusive right AND the downstream creator's freedom to transform. The carveout (non-commercial exemption) appears as an immutable constraint arising from the irreducible tension between property rights and creative freedom. However, structural analysis reveals beneficiaries (rights holders gain licensing revenue from the commercial carveout), suggesting the mountain classification is aspirational naturalization rather than genuine natural law.
constraint_indexing:constraint_classification(derivative_work_statutory_boundary__hybrid_carveout_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(derivative_work_statutory_boundary__hybrid_carveout_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(derivative_work_statutory_boundary__hybrid_carveout_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(derivative_work_statutory_boundary__hybrid_carveout_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(derivative_work_statutory_boundary__hybrid_carveout_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(derivative_work_statutory_boundary__hybrid_carveout_reading, TR),
    TR >= 0.70.

:- end_tests(derivative_work_statutory_boundary__hybrid_carveout_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.48): Moderate-high. The commercial carveout creates licensing revenue for rights holders (extraction benefit) while exempting non-commercial creators from costs (reduces extraction on that group). But the net effect is extraction because commercial creators pay licensing fees and cannot freely transform; the carveout does not eliminate asymmetry, merely relocates it. The rising trajectory (0.35 → 0.48 over the interval) reflects increasing enforcement maturity and licensing market concentration, which have intensified the extraction mechanism. Suppression (0.52): Moderate. Non-commercial users face low suppression (they have clear legal protection). Commercial users face moderate suppression (licensing requirements and fee negotiation constraints). Trapped subcultural creators face high suppression (monetization is essential but requires authorization they cannot obtain). Average across perspectives: 0.52 reflects the mixed suppression profile. Theater ratio (0.58): Moderate-high. The non-commercial carveout is partially theatrical—its protection is real but enforcement is spotty, and many creators self-censor out of caution rather than relying on the legal carveout. Licensing agreements are largely templatic (reproduction of statutory terms). The rise from 0.48 to 0.58 reflects increasing reliance on automated enforcement and standardized licensing, which reduces genuine negotiation content.
 *
 * PERSPECTIVAL GAP:
 *   The commercial/non-commercial boundary creates radically different experiences. Non-commercial users see rope (coordination benefit, no extraction). Commercial developers see tangled rope (licensing requirement as both coordination mechanism and extraction gate). Trapped subcultural creators see snare (no path to monetization without authorization they cannot obtain). Rights holders see tangled rope (licensing revenue plus coordination benefit, with extraction flowing toward them). The open culture coalition sees scaffold (building alternative pathways to replace the statutory boundary). The copyright apparatus sees piton (enforcing a distinction that is increasingly performative). The analytical observer risks mountain (naturalizing the boundary as inherent to information economics). The gap reveals that the hybrid carveout is not a single constraint from a neutral perspective—it is a set of incompatible readings depending on the agent's relationship to the licensing gate.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is determined by each agent's structural position relative to the licensing gate. Non-commercial users derive d ≈ 0.20 (beneficiaries with high exit via non-commercialization); commercial developers derive d ≈ 0.65 (moderate victims constrained by licensing costs); trapped subcultural creators derive d ≈ 0.90 (victims with no exit); rights holders derive d ≈ 0.15 (beneficiaries with arbitrage options). The sigmoid f(d) amplifies the experienced extractiveness: non-commercial users experience f(d) ≈ -0.01 (actually subsidized); trapped creators experience f(d) ≈ 1.42 (maximum perceived extraction). Commercial developers at d ≈ 0.65 experience f(d) ≈ 1.00 (moderate experienced extraction), reflecting that they see both coordination benefit and licensing cost.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved through reading pluralism: the constraint is tangled rope from the analytical perspective (genuine coordination + asymmetric extraction), but each stakeholder experiences a different type depending on their structural position relative to the carveout. The non-commercial user's rope reading is genuine—they truly benefit from coordination without extraction. The trapped creator's snare reading is genuine—they face maximum suppression. The analytical observer's temptation toward mountain classification (naturalizing the boundary) is the primary mandatrophy risk: the constraint appears immutable only when observed from the position of someone who never needs authorization (non-commercial users) or who profits from issuing it (rights holders). The commercial developer's tangled-rope reading reveals the true structure: the constraint is both coordination and extraction depending on the agent's power to negotiate licensing terms.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    commercial_boundary_definition_ambiguity,
    'Where is the bright line between non-commercial and commercial use? Does indirect monetization (ad revenue from free derivative work, value-add bundling, affiliate compensation) constitute commercial use?',
    'Case law interpretation and statutory amendment; evolution of fair-use doctrine boundaries; comparative analysis of jurisdiction implementations (EU vs US vs UK commercial carveout thresholds)',
    'Narrow boundary (strict interpretation): fewer creators qualify for carveout; Snare classification dominates. Wide boundary (permissive interpretation): more non-commercial protection; Rope classification expands.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(commercial_boundary_definition_ambiguity, conceptual, 'Ambiguity in the commercial/non-commercial boundary definition').

omega_variable(
    enforcement_cost_asymmetry,
    'Who bears the burden of proving non-commercial status? Does the creator self-certify, or must the rights holder affirmatively verify?',
    'Empirical study of licensing dispute resolution; analysis of takedown notice accuracy; measurement of false-positive enforcement against legitimate non-commercial uses',
    'Creator burden: carveout is theoretical (high suppression from enforcement risk). Rights-holder burden: carveout is operational (low suppression, real protection for non-commercial users).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_cost_asymmetry, empirical, 'Asymmetry in enforcement burden for non-commercial carveout proof').

omega_variable(
    licensing_market_capture_risk,
    'Does the statutory carveout (commercial-use licensing gate) enable competitive licensing markets, or does it concentrate extraction power in dominant rights holders (network effects, repertoire control)?',
    'Market structure analysis: measurement of licensing fee variance across comparable works; study of licensing negotiation outcomes for independent derivatives; comparison of licensing concentration metrics to oligopoly thresholds',
    'Competitive market: commercial developers experience moderate extraction (Tangled Rope from their perspective). Monopolistic capture: commercial developers experience severe extraction (Snare). This delta determines whether the commercial carveout is a coordination mechanism or an extraction gate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(licensing_market_capture_risk, empirical, 'Whether licensing markets are competitive or concentrated').

omega_variable(
    kernel_reading_contest,
    'Is this hybrid carveout reading (non-commercial permitted, commercial requires license) a genuine middle position, or does it collapse toward one of the sibling readings (pure coordination vs pure enclosure)?',
    'Historical analysis of statutory evolution; comparative reading of legislative intent vs actual enforcement patterns; assessment of whether the carveout is stable or trending toward either sibling reading''s logic',
    'If hybrid is stable: Tangled Rope is correct classification. If collapsing toward coordination: Rope (sibling reading logic). If collapsing toward enclosure: Snare (sibling reading logic). Structural evolution of case law and licensing practice will indicate the stable attractor.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Stability of the hybrid carveout reading relative to sibling readings').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(derivative_work_statutory_boundary__hybrid_carveout_reading, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(deriv_hybrid_theater_t0, derivative_work_statutory_boundary__hybrid_carveout_reading, theater_ratio, 0, 0.48).
narrative_ontology:measurement(deriv_hybrid_theater_t5, derivative_work_statutory_boundary__hybrid_carveout_reading, theater_ratio, 5, 0.54).
narrative_ontology:measurement(deriv_hybrid_theater_t10, derivative_work_statutory_boundary__hybrid_carveout_reading, theater_ratio, 10, 0.58).

% Extraction over time
narrative_ontology:measurement(deriv_hybrid_extract_t0, derivative_work_statutory_boundary__hybrid_carveout_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(deriv_hybrid_extract_t5, derivative_work_statutory_boundary__hybrid_carveout_reading, base_extractiveness, 5, 0.42).
narrative_ontology:measurement(deriv_hybrid_extract_t10, derivative_work_statutory_boundary__hybrid_carveout_reading, base_extractiveness, 10, 0.48).

% Suppression requirement over time
narrative_ontology:measurement(deriv_hybrid_suppress_t0, derivative_work_statutory_boundary__hybrid_carveout_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(deriv_hybrid_suppress_t5, derivative_work_statutory_boundary__hybrid_carveout_reading, suppression_requirement, 5, 0.49).
narrative_ontology:measurement(deriv_hybrid_suppress_t10, derivative_work_statutory_boundary__hybrid_carveout_reading, suppression_requirement, 10, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(derivative_work_statutory_boundary__hybrid_carveout_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(derivative_work_statutory_boundary__hybrid_carveout_reading, 0.18).
narrative_ontology:affects_constraint(derivative_work_statutory_boundary__hybrid_carveout_reading, fair_use_doctrine_scope).
narrative_ontology:affects_constraint(derivative_work_statutory_boundary__hybrid_carveout_reading, digital_rights_management_circumvention).
narrative_ontology:affects_constraint(derivative_work_statutory_boundary__hybrid_carveout_reading, orphan_works_licensing_gap).

% DUAL FORMULATION NOTE:
% The derivative work boundary decomposes into three structurally distinct constraints with different ε values: (1) fair_use_doctrine_scope (ε ≈ 0.25, Mountain for natural-law view; Mountain for coordination view; Rope for practical enforcement) — the underlying fair-use doctrine; (2) this constraint (ε ≈ 0.48, Tangled Rope) — the statutory carveout distinguishing commercial and non-commercial; (3) digital_rights_management_circumvention (ε ≈ 0.68, Snare) — the enforcement mechanism that bypasses statutory carveouts through technical protection. The three constraints are linked: the carveout reading assumes fair use is operative and DRM circumvention is not allowed; violations of either assumption degrade the carveout.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(derivative_work_statutory_boundary__hybrid_carveout_reading, powerless, 0.92).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
