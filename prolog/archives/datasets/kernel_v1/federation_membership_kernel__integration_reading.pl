% ============================================================================
% CONSTRAINT STORY: federation_membership_kernel__integration_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_federation_membership_kernel__integration_reading, []).

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
 *   constraint_id: federation_membership_kernel__integration_reading
 *   human_readable: EU Free Movement as Supranational Integration (ECJ Expansionism Reading)
 *   domain: political_economy/federalism/migration_policy
 *
 * SUMMARY:
 *   This constraint instantiates the integration_reading of the
 *   federation_membership_kernel — the commitment that free movement is a
 *   fundamental right constitutive of EU citizenship and single market
 *   completion, and that supranational authority (ECJ) should interpret free
 *   movement scope expansively to maximize labor mobility and equal
 *   treatment. This reading collides with two sibling interpretations: the
 *   member_sovereignty_reading (which prioritizes national welfare capacity
 *   and labor protection) and the welfare_coordination_reading (which treats
 *   free movement as operating through coordination of national welfare
 *   systems, not supranational override). The integration_reading is the most
 *   expansive interpretation — it maximizes beneficiary rights while
 *   externalizing costs to welfare systems, displaced labor, sending states,
 *   and national labor institutions. The constraint exhibits tangled-rope
 *   structure: it performs genuine coordination (solving labor market access
 *   problems for mobile workers and employers) while simultaneously
 *   extracting from multiple victim sets without compensatory mechanisms. The
 *   extractiveness trajectory shows increasing extraction over time (0.32 →
 *   0.58) as ECJ doctrine accumulates and scope expands; suppression
 *   increases as national labor protections are progressively overridden;
 *   theater remains moderate because the institutional mechanism is fairly
 *   transparent (ECJ doctrine is explicit, not hidden).
 *
 * KEY AGENTS:
 *   - Mobile High-Skill Workers: Primary beneficiaries (institutional/arbitrage) — access to supranational labor market, non-discrimination protection, wage arbitrage across jurisdictions
 *   - Employers in Labor-Intensive Sectors: Secondary beneficiaries (powerful/arbitrage) — expanded recruitment pool, wage suppression in low-skill sectors, flexibility to locate production across borders
 *   - EU Institutions (ECJ, Commission): Beneficiary-aligned actors (institutional/constrained) — expanding free movement doctrine serves federalizing agenda and supranational institutional capacity
 *   - Displaced Local Labor in Receiving States: Primary victims (powerless/trapped) — wage suppression, employment discrimination, erosion of protective frameworks with no exit option
 *   - Receiving State Welfare Systems: Systemic victim (institutional/constrained) — bearing costs for economically inactive migrants, fiscal externalities, without compensatory fiscal transfers from EU or migrant-sending states
 *   - Sending States (Brain Drain): Secondary victims (institutional/constrained) — losing high-skill tax base and human capital; externalized costs of labor loss without compensation mechanisms
 *   - National Labor Protections Institutions: Degraded institutional victim — collective agreements, wage councils, apprenticeship frameworks hollowed by supranational override (piton perspective)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(federation_membership_kernel__integration_reading, 0.58).
domain_priors:suppression_score(federation_membership_kernel__integration_reading, 0.62).
domain_priors:theater_ratio(federation_membership_kernel__integration_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(federation_membership_kernel__integration_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(federation_membership_kernel__integration_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(federation_membership_kernel__integration_reading, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(federation_membership_kernel__integration_reading, tangled_rope).
narrative_ontology:human_readable(federation_membership_kernel__integration_reading, "EU Free Movement as Supranational Integration (ECJ Expansionism Reading)").
narrative_ontology:topic_domain(federation_membership_kernel__integration_reading, "political_economy/federalism/migration_policy").

domain_priors:requires_active_enforcement(federation_membership_kernel__integration_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(federation_membership_kernel__integration_reading, '84b17d6f-aa9f-4f4c-8942-29279b741de9').
narrative_ontology:cs_kernel_codification('84b17d6f-aa9f-4f4c-8942-29279b741de9', formalized).
narrative_ontology:cs_authority_grounding('84b17d6f-aa9f-4f4c-8942-29279b741de9', extraction).
narrative_ontology:cs_interpretation_layer_present('84b17d6f-aa9f-4f4c-8942-29279b741de9').
narrative_ontology:cs_reading_relation('84b17d6f-aa9f-4f4c-8942-29279b741de9', federation_membership_kernel__member_sovereignty_reading, coexists_with).
narrative_ontology:cs_reading_relation('84b17d6f-aa9f-4f4c-8942-29279b741de9', federation_membership_kernel__welfare_coordination_reading, influences).
narrative_ontology:cs_axiom('84b17d6f-aa9f-4f4c-8942-29279b741de9', foundational, free_movement_is_fundamental_right).
narrative_ontology:cs_axiom_status(free_movement_is_fundamental_right, holdable).
narrative_ontology:cs_axiom_grounding('84b17d6f-aa9f-4f4c-8942-29279b741de9', free_movement_is_fundamental_right, deontological).
narrative_ontology:cs_axiom('84b17d6f-aa9f-4f4c-8942-29279b741de9', foundational, supranational_authority_maximizes_integration).
narrative_ontology:cs_axiom_status(supranational_authority_maximizes_integration, holdable).
narrative_ontology:cs_axiom_grounding('84b17d6f-aa9f-4f4c-8942-29279b741de9', supranational_authority_maximizes_integration, instrumental).
narrative_ontology:cs_reference_frame('84b17d6f-aa9f-4f4c-8942-29279b741de9', eu_federal_completion).
narrative_ontology:cs_drift_state('84b17d6f-aa9f-4f4c-8942-29279b741de9', post_lisbon_treaty_expansion, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('84b17d6f-aa9f-4f4c-8942-29279b741de9', '').
narrative_ontology:cs_kernel_id(federation_membership_kernel__integration_reading, federation_membership_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(federation_membership_kernel__integration_reading, mobile_workers_high_skill).
narrative_ontology:constraint_beneficiary(federation_membership_kernel__integration_reading, receiving_state_employers).
narrative_ontology:constraint_beneficiary(federation_membership_kernel__integration_reading, supranational_institutions).
narrative_ontology:constraint_victim(federation_membership_kernel__integration_reading, displaced_local_labor).
narrative_ontology:constraint_victim(federation_membership_kernel__integration_reading, receiving_state_welfare_systems).
narrative_ontology:constraint_victim(federation_membership_kernel__integration_reading, sending_state_human_capital).
narrative_ontology:constraint_victim(federation_membership_kernel__integration_reading, national_labor_market_protections).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DISPLACED LOCAL LABOR IN RECEIVING STATE (SNARE) — Faces wage suppression, employment discrimination, and erosion of labor market protections without meaningful exit. Suppression of alternatives is structural: union wage floors are hollowed by supranational non-discrimination rulings; apprenticeship pathways are devalued by influx of trained workers; retraining programs cannot match demographic scale of displacement. The ECJ framework criminalizes protective measures as discrimination. This agent experiences maximum extraction with no exit option.
constraint_indexing:constraint_classification(federation_membership_kernel__integration_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: MEMBER STATE AS ORGANIZED COLLECTIVE (TANGLED ROPE) — The receiving member state experiences genuine coordination benefits (access to talent pool, labor market flexibility, economies of scale in services) alongside asymmetric extraction (bearing welfare costs for economically inactive migrants, wage suppression in low-skill sectors, fiscal externalities). The state is organized (can petition EU, propose opt-out directives, negotiate in Council) but constrained by ECJ rulings that override domestic labor law. Exit would mean EU departure — extremely costly. Moderate organized power but high structural suppression of alternatives.
constraint_indexing:constraint_classification(federation_membership_kernel__integration_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: MOBILE HIGH-SKILL WORKERS (ROPE) — Primary beneficiaries experience the constraint as pure coordination: it solves their collective action problem of accessing labor markets across borders. They benefit from wage arbitrage (moving to high-wage economies), arbitrage of social benefits, and supranational enforcement of non-discrimination. The framework subsidizes their mobility through ECJ expansion of free movement rights. These agents have institutional power (can organize diaspora networks, access multiple labor markets) and arbitrage capacity (can move between jurisdictions and return). They experience minimal extraction — the constraint extracts FOR them, not from them.
constraint_indexing:constraint_classification(federation_membership_kernel__integration_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(continental))).

% PERSPECTIVE 4: EMPLOYERS IN RECEIVING STATES (ROPE) — Secondary beneficiaries. The constraint functions as pure coordination that solves their labor supply problem — expanding the geographic scope of recruitment lowers their wage costs and increases workforce flexibility. They benefit from supranational enforcement of free movement and have arbitrage capacity (can locate production in different jurisdictions, access workers transnationally). The ECJ expansion maximizes their benefits by ensuring broad definition of economically active and minimizing restrictions on residence. Powerful actors with low experienced extraction.
constraint_indexing:constraint_classification(federation_membership_kernel__integration_reading, rope,
    context(agent_power(powerful),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(continental))).

% PERSPECTIVE 5: EU INSTITUTIONAL ACTORS (SCAFFOLD) — The supranational authority (ECJ, European Commission) interprets and expands free movement doctrine as a means of completing the single market and deepening integration. From the EU's perspective, this is a temporary coordination mechanism: the scaffold is the transitional period during which member states harmonize social policy, fiscal transfers, and labor market regulation upward. The sunset is a European Social Union with harmonized welfare systems and compensatory fiscal transfers. The ECJ sees its rulings as removing barriers to the ultimate equilibrium (full mobility). Current suppression (national welfare conflicts) is treated as a problem to be solved through integration, not as a permanent feature.
constraint_indexing:constraint_classification(federation_membership_kernel__integration_reading, scaffold,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 6: NATIONAL LABOR PROTECTIONS AS PITON — The institutional infrastructure of member state labor law (wage councils, apprenticeship frameworks, union-negotiated collective agreements, social market employment norms) is maintained as degraded, partially performative institutions. ECJ doctrine treats these as barriers to free movement rather than legitimate coordination mechanisms. The institutions persist through inertia (constitutional entrenchment, union membership) but have lost functional authority — employers navigate around them using supranational non-discrimination doctrine. Theater ratio is high: the institutions appear to function while their core function (protecting domestic labor standards) is hollowed by supranational override.
constraint_indexing:constraint_classification(federation_membership_kernel__integration_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 7: SENDING STATES (BRAIN DRAIN, TANGLED ROPE) — States that net export labor experience both coordination benefits (access to remittances, higher wages for emigrants, demographic relief from structural unemployment) and extraction (loss of high-skill labor and tax base, reduced return on education investments, hollowing of professional sectors). The supranational expansion of free movement extracts from these states by maximizing worker exit without compensatory mechanisms. They are organized institutional actors but constrained in their exit options (cannot restrict emigration without ECJ violation) and bear real costs that supranational framework externalizes rather than coordinates.
constraint_indexing:constraint_classification(federation_membership_kernel__integration_reading, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER / NATURALIZATION RISK (MOUNTAIN) — From a civilizational perspective, this reading risks naturalizing a specific institutional choice as an inherent property of markets or rights. The frame treats 'free movement as fundamental right' and 'supranational expansion' as inevitable features of economic integration, analogous to physical laws. However, the structural data contradicts this: beneficiaries exist (mobile workers, employers, supranational institutions); victims exist (displaced labor, welfare systems); suppression is manufactured through doctrine, not inherent. The engine's false-summit detector will reveal this as naturalization of a contestable institutional commitment.
constraint_indexing:constraint_classification(federation_membership_kernel__integration_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(federation_membership_kernel__integration_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(federation_membership_kernel__integration_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(federation_membership_kernel__integration_reading, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(federation_membership_kernel__integration_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(federation_membership_kernel__integration_reading, TR),
    TR >= 0.70.

:- end_tests(federation_membership_kernel__integration_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. The constraint extracts substantially from multiple victim sets (displaced labor faces wage suppression; welfare systems bear fiscal costs; sending states lose human capital) while transferring these benefits to mobile workers and employers. The extraction is not maximum (0.72+) because beneficiaries also provide some coordination value (employers genuinely solve labor market matching; EU institutions produce real single-market efficiencies). The trajectory from 0.32 to 0.58 reflects ECJ doctrine accumulation over the observation period — early free-movement provisions had narrower scope and lower extraction; later expansions (Citizenship Directive, case law broadening economically-inactive rights, anti-discrimination doctrine) increased extraction by reducing member-state exit options. Suppression (0.62): High. Alternatives to free movement doctrine are actively suppressed through ECJ override of national labor law, non-discrimination doctrine criminalizing protective measures, and infringement proceedings against member states. The suppression is not total (member states retain some legislative space) but severe enough to prevent meaningful member-state resistance. Theater ratio (0.48): Moderate-low. The constraint is not primarily performative — ECJ doctrine is explicit and enforced, ECJ rulings have real effects on labor market policy, and the institutional mechanism (supranational authority over national labor law) is transparent. The theater is moderate rather than low because some institutional performance occurs: member states perform compliance while seeking exceptions; EU institutions perform 'federalizing' rhetoric while denying they are constructing new extraction mechanisms; beneficiaries perform gratitude to 'rights-based' framing while benefiting from asymmetric extraction.
 *
 * PERSPECTIVAL GAP:
 *   The integration_reading produces maximal perspectival divergence across the observation space. Mobile workers and employers see rope or even pure coordination — the constraint solves their labor-market access problem cleanly, with supranational enforcement providing reliable non-discrimination protection. Receiving-state governments see tangled rope — genuine coordination benefits in labor market access and economic flexibility, but asymmetric extraction through welfare costs and labor-market pressures without compensatory mechanisms. Displaced local labor sees snare — no benefit, only extraction through wage suppression and narrowed employment options, with no exit (trapped in national labor market, cannot follow jobs to other EU states competitively, cannot force ECJ to recognize protective measures as compatible with free movement). Sending states see tangled rope with brain-drain asymmetry — some coordination benefit (remittances, demographic relief from unemployment) but primary extraction through loss of human capital and tax base. National labor institutions see piton — their formal authority persists but is functionally hollowed by supranational override doctrine. The civilizational analytical observer risks seeing mountain (free movement as inherent to markets or to rights), but the structural data reveals this as false summit: the constraint is a specific institutional choice, not a natural law, and benefits specific agents while extracting from others.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's d value (structural position relative to extraction flow) is derived from power level, exit options, and beneficiary/victim declaration. Mobile high-skill workers have d ≈ 0.05 (full beneficiaries with arbitrage exit) → f(d) ≈ -0.12 → negative χ (extraction flows toward them). EU institutions have d ≈ 0.15 (beneficiaries aligned with mechanism, constrained by member-state resistance) → f(d) ≈ -0.01 → near-zero χ. Receiving-state members have d ≈ 0.68 (organized but constrained, bearing welfare costs) → f(d) ≈ 1.05 → moderate χ. Displaced labor has d ≈ 0.92 (powerless/trapped victims) → f(d) ≈ 1.35 → high χ. The scope modifier σ(continental) = 1.1 scales extractiveness upward for all perspectives — the constraint operates at continental scale, making verification and exit more difficult. The perspectival gap is pronounced: beneficiaries experience rope (coordination solving their collective action problem); victims at the powerless/trapped end experience snare (extraction with no exit); organized but constrained member states experience tangled rope (mixed benefits and costs with constrained agency); the analytical observer risks seeing mountain (naturalizing the institutional choice as inevitable).
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by instantiating one reading of a contested kernel within a federation. The tangled-rope classification is correct for the integration_reading, but different readings of the same kernel produce different classifications (member_sovereignty_reading would be rope or scaffold; welfare_coordination_reading would be pure rope). The mandate isn't 'which reading is right' — it's 'which institutional commitment is this federation instantiating?' The integration_reading's tangled-rope structure shows that free movement is both a genuine coordination mechanism (solving labor-market access problems) and an extraction mechanism (externalizing welfare and labor-protection costs). This is not a classification error — it is a statement that the integration_reading creates both coordination benefits (for mobile workers, employers, single market) and asymmetric extraction (from welfare systems, displaced labor, sending states). The sibling readings would dissolve the tangled-rope by either: (a) member_sovereignty — returning to pure rope by restoring member-state exit options to refuse harmful migration, or (b) welfare_coordination — maintaining rope by adding compensatory fiscal mechanisms that convert extraction into coordination.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    welfare_fiscal_compensation_threshold,
    'What level of fiscal compensation from EU or receiving state would convert the extraction experienced by welfare systems from snare-level to rope-level (coordination with distributed costs)?',
    'Comparative analysis of fiscal transfer mechanisms in federal systems (US, Canada, Australia); estimation of dynamic fiscal effects of migration on receiving states; modeling of cost-shifting under different compensation architectures',
    'If low threshold (< 0.15 of migrant-driven fiscal cost): coordination frame becomes tenable, constraint reclassifies to rope from member-state perspective. If high threshold (> 0.45): current architecture is fundamentally asymmetric extraction, snare classification from welfare-system perspective becomes appropriate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(welfare_fiscal_compensation_threshold, empirical, 'Fiscal compensation level required to convert extraction to coordination').

omega_variable(
    labor_market_protection_foreclosure,
    'Does the integration reading''s free-movement doctrine logically foreclose the member-sovereignty reading''s labor-protection rights, or do the readings coexist as competing institutional commitments?',
    'Doctrinal analysis: can a member state hold both commitment to ECJ-enforced free movement AND commitment to domestic labor protection frameworks? Court decisions testing boundaries (Omega framework test: if a court rules that ''labor protection X'' is incompatible with free movement as such, not merely with this directive, then foreclosure is instantiated).',
    'If forecloses: the readings are incompatible in any single institutional framework; ECJ doctrine is making a zero-sum choice. If coexists: the tension is resolvable through institutional redesign (e.g., higher-order coordination rule saying ''free movement applies except where incompatible with fundamental social rights'').',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(labor_market_protection_foreclosure, conceptual, 'Whether integration reading logically forecloses member-sovereignty reading').

omega_variable(
    ecj_expansion_intention,
    'Is the ECJ''s expansive interpretation of free movement doctrine intentionally extractive (deliberately maximizing beneficiary advantages) or incidentally extractive (doctrine-following that produces asymmetric effects as byproduct)?',
    'Institutional history of ECJ case selection and reasoning; analysis of alternative doctrinal paths ECJ could have taken; comparison of ECJ expansion trajectory with coordinating interpretations used in other federations (e.g., US commerce clause); interviews with ECJ justices and legal scholars about institutional incentives',
    'If intentional extraction: the tangled-rope classification is correct — ECJ is an active beneficiary, not neutral arbiter. If incidental: constraint might be better classified as piton (institutional momentum without beneficiary awareness) or rope (coordination mechanism with unfortunate side effects).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(ecj_expansion_intention, conceptual, 'Whether ECJ expansion is intentionally extractive or incidentally so').

omega_variable(
    brain_drain_quantification,
    'What proportion of sending-state fiscal loss from migration is attributable to this constraint''s supranational enforcement (vs. to other factors like wage differentials, quality-of-life factors, historical emigration networks)?',
    'Counterfactual analysis: estimate brain drain and fiscal loss under member-sovereignty reading (national restrictions permitted) vs. integration reading (supranational enforcement); sensitivity analysis on free-movement doctrine scope',
    'If high attribution (>60%): sending states are clear victims of this reading''s extraction; constraint is asymmetric. If low attribution (<30%): brain drain is driven by market factors, not by the constraint itself.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(brain_drain_quantification, empirical, 'Brain drain attributable to supranational free-movement enforcement').

omega_variable(
    kernel_reading_contest_outcome,
    'This constraint instantiates the integration_reading of the federation_membership_kernel. The sibling readings (member_sovereignty_reading, welfare_coordination_reading) represent alternative institutional commitments to the same kernel. Which reading will prevail in future EU development — will integration intensify, or will member states reassert sovereignty/welfare coordination?',
    'Long-term institutional trajectory: ECJ case law direction; political shifts in member-state representation; fiscal pressures on welfare states; migration backlash and democratic demand for national controls; success/failure of EU-level social policy harmonization (European Social Union project). Observable signals: do future ECJ rulings expand or contract free movement scope? Do member states negotiate opt-outs or carve-outs? Does EU develop compensatory fiscal architecture?',
    'If integration_reading dominates: this constraint''s extractive structure becomes entrenched, and the tangled_rope classification may migrate toward snare. If member_sovereignty_reading reasserts: the constraint''s institutional basis is partially dismantled, reclassifying toward rope or piton. If welfare_coordination_reading instantiates: constraint converts to coordination mechanism with compensatory mechanisms (rope or scaffold).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_contest_outcome, conceptual, 'Institutional trajectory: which reading of the federation_membership_kernel will dominate').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(federation_membership_kernel__integration_reading, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fed_int_tr_t0, federation_membership_kernel__integration_reading, theater_ratio, 0, 0.38).
narrative_ontology:measurement(fed_int_tr_t5, federation_membership_kernel__integration_reading, theater_ratio, 5, 0.43).
narrative_ontology:measurement(fed_int_tr_t10, federation_membership_kernel__integration_reading, theater_ratio, 10, 0.48).

% Extraction over time
narrative_ontology:measurement(fed_int_be_t0, federation_membership_kernel__integration_reading, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(fed_int_be_t5, federation_membership_kernel__integration_reading, base_extractiveness, 5, 0.45).
narrative_ontology:measurement(fed_int_be_t10, federation_membership_kernel__integration_reading, base_extractiveness, 10, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(fed_int_su_t0, federation_membership_kernel__integration_reading, suppression_requirement, 0, 0.42).
narrative_ontology:measurement(fed_int_su_t5, federation_membership_kernel__integration_reading, suppression_requirement, 5, 0.52).
narrative_ontology:measurement(fed_int_su_t10, federation_membership_kernel__integration_reading, suppression_requirement, 10, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(federation_membership_kernel__integration_reading, resource_allocation).
narrative_ontology:affects_constraint(federation_membership_kernel__integration_reading, federation_membership_kernel__member_sovereignty_reading).
narrative_ontology:affects_constraint(federation_membership_kernel__integration_reading, federation_membership_kernel__welfare_coordination_reading).
narrative_ontology:affects_constraint(federation_membership_kernel__integration_reading, european_social_union_convergence).
narrative_ontology:affects_constraint(federation_membership_kernel__integration_reading, labor_market_segmentation_by_origin).

% DUAL FORMULATION NOTE:
% This is one reading of the federation_membership_kernel. The sibling readings (member_sovereignty_reading, welfare_coordination_reading) represent alternative institutional commitments to the same kernel. Each reading produces its own constraint with distinct ε, victims, beneficiaries, and classification. The network links show how the integration_reading affects (creates structural pressure on) the other readings: ECJ expansion of free movement narrows the institutional space available for member-sovereignty and welfare-coordination readings. The constraint also affects downstream claims about labor-market segmentation and social union convergence.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(federation_membership_kernel__integration_reading, institutional, 0.18).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
