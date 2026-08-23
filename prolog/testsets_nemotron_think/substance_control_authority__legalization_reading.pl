% ============================================================================
% CONSTRAINT STORY: substance_control_authority__legalization_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_substance_control_authority__legalization_reading, []).

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
 *   constraint_id: substance_control_authority__legalization_reading
 *   human_readable: State Authority to Regulate Drug Markets as Legal Commerce
 *   domain: public_health_policy/criminal_justice/political_economy
 *
 * SUMMARY:
 *   This constraint story models the legalization reading of the
 *   substance_control_authority kernel: state authority exercised through
 *   legal regulated markets with quality and access controls, replacing
 *   criminal prohibition. The constraint coordinates legal commerce (solving
 *   prohibition's quality/access/safety failures) while extracting via
 *   taxation, licensing, and compliance costs. It actively enforces against
 *   the residual black market. The claimed type is tangled_rope — genuine
 *   coordination function (quality control, black market displacement, public
 *   health tools) with asymmetric extraction (state revenue, licensed
 *   producer profits, consumer compliance costs borne disproportionately by
 *   users). The engine will compute per-seat classifications from the
 *   structural data authored here.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(substance_control_authority__legalization_reading, 0.48).
domain_priors:suppression_score(substance_control_authority__legalization_reading, 0.52).
domain_priors:theater_ratio(substance_control_authority__legalization_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(substance_control_authority__legalization_reading, extractiveness, 0.48).
narrative_ontology:constraint_metric(substance_control_authority__legalization_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(substance_control_authority__legalization_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(substance_control_authority__legalization_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(substance_control_authority__legalization_reading, resistance, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(substance_control_authority__legalization_reading, tangled_rope).
narrative_ontology:human_readable(substance_control_authority__legalization_reading, "State Authority to Regulate Drug Markets as Legal Commerce").
narrative_ontology:topic_domain(substance_control_authority__legalization_reading, "public_health_policy/criminal_justice/political_economy").

domain_priors:requires_active_enforcement(substance_control_authority__legalization_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(substance_control_authority__legalization_reading, '2370340e-536d-4753-96bf-54e3a1503bad').
narrative_ontology:cs_kernel_codification('2370340e-536d-4753-96bf-54e3a1503bad', formalized).
narrative_ontology:cs_authority_grounding('2370340e-536d-4753-96bf-54e3a1503bad', lineage).
narrative_ontology:cs_interpretation_layer_present('2370340e-536d-4753-96bf-54e3a1503bad').
narrative_ontology:cs_reading_relation('2370340e-536d-4753-96bf-54e3a1503bad', substance_control_authority__prohibition_reading, forecloses).
narrative_ontology:cs_reading_relation('2370340e-536d-4753-96bf-54e3a1503bad', substance_control_authority__harm_reduction_reading, coexists_with).
narrative_ontology:cs_axiom('2370340e-536d-4753-96bf-54e3a1503bad', foundational, drug_markets_are_legitimate_commerce).
narrative_ontology:cs_axiom_status(drug_markets_are_legitimate_commerce, holdable).
narrative_ontology:cs_axiom_grounding('2370340e-536d-4753-96bf-54e3a1503bad', drug_markets_are_legitimate_commerce, conventional).
narrative_ontology:cs_axiom('2370340e-536d-4753-96bf-54e3a1503bad', foundational, state_regulation_reduces_net_harm_vs_prohibition).
narrative_ontology:cs_axiom_status(state_regulation_reduces_net_harm_vs_prohibition, holdable).
narrative_ontology:cs_axiom_grounding('2370340e-536d-4753-96bf-54e3a1503bad', state_regulation_reduces_net_harm_vs_prohibition, empirically_contingent).
narrative_ontology:cs_reference_frame('2370340e-536d-4753-96bf-54e3a1503bad', regulated_legal_market_framework).
narrative_ontology:cs_drift_state('2370340e-536d-4753-96bf-54e3a1503bad', contemporary_implementation, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('2370340e-536d-4753-96bf-54e3a1503bad', '2026-08-15T14:30:00Z').
narrative_ontology:cs_kernel_id(substance_control_authority__legalization_reading, substance_control_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(substance_control_authority__legalization_reading, state_regulatory_authority).
narrative_ontology:constraint_beneficiary(substance_control_authority__legalization_reading, licensed_producers).
narrative_ontology:constraint_beneficiary(substance_control_authority__legalization_reading, consumers_users).
narrative_ontology:constraint_beneficiary(substance_control_authority__legalization_reading, public_health_authorities).
narrative_ontology:constraint_beneficiary(substance_control_authority__legalization_reading, third_party_communities).
narrative_ontology:constraint_victim(substance_control_authority__legalization_reading, consumers_users).
narrative_ontology:constraint_victim(substance_control_authority__legalization_reading, unlicensed_producers_black_market).
narrative_ontology:constraint_vindicates(substance_control_authority__legalization_reading, regulated_markets_reduce_harm_vs_prohibition).
narrative_ontology:constraint_vindicates(substance_control_authority__legalization_reading, state_regulation_eliminates_black_markets).
narrative_ontology:constraint_vindicates(substance_control_authority__legalization_reading, quality_control_protects_consumers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Enacts and enforces the legal framework for drug markets: licensing, quality standards, taxation, age/quantity limits, advertising restrictions. Collects tax revenue and licensing fees. Bears enforcement costs (inspections, compliance, black market suppression). Can amend legislation but faces political constraints.
narrative_ontology:constraint_stakeholder(substance_control_authority__legalization_reading, state_regulatory_authority, agenda_setter,
    institutional, generational, arbitrage, national).

% Operate legally licensed production and distribution. Capture the legal market share previously held by black market. Pay licensing fees and taxes, comply with quality/access regulations. Benefit from state enforcement against unlicensed competitors. Exit requires abandoning licensed operations or moving to another jurisdiction.
narrative_ontology:constraint_stakeholder(substance_control_authority__legalization_reading, licensed_producers, beneficiary,
    organized, biographical, constrained, national).

% Access regulated products with known potency/purity, age-gated purchase, and legal protection. Pay higher prices inclusive of taxes and compliance costs; face quantity limits and purchase restrictions. If dependent, cannot easily exit consumption; if recreational, can substitute or reduce use. Previously criminalized, now legal but regulated.
narrative_ontology:constraint_stakeholder(substance_control_authority__legalization_reading, consumers_users, beneficiary,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(substance_control_authority__legalization_reading, consumers_users, payer).

% Previously supplied the entire market; now criminalized and actively suppressed by state enforcement. Lose market share to licensed producers. Face arrest, asset seizure, violence from competitors. Cannot legally transition to licensed status (barriers: capital, compliance, criminal record). Exit means leaving the trade entirely or moving to jurisdictions without legalization.
narrative_ontology:constraint_stakeholder(substance_control_authority__legalization_reading, unlicensed_producers_black_market, payer,
    moderate, biographical, trapped, national).

% Gain regulatory tools: product standards, potency labeling, health warnings, consumption data, revenue earmarked for treatment/prevention. Shift from punitive to health-oriented approach. Bear responsibility for monitoring outcomes (overdose rates, youth access, dependence). Their authority expands but depends on political will and funding.
narrative_ontology:constraint_stakeholder(substance_control_authority__legalization_reading, public_health_authorities, beneficiary,
    institutional, generational, analytical, national).

% Enforce the new regulatory regime: inspect licensed facilities, suppress unlicensed supply, enforce age/quantity limits. Resources shift from low-level possession arrests to regulatory compliance and black market interdiction. Institutional culture and metrics adjust; some units resist mission change. Budget and prestige tied to enforcement volume.
narrative_ontology:constraint_stakeholder(substance_control_authority__legalization_reading, law_enforcement, agenda_setter,
    institutional, generational, analytical, national).

% Experience reduced drug-related crime, violence, and public disorder as black markets shrink. Property values and local safety improve. May host licensed retail outlets (zoning disputes). Bear externalities if regulation fails (e.g., increased public consumption, youth access). Cannot easily relocate; voice matters in local regulatory decisions.
narrative_ontology:constraint_stakeholder(substance_control_authority__legalization_reading, third_party_communities, beneficiary,
    organized, generational, constrained, regional).

% Evaluates the constraint's net effects across all seats: extraction vs. coordination, suppression vs. protection, displacement vs. elimination of black markets, health outcomes, fiscal impacts, equity across communities. No material stake; provides the engine's computational seat.
narrative_ontology:constraint_stakeholder(substance_control_authority__legalization_reading, analytical_observer, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Regulates drug markets as legal commerce to ensure product quality and potency transparency, control access through age and quantity limits, eliminate unregulated black markets and their associated violence, and generate state revenue through taxation and licensing to fund public health and enforcement.
% TRANSFER_FUNCTION: Moves tax revenue and licensing fees from consumers and licensed producers to the state; moves market share and profit from unlicensed black market operators to licensed producers; moves regulatory compliance costs (testing, labeling, security, record-keeping) to all legal market participants; moves enforcement costs from criminalizing users to regulating commerce.
% ABSENT_VOICES: People who use drugs and oppose any state regulation of their consumption on autonomy grounds; abolitionist drug policy advocates who view state-regulated markets as legitimizing and entrenching harmful substances; communities disproportionately impacted by previous prohibition who may not trust state regulation to repair harms; indigenous and traditional use communities whose practices may not fit commercial regulatory frameworks.
% DISAPPEARANCE_RATIONALE: If the legal regulatory framework vanished overnight, licensed markets would collapse; unregulated black markets would rapidly re-expand to meet demand; product quality and potency transparency would be lost; state tax revenue and regulatory capacity would evaporate; users would return to criminal victimization and unregulated supply harms; law enforcement would revert to prohibition-era posture; third-party communities would see crime and disorder resurge.
% FOUNDING_PROBLEM: The failure of prohibition to control drug markets: unregulated black markets produced unsafe products of unknown potency, fueled organized crime and violence, criminalized millions of users (disproportionately marginalized communities), failed to protect third parties from drug-related crime and disorder, and consumed vast enforcement resources without reducing supply or demand.
% FOUNDING_PROBLEM_CORROBORATION: Public health research documenting prohibition's harms (overdose crisis driven by unregulated fentanyl-adulterated supply, mass incarceration for possession, racial disparities in enforcement); economic analyses of black market externalities (violence, corruption, lost tax revenue); law enforcement testimony on prohibition's enforcement futility (e.g., LEAP - Law Enforcement Action Platform); historical record of alcohol prohibition's identical failure mode.
narrative_ontology:disappearance_verdict(substance_control_authority__legalization_reading, world_rearranges).
narrative_ontology:founding_problem_status(substance_control_authority__legalization_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(substance_control_authority__legalization_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(substance_control_authority__legalization_reading, 'none', 1).
narrative_ontology:epsilon_provenance(substance_control_authority__legalization_reading, 0.48, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(substance_control_authority__legalization_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(substance_control_authority__legalization_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(substance_control_authority__legalization_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.48) reflects significant but not dominant extraction: state captures tax revenue (~20-30% retail), licensed producers capture market rents, consumers pay compliance-inflated prices. Suppression (0.52) is moderate: enforcement targets unlicensed supply and regulatory violations, not users per se, but requires active interdiction. Theater ratio (0.28) rises over time as regulatory capture and industry lobbying shape rules toward incumbent licensed producers' interests. Accessibility collapse (0.45) is partial: legal access exists but is gated (age, quantity, geography); black market persists at margins. Resistance (0.48) comes from residual black market, anti-legalization political forces, and users facing access restrictions. All metrics authored at shared time points (0,5,10,15,20) for aligned temporal analysis.
 *
 * PERSPECTIVAL GAP:
 *   The state/licensed producer seats experience this as coordination: they built and maintain a functional regulated market that solves prohibition's failures. The consumer/user seat experiences both coordination (safer supply) and extraction (taxes, restrictions) — net classification depends on dependency level and access barriers. The unlicensed producer seat experiences pure suppression/extraction (criminalization, market loss). The engine computes this divergence from the structural power/exit/role data; the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   State regulatory authority: structural beneficiary (collects revenue, sets rules, arbitrage exit) — d near 0.15. Licensed producers: beneficiaries (capture legal market, constrained exit) — d ~0.25. Consumers/users: dual — genuine beneficiaries of quality/safety but payers of taxes/compliance costs; net directionality depends on use pattern (dependent users more target-like, d~0.6; recreational users more symmetric, d~0.45). Unlicensed producers: full targets/payers (criminalized, trapped) — d~0.95. Public health authorities: beneficiaries (expanded tools, data, funding) — d~0.2. Law enforcement: agenda setters with shifted mission — d~0.3 (enforcement costs but institutional maintenance). Third-party communities: beneficiaries (reduced crime/disorder) — d~0.25. Analytical observer: d=0.5 by definition.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (prohibition's failure) remains live — the harms prohibition produced persist as the justification for regulation. However, the constraint risks mandatrophy if: (1) regulatory capture shifts rules toward licensed producer profits over public health; (2) tax rates exceed the black market displacement threshold, reviving unregulated supply; (3) access restrictions recreate the exclusion that drove black markets. The theater_ratio rise (0.15→0.28) signals early Goodhart drift: compliance metrics (license counts, inspection pass rates) may substitute for outcome metrics (overdose deaths, youth access, black market share). The constraint is not yet a piton — its coordination function is real and actively maintained — but the trajectory warrants monitoring.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    committer_kernel_reading_structure,
    'How does the legalization reading''s structural relationship to the substance_control_authority kernel differ from its sibling readings, and what classification consequences follow from the reading''s specific beneficiary/victim structure?',
    'Compare the three readings'' constraint stories side-by-side: map each reading''s beneficiary/victim declarations, directionality profiles, and claimed types. The engine''s per-seat classification will reveal whether the kernel''s authority structure computes differently under each reading.',
    'If prohibition_reading computes as snare (high extraction from users via criminalization) and legalization_reading computes as tangled_rope (coordination + asymmetric extraction), the kernel itself is not a single constraint but a generator of structurally distinct constraints. The committer frame''s value is exposing this structural divergence.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(committer_kernel_reading_structure, conceptual, 'Kernel-reading structural delta and its classification consequences').

omega_variable(
    black_market_elimination_vs_displacement,
    'Does the legal regulated market structurally eliminate the black market, or merely displace it to the margins (e.g., unregulated potency products, tax-evading sales, jurisdictions without legalization)?',
    'Longitudinal market share data: track licensed vs. unlicensed market volume over time in legalized jurisdictions. If unlicensed share approaches zero and stays there, elimination; if it stabilizes at >10-15%, displacement with persistent residual.',
    'If displacement not elimination, the constraint''s suppression requirement remains high indefinitely (enforcement against persistent black market), and the coordination function is incomplete — the ''eliminate black markets'' vindicated proposition fails. This would increase effective extraction (ongoing enforcement costs) and potentially shift classification toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(black_market_elimination_vs_displacement, empirical, 'Whether regulated markets fully eliminate or only partially displace unregulated supply').

omega_variable(
    tax_rate_laffer_curve_for_regulated_drugs,
    'At what tax rate does the legal market''s price advantage over the black market disappear, causing consumers to return to unregulated supply and undermining the constraint''s coordination function?',
    'Econometric estimation of price elasticity of demand for regulated vs. unregulated products; natural experiments from jurisdictions with different tax rates; black market price monitoring.',
    'If current tax rates are near or above the threshold, the constraint''s extractiveness is self-undermining: higher extraction revives the black market it was meant to eliminate. This creates a structural ceiling on extraction that the state may not respect, converting coordination into extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(tax_rate_laffer_curve_for_regulated_drugs, empirical, 'Structural ceiling on extraction before coordination function collapses').

omega_variable(
    suppression_mechanism_regulatory_vs_criminal,
    'Is the suppression exerted by this constraint primarily regulatory (licensing penalties, fines, compliance orders) or criminal (arrest, incarceration for unlicensed activity), and does this distinction affect the suppression metric''s meaning?',
    'Analyze enforcement data: proportion of actions that are administrative vs. criminal; severity distribution of penalties; whether users face criminal penalties for regulatory violations (e.g., possession over limit).',
    'If suppression is largely criminal (arrest/incarceration for black market participation), the constraint inherits prohibition''s carceral machinery despite its regulatory framing — suppression metric understates continuity. If primarily administrative, the constraint genuinely shifts suppression modality. This affects whether the constraint is a clean break or a reformed continuation.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_mechanism_regulatory_vs_criminal, conceptual, 'Modality of suppression in the legalized regime: regulatory vs. criminal').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(substance_control_authority__legalization_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(scalr_tr_t0, substance_control_authority__legalization_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(scalr_tr_t5, substance_control_authority__legalization_reading, theater_ratio, 5, 0.2).
narrative_ontology:measurement(scalr_tr_t10, substance_control_authority__legalization_reading, theater_ratio, 10, 0.24).
narrative_ontology:measurement(scalr_tr_t15, substance_control_authority__legalization_reading, theater_ratio, 15, 0.27).
narrative_ontology:measurement(scalr_tr_t20, substance_control_authority__legalization_reading, theater_ratio, 20, 0.28).

% Extraction over time
narrative_ontology:measurement(scalr_be_t0, substance_control_authority__legalization_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(scalr_be_t5, substance_control_authority__legalization_reading, base_extractiveness, 5, 0.45).
narrative_ontology:measurement(scalr_be_t10, substance_control_authority__legalization_reading, base_extractiveness, 10, 0.47).
narrative_ontology:measurement(scalr_be_t15, substance_control_authority__legalization_reading, base_extractiveness, 15, 0.48).
narrative_ontology:measurement(scalr_be_t20, substance_control_authority__legalization_reading, base_extractiveness, 20, 0.48).

% Suppression requirement over time
narrative_ontology:measurement(scalr_su_t0, substance_control_authority__legalization_reading, suppression_requirement, 0, 0.65).
narrative_ontology:measurement(scalr_su_t5, substance_control_authority__legalization_reading, suppression_requirement, 5, 0.58).
narrative_ontology:measurement(scalr_su_t10, substance_control_authority__legalization_reading, suppression_requirement, 10, 0.53).
narrative_ontology:measurement(scalr_su_t15, substance_control_authority__legalization_reading, suppression_requirement, 15, 0.51).
narrative_ontology:measurement(scalr_su_t20, substance_control_authority__legalization_reading, suppression_requirement, 20, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(substance_control_authority__legalization_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(substance_control_authority__legalization_reading, 0.12).
narrative_ontology:affects_constraint(substance_control_authority__legalization_reading, substance_control_authority__prohibition_reading).
narrative_ontology:affects_constraint(substance_control_authority__legalization_reading, substance_control_authority__harm_reduction_reading).
narrative_ontology:affects_constraint(substance_control_authority__legalization_reading, public_health_surveillance_infrastructure).
narrative_ontology:affects_constraint(substance_control_authority__legalization_reading, tax_revenue_allocation_drug_markets).
narrative_ontology:affects_constraint(substance_control_authority__legalization_reading, licensed_cannabis_market_regulations).

% DUAL FORMULATION NOTE:
% This constraint (legalization_reading) and prohibition_reading are mutually exclusive instantiations of the substance_control_authority kernel — they foreclose each other within any single legal framework. The harm_reduction_reading coexists with both: a jurisdiction can have legal regulated markets AND harm reduction services (safe consumption sites, drug checking). The three readings form a constraint family linked by the kernel; each has distinct beneficiary/victim structures and classification.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(substance_control_authority__legalization_reading, institutional, 0.2).
constraint_indexing:directionality_override(substance_control_authority__legalization_reading, moderate, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
