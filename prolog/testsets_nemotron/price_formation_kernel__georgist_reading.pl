% ============================================================================
% CONSTRAINT STORY: price_formation_kernel__georgist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_price_formation_kernel__georgist_reading, []).

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
 *   constraint_id: price_formation_kernel__georgist_reading
 *   human_readable: Georgist Price Formation Reading — Land Rent Extraction from Improvement Production
 *   domain: political_economy/housing/institutional_analysis
 *
 * SUMMARY:
 *   The Georgist reading of price formation asserts a structural separation
 *   between land rent (unearned, arising from fixed location scarcity and
 *   community-generated value) and improvement value (earned, arising from
 *   labor and capital applied to land). The land component is a mountain —
 *   fixed supply is a physical fact — but the capture of its rent by private
 *   titleholders operates as a snare: landowners extract from wage-earners
 *   and productive enterprise without contributing to production. The
 *   improvement component is a rope — it coordinates production by rewarding
 *   labor and capital. The constraint is the price formation system that
 *   entangles these: market prices conflate the two, making rent extraction
 *   appear as return to capital. This reading is one of four instantiations
 *   of the price_formation_kernel.
 *
 * KEY AGENTS:
 *   - landowning_interests: Primary beneficiary (institutional/arbitrage) — captures location rent through private title
 *   - speculative_holders: Secondary beneficiary (powerful/constrained) — holds land for appreciation without improvement
 *   - rentier_capital: Beneficiary (institutional/arbitrage) — financializes land rent into income streams
 *   - wage_earners: Primary victim (organized/trapped) — pays rent from labor income, cannot exit location dependence
 *   - tenant_households: Primary victim (powerless/trapped) — bears full rent incidence with zero equity accumulation
 *   - productive_enterprises: Victim (powerful/constrained) — pays location rent as business cost, distorts investment
 *   - new_entrants: Victim (moderate/trapped) — faces capitalized rent barrier to housing and business location
 *   - georgist_advocates: Excluded (organized/constrained) — proposes land value tax to separate rent from improvement
 *   - mainstream_economists: Observer (analytical/analytical) — treats rent as factor return, not extraction
 *   - policy_makers: Agenda_setter (institutional/constrained) — maintains title system, resists LVT adoption
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(price_formation_kernel__georgist_reading, 0.68).
domain_priors:suppression_score(price_formation_kernel__georgist_reading, 0.42).
domain_priors:theater_ratio(price_formation_kernel__georgist_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(price_formation_kernel__georgist_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(price_formation_kernel__georgist_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(price_formation_kernel__georgist_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(price_formation_kernel__georgist_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(price_formation_kernel__georgist_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(price_formation_kernel__georgist_reading, tangled_rope).
narrative_ontology:human_readable(price_formation_kernel__georgist_reading, "Georgist Price Formation Reading — Land Rent Extraction from Improvement Production").
narrative_ontology:topic_domain(price_formation_kernel__georgist_reading, "political_economy/housing/institutional_analysis").

domain_priors:requires_active_enforcement(price_formation_kernel__georgist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(price_formation_kernel__georgist_reading, '2d859ac3-3377-4aa1-b56d-d568e06c4a08').
narrative_ontology:cs_kernel_codification('2d859ac3-3377-4aa1-b56d-d568e06c4a08', fixed_text).
narrative_ontology:cs_authority_grounding('2d859ac3-3377-4aa1-b56d-d568e06c4a08', lineage).
narrative_ontology:cs_interpretation_layer_present('2d859ac3-3377-4aa1-b56d-d568e06c4a08').
narrative_ontology:cs_reading_relation('2d859ac3-3377-4aa1-b56d-d568e06c4a08', price_formation_kernel__naturalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('2d859ac3-3377-4aa1-b56d-d568e06c4a08', price_formation_kernel__institutional_reading, influences).
narrative_ontology:cs_reading_relation('2d859ac3-3377-4aa1-b56d-d568e06c4a08', price_formation_kernel__financialization_reading, coexists_with).
narrative_ontology:cs_axiom('2d859ac3-3377-4aa1-b56d-d568e06c4a08', foundational, land_rent_is_unearned_income).
narrative_ontology:cs_axiom_status(land_rent_is_unearned_income, holdable).
narrative_ontology:cs_axiom_grounding('2d859ac3-3377-4aa1-b56d-d568e06c4a08', land_rent_is_unearned_income, deontological).
narrative_ontology:cs_axiom('2d859ac3-3377-4aa1-b56d-d568e06c4a08', foundational, improvement_value_is_earned_income).
narrative_ontology:cs_axiom_status(improvement_value_is_earned_income, holdable).
narrative_ontology:cs_axiom_grounding('2d859ac3-3377-4aa1-b56d-d568e06c4a08', improvement_value_is_earned_income, deontological).
narrative_ontology:cs_axiom('2d859ac3-3377-4aa1-b56d-d568e06c4a08', secondary, land_value_tax_captures_rent_without_distortion).
narrative_ontology:cs_axiom_status(land_value_tax_captures_rent_without_distortion, holdable).
narrative_ontology:cs_axiom_grounding('2d859ac3-3377-4aa1-b56d-d568e06c4a08', land_value_tax_captures_rent_without_distortion, empirically_contingent).
narrative_ontology:cs_reference_frame('2d859ac3-3377-4aa1-b56d-d568e06c4a08', classical_political_economy_rent_theory).
narrative_ontology:cs_drift_state('2d859ac3-3377-4aa1-b56d-d568e06c4a08', contemporary_financialized_housing, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('2d859ac3-3377-4aa1-b56d-d568e06c4a08', '').
narrative_ontology:cs_kernel_id(price_formation_kernel__georgist_reading, price_formation_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(price_formation_kernel__georgist_reading, landowning_interests).
narrative_ontology:constraint_beneficiary(price_formation_kernel__georgist_reading, speculative_holders).
narrative_ontology:constraint_beneficiary(price_formation_kernel__georgist_reading, rentier_capital).
narrative_ontology:constraint_victim(price_formation_kernel__georgist_reading, wage_earners).
narrative_ontology:constraint_victim(price_formation_kernel__georgist_reading, tenant_households).
narrative_ontology:constraint_victim(price_formation_kernel__georgist_reading, productive_enterprises).
narrative_ontology:constraint_victim(price_formation_kernel__georgist_reading, new_entrants).
narrative_ontology:constraint_vindicates(price_formation_kernel__georgist_reading, land_value_tax_justification).
narrative_ontology:constraint_vindicates(price_formation_kernel__georgist_reading, unearned_increment_capture).
narrative_ontology:constraint_vindicates(price_formation_kernel__georgist_reading, factor_separation_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold title to land in high-value locations. Collect ground rent from tenants and businesses without producing the location value — it arises from public infrastructure, agglomeration, and community. Can diversify across markets, use financial instruments to hedge, and influence policy to protect the title system. Exit is near-arbitrage: they can sell appreciated assets and reallocate globally.
narrative_ontology:constraint_stakeholder(price_formation_kernel__georgist_reading, landowning_interests, beneficiary,
    institutional, generational, arbitrage, global).

% Acquire and hold land for capital appreciation, often leaving it vacant or underimproved. Benefit from the same rent capture as landowning_interests but with shorter time horizons and higher leverage. Exit is constrained by market cycles and carrying costs — they cannot instantly liquidate without loss, but have more mobility than wage-earners.
narrative_ontology:constraint_stakeholder(price_formation_kernel__georgist_reading, speculative_holders, beneficiary,
    powerful, biographical, constrained, national).

% Financial intermediaries (REITs, pension funds, sovereign wealth) that securitize land rent into financial products. They capture the rent stream at scale, lobby for favorable tax treatment (e.g., REIT pass-through), and diversify globally. Exit is arbitrage-grade: they can reallocate across asset classes and jurisdictions instantly.
narrative_ontology:constraint_stakeholder(price_formation_kernel__georgist_reading, rentier_capital, beneficiary,
    institutional, generational, arbitrage, global).

% Pay 30-50% of income on housing in high-rent cities. The rent component is a pure transfer to landowners — they receive no improvement service for it. Labor mobility is constrained by job networks, family ties, and the same rent barrier in destination cities. Union organization provides some political voice but not exit from the rent relation.
narrative_ontology:constraint_stakeholder(price_formation_kernel__georgist_reading, wage_earners, payer,
    organized, biographical, trapped, regional).

% Bear the full incidence of land rent with zero equity accumulation. Face displacement risk from rent increases. Exit options are near-zero: moving costs, deposit barriers, and the universality of rent in all rental markets trap them. No meaningful collective organization in most jurisdictions.
narrative_ontology:constraint_stakeholder(price_formation_kernel__georgist_reading, tenant_households, payer,
    powerless, immediate, trapped, local).

% Pay location rent as a business cost (commercial lease or owned-premises opportunity cost). This distorts investment: firms underinvest in high-rent areas or relocate to lower-rent peripheries, reducing agglomeration benefits. Exit is constrained by workforce, supply chains, and market access — they cannot simply move to where rent is zero.
narrative_ontology:constraint_stakeholder(price_formation_kernel__georgist_reading, productive_enterprises, payer,
    powerful, biographical, constrained, national).

% First-time buyers and new businesses face capitalized rent barriers: down payments reflect future rent streams. They pay for location value created by prior generations. Exit is trapped — the only 'exit' is accepting the rent burden or leaving the market entirely (delaying household formation, foregoing business entry).
narrative_ontology:constraint_stakeholder(price_formation_kernel__georgist_reading, new_entrants, payer,
    moderate, biographical, trapped, regional).

% Advocate for land value tax to capture rent for public revenue and untax improvements. Structurally excluded from mainstream policy discourse — their proposal threatens the primary beneficiary groups. They have intellectual organization but no institutional power. Exit from exclusion requires policy window (crisis, reform moment) which is constrained and rare.
narrative_ontology:constraint_stakeholder(price_formation_kernel__georgist_reading, georgist_advocates, excluded,
    organized, generational, constrained, global).

% Treat land rent as a factor return equivalent to wages and interest. The dominant neoclassical framework merges land into 'capital,' obscuring the unearned/earned distinction. Their analytical frame naturalizes the rent capture. Exit is analytical: they can adopt the Georgist frame intellectually but the professional incentives and textbook canon constrain the shift.
narrative_ontology:constraint_stakeholder(price_formation_kernel__georgist_reading, mainstream_economists, observer,
    analytical, civilizational, analytical, universal).

% Administer the property title system, zoning, and tax code that sustain the rent capture. Face electoral pressure from both beneficiaries (campaign finance, lobbying) and victims (housing affordability anger). Constrained exit: they cannot abolish private land title without constitutional crisis, but can adopt LVT at margins (split-rate, exemptions). Their agenda-setting power maintains the hybrid constraint.
narrative_ontology:constraint_stakeholder(price_formation_kernel__georgist_reading, policy_makers, agenda_setter,
    institutional, generational, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The improvement component coordinates production by rewarding labor and capital applied to land — builders build, firms invest, households improve — because they capture the value they create. The land component does NOT coordinate: fixed supply cannot be elicited by price. The hybrid system partially coordinates (improvements) while extracting (land rent).
% TRANSFER_FUNCTION: Moves location rent (community-generated land value) from wage-earners, tenants, productive enterprises, and new entrants to landowning_interests, speculative_holders, and rentier_capital via the price system. The transfer is embedded in every rent payment and purchase price — the land share is capitalized rent.
% ABSENT_VOICES: Future generations who will inherit the rent-capitalized asset structure; displaced communities who have already exited high-rent areas (their absence is the suppression evidence); informal economy workers whose housing is entirely rent-burdened with no policy representation. Geogist_advocates are the excluded stakeholders who would object if present.
% DISAPPEARANCE_RATIONALE: If the land-rent-capture component vanished (e.g., via full LVT), land prices would collapse to near-zero, improvement values would remain, housing costs would fall dramatically, speculative vacancy would end, and productive investment would reallocate to highest-use locations. The world would rearrange: landowners lose rent income; wage-earners and tenants gain disposable income; productive enterprises face lower location costs. The improvement coordination (rope) would persist and strengthen.
% FOUNDING_PROBLEM: Early political economy (Smith, Ricardo, Mill) identified land rent as unearned and proposed taxing it to fund public revenue without distorting production. The Georgist reading (Henry George, Progress and Poverty, 1879) built a mass movement around this: the founding problem was 'how to secure the producer's full product while socializing the unearned increment.' Private land title was the compromise — secure tenure for improvement, but rent capture persisted.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem (secure tenure for improvement) is corroborated as dead by: (1) empirical vacancy and underuse rates in high-rent cities — title no longer allocates to productive use; (2) the rise of financialized land holding (REITs, corporate landlords) where tenure security serves rent extraction, not improvement; (3) even mainstream urban economists (Glaeser, Gyourko) acknowledge 'zoning tax' and 'superstar city' rent capture as distinct from improvement returns. No non-beneficiary source corroborates that the founding problem is live — only landowner lobbyists claim 'property rights' require rent capture.
narrative_ontology:disappearance_verdict(price_formation_kernel__georgist_reading, world_rearranges).
narrative_ontology:founding_problem_status(price_formation_kernel__georgist_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(price_formation_kernel__georgist_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(price_formation_kernel__georgist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(price_formation_kernel__georgist_reading, 0.68, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(price_formation_kernel__georgist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(price_formation_kernel__georgist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(price_formation_kernel__georgist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness (0.68) reflects the land component's dominance in high-value urban markets — rent share of housing cost often exceeds 50% in supply-constrained cities. The improvement component's low ε (~0.15) is averaged in, yielding the composite. Suppression (0.42) is moderate: structural barriers to LVT (vested interest lobbying, constitutional property protections) are real but not total — some jurisdictions have adopted split-rate taxation. Theater ratio (0.28) reflects the performative defense of 'property rights' that masks rent extraction. Accessibility collapse (0.65) is high for victims: exit from land rent requires geographic mobility that wage-earners lack. Resistance (0.58) is significant: tenant movements, Georgist advocacy, and some policy adoption show active contestation. The claimed_type tangled_rope captures the hybrid: land component = mountain (fixed supply) + snare (rent capture); improvement component = rope (production coordination). The engine will compute per-seat types from the structural data.
 *
 * PERSPECTIVAL GAP:
 *   From landowning_interests and rentier_capital seats, the constraint appears as mountain+rope: land title is 'natural property,' improvements are rewarded. From wage_earners and tenant_households, it appears as snare: they pay for location value they created through community but cannot capture. From productive_enterprises, it appears as tangled_rope: they coordinate production (rope) but pay extractive location rent (snare). The engine computes this divergence from power/exit/role data.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (landowning_interests, speculative_holders, rentier_capital) collect rent without producing location value — d near 0.0 (beneficiary end). Victims (wage_earners, tenant_households, productive_enterprises, new_entrants) pay rent from earned income with constrained exit — d near 1.0 (target end). Geogist_advocates are excluded from the policy conversation despite structural relevance — d not computed (excluded role). Policy_makers as agenda_setters have d ~0.3: they administer the title system but face electoral pressure. Mainstream_economists as observers have d=0.5 (analytical). The land component's mountain nature (fixed supply) creates the extraction floor; the title system converts it into private capture.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (efficient land allocation via private title) is dead — title no longer allocates land to highest productive use but to highest rent-capture capacity. The arrangement persists as zombie coordination: the rope (improvement incentive) is real but the snare (rent capture) has hypertrophied. Mandatrophy is unresolved: the mandate (secure tenure for improvement) has been captured by the extraction (rent capitalization). The constraint prevents mislabeling: without the Georgist reading, the snare component is invisible — rent looks like 'return to capital.' The reading restores the coordination/extraction distinction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is the Georgist reading of price formation a distinct constraint from the naturalist, institutional, and financialization readings, or a measurement frame on a single phenomenon?',
    'Per ε-invariance: if the land component''s ε differs from the improvement component''s ε by >0.25, they are structurally distinct constraints. This reading declares the land component as mountain+snare hybrid (ε≈0.75) and improvement as rope (ε≈0.15). A single ε would average to ~0.45 — the gap validates decomposition.',
    'If decomposition fails, the kernel collapses to one constraint with observer-dependent ε — the engine would flag the violation. The reading structure assumes successful decomposition.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Validates ε-invariance decomposition of the price_formation_kernel').

omega_variable(
    land_rent_extraction_mechanism,
    'Is land rent capture structurally extractive (snare) or coordination (rope) when landowners hold location monopolies without producing improvements?',
    'Empirical: measure rent share of housing cost in supply-constrained markets. If rent share >40% and correlates with landowner income without improvement investment, extraction is structurally demonstrated. Policy counterfactual: land value tax adoption — if extraction falls without supply collapse, the snare classification holds.',
    'If extraction is structural, the land component is a snare riding on a mountain (fixed supply). If coordination, the land component is a rope on a mountain. The Georgist reading claims the former.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(land_rent_extraction_mechanism, empirical, 'Whether location monopoly rent is coordination or extraction').

omega_variable(
    improvement_coordination_purity,
    'Does the improvement component operate as pure coordination (rope) or does it carry extractive coupling to the land component?',
    'Measure whether improvement investment decisions are distorted by land rent capture (e.g., underbuilding to reduce tax base, speculative vacancy). If improvement allocation tracks productivity signals without land-rent distortion, rope holds. If land rent signals dominate, the rope is contaminated.',
    'If contaminated, the improvement component is tangled_rope, not rope — the whole constraint collapses to tangled_rope from every seat. This reading claims rope purity for improvements.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(improvement_coordination_purity, empirical, 'Whether production coordination is distorted by land rent extraction').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression of land-value-tax alternatives structural (vested interest capture of policy) or internalized (ideological normalization of rent as property right)?',
    'Post-reform suppression trajectory: in jurisdictions that adopted LVT (e.g., Pennsylvania split-rate, Danish land tax), measure whether ideological resistance persists after structural barriers are removed. If suppression persists, internalized component is significant.',
    'If internalized, effective suppression > structural measure — targets carry the suppression with them. This reading claims structural suppression dominates but acknowledges internalized residue.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression of land-value-tax alternatives').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(price_formation_kernel__georgist_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pric_tr_t0, price_formation_kernel__georgist_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(pric_tr_t25, price_formation_kernel__georgist_reading, theater_ratio, 25, 0.18).
narrative_ontology:measurement(pric_tr_t50, price_formation_kernel__georgist_reading, theater_ratio, 50, 0.22).
narrative_ontology:measurement(pric_tr_t75, price_formation_kernel__georgist_reading, theater_ratio, 75, 0.25).
narrative_ontology:measurement(pric_tr_t100, price_formation_kernel__georgist_reading, theater_ratio, 100, 0.28).

% Extraction over time
narrative_ontology:measurement(pric_be_t0, price_formation_kernel__georgist_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(pric_be_t25, price_formation_kernel__georgist_reading, base_extractiveness, 25, 0.52).
narrative_ontology:measurement(pric_be_t50, price_formation_kernel__georgist_reading, base_extractiveness, 50, 0.58).
narrative_ontology:measurement(pric_be_t75, price_formation_kernel__georgist_reading, base_extractiveness, 75, 0.62).
narrative_ontology:measurement(pric_be_t100, price_formation_kernel__georgist_reading, base_extractiveness, 100, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(pric_su_t0, price_formation_kernel__georgist_reading, suppression_requirement, 0, 0.28).
narrative_ontology:measurement(pric_su_t25, price_formation_kernel__georgist_reading, suppression_requirement, 25, 0.32).
narrative_ontology:measurement(pric_su_t50, price_formation_kernel__georgist_reading, suppression_requirement, 50, 0.38).
narrative_ontology:measurement(pric_su_t75, price_formation_kernel__georgist_reading, suppression_requirement, 75, 0.41).
narrative_ontology:measurement(pric_su_t100, price_formation_kernel__georgist_reading, suppression_requirement, 100, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(price_formation_kernel__georgist_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(price_formation_kernel__georgist_reading, 0.18).
narrative_ontology:affects_constraint(price_formation_kernel__georgist_reading, price_formation_kernel__naturalist_reading).
narrative_ontology:affects_constraint(price_formation_kernel__georgist_reading, price_formation_kernel__institutional_reading).
narrative_ontology:affects_constraint(price_formation_kernel__georgist_reading, price_formation_kernel__financialization_reading).
narrative_ontology:affects_constraint(price_formation_kernel__georgist_reading, land_value_tax_implementation).
narrative_ontology:affects_constraint(price_formation_kernel__georgist_reading, zoning_regulatory_capture).
narrative_ontology:affects_constraint(price_formation_kernel__georgist_reading, housing_financialization_feedback).

% DUAL FORMULATION NOTE:
% This constraint decomposes the price_formation_kernel into land (mountain+snare) and improvement (rope) components. The naturalist_reading treats the kernel as mountain (ε≈0.1). The institutional_reading treats it as scaffold/tangled_rope (ε≈0.4). The financialization_reading treats it as snare (ε≈0.75). This reading is the only one that structurally separates the components. All four readings form a constraint family linked by affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(price_formation_kernel__georgist_reading, institutional, 0.15).
constraint_indexing:directionality_override(price_formation_kernel__georgist_reading, powerful, 0.85).
constraint_indexing:directionality_override(price_formation_kernel__georgist_reading, organized, 0.75).
constraint_indexing:directionality_override(price_formation_kernel__georgist_reading, moderate, 0.8).
constraint_indexing:directionality_override(price_formation_kernel__georgist_reading, powerless, 0.95).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
