% ============================================================================
% CONSTRAINT STORY: federation_membership_treaty__integration_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_federation_membership_treaty__integration_primary, []).

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
 *   constraint_id: federation_membership_treaty__integration_primary
 *   human_readable: Free Movement Constitutive Principle (Integration-Primary Reading)
 *   domain: political_economy/federalism/migration_policy
 *
 * SUMMARY:
 *   This constraint instantiates the integration-primary reading of the
 *   federation-membership treaty's free-movement kernel. Under this reading,
 *   the ability of workers, service providers, and capital to move across
 *   member state boundaries without discrimination is treated as constitutive
 *   of the single market itself — a foundational commitment that cannot be
 *   legitimately restricted unless the restriction meets a narrow
 *   justification test. The reading suppresses member state authority to
 *   implement labor-market protections, welfare-system boundaries, or
 *   immigration restrictions, treating such measures as presumptive
 *   violations of integration's core principle. Mobile workers and
 *   multinational firms benefit from the unrestricted access; local labor
 *   markets, welfare systems, and domestic workers competing in
 *   high-migration sectors bear the costs. The extraction is substantial and
 *   active — it requires continuous enforcement of the
 *   presumption-against-restriction through court decisions, commission
 *   rulings, and member state compliance machinery.
 *
 * KEY AGENTS:
 *   - Mobile workers: beneficiaries of unrestricted labor-market access; exit from mobility is rendered normatively illegitimate
 *   - Multinational firms: beneficiaries of labor arbitrage and cross-border hiring freedom
 *   - Local labor markets & domestic workers: victims of wage pressure from unrestricted competition
 *   - National welfare systems: victims of fiscal costs from residence-based access and unrestricted migration
 *   - Member state governments: agenda-setters forced to enforce the principle while bearing electoral costs from harmed constituencies
 *   - Federation integration authority: authoritative interpreter suppressing state-level restrictions
 *   - Sovereignty-primary advocates: excluded from authoritative voice; treated as foreclosed by federation membership
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(federation_membership_treaty__integration_primary, 0.68).
domain_priors:suppression_score(federation_membership_treaty__integration_primary, 0.79).
domain_priors:theater_ratio(federation_membership_treaty__integration_primary, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(federation_membership_treaty__integration_primary, extractiveness, 0.68).
narrative_ontology:constraint_metric(federation_membership_treaty__integration_primary, suppression_requirement, 0.79).
narrative_ontology:constraint_metric(federation_membership_treaty__integration_primary, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(federation_membership_treaty__integration_primary, accessibility_collapse, 0.71).
narrative_ontology:constraint_metric(federation_membership_treaty__integration_primary, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(federation_membership_treaty__integration_primary, tangled_rope).
narrative_ontology:human_readable(federation_membership_treaty__integration_primary, "Free Movement Constitutive Principle (Integration-Primary Reading)").
narrative_ontology:topic_domain(federation_membership_treaty__integration_primary, "political_economy/federalism/migration_policy").

domain_priors:requires_active_enforcement(federation_membership_treaty__integration_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(federation_membership_treaty__integration_primary, 'de8a08d7-9bca-4918-b762-a0bfb277d14d').
narrative_ontology:cs_kernel_codification('de8a08d7-9bca-4918-b762-a0bfb277d14d', formalized).
narrative_ontology:cs_authority_grounding('de8a08d7-9bca-4918-b762-a0bfb277d14d', lineage).
narrative_ontology:cs_interpretation_layer_present('de8a08d7-9bca-4918-b762-a0bfb277d14d').
narrative_ontology:cs_reading_relation('de8a08d7-9bca-4918-b762-a0bfb277d14d', federation_membership_treaty__sovereignty_primary, forecloses).
narrative_ontology:cs_reading_relation('de8a08d7-9bca-4918-b762-a0bfb277d14d', federation_membership_treaty__subsidiarity_balance, influences).
narrative_ontology:cs_axiom('de8a08d7-9bca-4918-b762-a0bfb277d14d', foundational, free_movement_constitutive_integration).
narrative_ontology:cs_axiom_status(free_movement_constitutive_integration, holdable).
narrative_ontology:cs_axiom_grounding('de8a08d7-9bca-4918-b762-a0bfb277d14d', free_movement_constitutive_integration, conventional).
narrative_ontology:cs_axiom('de8a08d7-9bca-4918-b762-a0bfb277d14d', foundational, restrictions_presumptively_illegitimate).
narrative_ontology:cs_axiom_status(restrictions_presumptively_illegitimate, holdable).
narrative_ontology:cs_axiom_grounding('de8a08d7-9bca-4918-b762-a0bfb277d14d', restrictions_presumptively_illegitimate, deontological).
narrative_ontology:cs_reference_frame('de8a08d7-9bca-4918-b762-a0bfb277d14d', free_movement_as_federation_core).
narrative_ontology:cs_drift_state('de8a08d7-9bca-4918-b762-a0bfb277d14d', contemporary_welfare_resistance_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('de8a08d7-9bca-4918-b762-a0bfb277d14d', '').
narrative_ontology:cs_kernel_id(federation_membership_treaty__integration_primary, federation_membership_treaty).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(federation_membership_treaty__integration_primary, mobile_workers).
narrative_ontology:constraint_beneficiary(federation_membership_treaty__integration_primary, multinational_firms).
narrative_ontology:constraint_beneficiary(federation_membership_treaty__integration_primary, cross_border_service_providers).
narrative_ontology:constraint_victim(federation_membership_treaty__integration_primary, local_labor_markets).
narrative_ontology:constraint_victim(federation_membership_treaty__integration_primary, national_welfare_systems).
narrative_ontology:constraint_victim(federation_membership_treaty__integration_primary, domestic_workers_competing_with_migration).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(federation_membership_treaty__integration_primary, member_state_governments).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Gain unrestricted access to labor markets across all member states. The constitutional principle treats their mobility as foundational to the integration project itself. They can relocate for employment without discrimination based on nationality. Exit for them means remaining in origin state; the constraint suppresses that exit by making it normatively inferior to mobility.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__integration_primary, mobile_workers, beneficiary,
    moderate, biographical, mobile, global).

% Operate across member states without visa-based labor restrictions, source workers from the entire federation, and avoid localized labor cost constraints. The principle enables labor arbitrage and prevents member states from ring-fencing jobs for citizens. They benefit from the suppression of national restrictions on hiring.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__integration_primary, multinational_firms, beneficiary,
    powerful, generational, arbitrage, global).

% Provide services (consulting, contracting, professional services) across borders without nationality-based licensing barriers. The principle treats national professional regulation as presumptively illegitimate unless narrowly justified, enabling them to compete directly with domestic professionals.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__integration_primary, cross_border_service_providers, beneficiary,
    powerful, biographical, arbitrage, global).

% Experience wage pressure and employment competition from unrestricted intra-federation mobility. They cannot implement job-preference policies for citizens or regulate labor supply based on local conditions. Member states are prohibited from protecting local labor markets through mobility restrictions, even during downturns or skills-specific unemployment.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__integration_primary, local_labor_markets, payer,
    organized, biographical, constrained, national).

% Bear fiscal costs as mobile workers and their families access healthcare, education, child benefits, and unemployment insurance based on residence rather than contribution history. The principle treats residence-based access as constitutive of integration; restrictions on welfare eligibility are presumptively illegitimate. States cannot limit access based on recent migration or citizenship status.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__integration_primary, national_welfare_systems, payer,
    institutional, generational, constrained, national).

% Compete directly with mobile workers from lower-wage member states for jobs, facing wage compression and reduced bargaining power in certain sectors. They cannot appeal to state-level labor-market protections or immigration restrictions because the principle treats such protections as violations of integration's foundational commitment. Their exit is remaining in the labor market at compressed wages or relocating themselves.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__integration_primary, domestic_workers_competing_with_migration, payer,
    powerless, biographical, trapped, local).

% Signed the treaty committing to free movement but bear the enforcement burden of suppressing national restrictions. They are caught between the constitutional principle (integration-primary) that treats free movement as non-negotiable, and electoral pressure from domestic workers and welfare constituencies harmed by unrestricted mobility. They administer the constraint but lack legitimate authority to revise it under the integration-primary reading.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__integration_primary, member_state_governments, payer,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(federation_membership_treaty__integration_primary, member_state_governments, agenda_setter).

% Interprets and enforces the free movement principle at the federation level. Courts, commissions, and dispute bodies clarify that restrictions are presumptively illegitimate and require narrow justification. They actively suppress member state attempts to implement local-preference policies, welfare conditions on migration, or labor-market protections that would reduce mobile worker access.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__integration_primary, federation_integration_authority, agenda_setter,
    institutional, generational, analytical, global).

% Would argue that member states retain authority to protect national labor markets and welfare systems; they would reframe free movement as conditional on state consent and proportionality rather than constitutive. They are excluded from authoritative voice under the integration-primary reading because that reading treats their premises as foreclosed by federation membership itself.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__integration_primary, sovereignty_primary_advocates, excluded,
    powerful, biographical, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(federation_membership_treaty__integration_primary, multinational_firms).
narrative_ontology:fixing_cost_class(federation_membership_treaty__integration_primary, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Enables labor mobility, capital flows, and service provision across member state boundaries without discrimination. Solves the coordination problem of allowing economic actors to find the most efficient allocation of labor and capital across a larger market than any single state could offer.
% TRANSFER_FUNCTION: Moves labor-cost arbitrage gains from domestic workers and welfare constituencies (as wage pressure and fiscal burden) toward mobile workers, multinational firms, and service providers. Transfers authority over labor-market policy from member states to the federation level, privileging mobility-maximizing rules over state-level protections.
% ABSENT_VOICES: Domestic workers in high-competition sectors, local labor unions, regional welfare administrators, and national governments seeking to implement cyclical labor-market policies are systematically excluded from authoritative voice under the integration-primary reading. Their concerns are treated as presumptively illegitimate unless they meet the narrow-justification bar — which the principle sets very high.
% DISAPPEARANCE_RATIONALE: If the constitutive free-movement principle disappeared, member states would immediately implement job-preference policies, welfare conditions on migration, and labor-market protections. Wage dynamics in lower-income sectors would shift as labor supply tightens. Welfare systems would screen migrants more aggressively. The federation's economic integration would contract toward country-level decision-making on labor mobility.
% FOUNDING_PROBLEM: Post-war European integration required breaking the zero-sum nationalist competition over labor and capital that fueled protectionism and conflict. Free movement as a constitutive principle was meant to make member state borders economically irrelevant and foreclose the return to protectionist labor markets.
% FOUNDING_PROBLEM_CORROBORATION: Federation integration authorities and legal scholars aligned with the integration-primary reading affirm the founding problem remains live: without constitutive free movement, states would revert to nationalist labor-market protection. Member state governments, labor advocates, and subsidiarity-balance theorists dispute this: they argue the founding problem (nationalist protectionism) is substantially resolved and the principle persists as a vehicle for wealth extraction from lower-wage constituencies and welfare systems, no longer as a guard against nationalist reversion.
narrative_ontology:disappearance_verdict(federation_membership_treaty__integration_primary, world_rearranges).
narrative_ontology:founding_problem_status(federation_membership_treaty__integration_primary, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(federation_membership_treaty__integration_primary, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(federation_membership_treaty__integration_primary, 'none', 1).
narrative_ontology:epsilon_provenance(federation_membership_treaty__integration_primary, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(federation_membership_treaty__integration_primary_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(federation_membership_treaty__integration_primary, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(federation_membership_treaty__integration_primary_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness measures 0.68 because the principle's operation systematically transfers labor-cost advantages and migration-arbitrage gains from domestic constituencies toward mobile workers and multinational firms. The measurement series shows extraction rising from 0.48 to 0.68 over the interval, reflecting the progressive tightening of the presumption-against-restriction as case law and commission decisions narrow the permitted exceptions. Suppression measures 0.79 because the constraint's persistence depends entirely on actively suppressing member state attempts to implement national labor-market and welfare protections — the principle has no self-enforcement; it requires continuous legal enforcement against state resistance. Theater ratio measures 0.42, moderate-to-low, because the coordination function (unified labor market enabling capital efficiency) is genuine, but an increasing share of the suppression machinery is deployed to defend mobility gains rather than optimize market efficiency. The measurement grid is shared across all metrics at every time point, reflecting the constraint's temporal evolution as integration deepens and resistance from affected constituencies grows.
 *
 * PERSPECTIVAL GAP:
 *   This reading should compute as tangled_rope from every seat except the beneficiary seats. From the mobile-worker and firm seats, the constraint is coordination — the principle solves the problem of fragmented labor markets. From the member-state and domestic-worker seats, the same principle is enforced extraction, because their attempts to solve local coordination problems (protecting workers, managing welfare costs) are presumptively illegitimate. The engine computes per-seat types from the structural data: beneficiary seats see coordination at near-zero extraction; victim seats see high extraction with active suppression; agenda-setter seats split between real coordination costs and enforcement burden. The authored claimed_type (tangled_rope) reflects the integration-primary reading's own position: it claims to solve coordination (the single market) but the measurement profile and stakeholder structure show substantial asymmetric extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   Mobile workers and firms occupy the beneficiary end of the directionality spectrum (d near 0.0) because they collect the mobility gains, face minimal constraints, and have arbitrage-grade exit (they can relocate within the federation or globally if the principle were revoked). Member states and domestic workers occupy the target end (d near 1.0) because they bear wage pressure and welfare costs while lacking legitimate authority to revise the constraint; their exit is either costly relocation or accepting compressed living standards. The federation integration authority sits at d ≈ 0.5 (symmetric): they are the agenda-setters (administrative power) but they also bear the legitimacy costs of enforcing the principle against rising resistance, and they cannot unilaterally revoke it. Directionality derives from beneficiary/victim declarations plus exit-option asymmetry: mobile workers have the highest exit options (arbitrage), member states have the lowest (institutional lock-in to the treaty).
 *
 * MANDATROPHY ANALYSIS:
 *   The integration-primary reading faces a classic mandatrophy question: the founding problem was nationalist protectionism and labor-market closure in the post-war period. The principle was meant to prevent member states from fragmenting into zero-sum labor competition. By the interval's end (2026), several indicators suggest the founding mandate has shifted: (1) explicit nationalist reversion is rare; (2) no member state has credibly threatened federation exit over labor mobility; (3) resistance now comes primarily from welfare constituencies and domestic workers who accept federation membership but dispute the integration-primary reading's treatment of free movement as non-negotiable. The constraint persists not because the founding problem (nationalist reversion) is live, but because the principle has become the primary vehicle for labor-cost arbitrage and wealth extraction. This is the Tangled Rope marker: genuine coordination (the single market) is still solved, but the distribution of gains is asymmetric and the principle's persistence depends on suppressing state-level alternatives, not on preventing nationalist collapse. The theater_ratio's slow rise (0.28 to 0.42) reflects this shift: initially the principle's enforcement was largely functional (detecting and removing genuine protectionist barriers); by the interval's end, increasing enforcement effort is dedicated to litigating the boundaries of 'narrow justification' — defending mobility gains rather than maintaining market efficiency.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    founding_problem_persistence,
    'Is the founding problem (nationalist protectionism and labor-market closure) genuinely live, or has it been substantially resolved and the principle now functions primarily as an extraction mechanism?',
    'Survey of member state governments'' actual labor-market policy preferences absent the integration-primary constraint; analysis of whether suppressed policies would constitute nationalist closure or proportional local coordination. Historical comparison to nationalist protectionism in the pre-federation period.',
    'If the founding problem is dead, the constraint''s classification shifts from Tangled Rope (coordination + extraction) toward Snare (extraction with coordination cover). If it remains live, the extraction is the price of the coordination benefit and Tangled Rope is correct.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(founding_problem_persistence, empirical, 'Whether the principle''s mandate persists or has become theatrical.').

omega_variable(
    labor_market_competition_causality,
    'To what degree does the measured wage pressure and employment competition in local labor markets causally derive from free movement under this principle, versus other structural factors (automation, global competition, capital mobility independent of worker mobility)?',
    'Econometric decomposition of wage trends; comparison of sectors and regions with different migration rates; counterfactual modeling of labor outcomes under alternative mobility regimes.',
    'If free movement causally accounts for a large share of wage compression, the extraction measure is justified. If other factors dominate, the extractiveness measure overstates the constraint''s causal role and should be downward-adjusted.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(labor_market_competition_causality, empirical, 'Causal attribution of labor-market harm to the free-movement principle.').

omega_variable(
    welfare_access_vs_incentive,
    'Does residence-based welfare access under the principle create a genuine fiscal burden on host member states, or do migration-driven labor gains and economic growth offset the access costs?',
    'Fiscal accounting of welfare expenditure on migrants by category, compared to tax contributions and GDP growth effects; analysis of whether net fiscal impact is negative or positive by member state.',
    'If welfare access produces net fiscal loss, the victim classification of national welfare systems is justified. If net fiscal impact is positive, welfare systems may be beneficiaries rather than victims, shifting the beneficiary/victim structure and reducing measured extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(welfare_access_vs_incentive, empirical, 'Whether the measured welfare-system burden is real or offset by gains.').

omega_variable(
    narrow_justification_boundary,
    'What counts as a ''narrow justification'' for restrictions under the integration-primary reading, and is the boundary stable or drifting toward foreclosing legitimate state protections?',
    'Content analysis of federation court and commission decisions over the interval; mapping of accepted vs. rejected justifications; assessment of whether the boundary has shifted to make previously-accepted restrictions illegitimate.',
    'If the boundary is drifting to foreclose legitimate state interests, the principle''s extractiveness may be increasing as state authority erodes. If the boundary is stable, the extraction plateau (0.68 from time-point 25 onward) is justified.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(narrow_justification_boundary, empirical, 'Evolution of the narrow-justification doctrine.').

omega_variable(
    kernel_framing_dependency,
    'Does the integration-primary reading''s classification depend on how we frame the kernel''s boundaries, or is the classification robust to alternative framings?',
    'Comparison to readings that treat the kernel as ''non-discrimination'' (broader, allowing state-level protections if applied equally) vs. ''unfettered movement'' (narrower, requiring absolute freedom). Re-analyze the constraint under each framing.',
    'If classification shifts substantially with framing, the ε-invariance principle may require decomposing into separate constraint stories per framing. If robust, the single story is justified.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_framing_dependency, conceptual, 'Framing under-determination of the integration-primary reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(federation_membership_treaty__integration_primary, 0, 35).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fede_tr_t0, federation_membership_treaty__integration_primary, theater_ratio, 0, 0.28).
narrative_ontology:measurement_basis(fede_tr_t0, observed).
narrative_ontology:measurement(fede_tr_t5, federation_membership_treaty__integration_primary, theater_ratio, 5, 0.31).
narrative_ontology:measurement_basis(fede_tr_t5, observed).
narrative_ontology:measurement(fede_tr_t10, federation_membership_treaty__integration_primary, theater_ratio, 10, 0.35).
narrative_ontology:measurement_basis(fede_tr_t10, observed).
narrative_ontology:measurement(fede_tr_t15, federation_membership_treaty__integration_primary, theater_ratio, 15, 0.38).
narrative_ontology:measurement_basis(fede_tr_t15, observed).
narrative_ontology:measurement(fede_tr_t20, federation_membership_treaty__integration_primary, theater_ratio, 20, 0.4).
narrative_ontology:measurement_basis(fede_tr_t20, observed).
narrative_ontology:measurement(fede_tr_t25, federation_membership_treaty__integration_primary, theater_ratio, 25, 0.41).
narrative_ontology:measurement_basis(fede_tr_t25, observed).
narrative_ontology:measurement(fede_tr_t30, federation_membership_treaty__integration_primary, theater_ratio, 30, 0.42).
narrative_ontology:measurement_basis(fede_tr_t30, observed).
narrative_ontology:measurement(fede_tr_t35, federation_membership_treaty__integration_primary, theater_ratio, 35, 0.42).
narrative_ontology:measurement_basis(fede_tr_t35, observed).

% Extraction over time
narrative_ontology:measurement(fede_be_t0, federation_membership_treaty__integration_primary, base_extractiveness, 0, 0.48).
narrative_ontology:measurement_basis(fede_be_t0, observed).
narrative_ontology:measurement(fede_be_t5, federation_membership_treaty__integration_primary, base_extractiveness, 5, 0.54).
narrative_ontology:measurement_basis(fede_be_t5, observed).
narrative_ontology:measurement(fede_be_t10, federation_membership_treaty__integration_primary, base_extractiveness, 10, 0.59).
narrative_ontology:measurement_basis(fede_be_t10, observed).
narrative_ontology:measurement(fede_be_t15, federation_membership_treaty__integration_primary, base_extractiveness, 15, 0.63).
narrative_ontology:measurement_basis(fede_be_t15, observed).
narrative_ontology:measurement(fede_be_t20, federation_membership_treaty__integration_primary, base_extractiveness, 20, 0.66).
narrative_ontology:measurement_basis(fede_be_t20, observed).
narrative_ontology:measurement(fede_be_t25, federation_membership_treaty__integration_primary, base_extractiveness, 25, 0.68).
narrative_ontology:measurement_basis(fede_be_t25, observed).
narrative_ontology:measurement(fede_be_t30, federation_membership_treaty__integration_primary, base_extractiveness, 30, 0.68).
narrative_ontology:measurement_basis(fede_be_t30, observed).
narrative_ontology:measurement(fede_be_t35, federation_membership_treaty__integration_primary, base_extractiveness, 35, 0.68).
narrative_ontology:measurement_basis(fede_be_t35, observed).

% Suppression requirement over time
narrative_ontology:measurement(fede_su_t0, federation_membership_treaty__integration_primary, suppression_requirement, 0, 0.68).
narrative_ontology:measurement_basis(fede_su_t0, observed).
narrative_ontology:measurement(fede_su_t5, federation_membership_treaty__integration_primary, suppression_requirement, 5, 0.71).
narrative_ontology:measurement_basis(fede_su_t5, observed).
narrative_ontology:measurement(fede_su_t10, federation_membership_treaty__integration_primary, suppression_requirement, 10, 0.74).
narrative_ontology:measurement_basis(fede_su_t10, observed).
narrative_ontology:measurement(fede_su_t15, federation_membership_treaty__integration_primary, suppression_requirement, 15, 0.76).
narrative_ontology:measurement_basis(fede_su_t15, observed).
narrative_ontology:measurement(fede_su_t20, federation_membership_treaty__integration_primary, suppression_requirement, 20, 0.77).
narrative_ontology:measurement_basis(fede_su_t20, observed).
narrative_ontology:measurement(fede_su_t25, federation_membership_treaty__integration_primary, suppression_requirement, 25, 0.78).
narrative_ontology:measurement_basis(fede_su_t25, observed).
narrative_ontology:measurement(fede_su_t30, federation_membership_treaty__integration_primary, suppression_requirement, 30, 0.79).
narrative_ontology:measurement_basis(fede_su_t30, observed).
narrative_ontology:measurement(fede_su_t35, federation_membership_treaty__integration_primary, suppression_requirement, 35, 0.79).
narrative_ontology:measurement_basis(fede_su_t35, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(federation_membership_treaty__integration_primary, resource_allocation).
narrative_ontology:boltzmann_floor_override(federation_membership_treaty__integration_primary, 0.18).
narrative_ontology:affects_constraint(federation_membership_treaty__integration_primary, federation_membership_treaty__sovereignty_primary).
narrative_ontology:affects_constraint(federation_membership_treaty__integration_primary, federation_membership_treaty__subsidiarity_balance).
narrative_ontology:affects_constraint(federation_membership_treaty__integration_primary, national_labor_protection_policies).
narrative_ontology:affects_constraint(federation_membership_treaty__integration_primary, welfare_system_access_boundaries).

% DUAL FORMULATION NOTE:
% This constraint is the integration-primary reading of the federation_membership_treaty kernel. The sovereignty_primary reading (federated states retain authority to protect national labor markets) forecloses this reading's core premise. The subsidiarity_balance reading coexists with this one, offering a middle position. All three readings share the same kernel but instantiate structurally different constraints with different beneficiary/victim structures and extractiveness profiles. The network edge from integration_primary affects the other readings by establishing the presumption-against-restriction that they must argue against.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(federation_membership_treaty__integration_primary, institutional, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
