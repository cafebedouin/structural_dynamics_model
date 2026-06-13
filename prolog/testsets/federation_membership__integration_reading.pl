% ============================================================================
% CONSTRAINT STORY: federation_membership__integration_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: federation_membership__integration_reading
 *   human_readable: Federation Membership as Irreversible Integration (Supranational Free Movement Reading)
 *   domain: political_economy/federalism/migration
 *
 * SUMMARY:
 *   This constraint instantiates the integration reading of the
 *   federation-membership kernel: membership is irreversible once acceded;
 *   supranational authority over migration is legitimate; free movement of
 *   citizens is a constitutional right that member states may not restrict.
 *   The reading concentrates benefits on mobile citizens and labor-arbitrage
 *   beneficiaries (who gain unconditional access to federation labor markets
 *   at higher productivity and wages) while concentrating costs on localized
 *   labor markets and low-skill resident workers (who face wage suppression,
 *   employment rationing, and institutional powerlessness to protect
 *   themselves). The supranational authority administers the constraint by
 *   invalidating member-state attempts at border closure or residency
 *   restrictions and by enforcing the irreversibility doctrine against any
 *   member state that attempts exit. The measurement series tracks rising
 *   extractiveness and suppression over a 40-year period: as mobile
 *   populations grew and localized wage effects accumulated, the political
 *   suppression required to maintain the integration reading intensified
 *   (member governments faced domestic pressure to restrict mobility but were
 *   forbidden by supranational enforcement). Theater ratio rises early then
 *   plateaus, suggesting that enforcement was initially theatrical (many
 *   violations, selective prosecution) but eventually became routinized once
 *   the doctrine was embedded in law and practice.
 *
 * KEY AGENTS:
 *   - Mobile EU citizens (beneficiary, organized, mobile exit): gain unconditional labor-market access and wage arbitrage opportunities.
 *   - Localized labor markets (payer, moderate, constrained exit): experience wage suppression and employment rationing in high-mobility sectors.
 *   - Low-skill resident workers (payer, powerless, trapped exit): face direct competition and institutional powerlessness; cannot relocate outside the federation without exiting labor market.
 *   - Host-state fiscal capacity (payer, institutional, constrained exit): bears service delivery costs for newly resident mobile workers; cannot restrict benefit access.
 *   - Supranational authority (agenda-setter, institutional, analytical exit): administers constraint, enforces irreversibility, invalidates member-state border restrictions.
 *   - Member-state governments (payer/observer, organized, constrained exit): politically pressured by resident populations but legally bound by integration doctrine; cannot exercise border control.
 *   - Excluded third-country nationals (excluded, powerless, trapped exit): remain barred from federation mobility rights; their exclusion is structural to the boundary maintenance that legitimates the constraint.
 *   - Competition authorities and policy analysts (observer, analytical, analytical exit): assess efficiency gains and displacement effects.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(federation_membership__integration_reading, 0.68).
domain_priors:suppression_score(federation_membership__integration_reading, 0.72).
domain_priors:theater_ratio(federation_membership__integration_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(federation_membership__integration_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(federation_membership__integration_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(federation_membership__integration_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(federation_membership__integration_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(federation_membership__integration_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(federation_membership__integration_reading, tangled_rope).
narrative_ontology:human_readable(federation_membership__integration_reading, "Federation Membership as Irreversible Integration (Supranational Free Movement Reading)").
narrative_ontology:topic_domain(federation_membership__integration_reading, "political_economy/federalism/migration").

domain_priors:requires_active_enforcement(federation_membership__integration_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(federation_membership__integration_reading, 'c71d5105-5c4a-43bf-bdae-dc7d3499fa99').
narrative_ontology:cs_kernel_codification('c71d5105-5c4a-43bf-bdae-dc7d3499fa99', formalized).
narrative_ontology:cs_authority_grounding('c71d5105-5c4a-43bf-bdae-dc7d3499fa99', extraction).
narrative_ontology:cs_interpretation_layer_present('c71d5105-5c4a-43bf-bdae-dc7d3499fa99').
narrative_ontology:cs_reading_relation('c71d5105-5c4a-43bf-bdae-dc7d3499fa99', federation_membership__sovereignty_reading, coexists_with).
narrative_ontology:cs_axiom('c71d5105-5c4a-43bf-bdae-dc7d3499fa99', foundational, mobility_irreversibility_doctrine).
narrative_ontology:cs_axiom_status(mobility_irreversibility_doctrine, holdable).
narrative_ontology:cs_axiom_grounding('c71d5105-5c4a-43bf-bdae-dc7d3499fa99', mobility_irreversibility_doctrine, deontological).
narrative_ontology:cs_axiom('c71d5105-5c4a-43bf-bdae-dc7d3499fa99', foundational, supranational_authority_over_borders).
narrative_ontology:cs_axiom_status(supranational_authority_over_borders, holdable).
narrative_ontology:cs_axiom_grounding('c71d5105-5c4a-43bf-bdae-dc7d3499fa99', supranational_authority_over_borders, conventional).
narrative_ontology:cs_reference_frame('c71d5105-5c4a-43bf-bdae-dc7d3499fa99', supranational_mobility_constitutionalism).
narrative_ontology:cs_drift_state('c71d5105-5c4a-43bf-bdae-dc7d3499fa99', contemporary_labor_displacement_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('c71d5105-5c4a-43bf-bdae-dc7d3499fa99', '').
narrative_ontology:cs_kernel_id(federation_membership__integration_reading, federation_membership).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(federation_membership__integration_reading, mobile_eu_citizens).
narrative_ontology:constraint_beneficiary(federation_membership__integration_reading, labor_arbitrage_beneficiaries).
narrative_ontology:constraint_victim(federation_membership__integration_reading, localized_labor_markets).
narrative_ontology:constraint_victim(federation_membership__integration_reading, low_skill_resident_workers).
narrative_ontology:constraint_victim(federation_membership__integration_reading, host_state_fiscal_capacity).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(federation_membership__integration_reading, member_state_governments).
narrative_ontology:constraint_vindicates(federation_membership__integration_reading, supranational_authority_legitimacy).
narrative_ontology:constraint_vindicates(federation_membership__integration_reading, human_mobility_as_fundamental_right).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Possess unconditional right to reside, work, and establish business in any member state without visa or labor market testing. Can relocate to pursue employment, education, or lifestyle preferences within the federation without member state permission. Benefit from wage arbitrage (high-skill workers relocating to premium markets), access to lower-cost services (high-wage workers in service-importing states), and institutional stability guaranteeing the right persists across political cycles.
narrative_ontology:constraint_stakeholder(federation_membership__integration_reading, mobile_eu_citizens, beneficiary,
    organized, biographical, mobile, global).

% Experience downward wage pressure and reduced employment opportunities where mobile workers have cost or skill advantages. Sectors with high mobile-worker concentration (construction, hospitality, care work, tech) see compressed wages and displacement. Local labor supply is constrained by the inability to restrict entry based on residency or national origin; wage-setting power is eroded by open supply elasticity.
narrative_ontology:constraint_stakeholder(federation_membership__integration_reading, localized_labor_markets, payer,
    moderate, biographical, constrained, regional).

% Face wage suppression and employment rationing in sectors with high mobile-worker concentration. Cannot relocate to avoid competition without leaving the federation or exiting the workforce. Lack the skill profile to arbitrage across borders as mobile workers do. Experience both direct competition and institutional powerlessness: the migration constraint is legally irreversible, and residency-based labor market protection is prohibited.
narrative_ontology:constraint_stakeholder(federation_membership__integration_reading, low_skill_resident_workers, payer,
    powerless, biographical, trapped, regional).

% Faces fiscal pressure from providing welfare, education, and healthcare services to newly resident mobile workers, especially where mobile populations cluster in high-demand sectors and contribute to housing-market pressures that public services must accommodate. Cannot condition benefit access on prior contribution or restrict public-service provision to citizens. Fiscal pressure is highest in host states with higher public provision and net inbound mobile-worker flows.
narrative_ontology:constraint_stakeholder(federation_membership__integration_reading, host_state_fiscal_capacity, payer,
    institutional, generational, constrained, national).

% Administers and enforces the integration reading: maintains that federation membership is irreversible once acceded, that free movement is a constitutional right that may not be suspended by member states, and that supranational authority over migration policy is legitimate because migration flows constitute a federation-level collective action problem. Enforces the constraint by invalidating member-state attempts at border closure, work permits, or residency restrictions, and by sanctions against non-compliance.
narrative_ontology:constraint_stakeholder(federation_membership__integration_reading, supranational_authority, agenda_setter,
    institutional, civilizational, analytical, global).

% Are legally bound by the integration reading but politically pressured by resident populations facing wage suppression and fiscal strain. Retain nominal sovereignty over many policy domains but cannot exercise border control, residency restrictions, or labor market protection measures consistent with the integration framework. Some governments actively support the reading (high-migration-receiving states with labor shortages); others experience domestic political pressure to restrict mobility but face supranational sanctions if they attempt exit.
narrative_ontology:constraint_stakeholder(federation_membership__integration_reading, member_state_governments, payer,
    organized, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(federation_membership__integration_reading, member_state_governments, observer).

% Remain barred from the unconditional mobility rights within the federation. Their access is governed by national labor market testing, quotas, and bilateral agreements. The integration reading's legitimacy rests partly on exclusion: the constitutional right to mobility is asserted as a federation member right, meaning non-members are categorically excluded from its protection. This creates a boundary maintenance function at the federation's external border.
narrative_ontology:constraint_stakeholder(federation_membership__integration_reading, excluded_third_country_nationals, excluded,
    powerless, biographical, trapped, regional).

% Analyze the constraint's operation and effects. Some emphasize efficiency gains (labor reallocation to higher-productivity uses, arbitrage efficiency, innovation from diverse populations). Others emphasize displacement effects (wage suppression in low-skill sectors, fiscal strain in host states, political legitimacy crisis as resident populations perceive their interests as sacrificed to supranational doctrine).
narrative_ontology:constraint_stakeholder(federation_membership__integration_reading, competition_authorities_and_policy_analysts, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(federation_membership__integration_reading, mobile_eu_citizens).
narrative_ontology:fixing_cost_class(federation_membership__integration_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the federation's collective action problem of labor mobility: if each member state restricts mobility unilaterally, the federation cannot function as an integrated market; if all restrict, the gains from specialization and reallocation are lost. The integration reading declares irreversibility and constitutional status to prevent individual states from free-riding (accepting mobile workers' tax contributions while restricting their access), which would collapse the whole arrangement.
% TRANSFER_FUNCTION: Transfers labor-market rents from localized resident populations (wage suppression, rationing of jobs) to mobile citizens (access to higher-wage markets, unconstrained relocation options) and to sectors with high mobile-worker demand (cost reduction in labor-intensive services, access to skill-mismatched labor). Also transfers fiscal burdens to host states and community service providers. The supranational authority collects legitimacy (authority over member states) and vindication of the integration doctrine.
% ABSENT_VOICES: Third-country nationals are structurally excluded from the conversation about federation mobility rights; their only voice is as beneficiaries of exclusion (through which boundary maintenance is legitimated). Low-skill resident workers, though geographically inside, have been systematically outvoiced in integration-era policy formation — most major federation institutions and supranational bodies are staffed by high-skill, mobile professionals whose experience differs radically from localized labor-market participants. Their interests appear in electoral outcomes and strikes, not in the institutions that set the constraint.
% DISAPPEARANCE_RATIONALE: If the integration reading and its enforcement vanished overnight (if free movement were reverted to member-state discretion, if supranational authority over migration were dismantled), member states would immediately reinstitute border controls, labor market testing, and residency restrictions. Wage pressure in affected sectors would ease, fiscal burdens on host states would decline, and political pressure on member governments would diminish. The federation's institutional architecture would undergo wholesale reconfiguration; some integration-dependent supply chains and professional mobility patterns would contract. The world does not revert to pre-integration conditions (supply chains are now embedded), but reorganizes around state-contingent mobility rules.
% FOUNDING_PROBLEM: Built to solve the federation's economic inefficiency under national labor market fragmentation: after the initial integration, member states maintained labor market controls that prevented efficient reallocation of workers to their highest-productivity uses. The founding problem was the persistence of Balkanized labor markets inside a nominally unified economy — workers could move capital easily but faced legal barriers to personal relocation.
% FOUNDING_PROBLEM_CORROBORATION: Supranational authorities and economic research communities attest the founding problem was real and was substantially solved: labor mobility increased, specialization deepened, and aggregate productivity measures improved. Labor economists and host-state governments (especially those experiencing net inflows and fiscal pressures) attest that the founding problem's solution created a secondary problem the integration reading does not address: localized wage suppression and fiscal strain. Third-country nationals outside the federation cannot speak to the founding problem; inside observers dispute whether the problem was integration-level (a federation issue) or could have been solved at state level with targeted labor-market reforms.
narrative_ontology:disappearance_verdict(federation_membership__integration_reading, world_rearranges).
narrative_ontology:founding_problem_status(federation_membership__integration_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(federation_membership__integration_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(federation_membership__integration_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(federation_membership__integration_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(federation_membership__integration_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(federation_membership__integration_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.68 at interval end) and rising through the first 30 years of the measurement interval because the integration reading's enforcement creates a sustained transfer from localized labor markets to mobile citizens. Suppression is higher still (0.72) because member states face binding constraints that prevent them from using border control or residency restrictions to protect localized workers — the constraint's persistence depends on actively preventing member-state exit and policy deviation. Theater ratio rises early (reaching 0.41 by year 30) because enforcement activity includes not only genuine administration of free-movement rights but also increasingly theatrical prosecution of member-state 'violations' — states attempt minor border restrictions; supranational bodies publicize the enforcement action; the integration reading is reaffirmed. Theater plateaus after year 30 (remains 0.41 through year 40) suggesting the doctrine has achieved sufficient institutional embedding that routine enforcement suffices; new violations decline because the doctrine is now internalized. The measurement grid is shared across all three metrics — every metric is authored at every examined time point, enabling temporal alignment and preventing the OQ-105 misalignment artifact. The rising trajectory reflects both the growing scale of mobile populations and the intensifying political suppression required to maintain the reading as low-skill resident workers' wage effects accumulate.
 *
 * PERSPECTIVAL GAP:
 *   From the supranational authority's seat, the constraint is genuine coordination solving the federation's labor-market fragmentation; from low-skill resident workers' seats, the same structure operates as enforced extraction. The engine should compute substantially different types from these two seats: the authority seat experiences the coordination function (beneficiary-proximate directionality, possibly rope or tangled-rope at that seat); the resident-worker seat experiences pure targeting (victim-proximate directionality, possibly snare at that seat). The authored claim (tangled_rope) sits between — asserting that both functions are real — but per-seat computation will likely show divergence: high extractiveness + victims + high suppression should compute toward snare at the victim seats, even though the coordination function is also real. That divergence is the measurement the corpus exists to take: a constraint that is claimed as coordination but computes as extraction at victim seats is exactly how extractive constraints maintain legitimacy through doctrine.
 *
 * DIRECTIONALITY LOGIC:
 *   Mobile citizens are at the beneficiary end of directionality (d near 0.0) because they collect unconditional access to the full federation labor market, face no residency restrictions, have high exit options (can relocate within the federation or exit to other federations), and bear no direct suppression. Supranational authority is also beneficiary-proximate (d near 0.1–0.2) because it collects legitimacy and authority over member states through the integration reading and administers the constraint's enforcement. Localized labor markets and low-skill resident workers are at the target end (d near 0.8–0.95) because they experience wage suppression, employment rationing, and face institutional powerlessness to protect themselves — their exit options are trapped or severely constrained; they cannot relocate within the federation without exiting their home states. Host-state fiscal capacity sits near symmetric (d near 0.5–0.6) because it bears genuine coordination costs (hosting mobile populations, providing services) but also experiences extractive burdens (fiscal pressure from not being able to restrict benefit access). Member-state governments sit near the payer end (d near 0.6–0.75) because they are politically pressured by resident populations but legally bound by the supranational authority and cannot exercise border control or wage protection — they are caught between two masters (their own citizens and the supranational doctrine). No directionality_overrides are needed; the structural derivation from beneficiary/victim declarations and exit options produces accurate directionality.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem was genuine and is now substantially solved: federation labor markets are substantially more mobile and efficient than pre-integration fragmented markets. The integration reading achieved its mandate — removing the legal barriers to labor mobility that member states had maintained. However, the founding problem's solution revealed a secondary problem that the integration reading does not address: localized wage suppression and fiscal strain in host states. The constraint shows mandatrophy signature: the original coordination problem (inefficient labor-market fragmentation) is solved, but the solution now persists by doctrine and supranational enforcement rather than by participant choice. A state experiencing net inbound mobile workers and low-skill wage suppression would, if free to exit, likely restructure the arrangement (adding residency restrictions, wage floors, fiscal-contribution requirements) — but supranational enforcement prevents that restructuring. The theater_ratio trajectory (rising early, plateauing at 0.41) suggests that mandatrophy has occurred: enforcement activity is now partly theatrical, reaffirming the integration doctrine for audiences rather than resolving genuine coordination failures. If extractiveness plateaus at 0.68 while theater remains at 0.41, the constraint is in mandatrophy state: real extraction persists not because the founding coordination problem requires it, but because the doctrine is now institutionally embedded and enforced by supranational authority. A Piton trajectory would show theater rising faster than extractiveness, with extractiveness eventually falling as the real function atrophies — this constraint shows the opposite (extractiveness rising, theater plateauing), suggesting it is a Tangled Rope with accumulating mandatrophy symptoms rather than a full Piton, but the trend bears monitoring.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reading_kernel_boundary,
    'Is this constraint fundamentally about federalism and supranational authority (the integration reading) or about membership reversibility and state exit (the contested domain that separates integration from sovereignty readings)?',
    'Historical and institutional analysis: which sibling reading was authored first? Which was embedded in founding texts? Which reading was invoked in early institutional disputes? Temporal priority and institutional genealogy reveal whether the kernel is centrally about supranational legitimacy or about membership stability.',
    'If the kernel is primarily about supranational legitimacy, the integration reading is the canonical reading and the sovereignty reading is the deviation. If the kernel is about membership reversibility, both readings are equally primary and neither forecloses the other — they coexist across different institutional actors'' commitments.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_kernel_boundary, conceptual, 'Whether the integration reading forecloses the sovereignty reading or coexists with it as a live alternative.').

omega_variable(
    labor_extraction_vs_coordination_cost,
    'What portion of the measured extractiveness (0.68) represents the intrinsic coordination cost of integrated labor markets versus the rents collected by mobile populations at the expense of localized residents?',
    'Counterfactual analysis: construct a federation-level labor market with mobility fully open but with fiscal transfers and wage-floor protections for resident low-skill workers (so mobile and resident workers face the same baseline). Measure the residual extractiveness in that counterfactual. The delta between the counterfactual and the actual measured extraction is the institutional choice, not the coordination necessity.',
    'A large delta supports reclassification to snare (pure extraction with coordination cover). A small delta supports the tangled_rope classification (genuine coordination with unavoidable asymmetric effects). The integration reading''s legitimacy depends partly on that ratio: if extraction dominates over coordination cost, the doctrine becomes harder to defend.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(labor_extraction_vs_coordination_cost, empirical, 'Whether the extraction is intrinsic to federation labor mobility or an institutional choice layered on top of it.').

omega_variable(
    suppression_internalization_dynamic,
    'Is the measured suppression (0.72) structural (member states are forced by supranational enforcement to accept mobility they resist) or internalized (member states and resident populations have accepted the integration ideology so thoroughly that they no longer perceive resistance as legitimate)?',
    'Post-enforcement trajectory analysis: in jurisdictions where supranational enforcement pressure loosened or disappeared, did suppression drop (indicating structural suppression that required active enforcement) or persist (indicating internalized adoption of the integration doctrine)? Also: do member states enact policies that would protect localized workers if permitted, suggesting suppressed preference?',
    'If structural, the constraint''s stability depends on continued enforcement; if internalized, the constraint is more stable and its theater ratio is lower (less performative enforcement needed). If internalized, the integration reading has achieved doctrinal hegemony; if structural, it remains contested doctrine held in place by institutional power.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_internalization_dynamic, empirical, 'Whether suppression is maintained by external enforcement or internalized ideological commitment.').

omega_variable(
    axiom_identity_fusion_mechanism,
    'For mobile EU citizens, has the constitutional status of free movement fused with personal/professional identity such that exit from the federation would feel like identity death? How much of their exit_options rating (mobile, arbitrage) actually reflects genuine choice versus identity-locked refusal of exit even as a theoretical option?',
    'Post-membership survey: in the rare cases where federation membership has been revoked or voluntarily exited, do former citizens report the subjective experience as identity loss? Do they retain the internalized commitment to free movement as a fundamental right even after losing institutional protection?',
    'If identity-fused, mobile citizens'' support for the integration reading is partially locked in by identity commitment rather than rational preference — exit_options should be downgraded from mobile/arbitrage toward identity_locked. This affects directionality: an identity-locked beneficiary is harder to persuade to exit the arrangement and provides more stable political support for the constraint.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(axiom_identity_fusion_mechanism, empirical, 'Whether free movement as a constitutional right has become fused with mobile citizens'' identity, locking in support for the integration reading.').

omega_variable(
    sibling_reading_foreclosure_test,
    'Does the integration reading''s assertion of irreversibility and supranational legitimacy logically foreclose the sovereignty reading''s assertion of conditional membership and state border authority, or do both readings remain live positions that different institutional actors can simultaneously hold?',
    'Logical consistency analysis: could a single institutional framework acknowledge both that membership is irreversible and that member states retain border authority? Could both axioms be held true within one coherent legal theory, or does one axiom directly contradict the other? If they can coexist in different doctrinal traditions held by different actors (courts, legislatures, member states), the relation is coexists_with; if they cannot coexist in any single framework, the relation is forecloses.',
    'If forecloses, the sibling reading is logically ruled out by this reading''s commitments — the kernels compose as a ranking where the integration reading is the master axiom. If coexists_with, both readings survive as live alternatives held by different institutional seats, and the contest between them is empirical/political, not logical.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_reading_foreclosure_test, conceptual, 'Whether the integration reading forecloses the sovereignty reading logically or leaves it as a coexisting alternative.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(federation_membership__integration_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fed_int_tr_t0, federation_membership__integration_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement_basis(fed_int_tr_t0, observed).
narrative_ontology:measurement(fed_int_tr_t5, federation_membership__integration_reading, theater_ratio, 5, 0.22).
narrative_ontology:measurement_basis(fed_int_tr_t5, observed).
narrative_ontology:measurement(fed_int_tr_t10, federation_membership__integration_reading, theater_ratio, 10, 0.28).
narrative_ontology:measurement_basis(fed_int_tr_t10, observed).
narrative_ontology:measurement(fed_int_tr_t15, federation_membership__integration_reading, theater_ratio, 15, 0.34).
narrative_ontology:measurement_basis(fed_int_tr_t15, observed).
narrative_ontology:measurement(fed_int_tr_t20, federation_membership__integration_reading, theater_ratio, 20, 0.38).
narrative_ontology:measurement_basis(fed_int_tr_t20, observed).
narrative_ontology:measurement(fed_int_tr_t25, federation_membership__integration_reading, theater_ratio, 25, 0.4).
narrative_ontology:measurement_basis(fed_int_tr_t25, observed).
narrative_ontology:measurement(fed_int_tr_t30, federation_membership__integration_reading, theater_ratio, 30, 0.41).
narrative_ontology:measurement_basis(fed_int_tr_t30, observed).
narrative_ontology:measurement(fed_int_tr_t40, federation_membership__integration_reading, theater_ratio, 40, 0.41).
narrative_ontology:measurement_basis(fed_int_tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(fed_int_be_t0, federation_membership__integration_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement_basis(fed_int_be_t0, observed).
narrative_ontology:measurement(fed_int_be_t5, federation_membership__integration_reading, base_extractiveness, 5, 0.48).
narrative_ontology:measurement_basis(fed_int_be_t5, observed).
narrative_ontology:measurement(fed_int_be_t10, federation_membership__integration_reading, base_extractiveness, 10, 0.54).
narrative_ontology:measurement_basis(fed_int_be_t10, observed).
narrative_ontology:measurement(fed_int_be_t15, federation_membership__integration_reading, base_extractiveness, 15, 0.59).
narrative_ontology:measurement_basis(fed_int_be_t15, observed).
narrative_ontology:measurement(fed_int_be_t20, federation_membership__integration_reading, base_extractiveness, 20, 0.64).
narrative_ontology:measurement_basis(fed_int_be_t20, observed).
narrative_ontology:measurement(fed_int_be_t25, federation_membership__integration_reading, base_extractiveness, 25, 0.66).
narrative_ontology:measurement_basis(fed_int_be_t25, observed).
narrative_ontology:measurement(fed_int_be_t30, federation_membership__integration_reading, base_extractiveness, 30, 0.68).
narrative_ontology:measurement_basis(fed_int_be_t30, observed).
narrative_ontology:measurement(fed_int_be_t40, federation_membership__integration_reading, base_extractiveness, 40, 0.68).
narrative_ontology:measurement_basis(fed_int_be_t40, observed).

% Suppression requirement over time
narrative_ontology:measurement(fed_int_su_t0, federation_membership__integration_reading, suppression_requirement, 0, 0.48).
narrative_ontology:measurement_basis(fed_int_su_t0, observed).
narrative_ontology:measurement(fed_int_su_t5, federation_membership__integration_reading, suppression_requirement, 5, 0.54).
narrative_ontology:measurement_basis(fed_int_su_t5, observed).
narrative_ontology:measurement(fed_int_su_t10, federation_membership__integration_reading, suppression_requirement, 10, 0.61).
narrative_ontology:measurement_basis(fed_int_su_t10, observed).
narrative_ontology:measurement(fed_int_su_t15, federation_membership__integration_reading, suppression_requirement, 15, 0.66).
narrative_ontology:measurement_basis(fed_int_su_t15, observed).
narrative_ontology:measurement(fed_int_su_t20, federation_membership__integration_reading, suppression_requirement, 20, 0.7).
narrative_ontology:measurement_basis(fed_int_su_t20, observed).
narrative_ontology:measurement(fed_int_su_t25, federation_membership__integration_reading, suppression_requirement, 25, 0.72).
narrative_ontology:measurement_basis(fed_int_su_t25, observed).
narrative_ontology:measurement(fed_int_su_t30, federation_membership__integration_reading, suppression_requirement, 30, 0.72).
narrative_ontology:measurement_basis(fed_int_su_t30, observed).
narrative_ontology:measurement(fed_int_su_t40, federation_membership__integration_reading, suppression_requirement, 40, 0.72).
narrative_ontology:measurement_basis(fed_int_su_t40, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(federation_membership__integration_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(federation_membership__integration_reading, 0.18).
narrative_ontology:affects_constraint(federation_membership__integration_reading, federation_membership__sovereignty_reading).
narrative_ontology:affects_constraint(federation_membership__integration_reading, third_country_border_exclusion).
narrative_ontology:affects_constraint(federation_membership__integration_reading, national_labor_market_protection).
narrative_ontology:affects_constraint(federation_membership__integration_reading, supranational_institutional_authority).

% DUAL FORMULATION NOTE:
% This story (integration_reading) and federation_membership__sovereignty_reading form a kernel pair: both constraints instantiate the contested federation-membership kernel. They have opposite beneficiary/victim structures, opposite directionality profiles, and opposite ε values. The integration reading shows high extractiveness from labor displacement; the sovereignty reading would show high extractiveness from capital immobility or reduced market size. The two readings affect each other structurally: if the sovereignty reading is institutionalized (member states retain border authority), the integration reading's architecture collapses — free movement becomes contingent rather than constitutional. The stories share network linkages to related constraints: the exclusion of third-country nationals is structurally dependent on the integration reading (the boundary that includes mobile citizens is the same boundary that excludes non-citizens); national labor-market protection measures are technically foreclosed by the integration reading but institutionalized under the sovereignty reading; supranational institutional authority is legitimated by the integration reading and constrained by the sovereignty reading.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
