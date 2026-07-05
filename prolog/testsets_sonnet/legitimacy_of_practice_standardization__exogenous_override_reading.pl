% ============================================================================
% CONSTRAINT STORY: legitimacy_of_practice_standardization__exogenous_override_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_legitimacy_of_practice_standardization__exogenous_override_reading, []).

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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: legitimacy_of_practice_standardization__exogenous_override_reading
 *   human_readable: State-Decreed Practice Standardization (Exogenous Override Reading)
 *   domain: political_history/modernization_studies/institutional_change
 *
 * SUMMARY:
 *   This story instantiates the exogenous_override reading of the
 *   practice-standardization kernel: legitimacy is claimed to flow from state
 *   decree issued for collective benefit — modernization, fiscal alignment,
 *   international parity. The historical pattern this reading captures is
 *   abrupt legal imposition of a new calendar and dress code, backed by
 *   enforcement machinery (registration denial, fines, exclusion from formal
 *   markets), producing not voluntary convergence but a stable long-run
 *   'double life': public/administrative compliance overlaid on persistent
 *   private/agricultural/ritual practice of the old standard, sustained for
 *   decades rather than resolving into either full adoption or full reversal.
 *   This is distinct from the endogenous_displacement_reading (which would
 *   describe a population that adopts the new standard because it perceives
 *   genuine utility, with no coercive apparatus required) and from the
 *   dual_practice_equilibrium_reading (which frames the coexistence as a
 *   stable, non-extractive domain partition rather than as an imposed
 *   override that a subordinate population evades). Those are different
 *   constraints, evaluated in sibling files, linked via
 *   network.affects_constraints.
 *
 * KEY AGENTS:
 *   - central_state_apparatus: agenda_setter (institutional/analytical) — decrees and enforces the standard
 *   - urban_administrative_class: beneficiary (organized/mobile) — already aligned, gains preferential access
 *   - international_trade_partners: beneficiary (institutional/arbitrage) — gains compatibility, bears no enforcement cost
 *   - rural_agrarian_population: payer (powerless/constrained) — maintains lunar calendar underground for decades
 *   - traditional_religious_authorities: payer/excluded (moderate/constrained) — displaced from public calendrical authority, not consulted
 *   - informal_sector_workers: payer (powerless/trapped) — absorbs enforcement friction directly
 *   - modernization_historians: observer (analytical) — assesses the archival record retrospectively
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(legitimacy_of_practice_standardization__exogenous_override_reading, 0.61).
domain_priors:suppression_score(legitimacy_of_practice_standardization__exogenous_override_reading, 0.78).
domain_priors:theater_ratio(legitimacy_of_practice_standardization__exogenous_override_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(legitimacy_of_practice_standardization__exogenous_override_reading, extractiveness, 0.61).
narrative_ontology:constraint_metric(legitimacy_of_practice_standardization__exogenous_override_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(legitimacy_of_practice_standardization__exogenous_override_reading, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(legitimacy_of_practice_standardization__exogenous_override_reading, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(legitimacy_of_practice_standardization__exogenous_override_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(legitimacy_of_practice_standardization__exogenous_override_reading, tangled_rope).
narrative_ontology:human_readable(legitimacy_of_practice_standardization__exogenous_override_reading, "State-Decreed Practice Standardization (Exogenous Override Reading)").
narrative_ontology:topic_domain(legitimacy_of_practice_standardization__exogenous_override_reading, "political_history/modernization_studies/institutional_change").

domain_priors:requires_active_enforcement(legitimacy_of_practice_standardization__exogenous_override_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(legitimacy_of_practice_standardization__exogenous_override_reading, '1ac7d126-e0e5-45be-9def-105a93c03049').
narrative_ontology:cs_kernel_codification('1ac7d126-e0e5-45be-9def-105a93c03049', formalized).
narrative_ontology:cs_authority_grounding('1ac7d126-e0e5-45be-9def-105a93c03049', extraction).
narrative_ontology:cs_interpretation_layer_present('1ac7d126-e0e5-45be-9def-105a93c03049').
narrative_ontology:cs_reading_relation('1ac7d126-e0e5-45be-9def-105a93c03049', legitimacy_of_practice_standardization__endogenous_displacement_reading, coexists_with).
narrative_ontology:cs_reading_relation('1ac7d126-e0e5-45be-9def-105a93c03049', legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, influences).
narrative_ontology:cs_axiom('1ac7d126-e0e5-45be-9def-105a93c03049', foundational, state_decree_for_collective_benefit_is_self_legitimating).
narrative_ontology:cs_axiom_status(state_decree_for_collective_benefit_is_self_legitimating, holdable).
narrative_ontology:cs_axiom_grounding('1ac7d126-e0e5-45be-9def-105a93c03049', state_decree_for_collective_benefit_is_self_legitimating, instrumental).
narrative_ontology:cs_axiom('1ac7d126-e0e5-45be-9def-105a93c03049', foundational, sovereign_authority_may_override_practice_without_populational_consent).
narrative_ontology:cs_axiom_status(sovereign_authority_may_override_practice_without_populational_consent, holdable).
narrative_ontology:cs_axiom_grounding('1ac7d126-e0e5-45be-9def-105a93c03049', sovereign_authority_may_override_practice_without_populational_consent, conventional).
narrative_ontology:cs_reference_frame('1ac7d126-e0e5-45be-9def-105a93c03049', sovereign_modernization_mandate).
narrative_ontology:cs_drift_state('1ac7d126-e0e5-45be-9def-105a93c03049', post_founding_decade_persistence, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('1ac7d126-e0e5-45be-9def-105a93c03049', '').
narrative_ontology:cs_kernel_id(legitimacy_of_practice_standardization__exogenous_override_reading, legitimacy_of_practice_standardization).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(legitimacy_of_practice_standardization__exogenous_override_reading, central_state_apparatus).
narrative_ontology:constraint_beneficiary(legitimacy_of_practice_standardization__exogenous_override_reading, urban_administrative_class).
narrative_ontology:constraint_beneficiary(legitimacy_of_practice_standardization__exogenous_override_reading, international_trade_partners).
narrative_ontology:constraint_victim(legitimacy_of_practice_standardization__exogenous_override_reading, rural_agrarian_population).
narrative_ontology:constraint_victim(legitimacy_of_practice_standardization__exogenous_override_reading, traditional_religious_authorities).
narrative_ontology:constraint_victim(legitimacy_of_practice_standardization__exogenous_override_reading, informal_sector_workers).
narrative_ontology:constraint_vindicates(legitimacy_of_practice_standardization__exogenous_override_reading, state_modernization_doctrine).
narrative_ontology:constraint_vindicates(legitimacy_of_practice_standardization__exogenous_override_reading, fiscal_rationalization_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Decrees the new calendar and dress code by legislative fiat, citing modernization, fiscal alignment with international accounting cycles, and diplomatic parity with reference states. Builds registries, courts, and administrative offices that operate exclusively on the new standard, and deploys police and bureaucratic sanction (fines, denial of services, loss of civil registration) against noncompliance. Collects the legitimacy dividend of appearing modern to foreign creditors and allies.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__exogenous_override_reading, central_state_apparatus, agenda_setter,
    institutional, generational, analytical, national).

% Civil servants, merchants, and professionals whose careers and contracts already run on urban/international schedules. They adopt the new calendar and dress with comparatively little friction, since it formalizes practices they had partially internalized already, and they gain preferential access to state offices, credit, and international correspondence that now require the new standard.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__exogenous_override_reading, urban_administrative_class, beneficiary,
    organized, biographical, mobile, national).

% Foreign governments and trading houses benefit from a synchronized calendar and standardized commercial dress/documentation conventions that reduce transaction friction and verification costs. They apply diplomatic and financial pressure that helped motivate the decree in the first place, but bear none of its domestic enforcement costs.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__exogenous_override_reading, international_trade_partners, beneficiary,
    institutional, generational, arbitrage, global).

% Farmers whose planting, harvest, and ritual calendars are organized around the lunar cycle continue to use it privately for agricultural and religious timing while nominally complying with the state calendar for registration, taxation, and legal documents. This produces a persistent 'double life': public compliance and private continuity, sustained for decades rather than dissolving into voluntary adoption. Exit from the decree is not realistically available — noncompliance risks loss of land registration and legal standing — but exit from the lunar practice is also not chosen; both are maintained simultaneously under cost.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__exogenous_override_reading, rural_agrarian_population, payer,
    powerless, biographical, constrained, regional).

% Clerics and ritual specialists whose calendrical and sartorial authority is displaced from public administrative recognition lose formal standing to set community time and dress norms, even as they continue to be consulted privately for the maintenance of lunar-calendar rites. Their objection — that the decree severs civic time from sacred time — was not solicited in the legislative process.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__exogenous_override_reading, traditional_religious_authorities, payer,
    moderate, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(legitimacy_of_practice_standardization__exogenous_override_reading, traditional_religious_authorities, excluded).

% Market vendors, seasonal laborers, and small producers whose informal contracts and market days were set by the old calendar face repeated friction, fines, and exclusion from formal markets and services when their bookkeeping and scheduling do not align cleanly with the decreed standard. They lack the administrative literacy or resources to navigate dual-calendar compliance and absorb the enforcement costs directly.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__exogenous_override_reading, informal_sector_workers, payer,
    powerless, immediate, trapped, local).

% Study the archival record of decree, enforcement intensity, and underground persistence to assess whether the standardization functioned as genuine coordination gain or as extraction dressed in modernization language. Their analysis is retrospective and does not alter the operative constraint.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__exogenous_override_reading, modernization_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(legitimacy_of_practice_standardization__exogenous_override_reading, central_state_apparatus).
narrative_ontology:fixing_cost_class(legitimacy_of_practice_standardization__exogenous_override_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Synchronizing the state's administrative, fiscal, and diplomatic calendar with international trading partners and rationalizing legal recognition of a single dress/time standard reduces transaction costs in taxation, contracting, and cross-border commerce that a fragmented multi-calendar system could not achieve voluntarily on the state's preferred timetable.
% TRANSFER_FUNCTION: Moves administrative legitimacy, market access, and legal standing from populations organized around the prior lunar/traditional standard to those already aligned (or able to quickly align) with the new state-decreed standard; moves enforcement costs (fines, loss of registration, exclusion from formal markets) onto rural and informal populations while the modernization dividend (diplomatic standing, foreign credit terms) accrues to the central state and its administrative and commercial allies.
% ABSENT_VOICES: Rural agrarian communities and traditional religious authorities were not meaningfully consulted in the legislative process that produced the decree; their objection — that abrupt imposition severs administrative time from lived agricultural and ritual time — surfaces only in later compliance-evasion patterns and oral-history record, not in the founding deliberation.
% DISAPPEARANCE_RATIONALE: From the central state's seat, reversal of the decree would collapse international credibility and fiscal-reporting compatibility overnight — the world rearranges toward isolation and diplomatic friction. From the rural and informal seats, the underground lunar-calendar and traditional-dress practices have persisted stably for decades regardless of the decree's formal status, so its disappearance would mostly just legalize what already continues in practice — the world stays much the same for them. The parties genuinely dispute which reading is correct because they are describing different observed strata of the same imposed standard.
% FOUNDING_PROBLEM: The state needed calendrical and administrative-dress compatibility with international creditors, treaty partners, and modern bureaucratic recordkeeping to secure fiscal stability, trade terms, and diplomatic legitimacy that a locally fragmented, multi-calendar administrative system could not credibly offer.
% FOUNDING_PROBLEM_CORROBORATION: The central state and its urban administrative allies attest the founding problem remains live — ongoing fiscal audits and trade negotiations still cite calendar/documentation standardization as a precondition. Independent economic historians and comparative-modernization scholars, outside the beneficiary set, note that many of the original fiscal-alignment gains were realized within the first decade and that the sustained decades-long enforcement apparatus increasingly polices symbolic compliance and revenue extraction (fines, registration fees) rather than functional coordination need — supporting a 'dead problem, persisting apparatus' reading contested by the state's own account.
narrative_ontology:disappearance_verdict(legitimacy_of_practice_standardization__exogenous_override_reading, contested).
narrative_ontology:founding_problem_status(legitimacy_of_practice_standardization__exogenous_override_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(legitimacy_of_practice_standardization__exogenous_override_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(legitimacy_of_practice_standardization__exogenous_override_reading, 'none', 1).
narrative_ontology:epsilon_provenance(legitimacy_of_practice_standardization__exogenous_override_reading, 0.61, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(legitimacy_of_practice_standardization__exogenous_override_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(legitimacy_of_practice_standardization__exogenous_override_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(legitimacy_of_practice_standardization__exogenous_override_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises from 0.35 to 0.61 over the interval: the initial fiscal/diplomatic coordination gain (compatible reporting cycles, trade-term alignment) is real and front-loaded, but the enforcement apparatus persists and increasingly extracts compliance-fee revenue and registration leverage from populations whose underlying practice never actually converged. Theater ratio climbs from 0.2 to 0.58 — the later-period enforcement increasingly functions as symbolic assertion of state modernity (documentation checks, dress-code fines) rather than solving a live coordination problem, since most of the achievable fiscal/diplomatic synchronization gain was captured early. Suppression is authored high and roughly flat-to-slightly-declining (0.85 to 0.78): the coercive infrastructure (registration denial, legal nonrecognition of the old calendar, sanction on noncompliant dress) is a raw structural feature built at the decree's founding and does not need to intensify to remain effective — it persists as standing infrastructure. This declining trajectory reflects gradual routinization (enforcement becomes bureaucratic habit requiring less active mobilization) rather than genuine relaxation of coercive capacity.
 *
 * PERSPECTIVAL GAP:
 *   From the state's seat, the decree is a rope: a solved coordination problem (calendar/documentation compatibility) enabling fiscal stability and international standing. From the rural and informal seats, the same structure computes as a tangled_rope shading toward snare: the coordination benefit accrued once, decades ago, to parties who were not them, while the enforcement cost is renewed continuously against them. This divergence is exactly the seat-level classification the engine is built to surface — it is not resolved by picking one side's framing as authoritative.
 *
 * DIRECTIONALITY LOGIC:
 *   The central_state_apparatus sits at the analytical/beneficiary end: it authors the standard, enforces it, and collects the diplomatic and fiscal dividend without bearing conversion costs itself. Urban administrative and international trade actors are structural beneficiaries because their existing practice already approximates the decreed standard — the decree formalizes rather than disrupts their situation, so it functions for them close to a subsidy. Rural agrarian populations, informal workers, and traditional authorities are structural targets: they bear the full cost of dual-system maintenance (compliance costs for the new standard, continuity costs for the old) with essentially no compensating access gain, and their exit options are constrained-to-trapped rather than mobile, which the engine's directionality derivation should push toward the high-χ target end.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding fiscal/diplomatic problem was substantially solved within roughly a decade (per the historians' corroboration), yet the enforcement apparatus — fines, registration denial, dress-code sanction — has persisted for forty years at only mildly declining suppression. This is the mandatrophy signature: an arrangement whose original coordination justification has been achieved continues to extract under the same justificatory language ('modernization,' 'fiscal stability') because the state apparatus that built the enforcement machinery has no structural incentive to dismantle it, and because dismantling it would concede that decades of enforcement against rural populations was, at minimum, disproportionate to remaining need.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    override_vs_partition_reading_boundary,
    'Is the multi-decade ''double life'' pattern (public compliance, private lunar-calendar continuity) better modeled as an exogenous override that rural populations evade under continuing coercion, or as a de facto dual_practice_equilibrium that both sides have tacitly accepted as a stable settlement?',
    'Examine whether state enforcement intensity is actively targeting private/ritual-domain lunar practice (supporting the override reading) or has retreated to policing only public/administrative-domain compliance while tolerating private practice (supporting the equilibrium reading). Longitudinal enforcement-record analysis by domain would resolve this.',
    'If enforcement has structurally retreated to the administrative domain only, this story''s claimed_type and metrics should migrate toward the dual_practice_equilibrium_reading sibling rather than remaining tangled_rope under override framing; if enforcement remains domain-indiscriminate, the override reading holds and this story''s extractive trajectory stands.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(override_vs_partition_reading_boundary, conceptual, 'Whether the observed equilibrium is coerced-override or negotiated-partition — the boundary between this reading and its dual_practice_equilibrium sibling.').

omega_variable(
    founding_problem_persistence_ambiguity,
    'Did the fiscal/diplomatic coordination problem the decree was built to solve genuinely persist for the full forty-year enforcement period, or was it substantially resolved early with enforcement continuing on inertia and revenue capture from fines/registration fees?',
    'Compare the marginal fiscal/diplomatic benefit attributable to continued strict enforcement in the later interval years against the enforcement-generated revenue (fines, registration denial fees) and administrative cost of maintaining the apparatus.',
    'If the founding problem was resolved early, the later-period constraint is better characterized as approaching a snare (extraction with atrophied coordination justification) rather than a tangled_rope with a live coordination function; this would also strengthen the mandatrophy_analysis finding.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(founding_problem_persistence_ambiguity, empirical, 'Whether the coordination justification for continued enforcement remained live throughout the interval or became a cover story for revenue extraction.').

omega_variable(
    exogenous_versus_endogenous_at_the_margin,
    'For the urban administrative class specifically, was their adoption of the new standard genuinely coerced by the decree, or would they have adopted it endogenously (per the sibling endogenous_displacement_reading) given their pre-existing partial alignment, making the decree''s coercive apparatus redundant for that population?',
    'Trace pre-decree adoption trends among urban administrative and merchant populations; if voluntary convergence was already underway before legal imposition, the decree''s coercive machinery was targeted primarily at rural/informal populations who would not have converged endogenously.',
    'This would sharpen the claim that the override apparatus is structurally aimed at extracting compliance specifically from populations for whom voluntary adoption (the endogenous reading) does not apply, reinforcing this story''s differentiated victim/beneficiary structure.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(exogenous_versus_endogenous_at_the_margin, empirical, 'Whether the coercive apparatus is redundant for the beneficiary population that would have converged without it, revealing its true target population.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(legitimacy_of_practice_standardization__exogenous_override_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(legi_tr_t0, legitimacy_of_practice_standardization__exogenous_override_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(legi_tr_t8, legitimacy_of_practice_standardization__exogenous_override_reading, theater_ratio, 8, 0.3).
narrative_ontology:measurement(legi_tr_t16, legitimacy_of_practice_standardization__exogenous_override_reading, theater_ratio, 16, 0.4).
narrative_ontology:measurement(legi_tr_t24, legitimacy_of_practice_standardization__exogenous_override_reading, theater_ratio, 24, 0.48).
narrative_ontology:measurement(legi_tr_t32, legitimacy_of_practice_standardization__exogenous_override_reading, theater_ratio, 32, 0.54).
narrative_ontology:measurement(legi_tr_t40, legitimacy_of_practice_standardization__exogenous_override_reading, theater_ratio, 40, 0.58).

% Extraction over time
narrative_ontology:measurement(legi_be_t0, legitimacy_of_practice_standardization__exogenous_override_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(legi_be_t8, legitimacy_of_practice_standardization__exogenous_override_reading, base_extractiveness, 8, 0.44).
narrative_ontology:measurement(legi_be_t16, legitimacy_of_practice_standardization__exogenous_override_reading, base_extractiveness, 16, 0.51).
narrative_ontology:measurement(legi_be_t24, legitimacy_of_practice_standardization__exogenous_override_reading, base_extractiveness, 24, 0.56).
narrative_ontology:measurement(legi_be_t32, legitimacy_of_practice_standardization__exogenous_override_reading, base_extractiveness, 32, 0.59).
narrative_ontology:measurement(legi_be_t40, legitimacy_of_practice_standardization__exogenous_override_reading, base_extractiveness, 40, 0.61).

% Suppression requirement over time
narrative_ontology:measurement(legi_su_t0, legitimacy_of_practice_standardization__exogenous_override_reading, suppression_requirement, 0, 0.85).
narrative_ontology:measurement(legi_su_t8, legitimacy_of_practice_standardization__exogenous_override_reading, suppression_requirement, 8, 0.82).
narrative_ontology:measurement(legi_su_t16, legitimacy_of_practice_standardization__exogenous_override_reading, suppression_requirement, 16, 0.79).
narrative_ontology:measurement(legi_su_t24, legitimacy_of_practice_standardization__exogenous_override_reading, suppression_requirement, 24, 0.78).
narrative_ontology:measurement(legi_su_t32, legitimacy_of_practice_standardization__exogenous_override_reading, suppression_requirement, 32, 0.78).
narrative_ontology:measurement(legi_su_t40, legitimacy_of_practice_standardization__exogenous_override_reading, suppression_requirement, 40, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(legitimacy_of_practice_standardization__exogenous_override_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(legitimacy_of_practice_standardization__exogenous_override_reading, endogenous_displacement_reading).
narrative_ontology:affects_constraint(legitimacy_of_practice_standardization__exogenous_override_reading, dual_practice_equilibrium_reading).

% DUAL FORMULATION NOTE:
% Three constraints share the kernel legitimacy_of_practice_standardization, decomposed per the ε-invariance principle: this story (exogenous_override_reading) models abrupt legal imposition with enforcement machinery and persistent underground non-convergence — a tangled_rope with rising extraction and theater. The sibling endogenous_displacement_reading models the same nominal 'practice change' where legitimacy derives from voluntary utility-driven adoption with no coercive apparatus, structurally closer to a rope. The sibling dual_practice_equilibrium_reading models the coexistence as a stable, negotiated domain partition (state governs public/administrative practice, tradition governs private/ritual practice) rather than an evaded override, structurally closer to a scaffold or rope depending on whether the partition was designed or emergent. All three are linked here because they represent structurally distinct claims that a single colloquial label ('the calendar reform was legitimate') would otherwise conflate.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
