% ============================================================================
% CONSTRAINT STORY: imposition_pathway_kernel__endogenous_climb_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_imposition_pathway_kernel__endogenous_climb_reading, []).

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
 *   constraint_id: imposition_pathway_kernel__endogenous_climb_reading
 *   human_readable: Endogenous Climb Reading of Commitment Displacement (Meiji Calendar/Dress Reform)
 *   domain: historical_sociology/state_formation
 *
 * SUMMARY:
 *   This story instantiates the endogenous_climb_reading of the
 *   imposition_pathway_kernel, applied to Meiji Japan's 1873 calendar and
 *   associated dress reforms. This reading holds that ALL apparent top-down
 *   commitment displacement decomposes, on closer inspection, into a
 *   pre-existing fringe adoption stage (here: treaty-port merchants and
 *   modernizing military units adopting Western dating and dress for
 *   interoperability reasons, years before the state decree) followed by
 *   state ratification that accelerates and formalizes the climb rather than
 *   initiating it. The decree's dramatic, sudden appearance in the historical
 *   record is, on this reading, an artifact of where the record is kept
 *   (state archives) rather than evidence that displacement began there. The
 *   reading's distinctive commitment is that NO commitment displacement is
 *   genuinely top-down-initiated — every apparent imposition, examined with
 *   sufficiently fine-grained archival access, will reveal an invisible
 *   antecedent climb. This is what distinguishes it from
 *   exogenous_override_reading (which holds some displacements are genuinely
 *   state-initiated with no antecedent) and hybrid_cascade_reading (which
 *   holds the state can manufacture an artificial fringe that then climbs,
 *   making override a valid initiating mechanism in some cases).
 *
 * KEY AGENTS:
 *   - meiji_state_modernizers: agenda_setter (institutional/arbitrage) — issues the ratifying decree
 *   - treaty_port_merchant_class: beneficiary (organized/mobile) — pre-decree fringe adopters
 *   - military_modernization_faction: beneficiary/agenda_setter (organized/constrained) — pre-decree fringe adopters with enforcement leverage
 *   - traditionalist_rural_populations: payer (powerless/trapped) — no visible fringe stage, experiences apparent imposition
 *   - displaced_calendar_ritual_specialists: payer (powerless/trapped) — structural losers with no climb of their own
 *   - historical_sociologists_of_diffusion: observer (analytical/analytical) — reconstructs the pre-decree timeline
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(imposition_pathway_kernel__endogenous_climb_reading, 0.42).
domain_priors:suppression_score(imposition_pathway_kernel__endogenous_climb_reading, 0.38).
domain_priors:theater_ratio(imposition_pathway_kernel__endogenous_climb_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(imposition_pathway_kernel__endogenous_climb_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(imposition_pathway_kernel__endogenous_climb_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(imposition_pathway_kernel__endogenous_climb_reading, theater_ratio, 0.55).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(imposition_pathway_kernel__endogenous_climb_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(imposition_pathway_kernel__endogenous_climb_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(imposition_pathway_kernel__endogenous_climb_reading, tangled_rope).
narrative_ontology:human_readable(imposition_pathway_kernel__endogenous_climb_reading, "Endogenous Climb Reading of Commitment Displacement (Meiji Calendar/Dress Reform)").
narrative_ontology:topic_domain(imposition_pathway_kernel__endogenous_climb_reading, "historical_sociology/state_formation").

domain_priors:requires_active_enforcement(imposition_pathway_kernel__endogenous_climb_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(imposition_pathway_kernel__endogenous_climb_reading, 'b1d8aa1e-f99c-49d5-a68c-46abc645bb6d').
narrative_ontology:cs_kernel_codification('b1d8aa1e-f99c-49d5-a68c-46abc645bb6d', distributed).
narrative_ontology:cs_authority_grounding('b1d8aa1e-f99c-49d5-a68c-46abc645bb6d', practice).
narrative_ontology:cs_interpretation_layer_present('b1d8aa1e-f99c-49d5-a68c-46abc645bb6d').
narrative_ontology:cs_reading_relation('b1d8aa1e-f99c-49d5-a68c-46abc645bb6d', imposition_pathway_kernel__exogenous_override_reading, coexists_with).
narrative_ontology:cs_reading_relation('b1d8aa1e-f99c-49d5-a68c-46abc645bb6d', imposition_pathway_kernel__hybrid_cascade_reading, influences).
narrative_ontology:cs_axiom('b1d8aa1e-f99c-49d5-a68c-46abc645bb6d', foundational, displacement_is_never_originary).
narrative_ontology:cs_axiom_status(displacement_is_never_originary, holdable).
narrative_ontology:cs_axiom_grounding('b1d8aa1e-f99c-49d5-a68c-46abc645bb6d', displacement_is_never_originary, empirically_contingent).
narrative_ontology:cs_axiom('b1d8aa1e-f99c-49d5-a68c-46abc645bb6d', secondary, decree_visibility_is_archival_artifact).
narrative_ontology:cs_axiom_status(decree_visibility_is_archival_artifact, holdable).
narrative_ontology:cs_axiom_grounding('b1d8aa1e-f99c-49d5-a68c-46abc645bb6d', decree_visibility_is_archival_artifact, empirically_contingent).
narrative_ontology:cs_reference_frame('b1d8aa1e-f99c-49d5-a68c-46abc645bb6d', treaty_port_and_military_precedent_as_originating_authority).
narrative_ontology:cs_drift_state('b1d8aa1e-f99c-49d5-a68c-46abc645bb6d', post_decree_consolidation_1890, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('b1d8aa1e-f99c-49d5-a68c-46abc645bb6d', '').
narrative_ontology:cs_kernel_id(imposition_pathway_kernel__endogenous_climb_reading, imposition_pathway_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(imposition_pathway_kernel__endogenous_climb_reading, meiji_state_modernizers).
narrative_ontology:constraint_beneficiary(imposition_pathway_kernel__endogenous_climb_reading, treaty_port_merchant_class).
narrative_ontology:constraint_beneficiary(imposition_pathway_kernel__endogenous_climb_reading, military_modernization_faction).
narrative_ontology:constraint_victim(imposition_pathway_kernel__endogenous_climb_reading, traditionalist_rural_populations).
narrative_ontology:constraint_victim(imposition_pathway_kernel__endogenous_climb_reading, displaced_calendar_ritual_specialists).
narrative_ontology:constraint_victim(imposition_pathway_kernel__endogenous_climb_reading, domestic_textile_and_dress_artisans).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Issue the 1873 calendar decree and dress codes for officials and military, timing the formal decree to a moment when treaty-port commerce, merchant almanacs, and modernizing military units had already substantially adopted Western dates and dress. They present the decree as an initiating act of state capacity; on this reading it functions instead as ratification and acceleration of an already-climbing practice, letting the state claim credit for a transition it did not originate.
narrative_ontology:constraint_stakeholder(imposition_pathway_kernel__endogenous_climb_reading, meiji_state_modernizers, agenda_setter,
    institutional, generational, arbitrage, national).

% Adopted Western calendar dating and dress in commercial dealings with foreign trading houses years before the state decree, because it reduced friction in cross-border contracts and shipping schedules. Their pre-decree adoption is the fringe climb this reading identifies as the true origin point; they benefit from the eventual decree because it standardizes what they had already normalized, removing residual friction with non-adopting counterparties.
narrative_ontology:constraint_stakeholder(imposition_pathway_kernel__endogenous_climb_reading, treaty_port_merchant_class, beneficiary,
    organized, biographical, mobile, regional).

% Officers and units training with Western-style drill schedules and dress had already climbed toward Western timekeeping and uniform conventions before the formal decree, for interoperability with foreign advisors and imported equipment manuals. Their pre-existing practice supplied momentum the eventual decree formalized; they retain influence over how the ratification is enforced against holdout units.
narrative_ontology:constraint_stakeholder(imposition_pathway_kernel__endogenous_climb_reading, military_modernization_faction, beneficiary,
    organized, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(imposition_pathway_kernel__endogenous_climb_reading, military_modernization_faction, agenda_setter).

% Continued using the lunisolar calendar and traditional dress for agricultural and ritual purposes with no exposure to treaty-port commerce or military drill; the decree's enforcement (tax filings, school calendars, administrative dates) is experienced as a sudden imposition with no visible antecedent climb in their own communities. On this reading their experience of abruptness is real but locally indexed — the climb happened elsewhere and was invisible to them, which is precisely the mechanism the reading names.
narrative_ontology:constraint_stakeholder(imposition_pathway_kernel__endogenous_climb_reading, traditionalist_rural_populations, payer,
    powerless, biographical, trapped, local).

% Diviners and almanac-makers whose livelihood depended on the lunisolar calendar's ritual calculations lose income and social standing once state administration and then commerce require Western dating. They had no fringe stage of their own within which to climb toward the new system before it displaced their function; enforcement fell on them as pure loss.
narrative_ontology:constraint_stakeholder(imposition_pathway_kernel__endogenous_climb_reading, displaced_calendar_ritual_specialists, payer,
    powerless, biographical, trapped, local).

% Producers of traditional dress for officialdom see demand collapse once Western dress becomes mandatory for civil servants and military, following the pattern set earlier and voluntarily by treaty-port elites. They can partially retool toward Western tailoring but bear transition costs the earlier fringe-adopting merchant class had already absorbed without state pressure.
narrative_ontology:constraint_stakeholder(imposition_pathway_kernel__endogenous_climb_reading, domestic_textile_and_dress_artisans, payer,
    moderate, biographical, constrained, regional).

% Mid-level officials tasked with enforcing the calendar and dress decree in regions with no prior fringe adoption; they would testify that from where they administer, the change looks exactly like top-down imposition with no visible climb, and their implementation difficulties are read by the center as resistance rather than as evidence against the endogenous-climb account. Their perspective is structurally excluded from the historiography this reading privileges.
narrative_ontology:constraint_stakeholder(imposition_pathway_kernel__endogenous_climb_reading, provincial_administrators, excluded,
    moderate, biographical, constrained, regional).

% Reconstruct the pre-decree adoption timeline from merchant ledgers, treaty-port correspondence, and military training records to argue that the 1873 decree compressed and formalized a climb already underway rather than initiating displacement from zero. Their analytical position is what makes the fringe-stage visible after the fact, against the state's own self-presentation as originator.
narrative_ontology:constraint_stakeholder(imposition_pathway_kernel__endogenous_climb_reading, historical_sociologists_of_diffusion, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(imposition_pathway_kernel__endogenous_climb_reading, diffuse).
narrative_ontology:fixing_cost_class(imposition_pathway_kernel__endogenous_climb_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Standardizing calendar dates and dress conventions solves a genuine coordination problem for cross-border commerce, military interoperability, and administrative record-keeping — a single dating and dress system reduces transaction friction across treaty ports, foreign trade, and modernized military units.
% TRANSFER_FUNCTION: Moves social and economic standing from ritual specialists and traditional-dress artisans (whose function depended on the displaced system) to modernizing merchants, military officers, and state administrators who had already invested in the new system before it was mandated; moves administrative burden onto rural populations with no prior stake in either system.
% ABSENT_VOICES: Provincial administrators enforcing the decree in non-adopting regions, and the rural populations and ritual specialists who experienced no fringe stage at all, are absent from the historical record this reading privileges — that record is built substantially from merchant and military archives, which are precisely the archives documenting pre-decree climb.
% DISAPPEARANCE_RATIONALE: If the state decree had never issued, this reading holds that the underlying climb (treaty-port and military adoption) would have continued and likely reached comparable adoption levels through commercial and interoperability pressure alone, only more slowly and unevenly across regions with no direct treaty-port or military exposure — the world partially rearranges (delayed, patchier diffusion) rather than snapping back to the prior system, but rural areas with no fringe stage might never have converted absent the decree, which is exactly the contested part.
% FOUNDING_PROBLEM: Cross-border commercial and military friction from incompatible dating and dress conventions between Japan and Western trading/military partners, first felt and locally solved by treaty-port merchants and modernizing military units before the state formalized a national solution.
% FOUNDING_PROBLEM_CORROBORATION: Economic historians examining treaty-port merchant ledgers and foreign trading-house correspondence (outside both the Meiji state and the merchant class itself) corroborate pre-decree Western-calendar usage in commercial contracts predating the 1873 decree by several years; military historians examining training records independently corroborate pre-decree adoption in modernizing units. No corroboration exists from rural or ritual-specialist sources, who left the state's own administrative record as their primary trace.
narrative_ontology:disappearance_verdict(imposition_pathway_kernel__endogenous_climb_reading, contested).
narrative_ontology:founding_problem_status(imposition_pathway_kernel__endogenous_climb_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(imposition_pathway_kernel__endogenous_climb_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(imposition_pathway_kernel__endogenous_climb_reading, 'none', 1).
narrative_ontology:epsilon_provenance(imposition_pathway_kernel__endogenous_climb_reading, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(imposition_pathway_kernel__endogenous_climb_reading_tests).
:- end_tests(imposition_pathway_kernel__endogenous_climb_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises sharply at the 1873 decree point (0.15 to 0.40) because that is when the ratified system begins to impose real costs on populations who had no fringe stage of their own — the reading's claim is that the COORDINATION function was already substantially achieved by 1873 for the fringe-adopting groups, so the marginal extraction visible after 1873 falls almost entirely on non-adopting populations who are being folded into an already-negotiated settlement without having negotiated it. Theater ratio jumps at the decree point (0.15 to 0.62) because the state's public performance of the decree as an initiating, sovereign act is, on this reading, substantially theatrical relative to the actual causal history — the state is dramatizing an act of origination it did not perform. Suppression rises at the same point because enforcement against non-adopting rural populations and displaced specialists requires active administrative and, in places, coercive machinery that was never needed for the treaty-port and military fringe (who adopted voluntarily). The coercion grid shows accessibility_collapse and resistance rising fastest at the class and individual levels by 1890 — this is the mechanism by which people who experienced no climb (rural populations, ritual specialists) mount resistance that people who DID climb (merchants, officers) never needed to.
 *
 * PERSPECTIVAL GAP:
 *   Provincial administrators and rural populations would report the decree as unambiguous top-down imposition with zero antecedent — exactly the exogenous_override_reading's account. The endogenous_climb_reading's claim is that this local experience of abruptness is real but is a visibility artifact, not evidence against the underlying mechanism: the climb happened in archives (treaty ledgers, military training logs) these populations had no access to and no part in producing.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (merchant class, military faction, state modernizers) are exactly the groups who already climbed before the decree; the decree's ratification function costs them little because they had already absorbed transition costs voluntarily, and it removes remaining friction with non-adopters. Victims (rural populations, ritual specialists, domestic artisans) are structurally distinguished by having NO fringe stage available to them — no treaty-port exposure, no military modernization contact — so the decree lands on them as the reading's namesake 'compressed climb': from their vantage the entire multi-decade climb collapses into a single administrative moment.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (cross-border commercial/military friction from incompatible date/dress conventions) is dead by 1890 — full national standardization is achieved and the friction that drove treaty-port and military adoption no longer exists as a live problem. The enforcement apparatus built to ratify the decree (school curricula, administrative date requirements, dress codes for officials) persists past the point where fringe-driven completion would have occurred anyway, which is consistent with tangled_rope rather than pure rope: a real coordination function existed and was substantially self-completing, but the state's enforcement machinery captured credit and imposed additional costs on populations who would likely have converted on a slower, less coercive timeline absent the decree.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    endogenous_climb_universality_claim,
    'Is it true that EVERY apparent top-down commitment displacement, including this one, decomposes into a genuine antecedent fringe-adoption stage — or are there displacements (per exogenous_override_reading) where state capacity alone initiates change with no prior climb, and this reading''s universalism is itself a selection effect from cases where fringe evidence happens to survive in archives?',
    'Systematic archival search across multiple historical impositions for cases where no antecedent adoption evidence exists at all, even after exhaustive search — versus cases where absence of evidence is itself evidence of absence of surviving records rather than absence of a climb stage. Comparative work across cases with strong versus weak archival survival would help distinguish universal climb from selection artifact.',
    'If genuine no-antecedent cases are found, endogenous_climb_reading is falsified as a universal claim and must be narrowed to ''most'' rather than ''all'' displacements, converging toward hybrid_cascade_reading or ceding ground to exogenous_override_reading for those cases.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(endogenous_climb_universality_claim, empirical, 'Whether the reading''s universal claim survives scrutiny of cases with poor archival fringe evidence.').

omega_variable(
    which_reading_this_story_instantiates,
    'This story authors the endogenous_climb_reading specifically — one of three declared readings (endogenous_climb_reading, exogenous_override_reading, hybrid_cascade_reading) of the imposition_pathway_kernel applied to the same Meiji reform episode. What would change under the sibling readings?',
    'Author the sibling readings as separate constraint stories (exogenous_override_reading, hybrid_cascade_reading) with their own ε, beneficiary/victim structure, and type, linked via network.affects_constraints — per the ε-invariance decomposition principle.',
    'Under exogenous_override_reading, the 1873 decree would be authored as a genuine initiating act with its own ε profile (likely higher extraction, since no antecedent coordination absorbed the transition cost) and the state would be the sole agenda_setter with no pre-decree beneficiary fringe. Under hybrid_cascade_reading, the military_modernization_faction''s pre-decree adoption would be reclassified as an artificially state-created fringe (since it modernized under state sponsorship) rather than a genuinely independent climb, changing the beneficiary structure and the founding_problem narrative.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(which_reading_this_story_instantiates, conceptual, 'This story''s committer-frame identity: which reading of the shared kernel it instantiates and what the sibling readings would change.').

omega_variable(
    fringe_visibility_asymmetry,
    'The reading''s account of why rural populations perceive the change as sudden imposition depends on their fringe stage being genuinely invisible to them (occurring in treaty ports and military units they had no contact with) rather than genuinely absent. Is invisibility distinguishable from absence given the available archival record?',
    'Local-level records (village registers, temple records, local commerce records) from regions with no treaty-port or military exposure, searched specifically for any indirect Western-calendar contact (traveling merchants, itinerant officials) prior to 1873.',
    'If some indirect pre-decree contact is found even in rural areas, it strengthens the endogenous_climb_reading''s universal claim. If none is found despite thorough search, it supports treating rural adoption as a genuinely distinct, non-climb-derived imposition — undermining the reading''s claim that ALL displacement is compressed climb.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(fringe_visibility_asymmetry, empirical, 'Whether rural non-adoption reflects invisible-to-us climb or genuine absence of any pre-decree pathway.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(imposition_pathway_kernel__endogenous_climb_reading, 1859, 1890).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(impo_tr_t1859, imposition_pathway_kernel__endogenous_climb_reading, theater_ratio, 1859, 0.1).
narrative_ontology:measurement(impo_tr_t1865, imposition_pathway_kernel__endogenous_climb_reading, theater_ratio, 1865, 0.15).
narrative_ontology:measurement(impo_tr_t1873, imposition_pathway_kernel__endogenous_climb_reading, theater_ratio, 1873, 0.62).
narrative_ontology:measurement(impo_tr_t1878, imposition_pathway_kernel__endogenous_climb_reading, theater_ratio, 1878, 0.58).
narrative_ontology:measurement(impo_tr_t1884, imposition_pathway_kernel__endogenous_climb_reading, theater_ratio, 1884, 0.55).
narrative_ontology:measurement(impo_tr_t1890, imposition_pathway_kernel__endogenous_climb_reading, theater_ratio, 1890, 0.55).

% Extraction over time
narrative_ontology:measurement(impo_be_t1859, imposition_pathway_kernel__endogenous_climb_reading, base_extractiveness, 1859, 0.15).
narrative_ontology:measurement(impo_be_t1865, imposition_pathway_kernel__endogenous_climb_reading, base_extractiveness, 1865, 0.22).
narrative_ontology:measurement(impo_be_t1873, imposition_pathway_kernel__endogenous_climb_reading, base_extractiveness, 1873, 0.4).
narrative_ontology:measurement(impo_be_t1878, imposition_pathway_kernel__endogenous_climb_reading, base_extractiveness, 1878, 0.44).
narrative_ontology:measurement(impo_be_t1884, imposition_pathway_kernel__endogenous_climb_reading, base_extractiveness, 1884, 0.42).
narrative_ontology:measurement(impo_be_t1890, imposition_pathway_kernel__endogenous_climb_reading, base_extractiveness, 1890, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(impo_su_t1859, imposition_pathway_kernel__endogenous_climb_reading, suppression_requirement, 1859, 0.05).
narrative_ontology:measurement(impo_su_t1865, imposition_pathway_kernel__endogenous_climb_reading, suppression_requirement, 1865, 0.08).
narrative_ontology:measurement(impo_su_t1873, imposition_pathway_kernel__endogenous_climb_reading, suppression_requirement, 1873, 0.45).
narrative_ontology:measurement(impo_su_t1878, imposition_pathway_kernel__endogenous_climb_reading, suppression_requirement, 1878, 0.4).
narrative_ontology:measurement(impo_su_t1884, imposition_pathway_kernel__endogenous_climb_reading, suppression_requirement, 1884, 0.38).
narrative_ontology:measurement(impo_su_t1890, imposition_pathway_kernel__endogenous_climb_reading, suppression_requirement, 1890, 0.38).

% Leveled coercion grid (OQ-93): 32/32 authored points at t0=1859, tn=1890
narrative_ontology:measurement(impo_grid_01, imposition_pathway_kernel__endogenous_climb_reading, accessibility_collapse(class), 1859, 0.05).
narrative_ontology:measurement(impo_grid_02, imposition_pathway_kernel__endogenous_climb_reading, accessibility_collapse(class), 1890, 0.7).
narrative_ontology:measurement(impo_grid_03, imposition_pathway_kernel__endogenous_climb_reading, accessibility_collapse(individual), 1859, 0.02).
narrative_ontology:measurement(impo_grid_04, imposition_pathway_kernel__endogenous_climb_reading, accessibility_collapse(individual), 1890, 0.65).
narrative_ontology:measurement(impo_grid_05, imposition_pathway_kernel__endogenous_climb_reading, accessibility_collapse(organizational), 1859, 0.2).
narrative_ontology:measurement(impo_grid_06, imposition_pathway_kernel__endogenous_climb_reading, accessibility_collapse(organizational), 1890, 0.6).
narrative_ontology:measurement(impo_grid_07, imposition_pathway_kernel__endogenous_climb_reading, accessibility_collapse(structural), 1859, 0.1).
narrative_ontology:measurement(impo_grid_08, imposition_pathway_kernel__endogenous_climb_reading, accessibility_collapse(structural), 1890, 0.55).
narrative_ontology:measurement(impo_grid_09, imposition_pathway_kernel__endogenous_climb_reading, resistance(class), 1859, 0.03).
narrative_ontology:measurement(impo_grid_10, imposition_pathway_kernel__endogenous_climb_reading, resistance(class), 1890, 0.5).
narrative_ontology:measurement(impo_grid_11, imposition_pathway_kernel__endogenous_climb_reading, resistance(individual), 1859, 0.01).
narrative_ontology:measurement(impo_grid_12, imposition_pathway_kernel__endogenous_climb_reading, resistance(individual), 1890, 0.45).
narrative_ontology:measurement(impo_grid_13, imposition_pathway_kernel__endogenous_climb_reading, resistance(organizational), 1859, 0.05).
narrative_ontology:measurement(impo_grid_14, imposition_pathway_kernel__endogenous_climb_reading, resistance(organizational), 1890, 0.15).
narrative_ontology:measurement(impo_grid_15, imposition_pathway_kernel__endogenous_climb_reading, resistance(structural), 1859, 0.02).
narrative_ontology:measurement(impo_grid_16, imposition_pathway_kernel__endogenous_climb_reading, resistance(structural), 1890, 0.2).
narrative_ontology:measurement(impo_grid_17, imposition_pathway_kernel__endogenous_climb_reading, stakes_inflation(class), 1859, 0.05).
narrative_ontology:measurement(impo_grid_18, imposition_pathway_kernel__endogenous_climb_reading, stakes_inflation(class), 1890, 0.55).
narrative_ontology:measurement(impo_grid_19, imposition_pathway_kernel__endogenous_climb_reading, stakes_inflation(individual), 1859, 0.03).
narrative_ontology:measurement(impo_grid_20, imposition_pathway_kernel__endogenous_climb_reading, stakes_inflation(individual), 1890, 0.6).
narrative_ontology:measurement(impo_grid_21, imposition_pathway_kernel__endogenous_climb_reading, stakes_inflation(organizational), 1859, 0.1).
narrative_ontology:measurement(impo_grid_22, imposition_pathway_kernel__endogenous_climb_reading, stakes_inflation(organizational), 1890, 0.35).
narrative_ontology:measurement(impo_grid_23, imposition_pathway_kernel__endogenous_climb_reading, stakes_inflation(structural), 1859, 0.08).
narrative_ontology:measurement(impo_grid_24, imposition_pathway_kernel__endogenous_climb_reading, stakes_inflation(structural), 1890, 0.5).
narrative_ontology:measurement(impo_grid_25, imposition_pathway_kernel__endogenous_climb_reading, suppression(class), 1859, 0.02).
narrative_ontology:measurement(impo_grid_26, imposition_pathway_kernel__endogenous_climb_reading, suppression(class), 1890, 0.45).
narrative_ontology:measurement(impo_grid_27, imposition_pathway_kernel__endogenous_climb_reading, suppression(individual), 1859, 0.02).
narrative_ontology:measurement(impo_grid_28, imposition_pathway_kernel__endogenous_climb_reading, suppression(individual), 1890, 0.5).
narrative_ontology:measurement(impo_grid_29, imposition_pathway_kernel__endogenous_climb_reading, suppression(organizational), 1859, 0.08).
narrative_ontology:measurement(impo_grid_30, imposition_pathway_kernel__endogenous_climb_reading, suppression(organizational), 1890, 0.3).
narrative_ontology:measurement(impo_grid_31, imposition_pathway_kernel__endogenous_climb_reading, suppression(structural), 1859, 0.05).
narrative_ontology:measurement(impo_grid_32, imposition_pathway_kernel__endogenous_climb_reading, suppression(structural), 1890, 0.4).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(imposition_pathway_kernel__endogenous_climb_reading, identity_coordination).
narrative_ontology:affects_constraint(imposition_pathway_kernel__endogenous_climb_reading, exogenous_override_reading).
narrative_ontology:affects_constraint(imposition_pathway_kernel__endogenous_climb_reading, hybrid_cascade_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the imposition_pathway_kernel, all applied to the same Meiji calendar/dress reform episode. exogenous_override_reading authors the same historical material treating the 1873 decree as a genuine state-initiated displacement with no antecedent climb (different beneficiary/victim structure: no pre-decree merchant/military beneficiary fringe, state as sole originating agenda_setter). hybrid_cascade_reading authors the military and state-employee adoption as an artificially state-created fringe that only subsequently climbed organically, treating override and climb as sequential rather than climb being universally prior. All three share the underlying historical episode but instantiate structurally distinct constraints per the ε-invariance decomposition principle — each carries its own ε, beneficiary/victim declarations, and claimed_type, linked here via affects_constraints rather than merged into one story with an observable parameter.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
