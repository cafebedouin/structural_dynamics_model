% ============================================================================
% CONSTRAINT STORY: stone_land_use_rule__commemorative_husk
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-14
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_stone_land_use_rule__commemorative_husk, []).

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
 *   constraint_id: stone_land_use_rule__commemorative_husk
 *   human_readable: Commemorative Husk of the Shoreline Warning Stone
 *   domain: disaster_anthropology/institutional_memory/land_use_governance
 *
 * SUMMARY:
 *   A coastal town keeps an ancestral stone carved after the founding
 *   inundation; the carving instructs that no house be raised below the line
 *   the water reached. For two generations the instruction governed:
 *   settlement stayed uphill and deviation drew sanction from the founding
 *   families. Across the story interval the binding force drained away:
 *   state-built seawalls supplied engineered confidence, waterfront parcels
 *   gained commercial value, and the sanctioning practice lapsed with the
 *   generation that remembered the water. Today the stone stands in a tended
 *   memorial park between the seawall and a row of shops; the annual ceremony
 *   draws wreaths, officials, and tourists; the zoning map shows the parcels
 *   seaward of the stone as buildable. This file instantiates the
 *   commemorative_husk reading of the stone_land_use_rule kernel: the stone
 *   as memorial artifact whose warning has decayed to symbolic gesture
 *   without behavioral force. Under this reading the standing arrangement
 *   (husk stone, permissive zoning, reassurance calendar) is the thing under
 *   assessment, and it is substantially extractive: it transfers inundation
 *   risk onto people who cannot decline it and converts the founding dead's
 *   testimony into moral cover for the development their words forbid. The
 *   sibling reading (behavioral_competence) treats the same stone as a live
 *   prohibition enforced by daily spatial practice; that is a separate
 *   constraint with its own file, its own epsilon, and its own victim set.
 *   KEY AGENTS (by structural relationship): - waterfront_developers: primary
 *   beneficiary (powerful/arbitrage) — captures land-value uplift from
 *   buildable seaward parcels - municipal_council: agenda_setter
 *   (institutional/constrained) — administers zoning and ceremony, secondary
 *   beneficiary via tax base - descendant_memorial_association: agenda_setter
 *   of the memorial's meaning (organized/identity_locked) — supplies the
 *   interpretive layer - low_mobility_elderly_coastal_residents: primary
 *   target (powerless/trapped) — bears present exposure -
 *   future_hazard_zone_residents: target (powerless/constrained) — bears
 *   transferred exposure, absent by construction -
 *   heritage_tourism_operators: secondary beneficiary (moderate/mobile) -
 *   revival_coalition: excluded challenger (organized/constrained) -
 *   coastal_hazard_researchers: analytical observer — sees the full structure
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(stone_land_use_rule__commemorative_husk, 0.72).
domain_priors:suppression_score(stone_land_use_rule__commemorative_husk, 0.42).
domain_priors:theater_ratio(stone_land_use_rule__commemorative_husk, 0.78).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(stone_land_use_rule__commemorative_husk, extractiveness, 0.72).
narrative_ontology:constraint_metric(stone_land_use_rule__commemorative_husk, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(stone_land_use_rule__commemorative_husk, theater_ratio, 0.78).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(stone_land_use_rule__commemorative_husk, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(stone_land_use_rule__commemorative_husk, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(stone_land_use_rule__commemorative_husk, tangled_rope).
narrative_ontology:human_readable(stone_land_use_rule__commemorative_husk, "Commemorative Husk of the Shoreline Warning Stone").
narrative_ontology:topic_domain(stone_land_use_rule__commemorative_husk, "disaster_anthropology/institutional_memory/land_use_governance").

domain_priors:requires_active_enforcement(stone_land_use_rule__commemorative_husk).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(stone_land_use_rule__commemorative_husk, '4fe48418-c9c8-4f03-a5dc-056194107c19').
narrative_ontology:cs_kernel_codification('4fe48418-c9c8-4f03-a5dc-056194107c19', fixed_text).
narrative_ontology:cs_authority_grounding('4fe48418-c9c8-4f03-a5dc-056194107c19', lineage).
narrative_ontology:cs_interpretation_layer_present('4fe48418-c9c8-4f03-a5dc-056194107c19').
narrative_ontology:cs_reading_relation('4fe48418-c9c8-4f03-a5dc-056194107c19', stone_land_use_rule__behavioral_competence, influences).
narrative_ontology:cs_axiom('4fe48418-c9c8-4f03-a5dc-056194107c19', foundational, inscription_obligates_remembrance_not_zoning).
narrative_ontology:cs_axiom_status(inscription_obligates_remembrance_not_zoning, holdable).
narrative_ontology:cs_axiom_grounding('4fe48418-c9c8-4f03-a5dc-056194107c19', inscription_obligates_remembrance_not_zoning, conventional).
narrative_ontology:cs_axiom('4fe48418-c9c8-4f03-a5dc-056194107c19', secondary, engineered_defenses_supersede_carved_setback).
narrative_ontology:cs_axiom_status(engineered_defenses_supersede_carved_setback, holdable).
narrative_ontology:cs_axiom_grounding('4fe48418-c9c8-4f03-a5dc-056194107c19', engineered_defenses_supersede_carved_setback, instrumental).
narrative_ontology:cs_reference_frame('4fe48418-c9c8-4f03-a5dc-056194107c19', ancestral_commemorative_testament).
narrative_ontology:cs_drift_state('4fe48418-c9c8-4f03-a5dc-056194107c19', contemporary, gap(authority_erosion, minor, true)).
narrative_ontology:cs_created_at('4fe48418-c9c8-4f03-a5dc-056194107c19', '').
narrative_ontology:cs_kernel_id(stone_land_use_rule__commemorative_husk, stone_land_use_rule).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(stone_land_use_rule__commemorative_husk, waterfront_developers).
narrative_ontology:constraint_beneficiary(stone_land_use_rule__commemorative_husk, municipal_council).
narrative_ontology:constraint_beneficiary(stone_land_use_rule__commemorative_husk, heritage_tourism_operators).
narrative_ontology:constraint_victim(stone_land_use_rule__commemorative_husk, low_mobility_elderly_coastal_residents).
narrative_ontology:constraint_victim(stone_land_use_rule__commemorative_husk, future_hazard_zone_residents).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(stone_land_use_rule__commemorative_husk, descendant_memorial_association).
narrative_ontology:constraint_vindicates(stone_land_use_rule__commemorative_husk, engineered_defense_sufficiency_doctrine).
narrative_ontology:constraint_vindicates(stone_land_use_rule__commemorative_husk, commemorative_compliance_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Buy and assemble parcels between the old high-water line and the sea, ground the carving marks as forbidden. Their project financing depends on those parcels staying buildable; they fund opposition to setback ordinances, litigate retreat plans, and market the heritage-waterfront address. Capital moves inland easily if coastal politics turn hostile, and finished buildings can be sold.
narrative_ontology:constraint_stakeholder(stone_land_use_rule__commemorative_husk, waterfront_developers, beneficiary,
    powerful, immediate, arbitrage, regional).

% Administers the zoning map that permits building seaward of the stone, maintains the memorial park, and presides over the annual ceremony. Property taxes from the seaward parcels and civic standing from the remembrance calendar flow to it, while restrictive re-zoning would crater the assessment base it depends on. Council members answer to voters living on both sides of the line.
narrative_ontology:constraint_stakeholder(stone_land_use_rule__commemorative_husk, municipal_council, agenda_setter,
    institutional, biographical, constrained, local).
narrative_ontology:stakeholder_secondary_role(stone_land_use_rule__commemorative_husk, municipal_council, beneficiary).

% Families of the founding disaster who tend the stone, polish it, lay the wreaths, and teach the story in schools. Stewardship is an inherited office: to stop tending would feel like abandoning the dead. Most members read the carving as a charge to remember rather than a rule to enforce, and several live on ground their great-grandparents ruled out.
narrative_ontology:constraint_stakeholder(stone_land_use_rule__commemorative_husk, descendant_memorial_association, agenda_setter,
    organized, generational, identity_locked, local).
narrative_ontology:stakeholder_secondary_role(stone_land_use_rule__commemorative_husk, descendant_memorial_association, beneficiary).

% Longtime townspeople, many descended from founding families, whose homes sit on the low ground the inscription warns about. Family graves, pensions, and unsellable equity tie them to houses inside the modeled inundation reach. They cannot relocate and cannot adequately insure; they attend the ceremony as mourners and live behind the seawall as its implicit guarantors.
narrative_ontology:constraint_stakeholder(stone_land_use_rule__commemorative_husk, low_mobility_elderly_coastal_residents, payer,
    powerless, biographical, trapped, local).

% The people who will occupy the seaward buildings over coming decades: buyers, renters, and workers in the waterfront shops. None existed when the current zoning was set and none attends the hearings that allocate their exposure. They arrive into a landscape where the visible stone suggests the danger is remembered and handled.
narrative_ontology:constraint_stakeholder(stone_land_use_rule__commemorative_husk, future_hazard_zone_residents, payer,
    powerless, generational, constrained, regional).

% Run guesthouses, ferry stops, and guided walks built around the stone as the town's signature site. Revenue follows the memorial calendar. Operators rebrand to the next heritage site without difficulty if the town's story changes.
narrative_ontology:constraint_stakeholder(stone_land_use_rule__commemorative_husk, heritage_tourism_operators, beneficiary,
    moderate, immediate, mobile, regional).

% Survivors of the most recent flood, younger descendants, and a few planners campaigning to restore a binding setback at the stone. They tabled a managed-retreat plan after the second flood and lost to litigation and budget arithmetic. Between crises they sit outside the zoning process, consulted late and overruled.
narrative_ontology:constraint_stakeholder(stone_land_use_rule__commemorative_husk, revival_coalition, excluded,
    organized, generational, constrained, local).

% University and national-institute teams who map the inundation reach, survey what residents believe the stone means, and publish on why markers stop governing behavior. They hold no vote; their maps enter the record and are set aside when assessments are struck.
narrative_ontology:constraint_stakeholder(stone_land_use_rule__commemorative_husk, coastal_hazard_researchers, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(stone_land_use_rule__commemorative_husk, waterfront_developers).
narrative_ontology:fixing_cost_class(stone_land_use_rule__commemorative_husk, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains a shared focal point for communal remembrance of the founding disaster and, historically, coordinated settlement away from the inundation zone. Today it coordinates the remembrance calendar, heritage visitation, and civic identity, while settlement decisions route through ordinary market channels.
% TRANSFER_FUNCTION: Moves inundation risk onto current low-mobility residents and future occupants of seaward parcels, and moves the moral authority of the founding dead behind present development decisions; moves land-value gains to waterfront owners, tax receipts to the municipality, and visitor revenue to tourism operators.
% ABSENT_VOICES: Future occupants of the seaward parcels are absent by construction: the people the warning was carved for cannot attend the zoning hearing. The founding dead are present only as a text they cannot revise. Low-income households priced out of safer ground appear as petitioners, not seats.
% DISAPPEARANCE_RATIONALE: If the arrangement vanished overnight (stone removed, ceremonies ended, reassurance calendar dropped), the town would lose its remembrance calendar and heritage draw, and the removal of the reassurance theater would expose the unprotected seaward zone, forcing either rapid re-regulation or visible abandonment of the shoreline economy. Zoning expectations, tourism revenue, and the association's custodial office all depend on the arrangement continuing as it is.
% FOUNDING_PROBLEM: After the founding inundation destroyed the shoreline settlement, survivors needed a durable way to keep future settlement off the low ground: a rule that would outlive living memory of how far the water reached.
% FOUNDING_PROBLEM_CORROBORATION: Survivor-descendant associations and coastal-hazard researchers attest the founding problem is live and unmet, citing inundation models that place occupied seaward parcels inside the mapped reach; the municipal council and development interests attest the problem is managed by engineered defenses, making further land-use restriction redundant. Disinterested corroboration exists (national hazard mapping, peer-reviewed surge studies) and supports the live-and-unmet reading; no source outside the benefiting parties attests the problem is solved.
narrative_ontology:disappearance_verdict(stone_land_use_rule__commemorative_husk, world_rearranges).
narrative_ontology:founding_problem_status(stone_land_use_rule__commemorative_husk, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(stone_land_use_rule__commemorative_husk, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(stone_land_use_rule__commemorative_husk, 'none', 1).
narrative_ontology:epsilon_provenance(stone_land_use_rule__commemorative_husk, 0.72, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(stone_land_use_rule__commemorative_husk_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(stone_land_use_rule__commemorative_husk, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(stone_land_use_rule__commemorative_husk_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Epsilon is high (0.72) because the referent is the standing husk arrangement assessed by this reading's own lights: the arrangement transfers disaster exposure onto non-consenting present and future residents while harvesting the dead's authority as cover, and the reassurance machinery actively lowers demand for the protective practice the carving ordered. Suppression is moderate (0.42) and falling: the series tracks the death of prohibitive enforcement (sanctions, taboos, interim ordinances) with a small counter-current of defensive maintenance (litigation against retreat plans, agenda control over zoning) that keeps the floor from reaching zero. Theater is high and rising (0.78) and is load-bearing rather than vestigial: the ceremony is the community's recurring proof to itself that the hazard is handled, which is precisely what subsidizes seaward building; that functional theater is why the claim is tangled_rope rather than piton despite the ratio. Accessibility_collapse is 0.48: revival ordinances and retreat programs were repeatedly proposed and collapsed, the engineered substitute (seawall) absorbs part of the alternative space, and individual exit exists only for mobile owners, not equity-poor elders. Resistance is 0.5: two organized revival windows and continuous scholarly critique, cyclically deflated. The measurement series runs on one shared nine-point grid and shows two full crisis-reform-relaxation-accumulation cycles (dips at T=33 and T=66 following the two floods); the oscillation is itself part of the mechanism, because each failed reform window teaches proponents that challenge is futile and each recovery re-prices the seaward parcels higher than before.
 *
 * PERSPECTIVAL GAP:
 *   From the descendant association's seat the arrangement is faithful custody: tending the stone is the duty, and the duty is being performed. From the coastal_hazard_researchers' seat it is a reassurance machine that measures well and protects poorly. From the waterfront_developers' seat it is cleared title with a photogenic backdrop. From the low_mobility_elderly_coastal_residents' seat it is a promise the town made to their great-grandparents and quietly broke while charging admission to the apology. Identity-lock dynamics concentrate in the association: the fusion is relational and ideological at once (self-concept constituted through custodial duty to the dead, plus a worldview in which remembering and honoring are the same act). If that frame broke, for instance through a survivor-led reframing of custody as complicity, the association would flip from supplier of the interpretive layer to internal opponent, and the arrangement's enforcement cost would rise sharply. Suppression splits roughly 60 percent structural (seawall confidence effects, insurance and assessment regimes, agenda control, litigation costs) and 40 percent internalized (normalized proximity, fatalism, commemorative satisfaction substituting for compliance); the omega variable carries the uncertainty.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive the derivation: waterfront_developers (arbitrage exit) and heritage_tourism_operators (mobile) sit near the full-beneficiary end, and municipal_council derives low d from its beneficiary secondary role despite administering the arrangement. Victim declarations drive the targets: low_mobility_elderly_coastal_residents (trapped) sit nearest the full-target end, and future_hazard_zone_residents (constrained, no seat in the process) sit nearly as high because nothing modulates their exposure. The descendant_memorial_association is the interesting seat: beneficiary-declared and identity_locked, it collects meaning and standing rather than money, so its derived d is low even though its members personally share the physical exposure they help obscure. No directionality overrides were needed; the beneficiary/victim plus exit data produce the right relationships.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (recurring inundation) is live; what atrophied is the arrangement's response to it. The R5 mismatch consumer reads founding_problem_status=contested against disappearance_verdict=world_rearranges and correctly declines to fire the zombie flag: the hazard is real, so the arrangement's persistence is not inertia around a dead problem but active maintenance around a live one it no longer serves. The tangled_rope claim prevents two mislabelings at once: a flat reading would call the stone a rope because it plainly coordinates remembrance and identity, and a cynical reading would call the husk a piton because the dominant activity around the stone is performance. Both miss the coupling: the performance is the extraction's enabling condition, and the coordination core is what makes the cover credible. Coalition check: the powerless victim classes did attempt coalition (elders plus revival_coalition plus researchers produced the T=66 managed-retreat plan) and were defeated by developer litigation plus the council's fiscal dependence on seaward assessments; the defeat is what consolidated the current high-theater steady state.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indexicality,
    'This constraint is one reading (commemorative_husk) of the stone_land_use_rule kernel: does the carved warning currently bind land use, or is it a memorial artifact without behavioral force? The sibling reading (behavioral_competence) holds the stone is a live prohibition enforced by daily spatial practice.',
    'Behavioral audit of siting decisions against the carved line: if recent seaward approvals and occupancies track the stone''s location at chance levels, the husk reading is confirmed; if deviations from the line are systematically sanctioned or avoided, the sibling reading is live.',
    'The readings assign different victim sets and different epsilon: under behavioral_competence, seaward builders are transgressors of a live rule and extraction is near-coordination-cost; under commemorative_husk, no one transgresses because nothing binds, and the extraction lands on non-consenting exposed populations. Classification of every seat flips with the resolution.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_indexicality, conceptual, 'Which reading of the stone kernel is instantiated: binding rule versus commemorative husk.').

omega_variable(
    reassurance_net_effect,
    'Is the stone''s present operation net-reassuring (raising aggregate exposure by damping vigilance, the levee/safe-development effect) or net-warning (some residents and visitors still internalize the line)?',
    'Before/after hazard-cognition surveys around ceremony dates and controlled comparison with comparable towns lacking a marker; siting-choice analysis for newcomers exposed to the memorial narrative versus those not.',
    'If net-reassuring, the husk is worse than removal and the authored epsilon understates the harm; if net-warning, part of the arrangement retains protective function and epsilon is overstated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reassurance_net_effect, empirical, 'Whether the decayed marker still warns or now mainly reassures.').

omega_variable(
    suppression_mechanism_split,
    'Is the measured suppression primarily structural (seawall confidence effects, insurance and assessment regimes, agenda control, litigation costs) or internalized (normalized proximity, fatalism, commemorative satisfaction substituting for compliance)?',
    'Post-revival-attempt attitude panels: if support for a binding setback rises sharply when procedural barriers drop, the suppression was structural; if support stays low, a large internalized share is carrying it.',
    'If largely internalized, institutional reform alone will not restore the protective practice and the arrangement''s persistence is more robust than the structural measure suggests; if largely structural, removing the procedural blockers revives the sibling reading''s conditions.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_split, empirical, 'Structural versus internalized share of the arrangement''s suppressive force.').

omega_variable(
    enforcement_decay_attribution,
    'Was the decay of the stone''s binding force inherent to the enforcement technology (a carved line with no standing enforcement institution cannot survive its founding generation) or contingent on this town''s development pressure and seawall subsidy?',
    'Comparative archival study of warning-stone communities with and without waterfront development pressure and state-engineered defenses; survival analysis of marker-governed setbacks across sites.',
    'If decay is inherent, the behavioral_competence reading was never robustly available and the husk is the technology''s terminal state; if contingent, the sibling reading remains recoverable under different institutional support, changing the remedial implications of the whole classification.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(enforcement_decay_attribution, empirical, 'Whether the husk outcome was structurally fated or locally contingent.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(stone_land_use_rule__commemorative_husk, 0, 90).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ston_tr_t0, stone_land_use_rule__commemorative_husk, theater_ratio, 0, 0.08).
narrative_ontology:measurement_basis(ston_tr_t0, observed).
narrative_ontology:measurement(ston_tr_t12, stone_land_use_rule__commemorative_husk, theater_ratio, 12, 0.16).
narrative_ontology:measurement_basis(ston_tr_t12, observed).
narrative_ontology:measurement(ston_tr_t24, stone_land_use_rule__commemorative_husk, theater_ratio, 24, 0.28).
narrative_ontology:measurement_basis(ston_tr_t24, observed).
narrative_ontology:measurement(ston_tr_t33, stone_land_use_rule__commemorative_husk, theater_ratio, 33, 0.22).
narrative_ontology:measurement_basis(ston_tr_t33, observed).
narrative_ontology:measurement(ston_tr_t45, stone_land_use_rule__commemorative_husk, theater_ratio, 45, 0.4).
narrative_ontology:measurement_basis(ston_tr_t45, observed).
narrative_ontology:measurement(ston_tr_t57, stone_land_use_rule__commemorative_husk, theater_ratio, 57, 0.58).
narrative_ontology:measurement_basis(ston_tr_t57, observed).
narrative_ontology:measurement(ston_tr_t66, stone_land_use_rule__commemorative_husk, theater_ratio, 66, 0.3).
narrative_ontology:measurement_basis(ston_tr_t66, observed).
narrative_ontology:measurement(ston_tr_t78, stone_land_use_rule__commemorative_husk, theater_ratio, 78, 0.68).
narrative_ontology:measurement_basis(ston_tr_t78, observed).
narrative_ontology:measurement(ston_tr_t90, stone_land_use_rule__commemorative_husk, theater_ratio, 90, 0.78).
narrative_ontology:measurement_basis(ston_tr_t90, observed).

% Extraction over time
narrative_ontology:measurement(ston_be_t0, stone_land_use_rule__commemorative_husk, base_extractiveness, 0, 0.18).
narrative_ontology:measurement_basis(ston_be_t0, observed).
narrative_ontology:measurement(ston_be_t12, stone_land_use_rule__commemorative_husk, base_extractiveness, 12, 0.3).
narrative_ontology:measurement_basis(ston_be_t12, observed).
narrative_ontology:measurement(ston_be_t24, stone_land_use_rule__commemorative_husk, base_extractiveness, 24, 0.46).
narrative_ontology:measurement_basis(ston_be_t24, observed).
narrative_ontology:measurement(ston_be_t33, stone_land_use_rule__commemorative_husk, base_extractiveness, 33, 0.38).
narrative_ontology:measurement_basis(ston_be_t33, observed).
narrative_ontology:measurement(ston_be_t45, stone_land_use_rule__commemorative_husk, base_extractiveness, 45, 0.55).
narrative_ontology:measurement_basis(ston_be_t45, observed).
narrative_ontology:measurement(ston_be_t57, stone_land_use_rule__commemorative_husk, base_extractiveness, 57, 0.63).
narrative_ontology:measurement_basis(ston_be_t57, observed).
narrative_ontology:measurement(ston_be_t66, stone_land_use_rule__commemorative_husk, base_extractiveness, 66, 0.52).
narrative_ontology:measurement_basis(ston_be_t66, observed).
narrative_ontology:measurement(ston_be_t78, stone_land_use_rule__commemorative_husk, base_extractiveness, 78, 0.67).
narrative_ontology:measurement_basis(ston_be_t78, observed).
narrative_ontology:measurement(ston_be_t90, stone_land_use_rule__commemorative_husk, base_extractiveness, 90, 0.72).
narrative_ontology:measurement_basis(ston_be_t90, observed).

% Suppression requirement over time
narrative_ontology:measurement(ston_su_t0, stone_land_use_rule__commemorative_husk, suppression_requirement, 0, 0.8).
narrative_ontology:measurement_basis(ston_su_t0, observed).
narrative_ontology:measurement(ston_su_t12, stone_land_use_rule__commemorative_husk, suppression_requirement, 12, 0.68).
narrative_ontology:measurement_basis(ston_su_t12, observed).
narrative_ontology:measurement(ston_su_t24, stone_land_use_rule__commemorative_husk, suppression_requirement, 24, 0.58).
narrative_ontology:measurement_basis(ston_su_t24, observed).
narrative_ontology:measurement(ston_su_t33, stone_land_use_rule__commemorative_husk, suppression_requirement, 33, 0.64).
narrative_ontology:measurement_basis(ston_su_t33, observed).
narrative_ontology:measurement(ston_su_t45, stone_land_use_rule__commemorative_husk, suppression_requirement, 45, 0.5).
narrative_ontology:measurement_basis(ston_su_t45, observed).
narrative_ontology:measurement(ston_su_t57, stone_land_use_rule__commemorative_husk, suppression_requirement, 57, 0.46).
narrative_ontology:measurement_basis(ston_su_t57, observed).
narrative_ontology:measurement(ston_su_t66, stone_land_use_rule__commemorative_husk, suppression_requirement, 66, 0.6).
narrative_ontology:measurement_basis(ston_su_t66, observed).
narrative_ontology:measurement(ston_su_t78, stone_land_use_rule__commemorative_husk, suppression_requirement, 78, 0.44).
narrative_ontology:measurement_basis(ston_su_t78, observed).
narrative_ontology:measurement(ston_su_t90, stone_land_use_rule__commemorative_husk, suppression_requirement, 90, 0.42).
narrative_ontology:measurement_basis(ston_su_t90, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(stone_land_use_rule__commemorative_husk, identity_coordination).
narrative_ontology:affects_constraint(stone_land_use_rule__commemorative_husk, stone_land_use_rule__behavioral_competence).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition per the epsilon-invariance principle: the colloquial label 'the warning stone' covers two structurally distinct claims. stone_land_use_rule__behavioral_competence (upstream) asserts the stone is a live land-use prohibition with negligible extraction; this story (downstream) asserts the stone is a commemorative husk whose arrangement transfers risk and harvests the dead's authority, with high epsilon. The upstream claim is the one cited as evidence ('the stone protected us for generations'); this reading contests its present currency. Each file links the other via network.affects_constraints; contamination propagates in both directions, since a verified revival of binding force dissolves this story's victim set, and a verified husk dissolves the sibling's.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
