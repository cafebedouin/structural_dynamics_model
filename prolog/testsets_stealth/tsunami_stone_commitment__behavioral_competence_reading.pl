% ============================================================================
% CONSTRAINT STORY: tsunami_stone_commitment__behavioral_competence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_tsunami_stone_commitment__behavioral_competence_reading, []).

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
 *   constraint_id: tsunami_stone_commitment__behavioral_competence_reading
 *   human_readable: Sanriku Tsunami Warning Stones — Live Norm Enforcement (Behavioral Competence Reading)
 *   domain: disaster_anthropology/institutional_memory/commitment_systems
 *
 * SUMMARY:
 *   On the Sanriku coast of northeastern Japan, stone stelae erected after
 *   the 1896 Meiji Great Sanriku tsunami — and re-erected or re-inscribed
 *   after 1933 — mark the high-water lines of past inundations with
 *   injunctions to build above them and to remember the calamity. This story
 *   instantiates the behavioral_competence_reading of the tsunami-stone
 *   kernel: for most of the interval the inscription operated as a live
 *   normative institution, its force carried not by the carved characters
 *   alone but by an intergenerational transmission apparatus — elder
 *   testimony, annual observances, place-name memory, siting discipline —
 *   that kept the warning behaviorally active across generations with no
 *   living witness of the last wave. Settlements that kept the discipline
 *   evacuated in time in March 2011; this reading credits the institution,
 *   not geology alone, for the difference. Per the kernel rules, the contest
 *   with the sibling readings is routed to omega variables and the network
 *   note, not argued inside this constraint. Claim and metrics are authored
 *   independently: claimed_type 'piton' states my structural belief that the
 *   arrangement is now a stabilized remnant of a formerly fully functional
 *   coordination mechanism — negligible extraction, no rent-collector,
 *   maintained past the point where its behavioral work is largely done —
 *   while the metric series honestly records the end-state atrophy (rising
 *   ceremonial share, falling enforcement capacity) that this reading's own
 *   reference frame predicts.
 *
 * KEY AGENTS:
 *   - - village_elder_transmitters: Agenda-setting custodians (moderate/identity_locked) — administer testimony and observance; bear the transmission labor
 *   - - sanriku_coastal_communities: Principal historical beneficiaries and continuing cost-bearers (organized/constrained)
 *   - - municipal_governments: Current administrative seat (institutional/mobile) — could alter or abandon the arrangement at will
 *   - - descendant_households_of_high_sited_settlements: Holders of the banked safety dividend (moderate/mobile)
 *   - - postwar_coastal_developers_and_newcomers: Defectors and cost-bearers outside the benefit circuit (powerful/arbitrage)
 *   - - national_engineering_agencies: Excluded substitute-providers whose seawalls and maps displace the memory discipline (institutional/mobile)
 *   - - disaster_memory_researchers: Analytical observers — see the full structure and publish the comparative record (analytical/analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(tsunami_stone_commitment__behavioral_competence_reading, 0.09).
domain_priors:suppression_score(tsunami_stone_commitment__behavioral_competence_reading, 0.09).
domain_priors:theater_ratio(tsunami_stone_commitment__behavioral_competence_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(tsunami_stone_commitment__behavioral_competence_reading, extractiveness, 0.09).
narrative_ontology:constraint_metric(tsunami_stone_commitment__behavioral_competence_reading, suppression_requirement, 0.09).
narrative_ontology:constraint_metric(tsunami_stone_commitment__behavioral_competence_reading, theater_ratio, 0.55).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(tsunami_stone_commitment__behavioral_competence_reading, accessibility_collapse, 0.25).
narrative_ontology:constraint_metric(tsunami_stone_commitment__behavioral_competence_reading, resistance, 0.42).

% --- Constraint claim ---
narrative_ontology:constraint_claim(tsunami_stone_commitment__behavioral_competence_reading, piton).
narrative_ontology:human_readable(tsunami_stone_commitment__behavioral_competence_reading, "Sanriku Tsunami Warning Stones — Live Norm Enforcement (Behavioral Competence Reading)").
narrative_ontology:topic_domain(tsunami_stone_commitment__behavioral_competence_reading, "disaster_anthropology/institutional_memory/commitment_systems").

domain_priors:requires_active_enforcement(tsunami_stone_commitment__behavioral_competence_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(tsunami_stone_commitment__behavioral_competence_reading, '23034d3f-2ac7-4f69-918d-69aaa841294b').
narrative_ontology:cs_kernel_codification('23034d3f-2ac7-4f69-918d-69aaa841294b', fixed_text).
narrative_ontology:cs_authority_grounding('23034d3f-2ac7-4f69-918d-69aaa841294b', lineage).
narrative_ontology:cs_interpretation_layer_present('23034d3f-2ac7-4f69-918d-69aaa841294b').
narrative_ontology:cs_reading_relation('23034d3f-2ac7-4f69-918d-69aaa841294b', tsunami_stone_commitment__commemorative_husk_reading, coexists_with).
narrative_ontology:cs_reading_relation('23034d3f-2ac7-4f69-918d-69aaa841294b', tsunami_stone_commitment__catastrophe_validation_axis, influences).
narrative_ontology:cs_axiom('23034d3f-2ac7-4f69-918d-69aaa841294b', foundational, inscription_constitutes_binding_norm).
narrative_ontology:cs_axiom_status(inscription_constitutes_binding_norm, holdable).
narrative_ontology:cs_axiom_grounding('23034d3f-2ac7-4f69-918d-69aaa841294b', inscription_constitutes_binding_norm, conventional).
narrative_ontology:cs_axiom('23034d3f-2ac7-4f69-918d-69aaa841294b', foundational, intergenerational_transmission_is_enforcement).
narrative_ontology:cs_axiom_status(intergenerational_transmission_is_enforcement, holdable).
narrative_ontology:cs_axiom_grounding('23034d3f-2ac7-4f69-918d-69aaa841294b', intergenerational_transmission_is_enforcement, empirically_contingent).
narrative_ontology:cs_reference_frame('23034d3f-2ac7-4f69-918d-69aaa841294b', inscription_as_standing_norm).
narrative_ontology:cs_drift_state('23034d3f-2ac7-4f69-918d-69aaa841294b', contemporary_post_2011_reconstruction, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('23034d3f-2ac7-4f69-918d-69aaa841294b', '').
narrative_ontology:cs_kernel_id(tsunami_stone_commitment__behavioral_competence_reading, tsunami_stone_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(tsunami_stone_commitment__behavioral_competence_reading, sanriku_coastal_communities).
narrative_ontology:constraint_beneficiary(tsunami_stone_commitment__behavioral_competence_reading, descendant_households_of_high_sited_settlements).
narrative_ontology:constraint_beneficiary(tsunami_stone_commitment__behavioral_competence_reading, disaster_memory_institutions).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(tsunami_stone_commitment__behavioral_competence_reading, sanriku_coastal_communities).
narrative_ontology:constraint_victim(tsunami_stone_commitment__behavioral_competence_reading, postwar_coastal_developers_and_newcomers).
narrative_ontology:constraint_vindicates(tsunami_stone_commitment__behavioral_competence_reading, landscape_inscribed_hazard_memory).
narrative_ontology:constraint_vindicates(tsunami_stone_commitment__behavioral_competence_reading, precautionary_siting_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Village and town settlements along the Sanriku coast that keep the observances, fund upkeep of the markers, and restrain building near the shore in line with the inscriptions. Over the interval they received the largest safety dividends — settlements that kept the discipline evacuated quickly in 2011 — while bearing the recurring labor of transmission and, after the postwar boom, the opportunity cost of shoreline land left undeveloped. Leaving is possible but costly: households are tied to fishing grounds, family graves, and mutual-aid networks.
narrative_ontology:constraint_stakeholder(tsunami_stone_commitment__behavioral_competence_reading, sanriku_coastal_communities, payer,
    organized, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(tsunami_stone_commitment__behavioral_competence_reading, sanriku_coastal_communities, beneficiary).

% Older residents who carry the testimony — who saw, or heard from those who saw, the black water — and who lead the annual gatherings at the stones, judge when a warning is urgent, and press younger families to site homes above the old lines. Their standing in the community rests on this custodial office; stepping away from it would mean becoming an ordinary elder with no distinctive role. Each cohort hands the duty to the next, and where outmigration broke the chain, the duty lapsed with no one left to hold it.
narrative_ontology:constraint_stakeholder(tsunami_stone_commitment__behavioral_competence_reading, village_elder_transmitters, agenda_setter,
    moderate, biographical, identity_locked, local).

% Town and city administrations that now own the arrangement's upkeep: heritage registration for the stelae, preservation budgets, drill scheduling, and the post-2011 programs that pair the stones with sirens and hazard maps. They could de-register, defund, or relocate the markers at will; nothing obliges them to continue. What they get back is modest: heritage-designation revenue, civic identity, and documented diligence in disaster-prevention planning.
narrative_ontology:constraint_stakeholder(tsunami_stone_commitment__behavioral_competence_reading, municipal_governments, agenda_setter,
    institutional, generational, mobile, regional).

% Families whose grandparents obeyed the inscriptions and built on the terraces above the inundation line. In 2011 their houses stood, their evacuation distances were short, and the family story of obedience became a local credential. They hold the arrangement's accumulated dividend; their continuing contribution is retelling the story and keeping the terrace lots in the family rather than selling to shoreward development.
narrative_ontology:constraint_stakeholder(tsunami_stone_commitment__behavioral_competence_reading, descendant_households_of_high_sited_settlements, beneficiary,
    moderate, generational, mobile, local).

% Firms and incoming residents who arrived after the transmission circuits formed and treated the markers as historical sentiment rather than instruction. They built factories, homes, and guesthouses on the low ground — some swept away in 2011 — and they bear the arrangement's costs as forgone developable parcels wherever the siting discipline still bites. Their capital is mobile: a parcel below the line refused here can be taken up in a prefecture with no such line.
narrative_ontology:constraint_stakeholder(tsunami_stone_commitment__behavioral_competence_reading, postwar_coastal_developers_and_newcomers, payer,
    powerful, immediate, arbitrage, regional).

% Ministry engineers and construction consortia who answered 2011 with concrete: seawalls, elevated roads, and revised hazard maps. They were never part of the observance circuit and plan in a separate professional conversation where the stones figure as historical data points. Their works substitute for the memory discipline at enormous budget scale, and their choices — wall heights, map lines — now set how much remaining behavioral work the inscriptions are asked to do.
narrative_ontology:constraint_stakeholder(tsunami_stone_commitment__behavioral_competence_reading, national_engineering_agencies, excluded,
    institutional, generational, mobile, national).

% Ethnographers, historians, and hazard scientists who inventory the stelae, interview transmitters, and compare survival outcomes across settlements. They publish the casualty-geography findings that outside agencies cite, advise municipalities on preservation, and supply the comparative record on which any verdict about the arrangement's effectiveness ultimately rests.
narrative_ontology:constraint_stakeholder(tsunami_stone_commitment__behavioral_competence_reading, disaster_memory_researchers, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(tsunami_stone_commitment__behavioral_competence_reading, diffuse).
narrative_ontology:fixing_cost_class(tsunami_stone_commitment__behavioral_competence_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the intergenerational-hazard-memory problem: a tsunami recurs on timescales longer than individual memory, so each generation must transfer siting discipline and evacuation readiness to the next without a living witness of the last event. The stones fix the high-water datum in the landscape; the transmission apparatus — testimony, annual observances, place-names, school lessons — keeps the datum behaviorally active.
% TRANSFER_FUNCTION: Moves restraint and credence across generations: each generation forgoes shoreline development below the marked line and transfers testimony-plus-obligation to its successors; successors inherit a standing evacuation disposition and a pre-committed safe-siting boundary. After 2011 it also moves municipal budget from general funds into preservation and drill programming.
% ABSENT_VOICES: The drowned of 1896, 1933, and 2011 cannot testify. Newcomers and firms who arrived after the transmission circuits formed were never addressed by the norm and did not sit in the observances where its obligations were renewed; national engineers planned protection in a separate conversation where the stones appeared as curiosities. Their absence explains why unanimity inside the ritual circle overstated the coast-wide reach of the discipline.
% DISAPPEARANCE_RATIONALE: Settlement siting patterns, annual observance calendars, municipal heritage budgets, school curricula, and evacuation-drill designs all reference the stones; post-2011 reconstruction debates cited them as the baseline datum. Overnight disappearance would strand the coast without its longest-horizon hazard record and sever the transmission chains that 2011 showed still save lives where intact.
% FOUNDING_PROBLEM: After the 1896 Meiji Sanriku tsunami killed roughly twenty-two thousand people — many on ground rebuilt after an earlier inundation — survivors faced a problem no living memory could solve: making a once-in-three-generations catastrophe govern everyday siting and escape behavior. Carved stones at the inundation line, backed by testimony and observance, were the chosen technology.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the beneficiary set: post-2011 casualty-geography studies by university teams and engineering-society reviews found faster evacuation and lower death rates in settlements with intact transmission practice; the Geological Survey of Japan's tsunami-deposit mapping independently establishes recurrence intervals that keep the founding problem open; municipal reconstruction ordinances in towns without stone traditions adopted equivalent memory-institution provisions, attesting the problem's persistence without reference to any village's self-account.
narrative_ontology:disappearance_verdict(tsunami_stone_commitment__behavioral_competence_reading, world_rearranges).
narrative_ontology:founding_problem_status(tsunami_stone_commitment__behavioral_competence_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(tsunami_stone_commitment__behavioral_competence_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(tsunami_stone_commitment__behavioral_competence_reading, 'none', 1).
narrative_ontology:epsilon_provenance(tsunami_stone_commitment__behavioral_competence_reading, 0.09, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(tsunami_stone_commitment__behavioral_competence_reading_tests).
:- end_tests(tsunami_stone_commitment__behavioral_competence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.09: the arrangement's costs are shoreline-buildable land forgone, observance labor, and preservation spending, borne overwhelmingly voluntarily, and no seat collects a rent — extraction is residual friction, not transfer. Suppression is authored at 0.09 as a raw structural property (unscaled by power or scope; only extractiveness is scaled downstream by the engine): enforcement is social and ritual — elder authority, communal expectation, drill compulsion — and it has largely retired into ceremony. Accessibility_collapse is low (0.25) because alternatives openly proliferate: official hazard maps, engineered seawalls, and personal judgment all substitute; the stones never monopolized the information. Resistance is moderate (0.42): real defection occurred — postwar siting below the lines, thinning observance attendance — which is precisely the evidence that made enforcement necessary and its decline visible. Theater_ratio is authored honestly at 0.55 for the end state: by 2025 a majority of stone-directed activity (anniversaries, plaques, school visits, heritage tourism) is commemorative rather than behaviorally load-bearing, though the reading's historical claim stands on the near-zero theater of the early interval. The measurement series run on one shared seven-point grid (1897, 1933, 1960, 1985, 2000, 2011, 2025) with all three tracked metrics authored at every point; the 2011 column is a crisis reactivation — extractiveness dips, enforcement spikes, theater briefly falls — a perturbation, not a cycle, so no cyclical-pattern machinery is invoked. Suppression_requirement is tracked because this story's subject IS enforcement-capacity change: the intergenerational enforcement apparatus builds, matures, and decays across the interval.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute differently. Elder transmitters sit identity_locked: the custodial role constitutes their standing in the community, so exit is unthinkable without dissolving the office itself — they experience the arrangement as constitutive duty, and if the identity frame broke (a cohort that stops asking), enforcement would collapse quickly, as it did in depopulated hamlets. Postwar developers and newcomers sat in arbitrage: capital simply sited below the line or elsewhere, experiencing the markers as ignorable sentiment. Descendant households hold a vindicated inheritance — the dividend is banked, and their cost is only retelling. Municipal governments treat the whole complex as a discretionary heritage line item they could defund at will. Same stones, four different lived arrangements; the engine derives this divergence from power, exit, and role data, not from the authored claim.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive the derivation: sanriku_coastal_communities and descendant_households_of_high_sited_settlements receive the arrangement's protection, damping their effective extraction toward subsidy; postwar_coastal_developers_and_newcomers bore restraint costs outside the benefit circuit, pushing their d toward the target end; elder transmitters sit near symmetric (transmission labor out, meaning and standing in); municipal governments tilt slightly to payer (preservation budget out, modest heritage return in). No seat approaches the full-target pole, consistent with the very low authored extractiveness. Receipt surface checked seat by seat: municipalities spend rather than collect, communities labor rather than collect, descendants hold an already-realized safety dividend rather than a flow, and researchers take knowledge, not rents — no seat captures the arrangement's gains, so gain_flow is authored as 'diffuse' as an affirmative finding, not a default.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification guards against two opposite errors. Calling the arrangement pure commemoration (the husk sibling's move) would erase the coordination credit the historical record supports and would wrongly imply the function never existed; calling it a fully live coordination mechanism would overstate present behavioral force that seawalls, hazard maps, and demographic attrition have displaced. The piton reading preserves the true sequence — a genuinely functional coordination institution whose primary function has largely atrophied, persisting by inertia, affection, and cheapness. The cost-asymmetry test holds: municipal governments plainly could alter or abandon the arrangement, but reviving live force would require rebuilding transmission institutions against depopulation headwinds, and dismantling the markers is culturally barred as desecration of a mass-fatality memorial — so the cost of either fix exceeds any benefit the administrator could capture, and no seat profits enough to maintain more than ceremony. The founding problem (hazard memory outlasting individual lifetimes) remains live — 2011 reopened it — so no resolved-mandatrophy declaration is authored; the mismatch consumer should find status=live paired with verdict=world_rearranges, the healthy cell.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_underdetermination,
    'Is the stone-and-transmission complex a live normative institution for most of the interval (this reading) or a commemorative artifact whose compliance was coincidental or weakly enforced (commemorative_husk_reading)?',
    'Archival search for enforcement episodes — disputes over building below the line, sanctioned violations, observance attendance records — plus matched-settlement comparison separating memory-driven evacuation from topographic coincidence.',
    'If the husk reading is right, this file''s coordination credit and vindicated propositions withdraw and the arrangement reduces to an inertial remnant with no functional history; extractiveness stays low either way, but the type narrative and the network edge to the validation axis change.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_underdetermination, empirical, 'Which reading of the tsunami-stone kernel the structural record supports.').

omega_variable(
    transmission_causality_attribution,
    'Did the transmission discipline itself — rather than elevation, wealth, seawalls, or luck — cause the differential survival of memory-keeping settlements in 2011?',
    'Matched-pair studies controlling for elevation, distance to shore, demography, and defensive infrastructure; oral-history triangulation of evacuation decision timing against wave arrival.',
    'Failure of attribution collapses this reading''s vindication basis toward the husk reading without changing extractiveness; success hardens the reading and strengthens its influence edge on the validation-axis sibling.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(transmission_causality_attribution, empirical, 'Whether intergenerational transmission, not confounders, produced the survival differential.').

omega_variable(
    engineered_substitution_irreversibility,
    'Is the post-seawall erosion of transmission practice a reversible trough or a ratchet — does each generation that skips the observance permanently break the chain?',
    'Longitudinal observance-participation and siting data across successor cohorts in walled versus unwalled settlements.',
    'A reversible trough supports eventual revival pressure and keeps the end-state a stabilized remnant; a ratchet drives the arrangement irreversibly into pure commemoration, converting the terminal condition into the husk sibling''s territory.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(engineered_substitution_irreversibility, empirical, 'Whether the decay of enforcement capacity is recoverable or cumulative.').

omega_variable(
    coordination_function_framing,
    'Is the arrangement''s primary coordination function the preservation of a landscape datum (an information standard: the high-water line as public measurement) or the maintenance of a community of memory (identity coordination: membership signaled by heeding)?',
    'Counterfactual test: if the datum were preserved by an equally durable non-social marker readable by strangers, would compliance persist? Historical evidence that outsiders and postwar newcomers ignored identical datums suggests the social layer is load-bearing.',
    'A datum-first framing would reclassify the coordination type toward information_standard with a lower inherent-cost floor and shift several seats'' derived directionality; the end-state classification would be unchanged but the Boltzmann accounting would move.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coordination_function_framing, conceptual, 'Alternative framings of the kernel''s coordination function and their classification consequences.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(tsunami_stone_commitment__behavioral_competence_reading, 1897, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tsc_behavioral_competence_tr_t1897, tsunami_stone_commitment__behavioral_competence_reading, theater_ratio, 1897, 0.03).
narrative_ontology:measurement(tsc_behavioral_competence_tr_t1933, tsunami_stone_commitment__behavioral_competence_reading, theater_ratio, 1933, 0.08).
narrative_ontology:measurement(tsc_behavioral_competence_tr_t1960, tsunami_stone_commitment__behavioral_competence_reading, theater_ratio, 1960, 0.2).
narrative_ontology:measurement(tsc_behavioral_competence_tr_t1985, tsunami_stone_commitment__behavioral_competence_reading, theater_ratio, 1985, 0.34).
narrative_ontology:measurement(tsc_behavioral_competence_tr_t2000, tsunami_stone_commitment__behavioral_competence_reading, theater_ratio, 2000, 0.44).
narrative_ontology:measurement(tsc_behavioral_competence_tr_t2011, tsunami_stone_commitment__behavioral_competence_reading, theater_ratio, 2011, 0.36).
narrative_ontology:measurement(tsc_behavioral_competence_tr_t2025, tsunami_stone_commitment__behavioral_competence_reading, theater_ratio, 2025, 0.55).

% Extraction over time
narrative_ontology:measurement(tsc_behavioral_competence_be_t1897, tsunami_stone_commitment__behavioral_competence_reading, base_extractiveness, 1897, 0.03).
narrative_ontology:measurement(tsc_behavioral_competence_be_t1933, tsunami_stone_commitment__behavioral_competence_reading, base_extractiveness, 1933, 0.04).
narrative_ontology:measurement(tsc_behavioral_competence_be_t1960, tsunami_stone_commitment__behavioral_competence_reading, base_extractiveness, 1960, 0.05).
narrative_ontology:measurement(tsc_behavioral_competence_be_t1985, tsunami_stone_commitment__behavioral_competence_reading, base_extractiveness, 1985, 0.06).
narrative_ontology:measurement(tsc_behavioral_competence_be_t2000, tsunami_stone_commitment__behavioral_competence_reading, base_extractiveness, 2000, 0.07).
narrative_ontology:measurement(tsc_behavioral_competence_be_t2011, tsunami_stone_commitment__behavioral_competence_reading, base_extractiveness, 2011, 0.06).
narrative_ontology:measurement(tsc_behavioral_competence_be_t2025, tsunami_stone_commitment__behavioral_competence_reading, base_extractiveness, 2025, 0.09).

% Suppression requirement over time
narrative_ontology:measurement(tsc_behavioral_competence_su_t1897, tsunami_stone_commitment__behavioral_competence_reading, suppression_requirement, 1897, 0.38).
narrative_ontology:measurement(tsc_behavioral_competence_su_t1933, tsunami_stone_commitment__behavioral_competence_reading, suppression_requirement, 1933, 0.32).
narrative_ontology:measurement(tsc_behavioral_competence_su_t1960, tsunami_stone_commitment__behavioral_competence_reading, suppression_requirement, 1960, 0.24).
narrative_ontology:measurement(tsc_behavioral_competence_su_t1985, tsunami_stone_commitment__behavioral_competence_reading, suppression_requirement, 1985, 0.17).
narrative_ontology:measurement(tsc_behavioral_competence_su_t2000, tsunami_stone_commitment__behavioral_competence_reading, suppression_requirement, 2000, 0.12).
narrative_ontology:measurement(tsc_behavioral_competence_su_t2011, tsunami_stone_commitment__behavioral_competence_reading, suppression_requirement, 2011, 0.26).
narrative_ontology:measurement(tsc_behavioral_competence_su_t2025, tsunami_stone_commitment__behavioral_competence_reading, suppression_requirement, 2025, 0.09).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(tsunami_stone_commitment__behavioral_competence_reading, identity_coordination).
narrative_ontology:affects_constraint(tsunami_stone_commitment__behavioral_competence_reading, tsunami_stone_commitment__commemorative_husk_reading).
narrative_ontology:affects_constraint(tsunami_stone_commitment__behavioral_competence_reading, tsunami_stone_commitment__catastrophe_validation_axis).

% DUAL FORMULATION NOTE:
% The colloquial label 'the tsunami stones' decomposes per the epsilon-invariance principle into three structurally distinct claims: (1) whether the inscription-plus-transmission complex carried live normative force (this file, behavioral_competence_reading); (2) whether the 2011 tsunami constitutes decisive binary validation (catastrophe_validation_axis); (3) whether the surviving remnant is mere commemoration (commemorative_husk_reading). Each carries its own epsilon, beneficiary structure, and type. Upstream confidence flows from this reading to the validation axis because validation presupposes behavioral force; the husk reading competes with this one directly over the same historical record. All three are linked via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
