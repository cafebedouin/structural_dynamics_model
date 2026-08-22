% ============================================================================
% CONSTRAINT STORY: nicene_christological_kernel__homoousios_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_nicene_christological_kernel__homoousios_reading, []).

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
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: nicene_christological_kernel__homoousios_reading
 *   human_readable: Nicene Homoousios Christology as Enforced Imperial-Ecclesiastical Orthodoxy
 *   domain: religious/political
 *
 * SUMMARY:
 *   This story authors the homoousios reading of the Nicene Christological
 *   kernel — the claim that Christ is of the same substance as the Father, as
 *   this claim was actually institutionalized between 325 (Nicaea) and 451
 *   (Chalcedon), with its enforcement apparatus maturing through
 *   Constantinople 381 and the Theodosian legal code. The reading is
 *   evaluated on its own terms as the standing arrangement under contest: a
 *   doctrinal formula that began as a response to a real theological crisis
 *   and became, over the interval, an imperially-enforced test of communion
 *   carrying exile, property confiscation, and suppression of the sibling
 *   homoiousios reading and other subordinationist Christologies. This is ONE
 *   of two sibling constraint stories emitted from the same kernel
 *   (nicene_christological_kernel); the homoiousios_reading story is authored
 *   separately with its own ε and its own stakeholder set, since the two
 *   readings have structurally different extraction profiles despite sharing
 *   a kernel.
 *
 * KEY AGENTS:
 *   - nicene_episcopal_hierarchy: agenda_setter (institutional/arbitrage) — drafts and enforces the creedal test
 *   - imperial_state_authority: agenda_setter/beneficiary (institutional/arbitrage) — provides civil enforcement machinery
 *   - athanasian_theological_faction: beneficiary (organized/mobile) — wins permanent doctrinal authority
 *   - gothic_arian_communities: payer (powerless/trapped) — inherited tradition rebranded heretical
 *   - north_african_homoian_congregations: payer (powerless/trapped) — lose property and standing
 *   - exiled_dissenting_bishops: payer (moderate/trapped) — deposed and exiled from sees
 *   - later_church_historians: observer (analytical) — reconstructs the political-theological mixture
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(nicene_christological_kernel__homoousios_reading, 0.71).
domain_priors:suppression_score(nicene_christological_kernel__homoousios_reading, 0.8).
domain_priors:theater_ratio(nicene_christological_kernel__homoousios_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(nicene_christological_kernel__homoousios_reading, extractiveness, 0.71).
narrative_ontology:constraint_metric(nicene_christological_kernel__homoousios_reading, suppression_requirement, 0.8).
narrative_ontology:constraint_metric(nicene_christological_kernel__homoousios_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(nicene_christological_kernel__homoousios_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(nicene_christological_kernel__homoousios_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(nicene_christological_kernel__homoousios_reading, tangled_rope).
narrative_ontology:human_readable(nicene_christological_kernel__homoousios_reading, "Nicene Homoousios Christology as Enforced Imperial-Ecclesiastical Orthodoxy").
narrative_ontology:topic_domain(nicene_christological_kernel__homoousios_reading, "religious/political").

domain_priors:requires_active_enforcement(nicene_christological_kernel__homoousios_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(nicene_christological_kernel__homoousios_reading, '30675104-4478-4219-ac51-83cfea1c1577').
narrative_ontology:cs_kernel_codification('30675104-4478-4219-ac51-83cfea1c1577', formalized).
narrative_ontology:cs_authority_grounding('30675104-4478-4219-ac51-83cfea1c1577', lineage).
narrative_ontology:cs_interpretation_layer_present('30675104-4478-4219-ac51-83cfea1c1577').
narrative_ontology:cs_reading_relation('30675104-4478-4219-ac51-83cfea1c1577', nicene_christological_kernel__homoiousios_reading, coexists_with).
narrative_ontology:cs_axiom('30675104-4478-4219-ac51-83cfea1c1577', foundational, full_ontological_identity_of_essence_required).
narrative_ontology:cs_axiom_status(full_ontological_identity_of_essence_required, holdable).
narrative_ontology:cs_axiom_grounding('30675104-4478-4219-ac51-83cfea1c1577', full_ontological_identity_of_essence_required, theological).
narrative_ontology:cs_axiom('30675104-4478-4219-ac51-83cfea1c1577', secondary, doctrinal_uniformity_is_necessary_for_ecclesial_unity).
narrative_ontology:cs_axiom_status(doctrinal_uniformity_is_necessary_for_ecclesial_unity, holdable).
narrative_ontology:cs_axiom_grounding('30675104-4478-4219-ac51-83cfea1c1577', doctrinal_uniformity_is_necessary_for_ecclesial_unity, instrumental).
narrative_ontology:cs_reference_frame('30675104-4478-4219-ac51-83cfea1c1577', conciliar_creedal_settlement).
narrative_ontology:cs_drift_state('30675104-4478-4219-ac51-83cfea1c1577', post_theodosian_code_enforcement, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('30675104-4478-4219-ac51-83cfea1c1577', '').
narrative_ontology:cs_kernel_id(nicene_christological_kernel__homoousios_reading, nicene_christological_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(nicene_christological_kernel__homoousios_reading, nicene_episcopal_hierarchy).
narrative_ontology:constraint_beneficiary(nicene_christological_kernel__homoousios_reading, imperial_state_authority).
narrative_ontology:constraint_beneficiary(nicene_christological_kernel__homoousios_reading, athanasian_theological_faction).
narrative_ontology:constraint_victim(nicene_christological_kernel__homoousios_reading, gothic_arian_communities).
narrative_ontology:constraint_victim(nicene_christological_kernel__homoousios_reading, north_african_homoian_congregations).
narrative_ontology:constraint_victim(nicene_christological_kernel__homoousios_reading, regional_liturgical_autonomy).
narrative_ontology:constraint_victim(nicene_christological_kernel__homoousios_reading, exiled_dissenting_bishops).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Convenes and controls councils (Nicaea 325, Constantinople 381), drafts the creedal formula, and administers anathemas against dissenters. Sets the terms of communion: sees are stripped or granted based on adherence. Holds the levers of excommunication, exile petitions to the emperor, and control of church property allocation.
narrative_ontology:constraint_stakeholder(nicene_christological_kernel__homoousios_reading, nicene_episcopal_hierarchy, agenda_setter,
    institutional, civilizational, arbitrage, continental).

% Backs the homoousian settlement with civil enforcement — exile decrees, confiscation of church buildings from non-conforming clergy, and legal penalties for continued dissent under edicts like those of Theodosius I. Gains a unified doctrinal basis for imperial cohesion; theological uniformity is treated as a proxy for political loyalty.
narrative_ontology:constraint_stakeholder(nicene_christological_kernel__homoousios_reading, imperial_state_authority, agenda_setter,
    institutional, generational, arbitrage, continental).
narrative_ontology:stakeholder_secondary_role(nicene_christological_kernel__homoousios_reading, imperial_state_authority, beneficiary).

% Wins the doctrinal contest and gains permanent theological authority, patronage networks, and control of major sees (Alexandria, later much of the Latin West). Its formula becomes the test of communion; it faces genuine risk only during intervals of imperial favor toward rival factions, from which it can recover through political alliance-building.
narrative_ontology:constraint_stakeholder(nicene_christological_kernel__homoousios_reading, athanasian_theological_faction, beneficiary,
    organized, generational, mobile, continental).

% Received Christianity in a homoian/subordinationist form via Ulfilas and hold it as an inherited ethnic-religious identity. Under Nicene consolidation their tradition is branded heretical; their clergy are barred from imperial ecclesiastical structures and their communities face social and legal marginalization within Roman territory despite no practical alternative faith identity to adopt.
narrative_ontology:constraint_stakeholder(nicene_christological_kernel__homoousios_reading, gothic_arian_communities, payer,
    powerless, generational, trapped, regional).

% Local congregations and clergy who held non-Nicene Christological positions or persisted in Donatist-adjacent regional practice face property confiscation, forced reconsecration of churches, and civil penalties once imperial enforcement machinery is applied to the province. Exit means abandoning ancestral worship community entirely.
narrative_ontology:constraint_stakeholder(nicene_christological_kernel__homoousios_reading, north_african_homoian_congregations, payer,
    powerless, generational, trapped, regional).

% Bishops who held homoiousian or subordinationist positions (or shifted with changing imperial favor) are deposed, exiled from their sees by imperial decree, and stripped of ecclesiastical income and standing. Some are later rehabilitated if political winds shift, but many die in exile with no path back to their communities.
narrative_ontology:constraint_stakeholder(nicene_christological_kernel__homoousios_reading, exiled_dissenting_bishops, payer,
    moderate, biographical, trapped, regional).

% The practice of regional churches setting their own Christological emphasis and liturgical formulas without centralized creedal policing is eliminated as an institutional possibility once the homoousian standard becomes the empire-wide test of orthodoxy enforced by civil law.
narrative_ontology:constraint_stakeholder(nicene_christological_kernel__homoousios_reading, regional_liturgical_autonomy, payer,
    powerless, generational, trapped, regional).
narrative_ontology:stakeholder_non_agent(nicene_christological_kernel__homoousios_reading, regional_liturgical_autonomy).

% Study the councils, imperial edicts, and exile records to reconstruct how much of the settlement reflects theological reasoning versus political consolidation. Have access to conciliar minutes, imperial legal codes (e.g., Theodosian Code), and the writings of the losing factions preserved secondhand through their opponents.
narrative_ontology:constraint_stakeholder(nicene_christological_kernel__homoousios_reading, later_church_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(nicene_christological_kernel__homoousios_reading, diffuse).
narrative_ontology:fixing_cost_class(nicene_christological_kernel__homoousios_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides the empire and church hierarchy with a single, non-negotiable Christological formula that settles what would otherwise be an unresolvable, communion-fracturing dispute about Christ's relationship to the Father, enabling unified liturgy, shared creedal recitation, and a stable basis for determining who is in and out of ecclesiastical communion.
% TRANSFER_FUNCTION: Moves theological authority, ecclesiastical office, imperial patronage, and physical church property from communities and clergy holding non-Nicene Christologies to those holding the homoousian formula, backed by imperial civil enforcement (exile, confiscation, legal penalty).
% ABSENT_VOICES: Gothic Arian communities and North African congregations who held sincerely-reasoned alternative Christologies were not seated as equal parties at the councils that decided against them; their theological reasoning survives mostly through hostile citation by the winning faction, and their objection — that homoousios collapses a distinction they considered essential to monotheistic coherence — is structurally absent from the settled record.
% DISAPPEARANCE_RATIONALE: If the enforced homoousian settlement vanished, the empire-wide test of communion would dissolve; regional Christologies (Gothic homoian Arianism, various subordinationist traditions) would persist as legitimate, untested alternatives rather than heresies; property and see-control currently allocated by creedal conformity would be renegotiated; and the precedent of civil law enforcing a specific metaphysical formula would lose its founding exemplar.
% FOUNDING_PROBLEM: The early fourth-century church faced a genuine, communion-threatening dispute (the Arian controversy) over whether the Son was co-eternal and consubstantial with the Father or a created, subordinate being — a real theological and pastoral crisis needing some resolution to prevent permanent schism.
% FOUNDING_PROBLEM_CORROBORATION: Nicene sources (Athanasius, later conciliar tradition) attest the problem was resolved correctly and definitively. Independent corroboration from outside the winning faction is thin: surviving Arian and homoian sources exist mostly as fragments quoted by their opponents, but modern historical-critical scholarship (including scholars with no doctrinal stake in either side) documents that imperial political consolidation under Constantine and later Theodosius, not purely theological resolution, drove the timing and enforcement mechanism of the settlement — suggesting the founding theological problem was real but its resolution was substantially overtaken by a political-administrative function.
narrative_ontology:disappearance_verdict(nicene_christological_kernel__homoousios_reading, world_rearranges).
narrative_ontology:founding_problem_status(nicene_christological_kernel__homoousios_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(nicene_christological_kernel__homoousios_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(nicene_christological_kernel__homoousios_reading, 'none', 1).
narrative_ontology:epsilon_provenance(nicene_christological_kernel__homoousios_reading, 0.71, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(nicene_christological_kernel__homoousios_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(nicene_christological_kernel__homoousios_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(nicene_christological_kernel__homoousios_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored high (0.71 at interval end) but not maximal — the founding theological dispute was genuine, so the formula is not pure invented pretext; the coordination function (settling a communion-fracturing dispute) is real, which is exactly why this reading is classified tangled_rope rather than snare. Suppression is authored higher than extraction (0.80) because the mechanism by which the formula persists — anathema, exile, property confiscation, imperial legal penalty — is a raw structural fact of the enforcement apparatus and is not scaled by scope or power in the engine's computation; it is simply severe and well-documented across the interval. Theater ratio rises modestly (0.12 to 0.30) reflecting that a growing share of enforcement activity (repeated re-affirmation councils, formulaic condemnation of positions already defeated) becomes performative reassertion of a settled result rather than live theological adjudication.
 *
 * PERSPECTIVAL GAP:
 *   From the seat of the nicene_episcopal_hierarchy and imperial_state_authority, the settlement reads as successful coordination: a genuine crisis resolved, unified communion restored, doctrinal clarity achieved. From the seat of gothic_arian_communities and north_african_homoian_congregations, the identical structure reads as enforced extraction: an inherited, sincerely-held theological position criminalized, property seized, and no path to legitimate dissent. The engine computes these as different types from the same structural data because the beneficiary seats have arbitrage-grade exit (they can adapt their theological production to whatever the winning formula requires) while the payer seats are trapped (their entire religious identity is the thing being suppressed).
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (nicene_episcopal_hierarchy, imperial_state_authority, athanasian_theological_faction) derive low d — the constraint's operation subsidizes their institutional position, patronage networks, and property holdings. Victims (gothic_arian_communities, north_african_homoian_congregations, exiled_dissenting_bishops) derive high d — trapped exit options and direct loss of standing, property, and communion rights push them toward the full-target end. regional_liturgical_autonomy is marked as a non-agent (agent: false) because it is an institutional possibility being eliminated, not an actor that can be classified as bearing directionality itself — it is retained for narrative completeness only.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding_problem_status is authored as contested rather than dead: the underlying theological question (how to articulate Christ's relationship to the Father without collapsing into either subordinationism or modalism) remains a live doctrinal concern within Christian theology to this day, which prevents a simple zombie-institution reading. But the specific enforcement mechanism — imperial civil penalty for holding an alternative Christological position — clearly outlived any plausible claim that civil coercion was proportionate to theological ambiguity, and continued to intensify (suppression rising from 0.35 to 0.80) well after the initial 325/381 conciliar settlements. The tangled_rope classification captures this: a genuine coordination function (settling schism) persists alongside asymmetric extraction (civil punishment of dissent) that the coordination function alone does not require.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    theological_necessity_vs_political_consolidation,
    'Was the homoousian formula''s imperial enforcement (exile, confiscation, civil penalty) a necessary consequence of resolving a genuine, communion-threatening theological dispute, or did the theological dispute serve as pretext/vehicle for a political consolidation project (unifying the empire around Constantine''s and later Theodosius''s preferred faction) that would have found another vehicle absent this specific controversy?',
    'Comparative analysis of enforcement intensity against comparable christological/doctrinal disputes that did NOT receive comparable imperial civil backing, and close reading of the timing correlation between imperial political needs (succession crises, unification pressure) and escalation of enforcement measures.',
    'If enforcement tracked political consolidation needs more than theological resolution needs, the tangled_rope classification is well-supported and the coordination component is smaller than the formula''s defenders claim. If enforcement tracked genuine schism-prevention needs, the coordination component is larger and closer to a rope with unfortunate but proportionate enforcement costs.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(theological_necessity_vs_political_consolidation, conceptual, 'Whether enforcement severity reflects theological necessity or political vehicle use.').

omega_variable(
    sibling_reading_symmetry,
    'Given that this story and the homoiousios_reading sibling share the same founding theological crisis, is the extractiveness gap between the two readings (this reading authored substantially higher) a genuine structural difference — because history happened to select homoousios as the enforced winner — or an artifact of authoring the losing reading''s counterfactual enforcement apparatus more leniently than it would actually have operated had it won?',
    'Examine historical episodes where homoian/subordinationist factions held temporary imperial favor (e.g., under Constantius II) to see whether their enforcement apparatus, when empowered, was comparably severe — this would suggest the extraction is a property of imperial-doctrinal fusion generally, not of homoousios specifically.',
    'If homoian enforcement under Constantius II was comparably severe when empowered, this suggests the extraction profile attaches to the fusion of doctrine with imperial power rather than to the homoousios content specifically, which would argue for symmetric ε treatment across sibling readings rather than the asymmetry currently authored.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_symmetry, empirical, 'Whether the ε asymmetry between sibling readings reflects genuine structural difference or historical-outcome artifact.').

omega_variable(
    corroboration_source_reliability,
    'How much can be trusted about the reconstructed theological reasoning and sincerity of the losing factions (Gothic Arians, homoian North Africans) given that nearly all surviving textual evidence of their positions comes through hostile citation by the winning Nicene faction?',
    'Cross-reference surviving primary Arian/homoian texts (fragments, the Gothic Bible tradition, conciliar records of minority positions) against secondhand hostile citations for consistency; weight historical reconstruction accordingly.',
    'If the hostile-citation record substantially distorts the losing factions'' actual positions, the victim characterization in this story may overstate or understate the coherence of their theological reasoning, affecting how much weight the absent_voices analysis should carry.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(corroboration_source_reliability, empirical, 'Reliability of secondhand sources for reconstructing suppressed theological positions.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(nicene_christological_kernel__homoousios_reading, 325, 451).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(nice_tr_t325, nicene_christological_kernel__homoousios_reading, theater_ratio, 325, 0.12).
narrative_ontology:measurement_basis(nice_tr_t325, observed).
narrative_ontology:measurement(nice_tr_t350, nicene_christological_kernel__homoousios_reading, theater_ratio, 350, 0.16).
narrative_ontology:measurement_basis(nice_tr_t350, observed).
narrative_ontology:measurement(nice_tr_t381, nicene_christological_kernel__homoousios_reading, theater_ratio, 381, 0.2).
narrative_ontology:measurement_basis(nice_tr_t381, observed).
narrative_ontology:measurement(nice_tr_t400, nicene_christological_kernel__homoousios_reading, theater_ratio, 400, 0.25).
narrative_ontology:measurement_basis(nice_tr_t400, observed).
narrative_ontology:measurement(nice_tr_t425, nicene_christological_kernel__homoousios_reading, theater_ratio, 425, 0.29).
narrative_ontology:measurement_basis(nice_tr_t425, observed).
narrative_ontology:measurement(nice_tr_t451, nicene_christological_kernel__homoousios_reading, theater_ratio, 451, 0.3).
narrative_ontology:measurement_basis(nice_tr_t451, observed).

% Extraction over time
narrative_ontology:measurement(nice_be_t325, nicene_christological_kernel__homoousios_reading, base_extractiveness, 325, 0.42).
narrative_ontology:measurement_basis(nice_be_t325, observed).
narrative_ontology:measurement(nice_be_t350, nicene_christological_kernel__homoousios_reading, base_extractiveness, 350, 0.48).
narrative_ontology:measurement_basis(nice_be_t350, observed).
narrative_ontology:measurement(nice_be_t381, nicene_christological_kernel__homoousios_reading, base_extractiveness, 381, 0.6).
narrative_ontology:measurement_basis(nice_be_t381, observed).
narrative_ontology:measurement(nice_be_t400, nicene_christological_kernel__homoousios_reading, base_extractiveness, 400, 0.66).
narrative_ontology:measurement_basis(nice_be_t400, observed).
narrative_ontology:measurement(nice_be_t425, nicene_christological_kernel__homoousios_reading, base_extractiveness, 425, 0.7).
narrative_ontology:measurement_basis(nice_be_t425, observed).
narrative_ontology:measurement(nice_be_t451, nicene_christological_kernel__homoousios_reading, base_extractiveness, 451, 0.71).
narrative_ontology:measurement_basis(nice_be_t451, observed).

% Suppression requirement over time
narrative_ontology:measurement(nice_su_t325, nicene_christological_kernel__homoousios_reading, suppression_requirement, 325, 0.35).
narrative_ontology:measurement_basis(nice_su_t325, observed).
narrative_ontology:measurement(nice_su_t350, nicene_christological_kernel__homoousios_reading, suppression_requirement, 350, 0.5).
narrative_ontology:measurement_basis(nice_su_t350, observed).
narrative_ontology:measurement(nice_su_t381, nicene_christological_kernel__homoousios_reading, suppression_requirement, 381, 0.68).
narrative_ontology:measurement_basis(nice_su_t381, observed).
narrative_ontology:measurement(nice_su_t400, nicene_christological_kernel__homoousios_reading, suppression_requirement, 400, 0.76).
narrative_ontology:measurement_basis(nice_su_t400, observed).
narrative_ontology:measurement(nice_su_t425, nicene_christological_kernel__homoousios_reading, suppression_requirement, 425, 0.79).
narrative_ontology:measurement_basis(nice_su_t425, observed).
narrative_ontology:measurement(nice_su_t451, nicene_christological_kernel__homoousios_reading, suppression_requirement, 451, 0.8).
narrative_ontology:measurement_basis(nice_su_t451, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(nicene_christological_kernel__homoousios_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(nicene_christological_kernel__homoousios_reading, 0.08).
narrative_ontology:affects_constraint(nicene_christological_kernel__homoousios_reading, homoiousios_reading).
narrative_ontology:affects_constraint(nicene_christological_kernel__homoousios_reading, chalcedonian_definition_kernel).

% DUAL FORMULATION NOTE:
% This constraint and homoiousios_reading are sibling readings of nicene_christological_kernel, decomposed per the ε-invariance principle: the two readings differ sharply in beneficiary/victim structure and extraction profile depending on which reading history's enforcement apparatus favored. This story (homoousios) carries the higher ε because it is the reading that was institutionally enforced with imperial civil backing from 325-451; the sibling story authors the homoiousios reading's own extraction profile independently, primarily as the suppressed alternative rather than as an enforcing power (though during periods of Constantius II's favor, that asymmetry may partially reverse — see the sibling_reading_symmetry omega).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
