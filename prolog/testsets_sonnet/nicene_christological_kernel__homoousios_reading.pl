% ============================================================================
% CONSTRAINT STORY: nicene_christological_kernel__homoousios_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: nicene_christological_kernel__homoousios_reading
 *   human_readable: Nicene Homoousios Doctrine as Enforced Imperial-Ecclesiastical Orthodoxy
 *   domain: historical_theology/ecclesiastical_authority
 *
 * SUMMARY:
 *   This story instantiates the homoousios reading of the Nicene
 *   Christological kernel: the doctrinal claim that Christ shares full,
 *   identical divine substance with the Father, as codified at the Council of
 *   Nicaea (325) and reaffirmed through subsequent councils against Arian and
 *   homoian challengers. The formula itself functions as a genuine
 *   coordination device — it resolves an open theological question and
 *   provides a stable creedal basis for communion across dispersed Christian
 *   communities. But its historical operation, from the mid-fourth century
 *   through the Council of Chalcedon (451), was substantially entangled with
 *   imperial state power: enforcement proceeded through exile of dissenting
 *   bishops (including periods where Arian-leaning emperors reversed
 *   enforcement direction), confiscation of church property from
 *   non-conforming congregations, and legal delegitimization of Gothic and
 *   North African communities whose received Christianity was homoian or
 *   subordinationist in character. The sibling reading (homoiousios_reading)
 *   is NOT part of this story — it is a separate constraint with its own ε
 *   and its own stakeholder structure, linked here via network edges. This
 *   story's ε reflects the homoousios formula's specific enforcement history,
 *   not a blended or averaged assessment across readings.
 *
 * KEY AGENTS:
 *   - nicene_episcopal_hierarchy: agenda_setter/beneficiary (institutional/arbitrage) — drafts and enforces the creed
 *   - imperial_state_authority: agenda_setter/beneficiary (institutional/arbitrage) — uses uniformity for administrative cohesion
 *   - athanasian_theological_faction: beneficiary (organized/constrained) — gains doctrinal and institutional vindication
 *   - gothic_arian_communities: payer (powerless/trapped) — inherited belief redefined as heresy
 *   - north_african_homoian_congregations: payer (powerless/trapped) — lose property and standing under shifting imperial favor
 *   - regional_theological_autonomy: payer, non-agent (powerless/trapped) — the pluralism foreclosed by enforcement
 *   - later_church_historians: observer (analytical/analytical) — assesses theological vs. political drivers
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(nicene_christological_kernel__homoousios_reading, 0.78).
domain_priors:suppression_score(nicene_christological_kernel__homoousios_reading, 0.85).
domain_priors:theater_ratio(nicene_christological_kernel__homoousios_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(nicene_christological_kernel__homoousios_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(nicene_christological_kernel__homoousios_reading, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(nicene_christological_kernel__homoousios_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(nicene_christological_kernel__homoousios_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(nicene_christological_kernel__homoousios_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(nicene_christological_kernel__homoousios_reading, tangled_rope).
narrative_ontology:human_readable(nicene_christological_kernel__homoousios_reading, "Nicene Homoousios Doctrine as Enforced Imperial-Ecclesiastical Orthodoxy").
narrative_ontology:topic_domain(nicene_christological_kernel__homoousios_reading, "historical_theology/ecclesiastical_authority").

domain_priors:requires_active_enforcement(nicene_christological_kernel__homoousios_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(nicene_christological_kernel__homoousios_reading, '4c1f5e24-9dff-4a36-b6ff-49388415dd70').
narrative_ontology:cs_kernel_codification('4c1f5e24-9dff-4a36-b6ff-49388415dd70', formalized).
narrative_ontology:cs_authority_grounding('4c1f5e24-9dff-4a36-b6ff-49388415dd70', lineage).
narrative_ontology:cs_interpretation_layer_present('4c1f5e24-9dff-4a36-b6ff-49388415dd70').
narrative_ontology:cs_reading_relation('4c1f5e24-9dff-4a36-b6ff-49388415dd70', nicene_christological_kernel__homoiousios_reading, forecloses).
narrative_ontology:cs_axiom('4c1f5e24-9dff-4a36-b6ff-49388415dd70', foundational, father_and_son_share_numerically_identical_essence).
narrative_ontology:cs_axiom_status(father_and_son_share_numerically_identical_essence, holdable).
narrative_ontology:cs_axiom_grounding('4c1f5e24-9dff-4a36-b6ff-49388415dd70', father_and_son_share_numerically_identical_essence, deontological).
narrative_ontology:cs_axiom('4c1f5e24-9dff-4a36-b6ff-49388415dd70', secondary, ontological_equality_required_for_soteriological_efficacy).
narrative_ontology:cs_axiom_status(ontological_equality_required_for_soteriological_efficacy, holdable).
narrative_ontology:cs_axiom_grounding('4c1f5e24-9dff-4a36-b6ff-49388415dd70', ontological_equality_required_for_soteriological_efficacy, theological).
narrative_ontology:cs_reference_frame('4c1f5e24-9dff-4a36-b6ff-49388415dd70', nicene_conciliar_settlement_325).
narrative_ontology:cs_drift_state('4c1f5e24-9dff-4a36-b6ff-49388415dd70', post_chalcedonian_consolidation_451, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('4c1f5e24-9dff-4a36-b6ff-49388415dd70', '').
narrative_ontology:cs_kernel_id(nicene_christological_kernel__homoousios_reading, nicene_christological_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(nicene_christological_kernel__homoousios_reading, nicene_episcopal_hierarchy).
narrative_ontology:constraint_beneficiary(nicene_christological_kernel__homoousios_reading, imperial_state_authority).
narrative_ontology:constraint_beneficiary(nicene_christological_kernel__homoousios_reading, athanasian_theological_faction).
narrative_ontology:constraint_victim(nicene_christological_kernel__homoousios_reading, gothic_arian_communities).
narrative_ontology:constraint_victim(nicene_christological_kernel__homoousios_reading, north_african_homoian_congregations).
narrative_ontology:constraint_victim(nicene_christological_kernel__homoousios_reading, regional_theological_autonomy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Bishops aligned with the Nicene formula draft and enforce the homoousios creed at councils, issue anathemas against dissenting clergy, and control access to sees, property, and communion. They frame the formula as the necessary safeguard against subordinationist error, and their institutional standing is built on successfully defending it.
narrative_ontology:constraint_stakeholder(nicene_christological_kernel__homoousios_reading, nicene_episcopal_hierarchy, agenda_setter,
    institutional, civilizational, arbitrage, continental).
narrative_ontology:stakeholder_secondary_role(nicene_christological_kernel__homoousios_reading, nicene_episcopal_hierarchy, beneficiary).

% Roman and later Byzantine emperors use enforcement of a single creedal formula as a mechanism of imperial unity, exiling bishops who refuse subscription and confiscating church property from non-conforming congregations. Doctrinal uniformity is instrumentally useful for administrative cohesion regardless of the theological merits.
narrative_ontology:constraint_stakeholder(nicene_christological_kernel__homoousios_reading, imperial_state_authority, agenda_setter,
    institutional, generational, arbitrage, continental).
narrative_ontology:stakeholder_secondary_role(nicene_christological_kernel__homoousios_reading, imperial_state_authority, beneficiary).

% Theologians and clergy who championed the homoousios formula gain doctrinal vindication, institutional promotion, and the authority to define what counts as heresy going forward. Their theological project becomes the measure of orthodoxy itself.
narrative_ontology:constraint_stakeholder(nicene_christological_kernel__homoousios_reading, athanasian_theological_faction, beneficiary,
    organized, generational, constrained, continental).

% Gothic converts received an Arian-inflected Christianity through earlier missionary work (Ulfilas) that framed the Son as ontologically subordinate. Under Nicene enforcement they are branded heretics, denied full communion and legal standing within the empire, and face pressure to abandon inherited belief or remain marginalized outsiders with no real exit from either the empire's reach or their own communal identity.
narrative_ontology:constraint_stakeholder(nicene_christological_kernel__homoousios_reading, gothic_arian_communities, payer,
    powerless, generational, trapped, regional).

% Local congregations holding homoian or subordinationist Christologies lose church buildings, clerical positions, and legal recognition when imperial edicts enforce Nicene subscription. Vandal-era reversals and later Byzantine reconquest subject them to alternating persecutions depending on which formula holds imperial favor, leaving them with no stable ground regardless of position taken.
narrative_ontology:constraint_stakeholder(nicene_christological_kernel__homoousios_reading, north_african_homoian_congregations, payer,
    powerless, biographical, trapped, regional).

% The practice of locally variant Christological formulations, tolerated in earlier centuries as regional theological culture, is foreclosed once a single formula is backed by imperial coercion. This is not an actor but a casualty: the space for doctrinal pluralism within the empire's Christian communities is what the enforcement apparatus eliminates.
narrative_ontology:constraint_stakeholder(nicene_christological_kernel__homoousios_reading, regional_theological_autonomy, payer,
    powerless, civilizational, trapped, continental).
narrative_ontology:stakeholder_non_agent(nicene_christological_kernel__homoousios_reading, regional_theological_autonomy).

% Scholars examining the councils, imperial edicts, exile records, and property confiscation decrees assess whether the homoousios formula's victory reflects theological necessity, political consolidation, or both operating through the same enforcement machinery.
narrative_ontology:constraint_stakeholder(nicene_christological_kernel__homoousios_reading, later_church_historians, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single, shareable formula for the relationship between Christ and the Father that resolves an otherwise open-ended theological dispute, enabling unified liturgy, common creed recitation, and a stable basis for clerical communion across dispersed Christian communities.
% TRANSFER_FUNCTION: Moves theological authority, ecclesiastical office, imperial favor, and church property from communities and clergy holding subordinationist Christologies toward the Nicene hierarchy and the imperial administration that backs it; moves the cost of doctrinal certainty onto those whose prior belief becomes heresy by definitional fiat.
% ABSENT_VOICES: Gothic Arian congregations and North African homoian communities are not seated at Nicaea or subsequent enforcing councils; their theological tradition, transmitted through their own missionary lineage, is adjudicated and condemned without their participation in the deciding body.
% DISAPPEARANCE_RATIONALE: If homoousios enforcement vanished, the imperial mechanism for compelling doctrinal uniformity would lose its content; regional Christological diversity (subordinationist, homoian, and other formulas) would likely re-emerge as live, tolerated positions rather than suppressed heresies, and ecclesiastical office would no longer hinge on creedal subscription to this specific formula.
% FOUNDING_PROBLEM: The early fourth-century church faced a genuine, unresolved dispute over Christ's relationship to the Father (Arius's subordinationism vs. emerging equal-substance views) that threatened to fracture Christian communion and, once Christianity became imperially favored, to fracture the empire's administrative unity along theological lines.
% FOUNDING_PROBLEM_CORROBORATION: Nicene sources (Athanasius, conciliar acts) attest the problem as a genuine Christological crisis requiring resolution. Independent corroboration from outside the beneficiary faction is thinner: later Arian and homoian sources (largely preserved only through hostile Nicene transmission or fragmentary Gothic/Vandal records) attest that their communities experienced the same period as a stable, coherent theological tradition until imperial coercion redefined it as crisis; modern historians of late antiquity (outside either faction) generally read the imperial adoption of Nicene enforcement as serving administrative unification at least as much as resolving theological uncertainty.
narrative_ontology:disappearance_verdict(nicene_christological_kernel__homoousios_reading, world_rearranges).
narrative_ontology:founding_problem_status(nicene_christological_kernel__homoousios_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(nicene_christological_kernel__homoousios_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(nicene_christological_kernel__homoousios_reading, 'none', 1).
narrative_ontology:epsilon_provenance(nicene_christological_kernel__homoousios_reading, 0.78, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness rises sharply from 0.35 at Nicaea (325) to 0.78 by Chalcedon (451) as the coordination function (a shared Christological formula) becomes increasingly fused with imperial enforcement machinery — exile, property confiscation, legal delisting of non-conforming clergy. Suppression tracks a similar trajectory (0.40 to 0.85) reflecting the hardening of enforcement infrastructure across the fourth and fifth centuries, including periods of enforcement reversal under Arian-sympathetic emperors that nonetheless demonstrate the same coercive apparatus applied in the opposite direction. Theater ratio is moderate (0.40) — genuine theological argument continued throughout (Cappadocian Fathers, Augustine), but an increasing share of conciliar activity became about consolidating political alignment rather than resolving substantive doctrine. Accessibility collapse is high (0.72): once imperial backing attached to the formula, alternative Christological framings became practically inaccessible to ordinary believers regardless of their theological merits. Resistance is high (0.70), reflecting sustained Gothic, Vandal, and North African resistance to Nicene enforcement across two centuries.
 *
 * DIRECTIONALITY LOGIC:
 *   The Nicene episcopal hierarchy and imperial authority sit at the beneficiary end: they set the terms of orthodoxy and collect institutional and administrative benefit from uniformity. The athanasian theological faction benefits similarly but with less direct coercive control — their exit options are constrained by the same doctrinal commitments that elevated them. Gothic Arian and North African homoian communities sit at the full-target end: trapped by geography and communal identity, unable to exit either the empire's jurisdiction or their own inherited faith tradition without total rupture. Regional theological autonomy is authored as a non-agent victim — it is the pluralistic practice itself, not an actor, that is foreclosed.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (a genuine, unresolved Christological dispute threatening communion) was substantively live in 325 but had been resolved doctrinally within Nicene communities well before the coercive enforcement apparatus reached its mature form by Chalcedon. The mismatch here (status: contested, verdict: world_rearranges) flags exactly the capture/zombie pattern the R5 interview is built to surface: the theological question was live, but by the fifth century the enforcement machinery increasingly served imperial administrative consolidation and factional institutional entrenchment rather than ongoing doctrinal deliberation. Treating this as pure Mountain (settled theological truth requiring no enforcement) would erase the coercive history; treating it as pure Snare would erase the genuine coordination function the formula provided for communities that voluntarily adopted it. Tangled Rope captures both: real coordination value plus asymmetric extraction from those who did not.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    theological_necessity_vs_political_consolidation,
    'Was the homoousios formula''s eventual dominance driven primarily by theological argument winning on its merits, or by its alignment with the administrative interests of successive Nicene-favoring emperors?',
    'Comparative analysis of periods where imperial favor shifted between homoousios and homoian factions (e.g., under Constantius II vs. Theodosius I): if doctrinal enforcement direction tracked imperial preference rather than accumulating theological consensus, this supports the political-consolidation reading.',
    'If enforcement primarily tracked imperial interest, the coordination-function claim is substantially weaker than presented and the constraint moves further toward pure extraction (snare) rather than tangled rope; if theological consensus was independently building regardless of imperial backing, the coordination function is more genuine.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(theological_necessity_vs_political_consolidation, conceptual, 'Whether homoousios''s triumph reflects theological merit or imperial administrative convenience.').

omega_variable(
    kernel_reading_relationship,
    'Is homoousios one reading of a genuinely underdetermined kernel (the ontological relationship between Christ and the Father), or does the kernel itself only appear underdetermined because both readings were live political factions with imperial backing at different times?',
    'Examine whether purely theological (non-politically-backed) communities independently converged on one reading over the other in the absence of imperial enforcement, as a natural experiment separating doctrinal from political drivers.',
    'If independent convergence occurred, the kernel has a genuine theological resolution and this reading''s dominance is partially non-arbitrary; if convergence tracked imperial power exclusively, the kernel remains structurally contested and this story''s classification as tangled_rope (rather than pure snare) becomes harder to sustain.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_relationship, conceptual, 'Whether the homoousios/homoiousios contest reflects genuine theological underdetermination or pure political faction alignment.').

omega_variable(
    gothic_communal_identity_suppression_mechanism,
    'Is the suppression experienced by Gothic Arian communities primarily structural (legal exclusion, property confiscation, denial of office) or partially internalized (communal identity fused with a theological tradition that outside pressure alone cannot dissolve)?',
    'Trace post-conversion trajectories of Gothic communities after eventual Nicene conformity (6th-7th century Visigothic Spain): if resistance and distinct identity persisted well past the removal of structural legal barriers, this indicates a substantial internalized component.',
    'If internalized, the effective suppression experienced by these communities exceeds what legal/structural measures alone would predict, meaning the true extractive cost is understated by structural suppression metrics alone.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(gothic_communal_identity_suppression_mechanism, empirical, 'Structural vs. internalized suppression mechanism for Gothic Arian communal identity.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(nicene_christological_kernel__homoousios_reading, 325, 451).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(nice_tr_t325, nicene_christological_kernel__homoousios_reading, theater_ratio, 325, 0.2).
narrative_ontology:measurement(nice_tr_t360, nicene_christological_kernel__homoousios_reading, theater_ratio, 360, 0.28).
narrative_ontology:measurement(nice_tr_t381, nicene_christological_kernel__homoousios_reading, theater_ratio, 381, 0.32).
narrative_ontology:measurement(nice_tr_t410, nicene_christological_kernel__homoousios_reading, theater_ratio, 410, 0.36).
narrative_ontology:measurement(nice_tr_t430, nicene_christological_kernel__homoousios_reading, theater_ratio, 430, 0.38).
narrative_ontology:measurement(nice_tr_t451, nicene_christological_kernel__homoousios_reading, theater_ratio, 451, 0.4).

% Extraction over time
narrative_ontology:measurement(nice_be_t325, nicene_christological_kernel__homoousios_reading, base_extractiveness, 325, 0.35).
narrative_ontology:measurement(nice_be_t360, nicene_christological_kernel__homoousios_reading, base_extractiveness, 360, 0.55).
narrative_ontology:measurement(nice_be_t381, nicene_christological_kernel__homoousios_reading, base_extractiveness, 381, 0.68).
narrative_ontology:measurement(nice_be_t410, nicene_christological_kernel__homoousios_reading, base_extractiveness, 410, 0.74).
narrative_ontology:measurement(nice_be_t430, nicene_christological_kernel__homoousios_reading, base_extractiveness, 430, 0.76).
narrative_ontology:measurement(nice_be_t451, nicene_christological_kernel__homoousios_reading, base_extractiveness, 451, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(nice_su_t325, nicene_christological_kernel__homoousios_reading, suppression_requirement, 325, 0.4).
narrative_ontology:measurement(nice_su_t360, nicene_christological_kernel__homoousios_reading, suppression_requirement, 360, 0.6).
narrative_ontology:measurement(nice_su_t381, nicene_christological_kernel__homoousios_reading, suppression_requirement, 381, 0.75).
narrative_ontology:measurement(nice_su_t410, nicene_christological_kernel__homoousios_reading, suppression_requirement, 410, 0.8).
narrative_ontology:measurement(nice_su_t430, nicene_christological_kernel__homoousios_reading, suppression_requirement, 430, 0.83).
narrative_ontology:measurement(nice_su_t451, nicene_christological_kernel__homoousios_reading, suppression_requirement, 451, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(nicene_christological_kernel__homoousios_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(nicene_christological_kernel__homoousios_reading, 0.08).
narrative_ontology:affects_constraint(nicene_christological_kernel__homoousios_reading, homoiousios_reading).

% DUAL FORMULATION NOTE:
% This story and homoiousios_reading form a two-member constraint family decomposing the single natural-language label 'the Nicene Christological controversy' into two structurally distinct claims per the ε-invariance principle. Each reading has its own beneficiary/victim structure, its own enforcement history, and its own ε trajectory over time — they are not the same constraint viewed from two angles, because at different historical moments each reading held imperial backing and enforced the other as heresy. Link is bidirectional in substance (both readings influenced the same underlying imperial administrative apparatus and traded institutional power), though this file only authors the outbound edge per schema requirements; the sibling file should author the return edge.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
