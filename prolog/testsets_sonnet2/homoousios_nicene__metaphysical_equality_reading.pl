% ============================================================================
% CONSTRAINT STORY: homoousios_nicene__metaphysical_equality_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_homoousios_nicene__metaphysical_equality_reading, []).

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
 *   constraint_id: homoousios_nicene__metaphysical_equality_reading
 *   human_readable: Homoousios as Full Ontological Equality of Father and Son (Nicene-Constantinopolitan Reading)
 *   domain: historical_theology/ecclesiastical_history/philosophy_of_religion
 *
 * SUMMARY:
 *   This story authors the metaphysical-equality reading of homoousios as
 *   adopted at Nicaea (325) and hardened at Constantinople (381): the Son
 *   shares numerically identical divine essence with the Father, is
 *   co-eternal, and stands in no relation of ontological subordination. This
 *   is one of three structurally distinct readings of the same contested
 *   kernel — the honorific-similarity reading (homoousios as likeness, not
 *   identity) and the subordinationist reading (shared divinity but derived,
 *   unequal being) are separate constraints with their own ε, beneficiaries,
 *   and victims. The metaphysical-equality reading, once backed by imperial
 *   enforcement under Theodosius I, becomes the boundary condition of
 *   ecclesiastical communion — anathematizing Arian, homoian, and eunomian
 *   Christologies and transferring episcopal office, property, and liturgical
 *   authority from those communities to the Nicene party. The extractiveness
 *   measured here is the referent under this reading's own lights: the
 *   standing arrangement (Nicene orthodoxy as enforced boundary), not the
 *   theological content the reading itself endorses.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(homoousios_nicene__metaphysical_equality_reading, 0.68).
domain_priors:suppression_score(homoousios_nicene__metaphysical_equality_reading, 0.86).
domain_priors:theater_ratio(homoousios_nicene__metaphysical_equality_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(homoousios_nicene__metaphysical_equality_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(homoousios_nicene__metaphysical_equality_reading, suppression_requirement, 0.86).
narrative_ontology:constraint_metric(homoousios_nicene__metaphysical_equality_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(homoousios_nicene__metaphysical_equality_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(homoousios_nicene__metaphysical_equality_reading, resistance, 0.74).

% --- Constraint claim ---
narrative_ontology:constraint_claim(homoousios_nicene__metaphysical_equality_reading, tangled_rope).
narrative_ontology:human_readable(homoousios_nicene__metaphysical_equality_reading, "Homoousios as Full Ontological Equality of Father and Son (Nicene-Constantinopolitan Reading)").
narrative_ontology:topic_domain(homoousios_nicene__metaphysical_equality_reading, "historical_theology/ecclesiastical_history/philosophy_of_religion").

domain_priors:requires_active_enforcement(homoousios_nicene__metaphysical_equality_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(homoousios_nicene__metaphysical_equality_reading, 'f0ad5a60-56b3-432c-8f25-fd634a9274f1').
narrative_ontology:cs_kernel_codification('f0ad5a60-56b3-432c-8f25-fd634a9274f1', formalized).
narrative_ontology:cs_authority_grounding('f0ad5a60-56b3-432c-8f25-fd634a9274f1', lineage).
narrative_ontology:cs_interpretation_layer_present('f0ad5a60-56b3-432c-8f25-fd634a9274f1').
narrative_ontology:cs_reading_relation('f0ad5a60-56b3-432c-8f25-fd634a9274f1', homoousios_nicene__subordinationist_reading, forecloses).
narrative_ontology:cs_reading_relation('f0ad5a60-56b3-432c-8f25-fd634a9274f1', homoousios_nicene__honorific_similarity_reading, forecloses).
narrative_ontology:cs_axiom('f0ad5a60-56b3-432c-8f25-fd634a9274f1', foundational, numerical_identity_of_divine_essence).
narrative_ontology:cs_axiom_status(numerical_identity_of_divine_essence, holdable).
narrative_ontology:cs_axiom_grounding('f0ad5a60-56b3-432c-8f25-fd634a9274f1', numerical_identity_of_divine_essence, theological).
narrative_ontology:cs_axiom('f0ad5a60-56b3-432c-8f25-fd634a9274f1', foundational, co_eternity_precludes_ontological_derivation).
narrative_ontology:cs_axiom_status(co_eternity_precludes_ontological_derivation, holdable).
narrative_ontology:cs_axiom_grounding('f0ad5a60-56b3-432c-8f25-fd634a9274f1', co_eternity_precludes_ontological_derivation, theological).
narrative_ontology:cs_axiom('f0ad5a60-56b3-432c-8f25-fd634a9274f1', secondary, no_subordination_in_being_permits_functional_order).
narrative_ontology:cs_axiom_status(no_subordination_in_being_permits_functional_order, holdable).
narrative_ontology:cs_axiom_grounding('f0ad5a60-56b3-432c-8f25-fd634a9274f1', no_subordination_in_being_permits_functional_order, theological).
narrative_ontology:cs_reference_frame('f0ad5a60-56b3-432c-8f25-fd634a9274f1', nicene_constantinopolitan_settlement).
narrative_ontology:cs_drift_state('f0ad5a60-56b3-432c-8f25-fd634a9274f1', post_reformation_and_modern_critical_scholarship, gap(practice_drift, minor, true)).
narrative_ontology:cs_created_at('f0ad5a60-56b3-432c-8f25-fd634a9274f1', '').
narrative_ontology:cs_kernel_id(homoousios_nicene__metaphysical_equality_reading, homoousios_nicene).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(homoousios_nicene__metaphysical_equality_reading, nicene_episcopal_hierarchy).
narrative_ontology:constraint_beneficiary(homoousios_nicene__metaphysical_equality_reading, imperial_church_settlement).
narrative_ontology:constraint_beneficiary(homoousios_nicene__metaphysical_equality_reading, athanasian_theological_faction).
narrative_ontology:constraint_victim(homoousios_nicene__metaphysical_equality_reading, arian_clergy).
narrative_ontology:constraint_victim(homoousios_nicene__metaphysical_equality_reading, subordinationist_congregations).
narrative_ontology:constraint_victim(homoousios_nicene__metaphysical_equality_reading, homoian_communities).
narrative_ontology:constraint_victim(homoousios_nicene__metaphysical_equality_reading, eunomian_theologians).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Bishops aligned with the Nicene formula draft, ratify, and enforce the homoousios boundary at council, then use imperial backing to depose rival bishops, close rival churches, and control creedal catechesis across the empire. They administer the interpretive apparatus that decides who counts as orthodox.
narrative_ontology:constraint_stakeholder(homoousios_nicene__metaphysical_equality_reading, nicene_episcopal_hierarchy, agenda_setter,
    institutional, civilizational, arbitrage, continental).

% Theologians who articulated and defended full ontological equality gain doctrinal victory, institutional patronage, and long-term canonical authority once their formula becomes the test of communion. They can move between sees and councils as imperial favor shifts, unlike the clergy whose livelihoods depend on a single congregation.
narrative_ontology:constraint_stakeholder(homoousios_nicene__metaphysical_equality_reading, athanasian_theological_faction, beneficiary,
    organized, civilizational, mobile, continental).
narrative_ontology:stakeholder_secondary_role(homoousios_nicene__metaphysical_equality_reading, athanasian_theological_faction, agenda_setter).

% The imperial state gains a single enforceable creedal standard usable to unify ecclesiastical administration across a fractious empire, backing council rulings with exile, property confiscation, and civil penalties against dissenting clergy.
narrative_ontology:constraint_stakeholder(homoousios_nicene__metaphysical_equality_reading, imperial_church_settlement, beneficiary,
    institutional, generational, arbitrage, continental).

% Clergy who held the Son derives being from the Father are deposed, exiled, or anathematized once homoousios becomes the boundary condition of communion. Their sees, congregations, and theological writings are systematically suppressed; recanting or exile are effectively the only options.
narrative_ontology:constraint_stakeholder(homoousios_nicene__metaphysical_equality_reading, arian_clergy, payer,
    moderate, biographical, trapped, continental).

% Ordinary believers whose received catechesis held a subordinate Son find their worshiping communities declared heretical, their church buildings reassigned, and their sacraments delegitimized by the new creedal test — a shift in metaphysical boundary imposed on communities who had no vote in the councils.
narrative_ontology:constraint_stakeholder(homoousios_nicene__metaphysical_equality_reading, subordinationist_congregations, payer,
    powerless, biographical, trapped, regional).

% Communities holding the Son is like the Father (without asserting identical essence) persist for generations under alternating imperial favor, but face repeated waves of suppression once the metaphysical-equality reading becomes fixed orthodoxy under Theodosian enforcement.
narrative_ontology:constraint_stakeholder(homoousios_nicene__metaphysical_equality_reading, homoian_communities, payer,
    moderate, generational, constrained, regional).

% Theologians arguing for a strong ontological distinction between Father and Son are barred from council deliberation once their position is pre-anathematized; their arguments enter the historical record chiefly through hostile citation by opponents who preserved them in order to refute them.
narrative_ontology:constraint_stakeholder(homoousios_nicene__metaphysical_equality_reading, eunomian_theologians, excluded,
    moderate, biographical, trapped, regional).

% Scholars reconstructing the fourth-century controversies from council acts, letters, and fragments preserved mostly by the winning faction, assessing how much of the surviving record reflects genuine theological consensus versus retrospective consolidation of the victors' narrative.
narrative_ontology:constraint_stakeholder(homoousios_nicene__metaphysical_equality_reading, later_church_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(homoousios_nicene__metaphysical_equality_reading, nicene_episcopal_hierarchy).
narrative_ontology:fixing_cost_class(homoousios_nicene__metaphysical_equality_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single, enforceable answer to the question of Christ's relationship to God the Father, allowing a fractured and doctrinally diverse church to speak with one creedal voice, coordinate liturgy, and present a unified front to imperial authority and rival religious movements.
% TRANSFER_FUNCTION: Moves interpretive authority, ecclesiastical office, and property from clergy and congregations holding subordinationist or homoian christologies to those holding the Nicene metaphysical-equality formula, backed by imperial coercive enforcement after Theodosius.
% ABSENT_VOICES: Arian, homoian, and eunomian clergy who were deposed or exiled are structurally absent from the councils that anathematized them after the Nicene-Constantinopolitan settlement hardened; their theological positions survive mainly as quotations embedded in refutations written by the winning side.
% DISAPPEARANCE_RATIONALE: If the metaphysical-equality reading of homoousios were not established as the boundary of communion, the entire subsequent architecture of Trinitarian orthodoxy, creedal tests for ordination, and the historical exclusion of Arian and homoian churches would not have taken the shape it did — ecclesiastical office-holding, liturgical formulas, and the very category of 'heresy' applied to subordinationist Christology depend on this reading having won.
% FOUNDING_PROBLEM: The early church faced genuine, unresolved disagreement about how to understand the relationship between Jesus Christ and God the Father in a way consistent with monotheism, apostolic tradition, and lived liturgical practice — a real theological and pastoral problem requiring some resolution to prevent indefinite fragmentation.
% FOUNDING_PROBLEM_CORROBORATION: Nicene and later orthodox historians attest the problem was genuinely resolved by sound theological reasoning at Nicaea and Constantinople. Independent historians of late antiquity (outside the confessional tradition that benefited from the outcome) attest that the resolution was substantially shaped by imperial political consolidation under Constantine and Theodosius, and that competing christologies remained live, populous, and theologically serious for over a century after 325 CE — suggesting the 'settled' status is itself a product of the enforcement this constraint records, not solely of theological argument.
narrative_ontology:disappearance_verdict(homoousios_nicene__metaphysical_equality_reading, world_rearranges).
narrative_ontology:founding_problem_status(homoousios_nicene__metaphysical_equality_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(homoousios_nicene__metaphysical_equality_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(homoousios_nicene__metaphysical_equality_reading, 'none', 1).
narrative_ontology:epsilon_provenance(homoousios_nicene__metaphysical_equality_reading, 0.68, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(homoousios_nicene__metaphysical_equality_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(homoousios_nicene__metaphysical_equality_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(homoousios_nicene__metaphysical_equality_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction rises from 0.35 at Nicaea (325, when enforcement was politically contested and imperial backing intermittent) to 0.68 by 425 (a century after Constantinople, when Theodosian legal enforcement, confiscation of Arian churches, and exile of dissenting clergy had become routine administrative practice). Suppression tracks a steeper climb (0.40 to 0.86) because the coercive apparatus — imperial edicts, forced subscription, exile of bishops who would not sign — matured faster than the underlying extraction of office and property. Theater ratio stays comparatively low (0.28 by 425) because the coordination function (a genuinely needed answer to a real Christological question) remained substantively load-bearing throughout; this is not a hollowed-out constraint performing a function it no longer serves, it is a still-functioning boundary whose enforcement cost has grown alongside its scope.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setting episcopal hierarchy's seat, homoousios-as-equality is settled orthodoxy defended against heresy — the natural, obviously correct reading. From the seat of an Arian or homoian congregation stripped of its church building and its clergy exiled, the same formula is an imposed metaphysical boundary enforced by imperial coercion against a theologically serious alternative. The engine computes these as different seat-level classifications from the same structural data; the divergence is the finding, not an error to reconcile.
 *
 * DIRECTIONALITY LOGIC:
 *   The Nicene episcopal hierarchy and the Athanasian theological faction are structural beneficiaries: they gain office, doctrinal authority, and imperial patronage from the boundary's enforcement, and both retain mobility (bishops moved between sees, theologians between courts) that dissenting clergy lacked. Arian clergy, homoian communities, subordinationist congregations, and eunomian theologians are targets: they bear deposition, exile, property loss, and communal delegitimization, and their exit options are trapped or at best constrained — recantation, exile, or continued practice under increasing legal jeopardy. The imperial state is a beneficiary of a different kind: it gains administrative unification, not doctrinal content, from the boundary's existence.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — how to state Christ's relationship to the Father without collapsing into modalism, tritheism, or subordinationism — was a genuine, live theological and pastoral problem in the early fourth century; this prevents classifying the whole affair as pure invented pretext. But by the time of Theodosian enforcement the same boundary was also functioning to consolidate episcopal power and imperial administrative unity independent of continuing theological deliberation — the tangled_rope classification holds both a real coordination function (resolving a genuine doctrinal question the church needed answered) and asymmetric extraction (transferring office and property from the losing faction) through the same structure, requiring active enforcement to persist rather than resting on unforced doctrinal consensus.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    theological_consensus_vs_imperial_consolidation,
    'Was the fixing of the metaphysical-equality reading as orthodox primarily the outcome of theological argument reaching genuine consensus, or primarily the outcome of imperial political consolidation using theological language as its vehicle?',
    'Comparative analysis of council attendance and voting patterns under differing imperial administrations; examination of how quickly doctrinal ''consensus'' shifted when imperial favor shifted (e.g., under Constantius II''s favor toward homoian positions versus Theodosius''s favor toward Nicene positions) — rapid doctrinal reversal correlated with imperial succession would support the political-consolidation reading.',
    'If theological argument was doing the real work, the coordination function dominates and the classification should weight toward rope; if imperial consolidation was doing the real work and theology was largely post-hoc justification, the classification should weight further toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(theological_consensus_vs_imperial_consolidation, conceptual, 'Whether Nicene orthodoxy''s fixation reflects genuine theological resolution or political consolidation using theological form.').

omega_variable(
    kernel_reading_indeterminacy,
    'Does the Greek term homoousios as used at Nicaea (325) itself underdetermine between the metaphysical-equality, subordinationist-compatible, and honorific-similarity readings, such that the Constantinopolitan (381) settlement represents a genuine clarification or an imposed narrowing of an originally ambiguous term?',
    'Philological analysis of homoousios''s usage in pre-Nicene sources (including its earlier condemnation at the Synod of Antioch, 268, for Sabellian associations) compared against its 325 and 381 usages, cross-referenced against contemporaneous non-Nicene theological vocabulary.',
    'High indeterminacy would strengthen the case that the metaphysical-equality reading''s dominance is a contingent, enforced outcome among several live linguistic possibilities rather than the term''s necessary or exclusive meaning — reinforcing high suppression and the tangled_rope classification''s extraction component.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_indeterminacy, empirical, 'Whether homoousios''s original semantic range genuinely underdetermines the three sibling readings or already favored one.').

omega_variable(
    surviving_record_selection_bias,
    'How much does the near-total loss of primary Arian, homoian, and eunomian theological writings (preserved mainly as quotations in hostile Nicene refutations) bias later historical assessment of how contested or settled the metaphysical-equality reading actually was at the time?',
    'Fragment reconstruction efforts and cross-checking against non-Christian or peripheral Christian sources (e.g., Gothic Arian communities, later Byzantine heresiological catalogs) that preserved independent testimony to the theological seriousness and popular reach of the losing positions.',
    'If the losing positions were far more theologically sophisticated and popularly entrenched than the surviving hostile record suggests, the suppression and victim-set characterization in this story understate the scale of what was displaced; if the fragments broadly corroborate the hostile characterization, the current framing holds.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(surviving_record_selection_bias, empirical, 'Whether the victor-preserved historical record understates the theological seriousness of the anathematized positions.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(homoousios_nicene__metaphysical_equality_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(homo_tr_t0, homoousios_nicene__metaphysical_equality_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(homo_tr_t20, homoousios_nicene__metaphysical_equality_reading, theater_ratio, 20, 0.15).
narrative_ontology:measurement(homo_tr_t40, homoousios_nicene__metaphysical_equality_reading, theater_ratio, 40, 0.19).
narrative_ontology:measurement(homo_tr_t60, homoousios_nicene__metaphysical_equality_reading, theater_ratio, 60, 0.22).
narrative_ontology:measurement(homo_tr_t80, homoousios_nicene__metaphysical_equality_reading, theater_ratio, 80, 0.25).
narrative_ontology:measurement(homo_tr_t100, homoousios_nicene__metaphysical_equality_reading, theater_ratio, 100, 0.28).

% Extraction over time
narrative_ontology:measurement(homo_be_t0, homoousios_nicene__metaphysical_equality_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(homo_be_t20, homoousios_nicene__metaphysical_equality_reading, base_extractiveness, 20, 0.48).
narrative_ontology:measurement(homo_be_t40, homoousios_nicene__metaphysical_equality_reading, base_extractiveness, 40, 0.58).
narrative_ontology:measurement(homo_be_t60, homoousios_nicene__metaphysical_equality_reading, base_extractiveness, 60, 0.63).
narrative_ontology:measurement(homo_be_t80, homoousios_nicene__metaphysical_equality_reading, base_extractiveness, 80, 0.66).
narrative_ontology:measurement(homo_be_t100, homoousios_nicene__metaphysical_equality_reading, base_extractiveness, 100, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(homo_su_t0, homoousios_nicene__metaphysical_equality_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(homo_su_t20, homoousios_nicene__metaphysical_equality_reading, suppression_requirement, 20, 0.55).
narrative_ontology:measurement(homo_su_t40, homoousios_nicene__metaphysical_equality_reading, suppression_requirement, 40, 0.7).
narrative_ontology:measurement(homo_su_t60, homoousios_nicene__metaphysical_equality_reading, suppression_requirement, 60, 0.8).
narrative_ontology:measurement(homo_su_t80, homoousios_nicene__metaphysical_equality_reading, suppression_requirement, 80, 0.84).
narrative_ontology:measurement(homo_su_t100, homoousios_nicene__metaphysical_equality_reading, suppression_requirement, 100, 0.86).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(homoousios_nicene__metaphysical_equality_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(homoousios_nicene__metaphysical_equality_reading, 0.1).
narrative_ontology:affects_constraint(homoousios_nicene__metaphysical_equality_reading, subordinationist_reading).
narrative_ontology:affects_constraint(homoousios_nicene__metaphysical_equality_reading, honorific_similarity_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three siblings decomposing the natural-language label 'the homoousios controversy' per the ε-invariance principle: metaphysical_equality_reading (this file, ε=0.68, tangled_rope — coordination around a genuine Christological question plus asymmetric extraction via imperial enforcement against dissenting clergy), subordinationist_reading (separate file — the Son shares divinity but derives being unequally from the Father), and honorific_similarity_reading (separate file — homoousios as likeness rather than strict identity, a lower-suppression, lower-extraction reading closer to a rope where it survived as a minority position). The three do not share ε; each is authored independently and linked here for contamination-propagation analysis, since the metaphysical-equality reading's eventual imperial enforcement directly suppressed the other two as live ecclesiastical options.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
