% ============================================================================
% CONSTRAINT STORY: homoousios_christology__arian_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_homoousios_christology__arian_reading, []).

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
 *   constraint_id: homoousios_christology__arian_reading
 *   human_readable: Arian Doctrinal Arrangement: Christ as Created and Subordinate
 *   domain: historical theology / ecclesiastical politics / commitment systems
 *
 * SUMMARY:
 *   This story instantiates ONE reading — the arian_reading — of the
 *   contested kernel homoousios_christology (who is Christ relative to the
 *   Father?). The constraint under description is the standing Arian
 *   arrangement itself: the ecclesiastical order requiring Christ to be
 *   confessed as created and subordinate, not of identical substance with the
 *   Father, administered by a distributed network of non-Nicene bishops,
 *   backed at intervals by imperial courts, and carried to the Goths as a
 *   national church with its own scriptural text. Per the epsilon-referent
 *   rule, epsilon's referent is THIS arrangement as the reading's own lights
 *   assess it — not the pro-Nicene establishment this reading contests, and
 *   not some alternative arrangement it would prefer (it prefers itself; the
 *   referent is what it operates). The claim/metric gap is deliberate and is
 *   the measurement: the reading's self-presentation is a rope (coordination
 *   around scriptural truth), while the structural record this story authors
 *   — coerced subscriptions at Ariminum, deposed sees, Gothic-territory
 *   persecution — supports the tangled_rope claim. Sibling readings
 *   (pro_nicene_reading, semi_arian_reading) are separate constraint stories
 *   with their own epsilon, victims, and enforcement structures; they are
 *   linked via network.affects_constraints and are not adjudicated inside
 *   this file. KEY AGENTS (by structural relationship): -
 *   arian_episcopal_network: agenda-setter (institutional / identity_locked)
 *   — administers the confession, staffs councils, receives vacated sees -
 *   homoian_imperial_court: beneficiary with enforcement leverage
 *   (institutional / arbitrage) — imposes or drops the formula at political
 *   will - gothic_arian_church: beneficiary (organized / identity_locked) —
 *   national church fused with Gothic identity -
 *   nicene_bishops_under_arian_jurisdiction: primary target (organized /
 *   identity_locked) — deposed, exiled, offered reinstatement on subscription
 *   - catholic_christians_in_gothic_territory: primary target (powerless /
 *   trapped) — excluded from churches, no exit -
 *   baptized_laity_of_arian_communities: excluded seat (moderate /
 *   constrained) — receives formulas, sets none - ecclesiastical_historians:
 *   analytical observer (analytical / analytical) — sees the full structure
 *
 * KEY AGENTS:
 *   - arian_episcopal_network: agenda-setter (institutional / identity_locked) — administers the confession, staffs councils, receives vacated sees
 *   - homoian_imperial_court: beneficiary with enforcement leverage (institutional / arbitrage) — imposes or drops the formula at political will
 *   - gothic_arian_church: beneficiary (organized / identity_locked) — national church fused with Gothic identity
 *   - nicene_bishops_under_arian_jurisdiction: primary target (organized / identity_locked) — deposed, exiled, offered reinstatement on subscription
 *   - catholic_christians_in_gothic_territory: primary target (powerless / trapped) — excluded from churches, no exit
 *   - baptized_laity_of_arian_communities: excluded seat (moderate / constrained) — receives formulas, sets none
 *   - ecclesiastical_historians: analytical observer (analytical / analytical) — sees the full structure
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(homoousios_christology__arian_reading, 0.31).
domain_priors:suppression_score(homoousios_christology__arian_reading, 0.58).
domain_priors:theater_ratio(homoousios_christology__arian_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(homoousios_christology__arian_reading, extractiveness, 0.31).
narrative_ontology:constraint_metric(homoousios_christology__arian_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(homoousios_christology__arian_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(homoousios_christology__arian_reading, accessibility_collapse, 0.32).
narrative_ontology:constraint_metric(homoousios_christology__arian_reading, resistance, 0.78).

% --- Constraint claim ---
narrative_ontology:constraint_claim(homoousios_christology__arian_reading, tangled_rope).
narrative_ontology:human_readable(homoousios_christology__arian_reading, "Arian Doctrinal Arrangement: Christ as Created and Subordinate").
narrative_ontology:topic_domain(homoousios_christology__arian_reading, "historical theology / ecclesiastical politics / commitment systems").

domain_priors:requires_active_enforcement(homoousios_christology__arian_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(homoousios_christology__arian_reading, '819bbd81-54a1-4edc-8566-66e1dbd07a50').
narrative_ontology:cs_kernel_codification('819bbd81-54a1-4edc-8566-66e1dbd07a50', fixed_text).
narrative_ontology:cs_authority_grounding('819bbd81-54a1-4edc-8566-66e1dbd07a50', distributed).
narrative_ontology:cs_reading_relation('819bbd81-54a1-4edc-8566-66e1dbd07a50', homoousios_christology__pro_nicene_reading, forecloses).
narrative_ontology:cs_reading_relation('819bbd81-54a1-4edc-8566-66e1dbd07a50', homoousios_christology__semi_arian_reading, coexists_with).
narrative_ontology:cs_axiom('819bbd81-54a1-4edc-8566-66e1dbd07a50', foundational, son_is_created_not_coeternal).
narrative_ontology:cs_axiom_status(son_is_created_not_coeternal, holdable).
narrative_ontology:cs_axiom_grounding('819bbd81-54a1-4edc-8566-66e1dbd07a50', son_is_created_not_coeternal, theological).
narrative_ontology:cs_axiom('819bbd81-54a1-4edc-8566-66e1dbd07a50', foundational, father_alone_unbegotten_source).
narrative_ontology:cs_axiom_status(father_alone_unbegotten_source, holdable).
narrative_ontology:cs_axiom_grounding('819bbd81-54a1-4edc-8566-66e1dbd07a50', father_alone_unbegotten_source, theological).
narrative_ontology:cs_axiom('819bbd81-54a1-4edc-8566-66e1dbd07a50', secondary, scriptural_terms_bind_creeds_do_not).
narrative_ontology:cs_axiom_status(scriptural_terms_bind_creeds_do_not, holdable).
narrative_ontology:cs_axiom_grounding('819bbd81-54a1-4edc-8566-66e1dbd07a50', scriptural_terms_bind_creeds_do_not, conventional).
narrative_ontology:cs_reference_frame('819bbd81-54a1-4edc-8566-66e1dbd07a50', pre_nicene_subordinationist_tradition).
narrative_ontology:cs_drift_state('819bbd81-54a1-4edc-8566-66e1dbd07a50', post_constantinople_381, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('819bbd81-54a1-4edc-8566-66e1dbd07a50', '').
narrative_ontology:cs_kernel_id(homoousios_christology__arian_reading, homoousios_christology).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(homoousios_christology__arian_reading, arian_episcopal_network).
narrative_ontology:constraint_beneficiary(homoousios_christology__arian_reading, homoian_imperial_court).
narrative_ontology:constraint_beneficiary(homoousios_christology__arian_reading, gothic_arian_church).
narrative_ontology:constraint_victim(homoousios_christology__arian_reading, nicene_bishops_under_arian_jurisdiction).
narrative_ontology:constraint_victim(homoousios_christology__arian_reading, catholic_christians_in_gothic_territory).
narrative_ontology:constraint_vindicates(homoousios_christology__arian_reading, subordinationist_exegesis).
narrative_ontology:constraint_vindicates(homoousios_christology__arian_reading, father_transcendence_doctrine).
narrative_ontology:constraint_vindicates(homoousios_christology__arian_reading, scriptural_terms_bind_creeds_do_not).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Bishops and court-connected clergy — Arius's defenders, the Eusebian circle, later the Homoian hierarchy — who draft counter-creeds, staff and convene councils, ordain compliant clergy, and refill sees vacated by deposed dissenters. Their office and standing are constituted by the confession they administer; renouncing it ends their authority. Enforcement flows through them: subscription demands, deposition proceedings, and the discipline of their own clergy.
narrative_ontology:constraint_stakeholder(homoousios_christology__arian_reading, arian_episcopal_network, agenda_setter,
    institutional, generational, identity_locked, continental).

% Emperors and their officials, who find a non-metaphysical formula administratively useful: it can be imposed by edict when unity is needed (Constantius II after 353, Valens in the East) and dropped when politics changes (Julian's recall of all exiles in 361). They collect episcopal compliance and administrative peace; they do not administer the doctrine and can withdraw support at will — and repeatedly did.
narrative_ontology:constraint_stakeholder(homoousios_christology__arian_reading, homoian_imperial_court, beneficiary,
    institutional, biographical, arbitrage, continental).
narrative_ontology:stakeholder_secondary_role(homoousios_christology__arian_reading, homoian_imperial_court, agenda_setter).

% The church founded through Ulfilas's mission: its own hierarchy, its own liturgical language in Gothic script, its own communion separate from imperial Catholics. Its clergy's standing depends on maintaining the separate structure, and the structure is fused with Gothic political identity against Rome; conversion to the imperial confession would dissolve the boundary the church exists to keep.
narrative_ontology:constraint_stakeholder(homoousios_christology__arian_reading, gothic_arian_church, beneficiary,
    organized, generational, identity_locked, regional).

% Bishops who confess the Son as homoousios living under jurisdictions the network administers. They face deposition, exile (Athanasius five times; Hosius pressed in extreme old age; Liberius offered reinstatement on signature), and their refusals are treated as obstinacy. Their exit is apostasy as they understand it; their resistance is organized through monastic networks, popular support in the great sees, and eventually the Roman church.
narrative_ontology:constraint_stakeholder(homoousios_christology__arian_reading, nicene_bishops_under_arian_jurisdiction, payer,
    organized, biographical, identity_locked, continental).

% Catholics living under Gothic Arian rule — in Tervingi lands under Athanaric's persecutions, and later inside the Visigothic and Ostrogothic kingdoms. They are excluded from churches handed to the Arian hierarchy, their clergy exiled or restricted, their worship surveilled. They have no exit from Gothic territory and no protector once the imperial frontier is behind them.
narrative_ontology:constraint_stakeholder(homoousios_christology__arian_reading, catholic_christians_in_gothic_territory, payer,
    powerless, generational, trapped, regional).

% The baptized who receive whatever formula the councils and courts settle on. They hold no seat in councils and no voice in courts; some riot over deposed bishops or shelter exiled clergy, and their riots occasionally move episcopal politics, but the formula is never theirs to set. Leaving the community is the only exit from its confession.
narrative_ontology:constraint_stakeholder(homoousios_christology__arian_reading, baptized_laity_of_arian_communities, excluded,
    moderate, generational, constrained, continental).

% Reconstruct the arrangement from council acta, creed texts, exile correspondence, and the Gothic scriptural record. They hold none of the positions, bear none of the costs, and see the full structure: the exegetical dispute, the enforcement machinery, the imperial alternation, and the identity fusion of the Gothic phase.
narrative_ontology:constraint_stakeholder(homoousios_christology__arian_reading, ecclesiastical_historians, observer,
    analytical, civilizational, analytical, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(homoousios_christology__arian_reading, arian_episcopal_network).
narrative_ontology:fixing_cost_class(homoousios_christology__arian_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves a real confession problem for communities that found the Nicene formula unscriptural: it gives bishops, missionaries, and converts a shared answer to 'who is Christ' — the Son as first creature and mediator of creation, the Father alone unbegotten — coordinating baptismal creeds, liturgy, ordination, and teaching across scattered non-Nicene communities, and for the Goths, coordinating a national church with its own scriptural text in the Gothic language.
% TRANSFER_FUNCTION: Moves ecclesiastical office and imperial favor from bishops who will not subscribe to those who will (deposed sees refilled by subscribers); moves coerced assent from dissenting clergy and laity to the enforced formula; moves religious legitimacy from the homoousios confession to the creature-confession; in the Gothic phase, moves communal identity itself — Arian Christianity as the marker separating Goth from Roman.
% ABSENT_VOICES: The baptized laity had no seat in any council that fixed a formula; Nicene dissenters were silenced by deposition before they could speak in the jurisdictions that deposed them (their objections survive mainly in exile correspondence); the homoiousian middle party signed homoian formulas they did not hold, their actual position erased from the subscription record. All three would object to the formulas as settled; none was in the room when settlement happened.
% DISAPPEARANCE_RATIONALE: If the arrangement vanished overnight, the Gothic churches lose their separate liturgical language and hierarchy and the Gothic-Roman religious boundary collapses; the imperial court loses its flexible unity formula and must adjudicate the christology question on the merits; the episcopal network's offices and sees — constituted by the confession — dissolve with it; and the Nicene party loses the opponents whose pressure forced the Cappadocian settlement. The ecclesiastical map of the fourth through sixth centuries rearranges around the absence.
% FOUNDING_PROBLEM: How to confess the Father's unbegotten uniqueness and monotheistic transcendence while also confessing the Son's saving-divine role: if the Son is truly God, is there a second God? Arius's answer — the Son is the first and greatest creature, begotten before all ages but not eternal — was built to preserve the Father's transcendence at the cost of the Son's coeternity.
% FOUNDING_PROBLEM_CORROBORATION: The problem is attested from outside the benefiting parties: the Nicene opponents took it seriously enough to answer it with a rival metaphysics (Athanasius, then the Cappadocians — one ousia, three hypostases), which presupposes the problem is real; Ammianus Marcellinus, a pagan observer with no stake in either confession, attests the disputes' reality and intensity; modern patristic scholarship corroborates the exegetical seriousness of the pre-325 dispute. What no outside source attests is that THIS arrangement was the necessary answer — the corroboration covers the problem, not the arrangement's enforcement of its solution.
narrative_ontology:disappearance_verdict(homoousios_christology__arian_reading, world_rearranges).
narrative_ontology:founding_problem_status(homoousios_christology__arian_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(homoousios_christology__arian_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(homoousios_christology__arian_reading, 'none', 1).
narrative_ontology:epsilon_provenance(homoousios_christology__arian_reading, 0.31, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(homoousios_christology__arian_reading_tests).
:- end_tests(homoousios_christology__arian_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.31) is reading-indexed: the Arian reading assessing its own arrangement sees coordination around what it holds as scriptural necessity (the Father alone unbegotten; the Son the first creature and mediator), and counts the discipline of dissenters as correction rather than extraction — so its self-assessed epsilon sits modestly above the identity-coordination floor. The declared victims are structural facts the reading's self-assessment omits; the omega reading_indexed_epsilon_blindspot carries that gap. Suppression (0.58) is authored as a raw structural property: the arrangement could not persist on assent alone — it required counter-councils, imperial edicts, deposition proceedings, and subscription compulsion, and its enforcement requirement oscillated with imperial favor (peak 0.78 at Ariminum/Seleucia 359, collapse to 0.35 under Julian 361, reconstitution 0.62 after Constantinople 381 when only Gothic royal power enforced it). The suppression_requirement series is authored because enforcement-capacity change IS the tracked dynamic here — machinery built, collapsed, rebuilt on a new base, then abandoned at Toledo. The scalar 0.58 represents the arrangement's operative-phase enforcement level (341-410), not the 589 dissolution endpoint; the series carries the lifecycle. Theater_ratio (0.22) is low: the arrangement's activity was mostly functional — creed drafting, ordination, mission, Ulfilas's Gothic translation — with a sharp theatrical peak at 359 (0.38), when deliberately vague homoios formulas were engineered for mass signature and bishops subscribed without conviction. Accessibility_collapse (0.32): the rival readings never became inaccessible — homoousios remained the live, ultimately victorious alternative throughout; the arrangement spent its existence contesting an accessible rival. Resistance (0.78): sustained, organized, and ultimately victorious resistance from Athanasius through the Cappadocians to Constantinople 381. All three series run on one shared time grid (318, 325, 341, 359, 361, 381, 410, 589) so every metric is authored at every examined point. The claimed_type (tangled_rope) is authored from the structural record — genuine coordination function plus enforced asymmetry plus required active enforcement — independently of the reading's rope-shaped self-presentation; the divergence is data, not an error to tune away.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently from the same structure. From the arian_episcopal_network seat, the arrangement is truth defended under persecution — a coordination it built, staffed, and bled for; the classification computed at that seat should reflect defended coordination. From the nicene_bishops seat, the same structure is enforced extraction with identity-locked exit: deposition, exile, subscription-or-ruin, with no exit that is not apostasy. From the homoian_imperial_court seat, it is a disposable administrative instrument — no doctrine, only compliance, dropped the moment politics changes. From the Gothic-territory Catholic seat it is confiscation and exclusion with no exit at all. The reading-indexed epsilon (0.31) is the network-adjacent seat's view; the payer seats should compute substantially higher effective extraction, and that per-seat divergence is the corpus measurement this story exists to supply.
 *
 * DIRECTIONALITY LOGIC:
 *   The beneficiary/victim declarations plus exit options drive the derivation, and no directionality overrides are needed: the network (beneficiary, identity_locked) derives near the beneficiary end — it collects sees and standing, and its only cost is defending its own position; the court (beneficiary, arbitrage) derives nearest the beneficiary end — it collects administrative unity on demand and exits at will, as Julian demonstrated in 361; the Gothic church (beneficiary, identity_locked) collects a national structure fused with its people's political identity. The Nicene bishops (payer, identity_locked) derive near the full-target end — they bear the transfer of office, liberty, and communion and cannot exit without apostasy as they understand it; Gothic-territory Catholics (payer, trapped) sit at the full-target end. The excluded laity derive near-symmetric: teaching received, conformity borne, formula never set. Receipt: the extraction's gains demonstrably accrue to the episcopal network — deposed sees refilled by subscribers, imperial favor, confiscated churches — so gain_flow names that seat rather than 'diffuse'. Cost-to-fix: every seat holding enforcement power over the arrangement could drop or switch it cheaply, and did (Constantius imposed, Julian dropped, Valens re-imposed, Theodosius replaced, the Visigothic crown abandoned in 589); what was prohibitive was the network's own exit, not the fixing — the arrangement outlived its enforcers because the capturer could not leave, not because removal was expensive. Hence fixing_cost: cheap.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — confessing the Father's unbegotten uniqueness without dissolving the Son's mediating role — never died; every later christology re-encounters it, which is why founding_problem_status is live even though this reading's answer lost. What atrophied was the arrangement's argumentative function: by the Gothic phase the Gothic clergy were maintaining an inherited confession rather than generating christological argument, and the arrangement's operative function had shifted to identity boundary-keeping (Goth versus Roman). The classification prevents mislabeling in both directions: reading the arrangement as pure extraction erases the genuine exegetical coordination of its first decades and the substantive mission work (Ulfilas's translation project was real scholarship with real coordination yield); reading it as pure coordination erases the coerced subscriptions, the depositions, and the Gothic-territory persecution. It did not decay into a piton because concentrated beneficiaries still collected to the end — the network's capture kept it tangled rather than inert; and when the Gothic crown's benefit calculation flipped in 589, the arrangement dissolved rather than lingering as performance, confirming that what held it up was enforcement plus capture, not theatrical inertia alone.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_position,
    'This constraint is the arian_reading of the homoousios_christology kernel; what would each sibling reading change structurally, and where exactly is the disagreement located?',
    'Compare the three family stories'' victim sets, enforcement structures, and reference frames: under the pro_nicene_reading the payer seats are Arian clergy facing imperial-Nicene enforcement; under the semi_arian_reading the coercion record is the extracted Ariminum signatures. The disagreement is located in the Son''s ontological status — created versus unbegotten — which determines who bears coercion in each arrangement.',
    'The readings are not one constraint measured differently; each instantiates a distinct arrangement with a distinct epsilon and distinct victims. Cross-reading comparisons must run through the family links, not through a shared classification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_position, conceptual, 'Committer structure: one of three readings of the homoousios kernel; siblings would invert the victim set or blur it.').

omega_variable(
    assent_vs_enforcement_persistence,
    'Did the Arian arrangement persist by genuine assent (communities convinced by the subordinationist exegesis) or by enforcement (imperial edict, Gothic royal power, subscription compulsion)?',
    'Compare subscription behavior under free competition (361-364, after Julian''s recall) with behavior under coerced periods; examine the Gothic case, where conversion was elite-driven from the crown and military caste downward. If free-competition adherence collapses relative to coerced-period adherence, enforcement carried the arrangement.',
    'If enforcement-driven, the arrangement''s suppression is structural and its enforcement phases classify snare-ward; if assent-driven, the coordination reading strengthens and the enforcement record is defensive rather than extractive.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(assent_vs_enforcement_persistence, empirical, 'Whether persistence ran on conviction or coercion — the central structural ambiguity of the arrangement''s lifecycle.').

omega_variable(
    reading_indexed_epsilon_blindspot,
    'The authored epsilon (0.31) is reading-indexed: it is the Arian arrangement assessed by the Arian reading''s own lights, under which doctrinal discipline of dissenters is correction, not extraction. What would epsilon be authored as from the payer seats?',
    'Re-author epsilon from the nicene_bishops_under_arian_jurisdiction and catholic_christians_in_gothic_territory seats, for whom the same arrangement is deposition, exile, and confiscation; compare against the reading-indexed value.',
    'From payer seats epsilon would be substantially higher, and per-seat classifications should diverge sharply from the reading''s self-assessment; that divergence is precisely the measurement this corpus exists to take, not an error to reconcile.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_indexed_epsilon_blindspot, conceptual, 'The gap between the reading''s self-assessed extraction and the extraction its declared victims experience.').

omega_variable(
    gothic_identity_fusion,
    'How much of the arrangement''s Gothic phase (c. 348-589) was held by theological conviction versus national-identity fusion — Arian Christianity as the boundary marker distinguishing Goth from Roman?',
    'Examine the speed and completeness of abandonment at the Third Council of Toledo (589) following the royal conversion of Reccared: a confession abandoned wholesale within a generation of the crown''s switch indicates identity-political rather than doctrinal commitment. Cross-check against internal Gothic dissent records and the survival of Gothic-script biblical tradition after conversion.',
    'If identity-political, the Gothic phase is identity coordination whose doctrinal content was partly theatrical; the clergy''s identity lock broke with the crown''s switch rather than with argument, and the arrangement''s late-period persistence was dynastic, not exegetical.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(gothic_identity_fusion, empirical, 'Whether the Gothic phase ran on doctrine or on national-identity fusion with doctrine as marker.').

omega_variable(
    homoiousian_coerced_signatures,
    'The semi_arian_reading''s bishops subscribed homoian formulas under pressure at Ariminum and Seleucia (359) that they did not hold; do those coerced signatures count as extraction by THIS arrangement, or as evidence that this arrangement was itself a defensive compromise under pressure?',
    'Compare the formula texts each episcopal party actually held against the texts they signed, using the dissimulation and protest literature surrounding Ariminum; distinguish the Homoian center from the homoiousian middle party.',
    'If extraction, this arrangement''s enforcement phase extracted from a third party beyond its declared victims, raising its effective extractiveness; if compromise, the arrangement''s center of gravity was defensive, and the coercion record reflects the same imperial machinery both readings suffered under.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(homoiousian_coerced_signatures, conceptual, 'Whether the coerced middle-party signatures are this arrangement''s extraction or shared imperial pressure.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(homoousios_christology__arian_reading, 318, 589).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(arian_reading_tr_t318, homoousios_christology__arian_reading, theater_ratio, 318, 0.1).
narrative_ontology:measurement_basis(arian_reading_tr_t318, observed).
narrative_ontology:measurement(arian_reading_tr_t325, homoousios_christology__arian_reading, theater_ratio, 325, 0.15).
narrative_ontology:measurement_basis(arian_reading_tr_t325, observed).
narrative_ontology:measurement(arian_reading_tr_t341, homoousios_christology__arian_reading, theater_ratio, 341, 0.2).
narrative_ontology:measurement_basis(arian_reading_tr_t341, observed).
narrative_ontology:measurement(arian_reading_tr_t359, homoousios_christology__arian_reading, theater_ratio, 359, 0.38).
narrative_ontology:measurement_basis(arian_reading_tr_t359, observed).
narrative_ontology:measurement(arian_reading_tr_t361, homoousios_christology__arian_reading, theater_ratio, 361, 0.3).
narrative_ontology:measurement_basis(arian_reading_tr_t361, observed).
narrative_ontology:measurement(arian_reading_tr_t381, homoousios_christology__arian_reading, theater_ratio, 381, 0.26).
narrative_ontology:measurement_basis(arian_reading_tr_t381, observed).
narrative_ontology:measurement(arian_reading_tr_t410, homoousios_christology__arian_reading, theater_ratio, 410, 0.24).
narrative_ontology:measurement_basis(arian_reading_tr_t410, observed).
narrative_ontology:measurement(arian_reading_tr_t589, homoousios_christology__arian_reading, theater_ratio, 589, 0.22).
narrative_ontology:measurement_basis(arian_reading_tr_t589, observed).

% Extraction over time
narrative_ontology:measurement(arian_reading_be_t318, homoousios_christology__arian_reading, base_extractiveness, 318, 0.22).
narrative_ontology:measurement_basis(arian_reading_be_t318, observed).
narrative_ontology:measurement(arian_reading_be_t325, homoousios_christology__arian_reading, base_extractiveness, 325, 0.3).
narrative_ontology:measurement_basis(arian_reading_be_t325, observed).
narrative_ontology:measurement(arian_reading_be_t341, homoousios_christology__arian_reading, base_extractiveness, 341, 0.33).
narrative_ontology:measurement_basis(arian_reading_be_t341, observed).
narrative_ontology:measurement(arian_reading_be_t359, homoousios_christology__arian_reading, base_extractiveness, 359, 0.38).
narrative_ontology:measurement_basis(arian_reading_be_t359, observed).
narrative_ontology:measurement(arian_reading_be_t361, homoousios_christology__arian_reading, base_extractiveness, 361, 0.3).
narrative_ontology:measurement_basis(arian_reading_be_t361, observed).
narrative_ontology:measurement(arian_reading_be_t381, homoousios_christology__arian_reading, base_extractiveness, 381, 0.35).
narrative_ontology:measurement_basis(arian_reading_be_t381, observed).
narrative_ontology:measurement(arian_reading_be_t410, homoousios_christology__arian_reading, base_extractiveness, 410, 0.34).
narrative_ontology:measurement_basis(arian_reading_be_t410, observed).
narrative_ontology:measurement(arian_reading_be_t589, homoousios_christology__arian_reading, base_extractiveness, 589, 0.31).
narrative_ontology:measurement_basis(arian_reading_be_t589, observed).

% Suppression requirement over time
narrative_ontology:measurement(arian_reading_su_t318, homoousios_christology__arian_reading, suppression_requirement, 318, 0.15).
narrative_ontology:measurement_basis(arian_reading_su_t318, observed).
narrative_ontology:measurement(arian_reading_su_t325, homoousios_christology__arian_reading, suppression_requirement, 325, 0.45).
narrative_ontology:measurement_basis(arian_reading_su_t325, observed).
narrative_ontology:measurement(arian_reading_su_t341, homoousios_christology__arian_reading, suppression_requirement, 341, 0.58).
narrative_ontology:measurement_basis(arian_reading_su_t341, observed).
narrative_ontology:measurement(arian_reading_su_t359, homoousios_christology__arian_reading, suppression_requirement, 359, 0.78).
narrative_ontology:measurement_basis(arian_reading_su_t359, observed).
narrative_ontology:measurement(arian_reading_su_t361, homoousios_christology__arian_reading, suppression_requirement, 361, 0.35).
narrative_ontology:measurement_basis(arian_reading_su_t361, observed).
narrative_ontology:measurement(arian_reading_su_t381, homoousios_christology__arian_reading, suppression_requirement, 381, 0.62).
narrative_ontology:measurement_basis(arian_reading_su_t381, observed).
narrative_ontology:measurement(arian_reading_su_t410, homoousios_christology__arian_reading, suppression_requirement, 410, 0.58).
narrative_ontology:measurement_basis(arian_reading_su_t410, observed).
narrative_ontology:measurement(arian_reading_su_t589, homoousios_christology__arian_reading, suppression_requirement, 589, 0.2).
narrative_ontology:measurement_basis(arian_reading_su_t589, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(homoousios_christology__arian_reading, identity_coordination).
narrative_ontology:affects_constraint(homoousios_christology__arian_reading, pro_nicene_reading).
narrative_ontology:affects_constraint(homoousios_christology__arian_reading, semi_arian_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'the Arian controversy' (and the kernel label 'homoousios christology') covers three structurally distinct arrangements, decomposed per the epsilon-invariance principle into three linked stories. This file is the arian_reading: its epsilon is reading-indexed (modest, 0.31 — the arrangement assessed by its own lights), its victims are Nicene dissenters under Arian jurisdiction and Catholics in Gothic territory, and its enforcement history runs from the Eusebian counter-councils through the Ariminum compulsion to Gothic royal enforcement. The pro_nicene_reading inverts the victim set (Arian clergy under imperial-Nicene enforcement) and carries a different epsilon; the semi_arian_reading carries the compromise's own coercion record. Upstream-downstream structure: the Arian arrangement's pressure forced the pro-Nicene side to sharpen its metaphysics (the Cappadocian one-ousia-three-hypostases settlement), and its existence shaped the homoiousian middle position; the pro-Nicene establishment's eventual victory (381) is the repudiation pressure recorded in this reading's drift_state. All three files link one another via network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
