% ============================================================================
% CONSTRAINT STORY: tordesillas_demarcation_kernel__spanish_conquest_legitimation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_tordesillas_demarcation_kernel__spanish_conquest_legitimation, []).

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
 *   constraint_id: tordesillas_demarcation_kernel__spanish_conquest_legitimation
 *   human_readable: Papal Demarcation as License for Spanish Conquest and Indigenous Subjugation (West of Line)
 *   domain: international_law/colonial_history/sovereignty
 *
 * SUMMARY:
 *   The Treaty of Tordesillas (1494) and preceding papal bulls (Inter Caetera
 *   1493, Dudum Siquidem 1493) constitute a reading of the demarcation kernel
 *   in which the papal grant operates as explicit license for Spanish
 *   territorial conquest and indigenous subjugation west of a meridian line
 *   roughly 370 leagues west of the Cape Verde Islands. This reading frames
 *   the constraint as a snare: the papal authority provides theological cover
 *   for military conquest, labor extraction via encomienda, forced
 *   conversion, and the systematic dismantling of indigenous governance and
 *   autonomy. The victims are the indigenous populations west of the line,
 *   whose territorial, labor, and spiritual autonomy are extracted under the
 *   claim of divinely-mandated stewardship. The beneficiaries are the Spanish
 *   crown, colonial administration, settler class, and the institutional
 *   Catholic Church. The constraint persists because its dismantling would
 *   require the Spanish crown to abandon territorial claims, colonial
 *   settlers to forgo labor extraction and status, and the church to renounce
 *   spiritual authority — costs each party deems prohibitive. The measurement
 *   series track the increasing intensity of extraction and suppression as
 *   indigenous resistance is militarily crushed and labor-extraction
 *   mechanisms (encomienda, slavery) become institutionalized. Theater ratio
 *   rises as the frequency of performative Christian instruction (missions,
 *   conversion ceremonies) grows relative to the underlying extraction
 *   machinery.
 *
 * KEY AGENTS:
 *   - Spanish crown: institutional agenda-setter; claims ultimate authority west of line; collects tribute and labor
 *   - Spanish colonial administration: institutional agenda-setter; enforces extraction, conversion, and suppression of resistance
 *   - Encomienda grantee class: powerful beneficiaries; extract indigenous labor and agricultural surplus; justified as Christian trusteeship
 *   - Indigenous populations west of line: powerless victims; subject to territorial dispossession, labor conscription, forced conversion, and military suppression of resistance
 *   - Catholic Church in Americas: institutional beneficiary; directs conversion; collects tithes; legitimates the constraint theologically
 *   - Indigenous leadership: moderate-power victims with identity-locked exit; nominal incorporation into colonial structure while authority erodes
 *   - Roman Papacy: institutional agenda-setter; issues legitimating bulls; arbiter of Christian geopolitics
 *   - Portuguese crown: excluded competitor; trapped by the line's provisions; contestation over exact boundary location
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(tordesillas_demarcation_kernel__spanish_conquest_legitimation, 0.89).
domain_priors:suppression_score(tordesillas_demarcation_kernel__spanish_conquest_legitimation, 0.92).
domain_priors:theater_ratio(tordesillas_demarcation_kernel__spanish_conquest_legitimation, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(tordesillas_demarcation_kernel__spanish_conquest_legitimation, extractiveness, 0.89).
narrative_ontology:constraint_metric(tordesillas_demarcation_kernel__spanish_conquest_legitimation, suppression_requirement, 0.92).
narrative_ontology:constraint_metric(tordesillas_demarcation_kernel__spanish_conquest_legitimation, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(tordesillas_demarcation_kernel__spanish_conquest_legitimation, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(tordesillas_demarcation_kernel__spanish_conquest_legitimation, resistance, 0.73).

% --- Constraint claim ---
narrative_ontology:constraint_claim(tordesillas_demarcation_kernel__spanish_conquest_legitimation, snare).
narrative_ontology:human_readable(tordesillas_demarcation_kernel__spanish_conquest_legitimation, "Papal Demarcation as License for Spanish Conquest and Indigenous Subjugation (West of Line)").
narrative_ontology:topic_domain(tordesillas_demarcation_kernel__spanish_conquest_legitimation, "international_law/colonial_history/sovereignty").

domain_priors:requires_active_enforcement(tordesillas_demarcation_kernel__spanish_conquest_legitimation).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(tordesillas_demarcation_kernel__spanish_conquest_legitimation, 'e84fd695-f097-4d65-906b-872b1ff8baae').
narrative_ontology:cs_kernel_codification('e84fd695-f097-4d65-906b-872b1ff8baae', fixed_text).
narrative_ontology:cs_authority_grounding('e84fd695-f097-4d65-906b-872b1ff8baae', extraction).
narrative_ontology:cs_interpretation_layer_present('e84fd695-f097-4d65-906b-872b1ff8baae').
narrative_ontology:cs_reading_relation('e84fd695-f097-4d65-906b-872b1ff8baae', tordesillas_demarcation_kernel__portuguese_exploration_legitimation, coexists_with).
narrative_ontology:cs_axiom('e84fd695-f097-4d65-906b-872b1ff8baae', foundational, papal_authority_grants_conquest_license).
narrative_ontology:cs_axiom_status(papal_authority_grants_conquest_license, holdable).
narrative_ontology:cs_axiom_grounding('e84fd695-f097-4d65-906b-872b1ff8baae', papal_authority_grants_conquest_license, theological).
narrative_ontology:cs_axiom('e84fd695-f097-4d65-906b-872b1ff8baae', foundational, non_christian_populations_legitimate_targets_of_forced_conversion).
narrative_ontology:cs_axiom_status(non_christian_populations_legitimate_targets_of_forced_conversion, holdable).
narrative_ontology:cs_axiom_grounding('e84fd695-f097-4d65-906b-872b1ff8baae', non_christian_populations_legitimate_targets_of_forced_conversion, theological).
narrative_ontology:cs_axiom('e84fd695-f097-4d65-906b-872b1ff8baae', secondary, encomienda_labor_extraction_is_reciprocal_stewardship).
narrative_ontology:cs_axiom_status(encomienda_labor_extraction_is_reciprocal_stewardship, overridden).
narrative_ontology:cs_axiom_grounding('e84fd695-f097-4d65-906b-872b1ff8baae', encomienda_labor_extraction_is_reciprocal_stewardship, conventional).
narrative_ontology:cs_reference_frame('e84fd695-f097-4d65-906b-872b1ff8baae', papal_temporal_authority_over_non_christian_territories).
narrative_ontology:cs_drift_state('e84fd695-f097-4d65-906b-872b1ff8baae', reformation_challenge_to_papal_authority, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('e84fd695-f097-4d65-906b-872b1ff8baae', '').
narrative_ontology:cs_kernel_id(tordesillas_demarcation_kernel__spanish_conquest_legitimation, tordesillas_demarcation_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(tordesillas_demarcation_kernel__spanish_conquest_legitimation, spanish_crown).
narrative_ontology:constraint_beneficiary(tordesillas_demarcation_kernel__spanish_conquest_legitimation, spanish_colonial_administration).
narrative_ontology:constraint_beneficiary(tordesillas_demarcation_kernel__spanish_conquest_legitimation, encomienda_grantee_class).
narrative_ontology:constraint_victim(tordesillas_demarcation_kernel__spanish_conquest_legitimation, indigenous_populations_west_of_line).
narrative_ontology:constraint_victim(tordesillas_demarcation_kernel__spanish_conquest_legitimation, african_enslaved_people_in_spanish_americas).
narrative_ontology:constraint_victim(tordesillas_demarcation_kernel__spanish_conquest_legitimation, excluded_european_competitors).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(tordesillas_demarcation_kernel__spanish_conquest_legitimation, catholic_church_in_americas).
narrative_ontology:constraint_beneficiary(tordesillas_demarcation_kernel__spanish_conquest_legitimation, atlantic_slave_trade_enterprise).
narrative_ontology:constraint_victim(tordesillas_demarcation_kernel__spanish_conquest_legitimation, indigenous_leadership_structures).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The Spanish monarchy invokes the papal grant as the supreme legitimizing instrument for territorial conquest and administrative control west of the demarcation line. The crown collects tribute, labor services (encomienda), and land grants from conquered indigenous populations. The papal framing allows the crown to present extraction as divinely mandated civilizational stewardship rather than naked conquest. Exit from this arrangement would mean abandoning all territorial and extractive claims in the Americas.
narrative_ontology:constraint_stakeholder(tordesillas_demarcation_kernel__spanish_conquest_legitimation, spanish_crown, agenda_setter,
    institutional, generational, arbitrage, global).

% Administers the territory west of the line on behalf of the crown. Issues encomienda grants, enforces conversion, collects tribute, and suppresses resistance. The papally-sanctioned framing legitimates their authority to restructure indigenous societies according to Christian and Spanish organizational norms. Administrative offices, land allocation, and labor extraction systems all flow through the institutional machinery the papal grant empowers.
narrative_ontology:constraint_stakeholder(tordesillas_demarcation_kernel__spanish_conquest_legitimation, spanish_colonial_administration, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(tordesillas_demarcation_kernel__spanish_conquest_legitimation, spanish_colonial_administration, beneficiary).

% Spanish settlers and colonial elites receive encomienda grants—temporary stewardship of indigenous labor and tribute rights—justified as Christian trusteeship mandated by papal authority. The grantees extract labor, agricultural surplus, and minerals while claiming to provide Christian instruction. The papal framing insulates grantees from accusations of slavery, reframing extraction as educational and spiritual obligation. Their exit options are limited; abandoning encomienda means forfeiting the economic foundation of colonial settlement and status.
narrative_ontology:constraint_stakeholder(tordesillas_demarcation_kernel__spanish_conquest_legitimation, encomienda_grantee_class, beneficiary,
    powerful, generational, constrained, continental).

% Subject to conquest, territorial dispossession, labor conscription via encomienda, forced conversion to Christianity, and suppression of indigenous religious and governance practices. The papal grant provides the Spanish claim with theological authority that indigenous leaders are told is unquestionable. Resistance is met with military force and characterized as rebellion against divinely-sanctioned authority. Exit means death, enslavement, or displacement; staying means ongoing extraction of labor, crops, and spiritual autonomy.
narrative_ontology:constraint_stakeholder(tordesillas_demarcation_kernel__spanish_conquest_legitimation, indigenous_populations_west_of_line, payer,
    powerless, biographical, trapped, continental).

% Directs forced conversion of indigenous populations, collects tithes and tribute, and exercises spiritual authority that reinforces secular extraction. Church hierarchy legitimates the Spanish conquest frame internally and to indigenous populations. The church's institutional expansion and resource accumulation depend on the continuation of the papal-grant framing. Individual friars and bishops often advocate for indigenous welfare, but the institutional church apparatus depends on the constraint persisting.
narrative_ontology:constraint_stakeholder(tordesillas_demarcation_kernel__spanish_conquest_legitimation, catholic_church_in_americas, beneficiary,
    institutional, generational, constrained, continental).
narrative_ontology:stakeholder_secondary_role(tordesillas_demarcation_kernel__spanish_conquest_legitimation, catholic_church_in_americas, agenda_setter).

% Is structurally excluded from the western hemisphere west of the line by the same papal authority that legitimates Spanish conquest. Portugal contests the line's exact location and claims portions of what Spain asserts as its western territory. Portuguese interests in expansion eastward and southward are constituted by exclusion from the west.
narrative_ontology:constraint_stakeholder(tordesillas_demarcation_kernel__spanish_conquest_legitimation, portuguese_crown, excluded,
    institutional, generational, trapped, global).

% France, England, and the Dutch republics are excluded from American territories by the papal partition, which the Spanish and Portuguese invoke against European competitors. The demarcation is enforced through military capacity and Spanish/Portuguese claims to papal authority. Non-Iberian European powers must either accept exclusion or challenge the papal authority itself—a challenge that destabilizes the entire Christendom-based international order.
narrative_ontology:constraint_stakeholder(tordesillas_demarcation_kernel__spanish_conquest_legitimation, other_european_monarchies, excluded,
    powerful, generational, trapped, global).

% Issues the papal bulls (Romanus Pontifex 1455, Inter Caetera 1493, Dudum Siquidem 1493) that establish the demarcation line and delegate to Spanish and Portuguese crowns the authority to conquer, convert, and administer territories. The papacy frames this as extending Christian dominion and spiritual salvation. The papacy collects diplomatic deference and spiritual authority affirmation from all parties; their ability to arbitrate European territorial disputes rests on maintaining the system's credibility.
narrative_ontology:constraint_stakeholder(tordesillas_demarcation_kernel__spanish_conquest_legitimation, roman_papacy, agenda_setter,
    institutional, generational, constrained, global).

% Indigenous nobility and caciques are initially incorporated into the Spanish colonial structure with nominal authority and tribute obligations. Many are told they rule by papal sanction through the crown. Over time, their authority is subordinated to Spanish officials and encomienda grantees. Exit means loss of whatever residual power they retain; staying means mediating Spanish demands to increasingly restive subject populations. Their identity as legitimate rulers is constituted through the very system that dismantles their autonomy.
narrative_ontology:constraint_stakeholder(tordesillas_demarcation_kernel__spanish_conquest_legitimation, indigenous_leadership_structures, payer,
    moderate, biographical, identity_locked, regional).

% As indigenous labor supply collapses under the extraction and disease, Spanish colonial administration increasingly imports enslaved African populations. The papal frame initially restricts slavery to non-Christians, but African enslavement becomes the substitute extraction mechanism. The slave trade enterprise benefits from the labor-shortage crisis the papal-framed conquest created, and the same theological apparatus (non-Christian populations as legitimate targets) extends to African slavery.
narrative_ontology:constraint_stakeholder(tordesillas_demarcation_kernel__spanish_conquest_legitimation, atlantic_slave_trade_enterprise, beneficiary,
    organized, generational, constrained, global).

% The papal demarcation doctrine becomes the first major articulation of international law governing territorial partition and extra-European expansion. Scholars invoke the precedent to justify colonial projects, to critique them, and to construct theories of sovereignty and discovery rights. The constraint shapes the intellectual framework through which later colonialism is justified.
narrative_ontology:constraint_stakeholder(tordesillas_demarcation_kernel__spanish_conquest_legitimation, international_law_scholarship_tradition, observer,
    analytical, civilizational, analytical, global).
narrative_ontology:stakeholder_non_agent(tordesillas_demarcation_kernel__spanish_conquest_legitimation, international_law_scholarship_tradition).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(tordesillas_demarcation_kernel__spanish_conquest_legitimation, spanish_crown).
narrative_ontology:fixing_cost_class(tordesillas_demarcation_kernel__spanish_conquest_legitimation, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a mechanism for partitioning unexplored/unconquered territories among Christian European powers without inter-European warfare, and provides a theological framework for legitimating conquest and forced religious conversion as a unified Christian project rather than mere territorial brigandage.
% TRANSFER_FUNCTION: Moves indigenous labor (via encomienda), agricultural surplus, tribute, mineral wealth, territorial control, and spiritual autonomy from indigenous populations to the Spanish crown, colonial administration, settler class, and Catholic Church. The transfer is justified as Christian stewardship and spiritual instruction, reframing extraction as civilizational obligation.
% ABSENT_VOICES: Indigenous populations themselves—who experience the constraint as imposed conquest—are told the arrangement is divinely mandated and unquestionable. Non-Iberian European powers excluded by the line are structurally barred from even negotiating the terms. African enslaved people, whose forced migration becomes the substitute extraction mechanism after indigenous labor collapse, have no voice in the system whatsoever.
% DISAPPEARANCE_RATIONALE: If the papal grant and the Spanish conquest framing dissolved overnight, Spanish territorial claims in the Americas would lack their primary legitimating authority. Indigenous governance structures, if they had not been destroyed, would re-emerge. Labor and tribute extraction would become indistinguishable from outright slavery and theft rather than framed as divinely-mandated stewardship. The entire colonial administrative apparatus would lose its theological anchoring and would have to operate as raw conquest, which would provoke much stronger resistance and international contestation. The Spanish Americas would reorganize around indigenous authority or contested European claims, not Spanish monopoly.
% FOUNDING_PROBLEM: The papacy and European Christendom faced the problem of partitioning newly-discovered and unconquered territories among Christian monarchs without triggering wars of European conquest. The constraint presents itself as solving the problem of justifying Christian expansion against non-Christian populations while maintaining Christian unity against inter-Christian conflict.
% FOUNDING_PROBLEM_CORROBORATION: The Spanish crown and papacy attest the founding problem is the orderly partition of Christian missionary opportunity and territorial governance. Indigenous leaders (under coercion) and Spanish colonial authorities attest it is solved. However, independent observers including Portuguese competitors, later colonial powers, indigenous resistance leaders (in internal testimony), and modern historians attest that the founding problem was less 'partitioning opportunity fairly' and more 'justifying naked conquest to other Europeans and to the conquered populations.' The constraint solves territorial partition among Christian monarchs; it does NOT solve the legitimacy problem of conquest — it masks it theologically.
narrative_ontology:disappearance_verdict(tordesillas_demarcation_kernel__spanish_conquest_legitimation, world_rearranges).
narrative_ontology:founding_problem_status(tordesillas_demarcation_kernel__spanish_conquest_legitimation, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(tordesillas_demarcation_kernel__spanish_conquest_legitimation, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(tordesillas_demarcation_kernel__spanish_conquest_legitimation, 'none', 1).
narrative_ontology:epsilon_provenance(tordesillas_demarcation_kernel__spanish_conquest_legitimation, 0.89, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(tordesillas_demarcation_kernel__spanish_conquest_legitimation_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(tordesillas_demarcation_kernel__spanish_conquest_legitimation, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(tordesillas_demarcation_kernel__spanish_conquest_legitimation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is very high (0.89 at interval end) because the constraint transfers indigenous labor, tribute, territorial control, and spiritual autonomy to Spanish/colonial beneficiaries with no reciprocal benefit to victims. The transfer is not negotiated or compensated—it is imposed by military force and theological authority. Suppression is even higher (0.92) because the constraint's persistence depends entirely on active military enforcement and theological indoctrination; without continuous suppression of indigenous resistance and alternative claims, the constraint collapses. The measurement series show suppression intensifying as indigenous resistance mounts (1520s-1560s major uprisings) and is militarily crushed, then stabilizing at near-maximum as indigenous capacity for organized resistance is exhausted and the system becomes routinized. Theater ratio rises early (conversion ceremonies, mission establishments) but does not dominate because the constraint's real function—labor extraction and territorial control—is continuous and visible to victims; theater complements rather than substitutes for enforcement. Accessibility collapse tracks suppression closely: indigenous alternatives (autonomous governance, rejection of conversion, labor refusal) narrow dramatically as Spanish military capacity is established and indigenous populations are decimated by disease, war, and overwork. Resistance peaks early (indigenous coalitions, military organized resistance) then declines as organizational capacity and demographic survival are compromised.
 *
 * PERSPECTIVAL GAP:
 *   From the Spanish crown and papal seat: the constraint is a legitimate framework for extending Christian dominion, civilizing non-Christian populations, and maintaining European peace by orderly partition. The extraction is justified as the cost of Christian instruction and security provision. From the indigenous victim seats: the constraint is an instrument of genocide, enslavement, and the theft of territory and labor under a theological cover story. No indigenous leader ever consented to this arrangement; it was imposed by military conquest. From the excluded European seats (Portugal, France, England): the constraint is an attempt to monopolize American territories by papal authority, a claim they ultimately reject. From the analytical seat: the constraint is a snare whose persistence depends entirely on the Spanish military monopoly in the region and whose collapse would require either Spanish defeat or indigenous population recovery sufficient to mount effective organized resistance.
 *
 * DIRECTIONALITY LOGIC:
 *   Spanish crown and colonial administration are full beneficiaries (d near 0.0): they collect tribute, labor, territorial control, and status without proportional cost; their exit is arbitrage (they can choose to leave and abandon claims, but the opportunity cost of forgoing the extraction is prohibitive). Encomienda grantees are beneficiaries (d around 0.15): they extract labor and agricultural surplus; their exit is constrained (abandoning encomienda means forfeiting colonial wealth and status). Indigenous populations are full targets (d near 1.0): they bear extraction of labor, tribute, territory, and spiritual autonomy with no reciprocal benefit; their exit is trapped (death, enslavement, or displacement are the alternatives to subjugation). Indigenous leadership is a partially-targeted beneficiary (d around 0.65): they nominally retain authority and social status within the colonial structure, but their autonomy erodes over time and they remain subject to Spanish override; their exit is identity-locked (losing the leadership role means losing their constituted identity as legitimate authority). Excluded competitors (Portugal, other European powers) are structural targets (d near 1.0) of the demarcation: they are barred from American territories but cannot exit from the attempt to overturn the constraint because doing so means renouncing any claim to Western Hemisphere expansion. The Catholic Church sits near symmetric (d around 0.45): it benefits from conversion/tithes/expansion, but it also bears the burden of administering a system that generates suffering and requires ongoing justification.
 *
 * MANDATROPHY ANALYSIS:
 *   Mandatrophy resolution is necessary here because the founding problem ('orderly partition of Christian missionary opportunity') is actually solved by the constraint, but the constraint's real function ('legitimizing naked conquest and labor extraction') persists indefinitely. The founding problem status is 'dead' by 1550—the territorial partition is established, European rivals have accepted or ceased challenging it, and there is no ongoing inter-European dispute about American boundaries that the constraint solves. Yet the constraint persists at near-maximum extractiveness (0.89) through 1650. This is a classic zombie constraint: the justifying rationale has evaporated, but the extraction machinery continues because the beneficiaries have built their wealth, power, and institutional identity around the extraction. Declaring mandatrophy_resolved would require the Spanish crown to acknowledge that the founding problem is dead and the constraint now operates as pure extraction—a declaration that would undermine the theological framing and invite challenges. Instead, the crown maintains the fiction that conversion and Christian stewardship are still the binding purpose, even as the actual operation is labor extraction and territorial monopoly. The constraint is mandatrophic by the measure 'founding problem dead + constraint persisting at unchanged intensity'; no beneficiary is incentivized to resolve it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    papal_authority_legitimacy,
    'Does the papacy possess temporal authority to grant non-Christian territories to Christian monarchs, or is the papal grant a theological cover story for pure territorial conquest?',
    'Historical and legal examination: if the papacy derives temporal authority only from Christian consensus or secular power acceptance (not from inherent authority), the grant becomes a negotiated allocation device rather than a license to conquer. This distinction dissolves under scrutiny from any non-Catholic Christian power (Protestant, Orthodox) or from indigenous ontologies that reject the premise entirely.',
    'If the authority is questioned, the constraint loses its primary legitimacy mechanism and becomes naked conquest requiring continuous military enforcement. If sustained, the constraint retains theological cover that moderates resistance and justifies suppression to European audiences.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(papal_authority_legitimacy, conceptual, 'Whether the papal grant grounds the constraint legitimately or merely covers it.').

omega_variable(
    encomienda_as_stewardship_vs_slavery,
    'Is encomienda a genuine system of Christian trusteeship with reciprocal obligation to provide instruction and protection, or is it de facto slavery disguised in trusteeship language?',
    'Empirical examination of actual encomienda practice: labor hours extracted, mortality rates, enforcement mechanisms, indigenous benefit provision, and grantee compliance with supposed instructional obligations. Contemporary testimony from Dominican friars (Las Casas, Montesinos) and indigenous witnesses documents the de facto slave-labor conditions and grantee non-compliance with instructional mandates.',
    'If encomienda is genuine stewardship, the extraction is partially justified as reciprocal obligation; suppression and theater rise to accommodate the pretense. If encomienda is de facto slavery, the constraint is pure extraction with no coordination component, and the measured suppression (0.92) understates the actual coercive force required to sustain the fiction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(encomienda_as_stewardship_vs_slavery, empirical, 'Whether encomienda labor extraction carries reciprocal obligation or is a slavery system in stewardship clothing.').

omega_variable(
    indigenous_consent_counterfactual,
    'Would indigenous populations, if given uncoerced choice with full information about the constraint, accept the arrangement or reject it?',
    'Counterfactual: examine indigenous resistance patterns (when Spain holds military monopoly vs. when indigenous forces are mobilized) and indigenous testimony collected outside coercive contexts (captured indigenous elders interrogated by independent friars, indigenous testimony in internal documents not intended for Spanish eyes). The resistance pattern is near-universal rejection when Spain lacks total military dominance; acceptance occurs only under duress.',
    'If universal rejection outside coercion: the constraint is pure snare by the consent standard (no party accepts it freely). If any significant voluntary acceptance: the constraint might shade toward tangled rope (coordination function for some, extraction for others). The evidence points strongly toward universal rejection outside duress.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(indigenous_consent_counterfactual, empirical, 'Whether indigenous populations would accept the constraint if not under military coercion.').

omega_variable(
    spanish_settler_identity_lock,
    'Are Spanish settlers in the Americas structurally dependent on the encomienda/conquest framing, or could their settlement and enrichment continue under alternative arrangements (indigenous autonomy with trade, negotiated labor contracts, competitive European settlement)?',
    'Counterfactual analysis: if the papally-framed conquest ended, would settlers abandon the Americas entirely, seek negotiated labor arrangements, or attempt to establish competing Spanish settlements? The behavior of non-Spanish European settlers (French, English, Dutch) who lack the papal grant and must negotiate shows the viability of non-conquest settlement alternatives. Spanish resistance to these alternatives is best explained by sunk identity investment in the conquest narrative and the monopoly-wealth encomienda provides.',
    'If settler identity is locked into conquest framing: the constraint persists in part through beneficiary identity-lock, not just military coercion. If settlers could adapt to alternative arrangements: the constraint''s persistence is purely coercive, and its collapse would require only military defeat, not identity-belief shift.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(spanish_settler_identity_lock, conceptual, 'Whether Spanish settler identity is locked into the conquest narrative or contingent to it.').

omega_variable(
    demographic_collapse_as_extraction_mechanism,
    'Is the catastrophic indigenous population collapse (1500-1650) a consequence of unintended disease and exploitation, or is it functionally equivalent to an extraction mechanism (removing obstacles to Spanish settlement and labor monopoly)?',
    'Empirical: epidemiological analysis (disease transmission patterns), coupled with Spanish policy analysis (treatment of sick indigenous people, prevention of population recovery). The evidence shows disease is primarily unintended, but Spanish policy actively blocks population recovery through overwork, malnutrition, and selective breeding policies. The result is functionally a demographic extermination dressed in the language of Christian stewardship.',
    'If collapse is unintended side effect: the constraint''s extraction measures (encomienda, tribute) interact catastrophically with disease but are not designed for genocide. If Spanish policy actively suppresses recovery: the constraint includes implicit demicide as an extraction mechanism. The historical record indicates active policy suppression of recovery by late 1500s.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(demographic_collapse_as_extraction_mechanism, empirical, 'Whether demographic collapse is unintended consequence or functional extraction mechanism.').

omega_variable(
    reading_foreclosure_via_protestant_reform,
    'Does the Protestant Reformation''s theological challenge to papal temporal authority structurally foreclose this reading (Spanish conquest legitimation via papal grant)?',
    'Historical: does the Protestant assertion that the papacy lacks temporal authority in secular matters undermine or eliminate Spanish reliance on the papal grant? Spanish response: Spain doubles down on papal-grant framing and aligns increasingly with the Counter-Reformation, defending the reading against the foreclosure attempt. The reading persists not because the foreclosure fails logically, but because Spain has sufficient military power to enforce the claim regardless of theological legitimacy.',
    'If the foreclosure attempt succeeds: the constraint loses its primary legitimating narrative and becomes naked conquest, accelerating resistance and international challenge. If the foreclosure fails: the constraint persists in parallel authority systems (papal + Spanish crown assertion of independent discovery rights) that eventually decouple.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_foreclosure_via_protestant_reform, conceptual, 'Whether Protestant theological challenge to papal authority forecloses the conquest-legitimation reading.').

omega_variable(
    suppression_structural_vs_internalized,
    'Is the measured suppression (0.92) primarily structural (military force, territorial control preventing exit) or internalized (indigenous populations have internalized inferiority narratives and Christian framing such that suppression persists even without enforcement)?',
    'Longitudinal and comparative: examine indigenous resistance patterns in early contact (high, before Spanish military dominance) vs. late colonial (lower, after generations in the system). Examine indigenous resistance in regions where Spanish military control briefly lapses (does resistance surge?). Examine indigenous rebellions framed in Christian terms vs. pre-Christian restoration terms (do internalized Christian frameworks constrain indigenous imagination of alternatives?). The evidence shows high early resistance (structural suppression not yet internalized), declining late resistance (combination of structural exhaustion and internalized inferiority), and rapid resistance surge when Spanish control weakens (indicating suppression remains substantially structural, not internalized).',
    'If primarily structural: the constraint''s suppression would decay rapidly if Spanish military capacity failed, and would require continuous enforcement. If substantially internalized: the constraint''s suppression would persist even after Spanish military withdrawal, and indigenous populations would resist their own liberation. The evidence suggests early-to-mid transition from structural to partially-internalized by 1650, with structural suppression still dominant.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structural_vs_internalized, empirical, 'Whether suppression is primarily enforced by military/structural means or internalized through ideology.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(tordesillas_demarcation_kernel__spanish_conquest_legitimation, 1493, 1650).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tordesillas_spanish_conquest_tr_t1493, tordesillas_demarcation_kernel__spanish_conquest_legitimation, theater_ratio, 1493, 0.22).
narrative_ontology:measurement(tordesillas_spanish_conquest_tr_t1520, tordesillas_demarcation_kernel__spanish_conquest_legitimation, theater_ratio, 1520, 0.31).
narrative_ontology:measurement(tordesillas_spanish_conquest_tr_t1560, tordesillas_demarcation_kernel__spanish_conquest_legitimation, theater_ratio, 1560, 0.38).
narrative_ontology:measurement(tordesillas_spanish_conquest_tr_t1600, tordesillas_demarcation_kernel__spanish_conquest_legitimation, theater_ratio, 1600, 0.42).
narrative_ontology:measurement(tordesillas_spanish_conquest_tr_t1625, tordesillas_demarcation_kernel__spanish_conquest_legitimation, theater_ratio, 1625, 0.43).
narrative_ontology:measurement(tordesillas_spanish_conquest_tr_t1650, tordesillas_demarcation_kernel__spanish_conquest_legitimation, theater_ratio, 1650, 0.41).

% Extraction over time
narrative_ontology:measurement(tordesillas_spanish_conquest_be_t1493, tordesillas_demarcation_kernel__spanish_conquest_legitimation, base_extractiveness, 1493, 0.72).
narrative_ontology:measurement(tordesillas_spanish_conquest_be_t1520, tordesillas_demarcation_kernel__spanish_conquest_legitimation, base_extractiveness, 1520, 0.81).
narrative_ontology:measurement(tordesillas_spanish_conquest_be_t1560, tordesillas_demarcation_kernel__spanish_conquest_legitimation, base_extractiveness, 1560, 0.88).
narrative_ontology:measurement(tordesillas_spanish_conquest_be_t1600, tordesillas_demarcation_kernel__spanish_conquest_legitimation, base_extractiveness, 1600, 0.89).
narrative_ontology:measurement(tordesillas_spanish_conquest_be_t1625, tordesillas_demarcation_kernel__spanish_conquest_legitimation, base_extractiveness, 1625, 0.87).
narrative_ontology:measurement(tordesillas_spanish_conquest_be_t1650, tordesillas_demarcation_kernel__spanish_conquest_legitimation, base_extractiveness, 1650, 0.89).

% Suppression requirement over time
narrative_ontology:measurement(tordesillas_spanish_conquest_su_t1493, tordesillas_demarcation_kernel__spanish_conquest_legitimation, suppression_requirement, 1493, 0.68).
narrative_ontology:measurement(tordesillas_spanish_conquest_su_t1520, tordesillas_demarcation_kernel__spanish_conquest_legitimation, suppression_requirement, 1520, 0.85).
narrative_ontology:measurement(tordesillas_spanish_conquest_su_t1560, tordesillas_demarcation_kernel__spanish_conquest_legitimation, suppression_requirement, 1560, 0.91).
narrative_ontology:measurement(tordesillas_spanish_conquest_su_t1600, tordesillas_demarcation_kernel__spanish_conquest_legitimation, suppression_requirement, 1600, 0.93).
narrative_ontology:measurement(tordesillas_spanish_conquest_su_t1625, tordesillas_demarcation_kernel__spanish_conquest_legitimation, suppression_requirement, 1625, 0.92).
narrative_ontology:measurement(tordesillas_spanish_conquest_su_t1650, tordesillas_demarcation_kernel__spanish_conquest_legitimation, suppression_requirement, 1650, 0.92).

% Leveled coercion grid (OQ-93): 32/32 authored points at t0=1493, tn=1650
narrative_ontology:measurement(tordesillas_spanish_conquest_grid_01, tordesillas_demarcation_kernel__spanish_conquest_legitimation, accessibility_collapse(class), 1493, 0.58).
narrative_ontology:measurement(tordesillas_spanish_conquest_grid_02, tordesillas_demarcation_kernel__spanish_conquest_legitimation, accessibility_collapse(class), 1650, 0.81).
narrative_ontology:measurement(tordesillas_spanish_conquest_grid_03, tordesillas_demarcation_kernel__spanish_conquest_legitimation, accessibility_collapse(individual), 1493, 0.64).
narrative_ontology:measurement(tordesillas_spanish_conquest_grid_04, tordesillas_demarcation_kernel__spanish_conquest_legitimation, accessibility_collapse(individual), 1650, 0.92).
narrative_ontology:measurement(tordesillas_spanish_conquest_grid_05, tordesillas_demarcation_kernel__spanish_conquest_legitimation, accessibility_collapse(organizational), 1493, 0.52).
narrative_ontology:measurement(tordesillas_spanish_conquest_grid_06, tordesillas_demarcation_kernel__spanish_conquest_legitimation, accessibility_collapse(organizational), 1650, 0.88).
narrative_ontology:measurement(tordesillas_spanish_conquest_grid_07, tordesillas_demarcation_kernel__spanish_conquest_legitimation, accessibility_collapse(structural), 1493, 0.71).
narrative_ontology:measurement(tordesillas_spanish_conquest_grid_08, tordesillas_demarcation_kernel__spanish_conquest_legitimation, accessibility_collapse(structural), 1650, 0.78).
narrative_ontology:measurement(tordesillas_spanish_conquest_grid_09, tordesillas_demarcation_kernel__spanish_conquest_legitimation, resistance(class), 1493, 0.73).
narrative_ontology:measurement(tordesillas_spanish_conquest_grid_10, tordesillas_demarcation_kernel__spanish_conquest_legitimation, resistance(class), 1650, 0.61).
narrative_ontology:measurement(tordesillas_spanish_conquest_grid_11, tordesillas_demarcation_kernel__spanish_conquest_legitimation, resistance(individual), 1493, 0.82).
narrative_ontology:measurement(tordesillas_spanish_conquest_grid_12, tordesillas_demarcation_kernel__spanish_conquest_legitimation, resistance(individual), 1650, 0.68).
narrative_ontology:measurement(tordesillas_spanish_conquest_grid_13, tordesillas_demarcation_kernel__spanish_conquest_legitimation, resistance(organizational), 1493, 0.71).
narrative_ontology:measurement(tordesillas_spanish_conquest_grid_14, tordesillas_demarcation_kernel__spanish_conquest_legitimation, resistance(organizational), 1650, 0.54).
narrative_ontology:measurement(tordesillas_spanish_conquest_grid_15, tordesillas_demarcation_kernel__spanish_conquest_legitimation, resistance(structural), 1493, 0.68).
narrative_ontology:measurement(tordesillas_spanish_conquest_grid_16, tordesillas_demarcation_kernel__spanish_conquest_legitimation, resistance(structural), 1650, 0.73).
narrative_ontology:measurement(tordesillas_spanish_conquest_grid_17, tordesillas_demarcation_kernel__spanish_conquest_legitimation, stakes_inflation(class), 1493, 0.52).
narrative_ontology:measurement(tordesillas_spanish_conquest_grid_18, tordesillas_demarcation_kernel__spanish_conquest_legitimation, stakes_inflation(class), 1650, 0.71).
narrative_ontology:measurement(tordesillas_spanish_conquest_grid_19, tordesillas_demarcation_kernel__spanish_conquest_legitimation, stakes_inflation(individual), 1493, 0.48).
narrative_ontology:measurement(tordesillas_spanish_conquest_grid_20, tordesillas_demarcation_kernel__spanish_conquest_legitimation, stakes_inflation(individual), 1650, 0.89).
narrative_ontology:measurement(tordesillas_spanish_conquest_grid_21, tordesillas_demarcation_kernel__spanish_conquest_legitimation, stakes_inflation(organizational), 1493, 0.61).
narrative_ontology:measurement(tordesillas_spanish_conquest_grid_22, tordesillas_demarcation_kernel__spanish_conquest_legitimation, stakes_inflation(organizational), 1650, 0.76).
narrative_ontology:measurement(tordesillas_spanish_conquest_grid_23, tordesillas_demarcation_kernel__spanish_conquest_legitimation, stakes_inflation(structural), 1493, 0.68).
narrative_ontology:measurement(tordesillas_spanish_conquest_grid_24, tordesillas_demarcation_kernel__spanish_conquest_legitimation, stakes_inflation(structural), 1650, 0.74).
narrative_ontology:measurement(tordesillas_spanish_conquest_grid_25, tordesillas_demarcation_kernel__spanish_conquest_legitimation, suppression(class), 1493, 0.72).
narrative_ontology:measurement(tordesillas_spanish_conquest_grid_26, tordesillas_demarcation_kernel__spanish_conquest_legitimation, suppression(class), 1650, 0.91).
narrative_ontology:measurement(tordesillas_spanish_conquest_grid_27, tordesillas_demarcation_kernel__spanish_conquest_legitimation, suppression(individual), 1493, 0.61).
narrative_ontology:measurement(tordesillas_spanish_conquest_grid_28, tordesillas_demarcation_kernel__spanish_conquest_legitimation, suppression(individual), 1650, 0.94).
narrative_ontology:measurement(tordesillas_spanish_conquest_grid_29, tordesillas_demarcation_kernel__spanish_conquest_legitimation, suppression(organizational), 1493, 0.54).
narrative_ontology:measurement(tordesillas_spanish_conquest_grid_30, tordesillas_demarcation_kernel__spanish_conquest_legitimation, suppression(organizational), 1650, 0.89).
narrative_ontology:measurement(tordesillas_spanish_conquest_grid_31, tordesillas_demarcation_kernel__spanish_conquest_legitimation, suppression(structural), 1493, 0.68).
narrative_ontology:measurement(tordesillas_spanish_conquest_grid_32, tordesillas_demarcation_kernel__spanish_conquest_legitimation, suppression(structural), 1650, 0.92).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(tordesillas_demarcation_kernel__spanish_conquest_legitimation, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(tordesillas_demarcation_kernel__spanish_conquest_legitimation, 0.18).
narrative_ontology:affects_constraint(tordesillas_demarcation_kernel__spanish_conquest_legitimation, tordesillas_demarcation_kernel__portuguese_exploration_legitimation).
narrative_ontology:affects_constraint(tordesillas_demarcation_kernel__spanish_conquest_legitimation, encomienda_labor_extraction_system).
narrative_ontology:affects_constraint(tordesillas_demarcation_kernel__spanish_conquest_legitimation, forced_conversion_indigenous_americas).
narrative_ontology:affects_constraint(tordesillas_demarcation_kernel__spanish_conquest_legitimation, atlantic_slave_trade_labor_substitution).

% DUAL FORMULATION NOTE:
% The Tordesillas demarcation kernel decomposes into two distinct constraint stories reflecting fundamentally different readings of the papal authority and territorial partition: (1) spanish_conquest_legitimation (this story)—emphasizing papal license for conquest, labor extraction, and forced conversion; high epsilon snare whose referent is indigenous subjugation; beneficiary set is Spanish crown/colonial admin/settler class. (2) portuguese_exploration_legitimation (sibling)—emphasizing papal confirmation of exploration priority and mutual exclusion of European competitors; moderate epsilon rope whose referent is orderly European partition; beneficiary set is primarily Iberian crowns, with coordination function for European peace. The two readings instantiate different ε values (0.89 vs. ~0.45 estimated) because they have different referents: Spanish reading measures the standing arrangement west of the line as experienced by victims (extraction-dominated); Portuguese reading measures the standing arrangement as experienced by Iberian powers (coordination-dominated). The epsilon-invariance principle requires two files: a single file attempting both readings would collapse under ε-value ambiguity. Both readings inhabit the same kernel text; the decomposition is epistemologically necessary, not a dispute about what the kernel 'really' says. Link via network.affects_constraints because the Spanish conquest legitimacy claim influences the Portuguese reading: once Spain asserts conquest license, Portugal must either accept the precedent (mutual conquest license) or contest it (territorial partition becomes zero-sum). The Portuguese reading in its historical manifestation accepts the precedent implicitly by not fighting over Western Hemisphere boundary disputes—a structural accommodation that this reading's intensity creates.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
