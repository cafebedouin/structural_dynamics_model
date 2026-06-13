% ============================================================================
% CONSTRAINT STORY: tordesillas_demarcation_kernel__spanish_conquest_legitimation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
    narrative_ontology:constraint_vindicates/2,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: tordesillas_demarcation_kernel__spanish_conquest_legitimation
 *   human_readable: Tordesillas Demarcation as Papal License for Spanish Conquest and Indigenous Subjugation
 *   domain: international_law/colonial_history/sovereignty_theory
 *
 * SUMMARY:
 *   The Treaty of Tordesillas (1494) partitioned the non-Christian world
 *   between Spanish and Portuguese crowns via papal authority. This story
 *   instantiates the SPANISH CONQUEST LEGITIMATION reading: the grant as
 *   license for Spanish territorial conquest, indigenous subjugation,
 *   encomienda labor extraction, and forced conversion in the Americas west
 *   of the demarcation line. The kernel itself (the text of the grant) is
 *   fixed; this reading interprets it as delegating to Spain the right to
 *   conquer and rule non-Christian peoples as tributaries and subjects. The
 *   sibling Portuguese reading interprets the same kernel as confirming
 *   Portuguese exploration rights east of the line and excluding European
 *   rivals. These readings coexist as competing institutional narratives —
 *   the papacy granted both, and both powers implemented their readings
 *   simultaneously. The constraint operates with near-maximal extraction
 *   (0.89) and suppression (0.92) because its enforcement depends on active
 *   erasure of indigenous sovereignty claims and violent subordination of
 *   indigenous resistance.
 *
 * KEY AGENTS:
 *   - Spanish Crown: agenda-setter, collects rents in territory, labor, resources, and geopolitical supremacy. Enforces the grant via military conquest and administrative hierarchy.
 *   - Indigenous populations: payers, stripped of sovereignty, land, labor capacity, and self-determination. Trapped by conquest, disease, and institutional subordination. No exit except death or assimilation.
 *   - Papacy: beneficiary, establishes the precedent that papal supremacy over temporal authority licenses Christian conquest. Gains ideological authority, tribute, and leverage over competing crowns.
 *   - Missionary clergy: dual-positioned beneficiaries and payers. Leverage the grant for conversion authority; internalize identity fusion between salvation and Spanish cultural dominance.
 *   - Rival European powers: excluded, compete for papal favor and seek their own grants. Remain outside the demarcation zone by force and diplomatic pressure.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(tordesillas_demarcation_kernel__spanish_conquest_legitimation, 0.89).
domain_priors:suppression_score(tordesillas_demarcation_kernel__spanish_conquest_legitimation, 0.92).
domain_priors:theater_ratio(tordesillas_demarcation_kernel__spanish_conquest_legitimation, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(tordesillas_demarcation_kernel__spanish_conquest_legitimation, extractiveness, 0.89).
narrative_ontology:constraint_metric(tordesillas_demarcation_kernel__spanish_conquest_legitimation, suppression_requirement, 0.92).
narrative_ontology:constraint_metric(tordesillas_demarcation_kernel__spanish_conquest_legitimation, theater_ratio, 0.68).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(tordesillas_demarcation_kernel__spanish_conquest_legitimation, accessibility_collapse, 0.95).
narrative_ontology:constraint_metric(tordesillas_demarcation_kernel__spanish_conquest_legitimation, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(tordesillas_demarcation_kernel__spanish_conquest_legitimation, snare).
narrative_ontology:human_readable(tordesillas_demarcation_kernel__spanish_conquest_legitimation, "Tordesillas Demarcation as Papal License for Spanish Conquest and Indigenous Subjugation").
narrative_ontology:topic_domain(tordesillas_demarcation_kernel__spanish_conquest_legitimation, "international_law/colonial_history/sovereignty_theory").

domain_priors:requires_active_enforcement(tordesillas_demarcation_kernel__spanish_conquest_legitimation).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(tordesillas_demarcation_kernel__spanish_conquest_legitimation, '9ebbf82a-6cb8-48bf-8b44-d0606899b016').
narrative_ontology:cs_kernel_codification('9ebbf82a-6cb8-48bf-8b44-d0606899b016', fixed_text).
narrative_ontology:cs_authority_grounding('9ebbf82a-6cb8-48bf-8b44-d0606899b016', extraction).
narrative_ontology:cs_interpretation_layer_present('9ebbf82a-6cb8-48bf-8b44-d0606899b016').
narrative_ontology:cs_reading_relation('9ebbf82a-6cb8-48bf-8b44-d0606899b016', tordesillas_demarcation_kernel__portuguese_exploration_legitimation, coexists_with).
narrative_ontology:cs_axiom('9ebbf82a-6cb8-48bf-8b44-d0606899b016', foundational, papal_supremacy_licenses_conquest).
narrative_ontology:cs_axiom_status(papal_supremacy_licenses_conquest, holdable).
narrative_ontology:cs_axiom_grounding('9ebbf82a-6cb8-48bf-8b44-d0606899b016', papal_supremacy_licenses_conquest, theological).
narrative_ontology:cs_axiom('9ebbf82a-6cb8-48bf-8b44-d0606899b016', foundational, non_christian_peoples_lack_sovereign_rights).
narrative_ontology:cs_axiom_status(non_christian_peoples_lack_sovereign_rights, overridden).
narrative_ontology:cs_axiom_grounding('9ebbf82a-6cb8-48bf-8b44-d0606899b016', non_christian_peoples_lack_sovereign_rights, conventional).
narrative_ontology:cs_reference_frame('9ebbf82a-6cb8-48bf-8b44-d0606899b016', european_christian_comity_partition_via_papal_license).
narrative_ontology:cs_drift_state('9ebbf82a-6cb8-48bf-8b44-d0606899b016', enlightenment_challenge_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('9ebbf82a-6cb8-48bf-8b44-d0606899b016', '').
narrative_ontology:cs_kernel_id(tordesillas_demarcation_kernel__spanish_conquest_legitimation, tordesillas_demarcation_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(tordesillas_demarcation_kernel__spanish_conquest_legitimation, spanish_crown_and_colonial_administration).
narrative_ontology:constraint_victim(tordesillas_demarcation_kernel__spanish_conquest_legitimation, indigenous_populations_west_of_line).
narrative_ontology:constraint_victim(tordesillas_demarcation_kernel__spanish_conquest_legitimation, indigenous_political_sovereignty).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(tordesillas_demarcation_kernel__spanish_conquest_legitimation, papacy).
narrative_ontology:constraint_beneficiary(tordesillas_demarcation_kernel__spanish_conquest_legitimation, missionary_clergy).
narrative_ontology:constraint_beneficiary(tordesillas_demarcation_kernel__spanish_conquest_legitimation, spanish_settlers_and_encomenderos).
narrative_ontology:constraint_victim(tordesillas_demarcation_kernel__spanish_conquest_legitimation, missionary_clergy).
narrative_ontology:constraint_victim(tordesillas_demarcation_kernel__spanish_conquest_legitimation, spanish_settlers_and_encomenderos).
narrative_ontology:constraint_vindicates(tordesillas_demarcation_kernel__spanish_conquest_legitimation, papal_supremacy_over_temporal_authority).
narrative_ontology:constraint_vindicates(tordesillas_demarcation_kernel__spanish_conquest_legitimation, european_right_to_subjugate_non_christian_territories).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Holds the papal grant as the legitimizing instrument for conquest west of the demarcation line. Claims authority to partition indigenous lands, extract resources through encomienda, demand labor and tribute, and mandate conversion. Uses the grant as the shield against European rival claims and as the theological authority for subjugation. Collects rents in gold, silver, labor, and territorial control.
narrative_ontology:constraint_stakeholder(tordesillas_demarcation_kernel__spanish_conquest_legitimation, spanish_crown_and_colonial_administration, agenda_setter,
    institutional, generational, arbitrage, global).

% Stripped of territorial sovereignty by the grant's legitimacy claim. Forced into labor (encomienda), tribute extraction, religious conversion, and subordination to Spanish administrative hierarchy. Their own cosmologies, political structures, and land-tenure systems are declared null. Exit is death, displacement, or assimilation; organized resistance is treated as heresy or treason. Their resources flow upward; their self-determination disappears.
narrative_ontology:constraint_stakeholder(tordesillas_demarcation_kernel__spanish_conquest_legitimation, indigenous_populations_west_of_line, payer,
    powerless, generational, trapped, regional).

% Non-agent entity retained for narrative completeness. The collective right of indigenous polities to self-determination and territorial authority is declared extinguished by the papal grant's allocation logic. Their sovereignty is not converted; it is erased from the legal landscape.
narrative_ontology:constraint_stakeholder(tordesillas_demarcation_kernel__spanish_conquest_legitimation, indigenous_political_sovereignty, payer,
    powerless, generational, trapped, regional).
narrative_ontology:stakeholder_non_agent(tordesillas_demarcation_kernel__spanish_conquest_legitimation, indigenous_political_sovereignty).

% Issues the grant, establishing a template by which papal temporal authority (through divine supremacy) can license European conquest of non-Christian territories. Gains ideological authority, tribute from converted lands, and geopolitical leverage over European crowns competing for papal favor. The grant is not merely descriptive; it is the instrument that makes the extraction legal.
narrative_ontology:constraint_stakeholder(tordesillas_demarcation_kernel__spanish_conquest_legitimation, papacy, beneficiary,
    institutional, generational, arbitrage, universal).

% Are contractually excluded from the demarcation zone west of the line by the papal grant's partition logic. They compete for papal recognition and seek their own grants or challenge the grant's validity; their exclusion is what the enforcement apparatus maintains. Entry into the zone is treated as violation of Christian comity and papal authority.
narrative_ontology:constraint_stakeholder(tordesillas_demarcation_kernel__spanish_conquest_legitimation, rival_european_powers, excluded,
    institutional, generational, constrained, global).

% Benefit from the grant as the legitimizing frame for conversion missions: indigenous souls are redeemable only under Spanish sovereignty, and conversion is mandatory under Spanish law. Their spiritual authority is leveraged by the crown to enforce the grant's extraction mechanisms. They also bear the cost of maintaining theological justification for the arrangement and internalize the identity fusion between conversion and cultural annihilation.
narrative_ontology:constraint_stakeholder(tordesillas_demarcation_kernel__spanish_conquest_legitimation, missionary_clergy, beneficiary,
    moderate, generational, identity_locked, regional).
narrative_ontology:stakeholder_secondary_role(tordesillas_demarcation_kernel__spanish_conquest_legitimation, missionary_clergy, payer).

% Receive land grants and labor rights (encomienda) predicated on the grant's legitimacy. They collect tribute and labor from indigenous populations under color of the papal license. They are mobile within the colonial zone but cannot exit the arrangement without losing their status and wealth; their entire stake is the extraction the grant enables.
narrative_ontology:constraint_stakeholder(tordesillas_demarcation_kernel__spanish_conquest_legitimation, spanish_settlers_and_encomenderos, beneficiary,
    moderate, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(tordesillas_demarcation_kernel__spanish_conquest_legitimation, spanish_settlers_and_encomenderos, payer).

% Document and argue about the grant's legitimacy, theological foundations, and historical consequences. They have analytical distance but no direct stake in the extraction. Their interpretations feed back into how the constraint is understood and justified in later periods.
narrative_ontology:constraint_stakeholder(tordesillas_demarcation_kernel__spanish_conquest_legitimation, historical_chroniclers_and_theologians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(tordesillas_demarcation_kernel__spanish_conquest_legitimation, spanish_crown_and_colonial_administration).
narrative_ontology:fixing_cost_class(tordesillas_demarcation_kernel__spanish_conquest_legitimation, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single, internationally recognized legal framework (papal authority) by which European Christian powers can partition non-Christian territories without falling into war with each other. Solves a genuine European geopolitical problem: how to divide the newly encountered world without devastating Christian comity.
% TRANSFER_FUNCTION: Moves sovereignty, resources (gold, silver, spices), labor, and souls from indigenous polities to Spanish Crown and European Christendom. Converts indigenous self-determination into Spanish territorial and extractive authority. Transfers spiritual authority to the papacy and missionary clergy.
% ABSENT_VOICES: Indigenous peoples and their political representatives are not present; they would deny the papal authority to partition their lands and would object to the legitimacy frame entirely. Rival European powers would contest the grant's exclusivity and seek their own authorizations. Dissenting theologians and jurists who question papal temporal authority or the morality of conquest are sidelined by institutional enforcement.
% DISAPPEARANCE_RATIONALE: If the Tordesillas grant's legitimacy disappeared overnight, Spanish territorial claims in the Americas would lose their primary legal foundation. Rival European powers would contest Spanish sovereignty; indigenous polities would reclaim self-determination (though centuries of conquest and disease had already decimated their capacity); the entire colonial administrative structure would face a legitimacy crisis. The New World would reorganize around competing claims without a papal arbiter.
% FOUNDING_PROBLEM: European Christian powers discovered lands inhabited by non-Christian peoples, far from Rome's direct authority. Without a mechanism to partition these lands, competing European claims would fragment Christian unity and provoke wars among Catholic crowns. The papacy needed a way to license exploration and conquest while preserving Christian comity and papal supremacy over temporal affairs.
% FOUNDING_PROBLEM_CORROBORATION: Spanish and papal documents of the period attest the founding problem as live and the grant as its solution. Modern historians of European geopolitics confirm the partition problem was real and urgent for Christian powers. Indigenous peoples and post-colonial analysts attest the grant's persistent legacy as a legitimacy shield for continued extraction and cultural subjugation, confirming the founding problem's operational status even as they dispute the legitimacy of the solution.
narrative_ontology:disappearance_verdict(tordesillas_demarcation_kernel__spanish_conquest_legitimation, world_rearranges).
narrative_ontology:founding_problem_status(tordesillas_demarcation_kernel__spanish_conquest_legitimation, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(tordesillas_demarcation_kernel__spanish_conquest_legitimation, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(tordesillas_demarcation_kernel__spanish_conquest_legitimation, 'none', 1).

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
 *   Extractiveness is near-maximal (0.89) because the entire arrangement is premised on transferring indigenous territory, labor, and tribute to Spanish control without reciprocal benefit or consent. The grant is not a coordination mechanism; it is the title deed for extraction. Suppression is even higher (0.92) because the constraint's persistence depends on active military subjugation, institutional erasure of alternative sovereignty claims, and violent suppression of resistance. Theater ratio rises over the 306-year interval (0.45 → 0.68) as the actual conquest becomes consolidated and enforcement shifts from overt violence to administrative routine and missionary conversion theater — the form of legitimacy performance increases as the need for kinetic force decreases, but the underlying extraction remains constant. The accessibility_collapse of 0.95 reflects that once indigenous populations understood they were trapped by the grant's legal apparatus and Spain's military monopoly, no alternative framing was available to them within the colonial system. The resistance of 0.71 is substantial (indigenous uprisings, persistent spiritual resistance, partial rejection of conversion) but insufficient to overturn the constraint's enforcement apparatus. These metrics describe a pure snare: extraction justified by a false coordination frame (Christian unity against paganism) that benefits only the conquistadors and the papacy.
 *
 * PERSPECTIVAL GAP:
 *   The Spanish Crown and Papacy experience this as a coordination mechanism solving a genuine European problem (partition without Christian war) and a spiritual mechanism (salvation and conversion). Indigenous populations and their descendants experience it as conquest, enslavement, and cultural genocide. The constraint computes differently per seat: from the Crown's institutional position, enforcement maintains legitimate Christian sovereignty; from the indigenous powerless position, suppression is experienced as existential annihilation. The divergence is structural — the grant creates asymmetric power and opposite interests — and is precisely what the snare classification captures.
 *
 * DIRECTIONALITY LOGIC:
 *   Spanish Crown: d ≈ 0.0–0.1 (full beneficiary). Institutional power, global scope, arbitrage-grade exit (can abandon colonies; Spain does not). Collects the constraint's entire extractive output. Papacy: d ≈ 0.05 (strong beneficiary). Institutional power, universal scope, arbitrage exit. Gains supremacy claim and legitimacy rent. Indigenous populations: d ≈ 0.95–1.0 (full target). Powerless, trapped, regional scope. Bears all suppression and extraction. Missionary clergy: d ≈ 0.3–0.4 (moderate beneficiary, partial target). Moderate power, identity_locked exit (conversion is their vocation; cannot exit without renouncing calling). Benefit from the grant's conversion license but bear internalized costs of justifying cultural annihilation. No directionality overrides needed; the derivation chain from beneficiary/victim + exit + power produces accurate d values.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint does NOT exhibit mandatrophy in the classical sense (a function whose founding problem has died but the mechanism persists). The founding problem — controlling European competition for non-Christian territories — remains LIVE throughout the interval, because Spanish and Portuguese enforcement of the demarcation is what keeps the problem solved. The theater_ratio rises (performance of conversion outpaces new conquest) but does not indicate function atrophy; rather, it shows consolidation of extracted power into administrative routine. The snare classification is stable because extraction, suppression, and the beneficiary/victim asymmetry persist throughout. Mandatrophy would appear if the grant's legitimacy collapsed while Spain continued extraction through pure force alone — that inflection occurs in the late 1700s–1800s as Enlightenment challenges the papal basis and indigenous movements gain resources to contest it, but within the 1494–1800 interval the founding problem and its solution remain operationally coupled.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    papal_authority_legitimacy,
    'Is the papacy''s claimed supremacy over temporal affairs sufficient to license conquest and subjugation of non-Christian peoples?',
    'Theological and legal challenge to papal temporal authority; alternative frameworks (divine right of crowns independent of papacy, natural law of indigenous peoples, etc.) that reject the grant''s legitimacy ground.',
    'If papal authority is rejected, the grant loses its primary legitimizing force; Spanish conquest becomes naked territorial seizure rather than licensed conversion and rule. The constraint would reclassify from grant-justified snare to pure military occupation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(papal_authority_legitimacy, conceptual, 'Whether papal supremacy over temporal authority is a valid legitimacy ground for licensing conquest.').

omega_variable(
    indigenous_sovereignty_existential,
    'Did indigenous political entities possess sovereignty and territorial rights that the grant could legally extinguish, or were they never rights-bearing entities under the European legal framework?',
    'Historical anthropology of indigenous governance structures; legal philosophy distinguishing between European sovereignty theory and indigenous political orders; indigenous peoples'' own retrospective accounts of their pre-conquest sovereignty.',
    'If indigenous entities held sovereignty, the grant was an instrument of legal erasure — the snare is constructed via jurisprudential negation. If they held no recognizable sovereignty (under European law), the grant is merely descriptive of an existing European presumption, not constructive of it. The distinction matters for evaluating whether the constraint creates the extraction or merely formalizes it.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(indigenous_sovereignty_existential, empirical, 'Whether indigenous political entities possessed enforceable territorial sovereignty prior to conquest.').

omega_variable(
    forced_conversion_necessity,
    'Is forced conversion to Christianity structurally necessary for the grant''s legitimacy, or is it a cover story for resource extraction?',
    'Analysis of encomienda system outcomes: did conversion rates track missionary effort, or did they track resource scarcity and labor demand? Did conversion reduce or increase extractive demands on indigenous peoples?',
    'If conversion is necessary, the grant frames extraction as a spiritual good; if conversion is instrumental cover, the theater_ratio masks pure economic predation. The classification remains snare either way, but the mechanism differs — theological coercion versus raw economic coercion.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(forced_conversion_necessity, empirical, 'Whether forced conversion is structurally necessary to the grant''s operation or incidental to economic extraction.').

omega_variable(
    suppression_internalization_indigenous,
    'To what extent is the measured suppression (0.92) structural (external military and administrative coercion) versus internalized (indigenous peoples accepting Spanish sovereignty and Christian identity as legitimate)?',
    'Post-conquest suppression trajectory: if suppression persists after Spanish administrative control declines (19th–20th centuries), internalization is high; if resistance resurges when coercive capacity weakens, suppression was primarily structural.',
    'If suppression is highly internalized, the constraint persists through ideology and identity fusion even after institutional enforcement capacity erodes; exit becomes psychological as well as material. If primarily structural, the constraint''s persistence depends on continuous military and administrative investment.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_internalization_indigenous, empirical, 'Structural versus internalized mechanisms of suppression in the conquest regime.').

omega_variable(
    kernel_reading_foreclosure_possibility,
    'Does the Spanish conquest legitimation reading logically foreclose the Portuguese exploration reading, or can both readings coexist in the same institutional framework?',
    'Jurisprudential analysis: if the papacy granted conquest rights to Spain, did it simultaneously withhold them from Portugal in the eastern hemisphere, or did it grant Portugal equivalent conquest rights? Can one crown have a license to conquer while the other merely has exploration rights?',
    'If readings foreclose each other, they compete for institutional supremacy; only one can be canonical. If they coexist, both crowns operated under the same grant with different interpretations — each justified its own reading and excluded the other''s competition within its zone. The coexistence case is the empirical reality; the question tests whether the logical structure permits it.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_foreclosure_possibility, conceptual, 'Whether the Spanish and Portuguese readings of the Tordesillas kernel logically foreclose each other or can coexist as competing institutional narratives.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(tordesillas_demarcation_kernel__spanish_conquest_legitimation, 1494, 1800).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tord_tr_t1494, tordesillas_demarcation_kernel__spanish_conquest_legitimation, theater_ratio, 1494, 0.45).
narrative_ontology:measurement_basis(tord_tr_t1494, observed).
narrative_ontology:measurement(tord_tr_t1550, tordesillas_demarcation_kernel__spanish_conquest_legitimation, theater_ratio, 1550, 0.52).
narrative_ontology:measurement_basis(tord_tr_t1550, observed).
narrative_ontology:measurement(tord_tr_t1650, tordesillas_demarcation_kernel__spanish_conquest_legitimation, theater_ratio, 1650, 0.63).
narrative_ontology:measurement_basis(tord_tr_t1650, observed).
narrative_ontology:measurement(tord_tr_t1750, tordesillas_demarcation_kernel__spanish_conquest_legitimation, theater_ratio, 1750, 0.7).
narrative_ontology:measurement_basis(tord_tr_t1750, observed).
narrative_ontology:measurement(tord_tr_t1800, tordesillas_demarcation_kernel__spanish_conquest_legitimation, theater_ratio, 1800, 0.68).
narrative_ontology:measurement_basis(tord_tr_t1800, observed).

% Extraction over time
narrative_ontology:measurement(tord_be_t1494, tordesillas_demarcation_kernel__spanish_conquest_legitimation, base_extractiveness, 1494, 0.78).
narrative_ontology:measurement_basis(tord_be_t1494, observed).
narrative_ontology:measurement(tord_be_t1550, tordesillas_demarcation_kernel__spanish_conquest_legitimation, base_extractiveness, 1550, 0.84).
narrative_ontology:measurement_basis(tord_be_t1550, observed).
narrative_ontology:measurement(tord_be_t1650, tordesillas_demarcation_kernel__spanish_conquest_legitimation, base_extractiveness, 1650, 0.89).
narrative_ontology:measurement_basis(tord_be_t1650, observed).
narrative_ontology:measurement(tord_be_t1750, tordesillas_demarcation_kernel__spanish_conquest_legitimation, base_extractiveness, 1750, 0.91).
narrative_ontology:measurement_basis(tord_be_t1750, observed).
narrative_ontology:measurement(tord_be_t1800, tordesillas_demarcation_kernel__spanish_conquest_legitimation, base_extractiveness, 1800, 0.89).
narrative_ontology:measurement_basis(tord_be_t1800, observed).

% Suppression requirement over time
narrative_ontology:measurement(tord_su_t1494, tordesillas_demarcation_kernel__spanish_conquest_legitimation, suppression_requirement, 1494, 0.82).
narrative_ontology:measurement_basis(tord_su_t1494, observed).
narrative_ontology:measurement(tord_su_t1550, tordesillas_demarcation_kernel__spanish_conquest_legitimation, suppression_requirement, 1550, 0.87).
narrative_ontology:measurement_basis(tord_su_t1550, observed).
narrative_ontology:measurement(tord_su_t1650, tordesillas_demarcation_kernel__spanish_conquest_legitimation, suppression_requirement, 1650, 0.91).
narrative_ontology:measurement_basis(tord_su_t1650, observed).
narrative_ontology:measurement(tord_su_t1750, tordesillas_demarcation_kernel__spanish_conquest_legitimation, suppression_requirement, 1750, 0.93).
narrative_ontology:measurement_basis(tord_su_t1750, observed).
narrative_ontology:measurement(tord_su_t1800, tordesillas_demarcation_kernel__spanish_conquest_legitimation, suppression_requirement, 1800, 0.92).
narrative_ontology:measurement_basis(tord_su_t1800, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(tordesillas_demarcation_kernel__spanish_conquest_legitimation, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(tordesillas_demarcation_kernel__spanish_conquest_legitimation, 0.22).
narrative_ontology:affects_constraint(tordesillas_demarcation_kernel__spanish_conquest_legitimation, tordesillas_demarcation_kernel__portuguese_exploration_legitimation).
narrative_ontology:affects_constraint(tordesillas_demarcation_kernel__spanish_conquest_legitimation, encomienda_labor_extraction_system).
narrative_ontology:affects_constraint(tordesillas_demarcation_kernel__spanish_conquest_legitimation, papal_supremacy_over_temporal_authority).

% DUAL FORMULATION NOTE:
% This constraint and the Portuguese sibling (exploration_legitimation) form a constraint family sharing the Tordesillas kernel. They instantiate competing readings of the same fixed text; the readings coexist as institutional narratives held by different powers. The Spanish reading (this file) interprets the grant as licensing conquest and subjugation; the Portuguese reading interprets it as confirming exploration rights and exclusion of rivals. Both readings implement enforcement of the grant simultaneously. The family is linked by network.affects_constraints in both directions. The ε values differ substantially (Spanish reading ≈0.89, Portuguese reading ≈0.70) because conquest with mass subjugation is more extractive than exploration with exclusion. The beneficiary/victim structures also differ: Spanish reading has indigenous populations as victims; Portuguese reading has rival explorers as excluded parties.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
