% ============================================================================
% CONSTRAINT STORY: biblical_divine_nature__trinitarian_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_biblical_divine_nature__trinitarian_reading, []).

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
 *   constraint_id: biblical_divine_nature__trinitarian_reading
 *   human_readable: Trinitarian Reading of the Divine Nature (Three Hypostases, One Ousia)
 *   domain: theology/religious_authority/doctrinal_history
 *
 * SUMMARY:
 *   This story models the Trinitarian reading of the biblical divine nature
 *   kernel: three hypostases sharing one ousia, preserving monotheism through
 *   essence-unity. From 325 CE (Nicaea) through 381 CE (Constantinople) and
 *   beyond, this formula was fixed as the boundary of orthodox Christian
 *   identity, first by conciliar anathema and then by imperial civil
 *   enforcement under Theodosius I. The reading's persistence depends on an
 *   active institutional hierarchy (episcopal sees, later confessional
 *   bodies) continuing to police the boundary against subordinationist
 *   (Arian), strictly unitarian, and modalist/Oneness formulations. This is
 *   ONE of three sibling readings of the same underlying kernel — the
 *   modalist_reading and unitarian_reading are separate constraint stories
 *   with their own epsilon values, beneficiary sets, and victim sets; they
 *   are not folded into this one.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(biblical_divine_nature__trinitarian_reading, 0.62).
domain_priors:suppression_score(biblical_divine_nature__trinitarian_reading, 0.78).
domain_priors:theater_ratio(biblical_divine_nature__trinitarian_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(biblical_divine_nature__trinitarian_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(biblical_divine_nature__trinitarian_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(biblical_divine_nature__trinitarian_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(biblical_divine_nature__trinitarian_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(biblical_divine_nature__trinitarian_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(biblical_divine_nature__trinitarian_reading, tangled_rope).
narrative_ontology:human_readable(biblical_divine_nature__trinitarian_reading, "Trinitarian Reading of the Divine Nature (Three Hypostases, One Ousia)").
narrative_ontology:topic_domain(biblical_divine_nature__trinitarian_reading, "theology/religious_authority/doctrinal_history").

domain_priors:requires_active_enforcement(biblical_divine_nature__trinitarian_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(biblical_divine_nature__trinitarian_reading, '4b24495d-b73b-4bff-940e-7f5d35e1d8d9').
narrative_ontology:cs_kernel_codification('4b24495d-b73b-4bff-940e-7f5d35e1d8d9', formalized).
narrative_ontology:cs_authority_grounding('4b24495d-b73b-4bff-940e-7f5d35e1d8d9', lineage).
narrative_ontology:cs_interpretation_layer_present('4b24495d-b73b-4bff-940e-7f5d35e1d8d9').
narrative_ontology:cs_reading_relation('4b24495d-b73b-4bff-940e-7f5d35e1d8d9', biblical_divine_nature__unitarian_reading, forecloses).
narrative_ontology:cs_reading_relation('4b24495d-b73b-4bff-940e-7f5d35e1d8d9', biblical_divine_nature__modalist_reading, forecloses).
narrative_ontology:cs_axiom('4b24495d-b73b-4bff-940e-7f5d35e1d8d9', foundational, three_coequal_coeternal_hypostases).
narrative_ontology:cs_axiom_status(three_coequal_coeternal_hypostases, holdable).
narrative_ontology:cs_axiom_grounding('4b24495d-b73b-4bff-940e-7f5d35e1d8d9', three_coequal_coeternal_hypostases, deontological).
narrative_ontology:cs_axiom('4b24495d-b73b-4bff-940e-7f5d35e1d8d9', foundational, single_undivided_ousia_grounds_monotheism).
narrative_ontology:cs_axiom_status(single_undivided_ousia_grounds_monotheism, holdable).
narrative_ontology:cs_axiom_grounding('4b24495d-b73b-4bff-940e-7f5d35e1d8d9', single_undivided_ousia_grounds_monotheism, conventional).
narrative_ontology:cs_reference_frame('4b24495d-b73b-4bff-940e-7f5d35e1d8d9', nicene_constantinopolitan_creedal_settlement).
narrative_ontology:cs_drift_state('4b24495d-b73b-4bff-940e-7f5d35e1d8d9', contemporary_pluralist_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('4b24495d-b73b-4bff-940e-7f5d35e1d8d9', '').
narrative_ontology:cs_kernel_id(biblical_divine_nature__trinitarian_reading, biblical_divine_nature).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(biblical_divine_nature__trinitarian_reading, nicene_ecumenical_hierarchy).
narrative_ontology:constraint_beneficiary(biblical_divine_nature__trinitarian_reading, trinitarian_clergy_and_theologians).
narrative_ontology:constraint_beneficiary(biblical_divine_nature__trinitarian_reading, conciliar_imperial_authority).
narrative_ontology:constraint_victim(biblical_divine_nature__trinitarian_reading, arian_clergy_and_laity).
narrative_ontology:constraint_victim(biblical_divine_nature__trinitarian_reading, unitarian_congregations).
narrative_ontology:constraint_victim(biblical_divine_nature__trinitarian_reading, oneness_pentecostal_communities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(biblical_divine_nature__trinitarian_reading, lay_believers_within_orthodoxy).
narrative_ontology:constraint_victim(biblical_divine_nature__trinitarian_reading, lay_believers_within_orthodoxy).
narrative_ontology:constraint_vindicates(biblical_divine_nature__trinitarian_reading, monotheistic_unity_of_god).
narrative_ontology:constraint_vindicates(biblical_divine_nature__trinitarian_reading, full_divinity_of_christ).
narrative_ontology:constraint_vindicates(biblical_divine_nature__trinitarian_reading, nicene_creed_authority).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Convenes and ratifies councils (Nicaea 325, Constantinople 381) that fix the homoousios formula, drafts creeds, and issues anathemas against dissenting formulations. Administers the doctrinal boundary and determines who counts as within communion.
narrative_ontology:constraint_stakeholder(biblical_divine_nature__trinitarian_reading, nicene_ecumenical_hierarchy, agenda_setter,
    institutional, civilizational, arbitrage, global).

% Build careers, sees, and theological authority on correct articulation and defense of the three-hypostases-one-ousia formula. Gain standing, patronage, and doctrinal legitimacy from policing the boundary; can move between sees within the orthodox communion without cost.
narrative_ontology:constraint_stakeholder(biblical_divine_nature__trinitarian_reading, trinitarian_clergy_and_theologians, beneficiary,
    organized, generational, mobile, global).

% Roman and Byzantine imperial power backs conciliar decisions with civil enforcement — exile, property confiscation, loss of clerical office — converting a doctrinal dispute into an imperial law-and-order matter. Benefits from a single unified state religion that stabilizes political legitimacy.
narrative_ontology:constraint_stakeholder(biblical_divine_nature__trinitarian_reading, conciliar_imperial_authority, beneficiary,
    institutional, generational, arbitrage, continental).
narrative_ontology:stakeholder_secondary_role(biblical_divine_nature__trinitarian_reading, conciliar_imperial_authority, agenda_setter).

% Hold that the Son is subordinate to and created by the Father; declared heretical at Nicaea and subsequent councils, deposed from clerical office, exiled, and had scriptures and congregations suppressed. Their theological position becomes legally and socially costly to hold once the formula is fixed.
narrative_ontology:constraint_stakeholder(biblical_divine_nature__trinitarian_reading, arian_clergy_and_laity, payer,
    moderate, biographical, trapped, continental).

% Affirm the numerical singularity of God with the Father alone as God; across the centuries following Nicaea (and again in later Socinian/Unitarian movements) they are excluded from mainstream Christian communion, denied recognition as Christian, and in earlier periods subject to civil penalty for antitrinitarian teaching.
narrative_ontology:constraint_stakeholder(biblical_divine_nature__trinitarian_reading, unitarian_congregations, payer,
    powerless, civilizational, trapped, regional).

% Hold a modalist-adjacent, non-Trinitarian baptismal and worship practice; in the twentieth century they are formally excluded from Trinitarian Pentecostal and evangelical fellowships, denied credentialing in Trinitarian denominational bodies, and characterized as outside orthodox Christianity despite shared broader evangelical commitments.
narrative_ontology:constraint_stakeholder(biblical_divine_nature__trinitarian_reading, oneness_pentecostal_communities, payer,
    moderate, biographical, constrained, national).

% Receive a stabilized, internally coherent account of God's unity and Christ's full divinity that resolves worship-directed devotion to Jesus without abandoning monotheism. Also bear the cost of doctrinal policing when their own private formulations drift from the technical creedal language and draw clerical correction.
narrative_ontology:constraint_stakeholder(biblical_divine_nature__trinitarian_reading, lay_believers_within_orthodoxy, beneficiary,
    powerless, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(biblical_divine_nature__trinitarian_reading, lay_believers_within_orthodoxy, payer).

% Study the councils, the political pressures on Constantine and Theodosius, the semantic instability of hypostasis/ousia across Greek and Latin traditions, and the enforcement record against dissenters, without a stake in the doctrinal outcome.
narrative_ontology:constraint_stakeholder(biblical_divine_nature__trinitarian_reading, historical_theologians_and_patristics_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(biblical_divine_nature__trinitarian_reading, nicene_ecumenical_hierarchy).
narrative_ontology:fixing_cost_class(biblical_divine_nature__trinitarian_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single, teachable formula that lets a fragmented set of local churches converge on one account of how Christ's worship-worthiness and the Spirit's activity are compatible with the strict monotheism inherited from Israelite scripture, preventing doctrinal fragmentation into mutually anathematizing sects at scale.
% TRANSFER_FUNCTION: Moves theological legitimacy, clerical office, congregational property, and civil standing from those who articulate the divine nature differently (subordinationist, unitarian, modalist) to those who hold the three-hypostases-one-ousia formula, backed first by conciliar anathema and later by imperial civil penalty.
% ABSENT_VOICES: Arian bishops present at Nicaea were outvoted and later exiled rather than persuaded; Jewish and Islamic monotheistic critiques of the formula's coherence are excluded from the intra-Christian conciliar process entirely; Oneness Pentecostal and Unitarian voices in later centuries are excluded from ecumenical bodies that define 'orthodox Christian' for interfaith and institutional purposes.
% DISAPPEARANCE_RATIONALE: If the homoousios formula and its enforcement apparatus vanished, denominational boundaries defining who may hold clerical office, participate in ecumenical councils, and be recognized as doctrinally orthodox would immediately reorganize; centuries of excluded communities (Unitarian, Oneness Pentecostal, historic Arian-descended groups) would have no structural barrier to inclusion in mainstream Christian institutional recognition.
% FOUNDING_PROBLEM: Early Christian communities worshiped Jesus as divine and experienced the Spirit as active while inheriting strict Jewish monotheism (Shema); without a formula, this produced incompatible local accounts — some effectively tritheist, some subordinationist, some modalist — threatening both theological coherence and ecclesial unity across a rapidly institutionalizing church.
% FOUNDING_PROBLEM_CORROBORATION: Patristic historians and comparative religion scholars outside any confessional Trinitarian commitment (e.g. historians of Constantine's political motives for convening Nicaea) attest the underlying coordination problem was real but argue the specific resolution was substantially shaped by imperial political convenience rather than pure theological necessity; Trinitarian theologians themselves attest the problem remains live as an ongoing catechetical and apologetic task, which is itself evidence the corroboration is not independent of the beneficiary class on that half of the claim.
narrative_ontology:disappearance_verdict(biblical_divine_nature__trinitarian_reading, world_rearranges).
narrative_ontology:founding_problem_status(biblical_divine_nature__trinitarian_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(biblical_divine_nature__trinitarian_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(biblical_divine_nature__trinitarian_reading, 'none', 1).
narrative_ontology:epsilon_provenance(biblical_divine_nature__trinitarian_reading, 0.62, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(biblical_divine_nature__trinitarian_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(biblical_divine_nature__trinitarian_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(biblical_divine_nature__trinitarian_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction (epsilon ~0.62) reflects that clerical office, congregational legitimacy, and (historically) civil standing were transferred from non-Trinitarian communities to Trinitarian institutional authority through anathema and, at points, imperial force — not merely through persuasion. Suppression is high overall (0.78 base) but the temporal series shows a sharp spike at 381-500 CE (Theodosian enforcement, closure of Arian churches) and a long decline through the Reformation and into the modern era as civil penalties for antitrinitarian belief lapsed in most jurisdictions, even as social/institutional exclusion (denial of 'Christian' status to Unitarians and Oneness Pentecostals) persisted at lower intensity. Theater ratio is moderate and slowly rising, reflecting that much contemporary enforcement is credal/confessional rather than coercive — churches require creedal assent for membership or ordination without civil force behind it.
 *
 * DIRECTIONALITY LOGIC:
 *   The nicene_ecumenical_hierarchy and conciliar_imperial_authority sit at the beneficiary end: they set the doctrinal boundary and historically enforced it with state power, gaining unified religious-political legitimacy. Trinitarian clergy and theologians benefit through career structures built on defending the formula. Arian clergy, Unitarian congregations, and Oneness Pentecostal communities sit at the target end: trapped or constrained exit (leaving does not remove the label of heterodoxy; it only changes which excluded community one belongs to), and they bear the historical costs of exile, deposition, and exclusion from ecumenical recognition. Lay believers within orthodoxy are dual-positioned: they benefit from doctrinal coherence but also bear low-grade policing costs when private belief drifts from technical formulation.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (reconciling worship of Christ and the Spirit's activity with monotheism) may remain partially live as a genuine theological and catechetical task, which prevents an easy 'zombie institution' verdict. But the enforcement apparatus — anathema, denial of Christian recognition to Oneness Pentecostals and Unitarians into the twentieth and twenty-first centuries — persists well past the point where civil coercion was necessary or available, suggesting the boundary-policing function has partially outlived its coordination necessity and now runs substantially on institutional identity maintenance (theater_ratio trending upward through the medieval and modern periods).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    trinitarian_reading_identity,
    'This constraint is one reading of the biblical_divine_nature kernel (trinitarian_reading). The sibling readings — modalist_reading and unitarian_reading — are separate constraint stories with independent epsilon values and beneficiary/victim structures. What would change if the unitarian_reading were adopted instead?',
    'Compare the three sibling constraint stories directly: under unitarian_reading, the beneficiary and victim sets invert (Trinitarian clergy become the excluded/anathematized party relative to a unitarian ecclesial authority), and epsilon is authored independently for that reading''s own standing arrangement.',
    'Confirms that this story''s epsilon (0.62) is specific to the Trinitarian institutional arrangement and its historical enforcement record, not a comparative or averaged value across the kernel''s readings. No cross-reading averaging should occur.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(trinitarian_reading_identity, conceptual, 'Documents this story''s status as one reading among three siblings sharing the biblical_divine_nature kernel.').

omega_variable(
    council_political_vs_theological_necessity,
    'Was the specific homoousios resolution at Nicaea/Constantinople a theologically necessary solution to the coordination problem (reconciling Christ''s divinity with monotheism), or was it substantially shaped by Constantine''s and Theodosius''s political interest in a unified imperial religion?',
    'Historical analysis of imperial correspondence, conciliar voting patterns and exile records, and comparison with alternative resolutions (subordinationism, modalism) that were theologically coherent but politically disfavored.',
    'If substantially politically shaped, the coordination function claimed for this reading is partly cover for a state-legitimacy transfer, pushing the classification toward snare; if substantially theologically necessary, the coordination function is more genuine, supporting the tangled_rope reading as authored.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(council_political_vs_theological_necessity, empirical, 'Whether Nicene/Constantinopolitan resolution reflects theological necessity or imperial political convenience.').

omega_variable(
    modern_enforcement_mechanism_ambiguity,
    'In the contemporary period, is the exclusion of Oneness Pentecostal and Unitarian communities from ''orthodox Christian'' recognition maintained by genuine theological conviction (structural) or by institutional inertia and denominational boundary-maintenance incentives (internalized/administrative)?',
    'Survey of contemporary denominational statements and credentialing bodies: do exclusion criteria reference live theological argument or largely cite historical creedal conformity as a membership test without renewed argument?',
    'If largely administrative inertia, the contemporary theater_ratio is understated and the constraint trends closer to piton in its present-day operation despite tangled_rope classification across the full historical interval.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(modern_enforcement_mechanism_ambiguity, empirical, 'Whether present-day non-Trinitarian exclusion is live theological conviction or administrative/credentialing inertia.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(biblical_divine_nature__trinitarian_reading, 325, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bibl_tr_t325, biblical_divine_nature__trinitarian_reading, theater_ratio, 325, 0.15).
narrative_ontology:measurement(bibl_tr_t381, biblical_divine_nature__trinitarian_reading, theater_ratio, 381, 0.2).
narrative_ontology:measurement(bibl_tr_t500, biblical_divine_nature__trinitarian_reading, theater_ratio, 500, 0.25).
narrative_ontology:measurement(bibl_tr_t1054, biblical_divine_nature__trinitarian_reading, theater_ratio, 1054, 0.3).
narrative_ontology:measurement(bibl_tr_t1517, biblical_divine_nature__trinitarian_reading, theater_ratio, 1517, 0.32).
narrative_ontology:measurement(bibl_tr_t1900, biblical_divine_nature__trinitarian_reading, theater_ratio, 1900, 0.35).
narrative_ontology:measurement(bibl_tr_t2025, biblical_divine_nature__trinitarian_reading, theater_ratio, 2025, 0.3).

% Extraction over time
narrative_ontology:measurement(bibl_be_t325, biblical_divine_nature__trinitarian_reading, base_extractiveness, 325, 0.55).
narrative_ontology:measurement(bibl_be_t381, biblical_divine_nature__trinitarian_reading, base_extractiveness, 381, 0.68).
narrative_ontology:measurement(bibl_be_t500, biblical_divine_nature__trinitarian_reading, base_extractiveness, 500, 0.72).
narrative_ontology:measurement(bibl_be_t1054, biblical_divine_nature__trinitarian_reading, base_extractiveness, 1054, 0.6).
narrative_ontology:measurement(bibl_be_t1517, biblical_divine_nature__trinitarian_reading, base_extractiveness, 1517, 0.58).
narrative_ontology:measurement(bibl_be_t1900, biblical_divine_nature__trinitarian_reading, base_extractiveness, 1900, 0.5).
narrative_ontology:measurement(bibl_be_t2025, biblical_divine_nature__trinitarian_reading, base_extractiveness, 2025, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(bibl_su_t325, biblical_divine_nature__trinitarian_reading, suppression_requirement, 325, 0.5).
narrative_ontology:measurement(bibl_su_t381, biblical_divine_nature__trinitarian_reading, suppression_requirement, 381, 0.85).
narrative_ontology:measurement(bibl_su_t500, biblical_divine_nature__trinitarian_reading, suppression_requirement, 500, 0.9).
narrative_ontology:measurement(bibl_su_t1054, biblical_divine_nature__trinitarian_reading, suppression_requirement, 1054, 0.65).
narrative_ontology:measurement(bibl_su_t1517, biblical_divine_nature__trinitarian_reading, suppression_requirement, 1517, 0.55).
narrative_ontology:measurement(bibl_su_t1900, biblical_divine_nature__trinitarian_reading, suppression_requirement, 1900, 0.35).
narrative_ontology:measurement(bibl_su_t2025, biblical_divine_nature__trinitarian_reading, suppression_requirement, 2025, 0.3).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(biblical_divine_nature__trinitarian_reading, identity_coordination).
narrative_ontology:affects_constraint(biblical_divine_nature__trinitarian_reading, unitarian_reading).
narrative_ontology:affects_constraint(biblical_divine_nature__trinitarian_reading, modalist_reading).

% DUAL FORMULATION NOTE:
% This story is one of three sibling readings of the biblical_divine_nature kernel (trinitarian_reading, unitarian_reading, modalist_reading). Each reading is authored as an independent constraint with its own epsilon, beneficiary/victim structure, and claimed type, per the epsilon-invariance principle — the natural-language label 'the doctrine of God' or 'Christian monotheism' conflates three structurally distinct claims about how divine unity and plurality relate. Network edges link all three so contamination/coupling analysis can trace how enforcement or legitimacy shifts in one reading affect the others (e.g. loss of state-backed enforcement for the trinitarian_reading historically increased the viability of unitarian_reading communities in the early modern period).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
