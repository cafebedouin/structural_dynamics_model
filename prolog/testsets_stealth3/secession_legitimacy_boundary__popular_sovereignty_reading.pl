% ============================================================================
% CONSTRAINT STORY: secession_legitimacy_boundary__popular_sovereignty_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_secession_legitimacy_boundary__popular_sovereignty_reading, []).

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
 *   constraint_id: secession_legitimacy_boundary__popular_sovereignty_reading
 *   human_readable: Provincial-Majority Plebiscitary Self-Legitimation Rule (Popular-Sovereignty Reading)
 *   domain: political economy/federalism/resource politics
 *
 * SUMMARY:
 *   A province whose consolidated majority community pursues sovereign
 *   statehood adopts a legitimacy doctrine: a democratic majority within the
 *   provincial boundary holds ultimate sovereignty, and a referendum result
 *   is self-legitimating - it requires no constitutional ratification, no
 *   federal consent, and no assent from populations inside the boundary who
 *   hold prior jurisdictional claims. The doctrine was written into party
 *   doctrine in the mid-1970s, exercised twice (1980, 1995), confronted by
 *   federal clarity legislation and court reference answers around 2000, and
 *   has since persisted in dormant but institutionally maintained form. The
 *   epsilon referent for this kernel-reading story is the standing
 *   arrangement under contest - the operation of the provincial-majority
 *   plebiscitary rule itself, including its dormancy - assessed by the
 *   reading's own lights; the constitutional-impossibility,
 *   grievance-threshold, and treaty-primacy readings are separate stories
 *   with separate victim sets and separate epsilon values. The claim and the
 *   metrics are independent authored facts: this story CLAIMS tangled_rope
 *   because the structure holds both a real coordination half and a real
 *   extraction half, while the metrics are authored from the rule's observed
 *   operation.
 *
 * KEY AGENTS:
 *   - - provincial_secessionist_majority: primary beneficiary (organized/constrained) - its majority preference converts directly into statehood for the territory
 *   - - secessionist_party_leadership: agenda setter (institutional/constrained) - drafts the question, times the vote, collects the governing payoff
 *   - - indigenous_treaty_nations: primary target (moderate/trapped) - bound by a vote their prior treaties place outside, no relocation available
 *   - - provincial_federalist_voters: target with demonstrated partial exit (organized/constrained)
 *   - - provincial_linguistic_minorities: target with portable-skill exit (organized/mobile)
 *   - - federal_authority: subordinated target (institutional/constrained) - defends the order the rule overrides but cannot invalidate the vote within the rule's logic
 *   - - rest_of_federation_residents: externalized target (organized/constrained) - absorbs partition costs with no ballot
 *   - - international_recognition_community: analytical observer (institutional/global) - converts legitimacy claims into statehood fact
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(secession_legitimacy_boundary__popular_sovereignty_reading, 0.62).
domain_priors:suppression_score(secession_legitimacy_boundary__popular_sovereignty_reading, 0.55).
domain_priors:theater_ratio(secession_legitimacy_boundary__popular_sovereignty_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(secession_legitimacy_boundary__popular_sovereignty_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(secession_legitimacy_boundary__popular_sovereignty_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(secession_legitimacy_boundary__popular_sovereignty_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(secession_legitimacy_boundary__popular_sovereignty_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(secession_legitimacy_boundary__popular_sovereignty_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(secession_legitimacy_boundary__popular_sovereignty_reading, tangled_rope).
narrative_ontology:human_readable(secession_legitimacy_boundary__popular_sovereignty_reading, "Provincial-Majority Plebiscitary Self-Legitimation Rule (Popular-Sovereignty Reading)").
narrative_ontology:topic_domain(secession_legitimacy_boundary__popular_sovereignty_reading, "political economy/federalism/resource politics").

domain_priors:requires_active_enforcement(secession_legitimacy_boundary__popular_sovereignty_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(secession_legitimacy_boundary__popular_sovereignty_reading, '381389bc-9f73-49e6-a46d-e34ca9a603c5').
narrative_ontology:cs_kernel_codification('381389bc-9f73-49e6-a46d-e34ca9a603c5', distributed).
narrative_ontology:cs_authority_grounding('381389bc-9f73-49e6-a46d-e34ca9a603c5', practice).
narrative_ontology:cs_reading_relation('381389bc-9f73-49e6-a46d-e34ca9a603c5', secession_legitimacy_boundary__constitutional_impossibility_reading, forecloses).
narrative_ontology:cs_reading_relation('381389bc-9f73-49e6-a46d-e34ca9a603c5', secession_legitimacy_boundary__grievance_threshold_reading, coexists_with).
narrative_ontology:cs_reading_relation('381389bc-9f73-49e6-a46d-e34ca9a603c5', secession_legitimacy_boundary__treaty_primacy_reading, forecloses).
narrative_ontology:cs_axiom('381389bc-9f73-49e6-a46d-e34ca9a603c5', foundational, provincial_majority_ultimacy).
narrative_ontology:cs_axiom_status(provincial_majority_ultimacy, holdable).
narrative_ontology:cs_axiom_grounding('381389bc-9f73-49e6-a46d-e34ca9a603c5', provincial_majority_ultimacy, deontological).
narrative_ontology:cs_axiom('381389bc-9f73-49e6-a46d-e34ca9a603c5', foundational, referendum_result_self_validating).
narrative_ontology:cs_axiom_status(referendum_result_self_validating, holdable).
narrative_ontology:cs_axiom_grounding('381389bc-9f73-49e6-a46d-e34ca9a603c5', referendum_result_self_validating, conventional).
narrative_ontology:cs_reference_frame('381389bc-9f73-49e6-a46d-e34ca9a603c5', plebiscitary_territorial_sovereignty).
narrative_ontology:cs_drift_state('381389bc-9f73-49e6-a46d-e34ca9a603c5', post_clarity_statute_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('381389bc-9f73-49e6-a46d-e34ca9a603c5', '').
narrative_ontology:cs_kernel_id(secession_legitimacy_boundary__popular_sovereignty_reading, secession_legitimacy_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(secession_legitimacy_boundary__popular_sovereignty_reading, provincial_secessionist_majority).
narrative_ontology:constraint_beneficiary(secession_legitimacy_boundary__popular_sovereignty_reading, secessionist_party_leadership).
narrative_ontology:constraint_victim(secession_legitimacy_boundary__popular_sovereignty_reading, indigenous_treaty_nations).
narrative_ontology:constraint_victim(secession_legitimacy_boundary__popular_sovereignty_reading, provincial_linguistic_minorities).
narrative_ontology:constraint_victim(secession_legitimacy_boundary__popular_sovereignty_reading, provincial_federalist_voters).
narrative_ontology:constraint_victim(secession_legitimacy_boundary__popular_sovereignty_reading, federal_authority).
narrative_ontology:constraint_victim(secession_legitimacy_boundary__popular_sovereignty_reading, rest_of_federation_residents).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The territorially consolidated voting bloc that supplies the rule's operative majorities: a linguistically and historically defined community concentrated in the province, mobilized periodically around the sovereignty question. When the rule fires, its majority preference converts directly into statehood for the territory it inhabits. Between campaigns its members carry the economic uncertainty of repeated referendums and the identity investment of a long nation-building project. Individual relocation away from the community's home territory means forfeiting the linguistic and civic life the project protects, so exit is costly and uncommon.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__popular_sovereignty_reading, provincial_secessionist_majority, beneficiary,
    organized, biographical, constrained, regional).

% The provincial party apparatus that wrote the rule into doctrine, drafted the referendum questions, called the votes, and would staff the institutions of any successor state. It controls the timing and wording of each vote and the campaign machinery behind them. A successful referendum converts directly into governing control of a new state's executive, bureaucracy, and resource revenues. Its members' careers, pensions, and historical legacies are bound up with the project continuing.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__popular_sovereignty_reading, secessionist_party_leadership, agenda_setter,
    institutional, generational, constrained, regional).

% Province-resident voters who identify with the existing country and vote against secession whenever the question appears. Under the rule their votes count only as arithmetic obstacles: a winning majority binds them to a citizenship and a border they rejected, with compensation negotiated afterward by the very governments the rule empowers. Skilled and bilingual members have repeatedly demonstrated exit by relocating to other provinces; rooted households, older members, and those holding property in the province absorb the outcome in place.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__popular_sovereignty_reading, provincial_federalist_voters, payer,
    organized, biographical, constrained, regional).

% Anglophone and allophone communities inside the province whose schools, hospitals, and professional institutions are organized around the existing federation. The rule counts them in the provincial denominator while the sovereignty project's identity core places them outside the nation being enacted; a passing vote subjects their language rights, property, and professional credentials to renegotiation by the successor state. Their skills and languages travel well, and waves of out-migration have followed each campaign, but community institutions, family land, and aging members anchor a large residue that cannot leave.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__popular_sovereignty_reading, provincial_linguistic_minorities, payer,
    organized, biographical, mobile, regional).

% Nations holding treaties concluded with the Crown before the province existed, with territories and communities spanning the provincial line. The rule treats the provincial boundary as the demos and the referendum as dispositive; their own governance structures, treaty relationships, and asserted vetoes carry no decision-weight in the vote that would redraw the status of their territories. They cannot relocate their territories or their treaty relationships. Their leaders respond by organizing parallel assertion of jurisdiction, but inside the rule's logic their position is that of a population bound by a vote they were never sovereign within.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__popular_sovereignty_reading, indigenous_treaty_nations, payer,
    moderate, generational, trapped, regional).
narrative_ontology:stakeholder_secondary_role(secession_legitimacy_boundary__popular_sovereignty_reading, indigenous_treaty_nations, excluded).

% The national government and constitutional order that the rule subordinates. It commands the courts, the currency, the armed forces, and the levers of international recognition, yet under the rule a bare provincial majority overrides all of it on the territorial question. Its realistic responses - clarity legislation, reference questions to the courts, refusal to negotiate - defend the existing order but cannot, within the rule's own logic, invalidate the vote itself. Its organizing horizon is the survival of the federation as constituted.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__popular_sovereignty_reading, federal_authority, payer,
    institutional, generational, constrained, national).

% Residents of the other provinces and federal territories whose country the vote would partition. The rule gives them no ballot on the disposition of shared territory, common debt, borders, or the fate of co-citizens inside the seceding province. They absorb the fiscal renegotiation, the precedent the result sets for their own regions, and the dissolution of a shared state. Their governments bargain afterward from weakness, holding recognition leverage and asset claims but no veto on the legitimacy question itself.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__popular_sovereignty_reading, rest_of_federation_residents, payer,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(secession_legitimacy_boundary__popular_sovereignty_reading, rest_of_federation_residents, excluded).

% Foreign states, international organizations, and recognition-adjudicating bodies whose acknowledgment determines whether a unilateral declaration acquires statehood in fact. They watch the rule's outputs, weigh accumulated precedents from other secession crises, and price the stability risk of endorsing plebiscitary border change. Their seat is analytical with respect to the legitimacy argument and consequential with respect to the outcome.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__popular_sovereignty_reading, international_recognition_community, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(secession_legitimacy_boundary__popular_sovereignty_reading, secessionist_party_leadership).
narrative_ontology:fixing_cost_class(secession_legitimacy_boundary__popular_sovereignty_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a determinate decision procedure for a territorial-status question the existing constitutional order leaves unanswered: one vote of the provincial electorate settles exit, converting an indefinitely deadlocked constitutional dispute into a decidable event. Within the secessionist community it coordinates strategy (win the vote), synchronizes mobilization, and supplies a shared legitimacy narrative.
% TRANSFER_FUNCTION: Transfers decision-authority over territory, hydro and mineral resources, debt apportionment, citizenship, and border control from the pan-federal constitutional order to whichever bloc commands a majority within the provincial boundary; moves legitimacy-conferral power from constitutional process and intergovernmental negotiation to a single plebiscite count. Costs concentrate on those inside the boundary who vote otherwise or hold prior jurisdictional claims the rule discounts, and on residents of the remainder state who receive no ballot at all.
% ABSENT_VOICES: Treaty-holding Indigenous nations assert prior jurisdiction but the rule defines their assent as carrying no decision-weight - they speak, and the rule renders their speech non-binding. Residents of other provinces and the federal parliament would object to partition-by-single-province-vote but sit wholly outside the enfranchised demos. Future residents - children not yet born, later migrants - inherit citizenship and border outcomes with no franchise whatsoever. The rule manufactures unanimity by defining the demos such that every dissenting seat is structurally outside it.
% DISAPPEARANCE_RATIONALE: If the rule vanished overnight, the secession question reverts to the constitutional channel - negotiated amendment or continued deadlock - and the secessionist project loses its operational path. Treaty nations lose the threat environment that drives their veto assertions, federal clarity machinery loses its antagonist, and the entire legitimacy contest reorganizes around whichever sibling reading fills the vacuum. Arrangements demonstrably depend on the rule's existence.
% FOUNDING_PROBLEM: A federated constitutional order provides no authorized path for a province to exit: constitutional texts bind provinces without granting unilateral departure, producing permanent deadlock between a mobilized territorial majority and an immovable constitutional superstructure.
% FOUNDING_PROBLEM_CORROBORATION: Seats outside the benefiting parties corroborate the founding deadlock itself: secession-reference jurisprudence affirms that no unilateral constitutional path exists, and comparative constitutional scholarship documents the same closure across most federations. What no outside seat corroborates is the reading's remedy - treaty-holder legal scholarship and federalist constitutional theory explicitly reject the inference from deadlock to provincial-majority ultimacy. Corroboration therefore covers the problem, not the proposed solution; the solution's warrant rests on the reading's own premises alone.
narrative_ontology:disappearance_verdict(secession_legitimacy_boundary__popular_sovereignty_reading, world_rearranges).
narrative_ontology:founding_problem_status(secession_legitimacy_boundary__popular_sovereignty_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(secession_legitimacy_boundary__popular_sovereignty_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(secession_legitimacy_boundary__popular_sovereignty_reading, 'none', 1).
narrative_ontology:epsilon_provenance(secession_legitimacy_boundary__popular_sovereignty_reading, 0.62, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(secession_legitimacy_boundary__popular_sovereignty_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(secession_legitimacy_boundary__popular_sovereignty_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(secession_legitimacy_boundary__popular_sovereignty_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.62 because the rule, when operative, binds non-consenting populations - treaty nations with no vote-weight on their own territories, federalist and linguistic-minority voters who lose, remainder-state residents with no ballot - to outcomes decided by a bare majority of a differently-defined demos; during dormancy the figure reflects latent capacity plus the standing discounting of treaty vetoes. Suppression is 0.55: sustaining the rule requires an active legal-political counter-apparatus (clarity statutes, court references, refusal of recognition) plus an internalized majoritarian-entitlement norm; suppression is a raw structural property and is deliberately NOT scaled by power or scope here. Theater ratio is 0.48 - high but short of inertial - because since 2000 the rule's activity has shifted from decisive votes toward commemoration, doctrine reaffirmation, and periodic polling, while retaining real reactivation capacity. Accessibility collapse is 0.58: for bound seats the alternatives narrow sharply once the rule is understood, but rule-level contestation (courts, international opinion, sibling readings) remains open. Resistance is 0.62, reflecting forty years of sustained institutional pushback. The measurement series runs on one shared time grid (1976, 1982, 1988, 1994, 2000, 2007, 2014, 2020) with every tracked metric authored at every point; the series shows one full surge-decline cycle - mobilization peaks near referendum proximity and constitutional-grievance events (1982 patriation, 1990 Meech collapse, 1995 vote, 2000 clarity confrontation), then decays. The oscillation is driven by external political events rather than functioning as an intermittent-reinforcement mechanism. base_properties are sampled at the 2020 trough-end of the cycle, so the scalar values describe a dormant-but-intact configuration, not the 1994-2000 peak.
 *
 * PERSPECTIVAL GAP:
 *   The engine should compute sharply divergent types across seats. From the agenda-setter seat the rule is simply democracy working - the community expressing its will through the only instrument available to it; from the trapped payer seat (treaty nations) the same structure is pure imposition, a vote they were never sovereign within deciding the status of their territories; from the mobile payer seats the rule prices exit (relocation, credential renegotiation) rather than eliminating choice; from the remainder-state payer seat it is externalized partition - costs imposed by an electorate that excludes them; and the analytical observer seat computes the legitimacy question as genuinely unresolved. The authored claim does not adjudicate this divergence; the structural data drives it.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive low directionality for the secessionist majority and the party leadership; victim declarations drive high directionality for the five payer seats. Exit profiles modulate within the victim set: treaty nations (trapped) sit nearest the full-target end - no arbitrage, no relocation, identity and territory fused; linguistic minorities (mobile, demonstrated out-migration) sit somewhat nearer the middle; federal authority (institutional, constrained) and remainder-state residents (organized, constrained) sit high. No directionality_overrides are authored: the override mechanism keys on power atoms, and this story's same-power seats diverge by ROLE rather than power - the organized atom hosts both the beneficiary majority and the victimized federalist voters, and the moderate atom hosts both linguistic minorities and treaty nations whose differentiation comes from exit options, not power. Per-power-atom overrides would flatten exactly the distinctions the role-plus-exit derivation already captures correctly.
 *
 * MANDATROPHY ANALYSIS:
 *   Classifying the rule as tangled_rope keeps both halves visible and prevents the two characteristic mislabelings. A pure-coordination (rope) label would erase the extraction half - the non-consenting minorities, the discounted treaty vetoes, the ballot-less remainder state - and launder majoritarian imposition as neutral decision-procedure. A pure-extraction (snare) label would erase the genuine coordination half - the rule really does convert a permanently deadlocked constitutional question into a decidable event for the community it empowers, and the founding problem it answers is corroborated as live by seats outside the benefiting parties. Mandatrophy is not resolved: founding_problem_status is live, so the arrangement has not outlived its mandate. But the mismatch watch (live status x world_rearranges verdict) correctly reports no zombie condition, while the theater trajectory (0.14 rising to 0.48) tracks the early stage of a decay path: if the coordination half continues atrophying into commemoration without reactivation, the rule drifts toward theatrical maintenance of a doctrine nobody currently exercises - at which point the classification should be revisited.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_commitment,
    'This story is one reading (popular_sovereignty_reading) of kernel secession_legitimacy_boundary; how would classification change under the sibling readings that fix a different demos or a different validity condition?',
    'Generate the three sibling stories (constitutional_impossibility, grievance_threshold, treaty_primacy) and compare per-seat classifications; the shared kernel joins them for cross-reading comparison.',
    'Under constitutional_impossibility the victim set shifts toward the secessionist majority facing legal foreclosure of its project; under treaty_primacy the victim set centers on treaty nations whose consent every other reading ignores; epsilon and computed type change per file rather than per debate.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_commitment, conceptual, 'Committer-frame indexicality: one kernel, four readings, four distinct constraints with distinct epsilon values.').

omega_variable(
    demos_definition_locus,
    'Is the provincial boundary a legitimate demos-defining unit, or an inherited administrative artifact elevated to moral salience by whoever holds it at the moment of the vote?',
    'Comparative analysis of the boundary''s genesis (colonial-era cartography versus ratified compact) and of how each reading''s alternative demos definition would redistribute decision-weight across the affected populations.',
    'If the boundary is an artifact, the rule''s extraction concentrates on populations placed inside it without consent (most sharply treaty nations), pushing computed classification toward the extractive end; if the boundary carries ratified standing, part of the measured cost is ordinary majoritarian politics rather than structural imposition.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(demos_definition_locus, conceptual, 'The located structural disagreement beneath the kernel contest: the moral status of the boundary that defines the entitled demos.').

omega_variable(
    majority_perception_register,
    'The reading holds that extraction claims are valid when the majority perceives them - is majority perception a stable, non-manipulable register, and do turnout floors and question-wording discipline it?',
    'Referendum audit data: turnout levels, independent question-clarity review, and post-vote preference-stability studies across the two exercised referendums.',
    'If perception proves manipulable (question design, timing, turnout asymmetries), the self-legitimation claim degrades toward manufactured consent and effective extraction rises; if perception is robust, the rule''s coordination half strengthens and the extractive reading narrows.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(majority_perception_register, empirical, 'Epistemic reliability of the majority-perception register on which the reading''s validity claim rests.').

omega_variable(
    suppression_internalization_split,
    'How much of the rule''s suppressive force is structural (clarity statutes, court reference doctrine, denial of treaty-veto standing) versus internalized (majoritarian-entitlement norms that survive repeated legal defeat)?',
    'Post-defeat trajectory analysis: if mobilization intensity recovers fully after each legal setback without any new structural opening, the internalized share is large; if recovery tracks concrete legal opportunity, the structural share dominates.',
    'A large internalized share raises effective suppression above the structural measure and predicts reactivation independent of legal opportunity; a structural-dominated profile makes the rule''s intensity contingent on federal countermeasures and easier to de-escalate by negotiated settlement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_internalization_split, empirical, 'Structural versus internalized composition of the rule''s suppression mechanism.').

omega_variable(
    reactivation_trajectory,
    'The rule is dormant at interval end - would a third-referendum scenario reactivate enforcement along the 1994-2000 trajectory, or have demographic and partisan change permanently lowered the ceiling?',
    'Cohort-stratified polling series on sovereignty support, party-platform tracking, and projected suppression and extractiveness paths conditioned on reactivation versus continued dormancy.',
    'Reactivation returns the constraint to its peak extractive configuration (series values near the 2000 maximum); permanent dormancy drifts it toward commemorative maintenance and eventual piton-like decay of the coordination function.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reactivation_trajectory, empirical, 'Whether the measured dormancy is a trough in a recurring cycle or a terminal decline.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(secession_legitimacy_boundary__popular_sovereignty_reading, 1976, 2020).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sece_tr_t1976, secession_legitimacy_boundary__popular_sovereignty_reading, theater_ratio, 1976, 0.14).
narrative_ontology:measurement(sece_tr_t1982, secession_legitimacy_boundary__popular_sovereignty_reading, theater_ratio, 1982, 0.2).
narrative_ontology:measurement(sece_tr_t1988, secession_legitimacy_boundary__popular_sovereignty_reading, theater_ratio, 1988, 0.24).
narrative_ontology:measurement(sece_tr_t1994, secession_legitimacy_boundary__popular_sovereignty_reading, theater_ratio, 1994, 0.3).
narrative_ontology:measurement(sece_tr_t2000, secession_legitimacy_boundary__popular_sovereignty_reading, theater_ratio, 2000, 0.37).
narrative_ontology:measurement(sece_tr_t2007, secession_legitimacy_boundary__popular_sovereignty_reading, theater_ratio, 2007, 0.43).
narrative_ontology:measurement(sece_tr_t2014, secession_legitimacy_boundary__popular_sovereignty_reading, theater_ratio, 2014, 0.46).
narrative_ontology:measurement(sece_tr_t2020, secession_legitimacy_boundary__popular_sovereignty_reading, theater_ratio, 2020, 0.48).

% Extraction over time
narrative_ontology:measurement(sece_be_t1976, secession_legitimacy_boundary__popular_sovereignty_reading, base_extractiveness, 1976, 0.44).
narrative_ontology:measurement(sece_be_t1982, secession_legitimacy_boundary__popular_sovereignty_reading, base_extractiveness, 1982, 0.5).
narrative_ontology:measurement(sece_be_t1988, secession_legitimacy_boundary__popular_sovereignty_reading, base_extractiveness, 1988, 0.55).
narrative_ontology:measurement(sece_be_t1994, secession_legitimacy_boundary__popular_sovereignty_reading, base_extractiveness, 1994, 0.63).
narrative_ontology:measurement(sece_be_t2000, secession_legitimacy_boundary__popular_sovereignty_reading, base_extractiveness, 2000, 0.67).
narrative_ontology:measurement(sece_be_t2007, secession_legitimacy_boundary__popular_sovereignty_reading, base_extractiveness, 2007, 0.66).
narrative_ontology:measurement(sece_be_t2014, secession_legitimacy_boundary__popular_sovereignty_reading, base_extractiveness, 2014, 0.64).
narrative_ontology:measurement(sece_be_t2020, secession_legitimacy_boundary__popular_sovereignty_reading, base_extractiveness, 2020, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(sece_su_t1976, secession_legitimacy_boundary__popular_sovereignty_reading, suppression_requirement, 1976, 0.38).
narrative_ontology:measurement(sece_su_t1982, secession_legitimacy_boundary__popular_sovereignty_reading, suppression_requirement, 1982, 0.45).
narrative_ontology:measurement(sece_su_t1988, secession_legitimacy_boundary__popular_sovereignty_reading, suppression_requirement, 1988, 0.52).
narrative_ontology:measurement(sece_su_t1994, secession_legitimacy_boundary__popular_sovereignty_reading, suppression_requirement, 1994, 0.61).
narrative_ontology:measurement(sece_su_t2000, secession_legitimacy_boundary__popular_sovereignty_reading, suppression_requirement, 2000, 0.71).
narrative_ontology:measurement(sece_su_t2007, secession_legitimacy_boundary__popular_sovereignty_reading, suppression_requirement, 2007, 0.65).
narrative_ontology:measurement(sece_su_t2014, secession_legitimacy_boundary__popular_sovereignty_reading, suppression_requirement, 2014, 0.59).
narrative_ontology:measurement(sece_su_t2020, secession_legitimacy_boundary__popular_sovereignty_reading, suppression_requirement, 2020, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(secession_legitimacy_boundary__popular_sovereignty_reading, identity_coordination).
narrative_ontology:affects_constraint(secession_legitimacy_boundary__popular_sovereignty_reading, secession_legitimacy_boundary__constitutional_impossibility_reading).
narrative_ontology:affects_constraint(secession_legitimacy_boundary__popular_sovereignty_reading, secession_legitimacy_boundary__grievance_threshold_reading).
narrative_ontology:affects_constraint(secession_legitimacy_boundary__popular_sovereignty_reading, secession_legitimacy_boundary__treaty_primacy_reading).

% DUAL FORMULATION NOTE:
% The colloquial concept 'secession legitimacy' decomposes into four structurally distinct constraints sharing one contested kernel; each reading fixes a different demos and a different validity condition, hence a different victim set and a different epsilon. This file authors the popular-sovereignty reading only. Upstream/downstream: the constitutional-impossibility reading currently holds the institutional upstream position (courts, clarity statutes, recognition practice), and its dominance is what sets the enforcement burden this reading must carry; this reading's plebiscitary successes in turn pressure the grievance-threshold reading's operating environment without eliminating it. Family members must be linked pairwise via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
