% ============================================================================
% CONSTRAINT STORY: shinbutsu_ontological_substrate__domain_partition_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_shinbutsu_ontological_substrate__domain_partition_reading, []).

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
 *   constraint_id: shinbutsu_ontological_substrate__domain_partition_reading
 *   human_readable: Kami-Buddha Domain Partition (Functional Coexistence Reading)
 *   domain: religious/history/commitment-systems
 *
 * SUMMARY:
 *   From roughly the ninth century to the Meiji separation of 1868, Japanese
 *   religious life operated on a division of jurisdiction: kami cults handled
 *   this-worldly affairs (rain, harvests, purification, protection) while
 *   Buddhist institutions held death, funerary care, and salvation. This
 *   story instantiates the domain-partition READING of the shinbutsu
 *   ontological-substrate kernel: the claim that coexistence was functional
 *   rather than ontological, a jurisdictional convention needing no shared
 *   metaphysics. Per the epsilon-invariance discipline, the sibling readings
 *   (ontological fusion; incoherent drift-bundle) are separate constraints in
 *   separate files; this file authors one stable epsilon for the standing
 *   arrangement as the partition reading assesses it. The claim/metric gap is
 *   deliberate and modest: the arrangement is CLAIMED as rope (genuine
 *   coordination, low coercive overhead, easy eventual separation), while the
 *   metrics record real accumulation, with extraction and suppression
 *   creeping up over ten centuries as fusion-era institutions layered
 *   registration compulsion onto the partition. The engine measures the
 *   divergence; nothing here reconciles claim to metrics.
 *
 * KEY AGENTS:
 *   - imperial_court: Agenda-setter (institutional/arbitrage) — administers the dual bureaucracy, adjudicates shrine-temple disputes, collects legitimation from both channels
 *   - shrine_priesthoods: Beneficiary (organized/constrained) — protected jurisdiction over this-worldly rites; hereditary office and shrine land anchor them
 *   - buddhist_establishments: Beneficiary (institutional/constrained) — reserved sphere over deathcare and salvation; landed and, late in the interval, registration-backed
 *   - lay_practitioners: Beneficiary/payer (moderate/constrained) — route needs by domain, pay both sides, receive complementary services
 *   - unaffiliated_folk_practitioners: Excluded (powerless/trapped) — fall outside both licensed channels; periodic suppression
 *   - religious_historians: Analytical observer — reconstruct the arrangement from archives
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(shinbutsu_ontological_substrate__domain_partition_reading, 0.34).
domain_priors:suppression_score(shinbutsu_ontological_substrate__domain_partition_reading, 0.33).
domain_priors:theater_ratio(shinbutsu_ontological_substrate__domain_partition_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(shinbutsu_ontological_substrate__domain_partition_reading, extractiveness, 0.34).
narrative_ontology:constraint_metric(shinbutsu_ontological_substrate__domain_partition_reading, suppression_requirement, 0.33).
narrative_ontology:constraint_metric(shinbutsu_ontological_substrate__domain_partition_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(shinbutsu_ontological_substrate__domain_partition_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(shinbutsu_ontological_substrate__domain_partition_reading, resistance, 0.25).

% --- Constraint claim ---
narrative_ontology:constraint_claim(shinbutsu_ontological_substrate__domain_partition_reading, rope).
narrative_ontology:human_readable(shinbutsu_ontological_substrate__domain_partition_reading, "Kami-Buddha Domain Partition (Functional Coexistence Reading)").
narrative_ontology:topic_domain(shinbutsu_ontological_substrate__domain_partition_reading, "religious/history/commitment-systems").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(shinbutsu_ontological_substrate__domain_partition_reading, '10c3c21c-c880-43be-bdfc-1cf3248c1b17').
narrative_ontology:cs_kernel_codification('10c3c21c-c880-43be-bdfc-1cf3248c1b17', implicit).
narrative_ontology:cs_authority_grounding('10c3c21c-c880-43be-bdfc-1cf3248c1b17', practice).
narrative_ontology:cs_interpretation_layer_present('10c3c21c-c880-43be-bdfc-1cf3248c1b17').
narrative_ontology:cs_reading_relation('10c3c21c-c880-43be-bdfc-1cf3248c1b17', shinbutsu_ontological_substrate__syncretic_fusion_reading, forecloses).
narrative_ontology:cs_reading_relation('10c3c21c-c880-43be-bdfc-1cf3248c1b17', shinbutsu_ontological_substrate__incoherent_bundle_reading, coexists_with).
narrative_ontology:cs_axiom('10c3c21c-c880-43be-bdfc-1cf3248c1b17', foundational, kami_buddha_coexistence_is_functional_not_ontological).
narrative_ontology:cs_axiom_status(kami_buddha_coexistence_is_functional_not_ontological, holdable).
narrative_ontology:cs_axiom_grounding('10c3c21c-c880-43be-bdfc-1cf3248c1b17', kami_buddha_coexistence_is_functional_not_ontological, empirically_contingent).
narrative_ontology:cs_axiom('10c3c21c-c880-43be-bdfc-1cf3248c1b17', secondary, jurisdictional_boundaries_need_no_metaphysical_ground).
narrative_ontology:cs_axiom_status(jurisdictional_boundaries_need_no_metaphysical_ground, holdable).
narrative_ontology:cs_axiom_grounding('10c3c21c-c880-43be-bdfc-1cf3248c1b17', jurisdictional_boundaries_need_no_metaphysical_ground, conventional).
narrative_ontology:cs_reference_frame('10c3c21c-c880-43be-bdfc-1cf3248c1b17', functional_domain_partition_order).
narrative_ontology:cs_drift_state('10c3c21c-c880-43be-bdfc-1cf3248c1b17', late_edo_pre_separation, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('10c3c21c-c880-43be-bdfc-1cf3248c1b17', '').
narrative_ontology:cs_kernel_id(shinbutsu_ontological_substrate__domain_partition_reading, shinbutsu_ontological_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(shinbutsu_ontological_substrate__domain_partition_reading, shrine_priesthoods).
narrative_ontology:constraint_beneficiary(shinbutsu_ontological_substrate__domain_partition_reading, buddhist_establishments).
narrative_ontology:constraint_beneficiary(shinbutsu_ontological_substrate__domain_partition_reading, imperial_court).
narrative_ontology:constraint_beneficiary(shinbutsu_ontological_substrate__domain_partition_reading, lay_practitioners).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(shinbutsu_ontological_substrate__domain_partition_reading, lay_practitioners).
narrative_ontology:constraint_vindicates(shinbutsu_ontological_substrate__domain_partition_reading, functional_jurisdictional_complementarity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintains parallel bureaus for kami worship and for the Buddhist clergy, ranks shrine lineages alongside monastic institutions, and adjudicates jurisdictional disputes between shrines and temples when petitioned. Draws legitimacy from patronizing both channels and from the order the division preserves. Later warrior governments inherit the adjudication role; a successor state ultimately dissolves the whole arrangement by decree in 1868.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_substrate__domain_partition_reading, imperial_court, agenda_setter,
    institutional, generational, arbitrage, national).

% Hereditary custodian lineages of kami cults. Conduct purification, harvest, and festival rites and hold recognized jurisdiction over this-worldly requests: rain, health, protection, settlement boundaries. Receive offerings, land income, and court rank. The partition protects their sphere from takeover of this-worldly rites by the temples; leaving would mean abandoning hereditary office, shrine land, and the cult community that sustains the lineage.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_substrate__domain_partition_reading, shrine_priesthoods, beneficiary,
    organized, generational, constrained, regional).

% Monastic complexes holding doctrinal authority over death, salvation, and the afterlife. Perform funerals, memorial services, and sutra offices; receive land grants, dues, and, after the seventeenth century, compulsory household registration fees. The partition reserves deathcare for them and shields their parishes from rival salvific providers. Exit would mean liquidating landed estates and disbanding ordination lineages.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_substrate__domain_partition_reading, buddhist_establishments, beneficiary,
    institutional, generational, constrained, national).

% Village and town households. Bring this-worldly requests to shrines and death-related needs to temples, following the partition's routing. Pay offerings, festival levies, temple dues, and later mandatory registration fees to both sides, while receiving complementary services from each. They can shift emphasis between kami and buddha practice but cannot opt out of the establishment system as a whole; village contracts bind whole communities to specific temples and shrines.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_substrate__domain_partition_reading, lay_practitioners, beneficiary,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(shinbutsu_ontological_substrate__domain_partition_reading, lay_practitioners, payer).

% Itinerant ascetics, mediums, diviners, and healers whose practices fall outside both licensed domains. They serve clients the establishments underserve but hold no recognized standing, no land, and no court rank; periodic prohibition campaigns target unauthorized preaching and unsanctioned rites. The partition defines the two legitimate channels and leaves them outside both; they would contest the arrangement's claim to exhaust the space of religious practice.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_substrate__domain_partition_reading, unaffiliated_folk_practitioners, excluded,
    powerless, biographical, trapped, regional).

% Reconstruct the arrangement from court decrees, dispute records, liturgical calendars, and registration rolls; assess whether coexistence operated as functional division, ontological fusion, or ungoverned accretion. They take no part in the arrangement's operation and can compare it with other plural-religious-order settlements.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_substrate__domain_partition_reading, religious_historians, observer,
    analytical, civilizational, analytical, global).

narrative_ontology:fixing_cost_class(shinbutsu_ontological_substrate__domain_partition_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Allocates religious labor between two establishment systems so that this-worldly needs (rain, harvest, purification, protection) route to kami cults and death-related needs (funerals, memorials, salvation) route to Buddhist institutions; adjudicates the boundary so rival cult centers do not fight zero-sum wars over the same patrons, and practitioners get one legible map of where to bring which problem.
% TRANSFER_FUNCTION: Moves offerings, festival levies, land income, and, after the seventeenth century, compulsory registration fees from lay households to the shrine and temple establishments; moves ritual services (rites, funerals, festivals) back to households; moves legitimation to the court and warrior governments from both channels' endorsement.
% ABSENT_VOICES: Unaffiliated folk specialists (mediums, itinerant ascetics, diviners) had no seat in the arrangement that defined the only two legitimate channels; women subject to exclusion zones on sacred mountains likewise appear in the record mainly as objects of regulation. Both would object that the partition exhausted religious possibility far short of where religious life actually ended.
% DISAPPEARANCE_RATIONALE: The 1868 separation edicts supply the natural experiment: ordered to untwine kami and buddhas, the country dismantled thousands of temples, laicized tens of thousands of clerics, stripped Buddhist imagery from shrines, and rebuilt kami worship as a state cult on the vacated ground. The religious landscape demonstrably depended on the partitioned arrangement; removing it did not leave the world unchanged, it forcibly re-plumbed it.
% FOUNDING_PROBLEM: An immigrant salvation religion with textual authority and a landed church met an indigenous cult system bound to the imperial house and the agrarian calendar, both competing for the same elite patronage and popular devotion; the arrangement was built to allocate spheres of competence so both could be patronized without annihilating each other.
% FOUNDING_PROBLEM_CORROBORATION: Court dispute records and shogunal judgments attest that the jurisdictional conflicts were real and recurrent, an attestation from administrative archives rather than from the benefiting establishments. Modern historiography splits: some scholars corroborate a live allocation problem that the partition managed for a millennium; others argue the conflicts were episodic, that simpler licensing could have handled them, and that the arrangement's persistence owed more to inertia than to a live problem. No outside consensus exists; the status is genuinely disputed.
narrative_ontology:disappearance_verdict(shinbutsu_ontological_substrate__domain_partition_reading, world_rearranges).
narrative_ontology:founding_problem_status(shinbutsu_ontological_substrate__domain_partition_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(shinbutsu_ontological_substrate__domain_partition_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(shinbutsu_ontological_substrate__domain_partition_reading, 'none', 1).
narrative_ontology:epsilon_provenance(shinbutsu_ontological_substrate__domain_partition_reading, 0.34, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(shinbutsu_ontological_substrate__domain_partition_reading_tests).
:- end_tests(shinbutsu_ontological_substrate__domain_partition_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is authored low-moderate (0.34 at interval end) because the partition delivers real services on both sides of the boundary; the rise from 0.13 tracks fusion-era institutional layering (temple supremacy claims, shrine-temple complexes, and finally compulsory parish registration) rather than extraction intrinsic to the partition itself. Suppression (0.33 end-state) is dominated by the late Edo registration regime, which made Buddhist affiliation compulsory for every household; before the seventeenth century the arrangement ran mostly on custom and episodic court mediation, which is why the suppression series stays low until roughly 1650 and then climbs. Theater stays low throughout (0.05 to 0.22): the rites performed were the functions performed. Accessibility collapse is moderate (0.40): folk practice, independent kami cults, and non-establishment deathways remained partly workable, so understanding the partition did not foreclose alternatives. Resistance is low-moderate (0.25): jurisdictional quarrels and late Tokugawa nativist critique, but no sustained opposition for most of the interval, because most seats were net beneficiaries. All three series share one seven-point grid (900 to 1868) so no metric is sampled against another's gaps; every point is observed (reconstructed from decrees, dispute records, and registration rolls), none projected.
 *
 * PERSPECTIVAL GAP:
 *   Seats diverge sharply. From the court's chair the arrangement is order-making administration it built and could restructure, and finally did. From the two establishments' chairs it is a protected market: each holds a reserved sphere the other may not enter. From the lay household's chair it is legible routing with a double bill, genuine service paired with cumulative dues. From the excluded folk specialist's chair, the same boundary that coordinates everyone else is the wall that keeps them outside both legitimate channels. The engine computes these per-seat classifications from the structural data; the authored rope claim adjudicates none of them.
 *
 * DIRECTIONALITY LOGIC:
 *   All four seated parties are declared beneficiaries, so derived directionality sits toward the subsidized end for each, which is correct for the establishments and the court and approximately right for the priesthoods. Lay practitioners are the deliberate complication: declared beneficiary (they receive the routing benefit) with a payer secondary role (they fund both sides), their true position is near-symmetric rather than subsidized; the secondary role is authored so the derivation reads the dual position rather than the headline role. The excluded folk practitioners carry no beneficiary declaration and no victim declaration, because exclusion is not extraction, but they bear the boundary's costs and would derive high directionality if seated; they are authored as excluded precisely so their absence informs the consensus-provenance check rather than the classification arithmetic. No directionality overrides are used: the beneficiary declarations plus exit atoms produce the right shape without correction.
 *
 * MANDATROPHY ANALYSIS:
 *   The rope claim guards against two misreadings. Read as a snare, the arrangement's late coercion (registration compulsion) would condemn a millennium of functional coexistence for the sins of its final century; the attribution omega separates the registration layer from the partition proper so the engine can price them apart. Read as a piton, the arrangement's long persistence would look like inertial theater; but the founding problem, allocating competence between rival systems, stayed live throughout, and the Meiji dissolution rearranged the world violently, which is the signature of a functioning constraint removed by force, not a dead one maintained by habit. Mandatrophy is therefore not resolved: the mandate was executing when the executioners arrived.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_underdetermination,
    'Does the domain-partition account capture the operative kernel of kami-buddha coexistence, or is the substrate better described by the fusion reading (ontological unity) or the bundle reading (no kernel, enforced drift)?',
    'Compare practitioner routing behavior (liturgical calendars, petition records, parish contracts) against doctrinal texts (honji suijaku treatises): if practice partitions while doctrine fuses, the functional reading captures the operative kernel; if practice tracks the fusion hierarchy, it does not.',
    'If the fusion reading is correct, institutional entanglement and extraction are far higher and this arrangement computes as a hybrid coordination/extraction structure; if the bundle reading is correct, the coordination function asserted here is illusory and the arrangement is better modeled as enforced drift with no coordination component.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_underdetermination, conceptual, 'Which reading of the shinbutsu substrate kernel this arrangement actually instantiates.').

omega_variable(
    layered_registration_attribution,
    'How much of the late-interval extraction belongs to the domain partition itself versus the layered parish-registration system (danka/terauke), which may be a distinct constraint riding on the partition?',
    'Decompose establishment revenue streams: voluntary offerings and service fees (partition-native) versus compulsory registration fees (registration-layer); compare periods before and after the 1630s registration mandates.',
    'If most measured extraction is the registration layer, the partition proper sits near pure coordination (extractiveness around 0.15) and the rope claim strengthens; if the extraction is intrinsic to the partition, the claim weakens toward a hybrid coordination/extraction structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(layered_registration_attribution, empirical, 'Attribution of measured extraction between the partition and the overlaid registration regime.').

omega_variable(
    separation_counterfactual_validity,
    'Does the post-1868 rearrangement evidence the world''s dependence on the partition, or only the violence of the specific way it was removed (anti-Buddhist animus driving the temple destructions)?',
    'Compare the Japanese separation with gentler plural-order transitions elsewhere; model whether a neutral, uncompensated dissolution would have produced comparable rearrangement absent the ideological purge.',
    'If the rearrangement was mostly purge-driven, the disappearance verdict overstates structural dependence and the arrangement was closer to removable-by-decree than the verdict implies; if rearrangement follows even under neutral removal, dependence is real.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(separation_counterfactual_validity, empirical, 'Validity of the Meiji separation as a natural experiment for the disappearance question.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(shinbutsu_ontological_substrate__domain_partition_reading, 900, 1868).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(shinbutsu_partition_tr_t900, shinbutsu_ontological_substrate__domain_partition_reading, theater_ratio, 900, 0.05).
narrative_ontology:measurement_basis(shinbutsu_partition_tr_t900, observed).
narrative_ontology:measurement(shinbutsu_partition_tr_t1100, shinbutsu_ontological_substrate__domain_partition_reading, theater_ratio, 1100, 0.07).
narrative_ontology:measurement_basis(shinbutsu_partition_tr_t1100, observed).
narrative_ontology:measurement(shinbutsu_partition_tr_t1300, shinbutsu_ontological_substrate__domain_partition_reading, theater_ratio, 1300, 0.1).
narrative_ontology:measurement_basis(shinbutsu_partition_tr_t1300, observed).
narrative_ontology:measurement(shinbutsu_partition_tr_t1500, shinbutsu_ontological_substrate__domain_partition_reading, theater_ratio, 1500, 0.13).
narrative_ontology:measurement_basis(shinbutsu_partition_tr_t1500, observed).
narrative_ontology:measurement(shinbutsu_partition_tr_t1650, shinbutsu_ontological_substrate__domain_partition_reading, theater_ratio, 1650, 0.16).
narrative_ontology:measurement_basis(shinbutsu_partition_tr_t1650, observed).
narrative_ontology:measurement(shinbutsu_partition_tr_t1750, shinbutsu_ontological_substrate__domain_partition_reading, theater_ratio, 1750, 0.19).
narrative_ontology:measurement_basis(shinbutsu_partition_tr_t1750, observed).
narrative_ontology:measurement(shinbutsu_partition_tr_t1868, shinbutsu_ontological_substrate__domain_partition_reading, theater_ratio, 1868, 0.22).
narrative_ontology:measurement_basis(shinbutsu_partition_tr_t1868, observed).

% Extraction over time
narrative_ontology:measurement(shinbutsu_partition_be_t900, shinbutsu_ontological_substrate__domain_partition_reading, base_extractiveness, 900, 0.13).
narrative_ontology:measurement_basis(shinbutsu_partition_be_t900, observed).
narrative_ontology:measurement(shinbutsu_partition_be_t1100, shinbutsu_ontological_substrate__domain_partition_reading, base_extractiveness, 1100, 0.16).
narrative_ontology:measurement_basis(shinbutsu_partition_be_t1100, observed).
narrative_ontology:measurement(shinbutsu_partition_be_t1300, shinbutsu_ontological_substrate__domain_partition_reading, base_extractiveness, 1300, 0.21).
narrative_ontology:measurement_basis(shinbutsu_partition_be_t1300, observed).
narrative_ontology:measurement(shinbutsu_partition_be_t1500, shinbutsu_ontological_substrate__domain_partition_reading, base_extractiveness, 1500, 0.26).
narrative_ontology:measurement_basis(shinbutsu_partition_be_t1500, observed).
narrative_ontology:measurement(shinbutsu_partition_be_t1650, shinbutsu_ontological_substrate__domain_partition_reading, base_extractiveness, 1650, 0.29).
narrative_ontology:measurement_basis(shinbutsu_partition_be_t1650, observed).
narrative_ontology:measurement(shinbutsu_partition_be_t1750, shinbutsu_ontological_substrate__domain_partition_reading, base_extractiveness, 1750, 0.31).
narrative_ontology:measurement_basis(shinbutsu_partition_be_t1750, observed).
narrative_ontology:measurement(shinbutsu_partition_be_t1868, shinbutsu_ontological_substrate__domain_partition_reading, base_extractiveness, 1868, 0.34).
narrative_ontology:measurement_basis(shinbutsu_partition_be_t1868, observed).

% Suppression requirement over time
narrative_ontology:measurement(shinbutsu_partition_su_t900, shinbutsu_ontological_substrate__domain_partition_reading, suppression_requirement, 900, 0.08).
narrative_ontology:measurement_basis(shinbutsu_partition_su_t900, observed).
narrative_ontology:measurement(shinbutsu_partition_su_t1100, shinbutsu_ontological_substrate__domain_partition_reading, suppression_requirement, 1100, 0.1).
narrative_ontology:measurement_basis(shinbutsu_partition_su_t1100, observed).
narrative_ontology:measurement(shinbutsu_partition_su_t1300, shinbutsu_ontological_substrate__domain_partition_reading, suppression_requirement, 1300, 0.13).
narrative_ontology:measurement_basis(shinbutsu_partition_su_t1300, observed).
narrative_ontology:measurement(shinbutsu_partition_su_t1500, shinbutsu_ontological_substrate__domain_partition_reading, suppression_requirement, 1500, 0.15).
narrative_ontology:measurement_basis(shinbutsu_partition_su_t1500, observed).
narrative_ontology:measurement(shinbutsu_partition_su_t1650, shinbutsu_ontological_substrate__domain_partition_reading, suppression_requirement, 1650, 0.22).
narrative_ontology:measurement_basis(shinbutsu_partition_su_t1650, observed).
narrative_ontology:measurement(shinbutsu_partition_su_t1750, shinbutsu_ontological_substrate__domain_partition_reading, suppression_requirement, 1750, 0.28).
narrative_ontology:measurement_basis(shinbutsu_partition_su_t1750, observed).
narrative_ontology:measurement(shinbutsu_partition_su_t1868, shinbutsu_ontological_substrate__domain_partition_reading, suppression_requirement, 1868, 0.33).
narrative_ontology:measurement_basis(shinbutsu_partition_su_t1868, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(shinbutsu_ontological_substrate__domain_partition_reading, resource_allocation).
narrative_ontology:affects_constraint(shinbutsu_ontological_substrate__domain_partition_reading, shinbutsu_ontological_substrate__syncretic_fusion_reading).
narrative_ontology:affects_constraint(shinbutsu_ontological_substrate__domain_partition_reading, shinbutsu_ontological_substrate__incoherent_bundle_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'shinbutsu shugo' (kami-buddha syncretism) covers three structurally distinct claims about one kernel, decomposed per the epsilon-invariance principle. This file instantiates the domain-partition reading (functional jurisdictional convention; low extraction, easy separation). The syncretic-fusion sibling instantiates the ontological-unity claim (higher entanglement, higher extraction, temple-over-shrine hierarchy), and the incoherent-bundle sibling denies any kernel at all (enforced drift, no coordination function). The upstream/downstream structure runs from this reading to the fusion sibling historically: fusion doctrine was cited as the deeper truth beneath the working partition, so the partition's apparent success lent credibility to the fusion claim. Each file carries its own epsilon, beneficiaries, and classification; the epsilons differ because the referent arrangements differ, not because one constraint is measured two ways.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
