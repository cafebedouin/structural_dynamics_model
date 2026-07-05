% ============================================================================
% CONSTRAINT STORY: reformation_composite__technological_mediation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_reformation_composite__technological_mediation_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: reformation_composite__technological_mediation_reading
 *   human_readable: Printing Press as Structural Amplifier of the Reformation
 *   domain: historical_epistemology/religious_history/political_economy
 *
 * SUMMARY:
 *   This story instantiates the technological-mediation reading of the
 *   Reformation kernel: the printing press as the load-bearing physical
 *   constraint that converted a recurring pattern of local theological
 *   dissent (Wycliffe, Hus, and others before it) into a continental,
 *   unsuppressible mass movement. The physical fact of movable-type
 *   reproduction — cheap, fast, geographically distributed, resistant to
 *   point-source suppression — is treated as the enabling substrate beneath
 *   the theological content and the political opportunism that rode on it.
 *   This is NOT a claim that theology or politics were epiphenomenal; it is a
 *   claim that the SPEED, SCALE, and SUPPRESSION-RESISTANCE of the movement
 *   are structurally attributable to print infrastructure, and that this
 *   attribution is independently measurable via publication counts, edition
 *   sizes, and literacy correlations, distinct from the doctrinal-content
 *   reading and the sovereignty-assertion reading.
 *
 * KEY AGENTS:
 *   - printer_publishers: beneficiary, sell controversy regardless of doctrinal side
 *   - reformist_clergy: beneficiary, gain unprecedented reach via reproducible text
 *   - literate_urban_burghers: beneficiary, gain direct scriptural access bypassing clerical mediation
 *   - illiterate_rural_populations: payer, bear costs of confessional war without participating in the print debate
 *   - traditional_scriptoria_and_manuscript_copyists: payer, economically obsoleted within a generation
 *   - excommunicated_dissenters_outside_print_networks: excluded, theological positions filtered out by lack of print-capital access, not doctrinal rejection
 *   - papal_and_imperial_censors: agenda_setter, attempt suppression but are structurally outmatched by decentralized press ownership
 *   - historians_of_print_culture: observer, reconstruct causal weight via publication/literacy data independent of participant framing
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(reformation_composite__technological_mediation_reading, 0.38).
domain_priors:suppression_score(reformation_composite__technological_mediation_reading, 0.42).
domain_priors:theater_ratio(reformation_composite__technological_mediation_reading, 0.12).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(reformation_composite__technological_mediation_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(reformation_composite__technological_mediation_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(reformation_composite__technological_mediation_reading, theater_ratio, 0.12).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(reformation_composite__technological_mediation_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(reformation_composite__technological_mediation_reading, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(reformation_composite__technological_mediation_reading, tangled_rope).
narrative_ontology:human_readable(reformation_composite__technological_mediation_reading, "Printing Press as Structural Amplifier of the Reformation").
narrative_ontology:topic_domain(reformation_composite__technological_mediation_reading, "historical_epistemology/religious_history/political_economy").

domain_priors:requires_active_enforcement(reformation_composite__technological_mediation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(reformation_composite__technological_mediation_reading, '25f44961-13f9-4d3a-b8e4-f8c5c88c2c0e').
narrative_ontology:cs_kernel_codification('25f44961-13f9-4d3a-b8e4-f8c5c88c2c0e', distributed).
narrative_ontology:cs_authority_grounding('25f44961-13f9-4d3a-b8e4-f8c5c88c2c0e', distributed).
narrative_ontology:cs_reading_relation('25f44961-13f9-4d3a-b8e4-f8c5c88c2c0e', reformation_composite__theological_fragmentation_reading, influences).
narrative_ontology:cs_reading_relation('25f44961-13f9-4d3a-b8e4-f8c5c88c2c0e', reformation_composite__political_realignment_reading, influences).
narrative_ontology:cs_axiom('25f44961-13f9-4d3a-b8e4-f8c5c88c2c0e', foundational, reproduction_technology_is_the_generative_constraint).
narrative_ontology:cs_axiom_status(reproduction_technology_is_the_generative_constraint, holdable).
narrative_ontology:cs_axiom_grounding('25f44961-13f9-4d3a-b8e4-f8c5c88c2c0e', reproduction_technology_is_the_generative_constraint, empirically_contingent).
narrative_ontology:cs_axiom('25f44961-13f9-4d3a-b8e4-f8c5c88c2c0e', secondary, doctrinal_content_is_downstream_variable_content).
narrative_ontology:cs_axiom_status(doctrinal_content_is_downstream_variable_content, holdable).
narrative_ontology:cs_axiom_grounding('25f44961-13f9-4d3a-b8e4-f8c5c88c2c0e', doctrinal_content_is_downstream_variable_content, empirically_contingent).
narrative_ontology:cs_reference_frame('25f44961-13f9-4d3a-b8e4-f8c5c88c2c0e', print_scale_reproduction_as_load_bearing_infrastructure).
narrative_ontology:cs_drift_state('25f44961-13f9-4d3a-b8e4-f8c5c88c2c0e', post_eisenstein_book_history_era, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('25f44961-13f9-4d3a-b8e4-f8c5c88c2c0e', '').
narrative_ontology:cs_kernel_id(reformation_composite__technological_mediation_reading, reformation_composite).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(reformation_composite__technological_mediation_reading, printer_publishers).
narrative_ontology:constraint_beneficiary(reformation_composite__technological_mediation_reading, reformist_clergy).
narrative_ontology:constraint_beneficiary(reformation_composite__technological_mediation_reading, literate_urban_burghers).
narrative_ontology:constraint_victim(reformation_composite__technological_mediation_reading, illiterate_rural_populations).
narrative_ontology:constraint_victim(reformation_composite__technological_mediation_reading, traditional_scriptoria_and_manuscript_copyists).
narrative_ontology:constraint_victim(reformation_composite__technological_mediation_reading, excommunicated_dissenters_outside_print_networks).
narrative_ontology:constraint_vindicates(reformation_composite__technological_mediation_reading, print_capitalism_drives_mass_ideological_movements).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Print shops in cities like Wittenberg, Basel, and Strasbourg mass-produce pamphlets, translated scripture, and polemical tracts at unprecedented speed and low unit cost. They select which texts to run based on market demand for controversy, profiting directly from theological conflict regardless of doctrinal outcome. Their mobility across jurisdictions lets them relocate presses when local authorities crack down.
narrative_ontology:constraint_stakeholder(reformation_composite__technological_mediation_reading, printer_publishers, beneficiary,
    organized, biographical, mobile, continental).
narrative_ontology:stakeholder_secondary_role(reformation_composite__technological_mediation_reading, printer_publishers, agenda_setter).

% Figures like Luther gain the ability to broadcast dissent across hundreds of miles within weeks rather than remaining a local pulpit controversy. The press converts their arguments into standardized, repeatable objects that travel without them, amplifying influence far beyond what oral preaching or hand-copied manuscript could achieve. Their exit from the movement they helped ignite is constrained once their name is attached to circulating print.
narrative_ontology:constraint_stakeholder(reformation_composite__technological_mediation_reading, reformist_clergy, beneficiary,
    moderate, generational, constrained, continental).

% Merchants, guild members, and educated laity in printing-hub cities gain direct access to vernacular scripture and theological argument without clerical mediation. They can read competing positions, form independent judgments, and participate in a new public sphere of pamphlet debate that bypasses the traditional teaching authority of the parish.
narrative_ontology:constraint_stakeholder(reformation_composite__technological_mediation_reading, literate_urban_burghers, beneficiary,
    moderate, biographical, mobile, regional).

% The majority of the population, unable to read, experience the print-driven upheaval only through secondhand sermons, rumor, and eventually war and confessional violence imposed on their territories by decisions made in the pamphlet-literate cities. They bear the costs of confessional conflict (war, displacement, tithe reallocation) without having participated in the print-mediated debate that produced it.
narrative_ontology:constraint_stakeholder(reformation_composite__technological_mediation_reading, illiterate_rural_populations, payer,
    powerless, biographical, trapped, regional).

% Monastic and guild-based manuscript production, previously the sole channel for textual reproduction and a source of livelihood and institutional prestige, is rendered economically obsolete within a generation. Their labor and accumulated expertise have no exit path into the new print economy, which requires different capital and skills entirely.
narrative_ontology:constraint_stakeholder(reformation_composite__technological_mediation_reading, traditional_scriptoria_and_manuscript_copyists, payer,
    powerless, biographical, trapped, local).

% Dissenting voices without access to printing capital or urban distribution networks (radical Anabaptist communities, peasant theologians, women excluded from the printer's guild economy) generate theological arguments that never achieve continental circulation. Their positions are structurally filtered out not by doctrinal rejection but by lack of access to the technological amplification that made Luther's or Calvin's positions durable and mobile.
narrative_ontology:constraint_stakeholder(reformation_composite__technological_mediation_reading, excommunicated_dissenters_outside_print_networks, excluded,
    powerless, biographical, trapped, local).

% Ecclesiastical and imperial authorities attempt to control the new technology through licensing, indices of prohibited books, and press seizures, but discover that decentralized, mobile printing capital across dozens of independent jurisdictions cannot be suppressed the way a single scriptorium or pulpit could be. Their enforcement apparatus is real but structurally outmatched by the distributed nature of the printing infrastructure.
narrative_ontology:constraint_stakeholder(reformation_composite__technological_mediation_reading, papal_and_imperial_censors, agenda_setter,
    institutional, generational, constrained, continental).

% Later scholarship (Eisenstein, Febvre and Martin) reconstructs the causal weight of print technology relative to theological and political factors, using publication counts, edition sizes, and literacy data as evidence independent of the participants' own framing of events.
narrative_ontology:constraint_stakeholder(reformation_composite__technological_mediation_reading, historians_of_print_culture, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Standardizes and mass-replicates theological argument so that dispersed reformist communities across the continent can coordinate around a shared, stable text rather than relying on itinerant preachers or hand-copied manuscripts subject to transcription drift — solving a genuine information-coordination problem for a movement that needed doctrinal consistency across distance.
% TRANSFER_FUNCTION: Moves influence and doctrinal authority from oral, geographically local clerical structures (parish priest, manuscript-copying monastery) to print-capital-holding cities and the individuals whose names circulate in cheap reproducible form; moves the material costs of the resulting confessional conflict onto rural and print-excluded populations who had no voice in the print-mediated debate.
% ABSENT_VOICES: Illiterate rural populations, radical dissenters outside urban print networks, and displaced manuscript copyists would object that the 'continental mass movement' was assembled by and for a literate, urban, capital-adjacent minority — but they are not present in the pamphlet debate the historical record foregrounds, and their objections survive mainly as suppressed movements (Peasants' War, radical Anabaptism) rather than circulated texts.
% DISAPPEARANCE_RATIONALE: Without movable-type printing, Luther's 1517 theses remain a local Wittenberg academic dispute of the kind that had occurred repeatedly for centuries without triggering continental fragmentation; the theological content alone (indulgences, justification by faith) had precedent in earlier dissent (Wycliffe, Hus) that was suppressed precisely because it lacked a mass-reproduction vector. Remove the press and the movement's speed, geographic scope, and resistance to suppression collapse to pre-print baseline rates.
% FOUNDING_PROBLEM: Pre-print theological dissent could be geographically contained and suppressed by controlling a small number of pulpits, scriptoria, and manuscript copies; the press was adopted first as a commercial and administrative tool (indulgence forms, Bibles, classical texts) and only secondarily became the infrastructure for uncontainable doctrinal replication.
% FOUNDING_PROBLEM_CORROBORATION: Book-historians (Febvre and Martin, Eisenstein) working from print-shop ledgers, edition counts, and surviving inventories external to any confessional tradition corroborate that print volume and geographic distribution networks, not doctrinal novelty alone, explain the speed and irreversibility of Reformation-era religious fragmentation; this reading is independently attested by economic historians of the print trade who have no stake in either Protestant or Catholic confessional narratives.
narrative_ontology:disappearance_verdict(reformation_composite__technological_mediation_reading, world_rearranges).
narrative_ontology:founding_problem_status(reformation_composite__technological_mediation_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(reformation_composite__technological_mediation_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(reformation_composite__technological_mediation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(reformation_composite__technological_mediation_reading, 0.38, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(reformation_composite__technological_mediation_reading_tests).
:- end_tests(reformation_composite__technological_mediation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises from near-zero (0.08 at 1450, when print is a commercial curiosity) to 0.38 by 1560 and then plateaus — the ramp tracks the period when print-capital ownership became a durable structural advantage (who owns presses shapes whose theology travels) rather than a neutral tool. Suppression climbs sharply between 1490 and 1530 (0.15 to 0.40) as ecclesiastical and imperial authorities scramble to build licensing and prohibition machinery against a technology whose decentralization made it structurally resistant to the same containment strategies that worked against Wycliffe and Hus. Theater ratio stays low throughout (peaking at 0.12) because censorship and print production were both substantively functional activities, not performative ones — the indices of prohibited books were genuinely enforced where enforcement was physically possible, they simply could not scale to cover distributed press ownership across dozens of independent polities.
 *
 * DIRECTIONALITY LOGIC:
 *   Printer-publishers sit closest to the beneficiary end: they profit from theological conflict as a commercial input regardless of doctrinal outcome, and their mobility across jurisdictions gives them arbitrage-like exit from local suppression attempts. Reformist clergy benefit structurally (amplified reach) but carry constrained exit once their name is fused to circulating print — they cannot un-print what has spread. Illiterate rural populations and manuscript copyists are structural targets: the first bear the downstream costs of confessional war they did not debate into being; the second are rendered obsolete by a capital shift they cannot participate in. Excluded dissenters occupy a distinct position from victims — they are not charged a cost by the print economy so much as filtered out of the historical record by lacking access to its amplification, a structurally different harm (erasure rather than extraction) that the framework captures via the excluded role rather than victim.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope classification (rather than pure mountain or pure rope) is deliberate: the press IS a genuine coordination technology — it solved the real problem of doctrinal drift across distance that plagued earlier, print-less heterodox movements — but its benefits accrued disproportionately to print-capital owners and urban literate populations while its costs (confessional war, obsoleted crafts, filtered-out dissent) fell on populations who never participated in the coordination it enabled. Calling this a pure mountain (as if print technology were merely a neutral physical fact with zero degrees of freedom) would erase the asymmetric distribution of who could own a press and whose theology therefore became continental. Calling it a pure snare would erase the genuine information-coordination function that let reform communities maintain doctrinal consistency across hundreds of miles — a real problem previous heterodox movements failed to solve. The composite classification preserves both facts.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    print_causal_weight_vs_content_and_politics,
    'Is the printing press best modeled as an independently sufficient causal mechanism (the Reformation happens in some form wherever print-scale reproduction exists), or as a necessary-but-not-sufficient enabling condition whose effects are inseparable from the specific theological content and political opportunism of this period?',
    'Comparative historical analysis: does theological dissent with comparable print-access in other regions/periods (e.g. earlier Chinese woodblock printing and religious heterodoxy, or print-era Ottoman religious dissent) produce comparably uncontainable continental fragmentation? If print-scale reproduction correlates with mass religious fragmentation independent of specific doctrinal content across multiple cases, the mountain-like independent-sufficiency reading strengthens.',
    'If print access alone is not sufficient (requires specific doctrinal and political conditions to co-occur), this reading''s claim to structural priority over the theological_fragmentation_reading and political_realignment_reading weakens — the three readings become more genuinely coequal rather than this one being foundational.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(print_causal_weight_vs_content_and_politics, empirical, 'Whether print technology is independently causally sufficient or merely a necessary enabling condition alongside theological and political factors.').

omega_variable(
    kernel_reading_disaggregation_location,
    'Where exactly does the technological_mediation reading diverge from its siblings — is it in the causal ORDER (press enables theology and politics vs. theology/politics use the press as a tool), in the OBSERVABLE SET (publication counts vs. doctrinal texts vs. state formation documents), or in the ATTRIBUTED AGENCY (printers as agents vs. theologians as agents vs. princes as agents)?',
    'This is the committer-structure question routed here per Rule 2: the three readings of reformation_composite disagree on which layer is foundational. A sibling reading (theological_fragmentation_reading) would take doctrinal incompatibility as the generative layer and print merely as an amplifier of an independently-arising theological conflict; political_realignment_reading would take sovereignty assertion as generative and both print and theology as instruments nation-states deployed. This reading inverts both: print access is the load-bearing constraint and doctrinal/political content is the variable content that print made continentally mobile.',
    'Resolving this determines whether the three readings are genuinely coexisting alternative framings (each defensible from a different disciplinary vantage: book history vs. theology vs. political history) or whether one has explanatory priority that should reclassify the others as downstream/dependent rather than coequal siblings.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_disaggregation_location, conceptual, 'Locating the precise structural disagreement between the three kernel readings — causal order, observable set, or attributed agency.').

omega_variable(
    excluded_dissent_erasure_measurability,
    'How much theological dissent that lacked print access is permanently unrecoverable from the historical record versus merely underrepresented, and does this affect how confidently we can claim print access explains WHICH dissenting positions became continental movements?',
    'Archival and archaeological recovery of non-print dissent records (court records of heresy trials, inquisition documents describing oral heterodoxy, material culture evidence) to estimate the volume and content diversity of dissent that never entered print circulation.',
    'If a large volume of theologically distinct dissent existed but is unrecoverable due to non-print transmission, this reading''s implicit claim that print access explains the SHAPE of surviving Reformation theology (not just its speed and scale) would need qualification — print may have filtered content as well as amplified reach.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(excluded_dissent_erasure_measurability, empirical, 'Whether print-access filtering affected which theological content became historically visible, beyond merely amplifying reach.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(reformation_composite__technological_mediation_reading, 1450, 1650).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(refo_tr_t1450, reformation_composite__technological_mediation_reading, theater_ratio, 1450, 0.03).
narrative_ontology:measurement(refo_tr_t1490, reformation_composite__technological_mediation_reading, theater_ratio, 1490, 0.05).
narrative_ontology:measurement(refo_tr_t1517, reformation_composite__technological_mediation_reading, theater_ratio, 1517, 0.07).
narrative_ontology:measurement(refo_tr_t1530, reformation_composite__technological_mediation_reading, theater_ratio, 1530, 0.1).
narrative_ontology:measurement(refo_tr_t1560, reformation_composite__technological_mediation_reading, theater_ratio, 1560, 0.11).
narrative_ontology:measurement(refo_tr_t1600, reformation_composite__technological_mediation_reading, theater_ratio, 1600, 0.12).
narrative_ontology:measurement(refo_tr_t1650, reformation_composite__technological_mediation_reading, theater_ratio, 1650, 0.12).

% Extraction over time
narrative_ontology:measurement(refo_be_t1450, reformation_composite__technological_mediation_reading, base_extractiveness, 1450, 0.08).
narrative_ontology:measurement(refo_be_t1490, reformation_composite__technological_mediation_reading, base_extractiveness, 1490, 0.14).
narrative_ontology:measurement(refo_be_t1517, reformation_composite__technological_mediation_reading, base_extractiveness, 1517, 0.22).
narrative_ontology:measurement(refo_be_t1530, reformation_composite__technological_mediation_reading, base_extractiveness, 1530, 0.34).
narrative_ontology:measurement(refo_be_t1560, reformation_composite__technological_mediation_reading, base_extractiveness, 1560, 0.37).
narrative_ontology:measurement(refo_be_t1600, reformation_composite__technological_mediation_reading, base_extractiveness, 1600, 0.38).
narrative_ontology:measurement(refo_be_t1650, reformation_composite__technological_mediation_reading, base_extractiveness, 1650, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(refo_su_t1450, reformation_composite__technological_mediation_reading, suppression_requirement, 1450, 0.1).
narrative_ontology:measurement(refo_su_t1490, reformation_composite__technological_mediation_reading, suppression_requirement, 1490, 0.15).
narrative_ontology:measurement(refo_su_t1517, reformation_composite__technological_mediation_reading, suppression_requirement, 1517, 0.3).
narrative_ontology:measurement(refo_su_t1530, reformation_composite__technological_mediation_reading, suppression_requirement, 1530, 0.4).
narrative_ontology:measurement(refo_su_t1560, reformation_composite__technological_mediation_reading, suppression_requirement, 1560, 0.42).
narrative_ontology:measurement(refo_su_t1600, reformation_composite__technological_mediation_reading, suppression_requirement, 1600, 0.42).
narrative_ontology:measurement(refo_su_t1650, reformation_composite__technological_mediation_reading, suppression_requirement, 1650, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(reformation_composite__technological_mediation_reading, information_standard).
narrative_ontology:boltzmann_floor_override(reformation_composite__technological_mediation_reading, 0.05).
narrative_ontology:affects_constraint(reformation_composite__technological_mediation_reading, theological_fragmentation_reading).
narrative_ontology:affects_constraint(reformation_composite__technological_mediation_reading, political_realignment_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the reformation_composite kernel, linked bidirectionally per the ε-invariance decomposition principle. theological_fragmentation_reading treats competing soteriological/ecclesiological commitments as the generative mechanism with print as mere amplifier; political_realignment_reading treats nation-state sovereignty assertion as the generative mechanism with both theology and print as instruments. This reading claims causal priority for the press's physical reproduction properties over both. All three should be read as coexisting, non-foreclosing framings unless subsequent comparative-historical analysis (see omega print_causal_weight_vs_content_and_politics) establishes genuine explanatory priority for one over the others.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
