% ============================================================================
% CONSTRAINT STORY: kami_buddha_ontology__incoherent_bundle
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-14
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_kami_buddha_ontology__incoherent_bundle, []).

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
 *   constraint_id: kami_buddha_ontology__incoherent_bundle
 *   human_readable: Shinbutsu-Shugo as an Institutionally Sustained Bundle of Contradictory Commitments
 *   domain: religious studies/Japanese cultural history
 *
 * SUMMARY:
 *   From roughly the eighth century to the eve of the Meiji separation
 *   decrees, kami shrines and Buddhist institutions across Japan operated as
 *   interlocked complexes: temple-appointed clergy administered shrines, kami
 *   were classified as manifestations of buddhas, and village finances fed
 *   both systems. This story instantiates the incoherent-bundle reading of
 *   the kami_buddha_ontology kernel: the arrangement is treated not as the
 *   expression of any single ontology but as a bundle of contradictory
 *   commitments — fusion alongside separation rules, hierarchy alongside
 *   reciprocity, scholastic systematization alongside ungoverned local
 *   practice — sustained by institutional interest, ritual success, and path
 *   dependence. The epsilon referent is the standing shinbutsu-shugo complex
 *   itself, assessed by this reading's own lights: the flows, subordinations,
 *   and services it actually moved, never the arrangement any reform program
 *   endorsed. Claim and metrics are authored independently: the claim is
 *   tangled_rope because the structure demonstrably coordinated (site
 *   finance, staffing, protection, legitimation) while asymmetrically
 *   extracting (revenue shares, administrative control, interpretive
 *   authority over kami); the metrics describe observed operation. The 1868
 *   separation lies outside the measured interval and enters the story as
 *   evidence, not as a data point.
 *
 * KEY AGENTS:
 *   - great_head_temples: Agenda-setting beneficiary (institutional/arbitrage) — administers shrines, captures revenues, sets doctrinal classifications
 *   - court_aristocracy: Beneficiary (institutional/arbitrage) — consumes dual legitimation, funds both systems, bears little friction
 *   - hereditary_shrine_lineages: Primary target (moderate/identity_locked) — displaced from their own sanctuaries and their deities' identities
 *   - ise_grand_shrine_priesthood: Resistant target (organized/identity_locked) — fought incorporation to a partial negotiated discount
 *   - village_payer_households: Diffuse target (powerless/trapped) — doubled obligations with festival benefits attached
 *   - kokugaku_scholars: Excluded critic (moderate/constrained) — nativist philology, actionable only after 1868
 *   - meiji_restoration_leaders: External executioner (institutional/arbitrage) — imposed separation and harvested the political ground
 *   - historical_religion_scholars: Analytical observer (analytical/analytical) — reconstructs the ledgers against the claims
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(kami_buddha_ontology__incoherent_bundle, 0.62).
domain_priors:suppression_score(kami_buddha_ontology__incoherent_bundle, 0.45).
domain_priors:theater_ratio(kami_buddha_ontology__incoherent_bundle, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(kami_buddha_ontology__incoherent_bundle, extractiveness, 0.62).
narrative_ontology:constraint_metric(kami_buddha_ontology__incoherent_bundle, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(kami_buddha_ontology__incoherent_bundle, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(kami_buddha_ontology__incoherent_bundle, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(kami_buddha_ontology__incoherent_bundle, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(kami_buddha_ontology__incoherent_bundle, tangled_rope).
narrative_ontology:human_readable(kami_buddha_ontology__incoherent_bundle, "Shinbutsu-Shugo as an Institutionally Sustained Bundle of Contradictory Commitments").
narrative_ontology:topic_domain(kami_buddha_ontology__incoherent_bundle, "religious studies/Japanese cultural history").

domain_priors:requires_active_enforcement(kami_buddha_ontology__incoherent_bundle).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(kami_buddha_ontology__incoherent_bundle, 'd9f01b3d-4367-4117-99b8-061c36f24c74').
narrative_ontology:cs_kernel_codification('d9f01b3d-4367-4117-99b8-061c36f24c74', distributed).
narrative_ontology:cs_authority_grounding('d9f01b3d-4367-4117-99b8-061c36f24c74', distributed).
narrative_ontology:cs_reading_relation('d9f01b3d-4367-4117-99b8-061c36f24c74', kami_buddha_ontology__honji_suijaku_monism, influences).
narrative_ontology:cs_reading_relation('d9f01b3d-4367-4117-99b8-061c36f24c74', kami_buddha_ontology__domain_partition, influences).
narrative_ontology:cs_axiom('d9f01b3d-4367-4117-99b8-061c36f24c74', foundational, contradiction_is_load_bearing).
narrative_ontology:cs_axiom_status(contradiction_is_load_bearing, holdable).
narrative_ontology:cs_axiom_grounding('d9f01b3d-4367-4117-99b8-061c36f24c74', contradiction_is_load_bearing, empirically_contingent).
narrative_ontology:cs_axiom('d9f01b3d-4367-4117-99b8-061c36f24c74', secondary, ritual_efficacy_outweighs_doctrine).
narrative_ontology:cs_axiom_status(ritual_efficacy_outweighs_doctrine, holdable).
narrative_ontology:cs_axiom_grounding('d9f01b3d-4367-4117-99b8-061c36f24c74', ritual_efficacy_outweighs_doctrine, instrumental).
narrative_ontology:cs_reference_frame('d9f01b3d-4367-4117-99b8-061c36f24c74', plural_institutional_composite).
narrative_ontology:cs_drift_state('d9f01b3d-4367-4117-99b8-061c36f24c74', bakumatsu_nativist_revival, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('d9f01b3d-4367-4117-99b8-061c36f24c74', '').
narrative_ontology:cs_kernel_id(kami_buddha_ontology__incoherent_bundle, kami_buddha_ontology).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(kami_buddha_ontology__incoherent_bundle, great_head_temples).
narrative_ontology:constraint_beneficiary(kami_buddha_ontology__incoherent_bundle, court_aristocracy).
narrative_ontology:constraint_victim(kami_buddha_ontology__incoherent_bundle, hereditary_shrine_lineages).
narrative_ontology:constraint_victim(kami_buddha_ontology__incoherent_bundle, village_payer_households).
narrative_ontology:constraint_victim(kami_buddha_ontology__incoherent_bundle, ise_grand_shrine_priesthood).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(kami_buddha_ontology__incoherent_bundle, village_payer_households).
narrative_ontology:constraint_vindicates(kami_buddha_ontology__incoherent_bundle, ritual_efficacy_primacy).
narrative_ontology:constraint_vindicates(kami_buddha_ontology__incoherent_bundle, syncretic_dual_legitimation).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Operate the great temple networks that appoint supervising clergy to major shrines, run the attached shrine-temples, classify kami within Buddhist doctrinal schemes, and take a share of shrine revenues and estate income. They decide which kami count as manifestations of which buddhas, staff the scholastic lineages that elaborate the correspondences, and shift personnel and patronage between court, shogunate, and provinces as politics move. Abandoning the arrangement would mean surrendering the shrine network's revenues and the doctrinal authority that anchors their public standing.
narrative_ontology:constraint_stakeholder(kami_buddha_ontology__incoherent_bundle, great_head_temples, agenda_setter,
    institutional, generational, arbitrage, national).

% Patronize both systems and draw legitimation from each: oracles from prominent shrines authorize court decisions, while Buddhist masses and consecrations sacralize reigns and lineages. They fill high clerical offices, grant and confirm the estate holdings that fund the complexes, and tilt favor between temple and shrine factions as factional politics require. Their personal exposure to the arrangement's frictions is small; they consume its outputs.
narrative_ontology:constraint_stakeholder(kami_buddha_ontology__incoherent_bundle, court_aristocracy, beneficiary,
    institutional, generational, arbitrage, national).

% Hold hereditary custodianship of local shrines. Over the interval many lose administrative command of their own sanctuaries to temple-appointed clergy, watch their deities reinterpreted as manifestations of buddhas, and remit a share of offerings uphill through the attached temple. Office, family name, and locality are inseparable for them: abandoning the post extinguishes the lineage, while keeping it means living inside someone else's account of their own god.
narrative_ontology:constraint_stakeholder(kami_buddha_ontology__incoherent_bundle, hereditary_shrine_lineages, payer,
    moderate, generational, identity_locked, regional).

% Administer the Ise sanctuary under a mythic charter predating Buddhism's arrival. They refuse Buddhist readings of their deity for most of the interval, exclude monks from the inner precincts longer than nearly any other site, and build an independent doctrinal corpus when pressure mounts. Resistance buys only a negotiated discount: by the late period Ise accommodates selected Buddhist terminology while keeping its core cult autonomous.
narrative_ontology:constraint_stakeholder(kami_buddha_ontology__incoherent_bundle, ise_grand_shrine_priesthood, payer,
    organized, generational, identity_locked, regional).

% Owe dues and labor to both the parish temple and the local shrine: rice levies, festival corvee, funeral fees, offering obligations. They receive in return the festival calendar, healing rites, and protective services the combined complex provides. Leaving either obligation short of fleeing the land forfeits tenure, and flight forfeits livelihood.
narrative_ontology:constraint_stakeholder(kami_buddha_ontology__incoherent_bundle, village_payer_households, payer,
    powerless, immediate, trapped, local).
narrative_ontology:stakeholder_secondary_role(kami_buddha_ontology__incoherent_bundle, village_payer_households, beneficiary).

% Eighteenth-century philologists arguing from textual evidence that the kami-buddha fusion is a late corruption laid over an originally pure and sovereign kami tradition. They publish outside the clerical appointment economy and hold no administrative power over the complexes during the interval; their critique becomes actionable only when a new state goes looking for ideological foundations after 1868.
narrative_ontology:constraint_stakeholder(kami_buddha_ontology__incoherent_bundle, kokugaku_scholars, excluded,
    moderate, civilizational, constrained, national).

% Restoration officials formed outside the temple-shrine establishment who take power in 1868 committed to dividing kami worship from Buddhism. They issue the separation decrees, preside over the demolition of thousands of temples and the laicizing of their clergy, and erect a state shrine system on the cleared ground, converting the old arrangement's dissolution into new state authority.
narrative_ontology:constraint_stakeholder(kami_buddha_ontology__incoherent_bundle, meiji_restoration_leaders, excluded,
    institutional, generational, arbitrage, national).

% Modern historians of Japanese religion who reconstruct how the complexes financed themselves, how correspondence doctrines circulated among rival schools, and why separation succeeded only under revolutionary conditions. They sit outside every historical seat and can set the arrangement's claims against its ledgers.
narrative_ontology:constraint_stakeholder(kami_buddha_ontology__incoherent_bundle, historical_religion_scholars, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Integrates an imported soteriological religion and indigenous territorial cults into one operational religious economy: shared sacred sites finance both systems, temple-run shrine administration solves staffing and record-keeping at national scale, mutual protection pacts between armed monasteries and shrine retinues police disputes among sacred corporations, and dual legitimation lets the court mobilize whichever authority a situation calls for.
% TRANSFER_FUNCTION: Moves rice income, timber, corvee labor, and votive wealth from village households and provincial estates up through the shrine-temples to the great head temples; moves administrative control of shrines and interpretive authority over kami from hereditary priestly houses to temple-appointed clergy; returns festival provision, funerary service, armed protection, and oracle-derived legitimacy downward to patrons and localities.
% ABSENT_VOICES: Nativist philologists and kami-cult autonomists object that fusion contaminates the kami tradition and serves temple finance rather than the deities; village households carry doubled obligations without any seat in the appointment or doctrinal councils that set them. Both groups sit outside the court-temple appointment network — in provincial academies, print culture, and rural shrines — until a revolutionary state imports their critique wholesale in 1868.
% DISAPPEARANCE_RATIONALE: When the separation decrees landed in 1868 the rearrangement was violent and total: thousands of temples demolished or stripped, tens of thousands of clergy forcibly laicized, icons burned or sold, shrine-temple compounds physically partitioned, parish funeral affiliations rewritten, and a state shrine system built atop the wreckage. Centuries of interlocked finance, clerical kinship, and festival routine did not dissolve quietly — the world rearranged.
% FOUNDING_PROBLEM: An immigrant religion carrying scriptural prestige, economic technique, and state backing arrives among entrenched territorial cults. Each side needs the other: Buddhism requires local anchorage and protection from cultic hostility; the cults require literacy, finance, and court access. The founding problem was achieving mutual legitimation between the two without open religious war.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: eighteenth-century nativist philology (Motoori Norinaga and successors) dated the fusion to post-Nara institutional accretion rather than continuing necessity; Meiji ritsuryo reformers invoked pre-fusion precedent to argue the arrangement was dispensable; modern academic historiography (Kuroda Toshio's kenmitsu analysis and the subsequent literature on combinatory religion) reconstructs the system's persistence as driven by institutional interest and path dependence after the original integration crisis closed. No beneficiary testimony is needed to establish obsolescence.
narrative_ontology:disappearance_verdict(kami_buddha_ontology__incoherent_bundle, world_rearranges).
narrative_ontology:founding_problem_status(kami_buddha_ontology__incoherent_bundle, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(kami_buddha_ontology__incoherent_bundle, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(kami_buddha_ontology__incoherent_bundle, 'none', 1).
narrative_ontology:epsilon_provenance(kami_buddha_ontology__incoherent_bundle, 0.62, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(kami_buddha_ontology__incoherent_bundle_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(kami_buddha_ontology__incoherent_bundle, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(kami_buddha_ontology__incoherent_bundle_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction (0.62 at interval end) reflects large, durable asymmetric flows — shrine revenue shares routed through the attached temples, hereditary lineages displaced from their own sanctuaries, interpretive authority over kami transferred to scholastic lineages — offset by real returned services: festival provision, funerals, armed protection, court legitimation. Suppression (0.45) is authored as the raw structural property it is, unscaled by power or scope, and is moderate because enforcement ran through appointment control, estate courts, and episodic armed demonstration rather than systematic violence; exit was blocked chiefly by lineage identity and land tenure, not by force. Theater (0.45) tracks the growing share of activity devoted to maintaining coherence claims — correspondence tables, mandalic mappings, inter-school polemics — relative to the practical exchanges beneath; it rises steadily as systematization outruns practice. Accessibility collapse (0.42) is low-moderate: alternatives visibly persisted (the Yoshida inverted hierarchy licensed in the fifteenth century, Ise's partial refusal, Pure Land parishes minimizing kami cult, nativist philology articulating principled exit). Resistance (0.55) records recurrent pulses rather than a cycle: armed shrine petition-processions, village leagues rising against temple exactions, Ise's boundary defense, and finally nativist critique — pulses driven by enforcement intensity, not an oscillating internal mechanism. The series share one time grid: every tracked metric is authored at every examined year, so no end-state value leaks backward into earlier rows.
 *
 * PERSPECTIVAL GAP:
 *   Seats should compute differently. From the head-temple chair the arrangement is administration working: revenues balanced, sanctuaries staffed, doctrine taught, protection delivered. From a hereditary shrine lineage the same ledger reads as dispossession — strangers installed over one's own god, offerings flowing uphill, the deity renamed in someone else's metaphysics. From a village household it is a bill with festivals attached. From the Ise seat it is a siege withstood only partially. The divergence is computed by the engine from power, exit, and role data; the authored claim adjudicates none of it.
 *
 * DIRECTIONALITY LOGIC:
 *   The declarations map to observable structure. great_head_temples and court_aristocracy sit at the beneficiary end: the temples capture receipts outright (see gain_flow), the court consumes legitimation while funding comparatively little. The three payer groups grade toward the target end: hereditary_shrine_lineages (identity_locked, moderate power) sit near-full-target; village_payer_households (trapped, powerless) sit near-full-target despite their secondary festival benefits; ise_grand_shrine_priesthood (organized, identity_locked) is a target that fought its way to a negotiated discount, placing it somewhat off the full-target pole. Suppression stays unscaled in reporting; effective-extraction amplification belongs to the engine's arithmetic over directionality and spatial scope. No directionality overrides are authored: beneficiary/victim declarations plus the exit atoms already yield the intended gradient, and no seat's derived position looked wrong on inspection.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — mutual legitimation of an imported soteriology and entrenched cults without religious war — closed centuries before the arrangement did. What persisted afterward was maintained by revenue, appointment power, and lineage identity rather than by the original necessity; the founding_problem_status x disappearance_verdict combination (dead x world_rearranges) should trip the capture/zombie mismatch flag, and that is the honest finding: a mandate outliving its function while still rearranging the world when removed. The tangled_rope claim guards against two mislabels. Calling the late bundle pure extraction erases the coordination services villagers, patrons, and the court demonstrably consumed; calling it inertial residue alone misses the active enforcement — appointment control, estate courts, armed demonstration — that held the asymmetric flows in place. The post-1868 successor arrangements (state shrine cult, household funeral Buddhism) are separate constraints for separate stories; whether they inherited the bundle's contradictions is reserved for the successor omega.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_underdetermination,
    'Is kami_buddha_ontology a single contested kernel with rival coherent readings (monism, partition), or no kernel at all as this reading asserts?',
    'Compile the sibling stories and compare per-seat classifications and epsilon profiles; if a sibling yields a stable low-conflict profile matching the historical consensus periods, kernel-existence gains support.',
    'If the kernel exists, this story over-attributes doctrinal conflict costs to institutional maintenance; if it does not, the siblings are component-strand ideologies and this story carries the arrangement-level classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_underdetermination, conceptual, 'Committer-frame uncertainty: whether the contested kernel is real or the bundle reading is the correct meta-description.').

omega_variable(
    bunri_natural_experiment,
    'Does the 1868 forced separation show the bundle''s contradictions were externally imposed (institutionally maintained) or demand-side (reproduced spontaneously by popular practice)?',
    'Post-separation trajectory analysis: if the post-1868 religious order recombined bundle-like patterns anyway (state shrine rites absorbing former fusion forms, household funerals staying Buddhist), the contradictions were demand-driven; if separation held cleanly, maintenance was institutional.',
    'Demand-side reproduction shifts causal weight and victim attribution from institutional imposition toward emergent practice, weakening the enforcement-gate reading of the bundle; clean separation strengthens it.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(bunri_natural_experiment, empirical, 'Whether removal of the enforcing institutions dissolves or reproduces the contradictory commitments.').

omega_variable(
    epsilon_decomposition_boundary,
    'Does the bundle carry one stable extraction value, or do revenue extraction and autonomy-or-interpretive expropriation come apart far enough to require two stories?',
    'Subperiod analysis comparing revenue-share ledgers against administrative-control and doctrinal-classification histories; if the observables diverge sharply in direction or magnitude within any subperiod, split into separate constraint stories linked by network edges.',
    'If they diverge, the single epsilon conflates a fiscal-flow component with a status-and-identity component and misprices both; if they track together, one story stands.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(epsilon_decomposition_boundary, conceptual, 'Epsilon-invariance guard: monitoring whether the bundle''s extraction is measurable under one observable or secretly two.').

omega_variable(
    successor_arrangement_transmigration,
    'After 1868 did the bundle terminate, or transmigrate into successor arrangements (state shrine cult, parish funeral Buddhism) carrying its contradictions forward?',
    'Author the successor arrangements as their own constraint stories and test for inherited contradiction structure, using this story''s network edges as the comparison baseline.',
    'Transmigration would indicate the bundle''s contradictions survive enforcement change (demand-side persistence and a candidate piton signature downstream); clean termination would localize causation in institutional maintenance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(successor_arrangement_transmigration, empirical, 'Lifecycle question: whether the bundle ended in 1868 or changed carriers.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(kami_buddha_ontology__incoherent_bundle, 750, 1840).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(kami_tr_t750, kami_buddha_ontology__incoherent_bundle, theater_ratio, 750, 0.2).
narrative_ontology:measurement_basis(kami_tr_t750, observed).
narrative_ontology:measurement(kami_tr_t850, kami_buddha_ontology__incoherent_bundle, theater_ratio, 850, 0.22).
narrative_ontology:measurement_basis(kami_tr_t850, observed).
narrative_ontology:measurement(kami_tr_t950, kami_buddha_ontology__incoherent_bundle, theater_ratio, 950, 0.28).
narrative_ontology:measurement_basis(kami_tr_t950, observed).
narrative_ontology:measurement(kami_tr_t1100, kami_buddha_ontology__incoherent_bundle, theater_ratio, 1100, 0.33).
narrative_ontology:measurement_basis(kami_tr_t1100, observed).
narrative_ontology:measurement(kami_tr_t1250, kami_buddha_ontology__incoherent_bundle, theater_ratio, 1250, 0.38).
narrative_ontology:measurement_basis(kami_tr_t1250, observed).
narrative_ontology:measurement(kami_tr_t1400, kami_buddha_ontology__incoherent_bundle, theater_ratio, 1400, 0.4).
narrative_ontology:measurement_basis(kami_tr_t1400, observed).
narrative_ontology:measurement(kami_tr_t1550, kami_buddha_ontology__incoherent_bundle, theater_ratio, 1550, 0.41).
narrative_ontology:measurement_basis(kami_tr_t1550, observed).
narrative_ontology:measurement(kami_tr_t1700, kami_buddha_ontology__incoherent_bundle, theater_ratio, 1700, 0.43).
narrative_ontology:measurement_basis(kami_tr_t1700, observed).
narrative_ontology:measurement(kami_tr_t1840, kami_buddha_ontology__incoherent_bundle, theater_ratio, 1840, 0.45).
narrative_ontology:measurement_basis(kami_tr_t1840, observed).

% Extraction over time
narrative_ontology:measurement(kami_be_t750, kami_buddha_ontology__incoherent_bundle, base_extractiveness, 750, 0.35).
narrative_ontology:measurement_basis(kami_be_t750, observed).
narrative_ontology:measurement(kami_be_t850, kami_buddha_ontology__incoherent_bundle, base_extractiveness, 850, 0.42).
narrative_ontology:measurement_basis(kami_be_t850, observed).
narrative_ontology:measurement(kami_be_t950, kami_buddha_ontology__incoherent_bundle, base_extractiveness, 950, 0.48).
narrative_ontology:measurement_basis(kami_be_t950, observed).
narrative_ontology:measurement(kami_be_t1100, kami_buddha_ontology__incoherent_bundle, base_extractiveness, 1100, 0.55).
narrative_ontology:measurement_basis(kami_be_t1100, observed).
narrative_ontology:measurement(kami_be_t1250, kami_buddha_ontology__incoherent_bundle, base_extractiveness, 1250, 0.6).
narrative_ontology:measurement_basis(kami_be_t1250, observed).
narrative_ontology:measurement(kami_be_t1400, kami_buddha_ontology__incoherent_bundle, base_extractiveness, 1400, 0.63).
narrative_ontology:measurement_basis(kami_be_t1400, observed).
narrative_ontology:measurement(kami_be_t1550, kami_buddha_ontology__incoherent_bundle, base_extractiveness, 1550, 0.61).
narrative_ontology:measurement_basis(kami_be_t1550, observed).
narrative_ontology:measurement(kami_be_t1700, kami_buddha_ontology__incoherent_bundle, base_extractiveness, 1700, 0.62).
narrative_ontology:measurement_basis(kami_be_t1700, observed).
narrative_ontology:measurement(kami_be_t1840, kami_buddha_ontology__incoherent_bundle, base_extractiveness, 1840, 0.62).
narrative_ontology:measurement_basis(kami_be_t1840, observed).

% Suppression requirement over time
narrative_ontology:measurement(kami_su_t750, kami_buddha_ontology__incoherent_bundle, suppression_requirement, 750, 0.25).
narrative_ontology:measurement_basis(kami_su_t750, observed).
narrative_ontology:measurement(kami_su_t850, kami_buddha_ontology__incoherent_bundle, suppression_requirement, 850, 0.32).
narrative_ontology:measurement_basis(kami_su_t850, observed).
narrative_ontology:measurement(kami_su_t950, kami_buddha_ontology__incoherent_bundle, suppression_requirement, 950, 0.4).
narrative_ontology:measurement_basis(kami_su_t950, observed).
narrative_ontology:measurement(kami_su_t1100, kami_buddha_ontology__incoherent_bundle, suppression_requirement, 1100, 0.47).
narrative_ontology:measurement_basis(kami_su_t1100, observed).
narrative_ontology:measurement(kami_su_t1250, kami_buddha_ontology__incoherent_bundle, suppression_requirement, 1250, 0.55).
narrative_ontology:measurement_basis(kami_su_t1250, observed).
narrative_ontology:measurement(kami_su_t1400, kami_buddha_ontology__incoherent_bundle, suppression_requirement, 1400, 0.58).
narrative_ontology:measurement_basis(kami_su_t1400, observed).
narrative_ontology:measurement(kami_su_t1550, kami_buddha_ontology__incoherent_bundle, suppression_requirement, 1550, 0.58).
narrative_ontology:measurement_basis(kami_su_t1550, observed).
narrative_ontology:measurement(kami_su_t1700, kami_buddha_ontology__incoherent_bundle, suppression_requirement, 1700, 0.46).
narrative_ontology:measurement_basis(kami_su_t1700, observed).
narrative_ontology:measurement(kami_su_t1840, kami_buddha_ontology__incoherent_bundle, suppression_requirement, 1840, 0.45).
narrative_ontology:measurement_basis(kami_su_t1840, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(kami_buddha_ontology__incoherent_bundle, resource_allocation).
narrative_ontology:affects_constraint(kami_buddha_ontology__incoherent_bundle, honji_suijaku_monism).
narrative_ontology:affects_constraint(kami_buddha_ontology__incoherent_bundle, domain_partition).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'shinbutsu-shugo' decomposes, per the epsilon-invariance principle, into three structurally distinct claims — honji_suijaku_monism (ontological identity; negligible-to-moderate extraction depending on how subordination of kami cults is priced), domain_partition (functional separation of life/death domains; primarily descriptive with modest enforcement overhead), and this story, the incoherent_bundle (arrangement-level: contradictions sustained by institutional interest; substantial asymmetric flows with genuine coordination services). The monism story sits upstream: its doctrine supplied the bundle's hierarchical strand and was cited as evidence of the arrangement's coherence. This story links both siblings via affects_constraints; each sibling should carry reciprocal edges and its own epsilon, beneficiaries, and claimed type.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
