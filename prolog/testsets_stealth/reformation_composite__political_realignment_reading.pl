% ============================================================================
% CONSTRAINT STORY: reformation_composite__political_realignment_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_reformation_composite__political_realignment_reading, []).

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
 *   constraint_id: reformation_composite__political_realignment_reading
 *   human_readable: Cuius Regio Settlement as Sovereignty Instrument (Political Realignment Reading)
 *   domain: historical epistemology/religious history/political economy
 *
 * SUMMARY:
 *   One historiographic tradition reads the Reformation's core as a political
 *   realignment: between 1555 and 1648 the emerging territorial states of the
 *   Holy Roman Empire used religious differentiation as an instrument of
 *   sovereignty, transferring jurisdiction, property, and allegiance from the
 *   supraterritorial authorities (papacy, emperor) to territorial rulers,
 *   with cuius regio eius religio as the settlement's operating rule. This
 *   file instantiates ONLY that reading of the reformation_composite kernel,
 *   per epsilon-invariance: the theological-fragmentation and
 *   technological-mediation readings are separate constraints with their own
 *   epsilon, beneficiaries, and victims, linked through
 *   network.affects_constraints. The referent of epsilon here is the standing
 *   arrangement under contest — the Augsburg-to-Westphalia settlement as this
 *   reading assesses it — never any endorsed alternative. KEY AGENTS (by
 *   structural relationship): - territorial_princes: Primary beneficiary and
 *   agenda-setter (institutional/arbitrage) — deploy confessional
 *   differentiation to consolidate sovereignty - holy_roman_emperor: Primary
 *   institutional victim (institutional/constrained) — cedes jurisdiction,
 *   property, and allegiance - papal_curia: Secondary institutional victim
 *   (institutional/identity_locked) — loses tithes, courts, and northern
 *   allegiance; cannot trade away its own constitutive claim -
 *   territorial_subjects: Extracted population (powerless/constrained) — bear
 *   conversion, conscription, and emigration costs -
 *   radical_reformation_communities: Excluded party (powerless/trapped) —
 *   criminalized by every confession, seated at no table -
 *   free_imperial_cities: Incidental beneficiary (organized/constrained) —
 *   keep bi-confessional autonomy, absorb refugee trade -
 *   confessionalization_historians: Analytical observer — reconstructs the
 *   settlement from visitation and protocol records
 *
 * KEY AGENTS:
 *   - territorial_princes: Primary beneficiary and agenda-setter (institutional/arbitrage) — deploy confessional differentiation to consolidate sovereignty
 *   - holy_roman_emperor: Primary institutional victim (institutional/constrained) — cedes jurisdiction, property, and allegiance
 *   - papal_curia: Secondary institutional victim (institutional/identity_locked) — loses tithes, courts, and northern allegiance; identity-locked to its universal-jurisdiction claim
 *   - territorial_subjects: Extracted population (powerless/constrained) — bear conversion, conscription, and emigration costs
 *   - radical_reformation_communities: Excluded party (powerless/trapped) — criminalized by every confession, seated at no table
 *   - free_imperial_cities: Incidental beneficiary (organized/constrained) — retain bi-confessional autonomy under imperial immediacy
 *   - confessionalization_historians: Analytical observer (analytical/analytical) — tests instrumentality against conviction in the record
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(reformation_composite__political_realignment_reading, 0.7).
domain_priors:suppression_score(reformation_composite__political_realignment_reading, 0.74).
domain_priors:theater_ratio(reformation_composite__political_realignment_reading, 0.36).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(reformation_composite__political_realignment_reading, extractiveness, 0.7).
narrative_ontology:constraint_metric(reformation_composite__political_realignment_reading, suppression_requirement, 0.74).
narrative_ontology:constraint_metric(reformation_composite__political_realignment_reading, theater_ratio, 0.36).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(reformation_composite__political_realignment_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(reformation_composite__political_realignment_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(reformation_composite__political_realignment_reading, tangled_rope).
narrative_ontology:human_readable(reformation_composite__political_realignment_reading, "Cuius Regio Settlement as Sovereignty Instrument (Political Realignment Reading)").
narrative_ontology:topic_domain(reformation_composite__political_realignment_reading, "historical epistemology/religious history/political economy").

domain_priors:requires_active_enforcement(reformation_composite__political_realignment_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(reformation_composite__political_realignment_reading, 'f20e5d74-3db1-4d90-97fc-7ac094befadb').
narrative_ontology:cs_kernel_codification('f20e5d74-3db1-4d90-97fc-7ac094befadb', distributed).
narrative_ontology:cs_authority_grounding('f20e5d74-3db1-4d90-97fc-7ac094befadb', expertise).
narrative_ontology:cs_reading_relation('f20e5d74-3db1-4d90-97fc-7ac094befadb', reformation_composite__theological_fragmentation_reading, coexists_with).
narrative_ontology:cs_reading_relation('f20e5d74-3db1-4d90-97fc-7ac094befadb', reformation_composite__technological_mediation_reading, coexists_with).
narrative_ontology:cs_axiom('f20e5d74-3db1-4d90-97fc-7ac094befadb', foundational, sovereignty_interests_shape_confessional_alignment).
narrative_ontology:cs_axiom_status(sovereignty_interests_shape_confessional_alignment, holdable).
narrative_ontology:cs_axiom_grounding('f20e5d74-3db1-4d90-97fc-7ac094befadb', sovereignty_interests_shape_confessional_alignment, empirically_contingent).
narrative_ontology:cs_axiom('f20e5d74-3db1-4d90-97fc-7ac094befadb', secondary, ecclesiastical_property_is_sovereignty_prize).
narrative_ontology:cs_axiom_status(ecclesiastical_property_is_sovereignty_prize, holdable).
narrative_ontology:cs_axiom_grounding('f20e5d74-3db1-4d90-97fc-7ac094befadb', ecclesiastical_property_is_sovereignty_prize, empirically_contingent).
narrative_ontology:cs_reference_frame('f20e5d74-3db1-4d90-97fc-7ac094befadb', unitary_latin_christendom_jurisdiction).
narrative_ontology:cs_drift_state('f20e5d74-3db1-4d90-97fc-7ac094befadb', post_confessionalization_turn, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('f20e5d74-3db1-4d90-97fc-7ac094befadb', '').
narrative_ontology:cs_kernel_id(reformation_composite__political_realignment_reading, reformation_composite).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(reformation_composite__political_realignment_reading, territorial_princes).
narrative_ontology:constraint_beneficiary(reformation_composite__political_realignment_reading, free_imperial_cities).
narrative_ontology:constraint_victim(reformation_composite__political_realignment_reading, holy_roman_emperor).
narrative_ontology:constraint_victim(reformation_composite__political_realignment_reading, papal_curia).
narrative_ontology:constraint_victim(reformation_composite__political_realignment_reading, territorial_subjects).
narrative_ontology:constraint_victim(reformation_composite__political_realignment_reading, radical_reformation_communities).
narrative_ontology:constraint_vindicates(reformation_composite__political_realignment_reading, cuius_regio_eius_religio_doctrine).
narrative_ontology:constraint_vindicates(reformation_composite__political_realignment_reading, territorial_sovereignty_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Rule their territories within the Holy Roman Empire. Between 1555 and 1648 they acquire the right to determine their subjects' confession, dissolve monasteries and absorb their lands, appoint church officials, and tax clergy. Several change confession when dynastic advantage shifts. Their leverage comes from armies, marriage alliances, and votes in imperial institutions; leaving the Empire is not a live option, but switching sides within it is routine.
narrative_ontology:constraint_stakeholder(reformation_composite__political_realignment_reading, territorial_princes, agenda_setter,
    institutional, generational, arbitrage, regional).
narrative_ontology:stakeholder_secondary_role(reformation_composite__political_realignment_reading, territorial_princes, beneficiary).

% Holds the imperial crown and presides over the Empire's courts and diets. Across the interval he watches jurisdictional business migrate from imperial courts to territorial consistories, sees ecclesiastical principalities secularized, and concedes, first at Augsburg and definitively at Westphalia, that territorial rulers may bind subjects' conscience and dispose of church property. His counters — ban, imperial army, Habsburg dynastic lands — prove insufficient to reverse the transfers, and his own constitutional position forbids simply walking away from the Empire he presides over.
narrative_ontology:constraint_stakeholder(reformation_composite__political_realignment_reading, holy_roman_emperor, payer,
    institutional, generational, constrained, continental).

% Claims universal jurisdiction over Christendom's church. Loses tithe streams, annates, appellate cases, and the allegiance of northern dioceses as the territorial settlements take effect. Protests at Augsburg and refusal to accept parts of Westphalia change little on the ground. Its own office is constituted by the universal-jurisdiction claim, so it cannot trade jurisdiction away for anything else without ceasing to be what it is.
narrative_ontology:constraint_stakeholder(reformation_composite__political_realignment_reading, papal_curia, payer,
    institutional, civilizational, identity_locked, global).

% Farm, pay taxes, and attend whichever church their ruler designates. When a ruler changes confession they face conversion, sale of property, or departure under the exodus clauses. Emigration is lawful after 1555 but costly: land, kin, and guild membership stay behind. They hold no vote at any diet that settles their confession, and they furnish the soldiers and taxes with which the settlement's wars are fought.
narrative_ontology:constraint_stakeholder(reformation_composite__political_realignment_reading, territorial_subjects, payer,
    powerless, biographical, constrained, local).

% Anabaptist, Spiritualist, and other congregations outside both major confessions. Every settlement criminalizes them; Catholic and Protestant territories alike hunt, expel, or execute members. They appear in no treaty text and hold no seat at any negotiation; their existence marks the boundary the settlements define themselves against, and flight between territories only exchanges one persecuting regime for another.
narrative_ontology:constraint_stakeholder(reformation_composite__political_realignment_reading, radical_reformation_communities, excluded,
    powerless, biographical, trapped, regional).

% Answer to the emperor directly rather than to a territorial prince. The 1555 settlement lets them keep both confessions side by side; several profit from dissolved monasteries and from trade and refugees arriving from stricter neighbors. They still garrison, pay for, and police confessional order locally, and their room narrows as the princely territories consolidating around them grow stronger.
narrative_ontology:constraint_stakeholder(reformation_composite__political_realignment_reading, free_imperial_cities, beneficiary,
    organized, generational, constrained, regional).

% Modern scholars reconstructing the period from visitation records, consistory minutes, treaty protocols, and correspondence. They test how much of the confessional map follows princely interest and how much follows parish-level conviction, and their findings feed back into how the settlement's terms are read by every other seat's successors.
narrative_ontology:constraint_stakeholder(reformation_composite__political_realignment_reading, confessionalization_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(reformation_composite__political_realignment_reading, territorial_princes).
narrative_ontology:fixing_cost_class(reformation_composite__political_realignment_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a territorial rule for confessional coexistence inside one empire: cuius regio eius religio determines which confession prevails in each territory, replacing unbounded confessional warfare with a legible allocation of churches, property, and jurisdiction between the two confessions' estates.
% TRANSFER_FUNCTION: Moves ecclesiastical jurisdiction, church property, tithe revenue, and confessional allegiance from supraterritorial authorities (papacy, emperor, bishops) to territorial rulers; moves the subject population's confessional choice from individuals to their prince.
% ABSENT_VOICES: Radical reformers — Anabaptists, Spiritualists, anti-Trinitarians — would object from a position of persecution by every confession and are seated nowhere. The peasantry, having risen in 1525 and been defeated before any settlement, have no delegation at Augsburg or Westphalia despite bearing the conscription and conversion costs. Jewish communities, subjected to intensified confessional policing inside the territories, are likewise unrepresented.
% DISAPPEARANCE_RATIONALE: Overnight removal of the territorial settlement would reopen every question it froze: which confession holds which church, who owns the secularized monasteries, whose courts hear matrimonial and tithe cases, and how two armed confessional blocs coexist in one constitution. Dynastic marriage networks, property titles, and imperial offices had all reorganized around the settlement; its removal forces a general renegotiation under arms.
% FOUNDING_PROBLEM: How can a single empire contain two irreconcilable confessions without permanent civil war — and who governs church property, courts, and appointment inside each territory?
% FOUNDING_PROBLEM_CORROBORATION: No living beneficiary exists to self-attest; the corroborating seats are documentary and historiographic. The imperial Gravamina lists and Reichstag protocols record the grievance structure before any settlement; the treaty texts themselves were negotiated under duress by both confessions' estates, neither of which treated the problem as fictitious; and the confessionalization school (Schilling, Reinhard) together with imperial constitutional history attests the problem-and-settlement structure from outside any benefiting party. Historians dispute whether the problem died with the Empire in 1806 or persists in descendant arrangements (state churches, establishment regimes), hence 'contested'.
narrative_ontology:disappearance_verdict(reformation_composite__political_realignment_reading, world_rearranges).
narrative_ontology:founding_problem_status(reformation_composite__political_realignment_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(reformation_composite__political_realignment_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(reformation_composite__political_realignment_reading, 'none', 1).
narrative_ontology:epsilon_provenance(reformation_composite__political_realignment_reading, 0.7, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(reformation_composite__political_realignment_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(reformation_composite__political_realignment_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(reformation_composite__political_realignment_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Claimed type is tangled_rope and the metrics are authored independently of that claim. The settlement has a genuine coordination function — it replaced unbounded confessional warfare inside the Empire with a legible territorial allocation, and both confessions' estates signed it — and it carries asymmetric extraction in the same structure: rulers collected confiscated property, jurisdiction, and taxing rights while subjects lost confessional choice and the supraterritorial authorities lost revenue and courts. Persistence required active enforcement (imperial execution ordinances, territorial visitations, consistory discipline), hence requires_active_enforcement. Extractiveness 0.70 reflects large transfers bounded by the settlement's real war-ending function. Suppression 0.74 is a raw structural value, unscaled by power or scope: conformity was legally compelled within each territory, with priced emigration as the sanctioned relief valve. Accessibility_collapse 0.58: alternatives did not vanish — exodus clauses, bi-confessional cities, and ruler-side conversion kept exits partly alive — but open dissent inside a territory collapsed. Resistance 0.72: the interval contains the Cologne War, the Thirty Years War, and recurring estate and city resistance; the settlement was fought over, not absorbed quietly. Theater_ratio 0.36: theological framing remained partly functional (a real confessional settlement) while a growing share of activity defended land and jurisdiction grabs, peaking mid-war and falling as Westphalian diplomacy increasingly dropped the pretense. The three measurement series share one time grid (1555/1575/1595/1618/1630/1648) so no metric is ever sampled against another's scalar; the suppression series rises as enforcement machinery matured through the war and declines after 1630 as Westphalia formalized minority protections — an enforcement-capacity arc, not noise.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently from the same settlement. From the princes' seat the arrangement is a legitimate order they built and enforce: peace, property, and the right to order their own churches. From the emperor's and curia's seats the identical clauses are dispossession — courts, tithes, and allegiance moved out from under them by treaty. From the subjects' seat it is compulsion with a priced exit. The engine derives these divergent per-seat classifications from the structural data (role, power, exit); nothing in the authored claim adjudicates between them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations (territorial_princes, free_imperial_cities) drive those agents toward the beneficiary end of d; victim declarations (holy_roman_emperor, papal_curia, territorial_subjects, radical_reformation_communities) drive them toward the target end. Exit modulates within that: princes hold arbitrage-grade exit — they switched confessions for advantage repeatedly — damping their effective burden toward subsidy; the curia is identity-locked to its universal-jurisdiction claim and cannot exit at all, placing it nearest the full-target end; subjects are constrained (emigration lawful but costly); the emperor is constrained by the constitution he administers. The radical communities sit formally outside the settlement yet are maximally exposed materially — their exclusion is part of what the enforcement machinery maintains — so their derived d sits at the extreme despite their holding no seat.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope classification prevents two symmetrical mislabels. Reading the settlement as pure rope ('a peace') erases the extraction — the property transfers and compelled conformity that funded and motivated it. Reading it as pure snare ('a power grab') erases the real coordination — unbounded confessional war was a live alternative in 1546-1555, and the territorial rule demonstrably contained it for decades. On mandate: the founding problem (contain Lutheran-Catholic conflict inside one empire) was live at 1555; by 1648 the arrangement had become the primary vehicle of sovereignty consolidation, its religious content thinned (theater_ratio peaking mid-war, then falling as diplomacy abandoned the pretense). Founding_problem_status is authored 'contested' rather than 'dead': the specific imperial problem died with the Empire in 1806, but the general problem — governing religious plurality through territorial authority — persists in descendant arrangements, so the mismatch consumer should read the dead-problem signal against that genealogical continuity rather than as a simple zombie flag.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reading_primacy_attribution,
    'This constraint is one reading (political_realignment_reading) of the reformation_composite kernel; what would change structurally if a sibling reading were instantiated instead?',
    'Author the sibling files (theological_fragmentation_reading, technological_mediation_reading) and compare computed types, epsilon, and beneficiary/victim sets across the family; convergence on one type would suggest the kernel resists decomposition.',
    'If the theological reading computes with a different victim set (convictional minorities rather than imperial/papal authority) and a different epsilon, the composite label ''the Reformation'' is doing extraction-concealing work and the family decomposition is load-bearing; if all three converge, the readings are facets of one constraint and should be merged.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_primacy_attribution, conceptual, 'Committer structure: which reading of the kernel this constraint instantiates and what siblings would change.').

omega_variable(
    cs_framing_underdetermination,
    'Is the right commitment-system framing the historiographic one declared here (kernel as contested interpretive object, authority in expert historiography), or the constitutional one (kernel as the Augsburg/Westphalia treaty text, authority in the lineage of imperial public law)?',
    'Test both framings against the corpus: under the constitutional framing the kernel is fixed_text with lineage authority and the drift axis runs through imperial public law (Reichspublizistik); under the declared framing drift runs through historiography.',
    'Under the constitutional framing, reading_relations would run between interpretations of the treaty text (strict-confessional versus irenic readings) rather than between causal-primacy readings, changing foreclosure analysis and the computed terminal attractor.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cs_framing_underdetermination, conceptual, 'Two coherent CS framings of the same material yield different cs_pattern classifications.').

omega_variable(
    instrumentality_vs_conviction,
    'Where does princely conviction end and instrumentality begin — were rulers deploying religious differentiation as a sovereignty tool, or acting from confessional commitment whose political effects were consequential?',
    'Correlate conversion and settlement decisions with territorial advantage (secularizable property, bishoprics, alliances) against private correspondence and theological consultation records; the Cologne War and the Palatine conversions are the sharpest test cases.',
    'If conviction dominates, the extraction structure migrates toward the theological reading''s constraint and this file''s epsilon is overstated; if instrumentality dominates, the political reading''s beneficiary-driven directionality stands.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(instrumentality_vs_conviction, empirical, 'Whether the settlement''s religious content was instrumental or convictional at the decisive nodes.').

omega_variable(
    subject_acquiescence_basis,
    'Did the settlement''s stability rest on subjects'' acquiescence or purely on the elites'' monopoly of coercive means?',
    'Compare visitation records and consistory discipline logs against recorded resistance events (flittings, refusals of oath, emigration volumes) per territory.',
    'If acquiescence was broad, measured suppression overstates active coercion and the coordination share of the settlement is larger than authored; if stability was purely coercive, suppression is understated and the subject-seat computation should sit nearer the snare end.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(subject_acquiescence_basis, empirical, 'Basis of settlement stability: consent or coercion.').

omega_variable(
    westphalia_continuity_or_transformation,
    'Does 1648 continue the 1555 arrangement or transform it into a different constraint?',
    'Compare the operative clauses (normal-year baseline, Calvinist admission, minority worship guarantees, territorial superiority) against Augsburg''s terms and track which enforcement machinery carried over.',
    'If transformation, the measurement series should be read as two constraints sharing a grid and the post-1648 points re-based; if continuity, the series stands as one lifecycle and the 1630-1648 suppression decline is internal normalization.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(westphalia_continuity_or_transformation, empirical, 'Lifecycle boundary question at the interval endpoint.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(reformation_composite__political_realignment_reading, 1555, 1648).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(refo_tr_t1555, reformation_composite__political_realignment_reading, theater_ratio, 1555, 0.2).
narrative_ontology:measurement_basis(refo_tr_t1555, observed).
narrative_ontology:measurement(refo_tr_t1575, reformation_composite__political_realignment_reading, theater_ratio, 1575, 0.26).
narrative_ontology:measurement_basis(refo_tr_t1575, observed).
narrative_ontology:measurement(refo_tr_t1595, reformation_composite__political_realignment_reading, theater_ratio, 1595, 0.32).
narrative_ontology:measurement_basis(refo_tr_t1595, observed).
narrative_ontology:measurement(refo_tr_t1618, reformation_composite__political_realignment_reading, theater_ratio, 1618, 0.38).
narrative_ontology:measurement_basis(refo_tr_t1618, observed).
narrative_ontology:measurement(refo_tr_t1630, reformation_composite__political_realignment_reading, theater_ratio, 1630, 0.41).
narrative_ontology:measurement_basis(refo_tr_t1630, observed).
narrative_ontology:measurement(refo_tr_t1648, reformation_composite__political_realignment_reading, theater_ratio, 1648, 0.36).
narrative_ontology:measurement_basis(refo_tr_t1648, observed).

% Extraction over time
narrative_ontology:measurement(refo_be_t1555, reformation_composite__political_realignment_reading, base_extractiveness, 1555, 0.54).
narrative_ontology:measurement_basis(refo_be_t1555, observed).
narrative_ontology:measurement(refo_be_t1575, reformation_composite__political_realignment_reading, base_extractiveness, 1575, 0.59).
narrative_ontology:measurement_basis(refo_be_t1575, observed).
narrative_ontology:measurement(refo_be_t1595, reformation_composite__political_realignment_reading, base_extractiveness, 1595, 0.62).
narrative_ontology:measurement_basis(refo_be_t1595, observed).
narrative_ontology:measurement(refo_be_t1618, reformation_composite__political_realignment_reading, base_extractiveness, 1618, 0.66).
narrative_ontology:measurement_basis(refo_be_t1618, observed).
narrative_ontology:measurement(refo_be_t1630, reformation_composite__political_realignment_reading, base_extractiveness, 1630, 0.68).
narrative_ontology:measurement_basis(refo_be_t1630, observed).
narrative_ontology:measurement(refo_be_t1648, reformation_composite__political_realignment_reading, base_extractiveness, 1648, 0.7).
narrative_ontology:measurement_basis(refo_be_t1648, observed).

% Suppression requirement over time
narrative_ontology:measurement(refo_su_t1555, reformation_composite__political_realignment_reading, suppression_requirement, 1555, 0.6).
narrative_ontology:measurement_basis(refo_su_t1555, observed).
narrative_ontology:measurement(refo_su_t1575, reformation_composite__political_realignment_reading, suppression_requirement, 1575, 0.66).
narrative_ontology:measurement_basis(refo_su_t1575, observed).
narrative_ontology:measurement(refo_su_t1595, reformation_composite__political_realignment_reading, suppression_requirement, 1595, 0.72).
narrative_ontology:measurement_basis(refo_su_t1595, observed).
narrative_ontology:measurement(refo_su_t1618, reformation_composite__political_realignment_reading, suppression_requirement, 1618, 0.8).
narrative_ontology:measurement_basis(refo_su_t1618, observed).
narrative_ontology:measurement(refo_su_t1630, reformation_composite__political_realignment_reading, suppression_requirement, 1630, 0.85).
narrative_ontology:measurement_basis(refo_su_t1630, observed).
narrative_ontology:measurement(refo_su_t1648, reformation_composite__political_realignment_reading, suppression_requirement, 1648, 0.74).
narrative_ontology:measurement_basis(refo_su_t1648, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(reformation_composite__political_realignment_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(reformation_composite__political_realignment_reading, reformation_composite__theological_fragmentation_reading).
narrative_ontology:affects_constraint(reformation_composite__political_realignment_reading, reformation_composite__technological_mediation_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'the Reformation' decomposes, per the epsilon-invariance principle, into three structurally distinct readings of one kernel. This file authors the political_realignment_reading (primary observable: cuius regio eius religio; beneficiaries: territorial rulers; victims: imperial/papal authority and territorial subjects). The theological_fragmentation_reading authors a constraint whose contested structure lives in soteriological incompatibility, and the technological_mediation_reading one whose structure lives in media-driven diffusion; each carries its own epsilon, its own beneficiaries and victims, and its own classification. The upstream/downstream intuition runs opposite to the usual pattern here: the political reading is often cited BY the other two (print spread the princes' program; doctrine supplied the differentiation being deployed), so this file links to both siblings and expects contamination edges in both directions. No epsilon is averaged across the family; each story stands alone.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
