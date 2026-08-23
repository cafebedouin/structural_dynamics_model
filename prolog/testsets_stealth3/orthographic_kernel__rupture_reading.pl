% ============================================================================
% CONSTRAINT STORY: orthographic_kernel__rupture_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_orthographic_kernel__rupture_reading, []).

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
 *   constraint_id: orthographic_kernel__rupture_reading
 *   human_readable: Enforced Latin Orthography as Deliberate Cultural Rupture (1928 Alphabet Law, rupture reading)
 *   domain: political linguistics/state formation/commitment systems
 *
 * SUMMARY:
 *   On 1 November 1928 the Grand National Assembly enacted the Alphabet Law
 *   (Kanun No. 1353), replacing Arabic script with Latin for Turkish. This
 *   story instantiates the RUPTURE READING of that arrangement: the script
 *   change as a deliberate instrument of civilizational discontinuity, not
 *   primarily a literacy measure. On this reading the modernization rationale
 *   was the public cover; the function was manufactured amnesia — within a
 *   decade the pre-reform literate population's written world
 *   (correspondence, gravestones, ledgers, five centuries of poetry and
 *   jurisprudence, the Ottoman archive) became illegible to the citizens of
 *   the republic that inherited it, and the state became sole curator of the
 *   national written record. Per the epsilon-invariance rule, this file
 *   authors epsilon ONLY for this reading's construal of the standing
 *   arrangement (exclusive Latin orthography backed by the 1928 law and its
 *   enforcement successors); the continuity and modernization readings are
 *   separate constraints (separate files) linked via
 *   network.affects_constraints. KEY AGENTS (by structural relationship): -
 *   post_reform_state_apparatus: Primary beneficiary and agenda-setter
 *   (institutional/arbitrage) — drafts, enforces, and collects -
 *   pre_reform_literate_population: Primary target (moderate/trapped) — bears
 *   the severance - islamic_scholarly_establishment: Secondary target
 *   (organized/trapped) - ottoman_calligraphers_and_scribes: Secondary target
 *   (powerless/trapped) - post_reform_generations: Downstream target with
 *   incidental literacy gain (moderate/constrained) -
 *   kemalist_republican_intelligentsia: Secondary beneficiary
 *   (powerful/mobile) - arabic_script_diaspora_press: Excluded actor
 *   (moderate/mobile) - historical_linguistics_analysts: Analytical observer
 *   — sees the full structure
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(orthographic_kernel__rupture_reading, 0.84).
domain_priors:suppression_score(orthographic_kernel__rupture_reading, 0.6).
domain_priors:theater_ratio(orthographic_kernel__rupture_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(orthographic_kernel__rupture_reading, extractiveness, 0.84).
narrative_ontology:constraint_metric(orthographic_kernel__rupture_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(orthographic_kernel__rupture_reading, theater_ratio, 0.55).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(orthographic_kernel__rupture_reading, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(orthographic_kernel__rupture_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(orthographic_kernel__rupture_reading, snare).
narrative_ontology:human_readable(orthographic_kernel__rupture_reading, "Enforced Latin Orthography as Deliberate Cultural Rupture (1928 Alphabet Law, rupture reading)").
narrative_ontology:topic_domain(orthographic_kernel__rupture_reading, "political linguistics/state formation/commitment systems").

domain_priors:requires_active_enforcement(orthographic_kernel__rupture_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(orthographic_kernel__rupture_reading, 'ef50e4d3-a56e-4011-9cbd-148bbaf20e97').
narrative_ontology:cs_kernel_codification('ef50e4d3-a56e-4011-9cbd-148bbaf20e97', formalized).
narrative_ontology:cs_authority_grounding('ef50e4d3-a56e-4011-9cbd-148bbaf20e97', extraction).
narrative_ontology:cs_interpretation_layer_present('ef50e4d3-a56e-4011-9cbd-148bbaf20e97').
narrative_ontology:cs_reading_relation('ef50e4d3-a56e-4011-9cbd-148bbaf20e97', orthographic_kernel__continuity_reading, coexists_with).
narrative_ontology:cs_reading_relation('ef50e4d3-a56e-4011-9cbd-148bbaf20e97', orthographic_kernel__modernization_reading, coexists_with).
narrative_ontology:cs_axiom('ef50e4d3-a56e-4011-9cbd-148bbaf20e97', foundational, legitimate_nationhood_requires_past_severance).
narrative_ontology:cs_axiom_status(legitimate_nationhood_requires_past_severance, holdable).
narrative_ontology:cs_axiom_grounding('ef50e4d3-a56e-4011-9cbd-148bbaf20e97', legitimate_nationhood_requires_past_severance, instrumental).
narrative_ontology:cs_axiom('ef50e4d3-a56e-4011-9cbd-148bbaf20e97', secondary, narrative_sovereignty_requires_archive_gatekeeping).
narrative_ontology:cs_axiom_status(narrative_sovereignty_requires_archive_gatekeeping, holdable).
narrative_ontology:cs_axiom_grounding('ef50e4d3-a56e-4011-9cbd-148bbaf20e97', narrative_sovereignty_requires_archive_gatekeeping, instrumental).
narrative_ontology:cs_reference_frame('ef50e4d3-a56e-4011-9cbd-148bbaf20e97', completed_civilizational_rupture).
narrative_ontology:cs_drift_state('ef50e4d3-a56e-4011-9cbd-148bbaf20e97', contemporary_heritage_revival_era, gap(revival_pressure, minor, true)).
narrative_ontology:cs_created_at('ef50e4d3-a56e-4011-9cbd-148bbaf20e97', '2026-06-12T00:00:00Z').
narrative_ontology:cs_kernel_id(orthographic_kernel__rupture_reading, orthographic_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(orthographic_kernel__rupture_reading, post_reform_state_apparatus).
narrative_ontology:constraint_beneficiary(orthographic_kernel__rupture_reading, kemalist_republican_intelligentsia).
narrative_ontology:constraint_victim(orthographic_kernel__rupture_reading, pre_reform_literate_population).
narrative_ontology:constraint_victim(orthographic_kernel__rupture_reading, islamic_scholarly_establishment).
narrative_ontology:constraint_victim(orthographic_kernel__rupture_reading, ottoman_calligraphers_and_scribes).
narrative_ontology:constraint_victim(orthographic_kernel__rupture_reading, post_reform_generations).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(orthographic_kernel__rupture_reading, post_reform_generations).
narrative_ontology:constraint_vindicates(orthographic_kernel__rupture_reading, deliberate_amnesia_nation_building_doctrine).
narrative_ontology:constraint_vindicates(orthographic_kernel__rupture_reading, script_severance_secularization_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Drafted and enacted the 1928 Alphabet Law and its successor regulations; ran the compulsory literacy campaigns, licensed printers, examined officials, and decided what could be published in which script. Since 1928 it has been the sole institution able to curate the national written record. Collects the arrangement's returns directly: a citizenry that reads only what the republic prints, and a pre-1928 archive legible only through state-controlled gatekeepers. Could suspend or modify the policy at will and bears little of its cost.
narrative_ontology:constraint_stakeholder(orthographic_kernel__rupture_reading, post_reform_state_apparatus, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(orthographic_kernel__rupture_reading, post_reform_state_apparatus, beneficiary).

% Writers, jurists, teachers, and journalists who staffed the new republic's schools, ministries, courts, and presses. Entered a cultural field cleared of Ottoman-trained rivals: new canon, new journals, new university chairs, a readership that could only read them. Their careers and authority were built inside the new orthography; leaving it would mean forfeiting the field they came to dominate.
narrative_ontology:constraint_stakeholder(orthographic_kernel__rupture_reading, kemalist_republican_intelligentsia, beneficiary,
    powerful, biographical, mobile, national).

% Adults literate in Ottoman script in 1928 — clerks, merchants, officers, clergy, teachers, and domestically educated women. Within months their handwriting, libraries, ledgers, and family correspondence became illegible to the next generation and unusable in public life. Retraining was offered late in life and taken unevenly; a large share never regained functional literacy in any script. No channel existed to keep their written world public.
narrative_ontology:constraint_stakeholder(orthographic_kernel__rupture_reading, pre_reform_literate_population, payer,
    moderate, biographical, trapped, national).

% Ulema, Sufi orders, and Quranic teachers whose institutional base (medreses, tekkes) had been closed in 1924-25, immediately before the script law. Their textual authority ran through Arabic and Ottoman script; the new orthography and the restriction of Arabic-script religious publishing in Turkish cut the transmission line to students. Networks and personal prestige survived; the institutions that reproduced them did not.
narrative_ontology:constraint_stakeholder(orthographic_kernel__rupture_reading, islamic_scholarly_establishment, payer,
    organized, generational, trapped, national).

% Master calligraphers, manuscript illuminators, and court scribes of Istanbul and the provincial centers. Demand for their craft collapsed when official documents, newspapers, and books moved to Latin type; a five-century art lineage lost its livelihood within a decade and survives mainly as museum piece and amateur practice.
narrative_ontology:constraint_stakeholder(orthographic_kernel__rupture_reading, ottoman_calligraphers_and_scribes, payer,
    powerless, biographical, trapped, regional).

% Citizens schooled entirely in the Latin orthography after 1928. They gained mass literacy in the new script but cannot read their grandparents' letters, family gravestones, or the Ottoman archive without specialist training. Learning the old script as an adult marks them socially as nostalgic or religiously motivated, so most do not; their access to the pre-1928 written world runs through credentialed intermediaries.
narrative_ontology:constraint_stakeholder(orthographic_kernel__rupture_reading, post_reform_generations, payer,
    moderate, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(orthographic_kernel__rupture_reading, post_reform_generations, beneficiary).

% Emigre publishers and journal editors in Cairo, Beirut, and the Balkans who continued printing Ottoman-script Turkish after 1928. Locked out of the Anatolian market by import and postal rules; their editions circulated among exiles and returned home only decades later as rare antiquities.
narrative_ontology:constraint_stakeholder(orthographic_kernel__rupture_reading, arabic_script_diaspora_press, excluded,
    moderate, biographical, mobile, continental).

% Archivists, paleographers, and historians of the language reform in Turkey and abroad. Measure the literacy transition, catalog what became illegible, and attest the founding problem and its status from outside the arrangement's beneficiary set. Their evidence base runs through the same state archives the arrangement gatekeeps.
narrative_ontology:constraint_stakeholder(orthographic_kernel__rupture_reading, historical_linguistics_analysts, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(orthographic_kernel__rupture_reading, post_reform_state_apparatus).
narrative_ontology:fixing_cost_class(orthographic_kernel__rupture_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: A single standardized national orthography unified administration, schooling, printing, signage, and typographic supply chains across the former empire's territory, replacing a layered court-and-clergy literacy regime with one mass-print standard aligned to international telegraph and linotype equipment.
% TRANSFER_FUNCTION: Moves interpretive authority over the written past from the pre-reform literate population and the Islamic-Ottoman textual tradition to the post-reform state apparatus, which becomes sole curator of the national archive and of what the next generation can read; secondarily moves literacy access to new cohorts in the new script.
% ABSENT_VOICES: The pre-reform literate classes held nominal seats in a single-party assembly where open dissent carried personal risk; the ulema had been organizationally dismantled in the four preceding years; Ottoman-script publishers at home and in the diaspora had no hearing; and the principal long-run bearers of the severance — people not yet born in 1928 — had no representation at all. They sit outside the assembly's discipline, in exile presses in Cairo and Beirut, and in the unborn.
% DISAPPEARANCE_RATIONALE: If the enforced exclusivity vanished overnight, dual-script publication would resume within months, families would commission transcriptions of abandoned correspondence, religious publishers would reprint the Ottoman Turkish corpus, and the state would lose its position as sole curator of the written past; the narrative monopoly the arrangement underwrites would dissolve.
% FOUNDING_PROBLEM: How a new republic could secure an identity discontinuous with the Ottoman-Islamic order it replaced, given that the population's high culture, archive, and script belonged to that order — answered, on this reading, by making the past literally unreadable.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the beneficiary set by contemporary diplomatic reporting (European and American embassy dispatches, 1928-1935, describing the reform as a conscious break with the Islamic past), by non-participant scholarship of the reform (Geoffrey Lewis's account of the reform as deliberate Westernizing rupture; Niyazi Berkes's critical sociology of Kemalist secularization), and by the memoir literature of the severed first generation. The state's own account attributes the reform to literacy and modernization and does not attest the rupture motive — the insider/outsider divergence is itself the kernel contest.
narrative_ontology:disappearance_verdict(orthographic_kernel__rupture_reading, world_rearranges).
narrative_ontology:founding_problem_status(orthographic_kernel__rupture_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(orthographic_kernel__rupture_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(orthographic_kernel__rupture_reading, 'none', 1).
narrative_ontology:epsilon_provenance(orthographic_kernel__rupture_reading, 0.84, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(orthographic_kernel__rupture_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(orthographic_kernel__rupture_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(orthographic_kernel__rupture_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Time mapping: t=0 is 1928, t=80 is 2008; all three series run on one shared grid ({0,5,10,20,30,40,55,70,80}) so every metric is authored at every examined point. Extractiveness (epsilon 0.84 at interval end): the referent is the standing arrangement assessed by this reading's own lights — near-total destruction of a population's textual continuity. The harm compounds (each cohort inherits less) but the marginal rate declined after the transitional cohort passed, hence the rise to a 0.92 peak at t=20 and slow decline thereafter as heritage programs and digitization partially widen access. Suppression (0.60): authored raw and unscaled — only extractiveness is scaled by the engine. The suppression_requirement series is authored because this story specifically traces enforcement-capacity change: a rapid ratchet (compulsory night schools, Arabic-script press bans, official literacy exams) peaking at t=5, then decay as compliance became self-sustaining — but decaying to a durable structural floor (~0.6), not to zero: official exclusivity, the school monopoly, and the pre-emptively demolished institutional base (medreses closed 1924, tekkes 1925) mean the alternative cannot regenerate without affirmative state action. Theater_ratio (0.55 at end): rose steadily as kinetic enforcement gave way to commemoration — annual celebrations of the 'script revolution,' later the state's staging of Ottoman heritage exhibitions celebrating what the rupture severed. Crossing 0.5 signals proxy-replacement drift, consistent with an arrangement whose active function ended and whose persistence is maintained as settled fact plus ceremony. Accessibility_collapse (0.8): alternatives (dual-script publication, gradual transition, regional script autonomy) were foreclosed by decree rather than outcompeted; residual channels (emigre presses in Cairo and Beirut, private familial use, liturgical Arabic) kept collapse short of total. Resistance (0.55): parliamentary dissent muzzled within single-party discipline, religious opposition preemptively crushed after 1925, widespread passive resistance (large adult cohorts never gained functional literacy in any script), and diaspora publishing persisting outside jurisdiction. Coalition note: the victim classes (ulema, calligraphers, literate burghers) overlapped socially and could in principle have coalitioned, but the single-party state had just demonstrated its tolerance for religious-political mobilization (Sheikh Said revolt, 1925; abortive Free Republican Party, 1930), closing the coalition window before the script law passed.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats and the agenda-setter seat compute radically different experiences of the same statute. From the state seat the arrangement is not a constraint at all but a sovereign achievement — it set the terms, bears almost none of the cost, and holds arbitrage-grade exit (it can relax or tighten at will, as it did in the 1950s). From the trapped payer seats the same structure is total dispossession: their accumulated cultural capital was rendered illegible by decree. The descendant seat shows partial identity fusion: formed as Latin-script Turks, learning Ottoman script carries social cost (it reads as Islamist nostalgia), so exit is constrained partly by an internalized frame rather than external barriers alone; if that frame broke — heritage normalization, mass transcription movements — the seat's exit widens and its computed extraction falls. Same-level dynamics: pre_reform_literate_population and post_reform_generations share moderate power but differ in exit (trapped vs constrained) and role composition (pure payer vs payer-with-incidental-gain), so the engine derives different directionalities for them despite equal nominal standing.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary/victim declarations drive the derivation. post_reform_state_apparatus (agenda_setter + beneficiary, arbitrage exit) sits nearest the beneficiary pole — the arrangement subsidizes it with narrative sovereignty. kemalist_republican_intelligentsia (beneficiary, mobile) sits low-d: it collected a cleared cultural field. pre_reform_literate_population (payer, trapped) and ottoman_calligraphers_and_scribes (payer, trapped, powerless) sit near the full-target end — trapped or identity-locked targets amplify effective extraction. islamic_scholarly_establishment (payer, trapped, organized) sits high-d. post_reform_generations (payer + beneficiary, constrained) derive mid-high: the derivation weighs the literacy gain against the severance loss. This reading holds the loss dominant, but no directionality_override is authored: the override mechanism is keyed by power atom, and the only other moderate-power seat (pre_reform_literate_population) needs a HIGHER d than the descendant seat — a moderate-atom override would misfire against the primary target. The granularity limit is recorded instead as the descendant_victim_set_boundary omega. arabic_script_diaspora_press (excluded, mobile) sits outside the extraction circuit; historical_linguistics_analysts are analytical.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (manufacturing discontinuity with the Ottoman-Islamic order) is dead: the rupture is complete and irreversible — the transmission chain of Arabic-script Turkish literacy broke within one generation and cannot be cheaply rebuilt (fixing_cost prohibitive). Yet disappearance_verdict is world_rearranges: arrangements still depend on the statute's exclusivity — archive gatekeeping, curriculum control, the credentialed intermediary class that mediates all access to the pre-1928 written world. The status=dead x verdict=world_rearranges mismatch is the capture/zombie signature: the arrangement outlived its mandate and now serves narrative-sovereignty rents for the seat named in gain_flow. Mandatrophy resolution prevents mislabeling in both directions: the modernization reading would mislabel the arrangement as coordination achieved (rope-flavored); a pure-coercion reading would miss that the enforcement machinery has largely decayed into ceremony (rising theater_ratio, falling suppression_requirement). The arrangement persists not by ongoing coercion but by the irreversibility it manufactured — extraction that succeeded so thoroughly its enforcement became optional. Classification as snare with decaying suppression and rising theater locates it correctly.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sibling_reading_epsilon_divergence,
    'This story instantiates the rupture_reading of orthographic_kernel; what epsilon and victim structure do continuity_reading and modernization_reading author for the same 1928 arrangement?',
    'Compile and compare the three sibling stories'' computed per-seat classifications; locate the disagreement at motive attribution and victim-set boundaries rather than at the statute''s observable operation.',
    'Continuity_reading likely authors high epsilon with the textual tradition itself as the harmed party; modernization_reading likely authors lower epsilon with a net-beneficiary structure (tangled_rope or rope). The same statute thus ranges across types depending on reading — the corpus measures this divergence rather than averaging it away.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_reading_epsilon_divergence, conceptual, 'Committer-frame routing: one kernel, three readings, three constraints with divergent epsilon.').

omega_variable(
    rupture_primacy_vs_modernization_motive,
    'Was cultural severance the primary intended function of the script change, or a foreseeable side effect of efficiency-driven modernization?',
    'Deliberation records, private papers (Ataturk''s notes, Education Ministry minutes), and sequence analysis: why 1928, why total substitution rather than gradual transition, why bans on dual-script publication rather than laissez-faire competition between orthographies.',
    'If rupture-primary, the snare claim stands; if efficiency-primary with rupture as accepted instrument, the arrangement computes as tangled_rope (genuine coordination function plus asymmetric extraction) and epsilon drops materially.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(rupture_primacy_vs_modernization_motive, conceptual, 'The motive-attribution dispute separating this reading from the modernization sibling.').

omega_variable(
    severance_irreversibility,
    'Is the cultural-continuity loss reversible through heritage programs, digitization, and mass Ottoman-Turkish education, or structurally permanent?',
    'Track specialist-literacy rates, archive access volumes, curriculum changes, and transcription-economy growth over coming decades.',
    'Partial reversibility pushes late-interval epsilon below 0.84 and eventually softens fixing_cost; permanence pins epsilon high and keeps fixing_cost prohibitive, sustaining the capture/zombie mismatch signature.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(severance_irreversibility, empirical, 'Whether the manufactured amnesia can be undone at acceptable cost.').

omega_variable(
    descendant_victim_set_boundary,
    'Does the victim set include post-reform generations severed without their consent, or only the contemporaries who experienced the loss?',
    'Conceptual: depends on whether the loss is defined as experienced dispossession (transitional cohort only) or as denied access to an inheritance one never knew (all subsequent cohorts). The directionality derivation cannot weigh this asymmetry for the descendant seat; a power-atom-keyed override would misfire against the equally moderate pre-reform seat, so the boundary is left to this omega.',
    'Inclusion pushes the descendant seat''s directionality toward the full-target end and widens the victim set beyond living memory; exclusion confines epsilon to the transitional cohort and lowers aggregate extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(descendant_victim_set_boundary, conceptual, 'Boundary of the victim set across generations.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(orthographic_kernel__rupture_reading, 0, 80).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(rupture_reading_tr_t0, orthographic_kernel__rupture_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement_basis(rupture_reading_tr_t0, observed).
narrative_ontology:measurement(rupture_reading_tr_t5, orthographic_kernel__rupture_reading, theater_ratio, 5, 0.18).
narrative_ontology:measurement_basis(rupture_reading_tr_t5, observed).
narrative_ontology:measurement(rupture_reading_tr_t10, orthographic_kernel__rupture_reading, theater_ratio, 10, 0.24).
narrative_ontology:measurement_basis(rupture_reading_tr_t10, observed).
narrative_ontology:measurement(rupture_reading_tr_t20, orthographic_kernel__rupture_reading, theater_ratio, 20, 0.3).
narrative_ontology:measurement_basis(rupture_reading_tr_t20, observed).
narrative_ontology:measurement(rupture_reading_tr_t30, orthographic_kernel__rupture_reading, theater_ratio, 30, 0.36).
narrative_ontology:measurement_basis(rupture_reading_tr_t30, observed).
narrative_ontology:measurement(rupture_reading_tr_t40, orthographic_kernel__rupture_reading, theater_ratio, 40, 0.41).
narrative_ontology:measurement_basis(rupture_reading_tr_t40, observed).
narrative_ontology:measurement(rupture_reading_tr_t55, orthographic_kernel__rupture_reading, theater_ratio, 55, 0.46).
narrative_ontology:measurement_basis(rupture_reading_tr_t55, observed).
narrative_ontology:measurement(rupture_reading_tr_t70, orthographic_kernel__rupture_reading, theater_ratio, 70, 0.51).
narrative_ontology:measurement_basis(rupture_reading_tr_t70, observed).
narrative_ontology:measurement(rupture_reading_tr_t80, orthographic_kernel__rupture_reading, theater_ratio, 80, 0.55).
narrative_ontology:measurement_basis(rupture_reading_tr_t80, observed).

% Extraction over time
narrative_ontology:measurement(rupture_reading_be_t0, orthographic_kernel__rupture_reading, base_extractiveness, 0, 0.78).
narrative_ontology:measurement_basis(rupture_reading_be_t0, observed).
narrative_ontology:measurement(rupture_reading_be_t5, orthographic_kernel__rupture_reading, base_extractiveness, 5, 0.86).
narrative_ontology:measurement_basis(rupture_reading_be_t5, observed).
narrative_ontology:measurement(rupture_reading_be_t10, orthographic_kernel__rupture_reading, base_extractiveness, 10, 0.9).
narrative_ontology:measurement_basis(rupture_reading_be_t10, observed).
narrative_ontology:measurement(rupture_reading_be_t20, orthographic_kernel__rupture_reading, base_extractiveness, 20, 0.92).
narrative_ontology:measurement_basis(rupture_reading_be_t20, observed).
narrative_ontology:measurement(rupture_reading_be_t30, orthographic_kernel__rupture_reading, base_extractiveness, 30, 0.91).
narrative_ontology:measurement_basis(rupture_reading_be_t30, observed).
narrative_ontology:measurement(rupture_reading_be_t40, orthographic_kernel__rupture_reading, base_extractiveness, 40, 0.89).
narrative_ontology:measurement_basis(rupture_reading_be_t40, observed).
narrative_ontology:measurement(rupture_reading_be_t55, orthographic_kernel__rupture_reading, base_extractiveness, 55, 0.87).
narrative_ontology:measurement_basis(rupture_reading_be_t55, observed).
narrative_ontology:measurement(rupture_reading_be_t70, orthographic_kernel__rupture_reading, base_extractiveness, 70, 0.85).
narrative_ontology:measurement_basis(rupture_reading_be_t70, observed).
narrative_ontology:measurement(rupture_reading_be_t80, orthographic_kernel__rupture_reading, base_extractiveness, 80, 0.84).
narrative_ontology:measurement_basis(rupture_reading_be_t80, observed).

% Suppression requirement over time
narrative_ontology:measurement(rupture_reading_su_t0, orthographic_kernel__rupture_reading, suppression_requirement, 0, 0.72).
narrative_ontology:measurement_basis(rupture_reading_su_t0, observed).
narrative_ontology:measurement(rupture_reading_su_t5, orthographic_kernel__rupture_reading, suppression_requirement, 5, 0.88).
narrative_ontology:measurement_basis(rupture_reading_su_t5, observed).
narrative_ontology:measurement(rupture_reading_su_t10, orthographic_kernel__rupture_reading, suppression_requirement, 10, 0.85).
narrative_ontology:measurement_basis(rupture_reading_su_t10, observed).
narrative_ontology:measurement(rupture_reading_su_t20, orthographic_kernel__rupture_reading, suppression_requirement, 20, 0.76).
narrative_ontology:measurement_basis(rupture_reading_su_t20, observed).
narrative_ontology:measurement(rupture_reading_su_t30, orthographic_kernel__rupture_reading, suppression_requirement, 30, 0.68).
narrative_ontology:measurement_basis(rupture_reading_su_t30, observed).
narrative_ontology:measurement(rupture_reading_su_t40, orthographic_kernel__rupture_reading, suppression_requirement, 40, 0.64).
narrative_ontology:measurement_basis(rupture_reading_su_t40, observed).
narrative_ontology:measurement(rupture_reading_su_t55, orthographic_kernel__rupture_reading, suppression_requirement, 55, 0.62).
narrative_ontology:measurement_basis(rupture_reading_su_t55, observed).
narrative_ontology:measurement(rupture_reading_su_t70, orthographic_kernel__rupture_reading, suppression_requirement, 70, 0.61).
narrative_ontology:measurement_basis(rupture_reading_su_t70, observed).
narrative_ontology:measurement(rupture_reading_su_t80, orthographic_kernel__rupture_reading, suppression_requirement, 80, 0.6).
narrative_ontology:measurement_basis(rupture_reading_su_t80, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(orthographic_kernel__rupture_reading, identity_coordination).
narrative_ontology:affects_constraint(orthographic_kernel__rupture_reading, orthographic_kernel__continuity_reading).
narrative_ontology:affects_constraint(orthographic_kernel__rupture_reading, orthographic_kernel__modernization_reading).

% DUAL FORMULATION NOTE:
% Decomposition of the colloquial label 'the Turkish script reform' into three epsilon-invariant readings of one kernel (orthographic_kernel), per the BGS pattern. The single statute supports three structurally distinct claims: continuity (what was lost), modernization (what was gained), rupture (what was intended). Each reading gets its own epsilon, beneficiaries, victims, and classification; this file is the rupture instantiation. Family links run through affects_constraints in all three files. Direction nuance: the rupture reading is downstream of the continuity reading (its motive claim cites the continuity reading's loss assessment as evidence) and exerts historiographic pressure on the modernization reading without foreclosing it.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
