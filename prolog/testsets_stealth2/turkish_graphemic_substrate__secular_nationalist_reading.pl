% ============================================================================
% CONSTRAINT STORY: turkish_graphemic_substrate__secular_nationalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_turkish_graphemic_substrate__secular_nationalist_reading, []).

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
 *   constraint_id: turkish_graphemic_substrate__secular_nationalist_reading
 *   human_readable: Secular-Nationalist Latin Graphemic Settlement (1928 Alphabet Law reading)
 *   domain: political linguistics / state formation / cultural engineering
 *
 * SUMMARY:
 *   One reading of the contested kernel turkish_graphemic_substrate. The
 *   secular-nationalist reading holds that Turkish linguistic identity is
 *   discontinuous with the Ottoman-Islamic past and that the Latin alphabet
 *   is the legitimate graphemic substrate aligned with European modernity;
 *   instantiated historically as the 1928 Alphabet Law (No. 1353), the Millet
 *   Mektepleri literacy campaigns, bans on Arabic-script publication, and the
 *   curricular monopoly that followed. Per the epsilon-invariance principle
 *   this file authors ONLY this reading: the standing arrangement under
 *   contest is the enforced Latin settlement, and epsilon is authored for
 *   that arrangement as this analysis sees it. The sibling readings are
 *   separate stories —
 *   turkish_graphemic_substrate__ottoman_continuity_reading (no rupture
 *   enforcement; clerical capital preserved; inverted victim polarity) and
 *   turkish_graphemic_substrate__gradual_transition_reading (scheduled
 *   dual-script coexistence; sunset-bounded) — linked through
 *   network.affects_constraints. Claim/metric independence is deliberate: the
 *   reading CLAIMS a civilizational-necessity settlement, while the authored
 *   metrics describe an arrangement with a real coordination core and
 *   substantial, unevenly distributed costs; the engine computes per-seat
 *   verdicts from the structural data, and divergence between claim and
 *   computed type is the datum, not an error.
 *
 * KEY AGENTS:
 *   - republican_state_cultural_engineering_apparatus: Agenda setter (institutional/arbitrage) — authors and enforces the script settlement; captures administrative legibility, narrative authority, and territorial uniformity
 *   - secular_republican_intelligentsia: Primary beneficiary (organized/constrained) — careers, status, and opportunity denominated in the new script
 *   - latin_script_printing_education_sector: Secondary beneficiary (organized/mobile) — mandated-demand markets in type, textbooks, and schooling
 *   - rural_adult_population: Payer with incidental gain (powerless/constrained) — bore the relearning levy; received first-literacy access
 *   - ottoman_script_literate_classes: Concentrated elite target (moderate/identity_locked) — script-specific cultural capital rendered worthless in public life
 *   - islamic_clerical_establishment: Target (organized/trapped) — severed from its textual transmission chain
 *   - kurdish_minority_communities: Concentrated target (powerless/trapped, regional scope) — assimilation enforcement fell hardest here
 *   - gradualist_faction_within_elite: Excluded voice (moderate/constrained) — managed-transition proposal outmaneuvered before the 1928 vote
 *   - ottomanist_diaspora_scholars: Excluded voice (moderate/mobile, continental scope) — preserve the severed archive from abroad
 *   - foreign_modernization_observers: Analytical observer (institutional/analytical) — benchmarked alignment and fed prestige and credit; bore no costs
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(turkish_graphemic_substrate__secular_nationalist_reading, 0.58).
domain_priors:suppression_score(turkish_graphemic_substrate__secular_nationalist_reading, 0.34).
domain_priors:theater_ratio(turkish_graphemic_substrate__secular_nationalist_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(turkish_graphemic_substrate__secular_nationalist_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(turkish_graphemic_substrate__secular_nationalist_reading, suppression_requirement, 0.34).
narrative_ontology:constraint_metric(turkish_graphemic_substrate__secular_nationalist_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(turkish_graphemic_substrate__secular_nationalist_reading, accessibility_collapse, 0.66).
narrative_ontology:constraint_metric(turkish_graphemic_substrate__secular_nationalist_reading, resistance, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(turkish_graphemic_substrate__secular_nationalist_reading, tangled_rope).
narrative_ontology:human_readable(turkish_graphemic_substrate__secular_nationalist_reading, "Secular-Nationalist Latin Graphemic Settlement (1928 Alphabet Law reading)").
narrative_ontology:topic_domain(turkish_graphemic_substrate__secular_nationalist_reading, "political linguistics / state formation / cultural engineering").

domain_priors:requires_active_enforcement(turkish_graphemic_substrate__secular_nationalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(turkish_graphemic_substrate__secular_nationalist_reading, '8e96a732-a52f-4704-b4a3-95719a1ef8c4').
narrative_ontology:cs_kernel_codification('8e96a732-a52f-4704-b4a3-95719a1ef8c4', formalized).
narrative_ontology:cs_authority_grounding('8e96a732-a52f-4704-b4a3-95719a1ef8c4', lineage).
narrative_ontology:cs_interpretation_layer_present('8e96a732-a52f-4704-b4a3-95719a1ef8c4').
narrative_ontology:cs_reading_relation('8e96a732-a52f-4704-b4a3-95719a1ef8c4', turkish_graphemic_substrate__ottoman_continuity_reading, forecloses).
narrative_ontology:cs_reading_relation('8e96a732-a52f-4704-b4a3-95719a1ef8c4', turkish_graphemic_substrate__gradual_transition_reading, influences).
narrative_ontology:cs_axiom('8e96a732-a52f-4704-b4a3-95719a1ef8c4', foundational, identity_constituted_by_rupture_from_ottoman_islamic_past).
narrative_ontology:cs_axiom_status(identity_constituted_by_rupture_from_ottoman_islamic_past, holdable).
narrative_ontology:cs_axiom_grounding('8e96a732-a52f-4704-b4a3-95719a1ef8c4', identity_constituted_by_rupture_from_ottoman_islamic_past, instrumental).
narrative_ontology:cs_axiom('8e96a732-a52f-4704-b4a3-95719a1ef8c4', foundational, latin_script_as_graphemic_legitimacy_standard).
narrative_ontology:cs_axiom_status(latin_script_as_graphemic_legitimacy_standard, holdable).
narrative_ontology:cs_axiom_grounding('8e96a732-a52f-4704-b4a3-95719a1ef8c4', latin_script_as_graphemic_legitimacy_standard, empirically_contingent).
narrative_ontology:cs_reference_frame('8e96a732-a52f-4704-b4a3-95719a1ef8c4', latin_script_foundational_settlement).
narrative_ontology:cs_drift_state('8e96a732-a52f-4704-b4a3-95719a1ef8c4', contemporary_neo_ottoman_contestation, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('8e96a732-a52f-4704-b4a3-95719a1ef8c4', '').
narrative_ontology:cs_kernel_id(turkish_graphemic_substrate__secular_nationalist_reading, turkish_graphemic_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(turkish_graphemic_substrate__secular_nationalist_reading, republican_state_cultural_engineering_apparatus).
narrative_ontology:constraint_beneficiary(turkish_graphemic_substrate__secular_nationalist_reading, secular_republican_intelligentsia).
narrative_ontology:constraint_beneficiary(turkish_graphemic_substrate__secular_nationalist_reading, latin_script_printing_education_sector).
narrative_ontology:constraint_victim(turkish_graphemic_substrate__secular_nationalist_reading, ottoman_script_literate_classes).
narrative_ontology:constraint_victim(turkish_graphemic_substrate__secular_nationalist_reading, islamic_clerical_establishment).
narrative_ontology:constraint_victim(turkish_graphemic_substrate__secular_nationalist_reading, kurdish_minority_communities).
narrative_ontology:constraint_victim(turkish_graphemic_substrate__secular_nationalist_reading, rural_adult_population).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(turkish_graphemic_substrate__secular_nationalist_reading, rural_adult_population).
narrative_ontology:constraint_vindicates(turkish_graphemic_substrate__secular_nationalist_reading, kemalist_civilizational_rupture_doctrine).
narrative_ontology:constraint_vindicates(turkish_graphemic_substrate__secular_nationalist_reading, phonetic_alphabet_superiority_for_turkish).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Writes and enforces the alphabet law, runs the national literacy campaigns, controls school curricula and printing licenses, and polices public use of the banned script. Gains administrative legibility, a uniform written medium for army, courts, and bureaucracy, and authority over which past remains publicly readable. Can adjust enforcement intensity, exemptions, and curricula at will; its own exposure to the rules it sets is minimal.
narrative_ontology:constraint_stakeholder(turkish_graphemic_substrate__secular_nationalist_reading, republican_state_cultural_engineering_apparatus, agenda_setter,
    institutional, generational, arbitrage, national).

% The lawyers, teachers, journalists, and officials who staffed the new order. Their skills, careers, and social standing are denominated in the new script and the new national narrative; status and opportunity flow to them from the settlement. Leaving it would mean forfeiting the positions the settlement distributes.
narrative_ontology:constraint_stakeholder(turkish_graphemic_substrate__secular_nationalist_reading, secular_republican_intelligentsia, beneficiary,
    organized, biographical, constrained, national).

% Type foundries, textbook publishers, private schools, and later software-localization firms whose markets exist because the official written medium changed. They invest in Latin-script infrastructure and sell into mandated demand. Their capital is portable across markets and could serve other countries' systems.
narrative_ontology:constraint_stakeholder(turkish_graphemic_substrate__secular_nationalist_reading, latin_script_printing_education_sector, beneficiary,
    organized, biographical, mobile, national).

% Adults in towns and villages who had to attend evening literacy courses or remain unable to read official notices, contracts, and newspapers. Those already literate in the old script watched that skill lose public value; those unschooled gained a first script that instructors described as faster to learn. Leaving means emigrating or withdrawing from official life.
narrative_ontology:constraint_stakeholder(turkish_graphemic_substrate__secular_nationalist_reading, rural_adult_population, payer,
    powerless, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(turkish_graphemic_substrate__secular_nationalist_reading, rural_adult_population, beneficiary).

% Civil servants, poets, historians, calligraphers, and merchants trained in the old script, whose professional standing, libraries, and lifetime reading were denominated in it. After 1928 their skill carried no public worth; many retrained, many withdrew. Their sense of vocation and lineage is bound to the script tradition, so setting it aside feels like setting themselves aside.
narrative_ontology:constraint_stakeholder(turkish_graphemic_substrate__secular_nationalist_reading, ottoman_script_literate_classes, payer,
    moderate, biographical, identity_locked, national).

% Prayer leaders, jurisprudents, and Sufi orders whose authority runs through Arabic and Ottoman textual transmission. The new official script cuts the day-to-day written link between pulpit and state and, with later restrictions, walls off the seminary curriculum. Their institutions cannot relocate a textual tradition accumulated over centuries; their horizon spans generations of students.
narrative_ontology:constraint_stakeholder(turkish_graphemic_substrate__secular_nationalist_reading, islamic_clerical_establishment, payer,
    organized, generational, trapped, national).

% Speakers of Kurdish and Zaza in the southeast, whose own oral and written traditions were targeted alongside the script change: the new official alphabet served Turkish only, and speaking or publishing in minority languages drew legal penalty for decades. Schooling, courts, and administration arrived exclusively in the official medium. Exit meant crossing borders or clandestine transmission at home.
narrative_ontology:constraint_stakeholder(turkish_graphemic_substrate__secular_nationalist_reading, kurdish_minority_communities, payer,
    powerless, generational, trapped, regional).

% Officials and educators who argued in 1928 for a multi-year dual-script transition with phased replacement of signage, records, and schooling. Outmaneuvered in the party congress and the assembly vote, they were sidelined from cultural policy; their proposal survives in meeting minutes and memoirs.
narrative_ontology:constraint_stakeholder(turkish_graphemic_substrate__secular_nationalist_reading, gradualist_faction_within_elite, excluded,
    moderate, biographical, constrained, national).

% Historians, archivists, and philologists — many emigrated or working from European and American universities — who kept the old script's scholarship alive outside Turkey. They argue the severance broke ordinary citizens' access to five centuries of their own records. Free to publish abroad, they hold no seat in domestic cultural policy.
narrative_ontology:constraint_stakeholder(turkish_graphemic_substrate__secular_nationalist_reading, ottomanist_diaspora_scholars, excluded,
    moderate, civilizational, mobile, continental).

% European diplomats, advisers, and later comparative scholars who recorded the reform's reception, benchmarked literacy statistics, and assessed Turkey's alignment with European institutions. Their assessments fed loans, treaties, and prestige, but they bear none of the arrangement's costs.
narrative_ontology:constraint_stakeholder(turkish_graphemic_substrate__secular_nationalist_reading, foreign_modernization_observers, observer,
    institutional, generational, analytical, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(turkish_graphemic_substrate__secular_nationalist_reading, republican_state_cultural_engineering_apparatus).
narrative_ontology:fixing_cost_class(turkish_graphemic_substrate__secular_nationalist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes one official written medium for schooling, administration, courts, army, and press across the national territory; replaces a script poorly matched to Turkish vowel structure with a phonetic alphabet that shortens literacy instruction; gives printers, teachers, and clerks a single standard instead of parallel scribal traditions.
% TRANSFER_FUNCTION: Moves cultural capital and public communicative legitimacy from Ottoman-script literates to Latin-script literates and to the state; renders five centuries of archival and literary production inaccessible to untrained readers; moves symbolic alignment and diplomatic credibility toward European capitals; in minority regions, moves linguistic public space from local languages to official Turkish.
% ABSENT_VOICES: The gradualist faction inside the ruling party (phased dual-script transition) was outmaneuvered before the law passed; clerical authorities and Ottoman-script professionals objected but lost press access and assembly votes as the single-party regime consolidated; Kurdish speakers were never consulted and their own script traditions were suppressed outright; elderly Ottomans who could not relearn had no channel. Unanimity behind the new substrate arose partly because dissenting seats had been removed from the room.
% DISAPPEARANCE_RATIONALE: Schooling, bureaucracy, signage, publishing, and now software localization all presuppose the Latin substrate; overnight removal would strand the literate population, force a second mass retraining, reopen the question of which past is publicly readable, and unravel the identity settlement built on the 1928 rupture.
% FOUNDING_PROBLEM: In the early republic's diagnosis: an imperial script poorly fitted to Turkish phonology, roughly one-in-ten literacy, a written culture anchored in Islamic-Ottoman transmission, and a new state seeking rapid differentiation from that inheritance and recognition from European powers.
% FOUNDING_PROBLEM_CORROBORATION: The technical half is corroborated from outside the benefiting parties: contemporaneous literacy censuses, European educational surveys, and later comparative pedagogy attest both the low baseline and the phonetic mismatch. The civilizational-rupture half is attested mainly by the reformers' own speeches and Kemalist historiography; Ottomanist historians, clerical voices, and today's neo-Ottoman currents dispute that rupture was necessary or that it succeeded on its own terms — no fully disinterested corroboration exists for that half, and that absence is itself signal.
narrative_ontology:disappearance_verdict(turkish_graphemic_substrate__secular_nationalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(turkish_graphemic_substrate__secular_nationalist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(turkish_graphemic_substrate__secular_nationalist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(turkish_graphemic_substrate__secular_nationalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(turkish_graphemic_substrate__secular_nationalist_reading, 0.58, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(turkish_graphemic_substrate__secular_nationalist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(turkish_graphemic_substrate__secular_nationalist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(turkish_graphemic_substrate__secular_nationalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.58 at interval end: the settlement transferred script-specific cultural capital from one elite to another, sealed five centuries of records behind a specialist wall for the untrained, and in the southeast operated as an instrument of forced linguistic assimilation — against which stands the largest literacy expansion in the region's recorded history, delivered through an alphabet genuinely better matched to Turkish phonology. Suppression 0.34 at end-state: enforcement machinery (publication bans, campaign compulsion, licensing) peaked at t0 and decayed as compliance normalized, with a visible re-intensification through the 1980s-1990s when script and language enforcement fused in the southeast. Theater 0.38: the daily function is total — every written transaction runs on the substrate — but a growing share of activity around the settlement is commemorative and defensive (alphabet anniversaries, curriculum battles, speech-and-statue politics), proxy conflict over a function long since routinized. Accessibility collapse 0.66: inside the territory, alternatives (dual signage, old-script press, old-script schooling) were closed by law and market; residual niches survive in seminaries, diaspora publishing, and specialized philology. Resistance 0.48: passive noncompliance by the old-literate generation, clerical objection, and the early-1930s reaction complex — real, punished, ultimately marginal. Coordination type: identity_coordination, because the dominant function whose failure would dissolve the arrangement is boundary maintenance of national identity against the Ottoman-Islamic inheritance; the encoding-standard layer is real but subordinate, and the standard FNL alert applies — the identity frame here coincides with coupling that concentrates burdens on powerless agents at regional scope, which the complexity offset does not excuse. The three temporal series share one grid (t = 0,12,24,36,50,65,80,97 on a 1928-2025 span); the extractiveness curve is deliberately non-monotonic — decline through consolidation, a rise through the repression era, a plateau under contemporary contestation — and the oscillation is documented rather than smoothed.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter seat the settlement is a founding act of national rescue: the same structure that clerical and old-literate seats experience as expropriation of a life's capital is, from the ministry, the price of a nation entering modernity on schedule. The rural seat computes both faces at once — a tax paid in evening classes and unread inheritances, a subsidy received as first literacy. The Kurdish seat experiences a third structure again: not a shared standard but an instrument of erasure. These are not value disagreements laid over one arrangement; they are different computed verdicts from different positions in the same structure, which is what the per-seat engine output registers.
 *
 * DIRECTIONALITY LOGIC:
 *   The state apparatus sits nearest the beneficiary pole: it collects legibility, narrative authority, and administrative uniformity while bearing almost none of the settlement's costs. The intelligentsia and the printing-education sector collect distributed benefits with constrained or mobile exits. The old-literate classes and the clerical establishment sit nearest the target pole — their losses are concentrated, script-specific, and in the clerical case institutionally inescapable; vocational identity binds the first seat, textual entrapment binds the second. The Kurdish seat is the concentrated target: powerless, trapped, regional scope, and the harshest enforcement in the system. The rural adult seat is genuinely mixed — declared among victims for the transition levy it paid, carrying a secondary beneficiary position for the literacy it gained — and the per-seat derivation should place it mid-spectrum rather than at either pole; no directionality override is authored because the override surface is keyed by power atom and would misfire across the distinct powerless seats. Larger spatial scope amplifies effective burden for the targets: verifying compliance across a national territory favored the center over peripheral resisters.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem had two halves with different lifecycles. The technical half — low literacy, phonetic mismatch — was substantially solved within two generations, and the arrangement's original justification accordingly atrophied. The civilizational half — identity reorientation away from the Ottoman-Islamic inheritance — never died; it is the live stake of contemporary politics (neo-Ottoman revisionism, curriculum fights, archive politics). Authoring founding_problem_status as contested rather than dead is what keeps this story out of a false zombie reading: the arrangement is not maintained theatrically over a corpse of function — its function is total and daily — nor is its mandate spent, since the identity settlement it embodies is precisely what is being fought over. The tangled-rope claim is what blocks the two symmetric mislabels: reading the settlement as pure coordination erases the expropriated seats; reading it as pure extraction erases the literacy dividend every subsequent generation collected.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_commitment_location,
    'This story instantiates the secular_nationalist_reading of kernel turkish_graphemic_substrate; what structurally changes if a sibling reading is instantiated instead?',
    'Compile the sibling stories and diff the structural surfaces: victim sets (the continuity reading preserves clerical and old-literate capital and inverts the polarity; the gradual reading carries a sunset clause and no expropriated-class victims), enforcement profiles, and each reading''s authored epsilon over its own standing arrangement.',
    'Under the continuity reading the extraction asymmetry relocates to the Latin-aligned modernizers; under the gradual reading the enforcement machinery converts into scheduled transition cost. Per-seat verdicts computed from this file are valid only for this reading''s arrangement.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_commitment_location, conceptual, 'Committer-frame routing: which reading of the graphemic-substrate kernel this constraint is, and what siblings would alter.').

omega_variable(
    rupture_counterfactual_literacy,
    'Did coerced rupture accelerate mass literacy beyond what a managed dual-script transition would have achieved?',
    'Comparative analysis of late-modernizing states that changed script with versus without coercive severance (Soviet Turkic republics'' Latinization and later Cyrillicization; demographic modeling of Turkish literacy curves against gradual-transition baselines proposed in the 1928 assembly debates).',
    'If gradual paths reach comparable literacy, the enforcement component reads as extraction riding a coordination core and per-seat verdicts shift toward the extractive pole; if rupture was decisive, the same enforcement reads as transition cost and verdicts shift toward the coordination pole.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(rupture_counterfactual_literacy, empirical, 'Whether the settlement''s coercive element was functionally necessary to its celebrated literacy outcome.').

omega_variable(
    heritage_access_irreversibility,
    'Is the sealing of the Ottoman-script archive behind a paleography wall a permanent levy on all subsequent generations, or a recoverable cost given digitization and transliteration?',
    'Track archive-accessibility indicators: OCR and transliteration coverage of Ottoman corpora, machine-translation fidelity for the old script, enrollment in paleography and archival-training programs.',
    'A permanent-loss reading raises the effective burden on every citizen seat and widens the gap between the settlement''s coordination claim and its distributional record; a recoverable reading confines the levy to the transition generations.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(heritage_access_irreversibility, empirical, 'Permanence of the heritage-access component of the settlement''s costs.').

omega_variable(
    minority_extraction_coupling,
    'Is the burden borne by Kurdish-speaking communities intrinsic to this reading''s homogenization commitment, or contingent on separately legislated language policy coupled to the script regime?',
    'Within-reading variation: compare enforcement of the official-medium monopoly in Turkish-majority versus Kurdish-majority provinces; isolate periods and places where script enforcement operated without mother-tongue suppression.',
    'If intrinsic, no decomposition of this story relieves the minority victim set and the reading''s coordination claim narrows to the Turkish-speaking core; if contingent, a variant reading without assimilation enforcement would carry a materially different victim structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(minority_extraction_coupling, conceptual, 'Whether the concentrated minority burden belongs to this reading''s logic or to adjacent policy.').

omega_variable(
    modernity_alignment_necessity,
    'Is Latin-graphemic alignment a necessary concomitant of European modernity, or a contingent ideological identification available to be declined?',
    'Survey modernizing states that retained non-Latin scripts (Greece, Israel, Japan, South Korea) and achieved comparable institutional and economic outcomes.',
    'If alignment is necessary, the settlement''s coercive elements weigh as unavoidable transition cost; if contingent, they weigh as enforced ideology and the suppression record counts more heavily against the coordination claim.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(modernity_alignment_necessity, conceptual, 'Naturality of the European-alignment premise underlying the reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(turkish_graphemic_substrate__secular_nationalist_reading, 0, 97).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tgs_snr_tr_t0, turkish_graphemic_substrate__secular_nationalist_reading, theater_ratio, 0, 0.07).
narrative_ontology:measurement_basis(tgs_snr_tr_t0, observed).
narrative_ontology:measurement(tgs_snr_tr_t12, turkish_graphemic_substrate__secular_nationalist_reading, theater_ratio, 12, 0.09).
narrative_ontology:measurement_basis(tgs_snr_tr_t12, observed).
narrative_ontology:measurement(tgs_snr_tr_t24, turkish_graphemic_substrate__secular_nationalist_reading, theater_ratio, 24, 0.13).
narrative_ontology:measurement_basis(tgs_snr_tr_t24, observed).
narrative_ontology:measurement(tgs_snr_tr_t36, turkish_graphemic_substrate__secular_nationalist_reading, theater_ratio, 36, 0.17).
narrative_ontology:measurement_basis(tgs_snr_tr_t36, observed).
narrative_ontology:measurement(tgs_snr_tr_t50, turkish_graphemic_substrate__secular_nationalist_reading, theater_ratio, 50, 0.21).
narrative_ontology:measurement_basis(tgs_snr_tr_t50, observed).
narrative_ontology:measurement(tgs_snr_tr_t65, turkish_graphemic_substrate__secular_nationalist_reading, theater_ratio, 65, 0.25).
narrative_ontology:measurement_basis(tgs_snr_tr_t65, observed).
narrative_ontology:measurement(tgs_snr_tr_t80, turkish_graphemic_substrate__secular_nationalist_reading, theater_ratio, 80, 0.32).
narrative_ontology:measurement_basis(tgs_snr_tr_t80, observed).
narrative_ontology:measurement(tgs_snr_tr_t97, turkish_graphemic_substrate__secular_nationalist_reading, theater_ratio, 97, 0.38).
narrative_ontology:measurement_basis(tgs_snr_tr_t97, observed).

% Extraction over time
narrative_ontology:measurement(tgs_snr_be_t0, turkish_graphemic_substrate__secular_nationalist_reading, base_extractiveness, 0, 0.72).
narrative_ontology:measurement_basis(tgs_snr_be_t0, observed).
narrative_ontology:measurement(tgs_snr_be_t12, turkish_graphemic_substrate__secular_nationalist_reading, base_extractiveness, 12, 0.67).
narrative_ontology:measurement_basis(tgs_snr_be_t12, observed).
narrative_ontology:measurement(tgs_snr_be_t24, turkish_graphemic_substrate__secular_nationalist_reading, base_extractiveness, 24, 0.61).
narrative_ontology:measurement_basis(tgs_snr_be_t24, observed).
narrative_ontology:measurement(tgs_snr_be_t36, turkish_graphemic_substrate__secular_nationalist_reading, base_extractiveness, 36, 0.56).
narrative_ontology:measurement_basis(tgs_snr_be_t36, observed).
narrative_ontology:measurement(tgs_snr_be_t50, turkish_graphemic_substrate__secular_nationalist_reading, base_extractiveness, 50, 0.52).
narrative_ontology:measurement_basis(tgs_snr_be_t50, observed).
narrative_ontology:measurement(tgs_snr_be_t65, turkish_graphemic_substrate__secular_nationalist_reading, base_extractiveness, 65, 0.57).
narrative_ontology:measurement_basis(tgs_snr_be_t65, observed).
narrative_ontology:measurement(tgs_snr_be_t80, turkish_graphemic_substrate__secular_nationalist_reading, base_extractiveness, 80, 0.6).
narrative_ontology:measurement_basis(tgs_snr_be_t80, observed).
narrative_ontology:measurement(tgs_snr_be_t97, turkish_graphemic_substrate__secular_nationalist_reading, base_extractiveness, 97, 0.58).
narrative_ontology:measurement_basis(tgs_snr_be_t97, observed).

% Suppression requirement over time
narrative_ontology:measurement(tgs_snr_su_t0, turkish_graphemic_substrate__secular_nationalist_reading, suppression_requirement, 0, 0.78).
narrative_ontology:measurement_basis(tgs_snr_su_t0, observed).
narrative_ontology:measurement(tgs_snr_su_t12, turkish_graphemic_substrate__secular_nationalist_reading, suppression_requirement, 12, 0.64).
narrative_ontology:measurement_basis(tgs_snr_su_t12, observed).
narrative_ontology:measurement(tgs_snr_su_t24, turkish_graphemic_substrate__secular_nationalist_reading, suppression_requirement, 24, 0.52).
narrative_ontology:measurement_basis(tgs_snr_su_t24, observed).
narrative_ontology:measurement(tgs_snr_su_t36, turkish_graphemic_substrate__secular_nationalist_reading, suppression_requirement, 36, 0.44).
narrative_ontology:measurement_basis(tgs_snr_su_t36, observed).
narrative_ontology:measurement(tgs_snr_su_t50, turkish_graphemic_substrate__secular_nationalist_reading, suppression_requirement, 50, 0.36).
narrative_ontology:measurement_basis(tgs_snr_su_t50, observed).
narrative_ontology:measurement(tgs_snr_su_t65, turkish_graphemic_substrate__secular_nationalist_reading, suppression_requirement, 65, 0.46).
narrative_ontology:measurement_basis(tgs_snr_su_t65, observed).
narrative_ontology:measurement(tgs_snr_su_t80, turkish_graphemic_substrate__secular_nationalist_reading, suppression_requirement, 80, 0.39).
narrative_ontology:measurement_basis(tgs_snr_su_t80, observed).
narrative_ontology:measurement(tgs_snr_su_t97, turkish_graphemic_substrate__secular_nationalist_reading, suppression_requirement, 97, 0.34).
narrative_ontology:measurement_basis(tgs_snr_su_t97, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(turkish_graphemic_substrate__secular_nationalist_reading, identity_coordination).
narrative_ontology:affects_constraint(turkish_graphemic_substrate__secular_nationalist_reading, turkish_graphemic_substrate__ottoman_continuity_reading).
narrative_ontology:affects_constraint(turkish_graphemic_substrate__secular_nationalist_reading, turkish_graphemic_substrate__gradual_transition_reading).
narrative_ontology:affects_constraint(turkish_graphemic_substrate__secular_nationalist_reading, turkish_language_purification_movement).
narrative_ontology:affects_constraint(turkish_graphemic_substrate__secular_nationalist_reading, kurdish_linguistic_suppression_regime).
narrative_ontology:affects_constraint(turkish_graphemic_substrate__secular_nationalist_reading, kemalist_laiklik_settlement).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'the Turkish script reform' covers three structurally distinct claims that the kernel decomposition separates. This story authors the secular_nationalist_reading — the enforced Latin settlement with rupture enforcement — and carries the family's highest epsilon because it alone contains the expropriation and assimilation enforcement. turkish_graphemic_substrate__gradual_transition_reading authors the counterfactual managed-transition arrangement (sunset-bounded, lower suppression, no expropriated-class victim set). turkish_graphemic_substrate__ottoman_continuity_reading authors the continuity arrangement (no rupture enforcement; the victim/beneficiary polarity inverts). Downstream edges run to the purification movement (the same engineering logic applied to vocabulary), the Kurdish linguistic-suppression regime (the assimilation enforcement this reading's homogenization delta licensed), and the laiklik settlement (the broader secularization architecture this reading operationalizes in the graphic domain). Citation flow follows the enacted reading: the settlement's celebrated outcomes were cited as evidence for the wider engineering program.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
