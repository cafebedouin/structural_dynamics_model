% ============================================================================
% CONSTRAINT STORY: script_as_identity__ottoman_continuity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_script_as_identity__ottoman_continuity_reading, []).

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
 *   constraint_id: script_as_identity__ottoman_continuity_reading
 *   human_readable: Arabic Script as Constitutive of Turkish-Islamic Identity (Ottoman Continuity Reading)
 *   domain: comparative linguistics/political authority/state-building
 *
 * SUMMARY:
 *   Within the Ottoman order (interval 1839-1928), the maintenance of Arabic
 *   script as the sole legitimate script for Turkish was defended by the
 *   ulama, the scribal estate, and the dynastic state on the claim that the
 *   script is constitutive of Turkish-Muslim identity and of unbroken
 *   continuity with Ottoman institutional memory. This story instantiates ONE
 *   reading of the contested kernel script_as_identity: the
 *   ottoman_continuity_reading. The standing arrangement under contest is the
 *   Arabic-script regime itself; epsilon is authored from this reading's own
 *   lights over that fixed referent (0.36 at interval end — the reading
 *   acknowledges the literacy burden and the ulama-scribal concentration of
 *   textual authority as real costs, but assesses them as largely intrinsic
 *   to the script's sacred and continuity-bearing function rather than
 *   imposed rent). Sibling readings over the same referent author different
 *   epsilon: the kemalist_rupture_reading authors high extraction (the
 *   arrangement as enforced enclosure of the Turkish mind), the
 *   phonetic_instrumentalism_reading authors moderate extraction (the
 *   arrangement as inefficient technology lock-in). Per the
 *   epsilon-invariance principle these are separate constraints linked by
 *   network.affects_constraints, not one observable-dependent story. The
 *   claim/metric gap is deliberate: claimed_type states the structure
 *   believed true (tangled_rope — genuine coordination carrying asymmetric
 *   extraction under active enforcement); the metrics describe the
 *   arrangement's actual operation independently. KEY AGENTS (by structural
 *   relationship): - ottoman_ulama: agenda-setting beneficiary
 *   (institutional/identity_locked) — administers religious-legal validation;
 *   authority flows from Arabic-script monopoly - imperial_chancery_scribes:
 *   beneficiary with administrative duties (organized/identity_locked) —
 *   career capital wholly invested in the script-register complex -
 *   ottoman_dynastic_state: agenda setter (institutional/constrained) —
 *   enforces the arrangement for legitimation; switching scripts would break
 *   its own continuity claims - turkish_speaking_commoners: primary target
 *   (powerless/trapped) — bears the literacy barrier; coalition capacity
 *   suppressed by the barrier itself - reformist_intellectuals: resisting
 *   target (moderate/mobile) — technical writing crippled; exits into exile
 *   publishing - islamic_textual_community: secondary beneficiary
 *   (organized/mobile) — transregional umma scholarship keeps Turkish legible
 *   within the shared script ecumene - minority_script_communities: excluded
 *   counterexample (moderate/constrained) — Karamanlidika and Armeno-Turkish
 *   prove alternatives workable; barred from the Muslim core -
 *   foreign_orientalists: analytical observer (organized/analytical) —
 *   catalogues the archive, documents the literacy barrier, holds no stake
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(script_as_identity__ottoman_continuity_reading, 0.36).
domain_priors:suppression_score(script_as_identity__ottoman_continuity_reading, 0.78).
domain_priors:theater_ratio(script_as_identity__ottoman_continuity_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(script_as_identity__ottoman_continuity_reading, extractiveness, 0.36).
narrative_ontology:constraint_metric(script_as_identity__ottoman_continuity_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(script_as_identity__ottoman_continuity_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(script_as_identity__ottoman_continuity_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(script_as_identity__ottoman_continuity_reading, resistance, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(script_as_identity__ottoman_continuity_reading, tangled_rope).
narrative_ontology:human_readable(script_as_identity__ottoman_continuity_reading, "Arabic Script as Constitutive of Turkish-Islamic Identity (Ottoman Continuity Reading)").
narrative_ontology:topic_domain(script_as_identity__ottoman_continuity_reading, "comparative linguistics/political authority/state-building").

domain_priors:requires_active_enforcement(script_as_identity__ottoman_continuity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(script_as_identity__ottoman_continuity_reading, '35e57616-2428-45f7-a3c9-51f0289ad3cc').
narrative_ontology:cs_kernel_codification('35e57616-2428-45f7-a3c9-51f0289ad3cc', distributed).
narrative_ontology:cs_authority_grounding('35e57616-2428-45f7-a3c9-51f0289ad3cc', lineage).
narrative_ontology:cs_interpretation_layer_present('35e57616-2428-45f7-a3c9-51f0289ad3cc').
narrative_ontology:cs_reading_relation('35e57616-2428-45f7-a3c9-51f0289ad3cc', script_as_identity__kemalist_rupture_reading, coexists_with).
narrative_ontology:cs_reading_relation('35e57616-2428-45f7-a3c9-51f0289ad3cc', script_as_identity__phonetic_instrumentalism_reading, forecloses).
narrative_ontology:cs_axiom('35e57616-2428-45f7-a3c9-51f0289ad3cc', foundational, quranic_script_sacred_inviolable).
narrative_ontology:cs_axiom_status(quranic_script_sacred_inviolable, holdable).
narrative_ontology:cs_axiom_grounding('35e57616-2428-45f7-a3c9-51f0289ad3cc', quranic_script_sacred_inviolable, theological).
narrative_ontology:cs_axiom('35e57616-2428-45f7-a3c9-51f0289ad3cc', foundational, ottoman_memory_requires_script_continuity).
narrative_ontology:cs_axiom_status(ottoman_memory_requires_script_continuity, holdable).
narrative_ontology:cs_axiom_grounding('35e57616-2428-45f7-a3c9-51f0289ad3cc', ottoman_memory_requires_script_continuity, empirically_contingent).
narrative_ontology:cs_reference_frame('35e57616-2428-45f7-a3c9-51f0289ad3cc', script_faith_polity_continuum).
narrative_ontology:cs_drift_state('35e57616-2428-45f7-a3c9-51f0289ad3cc', late_ottoman_reform_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('35e57616-2428-45f7-a3c9-51f0289ad3cc', '').
narrative_ontology:cs_kernel_id(script_as_identity__ottoman_continuity_reading, script_as_identity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(script_as_identity__ottoman_continuity_reading, ottoman_ulama).
narrative_ontology:constraint_beneficiary(script_as_identity__ottoman_continuity_reading, imperial_chancery_scribes).
narrative_ontology:constraint_beneficiary(script_as_identity__ottoman_continuity_reading, islamic_textual_community).
narrative_ontology:constraint_victim(script_as_identity__ottoman_continuity_reading, turkish_speaking_commoners).
narrative_ontology:constraint_victim(script_as_identity__ottoman_continuity_reading, reformist_intellectuals).
narrative_ontology:constraint_vindicates(script_as_identity__ottoman_continuity_reading, script_identity_constitution_doctrine).
narrative_ontology:constraint_vindicates(script_as_identity__ottoman_continuity_reading, caliphal_documentary_continuity_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Staff the medreses, validate contracts and rulings, and authenticate the written word. Their schooling, income, and social rank rest on mastery of Arabic-script literacy that ordinary subjects lack; a change of script would strand that capital and dissolve the office's gatekeeping position. Exit would mean ceasing to be what they are.
narrative_ontology:constraint_stakeholder(script_as_identity__ottoman_continuity_reading, ottoman_ulama, agenda_setter,
    institutional, generational, identity_locked, continental).
narrative_ontology:stakeholder_secondary_role(script_as_identity__ottoman_continuity_reading, ottoman_ulama, beneficiary).

% Compose, copy, and archive the empire's paperwork in the elite Ottoman register written in Arabic script. Decades of apprenticeship are sunk into the script-register complex; salaries and promotion ladders run through it. Leaving means forfeiting career capital accumulated over a working life.
narrative_ontology:constraint_stakeholder(script_as_identity__ottoman_continuity_reading, imperial_chancery_scribes, beneficiary,
    organized, biographical, identity_locked, continental).
narrative_ontology:stakeholder_secondary_role(script_as_identity__ottoman_continuity_reading, imperial_chancery_scribes, agenda_setter).

% Promulgates law, issues firmans, and stamps documents under a seal whose continuity with predecessors is part of its claim to rule. It enforces the script arrangement through printing licenses, control of chancery appointments, and patronage of the religious establishment. Adopting a new script would undercut its own continuity narrative, so it bears the enforcement cost willingly.
narrative_ontology:constraint_stakeholder(script_as_identity__ottoman_continuity_reading, ottoman_dynastic_state, agenda_setter,
    institutional, generational, constrained, continental).

% Speak Turkish at home and need writing for petitions, contracts, and scripture. Acquiring literacy in their own language takes years because the script marks few vowels and the taught register borrows heavily from Arabic and Persian. Most remain unlettered and pay scribes to mediate; organizing around the grievance is hard for people whose grievance is literacy itself.
narrative_ontology:constraint_stakeholder(script_as_identity__ottoman_continuity_reading, turkish_speaking_commoners, payer,
    powerless, biographical, trapped, national).

% Write journalism, technical manuals, and fiction, and find the script slow for typesetting, telegraphy, and scientific notation. Some publish from exile in Latin-letter Turkish; imports of such prints are interdicted at the border. Returning home means submitting to the arrangement they argue against.
narrative_ontology:constraint_stakeholder(script_as_identity__ottoman_continuity_reading, reformist_intellectuals, payer,
    moderate, biographical, mobile, continental).

% Scholars, merchants, and pilgrims from the Arab, Persian, and Turkic lands who read and correspond in the shared script. Turkish written in Arabic letters stays legible to this network; a switch would wall Anatolian print off from it.
narrative_ontology:constraint_stakeholder(script_as_identity__ottoman_continuity_reading, islamic_textual_community, beneficiary,
    organized, generational, mobile, global).

% Orthodox and Armenian printers who set Turkish in Greek and Armenian letters for their own communities. Their books demonstrate that Turkish can be written otherwise, which is precisely why their wares are kept out of Muslim markets and their example dismissed.
narrative_ontology:constraint_stakeholder(script_as_identity__ottoman_continuity_reading, minority_script_communities, excluded,
    moderate, biographical, constrained, regional).

% European scholars who catalogue Ottoman archives, edit texts, and compare literacy across script regimes. They publish the comparisons the parties cite against each other and hold no position in the dispute.
narrative_ontology:constraint_stakeholder(script_as_identity__ottoman_continuity_reading, foreign_orientalists, observer,
    organized, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(script_as_identity__ottoman_continuity_reading, ottoman_ulama).
narrative_ontology:fixing_cost_class(script_as_identity__ottoman_continuity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: One written form served Turkish, Arabic, and Persian interchangeably across a multi-confessional empire's law, administration, and scholarship; six centuries of chancery records remain mutually readable; the believers' language stayed typographically continuous with revelation and with the transregional Islamic textual corpus.
% TRANSFER_FUNCTION: Moves textual authority and literacy costs: years of acquisition flow from would-be readers (as schooling burden or exclusion) to the ulama-scribal complex (as status, office, and income); access to the written record concentrates in the script-literate estates; fees for mediated petitions and contracts flow from the unlettered to scribes.
% ABSENT_VOICES: Commoner would-be readers had no seat in the councils where script policy was debated; minority script communities held working alternatives but were barred from the Muslim-core market their existence threatened; unlettered women, doubly excluded from medrese literacy, were absent entirely. Dissent entered mainly through exile publication, which the customs regime tried to interdict.
% DISAPPEARANCE_RATIONALE: Overnight removal (say, in 1860) would paralyze the chancery, invalidate the documentary continuum on which dynastic legitimation rested, dissolve the ulama's educational monopoly, and cut the empire's typographic tie to the umma's textual body — while simultaneously dropping the cost of mass literacy. Every seated actor's position rearranges; nothing about the arrangement is self-maintaining absent enforcement.
% FOUNDING_PROBLEM: At adoption, from the conversion era onward: join the Islamic textual civilization, give a multi-confessional empire one chancery script, and keep the believers' language typographically continuous with revelation and with Arabic and Persian scholarship.
% FOUNDING_PROBLEM_CORROBORATION: From outside the benefiting parties: reformist_intellectuals attest the administrative-legibility half was overtaken by print and mass schooling well before interval end; foreign_orientalists attest the arrangement survived on enforcement rather than function. The ulama attest the sacred-access half remains live. No source outside the benefiting parties corroborates the full continuity genealogy as this reading states it — that absence is itself signal.
narrative_ontology:disappearance_verdict(script_as_identity__ottoman_continuity_reading, world_rearranges).
narrative_ontology:founding_problem_status(script_as_identity__ottoman_continuity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(script_as_identity__ottoman_continuity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(script_as_identity__ottoman_continuity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(script_as_identity__ottoman_continuity_reading, 0.36, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(script_as_identity__ottoman_continuity_reading_tests).
:- end_tests(script_as_identity__ottoman_continuity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction (0.36) is this reading's own end-state assessment: it concedes the literacy barrier and the concentration of textual authority as costs, but prices them as largely intrinsic to a script it holds constitutive. Suppression (0.78) is a raw structural property, unscaled by power or scope: printing licenses, chancery monopoly over legitimate documents, fatwa-backed delegitimation of script change, and border interdiction of Latin-letter exile prints. Accessibility_collapse (0.62) sits below mountain range because alternatives were demonstrably workable — minority-script printing and exile Latin-letter presses proved it — yet foreclosed for the Muslim core by enforcement rather than nature. Resistance (0.48) reflects real but fragmented challenge: Tanzimat-era debate, journal campaigns, ministerial simplification attempts, exile publication. Theater_ratio (0.25) is low-to-moderate: the arrangement was functionally load-bearing throughout, with performativity rising late as defense became nostalgic (calligraphic revival as politics, ritualized fatwa against reform). Identity-lock dynamics: the ulama's lock is religious-professional fusion (the office IS the literacy); the scribes' lock is career-path dependence (decades of sunk apprenticeship); if either frame broke — if script mastery were reframed as skill rather than sanctity or vocation — their exit options would loosen toward constrained and the computed extraction asymmetry would sharpen. Coalition check: the powerless victim seat's coalition potential is structurally suppressed — the grievance is literacy itself, so the aggrieved cannot easily read, circulate, or sign a joint protest; religious framing further delegitimized cross-class reform coalitions. The measurement series run on one shared time grid (every tracked metric authored at all eight points) so no end-state value is silently substituted into earlier rows. Coordination type is declared identity_coordination with the type-default floor: the dominant function whose failure would cause the coordination problem this reading exists to solve is boundary-and-continuity maintenance, not mere encoding. The FNL gaming alert is taken seriously — identity framing is a classic cover story — and the coupling here is flagged rather than excused: the identity function is genuine (shared sacred typography, umma legibility), but the same structure concentrates textual authority in a gatekeeping estate, which is why the claimed type is tangled_rope rather than rope.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats and the beneficiary seats compute differently from identical structural data. From the ulama and scribal seats the arrangement is the inherited architecture of worship, law, and memory — costs are the price of belonging. From the commoner seat it is a barrier that taxes their own language; from the reformist seat, a technology embargo enforced at the border. The reading's own seat authors epsilon at 0.36 over the fixed referent while a rupture-reading seat over the identical referent authors far higher — that divergence across readings of one kernel is precisely the measurement the corpus exists to take, and the engine's per-seat computation from the structural data, not the authored claim, adjudicates it.
 *
 * DIRECTIONALITY LOGIC:
 *   Declared beneficiaries derive low d: the ulama sit deepest (beneficiary plus identity_locked plus institutional reach amplifies the subsidy side), the scribes near them (beneficiary, identity_locked, but bearing real training costs), and the islamic_textual_community nearest the pure-beneficiary end (beneficiary with mobile exit and no enforcement burden). Declared victims derive high d: turkish_speaking_commoners approach the full-target end (victim, trapped, powerless), while reformist_intellectuals are damped somewhat below them by mobile exit into exile publishing. The dynastic state declares no beneficiary/victim position, so its d falls to the canonical institutional fallback — an acknowledged imprecision: the state collects legitimation rents and pays enforcement costs it judges worthwhile, a near-beneficiary net position the fallback only approximates. No directionality_overrides are authored: the override mechanism keys on power atoms, and an institutional-level override intended for the state would also strike the ulama seat, whose derived near-beneficiary d is correct. The derivation chain is left intact deliberately.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding mandate was twofold: sacred-typographic access and imperial-administrative legibility. By interval end the administrative half is dead — print capitalism, telegraphy, mass schooling, and the nation-state form overtook it — while the sacred-access half remains live within this reading's frame. The arrangement never internally conceded obsolescence; it was terminated externally by the 1928 act of a rival reading's victorious coalition. Because the resolution was external abolition rather than internal recognition, no mandatrophy resolution is declared, and the mismatch consumer finds no zombie flag here: founding_problem_status is contested (not dead) and the disappearance verdict is world_rearranges, so the dead-status-plus-world_rearranges capture signature does not fire. The piton test also fails on its own terms: extraction was concentrated, not diffuse — the ulama demonstrably captured the gains — so had the arrangement lingered past function it would have persisted as capture, not as orphaned inertia.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indexicality,
    'This constraint is one reading of the kernel script_as_identity — what would the sibling readings change structurally, and where exactly is the disagreement located?',
    'No empirical resolution; the contest is settled politically (as in 1928) or by adopting a rival reading''s framework. Structural mapping: instantiate the sibling stories and compare victim sets, epsilon, and computed types over the fixed referent.',
    'Under kemalist_rupture_reading the victim set expands to all Turkish speakers as minds enclosed and epsilon rises sharply; under phonetic_instrumentalism_reading the victim set thins to efficiency losers and the religious-enforcement component reads as pure overhead. The arrangement''s classification flips accordingly.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_indexicality, conceptual, 'Committer structure: one of three readings of the script_as_identity kernel; disagreement located in script ontology (constitutive vs instrumental).').

omega_variable(
    constructed_vs_discovered_constitution,
    'Is the identity-constitution of Arabic script a discovered fact about Turkish-Muslim identity or a constructed doctrine serving ulama-scribal gatekeeping interests?',
    'Comparative script-switch cases among Muslim peoples who retained identity after changing scripts (Latin-letter Malay Muslims, Cyrillic- and Latin-letter Soviet Turkic peoples) versus communities that kept the script under rupture (the Persianate sphere). If identity survives script change elsewhere, constitution is constructed.',
    'If constructed, the coordination story thins toward cover and the computed type shifts snare-ward; if discovered, the tangled_rope reading is vindicated and the ulama''s positional gains read as legitimate coordination cost.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(constructed_vs_discovered_constitution, empirical, 'Naturalness of the script-identity bond: the false-summit-style ambiguity for this reading.').

omega_variable(
    script_vs_schooling_literacy_attribution,
    'How much of the mass illiteracy borne by turkish_speaking_commoners is attributable to the script''s poor fit with Turkish phonology, versus schooling scarcity and poverty?',
    'Literacy-trajectory comparison across contemporaneous Ottoman confessional communities schooled in different scripts (Karamanlidika Greeks, Armeno-Turkish Armenians) and pre/post-1928 Turkish cohorts, controlling for school provision.',
    'High script-attribution raises the victims'' effective extraction and pushes the computed type toward snare; low attribution leaves a rope-dominant profile with the barrier read as incidental.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(script_vs_schooling_literacy_attribution, empirical, 'Causal share of the literacy barrier attributable to script difficulty.').

omega_variable(
    post_abolition_identity_persistence,
    'Did Turkish-Muslim identity persist after the 1928 severance, as this reading''s constitutive claim implies it could not?',
    'Post-1928 religious-identity continuity studies, the survival of religious orders, and the persistence of Arabic-script religious printing parallel to the Latin-letter state.',
    'Persistence weakens the foundational constitutive claim (visible to the engine as axiom-overriding pressure on the empirically contingent memory axiom); collapse would vindicate the reading and retroactively raise the arrangement''s coordination weight.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(post_abolition_identity_persistence, empirical, 'Whether the reading''s core premise survives its own falsification test of 1928.').

omega_variable(
    cs_framing_underdetermination,
    'Is the defensible kernel the script-practice itself (implicit codification, authority from practice) or the identity-doctrine articulated in fatwa and treatise (distributed codification, lineage authority)?',
    'Trace whether adjudication at moments of challenge ran through enacted chancery custom or through doctrinal statements (seyhulislam fatwas decisive).',
    'The practice-framing yields kernel_codification implicit with authority_grounding practice; the doctrine-framing yields distributed/lineage as authored. The CS pattern classification differs between framings; the authored choice follows the fatwa-centered record of the reform debates.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cs_framing_underdetermination, conceptual, 'Alternative framings of the commitment system produce different CS classifications.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(script_as_identity__ottoman_continuity_reading, 1839, 1928).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(scri_tr_t1839, script_as_identity__ottoman_continuity_reading, theater_ratio, 1839, 0.1).
narrative_ontology:measurement_basis(scri_tr_t1839, observed).
narrative_ontology:measurement(scri_tr_t1856, script_as_identity__ottoman_continuity_reading, theater_ratio, 1856, 0.12).
narrative_ontology:measurement_basis(scri_tr_t1856, observed).
narrative_ontology:measurement(scri_tr_t1876, script_as_identity__ottoman_continuity_reading, theater_ratio, 1876, 0.15).
narrative_ontology:measurement_basis(scri_tr_t1876, observed).
narrative_ontology:measurement(scri_tr_t1897, script_as_identity__ottoman_continuity_reading, theater_ratio, 1897, 0.18).
narrative_ontology:measurement_basis(scri_tr_t1897, observed).
narrative_ontology:measurement(scri_tr_t1908, script_as_identity__ottoman_continuity_reading, theater_ratio, 1908, 0.21).
narrative_ontology:measurement_basis(scri_tr_t1908, observed).
narrative_ontology:measurement(scri_tr_t1918, script_as_identity__ottoman_continuity_reading, theater_ratio, 1918, 0.23).
narrative_ontology:measurement_basis(scri_tr_t1918, observed).
narrative_ontology:measurement(scri_tr_t1923, script_as_identity__ottoman_continuity_reading, theater_ratio, 1923, 0.24).
narrative_ontology:measurement_basis(scri_tr_t1923, observed).
narrative_ontology:measurement(scri_tr_t1928, script_as_identity__ottoman_continuity_reading, theater_ratio, 1928, 0.25).
narrative_ontology:measurement_basis(scri_tr_t1928, observed).

% Extraction over time
narrative_ontology:measurement(scri_be_t1839, script_as_identity__ottoman_continuity_reading, base_extractiveness, 1839, 0.2).
narrative_ontology:measurement_basis(scri_be_t1839, observed).
narrative_ontology:measurement(scri_be_t1856, script_as_identity__ottoman_continuity_reading, base_extractiveness, 1856, 0.23).
narrative_ontology:measurement_basis(scri_be_t1856, observed).
narrative_ontology:measurement(scri_be_t1876, script_as_identity__ottoman_continuity_reading, base_extractiveness, 1876, 0.26).
narrative_ontology:measurement_basis(scri_be_t1876, observed).
narrative_ontology:measurement(scri_be_t1897, script_as_identity__ottoman_continuity_reading, base_extractiveness, 1897, 0.29).
narrative_ontology:measurement_basis(scri_be_t1897, observed).
narrative_ontology:measurement(scri_be_t1908, script_as_identity__ottoman_continuity_reading, base_extractiveness, 1908, 0.31).
narrative_ontology:measurement_basis(scri_be_t1908, observed).
narrative_ontology:measurement(scri_be_t1918, script_as_identity__ottoman_continuity_reading, base_extractiveness, 1918, 0.33).
narrative_ontology:measurement_basis(scri_be_t1918, observed).
narrative_ontology:measurement(scri_be_t1923, script_as_identity__ottoman_continuity_reading, base_extractiveness, 1923, 0.35).
narrative_ontology:measurement_basis(scri_be_t1923, observed).
narrative_ontology:measurement(scri_be_t1928, script_as_identity__ottoman_continuity_reading, base_extractiveness, 1928, 0.36).
narrative_ontology:measurement_basis(scri_be_t1928, observed).

% Suppression requirement over time
narrative_ontology:measurement(scri_su_t1839, script_as_identity__ottoman_continuity_reading, suppression_requirement, 1839, 0.55).
narrative_ontology:measurement_basis(scri_su_t1839, observed).
narrative_ontology:measurement(scri_su_t1856, script_as_identity__ottoman_continuity_reading, suppression_requirement, 1856, 0.58).
narrative_ontology:measurement_basis(scri_su_t1856, observed).
narrative_ontology:measurement(scri_su_t1876, script_as_identity__ottoman_continuity_reading, suppression_requirement, 1876, 0.63).
narrative_ontology:measurement_basis(scri_su_t1876, observed).
narrative_ontology:measurement(scri_su_t1897, script_as_identity__ottoman_continuity_reading, suppression_requirement, 1897, 0.68).
narrative_ontology:measurement_basis(scri_su_t1897, observed).
narrative_ontology:measurement(scri_su_t1908, script_as_identity__ottoman_continuity_reading, suppression_requirement, 1908, 0.72).
narrative_ontology:measurement_basis(scri_su_t1908, observed).
narrative_ontology:measurement(scri_su_t1918, script_as_identity__ottoman_continuity_reading, suppression_requirement, 1918, 0.76).
narrative_ontology:measurement_basis(scri_su_t1918, observed).
narrative_ontology:measurement(scri_su_t1923, script_as_identity__ottoman_continuity_reading, suppression_requirement, 1923, 0.77).
narrative_ontology:measurement_basis(scri_su_t1923, observed).
narrative_ontology:measurement(scri_su_t1928, script_as_identity__ottoman_continuity_reading, suppression_requirement, 1928, 0.78).
narrative_ontology:measurement_basis(scri_su_t1928, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(script_as_identity__ottoman_continuity_reading, identity_coordination).
narrative_ontology:affects_constraint(script_as_identity__ottoman_continuity_reading, kemalist_rupture_reading).
narrative_ontology:affects_constraint(script_as_identity__ottoman_continuity_reading, phonetic_instrumentalism_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'the Turkish script question' decomposes into three readings of one kernel (script_as_identity), each epsilon-invariant over the shared referent — the Arabic-script arrangement for Turkish, 1839-1928. This story (ottoman_continuity_reading) authors epsilon 0.36 from the continuity seat; kemalist_rupture_reading authors high epsilon from the rupture seat; phonetic_instrumentalism_reading authors moderate epsilon from the instrumentality seat. The continuity reading is upstream: its claims supplied the terms the rivals defined themselves against, so influence edges run from this story to both siblings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
