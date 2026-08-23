% ============================================================================
% CONSTRAINT STORY: shinbutsu_ontological_substrate__domain_partition_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
 *   constraint_id: shinbutsu_ontological_substrate__domain_partition_reading
 *   human_readable: Kami-Buddha Two-Domain Functional Partition Arrangement (Domain Partition Reading)
 *   domain: religious studies/Japanese history/commitment systems
 *
 * SUMMARY:
 *   From the Nara period onward, Japanese religious life ran on a two-track
 *   arrangement: kami shrines administered this-worldly efficacy — rain,
 *   harvests, protection from misfortune, purification — while Buddhist
 *   temples administered death and salvation — funerals, memorial rites,
 *   karmic care. This story instantiates the domain_partition_reading of the
 *   shinbutsu_ontological_substrate kernel: on this reading the tracks were
 *   functionally specialized rather than ontologically fused, the honji
 *   suijaku vocabulary identifying kami as manifestations of buddhas was
 *   institutional accommodation, and the arrangement could therefore be
 *   severed in 1868 without destroying anything essential on either side. It
 *   is one member of a three-story family decomposed per the
 *   epsilon-invariance principle; the sibling stories
 *   (syncretic_fusion_reading, incoherent_bundle_reading) author the same
 *   historical material under different commitments and carry different
 *   epsilon values. Epsilon's referent is the standing two-track arrangement
 *   as it operated through the Tokugawa period (interval 0-30 maps to
 *   1600-1868), assessed by this reading's own lights: a genuine division of
 *   religious labor carrying a real extractive overlay (compulsory parish
 *   affiliation and fee-bearing death care) — not the harmonious
 *   complementarity of pious memory, nor a pure control machine. Claim and
 *   metrics are authored independently.
 *
 * KEY AGENTS:
 *   - tokugawa_shogunate: agenda-setting seat (institutional/arbitrage) — writes and enforces the rules, collects compliance, bears stipend and enforcement costs
 *   - buddhist_temple_establishment: primary collecting seat (institutional/constrained) — funerary monopoly plus registration duties, dual-positioned collector-administrator
 *   - shinto_shrine_priesthood: secondary beneficiary seat (organized/identity_locked) — protected this-world jurisdiction, hereditary offices bind exit
 *   - commoner_danka_households: primary bearing seat (powerless/trapped) — compulsory affiliation, fee-bearing death care, certificate-dependent mobility
 *   - hidden_christian_communities: bearing seat under proscription (powerless/trapped) — concealment as the only remaining option
 *   - unaffiliated_folk_practitioners: bearing seat at the margins (powerless/constrained) — licensing pressure or absorption
 *   - women_excluded_from_sacred_sites: excluded seat — would object to exclusion zones but holds no place in the conversation
 *   - religious_studies_historians: analytical observer — sees the full structure across the whole interval
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(shinbutsu_ontological_substrate__domain_partition_reading, 0.6).
domain_priors:suppression_score(shinbutsu_ontological_substrate__domain_partition_reading, 0.56).
domain_priors:theater_ratio(shinbutsu_ontological_substrate__domain_partition_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(shinbutsu_ontological_substrate__domain_partition_reading, extractiveness, 0.6).
narrative_ontology:constraint_metric(shinbutsu_ontological_substrate__domain_partition_reading, suppression_requirement, 0.56).
narrative_ontology:constraint_metric(shinbutsu_ontological_substrate__domain_partition_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(shinbutsu_ontological_substrate__domain_partition_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(shinbutsu_ontological_substrate__domain_partition_reading, resistance, 0.52).

% --- Constraint claim ---
narrative_ontology:constraint_claim(shinbutsu_ontological_substrate__domain_partition_reading, tangled_rope).
narrative_ontology:human_readable(shinbutsu_ontological_substrate__domain_partition_reading, "Kami-Buddha Two-Domain Functional Partition Arrangement (Domain Partition Reading)").
narrative_ontology:topic_domain(shinbutsu_ontological_substrate__domain_partition_reading, "religious studies/Japanese history/commitment systems").

domain_priors:requires_active_enforcement(shinbutsu_ontological_substrate__domain_partition_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(shinbutsu_ontological_substrate__domain_partition_reading, 'f36cbb4a-8b5c-42b2-b6bd-a0d95437afcb').
narrative_ontology:cs_kernel_codification('f36cbb4a-8b5c-42b2-b6bd-a0d95437afcb', distributed).
narrative_ontology:cs_authority_grounding('f36cbb4a-8b5c-42b2-b6bd-a0d95437afcb', lineage).
narrative_ontology:cs_interpretation_layer_present('f36cbb4a-8b5c-42b2-b6bd-a0d95437afcb').
narrative_ontology:cs_reading_relation('f36cbb4a-8b5c-42b2-b6bd-a0d95437afcb', shinbutsu_ontological_substrate__syncretic_fusion_reading, forecloses).
narrative_ontology:cs_reading_relation('f36cbb4a-8b5c-42b2-b6bd-a0d95437afcb', shinbutsu_ontological_substrate__incoherent_bundle_reading, coexists_with).
narrative_ontology:cs_axiom('f36cbb4a-8b5c-42b2-b6bd-a0d95437afcb', foundational, kami_buddha_jurisdictions_disjoint).
narrative_ontology:cs_axiom_status(kami_buddha_jurisdictions_disjoint, holdable).
narrative_ontology:cs_axiom_grounding('f36cbb4a-8b5c-42b2-b6bd-a0d95437afcb', kami_buddha_jurisdictions_disjoint, conventional).
narrative_ontology:cs_axiom('f36cbb4a-8b5c-42b2-b6bd-a0d95437afcb', foundational, coexistence_functional_not_ontological).
narrative_ontology:cs_axiom_status(coexistence_functional_not_ontological, holdable).
narrative_ontology:cs_axiom_grounding('f36cbb4a-8b5c-42b2-b6bd-a0d95437afcb', coexistence_functional_not_ontological, conventional).
narrative_ontology:cs_reference_frame('f36cbb4a-8b5c-42b2-b6bd-a0d95437afcb', parallel_two_domain_jurisdiction).
narrative_ontology:cs_drift_state('f36cbb4a-8b5c-42b2-b6bd-a0d95437afcb', meiji_separation_onset, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('f36cbb4a-8b5c-42b2-b6bd-a0d95437afcb', '').
narrative_ontology:cs_kernel_id(shinbutsu_ontological_substrate__domain_partition_reading, shinbutsu_ontological_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(shinbutsu_ontological_substrate__domain_partition_reading, buddhist_temple_establishment).
narrative_ontology:constraint_beneficiary(shinbutsu_ontological_substrate__domain_partition_reading, shinto_shrine_priesthood).
narrative_ontology:constraint_beneficiary(shinbutsu_ontological_substrate__domain_partition_reading, tokugawa_shogunate).
narrative_ontology:constraint_victim(shinbutsu_ontological_substrate__domain_partition_reading, commoner_danka_households).
narrative_ontology:constraint_victim(shinbutsu_ontological_substrate__domain_partition_reading, hidden_christian_communities).
narrative_ontology:constraint_victim(shinbutsu_ontological_substrate__domain_partition_reading, unaffiliated_folk_practitioners).
narrative_ontology:constraint_vindicates(shinbutsu_ontological_substrate__domain_partition_reading, separate_domain_jurisdiction_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Issues the edicts that organize religious life: the parish registration system requiring every household to belong to a Buddhist temple, the bans on Christianity, and the regulations pairing shrines and temples under dedicated commissioners (jisha-bugyo). Gains census data, travel control, and ideological stability from the arrangement; pays stipends to major shrines and head temples and funds enforcement. Stands above the arrangement and reshapes or abandons pieces of it when policy shifts, as the 1868 separation orders show.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_substrate__domain_partition_reading, tokugawa_shogunate, agenda_setter,
    institutional, generational, arbitrage, national).

% Operates the parish (danka) system: conducts the funerals and memorial rites every household eventually requires, issues the certifications the authorities demand, and collects fees, rice dues, and labor from parishioners in return. Performs registration and reporting duties locally. Its income, legal standing, and monopoly over death care all rest on the arrangement; walking away would mean surrendering protected status and the funerary stream.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_substrate__domain_partition_reading, buddhist_temple_establishment, beneficiary,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(shinbutsu_ontological_substrate__domain_partition_reading, buddhist_temple_establishment, agenda_setter).

% Administers festivals, purification rites, prayers for rain and harvest, and warding of misfortune — the this-world side of the division of religious labor. Receives offerings and, for major shrines, official stipends. Priest offices pass along hereditary lines tied to particular shrines, so abandoning the office means abandoning the lineage and its land. Great shrines bargain with Buddhist institutions from strength; thousands of village shrines host Buddhist chapels on their grounds and depend on neighboring temples for upkeep.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_substrate__domain_partition_reading, shinto_shrine_priesthood, beneficiary,
    organized, generational, identity_locked, national).

% Must affiliate with a Buddhist parish temple: a death in the family obligates funerals and ongoing memorial services with fees attached, and the temple's certificate is required for marriage, travel, and changing residence. Households also fund village shrine festivals. Individually they cannot decline affiliation; collective refusal surfaces as village riots, occasionally successful, more often punished.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_substrate__domain_partition_reading, commoner_danka_households, payer,
    powerless, biographical, trapped, regional).

% Descendants of converted communities in Kyushu and elsewhere practice baptism and prayer in secret after the bans, masking observance behind Buddhist and shrine appearances and treading on fumie tablets when demanded. Discovery means execution, forced relocation, or the destruction of their village. They cannot emigrate and cannot practice openly; survival depends on concealment.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_substrate__domain_partition_reading, hidden_christian_communities, payer,
    powerless, generational, trapped, regional).

% Independent healers, diviners, mediums, and mountain ascetics operating outside the licensed shrine and temple structures. The authorities require them to affiliate with recognized schools or monasteries, take out licenses, or cease practice outright; many are absorbed into Shugendo lineages or temple networks on unfavorable terms. The alternative is marginality or prosecution.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_substrate__domain_partition_reading, unaffiliated_folk_practitioners, payer,
    powerless, biographical, constrained, regional).

% Barred by joint shrine-temple exclusion zones (nyonin kekkai) from major mountain pilgrimage centers and from most clerical careers; women's religious participation is routed through domestic rites, natal-family altars, and peripheral halls. They hold no seat in the councils that set these rules and have no institutional channel through which to contest them.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_substrate__domain_partition_reading, women_excluded_from_sacred_sites, excluded,
    powerless, biographical, constrained, regional).

% Modern scholars reconstruct the arrangement from temple registers, shrine records, bakufu edicts, and sectarian documents, testing whether the coexistence rested on a shared metaphysics, a workable division of labor, or mere accumulation. They classify and debate but hold no power over the historical arrangement itself.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_substrate__domain_partition_reading, religious_studies_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(shinbutsu_ontological_substrate__domain_partition_reading, buddhist_temple_establishment).
narrative_ontology:fixing_cost_class(shinbutsu_ontological_substrate__domain_partition_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Allocates the spectrum of religious need across two provider systems: shrine institutions administer this-worldly efficacy (rain, harvest, protection, purification) while temple institutions administer death and salvation (funerals, memorial rites, karmic care). Mutual recognition prevents jurisdictional conflict and gives households a single legible map of which institution serves which need.
% TRANSFER_FUNCTION: Moves offerings, stipends, and funerary and memorial fees from commoner households and patrons to the temple and shrine establishments; moves administrative compliance (registration, certification, surveillance reporting) from households to the state through the temples.
% ABSENT_VOICES: Danka households had no formal voice in setting parish obligations; hidden Christians practiced under sentence of discovery and death; women were barred from sacred sites by rules set without their representation; unlicensed folk practitioners had no standing to defend their practices before the licensing authorities. Dissent reached the surface only as flight, riot, or clandestinity.
% DISAPPEARANCE_RATIONALE: If the two-domain arrangement vanished overnight, households would lose the shared map of religious provision and face competing claims over death care; the state would lose its registration and surveillance apparatus overnight; both establishments would lose legitimation, revenue, and legal standing. Something like this rearrangement is what actually happened after 1868 — painfully, through property seizures, forced layovers of clergy, and the reconstruction of both traditions around new national frames.
% FOUNDING_PROBLEM: An imported salvific tradition (Buddhism) and indigenous cults of place and prosperity had to coexist without either destroying the other or forcing worshippers to choose between this-worldly and ultimate concerns; the court and later the warrior government needed a stable framework for protecting, regulating, and taxing both.
% FOUNDING_PROBLEM_CORROBORATION: No seat outside the two establishments attests that the integration problem was still live at interval end. Confucian advisors (Hayashi Razan's circle) attacked the temples' wealth and influence from outside both cults; National Learning (kokugaku) scholars explicitly denied that kami needed Buddhist framing and treated the fusion vocabulary as corruption; bakufu administrative records attest the arrangement's utility as census and control machinery rather than as theological necessity. Temple and shrine histories, the interested parties, are the only sources asserting continuing soteriological indispensability.
narrative_ontology:disappearance_verdict(shinbutsu_ontological_substrate__domain_partition_reading, world_rearranges).
narrative_ontology:founding_problem_status(shinbutsu_ontological_substrate__domain_partition_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(shinbutsu_ontological_substrate__domain_partition_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(shinbutsu_ontological_substrate__domain_partition_reading, 'none', 1).
narrative_ontology:epsilon_provenance(shinbutsu_ontological_substrate__domain_partition_reading, 0.6, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(shinbutsu_ontological_substrate__domain_partition_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(shinbutsu_ontological_substrate__domain_partition_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(shinbutsu_ontological_substrate__domain_partition_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.60: the two-domain division itself is cheap to operate, but by the eighteenth century the parish system had welded compulsory temple affiliation, fee-bearing funerals and memorial cycles, and certification-for-travel into the standing package, so the arrangement extracts substantially without reaching predatory extremes. Suppression 0.56: enforcement runs through the Kirishitan bans, mandatory parish certification, and licensing of independent practitioners; daily religious life is regulated rather than terrorized. Theater 0.30: festival and rite calendars remain functionally load-bearing (agricultural timing, death care, community assembly), with a growing ceremonial residue around court and bakufu observance. Accessibility_collapse 0.48: Christianity is crushed and independent practice squeezed into licensed channels, but sect variety and household folk observance persist inside the tracks. Resistance 0.52: village riots against temple exactions, concealed Christian persistence, and nativist critique build across the interval. All three tracked series share one grid (t=0,6,12,18,24,30). Suppression_requirement is tracked deliberately: enforcement capacity visibly built up (ban codification, registration hardening through the mid-seventeenth century, then maturation) rather than staying flat, so the scalar alone would miss the dynamic.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat and the bearing seats compute different types from the same structure. From the bakufu seat the arrangement is administrative order: legible households, controllable clergy, suppressed sedition. From the temple seat it is livelihood and duty fused — certification work that funds the dharma. From the danka household seat the same structure is compulsory fees and mandatory death services with exit closed. The excluded seats sharpen the divergence: hidden Christians experience the arrangement as a mortal threat administered through parish paperwork, and women encounter it as exclusion zones set without their voice. Individually powerless households are not without coalition resources — village-level refusals (ikki) periodically forced fee concessions and occasionally burned registrar-temples — which is why resistance sits above midpoint despite trapped individual exits.
 *
 * DIRECTIONALITY LOGIC:
 *   The shogunate declares no victim position and holds arbitrage-grade exit, placing it near the beneficiary end; its costs (stipends, enforcement spending) are real but dwarfed by the compliance and legibility it receives. The temple establishment collects the funerary stream and sits near the beneficiary end despite bearing enforcement labor. The shrine priesthood gains a protected jurisdiction; identity-lock through hereditary office keeps exit near zero without raising its extraction exposure. The declared victim groups — danka households, hidden Christian communities, unaffiliated folk practitioners — sit near the full-target end, and trapped or coerced exit keeps them there; the engine derives their elevated effective extraction from the beneficiary/victim declarations plus exit modulation. Women excluded from sacred sites are an authored absence: they shape the consensus picture but, per R3, feed commentary rather than correction.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope claim prevents two symmetrical mislabels. Reading the arrangement as pure coordination (the complementarity myth) would erase the parish fee stream and the registration coercion; reading it as a pure snare would erase the real division of labor that gave households a legible map of religious provision and gave both cults protected space to develop. The R5 interview sharpens the lifecycle question: the founding integration problem was largely settled by the medieval period, yet the arrangement not only persisted but thickened around revenue and surveillance — founding_problem_status 'contested' combined with disappearance_verdict 'world_rearranges' is precisely the mismatch signature the engine reads as a capture/zombie tendency, cross-checked against the rising theater and extraction series. On this reading the 1868 severance moved fast because the coordination core was never ontologically load-bearing; what fought back was property and office, not metaphysics.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reading_position_in_kernel_contest,
    'This story instantiates the domain_partition_reading of the shinbutsu_ontological_substrate kernel; what would adopting a sibling reading change structurally?',
    'Compare the three linked family stories'' epsilon values, beneficiary/victim structures, and computed types: syncretic_fusion_reading authors the same arrangement as an ontological unity (high entanglement, separation as metaphysical violence); incoherent_bundle_reading denies a coherent kernel altogether (accumulated drift under state enforcement, no commitment to classify).',
    'If the fusion reading is adopted, the 1868 separation becomes destruction of a fused commitment rather than removal of an overlay, and entanglement metrics rise sharply; if the bundle reading is adopted, this constraint dissolves into institutional-history artifacts and no classification attaches to the kernel.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_position_in_kernel_contest, conceptual, 'Committer structure: which reading of the kernel this story instantiates and what the siblings would change.').

omega_variable(
    location_of_kernel_disagreement,
    'Where exactly do the readings disagree — is the honji suijaku vocabulary metaphysical truth, institutional accommodation, or accumulated drift?',
    'Doctrinal analysis separating prescriptive texts (treatises asserting identity) from descriptive practice records (registers, contracts, rite schedules); test whether day-to-day practice presupposed identity or merely tolerated the vocabulary.',
    'A metaphysical-truth resolution supports the sibling fusion reading and raises this arrangement''s effective entanglement; an accommodation resolution sustains this reading''s low-entanglement profile; a drift resolution hands the terrain to the bundle reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(location_of_kernel_disagreement, conceptual, 'Locates the disputed structural element: the semantic status of honji suijaku.').

omega_variable(
    separation_cost_attribution,
    'Was the violence of the 1868 separation orders (haibutsu kishaku) driven by ontological fusion that had to be undone, or by institutions defending property and status?',
    'Archival mapping of destruction and forced-layover targets: if concentrated on revenue-bearing temple complexes and danka rolls rather than on doctrinally fused sites, the cost was institutional, not metaphysical.',
    'Property-driven costs confirm easy structural separation and this reading''s low-entanglement delta; evidence that doctrine itself resisted severance would raise entanglement and shift weight toward the fusion reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(separation_cost_attribution, empirical, 'Whether separation difficulty reflects ontology or asset defense.').

omega_variable(
    danka_layer_separability,
    'How much of the measured extraction belongs to the two-domain partition itself versus the state surveillance layer (temple registration) riding on it?',
    'Compare periods and regions where registration enforcement varied independently of dual-domain religious practice; isolate the share of funerary-fee burden attributable to monopoly pricing versus to mandated affiliation.',
    'If separable, the partition core computes nearer pure coordination and the extractive load belongs to a distinct state-control constraint; if inseparable, the tangled_rope classification stands as authored.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(danka_layer_separability, empirical, 'Decomposability of the coordination core from the extraction overlay.').

omega_variable(
    kami_autonomy_or_subordination,
    'Did the domain division protect the kami cults'' autonomy, as this reading holds, or mask their subordination within a Buddhist-framed hierarchy?',
    'Compare shrine economic records and kami prestige markers before and at the height of honji suijaku usage; trace who controlled shrine appointments and revenues.',
    'Subordination evidence would weaken the shrine-establishment beneficiary declaration, raise extraction asymmetry, and push the classification toward the snare end; autonomy evidence stabilizes the tangled_rope reading.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kami_autonomy_or_subordination, conceptual, 'Whether the partition shielded or disguised the kami cults'' position.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(shinbutsu_ontological_substrate__domain_partition_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(shin_tr_t0, shinbutsu_ontological_substrate__domain_partition_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement_basis(shin_tr_t0, observed).
narrative_ontology:measurement(shin_tr_t6, shinbutsu_ontological_substrate__domain_partition_reading, theater_ratio, 6, 0.2).
narrative_ontology:measurement_basis(shin_tr_t6, observed).
narrative_ontology:measurement(shin_tr_t12, shinbutsu_ontological_substrate__domain_partition_reading, theater_ratio, 12, 0.22).
narrative_ontology:measurement_basis(shin_tr_t12, observed).
narrative_ontology:measurement(shin_tr_t18, shinbutsu_ontological_substrate__domain_partition_reading, theater_ratio, 18, 0.25).
narrative_ontology:measurement_basis(shin_tr_t18, observed).
narrative_ontology:measurement(shin_tr_t24, shinbutsu_ontological_substrate__domain_partition_reading, theater_ratio, 24, 0.28).
narrative_ontology:measurement_basis(shin_tr_t24, observed).
narrative_ontology:measurement(shin_tr_t30, shinbutsu_ontological_substrate__domain_partition_reading, theater_ratio, 30, 0.3).
narrative_ontology:measurement_basis(shin_tr_t30, observed).

% Extraction over time
narrative_ontology:measurement(shin_be_t0, shinbutsu_ontological_substrate__domain_partition_reading, base_extractiveness, 0, 0.4).
narrative_ontology:measurement_basis(shin_be_t0, observed).
narrative_ontology:measurement(shin_be_t6, shinbutsu_ontological_substrate__domain_partition_reading, base_extractiveness, 6, 0.45).
narrative_ontology:measurement_basis(shin_be_t6, observed).
narrative_ontology:measurement(shin_be_t12, shinbutsu_ontological_substrate__domain_partition_reading, base_extractiveness, 12, 0.5).
narrative_ontology:measurement_basis(shin_be_t12, observed).
narrative_ontology:measurement(shin_be_t18, shinbutsu_ontological_substrate__domain_partition_reading, base_extractiveness, 18, 0.54).
narrative_ontology:measurement_basis(shin_be_t18, observed).
narrative_ontology:measurement(shin_be_t24, shinbutsu_ontological_substrate__domain_partition_reading, base_extractiveness, 24, 0.57).
narrative_ontology:measurement_basis(shin_be_t24, observed).
narrative_ontology:measurement(shin_be_t30, shinbutsu_ontological_substrate__domain_partition_reading, base_extractiveness, 30, 0.6).
narrative_ontology:measurement_basis(shin_be_t30, observed).

% Suppression requirement over time
narrative_ontology:measurement(shin_su_t0, shinbutsu_ontological_substrate__domain_partition_reading, suppression_requirement, 0, 0.36).
narrative_ontology:measurement_basis(shin_su_t0, observed).
narrative_ontology:measurement(shin_su_t6, shinbutsu_ontological_substrate__domain_partition_reading, suppression_requirement, 6, 0.47).
narrative_ontology:measurement_basis(shin_su_t6, observed).
narrative_ontology:measurement(shin_su_t12, shinbutsu_ontological_substrate__domain_partition_reading, suppression_requirement, 12, 0.52).
narrative_ontology:measurement_basis(shin_su_t12, observed).
narrative_ontology:measurement(shin_su_t18, shinbutsu_ontological_substrate__domain_partition_reading, suppression_requirement, 18, 0.55).
narrative_ontology:measurement_basis(shin_su_t18, observed).
narrative_ontology:measurement(shin_su_t24, shinbutsu_ontological_substrate__domain_partition_reading, suppression_requirement, 24, 0.56).
narrative_ontology:measurement_basis(shin_su_t24, observed).
narrative_ontology:measurement(shin_su_t30, shinbutsu_ontological_substrate__domain_partition_reading, suppression_requirement, 30, 0.56).
narrative_ontology:measurement_basis(shin_su_t30, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(shinbutsu_ontological_substrate__domain_partition_reading, resource_allocation).
narrative_ontology:affects_constraint(shinbutsu_ontological_substrate__domain_partition_reading, shinbutsu_ontological_substrate__syncretic_fusion_reading).
narrative_ontology:affects_constraint(shinbutsu_ontological_substrate__domain_partition_reading, shinbutsu_ontological_substrate__incoherent_bundle_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'kami-buddha syncretism' covers three structurally distinct commitments, decomposed per the epsilon-invariance principle into three linked stories sharing the kernel shinbutsu_ontological_substrate: this story (domain_partition_reading — functional coexistence, low entanglement, moderate epsilon), shinbutsu_ontological_substrate__syncretic_fusion_reading (ontological unity; honji suijaku as metaphysical truth; high entanglement, different epsilon), and shinbutsu_ontological_substrate__incoherent_bundle_reading (no coherent kernel; accumulated drift under state enforcement; no unified epsilon to author). Each story carries its own epsilon, beneficiary/victim structure, and classification; the edges here record family membership, not agreement.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
