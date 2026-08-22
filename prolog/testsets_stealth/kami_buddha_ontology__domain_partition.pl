% ============================================================================
% CONSTRAINT STORY: kami_buddha_ontology__domain_partition
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_kami_buddha_ontology__domain_partition, []).

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
 *   constraint_id: kami_buddha_ontology__domain_partition
 *   human_readable: Kami-Buddha Domain Partition: Life/Purity versus Death/Impurity Jurisdictions
 *   domain: religious/institutional/cultural
 *
 * SUMMARY:
 *   In early modern Japan, the religious field was organized by a division of
 *   ontological labor: kami governed life, fertility, and purity; buddhas and
 *   bodhisattvas governed death, impurity, and the afterlife. The arrangement
 *   solved a real coordination problem — no rite had two providers, death
 *   pollution stayed out of kami spheres per a purity doctrine far older than
 *   either establishment's economics — and simultaneously built the largest
 *   compulsory revenue stream in the religious economy: the temple funerary
 *   monopoly, cemented by the bakufu's temple-certification (terauke) system,
 *   which made every household a registered parishioner owing mortuary fees,
 *   memorial stipends, and dues. This file instantiates ONE reading of the
 *   kami_buddha_ontology kernel — the domain_partition reading — as a clean
 *   epsilon-invariant constraint: two parallel ontologies, functional
 *   complementarity without fusion, no hierarchy, practical coordination
 *   without theoretical unity. The sibling readings (honji_suijaku_monism,
 *   incoherent_bundle) are separate constraints, not positions described
 *   inside this one. The claim/metric gap is deliberate and load-bearing: the
 *   reading CLAIMS complementary coordination of distinct domains; the
 *   authored metrics describe an arrangement whose death side accumulated
 *   compulsory rents for two centuries and whose enforcement turned violent
 *   the moment the reading seized state power in 1868. The engine measures
 *   that divergence; nothing here reconciles claim to metrics.
 *
 * KEY AGENTS:
 *   - - commoner_households: primary target (powerless/trapped) — bears the compulsory dual payments across every lifecycle stage
 *   - - buddhist_temple_establishment: primary beneficiary (institutional/constrained) — collects the funerary-monopoly rents and administers the death domain
 *   - - shinto_shrine_establishment: secondary beneficiary (institutional/constrained) — collects life-side offerings and maintains the purity boundary
 *   - - tokugawa_bakufu: agenda_setter and indirect beneficiary (institutional/arbitrage) — legislates the arrangement and harvests its surveillance yield
 *   - - hidden_christian_communities: excluded objectors (powerless/trapped) — bear persecution for refusing the death-domain monopoly
 *   - - dual_practice_ascetics: squeezed middle (organized/constrained) — straddler tradition forced into single-domain affiliation
 *   - - kokugaku_scholars: analytical observer (moderate/analytical) — sees the whole structure from outside every establishment's payroll
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(kami_buddha_ontology__domain_partition, 0.62).
domain_priors:suppression_score(kami_buddha_ontology__domain_partition, 0.58).
domain_priors:theater_ratio(kami_buddha_ontology__domain_partition, 0.36).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(kami_buddha_ontology__domain_partition, extractiveness, 0.62).
narrative_ontology:constraint_metric(kami_buddha_ontology__domain_partition, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(kami_buddha_ontology__domain_partition, theater_ratio, 0.36).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(kami_buddha_ontology__domain_partition, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(kami_buddha_ontology__domain_partition, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(kami_buddha_ontology__domain_partition, tangled_rope).
narrative_ontology:human_readable(kami_buddha_ontology__domain_partition, "Kami-Buddha Domain Partition: Life/Purity versus Death/Impurity Jurisdictions").
narrative_ontology:topic_domain(kami_buddha_ontology__domain_partition, "religious/institutional/cultural").

domain_priors:requires_active_enforcement(kami_buddha_ontology__domain_partition).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(kami_buddha_ontology__domain_partition, '3ffccf7d-23b0-42a7-a599-dca1902be08d').
narrative_ontology:cs_kernel_codification('3ffccf7d-23b0-42a7-a599-dca1902be08d', distributed).
narrative_ontology:cs_authority_grounding('3ffccf7d-23b0-42a7-a599-dca1902be08d', practice).
narrative_ontology:cs_interpretation_layer_present('3ffccf7d-23b0-42a7-a599-dca1902be08d').
narrative_ontology:cs_reading_relation('3ffccf7d-23b0-42a7-a599-dca1902be08d', kami_buddha_ontology__honji_suijaku_monism, coexists_with).
narrative_ontology:cs_reading_relation('3ffccf7d-23b0-42a7-a599-dca1902be08d', kami_buddha_ontology__incoherent_bundle, influences).
narrative_ontology:cs_axiom('3ffccf7d-23b0-42a7-a599-dca1902be08d', foundational, kami_buddha_distinct_jurisdictions).
narrative_ontology:cs_axiom_status(kami_buddha_distinct_jurisdictions, holdable).
narrative_ontology:cs_axiom_grounding('3ffccf7d-23b0-42a7-a599-dca1902be08d', kami_buddha_distinct_jurisdictions, theological).
narrative_ontology:cs_axiom('3ffccf7d-23b0-42a7-a599-dca1902be08d', secondary, death_impurity_barred_from_kami_sites).
narrative_ontology:cs_axiom_status(death_impurity_barred_from_kami_sites, holdable).
narrative_ontology:cs_axiom_grounding('3ffccf7d-23b0-42a7-a599-dca1902be08d', death_impurity_barred_from_kami_sites, conventional).
narrative_ontology:cs_reference_frame('3ffccf7d-23b0-42a7-a599-dca1902be08d', complementary_domain_sovereignty).
narrative_ontology:cs_drift_state('3ffccf7d-23b0-42a7-a599-dca1902be08d', meiji_separation_edicts, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('3ffccf7d-23b0-42a7-a599-dca1902be08d', '').
narrative_ontology:cs_kernel_id(kami_buddha_ontology__domain_partition, kami_buddha_ontology).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(kami_buddha_ontology__domain_partition, buddhist_temple_establishment).
narrative_ontology:constraint_beneficiary(kami_buddha_ontology__domain_partition, shinto_shrine_establishment).
narrative_ontology:constraint_beneficiary(kami_buddha_ontology__domain_partition, tokugawa_bakufu).
narrative_ontology:constraint_victim(kami_buddha_ontology__domain_partition, commoner_households).
narrative_ontology:constraint_victim(kami_buddha_ontology__domain_partition, hidden_christian_communities).
narrative_ontology:constraint_victim(kami_buddha_ontology__domain_partition, dual_practice_ascetics).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(kami_buddha_ontology__domain_partition, commoner_households).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Operates the death domain exclusively: funerals, memorial services (hōji), grave custody, and posthumous ordination names (kaimyō). Holds every commoner household as a registered danka parishioner obligated to route all mortuary rites through its temple, and collects funerary fees, memorial stipends, and annual dues. Administers the household registers that certify orthodoxy, and polices the boundary that keeps death rites out of shrine precincts. Its sacred mission and its revenue base are the same territory; abandoning the death-domain monopoly would mean surrendering the economic foundation of most rural temples.
narrative_ontology:constraint_stakeholder(kami_buddha_ontology__domain_partition, buddhist_temple_establishment, beneficiary,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(kami_buddha_ontology__domain_partition, buddhist_temple_establishment, agenda_setter).

% Conducts purification rites, festivals (matsuri), agricultural ceremonies, and life-cycle blessings — births, childhood milestones, weddings, construction purifications. Receives offerings, festival contributions, and purification fees from the same households that pay the temples. Maintains death taboos rigorously: no funerals on shrine grounds, priests avoid corpse contact, mourners undergo purification before approaching the kami. Major shrines hold landed endowments; rural shrines survive on parishioner offerings and often sit administratively under nearby temples.
narrative_ontology:constraint_stakeholder(kami_buddha_ontology__domain_partition, shinto_shrine_establishment, beneficiary,
    institutional, generational, constrained, national).

% Legislates the arrangement from above: the temple-certification (terauke) system makes registration at a Buddhist temple compulsory for every household, legally cementing the death-domain monopoly. Regulates both establishments through the jisha bugyō magistrates, adjudicates jurisdictional disputes, and uses the temple registration network as its Christian-suppression surveillance infrastructure. Collects no ritual fees itself but gains a totalizing population-control lever; restructuring the arrangement would require rebuilding that control apparatus from scratch.
narrative_ontology:constraint_stakeholder(kami_buddha_ontology__domain_partition, tokugawa_bakufu, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(kami_buddha_ontology__domain_partition, tokugawa_bakufu, beneficiary).

% Pays both sides of the partition across every lifecycle: funerary and memorial fees, grave maintenance, and danka dues to the temple; festival levies, offering obligations, and purification fees to the shrine. Receives genuine services from each. Cannot obtain a legitimate burial outside the temple system, cannot skip shrine obligations without community sanction, and cannot change temples or move villages without certification chains that the temple controls. The compulsory character of the payments, not their existence, is what households contest.
narrative_ontology:constraint_stakeholder(kami_buddha_ontology__domain_partition, commoner_households, payer,
    powerless, biographical, trapped, local).
narrative_ontology:stakeholder_secondary_role(kami_buddha_ontology__domain_partition, commoner_households, beneficiary).

% Communities, concentrated in the Nagasaki region, whose conscience forbids Buddhist mortuary rites and kami veneration alike. They tread on fumie tablets, maintain nominal temple registration, and perform Buddhist-form funerals outwardly while practicing secretly. Discovery means persecution, execution, or forced conversion. Their objection — that the death-domain monopoly leaves them no lawful way to bury their dead according to conscience — never enters any official deliberation; the arrangement's unanimity is manufactured by their exclusion.
narrative_ontology:constraint_stakeholder(kami_buddha_ontology__domain_partition, hidden_christian_communities, excluded,
    powerless, generational, trapped, regional).

% Shugendō adepts, itinerant holy men, and mountain practitioners whose entire repertoire straddles the partition — kami invocations and esoteric Buddhist rites in a single practice. As the binary hardens, they are forced to affiliate with either a temple line or a shrine line, pay affiliation and licensing fees, and abandon the mixed rites that constitute their tradition. Their syncretic practice is rendered administratively illegible by the two-domain scheme; compliance costs them their distinctive identity.
narrative_ontology:constraint_stakeholder(kami_buddha_ontology__domain_partition, dual_practice_ascetics, payer,
    organized, biographical, constrained, regional).

% Nativist philologists of the Motoori and Hirata lineages who read the classical corpus, argue for the antiquity and primacy of the kami cult, and attack the buddha-centric death economy as a foreign accretion. They see the whole structure — the fused practice, the funerary rents, the purity doctrine — from outside any establishment's payroll. Their analysis circulates among mid-ranking samurai and educated rural elites, and becomes the intellectual ammunition for the state movement that finally tears the arrangement apart.
narrative_ontology:constraint_stakeholder(kami_buddha_ontology__domain_partition, kokugaku_scholars, observer,
    moderate, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(kami_buddha_ontology__domain_partition, buddhist_temple_establishment).
narrative_ontology:fixing_cost_class(kami_buddha_ontology__domain_partition, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Divides ritual labor between two specialist institutions so that no rite has two competing providers and no provider lacks a jurisdiction: life, fertility, and purity rites route to shrine lineages; death, funerary, and afterlife rites route to temple lineages. This prevents jurisdictional conflict over the same households, matches each rite to the institution holding the relevant doctrinal apparatus, and operationalizes the pollution rule that keeps death impurity (kegare) away from kami spheres.
% TRANSFER_FUNCTION: Moves money, labor, and status recognition from commoner households to both establishments — compulsory funerary fees, memorial stipends, grave custody, and danka dues to temples; offerings, festival levies, and purification fees to shrines — and moves compliance information (household orthodoxy certifications) from households through temples to the bakufu.
% ABSENT_VOICES: Hidden Christian communities are excluded by persecution; their objection to a death-domain monopoly that criminalizes conscientious burial is structurally silenced. Dual-practice ascetics sit inside the system but their tradition has no seat where the binary's terms are set. The dead themselves — the purported beneficiaries of the mortuary apparatus — have no voice anywhere; all mortuary choices are made by survivors under compulsion.
% DISAPPEARANCE_RATIONALE: If the partition vanished overnight, households would still need birth and death rites handled, but the compulsory payment structure, the temple registration monopoly, and the bakufu's certification-based surveillance would all dissolve. Ritual provision would open to competition, funerary prices would fall toward service cost, the danka system's dues would become voluntary, and the state would lose its conversion-detection network — the religious economy would reorganize around whichever providers households actually chose.
% FOUNDING_PROBLEM: Managing death pollution relative to purity-centered kami cults while guaranteeing every household an authorized ritual provider for each stage of life. The purity/death division is attested in classical liturgy and statute (the Ōharae no kotoba litany, Engishiki pollution regulations) centuries before either establishment had an economic stake in it; the early modern arrangement grafted a compulsory funerary economy and a surveillance apparatus onto that older coordination problem.
% FOUNDING_PROBLEM_CORROBORATION: The pre-institutional textual record corroborates the founding problem from outside all benefiting parties: classical norito and ritsuryō-code pollution regulations attest death-purity separation long before temple funerary revenues existed, and modern historiography of kegare confirms the division's antiquity. The beneficiary establishments obviously attest liveness as well, but they are not the load-bearing witnesses. Contested corroboration cuts the other way too: Kuroda-line scholarship argues the partition was never the operative medieval frame at all — that contest is carried explicitly in the omega variables rather than resolved here.
narrative_ontology:disappearance_verdict(kami_buddha_ontology__domain_partition, world_rearranges).
narrative_ontology:founding_problem_status(kami_buddha_ontology__domain_partition, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(kami_buddha_ontology__domain_partition, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(kami_buddha_ontology__domain_partition, 'none', 1).
narrative_ontology:epsilon_provenance(kami_buddha_ontology__domain_partition, 0.62, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(kami_buddha_ontology__domain_partition_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(kami_buddha_ontology__domain_partition, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(kami_buddha_ontology__domain_partition_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is substantial (0.62 scalar; series rising 0.44 to 0.67) because the death-side payments were compulsory, decoupled from service cost, and backed by law — the danka system converted a pastoral relationship into a tax relationship. Suppression (0.58) reflects structural compulsion: registration was mandatory, temple-switching required certification the temple controlled, and refusal on conscience grounds was persecuted. Theater (0.36) is moderate: the life-side rites deliver real services and the purity boundary does real coordinative work, but a growing share of activity across the interval is formulaic memorial performance and doctrinal self-justification. Accessibility_collapse (0.60) is partial: once the partition is understood, lawful alternatives are largely foreclosed — no legitimate burial outside the temple, no shrine death rites — yet folk practice persisted underneath and conscience communities survived underground. Resistance (0.45) is real but scattered: hidden Christian defiance, household resentment of fees, and the kokugaku intellectual assault that ultimately armed the state's demolition. The three measurement series share one eight-point grid (1600-1868) so every metric is authored at every examined time point; trajectories are monotonic rather than cyclical — enforcement machinery matured steadily (customary boundary, to legalized compulsion in the 1630s-50s, to violent state enforcement in 1868), with the terminal suppression spike (0.78) marking the shinbutsu bunri edicts and the anti-temple violence that followed, and the terminal theater dip (0.28) marking the moment enforcement became brutally functional rather than ceremonial.
 *
 * PERSPECTIVAL GAP:
 *   Four seats inhabit the same structure as four different arrangements. From the temple seat, the partition is sacred order and livelihood as one indivisible thing — the funerary monopoly IS the dharma work; questioning the fees questions the salvation it funds. From the household seat, the same structure is a compulsory double-payment regime in which the services are real but the compulsion is theft-adjacent. From the bakufu seat, it is administrative infrastructure — a population registry wearing liturgical clothing. From the kokugaku seat, it is a foreign usurpation of the native cult's proper sphere. The engine computes per-seat classifications from the structural data; the divergence between the beneficiary seats' coordination experience and the payer seats' extraction experience is the measurement, not a defect to be averaged away.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary/victim declarations drive the derivation. The temple establishment sits nearest the beneficiary pole (collects the largest compulsory transfer, administers the rules, constrained exit because its endowment is the arrangement). The shrine establishment is a beneficiary at slightly higher d — it collects real revenue but its take is less compulsory and its rural members were often administratively subordinate to temples. The bakufu declares as agenda_setter with secondary beneficiary: it pays administration costs and collects control-yield rather than fees, placing it near-symmetric but structurally protected. Commoner households are the primary target — high d, amplified by trapped exit (certification chains bind mobility). Hidden Christian communities are targets at maximum effective extraction: trapped, persecuted, and paying compliance costs for rites they reject. Dual-practice ascetics are targets whose organized networks soften but cannot remove the squeeze. Kokugaku scholars occupy the analytical seat with no directional stake. No directionality overrides are authored: the beneficiary/victim declarations plus exit options produce the correct differentiation without correction.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled-rope classification is what prevents two symmetric misreadings. Reading the arrangement as pure rope (as the establishments' framing does) erases the compulsory funerary economy and the persecuted consciences — the coordination story is real but it is not the whole story. Reading it as pure snare (as the kokugaku polemic and a naive secular reading do) erases the genuine life-side coordination and the antiquity of the pollution logic the arrangement operationalizes — the extraction rides on a real division of labor, not on a fabricated one. The mandatrophy question resolves cleanly here: the founding problem (death-purity management with guaranteed ritual provision) is still live — it predates the arrangement, survived its 1868 demolition, and persists in contemporary disputes over Shinto funerals — while the particular institutional form that carried it (danka compulsion, terauke certification) is dead. A constraint whose problem outlives its form is precisely the case where the six-questions mismatch check earns its keep: status=live with verdict=world_rearranges flags that what rearranged in 1868 was the carrier, not the need.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'Is the domain partition a genuinely operative pre-Meiji structure steering practice, or a nativist-and-Meiji retrojection imposed on a reality whose operative frame was honji suijaku fusion?',
    'Period practice records predating 1800 — where funerals were actually held, whether shrine death-taboos were enforced against ordinary households, household ritual diaries, village ordinance books — assessed independently of Meiji-era ideological writing.',
    'If retrojection, this reading''s epsilon misattributes two centuries of extraction to a doctrine that was not steering practice until 1868; the extraction history migrates to the enforcement moment and the classification collapses toward the sibling readings'' accounts of the same terrain.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contestation, empirical, 'Whether the partition reading describes the standing arrangement or a later imposition — the core contest between this reading and its siblings.').

omega_variable(
    monopoly_vs_partition_extraction,
    'How much of the measured extraction belongs to the partition principle itself, versus the terauke/danka surveillance machinery that rode on the death-domain monopoly?',
    'Compare extraction intensity across regions and periods where a funerary monopoly existed without compulsory state-backed registration, isolating the doctrinal division from the legal compulsion layered onto it.',
    'Determines whether the tangled-rope verdict attaches to the doctrine (coordination with rents) or to its state capture (a snare wearing a doctrine); the two resolutions license different remedies in the sibling analyses.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(monopoly_vs_partition_extraction, conceptual, 'Attribution split between the partition doctrine and the compulsory registration apparatus built on it.').

omega_variable(
    pollution_logic_naturalness,
    'Is the life/death division a deep feature of the religious grammar — pollution logic that recurs independently across cultures — or a contingent institutional settlement particular to these two establishments?',
    'Comparative analysis of ritual-pollution systems cross-culturally, plus internal Japanese variation across class, region, and period (court versus village practice, frontier settlements with weak institutional reach).',
    'High naturalness raises the coordination-function weight and pulls the classification rope-ward; demonstrated contingency raises the extraction weight and pulls it snare-ward.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(pollution_logic_naturalness, empirical, 'Whether the partition instantiates a general purity structure or a negotiable institutional deal.').

omega_variable(
    household_counterfactual_demand,
    'Absent compulsion, would households have voluntarily purchased both establishments'' services at anything like the observed rates?',
    'Frontier and remote-region records where enforcement was thin, and the magnitude of mortuary-demand collapse after 1871 when the danka obligation was abolished.',
    'A large post-liberation collapse implies compulsion carried the arrangement and the suppression scalar is understated; persistent demand implies genuine service value and supports the coordination-function component of the tangled-rope reading.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(household_counterfactual_demand, empirical, 'Revealed-preference test of how much of the arrangement''s volume compulsion was supplying.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(kami_buddha_ontology__domain_partition, 1600, 1868).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(kb_domain_partition_tr_t1600, kami_buddha_ontology__domain_partition, theater_ratio, 1600, 0.2).
narrative_ontology:measurement(kb_domain_partition_tr_t1640, kami_buddha_ontology__domain_partition, theater_ratio, 1640, 0.24).
narrative_ontology:measurement(kb_domain_partition_tr_t1690, kami_buddha_ontology__domain_partition, theater_ratio, 1690, 0.29).
narrative_ontology:measurement(kb_domain_partition_tr_t1740, kami_buddha_ontology__domain_partition, theater_ratio, 1740, 0.32).
narrative_ontology:measurement(kb_domain_partition_tr_t1790, kami_buddha_ontology__domain_partition, theater_ratio, 1790, 0.35).
narrative_ontology:measurement(kb_domain_partition_tr_t1830, kami_buddha_ontology__domain_partition, theater_ratio, 1830, 0.37).
narrative_ontology:measurement(kb_domain_partition_tr_t1853, kami_buddha_ontology__domain_partition, theater_ratio, 1853, 0.39).
narrative_ontology:measurement(kb_domain_partition_tr_t1868, kami_buddha_ontology__domain_partition, theater_ratio, 1868, 0.28).

% Extraction over time
narrative_ontology:measurement(kb_domain_partition_be_t1600, kami_buddha_ontology__domain_partition, base_extractiveness, 1600, 0.44).
narrative_ontology:measurement(kb_domain_partition_be_t1640, kami_buddha_ontology__domain_partition, base_extractiveness, 1640, 0.56).
narrative_ontology:measurement(kb_domain_partition_be_t1690, kami_buddha_ontology__domain_partition, base_extractiveness, 1690, 0.6).
narrative_ontology:measurement(kb_domain_partition_be_t1740, kami_buddha_ontology__domain_partition, base_extractiveness, 1740, 0.61).
narrative_ontology:measurement(kb_domain_partition_be_t1790, kami_buddha_ontology__domain_partition, base_extractiveness, 1790, 0.62).
narrative_ontology:measurement(kb_domain_partition_be_t1830, kami_buddha_ontology__domain_partition, base_extractiveness, 1830, 0.63).
narrative_ontology:measurement(kb_domain_partition_be_t1853, kami_buddha_ontology__domain_partition, base_extractiveness, 1853, 0.64).
narrative_ontology:measurement(kb_domain_partition_be_t1868, kami_buddha_ontology__domain_partition, base_extractiveness, 1868, 0.67).

% Suppression requirement over time
narrative_ontology:measurement(kb_domain_partition_su_t1600, kami_buddha_ontology__domain_partition, suppression_requirement, 1600, 0.35).
narrative_ontology:measurement(kb_domain_partition_su_t1640, kami_buddha_ontology__domain_partition, suppression_requirement, 1640, 0.52).
narrative_ontology:measurement(kb_domain_partition_su_t1690, kami_buddha_ontology__domain_partition, suppression_requirement, 1690, 0.55).
narrative_ontology:measurement(kb_domain_partition_su_t1740, kami_buddha_ontology__domain_partition, suppression_requirement, 1740, 0.56).
narrative_ontology:measurement(kb_domain_partition_su_t1790, kami_buddha_ontology__domain_partition, suppression_requirement, 1790, 0.57).
narrative_ontology:measurement(kb_domain_partition_su_t1830, kami_buddha_ontology__domain_partition, suppression_requirement, 1830, 0.58).
narrative_ontology:measurement(kb_domain_partition_su_t1853, kami_buddha_ontology__domain_partition, suppression_requirement, 1853, 0.59).
narrative_ontology:measurement(kb_domain_partition_su_t1868, kami_buddha_ontology__domain_partition, suppression_requirement, 1868, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(kami_buddha_ontology__domain_partition, resource_allocation).
narrative_ontology:affects_constraint(kami_buddha_ontology__domain_partition, honji_suijaku_monism).
narrative_ontology:affects_constraint(kami_buddha_ontology__domain_partition, incoherent_bundle).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'Shinto-Buddhist relations' decomposes, per the epsilon-invariance principle, into three structurally distinct claims that must not share one story. (1) honji_suijaku_monism — an identity-hierarchy claim with its own epsilon, beneficiary structure (the temple complex atop the hierarchy), and medieval empirical career. (2) This file, domain_partition — a distinctness-complementarity claim whose epsilon tracks the compulsory funerary economy built on the death-domain monopoly. (3) incoherent_bundle — a meta-claim denying the kernel's coherence altogether, whose epsilon attaches to the institutional machinery sustaining contradictions. Upstream/downstream structure: honji_suijaku_monism was the operative medieval frame and is cited as evidence AGAINST this reading's historical reality (the partition may be retrojection — see omega kernel_reading_contestation); conversely, this reading's 1868 enforcement episode supplies the principal data the incoherent_bundle reading theorizes. All three files link one another via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
