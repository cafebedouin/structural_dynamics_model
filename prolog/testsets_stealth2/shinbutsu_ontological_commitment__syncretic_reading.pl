% ============================================================================
% CONSTRAINT STORY: shinbutsu_ontological_commitment__syncretic_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_shinbutsu_ontological_commitment__syncretic_reading, []).

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
 *   constraint_id: shinbutsu_ontological_commitment__syncretic_reading
 *   human_readable: Honji-Suijaku Syncretic Order: Kami as Manifest Traces of Buddhas
 *   domain: religious/ontological/historical
 *
 * SUMMARY:
 *   Medieval and early-modern Japan ran its religious life through a single
 *   combinative order: under honji-suijaku metaphysics, every significant
 *   kami was identified as a local manifestation (suijaku) of a buddha or
 *   bodhisattva (honji) — Amaterasu with Dainichi in some Shingon readings,
 *   Hachiman with Amida — and shrines were administered, financed, and
 *   ritually interpreted through temple complexes. This story authors THAT
 *   arrangement as instantiated by the syncretic reading of the
 *   shinbutsu_ontological_commitment kernel: one unified cosmological order,
 *   genuinely integrative and genuinely hierarchical. The claim/metric gap is
 *   deliberate: the reading claims a coherent unified order (that is what
 *   distinguishes it from its siblings), while the authored metrics describe
 *   substantial asymmetric transfer running through that order — the engine
 *   measures the divergence. Per the epsilon-invariance principle this is one
 *   of three files: the partition reading and the incoherence reading
 *   instantiate different constraints with different epsilon, victim sets,
 *   and enforcement targets, linked through network.affects_constraints. KEY
 *   AGENTS (by structural relationship): - buddhist_monastic_establishments:
 *   agenda-setter and principal beneficiary (institutional/arbitrage) —
 *   administers shrine-temples, authors the correspondence doctrine, collects
 *   offerings and fees - buddhist_doctrinal_lineages: beneficiary
 *   (institutional/arbitrage) — collects doctrinal prestige and canonical
 *   authority from kami subordination - shrine_priesthoods: primary target
 *   (organized/identity_locked) — hereditary kami-cult lineages bearing
 *   ontological demotion and administrative subordination -
 *   village_shrine_communities: diffuse payers with incidental benefits
 *   (powerless/constrained) — finance the combined complex, receive mortuary
 *   and festival services - imperial_court: beneficiary
 *   (powerful/constrained) — runs one ritual calendar of sovereignty through
 *   the unified order - warrior_governments: secondary agenda-setter and
 *   beneficiary (institutional/mobile) — enforces the institutional machinery
 *   for surveillance and taxation - kami_primacy_advocates: excluded voice
 *   (moderate/constrained) — Watarai, Yoshida, and kokugaku partisans
 *   marginalized at the frame's edge - religious_historians: analytical
 *   observer (analytical/analytical)
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(shinbutsu_ontological_commitment__syncretic_reading, 0.64).
domain_priors:suppression_score(shinbutsu_ontological_commitment__syncretic_reading, 0.62).
domain_priors:theater_ratio(shinbutsu_ontological_commitment__syncretic_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(shinbutsu_ontological_commitment__syncretic_reading, extractiveness, 0.64).
narrative_ontology:constraint_metric(shinbutsu_ontological_commitment__syncretic_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(shinbutsu_ontological_commitment__syncretic_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(shinbutsu_ontological_commitment__syncretic_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(shinbutsu_ontological_commitment__syncretic_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(shinbutsu_ontological_commitment__syncretic_reading, tangled_rope).
narrative_ontology:human_readable(shinbutsu_ontological_commitment__syncretic_reading, "Honji-Suijaku Syncretic Order: Kami as Manifest Traces of Buddhas").
narrative_ontology:topic_domain(shinbutsu_ontological_commitment__syncretic_reading, "religious/ontological/historical").

domain_priors:requires_active_enforcement(shinbutsu_ontological_commitment__syncretic_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(shinbutsu_ontological_commitment__syncretic_reading, '00b4c41b-760e-4bbe-ad4d-1f5b8fd1a9f1').
narrative_ontology:cs_kernel_codification('00b4c41b-760e-4bbe-ad4d-1f5b8fd1a9f1', distributed).
narrative_ontology:cs_authority_grounding('00b4c41b-760e-4bbe-ad4d-1f5b8fd1a9f1', distributed).
narrative_ontology:cs_reading_relation('00b4c41b-760e-4bbe-ad4d-1f5b8fd1a9f1', shinbutsu_ontological_commitment__partition_reading, forecloses).
narrative_ontology:cs_reading_relation('00b4c41b-760e-4bbe-ad4d-1f5b8fd1a9f1', shinbutsu_ontological_commitment__incoherence_reading, forecloses).
narrative_ontology:cs_axiom('00b4c41b-760e-4bbe-ad4d-1f5b8fd1a9f1', foundational, kami_buddha_cosmological_unity).
narrative_ontology:cs_axiom_status(kami_buddha_cosmological_unity, holdable).
narrative_ontology:cs_axiom_grounding('00b4c41b-760e-4bbe-ad4d-1f5b8fd1a9f1', kami_buddha_cosmological_unity, theological).
narrative_ontology:cs_axiom('00b4c41b-760e-4bbe-ad4d-1f5b8fd1a9f1', secondary, kami_as_traces_subordinate_to_buddha_ground).
narrative_ontology:cs_axiom_status(kami_as_traces_subordinate_to_buddha_ground, holdable).
narrative_ontology:cs_axiom_grounding('00b4c41b-760e-4bbe-ad4d-1f5b8fd1a9f1', kami_as_traces_subordinate_to_buddha_ground, theological).
narrative_ontology:cs_reference_frame('00b4c41b-760e-4bbe-ad4d-1f5b8fd1a9f1', unified_honji_suijaku_cosmos).
narrative_ontology:cs_drift_state('00b4c41b-760e-4bbe-ad4d-1f5b8fd1a9f1', post_meiji_separation, gap(repudiation_pressure, severe, true)).
narrative_ontology:cs_created_at('00b4c41b-760e-4bbe-ad4d-1f5b8fd1a9f1', '').
narrative_ontology:cs_kernel_id(shinbutsu_ontological_commitment__syncretic_reading, shinbutsu_ontological_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(shinbutsu_ontological_commitment__syncretic_reading, buddhist_monastic_establishments).
narrative_ontology:constraint_beneficiary(shinbutsu_ontological_commitment__syncretic_reading, buddhist_doctrinal_lineages).
narrative_ontology:constraint_beneficiary(shinbutsu_ontological_commitment__syncretic_reading, imperial_court).
narrative_ontology:constraint_beneficiary(shinbutsu_ontological_commitment__syncretic_reading, warrior_governments).
narrative_ontology:constraint_victim(shinbutsu_ontological_commitment__syncretic_reading, shrine_priesthoods).
narrative_ontology:constraint_victim(shinbutsu_ontological_commitment__syncretic_reading, village_shrine_communities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(shinbutsu_ontological_commitment__syncretic_reading, village_shrine_communities).
narrative_ontology:constraint_vindicates(shinbutsu_ontological_commitment__syncretic_reading, honji_suijaku_doctrine).
narrative_ontology:constraint_vindicates(shinbutsu_ontological_commitment__syncretic_reading, ryobu_sanno_correspondence_systems).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Operates the great temple complexes (Enryakuji, Onjoji, Todaiji) that built shrine-temples on kami grounds, supplied their clergy, kept their ledgers, and produced the correspondence doctrines identifying each major kami with a school's central buddha. Sets the terms under which shrines participate in the combined ritual economy and collects offerings, land rents, and administrative fees routed through the shrine-temples. Because it authors the doctrinal frame it administers, it faces no barrier from the arrangement it maintains — revising a correspondence table is within its ordinary competence.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_commitment__syncretic_reading, buddhist_monastic_establishments, agenda_setter,
    institutional, generational, arbitrage, national).

% Tendai and Shingon exegetical houses whose correspondence systems (Sanno, Ryobu) became the authoritative accounts of kami identity. Collects prestige, students, and canonical standing whenever a shrine accepts its genealogy for a local deity; does not itself run shrine administration. Its stake is reputational and doctrinal rather than fiscal.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_commitment__syncretic_reading, buddhist_doctrinal_lineages, beneficiary,
    institutional, generational, arbitrage, national).

% Hereditary lineages (Nakatomi, Imbe, Watarai and hundreds of local houses) whose office, income, and marriage alliances are bound to serving a specific kami across generations. Under the arrangement their deity is accounted a manifestation of a buddha, their rites provisional means, and their shrines frequently placed under a temple's supervision with revenues shared or diverted. Leaving the office would dissolve the lineage's purpose and ancestry; remaining means accepting subordinate rank for the object of their lifelong service.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_commitment__syncretic_reading, shrine_priesthoods, payer,
    organized, generational, identity_locked, regional).

% Peasant congregations that finance festivals now narrated as Buddhist events, pay levies and labor dues to the combined temple-shrine complex, and learn their local deity's 'true identity' from temple clergy. In exchange they receive mortuary rites, festival organization, calendrical regulation, and occasional mediation from the same complex. They have no voice in the doctrinal assignment and no realistic option of withdrawing their kami from the system.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_commitment__syncretic_reading, village_shrine_communities, payer,
    powerless, biographical, constrained, local).
narrative_ontology:stakeholder_secondary_role(shinbutsu_ontological_commitment__syncretic_reading, village_shrine_communities, beneficiary).

% Runs sovereignty's ritual calendar through the unified order: the sovereign appears simultaneously as descendant of the sun kami and protector of the dharma, and court rites draw on both registers without contradiction. Benefits from not having to choose between the clerical establishment and the nativist ritual houses. Exiting would mean rewriting the ritual constitution of the throne itself, so the court accommodates whatever integration the clergy proposes.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_commitment__syncretic_reading, imperial_court, beneficiary,
    powerful, civilizational, constrained, national).

% The Kamakura, Muromachi, and Tokugawa bakufu patronize the combined complexes, confirm abbots and priests, and from the seventeenth century compel every household to register with a Buddhist temple (terauke), making funerary affiliation compulsory and turning the combined network into an instrument of census, taxation, and Christian suppression. They enforce the arrangement's institutional machinery and draw administrative benefit from it, while treating its doctrine as instrumentally useful rather than personally binding.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_commitment__syncretic_reading, warrior_governments, agenda_setter,
    institutional, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(shinbutsu_ontological_commitment__syncretic_reading, warrior_governments, beneficiary).

% Watarai priests at Ise, Yoshida Kanetomo with his inverted genealogy (kami original, buddhas derivatives), and later the kokugaku scholars argue that the kami precede and exceed the buddhas. Licensed only where their claims can be housed inside the combinative frame's terms and marginalized elsewhere, they circulate at the arrangement's edges — petitioning, writing, preserving alternative genealogies — until the political opening of the 1860s gives their position state power.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_commitment__syncretic_reading, kami_primacy_advocates, excluded,
    moderate, generational, constrained, national).

% Modern scholars reconstruct the arrangement from doctrinal treatises, shrine ledgers, court diaries, and bakufu edicts, and dispute among themselves whether it constituted a lived unified commitment, a domain partition, or an administered incoherence. They bear none of its costs and collect none of its revenues.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_commitment__syncretic_reading, religious_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(shinbutsu_ontological_commitment__syncretic_reading, buddhist_monastic_establishments).
narrative_ontology:fixing_cost_class(shinbutsu_ontological_commitment__syncretic_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the coexistence problem of two totalizing religious systems on one archipelago: a single cosmological frame lets temples and shrines share sites, calendars, patrons, and personnel; assigns each kami a place in the buddha's salvation economy; gives the kami cult scriptural dignity and the Buddhist establishment local roots; and lets the court and warrior governments govern one ritual landscape instead of two warring ones.
% TRANSFER_FUNCTION: Moves ontological primacy, administrative authority, and revenue from shrine institutions to temple complexes (via jinguji supervision, redirection of offerings and land rent, and doctrinal reclassification of kami); moves legitimacy downward from buddhas to kami; and moves household registration, funerary obligation, and festival labor from commoner households to the combined complexes.
% ABSENT_VOICES: Kami-primacy partisans (Watarai lineages, the Yoshida school, kokugaku scholars) would object that the hierarchy inverts the true order; they are present only at the margins, licensed when containable. Village worshippers would object to having their deities reidentified without consent; they are present only as payers and audiences. Neither seat sits where the doctrinal frame is written.
% DISAPPEARANCE_RATIONALE: The Meiji separation of 1868 is the natural experiment: decrees ordered shrines purged of Buddhist objects, shrine-temples demolished, priests reclassified, and combined festivals rewritten; the haibutsu kishaku movement closed thousands of temples within a few years. The speed and completeness of the rearrangement show how much institutional architecture — revenue, personnel, calendars, kinship — was organized around the unified frame.
% FOUNDING_PROBLEM: After the Buddhist transmission, the realm contained two systems each claiming comprehensive truth: an imported salvation religion with scriptural, artistic, and technological superiority, and an indigenous kami cult bound to land, ancestry, and the agricultural cycle. Patrons needed both — Buddhist rites for death and karma, kami rites for harvest and protection — and neither clergy could simply eliminate the other.
% FOUNDING_PROBLEM_CORROBORATION: Attested from outside the benefiting parties: nativist critics concede the coexistence problem was real even while rejecting the hierarchical solution (Yoshida Kanetomo built his counter-genealogy to answer the same problem; Motoori Norinaga attacked Buddhist contamination as a corruption of something that needed managing); court ritual codes (Engishiki) and bakufu temple edicts independently document the coordination need; modern religious historians with no stake in any party's continuity reconstruct the problem from appointment, taxation, and festival records.
narrative_ontology:disappearance_verdict(shinbutsu_ontological_commitment__syncretic_reading, world_rearranges).
narrative_ontology:founding_problem_status(shinbutsu_ontological_commitment__syncretic_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(shinbutsu_ontological_commitment__syncretic_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(shinbutsu_ontological_commitment__syncretic_reading, 'none', 1).
narrative_ontology:epsilon_provenance(shinbutsu_ontological_commitment__syncretic_reading, 0.64, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(shinbutsu_ontological_commitment__syncretic_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(shinbutsu_ontological_commitment__syncretic_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(shinbutsu_ontological_commitment__syncretic_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction (0.64 at interval end) reflects a large continuous transfer — administrative control of shrines through jinguji, redirection of offerings and land rent, compulsory funerary affiliation under the Tokugawa registration system — discounted by genuine two-way legitimation (kami cults gained scriptural dignity and mortuary services; temples gained territorial roots). Suppression (0.62) is structural and doctrinal rather than violent: an exegetical monopoly over kami identity, temple supervision of shrine offices, and finally state-compelled temple registration; it tracks the enforcement ratchet visible in the suppression_requirement series. Theater (0.30) stays below proxy-collapse range: the doctrine did real coordinative work, though formalism grows under compulsory registration. Accessibility_collapse (0.50): the alternative — kami as autonomous supreme deities — never fully collapsed; it survived as licensed inversion (Yoshida), marginal texts (Watarai), and print-era kokugaku. Resistance (0.45): sustained elite resistance across five centuries, culminating in the coalition that took the state in 1868. Coordination type identity_coordination: the arrangement's dominant function is maintaining the boundary of a single religious community within which kami-veneration and buddha-veneration are both legitimate membership acts; its failure mode is boundary collapse into zero-sum sectarian rivalry. The three series share one time grid (900–1868 at six points) so no metric is sampled against another's gaps. The trajectory is a monotonic enforcement ratchet, not a cycle: voluntary assimilation (pre-1100), systematized hierarchy (1100–1500), compulsory institutionalization (1600–1868), terminated exogenously rather than decaying.
 *
 * PERSPECTIVAL GAP:
 *   From the temple seat the arrangement is a harmonious integration it authored and profits from; from the shrine seat it is the demotion of the objects of a lineage's lifelong service; from the village seat it is simultaneously inclusion (mortuary care, festivals) and levy; from the court seat it is administrative convenience; from the nativist margin it is an invertible error awaiting its moment. The engine computes these divergent per-seat classifications from the structural data; the authored claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations (temple complexes, doctrinal lineages, court, warrior governments) place those seats near the beneficiary end of d; the shrine priesthoods, declared victims and identity_locked in their hereditary office, sit near the full-target end; village communities, dual-positioned payers-with-benefits, compute mid-range. No directionality overrides are needed: the beneficiary/victim declarations plus exit atoms already differentiate the seats, including the two agenda-setting seats (temple complexes author the doctrine; warrior governments enforce its machinery while treating the doctrine as instrumental).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — how two totalizing religious systems share one realm — stayed live for the entire interval; the arrangement was terminated by political revolution, not by atrophy, so no mandatrophy is declared and the theater series never approaches proxy-collapse range. The tangled_rope classification prevents two mislabelings: reading the arrangement as pure extraction would erase the real coordination achieved (without the unified frame, temple and shrine economies compete zero-sum for patrons, sites, and calendars, and the kami cult loses access to the mortuary care its communities needed); reading it as pure coordination would erase the identifiable payers (shrine lineages losing autonomy and rank) and the suppressed alternative (kami primacy). The R5 mismatch check reads live-status against world_rearranges: a functioning constraint, no zombie flag.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_membership_routing,
    'This story instantiates the syncretic reading of the shinbutsu_ontological_commitment kernel (kami and buddhas as one unified cosmological order); the partition reading (separate domains, no ontological integration) and the incoherence reading (no stable commitment, tolerated incoherence) are separate constraints — what structurally changes if a sibling is adopted instead?',
    'Author the sibling stories and diff the structural surfaces: the partition reading removes the ontological-subordination victim set and the enforcement target (nothing integrative to enforce); the incoherence reading converts doctrinal enforcement into tolerance administration and dissolves the coherence-based coordination claim.',
    'Under the partition reading the arrangement loses its extraction-bearing hierarchy and trends toward a domain-partitioning coordination mechanism; under the incoherence reading the suppression series measures bureaucratic tolerance rather than doctrinal coercion and the claimed coherence collapses.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_membership_routing, conceptual, 'Committer-frame routing: kernel membership, sibling structural deltas, and the axes of disagreement (integration for partition; stability-of-commitment for incoherence).').

omega_variable(
    layering_softener_decomposition,
    'Historians routinely soften the syncretic/partition contradiction by layering — unified doctrine among elites, partitioned practice among commoners. If that layered hybrid is the true structure, does this constraint decompose into an elite-doctrine constraint and a popular-practice constraint with separate epsilon values?',
    'Compare doctrinal investment (treatises, lecture halls, correspondence liturgies) against parish-level practice records (funerary registers, festival accounts): if the two strata stabilize different arrangements, split the story.',
    'Decomposition would assign high coherence and moderate extraction to the elite stratum and low coherence with diffuse levies to the popular stratum, replacing this single hybrid profile with a linked pair.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(layering_softener_decomposition, conceptual, 'Whether the unified frame is one arrangement or two stratified ones.').

omega_variable(
    sincerity_vs_administration,
    'Did the clerical and courtly elites hold the unity as a lived ontological commitment, or administer it as a convenient fiction maintained for institutional peace?',
    'Evidence of costly doctrinal investment (building correspondence chapels, training in esoteric rites at shrines, litigating precedence disputes at real cost) versus bare compliance in administrative records.',
    'If administered fiction, the arrangement''s coherence is theatrical and theater_ratio is understated; if sincerely held, the doctrinal enforcement metrics measure real coercion of real belief.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sincerity_vs_administration, empirical, 'Lived commitment versus administrative convenience behind the unified frame.').

omega_variable(
    subordination_as_elevation_or_degradation,
    'Did shrine elites experience trace-status (kami as manifestations of buddhas) as degradation of their deities or as elevation — access to buddhahood, scriptural dignity, universal salvation for their congregations?',
    'Read shrine-side texts against temple-side texts: acceptance language, requests for jinguji construction, and lineage marriages into temple networks versus protest records and autonomy petitions.',
    'If experienced as elevation, the shrine seat''s effective extraction drops sharply and the arrangement trends toward coordination from that seat; if degradation, the payer classification stands and hardens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(subordination_as_elevation_or_degradation, preference, 'Valence of subordination for the subordinated seat.').

omega_variable(
    exogenous_termination_counterfactual,
    'Absent the Meiji political rupture, would the arrangement have persisted as a stable equilibrium, or was kokugaku pressure already eroding it toward collapse?',
    'Counterfactual analysis of late-Tokugawa indicators: shrine-priest petitions, daimyo-level nativist patronage, temple financial stress, and the speed with which local elites switched sides in 1868.',
    'A brittle arrangement would suggest the enforcement ratchet was masking decay (inertial dynamics before termination); a robust one confirms a healthy hybrid coordination-extraction structure killed by exogenous political force.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(exogenous_termination_counterfactual, empirical, 'Whether the 1868 termination reveals endogenous decay or exogenous destruction.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(shinbutsu_ontological_commitment__syncretic_reading, 900, 1868).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(shin_tr_t900, shinbutsu_ontological_commitment__syncretic_reading, theater_ratio, 900, 0.14).
narrative_ontology:measurement(shin_tr_t1100, shinbutsu_ontological_commitment__syncretic_reading, theater_ratio, 1100, 0.17).
narrative_ontology:measurement(shin_tr_t1300, shinbutsu_ontological_commitment__syncretic_reading, theater_ratio, 1300, 0.21).
narrative_ontology:measurement(shin_tr_t1500, shinbutsu_ontological_commitment__syncretic_reading, theater_ratio, 1500, 0.26).
narrative_ontology:measurement(shin_tr_t1700, shinbutsu_ontological_commitment__syncretic_reading, theater_ratio, 1700, 0.29).
narrative_ontology:measurement(shin_tr_t1868, shinbutsu_ontological_commitment__syncretic_reading, theater_ratio, 1868, 0.3).

% Extraction over time
narrative_ontology:measurement(shin_be_t900, shinbutsu_ontological_commitment__syncretic_reading, base_extractiveness, 900, 0.32).
narrative_ontology:measurement(shin_be_t1100, shinbutsu_ontological_commitment__syncretic_reading, base_extractiveness, 1100, 0.42).
narrative_ontology:measurement(shin_be_t1300, shinbutsu_ontological_commitment__syncretic_reading, base_extractiveness, 1300, 0.52).
narrative_ontology:measurement(shin_be_t1500, shinbutsu_ontological_commitment__syncretic_reading, base_extractiveness, 1500, 0.56).
narrative_ontology:measurement(shin_be_t1700, shinbutsu_ontological_commitment__syncretic_reading, base_extractiveness, 1700, 0.62).
narrative_ontology:measurement(shin_be_t1868, shinbutsu_ontological_commitment__syncretic_reading, base_extractiveness, 1868, 0.64).

% Suppression requirement over time
narrative_ontology:measurement(shin_su_t900, shinbutsu_ontological_commitment__syncretic_reading, suppression_requirement, 900, 0.24).
narrative_ontology:measurement(shin_su_t1100, shinbutsu_ontological_commitment__syncretic_reading, suppression_requirement, 1100, 0.36).
narrative_ontology:measurement(shin_su_t1300, shinbutsu_ontological_commitment__syncretic_reading, suppression_requirement, 1300, 0.46).
narrative_ontology:measurement(shin_su_t1500, shinbutsu_ontological_commitment__syncretic_reading, suppression_requirement, 1500, 0.5).
narrative_ontology:measurement(shin_su_t1700, shinbutsu_ontological_commitment__syncretic_reading, suppression_requirement, 1700, 0.6).
narrative_ontology:measurement(shin_su_t1868, shinbutsu_ontological_commitment__syncretic_reading, suppression_requirement, 1868, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(shinbutsu_ontological_commitment__syncretic_reading, identity_coordination).
narrative_ontology:affects_constraint(shinbutsu_ontological_commitment__syncretic_reading, shinbutsu_ontological_commitment__partition_reading).
narrative_ontology:affects_constraint(shinbutsu_ontological_commitment__syncretic_reading, shinbutsu_ontological_commitment__incoherence_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'shinbutsu-shugo' (kami-buddha harmonization) decomposes, per the epsilon-invariance principle, into three structurally distinct claims: this syncretic reading (one unified cosmological order; epsilon ~0.64; victims = shrine lineages' autonomy), the partition reading (separate life-cycle/afterlife domains without ontological integration; no ontological-subordination victims), and the incoherence reading (no stable commitment; the enforcement target dissolves). The syncretic reading is the upstream, traditionally dominant claim that the other two define themselves against; each file links the others through affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
