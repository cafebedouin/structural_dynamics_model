% ============================================================================
% CONSTRAINT STORY: kami_buddha_ontology__honji_suijaku_monism
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [HISTORICAL_ABOLISHED_1868]
% ============================================================================

:- module(constraint_kami_buddha_ontology__honji_suijaku_monism, []).

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
 *   constraint_id: kami_buddha_ontology__honji_suijaku_monism
 *   human_readable: Honji-Suijaku Doctrine: Kami as Phenomenal Traces of Buddhas
 *   domain: religion/cultural_history
 *
 * SUMMARY:
 *   The honji-suijaku doctrine held that kami and buddhas are ontologically
 *   identical: every kami is a phenomenal trace (suijaku) of a buddha or
 *   bodhisattva who is the original ground (honji). Authored and systematized
 *   by the Buddhist scholastic establishment from roughly the ninth century
 *   onward, it was not merely a metaphysical thesis — it was the operating
 *   charter of the medieval Japanese religious economy. It licensed the
 *   erection of shrine-temples inside shrine precincts, the reading of sutras
 *   to kami as beings needing salvation, the subordination of shrine
 *   priesthoods to resident monks, and the channeling of shrine land revenue
 *   and ritual fees toward monastic corporations. It also solved a real
 *   problem: for nearly a millennium it gave the archipelago one workable
 *   religious order in which indigenous cult and imported salvation were both
 *   livable. This story is ONE READING of the contested kami-buddha ontology
 *   kernel; the sibling readings (domain_partition, incoherent_bundle) are
 *   separate constraints with their own epsilon values, victim structures,
 *   and classifications, linked through the network block. The claim/metrics
 *   gap is deliberate: the doctrine is CLAIMED as tangled_rope on structural
 *   grounds (both coordination and asymmetric extraction, actively enforced),
 *   while the metrics are authored from the historical record independently —
 *   the engine computes per-seat classifications from the structural data.
 *
 * KEY AGENTS:
 *   - buddhist_monastic_establishment: agenda_setter (institutional/arbitrage) — authored and enforced the doctrine; collected doctrinal precedence, land income, and ritual fees
 *   - imperial_court: beneficiary with payer costs (institutional/constrained) — sponsored the unified order as its legitimation architecture
 *   - shrine_priest_lineages: primary elite target (moderate/constrained) — lost interpretive precedence over their own deities; could not leave their offices
 *   - provincial_shrine_cults: primary target (powerless/trapped) — absorbed into temple networks without leverage
 *   - peasant_worshippers: dual beneficiary/payer (powerless/mobile) — received the integrated devotional world; paid both institutions
 *   - yoshida_shinto_reformers: excluded internal dissent (moderate/constrained) — inverted the hierarchy from the fifteenth century
 *   - kokugaku_scholars: excluded external critics (moderate/constrained) — built the philological case that the Meiji state executed
 *   - historians_of_religion: analytical observer (analytical/analytical) — reconstructs the dual character from surviving records
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(kami_buddha_ontology__honji_suijaku_monism, 0.62).
domain_priors:suppression_score(kami_buddha_ontology__honji_suijaku_monism, 0.2).
domain_priors:theater_ratio(kami_buddha_ontology__honji_suijaku_monism, 0.46).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(kami_buddha_ontology__honji_suijaku_monism, extractiveness, 0.62).
narrative_ontology:constraint_metric(kami_buddha_ontology__honji_suijaku_monism, suppression_requirement, 0.2).
narrative_ontology:constraint_metric(kami_buddha_ontology__honji_suijaku_monism, theater_ratio, 0.46).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(kami_buddha_ontology__honji_suijaku_monism, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(kami_buddha_ontology__honji_suijaku_monism, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(kami_buddha_ontology__honji_suijaku_monism, tangled_rope).
narrative_ontology:human_readable(kami_buddha_ontology__honji_suijaku_monism, "Honji-Suijaku Doctrine: Kami as Phenomenal Traces of Buddhas").
narrative_ontology:topic_domain(kami_buddha_ontology__honji_suijaku_monism, "religion/cultural_history").

domain_priors:requires_active_enforcement(kami_buddha_ontology__honji_suijaku_monism).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(kami_buddha_ontology__honji_suijaku_monism, '743e2466-29d1-4165-a8d9-3b83861a8465').
narrative_ontology:cs_kernel_codification('743e2466-29d1-4165-a8d9-3b83861a8465', formalized).
narrative_ontology:cs_authority_grounding('743e2466-29d1-4165-a8d9-3b83861a8465', lineage).
narrative_ontology:cs_interpretation_layer_present('743e2466-29d1-4165-a8d9-3b83861a8465').
narrative_ontology:cs_reading_relation('743e2466-29d1-4165-a8d9-3b83861a8465', kami_buddha_ontology__domain_partition, forecloses).
narrative_ontology:cs_reading_relation('743e2466-29d1-4165-a8d9-3b83861a8465', kami_buddha_ontology__incoherent_bundle, coexists_with).
narrative_ontology:cs_axiom('743e2466-29d1-4165-a8d9-3b83861a8465', foundational, kami_lack_independent_existence).
narrative_ontology:cs_axiom_status(kami_lack_independent_existence, holdable).
narrative_ontology:cs_axiom_grounding('743e2466-29d1-4165-a8d9-3b83861a8465', kami_lack_independent_existence, theological).
narrative_ontology:cs_axiom('743e2466-29d1-4165-a8d9-3b83861a8465', secondary, kami_require_buddhist_guidance).
narrative_ontology:cs_axiom_status(kami_require_buddhist_guidance, holdable).
narrative_ontology:cs_axiom_grounding('743e2466-29d1-4165-a8d9-3b83861a8465', kami_require_buddhist_guidance, theological).
narrative_ontology:cs_reference_frame('743e2466-29d1-4165-a8d9-3b83861a8465', buddha_as_honji_hierarchical_cosmology).
narrative_ontology:cs_drift_state('743e2466-29d1-4165-a8d9-3b83861a8465', meiji_shinbutsu_bunri, gap(repudiation_pressure, severe, true)).
narrative_ontology:cs_created_at('743e2466-29d1-4165-a8d9-3b83861a8465', '').
narrative_ontology:cs_kernel_id(kami_buddha_ontology__honji_suijaku_monism, kami_buddha_ontology).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(kami_buddha_ontology__honji_suijaku_monism, buddhist_monastic_establishment).
narrative_ontology:constraint_beneficiary(kami_buddha_ontology__honji_suijaku_monism, imperial_court).
narrative_ontology:constraint_beneficiary(kami_buddha_ontology__honji_suijaku_monism, peasant_worshippers).
narrative_ontology:constraint_victim(kami_buddha_ontology__honji_suijaku_monism, shrine_priest_lineages).
narrative_ontology:constraint_victim(kami_buddha_ontology__honji_suijaku_monism, provincial_shrine_cults).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(kami_buddha_ontology__honji_suijaku_monism, imperial_court).
narrative_ontology:constraint_victim(kami_buddha_ontology__honji_suijaku_monism, peasant_worshippers).
narrative_ontology:constraint_vindicates(kami_buddha_ontology__honji_suijaku_monism, buddhocentric_ontological_monism).
narrative_ontology:constraint_vindicates(kami_buddha_ontology__honji_suijaku_monism, kami_salvation_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The great temple complexes (Tendai, Shingon, Nara schools) authored the doctrine in scholastic treatises, staffed the shrine-temples (jingūji) erected inside shrine precincts, recited sutras before kami on the premise that kami are unenlightened beings needing guidance, and collected ritual fees, land income, and doctrinal precedence as a result. They define what a kami is, so exit from the framework is meaningless for them: they are its authors and principal collectors.
narrative_ontology:constraint_stakeholder(kami_buddha_ontology__honji_suijaku_monism, buddhist_monastic_establishment, agenda_setter,
    institutional, generational, arbitrage, national).

% Sponsored both halves of the unified order: Buddhism as protector of the realm, kami cults as the locus of dynastic and local legitimacy. The doctrine let the court patronize both without contradiction and lent the regime a single sacred calendar. The court also funded rites on both sides and could not abandon either pillar of its legitimation without destabilizing its own position.
narrative_ontology:constraint_stakeholder(kami_buddha_ontology__honji_suijaku_monism, imperial_court, beneficiary,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(kami_buddha_ontology__honji_suijaku_monism, imperial_court, payer).

% Hereditary kannushi families at major shrines watched their deities reclassified as provisional traces requiring Buddhist salvation, with resident monks installed above or beside them holding interpretive precedence. Their office, land, and ancestry are bound to the shrine, so leaving is not a live option; their strategies were petition, negotiation, and occasional successful refusal — Ise Grand Shrine famously kept a Buddhist temple out of its precincts for the entire period.
narrative_ontology:constraint_stakeholder(kami_buddha_ontology__honji_suijaku_monism, shrine_priest_lineages, payer,
    moderate, generational, constrained, regional).

% Local shrines far from court attention were absorbed into temple networks wholesale: Buddhist statues installed in shrine halls, sutras read to the enshrined deity, festival economics rerouted through the supervising temple. They had no leverage to refuse and no forum in which an independent account of their own kami could be articulated.
narrative_ontology:constraint_stakeholder(kami_buddha_ontology__honji_suijaku_monism, provincial_shrine_cults, payer,
    powerless, generational, trapped, regional).

% Received a single working devotional world: shrines for birth, purity, agriculture, and festival; temples for death, funerary rite, and karma. The integration solved a real practical problem for them. They also paid fees, tithes, and labor to both institutions, diffusely and without bargaining power.
narrative_ontology:constraint_stakeholder(kami_buddha_ontology__honji_suijaku_monism, peasant_worshippers, beneficiary,
    powerless, immediate, mobile, local).
narrative_ontology:stakeholder_secondary_role(kami_buddha_ontology__honji_suijaku_monism, peasant_worshippers, payer).

% From the fifteenth century, Yoshida Kanetomo and his successors argued the hierarchy ran the wrong way: kami are the original ground and buddhas the provisional traces. They operated at the margins of the establishment they critiqued, dependent on the very shrine-temple economy they sought to invert, and their counter-doctrine was tolerated as one school among many rather than admitted as a rival foundation.
narrative_ontology:constraint_stakeholder(kami_buddha_ontology__honji_suijaku_monism, yoshida_shinto_reformers, excluded,
    moderate, generational, constrained, national).

% Edo-period nativist scholars (Motoori Norinaga, Hirata Atsutane) argued the entire Buddhist overlay was a usurpation: kami are self-sufficient and never required foreign grounding. Excluded from the doctrinal conversation for centuries, they built the philological case outside it — and their program is what the Meiji state executed in 1868.
narrative_ontology:constraint_stakeholder(kami_buddha_ontology__honji_suijaku_monism, kokugaku_scholars, excluded,
    moderate, civilizational, constrained, national).

% Modern scholarship reconstructs the doctrine's dual character: centuries of genuine integrative work binding two religious systems into one livable order, operating simultaneously as an instrument of monastic aggrandizement. They take testimony from all surviving records and hold no position inside the arrangement.
narrative_ontology:constraint_stakeholder(kami_buddha_ontology__honji_suijaku_monism, historians_of_religion, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(kami_buddha_ontology__honji_suijaku_monism, buddhist_monastic_establishment).
narrative_ontology:fixing_cost_class(kami_buddha_ontology__honji_suijaku_monism, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Bound an indigenous deity cult to an imported salvific religion inside one doctrinal and ritual framework: it explained how local kami and universal buddhas could both be real, standardized the shrine-temple complex, synchronized calendars and festivals, and gave practitioners a single institutional world covering purity and life rites on one side and death and funerary rites on the other.
% TRANSFER_FUNCTION: Moved interpretive authority over kami, shrine land revenues, ritual fees, and doctrinal precedence from shrine custodians and kami cults to Buddhist monastic corporations; moved legitimation upward to the court and later the bakufu as sponsors of the unified order.
% ABSENT_VOICES: Shrine priesthoods whose deities were reclassified without their consent spoke only through petition and occasional refusal; kokugaku scholars entered the conversation roughly eight centuries late; and the kami themselves — the ostensible subjects of the entire doctrine — had no seat at all: monks and priests testified on their behalf about their need for salvation.
% DISAPPEARANCE_RATIONALE: Historically witnessed rather than hypothetical: when the Meiji government severed kami and buddhas in 1868, the shrine-temple complexes split apart, the haibutsu kishaku movement destroyed thousands of temples, hereditary priestly offices were reorganized under state control, and Shinto was reinvented as a national cult. The arrangements built on the doctrine did not survive its removal intact — the world rearranged violently.
% FOUNDING_PROBLEM: The Nara- and Heian-period encounter posed a problem no actor could ignore: the court had adopted Buddhism as a protector of the realm while kami cults remained the locus of local legitimacy and purity. How are local deities and universal buddhas both real, and what does each owe the other?
% FOUNDING_PROBLEM_CORROBORATION: Outside the Buddhist beneficiary set: kokugaku scholars attested that the problem as posed was misconceived — kami are self-sufficient and never required Buddhist grounding; Confucian scholars independently treated the fusion as political accommodation rather than metaphysical necessity; modern historians of religion corroborate that the doctrine performed real integrative work for centuries while simultaneously functioning as an instrument of monastic aggrandizement. No disinterested party attests that the founding problem remained live once the integration had stabilized.
narrative_ontology:disappearance_verdict(kami_buddha_ontology__honji_suijaku_monism, world_rearranges).
narrative_ontology:founding_problem_status(kami_buddha_ontology__honji_suijaku_monism, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(kami_buddha_ontology__honji_suijaku_monism, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(kami_buddha_ontology__honji_suijaku_monism, 'none', 1).
narrative_ontology:epsilon_provenance(kami_buddha_ontology__honji_suijaku_monism, 0.62, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(kami_buddha_ontology__honji_suijaku_monism_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(kami_buddha_ontology__honji_suijaku_monism, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(kami_buddha_ontology__honji_suijaku_monism_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is substantial (0.62 at interval end) because the doctrine decoupled ontological rank from anything the kami cults did: simply by being kami, a deity was ruled a dependent trace, and the material consequences — jingūji endowments, sutra-offering fees, monastic precedence in shrine administration — flowed uphill to the temples. Suppression's end-state value is low (0.20) but its trajectory is the story: enforcement intensity peaked around the fourteenth century (0.55) when the hierarchy still had to be imposed, then decayed through normalization as the framework became hegemonic common sense — a falling suppression series here models enforcement decay through success, not liberation. Theater rises steadily from 0.08 to 0.46: by the late Edo period the doctrine's intellectual legitimacy had been hollowed out by kokugaku critique, and scholastic defense increasingly consisted of restating positions rather than answering the attack — performative maintenance of a structure held by inertia. Accessibility collapse is moderate (0.55): within the doctrinal universe, independent kami-theology was nearly unthinkable for centuries, but Yoshida's inversion proved an alternative was constructible, and kokugaku built a complete one. Resistance is correspondingly high (0.60): Ise's millennium-long exclusion of the jingūji, priestly petitions, the Yoshida reversal, and finally the nativist campaign that ended in state abolition. All three tracked series share one time grid (900–1868, eight points) so no metric's end-state leaks backward into earlier rows.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute very differently. From the agenda-setter seat (buddhist_monastic_establishment), the arrangement is a doctrinal truth it discovered and a coordination order it built: the kami genuinely are traces, and the resource flows are the price of their salvation — a rope-shaped world. From the payer seats (shrine_priest_lineages, provincial_shrine_cults), the same structure operates as enforced subordination: their deities were reclassified without consent and their revenues rerouted — extraction. From the excluded seats (yoshida_shinto_reformers, kokugaku_scholars), it is usurpation wearing metaphysics as a costume. The engine derives these divergent per-seat classifications from the power, exit, and role data; nothing in the authored claim adjudicates between them.
 *
 * DIRECTIONALITY LOGIC:
 *   The beneficiary/victim declarations drive the derivation. The monastic establishment sits at the beneficiary pole (d near 0.0): it collects the transfers and writes the rules, with arbitrage-grade exit since it defines the framework. The imperial court sits low but not minimal (declared beneficiary with a payer secondary role): it collected legitimation but funded both sides. Peasant worshippers sit near symmetric: genuine coordination benefit against diffuse fees. The victim groups sit at the target pole (d near 1.0), amplified by exit structure — provincial_shrine_cults are trapped (no leverage, no forum), pushing them toward the full-target end, while shrine_priest_lineages are constrained rather than trapped (they negotiated, petitioned, and in Ise's case refused outright), sitting marginally nearer the middle. Larger scope (national doctrinal reach) scales verification difficulty and hence effective extraction modestly upward; suppression, by contrast, is authored as a raw structural property and is not scaled.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification guards against two opposite misreadings. Reading the doctrine as pure extraction (snare) erases the centuries of genuine integrative work: it really did solve the kami-buddha encounter problem, standardize the ritual economy, and give ordinary practitioners a single livable world — a snare label would mistake the coordination half for cover. Reading it as pure coordination (rope) erases the asymmetry: the same structure that integrated also subordinated, and the hierarchy's benefits concentrated in precisely the corporations that authored it. Tangled rope holds both facts. The temporal series additionally documents the atrophy path without asserting it completed: the founding problem's status is contested (traditionalist schools kept systematizing; kokugaku declared the problem misconceived), so no mandatrophy-resolution flag is claimed — but the late-Edo signature (rising theater, decaying enforcement, extraction held steady by inertia) is the recognizable pre-piton drift, interrupted not by internal correction but by the Meiji state destroying the arrangement from outside.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_position,
    'This constraint is one reading (honji_suijaku_monism) of the contested kami_buddha_ontology kernel — how would instantiating the sibling readings change the structural picture?',
    'Comparative analysis across the three reading files: domain_partition assigns kami and buddhas separate functional domains (different victim set — nobody''s deity is demoted, but death/purity traffic is segregated); incoherent_bundle denies a coherent kernel exists and decomposes the complex into micro-constraints with individually computed extraction.',
    'Under domain_partition the hierarchical extraction structure disappears (no ontological subordination to enforce) and epsilon drops sharply; under incoherent_bundle this story''s unit of analysis dissolves and its epsilon becomes a weighted composite of bundle elements.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_position, conceptual, 'Committer-frame position: this story instantiates one of three declared readings of the kami-buddha ontology kernel.').

omega_variable(
    sincere_metaphysics_vs_instrument,
    'Was the ontological claim sincerely held metaphysics, an instrument of institutional aggrandizement, or both at once — and does the answer change what the theater ratio measures?',
    'Internal analysis of scholastic treatises (argumentative quality, whether objections are engaged or restated) cross-checked against the timing of institutional gains following doctrinal innovations; historian assessment of doctrinal development independent of revenue events.',
    'If sincerely held, the rising late-Edo theater ratio reads as failed intellectual defense of a真 belief; if instrumental, the same rise reads as the mask slipping off a rent-collection mechanism — the latter supports higher effective extraction attribution to the agenda-setter seat.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sincere_metaphysics_vs_instrument, conceptual, 'Whether the doctrine''s operators believed their own ontology, which governs how theater_ratio should be interpreted.').

omega_variable(
    coordination_extraction_separability,
    'How much of the measured extraction is the inherent cost of running a combined shrine-temple ritual economy versus rent taken through ontological subordination?',
    'Compare shrine complexes that resisted absorption (Ise, which excluded the jingūji entirely) with absorbed complexes of comparable size: if resistant shrines sustained comparable ritual vitality at lower monastic overhead, the overhead was rent rather than coordination cost.',
    'If separable, the excess extraction above the identity-coordination floor is attributable to the hierarchy specifically and remedies short of full separation become coherent; if inseparable, part of the measured extraction is the price of the integration itself.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_extraction_separability, empirical, 'Whether the doctrine''s coordination and extraction components are structurally separable, testable against resistant-shrine natural experiments.').

omega_variable(
    persistence_without_enforcement,
    'Would the integrated kami-buddha framework have persisted in the absence of institutional enforcement, or was continuous enforcement load-bearing throughout?',
    'Examine periods of weakened central authority (the Sengoku fragmentation, roughly 1467–1600): if shrine-temple practice and lay adherence held while enforcement capacity lapsed, the framework was self-sustaining and the suppression series overstates its structural necessity.',
    'High persistence under enforcement lapse would lower the constraint''s effective suppression attribution and strengthen the rope-half of the tangled_rope verdict; rapid decay would confirm enforcement dependence and push the reading toward the snare boundary.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(persistence_without_enforcement, empirical, 'Counterfactual persistence of the arrangement absent enforcement, resolvable from Sengoku-era practice continuity.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(kami_buddha_ontology__honji_suijaku_monism, 900, 1868).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(kami_tr_t900, kami_buddha_ontology__honji_suijaku_monism, theater_ratio, 900, 0.08).
narrative_ontology:measurement(kami_tr_t1050, kami_buddha_ontology__honji_suijaku_monism, theater_ratio, 1050, 0.12).
narrative_ontology:measurement(kami_tr_t1200, kami_buddha_ontology__honji_suijaku_monism, theater_ratio, 1200, 0.18).
narrative_ontology:measurement(kami_tr_t1350, kami_buddha_ontology__honji_suijaku_monism, theater_ratio, 1350, 0.23).
narrative_ontology:measurement(kami_tr_t1500, kami_buddha_ontology__honji_suijaku_monism, theater_ratio, 1500, 0.29).
narrative_ontology:measurement(kami_tr_t1650, kami_buddha_ontology__honji_suijaku_monism, theater_ratio, 1650, 0.34).
narrative_ontology:measurement(kami_tr_t1800, kami_buddha_ontology__honji_suijaku_monism, theater_ratio, 1800, 0.43).
narrative_ontology:measurement(kami_tr_t1868, kami_buddha_ontology__honji_suijaku_monism, theater_ratio, 1868, 0.46).

% Extraction over time
narrative_ontology:measurement(kami_be_t900, kami_buddha_ontology__honji_suijaku_monism, base_extractiveness, 900, 0.28).
narrative_ontology:measurement(kami_be_t1050, kami_buddha_ontology__honji_suijaku_monism, base_extractiveness, 1050, 0.4).
narrative_ontology:measurement(kami_be_t1200, kami_buddha_ontology__honji_suijaku_monism, base_extractiveness, 1200, 0.52).
narrative_ontology:measurement(kami_be_t1350, kami_buddha_ontology__honji_suijaku_monism, base_extractiveness, 1350, 0.6).
narrative_ontology:measurement(kami_be_t1500, kami_buddha_ontology__honji_suijaku_monism, base_extractiveness, 1500, 0.63).
narrative_ontology:measurement(kami_be_t1650, kami_buddha_ontology__honji_suijaku_monism, base_extractiveness, 1650, 0.62).
narrative_ontology:measurement(kami_be_t1800, kami_buddha_ontology__honji_suijaku_monism, base_extractiveness, 1800, 0.61).
narrative_ontology:measurement(kami_be_t1868, kami_buddha_ontology__honji_suijaku_monism, base_extractiveness, 1868, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(kami_su_t900, kami_buddha_ontology__honji_suijaku_monism, suppression_requirement, 900, 0.3).
narrative_ontology:measurement(kami_su_t1050, kami_buddha_ontology__honji_suijaku_monism, suppression_requirement, 1050, 0.42).
narrative_ontology:measurement(kami_su_t1200, kami_buddha_ontology__honji_suijaku_monism, suppression_requirement, 1200, 0.5).
narrative_ontology:measurement(kami_su_t1350, kami_buddha_ontology__honji_suijaku_monism, suppression_requirement, 1350, 0.55).
narrative_ontology:measurement(kami_su_t1500, kami_buddha_ontology__honji_suijaku_monism, suppression_requirement, 1500, 0.53).
narrative_ontology:measurement(kami_su_t1650, kami_buddha_ontology__honji_suijaku_monism, suppression_requirement, 1650, 0.45).
narrative_ontology:measurement(kami_su_t1800, kami_buddha_ontology__honji_suijaku_monism, suppression_requirement, 1800, 0.38).
narrative_ontology:measurement(kami_su_t1868, kami_buddha_ontology__honji_suijaku_monism, suppression_requirement, 1868, 0.2).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(kami_buddha_ontology__honji_suijaku_monism, identity_coordination).
narrative_ontology:affects_constraint(kami_buddha_ontology__honji_suijaku_monism, kami_buddha_ontology__domain_partition).
narrative_ontology:affects_constraint(kami_buddha_ontology__honji_suijaku_monism, kami_buddha_ontology__incoherent_bundle).

% DUAL FORMULATION NOTE:
% Constraint family: three readings of the kami_buddha_ontology kernel, decomposed per the epsilon-invariance principle because the colloquial label 'shinbutsu-shugo' conflates structurally distinct claims. This file instantiates honji_suijaku_monism (ontological identity, Buddhist entities prior): epsilon 0.62, victims are the subordinated shrine seats. The domain_partition sibling (functional segregation, no ontological subordination) carries a different victim structure and materially lower epsilon — nothing in it demotes anyone's deity. The incoherent_bundle sibling denies the kernel's coherence altogether and decomposes the complex into separately-classified micro-constraints. This reading is upstream of both historically: the monist hierarchy's dominance is what made functional separation thinkable as a reform demand and what the bundle analysis takes as its primary evidence of contradiction.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
