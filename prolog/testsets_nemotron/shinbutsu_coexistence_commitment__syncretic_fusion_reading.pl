% ============================================================================
% CONSTRAINT STORY: shinbutsu_coexistence_commitment__syncretic_fusion_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_shinbutsu_coexistence_commitment__syncretic_fusion_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: shinbutsu_coexistence_commitment__syncretic_fusion_reading
 *   human_readable: Honji Suijaku Ontological Unification (Syncretic Fusion Reading)
 *   domain: religious/philosophical/historical
 *
 * SUMMARY:
 *   The honji suijaku (original ground, trace manifestation) doctrine unified
 *   kami and Buddhist deities by positioning kami as local Japanese
 *   manifestations (suijaku) of universal Buddhist truths (honji). This
 *   created a single coherent ontology where every kami had a Buddhist
 *   original — Amaterasu as Dainichi Nyorai, Hachiman as Amida, etc. The
 *   constraint operated through the jinguji (shrine-temple) institutional
 *   network, where Buddhist monks performed rites for kami, and through court
 *   ritual bureaucracy that codified the correspondences. The arrangement
 *   solved a genuine coordination problem: integrating indigenous ritual life
 *   with continental Buddhist soteriology without requiring populations to
 *   abandon either. But it also extracted — local kami cults lost autonomous
 *   doctrinal authority, non-Buddhist ritual specialists were marginalized,
 *   and indigenous traditions were absorbed into a Buddhist frame. The
 *   constraint persisted through active enforcement (imperial edicts,
 *   monastic discipline, jinguji administrative control) and degraded over
 *   time as doctrinal elaboration outpaced ritual coherence, becoming
 *   increasingly performative by the late Heian period.
 *
 * KEY AGENTS:
 *   - buddhist_monastic_establishment: Primary beneficiary (institutional/powerful) — controls doctrinal interpretation, receives land/tax benefits, administers jinguji
 *   - imperial_court_ritual_bureaucracy: Beneficiary (institutional/moderate) — legitimates rule through unified ritual system, controls shrine appointments
 *   - jinguji_institutional_network: Beneficiary/agenda_setter (organized/powerful) — structural embodiment of the fusion, collects offerings, manages merged estates
 *   - local_kami_cult_practitioners: Victim (moderate/constrained) — lose autonomous ritual authority, must conform to Buddhist doctrinal framework
 *   - non_buddhist_ritual_specialists: Victim (powerless/trapped) — onmyoji, miko, and other specialists displaced or subordinated
 *   - indigenous_ritual_traditions: Victim (powerless/identity_locked) — absorbed into Buddhist ontology, continuity maintained only through Buddhist forms
 *   - lay_population: Payer/beneficiary (organized/constrained) — gains unified soteriological framework but bears extraction through offerings and labor
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(shinbutsu_coexistence_commitment__syncretic_fusion_reading, 0.48).
domain_priors:suppression_score(shinbutsu_coexistence_commitment__syncretic_fusion_reading, 0.62).
domain_priors:theater_ratio(shinbutsu_coexistence_commitment__syncretic_fusion_reading, 0.31).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(shinbutsu_coexistence_commitment__syncretic_fusion_reading, extractiveness, 0.48).
narrative_ontology:constraint_metric(shinbutsu_coexistence_commitment__syncretic_fusion_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(shinbutsu_coexistence_commitment__syncretic_fusion_reading, theater_ratio, 0.31).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(shinbutsu_coexistence_commitment__syncretic_fusion_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(shinbutsu_coexistence_commitment__syncretic_fusion_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(shinbutsu_coexistence_commitment__syncretic_fusion_reading, tangled_rope).
narrative_ontology:human_readable(shinbutsu_coexistence_commitment__syncretic_fusion_reading, "Honji Suijaku Ontological Unification (Syncretic Fusion Reading)").
narrative_ontology:topic_domain(shinbutsu_coexistence_commitment__syncretic_fusion_reading, "religious/philosophical/historical").

domain_priors:requires_active_enforcement(shinbutsu_coexistence_commitment__syncretic_fusion_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(shinbutsu_coexistence_commitment__syncretic_fusion_reading, '9d3599f1-cd1a-4f69-94cc-86a8d250ed55').
narrative_ontology:cs_kernel_codification('9d3599f1-cd1a-4f69-94cc-86a8d250ed55', distributed).
narrative_ontology:cs_authority_grounding('9d3599f1-cd1a-4f69-94cc-86a8d250ed55', lineage).
narrative_ontology:cs_interpretation_layer_present('9d3599f1-cd1a-4f69-94cc-86a8d250ed55').
narrative_ontology:cs_reading_relation('9d3599f1-cd1a-4f69-94cc-86a8d250ed55', shinbutsu_coexistence_commitment__domain_partition_reading, influences).
narrative_ontology:cs_reading_relation('9d3599f1-cd1a-4f69-94cc-86a8d250ed55', shinbutsu_coexistence_commitment__incoherent_bundle_reading, coexists_with).
narrative_ontology:cs_axiom('9d3599f1-cd1a-4f69-94cc-86a8d250ed55', foundational, kami_are_suijaku_of_buddhas).
narrative_ontology:cs_axiom_status(kami_are_suijaku_of_buddhas, holdable).
narrative_ontology:cs_axiom_grounding('9d3599f1-cd1a-4f69-94cc-86a8d250ed55', kami_are_suijaku_of_buddhas, deontological).
narrative_ontology:cs_axiom('9d3599f1-cd1a-4f69-94cc-86a8d250ed55', foundational, honji_suijaku_exhausts_kami_identity).
narrative_ontology:cs_axiom_status(honji_suijaku_exhausts_kami_identity, holdable).
narrative_ontology:cs_axiom_grounding('9d3599f1-cd1a-4f69-94cc-86a8d250ed55', honji_suijaku_exhausts_kami_identity, conventional).
narrative_ontology:cs_reference_frame('9d3599f1-cd1a-4f69-94cc-86a8d250ed55', nara_heian_honji_suijaku_consensus).
narrative_ontology:cs_drift_state('9d3599f1-cd1a-4f69-94cc-86a8d250ed55', late_heian_doctrinal_proliferation, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('9d3599f1-cd1a-4f69-94cc-86a8d250ed55', '').
narrative_ontology:cs_kernel_id(shinbutsu_coexistence_commitment__syncretic_fusion_reading, shinbutsu_coexistence_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(shinbutsu_coexistence_commitment__syncretic_fusion_reading, buddhist_monastic_establishment).
narrative_ontology:constraint_beneficiary(shinbutsu_coexistence_commitment__syncretic_fusion_reading, imperial_court_ritual_bureaucracy).
narrative_ontology:constraint_beneficiary(shinbutsu_coexistence_commitment__syncretic_fusion_reading, jinguji_institutional_network).
narrative_ontology:constraint_victim(shinbutsu_coexistence_commitment__syncretic_fusion_reading, local_kami_cult_practitioners).
narrative_ontology:constraint_victim(shinbutsu_coexistence_commitment__syncretic_fusion_reading, non_buddhist_ritual_specialists).
narrative_ontology:constraint_victim(shinbutsu_coexistence_commitment__syncretic_fusion_reading, indigenous_ritual_traditions).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(shinbutsu_coexistence_commitment__syncretic_fusion_reading, lay_population).
narrative_ontology:constraint_victim(shinbutsu_coexistence_commitment__syncretic_fusion_reading, lay_population).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Controls the honji suijaku doctrinal framework — determines which kami correspond to which Buddhas, authors commentaries, trains monks. Receives massive land grants, tax exemptions, and ritual prerogatives through jinguji administration. Exit is arbitrage-grade: they could (and did) pivot to pure Buddhist institutions when the fusion became politically costly.
narrative_ontology:constraint_stakeholder(shinbutsu_coexistence_commitment__syncretic_fusion_reading, buddhist_monastic_establishment, beneficiary,
    institutional, generational, arbitrage, national).

% Uses the unified ritual system to legitimate imperial authority — the emperor as ritual center of a cosmologically integrated realm. Controls shrine appointments and rank (kan'i) through the Jingikan. Gains coordination benefit (unified ritual calendar, standardized protocol) but bears administrative costs. Exit is mobile: the court could (and eventually did) shift to alternative legitimacy bases.
narrative_ontology:constraint_stakeholder(shinbutsu_coexistence_commitment__syncretic_fusion_reading, imperial_court_ritual_bureaucracy, beneficiary,
    institutional, generational, mobile, national).

% The physical and administrative embodiment of shinbutsu-shugo: shrine-temple complexes where Buddhist monks perform rites for kami. Manages merged estates, collects offerings from both traditions, enforces doctrinal compliance locally. Sets the agenda for local ritual practice. Constrained exit: dissolving a jinguji means abandoning centuries of accumulated land, parishioners, and institutional memory.
narrative_ontology:constraint_stakeholder(shinbutsu_coexistence_commitment__syncretic_fusion_reading, jinguji_institutional_network, agenda_setter,
    organized, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(shinbutsu_coexistence_commitment__syncretic_fusion_reading, jinguji_institutional_network, beneficiary).

% Hereditary shrine families (shake) and local ritual specialists who lose autonomous doctrinal authority — their kami are now 'manifestations' of Buddhist originals defined by distant monastic elites. Must conform to Buddhist liturgical forms, accept monk oversight, share offerings. Constrained exit: they can resist correspondence assignments but cannot exit the system without losing shrine status and community standing.
narrative_ontology:constraint_stakeholder(shinbutsu_coexistence_commitment__syncretic_fusion_reading, local_kami_cult_practitioners, payer,
    moderate, biographical, constrained, local).

% Onmyoji (yin-yang masters), miko (shamanesses), ascetics, and folk ritualists operating outside Buddhist institutions. Displaced from court patronage, barred from jinguji, marginalized as 'heterodox' or 'superstitious.' Trapped exit: no alternative institutional home, their knowledge traditions have no recognized path to legitimacy within the honji suijaku framework.
narrative_ontology:constraint_stakeholder(shinbutsu_coexistence_commitment__syncretic_fusion_reading, non_buddhist_ritual_specialists, payer,
    powerless, immediate, trapped, local).

% The living ritual practices of communities — festivals, agricultural rites, lifecycle ceremonies — that continue only by being absorbed into Buddhist forms. The kami's identity is now constitutively Buddhist; to practice the tradition IS to participate in the honji suijaku frame. Identity-locked exit: abandoning the frame means abandoning the tradition itself — there is no 'pure' indigenous practice recoverable from within the system.
narrative_ontology:constraint_stakeholder(shinbutsu_coexistence_commitment__syncretic_fusion_reading, indigenous_ritual_traditions, payer,
    powerless, generational, identity_locked, local).

% Gains a unified soteriological framework: kami protect this life, Buddhas save the next, and the two are one system. Bears extraction through offerings, labor for jinguji construction, and mandatory participation in Buddhist-ritual calendar. Constrained exit: no alternative ritual economy exists at scale; 'opting out' means social marginalization.
narrative_ontology:constraint_stakeholder(shinbutsu_coexistence_commitment__syncretic_fusion_reading, lay_population, payer,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(shinbutsu_coexistence_commitment__syncretic_fusion_reading, lay_population, beneficiary).

% Tendai, Shingon, and later schools contest specific honji suijaku correspondences (e.g., which Buddha is Amaterasu's honji) but accept the framework's legitimacy. Their disputes are intra-framework — they compete for interpretive authority, not against the constraint itself.
narrative_ontology:constraint_stakeholder(shinbutsu_coexistence_commitment__syncretic_fusion_reading, rival_buddhist_schools, observer,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Integrated indigenous Japanese ritual life (kami worship) with continental Buddhist soteriology into a single coherent system — solving the civilizational problem of adopting a foreign religion without rupturing native practice, and solving the Buddhist problem of making the dharma intelligible in a Japanese context.
% TRANSFER_FUNCTION: Moves doctrinal authority and ritual sovereignty from local kami cults and non-Buddhist specialists to the Buddhist monastic establishment and court bureaucracy; moves material resources (land, offerings, labor) from local communities to jinguji institutions; moves legitimacy from autonomous traditions to the unified honji suijaku frame.
% ABSENT_VOICES: The kami themselves (as understood by their pre-Buddhist devotees) — the honji suijaku frame speaks for them by defining their 'true nature' as Buddhist. Pre-literate ritual traditions with no textual record. Women's ritual communities (miko-led) that were systematically displaced by male monastic authority. These voices are structurally excluded: the constraint's coherence depends on their silence.
% DISAPPEARANCE_RATIONALE: If honji suijaku vanished overnight in 1000 CE: jinguji would lose their doctrinal rationale and administrative unity; local kami cults would reclaim autonomous authority; Buddhist schools would lose their primary interface with the population; the court would lose its unified ritual legitimacy base. The entire ritual-political economy of Heian Japan would reorganize — not collapse, but fundamentally restructure around new coordination mechanisms.
% FOUNDING_PROBLEM: How to incorporate Buddhism as a continental civilization-package (soteriology, philosophy, ritual, institutional forms) into Japan without either rejecting it as foreign or abandoning indigenous ritual life as 'superstition' — a civilizational integration problem facing the Yamato court and Buddhist missionaries from the 6th century onward.
% FOUNDING_PROBLEM_CORROBORATION: The Buddhist establishment attests the problem remains live (ongoing need to make dharma accessible). The court bureaucracy attests it is substantially solved (Buddhism is established, the fusion has served its purpose). Indigenous traditions (via later kokugaku scholars) attest it was never a problem for them — the fusion created the problem of erasure. No single corroboration exists outside the beneficiary set; the contestation itself is the structural fact.
narrative_ontology:disappearance_verdict(shinbutsu_coexistence_commitment__syncretic_fusion_reading, world_rearranges).
narrative_ontology:founding_problem_status(shinbutsu_coexistence_commitment__syncretic_fusion_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(shinbutsu_coexistence_commitment__syncretic_fusion_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(shinbutsu_coexistence_commitment__syncretic_fusion_reading, 'none', 1).
narrative_ontology:epsilon_provenance(shinbutsu_coexistence_commitment__syncretic_fusion_reading, 0.48, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(shinbutsu_coexistence_commitment__syncretic_fusion_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(shinbutsu_coexistence_commitment__syncretic_fusion_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(shinbutsu_coexistence_commitment__syncretic_fusion_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.48) reflects the constraint's dual nature: genuine coordination (integrating two ritual systems) with substantial extraction (Buddhist monopoly on doctrinal authority, marginalization of alternatives). Suppression (0.62) is high because the constraint required active enforcement — imperial edicts mandating jinguji, monastic discipline policing doctrinal boundaries, court control of shrine appointments. Theater ratio (0.31) rises over time as doctrinal elaboration (e.g., honji suijaku correspondences proliferating to hundreds of kami) outpaces ritual coherence — by late Heian the system performs unity while local practice diverges. Accessibility collapse (0.68) is substantial: once the honji suijaku frame is accepted, alternative framings (domain partition, pure indigenous practice) become structurally difficult to articulate. Resistance (0.45) is moderate: local cults resisted absorption but lacked organized counter-institutional power; the primary resistance came from rival Buddhist schools contesting correspondences, not from indigenous traditions.
 *
 * PERSPECTIVAL GAP:
 *   The Buddhist monastic establishment experiences this as a rope — they built and maintain a coordination structure that integrates the population into a unified soteriological system. Local kami practitioners experience it as a snare — their autonomy is extracted through a coordination story they did not author. The imperial court experiences it as a scaffold — a transitional arrangement that legitimizes their rule while the population is gradually Buddhistized. The engine computes these seat divergences from the structural data: beneficiaries with institutional power and arbitrage-grade exit (monastic establishment) sit at low d; victims with identity-locked exit and constrained options (indigenous traditions) sit at high d.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries declared: buddhist_monastic_establishment (controls interpretation, collects rents), imperial_court_ritual_bureaucracy (legitimacy through unified ritual), jinguji_institutional_network (structural embodiment, resource control). Victims declared: local_kami_cult_practitioners (lose doctrinal autonomy), non_buddhist_ritual_specialists (displaced), indigenous_ritual_traditions (absorbed). Directionality derives from this: monastic establishment d~0.15 (beneficiary with institutional power), court bureaucracy d~0.25 (beneficiary with moderate power), jinguji d~0.2 (agenda_setter with organized power), local practitioners d~0.75 (constrained exit, moderate power), ritual specialists d~0.9 (trapped, powerless), indigenous traditions d~0.95 (identity_locked, powerless). Lay population sits near symmetric d~0.5 (constrained exit, organized power, both benefits and pays).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (integrating indigenous and Buddhist ritual life without civilizational rupture) was live through the early Heian period. By late Heian (10th-12th century), the coordination function had substantially degraded into doctrinal performance — the founding problem was largely solved (Buddhism was established) but the constraint persisted and intensified extraction. This is a classic mandatrophy trajectory: scaffold→tangled_rope→piton. The constraint was never formally sunset; Meiji Shinbutsu Bunri (1868) violently terminated it. The reading's classification as tangled_rope captures the mid-Heian period when coordination and extraction were both structurally real — not the terminal piton phase.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_status,
    'Is this constraint one reading of a contested kernel (shinbutsu_coexistence_commitment) rather than a standalone constraint?',
    'Comparative analysis of sibling readings (domain_partition_reading, incoherent_bundle_reading) to establish distinct ε, beneficiary/victim structures, and classification outcomes per reading.',
    'If confirmed as a kernel reading, this constraint''s ε, stakeholder structure, and type are reading-indexed properties — not properties of the kernel itself. The other readings instantiate separate constraints with their own metrics.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_status, conceptual, 'Commits this constraint to the committer frame: one reading, one ε, one classification. Sibling readings are separate constraint files linked via network.affects_constraints.').

omega_variable(
    structural_delta_verification,
    'Does the syncretic_fusion_reading genuinely instantiate the expected structural delta: single coherent ontology, high doctrinal consistency constraint, theological elite interpretive authority, jinguji as structural embodiment?',
    'Historical analysis of Nara-Heian period jinguji records, doctrinal commentaries, and institutional charters to verify whether the reading''s claimed structural features match the operative arrangement.',
    'If the structural delta is not borne out, the reading''s claimed_type (tangled_rope) and its beneficiary/victim structure may misrepresent the actual constraint — the engine would compute a different classification from the structural data.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(structural_delta_verification, empirical, 'Validates the source material''s structural claims against the historical record for this specific reading.').

omega_variable(
    beneficiary_structure_ambiguity,
    'Are the declared beneficiaries (buddhist_monastic_establishment, imperial_court_ritual_bureaucracy, jinguji_institutional_network) the actual rent-capturing agents, or do they primarily bear the coordination costs of maintaining doctrinal unity?',
    'Analysis of land grants, tax exemptions, and ritual prerogatives flowing to each group under the honji suijaku framework versus the administrative burdens they absorbed.',
    'If the monastic establishment and court bureaucracy were net payers rather than net beneficiaries, the constraint''s extraction profile and classification would shift — the engine''s directionality computation depends on accurate beneficiary/victim declarations.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_structure_ambiguity, empirical, 'Tests whether the coordination function''s beneficiaries are correctly identified or whether the constraint operates as a piton/scaffold with diffuse costs.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(shinbutsu_coexistence_commitment__syncretic_fusion_reading, 710, 1185).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(shin_tr_t710, shinbutsu_coexistence_commitment__syncretic_fusion_reading, theater_ratio, 710, 0.18).
narrative_ontology:measurement(shin_tr_t794, shinbutsu_coexistence_commitment__syncretic_fusion_reading, theater_ratio, 794, 0.22).
narrative_ontology:measurement(shin_tr_t850, shinbutsu_coexistence_commitment__syncretic_fusion_reading, theater_ratio, 850, 0.27).
narrative_ontology:measurement(shin_tr_t950, shinbutsu_coexistence_commitment__syncretic_fusion_reading, theater_ratio, 950, 0.31).
narrative_ontology:measurement(shin_tr_t1050, shinbutsu_coexistence_commitment__syncretic_fusion_reading, theater_ratio, 1050, 0.34).
narrative_ontology:measurement(shin_tr_t1185, shinbutsu_coexistence_commitment__syncretic_fusion_reading, theater_ratio, 1185, 0.31).

% Extraction over time
narrative_ontology:measurement(shin_be_t710, shinbutsu_coexistence_commitment__syncretic_fusion_reading, base_extractiveness, 710, 0.32).
narrative_ontology:measurement(shin_be_t794, shinbutsu_coexistence_commitment__syncretic_fusion_reading, base_extractiveness, 794, 0.41).
narrative_ontology:measurement(shin_be_t850, shinbutsu_coexistence_commitment__syncretic_fusion_reading, base_extractiveness, 850, 0.48).
narrative_ontology:measurement(shin_be_t950, shinbutsu_coexistence_commitment__syncretic_fusion_reading, base_extractiveness, 950, 0.52).
narrative_ontology:measurement(shin_be_t1050, shinbutsu_coexistence_commitment__syncretic_fusion_reading, base_extractiveness, 1050, 0.55).
narrative_ontology:measurement(shin_be_t1185, shinbutsu_coexistence_commitment__syncretic_fusion_reading, base_extractiveness, 1185, 0.48).

% Suppression requirement over time
narrative_ontology:measurement(shin_su_t710, shinbutsu_coexistence_commitment__syncretic_fusion_reading, suppression_requirement, 710, 0.45).
narrative_ontology:measurement(shin_su_t794, shinbutsu_coexistence_commitment__syncretic_fusion_reading, suppression_requirement, 794, 0.52).
narrative_ontology:measurement(shin_su_t850, shinbutsu_coexistence_commitment__syncretic_fusion_reading, suppression_requirement, 850, 0.58).
narrative_ontology:measurement(shin_su_t950, shinbutsu_coexistence_commitment__syncretic_fusion_reading, suppression_requirement, 950, 0.62).
narrative_ontology:measurement(shin_su_t1050, shinbutsu_coexistence_commitment__syncretic_fusion_reading, suppression_requirement, 1050, 0.65).
narrative_ontology:measurement(shin_su_t1185, shinbutsu_coexistence_commitment__syncretic_fusion_reading, suppression_requirement, 1185, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(shinbutsu_coexistence_commitment__syncretic_fusion_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(shinbutsu_coexistence_commitment__syncretic_fusion_reading, 0.08).
narrative_ontology:affects_constraint(shinbutsu_coexistence_commitment__syncretic_fusion_reading, shinbutsu_coexistence_commitment__domain_partition_reading).
narrative_ontology:affects_constraint(shinbutsu_coexistence_commitment__syncretic_fusion_reading, shinbutsu_coexistence_commitment__incoherent_bundle_reading).
narrative_ontology:affects_constraint(shinbutsu_coexistence_commitment__syncretic_fusion_reading, meiji_shinbutsu_bunri).

% DUAL FORMULATION NOTE:
% The shinbutsu_coexistence_commitment kernel decomposes into three structurally distinct constraints: this syncretic_fusion_reading (honji suijaku ontological unification, tangled_rope), domain_partition_reading (separate domains coordination, likely rope), and incoherent_bundle_reading (ambiguity-maintained bundle, likely snare or piton). This reading's honji suijaku doctrine structurally influences the domain_partition_reading by providing the theological vocabulary the partition reading must negotiate with, and influences the incoherent_bundle_reading by supplying the coherent doctrine the bundle reading claims never existed. All three are linked bidirectionally in the constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(shinbutsu_coexistence_commitment__syncretic_fusion_reading, institutional, 0.15).
constraint_indexing:directionality_override(shinbutsu_coexistence_commitment__syncretic_fusion_reading, organized, 0.2).
constraint_indexing:directionality_override(shinbutsu_coexistence_commitment__syncretic_fusion_reading, moderate, 0.75).
constraint_indexing:directionality_override(shinbutsu_coexistence_commitment__syncretic_fusion_reading, powerless, 0.9).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
