% ============================================================================
% CONSTRAINT STORY: shinbutsu_coexistence_commitment__syncretic_fusion_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
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
 *   human_readable: Honji Suijaku: Kami as Local Manifestations of Universal Buddhist Truth
 *   domain: religious_studies/philosophy_of_religion/japanese_history
 *
 * SUMMARY:
 *   This constraint authors the syncretic fusion reading of the
 *   shinbutsu-shugo kernel: the honji-suijaku doctrine as an ontologically
 *   genuine unification in which kami are understood as local manifestations
 *   (suijaku, 'trace') of universal Buddhist principles (honji, 'original
 *   ground'). Under this reading, the doctrine is not a diplomatic
 *   accommodation between two irreducibly separate systems (the
 *   domain-partition reading) nor an incoherent institutional bundle papering
 *   over contradiction (the incoherent-bundle reading) — it is a single
 *   coherent theological achievement with real metaphysical content,
 *   elaborated by monastic scholarship and embodied structurally in the
 *   combined shrine-temple (jinguji) institution. Under this reading's own
 *   lights, the doctrine's coordination function (avoiding sectarian
 *   conflict, integrating two traditions into one practice) is real, but it
 *   also concentrates interpretive authority in Buddhist theological elites
 *   and imposes doctrinal subordination on kami traditions that do not map
 *   cleanly onto Buddhist soteriology — that concentration and subordination
 *   is what the ε in this story measures, assessed from within the fusion
 *   reading's own commitments, not from the standpoint of the reading's own
 *   preferred alternative.
 *
 * KEY AGENTS:
 *   - jinguji_temple_shrine_complexes: institutional beneficiary and structural embodiment of the fused ontology — administers dual clergy and combined landholding
 *   - buddhist_theological_elite: agenda-setter — produces and adjudicates the honji-suijaku correspondences, holding interpretive authority over doctrinal orthodoxy
 *   - shrine_priests_of_unaffiliated_kami: payer — bears reclassification and loss of standing when their kami resists clean Buddhist correspondence
 *   - local_kami_cults_with_incompatible_cosmology: payer, powerless, trapped — have no voice in the assignment of their own 'original ground'
 *   - religious_studies_scholars: analytical observer of the historical and textual record
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(shinbutsu_coexistence_commitment__syncretic_fusion_reading, 0.58).
domain_priors:suppression_score(shinbutsu_coexistence_commitment__syncretic_fusion_reading, 0.52).
domain_priors:theater_ratio(shinbutsu_coexistence_commitment__syncretic_fusion_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(shinbutsu_coexistence_commitment__syncretic_fusion_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(shinbutsu_coexistence_commitment__syncretic_fusion_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(shinbutsu_coexistence_commitment__syncretic_fusion_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(shinbutsu_coexistence_commitment__syncretic_fusion_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(shinbutsu_coexistence_commitment__syncretic_fusion_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(shinbutsu_coexistence_commitment__syncretic_fusion_reading, tangled_rope).
narrative_ontology:human_readable(shinbutsu_coexistence_commitment__syncretic_fusion_reading, "Honji Suijaku: Kami as Local Manifestations of Universal Buddhist Truth").
narrative_ontology:topic_domain(shinbutsu_coexistence_commitment__syncretic_fusion_reading, "religious_studies/philosophy_of_religion/japanese_history").

domain_priors:requires_active_enforcement(shinbutsu_coexistence_commitment__syncretic_fusion_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(shinbutsu_coexistence_commitment__syncretic_fusion_reading, 'e925c56c-8b02-4a91-a17d-a0e013f5ed41').
narrative_ontology:cs_kernel_codification('e925c56c-8b02-4a91-a17d-a0e013f5ed41', distributed).
narrative_ontology:cs_authority_grounding('e925c56c-8b02-4a91-a17d-a0e013f5ed41', lineage).
narrative_ontology:cs_interpretation_layer_present('e925c56c-8b02-4a91-a17d-a0e013f5ed41').
narrative_ontology:cs_reading_relation('e925c56c-8b02-4a91-a17d-a0e013f5ed41', shinbutsu_coexistence_commitment__domain_partition_reading, forecloses).
narrative_ontology:cs_reading_relation('e925c56c-8b02-4a91-a17d-a0e013f5ed41', shinbutsu_coexistence_commitment__incoherent_bundle_reading, forecloses).
narrative_ontology:cs_axiom('e925c56c-8b02-4a91-a17d-a0e013f5ed41', foundational, single_ontological_ground_thesis).
narrative_ontology:cs_axiom_status(single_ontological_ground_thesis, holdable).
narrative_ontology:cs_axiom_grounding('e925c56c-8b02-4a91-a17d-a0e013f5ed41', single_ontological_ground_thesis, theological).
narrative_ontology:cs_axiom('e925c56c-8b02-4a91-a17d-a0e013f5ed41', foundational, kami_as_genuine_trace_manifestation).
narrative_ontology:cs_axiom_status(kami_as_genuine_trace_manifestation, holdable).
narrative_ontology:cs_axiom_grounding('e925c56c-8b02-4a91-a17d-a0e013f5ed41', kami_as_genuine_trace_manifestation, theological).
narrative_ontology:cs_axiom('e925c56c-8b02-4a91-a17d-a0e013f5ed41', secondary, buddhist_elite_interpretive_primacy).
narrative_ontology:cs_axiom_status(buddhist_elite_interpretive_primacy, holdable).
narrative_ontology:cs_axiom_grounding('e925c56c-8b02-4a91-a17d-a0e013f5ed41', buddhist_elite_interpretive_primacy, conventional).
narrative_ontology:cs_reference_frame('e925c56c-8b02-4a91-a17d-a0e013f5ed41', nara_heian_syncretic_synthesis).
narrative_ontology:cs_drift_state('e925c56c-8b02-4a91-a17d-a0e013f5ed41', tokugawa_jinguji_consolidation, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('e925c56c-8b02-4a91-a17d-a0e013f5ed41', '').
narrative_ontology:cs_kernel_id(shinbutsu_coexistence_commitment__syncretic_fusion_reading, shinbutsu_coexistence_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(shinbutsu_coexistence_commitment__syncretic_fusion_reading, jinguji_temple_shrine_complexes).
narrative_ontology:constraint_beneficiary(shinbutsu_coexistence_commitment__syncretic_fusion_reading, buddhist_theological_elite).
narrative_ontology:constraint_beneficiary(shinbutsu_coexistence_commitment__syncretic_fusion_reading, shugendo_practitioners).
narrative_ontology:constraint_beneficiary(shinbutsu_coexistence_commitment__syncretic_fusion_reading, imperial_court_ritual_apparatus).
narrative_ontology:constraint_victim(shinbutsu_coexistence_commitment__syncretic_fusion_reading, shrine_priests_of_unaffiliated_kami).
narrative_ontology:constraint_victim(shinbutsu_coexistence_commitment__syncretic_fusion_reading, local_kami_cults_with_incompatible_cosmology).
narrative_ontology:constraint_victim(shinbutsu_coexistence_commitment__syncretic_fusion_reading, lay_practitioners_seeking_kami_specific_worship).
narrative_ontology:constraint_vindicates(shinbutsu_coexistence_commitment__syncretic_fusion_reading, buddhist_universal_truth_doctrine).
narrative_ontology:constraint_vindicates(shinbutsu_coexistence_commitment__syncretic_fusion_reading, single_ontology_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Combined shrine-temple institutions administer both kami ritual and Buddhist doctrine on the same grounds, drawing revenue and land grants from both streams. The honji suijaku framework is the institution's own organizing charter — it justifies dual clergy, dual ritual calendars, and consolidated landholding under a single administrative roof.
narrative_ontology:constraint_stakeholder(shinbutsu_coexistence_commitment__syncretic_fusion_reading, jinguji_temple_shrine_complexes, beneficiary,
    institutional, civilizational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(shinbutsu_coexistence_commitment__syncretic_fusion_reading, jinguji_temple_shrine_complexes, agenda_setter).

% Monastic scholars at major temple complexes (Tendai, Shingon) produce the honji-suijaku correspondences, assigning specific Buddhas and bodhisattvas as the 'original ground' of specific kami. They hold interpretive authority over which correspondences are doctrinally sound, and their rulings determine which shrines gain prestige and patronage.
narrative_ontology:constraint_stakeholder(shinbutsu_coexistence_commitment__syncretic_fusion_reading, buddhist_theological_elite, agenda_setter,
    institutional, civilizational, arbitrage, national).

% Mountain ascetics build an entire practice on the premise that kami and Buddhist divinities are aspects of one reality accessible through the same austerities. The fusion doctrine is the ground of their vocation; without it their syncretic ritual technology has no theological warrant.
narrative_ontology:constraint_stakeholder(shinbutsu_coexistence_commitment__syncretic_fusion_reading, shugendo_practitioners, beneficiary,
    organized, generational, constrained, regional).

% Court ritualists use the unified ontology to fold imperial kami ancestry claims into a Buddhist cosmological order that also legitimates the state, gaining a doctrinal apparatus that reinforces both religious and political authority simultaneously.
narrative_ontology:constraint_stakeholder(shinbutsu_coexistence_commitment__syncretic_fusion_reading, imperial_court_ritual_apparatus, beneficiary,
    institutional, civilizational, arbitrage, national).

% Priests serving kami with no clean Buddhist correspondence, or kami whose local character resists subordination to a 'original ground,' must either accept reclassification as a lesser manifestation of an assigned Buddha or risk losing standing, patronage, and doctrinal legitimacy relative to shrines with established honji. Their ritual authority is now graded by proximity to the Buddhist center.
narrative_ontology:constraint_stakeholder(shinbutsu_coexistence_commitment__syncretic_fusion_reading, shrine_priests_of_unaffiliated_kami, payer,
    moderate, generational, constrained, local).

% Communities whose kami worship involves ancestral, territorial, or purity logics that do not map onto Buddhist salvation soteriology find their cosmology reinterpreted or subordinated by theological elites who never consult them. They have no institutional standing to contest the assigned correspondence.
narrative_ontology:constraint_stakeholder(shinbutsu_coexistence_commitment__syncretic_fusion_reading, local_kami_cults_with_incompatible_cosmology, payer,
    powerless, generational, trapped, local).

% Ordinary worshippers who want to petition a specific kami for a specific local concern (harvest, childbirth, purification) are increasingly required to engage the fused apparatus — Buddhist rites, Buddhist clergy, Buddhist doctrine of merit — layered atop what was previously direct kami petition, raising the cost and complexity of what had been simple local devotion.
narrative_ontology:constraint_stakeholder(shinbutsu_coexistence_commitment__syncretic_fusion_reading, lay_practitioners_seeking_kami_specific_worship, payer,
    powerless, biographical, constrained, local).

% Historians and scholars of religion examine the honji-suijaku textual corpus, temple records, and shrine genealogies to assess whether the fusion doctrine reflects a coherent theological achievement or an institutionally convenient overlay. They are not party to the doctrine's operation but adjudicate its historical claims.
narrative_ontology:constraint_stakeholder(shinbutsu_coexistence_commitment__syncretic_fusion_reading, religious_studies_scholars, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single, internally consistent cosmological framework that lets kami worship and Buddhist doctrine operate within the same institutional and ritual space without requiring either tradition's practitioners to renounce the other — solving the real problem of two established religious systems coexisting in the same population without schism.
% TRANSFER_FUNCTION: Moves ritual authority, land revenue, and doctrinal legitimacy from independent kami cults and their priests toward combined jinguji institutions and the Buddhist theological elite who author and adjudicate the honji-suijaku correspondences; also moves interpretive control over what a given kami 'really is' from local communities to centralized monastic scholarship.
% ABSENT_VOICES: Local kami cults with cosmologies that resist Buddhist correspondence (particularly agrarian, purity-based, and ancestral kami traditions) are not consulted in the assignment of their 'original ground' — the correspondences are produced by Buddhist monastic scholarship and imposed onto shrine tradition from outside. Non-elite lay worshippers seeking direct, unmediated kami petition have no voice in whether the fusion apparatus should govern their local shrine.
% DISAPPEARANCE_RATIONALE: If the honji-suijaku ontological unification vanished overnight, jinguji complexes would face an immediate legitimacy crisis over their combined landholdings and dual clergy, Buddhist theological elites would lose interpretive jurisdiction over kami worship, and independent kami shrines would be free to reassert unmediated local cosmologies — precisely what in fact happened, abruptly and by state decree, during the Meiji shinbutsu bunri (separation edicts) of 1868, which dismantled jinguji institutions nationwide within years.
% FOUNDING_PROBLEM: Buddhism arrived in Japan as a foreign, textually sophisticated tradition claiming universal soteriological truth, while indigenous kami worship was locally rooted, textually thin, and tied to specific places and lineages; the two could have produced sectarian conflict or mutual exclusion, and honji-suijaku offered a theological solution that let both traditions' institutions, patronage networks, and practitioner bases persist and interlock rather than compete for exclusive territory.
% FOUNDING_PROBLEM_CORROBORATION: Meiji-era state officials, Shinto revivalist scholars (kokugaku), and modern historians of Japanese religion outside the Buddhist theological tradition attest that by the early modern period the original problem of managing two competing traditions had been resolved for centuries, and that the honji-suijaku apparatus by the Tokugawa era functioned primarily to preserve jinguji institutional revenue and Buddhist administrative jurisdiction over shrines rather than to solve any live doctrinal tension — this is precisely the reading kokugaku scholars used to justify dismantling it, though their own account is not neutral either, being motivated by a rival nativist agenda.
narrative_ontology:disappearance_verdict(shinbutsu_coexistence_commitment__syncretic_fusion_reading, world_rearranges).
narrative_ontology:founding_problem_status(shinbutsu_coexistence_commitment__syncretic_fusion_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(shinbutsu_coexistence_commitment__syncretic_fusion_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(shinbutsu_coexistence_commitment__syncretic_fusion_reading, 'none', 1).
narrative_ontology:epsilon_provenance(shinbutsu_coexistence_commitment__syncretic_fusion_reading, 0.58, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness is authored moderate-high (0.58) and rising across the interval: the doctrine begins (Nara-Heian period) as a genuinely functional accommodation of two coexisting traditions, but by the Tokugawa period the honji-suijaku apparatus has become the load-bearing structure for jinguji land revenue and monastic jurisdictional claims over shrines, independent of whether any live doctrinal tension remains to solve. Suppression rises correspondingly (0.20 to 0.52) as the correspondences harden from theological proposal into institutionally enforced classification — a shrine's standing and patronage increasingly depend on accepting its assigned honji. Theater ratio also rises (0.10 to 0.38) as the doctrine's institutional maintenance increasingly serves jurisdictional and revenue functions dressed in continued theological elaboration, though it never crosses into pure performance — real doctrinal work (correspondence scholarship, ritual integration) continues throughout. Accessibility collapse is authored high (0.62): once a shrine is absorbed into a jinguji complex and assigned an honji, reverting to independent kami worship becomes institutionally and doctrinally very difficult. Resistance is moderate (0.45): local communities with incompatible cosmologies bear the cost quietly for centuries, but resistance surfaces sharply and successfully at the doctrine's collapse (Meiji shinbutsu bunri), showing the resistance was suppressed rather than absent.
 *
 * PERSPECTIVAL GAP:
 *   From the seat of the Buddhist theological elite and jinguji administration, this is a genuine metaphysical achievement — a defensible, coherent account of how two traditions' divinities relate. From the seat of an unaffiliated kami shrine priest facing reclassification, the same doctrine operates as an externally imposed hierarchy that subordinates local cosmology to a center it never had a say in constructing. The engine computes these as different seat-level classifications from the same structural data; this story does not adjudicate which seat is 'right' — it authors the structural facts (who sets the correspondences, who bears reclassification, who can exit) that generate the divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Jinguji institutions, the Buddhist theological elite, shugendo practitioners, and the imperial ritual apparatus are declared beneficiaries: each derives structural benefit — revenue, jurisdiction, doctrinal warrant, or political legitimacy — from the unified ontology and has arbitrage-grade exit (they can shift emphasis between traditions as convenient, and set the very terms of correspondence). Shrine priests of unaffiliated kami, incompatible-cosmology kami cults, and kami-specific lay worshippers are declared victims: their exit is constrained or trapped (leaving the fused framework means losing institutional standing that has, by design, become the only avenue to legitimacy) and the constraint's operation subordinates their independent cosmology to an externally-assigned 'original ground.' This maps cleanly to the engine's directionality derivation: powerful arbitrage-capable agenda-setters and beneficiaries sit near the subsidized end; powerless, trapped payers sit near the full-target end.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as tangled_rope (rather than snare) preserves the genuine coordination function this reading claims: honji-suijaku did solve a real problem (managing two coexisting religious traditions without schism) for a substantial historical period, and that coordination benefit is not fictional under this reading. But calling it a pure rope would erase the asymmetric extraction that the doctrine's institutionalization produced over time — the founding_problem_status is authored 'dead' precisely because by the time of Meiji dismantlement, the doctrine's persistence was substantially explained by jinguji revenue and Buddhist jurisdictional interest rather than any live syncretic necessity. Tangled rope holds both facts without collapsing one into the other: real coordination at founding, real extraction by maturity, both operating through the identical structure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    genuine_coherence_vs_institutional_convenience,
    'Was honji-suijaku a genuinely coherent theological synthesis with independent metaphysical content, or a strategically ambiguous framework whose ''coherence'' is a retrospective scholarly imposition on what was always institutional accommodation?',
    'Close textual analysis of honji-suijaku doctrinal treatises (e.g., Keiran Shuyoshu) for internal logical consistency and systematic correspondence rules, cross-checked against whether correspondences were applied consistently or ad hoc/regionally variable in ways that would indicate political rather than doctrinal motivation.',
    'If genuinely coherent, this reading (syncretic_fusion) is the structurally accurate one and the coordination_function claim is fully warranted. If the coherence is a retrospective imposition, the incoherent_bundle_reading is closer to the structural truth and this story''s coordination credit is overstated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(genuine_coherence_vs_institutional_convenience, conceptual, 'Whether the fusion doctrine had independent theological coherence or was institutionally convenient ambiguity.').

omega_variable(
    natural_theology_vs_constructed_hierarchy,
    'Is the honji-suijaku correspondence system best understood as theological elites discovering a true metaphysical relationship between kami and Buddhas, or as theological elites constructing a hierarchy that happens to place their own tradition''s texts and institutions at the interpretive center?',
    'Examine whether the assignment of ''original ground'' status systematically favored kami associated with politically powerful shrines/lineages versus kami associated with marginal or peripheral communities — a systematic correlation with power would support the constructed-hierarchy reading.',
    'Bears directly on whether the beneficiary/victim structure authored here reflects genuine doctrinal discovery (in which case the extraction reading may be too harsh) or motivated theological construction (in which case the extraction reading may understate the asymmetry).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_theology_vs_constructed_hierarchy, empirical, 'Whether honji-suijaku correspondences tracked genuine doctrine or institutional power.').

omega_variable(
    committer_framing_disagreement_location,
    'Where exactly does this reading''s disagreement with domain_partition_reading and incoherent_bundle_reading actually live — in what counts as evidence of ''ontological unification,'' or in whether unification (if real) was voluntary theological achievement versus imposed hierarchy?',
    'None available within the kernel itself; this is a genealogical/philosophical dispute about what would count as evidence of ontological unity, not an empirical question with a determinate resolution procedure. Would require comparative doctrinal history alongside political-economy analysis of shrine-temple institutions.',
    'If the disagreement is purely conceptual (what counts as ''unification''), all three readings can remain coexisting accounts of the same historical material told at different levels of abstraction. If the disagreement is substantially empirical (did correspondences actually cohere doctrinally, or was ambiguity deliberate), one reading may be straightforwardly more accurate than the others as history, even though all three remain authorable as distinct constraint stories under the ε-invariance principle.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(committer_framing_disagreement_location, conceptual, 'Locates where the three sibling readings of the kernel actually diverge — evidentiary standard vs. voluntariness of the unification.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(shinbutsu_coexistence_commitment__syncretic_fusion_reading, 0, 1868).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(shin_tr_t0, shinbutsu_coexistence_commitment__syncretic_fusion_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement_basis(shin_tr_t0, observed).
narrative_ontology:measurement(shin_tr_t400, shinbutsu_coexistence_commitment__syncretic_fusion_reading, theater_ratio, 400, 0.15).
narrative_ontology:measurement_basis(shin_tr_t400, observed).
narrative_ontology:measurement(shin_tr_t800, shinbutsu_coexistence_commitment__syncretic_fusion_reading, theater_ratio, 800, 0.22).
narrative_ontology:measurement_basis(shin_tr_t800, observed).
narrative_ontology:measurement(shin_tr_t1200, shinbutsu_coexistence_commitment__syncretic_fusion_reading, theater_ratio, 1200, 0.28).
narrative_ontology:measurement_basis(shin_tr_t1200, observed).
narrative_ontology:measurement(shin_tr_t1600, shinbutsu_coexistence_commitment__syncretic_fusion_reading, theater_ratio, 1600, 0.34).
narrative_ontology:measurement_basis(shin_tr_t1600, observed).
narrative_ontology:measurement(shin_tr_t1868, shinbutsu_coexistence_commitment__syncretic_fusion_reading, theater_ratio, 1868, 0.38).
narrative_ontology:measurement_basis(shin_tr_t1868, observed).

% Extraction over time
narrative_ontology:measurement(shin_be_t0, shinbutsu_coexistence_commitment__syncretic_fusion_reading, base_extractiveness, 0, 0.22).
narrative_ontology:measurement_basis(shin_be_t0, observed).
narrative_ontology:measurement(shin_be_t400, shinbutsu_coexistence_commitment__syncretic_fusion_reading, base_extractiveness, 400, 0.34).
narrative_ontology:measurement_basis(shin_be_t400, observed).
narrative_ontology:measurement(shin_be_t800, shinbutsu_coexistence_commitment__syncretic_fusion_reading, base_extractiveness, 800, 0.45).
narrative_ontology:measurement_basis(shin_be_t800, observed).
narrative_ontology:measurement(shin_be_t1200, shinbutsu_coexistence_commitment__syncretic_fusion_reading, base_extractiveness, 1200, 0.51).
narrative_ontology:measurement_basis(shin_be_t1200, observed).
narrative_ontology:measurement(shin_be_t1600, shinbutsu_coexistence_commitment__syncretic_fusion_reading, base_extractiveness, 1600, 0.55).
narrative_ontology:measurement_basis(shin_be_t1600, observed).
narrative_ontology:measurement(shin_be_t1868, shinbutsu_coexistence_commitment__syncretic_fusion_reading, base_extractiveness, 1868, 0.58).
narrative_ontology:measurement_basis(shin_be_t1868, observed).

% Suppression requirement over time
narrative_ontology:measurement(shin_su_t0, shinbutsu_coexistence_commitment__syncretic_fusion_reading, suppression_requirement, 0, 0.2).
narrative_ontology:measurement_basis(shin_su_t0, observed).
narrative_ontology:measurement(shin_su_t400, shinbutsu_coexistence_commitment__syncretic_fusion_reading, suppression_requirement, 400, 0.28).
narrative_ontology:measurement_basis(shin_su_t400, observed).
narrative_ontology:measurement(shin_su_t800, shinbutsu_coexistence_commitment__syncretic_fusion_reading, suppression_requirement, 800, 0.35).
narrative_ontology:measurement_basis(shin_su_t800, observed).
narrative_ontology:measurement(shin_su_t1200, shinbutsu_coexistence_commitment__syncretic_fusion_reading, suppression_requirement, 1200, 0.42).
narrative_ontology:measurement_basis(shin_su_t1200, observed).
narrative_ontology:measurement(shin_su_t1600, shinbutsu_coexistence_commitment__syncretic_fusion_reading, suppression_requirement, 1600, 0.48).
narrative_ontology:measurement_basis(shin_su_t1600, observed).
narrative_ontology:measurement(shin_su_t1868, shinbutsu_coexistence_commitment__syncretic_fusion_reading, suppression_requirement, 1868, 0.52).
narrative_ontology:measurement_basis(shin_su_t1868, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(shinbutsu_coexistence_commitment__syncretic_fusion_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(shinbutsu_coexistence_commitment__syncretic_fusion_reading, 0.1).
narrative_ontology:affects_constraint(shinbutsu_coexistence_commitment__syncretic_fusion_reading, shinbutsu_coexistence_commitment__domain_partition_reading).
narrative_ontology:affects_constraint(shinbutsu_coexistence_commitment__syncretic_fusion_reading, shinbutsu_coexistence_commitment__incoherent_bundle_reading).

% DUAL FORMULATION NOTE:
% This story is one of three constraint stories decomposing the natural-language label 'shinbutsu-shugo' / honji-suijaku per the ε-invariance principle: the label conflates a doctrinal-metaphysical claim (this story), a functional-domain claim (domain_partition_reading — kami and Buddhas govern separate existential territories without ontological claims), and a meta-level historiographical claim about the kernel's own coherence (incoherent_bundle_reading — the bundle was never doctrinally unified but institutionally maintained through ambiguity). Each reading authors a different ε because each is a genuinely different structural claim about what shinbutsu-shugo was: this story's ε (0.58) reflects genuine coordination degrading into institutional extraction over centuries; domain_partition_reading would author lower suppression and extraction since it makes no unification claim requiring doctrinal enforcement; incoherent_bundle_reading would author the highest theater_ratio since it denies any doctrinal coherence was ever real, treating the entire apparatus as calculated institutional ambiguity.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
