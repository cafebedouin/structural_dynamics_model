% ============================================================================
% CONSTRAINT STORY: shinbutsu_coexistence_commitment__syncretic_fusion_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
 *   human_readable: Honji Suijaku Ontological Unification of Kami and Buddhas
 *   domain: religious_studies/philosophy_of_religion/japanese_history
 *
 * SUMMARY:
 *   This story instantiates the syncretic fusion reading of the shinbutsu
 *   coexistence kernel: kami and Buddhist deities are held to be
 *   ontologically unified, with kami as honji suijaku — local, provisional
 *   traces (suijaku) of universal Buddhist principles (honji, buddhas and
 *   bodhisattvas). This is a single coherent ontology requiring active
 *   doctrinal maintenance by a theological elite, structurally embodied in
 *   the jinguji combined shrine-temple institutions that proliferated from
 *   roughly the Nara period through the early Meiji period (~8th century to
 *   1868). It is a genuine coordination achievement — it let a religiously
 *   plural society avoid the social cost of unresolved theological
 *   contradiction — but the coordination was administered by an interpretive
 *   elite whose authority and whose institutions' economic base depended on
 *   the fusion remaining doctrinally settled, at the expense of local kami
 *   custodians whose deities were subordinated to an imported cosmological
 *   hierarchy they had no part in authoring. Two sibling readings of the same
 *   underlying kernel — that kami and buddhas govern separate existential
 *   domains without ontological fusion (domain_partition_reading), and that
 *   the entire arrangement was never coherent at all but an ambiguity
 *   sustained by institutional power (incoherent_bundle_reading) — are NOT
 *   represented in this file; they are separate constraints with their own ε
 *   and structure, linked here only by network reference.
 *
 * KEY AGENTS:
 *   - shingon_tendai_theological_elite: institutional/arbitrage — sets and defends the honji-suijaku correspondences
 *   - jinguji_temple_shrine_complexes: institutional/constrained — structural embodiment, economically dependent on the fusion holding
 *   - local_kami_cult_custodians: moderate/trapped — bear the cost of cosmological subordination
 *   - shrine_priests_excluded_from_doctrine: powerless/trapped — administer rites whose meaning is fixed elsewhere
 *   - comparative_religion_scholars: analytical/analytical — external observers of the syncretism's coherence and function
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(shinbutsu_coexistence_commitment__syncretic_fusion_reading, 0.58).
domain_priors:suppression_score(shinbutsu_coexistence_commitment__syncretic_fusion_reading, 0.52).
domain_priors:theater_ratio(shinbutsu_coexistence_commitment__syncretic_fusion_reading, 0.34).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(shinbutsu_coexistence_commitment__syncretic_fusion_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(shinbutsu_coexistence_commitment__syncretic_fusion_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(shinbutsu_coexistence_commitment__syncretic_fusion_reading, theater_ratio, 0.34).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(shinbutsu_coexistence_commitment__syncretic_fusion_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(shinbutsu_coexistence_commitment__syncretic_fusion_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(shinbutsu_coexistence_commitment__syncretic_fusion_reading, tangled_rope).
narrative_ontology:human_readable(shinbutsu_coexistence_commitment__syncretic_fusion_reading, "Honji Suijaku Ontological Unification of Kami and Buddhas").
narrative_ontology:topic_domain(shinbutsu_coexistence_commitment__syncretic_fusion_reading, "religious_studies/philosophy_of_religion/japanese_history").

domain_priors:requires_active_enforcement(shinbutsu_coexistence_commitment__syncretic_fusion_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(shinbutsu_coexistence_commitment__syncretic_fusion_reading, 'cfddc47e-be23-4dbb-8e5a-ae1175f57dfe').
narrative_ontology:cs_kernel_codification('cfddc47e-be23-4dbb-8e5a-ae1175f57dfe', formalized).
narrative_ontology:cs_authority_grounding('cfddc47e-be23-4dbb-8e5a-ae1175f57dfe', lineage).
narrative_ontology:cs_interpretation_layer_present('cfddc47e-be23-4dbb-8e5a-ae1175f57dfe').
narrative_ontology:cs_reading_relation('cfddc47e-be23-4dbb-8e5a-ae1175f57dfe', shinbutsu_coexistence_commitment__domain_partition_reading, forecloses).
narrative_ontology:cs_reading_relation('cfddc47e-be23-4dbb-8e5a-ae1175f57dfe', shinbutsu_coexistence_commitment__incoherent_bundle_reading, influences).
narrative_ontology:cs_axiom('cfddc47e-be23-4dbb-8e5a-ae1175f57dfe', foundational, single_ultimate_ontology_underlies_kami_and_buddhas).
narrative_ontology:cs_axiom_status(single_ultimate_ontology_underlies_kami_and_buddhas, holdable).
narrative_ontology:cs_axiom_grounding('cfddc47e-be23-4dbb-8e5a-ae1175f57dfe', single_ultimate_ontology_underlies_kami_and_buddhas, theological).
narrative_ontology:cs_axiom('cfddc47e-be23-4dbb-8e5a-ae1175f57dfe', secondary, esoteric_correspondence_charts_fix_deity_identity).
narrative_ontology:cs_axiom_status(esoteric_correspondence_charts_fix_deity_identity, overridden).
narrative_ontology:cs_axiom_grounding('cfddc47e-be23-4dbb-8e5a-ae1175f57dfe', esoteric_correspondence_charts_fix_deity_identity, conventional).
narrative_ontology:cs_reference_frame('cfddc47e-be23-4dbb-8e5a-ae1175f57dfe', nara_heian_esoteric_synthesis).
narrative_ontology:cs_drift_state('cfddc47e-be23-4dbb-8e5a-ae1175f57dfe', meiji_shinbutsu_bunri_edicts, gap(repudiation_pressure, severe, true)).
narrative_ontology:cs_created_at('cfddc47e-be23-4dbb-8e5a-ae1175f57dfe', '').
narrative_ontology:cs_kernel_id(shinbutsu_coexistence_commitment__syncretic_fusion_reading, shinbutsu_coexistence_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(shinbutsu_coexistence_commitment__syncretic_fusion_reading, shingon_tendai_theological_elite).
narrative_ontology:constraint_beneficiary(shinbutsu_coexistence_commitment__syncretic_fusion_reading, jinguji_temple_shrine_complexes).
narrative_ontology:constraint_beneficiary(shinbutsu_coexistence_commitment__syncretic_fusion_reading, court_sponsored_ritual_specialists).
narrative_ontology:constraint_victim(shinbutsu_coexistence_commitment__syncretic_fusion_reading, local_kami_cult_custodians).
narrative_ontology:constraint_victim(shinbutsu_coexistence_commitment__syncretic_fusion_reading, shrine_priests_excluded_from_doctrine).
narrative_ontology:constraint_victim(shinbutsu_coexistence_commitment__syncretic_fusion_reading, lay_worshippers_denied_direct_kami_access).
narrative_ontology:constraint_vindicates(shinbutsu_coexistence_commitment__syncretic_fusion_reading, buddhist_dharma_universality_doctrine).
narrative_ontology:constraint_vindicates(shinbutsu_coexistence_commitment__syncretic_fusion_reading, single_ontology_of_sacred_manifestation).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Produces and refines the honji suijaku correspondences (which kami is the suijaku/trace-manifestation of which buddha or bodhisattva), controls the esoteric commentarial literature that fixes these identifications, and adjudicates disputes about doctrinal consistency. Their institutional standing rests on being the interpretive authority capable of resolving apparent contradictions between kami worship and Buddhist cosmology.
narrative_ontology:constraint_stakeholder(shinbutsu_coexistence_commitment__syncretic_fusion_reading, shingon_tendai_theological_elite, agenda_setter,
    institutional, civilizational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(shinbutsu_coexistence_commitment__syncretic_fusion_reading, shingon_tendai_theological_elite, beneficiary).

% Combined shrine-temple institutions (jinguji) exist as the physical embodiment of the unified ontology, drawing revenue, land grants, and pilgrim traffic from both kami veneration and Buddhist ritual under one roof. Their continued economic viability depends on the honji suijaku framework remaining doctrinally authoritative; a domain-partition or incoherent-bundle reading would threaten their structural rationale.
narrative_ontology:constraint_stakeholder(shinbutsu_coexistence_commitment__syncretic_fusion_reading, jinguji_temple_shrine_complexes, beneficiary,
    institutional, generational, constrained, regional).

% Perform state rituals that require kami and buddhas to be legible within a single cosmological order (e.g., linking imperial ancestor kami to cosmic buddhas), securing court patronage and legitimizing the dynastic-religious order. Their function collapses without the fusion premise.
narrative_ontology:constraint_stakeholder(shinbutsu_coexistence_commitment__syncretic_fusion_reading, court_sponsored_ritual_specialists, beneficiary,
    powerful, generational, constrained, national).

% Hereditary caretakers of local kami shrines whose autochthonous deity is reinterpreted as a mere provisional trace of a distant, universal Buddhist principle. Their local cosmology is subordinated to an imported theological schema they did not author and cannot easily contest without appearing to reject orthodox Buddhist truth, which was politically and socially costly across most of the premodern period.
narrative_ontology:constraint_stakeholder(shinbutsu_coexistence_commitment__syncretic_fusion_reading, local_kami_cult_custodians, payer,
    moderate, generational, trapped, local).

% Ritual specialists who maintain kami worship practice but lack the esoteric Buddhist training to participate in the theological literature that defines their own deity's ontological status. They administer rites whose ultimate meaning is fixed elsewhere, by an interpretive elite they cannot access or challenge on its own terms.
narrative_ontology:constraint_stakeholder(shinbutsu_coexistence_commitment__syncretic_fusion_reading, shrine_priests_excluded_from_doctrine, payer,
    powerless, biographical, trapped, local).

% Ordinary worshippers who approach a kami shrine for direct, unmediated relationship with a local deity find that relationship reframed as access to a universal Buddhist truth via a provisional local avatar. Their devotional experience is doctrinally reinterpreted without their participation in that reinterpretation, though in daily practice this mostly remained invisible to them.
narrative_ontology:constraint_stakeholder(shinbutsu_coexistence_commitment__syncretic_fusion_reading, lay_worshippers_denied_direct_kami_access, payer,
    powerless, biographical, constrained, local).

% Not present in the premodern kernel's own framework, but structurally poised to reject the fusion premise entirely once state interests shifted toward a purified, nationalist Shinto separated from Buddhism. Their later shinbutsu bunri (separation) policy demonstrates that a different framework could dissolve this reading's ontology by fiat, though this constraint story concerns the fusion reading as it operated on its own terms during its active period.
narrative_ontology:constraint_stakeholder(shinbutsu_coexistence_commitment__syncretic_fusion_reading, meiji_state_shinto_reformers, excluded,
    institutional, generational, mobile, national).

% Study honji suijaku as a historical syncretism, comparing its ontological unification claim to other syncretic religious systems and assessing whether the fusion was doctrinally coherent, politically instrumental, or both simultaneously.
narrative_ontology:constraint_stakeholder(shinbutsu_coexistence_commitment__syncretic_fusion_reading, comparative_religion_scholars, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single coherent cosmological framework that lets a religiously plural society avoid open contradiction between indigenous kami veneration and imported Buddhist doctrine, allowing joint ritual practice, shared institutions (jinguji), and a unified court liturgy instead of two competing, mutually exclusive sacred orders.
% TRANSFER_FUNCTION: Moves interpretive authority, ritual revenue, and land/patronage income from local kami cult custodians and shrine priests toward the theological elite and combined jinguji institutions who control the honji suijaku correspondences; moves cosmological self-determination away from lay worshippers and toward doctrinal specialists who define what their local deity 'really' is.
% ABSENT_VOICES: Local kami cult custodians and shrine priests without esoteric Buddhist training would likely object that their deity's meaning has been annexed by an interpretive apparatus they cannot access, but the doctrinal literature itself was produced almost entirely within Buddhist monastic institutions, so this objection rarely appears in the surviving textual record — it is preserved, if at all, in oral tradition and later Shinto revivalist polemics that predate but anticipate the Meiji-era separatist reaction.
% DISAPPEARANCE_RATIONALE: If the unified ontology were withdrawn, jinguji institutions would lose their structural rationale and likely split into separate kami and Buddhist establishments (as in fact happened under Meiji shinbutsu bunri), theological elites would lose their exclusive interpretive claim over kami identity, and local shrine custodians would regain unmediated authority to define their own deities — this is not a hypothetical, it is the historically observed outcome of the kernel's dissolution.
% FOUNDING_PROBLEM: Buddhism arrived in Japan as a universalizing, text-based, monastically organized religion in a society with deeply rooted, geographically particular kami worship; without some reconciling framework, the two systems risked either open theological conflict or an incoherent, unexplained coexistence that threatened both Buddhist claims to universal truth and kami cults' local legitimacy.
% FOUNDING_PROBLEM_CORROBORATION: Meiji-era state Shinto reformers and comparative religion scholars working from outside the tradition's own theological establishment attest that by the 19th century the fusion functioned primarily as an inherited institutional arrangement rather than a live doctrinal necessity — the ease and speed with which shinbutsu bunri separated the two systems in the 1868-1872 period, without triggering the theological crisis the honji suijaku framework was ostensibly built to prevent, is treated by historians (e.g., in the documented administrative record of the separation edicts and subsequent haibutsu kishaku destruction of Buddhist material at shrines) as strong evidence the coordination problem itself had become vestigial, corroborated from outside both the theological elite and the jinguji beneficiaries.
narrative_ontology:disappearance_verdict(shinbutsu_coexistence_commitment__syncretic_fusion_reading, world_rearranges).
narrative_ontology:founding_problem_status(shinbutsu_coexistence_commitment__syncretic_fusion_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(shinbutsu_coexistence_commitment__syncretic_fusion_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
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
 *   Extractiveness is authored at a moderate-high 0.58: the fusion genuinely solves a coordination problem (avoiding open theological conflict in a plural religious society) but the specific institutional form that solved it — jinguji complexes and elite doctrinal control — also captured revenue, land, and interpretive authority away from local custodians in a way not strictly necessary to the coordination function itself. Suppression is authored at 0.52, reflecting that dissent from local kami cults was rarely met with violent coercion but was structurally discouraged: rejecting the fusion risked being read as rejecting Buddhist orthodoxy in a society where Buddhist institutions held substantial state backing, particularly from the Nara period onward. Theater ratio rises modestly over the interval (0.15 to 0.34) reflecting the accumulation, especially from the medieval period onward, of increasingly elaborate and formulaic honji-suijaku correspondence charts (suijaku mandara, deity-matching lists) that functioned more as institutional credentialing than as live theological reasoning. Accessibility collapse is authored high (0.62) — once a shrine was absorbed into a jinguji complex and its kami assigned a honji, the alternative (an unaffiliated, purely kami-focused local cult) became progressively harder to maintain against the dominant institutional and doctrinal current. Resistance is authored moderate (0.40): real but muted, since most surviving resistance is preserved only obliquely, in later Shinto revivalist and nativist (kokugaku) polemics that predate the Meiji rupture rather than in direct premodern protest.
 *
 * PERSPECTIVAL GAP:
 *   From the theological elite's seat, the honji-suijaku framework is a triumph of doctrinal reasoning that successfully reconciles two truth-claims that would otherwise conflict — a genuine intellectual and religious achievement. From a local kami custodian's seat, the same framework is experienced as having one's ancestral deity redefined, by outsiders, as a mere provisional shadow of a foreign truth. The engine computes these as structurally different seat-level classifications from the same authored data; this story does not adjudicate between them, only supplies the structure from which each seat's classification follows.
 *
 * DIRECTIONALITY LOGIC:
 *   The theological elite and jinguji institutions sit near the beneficiary end of directionality: they set the terms of the ontology, collect the institutional and economic benefits of combined shrine-temple operation, and hold arbitrage-grade exit (they could, and later did, adapt their doctrinal claims as political conditions shifted). Local kami cult custodians and excluded shrine priests sit near the target end: trapped exit options (leaving the fusion framework meant either theological marginalization or, before the framework was fully entrenched, political vulnerability), and their local cosmological authority is what is being transferred away. Lay worshippers are authored as constrained rather than trapped — in daily devotional practice the reinterpretation was often invisible to them, so the cost they bear is more diffuse and less consciously felt than that borne by the custodians and priests whose institutional standing was directly restructured.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — avoiding an unresolved theological standoff between an indigenous particularist religion and a universalizing imported one — was live and genuine at the point of origin (Nara-Heian transition). By the medieval and early modern periods the coordination function had become largely inherited rather than actively solved: the jinguji institutions persisted and the doctrinal apparatus continued to be elaborated, but the underlying question of whether kami and buddhas needed to be reconciled at all had, for most practical purposes, been settled by inertia rather than live theological necessity. The speed and administrative ease of the Meiji shinbutsu bunri separation — accomplished by edict within a few years, without the social crisis the fusion doctrine was in principle designed to prevent — is treated here as strong evidence that the founding problem had gone dead well before the formal dissolution. This is exactly the kind of case where classifying the constraint as pure coordination (a Rope) would obscure the real, if not overwhelming, extraction the theological elite and jinguji institutions accrued from administering an increasingly vestigial synthesis; classifying it as pure extraction (a Snare) would erase the genuine multi-century coordination service the fusion provided to a religiously plural society before its later ossification. Tangled Rope registers both facts without collapsing them into each other.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indeterminacy,
    'Was honji suijaku actually experienced and practiced as a single coherent ontology (this reading), as a domain-partitioned coexistence with no real fusion claim (domain_partition_reading), or as a working ambiguity that was never resolved into either coherent form (incoherent_bundle_reading)?',
    'Comparative analysis of extant medieval doctrinal texts, temple-shrine administrative records, and popular devotional practice manuals across regions and periods, distinguishing elite theological literature (which strongly supports the fusion reading) from local ritual practice records (which may better support domain partition or unresolved ambiguity).',
    'If the fusion reading is descriptively accurate for elite doctrine but the domain-partition or incoherent-bundle reading better describes lived local practice, then this constraint''s high accessibility_collapse and suppression values may overstate the framework''s actual grip on everyday religious life outside theological and institutional circles.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_indeterminacy, conceptual, 'Whether the fusion ontology was genuinely dominant, one framing among several, or never coherently resolved.').

omega_variable(
    elite_versus_popular_doctrinal_authority,
    'To what extent did the honji-suijaku correspondences authored by the Shingon/Tendai theological elite actually govern local shrine practice, versus functioning as a parallel elite discourse largely disconnected from how ordinary worshippers and local priests related to their kami?',
    'Examination of local shrine liturgical records, votive inscriptions, and pilgrimage accounts for evidence of the honji-suijaku framework''s actual penetration into non-elite religious practice, versus its confinement to monastic and court commentarial literature.',
    'If penetration was shallow, the extractiveness and suppression values authored here overstate the cost borne by lay worshippers and possibly by shrine priests, and the constraint may be better modeled as bifurcated — elite doctrinal capture with minimal effect on popular practice.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(elite_versus_popular_doctrinal_authority, empirical, 'Whether elite doctrinal fusion actually constrained popular kami practice or ran in parallel to it.').

omega_variable(
    meiji_dissolution_speed_as_evidence,
    'Does the administrative speed of the Meiji shinbutsu bunri separation demonstrate that the fusion''s founding problem had gone dead, or does it instead demonstrate that state coercive power can dissolve even a genuinely live coordination arrangement quickly when political will and force are applied?',
    'Comparative study of the social and religious disruption that accompanied the separation (including haibutsu kishaku violence against Buddhist institutions) — genuine disruption would suggest the coordination function was still live and its removal was costly, not vestigial.',
    'If the separation caused substantial documented social and religious disruption, the founding_problem_status of ''dead'' authored in this story may be too strong, and the Meiji dissolution should be read as coercive imposition rather than confirmation of obsolescence.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(meiji_dissolution_speed_as_evidence, empirical, 'Whether the ease of Meiji-era separation proves the fusion''s coordination function had become vestigial, or merely that state power can override a still-functioning arrangement.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(shinbutsu_coexistence_commitment__syncretic_fusion_reading, 0, 1200).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(shin_tr_t0, shinbutsu_coexistence_commitment__syncretic_fusion_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement_basis(shin_tr_t0, projected).
narrative_ontology:measurement(shin_tr_t200, shinbutsu_coexistence_commitment__syncretic_fusion_reading, theater_ratio, 200, 0.18).
narrative_ontology:measurement_basis(shin_tr_t200, projected).
narrative_ontology:measurement(shin_tr_t400, shinbutsu_coexistence_commitment__syncretic_fusion_reading, theater_ratio, 400, 0.22).
narrative_ontology:measurement_basis(shin_tr_t400, projected).
narrative_ontology:measurement(shin_tr_t600, shinbutsu_coexistence_commitment__syncretic_fusion_reading, theater_ratio, 600, 0.26).
narrative_ontology:measurement_basis(shin_tr_t600, projected).
narrative_ontology:measurement(shin_tr_t800, shinbutsu_coexistence_commitment__syncretic_fusion_reading, theater_ratio, 800, 0.29).
narrative_ontology:measurement_basis(shin_tr_t800, observed).
narrative_ontology:measurement(shin_tr_t1000, shinbutsu_coexistence_commitment__syncretic_fusion_reading, theater_ratio, 1000, 0.32).
narrative_ontology:measurement_basis(shin_tr_t1000, observed).
narrative_ontology:measurement(shin_tr_t1200, shinbutsu_coexistence_commitment__syncretic_fusion_reading, theater_ratio, 1200, 0.34).
narrative_ontology:measurement_basis(shin_tr_t1200, observed).

% Extraction over time
narrative_ontology:measurement(shin_be_t0, shinbutsu_coexistence_commitment__syncretic_fusion_reading, base_extractiveness, 0, 0.32).
narrative_ontology:measurement_basis(shin_be_t0, projected).
narrative_ontology:measurement(shin_be_t200, shinbutsu_coexistence_commitment__syncretic_fusion_reading, base_extractiveness, 200, 0.4).
narrative_ontology:measurement_basis(shin_be_t200, projected).
narrative_ontology:measurement(shin_be_t400, shinbutsu_coexistence_commitment__syncretic_fusion_reading, base_extractiveness, 400, 0.47).
narrative_ontology:measurement_basis(shin_be_t400, projected).
narrative_ontology:measurement(shin_be_t600, shinbutsu_coexistence_commitment__syncretic_fusion_reading, base_extractiveness, 600, 0.52).
narrative_ontology:measurement_basis(shin_be_t600, projected).
narrative_ontology:measurement(shin_be_t800, shinbutsu_coexistence_commitment__syncretic_fusion_reading, base_extractiveness, 800, 0.55).
narrative_ontology:measurement_basis(shin_be_t800, observed).
narrative_ontology:measurement(shin_be_t1000, shinbutsu_coexistence_commitment__syncretic_fusion_reading, base_extractiveness, 1000, 0.57).
narrative_ontology:measurement_basis(shin_be_t1000, observed).
narrative_ontology:measurement(shin_be_t1200, shinbutsu_coexistence_commitment__syncretic_fusion_reading, base_extractiveness, 1200, 0.58).
narrative_ontology:measurement_basis(shin_be_t1200, observed).

% Suppression requirement over time
narrative_ontology:measurement(shin_su_t0, shinbutsu_coexistence_commitment__syncretic_fusion_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement_basis(shin_su_t0, projected).
narrative_ontology:measurement(shin_su_t200, shinbutsu_coexistence_commitment__syncretic_fusion_reading, suppression_requirement, 200, 0.36).
narrative_ontology:measurement_basis(shin_su_t200, projected).
narrative_ontology:measurement(shin_su_t400, shinbutsu_coexistence_commitment__syncretic_fusion_reading, suppression_requirement, 400, 0.41).
narrative_ontology:measurement_basis(shin_su_t400, projected).
narrative_ontology:measurement(shin_su_t600, shinbutsu_coexistence_commitment__syncretic_fusion_reading, suppression_requirement, 600, 0.45).
narrative_ontology:measurement_basis(shin_su_t600, projected).
narrative_ontology:measurement(shin_su_t800, shinbutsu_coexistence_commitment__syncretic_fusion_reading, suppression_requirement, 800, 0.48).
narrative_ontology:measurement_basis(shin_su_t800, observed).
narrative_ontology:measurement(shin_su_t1000, shinbutsu_coexistence_commitment__syncretic_fusion_reading, suppression_requirement, 1000, 0.5).
narrative_ontology:measurement_basis(shin_su_t1000, observed).
narrative_ontology:measurement(shin_su_t1200, shinbutsu_coexistence_commitment__syncretic_fusion_reading, suppression_requirement, 1200, 0.52).
narrative_ontology:measurement_basis(shin_su_t1200, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(shinbutsu_coexistence_commitment__syncretic_fusion_reading, identity_coordination).
narrative_ontology:affects_constraint(shinbutsu_coexistence_commitment__syncretic_fusion_reading, shinbutsu_coexistence_commitment__domain_partition_reading).
narrative_ontology:affects_constraint(shinbutsu_coexistence_commitment__syncretic_fusion_reading, shinbutsu_coexistence_commitment__incoherent_bundle_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the shinbutsu_coexistence_commitment kernel. The domain_partition_reading models kami and buddhas as governing separate existential domains without ontological unification (lower doctrinal-consistency requirement, more diffuse authority structure). The incoherent_bundle_reading models the arrangement as never having achieved kernel coherence at all, sustained instead by deliberate ambiguity and institutional power, and collapsing rapidly under Meiji pressure precisely because there was no coherent doctrine to defend. Each reading carries its own ε, beneficiary/victim structure, and classification; they are linked here via affects_constraints because the readings compete for interpretive dominance over the same historical religious complex and evidence bearing on one reading's plausibility is often cited as evidence against the others.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
