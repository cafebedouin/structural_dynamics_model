% ============================================================================
% CONSTRAINT STORY: digital_money_origin__became_thinkable_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_digital_money_origin__became_thinkable_reading, []).

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
 *   constraint_id: digital_money_origin__became_thinkable_reading
 *   human_readable: Digital Money Emerged at Conceptual Conceivability — Origin-Dating Convention (became_thinkable Reading)
 *   domain: monetary_history/technology_studies/institutional_economics
 *
 * SUMMARY:
 *   This story instantiates the became_thinkable_reading of the kernel
 *   digital_money_origin: the claim that digital money emerged when the
 *   concept became technically and institutionally conceivable — roughly the
 *   late 1970s through the 1980s, when electronic funds transfer matured,
 *   blind-signature research made untraceable digital cash articulable, and
 *   central-bank programs took up electronic money — prior to widespread
 *   implementation. The standing arrangement under contest, and the sole ε
 *   referent, is the conceivability threshold together with the periodization
 *   convention that dates emergence at it: a gating structure over digital
 *   money's history that coordinates discourse around a shared origin while
 *   allocating founding credit to the architects of the concept and
 *   positioning later builders and informal practitioners as diffusion,
 *   prehistory, or omission. Per the ε-invariance rule this file authors one
 *   reading only; the sibling readings (first_held_reading,
 *   regulatory_recognition_reading) are separate constraints linked through
 *   network.affects_constraints, and no ε is hedged or averaged across them.
 *   KEY AGENTS (by structural relationship): early_institutional_architects —
 *   primary beneficiary (powerful/identity_locked), collect founding credit,
 *   fused with the origin claim; central_banking_institutions — secondary
 *   beneficiary (institutional/arbitrage), inherit the stewardship genealogy;
 *   monetary_history_gatekeepers — agenda setter with beneficiary position
 *   (institutional/constrained), administer the dating;
 *   practical_implementers — primary target (organized/mobile), positioned as
 *   diffusion; informal_value_transfer_networks — excluded target
 *   (powerless/trapped), erased from the story; heterodox_monetary_historians
 *   — resisting target (organized/mobile), publish rival datings;
 *   science_technology_studies_scholars — analytical observer, trace the
 *   genealogy politics without holding a dating.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(digital_money_origin__became_thinkable_reading, 0.6).
domain_priors:suppression_score(digital_money_origin__became_thinkable_reading, 0.52).
domain_priors:theater_ratio(digital_money_origin__became_thinkable_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(digital_money_origin__became_thinkable_reading, extractiveness, 0.6).
narrative_ontology:constraint_metric(digital_money_origin__became_thinkable_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(digital_money_origin__became_thinkable_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(digital_money_origin__became_thinkable_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(digital_money_origin__became_thinkable_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(digital_money_origin__became_thinkable_reading, tangled_rope).
narrative_ontology:human_readable(digital_money_origin__became_thinkable_reading, "Digital Money Emerged at Conceptual Conceivability — Origin-Dating Convention (became_thinkable Reading)").
narrative_ontology:topic_domain(digital_money_origin__became_thinkable_reading, "monetary_history/technology_studies/institutional_economics").

domain_priors:requires_active_enforcement(digital_money_origin__became_thinkable_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(digital_money_origin__became_thinkable_reading, 'a8b3c72a-fa71-40e1-a498-bb3835735452').
narrative_ontology:cs_kernel_codification('a8b3c72a-fa71-40e1-a498-bb3835735452', distributed).
narrative_ontology:cs_authority_grounding('a8b3c72a-fa71-40e1-a498-bb3835735452', expertise).
narrative_ontology:cs_interpretation_layer_present('a8b3c72a-fa71-40e1-a498-bb3835735452').
narrative_ontology:cs_reading_relation('a8b3c72a-fa71-40e1-a498-bb3835735452', digital_money_origin__first_held_reading, coexists_with).
narrative_ontology:cs_reading_relation('a8b3c72a-fa71-40e1-a498-bb3835735452', digital_money_origin__regulatory_recognition_reading, influences).
narrative_ontology:cs_axiom('a8b3c72a-fa71-40e1-a498-bb3835735452', foundational, conceptual_articulation_constitutes_emergence).
narrative_ontology:cs_axiom_status(conceptual_articulation_constitutes_emergence, holdable).
narrative_ontology:cs_axiom_grounding('a8b3c72a-fa71-40e1-a498-bb3835735452', conceptual_articulation_constitutes_emergence, empirically_contingent).
narrative_ontology:cs_axiom('a8b3c72a-fa71-40e1-a498-bb3835735452', secondary, implementation_constitutes_diffusion_not_emergence).
narrative_ontology:cs_axiom_status(implementation_constitutes_diffusion_not_emergence, holdable).
narrative_ontology:cs_axiom_grounding('a8b3c72a-fa71-40e1-a498-bb3835735452', implementation_constitutes_diffusion_not_emergence, empirically_contingent).
narrative_ontology:cs_reference_frame('a8b3c72a-fa71-40e1-a498-bb3835735452', conceptual_inception_frame).
narrative_ontology:cs_drift_state('a8b3c72a-fa71-40e1-a498-bb3835735452', post_bitcoin_implementation_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('a8b3c72a-fa71-40e1-a498-bb3835735452', '').
narrative_ontology:cs_kernel_id(digital_money_origin__became_thinkable_reading, digital_money_origin).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(digital_money_origin__became_thinkable_reading, early_institutional_architects).
narrative_ontology:constraint_beneficiary(digital_money_origin__became_thinkable_reading, central_banking_institutions).
narrative_ontology:constraint_beneficiary(digital_money_origin__became_thinkable_reading, monetary_history_gatekeepers).
narrative_ontology:constraint_victim(digital_money_origin__became_thinkable_reading, practical_implementers).
narrative_ontology:constraint_victim(digital_money_origin__became_thinkable_reading, informal_value_transfer_networks).
narrative_ontology:constraint_victim(digital_money_origin__became_thinkable_reading, heterodox_monetary_historians).
narrative_ontology:constraint_vindicates(digital_money_origin__became_thinkable_reading, conceptual_priority_doctrine).
narrative_ontology:constraint_vindicates(digital_money_origin__became_thinkable_reading, institutional_precedence_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Cryptography researchers, electronic-payment theorists, and central-bank research divisions of the 1970s-1980s whose papers, patents, and pilot projects made untraceable digital cash articulable and fundable. The convention dates the field's beginning to their moment, so citation priority, keynote genealogies, and founder status flow to them decades after most of their commercial ventures failed. Their standing is bound to the origin claim: a re-dating dissolves their founder identity, and no comparable asset replaces it.
narrative_ontology:constraint_stakeholder(digital_money_origin__became_thinkable_reading, early_institutional_architects, beneficiary,
    powerful, biographical, identity_locked, global).

% Monetary authorities that studied electronic money from the early 1980s and now inherit a stewardship narrative: the story that digital money's history runs through institutional research and prudence rather than informal or commercial improvisation. The dating anchors regulatory-continuity arguments from early e-money reports to present frameworks. They can re-narrate their institutional history at will and bear almost none of the convention's costs.
narrative_ontology:constraint_stakeholder(digital_money_origin__became_thinkable_reading, central_banking_institutions, beneficiary,
    institutional, generational, arbitrage, global).

% Textbook authors, journal editors, and curriculum committees who administer the dating through peer review, syllabi, and canonical surveys. Their editorial authority and career rewards are invested in the convention they maintain; switching to a rival dating would devalue their accumulated narrative capital, though the switch itself remains available to them.
narrative_ontology:constraint_stakeholder(digital_money_origin__became_thinkable_reading, monetary_history_gatekeepers, agenda_setter,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(digital_money_origin__became_thinkable_reading, monetary_history_gatekeepers, beneficiary).

% Engineers and entrepreneurs who built working digital value systems — early electronic-payment processors, mobile-money platforms, cryptocurrency implementers. The convention classifies their work as diffusion of an already-emerged phenomenon, denying them founding status however transformative their systems proved. Many respond by building their own genealogies; the residual cost is permanent secondary positioning in the mainstream record.
narrative_ontology:constraint_stakeholder(digital_money_origin__became_thinkable_reading, practical_implementers, payer,
    organized, biographical, mobile, global).

% Hawala operators, community-currency organizers, and other practitioners of digital-adjacent value transfer whose systems predate or parallel canonical digital money but were never articulable within the institutional discourse that defines conceivability. The origin story omits them entirely; they hold no seat in the historiographical conversation and no way to claim the standing their operating practice might otherwise earn.
narrative_ontology:constraint_stakeholder(digital_money_origin__became_thinkable_reading, informal_value_transfer_networks, payer,
    powerless, generational, trapped, global).
narrative_ontology:stakeholder_secondary_role(digital_money_origin__became_thinkable_reading, informal_value_transfer_networks, excluded).

% Economic sociologists, anthropologists of money, and heterodox historians who date origins at first holding or at regulatory recognition and publish counter-genealogies. They bear citation and venue costs in gatekeeping journals while sustaining an expanding parallel venue network that makes their position livable.
narrative_ontology:constraint_stakeholder(digital_money_origin__became_thinkable_reading, heterodox_monetary_historians, payer,
    organized, biographical, mobile, global).

% Scholars of the history and sociology of technology who trace how origin claims are constructed and contested without themselves holding a dating. They observe the full structure — whose concept, whose threshold, whose erasure — and collect no founding credit and bear no erasure.
narrative_ontology:constraint_stakeholder(digital_money_origin__became_thinkable_reading, science_technology_studies_scholars, observer,
    analytical, biographical, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(digital_money_origin__became_thinkable_reading, early_institutional_architects).
narrative_ontology:fixing_cost_class(digital_money_origin__became_thinkable_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Supplies a single conventional origin point for digital money, allowing monetary historians, textbook writers, and policy analysts to periodize the field, compare national trajectories, and teach a coherent curriculum rather than re-litigating origins in every study.
% TRANSFER_FUNCTION: Moves founding status, citation priority, and genealogical authority from practical implementers and informal value-transfer practitioners to the conceptual and institutional architects whose framing defines the origin point, and moves narrative ownership of digital money's history into academic and central-bank channels.
% ABSENT_VOICES: Hawala operators, community-currency organizers, and uncredentialed builders of working digital value systems are outside the historiographical conversation entirely; they would date origins by practice and holding and would contest the erasure of informal digital value transfer from the origin story. Non-Western monetary historians are also underrepresented in the gatekeeping venues where the convention is administered.
% DISAPPEARANCE_RATIONALE: If the conceivability dating vanished overnight, curricula, textbook narratives, and policy genealogies would re-periodize around one of the rival datings, founding credit would redistribute from architects to early holders or to recognizing authorities, and the stewardship narrative running from 1980s e-money research to present regulation would lose its anchor.
% FOUNDING_PROBLEM: Digital money's history was undated and fragmented: theorists, practitioners, and regulators each marked beginnings differently, making comparative monetary history, curriculum design, and regulatory analysis unreliable.
% FOUNDING_PROBLEM_CORROBORATION: Heterodox historians and practitioner communities — outside the beneficiary set — corroborate that a shared periodization remains needed (they publish competing datings precisely because the coordination problem is real) while disputing that conceivability is the right criterion. No source outside the beneficiary set attests that the conceivability dating specifically resolves the problem correctly.
narrative_ontology:disappearance_verdict(digital_money_origin__became_thinkable_reading, world_rearranges).
narrative_ontology:founding_problem_status(digital_money_origin__became_thinkable_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(digital_money_origin__became_thinkable_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(digital_money_origin__became_thinkable_reading, 'none', 1).
narrative_ontology:epsilon_provenance(digital_money_origin__became_thinkable_reading, 0.6, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(digital_money_origin__became_thinkable_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(digital_money_origin__became_thinkable_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(digital_money_origin__became_thinkable_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Claim and metrics are authored independently. The claim is tangled_rope on structure: a genuine coordination function (some conventional origin point is required for comparative monetary history, teaching, and policy analysis) fused with asymmetric extraction (founding credit flows to the architects of the concept; implementers are positioned as diffusion and informal practitioners are omitted entirely), held together by active enforcement (curricular gatekeeping, peer review, textbook conformity) — hence requires_active_enforcement, declared beneficiaries, and declared victims. Metrics: extractiveness 0.60 — substantial credit and status extraction bounded by the convention's real coordination yield. Suppression 0.52 is structural (venue access, citation networks, syllabus control) with a modest internalized component (canon training in graduate curricula); it is authored as a raw structural property and is not scaled by power or scope — the engine owns any scaling, and only of extractiveness. Theater_ratio 0.45 — anniversary narratives, prophet-genealogies, and founding-myth keynotes increasingly perform the convention's maintenance as its descriptive authority erodes. Accessibility_collapse 0.45 — rival datings remain publishable and live; the convention does not collapse its alternatives. Resistance 0.55 — sustained practitioner and heterodox counter-historiography. The measurement series runs on one shared grid (seven points, 1980-2025) with all three tracked metrics authored at every point; extraction rises through the convention's consolidation and crypto-era stake inflation, then eases slightly as practitioner historiography gains standing, while theater rises monotonically and enforcement rises with a late easing as parallel venues institutionalize.
 *
 * PERSPECTIVAL GAP:
 *   The architect seat computes the convention as discovery: emergence genuinely happened at conceivability, the dating is truth-tracking, and the credit it carries is deserved. The implementer seat computes the same convention as credit capture: emergence is what working systems do, and conceivability is a retrospective claim staked by whoever owns the conceptual record. The gatekeeper seat computes it as infrastructure: the convention is what the discipline administers, and its operative value is coordination rather than truth. The trapped seat (informal practitioners) experiences pure erasure — the convention does not even mis-date them, it omits them. Same nominal discourse, four different constraints; the engine computes per-seat classifications from the power, exit, and directionality data rather than from this commentary. An implementer-heterodox coalition is possible in principle — both prefer later datings — but the seats' different preferred criteria (holding versus recognition) have so far kept it loose.
 *
 * DIRECTIONALITY LOGIC:
 *   Architects are declared beneficiaries with identity-locked exit: their d sits near the beneficiary end, and their fusion with the origin claim makes the position self-reinforcing. Central banks are beneficiaries with arbitrage-grade exit — they can re-narrate institutional history at will — so their d sits nearest zero and the convention effectively subsidizes their stewardship narrative. Gatekeepers carry the agenda-setter role with a beneficiary secondary role: low-to-moderate d, collecting career rents from administration. Implementers and heterodox historians are declared victims; mobility damps their effective extraction below the trapped end, since both can and do build parallel venues. Informal value-transfer networks are victims with trapped exit: they sit nearest the full-target end, bearing erasure with no discursive exit at all. The observer seat collects no directional position. No directionality overrides are authored: the beneficiary/victim declarations plus exit atoms produce the correct d for every seat, and the gatekeeper seat is given structural data through its beneficiary secondary role rather than an override.
 *
 * MANDATROPHY ANALYSIS:
 *   Two mislabels are prevented. As natural law: the convention presents itself as discovered fact — emergence simply happened at conceivability — but the dating is a choice among live alternatives, meets sustained resistance, and leaves its alternatives accessible; no naturality is authored and emerges_naturally stays unset. As pure extraction: the extraction is real but rides a coordination function that any periodization must supply, and a pure-extraction reading would mispredict why the convention survives contestation (comparative history, teaching, and policy analysis would still need some conventional dating even if this one fell). The tangled-rope classification holds both facts. The founding problem — the need for a shared periodization — remains live, so no mandatrophy resolution is declared; the theater measurements track the growing performative share of maintenance without the mandate itself having expired.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'This constraint is one reading of the kernel digital_money_origin; what would the sibling readings (first_held, regulatory_recognition) change structurally if adopted as the governing periodization?',
    'Corpus-level comparison of the three reading-stories'' beneficiary/victim structures and computed types; no within-story data resolves a reading choice.',
    'Adopting first_held moves the origin to practical holding, shifting beneficiaries from architects to early holders and victims toward conceptual gatekeepers; adopting regulatory_recognition shifts beneficiaries to central banks and statistical agencies. This story''s classification is reading-indexed, not kernel-level.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Committer structure: which reading of the origin kernel this story instantiates and what siblings would structurally change.').

omega_variable(
    conceivability_threshold_location,
    'When exactly did digital money become technically and institutionally conceivable — 1970s electronic funds transfer, Chaum''s 1982 blind-signature work, and the late-1980s DigiCash venture bracket a decade of ambiguity?',
    'Archival work on central-bank research programs, patent filings, and academic citation onset for electronic-money concepts.',
    'An earlier threshold widens the architect beneficiary set to EFT-era actors and deepens the exclusion of 1990s implementers; a later threshold shrinks the convention''s genealogical reach and the stewardship narrative it anchors.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(conceivability_threshold_location, empirical, 'Internal under-determination of the reading''s own origin date.').

omega_variable(
    exclusion_boundary_status,
    'Are informal value-transfer networks (Hawala, community currencies) genuine victims of the conceivability arrangement, or simply outside the category ''digital money'' that the arrangement periodizes?',
    'Conceptual analysis of how the convention''s category boundary is drawn: if ''digital money'' is defined so that informal digital-adjacent value transfer falls outside by construction, victim status holds; if the category independently excludes them, the victim declaration over-reaches.',
    'If outside the category, the victim set shrinks to implementers and heterodox historians and the trapped seat''s effective extraction drops; if inside only by exclusion, the erasure function is confirmed and current extraction is understated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(exclusion_boundary_status, conceptual, 'Whether the excluded-practitioner victim class is real or a category artifact.').

omega_variable(
    coordination_vs_credit_capture,
    'Is the shared-periodization function genuine (comparative monetary history needs some conventional origin), or is it cover for founding-credit capture?',
    'Counterfactual comparison: did pre-convention monetary history suffer measurable dating fragmentation, and would the sibling datings coordinate discourse equally well?',
    'If any conventional dating coordinates equally well, the specific choice of conceivability is pure credit allocation and the arrangement trends toward pure extraction; if conceivability uniquely serves comparative analysis, more of the measured extraction is coordination cost.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_vs_credit_capture, empirical, 'Whether the coordination function is real or cover for capture.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(digital_money_origin__became_thinkable_reading, 1980, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(digi_tr_t1980, digital_money_origin__became_thinkable_reading, theater_ratio, 1980, 0.12).
narrative_ontology:measurement_basis(digi_tr_t1980, observed).
narrative_ontology:measurement(digi_tr_t1988, digital_money_origin__became_thinkable_reading, theater_ratio, 1988, 0.16).
narrative_ontology:measurement_basis(digi_tr_t1988, observed).
narrative_ontology:measurement(digi_tr_t1996, digital_money_origin__became_thinkable_reading, theater_ratio, 1996, 0.22).
narrative_ontology:measurement_basis(digi_tr_t1996, observed).
narrative_ontology:measurement(digi_tr_t2004, digital_money_origin__became_thinkable_reading, theater_ratio, 2004, 0.28).
narrative_ontology:measurement_basis(digi_tr_t2004, observed).
narrative_ontology:measurement(digi_tr_t2012, digital_money_origin__became_thinkable_reading, theater_ratio, 2012, 0.35).
narrative_ontology:measurement_basis(digi_tr_t2012, observed).
narrative_ontology:measurement(digi_tr_t2020, digital_money_origin__became_thinkable_reading, theater_ratio, 2020, 0.42).
narrative_ontology:measurement_basis(digi_tr_t2020, observed).
narrative_ontology:measurement(digi_tr_t2025, digital_money_origin__became_thinkable_reading, theater_ratio, 2025, 0.45).
narrative_ontology:measurement_basis(digi_tr_t2025, observed).

% Extraction over time
narrative_ontology:measurement(digi_be_t1980, digital_money_origin__became_thinkable_reading, base_extractiveness, 1980, 0.32).
narrative_ontology:measurement_basis(digi_be_t1980, observed).
narrative_ontology:measurement(digi_be_t1988, digital_money_origin__became_thinkable_reading, base_extractiveness, 1988, 0.42).
narrative_ontology:measurement_basis(digi_be_t1988, observed).
narrative_ontology:measurement(digi_be_t1996, digital_money_origin__became_thinkable_reading, base_extractiveness, 1996, 0.52).
narrative_ontology:measurement_basis(digi_be_t1996, observed).
narrative_ontology:measurement(digi_be_t2004, digital_money_origin__became_thinkable_reading, base_extractiveness, 2004, 0.58).
narrative_ontology:measurement_basis(digi_be_t2004, observed).
narrative_ontology:measurement(digi_be_t2012, digital_money_origin__became_thinkable_reading, base_extractiveness, 2012, 0.64).
narrative_ontology:measurement_basis(digi_be_t2012, observed).
narrative_ontology:measurement(digi_be_t2020, digital_money_origin__became_thinkable_reading, base_extractiveness, 2020, 0.62).
narrative_ontology:measurement_basis(digi_be_t2020, observed).
narrative_ontology:measurement(digi_be_t2025, digital_money_origin__became_thinkable_reading, base_extractiveness, 2025, 0.6).
narrative_ontology:measurement_basis(digi_be_t2025, observed).

% Suppression requirement over time
narrative_ontology:measurement(digi_su_t1980, digital_money_origin__became_thinkable_reading, suppression_requirement, 1980, 0.28).
narrative_ontology:measurement_basis(digi_su_t1980, observed).
narrative_ontology:measurement(digi_su_t1988, digital_money_origin__became_thinkable_reading, suppression_requirement, 1988, 0.33).
narrative_ontology:measurement_basis(digi_su_t1988, observed).
narrative_ontology:measurement(digi_su_t1996, digital_money_origin__became_thinkable_reading, suppression_requirement, 1996, 0.38).
narrative_ontology:measurement_basis(digi_su_t1996, observed).
narrative_ontology:measurement(digi_su_t2004, digital_money_origin__became_thinkable_reading, suppression_requirement, 2004, 0.44).
narrative_ontology:measurement_basis(digi_su_t2004, observed).
narrative_ontology:measurement(digi_su_t2012, digital_money_origin__became_thinkable_reading, suppression_requirement, 2012, 0.5).
narrative_ontology:measurement_basis(digi_su_t2012, observed).
narrative_ontology:measurement(digi_su_t2020, digital_money_origin__became_thinkable_reading, suppression_requirement, 2020, 0.55).
narrative_ontology:measurement_basis(digi_su_t2020, observed).
narrative_ontology:measurement(digi_su_t2025, digital_money_origin__became_thinkable_reading, suppression_requirement, 2025, 0.52).
narrative_ontology:measurement_basis(digi_su_t2025, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(digital_money_origin__became_thinkable_reading, information_standard).
narrative_ontology:affects_constraint(digital_money_origin__became_thinkable_reading, digital_money_origin__first_held_reading).
narrative_ontology:affects_constraint(digital_money_origin__became_thinkable_reading, digital_money_origin__regulatory_recognition_reading).

% DUAL FORMULATION NOTE:
% The colloquial question 'when did digital money emerge' decomposes into three structurally distinct periodization claims (per the ε-invariance principle): emergence at conceptual/institutional conceivability (this file), at first practical holding (first_held_reading), and at formal regulatory incorporation (regulatory_recognition_reading). Each has its own ε, beneficiary set, and victim set; the conceivability reading is upstream of the regulatory reading because the conceptual framing it enshrines defines the object that regulators later recognize. All family members are linked via network.affects_constraints; no ε is hedged across readings within any single file.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
