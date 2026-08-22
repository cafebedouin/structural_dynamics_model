% ============================================================================
% CONSTRAINT STORY: imposition_pathway_kernel__endogenous_climb_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_imposition_pathway_kernel__endogenous_climb_reading, []).

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
 *   constraint_id: imposition_pathway_kernel__endogenous_climb_reading
 *   human_readable: Endogenous Climb Reading of Commitment Displacement Pathways
 *   domain: historical sociology/state formation/commitment systems
 *
 * SUMMARY:
 *   This story instantiates the endogenous_climb_reading of the
 *   imposition_pathway_kernel: the operative standard in comparative
 *   historical sociology under which commitment displacement — calendar
 *   reform, dress codes, legal and ritual obligations — is explained
 *   exclusively as fringe adoption followed by gradual climb, with state
 *   decrees cast as accelerants that ratify change already underway, and
 *   apparent top-down impositions recoded as compressed climbs with invisible
 *   fringe stages. Enforcement runs through peer review at flagship venues,
 *   canon formation in textbooks, and graduate curricular formation. The
 *   epsilon referent is the standing arrangement under contest — the climb
 *   template as operative explanatory standard together with its gatekeeping
 *   machinery — assessed by this reading's own lights: the reading endorses
 *   the mechanism as descriptively true, so the authored epsilon prices the
 *   costs it can acknowledge from inside (anomaly-recoding, rival-program
 *   marginalization, curricular pre-emption), not the fuller indictment a
 *   sibling reading would author over its own referent. Sibling readings
 *   (exogenous_override_reading, hybrid_cascade_reading) are separate
 *   constraint files linked through network.affects_constraints. The claim
 *   and the metrics are authored independently: claimed_type from structural
 *   belief, metrics from descriptive operation.
 *
 * KEY AGENTS:
 *   - - peer_review_gatekeepers: Agenda setter (institutional/arbitrage) — administers acceptance, converts enforcement into editorial scarcity and citation concentration
 *   - - comparative_historical_sociologists: Primary beneficiary (organized/constrained) — collects comparability, career capital, curricular presence
 *   - - state_capacity_scholars: Primary target (powerful/constrained) — rival program recast as describing compression, flagship access denied
 *   - - regional_archival_specialists: Target (moderate/trapped) — evidentiary craft conscripted; archive silence read as invisible fringe
 *   - - graduate_trainees: Target (powerless/identity_locked) — formed into the template before independent evaluation
 *   - - policy_advisors_on_reform: Downstream target with incidental benefit (organized/mobile)
 *   - - non_western_historiographers: Excluded voice (moderate/constrained) — decree-led documentary traditions without venue standing
 *   - - methodology_observers: Analytical observer (analytical/analytical) — audits the universality inference and the falsifiability of the invisibility clause
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(imposition_pathway_kernel__endogenous_climb_reading, 0.6).
domain_priors:suppression_score(imposition_pathway_kernel__endogenous_climb_reading, 0.58).
domain_priors:theater_ratio(imposition_pathway_kernel__endogenous_climb_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(imposition_pathway_kernel__endogenous_climb_reading, extractiveness, 0.6).
narrative_ontology:constraint_metric(imposition_pathway_kernel__endogenous_climb_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(imposition_pathway_kernel__endogenous_climb_reading, theater_ratio, 0.35).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(imposition_pathway_kernel__endogenous_climb_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(imposition_pathway_kernel__endogenous_climb_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(imposition_pathway_kernel__endogenous_climb_reading, tangled_rope).
narrative_ontology:human_readable(imposition_pathway_kernel__endogenous_climb_reading, "Endogenous Climb Reading of Commitment Displacement Pathways").
narrative_ontology:topic_domain(imposition_pathway_kernel__endogenous_climb_reading, "historical sociology/state formation/commitment systems").

domain_priors:requires_active_enforcement(imposition_pathway_kernel__endogenous_climb_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(imposition_pathway_kernel__endogenous_climb_reading, '4e24e3af-5e77-4699-8b69-9dc5d35c881d').
narrative_ontology:cs_kernel_codification('4e24e3af-5e77-4699-8b69-9dc5d35c881d', distributed).
narrative_ontology:cs_authority_grounding('4e24e3af-5e77-4699-8b69-9dc5d35c881d', expertise).
narrative_ontology:cs_interpretation_layer_present('4e24e3af-5e77-4699-8b69-9dc5d35c881d').
narrative_ontology:cs_reading_relation('4e24e3af-5e77-4699-8b69-9dc5d35c881d', imposition_pathway_kernel__exogenous_override_reading, forecloses).
narrative_ontology:cs_reading_relation('4e24e3af-5e77-4699-8b69-9dc5d35c881d', imposition_pathway_kernel__hybrid_cascade_reading, forecloses).
narrative_ontology:cs_axiom('4e24e3af-5e77-4699-8b69-9dc5d35c881d', foundational, fringe_climb_pathway_universality).
narrative_ontology:cs_axiom_status(fringe_climb_pathway_universality, holdable).
narrative_ontology:cs_axiom_grounding('4e24e3af-5e77-4699-8b69-9dc5d35c881d', fringe_climb_pathway_universality, empirically_contingent).
narrative_ontology:cs_axiom('4e24e3af-5e77-4699-8b69-9dc5d35c881d', foundational, decrees_ratify_not_initiate).
narrative_ontology:cs_axiom_status(decrees_ratify_not_initiate, holdable).
narrative_ontology:cs_axiom_grounding('4e24e3af-5e77-4699-8b69-9dc5d35c881d', decrees_ratify_not_initiate, empirically_contingent).
narrative_ontology:cs_reference_frame('4e24e3af-5e77-4699-8b69-9dc5d35c881d', endogenous_climb_universality).
narrative_ontology:cs_drift_state('4e24e3af-5e77-4699-8b69-9dc5d35c881d', contemporary, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('4e24e3af-5e77-4699-8b69-9dc5d35c881d', '').
narrative_ontology:cs_kernel_id(imposition_pathway_kernel__endogenous_climb_reading, imposition_pathway_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(imposition_pathway_kernel__endogenous_climb_reading, comparative_historical_sociologists).
narrative_ontology:constraint_beneficiary(imposition_pathway_kernel__endogenous_climb_reading, peer_review_gatekeepers).
narrative_ontology:constraint_victim(imposition_pathway_kernel__endogenous_climb_reading, state_capacity_scholars).
narrative_ontology:constraint_victim(imposition_pathway_kernel__endogenous_climb_reading, regional_archival_specialists).
narrative_ontology:constraint_victim(imposition_pathway_kernel__endogenous_climb_reading, graduate_trainees).
narrative_ontology:constraint_victim(imposition_pathway_kernel__endogenous_climb_reading, policy_advisors_on_reform).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(imposition_pathway_kernel__endogenous_climb_reading, policy_advisors_on_reform).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Editors and reviewers at flagship comparative-historical sociology venues. Accept or reject displacement accounts; a decree-first narrative is desk-rejected or returned with instructions to locate the pre-decree adopters. Converts enforcement into editorial scarcity, citation concentration, and curricular authority; accepted-manuscript surplus and review labor flow back to the gatekeeping seats. Exit is arbitrage: reputation carries into adjacent fields if the venue system loses standing.
narrative_ontology:constraint_stakeholder(imposition_pathway_kernel__endogenous_climb_reading, peer_review_gatekeepers, agenda_setter,
    institutional, generational, arbitrage, global).

% Practitioners whose working toolkit is the climb protocol: identify fringe adopters, date the adoption curve, read the decree as accelerant. The template yields publishable, comparable accounts across national cases, grant eligibility, and syllabus presence. Many sincerely hold the thesis and defend it against rivals at real effort. Leaving means retraining into a rival framework at the cost of accumulated case capital.
narrative_ontology:constraint_stakeholder(imposition_pathway_kernel__endogenous_climb_reading, comparative_historical_sociologists, beneficiary,
    organized, biographical, constrained, global).

% Political sociologists who argue that state capacity sometimes displaces commitments directly — conscription, language policy, revolutionary calendars imposed on resistant populations. Under the operative standard their cases are recast as describing the compression of a climb rather than a distinct mechanism, and flagship venues are effectively closed to the rival framing. They publish in adjacent outlets and absorb the citation penalty; abandoning the program means abandoning their core questions.
narrative_ontology:constraint_stakeholder(imposition_pathway_kernel__endogenous_climb_reading, state_capacity_scholars, payer,
    powerful, generational, constrained, global).

% Japanists, Ottomanists, Turkishists, and similar area specialists whose archives can date adoption curves against decree dates. The operative standard expects their evidence to yield pre-decree adopters; where the record is silent, the silence is read as an invisible fringe stage rather than as disconfirmation. Their evidentiary craft is conscripted to confirm the template, and their publication channel is governed by the same gatekeeping that sets the expectation. Career-specific skills and channel monopoly make exit costly.
narrative_ontology:constraint_stakeholder(imposition_pathway_kernel__endogenous_climb_reading, regional_archival_specialists, payer,
    moderate, biographical, trapped, regional).

% Doctoral students formed through seminar canons that present the climb pathway as settled before they encounter rival mechanisms. Professional self-concept fuses with paradigm fluency: questioning the template reads, to examiners and to themselves, as methodological immaturity. Exit would mean re-founding a professional identity mid-career.
narrative_ontology:constraint_stakeholder(imposition_pathway_kernel__endogenous_climb_reading, graduate_trainees, payer,
    powerless, biographical, identity_locked, national).

% Ministry advisers and consultants who translate the scholarship into sequencing doctrine: pilot with fringe constituencies, build coalitions, then legislate. They benefit incidentally when sequencing succeeds and sellable roadmaps result; they bear the cost when a decree-first window existed and incremental sequencing forfeited it. They can switch frameworks between engagements.
narrative_ontology:constraint_stakeholder(imposition_pathway_kernel__endogenous_climb_reading, policy_advisors_on_reform, payer,
    organized, immediate, mobile, continental).
narrative_ontology:stakeholder_secondary_role(imposition_pathway_kernel__endogenous_climb_reading, policy_advisors_on_reform, beneficiary).

% Scholars working in non-Anglophone documentary traditions whose sources narrate decree-led change — edict texts treated as causal, chronicles recording compliance following promulgation. They lack standing in the gatekept venues and enter the literature only as raw material. Were they seated, they would contest the universality claim from traditions the template reads through rather than with.
narrative_ontology:constraint_stakeholder(imposition_pathway_kernel__endogenous_climb_reading, non_western_historiographers, excluded,
    moderate, generational, constrained, global).

% Philosophers and methodologists of the historical sciences who audit the inference from celebrated cases to universal claims. They track whether the invisibility clause is empirically restrictive and whether case selection tracks archival legibility. No stake in which reading wins.
narrative_ontology:constraint_stakeholder(imposition_pathway_kernel__endogenous_climb_reading, methodology_observers, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(imposition_pathway_kernel__endogenous_climb_reading, peer_review_gatekeepers).
narrative_ontology:fixing_cost_class(imposition_pathway_kernel__endogenous_climb_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the comparability problem in comparative historical sociology: a single protocol (locate fringe adopters, date the climb, read the decree as accelerant) renders heterogeneous national displacement episodes commensurable, makes research cumulative, and gives graduate training a teachable method.
% TRANSFER_FUNCTION: Moves explanatory authority and career capital toward scholars producing climb-narratives; moves anomalous decree-first evidence into mandatory compressed-climb recoding; moves curricular content into graduate formation before independent evaluation of alternatives.
% ABSENT_VOICES: Non-Western historiographers holding decree-led documentary traditions are structurally outside the conversation and would contest universality from sources the template reads through; state-centric scholars sit inside the discipline but outside the flagship venues their objections would need. The displaced populations themselves — Meiji commoners, Anatolian villagers — appear only as adopter data, not as voices on what compliance felt like.
% DISAPPEARANCE_RATIONALE: If the climb standard vanished overnight, syllabi, grant templates, and the displacement literatures would reorganize around competing protocols; the state-capacity program would immediately contest the vacated cell; policy sequencing doctrine would lose its scholarly warrant and reform playbooks would be rewritten.
% FOUNDING_PROBLEM: Postwar modernization theory needed a general account of how societies adopt new binding commitments without reducing change either to coercion or to Western agency; the climb template supplied an endogenous, sequenced mechanism in which decrees ratify social change already underway.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: state-capacity scholars and comparative-methods methodologists attest the founding problem is misconceived as posed (displacement mechanisms are plural, not one); area-specialist archives attest decree-first episodes the template must strain to absorb; no beneficiary-independent source attests that the problem as originally framed remains live in its original form.
narrative_ontology:disappearance_verdict(imposition_pathway_kernel__endogenous_climb_reading, world_rearranges).
narrative_ontology:founding_problem_status(imposition_pathway_kernel__endogenous_climb_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(imposition_pathway_kernel__endogenous_climb_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(imposition_pathway_kernel__endogenous_climb_reading, 'none', 1).
narrative_ontology:epsilon_provenance(imposition_pathway_kernel__endogenous_climb_reading, 0.6, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(imposition_pathway_kernel__endogenous_climb_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(imposition_pathway_kernel__endogenous_climb_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(imposition_pathway_kernel__endogenous_climb_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is 0.60: the template delivers genuine commensurability, but operating it as a universal extracts a recoding tax from area specialists, closes venue access to the rival program, and pre-empts trainee judgment — costs the reading itself can acknowledge. Suppression is 0.58 as a raw structural property (unscaled by power or scope; only extractiveness is scaled downstream): gatekeeping is real but alternatives survive in adjacent fields and specialist outlets, keeping it below snare-grade closure. Theater is 0.35: ritual citation of Meiji as the canonical climb and pro-forma diffusion paragraphs recur regardless of case evidence, while the protocol still performs real explanatory work. Accessibility_collapse is 0.40 — the sibling readings remain live positions, so understanding the template does not collapse alternatives. Resistance is 0.55 — sustained critique from state-capacity scholars, methodologists, and postcolonial historiography. The measurement series run on one shared grid (t0 approx 1955, postwar consolidation of modernization theory; tn approx 2025): extractiveness and theater rise monotonically as the paradigm consolidates and anomaly-absorption deepens, with a slight late plateau as visible contestation raises the price of further extraction; suppression_requirement traces the enforcement machinery's maturation through the canon-building decades and its plateau once gatekeeping saturated — this series is authored because the story specifically tracks enforcement-capacity change, not merely extraction shift. Receipt and cost surface: gains demonstrably accrue to the gatekeeping seat (submission surplus converted into scarcity rents and citation concentration), so gain_flow names that seat rather than asserting diffuse; fixing is prohibitive for whoever could fix it — the gatekeeping complex would have to dissolve the authority structure that constitutes its own position, a cost exceeding any benefit it perceives.
 *
 * PERSPECTIVAL GAP:
 *   From the gatekeeper seat the template is quality control: it filters under-sourced narratives and keeps comparison disciplined, so that seat should compute a classification nearer rope. From the area-specialist seat the same structure is a recoding tax — the archive must yield pre-decree adopters or its silence is read as invisibility — so that seat computes heavy extraction under trapped exit. Graduate trainees experience pre-consent formation: the identity lock is professional (career path dependence fused with paradigm fluency during formation), and if the frame broke they would re-enter the field as comparatively neutral analysts, raising their computed mobility. Policy seats experience sequencing doctrine that fails exactly when a decree-first window existed. Same-level divergence: state_capacity_scholars and comparative_historical_sociologists hold comparable seniority and standing, yet venue control differentiates their exits — one seat's constrained exit is the other seat's market position. The engine computes this per-seat divergence from the structural data; the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Gatekeepers and paradigm scholars sit near the beneficiary pole: the template subsidizes their publication, citation share, and authority, and their exit options (arbitrage, constrained-but-real) dampen effective extraction further. Declared victims derive high directionality: state_capacity_scholars near 0.8 (powerful individuals, venue-constrained as a class), regional_archival_specialists near 0.85 (trapped by career-specific skill and channel monopoly), graduate_trainees nearest the full-target end (identity_locked amplifies beyond their formal powerlessness), policy_advisors around 0.7 (harm is mediated through failed interventions and partly offset by incidental consulting benefit, hence the secondary beneficiary role rather than an override). Excluded historiographers fall outside the derivation entirely — their exclusion is precisely that they are not yet in the conversation the constraint governs. Global spatial scope for the scholarly economy modestly amplifies effective extraction by making the universality claim harder to verify case-by-case.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope claim prevents two opposite mislabels. Reading the template as pure rope would hide the recoding tax area specialists pay and the closure of the rival program's venue access; reading it as snare would erase the genuine commensurability function that made postwar comparison possible and that many practitioners would defend absent any rent. Genealogy: the founding problem — a general, non-coercionist mechanism of norm change — is contested rather than dead; the plural-mechanism turn outside the paradigm holds it misconceived, while insiders still treat it as open. The status-times-verdict pair (contested x world_rearranges) therefore does not trip the zombie flag: the arrangement persists on live career investment and real coordination output, not inertial performance alone, which is why theater_ratio sits at a moderate 0.35 rather than piton range. The temporal series nonetheless shows the classic tangled-rope signature — extraction accumulating on top of coordination as enforcement matured — which is the drift the corpus exists to catch.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_position,
    'This constraint is one reading of imposition_pathway_kernel (endogenous_climb_reading); what would each sibling reading change structurally if adopted?',
    'Cross-file comparison of the three reading files'' epsilon, victim sets, and mechanism cells, joined to the decomposition manifest''s Q-choice record.',
    'Adopting exogenous_override would shrink this constraint''s domain (a separate override cell removes decree-first cases from the climb account), lowering the extraction attributable to climb enforcement; adopting hybrid_cascade would relocate initiation to the state, changing the beneficiary/victim map by entering the state as an initiating agent.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_position, conceptual, 'Committer structure: one reading of the imposition pathway kernel; siblings are exogenous_override_reading and hybrid_cascade_reading.').

omega_variable(
    initiation_evidence_underdetermination,
    'Does dated archival evidence show pre-decree fringe adoption in the canonical cases (Meiji calendar 1873, dress edicts, Turkish hat law 1925, French revolutionary calendar), or do decree-first episodes with no detectable fringe stage exist?',
    'Systematic pre-dating of adoption curves against decree dates across a case battery, with pre-registered detection thresholds for what counts as a fringe stage and stratified sampling across foreign-exposure levels.',
    'Confirmed fringe stages strengthen the endogenous reading and its current classification; demonstrated decree-first cases break the universality axiom, shifting the mechanism cell toward the exogenous_override reading and raising the extraction attributable to anomaly-suppression.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(initiation_evidence_underdetermination, empirical, 'Whether the initiation question resolves for or against pre-decree adoption in the canonical cases.').

omega_variable(
    invisibility_clause_falsifiability,
    'Is the invisible-fringe-stages clause empirically restrictive, or does it render the reading compatible with any evidence (an undetected fringe is always an invisible fringe)?',
    'Pre-registration of what evidence would count as fringe-absence: adoption curves flat until decree within detection limits, no elite or deviant early adopters in any sampled stratum, contemporaneous testimony of surprise at promulgation.',
    'If the clause is unfalsifiable as operated, enforcing the reading shifts from coordination toward suppression of an unfalsifiable claim, pushing effective classification toward snare; if it is falsifiable and passes, the tangled_rope reading stands.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(invisibility_clause_falsifiability, conceptual, 'Whether the reading''s anomaly-absorption clause preserves empirical content or functions as paradigm protection.').

omega_variable(
    meiji_case_selection_bias,
    'Is Meiji representative of commitment displacement generally, or a best case selected because treaty-port exposure made fringe adoption unusually legible to later researchers?',
    'Test whether fringe-stage detection correlates with archival legibility and foreign exposure rather than with pre-decree adoption itself, comparing high- and low-legibility displacement episodes.',
    'If detection tracks legibility, the universality claim rests on observation bias and the reading''s evidential base narrows to maritime-exposure outliers, weakening the axiom that grounds this reading against its siblings.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(meiji_case_selection_bias, empirical, 'Whether the canonical case generalizes or is a legibility artifact.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(imposition_pathway_kernel__endogenous_climb_reading, 0, 70).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ipk_endo_tr_t0, imposition_pathway_kernel__endogenous_climb_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(ipk_endo_tr_t10, imposition_pathway_kernel__endogenous_climb_reading, theater_ratio, 10, 0.16).
narrative_ontology:measurement(ipk_endo_tr_t20, imposition_pathway_kernel__endogenous_climb_reading, theater_ratio, 20, 0.21).
narrative_ontology:measurement(ipk_endo_tr_t35, imposition_pathway_kernel__endogenous_climb_reading, theater_ratio, 35, 0.26).
narrative_ontology:measurement(ipk_endo_tr_t50, imposition_pathway_kernel__endogenous_climb_reading, theater_ratio, 50, 0.3).
narrative_ontology:measurement(ipk_endo_tr_t60, imposition_pathway_kernel__endogenous_climb_reading, theater_ratio, 60, 0.33).
narrative_ontology:measurement(ipk_endo_tr_t70, imposition_pathway_kernel__endogenous_climb_reading, theater_ratio, 70, 0.35).

% Extraction over time
narrative_ontology:measurement(ipk_endo_be_t0, imposition_pathway_kernel__endogenous_climb_reading, base_extractiveness, 0, 0.34).
narrative_ontology:measurement(ipk_endo_be_t10, imposition_pathway_kernel__endogenous_climb_reading, base_extractiveness, 10, 0.42).
narrative_ontology:measurement(ipk_endo_be_t20, imposition_pathway_kernel__endogenous_climb_reading, base_extractiveness, 20, 0.49).
narrative_ontology:measurement(ipk_endo_be_t35, imposition_pathway_kernel__endogenous_climb_reading, base_extractiveness, 35, 0.55).
narrative_ontology:measurement(ipk_endo_be_t50, imposition_pathway_kernel__endogenous_climb_reading, base_extractiveness, 50, 0.59).
narrative_ontology:measurement(ipk_endo_be_t60, imposition_pathway_kernel__endogenous_climb_reading, base_extractiveness, 60, 0.61).
narrative_ontology:measurement(ipk_endo_be_t70, imposition_pathway_kernel__endogenous_climb_reading, base_extractiveness, 70, 0.6).

% Suppression requirement over time
narrative_ontology:measurement(ipk_endo_su_t0, imposition_pathway_kernel__endogenous_climb_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(ipk_endo_su_t10, imposition_pathway_kernel__endogenous_climb_reading, suppression_requirement, 10, 0.38).
narrative_ontology:measurement(ipk_endo_su_t20, imposition_pathway_kernel__endogenous_climb_reading, suppression_requirement, 20, 0.46).
narrative_ontology:measurement(ipk_endo_su_t35, imposition_pathway_kernel__endogenous_climb_reading, suppression_requirement, 35, 0.53).
narrative_ontology:measurement(ipk_endo_su_t50, imposition_pathway_kernel__endogenous_climb_reading, suppression_requirement, 50, 0.57).
narrative_ontology:measurement(ipk_endo_su_t60, imposition_pathway_kernel__endogenous_climb_reading, suppression_requirement, 60, 0.58).
narrative_ontology:measurement(ipk_endo_su_t70, imposition_pathway_kernel__endogenous_climb_reading, suppression_requirement, 70, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(imposition_pathway_kernel__endogenous_climb_reading, information_standard).
narrative_ontology:affects_constraint(imposition_pathway_kernel__endogenous_climb_reading, imposition_pathway_kernel__exogenous_override_reading).
narrative_ontology:affects_constraint(imposition_pathway_kernel__endogenous_climb_reading, imposition_pathway_kernel__hybrid_cascade_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'how states impose new commitments' decomposes into three structurally distinct mechanism-claims with different epsilon values, per the epsilon-invariance principle: the endogenous climb account (this file, epsilon 0.60 over the climb-template-as-operative-standard), the exogenous override account (a distinct decree-first mechanism cell), and the hybrid cascade account (override-initiated, climb-completed). The family is linked through affects_constraints in all three files. Currently the endogenous reading is upstream: its dominance in venues and curricula shapes the reception conditions of the sibling files, which is why the edges run from this file to both siblings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
