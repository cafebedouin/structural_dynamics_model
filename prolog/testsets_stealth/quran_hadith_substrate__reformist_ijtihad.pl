% ============================================================================
% CONSTRAINT STORY: quran_hadith_substrate__reformist_ijtihad
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-04
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_quran_hadith_substrate__reformist_ijtihad, []).

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
 *   constraint_id: quran_hadith_substrate__reformist_ijtihad
 *   human_readable: Reformist Ijtihad Mandate over the Quran-Hadith Substrate
 *   domain: religious/legal-theoretic
 *
 * SUMMARY:
 *   In jurisdictions and transnational communities where reformist readings
 *   of the Quran-hadith substrate hold institutional backing, interpretive
 *   authority operates under a mandated method: whenever an inherited
 *   classical ruling conflicts with contemporary ethical commitments,
 *   recognized human rights norms, or an assessed public interest, qualified
 *   interpreters are required to re-derive the ruling from the founding
 *   sources rather than defer to transmitted school positions, and the
 *   Quran's ethical trajectory is treated as the controlling layer of
 *   revelation, with report-based applications applied literally only where
 *   they do not obstruct that trajectory. The arrangement coordinates Muslim
 *   religious life under modern conditions: it supplies a single authorized
 *   hermeneutic so that individuals, courts, and legislatures need not
 *   improvise private resolutions to the tradition-modernity collision, and
 *   it credentializes who may perform the re-derivation. Through the same
 *   structure flows a transfer of interpretive authority: certification,
 *   court staffing, publication authority, and adjudication roles move from
 *   traditionally transmitted scholarly estates toward academically and
 *   state-backed interpreter institutions, while ethical burdens of
 *   exclusionary testimony, unequal divorce, and minority-status rulings are
 *   lifted from women, gender and sexual minorities, and non-Muslim citizens
 *   and shifted onto the interpretive tradition itself. This file authors
 *   that reading alone as a single epsilon-invariant constraint; the other
 *   readings of the same textual substrate are separate stories joined
 *   through the network section.
 *
 * KEY AGENTS:
 *   - - reformist_religious_institutions: Agenda-setter (institutional/constrained) — administers the mandate through credentialing, judicial training, and deliberation committees
 *   - - reformist_interpreter_class: Primary beneficiary (organized/mobile) — collects careers, citation authority, and adjudication roles the mandate creates
 *   - - progressive_muslim_communities: Beneficiary (moderate/constrained) — validated practice without exit from the faith
 *   - - muslim_women_reform_advocates: Beneficiary (organized/identity_locked) — reopened rulings on divorce, testimony, and leadership
 *   - - lgbtq_muslims: Beneficiary (powerless/identity_locked) — protected only while the ethical-trajectory principle holds
 *   - - non_muslim_citizens_of_islamic_law_states: Beneficiary (moderate/constrained) — equalized personal-status rulings
 *   - - traditional_ulema_establishment: Primary target (institutional/identity_locked) — loses binding force of transmitted rulings and the certification monopoly
 *   - - hadith_literalist_networks: Target (organized/identity_locked) — method subordinated to the Quranic ethical arc
 *   - - conservative_lay_believers: Dual-positioned (moderate/constrained) — inherited practice delegitimized yet consumes the flexibilities
 *   - - traditionally_trained_parish_imams: Excluded voice (moderate/trapped) — displaced by credential gates, absent from the councils
 *   - - academic_comparativists: Analytical observer (analytical/analytical) — documents the authority transfer from outside the confessional arena
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(quran_hadith_substrate__reformist_ijtihad, 0.44).
domain_priors:suppression_score(quran_hadith_substrate__reformist_ijtihad, 0.38).
domain_priors:theater_ratio(quran_hadith_substrate__reformist_ijtihad, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(quran_hadith_substrate__reformist_ijtihad, extractiveness, 0.44).
narrative_ontology:constraint_metric(quran_hadith_substrate__reformist_ijtihad, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(quran_hadith_substrate__reformist_ijtihad, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(quran_hadith_substrate__reformist_ijtihad, accessibility_collapse, 0.38).
narrative_ontology:constraint_metric(quran_hadith_substrate__reformist_ijtihad, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(quran_hadith_substrate__reformist_ijtihad, tangled_rope).
narrative_ontology:human_readable(quran_hadith_substrate__reformist_ijtihad, "Reformist Ijtihad Mandate over the Quran-Hadith Substrate").
narrative_ontology:topic_domain(quran_hadith_substrate__reformist_ijtihad, "religious/legal-theoretic").

domain_priors:requires_active_enforcement(quran_hadith_substrate__reformist_ijtihad).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(quran_hadith_substrate__reformist_ijtihad, '3d3b9582-c6c5-4151-8d98-f96b16b4880e').
narrative_ontology:cs_kernel_codification('3d3b9582-c6c5-4151-8d98-f96b16b4880e', fixed_text).
narrative_ontology:cs_authority_grounding('3d3b9582-c6c5-4151-8d98-f96b16b4880e', expertise).
narrative_ontology:cs_interpretation_layer_present('3d3b9582-c6c5-4151-8d98-f96b16b4880e').
narrative_ontology:cs_reading_relation('3d3b9582-c6c5-4151-8d98-f96b16b4880e', quran_hadith_substrate__traditionalist_taqlid, forecloses).
narrative_ontology:cs_reading_relation('3d3b9582-c6c5-4151-8d98-f96b16b4880e', quran_hadith_substrate__state_hybrid, influences).
narrative_ontology:cs_axiom('3d3b9582-c6c5-4151-8d98-f96b16b4880e', foundational, contextual_ijtihad_mandatory_on_conflict).
narrative_ontology:cs_axiom_status(contextual_ijtihad_mandatory_on_conflict, holdable).
narrative_ontology:cs_axiom_grounding('3d3b9582-c6c5-4151-8d98-f96b16b4880e', contextual_ijtihad_mandatory_on_conflict, instrumental).
narrative_ontology:cs_axiom('3d3b9582-c6c5-4151-8d98-f96b16b4880e', foundational, quranic_ethical_trajectory_supremacy).
narrative_ontology:cs_axiom_status(quranic_ethical_trajectory_supremacy, holdable).
narrative_ontology:cs_axiom_grounding('3d3b9582-c6c5-4151-8d98-f96b16b4880e', quranic_ethical_trajectory_supremacy, theological).
narrative_ontology:cs_reference_frame('3d3b9582-c6c5-4151-8d98-f96b16b4880e', ethical_trajectory_revelation).
narrative_ontology:cs_drift_state('3d3b9582-c6c5-4151-8d98-f96b16b4880e', contemporary_post_musawah_era, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('3d3b9582-c6c5-4151-8d98-f96b16b4880e', '').
narrative_ontology:cs_kernel_id(quran_hadith_substrate__reformist_ijtihad, quran_hadith_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(quran_hadith_substrate__reformist_ijtihad, progressive_muslim_communities).
narrative_ontology:constraint_beneficiary(quran_hadith_substrate__reformist_ijtihad, muslim_women_reform_advocates).
narrative_ontology:constraint_beneficiary(quran_hadith_substrate__reformist_ijtihad, lgbtq_muslims).
narrative_ontology:constraint_beneficiary(quran_hadith_substrate__reformist_ijtihad, non_muslim_citizens_of_islamic_law_states).
narrative_ontology:constraint_beneficiary(quran_hadith_substrate__reformist_ijtihad, reformist_interpreter_class).
narrative_ontology:constraint_victim(quran_hadith_substrate__reformist_ijtihad, traditional_ulema_establishment).
narrative_ontology:constraint_victim(quran_hadith_substrate__reformist_ijtihad, hadith_literalist_networks).
narrative_ontology:constraint_victim(quran_hadith_substrate__reformist_ijtihad, conservative_lay_believers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(quran_hadith_substrate__reformist_ijtihad, conservative_lay_believers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% State religious directorates, university faculties of the law, and transnational research councils that issue and administer the mandate: they certify who may re-derive rulings, train judges, staff deliberation committees, and publish the approved curricula. They enforce the method through credentialing and court appointment rather than sanction. Their role exists only while the mandate holds, they depend on state or academic patronage, and they absorb the counter-mobilization pressure the mandate generates.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__reformist_ijtihad, reformist_religious_institutions, agenda_setter,
    institutional, generational, constrained, regional).

% Academically trained scholars, feminist jurists, and reform-minded muftis who staff the councils and produce the readings. The mandate creates their careers, citation authority, and adjudication roles. Unlike the institutions they serve, they can move between universities, advocacy organizations, and state bodies, so their commitment to the arrangement is professional rather than existential.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__reformist_ijtihad, reformist_interpreter_class, beneficiary,
    organized, biographical, mobile, global).

% Lay Muslims who want to remain inside the faith while rejecting inherited exclusions. The mandate validates their practice without requiring exit into secularism or conversion, both of which carry heavy social and familial cost. They press for the mandate's expansion rather than leave, and their access to it depends on institutional backing they do not control.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__reformist_ijtihad, progressive_muslim_communities, beneficiary,
    moderate, biographical, constrained, global).

% Transnational advocacy networks that use the mandate to reopen rulings on divorce, testimony, religious leadership, and dress. Their faith identity is fused with the struggle: leaving the tradition would defeat the purpose of the work, so they are bound to the interpretive arena whatever it yields, and their gains persist only while the mandate holds against counter-reading.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__reformist_ijtihad, muslim_women_reform_advocates, beneficiary,
    organized, generational, identity_locked, global).

% Believers for whom the ethical-trajectory principle is the principal doctrinal route to inclusion. They hold almost no institutional power, cannot exit either the faith or their orientation, and their protection depends entirely on the mandate surviving literalist counter-application. They benefit without running anything and without leverage to defend the arrangement themselves.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__reformist_ijtihad, lgbtq_muslims, beneficiary,
    powerless, biographical, identity_locked, global).

% Religious minorities in jurisdictions applying Islamic personal-status law. Contextual re-derivation equalizes their position on testimony, marriage, and inheritance where classical terms disabled them. They benefit without administering the system; their alternative, emigration, is costly and only partial, since the disability follows diaspora communities informally.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__reformist_ijtihad, non_muslim_citizens_of_islamic_law_states, beneficiary,
    moderate, biographical, constrained, national).

% Madhhab-trained scholarly estates, endowed seminaries, and muftiates of the classical transmission lines. The mandate strips the rulings they spent lifetimes mastering of binding force and transfers certification to rival credentials. Their authority is constituted by the transmission chain itself, so adopting the rival method would dissolve the seat they occupy; they respond with counter-mobilization, fatwa wars, and institutional competition rather than adaptation.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__reformist_ijtihad, traditional_ulema_establishment, payer,
    institutional, civilizational, identity_locked, global).

% Transnational scripturalist movements whose epistemology ranks authenticated reports above contextual reasoning. The mandate's subordination of literal application to the Quranic ethical arc directly devalues their method, their publications, and their claim to speak for the tradition. Their identity is fused to the literalist frame; they fight the mandate rather than adopt it, and they are its most durable organized opposition.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__reformist_ijtihad, hadith_literalist_networks, payer,
    organized, generational, identity_locked, global).

% Pious laypeople formed under transmitted-practice norms whose observance is ruled ethically deficient in reformed jurisdictions. They bear the cost of having inherited practice delegitimized from within the tradition, yet they also consume the mandate's flexibilities when life requires them: easier dissolution of marriage, permissible financial instruments, workable minority status. Their position inside the arrangement is genuinely mixed.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__reformist_ijtihad, conservative_lay_believers, payer,
    moderate, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(quran_hadith_substrate__reformist_ijtihad, conservative_lay_believers, beneficiary).

% Mosque-level clergy trained in classical curricula who are displaced by the mandate's credential requirements but hold no seat on the councils that set those requirements. They would object that the arrangement dispossesses working clergy in favor of an academic class; their objection surfaces as sermon rhetoric and quiet noncompliance, never inside the process.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__reformist_ijtihad, traditionally_trained_parish_imams, excluded,
    moderate, biographical, trapped, national).

% Historians and sociologists of Islamic law who study the mandate's operation from outside the confessional arena. They document the transfer of interpretive authority, the credentialing shift, and the movement's institutional history without collecting from the arrangement or enforcing it, and their analyses are cited by every other seat when convenient.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__reformist_ijtihad, academic_comparativists, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(quran_hadith_substrate__reformist_ijtihad, reformist_religious_institutions).
narrative_ontology:fixing_cost_class(quran_hadith_substrate__reformist_ijtihad, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the tradition-modernity collision for communities that intend to remain inside the revelation framework: it supplies one authorized method, contextual re-derivation keyed to the law's higher objectives and assessed public interest, so that individuals, courts, and legislatures facing a conflict between an inherited ruling and contemporary conditions resolve it through a shared procedure instead of private improvisation, and it credentializes who may perform the re-derivation.
% TRANSFER_FUNCTION: Moves interpretive authority, meaning certification power, court staffing, publication authority, adjudication roles, and the income and status attached to them, from traditionally transmitted scholarly estates toward academically and state-backed interpreter institutions; simultaneously it moves ethical and legal burdens, such as exclusionary testimony rules, unequal divorce and inheritance terms, and minority-status disabilities, off women, gender and sexual minorities, and non-Muslim citizens and onto the interpretive tradition itself.
% ABSENT_VOICES: Traditionally trained parish imams displaced by the credential gates have no seat on the councils that set those gates; conservative laity in reformed jurisdictions experience the mandate as issued over their heads; ex-Muslim critics who regard the entire reform project as apologetic management stand wholly outside the confessional arena. Unanimity inside the councils therefore reflects the composition of the room, not the absence of dissent.
% DISAPPEARANCE_RATIONALE: If the mandate vanished overnight, reformed family codes would lose their doctrinal foundation and revert toward classical rulings or ad hoc state improvisation; progressive communities, women's advocacy networks, and LGBTQ Muslims would lose their principal protected channel and face renewed exclusion or exit; certification and court staffing would return to the traditional estates; and the underlying collision between inherited rulings and contemporary conditions would resurface unresolved, fragmenting into private judgments.
% FOUNDING_PROBLEM: By the nineteenth century the interpretive gate had closed: madhhab consensus and emulation norms froze rulings that assumed pre-modern social structures just as Muslim societies encountered constitutional equality, mass education, women's public participation, and non-Muslim citizenship, producing a collision between binding transmitted law and lived conditions that ordinary deference could not resolve.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: the traditional scholarly estates themselves attest the collision, though their prescribed remedy is continued emulation rather than re-derivation; state legislatures across Muslim-majority jurisdictions attest it by enacting reformed personal-status codes; and academic historians of Islamic law document the closure of independent reasoning and its modern consequences from outside the movement entirely.
narrative_ontology:disappearance_verdict(quran_hadith_substrate__reformist_ijtihad, world_rearranges).
narrative_ontology:founding_problem_status(quran_hadith_substrate__reformist_ijtihad, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(quran_hadith_substrate__reformist_ijtihad, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth+rescue1', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(quran_hadith_substrate__reformist_ijtihad, 'none', 1).
narrative_ontology:epsilon_provenance(quran_hadith_substrate__reformist_ijtihad, 0.44, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(quran_hadith_substrate__reformist_ijtihad_tests).
:- end_tests(quran_hadith_substrate__reformist_ijtihad_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction sits at 0.44 because the mandate's costs are real but bounded: traditional seats lose binding force and certification revenue, conservative laity bear delegitimation of inherited practice, and the trigger criterion is defined by the winning interpreter class, which lets self-serving re-derivations through; against this, the mandate lifts severe burdens off its beneficiaries and solves a coordination problem no seat denies exists. Suppression is comparatively low (0.38) for a religious-authority arrangement: the mandate suppresses literalist application and emulation-based deference within its jurisdiction but coexists with explicit pluralism claims, and its enforcement runs through credentialing and court appointment rather than sanction. Theater rises steadily (0.12 to 0.38) as the movement institutionalizes: early reform produced direct statutory change, while the contemporary scene layers conferences, declarations, and symbolic appointments over a still-real functional core. All three series share one time grid (points 0, 20, 40, 65, 90, 110, 130). The trajectories are deliberately non-monotonic: extraction peaked during the colonial-era displacement of the classical courts, bottomed in the post-independence settlement when benefits were broadest, and re-concentrated as credentialism thickened; the suppression_requirement series traces enforcement-capacity buildup through the codification era, decay under Islamist resurgence, and partial stabilization, which is the sanctioned use of that series since this story tracks enforcement machinery rather than merely shifting extraction. Base properties are measured at the interval endpoint, the settled moderate phase, not at the peak-displacement phase.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute differently. From the traditional scholarly estate's position the mandate is dispossession enforced by rival credentials: identical texts, opposite verdicts, and no exit that does not dissolve the seat itself, since the estate's authority is constituted by the transmission chain, making the identity fusion institutional rather than preferential. From the beneficiary seats the same structure is liberation with a price: protection contingent on institutional backing they do not control, and for the women's networks and LGBTQ believers the fusion is relational and ideological, binding them to an arena that may turn on them. The agenda-setter seat experiences neither pure benefit nor pure cost but existential dependency: its role exists only while the mandate holds, and it absorbs the counter-mobilization risk. Conservative lay believers straddle: their derived directionality from the victim declaration alone would sit near the full-target end, but they concurrently consume the mandate's flexibilities, so their true position is mid-range; correcting this would need a per-agent override, and the available override surface keys on power atoms shared with other seats (progressive communities are also moderate), so no override is authored and the residual imprecision is documented here instead.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive low directionality for the five beneficiary seats: progressive communities, women's advocacy networks, LGBTQ Muslims, and non-Muslim citizens receive validated status without running the machinery, and the interpreter class additionally collects the careers and adjudication roles the mandate creates. Victim declarations drive high directionality for the scholarly estate and the literalist networks, amplified toward the full-target end by identity-locked exit, since neither can adopt the rival epistemology without dissolving the identity that constitutes it. The agenda-setting institutions derive near-beneficiary directionality from their collector position, tempered by their enforcement burden and exposure. Spatial scope is global for the transnational seats and national for jurisdiction-bound ones; wider scope raises verification difficulty and thereby modestly amplifies effective extraction on targets, which matters most for the scholarly estate operating across many jurisdictions. Suppression, by contrast, is authored as a raw structural property and is not scaled by power or scope.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem, a closed interpretive gate colliding with post-colonial constitutional equality and changed social conditions, remains live, so no mandatrophy is declared and the mismatch consumer finds a live status paired with a world-rearranges verdict, producing no zombie flag. The classification guards against two symmetric mislabels: reading the mandate as pure coordination erases the authority transfer taken from traditional seats through the same structure; reading it as pure extraction erases the genuine coordination function and the absence of coerced participation among its beneficiaries, whose involvement is sought rather than compelled. The rising theater ratio is the drift signal to watch: if conferences and declarations fully replace statutory output while credential gates tighten, the arrangement slides toward inertial performance maintained by the interpreter class's self-interest, but the current functional core, meaning live court doctrines and codified family law resting on the method, keeps that outcome distant.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_governance_contestation,
    'This story instantiates one reading (reformist_ijtihad) of the quran_hadith_substrate kernel; which reading actually governs a given jurisdiction or practice community at a given time?',
    'Track institutional adjudication signals: which method courts cite, who certifies interpreters, which curricula the seminaries teach; governance flips are observable in appointment and codification records.',
    'If the traditionalist reading regains governance, this constraint''s beneficiary seats become its casualties and the extraction profile recomputes under the sibling story; if the state-hybrid reading governs, the mandate survives only where sovereignty finds it useful.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_governance_contestation, conceptual, 'Committer-frame routing: one of three readings of a shared textual kernel; the disagreement is located in where binding interpretive authority attaches.').

omega_variable(
    epsilon_regime_dependence,
    'Is the moderate extraction level a stable property of the reading or an artifact of the current balance of institutional backing, given the reading''s documented vulnerability to traditionalist counter-mobilization?',
    'Longitudinal comparison of extraction proxies across regimes of backing: the authored suppression_requirement series already shows enforcement capacity falling during the resurgence window; pair it with beneficiary-outcome data across the same windows.',
    'Under traditionalist resurgence the reading''s own extraction collapses because little is left to extract with, while its beneficiaries'' exposure to the sibling regime''s costs rises; the same structure would classify differently across the regime boundary.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(epsilon_regime_dependence, empirical, 'Whether measured extraction is regime-dependent rather than intrinsic to the reading.').

omega_variable(
    interpreter_class_capture,
    'Does the settlement still serve its declared beneficiaries, or has the credentialed interpreter class captured the surplus the mandate distributes?',
    'Outcome audits correlating council rulings and appointments with beneficiary interests versus institutional self-maintenance; track whether the rising theater ratio reflects declining statutory output.',
    'If captured, effective extraction rises above the authored band and the arrangement drifts toward the extractive end of the hybrid range despite unchanged doctrine.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(interpreter_class_capture, empirical, 'Benefit delivery versus interpreter-class self-maintenance.').

omega_variable(
    maslaha_trigger_indeterminacy,
    'Who defines the mandate''s trigger criteria, namely contemporary ethics, human rights norms, and public interest, and does the definition track the beneficiaries those criteria name or the interpreter class that wields them?',
    'Comparative analysis of how distinct reformist bodies operationalize the trigger: convergence on beneficiary-serving definitions indicates a genuine criterion; divergence tracking institutional interest indicates self-authorization.',
    'Self-authored triggers convert the mandate''s safety valve into the interpreter class''s license, raising effective extraction without any doctrinal change.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(maslaha_trigger_indeterminacy, conceptual, 'Indeterminacy of the constraint''s own activation criterion.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(quran_hadith_substrate__reformist_ijtihad, 0, 130).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qura_tr_t0, quran_hadith_substrate__reformist_ijtihad, theater_ratio, 0, 0.12).
narrative_ontology:measurement(qura_tr_t20, quran_hadith_substrate__reformist_ijtihad, theater_ratio, 20, 0.17).
narrative_ontology:measurement(qura_tr_t40, quran_hadith_substrate__reformist_ijtihad, theater_ratio, 40, 0.22).
narrative_ontology:measurement(qura_tr_t65, quran_hadith_substrate__reformist_ijtihad, theater_ratio, 65, 0.27).
narrative_ontology:measurement(qura_tr_t90, quran_hadith_substrate__reformist_ijtihad, theater_ratio, 90, 0.31).
narrative_ontology:measurement(qura_tr_t110, quran_hadith_substrate__reformist_ijtihad, theater_ratio, 110, 0.35).
narrative_ontology:measurement(qura_tr_t130, quran_hadith_substrate__reformist_ijtihad, theater_ratio, 130, 0.38).

% Extraction over time
narrative_ontology:measurement(qura_be_t0, quran_hadith_substrate__reformist_ijtihad, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(qura_be_t20, quran_hadith_substrate__reformist_ijtihad, base_extractiveness, 20, 0.47).
narrative_ontology:measurement(qura_be_t40, quran_hadith_substrate__reformist_ijtihad, base_extractiveness, 40, 0.42).
narrative_ontology:measurement(qura_be_t65, quran_hadith_substrate__reformist_ijtihad, base_extractiveness, 65, 0.38).
narrative_ontology:measurement(qura_be_t90, quran_hadith_substrate__reformist_ijtihad, base_extractiveness, 90, 0.43).
narrative_ontology:measurement(qura_be_t110, quran_hadith_substrate__reformist_ijtihad, base_extractiveness, 110, 0.45).
narrative_ontology:measurement(qura_be_t130, quran_hadith_substrate__reformist_ijtihad, base_extractiveness, 130, 0.44).

% Suppression requirement over time
narrative_ontology:measurement(qura_su_t0, quran_hadith_substrate__reformist_ijtihad, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(qura_su_t20, quran_hadith_substrate__reformist_ijtihad, suppression_requirement, 20, 0.38).
narrative_ontology:measurement(qura_su_t40, quran_hadith_substrate__reformist_ijtihad, suppression_requirement, 40, 0.52).
narrative_ontology:measurement(qura_su_t65, quran_hadith_substrate__reformist_ijtihad, suppression_requirement, 65, 0.58).
narrative_ontology:measurement(qura_su_t90, quran_hadith_substrate__reformist_ijtihad, suppression_requirement, 90, 0.44).
narrative_ontology:measurement(qura_su_t110, quran_hadith_substrate__reformist_ijtihad, suppression_requirement, 110, 0.4).
narrative_ontology:measurement(qura_su_t130, quran_hadith_substrate__reformist_ijtihad, suppression_requirement, 130, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(quran_hadith_substrate__reformist_ijtihad, identity_coordination).
narrative_ontology:affects_constraint(quran_hadith_substrate__reformist_ijtihad, traditionalist_taqlid).
narrative_ontology:affects_constraint(quran_hadith_substrate__reformist_ijtihad, state_hybrid).

% DUAL FORMULATION NOTE:
% The colloquial label 'Islamic legal authority' covers three structurally distinct constraints that share one textual kernel: the reformist mandate authored here, the traditionalist emulation regime, and the state-selective hybrid. Their extraction profiles differ because their victim sets and enforcement bases differ: this reading extracts interpretive authority from traditional estates while lifting burdens from marginalized believers; the traditionalist reading extracts interpretive freedom from the laity and dissenting scholars; the hybrid extracts selectively by domain. Per the epsilon-invariance principle the label is decomposed into three files, each with a single stable epsilon, linked through affects_constraints; the upstream/downstream pressure between them runs through credentialing and codification records rather than through any logical resolution of the dispute.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
