% ============================================================================
% CONSTRAINT STORY: quran_9_5_scope__contextual_defensive
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_quran_9_5_scope__contextual_defensive, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: quran_9_5_scope__contextual_defensive
 *   human_readable: Qur'an 9:5 Contextual-Defensive Reading: Treaty-Breach Response Framework
 *   domain: religious/legal/political
 *
 * SUMMARY:
 *   This constraint instantiates one reading of the contested Qur'anic kernel
 *   Verse 9:5 ('slay the polytheists wherever you find them'). The
 *   contextual-defensive reading interprets this verse as addressing specific
 *   7th-century Medinan circumstances: tribes that had violated treaties with
 *   the emerging Muslim polity. Under this reading, Verse 9:5 does not
 *   abrogate peaceful verses emphasizing coexistence; it does not establish
 *   universal offensive jihad; it authorizes defensive response to treaty
 *   violation only. This reading permits Islamic legal scholarship and
 *   Muslim-majority states to maintain scriptural fidelity while respecting
 *   treaty obligations, pluralism, and minority rights. The constraint's
 *   beneficiaries are pluralist scholars and integrationist states; its
 *   structure prevents the verse from being weaponized as a universal mandate
 *   while preserving its defensive function. The measurement series spans
 *   1,400 years (from 7th-century revelation to contemporary jurisprudence),
 *   tracking how the reading's extractiveness and suppression have evolved as
 *   Islamic scholarship encountered non-Muslim polities and international
 *   legal frameworks.
 *
 * KEY AGENTS:
 *   - pluralist_islamic_legal_scholars: organized actors producing and circulating this reading through fatwas, published exegeses, and educational curricula
 *   - integrationist_muslim_majority_states: institutional agenda-setters incorporating this reading into constitutional frameworks and treaty obligations
 *   - literalist_jurisprudential_traditionalists: excluded from the pluralist frame, maintaining abrogating_universal and universalist readings
 *   - non_muslim_treaty_partners: moderate-power beneficiaries whose treaties are protected by this reading's constraint on offensive authorization
 *   - progressive_synthesis_adherents: organized allies who affirm contextuality but diverge on whether the reading still binds or is superseded by ethical principles
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(quran_9_5_scope__contextual_defensive, 0.28).
domain_priors:suppression_score(quran_9_5_scope__contextual_defensive, 0.15).
domain_priors:theater_ratio(quran_9_5_scope__contextual_defensive, 0.12).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(quran_9_5_scope__contextual_defensive, extractiveness, 0.28).
narrative_ontology:constraint_metric(quran_9_5_scope__contextual_defensive, suppression_requirement, 0.15).
narrative_ontology:constraint_metric(quran_9_5_scope__contextual_defensive, theater_ratio, 0.12).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(quran_9_5_scope__contextual_defensive, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(quran_9_5_scope__contextual_defensive, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(quran_9_5_scope__contextual_defensive, rope).
narrative_ontology:human_readable(quran_9_5_scope__contextual_defensive, "Qur'an 9:5 Contextual-Defensive Reading: Treaty-Breach Response Framework").
narrative_ontology:topic_domain(quran_9_5_scope__contextual_defensive, "religious/legal/political").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(quran_9_5_scope__contextual_defensive, '30b0a805-3be5-4b08-aa83-600685d01d59').
narrative_ontology:cs_kernel_codification('30b0a805-3be5-4b08-aa83-600685d01d59', fixed_text).
narrative_ontology:cs_authority_grounding('30b0a805-3be5-4b08-aa83-600685d01d59', lineage).
narrative_ontology:cs_interpretation_layer_present('30b0a805-3be5-4b08-aa83-600685d01d59').
narrative_ontology:cs_reading_relation('30b0a805-3be5-4b08-aa83-600685d01d59', quran_9_5_scope__abrogating_universal, coexists_with).
narrative_ontology:cs_reading_relation('30b0a805-3be5-4b08-aa83-600685d01d59', quran_9_5_scope__progressive_synthesis, influences).
narrative_ontology:cs_axiom('30b0a805-3be5-4b08-aa83-600685d01d59', foundational, quranic_contextuality_binding).
narrative_ontology:cs_axiom_status(quranic_contextuality_binding, holdable).
narrative_ontology:cs_axiom_grounding('30b0a805-3be5-4b08-aa83-600685d01d59', quranic_contextuality_binding, deontological).
narrative_ontology:cs_axiom('30b0a805-3be5-4b08-aa83-600685d01d59', foundational, treaty_obligation_superseding).
narrative_ontology:cs_axiom_status(treaty_obligation_superseding, holdable).
narrative_ontology:cs_axiom_grounding('30b0a805-3be5-4b08-aa83-600685d01d59', treaty_obligation_superseding, deontological).
narrative_ontology:cs_axiom('30b0a805-3be5-4b08-aa83-600685d01d59', secondary, peace_verses_harmonization).
narrative_ontology:cs_axiom_status(peace_verses_harmonization, holdable).
narrative_ontology:cs_axiom_grounding('30b0a805-3be5-4b08-aa83-600685d01d59', peace_verses_harmonization, deontological).
narrative_ontology:cs_reference_frame('30b0a805-3be5-4b08-aa83-600685d01d59', contextual_hadith_exegesis_tradition).
narrative_ontology:cs_drift_state('30b0a805-3be5-4b08-aa83-600685d01d59', contemporary_pluralist_legal_systems, gap(authority_erosion, substantial, true)).
narrative_ontology:cs_created_at('30b0a805-3be5-4b08-aa83-600685d01d59', '').
narrative_ontology:cs_kernel_id(quran_9_5_scope__contextual_defensive, quran_9_5_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(quran_9_5_scope__contextual_defensive, integrationist_muslim_majority_states).
narrative_ontology:constraint_beneficiary(quran_9_5_scope__contextual_defensive, pluralist_islamic_legal_scholars).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(quran_9_5_scope__contextual_defensive, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(quran_9_5_scope__contextual_defensive, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(quran_9_5_scope__contextual_defensive_tests).
:- end_tests(quran_9_5_scope__contextual_defensive_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.28) because the constraint's primary function is coordinating hermeneutical practice, not material extraction. The constraint gives pluralist scholars and states a coherent framework to affirm both scripture and pluralism — a genuine collective-action problem solved (the coordination problem: how to be scripturally faithful AND legally pluralist?). No party bears an imposed cost; all named beneficiaries gain legitimacy. Suppression is minimal (0.15) because the constraint operates through persuasion, scholarship, and institutional adoption, not through force or institutional silencing (an omega documents the degree of actual suppression). Theater is very low (0.12): the constraint's function is its performed function — scholars genuinely engage in contextual analysis; states genuinely rely on this reading to justify treaty compliance. The measurement series shows slight growth in extractiveness over 1,400 years (0.05→0.28) reflecting: (1) as interaction with non-Muslim polities intensified, the need for the reading's legitimation grew, and (2) as state power grew, the reading acquired more institutional capture risk. But growth remains shallow because the reading's hermeneutical coherence is intrinsic, not engineered. The shared time grid ensures every metric is authored at every interval point; cyclical dynamics are minimal because this is legal-doctrinal evolution, not oscillating pressure.
 *
 * PERSPECTIVAL GAP:
 *   The pluralist scholar and the integrationist state would experience this constraint as coordinating — a shared hermeneutical framework enabling both to act faithfully and prudently. The literalist traditionalist would experience it as constraining — a framework that excludes their reading from institutional legitimacy. The non-Muslim treaty partner experiences it as protecting — a constraint that makes the Muslim-majority state's treaty obligations hermeneutically non-negotiable. The engine computes these divergences from the structural differences in power, exit, and role.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (pluralist scholars, integrationist states) have low directionality (0.1-0.25, gaining from the constraint without bearing its costs). Excluded literalists have no numerical directionality in this story (they are not a victim set, per R3; their exclusion is structural to this reading's coherence, not a suppressed group being extracted from). Non-Muslim treaty partners have moderate-positive directionality (0.3-0.4, receiving benefit but also bearing treaty obligations that the constraint makes non-negotiable). No directionality overrides are needed because the structural derivation (beneficiary/victim + exit → d) produces accurate values.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint shows no signs of mandatrophy. Its founding problem (integrating Verse 9:5 with peace verses while maintaining doctrinal integrity) remains live in contemporary Islamic scholarship. The constraint continues to solve a real coordination problem: scholars and states need hermeneutical legitimacy to practice pluralism, and this reading provides it. The performance cost (engagement with complex historical-contextual exegesis) is proportional to the coordination benefit (genuine coherence across scriptural fidelity and political pluralism). If the constraint were merely theatrical, we would see rising theater_ratio; instead, it remains near 0.12 because scholarship continues to engage substantively with the exegetical apparatus. Mandatrophy would manifest if pluralist states abandoned the reading while maintaining its institutional frameworks for legitimation theater — that has not occurred at scale. The constraint remains functionally tied to the problem it solves.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_vs_reading_identity,
    'Is this constraint a bounded READING of the Qur''anic kernel Verse 9:5, or is it a distinct constraint instantiating competing truth claims about what the verse means?',
    'If the three sibling readings (contextual_defensive, abrogating_universal, progressive_synthesis) coexist as live jurisprudential positions within Islamic scholarship, each reading constitutes a separate constraint story. If one reading becomes canonically settled and others fade to historical curiosities, the constraint becomes the settlement itself, not the reading.',
    'Classification depends on whether we treat this as a rope coordinating one of three live hermeneutical options (rope: coordination among scholars who adopt this reading), or as a mountain-adjacent natural result of philological analysis (would require reclassification). The kernel framing (one reading of three) is authored here; abandoning the kernel frame would revise the type claim significantly.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_vs_reading_identity, conceptual, 'Whether this constraint is a live reading of a contested kernel or a settled hermeneutical fact.').

omega_variable(
    contextuality_vs_eternality_boundary,
    'Does the contextual-defensive reading permit Verse 9:5 to retain ANY prescriptive force in contemporary Islamic law, or does it reduce the verse to historical narrative with no ongoing application?',
    'Examine how Islamic courts, fatwa bodies, and state legal systems invoking this reading apply 9:5: do they cite it as authorizing modern defensive military action (prescriptive), or do they treat it as historical record only (descriptive)? The answer determines whether the constraint permits ongoing function or has been absorbed into historical interpretation.',
    'If prescriptive force persists, the constraint coordinates interpretation while preserving the verse''s authority — genuine rope dynamics. If the reading reduces 9:5 to narrative, then the constraint is a hermeneutical tool for neutralizing difficult verses; extraction rises because beneficiaries gain a mechanism to nullify problematic text without explicitly saying so.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(contextuality_vs_eternality_boundary, empirical, 'Whether contextual-defensive reading preserves prescriptive force or historicizes the verse entirely.').

omega_variable(
    abrogation_doctrine_contestation,
    'Does the contextual-defensive reading invoke standard Islamic abrogation doctrine (nasikh/mansukh) to explain why 9:5 does not abrogate peace verses, or does it reject abrogation doctrine entirely in favor of hermeneutical contextualization?',
    'Examine jurisprudential literature: adherents of this reading either (a) use abrogation theory but argue 9:5 does not abrogate peace verses, or (b) reject literalist abrogation in favor of historical-contextual analysis. These are structurally distinct hermeneutical moves with different implications.',
    'If (a), the reading operates within traditional jurisprudential frameworks and is vulnerable to counter-arguments about abrogation from the abrogating_universal reading. If (b), the reading represents a methodological break (post-modern hermeneutics vs. classical fiqh) that separates it fundamentally from its siblings and shifts it toward progressive-synthesis territory.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(abrogation_doctrine_contestation, conceptual, 'Whether the reading invokes standard abrogation doctrine or rejects it for alternative hermeneutics.').

omega_variable(
    state_legitimation_instrumentalization,
    'Do integrationist Muslim-majority states adopt the contextual-defensive reading genuinely for its hermeneutical coherence, or instrumentally to provide theological cover for secular governance they would practice regardless?',
    'Compare states'' theological framing (official statements, judicial opinions, curriculum) with their actual legal systems and treaty behavior. If coherence exists: framework aligns with practice. If instrumental: the reading is a mask; actual law is secular regardless of theological framing.',
    'If genuine, beneficiaries truly benefit from resolving a coordination problem and the constraint operates as rope. If instrumental, the beneficiary gains plausible deniability while silencing literalist populations; suppression rises and the constraint becomes a snare-variant (coordinating appearance while extracting autonomy from those who take the theological reading seriously).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(state_legitimation_instrumentalization, empirical, 'Whether state adoption is genuine hermeneutical commitment or instrumental legitimation cover.').

omega_variable(
    literalist_suppression_mechanism,
    'To what degree do pluralist legal institutions (universities, fatwa bodies, state religious affairs) actively suppress, exclude, or delegitimize literalist-universalist readings, versus simply advocating for their own reading?',
    'Examine whether pluralist-dominant institutions formally exclude literalist scholars from participation, prevent publication of competing interpretations, use institutional power to enforce conformity, or fund research asymmetrically. High institutional suppression = snare dynamics; low suppression = rope dynamics.',
    'If suppression is low, the constraint coordinates interpretation without coercion — rope classification holds. If suppression is high, the constraint rides extraction machinery (institutional silencing of competitors); classification revises toward snare and suppression metric rises.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(literalist_suppression_mechanism, empirical, 'Whether pluralist dominance involves institutional suppression of literalist alternatives.').

omega_variable(
    identity_lock_fusion_binding,
    'For Islamic legal scholars adopting the contextual-defensive reading, is the reading fused with professional identity (career dependence on the reading''s legitimacy), ideological identity (worldview requires the reading to be coherent), or relational identity (community membership depends on affirming the reading)?',
    'Examine the cost to scholars of switching readings: (a) career cost (loss of position if reading changes), (b) worldview cost (changing reading requires adopting incoherent theology), (c) community cost (affirming alternative reading would mean leaving scholarly networks). The pattern reveals the identity-lock mechanism.',
    'High identity-lock (fused across career, ideology, community) reduces exit mobility artificially and can convert the constraint from rope to tangled-rope if the identity lock is maintained through institutional suppression. Lower identity-lock suggests genuine coordination without identity fusion.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_fusion_binding, empirical, 'Degree and mechanism of identity fusion binding scholars to this reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(quran_9_5_scope__contextual_defensive, 0, 1400).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qura_tr_t0, quran_9_5_scope__contextual_defensive, theater_ratio, 0, 0.08).
narrative_ontology:measurement(qura_tr_t200, quran_9_5_scope__contextual_defensive, theater_ratio, 200, 0.09).
narrative_ontology:measurement(qura_tr_t600, quran_9_5_scope__contextual_defensive, theater_ratio, 600, 0.1).
narrative_ontology:measurement(qura_tr_t1000, quran_9_5_scope__contextual_defensive, theater_ratio, 1000, 0.11).
narrative_ontology:measurement(qura_tr_t1200, quran_9_5_scope__contextual_defensive, theater_ratio, 1200, 0.12).
narrative_ontology:measurement(qura_tr_t1400, quran_9_5_scope__contextual_defensive, theater_ratio, 1400, 0.12).

% Extraction over time
narrative_ontology:measurement(qura_be_t0, quran_9_5_scope__contextual_defensive, base_extractiveness, 0, 0.05).
narrative_ontology:measurement(qura_be_t200, quran_9_5_scope__contextual_defensive, base_extractiveness, 200, 0.12).
narrative_ontology:measurement(qura_be_t600, quran_9_5_scope__contextual_defensive, base_extractiveness, 600, 0.18).
narrative_ontology:measurement(qura_be_t1000, quran_9_5_scope__contextual_defensive, base_extractiveness, 1000, 0.26).
narrative_ontology:measurement(qura_be_t1200, quran_9_5_scope__contextual_defensive, base_extractiveness, 1200, 0.27).
narrative_ontology:measurement(qura_be_t1400, quran_9_5_scope__contextual_defensive, base_extractiveness, 1400, 0.28).

% Suppression requirement over time
narrative_ontology:measurement(qura_su_t0, quran_9_5_scope__contextual_defensive, suppression_requirement, 0, 0.12).
narrative_ontology:measurement(qura_su_t200, quran_9_5_scope__contextual_defensive, suppression_requirement, 200, 0.13).
narrative_ontology:measurement(qura_su_t600, quran_9_5_scope__contextual_defensive, suppression_requirement, 600, 0.14).
narrative_ontology:measurement(qura_su_t1000, quran_9_5_scope__contextual_defensive, suppression_requirement, 1000, 0.15).
narrative_ontology:measurement(qura_su_t1200, quran_9_5_scope__contextual_defensive, suppression_requirement, 1200, 0.15).
narrative_ontology:measurement(qura_su_t1400, quran_9_5_scope__contextual_defensive, suppression_requirement, 1400, 0.15).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(quran_9_5_scope__contextual_defensive, identity_coordination).
narrative_ontology:boltzmann_floor_override(quran_9_5_scope__contextual_defensive, 0.12).
narrative_ontology:affects_constraint(quran_9_5_scope__contextual_defensive, quran_9_5_scope__abrogating_universal).
narrative_ontology:affects_constraint(quran_9_5_scope__contextual_defensive, quran_9_5_scope__progressive_synthesis).
narrative_ontology:affects_constraint(quran_9_5_scope__contextual_defensive, islamic_legal_pluralism_doctrine).
narrative_ontology:affects_constraint(quran_9_5_scope__contextual_defensive, muslim_state_treaty_obligation_framework).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the kernel quran_9_5_scope. Each reading is a separate constraint story because ε (extractiveness) differs substantially across readings: contextual_defensive has low extraction (0.28, coordination problem solved); abrogating_universal has moderate-to-high extraction (rents collected by authority claiming mandate); progressive_synthesis has zero extraction (verse historicized, no prescriptive force). All three readings are ε-invariant within themselves but not across each other — they are distinct constraints, not measurements of one constraint from different vantage points. They are linked via network.affects_constraints because each reading's institutional success affects the others' legitimacy landscape.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
