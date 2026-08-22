% ============================================================================
% CONSTRAINT STORY: shinbutsu_ontological_substrate__incoherent_bundle_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_shinbutsu_ontological_substrate__incoherent_bundle_reading, []).

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
 *   constraint_id: shinbutsu_ontological_substrate__incoherent_bundle_reading
 *   human_readable: Shinbutsu Syncretism as Incoherent State Enforcement Bundle
 *   domain: religious/political/institutional
 *
 * SUMMARY:
 *   This constraint instantiates one reading of the shinbutsu (kami-buddha)
 *   relationship in Tokugawa and Meiji Japan. The incoherent-bundle reading
 *   rejects the claim that syncretism expresses a metaphysical truth (the
 *   syncretic-fusion reading) or that kami and buddhas naturally govern
 *   separate domains (the domain-partition reading). Instead, it asserts that
 *   syncretism is accumulated institutional drift maintained by state
 *   enforcement: no coherent kernel exists; the state benefits from keeping
 *   both priesthoods dependent on state arbitration to navigate theological
 *   contradictions; practitioners are forced to hold contradictory beliefs
 *   without resolution. The constraint classifies as snare because the
 *   state's interest in maintaining religious incoherence exceeds any genuine
 *   coordination benefit, and the suppression of heterodox alternatives
 *   (which would provide coherent theological options) is the enforcement
 *   machinery's core function.
 *
 * KEY AGENTS:
 *   - Tokugawa state: enforces syncretism as settled doctrine; collects religious authority and priesthood dependence
 *   - Shinto priests: forced to present kami as Buddhist manifestations; suppressed from asserting kami autonomy
 *   - Buddhist monks: forced to accommodate kami within Buddhist cosmology; suppressed from Buddhist theological coherence
 *   - Lay practitioners: navigate contradictions through ritual habit, without transparent resolution
 *   - Heterodox sects: excluded from institutional recognition; would provide doctrinal alternatives
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(shinbutsu_ontological_substrate__incoherent_bundle_reading, 0.68).
domain_priors:suppression_score(shinbutsu_ontological_substrate__incoherent_bundle_reading, 0.71).
domain_priors:theater_ratio(shinbutsu_ontological_substrate__incoherent_bundle_reading, 0.62).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(shinbutsu_ontological_substrate__incoherent_bundle_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(shinbutsu_ontological_substrate__incoherent_bundle_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(shinbutsu_ontological_substrate__incoherent_bundle_reading, theater_ratio, 0.62).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(shinbutsu_ontological_substrate__incoherent_bundle_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(shinbutsu_ontological_substrate__incoherent_bundle_reading, resistance, 0.54).

% --- Constraint claim ---
narrative_ontology:constraint_claim(shinbutsu_ontological_substrate__incoherent_bundle_reading, snare).
narrative_ontology:human_readable(shinbutsu_ontological_substrate__incoherent_bundle_reading, "Shinbutsu Syncretism as Incoherent State Enforcement Bundle").
narrative_ontology:topic_domain(shinbutsu_ontological_substrate__incoherent_bundle_reading, "religious/political/institutional").

domain_priors:requires_active_enforcement(shinbutsu_ontological_substrate__incoherent_bundle_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(shinbutsu_ontological_substrate__incoherent_bundle_reading, '08dd85cd-9685-4aab-a880-45e02c3b3b54').
narrative_ontology:cs_kernel_codification('08dd85cd-9685-4aab-a880-45e02c3b3b54', fixed_text).
narrative_ontology:cs_authority_grounding('08dd85cd-9685-4aab-a880-45e02c3b3b54', extraction).
narrative_ontology:cs_interpretation_layer_present('08dd85cd-9685-4aab-a880-45e02c3b3b54').
narrative_ontology:cs_reading_relation('08dd85cd-9685-4aab-a880-45e02c3b3b54', shinbutsu_ontological_substrate__syncretic_fusion_reading, forecloses).
narrative_ontology:cs_reading_relation('08dd85cd-9685-4aab-a880-45e02c3b3b54', shinbutsu_ontological_substrate__domain_partition_reading, coexists_with).
narrative_ontology:cs_axiom('08dd85cd-9685-4aab-a880-45e02c3b3b54', foundational, syncretism_is_institutional_arrangement_not_metaphysical_truth).
narrative_ontology:cs_axiom_status(syncretism_is_institutional_arrangement_not_metaphysical_truth, holdable).
narrative_ontology:cs_axiom_grounding('08dd85cd-9685-4aab-a880-45e02c3b3b54', syncretism_is_institutional_arrangement_not_metaphysical_truth, empirically_contingent).
narrative_ontology:cs_axiom('08dd85cd-9685-4aab-a880-45e02c3b3b54', foundational, state_benefits_from_enforced_incoherence).
narrative_ontology:cs_axiom_status(state_benefits_from_enforced_incoherence, holdable).
narrative_ontology:cs_axiom_grounding('08dd85cd-9685-4aab-a880-45e02c3b3b54', state_benefits_from_enforced_incoherence, instrumental).
narrative_ontology:cs_reference_frame('08dd85cd-9685-4aab-a880-45e02c3b3b54', coherent_religious_competition_early_tokugawa).
narrative_ontology:cs_drift_state('08dd85cd-9685-4aab-a880-45e02c3b3b54', late_edo_meiji_transition, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('08dd85cd-9685-4aab-a880-45e02c3b3b54', '').
narrative_ontology:cs_kernel_id(shinbutsu_ontological_substrate__incoherent_bundle_reading, shinbutsu_ontological_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(shinbutsu_ontological_substrate__incoherent_bundle_reading, tokugawa_state).
narrative_ontology:constraint_victim(shinbutsu_ontological_substrate__incoherent_bundle_reading, shinto_priests).
narrative_ontology:constraint_victim(shinbutsu_ontological_substrate__incoherent_bundle_reading, buddhist_monks).
narrative_ontology:constraint_victim(shinbutsu_ontological_substrate__incoherent_bundle_reading, lay_practitioners).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(shinbutsu_ontological_substrate__incoherent_bundle_reading, shinto_priests).
narrative_ontology:constraint_beneficiary(shinbutsu_ontological_substrate__incoherent_bundle_reading, buddhist_monks).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Enforces unified shinbutsu syncretism as official doctrine to consolidate religious authority under state control. Does not resolve the underlying theological contradictions; rather, enforces their coexistence as settled fact. Collects legitimacy from religious unification while suppressing both sectarian competition and coherent theological alternatives. The state benefits from a fragmented, mutually-delegitimizing priesthood that depends on state arbitration to navigate contradictions.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_substrate__incoherent_bundle_reading, tokugawa_state, agenda_setter,
    institutional, generational, arbitrage, national).

% Must present kami as manifestations of buddhas (honji suijaku) or subordinate to them, contrary to their own theological understanding of kami as autonomous spiritual beings. They gain state-sanctioned legitimacy and shrine revenue from syncretism, but lose independent doctrinal authority and are forced to interpret their own tradition through Buddhist conceptual frameworks they do not endorse. Exit via assertion of kami autonomy means loss of shrine revenue and state favor.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_substrate__incoherent_bundle_reading, shinto_priests, payer,
    organized, biographical, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(shinbutsu_ontological_substrate__incoherent_bundle_reading, shinto_priests, beneficiary).

% Must accommodate kami as Buddhist deities or cosmic principles rather than as alien spiritual beings, contrary to Buddhist cosmological claims. They gain revenue and state protection from syncretism, but cannot maintain theological coherence in their own scriptural frameworks. Exit via rejection of syncretism means loss of temple patronage and state licensing authority over Buddhist institutions.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_substrate__incoherent_bundle_reading, buddhist_monks, payer,
    organized, biographical, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(shinbutsu_ontological_substrate__incoherent_bundle_reading, buddhist_monks, beneficiary).

% Encounter shinto shrines and Buddhist temples presenting contradictory theological claims about kami-buddha relationships, presented as unified doctrine. No authoritative resolution exists; practitioners navigate by ritual habit and local custom rather than coherent belief. Cannot exit the religious system without social and economic sanction (marriage, funerary rites, community standing all depend on religious participation). The incoherence is not transparent—it is hidden beneath ritual practice and institutional authority.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_substrate__incoherent_bundle_reading, lay_practitioners, payer,
    powerless, biographical, constrained, national).

% Offer coherent but unsanctioned alternatives (Pure Land Buddhism unmoored from syncretism, Shinto nativism rejecting Buddhist cosmology) and are actively suppressed by state enforcement machinery. Would introduce theological clarity and doctrinal competition if admitted; their exclusion is essential to maintaining the incoherent bundle.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_substrate__incoherent_bundle_reading, heterodox_sects, excluded,
    moderate, biographical, trapped, national).

% Documents the contradiction between the state's claim that syncretism expresses metaphysical truth and the institutional machinery that maintains it without resolution. Can trace how enforcement suppresses both theological alternatives and the recognition of contradiction itself.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_substrate__incoherent_bundle_reading, analytical_observer, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(shinbutsu_ontological_substrate__incoherent_bundle_reading, tokugawa_state).
narrative_ontology:fixing_cost_class(shinbutsu_ontological_substrate__incoherent_bundle_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: In the incoherent-bundle reading, there is no genuine coordination function. The state presents syncretism as solving the coordination problem of integrating indigenous and imported religions—but it solves this only by suppressing the recognition that no coherent solution exists. The apparent coordination is theatrical.
% TRANSFER_FUNCTION: Moves religious authority, priestly prestige, and temple revenue flows from competing independent priesthoods to a unified, state-arbitrated system. The priesthoods receive revenue and state legitimacy in exchange for doctrinal suppression—they must present contradictory beliefs as unified truth. Lay practitioners pay in cognitive dissonance and religious incoherence. The state collects political and religious authority.
% ABSENT_VOICES: Heterodox sects (Pure Land purists, Shinto nativists) are structurally excluded from the conversation. They would articulate either a coherent Buddhist position independent of kami or a coherent Shinto position independent of Buddhist cosmology, thereby making the incoherence visible. Their suppression is what maintains the syncretism as apparently settled doctrine.
% DISAPPEARANCE_RATIONALE: If syncretism enforcement vanished, the unified priesthood would fracture immediately along theological lines. Buddhist temples would either commit to Buddhist cosmology (marginalizing kami) or split into syncretic and purist branches. Shinto shrines would either assert kami autonomy (rejecting honji suijaku) or acknowledge themselves as Buddhist institutions. The religious field would reorganize rapidly around coherent doctrinal positions, and multiple priesthoods would compete for authority. The state would lose the ability to arbitrate contradictions and claim unified religious sanction.
% FOUNDING_PROBLEM: Early Tokugawa consolidation faced competing claims from Shinto and Buddhist institutions, each presenting itself as the legitimate framework for understanding Japanese religion. The state did not resolve this competition by choosing one tradition or by constructing a coherent synthesis. Instead, it enforced coexistence: both traditions would be simultaneously true, with honji suijaku (original essence, manifest traces) as the official doctrine claiming Buddhist superiority while accommodating kami worship.
% FOUNDING_PROBLEM_CORROBORATION: Modern scholarship (Kuroda Toshio, Grapard, Sakurai Tokutarō) documents that the founding 'problem' of religious competition was effectively solved by the Edo period through state enforcement and priesthood professionalization—the syncretism was no longer solving anything, but persisting through institutional inertia. Contemporary Shinto nativists (Hirata Atsutane) explicitly attest the founding problem is obsolete and syncretism is pure extraction. Buddhist modernizers also attest that their theology is contradicted by forced accommodation of kami. Only state authorities attest the founding problem remains live—their attestation is the constraint's own self-maintenance, not independent corroboration.
narrative_ontology:disappearance_verdict(shinbutsu_ontological_substrate__incoherent_bundle_reading, world_rearranges).
narrative_ontology:founding_problem_status(shinbutsu_ontological_substrate__incoherent_bundle_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(shinbutsu_ontological_substrate__incoherent_bundle_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(shinbutsu_ontological_substrate__incoherent_bundle_reading, 'none', 1).
narrative_ontology:epsilon_provenance(shinbutsu_ontological_substrate__incoherent_bundle_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(shinbutsu_ontological_substrate__incoherent_bundle_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(shinbutsu_ontological_substrate__incoherent_bundle_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(shinbutsu_ontological_substrate__incoherent_bundle_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises from 0.45 to 0.68 over the interval because the state's enforcement capacity strengthens and the priesthoods become more dependent on state licensing and revenue allocation—syncretism becomes less negotiable and more clearly extractive as institutional alternatives close off. Theater ratio rises from 0.30 to 0.62 because the maintenance function becomes increasingly performative: by the late Edo and Meiji periods, the state maintains syncretism through ceremonial assertion (imperial shinto-buddhism rituals, official doctrine) rather than through genuine theological synthesis or participant buy-in. The founding problem (religious competition between Shinto and Buddhism) is dead by t=150 (the problem was solved by priesthood professionalization and state licensing), yet suppression actually increases at t=150–200 (when nativist scholars began explicitly challenging syncretism as incoherent), suggesting the state is suppressing the recognition of obsolescence rather than defending a live coordination function. Theater ratio then stabilizes at 0.62, indicating a piton-like state where mostly performative maintenance and suppression of heterodox alternatives sustain the constraint. Suppression plateaus at t=150, reflecting the enforcement infrastructure's maturation.
 *
 * PERSPECTIVAL GAP:
 *   The state (agenda-setter seat) experiences syncretism as a coordination solution it designed and maintains for unified religious authority. The priesthoods (payer seats) experience syncretism as forced incoherence that requires state arbitration to navigate: kami priests must suppress their own theology to be licensed; Buddhist monks must expand their cosmology to accommodate beings their tradition treats as alien or subordinate. Lay practitioners (powerless payer seats) experience syncretism as authoritative contradiction—no way to know which theological framework is true, so they rely on ritual habit and local priesthood practice rather than coherent belief. The engine should compute these divergences automatically from the structural data: the state's d approaches beneficiary end (it collects authority without bearing costs); the priesthoods' d sits near target end (they bear suppression costs and identity constraint); lay practitioners' d sits even further toward target (trapped, no exit). The domain-partition reading and syncretic-fusion reading would compute differently at the same seats because they assign different causal and metaphysical structures to the relationship between kami and buddhas.
 *
 * DIRECTIONALITY LOGIC:
 *   The state is the structural beneficiary: it consolidates religious authority, makes both priesthoods dependent on state licensing and arbitration, and suppresses alternatives that would challenge its control. Shinto priests are targets: they must suppress their own theology and are paid through state-controlled temple revenues. Buddhist monks are targets: they must expand their cosmology and are paid through state-controlled temple networks. Lay practitioners are targets: they navigate contradictions without resolution or exit. Heterodox sects are excluded entirely from the institutional conversation—their potential authority is suppressed before they can compete. The suppression is structural: state control of temple licensing, priesthood ordination, and ritual authorization makes exit impossible without loss of livelihood and social standing.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint exhibits mandatrophy (the founding problem is dead). By t=100–150, the founding problem of religious competition was solved through state institutional design and priesthood professionalization—multiple priesthoods no longer competed for authority; they were licensed and revenue-allocated by the state. Yet the syncretism constraint persists and even intensifies (suppression rises, theater rises), indicating the constraint is no longer solving the problem it was built to solve. The state maintains syncretism after its coordination function is exhausted because the constraint's incoherence makes both priesthoods dependent on state arbitration. The state benefits from suppressing the recognition that the founding problem is obsolete—if priesthoods could compete coherently, the state would lose its arbitrating power. This is the characteristic piton signature: the primary function (solving religious competition) has atrophied, but the constraint persists through enforcement (suppression of heterodox alternatives, state-controlled temple networks) and performance (ceremonial affirmation of syncretism as unified doctrine). The incoherent-bundle reading makes the mandatrophy visible by rejecting the claim that syncretism is metaphysical truth—if it is merely accumulated institutional drift, then the persistence without function is exactly what we should expect.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    ontological_status_vs_institutional_arrangement,
    'Is syncretism a genuine metaphysical claim about the nature of kami and buddhas (syncretic-fusion axiom) or a pure institutional arrangement enforced by the state (incoherent-bundle reading)?',
    'Examine independent theological writings by priesthoods when state enforcement relaxes (e.g., Edo-period Buddhist and Shinto scholars in private writings; post-Meiji Restoration when the state rescinded syncretism enforcement). If priesthoods immediately assert incompatible metaphysical claims, the coherent alternative exists and was suppressed by state enforcement.',
    'If coherent alternatives emerge under relaxed enforcement, syncretism is purely institutional (incoherent-bundle reading is correct). If priesthoods voluntarily maintain syncretism even when state enforcement lifts, syncretism may express genuine theological commitment (syncretic-fusion or domain-partition readings become more plausible).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(ontological_status_vs_institutional_arrangement, empirical, 'Whether syncretism is metaphysical truth or enforced institutional arrangement').

omega_variable(
    coherence_vs_incoherence_framing,
    'Is the incoherent-bundle reading''s claim that ''no coherent kernel exists'' itself coherent? Or does the claim depend on an implicit framework (e.g., Western metaphysical coherence standards) that the syncretic-fusion reading rejects?',
    'Examine whether Japanese-tradition scholars (Kuroda, Tamura, Sakurai) who document contradiction are applying universal coherence standards or Buddhist/Shinto-internal standards. If the standards are universal, the incoherence is real. If the standards are imported from Western philosophy, the incoherence may be an artifact of the analytical framework.',
    'If incoherence is universal, the incoherent-bundle reading stands as an alternative to the syncretic-fusion reading. If incoherence is frame-dependent, the readings may be conceptually rather than structurally distinct—both could be true under different analytical frameworks, making them conceptual competitors rather than empirically distinct constraints.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coherence_vs_incoherence_framing, conceptual, 'Whether syncretism is incoherent in absolute terms or relative to an analytical framework').

omega_variable(
    priesthood_identity_lock_mechanism,
    'Is the suppression of heterodox alternatives (Pure Land, Shinto nativism) structural (economic exclusion, legal prohibition) or internalized (priesthoods themselves come to believe syncretism is true)?',
    'Compare priesthood private writings (diaries, internal debates) with public assertions. If private writings show skepticism about syncretism while public assertions affirm it, suppression is partly structural. If private writings also affirm syncretism, identity-lock may be internalized.',
    'If suppression is structural, it can be lifted by state policy change. If suppression is internalized, priesthoods would resist heterodox alternatives even without state enforcement, making the constraint more stable and harder to dismantle.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(priesthood_identity_lock_mechanism, empirical, 'Whether priestly identity-lock to syncretism is structural or internalized').

omega_variable(
    sibling_reading_foreclosure_ambiguity,
    'Does the incoherent-bundle reading''s rejection of coherent kernels logically foreclose the syncretic-fusion reading, or can both readings coexist by assigning different meaning to ''coherence''?',
    'Formalize the syncretic-fusion reading''s claim about honji suijaku (what does ''ontological unity'' mean?) and the incoherent-bundle reading''s claim about incoherence (by what standard?). If the terms are incommensurable, the readings may coexist (different conceptual frameworks). If the terms are commensurable and contradictory, the incoherent-bundle reading forecloses syncretic-fusion.',
    'If readings foreclose each other, they cannot be simultaneously held by different parties in the same institutional framework. If readings coexist via conceptual incommensurability, both can be true under different analytical standards, and the three readings form a conceptual triplet rather than a forced-choice set.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sibling_reading_foreclosure_ambiguity, conceptual, 'Whether the incoherent-bundle and syncretic-fusion readings logically foreclose each other or coexist via incommensurable frameworks').

omega_variable(
    meiji_rescission_and_resurrection,
    'The Meiji state rescinded syncretism enforcement (shinbutsu bunri, kami-buddha separation, 1868–1872), creating a natural experiment in constraint relaxation. Why did the state resurrect enforcement (state shinto, imperial syncretism rituals by 1890s) if the founding problem was already solved?',
    'Examine Meiji political documents and imperial ceremony records. If the resurrection targeted new political problems (imperial legitimacy, national consolidation, anti-Christian sentiment), syncretism is instrumentally useful regardless of its metaphysical status. If the resurrection targeted the same religious competition, the founding problem may have been less solved than we thought.',
    'If resurrection targeted new problems, syncretism in its new form is a different constraint (different ε, different beneficiaries). If resurrection targeted the same old problem, the incoherent-bundle reading''s claim about mandatrophy may be wrong—the founding problem may be persistent rather than dead, and the constraint solves it even after state licensing weakens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(meiji_rescission_and_resurrection, empirical, 'Whether Meiji syncretism resurrection addressed the same founding problem or new state interests').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(shinbutsu_ontological_substrate__incoherent_bundle_reading, 0, 250).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(shin_tr_t0, shinbutsu_ontological_substrate__incoherent_bundle_reading, theater_ratio, 0, 0.3).
narrative_ontology:measurement(shin_tr_t50, shinbutsu_ontological_substrate__incoherent_bundle_reading, theater_ratio, 50, 0.38).
narrative_ontology:measurement(shin_tr_t100, shinbutsu_ontological_substrate__incoherent_bundle_reading, theater_ratio, 100, 0.48).
narrative_ontology:measurement(shin_tr_t150, shinbutsu_ontological_substrate__incoherent_bundle_reading, theater_ratio, 150, 0.56).
narrative_ontology:measurement(shin_tr_t200, shinbutsu_ontological_substrate__incoherent_bundle_reading, theater_ratio, 200, 0.6).
narrative_ontology:measurement(shin_tr_t250, shinbutsu_ontological_substrate__incoherent_bundle_reading, theater_ratio, 250, 0.62).

% Extraction over time
narrative_ontology:measurement(shin_be_t0, shinbutsu_ontological_substrate__incoherent_bundle_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(shin_be_t50, shinbutsu_ontological_substrate__incoherent_bundle_reading, base_extractiveness, 50, 0.52).
narrative_ontology:measurement(shin_be_t100, shinbutsu_ontological_substrate__incoherent_bundle_reading, base_extractiveness, 100, 0.61).
narrative_ontology:measurement(shin_be_t150, shinbutsu_ontological_substrate__incoherent_bundle_reading, base_extractiveness, 150, 0.66).
narrative_ontology:measurement(shin_be_t200, shinbutsu_ontological_substrate__incoherent_bundle_reading, base_extractiveness, 200, 0.67).
narrative_ontology:measurement(shin_be_t250, shinbutsu_ontological_substrate__incoherent_bundle_reading, base_extractiveness, 250, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(shin_su_t0, shinbutsu_ontological_substrate__incoherent_bundle_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(shin_su_t50, shinbutsu_ontological_substrate__incoherent_bundle_reading, suppression_requirement, 50, 0.6).
narrative_ontology:measurement(shin_su_t100, shinbutsu_ontological_substrate__incoherent_bundle_reading, suppression_requirement, 100, 0.66).
narrative_ontology:measurement(shin_su_t150, shinbutsu_ontological_substrate__incoherent_bundle_reading, suppression_requirement, 150, 0.7).
narrative_ontology:measurement(shin_su_t200, shinbutsu_ontological_substrate__incoherent_bundle_reading, suppression_requirement, 200, 0.7).
narrative_ontology:measurement(shin_su_t250, shinbutsu_ontological_substrate__incoherent_bundle_reading, suppression_requirement, 250, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(shinbutsu_ontological_substrate__incoherent_bundle_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(shinbutsu_ontological_substrate__incoherent_bundle_reading, shinbutsu_ontological_substrate__syncretic_fusion_reading).
narrative_ontology:affects_constraint(shinbutsu_ontological_substrate__incoherent_bundle_reading, shinbutsu_ontological_substrate__domain_partition_reading).

% DUAL FORMULATION NOTE:
% The shinbutsu kernel admits three structurally distinct readings: syncretic-fusion (kami-buddha unity is metaphysical truth), domain-partition (kami-buddha coexistence is functional, domains separate), and incoherent-bundle (no coherent kernel; syncretism is enforced institutional drift). Each reading assigns different ε values, beneficiary structures, and causal stories to the same institutional fact. The incoherent-bundle reading argues the state benefits from maintaining theological incoherence to keep priesthoods dependent on state arbitration; it classifies as snare. The syncretic-fusion reading argues syncretism expresses genuine metaphysical unity and classifies (from the syncretic-fusion seat) as rope. The domain-partition reading argues kami and buddhas naturally separate and syncretism is a stable functional arrangement, classifying (from that seat) as rope. The three readings are linked via network.affects_constraints: the incoherent-bundle reading influences both siblings by arguing the apparent metaphysical or functional coherence masks state enforcement of contradiction.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
