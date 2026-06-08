% ============================================================================
% CONSTRAINT STORY: kami_buddha_ontology_flat_control
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_kami_buddha_ontology_flat_control, []).

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
    narrative_ontology:omega_variable/3,
    narrative_ontology:flat_control_of/2,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: kami_buddha_ontology_flat_control
 *   human_readable: Kami-Buddha Ontological Relationship in Japanese Religious Practice
 *   domain: religious_studies/japanese_history/ontology
 *
 * SUMMARY:
 *   The kami-buddha relationship in Japanese religious practice presents a
 *   fundamental ontological puzzle: practitioners simultaneously venerate
 *   kami (indigenous Shinto deities) and Buddhist figures within integrated
 *   ritual systems, yet the two traditions rest on incompatible cosmological
 *   foundations. Kami are understood as localized, embodied spirits tied to
 *   specific places and natural phenomena; Buddhist entities (bodhisattvas,
 *   buddhas) are understood as universal, transcendent principles. The
 *   constraint is the institutional and cognitive apparatus that maintains
 *   this simultaneous veneration as coherent practice rather than allowing
 *   explicit partition or separation. The honji suijaku doctrine (original
 *   essence, manifest traces) provides the primary theological framework:
 *   kami are understood as local manifestations of universal Buddhist
 *   principles, subordinating indigenous kami to Buddhist metaphysics. This
 *   constraint exhibits all six DR types from different perspectives,
 *   revealing how institutional power, identity-fusion, and doctrinal
 *   performance interact to maintain an ontologically unstable arrangement.
 *   The extractiveness trajectory shows a rise during the Edo period
 *   (institutional consolidation), a spike during the Meiji Restoration (when
 *   the state forcibly separated kami and Buddha to assert Shinto
 *   nationalism), and a stabilization in the postwar period as reintegration
 *   occurred. The theater ratio shows a similar pattern: the doctrinal
 *   apparatus became increasingly elaborate and performative as the
 *   underlying contradiction became more visible, peaking during the Meiji
 *   separation crisis when the state had to actively suppress the integrated
 *   practice, then declining slightly as postwar religious pluralism reduced
 *   the need for explicit doctrinal justification.
 *
 * KEY AGENTS:
 *   - Village Practitioners: Primary victims (powerless/identity_locked) — bear the cost of maintaining contradictory ontological claims; identity fused with integrated practice
 *   - Shrine Priests: Secondary beneficiaries and enforcers (moderate/constrained) — benefit from institutional authority and resource flows; constrained by institutional hierarchy
 *   - Institutional Buddhism: Primary beneficiary (institutional/arbitrage) — expands jurisdiction and resource capture through kami integration; experiences constraint as coordination
 *   - State Religious Authority: Primary beneficiary and enforcer (organized/constrained) — uses kami-buddha relationship as tool for religious monopoly and population control; enforces through doctrinal authority and institutional licensing
 *   - Doctrinal Scholasticism: Performative apparatus (institutional/arbitrage) — maintains elaborate theological frameworks that rationalize simultaneous veneration; function atrophied but performance continues through professional investment
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing contingent institutional arrangement as immutable feature of Japanese consciousness
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(kami_buddha_ontology_flat_control, 0.35).
domain_priors:suppression_score(kami_buddha_ontology_flat_control, 0.42).
domain_priors:theater_ratio(kami_buddha_ontology_flat_control, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(kami_buddha_ontology_flat_control, extractiveness, 0.35).
narrative_ontology:constraint_metric(kami_buddha_ontology_flat_control, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(kami_buddha_ontology_flat_control, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(kami_buddha_ontology_flat_control, tangled_rope).
narrative_ontology:human_readable(kami_buddha_ontology_flat_control, "Kami-Buddha Ontological Relationship in Japanese Religious Practice").
narrative_ontology:topic_domain(kami_buddha_ontology_flat_control, "religious_studies/japanese_history/ontology").

domain_priors:requires_active_enforcement(kami_buddha_ontology_flat_control).

% --- Construction-pair linkage (forced-flat control of a kernel) ---
narrative_ontology:flat_control_of(kami_buddha_ontology_flat_control, kami_buddha_ontology).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(kami_buddha_ontology_flat_control, institutional_buddhism).
narrative_ontology:constraint_beneficiary(kami_buddha_ontology_flat_control, state_religious_authority).
narrative_ontology:constraint_victim(kami_buddha_ontology_flat_control, kami_indigenous_autonomy).
narrative_ontology:constraint_victim(kami_buddha_ontology_flat_control, practitioner_epistemic_clarity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: VILLAGE PRACTITIONER (SNARE) — Structurally mobile (could theoretically abandon practice) but identity-fused with simultaneous kami-buddha veneration. The practitioner's identity is constituted through the integrated ritual practice; exit would require abandoning not just the constraint but the self-concept built within it. The constraint appears unchangeable from within the identity frame, even though the binding is cognitive rather than material. Maximum experienced extraction: the practitioner bears the cost of maintaining contradictory ontological claims without resolution.
constraint_indexing:constraint_classification(kami_buddha_ontology_flat_control, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(regional))).

% PERSPECTIVE 2: SHRINE PRIEST (TANGLED ROPE) — Constrained by institutional affiliation and career dependence on shrine authority, but also benefits from the ambiguity: the kami-buddha relationship allows shrine priests to maintain ritual authority and resource flows from both Buddhist and Shinto institutional networks. Genuine coordination function (managing dual-tradition practice) exists alongside asymmetric extraction (priests collect authority and resources; practitioners bear ontological confusion). Requires active enforcement through institutional hierarchy and doctrinal authority.
constraint_indexing:constraint_classification(kami_buddha_ontology_flat_control, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: INSTITUTIONAL BUDDHISM (ROPE) — Benefits from the kami-buddha relationship through expanded jurisdiction and resource capture. The constraint solves a genuine coordination problem: how to integrate indigenous kami veneration into Buddhist cosmology without losing institutional control. Buddhism experiences the relationship as coordination — the honji suijaku doctrine (original essence, manifest traces) provides a framework that subordinates kami to Buddhist metaphysics while preserving practitioner loyalty. Net beneficiary with arbitrage options: Buddhism can exit by rejecting kami integration, but chooses not to because the arrangement expands its reach.
constraint_indexing:constraint_classification(kami_buddha_ontology_flat_control, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: STATE RELIGIOUS AUTHORITY (TANGLED ROPE) — Organized institutional actor that benefits from the kami-buddha ambiguity as a tool for religious monopoly and population control. The constraint coordinates religious practice under state oversight while extracting legitimacy and compliance. The state enforces the relationship through doctrinal authority, shrine licensing, and priest certification. Constrained by the need to maintain the fiction of ontological coherence — if the relationship collapses into explicit contradiction, state authority over religious meaning-making becomes visible and contestable.
constraint_indexing:constraint_classification(kami_buddha_ontology_flat_control, tangled_rope,
    context(agent_power(organized),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: DOCTRINAL SCHOLASTICISM (PITON) — The honji suijaku doctrine and related theological frameworks are largely performative: they provide the appearance of ontological coherence without resolving the underlying contradiction. Scholars and priests maintain elaborate doctrinal systems that rationalize simultaneous veneration, but the systems are increasingly recognized as post-hoc justifications rather than genuine explanations. Theater ratio is high: the doctrinal apparatus persists through institutional inertia and professional investment, not because it solves the ontological problem. The function has atrophied — practitioners no longer believe the doctrine resolves the contradiction — but the performance continues.
constraint_indexing:constraint_classification(kami_buddha_ontology_flat_control, piton,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, the kami-buddha relationship might appear as an immutable feature of Japanese religious consciousness: the simultaneous veneration is so deeply embedded in practice that it appears to emerge naturally from the structure of Japanese cosmology itself. This perspective risks naturalizing what is actually a contingent institutional arrangement maintained through enforcement and identity-locking. The engine's false summit detector will identify this as a false summit: the 'natural' integration is actually a constructed constraint that benefits institutional Buddhism and state authority.
constraint_indexing:constraint_classification(kami_buddha_ontology_flat_control, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(kami_buddha_ontology_flat_control_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(kami_buddha_ontology_flat_control, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(kami_buddha_ontology_flat_control, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(kami_buddha_ontology_flat_control, TR),
    TR >= 0.70.

:- end_tests(kami_buddha_ontology_flat_control_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.35): Moderate. The constraint extracts from practitioners (who bear ontological confusion and cognitive dissonance) and from kami autonomy (which is subordinated to Buddhist metaphysics), but the extraction is not maximal because the integrated practice also provides genuine coordination benefits — practitioners can maintain loyalty to both traditions without forced choice, and the constraint enables resource flows and institutional stability. The extractiveness trajectory shows rise during institutional consolidation (Edo), spike during forced separation (Meiji), and stabilization as reintegration occurred. Suppression (0.42): Moderate. Significant barriers to exit include social penalty (disaffiliation from integrated practice carries community cost), identity-fusion (practitioners' self-concept is constituted through the practice), and institutional enforcement (shrine licensing, priest certification, doctrinal authority). But suppression is not total — some practitioners do exit, and contemporary religious pluralism has reduced barriers. The spike during Meiji reflects explicit state suppression of integrated practice. Theater ratio (0.58): Moderate-high. The honji suijaku doctrine and related theological frameworks are substantially performative — they provide appearance of ontological coherence without resolving the underlying contradiction. The doctrine is increasingly recognized as post-hoc justification rather than genuine explanation. Theater peaked during Meiji separation crisis when the state had to actively suppress the integrated practice and elaborate new doctrinal justifications for separation. Contemporary theater remains elevated because the doctrinal apparatus persists through institutional inertia despite reduced belief in its explanatory power.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates perspectival divergence across institutional positions. The village practitioner (powerless/identity_locked) experiences the constraint as a snare — they cannot exit because their identity is fused with the integrated practice, and they bear the cost of maintaining contradictory ontological claims. The shrine priest (moderate/constrained) experiences tangled rope — they benefit from institutional authority and resource flows while also being constrained by institutional hierarchy. Institutional Buddhism (institutional/arbitrage) experiences rope — the constraint solves a genuine coordination problem (integrating indigenous practice into Buddhist framework) and Buddhism benefits from expanded jurisdiction. The state religious authority (organized/constrained) experiences tangled rope — the constraint coordinates religious practice under state oversight while extracting legitimacy and compliance. The doctrinal scholasticism (institutional/arbitrage) experiences piton — the elaborate theological frameworks are performative, maintained through professional investment rather than genuine explanatory power. The analytical observer (analytical/analytical) risks seeing mountain — naturalizing the kami-buddha relationship as an immutable feature of Japanese consciousness — but the structural data reveals this as a false summit: the relationship is a constructed constraint maintained through enforcement and identity-locking.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality (d) is determined by the agent's structural position relative to the extraction flow. Village practitioners are full targets (d ≈ 0.85): they bear the cost of ontological confusion and identity-fusion prevents exit. Shrine priests are partial targets (d ≈ 0.45): they benefit from institutional authority but are constrained by hierarchy. Institutional Buddhism is a beneficiary (d ≈ 0.15): the constraint expands their jurisdiction and they have arbitrage options (could exit by rejecting kami integration). State religious authority is a beneficiary (d ≈ 0.20): the constraint enables religious monopoly and they have constrained exit (could theoretically abandon religious control but choose not to). The engine derives these d values from beneficiary/victim declarations and exit options; the directionality determines effective extraction (χ) through the sigmoid function f(d). Trapped and identity-locked agents experience higher χ; beneficiaries with arbitrage options experience lower or negative χ.
 *
 * MANDATROPHY ANALYSIS:
 *   The kami-buddha relationship exhibits mandatrophy: the original mandate (integrating indigenous kami veneration into Buddhist institutional framework during the Heian period) has outlived its functional necessity. The constraint persists not because it solves the original coordination problem but because institutional Buddhism and state authority benefit from maintaining it. The Meiji Restoration (1868) provides a diagnostic moment: the state forcibly separated kami and Buddha to assert Shinto nationalism, demonstrating that the 'natural' integration was actually contingent on institutional enforcement. The postwar reintegration (1945+) shows that the constraint can be reassembled when institutional interests align. The contemporary persistence of the constraint despite widespread intellectual rejection of the honji suijaku doctrine indicates that the mandate has been replaced by institutional inertia and identity-fusion. The theater ratio trajectory (rising from 0.42 to 0.65 during Meiji separation, then declining to 0.58 in contemporary period) reflects this mandatrophy: as the original coordination function became less necessary, the doctrinal apparatus became more elaborate and performative to maintain the appearance of coherence. The constraint now persists primarily through identity-locking of practitioners and institutional investment in doctrinal scholasticism, not through genuine coordination function.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    ontological_unity_vs_partition,
    'Does the kami-buddha relationship constitute a genuine unified cosmology, or is it a domain-partitioned coexistence maintained by institutional enforcement?',
    'Historical analysis of doctrinal development (honji suijaku emergence and evolution); ethnographic study of practitioner understanding (do practitioners experience unified cosmology or compartmentalized practice?); examination of moments when the partition breaks down (Meiji Restoration separation, contemporary disaffiliation patterns)',
    'If unified cosmology: the constraint is primarily Rope (coordination) from most perspectives. If domain-partitioned: the constraint is primarily Snare (extraction) from practitioner perspective and Tangled Rope from institutional perspectives. The classification hinges on whether the integration is experienced as coherent or maintained through suppression of contradiction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ontological_unity_vs_partition, empirical, 'Whether kami-buddha relationship is unified cosmology or enforced partition').

omega_variable(
    identity_lock_vs_constrained_exit,
    'Is the practitioner''s inability to exit the simultaneous veneration due to identity-fusion (cognitive lock) or to material constraints (social penalty, economic dependence, legal restriction)?',
    'Comparative analysis of practitioners who have exited (what barriers did they overcome?); study of practitioners who maintain the practice despite intellectual rejection (what prevents exit?); examination of generational change in practice adherence and explicit disaffiliation rates',
    'If identity-locked: the constraint operates through internalized framing and would persist even if external barriers were removed. If constrained/trapped: the constraint operates through material barriers and would dissolve if barriers were removed. The distinction determines whether the suppression metric reflects structural or internalized mechanisms.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(identity_lock_vs_constrained_exit, empirical, 'Whether practitioner exit barriers are cognitive or material').

omega_variable(
    doctrinal_sincerity_vs_performance,
    'Do institutional actors (Buddhist scholars, shrine priests, state authorities) genuinely believe the honji suijaku doctrine resolves the ontological contradiction, or do they maintain it as performative cover for institutional interests?',
    'Analysis of doctrinal evolution and internal critiques within Buddhist scholarship; examination of private correspondence and institutional records; study of how doctrine is taught to priests vs. how it is explained to practitioners; observation of doctrinal flexibility when institutional interests change',
    'If sincere belief: theater_ratio should be lower (0.30-0.40) and the constraint is more Rope-like. If performative: theater_ratio is correctly assessed at 0.58+ and the constraint is more Piton-like. The distinction affects whether the constraint is maintained through genuine coordination or through institutional inertia.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(doctrinal_sincerity_vs_performance, empirical, 'Whether doctrinal framework is sincere or performative').

omega_variable(
    state_enforcement_mechanism,
    'What specific enforcement mechanisms does the state use to maintain the kami-buddha relationship as a unified constraint rather than allowing explicit partition or separation?',
    'Historical examination of shrine licensing, priest certification, doctrinal approval processes, and legal restrictions on religious practice; analysis of moments when enforcement was relaxed (Meiji Restoration, postwar religious freedom) and how the constraint changed; study of contemporary enforcement through subtle mechanisms (funding, prestige, institutional recognition)',
    'If enforcement is explicit and visible: suppression metric is accurate and the constraint is clearly Tangled Rope. If enforcement is subtle or internalized: suppression may be underestimated and the constraint may be more Snare-like than assessed. The mechanism determines whether the constraint persists through coercion or through cognitive capture.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(state_enforcement_mechanism, empirical, 'State enforcement mechanisms maintaining kami-buddha unity').

omega_variable(
    false_summit_natural_law_risk,
    'Is the kami-buddha relationship a genuine natural law of Japanese religious consciousness, or a constructed constraint that benefits institutional Buddhism and state authority?',
    'Comparative analysis with other religious traditions (do other cultures show similar simultaneous veneration of incompatible ontologies?); historical analysis of pre-Buddhist Japan (was kami veneration ontologically unified before Buddhist integration?); study of contemporary Japan (are younger generations maintaining the relationship, or is it atrophying?)',
    'If natural law: the constraint should be classified as Mountain from all perspectives. If constructed: the constraint is a false summit and should be reclassified as Tangled Rope or Snare depending on perspective. The distinction determines whether the constraint is immutable or contingent.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(false_summit_natural_law_risk, conceptual, 'Whether kami-buddha relationship is natural law or constructed constraint').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(kami_buddha_ontology_flat_control, 0, 12).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(kbo_theater_t0_heian, kami_buddha_ontology_flat_control, theater_ratio, 0, 0.42).
narrative_ontology:measurement(kbo_theater_t3_edo, kami_buddha_ontology_flat_control, theater_ratio, 3, 0.48).
narrative_ontology:measurement(kbo_theater_t6_meiji_separation, kami_buddha_ontology_flat_control, theater_ratio, 6, 0.65).
narrative_ontology:measurement(kbo_theater_t9_postwar_reintegration, kami_buddha_ontology_flat_control, theater_ratio, 9, 0.62).
narrative_ontology:measurement(kbo_theater_t12_contemporary, kami_buddha_ontology_flat_control, theater_ratio, 12, 0.58).

% Extraction over time
narrative_ontology:measurement(kbo_extractiveness_t0_heian, kami_buddha_ontology_flat_control, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(kbo_extractiveness_t3_edo, kami_buddha_ontology_flat_control, base_extractiveness, 3, 0.32).
narrative_ontology:measurement(kbo_extractiveness_t6_meiji_separation, kami_buddha_ontology_flat_control, base_extractiveness, 6, 0.38).
narrative_ontology:measurement(kbo_extractiveness_t9_postwar_reintegration, kami_buddha_ontology_flat_control, base_extractiveness, 9, 0.35).
narrative_ontology:measurement(kbo_extractiveness_t12_contemporary, kami_buddha_ontology_flat_control, base_extractiveness, 12, 0.35).

% Suppression requirement over time
narrative_ontology:measurement(kbo_suppression_t0_heian, kami_buddha_ontology_flat_control, suppression_requirement, 0, 0.38).
narrative_ontology:measurement(kbo_suppression_t6_meiji_separation, kami_buddha_ontology_flat_control, suppression_requirement, 6, 0.55).
narrative_ontology:measurement(kbo_suppression_t12_contemporary, kami_buddha_ontology_flat_control, suppression_requirement, 12, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(kami_buddha_ontology_flat_control, identity_coordination).
narrative_ontology:affects_constraint(kami_buddha_ontology_flat_control, meiji_shinto_nationalism).
narrative_ontology:affects_constraint(kami_buddha_ontology_flat_control, postwar_religious_pluralism).
narrative_ontology:affects_constraint(kami_buddha_ontology_flat_control, buddhist_institutional_authority).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
