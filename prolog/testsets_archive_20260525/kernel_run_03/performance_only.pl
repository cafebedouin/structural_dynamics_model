% ============================================================================
% CONSTRAINT STORY: performance_only
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_performance_only, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: performance_only
 *   human_readable: Performance-Only Reading of the Kodashim Corpus: Legitimate Extraction via Deferred Messianic Promise
 *   domain: religious_studies/rabbinic_judaism/commitment_systems
 *
 * SUMMARY:
 *   The performance_only reading of the Kodashim corpus (the rabbinic legal
 *   tradition concerning Temple sacrifice) represents a specific
 *   institutional response to the destruction of the Second Temple in 70 CE.
 *   The reading treats the archive of sacrificial law as preparation for
 *   messianic restoration, when the Temple will be rebuilt and sacrifice will
 *   resume. Under this reading, the entire corpus—detailed specifications for
 *   animal offerings, priestly procedures, sanctuary measurements, purity
 *   requirements—gains legitimacy as a deferred-performance document: its
 *   value lies entirely in its future enactment. This constraint exhibits
 *   high extractiveness (0.68) and high suppression (0.72) because it creates
 *   a structure where practitioners devote decades to mastering laws they
 *   cannot and will not perform, motivated by a messianic restoration
 *   axiomatically placed outside historical time. The theater ratio has risen
 *   sharply over 1500 years (from 0.55 to 0.85) as the archive has become
 *   increasingly elaborate (Talmudic analysis spawned Geonic responsa,
 *   medieval commentaries, Kabbalistic reinterpretations, and contemporary
 *   hypotheticals about sacrifice mechanics) despite universal acknowledgment
 *   that the performance remains impossible. This constraint is ONE READING
 *   of the Kodashim kernel, instantiating the performance_only
 *   interpretation. Two sibling readings coexist: study_as_exercise (treating
 *   archive study as intrinsically valuable spiritual discipline independent
 *   of deferred performance) and substitution_archive (treating prayer,
 *   intention, or other acts as contemporary substitutes for sacrifice, not
 *   merely preparation for future restoration). The performance_only reading
 *   is neither self-evident nor natural-law; it is a contingent institutional
 *   commitment with identifiable beneficiaries (messianic-preparation
 *   institutions) and victims (practitioners whose labor is extracted toward
 *   impossible future performance).
 *
 * KEY AGENTS:
 *   - Devotional Practitioners (powerless/identity_locked): Primary victims bearing labor, time, and spiritual devotion extracted toward deferred performance they cannot enact.
 *   - Community of Archive Keepers (moderate/constrained): Secondary victims maintaining textual precision and educational infrastructure for a practice they cannot perform.
 *   - Messianic-Preparation Institutions (institutional/arbitrage): Primary beneficiaries extracting institutional authority, resource allocation, and obedience justified by archive legitimacy.
 *   - Counter-Reading Communities (organized/constrained): Organized holders of alternative readings (study_as_exercise) experiencing extraction pressure through delegitimization by performance_only authority.
 *   - Historical Performance Institutions (institutional/arbitrage): Post-70 CE and post-135 CE rabbinic institutions that created the reading as functional response to catastrophe; now maintain it through inertia.
 *   - Analytical Observer (analytical/analytical): Risks naturalizing contingent institutional commitment as immutable natural law.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(performance_only, 0.68).
domain_priors:suppression_score(performance_only, 0.72).
domain_priors:theater_ratio(performance_only, 0.85).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(performance_only, extractiveness, 0.68).
narrative_ontology:constraint_metric(performance_only, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(performance_only, theater_ratio, 0.85).

% --- Constraint claim ---
narrative_ontology:constraint_claim(performance_only, snare).
narrative_ontology:human_readable(performance_only, "Performance-Only Reading of the Kodashim Corpus: Legitimate Extraction via Deferred Messianic Promise").
narrative_ontology:topic_domain(performance_only, "religious_studies/rabbinic_judaism/commitment_systems").

domain_priors:requires_active_enforcement(performance_only).

% --- Commitment system structure ---
narrative_ontology:cs_kernel_codification(performance_only, fixed_text).
narrative_ontology:cs_authority_grounding(performance_only, extraction).
narrative_ontology:cs_interpretation_layer_present(performance_only).
narrative_ontology:cs_kernel_id(performance_only, kodashim_corpus).
narrative_ontology:cs_reading_relation(performance_only, study_as_exercise, coexists_with).
narrative_ontology:cs_reading_relation(performance_only, substitution_archive, coexists_with).
narrative_ontology:cs_axiom(performance_only, foundational, performance_validates_law).
narrative_ontology:cs_axiom_status(performance_validates_law, holdable).
narrative_ontology:cs_axiom_grounding(performance_only, performance_validates_law, deontological).
narrative_ontology:cs_axiom(performance_only, foundational, deferred_messianic_restoration).
narrative_ontology:cs_axiom_status(deferred_messianic_restoration, holdable).
narrative_ontology:cs_axiom_grounding(performance_only, deferred_messianic_restoration, theological).
narrative_ontology:cs_reference_frame(performance_only, textual_preservation_for_restoration).
narrative_ontology:cs_drift_state(performance_only, contemporary_post_temple_indefinite, gap(axiom_overriding, substantial, false)).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(performance_only, messianic_preparation_institutions).
narrative_ontology:constraint_victim(performance_only, devotional_practitioners).
narrative_ontology:constraint_victim(performance_only, archive_as_living_practice_adherents).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DEVOTIONAL PRACTITIONER (SNARE via identity_locked) — Structurally mobile (could cease study, could adopt alternative religious practice, could emigrate), but identity is constituted through the archive. The practitioner's self-concept fuses with the role of 'keeper of the law until the Temple returns.' Exit would require abandoning not just a practice but an identity that has been internalized through decades of study. The constraint extracts from this agent: time, labor, spiritual devotion, and hope — all directed toward a performance state (restoration of Temple sacrifice) that the reading explicitly declares impossible in the current era. Maximum extraction because the agent bears full cost while the performance remains perpetually deferred. The performance_only reading makes this extraction visible: study has value only as preparation for future performance, yet that performance is axiomatically denied to this agent.
constraint_indexing:constraint_classification(performance_only, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(global))).

% PERSPECTIVE 2: COMMUNITY OF ARCHIVE KEEPERS (SNARE) — Organized at the community level but facing generational resource constraints (educational infrastructure, rabbinic training, institutional maintenance). The performance_only reading constrains this community: they must maintain textual and interpretive precision for a practice (animal sacrifice) that they cannot perform. The suppression is structural—the community cannot exit without forfeiting religious identity; cannot perform the archive's prescribed acts; cannot claim the archive is merely historical (the reading forbids this reframing). Suppression manifests as continuous tension between textual precision (required to maintain legitimacy of future performance) and lived impossibility (the performance cannot occur). High theater: the archive functions primarily as performative keeper of the law, not as guide to current practice.
constraint_indexing:constraint_classification(performance_only, snare,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: MESSIANIC-PREPARATION INSTITUTIONS (ROPE) — Benefit from the performance_only reading: it provides institutional legitimacy for resource allocation, educational infrastructure, and political authority. These institutions extract value from communities' continued investment in archive preservation without bearing the cost of implementing the archive's prescriptions. The reading enables institutional arbitrage: institutions can claim to guide preparation for an impossible performance, allocate resources to maintain the archive, and extract obedience and funding from communities based on archive authority—all while the performance axiomatically remains deferred. From the institutional perspective, this constraint solves a coordination problem: how to maintain religious authority and resource control when the core practice (Temple sacrifice) is impossible? Answer: reframe the archive as preparation for messianic restoration. The coordination function is genuine from this perspective—the constraint does solve the institutional problem of maintaining authority and unity across diaspora communities.
constraint_indexing:constraint_classification(performance_only, rope,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: COUNTER-READING COMMUNITIES (TANGLED ROPE) — Communities holding the sibling 'study_as_exercise' reading see genuine coordination function in the archive (intellectual tradition, spiritual discipline, law study as intrinsic good) but experience extraction pressure from performance_only institutions that claim sole authority over archive interpretation. These communities have constrained exit: they cannot deny the archive's textual authority (would forfeit tradition), but they cannot accept the performance_only framing (which subordinates their reading to messianic expectation). The constraint generates asymmetric extraction: performance_only institutions delegitimize alternative readings, monopolize interpretive authority, and extract compliance through claims that only messianic preparation justifies archive study. The tangled rope classification reflects that coordination exists (shared textual tradition, unified legal framework) but is distorted by extraction (institutional monopoly on legitimate interpretation).
constraint_indexing:constraint_classification(performance_only, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: HISTORICAL PERFORMANCE INSTITUTIONS (PITON) — The post-70 CE and post-135 CE rabbinic institutions that initially created the performance_only reading were addressing a genuine catastrophe: the Temple is destroyed; sacrifice cannot be performed; yet religious authority, textual precision, and community identity must be maintained. The reading was functional—it provided a framework for continuing religious practice under impossible conditions. Two millennia later, the reading persists largely through institutional inertia: rabbinic education teaches performance_only as the settled tradition; institutions allocate resources to archive preservation; communities accept the deferred-performance frame as normal. But the theater has risen sharply (≈0.85): the archive is maintained with elaborate precision (responsa literature, analytic commentaries, detailed hypotheticals about sacrifice mechanics) despite universal acknowledgment that sacrifice cannot occur until messianic restoration. The institutional machinery for archive maintenance persists because institutions depend on it for authority and resource allocation, not because practitioners believe restoration is imminent or that archive study requires messianic justification.
constraint_indexing:constraint_classification(performance_only, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, the performance_only reading appears to reflect an immutable structural feature: the Temple is destroyed; sacrifice is impossible; therefore performance is permanently deferred; therefore the archive can only be a preparation for impossible future performance. This perspective treats the performance_only framing as a natural law—a consequence of irreversible historical fact (destruction) that leaves no degrees of freedom. However, this classification is a FALSE SUMMIT: the performance_only reading is not a consequence of destruction alone, but a specific *interpretation* of destruction that has institutional beneficiaries and imposes extraction costs on practitioners. The sibling 'study_as_exercise' reading treats the same destroyed Temple as grounds for an entirely different constraint (study as intrinsic practice, not deferred performance). The mountain classification naturalizes what is actually a contingent institutional commitment.
constraint_indexing:constraint_classification(performance_only, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(performance_only_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(performance_only, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(performance_only, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(performance_only, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(performance_only, TR),
    TR >= 0.70.

:- end_tests(performance_only_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High but not maximal. The performance_only reading extracts from practitioners (time spent studying unperfomable laws, devotion directed toward impossible future, identity fused with archive keeper role), but the extraction is justified by a legitimacy narrative (preparation for messianic restoration). The narrative provides partial mitigation—practitioners believe their labor is meaningful, even if it will never result in actual performance. The extractiveness is not as severe as a pure snare (which provides no justifying narrative) because the institutional beneficiaries do solve a genuine problem: how to maintain religious authority and community identity when the core practice is impossible. If extractiveness were higher (0.75+), the reading would lose plausibility—practitioners would recognize the deception. The extractiveness reflects the edge case where extraction can persist precisely because it is partially legitimized by an institutional commitment. Suppression (0.72): High. Practitioners face multiple suppression mechanisms: (1) theological suppression—the reading makes alternative interpretations (study_as_exercise) illegitimate by treating performance as the ultimate validating ground; (2) institutional suppression—community and rabbinic authority enforce the performance_only frame and delegitimize alternatives; (3) identity suppression—practitioners' self-concept is constituted through the archive keeper role, making exit psychologically impossible even if materially possible (identity_locked exit). Theater ratio (0.85): Very high and rising. The archive's primary function is performative: maintaining textual precision, training new interpreters, producing elaborate hypotheticals about sacrifice mechanics that cannot be tested or implemented. The theater has increased over 1500 years as the archive has become more elaborate without ever moving closer to actual performance. The gap between textual sophistication and performance possibility has grown; the reading justifies this gap by treating sophistication as preparation (which explains why performance keeps receding but archive keeps elaborating).
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates maximum perspectival divergence from a single set of base properties. The institutional beneficiary sees pure coordination (Rope from perspective 3): the reading solves the problem of maintaining authority when performance is impossible—a genuine coordination function. The organized counter-reading communities see tangled rope (Perspective 4): the archive provides coordination (shared legal tradition, unified textual authority) but is distorted by extraction (monopoly on interpretation, delegitimization of alternatives). The practitioners see pure extraction (Snare from perspective 1): the archive extracts labor and identity toward a performance that will never occur, justified by a narrative about deferred messianic restoration. The historical institutions see degraded ritual (Piton from perspective 5): the archive is maintained through institutional machinery that persists despite lack of genuine belief in imminent restoration. The analytical observer at civilizational scope risks seeing immutable natural law (Mountain from perspective 6): the destroyed Temple makes performance impossible, therefore the archive must be deferred-performance by necessity. But this is a false summit—the performance_only reading is a contingent institutional choice, not a natural consequence of destruction. The sibling study_as_exercise reading treats the same destroyed Temple as grounding archive study as intrinsically valuable practice, not deferred performance. The gap between perspectives reveals that the natural law status is spurious.
 *
 * DIRECTIONALITY LOGIC:
 *   The performance_only reading derives directionality from the asymmetry between institutional beneficiaries and practitioner victims. Beneficiaries (messianic-preparation institutions) occupy positions with arbitrage options: they can maintain authority by allocating resources to archive preservation without performing the archive's prescriptions. Victims (practitioners) are trapped in identity_locked or constrained positions: they cannot exit without abandoning identity or forfeiting community membership. The beneficiary directionality (d ≈ 0.10-0.20, full beneficiary with arbitrage options) produces negative effective extraction f(d) ≈ -0.01 to 0.02, indicating that institutions experience the constraint as net beneficial coordination. The victim directionality (d ≈ 0.88-0.92, full target with identity-lock) produces maximum effective extraction f(d) ≈ 1.28-1.35, indicating that practitioners experience high felt extraction. The scope is global (diaspora Jewish communities across geographies and centuries) and the extraction is scaled by scope modifier σ(global) = 1.2, amplifying the effective extractiveness. The chi calculation produces values consistent with snare classification: χ = ε × f(d) × σ(S) ≈ 0.68 × 1.30 × 1.2 ≈ 1.06 for victims, producing effective extraction well above coordination thresholds.
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLVED: The performance_only reading is unambiguously a Snare (extractiveness 0.68 > 0.46, suppression 0.72 ≥ 0.60, effective extraction χ ≥ 0.66 for victims), not a disguised Rope. The mandatrophy is resolved by recognizing that the constraint does NOT provide genuine coordination benefit to practitioners—it provides coordination benefit to institutions while extracting from practitioners. The coordination function (maintaining religious authority and community identity) is real but asymmetric: it benefits institutional beneficiaries and harms practitioner victims. This is the defining structure of a Snare: legitimate coordination function for beneficiaries, pure extraction for victims, achieved through a suppression mechanism (theology + institutional authority + identity-lock) that makes exit appear impossible or illegitimate. The reading is not a failed Rope that accidentally became extractive; it is a Snare whose legitimacy narrative (preparation for messianic restoration) justifies the asymmetric extraction to both beneficiaries and victims.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    messianic_timeline_contingency,
    'Is the performance_only reading''s legitimacy contingent on the possibility of messianic restoration, or is restoration an unfalsifiable promise that makes the reading functionally permanent?',
    'Examine rabbinic sources for explicit temporal predictions of restoration; analyze historical rabbinic responses to failed messianic expectations (Bar Kokhba, Sabbatai Zvi); determine whether contemporaneous rabbinic authorities treated restoration as imminent, distant, or eschatological-but-not-historically-bounded.',
    'If restoration is treated as imminent: the performance_only reading is a temporary scaffold with a genuine sunset. If restoration is eschatological (outside historical time): the reading is functionally permanent and the extraction is locked indefinitely. If restoration is unfalsifiable: the reading becomes a mechanism for extracting perpetual compliance without ever delivering the promised performance state.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(messianic_timeline_contingency, empirical, 'Whether messianic restoration is imminent, distant, or eschatological').

omega_variable(
    identity_lock_mechanism_epistemic,
    'Is the identity-lock binding on practitioners cognitive (the archive''s legitimacy narrative has become internalized as identity) or structural (the archive is embedded in broader institutional and social forms that practitioners cannot exit without material loss)?',
    'Ethnographic and historical analysis: examine practitioners who have attempted to exit (apostasy, conversion, adoption of alternative Judaism readings); identify whether exit failure is due to internalized identity (they could leave structurally but cannot psychologically) or to community sanctions, institutional barriers, or family pressure (structural constraints that would need external alteration, not internal identity work).',
    'If purely cognitive: practitioners could potentially break the identity lock through frames/communities that revalue alternative readings. If structural: the constraint operates through irreducible external barriers and identity work alone cannot enable exit. If mixed: the classification of identity_locked is accurate; the suppression (0.72) reflects both internalized and structural mechanisms.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_mechanism_epistemic, empirical, 'Whether identity-lock on practitioners is cognitive or structural').

omega_variable(
    institutional_benefit_extractability,
    'Can messianic-preparation institutions maintain authority and resource allocation without the performance_only reading, using alternative frameworks (study-as-intrinsic-good, archive-as-civilization-foundation, or other sibling readings)?',
    'Comparative institutional analysis: identify rabbinic movements and Jewish communities that have adopted alternative readings (Karaite rejection of oral tradition, Reconstructionist treatment of archive as evolving practice, Kabbalah''s reframing of sacrifice as mystical meditation); assess whether these communities maintain comparable institutional capacity, authority structures, and resource flows despite abandoning performance_only framing.',
    'If institutions can maintain authority through alternative readings: the performance_only reading is contingent and chosen for its extractive benefits—the snare classification is confirmed. If alternative readings systematically fail to sustain comparable institutions: the performance_only reading may provide genuine institutional coordination benefit and the classification should shift toward tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_benefit_extractability, empirical, 'Whether institutional authority requires performance_only framing').

omega_variable(
    false_summit_natural_law_claim,
    'Does the performance_only reading''s claim to reflect immutable natural law derive from the destroyed Temple (an actual historical fact), or does it derive from a specific interpretive commitment that could coherently be replaced by the sibling ''study_as_exercise'' reading (which treats the destroyed Temple as grounding archive study as intrinsic practice rather than deferred performance)?',
    'Textual analysis of foundational rabbinic sources (Mishnah, Gemara, early Geonic literature): identify whether the destruction is treated as generating uniquely one interpretive path (performance-only) or whether multiple readings coexist in the sources themselves. Assess whether the performance_only reading is presented as derived from destruction or as one application of broader legal and theological principles.',
    'If multiple readings coexist in sources: the performance_only framing is a contingent institutional choice, not a natural consequence of destruction—the false summit detection is justified and the mountain perspective is reclassified as snare or tangled_rope. If sources present performance-only as the uniquely derived interpretation: the natural law classification may be appropriate and the false summit trigger is a misfire.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(false_summit_natural_law_claim, conceptual, 'Whether performance-only derives from destruction or from interpretive choice').

omega_variable(
    committer_frame_kernel_underdetermination,
    'Is the performance_only reading''s legitimacy claim grounded in the objective fact that the Temple is destroyed (an empirical constraint on performance), or is it grounded in a normative commitment about what legitimate religious authority requires (a theological/institutional commitment)?',
    'Examine whether rabbinic sources that embrace performance_only do so because destruction makes performance impossible (empirical ground) or because they hold a normative commitment that archive study must defer to future performance as the ultimate validation of religious law (normative ground). The distinction: empirical ground would permit alternative readings if the Temple were restored; normative ground would constrain how restoration could be interpreted even if it occurred.',
    'If empirical: performance_only is a contingent institutional reading justified by temporary historical conditions. If normative: performance_only is a foundational commitment about the proper relationship between textual authority and embodied practice that would persist even if conditions changed. If mixed: the reading''s claim to natural law status is partly justified and partly committal.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(committer_frame_kernel_underdetermination, conceptual, 'Whether performance-only grounds on empirical fact or normative commitment').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(performance_only, 0, 1500).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(perf_tr_t0, performance_only, theater_ratio, 0, 0.55).
narrative_ontology:measurement(perf_tr_t500, performance_only, theater_ratio, 500, 0.68).
narrative_ontology:measurement(perf_tr_t1000, performance_only, theater_ratio, 1000, 0.82).
narrative_ontology:measurement(perf_tr_t1500, performance_only, theater_ratio, 1500, 0.85).

% Extraction over time
narrative_ontology:measurement(perf_be_t0, performance_only, base_extractiveness, 0, 0.52).
narrative_ontology:measurement(perf_be_t500, performance_only, base_extractiveness, 500, 0.58).
narrative_ontology:measurement(perf_be_t1000, performance_only, base_extractiveness, 1000, 0.65).
narrative_ontology:measurement(perf_be_t1500, performance_only, base_extractiveness, 1500, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(performance_only, identity_coordination).
narrative_ontology:affects_constraint(performance_only, study_as_exercise).
narrative_ontology:affects_constraint(performance_only, substitution_archive).
narrative_ontology:affects_constraint(performance_only, rabbinic_authority_legitimacy).

% DUAL FORMULATION NOTE:
% The Kodashim corpus decomposes into three structurally distinct constraints, one per reading. The performance_only reading (this file) has ε ≈ 0.68 (high extraction); the study_as_exercise reading has ε ≈ 0.25 (coordination without extraction); the substitution_archive reading has ε ≈ 0.40 (tangled rope with mixed coordination and substitutional extraction). These are not the same constraint viewed from different angles—they are three different constraints with fundamentally different ε values, beneficiary/victim structures, and institutional beneficiaries. Each reading is instantiated as a separate constraint story and linked via network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
