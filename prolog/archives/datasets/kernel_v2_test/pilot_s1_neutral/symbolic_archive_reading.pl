% ============================================================================
% CONSTRAINT STORY: symbolic_archive_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_symbolic_archive_reading, []).

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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: symbolic_archive_reading
 *   human_readable: Sacrifice Law as Symbolic Archive and Cultural Continuity
 *   domain: religious_law/halakhic_authority/commitment_system
 *
 * SUMMARY:
 *   The symbolic archive reading of the sacrifice obligation kernel treats
 *   sacrifice law as a continuous cultural-historical archive to be preserved
 *   and studied, not as a binding obligation to be obeyed. Under this
 *   reading, the cessation of sacrifice practice (whether due to Temple
 *   destruction or any other cause) does not create a violation or crisis —
 *   study itself becomes the mechanism of continuity. The interpretive
 *   community engages sacrifice texts (Mishnah, Talmud, medieval codes) as
 *   the record of a complex legal tradition, allowing the tradition's
 *   intellectual and moral resources to inform ongoing Jewish thought and
 *   ethics. The reading carries zero obligation, zero suppression, and zero
 *   extraction. It coordinates a genuine collective-action problem — how does
 *   a tradition persist when its original practice ceases? — through
 *   voluntary intellectual engagement. This reading coexists with three
 *   siblings: the study-as-exercise reading (study substitutes mechanically
 *   for performance), the performance-only reading (sacrifice obligation
 *   remains binding whenever possible), and the messianic suspension reading
 *   (obligation is suspended only pending restoration of the Temple). Each
 *   reading assigns a different ε value to the same kernel commitment and
 *   different structural consequences for the obligation's status.
 *
 * KEY AGENTS:
 *   - Jewish Collective Memory: Primary beneficiary (moderate/mobile) — benefits from preservation of textual tradition and cultural continuity; study practices ensure knowledge transmission across generations
 *   - Interpretive Tradition: Primary beneficiary (institutional/arbitrage) — continues to evolve as scholars engage sacrifice texts; benefits from ongoing creative reinterpretation
 *   - Individual Halakhic Scholar: Participant (moderate/mobile) — voluntarily engages sacrifice study as intellectual and cultural practice; no coercion, no asymmetric extraction
 *   - Transmitted Tradition Community: Collective beneficiary (moderate/constrained) — generational commitment to sustaining textual understanding and cultural coherence; modest coordination costs
 *   - Halakhic Authority Structure: Institutional interpreter (institutional/arbitrage) — maintains the reading's legitimacy by preserving the textual tradition as living material; benefits from institutional continuity
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(symbolic_archive_reading, 0.0).
domain_priors:suppression_score(symbolic_archive_reading, 0.0).
domain_priors:theater_ratio(symbolic_archive_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(symbolic_archive_reading, extractiveness, 0.0).
narrative_ontology:constraint_metric(symbolic_archive_reading, suppression_requirement, 0.0).
narrative_ontology:constraint_metric(symbolic_archive_reading, theater_ratio, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(symbolic_archive_reading, rope).
narrative_ontology:human_readable(symbolic_archive_reading, "Sacrifice Law as Symbolic Archive and Cultural Continuity").
narrative_ontology:topic_domain(symbolic_archive_reading, "religious_law/halakhic_authority/commitment_system").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(symbolic_archive_reading, 'a5fdb1e1-be2a-486f-96ca-0b22a31f90a8').
narrative_ontology:cs_kernel_codification('a5fdb1e1-be2a-486f-96ca-0b22a31f90a8', fixed_text).
narrative_ontology:cs_authority_grounding('a5fdb1e1-be2a-486f-96ca-0b22a31f90a8', lineage).
narrative_ontology:cs_interpretation_layer_present('a5fdb1e1-be2a-486f-96ca-0b22a31f90a8').
narrative_ontology:cs_reading_relation('a5fdb1e1-be2a-486f-96ca-0b22a31f90a8', symbolic_archive_reading__study_as_exercise_reading, influences).
narrative_ontology:cs_reading_relation('a5fdb1e1-be2a-486f-96ca-0b22a31f90a8', symbolic_archive_reading__performance_only_reading, coexists_with).
narrative_ontology:cs_reading_relation('a5fdb1e1-be2a-486f-96ca-0b22a31f90a8', symbolic_archive_reading__messianic_suspension_reading, influences).
narrative_ontology:cs_axiom('a5fdb1e1-be2a-486f-96ca-0b22a31f90a8', foundational, study_preserves_tradition_without_obligation).
narrative_ontology:cs_axiom_status(study_preserves_tradition_without_obligation, holdable).
narrative_ontology:cs_axiom_grounding('a5fdb1e1-be2a-486f-96ca-0b22a31f90a8', study_preserves_tradition_without_obligation, deontological).
narrative_ontology:cs_axiom('a5fdb1e1-be2a-486f-96ca-0b22a31f90a8', foundational, textual_continuity_sufficient_for_mandate_fulfillment).
narrative_ontology:cs_axiom_status(textual_continuity_sufficient_for_mandate_fulfillment, holdable).
narrative_ontology:cs_axiom_grounding('a5fdb1e1-be2a-486f-96ca-0b22a31f90a8', textual_continuity_sufficient_for_mandate_fulfillment, conventional).
narrative_ontology:cs_reference_frame('a5fdb1e1-be2a-486f-96ca-0b22a31f90a8', textual_continuity_through_study).
narrative_ontology:cs_drift_state('a5fdb1e1-be2a-486f-96ca-0b22a31f90a8', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('a5fdb1e1-be2a-486f-96ca-0b22a31f90a8', '2026-02-26T14:32:00Z').
narrative_ontology:cs_kernel_id(symbolic_archive_reading, sacrifice_obligation_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(symbolic_archive_reading, jewish_collective_memory).
narrative_ontology:constraint_beneficiary(symbolic_archive_reading, interpretive_tradition).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(symbolic_archive_reading, individual_halakhic_scholar).
narrative_ontology:constraint_beneficiary(symbolic_archive_reading, transmitted_tradition_community).
narrative_ontology:constraint_vindicates(symbolic_archive_reading, textual_continuity_doctrine).
narrative_ontology:constraint_vindicates(symbolic_archive_reading, memory_preservation_as_religious_obligation).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The Jewish tradition's accumulated ethical, legal, and spiritual resources embedded in sacrifice texts remain accessible and studied across generations. Through ongoing engagement with these texts, the collective preserves connection to its own intellectual history and moral framework. Continuity is maintained not through obligation but through voluntary intellectual inheritance.
narrative_ontology:constraint_stakeholder(symbolic_archive_reading, jewish_collective_memory, beneficiary,
    moderate, civilizational, mobile, global).

% The living tradition of textual interpretation continues to evolve as scholars engage sacrifice law as a resource for contemporary ethical and legal reasoning. The tradition persists through creative reinterpretation rather than literal observance. The beneficiary here is not a coercive entity but the intellectual legacy itself.
narrative_ontology:constraint_stakeholder(symbolic_archive_reading, interpretive_tradition, beneficiary,
    moderate, civilizational, mobile, global).
narrative_ontology:stakeholder_non_agent(symbolic_archive_reading, interpretive_tradition).

% The scholar voluntarily engages sacrifice texts as intellectual and spiritual practice. They choose to study, set their own pace and depth, and may exit at any time. They benefit from access to complex textual tradition and participation in an interpretive community. They set the agenda of their own engagement — neither obligation nor coercion structures the practice.
narrative_ontology:constraint_stakeholder(symbolic_archive_reading, individual_halakhic_scholar, agenda_setter,
    moderate, biographical, mobile, regional).
narrative_ontology:stakeholder_secondary_role(symbolic_archive_reading, individual_halakhic_scholar, beneficiary).

% Across generations, the community of interpreters maintains the textual tradition through study and transmission to the next generation. The community bears modest costs (time, effort, institutional maintenance) in sustaining the practice, but benefits from cultural continuity and coherence. Participation is expected but not enforced — generational commitment rather than external coercion.
narrative_ontology:constraint_stakeholder(symbolic_archive_reading, transmitted_tradition_community, beneficiary,
    moderate, generational, constrained, regional).

% The authority structure (rabbinical leadership, textual canonization, institutional transmission mechanisms) maintains the reading's legitimacy by preserving sacrifice law as continuous living text. The structure benefits from institutional continuity and authority derived from textual stewardship. It sets the framework within which study is understood as cultural continuity rather than obligation or substitute-performance.
narrative_ontology:constraint_stakeholder(symbolic_archive_reading, halakhic_authority_structure, agenda_setter,
    institutional, civilizational, arbitrage, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Preservation of a complex legal and ethical textual tradition across generations when its original practice becomes impossible or is suspended. How does a tradition persist, remain accessible, and continue to inform contemporary life when its original function (literal performance of sacrifices) ceases? Study-as-archive solves this through intellectual engagement rather than literal enactment.
% TRANSFER_FUNCTION: Time and intellectual effort flow from participants (scholars, communities) to the preservation and transmission of textual tradition. In return, participants receive access to cultural resources, intellectual community, connection to collective memory, and the satisfaction of maintaining a continuous tradition. Unlike extractive arrangements, this transfer is reciprocal — all participants benefit from both giving and receiving.
% ABSENT_VOICES: Those outside the Jewish interpretive tradition who do not participate in sacrifice law study are not present in this arrangement. Communities that have rejected continuity with the rabbinic tradition are excluded from this particular reading's framework. Those who hold the performance-only or messianic-suspension readings would argue that this reading wrongly suspends obligation — their objection is registered in the omega on reading coherence and the reading_relations declarations.
% DISAPPEARANCE_RATIONALE: If the symbolic archive reading and its accompanying study practices disappeared, the Jewish tradition would lose a major mechanism for maintaining continuity with its own intellectual history. Sacrifice law texts would become historical artifacts rather than living resources for ethical reasoning. Rabbinic Judaism depends on continuous engagement with the full corpus of halakhic literature; the systematic study and interpretation of sacrifice law is part of that machinery. Without it, the tradition's self-understanding and capacity to evolve in conversation with its sources would be diminished. The texts would not disappear, but the living tradition would rearrange — it would become more disconnected from its own depth.
% FOUNDING_PROBLEM: After the destruction of the Second Temple (70 CE), the Jewish people faced an unprecedented problem: how does a tradition whose central practice (Temple sacrifice) becomes impossible continue to exist as a living intellectual and spiritual force? The symbolic archive reading answers: study and textual engagement preserve the tradition's wisdom and keep it available for future generations, whether or not literal performance ever resumes.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem's persistence is attested by: (1) the Talmud itself, which extensively discusses sacrifice law despite the Temple's destruction, indicating the rabbinic tradition's commitment to keeping the material alive; (2) medieval and modern halakhic authorities who continue to engage sacrifice texts as core curriculum; (3) contemporary Jewish communities across denominations that maintain sacrifice law study in their curricula; (4) the fact that no alternative reading (performance-only, messianic-suspension, exercise) has fully displaced this reading, indicating that all remain live frameworks for understanding the same problem.
narrative_ontology:disappearance_verdict(symbolic_archive_reading, world_rearranges).
narrative_ontology:founding_problem_status(symbolic_archive_reading, live).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: INDIVIDUAL HALAKHIC SCHOLAR (ROPE) — Voluntary participant in the interpretive community. Engages sacrifice study as cultural and intellectual practice with no external coercion. Exit is possible (one may cease study), but the practice coordinates genuine collective understanding of textual tradition. Moderate power, mobile exit options: perceives the constraint as pure coordination without extraction.
constraint_indexing:constraint_classification(symbolic_archive_reading, rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(regional))).

% PERSPECTIVE 2: TRANSMITTED TRADITION COMMUNITY (ROPE) — Over generations, sacrifice law study transmits textual understanding and collective identity. Participants benefit from continuity and cultural coherence; they also bear the modest cost of maintaining the practice. The constraint coordinates identity preservation — a genuine collective-action problem (how does tradition persist if no one studies it?) — with no asymmetric extraction. Constrained exit reflects generational commitment rather than coercion.
constraint_indexing:constraint_classification(symbolic_archive_reading, rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: HALAKHIC AUTHORITY STRUCTURE — SYMBOLIC ARCHIVE READING (ROPE) — From this reading's institutional seat, the authority structure preserves sacrifice law as continuous living text without claiming binding obligation to practice sacrifice. The structure benefits from maintaining interpretive continuity (its legitimacy derives partly from textual transmission); participants in study benefit from access to canonical tradition. No enforcement overhead, no victim set, no suppression. This is coordination: how does one preserve a complex textual and legal tradition for future generations?
constraint_indexing:constraint_classification(symbolic_archive_reading, rope,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: ANALYTICAL OBSERVER (ROPE) — From the civilizational analytical frame, sacrifice law under the symbolic archive reading functions as pure coordination: preserving a complex legal and ethical tradition that has structured Jewish thought for two millennia. No binding obligation (extraction zero), no enforcement (suppression zero), no victims. The archive preserves identity and continuity for the Jewish collective and the interpretive tradition itself. Theater ratio near zero because the practice makes no false claim — study is transparently what it is (cultural preservation), not disguised obligation.
constraint_indexing:constraint_classification(symbolic_archive_reading, rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(symbolic_archive_reading_tests).
:- end_tests(symbolic_archive_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.0): This reading explicitly rejects obligation, so no extraction can occur. No agent is compelled; all participation is voluntary. The study of sacrifice law contributes to cultural continuity but creates no victims and no asymmetric distribution of costs and benefits. Suppression (0.0): No coercive mechanism exists. Agents may exit the practice at any time. Theater ratio (0.05, rising slightly to 0.05 over the interval): The practice is nearly completely non-performative — study is transparently what it is (cultural and intellectual engagement with a textual tradition), not disguised obligation or ritual substitute. The minimal rise reflects minor institutional ceremonialism in how the tradition is transmitted (scholarly conventions, textual canonization), but this is packaging rather than false claim. Claimed type (Rope): This is a pure coordination mechanism. The collective-action problem is genuine: if no one studies the tradition, it ceases to be available for future generations. The solution (voluntary study communities) is not suppressed, not extracted from, and not coercive. Multiple agents benefit (collective memory, interpretive tradition, individual scholars); no agent bears asymmetric cost.
 *
 * PERSPECTIVAL GAP:
 *   This reading produces minimal perspectival gap because all perspectives converge on the rope classification. The individual scholar sees coordination without coercion; the tradition community sees generational preservation without obligation; the authority structure sees institutional continuity without enforcement overhead; the analytical observer sees pure coordination. There is no target vs. beneficiary tension. The absence of perspectival gap is itself the reading's defining feature — it is the only reading where the obligation does not create an asymmetric structure. The other sibling readings (study-as-exercise, performance-only, messianic-suspension) all introduce obligation or asymmetry, creating perspectival gaps; this reading eliminates the gap by eliminating the obligation.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality analysis: beneficiaries_with_arbitrage_options (collective_memory, interpretive_tradition) receive low d values from the derivation chain (beneficiary status + arbitrage-level exit options = full beneficiary end of the spectrum). Moderate participants with mobile exit options (individual_scholars, tradition_community) receive mid-range d values reflecting symmetric exchange. The halakhic_authority has slightly higher d reflecting its institutional position, but still well below the midpoint because this reading assigns no significant extraction role to the authority. All d values remain constrained by the near-zero base ε: even an agent at maximum d (full target, d=1.0) would experience minimal chi because the underlying extractiveness is nearly zero. The symbolic archive reading produces no perspectival gap in experienced extraction because no perspective perceives themselves as bearing extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   SYMBOLIC ARCHIVE RESOLUTION: This reading resolves the sacrifice-obligation mandatrophy by reframing the mandate's outcome. The original mandate ('bring offerings at the Temple') has indeed become impossible to carry out. Rather than treating this as a violation requiring repair (obligation-suspension readings) or as a substitute-performance problem (exercise reading), the symbolic archive reading reformulates the mandate's function: the purpose shifts from literal performance to textual preservation and continuous ethical engagement. The mandate does not resolve (it is not 'completed'); it transforms. The textual tradition becomes the archive through which the sacrificial wisdom and ethical framework continue to inform Jewish life. Mandatrophy_resolved = false because the mandate's original form (literal performance) cannot be restored, but the mandate's function (preserving the tradition's resources) is being fulfilled through a different mechanism. This is not resolution but transformation — the constraint persists but the obligation structure it enforces has been reinterpreted out of existence.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reading_identity_definition,
    'Does ''symbolic archive reading'' maintain sufficient logical coherence as a distinct reading, or does it collapse into ''performance only reading'' under scrutiny?',
    'Textual evidence from halakhic authorities; documentation of explicit statements separating study-as-tradition from study-as-substitute-performance; comparative analysis of the two readings'' axioms and how they differ',
    'If coherent: symbolic archive reading remains a live reading with its own distinct ε and beneficiary structure. If collapses: the two readings are not genuinely distinguishable, and the kernel only has three readings, not four.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_identity_definition, conceptual, 'Whether symbolic archive reading is logically coherent or collapses into performance-only reading').

omega_variable(
    obligation_status_ambiguity,
    'Under the symbolic archive reading, is there a residual halachic obligation to study sacrifice law (on grounds of textual transmission and continuity), or is study purely optional cultural practice?',
    'Analysis of how the symbolic archive reading grounds its claim in halakhic sources; documentation of whether study obligation (Talmudic or rabbinic) is retained, reinterpreted, or explicitly suspended by this reading; comparison with how other readings handle the same obligation',
    'If study is obligatory: the reading carries mild suppression (obligation to engage) and beneficiaries collect from mandated continuity effort (ε shifts to ~0.05–0.10, possibly reclassifying to tangled_rope). If purely optional: zero suppression, zero obligation, pure coordination (ε remains 0.0, rope classification holds).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(obligation_status_ambiguity, empirical, 'Whether study obligation persists or is explicitly suspended under this reading').

omega_variable(
    kernel_codification_status,
    'How is the sacrifice obligation kernel itself codified in halakhic texts — is it a fixed legal requirement, a distributed interpretive matter, or an implicit background commitment?',
    'Textual archaeology: cataloging explicit statements of the obligation, its sources, and how different periods and authorities have treated its codification; analysis of whether the kernel has formal structure or emerges from practice',
    'If fixed: the kernel is a stable commitment that this reading interprets (formalized kernel, lineage authority grounding). If distributed: multiple interpretations coexist without arbitration (distributed kernel, requires reading_relations to clarify coexistence). If implicit: the obligation emerges from practice and is never formally stated (implicit kernel, practice authority grounding).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_codification_status, empirical, 'Codification status of the sacrifice obligation kernel itself').

omega_variable(
    messianic_interaction_structure,
    'What is the exact structural relationship between the symbolic archive reading and the messianic suspension reading — does one foreclose the other, or do they coexist as different frameworks for different moments?',
    'Historical-textual analysis of how messianic suspension has been defended; documentation of whether symbolic archive reading was invoked before messianic frameworks arose, or whether they developed in tandem; analysis of whether a contemporary interpreter can hold both simultaneously',
    'If forecloses: symbolic archive reading (study-as-cultural-practice) makes messianic suspension unnecessary — one reading rules out the other. If coexists: both remain live, with different eschatological framings. If influences: symbolic archive reading changes what messianic suspension has to defend against (creates pressure on the sibling''s legitimacy). Classification of reading_relations determines how the two stories relate in the network.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(messianic_interaction_structure, empirical, 'Structural relationship between symbolic archive and messianic suspension readings').

omega_variable(
    cultural_continuity_vulnerability,
    'Is the symbolic archive reading vulnerable to the objection that pure study-without-obligation will not sustain transmission across generations — that the practice requires at least mild obligation to persist?',
    'Historical evidence of voluntary cultural practices that have persisted (or failed to persist) across generations; comparative analysis with other traditions'' non-obligatory archives; evidence from communities that have explicitly adopted this reading about whether study actually continues',
    'If vulnerable: the reading''s beneficiary claim (collective memory preservation) may be unrealistic — study may require at least constrained (obligatory) participation to survive generationally, shifting the classification toward tangled_rope. If resilient: pure coordination can sustain archive transmission, and rope classification is realistic.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cultural_continuity_vulnerability, empirical, 'Whether pure voluntary study can sustain multi-generational transmission without obligation').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(symbolic_archive_reading, 0, 1000).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(symarch_tr_t0, symbolic_archive_reading, theater_ratio, 0, 0.02).
narrative_ontology:measurement(symarch_tr_t500, symbolic_archive_reading, theater_ratio, 500, 0.05).
narrative_ontology:measurement(symarch_tr_t1000, symbolic_archive_reading, theater_ratio, 1000, 0.05).

% Extraction over time
narrative_ontology:measurement(symarch_be_t0, symbolic_archive_reading, base_extractiveness, 0, 0.0).
narrative_ontology:measurement(symarch_be_t500, symbolic_archive_reading, base_extractiveness, 500, 0.0).
narrative_ontology:measurement(symarch_be_t1000, symbolic_archive_reading, base_extractiveness, 1000, 0.0).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(symbolic_archive_reading, identity_coordination).
narrative_ontology:affects_constraint(symbolic_archive_reading, study_as_exercise_reading).
narrative_ontology:affects_constraint(symbolic_archive_reading, performance_only_reading).
narrative_ontology:affects_constraint(symbolic_archive_reading, messianic_suspension_reading).

% DUAL FORMULATION NOTE:
% The symbolic_archive_reading is one of four structurally distinct readings of the sacrifice_obligation_kernel. Each reading assigns a different ε value and different victim/beneficiary structure to the same halakhic commitment. These four stories form a kernel family linked by network.affects_constraints. The symbolic_archive_reading has zero extractiveness because it explicitly rejects binding obligation; the siblings have non-zero ε values reflecting different obligation structures. Each story is a coherent reading of the kernel — none is 'wrong' — but they are empirically and structurally distinct constraints with different classification consequences.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
