% ============================================================================
% CONSTRAINT STORY: temple_sacrifice_obligation__study_as_occupation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_temple_sacrifice_obligation__study_as_occupation, []).

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
 *   constraint_id: temple_sacrifice_obligation__study_as_occupation
 *   human_readable: Study of Sacrifice Law as Legitimate Occupation of Obligation (Temple Absent)
 *   domain: religious_studies/halakhic_authority/commitment_systems
 *
 * SUMMARY:
 *   This constraint instantiates ONE READING of the contested kernel of
 *   Temple sacrifice obligation in diaspora Judaism. The kernel is a
 *   formalized, authoritative commitment (mishnaic and talmudic law
 *   specifying obligatory sacrifice acts in the Temple) that became
 *   impossible to perform after the Temple's destruction in 70 CE. Three
 *   distinct readings have emerged: (1) study_as_occupation — study of
 *   sacrifice law constitutes legitimate fulfillment of the obligation; (2)
 *   study_as_archiving — study preserves knowledge for future restoration but
 *   does not fulfill the obligation; (3) messianic_suspension — the
 *   obligation is suspended (neither fulfilled nor violated) pending
 *   messianic restoration and Temple rebuilding. This story models reading
 *   (1): study-as-occupation. The constraint is a pure coordination mechanism
 *   with minimal extraction. The rabbinic authority declares that intensive
 *   engagement with sacrifice law (textual analysis, legal reasoning,
 *   creative interpretation) constitutes the legitimate form of
 *   obligation-bearing when Temple performance is impossible. This reading
 *   coordinates the diaspora Jewish community around a hermeneutical practice
 *   that honors the original obligation structure without requiring ritual
 *   performance. The authority structure absorbs the impossibility of
 *   performance (Temple destroyed, return impossible in the immediate
 *   historical frame) without surfacing a revision of the obligation itself —
 *   the obligation is not canceled but transformed into a textual and
 *   intellectual form.
 *
 * KEY AGENTS:
 *   - Rabbinic Authority: Institutional actor (institutional/arbitrage) — declares study as legitimate occupation; benefits from the authority and legitimacy that accrues to a hermeneutical innovation that solves a real coordination problem
 *   - Observant Community: Moderate agents (moderate/mobile at civilizational scale; moderate/constrained at biographical scale) — participate in collective hermeneutical practice; identity-constituted by the obligation-via-study frame
 *   - Individual Practitioner: Moderate agents at biographical scale (moderate/constrained, identity_locked) — experience the obligation as identity-binding and somewhat extractive (community enforcement, social cost); genuine obligation-bearing but with asymmetric enforcement
 *   - Analytical Observer: Civilizational view (analytical/analytical) — sees a pure coordination solution: the reading uses hermeneutical innovation to maintain obligation-bearing when the original obligatory act becomes impossible
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(temple_sacrifice_obligation__study_as_occupation, 0.12).
domain_priors:suppression_score(temple_sacrifice_obligation__study_as_occupation, 0.08).
domain_priors:theater_ratio(temple_sacrifice_obligation__study_as_occupation, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(temple_sacrifice_obligation__study_as_occupation, extractiveness, 0.12).
narrative_ontology:constraint_metric(temple_sacrifice_obligation__study_as_occupation, suppression_requirement, 0.08).
narrative_ontology:constraint_metric(temple_sacrifice_obligation__study_as_occupation, theater_ratio, 0.25).

% --- Constraint claim ---
narrative_ontology:constraint_claim(temple_sacrifice_obligation__study_as_occupation, rope).
narrative_ontology:human_readable(temple_sacrifice_obligation__study_as_occupation, "Study of Sacrifice Law as Legitimate Occupation of Obligation (Temple Absent)").
narrative_ontology:topic_domain(temple_sacrifice_obligation__study_as_occupation, "religious_studies/halakhic_authority/commitment_systems").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(temple_sacrifice_obligation__study_as_occupation, 'a9f9830e-706c-4b8b-8482-4ff971a2b588').
narrative_ontology:cs_kernel_codification('a9f9830e-706c-4b8b-8482-4ff971a2b588', formalized).
narrative_ontology:cs_authority_grounding('a9f9830e-706c-4b8b-8482-4ff971a2b588', lineage).
narrative_ontology:cs_interpretation_layer_present('a9f9830e-706c-4b8b-8482-4ff971a2b588').
narrative_ontology:cs_reading_relation('a9f9830e-706c-4b8b-8482-4ff971a2b588', temple_sacrifice_obligation__study_as_archiving, forecloses).
narrative_ontology:cs_reading_relation('a9f9830e-706c-4b8b-8482-4ff971a2b588', temple_sacrifice_obligation__messianic_suspension, forecloses).
narrative_ontology:cs_axiom('a9f9830e-706c-4b8b-8482-4ff971a2b588', foundational, study_constitutively_fulfills_obligation).
narrative_ontology:cs_axiom_status(study_constitutively_fulfills_obligation, holdable).
narrative_ontology:cs_axiom_grounding('a9f9830e-706c-4b8b-8482-4ff971a2b588', study_constitutively_fulfills_obligation, conventional).
narrative_ontology:cs_axiom('a9f9830e-706c-4b8b-8482-4ff971a2b588', foundational, obligation_transformed_not_suspended).
narrative_ontology:cs_axiom_status(obligation_transformed_not_suspended, holdable).
narrative_ontology:cs_axiom_grounding('a9f9830e-706c-4b8b-8482-4ff971a2b588', obligation_transformed_not_suspended, deontological).
narrative_ontology:cs_reference_frame('a9f9830e-706c-4b8b-8482-4ff971a2b588', diaspora_obligation_through_hermeneutical_transformation).
narrative_ontology:cs_drift_state('a9f9830e-706c-4b8b-8482-4ff971a2b588', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('a9f9830e-706c-4b8b-8482-4ff971a2b588', '2026-02-26T14:32:18Z').
narrative_ontology:cs_kernel_id(temple_sacrifice_obligation__study_as_occupation, temple_sacrifice_obligation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(temple_sacrifice_obligation__study_as_occupation, talmudic_interpretive_community).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: RABBINIC AUTHORITY (ROPE) — The institutional interpreters of Jewish law declare that intensive study of sacrifice law constitutes legitimate fulfillment of the obligation when Temple performance is impossible. This is a pure coordination mechanism: the community coordinates its obligation-bearing through collective intellectual engagement rather than ritual performance. Beneficiary of the reading — the interpretive community's authority and legitimacy rest on this framework. Low extraction, minimal suppression, minimal theater. The ruling coordinates a scattered diaspora community around a shared hermeneutical practice.
constraint_indexing:constraint_classification(temple_sacrifice_obligation__study_as_occupation, rope,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 2: OBSERVANT COMMUNITY (ROPE) — Individual Jews practicing diaspora Judaism experience the obligation as genuinely coordinated through study. They participate in a collective hermeneutical practice that honors the original obligation structure without requiring Temple performance. Experiences low extraction — the study requirement is onerous but not coercive; engagement is volitional and identity-constituting. Exit is mobile (one can leave the community, the practice, or the faith) but the frame makes exit unthinkable from within. Theater is minimal — the study engages real textual and legal complexity.
constraint_indexing:constraint_classification(temple_sacrifice_obligation__study_as_occupation, rope,
    context(agent_power(moderate),
            time_horizon(civilizational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 3: INDIVIDUAL OBSERVANT PRACTITIONER (TANGLED ROPE) — An individual embedded in the community who experiences genuine obligation-bearing through study but also experiences the constraint as partially extractive: the community enforces study norms, there are career/social costs to non-participation, and the obligation structure subordinates other obligations or pursuits. Exit is constrained (cost is high: social estrangement, identity dissolution, loss of community). Some coordination benefit (shared hermeneutical practice, intellectual engagement) paired with asymmetric extraction (enforcement of norms, social cost).
constraint_indexing:constraint_classification(temple_sacrifice_obligation__study_as_occupation, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 4: ANALYTICAL OBSERVER (ROPE) — From a civilizational/universal view, this constraint is a pure coordination mechanism: the rabbinic authority has solved a real coordination problem (how to maintain obligation-bearing when the original obligatory act is impossible) through a hermeneutical innovation. The reading coordinates the community around the principle that intensive engagement with law constitutes legitimate fulfillment. Low extractiveness because the mechanism serves the declared coordination goal. The theater is minimal — the study genuinely engages the complexity of sacrifice law. The constraint is a structural solution, not a cover story.
constraint_indexing:constraint_classification(temple_sacrifice_obligation__study_as_occupation, rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(temple_sacrifice_obligation__study_as_occupation_tests).
:- end_tests(temple_sacrifice_obligation__study_as_occupation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.12): Minimal. The constraint is fundamentally a coordination mechanism — the rabbinic authority has solved a real coordination problem through hermeneutical innovation. The reading benefits the interpretive community's authority and legitimacy, but this is a legitimate institutional benefit (they solved a problem), not extractive overhead. Extractiveness is low because the mechanism serves the declared goal: maintaining obligation-bearing when the original obligatory act is impossible. The study requirement is genuinely what the obligation has become; there is no hidden mechanism extracting from the community for an external beneficiary. Suppression (0.08): Minimal. There are community norms enforcing study (social pressure, reputation), but suppression is low because the frame makes suppression nearly invisible — from within the frame, study is not coerced but self-evident. The obligation-bearing community accepts the study requirement as the legitimate form of the obligation. Suppression appears only from a biographical, constrained perspective (PERSPECTIVE 3). Theater ratio (0.25): Low-minimal. The textual study engages real legal and hermeneutical complexity. The interpretation is not a performative substitute but a genuine intellectual practice. Theater is minimal because the study requirement is what it claims to be: engagement with legal texts and their implications. The constraint is stable across the 1000-year measurement interval because the reading has been hegemonic in Jewish practice for nearly 2000 years and shows no sign of degradation. The slight rise in theater_ratio (0.20 → 0.25) reflects increasing formalization and ritual-ization of study practices (yeshiva structures, prayer-integrated study, canonical commentaries) but this is not piton-level theater — it is the natural elaboration of a stable coordination mechanism.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap is minimal in this constraint because all agents experience the coordination mechanism in roughly the same way. The rabbinic authority and the observant community both see this as a legitimate solution (Rope). The analytical observer also sees it as a pure coordination solution (Rope). The individual practitioner at biographical scale experiences some extraction (constrained exit, community enforcement) and thus sees Tangled Rope — but this is a secondary effect of identity-locking and community norms, not a structural feature of the reading itself. The gap exists primarily between this reading and its siblings: study-as-occupation sees low extractiveness; study-as-archiving would see higher theater (study is a substitute, not a fulfillment); messianic_suspension would see the obligation as deferred, not occupied. The within-reading perspectival gap is small because the reading has achieved hermeneutical consensus across institutional and individual scales.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is low across all perspectives because the constraint is fundamentally a coordination mechanism with minimal extraction. The rabbinic authority is the beneficiary (d ≈ 0.15) — they benefit from the innovation and the authority it confers, but this is legitimate institutional benefit, not extractive overhead. Derived from: institutional power + arbitrage exit + beneficiary status. The observant community is both beneficiary and participant (d ≈ 0.35) — they benefit from a coordination solution that enables diaspora Judaism to persist, but also bear the cost of the study requirement. Derived from: moderate power + mobile exit (civilizational) + beneficiary status. The individual practitioner at biographical scale has higher d (≈ 0.55) — constrained exit + some victim status (enforcement of norms, social cost) paired with genuine benefit (identity constitution, obligation fulfillment). Derived from: moderate power + constrained exit + mixed beneficiary/victim. The analytical observer (d ≈ 0.72) sees the structure clearly and notes the innovation's elegance — no override needed.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint avoids mandatrophy because it instantiates a pure coordination mechanism that has achieved hermeneutical consensus. There is no hidden extraction masquerading as coordination, and no hidden coordination masquerading as extraction. The reading declares that study IS the obligation, and the community has accepted this declaration for nearly 2000 years. The reading works because: (1) it preserves the obligation structure (not canceling or suspending it), (2) it makes the obligation performable in diaspora (transforming it into textual-legal engagement), (3) it provides intellectual and spiritual fulfillment (genuine engagement with legal complexity). The theater is minimal because the study requirement is what it claims to be. No mandatrophy resolution is needed.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_interpretive_closure,
    'Does the rabbinic declaration that ''study occupies the obligation'' close the kernel (make revision of the obligation structure unnecessary) or merely defer revision until messianic restoration?',
    'Analysis of rabbinic textual debate: do later authorities dispute whether study truly fulfills the obligation or do they extend/refine the same principle? Does any authority claim the obligation remains unfulfilled despite study?',
    'If closure: study_as_occupation reading is stable and the obligation structure is fully absorbed into the interpretive framework. If deferral: the obligation remains formally unresolve and study is a temporary substitute, shifting classification toward piton or tangled_rope. If disputed: the kernel remains genuinely contested across rabbinic sources.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_interpretive_closure, empirical, 'Whether study closes the obligation or defers it').

omega_variable(
    study_intensity_threshold,
    'What level of engagement with sacrifice law constitutes ''legitimate occupation''? Is this threshold well-defined in rabbinic sources or ambiguous?',
    'Textual analysis of rabbinic responsa: how much study is required? Are there disputes about sufficiency? Do authorities differentiate between casual study and intensive engagement?',
    'If well-defined: the reading has clear enforcement criteria (not piton-theater). If ambiguous: the reading relies on interpretive discretion and becomes more theater-heavy (shift toward piton). If radically demanding: the reading becomes more extractive (shift toward tangled_rope).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(study_intensity_threshold, empirical, 'Whether the sufficiency threshold for study is well-defined').

omega_variable(
    reading_instantiation_scope,
    'Is this reading (study as occupation) the hegemonic interpretation in contemporary Jewish practice, or is it one competing reading among the three siblings?',
    'Survey of contemporary Jewish communities: which reading do they adopt or combine? Do different communities hold different readings?',
    'If hegemonic: the constraint''s classification from institutional perspective is Rope (coordinating framework). If sectarian: the reading has lower authority and the classification may shift to piton (theater-maintained interpretation). If syncretic: multiple readings coexist, affecting how the kernel-reading structure is modeled.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_instantiation_scope, empirical, 'Whether study-as-occupation is hegemonic or contested').

omega_variable(
    sibling_reading_coexistence,
    'Can an observant Jew simultaneously hold that study occupies the obligation AND that the obligation remains suspended pending restoration AND that study merely archives for future performance?',
    'Textual analysis of sources that combine two or more of the three readings. Analysis of how different streams (Orthodoxy, Conservative, Reconstructionism) position the readings relative to each other.',
    'If logically compatible: reading_relations should be coexists_with (different parties hold different readings within a single tradition). If mutually exclusive: forecloses relations may be more accurate. If layered (study both occupies AND defers): the kernel itself may be more complex than the three-reading decomposition suggests.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_reading_coexistence, conceptual, 'Whether the three readings are logically compatible').

omega_variable(
    authority_structure_revision_visibility,
    'If study is the legitimate occupation, why do rabbinic sources continue to discuss the Temple''s destruction as a catastrophe and the obligation as suspended/unfulfilled? Is this discourse about a genuinely unresolved obligation, or is it performative recollection?',
    'Textual analysis: do authorities treat the obligation-via-study as settled or disputed? Is there ongoing argument about sufficiency? Do they mourn the impossibility of Temple sacrifice while declaring study sufficient?',
    'If genuinely unresolved: the authority structure has NOT absorbed the impossibility, and the classification shifts toward piton (maintaining theater around an unresolved obligation). If settled: the reading is stable. If mourning-without-resolution is itself the obligation (obligation to remember and lament): the reading instantiates a different obligation structure entirely.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(authority_structure_revision_visibility, conceptual, 'Whether study-as-occupation resolves or defers the obligation').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(temple_sacrifice_obligation__study_as_occupation, 0, 1000).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tso_tr_t0, temple_sacrifice_obligation__study_as_occupation, theater_ratio, 0, 0.2).
narrative_ontology:measurement(tso_tr_t1000, temple_sacrifice_obligation__study_as_occupation, theater_ratio, 1000, 0.25).

% Extraction over time
narrative_ontology:measurement(tso_be_t0, temple_sacrifice_obligation__study_as_occupation, base_extractiveness, 0, 0.1).
narrative_ontology:measurement(tso_be_t500, temple_sacrifice_obligation__study_as_occupation, base_extractiveness, 500, 0.12).
narrative_ontology:measurement(tso_be_t1000, temple_sacrifice_obligation__study_as_occupation, base_extractiveness, 1000, 0.12).

% Suppression requirement over time
narrative_ontology:measurement(tso_su_t0, temple_sacrifice_obligation__study_as_occupation, suppression_requirement, 0, 0.08).
narrative_ontology:measurement(tso_su_t500, temple_sacrifice_obligation__study_as_occupation, suppression_requirement, 500, 0.08).
narrative_ontology:measurement(tso_su_t1000, temple_sacrifice_obligation__study_as_occupation, suppression_requirement, 1000, 0.08).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(temple_sacrifice_obligation__study_as_occupation, identity_coordination).
narrative_ontology:affects_constraint(temple_sacrifice_obligation__study_as_occupation, temple_sacrifice_obligation__study_as_archiving).
narrative_ontology:affects_constraint(temple_sacrifice_obligation__study_as_occupation, temple_sacrifice_obligation__messianic_suspension).

% DUAL FORMULATION NOTE:
% The Temple sacrifice obligation kernel has three structurally distinct readings with different ε values and different authority structures. Study-as-occupation (this story) treats study as fulfillment (ε=0.12, Rope). Study-as-archiving treats study as preservation without fulfillment (ε≈0.35, likely Tangled Rope or Piton). Messianic_suspension treats the obligation as deferred (ε≈0.25, likely Piton). Each reading has its own perspectives and its own classification. They are linked as constraint family via affects_constraints network edges.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
