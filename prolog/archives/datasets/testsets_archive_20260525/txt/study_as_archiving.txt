% ============================================================================
% CONSTRAINT STORY: study_as_archiving
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_study_as_archiving, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: study_as_archiving
 *   human_readable: Study as Archiving: Halakhic Preservation Without Performative Fulfillment
 *   domain: religious_studies/halakhic_authority/commitment_systems
 *
 * SUMMARY:
 *   This constraint models a specific reading of the contested kernel 'Temple
 *   sacrifice obligation': study as archiving. The constraint describes the
 *   binding halakhic obligation to perform Temple sacrifice, which became
 *   impossible after the Roman destruction in 70 CE. Rather than rescinding
 *   the obligation, halakhic authority maintained its binding status by
 *   reframing study of sacrificial law as a substitute for performance. This
 *   reading instantiates the 'study as archiving' interpretation — study
 *   preserves the obligation's textual content and intellectual continuity
 *   without claiming to fulfill the obligation itself. The constraint
 *   exhibits high extractiveness (0.52) because the observant community is
 *   bound to a command they cannot perform, while the halakhic authority
 *   structure benefits from preserving the obligation's binding status (which
 *   sustains interpretive authority and textual continuity). The measurement
 *   trajectory shows increasing theater ratio from 0.35 to 0.58 over the
 *   first millennium post-Temple, reflecting how the study-as-substitute
 *   framework evolved from emergency measure to elaborate institutional
 *   practice, with expanding interpretive apparatus compensating for the
 *   absence of actual sacrifice. The sibling readings (study_as_occupation
 *   and messianic_suspension) are separate constraints with different ε
 *   values and beneficiary structures; this story presents only the archiving
 *   reading.
 *
 * KEY AGENTS:
 *   - Observant Community: Primary victim (powerless/trapped) — bound by obligation to perform what is structurally impossible; no exit from the unfulfilled command
 *   - Temple Sacrifice Obligation: Primary victim (institutional/trapped) — the divine command itself, unfulfilled for two millennia, preserved in binding status without performance
 *   - Halakhic Authority Structure: Primary beneficiary (institutional/arbitrage) — maintains binding status of obligation, which sustains their interpretive authority and validates textual continuity across epochs
 *   - Interpretive Tradition: Secondary beneficiary (institutional/arbitrage) — study-as-substitute framework enables preservation of sacrifice knowledge, development of elaborate jurisprudence, and communal scholarly identity
 *   - Halakhic Interpreters: Secondary beneficiary (moderate/constrained) — benefit from authority and interpretive agency derived from maintaining the constraint, though also constrained by the requirement to explain non-performance
 *   - Messianic Restoration Movement: Organized actor (organized/constrained) — sees constraint as temporary with built-in sunset clause; views study as preparation pending literal restoration
 *   - Modernizing Communities: Powerful actor (powerful/mobile) — can exit traditional frameworks entirely; use study-as-substitute to maintain Jewish identity without theurgic burden
 *   - Post-Temple Institutional Framework: Institutional actor (institutional/arbitrage) — perceives own maintenance of obligation as increasingly performative (piton perspective); sustained by institutional inertia
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(study_as_archiving, 0.52).
domain_priors:suppression_score(study_as_archiving, 0.68).
domain_priors:theater_ratio(study_as_archiving, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(study_as_archiving, extractiveness, 0.52).
narrative_ontology:constraint_metric(study_as_archiving, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(study_as_archiving, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(study_as_archiving, tangled_rope).
narrative_ontology:human_readable(study_as_archiving, "Study as Archiving: Halakhic Preservation Without Performative Fulfillment").
narrative_ontology:topic_domain(study_as_archiving, "religious_studies/halakhic_authority/commitment_systems").

domain_priors:requires_active_enforcement(study_as_archiving).

% --- Commitment system structure ---
narrative_ontology:cs_kernel_codification(study_as_archiving, fixed_text).
narrative_ontology:cs_authority_grounding(study_as_archiving, lineage).
narrative_ontology:cs_interpretation_layer_present(study_as_archiving).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(study_as_archiving, halakhic_authority_structure).
narrative_ontology:constraint_beneficiary(study_as_archiving, interpretive_tradition).
narrative_ontology:constraint_victim(study_as_archiving, temple_sacrifice_obligation).
narrative_ontology:constraint_victim(study_as_archiving, observant_community).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: OBSERVANT COMMUNITY (SNARE) — Bound by halakhic obligation to perform Temple sacrifice; trapped in post-Temple context where performance is materially impossible. Bears the existential extraction of being commanded to do what cannot be done. No exit: obligation persists across 2000 years regardless of structural impossibility. Suppression is absolute — the very structure that enforces the law also prevents its fulfillment.
constraint_indexing:constraint_classification(study_as_archiving, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(universal))).

% PERSPECTIVE 2: HALAKHIC INTERPRETER (TANGLED ROPE) — Constrained by the binding authority of the sacrificial obligation and the structural impossibility of performance. But also benefits from the constraint: interpretive agency, textual authority, and communal leadership derive from maintaining the obligation's binding status while explaining its non-performance through the study-as-substitute framework. Mixed extraction and coordination — the interpreter must maintain the constraint to preserve their own authority.
constraint_indexing:constraint_classification(study_as_archiving, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: HALAKHIC AUTHORITY STRUCTURE (ROPE) — Benefits from the constraint as a coordination mechanism. Study as substitute for sacrifice preserves the interpretive tradition, maintains textual continuity, and enables halakhic authority to function across epochs without the Temple. The constraint coordinates knowledge preservation and ritual-less Judaism. Net beneficiary — experiences the constraint as enabling, not extractive.
constraint_indexing:constraint_classification(study_as_archiving, rope,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(universal))).

% PERSPECTIVE 4: MODERNIZING JEWISH COMMUNITY (TANGLED ROPE) — Mobile: can exit traditional halakhic frameworks entirely (assimilation, reform, secularism). But benefits from the study-as-substitute framework by maintaining Jewish identity and textual continuity without the theurgic burden of sacrifice. Moderate extraction: the constraint imposes interpretive burden (maintaining obligation + non-performance paradox) but offers identity coordination. Exit is possible but costly to community membership.
constraint_indexing:constraint_classification(study_as_archiving, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: MESSIANIC RESTORATION MOVEMENT (SCAFFOLD) — Organized agents viewing study as temporary substitute pending literal Temple reconstruction. See the constraint as having built-in sunset: when the Temple is restored (in messianic age), study reverts to preparation and the obligation becomes performable. Low effective extraction because this perspective has agency and sees an explicit exit path. Theater is low within this frame — study is genuine preparation, not performative substitution.
constraint_indexing:constraint_classification(study_as_archiving, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: POST-TEMPLE INSTITUTIONAL FRAMEWORK (PITON) — The halakhic system itself perceives the study-as-substitute framework as substantially performative ritual that maintains the appearance of continuity while the primary function (actual sacrifice) has atrophied. Theater ratio high (0.58): extensive interpretive apparatus and liturgical commemoration generate the experience of engagement with sacrifice without sacrifice. The framework sees its own maintenance of the obligation as inertial — kept alive because removing it would disrupt identity and authority, not because it is functionally necessary.
constraint_indexing:constraint_classification(study_as_archiving, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(universal))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From civilizational/universal scope, the binding persistence of unfulfilled obligations is an immutable feature of commitment-system logic: once a law is canonically established, it remains binding even if the conditions for performance vanish. The constraint appears as a law of textual authority itself. However, structural data reveals this as a false summit: the persistence of the obligation is contingent on institutional enforcement (halakhic authority, interpretive tradition, community bonds), not on logic or nature alone. The mountain framing naturalizes what is actually a constructed institutional arrangement.
constraint_indexing:constraint_classification(study_as_archiving, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(study_as_archiving_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(study_as_archiving, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(study_as_archiving, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(study_as_archiving, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(study_as_archiving, TR),
    TR >= 0.70.

:- end_tests(study_as_archiving_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The observant community is bound to a command whose primary performance condition (Temple existence) has been absent for two millennia. This is extraction — the binding force persists despite structural impossibility of fulfillment. However, the extractiveness is not maximal (would be 0.70+) because the study-as-substitute framework does provide a legitimate coordination function: it enables Judaism to persist without theurgic practice, preserves knowledge, and offers intellectual engagement with the obligation. The extraction is genuine but mixed with coordination benefit. Suppression (0.68): High. Observant Jews cannot exit the obligation without exiting Jewish identity. The halakhic framework permits no rescission of the binding status. Alternative framings (Reform Judaism's abandonment of the obligation, secular rejection, heterodox movements) are institutionally marginalized. Barriers to reinterpreting the obligation are epistemic and identity-based (interpreting the obligation away would require abandoning the framework that constitutes observant Jewish identity) and structural (institutional halakhic authority maintains binding status). Theater ratio (0.58): Moderate-high and increasing. Initially (0.35 at 70 CE), the framework was minimal — remembrance and minimal study. Over centuries, the apparatus expanded: daily Talmudic study, liturgical sacrificial commemoration (musaf service), detailed legal analysis of sacrifice conditions, piyyutim (liturgical poetry) enacting sacrifice symbolically. The increase in theater reflects Goodhart drift — as the primary function (actual sacrifice) remained impossible, the proxy function (study, commemoration, symbolic reenactment) elaborated to fill the experiential gap. Modern observers perceive this as ritual theater more clearly than medieval practitioners did; the ratio measures this objective elaboration.
 *
 * PERSPECTIVAL GAP:
 *   The observant community at biological timescale experiences Snare — they are trapped in an unfulfilled obligation with no exit short of identity dissolution. The halakhic authority at civilizational scope experiences Rope — the constraint enables knowledge preservation and institutional continuity. The modernizing community experiences Tangled Rope — they benefit from identity coordination but bear the interpretive burden of maintaining an unperformable obligation. The messianic movement experiences Scaffold — they see explicit sunset logic (restoration pending). The piton perspective (institutional framework observing itself) sees degraded ritual — the apparatus is maintained through inertia. The analytical observer at civilizational scope risks seeing Mountain (natural law of commitment systems) until the false summit signature fires, revealing that the obligation's persistence is contingent on institutional enforcement, not intrinsic to logic or nature.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is determined by the agent's structural position relative to the obligation's binding force. The observant community (trapped powerless victims) experiences d ≈ 0.95 — they are the target of the extraction, bearing the cost of unfulfilled obligation. The halakhic authority (institutional beneficiary) experiences d ≈ 0.10 — they benefit from maintaining the obligation, which sustains their interpretive function. The measurable gap (d_victim − d_beneficiary ≈ 0.85) is the hallmark of asymmetric extraction. The study-as-substitute framework itself is the mechanism that enables this extraction: by proposing that study fulfills the obligation intellectually, the authority reduces perceived violation of the binding command while preventing actual performance (which would require Temple reconstruction, which would disrupt rabbinic authority's control over halakhic interpretation). The framework preserves the obligation's binding status while ensuring its non-performance — a perfect extractive mechanism disguised as coordination.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by showing that the study-as-archiving reading is genuinely Tangled Rope, not pure Snare or pure Rope. The constraint coordinates knowledge preservation and textual continuity (rope function) while extracting from the observant community through binding them to an unperformable obligation (snare extraction). The mandate that could have triggered the mandatrophy paradox — 'is this really coordination, or is it just extraction masquerading as coordination?' — is resolved by acknowledging both dimensions simultaneously. The interpretive tradition genuinely benefits from the constraint's coordination function (knowledge preservation). The observant community genuinely bears extraction (unfulfilled binding obligation). Both are true. The constraint is not pure coordination rationalized as extraction; it is hybrid coordination-extraction with an asymmetric victim set. This is exactly what Tangled Rope models.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    study_as_substitute_ambiguity,
    'Does study genuinely fulfill the sacrifice obligation, or does it merely archive the obligation''s content while deferring actual fulfillment?',
    'Textual analysis of halakhic sources: do medieval and modern authorities claim study IS fulfillment (identity) or study SUBSTITUTES FOR fulfillment (proxy)? Longitudinal analysis of rhetoric across periods — does substitution language persist or does interpretation drift toward literal fulfillment claims?',
    'If identity: constraint is Rope (coordination of knowledge preservation with legitimate function). If proxy: constraint is Tangled Rope (preservation + extraction of unfulfilled obligation). This reading (study_as_archiving) presumes proxy; a ''study_as_fulfillment'' reading would produce lower extractiveness.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(study_as_substitute_ambiguity, conceptual, 'Whether study is substitute-for or surrogate-of the sacrifice obligation').

omega_variable(
    obligation_binding_persistence,
    'What grounds the continued binding force of an obligation whose primary performance condition (Temple existence) has been absent for two millennia?',
    'Philosophical analysis of binding authority in commitment systems: is binding force intrinsic (textual authority is self-perpetuating) or extrinsic (community enforcement and interpretive tradition sustain it)? Comparative analysis of other unfulfilled obligations in rabbinic law — are all preserved equally or do some degrade (piton)?',
    'If intrinsic: mountain classification is appropriate — binding force is immutable. If extrinsic: the mountain perspective is a false summit — institutional enforcement is contingent and could theoretically cease. This reading assumes extrinsic grounding.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(obligation_binding_persistence, conceptual, 'Source of continued binding force of unfulfilled obligations').

omega_variable(
    messianic_suspension_boundaries,
    'Does the study-as-archiving reading logically entail messianic restoration (sibling reading: messianic_suspension), or are they distinct constraint framings?',
    'Textual analysis: do halakhic sources treating study as substitute also explicitly condition that status on messianic restoration? Are there sources maintaining perpetual substitution independent of restoration hope?',
    'If entailed: study_as_archiving and messianic_suspension are coupled constraints (linked in network.affects_constraints). If distinct: they represent genuinely alternative readings of the same kernel (Temple sacrifice obligation) with different ε values and beneficiary structures. This affects how the constraint family is decomposed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(messianic_suspension_boundaries, conceptual, 'Whether study-as-archiving entails messianic restoration or functions as independent constraint').

omega_variable(
    kernel_reading_selection,
    'This story instantiates the ''study_as_archiving'' reading. What structural signals indicated this reading rather than ''study_as_occupation'' or ''messianic_suspension''?',
    'Explicit citation of source material and interpretive tradition. The study_as_archiving reading emphasizes preservation without fulfillment, maintenance of binding status despite non-performance, and the extractiveness of enforcing unfulfilled obligations. This contrasts with study_as_occupation (study as autonomous good, not dependent on sacrifice) and messianic_suspension (study as temporary pending restoration).',
    'Different readings produce different ε values, beneficiary sets, and victim identifications. This omega documents that the constraint story represents one coherent reading of the contested kernel, not an average or hybrid. Sibling readings are separate constraint files.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_selection, conceptual, 'Kernel reading selection rationale for study_as_archiving').

omega_variable(
    historical_displacement_timing,
    'Did the study-as-substitute framework emerge immediately after 70 CE with Temple destruction, or did it develop gradually? Does timing affect whether it is coordinate solution vs. post-hoc rationalization?',
    'Historical analysis of rabbinic sources across centuries: when do earliest sources explicitly invoke study as substitute? Is there evidence of alternative frameworks being displaced? Does rhetoric shift from temporary emergency measure to permanent principle?',
    'If immediate/coordinated response: the framework is more legitimate as solution to genuine coordination problem. If gradual/retroactive: the framework appears more as rationalization for institutional persistence, raising extractiveness. Theater ratio may be historical artifact rather than current structural feature.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(historical_displacement_timing, empirical, 'Historical emergence and development of study-as-substitute framework').

omega_variable(
    community_consent_vs_enforcement,
    'Is the continued binding status of the sacrifice obligation maintained through active community consent (coordination) or through institutional enforcement against potential resistance (extraction)?',
    'Ethnographic and historical analysis: do observant communities affirm the study-as-substitute framework as satisfactory, or express residual obligation/guilt for non-performance? Is there evidence of institutional suppression of alternative frameworks (Reform, secular, heterodox movements)?',
    'High consent: suppression value should be lower, constraint shifts toward Rope. High institutional enforcement: suppression value correct at 0.68, supports Tangled Rope. Community voices silenced or marginalized by authority structure suggest extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(community_consent_vs_enforcement, empirical, 'Whether obligation persistence is consensual coordination or enforced institutional arrangement').

omega_variable(
    false_summit_detection,
    'Is this constraint a genuine natural law of commitment-system logic (mountain), or does the mountain perspective naturalize a contingent institutional arrangement?',
    'Logical analysis: do all commitment systems necessarily persist in binding unfulfilled obligations indefinitely? Are there comparative cases (Christian Eucharist as ongoing sacrifice vs. one-time fulfillment; Islamic ritual as performable vs. deferred)? Can unfulfilled obligations be explicitly rescinded?',
    'If genuinely natural: mountain classification stands. If naturalized contingency: engine''s false summit detector flags the constraint as reclassifiable to Tangled Rope or Snare. This omega documents the vulnerability of the analytical perspective to naturalizing institutional arrangements.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(false_summit_detection, conceptual, 'Whether natural law mountain perspective naturalizes contingent institutional arrangement').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(study_as_archiving, 0, 1000).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(study_arch_tr_t0, study_as_archiving, theater_ratio, 0, 0.35).
narrative_ontology:measurement(study_arch_tr_t500, study_as_archiving, theater_ratio, 500, 0.48).
narrative_ontology:measurement(study_arch_tr_t1000, study_as_archiving, theater_ratio, 1000, 0.58).

% Extraction over time
narrative_ontology:measurement(study_arch_be_t0, study_as_archiving, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(study_arch_be_t500, study_as_archiving, base_extractiveness, 500, 0.47).
narrative_ontology:measurement(study_arch_be_t1000, study_as_archiving, base_extractiveness, 1000, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(study_as_archiving, identity_coordination).
narrative_ontology:affects_constraint(study_as_archiving, study_as_occupation).
narrative_ontology:affects_constraint(study_as_archiving, messianic_suspension).
narrative_ontology:affects_constraint(study_as_archiving, temple_sacrifice_obligation).

% DUAL FORMULATION NOTE:
% The temple_sacrifice_obligation kernel admits three distinct constraint readings with different ε values and beneficiary structures. study_as_archiving (this file, ε=0.52, Tangled Rope) models the dominant institutional reading where study preserves the obligation without fulfilling it. study_as_occupation (sibling, ε≈0.30, Rope) treats study as autonomous positive commandment. messianic_suspension (sibling, ε≈0.45, Scaffold) suspends the obligation pending literal restoration. Each reading is a complete constraint with its own perspectives, metrics, and victim set. Network links establish family membership; the kernel itself is modeled in a separate parent constraint file.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(study_as_archiving, institutional, 0.1).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
