% ============================================================================
% CONSTRAINT STORY: continuity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_continuity_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: continuity_reading
 *   human_readable: Medieval Latin as Continuous Drift from Classical Latin (Continuity Reading)
 *   domain: historical_linguistics/philology/commitment_systems
 *
 * SUMMARY:
 *   Medieval Latin presents a persistent linguistic phenomenon: observable
 *   sound changes, morphological simplifications, and syntactic shifts from
 *   the Classical period, yet practitioners (clerics, scribes,
 *   administrators) frame their activity as maintaining a continuous Latin
 *   tradition. The continuity reading models this phenomenon as natural drift
 *   within a self-regulating linguistic system. Correction occurs through
 *   practitioners' internalization of evolving norms, not through external
 *   enforcement or a rupture that would initiate a new system. The constraint
 *   is the commitment to this framing — a commitment that grounds
 *   ecclesiastical and scribal authority as keepers of a living tradition
 *   rather than managers of a linguistic break. The extractiveness is low
 *   because the coordination function is genuine: maintaining Latin as a
 *   cross-regional administrative and religious lingua franca requires norm
 *   convergence, and practitioners benefit from knowing that their competence
 *   remains valid across generations. Suppression is minimal because the
 *   framing does not require practitioners to deny observed change — it
 *   integrates change into the system. Theater is low because the analytical
 *   content (drift correction via internal norm absorption) describes actual
 *   linguistic process, not performative ritual.
 *
 * KEY AGENTS:
 *   - Medieval Practitioners (scribes, clerics, administrators): Primary beneficiary (moderate/constrained) — their existing competence in Classical grammar remains valid under the continuity frame; they experience themselves as maintaining rather than breaking tradition.
 *   - Ecclesiastical Authority (monastery scriptoria, cathedral schools): Primary beneficiary (institutional/arbitrage) — legitimacy derives from guardianship of a living classical tradition; no costly enforcement required because practitioners self-regulate through normative absorption.
 *   - Analytical Observer (modern linguistics, philology): Tertiary actor (analytical/analytical) — the continuity reading is one coherent analytical frame; it describes genuine linguistic process but risks invisibilizing practitioners who bear costs during norm shifts.
 *   - Alternative Reading Community (rupture scholars): Implicit sibling reader (analytical/analytical) — represents the rupture reading, which frames the same data as evidence of linguistic discontinuity and new-system initiation.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(continuity_reading, 0.18).
domain_priors:suppression_score(continuity_reading, 0.12).
domain_priors:theater_ratio(continuity_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(continuity_reading, extractiveness, 0.18).
narrative_ontology:constraint_metric(continuity_reading, suppression_requirement, 0.12).
narrative_ontology:constraint_metric(continuity_reading, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(continuity_reading, rope).
narrative_ontology:human_readable(continuity_reading, "Medieval Latin as Continuous Drift from Classical Latin (Continuity Reading)").
narrative_ontology:topic_domain(continuity_reading, "historical_linguistics/philology/commitment_systems").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(continuity_reading, '2861ca50-4022-4f8c-98f3-1987e025bff8').
narrative_ontology:cs_created_at('2861ca50-4022-4f8c-98f3-1987e025bff8', '').
narrative_ontology:cs_kernel_codification('2861ca50-4022-4f8c-98f3-1987e025bff8', distributed).
narrative_ontology:cs_authority_grounding('2861ca50-4022-4f8c-98f3-1987e025bff8', lineage).
narrative_ontology:cs_interpretation_layer_present('2861ca50-4022-4f8c-98f3-1987e025bff8').
narrative_ontology:cs_kernel_id(continuity_reading, correct_latin).
narrative_ontology:cs_reading_relation('2861ca50-4022-4f8c-98f3-1987e025bff8', rupture_reading, coexists_with).
narrative_ontology:cs_axiom('2861ca50-4022-4f8c-98f3-1987e025bff8', foundational, latin_constitutes_single_evolving_system).
narrative_ontology:cs_axiom_status(latin_constitutes_single_evolving_system, holdable).
narrative_ontology:cs_axiom('2861ca50-4022-4f8c-98f3-1987e025bff8', foundational, correction_occurs_via_internal_norm_absorption).
narrative_ontology:cs_axiom_status(correction_occurs_via_internal_norm_absorption, holdable).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(continuity_reading, medieval_practitioners).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: MEDIEVAL PRACTITIONER (ROPE) — Continuity reading legitimates their existing competence. They are applying Classical grammatical rules to evolving speech, experiencing this as skillful coordination within a living system. Suppression minimal because the constraint does not require denying observed change — it integrates change into the system. Exits constrained by training investment and professional role, but no coercion to maintain the framing.
constraint_indexing:constraint_classification(continuity_reading, rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 2: ECCLESIASTICAL AUTHORITY (ROPE) — Benefits from continuity framing: it legitimates their role as keepers of a living classical tradition rather than managers of a linguistic rupture. No costly enforcement burden — practitioners self-enforce through normative absorption. Authority can arbitrage between prescriptive grammar and actual usage, adjusting standards as needed. Low theater because the coordination function is genuine: maintaining Latin as a cross-regional language of religious and administrative communication.
constraint_indexing:constraint_classification(continuity_reading, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 3: ANALYTICAL OBSERVER / CONTINUITY FRAME (ROPE) — From a reconstructionist perspective that models language as a continuous system, medieval Latin is Classical Latin observed through the lens of natural sound change, analogy, and semantic shift. The 'correction' occurs within the system itself: practitioners recognize deviations and apply conscious standardization as internal norm maintenance. No external enforcement gate — the constraint is the system's own self-regulating mechanism. Theater ratio low because the analytical frame describes genuine linguistic process, not performative ritual.
constraint_indexing:constraint_classification(continuity_reading, rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(continuity_reading_tests).
:- end_tests(continuity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.18): Low. The continuity reading posits that medieval practitioners benefit from being incorporated into a continuous tradition rather than facing exclusion from a new standard. The extraction that does occur is minimal coordination overhead — practitioners internalize evolving norms without external coercion. The measurement trajectory (0.10 → 0.22 over 800 years) shows slight accumulation as standards become more elaborate and correction more explicit, but the absolute level remains low. Suppression (0.12): Low. The continuity frame does not require practitioners to deny observed linguistic change. They can see drift and still frame their activity as maintenance of a living system. Suppression is minimal because the constraint is compatible with perceptual reality — practitioners can simultaneously observe changes and frame themselves as maintaining continuity. Theater ratio (0.35): Low-moderate. The continuity reading has genuine explanatory content (natural drift, internal norm maintenance) but includes performative elements. The presentation of correction as 'maintaining' rather than 'imposing' has a framing function that smooths over authority struggles. As standards became more elaborate and explicit (400-800 years), the theater ratio rises slightly but remains low because the analytical content remains substantial.
 *
 * PERSPECTIVAL GAP:
 *   The continuity reading produces consistent classification (rope across all perspectives) because it is a coherent analytical frame that aligns beneficiary and victim relationships. Medieval practitioners benefit from continuity framing and experience low suppression. Ecclesiastical authority benefits and experiences low enforcement burden. The analytical observer sees genuine linguistic process. There is no substantial perspectival gap within the continuity reading itself — the gap exists between this reading and the rupture reading. That inter-reading gap is captured in the omega variables and the kernel_context note.
 *
 * DIRECTIONALITY LOGIC:
 *   All three perspectives derive directionality (d) from their structural relationship to the constraint. Medieval practitioners are beneficiaries (their competence remains valid) with constrained exit options (career investment in scribal or clerical role) — this produces moderate d. Ecclesiastical authority are beneficiaries with arbitrage options (they can shift standards if needed) — this produces low d. The analytical observer is neutral (analytical power, analytical exit) and derives canonical d from the analytical position. All three produce low effective extractiveness (chi) because the base extraction (ε=0.18) is low and the directionality values do not amplify it.
 *
 * MANDATROPHY ANALYSIS:
 *   The continuity reading avoids the mandatrophy by producing a coherent constraint type (rope) across all perspectives. The reading successfully resolves the classification puzzle by framing the phenomenon as genuine coordination (maintaining a cross-regional lingua franca) rather than hiding extraction under a coordination label or vice versa. The measurement trajectory (slowly rising theater as standards became more formalized) is consistent with a rope constraint that is not degrading into piton — the theater remains low (≤0.35) and the extractiveness remains low (≤0.22). If the measurements showed theater_ratio > 0.70, the rope would degrade to piton (institutional inertia). If extractiveness climbed to 0.46+, the reading would transition to tangled_rope (coordination + significant extraction hidden). Neither occurs under this reading's model, confirming that the constraint is indeed a low-extraction coordination mechanism.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    continuity_vs_rupture_framing,
    'Is medieval Latin a continuous evolution from Classical Latin, or a rupture event initiating a new language system?',
    'Empirical reconstruction of sound change trajectories, analogy patterns, and syntax shifts. Determine whether observed features follow predictable natural-language drift or show discontinuous leaps. Examine practitioner self-awareness: do they frame their activity as ''maintaining Latin'' (continuity) or ''creating a new form'' (rupture)?',
    'Continuity reading: low ε (0.18), rope class, beneficiaries are medieval practitioners whose competence remains valid. Rupture reading: ε ≥ 0.42, snare or tangled_rope class, victims are practitioners excluded from the new standard, beneficiaries are those who control the rupture boundary.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(continuity_vs_rupture_framing, empirical, 'Whether medieval Latin represents continuous evolution or rupture event').

omega_variable(
    internal_vs_external_authority_locus,
    'Does linguistic correction authority derive from within the system (practitioners'' own norm-setting) or from external enforcement (grammarians, scribal hierarchies, ecclesiastical mandates)?',
    'Textual analysis of correction patterns: are deviations from Classical norms marked as errors internally in manuscripts? Do marginal notes show self-correction or external correction? Examine grammatical treatises: do they frame correction as discovering pre-existing rules or imposing new ones?',
    'Internal locus: rope (coordination mechanism). External locus: tangled_rope or snare (enforcement mechanism becomes primary). The continuity reading assumes internal authority — practitioners absorb norms because they see themselves as maintaining a tradition, not because they are coerced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(internal_vs_external_authority_locus, empirical, 'Locus of correction authority: internal system maintenance or external enforcement').

omega_variable(
    beneficiary_victim_detection_ambiguity,
    'Under the continuity reading, who bears costs? The reading posits no victim set because ''correction'' is refinement within a continuous system. But does this invisibilize practitioners whose existing competence becomes non-standard?',
    'Prosopographical analysis of scribal careers: do practitioners who learned under one generation''s standard face diminished opportunity under the next? Compare manuscript production patterns across scriptoria with different ''continuity'' lineages. Examine evidence of practitioners switching scriptorias or styles when standards shift.',
    'If costs are invisible under continuity framing: the reading is accurate but requires careful documentation that victims exist (they are just not recognized as such by the continuity frame). If costs are substantial and practitioners experience them as discontinuous: the rupture reading better captures their structural reality, and continuity reading is a cover story.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_victim_detection_ambiguity, empirical, 'Whether continuity reading invisibilizes costs borne by practitioners during norm shifts').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(continuity_reading, 0, 800).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cont_tr_t0, continuity_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(cont_tr_t400, continuity_reading, theater_ratio, 400, 0.32).
narrative_ontology:measurement(cont_tr_t800, continuity_reading, theater_ratio, 800, 0.35).

% Extraction over time
narrative_ontology:measurement(cont_be_t0, continuity_reading, base_extractiveness, 0, 0.1).
narrative_ontology:measurement(cont_be_t400, continuity_reading, base_extractiveness, 400, 0.18).
narrative_ontology:measurement(cont_be_t800, continuity_reading, base_extractiveness, 800, 0.22).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(continuity_reading, information_standard).
narrative_ontology:affects_constraint(continuity_reading, rupture_reading).

% DUAL FORMULATION NOTE:
% The continuity_reading and rupture_reading are two coherent readings of the same kernel (correct_latin). They have different epsilon values, different victim sets, and different beneficiary structures. This is a constraint family decomposition per the ε-invariance principle: the observable-dependent way of framing 'correct Latin' changes the structural classification. Continuity reading: ε=0.18, rope, beneficiaries=medieval_practitioners, no victims. Rupture reading: ε≥0.42, snare/tangled_rope, beneficiaries=new-system controllers, victims=practitioners excluded from new standard. The network link is influences (not forecloses or coexists_with in isolation) because the two readings create pressure on each other's legitimacy conditions: if continuity reading is accepted, rupture reading's authority narrative loses force; if rupture reading dominates, continuity reading becomes a cover story.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
