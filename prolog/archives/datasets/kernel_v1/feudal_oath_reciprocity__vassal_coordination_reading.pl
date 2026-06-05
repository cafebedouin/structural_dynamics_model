% ============================================================================
% CONSTRAINT STORY: feudal_oath_reciprocity__vassal_coordination_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_feudal_oath_reciprocity__vassal_coordination_reading, []).

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
 *   constraint_id: feudal_oath_reciprocity__vassal_coordination_reading
 *   human_readable: Feudal Oath Reciprocity: Vassal Coordination Reading
 *   domain: medieval_political_economy/institutional_law
 *
 * SUMMARY:
 *   The feudal oath represents a foundational commitment mechanism for
 *   medieval political order. This constraint story instantiates ONE reading:
 *   the vassal_coordination_reading, which interprets the oath as a genuine
 *   reciprocal obligation structure where both lord and vassal are bound by
 *   charter-specified duties, both have enforceable remedies for breach, and
 *   both experience relatively low extraction because obligations are
 *   symmetrical and bounded. This reading emphasizes the coordination
 *   function — the oath solves the medieval military and political problem of
 *   how to maintain dispersed armed retainers without centralized
 *   bureaucracy. The charter text codifies mutual obligations, making the
 *   oath a functional governance mechanism rather than unidirectional
 *   extraction or purely performative ritual. However, this reading coexists
 *   with two alternative readings: the lord_extraction_reading (which
 *   interprets the oath as a mechanism for the lord to extract military and
 *   economic surplus while obscuring power asymmetries behind reciprocal
 *   language) and the ecclesiastical_mediation_reading (which sees the oath's
 *   real enforcement mechanism operating through church arbitration rather
 *   than feudal courts). The three readings are empirically distinguishable
 *   through documentary analysis of charter enforceability and breach
 *   outcomes.
 *
 * KEY AGENTS:
 *   - Vassal Cohort (moderate/constrained): Bound by oath to provide knight service, counsel, and revenue; entitled to lord's protection, justice, and relief. Direct beneficiary and direct obligant.
 *   - Lord's Household (institutional/constrained): Benefits from vassal military service and revenue; obligated to protect vassals and maintain justice. Direct beneficiary and direct obligant.
 *   - Serf/Peasant Population (powerless/trapped): Bears labor extraction to support vassal's obligations; benefits from lord's obligation to maintain peace. No direct oath relationship but structurally embedded in feudal hierarchy.
 *   - Ecclesiastical Authority (institutional/arbitrage): May mediate oath disputes; benefits from ecclesiastical authority over secular oath-breaking; not direct party to vassal-lord oath but potential arbiter.
 *   - Analytical Observer (analytical/analytical): Examines oath as coordination mechanism for medieval political economy; evaluates whether reciprocal framing is structurally accurate or rhetorical concealment of asymmetry.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(feudal_oath_reciprocity__vassal_coordination_reading, 0.18).
domain_priors:suppression_score(feudal_oath_reciprocity__vassal_coordination_reading, 0.25).
domain_priors:theater_ratio(feudal_oath_reciprocity__vassal_coordination_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(feudal_oath_reciprocity__vassal_coordination_reading, extractiveness, 0.18).
narrative_ontology:constraint_metric(feudal_oath_reciprocity__vassal_coordination_reading, suppression_requirement, 0.25).
narrative_ontology:constraint_metric(feudal_oath_reciprocity__vassal_coordination_reading, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(feudal_oath_reciprocity__vassal_coordination_reading, rope).
narrative_ontology:human_readable(feudal_oath_reciprocity__vassal_coordination_reading, "Feudal Oath Reciprocity: Vassal Coordination Reading").
narrative_ontology:topic_domain(feudal_oath_reciprocity__vassal_coordination_reading, "medieval_political_economy/institutional_law").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(feudal_oath_reciprocity__vassal_coordination_reading, 'd2c22eac-94c3-4187-a878-5b4772955c7b').
narrative_ontology:cs_kernel_codification('d2c22eac-94c3-4187-a878-5b4772955c7b', fixed_text).
narrative_ontology:cs_authority_grounding('d2c22eac-94c3-4187-a878-5b4772955c7b', lineage).
narrative_ontology:cs_interpretation_layer_present('d2c22eac-94c3-4187-a878-5b4772955c7b').
narrative_ontology:cs_reading_relation('d2c22eac-94c3-4187-a878-5b4772955c7b', feudal_oath_reciprocity__lord_extraction_reading, coexists_with).
narrative_ontology:cs_reading_relation('d2c22eac-94c3-4187-a878-5b4772955c7b', feudal_oath_reciprocity__ecclesiastical_mediation_reading, influences).
narrative_ontology:cs_axiom('d2c22eac-94c3-4187-a878-5b4772955c7b', foundational, charter_text_mutually_binding).
narrative_ontology:cs_axiom_status(charter_text_mutually_binding, holdable).
narrative_ontology:cs_axiom_grounding('d2c22eac-94c3-4187-a878-5b4772955c7b', charter_text_mutually_binding, conventional).
narrative_ontology:cs_axiom('d2c22eac-94c3-4187-a878-5b4772955c7b', foundational, feudal_court_enforces_charter).
narrative_ontology:cs_axiom_status(feudal_court_enforces_charter, holdable).
narrative_ontology:cs_axiom_grounding('d2c22eac-94c3-4187-a878-5b4772955c7b', feudal_court_enforces_charter, conventional).
narrative_ontology:cs_reference_frame('d2c22eac-94c3-4187-a878-5b4772955c7b', mutual_obligation_framework).
narrative_ontology:cs_drift_state('d2c22eac-94c3-4187-a878-5b4772955c7b', late_medieval_period, gap(authority_erosion, substantial, true)).
narrative_ontology:cs_created_at('d2c22eac-94c3-4187-a878-5b4772955c7b', '2026-02-26T14:23:45Z').
narrative_ontology:cs_kernel_id(feudal_oath_reciprocity__vassal_coordination_reading, feudal_oath_reciprocity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(feudal_oath_reciprocity__vassal_coordination_reading, vassal_cohort).
narrative_ontology:constraint_beneficiary(feudal_oath_reciprocity__vassal_coordination_reading, lord_household).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: VASSAL (ROPE) — Interprets the oath as a mutual coordination mechanism: the lord's obligations to protect, provision, and mediate justice are as binding as the vassal's obligations to provide military service and counsel. The charter text specifies both sides; breach by either party is grounds for oath dissolution. The vassal experiences low extraction because obligations are symmetrical and enforceable. Theater is minimal — obligations are concrete (knight service, harvest duties, council attendance) not performative.
constraint_indexing:constraint_classification(feudal_oath_reciprocity__vassal_coordination_reading, rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 2: LORD'S HOUSEHOLD (ROPE) — Benefits from the vassal oath as a coordination mechanism for military musters, revenue collection, and dispute resolution. The charter establishes predictable obligations: vassals owe defined knight service, not unlimited demands. From this perspective, the oath coordinates a dispersed network of armed retainers into a functional hierarchy. Extraction is bounded by charter — the lord cannot arbitrarily increase obligations without risking oath-dissolution and defection. Both lord and vassal experience the constraint as coordination with mutual exit costs.
constraint_indexing:constraint_classification(feudal_oath_reciprocity__vassal_coordination_reading, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: ANALYTICAL OBSERVER (ROPE) — Examines the oath as a solution to the feudal era's primary coordination problem: how to maintain military capacity and political order without centralized bureaucracy. The charter text is the institution's self-limiting mechanism — lords who breach obligations lose vassals; vassals who breach lose protection. The reciprocal framing is genuine: both parties are bound, both have exit costs, both have litigation remedies. Theater is low because the obligation structure is enforced through concrete consequences (military abandonment, withholding homage), not ritual performance.
constraint_indexing:constraint_classification(feudal_oath_reciprocity__vassal_coordination_reading, rope,
    context(agent_power(analytical),
            time_horizon(generational),
            exit_options(analytical),
            spatial_scope(continental))).

% PERSPECTIVE 4: SERF/PEASANT (TANGLED ROPE) — The vassal oath coordinates military and political obligations but embeds the serf in the vassal's extractive relationship. The serf provides labor and rent to the vassal; the vassal's oath specifies obligations to the lord but not to the serf. The serf experiences the feudal hierarchy as one-directional extraction with minimal coordination benefit. However, the serf also benefits from the lord's obligation to protect — the oath's peace-keeping function extends to serfs, who cannot unilaterally exit and bear full suppression cost.
constraint_indexing:constraint_classification(feudal_oath_reciprocity__vassal_coordination_reading, tangled_rope,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(feudal_oath_reciprocity__vassal_coordination_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(feudal_oath_reciprocity__vassal_coordination_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(feudal_oath_reciprocity__vassal_coordination_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

:- end_tests(feudal_oath_reciprocity__vassal_coordination_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.18): Low. This reading interprets the oath as a coordination mechanism with bounded obligations on both sides. The lord cannot unilaterally increase knight service demands or revenue without risking oath dissolution — the charter specifies limits. The vassal cannot unilaterally withhold service without forfeiting protection and relief. The low extractiveness reflects that both parties benefit from the coordination (military capacity, political order) and both have exit costs and enforceable remedies. The ε value is contingent on the alternative reading: if the lord_extraction_reading is correct and power asymmetries make the lord's obligations vacuous while the vassal's are concrete, extractiveness would be much higher (0.50+). This reading assumes the charter text has genuine enforceability. Suppression (0.25): Low-moderate. The oath creates binding obligations with exit costs for both parties (loss of military protection for vassal, loss of vassal for lord), but suppression is not high because the charter establishes legal procedures for breach litigation and the feudal court system provides adjudication mechanisms. Neither party can be arbitrarily constrained beyond charter terms. Theater ratio (0.35): Low. The obligation structure is concrete and measurable: 40 days knight service, castle garrison duties, harvest contributions, council attendance. Theater is present (oath ceremony, homage ritual) but represents a minority of the obligation content. The reading minimizes theater by emphasizing the enforcement mechanisms (courts, breach remedies) rather than the performative aspects.
 *
 * PERSPECTIVAL GAP:
 *   The vassal_coordination_reading produces low perspectival gap — all three agent perspectives (vassal, lord, analytical observer) classify as rope with low extractiveness. The gap opens when comparing this reading to the lord_extraction_reading: if the lord can breach protection obligations with impunity while the vassal cannot breach service obligations, the vassal's d value increases dramatically and the constraint reclassifies to snare from the vassal's perspective. The ecclesiastical_mediation_reading introduces a different gap: it suggests the charter text is secondary to actual enforcement mechanisms (church arbitration), which would change the suppression profile and the theater ratio. This reading assumes charter-based feudal court enforcement is primary.
 *
 * DIRECTIONALITY LOGIC:
 *   Under the vassal_coordination_reading, directionality (d) is symmetric for both vassal and lord perspectives. Both are beneficiaries (they benefit from mutual coordination), both are obligants (they bear duties), and both have constrained exit (breach triggers loss of relationship with concrete costs). The engine's derivation from beneficiary/victim + exit_options produces symmetric d values for both perspectives, which is the intended result of this reading. The powerless serf perspective shows asymmetric directionality — the serf is structurally downstream of the vassal-lord coordination and bears extraction cost without direct oath benefits. The analytical observer perspective tests the symmetry empirically: if documentary evidence shows charter enforceability is asymmetric (lord breaches systematically, vassal has no litigation recourse), the d value would shift, and the rope classification would fail. This reading presupposes that the charter has genuine mutual enforceability.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading resolves the mandatrophy by framing the feudal oath as genuine reciprocal coordination, not extraction disguised as coordination. The mandatrophy is whether the oath's reciprocal framing is structurally accurate or rhetorical. Under this reading, it is accurate: both parties are bound, both benefit from coordination, and both have enforceable exit mechanisms (oath dissolution). The charter text is not theater — it specifies concrete obligations that both parties use as litigation grounds. This reading does not claim the oath is extraction-free; it claims the extraction is bounded and mutual, making the constraint a rope rather than a snare or tangled rope. The serf's exclusion from the oath reveals that the reciprocity is limited to the vassal-lord dyad, but this does not negate reciprocity within that dyad.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    charter_enforceability_ambiguity,
    'Is the charter text a binding mutual constraint enforceable through feudal court procedures, or a legitimacy claim that lords breach with impunity when power imbalances favor extraction?',
    'Documentary evidence: frequency of oath-dissolution suits, successful vassal litigation against lord breach, historical cases of vassals withdrawing homage and their outcomes. Comparison of charter claims vs. actual enforcement outcomes across regional and temporal variation.',
    'If charters are enforceable: rope classification confirmed — extraction is structurally bounded. If enforcement is nominal and lords breach systematically: reclassify as snare or tangled rope with higher extractiveness and suppression.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(charter_enforceability_ambiguity, empirical, 'Degree of charter enforceability in practice vs. normative text').

omega_variable(
    alternative_readings_of_oath_kernel,
    'Does the feudal oath constitute a single kernel (reciprocal obligation) or multiple kernels (extraction, mediation, hierarchy)?',
    'Textual analysis of specific charters and oath formulas. Identification of which clauses codify lord obligations vs. vassal obligations. Examination of medieval legal commentary (jurists, scribes) on oath interpretation. Analysis of which clauses were most frequently breached or litigated.',
    'If the oath is a unified reciprocal kernel: this reading is the correct structural decomposition. If the oath contains multiple distinct kernels (extraction, mediation, protection), each should be a separate constraint story with different ε values.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(alternative_readings_of_oath_kernel, conceptual, 'Whether the feudal oath is a single kernel or multiple structurally distinct kernels').

omega_variable(
    power_asymmetry_concealment,
    'Does the reciprocal framing of the oath obscure underlying power asymmetries that make the lord''s obligations vacuous while the vassal''s are concrete?',
    'Comparative analysis of obligation specificity: vassal duties (40 days knight service, 1/3 revenue, council attendance) vs. lord duties (protection, justice, relief). Assessment of which obligations are measurable and enforceable. Examination of whether lords'' breach of protection obligations triggered vassal litigation or was treated as a political matter outside law.',
    'If the reciprocal framing accurately captures structural symmetry: rope classification holds. If lord duties are vague (protection is undefined, justice is discretionary) while vassal duties are concrete: reclassify to tangled rope with higher extractiveness. This would shift the reading toward the lord_extraction_reading and away from coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(power_asymmetry_concealment, empirical, 'Whether reciprocal framing masks asymmetric obligation specificity').

omega_variable(
    ecclesiastical_mediation_alternative,
    'To what degree does the ecclesiastical_mediation_reading explain the oath''s actual enforcement mechanism — church arbitration of disputes rather than feudal court procedures?',
    'Documentary evidence: records of ecclesiastical arbitration in feudal disputes, frequency of oath disputes resolved through church mediation vs. feudal courts, and whether church mediation outcomes differ structurally from feudal court outcomes.',
    'If ecclesiastical mediation is primary: this reading (vassal_coordination via charter) may be modeling a secondary or aspirational mechanism, while the actual constraint operates through religious authority structures. This would influence whether to separate a distinct constraint for ecclesiastical_mediation_reading.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(ecclesiastical_mediation_alternative, empirical, 'Role of ecclesiastical mediation vs. charter-based feudal court enforcement').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(feudal_oath_reciprocity__vassal_coordination_reading, 0, 150).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(forc_tr_t0, feudal_oath_reciprocity__vassal_coordination_reading, theater_ratio, 0, 0.3).
narrative_ontology:measurement(forc_tr_t75, feudal_oath_reciprocity__vassal_coordination_reading, theater_ratio, 75, 0.35).
narrative_ontology:measurement(forc_tr_t150, feudal_oath_reciprocity__vassal_coordination_reading, theater_ratio, 150, 0.4).

% Extraction over time
narrative_ontology:measurement(forc_be_t0, feudal_oath_reciprocity__vassal_coordination_reading, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(forc_be_t75, feudal_oath_reciprocity__vassal_coordination_reading, base_extractiveness, 75, 0.18).
narrative_ontology:measurement(forc_be_t150, feudal_oath_reciprocity__vassal_coordination_reading, base_extractiveness, 150, 0.2).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(feudal_oath_reciprocity__vassal_coordination_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(feudal_oath_reciprocity__vassal_coordination_reading, feudal_oath_reciprocity__lord_extraction_reading).
narrative_ontology:affects_constraint(feudal_oath_reciprocity__vassal_coordination_reading, feudal_oath_reciprocity__ecclesiastical_mediation_reading).

% DUAL FORMULATION NOTE:
% Three constraint stories decompose the feudal oath kernel along different interpretations of the same charter text. All three share kernel_id: feudal_oath_reciprocity but differ in reading_id (vassal_coordination_reading, lord_extraction_reading, ecclesiastical_mediation_reading). Each has distinct ε values: vassal_coordination assumes charter enforceability (ε=0.18 rope); lord_extraction assumes asymmetric power undermines enforceability (ε=0.50+ tangled_rope/snare); ecclesiastical_mediation shifts enforcement mechanism to church arbitration (ε varies by mediation effectiveness). The three stories are linked through network.affects_constraints and should be read together to understand the full ambiguity of the feudal oath kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
