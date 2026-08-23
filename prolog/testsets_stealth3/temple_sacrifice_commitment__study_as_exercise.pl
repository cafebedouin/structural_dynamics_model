% ============================================================================
% CONSTRAINT STORY: temple_sacrifice_commitment__study_as_exercise
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_temple_sacrifice_commitment__study_as_exercise, []).

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
    narrative_ontology:suppression_profile/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: temple_sacrifice_commitment__study_as_exercise
 *   human_readable: Study of Sacrificial Law as Performance of the Divine Command (Post-Temple Occupation Reading)
 *   domain: religious_law/commitment_system_theory
 *
 * SUMMARY:
 *   After the destruction of the Second Temple, the rabbinic academies
 *   preserved and taught the entire sacrificial corpus in operational detail,
 *   and ruled that engaging those laws fulfills the command they encode:
 *   study of the burnt-offering is counted as bringing the burnt-offering.
 *   This story models that standing arrangement, assessed by the reading's
 *   own lights: the ε referent is the study-as-occupancy practice itself as
 *   it operates, not the rival readings' accounts of it and not any endorsed
 *   replacement. The arrangement runs on voluntary participation, voluntary
 *   funding, and an interpretive apparatus that has held for nineteen
 *   centuries; the studying community is the beneficiary set, and no seat
 *   bears an imposed cost. KEY AGENTS (by structural relationship): -
 *   rabbinic_academy_leadership: agenda-setting administrator (institutional
 *   / identity_locked) - master_scholars: primary beneficiary seat
 *   concentrating honor and authority (organized / constrained) -
 *   yeshiva_student_body: participant beneficiary bearing voluntary time cost
 *   (moderate / mobile) - lay_supporting_community: sustaining beneficiary
 *   funding and reciting (moderate / constrained) -
 *   kohen_priestly_households: displaced-service lineage retained in textual
 *   centrality (moderate / constrained) - restorationist_currents: excluded
 *   minority pressing to reopen the settlement (organized / trapped) -
 *   academic_historians: analytical observer outside the framework
 *   (analytical / analytical)
 *
 * KEY AGENTS:
 *   - - rabbinic_academy_leadership: agenda-setting administrator (institutional / identity_locked) — defines curriculum, ordains, rules on fulfillment
 *   - - master_scholars: primary beneficiary seat (organized / constrained) — concentrates honor, livelihood, interpretive authority
 *   - - yeshiva_student_body: participant beneficiary (moderate / mobile) — bears voluntary time and opportunity cost
 *   - - lay_supporting_community: sustaining beneficiary (moderate / constrained) — funds academies and recites korbanot
 *   - - kohen_priestly_households: beneficiary of retained textual vocation (moderate / constrained)
 *   - - restorationist_currents: excluded seat (organized / trapped) — would reopen the settlement
 *   - - academic_historians: analytical observer (analytical / analytical) — documents the adaptation from outside
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(temple_sacrifice_commitment__study_as_exercise, 0.06).
domain_priors:suppression_score(temple_sacrifice_commitment__study_as_exercise, 0.08).
domain_priors:theater_ratio(temple_sacrifice_commitment__study_as_exercise, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(temple_sacrifice_commitment__study_as_exercise, extractiveness, 0.06).
narrative_ontology:constraint_metric(temple_sacrifice_commitment__study_as_exercise, suppression_requirement, 0.08).
narrative_ontology:constraint_metric(temple_sacrifice_commitment__study_as_exercise, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(temple_sacrifice_commitment__study_as_exercise, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(temple_sacrifice_commitment__study_as_exercise, resistance, 0.25).

% --- Constraint claim ---
narrative_ontology:constraint_claim(temple_sacrifice_commitment__study_as_exercise, rope).
narrative_ontology:human_readable(temple_sacrifice_commitment__study_as_exercise, "Study of Sacrificial Law as Performance of the Divine Command (Post-Temple Occupation Reading)").
narrative_ontology:topic_domain(temple_sacrifice_commitment__study_as_exercise, "religious_law/commitment_system_theory").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(temple_sacrifice_commitment__study_as_exercise, '5040ad14-1025-49b2-98c7-fa0d7812ebb2').
narrative_ontology:cs_kernel_codification('5040ad14-1025-49b2-98c7-fa0d7812ebb2', formalized).
narrative_ontology:cs_authority_grounding('5040ad14-1025-49b2-98c7-fa0d7812ebb2', lineage).
narrative_ontology:cs_interpretation_layer_present('5040ad14-1025-49b2-98c7-fa0d7812ebb2').
narrative_ontology:cs_reading_relation('5040ad14-1025-49b2-98c7-fa0d7812ebb2', temple_sacrifice_commitment__performance_only, coexists_with).
narrative_ontology:cs_reading_relation('5040ad14-1025-49b2-98c7-fa0d7812ebb2', temple_sacrifice_commitment__hybrid_preparatory, coexists_with).
narrative_ontology:cs_reading_relation('5040ad14-1025-49b2-98c7-fa0d7812ebb2', temple_sacrifice_commitment__symbolic_transformation, influences).
narrative_ontology:cs_axiom('5040ad14-1025-49b2-98c7-fa0d7812ebb2', foundational, study_constitutes_offering_performance).
narrative_ontology:cs_axiom_status(study_constitutes_offering_performance, holdable).
narrative_ontology:cs_axiom_grounding('5040ad14-1025-49b2-98c7-fa0d7812ebb2', study_constitutes_offering_performance, theological).
narrative_ontology:cs_axiom('5040ad14-1025-49b2-98c7-fa0d7812ebb2', secondary, intellectual_occupation_suffices_absent_altar).
narrative_ontology:cs_axiom_status(intellectual_occupation_suffices_absent_altar, holdable).
narrative_ontology:cs_axiom_grounding('5040ad14-1025-49b2-98c7-fa0d7812ebb2', intellectual_occupation_suffices_absent_altar, instrumental).
narrative_ontology:cs_reference_frame('5040ad14-1025-49b2-98c7-fa0d7812ebb2', study_occupied_command_continuity).
narrative_ontology:cs_drift_state('5040ad14-1025-49b2-98c7-fa0d7812ebb2', contemporary_liturgical_recitation, gap(practice_drift, minor, false)).
narrative_ontology:cs_created_at('5040ad14-1025-49b2-98c7-fa0d7812ebb2', '').
narrative_ontology:cs_kernel_id(temple_sacrifice_commitment__study_as_exercise, temple_sacrifice_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(temple_sacrifice_commitment__study_as_exercise, rabbinic_academy_leadership).
narrative_ontology:constraint_beneficiary(temple_sacrifice_commitment__study_as_exercise, master_scholars).
narrative_ontology:constraint_beneficiary(temple_sacrifice_commitment__study_as_exercise, yeshiva_student_body).
narrative_ontology:constraint_beneficiary(temple_sacrifice_commitment__study_as_exercise, lay_supporting_community).
narrative_ontology:constraint_beneficiary(temple_sacrifice_commitment__study_as_exercise, kohen_priestly_households).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Heads the academies (from Yavneh through the geonic yeshivot to contemporary institutions) that define the curriculum in which the sacrificial tractates are taught, ordain successors, and rule that engaging the sacrificial laws fulfills the command concerning them. Their teaching office exists only inside the tradition they administer; abandoning the reading would dissolve the warrant by which they teach.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__study_as_exercise, rabbinic_academy_leadership, agenda_setter,
    institutional, generational, identity_locked, global).

% Devote their lives to mastery of the sacrificial orders (Seder Kodashim and its commentaries). Honor, livelihood support, and interpretive authority flow disproportionately to this seat; leaving scholarship mid-career forfeits standing accumulated across decades and, for most, the relational world built around the study hall.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__study_as_exercise, master_scholars, beneficiary,
    organized, biographical, constrained, global).

% Spend structured daily hours studying the laws of offerings, reciting the korbanot passages in the liturgy, and sitting examinations on material whose material referent has not existed for nineteen centuries. The time is theirs to give; a student can reduce engagement or leave for trade or profession with mild communal disappointment rather than sanction.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__study_as_exercise, yeshiva_student_body, beneficiary,
    moderate, biographical, mobile, global).

% Sustains the academies through donations framed as partnership in the command, and participates in the occupied commitment through the fixed korbanot recitations in the daily prayer rite. Embedded in congregational life, they receive the assurance that obligations tied to the altar remain live and fulfillable where they stand.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__study_as_exercise, lay_supporting_community, beneficiary,
    moderate, generational, constrained, global).

% Descendants of the priestly line whose ancestors performed the offerings. They lost the operative service role with the altar but retain ritual dignities and a central place in the studied material; their family vocation survives as subject matter rather than practice.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__study_as_exercise, kohen_priestly_households, beneficiary,
    moderate, generational, constrained, global).

% Movements inside the tradition that regard the sufficiency of study as premature closure and press for active preparation for renewed material service. They cannot realize their program within the settled consensus, and leaving the framework altogether would cost them the covenantal belonging that motivates the pressure.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__study_as_exercise, restorationist_currents, excluded,
    organized, civilizational, trapped, continental).

% Document the post-destruction adaptation from outside the framework's authority structures, tracing how the academies preserved and elaborated the sacrificial corpus after 70 CE. They neither collect nor pay anything under the arrangement; their analyses circulate in a separate economy of citation.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__study_as_exercise, academic_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(temple_sacrifice_commitment__study_as_exercise, master_scholars).
narrative_ontology:fixing_cost_class(temple_sacrifice_commitment__study_as_exercise, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates covenant fidelity for a dispersed community whose commanded site of performance was destroyed: it converts an immovable obligation tied to one altar into a portable, replicable practice available in every place and generation, solved once centrally through a fixed curriculum and liturgy.
% TRANSFER_FUNCTION: Moves hours of attention and labor from individuals into the communal corpus of learning; moves status, honor, and livelihood support toward the master-scholar seat; moves material support from the wider community to the academies under the frame of voluntary partnership in a shared obligation.
% ABSENT_VOICES: Restorationist currents inside the tradition would reopen the question the settlement closed, and are structurally sidelined by the sufficiency claim. Samaritan communities, who never accepted the rabbinic reading and continue a material Passover practice, stand wholly outside the conversation. For most of the interval, women were excluded from the elite study circles in which the reading was elaborated; their relationship to the occupied commitment entered the record mainly through household observance.
% DISAPPEARANCE_RATIONALE: If the practice vanished overnight, the academies would lose their central curriculum and ordination content, the daily liturgy would shed its korbanot sections, the kohen lineages would lose their textual vocation, and the covenant-fidelity mechanism would stand unresolved again, forcing the surviving interpretive alternatives to compete for the vacated ground.
% FOUNDING_PROBLEM: After the destruction of the Second Temple in 70 CE cut off the altar, the tradition faced the question of how commands addressed to that altar remained fulfillable, and how a covenant organized around sacrificial service could survive without its material site.
% FOUNDING_PROBLEM_CORROBORATION: The altar's absence is attested from outside the benefiting parties by Josephus's contemporary account of the destruction and its aftermath, by Roman-era and patristic observers who noted that Jewish practice continued without sacrifice, and by modern academic scholarship on the post-70 CE adaptation (e.g., the historiography of Yavneh and the redaction of the sacrificial orders). None of these witnesses stands inside the studying community.
narrative_ontology:disappearance_verdict(temple_sacrifice_commitment__study_as_exercise, world_rearranges).
narrative_ontology:founding_problem_status(temple_sacrifice_commitment__study_as_exercise, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(temple_sacrifice_commitment__study_as_exercise, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(temple_sacrifice_commitment__study_as_exercise, 'none', 1).
narrative_ontology:epsilon_provenance(temple_sacrifice_commitment__study_as_exercise, 0.06, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(temple_sacrifice_commitment__study_as_exercise_tests).
:- end_tests(temple_sacrifice_commitment__study_as_exercise_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is authored near zero (0.06) because the costs the arrangement imposes are voluntarily assumed and internally framed as participation: study hours are self-given, donations are a framed religious act, and the only concentrated gain is honor and interpretive authority accruing to the master-scholar seat. Residual extraction reflects opportunity cost for full-time students and status concentration, and it sits below the identity-coordination floor (0.08), so the engine should read it as coordination cost rather than extractive overhead. Suppression is low (0.08): the practice needs no coercion to persist; dissenters exit into other traditions or press from inside without penalty machinery. Theater ratio is low (0.10): the activity genuinely occupies the commitment rather than performing its memory, though fixed liturgical recitation introduces a small rote component. Accessibility_collapse is moderate-low (0.30): rival interpretations of the same kernel remain openly live and practiced, so understanding this reading collapses no alternative. Resistance is low-moderate (0.25): occasional internal restorationist challenge and external scholarly reframing, but no organized opposition inside practicing communities. Suppression is authored as a raw structural property and is not scaled; only extraction is scaled by directionality and spatial scope, and the global scope amplifies an already-negligible epsilon by a trivial amount. Enforcement capacity is static across the interval, so no suppression_requirement series is authored. Both tracked metrics run on one shared eight-point grid.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently from their positions. From the academy-leadership and scholar seats the arrangement is faithful continuity: the command is alive and being kept. From the student seat it is formative discipline with an exit that stays open. From the excluded restorationist seat the same settlement reads as premature closure of a question that ought to remain burning. From the analytical seat it reads as a successful institutional adaptation. The engine computes per-seat classifications from power, exit, and directionality data; the authored claim does not adjudicate among them.
 *
 * DIRECTIONALITY LOGIC:
 *   Every participant seat derives a directionality near the beneficiary pole: the declared beneficiaries (academy leadership, master scholars, students, lay community, priestly households) all receive covenant continuity, honor, or vocation from the arrangement, and none bears an imposed transfer. The highest directionality in the story belongs to the yeshiva student body, whose opportunity cost is borne personally even though exit remains mobile; it stays far from the full-target pole. The restorationist seat is excluded, not targeted: exclusion denies voice but imposes no extracted cost, which is precisely why this reading generates no victim set. No directionality overrides are needed because the derivation from beneficiary declarations plus exit options reproduces the intended relationships.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification guards against mislabeling in both directions. From outside, the reading is often caricatured as rationalized substitution, which would push toward an extraction-heavy verdict; the structural data refuse that: the founding problem (an altar-less covenant with live obligations) is still live, theater remains low, and nothing has atrophied into mere performance. From inside, the temptation is to declare the arrangement a permanent fixture of reality; emerges_naturally is false because the reading is a constructed interpretive settlement, adopted under duress in 70 CE and sustained by institutional choice ever since. Mandatrophy is not resolved: the founding problem persists, the function has not migrated elsewhere, and the arrangement has no sunset clause, because this reading holds study as sufficient for as long as the material conditions are absent rather than transitional until they return.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest_resolution,
    'Which reading of the temple_sacrifice_commitment kernel is authoritative: full occupation by study (this reading), suspended preparatory maintenance, archival preservation awaiting material resumption, or authorized transformation into prayer?',
    'Authoritative halakhic consensus formation across the living interpretive centers, or the messianic-restoration test case: whether communities that regain material capacity resume performance or retain study-priority would adjudicate between this reading and its rivals.',
    'If a rival reading wins, the beneficiary and victim structure changes sharply: the archival reading renders the current practice inert, and the preparatory reading reintroduces a deferred-obligation structure with different extraction characteristics. This story''s near-zero epsilon holds only under the present reading''s victory.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest_resolution, conceptual, 'Committer-frame ambiguity: this constraint is one reading of a contested kernel, and its structural profile is indexed to that reading winning.').

omega_variable(
    intrinsic_value_vs_rationalized_adaptation,
    'Is study''s sufficiency an intrinsic valuation of intellectual engagement with the command, or a rationalization of political incapacity that the community would abandon if the altar returned?',
    'Comparative analysis of communities facing partial capacity: episodes of attempted restoration (Passover-offering attempts on the Temple mount) and communities that never adopted the rabbinic reading (Samaritan practice) reveal whether material performance is preferred wherever possible.',
    'If the sufficiency claim is incapacity-rationalization, the arrangement functions as a holding pattern rather than a settled good, shifting color toward a transitional structure and raising the weight of the restorationist seat''s objection.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(intrinsic_value_vs_rationalized_adaptation, conceptual, 'Whether the reading''s core valuation is genuine preference or adapted necessity.').

omega_variable(
    modern_institutionalization_drift,
    'Has mass institutionalization of full-time study (stipends, state subsidies, military-service exemptions) introduced extraction absent from the classical arrangement, in which third parties now bear costs for others'' study?',
    'Decompose into a separate constraint story tracking resource flows and burden incidence in contemporary subsidized-study arrangements, with its own beneficiaries and victims; this story''s epsilon is unaffected either way.',
    'If the modern subsidy arrangement carries identifiable payers, it warrants its own substantially extractive profile; conflating it with this reading would contaminate a genuinely near-zero-epsilon constraint with a different structure''s costs.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(modern_institutionalization_drift, empirical, 'Epsilon-invariance guard: the classical reading and the modern subsidized-study economy are candidate distinct constraints.').

omega_variable(
    liturgical_recitation_status,
    'Does the fixed, often rapid liturgical recitation of the korbanot passages count as engaged occupation of the command under this reading, or as archival rehearsal of the kind the performance-only reading attributes to all study without depth?',
    'Pedagogical and comprehension studies of reciter engagement versus the depth standard the academies apply in the study hall.',
    'If recitation falls short of occupation, a large fraction of the practicing population instantiates a weaker version of the reading, raising theater_ratio for the practiced form and narrowing the gap toward the rival reading''s characterization.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(liturgical_recitation_status, conceptual, 'Whether the dominant practiced form still instantiates this reading or drifts toward archival recitation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(temple_sacrifice_commitment__study_as_exercise, 70, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(temp_tr_t70, temple_sacrifice_commitment__study_as_exercise, theater_ratio, 70, 0.18).
narrative_ontology:measurement(temp_tr_t100, temple_sacrifice_commitment__study_as_exercise, theater_ratio, 100, 0.14).
narrative_ontology:measurement(temp_tr_t220, temple_sacrifice_commitment__study_as_exercise, theater_ratio, 220, 0.11).
narrative_ontology:measurement(temp_tr_t500, temple_sacrifice_commitment__study_as_exercise, theater_ratio, 500, 0.08).
narrative_ontology:measurement(temp_tr_t1040, temple_sacrifice_commitment__study_as_exercise, theater_ratio, 1040, 0.07).
narrative_ontology:measurement(temp_tr_t1560, temple_sacrifice_commitment__study_as_exercise, theater_ratio, 1560, 0.08).
narrative_ontology:measurement(temp_tr_t1880, temple_sacrifice_commitment__study_as_exercise, theater_ratio, 1880, 0.09).
narrative_ontology:measurement(temp_tr_t2025, temple_sacrifice_commitment__study_as_exercise, theater_ratio, 2025, 0.1).

% Extraction over time
narrative_ontology:measurement(temp_be_t70, temple_sacrifice_commitment__study_as_exercise, base_extractiveness, 70, 0.14).
narrative_ontology:measurement(temp_be_t100, temple_sacrifice_commitment__study_as_exercise, base_extractiveness, 100, 0.11).
narrative_ontology:measurement(temp_be_t220, temple_sacrifice_commitment__study_as_exercise, base_extractiveness, 220, 0.09).
narrative_ontology:measurement(temp_be_t500, temple_sacrifice_commitment__study_as_exercise, base_extractiveness, 500, 0.07).
narrative_ontology:measurement(temp_be_t1040, temple_sacrifice_commitment__study_as_exercise, base_extractiveness, 1040, 0.06).
narrative_ontology:measurement(temp_be_t1560, temple_sacrifice_commitment__study_as_exercise, base_extractiveness, 1560, 0.06).
narrative_ontology:measurement(temp_be_t1880, temple_sacrifice_commitment__study_as_exercise, base_extractiveness, 1880, 0.07).
narrative_ontology:measurement(temp_be_t2025, temple_sacrifice_commitment__study_as_exercise, base_extractiveness, 2025, 0.06).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(temple_sacrifice_commitment__study_as_exercise, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(temple_sacrifice_commitment__study_as_exercise, identity_coordination).
narrative_ontology:affects_constraint(temple_sacrifice_commitment__study_as_exercise, temple_sacrifice_commitment__performance_only).
narrative_ontology:affects_constraint(temple_sacrifice_commitment__study_as_exercise, temple_sacrifice_commitment__hybrid_preparatory).
narrative_ontology:affects_constraint(temple_sacrifice_commitment__study_as_exercise, temple_sacrifice_commitment__symbolic_transformation).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'what happened to the sacrifice command after the destruction' decomposes into four structurally distinct commitments, one per reading of the shared kernel. This member (study_as_exercise) carries near-zero epsilon with a beneficiary-only structure. Its siblings carry different structures: the archival reading renders the practice inert (piton-colored), the preparatory reading builds a deferred-obligation structure, and the symbolic-transformation reading relocates the instantiation into prayer. Each story owns its own epsilon, beneficiary set, and classification; the family edges record that the shared scriptural kernel is the common upstream citation and that this reading's success supplied the legitimating precedent on which the transformation reading partially builds.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
