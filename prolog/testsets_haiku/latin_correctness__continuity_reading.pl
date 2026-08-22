% ============================================================================
% CONSTRAINT STORY: latin_correctness__continuity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_latin_correctness__continuity_reading, []).

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
 *   constraint_id: latin_correctness__continuity_reading
 *   human_readable: Medieval Latin as Legitimate Linguistic Continuation
 *   domain: intellectual_history/historical_linguistics
 *
 * SUMMARY:
 *   This constraint story instantiates the CONTINUITY READING of the
 *   contested kernel 'latin_correctness'. The reading holds that Medieval
 *   Latin is the legitimate continuation of classical Latin through organic
 *   linguistic change — vernacular phonology, expanded vocabulary, and
 *   modified syntax are natural developments of an inherited tradition, not
 *   corruptions of a fixed standard. The constraint describes the legitimacy
 *   claim: that medieval scribes, scholars, theologians, and ecclesiastical
 *   institutions are authorized to use and evolve Latin according to their
 *   communicative needs because they inherit the tradition. This reading is
 *   one of three coherent positions on the kernel; the other two
 *   (hybrid_reading, rupture_reading) are distinct constraint stories in the
 *   constraint family, linked via network.affects_constraints. The
 *   ε-invariance principle requires that each reading be authored as a
 *   separate constraint with its own metrics and beneficiary structure,
 *   because the readings assign different ε values to the standing
 *   arrangement (the medieval textual practice). The continuity reading
 *   treats medieval Latin as low-extractiveness coordination (legitimate
 *   inheritance); the rupture reading treats it as high-extractiveness error
 *   (corruption requiring correction). The metrics authored here (low
 *   extractiveness, low suppression, low theater) describe the standing
 *   medieval practice AS THE CONTINUITY READING UNDERSTANDS IT.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(latin_correctness__continuity_reading, 0.12).
domain_priors:suppression_score(latin_correctness__continuity_reading, 0.08).
domain_priors:theater_ratio(latin_correctness__continuity_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(latin_correctness__continuity_reading, extractiveness, 0.12).
narrative_ontology:constraint_metric(latin_correctness__continuity_reading, suppression_requirement, 0.08).
narrative_ontology:constraint_metric(latin_correctness__continuity_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(latin_correctness__continuity_reading, accessibility_collapse, 0.25).
narrative_ontology:constraint_metric(latin_correctness__continuity_reading, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(latin_correctness__continuity_reading, rope).
narrative_ontology:human_readable(latin_correctness__continuity_reading, "Medieval Latin as Legitimate Linguistic Continuation").
narrative_ontology:topic_domain(latin_correctness__continuity_reading, "intellectual_history/historical_linguistics").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(latin_correctness__continuity_reading, '88af7dbc-e86c-4aab-a3c0-b965e4297bcf').
narrative_ontology:cs_kernel_codification('88af7dbc-e86c-4aab-a3c0-b965e4297bcf', distributed).
narrative_ontology:cs_authority_grounding('88af7dbc-e86c-4aab-a3c0-b965e4297bcf', lineage).
narrative_ontology:cs_interpretation_layer_present('88af7dbc-e86c-4aab-a3c0-b965e4297bcf').
narrative_ontology:cs_reading_relation('88af7dbc-e86c-4aab-a3c0-b965e4297bcf', latin_correctness__rupture_reading, forecloses).
narrative_ontology:cs_reading_relation('88af7dbc-e86c-4aab-a3c0-b965e4297bcf', latin_correctness__hybrid_reading, influences).
narrative_ontology:cs_axiom('88af7dbc-e86c-4aab-a3c0-b965e4297bcf', foundational, linguistic_legitimacy_through_inheritance).
narrative_ontology:cs_axiom_status(linguistic_legitimacy_through_inheritance, holdable).
narrative_ontology:cs_axiom_grounding('88af7dbc-e86c-4aab-a3c0-b965e4297bcf', linguistic_legitimacy_through_inheritance, deontological).
narrative_ontology:cs_axiom('88af7dbc-e86c-4aab-a3c0-b965e4297bcf', foundational, organic_evolution_as_valid_development).
narrative_ontology:cs_axiom_status(organic_evolution_as_valid_development, holdable).
narrative_ontology:cs_axiom_grounding('88af7dbc-e86c-4aab-a3c0-b965e4297bcf', organic_evolution_as_valid_development, empirically_contingent).
narrative_ontology:cs_reference_frame('88af7dbc-e86c-4aab-a3c0-b965e4297bcf', organic_linguistic_inheritance_framework).
narrative_ontology:cs_drift_state('88af7dbc-e86c-4aab-a3c0-b965e4297bcf', high_middle_ages_scholasticism, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('88af7dbc-e86c-4aab-a3c0-b965e4297bcf', '').
narrative_ontology:cs_kernel_id(latin_correctness__continuity_reading, latin_correctness).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(latin_correctness__continuity_reading, medieval_scribes_scholars).
narrative_ontology:constraint_beneficiary(latin_correctness__continuity_reading, ecclesiastical_writers).
narrative_ontology:constraint_beneficiary(latin_correctness__continuity_reading, vernacular_language_communities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(latin_correctness__continuity_reading, theological_glossators).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Medieval clerics and scholars write in a living, evolving Latin that incorporates vernacular phonology, extended vocabulary, and new syntactic patterns. Under the continuity reading, their work is legitimate linguistic practice, not corruption. They inherit a language and adapt it to new contexts (theology, administration, technical description). Their exit option is to write in vernacular, which they increasingly do; the choice to write in Latin is a positive commitment to the inherited tradition.
narrative_ontology:constraint_stakeholder(latin_correctness__continuity_reading, medieval_scribes_scholars, beneficiary,
    moderate, biographical, mobile, continental).

% The Church maintains Latin as the official language of liturgy, canon law, and inter-diocesan communication. The continuity reading legitimizes the Church's practice of adapting Latin to new institutional needs (sophisticated theological vocabulary, legal terminology, administrative precision) without requiring reconstruction of classical purity. The Church's authority to define what counts as legitimate Latin rests on its role as the institutional custodian of the tradition.
narrative_ontology:constraint_stakeholder(latin_correctness__continuity_reading, ecclesiastical_institution, agenda_setter,
    institutional, civilizational, arbitrage, continental).

% Romance-language speakers (proto-French, proto-Italian, proto-Spanish communities) experience medieval Latin as continuous with their own linguistic inheritance. The continuity reading validates that Latin and vernacular Latin-descended languages are branches of one living tradition, not that one is corrupt and one is pure. This reading makes space for the legitimacy of the emerging vernacular languages without requiring them to be seen as degradations of a fixed standard.
narrative_ontology:constraint_stakeholder(latin_correctness__continuity_reading, vernacular_language_communities, beneficiary,
    organized, generational, arbitrage, regional).

% Scholastic theologians (Aquinas, Scotus, and their schools) use medieval Latin as their working language for sophisticated philosophical and theological disputation. Under the continuity reading, their neologisms, modified syntax, and technical vocabulary are legitimate adaptations of the tradition, not violations of it. They simultaneously benefit from and reinforce the continuity reading by treating their linguistic practice as authorized by inheritance.
narrative_ontology:constraint_stakeholder(latin_correctness__continuity_reading, theological_glossators, beneficiary,
    powerful, civilizational, mobile, continental).
narrative_ontology:stakeholder_secondary_role(latin_correctness__continuity_reading, theological_glossators, agenda_setter).

% Later Renaissance and early modern humanists (Petrarch forward) argue against the continuity reading, claiming that medieval Latin is corruption and that classical texts must be recovered and imitated as the true standard. They are excluded from the medieval period itself but retrospectively contest the legitimacy of the medieval reading from an analytical position. Their return to classical texts becomes a philological program.
narrative_ontology:constraint_stakeholder(latin_correctness__continuity_reading, classical_purist_advocates, excluded,
    analytical, generational, analytical, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(latin_correctness__continuity_reading, diffuse).
narrative_ontology:fixing_cost_class(latin_correctness__continuity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Preserves a supra-regional, trans-vernacular communication medium that allows scholars, clerics, and administrators across linguistically fragmented medieval Europe to write, read, and exchange texts with shared reference points. Medieval Latin solves the coordination problem of maintaining intellectual continuity across the breakdown of unified empire and diversification into mutually unintelligible vernaculars.
% TRANSFER_FUNCTION: The continuity reading transfers legitimacy and authority to medieval speakers: it affirms that they inherit the right to use and modify Latin according to their communicative needs. No wealth or status transfer occurs; the constraint operates as a permission structure, not an extraction mechanism.
% ABSENT_VOICES: Classical purists (future Renaissance humanists) would object that the continuity reading surrenders the standard of classical purity and permits corruption. Strict prescriptivists (any era) who demand conformity to a fixed canon are structurally excluded from the medieval period itself but would contest the legitimacy of the reading from a backward-looking posture.
% DISAPPEARANCE_RATIONALE: If the continuity reading vanished overnight and the rupture reading became canonical, medieval scholars would face a choice: either write 'incorrectly' (according to classical standards), or cease writing in Latin altogether and shift to vernacular. The intellectual continuity of the medieval Latin textual tradition would be reframed as a long period of error, not development. Institutional documentation, theological works, and administrative texts written in medieval Latin would become retroactively delegitimized as corrupted, not evolved.
% FOUNDING_PROBLEM: After the collapse of unified empire and the diversification of spoken Romance languages, Latin as a living speech dies out at the mass level. But Latin as a written, liturgical, and intellectual medium persists. The founding problem: how can the Latin tradition remain legitimate and productive when it no longer matches how anyone speaks natively, and when its users must learn it as a second language from texts?
% FOUNDING_PROBLEM_CORROBORATION: Medieval ecclesiastical and scholarly practice attests the problem is live: monasteries and cathedral schools maintain Latin instruction precisely to preserve the written tradition across the linguistic transition. Later humanists (14th–16th centuries) attest the founding problem differently, claiming medieval solutions were corruptions and that recovery of classical purity is the true solution. Modern historical linguists outside the normative tradition (Löfstedt, Banniard, Wright) corroborate that medieval Latin is the living continuation of Latin through vernacular phonological influence and lexical expansion — the tradition solves the founding problem through organic adaptation, not corruption.
narrative_ontology:disappearance_verdict(latin_correctness__continuity_reading, world_rearranges).
narrative_ontology:founding_problem_status(latin_correctness__continuity_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(latin_correctness__continuity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(latin_correctness__continuity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(latin_correctness__continuity_reading, 0.12, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(latin_correctness__continuity_reading_tests).
:- end_tests(latin_correctness__continuity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.12) because the continuity reading treats medieval Latin use as a non-coercive permission structure: medieval speakers inherit the right to use Latin and adapt it to new contexts. No one is forced to write in medieval Latin (the vernacular alternative remains open); no central authority collecting rents from the practice; no victim set bearing costs for the beneficiaries' gain. The constraint is coordination, not extraction. Suppression is very low (0.08) because no significant coercive apparatus is required to maintain the continuity reading — it legitimates existing practice rather than defending it against attack. The medieval Church maintains Latin instruction and ensures the textual tradition survives, but this is custodianship, not suppression. Theater ratio is low-moderate (0.15) and rises slightly during the scholastic period (900–1200) when theological disputation becomes increasingly formalized and performance-like, but remains low because the core coordination function (supra-regional scholarly communication) is real and functional throughout. The measurement series shows extractiveness and theater ratio rising slightly during the high Middle Ages as the scholarly establishment becomes more institutionalized, then declining slightly toward 1200 as the constraints of Latin use become more apparent and the vernacular alternatives become more viable. Suppression remains stable and low: there is no enforcer working to keep the continuity reading in place — it simply describes how medieval users experience their own practice.
 *
 * PERSPECTIVAL GAP:
 *   The beneficiary seats (medieval scribes, theologians, ecclesiastical authorities) experience the continuity reading as legitimate; it grants them authority and continuity. The excluded seat (classical purists, future humanists) experiences the same medieval practice very differently — as corruption requiring correction. The engine computes this divergence from the structural data: beneficiaries have low d (they benefit from the reading), excluded parties would have high d (they are targeted by the continuity framing as corruptors rather than legitimate practitioners). The perspectival gap is not a disagreement about facts (the medieval texts exist and have their features); it is a disagreement about the legitimacy framework — what counts as correct, what legitimates inheritance, what makes deviation acceptable.
 *
 * DIRECTIONALITY LOGIC:
 *   Medieval scribes and scholars are structural beneficiaries under the continuity reading: it grants them the right to use and adapt Latin, validates their practice, and positions them as legitimate inheritors rather than corruptors. They have moderate power and mobile exit options (they can write in vernacular if they choose); their directionality is toward beneficiary (low d, around 0.20). The ecclesiastical institution is the agenda setter: it maintains Latin instruction, preserves the textual tradition, and through its authority teaches the continuity reading to each generation of monks and clerics. It has institutional power and arbitrage-level exit options (it controls which reading is taught, which texts are copied, which authors are read). The vernacular-language communities benefit from the continuity reading because it validates the linguistic relationship between Latin and their own speech — they inherit the tradition not as alien, but as ancestral. Classical purists are excluded: they are not part of the medieval communicative world but would retrospectively contest the legitimacy of the medieval reading and claim corruption instead. No agent bears the cost of the continuity reading operating: the constraint is non-extractive.
 *
 * MANDATROPHY ANALYSIS:
 *   The continuity reading does NOT exhibit mandatrophy. The founding problem (how to maintain Latin tradition across linguistic transition from unity to diversity) remains live throughout the interval: medieval scribes and scholars continue to face the genuine coordination challenge of writing in a supra-regional medium while their spoken language diverges. The constraint persists because the problem persists, not because mandate has outlived function. The theorem that detects mandatrophy (founding_problem_status='dead' AND disappearance_verdict='world_rearranges') does not apply here. The founding problem does not become dead until the Renaissance, when humanist recovery of classical texts makes the rupture_reading (classical purity is the standard) compete successfully with the continuity_reading. Until that external shift, the continuity reading remains justified by the live problem it solves.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reading_vs_rupture_boundary,
    'At what point does accumulated linguistic change cross from ''legitimate evolution'' (continuity reading) to ''corruption requiring correction'' (rupture reading)?',
    'No mathematical boundary exists; the reading determines the classification. Different frameworks place the boundary at different moments (6th century, 8th century, 12th century). Historical data cannot resolve which is correct because the reading and the data are entangled.',
    'This is the irreducible ambiguity in the kernel contest: whether medieval Latin is a continuation or a corruption is not a fact about the texts, but a choice of framework. Both readings are internally coherent and both fit the medieval textual record.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_vs_rupture_boundary, conceptual, 'The reading determines the classification, not external facts.').

omega_variable(
    authority_grounding_shift,
    'Is the authority to define ''correct Latin'' grounded in continuity of living practice and institutional custodianship (continuity reading) or in fidelity to classical texts reconstructed from ancient sources (rupture reading)?',
    'The two readings rest on opposite authority grounding structures: continuity = practice/lineage (inherited teaching); rupture = expertise/textual authority (classical philology). No empirical test can decide between authority groundings — it is a choice about what legitimates a tradition.',
    'The choice of authority grounding determines the entire evaluation. If medieval practice grounds authority, the continuity reading is valid; if classical texts ground it, the rupture reading is. This is the fundamental structural divergence between the readings.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(authority_grounding_shift, conceptual, 'The authority grounding choice entails the reading choice.').

omega_variable(
    beneficiary_set_boundary,
    'Are the vernacular-language communities beneficiaries of the continuity reading, or are they victims of a constraint that delays the legitimation of their own languages?',
    'The continuity reading treats Romance-speaking communities as beneficiaries (their inheritance is validated). An alternative analysis treats them as victims (the Latin tradition suppresses vernacular legitimacy). The measured suppression is very low in the continuity reading because it does not frame Latin as coercive — but a different reading would frame the same medieval practice as diglossia imposed by the Church.',
    'If vernaculars are treated as victims, the constraint would reclassify from rope to tangled_rope or snare, depending on whether the ecclesiastical institution is read as coordinating genuine supra-regional needs or merely extracting clerical dominance. This omega documents the ambiguity in the beneficiary declaration.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_set_boundary, conceptual, 'Whether the vernacular-speaking communities are beneficiaries or victims depends on the reading''s framing of diglossia.').

omega_variable(
    performance_vs_function_drift,
    'As scholastic formalism increases (900–1200), does the constraint''s theater ratio rise because the coordination function is being replaced by scholastic ritual, or because the function is being elaborated through formal techniques?',
    'Examine the productivity of scholastic disputation: does it solve new coordination problems (theological precision, institutional argumentation) or does it replace practical coordination with formal performance? The rise in theater_ratio during this period measures the shift, but the interpretation depends on whether elaboration counts as functional expansion or functional decay.',
    'If theater rise signals functional decay, the constraint would trend toward piton classification; if it signals functional elaboration, the rope classification holds. The measurement data shows theater rising but not dominantly — the ambiguity remains.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(performance_vs_function_drift, empirical, 'Whether scholastic formalism elaborates or replaces the coordination function.').

omega_variable(
    kernel_reading_contest_foundation,
    'Is the kernel contest between continuity_reading and rupture_reading grounded in irreducible disagreement about authority (lineage/practice vs. text recovery), or will empirical evidence about medieval linguistic practice ultimately vindicate one reading?',
    'Modern historical linguistics (Banniard, Wright, Löfstedt) treats medieval Latin as organic evolution from classical, which supports the continuity reading empirically. However, the rupture reading''s authority grounding (fidelity to classical texts) is not empirical — it is normative. No amount of historical evidence can refute a normative claim about what Latin ''should'' be.',
    'If the contest is empirical, the continuity reading wins by modern linguistics. If it is normative/authority-grounded, both readings remain live, and the Humanist turn to classical recovery is a choice about authority grounding, not a discovery about correctness.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest_foundation, conceptual, 'Whether the kernel contest is empirical or authority-grounded.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(latin_correctness__continuity_reading, 400, 1200).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(lati_tr_t400, latin_correctness__continuity_reading, theater_ratio, 400, 0.08).
narrative_ontology:measurement(lati_tr_t550, latin_correctness__continuity_reading, theater_ratio, 550, 0.11).
narrative_ontology:measurement(lati_tr_t750, latin_correctness__continuity_reading, theater_ratio, 750, 0.14).
narrative_ontology:measurement(lati_tr_t900, latin_correctness__continuity_reading, theater_ratio, 900, 0.16).
narrative_ontology:measurement(lati_tr_t1050, latin_correctness__continuity_reading, theater_ratio, 1050, 0.17).
narrative_ontology:measurement(lati_tr_t1200, latin_correctness__continuity_reading, theater_ratio, 1200, 0.15).

% Extraction over time
narrative_ontology:measurement(lati_be_t400, latin_correctness__continuity_reading, base_extractiveness, 400, 0.08).
narrative_ontology:measurement(lati_be_t550, latin_correctness__continuity_reading, base_extractiveness, 550, 0.1).
narrative_ontology:measurement(lati_be_t750, latin_correctness__continuity_reading, base_extractiveness, 750, 0.12).
narrative_ontology:measurement(lati_be_t900, latin_correctness__continuity_reading, base_extractiveness, 900, 0.11).
narrative_ontology:measurement(lati_be_t1050, latin_correctness__continuity_reading, base_extractiveness, 1050, 0.13).
narrative_ontology:measurement(lati_be_t1200, latin_correctness__continuity_reading, base_extractiveness, 1200, 0.12).

% Suppression requirement over time
narrative_ontology:measurement(lati_su_t400, latin_correctness__continuity_reading, suppression_requirement, 400, 0.05).
narrative_ontology:measurement(lati_su_t550, latin_correctness__continuity_reading, suppression_requirement, 550, 0.06).
narrative_ontology:measurement(lati_su_t750, latin_correctness__continuity_reading, suppression_requirement, 750, 0.08).
narrative_ontology:measurement(lati_su_t900, latin_correctness__continuity_reading, suppression_requirement, 900, 0.09).
narrative_ontology:measurement(lati_su_t1050, latin_correctness__continuity_reading, suppression_requirement, 1050, 0.09).
narrative_ontology:measurement(lati_su_t1200, latin_correctness__continuity_reading, suppression_requirement, 1200, 0.08).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(latin_correctness__continuity_reading, information_standard).
narrative_ontology:boltzmann_floor_override(latin_correctness__continuity_reading, 0.06).
narrative_ontology:affects_constraint(latin_correctness__continuity_reading, latin_correctness__rupture_reading).
narrative_ontology:affects_constraint(latin_correctness__continuity_reading, latin_correctness__hybrid_reading).

% DUAL FORMULATION NOTE:
% The latin_correctness kernel decomposes into three structurally distinct constraint stories: continuity_reading (Medieval Latin is legitimate organic evolution), rupture_reading (Medieval Latin is corruption requiring classical recovery), and hybrid_reading (Classical norms apply to literary domains; medieval forms legitimate for technical/practical domains). Each reading assigns different ε values to the identical medieval textual practice because each reading instantiates a different legitimacy framework. The three stories are linked via network.affects_constraints: continuity → rupture/hybrid; rupture → hybrid. They share a kernel (the contested claim 'what is correct Latin?') but diverge in their assignment of authority grounding and their beneficiary/victim structures. A four-part constraint family examining the same kernel under different readings (continuity, rupture, hybrid, and potentially a quantitative-standard reading focusing on normalization via institutional measurement) would illuminate how authority grounding and definition of legitimate practice structure the contest.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
