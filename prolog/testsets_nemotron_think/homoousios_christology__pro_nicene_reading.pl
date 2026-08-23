% ============================================================================
% CONSTRAINT STORY: homoousios_christology__pro_nicene_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_homoousios_christology__pro_nicene_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: homoousios_christology__pro_nicene_reading
 *   human_readable: Nicene Homoousios Confession (Pro-Nicene Reading)
 *   domain: historical_theology/ecclesiastical_politics
 *
 * SUMMARY:
 *   The pro-Nicene reading of homoousios (325) establishes Christ as
 *   consubstantial with the Father — identical divine substance. This reading
 *   becomes the enforced orthodoxy of the imperial church through the Council
 *   of Constantinople (381) and Theodosius I's legislation. The constraint
 *   operates as a tangled rope: it solves a genuine coordination problem
 *   (imperial theological unity, ecclesiastical communion) while extracting
 *   conformity through anathema, deposition, exile, and property
 *   confiscation. Beneficiaries are the imperial church hierarchy, conforming
 *   bishops, and the emperor; victims are Arian and semi-Arian bishops, Arian
 *   laity, and excluded Germanic Arian churches. The enforcement machinery
 *   (councils, imperial rescripts, episcopal courts) is active and intensive.
 *   The measurement series (325–451, indexed from Nicaea) shows rising
 *   extraction and theater as the coordination function stabilizes but
 *   enforcement persists.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(homoousios_christology__pro_nicene_reading, 0.78).
domain_priors:suppression_score(homoousios_christology__pro_nicene_reading, 0.85).
domain_priors:theater_ratio(homoousios_christology__pro_nicene_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(homoousios_christology__pro_nicene_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(homoousios_christology__pro_nicene_reading, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(homoousios_christology__pro_nicene_reading, theater_ratio, 0.35).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(homoousios_christology__pro_nicene_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(homoousios_christology__pro_nicene_reading, resistance, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(homoousios_christology__pro_nicene_reading, tangled_rope).
narrative_ontology:human_readable(homoousios_christology__pro_nicene_reading, "Nicene Homoousios Confession (Pro-Nicene Reading)").
narrative_ontology:topic_domain(homoousios_christology__pro_nicene_reading, "historical_theology/ecclesiastical_politics").

domain_priors:requires_active_enforcement(homoousios_christology__pro_nicene_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(homoousios_christology__pro_nicene_reading, 'a9773c15-b622-484d-920c-e54ce8117b7b').
narrative_ontology:cs_kernel_codification('a9773c15-b622-484d-920c-e54ce8117b7b', fixed_text).
narrative_ontology:cs_authority_grounding('a9773c15-b622-484d-920c-e54ce8117b7b', lineage).
narrative_ontology:cs_interpretation_layer_present('a9773c15-b622-484d-920c-e54ce8117b7b').
narrative_ontology:cs_reading_relation('a9773c15-b622-484d-920c-e54ce8117b7b', homoousios_christology__arian_reading, forecloses).
narrative_ontology:cs_reading_relation('a9773c15-b622-484d-920c-e54ce8117b7b', homoousios_christology__semi_arian_reading, influences).
narrative_ontology:cs_axiom('a9773c15-b622-484d-920c-e54ce8117b7b', foundational, christ_is_homoousios_with_father).
narrative_ontology:cs_axiom_status(christ_is_homoousios_with_father, holdable).
narrative_ontology:cs_axiom_grounding('a9773c15-b622-484d-920c-e54ce8117b7b', christ_is_homoousios_with_father, theological).
narrative_ontology:cs_axiom('a9773c15-b622-484d-920c-e54ce8117b7b', secondary, the_father_and_son_share_one_divine_essence).
narrative_ontology:cs_axiom_status(the_father_and_son_share_one_divine_essence, holdable).
narrative_ontology:cs_axiom_grounding('a9773c15-b622-484d-920c-e54ce8117b7b', the_father_and_son_share_one_divine_essence, theological).
narrative_ontology:cs_reference_frame('a9773c15-b622-484d-920c-e54ce8117b7b', nicene_orthodoxy).
narrative_ontology:cs_drift_state('a9773c15-b622-484d-920c-e54ce8117b7b', chalcedonian_reaffirmation, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('a9773c15-b622-484d-920c-e54ce8117b7b', '').
narrative_ontology:cs_kernel_id(homoousios_christology__pro_nicene_reading, homoousios_christology).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(homoousios_christology__pro_nicene_reading, imperial_church_hierarchy).
narrative_ontology:constraint_beneficiary(homoousios_christology__pro_nicene_reading, nicene_bishops).
narrative_ontology:constraint_beneficiary(homoousios_christology__pro_nicene_reading, roman_emperor).
narrative_ontology:constraint_victim(homoousios_christology__pro_nicene_reading, arian_bishops).
narrative_ontology:constraint_victim(homoousios_christology__pro_nicene_reading, semi_arian_bishops).
narrative_ontology:constraint_victim(homoousios_christology__pro_nicene_reading, arian_laity).
narrative_ontology:constraint_victim(homoousios_christology__pro_nicene_reading, germanic_arian_churches).
narrative_ontology:constraint_vindicates(homoousios_christology__pro_nicene_reading, nicene_creed).
narrative_ontology:constraint_vindicates(homoousios_christology__pro_nicene_reading, trinitarian_orthodoxy).
narrative_ontology:constraint_vindicates(homoousios_christology__pro_nicene_reading, christological_orthodoxy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers the creed through councils, episcopal appointments, and anathema. Controls the definition of orthodoxy and the machinery of enforcement. Gains institutional coherence, imperial patronage, and jurisdictional authority from the constraint's operation.
narrative_ontology:constraint_stakeholder(homoousios_christology__pro_nicene_reading, imperial_church_hierarchy, agenda_setter,
    institutional, generational, analytical, universal).
narrative_ontology:stakeholder_secondary_role(homoousios_christology__pro_nicene_reading, imperial_church_hierarchy, beneficiary).

% Secure their sees, imperial favor, and ecclesiastical careers by confessing homoousios. Those who refuse are deposed; those who conform gain access to the imperial church's resources and authority networks. Exit means deposition and exile — constrained by the very structure they uphold.
narrative_ontology:constraint_stakeholder(homoousios_christology__pro_nicene_reading, nicene_bishops, beneficiary,
    organized, biographical, constrained, continental).

% Uses the creed as an instrument of imperial unity — a single confession binding the empire's churches. Gains political coherence and a unified ecclesiastical partner. Can pivot between Arian and Nicene factions (Constantius II, Valens vs. Theodosius I), making exit from any single theological settlement arbitrage-grade.
narrative_ontology:constraint_stakeholder(homoousios_christology__pro_nicene_reading, roman_emperor, beneficiary,
    institutional, generational, arbitrage, universal).

% Deposed, exiled, and barred from episcopal office for refusing homoousios. Their churches are confiscated; their flocks pressured to conform. Exit from the imperial church means loss of legal standing, property, and public worship — trapped by the enforcement machinery they oppose.
narrative_ontology:constraint_stakeholder(homoousios_christology__pro_nicene_reading, arian_bishops, payer,
    organized, biographical, trapped, continental).

% Hold homoiousios (similar substance) as a compromise. Pressured by both sides: Nicenes demand full homoousios; Arians reject any concession. Many eventually conform to homoousios under Theodosius I to retain their sees — constrained exit, paying the cost of theological compromise.
narrative_ontology:constraint_stakeholder(homoousios_christology__pro_nicene_reading, semi_arian_bishops, payer,
    organized, biographical, constrained, continental).

% Forced to attend Nicene parishes or worship clandestinely. Their clergy are replaced; their meeting places confiscated. No meaningful exit within the empire — trapped by geography, law, and the parish structure that enforces conformity.
narrative_ontology:constraint_stakeholder(homoousios_christology__pro_nicene_reading, arian_laity, payer,
    powerless, biographical, trapped, local).

% Maintain Arian confession outside direct imperial enforcement (Gothic, Vandal, Lombard kingdoms). Would object to homoousios as imperial imposition but are structurally excluded from the ecumenical councils that define orthodoxy. Their political power does not translate into theological voice within the imperial church.
narrative_ontology:constraint_stakeholder(homoousios_christology__pro_nicene_reading, germanic_arian_churches, excluded,
    powerful, generational, trapped, continental).

% Basil of Caesarea, Gregory of Nazianzus, Gregory of Nyssa — theological architects who formulated the pro-Nicene trinitarian grammar. They analyze the constraint from inside the tradition, providing the intellectual infrastructure that makes homoousios defensible. Neither coerced nor collecting rents; their exit is analytical (they could have pursued other theological projects).
narrative_ontology:constraint_stakeholder(homoousios_christology__pro_nicene_reading, cappadocian_fathers, observer,
    organized, biographical, analytical, regional).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Unified christological confession enabling imperial theological unity, ecclesiastical communion across sees, and a common boundary between orthodoxy and heresy — solving the coordination problem of doctrinal fragmentation that threatened both church cohesion and imperial stability.
% TRANSFER_FUNCTION: Moves ecclesiastical authority, imperial patronage, career advancement, and legal standing from dissenting bishops (Arian, semi-Arian) to conforming Nicene bishops; moves theological legitimacy from contested christologies to the homoousios formula; moves the laity's worship life from pluralistic practice to enforced uniformity.
% ABSENT_VOICES: Arian laity outside the empire (Gothic, Vandal territories) who maintained their confession without imperial coercion; Jewish and pagan critics who regarded the entire controversy as incoherent speculation; monastic communities (e.g., Egyptian desert) who largely avoided the controversy but were later co-opted into enforcement; women in the imperial household (e.g., Constantia, sister of Constantius II) who influenced theology but held no formal seat.
% DISAPPEARANCE_RATIONALE: If the homoousios enforcement vanished overnight, the imperial church would fragment into competing christological communions; the episcopal hierarchy would lose its coercive unity; the emperor would lose a primary instrument of ideological integration; Germanic Arian kingdoms would gain theological parity; the entire ecclesiastical-political order of the late Roman Empire would restructure.
% FOUNDING_PROBLEM: The Arian controversy (c. 318 onward) threatened to split the church and destabilize the empire: Arius taught the Son was created and subordinate, while Alexander of Alexandria insisted on eternal generation. Constantine convened Nicaea (325) to impose a single formula that would define orthodoxy, exclude heresy, and restore imperial-church unity.
% FOUNDING_PROBLEM_CORROBORATION: Athanasius and the Cappadocian Fathers attest the problem was real and theological (from within the beneficiary set). Ammianus Marcellinus (pagan historian) and the Arian historian Philostorgius attest the controversy was politically exploited by emperors and bishops for control (from outside the beneficiary set). Modern scholarship (e.g., R.P.C. Hanson, Rowan Williams) corroborates both the genuine theological stakes and the political instrumentalization.
narrative_ontology:disappearance_verdict(homoousios_christology__pro_nicene_reading, world_rearranges).
narrative_ontology:founding_problem_status(homoousios_christology__pro_nicene_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(homoousios_christology__pro_nicene_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(homoousios_christology__pro_nicene_reading, 'none', 1).
narrative_ontology:epsilon_provenance(homoousios_christology__pro_nicene_reading, 0.78, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(homoousios_christology__pro_nicene_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(homoousios_christology__pro_nicene_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(homoousios_christology__pro_nicene_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.78) is high because the constraint moves substantial resources (episcopal offices, imperial patronage, legal standing, property) from dissenters to conformists. Suppression (0.85) is very high because persistence depends on active exclusion — anathema, deposition, exile, and the Theodosian Code's heresy laws. Theater (0.35) is moderate: genuine theological conviction (Cappadocian formulation) coexists with political instrumentalization (Theodosius using orthodoxy for imperial unity). Accessibility collapse (0.72) is high — Arianism as a public option collapses within the empire by 381, though it persists among Germanic kingdoms. Resistance (0.48) is moderate — intense during the Arian ascendancy (337–381) but diminishing after Theodosius. The claim (tangled_rope) and metrics are authored independently; the engine will compute per-seat types from the structural data.
 *
 * PERSPECTIVAL GAP:
 *   From the imperial church hierarchy's seat, the constraint is genuine coordination — a divinely revealed truth that unifies the church. From the Arian bishop's seat, it is enforced extraction — a political tool that destroys their ministry. From the semi-Arian bishop's seat, it is a narrowing vise — the compromise position is eliminated by both sides. The engine computes these divergent seat types from the stakeholder power/exit data; the authored claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   The imperial church hierarchy and emperor are structural beneficiaries (d near 0.0) — they collect authority, unity, and revenue from the constraint. Nicene bishops are beneficiaries (d ~0.15) — they gain career security and status. Arian bishops are full targets (d ~0.95) — deposed, exiled, property seized. Semi-Arian bishops are high-target (d ~0.75) — pressured to conform or lose office. Arian laity are trapped targets (d ~0.9) — no exit, forced conformity. Germanic Arian churches are excluded (no d computation — not subject to the constraint's enforcement). Cappadocian fathers are analytical observers (d = 0.5). The engine derives d from these structural positions plus exit options.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (Arian controversy threatening imperial unity) was arguably resolved by 381, yet the constraint persists and intensifies. The mandate has not been formally sunset; instead, the enforcement machinery expands (Theodosian Code, later Justinian). This is mandatrophy: the coordination function (unity against Arianism) atrophies as Arianism ceases to be an internal threat, but the constraint persists as a boundary-maintenance mechanism for ecclesiastical authority. The pro-Nicene reading treats the founding problem as live (heresy always recurs); the structural data shows it as dead (no internal Arian threat after 381). The mismatch flags capture.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    coordination_extraction_boundary,
    'Is the homoousios formula genuinely necessary for ecclesiastical communion and trinitarian coherence, or is it a politically convenient boundary marker that extracts conformity?',
    'Counterfactual historical analysis: if the empire had adopted homoiousios (as nearly happened under Constantius II), would trinitarian theology and church unity have collapsed, or stabilized differently? Comparative study of non-Chalcedonian churches that use different christological formulas.',
    'If the formula is genuinely necessary for coherence, the coordination function is structural and extraction is the price of unity. If it is politically contingent, the measured extraction is largely rent-seeking riding on a manufactured consensus.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coordination_extraction_boundary, conceptual, 'Whether the homoousios/homoiousios distinction is theologically indispensable or politically constructed.').

omega_variable(
    enforcement_mechanism_nature,
    'Is the measured suppression structural (imperial law, episcopal courts) or internalized (theological conscience, identity fusion with orthodoxy)?',
    'Post-exit suppression trajectory: when Germanic Arian kingdoms converted to Nicene Christianity (e.g., Visigoths 589, Lombards 7th c.), did suppression persist as internalized conformity, or did it dissolve with the structural enforcement? Analysis of dissent persistence in Monophysite/Miaphysite churches outside imperial enforcement.',
    'If internalized, effective suppression is higher than structural measure — the constraint travels with the agent after exit. If purely structural, the constraint''s power is bound to the imperial enforcement machinery.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_mechanism_nature, empirical, 'Structural vs. internalized suppression mechanism in theological conformity.').

omega_variable(
    kernel_reading_identity,
    'Does this constraint story accurately capture the pro-Nicene reading as a distinct structural entity, or does it conflate the reading with the kernel itself?',
    'Cross-reading comparison: generate parallel constraint stories for arian_reading and semi_arian_reading with their own ε, beneficiaries, victims, and cs_structure. Verify that each reading''s structural profile (extraction, suppression, beneficiary/victim sets) is stable and distinct. Confirm that the pro-Nicene reading''s high ε and active enforcement are properties of THIS reading''s institutional instantiation, not of the kernel abstractly.',
    'If the readings cannot be cleanly separated into distinct constraint stories with stable metrics, the kernel decomposition is invalid and the ε-invariance principle is violated. If they separate cleanly, the pro-Nicene reading''s classification as tangled_rope stands on its own structural merits.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Whether the kernel-reading decomposition yields structurally distinct constraints per the ε-invariance principle.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(homoousios_christology__pro_nicene_reading, 0, 126).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(homoousios_pro_nicene_tr_t0, homoousios_christology__pro_nicene_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement(homoousios_pro_nicene_tr_t30, homoousios_christology__pro_nicene_reading, theater_ratio, 30, 0.24).
narrative_ontology:measurement(homoousios_pro_nicene_tr_t60, homoousios_christology__pro_nicene_reading, theater_ratio, 60, 0.31).
narrative_ontology:measurement(homoousios_pro_nicene_tr_t90, homoousios_christology__pro_nicene_reading, theater_ratio, 90, 0.35).
narrative_ontology:measurement(homoousios_pro_nicene_tr_t126, homoousios_christology__pro_nicene_reading, theater_ratio, 126, 0.35).

% Extraction over time
narrative_ontology:measurement(homoousios_pro_nicene_be_t0, homoousios_christology__pro_nicene_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(homoousios_pro_nicene_be_t30, homoousios_christology__pro_nicene_reading, base_extractiveness, 30, 0.58).
narrative_ontology:measurement(homoousios_pro_nicene_be_t60, homoousios_christology__pro_nicene_reading, base_extractiveness, 60, 0.72).
narrative_ontology:measurement(homoousios_pro_nicene_be_t90, homoousios_christology__pro_nicene_reading, base_extractiveness, 90, 0.76).
narrative_ontology:measurement(homoousios_pro_nicene_be_t126, homoousios_christology__pro_nicene_reading, base_extractiveness, 126, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(homoousios_pro_nicene_su_t0, homoousios_christology__pro_nicene_reading, suppression_requirement, 0, 0.68).
narrative_ontology:measurement(homoousios_pro_nicene_su_t30, homoousios_christology__pro_nicene_reading, suppression_requirement, 30, 0.78).
narrative_ontology:measurement(homoousios_pro_nicene_su_t60, homoousios_christology__pro_nicene_reading, suppression_requirement, 60, 0.84).
narrative_ontology:measurement(homoousios_pro_nicene_su_t90, homoousios_christology__pro_nicene_reading, suppression_requirement, 90, 0.85).
narrative_ontology:measurement(homoousios_pro_nicene_su_t126, homoousios_christology__pro_nicene_reading, suppression_requirement, 126, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(homoousios_christology__pro_nicene_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(homoousios_christology__pro_nicene_reading, 0.08).
narrative_ontology:affects_constraint(homoousios_christology__pro_nicene_reading, chalcedonian_christology).
narrative_ontology:affects_constraint(homoousios_christology__pro_nicene_reading, trinitarian_orthodoxy).
narrative_ontology:affects_constraint(homoousios_christology__pro_nicene_reading, imperial_heresy_law).

% DUAL FORMULATION NOTE:
% This story (pro_nicene_reading) is one of three in the homoousios_christology constraint family. The arian_reading and semi_arian_reading instantiate different constraints with different beneficiary/victim structures and ε values. The pro-Nicene reading's high enforcement ε and imperial-church alignment benefits are structurally distinct from the Arian reading's lower enforcement (when dominant) and different beneficiary set. All three stories link via network.affects_constraints to enable contamination analysis across the family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
