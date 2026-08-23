% ============================================================================
% CONSTRAINT STORY: feudal_oath_reciprocity__ecclesiastical_mediation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_feudal_oath_reciprocity__ecclesiastical_mediation_reading, []).

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
 *   constraint_id: feudal_oath_reciprocity__ecclesiastical_mediation_reading
 *   human_readable: Feudal Oath under Ecclesiastical Mediation
 *   domain: medieval_political_economy
 *
 * SUMMARY:
 *   This constraint story instantiates the ecclesiastical_mediation_reading
 *   of the contested kernel feudal_oath_reciprocity. Under this reading, the
 *   feudal oath is not merely a secular contract of service and reward but a
 *   sacramental act bound by Christian charity, enforceable through
 *   ecclesiastical courts and the threat of spiritual sanctions. The
 *   arrangement coordinates decentralized warrior society by capping
 *   arbitrary violence, while asymmetrically extracting interpretive
 *   authority for the Church and limiting secular lords' extraction capacity.
 *   It is claimed as tangled_rope: genuine coordination (sacred reciprocal
 *   stabilization) fused with asymmetric extraction (clerical jurisdiction
 *   and lordly constraint).
 *
 * KEY AGENTS:
 *   - Ecclesiastical authority (papal curia, bishops, canon lawyers): agenda-setter and beneficiary â claims jurisdiction over oath interpretation and enforcement.
 *   - Secular lords (nobility): payer/target â extraction capacity is bounded by theological limits and church court oversight.
 *   - Vassal knights: beneficiary â receive protected tenure and a recognized avenue against excessive demands, though still subordinate.
 *   - Peasant communities: excluded â bear material extraction but are invisible to the oath framework.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(feudal_oath_reciprocity__ecclesiastical_mediation_reading, 0.58).
domain_priors:suppression_score(feudal_oath_reciprocity__ecclesiastical_mediation_reading, 0.52).
domain_priors:theater_ratio(feudal_oath_reciprocity__ecclesiastical_mediation_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(feudal_oath_reciprocity__ecclesiastical_mediation_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(feudal_oath_reciprocity__ecclesiastical_mediation_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(feudal_oath_reciprocity__ecclesiastical_mediation_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(feudal_oath_reciprocity__ecclesiastical_mediation_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(feudal_oath_reciprocity__ecclesiastical_mediation_reading, resistance, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(feudal_oath_reciprocity__ecclesiastical_mediation_reading, tangled_rope).
narrative_ontology:human_readable(feudal_oath_reciprocity__ecclesiastical_mediation_reading, "Feudal Oath under Ecclesiastical Mediation").
narrative_ontology:topic_domain(feudal_oath_reciprocity__ecclesiastical_mediation_reading, "medieval_political_economy").

domain_priors:requires_active_enforcement(feudal_oath_reciprocity__ecclesiastical_mediation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(feudal_oath_reciprocity__ecclesiastical_mediation_reading, '2039de5e-f3a7-4a48-8ec7-74a6e441fed1').
narrative_ontology:cs_kernel_codification('2039de5e-f3a7-4a48-8ec7-74a6e441fed1', fixed_text).
narrative_ontology:cs_authority_grounding('2039de5e-f3a7-4a48-8ec7-74a6e441fed1', lineage).
narrative_ontology:cs_interpretation_layer_present('2039de5e-f3a7-4a48-8ec7-74a6e441fed1').
narrative_ontology:cs_reading_relation('2039de5e-f3a7-4a48-8ec7-74a6e441fed1', feudal_oath_reciprocity__lord_extraction_reading, influences).
narrative_ontology:cs_reading_relation('2039de5e-f3a7-4a48-8ec7-74a6e441fed1', feudal_oath_reciprocity__vassal_coordination_reading, coexists_with).
narrative_ontology:cs_axiom('2039de5e-f3a7-4a48-8ec7-74a6e441fed1', foundational, sacramental_oath_binds_secular_conscience).
narrative_ontology:cs_axiom_status(sacramental_oath_binds_secular_conscience, holdable).
narrative_ontology:cs_axiom_grounding('2039de5e-f3a7-4a48-8ec7-74a6e441fed1', sacramental_oath_binds_secular_conscience, theological).
narrative_ontology:cs_axiom('2039de5e-f3a7-4a48-8ec7-74a6e441fed1', foundational, christian_charity_limits_extraction).
narrative_ontology:cs_axiom_status(christian_charity_limits_extraction, holdable).
narrative_ontology:cs_axiom_grounding('2039de5e-f3a7-4a48-8ec7-74a6e441fed1', christian_charity_limits_extraction, deontological).
narrative_ontology:cs_reference_frame('2039de5e-f3a7-4a48-8ec7-74a6e441fed1', sacramental_kinship_framework).
narrative_ontology:cs_drift_state('2039de5e-f3a7-4a48-8ec7-74a6e441fed1', high_medieval_papal_monarchy, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('2039de5e-f3a7-4a48-8ec7-74a6e441fed1', '').
narrative_ontology:cs_kernel_id(feudal_oath_reciprocity__ecclesiastical_mediation_reading, feudal_oath_reciprocity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(feudal_oath_reciprocity__ecclesiastical_mediation_reading, ecclesiastical_authority).
narrative_ontology:constraint_beneficiary(feudal_oath_reciprocity__ecclesiastical_mediation_reading, vassal_knights).
narrative_ontology:constraint_victim(feudal_oath_reciprocity__ecclesiastical_mediation_reading, secular_lords).
narrative_ontology:constraint_vindicates(feudal_oath_reciprocity__ecclesiastical_mediation_reading, christian_reciprocity_doctrine).
narrative_ontology:constraint_vindicates(feudal_oath_reciprocity__ecclesiastical_mediation_reading, canon_law_jurisdiction).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers the theology of sacramental oaths and claims jurisdiction over oath-breaking through canon law, ecclesiastical courts, and spiritual sanctions. Gains interpretive authority over feudal relationships, extending moral and legal oversight into secular lord-vassal bonds across Latin Christendom.
narrative_ontology:constraint_stakeholder(feudal_oath_reciprocity__ecclesiastical_mediation_reading, ecclesiastical_authority, agenda_setter,
    institutional, generational, arbitrage, continental).
narrative_ontology:stakeholder_secondary_role(feudal_oath_reciprocity__ecclesiastical_mediation_reading, ecclesiastical_authority, beneficiary).

% Swear and receive oaths of fealty and homage. Their capacity to extract surplus from vassal tenures is bounded by theological claims of Christian charity and the threat of ecclesiastical censure; they cannot maximize extraction without risking excommunication, interdict, or loss of honorable standing.
narrative_ontology:constraint_stakeholder(feudal_oath_reciprocity__ecclesiastical_mediation_reading, secular_lords, payer,
    powerful, biographical, constrained, regional).

% Swear oaths in exchange for fiefs and protection. Benefit from theological limits on lordly demands that provide a recognized, if irregular, basis for appealing excessive extraction to ecclesiastical arbiters; remain subordinate but are not maximally extractable under the sacramental frame.
narrative_ontology:constraint_stakeholder(feudal_oath_reciprocity__ecclesiastical_mediation_reading, vassal_knights, beneficiary,
    moderate, biographical, constrained, regional).

% Bear the material weight of both lordly extraction and ecclesiastical tithes. Their voices are absent from oath ceremonies and canon law proceedings; they experience the constraint only as a distant, rarely enforced cap on violence that does not alter their daily surplus labor.
narrative_ontology:constraint_stakeholder(feudal_oath_reciprocity__ecclesiastical_mediation_reading, peasant_communities, excluded,
    powerless, immediate, trapped, local).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared sacred framework that binds lords and vassals in a reciprocal relationship, reducing arbitrary violence and establishing predictable mutual obligations through Christian kinship language sworn before God.
% TRANSFER_FUNCTION: Moves interpretive authority over feudal obligations from secular custom to ecclesiastical courts and moral theologians; moves surplus extraction potential from secular lords to vassals (via theological caps on demand) and to the Church (via expanded jurisdiction and legitimacy).
% ABSENT_VOICES: Peasant laborers and non-knightly cultivators are structurally excluded from oath ceremonies and canonical proceedings; their experience of extraction is invisible to the sacramental framework. Vernacular customary courts that might resist Latin clerical interpretation are marginalized.
% DISAPPEARANCE_RATIONALE: Without the sacramental oath framework, secular lords would face fewer theological constraints on surplus extraction, vassals would lose a recognized avenue of appeal against arbitrary demands, and the Church would lose its central institutional role in feudal legitimation; the political economy would reorganize around purely secular or customary obligation.
% FOUNDING_PROBLEM: Post-Carolingian decentralization created endemic local violence and arbitrary lordly extraction without a centralized enforcement mechanism; the Church offered a moral and juridical framework to cap violence and stabilize reciprocal expectations.
% FOUNDING_PROBLEM_CORROBORATION: Ecclesiastical chroniclers and canon lawyers attest the problem was limiting blood-feud and arbitrary violence. Modern legal historians and non-ecclesiastical charter evidence contest whether the sacramental framework was designed to protect vassals or primarily to extend church jurisdiction; corroboration from beneficiary-external secular sources in the period is sparse.
narrative_ontology:disappearance_verdict(feudal_oath_reciprocity__ecclesiastical_mediation_reading, world_rearranges).
narrative_ontology:founding_problem_status(feudal_oath_reciprocity__ecclesiastical_mediation_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(feudal_oath_reciprocity__ecclesiastical_mediation_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(feudal_oath_reciprocity__ecclesiastical_mediation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(feudal_oath_reciprocity__ecclesiastical_mediation_reading, 0.58, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(feudal_oath_reciprocity__ecclesiastical_mediation_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(feudal_oath_reciprocity__ecclesiastical_mediation_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(feudal_oath_reciprocity__ecclesiastical_mediation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58) is moderate because the Church gains real interpretive authority and lords lose extraction latitude, but the coordination function (reciprocal stabilization) is also genuine. Suppression (0.52) reflects active enforcement through ecclesiastical courts, excommunication, and interdict, moderated by the difficulty of universal surveillance. Theater ratio (0.45) captures the highly ritualized, performative character of oath-swearing and public penance, which is functional but carries symbolic overhead. Accessibility collapse (0.68) is high because once the sacramental frame is accepted, purely secular contract alternatives lose legitimacy. Resistance (0.48) reflects documented secular pushback (investiture controversies, royal assertions of autonomy). The measurement series run on one shared time grid so every metric is authored at every examined time point.
 *
 * PERSPECTIVAL GAP:
 *   The ecclesiastical seat experiences the constraint as a rope: it solves violence and disorder through sacred coordination. The secular lord seat experiences it as a snare or tangled rope: it extracts surplus capacity and subjects secular power to clerical oversight. The vassal seat experiences partial coordination benefit with residual extraction. The engine computes this divergence from structural data; the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Ecclesiastical authority is a beneficiary with low directionality (gains jurisdiction, subsidized by the constraint). Secular lords are targets with high directionality (lose extraction capacity). Vassal knights sit between beneficiary and symmetric: they gain protected tenure but still render service. Peasant communities are excluded from the directional calculus entirely.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem â decentralized violence without centralized enforcement â was substantially addressed by the high medieval period through the growth of royal justice and territorial administration. However, the ecclesiastical interpretive apparatus persisted and even expanded (Gregorian reform, papal monarchy), suggesting partial mandatrophy: the coordination function was increasingly absorbed by state structures, while the Church's interpretive claims persisted as institutional inertia and theological theater. The temporal measurements show rising theater and extraction through 1200, with slight decline by 1300 as secular states reassert autonomy.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_position,
    'This constraint is the ecclesiastical_mediation_reading of kernel feudal_oath_reciprocity. A sibling lord_extraction_reading would treat the same oath as authorizing maximal lordly extraction, while a vassal_coordination_reading would treat it as fixed charter reciprocity with minimal church involvement. Does the ecclesiastical interpretive layer genuinely constrain lords, or does it provide theological cover for extraction?',
    'Cross-reading comparison: if ecclesiastical court records show consistent reduction of lordly demands, the reading is constraining; if they show rubber-stamping of lordly claims, the reading is cover.',
    'If the latter, effective extractiveness shifts toward the church as snare-agenda-setter; if the former, the tangled-rope classification holds with church as coordinator.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_position, conceptual, 'Structural ambiguity between ecclesiastical constraint and ecclesiastical cover').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is enforcement of the sacramental oath primarily structural (ecclesiastical courts, interdict, and material sanctions) or internalized (fear of damnation, honor loss, and conscience)?',
    'Comparative analysis of oath-compliance rates in regions under strong versus weak church court presence; if compliance persists absent court capacity, suppression is partly internalized.',
    'Internalized suppression raises effective extraction beyond the structural measure because the target carries the constraint after external enforcement fades.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs internalized enforcement mechanism').

omega_variable(
    mandatrophy_boundary,
    'Did the sacramental oath framework outlive the decentralized violence problem it was built to solve?',
    'Correlate the rise of centralized territorial states and royal justice with declining ecclesiastical feudal jurisdiction; if church oversight persists after state justice supplants it, mandatrophy is present.',
    'A dead founding problem with persistent authority structures would indicate a drift toward piton or snare classification in later periods.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mandatrophy_boundary, empirical, 'Whether the constraint''s founding problem is still live').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(feudal_oath_reciprocity__ecclesiastical_mediation_reading, 900, 1300).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(feud_tr_t900, feudal_oath_reciprocity__ecclesiastical_mediation_reading, theater_ratio, 900, 0.3).
narrative_ontology:measurement(feud_tr_t950, feudal_oath_reciprocity__ecclesiastical_mediation_reading, theater_ratio, 950, 0.32).
narrative_ontology:measurement(feud_tr_t1000, feudal_oath_reciprocity__ecclesiastical_mediation_reading, theater_ratio, 1000, 0.35).
narrative_ontology:measurement(feud_tr_t1050, feudal_oath_reciprocity__ecclesiastical_mediation_reading, theater_ratio, 1050, 0.38).
narrative_ontology:measurement(feud_tr_t1100, feudal_oath_reciprocity__ecclesiastical_mediation_reading, theater_ratio, 1100, 0.42).
narrative_ontology:measurement(feud_tr_t1150, feudal_oath_reciprocity__ecclesiastical_mediation_reading, theater_ratio, 1150, 0.46).
narrative_ontology:measurement(feud_tr_t1200, feudal_oath_reciprocity__ecclesiastical_mediation_reading, theater_ratio, 1200, 0.5).
narrative_ontology:measurement(feud_tr_t1250, feudal_oath_reciprocity__ecclesiastical_mediation_reading, theater_ratio, 1250, 0.48).
narrative_ontology:measurement(feud_tr_t1300, feudal_oath_reciprocity__ecclesiastical_mediation_reading, theater_ratio, 1300, 0.45).

% Extraction over time
narrative_ontology:measurement(feud_be_t900, feudal_oath_reciprocity__ecclesiastical_mediation_reading, base_extractiveness, 900, 0.45).
narrative_ontology:measurement(feud_be_t950, feudal_oath_reciprocity__ecclesiastical_mediation_reading, base_extractiveness, 950, 0.48).
narrative_ontology:measurement(feud_be_t1000, feudal_oath_reciprocity__ecclesiastical_mediation_reading, base_extractiveness, 1000, 0.52).
narrative_ontology:measurement(feud_be_t1050, feudal_oath_reciprocity__ecclesiastical_mediation_reading, base_extractiveness, 1050, 0.55).
narrative_ontology:measurement(feud_be_t1100, feudal_oath_reciprocity__ecclesiastical_mediation_reading, base_extractiveness, 1100, 0.58).
narrative_ontology:measurement(feud_be_t1150, feudal_oath_reciprocity__ecclesiastical_mediation_reading, base_extractiveness, 1150, 0.6).
narrative_ontology:measurement(feud_be_t1200, feudal_oath_reciprocity__ecclesiastical_mediation_reading, base_extractiveness, 1200, 0.62).
narrative_ontology:measurement(feud_be_t1250, feudal_oath_reciprocity__ecclesiastical_mediation_reading, base_extractiveness, 1250, 0.6).
narrative_ontology:measurement(feud_be_t1300, feudal_oath_reciprocity__ecclesiastical_mediation_reading, base_extractiveness, 1300, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(feud_su_t900, feudal_oath_reciprocity__ecclesiastical_mediation_reading, suppression_requirement, 900, 0.4).
narrative_ontology:measurement(feud_su_t950, feudal_oath_reciprocity__ecclesiastical_mediation_reading, suppression_requirement, 950, 0.42).
narrative_ontology:measurement(feud_su_t1000, feudal_oath_reciprocity__ecclesiastical_mediation_reading, suppression_requirement, 1000, 0.45).
narrative_ontology:measurement(feud_su_t1050, feudal_oath_reciprocity__ecclesiastical_mediation_reading, suppression_requirement, 1050, 0.48).
narrative_ontology:measurement(feud_su_t1100, feudal_oath_reciprocity__ecclesiastical_mediation_reading, suppression_requirement, 1100, 0.52).
narrative_ontology:measurement(feud_su_t1150, feudal_oath_reciprocity__ecclesiastical_mediation_reading, suppression_requirement, 1150, 0.55).
narrative_ontology:measurement(feud_su_t1200, feudal_oath_reciprocity__ecclesiastical_mediation_reading, suppression_requirement, 1200, 0.58).
narrative_ontology:measurement(feud_su_t1250, feudal_oath_reciprocity__ecclesiastical_mediation_reading, suppression_requirement, 1250, 0.55).
narrative_ontology:measurement(feud_su_t1300, feudal_oath_reciprocity__ecclesiastical_mediation_reading, suppression_requirement, 1300, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(feudal_oath_reciprocity__ecclesiastical_mediation_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(feudal_oath_reciprocity__ecclesiastical_mediation_reading, 0.08).
narrative_ontology:affects_constraint(feudal_oath_reciprocity__ecclesiastical_mediation_reading, lord_extraction_reading).
narrative_ontology:affects_constraint(feudal_oath_reciprocity__ecclesiastical_mediation_reading, vassal_coordination_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the feudal_oath_reciprocity kernel. The three readings (ecclesiastical_mediation_reading, lord_extraction_reading, vassal_coordination_reading) share the same historical practices but instantiate structurally distinct constraints with different epsilon values and stakeholder directionalities.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
