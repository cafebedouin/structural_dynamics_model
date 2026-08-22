% ============================================================================
% CONSTRAINT STORY: magna_carta_clause_39__originalist_limitation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_magna_carta_clause_39__originalist_limitation_reading, []).

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
 *   constraint_id: magna_carta_clause_39__originalist_limitation_reading
 *   human_readable: Clause 39 Originalist Limitation: 13th-Century Baronial Protection
 *   domain: constitutional/legal/historical
 *
 * SUMMARY:
 *   This constraint instantiates the originalist_limitation_reading of the
 *   magna_carta_clause_39 kernel. It treats Clause 39 as procedurally binding
 *   King John only in the specific documented abuses that provoked the 1215
 *   baronial rebellionâarbitrary imprisonment, dispossession, outlawing,
 *   and exile of the baronial leadership without judgment by peers. The
 *   reading rejects universal due process extension and confines the clause's
 *   operation to the feudal compact negotiated at Runnymede. Key agents
 *   include the english_crown (whose prerogative is curtailed), the
 *   baronial_leadership (who receive narrow procedural protection), and
 *   wider_english_subjects (structurally excluded from the clause's
 *   safeguards under this reading).
 *
 * KEY AGENTS:
 *   - english_crown: Primary payer (institutional/constrained) â bears the loss of absolute prerogative over the baronial class
 *   - baronial_leadership: Primary beneficiary (organized/constrained) â receives judgment-by-peers protection for documented 1215 grievances
 *   - wider_english_subjects: Excluded party (powerless/trapped) â falls outside the narrow scope of the originalist reading
 *   - legal_historians: Analytical observer (analytical/analytical) â reconstructs the 1215 context without entering the feudal calculus
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(magna_carta_clause_39__originalist_limitation_reading, 0.42).
domain_priors:suppression_score(magna_carta_clause_39__originalist_limitation_reading, 0.55).
domain_priors:theater_ratio(magna_carta_clause_39__originalist_limitation_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(magna_carta_clause_39__originalist_limitation_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(magna_carta_clause_39__originalist_limitation_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(magna_carta_clause_39__originalist_limitation_reading, theater_ratio, 0.35).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(magna_carta_clause_39__originalist_limitation_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(magna_carta_clause_39__originalist_limitation_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(magna_carta_clause_39__originalist_limitation_reading, tangled_rope).
narrative_ontology:human_readable(magna_carta_clause_39__originalist_limitation_reading, "Clause 39 Originalist Limitation: 13th-Century Baronial Protection").
narrative_ontology:topic_domain(magna_carta_clause_39__originalist_limitation_reading, "constitutional/legal/historical").

domain_priors:requires_active_enforcement(magna_carta_clause_39__originalist_limitation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(magna_carta_clause_39__originalist_limitation_reading, '5e4dbfcb-646a-46d0-adf0-8c44ceb90a07').
narrative_ontology:cs_kernel_codification('5e4dbfcb-646a-46d0-adf0-8c44ceb90a07', fixed_text).
narrative_ontology:cs_authority_grounding('5e4dbfcb-646a-46d0-adf0-8c44ceb90a07', lineage).
narrative_ontology:cs_interpretation_layer_present('5e4dbfcb-646a-46d0-adf0-8c44ceb90a07').
narrative_ontology:cs_reading_relation('5e4dbfcb-646a-46d0-adf0-8c44ceb90a07', magna_carta_clause_39__liberal_due_process_reading, forecloses).
narrative_ontology:cs_reading_relation('5e4dbfcb-646a-46d0-adf0-8c44ceb90a07', magna_carta_clause_39__feudal_prerogative_reading, coexists_with).
narrative_ontology:cs_axiom('5e4dbfcb-646a-46d0-adf0-8c44ceb90a07', foundational, baronial_judgment_by_peers).
narrative_ontology:cs_axiom_status(baronial_judgment_by_peers, holdable).
narrative_ontology:cs_axiom_grounding('5e4dbfcb-646a-46d0-adf0-8c44ceb90a07', baronial_judgment_by_peers, conventional).
narrative_ontology:cs_axiom('5e4dbfcb-646a-46d0-adf0-8c44ceb90a07', foundational, documented_1215_abuse_limitation).
narrative_ontology:cs_axiom_status(documented_1215_abuse_limitation, holdable).
narrative_ontology:cs_axiom_grounding('5e4dbfcb-646a-46d0-adf0-8c44ceb90a07', documented_1215_abuse_limitation, conventional).
narrative_ontology:cs_reference_frame('5e4dbfcb-646a-46d0-adf0-8c44ceb90a07', feudal_compact_of_runnymede).
narrative_ontology:cs_drift_state('5e4dbfcb-646a-46d0-adf0-8c44ceb90a07', post_confirmatio_cartarum_1297, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('5e4dbfcb-646a-46d0-adf0-8c44ceb90a07', '').
narrative_ontology:cs_kernel_id(magna_carta_clause_39__originalist_limitation_reading, magna_carta_clause_39).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(magna_carta_clause_39__originalist_limitation_reading, baronial_leadership).
narrative_ontology:constraint_victim(magna_carta_clause_39__originalist_limitation_reading, english_crown).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Retains broad feudal prerogative but is constrained from arbitrarily imprisoning, dispossessing, or exiling the baronial class without judgment by peers; the constraint extracts directly from royal authority. Exit is constrained by feudal oath, political legitimacy requirements, and the threat of baronial rebellion; the Crown sought papal annulment of the charter to escape the limitation.
narrative_ontology:constraint_stakeholder(magna_carta_clause_39__originalist_limitation_reading, english_crown, payer,
    institutional, generational, constrained, national).

% Receives procedural protection against the specific royal abuses documented in the 1215 contextâarbitrary imprisonment, dispossession, outlawing, and exileâsecured through the judgment of peers. Their collective bargaining power is the enforcement mechanism, but they remain bound by homage, fealty, and the feudal land tenure system.
narrative_ontology:constraint_stakeholder(magna_carta_clause_39__originalist_limitation_reading, baronial_leadership, beneficiary,
    organized, biographical, constrained, national).

% Common free men, unfree tenants, merchants, women, and non-baronial landholders fall outside the narrow scope of this reading; they remain fully exposed to royal prerogative and local seigneurial jurisdiction without access to the clause's procedural safeguards.
narrative_ontology:constraint_stakeholder(magna_carta_clause_39__originalist_limitation_reading, wider_english_subjects, excluded,
    powerless, biographical, trapped, local).

% Modern legal historians and originalist scholars who reconstruct the 1215 feudal context to determine the clause's bounded application; they neither collect from nor pay into the constraint but analyze its structural limitation to documented baronial grievances.
narrative_ontology:constraint_stakeholder(magna_carta_clause_39__originalist_limitation_reading, legal_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(magna_carta_clause_39__originalist_limitation_reading, baronial_leadership).
narrative_ontology:fixing_cost_class(magna_carta_clause_39__originalist_limitation_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Resolves the immediate crisis of the First Barons' War by binding King John to specific procedural constraints regarding the treatment of his baronial tenants-in-chief, replacing arbitrary royal will with judgment by peers for documented grievances.
% TRANSFER_FUNCTION: Moves security of person and property from the unchecked english_crown to the baronial_leadership, requiring established legal procedure before dispossession or imprisonment, but only within the narrow class of documented 1215 abuses.
% ABSENT_VOICES: Wider_english_subjects including common free men, merchants, unfree tenants, and women are structurally excluded from the Runnymede negotiation; they would have sought broader protections against arbitrary power had they been present.
% DISAPPEARANCE_RATIONALE: If the originalist limitation vanished, the specific procedural guarantees securing the 1215 baronial peace would disappear, permitting the Crown to resume the documented abusesâarbitrary imprisonment and dispossessionâthat provoked the rebellion, collapsing the immediate feudal settlement.
% FOUNDING_PROBLEM: King John's arbitrary exercise of royal prerogativeâimprisoning, dispossessing, outlawing, and exiling barons without lawful judgmentâhad destabilized the feudal order and provoked the First Barons' War.
% FOUNDING_PROBLEM_CORROBORATION: Chronicle sources such as Roger of Wendover and the Articles of the Barons attest to the grievances from outside the beneficiary seat; modern legal historians (e.g., J.C. Holt) corroborate that the clause addressed immediate, specific baronial grievances rather than establishing universal principles, speaking from an analytical seat outside the feudal beneficiary structure.
narrative_ontology:disappearance_verdict(magna_carta_clause_39__originalist_limitation_reading, world_rearranges).
narrative_ontology:founding_problem_status(magna_carta_clause_39__originalist_limitation_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(magna_carta_clause_39__originalist_limitation_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(magna_carta_clause_39__originalist_limitation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(magna_carta_clause_39__originalist_limitation_reading, 0.42, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(magna_carta_clause_39__originalist_limitation_reading_tests).
:- end_tests(magna_carta_clause_39__originalist_limitation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.42) because the constraint genuinely limits royal power but only in the narrow, documented ways recorded in the 1215 grievances. Suppression (0.55) reflects that the charter required baronial arms and repeated reissuance to enforce; King John and Innocent III actively resisted it. Theater_ratio (0.35 at interval end) rises modestly as later confirmations (e.g., 1297) ritualize the text while practice drifts from the original baronial grievances. Accessibility_collapse (0.60) is moderate because alternatives like full rebellion or absolute submission remained structurally imaginable, though the clause provided a specific legal path. Resistance (0.50) is significant due to Crown and papal opposition.
 *
 * PERSPECTIVAL GAP:
 *   The english_crown seat experiences the constraint as extraction of its divine and feudal prerogativeâa coerced concession obtained under duress at Runnymede. The baronial_leadership seat experiences it as necessary coordination securing feudal property and personal security against documented royal abuses. The wider_english_subjects seat experiences no protection at all, living under the full remaining weight of royal and seigneurial power. The engine computes this divergence from the structural data: beneficiary declarations, victim declarations, and differentiated exit options.
 *
 * DIRECTIONALITY LOGIC:
 *   The english_crown is the structural target (d near 1.0): the constraint directly extracts from royal prerogative by requiring judgment before action against the barons. The baronial_leadership is the structural beneficiary (d near 0.0): they receive the procedural safeguard. Wider_english_subjects are excluded (no directional role in the constraint's operationâthey are outside the arrangement). No override is needed; the derivation chain produces the correct directionalities from the beneficiary/victim declarations and the Crown's constrained exit versus the barons' collective-but-feudally-bound position.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problemâKing John's arbitrary abuse of the baronsâwas substantially addressed by the 1215 charter and subsequent confirmations. Under this originalist reading, the constraint is explicitly bounded to that dead problem; the reading does not claim ongoing universal validity. This prevents mislabeling the arrangement as a pure extraction mechanism (Snare) because the coordination function was genuine and specific, and it prevents mislabeling it as a permanent Rope because its function was always transitional to a specific baronial peace.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    originalist_beneficiary_scope,
    'Does the originalist reading of Clause 39 confine its benefits strictly to the baronial class named in 1215, or did ''liber homo'' already encompass a broader class of free tenants?',
    'Palaeographic and legal-historical analysis of ''liber homo'' usage in early thirteenth-century charters and comparable feudal texts.',
    'If ''liber homo'' was broader, the beneficiary set expands and extraction from the Crown increases; if strictly baronial, the constraint remains narrow as authored.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(originalist_beneficiary_scope, empirical, 'Ambiguity in the class of beneficiaries protected by the clause').

omega_variable(
    committer_sibling_foreclosure,
    'Does the originalist limitation reading logically foreclose the liberal due process reading within a single interpretive framework, or can they coexist as methodological alternatives?',
    'Analysis of whether a single court or scholar can simultaneously hold that the clause means only 1215 grievances and universal individual rights.',
    'If foreclosed, the kernel generates hard logical contradiction between readings; if coexisting, the dispute remains methodological rather than logical.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(committer_sibling_foreclosure, conceptual, 'Logical relationship between originalist and liberal readings').

omega_variable(
    enforcement_mechanism_1215,
    'Was the constraint''s persistence in the 1215â1300 interval driven by ongoing baronial coercion, or by Crown strategic concession?',
    'Review of reissuance patterns (1216, 1217, 1225) and baronial enforcement actions during the minority of Henry III.',
    'If primarily coerced, active enforcement is higher and the Tangled Rope classification is reinforced; if conceded, the constraint approaches a Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_mechanism_1215, empirical, 'Source of enforcement for the originalist constraint').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(magna_carta_clause_39__originalist_limitation_reading, 1215, 1300).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mc39_orig_tr_t1215, magna_carta_clause_39__originalist_limitation_reading, theater_ratio, 1215, 0.15).
narrative_ontology:measurement(mc39_orig_tr_t1230, magna_carta_clause_39__originalist_limitation_reading, theater_ratio, 1230, 0.18).
narrative_ontology:measurement(mc39_orig_tr_t1245, magna_carta_clause_39__originalist_limitation_reading, theater_ratio, 1245, 0.22).
narrative_ontology:measurement(mc39_orig_tr_t1260, magna_carta_clause_39__originalist_limitation_reading, theater_ratio, 1260, 0.2).
narrative_ontology:measurement(mc39_orig_tr_t1275, magna_carta_clause_39__originalist_limitation_reading, theater_ratio, 1275, 0.28).
narrative_ontology:measurement(mc39_orig_tr_t1290, magna_carta_clause_39__originalist_limitation_reading, theater_ratio, 1290, 0.32).
narrative_ontology:measurement(mc39_orig_tr_t1300, magna_carta_clause_39__originalist_limitation_reading, theater_ratio, 1300, 0.35).

% Extraction over time
narrative_ontology:measurement(mc39_orig_be_t1215, magna_carta_clause_39__originalist_limitation_reading, base_extractiveness, 1215, 0.45).
narrative_ontology:measurement(mc39_orig_be_t1230, magna_carta_clause_39__originalist_limitation_reading, base_extractiveness, 1230, 0.43).
narrative_ontology:measurement(mc39_orig_be_t1245, magna_carta_clause_39__originalist_limitation_reading, base_extractiveness, 1245, 0.42).
narrative_ontology:measurement(mc39_orig_be_t1260, magna_carta_clause_39__originalist_limitation_reading, base_extractiveness, 1260, 0.44).
narrative_ontology:measurement(mc39_orig_be_t1275, magna_carta_clause_39__originalist_limitation_reading, base_extractiveness, 1275, 0.4).
narrative_ontology:measurement(mc39_orig_be_t1290, magna_carta_clause_39__originalist_limitation_reading, base_extractiveness, 1290, 0.41).
narrative_ontology:measurement(mc39_orig_be_t1300, magna_carta_clause_39__originalist_limitation_reading, base_extractiveness, 1300, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(mc39_orig_su_t1215, magna_carta_clause_39__originalist_limitation_reading, suppression_requirement, 1215, 0.65).
narrative_ontology:measurement(mc39_orig_su_t1230, magna_carta_clause_39__originalist_limitation_reading, suppression_requirement, 1230, 0.55).
narrative_ontology:measurement(mc39_orig_su_t1245, magna_carta_clause_39__originalist_limitation_reading, suppression_requirement, 1245, 0.5).
narrative_ontology:measurement(mc39_orig_su_t1260, magna_carta_clause_39__originalist_limitation_reading, suppression_requirement, 1260, 0.6).
narrative_ontology:measurement(mc39_orig_su_t1275, magna_carta_clause_39__originalist_limitation_reading, suppression_requirement, 1275, 0.45).
narrative_ontology:measurement(mc39_orig_su_t1290, magna_carta_clause_39__originalist_limitation_reading, suppression_requirement, 1290, 0.48).
narrative_ontology:measurement(mc39_orig_su_t1300, magna_carta_clause_39__originalist_limitation_reading, suppression_requirement, 1300, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
