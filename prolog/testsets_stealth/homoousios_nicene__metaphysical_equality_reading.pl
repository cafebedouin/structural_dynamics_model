% ============================================================================
% CONSTRAINT STORY: homoousios_nicene__metaphysical_equality_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_homoousios_nicene__metaphysical_equality_reading, []).

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
 *   constraint_id: homoousios_nicene__metaphysical_equality_reading
 *   human_readable: Nicene Consubstantiality Settlement — Metaphysical Equality Reading (325-381)
 *   domain: historical_theology/ecclesiastical_history/philosophy_of_religion
 *
 * SUMMARY:
 *   Between 325 and 381 the consubstantiality definition of the Father-Son
 *   relation was imposed, eroded, and re-imposed. The term homoousios
 *   functions as a metaphysical boundary — same divine essence, co-eternal,
 *   no subordination in being — administered by councils and enforced by
 *   imperial machinery that deposed, exiled, and dispossessed those who read
 *   the relation otherwise. This file authors THAT standing arrangement — the
 *   enforced settlement, not the abstract doctrine — as assessed by the
 *   metaphysical_equality_reading's own lights. The claim/metric gap is
 *   deliberate: the reading CLAIMS tangled_rope (a real coordination
 *   achievement fused with asymmetric extraction), while the metrics are
 *   authored independently from the arrangement's observable operation — high
 *   suppression, oscillating enforcement, substantial resistance. The engine
 *   measures any divergence; the claim is not tuned to predicted output.
 *
 * KEY AGENTS:
 *   - pro_nicene_episcopal_hierarchy: Primary agenda-setter and beneficiary (institutional/identity_locked) — administers the boundary, collects jurisdiction and offices
 *   - imperial_administration: Secondary beneficiary (institutional/mobile) — enforces for unity, switches patronage when calculations change
 *   - heterodox_christologians: Primary target (organized/trapped) — bear anathema, deposition, exile
 *   - dispossessed_dissenting_congregations: Secondary target (moderate/constrained) — lose buildings, clergy, and funds
 *   - contested_see_laity: Excluded voice (powerless/constrained) — absorbs the whiplash with no seat
 *   - gothic_arian_churches: Excluded carrier of the alternative (organized/mobile) — beyond enforcement reach
 *   - modern_patristic_scholarship: Analytical observer (analytical/analytical) — sees the full structure from the documents
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(homoousios_nicene__metaphysical_equality_reading, 0.74).
domain_priors:suppression_score(homoousios_nicene__metaphysical_equality_reading, 0.84).
domain_priors:theater_ratio(homoousios_nicene__metaphysical_equality_reading, 0.21).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(homoousios_nicene__metaphysical_equality_reading, extractiveness, 0.74).
narrative_ontology:constraint_metric(homoousios_nicene__metaphysical_equality_reading, suppression_requirement, 0.84).
narrative_ontology:constraint_metric(homoousios_nicene__metaphysical_equality_reading, theater_ratio, 0.21).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(homoousios_nicene__metaphysical_equality_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(homoousios_nicene__metaphysical_equality_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(homoousios_nicene__metaphysical_equality_reading, tangled_rope).
narrative_ontology:human_readable(homoousios_nicene__metaphysical_equality_reading, "Nicene Consubstantiality Settlement — Metaphysical Equality Reading (325-381)").
narrative_ontology:topic_domain(homoousios_nicene__metaphysical_equality_reading, "historical_theology/ecclesiastical_history/philosophy_of_religion").

domain_priors:requires_active_enforcement(homoousios_nicene__metaphysical_equality_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(homoousios_nicene__metaphysical_equality_reading, '3ca8fa0c-1107-49b0-86dd-03891b1d51ad').
narrative_ontology:cs_kernel_codification('3ca8fa0c-1107-49b0-86dd-03891b1d51ad', formalized).
narrative_ontology:cs_authority_grounding('3ca8fa0c-1107-49b0-86dd-03891b1d51ad', extraction).
narrative_ontology:cs_interpretation_layer_present('3ca8fa0c-1107-49b0-86dd-03891b1d51ad').
narrative_ontology:cs_reading_relation('3ca8fa0c-1107-49b0-86dd-03891b1d51ad', homoousios_nicene__subordinationist_reading, forecloses).
narrative_ontology:cs_reading_relation('3ca8fa0c-1107-49b0-86dd-03891b1d51ad', homoousios_nicene__honorific_similarity_reading, forecloses).
narrative_ontology:cs_axiom('3ca8fa0c-1107-49b0-86dd-03891b1d51ad', foundational, son_shares_fathers_divine_essence).
narrative_ontology:cs_axiom_status(son_shares_fathers_divine_essence, holdable).
narrative_ontology:cs_axiom_grounding('3ca8fa0c-1107-49b0-86dd-03891b1d51ad', son_shares_fathers_divine_essence, theological).
narrative_ontology:cs_axiom('3ca8fa0c-1107-49b0-86dd-03891b1d51ad', secondary, conciliar_definition_binding_on_faithful).
narrative_ontology:cs_axiom_status(conciliar_definition_binding_on_faithful, holdable).
narrative_ontology:cs_axiom_grounding('3ca8fa0c-1107-49b0-86dd-03891b1d51ad', conciliar_definition_binding_on_faithful, conventional).
narrative_ontology:cs_reference_frame('3ca8fa0c-1107-49b0-86dd-03891b1d51ad', consubstantial_apostolic_faith).
narrative_ontology:cs_drift_state('3ca8fa0c-1107-49b0-86dd-03891b1d51ad', contemporary_trinitarian_debate, gap(repudiation_pressure, minor, false)).
narrative_ontology:cs_created_at('3ca8fa0c-1107-49b0-86dd-03891b1d51ad', '').
narrative_ontology:cs_kernel_id(homoousios_nicene__metaphysical_equality_reading, homoousios_nicene).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(homoousios_nicene__metaphysical_equality_reading, pro_nicene_episcopal_hierarchy).
narrative_ontology:constraint_beneficiary(homoousios_nicene__metaphysical_equality_reading, imperial_administration).
narrative_ontology:constraint_victim(homoousios_nicene__metaphysical_equality_reading, heterodox_christologians).
narrative_ontology:constraint_victim(homoousios_nicene__metaphysical_equality_reading, dispossessed_dissenting_congregations).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Bishops of the great sees and their allies convene councils, draft and demand subscription to the consubstantiality formula, depose non-conforming colleagues, and administer the baptismal standard that defines membership. Their jurisdictional authority, the prestige of the see-system, and their personal offices are bound up with the settlement they administer; abandoning it would dissolve the basis on which they hold position.
narrative_ontology:constraint_stakeholder(homoousios_nicene__metaphysical_equality_reading, pro_nicene_episcopal_hierarchy, agenda_setter,
    institutional, generational, identity_locked, continental).
narrative_ontology:stakeholder_secondary_role(homoousios_nicene__metaphysical_equality_reading, pro_nicene_episcopal_hierarchy, beneficiary).

% Emperors convene and fund councils, ratify or veto their outcomes, and carry decisions into effect with exile decrees and troops. They collect administrative unity and religious legitimation from a church speaking with one voice on the defining question. Their relationship to the theological content is instrumental: when unity calculations changed, Constantius shifted patronage to the homoean party, and Theodosius shifted it back.
narrative_ontology:constraint_stakeholder(homoousios_nicene__metaphysical_equality_reading, imperial_administration, beneficiary,
    institutional, generational, mobile, continental).

% Bishops, presbyters, monks, and teachers who read the Son as deriving his being from the Father, or as merely like the Father, lose office, income, church buildings, and legal standing whenever the settlement is enforced. Exile decrees follow them across provinces. Remaining means subscribing formulas they believe false; leaving means losing community and livelihood together. At various moments they commanded imperial favor and ran the machinery against the other side, but within this arrangement they are the party the anathemas name.
narrative_ontology:constraint_stakeholder(homoousios_nicene__metaphysical_equality_reading, heterodox_christologians, payer,
    organized, biographical, trapped, continental).

% Urban congregations whose clergy are deposed find soldiers installing replacement bishops. They attend liturgies led by men they regard as unsound, or withdraw to house-churches and lose access to their own buildings, cemeteries, and charitable funds, which change hands by imperial letter.
narrative_ontology:constraint_stakeholder(homoousios_nicene__metaphysical_equality_reading, dispossessed_dissenting_congregations, payer,
    moderate, biographical, constrained, regional).

% Ordinary worshippers in cities like Alexandria and Constantinople live through abrupt changes of clergy, liturgy, and communal allegiance decided far above them. They riot, petition, and shelter favored teachers, but no council seat or drafting committee is open to them.
narrative_ontology:constraint_stakeholder(homoousios_nicene__metaphysical_equality_reading, contested_see_laity, excluded,
    powerless, biographical, constrained, local).

% Churches beyond the Danube receive the alternative reading through missionary bishops such as Ulfila and carry it entirely outside imperial enforcement. Their flourishing shows the boundary stops at the frontier; they later enter the empire as federate armies with their own clergy and liturgy intact.
narrative_ontology:constraint_stakeholder(homoousios_nicene__metaphysical_equality_reading, gothic_arian_churches, excluded,
    organized, generational, mobile, continental).

% Reconstructs the controversy from letters, conciliar acts, coins, and papyri; separates the metaphysical claim from its enforcement history; reads the surviving documents of every party, including the losers whose works survive mostly in quotation by opponents.
narrative_ontology:constraint_stakeholder(homoousios_nicene__metaphysical_equality_reading, modern_patristic_scholarship, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(homoousios_nicene__metaphysical_equality_reading, pro_nicene_episcopal_hierarchy).
narrative_ontology:fixing_cost_class(homoousios_nicene__metaphysical_equality_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single authoritative answer to the question of the Son's relation to the Father, enabling shared baptismal confession, mutually recognized ordinations, common liturgy, and a fixed membership boundary across a geographically dispersed network of congregations that had begun refusing communion to one another.
% TRANSFER_FUNCTION: Moves interpretive authority — the right to define doctrine — from dispersed local teachers and congregational traditions to the conciliar-episcopal center; moves ecclesiastical offices, buildings, and stipends from deposed dissenting clergy to conforming replacements; moves dissenters themselves from good standing to anathema and exile.
% ABSENT_VOICES: The eastern episcopate at Nicaea contained a large middle party that signed under imperial pressure with private reservations many later revoked; the laity of contested sees had no seat anywhere; the churches beyond the frontier carried the alternative reading wholly outside the room. Unanimity at the councils was manufactured as much as deliberated.
% DISAPPEARANCE_RATIONALE: Overnight removal would reopen the communion crisis: competing baptisms, unrecognized ordinations, congregations refusing each other's eucharists, and an empire stripped of the religious legitimation it had learned to draw from a unified church. The see-system, the liturgical calendar, and the entire subsequent credal architecture depend on the settlement.
% FOUNDING_PROBLEM: Presbyter Arius taught that the Son was made from nothing and that there was a time when he was not. Congregations sharing one scripture and one baptismal formula arrived at incompatible answers to who Christ is, and broke communion over it; the emperor needed a single answer to hold the church together as an instrument of imperial cohesion.
% FOUNDING_PROBLEM_CORROBORATION: The crisis itself is corroborated from outside the benefiting parties: Arius' own letters (to Eusebius of Nicomedia, and the Thalia fragments preserved in opponents' quotations) attest the dispute was live before any settlement existed; Constantine's correspondence shows it reached the palace independently of episcopal agendas; modern patristic scholarship documents pre-Nicene diversity in theologies of the Son. What remains disputed is whether the consubstantiality answer was compelled by the problem or installed by enforcement — the losing parties' testimony survives mostly through their opponents, which is itself signal.
narrative_ontology:disappearance_verdict(homoousios_nicene__metaphysical_equality_reading, world_rearranges).
narrative_ontology:founding_problem_status(homoousios_nicene__metaphysical_equality_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(homoousios_nicene__metaphysical_equality_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(homoousios_nicene__metaphysical_equality_reading, 'none', 1).
narrative_ontology:epsilon_provenance(homoousios_nicene__metaphysical_equality_reading, 0.74, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(homoousios_nicene__metaphysical_equality_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(homoousios_nicene__metaphysical_equality_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(homoousios_nicene__metaphysical_equality_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is high (0.74 at the 381 settlement) because enforcement transferred real assets — sees, buildings, stipends, legal standing — from dissenters to conformists, and the rate of transfer was decoupled from any service rendered. Suppression is higher still (0.84) because the arrangement's persistence depended on actively excluding alternative readings through exile decrees, church seizures, and legal disability, not on voluntary assent. Theater is low-moderate (0.21 at end-state): the doctrinal work was substantively real, but the 340s-350s show a theatrical peak (0.41) when creeds proliferated as court diplomacy and subscription became a career gate. CYCLICAL PATTERN: the series is deliberately non-monotonic — one full cycle (peak 325, trough 357, new peak 381) driven by imperial enforcement capacity flipping between factions. Each flip purged the other side's personnel and concentrated offices with conformists, so the oscillation itself amplified concentration of gains: intermittent enforcement functioned as an extraction mechanism, not noise. The base_properties scalars reflect the end-state (381 settlement, near enforcement peak). All three series share one time grid; suppression_requirement is tracked because the story's subject IS enforcement-capacity change (decay to 0.44 by 357, re-ratchet to 0.84 by 381, exceeding the original imposition).
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute very differently. From the episcopal seat the arrangement is the church's constitution — the coordination that ended communion-breaking ambiguity, experienced as rope-forward. From the heterodox seat the same structure is ruinous enforcement — deposition, exile, dispossession — experienced as extraction-forward with the coordination story as cover. From the imperial seat it is an instrument: a unity technology whose theological content is switchable, which is exactly why the same emperor-class enforced opposite readings within a generation. The laity seat experiences arbitrariness — allegiance changing by imperial letter. The engine computes this divergence from the structural data; the authored claim adjudicates none of it.
 *
 * DIRECTIONALITY LOGIC:
 *   The hierarchy sits near the beneficiary end: it declares the boundary, administers enforcement, and receives the transferred offices — identity_lock amplifies rather than dampens its beneficiary position, since its authority IS the settlement. The imperial administration benefits incidentally (unity, legitimation) with mobile exit — it can and did walk away to the rival reading — placing it near-symmetric with a subsidy tilt. Heterodox christologians are trapped targets near the full-target end: no exit preserves both livelihood and conviction. Dispossessed congregations sit high-target but slightly below the clergy (they lose assets and community, not office and legal standing). Gothic churches fall outside the arrangement's effective scope — near-zero exposure despite nominal opposition. The observer seat carries no directional exposure.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification guards against two symmetrical errors. Reading the arrangement as pure extraction erases the genuine coordination achieved: a shared grammatical answer that ended a real communion crisis and made mutual recognition of baptism and ordination possible across a continental network. Reading it as pure coordination erases the anathema machinery, the asset transfers, and the manufactured unanimity. Tangled rope holds both faces in one structure. On the genealogy interview: the founding problem (the Arian rupture) is historically closed, but the underlying definitional problem recurs in every generation that rereads the sources — hence status contested, not dead; and since the arrangement persists while the parties dispute whether its problem is solved, no mandatrophy resolution is declared. The mandate (guarding the definition) remains the arrangement's live function; there is no sunset and no atrophy.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'This story instantiates the metaphysical_equality_reading of kernel homoousios_nicene; how would the classification shift under the sibling readings?',
    'Author the sibling stories (subordinationist_reading, honorific_similarity_reading) with their own beneficiary/victim sets and compare computed per-seat types across the kernel family.',
    'Under the honorific_similarity_reading the boundary is looser and suppression plausibly lower, pulling the arrangement toward coordination-only territory; under the subordinationist_reading the victim set partially inverts — during homoean ascendance the pro-Nicene party bears the costs — so the same enforcement machinery computes as a different constraint with different directionalities.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Committer structure: this is one reading of a contested kernel; sibling readings are separate constraints, not hedges inside this one.').

omega_variable(
    discovered_vs_constructed_boundary,
    'Is the consubstantiality boundary a truth-tracking discovery about divine reality, or a constructed settlement whose persistence depends on enforcement?',
    'Comparative doctrinal history: whether the same boundary re-emerges in settings without enforcement pressure (independent reception across languages, cultures, and centuries) or tracks enforcement capacity instead.',
    'If constructed, the post-381 stability is enforcement-dependent and drift toward pure extraction becomes likelier as enforcement capacity fluctuates; if discovered, the coordination reading strengthens and the measured extraction overstates structural pathology.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(discovered_vs_constructed_boundary, conceptual, 'Whether the metaphysical boundary is natural law analog or constructed rule — the classic fideist/history-of-doctrine ambiguity.').

omega_variable(
    conformity_conviction_ratio,
    'How much of observed doctrinal conformity after each enforcement event reflects conviction versus coercion or opportunism?',
    'Behavior under enforcement reversal: when Constantius flipped the machinery to homoean control, large majorities of bishops subscribed within months; counting defections under each flip estimates the coerced share of conformity.',
    'If conformity is largely coerced, the measured suppression understates the internalized component carried by clergy formed under the settlement; if convictional, the settlement''s stability is more robust than the enforcement metrics alone imply.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(conformity_conviction_ratio, empirical, 'Structural versus internalized conformity among the episcopate under alternating enforcement.').

omega_variable(
    settlement_stability_source,
    'Why did the settlement hold after 381 after oscillating for fifty-six years — restored enforcement capacity (the Theodosian state) or grammatical stabilization (the Cappadocian synthesis making the terminology finally workable)?',
    'Counterfactual analysis of post-Theodosian dynasties: whether weaker emperors would have preserved the boundary; tracing where the ousia/hypostasis terminology stabilized first (monastic and theological networks versus courts).',
    'If enforcement-dependent, the arrangement''s persistence tracks state capacity and future drift follows fiscal-military decline; if grammar-stabilized, persistence is robust to enforcement decay and inertial-degradation risk falls.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(settlement_stability_source, empirical, 'Source of the settlement''s post-381 stability: coercion versus conceptual consolidation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(homoousios_nicene__metaphysical_equality_reading, 325, 381).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(homoousios_meta_eq_tr_t325, homoousios_nicene__metaphysical_equality_reading, theater_ratio, 325, 0.14).
narrative_ontology:measurement(homoousios_meta_eq_tr_t333, homoousios_nicene__metaphysical_equality_reading, theater_ratio, 333, 0.19).
narrative_ontology:measurement(homoousios_meta_eq_tr_t341, homoousios_nicene__metaphysical_equality_reading, theater_ratio, 341, 0.27).
narrative_ontology:measurement(homoousios_meta_eq_tr_t349, homoousios_nicene__metaphysical_equality_reading, theater_ratio, 349, 0.34).
narrative_ontology:measurement(homoousios_meta_eq_tr_t357, homoousios_nicene__metaphysical_equality_reading, theater_ratio, 357, 0.41).
narrative_ontology:measurement(homoousios_meta_eq_tr_t365, homoousios_nicene__metaphysical_equality_reading, theater_ratio, 365, 0.33).
narrative_ontology:measurement(homoousios_meta_eq_tr_t373, homoousios_nicene__metaphysical_equality_reading, theater_ratio, 373, 0.25).
narrative_ontology:measurement(homoousios_meta_eq_tr_t381, homoousios_nicene__metaphysical_equality_reading, theater_ratio, 381, 0.21).

% Extraction over time
narrative_ontology:measurement(homoousios_meta_eq_be_t325, homoousios_nicene__metaphysical_equality_reading, base_extractiveness, 325, 0.7).
narrative_ontology:measurement(homoousios_meta_eq_be_t333, homoousios_nicene__metaphysical_equality_reading, base_extractiveness, 333, 0.52).
narrative_ontology:measurement(homoousios_meta_eq_be_t341, homoousios_nicene__metaphysical_equality_reading, base_extractiveness, 341, 0.47).
narrative_ontology:measurement(homoousios_meta_eq_be_t349, homoousios_nicene__metaphysical_equality_reading, base_extractiveness, 349, 0.58).
narrative_ontology:measurement(homoousios_meta_eq_be_t357, homoousios_nicene__metaphysical_equality_reading, base_extractiveness, 357, 0.42).
narrative_ontology:measurement(homoousios_meta_eq_be_t365, homoousios_nicene__metaphysical_equality_reading, base_extractiveness, 365, 0.61).
narrative_ontology:measurement(homoousios_meta_eq_be_t373, homoousios_nicene__metaphysical_equality_reading, base_extractiveness, 373, 0.69).
narrative_ontology:measurement(homoousios_meta_eq_be_t381, homoousios_nicene__metaphysical_equality_reading, base_extractiveness, 381, 0.74).

% Suppression requirement over time
narrative_ontology:measurement(homoousios_meta_eq_su_t325, homoousios_nicene__metaphysical_equality_reading, suppression_requirement, 325, 0.76).
narrative_ontology:measurement(homoousios_meta_eq_su_t333, homoousios_nicene__metaphysical_equality_reading, suppression_requirement, 333, 0.58).
narrative_ontology:measurement(homoousios_meta_eq_su_t341, homoousios_nicene__metaphysical_equality_reading, suppression_requirement, 341, 0.52).
narrative_ontology:measurement(homoousios_meta_eq_su_t349, homoousios_nicene__metaphysical_equality_reading, suppression_requirement, 349, 0.55).
narrative_ontology:measurement(homoousios_meta_eq_su_t357, homoousios_nicene__metaphysical_equality_reading, suppression_requirement, 357, 0.44).
narrative_ontology:measurement(homoousios_meta_eq_su_t365, homoousios_nicene__metaphysical_equality_reading, suppression_requirement, 365, 0.67).
narrative_ontology:measurement(homoousios_meta_eq_su_t373, homoousios_nicene__metaphysical_equality_reading, suppression_requirement, 373, 0.73).
narrative_ontology:measurement(homoousios_meta_eq_su_t381, homoousios_nicene__metaphysical_equality_reading, suppression_requirement, 381, 0.84).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(homoousios_nicene__metaphysical_equality_reading, identity_coordination).
narrative_ontology:affects_constraint(homoousios_nicene__metaphysical_equality_reading, subordinationist_reading).
narrative_ontology:affects_constraint(homoousios_nicene__metaphysical_equality_reading, honorific_similarity_reading).

% DUAL FORMULATION NOTE:
% Constraint family decomposition per the epsilon-invariance principle: the colloquial label 'the Nicene settlement' conflates at least three structurally distinct claims — (1) the metaphysical content (strict consubstantiality; THIS file), (2) the claim that the term is compatible with derivational subordination (subordinationist_reading), and (3) the claim that the term signifies honorific likeness rather than identity (honorific_similarity_reading). Each carries its own epsilon, its own beneficiary/victim structure, and its own enforcement profile; measuring one with another's observable would violate epsilon-invariance. The upstream member (this reading, highest empirical entrenchment after 381) influences the downstream members' operating environment: its enforcement success is precisely what the sibling readings were formulated to resist or reinterpret. All family members link via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
