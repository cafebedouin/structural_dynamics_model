% ============================================================================
% CONSTRAINT STORY: creed_381_pneumatology__filioque_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_creed_381_pneumatology__filioque_reading, []).

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
 *   constraint_id: creed_381_pneumatology__filioque_reading
 *   human_readable: Filioque Doctrine and Papal Magisterial Authority
 *   domain: ecclesiastical/theological
 *
 * SUMMARY:
 *   The Second Ecumenical Council (381) affirmed that the Holy Spirit
 *   'proceeds from the Father' but left the relationship between Father and
 *   Son in pneumatology implicit. Western theology, particularly from
 *   Augustine onward, articulated the conviction that the Spirit proceeds
 *   from Father and Son (Filioque). By the 9th century, this formula was
 *   gradually incorporated into the Latin creed without ecumenical
 *   consultation. The constraint described here is the PAPAL MAGISTERIUM'S
 *   AUTHORITATIVE CLAIM that the Filioque is a legitimate DEVELOPMENT of 381
 *   — not an innovation — and that the papal/conciliar magisterium possesses
 *   binding authority to make this clarification. This reading asserts a
 *   specific distribution of ecclesiastical authority: Rome interprets 381;
 *   the East must assent. From the East's perspective, this is unilateral
 *   doctrinal imposition. From Rome's perspective, this is legitimate
 *   development grounded in apostolic tradition. The engine computes each
 *   seat's type from the structural data; the authored metrics describe an
 *   increasingly extractive, actively enforced arrangement.
 *
 * KEY AGENTS:
 *   - Papal magisterium (agenda-setter, institutional power): sets the doctrinal frame, declares Filioque binding, enforces through discipline and exclusion.
 *   - Latin Western Church (beneficiary, organized power): gains doctrinal clarity and institutional backing for Western trinitarian theology.
 *   - Eastern Orthodox Churches (payer + excluded, organized power, identity-locked exit): maintain monoprocession as core doctrine, forced to choose between assenting to Filioque (which contradicts their theology) or remaining outside communion.
 *   - Oriental Orthodox traditions (payer, moderate power, trapped exit): similarly positioned outside the Roman framework.
 *   - Ecumenical Councils (powerful, constrained exit): lose interpretive authority to the papal magisterium's unilateral development doctrine.
 *   - Eastern theological tradition (payer, identity-locked exit): pneumatology and Christology bound to monoprocession; assent to Filioque requires theological identity reconstruction.
 *   - Modern ecumenical movement (observer, constrained exit): seeks bilateral recognition but cannot negotiate within the papal magisterium's framework without the magisterium's authorization.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(creed_381_pneumatology__filioque_reading, 0.71).
domain_priors:suppression_score(creed_381_pneumatology__filioque_reading, 0.68).
domain_priors:theater_ratio(creed_381_pneumatology__filioque_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(creed_381_pneumatology__filioque_reading, extractiveness, 0.71).
narrative_ontology:constraint_metric(creed_381_pneumatology__filioque_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(creed_381_pneumatology__filioque_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(creed_381_pneumatology__filioque_reading, accessibility_collapse, 0.64).
narrative_ontology:constraint_metric(creed_381_pneumatology__filioque_reading, resistance, 0.73).

% --- Constraint claim ---
narrative_ontology:constraint_claim(creed_381_pneumatology__filioque_reading, tangled_rope).
narrative_ontology:human_readable(creed_381_pneumatology__filioque_reading, "Filioque Doctrine and Papal Magisterial Authority").
narrative_ontology:topic_domain(creed_381_pneumatology__filioque_reading, "ecclesiastical/theological").

domain_priors:requires_active_enforcement(creed_381_pneumatology__filioque_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(creed_381_pneumatology__filioque_reading, '270f0112-b6f7-4891-893f-ec2269f7cf4c').
narrative_ontology:cs_kernel_codification('270f0112-b6f7-4891-893f-ec2269f7cf4c', formalized).
narrative_ontology:cs_authority_grounding('270f0112-b6f7-4891-893f-ec2269f7cf4c', extraction).
narrative_ontology:cs_interpretation_layer_present('270f0112-b6f7-4891-893f-ec2269f7cf4c').
narrative_ontology:cs_reading_relation('270f0112-b6f7-4891-893f-ec2269f7cf4c', creed_381_pneumatology__monoprocession_reading, forecloses).
narrative_ontology:cs_reading_relation('270f0112-b6f7-4891-893f-ec2269f7cf4c', creed_381_pneumatology__ecumenical_reunion_reading, influences).
narrative_ontology:cs_axiom('270f0112-b6f7-4891-893f-ec2269f7cf4c', foundational, papal_magisterium_unilateral_clarification_authority).
narrative_ontology:cs_axiom_status(papal_magisterium_unilateral_clarification_authority, holdable).
narrative_ontology:cs_axiom_grounding('270f0112-b6f7-4891-893f-ec2269f7cf4c', papal_magisterium_unilateral_clarification_authority, deontological).
narrative_ontology:cs_axiom('270f0112-b6f7-4891-893f-ec2269f7cf4c', foundational, filioque_implicit_in_creed_381).
narrative_ontology:cs_axiom_status(filioque_implicit_in_creed_381, holdable).
narrative_ontology:cs_axiom_grounding('270f0112-b6f7-4891-893f-ec2269f7cf4c', filioque_implicit_in_creed_381, empirically_contingent).
narrative_ontology:cs_reference_frame('270f0112-b6f7-4891-893f-ec2269f7cf4c', papal_magisterial_supremacy).
narrative_ontology:cs_drift_state('270f0112-b6f7-4891-893f-ec2269f7cf4c', post_vatican_ii_ecumenical_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('270f0112-b6f7-4891-893f-ec2269f7cf4c', '').
narrative_ontology:cs_kernel_id(creed_381_pneumatology__filioque_reading, creed_381_pneumatology).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(creed_381_pneumatology__filioque_reading, papal_see).
narrative_ontology:constraint_beneficiary(creed_381_pneumatology__filioque_reading, latin_western_church).
narrative_ontology:constraint_victim(creed_381_pneumatology__filioque_reading, eastern_orthodox_churches).
narrative_ontology:constraint_victim(creed_381_pneumatology__filioque_reading, oriental_orthodox_traditions).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(creed_381_pneumatology__filioque_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(creed_381_pneumatology__filioque_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(creed_381_pneumatology__filioque_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(creed_381_pneumatology__filioque_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(creed_381_pneumatology__filioque_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises from 0.12 (near the Council of 381) to 0.71 by the modern era because the constraint's enforcement becomes more systematic and its reach more global. At 381, the doctrine is implicit; no extraction yet. By the 9th-11th centuries (Photios, East-West schism) extractiveness jumps as the papal magisterium asserts unilateral authority and the East resists, paying the cost of separation. By Florence (1438) and Trent (1546), extractiveness stabilizes at high levels: the Western Church is unified around Filioque, Eastern churches are disciplined into exclusion, and the magisterial authority to bind consciences on doctrine is institutionally entrenched. Theater ratio remains moderate (0.42 at interval end) because the substantive doctrinal defense of Filioque is real — scholastic theology, patristic citation, and liturgical integration are not pure performance. But the constraint's operative persistence increasingly depends on institutional discipline (excommunication, exclusion from ecumenical tables) rather than on the intrinsic persuasiveness of the Filioque argument itself. Suppression is high (0.68) because Eastern churches maintain resistance throughout the interval, and the papal magisterium must actively enforce the doctrine through excommunication, refusal of communion, and control of conciliar participation. The accessibility_collapse is high (0.64) because, once the papal magisterium declares Filioque binding, exit for Western churches is identity-locked: to reject Filioque is to exit Roman communion and to abandon Western theological tradition. The constraint PERSISTS because institutional discipline (suppression) holds it in place, not because alternatives have genuinely disappeared — hence theater_ratio remains at 0.42 (substantial performative maintenance).
 *
 * PERSPECTIVAL GAP:
 *   From the papal magisterium's seat: this is a Rope (genuine coordination of Western doctrine, legitimate development, necessary for unity). From the Eastern Orthodox seat: this is a Snare (unilateral imposition, masked as development, sustained by excommunication and exclusion). From the modern ecumenical observer's seat: this is a Piton (the original coordination function — settling pneumatology — is atrophied and replaced by institutional maintenance; the magisterium could authorize bilateral recognition but does not, so the constraint persists mostly by inertia and enforcement). The engine will compute these divergent classifications from the structural data: the papal magisterium holds power and agenda-setting authority (d near 0.0, low extraction from its perspective), while Eastern churches are organized, trapped, and identity-locked (d near 1.0, high extraction from their perspective). The asymmetry is the point: a Tangled Rope exhibits exactly this per-seat divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   The papal magisterium is the agenda-setter and beneficiary: it gains doctrinal authority, institutional dominion, and the capacity to unify (or exclude) under its framework. Its d approaches 0.0 (full beneficiary, low effective extraction). The Latin Western Church is a secondary beneficiary: it gains theological clarity and institutional backing. Its d is low (~0.15–0.25: it participates in the benefit, not the extraction). Eastern Orthodox and Oriental Orthodox churches are the targets: they lose theological autonomy, are excluded from ecumenical tables, face identity-based pressure to assent to a doctrine that contradicts their tradition, and are disciplined for resistance. Their d approaches 1.0 (full targets, high extraction). The Eastern theological tradition, bound to monoprocession through identity-fusion, sits at high d (~0.85): it is precisely what the constraint targets (identity-locked exit amplifies the extraction). The ecumenical councils and modern ecumenical movement are observers with constrained exits: they cannot negotiate outside the papal magisterial frame. Their d is moderate-high (~0.55–0.70) because they bear the structural cost of the constraint (inability to broker reunion on equal terms) without collecting rents.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem was genuine: how to ground Western Filioque intuition in 381's authority without breaking communion? The papal magisterium's answer was: declare Filioque a development of 381's implicit doctrine and bind all churches to this reading. By the modern era (esp. post-Vatican II), the founding problem is contested: was it ever a real doctrinal problem, or was it a Western theological intuition that became institutionally enforced? The constraint persists at high extractiveness (0.71) not because the doctrinal development is intrinsically persuasive (many theologians now concede Filioque is a Western addition) but because papal magisterial authority is institutionalized and the alternative (bilateral recognition of monoprocession and Filioque as complementary readings) would require the magisterium to relinquish the unilateral interpretation authority itself. Theater_ratio at 0.42 (moderate) reflects this: substantial theological defense persists, but institutional enforcement is increasingly visible. If mandatrophy_resolved were true, the constraint would have vanished or transformed by now; instead, it persists largely through institutional inertia and the magisterium's interest in maintaining the authority structure (even as ecumenical sensitivity increases). The constraint is a textbook Tangled Rope — genuine coordination function (Western doctrinal unity) + asymmetric extraction (Eastern churches excluded and disciplined) + active enforcement (magisterial discipline, refusal of communion, control of conciliar tables).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    doctrinal_development_vs_innovation,
    'Is Filioque truly an implicit development of 381''s monoprocession formula, or is it an innovation — a Western theological intuition that was gradually inserted into the creed and then retrospectively legitimated as development through papal authority?',
    'Patristic exegesis, historical analysis of the creed''s gradual textual transformation in the West (8th–11th centuries), and comparative pneumatology in Eastern and Western Fathers (Augustine, Gregory Nazianzus, Photios, Maximus). If Fathers intending monoprocession are cited with equal weight by both sides, the resolution remains contested; if one side''s exegetical reading is shown to misrepresent the sources, the claim of development can be challenged.',
    'If Filioque is development (implicit in 381), the papal magisterium''s unilateral clarification is legitimate, and the constraint is a Tangled Rope with justified coordination + extraction. If Filioque is innovation (not implicit in 381), the magisterium''s claim rests on the authority to CHANGE doctrine unilaterally, making the constraint a Snare (pure imposition). The classification pivots on this factual/conceptual ambiguity.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(doctrinal_development_vs_innovation, empirical, 'The metaphysical status of Filioque as development versus innovation.').

omega_variable(
    authority_to_bind_ecumenical_doctrine,
    'Does the papal magisterium possess the structural authority to bind all churches to a particular reading of ecumenical councils, or does such authority require ecumenical conciliar consensus (including Eastern participation)?',
    'Ecclesiology and canon law: does magisterial authority derive from apostolic succession (papal) or from conciliar communion (ecumenical)? If magisterial authority is supreme, papal unilateralism is legitimate; if authority requires conciliar consensus, the papal assertion is usurpation. Historical test: have Eastern churches ever assented to Filioque, or has it remained imposed against their judgment?',
    'If the magisterium''s authority is supreme, the constraint is a Tangled Rope operating as intended (legitimate authority + coordinated doctrine + enforced assent). If authority requires ecumenical consensus, the constraint is a Snare (unilateral imposition of a doctrine rejected by half of global Christendom). The classification hinges on which authority structure is true.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(authority_to_bind_ecumenical_doctrine, conceptual, 'Whether the papal magisterium''s authority is unilateral or requires ecumenical conciliar consent.').

omega_variable(
    identity_lock_mechanism_in_eastern_churches,
    'Is the Eastern churches'' resistance to Filioque a matter of structural (institutional barriers to doctrinal change) or internalized (theological identity so fused with monoprocession that assent is felt as self-annihilation) suppression? Do Eastern theologians maintain monoprocession because the papal magisterium forbids Filioque, or because their theological tradition has made monoprocession non-negotiable?',
    'Post-suppression trajectory: if Eastern churches that negotiated Filioque (e.g., some Uniate traditions) subsequently maintained Western discipline, suppression was structural; if they reverted to monoprocession when freed from papal pressure, suppression was internalized. Or: counterfactual analysis of what doctrinal positions Eastern churches would hold if the papal magisterium had never enforced Filioque — would they have developed a complementary (or Filioque) reading, or is monoprocession integral to Eastern theology independent of magisterial pressure?',
    'If suppression is structural (external enforcement), then removing the papal constraint might permit Eastern churches to negotiate or even adopt Filioque; the high extraction is imposed from outside. If suppression is internalized (theological identity), then removing the constraint would not dissolve resistance; the extraction is amplified by the target''s own identity-commitments, and the effective extraction is higher than measured suppression suggests.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_mechanism_in_eastern_churches, empirical, 'Whether Eastern church resistance to Filioque is structural or internalized suppression.').

omega_variable(
    ecumenical_legitimacy_of_unilateral_clarification,
    'If the papal magisterium claims authority to clarify implicit doctrine unilaterally, is this authority legitimate in an ecumenical context, or does ecumenical legitimacy require that Eastern churches assent to the clarification as valid before it can be binding?',
    'Ecumenical dialogue and bilateral recognition: if Eastern Orthodox theologians come to affirm Filioque as a legitimate theological development (not necessarily their primary formulation, but not false), then the magisterium''s clarification has become ecumenically legitimate ex post facto. If Eastern churches remain opposed despite centuries of dialogue, the unilateral authority claim lacks ecumenical legitimacy and operates as disciplinary enforcement, not doctrinal persuasion.',
    'If ecumenical legitimacy requires Eastern assent, the constraint''s extractiveness would be reduced if the magisterium authorized bilateral recognition (Ecumenical_reunion_reading). The sibling reading (ecumenical_reunion_reading) would coexist or influence this one, reducing its extractive force. If the magisterium''s unilateral authority is legitimate independent of Eastern assent, the constraint persists at current extraction levels.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ecumenical_legitimacy_of_unilateral_clarification, preference, 'Whether unilateral magisterial clarification is legitimate without ecumenical consensus.').

omega_variable(
    reading_contest_over_creed_381,
    'Is the kernel ''creed_381_pneumatology'' such that only ONE of the three readings (filioque, monoprocession, ecumenical_reunion) can be true, or can multiple readings coexist as legitimate theological expressions within a single communion?',
    'Doctrinal metaphysics and ecclesiology: Does pneumatology admit of multiple legitimate formulations (e.g., the Spirit proceeds from Father alone AND from Father and Son as complementary insights), or does the pneumatological fact admit only one true description? Can Eastern and Western Christologies both be right about the Spirit''s procession, or are they metaphysically incompatible?',
    'If only one reading is metaphysically true, the constraint''s extractiveness is justified (the magisterium binds consciences to truth). If multiple readings are compatible, the constraint''s extractiveness is reduced by the ecumenical_reunion_reading (which permits bilateral recognition), and the filioque_reading becomes an instance of false certainty. The classification would shift from Tangled Rope toward Piton (extracting rents from enforced doctrinal uniformity rather than solving a real coordination problem).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_contest_over_creed_381, conceptual, 'The metaphysical and ecclesiological status of reading pluralism within a single communion.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(creed_381_pneumatology__filioque_reading, 381, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cree_tr_t381, creed_381_pneumatology__filioque_reading, theater_ratio, 381, 0.05).
narrative_ontology:measurement(cree_tr_t589, creed_381_pneumatology__filioque_reading, theater_ratio, 589, 0.12).
narrative_ontology:measurement(cree_tr_t867, creed_381_pneumatology__filioque_reading, theater_ratio, 867, 0.21).
narrative_ontology:measurement(cree_tr_t1054, creed_381_pneumatology__filioque_reading, theater_ratio, 1054, 0.31).
narrative_ontology:measurement(cree_tr_t1438, creed_381_pneumatology__filioque_reading, theater_ratio, 1438, 0.38).
narrative_ontology:measurement(cree_tr_t1965, creed_381_pneumatology__filioque_reading, theater_ratio, 1965, 0.42).
narrative_ontology:measurement(cree_tr_t2024, creed_381_pneumatology__filioque_reading, theater_ratio, 2024, 0.42).

% Extraction over time
narrative_ontology:measurement(cree_be_t381, creed_381_pneumatology__filioque_reading, base_extractiveness, 381, 0.12).
narrative_ontology:measurement(cree_be_t589, creed_381_pneumatology__filioque_reading, base_extractiveness, 589, 0.24).
narrative_ontology:measurement(cree_be_t867, creed_381_pneumatology__filioque_reading, base_extractiveness, 867, 0.39).
narrative_ontology:measurement(cree_be_t1054, creed_381_pneumatology__filioque_reading, base_extractiveness, 1054, 0.58).
narrative_ontology:measurement(cree_be_t1438, creed_381_pneumatology__filioque_reading, base_extractiveness, 1438, 0.67).
narrative_ontology:measurement(cree_be_t1965, creed_381_pneumatology__filioque_reading, base_extractiveness, 1965, 0.71).
narrative_ontology:measurement(cree_be_t2024, creed_381_pneumatology__filioque_reading, base_extractiveness, 2024, 0.71).

% Suppression requirement over time
narrative_ontology:measurement(cree_su_t381, creed_381_pneumatology__filioque_reading, suppression_requirement, 381, 0.08).
narrative_ontology:measurement(cree_su_t589, creed_381_pneumatology__filioque_reading, suppression_requirement, 589, 0.18).
narrative_ontology:measurement(cree_su_t867, creed_381_pneumatology__filioque_reading, suppression_requirement, 867, 0.35).
narrative_ontology:measurement(cree_su_t1054, creed_381_pneumatology__filioque_reading, suppression_requirement, 1054, 0.56).
narrative_ontology:measurement(cree_su_t1438, creed_381_pneumatology__filioque_reading, suppression_requirement, 1438, 0.62).
narrative_ontology:measurement(cree_su_t1965, creed_381_pneumatology__filioque_reading, suppression_requirement, 1965, 0.68).
narrative_ontology:measurement(cree_su_t2024, creed_381_pneumatology__filioque_reading, suppression_requirement, 2024, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(creed_381_pneumatology__filioque_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(creed_381_pneumatology__filioque_reading, 0.25).
narrative_ontology:affects_constraint(creed_381_pneumatology__filioque_reading, creed_381_pneumatology__monoprocession_reading).
narrative_ontology:affects_constraint(creed_381_pneumatology__filioque_reading, creed_381_pneumatology__ecumenical_reunion_reading).
narrative_ontology:affects_constraint(creed_381_pneumatology__filioque_reading, photios_schism_1054).
narrative_ontology:affects_constraint(creed_381_pneumatology__filioque_reading, vatican_ii_ecumenical_openness).
narrative_ontology:affects_constraint(creed_381_pneumatology__filioque_reading, papal_magisterial_supremacy).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the kernel 'creed_381_pneumatology'. The sibling readings are: (1) monoprocession_reading — the Spirit proceeds from Father alone; 381 is inviolable without ecumenical consent; unilateral amendment is breach; (2) ecumenical_reunion_reading — both Filioque and monoprocession are acceptable as regional theological expressions; bilateral recognition replaces unilateral imposition. The ε values differ significantly: filioque_reading is high-extraction (0.71) because it establishes unilateral papal authority to bind doctrine; monoprocession_reading is low-extraction (governance by ecumenical consensus, no unilateral override); ecumenical_reunion_reading is moderate-extraction (bilateral negotiation reduces asymmetry). The three readings have different beneficiaries (Rome, Ecumenical Councils, Bilateral Churches respectively), different victims (Eastern Churches, none, none), and different authority structures (papal unilateral, conciliar consensus, negotiated bilateral). They are NOT the same constraint observed from different angles — they are three fundamentally different distributions of ecclesiastical power grounded in the SAME disputed kernel (381's pneumatology).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
