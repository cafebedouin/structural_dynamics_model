% ============================================================================
% CONSTRAINT STORY: homoousios_nicene__honorific_similarity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_homoousios_nicene__honorific_similarity_reading, []).

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
    narrative_ontology:measurement_basis/2,
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
 *   constraint_id: homoousios_nicene__honorific_similarity_reading
 *   human_readable: Nicene Homoousios — Honorific Similarity Reading (Blur Boundary)
 *   domain: historical_theology/ecclesiastical_history/philosophy_of_religion
 *
 * SUMMARY:
 *   A single Greek word — homoousios, of the same substance — imposed at
 *   Nicaea in 325 became the most fought-over term in Christian history. This
 *   story instantiates ONE reading of that contested kernel: the honorific
 *   similarity reading, under which the term signifies likeness and honorific
 *   unity — the Son is genuinely divine and worthy of the Father's worship —
 *   without strict metaphysical identity of essence. As an operative
 *   arrangement (c. 325-381), the reading drew a doctrinal boundary relaxed
 *   enough for the homoiousian moderates to subscribe, enforced broadly
 *   enough to keep heresy charges hanging over hard subordinationists, and
 *   open enough to shift interpretive discretion toward local bishops and
 *   pastoral judgment. Its coordination function was real — one signable word
 *   holding an empire's communion together for two generations — and its
 *   extraction was real and asymmetric: the strict Nicene party paid in
 *   definitional force and the hard subordinationists paid in condemnability,
 *   both through the same structure, sustained by imperial enforcement and
 *   conciliar pressure. Per the epsilon-invariance discipline, the sibling
 *   readings (metaphysical equality; subordination) are separate constraint
 *   stories with their own epsilon, victims, and beneficiaries; this file
 *   authors only this reading, and its epsilon referent is the standing blur
 *   arrangement as its holders understood and enforced it, 325-381. Claim and
 *   metrics are authored independently: the claimed_type records what the
 *   reading's structure is, the metrics record its descriptive operation, and
 *   the engine computes per-seat classifications from the structural data —
 *   where they diverge, that divergence is the measurement. KEY AGENTS (by
 *   structural relationship): - imperial_authority: Agenda setter
 *   (institutional/arbitrage) — enforces whichever formula promises unity;
 *   bears no doctrinal cost - semi_arian_moderates: Primary beneficiary
 *   (organized/mobile) — the homoiousian bloc the blur exists to include -
 *   apophatic_traditions: Secondary beneficiary (moderate/identity_locked) —
 *   reverent silence protected by the blur's restraint - local_bishops:
 *   Beneficiary with cost-bearing second face (moderate/constrained) — gain
 *   discretion, bear enforcement risk - strict_nicene_enforcers: Primary
 *   target (organized/identity_locked) — pay in definitional force; exit
 *   fused with confession - hard_subordinationists: Primary target
 *   (organized/mobile) — pay in heresy exposure; survive by formula-shifting
 *   - lay_congregations: Excluded voice (powerless/trapped) — receive every
 *   formula from the altar, seat at no council - doctrinal_historians:
 *   Analytical observer (analytical/analytical) — reconstruct the structure
 *   from acta and polemic
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(homoousios_nicene__honorific_similarity_reading, 0.44).
domain_priors:suppression_score(homoousios_nicene__honorific_similarity_reading, 0.5).
domain_priors:theater_ratio(homoousios_nicene__honorific_similarity_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(homoousios_nicene__honorific_similarity_reading, extractiveness, 0.44).
narrative_ontology:constraint_metric(homoousios_nicene__honorific_similarity_reading, suppression_requirement, 0.5).
narrative_ontology:constraint_metric(homoousios_nicene__honorific_similarity_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(homoousios_nicene__honorific_similarity_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(homoousios_nicene__honorific_similarity_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(homoousios_nicene__honorific_similarity_reading, tangled_rope).
narrative_ontology:human_readable(homoousios_nicene__honorific_similarity_reading, "Nicene Homoousios — Honorific Similarity Reading (Blur Boundary)").
narrative_ontology:topic_domain(homoousios_nicene__honorific_similarity_reading, "historical_theology/ecclesiastical_history/philosophy_of_religion").

domain_priors:requires_active_enforcement(homoousios_nicene__honorific_similarity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(homoousios_nicene__honorific_similarity_reading, '55735111-cc39-4906-90dc-bf22d8268e34').
narrative_ontology:cs_kernel_codification('55735111-cc39-4906-90dc-bf22d8268e34', fixed_text).
narrative_ontology:cs_authority_grounding('55735111-cc39-4906-90dc-bf22d8268e34', practice).
narrative_ontology:cs_interpretation_layer_present('55735111-cc39-4906-90dc-bf22d8268e34').
narrative_ontology:cs_reading_relation('55735111-cc39-4906-90dc-bf22d8268e34', homoousios_nicene__metaphysical_equality_reading, forecloses).
narrative_ontology:cs_reading_relation('55735111-cc39-4906-90dc-bf22d8268e34', homoousios_nicene__subordinationist_reading, coexists_with).
narrative_ontology:cs_axiom('55735111-cc39-4906-90dc-bf22d8268e34', foundational, homoousios_signifies_likeness_not_identity).
narrative_ontology:cs_axiom_status(homoousios_signifies_likeness_not_identity, holdable).
narrative_ontology:cs_axiom_grounding('55735111-cc39-4906-90dc-bf22d8268e34', homoousios_signifies_likeness_not_identity, empirically_contingent).
narrative_ontology:cs_axiom('55735111-cc39-4906-90dc-bf22d8268e34', foundational, confessional_unity_without_metaphysical_definition).
narrative_ontology:cs_axiom_status(confessional_unity_without_metaphysical_definition, holdable).
narrative_ontology:cs_axiom_grounding('55735111-cc39-4906-90dc-bf22d8268e34', confessional_unity_without_metaphysical_definition, instrumental).
narrative_ontology:cs_reference_frame('55735111-cc39-4906-90dc-bf22d8268e34', honorific_unity_confession).
narrative_ontology:cs_drift_state('55735111-cc39-4906-90dc-bf22d8268e34', post_constantinopolitan_settlement, gap(axiom_overriding, severe, true)).
narrative_ontology:cs_created_at('55735111-cc39-4906-90dc-bf22d8268e34', '').
narrative_ontology:cs_kernel_id(homoousios_nicene__honorific_similarity_reading, homoousios_nicene).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(homoousios_nicene__honorific_similarity_reading, semi_arian_moderates).
narrative_ontology:constraint_beneficiary(homoousios_nicene__honorific_similarity_reading, apophatic_traditions).
narrative_ontology:constraint_beneficiary(homoousios_nicene__honorific_similarity_reading, local_bishops).
narrative_ontology:constraint_victim(homoousios_nicene__honorific_similarity_reading, strict_nicene_enforcers).
narrative_ontology:constraint_victim(homoousios_nicene__honorific_similarity_reading, hard_subordinationists).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(homoousios_nicene__honorific_similarity_reading, local_bishops).
narrative_ontology:constraint_vindicates(homoousios_nicene__honorific_similarity_reading, economical_confession_doctrine).
narrative_ontology:constraint_vindicates(homoousios_nicene__honorific_similarity_reading, apophatic_restraint_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The emperor and his court, who enforced whichever formula promised an end to disruptive dispute and switched between formulae as politics demanded. The blurred reading's broad tent served the unity project: one signable word for an episcopate that could not agree on what it meant. The court bore no doctrinal cost itself and adjudicated enforcement, not theology.
narrative_ontology:constraint_stakeholder(homoousios_nicene__honorific_similarity_reading, imperial_authority, agenda_setter,
    institutional, biographical, arbitrage, continental).

% Bishops of the homoiousian party, who held that the Son is like the Father in substance. The blurred reading of the creed's term let them subscribe the common confession without abandoning their theology of real-but-distinct substance. They moved between formulae across councils as imperial favor shifted, and their inclusion in communion was the arrangement's principal product. Their alternative — refusing subscription — cost them sees and exile.
narrative_ontology:constraint_stakeholder(homoousios_nicene__honorific_similarity_reading, semi_arian_moderates, beneficiary,
    organized, biographical, mobile, continental).

% Theologians and monastic teachers committed to reverent silence about the divine essence. The blurred reading let them affirm the creed's unity language without committing to a metaphysical definition they held to be beyond speech. Taking the strict definitional path would betray their method; taking the subordinationist path would betray the worship they owed the Son. They are held by their own epistemology as much as by enforcement.
narrative_ontology:constraint_stakeholder(homoousios_nicene__honorific_similarity_reading, apophatic_traditions, beneficiary,
    moderate, generational, identity_locked, continental).

% Provincial bishops who gained interpretive discretion under the blurred reading: what the creed's term required of their teaching was settled pastorally, in use, rather than by a metaphysical definition they could not adjudicate. The same discretion cut both ways — when imperial policy shifted formulae, a signature on the wrong text could cost a bishop his see, and several paid exactly that.
narrative_ontology:constraint_stakeholder(homoousios_nicene__honorific_similarity_reading, local_bishops, beneficiary,
    moderate, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(homoousios_nicene__honorific_similarity_reading, local_bishops, payer).

% The party around Athanasius and, later, the Cappadocian theologians, who held that the creed's term secures the Son's full identity of essence with the Father. The blurred reading drained the term of the definitional force they had been exiled for defending and branded their precision as rigidity. They could not accept the blur without unmaking the confession their identity was built on; their refusal was continuous across the interval.
narrative_ontology:constraint_stakeholder(homoousios_nicene__honorific_similarity_reading, strict_nicene_enforcers, payer,
    organized, generational, identity_locked, continental).

% Arian and anomoian teachers, who held the Son's being to be derived from and lesser than the Father's. Even under the blurred reading, the honorific unity language kept the charge of heresy available against them, and councils under the blur's umbrella condemned them. They survived by shifting between formulae and court favor, but no version of the unity language was safe for them to sign.
narrative_ontology:constraint_stakeholder(homoousios_nicene__honorific_similarity_reading, hard_subordinationists, payer,
    organized, biographical, mobile, continental).

% The baptized faithful, who received each successive formula from the altar with no seat in the councils that drafted it. Their worship, baptism, and communal identity were the stakes over which the wings fought. Leaving the church's doctrinal regime was not a live option; their voice enters the record only as the audience the formulae were performed before.
narrative_ontology:constraint_stakeholder(homoousios_nicene__honorific_similarity_reading, lay_congregations, excluded,
    powerless, generational, trapped, continental).

% Later historians and theologians — Socrates Scholasticus and Sozomen first among them — who reconstruct the controversy from council acta, imperial letters, and polemic. They hold no seat inside the dispute: they see the term's ambiguity, the enforcement record, and each party's costs from outside the frame any faction could admit.
narrative_ontology:constraint_stakeholder(homoousios_nicene__honorific_similarity_reading, doctrinal_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(homoousios_nicene__honorific_similarity_reading, diffuse).
narrative_ontology:fixing_cost_class(homoousios_nicene__honorific_similarity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single creedal term broad enough for the empire's divided episcopate to subscribe jointly — coordinating communion, ordination recognition, and conciliar membership across factions that could not agree on what the term metaphysically commits them to. The blur is the coordination: one word, multiple understood senses, one enforceable subscription.
% TRANSFER_FUNCTION: Moves doctrinal legitimacy and confessional standing from the metaphysical precision of both wings — strict-identity readers pay in definitional force, subordinationist readers pay in heresy exposure — to the moderate center, and moves interpretive discretion toward local bishops and the imperial unity project.
% ABSENT_VOICES: The lay congregations who received each formula from the altar with no seat at the drafting councils; the hard subordinationists at the councils that condemned them; the exiled strict readers (Athanasius spent the blur's peak decades in exile) whose precision was ruled out of order; and the next generation of definers — the Cappadocians were children during the blur's enforcement peak.
% DISAPPEARANCE_RATIONALE: If the blur vanished overnight — if every bishop had to state what the creed's term metaphysically commits them to — the church would have split immediately along the substance line: the homoiousian bloc, the largest single party through the 350s, could not sign strict identity, and the strict party could not sign open subordination. The conciliar system, imperial church policy, and episcopal careers were arranged around the signable middle; removing it forces an immediate binary the empire's church could not survive as one communion.
% FOUNDING_PROBLEM: Nicaea (325) imposed a creed whose key term was metaphysically ambiguous and politically explosive: homoousios had no agreed definition, the largest episcopal blocs could not honestly sign it under a strict reading, and the empire needed a formula that would hold. The honorific similarity reading was built to make the term signable — to solve the enforcement crisis Nicaea itself created.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the beneficiary set: the strict Nicene party (Athanasius, De Decretis) attests the signability crisis was real while insisting its solution was definition, not blur; Hilary of Poitiers, exiled and outside the moderate coalition, documents the coercion behind the formula-swapping; the church historians Socrates Scholasticus and Sozomen, writing from no faction's seat, record the sequence of imposed formulae and their collapse. The strict party disputed that the problem was ever legitimate (calling the blur evasion rather than solution), which is why the status is authored contested rather than dead: the parties dispute the founding problem's reality and resolution, not merely its current urgency.
narrative_ontology:disappearance_verdict(homoousios_nicene__honorific_similarity_reading, world_rearranges).
narrative_ontology:founding_problem_status(homoousios_nicene__honorific_similarity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(homoousios_nicene__honorific_similarity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(homoousios_nicene__honorific_similarity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(homoousios_nicene__honorific_similarity_reading, 0.44, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(homoousios_nicene__honorific_similarity_reading_tests).
:- end_tests(homoousios_nicene__honorific_similarity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is moderate (0.44 at interval end, peaking 0.66 mid-interval) because the blur's costs fall on both wings through the same structure — the strict party loses the term's definitional force, the subordinationists remain condemnable under honorific unity — while the coordination benefit (one signable word) is broad and genuine. Suppression (0.50 at end, 0.72 at the 355 peak) reflects the enforcement machinery: imperial exile, forced subscription (Hosius under threat, Liberius exiled), and conciliar pressure; it is authored as a raw structural property and is deliberately NOT scaled — the engine scales only extractiveness, by directionality and scope. Theater_ratio (0.38 at end, peaking 0.60 after the 359-360 homoian settlement) tracks the performative quality of common subscription: at peak, one word was confessed in public while meaning privately different things in different sees. Accessibility_collapse is LOW (0.35) — the blur's era was one of formula proliferation, not alternative collapse: homoiousios, homoion, and anomoian formulae stayed live throughout, which is precisely why continuous enforcement was needed. Resistance is HIGH (0.6): both wings fought the blur for the whole interval — Athanasian polemic against dilution, Arian rejection of any unity language strong enough to condemn them. The measurement series run on one shared time grid (all three metrics at all seven points) so no metric's end-state value is back-projected onto earlier times. Receipt surface: gain_flow is authored 'diffuse' as an affirmative claim after checking every named seat — the imperial seat consumed political peace but enforced any signable formula indifferently (its gain is not specific to this reading); the moderates gained standing but do not collect the wings' costs; the product, a common confession, is consumed collectively by the tent, so no single seat captures the extraction. fixing_cost is authored 'prohibitive': every cheap fix (imperial formula-swaps) collapsed within a decade, and the durable fix required the ousia/hypostasis conceptual machinery that took a generation and two councils to build.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute differently and the structural data is built to make them do so. From the imperial seat the blur is an instrument: a formula broad enough to end disruptive dispute, switchable when politics demands. From the moderate beneficiary seats it is refuge: the only confession their theology can honestly sign. From the strict Nicene seat the same word is betrayal — the creed they were exiled for drained of the metaphysical claim that made it worth the exile. From the subordinationist seat it is a trap: unity language just strong enough to keep the heresy charge permanently available. Note also why coalition power never materialized among the two victim seats: they were each other's enemies. The strict party's refusal of the blur protected the very unity language that condemned the subordinationists, and the subordinationists' rejection of any strong unity language vindicated the strict party's suspicion of dilution — the blur's extraction was structurally protected by the wings' mutual hostility, which is why two organized, aggrieved blocs never combined against it.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries derive low directionality: semi_arian_moderates (the blur's inclusion product), apophatic_traditions (whose restraint the blur protects), and local_bishops (who gain interpretive discretion). Victims derive high directionality: strict_nicene_enforcers are identity_locked — their exit is fused with the strict confession, so they sit near the full-target end — and hard_subordinationists, though mobile through formula-shifting, are damped only slightly by that mobility because the standing heresy exposure keeps them near it. Local_bishops are genuinely dual-positioned: the derivation reads their beneficiary declaration and tilts them low, understating their enforcement-risk cost-bearing; that dual position is recorded on the stakeholder surface (beneficiary with payer secondary role) rather than forced through a directionality override, because a single power_atom override at moderate would also capture apophatic_traditions, whose beneficiary tilt is genuine and should not be disturbed. No overrides are authored; the derivation chain produces the correct qualitative structure from the declared beneficiaries, victims, and exit options.
 *
 * MANDATROPHY ANALYSIS:
 *   The hybrid coordination-extraction classification is what prevents two symmetric mislabels. Read as pure coordination, the blur's real extraction disappears: both wings demonstrably paid through the structure they were asked to sign. Read as pure extraction, the coordination function disappears: without the signable middle, the empire's communion demonstrably splits — the conciliar record shows every attempt to force a single wing's formula collapsed within a decade. The blur is both at once, held together by enforcement. On mandatrophy specifically: this was not a mandate outliving its function. The arrangement was displaced at the Constantinopolitan settlement (381), when the founding problem was dissolved by definition rather than served by blur — function and arrangement ended together. Its theater_ratio rose mid-interval (peak 0.60) as subscription became performative, but the ending was replacement, not atrophy: the R5 mismatch surface (contested founding-problem status crossed with world_rearranges disappearance) routes to investigation of the transitional class, not a zombie verdict, and the temporal series corroborates displacement rather than persistence-after-death.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_underdetermination,
    'Is the honorific similarity reading a faithful instantiation of the homoousios kernel or a revisionist projection onto an ambiguous term — and how does this story''s classification shift under the sibling readings?',
    'Philological analysis of ousia-compound usage in fourth-century conciliar acta (Eusebius''s subscription letter to his diocese, the homoiousian council letters) tested against each reading''s semantic claim, plus explicit classification of the sibling readings as separate constraint stories linked through network.affects_constraints.',
    'Under the metaphysical-equality sibling the victim set collapses to subordinationists alone and extraction concentrates on them; under the subordinationist sibling the strict Nicenes become the sole victims. This reading''s dual-victim structure is what makes the hybrid coordination-plus-extraction classification available; the siblings compute different types from their own structural data.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_underdetermination, conceptual, 'Whether this reading is the kernel''s faithful sense; sibling readings restructure the victim and beneficiary sets entirely.').

omega_variable(
    suppression_mechanism_ambiguity,
    'How much of the blur''s stability was structural coercion (imperial exile, forced subscription, conciliar pressure) versus internalized conviction (bishops sincerely holding the blur as the creed''s true sense)?',
    'Post-coercion trajectory analysis: where enforcement relaxed (after 361 under Julian''s toleration, after 378 under Gratian), did subscription to blur-family formulae persist without pressure (internalized) or collapse (structural)?',
    'If largely internalized, the arrangement''s effective suppressive force outlasted the imperial machinery and the Constantinopolitan settlement explains less of the blur''s end than the enforcement record suggests; if largely structural, the 381 settlement alone accounts for the displacement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural versus internalized share of the blur''s suppressive force.').

omega_variable(
    moderate_coalition_durability,
    'Were the semi-arian moderates genuine net beneficiaries of the blur, or transitional victims — included early, then consumed when the homoian formulae (359-360) squeezed the middle from one side and the strict settlement (381) from the other?',
    'Track the homoiousian bloc''s confessional standing across the interval: council invitations, imperial favor, and post-381 absorption. Did their inclusion gain persist, or did the middle bloc evaporate between the wings?',
    'If the moderates were transitional victims, the beneficiary declaration weakens, the moderate seat''s derived directionality rises toward the target end, and the coordination function looks thinner — pushing classification toward pure extraction; if durable beneficiaries, the hybrid reading stands.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(moderate_coalition_durability, empirical, 'Whether the blur''s primary beneficiaries kept their gains across the interval or were themselves consumed.').

omega_variable(
    unity_performance_vs_convergence,
    'Was the shared confession under the blur a performance (one word, privately different meanings — theatrical unity) or a genuine convergence of understanding (the blur actually collapsed the dispute in most sees)?',
    'Compare local pastoral records, sermons, and catechetical texts against conciliar polemic: did parish-level teaching converge on a working sense of the term, or did private-meaning divergence persist beneath public uniformity?',
    'If performance, theater_ratio is understated at its mid-interval peak and the coordination function is thinner than claimed; if convergence, the blur achieved real doctrinal peace and much of the measured extraction is nearer the price of coordination itself.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(unity_performance_vs_convergence, empirical, 'Whether public confessional unity under the blur masked or resolved private disagreement.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(homoousios_nicene__honorific_similarity_reading, 325, 381).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(homo_tr_t325, homoousios_nicene__honorific_similarity_reading, theater_ratio, 325, 0.3).
narrative_ontology:measurement_basis(homo_tr_t325, observed).
narrative_ontology:measurement(homo_tr_t335, homoousios_nicene__honorific_similarity_reading, theater_ratio, 335, 0.36).
narrative_ontology:measurement_basis(homo_tr_t335, observed).
narrative_ontology:measurement(homo_tr_t345, homoousios_nicene__honorific_similarity_reading, theater_ratio, 345, 0.44).
narrative_ontology:measurement_basis(homo_tr_t345, observed).
narrative_ontology:measurement(homo_tr_t355, homoousios_nicene__honorific_similarity_reading, theater_ratio, 355, 0.54).
narrative_ontology:measurement_basis(homo_tr_t355, observed).
narrative_ontology:measurement(homo_tr_t365, homoousios_nicene__honorific_similarity_reading, theater_ratio, 365, 0.6).
narrative_ontology:measurement_basis(homo_tr_t365, observed).
narrative_ontology:measurement(homo_tr_t375, homoousios_nicene__honorific_similarity_reading, theater_ratio, 375, 0.5).
narrative_ontology:measurement_basis(homo_tr_t375, observed).
narrative_ontology:measurement(homo_tr_t381, homoousios_nicene__honorific_similarity_reading, theater_ratio, 381, 0.38).
narrative_ontology:measurement_basis(homo_tr_t381, observed).

% Extraction over time
narrative_ontology:measurement(homo_be_t325, homoousios_nicene__honorific_similarity_reading, base_extractiveness, 325, 0.45).
narrative_ontology:measurement_basis(homo_be_t325, observed).
narrative_ontology:measurement(homo_be_t335, homoousios_nicene__honorific_similarity_reading, base_extractiveness, 335, 0.5).
narrative_ontology:measurement_basis(homo_be_t335, observed).
narrative_ontology:measurement(homo_be_t345, homoousios_nicene__honorific_similarity_reading, base_extractiveness, 345, 0.58).
narrative_ontology:measurement_basis(homo_be_t345, observed).
narrative_ontology:measurement(homo_be_t355, homoousios_nicene__honorific_similarity_reading, base_extractiveness, 355, 0.66).
narrative_ontology:measurement_basis(homo_be_t355, observed).
narrative_ontology:measurement(homo_be_t365, homoousios_nicene__honorific_similarity_reading, base_extractiveness, 365, 0.64).
narrative_ontology:measurement_basis(homo_be_t365, observed).
narrative_ontology:measurement(homo_be_t375, homoousios_nicene__honorific_similarity_reading, base_extractiveness, 375, 0.52).
narrative_ontology:measurement_basis(homo_be_t375, observed).
narrative_ontology:measurement(homo_be_t381, homoousios_nicene__honorific_similarity_reading, base_extractiveness, 381, 0.44).
narrative_ontology:measurement_basis(homo_be_t381, observed).

% Suppression requirement over time
narrative_ontology:measurement(homo_su_t325, homoousios_nicene__honorific_similarity_reading, suppression_requirement, 325, 0.48).
narrative_ontology:measurement_basis(homo_su_t325, observed).
narrative_ontology:measurement(homo_su_t335, homoousios_nicene__honorific_similarity_reading, suppression_requirement, 335, 0.55).
narrative_ontology:measurement_basis(homo_su_t335, observed).
narrative_ontology:measurement(homo_su_t345, homoousios_nicene__honorific_similarity_reading, suppression_requirement, 345, 0.62).
narrative_ontology:measurement_basis(homo_su_t345, observed).
narrative_ontology:measurement(homo_su_t355, homoousios_nicene__honorific_similarity_reading, suppression_requirement, 355, 0.72).
narrative_ontology:measurement_basis(homo_su_t355, observed).
narrative_ontology:measurement(homo_su_t365, homoousios_nicene__honorific_similarity_reading, suppression_requirement, 365, 0.68).
narrative_ontology:measurement_basis(homo_su_t365, observed).
narrative_ontology:measurement(homo_su_t375, homoousios_nicene__honorific_similarity_reading, suppression_requirement, 375, 0.58).
narrative_ontology:measurement_basis(homo_su_t375, observed).
narrative_ontology:measurement(homo_su_t381, homoousios_nicene__honorific_similarity_reading, suppression_requirement, 381, 0.5).
narrative_ontology:measurement_basis(homo_su_t381, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(homoousios_nicene__honorific_similarity_reading, identity_coordination).
narrative_ontology:affects_constraint(homoousios_nicene__honorific_similarity_reading, homoousios_nicene__metaphysical_equality_reading).
narrative_ontology:affects_constraint(homoousios_nicene__honorific_similarity_reading, homoousios_nicene__subordinationist_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'what Nicaea decided about homoousios' covers three structurally distinct constraints — one per reading of the kernel — each with its own stable epsilon, per the epsilon-invariance principle. This story (honorific similarity) carries a moderate, dual-victim epsilon: the blur extracts from both wings through one structure. The metaphysical-equality sibling carries a different epsilon (extraction concentrated on subordinationists once identity is fixed) and the subordinationist sibling another (extraction concentrated on the strict party). Downstream structure: this reading's enforcement record and eventual displacement supplied the political and conceptual conditions under which the strict definition was imposed (edge toward the metaphysical-equality sibling), while its honorific-unity language kept condemnation of hard subordinationists continuously available (edge toward the subordinationist sibling).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
