% ============================================================================
% CONSTRAINT STORY: homoousios_nicene__honorific_similarity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [RESOLVED MANDATROPHY]
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
 *   constraint_id: homoousios_nicene__honorific_similarity_reading
 *   human_readable: Homoousios as Honorific Likeness — the Homoiousian Settlement Regime
 *   domain: historical theology/ecclesiastical history/philosophy of religion
 *
 * SUMMARY:
 *   The Nicene term homoousios became, within a generation of Nicaea, a
 *   contested kernel: one word, three rival significations, each
 *   instantiating a structurally different constraint. This story authors ONE
 *   reading only — the honorific-similarity reading, under which homoousios
 *   signifies likeness or kindred being (blurring toward homoiousios), an
 *   honorific unity that refuses ontological reduction rather than asserting
 *   strict metaphysical identity. The interval 0-30 maps to 351-381 CE: the
 *   reading's rise under Constantius II, its peak at the
 *   Sirmium-Ancyra-Dated-Creed sequence (357-359), and its displacement after
 *   the Cappadocian ousia/hypostasis synthesis and Constantinople I (381).
 *   Per the epsilon-invariance principle, the sibling readings
 *   (metaphysical_equality_reading, subordinationist_reading) are separate
 *   stories with their own epsilon values, victim sets, and enforcement
 *   geometries; this file links them via network.affects_constraints and
 *   documents the decomposition in the dual-formulation note. The
 *   claim/metric gap is deliberate: the reading CLAIMS tangled_rope (genuine
 *   mediation plus real asymmetric cost), while the authored metrics describe
 *   its actual operation independently — the engine measures any divergence.
 *
 * KEY AGENTS:
 *   - semi_arian_moderate_bishops: Primary beneficiary (organized/constrained) — collects office security and legitimacy-space while the likeness settlement holds
 *   - apophatic_traditions: Secondary beneficiary (moderate/identity_locked) — gains shelter from demands to define the divine essence
 *   - strict_nicene_enforcers: Primary target (organized/identity_locked) — bears condemnation as rigid and modalist-leaning for insisting on identity
 *   - hard_subordinationists: Co-target (organized/constrained) — bears anathema despite the softened language
 *   - local_episcopal_conciliar_networks: Agenda setter (institutional/mobile) — drafts, revises, and promulgates the formulae; holds definitional agenda control
 *   - imperial_court: Co-agenda-setter and incidental beneficiary (institutional/arbitrage) — enforces whichever formula promises civic quiet
 *   - ordinary_laity: Diffuse payer (powerless/trapped) — bears liturgical disruption and violence with no seat in any synod
 *   - patristic_historians: Analytical observer (analytical/analytical) — sees the full three-reading structure from outside
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(homoousios_nicene__honorific_similarity_reading, 0.31).
domain_priors:suppression_score(homoousios_nicene__honorific_similarity_reading, 0.44).
domain_priors:theater_ratio(homoousios_nicene__honorific_similarity_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(homoousios_nicene__honorific_similarity_reading, extractiveness, 0.31).
narrative_ontology:constraint_metric(homoousios_nicene__honorific_similarity_reading, suppression_requirement, 0.44).
narrative_ontology:constraint_metric(homoousios_nicene__honorific_similarity_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(homoousios_nicene__honorific_similarity_reading, accessibility_collapse, 0.38).
narrative_ontology:constraint_metric(homoousios_nicene__honorific_similarity_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(homoousios_nicene__honorific_similarity_reading, tangled_rope).
narrative_ontology:human_readable(homoousios_nicene__honorific_similarity_reading, "Homoousios as Honorific Likeness — the Homoiousian Settlement Regime").
narrative_ontology:topic_domain(homoousios_nicene__honorific_similarity_reading, "historical theology/ecclesiastical history/philosophy of religion").

domain_priors:requires_active_enforcement(homoousios_nicene__honorific_similarity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(homoousios_nicene__honorific_similarity_reading, 'e1ed25cf-61b0-4a7b-95c3-9b3fcc7496a2').
narrative_ontology:cs_kernel_codification('e1ed25cf-61b0-4a7b-95c3-9b3fcc7496a2', formalized).
narrative_ontology:cs_authority_grounding('e1ed25cf-61b0-4a7b-95c3-9b3fcc7496a2', lineage).
narrative_ontology:cs_interpretation_layer_present('e1ed25cf-61b0-4a7b-95c3-9b3fcc7496a2').
narrative_ontology:cs_reading_relation('e1ed25cf-61b0-4a7b-95c3-9b3fcc7496a2', homoousios_nicene__metaphysical_equality_reading, forecloses).
narrative_ontology:cs_reading_relation('e1ed25cf-61b0-4a7b-95c3-9b3fcc7496a2', homoousios_nicene__subordinationist_reading, influences).
narrative_ontology:cs_axiom('e1ed25cf-61b0-4a7b-95c3-9b3fcc7496a2', foundational, honorific_unity_without_ontological_identity).
narrative_ontology:cs_axiom_status(honorific_unity_without_ontological_identity, holdable).
narrative_ontology:cs_axiom_grounding('e1ed25cf-61b0-4a7b-95c3-9b3fcc7496a2', honorific_unity_without_ontological_identity, theological).
narrative_ontology:cs_axiom('e1ed25cf-61b0-4a7b-95c3-9b3fcc7496a2', secondary, apophatic_reserve_on_divine_essence).
narrative_ontology:cs_axiom_status(apophatic_reserve_on_divine_essence, holdable).
narrative_ontology:cs_axiom_grounding('e1ed25cf-61b0-4a7b-95c3-9b3fcc7496a2', apophatic_reserve_on_divine_essence, deontological).
narrative_ontology:cs_reference_frame('e1ed25cf-61b0-4a7b-95c3-9b3fcc7496a2', honorific_likeness_confessional_standard).
narrative_ontology:cs_drift_state('e1ed25cf-61b0-4a7b-95c3-9b3fcc7496a2', post_constantinopolitan_settlement, gap(repudiation_pressure, severe, false)).
narrative_ontology:cs_created_at('e1ed25cf-61b0-4a7b-95c3-9b3fcc7496a2', '2026-06-12T00:00:00Z').
narrative_ontology:cs_kernel_id(homoousios_nicene__honorific_similarity_reading, homoousios_nicene).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(homoousios_nicene__honorific_similarity_reading, semi_arian_moderate_bishops).
narrative_ontology:constraint_beneficiary(homoousios_nicene__honorific_similarity_reading, apophatic_traditions).
narrative_ontology:constraint_victim(homoousios_nicene__honorific_similarity_reading, strict_nicene_enforcers).
narrative_ontology:constraint_victim(homoousios_nicene__honorific_similarity_reading, hard_subordinationists).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(homoousios_nicene__honorific_similarity_reading, imperial_court).
narrative_ontology:constraint_victim(homoousios_nicene__honorific_similarity_reading, ordinary_laity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Eastern bishops of the homoiousian party (Basil of Ancyra, Eustathius of Sebaste, Eleusius of Cyzicus) who confess the Son as like-in-essence to the Father. While the likeness settlement holds, they keep their sees, retain imperial access, and can sign the creed without conceding either the strict-identity gloss or the subordinationist gloss. Leaving the settlement means schism, deposition, or signing a formula they take to err in one direction or the other.
narrative_ontology:constraint_stakeholder(homoousios_nicene__honorific_similarity_reading, semi_arian_moderate_bishops, beneficiary,
    organized, biographical, constrained, continental).

% Theologians and ascetic teachers who hold that the divine essence outruns human speech. The likeness reading shelters them: they may confess the creed's union-language reverently without being forced to define ousia or count essences. Their stake is the preserved space to worship without metaphysical definition; abandoning that reserve would dissolve the very method that constitutes them, so they do not treat exit as a live option.
narrative_ontology:constraint_stakeholder(homoousios_nicene__honorific_similarity_reading, apophatic_traditions, beneficiary,
    moderate, generational, identity_locked, continental).

% Bishops and teachers in the Athanasian line who hold that homoousios means one identical divine essence, co-eternal, with no interval between Father and Son. Under the likeness regime their precision is read as rigidity bordering on Sabellian conflation; they lose sees to exile, watch moderate signatories occupy their churches, and can neither sign the likeness formula in conscience nor drop the identity claim without betraying what they hold the fathers handed down.
narrative_ontology:constraint_stakeholder(homoousios_nicene__honorific_similarity_reading, strict_nicene_enforcers, payer,
    organized, generational, identity_locked, continental).

% Teachers of the Arian and Anomoean parties who hold the Son's being to be derived from the Father. The likeness formula softens the creed's language but keeps their position outside the fence: they remain liable to anathema whenever the middle party enforces. Some maneuver through imperial patronage and court bishops, but leaving the confessional contest means surrendering their churches and congregations.
narrative_ontology:constraint_stakeholder(homoousios_nicene__honorific_similarity_reading, hard_subordinationists, payer,
    organized, biographical, constrained, continental).

% Provincial synods and episcopal conferences that draft, revise, and promulgate credal formulae — Sirmium, Ancyra 358, the Dated Creed of 359, Seleucia and Ariminum. Each assembly calibrates wording to the pressures before it, which gives it agenda control over what confession suffices for communion. When a formula fails to hold the churches together, the same networks bear the blame and reconvene to try new wording.
narrative_ontology:constraint_stakeholder(homoousios_nicene__honorific_similarity_reading, local_episcopal_conciliar_networks, agenda_setter,
    institutional, generational, mobile, continental).

% The emperor and his court bishops, who need ecclesiastical peace for administrative stability. They enforce whichever formula currently promises quiet, gain leverage over appointments and exile decisions, and can withdraw patronage from one party and extend it to another as conditions change. Their commitment to any particular reading is instrumental and reversible.
narrative_ontology:constraint_stakeholder(homoousios_nicene__honorific_similarity_reading, imperial_court, agenda_setter,
    institutional, biographical, arbitrage, continental).
narrative_ontology:stakeholder_secondary_role(homoousios_nicene__honorific_similarity_reading, imperial_court, beneficiary).

% Congregations in cities like Alexandria, Antioch, and Constantinople who receive a changed formula every few years. They bear the liturgical disruption, the exile of familiar pastors, and episodes of street violence between confessional factions. They hold no seat in any synod, and their practical choice is limited to whatever communion is available in their city.
narrative_ontology:constraint_stakeholder(homoousios_nicene__honorific_similarity_reading, ordinary_laity, payer,
    powerless, biographical, trapped, regional).

% Modern scholars reconstructing the semantic range of homoousios and the politics of the fourth-century councils. They read the rival significations as competing construals of one formula, assess each on its own documentary record, and hold no confessional stake in which reading prevailed.
narrative_ontology:constraint_stakeholder(homoousios_nicene__honorific_similarity_reading, patristic_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(homoousios_nicene__honorific_similarity_reading, semi_arian_moderate_bishops).
narrative_ontology:fixing_cost_class(homoousios_nicene__honorific_similarity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single confessional formula broad enough to hold moderate eastern bishops, wavering western signatories, and court parties in one communion — solving the collective-action problem of defining minimum doctrinal agreement without forcing a metaphysical decision no assembly could carry unanimously.
% TRANSFER_FUNCTION: Moves doctrinal legitimacy and episcopal office security from the confessional wings (strict-identity teachers and subordinationist teachers) to the moderate center; moves interpretive authority from fixed metropolitan definition toward local bishops exercising pastoral discretion.
% ABSENT_VOICES: Ordinary laity had no synodical seat though they bore the disruptions; rural clergy and monastic communities outside the episcopal networks were unrepresented; and the condemned wings themselves were often literally absent from the assemblies that ruled on them (exiled bishops signing under duress or not at all). They are located outside the conciliar process — petitioning courts, writing treatises from exile, or rioting in the streets.
% DISAPPEARANCE_RATIONALE: If the likeness settlement vanished overnight, the communion splits immediately into an identity camp and a subordination camp: sees change hands within months, imperial policy is forced to pick a side, and liturgical formulae are rewritten city by city. The arrangement was load-bearing for fourth-century ecclesiastical peace.
% FOUNDING_PROBLEM: After Nicaea (325) the church needed a formula that honored the Son's divinity against subordinationist diminution while avoiding the modalist collapse of Father into Son that strict identity language seemed to invite. The likeness reading was built to solve the problem of confessing real union without performing ontological arithmetic.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the beneficiary set: the strict Nicene wing — this reading's chief opponents — acknowledged the modalist anxiety the reading addressed even while rejecting its solution, and the subordinationist wing attests the anti-diminution motivation from the other side. Contemporary patristic historiography, holding no confessional stake, attests both the problem's reality and the settlement's eventual failure to hold assent.
narrative_ontology:disappearance_verdict(homoousios_nicene__honorific_similarity_reading, world_rearranges).
narrative_ontology:founding_problem_status(homoousios_nicene__honorific_similarity_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(homoousios_nicene__honorific_similarity_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(homoousios_nicene__honorific_similarity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(homoousios_nicene__honorific_similarity_reading, 0.31, 'stealth/ox-alpha', 'none', direct).

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
 *   End-state extractiveness is 0.31 with a mid-interval peak of 0.55: the settlement's costs fell on both wings simultaneously (precision taxed as rigidity, subordination taxed as heresy), which is real imposition, but the reading's own lights frame those costs as necessary boundary-keeping rather than rent — hence a moderate reading-indexed epsilon rather than a high one. Suppression (0.44 end-state) tracks enforcement machinery, and the suppression_requirement series is authored deliberately: this story specifically traces enforcement-capacity change — the imperial ratchet of 353-360 building the machinery up, then its decay and capture after 361 when imperial direction lapsed and the homoian party bypassed the likeness apparatus. Theater_ratio rises monotonically (0.16 to 0.40): as the mediating function failed, adherence degraded into pro-forma signature and formula recitation — Goodhart drift visible before the arrangement's terminal displacement. Accessibility_collapse is low (0.38) because breadth is the point: both wings remain live alternatives throughout, unlike a snare that closes exits. Resistance is high (0.68): Athanasian intransigence and Arian counter-mobilization met the settlement continuously. Fixing cost was prohibitive: every attempt to replace calibration with a definitive ruling (the Sirmium ban on ousia-language, the Dated Creed) purchased temporary quiet at the price of deeper schism, so the cost of settling exceeded the benefit of clarity. All series share one time grid ({0,4,8,14,22,30}); endpoint values equal the base_properties scalars.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently from the same text. From the moderate bishop's seat the formula is charity: breadth that keeps the church one and lets a man sign in good conscience. From the strict Nicene seat the same formula is enforced vagueness: a tax on precision that rewards equivocation and punishes the one group trying to say exactly what Nicaea meant. From the subordinationist seat it is a trap: softened language wrapped around an unchanged anathema. The two victim wings attempted coalition exactly once (the Alexandria 362 brokerage attempt) and failed — identity-lock on both sides prevented a durable alliance of the taxed against the taxers, which is why the victim set never converted shared grievance into combined leverage.
 *
 * DIRECTIONALITY LOGIC:
 *   The moderate bishops sit near the beneficiary end: the settlement subsidizes them with legitimacy-space financed by the wings' exclusion. Apophatic traditions likewise collect shelter, amplified by identity-lock — their method is constituted by reverent reserve, so they cannot exit without self-dissolution, which anchors them deep in the subsidized position. The two wings sit near the target end: both pay condemnation risk for positions the settlement defines as extreme. Identity-lock binds the strict Nicaeans especially tightly — their teaching office is fused with the identity claim, so they are trapped nearer the full-target end than their mobility would otherwise suggest. The imperial court derives damped benefit through arbitrage: it supports the settlement instrumentally and exits cheaply when alignment shifts, keeping it nearer the beneficiary side than a committed enforcer would sit. Ordinary laity bear a diffuse, roughly symmetric burden. Suppression is authored as a raw structural property and is NOT scaled by power or scope — only extractiveness is scaled, by directionality and spatial scope in the engine's computation.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope classification prevents mislabeling in both directions. A pure-extraction reading would erase the settlement's genuine coordination achievement: it held a communion together across a semantic dispute no assembly could resolve, and it solved a real collective-action problem for roughly a decade. A pure-coordination reading would erase the documented costs borne by both wings and the enforcement machinery those costs required. On mandatrophy: the founding problem (confessing union without ontological arithmetic) remained live long after this settlement died — what atrophied was the settlement's specific mandate, brokering between wings via calibrated vagueness. Once Constantinople 381 fixed the identity signification and the ousia/hypostasis distinction gave precision a stable home, the likeness formula had nothing left to mediate; signatures continued pro-forma (visible in the rising theater_ratio) after the function had departed. Hence mandatrophy_resolved is declared true: the arrangement outlived its function and persisted theatrically until displacement completed.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_instantiation,
    'This constraint instantiates the honorific_similarity_reading of kernel homoousios_nicene; what structural deltas would the sibling readings introduce if instantiated instead?',
    'Comparative audit of the three stories'' beneficiary/victim sets and enforcement geometry: the metaphysical_equality_reading collapses the victim set to subordinationists alone and recenters enforcement in metropolitan definition; the subordinationist_reading inverts the victim set onto strict Nicaeans.',
    'Adopting a sibling reading changes who sits inside the fence, so per-seat classifications and the extraction profile redistribute across the agent surface; this story''s numbers are valid only for the likeness reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_instantiation, conceptual, 'Committer structure: one of three rival readings of the homoousios kernel.').

omega_variable(
    homoousios_semantic_range,
    'Was the ousia/hypostasis terminology stable enough across 351-381 for ''likeness versus identity'' to mark a determinate boundary, or were the terms still fluid?',
    'Philological corpus study of pre-381 usage: chart what signatories took themselves to be asserting when they signed likeness formulae, and whether the identity/likeness distinction was drawn consistently by contemporaries.',
    'If the terms were fluid, part of this reading''s distinctness dissolves into shared imprecision and its victim set shrinks to those condemned for explicit positions rather than terminological drift; if stable, the three readings are cleanly separable constraints.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(homoousios_semantic_range, empirical, 'Whether the semantic boundary this reading polices was determinate in its own era.').

omega_variable(
    authority_grounding_framing,
    'Is this reading''s authority grounded in apostolic-conciliar lineage (succession transmitting the formula) or in episcopal practice (local pastoral discretion as the operating standard)?',
    'Test which seat the reading''s own defenders treated as decisive when challenged: appeals to Nicaea''s succession and the fathers indicate lineage; appeals to the bishop''s discretion within his own church indicate practice.',
    'Under the practice framing the interpretive layer widens further and the terminal drift reads as practice_drift rather than repudiation_pressure; under lineage the declared framing stands as authored.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(authority_grounding_framing, conceptual, 'CS-framing under-determination: two coherent authority framings for the same reading.').

omega_variable(
    imperial_coercion_dependence,
    'How much of the settlement''s enforcement capacity was structurally its own (synodal discipline, communion exclusion) versus borrowed imperial coercion?',
    'Compare enforcement intensity across reigns: the Constantius ratchet (353-361) versus the post-361 fragmentation when imperial direction lapsed and parties realigned.',
    'If most suppression was imperial, the arrangement''s native suppression is far lower than measured and its persistence was externally propped; if synodal discipline carried it, the measured suppression is intrinsic to the arrangement.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(imperial_coercion_dependence, empirical, 'Whether the enforcement machinery was native or borrowed.').

omega_variable(
    wing_symmetry_of_costs,
    'Did the two victim wings bear symmetric costs under the likeness regime, or did enforcement oscillate with imperial favor so that one wing paid at a time?',
    'Code the condemnation and exile record 351-381 by wing and year; test for phase-alignment with imperial alignments and court personnel.',
    'Asymmetric oscillation would mean the regime functioned as a sequencing device alternating targets rather than a steady boundary, shifting the extraction profile from balanced to alternating and changing which seat computes as primary target at any given time.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(wing_symmetry_of_costs, empirical, 'Whether the two wings paid symmetrically or in alternation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(homoousios_nicene__honorific_similarity_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(homo_tr_t0, homoousios_nicene__honorific_similarity_reading, theater_ratio, 0, 0.16).
narrative_ontology:measurement(homo_tr_t4, homoousios_nicene__honorific_similarity_reading, theater_ratio, 4, 0.2).
narrative_ontology:measurement(homo_tr_t8, homoousios_nicene__honorific_similarity_reading, theater_ratio, 8, 0.24).
narrative_ontology:measurement(homo_tr_t14, homoousios_nicene__honorific_similarity_reading, theater_ratio, 14, 0.29).
narrative_ontology:measurement(homo_tr_t22, homoousios_nicene__honorific_similarity_reading, theater_ratio, 22, 0.35).
narrative_ontology:measurement(homo_tr_t30, homoousios_nicene__honorific_similarity_reading, theater_ratio, 30, 0.4).

% Extraction over time
narrative_ontology:measurement(homo_be_t0, homoousios_nicene__honorific_similarity_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(homo_be_t4, homoousios_nicene__honorific_similarity_reading, base_extractiveness, 4, 0.46).
narrative_ontology:measurement(homo_be_t8, homoousios_nicene__honorific_similarity_reading, base_extractiveness, 8, 0.55).
narrative_ontology:measurement(homo_be_t14, homoousios_nicene__honorific_similarity_reading, base_extractiveness, 14, 0.49).
narrative_ontology:measurement(homo_be_t22, homoousios_nicene__honorific_similarity_reading, base_extractiveness, 22, 0.37).
narrative_ontology:measurement(homo_be_t30, homoousios_nicene__honorific_similarity_reading, base_extractiveness, 30, 0.31).

% Suppression requirement over time
narrative_ontology:measurement(homo_su_t0, homoousios_nicene__honorific_similarity_reading, suppression_requirement, 0, 0.42).
narrative_ontology:measurement(homo_su_t4, homoousios_nicene__honorific_similarity_reading, suppression_requirement, 4, 0.61).
narrative_ontology:measurement(homo_su_t8, homoousios_nicene__honorific_similarity_reading, suppression_requirement, 8, 0.67).
narrative_ontology:measurement(homo_su_t14, homoousios_nicene__honorific_similarity_reading, suppression_requirement, 14, 0.58).
narrative_ontology:measurement(homo_su_t22, homoousios_nicene__honorific_similarity_reading, suppression_requirement, 22, 0.5).
narrative_ontology:measurement(homo_su_t30, homoousios_nicene__honorific_similarity_reading, suppression_requirement, 30, 0.44).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(homoousios_nicene__honorific_similarity_reading, identity_coordination).
narrative_ontology:affects_constraint(homoousios_nicene__honorific_similarity_reading, metaphysical_equality_reading).
narrative_ontology:affects_constraint(homoousios_nicene__honorific_similarity_reading, subordinationist_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'the Nicene homoousios' covers three structurally distinct claims with different epsilon values, victim sets, and enforcement geometries: this honorific-similarity reading (victims on both wings, beneficiaries in the moderate center and apophatic traditions), the metaphysical_equality_reading (centralized enforcement, subordinationists as sole victims), and the subordinationist_reading (strict Nicaeans as victims). Each is authored as its own story per the epsilon-invariance principle; the family is linked here. The equality reading is upstream in eventual authority — it won at Constantinople 381 — and its success is the proximate cause of this reading's terminal repudiation; this reading in turn shaped both siblings' operating environments during its ascendancy by absorbing their moderate constituencies.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
