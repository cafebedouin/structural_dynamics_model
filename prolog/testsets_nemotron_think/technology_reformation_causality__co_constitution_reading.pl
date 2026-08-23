% ============================================================================
% CONSTRAINT STORY: technology_reformation_causality__co_constitution_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_technology_reformation_causality__co_constitution_reading, []).

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
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: technology_reformation_causality__co_constitution_reading
 *   human_readable: Printing Press–Reformation Co-Constitution Dynamic
 *   domain: historical/technological/religious
 *
 * SUMMARY:
 *   This constraint story instantiates the co_constitution_reading of the
 *   technology_reformation_causality kernel. It models the bidirectional
 *   causality where printing technology enabled the Reformation (rope-like
 *   coordination of mass dissemination) while reformers shaped what the press
 *   produced, creating a confessional print ecosystem whose alternatives
 *   atrophied (piton-like inertial authority). The extractiveness (ε=0.55)
 *   derives from the interaction term: the press alone was a coordination
 *   tool; reformers alone were local dissidents; their coupling produced
 *   asymmetric extraction where Catholic authority and radical dissent paid
 *   the cost. The claimed_type is tangled_rope because the constraint has
 *   genuine coordination (press solves dissemination) AND asymmetric
 *   extraction (magisterial reformers capture the press, exclude radicals,
 *   and their confessional structures persist by inertia).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(technology_reformation_causality__co_constitution_reading, 0.55).
domain_priors:suppression_score(technology_reformation_causality__co_constitution_reading, 0.45).
domain_priors:theater_ratio(technology_reformation_causality__co_constitution_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(technology_reformation_causality__co_constitution_reading, extractiveness, 0.55).
narrative_ontology:constraint_metric(technology_reformation_causality__co_constitution_reading, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(technology_reformation_causality__co_constitution_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(technology_reformation_causality__co_constitution_reading, accessibility_collapse, 0.52).
narrative_ontology:constraint_metric(technology_reformation_causality__co_constitution_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(technology_reformation_causality__co_constitution_reading, tangled_rope).
narrative_ontology:human_readable(technology_reformation_causality__co_constitution_reading, "Printing Press–Reformation Co-Constitution Dynamic").
narrative_ontology:topic_domain(technology_reformation_causality__co_constitution_reading, "historical/technological/religious").

domain_priors:requires_active_enforcement(technology_reformation_causality__co_constitution_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(technology_reformation_causality__co_constitution_reading, '0cfde4c5-f4d5-41be-99bd-efe26ac66016').
narrative_ontology:cs_kernel_codification('0cfde4c5-f4d5-41be-99bd-efe26ac66016', distributed).
narrative_ontology:cs_authority_grounding('0cfde4c5-f4d5-41be-99bd-efe26ac66016', practice).
narrative_ontology:cs_interpretation_layer_present('0cfde4c5-f4d5-41be-99bd-efe26ac66016').
narrative_ontology:cs_reading_relation('0cfde4c5-f4d5-41be-99bd-efe26ac66016', technology_reformation_causality__technological_determinism_reading, forecloses).
narrative_ontology:cs_reading_relation('0cfde4c5-f4d5-41be-99bd-efe26ac66016', technology_reformation_causality__beneficiary_agency_reading, influences).
narrative_ontology:cs_axiom('0cfde4c5-f4d5-41be-99bd-efe26ac66016', foundational, bidirectional_causality_claim).
narrative_ontology:cs_axiom_status(bidirectional_causality_claim, holdable).
narrative_ontology:cs_axiom_grounding('0cfde4c5-f4d5-41be-99bd-efe26ac66016', bidirectional_causality_claim, empirically_contingent).
narrative_ontology:cs_axiom('0cfde4c5-f4d5-41be-99bd-efe26ac66016', foundational, technology_as_enabling_not_determining).
narrative_ontology:cs_axiom_status(technology_as_enabling_not_determining, holdable).
narrative_ontology:cs_axiom_grounding('0cfde4c5-f4d5-41be-99bd-efe26ac66016', technology_as_enabling_not_determining, empirically_contingent).
narrative_ontology:cs_axiom('0cfde4c5-f4d5-41be-99bd-efe26ac66016', secondary, interaction_term_extractiveness).
narrative_ontology:cs_axiom_status(interaction_term_extractiveness, holdable).
narrative_ontology:cs_axiom_grounding('0cfde4c5-f4d5-41be-99bd-efe26ac66016', interaction_term_extractiveness, empirically_contingent).
narrative_ontology:cs_reference_frame('0cfde4c5-f4d5-41be-99bd-efe26ac66016', co_constitution_historiography).
narrative_ontology:cs_drift_state('0cfde4c5-f4d5-41be-99bd-efe26ac66016', contemporary_digital_humanities, gap(practice_drift, minor, true)).
narrative_ontology:cs_created_at('0cfde4c5-f4d5-41be-99bd-efe26ac66016', '').
narrative_ontology:cs_kernel_id(technology_reformation_causality__co_constitution_reading, technology_reformation_causality).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(technology_reformation_causality__co_constitution_reading, magisterial_reformers).
narrative_ontology:constraint_beneficiary(technology_reformation_causality__co_constitution_reading, print_shop_operators).
narrative_ontology:constraint_beneficiary(technology_reformation_causality__co_constitution_reading, vernacular_reading_public).
narrative_ontology:constraint_victim(technology_reformation_causality__co_constitution_reading, catholic_church_institutional_authority).
narrative_ontology:constraint_victim(technology_reformation_causality__co_constitution_reading, radical_reformers_excluded_from_print).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(technology_reformation_causality__co_constitution_reading, print_shop_operators).
narrative_ontology:constraint_vindicates(technology_reformation_causality__co_constitution_reading, bidirectional_causality_thesis).
narrative_ontology:constraint_vindicates(technology_reformation_causality__co_constitution_reading, technology_as_enabling_not_determining).
narrative_ontology:constraint_vindicates(technology_reformation_causality__co_constitution_reading, human_agency_shapes_technological_trajectory).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Luther, Calvin, Zwingli and their institutional successors. They author the theological content that fills the press, shape vernacular translations, and build confessionally-bound reading publics. Their authority becomes fused with the print medium — exit means abandoning the very public sphere they constitute. They benefit from unprecedented reach but become trapped in the interpretive frameworks they created.
narrative_ontology:constraint_stakeholder(technology_reformation_causality__co_constitution_reading, magisterial_reformers, agenda_setter,
    organized, generational, identity_locked, continental).
narrative_ontology:stakeholder_secondary_role(technology_reformation_causality__co_constitution_reading, magisterial_reformers, beneficiary).

% Printers in Basel, Strasbourg, Wittenberg, Geneva. They profit from Reformation pamphlet boom (1517-1530: 5000+ editions). But they bear financial risk of censorship, confiscation, and market saturation. Their exit is constrained by guild structures, capital investment in presses, and dependence on reformer networks for best-selling content.
narrative_ontology:constraint_stakeholder(technology_reformation_causality__co_constitution_reading, print_shop_operators, beneficiary,
    moderate, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(technology_reformation_causality__co_constitution_reading, print_shop_operators, payer).

% Urban artisans, merchants, minor nobility who gain access to vernacular Bibles, catechisms, polemics. They benefit from new textual access but their reading is channeled through confessionally-curated print ecosystems. Exit means literacy without approved texts — effectively constrained by the same print infrastructure that enabled access.
narrative_ontology:constraint_stakeholder(technology_reformation_causality__co_constitution_reading, vernacular_reading_public, beneficiary,
    powerless, biographical, constrained, continental).

% Papacy, episcopate, Inquisition, Index Congregation. They lose monopoly on scriptural interpretation and sacramental mediation. They respond with censorship machinery (Index, pre-publication licensing, press regulation) but the constraint's co-constitutive dynamic means their resistance itself shapes print culture. Exit from the constraint would mean surrendering doctrinal authority — structurally constrained by their institutional identity.
narrative_ontology:constraint_stakeholder(technology_reformation_causality__co_constitution_reading, catholic_church_institutional_authority, payer,
    institutional, civilizational, constrained, global).

% Anabaptists, Spiritualists, anti-Trinitarians. Their texts are suppressed by BOTH magisterial reformers and Catholic authorities. They lack press access, face execution for printing, and their ideas circulate only in manuscript. They are the clearest victims of the co-constitution: the press-reformer alliance that enabled Reformation also policed its boundaries.
narrative_ontology:constraint_stakeholder(technology_reformation_causality__co_constitution_reading, radical_reformers_excluded_from_print, payer,
    powerless, immediate, trapped, regional).
narrative_ontology:stakeholder_secondary_role(technology_reformation_causality__co_constitution_reading, radical_reformers_excluded_from_print, excluded).

% Electoral Saxony, Zurich Council, Geneva Council, Scandinavian monarchies. They authorize or suppress printing, mandate confessional uniformity, and capture the press for state-building. They have mobile exit — they can switch confessional allegiance (Cuius regio, eius religio) and redirect print patronage. They are the only actors with genuine arbitrage across the constraint.
narrative_ontology:constraint_stakeholder(technology_reformation_causality__co_constitution_reading, secular_princes_and_city_councils, agenda_setter,
    powerful, generational, mobile, national).

% Scholars from Eisenstein to Pettegree to contemporary digital humanists. They analyze the co-constitution from outside the historical constraint. Their exit is analytical — they can change interpretive frameworks without material cost. They are the seat that sees the full bidirectional structure.
narrative_ontology:constraint_stakeholder(technology_reformation_causality__co_constitution_reading, historians_of_early_modern_europe, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The press solved the coordination problem of mass textual dissemination: identical copies, wide geographic reach, durable fixation — enabling synchronized theological debate and confessional formation across fragmented German lands and beyond.
% TRANSFER_FUNCTION: Interpretive authority over scripture and sacramental practice transferred from Catholic magisterium (centralized, Latin, clerical) to magisterial reformers (distributed, vernacular, lay-accessible) via the press. Economic value transferred from manuscript culture to print shops. Legitimacy transferred from papal to princely authority.
% ABSENT_VOICES: Radical reformers (Anabaptists, Spiritualists) suppressed by both confessional establishments; women writers and readers whose access was mediated by male household heads; non-European recipients of printed missionary texts (Americas, Asia) who had no say in the European co-constitution; Jewish communities subject to press censorship and forced conversionary printing.
% DISAPPEARANCE_RATIONALE: If the co-constitutive dynamic vanished overnight (press reverts to manuscript-speed, reformers lose print amplification), the Reformation fragments into local heresies suppressed by Catholic authority; no confessional churches form; no print public sphere emerges; European state-church settlements (Augsburg 1555, Westphalia 1648) never occur. The world rearranges fundamentally.
% FOUNDING_PROBLEM: How to disseminate reform ideas beyond the reach of local episcopal censorship and create a synchronized movement across politically fragmented territories where Latin literacy was confined to clerics.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem (dissemination under censorship) is dead — print technology evolved beyond Reformation-era constraints, and modern states regulate communication through entirely different infrastructures. Corroborated by media historians outside Protestant tradition (Eisenstein, Febvre-Martin, Chartier) who document the transition from Reformation print to Enlightenment public sphere as a solved problem.
narrative_ontology:disappearance_verdict(technology_reformation_causality__co_constitution_reading, world_rearranges).
narrative_ontology:founding_problem_status(technology_reformation_causality__co_constitution_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(technology_reformation_causality__co_constitution_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(technology_reformation_causality__co_constitution_reading, 'none', 1).
narrative_ontology:epsilon_provenance(technology_reformation_causality__co_constitution_reading, 0.55, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(technology_reformation_causality__co_constitution_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(technology_reformation_causality__co_constitution_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(technology_reformation_causality__co_constitution_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.55 reflects the interaction term: not the press itself (low ε) nor reformers alone (no ε without press), but their coupling. Suppression 0.45 is moderate — Catholic Index and reformer censorship are real but porous (smuggled books, manuscript circulation). Theater_ratio 0.38 rises as confessional print becomes ritualized (catechisms, authorized Bibles, polemical repetition). Accessibility_collapse 0.52: alternatives exist (radical manuscript networks, Catholic manuscript culture) but are marginalized by print economics. Resistance 0.68: high from Catholic institutions, radical reformers, and internal reformer conflicts (Luther vs. Zwingli, Calvin vs. Servetus). The measurement grid shows extractiveness peaking at confessionalization (1535-1555), theater rising as confessional cultures solidify, suppression peaking at Index implementation (1559) then stabilizing.
 *
 * PERSPECTIVAL GAP:
 *   The engine should compute divergent seat types: from the reformer seat, the constraint appears as rope (coordination they built and benefit from); from the radical reformer seat, as snare (exclusion enforced by the reformer-press alliance); from the Catholic seat, as tangled_rope (genuine dissemination coordination they cannot control, extracting their authority); from the printer seat, as rope with extraction (profit + risk); from the prince seat, as scaffold (transitional tool for state-building). The analytical seat sees the full tangled_rope.
 *
 * DIRECTIONALITY LOGIC:
 *   Magisterial reformers are identity_locked — their authority is constituted through the print public they create; exit means self-dissolution. Print operators are constrained — capital-locked and network-dependent. Vernacular public is constrained — literacy channeled through confessional print. Catholic authority is constrained — institutional identity prevents exit but they adapt (Counter-Reformation print). Radical reformers are trapped — no press access, physical danger. Secular princes are mobile — they arbitrage confessional print for state-building. Historians are analytical — full structural visibility.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (dissemination under censorship) is dead — solved by print's evolution and state communication infrastructures. But the confessional print structures persist as pitons: authorized Bibles, catechisms, confessional publishing houses continue with atrophied function. The mandate (reform dissemination) has been resolved; the constraint (confessional print monopoly) persists by inertia. This is a classic mandatrophy case: the coordination function (press enabling reform) completed; the extraction function (confessional boundary maintenance) remains.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is this constraint a genuine co-constitution (bidirectional causality with ε from interaction) or a post-hoc synthesis that papers over the technological_determinism vs. beneficiary_agency dispute?',
    'Counterfactual simulation: if press technology had identical affordances but reformers lacked theological content (or vice versa), would the same extraction pattern emerge? Digital humanities network analysis of print-reformer co-citation networks across 1517-1555.',
    'If the interaction term is epiphenomenal, the constraint decomposes into two separate constraints (press-as-rope, reformers-as-piton) linked by network.affects_constraints. If irreducible, this single tangled_rope constraint stands.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Whether the co-constitution is structurally irreducible or a synthesis of two distinct constraints.').

omega_variable(
    technological_determinism_foreclosure,
    'Does the co_constitution_reading''s core premise (press enables but does not determine) logically foreclose the technological_determinism_reading (press makes Reformation inevitable) within any single historiographical framework?',
    'Analyze whether a single historian can hold both ''press caused Reformation'' and ''press enabled but reformers shaped'' without contradiction. The foreclosure test: does accepting bidirectional causality require rejecting monocausal technological determination?',
    'If forecloses, the readings cannot coexist in one framework — cs_structure.reading_relations = forecloses. If coexists_with, they are competing but compatible emphases.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(technological_determinism_foreclosure, conceptual, 'Logical relationship between co-constitution and technological determinism readings.').

omega_variable(
    beneficiary_agency_coexistence,
    'Does the co_constitution_reading structurally coexist with the beneficiary_agency_reading (reformers strategically deployed press as tool), or does the interaction-term ε create downstream pressure on the agency-only framing?',
    'Trace whether historians citing ''strategic deployment'' also invoke ''unintended consequences of print'' (Eisenstein''s ''unacknowledged revolution''). If the agency reading systematically generates the co-constitution reading as its own completion, the relation is influences.',
    'If influences, the agency reading creates legitimacy conditions for the co-constitution reading. If coexists_with, they are parallel camps.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_agency_coexistence, empirical, 'Structural pressure from agency reading to co-constitution reading.').

omega_variable(
    interaction_term_operationalization,
    'What exactly constitutes the ε=0.55 interaction term? Is it measurable as (joint outcome - press-only outcome - reformer-only outcome)?',
    'Quantitative history: compare Reformation spread in print-rich vs. print-poor territories with similar reformer presence; compare reformer success with vs. without press access. The interaction term is the residual after main effects.',
    'If the interaction term is statistically negligible, the constraint is mis-specified as tangled_rope — it should be two constraints (rope + piton). If robust, the single constraint stands.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(interaction_term_operationalization, empirical, 'Operationalization and measurability of the interaction-term extractiveness.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(technology_reformation_causality__co_constitution_reading, 1517, 1648).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(trccr_tr_t1517, technology_reformation_causality__co_constitution_reading, theater_ratio, 1517, 0.15).
narrative_ontology:measurement(trccr_tr_t1525, technology_reformation_causality__co_constitution_reading, theater_ratio, 1525, 0.22).
narrative_ontology:measurement(trccr_tr_t1535, technology_reformation_causality__co_constitution_reading, theater_ratio, 1535, 0.31).
narrative_ontology:measurement(trccr_tr_t1545, technology_reformation_causality__co_constitution_reading, theater_ratio, 1545, 0.35).
narrative_ontology:measurement(trccr_tr_t1555, technology_reformation_causality__co_constitution_reading, theater_ratio, 1555, 0.38).
narrative_ontology:measurement(trccr_tr_t1580, technology_reformation_causality__co_constitution_reading, theater_ratio, 1580, 0.37).
narrative_ontology:measurement(trccr_tr_t1618, technology_reformation_causality__co_constitution_reading, theater_ratio, 1618, 0.39).
narrative_ontology:measurement(trccr_tr_t1648, technology_reformation_causality__co_constitution_reading, theater_ratio, 1648, 0.38).

% Extraction over time
narrative_ontology:measurement(trccr_be_t1517, technology_reformation_causality__co_constitution_reading, base_extractiveness, 1517, 0.25).
narrative_ontology:measurement(trccr_be_t1525, technology_reformation_causality__co_constitution_reading, base_extractiveness, 1525, 0.42).
narrative_ontology:measurement(trccr_be_t1535, technology_reformation_causality__co_constitution_reading, base_extractiveness, 1535, 0.51).
narrative_ontology:measurement(trccr_be_t1545, technology_reformation_causality__co_constitution_reading, base_extractiveness, 1545, 0.55).
narrative_ontology:measurement(trccr_be_t1555, technology_reformation_causality__co_constitution_reading, base_extractiveness, 1555, 0.53).
narrative_ontology:measurement(trccr_be_t1580, technology_reformation_causality__co_constitution_reading, base_extractiveness, 1580, 0.54).
narrative_ontology:measurement(trccr_be_t1618, technology_reformation_causality__co_constitution_reading, base_extractiveness, 1618, 0.56).
narrative_ontology:measurement(trccr_be_t1648, technology_reformation_causality__co_constitution_reading, base_extractiveness, 1648, 0.55).

% Suppression requirement over time
narrative_ontology:measurement(trccr_su_t1517, technology_reformation_causality__co_constitution_reading, suppression_requirement, 1517, 0.3).
narrative_ontology:measurement(trccr_su_t1525, technology_reformation_causality__co_constitution_reading, suppression_requirement, 1525, 0.45).
narrative_ontology:measurement(trccr_su_t1535, technology_reformation_causality__co_constitution_reading, suppression_requirement, 1535, 0.52).
narrative_ontology:measurement(trccr_su_t1545, technology_reformation_causality__co_constitution_reading, suppression_requirement, 1545, 0.48).
narrative_ontology:measurement(trccr_su_t1555, technology_reformation_causality__co_constitution_reading, suppression_requirement, 1555, 0.42).
narrative_ontology:measurement(trccr_su_t1580, technology_reformation_causality__co_constitution_reading, suppression_requirement, 1580, 0.44).
narrative_ontology:measurement(trccr_su_t1618, technology_reformation_causality__co_constitution_reading, suppression_requirement, 1618, 0.46).
narrative_ontology:measurement(trccr_su_t1648, technology_reformation_causality__co_constitution_reading, suppression_requirement, 1648, 0.45).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(technology_reformation_causality__co_constitution_reading, information_standard).
narrative_ontology:boltzmann_floor_override(technology_reformation_causality__co_constitution_reading, 0.03).
narrative_ontology:affects_constraint(technology_reformation_causality__co_constitution_reading, technology_reformation_causality__technological_determinism_reading).
narrative_ontology:affects_constraint(technology_reformation_causality__co_constitution_reading, technology_reformation_causality__beneficiary_agency_reading).

% DUAL FORMULATION NOTE:
% This constraint family (technology_reformation_causality) decomposes the single label 'printing press caused the Reformation' into three structurally distinct constraints with different ε values: technological_determinism_reading (ε≈0.2, press as Mountain), beneficiary_agency_reading (ε≈0.35, press as Rope + reformers as agenda_setters), co_constitution_reading (ε≈0.55, interaction term as Tangled Rope). The upstream deterministic claim is often cited as evidence for the downstream co-constitution claim, creating network influence.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(technology_reformation_causality__co_constitution_reading, organized, 0.25).
constraint_indexing:directionality_override(technology_reformation_causality__co_constitution_reading, moderate, 0.55).
constraint_indexing:directionality_override(technology_reformation_causality__co_constitution_reading, powerless, 0.85).
constraint_indexing:directionality_override(technology_reformation_causality__co_constitution_reading, institutional, 0.7).
constraint_indexing:directionality_override(technology_reformation_causality__co_constitution_reading, powerful, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
