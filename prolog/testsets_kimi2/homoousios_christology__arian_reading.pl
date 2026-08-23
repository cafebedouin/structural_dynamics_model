% ============================================================================
% CONSTRAINT STORY: homoousios_christology__arian_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_homoousios_christology__arian_reading, []).

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
 *   constraint_id: homoousios_christology__arian_reading
 *   human_readable: Arian Subordinationist Christology (Created Logos)
 *   domain: historical/theological/political
 *
 * SUMMARY:
 *   The Arian reading of the homoousios christology kernel asserts that the
 *   Son is a created being, subordinate to the unoriginate Father, and not of
 *   identical substance (homoousios). From roughly 318 to 381 CE, this
 *   reading functioned as an ecclesiastical-political constraint that
 *   coordinated a trans-provincial network of non-Nicene bishops while
 *   extracting episcopal office and liturgical control from pro-Nicene
 *   bishops and their congregations through imperially enforced creedal
 *   subscription. The constraint is authored as one reading of a three-way
 *   kernel contest; the pro-Nicene and semi-Arian readings are structurally
 *   distinct constraints linked through the same historical kernel.
 *
 * KEY AGENTS:
 *   - arian_imperial_court: Agenda-setter (institutional/arbitrage) â emperors and court theologians who convene councils and enforce creeds
 *   - arian_clergy: Beneficiary (organized/constrained) â bishops and priests who gain and maintain office through the Arian theological network
 *   - pro_nicene_bishops: Payer (organized/constrained) â exiled and deposed bishops who bear the cost of theological exclusion
 *   - nicene_laity: Payer (powerless/trapped) â congregants deprived of Nicene episcopal leadership and compelled into Arian churches
 *   - semi_arian_theologians: Excluded (moderate/constrained) â middle-party theologians whose homoiousian formulas are marginalized by both poles
 *   - pro_nicene_theologian_observer: Observer (organized/analytical) â Athanasian and Hilary networks documenting coercion from exile
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(homoousios_christology__arian_reading, 0.62).
domain_priors:suppression_score(homoousios_christology__arian_reading, 0.85).
domain_priors:theater_ratio(homoousios_christology__arian_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(homoousios_christology__arian_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(homoousios_christology__arian_reading, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(homoousios_christology__arian_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(homoousios_christology__arian_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(homoousios_christology__arian_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(homoousios_christology__arian_reading, tangled_rope).
narrative_ontology:human_readable(homoousios_christology__arian_reading, "Arian Subordinationist Christology (Created Logos)").
narrative_ontology:topic_domain(homoousios_christology__arian_reading, "historical/theological/political").

domain_priors:requires_active_enforcement(homoousios_christology__arian_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(homoousios_christology__arian_reading, 'b230888b-66d4-4054-82c9-0a70a11c8afa').
narrative_ontology:cs_kernel_codification('b230888b-66d4-4054-82c9-0a70a11c8afa', fixed_text).
narrative_ontology:cs_authority_grounding('b230888b-66d4-4054-82c9-0a70a11c8afa', lineage).
narrative_ontology:cs_interpretation_layer_present('b230888b-66d4-4054-82c9-0a70a11c8afa').
narrative_ontology:cs_reading_relation('b230888b-66d4-4054-82c9-0a70a11c8afa', homoousios_christology__pro_nicene_reading, forecloses).
narrative_ontology:cs_reading_relation('b230888b-66d4-4054-82c9-0a70a11c8afa', homoousios_christology__semi_arian_reading, coexists_with).
narrative_ontology:cs_axiom('b230888b-66d4-4054-82c9-0a70a11c8afa', foundational, christ_is_created_being).
narrative_ontology:cs_axiom_status(christ_is_created_being, holdable).
narrative_ontology:cs_axiom_grounding('b230888b-66d4-4054-82c9-0a70a11c8afa', christ_is_created_being, theological).
narrative_ontology:cs_axiom('b230888b-66d4-4054-82c9-0a70a11c8afa', foundational, father_alone_is_unoriginate).
narrative_ontology:cs_axiom_status(father_alone_is_unoriginate, holdable).
narrative_ontology:cs_axiom_grounding('b230888b-66d4-4054-82c9-0a70a11c8afa', father_alone_is_unoriginate, theological).
narrative_ontology:cs_reference_frame('b230888b-66d4-4054-82c9-0a70a11c8afa', scriptural_monotheism_framework).
narrative_ontology:cs_drift_state('b230888b-66d4-4054-82c9-0a70a11c8afa', post_theodosian_restoration, gap(repudiation_pressure, severe, true)).
narrative_ontology:cs_created_at('b230888b-66d4-4054-82c9-0a70a11c8afa', '').
narrative_ontology:cs_kernel_id(homoousios_christology__arian_reading, homoousios_christology).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(homoousios_christology__arian_reading, arian_clergy).
narrative_ontology:constraint_beneficiary(homoousios_christology__arian_reading, arian_imperial_court).
narrative_ontology:constraint_victim(homoousios_christology__arian_reading, pro_nicene_bishops).
narrative_ontology:constraint_victim(homoousios_christology__arian_reading, nicene_laity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Emperors (Constantius II, Valens) and court theologians who convene synods, draft creedal formulas, and enforce subscription to Arian Christology as a condition of episcopal legitimacy. They collect political unity and ecclesiastical control, and can shift theological allegiance when imperial strategy changes.
narrative_ontology:constraint_stakeholder(homoousios_christology__arian_reading, arian_imperial_court, agenda_setter,
    institutional, generational, arbitrage, national).

% Bishops and presbyters who teach the subordination of the Son and maintain communion through shared theological formulas. They receive imperial appointment, retain their sees, and participate in a trans-provincial network that defines orthodoxy for the Eastern church under Arian emperors.
narrative_ontology:constraint_stakeholder(homoousios_christology__arian_reading, arian_clergy, beneficiary,
    organized, biographical, constrained, regional).

% Bishops who affirm the consubstantiality of the Son with the Father. They are deposed, exiled, and replaced by Arian appointees under imperial edict; their theological networks are driven underground or into diaspora.
narrative_ontology:constraint_stakeholder(homoousios_christology__arian_reading, pro_nicene_bishops, payer,
    organized, biographical, constrained, regional).

% Urban and rural Christians who lose access to Nicene episcopal leadership and liturgy; they are compelled to attend churches pastored by imperially appointed Arian clergy, with limited ability to relocate or worship outside the sanctioned structure.
narrative_ontology:constraint_stakeholder(homoousios_christology__arian_reading, nicene_laity, payer,
    powerless, biographical, trapped, local).

% Theologians who affirm homoiousios (similar substance) and attempt to mediate between Arian and Nicene poles. Their formulas are rejected by hardline Arian synods and they are pressured to conform to explicit creaturehood language.
narrative_ontology:constraint_stakeholder(homoousios_christology__arian_reading, semi_arian_theologians, excluded,
    moderate, biographical, constrained, regional).

% Athanasian and Hilary networks writing from exile or hiding; they document the imperial coercion of theology and argue that the Arian constraint is sustained by state power rather than apostolic tradition.
narrative_ontology:constraint_stakeholder(homoousios_christology__arian_reading, pro_nicene_theologian_observer, observer,
    organized, generational, analytical, regional).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Resolves the paradox of strict monotheism alongside Christ-devotion by locating the Son as the supreme creature through whom the unoriginate Father acts, thereby preserving absolute divine singularity while accommodating scriptural language about the Son's obedience and subordination.
% TRANSFER_FUNCTION: Moves episcopal appointment authority and theological legitimacy from traditional Nicene networks to Arian-affiliated clergy and imperially sponsored councils; moves liturgical attendance and ecclesiastical obedience from Nicene laity to Arian-appointed bishops.
% ABSENT_VOICES: Semi-Arian (homoiousian) theologians whose compromise formulas are excluded from hardline Arian councils; pro-Nicene laity in rural dioceses whose loyalty to exiled bishops is overridden by imperial appointment of Arian pastors.
% DISAPPEARANCE_RATIONALE: If the Arian subordinationist constraint vanished overnight, imperial councils would cease enforcing creaturehood creeds, pro-Nicene bishops would be restored to their sees, catechetical and liturgical texts would be recast around consubstantiality, and the Eastern episcopal map would reorganize around Nicene communion lines.
% FOUNDING_PROBLEM: How to maintain strict monotheism (one uncreated God) while accounting for the scriptural witness to Christ's pre-existence and redemptive work without collapsing into ditheism; and how to secure imperial religious unity against theological fragmentation.
% FOUNDING_PROBLEM_CORROBORATION: Pro-Nicene historians Sozomen and Socrates attest the theological problem persisted but was resolved by the Nicene settlement; pagan historian Ammianus Marcellinus corroborates from outside the Christian beneficiary set that theological disputes were manipulated by the imperial court for political unity.
narrative_ontology:disappearance_verdict(homoousios_christology__arian_reading, world_rearranges).
narrative_ontology:founding_problem_status(homoousios_christology__arian_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(homoousios_christology__arian_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(homoousios_christology__arian_reading, 'none', 1).
narrative_ontology:epsilon_provenance(homoousios_christology__arian_reading, 0.62, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(homoousios_christology__arian_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(homoousios_christology__arian_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(homoousios_christology__arian_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness peaks at 0.62 under Constantius II because the systematic displacement of Nicene bishops and reallocation of their sees constitutes a measurable transfer of office and authority. Suppression peaks at 0.85 because the constraint's persistence depends on imperial councils, exiles, and mandatory creedal subscription rather than on voluntary theological convergence. Theater_ratio rises to 0.48 under peak enforcement because much conciliar activity performed consensus that was actually secured by political threat. Accessibility_collapse is high (0.68) because once the imperial apparatus endorsed Arianism, Nicene alternatives became structurally inaccessible within the official church; resistance remains high (0.75) because the Nicene network maintained clandestine and exiled opposition throughout. The measurement grid is aligned: all three metrics are authored at every shared time point.
 *
 * PERSPECTIVAL GAP:
 *   From the Arian clerical seat, the constraint reads as rope or mountain â a restoration of scriptural monotheism against Nicene metaphysical innovation. From the pro-Nicene episcopal seat, it reads as snare â imperial coercion harnessed to a theological fiction. The engine computes this divergence from the structural data: beneficiary declarations and constrained exit on one side, victim declarations plus active enforcement on the other. The tangled_rope claimed type captures that both readings are structurally informed â genuine coordination among Arian clergy is inseparable from asymmetric extraction imposed on Nicene victims.
 *
 * DIRECTIONALITY LOGIC:
 *   The arian_imperial_court and arian_clergy are structural beneficiaries: they collect theological legitimacy and episcopal office from the constraint (low d). The pro_nicene_bishops and nicene_laity are structural targets: they pay through exile, deposition, and liturgical displacement (high d). The semi_arian_theologians are squeezed between positions â neither fully beneficiary nor target, their exclusion is the shadow price of the binary enforcement structure. Directionality is amplified for nicene_laity by their trapped exit (geographic and social immobility) and damped for the imperial court by its arbitrage-grade exit (the court can shift theological allegiance with political convenience, as seen across successive emperors).
 *
 * MANDATROPHY ANALYSIS:
 *   Classifying this as tangled_rope rather than snare prevents misreading the entire arrangement as pure extraction: the early Arian network genuinely coordinated bishops around a shared theological solution to monotheistic paradox, and the constraint is not reducible to cover story. Conversely, classifying it as tangled_rope rather than rope prevents misreading the imperial enforcement as benign coordination cost: the active suppression of Nicene bishops and the extraction of their sees is not a side effect but an integrated feature of the constraint's persistence. The R5 genealogy (founding_problem_status: dead) captures the atrophy: the coordination function decayed while the extraction machinery continued, which is exactly the lifecycle pattern that distinguishes tangled_rope from both pure coordination and pure extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    imperial_enforcement_vs_theological_coherence,
    'Is the constraint''s persistence driven by genuine theological conviction among the clergy, or by imperial enforcement that would collapse without state power?',
    'Comparison of theological subscription rates before and after shifts in imperial preference (Julian''s tolerance, Theodosius'' reversal).',
    'If enforcement-dependent, classification shifts toward snare; if conviction persists independently, remains tangled_rope or rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(imperial_enforcement_vs_theological_coherence, empirical, 'Whether the constraint rests on coercion or genuine doctrinal coordination.').

omega_variable(
    kernel_foreclosure_status,
    'Does the Arian axiom that Christ is created logically foreclose the pro-Nicene axiom of consubstantiality within a single theological framework, or do the readings merely coexist as incompatible party commitments?',
    'Formal analysis of the logical relationship between createdness and consubstantiality in fourth-century metaphysics and ecclesial practice.',
    'If genuine foreclosure, the kernel generates commitment-system contradiction and the engine should register structural incompatibility; if coexistence, the constraint is better modeled as inter-party competition.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_foreclosure_status, conceptual, 'Logical relationship between Arian and pro-Nicene axioms.').

omega_variable(
    semi_arian_exclusion,
    'Are Semi-Arian theologians structurally excluded from this constraint''s coordination function, or are they tacit beneficiaries of its resistance to Nicene consolidation?',
    'Historical analysis of Homoiousian alliance patterns with Arian and Nicene parties across the 350sâ370s.',
    'If excluded, the constraint''s victim set is narrower than the non-Nicene population; if tacit beneficiaries, the beneficiary set broadens and extraction is more diffuse.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(semi_arian_exclusion, empirical, 'Structural position of Semi-Arian theologians in the Arian constraint.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(homoousios_christology__arian_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(homo_tr_t0, homoousios_christology__arian_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(homo_tr_t10, homoousios_christology__arian_reading, theater_ratio, 10, 0.25).
narrative_ontology:measurement(homo_tr_t25, homoousios_christology__arian_reading, theater_ratio, 25, 0.42).
narrative_ontology:measurement(homo_tr_t40, homoousios_christology__arian_reading, theater_ratio, 40, 0.48).
narrative_ontology:measurement(homo_tr_t50, homoousios_christology__arian_reading, theater_ratio, 50, 0.45).
narrative_ontology:measurement(homo_tr_t60, homoousios_christology__arian_reading, theater_ratio, 60, 0.4).

% Extraction over time
narrative_ontology:measurement(homo_be_t0, homoousios_christology__arian_reading, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(homo_be_t10, homoousios_christology__arian_reading, base_extractiveness, 10, 0.38).
narrative_ontology:measurement(homo_be_t25, homoousios_christology__arian_reading, base_extractiveness, 25, 0.55).
narrative_ontology:measurement(homo_be_t40, homoousios_christology__arian_reading, base_extractiveness, 40, 0.62).
narrative_ontology:measurement(homo_be_t50, homoousios_christology__arian_reading, base_extractiveness, 50, 0.58).
narrative_ontology:measurement(homo_be_t60, homoousios_christology__arian_reading, base_extractiveness, 60, 0.48).

% Suppression requirement over time
narrative_ontology:measurement(homo_su_t0, homoousios_christology__arian_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(homo_su_t10, homoousios_christology__arian_reading, suppression_requirement, 10, 0.5).
narrative_ontology:measurement(homo_su_t25, homoousios_christology__arian_reading, suppression_requirement, 25, 0.78).
narrative_ontology:measurement(homo_su_t40, homoousios_christology__arian_reading, suppression_requirement, 40, 0.85).
narrative_ontology:measurement(homo_su_t50, homoousios_christology__arian_reading, suppression_requirement, 50, 0.72).
narrative_ontology:measurement(homo_su_t60, homoousios_christology__arian_reading, suppression_requirement, 60, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(homoousios_christology__arian_reading, identity_coordination).
narrative_ontology:affects_constraint(homoousios_christology__arian_reading, pro_nicene_reading).
narrative_ontology:affects_constraint(homoousios_christology__arian_reading, semi_arian_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the homoousios christology kernel. The arian_reading, pro_nicene_reading, and semi_arian_reading are sibling constraints that share a historical kernel but instantiate mutually incompatible structural arrangements due to differing axioms about Christ's relationship to the Father.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
