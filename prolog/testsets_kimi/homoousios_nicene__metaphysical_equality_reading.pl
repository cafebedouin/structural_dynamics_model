% ============================================================================
% CONSTRAINT STORY: homoousios_nicene__metaphysical_equality_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
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
 *   constraint_id: homoousios_nicene__metaphysical_equality_reading
 *   human_readable: Nicene Homoousios: Metaphysical Equality Reading
 *   domain: historical_theology/ecclesiastical_history/philosophy_of_religion
 *
 * SUMMARY:
 *   The Council of Nicaea (325 CE) established homoousios ('of one
 *   substance') as the definitive formula for the relationship between Father
 *   and Son, interpreted in this reading as strict ontological
 *   equalityâco-eternal, same divine essence, no subordination in being.
 *   This constraint operates through conciliar and imperial enforcement: the
 *   episcopal hierarchy gains exclusive interpretive authority over
 *   Trinitarian doctrine, while heterodox christologies (Arian, Eunomian,
 *   subordinationist) are anathematized and expelled from sacramental
 *   communion. The constraint claims to solve the coordination problem of
 *   doctrinal fragmentation; the authored metrics track the active
 *   enforcement and extraction required to maintain this specific
 *   metaphysical equality reading against structurally excluded alternatives.
 *
 * KEY AGENTS:
 *   - episcopal_hierarchy: Agenda-setter (institutional/constrained) â administers conciliar authority, captures interpretive power
 *   - heterodox_christologies: Primary payer (powerless/trapped) â anathematized, sacramentally excluded
 *   - imperial_authority: Agenda-setter/beneficiary (institutional/mobile) â enforces via edict, extracts political unity
 *   - subordinationist_theologians: Secondary payer (moderate/trapped) â doctrinally condemned
 *   - nicene_orthodox_community: Beneficiary (organized/identity_locked) â receives doctrinal boundary, pays in constrained theological inquiry
 *   - modern_historical_theologian: Observer (analytical) â external analytical seat
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(homoousios_nicene__metaphysical_equality_reading, 0.78).
domain_priors:suppression_score(homoousios_nicene__metaphysical_equality_reading, 0.88).
domain_priors:theater_ratio(homoousios_nicene__metaphysical_equality_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(homoousios_nicene__metaphysical_equality_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(homoousios_nicene__metaphysical_equality_reading, suppression_requirement, 0.88).
narrative_ontology:constraint_metric(homoousios_nicene__metaphysical_equality_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(homoousios_nicene__metaphysical_equality_reading, accessibility_collapse, 0.82).
narrative_ontology:constraint_metric(homoousios_nicene__metaphysical_equality_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(homoousios_nicene__metaphysical_equality_reading, tangled_rope).
narrative_ontology:human_readable(homoousios_nicene__metaphysical_equality_reading, "Nicene Homoousios: Metaphysical Equality Reading").
narrative_ontology:topic_domain(homoousios_nicene__metaphysical_equality_reading, "historical_theology/ecclesiastical_history/philosophy_of_religion").

domain_priors:requires_active_enforcement(homoousios_nicene__metaphysical_equality_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(homoousios_nicene__metaphysical_equality_reading, 'f91792df-a6dc-4432-a443-89cfa06ba1c5').
narrative_ontology:cs_kernel_codification('f91792df-a6dc-4432-a443-89cfa06ba1c5', fixed_text).
narrative_ontology:cs_authority_grounding('f91792df-a6dc-4432-a443-89cfa06ba1c5', lineage).
narrative_ontology:cs_interpretation_layer_present('f91792df-a6dc-4432-a443-89cfa06ba1c5').
narrative_ontology:cs_reading_relation('f91792df-a6dc-4432-a443-89cfa06ba1c5', homoousios_nicene__subordinationist_reading, forecloses).
narrative_ontology:cs_reading_relation('f91792df-a6dc-4432-a443-89cfa06ba1c5', homoousios_nicene__honorific_similarity_reading, forecloses).
narrative_ontology:cs_axiom('f91792df-a6dc-4432-a443-89cfa06ba1c5', foundational, father_and_son_one_ousia).
narrative_ontology:cs_axiom_status(father_and_son_one_ousia, holdable).
narrative_ontology:cs_axiom_grounding('f91792df-a6dc-4432-a443-89cfa06ba1c5', father_and_son_one_ousia, theological).
narrative_ontology:cs_axiom('f91792df-a6dc-4432-a443-89cfa06ba1c5', foundational, co_eternal_no_subordination_in_being).
narrative_ontology:cs_axiom_status(co_eternal_no_subordination_in_being, holdable).
narrative_ontology:cs_axiom_grounding('f91792df-a6dc-4432-a443-89cfa06ba1c5', co_eternal_no_subordination_in_being, theological).
narrative_ontology:cs_reference_frame('f91792df-a6dc-4432-a443-89cfa06ba1c5', trinitarian_ontological_equality).
narrative_ontology:cs_drift_state('f91792df-a6dc-4432-a443-89cfa06ba1c5', post_theodosian_enforcement_381, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('f91792df-a6dc-4432-a443-89cfa06ba1c5', '').
narrative_ontology:cs_kernel_id(homoousios_nicene__metaphysical_equality_reading, homoousios_nicene).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(homoousios_nicene__metaphysical_equality_reading, episcopal_hierarchy).
narrative_ontology:constraint_beneficiary(homoousios_nicene__metaphysical_equality_reading, imperial_authority).
narrative_ontology:constraint_beneficiary(homoousios_nicene__metaphysical_equality_reading, nicene_orthodox_community).
narrative_ontology:constraint_victim(homoousios_nicene__metaphysical_equality_reading, heterodox_christologies).
narrative_ontology:constraint_victim(homoousios_nicene__metaphysical_equality_reading, subordinationist_theologians).
narrative_ontology:constraint_vindicates(homoousios_nicene__metaphysical_equality_reading, trinitarian_ontological_equality).
narrative_ontology:constraint_vindicates(homoousios_nicene__metaphysical_equality_reading, nicene_creed_authority).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers and interprets the Nicene formula through conciliar authority. Gains exclusive interpretive power over Trinitarian doctrine, determining orthodoxy and anathematizing deviations. Their legitimacy derives from apostolic succession and conciliar tradition.
narrative_ontology:constraint_stakeholder(homoousios_nicene__metaphysical_equality_reading, episcopal_hierarchy, agenda_setter,
    institutional, generational, constrained, global).

% Theological traditions that understand the Son's relationship to the Father differently. Subjected to anathema, exclusion from sacramental community, and imperial penalties. Their theological voice is excluded by the conciliar enforcement mechanism that binds the imperial church.
narrative_ontology:constraint_stakeholder(homoousios_nicene__metaphysical_equality_reading, heterodox_christologies, payer,
    powerless, biographical, trapped, regional).

% Convenes ecumenical councils and enforces doctrinal conformity through imperial edict. Benefits from a unified church that legitimates imperial rule and suppresses sectarian division. Retains theological mobility not available to the episcopal office.
narrative_ontology:constraint_stakeholder(homoousios_nicene__metaphysical_equality_reading, imperial_authority, agenda_setter,
    institutional, generational, mobile, global).

% Theologians who hold that the Son derives being from the Father or is functionally subordinate. Their theological position is ruled heterodox by the conciliar standard, subjecting them to condemnation and loss of office or communion.
narrative_ontology:constraint_stakeholder(homoousios_nicene__metaphysical_equality_reading, subordinationist_theologians, payer,
    moderate, biographical, trapped, regional).

% The broader community of believers and clergy for whom the homoousios formula provides doctrinal clarity and boundary against theological fragmentation. Their religious identity becomes fused to the metaphysical equality claim, making theological exit unthinkable.
narrative_ontology:constraint_stakeholder(homoousios_nicene__metaphysical_equality_reading, nicene_orthodox_community, beneficiary,
    organized, generational, identity_locked, global).

% Contemporary scholars analyzing the fourth-century Trinitarian controversies from outside the operative ecclesiastical power structure. They assess the historical enforcement of doctrine without bearing its costs or receiving its benefits.
narrative_ontology:constraint_stakeholder(homoousios_nicene__metaphysical_equality_reading, modern_historical_theologian, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(homoousios_nicene__metaphysical_equality_reading, episcopal_hierarchy).
narrative_ontology:fixing_cost_class(homoousios_nicene__metaphysical_equality_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Resolves fourth-century Trinitarian controversy by establishing a shared formula (homoousios) that coordinates ecclesiastical belief and practice across the imperial church, preventing doctrinal fragmentation.
% TRANSFER_FUNCTION: Moves interpretive authority over divine nature from distributed theological discourse to the episcopal conciliar hierarchy; moves the cost of exclusion (anathema, exile, loss of communion) from the enforcing hierarchy to heterodox christologies and subordinationist theologians.
% ABSENT_VOICES: Arian laity, non-episcopal theological voices, women, lower clergy, and monastic communities outside the conciliar circuit are structurally excluded from the interpretive process; their theological intuitions about the Son's relationship to the Father carry no weight in the conciliar framework.
% DISAPPEARANCE_RATIONALE: If the metaphysical equality reading vanished overnight, the imperial church would lose its primary boundary marker for orthodoxy. The episcopal hierarchy would lose exclusive interpretive authority over Trinitarian doctrine; suppressed subordinationist and Arian communities would re-enter discourse; the fourth-century ecclesiastical landscape would reorganize around competing christological frameworks.
% FOUNDING_PROBLEM: Doctrinal fragmentation in the early church regarding the Son's relationship to the Father, threatening ecclesiastical unity and the church's public theological witness in the Roman Empire.
% FOUNDING_PROBLEM_CORROBORATION: Modern historical theologians and sociologists of religion attest the coordination problem was real but dispute whether the specific metaphysical equality reading was the necessary solution. Imperial chroniclers (Sozomen, Socrates Scholasticus) provide external contemporary observation, though their accounts are shaped by the victorious orthodox party. No source entirely outside the theological dispute corroborates the particular metaphysical equality reading as the only resolution.
narrative_ontology:disappearance_verdict(homoousios_nicene__metaphysical_equality_reading, world_rearranges).
narrative_ontology:founding_problem_status(homoousios_nicene__metaphysical_equality_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(homoousios_nicene__metaphysical_equality_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(homoousios_nicene__metaphysical_equality_reading, 'none', 1).
narrative_ontology:epsilon_provenance(homoousios_nicene__metaphysical_equality_reading, 0.78, 'kimi-k2.6', 'none', direct).

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
 *   Extractiveness (0.78) is high because anathematization and imperial exclusion impose severe costs on heterodox groups. Suppression (0.88) is higher still because the constraint's persistence depends on actively excluding subordinationist and Arian alternativesâwithout enforcement, the metaphysical equality reading loses its monopoly. Theater_ratio (0.45) reflects substantial performative maintenance: conciliar ritual, creedal recitation, and anathema formulas that exceed the minimum coordination requirement. Accessibility_collapse (0.82) is high because once inside the Nicene framework, Trinitarian alternatives become nearly unthinkable. Resistance (0.55) captures the substantial but ultimately overcome Arian and subordinationist opposition during the fourth century. The temporal measurements trace a U-shaped trajectory: the constraint weakens during the Arian ascendancy (mid-century) and hardens under Theodosian enforcement (381 CE).
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seats (episcopal hierarchy, imperial authority) experience the constraint as necessary coordination of theological unity and imperial stability. The payer seats (heterodox groups, subordinationist theologians) experience the same structure as coercive extraction that suppresses their theological existence. The engine computes this divergence from the structural data: same constraint, opposite directionality derivations, producing different per-seat classifications.
 *
 * DIRECTIONALITY LOGIC:
 *   The episcopal_hierarchy and imperial_authority are structural beneficiaries of the constraint: they collect interpretive authority and political legitimation respectively, placing them near the beneficiary end of directionality (low d). The nicene_orthodox_community is also a beneficiary but with identity_locked exit, which may damp the subsidy effect since their exit is structurally closed despite their beneficiary status. Heterodox_christologies and subordinationist_theologians are declared victims; they bear the costs of anathematization and exclusion, placing them near the full-target end (high d). The modern observer sits at analytical exit with neutral directionality.
 *
 * MANDATROPHY ANALYSIS:
 *   Without the R5 genealogy interview and the victim/beneficiary asymmetry, this constraint could be misread as a Rope (pure coordination of ecclesiastical unity) or a Snare (pure extraction by the episcopal hierarchy). The Tangled Rope classification is warranted because: (1) there is a genuine coordination problem (fourth-century doctrinal fragmentation was real); (2) there are identifiable victims (anathematized heterodox); (3) the constraint requires active enforcement to hold (conciliar decrees plus imperial coercion). The arrangement has outlived its original imperial context while persisting through institutional inertia, but the founding problem status remains contested rather than dead.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    theological_vs_political_function,
    'Does the homoousios constraint primarily coordinate genuine theological consensus about the divine nature, or does it function as an imperial-ecclesiastical power structure extracting compliance for political unity?',
    'Comparative analysis of conciliar behavior in periods of weak versus strong imperial involvement; theological output quality assessment independent of enforcement.',
    'If primarily political, extraction and suppression metrics should read higher and the coordination function is cover; if primarily theological, the tangled rope classification tilts toward coordination-heavy hybrid.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(theological_vs_political_function, conceptual, 'Whether the constraint''s core function is theological coordination or political extraction.').

omega_variable(
    orthodox_identity_lock,
    'To what extent is compliance with the metaphysical equality reading sustained by internalized theological identity rather than external enforcement?',
    'Analysis of conversion and apostasy patterns; theological diversity tolerance in post-Christendom contexts where external enforcement is absent.',
    'High internalization would mean suppression persists even when external coercion is removed, increasing effective extraction for identity-locked agents.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(orthodox_identity_lock, empirical, 'Structural versus internalized suppression mechanism.').

omega_variable(
    kernel_reading_incompatibility,
    'Does the metaphysical equality reading logically foreclose the honorific similarity reading, or can they coexist as different emphases within a broader theological framework?',
    'Historical analysis of whether any fourth-century theologian successfully held both strict homoousios and a non-identitarian similarity model simultaneously without conciliar condemnation.',
    'If foreclosed, the relation to honorific_similarity_reading is correctly typed as forecloses; if cohabitable, the engine''s contradiction detection may need refinement and the relation should be coexists_with.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_incompatibility, conceptual, 'Uncertainty about the logical relationship between sibling readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(homoousios_nicene__metaphysical_equality_reading, 0, 55).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(homo_tr_t0, homoousios_nicene__metaphysical_equality_reading, theater_ratio, 0, 0.3).
narrative_ontology:measurement(homo_tr_t10, homoousios_nicene__metaphysical_equality_reading, theater_ratio, 10, 0.32).
narrative_ontology:measurement(homo_tr_t20, homoousios_nicene__metaphysical_equality_reading, theater_ratio, 20, 0.38).
narrative_ontology:measurement(homo_tr_t30, homoousios_nicene__metaphysical_equality_reading, theater_ratio, 30, 0.42).
narrative_ontology:measurement(homo_tr_t40, homoousios_nicene__metaphysical_equality_reading, theater_ratio, 40, 0.44).
narrative_ontology:measurement(homo_tr_t50, homoousios_nicene__metaphysical_equality_reading, theater_ratio, 50, 0.45).
narrative_ontology:measurement(homo_tr_t55, homoousios_nicene__metaphysical_equality_reading, theater_ratio, 55, 0.45).

% Extraction over time
narrative_ontology:measurement(homo_be_t0, homoousios_nicene__metaphysical_equality_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(homo_be_t10, homoousios_nicene__metaphysical_equality_reading, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(homo_be_t20, homoousios_nicene__metaphysical_equality_reading, base_extractiveness, 20, 0.42).
narrative_ontology:measurement(homo_be_t30, homoousios_nicene__metaphysical_equality_reading, base_extractiveness, 30, 0.45).
narrative_ontology:measurement(homo_be_t40, homoousios_nicene__metaphysical_equality_reading, base_extractiveness, 40, 0.62).
narrative_ontology:measurement(homo_be_t50, homoousios_nicene__metaphysical_equality_reading, base_extractiveness, 50, 0.72).
narrative_ontology:measurement(homo_be_t55, homoousios_nicene__metaphysical_equality_reading, base_extractiveness, 55, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(homo_su_t0, homoousios_nicene__metaphysical_equality_reading, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(homo_su_t10, homoousios_nicene__metaphysical_equality_reading, suppression_requirement, 10, 0.52).
narrative_ontology:measurement(homo_su_t20, homoousios_nicene__metaphysical_equality_reading, suppression_requirement, 20, 0.45).
narrative_ontology:measurement(homo_su_t30, homoousios_nicene__metaphysical_equality_reading, suppression_requirement, 30, 0.48).
narrative_ontology:measurement(homo_su_t40, homoousios_nicene__metaphysical_equality_reading, suppression_requirement, 40, 0.7).
narrative_ontology:measurement(homo_su_t50, homoousios_nicene__metaphysical_equality_reading, suppression_requirement, 50, 0.82).
narrative_ontology:measurement(homo_su_t55, homoousios_nicene__metaphysical_equality_reading, suppression_requirement, 55, 0.88).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(homoousios_nicene__metaphysical_equality_reading, identity_coordination).
narrative_ontology:affects_constraint(homoousios_nicene__metaphysical_equality_reading, subordinationist_reading).
narrative_ontology:affects_constraint(homoousios_nicene__metaphysical_equality_reading, honorific_similarity_reading).

% DUAL FORMULATION NOTE:
% The homoousios_nicene kernel decomposes into three structurally distinct constraints: the metaphysical equality reading (strict ontological identity), the subordinationist_reading (derivative divinity), and the honorific_similarity_reading (homoiousios-like likeness, not strict identity). Each reading carries a different epsilon, victim set, and enforcement structure. They are linked as a constraint family because they compete to occupy the same theological and institutional space.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
