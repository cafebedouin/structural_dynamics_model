% ============================================================================
% CONSTRAINT STORY: divine_marriage_command__coercion_visibility_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_divine_marriage_command__coercion_visibility_reading, []).

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
 *   constraint_id: divine_marriage_command__coercion_visibility_reading
 *   human_readable: Polygamy Suspension as Coercion-Driven Doctrinal Shift (Coercion Visibility Reading)
 *   domain: religious/political/theological
 *
 * SUMMARY:
 *   The 1890 Manifesto (Official Declaration 1) suspended the practice of
 *   plural marriage in the LDS Church under sustained federal coercion
 *   (Edmunds Act, Edmunds-Tucker Act, disincorporation threat, imprisonment
 *   of leadership). This reading — the coercion-visibility reading —
 *   acknowledges that the Manifesto was a direct response to existential
 *   institutional pressure, not a spontaneous revelation. Theological
 *   legitimacy is retroactively derived from the necessity of institutional
 *   survival: the Church could not fulfill its divine mission if it ceased to
 *   exist as a legal entity. This creates a tangled rope: the constraint
 *   coordinates the community's survival and legal integration
 *   (beneficiaries: institutional leadership, federal authorities) while
 *   extracting theological conformity from committed members and suppressing
 *   dissident continuity (victims: theologically committed members, dissident
 *   factions). The constraint requires active enforcement (excommunication,
 *   temple recommend denial, disciplinary councils) to maintain the
 *   suspension.
 *
 * KEY AGENTS:
 *   - institutional_leadership: Primary agenda_setter (institutional/arbitrage) — sets doctrine, manages survival, collects institutional continuity
 *   - federal_authorities: Co-beneficiary (institutional/arbitrage) — extracts compliance with monogamy norms, gains legal integration of Utah
 *   - theologically_committed_members: Primary payer (organized/identity_locked) — bear theological discontinuity, loss of exaltation pathway, identity fracture
 *   - dissident_factions: Payer/excluded (powerless/trapped) — maintain prior doctrine, face excommunication, form fundamentalist schisms
 *   - rank_and_file_members: Beneficiary/payer (moderate/constrained) — gain legal protection, social integration; bear cognitive dissonance, theological revision
 *   - analytical_observers: Observer (analytical/analytical) — trace the coercion-doctrine-negotiation pattern across religious history
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(divine_marriage_command__coercion_visibility_reading, 0.78).
domain_priors:suppression_score(divine_marriage_command__coercion_visibility_reading, 0.72).
domain_priors:theater_ratio(divine_marriage_command__coercion_visibility_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(divine_marriage_command__coercion_visibility_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(divine_marriage_command__coercion_visibility_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(divine_marriage_command__coercion_visibility_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(divine_marriage_command__coercion_visibility_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(divine_marriage_command__coercion_visibility_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(divine_marriage_command__coercion_visibility_reading, tangled_rope).
narrative_ontology:human_readable(divine_marriage_command__coercion_visibility_reading, "Polygamy Suspension as Coercion-Driven Doctrinal Shift (Coercion Visibility Reading)").
narrative_ontology:topic_domain(divine_marriage_command__coercion_visibility_reading, "religious/political/theological").

domain_priors:requires_active_enforcement(divine_marriage_command__coercion_visibility_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(divine_marriage_command__coercion_visibility_reading, '775fa19a-57a6-40b1-8569-a33513603e6c').
narrative_ontology:cs_kernel_codification('775fa19a-57a6-40b1-8569-a33513603e6c', fixed_text).
narrative_ontology:cs_authority_grounding('775fa19a-57a6-40b1-8569-a33513603e6c', extraction).
narrative_ontology:cs_interpretation_layer_present('775fa19a-57a6-40b1-8569-a33513603e6c').
narrative_ontology:cs_reading_relation('775fa19a-57a6-40b1-8569-a33513603e6c', divine_marriage_command__continuationist_reading, forecloses).
narrative_ontology:cs_reading_relation('775fa19a-57a6-40b1-8569-a33513603e6c', divine_marriage_command__substitutionist_reading, coexists_with).
narrative_ontology:cs_axiom('775fa19a-57a6-40b1-8569-a33513603e6c', foundational, institutional_survival_grounds_theological_legitimacy).
narrative_ontology:cs_axiom_status(institutional_survival_grounds_theological_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('775fa19a-57a6-40b1-8569-a33513603e6c', institutional_survival_grounds_theological_legitimacy, instrumental).
narrative_ontology:cs_axiom('775fa19a-57a6-40b1-8569-a33513603e6c', foundational, coercion_is_valid_input_for_doctrinal_shift).
narrative_ontology:cs_axiom_status(coercion_is_valid_input_for_doctrinal_shift, holdable).
narrative_ontology:cs_axiom_grounding('775fa19a-57a6-40b1-8569-a33513603e6c', coercion_is_valid_input_for_doctrinal_shift, empirically_contingent).
narrative_ontology:cs_reference_frame('775fa19a-57a6-40b1-8569-a33513603e6c', prophetic_continuity_framework).
narrative_ontology:cs_drift_state('775fa19a-57a6-40b1-8569-a33513603e6c', post_manifesto_institutional_consolidation, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('775fa19a-57a6-40b1-8569-a33513603e6c', '').
narrative_ontology:cs_kernel_id(divine_marriage_command__coercion_visibility_reading, divine_marriage_command).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(divine_marriage_command__coercion_visibility_reading, institutional_leadership).
narrative_ontology:constraint_beneficiary(divine_marriage_command__coercion_visibility_reading, federal_authorities).
narrative_ontology:constraint_victim(divine_marriage_command__coercion_visibility_reading, theologically_committed_members).
narrative_ontology:constraint_victim(divine_marriage_command__coercion_visibility_reading, dissident_factions).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(divine_marriage_command__coercion_visibility_reading, theologically_committed_members).
narrative_ontology:constraint_beneficiary(divine_marriage_command__coercion_visibility_reading, rank_and_file_members).
narrative_ontology:constraint_victim(divine_marriage_command__coercion_visibility_reading, rank_and_file_members).
narrative_ontology:constraint_vindicates(divine_marriage_command__coercion_visibility_reading, institutional_survival_requires_doctrinal_flexibility).
narrative_ontology:constraint_vindicates(divine_marriage_command__coercion_visibility_reading, theological_legitimacy_can_derive_from_necessity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Controls doctrinal interpretation and disciplinary machinery. Authored the Manifesto under federal threat of disincorporation and leadership imprisonment. Gains institutional survival, legal recognition, statehood for Utah, and ongoing authority to define orthodoxy. Can revise doctrine further (as with 1978 priesthood revelation) — exit from this constraint is doctrinal revision, not departure.
narrative_ontology:constraint_stakeholder(divine_marriage_command__coercion_visibility_reading, institutional_leadership, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(divine_marriage_command__coercion_visibility_reading, institutional_leadership, beneficiary).

% Applied sustained legal, military, and economic pressure (Edmunds Act 1882, Edmunds-Tucker Act 1887, disincorporation, asset seizure, imprisonment) to force abandonment of polygamy. Gained compliance with federal marriage law, integration of Utah Territory, and a precedent for federal authority over religious practice. Their exit from this constraint is irrelevant — they are the external coercive power.
narrative_ontology:constraint_stakeholder(divine_marriage_command__coercion_visibility_reading, federal_authorities, beneficiary,
    institutional, generational, arbitrage, national).

% Theologically constituted by plural marriage as an exaltation requirement (D&C 132). The Manifesto severs their pathway to highest salvation without their consent. They gain legal protection and social integration but lose the defining practice of their religious identity. Exit means abandoning eternal family sealings, community, and self-concept — identity_locked because the theology constitutes the self. Some remain quietly committed; some form dissident factions.
narrative_ontology:constraint_stakeholder(divine_marriage_command__coercion_visibility_reading, theologically_committed_members, payer,
    organized, biographical, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(divine_marriage_command__coercion_visibility_reading, theologically_committed_members, beneficiary).

% Maintain that plural marriage remains a divine commandment (continuationist reading). Face excommunication, loss of temple access, family rupture, and legal persecution. Their exit options are nil: they cannot remain in the institution without submitting to the Manifesto, and leaving severs them from the community and salvation economy they believe is true. They are the M-set gap — the constraint fails to bind them, requiring ongoing suppression.
narrative_ontology:constraint_stakeholder(divine_marriage_command__coercion_visibility_reading, dissident_factions, payer,
    powerless, biographical, trapped, local).
narrative_ontology:stakeholder_secondary_role(divine_marriage_command__coercion_visibility_reading, dissident_factions, excluded).

% Most never practiced plural marriage but accepted it as doctrine. Gain mainstream acceptance, legal protection, property rights, and social integration. Bear cognitive dissonance: the prophet suspended a 'forever' commandment. Their exit is constrained by family, community, and cultural embeddedness — leaving costs social capital but not eternal identity.
narrative_ontology:constraint_stakeholder(divine_marriage_command__coercion_visibility_reading, rank_and_file_members, beneficiary,
    moderate, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(divine_marriage_command__coercion_visibility_reading, rank_and_file_members, payer).

% Historians, sociologists, theologians, and political theorists who trace the coercion-doctrine-negotiation pattern. They see the full structure: federal power, institutional survival, theological revision, dissident continuity. They neither collect nor pay — they classify.
narrative_ontology:constraint_stakeholder(divine_marriage_command__coercion_visibility_reading, analytical_observers, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(divine_marriage_command__coercion_visibility_reading, institutional_leadership).
narrative_ontology:fixing_cost_class(divine_marriage_command__coercion_visibility_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the existential coordination problem of institutional survival under federal coercion: the Church could not continue as a legal entity, hold property, or operate temples while practicing plural marriage. The Manifesto coordinates the community's integration into the American legal-political order.
% TRANSFER_FUNCTION: Moves theological authority and member conformity from the polygamy-practicing membership to the institutional leadership, as the price of survival. Federal authorities receive compliance with monogamy norms and Utah's integration. Members lose a defining practice; leadership gains control of doctrinal boundaries.
% ABSENT_VOICES: The continuationist dissidents (fundamentalist groups) are structurally excluded — they would object that the Manifesto is a capitulation, not revelation, but their voice is suppressed by excommunication and legal marginalization. Pre-1890 leadership (Taylor, Woodruff pre-Manifesto) are absent by time — their private writings suggest they viewed the Manifesto as necessity, not revelation, but they cannot testify now.
% DISAPPEARANCE_RATIONALE: If the Manifesto and its enforcement vanished overnight: fundamentalist groups would lose their primary institutional antagonist and gain theological vindication; the LDS Church would face an immediate legitimacy crisis (prophetic credibility, temple sealings, D&C 132 status); federal-religious relations would reopen; the theological boundary between 'mainstream' and 'fundamentalist' Mormonism would collapse. The world rearranges because arrangements depend on this constraint.
% FOUNDING_PROBLEM: Federal coercion (Edmunds Act, Edmunds-Tucker Act, disincorporation threat, imprisonment of leadership) threatened the institutional existence of the LDS Church, which the leadership believed was necessary for the fulfillment of its divine mission. The Church could not survive as a legal entity while practicing plural marriage.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated by federal legislative record (Edmunds Act, Edmunds-Tucker Act), court cases (Late Corp. of the Church of Jesus Christ of Latter-day Saints v. United States, 1890), and the Church's own petition for statehood (which required a constitutional ban on polygamy). The coercion is a matter of public historical record, not self-asserted by beneficiaries. However, the institutional leadership's framing of the Manifesto as 'revelation' rather than 'capitulation' remains the official account — the corroboration establishes the coercion, not the legitimacy derivation.
narrative_ontology:disappearance_verdict(divine_marriage_command__coercion_visibility_reading, world_rearranges).
narrative_ontology:founding_problem_status(divine_marriage_command__coercion_visibility_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(divine_marriage_command__coercion_visibility_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(divine_marriage_command__coercion_visibility_reading, 'none', 1).
narrative_ontology:epsilon_provenance(divine_marriage_command__coercion_visibility_reading, 0.78, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(divine_marriage_command__coercion_visibility_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(divine_marriage_command__coercion_visibility_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(divine_marriage_command__coercion_visibility_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises from 0.35 to 0.78 over the interval: initially the Manifesto had genuine coordination value (survival, statehood, reduced persecution) with modest extraction; over time the survival necessity recedes but the doctrinal suspension hardens into a permanent boundary, converting coordination into extraction. Suppression requirement rises from 0.4 to 0.72: early enforcement was porous (post-Manifesto polygamy continued covertly with leadership tolerance); later enforcement hardens as the boundary becomes identity-defining. Theater ratio rises from 0.15 to 0.45: the 'revelation' framing becomes increasingly performative as the coercion-acknowledgment reading gains scholarly traction but remains institutionally marginalized. The constraint is a tangled rope because it solves a real coordination problem (institutional survival under coercion) AND extracts asymmetric costs from members whose theological identity was constituted by the now-suspended practice.
 *
 * PERSPECTIVAL GAP:
 *   From the institutional_leadership seat: the constraint is a scaffold that became a rope — temporary survival measure that achieved permanent coordination value (legal integration, mainstream legitimacy). From the theologically_committed_members seat: the constraint is a snare — their defining practice was extracted under duress, the 'revelation' cover story is theater, and dissent is punished. From the dissident_factions seat: the constraint is a snare with no coordination function — they are excluded from the survival benefit and bear the full extraction. The engine computes these divergences from the structural data: leadership has arbitrage exit (can revise doctrine), committed members are identity_locked (theology constitutes self), dissidents are trapped (excommunication severs community).
 *
 * DIRECTIONALITY LOGIC:
 *   institutional_leadership and federal_authorities are beneficiaries (d ~ 0.1-0.2): they gain survival/integration/control. theologically_committed_members are targets (d ~ 0.85): identity_locked by theology, exit means losing eternal family sealings and community. dissident_factions are targets (d ~ 0.95): trapped, no exit without total loss. rank_and_file_members are near-symmetric (d ~ 0.5): constrained exit (social/family costs), mixed benefit/cost. The beneficiary/victim declarations drive the engine's directionality derivation; no overrides needed.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (federal coercion threatening institutional existence) is dead — the coercion has ceased, the Church is legally secure, Utah is a state. Yet the constraint persists and hardens. This is mandatrophy: the arrangement outlives its founding coordination function and becomes extractive theater. The theater_ratio rise documents this. The founding_problem_status 'dead' with disappearance_verdict 'world_rearranges' flags the capture/zombie pattern: the world would rearrange (fundamentalist schisms would lose institutional opponent, theology would face open revision) but the constraint persists because the institutional_leadership seat captures the extraction (control of doctrine, boundary maintenance).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is this constraint a genuine Mountain of revealed truth, or a Tangled Rope of institutional survival under coercion?',
    'Historical analysis of internal deliberations (e.g., Council of Fifty minutes, Woodruff''s journals) to determine whether the 1890 Manifesto was framed internally as revelation or capitulation. If the authority structure itself acknowledges coercion as the causal input, the Mountain claim fails.',
    'If Mountain, the constraint is ε-invariant natural law (extractiveness ~0). If Tangled Rope/Snare, the constraint is a constructed arrangement with high extractiveness and acknowledged exogenous pressure — the reading''s legitimacy depends on whether necessity can ground theological authority.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Whether the constraint is a natural-law Mountain or a coercion-responsive construct — the core committer-frame ambiguity for this kernel reading.').

omega_variable(
    coercion_acknowledgment_legitimacy_crisis,
    'Does acknowledging federal coercion as the causal input for doctrinal shift create a legitimacy crisis for the authority structure?',
    'Track institutional discourse post-1890: if leadership frames the Manifesto as ''revelation'' publicly while conceding ''necessity'' privately, the gap is managed. If the coercion-acknowledgment becomes the official account, the authority structure must either ground legitimacy in institutional survival (extraction grounding) or face repudiation pressure.',
    'If legitimacy crisis is real, the authority_grounding shifts from ''lineage'' to ''extraction'' — the arrangement persists because it extracts compliance/survival benefit, not because it transmits revelation. This changes the CS classification and the constraint''s drift trajectory.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coercion_acknowledgment_legitimacy_crisis, conceptual, 'Whether the coercion-visibility reading''s core claim (Manifesto = response to coercion) destabilizes the authority structure''s own legitimacy ground.').

omega_variable(
    m_set_gap_closure,
    'Is the M-set gap (the set of members for whom the constraint is binding) actually closed by this reading, or does it reopen through dissident continuity?',
    'Measure continuity of polygamous practice and theological commitment post-1890: fundamentalist schisms, underground practice, and theological dissent indicate the M-set was not closed — the constraint failed to bind its intended subjects.',
    'If M-set gap remains open, the constraint''s suppression requirement is higher than measured (active enforcement against dissidents) and its extraction is more snare-like (coercion without coordination benefit for the excluded).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(m_set_gap_closure, empirical, 'Whether the coercion-visibility reading actually resolves the membership-boundedness problem it claims to solve.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(divine_marriage_command__coercion_visibility_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(divine_marriage_command__coercion_visibility_reading_tr_t0, divine_marriage_command__coercion_visibility_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(divine_marriage_command__coercion_visibility_reading_tr_t10, divine_marriage_command__coercion_visibility_reading, theater_ratio, 10, 0.2).
narrative_ontology:measurement(divine_marriage_command__coercion_visibility_reading_tr_t20, divine_marriage_command__coercion_visibility_reading, theater_ratio, 20, 0.28).
narrative_ontology:measurement(divine_marriage_command__coercion_visibility_reading_tr_t30, divine_marriage_command__coercion_visibility_reading, theater_ratio, 30, 0.35).
narrative_ontology:measurement(divine_marriage_command__coercion_visibility_reading_tr_t40, divine_marriage_command__coercion_visibility_reading, theater_ratio, 40, 0.4).
narrative_ontology:measurement(divine_marriage_command__coercion_visibility_reading_tr_t50, divine_marriage_command__coercion_visibility_reading, theater_ratio, 50, 0.45).

% Extraction over time
narrative_ontology:measurement(divine_marriage_command__coercion_visibility_reading_be_t0, divine_marriage_command__coercion_visibility_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(divine_marriage_command__coercion_visibility_reading_be_t10, divine_marriage_command__coercion_visibility_reading, base_extractiveness, 10, 0.45).
narrative_ontology:measurement(divine_marriage_command__coercion_visibility_reading_be_t20, divine_marriage_command__coercion_visibility_reading, base_extractiveness, 20, 0.55).
narrative_ontology:measurement(divine_marriage_command__coercion_visibility_reading_be_t30, divine_marriage_command__coercion_visibility_reading, base_extractiveness, 30, 0.62).
narrative_ontology:measurement(divine_marriage_command__coercion_visibility_reading_be_t40, divine_marriage_command__coercion_visibility_reading, base_extractiveness, 40, 0.7).
narrative_ontology:measurement(divine_marriage_command__coercion_visibility_reading_be_t50, divine_marriage_command__coercion_visibility_reading, base_extractiveness, 50, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(divine_marriage_command__coercion_visibility_reading_su_t0, divine_marriage_command__coercion_visibility_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(divine_marriage_command__coercion_visibility_reading_su_t10, divine_marriage_command__coercion_visibility_reading, suppression_requirement, 10, 0.5).
narrative_ontology:measurement(divine_marriage_command__coercion_visibility_reading_su_t20, divine_marriage_command__coercion_visibility_reading, suppression_requirement, 20, 0.6).
narrative_ontology:measurement(divine_marriage_command__coercion_visibility_reading_su_t30, divine_marriage_command__coercion_visibility_reading, suppression_requirement, 30, 0.65).
narrative_ontology:measurement(divine_marriage_command__coercion_visibility_reading_su_t40, divine_marriage_command__coercion_visibility_reading, suppression_requirement, 40, 0.7).
narrative_ontology:measurement(divine_marriage_command__coercion_visibility_reading_su_t50, divine_marriage_command__coercion_visibility_reading, suppression_requirement, 50, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(divine_marriage_command__coercion_visibility_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(divine_marriage_command__coercion_visibility_reading, 0.1).
narrative_ontology:affects_constraint(divine_marriage_command__coercion_visibility_reading, divine_marriage_command__continuationist_reading).
narrative_ontology:affects_constraint(divine_marriage_command__coercion_visibility_reading, divine_marriage_command__substitutionist_reading).
narrative_ontology:affects_constraint(divine_marriage_command__coercion_visibility_reading, lds_temple_recommend_boundary).
narrative_ontology:affects_constraint(divine_marriage_command__coercion_visibility_reading, lds_excommunication_mechanism).
narrative_ontology:affects_constraint(divine_marriage_command__coercion_visibility_reading, utah_statehood_constitutional_convention).

% DUAL FORMULATION NOTE:
% This reading decomposes the 'divine marriage command' kernel by acknowledging the coercion-visibility axis that the continuationist reading forecloses and the substitutionist reading reinterprets. The three readings form a constraint family linked by network.affects_constraints. The coercion_visibility_reading is downstream of the historical coercion event; the continuationist_reading is upstream (claims revelatory continuity); the substitutionist_reading is parallel (claims new revelation). The ε values differ sharply: continuationist ε ≈ 0.1 (revelation = low extraction), substitutionist ε ≈ 0.3 (new revelation = moderate extraction for transition), coercion_visibility ε ≈ 0.78 (coercion-acknowledged = high extraction).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
