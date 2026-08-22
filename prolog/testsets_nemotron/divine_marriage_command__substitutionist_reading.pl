% ============================================================================
% CONSTRAINT STORY: divine_marriage_command__substitutionist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-27
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_divine_marriage_command__substitutionist_reading, []).

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
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: divine_marriage_command__substitutionist_reading
 *   human_readable: Divine Marriage Command — Substitutionist Reading (Monogamy as New Revelation)
 *   domain: religious_authority/commitment_systems/political_theology
 *
 * SUMMARY:
 *   The 1890 Manifesto (Official Declaration 1) announced the end of plural
 *   marriage in the LDS Church. The substitutionist reading holds that this
 *   was a genuine new revelation — God changed the command — making monogamy
 *   now doctrinally required and polygamy apostasy. This reading is the
 *   official institutional position. It coordinates the institution's
 *   survival and integration with the nation-state, but it extracts from
 *   fundamentalist families who are excommunicated and lose sealing
 *   ordinances, community, and generational continuity. The constraint
 *   requires active enforcement (temple recommend discipline,
 *   excommunication, public disavowal) and has no sunset clause. The
 *   claimed_type is tangled_rope: genuine coordination (institutional
 *   survival, state accommodation, mainstream membership stability) fused
 *   with asymmetric extraction (fundamentalists bear the cost of the
 *   transition).
 *
 * KEY AGENTS:
 *   - institutional_leadership: agenda_setter (institutional/arbitrage) — sets doctrine, controls temple access, manages state relations
 *   - mainstream_membership: beneficiary (organized/constrained) — gains state tolerance, cultural normalization, institutional stability
 *   - state_actors: beneficiary (institutional/analytical) — gains a Mormonism that fits monogamous legal order
 *   - fundamentalist_families: victim (powerless/trapped) — excommunicated, lose sealings and community, no exit that preserves identity
 *   - excommunicated_adherents: victim (powerless/identity_locked) — identity fused to the kernel's original form; exit means apostasy
 *   - analytical_observer: observer (analytical/analytical) — sees the full structure
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(divine_marriage_command__substitutionist_reading, 0.52).
domain_priors:suppression_score(divine_marriage_command__substitutionist_reading, 0.45).
domain_priors:theater_ratio(divine_marriage_command__substitutionist_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(divine_marriage_command__substitutionist_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(divine_marriage_command__substitutionist_reading, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(divine_marriage_command__substitutionist_reading, theater_ratio, 0.35).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(divine_marriage_command__substitutionist_reading, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(divine_marriage_command__substitutionist_reading, resistance, 0.25).

% --- Constraint claim ---
narrative_ontology:constraint_claim(divine_marriage_command__substitutionist_reading, tangled_rope).
narrative_ontology:human_readable(divine_marriage_command__substitutionist_reading, "Divine Marriage Command — Substitutionist Reading (Monogamy as New Revelation)").
narrative_ontology:topic_domain(divine_marriage_command__substitutionist_reading, "religious_authority/commitment_systems/political_theology").

domain_priors:requires_active_enforcement(divine_marriage_command__substitutionist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(divine_marriage_command__substitutionist_reading, 'bcef5b10-25a5-4d29-9437-fc9cfa027d95').
narrative_ontology:cs_kernel_codification('bcef5b10-25a5-4d29-9437-fc9cfa027d95', formalized).
narrative_ontology:cs_authority_grounding('bcef5b10-25a5-4d29-9437-fc9cfa027d95', lineage).
narrative_ontology:cs_interpretation_layer_present('bcef5b10-25a5-4d29-9437-fc9cfa027d95').
narrative_ontology:cs_reading_relation('bcef5b10-25a5-4d29-9437-fc9cfa027d95', divine_marriage_command__continuationist_reading, influences).
narrative_ontology:cs_reading_relation('bcef5b10-25a5-4d29-9437-fc9cfa027d95', divine_marriage_command__coercion_visibility_reading, forecloses).
narrative_ontology:cs_axiom('bcef5b10-25a5-4d29-9437-fc9cfa027d95', foundational, manifesto_as_new_revelation).
narrative_ontology:cs_axiom_status(manifesto_as_new_revelation, holdable).
narrative_ontology:cs_axiom_grounding('bcef5b10-25a5-4d29-9437-fc9cfa027d95', manifesto_as_new_revelation, deontological).
narrative_ontology:cs_axiom('bcef5b10-25a5-4d29-9437-fc9cfa027d95', foundational, polygamy_as_apostasy_post_1890).
narrative_ontology:cs_axiom_status(polygamy_as_apostasy_post_1890, holdable).
narrative_ontology:cs_axiom_grounding('bcef5b10-25a5-4d29-9437-fc9cfa027d95', polygamy_as_apostasy_post_1890, deontological).
narrative_ontology:cs_reference_frame('bcef5b10-25a5-4d29-9437-fc9cfa027d95', prophetic_continuity_through_revelation).
narrative_ontology:cs_drift_state('bcef5b10-25a5-4d29-9437-fc9cfa027d95', contemporary_institutional_orthodoxy, gap(stable, minor, false)).
narrative_ontology:cs_created_at('bcef5b10-25a5-4d29-9437-fc9cfa027d95', '').
narrative_ontology:cs_kernel_id(divine_marriage_command__substitutionist_reading, divine_marriage_command).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(divine_marriage_command__substitutionist_reading, institutional_leadership).
narrative_ontology:constraint_beneficiary(divine_marriage_command__substitutionist_reading, mainstream_membership).
narrative_ontology:constraint_beneficiary(divine_marriage_command__substitutionist_reading, state_actors).
narrative_ontology:constraint_victim(divine_marriage_command__substitutionist_reading, fundamentalist_families).
narrative_ontology:constraint_victim(divine_marriage_command__substitutionist_reading, excommunicated_adherents).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The First Presidency and Quorum of the Twelve Apostles. They declare doctrine, control temple access, manage state relations, and define orthodoxy. They authored the 1890 Manifesto and 1904 Second Manifesto as revelation. They face no meaningful exit — reversing the Manifesto would collapse institutional legitimacy, state standing, and mainstream coherence. They collect the gains of state tolerance, property retention, and global growth.
narrative_ontology:constraint_stakeholder(divine_marriage_command__substitutionist_reading, institutional_leadership, agenda_setter,
    institutional, generational, arbitrage, global).

% The body of LDS members who accept the Manifesto as revelation. They gain cultural normalization, citizenship without conflict, temple access, and community stability. Their exit is constrained — leaving means losing community, family sealing, and identity — but they do not experience the constraint as extraction. They benefit from the coordination function.
narrative_ontology:constraint_stakeholder(divine_marriage_command__substitutionist_reading, mainstream_membership, beneficiary,
    organized, biographical, constrained, global).

% Federal and state governments (1890s onward). They gained a Mormonism compatible with monogamous marriage law, enabling Utah statehood (1896) and ending the 'Mormon question' as a political crisis. They neither pay nor are coerced by this constraint; they are external beneficiaries of its coordination function.
narrative_ontology:constraint_stakeholder(divine_marriage_command__substitutionist_reading, state_actors, beneficiary,
    institutional, generational, analytical, national).

% Families and communities continuing plural marriage after 1890. They are excommunicated, lose temple sealings (eternal family bonds), lose welfare access, face legal prosecution, and are socially marginalized. Their exit options are physical relocation to isolated enclaves (which they did) — but they cannot exit the constraint's definition of them as apostates without abandoning their identity-fused commitment to the original revelation. They bear the extraction.
narrative_ontology:constraint_stakeholder(divine_marriage_command__substitutionist_reading, fundamentalist_families, payer,
    powerless, biographical, trapped, local).

% Individuals who accepted the substitutionist reading's authority but were disciplined for fundamentalist sympathies or associations. Their identity is fused to the institution's claim to exclusive priesthood keys; excommunication severs their eternal sealing and cosmological standing. They cannot 'leave and be fine' — the constraint defines their eternal destiny. They bear extraction without the communal buffer of fundamentalist enclaves.
narrative_ontology:constraint_stakeholder(divine_marriage_command__substitutionist_reading, excommunicated_adherents, payer,
    powerless, biographical, identity_locked, local).

% Historians, theologians, political theorists analyzing the constraint from outside. They see the full structure: the kernel dispute, the three readings, the extraction from fundamentalists, the coordination for the mainstream. They neither pay nor collect.
narrative_ontology:constraint_stakeholder(divine_marriage_command__substitutionist_reading, analytical_observer, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the LDS Church's institutional survival and integration with the American nation-state by resolving the federal 'Mormon question' through a doctrinal shift that enables statehood, property retention, and global missionary expansion under monogamous legal order.
% TRANSFER_FUNCTION: Moves eternal sealing ordinances, temple access, community membership, welfare resources, and generational continuity FROM fundamentalist families and excommunicated adherents TO the institutional center (which converts them into state tolerance, mainstream growth, property security, and doctrinal coherence).
% ABSENT_VOICES: Fundamentalist families and excommunicated adherents are structurally excluded from the institutional conversation — they are not consulted on the Manifesto's meaning, and their objection is defined as apostasy. The coercion_visibility_reading is also excluded from official discourse; acknowledging federal coercion as the Manifesto's driver would collapse the revelation frame.
% DISAPPEARANCE_RATIONALE: If the substitutionist reading vanished overnight, the LDS Church would lose its official doctrinal basis for monogamy, triggering immediate legitimacy crisis: fundamentalists would claim vindication, temple recommend standards would collapse, state accommodation could be questioned, and the global institution would face schism or reversion. The world rearranges because the constraint IS the institution's current self-understanding.
% FOUNDING_PROBLEM: The federal government's escalating coercion (Edmunds Act 1882, Edmunds-Tucker Act 1887) threatened the institutional survival of the LDS Church: disincorporation, seizure of temples, imprisonment of leadership, destruction of the priesthood hierarchy. The Manifesto was issued to preserve the institution.
% FOUNDING_PROBLEM_CORROBORATION: The federal threat ended with Utah statehood (1896) and the seating of Reed Smoot (1907) — attested by congressional records, not by the institution. The institution's own leadership (1904 Second Manifesto, 1910+ excommunications) treats the constraint as doctrinally permanent, not provisional. No corroborating source outside the beneficiary set attests the founding problem is still live; the institution itself has shifted to a revelation-based justification.
narrative_ontology:disappearance_verdict(divine_marriage_command__substitutionist_reading, world_rearranges).
narrative_ontology:founding_problem_status(divine_marriage_command__substitutionist_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(divine_marriage_command__substitutionist_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(divine_marriage_command__substitutionist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(divine_marriage_command__substitutionist_reading, 0.52, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(divine_marriage_command__substitutionist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(divine_marriage_command__substitutionist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(divine_marriage_command__substitutionist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52) reflects the transfer: fundamentalists lose eternal sealings, community, generational transmission; the institution gains state tolerance, property retention, mainstream growth. Suppression (0.45) is moderate — enforcement is real (excommunication, temple recommend denial) but not totalitarian; fundamentalists can physically leave but cannot exit without identity rupture. Theater ratio (0.35) captures that the revelation framing performs doctrinal continuity while the functional work is institutional survival. Accessibility collapse (0.78) is high: once the Manifesto is accepted as revelation, the prior command is not just suspended but doctrinally impossible — alternatives collapse. Resistance (0.25) is low from the mainstream (who accept the revelation) but high from fundamentalists (who are structurally excluded from the conversation).
 *
 * DIRECTIONALITY LOGIC:
 *   Institutional leadership (agenda_setter) sits near the beneficiary end: they control the constraint, gain legitimacy, and face arbitrage-grade exit (they could theoretically reverse but won't). Mainstream membership (beneficiary) is symmetric-to-beneficiary: they gain normalization, lose little. State actors (beneficiary) are full beneficiaries: they get a compliant religion. Fundamentalist families (victim) are full targets: identity-locked, trapped, bear the extraction. Excommunicated adherents (victim) are identity-locked targets: their self-concept is constituted through the original kernel; exit is unthinkable. The engine derives directionality from these declarations.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (federal destruction of the institution) is dead — the federal threat ended by 1896 statehood. Yet the constraint persists and hardens (Second Manifesto 1904, priesthood/temple discipline). The arrangement has outlived its founding problem: it now coordinates mainstream identity and state integration, but the extraction from fundamentalists continues without the original justification. This is mandatrophy — the constraint's mandate has atrophied into a new function.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_dispute,
    'Is the 1890 Manifesto a genuine new revelation superseding the prior command, or a prudential suspension under federal coercion?',
    'Textual-historical analysis of the Manifesto''s language and subsequent authoritative interpretations; the framing chosen by the institutional leadership at each subsequent crisis point.',
    'If substitutionist: polygamy is doctrinally dead, fundamentalists are apostates, institutional legitimacy is coherent. If continuationist: polygamy remains valid but suspended, fundamentalists are the true heirs, the institution is in schism from its own kernel.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_dispute, conceptual, 'The committer-frame dispute: whether this reading (substitutionist) or its sibling (continuationist) correctly reads the kernel''s drift.').

omega_variable(
    revelation_vs_coercion_framing,
    'Does the institutional legitimacy of the substitutionist reading require the Manifesto to be framed as revelation rather than as a response to federal coercion?',
    'Institutional rhetoric at each legitimacy crisis (1904 Second Manifesto, post-1910 excommunications, modern priesthood/temple recommend interviews): does leadership explicitly disavow the coercion_visibility_reading''s framing?',
    'If legitimacy requires the revelation frame, the coercion_visibility_reading is not merely an alternative reading but an existential threat to the institution''s self-understanding — making the exclusion of that reading a structural necessity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(revelation_vs_coercion_framing, conceptual, 'Whether the substitutionist reading''s legitimacy structurally depends on foreclosing the coercion_visibility_reading.').

omega_variable(
    extraction_from_excommunication,
    'Does the excommunication of fundamentalist families constitute extraction (transfer of status, community, eternal sealing from victims to institutional cohesion) or a genuine coordination cost of maintaining doctrinal unity?',
    'Comparative analysis: do excommunicated families lose sealing ordinances and community while the institution gains measurable cohesion/funding/state tolerance? Track asset flows (temple access, welfare resources, generational retention) across the boundary.',
    'If extraction: this reading operates as a tangled_rope with real victims. If coordination cost: the constraint may be a rope from all seats, and the victim declaration is a category error.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_from_excommunication, empirical, 'Whether the enforcement mechanism (excommunication) transfers value from fundamentalists to the institutional center.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(divine_marriage_command__substitutionist_reading, 1890, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(divine_marriage_command_substitutionist_reading_tr_t1890, divine_marriage_command__substitutionist_reading, theater_ratio, 1890, 0.2).
narrative_ontology:measurement(divine_marriage_command_substitutionist_reading_tr_t1904, divine_marriage_command__substitutionist_reading, theater_ratio, 1904, 0.28).
narrative_ontology:measurement(divine_marriage_command_substitutionist_reading_tr_t1910, divine_marriage_command__substitutionist_reading, theater_ratio, 1910, 0.32).
narrative_ontology:measurement(divine_marriage_command_substitutionist_reading_tr_t1950, divine_marriage_command__substitutionist_reading, theater_ratio, 1950, 0.34).
narrative_ontology:measurement(divine_marriage_command_substitutionist_reading_tr_t1990, divine_marriage_command__substitutionist_reading, theater_ratio, 1990, 0.35).
narrative_ontology:measurement(divine_marriage_command_substitutionist_reading_tr_t2024, divine_marriage_command__substitutionist_reading, theater_ratio, 2024, 0.35).

% Extraction over time
narrative_ontology:measurement(divine_marriage_command_substitutionist_reading_be_t1890, divine_marriage_command__substitutionist_reading, base_extractiveness, 1890, 0.35).
narrative_ontology:measurement(divine_marriage_command_substitutionist_reading_be_t1904, divine_marriage_command__substitutionist_reading, base_extractiveness, 1904, 0.42).
narrative_ontology:measurement(divine_marriage_command_substitutionist_reading_be_t1910, divine_marriage_command__substitutionist_reading, base_extractiveness, 1910, 0.48).
narrative_ontology:measurement(divine_marriage_command_substitutionist_reading_be_t1950, divine_marriage_command__substitutionist_reading, base_extractiveness, 1950, 0.5).
narrative_ontology:measurement(divine_marriage_command_substitutionist_reading_be_t1990, divine_marriage_command__substitutionist_reading, base_extractiveness, 1990, 0.51).
narrative_ontology:measurement(divine_marriage_command_substitutionist_reading_be_t2024, divine_marriage_command__substitutionist_reading, base_extractiveness, 2024, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(divine_marriage_command_substitutionist_reading_su_t1890, divine_marriage_command__substitutionist_reading, suppression_requirement, 1890, 0.3).
narrative_ontology:measurement(divine_marriage_command_substitutionist_reading_su_t1904, divine_marriage_command__substitutionist_reading, suppression_requirement, 1904, 0.38).
narrative_ontology:measurement(divine_marriage_command_substitutionist_reading_su_t1910, divine_marriage_command__substitutionist_reading, suppression_requirement, 1910, 0.44).
narrative_ontology:measurement(divine_marriage_command_substitutionist_reading_su_t1950, divine_marriage_command__substitutionist_reading, suppression_requirement, 1950, 0.45).
narrative_ontology:measurement(divine_marriage_command_substitutionist_reading_su_t1990, divine_marriage_command__substitutionist_reading, suppression_requirement, 1990, 0.45).
narrative_ontology:measurement(divine_marriage_command_substitutionist_reading_su_t2024, divine_marriage_command__substitutionist_reading, suppression_requirement, 2024, 0.45).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(divine_marriage_command__substitutionist_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(divine_marriage_command__substitutionist_reading, 0.08).
narrative_ontology:affects_constraint(divine_marriage_command__substitutionist_reading, divine_marriage_command__continuationist_reading).
narrative_ontology:affects_constraint(divine_marriage_command__substitutionist_reading, divine_marriage_command__coercion_visibility_reading).
narrative_ontology:affects_constraint(divine_marriage_command__substitutionist_reading, lds_temple_recommend_discipline).
narrative_ontology:affects_constraint(divine_marriage_command__substitutionist_reading, lds_fundamentalist_exclusion).

% DUAL FORMULATION NOTE:
% Constraint family: divine_marriage_command kernel with three readings. This reading (substitutionist) declares monogamy as new revelation; continuationist declares polygamy still valid but suspended; coercion_visibility declares the Manifesto as survival-driven. Each has distinct ε, victims, and legitimacy logic. The substitutionist reading influences the continuationist (makes it a schismatic position) and forecloses the coercion_visibility reading within the institutional framework (the institution cannot officially acknowledge coercion without collapsing its revelation claim).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(divine_marriage_command__substitutionist_reading, institutional, 0.15).
constraint_indexing:directionality_override(divine_marriage_command__substitutionist_reading, powerless, 0.95).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
