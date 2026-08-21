% ============================================================================
% CONSTRAINT STORY: territorial_legitimacy_dual__zionist_refuge_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_territorial_legitimacy_dual__zionist_refuge_reading, []).

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
 *   constraint_id: territorial_legitimacy_dual__zionist_refuge_reading
 *   human_readable: Zionist Refuge Reading of Israeli Territorial Legitimacy
 *   domain: political_theory/international_relations/territorial_sovereignty
 *
 * SUMMARY:
 *   This constraint story instantiates the 'Zionist Refuge' reading of
 *   Israeli territorial legitimacy. From this perspective, Israel's right to
 *   exist and control its territory is grounded in the historical persecution
 *   of Jewish people (culminating in the Holocaust), a divine promise (for
 *   some adherents), and the acceptance of the 1947 UN Partition Plan. The
 *   legitimacy of the 1948 borders is considered uncontested, while post-1967
 *   boundaries are seen as negotiable, often justified by security concerns.
 *   Palestinian displacement is framed as a consequence of Arab rejection of
 *   the partition and subsequent conflicts. This reading coordinates Jewish
 *   self-determination and security but does so through a structure that
 *   extracts from and suppresses Palestinian claims.
 *
 * KEY AGENTS:
 *   - israeli_state: Agenda setter (institutional/constrained)
 *   - zionist_movement: Beneficiary (organized/identity_locked)
 *   - israeli_citizens: Beneficiary (moderate/identity_locked)
 *   - palestinian_people: Payer (powerless/trapped)
 *   - arab_states_opposing_israel: Payer (institutional/constrained)
 *   - united_nations: Observer (institutional/analytical)
 *   - international_law_bodies: Observer (institutional/analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(territorial_legitimacy_dual__zionist_refuge_reading, 0.78).
domain_priors:suppression_score(territorial_legitimacy_dual__zionist_refuge_reading, 0.85).
domain_priors:theater_ratio(territorial_legitimacy_dual__zionist_refuge_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(territorial_legitimacy_dual__zionist_refuge_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(territorial_legitimacy_dual__zionist_refuge_reading, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(territorial_legitimacy_dual__zionist_refuge_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(territorial_legitimacy_dual__zionist_refuge_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(territorial_legitimacy_dual__zionist_refuge_reading, resistance, 0.9).

% --- Constraint claim ---
narrative_ontology:constraint_claim(territorial_legitimacy_dual__zionist_refuge_reading, tangled_rope).
narrative_ontology:human_readable(territorial_legitimacy_dual__zionist_refuge_reading, "Zionist Refuge Reading of Israeli Territorial Legitimacy").
narrative_ontology:topic_domain(territorial_legitimacy_dual__zionist_refuge_reading, "political_theory/international_relations/territorial_sovereignty").

domain_priors:requires_active_enforcement(territorial_legitimacy_dual__zionist_refuge_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(territorial_legitimacy_dual__zionist_refuge_reading, 'f4c2d58e-2abd-4d8e-a17b-8b90ce1fc487').
narrative_ontology:cs_kernel_codification('f4c2d58e-2abd-4d8e-a17b-8b90ce1fc487', formalized).
narrative_ontology:cs_authority_grounding('f4c2d58e-2abd-4d8e-a17b-8b90ce1fc487', lineage).
narrative_ontology:cs_interpretation_layer_present('f4c2d58e-2abd-4d8e-a17b-8b90ce1fc487').
narrative_ontology:cs_reading_relation('f4c2d58e-2abd-4d8e-a17b-8b90ce1fc487', territorial_legitimacy_dual__palestinian_autochthony_reading, coexists_with).
narrative_ontology:cs_reading_relation('f4c2d58e-2abd-4d8e-a17b-8b90ce1fc487', territorial_legitimacy_dual__two_state_coexistence_reading, coexists_with).
narrative_ontology:cs_axiom('f4c2d58e-2abd-4d8e-a17b-8b90ce1fc487', foundational, jewish_self_determination_in_homeland).
narrative_ontology:cs_axiom_status(jewish_self_determination_in_homeland, holdable).
narrative_ontology:cs_axiom_grounding('f4c2d58e-2abd-4d8e-a17b-8b90ce1fc487', jewish_self_determination_in_homeland, deontological).
narrative_ontology:cs_axiom('f4c2d58e-2abd-4d8e-a17b-8b90ce1fc487', foundational, security_imperative_for_territorial_control).
narrative_ontology:cs_axiom_status(security_imperative_for_territorial_control, holdable).
narrative_ontology:cs_axiom_grounding('f4c2d58e-2abd-4d8e-a17b-8b90ce1fc487', security_imperative_for_territorial_control, instrumental).
narrative_ontology:cs_reference_frame('f4c2d58e-2abd-4d8e-a17b-8b90ce1fc487', post_holocaust_zionist_imperative).
narrative_ontology:cs_drift_state('f4c2d58e-2abd-4d8e-a17b-8b90ce1fc487', contemporary_international_relations, gap(stable, minor, false)).
narrative_ontology:cs_created_at('f4c2d58e-2abd-4d8e-a17b-8b90ce1fc487', '').
narrative_ontology:cs_kernel_id(territorial_legitimacy_dual__zionist_refuge_reading, territorial_legitimacy_dual).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(territorial_legitimacy_dual__zionist_refuge_reading, israeli_state).
narrative_ontology:constraint_beneficiary(territorial_legitimacy_dual__zionist_refuge_reading, zionist_movement).
narrative_ontology:constraint_beneficiary(territorial_legitimacy_dual__zionist_refuge_reading, israeli_citizens).
narrative_ontology:constraint_victim(territorial_legitimacy_dual__zionist_refuge_reading, palestinian_people).
narrative_ontology:constraint_victim(territorial_legitimacy_dual__zionist_refuge_reading, arab_states_opposing_israel).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The sovereign entity that asserts and enforces its territorial claims based on historical, religious, and international legal grounds. It benefits from the recognition and control derived from this legitimacy framework, actively defending its borders and settlements.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__zionist_refuge_reading, israeli_state, agenda_setter,
    institutional, generational, constrained, national).

% The ideological and political movement advocating for and supporting a Jewish homeland in Israel. It benefits from the realization and perpetuation of this legitimacy claim, seeing it as the fulfillment of historical and religious aspirations.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__zionist_refuge_reading, zionist_movement, beneficiary,
    organized, generational, identity_locked, global).

% Citizens of Israel who derive national identity, security, and a sense of belonging from the state's existence and its territorial claims. They are direct beneficiaries of the stability and protection afforded by this legitimacy framework.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__zionist_refuge_reading, israeli_citizens, beneficiary,
    moderate, biographical, identity_locked, national).

% The indigenous population that experiences displacement, loss of land, and denial of self-determination as a direct consequence of the territorial claims asserted by this reading. They bear the primary costs of the constraint's operation.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__zionist_refuge_reading, palestinian_people, payer,
    powerless, generational, trapped, regional).

% Neighboring states that have historically opposed Israel's territorial claims and existence, often due to solidarity with the Palestinian cause or their own geopolitical interests. They bear political and sometimes military costs in their opposition.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__zionist_refuge_reading, arab_states_opposing_israel, payer,
    institutional, generational, constrained, regional).

% An international body that played a role in the 1947 partition plan and continues to monitor the conflict, passing resolutions and attempting to mediate. It observes the unfolding of this legitimacy claim and its consequences.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__zionist_refuge_reading, united_nations, observer,
    institutional, civilizational, analytical, universal).

% Organizations and courts that interpret and apply international law, often scrutinizing Israel's actions in the occupied territories and the legality of settlements. They provide an analytical perspective on the legitimacy claims.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__zionist_refuge_reading, international_law_bodies, observer,
    institutional, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a foundational framework for Jewish self-determination and security in a designated homeland, coordinating national identity, defense, and governance for its beneficiaries.
% TRANSFER_FUNCTION: Transfers territorial control, sovereignty, and the right to self-determination to the Israeli state and its citizens, from the Palestinian people who claim autochthonous rights to the same land.
% ABSENT_VOICES: The Palestinian people, whose narrative of continuous habitation, displacement trauma, and right of return is actively marginalized or denied within this specific legitimacy framework. Their voices would fundamentally challenge the premise of uncontested 1948 legitimacy and the framing of their displacement.
% DISAPPEARANCE_RATIONALE: If this specific reading of Israel's legitimacy vanished overnight, the entire geopolitical structure of the Middle East would be fundamentally destabilized. The Israeli state's foundational claims would be undermined, leading to a complete re-evaluation of borders, rights, and national identities, with profound regional and global consequences.
% FOUNDING_PROBLEM: The historical persecution of Jewish people, culminating in the Holocaust, and the urgent need for a secure, sovereign homeland where Jewish self-determination could be realized, free from antisemitism.
% FOUNDING_PROBLEM_CORROBORATION: Historical records of antisemitism and persecution (e.g., pogroms, Holocaust), UN General Assembly Resolution 181 (Partition Plan), and ongoing security threats (from this reading's perspective) corroborate the founding problem. International legal scholars and historians outside the immediate beneficiary groups attest to the historical context and the UN's role.
narrative_ontology:disappearance_verdict(territorial_legitimacy_dual__zionist_refuge_reading, world_rearranges).
narrative_ontology:founding_problem_status(territorial_legitimacy_dual__zionist_refuge_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(territorial_legitimacy_dual__zionist_refuge_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(territorial_legitimacy_dual__zionist_refuge_reading, 'none', 1).
narrative_ontology:epsilon_provenance(territorial_legitimacy_dual__zionist_refuge_reading, 0.78, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(territorial_legitimacy_dual__zionist_refuge_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(territorial_legitimacy_dual__zionist_refuge_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(territorial_legitimacy_dual__zionist_refuge_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness is high (0.78) because this reading's assertion of exclusive or primary territorial rights directly results in the dispossession and subjugation of the Palestinian people. Suppression is very high (0.85) due to the active military, legal, and political enforcement required to maintain control and counter resistance to these claims. Theater ratio is low (0.15) because the core claims are deeply held and genuinely believed by its proponents, with little performative maintenance; the enforcement is direct and functional. Accessibility collapse is high (0.70) from this reading's perspective, as it views its claims as foundational and largely non-negotiable, limiting alternatives for those it governs. Resistance is very high (0.90) reflecting the ongoing and intense opposition from the Palestinian people and their allies.
 *
 * PERSPECTIVAL GAP:
 *   The Israeli state and its citizens experience this constraint as a legitimate and necessary framework for their security and national identity, providing coordination and protection. In contrast, the Palestinian people experience the same structure as a profound act of extraction and suppression, denying their fundamental rights and self-determination. The engine will compute these divergent classifications from the declared structural relationships and metrics.
 *
 * DIRECTIONALITY LOGIC:
 *   The Israeli state, the Zionist movement, and Israeli citizens are beneficiaries (low directionality) as they gain territorial control, security, and national identity. The Palestinian people and Arab states opposing Israel are targets (high directionality) as they bear the costs of displacement, loss of land, and denial of self-determination. The UN and international law bodies act as observers, analyzing the claims and their consequences.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is not subject to mandatrophy in the traditional sense, as its core mandate (Jewish self-determination and security) is considered 'live' by its proponents. However, the contestation around the 'founding_problem_status' (live vs. solved/shifted) highlights a potential for the coordination function to be perceived as having atrophied for some, while the extractive elements persist. The high extractiveness and suppression, coupled with the 'live' founding problem status, prevent mislabeling it as a Piton; it is an actively maintained and highly contested Tangled Rope.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    divine_promise_empirical_status,
    'To what extent does the ''divine promise'' aspect of legitimacy rely on empirically verifiable claims versus theological belief, and how does this affect its contestability?',
    'Conceptual analysis of theological claims and their role in political legitimacy, distinguishing between faith-based and secular arguments for territorial rights.',
    'If primarily theological, the claim is less susceptible to empirical challenge but may lack universal appeal for secular international bodies. If framed with empirical components, it becomes vulnerable to counter-evidence, potentially weakening the overall legitimacy claim.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(divine_promise_empirical_status, conceptual, 'The role of divine promise in territorial legitimacy.').

omega_variable(
    historical_persecution_vs_contemporary_displacement,
    'How does the historical persecution of Jewish people, central to this reading, weigh against the contemporary and ongoing displacement and suffering of the Palestinian people in assessing overall justice and legitimacy?',
    'A framework for inter-generational justice that can simultaneously acknowledge and address historical traumas of multiple groups without negating the claims of others.',
    'If a framework for simultaneous acknowledgment is adopted, the perceived legitimacy of this reading might be reduced, requiring greater accommodation of Palestinian claims. If not, the conflict of narratives persists, fueling ongoing extraction and resistance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(historical_persecution_vs_contemporary_displacement, preference, 'Balancing historical Jewish persecution with contemporary Palestinian displacement.').

omega_variable(
    un_partition_acceptance_vs_territorial_expansion,
    'Does the initial acceptance of the UN Partition Plan (1947) by the Zionist leadership, as a basis for legitimacy, remain consistent with subsequent territorial expansion beyond those proposed borders, particularly after 1967?',
    'Legal and historical analysis of UN resolutions, international law on occupation, and the evolution of Israeli state policy regarding borders and settlements.',
    'If inconsistencies are found, the ''UN partition acceptance'' leg of this reading''s legitimacy may be weakened, potentially shifting international legal and diplomatic support. If consistent, it reinforces the claim of security-driven, legitimate expansion.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(un_partition_acceptance_vs_territorial_expansion, empirical, 'Consistency of UN partition acceptance with later territorial changes.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression of Palestinian agency and claims primarily structural (external barriers, military occupation, legal restrictions) or internalized (cognitive patterns, identity fusion, despair)?',
    'Post-exit suppression trajectory: if suppression persists after the extractive mechanisms (e.g., occupation, legal discrimination) are removed, reclassify as partially internalized. This would require a hypothetical scenario or a significant policy shift.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests — the target carries the suppression with them after structural barriers are removed, making resolution more complex. If purely structural, removing external barriers would be sufficient.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for Palestinian people.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(territorial_legitimacy_dual__zionist_refuge_reading, 1948, 2023).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(terr_tr_t1948, territorial_legitimacy_dual__zionist_refuge_reading, theater_ratio, 1948, 0.1).
narrative_ontology:measurement(terr_tr_t1967, territorial_legitimacy_dual__zionist_refuge_reading, theater_ratio, 1967, 0.12).
narrative_ontology:measurement(terr_tr_t1987, territorial_legitimacy_dual__zionist_refuge_reading, theater_ratio, 1987, 0.14).
narrative_ontology:measurement(terr_tr_t2000, territorial_legitimacy_dual__zionist_refuge_reading, theater_ratio, 2000, 0.15).
narrative_ontology:measurement(terr_tr_t2010, territorial_legitimacy_dual__zionist_refuge_reading, theater_ratio, 2010, 0.15).
narrative_ontology:measurement(terr_tr_t2023, territorial_legitimacy_dual__zionist_refuge_reading, theater_ratio, 2023, 0.15).

% Extraction over time
narrative_ontology:measurement(terr_be_t1948, territorial_legitimacy_dual__zionist_refuge_reading, base_extractiveness, 1948, 0.6).
narrative_ontology:measurement(terr_be_t1967, territorial_legitimacy_dual__zionist_refuge_reading, base_extractiveness, 1967, 0.7).
narrative_ontology:measurement(terr_be_t1987, territorial_legitimacy_dual__zionist_refuge_reading, base_extractiveness, 1987, 0.75).
narrative_ontology:measurement(terr_be_t2000, territorial_legitimacy_dual__zionist_refuge_reading, base_extractiveness, 2000, 0.77).
narrative_ontology:measurement(terr_be_t2010, territorial_legitimacy_dual__zionist_refuge_reading, base_extractiveness, 2010, 0.78).
narrative_ontology:measurement(terr_be_t2023, territorial_legitimacy_dual__zionist_refuge_reading, base_extractiveness, 2023, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(terr_su_t1948, territorial_legitimacy_dual__zionist_refuge_reading, suppression_requirement, 1948, 0.7).
narrative_ontology:measurement(terr_su_t1967, territorial_legitimacy_dual__zionist_refuge_reading, suppression_requirement, 1967, 0.8).
narrative_ontology:measurement(terr_su_t1987, territorial_legitimacy_dual__zionist_refuge_reading, suppression_requirement, 1987, 0.83).
narrative_ontology:measurement(terr_su_t2000, territorial_legitimacy_dual__zionist_refuge_reading, suppression_requirement, 2000, 0.84).
narrative_ontology:measurement(terr_su_t2010, territorial_legitimacy_dual__zionist_refuge_reading, suppression_requirement, 2010, 0.85).
narrative_ontology:measurement(terr_su_t2023, territorial_legitimacy_dual__zionist_refuge_reading, suppression_requirement, 2023, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(territorial_legitimacy_dual__zionist_refuge_reading, identity_coordination).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
