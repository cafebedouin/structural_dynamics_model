% ============================================================================
% CONSTRAINT STORY: quran_ontological_status__uncreated_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_quran_ontological_status__uncreated_reading, []).

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
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: quran_ontological_status__uncreated_reading
 *   human_readable: Qur'an as Uncreated Eternal Divine Speech (Uncreated Reading)
 *   domain: theological/philosophical/political
 *
 * SUMMARY:
 *   The uncreated reading (kalām Allāh qadīm) asserts that the Qur'an is
 *   God's eternal, uncreated speech — coeternal with His essence. This
 *   doctrine, crystallized in the post-mihna Sunni consensus
 *   (Ash'ari/Maturidi), functions as a mountain constraint: it presents
 *   itself as a natural theological fact, beyond historical contingency. Yet
 *   it generates a clear beneficiary/victim structure: traditional jurists,
 *   literalist communities, and anti-rationalist schools gain epistemic
 *   authority and institutional control; rational theologians, metaphorical
 *   interpreters, and reform movements pay through marginalization, heresy
 *   accusations, and blocked interpretive pathways. The constraint requires
 *   active enforcement (mihna, creedal tests, state patronage of orthodoxy)
 *   and its theater ratio rises in modernity as literalist performance
 *   intensifies amid challenges. The claimed mountain type diverges from the
 *   metric profile (extraction 0.65, suppression 0.75) — this divergence is
 *   the false summit signature.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(quran_ontological_status__uncreated_reading, 0.65).
domain_priors:suppression_score(quran_ontological_status__uncreated_reading, 0.75).
domain_priors:theater_ratio(quran_ontological_status__uncreated_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(quran_ontological_status__uncreated_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(quran_ontological_status__uncreated_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(quran_ontological_status__uncreated_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(quran_ontological_status__uncreated_reading, accessibility_collapse, 0.88).
narrative_ontology:constraint_metric(quran_ontological_status__uncreated_reading, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(quran_ontological_status__uncreated_reading, mountain).
narrative_ontology:human_readable(quran_ontological_status__uncreated_reading, "Qur'an as Uncreated Eternal Divine Speech (Uncreated Reading)").
narrative_ontology:topic_domain(quran_ontological_status__uncreated_reading, "theological/philosophical/political").

domain_priors:requires_active_enforcement(quran_ontological_status__uncreated_reading).
domain_priors:emerges_naturally(quran_ontological_status__uncreated_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(quran_ontological_status__uncreated_reading, 'd37208ec-37ea-4012-bc0a-76b3037513fb').
narrative_ontology:cs_kernel_codification('d37208ec-37ea-4012-bc0a-76b3037513fb', formalized).
narrative_ontology:cs_authority_grounding('d37208ec-37ea-4012-bc0a-76b3037513fb', lineage).
narrative_ontology:cs_interpretation_layer_present('d37208ec-37ea-4012-bc0a-76b3037513fb').
narrative_ontology:cs_reading_relation('d37208ec-37ea-4012-bc0a-76b3037513fb', quran_ontological_status__created_reading, forecloses).
narrative_ontology:cs_reading_relation('d37208ec-37ea-4012-bc0a-76b3037513fb', quran_ontological_status__state_enforced_creation_reading, forecloses).
narrative_ontology:cs_axiom('d37208ec-37ea-4012-bc0a-76b3037513fb', foundational, quran_is_eternal_uncreated_speech).
narrative_ontology:cs_axiom_status(quran_is_eternal_uncreated_speech, holdable).
narrative_ontology:cs_axiom_grounding('d37208ec-37ea-4012-bc0a-76b3037513fb', quran_is_eternal_uncreated_speech, theological).
narrative_ontology:cs_axiom('d37208ec-37ea-4012-bc0a-76b3037513fb', foundational, textual_meaning_is_fixed_divine_fact).
narrative_ontology:cs_axiom_status(textual_meaning_is_fixed_divine_fact, holdable).
narrative_ontology:cs_axiom_grounding('d37208ec-37ea-4012-bc0a-76b3037513fb', textual_meaning_is_fixed_divine_fact, theological).
narrative_ontology:cs_reference_frame('d37208ec-37ea-4012-bc0a-76b3037513fb', eternal_uncreated_quran_framework).
narrative_ontology:cs_drift_state('d37208ec-37ea-4012-bc0a-76b3037513fb', contemporary_reform_challenge, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('d37208ec-37ea-4012-bc0a-76b3037513fb', '').
narrative_ontology:cs_kernel_id(quran_ontological_status__uncreated_reading, quran_ontological_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(quran_ontological_status__uncreated_reading, traditional_jurists).
narrative_ontology:constraint_beneficiary(quran_ontological_status__uncreated_reading, literalist_communities).
narrative_ontology:constraint_beneficiary(quran_ontological_status__uncreated_reading, anti_rationalist_schools).
narrative_ontology:constraint_victim(quran_ontological_status__uncreated_reading, rational_theologians).
narrative_ontology:constraint_victim(quran_ontological_status__uncreated_reading, metaphorical_interpreters).
narrative_ontology:constraint_victim(quran_ontological_status__uncreated_reading, reform_movements).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(quran_ontological_status__uncreated_reading, caliphate_state).
narrative_ontology:constraint_vindicates(quran_ontological_status__uncreated_reading, divine_speech_is_eternal).
narrative_ontology:constraint_vindicates(quran_ontological_status__uncreated_reading, quran_is_uncreated).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Classical fuqaha and ulema whose interpretive authority derives from the Qur'an's fixed, eternal status. They administer the legal-theological framework built on textual inerrancy. Exit would mean abandoning the epistemic foundation of their professional identity and institutional role.
narrative_ontology:constraint_stakeholder(quran_ontological_status__uncreated_reading, traditional_jurists, beneficiary,
    institutional, generational, identity_locked, global).

% Communities (e.g., Salafi, Athari, traditionalist madhhab followers) for whom the uncreated Qur'an is the non-negotiable anchor of faith and practice. The constraint secures their hermeneutic certainty and communal cohesion. Exit is experienced as apostasy or epistemic collapse.
narrative_ontology:constraint_stakeholder(quran_ontological_status__uncreated_reading, literalist_communities, beneficiary,
    organized, biographical, identity_locked, global).

% Ash'ari and Maturidi theological establishments that won the classical controversy over the Qur'an's createdness. Their doctrinal victory is enshrined in creeds and curricula. They benefit from the constraint's stabilization of orthodoxy. Exit would require dismantling centuries of scholastic architecture.
narrative_ontology:constraint_stakeholder(quran_ontological_status__uncreated_reading, anti_rationalist_schools, beneficiary,
    institutional, generational, constrained, global).

% Mu'tazilite and later rationalist theologians (and their modern heirs) who argue for the Qur'an's createdness to preserve divine transcendence. They bear the cost of marginalization, heresy accusations, and exclusion from mainstream institutions. Exit means leaving the Islamic theological conversation entirely or accepting fringe status.
narrative_ontology:constraint_stakeholder(quran_ontological_status__uncreated_reading, rational_theologians, payer,
    moderate, biographical, constrained, global).

% Sufi, philosophical, and esoteric interpreters (e.g., Ibn 'Arabi, Rumi, later modernist exegetes) who read the Qur'an symbolically or contextually. The uncreated reading delegitimizes their hermeneutic as subjective innovation. They pay in epistemic suspicion and institutional barriers.
narrative_ontology:constraint_stakeholder(quran_ontological_status__uncreated_reading, metaphorical_interpreters, payer,
    moderate, biographical, constrained, global).

% Modernist and reformist movements (19th century to present) seeking textual flexibility for gender equality, human rights, legal reform, etc. The mountain constraint blocks historicist or contextual readings. They bear the cost of being labeled neo-Mu'tazilite or heretical.
narrative_ontology:constraint_stakeholder(quran_ontological_status__uncreated_reading, reform_movements, payer,
    moderate, biographical, constrained, global).

% The political authority (historically Abbasid post-mihna, Ottoman, modern nation-states) that enforces the uncreated reading as state orthodoxy. It gains legitimacy from upholding the 'true' doctrine and uses the constraint to regulate religious discourse. It can shift enforcement intensity but cannot abandon the doctrine without losing religious legitimacy.
narrative_ontology:constraint_stakeholder(quran_ontological_status__uncreated_reading, caliphate_state, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(quran_ontological_status__uncreated_reading, caliphate_state, beneficiary).

% Scholars of Islamic theology, history, and philosophy of language who analyze the constraint from outside the commitment. They see the structural dynamics of orthodoxy formation, enforcement, and contestation without being bound by the constraint's truth claims.
narrative_ontology:constraint_stakeholder(quran_ontological_status__uncreated_reading, analytical_observer, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a fixed, authoritative textual anchor for Islamic law, theology, and communal identity, solving the coordination problem of divergent interpretations and stabilizing the epistemic hierarchy of the scholarly class.
% TRANSFER_FUNCTION: Moves interpretive authority and definitional power from rational/theological dissenters to traditional jurists and literalist communities, cementing a hierarchical epistemic structure where the text's meaning is closed and the interpreter's role is transmission not construction.
% ABSENT_VOICES: Rational theologians (Mu'tazilites) were historically excluded via the mihna (inquisition) and subsequent creedal enforcement; contemporary reformist voices (feminist, liberal, historicist) are structurally excluded from classical scholarly institutions and state-religious apparatuses that gatekeep orthodoxy.
% DISAPPEARANCE_RATIONALE: The uncreated reading underpins the classical schema of textual inerrancy, prophetic authority, and juristic methodology (usul al-fiqh). Its removal would collapse the epistemic foundation of traditional fiqh and kalam, forcing a reorganization of Islamic legal and theological authority around createdness, historicism, or metaphorical readings.
% FOUNDING_PROBLEM: The early Muslim community faced divergent readings of the Qur'an and theological disputes about God's attributes (especially speech). The uncreated reading settled the ontological status of revelation to stabilize law, creed, and communal boundaries against rationalist challenges that threatened to make theology subordinate to human reason.
% FOUNDING_PROBLEM_CORROBORATION: Classical Ash'ari and Maturidi theologians (e.g., al-Ghazali, al-Baqillani) attest the problem is live — the Qur'an's eternity is necessary to protect divine attributes. Modern historians (e.g., Wim Raven, Sabine Schmidtke) and reformist scholars (Fazlur Rahman, Nasr Abu Zayd, Abdullahi An-Na'im) attest the founding problem is dead — the historical context of Greek rationalist challenge has passed, and the constraint now functions as a barrier to reform. The dispute is ongoing in theological seminaries and intellectual circles.
narrative_ontology:disappearance_verdict(quran_ontological_status__uncreated_reading, world_rearranges).
narrative_ontology:founding_problem_status(quran_ontological_status__uncreated_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(quran_ontological_status__uncreated_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(quran_ontological_status__uncreated_reading, 'none', 1).
narrative_ontology:epsilon_provenance(quran_ontological_status__uncreated_reading, 0.65, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(quran_ontological_status__uncreated_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(quran_ontological_status__uncreated_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(quran_ontological_status__uncreated_reading, ExtMetricName, E),
    domain_priors:suppression_score(quran_ontological_status__uncreated_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(quran_ontological_status__uncreated_reading),
    narrative_ontology:constraint_metric(quran_ontological_status__uncreated_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(quran_ontological_status__uncreated_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(quran_ontological_status__uncreated_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high because the constraint transfers interpretive monopoly to a specific scholarly class and closes the textual field to historicist or metaphorical readings. Suppression is high because persistence depends on active exclusion of alternatives (mihna historically; blasphemy laws, academic gatekeeping, social ostracism currently). Theater ratio rises over time: early enforcement was substantive (state inquisition), later maintenance becomes performative (ritualistic affirmation of creeds, ceremonial book-burnings, symbolic fatwas) as the constraint's functional necessity wanes but its identity-marking function intensifies. Accessibility collapse is near-total within the constraint's own framework — once you accept the premise, alternatives are not just wrong but incomprehensible (kufr). Resistance is moderate: the constraint faced the mihna (resistance from state power) and continuous intellectual dissent, but within the Sunni mainstream it meets little resistance after 10th century.
 *
 * PERSPECTIVAL GAP:
 *   From the beneficiary seats, the constraint is a mountain: it feels like the natural order of reality, the very condition of Islamic truth. From the victim seats, it is a snare: an enforced closure that extracts their intellectual labor and existential legitimacy. From the agenda_setter seat, it is a tangled rope: it coordinates communal unity and state legitimacy but requires costly enforcement and suppresses dissent that could be socially productive. The engine computes these per-seat types from the structural data; the authored claim (mountain) reflects only the beneficiary/agenda_setter phenomenology.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (traditional jurists, literalists, anti-rationalist schools) are identity_locked: their professional, communal, and doctrinal selves are constituted by the constraint. Exit is not a live option without self-dissolution. Victims (rational theologians, metaphorical interpreters, reformers) are constrained: they can operate at margins, in exile, or in segregated spaces, but cannot access mainstream authority. The agenda_setter (caliphate_state) is institutional with arbitrage exit — it can modulate enforcement but cannot abandon the doctrine without losing its religious legitimacy capital. The analytical observer sits outside the directionality field.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (stabilizing revelation against rationalist dissolution) is contested: traditionalists say it remains live (divine transcendence is perpetually threatened by human reason); reformists say it is dead (the historical challenge has passed, the constraint now serves power). The constraint persists despite the contested status — a mandate that has outlived its original function but is maintained because the beneficiaries' identity and authority are fused with it. This is mandatrophy: the arrangement's justification has atrophied, but the constraint remains because the cost of dismantling it (epistemic collapse of the traditional framework) exceeds what any single actor can bear.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_constructed_orthodoxy,
    'Is the uncreated reading a genuine natural law (mountain) — an ontological fact about divine speech — or a constructed orthodoxy that benefits identifiable elites (traditional jurists, literalist communities)?',
    'Comparative theological analysis: if the doctrine''s necessity can be derived solely from divine attributes (without reference to historical controversy), it leans mountain. If its formulation tracks the political-theological needs of post-mihna Abbasid and later Sunni establishments, it leans constructed.',
    'If constructed, the constraint is a false summit mountain (FSM) — a snare or tangled rope masquerading as natural law. The engine''s FSM signature would trigger reclassification to tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_orthodoxy, conceptual, 'Core ambiguity: mountain vs. constructed extraction.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (state enforcement, institutional gatekeeping) or internalized (theological commitment that makes dissent unthinkable)?',
    'Post-exit suppression trajectory: track individuals who leave traditional circles — if suppression persists (self-censorship, epistemic anxiety), internalized component is significant. Historical analysis of mihna vs. post-mihna eras: state enforcement faded but social enforcement intensified.',
    'If internalized, effective suppression is higher than structural measures suggest — the target carries the constraint after formal exit. This amplifies extraction for identity_locked agents.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression in theological orthodoxy.').

omega_variable(
    kernel_reading_relations,
    'Does the uncreated reading logically foreclose the created reading, or do they coexist as live options in different frameworks?',
    'Analyze the logical structure of each reading''s axioms: if ''quran_is_eternal'' and ''quran_is_created'' are contradictory predicates applied to the same subject in the same respect, foreclosure holds. If they operate in different semantic registers (e.g., eternal meaning vs. temporal manifestation), coexistence may be possible.',
    'Foreclosure means the kernel cannot host both readings in one commitment system — the contest is zero-sum. Coexistence means pluralism is structurally possible.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_relations, conceptual, 'Logical relationship between sibling readings of the quran_ontological_status kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(quran_ontological_status__uncreated_reading, 600, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(quran_uncreated_tr_t600, quran_ontological_status__uncreated_reading, theater_ratio, 600, 0.1).
narrative_ontology:measurement(quran_uncreated_tr_t800, quran_ontological_status__uncreated_reading, theater_ratio, 800, 0.2).
narrative_ontology:measurement(quran_uncreated_tr_t900, quran_ontological_status__uncreated_reading, theater_ratio, 900, 0.3).
narrative_ontology:measurement(quran_uncreated_tr_t1000, quran_ontological_status__uncreated_reading, theater_ratio, 1000, 0.35).
narrative_ontology:measurement(quran_uncreated_tr_t1200, quran_ontological_status__uncreated_reading, theater_ratio, 1200, 0.4).
narrative_ontology:measurement(quran_uncreated_tr_t1500, quran_ontological_status__uncreated_reading, theater_ratio, 1500, 0.45).
narrative_ontology:measurement(quran_uncreated_tr_t2024, quran_ontological_status__uncreated_reading, theater_ratio, 2024, 0.5).

% Extraction over time
narrative_ontology:measurement(quran_uncreated_be_t600, quran_ontological_status__uncreated_reading, base_extractiveness, 600, 0.3).
narrative_ontology:measurement(quran_uncreated_be_t800, quran_ontological_status__uncreated_reading, base_extractiveness, 800, 0.5).
narrative_ontology:measurement(quran_uncreated_be_t900, quran_ontological_status__uncreated_reading, base_extractiveness, 900, 0.65).
narrative_ontology:measurement(quran_uncreated_be_t1000, quran_ontological_status__uncreated_reading, base_extractiveness, 1000, 0.7).
narrative_ontology:measurement(quran_uncreated_be_t1200, quran_ontological_status__uncreated_reading, base_extractiveness, 1200, 0.65).
narrative_ontology:measurement(quran_uncreated_be_t1500, quran_ontological_status__uncreated_reading, base_extractiveness, 1500, 0.6).
narrative_ontology:measurement(quran_uncreated_be_t2024, quran_ontological_status__uncreated_reading, base_extractiveness, 2024, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(quran_uncreated_su_t600, quran_ontological_status__uncreated_reading, suppression_requirement, 600, 0.2).
narrative_ontology:measurement(quran_uncreated_su_t800, quran_ontological_status__uncreated_reading, suppression_requirement, 800, 0.4).
narrative_ontology:measurement(quran_uncreated_su_t900, quran_ontological_status__uncreated_reading, suppression_requirement, 900, 0.8).
narrative_ontology:measurement(quran_uncreated_su_t1000, quran_ontological_status__uncreated_reading, suppression_requirement, 1000, 0.7).
narrative_ontology:measurement(quran_uncreated_su_t1200, quran_ontological_status__uncreated_reading, suppression_requirement, 1200, 0.6).
narrative_ontology:measurement(quran_uncreated_su_t1500, quran_ontological_status__uncreated_reading, suppression_requirement, 1500, 0.5).
narrative_ontology:measurement(quran_uncreated_su_t2024, quran_ontological_status__uncreated_reading, suppression_requirement, 2024, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(quran_ontological_status__uncreated_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(quran_ontological_status__uncreated_reading, 0.08).
narrative_ontology:affects_constraint(quran_ontological_status__uncreated_reading, quran_ontological_status__created_reading).
narrative_ontology:affects_constraint(quran_ontological_status__uncreated_reading, quran_ontological_status__state_enforced_creation_reading).

% DUAL FORMULATION NOTE:
% This constraint is one member of the quran_ontological_status constraint family. The three readings (uncreated, created, state_enforced_creation) share the same kernel but instantiate different constraints with distinct ε values, beneficiary/victim structures, and enforcement histories. The uncreated reading claims mountain status with high accessibility_collapse; the created reading claims rope (coordination of reason and revelation); the state_enforced_creation reading is a snare (state coercion of doctrine).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(quran_ontological_status__uncreated_reading, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
