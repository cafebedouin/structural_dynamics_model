% ============================================================================
% CONSTRAINT STORY: quran_ontological_status__uncreated_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-04
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
 *   The uncreated reading holds that the Qur'an is God's eternal speech
 *   (kalām), coeternal with His essence — not a created artifact in time.
 *   This doctrine, crystallized in the 9th century against Mu'tazilite
 *   rationalism and hardened through the Abbasid mihna (inquisition),
 *   functions as an ontic constraint: it fixes revelation outside history,
 *   making textual meaning a divine fact rather than a human interpretation.
 *   The reading presents itself as a mountain (divine ontology), but its
 *   historical trajectory — from pious consensus to enforced orthodoxy to
 *   institutionalized dogma — raises the false summit question. Beneficiaries
 *   (traditional jurists, anti-rationalist schools, caliphal claimants) gain
 *   fixed authority; victims (rational theologians, metaphorical
 *   interpreters, reform movements) bear the cost of foreclosed hermeneutic
 *   space.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(quran_ontological_status__uncreated_reading, 0.15).
domain_priors:suppression_score(quran_ontological_status__uncreated_reading, 0.62).
domain_priors:theater_ratio(quran_ontological_status__uncreated_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(quran_ontological_status__uncreated_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(quran_ontological_status__uncreated_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(quran_ontological_status__uncreated_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(quran_ontological_status__uncreated_reading, accessibility_collapse, 0.88).
narrative_ontology:constraint_metric(quran_ontological_status__uncreated_reading, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(quran_ontological_status__uncreated_reading, mountain).
narrative_ontology:human_readable(quran_ontological_status__uncreated_reading, "Qur'an as Uncreated Eternal Divine Speech (Uncreated Reading)").
narrative_ontology:topic_domain(quran_ontological_status__uncreated_reading, "theological/philosophical/political").

domain_priors:emerges_naturally(quran_ontological_status__uncreated_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(quran_ontological_status__uncreated_reading, 'aabefb36-aca3-4ead-a760-73246f13c4fa').
narrative_ontology:cs_kernel_codification('aabefb36-aca3-4ead-a760-73246f13c4fa', fixed_text).
narrative_ontology:cs_authority_grounding('aabefb36-aca3-4ead-a760-73246f13c4fa', lineage).
narrative_ontology:cs_interpretation_layer_present('aabefb36-aca3-4ead-a760-73246f13c4fa').
narrative_ontology:cs_reading_relation('aabefb36-aca3-4ead-a760-73246f13c4fa', quran_ontological_status__created_reading, forecloses).
narrative_ontology:cs_reading_relation('aabefb36-aca3-4ead-a760-73246f13c4fa', quran_ontological_status__state_enforced_creation_reading, influences).
narrative_ontology:cs_axiom('aabefb36-aca3-4ead-a760-73246f13c4fa', foundational, quran_speech_coeternal_with_divine_essence).
narrative_ontology:cs_axiom_status(quran_speech_coeternal_with_divine_essence, holdable).
narrative_ontology:cs_axiom_grounding('aabefb36-aca3-4ead-a760-73246f13c4fa', quran_speech_coeternal_with_divine_essence, deontological).
narrative_ontology:cs_axiom('aabefb36-aca3-4ead-a760-73246f13c4fa', foundational, literal_meaning_ontologically_prior_to_interpretation).
narrative_ontology:cs_axiom_status(literal_meaning_ontologically_prior_to_interpretation, holdable).
narrative_ontology:cs_axiom_grounding('aabefb36-aca3-4ead-a760-73246f13c4fa', literal_meaning_ontologically_prior_to_interpretation, deontological).
narrative_ontology:cs_axiom('aabefb36-aca3-4ead-a760-73246f13c4fa', secondary, rationalist_speculation_on_divine_attributes_is_heresy).
narrative_ontology:cs_axiom_status(rationalist_speculation_on_divine_attributes_is_heresy, holdable).
narrative_ontology:cs_axiom_grounding('aabefb36-aca3-4ead-a760-73246f13c4fa', rationalist_speculation_on_divine_attributes_is_heresy, conventional).
narrative_ontology:cs_reference_frame('aabefb36-aca3-4ead-a760-73246f13c4fa', classical_asharite_orthodoxy).
narrative_ontology:cs_drift_state('aabefb36-aca3-4ead-a760-73246f13c4fa', post_mihna_consolidation, gap(codification_collapse, substantial, false)).
narrative_ontology:cs_created_at('aabefb36-aca3-4ead-a760-73246f13c4fa', '').
narrative_ontology:cs_kernel_id(quran_ontological_status__uncreated_reading, quran_ontological_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(quran_ontological_status__uncreated_reading, traditional_jurists).
narrative_ontology:constraint_beneficiary(quran_ontological_status__uncreated_reading, literalist_communities).
narrative_ontology:constraint_beneficiary(quran_ontological_status__uncreated_reading, anti_rationalist_schools).
narrative_ontology:constraint_beneficiary(quran_ontological_status__uncreated_reading, caliphal_authority_claimants).
narrative_ontology:constraint_victim(quran_ontological_status__uncreated_reading, rational_theologians).
narrative_ontology:constraint_victim(quran_ontological_status__uncreated_reading, metaphorical_interpreters).
narrative_ontology:constraint_victim(quran_ontological_status__uncreated_reading, reform_movements).
narrative_ontology:constraint_vindicates(quran_ontological_status__uncreated_reading, divine_speech_coeternal_with_essence).
narrative_ontology:constraint_vindicates(quran_ontological_status__uncreated_reading, textual_inerrancy).
narrative_ontology:constraint_vindicates(quran_ontological_status__uncreated_reading, prophetic_authority_maximal).
narrative_ontology:constraint_vindicates(quran_ontological_status__uncreated_reading, literalist_hermeneutic_privileged).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Their interpretive authority and legal methodology depend on the text being fixed, unchanging, and directly traceable to divine will. If the Qur'an is created, their claim to mediate unchanging divine law collapses. Exit from this position means abandoning the epistemic foundation of their professional identity and social role.
narrative_ontology:constraint_stakeholder(quran_ontological_status__uncreated_reading, traditional_jurists, beneficiary,
    organized, generational, identity_locked, continental).

% Their religious practice and communal identity are constituted through literal adherence to a text they experience as the direct, unmediated speech of God. The uncreated reading validates their piety as conformity to eternal truth; a created reading would make their devotion contingent on a historical artifact.
narrative_ontology:constraint_stakeholder(quran_ontological_status__uncreated_reading, literalist_communities, beneficiary,
    moderate, biographical, identity_locked, continental).

% Schools like the Ash'arites and later traditionalist movements defined their doctrinal boundaries by defending the uncreated Qur'an against rationalist theology. They administer the constraint through madrasa curricula, fatwa networks, and political alliance. Their institutional survival is fused to the constraint's persistence.
narrative_ontology:constraint_stakeholder(quran_ontological_status__uncreated_reading, anti_rationalist_schools, agenda_setter,
    institutional, civilizational, trapped, continental).
narrative_ontology:stakeholder_secondary_role(quran_ontological_status__uncreated_reading, anti_rationalist_schools, beneficiary).

% Rulers who claim legitimacy through guardianship of an uncreated revelation gain a fixed, non-negotiable source of authority that transcends political contingency. The constraint lets them anchor sovereignty in divine eternity rather than temporal negotiation.
narrative_ontology:constraint_stakeholder(quran_ontological_status__uncreated_reading, caliphal_authority_claimants, agenda_setter,
    institutional, generational, constrained, continental).
narrative_ontology:stakeholder_secondary_role(quran_ontological_status__uncreated_reading, caliphal_authority_claimants, beneficiary).

% Mu'tazilite and other rationalist theologians who argue the Qur'an must be created to preserve divine transcendence and unity (tawhid) face heresy accusations, exclusion from patronage, and in extreme cases (the mihna) state violence. Their intellectual project requires textual flexibility that the uncreated reading forecloses.
narrative_ontology:constraint_stakeholder(quran_ontological_status__uncreated_reading, rational_theologians, payer,
    organized, biographical, constrained, continental).

% Interpreters who read Qur'anic language as metaphorical, contextual, or accommodating to reason are delegitimized by the uncreated reading's claim that every word is literally God's eternal speech. Their hermeneutic is treated as distortion rather than legitimate engagement.
narrative_ontology:constraint_stakeholder(quran_ontological_status__uncreated_reading, metaphorical_interpreters, payer,
    moderate, biographical, constrained, continental).
narrative_ontology:stakeholder_secondary_role(quran_ontological_status__uncreated_reading, metaphorical_interpreters, excluded).

% Movements seeking legal, social, or theological reform on grounds of changed circumstances, reason, or justice find the uncreated reading an immovable barrier: if the text is God's eternal speech, its rulings cannot be adapted without implying God changes or errs.
narrative_ontology:constraint_stakeholder(quran_ontological_status__uncreated_reading, reform_movements, payer,
    moderate, generational, constrained, continental).
narrative_ontology:stakeholder_secondary_role(quran_ontological_status__uncreated_reading, reform_movements, excluded).

% Analytical observers (philosophers, historians, comparative theologians) who study the constraint's operation across the tradition without being personally subject to its enforcement or benefiting from its rents.
narrative_ontology:constraint_stakeholder(quran_ontological_status__uncreated_reading, philosophical_observers, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a fixed, authoritative reference point for legal, theological, and communal coordination across the Islamic world: a single text whose meaning is stable, universal, and binding because it is God's own eternal speech.
% TRANSFER_FUNCTION: Moves interpretive authority and definitional power over divine will from human reason and historical contingency to a fixed textual locus controlled by traditional jurists and anti-rationalist institutions; moves the cost of theological innovation onto rationalists, reformers, and marginalized interpreters.
% ABSENT_VOICES: Early Kharijite and Shi'a rationalist voices who rejected both the uncreated dogma and the state enforcement that hardened it; non-Arab converts (mawali) whose theological perspectives were filtered through Arab-tribal patronage structures; women scholars and mystics whose interpretive traditions were excluded from the madrasa canon that institutionalized the uncreated reading.
% DISAPPEARANCE_RATIONALE: If the uncreated reading vanished overnight, the textual fixity underpinning traditional fiqh, Ash'arite kalam, and caliphal legitimacy claims would collapse. Rationalist theology, metaphorical hermeneutics, and reformist projects would lose their primary structural opponent. The mihna's historical trauma would lose its defining theological stakes. The Islamic intellectual landscape would reorganize around contested textuality rather than eternal fixity.
% FOUNDING_PROBLEM: How to secure the Qur'an's authority against (a) Christian Logos theology that claims divine speech entered time in Christ, (b) Mu'tazilite rationalism that subjects revelation to Greek philosophical categories, and (c) political fragmentation that threatens a unified legal-theological order across the caliphate.
% FOUNDING_PROBLEM_CORROBORATION: Traditionalist sources (Ibn Hanbal, Ash'ari) attest the problem is live: heresy, fragmentation, and non-Islamic theology still threaten. Rationalist and reformist sources (Mu'tazila, modernist exegetes) attest the founding problem is dead or transformed: Christian polemic is no longer the primary interlocutor, Greek categories are no longer the only rational framework, and political unity is no longer the caliphate's project. The mihna's historical record — state enforcement of a theological position — corroborates that the constraint's persistence exceeded its founding rationale.
narrative_ontology:disappearance_verdict(quran_ontological_status__uncreated_reading, world_rearranges).
narrative_ontology:founding_problem_status(quran_ontological_status__uncreated_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(quran_ontological_status__uncreated_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(quran_ontological_status__uncreated_reading, 'none', 1).
narrative_ontology:epsilon_provenance(quran_ontological_status__uncreated_reading, 0.15, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(quran_ontological_status__uncreated_reading_tests).

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
 *   Extractiveness is low (0.15) from the reading's self-understanding: the constraint claims to simply *be* reality, not extract. But suppression is moderate-high (0.62) because the mihna and subsequent institutional enforcement actively silenced alternatives. Theater ratio (0.28) rises over the interval as the doctrine shifts from lived conviction to institutional performance. Accessibility collapse (0.88) is near-mountain: once the uncreated reading is accepted, alternatives (createdness, metaphor) become unintelligible within the framework. Resistance (0.35) reflects historical Mu'tazilite resistance and later reformist pushback, but the constraint's mountain claim makes resistance appear as heresy rather than disagreement.
 *
 * PERSPECTIVAL GAP:
 *   From the beneficiary seats (jurists, literalists, anti-rationalists), the constraint *is* a mountain — divine reality, not human arrangement. From the victim seats (rationalists, reformers), it operates as a snare — enforced dogma foreclosing intellectual space. From the analytical seat, it reads as a tangled_rope: genuine coordination (unified textual reference for law and community) fused with asymmetric extraction (silencing rationalist and reformist alternatives). The engine computes this divergence; the claimed_type states the reading's self-understanding.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (traditional_jurists, literalist_communities, anti_rationalist_schools, caliphal_authority_claimants) derive authority, identity, and legitimacy from the constraint — they are structural beneficiaries (d near 0.0). Victims (rational_theologians, metaphorical_interpreters, reform_movements) bear epistemic exclusion, professional marginalization, and in historical cases state violence — they are structural targets (d near 1.0). The engine computes per-seat χ from these declarations. Caliphal claimants have constrained exit (political necessity); jurists and literalists are identity_locked (exit means abandoning self-constituting framework).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (securing Qur'anic authority against Christian Logos, Greek rationalism, political fragmentation) is contested: traditionalists say it persists; reformists say it mutated. The constraint's mandate (fixing revelation as eternal) has outlived its original theological-political context (9th century Abbasid consolidation) but persists through institutional inertia (madrasa system, fatwa networks, state religions). This is classic mandatrophy: the coordination function (unified reference) remains, but the extraction layer (suppression of alternatives) no longer serves the founding problem. The constraint is not resolved — it reproduces through identity_lock and institutional capture.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    mountain_vs_constructed_natural_law,
    'Is the uncreated Qur''an a genuine natural-law constraint (divine ontology) or a constructed constraint that benefits identifiable theological and political actors?',
    'Comparative analysis of whether the constraint''s enforcement pattern tracks theological conviction or institutional interest: do beneficiaries enforce it when it costs them, or only when it serves them? Historical tracing of when the doctrine hardened from pious consensus to enforced orthodoxy.',
    'If constructed, the constraint is a false summit mountain — the FSM signature would trigger reclassification to tangled_rope (coordination of communal identity + extraction from rationalists/reformers). If genuine natural law, mountain classification holds and beneficiaries are epiphenomenal.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mountain_vs_constructed_natural_law, conceptual, 'Natural-law vs. constructed status of the uncreated Qur''an doctrine').

omega_variable(
    committer_framing_ontological_status,
    'This constraint is the uncreated_reading of the quran_ontological_status kernel. How does the sibling created_reading structurally differ, and where is the disagreement located?',
    'Map the structural delta: created_reading denies coeternality of speech and essence, which collapses the mountain into a historical artifact — lowering accessibility_collapse, raising resistance, and shifting beneficiaries/victims. The disagreement is located on the ontological status of divine attributes (speech as essence vs. speech as act).',
    'If created_reading is structurally coherent, the kernel has multiple ε-invariant constraints (per DP-001). The uncreated_reading''s ε=0.15 reflects its self-understanding as mountain; created_reading would author higher ε for the standing arrangement it contests. Both are valid readings of the same kernel.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(committer_framing_ontological_status, conceptual, 'Committer-frame structural location of disagreement between uncreated and created readings').

omega_variable(
    state_enforcement_as_extraction_layer,
    'Does the mihna (state inquisition enforcing createdness) represent a separate extraction layer atop the theological constraint, or is state enforcement intrinsic to the uncreated reading''s mountain claim?',
    'Trace whether anti-rationalist schools enforced the uncreated reading through state power before the mihna, or whether the mihna provoked the hardening. If enforcement followed the mihna as reactive consolidation, the uncreated reading''s mountain status is historically contingent.',
    'If state enforcement is intrinsic, the uncreated reading was always a tangled_rope (coordination + extraction). If enforcement is reactive, the mountain claim has a historical rupture that the FSM signature would detect via rising extractiveness measurements.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(state_enforcement_as_extraction_layer, empirical, 'Whether state enforcement is constitutive or contingent for the uncreated reading').

omega_variable(
    identity_lock_mechanism_for_jurists,
    'What specific identity-fusion mechanism binds traditional jurists to the uncreated reading — professional identity (career path dependence), relational identity (community constituted through the doctrine), ideological identity (worldview making exit unthinkable), or institutional identity (madrasa become its function)?',
    'Comparative study of jurists who did exit (e.g., early rationalists, modern reformers): what broke the lock? Was it intellectual conviction, institutional marginalization, political protection, or generational replacement?',
    'If identity_lock is primarily professional/institutional, the constraint is a piton (atrophied function maintained theatrically). If ideological/relational, the mountain claim has deeper epistemic roots. The engine derives identity_locked from beneficiary/victim + exit; this omega documents the mechanism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_mechanism_for_jurists, conceptual, 'Identity-lock mechanism for traditional jurists under the uncreated reading').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(quran_ontological_status__uncreated_reading, 750, 1000).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(quran_uncreated_tr_t750, quran_ontological_status__uncreated_reading, theater_ratio, 750, 0.1).
narrative_ontology:measurement(quran_uncreated_tr_t800, quran_ontological_status__uncreated_reading, theater_ratio, 800, 0.15).
narrative_ontology:measurement(quran_uncreated_tr_t833, quran_ontological_status__uncreated_reading, theater_ratio, 833, 0.22).
narrative_ontology:measurement(quran_uncreated_tr_t850, quran_ontological_status__uncreated_reading, theater_ratio, 850, 0.25).
narrative_ontology:measurement(quran_uncreated_tr_t900, quran_ontological_status__uncreated_reading, theater_ratio, 900, 0.27).
narrative_ontology:measurement(quran_uncreated_tr_t950, quran_ontological_status__uncreated_reading, theater_ratio, 950, 0.28).
narrative_ontology:measurement(quran_uncreated_tr_t1000, quran_ontological_status__uncreated_reading, theater_ratio, 1000, 0.28).

% Extraction over time
narrative_ontology:measurement(quran_uncreated_be_t750, quran_ontological_status__uncreated_reading, base_extractiveness, 750, 0.08).
narrative_ontology:measurement(quran_uncreated_be_t800, quran_ontological_status__uncreated_reading, base_extractiveness, 800, 0.1).
narrative_ontology:measurement(quran_uncreated_be_t833, quran_ontological_status__uncreated_reading, base_extractiveness, 833, 0.12).
narrative_ontology:measurement(quran_uncreated_be_t850, quran_ontological_status__uncreated_reading, base_extractiveness, 850, 0.15).
narrative_ontology:measurement(quran_uncreated_be_t900, quran_ontological_status__uncreated_reading, base_extractiveness, 900, 0.15).
narrative_ontology:measurement(quran_uncreated_be_t950, quran_ontological_status__uncreated_reading, base_extractiveness, 950, 0.15).
narrative_ontology:measurement(quran_uncreated_be_t1000, quran_ontological_status__uncreated_reading, base_extractiveness, 1000, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(quran_uncreated_su_t750, quran_ontological_status__uncreated_reading, suppression_requirement, 750, 0.2).
narrative_ontology:measurement(quran_uncreated_su_t800, quran_ontological_status__uncreated_reading, suppression_requirement, 800, 0.35).
narrative_ontology:measurement(quran_uncreated_su_t833, quran_ontological_status__uncreated_reading, suppression_requirement, 833, 0.55).
narrative_ontology:measurement(quran_uncreated_su_t850, quran_ontological_status__uncreated_reading, suppression_requirement, 850, 0.6).
narrative_ontology:measurement(quran_uncreated_su_t900, quran_ontological_status__uncreated_reading, suppression_requirement, 900, 0.62).
narrative_ontology:measurement(quran_uncreated_su_t950, quran_ontological_status__uncreated_reading, suppression_requirement, 950, 0.62).
narrative_ontology:measurement(quran_uncreated_su_t1000, quran_ontological_status__uncreated_reading, suppression_requirement, 1000, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(quran_ontological_status__uncreated_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(quran_ontological_status__uncreated_reading, 0.08).
narrative_ontology:affects_constraint(quran_ontological_status__uncreated_reading, quran_ontological_status__created_reading).
narrative_ontology:affects_constraint(quran_ontological_status__uncreated_reading, quran_ontological_status__state_enforced_creation_reading).
narrative_ontology:affects_constraint(quran_ontological_status__uncreated_reading, islamic_legal_authority__traditionalist).
narrative_ontology:affects_constraint(quran_ontological_status__uncreated_reading, caliphal_legitimacy__guardian_of_revelation).

% DUAL FORMULATION NOTE:
% Kernel family: quran_ontological_status. This reading (uncreated) claims mountain status with ε=0.15. The created_reading would author higher ε for the standing arrangement it contests (the uncreated dogma as enforced orthodoxy). The state_enforced_creation_reading authors ε for the mihna apparatus itself. The three readings share the kernel but instantiate different constraints with different structural profiles.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(quran_ontological_status__uncreated_reading, institutional, 0.1).
constraint_indexing:directionality_override(quran_ontological_status__uncreated_reading, organized, 0.15).
constraint_indexing:directionality_override(quran_ontological_status__uncreated_reading, moderate, 0.8).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
