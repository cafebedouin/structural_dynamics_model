% ============================================================================
% CONSTRAINT STORY: simultaneous_veneration__pragmatic_incoherence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_simultaneous_veneration__pragmatic_incoherence_reading, []).

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
 *   constraint_id: simultaneous_veneration__pragmatic_incoherence_reading
 *   human_readable: Simultaneous Veneration as Pragmatic Incoherence (Meiji Revelation)
 *   domain: religious_studies/comparative_religion/japanese_history
 *
 * SUMMARY:
 *   The simultaneous veneration of kami and buddhas (shinbutsu-shūgō) in
 *   Tokugawa Japan (1600–1868) is often described as a harmonious syncretism.
 *   This reading argues it was never coherent: practitioners held
 *   contradictory beliefs (kami as this-worldly powers, buddhas as afterlife
 *   saviors; honji-suijaku as metaphysics vs. honji-suijaku as bureaucratic
 *   fiction) without resolution, sustained only because the Tokugawa state's
 *   terauke system enforced affiliation without enforcing doctrinal
 *   coherence. The Meiji shinbutsu-bunri (1868) was not an imposition on a
 *   coherent tradition but the revelation of latent incoherence — when the
 *   enforcement pressure vanished, the contradiction surfaced as mass
 *   violence (haibutsu-kishaku). The constraint is a piton: its original
 *   coordination function (population registration, anti-Christian policing,
 *   institutional complementarity) atrophied, leaving mostly theatrical
 *   maintenance of syncretic rituals by beneficiaries who could not afford
 *   coherence.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(simultaneous_veneration__pragmatic_incoherence_reading, 0.78).
domain_priors:suppression_score(simultaneous_veneration__pragmatic_incoherence_reading, 0.45).
domain_priors:theater_ratio(simultaneous_veneration__pragmatic_incoherence_reading, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(simultaneous_veneration__pragmatic_incoherence_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(simultaneous_veneration__pragmatic_incoherence_reading, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(simultaneous_veneration__pragmatic_incoherence_reading, theater_ratio, 0.65).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(simultaneous_veneration__pragmatic_incoherence_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(simultaneous_veneration__pragmatic_incoherence_reading, resistance, 0.25).

% --- Constraint claim ---
narrative_ontology:constraint_claim(simultaneous_veneration__pragmatic_incoherence_reading, piton).
narrative_ontology:human_readable(simultaneous_veneration__pragmatic_incoherence_reading, "Simultaneous Veneration as Pragmatic Incoherence (Meiji Revelation)").
narrative_ontology:topic_domain(simultaneous_veneration__pragmatic_incoherence_reading, "religious_studies/comparative_religion/japanese_history").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(simultaneous_veneration__pragmatic_incoherence_reading, '4fc3cd7d-a4b7-43ac-b98e-08cf08a01ace').
narrative_ontology:cs_kernel_codification('4fc3cd7d-a4b7-43ac-b98e-08cf08a01ace', implicit).
narrative_ontology:cs_authority_grounding('4fc3cd7d-a4b7-43ac-b98e-08cf08a01ace', practice).
narrative_ontology:cs_interpretation_layer_present('4fc3cd7d-a4b7-43ac-b98e-08cf08a01ace').
narrative_ontology:cs_reading_relation('4fc3cd7d-a4b7-43ac-b98e-08cf08a01ace', simultaneous_veneration__ontological_fusion_reading, forecloses).
narrative_ontology:cs_reading_relation('4fc3cd7d-a4b7-43ac-b98e-08cf08a01ace', simultaneous_veneration__domain_partition_reading, coexists_with).
narrative_ontology:cs_axiom('4fc3cd7d-a4b7-43ac-b98e-08cf08a01ace', foundational, syncretic_arrangement_lacks_doctrinal_coherence).
narrative_ontology:cs_axiom_status(syncretic_arrangement_lacks_doctrinal_coherence, holdable).
narrative_ontology:cs_axiom_grounding('4fc3cd7d-a4b7-43ac-b98e-08cf08a01ace', syncretic_arrangement_lacks_doctrinal_coherence, empirically_contingent).
narrative_ontology:cs_axiom('4fc3cd7d-a4b7-43ac-b98e-08cf08a01ace', foundational, meiji_separation_reveals_latent_contradiction).
narrative_ontology:cs_axiom_status(meiji_separation_reveals_latent_contradiction, holdable).
narrative_ontology:cs_axiom_grounding('4fc3cd7d-a4b7-43ac-b98e-08cf08a01ace', meiji_separation_reveals_latent_contradiction, empirically_contingent).
narrative_ontology:cs_reference_frame('4fc3cd7d-a4b7-43ac-b98e-08cf08a01ace', tokugawa_managed_ambiguity).
narrative_ontology:cs_drift_state('4fc3cd7d-a4b7-43ac-b98e-08cf08a01ace', meiji_restoration, gap(authority_erosion, severe, false)).
narrative_ontology:cs_created_at('4fc3cd7d-a4b7-43ac-b98e-08cf08a01ace', '').
narrative_ontology:cs_kernel_id(simultaneous_veneration__pragmatic_incoherence_reading, simultaneous_veneration).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(simultaneous_veneration__pragmatic_incoherence_reading, tokugawa_temple_registrars).
narrative_ontology:constraint_beneficiary(simultaneous_veneration__pragmatic_incoherence_reading, local_shinto_priesthoods).
narrative_ontology:constraint_beneficiary(simultaneous_veneration__pragmatic_incoherence_reading, honji_suijaku_theologians).
narrative_ontology:constraint_victim(simultaneous_veneration__pragmatic_incoherence_reading, ordinary_village_practitioners).
narrative_ontology:constraint_victim(simultaneous_veneration__pragmatic_incoherence_reading, lay_brotherhood_members).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(simultaneous_veneration__pragmatic_incoherence_reading, lay_brotherhood_members).
narrative_ontology:constraint_vindicates(simultaneous_veneration__pragmatic_incoherence_reading, honji_suijaku_soteriology).
narrative_ontology:constraint_vindicates(simultaneous_veneration__pragmatic_incoherence_reading, shrine_temple_syncretic_authority).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administer the terauke (temple registration) system requiring all households to register with a Buddhist temple. They coordinate the bureaucratic enforcement of religious affiliation but do not enforce doctrinal coherence — the system functions as population control, not theological policing. They benefit from stable institutional positions and fee income from certification.
narrative_ontology:constraint_stakeholder(simultaneous_veneration__pragmatic_incoherence_reading, tokugawa_temple_registrars, agenda_setter,
    institutional, generational, arbitrage, national).

% Serve as shrine priests within the shrine-temple multiplex system (jingū-ji). They perform kami rituals for this-worldly benefits (harvest, health, protection) while Buddhist clergy handle funerals and afterlife. They collect offering income and maintain local status through the syncretic arrangement; their livelihood depends on the multiplex not being interrogated for coherence.
narrative_ontology:constraint_stakeholder(simultaneous_veneration__pragmatic_incoherence_reading, local_shinto_priesthoods, beneficiary,
    organized, biographical, constrained, regional).

% Produce and maintain the honji-suijaku (original ground / trace manifestation) theoretical framework that presents kami as local manifestations of buddhas. Their scholarly authority and institutional positions (temple leadership, academic lineages) depend on this framework being treated as settled doctrine rather than contested interpretation. They cannot exit the identity of 'syncretic theologian' without losing their entire professional standing.
narrative_ontology:constraint_stakeholder(simultaneous_veneration__pragmatic_incoherence_reading, honji_suijaku_theologians, beneficiary,
    organized, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(simultaneous_veneration__pragmatic_incoherence_reading, honji_suijaku_theologians, agenda_setter).

% Participate in both shrine festivals and Buddhist funerals, pray to kami for worldly benefits and to buddhas for salvation, and hold contradictory beliefs about the nature of the beings they venerate without ever being asked to reconcile them. They bear the cognitive cost of unresolved contradiction and the material cost of supporting both institutions through offerings and labor. Exit is structurally impossible — the terauke system legally binds them to a temple, and the village shrine is the only site for communal this-worldly petition.
narrative_ontology:constraint_stakeholder(simultaneous_veneration__pragmatic_incoherence_reading, ordinary_village_practitioners, payer,
    powerless, biographical, trapped, local).

% Organize in kō (lay brotherhoods) that sponsor pilgrimages to both shrines and temples. They gain social cohesion, mutual aid networks, and spiritual assurance from participation, but also contribute labor and funds that sustain the incoherent multiplex. Some develop sophisticated personal syntheses; most simply navigate the contradiction pragmatically. Exit is possible through relocation or joining a single-tradition group (e.g., True Pure Land), but social and economic ties make it costly.
narrative_ontology:constraint_stakeholder(simultaneous_veneration__pragmatic_incoherence_reading, lay_brotherhood_members, payer,
    moderate, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(simultaneous_veneration__pragmatic_incoherence_reading, lay_brotherhood_members, beneficiary).

% After 1868, they impose shinbutsu-bunri (separation of kami and buddhas) as state policy, destroying shrine-temple multiplexes and forcing a choice. They interpret the pre-Meiji arrangement as 'superstition' to be cleared, but their separation decree reveals the latent incoherence rather than creating it — practitioners who never had to choose suddenly must, and the contradiction surfaces as mass haibutsu-kishaku (abolish buddhas) violence.
narrative_ontology:constraint_stakeholder(simultaneous_veneration__pragmatic_incoherence_reading, meiji_restoration_ideologues, observer,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The shrine-temple multiplex provided a division of spiritual labor: shrines handled this-worldly petition (agriculture, health, protection) and temples handled afterlife salvation (funerals, ancestral rites, rebirth). The terauke system added population registration and anti-Christian policing. No coherent theology was required — the arrangement coordinated institutional survival and social order through functional complementarity.
% TRANSFER_FUNCTION: Moves offerings, labor, and cognitive compliance from ordinary practitioners and lay brotherhoods to temple registrars, shrine priesthoods, and syncretic theologians. The terauke certification fees flow to temples; shrine offerings flow to priesthoods; theological authority flows to honji-suijaku exegetes. The practitioners pay in money, time, and unresolved contradiction.
% ABSENT_VOICES: There was no recognized category of 'dissenter from syncretism' in Tokugawa Japan — the terauke system criminalized non-affiliation (as Christianity), and the shrine-temple multiplex left no institutional space for a 'pure' Shinto or 'pure' Buddhist identity outside the syncretic frame. Crypto-Christians, mountain ascetics (shugendō) who resisted institutional capture, and True Pure Land adherents who rejected syncretism existed but were structurally excluded from the public religious field.
% DISAPPEARANCE_RATIONALE: If the syncretic multiplex vanished overnight (as it effectively did in 1868–1871), the world rearranged violently: shrine-temple complexes were physically destroyed, Buddhist artifacts were burned or thrown into rivers, priests were defrocked or forced to become Shinto priests, and practitioners were compelled to choose — revealing that the arrangement had been holding together a contradiction that could not survive exposure. The Meiji separation was not an imposition on a coherent system but the rupture of a fragile equilibrium.
% FOUNDING_PROBLEM: After the Sengoku period's chaos, the Tokugawa shogunate needed a stable religious order that could register the population, suppress Christianity, and legitimize local authority without doctrinal conflict. The shrine-temple multiplex solved this by assigning complementary functions to kami and buddhas and suppressing the question of their ontological relationship.
% FOUNDING_PROBLEM_CORROBORATION: Tokugawa administrative records (bakufu diaries, temple registration rolls) confirm the population-control and anti-Christian functions. The Meiji government's own separation edicts (1868, 1871) explicitly cite the 'confusion' of kami and buddhas as a problem to be solved, corroborating that the founding problem (stability through managed ambiguity) was recognized as obsolete by the new regime. No contemporary Tokugawa source outside the benefiting institutions (temples, shrines, theologians) attests that the arrangement solved a genuine spiritual problem for practitioners — the corroboration for 'dead' status comes from the state that replaced it, not from the arrangement's own beneficiaries.
narrative_ontology:disappearance_verdict(simultaneous_veneration__pragmatic_incoherence_reading, world_rearranges).
narrative_ontology:founding_problem_status(simultaneous_veneration__pragmatic_incoherence_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(simultaneous_veneration__pragmatic_incoherence_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(simultaneous_veneration__pragmatic_incoherence_reading, 'none', 1).
narrative_ontology:epsilon_provenance(simultaneous_veneration__pragmatic_incoherence_reading, 0.78, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(simultaneous_veneration__pragmatic_incoherence_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(simultaneous_veneration__pragmatic_incoherence_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(simultaneous_veneration__pragmatic_incoherence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.78 at interval end) because practitioners bear cognitive and material costs for an arrangement that provides no coherent theology — the contradiction is the extraction mechanism, not a bug. Suppression is moderate (0.45): the terauke system enforced affiliation but not belief, so suppression targeted exit (leaving the multiplex) rather than internal dissent. Theater ratio rises from 0.3 to 0.65 as honji-suijaku theology becomes increasingly performative — elaborate commentaries on a framework no one genuinely holds as metaphysics. Accessibility collapse is low (0.35) because alternatives (Pure Land exclusivity, Shinto nativism, crypto-Christianity) existed but were structurally suppressed. Resistance is low (0.25) until 1868 because the contradiction was managed, not confronted.
 *
 * PERSPECTIVAL GAP:
 *   The beneficiary seats (registrars, theologians, shrine priests) experience the arrangement as functional coordination — it solves their institutional problems. The payer seats (ordinary practitioners, lay brotherhoods) experience it as cognitive extraction — they hold contradictions they cannot resolve and pay for institutions that require those contradictions. The engine computes this divergence from the structural data; the claimed type (piton) reflects the atrophied coordination function.
 *
 * DIRECTIONALITY LOGIC:
 *   Tokugawa temple registrars and honji-suijaku theologians are beneficiaries (d near 0.1–0.2): they collect institutional rents and authority from the arrangement. Local shrine priesthoods are beneficiaries (d ~0.25): they gain livelihood and status but are more constrained by local expectations. Ordinary practitioners are full targets (d ~0.9): trapped by terauke, cognitively extracting from unresolved contradiction. Lay brotherhood members are partial targets with some benefit (d ~0.6): they gain social cohesion but pay for the multiplex's upkeep. Meiji ideologues are analytical observers (d=0.5): they see the structure from outside and impose the rupture.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (Tokugawa population control and anti-Christian policy via religious registration) is dead — the Meiji state solved population registration through modern bureaucracy and abandoned the anti-Christian imperative. The arrangement persists only as theatrical maintenance by beneficiaries who cannot transition to a coherent theology without losing their institutional basis. This is classic mandatrophy: the mandate (registration/control) outlived its function, but the constraint (syncretic multiplex) remained due to beneficiary inertia. The Meiji separation was the forced resolution.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    coherence_threshold,
    'At what threshold of doctrinal contradiction does a religious arrangement cease to be ''syncretic'' and become ''incoherent''? Is there a structural test, or is this a retrospective judgment from the Meiji rupture?',
    'Comparative analysis of other syncretic systems (e.g., Sino-Japanese Buddhism, Latin American folk Catholicism) that survived without state-imposed separation — do they exhibit similar contradiction profiles?',
    'If a structural test exists, the ''pragmatic incoherence'' reading gains empirical grounding; if it is purely retrospective, the reading risks projecting Meiji categories backward.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coherence_threshold, conceptual, 'Whether incoherence is a structural property of the arrangement or an observer-relative judgment').

omega_variable(
    extraction_mechanism,
    'Is the extraction primarily cognitive (holding unresolved contradictions) or material (offerings, fees, labor)? Does the cognitive extraction require the material structure, or could it operate independently?',
    'Economic history of temple/shrine finances in Tokugawa Japan; cognitive anthropology of contradiction-holding in contemporary syncretic practitioners.',
    'If cognitive extraction is primary and structurally necessary, the constraint is a snare even without high material suppression. If material extraction is primary, the piton classification (theatrical maintenance of atrophied function) is more accurate.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(extraction_mechanism, empirical, 'Primary extraction channel and its structural necessity').

omega_variable(
    meiji_as_revelation_vs_imposition,
    'Was the Meiji separation a revelation of latent incoherence (this reading''s claim) or an imposition of modern categories (religion/Shinto/Buddhism) on a functioning lifeworld?',
    'Micro-history of 1868–1871 separation in specific domains: did practitioners experience the separation as clarifying or as destruction? Analyze haibutsu-kishaku violence patterns — spontaneous popular action vs. state-directed.',
    'If revelation, the constraint was a piton whose collapse was inevitable. If imposition, the constraint was a functioning rope/scaffold destroyed by state violence — changing the classification of the pre-Meiji arrangement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(meiji_as_revelation_vs_imposition, conceptual, 'Whether Meiji separation reveals or creates the incoherence').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(simultaneous_veneration__pragmatic_incoherence_reading, 1600, 1870).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(simu_tr_t1600, simultaneous_veneration__pragmatic_incoherence_reading, theater_ratio, 1600, 0.3).
narrative_ontology:measurement(simu_tr_t1650, simultaneous_veneration__pragmatic_incoherence_reading, theater_ratio, 1650, 0.38).
narrative_ontology:measurement(simu_tr_t1700, simultaneous_veneration__pragmatic_incoherence_reading, theater_ratio, 1700, 0.45).
narrative_ontology:measurement(simu_tr_t1750, simultaneous_veneration__pragmatic_incoherence_reading, theater_ratio, 1750, 0.52).
narrative_ontology:measurement(simu_tr_t1800, simultaneous_veneration__pragmatic_incoherence_reading, theater_ratio, 1800, 0.58).
narrative_ontology:measurement(simu_tr_t1850, simultaneous_veneration__pragmatic_incoherence_reading, theater_ratio, 1850, 0.62).
narrative_ontology:measurement(simu_tr_t1870, simultaneous_veneration__pragmatic_incoherence_reading, theater_ratio, 1870, 0.65).

% Extraction over time
narrative_ontology:measurement(simu_be_t1600, simultaneous_veneration__pragmatic_incoherence_reading, base_extractiveness, 1600, 0.55).
narrative_ontology:measurement(simu_be_t1650, simultaneous_veneration__pragmatic_incoherence_reading, base_extractiveness, 1650, 0.6).
narrative_ontology:measurement(simu_be_t1700, simultaneous_veneration__pragmatic_incoherence_reading, base_extractiveness, 1700, 0.65).
narrative_ontology:measurement(simu_be_t1750, simultaneous_veneration__pragmatic_incoherence_reading, base_extractiveness, 1750, 0.7).
narrative_ontology:measurement(simu_be_t1800, simultaneous_veneration__pragmatic_incoherence_reading, base_extractiveness, 1800, 0.73).
narrative_ontology:measurement(simu_be_t1850, simultaneous_veneration__pragmatic_incoherence_reading, base_extractiveness, 1850, 0.76).
narrative_ontology:measurement(simu_be_t1870, simultaneous_veneration__pragmatic_incoherence_reading, base_extractiveness, 1870, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(simu_su_t1600, simultaneous_veneration__pragmatic_incoherence_reading, suppression_requirement, 1600, 0.5).
narrative_ontology:measurement(simu_su_t1650, simultaneous_veneration__pragmatic_incoherence_reading, suppression_requirement, 1650, 0.45).
narrative_ontology:measurement(simu_su_t1700, simultaneous_veneration__pragmatic_incoherence_reading, suppression_requirement, 1700, 0.4).
narrative_ontology:measurement(simu_su_t1750, simultaneous_veneration__pragmatic_incoherence_reading, suppression_requirement, 1750, 0.38).
narrative_ontology:measurement(simu_su_t1800, simultaneous_veneration__pragmatic_incoherence_reading, suppression_requirement, 1800, 0.4).
narrative_ontology:measurement(simu_su_t1850, simultaneous_veneration__pragmatic_incoherence_reading, suppression_requirement, 1850, 0.42).
narrative_ontology:measurement(simu_su_t1870, simultaneous_veneration__pragmatic_incoherence_reading, suppression_requirement, 1870, 0.45).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(simultaneous_veneration__pragmatic_incoherence_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(simultaneous_veneration__pragmatic_incoherence_reading, 0.12).
narrative_ontology:affects_constraint(simultaneous_veneration__pragmatic_incoherence_reading, simultaneous_veneration__ontological_fusion_reading).
narrative_ontology:affects_constraint(simultaneous_veneration__pragmatic_incoherence_reading, simultaneous_veneration__domain_partition_reading).
narrative_ontology:affects_constraint(simultaneous_veneration__pragmatic_incoherence_reading, meiji_shinbutsu_bunri).

% DUAL FORMULATION NOTE:
% The simultaneous_veneration kernel decomposes into three readings with distinct ε values and type trajectories. This reading (pragmatic_incoherence) has high ε (0.78) and classifies as piton — the arrangement atrophied into theatrical maintenance. The ontological_fusion_reading would claim low ε (mountain/rope) by treating honji-suijaku as genuine metaphysics. The domain_partition_reading would claim moderate ε (tangled_rope) by treating functional specialization as genuine coordination with residual extraction. The ε-invariance principle requires separate stories because the observables (what counts as 'the constraint') differ structurally.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(simultaneous_veneration__pragmatic_incoherence_reading, organized, 0.15).
constraint_indexing:directionality_override(simultaneous_veneration__pragmatic_incoherence_reading, powerless, 0.9).
constraint_indexing:directionality_override(simultaneous_veneration__pragmatic_incoherence_reading, moderate, 0.6).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
