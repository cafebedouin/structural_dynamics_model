% ============================================================================
% CONSTRAINT STORY: kami_buddha_ontology__honji_suijaku_monism
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_kami_buddha_ontology__honji_suijaku_monism, []).

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
 *   constraint_id: kami_buddha_ontology__honji_suijaku_monism
 *   human_readable: Honji-Suijaku Ontological Monism (Kami as Buddhist Traces)
 *   domain: religious/philosophical/japanese_history
 *
 * SUMMARY:
 *   Honji-suijaku (original ground / trace manifestation) was the dominant
 *   theoretical framework governing Japanese religion from the Heian through
 *   Edo periods. It asserted that kami (indigenous Japanese deities) are
 *   phenomenal manifestations (suijaku) of buddhas/bodhisattvas (honji —
 *   original ground), establishing Buddhist ontological priority. What began
 *   as a heuristic for integration (early Heian) hardened into a doctrinal
 *   hierarchy enforced by temple-shrine complexes (jingū-ji), state
 *   patronage, and monastic authority. The constraint coordinated Japanese
 *   religious life — unified pilgrimage, shared festivals, common soteriology
 *   — while extracting institutional authority, land, and economic resources
 *   from kami traditions to Buddhist establishments. The Meiji Restoration
 *   (1868) violently dismantled it, proving the world rearranges without it.
 *   This story captures the honji-suijaku monism reading specifically, not
 *   the broader shinbutsu-shugo phenomenon.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(kami_buddha_ontology__honji_suijaku_monism, 0.68).
domain_priors:suppression_score(kami_buddha_ontology__honji_suijaku_monism, 0.62).
domain_priors:theater_ratio(kami_buddha_ontology__honji_suijaku_monism, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(kami_buddha_ontology__honji_suijaku_monism, extractiveness, 0.68).
narrative_ontology:constraint_metric(kami_buddha_ontology__honji_suijaku_monism, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(kami_buddha_ontology__honji_suijaku_monism, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(kami_buddha_ontology__honji_suijaku_monism, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(kami_buddha_ontology__honji_suijaku_monism, resistance, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(kami_buddha_ontology__honji_suijaku_monism, tangled_rope).
narrative_ontology:human_readable(kami_buddha_ontology__honji_suijaku_monism, "Honji-Suijaku Ontological Monism (Kami as Buddhist Traces)").
narrative_ontology:topic_domain(kami_buddha_ontology__honji_suijaku_monism, "religious/philosophical/japanese_history").

domain_priors:requires_active_enforcement(kami_buddha_ontology__honji_suijaku_monism).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(kami_buddha_ontology__honji_suijaku_monism, 'd5385696-b72d-4382-8953-bbbb5e9201bd').
narrative_ontology:cs_kernel_codification('d5385696-b72d-4382-8953-bbbb5e9201bd', formalized).
narrative_ontology:cs_authority_grounding('d5385696-b72d-4382-8953-bbbb5e9201bd', lineage).
narrative_ontology:cs_interpretation_layer_present('d5385696-b72d-4382-8953-bbbb5e9201bd').
narrative_ontology:cs_reading_relation('d5385696-b72d-4382-8953-bbbb5e9201bd', kami_buddha_ontology__domain_partition, forecloses).
narrative_ontology:cs_reading_relation('d5385696-b72d-4382-8953-bbbb5e9201bd', kami_buddha_ontology__incoherent_bundle, influences).
narrative_ontology:cs_axiom('d5385696-b72d-4382-8953-bbbb5e9201bd', foundational, buddha_ontological_priority).
narrative_ontology:cs_axiom_status(buddha_ontological_priority, holdable).
narrative_ontology:cs_axiom_grounding('d5385696-b72d-4382-8953-bbbb5e9201bd', buddha_ontological_priority, theological).
narrative_ontology:cs_axiom('d5385696-b72d-4382-8953-bbbb5e9201bd', foundational, kami_as_suijaku_traces).
narrative_ontology:cs_axiom_status(kami_as_suijaku_traces, holdable).
narrative_ontology:cs_axiom_grounding('d5385696-b72d-4382-8953-bbbb5e9201bd', kami_as_suijaku_traces, theological).
narrative_ontology:cs_axiom('d5385696-b72d-4382-8953-bbbb5e9201bd', foundational, single_ultimate_reality).
narrative_ontology:cs_axiom_status(single_ultimate_reality, holdable).
narrative_ontology:cs_axiom_grounding('d5385696-b72d-4382-8953-bbbb5e9201bd', single_ultimate_reality, deontological).
narrative_ontology:cs_reference_frame('d5385696-b72d-4382-8953-bbbb5e9201bd', heian_buddhist_soteriology).
narrative_ontology:cs_drift_state('d5385696-b72d-4382-8953-bbbb5e9201bd', edo_shinto_revival, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('d5385696-b72d-4382-8953-bbbb5e9201bd', '').
narrative_ontology:cs_kernel_id(kami_buddha_ontology__honji_suijaku_monism, kami_buddha_ontology).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(kami_buddha_ontology__honji_suijaku_monism, buddhist_institutions).
narrative_ontology:constraint_beneficiary(kami_buddha_ontology__honji_suijaku_monism, buddhist_theologians).
narrative_ontology:constraint_beneficiary(kami_buddha_ontology__honji_suijaku_monism, imperial_court).
narrative_ontology:constraint_victim(kami_buddha_ontology__honji_suijaku_monism, shinto_priesthoods).
narrative_ontology:constraint_victim(kami_buddha_ontology__honji_suijaku_monism, local_kami_cults).
narrative_ontology:constraint_victim(kami_buddha_ontology__honji_suijaku_monism, folk_practitioners).
narrative_ontology:constraint_vindicates(kami_buddha_ontology__honji_suijaku_monism, buddha_ontological_priority).
narrative_ontology:constraint_vindicates(kami_buddha_ontology__honji_suijaku_monism, single_ultimate_reality).
narrative_ontology:constraint_vindicates(kami_buddha_ontology__honji_suijaku_monism, kami_as_suijaku_traces).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Major temple complexes (Tendai, Shingon, etc.) formulated and enforced honji-suijaku doctrine. They controlled doctrinal interpretation, ritual calendars, and access to Buddhist soteriology. The doctrine secured their institutional primacy over shrine networks and justified landholdings, tax exemptions, and political influence. Exit meant abandoning the entire Buddhist institutional structure — effectively impossible for the institution itself.
narrative_ontology:constraint_stakeholder(kami_buddha_ontology__honji_suijaku_monism, buddhist_institutions, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(kami_buddha_ontology__honji_suijaku_monism, buddhist_institutions, beneficiary).

% Monk-scholars (Saicho, Kukai, later medieval commentators) authored the theoretical systematization. Their intellectual authority and career advancement depended on the doctrine's acceptance. They gained prestige, patronage, and control of esoteric transmission lineages. Dissent within the Buddhist fold was possible but risky; exit to a non-Buddhist intellectual life meant losing their entire professional identity.
narrative_ontology:constraint_stakeholder(kami_buddha_ontology__honji_suijaku_monism, buddhist_theologians, beneficiary,
    organized, biographical, constrained, national).

% The court patronized both systems but increasingly relied on Buddhist ritual for state protection (chingo kokka). Honji-suijaku provided a unified ideological framework legitimizing imperial authority over both 'native' and 'foreign' cults. The court could arbitrate between institutions but depended on Buddhist ritual expertise. Exit from the framework would undermine the court's own sacral legitimacy.
narrative_ontology:constraint_stakeholder(kami_buddha_ontology__honji_suijaku_monism, imperial_court, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(kami_buddha_ontology__honji_suijaku_monism, imperial_court, beneficiary).

% Shrine lineages (Jingu, Kamo, Kasuga, etc.) were subordinated doctrinally: their kami declared 'traces' of Buddhist originals. They lost independent soteriological authority, were pressed into Buddhist ritual frameworks (sutra readings before kami), and saw resources diverted to temple-shrine complexes (jingū-ji). Some priestly families adopted Buddhist ordination to survive. Exit meant abandoning hereditary office and ancestral cult — identity-locked for most.
narrative_ontology:constraint_stakeholder(kami_buddha_ontology__honji_suijaku_monism, shinto_priesthoods, payer,
    organized, generational, constrained, regional).

% Village and regional kami practices (yama-no-kami, ujigami, etc.) were absorbed into the honji-suijaku hierarchy without consultation. Local festivals were Buddhistized; shrine lands were claimed by temples. Practitioners had no doctrinal voice and no institutional exit — their religious identity was constituted through the very kami now declared subordinate. Resistance took covert forms (hidden festivals, dual practice).
narrative_ontology:constraint_stakeholder(kami_buddha_ontology__honji_suijaku_monism, local_kami_cults, payer,
    powerless, biographical, identity_locked, local).
narrative_ontology:stakeholder_secondary_role(kami_buddha_ontology__honji_suijaku_monism, local_kami_cults, excluded).

% Ordinary people navigated a ritual world where birth/marriage were Shinto but death/memorial were Buddhist, all framed by honji-suijaku. They paid for both systems (shrine offerings, temple funerals, memorial services) with no say in the doctrinal framework. Exit from the ritual economy was socially impossible — it structured the entire lifecycle. Their 'consent' was structural, not chosen.
narrative_ontology:constraint_stakeholder(kami_buddha_ontology__honji_suijaku_monism, folk_practitioners, payer,
    powerless, immediate, trapped, local).

% Historians of Japanese religion, philosophers of religion, and scholars of syncretism analyze honji-suijaku as a historical doctrinal formation. They have no stake in its truth but their interpretive frameworks (e.g., 'syncretism,' 'combinatory religion,' 'ontological hierarchy') shape how the constraint is understood today. They can freely adopt or reject any reading.
narrative_ontology:constraint_stakeholder(kami_buddha_ontology__honji_suijaku_monism, modern_scholars, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provided a single unified ontological and soteriological framework integrating indigenous Japanese kami worship with imported Buddhist doctrine — solving the problem of how to worship kami without rejecting Buddhism, and how to practice Buddhism without denying the efficacy of kami. Created shared ritual calendars, unified pilgrimage circuits (e.g., Kumano), and a common symbolic vocabulary.
% TRANSFER_FUNCTION: Moved doctrinal primacy, ritual authority, land control, and economic resources from autonomous kami traditions to Buddhist institutions. Kami traditions supplied legitimacy, local embeddedness, and life-cycle ritual clientele; Buddhist institutions supplied soteriological theory, monastic organization, and state patronage access. The transfer was asymmetric: Buddhist terms set the framework.
% ABSENT_VOICES: Women's religious communities (nunneries, female mediums/miko), minority/outcaste groups whose kami were deemed 'low' in the hierarchy, peripheral regional cults never fully incorporated, and the kami themselves (in the indigenous understanding, kami are not passive traces but active agents — their 'voice' is excluded by the very ontology).
% DISAPPEARANCE_RATIONALE: The Meiji shinbutsu bunri (1868) empirically demonstrated this: when the state forcibly separated kami and buddhas, shrine lands were restored, Buddhist temples were destroyed or converted, priestly lineages were purged, and a new State Shinto ontology was imposed. The ritual economy, institutional geography, and sacred geography of Japan were fundamentally reorganized. The constraint's disappearance rearranged the world.
% FOUNDING_PROBLEM: How to incorporate the indigenous Japanese sacred landscape (kami) into the universal soteriology of Buddhism without denying the reality of either — specifically, how to explain kami's efficacy and presence if Buddhism is the ultimate truth, and how to practice Buddhism in Japan without rejecting the ancestral gods.
% FOUNDING_PROBLEM_CORROBORATION: Meiji reformers (Shinto theologians like Hirata Atsutane, state policymakers) explicitly declared the honji-suijaku problem 'solved' by separation — the founding problem of integration was replaced by a new problem of separation. Modern historians (Kuroda Toshio, Faure, Rambelli) corroborate that the integration problem was historically contingent and dissolved with the institutions that sustained it. No contemporary Buddhist institution maintains honji-suijaku as live doctrine.
narrative_ontology:disappearance_verdict(kami_buddha_ontology__honji_suijaku_monism, world_rearranges).
narrative_ontology:founding_problem_status(kami_buddha_ontology__honji_suijaku_monism, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(kami_buddha_ontology__honji_suijaku_monism, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(kami_buddha_ontology__honji_suijaku_monism, 'none', 1).
narrative_ontology:epsilon_provenance(kami_buddha_ontology__honji_suijaku_monism, 0.68, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(kami_buddha_ontology__honji_suijaku_monism_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(kami_buddha_ontology__honji_suijaku_monism, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(kami_buddha_ontology__honji_suijaku_monism_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68) is substantial: Buddhist institutions captured the doctrinal high ground, ritual control, and material resources of kami traditions for nearly a millennium. Suppression (0.62) is significant but not total: local kami practices persisted covertly, and the domain_partition reading remained alive in folk practice and certain shrine lineages. Theater ratio (0.28) reflects genuine coordination function (unified soteriology, ritual integration) alongside performative doctrinal enforcement — the system worked for participants even as it extracted. Accessibility collapse (0.55) is moderate: alternatives (pure Shinto, pure Buddhism, folk syncretism) existed but were marginalized by institutional power. Resistance (0.48) was real but mostly latent until the Meiji rupture.
 *
 * PERSPECTIVAL GAP:
 *   From the Buddhist institutional seat, honji-suijaku appears as genuine coordination — a compassionate upaya (skillful means) integrating kami into the Buddhist path. From the Shinto priesthood seat, it appears as enforced subordination — their ancestral deities demoted to 'traces' by a foreign system. From the folk practitioner seat, it appears as the only available ritual world — the distinction between coordination and extraction is invisible because exit is impossible. The engine computes this divergence from the structural data; the claimed_type (tangled_rope) reflects the analyst's judgment that BOTH coordination and extraction are structurally real.
 *
 * DIRECTIONALITY LOGIC:
 *   Buddhist institutions and theologians are structural beneficiaries (d ~0.15-0.25): they collect doctrinal authority, material resources, and state patronage from the arrangement. The imperial court is a dual beneficiary/agenda-setter (d ~0.2): it gains ideological unification but depends on Buddhist ritual expertise. Shinto priesthoods are payers with constrained exit (d ~0.75): they lose independent authority but retain institutional survival through subordination. Local kami cults and folk practitioners are payers with identity-locked/trapped exit (d ~0.85-0.95): their religious identity is constituted through the very kami declared subordinate. Modern scholars are analytical observers (d = 0.5).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (integrating kami and Buddhism) was historically resolved by the Meiji separation — the problem is dead but the arrangement persisted for centuries after its theoretical coherence fractured (Sengoku/Edo period saw competing honji-suijaku systems, Shinto revival movements). The constraint became a piton in its late phase: maintained by institutional inertia and state enforcement long after its integrative function degraded. The theater_ratio rise from 0.1 to 0.32 tracks this mandatrophy — performative enforcement replacing genuine coordination.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    ontological_vs_institutional_primacy,
    'Was honji-suijaku primarily an ontological claim (about what kami ARE) or an institutional strategy (about who controls ritual authority)?',
    'Compare early Heian theoretical texts (e.g., Kukai''s Sokushin Jobutsu Gi) with late Heian/Kamakura institutional records (temple-shrine land disputes, jingū-ji administration). If doctrinal elaboration precedes and exceeds institutional need, ontological; if doctrine tracks institutional expansion, strategic.',
    'If primarily institutional strategy, the constraint is a snare disguised as a mountain — extraction masked by ontological claim. If genuinely ontological, it is a tangled rope with sincere coordination function. Changes the claimed_type assessment and the extraction/coordination balance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ontological_vs_institutional_primacy, conceptual, 'Whether the ontological monism is the cause or the cover for institutional extraction.').

omega_variable(
    coordination_extraction_boundary_honji_suijaku,
    'Is the coordination function (unified soteriology, shared ritual) structurally separable from the extraction function (Buddhist primacy, resource capture), or are they inextricably fused?',
    'Examine historical moments when coordination was maintained but extraction reduced (e.g., certain shrine-temple complexes where kami rituals retained autonomy). If such cases exist stably, the functions are separable; if extraction always reasserts, they are fused.',
    'If separable, the constraint could have been a rope (coordination without extraction) — the extraction was contingent, not structural. If fused, tangled_rope is the only honest classification. Affects whether the Meiji separation was a ''fix'' or a destruction of genuine coordination.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coordination_extraction_boundary_honji_suijaku, conceptual, 'Whether the monism''s coordination and extraction components can be disentangled.').

omega_variable(
    committer_frame_underdetermination,
    'Does the honji_suijaku_monism reading genuinely foreclose the domain_partition reading, or do they operate at different levels (elite doctrine vs. lived practice) such that both can be ''true'' for different agents?',
    'Analyze whether any single historical agent/community held BOTH readings simultaneously without cognitive dissonance (e.g., a monk who theorized monism but practiced domain-partition rituals). If yes, forecloses is too strong; coexists_with or influences is appropriate.',
    'If they coexisted in practice, the kernel is not a clean logical fork but a stratified discourse. The engine''s foreclosure computation would overstate structural displacement. The reading_relations should be influences or coexists_with, not forecloses.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(committer_frame_underdetermination, empirical, 'Whether the declared foreclosure relation between monism and domain_partition reflects lived historical reality or only elite theoretical conflict.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(kami_buddha_ontology__honji_suijaku_monism, 794, 1868).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(honji_suijaku_tr_t794, kami_buddha_ontology__honji_suijaku_monism, theater_ratio, 794, 0.1).
narrative_ontology:measurement(honji_suijaku_tr_t950, kami_buddha_ontology__honji_suijaku_monism, theater_ratio, 950, 0.15).
narrative_ontology:measurement(honji_suijaku_tr_t1100, kami_buddha_ontology__honji_suijaku_monism, theater_ratio, 1100, 0.2).
narrative_ontology:measurement(honji_suijaku_tr_t1250, kami_buddha_ontology__honji_suijaku_monism, theater_ratio, 1250, 0.25).
narrative_ontology:measurement(honji_suijaku_tr_t1400, kami_buddha_ontology__honji_suijaku_monism, theater_ratio, 1400, 0.28).
narrative_ontology:measurement(honji_suijaku_tr_t1550, kami_buddha_ontology__honji_suijaku_monism, theater_ratio, 1550, 0.3).
narrative_ontology:measurement(honji_suijaku_tr_t1600, kami_buddha_ontology__honji_suijaku_monism, theater_ratio, 1600, 0.32).
narrative_ontology:measurement(honji_suijaku_tr_t1868, kami_buddha_ontology__honji_suijaku_monism, theater_ratio, 1868, 0.28).

% Extraction over time
narrative_ontology:measurement(honji_suijaku_be_t794, kami_buddha_ontology__honji_suijaku_monism, base_extractiveness, 794, 0.35).
narrative_ontology:measurement(honji_suijaku_be_t950, kami_buddha_ontology__honji_suijaku_monism, base_extractiveness, 950, 0.48).
narrative_ontology:measurement(honji_suijaku_be_t1100, kami_buddha_ontology__honji_suijaku_monism, base_extractiveness, 1100, 0.55).
narrative_ontology:measurement(honji_suijaku_be_t1250, kami_buddha_ontology__honji_suijaku_monism, base_extractiveness, 1250, 0.62).
narrative_ontology:measurement(honji_suijaku_be_t1400, kami_buddha_ontology__honji_suijaku_monism, base_extractiveness, 1400, 0.68).
narrative_ontology:measurement(honji_suijaku_be_t1550, kami_buddha_ontology__honji_suijaku_monism, base_extractiveness, 1550, 0.71).
narrative_ontology:measurement(honji_suijaku_be_t1600, kami_buddha_ontology__honji_suijaku_monism, base_extractiveness, 1600, 0.7).
narrative_ontology:measurement(honji_suijaku_be_t1868, kami_buddha_ontology__honji_suijaku_monism, base_extractiveness, 1868, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(honji_suijaku_su_t794, kami_buddha_ontology__honji_suijaku_monism, suppression_requirement, 794, 0.3).
narrative_ontology:measurement(honji_suijaku_su_t950, kami_buddha_ontology__honji_suijaku_monism, suppression_requirement, 950, 0.42).
narrative_ontology:measurement(honji_suijaku_su_t1100, kami_buddha_ontology__honji_suijaku_monism, suppression_requirement, 1100, 0.5).
narrative_ontology:measurement(honji_suijaku_su_t1250, kami_buddha_ontology__honji_suijaku_monism, suppression_requirement, 1250, 0.58).
narrative_ontology:measurement(honji_suijaku_su_t1400, kami_buddha_ontology__honji_suijaku_monism, suppression_requirement, 1400, 0.62).
narrative_ontology:measurement(honji_suijaku_su_t1550, kami_buddha_ontology__honji_suijaku_monism, suppression_requirement, 1550, 0.65).
narrative_ontology:measurement(honji_suijaku_su_t1600, kami_buddha_ontology__honji_suijaku_monism, suppression_requirement, 1600, 0.64).
narrative_ontology:measurement(honji_suijaku_su_t1868, kami_buddha_ontology__honji_suijaku_monism, suppression_requirement, 1868, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(kami_buddha_ontology__honji_suijaku_monism, identity_coordination).
narrative_ontology:boltzmann_floor_override(kami_buddha_ontology__honji_suijaku_monism, 0.08).
narrative_ontology:affects_constraint(kami_buddha_ontology__honji_suijaku_monism, kami_buddha_ontology__domain_partition).
narrative_ontology:affects_constraint(kami_buddha_ontology__honji_suijaku_monism, kami_buddha_ontology__incoherent_bundle).
narrative_ontology:affects_constraint(kami_buddha_ontology__honji_suijaku_monism, meiji_shinbutsu_bunri).
narrative_ontology:affects_constraint(kami_buddha_ontology__honji_suijaku_monism, state_shinto_ontology).

% DUAL FORMULATION NOTE:
% Part of the kami_buddha_ontology constraint family. This reading (honji_suijaku_monism) asserts ontological identity with Buddhist priority. The domain_partition reading asserts ontological distinction with functional separation. The incoherent_bundle reading denies the kernel's coherence entirely. All three share the same referent (Japanese religio-historical formation) but instantiate different constraints with different ε, beneficiaries, and types. The monism reading historically dominated (794-1868) and structurally influenced the others.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(kami_buddha_ontology__honji_suijaku_monism, institutional, 0.15).
constraint_indexing:directionality_override(kami_buddha_ontology__honji_suijaku_monism, organized, 0.35).
constraint_indexing:directionality_override(kami_buddha_ontology__honji_suijaku_monism, powerless, 0.9).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
