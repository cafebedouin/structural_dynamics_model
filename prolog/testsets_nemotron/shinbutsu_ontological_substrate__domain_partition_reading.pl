% ============================================================================
% CONSTRAINT STORY: shinbutsu_ontological_substrate__domain_partition_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-07-25
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_shinbutsu_ontological_substrate__domain_partition_reading, []).

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
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
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
 *   constraint_id: shinbutsu_ontological_substrate__domain_partition_reading
 *   human_readable: Shinbutsu Domain Partition (This-World / Afterlife)
 *   domain: religious_studies/japanese_history/commitment_systems
 *
 * SUMMARY:
 *   This constraint story instantiates the domain_partition_reading of the
 *   shinbutsu_ontological_substrate kernel. It asserts that kami (Shinto
 *   deities) and buddhas (Buddhist deities) governed separate but
 *   complementary domains — kami overseeing this-worldly affairs
 *   (agriculture, clan welfare, local territory) and buddhas overseeing
 *   afterlife/soteriological matters — with coexistence being a functional,
 *   pragmatic arrangement rather than an ontological unity. The honji suijaku
 *   (original ground / trace manifestation) framework is read here as an
 *   institutional accommodation that allowed two ritual economies to operate
 *   without conflict, not as a metaphysical claim of identity. The
 *   constraint's extraction is low during its stable period because both
 *   establishments benefited from the domain partition: shrines maintained
 *   local authority and land rights; temples controlled funerary and memorial
 *   revenue; communities received comprehensive ritual coverage. The Meiji
 *   separation (1868-1872) represents a dramatic spike in extraction and
 *   suppression as the state forcibly disentangled what the reading claims
 *   was easily separable.
 *
 * KEY AGENTS:
 *   - shinto_shrine_establishments: Primary beneficiary (this-world domain) — controlled local ritual economies, land, clan patronage
 *   - buddhist_temple_establishments: Primary beneficiary (afterlife domain) — controlled funerary revenue, memorial services, soteriological authority
 *   - local_communities: Beneficiary — received comprehensive ritual coverage across life domains without duplication
 *   - imperial_court_heian: Agenda setter (early) — patronized both systems, managed jurisdictional boundaries
 *   - tokugawa_shogunate: Agenda setter (late) — enforced temple registration (terauke) system that institutionalized the partition
 *   - meiji_state: Excluded/agenda setter (disruption) — forcibly separated the domains, destroyed the coexistence arrangement
 *   - honji_suijaku_theologians: Observer — articulated the doctrinal framework that stabilized the partition
 *   - modern_religious_studies_scholars: Observer — analytical seat evaluating the kernel's readings
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(shinbutsu_ontological_substrate__domain_partition_reading, 0.15).
domain_priors:suppression_score(shinbutsu_ontological_substrate__domain_partition_reading, 0.05).
domain_priors:theater_ratio(shinbutsu_ontological_substrate__domain_partition_reading, 0.12).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(shinbutsu_ontological_substrate__domain_partition_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(shinbutsu_ontological_substrate__domain_partition_reading, suppression_requirement, 0.05).
narrative_ontology:constraint_metric(shinbutsu_ontological_substrate__domain_partition_reading, theater_ratio, 0.12).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(shinbutsu_ontological_substrate__domain_partition_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(shinbutsu_ontological_substrate__domain_partition_reading, resistance, 0.25).

% --- Constraint claim ---
narrative_ontology:constraint_claim(shinbutsu_ontological_substrate__domain_partition_reading, rope).
narrative_ontology:human_readable(shinbutsu_ontological_substrate__domain_partition_reading, "Shinbutsu Domain Partition (This-World / Afterlife)").
narrative_ontology:topic_domain(shinbutsu_ontological_substrate__domain_partition_reading, "religious_studies/japanese_history/commitment_systems").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(shinbutsu_ontological_substrate__domain_partition_reading, 'c59bf0fb-7e67-4b87-a4fd-08ec3a0dbd42').
narrative_ontology:cs_kernel_codification('c59bf0fb-7e67-4b87-a4fd-08ec3a0dbd42', distributed).
narrative_ontology:cs_authority_grounding('c59bf0fb-7e67-4b87-a4fd-08ec3a0dbd42', practice).
narrative_ontology:cs_reading_relation('c59bf0fb-7e67-4b87-a4fd-08ec3a0dbd42', shinbutsu_ontological_substrate__syncretic_fusion_reading, coexists_with).
narrative_ontology:cs_reading_relation('c59bf0fb-7e67-4b87-a4fd-08ec3a0dbd42', shinbutsu_ontological_substrate__incoherent_bundle_reading, coexists_with).
narrative_ontology:cs_axiom('c59bf0fb-7e67-4b87-a4fd-08ec3a0dbd42', foundational, kami_buddha_domain_separation_functional).
narrative_ontology:cs_axiom_status(kami_buddha_domain_separation_functional, holdable).
narrative_ontology:cs_axiom_grounding('c59bf0fb-7e67-4b87-a4fd-08ec3a0dbd42', kami_buddha_domain_separation_functional, conventional).
narrative_ontology:cs_axiom('c59bf0fb-7e67-4b87-a4fd-08ec3a0dbd42', secondary, honji_suijaku_as_institutional_accommodation).
narrative_ontology:cs_axiom_status(honji_suijaku_as_institutional_accommodation, holdable).
narrative_ontology:cs_axiom_grounding('c59bf0fb-7e67-4b87-a4fd-08ec3a0dbd42', honji_suijaku_as_institutional_accommodation, conventional).
narrative_ontology:cs_reference_frame('c59bf0fb-7e67-4b87-a4fd-08ec3a0dbd42', pre_meiji_honji_suijaku_coexistence).
narrative_ontology:cs_drift_state('c59bf0fb-7e67-4b87-a4fd-08ec3a0dbd42', tokugawa_late_period, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('c59bf0fb-7e67-4b87-a4fd-08ec3a0dbd42', '').
narrative_ontology:cs_kernel_id(shinbutsu_ontological_substrate__domain_partition_reading, shinbutsu_ontological_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(shinbutsu_ontological_substrate__domain_partition_reading, shinto_shrine_establishments).
narrative_ontology:constraint_beneficiary(shinbutsu_ontological_substrate__domain_partition_reading, buddhist_temple_establishments).
narrative_ontology:constraint_beneficiary(shinbutsu_ontological_substrate__domain_partition_reading, local_communities).
narrative_ontology:constraint_vindicates(shinbutsu_ontological_substrate__domain_partition_reading, kami_buddha_domain_separation_doctrine).
narrative_ontology:constraint_vindicates(shinbutsu_ontological_substrate__domain_partition_reading, honji_suijaku_as_pragmatic_accommodation).
narrative_ontology:constraint_vindicates(shinbutsu_ontological_substrate__domain_partition_reading, this_world_afterlife_governance_partition).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Control local ritual economies: agricultural festivals, clan patronage, territorial protection. Receive land donations, offerings, and political legitimacy from local elites. The domain partition protects their this-world jurisdiction from Buddhist encroachment. Exit would mean losing established parishioner bases and land rights — constrained by institutional history and local embeddedness.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_substrate__domain_partition_reading, shinto_shrine_establishments, beneficiary,
    organized, generational, constrained, local).

% Control afterlife/soteriological domain: funerals, memorial services, ancestral rites, salvation doctrine. Receive funerary revenue, memorial endowments, and state registration fees (terauke). The domain partition protects their afterlife monopoly from Shinto competition. Exit would mean losing the funerary economy and state recognition — constrained by the terauke system's institutional lock-in.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_substrate__domain_partition_reading, buddhist_temple_establishments, beneficiary,
    organized, generational, constrained, regional).

% Receive comprehensive ritual coverage: shrines for this-world needs (harvest, disease, clan continuity), temples for afterlife needs (funerals, ancestors, salvation). Pay offerings and fees to both but avoid duplication. Can shift allegiance locally (mobile exit) but the dual system is the only available ritual economy — no secular alternative exists in period.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_substrate__domain_partition_reading, local_communities, beneficiary,
    moderate, biographical, mobile, local).

% Patronizes both shrine and temple establishments; manages jurisdictional boundaries through court ritual calendar and appointment of ritual officials. Benefits from dual legitimization (Shinto for imperial ancestry, Buddhism for state protection). Can arbitrate disputes between establishments. Exit options are arbitrage-level — the court sits above both systems.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_substrate__domain_partition_reading, imperial_court_heian, agenda_setter,
    institutional, generational, arbitrage, national).

% Enforces temple registration system (terauke) requiring every household to register with a Buddhist temple, effectively making temples state agents for population control and anti-Christian suppression. Simultaneously patronizes major shrines (Ise, Hachiman) for political legitimacy. The shogunate administers the domain partition as governance infrastructure. Exit is arbitrage — the state controls both systems.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_substrate__domain_partition_reading, tokugawa_shogunate, agenda_setter,
    institutional, generational, arbitrage, national).

% Initially excluded from the shinbutsu arrangement (the Tokugawa system managed it). Becomes agenda setter through revolutionary force (1868). Views the honji suijaku coexistence as feudal superstition obstructing modern nation-state formation. Imposes shinbutsu bunri (separation) by decree: destroys temples, defrocks priests, seizes land, creates State Shinto. The separation's violence tests the domain partition reading's claim of 'easy separation.'
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_substrate__domain_partition_reading, meiji_state, excluded,
    institutional, biographical, arbitrage, national).

% Articulate the doctrinal framework (honji suijaku) that stabilizes the partition: buddhas are the 'original ground' (honji), kami are 'trace manifestations' (suijaku) adapted to Japanese conditions. This reading treats the framework as pragmatic accommodation, not metaphysical truth. They observe the system from within the Buddhist intellectual tradition; their exit is analytical (they can reinterpret).
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_substrate__domain_partition_reading, honji_suijaku_theologians, observer,
    moderate, generational, analytical, national).

% Analyze the shinbutsu relationship from outside the commitment system. Competing interpretations map to the three kernel readings: domain partition (Kuroda Toshio, Hardacre), syncretic fusion (traditional honji suijaku scholarship), incoherent bundle (critical religion studies). They hold analytical exit and civilizational time horizon.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_substrate__domain_partition_reading, modern_religious_studies_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(shinbutsu_ontological_substrate__domain_partition_reading, diffuse).
narrative_ontology:fixing_cost_class(shinbutsu_ontological_substrate__domain_partition_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the problem of comprehensive ritual coverage across two distinct life domains — this-worldly welfare (agriculture, health, clan continuity) and afterlife/soteriological needs (funerals, ancestors, salvation) — by partitioning jurisdiction between two specialized ritual economies (Shinto shrines and Buddhist temples) rather than forcing one system to cover both poorly or competing destructively.
% TRANSFER_FUNCTION: Moves ritual authority, land revenue, and parishioner allegiance: shrines receive this-world offerings and land rights; temples receive funerary fees and memorial endowments; the state (Tokugawa) receives population registration and ideological control via terauke. No direct transfer between shrines and temples — the partition prevents transfer by assigning non-overlapping domains.
% ABSENT_VOICES: Commoners who might have preferred a single, simpler ritual system; heterodox Buddhist sects (e.g., Jodo Shinshu) that resisted terauke registration; crypto-Christians who used the shinbutsu ambiguity for cover; women and outcaste communities whose ritual needs were marginalized by both establishments. These voices are structurally excluded from the institutional negotiation.
% DISAPPEARANCE_RATIONALE: If the domain partition vanished overnight (as it effectively did in 1868-1872), the ritual economy would reorganize violently: shrines lose afterlife revenue they never had but gain state patronage; temples lose state registration monopoly and funerary revenue; communities lose dual coverage and face State Shinto imposition; new religious movements emerge to fill gaps. The Meiji separation demonstrates this rearrangement empirically.
% FOUNDING_PROBLEM: Early Japanese communities needed ritual coverage for both this-worldly agricultural/clan continuity and afterlife/salvation concerns. Neither the indigenous kami cults nor imported Buddhism alone provided comprehensive coverage. The domain partition emerged as a functional solution: kami for this world, buddhas for the next, with honji suijaku as the doctrinal glue.
% FOUNDING_PROBLEM_CORROBORATION: Kuroda Toshio (historian, non-sectarian) attests the domain partition solved a genuine coordination problem for medieval communities. Hardacre (anthropologist, outside Buddhist/Shinto establishments) confirms the functional complementarity. The Shinto and Buddhist establishments themselves attest the problem is live (both still operate), but their self-interest is evident. No purely internal corroboration exists — the founding problem's persistence is attested by external scholars and the contemporary coexistence pattern.
narrative_ontology:disappearance_verdict(shinbutsu_ontological_substrate__domain_partition_reading, world_rearranges).
narrative_ontology:founding_problem_status(shinbutsu_ontological_substrate__domain_partition_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(shinbutsu_ontological_substrate__domain_partition_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(shinbutsu_ontological_substrate__domain_partition_reading, 'none', 1).
narrative_ontology:epsilon_provenance(shinbutsu_ontological_substrate__domain_partition_reading, 0.15, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(shinbutsu_ontological_substrate__domain_partition_reading_tests).
:- end_tests(shinbutsu_ontological_substrate__domain_partition_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness (0.15 at interval midpoint ~1250) reflects the domain partition reading's core claim: the arrangement was mutually beneficial coordination, not extraction. The low suppression (0.05) reflects minimal coercion needed to maintain the partition during its stable centuries — the terauke system managed boundaries administratively, not violently. Theater ratio (0.12) is low because the honji suijaku framework performed genuine doctrinal work stabilizing the coexistence. The temporal measurements show a long stable period (750-1750) with gradual drift toward entanglement (rising extractiveness, theater, suppression) as institutional boundaries blurred, then a catastrophic Meiji spike (1868-1872) where state coercion forcibly separated the domains — the separation's violence is the domain partition reading's counterfactual test: if coexistence was truly functional and non-ontological, separation should have been easy. The spike suggests otherwise, which the omega 'separation_feasibility' captures. Post-Meiji (1890), extractiveness and theater collapse as the new state-enforced separation stabilizes.
 *
 * PERSPECTIVAL GAP:
 *   The shrine and temple establishments both experience this as coordination (rope) from their seats — each gets its protected domain. Local communities experience it as beneficial coverage. The Meiji state experiences it as an obstacle to modernization (extraction target). Modern scholars disagree on which reading is structurally true. The engine computes per-seat types from the structural data; this story authors the domain-partition reading's structural claims.
 *
 * DIRECTIONALITY LOGIC:
 *   Shrine and temple establishments are structural beneficiaries (d near 0.0) — they collect protected revenue streams and authority. Local communities are near-symmetric beneficiaries (d ~ 0.3) — they gain comprehensive ritual services with minimal cost. The Meiji state is an excluded agent that becomes an agenda setter through force (d near 1.0 during separation). The terauke system under Tokugawa gave temples state-backed monopoly on funerary registration, making them more institutional beneficiaries; shrines retained local autonomy. No victims are declared for the stable period — this is the reading's claim.
 *
 * MANDATROPHY ANALYSIS:
 *   The domain partition reading presents the pre-Meiji arrangement as solving a genuine coordination problem: two ritual economies with different specializations avoiding destructive competition. The founding problem (comprehensive ritual coverage across this-world and afterlife domains) remained live throughout the period — communities needed both. The arrangement did not outlive its function; it was forcibly terminated by an external actor (Meiji state) pursuing a different agenda (national unification under State Shinto). This is not mandatrophy (internal decay) but exogenous destruction. The contemporary coexistence pattern (post-1945) reinstantiates a version of the domain partition without state enforcement.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_disambiguation,
    'Is the shinbutsu relationship a single kernel with multiple readings, or are these structurally distinct constraints that share only a colloquial label?',
    'Decompose the domain partition reading into its own ε-invariant constraint story and verify whether ε differs substantially from the syncretic fusion and incoherent bundle readings. If ε differs, they are distinct constraints linked by network.affects_constraints.',
    'If ε is invariant per reading, the decomposition is validated. If ε varies with measurement basis, the kernel label conflates multiple constraints and further decomposition is needed.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_disambiguation, conceptual, 'Commitment-system kernel vs. constraint family distinction for shinbutsu arrangements').

omega_variable(
    separation_feasibility,
    'Was the Meiji-era shinbutsu bunri (separation) genuinely easy, or did it require substantial state coercion that the domain-partition reading downplays?',
    'Historical analysis of the separation edicts'' implementation: temple destruction, priest defrocking, and community resistance. Compare institutional entanglement metrics pre-1868 vs. post-1872.',
    'If separation required high coercion, the domain partition reading''s claim of ''functional, not ontological'' coexistence is falsified — the institutional entanglement was deeper than pragmatic coexistence suggests. This would shift the reading toward syncretic_fusion or incoherent_bundle.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(separation_feasibility, empirical, 'Whether the domain partition reading accurately represents historical institutional entanglement').

omega_variable(
    extraction_referent,
    'For this reading, what is the standing arrangement under contest that ε refers to — the pre-Meiji honji suijaku system, the Meiji separation itself, or the contemporary coexistence pattern?',
    'Clarify the temporal referent in the reading''s own lights. The domain partition reading presents pre-Meiji arrangement as pragmatic coexistence; ε should assess that arrangement''s extractiveness from the reading''s perspective.',
    'If ε refers to the pre-Meiji system, the low extractiveness score (0.15) reflects the reading''s claim of mutual benefit. If ε refers to Meiji separation, the coercion would raise extraction. The reading must be consistent.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(extraction_referent, conceptual, 'ε referent fixation for kernel-reading stories').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(shinbutsu_ontological_substrate__domain_partition_reading, 750, 1890).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(shin_tr_t750, shinbutsu_ontological_substrate__domain_partition_reading, theater_ratio, 750, 0.05).
narrative_ontology:measurement(shin_tr_t850, shinbutsu_ontological_substrate__domain_partition_reading, theater_ratio, 850, 0.08).
narrative_ontology:measurement(shin_tr_t950, shinbutsu_ontological_substrate__domain_partition_reading, theater_ratio, 950, 0.1).
narrative_ontology:measurement(shin_tr_t1050, shinbutsu_ontological_substrate__domain_partition_reading, theater_ratio, 1050, 0.1).
narrative_ontology:measurement(shin_tr_t1150, shinbutsu_ontological_substrate__domain_partition_reading, theater_ratio, 1150, 0.12).
narrative_ontology:measurement(shin_tr_t1250, shinbutsu_ontological_substrate__domain_partition_reading, theater_ratio, 1250, 0.15).
narrative_ontology:measurement(shin_tr_t1350, shinbutsu_ontological_substrate__domain_partition_reading, theater_ratio, 1350, 0.18).
narrative_ontology:measurement(shin_tr_t1450, shinbutsu_ontological_substrate__domain_partition_reading, theater_ratio, 1450, 0.2).
narrative_ontology:measurement(shin_tr_t1550, shinbutsu_ontological_substrate__domain_partition_reading, theater_ratio, 1550, 0.25).
narrative_ontology:measurement(shin_tr_t1650, shinbutsu_ontological_substrate__domain_partition_reading, theater_ratio, 1650, 0.3).
narrative_ontology:measurement(shin_tr_t1750, shinbutsu_ontological_substrate__domain_partition_reading, theater_ratio, 1750, 0.35).
narrative_ontology:measurement(shin_tr_t1850, shinbutsu_ontological_substrate__domain_partition_reading, theater_ratio, 1850, 0.4).
narrative_ontology:measurement(shin_tr_t1872, shinbutsu_ontological_substrate__domain_partition_reading, theater_ratio, 1872, 0.6).
narrative_ontology:measurement(shin_tr_t1890, shinbutsu_ontological_substrate__domain_partition_reading, theater_ratio, 1890, 0.1).

% Extraction over time
narrative_ontology:measurement(shin_be_t750, shinbutsu_ontological_substrate__domain_partition_reading, base_extractiveness, 750, 0.1).
narrative_ontology:measurement(shin_be_t850, shinbutsu_ontological_substrate__domain_partition_reading, base_extractiveness, 850, 0.12).
narrative_ontology:measurement(shin_be_t950, shinbutsu_ontological_substrate__domain_partition_reading, base_extractiveness, 950, 0.15).
narrative_ontology:measurement(shin_be_t1050, shinbutsu_ontological_substrate__domain_partition_reading, base_extractiveness, 1050, 0.15).
narrative_ontology:measurement(shin_be_t1150, shinbutsu_ontological_substrate__domain_partition_reading, base_extractiveness, 1150, 0.18).
narrative_ontology:measurement(shin_be_t1250, shinbutsu_ontological_substrate__domain_partition_reading, base_extractiveness, 1250, 0.2).
narrative_ontology:measurement(shin_be_t1350, shinbutsu_ontological_substrate__domain_partition_reading, base_extractiveness, 1350, 0.22).
narrative_ontology:measurement(shin_be_t1450, shinbutsu_ontological_substrate__domain_partition_reading, base_extractiveness, 1450, 0.25).
narrative_ontology:measurement(shin_be_t1550, shinbutsu_ontological_substrate__domain_partition_reading, base_extractiveness, 1550, 0.3).
narrative_ontology:measurement(shin_be_t1650, shinbutsu_ontological_substrate__domain_partition_reading, base_extractiveness, 1650, 0.35).
narrative_ontology:measurement(shin_be_t1750, shinbutsu_ontological_substrate__domain_partition_reading, base_extractiveness, 1750, 0.4).
narrative_ontology:measurement(shin_be_t1850, shinbutsu_ontological_substrate__domain_partition_reading, base_extractiveness, 1850, 0.55).
narrative_ontology:measurement(shin_be_t1872, shinbutsu_ontological_substrate__domain_partition_reading, base_extractiveness, 1872, 0.65).
narrative_ontology:measurement(shin_be_t1890, shinbutsu_ontological_substrate__domain_partition_reading, base_extractiveness, 1890, 0.2).

% Suppression requirement over time
narrative_ontology:measurement(shin_su_t750, shinbutsu_ontological_substrate__domain_partition_reading, suppression_requirement, 750, 0.02).
narrative_ontology:measurement(shin_su_t850, shinbutsu_ontological_substrate__domain_partition_reading, suppression_requirement, 850, 0.03).
narrative_ontology:measurement(shin_su_t950, shinbutsu_ontological_substrate__domain_partition_reading, suppression_requirement, 950, 0.05).
narrative_ontology:measurement(shin_su_t1050, shinbutsu_ontological_substrate__domain_partition_reading, suppression_requirement, 1050, 0.05).
narrative_ontology:measurement(shin_su_t1150, shinbutsu_ontological_substrate__domain_partition_reading, suppression_requirement, 1150, 0.08).
narrative_ontology:measurement(shin_su_t1250, shinbutsu_ontological_substrate__domain_partition_reading, suppression_requirement, 1250, 0.1).
narrative_ontology:measurement(shin_su_t1350, shinbutsu_ontological_substrate__domain_partition_reading, suppression_requirement, 1350, 0.15).
narrative_ontology:measurement(shin_su_t1450, shinbutsu_ontological_substrate__domain_partition_reading, suppression_requirement, 1450, 0.2).
narrative_ontology:measurement(shin_su_t1550, shinbutsu_ontological_substrate__domain_partition_reading, suppression_requirement, 1550, 0.25).
narrative_ontology:measurement(shin_su_t1650, shinbutsu_ontological_substrate__domain_partition_reading, suppression_requirement, 1650, 0.3).
narrative_ontology:measurement(shin_su_t1750, shinbutsu_ontological_substrate__domain_partition_reading, suppression_requirement, 1750, 0.35).
narrative_ontology:measurement(shin_su_t1850, shinbutsu_ontological_substrate__domain_partition_reading, suppression_requirement, 1850, 0.5).
narrative_ontology:measurement(shin_su_t1872, shinbutsu_ontological_substrate__domain_partition_reading, suppression_requirement, 1872, 0.9).
narrative_ontology:measurement(shin_su_t1890, shinbutsu_ontological_substrate__domain_partition_reading, suppression_requirement, 1890, 0.05).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(shinbutsu_ontological_substrate__domain_partition_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(shinbutsu_ontological_substrate__domain_partition_reading, 0.08).
narrative_ontology:affects_constraint(shinbutsu_ontological_substrate__domain_partition_reading, shinbutsu_ontological_substrate__syncretic_fusion_reading).
narrative_ontology:affects_constraint(shinbutsu_ontological_substrate__domain_partition_reading, shinbutsu_ontological_substrate__incoherent_bundle_reading).

% DUAL FORMULATION NOTE:
% This story decomposes the shinbutsu_ontological_substrate kernel into three structurally distinct constraints per the ε-invariance principle. The domain_partition_reading has low ε (0.15) and claims rope (mutual coordination). The syncretic_fusion_reading would have higher ε (ontological unity claim requires doctrinal enforcement) and likely classifies as tangled_rope. The incoherent_bundle_reading would have highest ε (state-enforced drift) and likely classifies as snare or piton. Each reading gets its own ε, stakeholders, and classification; they are linked via network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
