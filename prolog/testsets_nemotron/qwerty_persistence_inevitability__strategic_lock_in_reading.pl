% ============================================================================
% CONSTRAINT STORY: qwerty_persistence_inevitability__strategic_lock_in_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_qwerty_persistence_inevitability__strategic_lock_in_reading, []).

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
 *   constraint_id: qwerty_persistence_inevitability__strategic_lock_in_reading
 *   human_readable: QWERTY Persistence as Manufacturer-Engineered Lock-In
 *   domain: technology_history/political_economy/institutional_analysis
 *
 * SUMMARY:
 *   This constraint story represents the strategic_lock_in_reading of the
 *   contested kernel qwerty_persistence_inevitability. The kernel is the
 *   observed fact that QWERTY has persisted as the dominant keyboard layout
 *   for 150+ years despite demonstrated ergonomic inferiority. This reading
 *   holds that the persistence is not accidental path dependence but the
 *   result of deliberate cartel engineering: the 1893 typewriter trust
 *   (Remington, Caligraph, Yost, Smith Premier, Densmore) formed a
 *   standardization cartel that jointly funded touch-typing schools, lobbied
 *   for civil service and corporate procurement mandates, and created a
 *   training/certification ecosystem that locked in QWERTY as the only viable
 *   standard. The sibling reading (path_dependency_reading) argues QWERTY's
 *   dominance emerged from early accidents and self-reinforcing network
 *   effects without strategic coordination. These are distinct constraints
 *   with different beneficiary/victim structures, different extractiveness
 *   profiles, and different policy implications.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(qwerty_persistence_inevitability__strategic_lock_in_reading, 0.68).
domain_priors:suppression_score(qwerty_persistence_inevitability__strategic_lock_in_reading, 0.75).
domain_priors:theater_ratio(qwerty_persistence_inevitability__strategic_lock_in_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(qwerty_persistence_inevitability__strategic_lock_in_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(qwerty_persistence_inevitability__strategic_lock_in_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(qwerty_persistence_inevitability__strategic_lock_in_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(qwerty_persistence_inevitability__strategic_lock_in_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(qwerty_persistence_inevitability__strategic_lock_in_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(qwerty_persistence_inevitability__strategic_lock_in_reading, tangled_rope).
narrative_ontology:human_readable(qwerty_persistence_inevitability__strategic_lock_in_reading, "QWERTY Persistence as Manufacturer-Engineered Lock-In").
narrative_ontology:topic_domain(qwerty_persistence_inevitability__strategic_lock_in_reading, "technology_history/political_economy/institutional_analysis").

domain_priors:requires_active_enforcement(qwerty_persistence_inevitability__strategic_lock_in_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(qwerty_persistence_inevitability__strategic_lock_in_reading, 'bb5db844-de2f-42e2-961c-bf7592b2df0e').
narrative_ontology:cs_kernel_codification('bb5db844-de2f-42e2-961c-bf7592b2df0e', distributed).
narrative_ontology:cs_authority_grounding('bb5db844-de2f-42e2-961c-bf7592b2df0e', practice).
narrative_ontology:cs_interpretation_layer_present('bb5db844-de2f-42e2-961c-bf7592b2df0e').
narrative_ontology:cs_reading_relation('bb5db844-de2f-42e2-961c-bf7592b2df0e', qwerty_persistence_inevitability__path_dependency_reading, coexists_with).
narrative_ontology:cs_axiom('bb5db844-de2f-42e2-961c-bf7592b2df0e', foundational, cartel_designed_lock_in).
narrative_ontology:cs_axiom_status(cartel_designed_lock_in, holdable).
narrative_ontology:cs_axiom_grounding('bb5db844-de2f-42e2-961c-bf7592b2df0e', cartel_designed_lock_in, empirically_contingent).
narrative_ontology:cs_axiom('bb5db844-de2f-42e2-961c-bf7592b2df0e', foundational, training_ecosystem_as_extraction_infrastructure).
narrative_ontology:cs_axiom_status(training_ecosystem_as_extraction_infrastructure, holdable).
narrative_ontology:cs_axiom_grounding('bb5db844-de2f-42e2-961c-bf7592b2df0e', training_ecosystem_as_extraction_infrastructure, empirically_contingent).
narrative_ontology:cs_axiom('bb5db844-de2f-42e2-961c-bf7592b2df0e', secondary, identity_locked_human_capital_as_barrier).
narrative_ontology:cs_axiom_status(identity_locked_human_capital_as_barrier, holdable).
narrative_ontology:cs_axiom_grounding('bb5db844-de2f-42e2-961c-bf7592b2df0e', identity_locked_human_capital_as_barrier, instrumental).
narrative_ontology:cs_reference_frame('bb5db844-de2f-42e2-961c-bf7592b2df0e', pre_cartel_fragmented_market).
narrative_ontology:cs_drift_state('bb5db844-de2f-42e2-961c-bf7592b2df0e', post_dvorak_challenge, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('bb5db844-de2f-42e2-961c-bf7592b2df0e', '2026-08-05T14:32:17Z').
narrative_ontology:cs_kernel_id(qwerty_persistence_inevitability__strategic_lock_in_reading, qwerty_persistence_inevitability).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(qwerty_persistence_inevitability__strategic_lock_in_reading, remington_typewriter_cartel_1893).
narrative_ontology:constraint_beneficiary(qwerty_persistence_inevitability__strategic_lock_in_reading, touch_typing_institute_network).
narrative_ontology:constraint_beneficiary(qwerty_persistence_inevitability__strategic_lock_in_reading, qwerty_equipment_manufacturers).
narrative_ontology:constraint_victim(qwerty_persistence_inevitability__strategic_lock_in_reading, professional_typists).
narrative_ontology:constraint_victim(qwerty_persistence_inevitability__strategic_lock_in_reading, general_typing_population).
narrative_ontology:constraint_victim(qwerty_persistence_inevitability__strategic_lock_in_reading, alternative_layout_innovators).
narrative_ontology:constraint_vindicates(qwerty_persistence_inevitability__strategic_lock_in_reading, manufactured_inevitability_doctrine).
narrative_ontology:constraint_vindicates(qwerty_persistence_inevitability__strategic_lock_in_reading, standardization_as_rent_extraction).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Formed the 1893 typewriter trust controlling 85% of US production; jointly funded touch-typing schools and certification systems that trained operators exclusively on QWERTY; controlled keyboard layout standardization through exclusive contracts with major businesses and government offices; extracted monopoly rents from both hardware sales and the training ecosystem they created.
narrative_ontology:constraint_stakeholder(qwerty_persistence_inevitability__strategic_lock_in_reading, remington_typewriter_cartel_1893, agenda_setter,
    institutional, generational, arbitrage, global).

% Network of commercial typing schools (Underwood, Remington, Caligraph-affiliated) that standardized curricula on QWERTY; collected tuition and certification fees; lobbied civil service and corporate HR departments to require QWERTY certification; their business model depended entirely on QWERTY's monopoly status — retraining to a new layout would destroy their curriculum investment and certification franchise.
narrative_ontology:constraint_stakeholder(qwerty_persistence_inevitability__strategic_lock_in_reading, touch_typing_institute_network, beneficiary,
    organized, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(qwerty_persistence_inevitability__strategic_lock_in_reading, touch_typing_institute_network, agenda_setter).

% Typewriter manufacturers outside the 1893 cartel (Royal, Smith Corona, later IBM) who adopted QWERTY to access the trained operator pool; benefited from network effects without bearing cartel formation costs; later extended the standard to computer keyboards, cementing lock-in across technology generations.
narrative_ontology:constraint_stakeholder(qwerty_persistence_inevitability__strategic_lock_in_reading, qwerty_equipment_manufacturers, beneficiary,
    powerful, biographical, constrained, global).

% Clerical workforce (overwhelmingly women by 1910) who invested hundreds of hours mastering QWERTY touch-typing; their professional identity, certification, and employment prospects were fused to the layout; retraining meant income loss, status degradation, and re-certification barriers; ergonomic costs (RSI, fatigue) were borne individually with no compensation.
narrative_ontology:constraint_stakeholder(qwerty_persistence_inevitability__strategic_lock_in_reading, professional_typists, payer,
    moderate, biographical, identity_locked, global).

% All subsequent keyboard users (students, office workers, home computer users) who inherited QWERTY as the only available standard; bear ongoing ergonomic penalties (20-40% higher finger travel vs. Dvorak/Colemak) and learning costs; no meaningful exit — alternative layouts require purchasing specialized hardware/software and solitary retraining with no institutional support.
narrative_ontology:constraint_stakeholder(qwerty_persistence_inevitability__strategic_lock_in_reading, general_typing_population, payer,
    moderate, biographical, constrained, global).

% Inventors of superior layouts (Dvorak 1936, Colemak 2006, and numerous others) who demonstrated measurable ergonomic and efficiency gains; systematically excluded from typewriter contracts, typing school curricula, civil service requirements, and later computer BIOS/OS defaults; their exclusion was actively maintained by cartridge compatibility requirements, procurement specifications, and certification mandates traceable to the 1893 cartel's training infrastructure.
narrative_ontology:constraint_stakeholder(qwerty_persistence_inevitability__strategic_lock_in_reading, alternative_layout_innovators, excluded,
    moderate, generational, trapped, global).

% Scholars (David 1985, Liebowitz & Margolis 1990, subsequent debate) analyzing QWERTY as a canonical case of path dependence vs. engineered lock-in; their interpretive framing shapes policy understanding of standardization, antitrust, and technology policy; no material stake in the constraint's operation.
narrative_ontology:constraint_stakeholder(qwerty_persistence_inevitability__strategic_lock_in_reading, economic_historians, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(qwerty_persistence_inevitability__strategic_lock_in_reading, remington_typewriter_cartel_1893).
narrative_ontology:fixing_cost_class(qwerty_persistence_inevitability__strategic_lock_in_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solved the genuine coordination problem of interoperable typewriter operation across manufacturers, offices, and a mobile clerical workforce — a single layout enabled portable skills, compatible machines, and efficient labor markets.
% TRANSFER_FUNCTION: Moves ergonomic efficiency, retraining costs, and monopoly rents from the entire typing population (professional and general) to the 1893 cartel members and their training/certification ecosystem; the transfer is sustained by controlling the standardization process and the human capital investments it creates.
% ABSENT_VOICES: Typists themselves (especially early female clerical workers) had no collective representation in the 1893 standardization decisions; alternative layout inventors were structurally excluded from the cartel's training and procurement channels; the ergonomic science of the era was ignored in favor of the cartel's commercial interests.
% DISAPPEARANCE_RATIONALE: If QWERTY's enforced monopoly vanished overnight, keyboard manufacturers would immediately offer multiple layouts; typing curricula would diversify; ergonomic layouts would capture significant market share within 5-10 years; the touch-typing certification industry would restructure around layout-agnostic competency; the constraint's entire enforcement infrastructure (procurement specs, BIOS defaults, certification mandates) would collapse.
% FOUNDING_PROBLEM: The 1870s-1880s typewriter market was fragmented: dozens of incompatible keyboard arrangements, no portable operator skills, no interoperable machine servicing, and no standard for clerical training — each manufacturer's machines required proprietary operator training.
% FOUNDING_PROBLEM_CORROBORATION: The coordination problem (interoperability, portable skills) was genuinely solved by 1900 — but the 1893 cartel then weaponized the standard to exclude competitors and extract rents for decades after the founding problem was solved. Economic historians David (1985) and Liebowitz & Margolis (1990) document the cartel's post-1900 behavior; the US v. Underwood antitrust case (1914) established the cartel's exclusionary practices; no independent party corroborates that the founding problem persisted past 1910.
narrative_ontology:disappearance_verdict(qwerty_persistence_inevitability__strategic_lock_in_reading, world_rearranges).
narrative_ontology:founding_problem_status(qwerty_persistence_inevitability__strategic_lock_in_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(qwerty_persistence_inevitability__strategic_lock_in_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(qwerty_persistence_inevitability__strategic_lock_in_reading, 'none', 1).
narrative_ontology:epsilon_provenance(qwerty_persistence_inevitability__strategic_lock_in_reading, 0.68, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(qwerty_persistence_inevitability__strategic_lock_in_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(qwerty_persistence_inevitability__strategic_lock_in_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(qwerty_persistence_inevitability__strategic_lock_in_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness (0.68) reflects the ongoing ergonomic tax (20-40% excess finger travel, higher RSI rates) borne by billions of users plus the monopoly rents extracted by the historical cartel and its institutional descendants — the constraint transfers value from the entire typing population to a narrow beneficiary set. Suppression (0.75) is high because the constraint's persistence depends on active exclusion: BIOS/OS defaults, procurement specifications, certification requirements, and the absence of institutional support for alternatives are not passive — they require continuous maintenance by standards bodies, manufacturers, and educational institutions. Theater ratio (0.42) captures that the 'interoperability' justification was genuine early on but has become increasingly performative — the coordination function was achieved by 1900; post-1910 enforcement serves rent extraction. The measurement grid uses a shared 7-point timeline from 1873 (Sholes prototype) to 2024, capturing the cartel formation (1893), clerical feminization peak (1910), Dvorak challenge (1936), PC standardization (1984), Colemak emergence (2006), and present.
 *
 * PERSPECTIVAL GAP:
 *   From the cartel's perspective (1893-1920), the constraint was a genuine coordination solution they built and profited from — a rope that became a tangled rope. From professional typists' perspective (1910-present), it is an identity-locked snare: their human capital investment makes exit personally catastrophic. From the general population's perspective, it is a diffuse extraction with no exit — a snare at civilizational scale. From alternative innovators' perspective, it is a suppression machine. The engine computes these seat-level classifications from the structural data; the single claimed_type (tangled_rope) represents the constraint's dominant structural character across seats.
 *
 * DIRECTIONALITY LOGIC:
 *   The 1893 cartel members are full beneficiaries (d ≈ 0.1) — they designed the lock-in, collected the rents, and controlled the standardization process. The typing school network is a secondary beneficiary/agenda-setter (d ≈ 0.2) — their business model depends on the monopoly but they administer the human capital lock-in. Equipment manufacturers adopting QWERTY later are beneficiaries with constrained exit (d ≈ 0.3) — they benefit from network effects but didn't create the lock-in. Professional typists are identity-locked payers (d ≈ 0.85) — their professional identity is fused to QWERTY mastery; retraining threatens livelihood and status. General typing population are constrained payers (d ≈ 0.75) — they bear ergonomic costs with no meaningful exit. Alternative layout innovators are trapped and excluded (d ≈ 0.95) — their exclusion IS the enforcement mechanism. Economic historians are analytical observers (d = 0.5).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (market fragmentation, no interoperable standard) was genuinely solved by ~1900. The constraint persisted for 120+ years after its founding problem died — this is mandatrophy in its purest form: the arrangement that solved the coordination problem was captured by the beneficiaries who then actively suppressed alternatives to maintain rent extraction. The founding_problem_status = dead with corroboration from outside the beneficiary set (antitrust records, economic history) confirms the capture. The constraint was never a pure snare — it had a real coordination function initially — but the coordination function atrophied while the extraction machinery intensified, producing the tangled_rope classification.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    cartel_intent_vs_emergent_order,
    'Was the 1893 typewriter trust''s standardization of QWERTY a deliberate lock-in strategy, or did they merely ratify an emerging de facto standard for genuine coordination reasons?',
    'Archival research on 1893 trust meeting minutes, correspondence between Remington/Caligraph/Yost executives, and the founding documents of the touch-typing school network — specifically whether they discussed excluding alternative layouts or merely achieving interoperability.',
    'If deliberate lock-in is documented, the constraint is a designed tangled_rope with clear beneficiary intent; if emergent, the beneficiary structure is retrospective and the constraint may be a rope that degraded into a piton. This omega directly determines whether the tangled_rope classification''s ''asymmetric extraction'' component is designed or evolved.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cartel_intent_vs_emergent_order, empirical, 'Whether the 1893 cartel''s standardization was strategic lock-in or genuine coordination.').

omega_variable(
    ergonomic_cost_magnitude,
    'What is the aggregate ergonomic cost (RSI, fatigue, speed penalty) imposed on the global typing population by QWERTY vs. optimal layouts, measured in DALYs or economic productivity loss?',
    'Large-scale longitudinal studies comparing QWERTY vs. Dvorak/Colemak users in controlled workplace settings, combined with global typing population estimates and RSI incidence data.',
    'If the ergonomic cost is confirmed at the 20-40% efficiency penalty range with significant RSI differential, the extractiveness metric (0.68) is validated; if the penalty is negligible in modern contexts (autocorrect, predictive text, voice input), the extractiveness may be overstated for the contemporary period.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(ergonomic_cost_magnitude, empirical, 'Magnitude of the ongoing ergonomic extraction from the typing population.').

omega_variable(
    suppression_mechanism_contemporary,
    'Is contemporary QWERTY suppression structural (BIOS defaults, procurement specs, certification mandates) or internalized (users believe QWERTY is ''natural'' or ''good enough'')?',
    'Survey experiments offering zero-cost layout switching with full institutional support; measure adoption rates and persistence. If suppression is structural, adoption will be high; if internalized, adoption will remain low even with support.',
    'If internalized suppression dominates, the constraint''s effective suppression is higher than structural measures suggest — users carry the suppression with them. This would increase the constraint''s snare character at the individual seat level.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_contemporary, conceptual, 'Structural vs. internalized suppression in the contemporary period.').

omega_variable(
    kernel_framing_ambiguity,
    'Does the kernel ''QWERTY persistence'' refer to the layout''s market dominance, the standardization process, the ergonomic suboptimality, or the cartel''s historical role — and do different framings produce different ε values?',
    'Decompose the kernel into its constituent claims (market share persistence, standardization history, ergonomic gap, cartel agency) and measure ε for each. If ε varies across framings, the kernel contains multiple constraints requiring separate stories per the ε-invariance principle.',
    'If the kernel conflates multiple constraints with different ε, the current story''s ε (0.68) may be a composite that obscures distinct dynamics. Decomposition would produce separate constraint stories for each structural claim, linked via network.affects_constraints.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_framing_ambiguity, conceptual, 'Whether the kernel label covers one constraint or multiple structurally distinct claims.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(qwerty_persistence_inevitability__strategic_lock_in_reading, 1873, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qwerty_persistence_inevitability__strategic_lock_in_reading_tr_t1873, qwerty_persistence_inevitability__strategic_lock_in_reading, theater_ratio, 1873, 0.05).
narrative_ontology:measurement(qwerty_persistence_inevitability__strategic_lock_in_reading_tr_t1893, qwerty_persistence_inevitability__strategic_lock_in_reading, theater_ratio, 1893, 0.18).
narrative_ontology:measurement(qwerty_persistence_inevitability__strategic_lock_in_reading_tr_t1910, qwerty_persistence_inevitability__strategic_lock_in_reading, theater_ratio, 1910, 0.28).
narrative_ontology:measurement(qwerty_persistence_inevitability__strategic_lock_in_reading_tr_t1936, qwerty_persistence_inevitability__strategic_lock_in_reading, theater_ratio, 1936, 0.35).
narrative_ontology:measurement(qwerty_persistence_inevitability__strategic_lock_in_reading_tr_t1984, qwerty_persistence_inevitability__strategic_lock_in_reading, theater_ratio, 1984, 0.39).
narrative_ontology:measurement(qwerty_persistence_inevitability__strategic_lock_in_reading_tr_t2006, qwerty_persistence_inevitability__strategic_lock_in_reading, theater_ratio, 2006, 0.41).
narrative_ontology:measurement(qwerty_persistence_inevitability__strategic_lock_in_reading_tr_t2024, qwerty_persistence_inevitability__strategic_lock_in_reading, theater_ratio, 2024, 0.42).

% Extraction over time
narrative_ontology:measurement(qwerty_persistence_inevitability__strategic_lock_in_reading_be_t1873, qwerty_persistence_inevitability__strategic_lock_in_reading, base_extractiveness, 1873, 0.15).
narrative_ontology:measurement(qwerty_persistence_inevitability__strategic_lock_in_reading_be_t1893, qwerty_persistence_inevitability__strategic_lock_in_reading, base_extractiveness, 1893, 0.35).
narrative_ontology:measurement(qwerty_persistence_inevitability__strategic_lock_in_reading_be_t1910, qwerty_persistence_inevitability__strategic_lock_in_reading, base_extractiveness, 1910, 0.52).
narrative_ontology:measurement(qwerty_persistence_inevitability__strategic_lock_in_reading_be_t1936, qwerty_persistence_inevitability__strategic_lock_in_reading, base_extractiveness, 1936, 0.61).
narrative_ontology:measurement(qwerty_persistence_inevitability__strategic_lock_in_reading_be_t1984, qwerty_persistence_inevitability__strategic_lock_in_reading, base_extractiveness, 1984, 0.65).
narrative_ontology:measurement(qwerty_persistence_inevitability__strategic_lock_in_reading_be_t2006, qwerty_persistence_inevitability__strategic_lock_in_reading, base_extractiveness, 2006, 0.67).
narrative_ontology:measurement(qwerty_persistence_inevitability__strategic_lock_in_reading_be_t2024, qwerty_persistence_inevitability__strategic_lock_in_reading, base_extractiveness, 2024, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(qwerty_persistence_inevitability__strategic_lock_in_reading_su_t1873, qwerty_persistence_inevitability__strategic_lock_in_reading, suppression_requirement, 1873, 0.1).
narrative_ontology:measurement(qwerty_persistence_inevitability__strategic_lock_in_reading_su_t1893, qwerty_persistence_inevitability__strategic_lock_in_reading, suppression_requirement, 1893, 0.45).
narrative_ontology:measurement(qwerty_persistence_inevitability__strategic_lock_in_reading_su_t1910, qwerty_persistence_inevitability__strategic_lock_in_reading, suppression_requirement, 1910, 0.62).
narrative_ontology:measurement(qwerty_persistence_inevitability__strategic_lock_in_reading_su_t1936, qwerty_persistence_inevitability__strategic_lock_in_reading, suppression_requirement, 1936, 0.68).
narrative_ontology:measurement(qwerty_persistence_inevitability__strategic_lock_in_reading_su_t1984, qwerty_persistence_inevitability__strategic_lock_in_reading, suppression_requirement, 1984, 0.72).
narrative_ontology:measurement(qwerty_persistence_inevitability__strategic_lock_in_reading_su_t2006, qwerty_persistence_inevitability__strategic_lock_in_reading, suppression_requirement, 2006, 0.74).
narrative_ontology:measurement(qwerty_persistence_inevitability__strategic_lock_in_reading_su_t2024, qwerty_persistence_inevitability__strategic_lock_in_reading, suppression_requirement, 2024, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(qwerty_persistence_inevitability__strategic_lock_in_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(qwerty_persistence_inevitability__strategic_lock_in_reading, 0.08).
narrative_ontology:affects_constraint(qwerty_persistence_inevitability__strategic_lock_in_reading, qwerty_persistence_inevitability__path_dependency_reading).
narrative_ontology:affects_constraint(qwerty_persistence_inevitability__strategic_lock_in_reading, computer_keyboard_bios_defaults).
narrative_ontology:affects_constraint(qwerty_persistence_inevitability__strategic_lock_in_reading, touch_typing_certification_mandates).
narrative_ontology:affects_constraint(qwerty_persistence_inevitability__strategic_lock_in_reading, dvorak_exclusion_from_procurement_specs).

% DUAL FORMULATION NOTE:
% Constraint family decomposition of kernel qwerty_persistence_inevitability: this reading (strategic_lock_in_reading) identifies the 1893 cartel as active architect of lock-in with ε=0.68 (tangled_rope); the sibling reading (path_dependency_reading) treats persistence as emergent network effects with ε≈0.15 (rope or mountain). The ε values differ by >0.5 — they are structurally distinct constraints sharing only the observable outcome (QWERTY dominance). Linked via affects_constraints in both directions.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(qwerty_persistence_inevitability__strategic_lock_in_reading, moderate, 0.85).
constraint_indexing:directionality_override(qwerty_persistence_inevitability__strategic_lock_in_reading, moderate, 0.75).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
