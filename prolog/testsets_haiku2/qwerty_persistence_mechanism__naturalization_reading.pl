% ============================================================================
% CONSTRAINT STORY: qwerty_persistence_mechanism__naturalization_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_qwerty_persistence_mechanism__naturalization_reading, []).

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
 *   constraint_id: qwerty_persistence_mechanism__naturalization_reading
 *   human_readable: QWERTY Keyboard Standard Persistence (Naturalization Reading)
 *   domain: economic_history/technology_standards
 *
 * SUMMARY:
 *   This constraint instantiates the NATURALIZATION READING of the QWERTY
 *   persistence kernel. Under this reading, QWERTY persists because (1) it
 *   was/became genuinely adequate for coordinating typing across devices, (2)
 *   switching costs reflect legitimate skill investment and network lock-in
 *   benefits, not suppression or extraction by incumbents, and (3)
 *   alternative layouts (Dvorak, Colemak, etc.) lapsed through fair
 *   competition, not through active suppression. The reading posits no
 *   systematic beneficiary collecting rents; the arrangement is a
 *   coordination solution that distributes benefits uniformly across typist
 *   populations and manufacturers. This reading contrasts with the lock-in
 *   reading (QWERTY persists despite inferiority through coordination
 *   failure) and the beneficiary extraction reading (QWERTY persists because
 *   manufacturers maintain it to protect training investments). The
 *   claim/metric gap is intentional: the constraint is claimed as rope
 *   (genuine coordination with uniform benefits) while the metrics show low
 *   extractiveness (0.18, stable across 150+ years), low suppression (0.12),
 *   and low theater (0.05), consistent with a nearly pure coordination
 *   mechanism.
 *
 * KEY AGENTS:
 *   - Typist populations: global organized constituency, benefit from network standardization, mobile exit (could learn Dvorak, but zero benefit while QWERTY is universal)
 *   - Keyboard manufacturers: institutional beneficiaries of production simplification, no monopoly position or rent collection, constrained but not trapped
 *   - Dvorak advocates: moderate power, mobile exit, bear an opportunity cost (forgone productivity gain) that is not extracted, economically rational to stay
 *   - Technical standards bodies: institutional agenda-setter, administrative coordination role, no enforcement machinery, exit from QWERTY would require market consensus shift
 *   - Historical alternative layouts: powerless and excluded by market selection, not institutional suppression, confined to niche adoption where the switching-cost barrier proved insurmountable
 *   - Efficiency researchers: analytical observers, measure and dispute the magnitude of layout differences, unable to coordinate a mass switch
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(qwerty_persistence_mechanism__naturalization_reading, 0.18).
domain_priors:suppression_score(qwerty_persistence_mechanism__naturalization_reading, 0.12).
domain_priors:theater_ratio(qwerty_persistence_mechanism__naturalization_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(qwerty_persistence_mechanism__naturalization_reading, extractiveness, 0.18).
narrative_ontology:constraint_metric(qwerty_persistence_mechanism__naturalization_reading, suppression_requirement, 0.12).
narrative_ontology:constraint_metric(qwerty_persistence_mechanism__naturalization_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(qwerty_persistence_mechanism__naturalization_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(qwerty_persistence_mechanism__naturalization_reading, resistance, 0.28).

% --- Constraint claim ---
narrative_ontology:constraint_claim(qwerty_persistence_mechanism__naturalization_reading, rope).
narrative_ontology:human_readable(qwerty_persistence_mechanism__naturalization_reading, "QWERTY Keyboard Standard Persistence (Naturalization Reading)").
narrative_ontology:topic_domain(qwerty_persistence_mechanism__naturalization_reading, "economic_history/technology_standards").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(qwerty_persistence_mechanism__naturalization_reading, '20555da0-9440-4ebf-9985-a68cec8436b0').
narrative_ontology:cs_kernel_codification('20555da0-9440-4ebf-9985-a68cec8436b0', implicit).
narrative_ontology:cs_authority_grounding('20555da0-9440-4ebf-9985-a68cec8436b0', practice).
narrative_ontology:cs_interpretation_layer_present('20555da0-9440-4ebf-9985-a68cec8436b0').
narrative_ontology:cs_reading_relation('20555da0-9440-4ebf-9985-a68cec8436b0', qwerty_persistence_mechanism__lock_in_reading, coexists_with).
narrative_ontology:cs_reading_relation('20555da0-9440-4ebf-9985-a68cec8436b0', qwerty_persistence_mechanism__beneficiary_extraction_reading, coexists_with).
narrative_ontology:cs_axiom('20555da0-9440-4ebf-9985-a68cec8436b0', foundational, coordination_benefit_exceeds_switching_cost).
narrative_ontology:cs_axiom_status(coordination_benefit_exceeds_switching_cost, holdable).
narrative_ontology:cs_axiom_grounding('20555da0-9440-4ebf-9985-a68cec8436b0', coordination_benefit_exceeds_switching_cost, instrumental).
narrative_ontology:cs_axiom('20555da0-9440-4ebf-9985-a68cec8436b0', foundational, no_systematic_beneficiary_maintains_qwerty).
narrative_ontology:cs_axiom_status(no_systematic_beneficiary_maintains_qwerty, holdable).
narrative_ontology:cs_axiom_grounding('20555da0-9440-4ebf-9985-a68cec8436b0', no_systematic_beneficiary_maintains_qwerty, empirically_contingent).
narrative_ontology:cs_reference_frame('20555da0-9440-4ebf-9985-a68cec8436b0', keyboard_coordination_as_user_choice).
narrative_ontology:cs_drift_state('20555da0-9440-4ebf-9985-a68cec8436b0', digital_era_2026, gap(stable, minor, true)).
narrative_ontology:cs_created_at('20555da0-9440-4ebf-9985-a68cec8436b0', '').
narrative_ontology:cs_kernel_id(qwerty_persistence_mechanism__naturalization_reading, qwerty_persistence_mechanism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(qwerty_persistence_mechanism__naturalization_reading, typist_populations).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(qwerty_persistence_mechanism__naturalization_reading, keyboard_manufacturers).
narrative_ontology:constraint_victim(qwerty_persistence_mechanism__naturalization_reading, dvorak_advocates).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefit from a single, globally standardized keyboard layout. Learning QWERTY gives access to every typewriter, computer, and shared typing environment on Earth without retooling. The network effect of universal standardization compounds the benefit: switching cost for any individual is high, but the value of staying is also high because everyone else stays. Their exit option is theoretically available — they could acquire Dvorak skill — but the benefit of doing so is zero as long as machines around them remain QWERTY.
narrative_ontology:constraint_stakeholder(qwerty_persistence_mechanism__naturalization_reading, typist_populations, beneficiary,
    organized, biographical, mobile, global).

% Benefit from having one standard to manufacture and market rather than multiple competing layouts. Production simplification, inventory efficiency, and market clarity are genuine efficiencies. They do not extract monopoly rent from the standard; they operate under thin margins in a competitive input market. Their constraint is standardization itself, not control of it.
narrative_ontology:constraint_stakeholder(qwerty_persistence_mechanism__naturalization_reading, keyboard_manufacturers, beneficiary,
    institutional, generational, constrained, global).

% Bear the cost of a suboptimal layout by this reading's lights: finger travel distance, same-finger usage frequency, and learning difficulty exceed Dvorak by empirically contested margins (0.5–10% depending on task). However, the switching cost — acquiring Dvorak skill while maintaining QWERTY competence for shared environments — exceeds the productivity gain under most real-world use cases. They are not trapped; they are economically rational to stay. Their 'cost' is an opportunity cost, not an extraction.
narrative_ontology:constraint_stakeholder(qwerty_persistence_mechanism__naturalization_reading, dvorak_advocates, payer,
    moderate, biographical, mobile, global).

% Maintain and document the QWERTY standard (ISO 4169 and equivalents). Their role is administrative coordination, not enforcement or rent collection. They do not defend QWERTY against alternatives; they codify whatever the market has settled on. Under this reading, their function is to preserve the coordination mechanism, not to suppress alternatives.
narrative_ontology:constraint_stakeholder(qwerty_persistence_mechanism__naturalization_reading, technical_standards_bodies, agenda_setter,
    institutional, generational, analytical, global).

% Dvorak, Colemak, and other alternatives were designed and have merit by various metrics. They are not present at the table because market selection — fair competition under identical switching-cost conditions — eliminated them or confined them to niche adoption. Under this reading, they lost fair competition, not suppression. Their absence from the conversation reflects their empirical failure to overcome the network effect, not institutional exclusion.
narrative_ontology:constraint_stakeholder(qwerty_persistence_mechanism__naturalization_reading, historical_alternative_layouts, excluded,
    powerless, biographical, trapped, local).

% Study keyboard ergonomics and typing efficiency. Disagree on whether Dvorak's advantages (when they exist) are meaningful enough to justify the switching cost. Publish contested findings without ability to coordinate a mass switch. Under this reading, their role is measurement, not advocacy for a particular layout.
narrative_ontology:constraint_stakeholder(qwerty_persistence_mechanism__naturalization_reading, efficiency_researchers, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single, universal keyboard layout standard that lets any typist use any typing device anywhere without retooling. The network effect is the coordination solution: everyone learns one layout because everyone else has learned that layout, and the marginal benefit of learning another layout approaches zero. This is a genuine collective-action problem: absent coordination, each typist might adopt a different layout, and the resulting fragmentation would reduce everyone's network value.
% TRANSFER_FUNCTION: The arrangement does not move value from one party to another in this reading. It distributes a coordination benefit uniformly: typists gain access to a global keyboard ecosystem; manufacturers gain simplification. Dvorak advocates bear an opportunity cost (forgone potential productivity gain) that is not extracted by a beneficiary — it is the rational choice to forgo a low-benefit, high-cost switch.
% ABSENT_VOICES: Manufacturers who might prefer a non-QWERTY layout are not present because they do not exist as a meaningful constituency — they benefit from any single standard. Dvorak designers and their early-adopter communities were present in the competition but are now absent from current market conversations because Dvorak failed to overcome the switching-cost barrier despite being available at zero cost to learn.
% DISAPPEARANCE_RATIONALE: If QWERTY disappeared, typing would not stop; it would reorganize around whatever single layout coordinated fastest. The most likely candidate would be the next-most-standardized layout (Dvorak in some regions, possibly a new layout in emerging markets). The transition would be costly and disruptive, but the underlying coordination problem — that typists need ONE standard — would persist and be solved by whatever filled the gap.
% FOUNDING_PROBLEM: Early mechanical typewriters had competing key layouts (QWERTY, DHIATENSOR, ALPHABETICAL, and others). Manufacturers needed to coordinate on a single layout to avoid making machines incompatible with each other and with trained typists. Typists needed to invest in learning a layout once and expect it to work on all machines.
% FOUNDING_PROBLEM_CORROBORATION: Industrial historians (Liebowitz & Margolis, 1990; David, 1985) attest that coordination failures and layout fragmentation were genuine problems in the late 19th century — incompatible machines and retraining costs created real friction. Standards bodies and manufacturers attest that a single global standard remains functionally valuable. Ergonomic researchers dispute whether Dvorak is meaningfully faster, but even advocates acknowledge switching costs are real and the gap is modest.
narrative_ontology:disappearance_verdict(qwerty_persistence_mechanism__naturalization_reading, world_rearranges).
narrative_ontology:founding_problem_status(qwerty_persistence_mechanism__naturalization_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(qwerty_persistence_mechanism__naturalization_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(qwerty_persistence_mechanism__naturalization_reading, 'none', 1).
narrative_ontology:epsilon_provenance(qwerty_persistence_mechanism__naturalization_reading, 0.18, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(qwerty_persistence_mechanism__naturalization_reading_tests).
:- end_tests(qwerty_persistence_mechanism__naturalization_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness starts moderate (0.25) in 1873 because the coordination problem itself is novel and some parties (early typists, alternative-layout designers) do not yet perceive the value of settling on one standard. As QWERTY coordination value becomes apparent, extractiveness declines to 0.15 by 1920 — the constraint delivers what it was built for and distributes the benefit widely. By 1960, digital computing and standardization bodies solidify the role, extractiveness stabilizes at 0.12. A slight uptick to 0.18 by 2000–2026 reflects ergonomic research suggesting Dvorak may have modest productivity advantages (5–10% in some tasks, near-zero in others) — if the advantage is real but not actionable due to switching costs, that unactionable advantage becomes a measured opportunity cost. Theater remains near-zero throughout (0.03–0.05): there is no pretense that QWERTY serves a function other than coordination; standards bodies document it straightforwardly. Suppression stays low and stable (0.08–0.12): the only 'suppressive' force is the network effect itself (which is not suppressive in the sense of active enforcement, but is a structural barrier). This measurement trajectory is consistent with a genuine coordination rope that has been highly stable and valuable for over a century.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap between seats is modest under this reading, which is exactly what a healthy coordination rope should show. A typist in 1920 sees QWERTY as a valuable standard that saves them from layout fragmentation. A manufacturer in 1920 sees the same constraint as simplification and market clarity. A Dvorak researcher in 1985 sees a layout that is (by some measures) 10% faster but (by all measures) worth zero benefit due to switching costs — they recognize the coordination value even while proposing an alternative. Under the lock-in reading, these seats would perceive sharp divergence: manufacturers would see a defended monopoly while typists would see an arbitrary standard. Under the beneficiary extraction reading, manufacturers would see strategic defense while typists would see suppression. The naturalization reading predicts modest divergence and broad consensus on the constraint's functional value — a prediction that can be empirically tested against archival evidence (manufacturer correspondence, professional typist associations, standards body records) and should show less contention than the contested readings.
 *
 * DIRECTIONALITY LOGIC:
 *   Under this reading, directionality clusters near symmetric/beneficiary for all seated parties. Typist populations are beneficiaries (d ≈ 0.25–0.35): they gain network access far exceeding the cost of learning one layout. Manufacturers are beneficiaries (d ≈ 0.15–0.25): they benefit from production simplification without bearing suppression costs. Dvorak advocates are near-symmetric or slight-target (d ≈ 0.45–0.55): they bear an opportunity cost (forgone potential productivity gain), but exit is available and the benefit of switching is genuinely low. No party is a full target (d >> 0.75) because no party is actively extracted from by a concentrated beneficiary — the constraint distributes coordination value, not concentrated rents. This contrasts sharply with the beneficiary extraction reading, where manufacturers would show high d (trapped into defending QWERTY to protect sunk training investments) and typists would show d near 0.5 (benefiting from coordination but also suppressed from alternatives).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (typewriter layout coordination, late 19th century) was genuinely live through the mechanical era (1873–1960) and remains live in digital computing (1960–present) in a modified form. Computer keyboards did not inherit QWERTY because of sunk training investments in typewriter skill; they inherited it because the transition from typewriter to computer happened faster than a format switch would have — typists who learned QWERTY on machines already knew QWERTY, making the choice of digital layout a near-zero-cost coordination decision (use the layout everyone already knows). The constraint does not show mandatrophy in this reading: the founding problem persists, the coordination function remains valuable, and no party is maintaining the arrangement theatrically. A mandatrophy verdict would require evidence that (a) the founding problem is dead (keyboard layouts are no longer a coordination problem) or (b) the arrangement persists despite the problem being dead (it's maintained purely for sunk investment protection or bureaucratic inertia). Under the naturalization reading, neither condition holds. The lock-in reading would classify this as mandatrophy: QWERTY persists despite being inferior, solving a problem that could be solved better by Dvorak if switching costs could be overcome. The beneficiary extraction reading would also flag mandatrophy: QWERTY persists to protect manufacturer training investments even though the original coordination problem is solved.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    dvorak_productivity_advantage_magnitude,
    'How large is the productivity advantage of Dvorak over QWERTY, measured across real-world typing tasks?',
    'Meta-analysis of empirical ergonomic studies, accounting for task type (transcription vs. composition), practitioner skill level, and practice history. Controlled experiments with equal training on both layouts.',
    'If the advantage is negligible (<2%), the naturalization reading is strongly supported: no switching cost could be justified. If the advantage is substantial (>15%), the reading weakens: a real gain foregone might indicate suppression or lock-in. The intermediate range (2–15%) leaves the reading defensible but contested — the advantage is real but not obviously worth the switching cost for most users.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(dvorak_productivity_advantage_magnitude, empirical, 'Whether Dvorak''s claimed efficiency gains are measurable and material across real use cases.').

omega_variable(
    manufacturer_active_defense_of_qwerty,
    'Did typewriter and keyboard manufacturers actively work to suppress alternatives or maintain QWERTY, or did they simply manufacture what the market demanded?',
    'Archival research into manufacturer correspondence, patent strategies, marketing materials, and standards-body participation. Historical testimony from alternative-layout designers on whether they faced suppression or market rejection.',
    'Evidence of active suppression (manufacturers refusing to produce alternatives, standards bodies excluding alternatives, patents weaponized against alternatives) would support the beneficiary extraction reading and undermine naturalization. Evidence of passive market selection (manufacturers would have produced alternatives if demand existed) supports naturalization.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(manufacturer_active_defense_of_qwerty, empirical, 'Whether QWERTY''s persistence reflects active maintenance by incumbents or passive market selection.').

omega_variable(
    switching_cost_vs_network_benefit_decomposition,
    'Can the individual switching cost and the network benefit be empirically separated, or are they structurally entangled?',
    'Experimental or quasi-experimental settings where switching costs are artificially reduced (e.g., keyboard-learning software, ergonomic training programs) and measure whether adoption rates increase. Historical cases where switching costs were reduced by technological change (e.g., programmable keyboards) and observe whether alternative layouts gain adoption.',
    'If switching costs and network benefits are entangled, the naturalization reading is correct: it is rational for users to stay even if an alternative is technically superior. If they can be separated and alternatives gain adoption when switching costs fall, the constraint has more lock-in characteristics (lock-in reading).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(switching_cost_vs_network_benefit_decomposition, empirical, 'Whether the persistence of QWERTY reflects rational coordination under switching costs, or reflects path-dependent lock-in.').

omega_variable(
    naturalization_vs_lock_in_epistemic_boundary,
    'Is there a principled way to distinguish a genuine coordination solution from a path-dependent lock-in that just looks like coordination?',
    'Theoretical analysis: a true coordination solution should show (a) uniform distribution of benefits across users, (b) low suppression of alternatives, (c) minimal theater in maintenance, (d) stability in the measured extractiveness. A lock-in should show (a) concentrated benefits or suppressed alternatives, (b) active enforcement or market-suppressing behavior, (c) higher theater (justifications that exceed actual function). The naturalization reading predicts the first profile; the lock-in reading predicts the second.',
    'If the readings predict different empirical profiles and both are ex-ante coherent, the empirical findings (measurements, archival evidence, stakeholder testimony) should differentiate them. A reading that predicts everything, or that predicts the same outcome as its rivals, is not doing theoretical work.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(naturalization_vs_lock_in_epistemic_boundary, conceptual, 'Whether the naturalization and lock-in readings are empirically distinguishable or reducible to the same mechanism under different framings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(qwerty_persistence_mechanism__naturalization_reading, 1873, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qwer_tr_t1873, qwerty_persistence_mechanism__naturalization_reading, theater_ratio, 1873, 0.02).
narrative_ontology:measurement(qwer_tr_t1920, qwerty_persistence_mechanism__naturalization_reading, theater_ratio, 1920, 0.03).
narrative_ontology:measurement(qwer_tr_t1960, qwerty_persistence_mechanism__naturalization_reading, theater_ratio, 1960, 0.04).
narrative_ontology:measurement(qwer_tr_t2000, qwerty_persistence_mechanism__naturalization_reading, theater_ratio, 2000, 0.05).
narrative_ontology:measurement(qwer_tr_t2026, qwerty_persistence_mechanism__naturalization_reading, theater_ratio, 2026, 0.05).

% Extraction over time
narrative_ontology:measurement(qwer_be_t1873, qwerty_persistence_mechanism__naturalization_reading, base_extractiveness, 1873, 0.25).
narrative_ontology:measurement(qwer_be_t1920, qwerty_persistence_mechanism__naturalization_reading, base_extractiveness, 1920, 0.15).
narrative_ontology:measurement(qwer_be_t1960, qwerty_persistence_mechanism__naturalization_reading, base_extractiveness, 1960, 0.12).
narrative_ontology:measurement(qwer_be_t2000, qwerty_persistence_mechanism__naturalization_reading, base_extractiveness, 2000, 0.18).
narrative_ontology:measurement(qwer_be_t2026, qwerty_persistence_mechanism__naturalization_reading, base_extractiveness, 2026, 0.18).

% Suppression requirement over time
narrative_ontology:measurement(qwer_su_t1873, qwerty_persistence_mechanism__naturalization_reading, suppression_requirement, 1873, 0.08).
narrative_ontology:measurement(qwer_su_t1920, qwerty_persistence_mechanism__naturalization_reading, suppression_requirement, 1920, 0.1).
narrative_ontology:measurement(qwer_su_t1960, qwerty_persistence_mechanism__naturalization_reading, suppression_requirement, 1960, 0.11).
narrative_ontology:measurement(qwer_su_t2000, qwerty_persistence_mechanism__naturalization_reading, suppression_requirement, 2000, 0.12).
narrative_ontology:measurement(qwer_su_t2026, qwerty_persistence_mechanism__naturalization_reading, suppression_requirement, 2026, 0.12).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(qwerty_persistence_mechanism__naturalization_reading, information_standard).
narrative_ontology:boltzmann_floor_override(qwerty_persistence_mechanism__naturalization_reading, 0.05).
narrative_ontology:affects_constraint(qwerty_persistence_mechanism__naturalization_reading, qwerty_persistence_mechanism__lock_in_reading).
narrative_ontology:affects_constraint(qwerty_persistence_mechanism__naturalization_reading, qwerty_persistence_mechanism__beneficiary_extraction_reading).

% DUAL FORMULATION NOTE:
% The QWERTY kernel decomposes into three constraint stories, each representing a different reading of why QWERTY persists. The naturalization reading (this story) claims the arrangement solves a real coordination problem and distributes benefits uniformly. The lock-in reading claims QWERTY persists through path-dependent coordination failure despite technical inferiority. The beneficiary extraction reading claims QWERTY persists because incumbents actively maintain it to protect sunk investments. The three stories share the same referent (QWERTY's historical dominance) but diagnose the persistence mechanism differently, yielding different ε, beneficiary structures, and computed classifications. The network edges reflect the readings' shared kernel identity and mutual empirical contestation: findings about Dvorak's productivity, manufacturer suppression, or switching-cost decomposition affect all three stories' credibility simultaneously.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
