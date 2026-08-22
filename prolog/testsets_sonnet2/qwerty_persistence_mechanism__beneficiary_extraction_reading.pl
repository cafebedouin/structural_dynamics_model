% ============================================================================
% CONSTRAINT STORY: qwerty_persistence_mechanism__beneficiary_extraction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_qwerty_persistence_mechanism__beneficiary_extraction_reading, []).

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
 *   constraint_id: qwerty_persistence_mechanism__beneficiary_extraction_reading
 *   human_readable: QWERTY Keyboard Layout Persistence (Beneficiary Extraction Reading)
 *   domain: economic history/technology studies/path dependence theory
 *
 * SUMMARY:
 *   Christopher Sholes designed QWERTY in the 1870s partly to reduce typebar
 *   jamming on early mechanical typewriters. Remington commercialized it, and
 *   the Union Typewriter Company trust (formed 1893 from merging
 *   manufacturers) subsequently coordinated pricing and, crucially, sponsored
 *   touch-typing curricula built around the QWERTY finger-mapping. This
 *   reading holds that the persistence of QWERTY past the point of technical
 *   necessity (typebar jamming ceased to be relevant with electric and later
 *   electronic keyboards) is best explained by the deliberate maintenance of
 *   switching costs by manufacturers and schools protecting sunk training
 *   investments and market position, not by inevitable lock-in or genuine
 *   merit.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(qwerty_persistence_mechanism__beneficiary_extraction_reading, 0.68).
domain_priors:suppression_score(qwerty_persistence_mechanism__beneficiary_extraction_reading, 0.71).
domain_priors:theater_ratio(qwerty_persistence_mechanism__beneficiary_extraction_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(qwerty_persistence_mechanism__beneficiary_extraction_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(qwerty_persistence_mechanism__beneficiary_extraction_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(qwerty_persistence_mechanism__beneficiary_extraction_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(qwerty_persistence_mechanism__beneficiary_extraction_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(qwerty_persistence_mechanism__beneficiary_extraction_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(qwerty_persistence_mechanism__beneficiary_extraction_reading, tangled_rope).
narrative_ontology:human_readable(qwerty_persistence_mechanism__beneficiary_extraction_reading, "QWERTY Keyboard Layout Persistence (Beneficiary Extraction Reading)").
narrative_ontology:topic_domain(qwerty_persistence_mechanism__beneficiary_extraction_reading, "economic history/technology studies/path dependence theory").

domain_priors:requires_active_enforcement(qwerty_persistence_mechanism__beneficiary_extraction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(qwerty_persistence_mechanism__beneficiary_extraction_reading, 'dfb042d1-dc1f-4f84-9657-6ce41631993e').
narrative_ontology:cs_kernel_codification('dfb042d1-dc1f-4f84-9657-6ce41631993e', distributed).
narrative_ontology:cs_authority_grounding('dfb042d1-dc1f-4f84-9657-6ce41631993e', practice).
narrative_ontology:cs_interpretation_layer_present('dfb042d1-dc1f-4f84-9657-6ce41631993e').
narrative_ontology:cs_reading_relation('dfb042d1-dc1f-4f84-9657-6ce41631993e', qwerty_persistence_mechanism__naturalization_reading, coexists_with).
narrative_ontology:cs_reading_relation('dfb042d1-dc1f-4f84-9657-6ce41631993e', qwerty_persistence_mechanism__lock_in_reading, influences).
narrative_ontology:cs_axiom('dfb042d1-dc1f-4f84-9657-6ce41631993e', foundational, persistence_reflects_active_beneficiary_maintenance).
narrative_ontology:cs_axiom_status(persistence_reflects_active_beneficiary_maintenance, holdable).
narrative_ontology:cs_axiom_grounding('dfb042d1-dc1f-4f84-9657-6ce41631993e', persistence_reflects_active_beneficiary_maintenance, empirically_contingent).
narrative_ontology:cs_axiom('dfb042d1-dc1f-4f84-9657-6ce41631993e', secondary, switching_costs_are_artificially_constructed_not_intrinsic).
narrative_ontology:cs_axiom_status(switching_costs_are_artificially_constructed_not_intrinsic, holdable).
narrative_ontology:cs_axiom_grounding('dfb042d1-dc1f-4f84-9657-6ce41631993e', switching_costs_are_artificially_constructed_not_intrinsic, empirically_contingent).
narrative_ontology:cs_reference_frame('dfb042d1-dc1f-4f84-9657-6ce41631993e', manufacturer_coordinated_training_standard).
narrative_ontology:cs_drift_state('dfb042d1-dc1f-4f84-9657-6ce41631993e', post_electronic_keyboard_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('dfb042d1-dc1f-4f84-9657-6ce41631993e', '').
narrative_ontology:cs_kernel_id(qwerty_persistence_mechanism__beneficiary_extraction_reading, qwerty_persistence_mechanism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(qwerty_persistence_mechanism__beneficiary_extraction_reading, remington_and_union_typewriter_trust).
narrative_ontology:constraint_beneficiary(qwerty_persistence_mechanism__beneficiary_extraction_reading, incumbent_typing_schools).
narrative_ontology:constraint_beneficiary(qwerty_persistence_mechanism__beneficiary_extraction_reading, vocational_certification_bodies).
narrative_ontology:constraint_victim(qwerty_persistence_mechanism__beneficiary_extraction_reading, novice_typists).
narrative_ontology:constraint_victim(qwerty_persistence_mechanism__beneficiary_extraction_reading, rival_layout_inventors).
narrative_ontology:constraint_victim(qwerty_persistence_mechanism__beneficiary_extraction_reading, clerical_workers_retraining_cost_bearers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(qwerty_persistence_mechanism__beneficiary_extraction_reading, office_employers).
narrative_ontology:constraint_victim(qwerty_persistence_mechanism__beneficiary_extraction_reading, office_employers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Manufactured the original QWERTY machines and, through the Union Typewriter Company trust formed in 1893, coordinated pricing, patents, and distribution among the dominant typewriter makers. Funded and endorsed touch-typing curricula built specifically around the QWERTY finger-mapping, tying its commercial position to the layout's continued dominance in schools and offices. Collects the ongoing benefit of a locked-in standard it built infrastructure to defend.
narrative_ontology:constraint_stakeholder(qwerty_persistence_mechanism__beneficiary_extraction_reading, remington_and_union_typewriter_trust, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(qwerty_persistence_mechanism__beneficiary_extraction_reading, remington_and_union_typewriter_trust, beneficiary).

% Built curricula, textbooks, and instructor credentials entirely around QWERTY finger placement, often with direct sponsorship or licensing arrangements from typewriter manufacturers. Retraining on an alternative layout would obsolete their teaching materials and instructor expertise; they lobby employers and certification boards to keep QWERTY as the tested standard, protecting the schools' existing capital rather than optimizing for student outcomes.
narrative_ontology:constraint_stakeholder(qwerty_persistence_mechanism__beneficiary_extraction_reading, incumbent_typing_schools, beneficiary,
    organized, biographical, constrained, national).

% Administer typing speed certifications used as hiring gatekeepers for clerical work. Standardized their tests on QWERTY, which makes any alternative layout invisible to the labor market regardless of demonstrated typing speed on that layout — the certification apparatus itself becomes a switching-cost enforcement mechanism, independent of the manufacturers' original commercial motive.
narrative_ontology:constraint_stakeholder(qwerty_persistence_mechanism__beneficiary_extraction_reading, vocational_certification_bodies, beneficiary,
    organized, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(qwerty_persistence_mechanism__beneficiary_extraction_reading, vocational_certification_bodies, agenda_setter).

% Must learn QWERTY to be employable as clerical workers, regardless of whether a faster or more ergonomic layout exists, because certification tests, employer expectations, and available instruction all assume QWERTY. Bear the cost of a suboptimal finger-mapping (documented strain and slower peak speeds relative to alternatives like Dvorak) with no individual power to change the standard they are tested against.
narrative_ontology:constraint_stakeholder(qwerty_persistence_mechanism__beneficiary_extraction_reading, novice_typists, payer,
    powerless, biographical, trapped, national).

% Designed and patented alternative layouts (most notably the Dvorak Simplified Keyboard) demonstrating measurable speed and fatigue improvements in trials. Could not get manufacturers to retool, schools to retrain, or certification bodies to test on the new layout — the incumbent infrastructure's coordinated refusal to adopt functioned as a de facto barrier regardless of the alternative's merits.
narrative_ontology:constraint_stakeholder(qwerty_persistence_mechanism__beneficiary_extraction_reading, rival_layout_inventors, payer,
    moderate, biographical, trapped, national).
narrative_ontology:stakeholder_secondary_role(qwerty_persistence_mechanism__beneficiary_extraction_reading, rival_layout_inventors, excluded).

% Already trained on QWERTY through years of muscle memory; any individual switch to a superior layout means temporarily falling below employable typing speed while retraining, a cost no individual worker can absorb alone even if the collective outcome of switching would be better. This is the switching-cost trap the incumbents' training-investment strategy created and then benefited from without having to actively suppress each new typist.
narrative_ontology:constraint_stakeholder(qwerty_persistence_mechanism__beneficiary_extraction_reading, clerical_workers_retraining_cost_bearers, payer,
    powerless, biographical, trapped, national).

% Benefit from a standardized, interchangeable pool of QWERTY-trained labor and equipment, but are themselves constrained by the same certification and training ecosystem — an employer that switched its offices to an alternative layout would face a much smaller applicant pool and would need to retrain or specially recruit staff.
narrative_ontology:constraint_stakeholder(qwerty_persistence_mechanism__beneficiary_extraction_reading, office_employers, beneficiary,
    powerful, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(qwerty_persistence_mechanism__beneficiary_extraction_reading, office_employers, payer).

% Study the QWERTY case as a canonical example in path-dependence and lock-in theory (David 1985 and its critics/defenders). Their disagreement over whether QWERTY reflects deliberate extraction, blind lock-in, or genuine adequacy is precisely the kernel contest this constraint story is one reading of.
narrative_ontology:constraint_stakeholder(qwerty_persistence_mechanism__beneficiary_extraction_reading, economic_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(qwerty_persistence_mechanism__beneficiary_extraction_reading, diffuse).
narrative_ontology:fixing_cost_class(qwerty_persistence_mechanism__beneficiary_extraction_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: A single, universally taught finger-mapping lets any trained typist use any QWERTY machine and lets any employer hire from a common labor pool without per-worker retraining — genuine coordination value that the beneficiary structure rides on top of.
% TRANSFER_FUNCTION: Moves the cost of an admittedly non-optimal layout (higher error rates, documented finger-travel inefficiency, repetitive strain relative to alternatives) from the manufacturers and schools who built and profit from the training infrastructure onto every subsequent typist and worker who must learn and re-learn it, and onto inventors of superior alternatives who cannot break the certification/training moat.
% ABSENT_VOICES: Rival layout inventors (Dvorak and successors) presented trial data supporting faster, lower-error typing but had no seat at the certification bodies or manufacturer trusts that controlled adoption; their objections appear in patent records and independent trials rather than in the institutional record of why QWERTY remained standard.
% DISAPPEARANCE_RATIONALE: If the coordinated training/certification/manufacturing infrastructure protecting QWERTY vanished overnight, the switching-cost floor for alternative layouts would collapse: certification bodies could test any layout, schools could teach whichever is fastest to learn, and the labor market would re-sort toward whatever layout wins on merit rather than incumbency — a different equilibrium than the one currently locked in, showing the current arrangement is not merely descriptive of the world but load-bearing for specific interests.
% FOUNDING_PROBLEM: In the 1870s-1890s, typewriter manufacturers needed typists trained fast and consistently across machines to sell typewriters as a viable office technology; a shared, teachable finger-mapping solved a genuine coordination problem for a nascent industry.
% FOUNDING_PROBLEM_CORROBORATION: Independent trials (U.S. Navy 1944 Dvorak study, subsequent ergonomics research cited in economic-history literature) attest that alternative layouts offer measurable performance gains, meaning the original 'we need any standard, fast' problem no longer requires QWERTY specifically — this corroboration comes from ergonomics researchers and economic historians outside the manufacturer/school beneficiary set, though the beneficiary set itself (successor certification bodies, keyboard manufacturers) continues to assert QWERTY's adequacy is settled rather than contingent.
narrative_ontology:disappearance_verdict(qwerty_persistence_mechanism__beneficiary_extraction_reading, world_rearranges).
narrative_ontology:founding_problem_status(qwerty_persistence_mechanism__beneficiary_extraction_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(qwerty_persistence_mechanism__beneficiary_extraction_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(qwerty_persistence_mechanism__beneficiary_extraction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(qwerty_persistence_mechanism__beneficiary_extraction_reading, 0.68, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(qwerty_persistence_mechanism__beneficiary_extraction_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(qwerty_persistence_mechanism__beneficiary_extraction_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(qwerty_persistence_mechanism__beneficiary_extraction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68) reflects a persistent gap between QWERTY's documented ergonomic/speed performance and superior alternatives (Dvorak trials), sustained well past the era when manufacturing constraints justified it. Suppression (0.71) is authored high because certification bodies and school curricula function as an active gate against alternative layouts, not merely passive inertia — this is the structural delta this reading asserts against the lock_in_reading's passive-coordination-failure account. Theater ratio rises across the interval (0.10 to 0.42) as the original manufacturing rationale (typebar jamming) became obsolete with new technology while the training/certification apparatus persisted and even hardened, indicating growing performative justification layered onto a hollowed-out original function.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter seat (trust/certification bodies), QWERTY appears as legitimate, functioning standardization — a rope. From the payer seats (novice typists, rival inventors), the same structure appears as an enforced extraction of switching costs that serves incumbent capital, not typing performance — because their exit is trapped by an infrastructure they did not build and cannot individually escape. The engine should compute these divergent per-seat readings from the structural power/exit data, not from any single narrative claim.
 *
 * DIRECTIONALITY LOGIC:
 *   The trust and its successor institutions (typing schools, certification bodies) sit at the beneficiary end: they built capital (curricula, credentials, market position) whose value depends on QWERTY's continuation and would be destroyed by a shift to a superior layout. Novice typists, retrained clerical workers, and rival inventors sit at the target end: trapped exit options because individual defection from the standard (learning Dvorak alone) yields no employment benefit given collectively-set certification standards — this is a coordination trap the beneficiaries built and now benefit from without needing continuous active suppression of each individual.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (need for ANY shared, teachable standard in a nascent typewriter industry, partly to reduce jamming) is genuinely dead as a technical matter once electric and electronic keyboards eliminated jamming risk entirely, yet the arrangement persists via certification and training infrastructure whose costs now fall on typists rather than manufacturers. Classifying this as tangled_rope rather than snare preserves the genuine coordination residue (a shared standard has real value) while still naming the asymmetric extraction (beneficiaries who built switching costs to protect sunk investment, not to optimize typing) — collapsing it to snare would erase the real coordination function this reading concedes exists; collapsing it to mountain (as naturalization_reading effectively does) would erase the beneficiary structure and active maintenance this reading is specifically about.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_disagreement_locus,
    'Is QWERTY''s persistence best explained by (a) deliberate beneficiary maintenance of switching costs [this reading], (b) path-dependent coordination failure with no identifiable maintaining actor [lock_in_reading], or (c) QWERTY''s genuine adequacy under fair competition [naturalization_reading]?',
    'Archival evidence on whether the Union Typewriter Company trust and successor certification bodies took specific actions to block, discredit, or exclude alternative layouts (versus merely failing to coordinate a switch), compared against independent trial data (Navy 1944 Dvorak study and successors) on relative performance.',
    'If archival evidence shows active suppression (patent blocking, certification exclusion, lobbying against alternative-layout adoption), this reading''s tangled_rope/high-suppression profile is the accurate one. If evidence shows only passive coordination failure with no identifiable actor benefiting from active maintenance, the lock_in_reading''s account is more accurate and this story''s beneficiary/enforcement claims would be overstated. If performance trials are inconclusive or QWERTY proves genuinely comparable under modern typing patterns, the naturalization_reading gains support and this story''s extraction claim weakens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_disagreement_locus, conceptual, 'The three-way kernel contest over which causal mechanism explains QWERTY persistence.').

omega_variable(
    trust_intent_vs_effect,
    'Did the Union Typewriter Company trust and typing schools intentionally design curricula to create switching costs as a competitive moat, or did the training ecosystem emerge from ordinary commercial incentives without anti-competitive intent, with the extraction effect being an unintended byproduct?',
    'Historical trust records, internal manufacturer correspondence, and antitrust-era documentation (the Union Typewriter Company was itself later subject to antitrust scrutiny) could establish whether curriculum standardization was a deliberate strategic choice versus incidental.',
    'Intentional design would support classifying the extraction as closer to snare-adjacent (deliberate rent-protection); unintentional emergence with the same effect keeps the tangled_rope classification (genuine coordination function persists even without conspiratorial intent) but shifts the moral valence of the beneficiary reading.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(trust_intent_vs_effect, empirical, 'Whether the beneficiary structure arose from strategic intent or incidental market dynamics.').

omega_variable(
    counterfactual_switching_cost_magnitude,
    'How much of the actual switching cost typists and employers face is attributable to the manufacturer/school-built infrastructure specifically, versus an irreducible cost of any standard-switch that would exist even absent any incumbent maintenance activity?',
    'Comparative case study against instances of successful large-scale keyboard-layout transition (e.g., national transitions to different input standards in other countries/scripts) to isolate the infrastructure-specific cost component from the baseline coordination-switch cost.',
    'If most of the switching cost is baseline coordination cost, the beneficiary_extraction_reading''s high suppression score is overstated relative to lock_in_reading''s account. If most of the cost is specifically attributable to certification/curriculum lock-in built and maintained by identifiable beneficiaries, this reading''s high suppression score is well-supported.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(counterfactual_switching_cost_magnitude, empirical, 'Decomposing switching costs into baseline coordination cost versus beneficiary-maintained artificial cost.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(qwerty_persistence_mechanism__beneficiary_extraction_reading, 0, 140).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qwer_tr_t0, qwerty_persistence_mechanism__beneficiary_extraction_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(qwer_tr_t25, qwerty_persistence_mechanism__beneficiary_extraction_reading, theater_ratio, 25, 0.2).
narrative_ontology:measurement(qwer_tr_t50, qwerty_persistence_mechanism__beneficiary_extraction_reading, theater_ratio, 50, 0.3).
narrative_ontology:measurement(qwer_tr_t80, qwerty_persistence_mechanism__beneficiary_extraction_reading, theater_ratio, 80, 0.36).
narrative_ontology:measurement(qwer_tr_t110, qwerty_persistence_mechanism__beneficiary_extraction_reading, theater_ratio, 110, 0.4).
narrative_ontology:measurement(qwer_tr_t140, qwerty_persistence_mechanism__beneficiary_extraction_reading, theater_ratio, 140, 0.42).

% Extraction over time
narrative_ontology:measurement(qwer_be_t0, qwerty_persistence_mechanism__beneficiary_extraction_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(qwer_be_t25, qwerty_persistence_mechanism__beneficiary_extraction_reading, base_extractiveness, 25, 0.55).
narrative_ontology:measurement(qwer_be_t50, qwerty_persistence_mechanism__beneficiary_extraction_reading, base_extractiveness, 50, 0.63).
narrative_ontology:measurement(qwer_be_t80, qwerty_persistence_mechanism__beneficiary_extraction_reading, base_extractiveness, 80, 0.66).
narrative_ontology:measurement(qwer_be_t110, qwerty_persistence_mechanism__beneficiary_extraction_reading, base_extractiveness, 110, 0.67).
narrative_ontology:measurement(qwer_be_t140, qwerty_persistence_mechanism__beneficiary_extraction_reading, base_extractiveness, 140, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(qwer_su_t0, qwerty_persistence_mechanism__beneficiary_extraction_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(qwer_su_t25, qwerty_persistence_mechanism__beneficiary_extraction_reading, suppression_requirement, 25, 0.68).
narrative_ontology:measurement(qwer_su_t50, qwerty_persistence_mechanism__beneficiary_extraction_reading, suppression_requirement, 50, 0.72).
narrative_ontology:measurement(qwer_su_t80, qwerty_persistence_mechanism__beneficiary_extraction_reading, suppression_requirement, 80, 0.7).
narrative_ontology:measurement(qwer_su_t110, qwerty_persistence_mechanism__beneficiary_extraction_reading, suppression_requirement, 110, 0.71).
narrative_ontology:measurement(qwer_su_t140, qwerty_persistence_mechanism__beneficiary_extraction_reading, suppression_requirement, 140, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(qwerty_persistence_mechanism__beneficiary_extraction_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(qwerty_persistence_mechanism__beneficiary_extraction_reading, 0.08).
narrative_ontology:affects_constraint(qwerty_persistence_mechanism__beneficiary_extraction_reading, qwerty_persistence_mechanism__lock_in_reading).
narrative_ontology:affects_constraint(qwerty_persistence_mechanism__beneficiary_extraction_reading, qwerty_persistence_mechanism__naturalization_reading).

% DUAL FORMULATION NOTE:
% This story is one of three linked constraints sharing the qwerty_persistence_mechanism kernel. The naturalization_reading treats QWERTY as adequate-by-fair-competition (near-mountain, low ε). The lock_in_reading treats persistence as pure path-dependent coordination failure with no identified beneficiary actively maintaining it (rope-adjacent, moderate ε, no active-enforcement gate). This story, the beneficiary_extraction_reading, asserts identifiable beneficiaries (manufacturer trust, typing schools, certification bodies) who actively maintain switching costs, warranting tangled_rope classification with active enforcement and a substantially higher ε than either sibling. All three share the same underlying historical episode but are structurally distinct constraints per the ε-invariance principle; they must not be merged or averaged.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
