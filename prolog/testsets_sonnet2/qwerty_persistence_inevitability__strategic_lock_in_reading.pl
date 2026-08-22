% ============================================================================
% CONSTRAINT STORY: qwerty_persistence_inevitability__strategic_lock_in_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
 *   human_readable: QWERTY Keyboard Standard as Manufacturer-Engineered Lock-In
 *   domain: technology_history/political_economy/institutional_analysis
 *
 * SUMMARY:
 *   The QWERTY keyboard layout, designed in the 1870s partly to reduce
 *   mechanical type-bar jamming, became the near-universal standard not
 *   purely through the accumulation of individual adoption decisions but
 *   through active coordination among typewriter manufacturers — most visibly
 *   the 1893 Union Typewriter Company combination — who standardized their
 *   machines on QWERTY and partnered with commercial typing schools to build
 *   a training and certification pipeline keyed to that layout. Once the
 *   labor market's typing credential was defined as QWERTY proficiency,
 *   employers demanded it, schools taught it, and no competing layout —
 *   including the empirically faster and less injurious Dvorak Simplified
 *   Keyboard patented in 1936 — could break into the credentialing loop,
 *   regardless of technical merit.
 *
 * KEY AGENTS:
 *   - typewriter_manufacturer_cartel_1893: agenda-setting beneficiary that coordinated standardization and training partnerships
 *   - commercial_typing_schools: co-beneficiary that operationalized the training lock-in
 *   - professional_typists: primary victims bearing retraining barriers and ergonomic cost
 *   - alternative_layout_inventors: excluded voices whose superior designs were structurally foreclosed
 *   - labor_historians: analytical observers assessing the coordination record
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(qwerty_persistence_inevitability__strategic_lock_in_reading, 0.68).
domain_priors:suppression_score(qwerty_persistence_inevitability__strategic_lock_in_reading, 0.62).
domain_priors:theater_ratio(qwerty_persistence_inevitability__strategic_lock_in_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(qwerty_persistence_inevitability__strategic_lock_in_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(qwerty_persistence_inevitability__strategic_lock_in_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(qwerty_persistence_inevitability__strategic_lock_in_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(qwerty_persistence_inevitability__strategic_lock_in_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(qwerty_persistence_inevitability__strategic_lock_in_reading, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(qwerty_persistence_inevitability__strategic_lock_in_reading, tangled_rope).
narrative_ontology:human_readable(qwerty_persistence_inevitability__strategic_lock_in_reading, "QWERTY Keyboard Standard as Manufacturer-Engineered Lock-In").
narrative_ontology:topic_domain(qwerty_persistence_inevitability__strategic_lock_in_reading, "technology_history/political_economy/institutional_analysis").

domain_priors:requires_active_enforcement(qwerty_persistence_inevitability__strategic_lock_in_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(qwerty_persistence_inevitability__strategic_lock_in_reading, 'de984ac0-f1e1-4a5d-bcf5-57a6a5da999a').
narrative_ontology:cs_kernel_codification('de984ac0-f1e1-4a5d-bcf5-57a6a5da999a', distributed).
narrative_ontology:cs_authority_grounding('de984ac0-f1e1-4a5d-bcf5-57a6a5da999a', extraction).
narrative_ontology:cs_interpretation_layer_present('de984ac0-f1e1-4a5d-bcf5-57a6a5da999a').
narrative_ontology:cs_reading_relation('de984ac0-f1e1-4a5d-bcf5-57a6a5da999a', qwerty_persistence_inevitability__path_dependency_reading, coexists_with).
narrative_ontology:cs_axiom('de984ac0-f1e1-4a5d-bcf5-57a6a5da999a', foundational, standardization_control_was_deliberately_captured).
narrative_ontology:cs_axiom_status(standardization_control_was_deliberately_captured, holdable).
narrative_ontology:cs_axiom_grounding('de984ac0-f1e1-4a5d-bcf5-57a6a5da999a', standardization_control_was_deliberately_captured, empirically_contingent).
narrative_ontology:cs_axiom('de984ac0-f1e1-4a5d-bcf5-57a6a5da999a', secondary, training_pipeline_coordination_constitutes_active_lockin).
narrative_ontology:cs_axiom_status(training_pipeline_coordination_constitutes_active_lockin, holdable).
narrative_ontology:cs_axiom_grounding('de984ac0-f1e1-4a5d-bcf5-57a6a5da999a', training_pipeline_coordination_constitutes_active_lockin, empirically_contingent).
narrative_ontology:cs_reference_frame('de984ac0-f1e1-4a5d-bcf5-57a6a5da999a', pre_cartel_open_layout_competition).
narrative_ontology:cs_drift_state('de984ac0-f1e1-4a5d-bcf5-57a6a5da999a', post_dvorak_patent_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('de984ac0-f1e1-4a5d-bcf5-57a6a5da999a', '').
narrative_ontology:cs_kernel_id(qwerty_persistence_inevitability__strategic_lock_in_reading, qwerty_persistence_inevitability).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(qwerty_persistence_inevitability__strategic_lock_in_reading, typewriter_manufacturer_cartel_1893).
narrative_ontology:constraint_beneficiary(qwerty_persistence_inevitability__strategic_lock_in_reading, commercial_typing_schools).
narrative_ontology:constraint_beneficiary(qwerty_persistence_inevitability__strategic_lock_in_reading, remington_successor_firms).
narrative_ontology:constraint_victim(qwerty_persistence_inevitability__strategic_lock_in_reading, professional_typists).
narrative_ontology:constraint_victim(qwerty_persistence_inevitability__strategic_lock_in_reading, office_workers_learning_to_type).
narrative_ontology:constraint_victim(qwerty_persistence_inevitability__strategic_lock_in_reading, ergonomic_injury_sufferers).
narrative_ontology:constraint_victim(qwerty_persistence_inevitability__strategic_lock_in_reading, alternative_layout_inventors).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(qwerty_persistence_inevitability__strategic_lock_in_reading, employers_hiring_typists).
narrative_ontology:constraint_victim(qwerty_persistence_inevitability__strategic_lock_in_reading, employers_hiring_typists).
narrative_ontology:constraint_vindicates(qwerty_persistence_inevitability__strategic_lock_in_reading, standardized_training_reduces_hiring_friction).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Remington and allied manufacturers formed the 1893 Union Typewriter Company combination, standardizing on QWERTY across member firms and coordinating with typing-school curricula so that a certified skill in one manufacturer's layout was a skill in all of theirs. They set the de facto industry standard, fund and license the training pipeline that reproduces it, and profit from every machine and every course sold to reach that pipeline.
narrative_ontology:constraint_stakeholder(qwerty_persistence_inevitability__strategic_lock_in_reading, typewriter_manufacturer_cartel_1893, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(qwerty_persistence_inevitability__strategic_lock_in_reading, typewriter_manufacturer_cartel_1893, beneficiary).

% Built curricula and touch-typing certification exclusively around QWERTY in partnership with manufacturers who supplied machines and endorsement. They collect tuition and licensing fees, and their institutional survival depends on QWERTY remaining the credentialed standard employers demand.
narrative_ontology:constraint_stakeholder(qwerty_persistence_inevitability__strategic_lock_in_reading, commercial_typing_schools, beneficiary,
    organized, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(qwerty_persistence_inevitability__strategic_lock_in_reading, commercial_typing_schools, agenda_setter).

% Invested months of muscle-memory training in a layout demonstrably less efficient and more injury-prone than documented alternatives (e.g., Dvorak). Once trained, switching layouts means discarding a hard-won, income-generating skill; employers hire and test only for QWERTY, so exit is not economically available even though the ergonomic cost is borne daily.
narrative_ontology:constraint_stakeholder(qwerty_persistence_inevitability__strategic_lock_in_reading, professional_typists, payer,
    moderate, biographical, trapped, national).

% Enter the labor market needing a marketable typing credential and are funneled into QWERTY instruction because it is the only certification employers recognize. They have no meaningful choice at the point of skill acquisition — the standardization was set decades before they arrived and presents itself as simply how typing is done.
narrative_ontology:constraint_stakeholder(qwerty_persistence_inevitability__strategic_lock_in_reading, office_workers_learning_to_type, payer,
    powerless, biographical, trapped, national).

% Bear repetitive strain injury risk correlated with QWERTY's finger-travel and hand-alternation patterns, a cost never priced into the manufacturers' or schools' calculus. Retraining onto a lower-injury layout after symptoms appear is possible in principle but professionally and financially prohibitive given sunk skill investment and employer non-recognition of alternatives.
narrative_ontology:constraint_stakeholder(qwerty_persistence_inevitability__strategic_lock_in_reading, ergonomic_injury_sufferers, payer,
    powerless, biographical, trapped, national).

% Designed and patented layouts with documented efficiency and ergonomic advantages (notably Dvorak, 1936) but could not break the manufacturer-school-employer credentialing loop; their alternatives were structurally locked out regardless of technical merit because no manufacturer had incentive to retrain the market or cede the standardization advantage it already held.
narrative_ontology:constraint_stakeholder(qwerty_persistence_inevitability__strategic_lock_in_reading, alternative_layout_inventors, excluded,
    powerless, biographical, trapped, national).

% Benefit from a standardized labor pool that reduces hiring and retraining friction — any certified typist can be placed at any QWERTY machine. They also indirectly pay through the aggregate productivity ceiling and injury-related costs the layout imposes, but individually lack incentive to defect from the standard unilaterally.
narrative_ontology:constraint_stakeholder(qwerty_persistence_inevitability__strategic_lock_in_reading, employers_hiring_typists, beneficiary,
    powerful, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(qwerty_persistence_inevitability__strategic_lock_in_reading, employers_hiring_typists, payer).

% Study the documentary record of the Union Typewriter Company's coordination, training-partnership contracts, and the suppressed adoption trials of alternative layouts, assessing whether persistence reflects genuine technical lock-in or engineered market coordination.
narrative_ontology:constraint_stakeholder(qwerty_persistence_inevitability__strategic_lock_in_reading, labor_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(qwerty_persistence_inevitability__strategic_lock_in_reading, typewriter_manufacturer_cartel_1893).
narrative_ontology:fixing_cost_class(qwerty_persistence_inevitability__strategic_lock_in_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: A single, universally recognized keyboard layout lets any trained typist operate any machine and lets any employer hire from a common labor pool without layout-specific retraining — a genuine coordination problem that standardization solves.
% TRANSFER_FUNCTION: Moves training costs, ergonomic injury risk, and foreclosed-efficiency losses onto typists and workers, while channeling licensing revenue, machine sales, and credentialing fees to the manufacturer cartel and its allied typing schools.
% ABSENT_VOICES: Alternative-layout inventors and the typists who would have benefited from a more efficient standard were never given a genuine market test; the 1893 combination's internal coordination records and training-partnership contracts are not part of the public adoption narrative that credits QWERTY's survival to consumer choice.
% DISAPPEARANCE_RATIONALE: If the manufacturer-training cartel's coordination collapsed and credentialing opened to competing layouts, employers would begin accepting alternative-layout certifications, typing schools would diversify curricula, and a multi-decade re-sorting of the labor market's skill signal would follow — the standardization is doing real coordination work that would need to be replaced, not merely removed.
% FOUNDING_PROBLEM: Early typewriter manufacturers needed a keyboard arrangement that reduced mechanical jamming in early strike-bar machines and, once several firms coalesced into the Union Typewriter Company, needed a single trainable standard to reduce cross-manufacturer hiring friction and lock in their collective market position against outside layout innovation.
% FOUNDING_PROBLEM_CORROBORATION: The jamming-prevention rationale is attested only by manufacturers' own historical accounts and is contradicted by mechanical historians who note strike-bar jamming was substantially addressed by other design changes decades before layout debates settled; independent ergonomics researchers and the Dvorak-era adoption-trial record (conducted outside the cartel's interest) corroborate that no jamming-based technical necessity survived past the early 20th century, yet the training-and-credentialing lock-in persisted.
narrative_ontology:disappearance_verdict(qwerty_persistence_inevitability__strategic_lock_in_reading, world_rearranges).
narrative_ontology:founding_problem_status(qwerty_persistence_inevitability__strategic_lock_in_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(qwerty_persistence_inevitability__strategic_lock_in_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(qwerty_persistence_inevitability__strategic_lock_in_reading, 'none', 1).
narrative_ontology:epsilon_provenance(qwerty_persistence_inevitability__strategic_lock_in_reading, 0.68, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness is authored at 0.68 — substantial but not maximal, reflecting that genuine coordination value (a common labor-market skill signal) is real and only partially offset by the extraction: the manufacturers' cartel captured control of what the standard would be and profited from that control, but individual typists still receive some real benefit from a portable, widely-recognized skill. Suppression jumps sharply at 1893 (0.5) when the Union Typewriter Company combination formalizes cross-manufacturer standardization and training partnerships, then stabilizes near 0.6-0.65 as the credentialing loop becomes self-sustaining through employer expectation rather than active manufacturer enforcement — this is the enforcement-hardening-then-normalization pattern captured in the suppression_requirement series. Theater ratio rises modestly (0.1 to 0.4) as the 'this is just the most efficient layout' narrative increasingly substitutes for the original mechanical-jamming rationale, which became obsolete once machines no longer jammed but the standardization story persisted anyway.
 *
 * PERSPECTIVAL GAP:
 *   From the manufacturer cartel's seat, standardization is pure coordination success — they built the industry's common language. From the professional typist's seat, the same arrangement is a trap: a skill investment that cannot be exited, imposed by a standard they had no part in choosing and that costs them ergonomically every working day. The engine should compute these as structurally different experiences of the identical arrangement, driven by the beneficiary/victim declarations and the trapped vs. arbitrage exit options — not by any difference in the constraint's stated purpose.
 *
 * DIRECTIONALITY LOGIC:
 *   The manufacturer cartel and allied typing schools sit at the beneficiary end: they set the standard, control the training pipeline, and collect the economic returns from standardization (machine sales, tuition, licensing). Typists and office workers sit at the target end: they bear the training-investment sunk cost, the ergonomic injury risk, and the foreclosure of switching to a superior layout, with trapped exit options because their marketable skill is defined entirely in terms of the locked-in standard. Employers occupy a genuinely mixed position — real beneficiaries of hiring-pool standardization, but also indirect payers of the aggregate productivity ceiling and injury costs, hence the dual role.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (mechanical jamming prevention) is dead — solved decades before layout debates concluded — while the standardization arrangement not only persisted but hardened into a full training-and-credentialing infrastructure. This is the mismatch the R5 genealogy interview is designed to surface: founding_problem_status=dead paired with disappearance_verdict=world_rearranges is exactly the capture/zombie signature, because the arrangement now persists for a different reason (credentialing lock-in and cartel coordination advantage) than the one that justified its origin. Classifying this as tangled_rope rather than pure snare preserves the fact that real coordination value (a common typing standard) still exists alongside the extraction — collapsing it to snare would erase the genuine labor-market benefit employers and even typists partially receive; classifying it as rope or mountain (the path-dependency reading's implicit framing) would erase the identifiable beneficiary cartel and the active training-partnership enforcement that this reading holds sustains it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    coordination_vs_accident_kernel_ambiguity,
    'Is QWERTY''s persistence better explained by deliberate manufacturer-cartel coordination and training-partnership lock-in (this reading), or by accident-driven path dependency with no identifiable strategic beneficiary extracting rents (the sibling path_dependency_reading)?',
    'Archival analysis of Union Typewriter Company internal correspondence, contracts with typing schools, and pricing/licensing records from 1893-1910 would establish whether coordination was deliberate rent-extraction or emergent convergence without strategic intent. Existing historical scholarship (David 1985; Liebowitz & Margolis 1990) is itself contested on this exact point.',
    'If archival evidence shows no deliberate cartel coordination behind training-school partnerships, this reading''s tangled_rope classification collapses toward the sibling''s rope/path-dependency framing, and the beneficiary declarations here would need to be substantially weakened or removed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_vs_accident_kernel_ambiguity, empirical, 'Whether QWERTY persistence reflects strategic cartel coordination or unstructured historical accident — the central kernel-dividing question.').

omega_variable(
    dvorak_efficiency_magnitude_uncertainty,
    'How large is the actual efficiency and ergonomic-injury-reduction advantage of alternative layouts (Dvorak and successors) over QWERTY, once controlled for the training-effect confound in the original comparative studies (many of which were conducted or funded by Dvorak''s own patent holder)?',
    'Independent, training-matched controlled studies comparing typing speed and repetitive strain outcomes across layouts, with funding and study design independent of any layout''s patent holders.',
    'If the efficiency gap is small once methodological bias is controlled, the ergonomic-injury victim declaration is overstated and the extraction magnitude (ε) authored here should be lowered; if the gap is large and robust, the current ε is conservative.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(dvorak_efficiency_magnitude_uncertainty, empirical, 'Uncertainty in the magnitude of QWERTY''s genuine ergonomic and efficiency cost relative to alternatives.').

omega_variable(
    reading_choice_signal_disclosure,
    'This story''s classification as tangled_rope with a named beneficiary cartel depends on treating the 1893 standardization as a deliberate coordination act rather than a neutral market convergence — what specific historical signal justified choosing this framing over the path-dependency framing for this file?',
    'The choice rests primarily on the documented existence of the Union Typewriter Company as a formal multi-firm combination with joint standardization and training-partnership activity, which is a stronger coordination signal than pure independent-adoption path dependency requires. A neutral path-dependency account would need this combination to have had no standardization-relevant coordination function, which the historical record does not support even under contested interpretation.',
    'If the Union Typewriter Company''s role is shown to have been primarily about patent pooling and price-fixing on typewriters generally, with no meaningful role in training-pipeline coordination specifically, the tangled_rope reading''s core evidentiary basis weakens substantially.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_choice_signal_disclosure, conceptual, 'Documents the specific evidentiary basis for choosing the strategic_lock_in framing over the path_dependency framing for this file, per CS-framing under-determination guidance.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(qwerty_persistence_inevitability__strategic_lock_in_reading, 1873, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qwer_tr_t1873, qwerty_persistence_inevitability__strategic_lock_in_reading, theater_ratio, 1873, 0.1).
narrative_ontology:measurement(qwer_tr_t1893, qwerty_persistence_inevitability__strategic_lock_in_reading, theater_ratio, 1893, 0.2).
narrative_ontology:measurement(qwer_tr_t1936, qwerty_persistence_inevitability__strategic_lock_in_reading, theater_ratio, 1936, 0.3).
narrative_ontology:measurement(qwer_tr_t1970, qwerty_persistence_inevitability__strategic_lock_in_reading, theater_ratio, 1970, 0.35).
narrative_ontology:measurement(qwer_tr_t2000, qwerty_persistence_inevitability__strategic_lock_in_reading, theater_ratio, 2000, 0.38).
narrative_ontology:measurement(qwer_tr_t2024, qwerty_persistence_inevitability__strategic_lock_in_reading, theater_ratio, 2024, 0.4).

% Extraction over time
narrative_ontology:measurement(qwer_be_t1873, qwerty_persistence_inevitability__strategic_lock_in_reading, base_extractiveness, 1873, 0.2).
narrative_ontology:measurement(qwer_be_t1893, qwerty_persistence_inevitability__strategic_lock_in_reading, base_extractiveness, 1893, 0.45).
narrative_ontology:measurement(qwer_be_t1936, qwerty_persistence_inevitability__strategic_lock_in_reading, base_extractiveness, 1936, 0.55).
narrative_ontology:measurement(qwer_be_t1970, qwerty_persistence_inevitability__strategic_lock_in_reading, base_extractiveness, 1970, 0.6).
narrative_ontology:measurement(qwer_be_t2000, qwerty_persistence_inevitability__strategic_lock_in_reading, base_extractiveness, 2000, 0.65).
narrative_ontology:measurement(qwer_be_t2024, qwerty_persistence_inevitability__strategic_lock_in_reading, base_extractiveness, 2024, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(qwer_su_t1873, qwerty_persistence_inevitability__strategic_lock_in_reading, suppression_requirement, 1873, 0.15).
narrative_ontology:measurement(qwer_su_t1893, qwerty_persistence_inevitability__strategic_lock_in_reading, suppression_requirement, 1893, 0.5).
narrative_ontology:measurement(qwer_su_t1936, qwerty_persistence_inevitability__strategic_lock_in_reading, suppression_requirement, 1936, 0.65).
narrative_ontology:measurement(qwer_su_t1970, qwerty_persistence_inevitability__strategic_lock_in_reading, suppression_requirement, 1970, 0.6).
narrative_ontology:measurement(qwer_su_t2000, qwerty_persistence_inevitability__strategic_lock_in_reading, suppression_requirement, 2000, 0.62).
narrative_ontology:measurement(qwer_su_t2024, qwerty_persistence_inevitability__strategic_lock_in_reading, suppression_requirement, 2024, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(qwerty_persistence_inevitability__strategic_lock_in_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(qwerty_persistence_inevitability__strategic_lock_in_reading, 0.1).
narrative_ontology:affects_constraint(qwerty_persistence_inevitability__strategic_lock_in_reading, path_dependency_reading).

% DUAL FORMULATION NOTE:
% This story and constraint_path_dependency_reading are the two readings of the qwerty_persistence_inevitability kernel. They share the observable surface (QWERTY's continued dominance despite documented alternative layouts) but diverge sharply on ε and claimed_type: this reading (strategic_lock_in) authors ε=0.68 and claimed_type=tangled_rope, naming the 1893 manufacturer cartel and typing schools as beneficiaries and typists as victims of engineered lock-in; the sibling reading (path_dependency) authors a substantially lower ε with no identifiable extracting beneficiary, treating persistence as emergent switching-cost accumulation. Per the ε-invariance principle, these are two distinct constraints linked here rather than one constraint with an ambiguous ε.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
