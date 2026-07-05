% ============================================================================
% CONSTRAINT STORY: qwerty_persistence_mechanism__beneficiary_extraction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:measurement_basis/2,
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
 *   human_readable: QWERTY Persistence as Beneficiary-Maintained Extraction
 *   domain: economic_history/technology_studies/path_dependence
 *
 * SUMMARY:
 *   This story instantiates the beneficiary-extraction reading of the QWERTY
 *   persistence kernel: the layout's continued dominance is explained not by
 *   superior ergonomics (naturalization_reading) nor by pure
 *   coordination-failure lock-in with no identifiable profiteer
 *   (lock_in_reading), but by the active, self-interested maintenance of the
 *   standard by Remington/Union Typewriter's manufacturing dominance, the
 *   typing schools whose curricula are sunk into QWERTY, and the
 *   certification bodies that gatekeep employability on a QWERTY-only
 *   benchmark. The original mechanical jam-prevention rationale died with
 *   electric typewriters and computer keyboards, but the beneficiary
 *   coalition kept the standard in place because their training investments,
 *   credentialing infrastructure, and market position depend on it, not
 *   because switching costs alone would have preserved it absent active
 *   maintenance.
 *
 * KEY AGENTS:
 *   - remington_union_typewriter_trust: agenda_setter (institutional/arbitrage) — sets and maintains the hardware/curriculum standard
 *   - incumbent_typing_schools: beneficiary (organized/constrained) — curriculum sunk cost
 *   - touch_typing_certification_bodies: beneficiary+agenda_setter (organized/constrained) — controls employability gate
 *   - novice_typists: payer (powerless/trapped) — bears training-investment extraction
 *   - clerical_workers: payer (powerless/trapped) — bears ergonomic/productivity cost
 *   - rival_keyboard_inventors: excluded (powerless/trapped) — shut out of tooling and credentialing chain
 *   - economic_historians: observer (analytical/analytical) — produces the competing kernel readings
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(qwerty_persistence_mechanism__beneficiary_extraction_reading, 0.68).
domain_priors:suppression_score(qwerty_persistence_mechanism__beneficiary_extraction_reading, 0.72).
domain_priors:theater_ratio(qwerty_persistence_mechanism__beneficiary_extraction_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(qwerty_persistence_mechanism__beneficiary_extraction_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(qwerty_persistence_mechanism__beneficiary_extraction_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(qwerty_persistence_mechanism__beneficiary_extraction_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(qwerty_persistence_mechanism__beneficiary_extraction_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(qwerty_persistence_mechanism__beneficiary_extraction_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(qwerty_persistence_mechanism__beneficiary_extraction_reading, tangled_rope).
narrative_ontology:human_readable(qwerty_persistence_mechanism__beneficiary_extraction_reading, "QWERTY Persistence as Beneficiary-Maintained Extraction").
narrative_ontology:topic_domain(qwerty_persistence_mechanism__beneficiary_extraction_reading, "economic_history/technology_studies/path_dependence").

domain_priors:requires_active_enforcement(qwerty_persistence_mechanism__beneficiary_extraction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(qwerty_persistence_mechanism__beneficiary_extraction_reading, '5101c6b8-5cd0-4f67-8df8-bb3ed66ee38a').
narrative_ontology:cs_kernel_codification('5101c6b8-5cd0-4f67-8df8-bb3ed66ee38a', distributed).
narrative_ontology:cs_authority_grounding('5101c6b8-5cd0-4f67-8df8-bb3ed66ee38a', extraction).
narrative_ontology:cs_interpretation_layer_present('5101c6b8-5cd0-4f67-8df8-bb3ed66ee38a').
narrative_ontology:cs_reading_relation('5101c6b8-5cd0-4f67-8df8-bb3ed66ee38a', qwerty_persistence_mechanism__naturalization_reading, forecloses).
narrative_ontology:cs_reading_relation('5101c6b8-5cd0-4f67-8df8-bb3ed66ee38a', qwerty_persistence_mechanism__lock_in_reading, influences).
narrative_ontology:cs_axiom('5101c6b8-5cd0-4f67-8df8-bb3ed66ee38a', foundational, persistence_reflects_active_beneficiary_maintenance).
narrative_ontology:cs_axiom_status(persistence_reflects_active_beneficiary_maintenance, holdable).
narrative_ontology:cs_axiom_grounding('5101c6b8-5cd0-4f67-8df8-bb3ed66ee38a', persistence_reflects_active_beneficiary_maintenance, empirically_contingent).
narrative_ontology:cs_axiom('5101c6b8-5cd0-4f67-8df8-bb3ed66ee38a', secondary, training_investment_protection_justifies_standard_lock).
narrative_ontology:cs_axiom_status(training_investment_protection_justifies_standard_lock, holdable).
narrative_ontology:cs_axiom_grounding('5101c6b8-5cd0-4f67-8df8-bb3ed66ee38a', training_investment_protection_justifies_standard_lock, instrumental).
narrative_ontology:cs_reference_frame('5101c6b8-5cd0-4f67-8df8-bb3ed66ee38a', mechanical_jam_prevention_standard).
narrative_ontology:cs_drift_state('5101c6b8-5cd0-4f67-8df8-bb3ed66ee38a', post_electronic_keyboard_era, gap(axiom_overriding, severe, false)).
narrative_ontology:cs_created_at('5101c6b8-5cd0-4f67-8df8-bb3ed66ee38a', '').
narrative_ontology:cs_kernel_id(qwerty_persistence_mechanism__beneficiary_extraction_reading, qwerty_persistence_mechanism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(qwerty_persistence_mechanism__beneficiary_extraction_reading, remington_union_typewriter_trust).
narrative_ontology:constraint_beneficiary(qwerty_persistence_mechanism__beneficiary_extraction_reading, incumbent_typing_schools).
narrative_ontology:constraint_beneficiary(qwerty_persistence_mechanism__beneficiary_extraction_reading, touch_typing_certification_bodies).
narrative_ontology:constraint_victim(qwerty_persistence_mechanism__beneficiary_extraction_reading, novice_typists).
narrative_ontology:constraint_victim(qwerty_persistence_mechanism__beneficiary_extraction_reading, rival_keyboard_inventors).
narrative_ontology:constraint_victim(qwerty_persistence_mechanism__beneficiary_extraction_reading, clerical_workers).
narrative_ontology:constraint_vindicates(qwerty_persistence_mechanism__beneficiary_extraction_reading, training_investment_protection_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Manufactures and licenses the dominant typewriter hardware, standardizes on QWERTY across its product line and licensees, and coordinates with certification schools to fix the layout as the credentialing standard. Collects licensing revenue and hardware sales premised on layout continuity; would lose sunk tooling and trained-workforce advantages if a rival layout gained traction.
narrative_ontology:constraint_stakeholder(qwerty_persistence_mechanism__beneficiary_extraction_reading, remington_union_typewriter_trust, agenda_setter,
    institutional, generational, arbitrage, national).

% Have built curricula, textbooks, and instructor expertise entirely around QWERTY touch-typing. Certify typists for hire under the QWERTY standard and charge tuition and certification fees. A layout change would strand their curriculum investment and instructor skill base, so they actively lobby against alternative-layout adoption in commercial and clerical hiring standards.
narrative_ontology:constraint_stakeholder(qwerty_persistence_mechanism__beneficiary_extraction_reading, incumbent_typing_schools, beneficiary,
    organized, generational, constrained, national).

% Administer typing speed certifications used as hiring gates by employers, exclusively benchmarked to QWERTY. Control what counts as 'qualified' typing speed and refuse to develop parallel certification tracks for alternative layouts, which forecloses employer recognition of any rival system regardless of its measured efficiency.
narrative_ontology:constraint_stakeholder(qwerty_persistence_mechanism__beneficiary_extraction_reading, touch_typing_certification_bodies, beneficiary,
    organized, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(qwerty_persistence_mechanism__beneficiary_extraction_reading, touch_typing_certification_bodies, agenda_setter).

% Must learn QWERTY to be employable as clerks and typists, regardless of whether an alternative layout would let them type faster with less repetitive strain risk. Their training hours and cognitive investment are the switching cost the arrangement extracts; once trained, retraining to a rival layout means discarding sunk skill capital, which is precisely the lock the beneficiaries rely on.
narrative_ontology:constraint_stakeholder(qwerty_persistence_mechanism__beneficiary_extraction_reading, novice_typists, payer,
    powerless, biographical, trapped, national).

% Employed under QWERTY-certified hiring standards, bear the ergonomic and productivity costs of a layout not optimized for their labor, and have no institutional channel to demand or credential an alternative because employers hire against the certification bodies' single benchmark.
narrative_ontology:constraint_stakeholder(qwerty_persistence_mechanism__beneficiary_extraction_reading, clerical_workers, payer,
    powerless, biographical, trapped, national).

% Designed and patented alternative layouts (e.g., Dvorak) demonstrating typing efficiency gains, but could not get manufacturers to tool hardware, schools to teach the layout, or certification bodies to recognize it. Excluded from the entire distribution and credentialing chain that determines whether an alternative layout can ever reach market-relevant adoption.
narrative_ontology:constraint_stakeholder(qwerty_persistence_mechanism__beneficiary_extraction_reading, rival_keyboard_inventors, excluded,
    powerless, biographical, trapped, national).

% Study the QWERTY case as a canonical example of path dependence, debating whether persistence reflects genuine coordination lock-in, deliberate rent protection by incumbents, or adequate performance under fair competition. Have no stake in the outcome but produce the competing readings this kernel comprises.
narrative_ontology:constraint_stakeholder(qwerty_persistence_mechanism__beneficiary_extraction_reading, economic_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(qwerty_persistence_mechanism__beneficiary_extraction_reading, remington_union_typewriter_trust).
narrative_ontology:fixing_cost_class(qwerty_persistence_mechanism__beneficiary_extraction_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: A single, universally taught keyboard layout lets employers hire interchangeable typists and lets typists move between employers without retraining — genuine coordination value exists in having ONE standard, whichever it is.
% TRANSFER_FUNCTION: Moves training-investment protection and certification-gatekeeping rents from novice typists, clerical workers, and rival inventors to the incumbent manufacturer, the licensed typing schools, and the certification bodies whose curricula and credentials are sunk into QWERTY specifically.
% ABSENT_VOICES: Rival keyboard inventors and the typists who would have benefited from a more efficient layout are structurally absent from the standard-setting conversation: manufacturers and schools that already profit from QWERTY control which layout gets tooled, taught, and certified, so the alternative's advocates never reach the decision table.
% DISAPPEARANCE_RATIONALE: If the coordinated maintenance of QWERTY (manufacturer tooling defaults, school curricula, certification benchmarks) disappeared overnight, hardware makers would be free to tool alternative layouts, schools could teach whichever layout tested fastest, and certification could re-benchmark — typists and employers would very plausibly reorganize around a layout selected on efficiency grounds rather than incumbency, given documented efficiency claims for alternatives that never got a fair market test.
% FOUNDING_PROBLEM: In the 1870s-80s, mechanical typewriters jammed when adjacent-key strikers collided; QWERTY was arranged partly to slow typists down and separate commonly-paired letters to reduce jamming, plus early manufacturer branding needs (the top row could type 'TYPE WRITER QUOTE' for salesmen).
% FOUNDING_PROBLEM_CORROBORATION: Mechanical engineers and typewriter historians outside the incumbent manufacturers attest that jam-prevention became irrelevant once electric and then electronic keyboards eliminated mechanical strikers entirely (by the mid-20th century); independent ergonomics researchers (not funded by typing-school or certification-body interests) have published layout-efficiency comparisons showing QWERTY's continued dominance persists for reasons unrelated to the original mechanical constraint. No corroboration for continued necessity comes from outside the manufacturer/school/certification beneficiary set.
narrative_ontology:disappearance_verdict(qwerty_persistence_mechanism__beneficiary_extraction_reading, world_rearranges).
narrative_ontology:founding_problem_status(qwerty_persistence_mechanism__beneficiary_extraction_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(qwerty_persistence_mechanism__beneficiary_extraction_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
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
 *   Extractiveness rises over the century (0.35 to 0.68) as the mechanical rationale dies but the beneficiary coalition's certification and curriculum lock-in hardens — extraction accumulates precisely as the founding problem becomes dead. Suppression is authored high (0.72) because persistence in this reading depends on active exclusion: certification bodies refuse to build parallel tracks, schools lobby against curriculum change, and manufacturers do not tool alternative layouts at scale, foreclosing exit for typists and inventors alike. Theater ratio rises moderately (0.10 to 0.45) as 'typing efficiency' framing becomes increasingly performative cover for what is functionally credential-gatekeeping.
 *
 * PERSPECTIVAL GAP:
 *   From the manufacturer/school/certification seat, QWERTY-standardization is a genuine service — it lets employers hire interchangeably. From the novice-typist and rival-inventor seat, the same standardization operates as a closed credentialing gate that never let a superior layout compete on merits. The engine computes this divergence from the structural beneficiary/victim/enforcement data; this story does not adjudicate which seat is 'right' — it authors the extraction-reading's structural claim.
 *
 * DIRECTIONALITY LOGIC:
 *   Remington/Union Typewriter and the certification bodies sit at the beneficiary end (d low): they collect licensing revenue, tuition, and gatekeeping rents from a standard they administer and could change but choose not to. Novice typists and clerical workers sit at the target end (d high): trapped exit options, their sunk training investment is exactly the switching cost the arrangement is designed to preserve. Rival inventors are excluded entirely from the standard-setting apparatus rather than coordinated by it — their exclusion is the enforcement object, not a side effect.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (mechanical key-jamming) is dead, but the arrangement persists — this is the mandatrophy signature. Corroboration for continued necessity comes only from within the beneficiary set (manufacturers, schools, certification bodies); independent ergonomics research and typewriter historians outside that set attest the mechanical rationale ended with electric keyboards. This reading treats that mismatch (status=dead + verdict=world_rearranges) as diagnostic of extraction dressed as coordination, distinguishing it from the lock_in_reading (which would attribute persistence to coordination failure with no identifiable profiteer) and the naturalization_reading (which would deny any efficiency gap existed).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    active_maintenance_vs_passive_inertia,
    'Did Remington, Union Typewriter, and the typing schools actively lobby against and suppress alternative layouts (e.g., Dvorak), or did QWERTY simply persist through ordinary switching-cost inertia with no identifiable coordinated suppression?',
    'Archival research into 19th/20th century typewriter manufacturer and typing-school trade association records, patent-dispute filings, and certification-body policy history to establish whether documented lobbying or exclusionary certification policy exists, versus mere absence of adoption.',
    'If active suppression is documented, this reading (beneficiary_extraction) is the historically correct one and the constraint is properly tangled_rope/snare-adjacent. If no coordinated suppression is found and persistence reflects pure network-effect coordination cost, the lock_in_reading is the better structural account and this story''s victim/beneficiary framing overstates intentionality.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(active_maintenance_vs_passive_inertia, empirical, 'Whether QWERTY persistence involved documented active suppression versus passive coordination inertia.').

omega_variable(
    dvorak_efficiency_gap_magnitude,
    'How large was the actual typing-efficiency gap between QWERTY and alternative layouts like Dvorak, and were early efficiency studies (some funded by Dvorak''s own promoters) methodologically sound?',
    'Independent modern ergonomic and cognitive-load replication studies controlling for training effects, conducted by researchers with no stake in either layout''s adoption.',
    'A large, well-replicated efficiency gap strengthens the extraction reading (suppressing a clearly superior alternative for rent-protection). A negligible or contested gap strengthens the naturalization_reading (QWERTY was adequate all along, and the ''suppression'' narrative overstates what was lost).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(dvorak_efficiency_gap_magnitude, empirical, 'Whether the alternative layout''s claimed efficiency advantage is real and substantial or overstated.').

omega_variable(
    certification_gatekeeping_intent,
    'Did certification bodies decline to build parallel non-QWERTY certification tracks out of active protection of sunk curriculum investment, or simply because market demand for alternative-layout certification never materialized?',
    'Review of certification body internal policy discussions or public statements regarding alternative-layout proposals, and market data on employer demand for non-QWERTY-certified typists over the period.',
    'Intentional gatekeeping supports treating the certification bodies as active beneficiaries maintaining an extractive gate; absence-of-demand would suggest the bodies were passively responding to (rather than manufacturing) market conditions, weakening the tangled_rope classification toward something closer to rope or piton.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(certification_gatekeeping_intent, conceptual, 'Whether certification-body inaction reflects active protectionism or passive market response.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(qwerty_persistence_mechanism__beneficiary_extraction_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qwer_tr_t0, qwerty_persistence_mechanism__beneficiary_extraction_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement_basis(qwer_tr_t0, observed).
narrative_ontology:measurement(qwer_tr_t20, qwerty_persistence_mechanism__beneficiary_extraction_reading, theater_ratio, 20, 0.2).
narrative_ontology:measurement_basis(qwer_tr_t20, observed).
narrative_ontology:measurement(qwer_tr_t40, qwerty_persistence_mechanism__beneficiary_extraction_reading, theater_ratio, 40, 0.3).
narrative_ontology:measurement_basis(qwer_tr_t40, observed).
narrative_ontology:measurement(qwer_tr_t60, qwerty_persistence_mechanism__beneficiary_extraction_reading, theater_ratio, 60, 0.37).
narrative_ontology:measurement_basis(qwer_tr_t60, observed).
narrative_ontology:measurement(qwer_tr_t80, qwerty_persistence_mechanism__beneficiary_extraction_reading, theater_ratio, 80, 0.42).
narrative_ontology:measurement_basis(qwer_tr_t80, observed).
narrative_ontology:measurement(qwer_tr_t100, qwerty_persistence_mechanism__beneficiary_extraction_reading, theater_ratio, 100, 0.45).
narrative_ontology:measurement_basis(qwer_tr_t100, observed).

% Extraction over time
narrative_ontology:measurement(qwer_be_t0, qwerty_persistence_mechanism__beneficiary_extraction_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement_basis(qwer_be_t0, observed).
narrative_ontology:measurement(qwer_be_t20, qwerty_persistence_mechanism__beneficiary_extraction_reading, base_extractiveness, 20, 0.48).
narrative_ontology:measurement_basis(qwer_be_t20, observed).
narrative_ontology:measurement(qwer_be_t40, qwerty_persistence_mechanism__beneficiary_extraction_reading, base_extractiveness, 40, 0.58).
narrative_ontology:measurement_basis(qwer_be_t40, observed).
narrative_ontology:measurement(qwer_be_t60, qwerty_persistence_mechanism__beneficiary_extraction_reading, base_extractiveness, 60, 0.63).
narrative_ontology:measurement_basis(qwer_be_t60, observed).
narrative_ontology:measurement(qwer_be_t80, qwerty_persistence_mechanism__beneficiary_extraction_reading, base_extractiveness, 80, 0.66).
narrative_ontology:measurement_basis(qwer_be_t80, observed).
narrative_ontology:measurement(qwer_be_t100, qwerty_persistence_mechanism__beneficiary_extraction_reading, base_extractiveness, 100, 0.68).
narrative_ontology:measurement_basis(qwer_be_t100, observed).

% Suppression requirement over time
narrative_ontology:measurement(qwer_su_t0, qwerty_persistence_mechanism__beneficiary_extraction_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement_basis(qwer_su_t0, observed).
narrative_ontology:measurement(qwer_su_t20, qwerty_persistence_mechanism__beneficiary_extraction_reading, suppression_requirement, 20, 0.58).
narrative_ontology:measurement_basis(qwer_su_t20, observed).
narrative_ontology:measurement(qwer_su_t40, qwerty_persistence_mechanism__beneficiary_extraction_reading, suppression_requirement, 40, 0.63).
narrative_ontology:measurement_basis(qwer_su_t40, observed).
narrative_ontology:measurement(qwer_su_t60, qwerty_persistence_mechanism__beneficiary_extraction_reading, suppression_requirement, 60, 0.67).
narrative_ontology:measurement_basis(qwer_su_t60, observed).
narrative_ontology:measurement(qwer_su_t80, qwerty_persistence_mechanism__beneficiary_extraction_reading, suppression_requirement, 80, 0.7).
narrative_ontology:measurement_basis(qwer_su_t80, observed).
narrative_ontology:measurement(qwer_su_t100, qwerty_persistence_mechanism__beneficiary_extraction_reading, suppression_requirement, 100, 0.72).
narrative_ontology:measurement_basis(qwer_su_t100, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(qwerty_persistence_mechanism__beneficiary_extraction_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(qwerty_persistence_mechanism__beneficiary_extraction_reading, 0.08).
narrative_ontology:affects_constraint(qwerty_persistence_mechanism__beneficiary_extraction_reading, qwerty_persistence_mechanism__lock_in_reading).
narrative_ontology:affects_constraint(qwerty_persistence_mechanism__beneficiary_extraction_reading, qwerty_persistence_mechanism__naturalization_reading).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the qwerty_persistence_mechanism kernel. beneficiary_extraction_reading (this file) claims tangled_rope with identifiable beneficiaries and active suppression. lock_in_reading claims a coordination-failure structure with no concentrated profiteer (likely rope or piton at the network-effect level). naturalization_reading claims the layout is genuinely adequate and alternatives lapsed through fair competition (likely mountain-adjacent or rope, denying suppression entirely). Each carries its own epsilon and stakeholder structure; they are linked via affects_constraints rather than merged, per the epsilon-invariance principle — the label 'QWERTY persistence' conflates three structurally distinct historical claims.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
