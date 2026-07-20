% ============================================================================
% CONSTRAINT STORY: salic_prohibition__sovereign_override_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_salic_prohibition__sovereign_override_reading, []).

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
 *   constraint_id: salic_prohibition__sovereign_override_reading
 *   human_readable: Salic Law as Revocable Positive Law Subject to Sovereign Legislative Authority
 *   domain: constitutional/political/historical
 *
 * SUMMARY:
 *   The Salic Law prohibition on female succession, interpreted as revocable
 *   positive law subject to sovereign legislative authority. Under this
 *   reading, the monarch retains the authority to override the prohibition
 *   through acts such as the Pragmatic Sanction when dynastic continuity
 *   demands it, while challengers to male agnatic succession are framed as
 *   rebels against legitimate sovereign authority. The constraint coordinates
 *   dynastic stability by preventing territorial fragmentation but
 *   asymmetrically extracts political sovereignty from women and cognatic
 *   claimants.
 *
 * KEY AGENTS:
 *   - sovereign_legislator: Agenda-setter (institutional/generational/constrained) â claims monopoly on succession legislation
 *   - male_dynastic_heirs: Primary beneficiary (powerful/biographical/constrained) â inherit precedence by gender
 *   - female_claimants: Primary target (moderate/biographical/constrained) â excluded from succession, delegitimized when challenging
 *   - nobility_administrative_class: Secondary beneficiary (organized/generational/constrained) â benefits from stability
 *   - foreign_cognatic_powers: Excluded (powerful/biographical/trapped) â support female claims but treated as rebels
 *   - constitutional_historians: Observer (analytical/civilizational/analytical) â interpret and record the structure
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(salic_prohibition__sovereign_override_reading, 0.68).
domain_priors:suppression_score(salic_prohibition__sovereign_override_reading, 0.75).
domain_priors:theater_ratio(salic_prohibition__sovereign_override_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(salic_prohibition__sovereign_override_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(salic_prohibition__sovereign_override_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(salic_prohibition__sovereign_override_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(salic_prohibition__sovereign_override_reading, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(salic_prohibition__sovereign_override_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(salic_prohibition__sovereign_override_reading, tangled_rope).
narrative_ontology:human_readable(salic_prohibition__sovereign_override_reading, "Salic Law as Revocable Positive Law Subject to Sovereign Legislative Authority").
narrative_ontology:topic_domain(salic_prohibition__sovereign_override_reading, "constitutional/political/historical").

domain_priors:requires_active_enforcement(salic_prohibition__sovereign_override_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(salic_prohibition__sovereign_override_reading, '0274eb6c-b544-42db-8d31-35d494d4c149').
narrative_ontology:cs_kernel_codification('0274eb6c-b544-42db-8d31-35d494d4c149', fixed_text).
narrative_ontology:cs_authority_grounding('0274eb6c-b544-42db-8d31-35d494d4c149', lineage).
narrative_ontology:cs_interpretation_layer_present('0274eb6c-b544-42db-8d31-35d494d4c149').
narrative_ontology:cs_reading_relation('0274eb6c-b544-42db-8d31-35d494d4c149', salic_prohibition__immutable_mandate_reading, forecloses).
narrative_ontology:cs_reading_relation('0274eb6c-b544-42db-8d31-35d494d4c149', salic_prohibition__cognatic_reversion_reading, coexists_with).
narrative_ontology:cs_axiom('0274eb6c-b544-42db-8d31-35d494d4c149', foundational, succession_law_positive_and_revisable).
narrative_ontology:cs_axiom_status(succession_law_positive_and_revisable, holdable).
narrative_ontology:cs_axiom_grounding('0274eb6c-b544-42db-8d31-35d494d4c149', succession_law_positive_and_revisable, conventional).
narrative_ontology:cs_axiom('0274eb6c-b544-42db-8d31-35d494d4c149', foundational, dynastic_continuity_over_agnatic_rigidity).
narrative_ontology:cs_axiom_status(dynastic_continuity_over_agnatic_rigidity, holdable).
narrative_ontology:cs_axiom_grounding('0274eb6c-b544-42db-8d31-35d494d4c149', dynastic_continuity_over_agnatic_rigidity, conventional).
narrative_ontology:cs_reference_frame('0274eb6c-b544-42db-8d31-35d494d4c149', dynastic_legislative_supremacy).
narrative_ontology:cs_drift_state('0274eb6c-b544-42db-8d31-35d494d4c149', constitutional_monarchy_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('0274eb6c-b544-42db-8d31-35d494d4c149', '').
narrative_ontology:cs_kernel_id(salic_prohibition__sovereign_override_reading, salic_prohibition).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(salic_prohibition__sovereign_override_reading, male_dynastic_heirs).
narrative_ontology:constraint_beneficiary(salic_prohibition__sovereign_override_reading, nobility_administrative_class).
narrative_ontology:constraint_victim(salic_prohibition__sovereign_override_reading, female_claimants).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Claims the authority to define and modify rules of dynastic succession through legislative acts such as Pragmatic Sanctions. Uses the Salic prohibition as a default rule while reserving the power to override it to prevent dynastic extinction or territorial fragmentation. Derives legitimacy from dynastic continuity and transmitted legislative sovereignty.
narrative_ontology:constraint_stakeholder(salic_prohibition__sovereign_override_reading, sovereign_legislator, agenda_setter,
    institutional, generational, constrained, national).

% Occupy the default position in the succession order by virtue of agnatic descent. Their claims to thrones and territories are protected by the exclusion of female and cognatic lines, reducing the pool of competitors.
narrative_ontology:constraint_stakeholder(salic_prohibition__sovereign_override_reading, male_dynastic_heirs, beneficiary,
    powerful, biographical, constrained, national).

% Born into dynastic families but excluded from succession to principal territories by the Salic prohibition. Their claims are treated as illegitimate or rebellious unless explicitly validated by a sovereign Pragmatic Sanction. Often seek support from foreign allies or through marriage alliances, but face institutional and military barriers to enforcement of their claims.
narrative_ontology:constraint_stakeholder(salic_prohibition__sovereign_override_reading, female_claimants, payer,
    moderate, biographical, constrained, national).

% Serve as executors and beneficiaries of a stable dynastic order. Their land tenure, offices, and privileges depend on predictable succession. They accept sovereign overrides when necessary to preserve the dynasty but generally support the male agnatic default as a bulwark against partition.
narrative_ontology:constraint_stakeholder(salic_prohibition__sovereign_override_reading, nobility_administrative_class, beneficiary,
    organized, generational, constrained, national).

% External sovereigns and dynastic houses allied to excluded female claimants through marriage or blood. They provide military and diplomatic support for cognatic claims but are treated as foreign interlopers or rebels by the dominant legal order, denied standing in domestic constitutional interpretation.
narrative_ontology:constraint_stakeholder(salic_prohibition__sovereign_override_reading, foreign_cognatic_powers, excluded,
    powerful, biographical, trapped, continental).

% Record, interpret, and debate the historical and juridical basis of succession rules. They analyze the textual origins of the Salic Law, the authority of Pragmatic Sanctions, and the outcomes of succession conflicts, without direct stake in the succession itself.
narrative_ontology:constraint_stakeholder(salic_prohibition__sovereign_override_reading, constitutional_historians, observer,
    analytical, civilizational, analytical, continental).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Prevents fragmentation of dynastic territories through multiple competing claims by establishing a clear, sex-based order of succession, and allows the sovereign to modify this order legislatively to preserve dynastic continuity in exceptional circumstances.
% TRANSFER_FUNCTION: Transfers the right of dynastic succession from female and cognatic lines to male agnatic lines, and concentrates the authority to define succession rules in the sovereign legislator.
% ABSENT_VOICES: Female claimants are formally excluded from the succession conversation; foreign powers allied to cognatic claimants are treated as external rebels rather than legitimate parties to constitutional interpretation.
% DISAPPEARANCE_RATIONALE: If the constraint vanished, competing claims by female heirs and their foreign allies would immediately resurface, triggering succession crises, territorial partitions, and challenges to the sovereign's monopoly on legislative authority over dynastic rules.
% FOUNDING_PROBLEM: The partition of dynastic territories among multiple heirs led to state fragmentation, weak central authority, and endemic dynastic warfare.
% FOUNDING_PROBLEM_CORROBORATION: Dynastic chronicles and legal historians outside the immediate beneficiary class attest to the fragmentation risks of partible inheritance; however, feminist legal historians and comparative constitutional scholars contest whether gender exclusion was necessary to solve it.
narrative_ontology:disappearance_verdict(salic_prohibition__sovereign_override_reading, world_rearranges).
narrative_ontology:founding_problem_status(salic_prohibition__sovereign_override_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(salic_prohibition__sovereign_override_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(salic_prohibition__sovereign_override_reading, 'none', 1).
narrative_ontology:epsilon_provenance(salic_prohibition__sovereign_override_reading, 0.68, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(salic_prohibition__sovereign_override_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(salic_prohibition__sovereign_override_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(salic_prohibition__sovereign_override_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is substantial (0.68) because half the potential claimant pool is excluded by gender, concentrating dynastic power. Suppression is high (0.75) because the constraint's persistence requires military and legal suppression of female claimants and their foreign allies (e.g., War of Austrian Succession). Theater ratio rises over time (0.45 to 0.70) as the sovereign override power becomes more frequently invoked, revealing the prohibition's contingent rather than natural basis; by the 19th century the constraint is largely performative. Accessibility collapse is high (0.80) because within the dynastic framework, no non-male succession path is institutionally legible without sovereign intervention. Resistance is moderate (0.55) because excluded claimants and foreign powers mount sustained but ultimately unsuccessful challenges.
 *
 * PERSPECTIVAL GAP:
 *   From the sovereign legislator's seat, the constraint is a flexible instrument of statecraft preserving dynastic continuity; from the female claimant's seat, it is an arbitrary gender exclusion backed by force. The male heir and administrative nobility experience it as protective coordination. Foreign powers experience it as a barrier to legitimate alliance through marriage and succession. The engine should compute seat divergence: sovereign and male heirs near the beneficiary end, female claimants near the target end.
 *
 * DIRECTIONALITY LOGIC:
 *   Sovereign legislator and male dynastic heirs are structural beneficiaries (d near 0.0â0.2): they gain concentrated authority and automatic precedence. Female claimants are structural targets (d near 0.9â1.0): they bear the cost of exclusion and delegitimization. The nobility/administrative class sits near symmetric (d ~0.4): they gain stability but have no individual exit from the dynastic system. Foreign powers are excluded with no structural voice (d ~0.8, trapped exit).
 *
 * MANDATROPHY ANALYSIS:
 *   The sovereign override reading prevents mandatrophy mislabeling by preserving a genuine coordination function (preventing territorial fragmentation and succession wars) alongside the extraction. Without the coordination story, the constraint would read as pure gender snare; without the extraction story, it would read as neutral rope. The tangled_rope classification captures that both are present and structurally coupled: the same rule that coordinates stability also extracts sovereignty from women.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sovereign_legislative_authenticity,
    'Is the sovereign''s override authority a genuine legislative power or an ad hoc post-hoc justification for power grabs?',
    'Historical pattern analysis of Pragmatic Sanctions â do they precede or follow crises? Systematic review of juristic writings on the nature of sovereign authority over succession.',
    'If always post-crisis, the positive law framing is cover for raw power; if systematically used for continuity planning, it is genuine legal authority.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sovereign_legislative_authenticity, empirical, 'Whether sovereign override is genuine legislation or post-hoc power assertion').

omega_variable(
    gender_exclusion_necessity,
    'Does the dynastic stability achieved by Salic Law require gender exclusion specifically, or did the same stability obtain in jurisdictions without it?',
    'Comparative dynastic history of cognatic succession systems (e.g., Spain, Portugal, some German territories) measuring fragmentation and civil war incidence.',
    'If cognatic systems were equally stable, the coordination function is separable from the extraction, strengthening the snare/tangled_rope reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(gender_exclusion_necessity, empirical, 'Whether coordination requires gender exclusion or is separable from it').

omega_variable(
    reading_boundary_ambiguity,
    'Does the sovereign_override_reading collapse into the immutable_mandate_reading when the sovereign does not exercise override, or remain distinct in kind?',
    'Analyze juristic writings â do theorists of sovereign override treat the prohibition as default-positive or default-natural?',
    'If collapse occurs, the reading is unstable and the constraint family should be reclassified.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_boundary_ambiguity, conceptual, 'Stability of the sovereign_override reading boundary against immutable_mandate').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(salic_prohibition__sovereign_override_reading, 0, 500).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sali_tr_t0, salic_prohibition__sovereign_override_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(sali_tr_t100, salic_prohibition__sovereign_override_reading, theater_ratio, 100, 0.3).
narrative_ontology:measurement(sali_tr_t200, salic_prohibition__sovereign_override_reading, theater_ratio, 200, 0.4).
narrative_ontology:measurement(sali_tr_t300, salic_prohibition__sovereign_override_reading, theater_ratio, 300, 0.5).
narrative_ontology:measurement(sali_tr_t400, salic_prohibition__sovereign_override_reading, theater_ratio, 400, 0.6).
narrative_ontology:measurement(sali_tr_t500, salic_prohibition__sovereign_override_reading, theater_ratio, 500, 0.7).

% Extraction over time
narrative_ontology:measurement(sali_be_t0, salic_prohibition__sovereign_override_reading, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(sali_be_t100, salic_prohibition__sovereign_override_reading, base_extractiveness, 100, 0.55).
narrative_ontology:measurement(sali_be_t200, salic_prohibition__sovereign_override_reading, base_extractiveness, 200, 0.65).
narrative_ontology:measurement(sali_be_t300, salic_prohibition__sovereign_override_reading, base_extractiveness, 300, 0.7).
narrative_ontology:measurement(sali_be_t400, salic_prohibition__sovereign_override_reading, base_extractiveness, 400, 0.65).
narrative_ontology:measurement(sali_be_t500, salic_prohibition__sovereign_override_reading, base_extractiveness, 500, 0.55).

% Suppression requirement over time
narrative_ontology:measurement(sali_su_t0, salic_prohibition__sovereign_override_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(sali_su_t100, salic_prohibition__sovereign_override_reading, suppression_requirement, 100, 0.6).
narrative_ontology:measurement(sali_su_t200, salic_prohibition__sovereign_override_reading, suppression_requirement, 200, 0.7).
narrative_ontology:measurement(sali_su_t300, salic_prohibition__sovereign_override_reading, suppression_requirement, 300, 0.75).
narrative_ontology:measurement(sali_su_t400, salic_prohibition__sovereign_override_reading, suppression_requirement, 400, 0.65).
narrative_ontology:measurement(sali_su_t500, salic_prohibition__sovereign_override_reading, suppression_requirement, 500, 0.5).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(salic_prohibition__sovereign_override_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(salic_prohibition__sovereign_override_reading, immutable_mandate_reading).
narrative_ontology:affects_constraint(salic_prohibition__sovereign_override_reading, cognatic_reversion_reading).

% DUAL FORMULATION NOTE:
% This constraint is the sovereign_override_reading of the salic_prohibition kernel, treating the prohibition as revocable positive law. Sibling constraints (immutable_mandate_reading, cognatic_reversion_reading) instantiate mutually contesting readings of the same kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
