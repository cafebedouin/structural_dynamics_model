% ============================================================================
% CONSTRAINT STORY: derivative_work_statutory_boundary__enclosure_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_derivative_work_statutory_boundary__enclosure_reading, []).

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
 *   constraint_id: derivative_work_statutory_boundary__enclosure_reading
 *   human_readable: Derivative Work Statutory Boundary â Enclosure Reading
 *   domain: legal/intellectual_property
 *
 * SUMMARY:
 *   This constraint story captures the enclosure_reading of the
 *   derivative_work_statutory_boundary kernel: the interpretation that any
 *   use of copyrighted expression in a new work constitutes preparation of a
 *   derivative work. This reading transforms copyright from a limited
 *   monopoly over specific markets into a broad enclosure of all downstream
 *   expressive re-use, requiring pre-creation licensing and capturing
 *   extraction for incumbent rights holders. It is claimed here as a snare
 *   â pure extraction with coordination as cover â with high
 *   extractiveness and suppression metrics authored independently of the
 *   claim.
 *
 * KEY AGENTS:
 *   - legacy_rights_holders (institutional/arbitrage): Primary beneficiary â collects licensing rents and veto leverage
 *   - remix_artists (moderate/constrained): Primary target â bears legal risk and licensing costs for transformative practice
 *   - generative_ai_labs (powerful/constrained): Secondary target â faces aggregate liability for training data use
 *   - public_domain_advocates (organized/analytical): Analytical observer â contests the reading from outside the extraction flow
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(derivative_work_statutory_boundary__enclosure_reading, 0.82).
domain_priors:suppression_score(derivative_work_statutory_boundary__enclosure_reading, 0.78).
domain_priors:theater_ratio(derivative_work_statutory_boundary__enclosure_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(derivative_work_statutory_boundary__enclosure_reading, extractiveness, 0.82).
narrative_ontology:constraint_metric(derivative_work_statutory_boundary__enclosure_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(derivative_work_statutory_boundary__enclosure_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(derivative_work_statutory_boundary__enclosure_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(derivative_work_statutory_boundary__enclosure_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(derivative_work_statutory_boundary__enclosure_reading, snare).
narrative_ontology:human_readable(derivative_work_statutory_boundary__enclosure_reading, "Derivative Work Statutory Boundary â Enclosure Reading").
narrative_ontology:topic_domain(derivative_work_statutory_boundary__enclosure_reading, "legal/intellectual_property").

domain_priors:requires_active_enforcement(derivative_work_statutory_boundary__enclosure_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(derivative_work_statutory_boundary__enclosure_reading, '75982a8e-8f2d-4faa-b97b-cfe6633c92ab').
narrative_ontology:cs_kernel_codification('75982a8e-8f2d-4faa-b97b-cfe6633c92ab', formalized).
narrative_ontology:cs_authority_grounding('75982a8e-8f2d-4faa-b97b-cfe6633c92ab', lineage).
narrative_ontology:cs_interpretation_layer_present('75982a8e-8f2d-4faa-b97b-cfe6633c92ab').
narrative_ontology:cs_reading_relation('75982a8e-8f2d-4faa-b97b-cfe6633c92ab', derivative_work_statutory_boundary__coordination_reading, coexists_with).
narrative_ontology:cs_reading_relation('75982a8e-8f2d-4faa-b97b-cfe6633c92ab', derivative_work_statutory_boundary__hybrid_carveout_reading, coexists_with).
narrative_ontology:cs_axiom('75982a8e-8f2d-4faa-b97b-cfe6633c92ab', foundational, all_expression_use_requires_authorization).
narrative_ontology:cs_axiom_status(all_expression_use_requires_authorization, holdable).
narrative_ontology:cs_axiom_grounding('75982a8e-8f2d-4faa-b97b-cfe6633c92ab', all_expression_use_requires_authorization, conventional).
narrative_ontology:cs_axiom('75982a8e-8f2d-4faa-b97b-cfe6633c92ab', foundational, no_transformative_immunity).
narrative_ontology:cs_axiom_status(no_transformative_immunity, holdable).
narrative_ontology:cs_axiom_grounding('75982a8e-8f2d-4faa-b97b-cfe6633c92ab', no_transformative_immunity, conventional).
narrative_ontology:cs_reference_frame('75982a8e-8f2d-4faa-b97b-cfe6633c92ab', maximalist_authorial_control).
narrative_ontology:cs_drift_state('75982a8e-8f2d-4faa-b97b-cfe6633c92ab', digital_remix_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('75982a8e-8f2d-4faa-b97b-cfe6633c92ab', '').
narrative_ontology:cs_kernel_id(derivative_work_statutory_boundary__enclosure_reading, derivative_work_statutory_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(derivative_work_statutory_boundary__enclosure_reading, legacy_rights_holders).
narrative_ontology:constraint_victim(derivative_work_statutory_boundary__enclosure_reading, remix_artists).
narrative_ontology:constraint_victim(derivative_work_statutory_boundary__enclosure_reading, generative_ai_labs).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Control vast catalogs of copyrighted expression. Under the enclosure reading, any use of their expression in new works triggers a derivative work right, generating licensing leverage and veto power over downstream innovation markets. They capture rents through pre-creation licensing and statutory damage threats.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__enclosure_reading, legacy_rights_holders, beneficiary,
    institutional, generational, arbitrage, global).

% Create new works by sampling, remixing, or transforming existing copyrighted expression. The enclosure reading treats every such use as infringing unless licensed, forcing them into expensive clearance processes or legal precarity. Their creative practice is structurally penalized.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__enclosure_reading, remix_artists, payer,
    moderate, biographical, constrained, global).

% Develop generative AI models by training on copyrighted works. The enclosure reading defines this training as mass preparation of unauthorized derivative works, exposing them to aggregate liability that threatens commercial viability and forces negotiation with fragmented rights holder collectives.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__enclosure_reading, generative_ai_labs, payer,
    powerful, biographical, constrained, global).

% Legal scholars and non-profit advocates arguing for narrower derivative work boundaries and robust fair use. They observe the extraction but do not directly pay or benefit; they contest the reading through amicus briefs and policy advocacy.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__enclosure_reading, public_domain_advocates, observer,
    organized, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(derivative_work_statutory_boundary__enclosure_reading, legacy_rights_holders).
narrative_ontology:fixing_cost_class(derivative_work_statutory_boundary__enclosure_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a formal mechanism for rights holders to authorize or prohibit adaptations of their works, ostensibly ensuring that expressive value flows back to originators and preventing market fragmentation from unauthorized sequels or translations.
% TRANSFER_FUNCTION: Moves licensing revenue, veto power, and litigation leverage from downstream creators and technology developers to incumbent rights holders; moves legal risk, project abandonment, and compliance costs from rights holders to downstream users.
% ABSENT_VOICES: Amateur fan creators, small independent remixers without legal representation, and open-source AI researchers are excluded from legislative drafting and industry licensing negotiations; their practices are criminalized or marginalized without a seat at the table.
% DISAPPEARANCE_RATIONALE: If the enclosure reading vanished, remix culture, fan fiction, sampling music, and AI training would proceed without pre-creation licensing gates; incumbent licensing revenue would collapse; courts would revert to narrower fixation or transformative-purpose tests; the information economy would reorganize around reuse rather than permission.
% FOUNDING_PROBLEM: Pre-statutory common law failed to ensure that authors could control or profit from adaptations, translations, and abridgments of their works, allowing third parties to free-ride on creative investment.
% FOUNDING_PROBLEM_CORROBORATION: Copyright historians and empirical legal scholars attest that modern licensing markets and narrower fair use doctrines already solve the adaptation free-rider problem; rights holder trade associations assert the problem persists, but no independent corroboration from outside the beneficiary set supports the maximalist enclosure framing.
narrative_ontology:disappearance_verdict(derivative_work_statutory_boundary__enclosure_reading, world_rearranges).
narrative_ontology:founding_problem_status(derivative_work_statutory_boundary__enclosure_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(derivative_work_statutory_boundary__enclosure_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(derivative_work_statutory_boundary__enclosure_reading, 'none', 1).
narrative_ontology:epsilon_provenance(derivative_work_statutory_boundary__enclosure_reading, 0.82, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(derivative_work_statutory_boundary__enclosure_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(derivative_work_statutory_boundary__enclosure_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(derivative_work_statutory_boundary__enclosure_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.82) is high because the reading captures all downstream uses of expression regardless of transformative purpose or market effect, funneling value to incumbents. Suppression (0.78) is high because the constraint depends on active legal machinery â statutory damages, DMCA takedown, injunction â to prevent unauthorized creation. Theater ratio (0.45) reflects that a substantial share of enforcement activity is performative: automated takedowns of non-infringing works, litigation threats against commercially non-competing uses, and compliance theater that exceeds actual market harm. Accessibility collapse (0.72) is high because fair use and independent creation are theoretically available alternatives that collapse in practice under legal uncertainty and litigation cost. Resistance (0.68) reflects organized pushback from technology sectors, remix communities, and public domain advocates. The founding problem is dead: direct adaptation markets are already served by narrower readings and licensing, so the enclosure reading persists as extraction beyond its obsolete justification.
 *
 * PERSPECTIVAL GAP:
 *   From the legacy rights holder seat, the constraint is legitimate property protection against free-riding; from the remix artist and AI lab seats, it is an innovation tax enforced by legal coercion. The engine computes this divergence from structural data: the beneficiary has institutional power and arbitrage-grade exit (can monetize globally), while targets are constrained by statutory threat and fragmented rights landscapes.
 *
 * DIRECTIONALITY LOGIC:
 *   Legacy rights holders are declared beneficiaries with global scope and arbitrage exit, producing low directionality (subsidy). Remix artists and generative AI labs are declared victims with constrained exit, producing high directionality (target). Public domain advocates are analytical observers with no structural capture. Effective extraction is therefore amplified for the creator and technology seats and damped for the incumbent seat.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem â uncompensated direct adaptations â is dead, corroborated by historians outside the beneficiary set. The disappearance verdict is world_rearranges: the constraint still shapes arrangements. This dead-problem + active-arrangement mismatch flags the constraint as a zombie/snare rather than a rope or scaffold, preventing misclassification as benign coordination.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    statutory_kernel_ambiguity,
    'Does the Copyright Act''s statutory definition of derivative work inherently support the enclosure reading, or is the reading a judicial expansion enabled by ambiguous statutory language?',
    'Textual analysis of the statutory definition against the legislative history, compared with the judicial trajectory that produced the enclosure reading.',
    'If the text inherently supports enclosure, the constraint is a snare embedded in the kernel itself; if it is a judicial expansion, the constraint is a snare layered onto a more moderate kernel.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(statutory_kernel_ambiguity, conceptual, 'Whether the statutory text mandates or merely permits the enclosure reading').

omega_variable(
    downstream_welfare_tradeoff,
    'Does the extraction from remix artists and AI labs under the enclosure reading produce net welfare gains by funding primary creation, or net losses by suppressing transformative innovation?',
    'Empirical economic analysis comparing incumbent licensing revenue against quantified downstream innovation suppression and deadweight loss from abandoned projects.',
    'A net welfare loss would support regulatory or legislative narrowing; a net gain would support the incumbent''s property-framing.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(downstream_welfare_tradeoff, empirical, 'Net welfare effect of enclosure on downstream innovation').

omega_variable(
    kernel_reading_compatibility,
    'Can the statutory kernel support the enclosure reading without logically foreclosing the coordination and hybrid carveout readings, or are these structurally incompatible interpretations of the same text?',
    'Comparative doctrinal analysis of the core premises: if the kernel contains an irreducible ambiguity, the readings are compatible interpretations; if it contains a dispositive command, one reading forecloses the others.',
    'If structurally incompatible, the dispute is zero-sum and will be resolved by power; if compatible, the kernel sustains productive legal pluralism.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_compatibility, conceptual, 'Structural compatibility of sibling readings within the same kernel').

omega_variable(
    enforcement_automation_impact,
    'Has the shift to automated enforcement (Content ID, DMCA bots) increased effective suppression beyond the statutory design, creating extraction that the kernel itself does not mandate?',
    'Comparison of pre-automation and post-automation takedown rates, false-positive rates, and settlement pressures against the statutory damage schedule.',
    'If automation amplifies suppression beyond statutory intent, the effective extraction is higher than the kernel''s Îµ and the snare classification strengthens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_automation_impact, empirical, 'Automation-driven suppression amplification beyond statutory design').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(derivative_work_statutory_boundary__enclosure_reading, 0, 35).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(deri_tr_t0, derivative_work_statutory_boundary__enclosure_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(deri_tr_t5, derivative_work_statutory_boundary__enclosure_reading, theater_ratio, 5, 0.25).
narrative_ontology:measurement(deri_tr_t10, derivative_work_statutory_boundary__enclosure_reading, theater_ratio, 10, 0.3).
narrative_ontology:measurement(deri_tr_t15, derivative_work_statutory_boundary__enclosure_reading, theater_ratio, 15, 0.35).
narrative_ontology:measurement(deri_tr_t20, derivative_work_statutory_boundary__enclosure_reading, theater_ratio, 20, 0.38).
narrative_ontology:measurement(deri_tr_t25, derivative_work_statutory_boundary__enclosure_reading, theater_ratio, 25, 0.4).
narrative_ontology:measurement(deri_tr_t30, derivative_work_statutory_boundary__enclosure_reading, theater_ratio, 30, 0.42).
narrative_ontology:measurement(deri_tr_t35, derivative_work_statutory_boundary__enclosure_reading, theater_ratio, 35, 0.45).

% Extraction over time
narrative_ontology:measurement(deri_be_t0, derivative_work_statutory_boundary__enclosure_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(deri_be_t5, derivative_work_statutory_boundary__enclosure_reading, base_extractiveness, 5, 0.6).
narrative_ontology:measurement(deri_be_t10, derivative_work_statutory_boundary__enclosure_reading, base_extractiveness, 10, 0.65).
narrative_ontology:measurement(deri_be_t15, derivative_work_statutory_boundary__enclosure_reading, base_extractiveness, 15, 0.7).
narrative_ontology:measurement(deri_be_t20, derivative_work_statutory_boundary__enclosure_reading, base_extractiveness, 20, 0.74).
narrative_ontology:measurement(deri_be_t25, derivative_work_statutory_boundary__enclosure_reading, base_extractiveness, 25, 0.78).
narrative_ontology:measurement(deri_be_t30, derivative_work_statutory_boundary__enclosure_reading, base_extractiveness, 30, 0.8).
narrative_ontology:measurement(deri_be_t35, derivative_work_statutory_boundary__enclosure_reading, base_extractiveness, 35, 0.82).

% Suppression requirement over time
narrative_ontology:measurement(deri_su_t0, derivative_work_statutory_boundary__enclosure_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(deri_su_t5, derivative_work_statutory_boundary__enclosure_reading, suppression_requirement, 5, 0.52).
narrative_ontology:measurement(deri_su_t10, derivative_work_statutory_boundary__enclosure_reading, suppression_requirement, 10, 0.6).
narrative_ontology:measurement(deri_su_t15, derivative_work_statutory_boundary__enclosure_reading, suppression_requirement, 15, 0.68).
narrative_ontology:measurement(deri_su_t20, derivative_work_statutory_boundary__enclosure_reading, suppression_requirement, 20, 0.72).
narrative_ontology:measurement(deri_su_t25, derivative_work_statutory_boundary__enclosure_reading, suppression_requirement, 25, 0.75).
narrative_ontology:measurement(deri_su_t30, derivative_work_statutory_boundary__enclosure_reading, suppression_requirement, 30, 0.77).
narrative_ontology:measurement(deri_su_t35, derivative_work_statutory_boundary__enclosure_reading, suppression_requirement, 35, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(derivative_work_statutory_boundary__enclosure_reading, derivative_work_statutory_boundary__coordination_reading).
narrative_ontology:affects_constraint(derivative_work_statutory_boundary__enclosure_reading, derivative_work_statutory_boundary__hybrid_carveout_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the derivative_work_statutory_boundary kernel. The kernel decomposes into three structurally distinct constraints because the epsilon values, beneficiary structures, and victim sets differ across readings. The enclosure reading (this file) asserts maximum extraction; the coordination reading asserts a narrower boundary with transformative immunity; the hybrid carveout reading introduces a commercial exploitation axis.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
