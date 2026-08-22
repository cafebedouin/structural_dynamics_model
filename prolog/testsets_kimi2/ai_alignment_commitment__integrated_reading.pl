% ============================================================================
% CONSTRAINT STORY: ai_alignment_commitment__integrated_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ai_alignment_commitment__integrated_reading, []).

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
 *   constraint_id: ai_alignment_commitment__integrated_reading
 *   human_readable: Siloed AI Alignment Institutional Structure (Integrated Reading)
 *   domain: AI Governance / Technology Ethics / Risk Assessment
 *
 * SUMMARY:
 *   This constraint story instantiates the integrated_reading of the
 *   ai_alignment_commitment kernel. The standing arrangement under contest is
 *   the current institutional structure of AI alignment work, which separates
 *   technical safety from AI ethics into distinct funding streams, conference
 *   tracks, and disciplinary communities. The integrated reading assesses
 *   this siloed structure as a constraint that fragments unified effort: it
 *   has genuine coordination value within each silo but extracts
 *   asymmetrically from present marginalized populations (who suffer biased
 *   systems safety ignores) and future humanity (who face uncontrolled risks
 *   ethics cannot mitigate). The claim of tangled_rope is authored
 *   independently from the metrics; the engine measures the divergence.
 *
 * KEY AGENTS:
 *   - Technical safety institutions (powerful/mobile) â primary beneficiaries of the control silo
 *   - AI ethics programs (organized/constrained) â primary beneficiaries of the justice silo
 *   - Present marginalized populations (powerless/trapped) â bear present-day costs of fragmented oversight
 *   - Future humanity (powerless/trapped) â bear existential and catastrophic risk costs
 *   - Funding and peer review gatekeepers (institutional/arbitrage) â agenda setters who enforce disciplinary boundaries
 *   - Integrated researchers (moderate/constrained) â excluded voices who bridge both domains
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_alignment_commitment__integrated_reading, 0.72).
domain_priors:suppression_score(ai_alignment_commitment__integrated_reading, 0.65).
domain_priors:theater_ratio(ai_alignment_commitment__integrated_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_alignment_commitment__integrated_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(ai_alignment_commitment__integrated_reading, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(ai_alignment_commitment__integrated_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ai_alignment_commitment__integrated_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(ai_alignment_commitment__integrated_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_alignment_commitment__integrated_reading, tangled_rope).
narrative_ontology:human_readable(ai_alignment_commitment__integrated_reading, "Siloed AI Alignment Institutional Structure (Integrated Reading)").
narrative_ontology:topic_domain(ai_alignment_commitment__integrated_reading, "AI Governance / Technology Ethics / Risk Assessment").

domain_priors:requires_active_enforcement(ai_alignment_commitment__integrated_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ai_alignment_commitment__integrated_reading, '29005d24-4b46-431d-ae5b-a90ed5a98d99').
narrative_ontology:cs_kernel_codification('29005d24-4b46-431d-ae5b-a90ed5a98d99', distributed).
narrative_ontology:cs_authority_grounding('29005d24-4b46-431d-ae5b-a90ed5a98d99', distributed).
narrative_ontology:cs_reading_relation('29005d24-4b46-431d-ae5b-a90ed5a98d99', ai_alignment_commitment__safety_control_reading, influences).
narrative_ontology:cs_reading_relation('29005d24-4b46-431d-ae5b-a90ed5a98d99', ai_alignment_commitment__ethics_justice_reading, influences).
narrative_ontology:cs_axiom('29005d24-4b46-431d-ae5b-a90ed5a98d99', foundational, control_and_justice_non_exclusive).
narrative_ontology:cs_axiom_status(control_and_justice_non_exclusive, holdable).
narrative_ontology:cs_axiom_grounding('29005d24-4b46-431d-ae5b-a90ed5a98d99', control_and_justice_non_exclusive, deontological).
narrative_ontology:cs_axiom('29005d24-4b46-431d-ae5b-a90ed5a98d99', foundational, siloed_fragmentation_extractive).
narrative_ontology:cs_axiom_status(siloed_fragmentation_extractive, holdable).
narrative_ontology:cs_axiom_grounding('29005d24-4b46-431d-ae5b-a90ed5a98d99', siloed_fragmentation_extractive, empirically_contingent).
narrative_ontology:cs_reference_frame('29005d24-4b46-431d-ae5b-a90ed5a98d99', unified_alignment_mandate).
narrative_ontology:cs_drift_state('29005d24-4b46-431d-ae5b-a90ed5a98d99', contemporary, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('29005d24-4b46-431d-ae5b-a90ed5a98d99', '').
narrative_ontology:cs_kernel_id(ai_alignment_commitment__integrated_reading, ai_alignment_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_alignment_commitment__integrated_reading, technical_safety_institutions).
narrative_ontology:constraint_beneficiary(ai_alignment_commitment__integrated_reading, ai_ethics_programs).
narrative_ontology:constraint_victim(ai_alignment_commitment__integrated_reading, present_marginalized_populations).
narrative_ontology:constraint_victim(ai_alignment_commitment__integrated_reading, future_humanity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Receive substantial funding and institutional legitimacy for technical AI safety research focused on control and existential risk. They benefit from disciplinary boundaries that do not require engagement with social justice frameworks or affected communities, allowing concentrated expertise development without integration overhead.
narrative_ontology:constraint_stakeholder(ai_alignment_commitment__integrated_reading, technical_safety_institutions, beneficiary,
    powerful, generational, mobile, global).

% Receive academic funding and legitimacy for algorithmic fairness and bias research. They define alignment in terms of present-day social justice, often without engagement with frontier technical control problems or long-term catastrophic risk, benefiting from a separate disciplinary track.
narrative_ontology:constraint_stakeholder(ai_alignment_commitment__integrated_reading, ai_ethics_programs, beneficiary,
    organized, generational, constrained, national).

% Bear the costs of AI systems deployed without integrated justice oversight: biased decisions in hiring, policing, lending, and content moderation. Their lived experience is frequently studied as an ethics object but rarely shapes safety research agendas or technical design priorities.
narrative_ontology:constraint_stakeholder(ai_alignment_commitment__integrated_reading, present_marginalized_populations, payer,
    powerless, biographical, trapped, global).

% Bears the risk of catastrophic or existential outcomes from advanced AI systems developed without integrated oversight. Has no representative voice in current funding structures, peer review, or governance institutions.
narrative_ontology:constraint_stakeholder(ai_alignment_commitment__integrated_reading, future_humanity, payer,
    powerless, civilizational, trapped, universal).

% Set disciplinary boundaries through grant calls, conference tracks, and publication venues that separate technical safety from AI ethics. Their decisions enforce the siloed structure by rewarding specialized depth over interdisciplinary integration, and they could alter these boundaries but face institutional inertia.
narrative_ontology:constraint_stakeholder(ai_alignment_commitment__integrated_reading, funding_and_peer_review_gatekeepers, agenda_setter,
    institutional, generational, arbitrage, global).

% Pursue research bridging technical control and justice concerns. They struggle to find funding, venues, or career paths that recognize integrated work, and are often forced to affiliate with either safety or ethics communities to survive professionally.
narrative_ontology:constraint_stakeholder(ai_alignment_commitment__integrated_reading, integrated_researchers, excluded,
    moderate, biographical, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(ai_alignment_commitment__integrated_reading, diffuse).
narrative_ontology:fixing_cost_class(ai_alignment_commitment__integrated_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates specialized research communities around distinct sub-problems of AI alignment â technical control on one side and social justice on the other â enabling focused expertise development, resource allocation, and peer validation within each domain.
% TRANSFER_FUNCTION: Moves resources, legitimacy, and epistemic authority to specialized safety and ethics programs while moving the costs of uncontrolled AI deployment and biased systems to marginalized populations and future generations.
% ABSENT_VOICES: Integrated researchers pursuing unified frameworks are structurally excluded from mainstream funding and top-tier publication venues; affected marginalized communities are consulted tokenistically but denied agenda-setting power; future generations have no representative voice.
% DISAPPEARANCE_RATIONALE: If the siloed structure vanished, research funding and publication venues would reorganize around integrated problems, disciplinary boundaries would soften, and the allocation of epistemic authority would shift away from pure technical or pure ethics specialization toward unified frameworks.
% FOUNDING_PROBLEM: How to ensure advanced AI systems behave in ways that benefit humanity and avoid catastrophic harm.
% FOUNDING_PROBLEM_CORROBORATION: Independent interdisciplinary scholars and affected community organizers attest that the founding problem requires integration of control and justice; mainstream safety and ethics institutions dispute this, asserting their siloed approaches are individually sufficient.
narrative_ontology:disappearance_verdict(ai_alignment_commitment__integrated_reading, world_rearranges).
narrative_ontology:founding_problem_status(ai_alignment_commitment__integrated_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ai_alignment_commitment__integrated_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(ai_alignment_commitment__integrated_reading, 'none', 1).
narrative_ontology:epsilon_provenance(ai_alignment_commitment__integrated_reading, 0.72, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ai_alignment_commitment__integrated_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(ai_alignment_commitment__integrated_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(ai_alignment_commitment__integrated_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.72) because the siloed arrangement externalizes severe harms to populations with no voice. Suppression (0.65) reflects active enforcement through funding and peer review boundaries that penalize integration. Theater ratio (0.45) captures performative alignment work in both silos that avoids the hard problem of integration. Accessibility collapse (0.40) is moderate because integrated alternatives are intellectually visible but institutionally blocked. Resistance (0.55) reflects growing pressure from integrated researchers and affected communities. Measurements share a single time grid to prevent misaligned drift dating.
 *
 * PERSPECTIVAL GAP:
 *   The beneficiary seats (safety and ethics institutions) experience the constraint as enabling legitimate specialization; the payer seats (marginalized populations, future humanity) experience it as a structure that exposes them to risk while denying them voice. The agenda setter seat sees a manageable disciplinary partition; the excluded seat sees a forced choice between two incomplete communities. The engine computes this divergence from the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries derive low directionality: they are subsidized by the constraint's enforcement of disciplinary boundaries that protect their funding and legitimacy. Victims derive high directionality: they are structurally targeted by the fragmentation, paying costs without receiving coordinating benefits. Gatekeepers sit near the beneficiary end because they control the boundaries and could arbitrage out. Integrated researchers are constrained but not fully trapped, placing them in the middle.
 *
 * MANDATROPHY ANALYSIS:
 *   Classifying this as tangled_rope rather than snare preserves the genuine coordination function each silo provides: technical safety does real control research, and ethics programs do real bias analysis. A snare classification would erase that value. A rope classification would miss the asymmetric extraction from those the silos fail to protect. The founding problem â ensuring AI benefits humanity â remains live, but the arrangement has drifted toward protective specialization that avoids integration costs.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    silo_coordination_or_extraction,
    'Does the siloed structure of AI alignment genuinely optimize specialized expertise, or does it primarily extract legitimacy by avoiding the harder work of integration?',
    'Comparative outcome analysis: measure whether integrated research programs produce measurably worse, equivalent, or better safety and justice outcomes relative to siloed programs at comparable scale.',
    'If siloing is found to underperform integration, the constraint shifts toward snare; if siloing is genuinely necessary for current capabilities, the coordination half of tangled_rope is strengthened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(silo_coordination_or_extraction, empirical, 'Whether siloing is functional specialization or evasion').

omega_variable(
    future_humanity_voice_feasibility,
    'Can future humanity''s interests be structurally represented in present alignment institutions without collapsing into present-power projection?',
    'Institutional design analysis of proxy representation mechanisms (long-term governance boards, futurist advocates, generational impact assessments) and their capture resistance.',
    'If impossible, future humanity remains a high-d victim with no exit; if possible, directionality for that seat could shift toward moderate.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(future_humanity_voice_feasibility, conceptual, 'Feasibility of future generation representation').

omega_variable(
    marginalized_population_tokenization,
    'Are present marginalized populations genuinely excluded, or are they included in ethics programs in ways that satisfy the coordination function?',
    'Participatory audits of AI ethics research: measure marginalized community control over research agendas, authorship, and resource allocation versus mere consultation or data extraction.',
    'If tokenized, effective suppression is higher than structural measures suggest and the victim classification is reinforced; if genuinely empowered, the extraction asymmetry is reduced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(marginalized_population_tokenization, empirical, 'Whether ethics inclusion is substantive or tokenistic').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_alignment_commitment__integrated_reading, 0, 14).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ai_align_int_tr_t0, ai_alignment_commitment__integrated_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(ai_align_int_tr_t3, ai_alignment_commitment__integrated_reading, theater_ratio, 3, 0.25).
narrative_ontology:measurement(ai_align_int_tr_t6, ai_alignment_commitment__integrated_reading, theater_ratio, 6, 0.3).
narrative_ontology:measurement(ai_align_int_tr_t9, ai_alignment_commitment__integrated_reading, theater_ratio, 9, 0.38).
narrative_ontology:measurement(ai_align_int_tr_t12, ai_alignment_commitment__integrated_reading, theater_ratio, 12, 0.42).
narrative_ontology:measurement(ai_align_int_tr_t14, ai_alignment_commitment__integrated_reading, theater_ratio, 14, 0.45).

% Extraction over time
narrative_ontology:measurement(ai_align_int_be_t0, ai_alignment_commitment__integrated_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(ai_align_int_be_t3, ai_alignment_commitment__integrated_reading, base_extractiveness, 3, 0.52).
narrative_ontology:measurement(ai_align_int_be_t6, ai_alignment_commitment__integrated_reading, base_extractiveness, 6, 0.58).
narrative_ontology:measurement(ai_align_int_be_t9, ai_alignment_commitment__integrated_reading, base_extractiveness, 9, 0.65).
narrative_ontology:measurement(ai_align_int_be_t12, ai_alignment_commitment__integrated_reading, base_extractiveness, 12, 0.7).
narrative_ontology:measurement(ai_align_int_be_t14, ai_alignment_commitment__integrated_reading, base_extractiveness, 14, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(ai_align_int_su_t0, ai_alignment_commitment__integrated_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(ai_align_int_su_t3, ai_alignment_commitment__integrated_reading, suppression_requirement, 3, 0.45).
narrative_ontology:measurement(ai_align_int_su_t6, ai_alignment_commitment__integrated_reading, suppression_requirement, 6, 0.5).
narrative_ontology:measurement(ai_align_int_su_t9, ai_alignment_commitment__integrated_reading, suppression_requirement, 9, 0.55).
narrative_ontology:measurement(ai_align_int_su_t12, ai_alignment_commitment__integrated_reading, suppression_requirement, 12, 0.6).
narrative_ontology:measurement(ai_align_int_su_t14, ai_alignment_commitment__integrated_reading, suppression_requirement, 14, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_alignment_commitment__integrated_reading, identity_coordination).
narrative_ontology:affects_constraint(ai_alignment_commitment__integrated_reading, safety_control_reading).
narrative_ontology:affects_constraint(ai_alignment_commitment__integrated_reading, ethics_justice_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the ai_alignment_commitment kernel, instantiated by the integrated_reading. It describes the same institutional domain as its siblings but assesses the structure as requiring simultaneous attention to control and justice problems as non-exclusive.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
