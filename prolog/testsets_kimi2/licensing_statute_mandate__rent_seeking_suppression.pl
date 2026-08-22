% ============================================================================
% CONSTRAINT STORY: licensing_statute_mandate__rent_seeking_suppression
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_licensing_statute_mandate__rent_seeking_suppression, []).

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
 *   constraint_id: licensing_statute_mandate__rent_seeking_suppression
 *   human_readable: Occupational Licensing Rent-Seeking Snare
 *   domain: labor_economics/regulatory_policy
 *
 * SUMMARY:
 *   This constraint story instantiates the rent_seeking_suppression reading
 *   of the licensing_statute_mandate kernel. It treats statutory credential
 *   requirements not as public safety coordination but as a snare: an
 *   actively enforced mechanism that restricts labor supply, inflates prices
 *   for consumers, and transfers rents to incumbent practitioners. The public
 *   safety narrative is read as theatrical cover for extraction. Sibling
 *   readings treat the same statutory text as safety coordination
 *   (public_safety_coordination) or class-sorting filter
 *   (graduated_access_filter).
 *
 * KEY AGENTS:
 *   - incumbent_practitioners: Primary beneficiary (organized/mobile) â captures rent through supply restriction and board control
 *   - labor_market_entrants: Primary target (powerless/constrained) â bears entry costs and exclusion
 *   - consumers: Secondary target (powerless/constrained) â pays scarcity premiums via higher prices
 *   - licensing_administrators: Agenda setter (institutional/constrained) â enforces statutory barriers
 *   - consumer_advocates: Observer (moderate/analytical) â critiques scope but is outvoted
 *   - unlicensed_practitioners: Excluded (powerless/trapped) â criminalized by the same structure
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(licensing_statute_mandate__rent_seeking_suppression, 0.79).
domain_priors:suppression_score(licensing_statute_mandate__rent_seeking_suppression, 0.85).
domain_priors:theater_ratio(licensing_statute_mandate__rent_seeking_suppression, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(licensing_statute_mandate__rent_seeking_suppression, extractiveness, 0.79).
narrative_ontology:constraint_metric(licensing_statute_mandate__rent_seeking_suppression, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(licensing_statute_mandate__rent_seeking_suppression, theater_ratio, 0.55).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(licensing_statute_mandate__rent_seeking_suppression, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(licensing_statute_mandate__rent_seeking_suppression, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(licensing_statute_mandate__rent_seeking_suppression, snare).
narrative_ontology:human_readable(licensing_statute_mandate__rent_seeking_suppression, "Occupational Licensing Rent-Seeking Snare").
narrative_ontology:topic_domain(licensing_statute_mandate__rent_seeking_suppression, "labor_economics/regulatory_policy").

domain_priors:requires_active_enforcement(licensing_statute_mandate__rent_seeking_suppression).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(licensing_statute_mandate__rent_seeking_suppression, 'fe213834-d64a-4755-b3f5-3f2a60818c0e').
narrative_ontology:cs_kernel_codification('fe213834-d64a-4755-b3f5-3f2a60818c0e', formalized).
narrative_ontology:cs_authority_grounding('fe213834-d64a-4755-b3f5-3f2a60818c0e', extraction).
narrative_ontology:cs_interpretation_layer_present('fe213834-d64a-4755-b3f5-3f2a60818c0e').
narrative_ontology:cs_reading_relation('fe213834-d64a-4755-b3f5-3f2a60818c0e', licensing_statute_mandate__public_safety_coordination, coexists_with).
narrative_ontology:cs_reading_relation('fe213834-d64a-4755-b3f5-3f2a60818c0e', licensing_statute_mandate__graduated_access_filter, coexists_with).
narrative_ontology:cs_axiom('fe213834-d64a-4755-b3f5-3f2a60818c0e', foundational, statutory_licensing_primary_function_is_rent_transfer).
narrative_ontology:cs_axiom_status(statutory_licensing_primary_function_is_rent_transfer, holdable).
narrative_ontology:cs_axiom_grounding('fe213834-d64a-4755-b3f5-3f2a60818c0e', statutory_licensing_primary_function_is_rent_transfer, empirically_contingent).
narrative_ontology:cs_axiom('fe213834-d64a-4755-b3f5-3f2a60818c0e', secondary, incumbent_control_of_licensing_boards_is_captured_governance).
narrative_ontology:cs_axiom_status(incumbent_control_of_licensing_boards_is_captured_governance, holdable).
narrative_ontology:cs_axiom_grounding('fe213834-d64a-4755-b3f5-3f2a60818c0e', incumbent_control_of_licensing_boards_is_captured_governance, empirically_contingent).
narrative_ontology:cs_reference_frame('fe213834-d64a-4755-b3f5-3f2a60818c0e', producer_rent_extraction_arrangement).
narrative_ontology:cs_drift_state('fe213834-d64a-4755-b3f5-3f2a60818c0e', contemporary_licensing_expansion_era, gap(stable, minor, false)).
narrative_ontology:cs_created_at('fe213834-d64a-4755-b3f5-3f2a60818c0e', '').
narrative_ontology:cs_kernel_id(licensing_statute_mandate__rent_seeking_suppression, licensing_statute_mandate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(licensing_statute_mandate__rent_seeking_suppression, incumbent_practitioners).
narrative_ontology:constraint_victim(licensing_statute_mandate__rent_seeking_suppression, labor_market_entrants).
narrative_ontology:constraint_victim(licensing_statute_mandate__rent_seeking_suppression, consumers).
narrative_ontology:constraint_vindicates(licensing_statute_mandate__rent_seeking_suppression, regulatory_capture_theory).
narrative_ontology:constraint_vindicates(licensing_statute_mandate__rent_seeking_suppression, public_choice_rent_seeking).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Receive persistent wage premiums and face reduced competition because statutory entry barriers limit labor supply. They often dominate licensing boards and support tighter standards. Their economic position is protected by the state-backed restriction on who may practice.
narrative_ontology:constraint_stakeholder(licensing_statute_mandate__rent_seeking_suppression, incumbent_practitioners, beneficiary,
    organized, biographical, mobile, national).

% Must complete costly mandated education, training, and examinations to obtain a license. They bear lost wages, tuition debt, and time costs. Many are deterred entirely from entering the profession, particularly those from lower-income backgrounds.
narrative_ontology:constraint_stakeholder(licensing_statute_mandate__rent_seeking_suppression, labor_market_entrants, payer,
    powerless, immediate, constrained, national).

% Pay higher prices for licensed services because the restricted supply reduces competitive pressure. They have fewer provider choices and limited legal access to lower-cost unlicensed alternatives.
narrative_ontology:constraint_stakeholder(licensing_statute_mandate__rent_seeking_suppression, consumers, payer,
    powerless, immediate, constrained, national).

% State agency staff and board members who process applications, administer examinations, set continuing education rules, and investigate or penalize unlicensed practice. They maintain the statutory framework and control the pace and scope of entry.
narrative_ontology:constraint_stakeholder(licensing_statute_mandate__rent_seeking_suppression, licensing_administrators, agenda_setter,
    institutional, generational, constrained, national).

% Researchers and advocacy organizations who argue that licensing scope exceeds safety justifications and that barriers harm mobility and affordability. They participate in rulemaking comments but are routinely outvoted by incumbent representatives.
narrative_ontology:constraint_stakeholder(licensing_statute_mandate__rent_seeking_suppression, consumer_advocates, observer,
    moderate, biographical, analytical, national).

% Individuals with relevant skills who are barred from legal practice by credential requirements. They work informally if at all, face fines or criminal penalties, and cannot build a formal reputation or access commercial infrastructure.
narrative_ontology:constraint_stakeholder(licensing_statute_mandate__rent_seeking_suppression, unlicensed_practitioners, excluded,
    powerless, immediate, trapped, local).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(licensing_statute_mandate__rent_seeking_suppression, incumbent_practitioners).
narrative_ontology:fixing_cost_class(licensing_statute_mandate__rent_seeking_suppression, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The constraint claims to solve asymmetric information about practitioner quality, but under this reading no genuine coordination problem is solved at the statutory barrier that voluntary certification, tort liability, and market reputation mechanisms would not solve more efficiently.
% TRANSFER_FUNCTION: Moves economic surplus from consumers and prospective workers to incumbent practitioners through artificial scarcity premiums, restricted labor supply, and barrier maintenance.
% ABSENT_VOICES: Unlicensed practitioners and low-income prospective entrants who cannot afford credentialing are structurally excluded from rulemaking hearings; their absence is treated as absence of demand rather than exclusion from the forum.
% DISAPPEARANCE_RATIONALE: If statutory credential requirements vanished overnight, labor supply would expand in licensed fields, incumbent wage premiums would compress toward competitive levels, consumer prices would fall, and enforcement agencies would lose their function. The market would reorganize around voluntary credentialing and direct consumer choice.
% FOUNDING_PROBLEM: Asymmetric information in professional services markets may lead consumers to purchase harmful or low-quality services.
% FOUNDING_PROBLEM_CORROBORATION: Labor economists and public-choice scholars outside the incumbent-beneficiary coalition attest that the safety rationale is empirically unsupported for many licensed occupations and that the original problem, where it existed, is now better addressed by liability law, consumer review platforms, and voluntary certification.
narrative_ontology:disappearance_verdict(licensing_statute_mandate__rent_seeking_suppression, world_rearranges).
narrative_ontology:founding_problem_status(licensing_statute_mandate__rent_seeking_suppression, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(licensing_statute_mandate__rent_seeking_suppression, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(licensing_statute_mandate__rent_seeking_suppression, 'none', 1).
narrative_ontology:epsilon_provenance(licensing_statute_mandate__rent_seeking_suppression, 0.79, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(licensing_statute_mandate__rent_seeking_suppression_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(licensing_statute_mandate__rent_seeking_suppression, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(licensing_statute_mandate__rent_seeking_suppression_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.79) because empirical literature shows licensing wage premiums persist after controlling for human capital, indicating rent transfer. Suppression is very high (0.85) because unlicensed practice is criminalized or heavily fined, and alternative pathways are structurally blocked. Theater ratio is moderate-high (0.55): the safety justification is loudly performed but decoupled from actual task risk in many licensed occupations. Accessibility collapse is high (0.70) because once the statutory framework is in place, consumers and entrants lose awareness of non-licensed alternatives. Resistance is moderate (0.45): economists and some reform coalitions actively oppose licensing scope, but incumbent political power dampens legislative success.
 *
 * PERSPECTIVAL GAP:
 *   The incumbent practitioner seat experiences the constraint as legitimate professional standards that protect investment in training. The entrant seat experiences it as an arbitrary wall. The consumer seat experiences it as higher prices without observable quality increments. The agenda-setter seat experiences it as neutral administration. These divergences are structurally inherent; the engine computes them from the role and power data.
 *
 * DIRECTIONALITY LOGIC:
 *   Incumbent_practitioners are declared beneficiaries with mobile exit options within the profession, placing d near the beneficiary pole. Labor_market_entrants and consumers are declared victims with constrained or trapped exit, placing d near the target pole. Licensing_administrators are agenda_setters with institutional power; without beneficiary declaration, their d reverts toward canonical fallback near symmetric. Consumer_advocates are observers with analytical exit.
 *
 * MANDATROPHY ANALYSIS:
 *   The sibling reading public_safety_coordination posits a founding problem (asymmetric information) that might once have been live. This snare reading asserts the mandate either never genuinely addressed that problem or has long since atrophied into pure extraction. The mandatrophy guard is satisfied by authoring founding_problem_status as dead and documenting corroboration from outside the beneficiary coalition. This prevents the engine from misclassifying a dead coordination function as ongoing rope; instead, the high extraction and suppression scores, combined with dead mandate status, route the verdict toward snare. The theater ratio (0.55) signals that substantial performative activity remains, distinguishing it from a pure piton where function is fully atrophied.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    safety_externality_vs_rent_transfer,
    'Do licensing statutes generate measurable safety or quality improvements that exceed what voluntary certification and tort liability would produce, or is the observed wage premium purely rent transfer?',
    'Cross-occupational regressions linking licensing stringency to consumer outcomes, and natural experiments from interstate reciprocity and sunrise review states.',
    'If safety effects are large and non-replicable by non-statutory means, epsilon falls toward tangled_rope; if absent, the snare classification is reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(safety_externality_vs_rent_transfer, empirical, 'Empirical ambiguity between public safety justification and rent extraction').

omega_variable(
    board_capture_vs_neutral_administration,
    'Are licensing boards structurally captured by incumbent practitioners, or do they maintain neutral consumer protection?',
    'Analysis of board composition, revolving doors, hearing testimony, and disciplinary patterns favoring incumbents over entrants.',
    'If capture is total, the constraint''s enforcement is an extension of incumbent power rather than neutral regulation; if partial, extraction is diluted.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(board_capture_vs_neutral_administration, empirical, 'Uncertainty about administrative neutrality versus incumbent capture').

omega_variable(
    kernel_reading_coexistence_or_foreclosure,
    'Can the rent-seeking reading and the public-safety reading be simultaneously true of the same statute, or does empirical resolution of the safety question foreclose one?',
    'Historical case studies of licensing adoption and repeal movements; analysis of whether safety and rent-transfer are structurally separable functions.',
    'If foreclosed, the kernel is not polysemous but factually determinate; if coexistent, the kernel remains genuinely contested and multiple constraints must be maintained.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_coexistence_or_foreclosure, conceptual, 'Whether the kernel readings are empirically resolvable or irreducibly polysemous').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(licensing_statute_mandate__rent_seeking_suppression, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(lice_tr_t0, licensing_statute_mandate__rent_seeking_suppression, theater_ratio, 0, 0.2).
narrative_ontology:measurement(lice_tr_t8, licensing_statute_mandate__rent_seeking_suppression, theater_ratio, 8, 0.28).
narrative_ontology:measurement(lice_tr_t16, licensing_statute_mandate__rent_seeking_suppression, theater_ratio, 16, 0.37).
narrative_ontology:measurement(lice_tr_t24, licensing_statute_mandate__rent_seeking_suppression, theater_ratio, 24, 0.45).
narrative_ontology:measurement(lice_tr_t32, licensing_statute_mandate__rent_seeking_suppression, theater_ratio, 32, 0.51).
narrative_ontology:measurement(lice_tr_t40, licensing_statute_mandate__rent_seeking_suppression, theater_ratio, 40, 0.55).

% Extraction over time
narrative_ontology:measurement(lice_be_t0, licensing_statute_mandate__rent_seeking_suppression, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(lice_be_t8, licensing_statute_mandate__rent_seeking_suppression, base_extractiveness, 8, 0.46).
narrative_ontology:measurement(lice_be_t16, licensing_statute_mandate__rent_seeking_suppression, base_extractiveness, 16, 0.58).
narrative_ontology:measurement(lice_be_t24, licensing_statute_mandate__rent_seeking_suppression, base_extractiveness, 24, 0.68).
narrative_ontology:measurement(lice_be_t32, licensing_statute_mandate__rent_seeking_suppression, base_extractiveness, 32, 0.74).
narrative_ontology:measurement(lice_be_t40, licensing_statute_mandate__rent_seeking_suppression, base_extractiveness, 40, 0.79).

% Suppression requirement over time
narrative_ontology:measurement(lice_su_t0, licensing_statute_mandate__rent_seeking_suppression, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(lice_su_t8, licensing_statute_mandate__rent_seeking_suppression, suppression_requirement, 8, 0.58).
narrative_ontology:measurement(lice_su_t16, licensing_statute_mandate__rent_seeking_suppression, suppression_requirement, 16, 0.66).
narrative_ontology:measurement(lice_su_t24, licensing_statute_mandate__rent_seeking_suppression, suppression_requirement, 24, 0.74).
narrative_ontology:measurement(lice_su_t32, licensing_statute_mandate__rent_seeking_suppression, suppression_requirement, 32, 0.8).
narrative_ontology:measurement(lice_su_t40, licensing_statute_mandate__rent_seeking_suppression, suppression_requirement, 40, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(licensing_statute_mandate__rent_seeking_suppression, public_safety_coordination).
narrative_ontology:affects_constraint(licensing_statute_mandate__rent_seeking_suppression, graduated_access_filter).

% DUAL FORMULATION NOTE:
% This story is one of three readings of the licensing_statute_mandate kernel. The rent_seeking_suppression reading decomposes the colloquial label 'occupational licensing' into a pure extraction mechanism, while siblings treat it as safety coordination or class-sorting filter. Each reading carries its own epsilon and stakeholder geometry.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
