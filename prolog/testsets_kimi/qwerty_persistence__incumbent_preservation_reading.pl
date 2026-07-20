% ============================================================================
% CONSTRAINT STORY: qwerty_persistence__incumbent_preservation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_qwerty_persistence__incumbent_preservation_reading, []).

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
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: qwerty_persistence__incumbent_preservation_reading
 *   human_readable: QWERTY Persistence via Incumbent Investment Defense
 *   domain: technology_history/industrial_standards/path_dependence
 *
 * SUMMARY:
 *   This constraint instantiates the incumbent_preservation_reading of the
 *   qwerty_persistence kernel. It treats the QWERTY keyboard layout not as a
 *   passive coordination equilibrium but as an actively defended incumbent
 *   standard whose persistence depends on beneficiary defense of sunk capital
 *   investments. Manufacturers, trained typists, and training institutions
 *   benefit from interoperability, but the constraint also extracts from
 *   alternative-adopters and efficiency-seekers by suppressing competing
 *   layouts. The sibling reading, lapsed_alternatives_reading, attributes
 *   persistence to passive network effects and critical-mass failure of
 *   alternatives.
 *
 * KEY AGENTS:
 *   - keyboard_manufacturers: Primary agenda-setter/beneficiary (institutional/arbitrage) â actively enforces standard to protect retooling investments
 *   - trained_typists: Primary beneficiary (organized/identity_locked) â human capital locked to layout, resists devaluation
 *   - typing_training_institutions: Secondary beneficiary (organized/constrained) â curriculum and certification revenue tied to standard
 *   - alternative_layout_adopters: Primary target (moderate/constrained) â bears compatibility penalties and social friction
 *   - efficiency_seeking_users: Secondary target (moderate/constrained) â forced into suboptimal layout by narrowed market
 *   - alternative_hardware_manufacturers: Excluded producer (moderate/trapped) â blocked from market access by incumbent-coordinated standards
 *   - technology historians: Analytical observer (institutional/analytical) â attests founding problem obsolescence
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(qwerty_persistence__incumbent_preservation_reading, 0.62).
domain_priors:suppression_score(qwerty_persistence__incumbent_preservation_reading, 0.58).
domain_priors:theater_ratio(qwerty_persistence__incumbent_preservation_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(qwerty_persistence__incumbent_preservation_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(qwerty_persistence__incumbent_preservation_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(qwerty_persistence__incumbent_preservation_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(qwerty_persistence__incumbent_preservation_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(qwerty_persistence__incumbent_preservation_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(qwerty_persistence__incumbent_preservation_reading, tangled_rope).
narrative_ontology:human_readable(qwerty_persistence__incumbent_preservation_reading, "QWERTY Persistence via Incumbent Investment Defense").
narrative_ontology:topic_domain(qwerty_persistence__incumbent_preservation_reading, "technology_history/industrial_standards/path_dependence").

domain_priors:requires_active_enforcement(qwerty_persistence__incumbent_preservation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(qwerty_persistence__incumbent_preservation_reading, '90bc3530-d3af-411b-bf7a-4d4faa741b71').
narrative_ontology:cs_kernel_codification('90bc3530-d3af-411b-bf7a-4d4faa741b71', formalized).
narrative_ontology:cs_authority_grounding('90bc3530-d3af-411b-bf7a-4d4faa741b71', extraction).
narrative_ontology:cs_reading_relation('90bc3530-d3af-411b-bf7a-4d4faa741b71', qwerty_persistence__lapsed_alternatives_reading, coexists_with).
narrative_ontology:cs_axiom('90bc3530-d3af-411b-bf7a-4d4faa741b71', foundational, incumbent_investment_protection_entitlement).
narrative_ontology:cs_axiom_status(incumbent_investment_protection_entitlement, holdable).
narrative_ontology:cs_axiom_grounding('90bc3530-d3af-411b-bf7a-4d4faa741b71', incumbent_investment_protection_entitlement, conventional).
narrative_ontology:cs_axiom('90bc3530-d3af-411b-bf7a-4d4faa741b71', foundational, active_suppression_explains_alternative_failure).
narrative_ontology:cs_axiom_status(active_suppression_explains_alternative_failure, holdable).
narrative_ontology:cs_axiom_grounding('90bc3530-d3af-411b-bf7a-4d4faa741b71', active_suppression_explains_alternative_failure, empirically_contingent).
narrative_ontology:cs_reference_frame('90bc3530-d3af-411b-bf7a-4d4faa741b71', installed_base_protection_framework).
narrative_ontology:cs_drift_state('90bc3530-d3af-411b-bf7a-4d4faa741b71', post_digital_keyboard_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('90bc3530-d3af-411b-bf7a-4d4faa741b71', '').
narrative_ontology:cs_kernel_id(qwerty_persistence__incumbent_preservation_reading, qwerty_persistence).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(qwerty_persistence__incumbent_preservation_reading, keyboard_manufacturers).
narrative_ontology:constraint_beneficiary(qwerty_persistence__incumbent_preservation_reading, trained_typists).
narrative_ontology:constraint_beneficiary(qwerty_persistence__incumbent_preservation_reading, typing_training_institutions).
narrative_ontology:constraint_victim(qwerty_persistence__incumbent_preservation_reading, alternative_layout_adopters).
narrative_ontology:constraint_victim(qwerty_persistence__incumbent_preservation_reading, efficiency_seeking_users).
narrative_ontology:constraint_victim(qwerty_persistence__incumbent_preservation_reading, alternative_hardware_manufacturers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Control global production tooling and supply chains optimized for QWERTY; sit on standards committees and fund compatibility studies defending the layout; bear massive retooling costs if the standard shifts, so they actively suppress alternative-layout certification and OEM partnerships.
narrative_ontology:constraint_stakeholder(qwerty_persistence__incumbent_preservation_reading, keyboard_manufacturers, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(qwerty_persistence__incumbent_preservation_reading, keyboard_manufacturers, beneficiary).

% Possess embodied human capital in QWERTY muscle memory; benefit from a standardized labor market where their skills are portable across employers; resist retraining that would devalue their speed credentials and employment prospects.
narrative_ontology:constraint_stakeholder(qwerty_persistence__incumbent_preservation_reading, trained_typists, beneficiary,
    organized, biographical, identity_locked, national).

% Sell QWERTY-based curricula, certification exams, and instructor time; a layout shift would obsolete their course materials and require instructor retraining, threatening their business model.
narrative_ontology:constraint_stakeholder(qwerty_persistence__incumbent_preservation_reading, typing_training_institutions, beneficiary,
    organized, biographical, constrained, national).

% Use Dvorak or other layouts and bear compatibility penalties: unfamiliar hardware in public spaces, software default resets, inability to use shared workstations without remapping, and social friction in workplace environments.
narrative_ontology:constraint_stakeholder(qwerty_persistence__incumbent_preservation_reading, alternative_layout_adopters, payer,
    moderate, immediate, constrained, national).

% Would prefer ergonomically optimized layouts but cannot purchase mainstream hardware with alternative legends at scale; forced to accept QWERTY's known inefficiency because the retail market is structurally narrowed by incumbent production choices.
narrative_ontology:constraint_stakeholder(qwerty_persistence__incumbent_preservation_reading, efficiency_seeking_users, payer,
    moderate, biographical, constrained, national).

% Would produce and sell alternative-layout keyboards if distribution channels and standards certification were not controlled by QWERTY incumbents; excluded from retail shelf space, corporate procurement lists, and educational tenders by the incumbent-coordinated standards environment.
narrative_ontology:constraint_stakeholder(qwerty_persistence__incumbent_preservation_reading, alternative_hardware_manufacturers, excluded,
    moderate, biographical, trapped, regional).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(qwerty_persistence__incumbent_preservation_reading, keyboard_manufacturers).
narrative_ontology:fixing_cost_class(qwerty_persistence__incumbent_preservation_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains a single, universally interoperable keyboard layout so that equipment, skills, and documentation remain compatible across organizations and generations of hardware.
% TRANSFER_FUNCTION: Moves capital protection value from the installed base of hardware, training, and human capital to incumbent manufacturers and trained workers, funded by the suppressed adoption of potentially superior alternatives.
% ABSENT_VOICES: Alternative-layout users and ergonomic researchers who would argue for layout diversity are not represented on standards bodies dominated by manufacturers; efficiency economists noting the deadweight loss are sidelined in industry committees.
% DISAPPEARANCE_RATIONALE: The installed base of physical keyboards, training infrastructure, and human capital is organized entirely around this layout; its disappearance would force retooling costs, retraining cycles, and a temporary collapse of interoperable text input across the installed hardware fleet.
% FOUNDING_PROBLEM: Early typewriter mechanical jamming required a layout that separated frequently used letter pairs; QWERTY solved the mechanical interference problem in the mechanical era.
% FOUNDING_PROBLEM_CORROBORATION: Historians of technology and mechanical engineers outside the benefiting parties attest that digital and electronic keyboards face no jamming constraints; the original problem is technologically obsolete, corroborated by academic technology historians and ergonomic researchers.
narrative_ontology:disappearance_verdict(qwerty_persistence__incumbent_preservation_reading, world_rearranges).
narrative_ontology:founding_problem_status(qwerty_persistence__incumbent_preservation_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(qwerty_persistence__incumbent_preservation_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(qwerty_persistence__incumbent_preservation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(qwerty_persistence__incumbent_preservation_reading, 0.62, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(qwerty_persistence__incumbent_preservation_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(qwerty_persistence__incumbent_preservation_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(qwerty_persistence__incumbent_preservation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62) is substantial because the gap between QWERTY's genuine coordination value and its social cost is captured by incumbents as protected rent; the layout is no longer technically necessary but is institutionally profitable. Suppression (0.58) reflects active standards-body defense, OEM lock-in, and market narrowing required to prevent alternative adoption. Theater ratio (0.45) captures the growing share of interoperability arguments that are performative rather than functional, since software remapping has dissolved much of the original hardware-interoperability need. Accessibility collapse (0.60) indicates alternatives are technically easy but commercially suppressed. Resistance (0.40) reflects persistent but marginalized ergonomic and efficiency advocacy.
 *
 * PERSPECTIVAL GAP:
 *   The manufacturer seat experiences the constraint as legitimate property defense and necessary coordination; the alternative adopter seat experiences the same structure as enforced extraction and suppressed choice. The trained typist seat experiences identity-locked benefit, while the efficiency-seeking user experiences constrained exit. The engine computes this divergence from identical structural data via role and exit modulation.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (keyboard_manufacturers, trained_typists, typing_training_institutions) sit near the beneficiary end of the directionality axis (low d) because the constraint subsidizes their sunk investments and human capital. Victims (alternative_layout_adopters, efficiency_seeking_users, alternative_hardware_manufacturers) sit near the target end (high d) because they bear the costs of a suppressed market and narrowed choice set. The agenda-setter role of manufacturers amplifies their beneficiary position relative to passive beneficiaries.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (mechanical jamming) is technologically dead, which risks mislabeling the constraint as a piton (atrophied function) or a rope (pure coordination). However, active enforcement, a defined victim set, and rising extraction metrics establish it as tangled_rope: genuine coordination value (interoperability) is inseparable from asymmetric extraction (investment protection). The theater ratio captures the growing gap between original mechanical justification and current performative defense, preventing misclassification as either pure coordination or pure extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    suppression_vs_network_effects,
    'Does QWERTY persist because incumbents actively suppress alternatives, or because network effects and coordination value would sustain it even without active defense?',
    'Natural experiment if a major platform endorsed an alternative layout natively; if adoption remains low, network effects suffice; if incumbents block such endorsement, suppression is confirmed.',
    'If network effects alone suffice, the incumbent_preservation reading overstates extraction and the constraint edges toward rope; if suppression is required, the reading is validated as tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_vs_network_effects, conceptual, 'Core contest between incumbent defense and passive coordination explanations.').

omega_variable(
    capital_investment_magnitude,
    'What is the actual replacement cost for manufacturers and training institutions relative to the social cost of QWERTY inefficiency?',
    'Industry-wide disclosure of retooling costs versus ergonomic productivity studies.',
    'If retooling costs are small and productivity gains large, the constraint is more extractive; if retooling costs are prohibitive, incumbent defense has genuine coordination value in preventing destructive churn.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(capital_investment_magnitude, empirical, 'Empirical magnitude of sunk costs versus social deadweight loss.').

omega_variable(
    theater_vs_function,
    'How much of the interoperability argument is genuine coordination floor versus performative justification for investment protection?',
    'Measure the delta between hardware QWERTY persistence and software keyboard remapping prevalence.',
    'High theater ratio would support classification toward snare or piton; low theater ratio supports tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(theater_vs_function, conceptual, 'Separability of coordination justification from extraction motive.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(qwerty_persistence__incumbent_preservation_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qwerty_ipr_tr_t0, qwerty_persistence__incumbent_preservation_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(qwerty_ipr_tr_t10, qwerty_persistence__incumbent_preservation_reading, theater_ratio, 10, 0.15).
narrative_ontology:measurement(qwerty_ipr_tr_t20, qwerty_persistence__incumbent_preservation_reading, theater_ratio, 20, 0.3).
narrative_ontology:measurement(qwerty_ipr_tr_t30, qwerty_persistence__incumbent_preservation_reading, theater_ratio, 30, 0.38).
narrative_ontology:measurement(qwerty_ipr_tr_t40, qwerty_persistence__incumbent_preservation_reading, theater_ratio, 40, 0.42).
narrative_ontology:measurement(qwerty_ipr_tr_t50, qwerty_persistence__incumbent_preservation_reading, theater_ratio, 50, 0.45).

% Extraction over time
narrative_ontology:measurement(qwerty_ipr_be_t0, qwerty_persistence__incumbent_preservation_reading, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(qwerty_ipr_be_t10, qwerty_persistence__incumbent_preservation_reading, base_extractiveness, 10, 0.3).
narrative_ontology:measurement(qwerty_ipr_be_t20, qwerty_persistence__incumbent_preservation_reading, base_extractiveness, 20, 0.48).
narrative_ontology:measurement(qwerty_ipr_be_t30, qwerty_persistence__incumbent_preservation_reading, base_extractiveness, 30, 0.55).
narrative_ontology:measurement(qwerty_ipr_be_t40, qwerty_persistence__incumbent_preservation_reading, base_extractiveness, 40, 0.6).
narrative_ontology:measurement(qwerty_ipr_be_t50, qwerty_persistence__incumbent_preservation_reading, base_extractiveness, 50, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(qwerty_ipr_su_t0, qwerty_persistence__incumbent_preservation_reading, suppression_requirement, 0, 0.2).
narrative_ontology:measurement(qwerty_ipr_su_t10, qwerty_persistence__incumbent_preservation_reading, suppression_requirement, 10, 0.35).
narrative_ontology:measurement(qwerty_ipr_su_t20, qwerty_persistence__incumbent_preservation_reading, suppression_requirement, 20, 0.65).
narrative_ontology:measurement(qwerty_ipr_su_t30, qwerty_persistence__incumbent_preservation_reading, suppression_requirement, 30, 0.55).
narrative_ontology:measurement(qwerty_ipr_su_t40, qwerty_persistence__incumbent_preservation_reading, suppression_requirement, 40, 0.5).
narrative_ontology:measurement(qwerty_ipr_su_t50, qwerty_persistence__incumbent_preservation_reading, suppression_requirement, 50, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(qwerty_persistence__incumbent_preservation_reading, lapsed_alternatives_reading).

% DUAL FORMULATION NOTE:
% The qwerty_persistence kernel decomposes into two structurally distinct constraints: this incumbent_preservation_reading (extractive, actively enforced) and the sibling lapsed_alternatives_reading (passive coordination, network-effects driven). Their epsilon values and beneficiary/victim structures differ widely and must not be averaged into a single constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
