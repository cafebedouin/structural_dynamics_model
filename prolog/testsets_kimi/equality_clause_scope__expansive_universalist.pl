% ============================================================================
% CONSTRAINT STORY: equality_clause_scope__expansive_universalist
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_equality_clause_scope__expansive_universalist, []).

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
 *   constraint_id: equality_clause_scope__expansive_universalist
 *   human_readable: Expansive Universalist Reading of Constitutional Equality
 *   domain: constitutional law / political philosophy / civil rights history
 *
 * SUMMARY:
 *   This constraint story models the expansive universalist reading of a
 *   constitutional equality clause, under which equality is treated as a
 *   self-evident, universal moral truth binding on all human persons
 *   regardless of historical exclusions. The reading functions as an
 *   interpretive constraint on legislatures and state actors: any law or
 *   practice that draws invidious distinctions is presumptively invalid, and
 *   courts possess low-threshold legitimacy to expand protected categories
 *   without awaiting democratic amendment. The reading claims mountain-like
 *   self-evidence but operates through institutional enforcement, active
 *   suppression of alternative originalist methodologies, and asymmetric
 *   extraction of policy autonomy from state governments. It is claimed as a
 *   rope-like coordination norm while the metrics describe tangled_rope
 *   operation.
 *
 * KEY AGENTS:
 *   - federal_judiciary: Primary agenda-setter (institutional/identity_locked) â acquires interpretive authority and institutional power through universalist expansion
 *   - marginalized_communities: Primary beneficiary (powerless/constrained) â depend on judicial protection against majoritarian exclusion
 *   - state_governments: Primary payer (institutional/constrained) â lose policy autonomy and bear compliance costs
 *   - originalist_jurists: Excluded voice (organized/constrained) â interpretive framework structurally delegitimized
 *   - civil_rights_bar: Secondary beneficiary (organized/mobile) â organizational vitality tied to reading's doctrinal dominance
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(equality_clause_scope__expansive_universalist, 0.55).
domain_priors:suppression_score(equality_clause_scope__expansive_universalist, 0.7).
domain_priors:theater_ratio(equality_clause_scope__expansive_universalist, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(equality_clause_scope__expansive_universalist, extractiveness, 0.55).
narrative_ontology:constraint_metric(equality_clause_scope__expansive_universalist, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(equality_clause_scope__expansive_universalist, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(equality_clause_scope__expansive_universalist, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(equality_clause_scope__expansive_universalist, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(equality_clause_scope__expansive_universalist, tangled_rope).
narrative_ontology:human_readable(equality_clause_scope__expansive_universalist, "Expansive Universalist Reading of Constitutional Equality").
narrative_ontology:topic_domain(equality_clause_scope__expansive_universalist, "constitutional law / political philosophy / civil rights history").

domain_priors:requires_active_enforcement(equality_clause_scope__expansive_universalist).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(equality_clause_scope__expansive_universalist, 'ea143f4a-6442-4925-a6cd-ec3190e86504').
narrative_ontology:cs_kernel_codification('ea143f4a-6442-4925-a6cd-ec3190e86504', fixed_text).
narrative_ontology:cs_authority_grounding('ea143f4a-6442-4925-a6cd-ec3190e86504', lineage).
narrative_ontology:cs_interpretation_layer_present('ea143f4a-6442-4925-a6cd-ec3190e86504').
narrative_ontology:cs_reading_relation('ea143f4a-6442-4925-a6cd-ec3190e86504', equality_clause_scope__restrictive_originalist, forecloses).
narrative_ontology:cs_reading_relation('ea143f4a-6442-4925-a6cd-ec3190e86504', equality_clause_scope__progressive_textualist, coexists_with).
narrative_ontology:cs_axiom('ea143f4a-6442-4925-a6cd-ec3190e86504', foundational, human_equality_universal_self_evident).
narrative_ontology:cs_axiom_status(human_equality_universal_self_evident, holdable).
narrative_ontology:cs_axiom_grounding('ea143f4a-6442-4925-a6cd-ec3190e86504', human_equality_universal_self_evident, deontological).
narrative_ontology:cs_axiom('ea143f4a-6442-4925-a6cd-ec3190e86504', foundational, historical_exclusion_correctable_hypocrisy).
narrative_ontology:cs_axiom_status(historical_exclusion_correctable_hypocrisy, holdable).
narrative_ontology:cs_axiom_grounding('ea143f4a-6442-4925-a6cd-ec3190e86504', historical_exclusion_correctable_hypocrisy, deontological).
narrative_ontology:cs_reference_frame('ea143f4a-6442-4925-a6cd-ec3190e86504', universal_human_equality_baseline).
narrative_ontology:cs_drift_state('ea143f4a-6442-4925-a6cd-ec3190e86504', contemporary_originalist_resurgence, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('ea143f4a-6442-4925-a6cd-ec3190e86504', '').
narrative_ontology:cs_kernel_id(equality_clause_scope__expansive_universalist, equality_clause_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(equality_clause_scope__expansive_universalist, marginalized_communities).
narrative_ontology:constraint_beneficiary(equality_clause_scope__expansive_universalist, federal_judiciary).
narrative_ontology:constraint_beneficiary(equality_clause_scope__expansive_universalist, civil_rights_bar).
narrative_ontology:constraint_victim(equality_clause_scope__expansive_universalist, state_governments).
narrative_ontology:constraint_victim(equality_clause_scope__expansive_universalist, originalist_jurists).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interprets the constitutional equality clause as a universal, self-evident moral baseline binding on all levels of government. Through expansive review, it acquires institutional authority to invalidate democratically enacted laws and state customs that exclude historically marginalized groups. The professional identity of the federal bench is fused to this interpretive project; abandoning universalist premises would constitute a rupture in judicial self-understanding.
narrative_ontology:constraint_stakeholder(equality_clause_scope__expansive_universalist, federal_judiciary, agenda_setter,
    institutional, generational, identity_locked, national).

% Receive federal judicial protection against discriminatory state and private action. Their access to equal citizenship depends almost entirely on judicial willingness to expand the equality principle, as majoritarian political processes have historically failed to remedy exclusion. Alternative routes to redress are structurally blocked by political marginalization.
narrative_ontology:constraint_stakeholder(equality_clause_scope__expansive_universalist, marginalized_communities, beneficiary,
    powerless, generational, constrained, national).

% Litigates, advocates, and organizes around the expansive universalist framework. Organizational funding, professional prestige, and strategic momentum depend on the reading continuing to supply a viable judicial path to rights recognition. While individual attorneys could shift practice areas, the bar as an institution is anchored to this interpretive paradigm.
narrative_ontology:constraint_stakeholder(equality_clause_scope__expansive_universalist, civil_rights_bar, beneficiary,
    organized, biographical, mobile, national).

% Bear the loss of policy autonomy when federal courts strike down exclusionary laws, mandate busing, or require institutional restructuring. Must deploy fiscal and administrative resources to comply with federal equality mandates even when those mandates contradict local democratic preferences. Exit is constrained by the supremacy clause and the threat of federal enforcement.
narrative_ontology:constraint_stakeholder(equality_clause_scope__expansive_universalist, state_governments, payer,
    institutional, generational, constrained, national).

% Maintain an alternative interpretive methodology that limits equality to historically intended beneficiaries, but their framework is treated as morally illegitimate within dominant constitutional discourse. They remain employed within the legal profession yet are structurally excluded from judicial appointment processes and mainstream academic legitimacy to the degree they reject universalist premises.
narrative_ontology:constraint_stakeholder(equality_clause_scope__expansive_universalist, originalist_jurists, excluded,
    organized, generational, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(equality_clause_scope__expansive_universalist, federal_judiciary).
narrative_ontology:fixing_cost_class(equality_clause_scope__expansive_universalist, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a single universal normative standard against which all legislation and state action is evaluated, coordinating a diverse federal polity without requiring democratic consensus on each specific exclusion or remedy.
% TRANSFER_FUNCTION: Moves interpretive authority from state legislatures and local majorities to federal courts; moves legal protection and remedial recognition from the political process to historically marginalized groups.
% ABSENT_VOICES: Restrictive originalists and states'-rights federalists are structurally excluded from the interpretive forum even though they command significant electoral majorities in many jurisdictions; their readings are treated as outside the bounds of legitimate constitutional argument.
% DISAPPEARANCE_RATIONALE: If the expansive universalist reading disappeared overnight, federal courts would lose the primary doctrinal engine for striking down discriminatory state laws; marginalized groups would be remitted to majoritarian politics and amendment processes; state governments would regain broad autonomy to regulate status and allocation; civil rights jurisprudence would reorganize around textual specificity or democratic proceduralism.
% FOUNDING_PROBLEM: How to secure equal citizenship and prevent majority tyranny in a federal system with deeply entrenched, historically racialized and gendered exclusions that democratic processes had proven unwilling to remedy.
% FOUNDING_PROBLEM_CORROBORATION: Historians and sociologists outside the judiciary document persistent structural exclusion corroborating the problem's continued vitality; originalist jurists and some political scientists contest that the federal judiciary was the proper solver, arguing the problem was either solved by prior amendments or should be left to democratic politics. Marginalized communities corroborate the persistence from outside the agenda-setting seat.
narrative_ontology:disappearance_verdict(equality_clause_scope__expansive_universalist, world_rearranges).
narrative_ontology:founding_problem_status(equality_clause_scope__expansive_universalist, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(equality_clause_scope__expansive_universalist, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(equality_clause_scope__expansive_universalist, 'none', 1).
narrative_ontology:epsilon_provenance(equality_clause_scope__expansive_universalist, 0.55, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(equality_clause_scope__expansive_universalist_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(equality_clause_scope__expansive_universalist, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(equality_clause_scope__expansive_universalist_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.55) is moderate-to-high because the reading systematically transfers interpretive authority from elected institutions to life-tenured courts and imposes compliance costs on state governments. Suppression (0.70) reflects the active exclusion of originalist methodology from legitimate constitutional discourse and the federal coercion required to enforce judicial mandates against resistant states. Theater_ratio (0.40 at interval end) captures the growing gap between expansive equality rhetoric and material enforcement, particularly in an era of performative institutional allyship paired with stagnant structural outcomes. Accessibility_collapse (0.78) is high because once the universalist premise is accepted, restrictive alternatives appear morally indefensible. Resistance (0.72) is high due to sustained originalist, federalist, and majoritarian pushback.
 *
 * PERSPECTIVAL GAP:
 *   The federal judiciary experiences this reading as the enforcement of self-evident moral truth and genuine coordination of a diverse polity under universal principle. State governments experience it as federal overreach and extraction of democratic autonomy. Marginalized communities experience it as necessary protection and delayed justice. Originalist jurists experience it as ideological capture of constitutional meaning. The engine computes this divergence from the same structural facts â the asymmetry of power, exit, and declared role â without adjudicating which seat is 'correct.'
 *
 * DIRECTIONALITY LOGIC:
 *   The federal judiciary sits near the beneficiary end because it acquires institutional authority and agenda-setting power through expansive interpretation; it also sets the rules. Marginalized communities are declared beneficiaries and experience the constraint as protective subsidy, though their constrained exit (total dependence on judicial remedy) means their effective extraction is damped rather than inverted. State governments are declared victims with constrained exit (must comply with federal orders), placing them near the full-target end. Originalist jurists are excluded from the interpretive forum and bear delegitimization costs. Civil rights bar benefits organizationally but retains enough mobility to avoid full subsidy lock-in.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading resists mandatrophy mislabeling because its coordination function (universal rights protection against majority tyranny) is structurally inseparable from its extraction function (judicial override of democratic majorities and suppression of alternative interpretive methodologies). A pure coordination reading (rope) would require beneficiaries without victims and no active enforcement; a pure extraction reading (snare) would lack the genuine protective function for marginalized groups. The tangled_rope classification captures that both functions are real, that the constraint requires active enforcement to persist against majoritarian resistance, and that the same structure coordinates some while extracting from others.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_or_constructed_norm,
    'Is the equality principle genuinely self-evident and naturally binding, or is it a constructed legal norm that mobilizes moral rhetoric to secure institutional authority?',
    'Comparative historical analysis: if equality collapses as a constraint when judicial enforcement is withdrawn, it is constructed; if it persists through non-judicial social coordination, it approaches natural-law status.',
    'If constructed, the constraint''s high accessibility_collapse is an artifact of ideological dominance rather than structural necessity, supporting reclassification toward snare or piton depending on beneficiary concentration.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_or_constructed_norm, conceptual, 'Whether the universalist equality claim is a natural law or an enforced construct').

omega_variable(
    judicial_power_transfer_legitimacy,
    'Does expansive judicial interpretation solve a genuine collective-action problem in protecting minorities, or does it extract democratic legitimacy from majorities without adequate compensatory coordination?',
    'Cross-jurisdictional comparison of rights outcomes in systems with strong judicial review versus parliamentary sovereignty, controlling for pre-existing social capital.',
    'If judicial expansion produces better minority protections without destabilizing democratic coordination, the extraction component is damped; if it produces equivalent or worse outcomes while concentrating power, extraction dominates.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(judicial_power_transfer_legitimacy, empirical, 'Whether judicial rights expansion coordinates or extracts').

omega_variable(
    suppression_internalization_in_states,
    'Is state compliance with expansive equality mandates driven by internalized legitimacy or by structural federal coercion?',
    'Post-enforcement trajectory analysis: if equality norms persist in state legislation after federal enforcement threats recede, suppression is partially internalized; if compliance collapses, suppression was purely structural.',
    'If internalized, effective suppression is higher than structural measures suggest; if purely structural, the constraint is more brittle and its theater ratio may understate performative compliance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_internalization_in_states, empirical, 'Structural versus internalized suppression mechanism in state compliance').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(equality_clause_scope__expansive_universalist, 0, 150).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(equa_tr_t0, equality_clause_scope__expansive_universalist, theater_ratio, 0, 0.1).
narrative_ontology:measurement(equa_tr_t30, equality_clause_scope__expansive_universalist, theater_ratio, 30, 0.35).
narrative_ontology:measurement(equa_tr_t60, equality_clause_scope__expansive_universalist, theater_ratio, 60, 0.3).
narrative_ontology:measurement(equa_tr_t90, equality_clause_scope__expansive_universalist, theater_ratio, 90, 0.2).
narrative_ontology:measurement(equa_tr_t120, equality_clause_scope__expansive_universalist, theater_ratio, 120, 0.3).
narrative_ontology:measurement(equa_tr_t150, equality_clause_scope__expansive_universalist, theater_ratio, 150, 0.4).

% Extraction over time
narrative_ontology:measurement(equa_be_t0, equality_clause_scope__expansive_universalist, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(equa_be_t30, equality_clause_scope__expansive_universalist, base_extractiveness, 30, 0.2).
narrative_ontology:measurement(equa_be_t60, equality_clause_scope__expansive_universalist, base_extractiveness, 60, 0.25).
narrative_ontology:measurement(equa_be_t90, equality_clause_scope__expansive_universalist, base_extractiveness, 90, 0.55).
narrative_ontology:measurement(equa_be_t120, equality_clause_scope__expansive_universalist, base_extractiveness, 120, 0.62).
narrative_ontology:measurement(equa_be_t150, equality_clause_scope__expansive_universalist, base_extractiveness, 150, 0.55).

% Suppression requirement over time
narrative_ontology:measurement(equa_su_t0, equality_clause_scope__expansive_universalist, suppression_requirement, 0, 0.85).
narrative_ontology:measurement(equa_su_t30, equality_clause_scope__expansive_universalist, suppression_requirement, 30, 0.3).
narrative_ontology:measurement(equa_su_t60, equality_clause_scope__expansive_universalist, suppression_requirement, 60, 0.45).
narrative_ontology:measurement(equa_su_t90, equality_clause_scope__expansive_universalist, suppression_requirement, 90, 0.8).
narrative_ontology:measurement(equa_su_t120, equality_clause_scope__expansive_universalist, suppression_requirement, 120, 0.6).
narrative_ontology:measurement(equa_su_t150, equality_clause_scope__expansive_universalist, suppression_requirement, 150, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(equality_clause_scope__expansive_universalist, identity_coordination).
narrative_ontology:boltzmann_floor_override(equality_clause_scope__expansive_universalist, 0.08).
narrative_ontology:affects_constraint(equality_clause_scope__expansive_universalist, equality_clause_scope__restrictive_originalist).
narrative_ontology:affects_constraint(equality_clause_scope__expansive_universalist, equality_clause_scope__progressive_textualist).

% DUAL FORMULATION NOTE:
% The equality_clause_scope kernel decomposes into three structurally distinct constraints because the label 'equality' conflates incompatible beneficiary scopes, amendment mechanisms, and epistemic foundations. Each reading has a distinct epsilon, victim/beneficiary structure, and enforcement profile. This story (expansive_universalist) influences the sibling readings by shifting the legitimacy conditions of constitutional argument without foreclosing the textualist alternative.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
