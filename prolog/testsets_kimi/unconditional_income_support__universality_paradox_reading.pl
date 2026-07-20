% ============================================================================
% CONSTRAINT STORY: unconditional_income_support__universality_paradox_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_unconditional_income_support__universality_paradox_reading, []).

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
    narrative_ontology:stakeholder_non_agent/2,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: unconditional_income_support__universality_paradox_reading
 *   human_readable: Unconditional Income Support: Universality Paradox Reading
 *   domain: political_economy/social_policy
 *
 * SUMMARY:
 *   Unconditional income support policies (universal basic income, negative
 *   income tax, child allowances) are frequently promoted with
 *   cross-ideological appeal: left-wing proponents emphasize poverty
 *   reduction and decommodification, while right-wing proponents emphasize
 *   administrative efficiency and labor-market deregulation. The
 *   universality_paradox_reading treats this cross-ideological appeal not as
 *   a stable coalition but as a structurally ambiguous Trojan horse: the same
 *   policy label conceals incompatible implementation paths (generous
 *   unconditional grants versus minimal transfers with heavy taxing-back)
 *   that converge on similar net fiscal outcomes. Political entrepreneurs and
 *   policy designers benefit from the ambiguity, which permits
 *   coalition-building without committing to redistributive magnitude.
 *   Targeted program recipients and the public good of ideological clarity
 *   are victimized: ambiguity prevents coherent policy evaluation and can be
 *   weaponized to justify cuts to means-tested services. This reading
 *   instantiates one structural interpretation of the
 *   unconditional_income_support kernel; it coexists with sibling readings
 *   that frame the policy as either a freedom floor or a dependency trap.
 *
 * KEY AGENTS:
 *   - Political entrepreneurs (organized/mobile beneficiaries): exploit policy ambiguity to assemble cross-ideological coalitions without committing to specific fiscal outcomes.
 *   - Policy designers (moderate/mobile beneficiaries): gain professional autonomy and rhetorical flexibility from designing taxing-back mechanisms that neutralize the universal-targeted distinction.
 *   - Targeted program recipients (powerless/trapped payers): face net benefit erosion through taxing-back and reduced targeted services, with no exit from the tax-benefit system.
 *   - Ideological clarity (analytical-scope abstract payer): the public good of transparent normative differentiation is degraded by the policy's structural ambiguity.
 *   - Fiscal transparency advocates and means-tested advocacy groups (excluded): would demand explicit accounting or progressive targeting but are marginalized by the ambiguity-dependent coalition.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(unconditional_income_support__universality_paradox_reading, 0.38).
domain_priors:suppression_score(unconditional_income_support__universality_paradox_reading, 0.4).
domain_priors:theater_ratio(unconditional_income_support__universality_paradox_reading, 0.5).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(unconditional_income_support__universality_paradox_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(unconditional_income_support__universality_paradox_reading, suppression_requirement, 0.4).
narrative_ontology:constraint_metric(unconditional_income_support__universality_paradox_reading, theater_ratio, 0.5).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(unconditional_income_support__universality_paradox_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(unconditional_income_support__universality_paradox_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(unconditional_income_support__universality_paradox_reading, tangled_rope).
narrative_ontology:human_readable(unconditional_income_support__universality_paradox_reading, "Unconditional Income Support: Universality Paradox Reading").
narrative_ontology:topic_domain(unconditional_income_support__universality_paradox_reading, "political_economy/social_policy").

domain_priors:requires_active_enforcement(unconditional_income_support__universality_paradox_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(unconditional_income_support__universality_paradox_reading, '14f49f31-ad90-4ca5-87bf-ee1296c49d09').
narrative_ontology:cs_kernel_codification('14f49f31-ad90-4ca5-87bf-ee1296c49d09', formalized).
narrative_ontology:cs_authority_grounding('14f49f31-ad90-4ca5-87bf-ee1296c49d09', distributed).
narrative_ontology:cs_reading_relation('14f49f31-ad90-4ca5-87bf-ee1296c49d09', unconditional_income_support__freedom_floor_reading, coexists_with).
narrative_ontology:cs_reading_relation('14f49f31-ad90-4ca5-87bf-ee1296c49d09', unconditional_income_support__dependency_trap_reading, coexists_with).
narrative_ontology:cs_axiom('14f49f31-ad90-4ca5-87bf-ee1296c49d09', foundational, fiscal_equivalence_under_taxing_back).
narrative_ontology:cs_axiom_status(fiscal_equivalence_under_taxing_back, holdable).
narrative_ontology:cs_axiom_grounding('14f49f31-ad90-4ca5-87bf-ee1296c49d09', fiscal_equivalence_under_taxing_back, empirically_contingent).
narrative_ontology:cs_axiom('14f49f31-ad90-4ca5-87bf-ee1296c49d09', foundational, ambiguity_as_coalition_prerequisite).
narrative_ontology:cs_axiom_status(ambiguity_as_coalition_prerequisite, holdable).
narrative_ontology:cs_axiom_grounding('14f49f31-ad90-4ca5-87bf-ee1296c49d09', ambiguity_as_coalition_prerequisite, instrumental).
narrative_ontology:cs_reference_frame('14f49f31-ad90-4ca5-87bf-ee1296c49d09', cross_ideological_coalition_vehicle).
narrative_ontology:cs_drift_state('14f49f31-ad90-4ca5-87bf-ee1296c49d09', contemporary_welfare_reform_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('14f49f31-ad90-4ca5-87bf-ee1296c49d09', '').
narrative_ontology:cs_kernel_id(unconditional_income_support__universality_paradox_reading, unconditional_income_support).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(unconditional_income_support__universality_paradox_reading, political_entrepreneurs).
narrative_ontology:constraint_beneficiary(unconditional_income_support__universality_paradox_reading, policy_designers).
narrative_ontology:constraint_victim(unconditional_income_support__universality_paradox_reading, targeted_program_recipients).
narrative_ontology:constraint_victim(unconditional_income_support__universality_paradox_reading, ideological_clarity).
narrative_ontology:constraint_vindicates(unconditional_income_support__universality_paradox_reading, taxing_back_neutrality).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Build cross-ideological coalitions by framing unconditional income support in vague terms that appeal simultaneously to anti-poverty advocates and anti-bureaucracy libertarians; benefit from the policy's ambiguity to secure diverse support without committing to specific fiscal mechanisms or net distributive outcomes.
narrative_ontology:constraint_stakeholder(unconditional_income_support__universality_paradox_reading, political_entrepreneurs, beneficiary,
    organized, biographical, mobile, national).

% Design tax-and-transfer mechanisms that make nominally universal benefits fiscally equivalent to targeted aid through taxing-back; gain professional autonomy and rhetorical flexibility from the ambiguity between universal provision and net redistribution.
narrative_ontology:constraint_stakeholder(unconditional_income_support__universality_paradox_reading, policy_designers, beneficiary,
    moderate, biographical, mobile, national).

% Receive nominally universal support but face implicit claw-backs through the tax system; experience reduced or eliminated targeted services as universality narratives justify cuts to means-tested aid; lack political voice to distinguish gross transfers from net benefit.
narrative_ontology:constraint_stakeholder(unconditional_income_support__universality_paradox_reading, targeted_program_recipients, payer,
    powerless, immediate, trapped, national).

% The public good of coherent ideological differentiation is degraded: the same policy label binds incompatible normative commitments (decommodification versus deregulation), preventing transparent evaluation of which vision is actually being implemented.
narrative_ontology:constraint_stakeholder(unconditional_income_support__universality_paradox_reading, ideological_clarity, payer,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(unconditional_income_support__universality_paradox_reading, ideological_clarity).

% Would demand explicit budgetary accounting of gross versus net transfers and clear distributional scoring; excluded because the political strategy depends on obscuring the taxing-back arithmetic that neutralizes the universal-targeted distinction.
narrative_ontology:constraint_stakeholder(unconditional_income_support__universality_paradox_reading, fiscal_transparency_advocates, excluded,
    moderate, generational, constrained, national).

% Would insist on progressive targeting and robust means-testing to protect the poor; excluded from the coalition because the universality narrative sidelines their constituency and reframes targeted aid as bureaucratic failure.
narrative_ontology:constraint_stakeholder(unconditional_income_support__universality_paradox_reading, means_tested_advocacy_groups, excluded,
    organized, biographical, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Enables cross-ideological coalition formation around a shared policy vehicle despite incompatible normative commitments; coordinates political support across left-wing anti-poverty advocates and right-wing anti-bureaucracy libertarians by deferring implementation specifics to a later stage that never arrives.
% TRANSFER_FUNCTION: Moves political capital and policy-design flexibility from the realm of transparent fiscal trade-offs to political entrepreneurs and technocrats who leverage ambiguity; moves fiscal resources from targeted program recipients to universal schemes whose net effect is neutralized by taxing-back.
% ABSENT_VOICES: Targeted social policy advocates who would insist on means-testing and anti-poverty targeting; fiscal hawks who would demand explicit budget accounting; ideological purists on both left and right who would reject the compromise if its net structure were visible.
% DISAPPEARANCE_RATIONALE: If the ambiguity vanished and the policy's fiscal structure were transparent, the cross-ideological coalition would fracture: left-wing supporters would confront regressive taxing-back, right-wing supporters would confront net fiscal redistribution, and the political vehicle would lose its coalition-building capacity. Policy debate would reorganize around explicit distributive conflict.
% FOUNDING_PROBLEM: Poverty and economic insecurity in advanced welfare states; political fragmentation preventing coherent social policy reform; administrative complexity and stigma of targeted welfare delivery.
% FOUNDING_PROBLEM_CORROBORATION: Anti-poverty researchers and international organizations (UNICEF, OECD) attest to the persistence of poverty and administrative burden, but many contest whether unconditional universality is the appropriate response, noting taxing-back neutrality. Right-leaning fiscal institutions (IMF, national treasuries) corroborate the administrative burden but prefer targeted support, corroborating from outside the beneficiary set that the universality framing is not the only solution.
narrative_ontology:disappearance_verdict(unconditional_income_support__universality_paradox_reading, world_rearranges).
narrative_ontology:founding_problem_status(unconditional_income_support__universality_paradox_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(unconditional_income_support__universality_paradox_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(unconditional_income_support__universality_paradox_reading, 'none', 1).
narrative_ontology:epsilon_provenance(unconditional_income_support__universality_paradox_reading, 0.38, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(unconditional_income_support__universality_paradox_reading_tests).
:- end_tests(unconditional_income_support__universality_paradox_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.38) because fiscal research shows net outcomes converge across designs; the constraint extracts mainly political clarity and targeted-program funding rather than massive fiscal transfers. Suppression (0.40) reflects the active discursive labor required to keep incompatible framings from colliding in public debate. Theater_ratio (0.50) captures the performative cross-ideological appeal that exceeds the policy's substantive differences across implementations. Accessibility_collapse (0.45): once the taxing-back mechanism is understood, the distinction between 'universal' and 'targeted' alternatives collapses, but this understanding is actively obscured. Resistance (0.40): targeted-aid advocates and fiscal transparency groups resist, but their voices are structurally marginalized by the broad coalition's dominance. The measurement series oscillate slightly: theater peaks mid-interval as the policy matures and the gap between promise and structure widens, then modestly declines as the ambiguity becomes harder to sustain.
 *
 * PERSPECTIVAL GAP:
 *   Political entrepreneurs and policy designers experience the constraint as a coordination device enabling coalition formation and professional autonomy (low d, low effective extraction). Targeted program recipients experience it as a vehicle for redirecting scarce resources and obscuring net benefit cuts (high d, high effective extraction). The excluded advocacy seats experience a form of suppression without extraction: their preferred alternatives are structurally crowded out by the ambiguous universal vehicle.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations (political_entrepreneurs, policy_designers) drive directionality toward the beneficiary end for those seats. Victim declarations (targeted_program_recipients, ideological_clarity) drive directionality toward the target end. Exit options differentiate strongly: political entrepreneurs and policy designers are mobile (can shift framing or career), while targeted recipients are trapped in the tax-benefit system with no arbitrage path. The abstract victim ideological_clarity has no exit by definition.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as tangled_rope prevents mislabeling the ambiguity as pure extraction (snare) because the constraint genuinely coordinates a cross-ideological coalition that would not form around transparently targeted or explicitly libertarian designs. It prevents mislabeling as pure coordination (rope) because the same ambiguity asymmetrically harms targeted populations and degrades public deliberation. The active enforcement requirement is satisfied by the ongoing discursive labor of maintaining incompatible framings simultaneously.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    universality_paradox_reading_scope,
    'Does the unconditional income support kernel structurally entail political ambiguity, or is ambiguity merely a contingent feature of current political strategy?',
    'Comparative analysis across jurisdictions: if diverse implementations universally exhibit taxing-back convergence and coalition ambiguity, the ambiguity is structural; if some implementations maintain ideological coherence, it is contingent.',
    'If structural, this reading captures the kernel''s invariant feature and the sibling readings (freedom_floor, dependency_trap) are observer-relative framings of the same ambiguity. If contingent, this reading is a historically bounded political strategy rather than a kernel-necessary constraint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(universality_paradox_reading_scope, conceptual, 'Whether political ambiguity is structurally necessary to the kernel or contingent.').

omega_variable(
    taxing_back_fiscal_equivalence,
    'Are the fiscal outcomes of ''universal'' and ''targeted'' income support genuinely equivalent under taxing-back, or does the equivalence mask scope for net redistribution?',
    'Microsimulation modeling across tax-benefit systems in multiple countries, controlling for take-up rates, administrative costs, and marginal tax rate effects.',
    'If equivalence is robust, the constraint''s extraction is primarily political (ambiguity) rather than fiscal; if equivalence breaks down, the constraint may function as a covert redistribution mechanism, shifting classification toward snare or scaffold.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(taxing_back_fiscal_equivalence, empirical, 'Whether taxing-back produces genuine fiscal equivalence or masks redistribution.').

omega_variable(
    ambiguity_as_enforcement,
    'Is the suppression of ideological clarity achieved through discursive control (active political framing) or epistemic opacity (inherent complexity of tax-benefit interaction)?',
    'Discourse analysis of policy debates versus technical modeling accessibility: if clarity is recoverable from public documents but absent from political rhetoric, suppression is discursive; if technical complexity genuinely prevents clarity even to experts, suppression is epistemic.',
    'Discursive suppression supports the tangled_rope classification with active enforcement; epistemic opacity suggests a rope or mountain (genuine coordination around irreducible complexity).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ambiguity_as_enforcement, conceptual, 'Whether suppression of clarity is discursive or epistemic.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(unconditional_income_support__universality_paradox_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(unco_tr_t0, unconditional_income_support__universality_paradox_reading, theater_ratio, 0, 0.3).
narrative_ontology:measurement(unco_tr_t6, unconditional_income_support__universality_paradox_reading, theater_ratio, 6, 0.42).
narrative_ontology:measurement(unco_tr_t12, unconditional_income_support__universality_paradox_reading, theater_ratio, 12, 0.52).
narrative_ontology:measurement(unco_tr_t18, unconditional_income_support__universality_paradox_reading, theater_ratio, 18, 0.55).
narrative_ontology:measurement(unco_tr_t24, unconditional_income_support__universality_paradox_reading, theater_ratio, 24, 0.52).
narrative_ontology:measurement(unco_tr_t30, unconditional_income_support__universality_paradox_reading, theater_ratio, 30, 0.5).

% Extraction over time
narrative_ontology:measurement(unco_be_t0, unconditional_income_support__universality_paradox_reading, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(unco_be_t6, unconditional_income_support__universality_paradox_reading, base_extractiveness, 6, 0.22).
narrative_ontology:measurement(unco_be_t12, unconditional_income_support__universality_paradox_reading, base_extractiveness, 12, 0.28).
narrative_ontology:measurement(unco_be_t18, unconditional_income_support__universality_paradox_reading, base_extractiveness, 18, 0.32).
narrative_ontology:measurement(unco_be_t24, unconditional_income_support__universality_paradox_reading, base_extractiveness, 24, 0.35).
narrative_ontology:measurement(unco_be_t30, unconditional_income_support__universality_paradox_reading, base_extractiveness, 30, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(unco_su_t0, unconditional_income_support__universality_paradox_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(unco_su_t6, unconditional_income_support__universality_paradox_reading, suppression_requirement, 6, 0.32).
narrative_ontology:measurement(unco_su_t12, unconditional_income_support__universality_paradox_reading, suppression_requirement, 12, 0.4).
narrative_ontology:measurement(unco_su_t18, unconditional_income_support__universality_paradox_reading, suppression_requirement, 18, 0.45).
narrative_ontology:measurement(unco_su_t24, unconditional_income_support__universality_paradox_reading, suppression_requirement, 24, 0.42).
narrative_ontology:measurement(unco_su_t30, unconditional_income_support__universality_paradox_reading, suppression_requirement, 30, 0.4).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(unconditional_income_support__universality_paradox_reading, unconditional_income_support__freedom_floor_reading).
narrative_ontology:affects_constraint(unconditional_income_support__universality_paradox_reading, unconditional_income_support__dependency_trap_reading).

% DUAL FORMULATION NOTE:
% The unconditional_income_support kernel decomposes into three structurally distinct constraints: the freedom_floor_reading (coordination/autonomy), the dependency_trap_reading (extraction/incentives), and this universality_paradox_reading (ambiguity/coalition politics). Each has a distinct epsilon, beneficiary/victim structure, and classification. They are linked as a constraint family because they share the same policy kernel but instantiate different structural claims about its operation.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
