% ============================================================================
% CONSTRAINT STORY: empty_shell_tolerance
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2025-01-09
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_empty_shell_tolerance, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: empty_shell_tolerance
 *   human_readable: Boomer Cohort Empty-Shell Marriage Tolerance Decline
 *   domain: family_sociology/demography/gender_studies
 *
 * SUMMARY:
 *   Between 2001 and 2021, the Boomer cohort's moral acceptability of divorce
 *   rose from 59% to 79%, driven not by rising conflict but by declining
 *   tolerance for maintaining emotionally empty marriages. This normative
 *   shift is downstream of the longevity mismatch constraint: marriages now
 *   last 50+ years, and the cultural script for maintaining vitality across
 *   that span has not kept pace. The tolerance decline is a coordination
 *   update—many wanted the option to exit but individual exits were
 *   stigmatized until the norm shifted cohort-wide. The constraint is claimed
 *   as rope (genuine coordination solving a collective action problem) and
 *   the metrics are authored to reflect moderate extraction with declining
 *   suppression over time, as the normative floor dropped and exits became
 *   less costly.
 *
 * KEY AGENTS:
 *   - boomer_women_in_unsatisfying_marriages: Primary beneficiaries (moderate/constrained) — gained social permission to exit emotionally hollow marriages
 *   - boomer_men_seeking_exit: Primary beneficiaries (moderate/mobile) — same normative shift, lower economic barriers
 *   - economically_dependent_spouses: Primary victims (powerless/trapped) — lost implicit security when tolerance floor dropped
 *   - religious_traditionalists: Secondary victims (organized/identity_locked) — experience normative shift as moral crisis
 *   - adult_children_of_divorced_boomers: Dual position (moderate/mobile) — benefit from modeled autonomy, bear costs of family disruption
 *   - family_sociologists: Analytical observers — document the structural asymmetry
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(empty_shell_tolerance, 0.28).
domain_priors:suppression_score(empty_shell_tolerance, 0.42).
domain_priors:theater_ratio(empty_shell_tolerance, 0.18).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(empty_shell_tolerance, extractiveness, 0.28).
narrative_ontology:constraint_metric(empty_shell_tolerance, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(empty_shell_tolerance, theater_ratio, 0.18).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(empty_shell_tolerance, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(empty_shell_tolerance, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(empty_shell_tolerance, rope).
narrative_ontology:human_readable(empty_shell_tolerance, "Boomer Cohort Empty-Shell Marriage Tolerance Decline").
narrative_ontology:topic_domain(empty_shell_tolerance, "family_sociology/demography/gender_studies").

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(empty_shell_tolerance, boomer_women_in_unsatisfying_marriages).
narrative_ontology:constraint_beneficiary(empty_shell_tolerance, boomer_men_seeking_exit).
narrative_ontology:constraint_beneficiary(empty_shell_tolerance, adult_children_of_divorced_boomers).
narrative_ontology:constraint_victim(empty_shell_tolerance, economically_dependent_spouses).
narrative_ontology:constraint_victim(empty_shell_tolerance, religious_traditionalists).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(empty_shell_tolerance, adult_children_of_divorced_boomers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Entered marriages under higher normative pressure to stay regardless of satisfaction. As tolerance for empty-shell arrangements declined cohort-wide, gained social permission to exit marriages that had become emotionally hollow. Economic constraints remain (pension division, housing costs, late-career earning capacity) but the moral stigma barrier dropped substantially. The shift in acceptability norms gave them a vocabulary and social support for exit that their mothers' generation lacked.
narrative_ontology:constraint_stakeholder(empty_shell_tolerance, boomer_women_in_unsatisfying_marriages, beneficiary,
    moderate, biographical, constrained, national).

% Benefit from the same normative shift but typically face lower economic barriers to exit than their female counterparts. The declining tolerance for maintaining appearances freed them from the expectation to sustain marriages for social respectability alone. Their exit options are structurally more mobile due to higher average earnings and lower caregiving obligations, though they still navigate the same moral acceptability landscape.
narrative_ontology:constraint_stakeholder(empty_shell_tolerance, boomer_men_seeking_exit, beneficiary,
    moderate, biographical, mobile, national).

% Bear the costs of the tolerance decline when their partners exit. Typically women who specialized in household production under an implicit lifetime partnership contract, now facing late-life economic precarity as the normative floor supporting marriage persistence eroded. The shift in acceptability norms removed a constraint that had protected their economic position, even if that protection came at the cost of emotional connection. They experience the tolerance decline as abandonment legitimized.
narrative_ontology:constraint_stakeholder(empty_shell_tolerance, economically_dependent_spouses, payer,
    powerless, biographical, trapped, national).

% Hold theological commitments to marriage permanence that make the cohort-wide tolerance decline a moral crisis rather than a coordination update. They experience the normative shift as their community abandoning a sacred commitment, and their identity is constituted through upholding marriage as a permanent covenant. Exit from this position would require abandoning the interpretive framework that makes their social world coherent. They cannot adopt the new tolerance norm without repudiating their grounding commitments.
narrative_ontology:constraint_stakeholder(empty_shell_tolerance, religious_traditionalists, payer,
    organized, generational, identity_locked, national).

% Benefit from parents modeling that exit from unsatisfying relationships is legitimate, giving them permission to prioritize emotional connection in their own partnerships. Also bear costs through disrupted family structures, divided holidays, and sometimes economic strain from supporting two parental households. Their net position depends on whether the parental marriage was high-conflict or merely low-vitality; the tolerance decline authorized exit from both types.
narrative_ontology:constraint_stakeholder(empty_shell_tolerance, adult_children_of_divorced_boomers, beneficiary,
    moderate, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(empty_shell_tolerance, adult_children_of_divorced_boomers, payer).

% Track the cohort-wide shift in divorce acceptability through longitudinal survey data. They document that the change is not driven by rising marital conflict but by declining willingness to maintain marriages that have become emotionally empty. They observe the structural asymmetry: the tolerance decline benefits those with exit options and harms those without them, even though the normative shift is framed as universal liberation.
narrative_ontology:constraint_stakeholder(empty_shell_tolerance, family_sociologists, observer,
    analytical, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(empty_shell_tolerance, diffuse).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a cohort-wide update to the normative floor for marriage persistence: from 'stay regardless of satisfaction' to 'exit is legitimate when vitality is gone.' Solves the collective action problem where individual exits were stigmatized but many privately wanted the option.
% TRANSFER_FUNCTION: Transfers social permission and legitimacy from the institution of marriage-as-permanence to individual autonomy in relationship exit decisions. Economically, transfers security from dependent spouses to exiting partners who gain freedom to reallocate resources.
% ABSENT_VOICES: Economically dependent spouses who specialized in household production under an implicit lifetime contract are present but structurally disadvantaged in the discourse. The normative shift is narrated as liberation, which makes it difficult for them to articulate that they are bearing costs without sounding like they oppose others' freedom.
% DISAPPEARANCE_RATIONALE: If the tolerance decline reversed overnight and the cohort returned to 2001 acceptability norms, thousands of late-life divorces would not occur, economically dependent spouses would retain implicit security, and the Boomer cohort's relationship landscape would look structurally different. The normative shift is driving behavioral change, not merely reflecting it.
% FOUNDING_PROBLEM: Longevity mismatch created marriages lasting 50+ years, far beyond historical norms, with no cultural script for maintaining vitality across that span. Many Boomer marriages reached a state of emotional emptiness while both partners still had decades of life expectancy remaining. The founding problem was: how to handle marriages that are not conflictual but have become hollow, when divorce carried severe stigma.
% FOUNDING_PROBLEM_CORROBORATION: Demographers and gerontologists outside the divorcing population document that the longevity increase is real and that relationship satisfaction follows a U-curve with a long trough in middle age. The founding problem—what to do with emotionally empty but non-conflictual marriages when both partners have 20-30 years of life remaining—is independently verified and remains structurally present.
narrative_ontology:disappearance_verdict(empty_shell_tolerance, world_rearranges).
narrative_ontology:founding_problem_status(empty_shell_tolerance, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(empty_shell_tolerance, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-06-24',
    'unspecified', 'agent/example_platform_commission.json',
    'claude-sonnet-4-5-20250929', 'unspecified').
narrative_ontology:story_seed(empty_shell_tolerance, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(empty_shell_tolerance_tests).
:- end_tests(empty_shell_tolerance_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.28 at interval end, declining from 0.38) because the tolerance decline solves a real coordination problem but imposes costs on economically dependent spouses who specialized under an implicit lifetime contract. Suppression is moderate-high initially (0.68) but declining (to 0.42) as the normative shift reduces the social cost of exit—early exits faced substantial stigma; later exits face much less. Theater ratio is low-moderate (0.18 at end, declining from 0.32) because the coordination function is real: the norm shift genuinely solved the collective action problem where individual exits were punished. Accessibility collapse is low-moderate (0.35) because alternatives to the new norm exist—religious communities maintain permanence norms, and some individuals choose to stay in low-vitality marriages. Resistance is moderate-high (0.58) because religious traditionalists and economically dependent spouses actively resist the normative shift, though they lack power to reverse it. The measurement series shows declining extraction and suppression over the interval as the norm consolidated and exits became less costly.
 *
 * PERSPECTIVAL GAP:
 *   From the beneficiary seats (those with exit options), the tolerance decline is experienced as liberation—a long-overdue update that frees them from maintaining hollow relationships. From the economically dependent spouse seat, the same shift is experienced as abandonment legitimized—the normative floor that protected their economic position eroded, and they lack the resources to exit themselves. From the religious traditionalist seat, it is experienced as moral collapse—their community abandoning a sacred commitment. The engine should compute these seats as experiencing different constraint types from the same structural arrangement: beneficiaries see rope (genuine coordination), victims see extraction (costs imposed without consent).
 *
 * DIRECTIONALITY LOGIC:
 *   Boomer women and men seeking exit are beneficiaries—they gain social permission and reduced stigma for exiting unsatisfying marriages. Their directionality sits near the beneficiary end (d ≈ 0.2-0.3), modulated by their constrained/mobile exit options. Economically dependent spouses are victims—they lose the implicit security the old norm provided. Their directionality sits near the target end (d ≈ 0.8-0.9), amplified by their trapped exit options and powerless position. Religious traditionalists are also victims but from an identity-locked position—they cannot adopt the new norm without repudiating their grounding commitments (d ≈ 0.7-0.8). Adult children are dual-positioned: they benefit from modeled autonomy but bear costs of family disruption (d ≈ 0.4-0.5, near symmetric).
 *
 * MANDATROPHY ANALYSIS:
 *   The tolerance decline is not mandatrophy—the founding problem (what to do with emotionally empty marriages when both partners have decades remaining) is still live and independently corroborated. The normative shift is a response to a real structural change (longevity increase), not a zombie constraint persisting after its function is gone. However, the asymmetry in who benefits and who pays is substantial: the coordination function is real, but it coordinates exit options for those who have them while imposing costs on those who don't. This is tangled rope territory—genuine coordination function with asymmetric extraction—but the authored claim is rope and the metrics are moderate, so the engine will measure the divergence.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    economic_dependency_counterfactual,
    'If economically dependent spouses had equal earning capacity and pension rights, would the tolerance decline still impose costs on them, or would they become net beneficiaries?',
    'Cross-national comparison with countries that have stronger economic protections for homemakers (pension splitting, caregiver credits). If dependent spouses in those systems experience the tolerance decline as liberation rather than abandonment, the extraction is economic, not inherent to the normative shift.',
    'If the costs are purely economic, the constraint is closer to pure rope—the coordination function is clean and the extraction is a separable policy failure. If costs persist even with economic parity, the tolerance decline itself is extractive.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(economic_dependency_counterfactual, empirical, 'Whether the extraction on dependent spouses is inherent or economically contingent').

omega_variable(
    vitality_maintenance_script,
    'Is the founding problem (lack of cultural script for maintaining vitality across 50+ year marriages) solvable, or is the tolerance decline the only available response to longevity mismatch?',
    'Longitudinal studies of couples who maintain high satisfaction across long marriages. If a replicable script exists and can be taught, the tolerance decline is one response among alternatives. If no such script exists at scale, the tolerance decline is the only coordination equilibrium available.',
    'If a vitality-maintenance script is viable, the tolerance decline is a choice that benefits exit-seekers at the expense of dependent spouses. If no script exists, the tolerance decline is an unavoidable adaptation to structural change, and the costs on dependent spouses are tragic but not extractive.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(vitality_maintenance_script, empirical, 'Whether the founding problem has an alternative solution').

omega_variable(
    identity_lock_mechanism,
    'Is the identity lock on religious traditionalists structural (their theology genuinely requires permanence) or internalized (they could reinterpret their tradition but choose not to)?',
    'Historical analysis of how religious communities have adapted to other normative shifts (contraception, interfaith marriage). If reinterpretation is theologically available but resisted, the lock is partly internalized. If the permanence doctrine is load-bearing for the entire theological structure, the lock is structural.',
    'If the lock is internalized, the costs on religious traditionalists are self-imposed and the tolerance decline is less extractive. If the lock is structural, they are genuine victims of a normative shift they cannot adopt without self-annihilation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_mechanism, conceptual, 'Whether religious traditionalists'' identity lock is structural or internalized').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(empty_shell_tolerance, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(empt_tr_t0, empty_shell_tolerance, theater_ratio, 0, 0.32).
narrative_ontology:measurement_basis(empt_tr_t0, observed).
narrative_ontology:measurement(empt_tr_t5, empty_shell_tolerance, theater_ratio, 5, 0.28).
narrative_ontology:measurement_basis(empt_tr_t5, observed).
narrative_ontology:measurement(empt_tr_t10, empty_shell_tolerance, theater_ratio, 10, 0.23).
narrative_ontology:measurement_basis(empt_tr_t10, observed).
narrative_ontology:measurement(empt_tr_t15, empty_shell_tolerance, theater_ratio, 15, 0.2).
narrative_ontology:measurement_basis(empt_tr_t15, observed).
narrative_ontology:measurement(empt_tr_t20, empty_shell_tolerance, theater_ratio, 20, 0.18).
narrative_ontology:measurement_basis(empt_tr_t20, observed).

% Extraction over time
narrative_ontology:measurement(empt_be_t0, empty_shell_tolerance, base_extractiveness, 0, 0.38).
narrative_ontology:measurement_basis(empt_be_t0, observed).
narrative_ontology:measurement(empt_be_t5, empty_shell_tolerance, base_extractiveness, 5, 0.35).
narrative_ontology:measurement_basis(empt_be_t5, observed).
narrative_ontology:measurement(empt_be_t10, empty_shell_tolerance, base_extractiveness, 10, 0.31).
narrative_ontology:measurement_basis(empt_be_t10, observed).
narrative_ontology:measurement(empt_be_t15, empty_shell_tolerance, base_extractiveness, 15, 0.29).
narrative_ontology:measurement_basis(empt_be_t15, observed).
narrative_ontology:measurement(empt_be_t20, empty_shell_tolerance, base_extractiveness, 20, 0.28).
narrative_ontology:measurement_basis(empt_be_t20, observed).

% Suppression requirement over time
narrative_ontology:measurement(empt_su_t0, empty_shell_tolerance, suppression_requirement, 0, 0.68).
narrative_ontology:measurement_basis(empt_su_t0, observed).
narrative_ontology:measurement(empt_su_t5, empty_shell_tolerance, suppression_requirement, 5, 0.59).
narrative_ontology:measurement_basis(empt_su_t5, observed).
narrative_ontology:measurement(empt_su_t10, empty_shell_tolerance, suppression_requirement, 10, 0.51).
narrative_ontology:measurement_basis(empt_su_t10, observed).
narrative_ontology:measurement(empt_su_t15, empty_shell_tolerance, suppression_requirement, 15, 0.46).
narrative_ontology:measurement_basis(empt_su_t15, observed).
narrative_ontology:measurement(empt_su_t20, empty_shell_tolerance, suppression_requirement, 20, 0.42).
narrative_ontology:measurement_basis(empt_su_t20, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(empty_shell_tolerance, identity_coordination).

% DUAL FORMULATION NOTE:
% This constraint is downstream of longevity_mismatch (the structural increase in marriage duration due to rising life expectancy). The longevity increase created the founding problem; the tolerance decline is the Boomer cohort's coordination response. The two constraints are linked but have different ε values: longevity_mismatch is a mountain (demographic fact, negligible extraction), while empty_shell_tolerance is a rope with moderate extraction (coordination function with asymmetric costs).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
