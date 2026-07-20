% ============================================================================
% CONSTRAINT STORY: family_law_authority__parsi_zoroastrian_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_family_law_authority__parsi_zoroastrian_reading, []).

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
    narrative_ontology:suppression_profile/2,
    narrative_ontology:coordination_type/2,
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
 *   constraint_id: family_law_authority__parsi_zoroastrian_reading
 *   human_readable: Parsi Zoroastrian Marriage as Community-Preserving Institution
 *   domain: comparative_law/religious_governance
 *
 * SUMMARY:
 *   This constraint instantiates the Parsi Zoroastrian reading of the
 *   family_law_authority kernel: marriage is not merely a personal union but
 *   a communal institution whose primary purpose is to preserve a
 *   micro-minority community. The reading is distinguished from its siblings
 *   by the necessity of priestly ritual validation (Ashirwad), the mandatory
 *   endogamy rule that expels intermarried members, and the small-community
 *   preservation logic that treats demographic retention as a sacred duty. It
 *   is claimed as coordination (community survival) but structurally extracts
 *   autonomy from individuals who wish to marry outside the fold. The engine
 *   will measure that divergence.
 *
 * KEY AGENTS:
 *   - parsi_priesthood: Primary agenda-setter (institutional/identity_locked) â controls ritual validity and enforces endogamy
 *   - parsi_laity_endogamous: Primary beneficiary (moderate/identity_locked) â retains status and communal rights by complying
 *   - parsi_individuals_intermarriage: Primary target/payer (moderate/constrained) â bears loss of status and exclusion
 *   - non_parsi_spouses: Excluded party (powerless/trapped) â no voice in the constraint that governs their partner
 *   - indian_judiciary: Analytical observer (institutional/analytical) â moderates civil effects without setting doctrine
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(family_law_authority__parsi_zoroastrian_reading, 0.72).
domain_priors:suppression_score(family_law_authority__parsi_zoroastrian_reading, 0.7).
domain_priors:theater_ratio(family_law_authority__parsi_zoroastrian_reading, 0.52).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(family_law_authority__parsi_zoroastrian_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(family_law_authority__parsi_zoroastrian_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(family_law_authority__parsi_zoroastrian_reading, theater_ratio, 0.52).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(family_law_authority__parsi_zoroastrian_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(family_law_authority__parsi_zoroastrian_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(family_law_authority__parsi_zoroastrian_reading, tangled_rope).
narrative_ontology:human_readable(family_law_authority__parsi_zoroastrian_reading, "Parsi Zoroastrian Marriage as Community-Preserving Institution").
narrative_ontology:topic_domain(family_law_authority__parsi_zoroastrian_reading, "comparative_law/religious_governance").

domain_priors:requires_active_enforcement(family_law_authority__parsi_zoroastrian_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(family_law_authority__parsi_zoroastrian_reading, '4a897cc8-76f6-41eb-8471-5cd0cbee57a3').
narrative_ontology:cs_kernel_codification('4a897cc8-76f6-41eb-8471-5cd0cbee57a3', fixed_text).
narrative_ontology:cs_authority_grounding('4a897cc8-76f6-41eb-8471-5cd0cbee57a3', lineage).
narrative_ontology:cs_interpretation_layer_present('4a897cc8-76f6-41eb-8471-5cd0cbee57a3').
narrative_ontology:cs_reading_relation('4a897cc8-76f6-41eb-8471-5cd0cbee57a3', family_law_authority__hindu_dharmashastra_reading, coexists_with).
narrative_ontology:cs_reading_relation('4a897cc8-76f6-41eb-8471-5cd0cbee57a3', family_law_authority__muslim_shariat_reading, coexists_with).
narrative_ontology:cs_reading_relation('4a897cc8-76f6-41eb-8471-5cd0cbee57a3', family_law_authority__christian_canonical_reading, coexists_with).
narrative_ontology:cs_reading_relation('4a897cc8-76f6-41eb-8471-5cd0cbee57a3', family_law_authority__secular_contractual_reading, coexists_with).
narrative_ontology:cs_axiom('4a897cc8-76f6-41eb-8471-5cd0cbee57a3', foundational, priestly_ritual_validity_required).
narrative_ontology:cs_axiom_status(priestly_ritual_validity_required, holdable).
narrative_ontology:cs_axiom_grounding('4a897cc8-76f6-41eb-8471-5cd0cbee57a3', priestly_ritual_validity_required, theological).
narrative_ontology:cs_axiom('4a897cc8-76f6-41eb-8471-5cd0cbee57a3', foundational, endogamy_mandatory_for_zoroastrian_identity).
narrative_ontology:cs_axiom_status(endogamy_mandatory_for_zoroastrian_identity, holdable).
narrative_ontology:cs_axiom_grounding('4a897cc8-76f6-41eb-8471-5cd0cbee57a3', endogamy_mandatory_for_zoroastrian_identity, deontological).
narrative_ontology:cs_reference_frame('4a897cc8-76f6-41eb-8471-5cd0cbee57a3', parsi_religious_law_tradition).
narrative_ontology:cs_drift_state('4a897cc8-76f6-41eb-8471-5cd0cbee57a3', contemporary_indian_secular_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('4a897cc8-76f6-41eb-8471-5cd0cbee57a3', '').
narrative_ontology:cs_kernel_id(family_law_authority__parsi_zoroastrian_reading, family_law_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(family_law_authority__parsi_zoroastrian_reading, parsi_laity_endogamous).
narrative_ontology:constraint_beneficiary(family_law_authority__parsi_zoroastrian_reading, parsi_priesthood).
narrative_ontology:constraint_victim(family_law_authority__parsi_zoroastrian_reading, parsi_individuals_intermarriage).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Controls ritual validation of Parsi marriages through sacred ceremonies (Ashirwad); refuses to solemnize intermarriages or recognize them as valid within the community. Derives authority, ritual fees, and institutional role from being the exclusive gatekeepers of matrimonial legitimacy. Their personal and professional identity is fused with the priestly function; exit means abandoning a hereditary religious office.
narrative_ontology:constraint_stakeholder(family_law_authority__parsi_zoroastrian_reading, parsi_priesthood, agenda_setter,
    institutional, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(family_law_authority__parsi_zoroastrian_reading, parsi_priesthood, beneficiary).

% Marries within the community, retains full status, burial rights at Towers of Silence, and inheritance under Parsi personal law. Their children are accepted as Parsi. They experience the constraint as protective of their identity and social network, not as extraction, though they forego broader marital autonomy.
narrative_ontology:constraint_stakeholder(family_law_authority__parsi_zoroastrian_reading, parsi_laity_endogamous, beneficiary,
    moderate, biographical, identity_locked, national).

% Seeks to marry a non-Parsi. Faces excommunication, loss of community status, exclusion from religious rites, and in many cases disinheritance. Can access civil marriage under the Special Marriage Act but at the cost of communal expulsion. Bears the direct cost of the boundary enforcement.
narrative_ontology:constraint_stakeholder(family_law_authority__parsi_zoroastrian_reading, parsi_individuals_intermarriage, payer,
    moderate, biographical, constrained, national).

% Are barred from conversion into Parsi Zoroastrianism in most traditional readings, and their marriage is not recognized by the priesthood. They have no standing in communal governance and no voice in the religious law that governs their partner's status.
narrative_ontology:constraint_stakeholder(family_law_authority__parsi_zoroastrian_reading, non_parsi_spouses, excluded,
    powerless, immediate, trapped, national).

% Adjudicates disputes arising under the Parsi Marriage and Divorce Act 1936 and intervenes when constitutional rights (equality, liberty) are invoked against priestly or panchayat decisions. Does not set religious doctrine but can moderate its civil effects through statutory interpretation.
narrative_ontology:constraint_stakeholder(family_law_authority__parsi_zoroastrian_reading, indian_judiciary, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(family_law_authority__parsi_zoroastrian_reading, parsi_priesthood).
narrative_ontology:fixing_cost_class(family_law_authority__parsi_zoroastrian_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Preserves a micro-minority religious community across generations by enforcing endogamy, maintaining ritual purity, and ensuring that children of accepted unions are socialized within the faith, thereby preventing demographic absorption into the surrounding majority.
% TRANSFER_FUNCTION: Transfers reproductive and marital autonomy from individual Parsis to the communal boundary-maintenance apparatus (priesthood and traditional institutions); individuals surrender the right to marry outside the group in exchange for recognized status, ritual access, and inheritance rights.
% ABSENT_VOICES: Non-Parsi partners have no standing in the religious framework; reformist Parsis who reject endogamy are excommunicated and silenced within communal governance; secular feminists and human-rights lawyers are outside the personal-law conversation unless litigation forces their entry.
% DISAPPEARANCE_RATIONALE: If priestly authority over marriage and the endogamy rule vanished, the community's primary boundary mechanism would dissolve. Intermarriage would no longer trigger expulsion, children of mixed unions would likely be incorporated, and the Parsi population trajectory would shift from managed decline toward assimilation. The social structure would reorganize around voluntary affiliation rather than ascriptive ritual gatekeeping.
% FOUNDING_PROBLEM: Survival of Zoroastrians as a distinct religious community in India after migration from Persia, facing demographic decline, conversion pressures, and the risk of cultural absorption into Hindu and Muslim majorities.
% FOUNDING_PROBLEM_CORROBORATION: Parsi priests and panchayat leaders attest the problem is live, citing low birth rates and emigration. External demographers confirm population decline but dispute that endogamy enforcement is the appropriate or effective response; some external scholars and reformist Parsis argue the constraint accelerates decline by alienating younger members and excluding children of mixed marriages.
narrative_ontology:disappearance_verdict(family_law_authority__parsi_zoroastrian_reading, world_rearranges).
narrative_ontology:founding_problem_status(family_law_authority__parsi_zoroastrian_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(family_law_authority__parsi_zoroastrian_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(family_law_authority__parsi_zoroastrian_reading, 'none', 1).
narrative_ontology:epsilon_provenance(family_law_authority__parsi_zoroastrian_reading, 0.72, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(family_law_authority__parsi_zoroastrian_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(family_law_authority__parsi_zoroastrian_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(family_law_authority__parsi_zoroastrian_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is substantial (0.72) because the constraint systematically denies marital autonomy and community status to a segment of the population. Suppression is high (0.70) because the mechanism requires active enforcement by priestly refusal, communal ostracism, and the threat of excommunication â the constraint would not hold without this active gatekeeping. Theater_ratio rises to 0.52, indicating metric substitution: an increasing share of communal activity is performative boundary-maintenance (rhetoric of purity, public expulsions) rather than functional demographic coordination, as the community continues to shrink despite enforcement. Accessibility_collapse is moderate (0.60): the Special Marriage Act provides a legal alternative, but the social and religious costs mean the alternative is only partially accessible. Resistance is moderate (0.55): reformist legal challenges exist but are countered by strong institutional identity-lock within the priesthood and conservative laity.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter/beneficiary seats experience the constraint as sacred coordination necessary for collective survival; the payer seat experiences it as coercive extraction of personal autonomy. The engine computes this divergence from the structural data â both laity and intermarrying individuals hold moderate power, but their directionality differs sharply because one is inside the boundary and the other is expelled by it.
 *
 * DIRECTIONALITY LOGIC:
 *   The priesthood is structurally near the beneficiary end because the constraint subsidizes their authority, ritual income, and hereditary office; the compliant laity are also near the beneficiary end because the constraint confers recognized status, burial rights, and inheritance. The intermarrying individuals are near the full-target end because they bear the direct cost of expulsion and status loss. Non-Parsi spouses are excluded entirely, bearing costs without stake. Directionality is derived from these structural positions plus exit modulation: the priesthood and compliant laity are identity_locked to the community (low exit mobility), which reinforces their beneficiary direction, while intermarrying individuals face constrained exit with high penalties (high directionality toward target).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem â survival of a micro-minority â may once have justified strict boundary maintenance. However, the temporal measurements show base_extractiveness rising from 0.52 to 0.72 and theater_ratio rising to 0.52 over the interval, suggesting the constraint has drifted from survival coordination toward inertial performance. The classification as tangled_rope prevents mislabeling: it acknowledges the genuine coordination residue (community preservation) while capturing the asymmetric extraction (autonomy denial) and active enforcement (priestly gatekeeping) that now characterize the arrangement.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_position,
    'Does the Parsi Zoroastrian reading of marriage as community-preservation represent a structurally distinct constraint from its sibling readings, or do all religious personal-law readings collapse into a single extraction pattern?',
    'Comparative structural analysis across the five kernel readings to test whether epsilon values, beneficiary/victim structures, and enforcement mechanisms are invariant to theological content or vary by community size and institutional form.',
    'If invariant, the kernel is a single commitment system with cosmetic variation; if variant, each reading is a structurally distinct constraint requiring separate classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_position, conceptual, 'Structural distinctness of this kernel reading versus cosmetic variation').

omega_variable(
    community_survival_vs_extraction,
    'Is endogamous enforcement a genuine coordination necessity for a micro-minority''s survival, or is it a snare that accelerates decline by repelling younger members?',
    'Demographic modeling comparing endogamous communities with open-boundary minorities of similar size; longitudinal survey of Parsi youth exit intentions correlated with enforcement rigidity.',
    'If survival requires the constraint, classification leans toward rope or scaffold; if the constraint undermines survival, it is a snare or piton.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(community_survival_vs_extraction, empirical, 'Whether the constraint coordinates survival or undermines it').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (enforced by priestly authority and community institutions) or internalized (members believe intermarriage is inherently sinful or polluting regardless of enforcement)?',
    'Ethnographic interview and post-exit belief trajectory: do members who leave the community continue to feel guilt or pollution anxiety after the extractive mechanism is removed?',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests â the target carries the suppression with them after exit.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs internalized suppression mechanism').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(family_law_authority__parsi_zoroastrian_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(flpz_tr_t0, family_law_authority__parsi_zoroastrian_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(flpz_tr_t20, family_law_authority__parsi_zoroastrian_reading, theater_ratio, 20, 0.3).
narrative_ontology:measurement(flpz_tr_t40, family_law_authority__parsi_zoroastrian_reading, theater_ratio, 40, 0.36).
narrative_ontology:measurement(flpz_tr_t60, family_law_authority__parsi_zoroastrian_reading, theater_ratio, 60, 0.42).
narrative_ontology:measurement(flpz_tr_t80, family_law_authority__parsi_zoroastrian_reading, theater_ratio, 80, 0.47).
narrative_ontology:measurement(flpz_tr_t100, family_law_authority__parsi_zoroastrian_reading, theater_ratio, 100, 0.52).

% Extraction over time
narrative_ontology:measurement(flpz_be_t0, family_law_authority__parsi_zoroastrian_reading, base_extractiveness, 0, 0.52).
narrative_ontology:measurement(flpz_be_t20, family_law_authority__parsi_zoroastrian_reading, base_extractiveness, 20, 0.58).
narrative_ontology:measurement(flpz_be_t40, family_law_authority__parsi_zoroastrian_reading, base_extractiveness, 40, 0.63).
narrative_ontology:measurement(flpz_be_t60, family_law_authority__parsi_zoroastrian_reading, base_extractiveness, 60, 0.68).
narrative_ontology:measurement(flpz_be_t80, family_law_authority__parsi_zoroastrian_reading, base_extractiveness, 80, 0.7).
narrative_ontology:measurement(flpz_be_t100, family_law_authority__parsi_zoroastrian_reading, base_extractiveness, 100, 0.72).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(family_law_authority__parsi_zoroastrian_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(family_law_authority__parsi_zoroastrian_reading, identity_coordination).
narrative_ontology:affects_constraint(family_law_authority__parsi_zoroastrian_reading, hindu_dharmashastra_reading).
narrative_ontology:affects_constraint(family_law_authority__parsi_zoroastrian_reading, muslim_shariat_reading).
narrative_ontology:affects_constraint(family_law_authority__parsi_zoroastrian_reading, christian_canonical_reading).
narrative_ontology:affects_constraint(family_law_authority__parsi_zoroastrian_reading, secular_contractual_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the family_law_authority kernel, distinguished by its Zoroastrian priestly lineage, endogamy mandate, and micro-minority preservation logic. Sibling readings instantiate structurally distinct constraints under Hindu, Muslim, Christian, and secular frameworks.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
