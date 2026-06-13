% ============================================================================
% CONSTRAINT STORY: marriage_authority__gender_rights_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_marriage_authority__gender_rights_reading, []).

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
    constraint_indexing:directionality_override/3,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: marriage_authority__gender_rights_reading
 *   human_readable: Patriarchal Personal Law Extraction via Community Authority — Gender Rights Reading
 *   domain: legal/constitutional/family_law
 *
 * SUMMARY:
 *   This constraint instantiates the GENDER RIGHTS READING of the contested
 *   marriage_authority kernel. Under this reading, personal law pluralism
 *   functions as a snare: it is presented as protecting religious minorities
 *   from majoritarian suppression, but its actual operation extracts women's
 *   rights to equality and property by shielding patriarchal family law rules
 *   from constitutional review. The constraint is not the pluralism itself
 *   (which could be a legitimate coordination mechanism), but rather the
 *   exemption of gender-discriminatory practices within personal law from
 *   constitutional equality guarantees. This reading cross-cuts the
 *   communal/secular divide: it accepts community autonomy in family law IF
 *   that autonomy does not depend on gender discrimination. It forecloses the
 *   pure communal-autonomy reading by asserting that constitutional rights
 *   override community tradition, but coexists with the
 *   judicial-harmonization reading (both accept case-by-case constitutional
 *   review) and influences the secularist reading by accepting pluralism in
 *   exchange for gender-floor enforcement.
 *
 * KEY AGENTS:
 *   - women_within_patriarchal_personal_law: victims of unequal divorce, property, and inheritance rights; trapped by identity-lock (marriage dissolves identity, family ostracism, economic dependence)
 *   - patriarchal_authority_holders: agenda-setters administering personal law; beneficiaries of state deference to their adjudication; actively enforce constraint by excluding rival interpretations
 *   - state_apparatus_deferring_enforcement: institutional beneficiary; avoids political cost of confronting religious authority by granting quasi-judicial power to personal law authorities
 *   - women_rights_advocates: excluded from authority structure; work through litigation and constitutional challenge; their inclusion would reframe authority from communal-autonomy to gender-equality grounds
 *   - constitutional_court: observer seat; possesses authority to impose constitutional floors on personal law practices via case-by-case review; critical to this reading's realization
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(marriage_authority__gender_rights_reading, 0.78).
domain_priors:suppression_score(marriage_authority__gender_rights_reading, 0.71).
domain_priors:theater_ratio(marriage_authority__gender_rights_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(marriage_authority__gender_rights_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(marriage_authority__gender_rights_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(marriage_authority__gender_rights_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(marriage_authority__gender_rights_reading, accessibility_collapse, 0.63).
narrative_ontology:constraint_metric(marriage_authority__gender_rights_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(marriage_authority__gender_rights_reading, snare).
narrative_ontology:human_readable(marriage_authority__gender_rights_reading, "Patriarchal Personal Law Extraction via Community Authority — Gender Rights Reading").
narrative_ontology:topic_domain(marriage_authority__gender_rights_reading, "legal/constitutional/family_law").

domain_priors:requires_active_enforcement(marriage_authority__gender_rights_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(marriage_authority__gender_rights_reading, '6e29d502-0829-4146-a3c4-95d311188167').
narrative_ontology:cs_kernel_codification('6e29d502-0829-4146-a3c4-95d311188167', fixed_text).
narrative_ontology:cs_authority_grounding('6e29d502-0829-4146-a3c4-95d311188167', extraction).
narrative_ontology:cs_interpretation_layer_present('6e29d502-0829-4146-a3c4-95d311188167').
narrative_ontology:cs_reading_relation('6e29d502-0829-4146-a3c4-95d311188167', marriage_authority__communal_autonomy_reading, forecloses).
narrative_ontology:cs_reading_relation('6e29d502-0829-4146-a3c4-95d311188167', marriage_authority__federalist_millet_reading, forecloses).
narrative_ontology:cs_reading_relation('6e29d502-0829-4146-a3c4-95d311188167', marriage_authority__secularist_reading, influences).
narrative_ontology:cs_reading_relation('6e29d502-0829-4146-a3c4-95d311188167', marriage_authority__judicial_harmonization_reading, coexists_with).
narrative_ontology:cs_axiom('6e29d502-0829-4146-a3c4-95d311188167', foundational, constitutional_equality_supremacy_in_family_law).
narrative_ontology:cs_axiom_status(constitutional_equality_supremacy_in_family_law, holdable).
narrative_ontology:cs_axiom_grounding('6e29d502-0829-4146-a3c4-95d311188167', constitutional_equality_supremacy_in_family_law, deontological).
narrative_ontology:cs_axiom('6e29d502-0829-4146-a3c4-95d311188167', foundational, women_bearers_of_fundamental_rights_status).
narrative_ontology:cs_axiom_status(women_bearers_of_fundamental_rights_status, holdable).
narrative_ontology:cs_axiom_grounding('6e29d502-0829-4146-a3c4-95d311188167', women_bearers_of_fundamental_rights_status, deontological).
narrative_ontology:cs_reference_frame('6e29d502-0829-4146-a3c4-95d311188167', religious_community_autonomy_in_family_law).
narrative_ontology:cs_drift_state('6e29d502-0829-4146-a3c4-95d311188167', contemporary_constitutional_equality_jurisprudence, gap(axiom_overriding, substantial, true)).
narrative_ontology:cs_created_at('6e29d502-0829-4146-a3c4-95d311188167', '').
narrative_ontology:cs_kernel_id(marriage_authority__gender_rights_reading, marriage_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(marriage_authority__gender_rights_reading, patriarchal_authority_holders).
narrative_ontology:constraint_beneficiary(marriage_authority__gender_rights_reading, state_apparatus_deferring_enforcement).
narrative_ontology:constraint_victim(marriage_authority__gender_rights_reading, women_within_patriarchal_personal_law).
narrative_ontology:constraint_victim(marriage_authority__gender_rights_reading, gender_equality_advocates).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(marriage_authority__gender_rights_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(marriage_authority__gender_rights_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(marriage_authority__gender_rights_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(marriage_authority__gender_rights_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(marriage_authority__gender_rights_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high and rising (0.58→0.78 over interval) because the constraint systematically transfers women's property and divorce rights to patriarchal authorities, and as women's constitutional claims gain traction, the extraction becomes more visible (the gap between claimed pluralism-protection and actual gender-subordination widens, raising the measured extraction of the constraint). Suppression is substantial (0.71) because the constraint persists through exclusion of women from authority structures, identity-lock (exit means social annihilation), and state refusal to hear women's equality claims. Theater is moderate (0.42) because the constraint is justified as protecting community autonomy (real coordination function), but a growing share of enforcement effort defends gender-discriminatory practices specifically rather than community autonomy generally. Accessibility collapse is moderate (0.63) because alternatives do exist (constitutional litigation, reformed personal law codes, secular arbitration outside state enforcement) but are costly and require exit from community identity. Resistance is high (0.72) because women's advocates, constitutional courts, and international human rights bodies increasingly challenge the constraint; the measurement series shows rising extraction and suppression as resistance accumulates, indicating the constraint is becoming harder to maintain and its extractive character more visible.
 *
 * PERSPECTIVAL GAP:
 *   From the patriarchal_authority_holders' and state_apparatus' seats, the constraint is a necessary compromise: pluralism protects minorities and avoids state overreach. From women_within_patriarchal_personal_law and women_rights_advocates' seats, the same structure is gender-discriminatory subordination dressed as cultural respect. The constraint's type diverges across seats: the state and patriarchal authorities would classify it as rope (genuine coordination of diversity), while the victims and advocates would classify it as snare (coordinated gender extraction). This reading takes the advocates' classification as structurally true: the constraint persists precisely because it extracts women's rights while maintaining the pluralism cover story. The divergence is not a defect — it is the measurement the framework exists to take.
 *
 * DIRECTIONALITY LOGIC:
 *   Women_within_patriarchal_personal_law sit at the target end (d near 1.0): they are trapped (identity_locked exit), powerless in the authority structure, and the constraint extracts their property and autonomy. Patriarchal_authority_holders sit at the beneficiary end (d near 0.0): they are agenda-setters, organized, mobile in exit, and they collect the authority and deference the constraint grants. State_apparatus_deferring_enforcement is symmetric-to-moderate beneficiary (d~0.2-0.3): they benefit by avoiding political cost, but they are constrained by constitutional obligations and international pressure, so the benefit is not costless. Women_rights_advocates are moderately targeted (d~0.7): they are excluded from authority, mobile in exit but constrained by commitment to affected women, and they bear costs of litigation and advocacy. Constitutional_court is analytical (d=0.5): they are positioned to reframe the constraint entirely but are themselves contested (some seats defend pluralism, others prioritize equality). Religious_authority_institutions are beneficiaries (d near 0.0): they benefit from state deference and control over family adjudication, though they are powerful enough to have exit options (extra-legal arbitration).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (protecting religious minorities from majoritarian suppression) was live at independence but is now substantially dead in most contemporary contexts: modern secular states do not threaten religious autonomy, and personal law pluralism persists not as anti-majoritarian protection but as shield for patriarchal authority holders. The constraint thus exhibits classic mandatrophy: the functional justification has atrophied, but the institutional arrangement persists through inertia and the concentrated interests of patriarchal authorities. However, this reading does NOT resolve mandatrophy by declaring the constraint a piton. Instead, it reframes the founding problem: the constraint is not dead, but repurposed. The NEW founding problem is gender equality, and the constraint now functions as systematic opposition to that goal. The state's mandate has inverted: from protecting religious autonomy to enforcing gender equality. The constraint persists because patriarchal authorities actively resist the new mandate, not because enforcement atrophied. This is a snare with rising theater as the contradiction between pluralism-justification and gender-extraction becomes visible.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    community_autonomy_vs_gender_equality_incommensurability,
    'Is the conflict between patriarchal community autonomy and gender equality fundamentally unresolvable, or can reformed personal law codes preserve community identity while accepting gender-egalitarian rules?',
    'Jurisdictional comparison: examine cases where communities have reformed their personal law codes to accept gender equality (property rights, divorce parity, inheritance) while maintaining religious identity markers. Document whether such reform is structural necessity or contingent historical choice.',
    'If reform is possible, the constraint is contingent (snare, remediable via reformed authority structure); if resolution is impossible, gender equality would require elimination of pluralism, supporting the secularist reading. The reading''s own thesis depends on this being resolvable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(community_autonomy_vs_gender_equality_incommensurability, conceptual, 'Whether patriarchal authority and gender equality can coexist in reformed personal law or are structurally incompatible.').

omega_variable(
    identity_lock_mechanism_in_gender_rights_reading,
    'Is the measured identity-lock of women_within_patriarchal_personal_law structural (economic dependence, lack of alternative legal status) or internalized (belief in family obligation, religious conviction that divorce is sin)?',
    'Post-exit trajectory studies: examine women who exit the personal law system (through conversion, migration, or court-granted alternative status) and measure suppression persistence. If suppression persists after exit, the identity-lock is partially internalized.',
    'If structural: reform via legal alternatives and economic support is sufficient. If internalized: the constraint''s suppression is higher than structural measures suggest; women carry the constraint with them after exit. Affects remediation strategy and measurement of effective suppression.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_mechanism_in_gender_rights_reading, empirical, 'Whether women''s exit options are structurally blocked or psychologically internalized.').

omega_variable(
    state_deferral_as_deliberate_vs_path_dependent,
    'Is the state''s deferral to personal law authorities a deliberate contemporary choice to respect pluralism, or a path-dependent institutional inertia from colonial legal pluralism that contemporary actors maintain from habit?',
    'Legislative history and judicial reasoning: examine whether contemporary state actors defend pluralism as affirmative value or justify it as unavoidable default. Compare with counterfactual scenarios where states actively reformed personal law (which have occurred in some jurisdictions).',
    'If deliberate: the constraint has a live political constituency defending pluralism; reform requires ideological contestation. If path-dependent: the constraint persists despite lack of affirmative defense; reform requires only institutional innovation and coalition-building. Affects the visibility of patriarchal interests as opposed to inertial institutional arrangements.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(state_deferral_as_deliberate_vs_path_dependent, empirical, 'Whether state deferral reflects contemporary normative commitment or institutional inertia.').

omega_variable(
    constitutional_court_authority_vs_legitimacy,
    'In this reading, does constitutional court authority to impose gender-equality floors derive from democratic legitimacy (representing the constitutional will of the people) or from judicial override of legislative/community choice?',
    'Examine constitutional text and founding principles: does the constitution explicitly authorize equal protection review of personal law, or is the court reading such authority into ambiguous texts? Compare with constitutional courts that have deferred to personal law autonomy.',
    'If democratic: the court is enforcing the people''s own constitutional commitment to equality, and state deferral to patriarchal authorities is unconstitutional. If judicial override: the court is imposing gender equality against the people''s pluralistic choice, which threatens the legitimacy of the reading in the eyes of patriarchal and communal-autonomy constituencies. Affects whether this reading is constitutionally grounded or represents an ambitious reinterpretation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(constitutional_court_authority_vs_legitimacy, conceptual, 'Whether constitutional equality review of personal law rests on democratic authorization or judicial overreach.').

omega_variable(
    kernel_reading_contest_representation,
    'Is this reading (gender_rights_reading) one seat in a contested kernel, or does the accumulation of constitutional litigation and women''s advocacy represent a reframe that is displacing the original communal_autonomy_reading as the operative authority grounding?',
    'Temporal analysis of authority structure change: document which seats control marriage adjudication authority (religious councils, community courts, constitutional courts) over time. If constitutional courts are increasingly being appealed to and overriding personal law authorities, the reading is not merely one seat but a reframe in progress.',
    'If the reading is reframing: it is not coexisting with communal-autonomy as a stable kernel, but is replacing it; the kernel contest is not plural equilibrium but historical transition. This affects the engine''s terminal-attractor computation and whether the constraint is stable (contested kernel with multiple live readings) or unstable (reading in displacement).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest_representation, empirical, 'Whether this reading represents one stable seat in a contested kernel or an ascending reading that is reframing authority.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(marriage_authority__gender_rights_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(marr_tr_t0, marriage_authority__gender_rights_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement(marr_tr_t5, marriage_authority__gender_rights_reading, theater_ratio, 5, 0.32).
narrative_ontology:measurement(marr_tr_t10, marriage_authority__gender_rights_reading, theater_ratio, 10, 0.36).
narrative_ontology:measurement(marr_tr_t15, marriage_authority__gender_rights_reading, theater_ratio, 15, 0.39).
narrative_ontology:measurement(marr_tr_t25, marriage_authority__gender_rights_reading, theater_ratio, 25, 0.42).
narrative_ontology:measurement(marr_tr_t40, marriage_authority__gender_rights_reading, theater_ratio, 40, 0.42).

% Extraction over time
narrative_ontology:measurement(marr_be_t0, marriage_authority__gender_rights_reading, base_extractiveness, 0, 0.58).
narrative_ontology:measurement(marr_be_t5, marriage_authority__gender_rights_reading, base_extractiveness, 5, 0.62).
narrative_ontology:measurement(marr_be_t10, marriage_authority__gender_rights_reading, base_extractiveness, 10, 0.68).
narrative_ontology:measurement(marr_be_t15, marriage_authority__gender_rights_reading, base_extractiveness, 15, 0.72).
narrative_ontology:measurement(marr_be_t25, marriage_authority__gender_rights_reading, base_extractiveness, 25, 0.76).
narrative_ontology:measurement(marr_be_t40, marriage_authority__gender_rights_reading, base_extractiveness, 40, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(marr_su_t0, marriage_authority__gender_rights_reading, suppression_requirement, 0, 0.52).
narrative_ontology:measurement(marr_su_t5, marriage_authority__gender_rights_reading, suppression_requirement, 5, 0.58).
narrative_ontology:measurement(marr_su_t10, marriage_authority__gender_rights_reading, suppression_requirement, 10, 0.63).
narrative_ontology:measurement(marr_su_t15, marriage_authority__gender_rights_reading, suppression_requirement, 15, 0.67).
narrative_ontology:measurement(marr_su_t25, marriage_authority__gender_rights_reading, suppression_requirement, 25, 0.7).
narrative_ontology:measurement(marr_su_t40, marriage_authority__gender_rights_reading, suppression_requirement, 40, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(marriage_authority__gender_rights_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(marriage_authority__gender_rights_reading, 0.12).
narrative_ontology:affects_constraint(marriage_authority__gender_rights_reading, marriage_authority__communal_autonomy_reading).
narrative_ontology:affects_constraint(marriage_authority__gender_rights_reading, marriage_authority__secularist_reading).
narrative_ontology:affects_constraint(marriage_authority__gender_rights_reading, marriage_authority__judicial_harmonization_reading).
narrative_ontology:affects_constraint(marriage_authority__gender_rights_reading, marriage_authority__federalist_millet_reading).

% DUAL FORMULATION NOTE:
% This constraint is part of the marriage_authority kernel family. All readings share the same kernel (persisting commitment to regulate marriage through personal law codes), but differ in authority grounding and structural relationship to gender discrimination. The gender_rights_reading instantiates ε-high snare because it asserts constitutional equality overrides community tradition; the communal_autonomy_reading instantiates ε-low rope because it asserts community tradition overrides state imposition. These are not the same constraint viewed from different angles — they are genuinely different constraints with different ε values, beneficiary/victim structures, and terminal attractors, all reading the same kernel. Link them via affects_constraints to enable cross-reading contamination analysis.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(marriage_authority__gender_rights_reading, institutional, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
