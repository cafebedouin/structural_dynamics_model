% ============================================================================
% CONSTRAINT STORY: family_law_authority__christian_canonical_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_family_law_authority__christian_canonical_reading, []).

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
 *   constraint_id: family_law_authority__christian_canonical_reading
 *   human_readable: Christian Canonical Marriage Authority (Sacramental Permanence and Ecclesiastical Governance)
 *   domain: religious/legal/social
 *
 * SUMMARY:
 *   This constraint story instantiates the Christian canonical reading of
 *   family law authority, wherein marriage is understood as a sacrament
 *   (Catholic) or covenant (Protestant evangelical) under ecclesiastical
 *   governance. Catholic doctrine holds marriage as permanently indissoluble
 *   unless declared null by a bishop or tribunal; Protestant denominations
 *   vary from strict no-divorce rules to permitting divorce and remarriage on
 *   grounds of adultery or abandonment (and increasingly, on broader
 *   compassionate grounds). The constraint's core claim is that the church,
 *   not the state or the couple, holds ultimate authority over marriage
 *   validity and dissolution. This is one of five readings of the contested
 *   kernel 'family_law_authority'; the others (Hindu dharmashastra, Muslim
 *   shariat, Parsi Zoroastrian, secular contractual) offer competing
 *   authority framings. This reading neither forecloses nor is foreclosed by
 *   the others in jurisdictions with religious pluralism; they coexist as
 *   live options and influence one another through legal accommodation (civil
 *   law recognizing religious marriages, religious law absorbing civil
 *   remedies). The measurement interval 1965–2024 captures post-Vatican II
 *   evolution in Catholic doctrine (increasing pastoral concern for divorce
 *   victims, Pope Francis's reforms to annulment procedures) and Protestant
 *   denominational drift (mainline Protestantism moving toward acceptance of
 *   divorce and remarriage; evangelical Christianity remaining more
 *   conservative). The theatrical ratio has risen (from 0.12 to 0.41) as
 *   actual enforcement has softened while the doctrine's symbolic affirmation
 *   intensifies—more Catholics divorce and remarry civilly (defying the
 *   doctrine) while the church maintains the sacramental no-divorce claim in
 *   official teaching.
 *
 * KEY AGENTS:
 *   - ecclesiastical_institution: Catholic bishops, Protestant denominational hierarchies; set and enforce doctrine; powerful, civilizational time horizon.
 *   - spouses_seeking_dissolution: moderate power, biographical horizon, constrained exit (no remarriage without church permission).
 *   - remarrying_divorced_persons: powerless, trapped exit (spiritual goods versus autonomy trade-off).
 *   - abused_spouses: powerless, identity-locked (religious identity fused with marriage covenant); excluded from proceedings; excluded voice but payer in practice.
 *   - state_civil_authority: parallel institutional seat with no direct authority in ecclesiastical law; operates independently.
 *   - feminist_theologians and divorce-reform advocates: organized seat, constrained exit (voice inside denominational structure but limited power), excluded from canonical proceedings.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(family_law_authority__christian_canonical_reading, 0.62).
domain_priors:suppression_score(family_law_authority__christian_canonical_reading, 0.71).
domain_priors:theater_ratio(family_law_authority__christian_canonical_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(family_law_authority__christian_canonical_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(family_law_authority__christian_canonical_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(family_law_authority__christian_canonical_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(family_law_authority__christian_canonical_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(family_law_authority__christian_canonical_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(family_law_authority__christian_canonical_reading, tangled_rope).
narrative_ontology:human_readable(family_law_authority__christian_canonical_reading, "Christian Canonical Marriage Authority (Sacramental Permanence and Ecclesiastical Governance)").
narrative_ontology:topic_domain(family_law_authority__christian_canonical_reading, "religious/legal/social").

domain_priors:requires_active_enforcement(family_law_authority__christian_canonical_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(family_law_authority__christian_canonical_reading, '46e10867-3354-40f7-9560-43e8a42e88a3').
narrative_ontology:cs_kernel_codification('46e10867-3354-40f7-9560-43e8a42e88a3', fixed_text).
narrative_ontology:cs_authority_grounding('46e10867-3354-40f7-9560-43e8a42e88a3', lineage).
narrative_ontology:cs_interpretation_layer_present('46e10867-3354-40f7-9560-43e8a42e88a3').
narrative_ontology:cs_reading_relation('46e10867-3354-40f7-9560-43e8a42e88a3', family_law_authority__muslim_shariat_reading, coexists_with).
narrative_ontology:cs_reading_relation('46e10867-3354-40f7-9560-43e8a42e88a3', family_law_authority__hindu_dharmashastra_reading, coexists_with).
narrative_ontology:cs_reading_relation('46e10867-3354-40f7-9560-43e8a42e88a3', family_law_authority__parsi_zoroastrian_reading, coexists_with).
narrative_ontology:cs_reading_relation('46e10867-3354-40f7-9560-43e8a42e88a3', family_law_authority__secular_contractual_reading, influences).
narrative_ontology:cs_axiom('46e10867-3354-40f7-9560-43e8a42e88a3', foundational, sacramental_indissolubility).
narrative_ontology:cs_axiom_status(sacramental_indissolubility, holdable).
narrative_ontology:cs_axiom_grounding('46e10867-3354-40f7-9560-43e8a42e88a3', sacramental_indissolubility, deontological).
narrative_ontology:cs_axiom('46e10867-3354-40f7-9560-43e8a42e88a3', foundational, ecclesiastical_marriage_authority).
narrative_ontology:cs_axiom_status(ecclesiastical_marriage_authority, holdable).
narrative_ontology:cs_axiom_grounding('46e10867-3354-40f7-9560-43e8a42e88a3', ecclesiastical_marriage_authority, conventional).
narrative_ontology:cs_reference_frame('46e10867-3354-40f7-9560-43e8a42e88a3', sacramental_permanent_covenant_under_ecclesiastical_governance).
narrative_ontology:cs_drift_state('46e10867-3354-40f7-9560-43e8a42e88a3', contemporary_2024, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('46e10867-3354-40f7-9560-43e8a42e88a3', '').
narrative_ontology:cs_kernel_id(family_law_authority__christian_canonical_reading, family_law_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(family_law_authority__christian_canonical_reading, ecclesiastical_institution).
narrative_ontology:constraint_victim(family_law_authority__christian_canonical_reading, spouses_seeking_dissolution).
narrative_ontology:constraint_victim(family_law_authority__christian_canonical_reading, remarrying_divorced_persons).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(family_law_authority__christian_canonical_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(family_law_authority__christian_canonical_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(family_law_authority__christian_canonical_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(family_law_authority__christian_canonical_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(family_law_authority__christian_canonical_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint is classified as Tangled Rope because it combines genuine coordination (stable, sacrament-blessed families as moral communities) with asymmetric extraction (spouses and divorce-seekers bear the cost of indissolubility while the church collects authority and legitimacy). Active enforcement is required: bishops adjudicate annulment petitions, dioceses deny remarriage blessings, confessional pressure discourages remarriage or cohabitation. Extractiveness has declined from 0.78 to 0.62 over the interval as civil law has decoupled from ecclesiastical authority and most believers in Western contexts now divorce civilly regardless of church teaching. Theater has risen from 0.12 to 0.41 as enforcement softens (fewer annulments denied, pastoral language emphasizes accompaniment rather than judgment) while the doctrine remains officially unchanged—the constraint persists increasingly through symbolic affirmation and community identity rather than coercive enforcement. Suppression has declined (0.85 to 0.71) because alternatives (civil divorce, secular remarriage, switching to denominations that permit divorce) are now accessible and normalized; the suppression that remains is primarily social (shame, exclusion from some community roles) and internalized (guilt, spiritual doubt) rather than structural. Accessibility collapse has remained moderate (0.68) because the civil alternative is fully available; what collapses is not the exit option but the spiritual goods within the church's domain (blessing, Eucharist access, community recognition of second marriage). Resistance has remained stable (0.58) because a substantial population still holds the sacramental doctrine as normatively binding while a growing population treats it as advisory. The claim/metric divergence is intentional: the constraint is CLAIMED as Tangled Rope by the institution (coordination + enforcement), while metrics show increasing theatricality and declining extractiveness—the engine measures this gap as a sign of institutional adaptation to eroding legitimacy.
 *
 * PERSPECTIVAL GAP:
 *   From the ecclesiastical institution's seat, the constraint solves a genuine coordination problem (binding spouses to permanent commitment, grounding family stability in sacramental permanence) and justifies enforcement as protection of the sacrament's integrity. From the spouses-seeking-dissolution and remarrying-divorced-persons seats, the same constraint operates as enforced extraction: their right to exit and remarry is conditioned on the church's permission, creating a monopoly on marriage blessing that extracts authority and obedience. From the abused-spouses seat, the constraint is directly harmful and suppressive, not merely extractive—it traps vulnerable people in unsafe situations and subordinates their welfare to the doctrine's supremacy. From the state's seat, the constraint is institutionally irrelevant in civil law but remains socially consequential in communities where religious authority is strong. The engine computes per-seat directionality: the institution's directionality is near 0.0 (full beneficiary), spouses and remarried persons sit near 0.8–1.0 (full targets), and abused spouses sit at 1.0 (targets with no recognized exit). Contested believers sit near 0.5 (symmetric: genuine coordination benefit, diffuse cost of enforced permanence). This divergence is the measurement the constraint system exists to detect.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary: ecclesiastical_institution. The church sets the rules, controls adjudication, collects legitimate authority and material resources (annulment fees, tithing pressure from those seeking church blessing despite civil remarriage). Victims: spouses_seeking_dissolution and remarrying_divorced_persons, who bear the cost of constrained exit and spiritual-goods rationing. Secondary target: abused_spouses, whose situation is made worse by indissolubility doctrine. Secondary beneficiary: faithful_believers whose marriages are stable and who benefit from the sacramental permanence doctrine as a spiritual good and commitment-reinforcer. The directionality derivation from beneficiary/victim declarations plus exit options flows naturally: the institution holds institutional power and arbitrage exit (can change doctrine whenever it chooses), moving d toward 0.0. Spouses hold moderate power, constrained or trapped exit, moving d toward 1.0. Abused spouses hold powerless status, identity-locked exit (religious identity fused with marriage covenant makes leaving costly even when safe exit is available civilly), moving d to full-target 1.0. No overrides are needed; the structural data produces appropriate directionalities.
 *
 * MANDATROPHY ANALYSIS:
 *   Mandatrophy is present and well-documented. The founding problem (family instability, need for moral formation through permanent covenant) was genuine and live in 1965 when Vatican II was promulgating the constraint's strengthened form. By 1995, civil law had universally decoupled from ecclesiastical authority; state-provided family stability through divorce law, child support, and property division; and therapeutic practice had superseded ecclesiastical authority in relationship counseling. The founding problem is now dead in pluralistic jurisdictions: civil law solves family stability without the church. Yet the constraint persists, increasingly through theater and symbolic affirmation rather than coercive enforcement. The measurement series shows this: base_extractiveness declines (fewer people comply with no-divorce because civil alternatives work), theater_ratio rises (the doctrine becomes more symbolic, less enforceable), suppression_requirement declines (alternatives are accessible, suppression harder to maintain). The constraint has become a Piton—an atrophied coordination mechanism maintained theatrically because the institution benefits from it (moral authority, legitimacy claim as family guardian) and no single power can change it (hierarchical authority structures resist doctrinal revision, and reformers lack coercive power). The mandatrophy resolution is partial: Pope Francis's 2015 reforms to annulment procedures (streamlining access) acknowledged the mandate's obsolescence and reduced enforcement friction, but without formally revising the no-divorce doctrine (which would require ecumenical consensus and doctrinal revision that the institution resists). This is classic mandate drift under institutional inertia.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sacramental_doctrine_vs_pastoral_accommodation,
    'Is the ecclesiastical doctrine genuinely committed to sacramental indissolubility, or is it increasingly a performance that accommodates pastoral reality (de facto acceptance of remarriage through annulment expansion and divorced-Catholic inclusion) while maintaining official doctrine?',
    'Comparison of official doctrine statements with actual annulment rates, pastoral guidance to divorced believers, and Vatican policy changes over time. If annulment rates rise to track civil divorce rates, and papal statements emphasize ''pastoral accompaniment'' of divorced believers, the doctrine may have functionally shifted while remaining verbally unchanged.',
    'If the constraint is functionally a Piton (maintained theatrically), the classification should flag degraded coordination and inertial persistence rather than active enforcement of a live norm. If the doctrine remains genuinely operative, the suppression and extraction scores should rise, not decline.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sacramental_doctrine_vs_pastoral_accommodation, empirical, 'Whether ecclesiastical doctrine on indissolubility is genuinely operative or performatively maintained.').

omega_variable(
    internalized_suppression_mechanism,
    'Post-exit suppression trajectory: Do spouses who leave the church''s authority structure (civil divorce, conversion to a permissive denomination, or secularization) experience suppression trajectory change? If suppression persists after the ecclesiastical mechanism is removed, what portion is internalized (fused with religious identity, guilt, family/community rupture) versus structural (legal barriers, community exclusion)?',
    'Qualitative research with divorced Catholics and post-Catholic/post-religious respondents; measurement of shame, guilt, and social rupture at intervals after exit; comparison of suppression levels among those who maintain faith identity versus those who secularize.',
    'If suppression is substantially internalized, the effective suppression on trapped targets (abused spouses who remain in the faith community) is higher than the measured structural suppression suggests. This would raise the severity classification and support reclassification of the constraint as more extractive for identity-locked targets.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(internalized_suppression_mechanism, empirical, 'Structural versus internalized suppression in Christian marriage doctrine.').

omega_variable(
    kernel_reading_variance_in,
    'The ε value for THIS reading (Christian canonical, sacramental permanence, church authority) is 0.62. How do the sibling readings differ in ε? Does Muslim shariat (contract-based, unilateral talaq dissolution rights for men) have lower ε (more accessible exit, less extraction)? Does Hindu dharmashastra (conjugal duty without sacramental permanence, family authority over individual will) have higher ε (more extraction, more internalized suppression)? Does secular contractual (no-fault divorce, no religious authority) have lower ε?',
    'Author separate constraint stories for each sibling reading, measuring each with its own ε, beneficiary/victim structure, and enforcement model. Compare the ε values across readings to establish how much of the measured extraction depends on the Christian sacramental framing versus on family law authority per se.',
    'If secular_contractual has substantially lower ε, the extraction is specifically attributable to religious authority and sacramental doctrine, not to family law structure. If muslim_shariat shows high ε (high extraction of men''s unilateral talaq rights, low exit for women), the problem is not religious authority per se but gendered implementation. The kernel contest requires per-reading measurement, not merged analysis.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_variance_in, empirical, 'Cross-reading ε variance in family law authority kernel.').

omega_variable(
    church_authority_authenticity,
    'Does ecclesiastical authority to set marriage doctrine rest on theologically genuine claims (divine revelation, apostolic succession, living tradition) or on institutional power and historical inertia? Is the doctrine''s legitimacy dependent on believers'' faith in its authenticity, or does it persist through social pressure and institutional momentum regardless of belief?',
    'Sociological research on believers'' actual stance toward ecclesiastical authority: do they regard it as divinely grounded, or as institutional convenience they accept out of community loyalty despite doubting its foundation? Comparison of doctrine compliance in high-faith-belief communities versus nominal-believer communities.',
    'If authority depends on authentic faith belief, suppression and theater scores should correlate inversely with faith-belief measures. If authority persists through institutional inertia despite erosion of belief, theater should remain high while effective belief declines. The measurement series showing rising theater amid declining extractiveness supports the inertia hypothesis.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(church_authority_authenticity, preference, 'Whether ecclesiastical authority''s legitimacy rests on authentic faith or institutional momentum.').

omega_variable(
    kernel_reading_identity_lock_variance,
    'Identity-lock differs across the sibling readings. For Christian canonical (this reading), religious identity is fused with the denomination and its doctrines; exit is costly because it means leaving the faith community. For secular contractual, identity-lock is absent (exiting a marriage does not threaten identity). For Hindu dharmashastra, identity-lock may be higher (family caste and kinship identity is constitutive). Does the identity-lock variance explain differential suppression across readings?',
    'Measure identity-lock intensity (how completely individual identity is fused with the marriage/religious framework) for each reading. Correlate with suppression scores and exit costs. If identity-lock correlates with higher effective suppression on trapped targets, the readings differ not in their formal authority claims but in how deeply they fuse with agent identity.',
    'A high-identity-lock reading (Christian canonical, Hindu dharmashastra) may show higher effective suppression on targets even when formal exit is legally available, because psychological/identity exit is costly. The sibling readings should be compared on this dimension to understand how much extraction is formal (authority claim) versus internalized (identity fusion).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_identity_lock_variance, empirical, 'Identity-lock variance in family law authority kernel readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(family_law_authority__christian_canonical_reading, 1965, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fami_tr_t1965, family_law_authority__christian_canonical_reading, theater_ratio, 1965, 0.12).
narrative_ontology:measurement(fami_tr_t1980, family_law_authority__christian_canonical_reading, theater_ratio, 1980, 0.19).
narrative_ontology:measurement(fami_tr_t1995, family_law_authority__christian_canonical_reading, theater_ratio, 1995, 0.28).
narrative_ontology:measurement(fami_tr_t2010, family_law_authority__christian_canonical_reading, theater_ratio, 2010, 0.36).
narrative_ontology:measurement(fami_tr_t2024, family_law_authority__christian_canonical_reading, theater_ratio, 2024, 0.41).

% Extraction over time
narrative_ontology:measurement(fami_be_t1965, family_law_authority__christian_canonical_reading, base_extractiveness, 1965, 0.78).
narrative_ontology:measurement(fami_be_t1980, family_law_authority__christian_canonical_reading, base_extractiveness, 1980, 0.72).
narrative_ontology:measurement(fami_be_t1995, family_law_authority__christian_canonical_reading, base_extractiveness, 1995, 0.68).
narrative_ontology:measurement(fami_be_t2010, family_law_authority__christian_canonical_reading, base_extractiveness, 2010, 0.65).
narrative_ontology:measurement(fami_be_t2024, family_law_authority__christian_canonical_reading, base_extractiveness, 2024, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(fami_su_t1965, family_law_authority__christian_canonical_reading, suppression_requirement, 1965, 0.85).
narrative_ontology:measurement(fami_su_t1980, family_law_authority__christian_canonical_reading, suppression_requirement, 1980, 0.8).
narrative_ontology:measurement(fami_su_t1995, family_law_authority__christian_canonical_reading, suppression_requirement, 1995, 0.75).
narrative_ontology:measurement(fami_su_t2010, family_law_authority__christian_canonical_reading, suppression_requirement, 2010, 0.73).
narrative_ontology:measurement(fami_su_t2024, family_law_authority__christian_canonical_reading, suppression_requirement, 2024, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(family_law_authority__christian_canonical_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(family_law_authority__christian_canonical_reading, 0.12).
narrative_ontology:affects_constraint(family_law_authority__christian_canonical_reading, family_law_authority__muslim_shariat_reading).
narrative_ontology:affects_constraint(family_law_authority__christian_canonical_reading, family_law_authority__hindu_dharmashastra_reading).
narrative_ontology:affects_constraint(family_law_authority__christian_canonical_reading, family_law_authority__parsi_zoroastrian_reading).
narrative_ontology:affects_constraint(family_law_authority__christian_canonical_reading, family_law_authority__secular_contractual_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the contested kernel 'family_law_authority.' The kernel unifies five structurally distinct readings, each with different ε, beneficiary/victim structures, and authority axioms. The readings coexist as live options in pluralistic jurisdictions and influence one another through legal accommodation (civil law recognizing religious marriages, religious law absorbing civil remedies). All five readings should be authored as separate constraint stories linked via this network field. The Christian canonical reading emphasizes sacramental permanence and ecclesiastical governance; the Muslim shariat reading emphasizes contractual recision and gendered dissolution rights; Hindu dharmashastra emphasizes conjugal duty and family authority; Parsi Zoroastrianism emphasizes community preservation; secular contractual emphasizes individual autonomy and state authority. Each reading has different ε-invariant properties and different power/exit distributions. Do not attempt to merge them into one constraint—they are genuinely different constraints that share a kernel authority claim.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
