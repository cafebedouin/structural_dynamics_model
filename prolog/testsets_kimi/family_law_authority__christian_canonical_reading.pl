% ============================================================================
% CONSTRAINT STORY: family_law_authority__christian_canonical_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: family_law_authority__christian_canonical_reading
 *   human_readable: Christian Canonical Marriage Authority (Sacramental/Denominational)
 *   domain: religious_governance/comparative_law
 *
 * SUMMARY:
 *   This constraint story captures the Christian canonical reading of the
 *   family_law_authority kernel, under which marriage is governed as
 *   sacrament (Catholic) or denominational ordinance (Protestant) by
 *   ecclesiastical authority. The Catholic magisterium claims exclusive
 *   jurisdiction over validity and enforces sacramental indissolubility,
 *   while Protestant assemblies exercise denominational governance that
 *   permits variance in divorce and remarriage. Both accumulate authority by
 *   controlling the boundary of legitimate Christian union, but the Catholic
 *   form generates substantially higher extraction through identity-locked
 *   exit and denial of dissolution.
 *
 * KEY AGENTS:
 *   - catholic_magisterium: Primary agenda-setter and beneficiary (institutional/universal) â accumulates sacramental jurisdiction and obedience
 *   - protestant_denomination_assemblies: Secondary agenda-setter and beneficiary (organized/national) â exercises governance with lighter extraction
 *   - catholic_laity_seeking_dissolution: Primary target (moderate/identity_locked) â bears the cost of indissolubility
 *   - partners_in_invalid_unions: Secondary target (powerless/constrained) â denied sacramental standing and community legitimacy
 *   - protestant_congregants: Intermediate seat (moderate/constrained) â submits to governance but retains more exit
 *   - civil_state_marriage_regime: Excluded observer (institutional/analytical) â parallel jurisdiction rejected by the canonical framework
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(family_law_authority__christian_canonical_reading, 0.62).
domain_priors:suppression_score(family_law_authority__christian_canonical_reading, 0.7).
domain_priors:theater_ratio(family_law_authority__christian_canonical_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(family_law_authority__christian_canonical_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(family_law_authority__christian_canonical_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(family_law_authority__christian_canonical_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(family_law_authority__christian_canonical_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(family_law_authority__christian_canonical_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(family_law_authority__christian_canonical_reading, tangled_rope).
narrative_ontology:human_readable(family_law_authority__christian_canonical_reading, "Christian Canonical Marriage Authority (Sacramental/Denominational)").
narrative_ontology:topic_domain(family_law_authority__christian_canonical_reading, "religious_governance/comparative_law").

domain_priors:requires_active_enforcement(family_law_authority__christian_canonical_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(family_law_authority__christian_canonical_reading, '47e7b903-75dc-4214-860d-3bf4671880ec').
narrative_ontology:cs_kernel_codification('47e7b903-75dc-4214-860d-3bf4671880ec', formalized).
narrative_ontology:cs_authority_grounding('47e7b903-75dc-4214-860d-3bf4671880ec', lineage).
narrative_ontology:cs_interpretation_layer_present('47e7b903-75dc-4214-860d-3bf4671880ec').
narrative_ontology:cs_reading_relation('47e7b903-75dc-4214-860d-3bf4671880ec', family_law_authority__hindu_dharmashastra_reading, coexists_with).
narrative_ontology:cs_reading_relation('47e7b903-75dc-4214-860d-3bf4671880ec', family_law_authority__muslim_shariat_reading, coexists_with).
narrative_ontology:cs_reading_relation('47e7b903-75dc-4214-860d-3bf4671880ec', family_law_authority__parsi_zoroastrian_reading, coexists_with).
narrative_ontology:cs_reading_relation('47e7b903-75dc-4214-860d-3bf4671880ec', family_law_authority__secular_contractual_reading, influences).
narrative_ontology:cs_axiom('47e7b903-75dc-4214-860d-3bf4671880ec', foundational, sacramental_indissolubility).
narrative_ontology:cs_axiom_status(sacramental_indissolubility, holdable).
narrative_ontology:cs_axiom_grounding('47e7b903-75dc-4214-860d-3bf4671880ec', sacramental_indissolubility, theological).
narrative_ontology:cs_axiom('47e7b903-75dc-4214-860d-3bf4671880ec', foundational, ecclesiastical_validity_jurisdiction).
narrative_ontology:cs_axiom_status(ecclesiastical_validity_jurisdiction, holdable).
narrative_ontology:cs_axiom_grounding('47e7b903-75dc-4214-860d-3bf4671880ec', ecclesiastical_validity_jurisdiction, conventional).
narrative_ontology:cs_reference_frame('47e7b903-75dc-4214-860d-3bf4671880ec', sacramental_permanence_framework).
narrative_ontology:cs_drift_state('47e7b903-75dc-4214-860d-3bf4671880ec', contemporary_secular_pluralism, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('47e7b903-75dc-4214-860d-3bf4671880ec', '').
narrative_ontology:cs_kernel_id(family_law_authority__christian_canonical_reading, family_law_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(family_law_authority__christian_canonical_reading, catholic_magisterium).
narrative_ontology:constraint_beneficiary(family_law_authority__christian_canonical_reading, protestant_denomination_assemblies).
narrative_ontology:constraint_victim(family_law_authority__christian_canonical_reading, catholic_laity_seeking_dissolution).
narrative_ontology:constraint_victim(family_law_authority__christian_canonical_reading, partners_in_invalid_unions).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(family_law_authority__christian_canonical_reading, protestant_congregants).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Claims sole divine authority to determine sacramental marital validity through canon law and canonical courts; enforces indissolubility by denying civil divorce any effect on the sacramental bond; regulates nullity and legitimacy; accumulates institutional obedience and jurisdictional control over intimate life.
narrative_ontology:constraint_stakeholder(family_law_authority__christian_canonical_reading, catholic_magisterium, agenda_setter,
    institutional, generational, analytical, universal).

% Exercise denominational governance over marriage permissions, divorce grounds, and remarriage authorization under scriptural supervision; retain authority to declare marital validity within the denomination while permitting greater congregational exit than Catholic indissolubility.
narrative_ontology:constraint_stakeholder(family_law_authority__christian_canonical_reading, protestant_denomination_assemblies, agenda_setter,
    organized, generational, analytical, national).

% Bound by sacramental indissolubility; civil divorce does not dissolve the canonical bond or permit sacramental remarriage; must pursue canonical annulment through ecclesiastical courts to regain full communion; exiting the constraint requires leaving Catholic identity and community standing.
narrative_ontology:constraint_stakeholder(family_law_authority__christian_canonical_reading, catholic_laity_seeking_dissolution, payer,
    moderate, biographical, identity_locked, national).

% Live in unions deemed canonically invalid due to lack of form, impediment, or prior bond; denied sacramental marriage status, communion access, and canonical legitimacy for offspring; subject to rulings of ecclesiastical courts without reciprocal authority to appeal to civil marriage law within the sacramental framework.
narrative_ontology:constraint_stakeholder(family_law_authority__christian_canonical_reading, partners_in_invalid_unions, payer,
    powerless, biographical, constrained, local).

% Submit to denominational authority over marriage and divorce but operate under governance that permits dissolution and remarriage under denominational supervision; bear the cost of ecclesiastical oversight while retaining more exit options than Catholic laity.
narrative_ontology:constraint_stakeholder(family_law_authority__christian_canonical_reading, protestant_congregants, payer,
    moderate, biographical, constrained, regional).

% Claims parallel jurisdiction over marriage formation and dissolution under civil law; its authority is explicitly subordinated or rejected by Catholic canonical supremacy claims, though Protestant denominations often accommodate it; excluded from the canonical framework's self-understanding of valid marriage.
narrative_ontology:constraint_stakeholder(family_law_authority__christian_canonical_reading, civil_state_marriage_regime, excluded,
    institutional, generational, analytical, national).

narrative_ontology:fixing_cost_class(family_law_authority__christian_canonical_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Orders sexual intimacy, child-rearing, inheritance, and lineage under a publicly recognized, transcendentally sanctioned bond; provides stable social architecture for family formation and intergenerational transmission within Christian communities.
% TRANSFER_FUNCTION: Moves authority to define marital validity, govern dissolution, and supervise remarriage from individual conscience and civil state to ecclesiastical bodies (papal curia, denominational assemblies); moves submission and jurisdictional obedience from laity to canonical and denominational courts.
% ABSENT_VOICES: Civil state authorities asserting sole marriage jurisdiction; secular contract parties; individuals seeking same-sex or queer sacramental recognition; Catholic laity who would choose no-fault civil divorce if available within the sacramental framework; women historically disproportionately affected by invalidity rulings.
% DISAPPEARANCE_RATIONALE: Catholic laity would reorganize around civil divorce and remarriage without annulment; ecclesiastical courts would lose jurisdiction over family law; Protestant congregations would shift to purely civil or internal voluntary norms; the Church's institutional role as gatekeeper to valid Christian marriage would collapse.
% FOUNDING_PROBLEM: Unregulated intimate unions in early Christian communities producing uncertain legitimacy, inheritance disputes, lack of public commitment mechanisms, and competition with Roman and pagan marital customs.
% FOUNDING_PROBLEM_CORROBORATION: Secular legal historians and sociologists attest that civil registration and contract law now handle the coordination function; Catholic canon lawyers and magisterial documents attest the sacramental problem remains live. Corroboration from outside the beneficiary set: comparative civil family law scholars and secular historians document the historical shift, while noting the Church's ongoing claim.
narrative_ontology:disappearance_verdict(family_law_authority__christian_canonical_reading, world_rearranges).
narrative_ontology:founding_problem_status(family_law_authority__christian_canonical_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(family_law_authority__christian_canonical_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(family_law_authority__christian_canonical_reading, 'none', 1).
narrative_ontology:epsilon_provenance(family_law_authority__christian_canonical_reading, 0.62, 'kimi-k2.6', 'none', direct).

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
 *   Extractiveness (0.62) is substantial because the constraint transfers control over intimate union and dissolution from individuals to ecclesiastical bodies, with the Catholic wing enforcing identity-locked exit. Suppression (0.70) reflects the active denial of civil divorce effects within the sacramental framework and the exclusion of alternative validity regimes. Theater ratio (0.42 at present) captures the growing performative dimension of sacramental authority in a pluralist society where civil alternatives are readily available. Accessibility collapse (0.65) is high for devout Catholics who internalize the canonical frame, though external civil alternatives persist. Resistance (0.55) reflects historical Protestant secession, modern laity non-compliance, and state secularization pressures. The measurement series track the centralization and subsequent partial erosion of canonical enforcement capacity from the early modern period to the present.
 *
 * PERSPECTIVAL GAP:
 *   The magisterium experiences the constraint as sacred guardianship of a divine institution; from the payer seats, particularly identity-locked Catholic laity, it operates as enforced extraction of autonomy. Protestant congregants occupy an intermediate position where governance is experienced as coordination with lighter extraction. The engine computes this divergence from beneficiary/victim declarations and exit asymmetries.
 *
 * DIRECTIONALITY LOGIC:
 *   The ecclesiastical authorities (catholic_magisterium and protestant_denomination_assemblies) are declared beneficiaries and agenda-setters, placing them at the beneficiary end of the directionality spectrum. Catholic laity seeking dissolution and partners in invalid unions are declared victims with constrained or identity-locked exit, placing them at the target end. Protestant congregants, while payers, have less trapped exit and thus a lower derived directionality than their Catholic counterparts. The structural asymmetry is driven by the Church's monopoly on validity declarations and the identity fusion that binds adherents to the canonical framework.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem â unstable unions without public commitment mechanisms â is contested as still live. Secular civil law now provides registration, dissolution, and inheritance coordination. The constraint persists through authority claims and institutional identity rather than purely functional necessity, signaling partial mandatrophy. However, the genuine coordination function (sacramental community boundaries, pastoral care, intergenerational stability) prevents classification as pure snare; the asymmetric authority accumulation makes it tangled rope rather than rope.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    catholic_protestant_extractive_divergence,
    'Does the Protestant permission for divorce under denominational governance reduce the extractiveness of this reading to the level of rope, or does the retention of ecclesiastical authority over validity keep it tangled rope regardless?',
    'Comparative analysis of Protestant vs. Catholic laity exit costs, annulment rates, and authority accumulation by denominational assemblies versus the Roman Curia.',
    'If Protestant governance is structurally non-extractive, this reading may need decomposition into separate Catholic sacramental and Protestant denominational sub-readings.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(catholic_protestant_extractive_divergence, conceptual, 'Whether denominational variance within the reading alters the structural classification').

omega_variable(
    kernel_reading_sibling_secular,
    'This reading instantiates sacramental authority; the secular contractual reading instantiates autonomous state contract. What structural elements change across readings?',
    'Cross-reading comparison of beneficiary/victim sets, enforcement mechanisms, and directionality derivation across the kernel family.',
    'Determines whether the kernel is fundamentally about marriage coordination or about authority allocation between religious and civil institutions.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_sibling_secular, conceptual, 'Structural comparison between canonical and secular contractual readings of the kernel').

omega_variable(
    sacramental_naturalness_ambiguity,
    'Is the sacramental permanence claimed by this reading a divinely instituted natural law or a historically constructed juridical apparatus?',
    'Historical-critical study of canon law development alongside theological claims of divine institution; examination of doctrinal change over time.',
    'If purely constructed, the coordination function is historically contingent and the extraction may be higher than theologically claimed; if genuinely held as natural law by adherents, the identity-locked exit is stronger.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sacramental_naturalness_ambiguity, empirical, 'Whether sacramental indissolubility is natural law or constructed authority').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(family_law_authority__christian_canonical_reading, 0, 200).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fami_tr_t0, family_law_authority__christian_canonical_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(fami_tr_t50, family_law_authority__christian_canonical_reading, theater_ratio, 50, 0.3).
narrative_ontology:measurement(fami_tr_t100, family_law_authority__christian_canonical_reading, theater_ratio, 100, 0.35).
narrative_ontology:measurement(fami_tr_t150, family_law_authority__christian_canonical_reading, theater_ratio, 150, 0.4).
narrative_ontology:measurement(fami_tr_t200, family_law_authority__christian_canonical_reading, theater_ratio, 200, 0.42).

% Extraction over time
narrative_ontology:measurement(fami_be_t0, family_law_authority__christian_canonical_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(fami_be_t50, family_law_authority__christian_canonical_reading, base_extractiveness, 50, 0.58).
narrative_ontology:measurement(fami_be_t100, family_law_authority__christian_canonical_reading, base_extractiveness, 100, 0.65).
narrative_ontology:measurement(fami_be_t150, family_law_authority__christian_canonical_reading, base_extractiveness, 150, 0.6).
narrative_ontology:measurement(fami_be_t200, family_law_authority__christian_canonical_reading, base_extractiveness, 200, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(fami_su_t0, family_law_authority__christian_canonical_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(fami_su_t50, family_law_authority__christian_canonical_reading, suppression_requirement, 50, 0.65).
narrative_ontology:measurement(fami_su_t100, family_law_authority__christian_canonical_reading, suppression_requirement, 100, 0.75).
narrative_ontology:measurement(fami_su_t150, family_law_authority__christian_canonical_reading, suppression_requirement, 150, 0.68).
narrative_ontology:measurement(fami_su_t200, family_law_authority__christian_canonical_reading, suppression_requirement, 200, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(family_law_authority__christian_canonical_reading, identity_coordination).
narrative_ontology:affects_constraint(family_law_authority__christian_canonical_reading, hindu_dharmashastra_reading).
narrative_ontology:affects_constraint(family_law_authority__christian_canonical_reading, muslim_shariat_reading).
narrative_ontology:affects_constraint(family_law_authority__christian_canonical_reading, parsi_zoroastrian_reading).
narrative_ontology:affects_constraint(family_law_authority__christian_canonical_reading, secular_contractual_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the family_law_authority kernel, which decomposes into five religious and secular readings. The Christian canonical reading claims sacramental or denominational authority over marriage validity, distinct from dharmic, shariat, Zoroastrian, and civil-contract readings. Network edges reflect shared regulatory domain and institutional competition in comparative family law.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
