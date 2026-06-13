% ============================================================================
% CONSTRAINT STORY: marriage_authority_kernel__christian_canonical_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_marriage_authority_kernel__christian_canonical_reading, []).

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
 *   constraint_id: marriage_authority_kernel__christian_canonical_reading
 *   human_readable: Christian Canonical Marriage Authority (Indian Christian Marriage Act 1872)
 *   domain: religious/constitutional/family law
 *
 * SUMMARY:
 *   The Indian Christian Marriage Act 1872 grounds marriage authority in
 *   Christian canonical law as interpreted and administered by the church's
 *   ecclesiastical apparatus. The act codifies the doctrine of marriage
 *   indissolubility, restricts divorce to fault-based grounds (adultery,
 *   cruelty, desertion), and preserves the church's exclusive authority to
 *   grant annulment. This reading instantiates ONE authority structure within
 *   the contested marriage-authority kernel: Christian canonical law is
 *   legitimate precisely because it derives from apostolic tradition and
 *   operates as the church's self-governance of its community. Sibling
 *   readings instantiate alternative authority structures (Hindu
 *   codification, Muslim shariat, Parsi custom, secular civil code) that
 *   compete in India's religious-pluralist constitutional space. This
 *   constraint story describes the Christian canonical reading as a clean
 *   ε-invariant structure — what makes Christian marriage law distinctive in
 *   India, who benefits, who bears costs, and how it coordinates community
 *   identity while extracting from those seeking exit.
 *
 * KEY AGENTS:
 *   - church_ecclesiastical_authority: institutional agenda-setter, maintains canonical doctrine and tribunal system
 *   - christian_women_married_persons: powerless payers bound by identity-locked exit, limited divorce grounds
 *   - remarriage_aspirants: moderate-power payers constrained by annulment gatekeeping
 *   - interfaith_couples: moderate-power payers asymmetrically constrained by canonical impediments
 *   - civil_courts: institutional observer, increasingly reading secular grounds into the statute
 *   - secular_civil_framers: institutional-level excluded voice promoting alternative reading
 *   - christian_community_preservation: institutional beneficiary, maintains doctrinal distinctiveness
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(marriage_authority_kernel__christian_canonical_reading, 0.48).
domain_priors:suppression_score(marriage_authority_kernel__christian_canonical_reading, 0.52).
domain_priors:theater_ratio(marriage_authority_kernel__christian_canonical_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(marriage_authority_kernel__christian_canonical_reading, extractiveness, 0.48).
narrative_ontology:constraint_metric(marriage_authority_kernel__christian_canonical_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(marriage_authority_kernel__christian_canonical_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(marriage_authority_kernel__christian_canonical_reading, accessibility_collapse, 0.64).
narrative_ontology:constraint_metric(marriage_authority_kernel__christian_canonical_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(marriage_authority_kernel__christian_canonical_reading, tangled_rope).
narrative_ontology:human_readable(marriage_authority_kernel__christian_canonical_reading, "Christian Canonical Marriage Authority (Indian Christian Marriage Act 1872)").
narrative_ontology:topic_domain(marriage_authority_kernel__christian_canonical_reading, "religious/constitutional/family law").

domain_priors:requires_active_enforcement(marriage_authority_kernel__christian_canonical_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(marriage_authority_kernel__christian_canonical_reading, '4c141207-a017-4d4c-8acd-6fc0ce810946').
narrative_ontology:cs_kernel_codification('4c141207-a017-4d4c-8acd-6fc0ce810946', fixed_text).
narrative_ontology:cs_authority_grounding('4c141207-a017-4d4c-8acd-6fc0ce810946', lineage).
narrative_ontology:cs_interpretation_layer_present('4c141207-a017-4d4c-8acd-6fc0ce810946').
narrative_ontology:cs_reading_relation('4c141207-a017-4d4c-8acd-6fc0ce810946', marriage_authority_kernel__hindu_codified_reading, coexists_with).
narrative_ontology:cs_reading_relation('4c141207-a017-4d4c-8acd-6fc0ce810946', marriage_authority_kernel__muslim_shariat_reading, coexists_with).
narrative_ontology:cs_reading_relation('4c141207-a017-4d4c-8acd-6fc0ce810946', marriage_authority_kernel__parsi_communal_reading, coexists_with).
narrative_ontology:cs_reading_relation('4c141207-a017-4d4c-8acd-6fc0ce810946', marriage_authority_kernel__secular_civil_reading, influences).
narrative_ontology:cs_axiom('4c141207-a017-4d4c-8acd-6fc0ce810946', foundational, marriage_indissolubility_apostolic_doctrine).
narrative_ontology:cs_axiom_status(marriage_indissolubility_apostolic_doctrine, holdable).
narrative_ontology:cs_axiom_grounding('4c141207-a017-4d4c-8acd-6fc0ce810946', marriage_indissolubility_apostolic_doctrine, deontological).
narrative_ontology:cs_axiom('4c141207-a017-4d4c-8acd-6fc0ce810946', foundational, ecclesiastical_authority_depositum_fidei).
narrative_ontology:cs_axiom_status(ecclesiastical_authority_depositum_fidei, holdable).
narrative_ontology:cs_axiom_grounding('4c141207-a017-4d4c-8acd-6fc0ce810946', ecclesiastical_authority_depositum_fidei, theological).
narrative_ontology:cs_reference_frame('4c141207-a017-4d4c-8acd-6fc0ce810946', apostolic_tradition_codified_1872).
narrative_ontology:cs_drift_state('4c141207-a017-4d4c-8acd-6fc0ce810946', contemporary_pluralist_india_2026, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('4c141207-a017-4d4c-8acd-6fc0ce810946', '').
narrative_ontology:cs_kernel_id(marriage_authority_kernel__christian_canonical_reading, marriage_authority_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(marriage_authority_kernel__christian_canonical_reading, church_ecclesiastical_authority).
narrative_ontology:constraint_beneficiary(marriage_authority_kernel__christian_canonical_reading, christian_community_cohesion).
narrative_ontology:constraint_victim(marriage_authority_kernel__christian_canonical_reading, women_seeking_divorce).
narrative_ontology:constraint_victim(marriage_authority_kernel__christian_canonical_reading, remarriage_aspirants).
narrative_ontology:constraint_victim(marriage_authority_kernel__christian_canonical_reading, interfaith_couples).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(marriage_authority_kernel__christian_canonical_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(marriage_authority_kernel__christian_canonical_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(marriage_authority_kernel__christian_canonical_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(marriage_authority_kernel__christian_canonical_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(marriage_authority_kernel__christian_canonical_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.48) because the constraint's operation concentrates gatekeeping authority in the church (high institutional power) while affecting dispersed individuals (lower effective power). The extraction is not maximal (as in a pure snare) because the coordination function is genuine — the constraint does solve a real problem of maintaining Christian community identity around marriage doctrine. Suppression is also moderate (0.52) because the identity-lock (being Christian means accepting Christian marriage law) and the constrained exit (civil remarriage carries social costs) operate without extreme coercion — Christian women in failed marriages can in principle migrate to secular marriage law, though the cost is significant. Theater has risen over 154 years (0.25 to 0.41) because as secular alternatives became available and social secularization advanced, the church's maintenance of canonical distinctiveness increasingly took performative rather than coordinate form — the tribunals' annulment function survives partly as institutional theater defending the doctrine rather than as vital coordination. The measurement series trace this dynamic: extractiveness stable-to-slightly-declining (the constraint's pull weakens as secular alternatives proliferate), theater rising (the maintenance effort becomes more visible as genuine coordination pressure falls), suppression stable-to-slightly-declining (the identity-lock persists but is less enforced, more internalized as choice). This is a Tangled Rope where the coordination (Christian community identity) has become smaller and the extraction (gatekeeping power) has become more visible.
 *
 * PERSPECTIVAL GAP:
 *   From the church's seat, this constraint is legitimate coordination: maintaining the doctrine of Christian marriage indissolubility and providing ecclesiastical dispute resolution is a form of community self-governance that respects Christian identity and tradition. From the Christian woman's seat, it is extraction with identity-lock: she cannot dissolve her marriage by her own choice and leaving the marriage law means leaving her community. From the secular civil observer's seat, it is a vestigial religious authority that violates constitutional equality principles and should be superseded by the secular civil reading. The engine computes these divergent readings from the structural data: institutional power + arbitrage exit options (church) yields a beneficiary directionality; powerless + identity-locked (women) yields a target directionality; analytical power + analytical exit (courts) yields an observer directionality. The computed type diverges per seat because the constraint IS experienced differently from each structural position.
 *
 * DIRECTIONALITY LOGIC:
 *   Church ecclesiastical authority sits at institutional power with arbitrage exit — high directionality toward beneficiary (d ≈ 0.15). Christian women married persons sit at powerless with identity-locked exit — directionality toward target (d ≈ 0.85). The identity-lock is the critical structural difference: being Christian and accepting Christian marriage law are fused. Exit means either (a) civil remarriage (abandoning canonical framework, carrying sectarian social costs) or (b) permanent unmarriage (remaining in the community but violating personal autonomy). Neither exit is genuinely free. Remarriage aspirants sit at moderate power with constrained exit — directionality intermediate but leaning target (d ≈ 0.68), because annulment gatekeeping constrains their options even if they could theoretically pursue civil remarriage. Secular observers sit at institutional power with analytical exit — directionality neutral (d = 0.5) because they experience the constraint as a governance question, not as a lived constraint.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (how to govern Christian marriage in colonial India without subsuming Christians into Hindu or Islamic personal law) was genuine coordination: it preserved Christian doctrinal distinctiveness and allowed the community to maintain marriage as a sacrament under ecclesiastical authority. That coordination problem remains partially live (the church continues to assert it), but it has been substantially superseded by constitutional secularism and the availability of secular civil marriage alternatives. The constraint has shifted from coordination + enforcement to coordination + extraction + theater: the church maintains the doctrine (coordination), enforces it through tribunals (enforcement), but increasingly as defensive institutional theater (rising theater_ratio) because the secular alternative means Christians no longer depend on canonical authority for marriage governance. The rising theater_ratio over 154 years and stable-to-declining extractiveness (as secular law expanded) suggest mandatrophy is partial: the constraint persists because it vindicates Christian doctrinal identity and maintains the church's institutional gatekeeping power, but the coordination that originally justified it has atrophied. The 2016 Jose Paul Supreme Court ruling, reading irretrievable breakdown into the statute despite the church's canonical opposition, is empirical evidence of this mandatrophy: the court applied reasoning from secular personal laws (Hindu, Muslim) to the Christian 1872 Act, effectively subordinating canonical grounds to civil grounds. The constraint remains (the 1872 Act still stands) but increasingly as institutional inertia and theatrical maintenance rather than as vital coordination.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    canonical_authority_legitimacy,
    'Is Christian canonical marriage authority legitimate as a form of religious community self-governance within India''s secular constitutional framework, or does it violate secular equality principles by subordinating Christian women to ecclesiastical gatekeeping?',
    'Constitutional Court clarification of the place of religious personal law within India''s secular scheme (follow-up to the Jose Paul ruling). Comparative analysis of religious autonomy doctrine across constitutions (Canada, Australia, South Africa) where this question has been addressed.',
    'If legitimate as self-governance: the constraint is Rope (genuine coordination with no invalidating extraction). If violates equality: the constraint is Snare (coordination as cover for extraction of authority). This is the foundational contest about the entire constraint''s type.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(canonical_authority_legitimacy, conceptual, 'Whether the canonical reading''s authority grounding is democratically defensible or constitutionally incompatible with secularism.').

omega_variable(
    christian_identity_fusion_malleability,
    'Is Christian identity in India constitutively fused with canonical marriage law (identity-lock is deep and irreversible), or is the fusion contingent and reversible (Christian identity could survive and flourish under secular divorce law)?',
    'Ethnographic longitudinal study of Christian women who opted for civil divorce and remarriage: do they report Christian identity as damaged or intact? Comparative study of European Christian communities where canonical authority has atrophied: is Christian identity in those contexts weakened? Institutional analysis of progressive Christian theology in India that rejects canonical indissolubility while maintaining Christian identity.',
    'If deep fusion: identity-lock is authentic and unavoidable, suppression is structural and extensive, exit costs are truly high, the constraint operates with legitimacy for those who hold the identity. If contingent: identity-lock is internalized conditioning, exit costs are socially manufactured (reversible through cultural change), the constraint operates partly through false consciousness, decoupling canonical law from Christian identity is possible.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(christian_identity_fusion_malleability, empirical, 'Whether identity-lock can be dissolved by changing institutional and cultural context, or is intrinsic to Christian faith.').

omega_variable(
    founding_problem_supersession,
    'Is the founding problem (Christian marriage authority in colonial pluralism) still live, or has it been substantially superseded by post-colonial constitutional secularism and the availability of secular civil marriage law?',
    'Survey of Christian community leaders on whether separate Christian personal law is still necessary and valued. Statistical analysis of Christian marriage choices: what fraction of Christians marry under the 1872 Act vs. Special Marriage Act 1954? Trends in civil vs. canonical divorce petitions over decades.',
    'If live: the constraint is justified coordination addressing a current need. If superseded: the constraint persists as institutional inertia and theater defending an obsolete founding problem, should be reclassified as Piton. This determination changes whether mandatrophy is partial (supersession partial) or near-complete (founding problem dead but constraint lives on).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(founding_problem_supersession, empirical, 'Whether the canonical reading still solves a problem Christians need solved, or persists as institutional performance.').

omega_variable(
    suppression_mechanism_structure_vs_internalization,
    'Is the measured suppression of Christian women''s divorce options primarily structural (external barriers: 1872 Act limits, tribunal gatekeeping, social costs) or substantially internalized (women internalize the doctrine as legitimate and carry the suppression psychologically)?',
    'Post-exit ethnography: women who obtain secular divorce and remarriage, then are re-interviewed years later about whether suppression persisted after structural removal. Comparative study of communities that have formally decoupled Christian identity from canonical marriage law (e.g., progressive Christian communities) and whether suppression of women''s autonomy declined. Qualitative analysis of women''s own accounts: do they frame divorce restriction as external constraint or as internal moral commitment?',
    'If primarily structural: changing the 1872 Act would substantially relieve suppression. If substantially internalized: legal change alone would be insufficient; cultural transformation in Christian identity frameworks would be required. Affects the remedy design and the diagnosis of how deep the constraint''s grip is.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_structure_vs_internalization, empirical, 'Whether the suppression persists after structural mechanisms are removed (internalization signature).').

omega_variable(
    kernel_reading_counterfactual_coherence,
    'Could Christian canonical marriage authority in India coexist with the Hindu codified reading, Muslim shariat reading, Parsi communal reading, and secular civil reading in a single constitutional framework without logical contradiction, or does accepting one reading logically foreclose accepting another?',
    'Close analysis of whether the five readings rest on incompatible foundational premises about what legitimates marriage law authority. Test case: can a court rule that Christian canonical marriage law is legitimate (for Christians) AND secular civil law is legitimate (for those who opt into it) AND Hindu law is legitimate (for Hindus) — all simultaneously, without contradiction? If yes: the readings coexist. If no: some foreclose others.',
    'If coexist without logical contradiction: the READING_RELATIONS should use coexists_with. If some foreclose others: use forecloses. This shapes the diagnosis of constitutional pluralism: is it genuinely pluralist (multiple legitimate readings held simultaneously), or is it jurisdictional partition that masks deeper conflicts?',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_counterfactual_coherence, conceptual, 'Whether the Christian canonical reading logically coexists with sibling readings or forecloses some.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(marriage_authority_kernel__christian_canonical_reading, 1872, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(marr_tr_t1872, marriage_authority_kernel__christian_canonical_reading, theater_ratio, 1872, 0.25).
narrative_ontology:measurement(marr_tr_t1920, marriage_authority_kernel__christian_canonical_reading, theater_ratio, 1920, 0.28).
narrative_ontology:measurement(marr_tr_t1960, marriage_authority_kernel__christian_canonical_reading, theater_ratio, 1960, 0.32).
narrative_ontology:measurement(marr_tr_t1990, marriage_authority_kernel__christian_canonical_reading, theater_ratio, 1990, 0.38).
narrative_ontology:measurement(marr_tr_t2010, marriage_authority_kernel__christian_canonical_reading, theater_ratio, 2010, 0.41).
narrative_ontology:measurement(marr_tr_t2026, marriage_authority_kernel__christian_canonical_reading, theater_ratio, 2026, 0.41).

% Extraction over time
narrative_ontology:measurement(marr_be_t1872, marriage_authority_kernel__christian_canonical_reading, base_extractiveness, 1872, 0.52).
narrative_ontology:measurement(marr_be_t1920, marriage_authority_kernel__christian_canonical_reading, base_extractiveness, 1920, 0.51).
narrative_ontology:measurement(marr_be_t1960, marriage_authority_kernel__christian_canonical_reading, base_extractiveness, 1960, 0.49).
narrative_ontology:measurement(marr_be_t1990, marriage_authority_kernel__christian_canonical_reading, base_extractiveness, 1990, 0.48).
narrative_ontology:measurement(marr_be_t2010, marriage_authority_kernel__christian_canonical_reading, base_extractiveness, 2010, 0.47).
narrative_ontology:measurement(marr_be_t2026, marriage_authority_kernel__christian_canonical_reading, base_extractiveness, 2026, 0.48).

% Suppression requirement over time
narrative_ontology:measurement(marr_su_t1872, marriage_authority_kernel__christian_canonical_reading, suppression_requirement, 1872, 0.48).
narrative_ontology:measurement(marr_su_t1920, marriage_authority_kernel__christian_canonical_reading, suppression_requirement, 1920, 0.5).
narrative_ontology:measurement(marr_su_t1960, marriage_authority_kernel__christian_canonical_reading, suppression_requirement, 1960, 0.52).
narrative_ontology:measurement(marr_su_t1990, marriage_authority_kernel__christian_canonical_reading, suppression_requirement, 1990, 0.54).
narrative_ontology:measurement(marr_su_t2010, marriage_authority_kernel__christian_canonical_reading, suppression_requirement, 2010, 0.54).
narrative_ontology:measurement(marr_su_t2026, marriage_authority_kernel__christian_canonical_reading, suppression_requirement, 2026, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(marriage_authority_kernel__christian_canonical_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(marriage_authority_kernel__christian_canonical_reading, 0.12).
narrative_ontology:affects_constraint(marriage_authority_kernel__christian_canonical_reading, marriage_authority_kernel__hindu_codified_reading).
narrative_ontology:affects_constraint(marriage_authority_kernel__christian_canonical_reading, marriage_authority_kernel__muslim_shariat_reading).
narrative_ontology:affects_constraint(marriage_authority_kernel__christian_canonical_reading, marriage_authority_kernel__parsi_communal_reading).
narrative_ontology:affects_constraint(marriage_authority_kernel__christian_canonical_reading, marriage_authority_kernel__secular_civil_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the contested marriage_authority_kernel in India's religious-pluralist constitutional space. The kernel question is: what authority grounds marriage law? Five readings provide structurally distinct answers, each instantiating a different constraint with a different ε, beneficiary/victim structure, and type. The Christian canonical reading asserts apostolic tradition and church doctrine; the Hindu codified reading asserts statutory civil law interpreted by secular courts; the Muslim shariat reading asserts Islamic jurisprudence as interpreted by community boards; the Parsi communal reading asserts community custom codified by the Parsi legislature; the secular civil reading asserts individual constitutional rights grounded in the Special Marriage Act 1954. Each reading is a separate constraint story in this corpus. They are linked via network.affects_constraints because they compete for institutional space and because accepting one reading constrains or influences the others. The divergence in ε values across the five readings (the Christian canonical reading has lower extractiveness than the Muslim shariat reading, for example, because Islamic divorce law gives women greater unilateral exit options like khul'; the secular reading has near-zero extractiveness because it is grounded in individual choice) is not measurement error or observational relativism — it is the reflection of genuinely different structural constraints with different capacities for extraction. The five readings have different founding problems, different beneficiaries and victims, different authority groundings, and different trajectories under contemporary constitutional pressure.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
