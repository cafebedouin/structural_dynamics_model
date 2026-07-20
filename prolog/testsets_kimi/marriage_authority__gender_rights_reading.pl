% ============================================================================
% CONSTRAINT STORY: marriage_authority__gender_rights_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
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
 *   constraint_id: marriage_authority__gender_rights_reading
 *   human_readable: Marriage Authority â Gender Rights Reading (Personal Law as Extractive Snare)
 *   domain: legal/constitutional/family_law
 *
 * SUMMARY:
 *   This constraint instantiates the gender_rights_reading of the
 *   marriage_authority kernel: marriage authority as it operates through
 *   patriarchal personal law codes that are contested via judicial expansion
 *   of constitutional equality guarantees. The reading cross-cuts the
 *   communal/secular divide by using constitutional norms to intervene inside
 *   community family law rather than abolishing pluralism outright. The
 *   constraint targets specific practices (triple talaq, maintenance,
 *   property shares) rather than dismantling the system-level structure of
 *   religious personal law. It is authored as a snare because the
 *   coordination storyâconstitutional equality protecting womenâmasks an
 *   extraction pattern in which the reform apparatus (advocates, litigation
 *   institutions) captures resources and legitimacy while the women it claims
 *   to help bear the costs of test-case litigation and community backlash.
 *
 * KEY AGENTS:
 *   - women_rights_advocates: Primary beneficiary (organized/mobile) â captures funding and institutional power from reform litigation
 *   - women_within_patriarchal_personal_law: Primary target (powerless/identity_locked) â bears patriarchal rules and reform-process costs
 *   - personal_law_religious_authorities: Agenda_setter (institutional/constrained) â enforces patriarchal norms through communal institutions
 *   - constitutional_judiciary: Analytical observer (institutional/analytical) â intervenes piecemeal without systemic change
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(marriage_authority__gender_rights_reading, 0.82).
domain_priors:suppression_score(marriage_authority__gender_rights_reading, 0.75).
domain_priors:theater_ratio(marriage_authority__gender_rights_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(marriage_authority__gender_rights_reading, extractiveness, 0.82).
narrative_ontology:constraint_metric(marriage_authority__gender_rights_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(marriage_authority__gender_rights_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(marriage_authority__gender_rights_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(marriage_authority__gender_rights_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(marriage_authority__gender_rights_reading, snare).
narrative_ontology:human_readable(marriage_authority__gender_rights_reading, "Marriage Authority â Gender Rights Reading (Personal Law as Extractive Snare)").
narrative_ontology:topic_domain(marriage_authority__gender_rights_reading, "legal/constitutional/family_law").

domain_priors:requires_active_enforcement(marriage_authority__gender_rights_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(marriage_authority__gender_rights_reading, 'be8359b4-ae18-4d3c-8e4c-18fd871b5711').
narrative_ontology:cs_kernel_codification('be8359b4-ae18-4d3c-8e4c-18fd871b5711', formalized).
narrative_ontology:cs_authority_grounding('be8359b4-ae18-4d3c-8e4c-18fd871b5711', lineage).
narrative_ontology:cs_interpretation_layer_present('be8359b4-ae18-4d3c-8e4c-18fd871b5711').
narrative_ontology:cs_reading_relation('be8359b4-ae18-4d3c-8e4c-18fd871b5711', marriage_authority__communal_autonomy_reading, forecloses).
narrative_ontology:cs_reading_relation('be8359b4-ae18-4d3c-8e4c-18fd871b5711', marriage_authority__secularist_reading, coexists_with).
narrative_ontology:cs_reading_relation('be8359b4-ae18-4d3c-8e4c-18fd871b5711', marriage_authority__federalist_millet_reading, influences).
narrative_ontology:cs_reading_relation('be8359b4-ae18-4d3c-8e4c-18fd871b5711', marriage_authority__judicial_harmonization_reading, influences).
narrative_ontology:cs_axiom('be8359b4-ae18-4d3c-8e4c-18fd871b5711', foundational, constitutional_equality_supremacy_in_family_law).
narrative_ontology:cs_axiom_status(constitutional_equality_supremacy_in_family_law, holdable).
narrative_ontology:cs_axiom_grounding('be8359b4-ae18-4d3c-8e4c-18fd871b5711', constitutional_equality_supremacy_in_family_law, deontological).
narrative_ontology:cs_axiom('be8359b4-ae18-4d3c-8e4c-18fd871b5711', foundational, gender_equality_as_non_negotiable_constitutional_floor).
narrative_ontology:cs_axiom_status(gender_equality_as_non_negotiable_constitutional_floor, holdable).
narrative_ontology:cs_axiom_grounding('be8359b4-ae18-4d3c-8e4c-18fd871b5711', gender_equality_as_non_negotiable_constitutional_floor, deontological).
narrative_ontology:cs_reference_frame('be8359b4-ae18-4d3c-8e4c-18fd871b5711', constitutional_equality_framework).
narrative_ontology:cs_drift_state('be8359b4-ae18-4d3c-8e4c-18fd871b5711', contemporary_personal_law_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('be8359b4-ae18-4d3c-8e4c-18fd871b5711', '').
narrative_ontology:cs_kernel_id(marriage_authority__gender_rights_reading, marriage_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(marriage_authority__gender_rights_reading, women_rights_advocates).
narrative_ontology:constraint_victim(marriage_authority__gender_rights_reading, women_within_patriarchal_personal_law).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Litigate constitutional challenges to patriarchal personal law practices, run NGOs, and sit on government reform commissions. Their funding, institutional standing, and policy influence grow through sustained engagement with discriminatory marriage rules. They do not administer personal law but capture resources, media visibility, and legal victories from the reform process.
narrative_ontology:constraint_stakeholder(marriage_authority__gender_rights_reading, women_rights_advocates, beneficiary,
    organized, biographical, mobile, national).

% Governed by religious personal law codes that discriminate in divorce, maintenance, and property. Must rely on lengthy constitutional litigation to assert equality claims, bearing costs of legal delay, community ostracism, economic insecurity, and familial rupture. Religious and kinship identity lock them into the personal law system; exit means social excommunication and loss of economic support.
narrative_ontology:constraint_stakeholder(marriage_authority__gender_rights_reading, women_within_patriarchal_personal_law, payer,
    powerless, immediate, identity_locked, national).

% Administer and interpret religious personal law codes governing marriage, divorce, and inheritance through communal institutions and religious courts. Resist constitutional equality incursions as threats to communal autonomy. Derive authority from religious lineage and community adherence; their institutional role depends on preserving patriarchal family structures.
narrative_ontology:constraint_stakeholder(marriage_authority__gender_rights_reading, personal_law_religious_authorities, agenda_setter,
    institutional, generational, constrained, national).

% Hears constitutional challenges to personal law practices, expanding equality guarantees piecemeal through case-by-case review. Positioned between communal autonomy claims and gender equality demands; its interventions create precedent but leave system-level personal law structures intact.
narrative_ontology:constraint_stakeholder(marriage_authority__gender_rights_reading, constitutional_judiciary, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(marriage_authority__gender_rights_reading, women_rights_advocates).
narrative_ontology:fixing_cost_class(marriage_authority__gender_rights_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Claims to reconcile religious personal law with constitutional gender equality through selective judicial intervention, protecting women from discriminatory practices while preserving a pluralist legal framework.
% TRANSFER_FUNCTION: Moves legitimacy, funding, organizational authority, and public attention from women within personal law communities to women rights advocates and the judicial reform apparatus, while leaving patriarchal system-level structures largely intact.
% ABSENT_VOICES: Women who endorse communal personal law framing, male community members defending patriarchal norms as religious duty, and legislative reformers seeking a Uniform Civil Code are marginalized by the judicialization of the conflict; their positions are heard as obiter dicta rather than core holdings.
% DISAPPEARANCE_RATIONALE: If the constraint vanished, personal law would lose its patriarchal enforcement architecture, women rights advocates would lose their primary institutional platform and funding base, family law authority would shift toward either communal autonomy or legislative uniformity, and the judicial reform ecosystem would collapse.
% FOUNDING_PROBLEM: Post-colonial need to protect minority religious communities from majoritarian cultural assimilation while maintaining a unified constitutional governance structure; gender equality introduced later as a counter-principle against patriarchal personal law.
% FOUNDING_PROBLEM_CORROBORATION: Constitutional historians and minority rights scholars outside the beneficiary set attest the founding problem of communal protection; feminist legal scholars and affected women attest that the gender equality framing is a later graft onto a structure built for communal accommodation, not individual rights.
narrative_ontology:disappearance_verdict(marriage_authority__gender_rights_reading, world_rearranges).
narrative_ontology:founding_problem_status(marriage_authority__gender_rights_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(marriage_authority__gender_rights_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(marriage_authority__gender_rights_reading, 'none', 1).
narrative_ontology:epsilon_provenance(marriage_authority__gender_rights_reading, 0.82, 'kimi-k2.6', 'none', direct).

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
 *   Extractiveness is high (0.82) because patriarchal personal law systematically transfers economic and decision-making autonomy from women to male family members and community structures, while the judicial reform process extracts time, social capital, and dignity from women as test-case subjects. Suppression (0.75) reflects the combination of legal pluralism trapping women in religious jurisdiction and community enforcement of patriarchal norms. Theater_ratio (0.45) captures the mix of genuine but partial judicial victories with performative reform that leaves system-level structures untouched. Accessibility_collapse (0.60) acknowledges that judicial remedies exist on paper but are socially and economically inaccessible for most women. Resistance (0.55) reflects organized community backlash and conservative judicial hesitation. The temporal series run on a shared grid showing extraction and theater accumulating over four decades of piecemeal litigation without structural transformation.
 *
 * PERSPECTIVAL GAP:
 *   The women_rights_advocates seat should compute as beneficiary (low d, damped or negative chi) because they gain funding, legitimacy, and policy influence from the constraint's persistence. The women_within_patriarchal_personal_law seat should compute as full target (high d, amplified chi) because they bear both the patriarchal extraction and the costs of judicial contestation. The religious authorities seat sits in a complex intermediate position: they enforce the constraint and benefit from its patriarchal distribution, but their authority is eroded by judicial challenges, creating directional pressure toward the target end that the structural derivation alone may not capture.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary and victim declarations map the structural asymmetry: women_rights_advocates are declared beneficiaries because their organizational survival, funding streams, and legal careers depend on sustained engagement with the constraint; they collect from the reform process the constraint sustains. Women_within_patriarchal_personal_law are declared victims because they pay the costs of discriminatory divorce, unequal maintenance, property exclusion, and the social and economic penalties of being test-case litigants. The judiciary is an observer with analytical exit, neither collecting nor paying. Religious authorities are agenda_setters whose directional position is structurally complexâderivation may place them near the beneficiary end, but judicial erosion pulls them toward the target end.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading prevents mislabeling by distinguishing the coordination story (constitutional equality as protection for women) from the extraction pattern (reform industry sustained by persistent patriarchal structure). If the constraint were a scaffold, it would carry a sunset clause and show declining extraction as the founding problem is solved; instead, extraction rises over time and no sunset exists. If it were a rope, beneficiaries would include the coordinated population (women) and extraction would be low; instead, women are victims and extraction is high. The divergence between the equality-framed coordination claim and the authored metrics is the signal the engine is designed to detect.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reform_industry_extraction,
    'Do women rights advocates structurally benefit from the persistence of patriarchal personal law more than from its abolition?',
    'Track funding, staffing, and litigation volume of women''s rights NGOs against rates of systemic personal law reform; if organizational growth continues while system-level change stalls, extraction is confirmed.',
    'Would reclassify the beneficiary structure and confirm the snare reading; if advocates benefit more from persistence than resolution, the coordination story is cover.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reform_industry_extraction, empirical, 'Whether the advocacy industry extracts from the persistence of patriarchal personal law').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression of women''s exit options structural (legal bars, economic dependency) or internalized (identity fusion with community and religion)?',
    'Post-reform trajectory analysis: if women continue to accept patriarchal settlements after legal barriers fall, suppression is partially internalized.',
    'If internalized, effective suppression exceeds structural measures; identity_locked exit would drive directionality closer to full target.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural versus internalized suppression mechanism in personal law compliance').

omega_variable(
    judicial_expansion_vs_legislative,
    'Does judicial expansion of equality into personal law foreclose legislative reform or merely delay it?',
    'Comparative analysis across jurisdictions with judicialized personal law reform versus legislative uniform family code adoption.',
    'If foreclosed, the constraint''s scope is wider than apparent; if delayed, the constraint may be a scaffold that failed to sunset.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(judicial_expansion_vs_legislative, conceptual, 'Whether judicialization blocks or delays legislative family law reform').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(marriage_authority__gender_rights_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(magr_gdr_tr_t0, marriage_authority__gender_rights_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(magr_gdr_tr_t8, marriage_authority__gender_rights_reading, theater_ratio, 8, 0.3).
narrative_ontology:measurement(magr_gdr_tr_t16, marriage_authority__gender_rights_reading, theater_ratio, 16, 0.35).
narrative_ontology:measurement(magr_gdr_tr_t24, marriage_authority__gender_rights_reading, theater_ratio, 24, 0.38).
narrative_ontology:measurement(magr_gdr_tr_t32, marriage_authority__gender_rights_reading, theater_ratio, 32, 0.42).
narrative_ontology:measurement(magr_gdr_tr_t40, marriage_authority__gender_rights_reading, theater_ratio, 40, 0.45).

% Extraction over time
narrative_ontology:measurement(magr_gdr_be_t0, marriage_authority__gender_rights_reading, base_extractiveness, 0, 0.7).
narrative_ontology:measurement(magr_gdr_be_t8, marriage_authority__gender_rights_reading, base_extractiveness, 8, 0.73).
narrative_ontology:measurement(magr_gdr_be_t16, marriage_authority__gender_rights_reading, base_extractiveness, 16, 0.75).
narrative_ontology:measurement(magr_gdr_be_t24, marriage_authority__gender_rights_reading, base_extractiveness, 24, 0.78).
narrative_ontology:measurement(magr_gdr_be_t32, marriage_authority__gender_rights_reading, base_extractiveness, 32, 0.8).
narrative_ontology:measurement(magr_gdr_be_t40, marriage_authority__gender_rights_reading, base_extractiveness, 40, 0.82).

% Suppression requirement over time
narrative_ontology:measurement(magr_gdr_su_t0, marriage_authority__gender_rights_reading, suppression_requirement, 0, 0.62).
narrative_ontology:measurement(magr_gdr_su_t8, marriage_authority__gender_rights_reading, suppression_requirement, 8, 0.65).
narrative_ontology:measurement(magr_gdr_su_t16, marriage_authority__gender_rights_reading, suppression_requirement, 16, 0.68).
narrative_ontology:measurement(magr_gdr_su_t24, marriage_authority__gender_rights_reading, suppression_requirement, 24, 0.71).
narrative_ontology:measurement(magr_gdr_su_t32, marriage_authority__gender_rights_reading, suppression_requirement, 32, 0.73).
narrative_ontology:measurement(magr_gdr_su_t40, marriage_authority__gender_rights_reading, suppression_requirement, 40, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(marriage_authority__gender_rights_reading, communal_autonomy_reading).
narrative_ontology:affects_constraint(marriage_authority__gender_rights_reading, federalist_millet_reading).
narrative_ontology:affects_constraint(marriage_authority__gender_rights_reading, judicial_harmonization_reading).
narrative_ontology:affects_constraint(marriage_authority__gender_rights_reading, secularist_reading).

% DUAL FORMULATION NOTE:
% Decomposed from the colloquial label 'personal law' which conflates communal autonomy, federalist pluralism, gender equality, judicial harmonization, and secular uniformist claims. Each reading carries a distinct epsilon and stakeholder structure.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
