% ============================================================================
% CONSTRAINT STORY: marriage_authority_kernel__muslim_shariat_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_marriage_authority_kernel__muslim_shariat_reading, []).

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
 *   constraint_id: marriage_authority_kernel__muslim_shariat_reading
 *   human_readable: Shariat-Based Marriage Authority via Personal Law Boards and Qazis
 *   domain: legal/constitutional/religious
 *
 * SUMMARY:
 *   In the Indian constitutional settlement, marriage and family law
 *   authority for Muslim citizens is granted to personal law jurisdiction,
 *   interpreted and administered by qazis and the All-India Muslim Personal
 *   Law Board. This constraint instantiates the Shariat-reading of the
 *   contested marriage-authority kernel — the claim that legitimate family
 *   law authority derives from religious tradition and community adjudication
 *   rather than secular state courts. The constraint coordinates Muslim
 *   community autonomy and religious self-governance; simultaneously, it
 *   enables and enforces gender-asymmetric rights (unilateral talaq,
 *   polygamy, differential inheritance) that fall disproportionately on women
 *   and dissidents. The founding problem — preserving Muslim community
 *   autonomy in a secular nation-state — was real in 1947 and remains
 *   contested today. The resolution mechanism (Shariat interpretation by
 *   qazis and boards) has evolved into a gatekeeping function that benefits
 *   institutional authorities and systematically excludes reform voices from
 *   within Islam. This story models the constraint from the Shariat-reading
 *   frame: the marriage authority derives legitimacy from religious text and
 *   tradition, not from state authorization or individual consent — that is
 *   the defining axiom of this reading.
 *
 * KEY AGENTS:
 *   - muslim_women: structurally targeted; identity-locked to jurisdiction; unequal divorce and inheritance rights
 *   - muslim_men: beneficiary; unilateral talaq, polygamy, inheritance advantage; constrained but advantaged within the system
 *   - qazi_institutional_authority: agenda-setter; interprets and enforces Shariat; controls adjudication monopoly
 *   - personal_law_board_authority: agenda-setter and beneficiary; negotiates state recognition; controls institutional interpretation
 *   - hindu_majority_state: observer; balances personal law autonomy against constitutional equality; unable to fully enforce its own equality commitments
 *   - reform_movements_within_islam: structurally excluded; denied standing in qazi authority and personal law board interpretation
 *   - secular_civil_code_advocates: excluded; challenging the constraint requires constitutional amendment or majoritarian politics
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(marriage_authority_kernel__muslim_shariat_reading, 0.68).
domain_priors:suppression_score(marriage_authority_kernel__muslim_shariat_reading, 0.71).
domain_priors:theater_ratio(marriage_authority_kernel__muslim_shariat_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(marriage_authority_kernel__muslim_shariat_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(marriage_authority_kernel__muslim_shariat_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(marriage_authority_kernel__muslim_shariat_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(marriage_authority_kernel__muslim_shariat_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(marriage_authority_kernel__muslim_shariat_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(marriage_authority_kernel__muslim_shariat_reading, tangled_rope).
narrative_ontology:human_readable(marriage_authority_kernel__muslim_shariat_reading, "Shariat-Based Marriage Authority via Personal Law Boards and Qazis").
narrative_ontology:topic_domain(marriage_authority_kernel__muslim_shariat_reading, "legal/constitutional/religious").

domain_priors:requires_active_enforcement(marriage_authority_kernel__muslim_shariat_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(marriage_authority_kernel__muslim_shariat_reading, '4d0039f4-3a25-4ce1-9195-be4f7054897f').
narrative_ontology:cs_kernel_codification('4d0039f4-3a25-4ce1-9195-be4f7054897f', fixed_text).
narrative_ontology:cs_authority_grounding('4d0039f4-3a25-4ce1-9195-be4f7054897f', extraction).
narrative_ontology:cs_interpretation_layer_present('4d0039f4-3a25-4ce1-9195-be4f7054897f').
narrative_ontology:cs_reading_relation('4d0039f4-3a25-4ce1-9195-be4f7054897f', marriage_authority_kernel__hindu_codified_reading, coexists_with).
narrative_ontology:cs_reading_relation('4d0039f4-3a25-4ce1-9195-be4f7054897f', marriage_authority_kernel__christian_canonical_reading, coexists_with).
narrative_ontology:cs_reading_relation('4d0039f4-3a25-4ce1-9195-be4f7054897f', marriage_authority_kernel__parsi_communal_reading, coexists_with).
narrative_ontology:cs_reading_relation('4d0039f4-3a25-4ce1-9195-be4f7054897f', marriage_authority_kernel__secular_civil_reading, influences).
narrative_ontology:cs_axiom('4d0039f4-3a25-4ce1-9195-be4f7054897f', foundational, shariat_divine_immutability).
narrative_ontology:cs_axiom_status(shariat_divine_immutability, holdable).
narrative_ontology:cs_axiom_grounding('4d0039f4-3a25-4ce1-9195-be4f7054897f', shariat_divine_immutability, deontological).
narrative_ontology:cs_axiom('4d0039f4-3a25-4ce1-9195-be4f7054897f', secondary, community_adjudication_authenticity).
narrative_ontology:cs_axiom_status(community_adjudication_authenticity, holdable).
narrative_ontology:cs_axiom_grounding('4d0039f4-3a25-4ce1-9195-be4f7054897f', community_adjudication_authenticity, conventional).
narrative_ontology:cs_reference_frame('4d0039f4-3a25-4ce1-9195-be4f7054897f', shariat_grounded_community_autonomy).
narrative_ontology:cs_drift_state('4d0039f4-3a25-4ce1-9195-be4f7054897f', contemporary_human_rights_contestation_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('4d0039f4-3a25-4ce1-9195-be4f7054897f', '').
narrative_ontology:cs_kernel_id(marriage_authority_kernel__muslim_shariat_reading, marriage_authority_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(marriage_authority_kernel__muslim_shariat_reading, muslim_men).
narrative_ontology:constraint_beneficiary(marriage_authority_kernel__muslim_shariat_reading, qazi_institutional_authority).
narrative_ontology:constraint_beneficiary(marriage_authority_kernel__muslim_shariat_reading, personal_law_board_authority).
narrative_ontology:constraint_victim(marriage_authority_kernel__muslim_shariat_reading, muslim_women).
narrative_ontology:constraint_victim(marriage_authority_kernel__muslim_shariat_reading, religious_minorities_within_community).
narrative_ontology:constraint_victim(marriage_authority_kernel__muslim_shariat_reading, secular_justice_seekers).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(marriage_authority_kernel__muslim_shariat_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(marriage_authority_kernel__muslim_shariat_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(marriage_authority_kernel__muslim_shariat_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(marriage_authority_kernel__muslim_shariat_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(marriage_authority_kernel__muslim_shariat_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises from 0.45 to 0.68 over the interval (t=0 to t=70). The initial rise (t=0 to t=50) models the growing awareness of gender-asymmetric rights and the personal law board's increasing institutional capture of community voice. The plateau at t=50+ reflects stabilization: extractiveness is maintained at high level, but further accumulation is arrested by the political/constitutional stalemate — neither secular civil code nor major reform has dislodged personal law authority. Theater ratio rises from 0.25 to 0.42 and plateaus, reflecting increasing performative invocation of 'community protection' while the real function has shifted toward institutional authority maintenance and gender-asymmetric rights preservation. Suppression requirement follows extractiveness: it must rise actively to exclude reform voices and secular justice seekers from adjudication, and must be maintained high to prevent state courts from intervening. The constraint is claimed as tangled_rope because it has a genuine coordination function (Muslim community autonomy and religious self-governance) AND asymmetric extraction (gender-unequal rights, institutional gatekeeping). Both properties are necessary to the structure: remove the extraction and the coordination framing collapses; remove the coordination framing and the gender asymmetry stands naked as a snare.
 *
 * PERSPECTIVAL GAP:
 *   From the personal law board and qazi frame: this is a legitimate coordination mechanism protecting minority community autonomy and religious self-determination against secular majority imposition. From the muslim_women and reform_movement frame: this is an enforced extraction mechanism using religious authority as cover for gender inequality and institutional gatekeeping. From the state's frame: this is a constitutional compromise that has become a site of irreconcilable tension between two commitments (personal law autonomy and individual equality). The engine should compute sharply different type classifications at each seat: institutional beneficiaries (board/qazi) compute as rope or coordinator; women and reformists compute as snare; the state computes as torn between coordination and extraction depending on which seat it privileges. The stakeholder surface and the structural data (beneficiary/victim declarations) enable the engine to derive these seat-level divergences automatically.
 *
 * DIRECTIONALITY LOGIC:
 *   Muslim_women are targets (d near 1.0): subject to personal law jurisdiction involuntarily (identity-locked exit), bear costs of gender-unequal rights, receive no benefit from the institutional structure. Qazi and personal law board are beneficiaries (d near 0.0): collect authority, gatekeeping rents, institutional recognition. Muslim_men are intermediate (d ~0.4-0.5): constrained by community reputation and religious obligation, but receive clear advantages (talaq, polygamy, inheritance). Reform movements and secular advocates are excluded (d near 1.0): they bear the cost of being shut out of adjudication while targets of the constraint's enforcement. The state is analytical (d=0.5): torn between honoring personal law autonomy and enforcing constitutional equality. No directionality overrides needed; the structural data (identity-locked exit for women, arbitrary gatekeeping for reformists, institutional authority for boards/qazis) drives the correct d values from the derivation chain.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem was genuine and live: Muslim community autonomy in a secular nation was real jeopardy under colonial and early-postcolonial law. The coordination solution was appropriate: reserve personal law to community authority. However, the mandate has become contested — is the constraint still necessary for community autonomy, or has it become a vehicle for institutional rent-collection by qazis and boards? The measurement series shows extractiveness and theater ratio rising and plateauing, which is consistent with mandatrophy at the institutional level (the boards/qazis have captured the coordination function for their own benefit) while the founding problem's status is contested (reformists say it is dead; traditionalists say it is live). The tangled_rope classification prevents misreading this as pure rope (coordination without extraction) or pure snare (extraction with false coordination cover); instead, it models the constraint as genuinely providing coordination AND genuinely extracting, with the extraction riding on the coordination function. The mandatrophy question ('is the founding problem still live or has the arrangement outlived its function?') cannot be resolved within the constraint story itself — it depends on political/constitutional choices about what Muslim community autonomy means in modern India, and those choices belong to the political system, not to the constraint analyst.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    shariat_interpretation_authenticity,
    'What counts as authentic interpretation of Shariat? Are personal law boards and qazis the sole legitimate interpreters, or are reform movements within Islam (women''s rights advocates, modernist scholars) equally authentic interpreters of Shariat?',
    'Empirical: survey Muslim scholars inside and outside the board on what interpretive methods and authorities they recognize as legitimate. Conceptual: the question depends on whether Shariat is treated as a fixed text (single interpretation) or as a living tradition (multiple interpretations). Does the constraint embody the reading''s true commitment to Shariat, or does it embody institutional gatekeeping disguised as religious authenticity?',
    'If reform movements are deemed equally authentic interpreters, personal law board authority is delegitimized and qazi monopoly collapses — the constraint reclassifies from tangled_rope toward a contested multiplicity or toward snare (institutional gatekeeping without coordination). If boards/qazis hold sole authenticity, the constraint holds as tangled_rope.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(shariat_interpretation_authenticity, conceptual, 'Whether the constraint''s interpretation monopoly is essential to Shariat authenticity or an institutional capture mechanism.').

omega_variable(
    identity_lock_mechanism,
    'Is the measured suppression of muslim_women a structural feature of the constraint (external barriers: state recognition of personal law jurisdiction, lack of secular remedy routes) or an internalized feature (women''s own identity fusion with Islam and family, making exit psychologically unavailable)?',
    'Post-exit trajectory study: do women who exit the jurisdiction (migrate, change legal status) report suppression relief suggesting structural lock, or do suppression dynamics persist suggesting internalized identity lock? Does suppression abate after legal remedy access is expanded?',
    'If structural: the constraint''s effective suppression is the state-recognized jurisdictional exclusion; expanding secular remedy access would reduce suppression. If internalized: the constraint has locked women into a religious identity that carries the suppression internally; remedy requires identity-level intervention (consciousness-raising, community reframing) that the constraint cannot address.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_mechanism, empirical, 'Whether suppression of women is structural (jurisdictional) or internalized (identity-fused).').

omega_variable(
    secular_majority_imposition_risk,
    'Is the founding problem (Muslim community subjection to secular majority law) genuinely live, or has the constraint become a vehicle for institutional gatekeeping that uses the threat of majority imposition as a cover story?',
    'Institutional analysis: would a Uniform Civil Code, if enacted, be applied uniformly to all citizens or selectively to Muslims? Comparative: in secular nation-states with religious minorities, does uniform law entail cultural subjection or religious pluralism compatible with diverse internal practices? Interview Muslims outside institutional authority about whether they fear secular law or fear institutional gatekeeping by boards/qazis.',
    'If majority imposition is real and live: the constraint''s coordination function is legitimate and essential; secular remedy would dissolve Muslim community. If institutional gatekeeping is the real threat: the constraint protects institutional power, not community autonomy; secular remedy would enable community autonomy by removing gatekeeping.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(secular_majority_imposition_risk, empirical, 'Whether the founding problem remains live or has been replaced by institutional gatekeeping as the actual threat.').

omega_variable(
    gender_asymmetry_necessity,
    'Are the gender-asymmetric rights (unilateral talaq, polygamy, inheritance inequality) essential to the Shariat-reading''s core identity, or are they contingent features that could be reformed without losing the reading''s integrity?',
    'Interpretive: Islamic legal scholarship shows numerous gender-egalitarian reinterpretations of Shariat (e.g., talaq conditions, polygamy restrictions, inheritance equalization). Are these reinterpretations still Shariat-reading, or do they cross into secular_civil_reading territory? Empirical: survey Muslim practitioners: do they regard gender equality as incompatible with Shariat, or as a legitimate reinterpretation?',
    'If gender asymmetry is essential: the constraint''s extraction component (gender-unequal rights) is inseparable from its coordination component (Shariat authority); reform would require abandoning the reading. If contingent: gender equity could be achieved within Shariat-reading by institutional reform; extraction could be reduced without eliminating the constraint.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(gender_asymmetry_necessity, conceptual, 'Whether gender inequality is intrinsic to Shariat-reading or a contingent institutional interpretation.').

omega_variable(
    kernel_reading_vs_plural_equilibrium,
    'Is the five-reading kernel structure itself the constraint, or is the constraint only this Shariat-reading and its interaction with the other readings?',
    'Analytical: a single constraint story models one reading in isolation. The plural equilibrium (all five readings coexisting) is a different constraint — the meta-constraint ''constitutional pluralism arrangement''. This omega is conceptual, not empirical: are we modeling the Shariat-reading''s internal structure, or the Shariat-reading''s role in a plural system?',
    'If the Shariat-reading is the unit: the constraint models marriage authority grounded in Shariat and Shariat interpretation. If the plural system is the unit: the constraint models how the state manages multiple readings and distributes jurisdictional authority. The two have different beneficiary/victim structures and different classifications.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_vs_plural_equilibrium, conceptual, 'Whether the constraint unit is a single reading or a plural kernel structure.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(marriage_authority_kernel__muslim_shariat_reading, 0, 70).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(marr_tr_t0, marriage_authority_kernel__muslim_shariat_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(marr_tr_t10, marriage_authority_kernel__muslim_shariat_reading, theater_ratio, 10, 0.28).
narrative_ontology:measurement(marr_tr_t20, marriage_authority_kernel__muslim_shariat_reading, theater_ratio, 20, 0.32).
narrative_ontology:measurement(marr_tr_t30, marriage_authority_kernel__muslim_shariat_reading, theater_ratio, 30, 0.37).
narrative_ontology:measurement(marr_tr_t40, marriage_authority_kernel__muslim_shariat_reading, theater_ratio, 40, 0.4).
narrative_ontology:measurement(marr_tr_t50, marriage_authority_kernel__muslim_shariat_reading, theater_ratio, 50, 0.42).
narrative_ontology:measurement(marr_tr_t60, marriage_authority_kernel__muslim_shariat_reading, theater_ratio, 60, 0.42).
narrative_ontology:measurement(marr_tr_t70, marriage_authority_kernel__muslim_shariat_reading, theater_ratio, 70, 0.42).

% Extraction over time
narrative_ontology:measurement(marr_be_t0, marriage_authority_kernel__muslim_shariat_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(marr_be_t10, marriage_authority_kernel__muslim_shariat_reading, base_extractiveness, 10, 0.51).
narrative_ontology:measurement(marr_be_t20, marriage_authority_kernel__muslim_shariat_reading, base_extractiveness, 20, 0.58).
narrative_ontology:measurement(marr_be_t30, marriage_authority_kernel__muslim_shariat_reading, base_extractiveness, 30, 0.63).
narrative_ontology:measurement(marr_be_t40, marriage_authority_kernel__muslim_shariat_reading, base_extractiveness, 40, 0.66).
narrative_ontology:measurement(marr_be_t50, marriage_authority_kernel__muslim_shariat_reading, base_extractiveness, 50, 0.68).
narrative_ontology:measurement(marr_be_t60, marriage_authority_kernel__muslim_shariat_reading, base_extractiveness, 60, 0.68).
narrative_ontology:measurement(marr_be_t70, marriage_authority_kernel__muslim_shariat_reading, base_extractiveness, 70, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(marr_su_t0, marriage_authority_kernel__muslim_shariat_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(marr_su_t10, marriage_authority_kernel__muslim_shariat_reading, suppression_requirement, 10, 0.6).
narrative_ontology:measurement(marr_su_t20, marriage_authority_kernel__muslim_shariat_reading, suppression_requirement, 20, 0.64).
narrative_ontology:measurement(marr_su_t30, marriage_authority_kernel__muslim_shariat_reading, suppression_requirement, 30, 0.67).
narrative_ontology:measurement(marr_su_t40, marriage_authority_kernel__muslim_shariat_reading, suppression_requirement, 40, 0.7).
narrative_ontology:measurement(marr_su_t50, marriage_authority_kernel__muslim_shariat_reading, suppression_requirement, 50, 0.71).
narrative_ontology:measurement(marr_su_t60, marriage_authority_kernel__muslim_shariat_reading, suppression_requirement, 60, 0.71).
narrative_ontology:measurement(marr_su_t70, marriage_authority_kernel__muslim_shariat_reading, suppression_requirement, 70, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(marriage_authority_kernel__muslim_shariat_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(marriage_authority_kernel__muslim_shariat_reading, 0.12).
narrative_ontology:affects_constraint(marriage_authority_kernel__muslim_shariat_reading, marriage_authority_kernel__hindu_codified_reading).
narrative_ontology:affects_constraint(marriage_authority_kernel__muslim_shariat_reading, marriage_authority_kernel__christian_canonical_reading).
narrative_ontology:affects_constraint(marriage_authority_kernel__muslim_shariat_reading, marriage_authority_kernel__parsi_communal_reading).
narrative_ontology:affects_constraint(marriage_authority_kernel__muslim_shariat_reading, marriage_authority_kernel__secular_civil_reading).

% DUAL FORMULATION NOTE:
% This story is one of five kernel readings of the marriage_authority_kernel. The kernel represents the constitutional settlement allocating authority over marriage and family law in India. This reading (Shariat-based, qazi-administered) coexists with four sibling readings, each grounding authority in a different source (Hindu codified law, Christian canon law, Parsi custom, secular civil code). The readings do not foreclose one another — different religious communities instantiate different readings simultaneously. The inter-reading relationships (coexists_with, influences) model how the readings interact in the plural system. Each reading has a distinct beneficiary/victim structure: this reading concentrates institutional authority in personal law boards and qazis, and concentrates gender-asymmetric rights benefits in men. The secular_civil_reading, by contrast, distributes authority to civil courts and equalizes gender rights. The two readings influence each other: pressure from the secular reading creates pressure on the Shariat reading to prove gender equity within Shariat; resistance from the Shariat reading creates constitutional precedent shielding community autonomy from secular law. The full constraint family enables the system to model plural legal authority and its distributional consequences.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(marriage_authority_kernel__muslim_shariat_reading, organized, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
