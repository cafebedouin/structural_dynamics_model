% ============================================================================
% CONSTRAINT STORY: plural_marriage_mandate__exogenous_override_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_plural_marriage_mandate__exogenous_override_reading, []).

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
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: plural_marriage_mandate__exogenous_override_reading
 *   human_readable: 1890 Manifesto as Federal Coercive Override of Divine Mandate
 *   domain: religious_institutional_history/political_theology
 *
 * SUMMARY:
 *   The 1890 Manifesto (Official Declaration 1) is read here as the terminal
 *   event in a 42-year federal coercive campaign (Morrill Act 1862 → Poland
 *   Act 1874 → Edmunds Act 1882 → Edmunds-Tucker Act 1887) that used
 *   imprisonment, property seizure, disfranchisement, and corporate
 *   dissolution to force the LDS Church to abandon plural marriage. The
 *   reading holds that the Manifesto's revelation narrative is a cover story
 *   constructed under duress; the constraint is the federal coercive
 *   apparatus itself, which extracts religious liberty and ecclesiastical
 *   autonomy from the church and its polygamist members for the benefit of
 *   federal territorial control and Protestant civilizational hegemony. This
 *   reading forecloses the endogenous reinterpretation reading (divine
 *   suspension) because the two premises — 'God changed the requirement' vs.
 *   'Federal guns changed the requirement' — cannot both be true in a single
 *   framework.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(plural_marriage_mandate__exogenous_override_reading, 0.82).
domain_priors:suppression_score(plural_marriage_mandate__exogenous_override_reading, 0.88).
domain_priors:theater_ratio(plural_marriage_mandate__exogenous_override_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(plural_marriage_mandate__exogenous_override_reading, extractiveness, 0.82).
narrative_ontology:constraint_metric(plural_marriage_mandate__exogenous_override_reading, suppression_requirement, 0.88).
narrative_ontology:constraint_metric(plural_marriage_mandate__exogenous_override_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(plural_marriage_mandate__exogenous_override_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(plural_marriage_mandate__exogenous_override_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(plural_marriage_mandate__exogenous_override_reading, snare).
narrative_ontology:human_readable(plural_marriage_mandate__exogenous_override_reading, "1890 Manifesto as Federal Coercive Override of Divine Mandate").
narrative_ontology:topic_domain(plural_marriage_mandate__exogenous_override_reading, "religious_institutional_history/political_theology").

domain_priors:requires_active_enforcement(plural_marriage_mandate__exogenous_override_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(plural_marriage_mandate__exogenous_override_reading, '97cca724-5e5a-408f-a5d1-d39fba3b2634').
narrative_ontology:cs_kernel_codification('97cca724-5e5a-408f-a5d1-d39fba3b2634', fixed_text).
narrative_ontology:cs_authority_grounding('97cca724-5e5a-408f-a5d1-d39fba3b2634', lineage).
narrative_ontology:cs_interpretation_layer_present('97cca724-5e5a-408f-a5d1-d39fba3b2634').
narrative_ontology:cs_reading_relation('97cca724-5e5a-408f-a5d1-d39fba3b2634', plural_marriage_mandate__endogenous_reinterpretation_reading, forecloses).
narrative_ontology:cs_reading_relation('97cca724-5e5a-408f-a5d1-d39fba3b2634', plural_marriage_mandate__institutional_pragmatism_reading, coexists_with).
narrative_ontology:cs_axiom('97cca724-5e5a-408f-a5d1-d39fba3b2634', foundational, federal_coercion_drove_manifesto).
narrative_ontology:cs_axiom_status(federal_coercion_drove_manifesto, holdable).
narrative_ontology:cs_axiom_grounding('97cca724-5e5a-408f-a5d1-d39fba3b2634', federal_coercion_drove_manifesto, empirically_contingent).
narrative_ontology:cs_axiom('97cca724-5e5a-408f-a5d1-d39fba3b2634', foundational, divine_requirement_not_suspended).
narrative_ontology:cs_axiom_status(divine_requirement_not_suspended, holdable).
narrative_ontology:cs_axiom_grounding('97cca724-5e5a-408f-a5d1-d39fba3b2634', divine_requirement_not_suspended, deontological).
narrative_ontology:cs_reference_frame('97cca724-5e5a-408f-a5d1-d39fba3b2634', divine_mandate_perpetual_binding).
narrative_ontology:cs_drift_state('97cca724-5e5a-408f-a5d1-d39fba3b2634', post_manifesto_1890, gap(authority_erosion, severe, false)).
narrative_ontology:cs_created_at('97cca724-5e5a-408f-a5d1-d39fba3b2634', '').
narrative_ontology:cs_kernel_id(plural_marriage_mandate__exogenous_override_reading, plural_marriage_mandate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(plural_marriage_mandate__exogenous_override_reading, federal_government).
narrative_ontology:constraint_beneficiary(plural_marriage_mandate__exogenous_override_reading, anti_polygamy_crusaders).
narrative_ontology:constraint_victim(plural_marriage_mandate__exogenous_override_reading, practicing_polygamists).
narrative_ontology:constraint_victim(plural_marriage_mandate__exogenous_override_reading, polygamist_families).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(plural_marriage_mandate__exogenous_override_reading, non_polygamist_mormons).
narrative_ontology:constraint_victim(plural_marriage_mandate__exogenous_override_reading, church_leadership).
narrative_ontology:constraint_victim(plural_marriage_mandate__exogenous_override_reading, non_polygamist_mormons).
narrative_ontology:constraint_vindicates(plural_marriage_mandate__exogenous_override_reading, federal_sovereignty_over_territories).
narrative_ontology:constraint_vindicates(plural_marriage_mandate__exogenous_override_reading, monogamy_as_civilizational_standard).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Enforces anti-polygamy laws (Edmunds Act, Edmunds-Tucker Act) through imprisonment, property seizure, disfranchisement, and threat of church corporate dissolution. Achieves territorial conformity for Utah statehood. Collects political legitimacy and territorial control.
narrative_ontology:constraint_stakeholder(plural_marriage_mandate__exogenous_override_reading, federal_government, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(plural_marriage_mandate__exogenous_override_reading, federal_government, beneficiary).

% Protestant reformers, women's organizations, and congressional allies who campaigned for federal suppression. Gain moral authority and legislative victories; their framing of polygamy as 'barbarism' becomes settled public orthodoxy.
narrative_ontology:constraint_stakeholder(plural_marriage_mandate__exogenous_override_reading, anti_polygamy_crusaders, beneficiary,
    organized, biographical, mobile, national).

% Men and women in plural marriages face federal prosecution, imprisonment (1,300+ convictions), property confiscation, loss of civil rights, and forced underground existence. Their religious identity fuses with the practice; exit means abandoning marriages, children, and eternal covenants they believe are divinely required.
narrative_ontology:constraint_stakeholder(plural_marriage_mandate__exogenous_override_reading, practicing_polygamists, payer,
    powerless, biographical, identity_locked, local).

% Wives and children in plural households bear economic ruin when husbands are imprisoned or flee, social stigma, loss of inheritance rights, and forced dispersal. Children lose fathers; wives lose legal recognition and support. No exit without dissolving the family unit.
narrative_ontology:constraint_stakeholder(plural_marriage_mandate__exogenous_override_reading, polygamist_families, payer,
    powerless, generational, trapped, local).

% First Presidency and Quorum of Twelve initially defend the mandate as divine, then issue the 1890 Manifesto under threat of total institutional destruction (temple seizure, corporate dissolution, leadership imprisonment). They lose control of the narrative and face schism from fundamentalist breakaways.
narrative_ontology:constraint_stakeholder(plural_marriage_mandate__exogenous_override_reading, church_leadership, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(plural_marriage_mandate__exogenous_override_reading, church_leadership, payer).

% Majority of Latter-day Saints not in plural marriage. Gain church survival, eventual statehood, and social normalization. Bear stigma by association and internalize the Manifesto as surrender rather than revelation.
narrative_ontology:constraint_stakeholder(plural_marriage_mandate__exogenous_override_reading, non_polygamist_mormons, beneficiary,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(plural_marriage_mandate__exogenous_override_reading, non_polygamist_mormons, payer).

% Territorial and Supreme Court justices (e.g., Reynolds v. United States, Late Corp. of the Church v. United States) uphold anti-polygamy statutes as constitutional exercises of territorial police power. Their rulings legitimize the coercive framework.
narrative_ontology:constraint_stakeholder(plural_marriage_mandate__exogenous_override_reading, federal_judiciary, observer,
    institutional, generational, analytical, national).

% Post-Manifesto polygamists (Woolley, Musser, Allred groups) who reject the Manifesto as coercion-induced apostasy. Excommunicated, marginalized, and prosecuted by both church and state. Would object to any reading that legitimizes the Manifesto; their voices are absent from official church history.
narrative_ontology:constraint_stakeholder(plural_marriage_mandate__exogenous_override_reading, fundamentalist_dissenters, excluded,
    powerless, generational, identity_locked, local).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: None — the arrangement does not solve a coordination problem for the practicing polygamists; it imposes federal territorial conformity through coercion. The church leadership's 'coordination' is damage control under duress.
% TRANSFER_FUNCTION: Moves religious liberty, property, family integrity, and ecclesiastical autonomy from practicing polygamists and the church institution to the federal government and its reformist allies, as the price of Utah statehood and church corporate survival.
% ABSENT_VOICES: Fundamentalist dissenters (post-Manifesto polygamists) and the women and children in plural families who left no written record — their objection would be that the Manifesto was capitulation, not revelation. They are excluded from both the church's official narrative and the federal record.
% DISAPPEARANCE_RATIONALE: If the Manifesto and its enforcement vanished in 1890, plural marriage would continue openly; the church would face institutional destruction but the divine mandate would persist; federal territorial policy would require new coercive machinery; the entire Utah statehood trajectory rewrites.
% FOUNDING_PROBLEM: The federal government sought to eliminate polygamy as a barrier to Utah statehood and as an affront to monogamous republican civilization; the church sought institutional survival against existential legal assault.
% FOUNDING_PROBLEM_CORROBORATION: Federal congressional records (Edmunds Act debates, Edmunds-Tucker Act passage) and Supreme Court opinions (Reynolds, Late Corp.) corroborate the federal intent to eradicate polygamy by force. The church's own 1891-1904 internal minutes (released 2017) show leadership acknowledging the Manifesto was issued under 'overwhelming pressure' to prevent 'utter destruction.' No corroborating source outside the benefiting parties attests a divine revelation suspending the mandate.
narrative_ontology:disappearance_verdict(plural_marriage_mandate__exogenous_override_reading, world_rearranges).
narrative_ontology:founding_problem_status(plural_marriage_mandate__exogenous_override_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(plural_marriage_mandate__exogenous_override_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(plural_marriage_mandate__exogenous_override_reading, 'none', 1).
narrative_ontology:epsilon_provenance(plural_marriage_mandate__exogenous_override_reading, 0.82, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(plural_marriage_mandate__exogenous_override_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(plural_marriage_mandate__exogenous_override_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(plural_marriage_mandate__exogenous_override_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.82) is high because the constraint strips practicing polygamists of liberty, property, family integrity, and civil rights while transferring territorial sovereignty and moral authority to the federal government. Suppression (0.88) is near-maximum: the constraint persists only through active enforcement (1,300+ convictions, temple seizure, corporate dissolution). Theater ratio (0.25) is low but non-zero: the revelation narrative performs a legitimating function for the church's survival. Accessibility collapse (0.72) is high: once the federal coercive machinery is understood, no exit exists for identity-locked polygamists short of abandoning their eternal covenants. Resistance (0.68) is substantial: the church resisted for decades through legal appeals, civil disobedience, and the 1890-1904 transition period before the Second Manifesto enforced compliance.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats (practicing_polygamists, polygamist_families) experience this as a snare — coercive extraction with no coordination function for them. The agenda_setter seats (federal_government, church_leadership post-1890) may frame it as scaffold (transitional) or rope (coordination of statehood). The engine computes this divergence from the structural data; the exogenous_override_reading asserts the payer-seat classification is the truth of the constraint.
 *
 * DIRECTIONALITY LOGIC:
 *   Federal government and anti-polygamy crusaders are structural beneficiaries (d ~ 0.1-0.2): they gain territorial conformity, statehood, and moral hegemony. Practicing polygamists and their families are full targets (d ~ 0.95): identity-locked, trapped, bearing the full extraction. Church leadership sits at d ~ 0.6: initially targets of coercion, then agenda-setters of the surrender narrative, bearing institutional extraction (loss of temples, corporate assets, narrative control). Non-polygamist Mormons are near-symmetric (d ~ 0.5): gain survival/statehood, bear stigma. Fundamentalist dissenters are excluded from the conversation entirely.
 *
 * MANDATROPHY ANALYSIS:
 *   The original divine mandate (plural marriage as exaltation requirement) is dead — the founding problem (establishing a covenant people) is re-read as superseded by the church itself, but only under coercion. The arrangement persists as extraction: the church retains the revelation narrative to legitimate its survival, while the federal government retains the legal framework to police residual polygamy. This is mandatrophy resolved as snare: the mandate's function atrophied under coercion, leaving a constraint that extracts compliance without delivering the promised coordination.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    coercion_vs_revelation_epistemic_access,
    'Can the historical record distinguish between a genuine prophetic revelation received under pressure versus a fabricated revelation narrative constructed to legitimate capitulation?',
    'Analysis of contemporary leadership diaries (Woodruff, Cannon, Smith), the 1890-1891 letterpress copybooks, and the 1904 Smoot hearings testimony for evidence of intentional framing vs. sincere conviction.',
    'If the revelation narrative is sincere, the constraint shifts toward scaffold (transitional divine suspension); if fabricated, it remains snare (coercive extraction masked as voluntary compliance).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coercion_vs_revelation_epistemic_access, conceptual, 'Epistemic access to leadership intent vs. sincere belief under duress').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (federal imprisonment, property seizure) or internalized (polygamists'' belief that resistance is spiritually forbidden after the Manifesto)?',
    'Post-Manifesto suppression trajectory: if practicing polygamists continued resisting until physical enforcement (1890-1904 Second Manifesto era), suppression is primarily structural; if resistance collapsed immediately after the Manifesto, internalized suppression is significant.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests — the target carries the suppression with them after the formal coercive apparatus relaxes.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism in the 1890-1904 transition').

omega_variable(
    federal_motive_purity,
    'Was federal suppression motivated purely by anti-polygamy moral conviction, or did it serve broader territorial conquest and resource extraction aims?',
    'Congressional debate analysis, railroad and mining interest lobbying records, and the timing of Utah statehood (1896) relative to the Manifesto (1890) and Second Manifesto (1904).',
    'If federal motive included territorial/resource extraction, the beneficiary set expands and the extraction is more clearly predatory; if purely moral, the constraint edges toward rope (coordination of civilizational standards).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(federal_motive_purity, empirical, 'Purity of federal motive: moral crusade vs. territorial conquest').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(plural_marriage_mandate__exogenous_override_reading, 1862, 1904).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pmm_eor_tr_t1862, plural_marriage_mandate__exogenous_override_reading, theater_ratio, 1862, 0.05).
narrative_ontology:measurement(pmm_eor_tr_t1874, plural_marriage_mandate__exogenous_override_reading, theater_ratio, 1874, 0.1).
narrative_ontology:measurement(pmm_eor_tr_t1882, plural_marriage_mandate__exogenous_override_reading, theater_ratio, 1882, 0.15).
narrative_ontology:measurement(pmm_eor_tr_t1887, plural_marriage_mandate__exogenous_override_reading, theater_ratio, 1887, 0.22).
narrative_ontology:measurement(pmm_eor_tr_t1890, plural_marriage_mandate__exogenous_override_reading, theater_ratio, 1890, 0.3).
narrative_ontology:measurement(pmm_eor_tr_t1904, plural_marriage_mandate__exogenous_override_reading, theater_ratio, 1904, 0.25).

% Extraction over time
narrative_ontology:measurement(pmm_eor_be_t1862, plural_marriage_mandate__exogenous_override_reading, base_extractiveness, 1862, 0.35).
narrative_ontology:measurement(pmm_eor_be_t1874, plural_marriage_mandate__exogenous_override_reading, base_extractiveness, 1874, 0.52).
narrative_ontology:measurement(pmm_eor_be_t1882, plural_marriage_mandate__exogenous_override_reading, base_extractiveness, 1882, 0.68).
narrative_ontology:measurement(pmm_eor_be_t1887, plural_marriage_mandate__exogenous_override_reading, base_extractiveness, 1887, 0.81).
narrative_ontology:measurement(pmm_eor_be_t1890, plural_marriage_mandate__exogenous_override_reading, base_extractiveness, 1890, 0.85).
narrative_ontology:measurement(pmm_eor_be_t1904, plural_marriage_mandate__exogenous_override_reading, base_extractiveness, 1904, 0.82).

% Suppression requirement over time
narrative_ontology:measurement(pmm_eor_su_t1862, plural_marriage_mandate__exogenous_override_reading, suppression_requirement, 1862, 0.3).
narrative_ontology:measurement(pmm_eor_su_t1874, plural_marriage_mandate__exogenous_override_reading, suppression_requirement, 1874, 0.55).
narrative_ontology:measurement(pmm_eor_su_t1882, plural_marriage_mandate__exogenous_override_reading, suppression_requirement, 1882, 0.75).
narrative_ontology:measurement(pmm_eor_su_t1887, plural_marriage_mandate__exogenous_override_reading, suppression_requirement, 1887, 0.88).
narrative_ontology:measurement(pmm_eor_su_t1890, plural_marriage_mandate__exogenous_override_reading, suppression_requirement, 1890, 0.9).
narrative_ontology:measurement(pmm_eor_su_t1904, plural_marriage_mandate__exogenous_override_reading, suppression_requirement, 1904, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(plural_marriage_mandate__exogenous_override_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(plural_marriage_mandate__exogenous_override_reading, 0.1).
narrative_ontology:affects_constraint(plural_marriage_mandate__exogenous_override_reading, plural_marriage_mandate__endogenous_reinterpretation_reading).
narrative_ontology:affects_constraint(plural_marriage_mandate__exogenous_override_reading, plural_marriage_mandate__institutional_pragmatism_reading).
narrative_ontology:affects_constraint(plural_marriage_mandate__exogenous_override_reading, utah_statehood_compact).
narrative_ontology:affects_constraint(plural_marriage_mandate__exogenous_override_reading, federal_territorial_police_power).

% DUAL FORMULATION NOTE:
% The plural_marriage_mandate kernel decomposes into three readings with distinct ε values: endogenous_reinterpretation_reading (ε ≈ 0.15, claimed mountain/rope), exogenous_override_reading (ε ≈ 0.82, claimed snare), institutional_pragmatism_reading (ε ≈ 0.65, claimed tangled_rope). The exogenous reading has the highest extractiveness because it reads the coercive apparatus as the constraint itself, not the divine mandate. The endogenous reading reads the divine mandate as the constraint (low extraction, high naturalness). The pragmatism reading reads the church's strategic adaptation as the constraint (moderate extraction, active enforcement). They are linked via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(plural_marriage_mandate__exogenous_override_reading, institutional, 0.65).
constraint_indexing:directionality_override(plural_marriage_mandate__exogenous_override_reading, powerless, 0.95).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
