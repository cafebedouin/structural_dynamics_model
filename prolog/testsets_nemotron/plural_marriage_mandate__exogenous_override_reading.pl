% ============================================================================
% CONSTRAINT STORY: plural_marriage_mandate__exogenous_override_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-07
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
 *   human_readable: 1890 Manifesto as Federal Coercion Override of Plural Marriage Mandate
 *   domain: religious_institutional_history/commitment_systems/political_theology
 *
 * SUMMARY:
 *   This constraint story models the 1890 Manifesto as a federal coercion
 *   event that forced the LDS Church to abandon plural marriage — a practice
 *   the church taught was a divine requirement for exaltation — under threat
 *   of institutional destruction (temple seizure, corporate dissolution,
 *   leadership imprisonment). The reading treats the Manifesto not as
 *   legitimate prophetic reinterpretation but as an exogenous override: the
 *   constraint's persistence depended on active federal enforcement
 *   (imprisonment, property seizure, disenfranchisement), its beneficiaries
 *   were federal and anti-polygamy actors achieving territorial conformity,
 *   and its victims were practicing polygamists who faced a choice between
 *   religious obligation and physical survival. The claimed type is snare:
 *   coercive extraction masked by the theater of voluntary compliance and
 *   revelatory narrative.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(plural_marriage_mandate__exogenous_override_reading, 0.88).
domain_priors:suppression_score(plural_marriage_mandate__exogenous_override_reading, 0.92).
domain_priors:theater_ratio(plural_marriage_mandate__exogenous_override_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(plural_marriage_mandate__exogenous_override_reading, extractiveness, 0.88).
narrative_ontology:constraint_metric(plural_marriage_mandate__exogenous_override_reading, suppression_requirement, 0.92).
narrative_ontology:constraint_metric(plural_marriage_mandate__exogenous_override_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(plural_marriage_mandate__exogenous_override_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(plural_marriage_mandate__exogenous_override_reading, resistance, 0.85).

% --- Constraint claim ---
narrative_ontology:constraint_claim(plural_marriage_mandate__exogenous_override_reading, snare).
narrative_ontology:human_readable(plural_marriage_mandate__exogenous_override_reading, "1890 Manifesto as Federal Coercion Override of Plural Marriage Mandate").
narrative_ontology:topic_domain(plural_marriage_mandate__exogenous_override_reading, "religious_institutional_history/commitment_systems/political_theology").

domain_priors:requires_active_enforcement(plural_marriage_mandate__exogenous_override_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(plural_marriage_mandate__exogenous_override_reading, '7c681fc5-3b6e-4f2a-9171-5a968f4272bb').
narrative_ontology:cs_kernel_codification('7c681fc5-3b6e-4f2a-9171-5a968f4272bb', fixed_text).
narrative_ontology:cs_authority_grounding('7c681fc5-3b6e-4f2a-9171-5a968f4272bb', lineage).
narrative_ontology:cs_interpretation_layer_present('7c681fc5-3b6e-4f2a-9171-5a968f4272bb').
narrative_ontology:cs_reading_relation('7c681fc5-3b6e-4f2a-9171-5a968f4272bb', plural_marriage_mandate__endogenous_reinterpretation_reading, forecloses).
narrative_ontology:cs_reading_relation('7c681fc5-3b6e-4f2a-9171-5a968f4272bb', plural_marriage_mandate__institutional_pragmatism_reading, coexists_with).
narrative_ontology:cs_axiom('7c681fc5-3b6e-4f2a-9171-5a968f4272bb', foundational, manifesto_as_coercive_surrender_not_revelation).
narrative_ontology:cs_axiom_status(manifesto_as_coercive_surrender_not_revelation, holdable).
narrative_ontology:cs_axiom_grounding('7c681fc5-3b6e-4f2a-9171-5a968f4272bb', manifesto_as_coercive_surrender_not_revelation, empirically_contingent).
narrative_ontology:cs_axiom('7c681fc5-3b6e-4f2a-9171-5a968f4272bb', foundational, divine_requirement_cannot_be_legitimately_suspended_by_exogenous_force).
narrative_ontology:cs_axiom_status(divine_requirement_cannot_be_legitimately_suspended_by_exogenous_force, holdable).
narrative_ontology:cs_axiom_grounding('7c681fc5-3b6e-4f2a-9171-5a968f4272bb', divine_requirement_cannot_be_legitimately_suspended_by_exogenous_force, deontological).
narrative_ontology:cs_reference_frame('7c681fc5-3b6e-4f2a-9171-5a968f4272bb', pre_1890_plural_marriage_as_nonnegotiable_divine_law).
narrative_ontology:cs_drift_state('7c681fc5-3b6e-4f2a-9171-5a968f4272bb', post_manifesto_1890, gap(axiom_overriding, severe, false)).
narrative_ontology:cs_created_at('7c681fc5-3b6e-4f2a-9171-5a968f4272bb', '').
narrative_ontology:cs_kernel_id(plural_marriage_mandate__exogenous_override_reading, plural_marriage_mandate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(plural_marriage_mandate__exogenous_override_reading, federal_government).
narrative_ontology:constraint_beneficiary(plural_marriage_mandate__exogenous_override_reading, anti_polygamy_crusaders).
narrative_ontology:constraint_beneficiary(plural_marriage_mandate__exogenous_override_reading, utah_statehood_advocates).
narrative_ontology:constraint_victim(plural_marriage_mandate__exogenous_override_reading, practicing_polygamists).
narrative_ontology:constraint_victim(plural_marriage_mandate__exogenous_override_reading, polygamous_families).
narrative_ontology:constraint_victim(plural_marriage_mandate__exogenous_override_reading, fundamentalist_dissenters).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(plural_marriage_mandate__exogenous_override_reading, lds_institutional_leadership).
narrative_ontology:constraint_vindicates(plural_marriage_mandate__exogenous_override_reading, federal_supremacy_over_religious_practice).
narrative_ontology:constraint_vindicates(plural_marriage_mandate__exogenous_override_reading, territorial_governance_via_moral_regulation).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Used anti-polygamy legislation (Morrill Act 1862, Edmunds Act 1882, Edmunds-Tucker Act 1887) to force LDS Church compliance, achieving territorial conformity and Utah statehood on federal terms. Collected political capital, legal precedent, and territorial control.
narrative_ontology:constraint_stakeholder(plural_marriage_mandate__exogenous_override_reading, federal_government, beneficiary,
    institutional, generational, arbitrage, national).

% Protestant reformers, women's organizations, and politicians who mobilized anti-polygamy sentiment as a moral crusade. Achieved their stated objective of eliminating 'the twin relic of barbarism' and gained organizational legitimacy.
narrative_ontology:constraint_stakeholder(plural_marriage_mandate__exogenous_override_reading, anti_polygamy_crusaders, beneficiary,
    organized, biographical, mobile, national).

% Non-Mormon territorial officials, mining interests, and railroad companies who wanted Utah integrated into federal governance and capitalist markets. Polygamy was the legal barrier; its elimination unlocked statehood and economic development.
narrative_ontology:constraint_stakeholder(plural_marriage_mandate__exogenous_override_reading, utah_statehood_advocates, beneficiary,
    powerful, biographical, constrained, regional).

% LDS men and women who entered plural marriage as religious obligation. Faced federal imprisonment (1,300+ convictions), property seizure, disenfranchisement, and hiding on the 'underground.' Could not exit without abandoning what they believed was a divine commandment for exaltation.
narrative_ontology:constraint_stakeholder(plural_marriage_mandate__exogenous_override_reading, practicing_polygamists, payer,
    powerless, biographical, identity_locked, local).

% Wives and children in plural families who lost husbands/fathers to prison, faced economic destitution from property seizures, and endured social stigma. Had no independent exit option — their situation was structurally determined by the male head's religious commitment and federal targeting.
narrative_ontology:constraint_stakeholder(plural_marriage_mandate__exogenous_override_reading, polygamous_families, payer,
    powerless, biographical, trapped, local).

% Those who rejected the Manifesto as illegitimate and continued plural marriage, forming the basis of modern fundamentalist Mormon groups. Were excommunicated by the institutional church and prosecuted by the state — excluded from both the coercive power and the accommodating institution.
narrative_ontology:constraint_stakeholder(plural_marriage_mandate__exogenous_override_reading, fundamentalist_dissenters, excluded,
    powerless, generational, identity_locked, local).

% Church presidents (Young, Taylor, Woodruff) who initially defended plural marriage as non-negotiable, then issued the 1890 Manifesto under existential threat (temple seizure, corporate dissolution, leadership imprisonment). Paid institutional survival as the price; retained religious authority by framing capitulation as revelation.
narrative_ontology:constraint_stakeholder(plural_marriage_mandate__exogenous_override_reading, lds_institutional_leadership, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(plural_marriage_mandate__exogenous_override_reading, lds_institutional_leadership, payer).

% Scholars of Mormon history, religious liberty, and church-state relations who examine the Manifesto as a case study in coercive state power overriding religious practice, and the institutional adaptation that follows.
narrative_ontology:constraint_stakeholder(plural_marriage_mandate__exogenous_override_reading, historical_analyst, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Federal anti-polygamy law coordinated national moral regulation and territorial governance by suppressing a religious practice deemed incompatible with republican citizenship and property regimes.
% TRANSFER_FUNCTION: Transferred religious autonomy, family integrity, and property from practicing polygamists to the federal state; transferred political legitimacy and territorial control from the LDS Church to federal authorities.
% ABSENT_VOICES: Polygamous women's own theological reasoning (distinct from male leadership's) was largely absent from both federal proceedings and church councils. Fundamentalist dissenters who maintained the practice were excluded from the institutional church's reconciliation with the state.
% DISAPPEARANCE_RATIONALE: If the coercive federal framework vanished in 1890, plural marriage would have continued as lived practice; the LDS Church would not have surrendered its temples, corporate existence, or leadership freedom; Utah statehood would have been delayed or conditioned differently; modern fundamentalist movements would not exist as schismatic reactions to the Manifesto.
% FOUNDING_PROBLEM: The federal government needed to resolve the 'Mormon Question' — a theocratic territorial governance structure practicing plural marriage that challenged federal sovereignty, Protestant moral order, and capitalist property norms in the Intermountain West.
% FOUNDING_PROBLEM_CORROBORATION: Federal congressional records (1862-1887) explicitly frame anti-polygamy legislation as territorial governance and moral regulation. Non-Mormon territorial officials' correspondence confirms the political objective. LDS leadership's own diary entries (Woodruff, Cannon) document the existential threat of temple seizure and corporate dissolution as the proximate cause of the Manifesto. No credible corroboration exists for the claim that the founding problem (federal sovereignty challenge) required the specific mechanism of doctrinal surrender rather than political accommodation.
narrative_ontology:disappearance_verdict(plural_marriage_mandate__exogenous_override_reading, world_rearranges).
narrative_ontology:founding_problem_status(plural_marriage_mandate__exogenous_override_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(plural_marriage_mandate__exogenous_override_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(plural_marriage_mandate__exogenous_override_reading, 'none', 1).
narrative_ontology:epsilon_provenance(plural_marriage_mandate__exogenous_override_reading, 0.88, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Extractiveness (0.88) is near-maximum because the constraint extracted the core religious practice defining community boundaries and eternal salvation, under threat of total institutional destruction. Suppression (0.92) is extreme: 1,300+ imprisonments, systematic property seizure via Edmunds-Tucker, disenfranchisement of all polygamists, and the 'underground' existence required to continue practice. Theater ratio (0.15) is low because the enforcement was genuine and brutal, not performative — though the Manifesto itself introduced a theatrical frame of 'voluntary' compliance. Accessibility collapse (0.65) is moderate-high: alternatives (exile to Mexico/Canada, fundamentalist schism) existed but required abandoning the institutional church or homeland. Resistance (0.85) was high: decades of legal challenges, civil disobedience, and eventual schism demonstrate the constraint was actively contested.
 *
 * DIRECTIONALITY LOGIC:
 *   Federal government and allies are structural beneficiaries (d near 0): they collected territorial control, statehood, moral authority, and legal precedent. Practicing polygamists and families are structural targets (d near 1): they bore imprisonment, property loss, family disruption, and salvation anxiety. LDS institutional leadership sits in a dual position: agenda_setter administering the constraint (issuing the Manifesto) but also payer bearing institutional survival costs. Fundamentalist dissenters are excluded: their voice was structurally silenced by both the coercive power and the accommodating institution. The directionality derivation from beneficiary/victim declarations + power + exit captures this asymmetry.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandate (plural marriage as divine requirement) was not atrophied — it was actively suppressed. The constraint's persistence depended entirely on federal enforcement machinery; when enforcement pressure shifted (post-statehood, Smoot hearings), the constraint mutated into new forms (Second Manifesto 1904, excommunication of fundamentalists). This is not mandatrophy (function fading from disuse) but constraint displacement: the original divine mandate was overridden by a superior coercive power, and the institutional church constructed a new constraint (monogamy as current revelation) to legitimate the capitulation. The snare classification captures this: the coordination story (voluntary revelation) is cover for the extraction story (federal coercion).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    coercion_vs_revelation_ambiguity,
    'Was the 1890 Manifesto experienced by contemporary participants as coercive surrender or as genuine revelation?',
    'Contemporary diaries, sermons, and correspondence from 1890-1891 — particularly from rank-and-file members and mid-level leadership — examined for whether the Manifesto was received as divine command or political necessity.',
    'If experienced as coercive surrender, the snare classification is strengthened; if experienced as genuine revelation by a critical mass, the endogenous reading gains structural plausibility and the constraint''s type shifts toward tangled_rope (coordination + extraction).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coercion_vs_revelation_ambiguity, empirical, 'Whether the constraint''s cover story was believed by its targets').

omega_variable(
    divine_requirement_ontology,
    'Was plural marriage structurally a divine requirement (non-negotiable) or a divine permission (negotiable) within LDS theology?',
    'Doctrinal analysis of pre-1890 LDS theology: statements by Joseph Smith, Brigham Young, and John Taylor on whether plural marriage was essential for exaltation or a dispensational commandment.',
    'If a non-negotiable requirement, the Manifesto''s surrender constitutes a structural break in the church''s claim to revelatory continuity (supporting snare). If a dispensational permission, the Manifesto is a legitimate policy change within theological parameters (supporting endogenous or pragmatic readings).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(divine_requirement_ontology, conceptual, 'Theological status of the surrendered practice').

omega_variable(
    continuity_of_coercion_post_1890,
    'Did federal coercion actually cease after the Manifesto, or did it mutate into new forms (Smoot hearings, Second Manifesto, fundamentalist prosecutions)?',
    'Legislative and prosecutorial record 1890-1910: Reed Smoot hearings (1904-1907), Second Manifesto (1904), continued fundamentalist prosecutions, and federal pressure on church leadership.',
    'If coercion continued, the 1890 Manifesto was not a resolution but a phase transition in an ongoing coercive constraint — supporting snare persistence. If coercion genuinely ceased, the Manifesto achieved its stated purpose and the constraint may have resolved.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(continuity_of_coercion_post_1890, empirical, 'Whether the coercive machinery persisted after the nominal surrender').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(plural_marriage_mandate__exogenous_override_reading, 1862, 1904).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(plural_marriage_mandate__exogenous_override_reading_tr_t1862, plural_marriage_mandate__exogenous_override_reading, theater_ratio, 1862, 0.05).
narrative_ontology:measurement(plural_marriage_mandate__exogenous_override_reading_tr_t1874, plural_marriage_mandate__exogenous_override_reading, theater_ratio, 1874, 0.08).
narrative_ontology:measurement(plural_marriage_mandate__exogenous_override_reading_tr_t1879, plural_marriage_mandate__exogenous_override_reading, theater_ratio, 1879, 0.1).
narrative_ontology:measurement(plural_marriage_mandate__exogenous_override_reading_tr_t1882, plural_marriage_mandate__exogenous_override_reading, theater_ratio, 1882, 0.12).
narrative_ontology:measurement(plural_marriage_mandate__exogenous_override_reading_tr_t1887, plural_marriage_mandate__exogenous_override_reading, theater_ratio, 1887, 0.14).
narrative_ontology:measurement(plural_marriage_mandate__exogenous_override_reading_tr_t1890, plural_marriage_mandate__exogenous_override_reading, theater_ratio, 1890, 0.15).
narrative_ontology:measurement(plural_marriage_mandate__exogenous_override_reading_tr_t1904, plural_marriage_mandate__exogenous_override_reading, theater_ratio, 1904, 0.2).

% Extraction over time
narrative_ontology:measurement(plural_marriage_mandate__exogenous_override_reading_be_t1862, plural_marriage_mandate__exogenous_override_reading, base_extractiveness, 1862, 0.35).
narrative_ontology:measurement(plural_marriage_mandate__exogenous_override_reading_be_t1874, plural_marriage_mandate__exogenous_override_reading, base_extractiveness, 1874, 0.45).
narrative_ontology:measurement(plural_marriage_mandate__exogenous_override_reading_be_t1879, plural_marriage_mandate__exogenous_override_reading, base_extractiveness, 1879, 0.55).
narrative_ontology:measurement(plural_marriage_mandate__exogenous_override_reading_be_t1882, plural_marriage_mandate__exogenous_override_reading, base_extractiveness, 1882, 0.65).
narrative_ontology:measurement(plural_marriage_mandate__exogenous_override_reading_be_t1887, plural_marriage_mandate__exogenous_override_reading, base_extractiveness, 1887, 0.75).
narrative_ontology:measurement(plural_marriage_mandate__exogenous_override_reading_be_t1890, plural_marriage_mandate__exogenous_override_reading, base_extractiveness, 1890, 0.88).
narrative_ontology:measurement(plural_marriage_mandate__exogenous_override_reading_be_t1904, plural_marriage_mandate__exogenous_override_reading, base_extractiveness, 1904, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(plural_marriage_mandate__exogenous_override_reading_su_t1862, plural_marriage_mandate__exogenous_override_reading, suppression_requirement, 1862, 0.25).
narrative_ontology:measurement(plural_marriage_mandate__exogenous_override_reading_su_t1874, plural_marriage_mandate__exogenous_override_reading, suppression_requirement, 1874, 0.45).
narrative_ontology:measurement(plural_marriage_mandate__exogenous_override_reading_su_t1879, plural_marriage_mandate__exogenous_override_reading, suppression_requirement, 1879, 0.55).
narrative_ontology:measurement(plural_marriage_mandate__exogenous_override_reading_su_t1882, plural_marriage_mandate__exogenous_override_reading, suppression_requirement, 1882, 0.7).
narrative_ontology:measurement(plural_marriage_mandate__exogenous_override_reading_su_t1887, plural_marriage_mandate__exogenous_override_reading, suppression_requirement, 1887, 0.85).
narrative_ontology:measurement(plural_marriage_mandate__exogenous_override_reading_su_t1890, plural_marriage_mandate__exogenous_override_reading, suppression_requirement, 1890, 0.92).
narrative_ontology:measurement(plural_marriage_mandate__exogenous_override_reading_su_t1904, plural_marriage_mandate__exogenous_override_reading, suppression_requirement, 1904, 0.88).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(plural_marriage_mandate__exogenous_override_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(plural_marriage_mandate__exogenous_override_reading, 0.12).
narrative_ontology:affects_constraint(plural_marriage_mandate__exogenous_override_reading, plural_marriage_mandate__endogenous_reinterpretation_reading).
narrative_ontology:affects_constraint(plural_marriage_mandate__exogenous_override_reading, plural_marriage_mandate__institutional_pragmatism_reading).
narrative_ontology:affects_constraint(plural_marriage_mandate__exogenous_override_reading, utah_statehood_constitution).
narrative_ontology:affects_constraint(plural_marriage_mandate__exogenous_override_reading, reed_smoot_hearings_constraint).

% DUAL FORMULATION NOTE:
% This constraint family (plural_marriage_mandate) decomposes the single historical event (1890 Manifesto) into three structurally distinct constraints with different ε values, beneficiary/victim structures, and types. The exogenous_override_reading has ε=0.88 (snare); the endogenous_reinterpretation_reading would have ε≈0.15 (rope/mountain); the institutional_pragmatism_reading would have ε≈0.45 (tangled_rope). They are linked because the endogenous and pragmatic readings cite the same historical facts but with different structural attributions.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(plural_marriage_mandate__exogenous_override_reading, institutional, 0.35).
constraint_indexing:directionality_override(plural_marriage_mandate__exogenous_override_reading, powerless, 0.95).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
