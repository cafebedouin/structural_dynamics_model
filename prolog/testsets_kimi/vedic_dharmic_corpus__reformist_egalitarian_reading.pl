% ============================================================================
% CONSTRAINT STORY: vedic_dharmic_corpus__reformist_egalitarian_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_vedic_dharmic_corpus__reformist_egalitarian_reading, []).

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
 *   constraint_id: vedic_dharmic_corpus__reformist_egalitarian_reading
 *   human_readable: Reformist Egalitarian Reading of Vedic Dharmic Corpus
 *   domain: religious_authority/social_stratification/interpretive_legitimacy
 *
 * SUMMARY:
 *   This constraint instantiates the reformist_egalitarian_reading of the
 *   contested vedic_dharmic_corpus kernel. It holds that textual meaning must
 *   conform to constitutional equality principles, that caste hierarchy is
 *   historical accretion rather than scriptural essence, and that rational
 *   critique supersedes traditional authority. Enforced through Indian
 *   constitutional law and judicial precedent, it displaces hereditary ritual
 *   and interpretive authority from orthodox Brahminical gatekeepers and
 *   religious institutions, transferring legitimacy to marginalized
 *   communities, reformist scholars, and state-mediated rational critique.
 *   The constraint is actively contested and entangled with state legal
 *   enforcement.
 *
 * KEY AGENTS:
 *   - constitutional_judiciary: agenda_setter (institutional/analytical) â enforces constitutional morality over traditional authority
 *   - dalit_movements: primary beneficiary (organized/constrained) â gains legal standing and anti-discrimination protections
 *   - reformist_scholars: secondary beneficiary (moderate/mobile) â produces the historical-critical scholarship that underwrites the reading
 *   - orthodox_religious_institutions: primary payer (organized/constrained) â loses administrative and ritual exclusivity to state oversight
 *   - brahminical_gatekeepers: secondary payer (moderate/identity_locked) â hereditary sacral authority eroded by legal equality
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vedic_dharmic_corpus__reformist_egalitarian_reading, 0.45).
domain_priors:suppression_score(vedic_dharmic_corpus__reformist_egalitarian_reading, 0.65).
domain_priors:theater_ratio(vedic_dharmic_corpus__reformist_egalitarian_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vedic_dharmic_corpus__reformist_egalitarian_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(vedic_dharmic_corpus__reformist_egalitarian_reading, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(vedic_dharmic_corpus__reformist_egalitarian_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vedic_dharmic_corpus__reformist_egalitarian_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(vedic_dharmic_corpus__reformist_egalitarian_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vedic_dharmic_corpus__reformist_egalitarian_reading, tangled_rope).
narrative_ontology:human_readable(vedic_dharmic_corpus__reformist_egalitarian_reading, "Reformist Egalitarian Reading of Vedic Dharmic Corpus").
narrative_ontology:topic_domain(vedic_dharmic_corpus__reformist_egalitarian_reading, "religious_authority/social_stratification/interpretive_legitimacy").

domain_priors:requires_active_enforcement(vedic_dharmic_corpus__reformist_egalitarian_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vedic_dharmic_corpus__reformist_egalitarian_reading, 'd85c7481-a3cd-408b-bd0e-e22879784e96').
narrative_ontology:cs_kernel_codification('d85c7481-a3cd-408b-bd0e-e22879784e96', fixed_text).
narrative_ontology:cs_authority_grounding('d85c7481-a3cd-408b-bd0e-e22879784e96', expertise).
narrative_ontology:cs_interpretation_layer_present('d85c7481-a3cd-408b-bd0e-e22879784e96').
narrative_ontology:cs_reading_relation('d85c7481-a3cd-408b-bd0e-e22879784e96', vedic_dharmic_corpus__hereditary_monopoly_reading, influences).
narrative_ontology:cs_reading_relation('d85c7481-a3cd-408b-bd0e-e22879784e96', vedic_dharmic_corpus__bhakti_devotional_reading, coexists_with).
narrative_ontology:cs_axiom('d85c7481-a3cd-408b-bd0e-e22879784e96', foundational, constitutional_equality_supersedes_text).
narrative_ontology:cs_axiom_status(constitutional_equality_supersedes_text, holdable).
narrative_ontology:cs_axiom_grounding('d85c7481-a3cd-408b-bd0e-e22879784e96', constitutional_equality_supersedes_text, conventional).
narrative_ontology:cs_axiom('d85c7481-a3cd-408b-bd0e-e22879784e96', foundational, caste_as_historical_accretion).
narrative_ontology:cs_axiom_status(caste_as_historical_accretion, holdable).
narrative_ontology:cs_axiom_grounding('d85c7481-a3cd-408b-bd0e-e22879784e96', caste_as_historical_accretion, empirically_contingent).
narrative_ontology:cs_reference_frame('d85c7481-a3cd-408b-bd0e-e22879784e96', constitutional_legal_order).
narrative_ontology:cs_drift_state('d85c7481-a3cd-408b-bd0e-e22879784e96', contemporary_hindutva_resurgence, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('d85c7481-a3cd-408b-bd0e-e22879784e96', '').
narrative_ontology:cs_kernel_id(vedic_dharmic_corpus__reformist_egalitarian_reading, vedic_dharmic_corpus).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vedic_dharmic_corpus__reformist_egalitarian_reading, dalit_movements).
narrative_ontology:constraint_beneficiary(vedic_dharmic_corpus__reformist_egalitarian_reading, reformist_scholars).
narrative_ontology:constraint_victim(vedic_dharmic_corpus__reformist_egalitarian_reading, orthodox_religious_institutions).
narrative_ontology:constraint_victim(vedic_dharmic_corpus__reformist_egalitarian_reading, brahminical_gatekeepers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interprets the Vedic dharmic corpus and religious custom through the lens of constitutional morality and equality; issues judgments opening public temples to all castes, regulates religious endowments, and subordinates traditional authority to fundamental rights.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__reformist_egalitarian_reading, constitutional_judiciary, agenda_setter,
    institutional, generational, analytical, national).

% Assert rights to equal temple entry, public dignity, and anti-discrimination protections under constitutional law; benefit from legal delegitimization of birth-based exclusion, though social exit from caste identity remains constrained by extra-legal discrimination.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__reformist_egalitarian_reading, dalit_movements, beneficiary,
    organized, generational, constrained, national).

% Produce historical-critical and philological scholarship arguing that caste hierarchy is a post-Vedic accretion rather than scriptural essence; their academic and public legitimacy rises as courts cite rational critique to overturn traditional interpretation.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__reformist_egalitarian_reading, reformist_scholars, beneficiary,
    moderate, biographical, mobile, national).

% Temple trusts, monastic orders, and religious endowments that lose exclusive administrative and ritual control to state oversight and inclusive-access mandates; forced to accommodate devotees from all castes and submit to statutory management boards.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__reformist_egalitarian_reading, orthodox_religious_institutions, payer,
    organized, generational, constrained, national).

% Hereditary ritual specialists and traditional interpreters whose sacral authority and economic support depend on birth-based lineage; the reformist reading legally erodes their exclusive claims to perform rites, teach texts, and mediate access to divine benefits.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__reformist_egalitarian_reading, brahminical_gatekeepers, payer,
    moderate, generational, identity_locked, national).

narrative_ontology:fixing_cost_class(vedic_dharmic_corpus__reformist_egalitarian_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes uniform legal personhood and equal access to public religious spaces regardless of birth status; provides a shared interpretive framework for resolving caste-based disputes through constitutional principles rather than ascriptive hierarchy.
% TRANSFER_FUNCTION: Moves authority to define legitimate religious practice and textual meaning from hereditary ritual specialists and orthodox institutions to constitutional courts, reformist scholars, and marginalized communities asserting equal rights.
% ABSENT_VOICES: Traditional grammarians and hereditary priests who read caste as intrinsic to cosmic order and divine ordinance; they are not in the constitutional room but would reject the equality premise root and branch.
% DISAPPEARANCE_RATIONALE: If this reading vanished, constitutional protections against caste discrimination would lose their primary interpretive grounding; temple entry disputes would revert to traditional authority, the legal basis for affirmative action and anti-untouchability law would weaken, and state oversight of religious endowments would contract sharply.
% FOUNDING_PROBLEM: Caste-based exclusion from public goods, religious participation, and social dignity; colonial and post-colonial need for a unified civic identity transcending ascriptive hierarchy.
% FOUNDING_PROBLEM_CORROBORATION: Dalit movements and constitutional historians attest the problem is live. Orthodox institutions claim the problem is misdiagnosed and that the traditional order already manages hierarchy justly. Independent sociological evidence on caste violence and exclusion indices corroborates the live-problem reading from outside the pure beneficiary set.
narrative_ontology:disappearance_verdict(vedic_dharmic_corpus__reformist_egalitarian_reading, world_rearranges).
narrative_ontology:founding_problem_status(vedic_dharmic_corpus__reformist_egalitarian_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vedic_dharmic_corpus__reformist_egalitarian_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(vedic_dharmic_corpus__reformist_egalitarian_reading, 'none', 1).
narrative_ontology:epsilon_provenance(vedic_dharmic_corpus__reformist_egalitarian_reading, 0.45, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(vedic_dharmic_corpus__reformist_egalitarian_reading_tests).
:- end_tests(vedic_dharmic_corpus__reformist_egalitarian_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.45) because the displacement of hereditary authority is substantial but incomplete: orthodox institutions retain significant social and informal power outside the legal frame. Suppression (0.65) is high because the constraint's persistence depends on state legal machinery actively overriding birth-based exclusion claims and temple autonomy assertions. Theater ratio (0.28) is moderate-low because much of the judicial and scholarly activity is functional anti-discrimination work, though some constitutional spectacle serves state legitimation. Resistance (0.7) is high because orthodox institutions mount sustained legal and social pushback. Accessibility collapse (0.6) reflects that hereditary authority claims collapse almost completely within constitutional legal discourse, even as they persist socially.
 *
 * PERSPECTIVAL GAP:
 *   The judiciary experiences this constraint as principled coordination toward a secular, egalitarian public order. Orthodox institutions experience the same structure as state confiscation of religious autonomy and hereditary property. The engine computes this divergence from the structural asymmetry in power, exit options, and beneficiary-victim declarations; the authored claim (tangled_rope) does not adjudicate which seat is correct.
 *
 * DIRECTIONALITY LOGIC:
 *   The constitutional judiciary and reformist scholars sit near the beneficiary end: they generate and administer the constraint and gain institutional power or academic legitimacy from its operation. Dalit movements are structural beneficiaries (low d) but their spatial scope and exit remain constrained by extra-legal discrimination, moderating the subsidy. Orthodox religious institutions and Brahminical gatekeepers are the targets: they bear the direct extraction of displaced authority and property control, with d near the full-target end amplified by identity-locked exit for hereditary specialists.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification prevents mislabeling this as a pure snare by acknowledging the genuine coordination function: constitutional equality does solve real collective-action problems of caste-based exclusion in public goods and religious access. However, it also prevents mislabeling as a pure rope because the extraction is asymmetric and directional â authority and economic rents are transferred from specific orthodox parties to the state and marginalized claimants, and the arrangement requires active legal enforcement to hold against entrenched resistance.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'This constraint instantiates the reformist_egalitarian_reading of vedic_dharmic_corpus; does the structural classification change if the hereditary_monopoly_reading or bhakti_devotional_reading is adopted as the operative frame?',
    'Comparative analysis of all three sibling readings within the kernel to see if epsilon values and beneficiary/victim structures invert or collapse.',
    'If the hereditary reading is structurally dominant, this constraint''s extraction profile would invert; if the bhakti reading dominates, the coercion profile softens toward a rope.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Committer-frame uncertainty about kernel reading contest.').

omega_variable(
    caste_accretion_empirical_status,
    'Is caste hierarchy demonstrably a post-Vedic historical accretion, or is it embedded in the textual fabric of the Vedic corpus in ways that philological analysis cannot cleanly separate?',
    'Interdisciplinary philological and archaeological consensus on the historical development of varna and jati; critical edition studies of relevant textual strata.',
    'If caste is shown to be core textual doctrine, the reformist reading''s empirical foundation collapses and its authority reverts to pure conventional constitutional fiat, raising extraction and potentially shifting type toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(caste_accretion_empirical_status, empirical, 'Empirical ambiguity about caste as accretion versus essence.').

omega_variable(
    state_secularism_religious_autonomy,
    'Does state legal enforcement of egalitarian textual interpretation constitute necessary coordination or asymmetric extraction from religious autonomy?',
    'Comparative constitutional analysis of religious-freedom jurisprudence and measurement of state intrusion into temple administration versus public-goods provision.',
    'If the state''s role is dominantly coordinative, the constraint stays tangled_rope; if dominantly extractive, it shifts toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(state_secularism_religious_autonomy, preference, 'Ambiguity about state enforcement as coordination or extraction.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vedic_dharmic_corpus__reformist_egalitarian_reading, 0, 70).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vedi_tr_t0, vedic_dharmic_corpus__reformist_egalitarian_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(vedi_tr_t14, vedic_dharmic_corpus__reformist_egalitarian_reading, theater_ratio, 14, 0.18).
narrative_ontology:measurement(vedi_tr_t28, vedic_dharmic_corpus__reformist_egalitarian_reading, theater_ratio, 28, 0.22).
narrative_ontology:measurement(vedi_tr_t42, vedic_dharmic_corpus__reformist_egalitarian_reading, theater_ratio, 42, 0.25).
narrative_ontology:measurement(vedi_tr_t56, vedic_dharmic_corpus__reformist_egalitarian_reading, theater_ratio, 56, 0.27).
narrative_ontology:measurement(vedi_tr_t70, vedic_dharmic_corpus__reformist_egalitarian_reading, theater_ratio, 70, 0.28).

% Extraction over time
narrative_ontology:measurement(vedi_be_t0, vedic_dharmic_corpus__reformist_egalitarian_reading, base_extractiveness, 0, 0.2).
narrative_ontology:measurement(vedi_be_t14, vedic_dharmic_corpus__reformist_egalitarian_reading, base_extractiveness, 14, 0.28).
narrative_ontology:measurement(vedi_be_t28, vedic_dharmic_corpus__reformist_egalitarian_reading, base_extractiveness, 28, 0.35).
narrative_ontology:measurement(vedi_be_t42, vedic_dharmic_corpus__reformist_egalitarian_reading, base_extractiveness, 42, 0.4).
narrative_ontology:measurement(vedi_be_t56, vedic_dharmic_corpus__reformist_egalitarian_reading, base_extractiveness, 56, 0.43).
narrative_ontology:measurement(vedi_be_t70, vedic_dharmic_corpus__reformist_egalitarian_reading, base_extractiveness, 70, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(vedi_su_t0, vedic_dharmic_corpus__reformist_egalitarian_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(vedi_su_t14, vedic_dharmic_corpus__reformist_egalitarian_reading, suppression_requirement, 14, 0.45).
narrative_ontology:measurement(vedi_su_t28, vedic_dharmic_corpus__reformist_egalitarian_reading, suppression_requirement, 28, 0.52).
narrative_ontology:measurement(vedi_su_t42, vedic_dharmic_corpus__reformist_egalitarian_reading, suppression_requirement, 42, 0.58).
narrative_ontology:measurement(vedi_su_t56, vedic_dharmic_corpus__reformist_egalitarian_reading, suppression_requirement, 56, 0.62).
narrative_ontology:measurement(vedi_su_t70, vedic_dharmic_corpus__reformist_egalitarian_reading, suppression_requirement, 70, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vedic_dharmic_corpus__reformist_egalitarian_reading, identity_coordination).
narrative_ontology:affects_constraint(vedic_dharmic_corpus__reformist_egalitarian_reading, vedic_dharmic_corpus__hereditary_monopoly_reading).
narrative_ontology:affects_constraint(vedic_dharmic_corpus__reformist_egalitarian_reading, vedic_dharmic_corpus__bhakti_devotional_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the vedic_dharmic_corpus kernel. The reformist_egalitarian_reading, hereditary_monopoly_reading, and bhakti_devotional_reading are structurally distinct constraints that share a textual kernel but emit different epsilon values and beneficiary/victim structures. They must be evaluated separately per the epsilon-invariance principle.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
